(* Regression coverage for the Luv APIs used by ReScript's native watcher.
   Use real filesystem events, private loops, and the same ONCE/NOWAIT cleanup
   pattern. Event counts and ordering vary between operating systems. *)

let check message condition =
  if not condition then failwith message

let ok operation = function
  | Ok value -> value
  | Error error ->
    failwith (Printf.sprintf "%s: %s (%s)" operation
      (Luv.Error.err_name error) (Luv.Error.strerror error))

let close_handle loop handle =
  let closed = ref false in
  Luv.Handle.close handle (fun () -> closed := true);
  check "handle is closing" (Luv.Handle.is_closing handle);
  let deadline = Unix.gettimeofday () +. 5. in
  while not !closed && Unix.gettimeofday () < deadline do
    ignore (Luv.Loop.run ~loop ~mode:`NOWAIT ())
  done;
  check "close callback was delivered" !closed

let await loop message ready =
  let timed_out = ref false in
  let timeout = ok "timeout init" (Luv.Timer.init ~loop ()) in
  ok "timeout start" (Luv.Timer.start timeout 5000 (fun () ->
    timed_out := true;
    Luv.Loop.stop loop));
  while not (ready ()) && not !timed_out do
    ignore (Luv.Loop.run ~loop ~mode:`ONCE ())
  done;
  ok "timeout stop" (Luv.Timer.stop timeout);
  close_handle loop timeout;
  check ("timed out: " ^ message) (ready ())

let with_loop f =
  let loop = ok "loop init" (Luv.Loop.init ()) in
  f loop;
  check "private loop has no active handles" (not (Luv.Loop.alive loop));
  ok "loop close" (Luv.Loop.close loop)

let rec remove_tree path =
  if Sys.is_directory path then begin
    Array.iter (fun entry -> remove_tree (Filename.concat path entry))
      (Sys.readdir path);
    Unix.rmdir path
  end else
    Sys.remove path

let with_directory f =
  let directory = Filename.temp_file "luv-rewatch-" ".dir" in
  Sys.remove directory;
  Unix.mkdir directory 0o700;
  match f directory with
  | value -> remove_tree directory; value
  | exception error -> remove_tree directory; raise error

let write_file path contents =
  let channel = open_out_bin path in
  output_string channel contents;
  close_out channel

let watch loop directory events =
  let handle = ok "watch init" (Luv.FS_event.init ~loop ()) in
  Luv.FS_event.start handle directory (fun result ->
    events := ok "watch callback" result :: !events;
    Luv.Loop.stop loop);
  check "watch is active" (Luv.Handle.is_active handle);
  handle

let close_watch loop handle =
  ok "watch stop" (Luv.FS_event.stop handle);
  check "watch is stopped" (not (Luv.Handle.is_active handle));
  close_handle loop handle

let has_event events names kind =
  List.exists (fun (filename, kinds) ->
    (* A missing filename tells rewatch to rescan the watched directory. *)
    (match filename with
     | None | Some "" -> true
     | Some filename -> List.mem (Filename.basename filename) names)
    && List.mem kind kinds) !events

let directory_changes () =
  with_directory (fun directory -> with_loop (fun loop ->
    let original = Filename.concat directory "source.res" in
    let renamed = Filename.concat directory "renamed.res" in
    write_file original "let value = 1\n";
    let events = ref [] in
    let handle = watch loop directory events in
    let expect message names kind action =
      ignore (Luv.Loop.run ~loop ~mode:`NOWAIT ());
      events := [];
      action ();
      Gc.full_major ();
      await loop message (fun () -> has_event events names kind)
    in
    expect "file modification" ["source.res"] `CHANGE
      (fun () -> write_file original "let value = 2\n");
    expect "file creation" ["created.res"] `RENAME
      (fun () -> write_file (Filename.concat directory "created.res") "created\n");
    expect "file rename" ["source.res"; "renamed.res"] `RENAME
      (fun () -> Sys.rename original renamed);
    expect "file deletion" ["renamed.res"] `RENAME
      (fun () -> Sys.remove renamed);
    close_watch loop handle))

let timer_cancellation () =
  with_directory (fun directory -> with_loop (fun loop ->
    let events = ref [] in
    let handle = watch loop directory events in
    let ticks = ref 0 in
    let timer = ok "timer init" (Luv.Timer.init ~loop ()) in
    ok "repeating timer start" (Luv.Timer.start ~repeat:10 timer 10 (fun () ->
      incr ticks;
      Luv.Loop.stop loop));
    await loop "timer interrupts an idle watcher" (fun () -> !ticks >= 3);
    ok "timer stop" (Luv.Timer.stop timer);
    let stopped_at = !ticks in
    let settled = ref false in
    let settle = ok "settle timer init" (Luv.Timer.init ~loop ()) in
    ok "settle timer start" (Luv.Timer.start settle 50 (fun () ->
      settled := true;
      Luv.Loop.stop loop));
    await loop "loop continues after timer cancellation" (fun () -> !settled);
    check "stopped timer has no further callbacks" (!ticks = stopped_at);
    check "idle watcher has no filesystem events" (!events = []);
    close_handle loop settle;
    close_handle loop timer;
    close_watch loop handle))

let watcher_refresh () =
  with_directory (fun directory -> with_loop (fun loop ->
    let first = Filename.concat directory "first" in
    let second = Filename.concat directory "second" in
    Unix.mkdir first 0o700;
    Unix.mkdir second 0o700;
    let first_events = ref [] and second_events = ref [] in
    let first_handle = watch loop first first_events in
    let second_handle = watch loop second second_events in
    write_file (Filename.concat second "queued.res") "queued\n";
    (* Reconciliation pumps NOWAIT while closing obsolete handles. Events on
       handles being kept must survive that cleanup. *)
    close_watch loop first_handle;
    await loop "event queued during handle cleanup"
      (fun () -> has_event second_events ["queued.res"] `RENAME);
    Unix.rmdir first;
    Unix.mkdir first 0o700;
    let replacement = watch loop first first_events in
    write_file (Filename.concat first "replacement.res") "replacement\n";
    await loop "replacement directory is watched"
      (fun () -> has_event first_events ["replacement.res"] `RENAME);
    close_watch loop replacement;
    close_watch loop second_handle))

let private_loops () =
  with_directory (fun directory -> with_loop (fun first_loop ->
    with_loop (fun second_loop ->
      let first_events = ref [] and second_events = ref [] in
      let first = watch first_loop directory first_events in
      let second = watch second_loop directory second_events in
      write_file (Filename.concat directory "shared.res") "shared\n";
      await first_loop "first private loop receives events"
        (fun () -> has_event first_events ["shared.res"] `RENAME);
      check "running one loop does not dispatch the other" (!second_events = []);
      await second_loop "second private loop receives events"
        (fun () -> has_event second_events ["shared.res"] `RENAME);
      check "default loop remains idle" (not (Luv.Loop.alive (Luv.Loop.default ())));
      close_watch first_loop first;
      close_watch second_loop second)))

let failed_watch () =
  with_directory (fun directory -> with_loop (fun loop ->
    let handle = ok "failed watch init" (Luv.FS_event.init ~loop ()) in
    let error = ref None in
    Luv.FS_event.start handle (Filename.concat directory "missing") (function
      | Ok _ -> failwith "watching a missing directory succeeded"
      | Error code -> error := Some code; Luv.Loop.stop loop);
    check "missing directory reports an error" (!error <> None);
    check "failed watch is inactive" (not (Luv.Handle.is_active handle));
    close_handle loop handle))

let () =
  Luv.Error.set_on_unhandled_exception raise;
  List.iter (fun (name, test) ->
    test ();
    Printf.printf "PASS %s\n%!" name) [
    "directory changes", directory_changes;
    "timer cancellation", timer_cancellation;
    "watcher refresh", watcher_refresh;
    "private loops", private_loops;
    "failed watch cleanup", failed_watch;
  ]
