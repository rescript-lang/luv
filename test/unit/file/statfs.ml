let ok = function
  | Ok value -> value
  | Error error -> failwith (Luv.Error.strerror error)

let check (statfs : Luv.File.Statfs.t) =
  let zero = Unsigned.UInt64.zero in
  if statfs.bsize = zero then failwith "statfs returned a zero block size";
  if statfs.f_spare <> (zero, zero, zero, zero) then
    failwith "statfs reserved fields are not zero"

let () =
  (* Exercise both conversion paths after allocator reuse. In libuv 1.52+,
     reading a fourth C spare slot would run past the allocated structure. *)
  for _ = 1 to 100 do
    check (ok (Luv.File.Sync.statfs "."))
  done;
  let called = ref false in
  Luv.File.statfs "." (fun result ->
    check (ok result);
    called := true);
  ignore (Luv.Loop.run ());
  if not !called then failwith "statfs callback was not delivered"
