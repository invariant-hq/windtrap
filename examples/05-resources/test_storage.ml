(* The guide's resources example: [bracket] scopes a resource to one test
   (teardown always runs if setup succeeded); [fixture] is acquired on first
   use, shared, and released by the runner at the end of the run. *)

open Windtrap

let with_db = bracket ~setup:Db.connect ~teardown:Db.close
let server = fixture ~teardown:Server.stop Server.start

let () =
  run "storage"
    [
      with_db "insert then get" (fun db ->
          Db.insert db "alice";
          equal int 1 (Db.count db));
      test "responds" (fun () -> is_true (Server.ping (server ())));
    ]
