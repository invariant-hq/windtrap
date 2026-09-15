(* The guide's resources-and-structure example: [bracket] scopes a resource
   to one test (teardown always runs if setup succeeded), [scoped] takes a
   callback-shaped resource whole, [fixture] is acquired on first use, shared,
   and released by the runner at the end of the run — and the optional
   arguments shape the suite: [~timeout] on a group is the default for every
   test under it, [slow] tags the long one so [--exclude-tag slow] can drop
   it. *)

open Windtrap

let with_db = bracket ~setup:Db.connect ~teardown:Db.close
let server = fixture ~teardown:Server.stop Server.start
let with_session = scoped (fun k -> Server.with_session (server ()) k)

let () =
  exit
  @@ run "storage"
       [
         with_db "insert then get" (fun db ->
             Db.insert db "alice";
             equal int 1 (Db.count db));
         with_session "opens a session" (fun session ->
             equal int 1 (Server.session_id session));
         group ~timeout:5. "server"
           [
             test "responds" (fun () -> is_true (Server.ping (server ())));
             slow "reindexes" (fun () -> is_true (Server.reindex (server ())));
           ];
       ]
