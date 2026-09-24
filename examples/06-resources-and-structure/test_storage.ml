(* The resources-and-structure chapter's examples: [bracket] scopes a
   resource to one test (teardown always runs if setup succeeded), [scoped]
   takes a callback-shaped resource whole, [fixture] is acquired on first
   use, shared, and released by the runner at the end of the run (a
   fixture that skips gates every test that uses it), and [temp_dir],
   [setenv] and [chdir] are put back by the runner on every outcome. The
   optional arguments shape the suite: [~timeout] and [~retries] on a group
   are the defaults for every test under it, [slow] tags the long one so
   [--exclude-tag slow] can drop it, [subtest] labels sub-cases inside one
   body, and [xfail] keeps a known-bug reproduction in-tree without a red
   run. *)

open Windtrap

let with_db = bracket ~setup:Db.connect ~teardown:Db.close
let server = fixture ~teardown:Server.stop Server.start
let with_session = scoped (fun k -> Server.with_session (server ()) k)

(* A fixture whose acquisition skips gates every test that uses it: the
   probe runs once for the whole run, and an absent device never turns the
   run red. *)
let device : unit -> unit =
  fixture (fun () -> skip ~reason:"no device in this environment" ())

(* Code that reads a setting straight out of the environment: the shape
   [setenv] exists for, missing case included. *)
module Config = struct
  let token () = Sys.getenv_opt "API_TOKEN"
end

let backends = [ ("list", 12); ("array", 12); ("bigarray", 12) ]

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
             test ~retries:2 "fetches the manifest" (fun () ->
                 is_true (Server.ping (server ())));
             test ~timeout:60. "keeps its own limit" (fun () ->
                 is_true (Server.ping (server ())));
           ];
         group "gpu"
           [
             test "elementwise" (fun () -> device ());
             test "reduction" (fun () -> device ());
           ];
         test "writes a config" (fun () ->
             let dir = temp_dir () in
             let file = Filename.concat dir "config.json" in
             Out_channel.with_open_text file (fun oc ->
                 Out_channel.output_string oc "{}");
             is_true (Sys.file_exists file));
         test "reads the token from the environment" (fun () ->
             setenv "API_TOKEN" (Some "t-123");
             equal (option string) (Some "t-123") (Config.token ());
             setenv "API_TOKEN" None;
             equal (option string) None (Config.token ()));
         test "builds in place" (fun () ->
             chdir (temp_dir ());
             Out_channel.with_open_text "built.txt" (fun oc ->
                 Out_channel.output_string oc "ok");
             is_true (Sys.file_exists "built.txt"));
         test "backend contract" (fun () ->
             (* a failing sub-case is recorded as "backend contract › <name>"
                and its siblings still run; the test fails at the end with
                every recorded entry. *)
             List.iter
               (fun (name, count) ->
                 subtest name (fun () -> equal int 12 count))
               backends);
         test "artifacts keyed by test identity" (fun () ->
             let key = String.concat "-" (current_test ()) in
             equal string "artifacts keyed by test identity" key);
         xfail ~reason:"issue #42: a duplicate row is counted twice"
           (with_db "counts distinct rows" (fun db ->
                Db.insert db "alice";
                Db.insert db "alice";
                equal int 1 (Db.count db)));
       ]
