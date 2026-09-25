open Windtrap

let shared_server = fixture ~teardown:Server.stop Server.start
let with_session = scoped (fun f -> Server.with_session (shared_server ()) f)

let server =
  group ~timeout:5. "server"
    [
      focus
        (test "it answers a ping" (fun () ->
             is_true (Server.ping (shared_server ()))));
      with_session "a first session gets id 1" (fun session ->
          equal int 1 (Server.session_id session));
      slow ~timeout:60. "reindexing keeps it running" (fun () ->
          is_true (Server.reindex (shared_server ())));
      test ~retries:2 "a backup completes" (fun () ->
          is_true (Server.backup (shared_server ())));
    ]
