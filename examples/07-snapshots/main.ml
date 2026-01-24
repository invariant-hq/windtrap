(** Snapshot testing.

    Snapshots compare output against saved files. On first run, they create the
    snapshot. On subsequent runs, they compare against it. Use -u (or --update)
    to update snapshots when output intentionally changes. *)

open Windtrap

type user = { name : string; age : int; email : string }

let user_to_json u =
  Printf.sprintf {|{"name": "%s", "age": %d, "email": "%s"}|} u.name u.age
    u.email

let format_report items =
  let header = "=== Report ===" in
  let lines = List.map (fun (k, v) -> Printf.sprintf "  %s: %s" k v) items in
  String.concat "\n" (header :: lines)

let () =
  run "Snapshots"
    [
      group "Basic snapshots"
        [
          test "json output" (fun () ->
              let user =
                { name = "Alice"; age = 30; email = "alice@example.com" }
              in
              snapshot ~pos:__POS__ (user_to_json user));
          test "formatted report" (fun () ->
              let report =
                format_report
                  [ ("Status", "OK"); ("Items", "42"); ("Duration", "1.5s") ]
              in
              snapshot ~pos:__POS__ report);
        ];
      group "Named snapshots"
        [
          test "with explicit name" (fun () ->
              (* Named snapshots are easier to identify in the __snapshots__
                 dir. *)
              snapshot ~pos:__POS__ ~name:"greeting" "Hello, World!");
          test "config file" (fun () ->
              let config =
                {|[server]
host = "localhost"
port = 8080

[database]
url = "postgres://localhost/mydb"|}
              in
              snapshot ~pos:__POS__ ~name:"config" config);
        ];
      group "With pretty-printers"
        [
          test "snapshot_pp" (fun () ->
              let numbers = [ 1; 2; 3; 4; 5 ] in
              snapshot_pp ~pos:__POS__ ~name:"numbers"
                Pp.(brackets (list ~sep:comma int))
                numbers);
        ];
      group "Formatted snapshots"
        [
          test "snapshotf" (fun () ->
              snapshotf ~pos:__POS__ ~name:"stats"
                "Users: %d, Active: %d, Ratio: %.2f%%" 1000 750 75.0);
        ];
    ]
