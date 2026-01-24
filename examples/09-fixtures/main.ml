(** Setup and teardown with fixtures.

    - [bracket]: Per-test setup/teardown with resource passed to test
    - [fixture]: Lazy shared resource, created on first use
    - [group ~setup ~teardown]: Suite-level hooks run once for the group
    - [~retries]: Retry flaky tests (use sparingly) *)

open Windtrap

(** A simulated database connection. *)
module Database = struct
  type t = { id : int; mutable data : (string * string) list }

  let next_id = ref 0

  let connect () =
    incr next_id;
    { id = !next_id; data = [] }

  let disconnect _db = ()
  let set db key value = db.data <- (key, value) :: db.data
  let get db key = List.assoc_opt key db.data
end

(** A temporary file handle. *)
module TempFile = struct
  type t = { path : string; channel : out_channel }

  let create () =
    let path = Filename.temp_file "test_" ".txt" in
    let channel = open_out path in
    { path; channel }

  let cleanup t =
    close_out t.channel;
    Sys.remove t.path

  let write t s = output_string t.channel s
  let path t = t.path
end

(** Create reusable test constructors with partial application. *)
let with_db = bracket ~setup:Database.connect ~teardown:Database.disconnect

let with_temp_file = bracket ~setup:TempFile.create ~teardown:TempFile.cleanup

(** Lazy fixture: created on first call, cached for subsequent calls. Use when
    multiple tests share an expensive resource. *)
let get_shared_db = fixture (fun () -> Database.connect ())

let () =
  run "Fixtures"
    [
      group "Database tests"
        [
          with_db "can store and retrieve" (fun db ->
              Database.set db "key" "value";
              equal
                (Testable.option Testable.string)
                (Some "value") (Database.get db "key"));
          with_db "returns None for missing keys" (fun db ->
              equal
                (Testable.option Testable.string)
                None
                (Database.get db "missing"));
        ];
      group "File tests"
        [
          with_temp_file "can write to file" (fun tf ->
              TempFile.write tf "hello";
              is_true (Sys.file_exists (TempFile.path tf)));
        ];
      group "Slow tests"
        [
          (* Slow tests are skipped with --quick. *)
          slow "expensive computation" (fun () ->
              let sum = List.init 100000 succ |> List.fold_left ( + ) 0 in
              equal Testable.int 5000050000 sum);
        ];
      group "Timeouts"
        [
          test ~timeout:1.0 "completes within timeout" (fun () ->
              equal Testable.int 42 42);
        ];
      (* Lazy fixture: resource created once on first access. *)
      group "Shared fixture"
        [
          test "first access creates resource" (fun () ->
              let db = get_shared_db () in
              Database.set db "shared" "data";
              is_true true);
          test "second access reuses same resource" (fun () ->
              let db = get_shared_db () in
              (* Data persists because it's the same connection. *)
              equal
                (Testable.option Testable.string)
                (Some "data") (Database.get db "shared"));
        ];
      (* Suite-level hooks: setup runs once before group, teardown after. *)
      group "Suite-level hooks"
        ~setup:(fun () -> print_endline "  [setup] Preparing test environment")
        ~teardown:(fun () -> print_endline "  [teardown] Cleaning up")
        [
          test "first test in group" (fun () -> is_true true);
          test "second test in group" (fun () -> is_true true);
        ];
      (* Retries: for flaky tests that occasionally fail. Use sparingly. *)
      group "Retries"
        [
          test ~retries:2 "flaky test with retries" (fun () ->
              (* This test is allowed to fail twice before giving up. *)
              is_true true);
        ];
    ]
