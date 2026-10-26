(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The suites of process.t, which PROCESS_SUITE selects: what a worker
   process starts from. Each declaration is a property of the program, the
   way it initialises, rather than of a flag. *)

open Windtrap

let strf = Printf.sprintf

(* Made as the module initialises, in every process that starts. *)
let pid = Unix.getpid ()
let own () = equal int pid (Unix.getpid ())

(* Ten tests that each read the value, and one between them that spawns a
   domain, which retires its worker, so the tests after it run in a worker
   that replaced it. *)
let own_pid () =
  print_endline "declaring the suite";
  List.init 5 (fun i -> test (strf "reads it, %d" i) own)
  @ [
      test "reads it and retires its worker" (fun () ->
          own ();
          Domain.join (Domain.spawn ignore));
    ]
  @ List.init 5 (fun i -> test (strf "reads it after, %d" i) own)

let status = function
  | Unix.WEXITED n -> strf "exited with code %d" n
  | Unix.WSIGNALED s -> strf "killed by signal %d" s
  | Unix.WSTOPPED s -> strf "stopped by signal %d" s

(* A run that only a child forked as the module initialises reaches, after
   the child spawned a domain. *)
let in_a_fork () =
  flush_all ();
  (match Unix.fork () with
  | 0 ->
      Domain.join (Domain.spawn ignore);
      let code =
        run ~argv:[| "child" |] "child" [ test "in the child" ignore ]
      in
      Unix._exit code
  | child ->
      let _, ended = Unix.waitpid [] child in
      print_endline ("the child " ^ status ended));
  [ test "in the parent" ignore ]

(* A second run that reads the code the first returned, in a process that
   spawned a domain before either. *)
let second_run () =
  Domain.join (Domain.spawn ignore);
  let first =
    run
      ~argv:[| "first"; "-o"; "_first" |]
      "first"
      [ test "fails" (fun () -> equal int 1 2) ]
  in
  [ test "the first run returned 1" (fun () -> equal int 1 first) ]

(* Tests named after a directory's listing, which the declaration changes:
   each start of the executable lists one file more. *)
let listing () =
  let dir = "listing" in
  if not (Sys.file_exists dir) then Sys.mkdir dir 0o755;
  let entries = Array.length (Sys.readdir dir) in
  close_out (open_out (Filename.concat dir (string_of_int entries)));
  List.map
    (fun entry -> test ("reads " ^ entry) ignore)
    (List.sort String.compare (Array.to_list (Sys.readdir dir)))

(* A resource that the first start of the executable takes, and every later
   start finds taken. *)
let held () =
  print_endline "opening the device";
  if Sys.file_exists "device" then begin
    prerr_endline "the device is held by another process";
    exit 3
  end;
  close_out (open_out "device");
  [ test "uses the device" ignore; test "uses it again" ignore ]

(* A lock that the first start of the executable takes and keeps: every
   later start waits for it, and the process that holds it waits for that
   start. *)
let locked () =
  print_endline "taking the lock";
  let starts = open_out_gen [ Open_append; Open_creat ] 0o644 "starts" in
  output_string starts "a start\n";
  close_out starts;
  let fd = Unix.openfile "lock" [ Unix.O_RDWR; Unix.O_CREAT ] 0o644 in
  Unix.lockf fd Unix.F_LOCK 0;
  [ test "never runs" ignore ]

(* A test that puts another program in the place of the executable, then
   retires its worker: the worker that replaces it is the executable the run
   started as. *)
let replaced () =
  [
    test "replaces the executable" (fun () ->
        Sys.rename "impostor" Sys.executable_name;
        Domain.join (Domain.spawn ignore));
    test "runs in the executable the run started as" own;
  ]

(* A file that each start of the executable makes as it initialises, named
   after its pid, and removes with an at_exit function registered with it.
   The second suite's test removes its worker's file first, so that worker's
   removal raises. *)
let made () =
  let dir = "made" in
  if not (Sys.file_exists dir) then Sys.mkdir dir 0o755;
  let path = Filename.concat dir (string_of_int (Unix.getpid ())) in
  close_out (open_out path);
  at_exit (fun () -> Sys.remove path);
  path

let cleaned () =
  ignore (made ());
  [ test "one" ignore; test "two" ignore ]

let cleaned_twice () =
  let path = made () in
  [ test "removes the file" (fun () -> Sys.remove path) ]

let () =
  let suite = Sys.getenv "PROCESS_SUITE" in
  let tests =
    match suite with
    | "own_pid" -> own_pid ()
    | "in_a_fork" -> in_a_fork ()
    | "second_run" -> second_run ()
    | "listing" -> listing ()
    | "held" -> held ()
    | "replaced" -> replaced ()
    | "locked" -> locked ()
    | "cleaned" -> cleaned ()
    | "cleaned_twice" -> cleaned_twice ()
    | other -> invalid_arg ("process_main: unknown PROCESS_SUITE " ^ other)
  in
  exit (run suite tests)
