(*--------------------------------------------------------------------------
  Copyright (c) 2026 Thibaut Mattio. All rights reserved.
  SPDX-License-Identifier: ISC

  Adapted from windtrap-next's test/internal/test_atomic_file.ml, trimmed to
  the v3 surface (no durability ceremony, one Sys_error path).
  --------------------------------------------------------------------------*)

open Windtrap
module Atomic_file = Windtrap.Private.Atomic_file

let fail label format =
  Printf.ksprintf (fun message -> Windtrap.fail (label ^ ": " ^ message)) format

let check label condition = is_true ~msg:label condition

let contains text substring =
  let text_length = String.length text in
  let substring_length = String.length substring in
  let rec loop offset =
    if offset + substring_length > text_length then false
    else if String.sub text offset substring_length = substring then true
    else loop (offset + 1)
  in
  loop 0

let equal_string label expected actual = equal ~msg:label string expected actual
let equal_int label expected actual = equal ~msg:label int expected actual

let sorted_directory path =
  Sys.readdir path |> Array.to_list |> List.sort String.compare

let equal_entries label expected actual =
  equal ~msg:label (list string) expected actual

(* The runner removes each test's scratch directory with the attempt. *)
let with_temporary_directory callback = callback (temp_dir ())

let with_umask mask callback =
  let previous = Unix.umask mask in
  Fun.protect ~finally:(fun () -> ignore (Unix.umask previous)) callback

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let expect_sys_error label ~path operation =
  match operation () with
  | _ -> fail label "expected Sys_error"
  | exception Sys_error message ->
      check
        (label ^ " message starts with the target path")
        (String.starts_with ~prefix:(path ^ ": ") message);
      message

(* Reserved names *)

let test_temp_prefix_is_reserved () =
  equal_string "temp_prefix" ".tmp-" Atomic_file.temp_prefix;
  check "temp name is recognized" (Atomic_file.is_temp_name ".tmp-1a2b-0");
  check "bare prefix is recognized" (Atomic_file.is_temp_name ".tmp-");
  check "prefix elsewhere is not recognized"
    (not (Atomic_file.is_temp_name "x.tmp-1"));
  check "shorter name is not recognized" (not (Atomic_file.is_temp_name ".tmp"));
  check "snapshot name is not recognized"
    (not (Atomic_file.is_temp_name "renders-empty.snap"))

(* Writing *)

let test_creates_exact_binary_file () =
  with_temporary_directory (fun directory ->
      let path = Filename.concat directory "target" in
      let contents =
        String.init
          ((256 * 1024) + 37)
          (fun index -> Char.chr (index land 0xff))
      in
      Atomic_file.write ~path contents;
      equal_string "create binary bytes" contents (read_file path);
      equal_entries "create binary siblings" [ "target" ]
        (sorted_directory directory))

let test_replaces_existing_file () =
  with_temporary_directory (fun directory ->
      let path = Filename.concat directory "target" in
      write_file path "old bytes that must disappear";
      Atomic_file.write ~path "new\000bytes";
      equal_string "replace existing bytes" "new\000bytes" (read_file path);
      equal_entries "replace existing siblings" [ "target" ]
        (sorted_directory directory))

let test_replaces_with_empty_file () =
  with_temporary_directory (fun directory ->
      let path = Filename.concat directory "target" in
      write_file path "old";
      Atomic_file.write ~path "";
      equal_string "replace empty bytes" "" (read_file path);
      equal_int "replace empty size" 0 (Unix.stat path).Unix.st_size)

let test_default_permissions_respect_the_umask () =
  if not Sys.win32 then
    with_temporary_directory (fun directory ->
        let path = Filename.concat directory "target" in
        with_umask 0o022 (fun () -> Atomic_file.write ~path "x");
        equal_int "default permissions under umask 022" 0o644
          ((Unix.stat path).Unix.st_perm land 0o777))

let test_explicit_permissions_respect_the_umask () =
  if not Sys.win32 then
    with_temporary_directory (fun directory ->
        let strict = Filename.concat directory "strict" in
        with_umask 0o022 (fun () ->
            Atomic_file.write ~perm:0o600 ~path:strict "x");
        equal_int "explicit 0o600 under umask 022" 0o600
          ((Unix.stat strict).Unix.st_perm land 0o777);
        let masked = Filename.concat directory "masked" in
        with_umask 0o077 (fun () ->
            Atomic_file.write ~perm:0o666 ~path:masked "x");
        equal_int "0o666 masked by umask 077" 0o600
          ((Unix.stat masked).Unix.st_perm land 0o777))

(* Failure paths *)

let test_invalid_permissions_do_no_io () =
  with_temporary_directory (fun directory ->
      let path = Filename.concat directory "target" in
      List.iter
        (fun perm ->
          let raised =
            try
              Atomic_file.write ~perm ~path "contents";
              false
            with Invalid_argument message ->
              equal_string "invalid permission message"
                "Atomic_file.write: perm must contain only bits within 0o777"
                message;
              true
          in
          check (Printf.sprintf "invalid permission %d raises" perm) raised)
        [ -1; 0o1000 ];
      equal_entries "invalid permission leaves directory empty" []
        (sorted_directory directory))

let test_missing_parent_fails_and_creates_nothing () =
  with_temporary_directory (fun directory ->
      let missing = Filename.concat directory "missing" in
      let path = Filename.concat missing "target" in
      let message =
        expect_sys_error "missing parent" ~path (fun () ->
            Atomic_file.write ~path "x")
      in
      check "missing parent names the failing step"
        (contains message "cannot create temporary file");
      check "missing parent remains absent" (not (Sys.file_exists missing));
      equal_entries "missing parent leaves root empty" []
        (sorted_directory directory))

let test_directory_target_is_unchanged_and_temporary_is_removed () =
  with_temporary_directory (fun directory ->
      let path = Filename.concat directory "target" in
      Unix.mkdir path 0o700;
      write_file (Filename.concat path "sentinel") "untouched";
      let message =
        expect_sys_error "directory target" ~path (fun () ->
            Atomic_file.write ~path "replacement")
      in
      check "directory target fails at replace"
        (contains message "cannot replace");
      equal_string "directory target sentinel" "untouched"
        (read_file (Filename.concat path "sentinel"));
      equal_entries "directory target has no sibling temporary" [ "target" ]
        (sorted_directory directory))

let test_read_only_parent_directory_fails_cleanly () =
  (* The portable failure-injection route: a read-only parent makes temporary
     creation fail before the target is ever touched. Root ignores directory
     permissions, so the check is skipped when running as root. *)
  if (not Sys.win32) && Unix.geteuid () <> 0 then
    with_temporary_directory (fun directory ->
        let locked = Filename.concat directory "locked" in
        Unix.mkdir locked 0o700;
        let path = Filename.concat locked "target" in
        write_file path "previous contents";
        Unix.chmod locked 0o500;
        Fun.protect
          ~finally:(fun () -> Unix.chmod locked 0o700)
          (fun () ->
            let message =
              expect_sys_error "read-only parent" ~path (fun () ->
                  Atomic_file.write ~path "replacement")
            in
            check "read-only parent names the failing step"
              (contains message "cannot create temporary file");
            equal_string "read-only parent leaves the target untouched"
              "previous contents" (read_file path);
            equal_entries "read-only parent gains no temporary" [ "target" ]
              (sorted_directory locked)))

let test_replacement_takes_the_temporary_permissions () =
  (* Frozen documented behavior: rename replaces the target's previous
     permission bits with the temporary's. *)
  if not Sys.win32 then
    with_temporary_directory (fun directory ->
        let path = Filename.concat directory "target" in
        write_file path "read-only contents";
        Unix.chmod path 0o444;
        with_umask 0o022 (fun () -> Atomic_file.write ~path "replaced");
        equal_string "read-only target bytes replaced" "replaced"
          (read_file path);
        equal_int "read-only target permissions replaced" 0o644
          ((Unix.stat path).Unix.st_perm land 0o777))

let test_target_symlink_is_replaced_not_followed () =
  if not Sys.win32 then
    with_temporary_directory (fun directory ->
        let referent = Filename.concat directory "referent" in
        let path = Filename.concat directory "target" in
        write_file referent "referent bytes";
        Unix.symlink referent path;
        Atomic_file.write ~path "new target";
        check "symlink becomes regular"
          ((Unix.lstat path).Unix.st_kind = Unix.S_REG);
        equal_string "symlink replacement bytes" "new target" (read_file path);
        equal_string "symlink referent untouched" "referent bytes"
          (read_file referent);
        equal_entries "symlink replacement siblings" [ "referent"; "target" ]
          (sorted_directory directory))

(* Atomicity under concurrency *)

let writer_contents writer round =
  Printf.sprintf "writer=%d round=%d\000%s" writer round
    (String.make (4096 + writer) (Char.chr (65 + writer)))

let child_replace path rounds writer =
  for round = 0 to rounds - 1 do
    try Atomic_file.write ~path (writer_contents writer round)
    with Sys_error message ->
      prerr_endline ("child replacement failed: " ^ message);
      exit 3
  done;
  exit 0

(* In a fresh process the internal temporary serial starts at zero, so
   pre-creating [count] decoys under this process's pid makes the first
   [count] temporary names collide deterministically. *)
let child_collide directory count expect_failure =
  let pid = Unix.getpid () in
  for serial = 0 to count - 1 do
    let name = Printf.sprintf "%s%x-%x" Atomic_file.temp_prefix pid serial in
    write_file (Filename.concat directory name) "decoy"
  done;
  let path = Filename.concat directory "target" in
  match Atomic_file.write ~path "after collisions" with
  | () ->
      if expect_failure then begin
        prerr_endline "collision child: write unexpectedly succeeded";
        exit 3
      end
      else exit 0
  | exception Sys_error message ->
      if expect_failure && contains message "cannot create temporary file" then
        exit 0
      else begin
        prerr_endline ("collision child: " ^ message);
        exit 3
      end

let wait_for_child label pid =
  match snd (Unix.waitpid [] pid) with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED code -> fail label "child exited %d" code
  | Unix.WSIGNALED signal -> fail label "child signaled %d" signal
  | Unix.WSTOPPED signal -> fail label "child stopped %d" signal

let spawn_collision_child directory count expect =
  let arguments =
    [|
      Sys.executable_name;
      "--atomic-file-collision-child";
      directory;
      string_of_int count;
      expect;
    |]
  in
  Unix.create_process Sys.executable_name arguments Unix.stdin Unix.stdout
    Unix.stderr

let test_colliding_temporary_names_are_skipped_not_clobbered () =
  with_temporary_directory (fun directory ->
      let pid = spawn_collision_child directory 4 "success" in
      wait_for_child "collision retry child" pid;
      let entries = sorted_directory directory in
      equal_int "collision retry entry count" 5 (List.length entries);
      check "collision retry target present" (List.mem "target" entries);
      equal_string "collision retry target bytes" "after collisions"
        (read_file (Filename.concat directory "target"));
      List.iter
        (fun name ->
          if name <> "target" then begin
            check
              (Printf.sprintf "decoy %S keeps the reserved prefix" name)
              (Atomic_file.is_temp_name name);
            equal_string
              (Printf.sprintf "decoy %S left untouched" name)
              "decoy"
              (read_file (Filename.concat directory name))
          end)
        entries)

let test_exhausted_temporary_names_fail_without_clobbering () =
  with_temporary_directory (fun directory ->
      (* 512 decoys exceed any plausible retry budget, so the write must give
         up with the temporary-creation error and touch nothing. *)
      let decoys = 512 in
      let pid = spawn_collision_child directory decoys "failure" in
      wait_for_child "collision exhaustion child" pid;
      let entries = sorted_directory directory in
      equal_int "exhaustion entry count" decoys (List.length entries);
      check "exhaustion never published a target"
        (not (List.mem "target" entries));
      check "exhaustion left every decoy in place"
        (List.for_all Atomic_file.is_temp_name entries))

let test_concurrent_processes_publish_only_whole_inputs () =
  with_temporary_directory (fun directory ->
      let path = Filename.concat directory "target" in
      let writers = 6 in
      let rounds = 24 in
      let children =
        List.init writers (fun writer ->
            let arguments =
              [|
                Sys.executable_name;
                "--atomic-file-child";
                path;
                string_of_int rounds;
                string_of_int writer;
              |]
            in
            Unix.create_process Sys.executable_name arguments Unix.stdin
              Unix.stdout Unix.stderr)
      in
      List.iteri
        (fun writer pid ->
          wait_for_child (Printf.sprintf "concurrent writer %d" writer) pid)
        children;
      let actual = read_file path in
      let candidates =
        List.init writers (fun writer -> writer_contents writer (rounds - 1))
      in
      check "concurrent final value is one complete final input"
        (List.exists (String.equal actual) candidates);
      equal_entries "concurrent writers leave no temporaries" [ "target" ]
        (sorted_directory directory))

let suite =
  [
    ("temp prefix is reserved", test_temp_prefix_is_reserved);
    ("creates exact binary file", test_creates_exact_binary_file);
    ("replaces existing file", test_replaces_existing_file);
    ("replaces with empty file", test_replaces_with_empty_file);
    ( "default permissions respect the umask",
      test_default_permissions_respect_the_umask );
    ( "explicit permissions respect the umask",
      test_explicit_permissions_respect_the_umask );
    ("invalid permissions do no I/O", test_invalid_permissions_do_no_io);
    ( "missing parent fails and creates nothing",
      test_missing_parent_fails_and_creates_nothing );
    ( "directory target is unchanged and temporary is removed",
      test_directory_target_is_unchanged_and_temporary_is_removed );
    ( "read-only parent directory fails cleanly",
      test_read_only_parent_directory_fails_cleanly );
    ( "replacement takes the temporary permissions",
      test_replacement_takes_the_temporary_permissions );
    ( "target symlink is replaced, not followed",
      test_target_symlink_is_replaced_not_followed );
    ( "colliding temporary names are skipped, not clobbered",
      test_colliding_temporary_names_are_skipped_not_clobbered );
    ( "exhausted temporary names fail without clobbering",
      test_exhausted_temporary_names_fail_without_clobbering );
    ( "concurrent processes publish only whole inputs",
      test_concurrent_processes_publish_only_whole_inputs );
  ]

let tests = List.map (fun (name, fn) -> test name fn) suite

(* The concurrency tests re-exec this executable as helper children; main
   must dispatch here before starting the runner. Never returns for a
   child invocation. *)
let dispatch_child () =
  match Array.to_list Sys.argv with
  | [ _; "--atomic-file-child"; path; rounds; writer ] ->
      child_replace path (int_of_string rounds) (int_of_string writer)
  | [ _; "--atomic-file-collision-child"; directory; count; expect ] ->
      child_collide directory (int_of_string count) (expect = "failure")
  | _ -> ()
