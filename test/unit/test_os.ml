(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Os: the monotonic clock, the environment readers and the one
   writer, atomic file publication, the project root, log root, sandbox
   reconstruction and display paths, and the standard-error line. One
   submodule per concern, each with its own helpers; the suite runs them
   as five groups. *)

open Windtrap
module Os = Windtrap.Private.Os

(* Monotonic clock *)

module Clock_suite = struct
  let tests =
    [
      test "count is non-negative and monotonic" (fun () ->
          let c = Os.counter () in
          is_true ~msg:"count is non-negative" (Os.count c >= 0L);
          let a = Os.count c in
          let b = Os.count c in
          is_true ~msg:"count is monotonic" (b >= a));
      test "sleeping is measured" (fun () ->
          (* 5ms of sleep reads as at least 1ms elapsed (loose bound to avoid
             scheduler flakiness). *)
          let c = Os.counter () in
          Unix.sleepf 0.005;
          is_true (Os.count c >= 1_000_000L));
      test "count_s agrees with count within float rounding" (fun () ->
          (* Read between two counts of the same counter, the seconds lie
             between the two in nanoseconds: a unit slip either way (a
             count in milliseconds, or in nanoseconds) falls outside, and
             no wall-clock bound is involved. *)
          let c = Os.counter () in
          Unix.sleepf 0.001;
          let before = Os.count c in
          let s = Os.count_s c in
          let after = Os.count c in
          let seconds ns = Int64.to_float ns /. 1_000_000_000. in
          is_true ~msg:"count_s is no less than the count before it"
            (s >= seconds before);
          is_true ~msg:"count_s is no more than the count after it"
            (s <= seconds after));
      test "count_s is never negative" (fun () ->
          at_least float_exact ~than:0. (Os.count_s (Os.counter ())));
    ]
end

(* Environment variables *)

module Env_suite = struct
  (* Every write goes through the runner's [setenv], which restores the
     variable when the attempt ends, so no test hands its bindings to the
     next. The readers treat the empty string as unset, and the tests clear
     a variable by binding it to [""]: that reading is what makes them
     deterministic whatever the ambient environment holds (INSIDE_DUNE under
     dune runtest), and [Os.setenv]'s real unbinding is proven on its own
     below. *)

  (* The reader is generic over the variable name (a mirror is named in
     [Cli]'s flag table, not here) so each test names a real variable and
     exercises the lookup its mirror uses; the vocabularies are pure
     functions over the value. *)
  let string_of = Os.getenv

  let tests =
    [
      test "set binds, and unbinds for real" (fun () ->
          let var = "WINDTRAP_TEST_ENV_SET" in
          (* The runner restores the variable's absence, whatever happens
             to [Os.setenv] below. *)
          setenv var None;
          Os.setenv var (Some "bound");
          equal ~msg:"the binding reaches the stdlib, not just Env's readers"
            (option string) (Some "bound") (Sys.getenv_opt var);
          Os.setenv var None;
          equal ~msg:"unbinding removes the variable" (option string) None
            (Sys.getenv_opt var);
          (* The whole reason the unbinding half is a C stub: the spelling
             [Unix] can manage leaves the variable set to the empty string,
             which is a different fact to every program that asks. *)
          Unix.putenv var "";
          equal ~msg:"an empty binding is not an unbinding" (option string)
            (Some "") (Sys.getenv_opt var);
          Os.setenv var None);
      test "set refuses the names POSIX refuses" (fun () ->
          let refused =
            Exn.invalid_arg ~substring:"environment variable name"
          in
          raises_match ~msg:"a name carrying '='" refused (fun () ->
              Os.setenv "WINDTRAP=BAD" (Some "x"));
          raises_match ~msg:"the empty name" refused (fun () ->
              Os.setenv "" None));
      test "set refuses a bad name before any change" (fun () ->
          let var = "WINDTRAP_TEST_ENV_EQ" in
          setenv var None;
          raises_match Exn.invalid_arg (fun () ->
              Os.setenv (var ^ "=x") (Some "v"));
          equal ~msg:"no variable was bound" (option string) None
            (Sys.getenv_opt var));
      test "empty value reads as unset" (fun () ->
          setenv "WINDTRAP_FILTER" (Some "");
          equal (option string) None (string_of "WINDTRAP_FILTER");
          setenv "WINDTRAP_FILTER" (Some "users");
          equal ~msg:"set value is returned" (option string) (Some "users")
            (string_of "WINDTRAP_FILTER");
          setenv "WINDTRAP_FILTER" (Some "");
          setenv "WINDTRAP_EXCLUDE" (Some "");
          equal ~msg:"exclude unset" (option string) None
            (string_of "WINDTRAP_EXCLUDE");
          setenv "WINDTRAP_EXCLUDE" (Some "slow suite");
          equal ~msg:"exclude set" (option string) (Some "slow suite")
            (string_of "WINDTRAP_EXCLUDE");
          setenv "WINDTRAP_EXCLUDE" (Some ""));
      cases ~name:Fun.id "truthy bool spellings"
        [ "1"; "true"; "TRUE"; "yes"; "Y"; "on" ] (fun v ->
          equal (option bool) (Some true) (Os.bool_of_string v));
      cases ~name:Fun.id "falsy bool spellings"
        [ "0"; "false"; "no"; "N"; "off"; "OFF" ] (fun v ->
          equal (option bool) (Some false) (Os.bool_of_string v));
      test "bool vocabulary edges" (fun () ->
          (* A value outside the vocabulary is [None], which every reader
             refuses loudly rather than reading as unset: the CLI layer
             names the variable ([Cli]'s tests pin that). *)
          equal ~msg:"an unparseable bool is neither" (option bool) None
            (Os.bool_of_string "bogus");
          equal ~msg:"the value is trimmed" (option bool) (Some true)
            (Os.bool_of_string " true ");
          equal ~msg:"an empty value is neither" (option bool) None
            (Os.bool_of_string "");
          contains ~msg:"the expected clause names the spellings" ~sub:"1/0"
            Os.bool_expected);
      test "bool_expected names every spelling but y and n" (fun () ->
          let words =
            String.split_on_char ' '
              (String.map
                 (fun c -> if c = '/' || c = ',' || c = ':' then ' ' else c)
                 Os.bool_expected)
          in
          List.iter
            (fun w -> mem ~msg:w string w words)
            [ "1"; "0"; "true"; "false"; "yes"; "no"; "on"; "off" ];
          List.iter (fun w -> is_false ~msg:w (List.mem w words)) [ "y"; "n" ]);
      test "comma lists split, trim, and drop empties" (fun () ->
          equal ~msg:"tags split and trimmed" (list string) [ "a"; "b"; "c" ]
            (Os.split_comma "a, b ,,c ");
          equal ~msg:"a lone label is a one-item list" (list string) [ "slow" ]
            (Os.split_comma "slow");
          equal ~msg:"separators alone are no labels" (list string) []
            (Os.split_comma " , "));
      test "CI detection: CI must be set and not falsy" (fun () ->
          setenv "CI" (Some "");
          setenv "GITHUB_ACTIONS" (Some "");
          is_false ~msg:"no CI" (Os.in_ci ());
          is_false ~msg:"no GitHub Actions" (Os.in_github_actions ());
          setenv "CI" (Some "true");
          is_true ~msg:"in_ci true" (Os.in_ci ());
          is_false ~msg:"CI alone is not GitHub Actions"
            (Os.in_github_actions ());
          setenv "GITHUB_ACTIONS" (Some "true");
          is_true ~msg:"CI plus GITHUB_ACTIONS" (Os.in_github_actions ());
          setenv "CI" (Some "false");
          is_false ~msg:"CI=false does not count as CI" (Os.in_ci ());
          is_false ~msg:"GITHUB_ACTIONS without CI is not GitHub Actions"
            (Os.in_github_actions ());
          setenv "CI" (Some "woodpecker");
          is_true ~msg:"non-boolean CI value counts as set" (Os.in_ci ());
          is_true ~msg:"a non-boolean CI still resolves GitHub Actions"
            (Os.in_github_actions ()));
      test "INSIDE_DUNE" (fun () ->
          setenv "INSIDE_DUNE" (Some "");
          is_false ~msg:"inside_dune false when cleared" (Os.inside_dune ());
          setenv "INSIDE_DUNE" (Some "1");
          is_true ~msg:"inside_dune true when set" (Os.inside_dune ());
          setenv "INSIDE_DUNE" (Some "false");
          is_false ~msg:"INSIDE_DUNE=false does not count" (Os.inside_dune ()));
      test "color mode vocabulary and the resolution rule" (fun () ->
          (* The vocabulary is [--color]'s; the flag's parser reads the
             variable through it, so an unknown word is refused there, not
             read as auto here. *)
          is_true ~msg:"color always"
            (Os.color_mode_of_string "always" = Some Os.Always);
          is_true ~msg:"color parsing is case-insensitive"
            (Os.color_mode_of_string "NEVER" = Some Os.Never);
          is_true ~msg:"color auto"
            (Os.color_mode_of_string "auto" = Some Os.Auto);
          is_true ~msg:"an unknown word is neither"
            (Os.color_mode_of_string "sometimes" = None);
          is_true ~msg:"always ignores tty"
            (Os.resolve_color Os.Always ~tty:false ~inside_dune:false
               ~term_dumb:false);
          is_false ~msg:"never ignores tty"
            (Os.resolve_color Os.Never ~tty:true ~inside_dune:true
               ~term_dumb:false);
          is_true ~msg:"auto on tty"
            (Os.resolve_color Os.Auto ~tty:true ~inside_dune:false
               ~term_dumb:false);
          is_true ~msg:"auto under dune"
            (Os.resolve_color Os.Auto ~tty:false ~inside_dune:true
               ~term_dumb:false);
          is_false ~msg:"auto plain pipe"
            (Os.resolve_color Os.Auto ~tty:false ~inside_dune:false
               ~term_dumb:false);
          (* TERM=dumb disables ANSI in Auto mode only: a dumb
             terminal renders no escape sequences, but an explicit request
             still wins. *)
          is_false ~msg:"auto on a dumb tty"
            (Os.resolve_color Os.Auto ~tty:true ~inside_dune:false
               ~term_dumb:true);
          is_false ~msg:"auto under dune with a dumb terminal"
            (Os.resolve_color Os.Auto ~tty:false ~inside_dune:true
               ~term_dumb:true);
          is_true ~msg:"always beats a dumb terminal"
            (Os.resolve_color Os.Always ~tty:true ~inside_dune:false
               ~term_dumb:true);
          (* NO_COLOR, the de-facto standard: any non-empty value, whatever
             it says, and Auto only. An explicit request still wins. *)
          setenv "NO_COLOR" (Some "1");
          is_false ~msg:"NO_COLOR silences auto on a tty"
            (Os.resolve_color Os.Auto ~tty:true ~inside_dune:false
               ~term_dumb:false);
          setenv "NO_COLOR" (Some "0");
          is_false ~msg:"NO_COLOR counts by presence, not by value"
            (Os.resolve_color Os.Auto ~tty:true ~inside_dune:false
               ~term_dumb:false);
          is_true ~msg:"always beats NO_COLOR"
            (Os.resolve_color Os.Always ~tty:false ~inside_dune:false
               ~term_dumb:false);
          setenv "NO_COLOR" (Some "");
          is_true ~msg:"an empty NO_COLOR is unset"
            (Os.resolve_color Os.Auto ~tty:true ~inside_dune:false
               ~term_dumb:false);
          (* Composing the two (a mode read from the environment applied to
             a named sink) is the caller's job, not this module's:
             [Report.terminal] does it for the runner and [coverage_cmd] for
             the coverage command, and both are pinned end to end by child
             runs that pass --color and compare bytes. *)
          ());
      test "TERM=dumb detection" (fun () ->
          setenv "TERM" (Some "dumb");
          is_true ~msg:"TERM=dumb detected" (Os.term_dumb ());
          setenv "TERM" (Some "xterm-256color");
          is_false ~msg:"a capable TERM is not dumb" (Os.term_dumb ());
          setenv "TERM" (Some "");
          is_false ~msg:"unset TERM is not dumb" (Os.term_dumb ());
          (* Compared as it is: no case folding, no trimming. *)
          setenv "TERM" (Some "DUMB");
          is_false ~msg:"another case is not dumb" (Os.term_dumb ());
          setenv "TERM" (Some "dumb ");
          is_false ~msg:"a padded value is not dumb" (Os.term_dumb ()));
      test "color_mode_of_string does not trim" (fun () ->
          is_none ~msg:"a leading space" (Os.color_mode_of_string " always");
          is_none ~msg:"a trailing newline" (Os.color_mode_of_string "never\n"));
    ]
end

(* Atomic file writes *)

module Atomic_suite = struct
  let contains text substring =
    Windtrap.Private.Text.contains_substring ~pattern:substring text

  let sorted_directory path =
    Sys.readdir path |> Array.to_list |> List.sort String.compare

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
    | _ -> failf "%s: expected Sys_error" label
    | exception Sys_error message ->
        is_true
          ~msg:(label ^ " message starts with the target path")
          (String.starts_with ~prefix:(path ^ ": ") message);
        message

  (* Reserved names *)

  let test_temp_prefix_is_reserved () =
    equal ~msg:"temp_prefix" string ".tmp-" Os.temp_prefix;
    is_true ~msg:"temp name is recognized" (Os.is_temp_name ".tmp-1a2b-0");
    is_true ~msg:"bare prefix is recognized" (Os.is_temp_name ".tmp-");
    is_true ~msg:"prefix elsewhere is not recognized"
      (not (Os.is_temp_name "x.tmp-1"));
    is_true ~msg:"shorter name is not recognized" (not (Os.is_temp_name ".tmp"));
    is_true ~msg:"an expect_file name is not recognized"
      (not (Os.is_temp_name "greeting.expected"))

  (* Writing *)

  let test_creates_exact_binary_file () =
    let directory = temp_dir () in
    let path = Filename.concat directory "target" in
    let contents =
      String.init ((256 * 1024) + 37) (fun index -> Char.chr (index land 0xff))
    in
    Os.atomic_write ~path contents;
    equal ~msg:"create binary bytes" string contents (read_file path);
    equal ~msg:"create binary siblings" (list string) [ "target" ]
      (sorted_directory directory)

  let test_replaces_existing_file () =
    let directory = temp_dir () in
    let path = Filename.concat directory "target" in
    write_file path "old bytes that must disappear";
    Os.atomic_write ~path "new\000bytes";
    equal ~msg:"replace existing bytes" string "new\000bytes" (read_file path);
    equal ~msg:"replace existing siblings" (list string) [ "target" ]
      (sorted_directory directory)

  let test_replaces_with_empty_file () =
    let directory = temp_dir () in
    let path = Filename.concat directory "target" in
    write_file path "old";
    Os.atomic_write ~path "";
    equal ~msg:"replace empty bytes" string "" (read_file path);
    equal ~msg:"replace empty size" int 0 (Unix.stat path).Unix.st_size

  let test_default_permissions_respect_the_umask () =
    if Sys.win32 then skip ~reason:"POSIX only" ();
    let directory = temp_dir () in
    let path = Filename.concat directory "target" in
    with_umask 0o022 (fun () -> Os.atomic_write ~path "x");
    equal ~msg:"default permissions under umask 022" int 0o644
      ((Unix.stat path).Unix.st_perm land 0o777)

  let test_explicit_permissions_respect_the_umask () =
    if Sys.win32 then skip ~reason:"POSIX only" ();
    let directory = temp_dir () in
    let strict = Filename.concat directory "strict" in
    with_umask 0o022 (fun () -> Os.atomic_write ~perm:0o600 ~path:strict "x");
    equal ~msg:"explicit 0o600 under umask 022" int 0o600
      ((Unix.stat strict).Unix.st_perm land 0o777);
    let masked = Filename.concat directory "masked" in
    with_umask 0o077 (fun () -> Os.atomic_write ~perm:0o666 ~path:masked "x");
    equal ~msg:"0o666 masked by umask 077" int 0o600
      ((Unix.stat masked).Unix.st_perm land 0o777)

  (* Failure paths *)

  let test_invalid_permissions_do_no_io () =
    let directory = temp_dir () in
    let path = Filename.concat directory "target" in
    List.iter
      (fun perm ->
        let raised =
          try
            Os.atomic_write ~perm ~path "contents";
            false
          with Invalid_argument message ->
            equal ~msg:"invalid permission message" string
              "Os.atomic_write: perm must contain only bits within 0o777"
              message;
            true
        in
        is_true ~msg:(Printf.sprintf "invalid permission %d raises" perm) raised)
      [ -1; 0o1000 ];
    equal ~msg:"invalid permission leaves directory empty" (list string) []
      (sorted_directory directory)

  let test_missing_parent_fails_and_creates_nothing () =
    let directory = temp_dir () in
    let missing = Filename.concat directory "missing" in
    let path = Filename.concat missing "target" in
    let message =
      expect_sys_error "missing parent" ~path (fun () ->
          Os.atomic_write ~path "x")
    in
    is_true ~msg:"missing parent names the failing step"
      (contains message "cannot create temporary file");
    is_true ~msg:"missing parent remains absent" (not (Sys.file_exists missing));
    equal ~msg:"missing parent leaves root empty" (list string) []
      (sorted_directory directory)

  let test_directory_target_is_unchanged_and_temporary_is_removed () =
    let directory = temp_dir () in
    let path = Filename.concat directory "target" in
    Unix.mkdir path 0o700;
    write_file (Filename.concat path "sentinel") "untouched";
    let message =
      expect_sys_error "directory target" ~path (fun () ->
          Os.atomic_write ~path "replacement")
    in
    is_true ~msg:"directory target fails at replace"
      (contains message "cannot replace");
    equal ~msg:"directory target sentinel" string "untouched"
      (read_file (Filename.concat path "sentinel"));
    equal ~msg:"directory target has no sibling temporary" (list string)
      [ "target" ]
      (sorted_directory directory)

  let test_read_only_parent_directory_fails_cleanly () =
    (* The portable failure-injection route: a read-only parent makes temporary
       creation fail before the target is ever touched. Root ignores directory
       permissions, so the check is skipped when running as root. *)
    if Sys.win32 then skip ~reason:"POSIX only" ();
    if Unix.geteuid () = 0 then
      skip ~reason:"root ignores directory permissions" ();
    let directory = temp_dir () in
    let locked = Filename.concat directory "locked" in
    Unix.mkdir locked 0o700;
    let path = Filename.concat locked "target" in
    write_file path "previous contents";
    Unix.chmod locked 0o500;
    Fun.protect
      ~finally:(fun () -> Unix.chmod locked 0o700)
      (fun () ->
        (* The failing step's name is the missing parent's test's. *)
        ignore
          (expect_sys_error "read-only parent" ~path (fun () ->
               Os.atomic_write ~path "replacement"));
        equal ~msg:"read-only parent leaves the target untouched" string
          "previous contents" (read_file path);
        equal ~msg:"read-only parent gains no temporary" (list string)
          [ "target" ] (sorted_directory locked))

  let test_replacement_takes_the_temporary_permissions () =
    (* Frozen documented behavior: rename replaces the target's previous
       permission bits with the temporary's. *)
    if Sys.win32 then skip ~reason:"POSIX only" ();
    let directory = temp_dir () in
    let path = Filename.concat directory "target" in
    write_file path "read-only contents";
    Unix.chmod path 0o444;
    with_umask 0o022 (fun () -> Os.atomic_write ~path "replaced");
    equal ~msg:"read-only target bytes replaced" string "replaced"
      (read_file path);
    equal ~msg:"read-only target permissions replaced" int 0o644
      ((Unix.stat path).Unix.st_perm land 0o777)

  let test_target_symlink_is_refused_not_followed () =
    (* Refusal subsumes the two protections this test has pinned in turn:
       writing through the link would modify a file the caller never named,
       and replacing the link (the previous contract) silently substituted a
       regular file for it while its referent kept the old bytes, reported
       as success to the caller. Publication never changes what kind of
       thing a path names; both sides survive byte-intact. *)
    if Sys.win32 then skip ~reason:"POSIX only" ();
    let directory = temp_dir () in
    let referent = Filename.concat directory "referent" in
    let path = Filename.concat directory "target" in
    write_file referent "referent bytes";
    Unix.symlink referent path;
    (match Os.atomic_write ~path "new target" with
    | () -> is_true ~msg:"a symlinked target must be refused" false
    | exception Sys_error message ->
        is_true ~msg:"the refusal names the linkness"
          (contains message "symbolic link"));
    is_true ~msg:"the link survives as a link"
      ((Unix.lstat path).Unix.st_kind = Unix.S_LNK);
    equal ~msg:"the referent keeps its bytes" string "referent bytes"
      (read_file referent);
    equal ~msg:"no temporary survives the refusal" (list string)
      [ "referent"; "target" ]
      (sorted_directory directory)

  (* Atomicity under concurrency *)

  let writer_contents writer round =
    Printf.sprintf "writer=%d round=%d\000%s" writer round
      (String.make (4096 + writer) (Char.chr (65 + writer)))

  let child_replace path rounds writer =
    for round = 0 to rounds - 1 do
      try Os.atomic_write ~path (writer_contents writer round)
      with Sys_error message ->
        prerr_endline ("child replacement failed: " ^ message);
        exit 3
    done;
    exit 0

  let wait_for_child label pid =
    match snd (Unix.waitpid [] pid) with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED code -> failf "%s: child exited %d" label code
    | Unix.WSIGNALED signal -> failf "%s: child signaled %d" label signal
    | Unix.WSTOPPED signal -> failf "%s: child stopped %d" label signal

  let test_concurrent_processes_publish_only_whole_inputs () =
    let directory = temp_dir () in
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
    is_true ~msg:"concurrent final value is one complete final input"
      (List.exists (String.equal actual) candidates);
    equal ~msg:"concurrent writers leave no temporaries" (list string)
      [ "target" ]
      (sorted_directory directory)

  (* Bounded retries on taken names *)

  (* A fresh process numbers its temporaries from 0, so the child takes the
     first 256 names of its own pid before it writes. *)
  let child_collide directory =
    for serial = 0 to 255 do
      let name = Printf.sprintf ".tmp-%x-%x" (Unix.getpid ()) serial in
      write_file (Filename.concat directory name) ""
    done;
    match Os.atomic_write ~path:(Filename.concat directory "target") "x" with
    | () -> exit 0
    | exception Sys_error message ->
        prerr_string message;
        exit 3

  let test_taken_names_are_retried_a_bounded_number_of_times () =
    let directory = temp_dir () in
    let module Child = Windtrap_test_support.Child in
    let r =
      Child.run Sys.executable_name [ "--atomic-collide-child"; directory ]
    in
    equal ~msg:"the write fails" int 3 (Child.exit_code r);
    is_true ~msg:"at the creation of its temporary"
      (contains r.Child.err "cannot create temporary file: File exists");
    is_false ~msg:"the target is not created"
      (Sys.file_exists (Filename.concat directory "target"));
    equal ~msg:"the taken names are left alone" int 256
      (Array.length (Sys.readdir directory))

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
      ( "target symlink is refused, not followed",
        test_target_symlink_is_refused_not_followed );
      ( "concurrent processes publish only whole inputs",
        test_concurrent_processes_publish_only_whole_inputs );
      ( "taken names are retried a bounded number of times",
        test_taken_names_are_retried_a_bounded_number_of_times );
    ]

  let tests = List.map (fun (name, fn) -> test name fn) suite

  let dispatch_child () =
    match Array.to_list Sys.argv with
    | [ _; "--atomic-file-child"; path; rounds; writer ] ->
        child_replace path (int_of_string rounds) (int_of_string writer)
    | [ _; "--atomic-collide-child"; directory ] -> child_collide directory
    | _ -> ()
end

(* Project root, reconstruction and display paths *)

module Path_suite = struct
  let fst3 (a, _, _) = a
  let is_hex = function '0' .. '9' | 'a' .. 'f' -> true | _ -> false

  let tests =
    [
      test "reconstruct proves containment under the root" (fun () ->
          let path = result string string in
          let ok input = Os.reconstruct ~root:"/proj" input in
          equal ~msg:"relative source resolves under root" path
            (Ok "/proj/test/foo.ml") (ok "test/foo.ml");
          equal ~msg:"sandbox path resolves under root" path
            (Ok "/proj/test/foo.ml")
            (ok "_build/default/test/foo.ml");
          equal ~msg:"absolute sandbox path resolves" path
            (Ok "/proj/test/foo.ml")
            (ok "/proj/_build/default/test/foo.ml");
          equal ~msg:"absolute in-root path resolves" path
            (Ok "/proj/test/foo.ml") (ok "/proj/test/foo.ml");
          equal ~msg:"dot and double-slash segments normalize" path
            (Ok "/proj/test/a/b.ml") (ok "test/./a//b.ml");
          equal ~msg:"internal dotdot stays contained" path
            (Ok "/proj/test/foo.ml") (ok "test/sub/../foo.ml");
          equal ~msg:"root with trailing slash accepted" path
            (Ok "/proj/test/foo.ml")
            (Os.reconstruct ~root:"/proj/" "test/foo.ml");
          equal ~msg:"filesystem root as root works" path (Ok "/test/foo.ml")
            (Os.reconstruct ~root:"/" "test/foo.ml");
          equal ~msg:"backslashes in the source path normalize" path
            (Ok "/proj/test/foo.ml")
            (Os.reconstruct ~root:"/proj" "_build\\default\\test\\foo.ml"));
      test "reconstruct rejects escapes" (fun () ->
          let ok input = Os.reconstruct ~root:"/proj" input in
          let is_error = function Error _ -> true | Ok _ -> false in
          is_true ~msg:"absolute path outside root fails"
            (is_error (ok "/elsewhere/foo.ml"));
          is_true ~msg:"dotdot escaping root fails"
            (is_error (ok "../escape.ml"));
          is_true ~msg:"nested dotdot escape fails"
            (is_error (ok "test/../../escape.ml"));
          is_true ~msg:"sandbox dotdot escape fails"
            (is_error (ok "_build/default/../../etc/passwd"));
          is_true ~msg:"root itself is not a source file" (is_error (ok "."));
          is_true ~msg:"empty path fails" (is_error (ok ""));
          is_true ~msg:"prefix sibling directory fails"
            (is_error (Os.reconstruct ~root:"/proj" "/proj2/test/foo.ml"));
          is_true ~msg:"relative root cannot prove containment"
            (is_error (Os.reconstruct ~root:"proj" "test/foo.ml"));
          equal ~msg:"error carries the unproven candidate"
            (result string string) (Error "/elsewhere/foo.ml")
            (ok "/elsewhere/foo.ml"));
      test "reconstruct strips a sandboxed action's build prefix" (fun () ->
          equal (result string string) (Ok "/proj/test/foo.ml")
            (Os.reconstruct ~root:"/proj"
               "_build/.sandbox/3f/default/test/foo.ml"));
      test "reconstruct's error candidate is resolved, not normalized"
        (fun () ->
          let ok input = Os.reconstruct ~root:"/proj" input in
          equal ~msg:"an escape" (result string string)
            (Error "/proj/test/../../escape.ml")
            (ok "test/../../escape.ml");
          equal ~msg:"an escape out of a build tree" (result string string)
            (Error "/proj/../../etc/passwd")
            (ok "_build/default/../../etc/passwd");
          equal ~msg:"an absolute path elsewhere" (result string string)
            (Error "/elsewhere/./a//foo.ml")
            (ok "/elsewhere/./a//foo.ml"));
      test "reconstruct is lexical" (fun () ->
          equal ~msg:"a root that does not exist" (result string string)
            (Ok "/no/such/root/a.ml")
            (Os.reconstruct ~root:"/no/such/root" "a.ml");
          if Sys.win32 then skip ~reason:"POSIX only" ();
          (* The link points out of the root; resolving it would refuse the
             path or move it. *)
          let root = temp_dir () in
          Unix.symlink "/elsewhere" (Filename.concat root "link");
          equal ~msg:"a symbolic link is not resolved" (result string string)
            (Ok (root ^ "/link/x.ml"))
            (Os.reconstruct ~root "link/x.ml"));
      test "reconstruct reads a drive as the anchor of an absolute path"
        (fun () ->
          let path = result string string in
          equal ~msg:"a sandbox path under a drive root" path
            (Ok "C:/w/test/a.ml")
            (Os.reconstruct ~root:"C:/w" "C:\\w\\_build\\default\\test\\a.ml");
          equal ~msg:"a lowercase drive" path (Ok "c:/w/a.ml")
            (Os.reconstruct ~root:"c:/w" "c:/w/a.ml");
          equal ~msg:"another drive is elsewhere" path (Error "D:/w/a.ml")
            (Os.reconstruct ~root:"C:/w" "D:/w/a.ml");
          equal ~msg:"the bare root of a drive is absolute" path (Error "C:/")
            (Os.reconstruct ~root:"/w" "C:/");
          equal ~msg:"a drive without its separator is relative" path
            (Ok "/w/C:a.ml")
            (Os.reconstruct ~root:"/w" "C:a.ml");
          equal ~msg:"a digit is no drive" path (Ok "/w/1:/a.ml")
            (Os.reconstruct ~root:"/w" "1:/a.ml"));
      test "reconstruct's candidate spells the root without trailing slashes"
        (fun () ->
          equal (result string string) (Error "w/a.ml")
            (Os.reconstruct ~root:"w//" "a.ml"));
      test "build_root cuts after the build directory and its context"
        (fun () ->
          let root = option string in
          equal ~msg:"a build context" root (Some "/w/_build/default")
            (Os.build_root "/w/_build/default/test");
          equal ~msg:"a sandboxed action's context" root
            (Some "/w/_build/.sandbox/3f/default")
            (Os.build_root "/w/_build/.sandbox/3f/default/test");
          equal ~msg:"backslashes" root (Some "/w/_build/default")
            (Os.build_root "\\w\\_build\\default\\test");
          equal ~msg:"a build directory with no context" root None
            (Os.build_root "/w/_build");
          equal ~msg:"no build directory" root None (Os.build_root "/w/src"));
      test "sanitize_component" (fun () ->
          equal ~msg:"safe name unchanged" string "abc-1_2.x"
            (Os.sanitize_component "abc-1_2.x");
          (* A name the mapping altered carries a digest of the original:
             without it the mapping is many-to-one and two tests share one
             capture log, which Capture opens O_TRUNC. The injectivity it
             stands for is asserted directly just below, and the digest's
             bytes by the MD5 test. *)
          let altered = Os.sanitize_component "a b/c" in
          equal ~msg:"unsafe chars become underscores" string "a_b_c"
            (String.sub altered 0 (min 5 (String.length altered)));
          is_true ~msg:"a hex digest of the original is appended"
            (String.length altered = 5 + 1 + 8
            && altered.[5] = '-'
            && String.for_all is_hex (String.sub altered 6 8));
          not_equal ~msg:"punctuation variants stay distinct" string
            (Os.sanitize_component "parse: empty")
            (Os.sanitize_component "parse, empty");
          is_true ~msg:"both still start with the readable form"
            (String.starts_with ~prefix:"parse__empty"
               (Os.sanitize_component "parse: empty")
            && String.starts_with ~prefix:"parse__empty"
                 (Os.sanitize_component "parse, empty"));
          not_equal ~msg:"empty and dot stay distinct" string
            (Os.sanitize_component "")
            (Os.sanitize_component ".");
          is_true ~msg:"empty becomes unnamed"
            (String.starts_with ~prefix:"unnamed-" (Os.sanitize_component ""));
          is_true ~msg:"dotdot becomes unnamed"
            (String.starts_with ~prefix:"unnamed-" (Os.sanitize_component ".."));
          let long = String.make 100 'a' in
          let sanitized = Os.sanitize_component long in
          equal ~msg:"long names truncated with digest" int 73
            (String.length sanitized);
          equal ~msg:"long name keeps prefix" string (String.make 40 'a')
            (String.sub sanitized 0 40);
          equal ~msg:"sanitize is deterministic" string sanitized
            (Os.sanitize_component long);
          not_equal ~msg:"distinct long names stay distinct" string sanitized
            (Os.sanitize_component (String.make 100 'b'));
          equal ~msg:"a result of exactly 80 bytes is kept whole" string
            (String.make 80 'a')
            (Os.sanitize_component (String.make 80 'a'));
          equal ~msg:"one of 81 bytes is cut" int 73
            (String.length (Os.sanitize_component (String.make 81 'a'))));
      (* Digests computed apart from windtrap, with Python's hashlib. *)
      test "sanitize_component's digest is MD5" (fun () ->
          equal ~msg:"the first 8 digits on a changed name" string
            "a_b_c-22ce5bc5"
            (Os.sanitize_component "a b/c");
          equal ~msg:"the whole digest on a long name" string
            (String.make 40 'a' ^ "_36a92cc94a9e0fa21f625f8bfb007adf")
            (Os.sanitize_component (String.make 100 'a'));
          equal ~msg:"the empty name" string "unnamed-d41d8cd9"
            (Os.sanitize_component ""));
      test "mkdir_p creates nested directories and is idempotent" (fun () ->
          let deep = Filename.concat (temp_dir ()) "a/b/c" in
          Os.mkdir_p deep;
          is_true ~msg:"mkdir_p creates nested directories"
            (Sys.file_exists deep && Sys.is_directory deep);
          Os.mkdir_p deep;
          is_true ~msg:"mkdir_p is idempotent" (Sys.is_directory deep));
      test "file_exists" (fun () ->
          let dir = temp_dir () in
          is_true ~msg:"file_exists on a directory" (Os.file_exists dir);
          is_false ~msg:"file_exists on a missing path"
            (Os.file_exists (Filename.concat dir "missing")));
      test "file_exists is false on any error" (fun () ->
          let file = temp_file () in
          is_false ~msg:"a path under a regular file (ENOTDIR)"
            (Os.file_exists (Filename.concat file "x"));
          if Sys.win32 then skip ~reason:"POSIX only" ();
          if Unix.geteuid () = 0 then
            skip ~reason:"root ignores directory permissions" ();
          let locked = Filename.concat (temp_dir ()) "locked" in
          Unix.mkdir locked 0o700;
          let inside = Filename.concat locked "x" in
          close_out (open_out inside);
          Unix.chmod locked 0o000;
          Fun.protect
            ~finally:(fun () -> Unix.chmod locked 0o700)
            (fun () ->
              is_false ~msg:"a file under an unreadable directory (EACCES)"
                (Os.file_exists inside)));
      test "mkdir_p of the empty path or the current directory does nothing"
        (fun () ->
          Os.mkdir_p "";
          Os.mkdir_p ".");
      test "mkdir_p leaves an existing file alone" (fun () ->
          let file = temp_file () in
          Out_channel.with_open_bin file (fun oc -> output_string oc "kept");
          Os.mkdir_p file;
          equal ~msg:"the file keeps its bytes" string "kept"
            (In_channel.with_open_bin file In_channel.input_all);
          raises_match ~msg:"a directory under it cannot be made"
            (function
              | Unix.Unix_error (Unix.ENOTDIR, _, _) -> true | _ -> false)
            (fun () -> Os.mkdir_p (Filename.concat file "sub")));
      test "mkdir_p creates with 0o770 under the umask" (fun () ->
          if Sys.win32 then skip ~reason:"POSIX only" ();
          let deep = Filename.concat (temp_dir ()) "a/b" in
          let previous = Unix.umask 0o020 in
          Fun.protect
            ~finally:(fun () -> ignore (Unix.umask previous))
            (fun () -> Os.mkdir_p deep);
          List.iter
            (fun dir ->
              equal ~msg:dir int 0o750 ((Unix.stat dir).Unix.st_perm land 0o777))
            [ deep; Filename.dirname deep ]);
      test "failure_reason never repeats the path" (fun () ->
          equal ~msg:"a Sys_error loses the path it starts with" string
            "cannot write: No space left on device"
            (Os.failure_reason ~path:"a/b.xml"
               (Sys_error "a/b.xml: cannot write: No space left on device"));
          equal ~msg:"another Sys_error is kept whole" string "c.xml: gone"
            (Os.failure_reason ~path:"a/b.xml" (Sys_error "c.xml: gone"));
          equal ~msg:"mkdir_p's error names the directory" string
            "cannot create directory blocked/out: Not a directory"
            (Os.failure_reason ~path:"blocked/out/r.xml"
               (Unix.Unix_error (Unix.ENOTDIR, "mkdir", "blocked/out")));
          equal ~msg:"any other exception is printed" string "Not_found"
            (Os.failure_reason ~path:"a" Not_found));
      test "project_root: explicit override wins" (fun () ->
          setenv "WINDTRAP_PROJECT_ROOT" (Some "/tmp/override");
          equal ~msg:"override wins" string "/tmp/override" (Os.project_root ());
          setenv "WINDTRAP_PROJECT_ROOT" (Some "rel");
          is_true ~msg:"relative override absolutized"
            ((not (Filename.is_relative (Os.project_root ())))
            && Filename.basename (Os.project_root ()) = "rel"));
      test "build_dir_of_path: the first _build component" (fun () ->
          let dir = option string in
          let of_path = Os.build_dir_of_path in
          equal ~msg:"the build context" dir (Some "/w/_build")
            (of_path "/w/_build/default");
          equal ~msg:"a sandboxed action's directory" dir (Some "/w/_build")
            (of_path "/w/_build/.sandbox/3f/default");
          equal ~msg:"a private build directory" dir (Some "/w/_build_x")
            (of_path "/w/_build_x/default/test/t.exe");
          equal ~msg:"only the first build component counts" dir
            (Some "/w/_build")
            (of_path "/w/_build/default/a/_build_y/b");
          equal ~msg:"no build component" dir None (of_path "/w/src/t.exe");
          equal ~msg:"backslashes normalize" dir (Some "/w/_build")
            (of_path "\\w\\_build\\default\\t.exe");
          equal ~msg:"a relative path keeps its prefix" dir (Some "a/_build")
            (of_path "a/_build/default"));
      test
        "project_root and default_log_dir: the build directory, from \
         INSIDE_DUNE" (fun () ->
          (* INSIDE_DUNE is dune's build context (never a sandbox path, and
             a private --build-dir when one was given) and the root is the
             directory above its build component, the log root inside it. *)
          setenv "WINDTRAP_PROJECT_ROOT" None;
          let under context =
            setenv "INSIDE_DUNE" (Some context);
            (Os.build_dir (), Os.project_root (), Os.default_log_dir ())
          in
          let triple = triple (option string) string string in
          equal ~msg:"the default context" triple
            (Some "/w/_build", "/w", "/w/_build/_tests")
            (under "/w/_build/default");
          equal ~msg:"a sandboxed action's context is the same" triple
            (Some "/w/_build", "/w", "/w/_build/_tests")
            (under "/w/_build/.sandbox/3f/default");
          equal ~msg:"a private build directory keeps its own logs" triple
            (Some "/w/_build_priv", "/w", "/w/_build_priv/_tests")
            (under "/w/_build_priv/default");
          (* A boolean spelling (a harness's INSIDE_DUNE=1) names no build
             directory, and the executable's own path decides. *)
          let own = Os.build_dir_of_path Sys.executable_name in
          equal ~msg:"a non-path value falls through to the executable"
            (option string) own
            (fst3 (under "1")));
      test
        "project_root and default_log_dir: the executable's own build \
         directory, or the working directory" (fun () ->
          setenv "WINDTRAP_PROJECT_ROOT" None;
          setenv "INSIDE_DUNE" None;
          match Os.build_dir_of_path Sys.executable_name with
          | Some dir ->
              (* This binary lives under a build directory: a test executable
                 run by hand from there finds its root above it. *)
              equal ~msg:"the executable's build directory" (option string)
                (Some dir) (Os.build_dir ());
              equal ~msg:"the root is the directory above it" string
                (Filename.dirname dir) (Os.project_root ());
              equal ~msg:"the logs live inside it" string
                (Filename.concat dir "_tests")
                (Os.default_log_dir ())
          | None ->
              (* An installed copy: nothing names a build directory, so the
                 root is the working directory and the logs go to the
                 temporary directory, never to a fresh _build. *)
              equal ~msg:"no build directory" (option string) None
                (Os.build_dir ());
              equal ~msg:"the root is the working directory" string
                (Sys.getcwd ()) (Os.project_root ());
              equal ~msg:"the logs go to the temporary directory" string
                (Filename.concat (Filename.get_temp_dir_name ()) "windtrap")
                (Os.default_log_dir ()));
      test "a relative INSIDE_DUNE is made absolute against the cwd" (fun () ->
          (* A working directory outside every build tree, so the only
             build directory in sight is the one the variable names. *)
          let cwd = temp_dir () in
          chdir cwd;
          let cwd = Sys.getcwd () in
          setenv "WINDTRAP_PROJECT_ROOT" None;
          setenv "INSIDE_DUNE" (Some "w/_build/default");
          equal ~msg:"build_dir" (option string)
            (Some (cwd ^ "/w/_build"))
            (Os.build_dir ());
          equal ~msg:"project_root" string (cwd ^ "/w") (Os.project_root ()));
      test "a working directory that is gone" (fun () ->
          let gone = Filename.concat (temp_dir ()) "gone" in
          Unix.mkdir gone 0o700;
          chdir gone;
          Unix.rmdir gone;
          setenv "WINDTRAP_PROJECT_ROOT" None;
          setenv "INSIDE_DUNE" (Some "w/_build/default");
          raises_match ~msg:"project_root raises" Exn.sys_error (fun () ->
              Os.project_root ());
          raises_match ~msg:"default_log_dir raises" Exn.sys_error (fun () ->
              Os.default_log_dir ());
          equal ~msg:"display_path removes no prefix and does the rest" string
            "/r/a.ml"
            (Os.display_path "/r/_build/default/./a.ml");
          equal ~msg:"display_artifact returns the path as given" string
            "/r/_build/./a.ml"
            (Os.display_artifact "/r/_build/./a.ml"));
      test "WINDTRAP_PROJECT_ROOT is normalized lexically" (fun () ->
          List.iter
            (fun root ->
              setenv "WINDTRAP_PROJECT_ROOT" (Some root);
              equal ~msg:(root ^ ": project_root") string "/r"
                (Os.project_root ());
              equal ~msg:(root ^ ": display_path") string "a/b.ml"
                (Os.display_path "/r/a/b.ml");
              equal
                ~msg:(root ^ ": display_artifact")
                string "_build/x.log"
                (Os.display_artifact "/r/_build/x.log"))
            [ "/r/"; "/r/."; "/r//"; "/x/../r"; "//r/./" ];
          chdir (temp_dir ());
          let cwd = Sys.getcwd () in
          setenv "WINDTRAP_PROJECT_ROOT" (Some "./sub/../r/");
          equal ~msg:"a relative one, against the working directory" string
            (cwd ^ "/r") (Os.project_root ());
          setenv "WINDTRAP_PROJECT_ROOT" (Some "/..");
          equal ~msg:"one that climbs above the root is kept" string "/.."
            (Os.project_root ()));
      test "display_path outside the root strips the build segment first"
        (fun () ->
          setenv "WINDTRAP_PROJECT_ROOT" (Some "/r");
          equal string "a.ml" (Os.display_path "/_build/default/r/a.ml"));
      test "display_artifact removes the root prefix and nothing else"
        (fun () ->
          setenv "WINDTRAP_PROJECT_ROOT" (Some "/r");
          equal ~msg:"a capture log keeps its build segment" string
            "_build/_tests/s/t.output"
            (Os.display_artifact "/r/_build/_tests/s/t.output");
          equal ~msg:"no normalization" string "./a//b"
            (Os.display_artifact "/r/./a//b");
          equal ~msg:"outside the root, as given" string "/elsewhere/./x"
            (Os.display_artifact "/elsewhere/./x"));
      test "display spells report paths project-root relative" (fun () ->
          (* The one producer of [wrote]/hint path spellings for both the
             library and inline runners. *)
          let root = Os.project_root () in
          equal ~msg:"build prefix stripped, root-relative" string "qa/x/t.exe"
            (Os.display_path (root ^ "/_build/default/qa/x/t.exe"));
          equal ~msg:"absolute in-root path relativized" string
            "qa/x/greeting.snap"
            (Os.display_path (root ^ "/qa/x/greeting.snap"));
          equal ~msg:"interior dot and empty segments dropped" string
            "qa/x/t.exe"
            (Os.display_path (root ^ "/./qa//x/./t.exe"));
          equal ~msg:"dotdot untouched" string "qa/../qa/t.exe"
            (Os.display_path (root ^ "/qa/../qa/t.exe"));
          equal ~msg:"path outside the root normalized, not relativized" string
            "/elsewhere/a/t.exe"
            (Os.display_path "/elsewhere/./a//t.exe"));
      test "display strips exactly one _build sandbox prefix" (fun () ->
          (* The [_build/<context>/] rule [display] and [reconstruct] share.
             It has no export of its own, so the cases that neither the plain
             [display] nor the [reconstruct] test reaches are pinned here,
             through the surface that prints them. *)
          let root = Os.project_root () in
          equal ~msg:"any context is stripped, not just default" string
            "qa/x/t.exe"
            (Os.display_path (root ^ "/_build/release.x/qa/x/t.exe"));
          equal ~msg:"only the first _build segment is stripped" string
            "a/_build/ctx/b.ml"
            (Os.display_path (root ^ "/_build/default/a/_build/ctx/b.ml"));
          equal ~msg:"trailing _build without a context is kept" string
            "a/_build"
            (Os.display_path (root ^ "/a/_build"));
          equal ~msg:"a path with no _build is left alone" string "qa/x/t.exe"
            (Os.display_path "qa/x/t.exe");
          equal ~msg:"backslashes normalized" string "w/test/foo.ml"
            (Os.display_path "w\\_build\\default\\test\\foo.ml"));
    ]
end

(* Standard error *)

(* The capture holds both streams in the order their bytes reached the
   descriptors, which is what makes the flush order observable. *)
module Say_suite = struct
  (* The messages, each said by a child on its own streams, so the parent
     reads standard output and standard error apart; the capture of a test
     merges them. *)
  let messages =
    [
      ("say", fun () -> Os.say "could not write the verdict file: disk full");
      ("warn", fun () -> Os.warn "could not write JUnit report: disk full");
      ( "lines",
        fun () ->
          Os.say
            "duplicate test paths:\n  a\nEvery full test path must be unique."
      );
      ("control", fun () -> Os.say "invalid value 'a\tb\027[31mc\127'");
      ("bytes", fun () -> Os.say "a\rb \xc3\xa9 \xff");
      (* The children below leave through [_exit], so no flush at exit
         writes what [say] did not. *)
      ( "closed",
        fun () ->
          print_string "pending";
          Unix.close Unix.stdout;
          Os.say "still said";
          Unix._exit 0 );
      ( "err_formatter",
        fun () ->
          Format.eprintf "pending ";
          Os.say "line";
          Unix._exit 0 );
    ]

  (* Re-exec dispatch for the children above; the suite's toplevel calls it
     before its run. Never returns for a child invocation. *)
  let dispatch_child () =
    match Array.to_list Sys.argv with
    | [ _; "--say-child"; name ] ->
        (List.assoc name messages) ();
        exit 0
    | _ -> ()

  (* What the child said on stderr, once it exited 0 with an empty stdout. *)
  let said name =
    let module Child = Windtrap_test_support.Child in
    let r = Child.run Sys.executable_name [ "--say-child"; name ] in
    equal ~msg:"the child exits 0" int 0 (Child.exit_code r);
    equal ~msg:"nothing goes to standard output" string "" r.Child.out;
    r.Child.err

  let tests =
    [
      test "say is one anchored line on stderr" (fun () ->
          equal string "windtrap: could not write the verdict file: disk full\n"
            (said "say"));
      test "standard output is flushed first, channel and formatter" (fun () ->
          print_string "channel, unflushed; ";
          Format.printf "formatter, unflushed@\n";
          Os.say "after both";
          equal string
            "channel, unflushed; formatter, unflushed\nwindtrap: after both\n"
            (output ()));
      test "warn says the run goes on, behind the same anchor" (fun () ->
          equal string
            "windtrap: warning: could not write JUnit report: disk full\n"
            (said "warn"));
      test "a message of several lines is anchored on its first" (fun () ->
          equal string
            "windtrap: duplicate test paths:\n\
            \  a\n\
             Every full test path must be unique.\n"
            (said "lines"));
      test
        "a control byte other than a line feed or a tab cannot restyle the \
         terminal" (fun () ->
          equal string "windtrap: invalid value 'a\tb\\x1b[31mc\\x7f'\n"
            (said "control"));
      test "a carriage return is escaped, bytes from 0x80 pass" (fun () ->
          equal string "windtrap: a\\x0db \xc3\xa9 \xff\n" (said "bytes"));
      test "a closed standard output does not cost the line" (fun () ->
          equal string "windtrap: still said\n" (said "closed"));
      test "Format.err_formatter is flushed before the line, stderr after"
        (fun () ->
          equal string "pending windtrap: line\n" (said "err_formatter"));
    ]
end

(* Signals *)

module Signal_suite = struct
  (* No run handles [SIGUSR1] or [SIGUSR2], and each ends the process under
     its default disposition. *)
  let usr1 = Sys.sigusr1
  let usr2 = Sys.sigusr2
  let raise_signal signal = Unix.kill (Unix.getpid ()) signal

  (* A handler runs at a safepoint after [kill] returns: [until ready] polls
     for about a second. *)
  let until ?(tries = 1000) ready =
    let rec poll tries =
      if ready () || tries = 0 then ()
      else begin
        Unix.sleepf 0.001;
        poll (tries - 1)
      end
    in
    poll tries

  (* How a forked process that runs [f] ended. It leaves through [_exit], so
     the at-exit functions of this process do not run twice. *)
  let forked f =
    match Unix.fork () with
    | 0 ->
        (try f () with _ -> ());
        Unix._exit 0
    | pid ->
        let rec wait () =
          match Unix.waitpid [] pid with
          | _, status -> status
          | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
        in
        wait ()

  let tests =
    [
      test "the handler gets the signal, and the one found is back after"
        (fun () ->
          if Sys.win32 then skip ~reason:"POSIX only" ();
          let mine (_ : int) = () in
          let found = Sys.signal usr1 (Sys.Signal_handle mine) in
          let got = ref None in
          Os.with_signals [ usr1 ]
            (fun signal -> got := Some signal)
            (fun () ->
              raise_signal usr1;
              until (fun () -> Option.is_some !got));
          let after = Sys.signal usr1 found in
          equal ~msg:"the handler got the signal" (option int) (Some usr1) !got;
          is_true ~msg:"the handler found is back"
            (match after with
            | Sys.Signal_handle f -> f == mine
            | Sys.Signal_default | Sys.Signal_ignore -> false));
      test "a signal the process was started with ignored stays ignored"
        (fun () ->
          if Sys.win32 then skip ~reason:"POSIX only" ();
          let found = Sys.signal usr1 Sys.Signal_ignore in
          let got = ref false in
          Os.with_signals [ usr1 ]
            (fun _ -> got := true)
            (fun () ->
              raise_signal usr1;
              until ~tries:50 (fun () -> !got));
          Sys.set_signal usr1 found;
          is_false ~msg:"the handler did not run" !got);
      test
        "the handler runs with the signals at their default disposition, \
         unblocked" (fun () ->
          if Sys.win32 then skip ~reason:"POSIX only" ();
          let ended_by sent =
            forked (fun () ->
                Sys.set_signal usr1 Sys.Signal_default;
                Sys.set_signal usr2 Sys.Signal_default;
                Os.with_signals [ usr1; usr2 ]
                  (fun _ ->
                    raise_signal sent;
                    Unix._exit 3)
                  (fun () ->
                    raise_signal usr1;
                    until (fun () -> false)))
          in
          is_true ~msg:"the signal itself, sent again, ends the process at once"
            (ended_by usr1 = Unix.WSIGNALED usr1);
          is_true ~msg:"and so does another of the list"
            (ended_by usr2 = Unix.WSIGNALED usr2));
      test "SIGPIPE keeps the handler, and the others go back to the default"
        (fun () ->
          if Sys.win32 then skip ~reason:"POSIX only" ();
          let status =
            forked (fun () ->
                Sys.set_signal Sys.sigpipe Sys.Signal_default;
                Sys.set_signal usr1 Sys.Signal_default;
                let count = ref 0 in
                Os.with_signals [ Sys.sigpipe; usr1 ]
                  (fun _ -> incr count)
                  (fun () ->
                    raise_signal Sys.sigpipe;
                    until (fun () -> !count = 1);
                    raise_signal Sys.sigpipe;
                    until (fun () -> !count = 2);
                    if !count <> 2 then Unix._exit (10 + !count);
                    raise_signal usr1;
                    until (fun () -> !count = 3));
                Unix._exit !count)
          in
          is_true ~msg:"two SIGPIPEs ran the handler, then SIGUSR1 ended it"
            (status = Unix.WSIGNALED usr1));
      test "a process forked inside dies by the signal" (fun () ->
          if Sys.win32 then skip ~reason:"POSIX only" ();
          let status =
            Os.with_signals [ usr1 ] ignore (fun () ->
                forked (fun () ->
                    raise_signal usr1;
                    until (fun () -> false)))
          in
          is_true ~msg:"and not by the handler, which would let it exit 0"
            (status = Unix.WSIGNALED usr1));
      test "die_by ends the process by a signal it handles and blocks"
        (fun () ->
          if Sys.win32 then skip ~reason:"POSIX only" ();
          let status =
            forked (fun () ->
                Sys.set_signal Sys.sigterm (Sys.Signal_handle ignore);
                ignore (Unix.sigprocmask Unix.SIG_BLOCK [ Sys.sigterm ]);
                Os.die_by Sys.sigterm)
          in
          is_true (status = Unix.WSIGNALED Sys.sigterm));
    ]
end

(* The concurrency and say tests re-exec this executable as helper
   children, so the suite's toplevel dispatches here before its run. Never
   returns for a child invocation. *)
let () = Atomic_suite.dispatch_child ()
let () = Say_suite.dispatch_child ()

let tests =
  [
    group "clock" Clock_suite.tests;
    group "env" Env_suite.tests;
    group "atomic" Atomic_suite.tests;
    group "paths" Path_suite.tests;
    group "say" Say_suite.tests;
    group "signals" Signal_suite.tests;
  ]

let () = exit @@ Windtrap.run "os" tests
