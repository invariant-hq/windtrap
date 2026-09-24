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

  (* The reader is generic over the variable name — a mirror is named in
     [Cli]'s flag table, not here — so each test names a real variable and
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
      test "value mirrors are passed through unparsed, like the seed" (fun () ->
          (* The CLI layer owns validation: a malformed winning
             token must reach it verbatim so it can error naming the
             variable, never vanish into a silent default. *)
          setenv "WINDTRAP_PROP_COUNT" (Some "500");
          equal ~msg:"prop_count raw" (option string) (Some "500")
            (string_of "WINDTRAP_PROP_COUNT");
          setenv "WINDTRAP_PROP_COUNT" (Some "1O0");
          equal ~msg:"malformed prop_count is passed through" (option string)
            (Some "1O0")
            (string_of "WINDTRAP_PROP_COUNT");
          setenv "WINDTRAP_PROP_COUNT" (Some "");
          equal ~msg:"prop_count unset" (option string) None
            (string_of "WINDTRAP_PROP_COUNT");
          setenv "WINDTRAP_TIMEOUT" (Some "2.5");
          equal ~msg:"timeout raw" (option string) (Some "2.5")
            (string_of "WINDTRAP_TIMEOUT");
          setenv "WINDTRAP_TIMEOUT" (Some "soon");
          equal ~msg:"malformed timeout is passed through" (option string)
            (Some "soon")
            (string_of "WINDTRAP_TIMEOUT");
          setenv "WINDTRAP_TIMEOUT" (Some "");
          equal ~msg:"timeout unset" (option string) None
            (string_of "WINDTRAP_TIMEOUT");
          setenv "WINDTRAP_SEED" (Some "s1:7be1d2c904aa31f5");
          equal ~msg:"seed raw" (option string) (Some "s1:7be1d2c904aa31f5")
            (string_of "WINDTRAP_SEED");
          setenv "WINDTRAP_SEED" (Some "");
          equal ~msg:"seed unset" (option string) None
            (string_of "WINDTRAP_SEED"));
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
             it says, and Auto only — an explicit request still wins. *)
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
          (* Composing the two — a mode read from the environment applied to
             a named sink — is the caller's job, not this module's:
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
          is_false ~msg:"unset TERM is not dumb" (Os.term_dumb ()));
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
        let message =
          expect_sys_error "read-only parent" ~path (fun () ->
              Os.atomic_write ~path "replacement")
        in
        is_true ~msg:"read-only parent names the failing step"
          (contains message "cannot create temporary file");
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
       regular file for it while its referent kept the old bytes — reported
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
    ]

  let tests = List.map (fun (name, fn) -> test name fn) suite

  let dispatch_child () =
    match Array.to_list Sys.argv with
    | [ _; "--atomic-file-child"; path; rounds; writer ] ->
        child_replace path (int_of_string rounds) (int_of_string writer)
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
      test "sanitize_component" (fun () ->
          equal ~msg:"safe name unchanged" string "abc-1_2.x"
            (Os.sanitize_component "abc-1_2.x");
          (* A name the mapping altered carries a digest of the original:
             without it the mapping is many-to-one and two tests share one
             capture log, which Capture opens O_TRUNC. The shape is the
             contract, not the digest bytes — pinning the hex would break on
             any digest change without catching a defect, and the injectivity
             it stands for is asserted directly just below. *)
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
            (Os.sanitize_component (String.make 100 'b')));
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
          (* INSIDE_DUNE is dune's build context — never a sandbox path, and
             a private --build-dir when one was given — and the root is the
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
          (* A boolean spelling — a harness's INSIDE_DUNE=1 — names no build
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
  let tests =
    [
      test "say is one anchored line on stderr" (fun () ->
          Os.say "could not write the verdict file: disk full";
          equal string "windtrap: could not write the verdict file: disk full\n"
            (output ()));
      test "standard output is flushed first, channel and formatter" (fun () ->
          print_string "channel, unflushed; ";
          Format.printf "formatter, unflushed@\n";
          Os.say "after both";
          equal string
            "channel, unflushed; formatter, unflushed\nwindtrap: after both\n"
            (output ()));
      test "warn says the run goes on, behind the same anchor" (fun () ->
          Os.warn "could not write JUnit report: disk full";
          equal string
            "windtrap: warning: could not write JUnit report: disk full\n"
            (output ()));
      test "a message of several lines is anchored on its first" (fun () ->
          Os.say
            "duplicate test paths:\n  a\nEvery full test path must be unique.";
          equal string
            "windtrap: duplicate test paths:\n\
            \  a\n\
             Every full test path must be unique.\n"
            (output ()));
      test "a control byte other than a line feed cannot restyle the terminal"
        (fun () ->
          Os.say "invalid value 'a\tb\027[31mc\127'";
          equal string "windtrap: invalid value 'a\\tb\\x1b[31mc\\x7f'\n"
            (output ()));
    ]
end

(* The concurrency test re-execs this executable as helper children, so the
   suite's toplevel dispatches here before its run. Never returns for a
   child invocation. *)
let () = Atomic_suite.dispatch_child ()

let tests =
  [
    group "clock" Clock_suite.tests;
    group "env" Env_suite.tests;
    group "atomic" Atomic_suite.tests;
    group "paths" Path_suite.tests;
    group "say" Say_suite.tests;
  ]

let () = exit @@ Windtrap.run "os" tests
