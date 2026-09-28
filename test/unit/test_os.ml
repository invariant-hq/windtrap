(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Os = Windtrap.Private.Os

let strf = Printf.sprintf
let read path = In_channel.with_open_bin path In_channel.input_all
let write path s = Out_channel.with_open_bin path (fun oc -> output_string oc s)
let entries dir = List.sort String.compare (Array.to_list (Sys.readdir dir))
let posix_only () = if Sys.win32 then skip ~reason:"POSIX only" ()

let permissions_honoured () =
  posix_only ();
  if Unix.geteuid () = 0 then
    skip ~reason:"root ignores directory permissions" ()

let shown = function None -> "unset" | Some v -> strf "%S" v

(* Child processes *)

(* A child leaves through [_exit], so the [at_exit] functions of this process,
   the runner's among them, run once. The buffers are flushed before the fork,
   so the child does not write them again. *)
let fork f =
  Format.pp_print_flush Format.std_formatter ();
  Format.pp_print_flush Format.err_formatter ();
  flush_all ();
  match Unix.fork () with
  | 0 -> (
      match f () with
      | () -> Unix._exit 0
      | exception e ->
          prerr_endline (Printexc.to_string e);
          Unix._exit 2)
  | pid -> pid

let rec wait pid =
  match Unix.waitpid [] pid with
  | _, status -> status
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait pid

let signal_name signal =
  let names =
    [
      (Sys.sighup, "SIGHUP");
      (Sys.sigint, "SIGINT");
      (Sys.sigpipe, "SIGPIPE");
      (Sys.sigterm, "SIGTERM");
      (Sys.sigusr1, "SIGUSR1");
      (Sys.sigusr2, "SIGUSR2");
    ]
  in
  Option.value (List.assoc_opt signal names) ~default:(string_of_int signal)

let ended = function
  | Unix.WEXITED code -> strf "exited %d" code
  | Unix.WSIGNALED signal -> "killed by " ^ signal_name signal
  | Unix.WSTOPPED signal -> "stopped by " ^ signal_name signal

(* Monotonic clock *)

let never_decreases () =
  let c = Os.counter () in
  let a = Os.count_s c in
  at_least float_exact ~than:a (Os.count_s c)

(* The wall clock brackets the reading, so a count in another unit falls
   outside; the millisecond of slack absorbs the two clocks' resolutions. *)
let in_seconds () =
  let start = Unix.gettimeofday () in
  let c = Os.counter () in
  Unix.sleepf 0.005;
  let elapsed = Os.count_s c in
  let span = Unix.gettimeofday () -. start in
  at_least float_exact ~than:0.001 elapsed;
  at_most float_exact ~than:(span +. 0.001) elapsed

let clock =
  group "Monotonic clock"
    [
      test "count_s is never negative" (fun () ->
          at_least float_exact ~than:0. (Os.count_s (Os.counter ())));
      test "count_s of one counter never decreases" never_decreases;
      test "count_s counts seconds" in_seconds;
    ]

(* Environment variables *)

let var = "WINDTRAP_TEST_OS"

let set_then_unset () =
  setenv var None;
  Os.setenv var (Some "bound");
  let bound = Sys.getenv_opt var in
  Os.setenv var None;
  equal
    (pair (option string) (option string))
    (Some "bound", None)
    (bound, Sys.getenv_opt var)

let refused_name name =
  setenv var None;
  raises_match (Exn.invalid_arg ~substring:"environment variable name")
    (fun () -> Os.setenv name (Some "v"));
  equal (option string) None (Sys.getenv_opt var)

let spellings () =
  let words =
    String.split_on_char ' '
      (String.map (function '/' | ',' | ':' -> ' ' | c -> c) Os.bool_expected)
  in
  equal (list string)
    [ "1"; "0"; "true"; "false"; "yes"; "no"; "on"; "off" ]
    (List.filter (fun w -> Option.is_some (Os.bool_of_string w)) words)

let environment =
  group "Environment variables"
    [
      cases "getenv is the value as it is, and None when unset or empty"
        ~name:(fun (value, _) -> shown value)
        [
          (None, None);
          (Some "", None);
          (Some "users", Some "users");
          (Some " a, b ", Some " a, b ");
        ]
        (fun (value, read) ->
          setenv var value;
          equal (option string) read (Os.getenv var));
      test "setenv binds a variable, and None unbinds it" set_then_unset;
      cases "setenv refuses an empty name or one with =, before any change"
        ~name:(strf "%S")
        [ ""; var ^ "=x" ]
        refused_name;
      cases "bool_of_string reads a boolean in any case, after trimming"
        ~name:(fun (s, _) -> strf "%S" s)
        [
          ("1", Some true);
          ("true", Some true);
          ("TRUE", Some true);
          ("yes", Some true);
          ("Y", Some true);
          ("on", Some true);
          (" true ", Some true);
          ("0", Some false);
          ("false", Some false);
          ("no", Some false);
          ("N", Some false);
          ("off", Some false);
          ("OFF", Some false);
          ("\tno\n", Some false);
          ("bogus", None);
          ("2", None);
          ("", None);
        ]
        (fun (s, b) -> equal (option bool) b (Os.bool_of_string s));
      test "bool_expected names every spelling but y and n" spellings;
      cases
        "split_comma is the trimmed items between commas, without empty ones"
        ~name:(fun (s, _) -> strf "%S" s)
        [
          ("a, b ,,c ", [ "a"; "b"; "c" ]);
          ("slow", [ "slow" ]);
          (" , ", []);
          ("", []);
        ]
        (fun (s, items) -> equal (list string) items (Os.split_comma s));
    ]

(* Detecting dune, a CI and the terminal *)

let ci_row (ci, github) =
  setenv "CI" ci;
  setenv "GITHUB_ACTIONS" github;
  strf "%s, %s"
    (if Os.in_ci () then "CI" else "no CI")
    (if Os.in_github_actions () then "GitHub Actions" else "no GitHub Actions")

let flag_cases name ~var read rows =
  cases name
    ~name:(fun (value, _) -> shown value)
    rows
    (fun (value, set) ->
      setenv var value;
      equal bool set (read ()))

let platform =
  group "Detecting dune, a CI and the terminal"
    [
      cases
        "in_ci and in_github_actions count CI and GITHUB_ACTIONS as set unless \
         unset, empty or false"
        ~name:(fun (ci, github, _) ->
          strf "CI %s, GITHUB_ACTIONS %s" (shown ci) (shown github))
        [
          (None, None, "no CI, no GitHub Actions");
          (Some "", Some "", "no CI, no GitHub Actions");
          (Some "true", None, "CI, no GitHub Actions");
          (Some "true", Some "true", "CI, GitHub Actions");
          (Some "true", Some "false", "CI, no GitHub Actions");
          (Some "false", Some "true", "no CI, no GitHub Actions");
          (Some "0", Some "true", "no CI, no GitHub Actions");
          (Some "OFF", None, "no CI, no GitHub Actions");
          (Some "woodpecker", Some "true", "CI, GitHub Actions");
          (Some "true", Some "banana", "CI, GitHub Actions");
        ]
        (fun (ci, github, row) -> equal string row (ci_row (ci, github)));
      flag_cases
        "inside_dune counts INSIDE_DUNE as set unless unset, empty or false"
        ~var:"INSIDE_DUNE" Os.inside_dune
        [
          (None, false);
          (Some "", false);
          (Some "1", true);
          (Some "/w/_build/default", true);
          (Some "false", false);
          (Some "no", false);
        ];
      flag_cases "term_dumb is true iff TERM is dumb as it is spelled"
        ~var:"TERM" Os.term_dumb
        [
          (Some "dumb", true);
          (Some "xterm-256color", false);
          (Some "", false);
          (None, false);
          (Some "DUMB", false);
          (Some "dumb ", false);
        ];
    ]

(* Colour *)

let mode_name = function
  | Some Os.Always -> "always"
  | Some Never -> "never"
  | Some Auto -> "auto"
  | None -> "none"

let resolved (mode, tty, inside_dune, term_dumb, no_color) =
  setenv "NO_COLOR" no_color;
  Os.resolve_color mode ~tty ~inside_dune ~term_dumb

let colour =
  group "Colour"
    [
      cases "color_mode_of_string reads a mode in any case, untrimmed"
        ~name:(fun (s, _) -> strf "%S" s)
        [
          ("always", "always");
          ("NEVER", "never");
          ("auto", "auto");
          ("Auto", "auto");
          ("sometimes", "none");
          (" always", "none");
          ("never\n", "none");
          ("", "none");
        ]
        (fun (s, mode) ->
          equal string mode (mode_name (Os.color_mode_of_string s)));
      cases
        "resolve_color styles under Always, never under Never, and under Auto \
         on a terminal or under dune unless TERM is dumb or NO_COLOR is set"
        ~name:(fun (name, _, _) -> name)
        [
          ("always, on a pipe", (Os.Always, false, false, false, None), true);
          ("always, on a dumb terminal", (Always, true, false, true, None), true);
          ( "always, with NO_COLOR",
            (Always, false, false, false, Some "1"),
            true );
          ( "never, on a terminal under dune",
            (Never, true, true, false, None),
            false );
          ("auto, on a terminal", (Auto, true, false, false, None), true);
          ("auto, under dune", (Auto, false, true, false, None), true);
          ("auto, on a pipe", (Auto, false, false, false, None), false);
          ("auto, on a dumb terminal", (Auto, true, false, true, None), false);
          ("auto, dumb under dune", (Auto, false, true, true, None), false);
          ("auto, NO_COLOR 1", (Auto, true, false, false, Some "1"), false);
          ("auto, NO_COLOR 0", (Auto, true, false, false, Some "0"), false);
          ( "auto, NO_COLOR under dune",
            (Auto, false, true, false, Some "1"),
            false );
          ("auto, an empty NO_COLOR", (Auto, true, false, false, Some ""), true);
        ]
        (fun (_, input, styled) -> equal bool styled (resolved input));
    ]

(* Atomic file writes *)

(* Every entry under [dir] with what it holds, in name order: a directory
   ends in [/], a link shows its target and a file its bytes. *)
let rec tree ?(under = "") dir =
  List.concat_map
    (fun name ->
      let path = Filename.concat dir name and shown = under ^ name in
      match (Unix.lstat path).st_kind with
      | S_DIR -> (shown ^ "/") :: tree ~under:(shown ^ "/") path
      | S_LNK -> [ shown ^ " -> " ^ Unix.readlink path ]
      | S_REG -> [ shown ^ ": " ^ read path ]
      | S_CHR | S_BLK | S_FIFO | S_SOCK -> [ shown ])
    (entries dir)

let with_umask mask f =
  let previous = Unix.umask mask in
  Fun.protect ~finally:(fun () -> ignore (Unix.umask previous)) f

let permissions path = (Unix.stat path).st_perm land 0o777

let fails_at ~path step = function
  | Sys_error message -> String.starts_with ~prefix:(path ^ ": " ^ step) message
  | _ -> false

let every_byte =
  String.init ((256 * 1024) + 37) (fun i -> Char.chr (i land 0xff))

let written (previous, contents) =
  let dir = temp_dir () in
  let path = Filename.concat dir "target" in
  Option.iter
    (fun (bytes, perm) ->
      write path bytes;
      Unix.chmod path perm)
    previous;
  Os.atomic_write ~path contents;
  equal
    (pair string (list string))
    (contents, [ "target" ])
    (read path, entries dir)

let created (umask, perm, previous) =
  posix_only ();
  let path = Filename.concat (temp_dir ()) "target" in
  Option.iter
    (fun p ->
      write path "old";
      Unix.chmod path p)
    previous;
  with_umask umask (fun () -> Os.atomic_write ?perm ~path "x");
  permissions path

let refused_perm perm =
  let dir = temp_dir () in
  raises
    (Invalid_argument
       "Os.atomic_write: perm must contain only bits within 0o777") (fun () ->
      Os.atomic_write ~perm ~path:(Filename.concat dir "target") "x");
  equal (list string) [] (entries dir)

let failed_write (setup, step) =
  let root = temp_dir () in
  let path = setup root in
  let before = tree root in
  raises_match (fails_at ~path step) (fun () ->
      Os.atomic_write ~path "replacement");
  equal (list string) before (tree root)

let missing_parent root = Filename.concat root "missing/target"

let directory_target root =
  let path = Filename.concat root "target" in
  Unix.mkdir path 0o700;
  write (Filename.concat path "sentinel") "untouched";
  path

let symbolic_link root =
  posix_only ();
  let referent = Filename.concat root "referent" in
  let path = Filename.concat root "target" in
  write referent "referent bytes";
  Unix.symlink referent path;
  path

let read_only_parent () =
  permissions_honoured ();
  let root = temp_dir () in
  let locked = Filename.concat root "locked" in
  Unix.mkdir locked 0o700;
  let path = Filename.concat locked "target" in
  write path "previous contents";
  Unix.chmod locked 0o500;
  raises_match (fails_at ~path "cannot create temporary file") (fun () ->
      Os.atomic_write ~path "replacement");
  equal (list string)
    [ "locked/"; "locked/target: previous contents" ]
    (tree root)

let writer_contents writer round =
  strf "writer=%d round=%d\000%s" writer round
    (String.make (4096 + writer) (Char.chr (65 + writer)))

let concurrent_writers () =
  posix_only ();
  let dir = temp_dir () in
  let path = Filename.concat dir "target" in
  let writers = 6 and rounds = 24 in
  let writer w () =
    for round = 0 to rounds - 1 do
      Os.atomic_write ~path (writer_contents w round)
    done
  in
  let children = List.init writers (fun w -> fork (writer w)) in
  equal (list string)
    (List.init writers (fun _ -> "exited 0"))
    (List.map (fun pid -> ended (wait pid)) children);
  mem string (read path)
    (List.init writers (fun w -> writer_contents w (rounds - 1)));
  equal (list string) [ "target" ] (entries dir)

(* A temporary is named [.tmp-<pid>-<serial>], its serial counting the
   temporaries its process made. The write runs in the test's own process,
   where a mutant is armed and reach is measured; a process has made fewer
   than 384 temporaries when the test starts, a mutation run's dry run
   included, so every name the write can try is taken. *)
let taken_names () =
  let dir = temp_dir () in
  let taken =
    List.init 640 (fun serial -> strf ".tmp-%x-%x" (Unix.getpid ()) serial)
  in
  List.iter
    (fun name ->
      Unix.close (Unix.openfile (Filename.concat dir name) [ O_CREAT ] 0o600))
    taken;
  let path = Filename.concat dir "target" in
  raises_match (fails_at ~path "cannot create temporary file: File exists")
    (fun () -> Os.atomic_write ~path "x");
  equal (list string) (List.sort String.compare taken) (entries dir)

let atomic_writes =
  group "Atomic file writes"
    [
      cases
        "atomic_write leaves the target holding its contents, and no other file"
        ~name:fst
        [
          ("a new file of every byte value, past one write", (None, every_byte));
          ( "a replaced file",
            (Some ("old bytes that must disappear", 0o644), "new\000bytes") );
          ("a file replaced by nothing", (Some ("old", 0o644), ""));
          ("a read-only file", (Some ("read-only contents", 0o444), "replaced"));
        ]
        (fun (_, row) -> written row);
      cases
        "the file takes perm under the umask, whatever the replaced file had"
        ~name:(fun (name, _, _) -> name)
        [
          ("the default under umask 022", (0o022, None, None), 0o644);
          ("0o600 under umask 022", (0o022, Some 0o600, None), 0o600);
          ("0o666 under umask 077", (0o077, Some 0o666, None), 0o600);
          ("the default over a 0o444 file", (0o022, None, Some 0o444), 0o644);
        ]
        (fun (_, input, perm) -> equal int perm (created input));
      cases "atomic_write refuses a perm beyond 0o777 before touching a file"
        ~name:(fun (name, _) -> name)
        [ ("-1", -1); ("0o1000", 0o1000); ("0o4755", 0o4755) ]
        (fun (_, perm) -> refused_perm perm);
      cases
        "a failed write raises Sys_error naming the path and the step, and \
         leaves the directory as it was"
        ~name:(fun (name, _, _) -> name)
        [
          ("a missing parent", missing_parent, "cannot create temporary file");
          ("a directory as target", directory_target, "cannot replace");
          ("a symbolic link as target", symbolic_link, "is a symbolic link");
        ]
        (fun (_, setup, step) -> failed_write (setup, step));
      test
        "a write into a read-only directory raises Sys_error and leaves it as \
         it was"
        read_only_parent;
      test "concurrent writers leave one whole contents and no temporary"
        concurrent_writers;
      test "a write whose temporary names are all taken fails, and leaves them"
        taken_names;
    ]

(* Project root and log root *)

let roots () = (Os.project_root (), Os.default_log_dir ())

let under inside_dune =
  setenv "WINDTRAP_PROJECT_ROOT" None;
  setenv "INSIDE_DUNE" inside_dune;
  roots ()

(* The temporary directory lies outside every build tree, so the only build
   directory in sight is the one a variable names. *)
let outside_builds () =
  chdir (temp_dir ());
  Sys.getcwd ()

(* The roots are spelled with '/', and the log root joins [_tests] to the
   build directory with [Filename.concat], as the product does. *)
let logs build = Filename.concat build "_tests"

let relative_inside_dune (value, (root, build)) =
  let cwd = Windtrap_test_support.slashed (outside_builds ()) in
  equal (pair string string)
    (cwd ^ "/" ^ root, logs (cwd ^ "/" ^ build))
    (under (Some value))

let falls_through value =
  ignore (outside_builds ());
  let own = under None in
  equal (pair string string) own (under value)

let executable's_directory () =
  let own = under None in
  let dir = Filename.dirname Sys.executable_name in
  let dir =
    if Filename.is_relative dir then Filename.concat (Sys.getcwd ()) dir
    else dir
  in
  equal (pair string string) (under (Some dir)) own

let root_from (value, root) =
  let cwd = Windtrap_test_support.slashed (outside_builds ()) in
  setenv "WINDTRAP_PROJECT_ROOT" (Some value);
  equal string
    (if Filename.is_relative root then cwd ^ "/" ^ root else root)
    (Os.project_root ())

let in_a_removed_directory () =
  if Sys.win32 then
    skip ~reason:"Windows cannot remove a process's working directory" ();
  let gone = Filename.concat (temp_dir ()) "gone" in
  Unix.mkdir gone 0o700;
  chdir gone;
  Unix.rmdir gone;
  setenv "WINDTRAP_PROJECT_ROOT" None;
  setenv "INSIDE_DUNE" (Some "w/_build/default")

let root =
  group "Project root and log root"
    [
      cases
        "INSIDE_DUNE names the build directory, cut after its first _build \
         component"
        ~name:fst
        [
          ("a build context", ("/w/_build/default", ("/w", "/w/_build")));
          ( "a sandboxed action's context",
            ("/w/_build/.sandbox/3f/default", ("/w", "/w/_build")) );
          ( "a private build directory",
            ("/w/_build_priv/default", ("/w", "/w/_build_priv")) );
          ( "a path deep under one",
            ("/w/_build_x/default/test/t.exe", ("/w", "/w/_build_x")) );
          ( "a second _build component",
            ("/w/_build/default/a/_build_y/b", ("/w", "/w/_build")) );
          ("the build directory itself", ("/w/_build", ("/w", "/w/_build")));
        ]
        (fun (_, (value, (root, build))) ->
          equal (pair string string) (root, logs build) (under (Some value)));
      cases
        "a relative INSIDE_DUNE is made absolute against the working directory"
        ~name:(fun (value, _) -> strf "%S" value)
        [
          ("w/_build/default", ("w", "w/_build"));
          ("w\\_build\\default", ("w", "w/_build"));
        ]
        relative_inside_dune;
      cases
        "an INSIDE_DUNE that names no build directory leaves it to the \
         executable"
        ~name:shown
        [ Some "1"; Some "/w/src"; Some "" ]
        falls_through;
      test "without INSIDE_DUNE, the directory of the executable decides"
        executable's_directory;
      cases
        "project_root is WINDTRAP_PROJECT_ROOT when set, normalized lexically"
        ~name:(fun (value, _) -> strf "%S" value)
        [
          ("/tmp/override", "/tmp/override");
          ("/r/", "/r");
          ("/r/.", "/r");
          ("/r//", "/r");
          ("/x/../r", "/r");
          ("//r/./", "/r");
          ("/..", "/..");
          ("rel", "rel");
          ("./sub/../r/", "r");
          ("sub\\..\\r\\", "r");
        ]
        root_from;
      test
        "project_root and default_log_dir raise Sys_error when the working \
         directory is gone" (fun () ->
          in_a_removed_directory ();
          raises_match Exn.sys_error (fun () -> Os.project_root ());
          raises_match Exn.sys_error (fun () -> Os.default_log_dir ()));
    ]

(* Source tree and build tree *)

let reconstruct_symlink () =
  posix_only ();
  let root = temp_dir () in
  Unix.symlink "/elsewhere" (Filename.concat root "link");
  equal (result string string)
    (Ok (root ^ "/link/x.ml"))
    (Os.reconstruct ~root "link/x.ml")

let reconstruction =
  group "Source tree and build tree"
    [
      cases
        "reconstruct proves a source path under the root, or gives the \
         candidate"
        ~name:(fun (name, _, _, _) -> name)
        [
          ("a relative path", "/proj", "test/foo.ml", Ok "/proj/test/foo.ml");
          ( "a build context's copy",
            "/proj",
            "_build/default/test/foo.ml",
            Ok "/proj/test/foo.ml" );
          ( "a sandboxed action's copy",
            "/proj",
            "_build/.sandbox/3f/default/test/foo.ml",
            Ok "/proj/test/foo.ml" );
          ( "an absolute copy under the root",
            "/proj",
            "/proj/_build/default/test/foo.ml",
            Ok "/proj/test/foo.ml" );
          ( "an absolute path under the root",
            "/proj",
            "/proj/test/foo.ml",
            Ok "/proj/test/foo.ml" );
          ( "dots and repeated separators",
            "/proj",
            "test/./a//b.ml",
            Ok "/proj/test/a/b.ml" );
          ( "a .. inside the root",
            "/proj",
            "test/sub/../foo.ml",
            Ok "/proj/test/foo.ml" );
          ( "a root with a trailing /",
            "/proj/",
            "test/foo.ml",
            Ok "/proj/test/foo.ml" );
          ("the filesystem root", "/", "test/foo.ml", Ok "/test/foo.ml");
          ( "backslashes",
            "/proj",
            "_build\\default\\test\\foo.ml",
            Ok "/proj/test/foo.ml" );
          ( "a root that does not exist",
            "/no/such/root",
            "a.ml",
            Ok "/no/such/root/a.ml" );
          ( "a drive",
            "C:/w",
            "C:\\w\\_build\\default\\test\\a.ml",
            Ok "C:/w/test/a.ml" );
          ("a lowercase drive", "c:/w", "c:/w/a.ml", Ok "c:/w/a.ml");
          ("a drive without its separator", "/w", "C:a.ml", Ok "/w/C:a.ml");
          ("a digit before a colon", "/w", "1:/a.ml", Ok "/w/1:/a.ml");
          ("another drive", "C:/w", "D:/w/a.ml", Error "D:/w/a.ml");
          ("the bare root of a drive", "/w", "C:/", Error "C:/");
          ( "an absolute path elsewhere",
            "/proj",
            "/elsewhere/foo.ml",
            Error "/elsewhere/foo.ml" );
          ( "an unnormalized path elsewhere",
            "/proj",
            "/elsewhere/./a//foo.ml",
            Error "/elsewhere/./a//foo.ml" );
          ( "a sibling with the root as prefix",
            "/proj",
            "/proj2/test/foo.ml",
            Error "/proj2/test/foo.ml" );
          ( "a .. out of the root",
            "/proj",
            "../escape.ml",
            Error "/proj/../escape.ml" );
          ( "a nested .. out of the root",
            "/proj",
            "test/../../escape.ml",
            Error "/proj/test/../../escape.ml" );
          ( "a .. out of a build context",
            "/proj",
            "_build/default/../../etc/passwd",
            Error "/proj/../../etc/passwd" );
          ("the root itself", "/proj", ".", Error "/proj/.");
          ("the empty path", "/proj", "", Error "/proj/");
          ("a relative root", "proj", "test/foo.ml", Error "proj/test/foo.ml");
          ("a relative root with trailing /", "w//", "a.ml", Error "w/a.ml");
        ]
        (fun (_, root, file, proven) ->
          equal (result string string) proven (Os.reconstruct ~root file));
      test "reconstruct resolves no symbolic link" reconstruct_symlink;
      cases "build_root is the build directory and the context after it"
        ~name:(fun (dir, _) -> strf "%S" dir)
        [
          ("/w/_build/default/test", Some "/w/_build/default");
          ("/w/_build/default", Some "/w/_build/default");
          ( "/w/_build/.sandbox/3f/default/test",
            Some "/w/_build/.sandbox/3f/default" );
          ("\\w\\_build\\default\\test", Some "/w/_build/default");
          ("/w/_build", None);
          ("/w/src", None);
        ]
        (fun (dir, context) ->
          equal (option string) context (Os.build_root dir));
    ]

(* Display paths *)

let displayed path =
  setenv "WINDTRAP_PROJECT_ROOT" (Some "/r");
  Os.display_path path

let artifact path =
  setenv "WINDTRAP_PROJECT_ROOT" (Some "/r");
  Os.display_artifact path

let under_root_spelled root =
  setenv "WINDTRAP_PROJECT_ROOT" (Some root);
  (Os.display_path "/r/a/b.ml", Os.display_artifact "/r/_build/x.log")

let display =
  group "Display paths"
    [
      cases
        "display_path spells a path relative to the project root, without its \
         build segment, dots or repeated separators"
        ~name:(fun (path, _) -> strf "%S" path)
        [
          ("/r/_build/default/qa/x/t.exe", "qa/x/t.exe");
          ("/r/_build/release.x/qa/x/t.exe", "qa/x/t.exe");
          ("/r/qa/x/greeting.snap", "qa/x/greeting.snap");
          ("/r/./qa//x/./t.exe", "qa/x/t.exe");
          ("/r/qa/../qa/t.exe", "qa/../qa/t.exe");
          ("/r/_build/default/a/_build/ctx/b.ml", "a/_build/ctx/b.ml");
          ("/r/a/_build", "a/_build");
          ("/_build/default/r/a.ml", "a.ml");
          ("/elsewhere/./a//t.exe", "/elsewhere/a/t.exe");
          ("qa/x/t.exe", "qa/x/t.exe");
          ("./t.exe", "t.exe");
          ("/r/.", ".");
          ("w\\_build\\default\\test\\foo.ml", "w/test/foo.ml");
        ]
        (fun (path, shown) -> equal string shown (displayed path));
      cases
        "display_path and display_artifact remove a root spelled with ., .. or \
         repeated separators"
        ~name:(strf "%S") [ "/r/"; "/r/."; "/r//"; "/x/../r"; "//r/./" ]
        (fun root ->
          equal (pair string string) ("a/b.ml", "_build/x.log")
            (under_root_spelled root));
      cases "display_artifact removes the root prefix and nothing else"
        ~name:(fun (path, _) -> strf "%S" path)
        [
          ("/r/_build/_tests/s/t.output", "_build/_tests/s/t.output");
          ("/r/./a//b", "./a//b");
          ("/elsewhere/./x", "/elsewhere/./x");
          ("\\r\\_build\\x.log", "_build\\x.log");
        ]
        (fun (path, shown) -> equal string shown (artifact path));
      test
        "display_path and display_artifact remove no prefix when the working \
         directory is gone" (fun () ->
          in_a_removed_directory ();
          equal (pair string string)
            ("/r/a.ml", "/r/_build/./a.ml")
            ( Os.display_path "/r/_build/default/./a.ml",
              Os.display_artifact "/r/_build/./a.ml" ));
    ]

(* Path components *)

let safe_byte = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' -> true
  | _ -> false

let one_safe_component s =
  let component = Os.sanitize_component s in
  at_most int ~than:80 (String.length component);
  satisfies ~claim:"safe bytes, and neither empty, . nor .." string
    (fun c -> String.for_all safe_byte c && not (List.mem c [ ""; "."; ".." ]))
    component

(* The digests are MD5's, as Python's hashlib computes them. *)
let components =
  group "Path components"
    [
      cases
        "sanitize_component keeps a safe name, and marks a changed one with \
         its digest"
        ~name:(fun (name, _, _) -> name)
        [
          ("a safe name", "abc-1_2.x", "abc-1_2.x");
          ("a safe name with a leading dot", ".hidden", ".hidden");
          ("a space and a slash", "a b/c", "a_b_c-22ce5bc5");
          ("a colon", "parse: empty", "parse__empty-910a10a4");
          ("a comma", "parse, empty", "parse__empty-f14525e6");
          ("a two-byte code point", "\xc3\xa9", "__-66ddcd97");
          ("the empty name", "", "unnamed-d41d8cd9");
          ("a dot", ".", "unnamed-5058f1af");
          ("two dots", "..", "unnamed-58b9e70b");
          ("80 safe bytes", String.make 80 'a', String.make 80 'a');
          ( "81 safe bytes",
            String.make 81 'a',
            String.make 40 'a' ^ "_986e6938ed767a8ae9530eef54bfe5f1" );
          ( "100 safe bytes",
            String.make 100 'a',
            String.make 40 'a' ^ "_36a92cc94a9e0fa21f625f8bfb007adf" );
          ( "100 other safe bytes",
            String.make 100 'b',
            String.make 40 'b' ^ "_d84a935724eac27d7c9676679b6cdbaf" );
          ( "90 spaces",
            String.make 90 ' ',
            String.make 40 '_' ^ "_0211afbc5c7fd69b7872436c5a99688d" );
        ]
        (fun (_, s, component) ->
          equal string component (Os.sanitize_component s));
      prop "sanitize_component is one safe component of at most 80 bytes"
        (Gen.string_of ~size:(Gen.int_range 0 120) Gen.char)
        one_safe_component;
    ]

(* Filesystem helpers *)

let unsearchable () =
  permissions_honoured ();
  let locked = Filename.concat (temp_dir ()) "locked" in
  Unix.mkdir locked 0o700;
  let inside = Filename.concat locked "x" in
  write inside "";
  Unix.chmod locked 0o000;
  equal bool false (Os.file_exists inside)

let creates_parents () =
  let root = temp_dir () in
  let deep = Filename.concat root "a/b/c" in
  let made = [ "a/"; "a/b/"; "a/b/c/" ] in
  Os.mkdir_p deep;
  equal (list string) made (tree root);
  Os.mkdir_p deep;
  equal (list string) made (tree root)

let mkdir_permissions () =
  posix_only ();
  let root = temp_dir () in
  with_umask 0o020 (fun () -> Os.mkdir_p (Filename.concat root "a/b"));
  equal (list int) [ 0o750; 0o750 ]
    (List.map
       (fun dir -> permissions (Filename.concat root dir))
       [ "a"; "a/b" ])

let leaves_a_file () =
  let file = temp_file () in
  write file "kept";
  Os.mkdir_p file;
  equal string "kept" (read file);
  (* Windows reports the path under a file as missing. *)
  let error = if Sys.win32 then Unix.ENOENT else Unix.ENOTDIR in
  raises_match
    (function Unix.Unix_error (e, _, _) -> e = error | _ -> false)
    (fun () -> Os.mkdir_p (Filename.concat file "sub"))

(* A link to nothing is missing to [file_exists] and taken to [mkdir], as a
   directory that another process creates meanwhile is. *)
let leaves_a_link () =
  posix_only ();
  let root = temp_dir () in
  let link = Filename.concat root "link" in
  let nowhere = Filename.concat root "nowhere" in
  Unix.symlink nowhere link;
  Os.mkdir_p link;
  equal (list string) [ "link -> " ^ nowhere ] (tree root)

let filesystem =
  group "Filesystem helpers"
    [
      cases "file_exists is true iff the path exists, and false on an error"
        ~name:(fun (name, _, _) -> name)
        [
          ("a directory", (fun () -> temp_dir ()), true);
          ("a file", (fun () -> temp_file ()), true);
          ( "a missing path",
            (fun () -> Filename.concat (temp_dir ()) "x"),
            false );
          ( "a path under a file",
            (fun () -> Filename.concat (temp_file ()) "x"),
            false );
        ]
        (fun (_, path, exists) -> equal bool exists (Os.file_exists (path ())));
      test "file_exists is false under a directory it cannot search"
        unsearchable;
      test "mkdir_p creates a directory and its missing parents, once"
        creates_parents;
      test "mkdir_p creates with 0o770 under the umask" mkdir_permissions;
      test "mkdir_p leaves an existing file alone" leaves_a_file;
      test "mkdir_p leaves a symbolic link to nothing alone" leaves_a_link;
      test "mkdir_p of the empty path or of . creates nothing" (fun () ->
          let cwd = outside_builds () in
          Os.mkdir_p "";
          Os.mkdir_p ".";
          equal (list string) [] (entries cwd));
      cases "failure_reason is why an operation on a path failed, less the path"
        ~name:(fun (name, _, _, _) -> name)
        [
          ( "a Sys_error that starts with the path",
            "a/b.xml",
            Sys_error "a/b.xml: cannot write: No space left on device",
            "cannot write: No space left on device" );
          ( "another Sys_error",
            "a/b.xml",
            Sys_error "c.xml: gone",
            "c.xml: gone" );
          ( "the Unix_error of mkdir_p",
            "blocked/out/r.xml",
            Unix.Unix_error (Unix.ENOTDIR, "mkdir", "blocked/out"),
            "cannot create directory blocked/out: Not a directory" );
          ("any other exception", "a", Not_found, "Not_found");
        ]
        (fun (_, path, exn, reason) ->
          equal string reason (Os.failure_reason ~path exn));
    ]

(* Standard error *)

let redirect path fd =
  let file = Unix.openfile path [ O_WRONLY; O_CREAT; O_TRUNC ] 0o600 in
  Unix.dup2 file fd;
  Unix.close file

(* How a child that runs [f] ended, and what it wrote on each stream. *)
let said f =
  posix_only ();
  let dir = temp_dir () in
  let out = Filename.concat dir "out" and err = Filename.concat dir "err" in
  let child () =
    redirect out Unix.stdout;
    redirect err Unix.stderr;
    f ()
  in
  let status = wait (fork child) in
  (ended status, read out, read err)

let closed_stdout () =
  print_string "pending";
  Unix.close Unix.stdout;
  Os.say "still said"

let pending_err_formatter () =
  Format.eprintf "pending ";
  Os.say "line"

let flushed_first () =
  print_string "channel, unflushed; ";
  Format.printf "formatter, unflushed@\n";
  Os.say "after both";
  equal string
    "channel, unflushed; formatter, unflushed\nwindtrap: after both\n"
    (output ())

let standard_error =
  group "Standard error"
    [
      cases "say writes windtrap: and its message as a line on standard error"
        ~name:(fun (name, _, _) -> name)
        [
          ( "a message",
            (fun () -> Os.say "could not write the verdict file: disk full"),
            "windtrap: could not write the verdict file: disk full\n" );
          ( "a warning",
            (fun () -> Os.warn "could not write JUnit report: disk full"),
            "windtrap: warning: could not write JUnit report: disk full\n" );
          ( "a message of several lines, anchored on its first",
            (fun () ->
              Os.say
                "duplicate test paths:\n\
                \  a\n\
                 Every full test path must be unique."),
            "windtrap: duplicate test paths:\n\
            \  a\n\
             Every full test path must be unique.\n" );
          ( "a control byte but a line feed or a tab, escaped",
            (fun () -> Os.say "invalid value 'a\tb\027[31mc\127'"),
            "windtrap: invalid value 'a\tb\\x1b[31mc\\x7f'\n" );
          ( "a carriage return escaped, bytes from 0x80 as they are",
            (fun () -> Os.say "a\rb \xc3\xa9 \xff"),
            "windtrap: a\\x0db \xc3\xa9 \xff\n" );
          ("a closed standard output", closed_stdout, "windtrap: still said\n");
          ( "a pending Format.err_formatter, flushed before",
            pending_err_formatter,
            "pending windtrap: line\n" );
        ]
        (fun (_, f, err) ->
          equal (triple string string string) ("exited 0", "", err) (said f));
      test "say flushes standard output first, formatter and channel"
        flushed_first;
    ]

(* Signals *)

(* No run handles SIGUSR1 or SIGUSR2, and each ends a process under its
   default disposition. *)
let usr1 = Sys.sigusr1
let usr2 = Sys.sigusr2
let raise_signal signal = Unix.kill (Unix.getpid ()) signal

(* A handler runs at a safepoint after [kill] returns, so a test polls for
   about [tries] milliseconds. *)
let until ?(tries = 1000) ready =
  let rec poll tries =
    if not (ready () || tries = 0) then begin
      Unix.sleepf 0.001;
      poll (tries - 1)
    end
  in
  poll tries

let never () = false

let disposition ~mine = function
  | Sys.Signal_handle f when f == mine -> "the handler found"
  | Sys.Signal_handle _ -> "another handler"
  | Sys.Signal_default -> "default"
  | Sys.Signal_ignore -> "ignored"

let handled_then_restored () =
  posix_only ();
  let mine (_ : int) = () in
  let found = Sys.signal usr1 (Sys.Signal_handle mine) in
  let got = ref [] in
  Os.with_signals [ usr1 ]
    (fun signal -> got := signal_name signal :: !got)
    (fun () ->
      raise_signal usr1;
      until (fun () -> !got <> []));
  let after = Sys.signal usr1 found in
  equal
    (pair (list string) string)
    ([ "SIGUSR1" ], "the handler found")
    (!got, disposition ~mine after)

(* A signal a process sends itself is delivered before [kill] returns, and its
   handler runs at the first safepoint, so ten polls would see it run. *)
let ignored_stays_ignored () =
  posix_only ();
  let found = Sys.signal usr1 Sys.Signal_ignore in
  let got = ref [] in
  Os.with_signals [ usr1 ]
    (fun signal -> got := signal_name signal :: !got)
    (fun () ->
      raise_signal usr1;
      until ~tries:10 (fun () -> !got <> []));
  Sys.set_signal usr1 found;
  equal (list string) [] !got

(* The handler sends [sent]: at its default disposition it ends the child at
   once, and otherwise the child exits 3. *)
let resent sent () =
  Sys.set_signal usr1 Sys.Signal_default;
  Sys.set_signal usr2 Sys.Signal_default;
  Os.with_signals [ usr1; usr2 ]
    (fun _ ->
      raise_signal sent;
      Unix._exit 3)
    (fun () ->
      raise_signal usr1;
      until never)

(* Two SIGPIPEs run the handler, and the SIGUSR1 after them ends the child,
   whose exit code otherwise counts the handler's runs. *)
let pipe_then_usr1 () =
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
  Unix._exit !count

let forked_inside () =
  Os.with_signals [ usr1 ] ignore (fun () ->
      wait
        (fork (fun () ->
             raise_signal usr1;
             until never)))

(* Whether SIGUSR1 is blocked in the calling thread. *)
let usr1_blocked () = List.mem usr1 (Unix.sigprocmask Unix.SIG_BLOCK [])

(* Whether a SIGUSR1 sent inside [with_blocked] was handled there, whether it
   was handled once [with_blocked] returned, and whether SIGUSR1 was then
   blocked. *)
let held_until_return () =
  posix_only ();
  let handled = ref false in
  let found = Sys.signal usr1 (Sys.Signal_handle (fun _ -> handled := true)) in
  let inside =
    Os.with_blocked [ usr1 ] (fun () ->
        raise_signal usr1;
        until ~tries:10 (fun () -> !handled);
        !handled)
  in
  let after = !handled and blocked = usr1_blocked () in
  Sys.set_signal usr1 found;
  equal (triple bool bool bool) (false, true, false) (inside, after, blocked)

(* What [with_blocked] raised when the handler of a signal held while [fn]
   ran raises [Exit], and whether SIGUSR1 was then blocked. *)
let handler_raises ~fn_raises =
  posix_only ();
  let found = Sys.signal usr1 (Sys.Signal_handle (fun _ -> raise Exit)) in
  let raised =
    match
      Os.with_blocked [ usr1 ] (fun () ->
          raise_signal usr1;
          if fn_raises then raise Not_found)
    with
    | () -> "returned"
    | exception e -> Printexc.to_string e
  in
  let blocked = usr1_blocked () in
  Sys.set_signal usr1 found;
  (raised, blocked)

let unblocked_after_raise () =
  posix_only ();
  let raised =
    match Os.with_blocked [ usr1 ] (fun () -> raise Not_found) with
    | () -> "returned"
    | exception e -> Printexc.to_string e
  in
  equal (pair string bool) ("Not_found", false) (raised, usr1_blocked ())

(* A signal blocked before [with_blocked] is blocked after it. *)
let blocked_before () =
  posix_only ();
  let found = Unix.sigprocmask Unix.SIG_BLOCK [ usr1 ] in
  Os.with_blocked [ usr1; usr2 ] ignore;
  let blocked = usr1_blocked () in
  ignore (Unix.sigprocmask Unix.SIG_SETMASK found : int list);
  is_true blocked

let dies_by signal () =
  Sys.set_signal signal (Sys.Signal_handle ignore);
  ignore (Unix.sigprocmask Unix.SIG_BLOCK [ signal ]);
  Os.die_by signal

let killed_by signal status =
  equal string ("killed by " ^ signal_name signal) (ended status)

let signals =
  group "Signals"
    [
      test
        "with_signals hands a signal to the handler, and puts back the one \
         found"
        handled_then_restored;
      test "a signal the process was started with ignored stays ignored"
        ignored_stays_ignored;
      cases
        "a signal of the list that the handler sends ends the process at once"
        ~name:fst
        [ ("the same signal", usr1); ("another of the list", usr2) ]
        (fun (_, sent) ->
          posix_only ();
          killed_by sent (wait (fork (resent sent))));
      test "SIGPIPE keeps the handler, and the others go back to their default"
        (fun () ->
          posix_only ();
          killed_by usr1 (wait (fork pipe_then_usr1)));
      test "a process forked inside dies by the signal" (fun () ->
          posix_only ();
          killed_by usr1 (forked_inside ()));
      test
        "with_blocked holds a signal until its function returns, and unblocks \
         it"
        held_until_return;
      test "with_blocked puts the mask back when its function raises"
        unblocked_after_raise;
      test "with_blocked puts back the mask it found" blocked_before;
      cases
        "what the handler of a held signal raises, with_blocked raises in \
         place of its function's value, not of its exception"
        ~name:fst
        [
          ("the function returns", (false, "Stdlib.Exit"));
          ("the function raises", (true, "Not_found"));
        ]
        (fun (_, (fn_raises, raised)) ->
          equal (pair string bool) (raised, false) (handler_raises ~fn_raises));
      cases "die_by ends the process by the signal, though handled and blocked"
        ~name:signal_name [ Sys.sighup; Sys.sigint; Sys.sigpipe; Sys.sigterm ]
        (fun signal ->
          posix_only ();
          killed_by signal (wait (fork (dies_by signal))));
    ]

let () =
  exit
    (run "os"
       [
         clock;
         environment;
         platform;
         colour;
         atomic_writes;
         root;
         reconstruction;
         display;
         components;
         filesystem;
         standard_error;
         signals;
       ])
