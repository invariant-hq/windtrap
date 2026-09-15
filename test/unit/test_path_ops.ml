(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Path_ops = Windtrap.Private.Path_ops

let is_hex = function '0' .. '9' | 'a' .. 'f' -> true | _ -> false

let tests =
  [
    test "reconstruct proves containment under the root" (fun () ->
        let path = result string string in
        let ok input = Path_ops.reconstruct ~root:"/proj" input in
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
          (Path_ops.reconstruct ~root:"/proj/" "test/foo.ml");
        equal ~msg:"filesystem root as root works" path (Ok "/test/foo.ml")
          (Path_ops.reconstruct ~root:"/" "test/foo.ml");
        equal ~msg:"backslashes in the source path normalize" path
          (Ok "/proj/test/foo.ml")
          (Path_ops.reconstruct ~root:"/proj" "_build\\default\\test\\foo.ml"));
    test "reconstruct rejects escapes" (fun () ->
        let ok input = Path_ops.reconstruct ~root:"/proj" input in
        let is_error = function Error _ -> true | Ok _ -> false in
        is_true ~msg:"absolute path outside root fails"
          (is_error (ok "/elsewhere/foo.ml"));
        is_true ~msg:"dotdot escaping root fails" (is_error (ok "../escape.ml"));
        is_true ~msg:"nested dotdot escape fails"
          (is_error (ok "test/../../escape.ml"));
        is_true ~msg:"sandbox dotdot escape fails"
          (is_error (ok "_build/default/../../etc/passwd"));
        is_true ~msg:"root itself is not a source file" (is_error (ok "."));
        is_true ~msg:"empty path fails" (is_error (ok ""));
        is_true ~msg:"prefix sibling directory fails"
          (is_error (Path_ops.reconstruct ~root:"/proj" "/proj2/test/foo.ml"));
        is_true ~msg:"relative root cannot prove containment"
          (is_error (Path_ops.reconstruct ~root:"proj" "test/foo.ml"));
        equal ~msg:"error carries the unproven candidate" (result string string)
          (Error "/elsewhere/foo.ml") (ok "/elsewhere/foo.ml"));
    test "sanitize_component" (fun () ->
        equal ~msg:"safe name unchanged" string "abc-1_2.x"
          (Path_ops.sanitize_component "abc-1_2.x");
        (* A name the mapping altered carries a digest of the original:
           without it the mapping is many-to-one and two tests share one
           capture log, which Capture opens O_TRUNC. The shape is the
           contract, not the digest bytes — pinning the hex would break on
           any digest change without catching a defect, and the injectivity
           it stands for is asserted directly just below. *)
        let altered = Path_ops.sanitize_component "a b/c" in
        equal ~msg:"unsafe chars become underscores" string "a_b_c"
          (String.sub altered 0 (min 5 (String.length altered)));
        is_true ~msg:"a hex digest of the original is appended"
          (String.length altered = 5 + 1 + 8
          && altered.[5] = '-'
          && String.for_all is_hex (String.sub altered 6 8));
        not_equal ~msg:"punctuation variants stay distinct" string
          (Path_ops.sanitize_component "parse: empty")
          (Path_ops.sanitize_component "parse, empty");
        is_true ~msg:"both still start with the readable form"
          (String.starts_with ~prefix:"parse__empty"
             (Path_ops.sanitize_component "parse: empty")
          && String.starts_with ~prefix:"parse__empty"
               (Path_ops.sanitize_component "parse, empty"));
        not_equal ~msg:"empty and dot stay distinct" string
          (Path_ops.sanitize_component "")
          (Path_ops.sanitize_component ".");
        is_true ~msg:"empty becomes unnamed"
          (String.starts_with ~prefix:"unnamed-"
             (Path_ops.sanitize_component ""));
        is_true ~msg:"dotdot becomes unnamed"
          (String.starts_with ~prefix:"unnamed-"
             (Path_ops.sanitize_component ".."));
        let long = String.make 100 'a' in
        let sanitized = Path_ops.sanitize_component long in
        equal ~msg:"long names truncated with digest" int 73
          (String.length sanitized);
        equal ~msg:"long name keeps prefix" string (String.make 40 'a')
          (String.sub sanitized 0 40);
        equal ~msg:"sanitize is deterministic" string sanitized
          (Path_ops.sanitize_component long);
        not_equal ~msg:"distinct long names stay distinct" string sanitized
          (Path_ops.sanitize_component (String.make 100 'b')));
    test "mkdir_p creates nested directories and is idempotent" (fun () ->
        let deep = Filename.concat (temp_dir ()) "a/b/c" in
        Path_ops.mkdir_p deep;
        is_true ~msg:"mkdir_p creates nested directories"
          (Sys.file_exists deep && Sys.is_directory deep);
        Path_ops.mkdir_p deep;
        is_true ~msg:"mkdir_p is idempotent" (Sys.is_directory deep));
    test "file_exists" (fun () ->
        let dir = temp_dir () in
        is_true ~msg:"file_exists on a directory" (Path_ops.file_exists dir);
        is_false ~msg:"file_exists on a missing path"
          (Path_ops.file_exists (Filename.concat dir "missing")));
    test "project_root: explicit override wins" (fun () ->
        Fun.protect
          ~finally:(fun () -> Unix.putenv "WINDTRAP_PROJECT_ROOT" "")
          (fun () ->
            Unix.putenv "WINDTRAP_PROJECT_ROOT" "/tmp/override";
            equal ~msg:"override wins" string "/tmp/override"
              (Path_ops.project_root ());
            equal ~msg:"default_log_dir under the root" string
              "/tmp/override/_build/_tests"
              (Path_ops.default_log_dir ());
            Unix.putenv "WINDTRAP_PROJECT_ROOT" "rel";
            is_true ~msg:"relative override absolutized"
              ((not (Filename.is_relative (Path_ops.project_root ())))
              && Filename.basename (Path_ops.project_root ()) = "rel")));
    test "project_root: marker walk skips _build" (fun () ->
        let cwd = Sys.getcwd () in
        Fun.protect
          ~finally:(fun () -> Sys.chdir cwd)
          (fun () ->
            let scratch = temp_dir () in
            let proj = Filename.concat scratch "proj" in
            let sub = Filename.concat proj "sub/dir" in
            let sandbox = Filename.concat proj "_build/default/test" in
            Path_ops.mkdir_p sub;
            Path_ops.mkdir_p sandbox;
            let write path =
              let oc = open_out path in
              output_string oc "(lang dune 3.0)\n";
              close_out oc
            in
            write (Filename.concat proj "dune-project");
            let root_from dir =
              Sys.chdir dir;
              let r = Path_ops.project_root () in
              Sys.chdir cwd;
              r
            in
            (* Resolve symlinks in the expectation (macOS /tmp is a symlink):
               the walk returns the physical cwd's ancestor. *)
            let phys dir =
              Sys.chdir dir;
              let r = Sys.getcwd () in
              Sys.chdir cwd;
              r
            in
            let proj_phys = phys proj in
            equal ~msg:"walk finds dune-project from a subdirectory" string
              proj_phys (root_from sub);
            equal ~msg:"walk skips _build sandboxes" string proj_phys
              (root_from sandbox);
            equal ~msg:"walk from the root itself" string proj_phys
              (root_from proj);
            (* Decoy markers inside the build tree must not win: dune
               materializes source files (including dune-project) under
               _build, and sandboxes add decoy .git markers. The walk must
               resume above _build. *)
            write (Filename.concat proj "_build/default/dune-project");
            equal ~msg:"decoy marker under _build/default is ignored" string
              proj_phys (root_from sandbox);
            let deep_sandbox =
              Filename.concat proj "_build/.sandbox/0abc/default"
            in
            Path_ops.mkdir_p deep_sandbox;
            write (Filename.concat deep_sandbox "dune-project");
            equal ~msg:"decoy marker in a nested sandbox is ignored" string
              proj_phys (root_from deep_sandbox)));
    test "display spells report paths project-root relative" (fun () ->
        (* The one producer of [wrote]/hint path spellings for both the
           library and inline runners (D5 §8; ppx/F-6). *)
        let root = Path_ops.project_root () in
        equal ~msg:"build prefix stripped, root-relative" string "qa/x/t.exe"
          (Path_ops.display (root ^ "/_build/default/qa/x/t.exe"));
        equal ~msg:"absolute in-root path relativized" string
          "qa/x/greeting.snap"
          (Path_ops.display (root ^ "/qa/x/greeting.snap"));
        equal ~msg:"interior dot and empty segments dropped" string "qa/x/t.exe"
          (Path_ops.display (root ^ "/./qa//x/./t.exe"));
        equal ~msg:"dotdot untouched" string "qa/../qa/t.exe"
          (Path_ops.display (root ^ "/qa/../qa/t.exe"));
        equal ~msg:"path outside the root normalized, not relativized" string
          "/elsewhere/a/t.exe"
          (Path_ops.display "/elsewhere/./a//t.exe"));
    test "display strips exactly one _build sandbox prefix" (fun () ->
        (* The [_build/<context>/] rule [display] and [reconstruct] share.
           It has no export of its own, so the cases that neither the plain
           [display] nor the [reconstruct] test reaches are pinned here,
           through the surface that prints them. *)
        let root = Path_ops.project_root () in
        equal ~msg:"any context is stripped, not just default" string
          "qa/x/t.exe"
          (Path_ops.display (root ^ "/_build/release.x/qa/x/t.exe"));
        equal ~msg:"only the first _build segment is stripped" string
          "a/_build/ctx/b.ml"
          (Path_ops.display (root ^ "/_build/default/a/_build/ctx/b.ml"));
        equal ~msg:"trailing _build without a context is kept" string "a/_build"
          (Path_ops.display (root ^ "/a/_build"));
        equal ~msg:"a path with no _build is left alone" string "qa/x/t.exe"
          (Path_ops.display "qa/x/t.exe");
        equal ~msg:"backslashes normalized" string "w/test/foo.ml"
          (Path_ops.display "w\\_build\\default\\test\\foo.ml"));
  ]

let () = exit @@ Windtrap.run "path_ops" tests
