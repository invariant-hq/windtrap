(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Windtrap_coverage: the register/visit/snapshot registry
   (saturation, duplicate and zero-block registrations, the warn-and-drop
   conflicting-registration path), collection algebra (add/merge matrices:
   disjoint, overlapping, conflicting), the v3 serialization with its
   optional writer identity (exact bytes, round trip, v1/v2-magic and
   corruption rejection), deterministic
   output filenames (sandbox-invariant exe hashing), extent -> line
   derivation (nesting, one-line matches, boundary offsets, huge files),
   report data (including stale-source rejection), and the at_exit dump
   end to end through a child executable. Ranges and excerpt regions are
   layout, live with the renderer, and are pinned in test/unit's render
   suite.

   A windtrap suite ([run] executes tests sequentially in declaration
   order). The registry tests accumulate state in the shared global
   registry, which is never reset: each uses distinctive file names and
   extents so that they cannot read each other's data. The dune action
   sets WINDTRAP_COVERAGE=off: the synthetic registrations would
   otherwise render a meaningless inline coverage line on every green
   run. *)

open Windtrap
module C = Windtrap_coverage
module I = Windtrap_instr

let check name cond = is_true ~msg:name cond
let check_string name ~expected ~actual = equal ~msg:name string expected actual
let check_int name ~expected ~actual = equal ~msg:name int expected actual

let check_invalid_arg name f =
  raises_match ~msg:name (fun e -> Exn.invalid_arg e) f

let contains needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec loop i =
    if i + n > h then false
    else if String.sub haystack i n = needle then true
    else loop (i + 1)
  in
  loop 0

let pt start_ofs end_ofs = { C.start_ofs; end_ofs }

let read_file path =
  match open_in_bin path with
  | ic ->
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () -> Some (really_input_string ic (in_channel_length ic)))
  | exception Sys_error _ -> None

let ok name = function
  | Ok t -> t
  | Error e -> failf "%s: unexpected error: %a" name C.pp_error e

(* Hermeticity: all paths are absolute, so the test behaves identically
   under dune's sandbox and when run by hand from anywhere. The child
   executable sits next to this one; scratch sources live in a private
   temp directory removed at exit. *)
let exe_dir = Filename.dirname Sys.executable_name

let rec remove_tree path =
  match Sys.is_directory path with
  | true ->
      Array.iter
        (fun name -> remove_tree (Filename.concat path name))
        (Sys.readdir path);
      Sys.rmdir path
  | false -> Sys.remove path
  | exception Sys_error _ -> ()

let scratch_dir =
  let dir = Filename.temp_file "windtrap_cov_scratch" "" in
  Sys.remove dir;
  Sys.mkdir dir 0o755;
  at_exit (fun () -> remove_tree dir);
  dir

let scratch path = Filename.concat scratch_dir path

(* The parent's own at_exit dump must not land in the project's
   _build/_coverage. The destination is resolved at the FIRST
   registration in the process, and under `--instrument-with` that is a
   windtrap core module's, at library load — before this file's
   initializer runs. So the override is set by the dune action
   (WINDTRAP_COVERAGE_FILE=test_coverage.coverage, resolved against the
   action's directory) and not by a putenv here, which would be too late
   to move it and would silently do nothing.

   Later putenv calls (the child tests) do not move it either, for the
   same reason: the path is resolved once. *)

(* Registry: register / visit / snapshot *)

(* Runs [f] with [stderr] captured to a scratch file; returns what it
   wrote. The runtime's warnings end with [%!], so no flushing races. *)
let with_captured_stderr f =
  let path = scratch "stderr.txt" in
  let saved = Unix.dup Unix.stderr in
  let fd =
    Unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644
  in
  Unix.dup2 fd Unix.stderr;
  Unix.close fd;
  Fun.protect
    ~finally:(fun () ->
      flush stderr;
      Unix.dup2 saved Unix.stderr;
      Unix.close saved)
    f;
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let registry_tests =
  [
    test "register and visit appear in the snapshot" (fun () ->
        (* Distinctive extents per registry test keep to_string line
           assertions unambiguous across the shared global registry. *)
        let counts = Array.make 2 0 in
        C.register ~file:"reg_vis.ml"
          ~points:[| pt 100 110; pt 120 130 |]
          ~counts;
        C.visit counts 0;
        let reports = C.file_reports (C.snapshot ()) in
        match List.find_opt (fun r -> r.C.file = "reg_vis.ml") reports with
        | None -> check "visited file appears in snapshot reports" false
        | Some r ->
            check_int "visit marks one of two blocks" ~expected:1
              ~actual:r.C.summary.C.visited;
            check_int "two blocks total" ~expected:2 ~actual:r.C.summary.C.total;
            check "uncovered extent is the unvisited block"
              (r.C.uncovered_extents = [ pt 120 130 ]);
            check "unresolvable source yields no source" (r.C.source = None);
            check "unresolvable source yields no lines"
              (r.C.uncovered_lines = []));
    test "visit saturates at max_int" (fun () ->
        let counts = [| max_int - 1 |] in
        C.register ~file:"reg_sat.ml" ~points:[| pt 7000 7010 |] ~counts;
        C.visit counts 0;
        C.visit counts 0;
        check "visit saturates at max_int"
          (contains
             (Printf.sprintf "7000 7010 %d\n" max_int)
             (C.to_string (C.snapshot ()))));
    test "duplicate registrations sum, blocks counted once" (fun () ->
        (* Same file registered twice with an equal table — a
           functor-style double instantiation: counts add, blocks are
           counted once. *)
        let counts_a = Array.make 1 0 and counts_b = Array.make 1 0 in
        C.register ~file:"reg_dup.ml"
          ~points:[| pt 8000 8010 |]
          ~counts:counts_a;
        C.register ~file:"reg_dup.ml"
          ~points:[| pt 8000 8010 |]
          ~counts:counts_b;
        C.visit counts_a 0;
        C.visit counts_b 0;
        check "duplicate registrations sum in snapshot"
          (contains "8000 8010 2\n" (C.to_string (C.snapshot ())));
        match
          List.find_opt
            (fun r -> r.C.file = "reg_dup.ml")
            (C.file_reports (C.snapshot ()))
        with
        | Some r ->
            check "duplicate registrations never double-count blocks"
              (r.C.summary = { C.visited = 1; total = 1 })
        | None ->
            check "duplicate registrations never double-count blocks" false);
    test "a zero-block file is data, present in reports" (fun () ->
        C.register ~file:"reg_none.ml" ~points:[||] ~counts:[||];
        match
          List.find_opt
            (fun r -> r.C.file = "reg_none.ml")
            (C.file_reports (C.snapshot ()))
        with
        | Some r ->
            check "a zero-block file reports an empty summary"
              (r.C.summary = { C.visited = 0; total = 0 });
            check "a zero-block file has no uncovered extents"
              (r.C.uncovered_extents = [])
        | None -> check "a zero-block file appears in reports" false);
    test "snapshots are isolated copies" (fun () ->
        (* Later visits do not leak into an earlier snapshot. *)
        let counts = Array.make 1 0 in
        C.register ~file:"reg_iso.ml" ~points:[| pt 9000 9010 |] ~counts;
        let before = C.snapshot () in
        C.visit counts 0;
        let after = C.snapshot () in
        check "snapshot taken before a visit is unchanged"
          (contains "9000 9010 0\n" (C.to_string before));
        check "snapshot taken after a visit sees it"
          (contains "9000 9010 1\n" (C.to_string after)));
    test "register and visit reject malformed tables" (fun () ->
        (* Loud rejection — instrumenter bugs fail fast. *)
        check_invalid_arg "register rejects points/counts length mismatch"
          (fun () ->
            C.register ~file:"reg_bad_len.ml"
              ~points:[| pt 0 1 |]
              ~counts:(Array.make 2 0));
        check_invalid_arg "register rejects inverted extent" (fun () ->
            C.register ~file:"reg_bad_ext.ml"
              ~points:[| pt 5 3 |]
              ~counts:(Array.make 1 0));
        check_invalid_arg "register rejects negative extent" (fun () ->
            C.register ~file:"reg_bad_neg.ml"
              ~points:[| pt (-1) 3 |]
              ~counts:(Array.make 1 0));
        check_invalid_arg "register rejects negative count" (fun () ->
            C.register ~file:"reg_bad_cnt.ml"
              ~points:[| pt 0 1 |]
              ~counts:[| -1 |]);
        check_invalid_arg "visit rejects an out-of-bounds index" (fun () ->
            C.visit (Array.make 1 0) 1));
    test "a conflicting registration warns and is dropped" (fun () ->
        (* A conflicting same-file registration is a build problem, not a
           program error: it must warn and be dropped, never raise
           (coverage cannot alter what the program does), and the
           snapshot keeps the first table. *)
        C.register ~file:"reg_conf.ml"
          ~points:[| pt 6000 6010 |]
          ~counts:(Array.make 1 0);
        let err =
          with_captured_stderr (fun () ->
              C.register ~file:"reg_conf.ml"
                ~points:[| pt 6000 6020 |]
                ~counts:(Array.make 1 0))
        in
        check "a conflicting registration warns on stderr"
          (contains "conflicting" err);
        check "the conflict warning suggests dune clean"
          (contains "dune clean" err);
        let serialized = C.to_string (C.snapshot ()) in
        check "a conflict keeps the first registration's table"
          (contains "6000 6010 0\n" serialized);
        check "a conflicting table is dropped from the snapshot"
          (not (contains "6000 6020" serialized)));
  ]

(* Collections: add / merge matrices *)

let ab () =
  let t =
    ok "ab b"
      (C.add C.empty ~file:"lib/b.ml"
         ~points:[| pt 10 20; pt 30 40 |]
         ~counts:[| 1; 0 |])
  in
  ok "ab a" (C.add t ~file:"lib/a.ml" ~points:[| pt 0 5 |] ~counts:[| 7 |])

let ab_serialized =
  "windtrap-coverage-v3\n\
   2\n\
   8 lib/a.ml\n\
   1\n\
   0 5 7\n\
   8 lib/b.ml\n\
   2\n\
   10 20 1\n\
   30 40 0\n"

let digest_of s = Digest.to_hex (Digest.string s)

let collection_tests =
  [
    test "serialization is the frozen v3 format" (fun () ->
        check_string "serialization is the frozen v3 format, files sorted"
          ~expected:ab_serialized
          ~actual:(C.to_string (ab ()));
        let reordered =
          let t =
            ok "reorder a"
              (C.add C.empty ~file:"lib/a.ml"
                 ~points:[| pt 0 5 |]
                 ~counts:[| 7 |])
          in
          ok "reorder b"
            (C.add t ~file:"lib/b.ml"
               ~points:[| pt 10 20; pt 30 40 |]
               ~counts:[| 1; 0 |])
        in
        check_string "serialization is insertion-order independent"
          ~expected:ab_serialized ~actual:(C.to_string reordered);
        check "empty collection is empty" (C.is_empty C.empty);
        check "non-empty collection is not empty" (not (C.is_empty (ab ()))));
    test "the serialized form round-trips, with and without identity" (fun () ->
        (match C.of_string ab_serialized with
        | Ok (reparsed, identity) ->
            check_string "of_string inverts to_string" ~expected:ab_serialized
              ~actual:(C.to_string reparsed);
            check "a collection without an identity line parses to none"
              (identity = None)
        | Error _ -> check "of_string inverts to_string" false);
        let identity =
          { C.exe = "default/test/a.exe"; digest = digest_of "exe-a" }
        in
        let with_identity = C.to_string ~identity (ab ()) in
        check "to_string records the identity after the magic"
          (contains
             (Printf.sprintf
                "windtrap-coverage-v3\nexe %s 18 default/test/a.exe\n2\n"
                identity.C.digest)
             with_identity);
        (match C.of_string with_identity with
        | Ok (reparsed, parsed) ->
            check "the identity line round-trips" (parsed = Some identity);
            check_string "the identity line does not disturb the collection"
              ~expected:ab_serialized ~actual:(C.to_string reparsed)
        | Error _ -> check "the identity line round-trips" false);
        (* Identities with spaces survive the length prefix. *)
        let spaced = { identity with C.exe = "default/my tests/a.exe" } in
        (match C.of_string (C.to_string ~identity:spaced (ab ())) with
        | Ok (_, parsed) ->
            check "an exe path with spaces round-trips" (parsed = Some spaced)
        | Error _ -> check "an exe path with spaces round-trips" false);
        check_invalid_arg "to_string rejects an empty exe path" (fun () ->
            C.to_string ~identity:{ identity with C.exe = "" } (ab ()));
        check_invalid_arg "to_string rejects a malformed digest" (fun () ->
            C.to_string ~identity:{ identity with C.digest = "abc123" } (ab ()));
        check_invalid_arg "to_string rejects an uppercase digest" (fun () ->
            C.to_string
              ~identity:
                {
                  identity with
                  C.digest = String.uppercase_ascii identity.C.digest;
                }
              (ab ())));
    test "merge of disjoint collections is their union" (fun () ->
        let a =
          ok "disjoint a"
            (C.add C.empty ~file:"lib/a.ml"
               ~points:[| pt 0 5 |]
               ~counts:[| 7 |])
        in
        let b =
          ok "disjoint b"
            (C.add C.empty ~file:"lib/b.ml"
               ~points:[| pt 10 20; pt 30 40 |]
               ~counts:[| 1; 0 |])
        in
        check_string "merge of disjoint collections is their union"
          ~expected:ab_serialized
          ~actual:(C.to_string (ok "disjoint merge" (C.merge a b))));
    test "merge adds counts for a shared file" (fun () ->
        let one =
          ok "overlap one"
            (C.add C.empty ~file:"lib/x.ml"
               ~points:[| pt 0 5; pt 6 9 |]
               ~counts:[| 1; 0 |])
        in
        let two =
          ok "overlap two"
            (C.add C.empty ~file:"lib/x.ml"
               ~points:[| pt 0 5; pt 6 9 |]
               ~counts:[| 4; 2 |])
        in
        check_string "merge adds counts for a shared file"
          ~expected:"windtrap-coverage-v3\n1\n8 lib/x.ml\n2\n0 5 5\n6 9 2\n"
          ~actual:(C.to_string (ok "overlap merge" (C.merge one two))));
    test "merge saturates counts at max_int" (fun () ->
        let one =
          ok "sat one"
            (C.add C.empty ~file:"lib/x.ml"
               ~points:[| pt 0 5 |]
               ~counts:[| max_int - 1 |])
        in
        let two =
          ok "sat two"
            (C.add C.empty ~file:"lib/x.ml"
               ~points:[| pt 0 5 |]
               ~counts:[| 5 |])
        in
        check "merge saturates counts at max_int"
          (contains
             (Printf.sprintf "0 5 %d\n" max_int)
             (C.to_string (ok "sat merge" (C.merge one two))));
        let both = ok "sat both" (C.merge two two) in
        check "saturated merge stays non-negative"
          (contains "0 5 10\n" (C.to_string both)));
    test "conflicting point tables fail loudly, never silently" (fun () ->
        let one =
          ok "conflict one"
            (C.add C.empty ~file:"lib/x.ml"
               ~points:[| pt 0 5 |]
               ~counts:[| 1 |])
        in
        let two =
          ok "conflict two"
            (C.add C.empty ~file:"lib/x.ml"
               ~points:[| pt 0 6 |]
               ~counts:[| 1 |])
        in
        (match C.merge one two with
        | Error (C.Point_mismatch { file }) ->
            check_string "mismatch names the conflicting file"
              ~expected:"lib/x.ml" ~actual:file
        | Ok _ | Error _ -> check "conflicting merge is a Point_mismatch" false);
        (match
           C.add one ~file:"lib/x.ml" ~points:[| pt 0 6 |] ~counts:[| 1 |]
         with
        | Error (C.Point_mismatch _) ->
            check "conflicting add is a Point_mismatch" true
        | Ok _ | Error _ -> check "conflicting add is a Point_mismatch" false);
        check
          "mismatch hint: re-run everything together, dune clean as fallback"
          (let message =
             Format.asprintf "%a" C.pp_error
               (C.Point_mismatch { file = "lib/x.ml" })
           in
           contains "dune build @cover" message && contains "dune clean" message);
        check "unknown-format hint instructs deletion, not a re-run alone"
          (let message =
             Format.asprintf "%a" C.pp_error
               (C.Data
                  (I.Unknown_format { path = "old.coverage"; header = "V1" }))
           in
           contains "delete" message && contains "_build/_coverage" message));
    test "empty is a merge identity" (fun () ->
        let t = ab () in
        check_string "empty is a left identity for merge"
          ~expected:ab_serialized
          ~actual:(C.to_string (ok "left id" (C.merge C.empty t)));
        check_string "empty is a right identity for merge"
          ~expected:ab_serialized
          ~actual:(C.to_string (ok "right id" (C.merge t C.empty))));
    test "add rejects malformed tables" (fun () ->
        check_invalid_arg "add rejects points/counts length mismatch" (fun () ->
            C.add C.empty ~file:"x" ~points:[| pt 0 1 |] ~counts:[| 0; 0 |]);
        check_invalid_arg "add rejects inverted extents" (fun () ->
            C.add C.empty ~file:"x" ~points:[| pt 3 1 |] ~counts:[| 0 |]);
        check_invalid_arg "add rejects negative counts" (fun () ->
            C.add C.empty ~file:"x" ~points:[| pt 0 1 |] ~counts:[| -2 |]));
    test "a zero-block file serializes and round-trips" (fun () ->
        (* A zero-block file: data (not emptiness), 0/0 summary, 100%,
           and a stable round trip through the serialized form. *)
        let empty_points_serialized =
          "windtrap-coverage-v3\n1\n8 lib/e.ml\n0\n"
        in
        let t =
          ok "zero-block add"
            (C.add C.empty ~file:"lib/e.ml" ~points:[||] ~counts:[||])
        in
        check "a zero-block file is data, not emptiness" (not (C.is_empty t));
        check "a zero-block collection sums to 0/0"
          (C.summary t = { C.visited = 0; total = 0 });
        check "a 0/0 summary reads as fully covered"
          (C.percentage (C.summary t) = 100.);
        check_string "a zero-block file serializes"
          ~expected:empty_points_serialized ~actual:(C.to_string t);
        check_string "a zero-block file round-trips"
          ~expected:empty_points_serialized
          ~actual:
            (C.to_string
               (ok "zero-block parse"
                  (Result.map fst (C.of_string empty_points_serialized)))));
  ]

(* Rejection of foreign and corrupt data *)

let rejection_tests =
  [
    test "foreign and corrupt data are rejected" (fun () ->
        (match C.of_string "WINDTRAP-COVERAGE-1 1 8 lib/a.ml 1 12 1 34" with
        | Error (C.Data (I.Unknown_format { header; _ })) ->
            check "v1 magic is rejected as unknown format"
              (contains "WINDTRAP-COVERAGE-1" header)
        | Ok _ | Error _ -> check "v1 magic is rejected as unknown format" false);
        (match
           C.of_string "windtrap-coverage-v2\n1\n8 lib/a.ml\n1\n0 5 1\n"
         with
        | Error (C.Data (I.Unknown_format { header; _ })) ->
            check "the pre-release v2 magic is rejected as unknown format"
              (contains "windtrap-coverage-v2" header)
        | Ok _ | Error _ ->
            check "the pre-release v2 magic is rejected as unknown format" false);
        (match C.of_string "" with
        | Error (C.Data (I.Unknown_format _)) ->
            check "empty data is unknown format" true
        | Ok _ | Error _ -> check "empty data is unknown format" false);
        (match C.of_string "windtrap-coverage-v33\n0\n" with
        | Error (C.Data (I.Unknown_format _)) ->
            check "magic must be followed by whitespace" true
        | Ok _ | Error _ -> check "magic must be followed by whitespace" false);
        let corrupt name payload =
          match C.of_string payload with
          | Error (C.Data (I.Corrupt _)) -> check name true
          | Ok _ -> check (name ^ " (parsed!)") false
          | Error _ -> check (name ^ " (wrong error)") false
        in
        corrupt "bare magic is truncated" "windtrap-coverage-v3";
        corrupt "missing file body is corrupt" "windtrap-coverage-v3\n1\n";
        corrupt "truncated file name is corrupt"
          "windtrap-coverage-v3\n1\n99 lib/a.ml\n";
        corrupt "inverted extent is corrupt"
          "windtrap-coverage-v3\n1\n8 lib/a.ml\n1\n5 3 1\n";
        corrupt "negative count is corrupt"
          "windtrap-coverage-v3\n1\n8 lib/a.ml\n1\n0 5 -1\n";
        corrupt "negative point count is corrupt"
          "windtrap-coverage-v3\n1\n8 lib/a.ml\n-1\n";
        corrupt "oversized point count is corrupt"
          "windtrap-coverage-v3\n1\n8 lib/a.ml\n999999999\n0 5 1\n";
        corrupt "trailing garbage is corrupt" "windtrap-coverage-v3\n0\nxx";
        let d32 = String.make 32 'a' in
        corrupt "a truncated exe identity is corrupt"
          (Printf.sprintf "windtrap-coverage-v3\nexe %s 99 default/a.exe\n0\n"
             d32);
        corrupt "an empty exe identity is corrupt"
          (Printf.sprintf "windtrap-coverage-v3\nexe %s 0 \n0\n" d32);
        corrupt "an identity without a digest is corrupt"
          "windtrap-coverage-v3\nexe 14 default/a.exe\n0\n";
        corrupt "an identity with a short digest is corrupt"
          "windtrap-coverage-v3\nexe abc123 14 default/a.exe\n0\n";
        (match C.of_string "windtrap-coverage-v3\n0\n" with
        | Ok (t, _) ->
            check "zero files parses to the empty collection" (C.is_empty t)
        | Error _ -> check "zero files parses to the empty collection" false);
        (match
           C.of_string
             "windtrap-coverage-v3\n\
              2\n\
              8 lib/a.ml\n\
              1\n\
              0 5 1\n\
              8 lib/a.ml\n\
              1\n\
              0 6 1\n"
         with
        | Error (C.Point_mismatch { file }) ->
            check "conflicting duplicate entries in one payload are a mismatch"
              (file = "lib/a.ml")
        | Ok _ | Error _ ->
            check "conflicting duplicate entries in one payload are a mismatch"
              false);
        match
          C.of_string
            "windtrap-coverage-v3\n\
             2\n\
             8 lib/a.ml\n\
             1\n\
             0 5 1\n\
             8 lib/a.ml\n\
             1\n\
             0 5 2\n"
        with
        | Ok (t, _) ->
            check "equal duplicate entries in one payload sum"
              (contains "0 5 3\n" (C.to_string t))
        | Error _ -> check "equal duplicate entries in one payload sum" false);
    test "loading a missing file is Unreadable" (fun () ->
        match C.load "no-such-file.coverage" with
        | Error (C.Data (I.Unreadable _)) ->
            check "loading a missing file is Unreadable" true
        | Ok _ | Error _ -> check "loading a missing file is Unreadable" false);
  ]

(* Deterministic output filenames, the root rule, identities *)

let filename_tests =
  [
    test "output filenames are deterministic" (fun () ->
        let direct = C.output_dir ~exe:"/w/p/_build/default/test/a.exe" in
        check_string "output_dir is deterministic" ~expected:direct
          ~actual:(C.output_dir ~exe:"/w/p/_build/default/test/a.exe");
        check "output_dir lives under the root's _build/_coverage"
          (String.starts_with ~prefix:"/w/p/_build/_coverage/windtrap-" direct);
        check "output_dir is a directory name, not a file's"
          (Filename.extension direct = "");
        check_string "sandboxed and direct runs share a file" ~expected:direct
          ~actual:
            (C.output_dir ~exe:"/w/p/_build/.sandbox/0abc12/default/test/a.exe");
        check "different executables get different files"
          (direct <> C.output_dir ~exe:"/w/p/_build/default/test/b.exe");
        check "different build contexts get different files"
          (direct <> C.output_dir ~exe:"/w/p/_build/alt/test/a.exe");
        check "the project location does not affect the name"
          (Filename.basename direct
          = Filename.basename
              (C.output_dir ~exe:"/elsewhere/_build/default/test/a.exe"));
        let outside = C.output_dir ~exe:"/opt/tools/mytool.exe" in
        check "an executable outside _build dumps under the current directory"
          (String.starts_with
             ~prefix:
               (Filename.concat (Sys.getcwd ()) "_build/_coverage/windtrap-")
             outside));
    test "build_root and exe_identity follow the topmost _build" (fun () ->
        check "build_root is the parent of the topmost _build component"
          (I.build_root ~path:"/w/p/_build/default/test" = Some "/w/p");
        check "build_root sees through the sandbox to the same root"
          (I.build_root ~path:"/w/p/_build/.sandbox/0abc12/default/test"
          = Some "/w/p");
        check "the topmost _build wins over planted inner ones"
          (I.build_root ~path:"/w/p/_build/.sandbox/_build/_coverage"
          = Some "/w/p");
        check "build_root outside _build is None"
          (I.build_root ~path:"/w/p/src/lib" = None);
        check "a relative path resolves against the current directory first"
          (I.build_root ~path:"src/lib"
          = I.build_root ~path:(Filename.concat (Sys.getcwd ()) "src/lib"));
        check_string "exe_identity is the path below _build"
          ~expected:"default/test/a.exe"
          ~actual:(I.exe_identity ~exe:"/w/p/_build/default/test/a.exe");
        check_string "exe_identity strips the sandbox prefix"
          ~expected:"default/test/a.exe"
          ~actual:
            (I.exe_identity
               ~exe:"/w/p/_build/.sandbox/0abc12/default/test/a.exe");
        check_string "exe_identity outside _build is the absolute path"
          ~expected:"/opt/tools/mytool.exe"
          ~actual:(I.exe_identity ~exe:"/opt/tools/mytool.exe");
        check "the identity is what output_dir hashes"
          (C.output_dir ~exe:"/w/p/_build/default/test/a.exe"
          = C.output_dir ~exe:"/w/p/_build/.sandbox/9f/default/test/a.exe"));
    (* One executable is one dump. Dune spells the same binary
       [runner_main.exe] in one rule and [./runner_main.exe] in another,
       and a suite that spawns a sibling names it [../../bin/main.exe];
       a key that kept the spelling would file a dump per spelling and
       leave every one but the last for the report to call stale. *)
    test "one executable is one identity, however it is spelled" (fun () ->
        let direct = "/w/p/_build/default/test/a.exe" in
        check_string "a . component is not a directory"
          ~expected:"default/test/a.exe"
          ~actual:(I.exe_identity ~exe:"/w/p/_build/default/test/./a.exe");
        check_string "nor is a chain of them" ~expected:"default/test/a.exe"
          ~actual:(I.exe_identity ~exe:"/w/p/./_build/./default/test/a.exe");
        check_string "a .. is the directory above it"
          ~expected:"default/test/a.exe"
          ~actual:(I.exe_identity ~exe:"/w/p/_build/default/test/sub/../a.exe");
        check_string "a doubled separator is one" ~expected:"default/test/a.exe"
          ~actual:(I.exe_identity ~exe:"/w/p/_build/default//test/a.exe");
        check "and every one of them shares the direct run's file"
          (List.for_all
             (fun spelling ->
               C.output_dir ~exe:spelling = C.output_dir ~exe:direct)
             [
               "/w/p/_build/default/test/./a.exe";
               "/w/p/_build/default/test/sub/../a.exe";
               "/w/p/_build/default/test/a.exe";
             ]);
        check_string "the rule reaches outside _build too"
          ~expected:"/opt/tools/mytool.exe"
          ~actual:(I.exe_identity ~exe:"/opt/tools/bin/./../mytool.exe");
        check ".. above the root stops at the root"
          (I.exe_identity ~exe:"/../../opt/mytool.exe" = "/opt/mytool.exe"));
  ]

(* Extent -> line derivation *)

let three_lines = "line one\nline two\nline three\n"
(* offsets: line 1 = 0-8 (newline at 8), line 2 = 9-17, line 3 = 18-27 *)

let line_tests =
  [
    test "extents derive their line numbers" (fun () ->
        let lines extents = C.lines_of_extents ~source:three_lines extents in
        check "an extent spanning the file marks every line"
          (lines [ pt 0 28 ] = [ 1; 2; 3 ]);
        check "an exact line extent marks only its line"
          (lines [ pt 9 17 ] = [ 2 ]);
        check "an extent ending on a line's newline stays on that line"
          (lines [ pt 9 18 ] = [ 2 ]);
        check "an extent crossing a newline marks both lines"
          (lines [ pt 9 19 ] = [ 2; 3 ]);
        check "two extents on one line mark it once"
          (lines [ pt 0 4; pt 5 8 ] = [ 1 ]);
        check "an empty extent marks the line containing it"
          (lines [ pt 9 9 ] = [ 2 ]);
        check "offsets past the end clamp to the last line"
          (lines [ pt 100 200 ] = [ 3 ]);
        check "nested uncovered extents are the outer extent's lines"
          (lines [ pt 0 28; pt 9 17 ] = [ 1; 2; 3 ]);
        check "no extents mark no lines" (lines [] = []);
        check "an empty source has no lines"
          (C.lines_of_extents ~source:"" [ pt 0 5 ] = []);
        check "a source without a trailing newline keeps its last line"
          (C.lines_of_extents ~source:"a\nb" [ pt 2 3 ] = [ 2 ]));
    test "a huge file stays exact" (fun () ->
        (* One file-spanning extent marks every line and collapses to a
           single range. *)
        let n = 20_000 in
        let buf = Buffer.create (n * 8) in
        for i = 1 to n do
          Printf.bprintf buf "line %d\n" i
        done;
        let source = Buffer.contents buf in
        let lines =
          C.lines_of_extents ~source [ pt 0 (String.length source) ]
        in
        check_int "a file-spanning extent marks every line of a huge file"
          ~expected:n ~actual:(List.length lines));
  ]

(* Summaries *)

let summary_tests =
  [
    test "aggregate summary sums files" (fun () ->
        check "aggregate summary sums files"
          (C.summary (ab ()) = { C.visited = 2; total = 3 }));
  ]

(* Reports against real sources *)

let write_source path contents =
  let rec mkdir_p dir =
    if dir = "" || dir = "." || Sys.file_exists dir then ()
    else begin
      mkdir_p (Filename.dirname dir);
      Sys.mkdir dir 0o755
    end
  in
  mkdir_p (Filename.dirname path);
  let oc = open_out_bin path in
  output_string oc contents;
  close_out oc

let report_tests =
  [
    test "reports resolve sources; inner extents override outer" (fun () ->
        (* A visited outer block containing an unvisited inner block:
           only the inner extent is reported (the inner-overrides-outer
           rendering rule). *)
        write_source (scratch "lib/eval.ml") three_lines;
        let t =
          ok "report add"
            (C.add C.empty ~file:"lib/eval.ml"
               ~points:[| pt 0 28; pt 9 17 |]
               ~counts:[| 1; 0 |])
        in
        (match C.file_reports ~source_roots:[ scratch_dir ] t with
        | [ r ] ->
            check_string "report names the source file" ~expected:"lib/eval.ml"
              ~actual:r.C.file;
            check "report resolves the source under the given root"
              (r.C.source = Some three_lines);
            check "a matching source is not stale" (not r.C.stale);
            check "only the unvisited inner extent is uncovered"
              (r.C.uncovered_extents = [ pt 9 17 ]);
            check "uncovered lines are the inner block's, not the outer's"
              (r.C.uncovered_lines = [ 2 ]);
            check
              "line hits are the fewest visits of any point touching the line"
              (r.C.line_hits = [ (1, 1); (2, 0); (3, 1) ]);
            check "report summary counts blocks"
              (r.C.summary = { C.visited = 1; total = 2 })
        | reports ->
            check_int "one file yields one report" ~expected:1
              ~actual:(List.length reports));
        (* Reports stay useful without sources: extents survive, lines
           are empty. *)
        match C.file_reports ~source_roots:[ scratch_dir ] (ab ()) with
        | [ ra; rb ] ->
            check_string "reports are ordered by file name" ~expected:"lib/a.ml"
              ~actual:ra.C.file;
            check "a missing source is reported as absent" (rb.C.source = None);
            check "a missing source is not stale" (not rb.C.stale);
            check "extents are available without the source"
              (rb.C.uncovered_extents = [ pt 30 40 ]);
            check "no source means no line numbers" (rb.C.uncovered_lines = []);
            check "nor line hits" (rb.C.line_hits = [])
        | reports ->
            check_int "two files yield two reports" ~expected:2
              ~actual:(List.length reports));
    test "stale sources are reported loudly" (fun () ->
        (* Stale data: the source shrank since the run, so its extents
           reach past the end. The report must say so loudly instead of
           painting whatever now sits at those lines. *)
        write_source (scratch "lib/stale.ml") three_lines;
        let t =
          ok "stale add"
            (C.add C.empty ~file:"lib/stale.ml"
               ~points:[| pt 0 10; pt 20 40 |]
               ~counts:[| 1; 0 |])
        in
        (match C.file_reports ~source_roots:[ scratch_dir ] t with
        | [ r ] ->
            check "a source shorter than the extents is stale" r.C.stale;
            check "a stale report withholds the source" (r.C.source = None);
            check "a stale report paints no lines" (r.C.uncovered_lines = []);
            check "a stale report keeps its extents"
              (r.C.uncovered_extents = [ pt 20 40 ]);
            check "a stale report keeps its summary"
              (r.C.summary = { C.visited = 1; total = 2 })
        | reports ->
            check_int "stale file yields one report" ~expected:1
              ~actual:(List.length reports));
        (* An extent ending exactly at the last byte is consistent. *)
        let t =
          ok "eof add"
            (C.add C.empty ~file:"lib/stale.ml"
               ~points:[| pt 0 (String.length three_lines) |]
               ~counts:[| 0 |])
        in
        match C.file_reports ~source_roots:[ scratch_dir ] t with
        | [ r ] ->
            check "an extent ending at EOF is not stale"
              ((not r.C.stale) && r.C.source = Some three_lines);
            check "an extent ending at EOF paints to the last line"
              (r.C.uncovered_lines = [ 1; 2; 3 ])
        | reports ->
            check_int "eof file yields one report" ~expected:1
              ~actual:(List.length reports));
  ]

(* The at_exit dump, end to end *)

let child_expected counts =
  Printf.sprintf
    "windtrap-coverage-v3\n1\n12 lib/child.ml\n3\n0 5 %d\n6 9 %d\n10 20 %d\n"
    counts.(0) counts.(1) counts.(2)

let dump_tests =
  [
    test "the at_exit dump works end to end" (fun () ->
        let child_exe = Filename.concat exe_dir "dump_child.exe" in
        let child_file = scratch "child.coverage" in
        Unix.putenv "WINDTRAP_COVERAGE_FILE" child_file;
        let run ?stderr mode =
          Sys.command (Filename.quote_command child_exe ?stderr [ mode ])
        in
        check_int "child run exits 0" ~expected:0 ~actual:(run "first");
        (match C.load child_file with
        | Ok (t, exe) ->
            check_string "the at_exit dump round-trips through load"
              ~expected:(child_expected [| 1; 0; 0 |])
              ~actual:(C.to_string t);
            (* The dump records its writer: the child's identity under
               this checkout's _build (sandbox-stripped, so it agrees
               with the parent's spelling of the same path) and its
               content digest. *)
            check "the dump records the child's executable identity"
              (exe
              = Some
                  {
                    C.exe = I.exe_identity ~exe:child_exe;
                    digest = Digest.to_hex (Digest.file child_exe);
                  })
        | Error e ->
            check
              (Printf.sprintf "child dump loads (%s)"
                 (Format.asprintf "%a" C.pp_error e))
              false);
        check_int "child re-run exits 0" ~expected:0 ~actual:(run "second");
        (match C.load child_file with
        | Ok (t, _) ->
            check_string "a re-run overwrites, never accumulates"
              ~expected:(child_expected [| 1; 2; 0 |])
              ~actual:(C.to_string t)
        | Error _ -> check "a re-run overwrites, never accumulates" false);
        let residue =
          Sys.readdir (Filename.dirname child_file)
          |> Array.to_list
          |> List.filter (fun name -> Filename.check_suffix name ".tmp")
        in
        check "the dump leaves no temporary files behind" (residue = []);
        (* A child linking two incompatible instrumentations of one file
           still exits 0 (never a crash at module load), dumps the first
           table, and warns on stderr. *)
        let conflict_err = scratch "conflict-stderr.txt" in
        check_int "conflicting child exits 0" ~expected:0
          ~actual:(run ~stderr:conflict_err "conflict");
        (match C.load child_file with
        | Ok (t, _) ->
            check_string "a conflicting child dumps the first table"
              ~expected:(child_expected [| 1; 0; 0 |])
              ~actual:(C.to_string t)
        | Error _ -> check "a conflicting child dumps the first table" false);
        (match read_file conflict_err with
        | Some err ->
            check "the conflicting child warns on stderr"
              (contains "conflicting" err && contains "dune clean" err)
        | None -> check "the conflicting child warns on stderr" false);
        Sys.remove child_file;
        check_int "silent child exits 0" ~expected:0 ~actual:(run "silent");
        check "a process with no registrations writes no file"
          (not (Sys.file_exists child_file)));
    (* The default destination: the executable's own directory, where
       every run keeps its own file. The child is copied under a scratch
       _build so that directory is scratch's, never this checkout's. *)
    test "runs of one executable accumulate; a rebuild's first run supersedes"
      (fun () ->
        let root = scratch "runs" in
        let exe = Filename.concat root "_build/default/child.exe" in
        (match read_file (Filename.concat exe_dir "dump_child.exe") with
        | Some bytes ->
            I.write_file exe bytes;
            Unix.chmod exe 0o755
        | None -> failf "cannot read dump_child.exe");
        Unix.putenv "WINDTRAP_COVERAGE_FILE" "";
        let run mode = Sys.command (Filename.quote_command exe [ mode ]) in
        let dir = C.output_dir ~exe in
        check_string "the directory sits under the executable's root"
          ~expected:(Filename.concat root "_build/_coverage")
          ~actual:(Filename.dirname dir);
        let dumps () =
          match Sys.readdir dir with
          | exception Sys_error _ -> []
          | names ->
              Array.to_list names
              |> List.filter (fun n -> Filename.check_suffix n ".coverage")
              |> List.sort compare
        in
        check_int "first run exits 0" ~expected:0 ~actual:(run "first");
        check_int "second run exits 0" ~expected:0 ~actual:(run "second");
        check_int "each run writes its own file" ~expected:2
          ~actual:(List.length (dumps ()));
        let digest = Digest.to_hex (Digest.file exe) in
        check "the files are named after the writer's digest"
          (List.for_all (String.starts_with ~prefix:(digest ^ "-")) (dumps ()));
        let merged =
          List.fold_left
            (fun acc name ->
              match C.load (Filename.concat dir name) with
              | Ok (t, _) -> ok "merge of the runs" (C.merge acc t)
              | Error e -> failf "%s: %a" name C.pp_error e)
            C.empty (dumps ())
        in
        check_string "the runs add up in the merge"
          ~expected:(child_expected [| 2; 2; 0 |])
          ~actual:(C.to_string merged);
        (* A predecessor's dump: named after, and recording, another
           build's digest. The next run removes it and keeps its own. *)
        let older = Digest.to_hex (Digest.string "an older build") in
        let stale = Filename.concat dir (older ^ "-000001.coverage") in
        I.write_file stale
          (C.to_string
             ~identity:{ C.exe = I.exe_identity ~exe; digest = older }
             merged);
        check_int "third run exits 0" ~expected:0 ~actual:(run "first");
        check "a rebuilt executable's first run removes its predecessors' dumps"
          (not (Sys.file_exists stale));
        check_int "and keeps every run of its own" ~expected:3
          ~actual:(List.length (dumps ()));
        check "no temporary files remain"
          (not
             (Array.exists
                (fun n -> Filename.check_suffix n ".tmp")
                (Sys.readdir dir))));
  ]

(* The suite *)

let () =
  run "coverage"
    [
      group "registry" registry_tests;
      group "collections" collection_tests;
      group "parse" rejection_tests;
      group "filenames" filename_tests;
      group "lines" line_tests;
      group "summaries" summary_tests;
      group "reports" report_tests;
      group "dump" dump_tests;
    ]
