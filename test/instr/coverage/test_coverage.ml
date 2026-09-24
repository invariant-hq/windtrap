(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Windtrap_runtime.Coverage: the register/visit/snapshot registry
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
   registry, which is never reset: each registers under a file name of
   its own ([fresh]) and reads that file back alone. *)

open Windtrap
module C = Windtrap_runtime.Coverage
module I = Windtrap_runtime.Instr
module Child = Windtrap_test_support.Child

let pt start_ofs end_ofs = { C.start_ofs; end_ofs }

let point =
  Testable.structural ~pp:(fun ppf (p : C.point) ->
      Format.fprintf ppf "%d-%d" p.C.start_ofs p.C.end_ofs)

let summary =
  Testable.contramap
    (fun (s : C.summary) -> (s.C.visited, s.C.total))
    (pair int int)

let read_file path = In_channel.with_open_bin path In_channel.input_all

let ok name = function
  | Ok t -> t
  | Error e -> failf "%s: unexpected error: %a" name C.pp_error e

(* Hermeticity: all paths are absolute, so the test behaves identically
   under dune's sandbox and when run by hand from anywhere. The child
   executable sits next to this one; scratch sources live in the test's
   own temporary directory. *)
let exe_dir = Filename.dirname Sys.executable_name

(* The parent's own at_exit dump must not land in the project's
   _build/_coverage. The destination is resolved at the FIRST
   registration in the process, and under `--instrument-with` that is a
   windtrap core module's, at library load — before this file's
   initializer runs. So the override is set by the dune action
   (WINDTRAP_COVERAGE_FILE=test_coverage.coverage, resolved against the
   action's directory) and not by a putenv here, which would be too late
   to move it and would silently do nothing. The children are started
   with their own variable stated, never with the parent's. *)

(* Registry: register / visit / snapshot *)

(* Runs [f] with [stderr] captured to a scratch file; returns what it
   wrote. The runtime's warnings end with [%!], so no flushing races. *)
let with_captured_stderr f =
  let path = temp_file () in
  let saved = Unix.dup Unix.stderr in
  let fd = Unix.openfile path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o644 in
  Unix.dup2 fd Unix.stderr;
  Unix.close fd;
  Fun.protect
    ~finally:(fun () ->
      flush stderr;
      Unix.dup2 saved Unix.stderr;
      Unix.close saved)
    f;
  read_file path

(* The registry is a process global, and a test here may run more than
   once in one process: the mutation loop's probe and its children are
   forks of the process that ran the dry run, registrations included. So
   every registry test registers under a name no earlier run of it used
   and reads its own file back alone; the extents stay distinctive per
   test so a line assertion cannot match another test's table either. *)
let fresh =
  let n = ref 0 in
  fun base ->
    incr n;
    Printf.sprintf "%s_%d.ml" base !n

let own file = C.filter (String.equal file) (C.snapshot ())

let registry_tests =
  [
    test "register and visit appear in the snapshot" (fun () ->
        let file = fresh "reg_vis" in
        let counts = Array.make 2 0 in
        C.register ~file ~points:[| pt 100 110; pt 120 130 |] ~counts;
        C.visit counts 0;
        match C.file_reports (own file) with
        | [ r ] ->
            equal ~msg:"visit marks one of two blocks" int 1
              r.C.summary.C.visited;
            equal ~msg:"two blocks total" int 2 r.C.summary.C.total;
            equal ~msg:"uncovered extent is the unvisited block" (list point)
              [ pt 120 130 ]
              r.C.uncovered_extents;
            is_none ~msg:"unresolvable source yields no source" r.C.source;
            equal ~msg:"unresolvable source yields no lines" (list int) []
              r.C.uncovered_lines
        | reports ->
            failf "the visited file is one report of the snapshot, got %d"
              (List.length reports));
    test "visit saturates at max_int" (fun () ->
        let file = fresh "reg_sat" in
        let counts = [| max_int - 1 |] in
        C.register ~file ~points:[| pt 7000 7010 |] ~counts;
        C.visit counts 0;
        C.visit counts 0;
        contains ~msg:"visit saturates at max_int"
          ~sub:(Printf.sprintf "7000 7010 %d\n" max_int)
          (C.to_string (own file)));
    test "duplicate registrations sum, blocks counted once" (fun () ->
        (* Same file registered twice with an equal table — a
           functor-style double instantiation: counts add, blocks are
           counted once. *)
        let file = fresh "reg_dup" in
        let counts_a = Array.make 1 0 and counts_b = Array.make 1 0 in
        C.register ~file ~points:[| pt 8000 8010 |] ~counts:counts_a;
        C.register ~file ~points:[| pt 8000 8010 |] ~counts:counts_b;
        C.visit counts_a 0;
        C.visit counts_b 0;
        contains ~msg:"duplicate registrations sum in snapshot"
          ~sub:"8000 8010 2\n"
          (C.to_string (own file));
        match C.file_reports (own file) with
        | [ r ] ->
            equal ~msg:"duplicate registrations never double-count blocks"
              summary
              { C.visited = 1; total = 1 }
              r.C.summary
        | reports ->
            failf "the file is one report, got %d" (List.length reports));
    test "a zero-block file is data, present in reports" (fun () ->
        let file = fresh "reg_none" in
        C.register ~file ~points:[||] ~counts:[||];
        match C.file_reports (own file) with
        | [ r ] ->
            equal ~msg:"a zero-block file reports an empty summary" summary
              { C.visited = 0; total = 0 }
              r.C.summary;
            equal ~msg:"a zero-block file has no uncovered extents" (list point)
              [] r.C.uncovered_extents
        | reports ->
            failf "a zero-block file is one report, got %d"
              (List.length reports));
    test "snapshots are isolated copies" (fun () ->
        (* Later visits do not leak into an earlier snapshot. *)
        let file = fresh "reg_iso" in
        let counts = Array.make 1 0 in
        C.register ~file ~points:[| pt 9000 9010 |] ~counts;
        let before = C.snapshot () in
        C.visit counts 0;
        let after = C.snapshot () in
        let of_file t = C.to_string (C.filter (String.equal file) t) in
        contains ~msg:"snapshot taken before a visit is unchanged"
          ~sub:"9000 9010 0\n" (of_file before);
        contains ~msg:"snapshot taken after a visit sees it"
          ~sub:"9000 9010 1\n" (of_file after));
    test "register and visit reject malformed tables" (fun () ->
        (* Loud rejection — instrumenter bugs fail fast. *)
        raises_match ~msg:"register rejects points/counts length mismatch"
          Exn.invalid_arg (fun () ->
            C.register ~file:"reg_bad_len.ml"
              ~points:[| pt 0 1 |]
              ~counts:(Array.make 2 0));
        raises_match ~msg:"register rejects inverted extent" Exn.invalid_arg
          (fun () ->
            C.register ~file:"reg_bad_ext.ml"
              ~points:[| pt 5 3 |]
              ~counts:(Array.make 1 0));
        raises_match ~msg:"register rejects negative extent" Exn.invalid_arg
          (fun () ->
            C.register ~file:"reg_bad_neg.ml"
              ~points:[| pt (-1) 3 |]
              ~counts:(Array.make 1 0));
        raises_match ~msg:"register rejects negative count" Exn.invalid_arg
          (fun () ->
            C.register ~file:"reg_bad_cnt.ml"
              ~points:[| pt 0 1 |]
              ~counts:[| -1 |]);
        raises_match ~msg:"visit rejects an out-of-bounds index" Exn.invalid_arg
          (fun () -> C.visit (Array.make 1 0) 1));
    test "a conflicting registration warns and is dropped" (fun () ->
        (* A conflicting same-file registration is a build problem, not a
           program error: it must warn and be dropped, never raise
           (coverage cannot alter what the program does), and the
           snapshot keeps the first table. *)
        let file = fresh "reg_conf" in
        C.register ~file ~points:[| pt 6000 6010 |] ~counts:(Array.make 1 0);
        let err =
          with_captured_stderr (fun () ->
              C.register ~file
                ~points:[| pt 6000 6020 |]
                ~counts:(Array.make 1 0))
        in
        equal ~msg:"behind windtrap's one anchor, a warning, the file first"
          string
          (Printf.sprintf
             "windtrap: warning: %s: conflicting instrumentation tables in one \
              executable (stale build artifacts? rebuild from clean); ignoring \
              one module's coverage data\n"
             file)
          err;
        let serialized = C.to_string (own file) in
        contains ~msg:"a conflict keeps the first registration's table"
          ~sub:"6000 6010 0\n" serialized;
        not_contains ~msg:"a conflicting table is dropped from the snapshot"
          ~sub:"6000 6020" serialized);
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
        equal ~msg:"serialization is the frozen v3 format, files sorted" string
          ab_serialized
          (C.to_string (ab ()));
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
        equal ~msg:"serialization is insertion-order independent" string
          ab_serialized (C.to_string reordered);
        is_true ~msg:"empty collection is empty" (C.is_empty C.empty);
        is_false ~msg:"non-empty collection is not empty" (C.is_empty (ab ())));
    test "the serialized form round-trips, with and without identity" (fun () ->
        let reparsed, identity =
          ok "of_string inverts to_string" (C.of_string ab_serialized)
        in
        equal ~msg:"of_string inverts to_string" string ab_serialized
          (C.to_string reparsed);
        is_none ~msg:"a collection without an identity line parses to none"
          identity;
        let identity =
          { C.exe = "default/test/a.exe"; digest = digest_of "exe-a" }
        in
        let with_identity = C.to_string ~identity (ab ()) in
        contains ~msg:"to_string records the identity after the magic"
          ~sub:
            (Printf.sprintf
               "windtrap-coverage-v3\nexe %s 18 default/test/a.exe\n2\n"
               identity.C.digest)
          with_identity;
        let reparsed, parsed =
          ok "the identity line round-trips" (C.of_string with_identity)
        in
        is_true ~msg:"the identity line round-trips" (parsed = Some identity);
        equal ~msg:"the identity line does not disturb the collection" string
          ab_serialized (C.to_string reparsed);
        (* Identities with spaces survive the length prefix. *)
        let spaced = { identity with C.exe = "default/my tests/a.exe" } in
        let _, parsed =
          ok "an exe path with spaces round-trips"
            (C.of_string (C.to_string ~identity:spaced (ab ())))
        in
        is_true ~msg:"an exe path with spaces round-trips" (parsed = Some spaced);
        raises_match ~msg:"to_string rejects an empty exe path" Exn.invalid_arg
          (fun () -> C.to_string ~identity:{ identity with C.exe = "" } (ab ()));
        raises_match ~msg:"to_string rejects a malformed digest" Exn.invalid_arg
          (fun () ->
            C.to_string ~identity:{ identity with C.digest = "abc123" } (ab ()));
        raises_match ~msg:"to_string rejects an uppercase digest"
          Exn.invalid_arg (fun () ->
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
        equal ~msg:"merge of disjoint collections is their union" string
          ab_serialized
          (C.to_string (ok "disjoint merge" (C.merge a b))));
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
        equal ~msg:"merge adds counts for a shared file" string
          "windtrap-coverage-v3\n1\n8 lib/x.ml\n2\n0 5 5\n6 9 2\n"
          (C.to_string (ok "overlap merge" (C.merge one two))));
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
        contains ~msg:"merge saturates counts at max_int"
          ~sub:(Printf.sprintf "0 5 %d\n" max_int)
          (C.to_string (ok "sat merge" (C.merge one two)));
        let both = ok "sat both" (C.merge two two) in
        contains ~msg:"saturated merge stays non-negative" ~sub:"0 5 10\n"
          (C.to_string both));
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
            equal ~msg:"mismatch names the conflicting file" string "lib/x.ml"
              file
        | Ok _ -> fail "a conflicting merge succeeded"
        | Error e -> failf "expected Point_mismatch, got %a" C.pp_error e);
        (match
           C.add one ~file:"lib/x.ml" ~points:[| pt 0 6 |] ~counts:[| 1 |]
         with
        | Error (C.Point_mismatch _) -> ()
        | Ok _ -> fail "a conflicting add succeeded"
        | Error e -> failf "expected Point_mismatch, got %a" C.pp_error e);
        (* The mismatch hint: re-run everything together, deletion as
           fallback. *)
        let message =
          Format.asprintf "%a" C.pp_error
            (C.Point_mismatch { file = "lib/x.ml" })
        in
        contains ~msg:"the mismatch hint is a re-run from one build"
          ~sub:"from one build" message;
        contains ~msg:"with deletion as the fallback"
          ~sub:"delete the coverage files" message;
        not_contains ~msg:"and names no build tool" ~sub:"dune " message;
        contains
          ~msg:"unknown-format hint instructs deletion, not a re-run alone"
          ~sub:"delete the stale coverage files"
          (Format.asprintf "%a" C.pp_error
             (C.Data (I.Unknown_format { path = "old.coverage"; header = "V1" }))));
    test "empty is a merge identity" (fun () ->
        let t = ab () in
        equal ~msg:"empty is a left identity for merge" string ab_serialized
          (C.to_string (ok "left id" (C.merge C.empty t)));
        equal ~msg:"empty is a right identity for merge" string ab_serialized
          (C.to_string (ok "right id" (C.merge t C.empty))));
    test "add rejects malformed tables" (fun () ->
        raises_match ~msg:"add rejects points/counts length mismatch"
          Exn.invalid_arg (fun () ->
            C.add C.empty ~file:"x" ~points:[| pt 0 1 |] ~counts:[| 0; 0 |]);
        raises_match ~msg:"add rejects inverted extents" Exn.invalid_arg
          (fun () ->
            C.add C.empty ~file:"x" ~points:[| pt 3 1 |] ~counts:[| 0 |]);
        raises_match ~msg:"add rejects negative counts" Exn.invalid_arg
          (fun () ->
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
        is_false ~msg:"a zero-block file is data, not emptiness" (C.is_empty t);
        equal ~msg:"a zero-block collection sums to 0/0" summary
          { C.visited = 0; total = 0 }
          (C.summary t);
        equal ~msg:"a 0/0 summary reads as fully covered" float_exact 100.
          (C.percentage (C.summary t));
        equal ~msg:"a zero-block file serializes" string empty_points_serialized
          (C.to_string t);
        equal ~msg:"a zero-block file round-trips" string
          empty_points_serialized
          (C.to_string
             (ok "zero-block parse"
                (Result.map fst (C.of_string empty_points_serialized)))));
  ]

(* Rejection of foreign and corrupt data *)

let rejection_tests =
  [
    test "foreign and corrupt data are rejected" (fun () ->
        let unknown name payload ~header =
          match C.of_string payload with
          | Error (C.Data (I.Unknown_format { header = h; _ })) ->
              contains ~msg:name ~sub:header h
          | Ok _ -> failf "%s: parsed" name
          | Error e -> failf "%s: got %a" name C.pp_error e
        in
        unknown "v1 magic is rejected as unknown format"
          "WINDTRAP-COVERAGE-1 1 8 lib/a.ml 1 12 1 34"
          ~header:"WINDTRAP-COVERAGE-1";
        unknown "the pre-release v2 magic is rejected as unknown format"
          "windtrap-coverage-v2\n1\n8 lib/a.ml\n1\n0 5 1\n"
          ~header:"windtrap-coverage-v2";
        unknown "empty data is unknown format" "" ~header:"";
        unknown "magic must be followed by whitespace"
          "windtrap-coverage-v33\n0\n" ~header:"";
        let corrupt name payload =
          match C.of_string payload with
          | Error (C.Data (I.Corrupt _)) -> ()
          | Ok _ -> failf "%s: parsed" name
          | Error e -> failf "%s: got %a" name C.pp_error e
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
        let t, _ =
          ok "zero files parses" (C.of_string "windtrap-coverage-v3\n0\n")
        in
        is_true ~msg:"zero files parses to the empty collection" (C.is_empty t);
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
            equal
              ~msg:"conflicting duplicate entries in one payload are a mismatch"
              string "lib/a.ml" file
        | Ok _ -> fail "conflicting duplicate entries in one payload parsed"
        | Error e -> failf "expected Point_mismatch, got %a" C.pp_error e);
        let t, _ =
          ok "equal duplicate entries in one payload"
            (C.of_string
               "windtrap-coverage-v3\n\
                2\n\
                8 lib/a.ml\n\
                1\n\
                0 5 1\n\
                8 lib/a.ml\n\
                1\n\
                0 5 2\n")
        in
        contains ~msg:"equal duplicate entries in one payload sum"
          ~sub:"0 5 3\n" (C.to_string t));
    test "loading a missing file is Unreadable" (fun () ->
        match
          C.load (Filename.concat (temp_dir ()) "no-such-file.coverage")
        with
        | Error (C.Data (I.Unreadable _)) -> ()
        | Ok _ -> fail "a missing file loaded"
        | Error e -> failf "expected Unreadable, got %a" C.pp_error e);
  ]

(* Deterministic output filenames, the root rule, identities *)

let filename_tests =
  [
    test "output filenames are deterministic" (fun () ->
        let direct = C.output_dir ~exe:"/w/p/_build/default/test/a.exe" in
        equal ~msg:"output_dir is deterministic" string direct
          (C.output_dir ~exe:"/w/p/_build/default/test/a.exe");
        starts_with ~msg:"output_dir lives under the root's _build/_coverage"
          ~affix:"/w/p/_build/_coverage/windtrap-" direct;
        equal ~msg:"output_dir is a directory name, not a file's" string ""
          (Filename.extension direct);
        equal ~msg:"sandboxed and direct runs share a file" string direct
          (C.output_dir ~exe:"/w/p/_build/.sandbox/0abc12/default/test/a.exe");
        not_equal ~msg:"different executables get different files" string direct
          (C.output_dir ~exe:"/w/p/_build/default/test/b.exe");
        not_equal ~msg:"different build contexts get different files" string
          direct
          (C.output_dir ~exe:"/w/p/_build/alt/test/a.exe");
        equal ~msg:"the project location does not affect the name" string
          (Filename.basename direct)
          (Filename.basename
             (C.output_dir ~exe:"/elsewhere/_build/default/test/a.exe"));
        starts_with ~msg:"a private build directory keeps its own dumps"
          ~affix:"/w/p/_build_ci/_coverage/windtrap-"
          (C.output_dir ~exe:"/w/p/_build_ci/default/test/a.exe");
        (* A tree built without dune must never grow a _build: the dumps
           go under the working directory's own _windtrap. *)
        starts_with
          ~msg:"an executable under no build directory dumps under _windtrap"
          ~affix:
            (Filename.concat (Sys.getcwd ()) "_windtrap/coverage/windtrap-")
          (C.output_dir ~exe:"/opt/tools/mytool.exe"));
    test "build_dir, build_root and exe_identity follow the first _build*"
      (fun () ->
        equal ~msg:"build_dir is the path cut after the first _build* component"
          (option string) (Some "/w/p/_build")
          (I.build_dir ~path:"/w/p/_build/default/test");
        equal
          ~msg:"a component merely starting with _build is a build directory"
          (option string) (Some "/w/p/_build_ci")
          (I.build_dir ~path:"/w/p/_build_ci/default/t.exe");
        equal ~msg:"build_dir outside any is None" (option string) None
          (I.build_dir ~path:"/w/p/src/lib");
        equal ~msg:"the data directory sits beside the contexts" string
          "/w/p/_build/_coverage"
          (I.data_dir C.format ~build_dir:"/w/p/_build");
        equal ~msg:"and outside a build directory under _windtrap" string
          "/w/p/_windtrap/coverage"
          (I.standalone_data_dir C.format ~root:"/w/p");
        equal ~msg:"build_root is the parent of the topmost _build component"
          (option string) (Some "/w/p")
          (I.build_root ~path:"/w/p/_build/default/test");
        equal ~msg:"build_root sees through the sandbox to the same root"
          (option string) (Some "/w/p")
          (I.build_root ~path:"/w/p/_build/.sandbox/0abc12/default/test");
        equal ~msg:"the topmost _build wins over planted inner ones"
          (option string) (Some "/w/p")
          (I.build_root ~path:"/w/p/_build/.sandbox/_build/_coverage");
        equal ~msg:"build_root outside _build is None" (option string) None
          (I.build_root ~path:"/w/p/src/lib");
        equal
          ~msg:"a relative path resolves against the current directory first"
          (option string)
          (I.build_root ~path:(Filename.concat (Sys.getcwd ()) "src/lib"))
          (I.build_root ~path:"src/lib");
        equal ~msg:"exe_identity is the path below _build" string
          "default/test/a.exe"
          (I.exe_identity ~exe:"/w/p/_build/default/test/a.exe");
        equal ~msg:"exe_identity strips the sandbox prefix" string
          "default/test/a.exe"
          (I.exe_identity ~exe:"/w/p/_build/.sandbox/0abc12/default/test/a.exe");
        equal ~msg:"exe_identity outside _build is the absolute path" string
          "/opt/tools/mytool.exe"
          (I.exe_identity ~exe:"/opt/tools/mytool.exe");
        equal ~msg:"the identity is what output_dir hashes" string
          (C.output_dir ~exe:"/w/p/_build/default/test/a.exe")
          (C.output_dir ~exe:"/w/p/_build/.sandbox/9f/default/test/a.exe"));
    (* One executable is one dump. Dune spells the same binary
       [runner_main.exe] in one rule and [./runner_main.exe] in another,
       and a suite that spawns a sibling names it [../../../bin/main.exe];
       a key that kept the spelling would file a dump per spelling and
       leave every one but the last for the report to call stale. *)
    test "one executable is one identity, however it is spelled" (fun () ->
        let direct = "/w/p/_build/default/test/a.exe" in
        equal ~msg:"a . component is not a directory" string
          "default/test/a.exe"
          (I.exe_identity ~exe:"/w/p/_build/default/test/./a.exe");
        equal ~msg:"nor is a chain of them" string "default/test/a.exe"
          (I.exe_identity ~exe:"/w/p/./_build/./default/test/a.exe");
        equal ~msg:"a .. is the directory above it" string "default/test/a.exe"
          (I.exe_identity ~exe:"/w/p/_build/default/test/sub/../a.exe");
        equal ~msg:"a doubled separator is one" string "default/test/a.exe"
          (I.exe_identity ~exe:"/w/p/_build/default//test/a.exe");
        List.iter
          (fun spelling ->
            equal
              ~msg:("every spelling shares the direct run's file: " ^ spelling)
              string (C.output_dir ~exe:direct)
              (C.output_dir ~exe:spelling))
          [
            "/w/p/_build/default/test/./a.exe";
            "/w/p/_build/default/test/sub/../a.exe";
            "/w/p/_build/default/test/a.exe";
          ];
        equal ~msg:"the rule reaches outside _build too" string
          "/opt/tools/mytool.exe"
          (I.exe_identity ~exe:"/opt/tools/bin/./../mytool.exe");
        equal ~msg:".. above the root stops at the root" string
          "/opt/mytool.exe"
          (I.exe_identity ~exe:"/../../opt/mytool.exe"));
  ]

(* Extent -> line derivation *)

let three_lines = "line one\nline two\nline three\n"
(* offsets: line 1 = 0-8 (newline at 8), line 2 = 9-17, line 3 = 18-27 *)

let line_tests =
  [
    test "extents derive their line numbers" (fun () ->
        let lines ~msg expected extents =
          equal ~msg (list int) expected
            (C.lines_of_extents ~source:three_lines extents)
        in
        lines ~msg:"an extent spanning the file marks every line" [ 1; 2; 3 ]
          [ pt 0 28 ];
        lines ~msg:"an exact line extent marks only its line" [ 2 ] [ pt 9 17 ];
        lines ~msg:"an extent ending on a line's newline stays on that line"
          [ 2 ]
          [ pt 9 18 ];
        lines ~msg:"an extent crossing a newline marks both lines" [ 2; 3 ]
          [ pt 9 19 ];
        lines ~msg:"two extents on one line mark it once" [ 1 ]
          [ pt 0 4; pt 5 8 ];
        lines ~msg:"an empty extent marks the line containing it" [ 2 ]
          [ pt 9 9 ];
        lines ~msg:"offsets past the end clamp to the last line" [ 3 ]
          [ pt 100 200 ];
        lines ~msg:"nested uncovered extents are the outer extent's lines"
          [ 1; 2; 3 ]
          [ pt 0 28; pt 9 17 ];
        lines ~msg:"no extents mark no lines" [] [];
        equal ~msg:"an empty source has no lines" (list int) []
          (C.lines_of_extents ~source:"" [ pt 0 5 ]);
        equal ~msg:"a source without a trailing newline keeps its last line"
          (list int) [ 2 ]
          (C.lines_of_extents ~source:"a\nb" [ pt 2 3 ]));
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
        equal ~msg:"a file-spanning extent marks every line of a huge file" int
          n (List.length lines));
  ]

(* Summaries *)

let summary_tests =
  [
    test "aggregate summary sums files" (fun () ->
        equal ~msg:"aggregate summary sums files" summary
          { C.visited = 2; total = 3 }
          (C.summary (ab ())));
  ]

(* Reports against real sources *)

let write_source root path contents =
  let path = Filename.concat root path in
  let rec mkdir_p dir =
    if not (Sys.file_exists dir) then begin
      mkdir_p (Filename.dirname dir);
      Sys.mkdir dir 0o755
    end
  in
  mkdir_p (Filename.dirname path);
  Out_channel.with_open_bin path (fun oc -> output_string oc contents)

let report_tests =
  [
    test "reports resolve sources; inner extents override outer" (fun () ->
        (* A visited outer block containing an unvisited inner block:
           only the inner extent is reported (the inner-overrides-outer
           rendering rule). *)
        let root = temp_dir () in
        write_source root "lib/eval.ml" three_lines;
        let t =
          ok "report add"
            (C.add C.empty ~file:"lib/eval.ml"
               ~points:[| pt 0 28; pt 9 17 |]
               ~counts:[| 1; 0 |])
        in
        (match C.file_reports ~source_roots:[ root ] t with
        | [ r ] ->
            equal ~msg:"report names the source file" string "lib/eval.ml"
              r.C.file;
            equal ~msg:"report resolves the source under the given root"
              (option string) (Some three_lines) r.C.source;
            is_false ~msg:"a matching source is not stale" r.C.stale;
            equal ~msg:"only the unvisited inner extent is uncovered"
              (list point)
              [ pt 9 17 ]
              r.C.uncovered_extents;
            equal ~msg:"uncovered lines are the inner block's, not the outer's"
              (list int) [ 2 ] r.C.uncovered_lines;
            equal
              ~msg:
                "line hits are the fewest visits of any point touching the line"
              (list (pair int int))
              [ (1, 1); (2, 0); (3, 1) ]
              r.C.line_hits;
            equal ~msg:"report summary counts blocks" summary
              { C.visited = 1; total = 2 }
              r.C.summary
        | reports ->
            failf "one file yields one report, got %d" (List.length reports));
        (* Reports stay useful without sources: extents survive, lines
           are empty. *)
        match C.file_reports ~source_roots:[ root ] (ab ()) with
        | [ ra; rb ] ->
            equal ~msg:"reports are ordered by file name" string "lib/a.ml"
              ra.C.file;
            equal ~msg:"files are the names, in the same order" (list string)
              [ "lib/a.ml"; "lib/b.ml" ]
              (C.files (ab ()));
            is_none ~msg:"a missing source is reported as absent" rb.C.source;
            is_false ~msg:"a missing source is not stale" rb.C.stale;
            equal ~msg:"extents are available without the source" (list point)
              [ pt 30 40 ]
              rb.C.uncovered_extents;
            equal ~msg:"no source means no line numbers" (list int) []
              rb.C.uncovered_lines;
            equal ~msg:"nor line hits" (list (pair int int)) [] rb.C.line_hits
        | reports ->
            failf "two files yield two reports, got %d" (List.length reports));
    test "stale sources are reported loudly" (fun () ->
        (* Stale data: the source shrank since the run, so its extents
           reach past the end. The report must say so loudly instead of
           painting whatever now sits at those lines. *)
        let root = temp_dir () in
        write_source root "lib/stale.ml" three_lines;
        let t =
          ok "stale add"
            (C.add C.empty ~file:"lib/stale.ml"
               ~points:[| pt 0 10; pt 20 40 |]
               ~counts:[| 1; 0 |])
        in
        (match C.file_reports ~source_roots:[ root ] t with
        | [ r ] ->
            is_true ~msg:"a source shorter than the extents is stale" r.C.stale;
            is_none ~msg:"a stale report withholds the source" r.C.source;
            equal ~msg:"a stale report paints no lines" (list int) []
              r.C.uncovered_lines;
            equal ~msg:"a stale report keeps its extents" (list point)
              [ pt 20 40 ]
              r.C.uncovered_extents;
            equal ~msg:"a stale report keeps its summary" summary
              { C.visited = 1; total = 2 }
              r.C.summary
        | reports ->
            failf "stale file yields one report, got %d" (List.length reports));
        (* An extent ending exactly at the last byte is consistent. *)
        let t =
          ok "eof add"
            (C.add C.empty ~file:"lib/stale.ml"
               ~points:[| pt 0 (String.length three_lines) |]
               ~counts:[| 0 |])
        in
        match C.file_reports ~source_roots:[ root ] t with
        | [ r ] ->
            is_false ~msg:"an extent ending at EOF is not stale" r.C.stale;
            equal ~msg:"and keeps its source" (option string) (Some three_lines)
              r.C.source;
            equal ~msg:"an extent ending at EOF paints to the last line"
              (list int) [ 1; 2; 3 ] r.C.uncovered_lines
        | reports ->
            failf "eof file yields one report, got %d" (List.length reports));
  ]

(* The at_exit dump, end to end *)

let child_expected counts =
  Printf.sprintf
    "windtrap-coverage-v3\n1\n12 lib/child.ml\n3\n0 5 %d\n6 9 %d\n10 20 %d\n"
    counts.(0) counts.(1) counts.(2)

let child_exe = Filename.concat exe_dir "dump_child.exe"

let dump_tests =
  [
    test "the at_exit dump works end to end" (fun () ->
        let child_file = Filename.concat (temp_dir ()) "child.coverage" in
        let run ?(dump = child_file) mode =
          Child.run ~env:[ ("WINDTRAP_COVERAGE_FILE", dump) ] child_exe [ mode ]
        in
        equal ~msg:"child run exits 0" int 0 (Child.exit_code (run "first"));
        (match C.load child_file with
        | Ok (t, exe) ->
            equal ~msg:"the at_exit dump round-trips through load" string
              (child_expected [| 1; 0; 0 |])
              (C.to_string t);
            (* The dump records its writer: the child's identity under
               this checkout's _build (sandbox-stripped, so it agrees
               with the parent's spelling of the same path) and its
               content digest. *)
            is_true ~msg:"the dump records the child's executable identity"
              (exe
              = Some
                  {
                    C.exe = I.exe_identity ~exe:child_exe;
                    digest = Digest.to_hex (Digest.file child_exe);
                  })
        | Error e -> failf "the child's dump does not load: %a" C.pp_error e);
        equal ~msg:"child re-run exits 0" int 0 (Child.exit_code (run "second"));
        (match C.load child_file with
        | Ok (t, _) ->
            equal ~msg:"a re-run overwrites, never accumulates" string
              (child_expected [| 1; 2; 0 |])
              (C.to_string t)
        | Error e -> failf "the re-run's dump does not load: %a" C.pp_error e);
        equal ~msg:"the dump leaves no temporary files behind" (list string) []
          (Sys.readdir (Filename.dirname child_file)
          |> Array.to_list
          |> List.filter (fun name -> Filename.check_suffix name ".tmp"));
        (* A child linking two incompatible instrumentations of one file
           still exits 0 (never a crash at module load), dumps the first
           table, and warns on stderr. *)
        let conflict = run "conflict" in
        equal ~msg:"conflicting child exits 0" int 0 (Child.exit_code conflict);
        (match C.load child_file with
        | Ok (t, _) ->
            equal ~msg:"a conflicting child dumps the first table" string
              (child_expected [| 1; 0; 0 |])
              (C.to_string t)
        | Error e ->
            failf "the conflicting child's dump does not load: %a" C.pp_error e);
        contains ~msg:"the conflicting child warns on stderr" ~sub:"conflicting"
          conflict.Child.err;
        contains ~msg:"and names the remedy" ~sub:"rebuild from clean"
          conflict.Child.err;
        (* A dump that cannot be written costs the run nothing but the
           line, which says what it could not write: a test executable
           prints it at exit, with nothing else naming coverage. *)
        let blocked = Filename.concat child_file "under-a-file.coverage" in
        let unwritable = run ~dump:blocked "first" in
        equal ~msg:"a child that cannot write its dump still exits 0" int 0
          (Child.exit_code unwritable);
        starts_with ~msg:"and says which file, and that it is the coverage file"
          ~affix:
            ("windtrap: warning: cannot write coverage file " ^ blocked ^ ": ")
          unwritable.Child.err;
        Sys.remove child_file;
        equal ~msg:"silent child exits 0" int 0 (Child.exit_code (run "silent"));
        is_false ~msg:"a process with no registrations writes no file"
          (Sys.file_exists child_file));
    (* The default destination: the executable's own directory, where
       every run keeps its own file. The child is copied under a scratch
       _build so that directory is scratch's, never this checkout's. The
       variable is stated empty, which reads as unset. *)
    test "runs of one executable accumulate; a rebuild's first run supersedes"
      (fun () ->
        let root = temp_dir () in
        let exe = Filename.concat root "_build/default/child.exe" in
        I.write_file exe (read_file child_exe);
        Unix.chmod exe 0o755;
        let run mode =
          Child.exit_code
            (Child.run ~env:[ ("WINDTRAP_COVERAGE_FILE", "") ] exe [ mode ])
        in
        let dir = C.output_dir ~exe in
        equal ~msg:"the directory sits under the executable's root" string
          (Filename.concat root "_build/_coverage")
          (Filename.dirname dir);
        let dumps () =
          match Sys.readdir dir with
          | exception Sys_error _ -> []
          | names ->
              Array.to_list names
              |> List.filter (fun n -> Filename.check_suffix n ".coverage")
              |> List.sort compare
        in
        equal ~msg:"first run exits 0" int 0 (run "first");
        equal ~msg:"second run exits 0" int 0 (run "second");
        equal ~msg:"each run writes its own file" int 2 (List.length (dumps ()));
        let digest = Digest.to_hex (Digest.file exe) in
        List.iter
          (fun name ->
            starts_with ~msg:"the files are named after the writer's digest"
              ~affix:(digest ^ "-") name)
          (dumps ());
        let merged =
          List.fold_left
            (fun acc name ->
              match C.load (Filename.concat dir name) with
              | Ok (t, _) -> ok "merge of the runs" (C.merge acc t)
              | Error e -> failf "%s: %a" name C.pp_error e)
            C.empty (dumps ())
        in
        equal ~msg:"the runs add up in the merge" string
          (child_expected [| 2; 2; 0 |])
          (C.to_string merged);
        (* A predecessor's dump: named after, and recording, another
           build's digest. The next run removes it and keeps its own. *)
        let older = Digest.to_hex (Digest.string "an older build") in
        let stale = Filename.concat dir (older ^ "-000001.coverage") in
        I.write_file stale
          (C.to_string
             ~identity:{ C.exe = I.exe_identity ~exe; digest = older }
             merged);
        equal ~msg:"third run exits 0" int 0 (run "first");
        is_false
          ~msg:
            "a rebuilt executable's first run removes its predecessors' dumps"
          (Sys.file_exists stale);
        equal ~msg:"and keeps every run of its own" int 3
          (List.length (dumps ()));
        equal ~msg:"no temporary files remain" (list string) []
          (List.filter
             (fun n -> Filename.check_suffix n ".tmp")
             (Array.to_list (Sys.readdir dir))));
  ]

(* The suite *)

let () =
  exit
  @@ run "coverage"
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
