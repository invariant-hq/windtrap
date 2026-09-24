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
   end to end through a child executable. Below both formats, the
   plumbing of Windtrap_runtime.Instr that no other suite reaches: the
   build-path rule's edges, the header, the atomic writes and the
   scanner, reader by reader. Ranges and excerpt regions are
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
        contains ~msg:"an unsaturated merge still adds" ~sub:"0 5 10\n"
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
    test
      "merge names the first file of its second argument, by name, that \
       disagrees" (fun () ->
        (* [lib/a.ml] agrees, [lib/m.ml] and [lib/z.ml] disagree, and [b]
           was built in the reverse of the order of names. *)
        let of_list tables =
          List.fold_left
            (fun t (file, end_ofs) ->
              ok file (C.add t ~file ~points:[| pt 0 end_ofs |] ~counts:[| 1 |]))
            C.empty tables
        in
        let a = of_list [ ("lib/a.ml", 5); ("lib/m.ml", 5); ("lib/z.ml", 5) ]
        and b = of_list [ ("lib/z.ml", 6); ("lib/m.ml", 6); ("lib/a.ml", 5) ] in
        match C.merge a b with
        | Error (C.Point_mismatch { file }) ->
            equal ~msg:"the first disagreeing name" string "lib/m.ml" file
        | Ok _ -> fail "a conflicting merge succeeded"
        | Error e -> failf "expected Point_mismatch, got %a" C.pp_error e);
    test "filter keeps the files whose name the predicate accepts" (fun () ->
        equal ~msg:"one file kept, its counts whole" string
          "windtrap-coverage-v3\n1\n8 lib/b.ml\n2\n10 20 1\n30 40 0\n"
          (C.to_string (C.filter (String.equal "lib/b.ml") (ab ())));
        equal ~msg:"every file kept" string ab_serialized
          (C.to_string (C.filter (fun _ -> true) (ab ())));
        is_true ~msg:"no file kept is the empty collection"
          (C.is_empty (C.filter (fun _ -> false) (ab ()))));
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
    test "an unknown first line is reported cut to 64 bytes, then escaped"
      (fun () ->
        let line = "\t\"" ^ String.make 100 'x' in
        match C.of_string (line ^ "\n0\n") with
        | Error (C.Data (I.Unknown_format { header; _ })) ->
            equal ~msg:"the header" string
              (String.escaped (String.sub line 0 64))
              header
        | Ok _ -> fail "a foreign first line parsed"
        | Error e -> failf "expected Unknown_format, got %a" C.pp_error e);
    test "whitespace is a space, a tab, a CR or a LF, and never inside a name"
      (fun () ->
        let t, _ =
          ok "CR LF line ends and tabs"
            (C.of_string
               "windtrap-coverage-v3\r\n1\r\n8 lib/a.ml\r\n1\r\n0\t5\t7\r\n")
        in
        equal ~msg:"CR LF line ends and tabs separate as spaces do" string
          "windtrap-coverage-v3\n1\n8 lib/a.ml\n1\n0 5 7\n" (C.to_string t);
        let odd = "lib/a\tb \r.ml" in
        let t =
          ok "odd name" (C.add C.empty ~file:odd ~points:[||] ~counts:[||])
        in
        equal ~msg:"a name holding whitespace keeps it" (list string) [ odd ]
          (C.files (fst (ok "odd round trip" (C.of_string (C.to_string t)))));
        match C.of_string "windtrap-coverage-v3\n1\n8\tlib/a.ml\n0\n" with
        | Error (C.Data (I.Corrupt _)) -> ()
        | Ok _ -> fail "a tab before a name parsed"
        | Error e -> failf "expected Corrupt, got %a" C.pp_error e);
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
        equal ~msg:"build_root takes a private build directory as one"
          (option string) (Some "/w/p")
          (I.build_root ~path:"/w/p/_build_ci/default/test/t.exe");
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
    test "every backslash is a separator, on every platform" (fun () ->
        equal ~msg:"a Windows spelling has the identity of the / one" string
          "default/test/a.exe"
          (I.exe_identity ~exe:"/w/p\\_build\\default\\test\\a.exe");
        equal ~msg:"and the same build directory" (option string)
          (Some "/w/p/_build")
          (I.build_dir ~path:"/w/p\\_build\\default");
        equal ~msg:"a name holding one names another path" string
          "default/a/b.exe"
          (I.exe_identity ~exe:"/w/_build/default/a\\b.exe"));
    test "a path that ends at its build directory has the empty identity"
      (fun () ->
        equal ~msg:"the build directory itself" string ""
          (I.exe_identity ~exe:"/w/p/_build");
        equal ~msg:"with a trailing separator" string ""
          (I.exe_identity ~exe:"/w/p/_build/"));
    test "a file is named by the MD5 of its executable's identity" (fun () ->
        let md5 s = Digest.to_hex (Digest.string s) in
        equal ~msg:"below a build directory" string
          ("/w/p/_build/_coverage/windtrap-" ^ md5 "default/test/a.exe"
         ^ ".coverage")
          (I.output_file C.format ~exe:"/w/p/_build/default/test/a.exe");
        equal ~msg:"the directory is the file without its extension" string
          ("/w/p/_build/_coverage/windtrap-" ^ md5 "default/test/a.exe")
          (I.output_dir C.format ~exe:"/w/p/_build/default/test/a.exe");
        equal ~msg:"below none, the hash of the absolute path" string
          (Filename.concat (Sys.getcwd ())
             ("_windtrap/coverage/windtrap-" ^ md5 "/opt/t.exe" ^ ".coverage"))
          (I.output_file C.format ~exe:"/opt/t.exe"));
    test "what needs an unreadable current directory raises Sys_error"
      (fun () ->
        let gone = Filename.concat (temp_dir ()) "gone" in
        Sys.mkdir gone 0o755;
        chdir gone;
        Sys.rmdir gone;
        let needs_cwd ~msg f = raises_match ~msg Exn.sys_error f in
        needs_cwd ~msg:"a relative path made absolute" (fun () ->
            I.absolute "a.exe");
        needs_cwd ~msg:"the build directory of a relative path" (fun () ->
            I.build_dir ~path:"_build/default");
        needs_cwd ~msg:"the identity of a relative executable" (fun () ->
            I.exe_identity ~exe:"_build/default/a.exe");
        needs_cwd ~msg:"the file of an executable below no build directory"
          (fun () -> I.output_file C.format ~exe:"/opt/t.exe");
        equal ~msg:"an absolute path below a build directory needs none" string
          "/w/_build/_coverage"
          (Filename.dirname
             (I.output_file C.format ~exe:"/w/_build/default/a.exe")));
  ]

(* The shared file plumbing and the scanner, below the two formats *)

let md5_of_bytes = String.make 32 'a'

(* [parse_error ~msg ~names f] asserts that [f] raises [Parse_error] with
   a reason that holds [names]. *)
let parse_error ~msg ~names f =
  match f () with
  | _ -> failf "%s: no Parse_error" msg
  | exception I.Parse_error reason -> contains ~msg ~sub:names reason

let cursor s =
  match I.start C.format ~path:"<test>" s with
  | Ok c -> c
  | Error e -> failf "start refused %S: %a" s (I.pp_error C.format) e

let plumbing_tests =
  [
    test "add_header refuses a malformed identity after writing the magic line"
      (fun () ->
        List.iter
          (fun identity ->
            let buffer = Buffer.create 64 in
            (match I.add_header C.format buffer (Some identity) with
            | () -> fail "a malformed identity was written"
            | exception Invalid_argument message ->
                starts_with ~msg:"the message is prefixed by the format's owner"
                  ~affix:(C.format.I.who ^ ": ") message);
            equal ~msg:"the magic line is already in the buffer" string
              "windtrap-coverage-v3\n" (Buffer.contents buffer))
          [
            { I.exe = ""; digest = md5_of_bytes };
            { I.exe = "a.exe"; digest = "abc" };
          ]);
    test "write_file that cannot rename leaves no temporary file" (fun () ->
        let dir = temp_dir () in
        let target = Filename.concat dir "target" in
        Sys.mkdir target 0o755;
        raises_match ~msg:"a directory in the way is a Sys_error" Exn.sys_error
          (fun () -> I.write_file target "data");
        equal ~msg:"and nothing but it remains" (list string) [ "target" ]
          (Array.to_list (Sys.readdir dir)));
    test "write_new_file names a new file by six hexadecimal digits" (fun () ->
        let dir = Filename.concat (temp_dir ()) "made/on/demand" in
        let first = I.write_new_file dir ~prefix:"run-" ~ext:"coverage" "one" in
        let second =
          I.write_new_file dir ~prefix:"run-" ~ext:"coverage" "two"
        in
        List.iter
          (fun path ->
            let name = Filename.basename path in
            equal ~msg:"in the directory, created on demand" string dir
              (Filename.dirname path);
            equal ~msg:"prefix, six digits, extension" int
              (String.length "run-" + 6 + String.length ".coverage")
              (String.length name);
            starts_with ~msg:"the prefix" ~affix:"run-" name;
            ends_with ~msg:"the extension" ~affix:".coverage" name;
            is_true ~msg:"six lowercase hexadecimal digits"
              (String.for_all
                 (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
                 (String.sub name 4 6)))
          [ first; second ];
        not_equal ~msg:"every call is a new file" string first second;
        equal ~msg:"each holds its own data" (pair string string) ("one", "two")
          (read_file first, read_file second);
        equal ~msg:"and nothing else is left" int 2
          (Array.length (Sys.readdir dir));
        let blocked = Filename.concat (temp_file ()) "under-a-file" in
        raises_match ~msg:"a directory that cannot be made is a Sys_error"
          Exn.sys_error (fun () ->
            I.write_new_file blocked ~prefix:"" ~ext:"coverage" "data"));
  ]

let scanner_tests =
  [
    test "start stands after the magic, before whitespace or the end" (fun () ->
        I.finish (cursor "windtrap-coverage-v3");
        equal ~msg:"the magic, then whitespace" int 7
          (I.read_nat (cursor "windtrap-coverage-v3\t7") "n");
        match I.start C.format ~path:"f.coverage" "windtrap-coverage-v30 1" with
        | Error (I.Unknown_format { path; header }) ->
            equal ~msg:"another format, named by its path" (pair string string)
              ("f.coverage", "windtrap-coverage-v30 1")
              (path, header)
        | Error e ->
            failf "expected Unknown_format, got %a" (I.pp_error C.format) e
        | Ok _ -> fail "a longer magic started");
    test "read_nat reads a decimal natural after whitespace" (fun () ->
        let c = cursor "windtrap-coverage-v3 \t\r\n 42 7" in
        equal ~msg:"after every kind of whitespace" (pair int int) (42, 7)
          (let a = I.read_nat c "a" in
           (a, I.read_nat c "b"));
        parse_error ~msg:"a negative number" ~names:"negative size" (fun () ->
            I.read_nat (cursor "windtrap-coverage-v3 -1") "size");
        parse_error ~msg:"no number" ~names:"expected size" (fun () ->
            I.read_nat (cursor "windtrap-coverage-v3 x") "size");
        parse_error ~msg:"none at the end" ~names:"expected size" (fun () ->
            I.read_nat (cursor "windtrap-coverage-v3 ") "size");
        parse_error ~msg:"a number past max_int" ~names:"invalid size"
          (fun () ->
            I.read_nat
              (cursor "windtrap-coverage-v3 99999999999999999999")
              "size"));
    test "read_count is bounded by the whole input" (fun () ->
        (* 23 bytes in all, the part already read included. *)
        let input n = "windtrap-coverage-v3 " ^ string_of_int n in
        equal ~msg:"the length of the input is a count" int 23
          (I.read_count (cursor (input 23)) "records");
        parse_error ~msg:"one more is not" ~names:"records exceeds data"
          (fun () -> I.read_count (cursor (input 24)) "records"));
    test "read_name reads a length, one space and that many bytes" (fun () ->
        let c = cursor "windtrap-coverage-v3 5 a b\tc 0 " in
        equal ~msg:"whitespace inside a name is its own" string "a b\tc"
          (I.read_name c "file");
        equal ~msg:"an empty name" string "" (I.read_name c "file");
        parse_error ~msg:"another whitespace before the bytes"
          ~names:"expected space before file" (fun () ->
            I.read_name (cursor "windtrap-coverage-v3 3\tabc") "file");
        parse_error ~msg:"fewer bytes than the length" ~names:"truncated file"
          (fun () -> I.read_name (cursor "windtrap-coverage-v3 9 abc") "file");
        parse_error ~msg:"no length" ~names:"expected file length" (fun () ->
            I.read_name (cursor "windtrap-coverage-v3 abc") "file"));
    test "read_word reads the bytes up to the next whitespace" (fun () ->
        let c = cursor "windtrap-coverage-v3  killed\tsurvived" in
        equal ~msg:"two words" (pair string string) ("killed", "survived")
          (let a = I.read_word c "verdict" in
           (a, I.read_word c "verdict"));
        parse_error ~msg:"at the end of the input" ~names:"expected verdict"
          (fun () -> I.read_word c "verdict"));
    test "read_identity reads the line that starts with exe, or nothing"
      (fun () ->
        let digest = md5_of_bytes in
        equal ~msg:"an identity whose path holds a space"
          (option (pair string string))
          (Some ("a b.exe", digest))
          (Option.map
             (fun (i : I.identity) -> (i.I.exe, i.I.digest))
             (I.read_identity
                (cursor ("windtrap-coverage-v3\nexe " ^ digest ^ " 7 a b.exe"))));
        let c = cursor "windtrap-coverage-v3 \n 3 lib" in
        is_none ~msg:"no identity" (I.read_identity c);
        equal ~msg:"and the cursor is past the whitespace only" int 3
          (I.read_nat c "count");
        parse_error ~msg:"a short digest" ~names:"digest" (fun () ->
            I.read_identity (cursor "windtrap-coverage-v3\nexe abc 1 a"));
        parse_error ~msg:"an empty path" ~names:"empty executable identity"
          (fun () ->
            I.read_identity
              (cursor ("windtrap-coverage-v3\nexe " ^ digest ^ " 0 ")));
        parse_error ~msg:"a truncated path" ~names:"truncated" (fun () ->
            I.read_identity
              (cursor ("windtrap-coverage-v3\nexe " ^ digest ^ " 9 a"))));
    test "finish accepts trailing whitespace and nothing else" (fun () ->
        I.finish (cursor "windtrap-coverage-v3 \t\r\n");
        parse_error ~msg:"a trailing byte" ~names:"trailing data" (fun () ->
            I.finish (cursor "windtrap-coverage-v3\nx")));
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
        (* One file-spanning extent over 20 000 lines marks each of them,
           once and in order. *)
        let n = 20_000 in
        let buf = Buffer.create (n * 8) in
        for i = 1 to n do
          Printf.bprintf buf "line %d\n" i
        done;
        let source = Buffer.contents buf in
        let lines =
          C.lines_of_extents ~source [ pt 0 (String.length source) ]
        in
        equal ~msg:"a file-spanning extent marks every line of a huge file"
          (list int) (List.init n succ) lines);
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
    test
      "a source is looked up by its recorded name, then under each root in \
       order, and the first readable one wins, stale or not" (fun () ->
        let report ~roots t =
          match C.file_reports ~source_roots:roots t with
          | [ r ] -> r
          | reports -> failf "one file, %d reports" (List.length reports)
        in
        let short = temp_dir () and whole = temp_dir () in
        write_source short "lib/order.ml" "short\n";
        write_source whole "lib/order.ml" three_lines;
        let t =
          ok "order add"
            (C.add C.empty ~file:"lib/order.ml"
               ~points:[| pt 0 20 |]
               ~counts:[| 0 |])
        in
        is_true ~msg:"the first root's copy is taken, though it is stale"
          (report ~roots:[ short; whole ] t).C.stale;
        equal ~msg:"in the other order the whole copy is" (option string)
          (Some three_lines) (report ~roots:[ whole; short ] t).C.source;
        equal ~msg:"a root holding no copy is passed over" (option string)
          (Some three_lines) (report ~roots:[ temp_dir (); whole ] t).C.source;
        (* An absolute recorded name, found as it is before any root is
           tried, where a root holds a stale copy under the same name. *)
        let recorded = Filename.concat (temp_dir ()) "recorded.ml" in
        write_source "/" recorded three_lines;
        write_source short recorded "short\n";
        let t =
          ok "recorded add"
            (C.add C.empty ~file:recorded ~points:[| pt 0 20 |] ~counts:[| 0 |])
        in
        equal ~msg:"the recorded name comes before every root" (option string)
          (Some three_lines) (report ~roots:[ short ] t).C.source);
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
    test
      "the destination is fixed at the first registration, a relative path \
       against the directory of that moment" (fun () ->
        let first = temp_dir () and later = temp_dir () in
        let r =
          Child.run ~cwd:first
            ~env:[ ("WINDTRAP_COVERAGE_FILE", "rel.coverage") ]
            child_exe [ "moved"; later ]
        in
        equal ~msg:"the child exits 0" int 0 (Child.exit_code r);
        (match C.load (Filename.concat first "rel.coverage") with
        | Ok (t, _) ->
            equal ~msg:"the dump is where the first registration put it" string
              (child_expected [| 1; 0; 0 |])
              (C.to_string t)
        | Error e -> failf "no dump where it was resolved: %a" C.pp_error e);
        equal ~msg:"a later move and a later variable change nothing"
          (list string) []
          (Array.to_list (Sys.readdir later)));
    test "a forked child that leaves through exit dumps too" (fun () ->
        (* Under the variable the two share one path, and the parent,
           which waits for the child, is the last writer. *)
        let dump = Filename.concat (temp_dir ()) "fork.coverage" in
        let r =
          Child.run
            ~env:[ ("WINDTRAP_COVERAGE_FILE", dump) ]
            child_exe [ "fork" ]
        in
        equal ~msg:"the parent exits 0" int 0 (Child.exit_code r);
        (match C.load dump with
        | Ok (t, _) ->
            equal ~msg:"the last to exit wins, and nothing is merged" string
              (child_expected [| 1; 0; 1 |])
              (C.to_string t)
        | Error e -> failf "the dump does not load: %a" C.pp_error e);
        (* Under the executable's own directory each keeps a file, and the
           counts from before the fork are in both. *)
        let root = temp_dir () in
        let exe = Filename.concat root "_build/default/child.exe" in
        I.write_file exe (read_file child_exe);
        Unix.chmod exe 0o755;
        let r =
          Child.run ~env:[ ("WINDTRAP_COVERAGE_FILE", "") ] exe [ "fork" ]
        in
        equal ~msg:"the copy exits 0" int 0 (Child.exit_code r);
        let dir = C.output_dir ~exe in
        let dumps =
          List.filter
            (fun n -> Filename.check_suffix n ".coverage")
            (Array.to_list (Sys.readdir dir))
        in
        equal ~msg:"two files, one per process" int 2 (List.length dumps);
        let merged =
          List.fold_left
            (fun acc name ->
              match C.load (Filename.concat dir name) with
              | Ok (t, _) -> ok "merge of the two" (C.merge acc t)
              | Error e -> failf "%s: %a" name C.pp_error e)
            C.empty dumps
        in
        equal ~msg:"the counts before the fork add up twice" string
          (child_expected [| 2; 1; 1 |])
          (C.to_string merged));
    test
      "a first registration that needs an unreadable current directory warns \
       and writes no dump" (fun () ->
        (* The child removes its directory, with the copy of itself in it,
           before it registers. A relative path needs the directory; an
           absolute one does not, and then only the identity is missing,
           since the executable cannot be read back at exit. *)
        let run dump =
          let parent = temp_dir () in
          let dir = Filename.concat parent "here" in
          Sys.mkdir dir 0o755;
          I.write_file
            (Filename.concat dir "dump_child.exe")
            (read_file child_exe);
          Unix.chmod (Filename.concat dir "dump_child.exe") 0o755;
          let r =
            Child.run ~cwd:dir
              ~env:[ ("WINDTRAP_COVERAGE_FILE", dump) ]
              "./dump_child.exe" [ "cwd-gone" ]
          in
          equal ~msg:"the child exits 0" int 0 (Child.exit_code r);
          is_false ~msg:"its directory is gone" (Sys.file_exists dir);
          r.Child.err
        in
        let err = run "rel.coverage" in
        starts_with ~msg:"a relative path: a warning"
          ~affix:
            "windtrap: warning: cannot determine the coverage output file: "
          err;
        equal ~msg:"on one line, and nothing else" int 1
          (List.length (String.split_on_char '\n' (String.trim err)));
        let dump = Filename.concat (temp_dir ()) "abs.coverage" in
        equal ~msg:"an absolute path needs no directory: no warning" text ""
          (run dump);
        match C.load dump with
        | Ok (t, identity) ->
            equal ~msg:"the dump is written" string
              (child_expected [| 1; 0; 0 |])
              (C.to_string t);
            is_none ~msg:"without the identity of an executable now gone"
              identity
        | Error e -> failf "no dump at the absolute path: %a" C.pp_error e);
    test
      "an executable named _build* below no build directory dies at exit on \
       its empty identity" (fun () ->
        let dir = temp_dir () in
        let exe = Filename.concat dir "_build_child.exe" in
        I.write_file exe (read_file child_exe);
        Unix.chmod exe 0o755;
        equal ~msg:"its identity is empty" string "" (I.exe_identity ~exe);
        let dump = Filename.concat dir "never.coverage" in
        let r =
          Child.run ~env:[ ("WINDTRAP_COVERAGE_FILE", dump) ] exe [ "first" ]
        in
        equal ~msg:"the process ends on the exception" int 2 (Child.exit_code r);
        contains ~msg:"which is to_string's Invalid_argument"
          ~sub:
            "Invalid_argument(\"Windtrap_runtime.Coverage: empty identity \
             exe\")"
          r.Child.err;
        is_false ~msg:"and nothing is written" (Sys.file_exists dump));
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
         group "files" plumbing_tests;
         group "scanner" scanner_tests;
         group "lines" line_tests;
         group "summaries" summary_tests;
         group "reports" report_tests;
         group "dump" dump_tests;
       ]
