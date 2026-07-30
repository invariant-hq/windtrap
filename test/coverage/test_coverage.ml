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
   report data (including stale-source rejection), excerpt regions, and
   the at_exit dump end to end through a child executable.

   A windtrap suite ([run] executes tests sequentially in declaration
   order). The registry tests accumulate state in the shared global
   registry — each uses distinctive file names and extents, but the
   dump-destination test must follow the first registration, so run the
   suite whole, not filtered. The dune action sets WINDTRAP_COVERAGE=off:
   the synthetic registrations would otherwise render a meaningless
   inline coverage line on every green run. *)

open Windtrap
module C = Windtrap_coverage

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
   _build/_coverage: point it at scratch before the first register below.
   The path is resolved at first registration, so later putenv calls (the
   child tests) do not move it. *)
let () =
  let parent_dump = scratch "parent.coverage" in
  Unix.putenv "WINDTRAP_COVERAGE_FILE" parent_dump

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
    test "dump_destination is the resolved override path" (fun () ->
        (* The dump destination is resolved at first registration — here
           the WINDTRAP_COVERAGE_FILE override set above — and readable
           in-process (the runner's sibling detection reads it at render
           time). *)
        check "dump_destination is the resolved override path"
          (C.dump_destination () = Some (scratch "parent.coverage")));
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
               (C.Unknown_format { path = "old.coverage"; header = "V1" })
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
        | Error (C.Unknown_format { header; _ }) ->
            check "v1 magic is rejected as unknown format"
              (contains "WINDTRAP-COVERAGE-1" header)
        | Ok _ | Error _ -> check "v1 magic is rejected as unknown format" false);
        (match
           C.of_string "windtrap-coverage-v2\n1\n8 lib/a.ml\n1\n0 5 1\n"
         with
        | Error (C.Unknown_format { header; _ }) ->
            check "the pre-release v2 magic is rejected as unknown format"
              (contains "windtrap-coverage-v2" header)
        | Ok _ | Error _ ->
            check "the pre-release v2 magic is rejected as unknown format" false);
        (match C.of_string "" with
        | Error (C.Unknown_format _) ->
            check "empty data is unknown format" true
        | Ok _ | Error _ -> check "empty data is unknown format" false);
        (match C.of_string "windtrap-coverage-v33\n0\n" with
        | Error (C.Unknown_format _) ->
            check "magic must be followed by whitespace" true
        | Ok _ | Error _ -> check "magic must be followed by whitespace" false);
        let corrupt name payload =
          match C.of_string payload with
          | Error (C.Corrupt _) -> check name true
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
        | Error (C.Unreadable _) ->
            check "loading a missing file is Unreadable" true
        | Ok _ | Error _ -> check "loading a missing file is Unreadable" false);
  ]

(* Deterministic output filenames, the root rule, identities *)

let filename_tests =
  [
    test "output filenames are deterministic" (fun () ->
        let direct = C.output_file ~exe:"/w/p/_build/default/test/a.exe" in
        check_string "output_file is deterministic" ~expected:direct
          ~actual:(C.output_file ~exe:"/w/p/_build/default/test/a.exe");
        check "output_file lives under the root's _build/_coverage"
          (String.starts_with ~prefix:"/w/p/_build/_coverage/windtrap-" direct);
        check "output_file has the .coverage suffix"
          (Filename.check_suffix direct ".coverage");
        check_string "sandboxed and direct runs share a file" ~expected:direct
          ~actual:
            (C.output_file ~exe:"/w/p/_build/.sandbox/0abc12/default/test/a.exe");
        check "different executables get different files"
          (direct <> C.output_file ~exe:"/w/p/_build/default/test/b.exe");
        check "different build contexts get different files"
          (direct <> C.output_file ~exe:"/w/p/_build/alt/test/a.exe");
        check "the project location does not affect the name"
          (Filename.basename direct
          = Filename.basename
              (C.output_file ~exe:"/elsewhere/_build/default/test/a.exe"));
        let outside = C.output_file ~exe:"/opt/tools/mytool.exe" in
        check "an executable outside _build dumps under the current directory"
          (String.starts_with
             ~prefix:
               (Filename.concat (Sys.getcwd ()) "_build/_coverage/windtrap-")
             outside));
    test "build_root and exe_identity follow the topmost _build" (fun () ->
        check "build_root is the parent of the topmost _build component"
          (C.build_root ~path:"/w/p/_build/default/test" = Some "/w/p");
        check "build_root sees through the sandbox to the same root"
          (C.build_root ~path:"/w/p/_build/.sandbox/0abc12/default/test"
          = Some "/w/p");
        check "the topmost _build wins over planted inner ones"
          (C.build_root ~path:"/w/p/_build/.sandbox/_build/_coverage"
          = Some "/w/p");
        check "build_root outside _build is None"
          (C.build_root ~path:"/w/p/src/lib" = None);
        check "a relative path resolves against the current directory first"
          (C.build_root ~path:"src/lib"
          = C.build_root ~path:(Filename.concat (Sys.getcwd ()) "src/lib"));
        check_string "exe_identity is the path below _build"
          ~expected:"default/test/a.exe"
          ~actual:(C.exe_identity ~exe:"/w/p/_build/default/test/a.exe");
        check_string "exe_identity strips the sandbox prefix"
          ~expected:"default/test/a.exe"
          ~actual:
            (C.exe_identity
               ~exe:"/w/p/_build/.sandbox/0abc12/default/test/a.exe");
        check_string "exe_identity outside _build is the absolute path"
          ~expected:"/opt/tools/mytool.exe"
          ~actual:(C.exe_identity ~exe:"/opt/tools/mytool.exe");
        check "the identity is what output_file hashes"
          (C.output_file ~exe:"/w/p/_build/default/test/a.exe"
          = C.output_file ~exe:"/w/p/_build/.sandbox/9f/default/test/a.exe"));
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
          ~expected:n ~actual:(List.length lines);
        check "a huge uncovered run collapses to one range"
          (C.collapse_ranges lines = [ (1, n) ]));
    test "line ranges collapse and format" (fun () ->
        check "collapse of contiguous runs"
          (C.collapse_ranges [ 1; 2; 3; 7; 8 ] = [ (1, 3); (7, 8) ]);
        check "collapse tolerates duplicates"
          (C.collapse_ranges [ 1; 1; 2; 5; 5 ] = [ (1, 2); (5, 5) ]);
        check "collapse of the empty list" (C.collapse_ranges [] = []);
        check "collapse of a singleton" (C.collapse_ranges [ 4 ] = [ (4, 4) ]);
        check_string "range formatting matches the report shape"
          ~expected:"88-94, 121"
          ~actual:(C.format_ranges [ (88, 94); (121, 121) ]);
        check_string "single-range formatting" ~expected:"1-3"
          ~actual:(C.format_ranges [ (1, 3) ]);
        check_string "empty-range formatting" ~expected:""
          ~actual:(C.format_ranges []));
  ]

(* Summaries *)

let summary_tests =
  [
    test "summaries format and style" (fun () ->
        let s = { C.visited = 312; total = 358 } in
        check_string "summary line matches the RFC transcript"
          ~expected:"87.2% (312/358 points)"
          ~actual:(Format.asprintf "%a" C.pp_summary s);
        check_string "an empty summary is 100%" ~expected:"100.0% (0/0 points)"
          ~actual:
            (Format.asprintf "%a" C.pp_summary { C.visited = 0; total = 0 });
        check "aggregate summary sums files"
          (C.summary (ab ()) = { C.visited = 2; total = 3 });
        check "80 percent is green"
          (C.style { C.visited = 8; total = 10 } = `Green);
        check "60 percent is yellow"
          (C.style { C.visited = 6; total = 10 } = `Yellow);
        check "79 percent is yellow"
          (C.style { C.visited = 79; total = 100 } = `Yellow);
        check "59 percent is red"
          (C.style { C.visited = 59; total = 100 } = `Red));
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
            check "no source means no line numbers" (rb.C.uncovered_lines = [])
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

(* Excerpt regions *)

let ten_lines =
  String.concat "" (List.init 10 (fun i -> Printf.sprintf "l%d\n" (i + 1)))

let excerpt_tests =
  [
    test "excerpt regions window their context" (fun () ->
        let numbers region = List.map (fun l -> l.C.number) region in
        let uncovered region =
          List.filter_map
            (fun l -> if l.C.uncovered then Some l.C.number else None)
            region
        in
        (match C.excerpts ~source:ten_lines [ 3; 4; 8 ] with
        | [ first; second ] ->
            check "first region spans the range plus context"
              (numbers first = [ 2; 3; 4; 5 ]);
            check "first region marks only uncovered lines"
              (uncovered first = [ 3; 4 ]);
            check "second region spans its range plus context"
              (numbers second = [ 7; 8; 9 ]);
            check "second region marks its uncovered line"
              (uncovered second = [ 8 ]);
            check "excerpt text is the source line"
              ((List.nth first 1).C.text = "l3")
        | regions ->
            check_int "separated ranges yield two regions" ~expected:2
              ~actual:(List.length regions));
        (match C.excerpts ~source:ten_lines [ 3; 6 ] with
        | [ only ] ->
            check "touching context windows merge into one region"
              (numbers only = [ 2; 3; 4; 5; 6; 7 ])
        | regions ->
            check_int "touching windows yield one region" ~expected:1
              ~actual:(List.length regions));
        (match C.excerpts ~context:0 ~source:ten_lines [ 5 ] with
        | [ [ line ] ] ->
            check "zero context keeps the bare line"
              (line.C.number = 5 && line.C.uncovered)
        | _ -> check "zero context keeps the bare line" false);
        (match C.excerpts ~source:ten_lines [ 1; 10 ] with
        | [ first; second ] ->
            check "context clamps at the top" (numbers first = [ 1; 2 ]);
            check "context clamps at the bottom" (numbers second = [ 9; 10 ])
        | _ -> check "boundary lines clamp their context" false);
        check "out-of-range lines are ignored"
          (C.excerpts ~source:ten_lines [ 0; 11; 99 ] = []);
        check "an empty source yields no excerpts"
          (C.excerpts ~source:"" [ 1 ] = []);
        match C.excerpts ~source:"a\nb\n" [ 2 ] with
        | [ region ] ->
            check "a trailing newline opens no phantom line"
              (numbers region = [ 1; 2 ] && (List.nth region 1).C.text = "b")
        | _ -> check "a trailing newline opens no phantom line" false);
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
                    C.exe = C.exe_identity ~exe:child_exe;
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
      group "excerpts" excerpt_tests;
      group "dump" dump_tests;
    ]
