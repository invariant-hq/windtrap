(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Ppx_runtime: the ordinary-OCaml half of ppx_windtrap. The PPX
   is not involved — node tables and locations are hand-built against
   synthetic source text, exactly as generated code would build them, and
   expect tests execute in-process through Runner.execute with a synthetic
   config. Covers the normalization matrix, registration/partitions,
   per-node reachability (including the 2+0 attack), ppx_expect-shaped
   correction goldens, the corrections round-trip on a real temp file, and
   the inline exit-code matrix.

   Synthetic sources carry an [@END] marker where the test body ends; it
   stands for the source text that would follow in a real file, so the
   recorded locations stay honest. Sources and goldens are spelled as
   [{x|…|x}] literals so payload delimiters appear verbatim.

   Discipline: no check may run inside a captured test body — its output
   would be captured and hidden. Bodies collect values into refs; checks
   run after Runner.execute returns. *)

open Windtrap.Private
open Harness

let () = init "ppx_runtime"

(* ───── Temp roots and config ───── *)

let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Array.iter
        (fun name -> remove_tree (Filename.concat path name))
        (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let with_temp_root f =
  let path = Filename.temp_file "windtrap-ppxrt-" ".dir" in
  Unix.unlink path;
  Unix.mkdir path 0o700;
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> f path)

let base_config ~log_dir () =
  { (Run.default_config ()) with Run.seed = 0x5eedL; log_dir }

(* ───── Location helpers over synthetic source text ───── *)

let find ?(from = 0) source pattern =
  let n = String.length pattern and h = String.length source in
  let rec loop i =
    if i + n > h then
      invalid_arg (Printf.sprintf "test helper: %S not found" pattern)
    else if String.sub source i n = pattern then i
    else loop (i + 1)
  in
  loop from

let mk_loc source start stop =
  let line = ref 1 and bol = ref 0 in
  String.iteri
    (fun i c ->
      if i < start && c = '\n' then begin
        incr line;
        bol := i + 1
      end)
    source;
  {
    Ppx_runtime.line = !line;
    start_bol = !bol;
    start_pos = start;
    end_pos = stop;
  }

(* [node_of source ~id ~node_text ?payload ()] builds a node from the (first)
   occurrence of [node_text] in [source]; [payload] is [(literal, contents,
   delimiter)] with [literal] spelled exactly as inside [node_text]. *)
let node_of source ~id ?(kind = Ppx_runtime.Expect) ~node_text ?payload () =
  let start = find source node_text in
  let stop = start + String.length node_text in
  let payload =
    Option.map
      (fun (literal, contents, delimiter) ->
        let pstart = find ~from:start source literal in
        {
          Ppx_runtime.contents;
          delimiter;
          loc = mk_loc source pstart (pstart + String.length literal);
        })
      payload
  in
  { Ppx_runtime.id; kind; loc = mk_loc source start stop; payload }

(* [(body_loc, trailing_loc)]: the body spans from [let%expect_test] to the
   [@END] marker; the trailing point is zero-width at the marker. *)
let body_locs source =
  let start = find source "let%expect_test" in
  let stop = find source "@END" in
  (mk_loc source start stop, mk_loc source stop stop)

(* ───── Scenario driver ───── *)

let file = "scratch_ppx.ml"
let test_path name = [ "Scratch_ppx"; name ]

type scenario = {
  outcome : Failure.outcome option; (* the expect test's recorded outcome *)
  exit_code : int; (* [inline_exit_code] of the run *)
  corrected : string option; (* [corrected_source] over [source] *)
}

let run_scenario ?(sanitize = fun s -> s) ?(run = fun f -> f ())
    ?(tweak_config = fun c -> c) ~source ~nodes ~name body =
  Ppx_runtime.reset ();
  let body_loc, trailing_loc = body_locs source in
  Ppx_runtime.add_expect_test ~file ~loc:body_loc ~tags:[] ~run ~sanitize ~nodes
    ~body_loc ~trailing_loc name body;
  let tests = Ppx_runtime.collect () in
  with_temp_root (fun log_dir ->
      let config = tweak_config (base_config ~log_dir ()) in
      match Runner.execute ~config ~suite:"ppxrt" tests with
      | Error error ->
          Printf.printf "startup error: %s\n%!" (Runner.startup_message error);
          { outcome = None; exit_code = -1; corrected = None }
      | Ok run_outcome ->
          let outcome =
            List.find_map
              (fun r ->
                if r.Run.path = test_path name then Some r.Run.outcome else None)
              (Run.results run_outcome.Runner.run)
          in
          {
            outcome;
            exit_code = Ppx_runtime.inline_exit_code run_outcome;
            corrected = Ppx_runtime.corrected_source ~file ~source;
          })

let failure_list = function
  | Some (Failure.Fail fs) -> fs
  | Some Failure.Pass | Some (Failure.Skip _) | None -> []

let is_pass = function Some Failure.Pass -> true | _ -> false

(* ───── Normalization ───── *)

let () =
  let n = Ppx_runtime.normalize in
  check_string "normalize: identity on plain text" ~expected:"a\nb"
    ~actual:(n "a\nb");
  check_string "normalize: rstrips every line" ~expected:"a\nb"
    ~actual:(n "a  \nb\t");
  check_string "normalize: drops blank edges" ~expected:"x"
    ~actual:(n "\n\n  x\n\n");
  check_string "normalize: dedents to min indent" ~expected:"a\n  b"
    ~actual:(n "  a\n    b");
  check_string "normalize: keeps interior blank lines" ~expected:"a\n\nb"
    ~actual:(n "  a\n   \n  b");
  check_string "normalize: empty is empty" ~expected:"" ~actual:(n "");
  check_string "normalize: whitespace-only is empty" ~expected:""
    ~actual:(n " \n\t\n ");
  check_string "normalize: CRLF becomes LF" ~expected:"a\nb"
    ~actual:(n "a\r\nb\r\n");
  check_string "normalize: typical payload equals flat output"
    ~expected:(n "INT 1\nPLUS\n")
    ~actual:(n "\n    INT 1\n    PLUS\n  ")

(* ───── Registration, grouping, partitions ───── *)

let zero_loc =
  { Ppx_runtime.line = 1; start_bol = 0; start_pos = 0; end_pos = 0 }

let () =
  Ppx_runtime.reset ();
  let nop () = () in
  Ppx_runtime.add_test ~file:"dir/a_file.ml" ~loc:zero_loc ~tags:[] "t1" nop;
  Ppx_runtime.add_test ~file:"dir/b_file.ml" ~loc:zero_loc ~tags:[] "t2" nop;
  Ppx_runtime.add_test ~file:"dir/a_file.ml" ~loc:zero_loc ~tags:[] "t3" nop;
  Ppx_runtime.enter_group ~file:"dir/a_file.ml" ~tags:[ "grouped" ] "G";
  Ppx_runtime.add_test ~file:"dir/a_file.ml" ~loc:zero_loc ~tags:[] "t4" nop;
  Ppx_runtime.leave_group ();
  let paths =
    List.map
      (fun case -> Test_tree.path_to_string case.Test_tree.path)
      (Test_tree.flatten (Ppx_runtime.collect ()))
  in
  check "collect groups per file module, first-registration order"
    (paths = [ "A_file › t1"; "A_file › t3"; "A_file › G › t4"; "B_file › t2" ]);
  check "collect drains the registry" (Ppx_runtime.collect () = []);
  check_string "partitions are file basenames, sorted"
    ~expected:"a_file.ml,b_file.ml"
    ~actual:(String.concat "," (Ppx_runtime.partitions ()));
  (match Ppx_runtime.leave_group () with
  | () -> check "leave_group without a group raises" false
  | exception Invalid_argument _ ->
      check "leave_group without a group raises" true);
  (* An unclosed group is a collect-time error. *)
  Ppx_runtime.reset ();
  Ppx_runtime.enter_group ~file:"dir/a_file.ml" ~tags:[] "open";
  match Ppx_runtime.collect () with
  | _ -> check "collect with an open group raises" false
  | exception Invalid_argument _ ->
      check "collect with an open group raises" true

let () =
  (* Partition filtering: set by init, applied at collect. *)
  Ppx_runtime.reset ();
  Ppx_runtime.init
    [|
      "runner";
      "inline-test-runner";
      "mylib";
      "-partition";
      "a_file.ml";
      "-source-tree-root";
      "../..";
      "-diff-cmd";
      "-";
    |];
  let nop () = () in
  Ppx_runtime.add_test ~file:"dir/a_file.ml" ~loc:zero_loc ~tags:[] "t1" nop;
  Ppx_runtime.add_test ~file:"dir/b_file.ml" ~loc:zero_loc ~tags:[] "t2" nop;
  let paths =
    List.map
      (fun case -> Test_tree.path_to_string case.Test_tree.path)
      (Test_tree.flatten (Ppx_runtime.collect ()))
  in
  check "partition filter keeps only the named file's tests"
    (paths = [ "A_file › t1" ]);
  (* init parses once; later vectors are ignored. *)
  Ppx_runtime.init [| "runner"; "-partition"; "b_file.ml" |];
  Ppx_runtime.add_test ~file:"dir/a_file.ml" ~loc:zero_loc ~tags:[] "t3" nop;
  Ppx_runtime.add_test ~file:"dir/b_file.ml" ~loc:zero_loc ~tags:[] "t4" nop;
  let paths =
    List.map
      (fun case -> Test_tree.path_to_string case.Test_tree.path)
      (Test_tree.flatten (Ppx_runtime.collect ()))
  in
  check "init is once-only" (paths = [ "A_file › t3" ])

(* ───── Expect matching ───── *)

let () =
  (* A typical indented payload matches flat output; already-formatted
     payloads produce no correction (no churn on first promote). *)
  let source =
    {x|let%expect_test "t" =
  print_string "INT 1\nPLUS\n";
  [%expect {|
    INT 1
    PLUS
  |}]@END
|x}
  in
  let payload_literal = "{|\n    INT 1\n    PLUS\n  |}" in
  let nodes =
    [
      node_of source ~id:0
        ~node_text:("[%expect " ^ payload_literal ^ "]")
        ~payload:
          (payload_literal, "\n    INT 1\n    PLUS\n  ", Ppx_runtime.Tag "")
        ();
    ]
  in
  let r =
    run_scenario ~source ~nodes ~name:"t" (fun () ->
        print_string "INT 1\nPLUS\n";
        Ppx_runtime.expect ~id:0)
  in
  check "matching payload passes" (is_pass r.outcome);
  check "matching payload records no correction" (r.corrected = None);
  check_int "matching run exits 0" ~expected:0 ~actual:r.exit_code

let () =
  (* Bare [%expect] with no output passes; [%expect_exact] matches raw. *)
  let source =
    {x|let%expect_test "t" =
  print_string "a\n  b";
  [%expect_exact {|a
  b|}];
  [%expect]@END
|x}
  in
  let nodes =
    [
      node_of source ~id:0 ~kind:Ppx_runtime.Expect_exact
        ~node_text:"[%expect_exact {|a\n  b|}]"
        ~payload:("{|a\n  b|}", "a\n  b", Ppx_runtime.Tag "")
        ();
      node_of source ~id:1 ~node_text:"[%expect]" ();
    ]
  in
  let r =
    run_scenario ~source ~nodes ~name:"t" (fun () ->
        print_string "a\n  b";
        Ppx_runtime.expect ~id:0;
        Ppx_runtime.expect ~id:1)
  in
  check "exact match and empty bare node pass" (is_pass r.outcome);
  check "no corrections on the exact/bare pass" (r.corrected = None)

let () =
  (* Exact is byte-for-byte: trailing whitespace that normalize would drop
     fails [%expect_exact], and the correction splices the raw bytes. *)
  let source =
    {x|let%expect_test "t" =
  print_string "a ";
  [%expect_exact {|a|}]@END
|x}
  in
  let nodes =
    [
      node_of source ~id:0 ~kind:Ppx_runtime.Expect_exact
        ~node_text:"[%expect_exact {|a|}]"
        ~payload:("{|a|}", "a", Ppx_runtime.Tag "")
        ();
    ]
  in
  let r =
    run_scenario ~source ~nodes ~name:"t" (fun () ->
        print_string "a ";
        Ppx_runtime.expect ~id:0)
  in
  check "expect_exact is byte-for-byte" (not (is_pass r.outcome));
  check "expect_exact correction splices the raw bytes"
    (match r.corrected with
    | Some corrected -> contains "[%expect_exact {|a |}]" corrected
    | None -> false)

(* ───── Correction goldens (mechanism (c): ppx_expect re-indentation) ───── *)

let () =
  (* Multi-line: contents at node column + 2, closing delimiter at node
     column — the exact shape ppx_expect writes. One run corrects every
     stale node (mismatches do not abort the body). *)
  let source =
    {x|let%expect_test "t" =
  print_string "INT 1\nPLUS\nINT 2\n";
  [%expect {|
    INT 1
    PLUS
  |}];
  print_string "done\n";
  [%expect {| old |}]@END
|x}
  in
  let p0 = "{|\n    INT 1\n    PLUS\n  |}" in
  let nodes =
    [
      node_of source ~id:0
        ~node_text:("[%expect " ^ p0 ^ "]")
        ~payload:(p0, "\n    INT 1\n    PLUS\n  ", Ppx_runtime.Tag "")
        ();
      node_of source ~id:1 ~node_text:"[%expect {| old |}]"
        ~payload:("{| old |}", " old ", Ppx_runtime.Tag "")
        ();
    ]
  in
  let r =
    run_scenario ~source ~nodes ~name:"t" (fun () ->
        print_string "INT 1\nPLUS\nINT 2\n";
        Ppx_runtime.expect ~id:0;
        print_string "done\n";
        Ppx_runtime.expect ~id:1)
  in
  (* The pinned ppx_expect corpus goldens write multi-line payloads with
     the extension head on its own line and contents and the closing
     delimiter both at node column + 2 (see the corrected shapes in
     upstream ppx_expect's test/negative-tests/*.corrected.expected at
     the pinned conformance commit, test/conformance/NOTICE). *)
  let golden =
    {x|let%expect_test "t" =
  print_string "INT 1\nPLUS\nINT 2\n";
  [%expect
    {|
    INT 1
    PLUS
    INT 2
    |}];
  print_string "done\n";
  [%expect {| done |}]@END
|x}
  in
  (match r.corrected with
  | Some corrected ->
      check_string
        "multi-line + single-line corrections re-indent like ppx_expect"
        ~expected:golden ~actual:corrected
  | None -> check "corrections were recorded" false);
  check "stale payloads fail the test"
    (List.exists
       (fun f ->
         match f.Failure.kind with Failure.Equality _ -> true | _ -> false)
       (failure_list r.outcome));
  check_int "expect-mismatch-only run exits 0 (promotion protocol)" ~expected:0
    ~actual:r.exit_code

let () =
  (* Bare node with output: the whole node is rewritten. *)
  let source =
    {x|let%expect_test "t" =
  print_string "hi\n";
  [%expect]@END
|x}
  in
  let nodes = [ node_of source ~id:0 ~node_text:"[%expect]" () ] in
  let r =
    run_scenario ~source ~nodes ~name:"t" (fun () ->
        print_string "hi\n";
        Ppx_runtime.expect ~id:0)
  in
  let golden =
    {x|let%expect_test "t" =
  print_string "hi\n";
  [%expect {| hi |}]@END
|x}
  in
  (match r.corrected with
  | Some corrected ->
      check_string "bare [%expect] is rewritten whole" ~expected:golden
        ~actual:corrected
  | None -> check "bare-node correction recorded" false);
  check_int "bare-node mismatch exits 0" ~expected:0 ~actual:r.exit_code

let () =
  (* Quote-delimited payloads stay quoted, with escaping. *)
  let source =
    {x|let%expect_test "t" =
  print_string "say \"bye\"";
  [%expect "hi"]@END
|x}
  in
  let nodes =
    [
      node_of source ~id:0 ~node_text:{|[%expect "hi"]|}
        ~payload:({|"hi"|}, "hi", Ppx_runtime.Quote)
        ();
    ]
  in
  let r =
    run_scenario ~source ~nodes ~name:"t" (fun () ->
        print_string "say \"bye\"";
        Ppx_runtime.expect ~id:0)
  in
  check "quote payload corrections stay quoted and escaped"
    (match r.corrected with
    | Some corrected -> contains {x|[%expect "say \"bye\""]|x} corrected
    | None -> false)

let () =
  (* Delimiter conflicts grow the tag. *)
  let source =
    {x|let%expect_test "t" =
  print_conflict ();
  [%expect {| old |}]@END
|x}
  in
  let nodes =
    [
      node_of source ~id:0 ~node_text:"[%expect {| old |}]"
        ~payload:("{| old |}", " old ", Ppx_runtime.Tag "")
        ();
    ]
  in
  let r =
    run_scenario ~source ~nodes ~name:"t" (fun () ->
        print_string "x |} y";
        Ppx_runtime.expect ~id:0)
  in
  check "conflicting contents re-tag the delimiter"
    (match r.corrected with
    | Some corrected -> contains "{xxx| x |} y |xxx}" corrected
    | None -> false)

let () =
  (* Re-indentation corner cases, one node each: tab-indented output (the
     legacy rule counts only leading spaces as indentation and strips tabs
     from contents), CRLF line endings (normalized to LF), and interior
     blank lines (rendered truly empty, no trailing spaces). *)
  let source =
    {x|let%expect_test "t" =
  chunk1 ();
  [%expect {| old1 |}];
  chunk2 ();
  [%expect {| old2 |}];
  chunk3 ();
  [%expect {| old3 |}]@END
|x}
  in
  let node i literal contents =
    node_of source ~id:i
      ~node_text:("[%expect " ^ literal ^ "]")
      ~payload:(literal, contents, Ppx_runtime.Tag "")
      ()
  in
  let nodes =
    [
      node 0 "{| old1 |}" " old1 ";
      node 1 "{| old2 |}" " old2 ";
      node 2 "{| old3 |}" " old3 ";
    ]
  in
  let r =
    run_scenario ~source ~nodes ~name:"t" (fun () ->
        print_string "\ta\n\tb\n";
        Ppx_runtime.expect ~id:0;
        print_string "c\r\nd\r\n";
        Ppx_runtime.expect ~id:1;
        print_string "e\n\nf\n";
        Ppx_runtime.expect ~id:2)
  in
  let golden =
    {x|let%expect_test "t" =
  chunk1 ();
  [%expect
    {|
    a
    b
    |}];
  chunk2 ();
  [%expect
    {|
    c
    d
    |}];
  chunk3 ();
  [%expect
    {|
    e

    f
    |}]@END
|x}
  in
  (match r.corrected with
  | Some corrected ->
      check_string "tabs, CRLF, and interior blanks re-indent like ppx_expect"
        ~expected:golden ~actual:corrected
  | None -> check "corner-case corrections recorded" false);
  check_int "corner-case mismatches exit 0" ~expected:0 ~actual:r.exit_code

let () =
  (* Multi-line quote-delimited correction: one leading/trailing " " line,
     one-space content indentation, every line escaped and the newlines
     escaped too, the whole payload on one source line — the corpus's
     quote shape (negative-tests/escaped_strings.ml.corrected.expected). *)
  let source = {x|let%expect_test "t" =
  lines ();
  [%expect "old"]@END
|x} in
  let nodes =
    [
      node_of source ~id:0 ~node_text:{|[%expect "old"]|}
        ~payload:({|"old"|}, "old", Ppx_runtime.Quote)
        ();
    ]
  in
  let r =
    run_scenario ~source ~nodes ~name:"t" (fun () ->
        print_string "say \"hi\"\nbye\n";
        Ppx_runtime.expect ~id:0)
  in
  let golden =
    {x|let%expect_test "t" =
  lines ();
  [%expect " \n say \"hi\"\n bye\n "]@END
|x}
  in
  match r.corrected with
  | Some corrected ->
      check_string "multi-line quote correction escapes onto one line"
        ~expected:golden ~actual:corrected
  | None -> check "multi-line quote correction recorded" false

let () =
  (* A quote-delimited correction past the 90-column margin wraps with
     line-continuation escapes at node column + 2, continuation content at
     node column + 3 — the corpus's wrapped-quote shape
     (negative-tests/normal_strings.ml.corrected.expected). *)
  let source = {x|let%expect_test "t" =
  lines ();
  [%expect "old"]@END
|x} in
  let nodes =
    [
      node_of source ~id:0 ~node_text:{|[%expect "old"]|}
        ~payload:({|"old"|}, "old", Ppx_runtime.Quote)
        ();
    ]
  in
  let block = String.make 40 'X' in
  let r =
    run_scenario ~source ~nodes ~name:"t" (fun () ->
        print_string (String.concat " " [ block; block; block ]);
        print_string "\n";
        print_string block;
        print_string "\n";
        Ppx_runtime.expect ~id:0)
  in
  let golden =
    {x|let%expect_test "t" =
  lines ();
  [%expect
    "|x} ^ " "
    ^ {x|\n\
    \ |x} ^ block ^ " " ^ block ^ {x| \
     |x} ^ block
    ^ {x|\n\
    \ |x} ^ block ^ {x|\n\
    \ "]@END
|x}
  in
  match r.corrected with
  | Some corrected ->
      check_string "overlong quote correction wraps at the margin"
        ~expected:golden ~actual:corrected
  | None -> check "wrapped quote correction recorded" false

let () =
  (* Trailing output: ";" at body end, new node at let-column + 2. *)
  let source = {x|let%expect_test "t" =
  print_string "extra"@END
|x} in
  let r =
    run_scenario ~source ~nodes:[] ~name:"t" (fun () -> print_string "extra")
  in
  let golden =
    {x|let%expect_test "t" =
  print_string "extra";
  [%expect {| extra |}]@END
|x}
  in
  (match r.corrected with
  | Some corrected ->
      check_string "trailing output inserts a node, ppx_expect-shaped"
        ~expected:golden ~actual:corrected
  | None -> check "trailing correction recorded" false);
  check "trailing output fails the test" (not (is_pass r.outcome));
  check_int "trailing-only failure exits 0" ~expected:0 ~actual:r.exit_code

let () =
  (* Multi-line trailing output: contents at let-column + 4. *)
  let source = {x|let%expect_test "t" =
  print_string "one\ntwo\n"@END
|x} in
  let r =
    run_scenario ~source ~nodes:[] ~name:"t" (fun () ->
        print_string "one\ntwo\n")
  in
  let golden =
    {x|let%expect_test "t" =
  print_string "one\ntwo\n";
  [%expect
    {|
    one
    two
    |}]@END
|x}
  in
  match r.corrected with
  | Some corrected ->
      check_string "multi-line trailing insert indents like ppx_expect"
        ~expected:golden ~actual:corrected
  | None -> check "multi-line trailing correction recorded" false

(* ───── Uncaught exceptions (design D3: never a correction) ───── *)

let () =
  (* Recipe-A shape (ppx/F-1): a tail [failwith]. No correction exists for
     the exception, so nothing can ever be spliced after the nonreturning
     statement (where the inserted node broke the build, warning 21). *)
  let source = {x|let%expect_test "t" =
  failwith "boom"@END
|x} in
  let r =
    run_scenario ~source ~nodes:[] ~name:"t" (fun () -> failwith "boom")
  in
  check "an uncaught exception records no correction" (r.corrected = None);
  check "the uncaught exception fails the test as a Raise"
    (match failure_list r.outcome with
    | [ { Failure.kind = Failure.Raise _; _ } ] -> true
    | _ -> false);
  check_int "uncaught-exn failure exits 1 (never a correction)" ~expected:1
    ~actual:r.exit_code

let () =
  (* Pending output before a raise stays out of the corrections: the
     trailing point is untouched, and the declared-but-unreached node is
     explained by the raise, not separately flagged. *)
  let source =
    {x|let%expect_test "t" =
  print_string "pre\n";
  failwith "late";
  [%expect {| never |}]@END
|x}
  in
  let nodes =
    [
      node_of source ~id:0 ~node_text:"[%expect {| never |}]"
        ~payload:("{| never |}", " never ", Ppx_runtime.Tag "")
        ();
    ]
  in
  let r =
    run_scenario ~source ~nodes ~name:"t" (fun () ->
        print_string "pre\n";
        failwith "late")
  in
  check "pending output before a raise stays out of the corrections"
    (r.corrected = None);
  check "unreached nodes behind an exception are not extra failures"
    (List.length (failure_list r.outcome) = 1);
  check_int "exn with pending output and unreached nodes exits 1" ~expected:1
    ~actual:r.exit_code

let () =
  (* A node mismatch before a raise keeps its correction; the raise adds
     none: only the reached node is rewritten, [failwith] stays the tail
     statement, and no node is inserted after it. *)
  let source =
    {x|let%expect_test "t" =
  print_string "a";
  [%expect {| wrong |}];
  print_string "b";
  failwith "boom"@END
|x}
  in
  let nodes =
    [
      node_of source ~id:0 ~node_text:"[%expect {| wrong |}]"
        ~payload:("{| wrong |}", " wrong ", Ppx_runtime.Tag "")
        ();
    ]
  in
  let r =
    run_scenario ~source ~nodes ~name:"t" (fun () ->
        print_string "a";
        Ppx_runtime.expect ~id:0;
        print_string "b";
        failwith "boom")
  in
  let golden =
    {x|let%expect_test "t" =
  print_string "a";
  [%expect {| a |}];
  print_string "b";
  failwith "boom"@END
|x}
  in
  (match r.corrected with
  | Some corrected ->
      check_string
        "a node mismatch before a raise keeps its correction; the raise adds \
         none"
        ~expected:golden ~actual:corrected
  | None -> check "reached-mismatch-before-raise correction recorded" false);
  check "the mismatch and the exception are both reported"
    (List.length (failure_list r.outcome) = 2);
  check_int "mismatch + raise exits 1" ~expected:1 ~actual:r.exit_code

let () =
  (* F-1's convergence property at the mechanism level: raising bodies
     never record inserts, so promote cycles are stable — nothing to
     accrete (recipe B), and nothing ever placed after a nonreturning
     statement (recipe A's compile break is structurally impossible). *)
  let source =
    {x|let%expect_test "t" =
  print_string "pending";
  boom ()@END
|x}
  in
  let body () =
    print_string "pending";
    if Sys.opaque_identity true then failwith "boom"
  in
  let r1 = run_scenario ~source ~nodes:[] ~name:"t" body in
  let r2 = run_scenario ~source ~nodes:[] ~name:"t" body in
  check "raising bodies never record inserts, so promote cycles are stable"
    (r1.corrected = None && r2.corrected = None);
  check_int "recipe-B shape exits 1 on every cycle" ~expected:1
    ~actual:r1.exit_code;
  check_int "recipe-B shape exits 1 on the second cycle too" ~expected:1
    ~actual:r2.exit_code

(* ───── Per-node reachability (mechanism (d)) ───── *)

let () =
  (* The 2+0 attack: one node reached twice, another never. An aggregate
     count would see 2 = 2 and pass; per-node bookkeeping must fail. *)
  let source =
    {x|let%expect_test "t" =
  for _ = 1 to 2 do
    print_string "x";
    [%expect {| x |}]
  done;
  [%expect {| never |}]@END
|x}
  in
  let nodes =
    [
      node_of source ~id:0 ~node_text:"[%expect {| x |}]"
        ~payload:("{| x |}", " x ", Ppx_runtime.Tag "")
        ();
      node_of source ~id:1 ~node_text:"[%expect {| never |}]"
        ~payload:("{| never |}", " never ", Ppx_runtime.Tag "")
        ();
    ]
  in
  let r =
    run_scenario ~source ~nodes ~name:"t" (fun () ->
        for _ = 1 to 2 do
          print_string "x";
          Ppx_runtime.expect ~id:0
        done)
  in
  check "2+0 fails: the unreached node is reported"
    (List.exists
       (fun f ->
         match f.Failure.kind with
         | Failure.Message m -> contains "never reached" m
         | _ -> false)
       (failure_list r.outcome));
  check "consistent double reach of the other node is fine"
    (List.length (failure_list r.outcome) = 1);
  check "unreached nodes record no correction" (r.corrected = None);
  check_int "unreached node exits 1 (not a correction)" ~expected:1
    ~actual:r.exit_code

let () =
  (* A node reached twice with different outputs is inconsistent: the
     correction is the CR block listing every output. *)
  let source =
    {x|let%expect_test "t" =
  chatty ();
  [%expect {| x |}]@END
|x}
  in
  let nodes =
    [
      node_of source ~id:0 ~node_text:"[%expect {| x |}]"
        ~payload:("{| x |}", " x ", Ppx_runtime.Tag "")
        ();
    ]
  in
  let r =
    run_scenario ~source ~nodes ~name:"t" (fun () ->
        print_string "x";
        Ppx_runtime.expect ~id:0;
        print_string "y";
        Ppx_runtime.expect ~id:0)
  in
  check "inconsistent reaches correct to the CR block"
    (match r.corrected with
    | Some corrected ->
        contains "CR expect_test: Test ran multiple times" corrected
        && contains "=== Output 1 / 2 ===" corrected
    | None -> false);
  check "inconsistent reaches fail the test" (not (is_pass r.outcome))

let () =
  (* A node in a loop reached twice with the same wrong output is one
     consistent result: a single ordinary correction, not the CR block —
     ppx_expect dedups reaches by their formatted result. *)
  let source =
    {x|let%expect_test "t" =
  for _ = 1 to 2 do
    print_string "x";
    [%expect {| old |}]
  done@END
|x}
  in
  let nodes =
    [
      node_of source ~id:0 ~node_text:"[%expect {| old |}]"
        ~payload:("{| old |}", " old ", Ppx_runtime.Tag "")
        ();
    ]
  in
  let r =
    run_scenario ~source ~nodes ~name:"t" (fun () ->
        for _ = 1 to 2 do
          print_string "x";
          Ppx_runtime.expect ~id:0
        done)
  in
  check "consistent repeated mismatch corrects once, without a CR block"
    (match r.corrected with
    | Some corrected ->
        contains "[%expect {| x |}]" corrected
        && not (contains "CR expect_test" corrected)
    | None -> false);
  check "consistent repeated mismatch is one failure"
    (List.length (failure_list r.outcome) = 1);
  check_int "consistent repeated mismatch exits 0" ~expected:0
    ~actual:r.exit_code

(* ───── Duplicate registrations (functor-instantiated tests) ───── *)

let () =
  (* ppx_expect runs a functor-duplicated test under one name; windtrap's
     path-uniqueness law renames later duplicates " (2)", " (3)" — in the
     scope they collide in — and runs them all. *)
  Ppx_runtime.reset ();
  let nop () = () in
  Ppx_runtime.add_test ~file:"dir/f.ml" ~loc:zero_loc ~tags:[] "dup" nop;
  Ppx_runtime.add_test ~file:"dir/f.ml" ~loc:zero_loc ~tags:[] "dup" nop;
  Ppx_runtime.add_test ~file:"dir/f.ml" ~loc:zero_loc ~tags:[] "dup" nop;
  Ppx_runtime.enter_group ~file:"dir/f.ml" ~tags:[] "G";
  Ppx_runtime.add_test ~file:"dir/f.ml" ~loc:zero_loc ~tags:[] "dup" nop;
  Ppx_runtime.leave_group ();
  Ppx_runtime.enter_group ~file:"dir/f.ml" ~tags:[] "G";
  Ppx_runtime.add_test ~file:"dir/f.ml" ~loc:zero_loc ~tags:[] "dup" nop;
  Ppx_runtime.leave_group ();
  let paths =
    List.map
      (fun case -> Test_tree.path_to_string case.Test_tree.path)
      (Test_tree.flatten (Ppx_runtime.collect ()))
  in
  check "duplicate names rename deterministically, per scope"
    (paths
    = [
        "F › dup";
        "F › dup (2)";
        "F › dup (3)";
        "F › G › dup";
        "F › G (2) › dup";
      ])

(* Registers the same expect test [times] times — the functor-instantiation
   shape: identical file, name, and node spans — with [body] told which
   instance it is. Returns (exit code, corrected). *)
let run_duplicated ~source ~nodes ~name ~times body =
  Ppx_runtime.reset ();
  let body_loc, trailing_loc = body_locs source in
  let instance = ref 0 in
  for _ = 1 to times do
    Ppx_runtime.add_expect_test ~file ~loc:body_loc ~tags:[]
      ~run:(fun f -> f ())
      ~sanitize:(fun s -> s)
      ~nodes ~body_loc ~trailing_loc name
      (fun () ->
        incr instance;
        body !instance)
  done;
  let tests = Ppx_runtime.collect () in
  with_temp_root (fun log_dir ->
      match
        Runner.execute ~config:(base_config ~log_dir ()) ~suite:"ppxrt" tests
      with
      | Error error ->
          Printf.printf "startup error: %s\n%!" (Runner.startup_message error);
          (-1, None)
      | Ok outcome ->
          ( Ppx_runtime.inline_exit_code outcome,
            Ppx_runtime.corrected_source ~file ~source ))

let dup_source =
  {x|let%expect_test "similar" =
  p ();
  [%expect {| bar |}]@END
|x}

let dup_nodes () =
  [
    node_of dup_source ~id:0 ~node_text:"[%expect {| bar |}]"
      ~payload:("{| bar |}", " bar ", Ppx_runtime.Tag "")
      ();
  ]

let () =
  (* Two instances whose outputs differ in raw bytes but format
     identically are one result: a single plain correction — ppx_expect's
     similar_distinct_outputs shape. *)
  let exit_code, corrected =
    run_duplicated ~source:dup_source ~nodes:(dup_nodes ()) ~name:"similar"
      ~times:2 (fun instance ->
        print_string (if instance = 1 then "foo" else "\n\nfoo\n\n");
        Ppx_runtime.expect ~id:0)
  in
  check "similar duplicate outputs merge into one plain correction"
    (match corrected with
    | Some corrected ->
        contains "[%expect {| foo |}]" corrected
        && not (contains "CR expect_test" corrected)
    | None -> false);
  check_int "merged duplicate corrections exit 0" ~expected:0 ~actual:exit_code

let () =
  (* Two instances with genuinely different outputs: the merged history is
     inconsistent, so the correction is the CR block listing both raws. *)
  let exit_code, corrected =
    run_duplicated ~source:dup_source ~nodes:(dup_nodes ()) ~name:"similar"
      ~times:2 (fun instance ->
        print_string (if instance = 1 then "foo" else "baz");
        Ppx_runtime.expect ~id:0)
  in
  check "distinct duplicate outputs merge into the CR block"
    (match corrected with
    | Some corrected ->
        contains "CR expect_test: Test ran multiple times" corrected
        && contains "=== Output 1 / 2 ===" corrected
        && contains "foo" corrected && contains "baz" corrected
    | None -> false);
  check_int "CR-covered duplicate corrections exit 0" ~expected:0
    ~actual:exit_code

let () =
  (* Two passing instances: no correction, exit 0 — the functor.ml shape. *)
  let exit_code, corrected =
    run_duplicated ~source:dup_source ~nodes:(dup_nodes ()) ~name:"similar"
      ~times:2 (fun _ ->
        print_string "bar";
        Ppx_runtime.expect ~id:0)
  in
  check "passing duplicates record nothing" (corrected = None);
  check_int "passing duplicates exit 0" ~expected:0 ~actual:exit_code

(* ───── The corrected-file style pass ───── *)

let () =
  (* {%expect|…|} shorthand: the correction rewrites the whole node and
     keeps the extension id; a payload containing the delimiter grows the
     tag ({%expect xxx|…|xxx}) instead of dropping [%expect]. *)
  let source = {x|let%expect_test "t" =
  p ();
  {%expect|old|}@END
|x} in
  let literal = "{%expect|old|}" in
  let start = find source literal in
  let node =
    {
      Ppx_runtime.id = 0;
      kind = Ppx_runtime.Expect;
      loc = mk_loc source start (start + String.length literal);
      payload =
        Some
          {
            Ppx_runtime.contents = "old";
            delimiter = Ppx_runtime.Tag "";
            (* Shorthand syntax: the payload literal is the whole node. *)
            loc = mk_loc source start (start + String.length literal);
          };
    }
  in
  let r =
    run_scenario ~source ~nodes:[ node ] ~name:"t" (fun () ->
        print_string "x |} y";
        Ppx_runtime.expect ~id:0)
  in
  check "shorthand retag keeps the %expect id"
    (match r.corrected with
    | Some corrected -> contains "{%expect xxx| x |} y |xxx}" corrected
    | None -> false)

let () =
  (* The style pass: a correction anywhere in the file standardizes every
     resolved node of that file — a matching node with nonstandard layout
     is rewritten (collapsed here), and a reached bare [%expect] with empty
     output materializes as [%expect {| |}]. ppx_expect's corpus goldens
     pin both (missing.ml, escaped_strings.ml). *)
  let source =
    {x|let%expect_test "t" =
  print_string "hello";
  [%expect
    {|
       hello
  |}];
  print_string "wrong";
  [%expect {| old |}];
  ignore ();
  [%expect]@END
|x}
  in
  let p0 = "{|\n       hello\n  |}" in
  let nodes =
    [
      node_of source ~id:0
        ~node_text:("[%expect\n    " ^ p0 ^ "]")
        ~payload:(p0, "\n       hello\n  ", Ppx_runtime.Tag "")
        ();
      node_of source ~id:1 ~node_text:"[%expect {| old |}]"
        ~payload:("{| old |}", " old ", Ppx_runtime.Tag "")
        ();
      node_of source ~id:2 ~node_text:"[%expect]" ();
    ]
  in
  let r =
    run_scenario ~source ~nodes ~name:"t" (fun () ->
        print_string "hello";
        Ppx_runtime.expect ~id:0;
        print_string "wrong";
        Ppx_runtime.expect ~id:1;
        Ppx_runtime.expect ~id:2)
  in
  let golden =
    {x|let%expect_test "t" =
  print_string "hello";
  [%expect {| hello |}];
  print_string "wrong";
  [%expect {| wrong |}];
  ignore ();
  [%expect {| |}]@END
|x}
  in
  (match r.corrected with
  | Some corrected ->
      check_string
        "the style pass standardizes matching and bare nodes alongside \
         corrections"
        ~expected:golden ~actual:corrected
  | None -> check "style-pass corrections recorded" false);
  check "the matching node is not a failure"
    (List.length (failure_list r.outcome) = 1);
  check_int "style-pass run exits 0" ~expected:0 ~actual:r.exit_code

let () =
  (* Without a correction in the file, matching-but-nonstandard nodes are
     left alone: match means no churn (ppx_expect's default formatting
     flexibility; the RFC's mechanism (c)). *)
  let source =
    {x|let%expect_test "t" =
  print_string "hello";
  [%expect
    {|
       hello
  |}]@END
|x}
  in
  let p0 = "{|\n       hello\n  |}" in
  let nodes =
    [
      node_of source ~id:0
        ~node_text:("[%expect\n    " ^ p0 ^ "]")
        ~payload:(p0, "\n       hello\n  ", Ppx_runtime.Tag "")
        ();
    ]
  in
  let r =
    run_scenario ~source ~nodes ~name:"t" (fun () ->
        print_string "hello";
        Ppx_runtime.expect ~id:0)
  in
  check "a matching file records no correction at all" (r.corrected = None);
  check "the matching test passes" (is_pass r.outcome)

(* ───── Control exceptions: skip, assertions, --stream ───── *)

let () =
  let source =
    {x|let%expect_test "t" =
  skip ();
  [%expect {| x |}]@END
|x}
  in
  let nodes =
    [
      node_of source ~id:0 ~node_text:"[%expect {| x |}]"
        ~payload:("{| x |}", " x ", Ppx_runtime.Tag "")
        ();
    ]
  in
  let r =
    run_scenario ~source ~nodes ~name:"t" (fun () ->
        ignore (Check.skip ~reason:"why" ()))
  in
  check "skip inside an expect test skips it"
    (match r.outcome with Some (Failure.Skip _) -> true | _ -> false);
  check "skip records no corrections" (r.corrected = None);
  check_int "a skipped expect test exits 0" ~expected:0 ~actual:r.exit_code

let () =
  (* Amendment C2, the hard half: a skip after nodes were already reached —
     one of them with a {e mismatch} — and with trailing output pending
     records nothing at all. No node correction (the mismatch dies with the
     skip), no trailing insertion, no unreached-node failure for the node
     after the skip. Were any of those recorded, the test's outcome would be
     Fail (recorded failures beat the skip in the runner), so the Skip
     outcome itself proves the machinery stayed silent. *)
  let source =
    {x|let%expect_test "t" =
  print_string "x";
  [%expect {| old |}];
  print_string "extra";
  skip ();
  [%expect {| never |}]@END
|x}
  in
  let nodes =
    [
      node_of source ~id:0 ~node_text:"[%expect {| old |}]"
        ~payload:("{| old |}", " old ", Ppx_runtime.Tag "")
        ();
      node_of source ~id:1 ~node_text:"[%expect {| never |}]"
        ~payload:("{| never |}", " never ", Ppx_runtime.Tag "")
        ();
    ]
  in
  let r =
    run_scenario ~source ~nodes ~name:"t" (fun () ->
        print_string "x";
        Ppx_runtime.expect ~id:0;
        print_string "extra";
        ignore (Check.skip ~reason:"gated" ()))
  in
  check "reached-then-skip is a skip, not a failure"
    (match r.outcome with Some (Failure.Skip _) -> true | _ -> false);
  check "reached-then-skip records no correction, mismatched reach included"
    (r.corrected = None);
  check_int "reached-then-skip exits 0" ~expected:0 ~actual:r.exit_code

(* ───── Amendment C2: skips against the promotion exit rule ───── *)

let () =
  (* A skipped expect test is invisible to the exit protocol: it neither
     forces 1 (it is not an uncovered failure) nor enables 0 (a real
     assertion failure alongside it still exits 1). *)
  let skip_source =
    {x|let%expect_test "gated" =
  skip ();
  [%expect {| never |}]@END
|x}
  in
  let register_skipped_expect name =
    let body_loc, trailing_loc = body_locs skip_source in
    let nodes =
      [
        node_of skip_source ~id:0 ~node_text:"[%expect {| never |}]"
          ~payload:("{| never |}", " never ", Ppx_runtime.Tag "")
          ();
      ]
    in
    Ppx_runtime.add_expect_test ~file ~loc:body_loc ~tags:[]
      ~run:(fun f -> f ())
      ~sanitize:(fun s -> s)
      ~nodes ~body_loc ~trailing_loc name
      (fun () -> ignore (Check.skip ~reason:"gated" ()))
  in
  (* The non-skip tests live in their own file: corrections are keyed per
     source file, and the skipped test's file must stay correction-free. *)
  let other_file = "scratch_other.ml" in
  let other_source = {x|let%expect_test "c" =
  p ()@END
|x} in
  let register_corrected_expect name =
    (* Trailing output: a covered correction, the exit-0 side of the rule. *)
    let body_loc, trailing_loc = body_locs other_source in
    Ppx_runtime.add_expect_test ~file:other_file ~loc:body_loc ~tags:[]
      ~run:(fun f -> f ())
      ~sanitize:(fun s -> s)
      ~nodes:[] ~body_loc ~trailing_loc name
      (fun () -> print_string "trailing")
  in
  let run_partition register =
    Ppx_runtime.reset ();
    register ();
    let tests = Ppx_runtime.collect () in
    with_temp_root (fun log_dir ->
        match
          Runner.execute ~config:(base_config ~log_dir ()) ~suite:"ppxrt" tests
        with
        | Error _ -> (-1, None, None)
        | Ok outcome ->
            ( Ppx_runtime.inline_exit_code outcome,
              Ppx_runtime.corrected_source ~file ~source:skip_source,
              Ppx_runtime.corrected_source ~file:other_file ~source:other_source
            ))
  in
  let exit_code, skip_corrected, other_corrected =
    run_partition (fun () ->
        register_skipped_expect "gated";
        register_corrected_expect "c")
  in
  check_int "C2: one skipped expect + one covered correction exits 0"
    ~expected:0 ~actual:exit_code;
  check "C2: the other test's correction really was recorded"
    (match other_corrected with
    | Some corrected -> contains "[%expect {| trailing |}]" corrected
    | None -> false);
  check "C2: the skipped test's nodes stay out of the corrections"
    (skip_corrected = None);
  let exit_code, _, _ =
    run_partition (fun () ->
        register_skipped_expect "gated";
        Ppx_runtime.add_test ~file:other_file ~loc:zero_loc ~tags:[] "bad"
          (fun () -> Check.is_true false))
  in
  check_int "C2: one skipped expect + one assertion failure exits 1" ~expected:1
    ~actual:exit_code;
  let exit_code, skip_corrected, _ =
    run_partition (fun () ->
        register_skipped_expect "gated-one";
        register_skipped_expect "gated-two";
        Ppx_runtime.add_test ~file:other_file ~loc:zero_loc ~tags:[]
          "plain-skip" (fun () -> ignore (Check.skip ())))
  in
  check_int "C2: an all-skipped partition exits 0" ~expected:0 ~actual:exit_code;
  check "C2: an all-skipped partition records no corrections"
    (skip_corrected = None)

let () =
  (* An assertion failure is never a correction (Law 11): exit 1 — but the
     reached mismatch's correction is still recorded and written. *)
  let source =
    {x|let%expect_test "t" =
  print_string "x";
  [%expect {| stale |}];
  is_true false@END
|x}
  in
  let nodes =
    [
      node_of source ~id:0 ~node_text:"[%expect {| stale |}]"
        ~payload:("{| stale |}", " stale ", Ppx_runtime.Tag "")
        ();
    ]
  in
  let r =
    run_scenario ~source ~nodes ~name:"t" (fun () ->
        print_string "x";
        Ppx_runtime.expect ~id:0;
        Check.is_true false)
  in
  check "assertion failure inside an expect test exits 1" (r.exit_code = 1);
  check "the reached mismatch's correction is still recorded"
    (match r.corrected with
    | Some corrected -> contains "[%expect {| x |}]" corrected
    | None -> false);
  check "both the mismatch and the assertion are reported"
    (List.length (failure_list r.outcome) = 2)

let () =
  (* A timeout propagates like every other body exception: never a
     correction (Law 11), exit 1 — while the reached mismatch's correction
     is still recorded, and nothing is inserted for the timeout itself. *)
  let source =
    {x|let%expect_test "t" =
  print_string "x";
  [%expect {| stale |}];
  hang ()@END
|x}
  in
  let nodes =
    [
      node_of source ~id:0 ~node_text:"[%expect {| stale |}]"
        ~payload:("{| stale |}", " stale ", Ppx_runtime.Tag "")
        ();
    ]
  in
  let r =
    run_scenario ~source ~nodes ~name:"t" (fun () ->
        print_string "x";
        Ppx_runtime.expect ~id:0;
        raise (Failure.Timeout 0.5))
  in
  check_int "timeout inside an expect test exits 1" ~expected:1
    ~actual:r.exit_code;
  check "the pre-timeout mismatch's correction is still recorded"
    (match r.corrected with
    | Some corrected ->
        contains "[%expect {| x |}]" corrected
        && not (contains "Timeout" corrected)
    | None -> false)

let () =
  (* Under --stream there is no captured output: the expect test fails at
     the node, not against silence, and it is not corrections-covered. *)
  let source =
    {x|let%expect_test "t" =
  print_string "x";
  [%expect {| x |}]@END
|x}
  in
  let nodes =
    [
      node_of source ~id:0 ~node_text:"[%expect {| x |}]"
        ~payload:("{| x |}", " x ", Ppx_runtime.Tag "")
        ();
    ]
  in
  let r =
    run_scenario
      ~tweak_config:(fun c -> { c with Run.stream = true })
      ~source ~nodes ~name:"t"
      (* No print: under --stream the body writes to the real descriptors,
         and this suite's transcript must stay clean. The node fails on the
         consume alone. *)
      (fun () -> Ppx_runtime.expect ~id:0)
  in
  check "--stream fails the expect test at the node"
    (List.exists
       (fun f ->
         match f.Failure.kind with
         | Failure.Message m -> contains "requires capture" m
         | _ -> false)
       (failure_list r.outcome));
  check_int "--stream expect failure exits 1" ~expected:1 ~actual:r.exit_code

(* ───── sanitize, run, and [%expect.output] ───── *)

let () =
  let sanitized = ref [] in
  let sanitize s =
    sanitized := s :: !sanitized;
    String.concat "N" (String.split_on_char '3' s)
  in
  let source =
    {x|let%expect_test "t" =
  print_string "id 3\n";
  [%expect {| id N |}]@END
|x}
  in
  let nodes =
    [
      node_of source ~id:0 ~node_text:"[%expect {| id N |}]"
        ~payload:("{| id N |}", " id N ", Ppx_runtime.Tag "")
        ();
    ]
  in
  let r =
    run_scenario ~sanitize ~source ~nodes ~name:"t" (fun () ->
        print_string "id 3\n";
        Ppx_runtime.expect ~id:0)
  in
  check "sanitize rewrites output before matching" (is_pass r.outcome);
  check "sanitize saw the raw output"
    (List.exists (fun s -> contains "id 3" s) !sanitized)

let () =
  let run_calls = ref 0 in
  let run f =
    incr run_calls;
    f ()
  in
  let source = {x|let%expect_test "t" =
  ()@END
|x} in
  let r = run_scenario ~run ~source ~nodes:[] ~name:"t" (fun () -> ()) in
  check "the config's run wraps the body exactly once" (!run_calls = 1);
  check "run-wrapped empty body passes" (is_pass r.outcome)

let () =
  let seen = ref None in
  let source =
    {x|let%expect_test "t" =
  print_string "abc";
  ignore [%expect.output]@END
|x}
  in
  let r =
    run_scenario ~source ~nodes:[] ~name:"t" (fun () ->
        print_string "abc";
        seen := Some (Ppx_runtime.expect_output ()))
  in
  check "[%expect.output] returns and consumes the output" (!seen = Some "abc");
  check "consumed output is not trailing output" (is_pass r.outcome);
  check "consumed output records no correction" (r.corrected = None)

let () =
  (* [%expect.output] interleaved with [%expect]: the consumption cursor
     advances, so the node sees only output printed after the read. *)
  let seen = ref None in
  let source =
    {x|let%expect_test "t" =
  print_string "a";
  ignore [%expect.output];
  print_string "b";
  [%expect {| b |}]@END
|x}
  in
  let nodes =
    [
      node_of source ~id:0 ~node_text:"[%expect {| b |}]"
        ~payload:("{| b |}", " b ", Ppx_runtime.Tag "")
        ();
    ]
  in
  let r =
    run_scenario ~source ~nodes ~name:"t" (fun () ->
        print_string "a";
        seen := Some (Ppx_runtime.expect_output ());
        print_string "b";
        Ppx_runtime.expect ~id:0)
  in
  check "[%expect.output] consumed only the earlier output" (!seen = Some "a");
  check "the following node sees only later output and passes"
    (is_pass r.outcome);
  check "interleaved consumption records no correction" (r.corrected = None)

let () =
  (* Outside any expect test, the node operations refuse loudly. *)
  Ppx_runtime.reset ();
  (match Ppx_runtime.expect_output () with
  | _ -> check "expect_output outside an expect test raises" false
  | exception Invalid_argument _ ->
      check "expect_output outside an expect test raises" true);
  match Ppx_runtime.expect ~id:0 with
  | () -> check "expect outside an expect test raises" false
  | exception Invalid_argument _ ->
      check "expect outside an expect test raises" true

(* ───── Exit-code matrix over whole runs ───── *)

let () =
  let exit_of ~register ~tweak_config () =
    Ppx_runtime.reset ();
    register ();
    let tests = Ppx_runtime.collect () in
    with_temp_root (fun log_dir ->
        let config = tweak_config (base_config ~log_dir ()) in
        match Runner.execute ~config ~suite:"ppxrt" tests with
        | Error _ -> -1
        | Ok outcome -> Ppx_runtime.inline_exit_code outcome)
  in
  let plain_pass () =
    Ppx_runtime.add_test ~file ~loc:zero_loc ~tags:[] "ok" (fun () -> ())
  in
  let plain_fail () =
    Ppx_runtime.add_test ~file ~loc:zero_loc ~tags:[] "bad" (fun () ->
        Check.is_true false)
  in
  check_int "matrix: all pass exits 0" ~expected:0
    ~actual:(exit_of ~register:plain_pass ~tweak_config:Fun.id ());
  check_int "matrix: let%test failure exits 1" ~expected:1
    ~actual:(exit_of ~register:plain_fail ~tweak_config:Fun.id ());
  check_int "matrix: empty selection exits 0 (empty partition, not a typo)"
    ~expected:0
    ~actual:
      (exit_of ~register:plain_pass
         ~tweak_config:(fun c -> { c with Run.filter = Some "nomatch" })
         ());
  (* Mixed: a covered expect mismatch plus a plain failure exits 1. *)
  let mixed () =
    let source = {x|let%expect_test "t" =
  p ()@END
|x} in
    let body_loc, trailing_loc = body_locs source in
    Ppx_runtime.add_expect_test ~file ~loc:body_loc ~tags:[]
      ~run:(fun f -> f ())
      ~sanitize:(fun s -> s)
      ~nodes:[] ~body_loc ~trailing_loc "t"
      (fun () -> print_string "trailing");
    plain_fail ()
  in
  check_int "matrix: covered mismatch + plain failure exits 1" ~expected:1
    ~actual:(exit_of ~register:mixed ~tweak_config:Fun.id ())

(* ───── Corrections round-trip on a real temp source file ───── *)

let () =
  Ppx_runtime.reset ();
  let source =
    {x|let%expect_test "t" =
  print_string "new\n";
  [%expect {| old |}]
|x}
  in
  let temp = Filename.temp_file "windtrap-ppxrt-src-" ".ml" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove temp with Sys_error _ -> ())
    (fun () ->
      let oc = open_out_bin temp in
      output_string oc source;
      close_out oc;
      let body_start = find source "let%expect_test" in
      let body_end = find source "|}]" + String.length "|}]" in
      let body_loc = mk_loc source body_start body_end in
      let trailing_loc = mk_loc source body_end body_end in
      let nodes =
        [
          node_of source ~id:0 ~node_text:"[%expect {| old |}]"
            ~payload:("{| old |}", " old ", Ppx_runtime.Tag "")
            ();
        ]
      in
      let start_dir = Sys.getcwd () in
      Ppx_runtime.add_expect_test ~file:temp ~loc:body_loc ~tags:[]
        ~run:(fun f -> f ())
        ~sanitize:(fun s -> s)
        ~nodes ~body_loc ~trailing_loc "t"
        (fun () ->
          print_string "new\n";
          Ppx_runtime.expect ~id:0;
          (* Tests may chdir; the runner does not restore the cwd, so
             flush_corrections must — .corrected files land in the
             module-load cwd regardless. *)
          Sys.chdir (Filename.get_temp_dir_name ()));
      let tests = Ppx_runtime.collect () in
      with_temp_root (fun log_dir ->
          match
            Runner.execute ~config:(base_config ~log_dir ()) ~suite:"ppxrt"
              tests
          with
          | Error _ -> check "round-trip run started" false
          | Ok _ -> ());
      check "the test body left the cwd changed"
        (not (String.equal (Sys.getcwd ()) start_dir));
      let written = Ppx_runtime.flush_corrections () in
      check "flush restores the module-load cwd"
        (String.equal (Sys.getcwd ()) start_dir);
      let corrected_name = Filename.basename temp ^ ".corrected" in
      check "flush writes <basename>.corrected in the start cwd"
        (written = [ corrected_name ] && Sys.file_exists corrected_name);
      let golden =
        {x|let%expect_test "t" =
  print_string "new\n";
  [%expect {| new |}]
|x}
      in
      (match
         let ic = open_in_bin corrected_name in
         Fun.protect
           ~finally:(fun () -> close_in_noerr ic)
           (fun () -> really_input_string ic (in_channel_length ic))
       with
      | corrected ->
          check_string "the .corrected round-trips the golden" ~expected:golden
            ~actual:corrected
      | exception Sys_error _ -> check "read back .corrected" false);
      (try Sys.remove corrected_name with Sys_error _ -> ());
      check "flush clears the corrections table"
        (Ppx_runtime.flush_corrections () = []))

(* ───── The ambient config module ───── *)

let () =
  (* The default config is the identity in every component the runtime
     consumes; its shape must keep include-and-override configs compiling. *)
  check_string "Expect_test_config.sanitize is the identity" ~expected:"x"
    ~actual:(Expect_test_config.sanitize "x");
  let ran = ref false in
  Expect_test_config.run (fun () -> ran := true);
  check "Expect_test_config.run applies the body" !ran;
  check "Expect_test_config.IO.return is the identity"
    (Expect_test_config.IO.return 7 = 7);
  check "upon_unreleasable_issue is `CR"
    (Expect_test_config.upon_unreleasable_issue = `CR);
  (* The shadowing pattern ppx_expect suites use must keep compiling. *)
  let module Shadow = struct
    include Expect_test_config

    let sanitize s = String.map (fun c -> if c = 'a' then 'b' else c) s
  end in
  check_string "include-and-override shadowing compiles and overrides"
    ~expected:"bb" ~actual:(Shadow.sanitize "ab");
  check "shadowed config keeps IO" (Shadow.IO.return true)

(* ───── Summary ───── *)

let () = finish ()
