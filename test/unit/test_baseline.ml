(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Baseline: the registry law (one content per key per run) over
   both keys, the three modes, the gating of corrections by [settle],
   corrected-file versus in-place writing, and the build-copy placement.
   Each test builds its own registry over a throwaway project root and
   drives [B.check] directly; the CI refusal is the runner's and is pinned
   in test_run.ml. *)

open Windtrap
open Windtrap.Private
module B = Baseline

let registered = ref []
let reg name body = registered := Windtrap.test name body :: !registered
let check name cond = is_true ~msg:name cond
let check_string name ~expected ~actual = equal ~msg:name string expected actual
let check_int name ~expected ~actual = equal ~msg:name int expected actual

(* Filesystem scaffolding *)

let with_root f = f (temp_dir ())

let write_raw path contents =
  Os.mkdir_p (Filename.dirname path);
  Out_channel.with_open_bin path (fun oc ->
      Out_channel.output_string oc contents)

let read_raw path = In_channel.with_open_bin path In_channel.input_all
let exists = Sys.file_exists

(* Failure-catching helpers *)

let expect_failure label f =
  match f () with
  | () ->
      check (label ^ ": raises Check_failure") false;
      None
  | exception Failure.Check_failure fl -> (
      match fl.Failure.kind with
      | Failure.Baseline { baseline; state } -> Some (baseline, state)
      | _ ->
          check (label ^ ": kind is Baseline") false;
          None)

let expect_pass label f =
  match f () with
  | () -> check (label ^ ": passes") true
  | exception Failure.Check_failure _ ->
      check (label ^ ": passes (got Check_failure)") false

let help = "test/help.expected"
let file_help = B.File help

(* A source file holding one flexible literal, and its subject: the
   position [__POS_OF__] records starts at its own token. *)
let source = "let () =\n  expect (f ()) @@ __POS_OF__ {| old |}\n"
let literal_pos = ("test/t.ml", 2, 19, 40)

(* Keys are positions: a second literal in a test needs its own line. *)
let literal ?(line = 2) ?(exact = false) value =
  B.Literal { pos = ("test/t.ml", line, 19, 40); value; exact }

(* File baselines *)

let () =
  reg "file: a missing baseline is read-only in Check mode" @@ fun () ->
  with_root @@ fun root ->
  let t = B.create ~root ~cwd:root ~mode:B.Check () in
  (match
     expect_failure "missing" (fun () -> B.check t file_help "hello\r\nworld")
   with
  | Some (Failure.File path, Failure.Missing { proposed }) ->
      check_string "the failure names the path as given" ~expected:help
        ~actual:path;
      check_string "the proposal is canonical" ~expected:"hello\nworld\n"
        ~actual:proposed
  | _ -> check "missing: File/Missing payload" false);
  check "nothing was created" (not (exists (Filename.concat root "test")));
  B.write t;
  check "Check mode writes nothing" (B.writes t = [] && B.refusals t = [])

let () =
  reg "file: both sides canonicalize" @@ fun () ->
  with_root @@ fun root ->
  write_raw (Filename.concat root help) "hello\r\nworld";
  let t = B.create ~root ~cwd:root ~mode:B.Check () in
  expect_pass "CRLF baseline vs LF actual" (fun () ->
      B.check t file_help "hello\nworld\n");
  expect_pass "actual without a final newline" (fun () ->
      B.check t file_help "hello\nworld");
  check_string "the file is untouched" ~expected:"hello\r\nworld"
    ~actual:(read_raw (Filename.concat root help))

let () =
  reg "file: mismatch carries both canonical texts" @@ fun () ->
  with_root @@ fun root ->
  write_raw (Filename.concat root help) "hello\n";
  let t = B.create ~root ~cwd:root ~mode:B.Check () in
  match expect_failure "mismatch" (fun () -> B.check t file_help "bye") with
  | Some (_, Failure.Mismatch { expected; actual }) ->
      check_string "expected is the canonical baseline" ~expected:"hello\n"
        ~actual:expected;
      check_string "actual is canonical" ~expected:"bye\n" ~actual
  | _ -> check "mismatch: Mismatch payload" false

let () =
  reg "file: the baseline is read once per run" @@ fun () ->
  with_root @@ fun root ->
  let path = Filename.concat root help in
  write_raw path "one\n";
  let t = B.create ~root ~cwd:root ~mode:B.Check () in
  expect_pass "first check reads the file" (fun () -> B.check t file_help "one");
  write_raw path "two\n";
  expect_pass "a recheck compares against the same content" (fun () ->
      B.check t file_help "one");
  match expect_failure "divergence" (fun () -> B.check t file_help "two") with
  | Some (_, Failure.Mismatch { expected; _ }) ->
      check_string "against the first-read baseline" ~expected:"one\n"
        ~actual:expected
  | _ -> check "divergence is a Mismatch" false

(* Literal baselines *)

let () =
  reg "literal: flexible and exact comparison" @@ fun () ->
  with_root @@ fun root ->
  let t = B.create ~root ~cwd:root ~mode:B.Check () in
  expect_pass "flexible: indentation and blank edges are free" (fun () ->
      B.check t (literal "\n    a\n      b\n  ") "a\n  b\n");
  (match
     expect_failure "flexible mismatch" (fun () ->
         B.check t (literal "\n    a\n      b\n  ") "a\nb")
   with
  | Some (Failure.Literal, Failure.Mismatch { expected; actual }) ->
      check_string "both sides in normalized form" ~expected:"a\n  b"
        ~actual:expected;
      check_string "the produced text, normalized" ~expected:"a\nb" ~actual
  | _ -> check "flexible mismatch: Literal/Mismatch payload" false);
  expect_pass "exact: byte for byte" (fun () ->
      B.check t (literal ~line:3 ~exact:true " a ") " a ");
  match
    expect_failure "exact mismatch" (fun () ->
        B.check t (literal ~line:3 ~exact:true " a ") "a")
  with
  | Some (Failure.Literal, Failure.Mismatch { expected; actual }) ->
      check "exact keeps the bytes" (expected = " a " && actual = "a")
  | _ -> check "exact mismatch: Literal/Mismatch payload" false

(* Unresolvable paths *)

let () =
  reg "a path that escapes the root is unresolvable in every mode" @@ fun () ->
  with_root @@ fun root ->
  List.iter
    (fun mode ->
      let t = B.create ~root ~cwd:root ~mode () in
      (match
         expect_failure "escaping file" (fun () ->
             B.check t (B.File "../x") "v")
       with
      | Some (Failure.File "../x", Failure.Unresolvable { candidate }) ->
          check "the candidate is reported" (candidate <> "")
      | _ -> check "escaping file: Unresolvable" false);
      (match
         expect_failure "escaping literal" (fun () ->
             B.check t
               (B.Literal
                  { pos = ("../t.ml", 1, 0, 0); value = "v"; exact = true })
               "w")
       with
      | Some (Failure.Literal, Failure.Unresolvable _) ->
          check "a literal outside the root cannot be corrected" true
      | _ -> check "escaping literal: Unresolvable" false);
      B.write t;
      check "nothing written" (B.writes t = [] && B.refusals t = []))
    [ B.Check; B.Corrected; B.Update ]

(* Corrected mode *)

let () =
  reg "corrected: the check fails, the correction lands beside the file"
  @@ fun () ->
  with_root @@ fun root ->
  let t = B.create ~root ~cwd:root ~mode:B.Corrected () in
  (match expect_failure "missing" (fun () -> B.check t file_help "hi") with
  | Some (_, Failure.Missing _) -> check "missing still fails" true
  | _ -> check "missing still fails" false);
  check_int "settle keeps one correction" ~expected:1
    ~actual:(B.settle t ~keep:true);
  B.write t;
  let corrected = Filename.concat root (help ^ ".corrected") in
  check_string "the .corrected holds the canonical content" ~expected:"hi\n"
    ~actual:(read_raw corrected);
  check "the file itself is not created"
    (not (exists (Filename.concat root help)));
  check "the write is reported"
    (B.writes t = [ { B.path = corrected; literals = 0 } ] && B.refusals t = [])

let () =
  reg "the registry law: one content per key per run" @@ fun () ->
  with_root @@ fun root ->
  let t = B.create ~root ~cwd:root ~mode:B.Corrected () in
  ignore (expect_failure "first" (fun () -> B.check t file_help "hi"));
  expect_pass "the same content again passes" (fun () ->
      B.check t file_help "hi");
  (match
     expect_failure "another content" (fun () -> B.check t file_help "yo")
   with
  | Some (_, Failure.Mismatch { expected; actual }) ->
      check "the mismatch is against the accepted content"
        (expected = "hi\n" && actual = "yo\n")
  | _ -> check "another content: Mismatch" false);
  check_int "only the first check recorded a correction" ~expected:1
    ~actual:(B.settle t ~keep:true)

let () =
  reg "gating: a dropped correction unaccepts its key" @@ fun () ->
  with_root @@ fun root ->
  let t = B.create ~root ~cwd:root ~mode:B.Corrected () in
  ignore (expect_failure "first" (fun () -> B.check t file_help "hi"));
  check_int "settle without keep drops it" ~expected:0
    ~actual:(B.settle t ~keep:false);
  (match expect_failure "next test" (fun () -> B.check t file_help "yo") with
  | Some (_, Failure.Missing { proposed }) ->
      check_string "the next test records its own content" ~expected:"yo\n"
        ~actual:proposed
  | _ -> check "next test: Missing again, not a Mismatch" false);
  check_int "and that one is kept" ~expected:1 ~actual:(B.settle t ~keep:true);
  B.write t;
  check_string "the kept content is what is written" ~expected:"yo\n"
    ~actual:(read_raw (Filename.concat root (help ^ ".corrected")))

(* Update mode *)

let () =
  reg "update: the check accepts silently and writes in place" @@ fun () ->
  with_root @@ fun root ->
  let t = B.create ~root ~cwd:root ~mode:B.Update () in
  expect_pass "a missing file is accepted" (fun () ->
      B.check t file_help "hello\r\nworld");
  (match
     expect_failure "another content" (fun () -> B.check t file_help "x")
   with
  | Some (_, Failure.Mismatch { expected; _ }) ->
      check_string "never last-write-wins" ~expected:"hello\nworld\n"
        ~actual:expected
  | _ -> check "another content: Mismatch" false);
  ignore (B.settle t ~keep:true);
  check "nothing is written before write"
    (not (exists (Filename.concat root help)));
  B.write t;
  check_string "the file holds the canonical content" ~expected:"hello\nworld\n"
    ~actual:(read_raw (Filename.concat root help));
  check "no .corrected beside it"
    (not (exists (Filename.concat root (help ^ ".corrected"))));
  check "the write is reported"
    (B.writes t = [ { B.path = Filename.concat root help; literals = 0 } ])

let () =
  reg "update: an equal baseline writes nothing" @@ fun () ->
  with_root @@ fun root ->
  write_raw (Filename.concat root help) "keep\r\n";
  let t = B.create ~root ~cwd:root ~mode:B.Update () in
  expect_pass "equal after canonicalization" (fun () ->
      B.check t file_help "keep");
  ignore (B.settle t ~keep:true);
  B.write t;
  check "no write recorded" (B.writes t = []);
  check_string "bytes untouched on disk" ~expected:"keep\r\n"
    ~actual:(read_raw (Filename.concat root help))

(* Literal corrections *)

let () =
  reg "literal correction: corrected file beside the source" @@ fun () ->
  with_root @@ fun root ->
  write_raw (Filename.concat root "test/t.ml") source;
  let t = B.create ~root ~cwd:root ~mode:B.Corrected () in
  ignore
    (expect_failure "mismatch" (fun () -> B.check t (literal " old ") "new"));
  ignore (B.settle t ~keep:true);
  B.write t;
  let corrected = Filename.concat root "test/t.ml.corrected" in
  check_string "the literal is rewritten in the copy"
    ~expected:"let () =\n  expect (f ()) @@ __POS_OF__ {| new |}\n"
    ~actual:(read_raw corrected);
  check_string "the source is untouched" ~expected:source
    ~actual:(read_raw (Filename.concat root "test/t.ml"));
  check "one literal reported"
    (B.writes t = [ { B.path = corrected; literals = 1 } ])

let () =
  reg "literal correction: in place under Update, several per file" @@ fun () ->
  with_root @@ fun root ->
  let two =
    "let () =\n\
    \  expect a @@ __POS_OF__ {| x |};\n\
    \  expect b @@ __POS_OF__ {| y |}\n"
  in
  write_raw (Filename.concat root "test/t.ml") two;
  let t = B.create ~root ~cwd:root ~mode:B.Update () in
  let lit line value =
    B.Literal { pos = ("test/t.ml", line, 14, 0); value; exact = false }
  in
  expect_pass "first literal accepted" (fun () -> B.check t (lit 2 " x ") "one");
  expect_pass "second literal accepted" (fun () ->
      B.check t (lit 3 " y ") "two");
  ignore (B.settle t ~keep:true);
  B.write t;
  check_string "both literals rewritten in the file"
    ~expected:
      "let () =\n\
      \  expect a @@ __POS_OF__ {| one |};\n\
      \  expect b @@ __POS_OF__ {| two |}\n"
    ~actual:(read_raw (Filename.concat root "test/t.ml"));
  check "two literals in one write"
    (B.writes t
    = [ { B.path = Filename.concat root "test/t.ml"; literals = 2 } ])

let () =
  reg "literal correction: a drifted source is refused" @@ fun () ->
  with_root @@ fun root ->
  let edited = "let () =\n  expect (f ()) @@ __POS_OF__ {| edited |}\n" in
  write_raw (Filename.concat root "test/t.ml") edited;
  let t = B.create ~root ~cwd:root ~mode:B.Update () in
  expect_pass "the check compares with the compiled literal" (fun () ->
      B.check t (literal " old ") "new");
  ignore (B.settle t ~keep:true);
  B.write t;
  check "nothing written" (B.writes t = []);
  (match B.refusals t with
  | [ (path, reason) ] ->
      check "the refusal names the file"
        (path = Filename.concat root "test/t.ml");
      check "and the reason"
        (Text.contains_substring ~pattern:"rebuild and rerun" reason)
  | _ -> check "one refusal" false);
  check_string "the file is left alone" ~expected:edited
    ~actual:(read_raw (Filename.concat root "test/t.ml"))

(* Build-copy placement *)

let () =
  reg "a build action reads and corrects dune's copy" @@ fun () ->
  with_root @@ fun root ->
  let build = Filename.concat root "_build/default" in
  let cwd = Filename.concat build "test" in
  write_raw (Filename.concat build help) "copy\n";
  let t = B.create ~root ~cwd ~mode:B.Corrected () in
  expect_pass "the copy is the baseline read" (fun () ->
      B.check t file_help "copy");
  check "the source-tree file need not exist"
    (not (exists (Filename.concat root help)));
  ignore
    (expect_failure "a mismatch" (fun () ->
         B.check t (B.File "test/other.expected") "x"));
  ignore (B.settle t ~keep:true);
  B.write t;
  let corrected = Filename.concat build "test/other.expected.corrected" in
  check ".corrected lands beside the copy" (exists corrected);
  check "and nowhere near the source tree"
    (not (exists (Filename.concat root "test/other.expected.corrected")));
  (* Sandboxed actions run under _build/.sandbox/<hash>/<context>/. *)
  let sandbox = Filename.concat root "_build/.sandbox/3f/default" in
  write_raw (Filename.concat sandbox help) "sandboxed\n";
  let t =
    B.create ~root ~cwd:(Filename.concat sandbox "test") ~mode:B.Check ()
  in
  expect_pass "the sandbox copy is the baseline read" (fun () ->
      B.check t file_help "sandboxed")

let () =
  reg "update inside a build action rewrites the source, not the copy"
  @@ fun () ->
  with_root @@ fun root ->
  let build = Filename.concat root "_build/default" in
  write_raw (Filename.concat build "test/t.ml") source;
  write_raw (Filename.concat root "test/t.ml") source;
  let t =
    B.create ~root ~cwd:(Filename.concat build "test") ~mode:B.Update ()
  in
  expect_pass "accepted" (fun () -> B.check t (literal " old ") "new");
  ignore (B.settle t ~keep:true);
  B.write t;
  check "the source is rewritten"
    (Text.contains_substring ~pattern:"{| new |}"
       (read_raw (Filename.concat root "test/t.ml")));
  check_string "the copy is untouched" ~expected:source
    ~actual:(read_raw (Filename.concat build "test/t.ml"))

let () =
  reg "a build context of another root is not this run's build action"
  @@ fun () ->
  with_root @@ fun root ->
  with_root @@ fun other ->
  write_raw (Filename.concat root help) "source\n";
  write_raw (Filename.concat other ("_build/default/" ^ help)) "elsewhere\n";
  let t =
    B.create ~root
      ~cwd:(Filename.concat other "_build/default/test")
      ~mode:B.Check ()
  in
  expect_pass "the file under the root is read" (fun () ->
      B.check t file_help "source")

let tests = List.rev !registered
let () = exit @@ Windtrap.run "baseline" tests
