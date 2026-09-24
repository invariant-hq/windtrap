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

(* Filesystem scaffolding *)

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
      is_true ~msg:(label ^ ": raises Check_failure") false;
      None
  | exception Failure.Check_failure fl -> (
      match fl.Failure.kind with
      | Failure.Baseline { baseline; state } -> Some (baseline, state)
      | _ ->
          is_true ~msg:(label ^ ": kind is Baseline") false;
          None)

let expect_pass label f =
  match f () with
  | () -> is_true ~msg:(label ^ ": passes") true
  | exception Failure.Check_failure _ ->
      is_true ~msg:(label ^ ": passes (got Check_failure)") false

let help = "test/help.expected"
let file_help = B.File help

(* A source file holding one flexible literal, and its subject: the
   position [__POS_OF__] records starts at its own token. *)
let source = "let () =\n  expect (f ()) @@ __POS_OF__ {| old |}\n"

(* Keys are positions: a second literal in a test needs its own line. *)
let literal ?(line = 2) ?(exact = false) value =
  B.Literal { pos = ("test/t.ml", line, 19, 40); value; exact }

(* File baselines *)

let () =
  reg "file: a missing baseline is read-only in Check mode" @@ fun () ->
  let root = temp_dir () in
  let t = B.create ~root ~cwd:root ~mode:B.Check () in
  (match
     expect_failure "missing" (fun () -> B.check t file_help "hello\r\nworld")
   with
  | Some (Failure.File path, Failure.Missing { proposed }) ->
      equal ~msg:"the failure names the path as given" string help path;
      equal ~msg:"the proposal is canonical" string "hello\nworld\n" proposed
  | _ -> is_true ~msg:"missing: File/Missing payload" false);
  is_true ~msg:"nothing was created"
    (not (exists (Filename.concat root "test")));
  B.write t;
  is_true ~msg:"Check mode writes nothing" (B.writes t = [])

let () =
  reg "file: both sides canonicalize" @@ fun () ->
  let root = temp_dir () in
  write_raw (Filename.concat root help) "hello\r\nworld";
  let t = B.create ~root ~cwd:root ~mode:B.Check () in
  expect_pass "CRLF baseline vs LF actual" (fun () ->
      B.check t file_help "hello\nworld\n");
  expect_pass "actual without a final newline" (fun () ->
      B.check t file_help "hello\nworld");
  equal ~msg:"the file is untouched" string "hello\r\nworld"
    (read_raw (Filename.concat root help))

let () =
  reg "file: mismatch carries both canonical texts" @@ fun () ->
  let root = temp_dir () in
  write_raw (Filename.concat root help) "hello\n";
  let t = B.create ~root ~cwd:root ~mode:B.Check () in
  match expect_failure "mismatch" (fun () -> B.check t file_help "bye") with
  | Some (_, Failure.Mismatch { expected; actual }) ->
      equal ~msg:"expected is the canonical baseline" string "hello\n" expected;
      equal ~msg:"actual is canonical" string "bye\n" actual
  | _ -> is_true ~msg:"mismatch: Mismatch payload" false

let () =
  reg "file: the baseline is read once per run" @@ fun () ->
  let root = temp_dir () in
  let path = Filename.concat root help in
  write_raw path "one\n";
  let t = B.create ~root ~cwd:root ~mode:B.Check () in
  expect_pass "first check reads the file" (fun () -> B.check t file_help "one");
  write_raw path "two\n";
  expect_pass "a recheck compares against the same content" (fun () ->
      B.check t file_help "one");
  match expect_failure "divergence" (fun () -> B.check t file_help "two") with
  | Some (_, Failure.Mismatch { expected; _ }) ->
      equal ~msg:"against the first-read baseline" string "one\n" expected
  | _ -> is_true ~msg:"divergence is a Mismatch" false

(* Literal baselines *)

let () =
  reg "literal: flexible and exact comparison" @@ fun () ->
  let root = temp_dir () in
  let t = B.create ~root ~cwd:root ~mode:B.Check () in
  expect_pass "flexible: indentation and blank edges are free" (fun () ->
      B.check t (literal "\n    a\n      b\n  ") "a\n  b\n");
  (match
     expect_failure "flexible mismatch" (fun () ->
         B.check t (literal "\n    a\n      b\n  ") "a\nb")
   with
  | Some
      (Failure.Literal { exact = false }, Failure.Mismatch { expected; actual })
    ->
      equal ~msg:"both sides in normalized form" string "a\n  b" expected;
      equal ~msg:"the produced text, normalized" string "a\nb" actual
  | _ -> is_true ~msg:"flexible mismatch: Literal/Mismatch payload" false);
  expect_pass "exact: byte for byte" (fun () ->
      B.check t (literal ~line:3 ~exact:true " a ") " a ");
  match
    expect_failure "exact mismatch" (fun () ->
        B.check t (literal ~line:3 ~exact:true " a ") "a")
  with
  | Some
      (Failure.Literal { exact = true }, Failure.Mismatch { expected; actual })
    ->
      is_true ~msg:"exact keeps the bytes" (expected = " a " && actual = "a")
  | _ -> is_true ~msg:"exact mismatch: Literal/Mismatch payload" false

(* Unresolvable paths *)

let () =
  reg "a path that escapes the root is unresolvable in every mode" @@ fun () ->
  let root = temp_dir () in
  List.iter
    (fun mode ->
      let t = B.create ~root ~cwd:root ~mode () in
      (match
         expect_failure "escaping file" (fun () ->
             B.check t (B.File "../x") "v")
       with
      | Some (Failure.File "../x", Failure.Unresolvable { candidate }) ->
          is_true ~msg:"the candidate is reported" (candidate <> "")
      | _ -> is_true ~msg:"escaping file: Unresolvable" false);
      (match
         expect_failure "escaping literal" (fun () ->
             B.check t
               (B.Literal
                  { pos = ("../t.ml", 1, 0, 0); value = "v"; exact = true })
               "w")
       with
      | Some (Failure.Literal { exact = true }, Failure.Unresolvable _) ->
          is_true ~msg:"a literal outside the root cannot be corrected" true
      | _ -> is_true ~msg:"escaping literal: Unresolvable" false);
      B.write t;
      is_true ~msg:"nothing written" (B.writes t = []))
    [ B.Check; B.Corrected; B.Update ]

(* Corrected mode *)

let () =
  reg "corrected: the check fails, the correction lands beside the file"
  @@ fun () ->
  let root = temp_dir () in
  let t = B.create ~root ~cwd:root ~mode:B.Corrected () in
  (match expect_failure "missing" (fun () -> B.check t file_help "hi") with
  | Some (_, Failure.Missing _) -> is_true ~msg:"missing still fails" true
  | _ -> is_true ~msg:"missing still fails" false);
  equal ~msg:"settle keeps one correction" int 1 (B.settle t ~keep:true);
  B.write t;
  let corrected = Filename.concat root (help ^ ".corrected") in
  equal ~msg:"the .corrected holds the canonical content" string "hi\n"
    (read_raw corrected);
  is_true ~msg:"the file itself is not created"
    (not (exists (Filename.concat root help)));
  is_true ~msg:"the write is reported"
    (B.writes t = [ B.Written { path = corrected; literals = 0 } ])

let () =
  reg "the registry law: one content per key per run" @@ fun () ->
  let root = temp_dir () in
  let t = B.create ~root ~cwd:root ~mode:B.Corrected () in
  ignore (expect_failure "first" (fun () -> B.check t file_help "hi"));
  expect_pass "the same content again passes" (fun () ->
      B.check t file_help "hi");
  (match
     expect_failure "another content" (fun () -> B.check t file_help "yo")
   with
  | Some (_, Failure.Mismatch { expected; actual }) ->
      is_true ~msg:"the mismatch is against the accepted content"
        (expected = "hi\n" && actual = "yo\n")
  | _ -> is_true ~msg:"another content: Mismatch" false);
  equal ~msg:"only the first check recorded a correction" int 1
    (B.settle t ~keep:true)

let () =
  reg "gating: a dropped correction unaccepts its key" @@ fun () ->
  let root = temp_dir () in
  let t = B.create ~root ~cwd:root ~mode:B.Corrected () in
  ignore (expect_failure "first" (fun () -> B.check t file_help "hi"));
  equal ~msg:"settle without keep drops it" int 0 (B.settle t ~keep:false);
  (match expect_failure "next test" (fun () -> B.check t file_help "yo") with
  | Some (_, Failure.Missing { proposed }) ->
      equal ~msg:"the next test records its own content" string "yo\n" proposed
  | _ -> is_true ~msg:"next test: Missing again, not a Mismatch" false);
  equal ~msg:"and that one is kept" int 1 (B.settle t ~keep:true);
  B.write t;
  equal ~msg:"the kept content is what is written" string "yo\n"
    (read_raw (Filename.concat root (help ^ ".corrected")))

(* Update mode *)

let () =
  reg "update: the check accepts silently and writes in place" @@ fun () ->
  let root = temp_dir () in
  let t = B.create ~root ~cwd:root ~mode:B.Update () in
  expect_pass "a missing file is accepted" (fun () ->
      B.check t file_help "hello\r\nworld");
  (match
     expect_failure "another content" (fun () -> B.check t file_help "x")
   with
  | Some (_, Failure.Mismatch { expected; _ }) ->
      equal ~msg:"never last-write-wins" string "hello\nworld\n" expected
  | _ -> is_true ~msg:"another content: Mismatch" false);
  ignore (B.settle t ~keep:true);
  is_true ~msg:"nothing is written before write"
    (not (exists (Filename.concat root help)));
  B.write t;
  equal ~msg:"the file holds the canonical content" string "hello\nworld\n"
    (read_raw (Filename.concat root help));
  is_true ~msg:"no .corrected beside it"
    (not (exists (Filename.concat root (help ^ ".corrected"))));
  is_true ~msg:"the write is reported"
    (B.writes t
    = [ B.Written { path = Filename.concat root help; literals = 0 } ])

let () =
  reg "update: an equal baseline writes nothing" @@ fun () ->
  let root = temp_dir () in
  write_raw (Filename.concat root help) "keep\r\n";
  let t = B.create ~root ~cwd:root ~mode:B.Update () in
  expect_pass "equal after canonicalization" (fun () ->
      B.check t file_help "keep");
  ignore (B.settle t ~keep:true);
  B.write t;
  is_true ~msg:"no write recorded" (B.writes t = []);
  equal ~msg:"bytes untouched on disk" string "keep\r\n"
    (read_raw (Filename.concat root help))

(* Literal corrections *)

let () =
  reg "literal correction: corrected file beside the source" @@ fun () ->
  let root = temp_dir () in
  write_raw (Filename.concat root "test/t.ml") source;
  let t = B.create ~root ~cwd:root ~mode:B.Corrected () in
  ignore
    (expect_failure "mismatch" (fun () -> B.check t (literal " old ") "new"));
  ignore (B.settle t ~keep:true);
  B.write t;
  let corrected = Filename.concat root "test/t.ml.corrected" in
  equal ~msg:"the literal is rewritten in the copy" string
    "let () =\n  expect (f ()) @@ __POS_OF__ {| new |}\n" (read_raw corrected);
  equal ~msg:"the source is untouched" string source
    (read_raw (Filename.concat root "test/t.ml"));
  is_true ~msg:"one literal reported"
    (B.writes t = [ B.Written { path = corrected; literals = 1 } ])

let () =
  reg "literal correction: in place under Update, several per file" @@ fun () ->
  let root = temp_dir () in
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
  equal ~msg:"both literals rewritten in the file" string
    "let () =\n\
    \  expect a @@ __POS_OF__ {| one |};\n\
    \  expect b @@ __POS_OF__ {| two |}\n"
    (read_raw (Filename.concat root "test/t.ml"));
  is_true ~msg:"two literals in one write"
    (B.writes t
    = [ B.Written { path = Filename.concat root "test/t.ml"; literals = 2 } ])

(* A refused mismatch, as ["line N: reason"]. *)
let refused label f =
  match f () with
  | () -> fail (label ^ ": the check passed")
  | exception Failure.Check_failure fl -> (
      match fl.Failure.kind with
      | Failure.Baseline
          {
            state = Failure.Mismatch _;
            withheld = Some (Failure.Refused { line; reason });
            _;
          } ->
          Printf.sprintf "line %d: %s" line reason
      | _ -> fail (label ^ ": not a refused mismatch"))

let () =
  reg "literal correction: a drifted source fails the check, under Update too"
  @@ fun () ->
  let edited = "let () =\n  expect (f ()) @@ __POS_OF__ {| edited |}\n" in
  List.iter
    (fun mode ->
      let root = temp_dir () in
      write_raw (Filename.concat root "test/t.ml") edited;
      let t = B.create ~root ~cwd:root ~mode () in
      equal ~msg:"the literal's line and the reason, naming no file" string
        "line 2: the literal differs from the value the binary was compiled \
         with; rebuild and rerun"
        (refused "drifted" (fun () -> B.check t (literal " old ") "new"));
      equal ~msg:"no correction is recorded" int 0 (B.settle t ~keep:true);
      B.write t;
      is_true ~msg:"nothing is attempted" (B.writes t = []);
      equal ~msg:"the file is left alone" string edited
        (read_raw (Filename.concat root "test/t.ml")))
    [ B.Corrected; B.Update ]

let () =
  reg "literal correction: an unreadable source fails the check" @@ fun () ->
  let root = temp_dir () in
  let t = B.create ~root ~cwd:root ~mode:B.Corrected () in
  equal ~msg:"the literal's line and the reason, naming no file" string
    "line 2: the source file cannot be read: No such file or directory"
    (refused "missing source" (fun () -> B.check t (literal " old ") "new"));
  is_true ~msg:"Check mode reads no source"
    (match
       B.check
         (B.create ~root ~cwd:root ~mode:B.Check ())
         (literal " old ") "new"
     with
    | () -> false
    | exception Failure.Check_failure { Failure.kind; _ } -> (
        match kind with
        | Failure.Baseline { withheld = None; _ } -> true
        | _ -> false))

let () =
  reg "literal correction: an edit between the check and the write is refused"
  @@ fun () ->
  let root = temp_dir () in
  let path = Filename.concat root "test/t.ml" in
  write_raw path source;
  let t = B.create ~root ~cwd:root ~mode:B.Update () in
  expect_pass "accepted" (fun () -> B.check t (literal " old ") "new");
  ignore (B.settle t ~keep:true);
  let edited = "let () =\n  expect (f ()) @@ __POS_OF__ {| edited |}\n" in
  write_raw path edited;
  B.write t;
  (match B.writes t with
  | [ B.Refused { path = refused; reason } ] ->
      equal ~msg:"the refusal names the file" string path refused;
      equal ~msg:"and says it changed" string
        "it changed during the run: the literal differs from the value the \
         binary was compiled with; rebuild and rerun"
        reason
  | _ -> fail "one refusal and nothing written");
  equal ~msg:"the file is left alone" string edited (read_raw path)

(* Build-copy placement *)

let () =
  reg "a build action reads and corrects dune's copy" @@ fun () ->
  let root = temp_dir () in
  let build = Filename.concat root "_build/default" in
  let cwd = Filename.concat build "test" in
  write_raw (Filename.concat build help) "copy\n";
  let t = B.create ~root ~cwd ~mode:B.Corrected () in
  expect_pass "the copy is the baseline read" (fun () ->
      B.check t file_help "copy");
  is_true ~msg:"the source-tree file need not exist"
    (not (exists (Filename.concat root help)));
  ignore
    (expect_failure "a mismatch" (fun () ->
         B.check t (B.File "test/other.expected") "x"));
  ignore (B.settle t ~keep:true);
  B.write t;
  let corrected = Filename.concat build "test/other.expected.corrected" in
  is_true ~msg:".corrected lands beside the copy" (exists corrected);
  is_true ~msg:"and nowhere near the source tree"
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
  let root = temp_dir () in
  let build = Filename.concat root "_build/default" in
  write_raw (Filename.concat build "test/t.ml") source;
  write_raw (Filename.concat root "test/t.ml") source;
  let t =
    B.create ~root ~cwd:(Filename.concat build "test") ~mode:B.Update ()
  in
  expect_pass "accepted" (fun () -> B.check t (literal " old ") "new");
  ignore (B.settle t ~keep:true);
  B.write t;
  is_true ~msg:"the source is rewritten"
    (Text.contains_substring ~pattern:"{| new |}"
       (read_raw (Filename.concat root "test/t.ml")));
  equal ~msg:"the copy is untouched" string source
    (read_raw (Filename.concat build "test/t.ml"))

let () =
  reg "a build context of another root is not this run's build action"
  @@ fun () ->
  let root = temp_dir () in
  let other = temp_dir () in
  write_raw (Filename.concat root help) "source\n";
  write_raw (Filename.concat other ("_build/default/" ^ help)) "elsewhere\n";
  let t =
    B.create ~root
      ~cwd:(Filename.concat other "_build/default/test")
      ~mode:B.Check ()
  in
  expect_pass "the file under the root is read" (fun () ->
      B.check t file_help "source")

(* The registry's edges *)

let () =
  reg "mode is the mode of create" @@ fun () ->
  List.iter
    (fun mode ->
      is_true ~msg:"mode" (B.mode (B.create ~root:"/" ~cwd:"/" ~mode ()) = mode))
    [ B.Check; B.Corrected; B.Update ]

let () =
  reg "two spellings of one file are two keys, and the later content is written"
  @@ fun () ->
  let root = temp_dir () in
  let t = B.create ~root ~cwd:root ~mode:B.Update () in
  expect_pass "the first spelling records" (fun () ->
      B.check t (B.File "a/b.txt") "one");
  expect_pass "the second spelling is another key" (fun () ->
      B.check t (B.File "a/./b.txt") "two");
  equal ~msg:"two corrections" int 2 (B.settle t ~keep:true);
  B.write t;
  equal ~msg:"the later content, no mismatch between them" string "two\n"
    (read_raw (Filename.concat root "a/b.txt"))

let () =
  reg "a relative root is made absolute when the registry is created"
  @@ fun () ->
  let top = temp_dir () in
  chdir top;
  let t = B.create ~root:"proj" ~cwd:"proj" ~mode:B.Update () in
  chdir (temp_dir ());
  expect_pass "accepted" (fun () -> B.check t (B.File "a.txt") "x");
  ignore (B.settle t ~keep:true);
  B.write t;
  equal ~msg:"written under the root as it stood at create" string "x\n"
    (read_raw (Filename.concat top "proj/a.txt"));
  is_true ~msg:"and reported absolute"
    (List.for_all
       (function
         | B.Written { path; _ } | B.Refused { path; _ } ->
             not (Filename.is_relative path))
       (B.writes t))

let () =
  reg "a root that ends in a slash is never a build action" @@ fun () ->
  let root = temp_dir () in
  write_raw (Filename.concat root help) "source\n";
  write_raw (Filename.concat root ("_build/default/" ^ help)) "copy\n";
  let t =
    B.create ~root:(root ^ "/")
      ~cwd:(Filename.concat root "_build/default/test")
      ~mode:B.Check ()
  in
  expect_pass "the file under the root is read, not dune's copy" (fun () ->
      B.check t file_help "source")

let () =
  reg "create raises Sys_error when the root needs an unreadable directory"
  @@ fun () ->
  if Sys.win32 then skip ~reason:"POSIX only" ();
  let gone = Filename.concat (temp_dir ()) "gone" in
  Unix.mkdir gone 0o700;
  chdir gone;
  Unix.rmdir gone;
  (match Sys.getcwd () with
  | _ -> skip ~reason:"this system reads a removed working directory" ()
  | exception Sys_error _ -> ());
  setenv "WINDTRAP_PROJECT_ROOT" (Some "relative");
  raises_match ~msg:"Os.project_root's Sys_error passes" Check.Exn.sys_error
    (fun () -> B.create ~cwd:"/" ~mode:B.Check ())

(* Checking *)

let () =
  reg "a check computes no location and carries the one given" @@ fun () ->
  let root = temp_dir () in
  let t = B.create ~root ~cwd:root ~mode:B.Check () in
  let loc_of ?loc () =
    match B.check t ?loc (B.File "x") "v" with
    | () -> failf "the check passed"
    | exception Failure.Check_failure f -> f.Failure.loc
  in
  is_true ~msg:"no location without one" (loc_of () = None);
  let loc = { Loc.file = "test/t.ml"; line = 7; column = 2 } in
  is_true ~msg:"the given location" (loc_of ~loc () = Some loc)

let () =
  reg "~correct:false checks whatever the mode" @@ fun () ->
  let root = temp_dir () in
  List.iter
    (fun mode ->
      let t = B.create ~root ~cwd:root ~mode () in
      ignore
        (expect_failure "a mismatch fails" (fun () ->
             B.check t ~correct:false (B.File "x") "v"));
      equal ~msg:"and records no correction" int 0 (B.settle t ~keep:true))
    [ B.Corrected; B.Update ]

let () =
  reg "the failure bounds its texts, the correction holds actual whole"
  @@ fun () ->
  let root = temp_dir () in
  let big =
    String.concat "" (List.init 20_000 (fun i -> string_of_int i ^ "\n"))
  in
  let t = B.create ~root ~cwd:root ~mode:B.Corrected () in
  (match expect_failure "missing" (fun () -> B.check t file_help big) with
  | Some (_, Failure.Missing { proposed }) ->
      is_true ~msg:"the proposal is bounded"
        (String.length proposed < String.length big)
  | _ -> is_true ~msg:"missing: Missing payload" false);
  ignore (B.settle t ~keep:true);
  B.write t;
  equal ~msg:"the .corrected holds every byte" string big
    (read_raw (Filename.concat root (help ^ ".corrected")))

let () =
  reg "check raises Sys_error on an existing file it cannot read" @@ fun () ->
  let root = temp_dir () in
  Unix.mkdir (Filename.concat root "dir.expected") 0o700;
  let t = B.create ~root ~cwd:root ~mode:B.Check () in
  raises_match ~msg:"a directory where the baseline is" Check.Exn.sys_error
    (fun () -> B.check t (B.File "dir.expected") "v")

(* Settling and writing *)

let () =
  reg "settle ~keep:false returns 0 whatever the attempt recorded" @@ fun () ->
  let root = temp_dir () in
  let t = B.create ~root ~cwd:root ~mode:B.Update () in
  expect_pass "one" (fun () -> B.check t (B.File "a") "1");
  expect_pass "two" (fun () -> B.check t (B.File "b") "2");
  equal ~msg:"two dropped, 0 returned" int 0 (B.settle t ~keep:false)

let () =
  reg "a second write writes nothing" @@ fun () ->
  let root = temp_dir () in
  let t = B.create ~root ~cwd:root ~mode:B.Update () in
  expect_pass "accepted" (fun () -> B.check t file_help "first");
  ignore (B.settle t ~keep:true);
  B.write t;
  let path = Filename.concat root help in
  write_raw path "edited\n";
  B.write t;
  equal ~msg:"the file keeps the edit" string "edited\n" (read_raw path);
  equal ~msg:"one write reported" int 1 (List.length (B.writes t))

let () =
  reg "update replaces a file baseline whatever happened to it" @@ fun () ->
  let root = temp_dir () in
  let path = Filename.concat root help in
  write_raw path "old\n";
  let t = B.create ~root ~cwd:root ~mode:B.Update () in
  expect_pass "accepted" (fun () -> B.check t file_help "new");
  write_raw path "edited since\n";
  ignore (B.settle t ~keep:true);
  B.write t;
  equal ~msg:"the correction replaces the edit" string "new\n" (read_raw path)

let () =
  reg "a file baseline whose write fails is a refusal" @@ fun () ->
  let root = temp_dir () in
  let t = B.create ~root ~cwd:root ~mode:B.Update () in
  expect_pass "accepted" (fun () -> B.check t (B.File "out/x") "v");
  ignore (B.settle t ~keep:true);
  (* A directory where the file goes: the rename over it fails. *)
  Os.mkdir_p (Filename.concat root "out/x");
  B.write t;
  match B.writes t with
  | [ B.Refused { path; reason } ] ->
      equal ~msg:"the refusal names the file" string
        (Filename.concat root "out/x")
        path;
      is_true ~msg:"with the Sys_error's message" (reason <> "")
  | _ -> fail "one refusal and nothing written"

let () =
  reg "a directory that cannot be created refuses its file alone" @@ fun () ->
  if Sys.win32 then skip ~reason:"POSIX only" ();
  if Unix.geteuid () = 0 then
    skip ~reason:"root writes a read-only directory" ();
  let root = temp_dir () in
  write_raw (Filename.concat root "test/t.ml") source;
  let read_only = Filename.concat root "ro" in
  Unix.mkdir read_only 0o700;
  let t = B.create ~root ~cwd:root ~mode:B.Update () in
  expect_pass "a literal" (fun () -> B.check t (literal " old ") "new");
  expect_pass "a file under the read-only directory" (fun () ->
      B.check t (B.File "ro/sub/x") "v");
  expect_pass "a file after it" (fun () -> B.check t (B.File "z.txt") "z");
  ignore (B.settle t ~keep:true);
  Unix.chmod read_only 0o500;
  Fun.protect
    ~finally:(fun () -> Unix.chmod read_only 0o700)
    (fun () -> B.write t);
  (match B.writes t with
  | [
   B.Refused { path = refused; reason };
   B.Written { path = source; literals = 1 };
   B.Written { path = after; literals = 0 };
  ] ->
      equal ~msg:"the file under the read-only directory is refused" string
        (Filename.concat root "ro/sub/x")
        refused;
      equal ~msg:"because its directory cannot be created" string
        (Printf.sprintf "cannot create directory %s: %s"
           (Os.display_path (Filename.concat root "ro/sub"))
           (Unix.error_message Unix.EACCES))
        reason;
      equal ~msg:"the source is written" string
        (Filename.concat root "test/t.ml")
        source;
      equal ~msg:"and so is the file after the refusal" string
        (Filename.concat root "z.txt")
        after
  | _ -> fail "one refusal and two writes, in path order");
  is_true ~msg:"the literal is rewritten"
    (Text.contains_substring ~pattern:"{| new |}"
       (read_raw (Filename.concat root "test/t.ml")));
  equal ~msg:"the file after the refusal holds its content" string "z\n"
    (read_raw (Filename.concat root "z.txt"))

let () =
  reg "refused and written files are in one list, in path order" @@ fun () ->
  let root = temp_dir () in
  write_raw (Filename.concat root "test/b.ml") source;
  let t = B.create ~root ~cwd:root ~mode:B.Update () in
  expect_pass "c first" (fun () -> B.check t (B.File "test/c.ml") "c");
  expect_pass "then b" (fun () ->
      B.check t
        (B.Literal
           { pos = ("test/b.ml", 2, 19, 40); value = " old "; exact = false })
        "new");
  expect_pass "then a" (fun () -> B.check t (B.File "test/a.ml") "a");
  ignore (B.settle t ~keep:true);
  (* Directories where the two files go: their renames fail. *)
  Os.mkdir_p (Filename.concat root "test/a.ml");
  Os.mkdir_p (Filename.concat root "test/c.ml");
  B.write t;
  let file name = Filename.concat root ("test/" ^ name) in
  equal ~msg:"sorted by path, whatever their outcome" (list string)
    [
      "refused " ^ file "a.ml"; "wrote " ^ file "b.ml"; "refused " ^ file "c.ml";
    ]
    (List.map
       (function
         | B.Written { path; _ } -> "wrote " ^ path
         | B.Refused { path; _ } -> "refused " ^ path)
       (B.writes t))

let tests = List.rev !registered
let () = exit @@ Windtrap.run "baseline" tests
