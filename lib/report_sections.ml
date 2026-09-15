(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   The failure blocks adapt windtrap v1's progress.ml, rebuilt over typed
   Failure payloads and Diff data — the report projects run data, never
   alters it. The section vocabulary is v3's: one gutter renderer serving
   coverage's per-file report and mutation's survivor blocks.
  ---------------------------------------------------------------------------*)

let spf = Printf.sprintf

(* Layout constants — illustrative, not contract. The report is not a
   canvas: one width, so a pipe and a wide terminal are byte-identical. *)
let columns = 80
let rule_width = 54
let max_diff_lines = 200
let max_proposed_lines = 20
let indent = "    "

(* Gap between a survivor witness's name and its declaration site. Wide,
   like the verbose status line's duration column and unlike the tight
   table gutters: test names vary enough in length that a two-space gap
   reads as a ragged wall. The executable column before it is tight:
   executable names are of a length. *)
let witness_gap = 6
let exe_gap = 3

(* Small helpers *)
let rec take n = function
  | [] -> []
  | _ when n <= 0 -> []
  | x :: rest -> x :: take (n - 1) rest

let dashes n = String.concat "" (List.init (max 0 n) (fun _ -> "\u{2500}"))

(* The one description of a failing property case — "example N" / "case N"
   / "case N, shrunk S steps" — shared by the one-line summary and the
   failure block's counterexample line. *)
let property_case_desc ~examples ~case_index ~shrink_steps =
  if examples then spf "example %d" (case_index + 1)
  else if shrink_steps = 0 then spf "case %d" case_index
  else spf "case %d, shrunk %d steps" case_index shrink_steps

(* POSIX single-quoting: closes the quote around every embedded [']. *)
let shell_quote s =
  "'" ^ String.concat "'\\''" (String.split_on_char '\'' s) ^ "'"

(* Command hints

   Every command hint completes the run's rerun spelling from the one
   invocation the facade computed at startup (Run.config.invocation) — no
   print site hard-codes an invocation, so no hint can name a command that
   would not re-run the suite. Under [`Mirrors] (every run dune drives,
   and the default) hints spell [WINDTRAP_*] environment prefixes to
   [dune runtest], the only interface that exists there. No color in any
   hint. *)
(* The acceptance line under a baseline failure. An executable invoked by
   hand accepts in place with [-u]; a run dune drives — the inline runner,
   or a stanza's [--corrected] action, both [`Mirrors] — wrote its
   corrections beside the file for [dune promote]. *)
let accept_line = function
  | `Exe cmd -> spf "accept: %s -u, then review with git diff" cmd
  | `Mirrors -> "accept: dune promote"

(* [count] is a property failure's one config-sourced knob
   (Failure.kind.Property): the hint restates it — [--prop-count]/
   [WINDTRAP_PROP_COUNT] — because replaying a late case needs at least as
   many cases as the failing run generated. A declaration-site count needs
   no flag and never reaches here, and the shrink budget is fixed, so the
   seed alone descends to the same node. *)
let replay_line ?count invocation ~seed ~filter =
  let token = Seed.to_string seed in
  let flags =
    match count with Some n -> spf " --prop-count %d" n | None -> ""
  in
  let env =
    match count with Some n -> spf " WINDTRAP_PROP_COUNT=%d" n | None -> ""
  in
  match (invocation, filter) with
  | `Exe cmd, Some flt ->
      spf "replay: %s --seed %s%s -f %s" cmd token flags (shell_quote flt)
  | `Exe cmd, None -> spf "replay: %s --seed %s%s" cmd token flags
  | `Mirrors, Some flt ->
      spf "replay: WINDTRAP_SEED=%s%s WINDTRAP_FILTER=%s dune runtest" token env
        (shell_quote flt)
  | `Mirrors, None -> spf "replay: WINDTRAP_SEED=%s%s dune runtest" token env

(* Failure locations record project-root-relative source paths (__POS__,
   debug info), so a relative path resolves against the project root first —
   under [dune runtest] the process cwd is inside _build, where the recorded
   path never opens — then, best-effort, as given. The excerpt therefore
   renders identically from the repo root and under dune. *)
let open_source file =
  match open_in file with ic -> Some ic | exception Sys_error _ -> None

let source_line file n =
  if n < 1 then None
  else
    let ic =
      if Filename.is_relative file then
        (* Best-effort all the way down: root discovery reads the cwd,
           which code under test may have deleted ([Sys.getcwd] then
           raises) — an unreadable excerpt prints nothing, never crashes
           the report. *)
        let root =
          match Path_ops.project_root () with
          | root -> Some root
          | exception Sys_error _ -> None
        in
        match
          Option.bind root (fun root -> open_source (Filename.concat root file))
        with
        | Some _ as ic -> ic
        | None -> open_source file
      else open_source file
    in
    match ic with
    | None -> None
    | Some ic ->
        Fun.protect
          ~finally:(fun () -> close_in_noerr ic)
          (fun () ->
            let rec skip k =
              match input_line ic with
              | line -> if k = 0 then Some line else skip (k - 1)
              | exception End_of_file -> None
            in
            skip (n - 1))

(* Terminal surfaces print user-controlled names (test paths, suite names,
   fixture names) verbatim; a raw newline or control byte in one corrupts
   the layout — it splits the FAIL header, and the live tail's line-wise
   erasure leaves residue. Escape C0 controls and DEL,
   OCaml-style. ESC is left to the [ansi] policy: the sink strips escape
   sequences under [ansi:false] and passes them through under [ansi:true]
   (the documented payload contract). *)
let sanitize_name s =
  let escapes c = (c < ' ' && c <> '\027') || c = '\127' in
  if not (String.exists escapes s) then s
  else begin
    let buf = Buffer.create (String.length s + 8) in
    String.iter
      (fun c ->
        match c with
        | '\n' -> Buffer.add_string buf "\\n"
        | '\t' -> Buffer.add_string buf "\\t"
        | '\r' -> Buffer.add_string buf "\\r"
        | c when escapes c ->
            Buffer.add_string buf (spf "\\x%02x" (Char.code c))
        | c -> Buffer.add_char buf c)
      s;
    Buffer.contents buf
  end

(* Failure projections *)

(* Comparison surfaces print the values a test produced, and a control byte
   in one of those drives the terminal instead of appearing in the report:
   ESC eats the label beside it and leaves the terminal coloured, CR
   overwrites the line the reader needed, and a grep for the reported value
   finds nothing. Every C0 byte and DEL therefore renders as a lowercase
   [\xNN] escape — one rule, no mnemonics, so [\x] marks every escape a
   reader sees — with LF and TAB the two exceptions, because line structure
   and indentation ARE the layout the block is built from.

   This is a projection, exactly like colour: equality, containment, and
   baseline storage never see it. The transform is not injective — a value
   holding the four characters [\x1b] renders like one holding the byte —
   because the alternative is escaping the backslash, which would double
   every escape in the [%S] renderings that make up most of a transcript.
   Every structural decision (are the two renderings equal, do their line
   lists differ, which regions changed) is therefore made on the raw
   values, and only the printed glyphs and their column arithmetic move
   into escaped space.

   Distinct from [sanitize_name] above: a name has no line structure to
   preserve, so it spells [\n] and [\t] out, and its ESC belongs to the
   [ansi] policy rather than to this one. *)
let control_byte c = (c < ' ' && c <> '\n' && c <> '\t') || c = '\127'

let show_controls s =
  if not (String.exists control_byte s) then s
  else begin
    let buf = Buffer.create (String.length s + 8) in
    String.iter
      (fun c ->
        if control_byte c then
          Buffer.add_string buf (spf "\\x%02x" (Char.code c))
        else Buffer.add_char buf c)
      s;
    Buffer.contents buf
  end

(* [span], given in [s]'s byte coordinates, moved into those of
   [show_controls s]. The escape is per byte and context-free, so escaping
   a prefix and escaping the whole string agree on that prefix: the moved
   span covers all four columns of every escape sequence it opened, which
   is what keeps a [~~~] marker under the region it marks. *)
let moved_span s ({ Diff.start; length } as span) =
  if not (String.exists control_byte s) then span
  else
    {
      Diff.start = String.length (show_controls (String.sub s 0 start));
      length = String.length (show_controls (String.sub s start length));
    }

(* One line, no escape codes, bounded: headline material. Stripping comes
   first so truncation cannot leave a dangling partial sequence. *)
let flat s =
  Text.truncate_utf8 60
    (String.map
       (function '\n' | '\r' | '\t' -> ' ' | c -> c)
       (Text.strip_ansi s))

(* The msg slot as displayed: a sub-case entry's [leaf › name] label
   (derived from the structured components — never sniffed from the text)
   joined with the user's annotation when there is one. *)
let labeled_msg (f : Failure.t) =
  match f.Failure.subtest with
  | [] -> f.Failure.msg
  | components -> (
      let label = Test_tree.path_to_string components in
      match f.Failure.msg with
      | None -> Some label
      | Some m -> Some (label ^ ": " ^ m))

let headline (f : Failure.t) =
  let base =
    match f.kind with
    | Failure.Equality { not_ = true; expected; _ } ->
        spf "both sides equal: %s" (flat expected)
    | Failure.Equality { expected; actual; _ } ->
        spf "expected %s, got %s" (flat expected) (flat actual)
    | Failure.Containment { needle; found_at; haystack_length; demand; _ } -> (
        (* The containment verdict, never a fake equality. The demand comes
           first: a chain break and a count mismatch are their own verdicts,
           and neither reads as "found / not found". *)
        let quoted = flat (spf "%S" needle) in
        match (demand, found_at) with
        | Failure.Ordered { index; resumed_at }, Some at ->
            spf "element %d %s out of order: at byte %d, before byte %d" index
              quoted at resumed_at
        | Failure.Ordered { index; resumed_at }, None ->
            spf "element %d %s not found at or after byte %d (%d-byte haystack)"
              index quoted resumed_at haystack_length
        | Failure.Anywhere, Some at ->
            spf "needle %s found at byte %d" quoted at
        | Failure.Anywhere, None ->
            spf "needle %s not found (%d-byte haystack)" quoted haystack_length)
    | Failure.Raise { expected = Some e; actual = Some a; _ } ->
        spf "expected exception %s, raised %s" (flat e) (flat a)
    | Failure.Raise { expected = Some e; actual = None; _ } ->
        spf "expected exception %s, none raised" (flat e)
    | Failure.Raise { expected = None; actual = Some a; predicate; _ } ->
        (* [predicate] tells a raises_match rejection from an exception
           nobody expected. *)
        if predicate then
          spf "exception did not satisfy the predicate: %s" (flat a)
        else spf "uncaught exception: %s" (flat a)
    | Failure.Raise { expected = None; actual = None; _ } ->
        "expected an exception, none raised"
    | Failure.Baseline { baseline; state } -> (
        let subject =
          match baseline with
          | Failure.Literal -> "expect"
          | Failure.File path -> spf "expect_file %S" path
        in
        match state with
        | Failure.Missing _ -> spf "%s: no baseline" subject
        | Failure.Mismatch _ -> spf "%s: mismatch" subject
        | Failure.Unresolvable _ ->
            spf "%s: cannot resolve the path under the project root" subject)
    | Failure.Property
        {
          rendered;
          case_index;
          shrink_steps;
          shrink_exhausted;
          timed_out;
          examples;
          _;
        } ->
        let desc = property_case_desc ~examples ~case_index ~shrink_steps in
        let desc =
          (* The shrink search hit the whole-test budget: the mark
             travels into the one-line summary too. *)
          match timed_out with
          | Some _ when not examples -> desc ^ ", timed out"
          | _ ->
              (* Likewise the step budget: "shrunk 100 steps" alone reads
                 as a converged search. *)
              if shrink_exhausted && not examples then desc ^ ", budget spent"
              else desc
        in
        spf "property failed (%s): %s" desc (flat rendered)
    | Failure.Message "" -> "(empty failure message)"
    | Failure.Message m -> flat m
  in
  match labeled_msg f with
  | None -> base
  | Some m -> spf "%s \u{2014} %s" (flat m) base

(* [s] with [spans] (ascending, non-overlapping byte ranges) wrapped in the
   escape codes of [style]; [s] unchanged when [ansi] is false. *)
let highlight ~ansi style s spans =
  if (not ansi) || spans = [] then s
  else begin
    let buf = Buffer.create (String.length s + 16) in
    let pos = ref 0 in
    List.iter
      (fun { Diff.start; length } ->
        Buffer.add_string buf (String.sub s !pos (start - !pos));
        Buffer.add_string buf
          (Pp.styled_string ~ansi style (String.sub s start length));
        pos := start + length)
      spans;
    Buffer.add_string buf (String.sub s !pos (String.length s - !pos));
    Buffer.contents buf
  end

(* The [~~~] line under a plain string: one column per code point. *)
let marker_line s spans =
  if spans = [] then None
  else begin
    let buf = Buffer.create (String.length s) in
    let col = ref 0 in
    List.iter
      (fun { Diff.start; length } ->
        let scol = Text.length_utf8 (String.sub s 0 start) in
        let width = max 1 (Text.length_utf8 (String.sub s start length)) in
        if scol > !col then
          Buffer.add_string buf (String.make (scol - !col) ' ');
        Buffer.add_string buf (String.make width '~');
        col := max scol !col + width)
      spans;
    Some (Buffer.contents buf)
  end

(* Trailing whitespace on a changed hunk line, made visible: one
   [·] (U+00B7) per space and one [→] (U+2192) per tab, on both the ansi
   and plain paths — a color-only highlight would vanish on exactly the
   no-color sinks (pipes, JUnit bodies, annotations) where the difference
   bites. Changed lines only; context lines are untouched. *)
let show_trailing_ws s =
  let n = String.length s in
  let i = ref n in
  while !i > 0 && (s.[!i - 1] = ' ' || s.[!i - 1] = '\t') do
    decr i
  done;
  if !i = n then s
  else begin
    let buf = Buffer.create (n + 8) in
    Buffer.add_substring buf s 0 !i;
    for j = !i to n - 1 do
      Buffer.add_string buf (if s.[j] = ' ' then "\u{00B7}" else "\u{2192}")
    done;
    Buffer.contents buf
  end

let pp_hunks ~ansi put ~ind hunks =
  let st style s = Pp.styled_string ~ansi style s in
  let total =
    List.fold_left (fun acc h -> acc + 1 + List.length h.Diff.lines) 0 hunks
  in
  let budget = ref max_diff_lines in
  let emit line =
    if !budget > 0 then put line;
    decr budget
  in
  List.iter
    (fun (h : Diff.hunk) ->
      emit
        (ind
        ^ st `Faint
            (spf "@@ -%d,%d +%d,%d @@" h.expected_start h.expected_count
               h.actual_start h.actual_count));
      List.iter
        (function
          (* Green is the expected side and red the actual one, here as
             everywhere else — not the diff tool's red-for-removed. One
             transcript shows both this path and the span path, often for
             the same run, and a colour that means "expected" on one line
             and "actual" three lines down is worse than unusual. The
             [---]/[+++] header and the [-]/[+] sigils already say which
             side is which, so the colour is free to carry the report's
             own meaning. *)
          | Diff.Keep s -> emit (ind ^ "  " ^ show_controls s)
          | Diff.Delete s ->
              emit (ind ^ st `Green ("- " ^ show_trailing_ws (show_controls s)))
          | Diff.Insert s ->
              emit (ind ^ st `Red ("+ " ^ show_trailing_ws (show_controls s))))
        h.lines)
    hunks;
  if total > max_diff_lines then
    put
      (ind
      ^ st `Faint
          (spf "\u{2026} (+%d more diff lines)" (total - max_diff_lines)))

let pp_eq_detail ~ansi put ~ind ~expected ~actual =
  let st style s = Pp.styled_string ~ansi style s in
  if String.contains expected '\n' || String.contains actual '\n' then
    begin match Diff.hunks ~expected ~actual () with
    | [] ->
        (* Line lists equal but bytes differ: the only such difference is a
           single trailing newline, which a line diff cannot show. *)
        let side =
          if String.length actual > String.length expected then "actual"
          else "expected"
        in
        put
          (ind
          ^ spf "values differ only by a trailing newline (on the %s side)" side
          )
    | hunks ->
        put (ind ^ st `Faint "--- expected");
        put (ind ^ st `Faint "+++ actual");
        pp_hunks ~ansi put ~ind hunks
    end
  else
    (* The marks under the two renderings: the changed regions character
       refinement found, or nothing when it declined. *)
    let marked =
      match Diff.refine ~expected ~actual with
      | Some r -> Some (r.Diff.expected_spans, r.Diff.actual_spans)
      | None -> None
    in
    (* Refinement ran against the raw values; the marks are drawn against
       the escaped ones, so both sides move into display coordinates
       together and a widened escape carries its marks with it. *)
    let marked =
      Option.map
        (fun (es, as_) ->
          (List.map (moved_span expected) es, List.map (moved_span actual) as_))
        marked
    in
    let expected = show_controls expected and actual = show_controls actual in
    match marked with
    | Some (es, as_) when ansi ->
        put
          (ind ^ st `Faint "expected" ^ "  "
          ^ highlight ~ansi `Green expected es);
        put (ind ^ st `Faint "actual" ^ "    " ^ highlight ~ansi `Red actual as_)
    | None when ansi ->
        (* Refinement declined: the values share too little for a partial
           mark to point at anything. Green and red are side colours, not
           change markers, so colouring each side whole is the same signal
           extended — and it keeps every equality failure reading the same
           way instead of the colour appearing and vanishing on a threshold
           the reader cannot see. Plain sinks show the two labelled values
           and stop: a full-width [~~~] would be the noise the threshold
           just removed. *)
        put (ind ^ st `Faint "expected" ^ "  " ^ st `Green expected);
        put (ind ^ st `Faint "actual" ^ "    " ^ st `Red actual)
    | _ ->
        (* Plain sinks carry the marks on their own line, under the side they
           belong to. Each side gets one only when it has marks to carry: a
           pure insertion changes nothing on the expected side, so no marker
           line prints under it. *)
        let es, as_ = match marked with Some p -> p | None -> ([], []) in
        let side label pad s spans =
          put (ind ^ st `Faint label ^ pad ^ s);
          match marker_line s spans with
          | Some m -> put (ind ^ "          " ^ m)
          | None -> ()
        in
        side "expected" "  " expected es;
        side "actual" "    " actual as_

let pp_eq ~ansi put ~ind ~expected ~actual =
  let st style s = Pp.styled_string ~ansi style s in
  if String.equal expected actual then begin
    (* The equality distinguished the values but their printer did not
       ([equal float nan nan], a lossy pp): explain the identical lines. A
       multi-line rendering prints once, in block form — repeating it twice
       under expected/actual labels would only pad the block, and inlining
       it after a label would break the four-space indentation.

       The test is made on the raw renderings because the claim is about
       them: escaping merges values it cannot tell apart, and a pair the
       printer did distinguish must never be reported as one it did not. *)
    let expected = show_controls expected and actual = show_controls actual in
    if String.contains expected '\n' then begin
      put (ind ^ st `Faint "both render as:");
      List.iter (fun l -> put (ind ^ "  " ^ l)) (Text.split_lines expected)
    end
    else begin
      put (ind ^ st `Faint "expected" ^ "  " ^ expected);
      put (ind ^ st `Faint "actual" ^ "    " ^ actual)
    end;
    put
      (ind
      ^ st `Faint
          "(the values render identically \u{2014} the printer shows less than \
           the equality compares)")
  end
  else pp_eq_detail ~ansi put ~ind ~expected ~actual

let rec pp_gen ~ansi ~excerpt ~filter ~commands ~invocation ~ind ppf
    (f : Failure.t) =
  let st style s = Pp.styled_string ~ansi style s in
  (* Under [ansi:false] the block must contain no escape codes (report_sections.mli):
     payload strings from a user pp may carry them, so every line is
     stripped at the sink. Our own styling is off on this path. *)
  let put line =
    Pp.pf ppf "%s@\n" (if ansi then line else Text.strip_ansi line)
  in
  let put_ind line = put (ind ^ line) in
  let put_block s =
    List.iter (fun line -> put_ind ("  " ^ line)) (Text.split_lines s)
  in
  (* Phase and location header. *)
  let phase =
    match f.phase with
    | Failure.Body -> None
    | Failure.Setup -> Some "setup"
    | Failure.Teardown -> Some "teardown"
    | Failure.Release -> Some "release"
  in
  (match (phase, f.loc) with
  | None, None -> ()
  | _ ->
      let parts =
        (match phase with
          | Some p -> [ st `Yellow ("[" ^ p ^ "]") ]
          | None -> [])
        @
        match f.loc with
        | Some l -> [ st `Faint (Loc.to_string l) ]
        | None -> []
      in
      put_ind (String.concat " " parts));
  (* The location is the test's declaration, not the failing call's line:
     say so once, under it, before the excerpt shows the reader the wrong
     line. The runtime cannot recover a tail-called frame, so the remedy
     is named here, at the point of use. Not for a property failure, whose
     location is its declaration by construction (the assertion's site is
     on [inner]), nor for an uncaught exception, which no verb raised —
     nothing to pass [~__POS__] to, and its backtrace names the line — nor
     for a file baseline, whose call takes no position and whose subject
     line names the file. *)
  (match (f.attribution, f.kind) with
  | Failure.Recorded, _
  | Failure.Declaration, Failure.Property _
  | Failure.Declaration, Failure.Raise { expected = None; predicate = false; _ }
  | Failure.Declaration, Failure.Baseline { baseline = Failure.File _; _ } ->
      ()
  | Failure.Declaration, _ ->
      put_ind
        (st `Faint
           "(assertion in tail position: its line is unknown; ~__POS__ names \
            it)"));
  (* Source excerpt, best-effort. *)
  (if excerpt then
     match f.loc with
     | Some { Loc.file; line; _ } -> (
         match source_line file line with
         | Some text ->
             put_ind (spf "  %s %s" (st `Faint (spf "%d \u{2502}" line)) text);
             put ""
         | None -> ())
     | None -> ());
  (match labeled_msg f with Some m -> put_ind m | None -> ());
  match f.kind with
  | Failure.Equality { not_ = true; expected; _ } ->
      let expected = show_controls expected in
      if String.contains expected '\n' then begin
        put_ind "both sides equal:";
        put_block expected
      end
      else put_ind (spf "both sides equal: %s" expected)
  | Failure.Equality { expected = claim; actual = value; diffable = false; _ }
    ->
      (* A claim is a description, not a rendering: never diff or refine the
         two (D5 §2). Colour still applies — green and red mark which side is
         which, and that is as true of a description as of a value, and so is
         visibility: a [~claim] may be built around a rendered bound
         ([greater than <x>]). *)
      let claim = show_controls claim and value = show_controls value in
      put_ind (st `Faint "expected" ^ "  " ^ st `Green claim);
      if String.contains value '\n' then begin
        put_ind (st `Faint "actual:");
        put_block (st `Red value)
      end
      else put_ind (st `Faint "actual" ^ "    " ^ st `Red value)
  | Failure.Equality { expected; actual; _ } ->
      pp_eq ~ansi put ~ind ~expected ~actual
  | Failure.Containment
      {
        needle;
        found_at;
        haystack_length;
        excerpt;
        excerpt_offset;
        demand;
        claim = _;
      } ->
      (* The block derives from the containment payload — needle, verdict,
         byte offset, marked occurrence — never a fake equality diff; the
         claim sentence is a description and stays out of the block. Labels
         pad to the [expected]/[actual] 10-column gutter. *)
      let verdict =
        (* The demand widens the verdict slot rather than adding lines: a
           chain break answers the same question the other verbs answer
           there, in more words. *)
        match (demand, found_at) with
        | Failure.Ordered { resumed_at; _ }, Some at ->
            spf "found at byte %d, before the search resumed at byte %d" at
              resumed_at
        | Failure.Ordered { resumed_at; _ }, None ->
            spf "not found at or after byte %d" resumed_at
        | Failure.Anywhere, Some at -> spf "found at byte %d" at
        | Failure.Anywhere, None -> "not found"
      in
      (* Which element of the chain broke it: its own line, because the
         index identifies the assertion the rest of the block is about. *)
      (match demand with
      | Failure.Ordered { index; _ } ->
          put_ind (st `Faint "element" ^ "   " ^ string_of_int index)
      | Failure.Anywhere -> ());
      (* [%S] carries its own escapes, OCaml's decimal ones, so the needle
         needs none of [show_controls]'s — as do the [%S]-quoted exception
         messages [pp_eq] diffs below. Only the unquoted surfaces do. *)
      put_ind (st `Faint "needle" ^ "    " ^ spf "%S \u{2014} %s" needle verdict);
      (* The occurrence's byte range inside the excerpt, when it is there to
         mark: a failed [not_contains] window always contains it, and an
         out-of-order chain break carries one that a cursor-anchored window
         may have left behind — hence the bounds test rather than a plain
         subtraction. Offsets are the payload's own, so the span is computed
         in raw bytes and moved into display coordinates where it is
         drawn. *)
      let occurrence =
        match found_at with
        | None -> None
        | Some at ->
            let start = at - excerpt_offset in
            let length =
              min (String.length needle) (String.length excerpt - start)
            in
            if start >= 0 && length > 0 then Some { Diff.start; length }
            else None
      in
      (if String.contains excerpt '\n' then begin
         (* Block form (the [both sides equal:] precedent): no unified diff,
            no markers; under [ansi] the occurrence highlights on its line. *)
         put_ind (st `Faint "haystack:");
         let lines = Text.split_lines excerpt in
         let offsets =
           (* Byte offset of each line's first byte within the excerpt. *)
           let rec go acc off = function
             | [] -> List.rev acc
             | line :: rest ->
                 go (off :: acc) (off + String.length line + 1) rest
           in
           go [] 0 lines
         in
         List.iter2
           (fun line off ->
             let styled =
               match occurrence with
               | Some { Diff.start; length }
                 when ansi && start >= off && start < off + String.length line
                 ->
                   let length = min length (off + String.length line - start) in
                   highlight ~ansi `Red (show_controls line)
                     [ moved_span line { Diff.start = start - off; length } ]
               | _ -> show_controls line
             in
             put_ind ("  " ^ styled))
           lines offsets
       end
       else
         let shown = show_controls excerpt in
         match occurrence with
         | Some span when ansi ->
             put_ind
               (st `Faint "haystack" ^ "  "
               ^ highlight ~ansi `Red shown [ moved_span excerpt span ])
         | occurrence -> (
             put_ind (st `Faint "haystack" ^ "  " ^ shown);
             match occurrence with
             | Some span -> (
                 match marker_line shown [ moved_span excerpt span ] with
                 | Some m -> put (ind ^ "          " ^ m)
                 | None -> ())
             | None -> ()));
      (* State what was omitted, iff the excerpt is partial. *)
      if
        excerpt_offset > 0
        || excerpt_offset + String.length excerpt < haystack_length
      then
        put_ind
          (st `Faint
             (spf "(excerpt: bytes %d-%d of a %d-byte haystack)" excerpt_offset
                (excerpt_offset + String.length excerpt - 1)
                haystack_length))
  | Failure.Raise { expected; actual; predicate; backtrace; message_diff } -> (
      (match message_diff with
      | Some { Failure.constructor; expected_message; actual_message } ->
          (* Right constructor, wrong payload: diff the messages instead of
             repeating the constructor twice. The failure site named the
             constructor; this one decision is the whole of the field. *)
          put_ind (spf "raised %s with the wrong message:" constructor);
          pp_eq ~ansi put ~ind
            ~expected:(spf "%S" expected_message)
            ~actual:(spf "%S" actual_message)
      | None -> (
          (* An exception rendering is a value like any other: a payload
             string reaches the block through [Printexc.to_string]. *)
          match
            (Option.map show_controls expected, Option.map show_controls actual)
          with
          | Some e, Some a ->
              put_ind (st `Faint "expected exception" ^ "  " ^ st `Green e);
              put_ind (st `Faint "raised            " ^ "  " ^ st `Red a)
          | Some e, None ->
              put_ind (st `Faint "expected exception" ^ "  " ^ st `Green e);
              put_ind "but no exception was raised"
          | None, Some a ->
              (* [predicate] tells a raises_match rejection from a test
                 body's escape — the two demand different reactions. *)
              put_ind
                (if predicate then
                   "raised exception does not satisfy the predicate:"
                 else "uncaught exception:");
              put_block (st `Red a)
          | None, None -> put_ind "expected an exception, but none was raised"));
      match backtrace with
      | Some bt ->
          List.iter (fun l -> put_ind (st `Faint l)) (Text.split_lines bt)
      | None -> ())
  | Failure.Baseline { baseline; state } -> (
      let accept () = if commands then put_ind (accept_line invocation) in
      (* Plain quotes, not %S: a path's UTF-8 must not be byte-escaped;
         [sanitize_name] guards the line against control bytes exactly as
         on every other name surface. *)
      let subject =
        match baseline with
        | Failure.Literal -> "expect"
        | Failure.File path ->
            spf "expect_file \"%s\"" (sanitize_name (Path_ops.display path))
      in
      match state with
      | Failure.Missing { proposed } ->
          put_ind (spf "%s: no baseline" subject);
          let lines = Text.split_lines (show_controls proposed) in
          let n = List.length lines in
          put_ind (spf "proposed (%d line%s):" n (if n = 1 then "" else "s"));
          List.iter
            (fun l -> put_ind ("  " ^ st `Cyan "\u{2506}" ^ " " ^ l))
            (take max_proposed_lines lines);
          if n > max_proposed_lines then
            put_ind
              ("  " ^ st `Cyan "\u{2506}" ^ " "
              ^ st `Faint
                  (spf "\u{2026} (+%d more lines)" (n - max_proposed_lines)));
          (* Promotion fills a file but never creates one: under dune the
             file must exist before a [diff?] step can register the
             correction, so the acceptance starts by creating it. *)
          if commands then
            begin match (invocation, baseline) with
            | `Mirrors, Failure.File path ->
                put_ind
                  (spf "accept: touch %s && dune runtest, then dune promote"
                     (shell_quote (Path_ops.display path)))
            | _ -> put_ind (accept_line invocation)
            end
      | Failure.Mismatch { expected; actual } ->
          put_ind (spf "%s: mismatch" subject);
          pp_hunks ~ansi put ~ind (Diff.hunks ~expected ~actual ());
          accept ()
      | Failure.Unresolvable { candidate } ->
          put_ind
            (spf "%s: the path cannot be proven to lie under the project root"
               subject);
          put_ind
            (spf "unverified path: %s"
               (sanitize_name (Path_ops.display candidate)));
          put_ind
            "(set WINDTRAP_PROJECT_ROOT to the directory the path is relative \
             to)")
  | Failure.Property
      {
        rendered;
        case_index;
        shrink_steps;
        shrink_exhausted;
        timed_out;
        root;
        count;
        examples;
        rendering;
        inner;
      } ->
      let desc = property_case_desc ~examples ~case_index ~shrink_steps in
      let rendered = show_controls rendered in
      (* A pre-image is marked in the slot itself — [from] — so a reader who
         stops at this line does not take it for the value the body received;
         the note below says what it is instead. *)
      let head =
        match rendering with
        | Failure.Pre_image -> spf "counterexample (%s): from" desc
        | Failure.Value -> spf "counterexample (%s):" desc
      in
      if String.contains rendered '\n' then begin
        put_ind head;
        put_block rendered
      end
      else put_ind (head ^ " " ^ rendered);
      (* A pre-image is not the value: say so once, here, where the reader
         is looking at it. *)
      (match rendering with
      | Failure.Value -> ()
      | Failure.Pre_image ->
          put_ind
            (st `Faint
               "(the value has no printer \u{2014} shown is its pre-image, \
                what map and bind computed it from)"));
      (* The shrink search hit the whole-test budget: the reported
         counterexample is the best found within it. [%g] matches the
         runner's [timed out after %gs] phrase so timeout greps catch
         both. *)
      (match timed_out with
      | Some limit ->
          put_ind
            (spf
               "timed out after %gs while shrinking; counterexample may not be \
                minimal"
               limit)
      | None ->
          (* Same fact, a different stop: the search ran out of budget, or
             out of reachable candidates because forcing one raised. Either
             way what is reported is the best it got to, and the step count
             is what tells the reader which — a count at the budget spent
             it, a count below it did not. *)
          if shrink_exhausted then
            put_ind
              (spf
                 "shrinking stopped after %d steps; counterexample may not be \
                  minimal"
                 shrink_steps));
      (match inner with
      | Some i ->
          (* A tail-called check inside a law honestly has no site: a
             dangling "at:" with no location line would misread. *)
          put_ind
            (match i.Failure.loc with
            | Some _ -> "which failed at:"
            | None -> "which failed with:");
          pp_gen ~ansi ~excerpt:false ~filter:None ~commands:false ~invocation
            ~ind:(ind ^ "  ") ppf i
      | None -> ());
      if commands && not examples then
        put_ind (replay_line ?count invocation ~seed:root ~filter)
  | Failure.Message "" -> put_ind "(empty failure message)"
  | Failure.Message m ->
      List.iter (fun line -> put_ind line) (Text.split_lines m)

let pp_failure ~ansi ?(excerpt = false) ?filter ?(invocation = `Mirrors) ppf f =
  pp_gen ~ansi ~excerpt ~filter ~commands:true ~invocation ~ind:indent ppf f

(* Sub-case entries carry their identity as data (Run.subtest fills the
   [subtest] components); the msg text is never consulted. *)
let is_subtest_failure (f : Failure.t) = f.Failure.subtest <> []
(* The section vocabulary

   The one vocabulary instrumentation reports are made of: styled lines,
   hint lines, aligned rows, source excerpts, and the failure section's
   rules. Coverage's per-file table and mutation's survivor blocks are
   two projections into it, drawn knowing nothing about the runtimes that
   measured the data — the subsystem that owns the numbers builds section
   data, and every name the runtime owns (a mutant identifier, the arming
   variable) arrives pre-spelled with the runtime's own functions. Law 12:
   a second copy of any of these drawers is exactly the drift the
   coverage command's structure exists to prevent. *)

(* The sink: where sections print and whether they style. With
   [ansi:false] every line is stripped at the sink, so escape codes
   arriving inside payload strings never reach a plain transcript. *)
type sink = { out : Format.formatter; ansi : bool }

let put k line =
  Pp.pf k.out "%s@\n" (if k.ansi then line else Text.strip_ansi line)

let st k style s = Pp.styled_string ~ansi:k.ansi style s

let labeled_rule label =
  let w = min columns rule_width in
  let inner = Text.length_utf8 label + 2 in
  let left = max 2 ((w - inner) / 2) in
  let right = max 2 (w - inner - left) in
  dashes left ^ " " ^ label ^ " " ^ dashes right

let rstrip s =
  let n = ref (String.length s) in
  while !n > 0 && s.[!n - 1] = ' ' do
    decr n
  done;
  String.sub s 0 !n

type span = { style : Pp.style option; text : string }

let plain text = { style = None; text }
let styled style text = { style = Some style; text }

let span_str k { style; text } =
  match style with None -> text | Some style -> st k style text

let line_str k spans = String.concat "" (List.map (span_str k) spans)

type column = { gap : string; align : [ `Left | `Right ]; width : int option }

(* Line numbers as ranges ([88-94, 121]), the coverage table's uncovered
   lists. Moved here from the coverage runtime with [excerpts]: layout
   lives with the vocabulary, not with the instrumentation that measured
   the lines. *)

let collapse_ranges lines =
  let rec loop acc range_start range_end = function
    | [] -> List.rev ((range_start, range_end) :: acc)
    | line :: rest ->
        if line <= range_end + 1 then
          loop acc range_start (max range_end line) rest
        else loop ((range_start, range_end) :: acc) line line rest
  in
  match lines with [] -> [] | first :: rest -> loop [] first first rest

let format_ranges ranges =
  ranges
  |> List.map (fun (s, e) ->
      if s = e then string_of_int s else Printf.sprintf "%d-%d" s e)
  |> String.concat ", "

(* The table's uncovered cell, bounded. A barely-tested file has
   hundreds of uncovered regions, and their ranges render as one cell of
   thousands of characters — a row no terminal can lay out, in the very
   report a reader opens to find out where to start. The staleness
   warnings bound their detail the same way; the unbounded answer is one
   flag away. *)
let uncovered_cap = 8

let bounded_ranges lines =
  let regions = collapse_ranges lines in
  let total = List.length regions in
  if total <= uncovered_cap then format_ranges regions
  else
    spf "%s (+%d more, -u shows them)"
      (format_ranges (List.filteri (fun i _ -> i < uncovered_cap) regions))
      (total - uncovered_cap)

(* Excerpt regions *)

type excerpt_line = { number : int; text : string; marked : bool }

(* One entry per line, mirroring the coverage runtime's offset rule: an
   empty source has no lines, and a trailing newline opens no phantom
   line. *)
let source_lines source =
  if String.length source = 0 then [||]
  else
    let lines = String.split_on_char '\n' source in
    let lines =
      if source.[String.length source - 1] = '\n' then
        match List.rev lines with "" :: rest -> List.rev rest | _ -> lines
      else lines
    in
    Array.of_list lines

let excerpts ?(context = 1) ~source marked =
  let lines = source_lines source in
  let total = Array.length lines in
  let marked =
    List.sort_uniq Int.compare marked
    |> List.filter (fun l -> l >= 1 && l <= total)
  in
  let marked_set = Hashtbl.create 16 in
  List.iter (fun l -> Hashtbl.replace marked_set l ()) marked;
  let windows =
    collapse_ranges marked
    |> List.map (fun (s, e) -> (max 1 (s - context), min total (e + context)))
  in
  let rec merge_windows = function
    | (s1, e1) :: (s2, e2) :: rest when s2 <= e1 + 1 ->
        merge_windows ((s1, max e1 e2) :: rest)
    | window :: rest -> window :: merge_windows rest
    | [] -> []
  in
  merge_windows windows
  |> List.map (fun (s, e) ->
      List.init
        (e - s + 1)
        (fun i ->
          let number = s + i in
          {
            number;
            text = lines.(number - 1);
            marked = Hashtbl.mem marked_set number;
          }))

(* The one gutter renderer: coverage draws the uncovered regions of a
   file, mutation draws the one line a survivor rewrites. *)

type excerpt = {
  file : string;
  heading : span list option;
  source : string;
  marked_lines : int list;
}

let excerpt k ?(context = 1) ?(marker = true) ?(margin = "  ") ?number_width e =
  (match e.heading with
  | None -> ()
  | Some h ->
      put k "";
      put k (spf "%s \u{2014} %s" e.file (line_str k h));
      put k "");
  let regions = excerpts ~context ~source:e.source e.marked_lines in
  let width =
    match number_width with
    | Some w -> w
    | None ->
        List.fold_left
          (List.fold_left (fun w l ->
               max w (String.length (string_of_int l.number))))
          4 regions
  in
  (* The marker rides inside [margin] rather than beside it, so a marked
     row's escape sequence opens at column zero exactly as it always
     has — the coverage transcript is byte-frozen, escapes included. *)
  let gutter marked =
    if not marker then margin
    else if marked then st k `Red (margin ^ "\u{258c}")
    else margin ^ " "
  in
  let separator =
    (if marker then margin ^ " " else margin)
    ^ "\u{00b7}\u{00b7}\u{00b7}\u{00b7}\u{00b7}"
  in
  List.iteri
    (fun i region ->
      if i > 0 then put k separator;
      List.iter
        (fun l ->
          put k
            (rstrip
               (spf "%s%*d \u{2502} %s" (gutter l.marked) width l.number l.text)))
        region)
    regions

(* The subsystem-neutral report-section vocabulary. Internal: every
   producer goes through the typed report entry points (coverage_report,
   mutation_report), and no third-backend consumer exists. Priced like Failure.kind all the same — a new constructor is
   a design amendment, not a convenience. Hint carries no spans by
   construction: no color in any hint. *)
type section =
  | Line of span list
  | Hint of string
  | Rows of { margin : string; columns : column list; rows : span list list }
  | Excerpt of {
      context : int;
      marker : bool;
      margin : string;
      number_width : int option;
      excerpt : excerpt;
    }
  | Rule of string option

let span_width (s : span) = Text.length_utf8 s.text

(* Column widths are the widest cell (or the caller's floor, for tables
   that align across blocks); padding sits outside a styled cell, and
   the rendered row is stripped of trailing spaces — after styling, so a
   row whose last cell styles an empty string sheds the padding before
   it, exactly as the hand-laid rows always did. *)
let put_rows k ~margin ~columns rows =
  let widths =
    List.map
      (fun (i, (c : column)) ->
        List.fold_left
          (fun w cells ->
            match List.nth_opt cells i with
            | Some cell -> max w (span_width cell)
            | None -> w)
          (Option.value c.width ~default:0)
          rows)
      (List.mapi (fun i c -> (i, c)) columns)
  in
  List.iter
    (fun cells ->
      let buf = Buffer.create 80 in
      Buffer.add_string buf margin;
      List.iteri
        (fun i cell ->
          match (List.nth_opt columns i, List.nth_opt widths i) with
          | Some c, Some width ->
              Buffer.add_string buf c.gap;
              let pad = String.make (max 0 (width - span_width cell)) ' ' in
              let text = span_str k cell in
              Buffer.add_string buf
                (match c.align with
                | `Right -> pad ^ text
                | `Left -> text ^ pad)
          | _, _ -> ())
        cells;
      put k (rstrip (Buffer.contents buf)))
    rows

let render_section k = function
  | Line spans -> put k (line_str k spans)
  | Hint line -> put k line
  | Rows { margin; columns; rows } -> put_rows k ~margin ~columns rows
  | Excerpt { context; marker; margin; number_width; excerpt = e } ->
      excerpt k ~context ~marker ~margin ?number_width e
  | Rule (Some label) -> put k (st k `Faint (labeled_rule label))
  | Rule None -> put k (st k `Faint (dashes (min columns rule_width)))

let print ~out ~ansi sections =
  let k = { out; ansi } in
  List.iter (render_section k) sections;
  Pp.flush out ()
(* Coverage (run data, rendered late)

   The one place the coverage layout lives: the [windtrap coverage]
   command's summary line and per-file report over merged files. The
   data arrives as the record below, built by the command, which holds
   the runtime — this module orders nothing and counts nothing, and it
   does not name the runtime. *)

type coverage_file = {
  file : string;
  visited : int;
  total : int;
  uncovered : int list;
  source : string option;
  stale : bool;
}

type coverage = { visited : int; total : int; files : coverage_file list }

let coverage_percentage ~visited ~total =
  if total = 0 then 100. else 100. *. float_of_int visited /. float_of_int total

(* The frozen thresholds the runtime's data has always been styled by:
   green at 80% and above, yellow at 60%, red below. *)
let coverage_style ~visited ~total : Pp.style =
  let pct = coverage_percentage ~visited ~total in
  if pct >= 80. then `Green else if pct >= 60. then `Yellow else `Red

(* The summary line of the coverage report, over the merge of every
   executable's dumps: the project number, not one process's view. *)
let coverage_line ~visited ~total () =
  [
    plain "coverage: ";
    styled
      (coverage_style ~visited ~total)
      (spf "%.1f%%" (coverage_percentage ~visited ~total));
    plain (spf " (%d/%d points)" visited total);
  ]

(* One source-excerpt block: file heading, then each uncovered region
   with a gutter marker on the uncovered lines and [·····] between
   regions. *)
let coverage_excerpt (f : coverage_file) =
  match f.source with
  | Some source when f.uncovered <> [] ->
      [
        Excerpt
          {
            context = 1;
            marker = true;
            margin = "  ";
            number_width = None;
            excerpt =
              {
                file = f.file;
                heading =
                  Some
                    [
                      styled
                        (coverage_style ~visited:f.visited ~total:f.total)
                        (spf "%.1f%%"
                           (coverage_percentage ~visited:f.visited
                              ~total:f.total));
                      plain (spf " (%d/%d)" f.visited f.total);
                    ];
                source;
                marked_lines = f.uncovered;
              };
          };
      ]
  | _ -> []

let coverage_report ~mode (c : coverage) =
  let table_row (f : coverage_file) =
    let note =
      if f.stale then
        "stale: the source changed \u{2014} re-run the instrumented tests"
      else if f.uncovered <> [] then "uncovered: " ^ bounded_ranges f.uncovered
        (* Unvisited points with no line attribution: the source was not
           found (a stale one already said so). *)
      else if f.visited < f.total then "(source not found)"
      else ""
    in
    [
      styled
        (coverage_style ~visited:f.visited ~total:f.total)
        (spf "%5.1f%%" (coverage_percentage ~visited:f.visited ~total:f.total));
      plain (string_of_int f.visited);
      plain "/";
      plain (string_of_int f.total);
      plain f.file;
      plain note;
    ]
  in
  Line (coverage_line ~visited:c.visited ~total:c.total ())
  :: Rows
       {
         margin = "  ";
         columns =
           [
             { gap = ""; align = `Right; width = None };
             { gap = "  "; align = `Right; width = None };
             { gap = ""; align = `Left; width = None };
             { gap = ""; align = `Left; width = None };
             { gap = "  "; align = `Left; width = None };
             { gap = "   "; align = `Left; width = None };
           ];
         rows = List.map table_row c.files;
       }
  :: (if mode = `Full then List.concat_map coverage_excerpt c.files else [])
(* Mutation (run data, rendered late)

   A survivor is a failure block: the same 54-column labelled rule, the
   same [  VERB  subject] head row, the same excerpt row, the same red —
   because a survivor is a defect report about a named test. An unreached
   mutant is the same block without the sentence, in yellow, because no
   test is to blame. No second failure vocabulary is invented here, and
   no ordering and no witness list is decided here: the producer hands
   over what it measured and this projects it. *)

type witness = { test : string; loc : Loc.t option; exe : string option }

(* The identifier arrives spelled: the producer holds the runtime, whose
   [id_to_string] is the canonical spelling, so this module spends none
   of the Law-12 coupling budget re-spelling it. *)
type mutant = {
  id : string;
  file : string;
  line : int;
  before : string;
  after : string;
  source : string option;
}

type survivor = { mutant : mutant; witnesses : witness list }
type scope = Suite | Selected of int | Executables of int

type mutation = {
  survivors : survivor list;
  unreached : mutant list;
  killed : int;
  scope : scope;
  filter : string option;
}

(* The command that arms a mutant under the run's selection, in the
   invocation's spelling, with the literal [<id>] where the reader pastes
   one: [--arm] under [`Exe], its WINDTRAP_MUTATE_ARM mirror under
   [`Mirrors] — the flag's own mirror, as [replay_line] spells the seed's.
   The filter is restated the way [replay_line] restates it — [-f] under
   [`Exe], [WINDTRAP_FILTER] under [`Mirrors] — because a survivor of a
   filtered run survived that selection, and the line must reproduce
   that run. Under [`Mirrors] no command is spelled at all: this renderer
   does not know how the suite is run, and the build-tool spelling that
   used to stand there was wrong in every project but the one it was
   written in. *)
let reproduce_line ~invocation ~filter =
  match (invocation, filter) with
  | `Exe cmd, Some flt ->
      spf "reproduce: %s --arm <id> -f %s" cmd (shell_quote flt)
  | `Exe cmd, None -> spf "reproduce: %s --arm <id>" cmd
  | `Mirrors, filter ->
      (* No CLI to spell, and no build tool to name: the aggregate is
         merged from executables this process never ran, and the inline
         runner is driven by a build it cannot see. The placeholder
         stands where the reader's own suite command goes — a run that
         carries the mutants and is not replayed from a cache, since an
         arming a cached run swallows appears to work and does not. *)
      spf
        "reproduce: WINDTRAP_MUTATE_ARM=<id>%s <re-run the instrumented suite>"
        (match filter with
        | Some flt -> " WINDTRAP_FILTER=" ^ shell_quote flt
        | None -> "")

let pad_to width s = String.make (max 0 (width - Text.length_utf8 s)) ' '

(* The head row and the excerpt row, shared by both block kinds. The
   excerpt is best-effort, as every excerpt is: a mutant whose source the
   producer could not read still names its line in the head row. *)
let mutant_sections ~verb ~id_width ~number_width (m : mutant) =
  let head =
    Line
      [
        plain "  ";
        verb;
        plain "  ";
        styled `Bold m.id;
        plain
          (pad_to id_width m.id ^ "   " ^ m.before ^ "  \u{2192}  " ^ m.after);
      ]
  in
  match m.source with
  | None -> [ head ]
  | Some source ->
      [
        head;
        Excerpt
          {
            context = 0;
            marker = false;
            margin = indent ^ "  ";
            number_width = Some number_width;
            excerpt =
              {
                file = m.file;
                heading = None;
                source;
                marked_lines = [ m.line ];
              };
          };
      ]

(* One witness row: the executable column only when the report has one
   ([exe_width] is [None] in a per-executable report, where every witness
   is this executable's), then the name, then the faint declaration site
   — empty for a producer that does not link the test tree. *)
let witness_row ~exe_width (w : witness) =
  (match exe_width with
    | Some _ -> [ plain (Option.value w.exe ~default:"") ]
    | None -> [])
  @ [
      plain (sanitize_name w.test);
      styled `Faint (match w.loc with Some l -> Loc.to_string l | None -> "");
    ]

let survivor_sections ~id_width ~exe_width ~witness_width ~number_width
    (s : survivor) =
  (* The sentence that is the product. A survivor always names at least
     one test (an unreached mutant is a different finding with a different
     remedy), so the empty case cannot arise from a verdict. The
     executables are counted only when the witnesses name more than one:
     a per-executable report names none, and one executable's tests are
     just tests. *)
  let witness_rows =
    match s.witnesses with
    | [] -> []
    | witnesses ->
        let n = List.length witnesses in
        let executables =
          List.length
            (List.sort_uniq compare
               (List.filter_map (fun (w : witness) -> w.exe) witnesses))
        in
        let sentence =
          if n = 1 then "1 test ran this line and did not fail:"
          else if executables > 1 then
            spf "%d tests in %d executables ran this line and none failed:" n
              executables
          else spf "%d tests ran this line and none failed:" n
        in
        let column width = { gap = ""; align = `Left; width } in
        [
          Line [];
          Line [ plain (indent ^ sentence) ];
          Rows
            {
              margin = indent ^ "  ";
              columns =
                (match exe_width with
                  | Some w -> [ column (Some w) ]
                  | None -> [])
                @ [ column (Some witness_width); column None ];
              rows = List.map (witness_row ~exe_width) witnesses;
            };
        ]
  in
  mutant_sections ~verb:(styled `Red "SURVIVED") ~id_width ~number_width
    s.mutant
  @ witness_rows

(* Zero terms are omitted, the way a passing suite prints no failure
   count: [0 survived] never prints — the clean form is its absence, the
   reached count standing alone — and neither does [0 killed] or
   [0 never reached]. The reached count is [killed + survived], a count of
   the lists and not a measurement, so the summary cannot disagree with
   the blocks above it. *)
let mutation_summary_spans (m : mutation) =
  let survived = List.length m.survivors in
  let unreached = List.length m.unreached in
  let reached =
    let n = m.killed + survived in
    match m.scope with
    | Suite -> spf "%d reached by this suite" n
    | Selected tests ->
        spf "%d reached by the %d selected test%s" n tests
          (if tests = 1 then "" else "s")
    | Executables _ -> spf "%d reached" n
  in
  let terms =
    (if survived > 0 then
       [
         [ styled `Red (spf "%d survived" survived); plain (" of " ^ reached) ];
       ]
     else [ [ plain reached ] ])
    @ (if m.killed > 0 then [ [ styled `Green (spf "%d killed" m.killed) ] ]
       else [])
    @ (if unreached > 0 then
         [ [ styled `Yellow (spf "%d never reached" unreached) ] ]
       else [])
    @
    match m.scope with
    | Executables n ->
        [ [ plain (spf "%d executable%s" n (if n = 1 then "" else "s")) ] ]
    | Suite | Selected _ -> []
  in
  let rec separated = function
    | [] -> []
    | [ last ] -> last
    | term :: rest -> term @ (plain " \u{00b7} " :: separated rest)
  in
  plain "mutants: " :: separated terms

(* Column widths are one per report, not one per block: the identifiers
   of a section, the witness names and the executables of the whole
   report, and the excerpt gutters throughout are meant to be read
   down. *)
let mutation_report ~invocation (m : mutation) =
  let widest f l = List.fold_left (fun w x -> max w (f x)) 0 l in
  let id_width_of = widest (fun (x : mutant) -> Text.length_utf8 x.id) in
  let survivor_mutants =
    List.map (fun (s : survivor) -> s.mutant) m.survivors
  in
  let number_width =
    max 1
      (widest
         (fun (x : mutant) -> String.length (string_of_int x.line))
         (survivor_mutants @ m.unreached))
  in
  let witnesses =
    List.concat_map (fun (s : survivor) -> s.witnesses) m.survivors
  in
  let witness_width =
    widest
      (fun (w : witness) -> Text.length_utf8 (sanitize_name w.test))
      witnesses
    + witness_gap
  in
  let exe_width =
    if List.exists (fun (w : witness) -> w.exe <> None) witnesses then
      Some
        (widest
           (fun (w : witness) ->
             Text.length_utf8 (Option.value w.exe ~default:""))
           witnesses
        + exe_gap)
    else None
  in
  let section label blocks =
    match blocks with
    | [] -> []
    | blocks ->
        [
          Line [];
          Rule (Some (spf "%s (%d)" label (List.length blocks)));
          Line [];
        ]
        @ List.concat
            (List.mapi
               (fun i block -> (if i > 0 then [ Line [] ] else []) @ block)
               blocks)
  in
  let survivor_part =
    let id_width = id_width_of survivor_mutants in
    section "survivors"
      (List.map
         (survivor_sections ~id_width ~exe_width ~witness_width ~number_width)
         m.survivors)
  in
  let unreached_part =
    let id_width = id_width_of m.unreached in
    section "never reached"
      (List.map
         (mutant_sections
            ~verb:(styled `Yellow "UNREACHED")
            ~id_width ~number_width)
         m.unreached)
  in
  let reported = m.survivors <> [] || m.unreached <> [] in
  survivor_part @ unreached_part
  @ (if reported then [ Line []; Rule None; Line [] ] else [])
  @ [ Line (mutation_summary_spans m) ]
  @
  if reported then [ Hint (reproduce_line ~invocation ~filter:m.filter) ]
  else []
