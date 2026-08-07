(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   The transcript layout (status lines, end-of-run failure blocks, slowest
   list) adapts windtrap v1's progress.ml, rebuilt over typed Failure
   payloads and Diff data — renderers project, never alter, run data.
  ---------------------------------------------------------------------------*)

let spf = Printf.sprintf

(* Layout constants — illustrative, not contract. *)
let duration_column = 51
let rule_width = 54
let compact_row_width = 60 (* glyphs per compact row, v1's wrap *)
let default_columns = 80
let default_tail_lines = 10
let slowest_count = 5
let slowest_threshold = 5.0 (* seconds *)
let max_diff_lines = 200
let max_proposed_lines = 20
let seq_summary_threshold = 8 (* elements *)
let seq_element_display = 40 (* code points, in the first-mismatch line *)
let indent = "    "

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

   Every command hint completes the run's rerun spelling with CLI flags:
   the driver computes the invocation once at startup and threads it here —
   no print site hard-codes an invocation, so no hint can name a command
   that would not re-run the suite. Under [`Mirrors] (the
   inline runner, and the default) hints spell [WINDTRAP_*] environment
   prefixes to [dune runtest], the only interface that exists there. No
   color in any hint. *)

type invocation = [ `Exe of string | `Mirrors ]

let accept_line = function
  | `Exe cmd -> spf "accept: %s -u, then review with git diff" cmd
  | `Mirrors ->
      "accept: WINDTRAP_UPDATE=1 dune runtest, then review with git diff"

(* [count] is a property failure's config-sourced case count
   (Failure.kind.Property): the hint restates it — [--prop-count]/
   [WINDTRAP_PROP_COUNT] — because replaying a late case needs at least as
   many cases as the failing run generated; a declaration-site count needs
   no flag and never reaches here. *)
let replay_line ?count invocation ~seed ~filter =
  let token = Seed.to_string seed in
  let flag = function Some n -> spf " --prop-count %d" n | None -> "" in
  let env = function Some n -> spf " WINDTRAP_PROP_COUNT=%d" n | None -> "" in
  match (invocation, filter) with
  | `Exe cmd, Some flt ->
      spf "replay: %s --seed %s%s -f %s" cmd token (flag count)
        (shell_quote flt)
  | `Exe cmd, None -> spf "replay: %s --seed %s%s" cmd token (flag count)
  | `Mirrors, Some flt ->
      spf "replay: WINDTRAP_SEED=%s%s WINDTRAP_FILTER=%s dune runtest" token
        (env count) (shell_quote flt)
  | `Mirrors, None ->
      spf "replay: WINDTRAP_SEED=%s%s dune runtest" token (env count)

let pp_duration secs =
  if secs >= 60. then
    (* Round to whole seconds first, or 119.6s prints as "1m60s". *)
    let total = int_of_float (Float.round secs) in
    spf "%dm%ds" (total / 60) (total mod 60)
  else if secs >= 1. then spf "%.2fs" secs
  else
    let ms = secs *. 1000. in
    if ms >= 10. then spf "%.0fms" ms else spf "%.1fms" ms

(* Wall-clock seconds for the summary line: three significant digits, but
   never scientific notation — [%.3g] alone prints [1e+03] from 999.5s up
   and [1e-05] below 0.1ms. *)
let pp_run_duration secs =
  if secs >= 999.5 then spf "%.0f" secs
  else if secs < 0.0001 then "0"
  else spf "%.3g" secs

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

(* One line, no escape codes, bounded: headline material. Stripping comes
   first so truncation cannot leave a dangling partial sequence. *)
let flat s =
  Text.truncate_utf8 60
    (String.map
       (function '\n' | '\r' | '\t' -> ' ' | c -> c)
       (Text.strip_ansi s))

let headline (f : Failure.t) =
  let base =
    match f.kind with
    | Failure.Equality { not_ = true; expected; _ } ->
        spf "both sides equal: %s" (flat expected)
    | Failure.Equality
        { claim = Failure.Contains { needle; found_at; haystack_length; _ }; _ }
      -> (
        (* The claim's own verdict, never a fake equality. *)
        match found_at with
        | Some at ->
            spf "needle %s found at byte %d" (flat (spf "%S" needle)) at
        | None ->
            spf "needle %s not found (%d-byte haystack)"
              (flat (spf "%S" needle))
              haystack_length)
    | Failure.Equality { expected; actual; _ } ->
        spf "expected %s, got %s" (flat expected) (flat actual)
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
    | Failure.Snapshot { name; state = Failure.Missing _; _ } ->
        spf "snapshot %S: no baseline" name
    | Failure.Snapshot { name; state = Failure.Mismatch _; _ } ->
        spf "snapshot %S: mismatch" name
    | Failure.Snapshot { name; state = Failure.Unresolvable; _ } ->
        spf "snapshot %S: cannot resolve a source file" name
    | Failure.Snapshot { name; state = Failure.Duplicate _; _ } ->
        spf "snapshot %S: duplicate name" name
    | Failure.Property
        { rendered; case_index; shrink_steps; timed_out; examples; _ } ->
        let desc = property_case_desc ~examples ~case_index ~shrink_steps in
        let desc =
          (* The shrink search hit the whole-test budget: the mark
             travels into the one-line summary too. *)
          match timed_out with
          | Some _ when not examples -> desc ^ ", timed out"
          | _ -> desc
        in
        spf "property failed (%s): %s" desc (flat rendered)
    | Failure.Message "" -> "(empty failure message)"
    | Failure.Message m -> flat m
  in
  match f.msg with None -> base | Some m -> spf "%s \u{2014} %s" (flat m) base

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
          | Diff.Keep s -> emit (ind ^ "  " ^ s)
          | Diff.Delete s -> emit (ind ^ st `Red ("- " ^ show_trailing_ws s))
          | Diff.Insert s -> emit (ind ^ st `Green ("+ " ^ show_trailing_ws s)))
        h.lines)
    hunks;
  if total > max_diff_lines then
    put
      (ind
      ^ st `Faint
          (spf "\u{2026} (+%d more diff lines)" (total - max_diff_lines)))

(* The element-grain summary line for two rendered sequences:
   worth a line only from [seq_summary_threshold] elements up — below that
   the ordinary diff already reads at a glance. *)
let seq_summary seq =
  match seq with
  | Some d
    when max d.Diff.expected_length d.Diff.actual_length
         >= seq_summary_threshold
         && d.Diff.differing > 0 ->
      let noun =
        match d.Diff.kind with `List -> "lists" | `Array -> "arrays"
      in
      let first =
        match d.Diff.first with
        | None -> ""
        | Some { Diff.index; expected; actual } -> (
            (* Canonical elements can still carry raw newlines (a custom pp
               inside a quoted string); the summary stays one line. *)
            let el s =
              Text.truncate_utf8 seq_element_display
                (String.map (function '\n' | '\r' -> ' ' | c -> c) s)
            in
            match (expected, actual) with
            | Some e, Some a ->
                spf "; first at [%d]: expected %s, actual %s" index (el e)
                  (el a)
            | Some e, None ->
                spf "; first at [%d]: expected %s, not in actual" index (el e)
            | None, Some a ->
                spf "; first at [%d]: actual %s, not in expected" index (el a)
            | None, None -> "" (* Diff.sequences never records an empty pair *))
      in
      if d.Diff.expected_length <> d.Diff.actual_length then
        Some
          (spf "%s differ in length: expected %d elements, actual %d%s" noun
             d.Diff.expected_length d.Diff.actual_length first)
      else
        Some
          (spf "%s differ at %d of %d elements%s" noun d.Diff.differing
             d.Diff.expected_length first)
  | _ -> None

(* The marks under an equality's two renderings, at the coarsest grain that
   applies: element alignment when both sides are sequence renderings that
   actually differ as sequences, character refinement otherwise. Sequences
   that parse but agree element-for-element differ only in the whitespace
   their printer's box inserted — refinement is what shows that, and so it
   is for a guarded sequence diff, whose empty spans say "no alignment was
   computed", not "nothing differs". *)
let eq_spans ~expected ~actual seq =
  let character () =
    match Diff.refine ~expected ~actual with
    | Some r -> Some (r.Diff.expected_spans, r.Diff.actual_spans)
    | None -> None
  in
  match seq with
  | Some d
    when d.Diff.differing > 0
         && (d.Diff.expected_spans <> [] || d.Diff.actual_spans <> []) ->
      Some (d.Diff.expected_spans, d.Diff.actual_spans)
  | _ -> character ()

let pp_eq_detail ~ansi put ~ind ~expected ~actual ~multiline seq =
  let st style s = Pp.styled_string ~ansi style s in
  if multiline then begin
    match Diff.hunks ~expected ~actual () with
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
    let marked = eq_spans ~expected ~actual seq in
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
           belong to. Both sides get one: element alignment makes a pure
           deletion ordinary, and a deletion has nothing to show on the
           actual side. Reaching here under [ansi] means [eq_spans] found
           nothing to mark, so both marker lines are empty anyway. *)
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
       it after a label would break the four-space indentation. *)
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
  else begin
    (* One sequence diff per failure, shared by the summary line and the
       marks. Spans are only ever displayed on the single-line path, and
       computing them is the expensive half (a bounded Wagner-Fischer over
       the replaced elements), so the multi-line path — where [Diff.hunks]
       does the showing — asks for the summary alone. *)
    let multiline =
      String.contains expected '\n' || String.contains actual '\n'
    in
    let seq = Diff.sequences ~spans:(not multiline) ~expected ~actual () in
    (match seq_summary seq with Some line -> put (ind ^ line) | None -> ());
    pp_eq_detail ~ansi put ~ind ~expected ~actual ~multiline seq
  end

let rec pp_gen ~ansi ~excerpt ~filter ~commands ~invocation ~ind ppf
    (f : Failure.t) =
  let st style s = Pp.styled_string ~ansi style s in
  (* Under [ansi:false] the block must contain no escape codes (render.mli):
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
  (match f.msg with Some m -> put_ind m | None -> ());
  match f.kind with
  | Failure.Equality { not_ = true; expected; _ } ->
      if String.contains expected '\n' then begin
        put_ind "both sides equal:";
        put_block expected
      end
      else put_ind (spf "both sides equal: %s" expected)
  | Failure.Equality
      {
        claim =
          Failure.Contains { needle; found_at; haystack_length; excerpt_offset };
        actual = excerpt_text;
        _;
      } ->
      (* Claim-aware containment: the block derives from the claim's
         fields — needle, verdict, byte offset, marked occurrence — never a
         fake equality diff. [actual] is the stored haystack excerpt; labels
         pad to the [expected]/[actual] 10-column gutter. *)
      let verdict =
        match found_at with
        | Some at -> spf "found at byte %d" at
        | None -> "not found"
      in
      put_ind (st `Faint "needle" ^ "    " ^ spf "%S \u{2014} %s" needle verdict);
      (* The occurrence's byte range inside the excerpt, when it is there to
         mark: a failed [not_contains] window always contains it. *)
      let occurrence =
        match found_at with
        | None -> None
        | Some at ->
            let start = at - excerpt_offset in
            let length =
              min (String.length needle) (String.length excerpt_text - start)
            in
            if start >= 0 && length > 0 then Some { Diff.start; length }
            else None
      in
      (if String.contains excerpt_text '\n' then begin
         (* Block form (the [both sides equal:] precedent): no unified diff,
            no markers; under [ansi] the occurrence highlights on its line. *)
         put_ind (st `Faint "haystack:");
         let lines = Text.split_lines excerpt_text in
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
                   highlight ~ansi `Red line
                     [ { Diff.start = start - off; length } ]
               | _ -> line
             in
             put_ind ("  " ^ styled))
           lines offsets
       end
       else
         match occurrence with
         | Some span when ansi ->
             put_ind
               (st `Faint "haystack" ^ "  "
               ^ highlight ~ansi `Red excerpt_text [ span ])
         | occurrence -> (
             put_ind (st `Faint "haystack" ^ "  " ^ excerpt_text);
             match occurrence with
             | Some span -> (
                 match marker_line excerpt_text [ span ] with
                 | Some m -> put (ind ^ "          " ^ m)
                 | None -> ())
             | None -> ()));
      (* State what was omitted, iff the excerpt is partial. *)
      if
        excerpt_offset > 0
        || excerpt_offset + String.length excerpt_text < haystack_length
      then
        put_ind
          (st `Faint
             (spf "(excerpt: bytes %d-%d of a %d-byte haystack)" excerpt_offset
                (excerpt_offset + String.length excerpt_text - 1)
                haystack_length))
  | Failure.Equality
      { claim = Failure.Satisfies | Failure.Matches; expected; actual; _ } ->
      (* Never diff or refine the claim sentence against the value:
         [expected] is a description, not a rendering. *)
      put_ind (st `Faint "expected" ^ "  " ^ expected);
      if String.contains actual '\n' then begin
        put_ind (st `Faint "actual:");
        put_block actual
      end
      else put_ind (st `Faint "actual" ^ "    " ^ actual)
  | Failure.Equality { expected; actual; _ } ->
      pp_eq ~ansi put ~ind ~expected ~actual
  | Failure.Raise
      {
        expected;
        actual;
        predicate;
        backtrace;
        same_constructor;
        expected_message;
        actual_message;
      } -> (
      (match (expected, actual) with
      | Some e, Some a -> (
          match (same_constructor, expected_message, actual_message) with
          | true, Some em, Some am when not (String.equal em am) ->
              (* Right constructor, wrong payload: diff the
                 messages instead of repeating the constructor twice. The
                 constructor's name is the rendering's prefix — Printexc
                 renders string-carrying exceptions as [Name("payload")]. *)
              let ctor =
                match String.index_opt e '(' with
                | Some j when j > 0 -> String.sub e 0 j
                | _ -> "the expected exception"
              in
              put_ind (spf "raised %s with the wrong message:" ctor);
              pp_eq ~ansi put ~ind ~expected:(spf "%S" em) ~actual:(spf "%S" am)
          | _ ->
              put_ind (st `Faint "expected exception" ^ "  " ^ e);
              put_ind (st `Faint "raised            " ^ "  " ^ a))
      | Some e, None ->
          put_ind (st `Faint "expected exception" ^ "  " ^ e);
          put_ind "but no exception was raised"
      | None, Some a ->
          (* [predicate] tells a raises_match rejection from a test body's
             escape — the two demand different reactions. *)
          put_ind
            (if predicate then
               "raised exception does not satisfy the predicate:"
             else "uncaught exception:");
          put_block a
      | None, None -> put_ind "expected an exception, but none was raised");
      match backtrace with
      | Some bt ->
          List.iter (fun l -> put_ind (st `Faint l)) (Text.split_lines bt)
      | None -> ())
  | Failure.Snapshot { name; path; state } -> (
      let accept () = if commands then put_ind (accept_line invocation) in
      match state with
      | Failure.Missing { proposed } ->
          put_ind
            (spf "snapshot %S: no baseline at %s" name (Path_ops.display path));
          let lines = Text.split_lines proposed in
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
          accept ()
      | Failure.Mismatch { expected; actual } ->
          put_ind
            (spf "snapshot %S: mismatch with %s" name (Path_ops.display path));
          pp_hunks ~ansi put ~ind (Diff.hunks ~expected ~actual ());
          accept ()
      | Failure.Unresolvable ->
          put_ind
            (spf
               "snapshot %S: cannot resolve a source file \u{2014} pass \
                ~pos:__POS__"
               name);
          if path <> "" then
            put_ind (spf "unverified path: %s" (Path_ops.display path))
      | Failure.Duplicate { first = Some first; _ } ->
          put_ind
            (spf "snapshot %S: duplicate name \u{2014} first checked at %s" name
               (Loc.to_string first))
      | Failure.Duplicate { first = None; first_test } ->
          (* Plain quotes, not %S: the joined path's UTF-8 [›] must not be
             byte-escaped. [sanitize_name] guards the line against
             control bytes exactly as on every other name surface. *)
          put_ind
            (spf "snapshot %S: duplicate name \u{2014} first checked by \"%s\""
               name (sanitize_name first_test)))
  | Failure.Property
      {
        rendered;
        case_index;
        shrink_steps;
        timed_out;
        root;
        count;
        examples;
        inner;
      } ->
      let desc = property_case_desc ~examples ~case_index ~shrink_steps in
      if String.contains rendered '\n' then begin
        put_ind (spf "counterexample (%s):" desc);
        put_block rendered
      end
      else put_ind (spf "counterexample (%s): %s" desc rendered);
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
      | None -> ());
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

(* Subtest entries are identified by their label riding the [msg] slot
   (Run.subtest): the test's leaf name, the frozen path
   separator, then the sub-case path. *)
let is_subtest_failure ~path (f : Failure.t) =
  match (List.rev path, f.Failure.msg) with
  | leaf :: _, Some msg -> String.starts_with ~prefix:(leaf ^ " \u{203a} ") msg
  | _, _ -> false

(* Renderer state *)

type t = {
  out : Format.formatter;
  ansi : bool;
  mode : [ `Quiet | `Compact | `Verbose ];
  live : bool;
  columns : int;
  tail_lines : int;
  slow_threshold : float; (* seconds; 0. disables the slow machinery *)
  invocation : invocation;
      (* the hint context: every acceptance, replay, rerun, and
         prune line derives from the one value the driver computed at
         startup. *)
  row : Buffer.t; (* styled glyphs of the current compact row *)
  mutable row_count : int; (* glyphs on the current compact row *)
  pending : Buffer.t;
      (* the buffered compact transcript — glyphs, wrap counters, notes —
         not yet committed to the sink. Printed by the first noteworthy
         event; discarded when a green, healthy run ends as one line. *)
  mutable deferred : bool;
      (* compact mode starts here: header and glyphs buffer until a
         noteworthy event (a counted failure, or an untagged test over the
         slow threshold) flushes them; false once flushed, and always false
         under [`Quiet] and [`Verbose]. *)
  mutable total_tests : int;
  mutable seen : int;
  mutable live_pending : bool;
  mutable suite : string option;
      (* recorded by [header] even in quiet mode: quiet prints no header, so
         its one-line summary carries the suite name instead — nothing may
         print without a name. The compact one-liner reuses the mechanism. *)
  mutable seed : Seed.seed option;
      (* recorded by [header] for the deferred header line and the compact
         one-liner's seed suffix. *)
}

let create ~out ~ansi ?(mode = `Compact) ?(live = false)
    ?(columns = default_columns) ?(tail_lines = default_tail_lines)
    ?(slow_threshold = 1.0) ?(invocation = `Mirrors) () =
  if columns < 20 then invalid_arg "Render.create: columns < 20";
  if tail_lines < 0 then invalid_arg "Render.create: tail_lines < 0";
  if not (Float.is_finite slow_threshold && slow_threshold >= 0.) then
    invalid_arg "Render.create: slow_threshold not finite and non-negative";
  {
    out;
    ansi;
    mode;
    live = live && ansi && mode <> `Quiet;
    columns;
    tail_lines;
    slow_threshold;
    invocation;
    row = Buffer.create 256;
    row_count = 0;
    pending = Buffer.create 256;
    deferred = mode = `Compact;
    total_tests = 0;
    seen = 0;
    live_pending = false;
    suite = None;
    seed = None;
  }

(* As [pp_gen]'s sink: with [ansi:false] escape codes arriving in test
   names or captured output are stripped, keeping the transcript clean. *)
let put t line =
  Pp.pf t.out "%s@\n" (if t.ansi then line else Text.strip_ansi line)

let st t style s = Pp.styled_string ~ansi:t.ansi style s

(* Erase the live tail. The whole line is cleared and the committed
   glyphs of the current compact row re-printed (the row buffer is empty
   in verbose mode, and nothing is committed while the compact transcript
   is deferred), so the bytes left on screen equal the pipe bytes. *)
let clear_live t =
  if t.live_pending then begin
    Pp.pf t.out "\r\027[2K%s" (if t.deferred then "" else Buffer.contents t.row);
    t.live_pending <- false
  end

(* The transcript *)

(* The header line, printed by [header] under [`Verbose] and by the first
   noteworthy event's flush under [`Compact] — from the recorded fields
   either way, so the two paths cannot drift. *)
let header_line t =
  match t.suite with
  | None -> ()
  | Some suite ->
      let seed_part =
        match t.seed with
        | None -> ""
        | Some s -> spf " (seed %s)" (Seed.to_string s)
      in
      put t
        (spf "%s: %d test%s%s" (sanitize_name suite) t.total_tests
           (if t.total_tests = 1 then "" else "s")
           seed_part)

let header t ~suite ~tests ~seed =
  t.total_tests <- tests;
  t.suite <- Some suite;
  t.seed <- seed;
  match t.mode with
  | `Quiet -> () (* the named summary line carries the suite instead *)
  | `Compact -> () (* deferred: printed by the first noteworthy event *)
  | `Verbose ->
      header_line t;
      Pp.flush t.out ()

(* The noteworthy flush (compact mode): commit the deferred header and the
   buffered glyph rows, then stream. A no-op once flushed and in the other
   modes, which never defer. *)
let flush_deferred t =
  if t.deferred then begin
    t.deferred <- false;
    header_line t;
    if Buffer.length t.pending > 0 then
      Pp.pf t.out "%s" (Buffer.contents t.pending);
    Buffer.clear t.pending;
    Pp.flush t.out ()
  end

let begin_test t ~path =
  if t.live then begin
    clear_live t;
    let name = sanitize_name (Test_tree.path_to_string path) in
    let counter = spf "[%d/%d]" (t.seen + 1) (max t.total_tests (t.seen + 1)) in
    match t.mode with
    | `Verbose ->
        let text = spf "Running %s %s\u{2026}" counter name in
        let text = Text.truncate_utf8 (t.columns - 4) text in
        Pp.pf t.out "\r\027[2K%s" (st t `Faint ("  " ^ text));
        Pp.flush t.out ();
        t.live_pending <- true
    | `Compact ->
        (* The erasable tail after the last glyph ([..F  [7/9] name…]):
           erased by [clear_live] before the next glyph or the end of the
           run, so the committed row is exactly the pipe bytes. While the
           transcript is deferred no glyph is committed, so the tail draws
           from column zero and its erasure leaves a green run's screen
           blank — the tail never forces the header out early. *)
        let base = if t.deferred then 0 else t.row_count in
        let width = t.columns - base - 1 in
        if width >= 8 then begin
          let text =
            Text.truncate_utf8 width (spf "  %s %s\u{2026}" counter name)
          in
          Pp.pf t.out "%s" (st t `Faint text);
          Pp.flush t.out ();
          t.live_pending <- true
        end
    | `Quiet -> ()
  end

let has_missing_baseline failures =
  List.exists
    (fun (f : Failure.t) ->
      match f.kind with
      | Failure.Snapshot { state = Failure.Missing _; _ } -> true
      | _ -> false)
    failures

(* "  TAG  <name><suffix>" padded so [timing] starts at a fixed column. *)
let test_line t ~tag ~style ~name ~suffix ~timing =
  let line = "  " ^ st t style tag ^ "  " ^ name ^ st t `Faint suffix in
  if timing = "" then line
  else
    let width = 4 + String.length tag + Text.length_utf8 (name ^ suffix) in
    let pad = max 2 (duration_column - width) in
    line ^ String.make pad ' ' ^ st t `Faint timing

(* One glyph, appended to the current row and — once the transcript
   flushed — committed immediately (the streaming law: a crash leaves the
   partial row visible). While deferred the same bytes accumulate in
   [pending] instead, so a later flush is byte-identical to having
   streamed. Rows wrap every [compact_row_width] glyphs with a faint
   [ [k/n]] counter when the total is known, a bare newline otherwise
   (v1 exact). *)
let emit_glyph t glyph =
  Buffer.add_string t.row glyph;
  t.row_count <- t.row_count + 1;
  if t.deferred then Buffer.add_string t.pending glyph
  else Pp.pf t.out "%s" glyph;
  if t.row_count >= compact_row_width then begin
    let counter =
      if t.total_tests > 0 then
        st t `Faint (spf " [%d/%d]" t.seen t.total_tests)
      else ""
    in
    if t.deferred then Buffer.add_string t.pending (counter ^ "\n")
    else Pp.pf t.out "%s@\n" counter;
    Buffer.clear t.row;
    t.row_count <- 0
  end;
  if not t.deferred then Pp.flush t.out ()

(* Close a partial compact row before printing full-width material. *)
let close_row t =
  if t.row_count > 0 then begin
    if t.deferred then Buffer.add_string t.pending "\n" else Pp.pf t.out "@\n";
    Buffer.clear t.row;
    t.row_count <- 0
  end

(* Run-scoped notices arrive between results (fixture releases fire after
   the last test, before [finish]), when a compact glyph row can still be
   open: close it, or the note splices into the row. While the compact
   transcript is deferred the notice buffers with the row — it prints in
   position if a noteworthy event flushes, and a green run keeps its
   one-line transcript — with an erasable live copy so a hanging fixture
   release still names itself on a terminal. *)
let note t line =
  if t.mode <> `Quiet then begin
    let line = sanitize_name line in
    clear_live t;
    close_row t;
    if t.deferred then begin
      Buffer.add_string t.pending
        ((if t.ansi then line else Text.strip_ansi line) ^ "\n");
      if t.live then begin
        Pp.pf t.out "%s" (st t `Faint (Text.truncate_utf8 (t.columns - 1) line));
        Pp.flush t.out ();
        t.live_pending <- true
      end
    end
    else begin
      put t line;
      Pp.flush t.out ()
    end
  end

(* The label-distribution table (one producer, two placements): the failure
   blocks always show it; a passing property's prints under [`Verbose] —
   the calibration view for collect/classify. *)
let pp_prop_stats t (s : Property.stats) =
  if s.collected <> [] then begin
    put t
      (indent
      ^ st t `Faint
          (spf "labels (%d passing case%s):" s.cases
             (if s.cases = 1 then "" else "s")));
    List.iter
      (fun (label, count) ->
        let line =
          if s.cases > 0 then
            spf "  %5.1f%%  %s"
              (100. *. float_of_int count /. float_of_int s.cases)
              label
          else spf "  %d  %s" count label
        in
        put t (indent ^ st t `Faint line))
      s.collected
  end;
  (* The failure headline already names the first unsatisfied requirement, so
     with a single requirement this list restates it in different words. It
     earns its place only when there are others to show alongside. *)
  if
    List.length s.coverage > 1
    && List.exists (fun c -> not c.Property.satisfied) s.coverage
  then begin
    put t (indent ^ "coverage requirements:");
    List.iter
      (fun (c : Property.cover_status) ->
        put t
          (indent
          ^ spf "  %s  %.1f%% (required %.1f%%)%s" c.label c.actual c.required
              (if c.satisfied then "" else " \u{2014} unsatisfied")))
      s.coverage
  end

(* Record-driven classification: a failing result that did
   not count is an excused expected failure — the runner's unexpected-pass
   synthesis arrives counted, so no failure message is ever inspected. *)
let counted_failure (r : Run.result) =
  match r.outcome with
  | Failure.Fail _ -> r.counted
  | Failure.Pass | Failure.Skip _ -> false

let verbose_result t (r : Run.result) =
  let name = sanitize_name (Test_tree.path_to_string r.path) in
  let timing =
    pp_duration r.duration
    ^ if r.attempts > 1 then spf " (%d attempts)" r.attempts else ""
  in
  match r.outcome with
  | Failure.Pass -> (
      put t (test_line t ~tag:"PASS" ~style:`Green ~name ~suffix:"" ~timing);
      (* A passing property with collected labels prints its distribution —
         the same [pp_prop_stats] projection as the failure blocks, so the
         bytes cannot drift. XFAIL and SKIP lines print no table. *)
      match r.prop_stats with
      | Some s when s.Property.collected <> [] -> pp_prop_stats t s
      | _ -> ())
  | Failure.Fail _ when not r.counted ->
      (* An expected failure: informational and dim. *)
      let suffix =
        match r.xfail with
        | Some { Test_tree.reason = Some reason } ->
            spf " (expected failure: %s)" reason
        | Some { Test_tree.reason = None } | None -> " (expected failure)"
      in
      put t (test_line t ~tag:"XFAIL" ~style:`Faint ~name ~suffix ~timing)
  | Failure.Fail failures ->
      let suffix =
        if has_missing_baseline failures then " \u{2014} no baseline" else ""
      in
      put t (test_line t ~tag:"FAIL" ~style:`Red ~name ~suffix ~timing)
  | Failure.Skip reason ->
      let suffix = match reason with Some r -> spf " (%s)" r | None -> "" in
      put t (test_line t ~tag:"SKIP" ~style:`Yellow ~name ~suffix ~timing:"")

let compact_glyph t (r : Run.result) =
  match r.outcome with
  | Failure.Pass -> st t `Green "."
  | Failure.Fail _ when not r.counted -> st t `Faint "x" (* expected failure *)
  | Failure.Fail _ -> st t `Red "F"
  | Failure.Skip _ -> st t `Yellow "S"

(* Over the slow threshold and not exempt: skips never count (their
   durations are not run time), tests tagged ["slow"] are exempt
   everywhere, and a zero threshold disables the machinery entirely. *)
let over_threshold t (r : Run.result) =
  t.slow_threshold > 0. && (not r.slow_tagged)
  && (match r.outcome with Failure.Skip _ -> false | _ -> true)
  && r.duration >= t.slow_threshold

let result t (r : Run.result) =
  t.seen <- t.seen + 1;
  clear_live t;
  match t.mode with
  | `Quiet -> () (* failures re-print in full at the end; nothing streams *)
  | `Compact ->
      (* The noteworthy rule: the first counted failure (an excused
         expected failure is not one), or the first completed untagged
         test over the slow threshold, commits the deferred header and
         rows; the triggering glyph and everything after stream live. *)
      if t.deferred && (counted_failure r || over_threshold t r) then
        flush_deferred t;
      emit_glyph t (compact_glyph t r)
  | `Verbose ->
      verbose_result t r;
      Pp.flush t.out ()

(* End of run *)

let labeled_rule t label =
  let w = min t.columns rule_width in
  let inner = Text.length_utf8 label + 2 in
  let left = max 2 ((w - inner) / 2) in
  let right = max 2 (w - inner - left) in
  dashes left ^ " " ^ label ^ " " ^ dashes right

let pp_tail t (tail : Failure.tail) =
  if not (tail.text = "" && tail.omitted_bytes = 0) then begin
    let lines = Text.split_lines tail.text in
    let total = List.length lines in
    let shown_count = min t.tail_lines total in
    let shown = List.filteri (fun i _ -> i >= total - shown_count) lines in
    let head =
      if tail.omitted_bytes > 0 then
        spf
          "\u{2500}\u{2500} captured output (last %d line%s, %d earlier bytes \
           omitted) \u{2500}\u{2500}"
          shown_count
          (if shown_count = 1 then "" else "s")
          tail.omitted_bytes
      else if shown_count < total then
        spf
          "\u{2500}\u{2500} captured output (last %d of %d lines) \
           \u{2500}\u{2500}"
          shown_count total
      else
        spf "\u{2500}\u{2500} captured output (%d line%s) \u{2500}\u{2500}"
          total
          (if total = 1 then "" else "s")
    in
    put t (indent ^ st t `Faint head);
    List.iter (fun l -> put t (indent ^ l)) shown;
    match tail.log_path with
    | Some p -> put t (indent ^ "full log: " ^ Path_ops.display_artifact p)
    | None -> ()
  end

let pp_block t (r : Run.result) =
  match r.outcome with
  | Failure.Pass | Failure.Skip _ -> ()
  | Failure.Fail failures -> (
      let name = Test_tree.path_to_string r.path in
      let attempts =
        if r.attempts > 1 then spf " (attempt %d of %d)" r.attempts r.attempts
        else ""
      in
      put t
        ("  " ^ st t `Red "FAIL" ^ "  "
        ^ st t `Bold (sanitize_name name)
        ^ st t `Faint attempts);
      List.iter
        (fun f ->
          pp_gen ~ansi:t.ansi ~excerpt:true ~filter:(Some name) ~commands:true
            ~invocation:t.invocation ~ind:indent t.out f)
        failures;
      (* The test drew from [srandom] and failed: print the replay line —
         the root token is in the log exactly when a stochastic failure
         needs replaying. A property failure already prints its own
         replay line from the same root; never two per block. *)
      (match r.srandom_root with
      | Some root
        when not
               (List.exists
                  (fun (f : Failure.t) ->
                    match f.kind with Failure.Property _ -> true | _ -> false)
                  failures) ->
          put t
            (indent ^ replay_line t.invocation ~seed:root ~filter:(Some name))
      | _ -> ());
      (match r.prop_stats with Some s -> pp_prop_stats t s | None -> ());
      match List.find_map (fun (f : Failure.t) -> f.output_tail) failures with
      | Some tail -> pp_tail t tail
      | None -> ())

let summary_line t ~passed ~failed ~skipped ~excused ~subtests ~duration =
  (* Quiet prints no header, and neither does a compact run still deferred
     at the end (green and healthy, the one-line transcript): the summary
     carries the suite name — nothing may print without a name. The
     deferred line also appends the root seed the header would have
     shown, so property runs stay replayable from one line. *)
  let named = t.mode = `Quiet || t.deferred in
  let prefix =
    match t.suite with
    | Some suite when named -> sanitize_name suite ^ ": "
    | _ -> ""
  in
  if passed + failed + skipped + excused = 0 then
    put t (prefix ^ "no tests ran.")
  else begin
    let passed_part =
      if passed > 0 || (failed = 0 && skipped = 0 && excused = 0) then
        [
          (if failed = 0 then st t `Green (spf "%d passed" passed)
           else spf "%d passed" passed);
        ]
      else []
    in
    let skipped_part =
      if skipped > 0 then [ spf "%d skipped" skipped ] else []
    in
    let excused_part =
      if excused > 0 then
        [
          spf "%d expected failure%s" excused (if excused = 1 then "" else "s");
        ]
      else []
    in
    let failed_part =
      if failed > 0 then
        let subtest_part =
          if subtests > 0 then
            spf " (%d subtest failure%s)" subtests
              (if subtests = 1 then "" else "s")
          else ""
        in
        [ st t `Red (spf "%d failed%s" failed subtest_part) ]
      else []
    in
    let seed_part =
      match t.seed with
      | Some s when t.deferred -> spf " (seed %s)" (Seed.to_string s)
      | _ -> ""
    in
    put t
      (prefix
      ^ String.concat ", "
          (passed_part @ skipped_part @ excused_part @ failed_part)
      ^ spf " in %ss%s." (pp_run_duration duration) seed_part)
  end

(* The slow warnings (spec: after the row and the failure blocks, before
   the summary): a labelled block in the shape of the failures block and
   the slowest list, rather than bare lines at column zero — a heading
   carrying the count, then one indented entry per test with the duration
   in a right-aligned leading column, so the paths line up and the
   durations can be read down. Slowest first: with several over the
   threshold, the top one is the one worth acting on, and execution order
   says nothing a reader wants here.

   The hint names the interface the run actually has, like every other
   hint — the inline runner has no CLI to offer a flag from. *)
let slow_warnings t slow_results =
  let rendered =
    List.map
      (fun (r : Run.result) ->
        (pp_duration r.duration, sanitize_name (Test_tree.path_to_string r.path)))
      (List.sort
         (fun (a : Run.result) (b : Run.result) ->
           Float.compare b.duration a.duration)
         slow_results)
  in
  (* [pp_duration] is ASCII, so byte length is display width. *)
  let width =
    List.fold_left (fun w (d, _) -> max w (String.length d)) 0 rendered
  in
  let warn s = put t (st t `Faint (st t `Yellow s)) in
  warn (spf "slow tests (%d):" (List.length rendered));
  List.iter (fun (d, path) -> warn (spf "  %*s  %s" width d path)) rendered;
  put t
    (st t `Faint
       (match t.invocation with
       | `Exe _ ->
           "(exempt with the \"slow\" tag, or raise --slow-threshold SECONDS)"
       | `Mirrors ->
           "(exempt with the \"slow\" tag, or raise WINDTRAP_SLOW_THRESHOLD)"))

let slowest t results =
  let timed =
    List.filter
      (fun (r : Run.result) ->
        match r.outcome with Failure.Skip _ -> false | _ -> true)
      results
  in
  let total =
    List.fold_left (fun acc (r : Run.result) -> acc +. r.duration) 0. timed
  in
  if total >= slowest_threshold && List.length timed >= slowest_count then begin
    let rendered =
      List.map
        (fun (r : Run.result) ->
          ( pp_duration r.duration,
            sanitize_name (Test_tree.path_to_string r.path) ))
        (take slowest_count
           (List.sort
              (fun (a : Run.result) (b : Run.result) ->
                Float.compare b.duration a.duration)
              timed))
    in
    (* Same duration column as the slow-tests block, which prints a few
       lines above this one in a verbose run: two lists of (duration, path)
       that align differently read as a mistake. *)
    let width =
      List.fold_left (fun w (d, _) -> max w (String.length d)) 0 rendered
    in
    put t "";
    put t (st t `Faint "slowest tests:");
    List.iter
      (fun (d, path) -> put t (st t `Faint (spf "  %*s  %s" width d path)))
      rendered
  end

(* Coverage (run data, rendered late)

   The one place the coverage layout lives: [finish]'s inline line, the
   [WINDTRAP_COVERAGE]/[--coverage] report and full modes, and — through
   [Private] — the [windtrap coverage] command, which renders the same
   [coverage_report] over merged files so the two reports cannot drift. *)

let rstrip s =
  let n = ref (String.length s) in
  while !n > 0 && s.[!n - 1] = ' ' do
    decr n
  done;
  String.sub s 0 !n

(* [style] is the runtime's frozen thresholds: green >= 80, yellow >= 60. *)
let pct_styled t (s : Windtrap_coverage.summary) =
  st t
    (Windtrap_coverage.style s :> Pp.style)
    (spf "%.1f%%" (Windtrap_coverage.percentage s))

let coverage_line t ?(note = "") ?hint (s : Windtrap_coverage.summary) =
  let note = if note = "" then "" else ", " ^ note in
  let hint = match hint with None -> "" | Some h -> " \u{00b7} " ^ h in
  put t
    (spf "coverage: %s (%d/%d points%s)%s" (pct_styled t s) s.visited s.total
       note hint)

(* One source-excerpt block: file heading, then each uncovered region with
   a gutter marker on the uncovered lines and [·····] between regions. *)
let coverage_excerpts t (r : Windtrap_coverage.file_report) =
  match r.source with
  | Some source when r.uncovered_lines <> [] ->
      put t "";
      put t
        (spf "%s \u{2014} %s (%d/%d)" r.file (pct_styled t r.summary)
           r.summary.visited r.summary.total);
      put t "";
      let regions = Windtrap_coverage.excerpts ~source r.uncovered_lines in
      let number_width =
        List.fold_left
          (List.fold_left (fun w (l : Windtrap_coverage.excerpt_line) ->
               max w (String.length (string_of_int l.number))))
          4 regions
      in
      List.iteri
        (fun i region ->
          if i > 0 then put t "   \u{00b7}\u{00b7}\u{00b7}\u{00b7}\u{00b7}";
          List.iter
            (fun (l : Windtrap_coverage.excerpt_line) ->
              let gutter =
                if l.uncovered then st t `Red "  \u{258c}" else "   "
              in
              put t
                (rstrip
                   (spf "%s%*d \u{2502} %s" gutter number_width l.number l.text)))
            region)
        regions
  | _ -> ()

let coverage_report t ?source_roots ~mode collection =
  if t.mode <> `Quiet then begin
    clear_live t;
    close_row t;
    coverage_line t (Windtrap_coverage.summary collection);
    let reports = Windtrap_coverage.file_reports ?source_roots collection in
    let width f =
      List.fold_left (fun w r -> max w (String.length (f r))) 0 reports
    in
    let visited_width =
      width (fun (r : Windtrap_coverage.file_report) ->
          string_of_int r.summary.visited)
    and total_width =
      width (fun (r : Windtrap_coverage.file_report) ->
          string_of_int r.summary.total)
    and file_width =
      width (fun (r : Windtrap_coverage.file_report) -> r.file)
    in
    List.iter
      (fun (r : Windtrap_coverage.file_report) ->
        let pct =
          st t
            (Windtrap_coverage.style r.summary :> Pp.style)
            (spf "%5.1f%%" (Windtrap_coverage.percentage r.summary))
        in
        let note =
          if r.stale then
            "stale: the source changed \u{2014} re-run the instrumented tests"
          else if r.uncovered_lines <> [] then
            "uncovered: "
            ^ Windtrap_coverage.format_ranges
                (Windtrap_coverage.collapse_ranges r.uncovered_lines)
          else if r.uncovered_extents <> [] then "(source not found)"
          else ""
        in
        put t
          (rstrip
             (spf "  %s  %*d/%-*d  %-*s   %s" pct visited_width
                r.summary.visited total_width r.summary.total file_width r.file
                note)))
      reports;
    if mode = `Full then List.iter (coverage_excerpts t) reports
  end

let finish t ?coverage ~results ~duration () =
  clear_live t;
  let failed_results, excused_results =
    List.partition counted_failure
      (List.filter
         (fun (r : Run.result) ->
           match r.outcome with Failure.Fail _ -> true | _ -> false)
         results)
  in
  let count p = List.length (List.filter p results) in
  let passed =
    count (fun (r : Run.result) ->
        match r.outcome with Failure.Pass -> true | _ -> false)
  in
  let skipped =
    count (fun (r : Run.result) ->
        match r.outcome with Failure.Skip _ -> true | _ -> false)
  in
  let failed = List.length failed_results in
  let subtests =
    List.fold_left
      (fun acc (r : Run.result) ->
        match r.outcome with
        | Failure.Fail fs ->
            acc + List.length (List.filter (is_subtest_failure ~path:r.path) fs)
        | _ -> acc)
      0 failed_results
  in
  let slow_results = List.filter (over_threshold t) results in
  if t.deferred && failed = 0 && slow_results = [] then
    (* Green and healthy: the deferred header and rows are discarded and
       the whole transcript is the one named summary line. *)
    summary_line t ~passed ~failed ~skipped
      ~excused:(List.length excused_results)
      ~subtests ~duration
  else begin
    flush_deferred t;
    close_row t;
    if failed > 0 then begin
      put t (st t `Faint (labeled_rule t (spf "failures (%d)" failed)));
      (* One blank line between blocks, none inside the run: a block is the
         unit a reader scans for, and the only other break in this region —
         between a block's location and its detail — must not read as loud
         as the boundary between two failures. *)
      List.iteri
        (fun i r ->
          if i > 0 then put t "";
          pp_block t r)
        failed_results;
      put t (st t `Faint (dashes (min t.columns rule_width)));
      put t ""
    end;
    if t.mode <> `Quiet && slow_results <> [] then begin
      slow_warnings t slow_results;
      put t ""
    end;
    summary_line t ~passed ~failed ~skipped
      ~excused:(List.length excused_results)
      ~subtests ~duration;
    (if failed > 0 then
       (* The rerun hint derives from the stored invocation: under
          [`Mirrors] no hint prints — [--failed] has no environment mirror,
          and a hint that cannot be followed is worse than none. *)
       match t.invocation with
       | `Exe cmd -> put t (spf "rerun failures only: %s --failed" cmd)
       | `Mirrors -> ());
    (* Diagnosis, not signal: the slowest list is verbose-only. *)
    if t.mode = `Verbose then slowest t results
  end;
  (* The sibling fact rides the summary record (the driver read it at
     snapshot time): siblings mean this number is
     one executable's view of the code it links, and the project number
     is the merge — the line says so instead of posing as the total. *)
  (match coverage with
  | Some { Run.visited; total; siblings } when t.mode <> `Quiet ->
      let s = { Windtrap_coverage.visited; total } in
      if siblings then
        coverage_line t ~note:"this executable"
          ~hint:"project: dune build @cover" s
      else coverage_line t ~hint:"WINDTRAP_COVERAGE=report for detail" s
  | _ -> ());
  Pp.flush t.out ()
