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
let indent = "    "

(* Gap between a survivor witness's name and its declaration site. Wide,
   like the verbose status line's duration column and unlike the tight
   table gutters: test names vary enough in length that a two-space gap
   reads as a ragged wall. *)
let witness_gap = 6

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

(* The resolved renderer settings: the four presentation knobs the CLI
   layer resolves and the runner never reads. The driver applies them —
   [color] against the sink's terminal status, the rest as [create]'s
   arguments — so the split between run configuration and presentation
   is a type boundary, not a discipline. *)
type settings = {
  color : Env.color_mode;
  tail_errors : int option;
  slow_threshold : float;
}

let default_settings =
  { color = Env.Auto; tail_errors = None; slow_threshold = 1.0 }

let accept_line = function
  | `Exe cmd -> spf "accept: %s -u, then review with git diff" cmd
  | `Mirrors ->
      "accept: WINDTRAP_UPDATE=1 dune runtest, then review with git diff"

(* [count] and [max_shrink] are a property failure's config-sourced knobs
   (Failure.kind.Property): the hint restates both — [--prop-count]/
   [WINDTRAP_PROP_COUNT] because replaying a late case needs at least as
   many cases as the failing run generated, [--max-shrink]/
   [WINDTRAP_MAX_SHRINK] because a shrink search under a different budget
   stops elsewhere and reports a different counterexample. A
   declaration-site count needs no flag and never reaches here; neither
   does an engine-default budget. *)
let replay_line ?count ?max_shrink invocation ~seed ~filter =
  let token = Seed.to_string seed in
  let opt spelling = function
    | Some n -> spf " %s %d" spelling n
    | None -> ""
  in
  let mirror name = function Some n -> spf " %s=%d" name n | None -> "" in
  let flags = opt "--prop-count" count ^ opt "--max-shrink" max_shrink in
  let env =
    mirror "WINDTRAP_PROP_COUNT" count ^ mirror "WINDTRAP_MAX_SHRINK" max_shrink
  in
  match (invocation, filter) with
  | `Exe cmd, Some flt ->
      spf "replay: %s --seed %s%s -f %s" cmd token flags (shell_quote flt)
  | `Exe cmd, None -> spf "replay: %s --seed %s%s" cmd token flags
  | `Mirrors, Some flt ->
      spf "replay: WINDTRAP_SEED=%s%s WINDTRAP_FILTER=%s dune runtest" token env
        (shell_quote flt)
  | `Mirrors, None -> spf "replay: WINDTRAP_SEED=%s%s dune runtest" token env

(* The stale-baseline lines: the offending files, then the removal under
   them. This hint is not spelled from the invocation like the others,
   because it does not re-run anything — a baseline is a committed file,
   so the report hands over the exact [rm] and leaves the edit to the
   user. *)
let stale_lines orphans =
  match orphans with
  | [] -> []
  | _ ->
      let paths = List.map Path_ops.display orphans in
      List.map (spf "stale baseline: %s") paths
      @ [
          spf "remove them: rm %s"
            (String.concat " " (List.map shell_quote paths));
        ]

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

(* Comparison surfaces print the values a test produced, and a control byte
   in one of those drives the terminal instead of appearing in the report:
   ESC eats the label beside it and leaves the terminal coloured, CR
   overwrites the line the reader needed, and a grep for the reported value
   finds nothing. Every C0 byte and DEL therefore renders as a lowercase
   [\xNN] escape — one rule, no mnemonics, so [\x] marks every escape a
   reader sees — with LF and TAB the two exceptions, because line structure
   and indentation ARE the layout the block is built from.

   This is a projection, exactly like colour: equality, containment, and
   snapshot storage never see it. The transform is not injective — a value
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
    | Failure.Snapshot { name; state = Failure.Missing _; _ } ->
        spf "snapshot %S: no baseline" name
    | Failure.Snapshot { name; state = Failure.Mismatch _; _ } ->
        spf "snapshot %S: mismatch" name
    | Failure.Snapshot { name; state = Failure.Unresolvable; _ } ->
        spf "snapshot %S: cannot resolve a source file" name
    | Failure.Snapshot { name; state = Failure.Duplicate _; _ } ->
        spf "snapshot %S: duplicate name" name
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
  if String.contains expected '\n' || String.contains actual '\n' then begin
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
  | Failure.Snapshot { name; path; state } -> (
      let accept () = if commands then put_ind (accept_line invocation) in
      match state with
      | Failure.Missing { proposed } ->
          put_ind
            (spf "snapshot %S: no baseline at %s" name (Path_ops.display path));
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
        shrink_exhausted;
        timed_out;
        root;
        count;
        max_shrink;
        examples;
        printerless;
        inner;
      } ->
      let desc = property_case_desc ~examples ~case_index ~shrink_steps in
      let rendered = show_controls rendered in
      if String.contains rendered '\n' then begin
        put_ind (spf "counterexample (%s):" desc);
        put_block rendered
      end
      else put_ind (spf "counterexample (%s): %s" desc rendered);
      (* The line above is a placeholder, not the value. Say so once, here,
         where the reader is looking at it — whichever placeholder shape the
         engine produced. *)
      if printerless then
        put_ind
          (st `Faint
             "(this generator has no printer \u{2014} attach one with \
              Gen.with_pp to see the value)");
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
        put_ind (replay_line ?count ?max_shrink invocation ~seed:root ~filter)
  | Failure.Message "" -> put_ind "(empty failure message)"
  | Failure.Message m ->
      List.iter (fun line -> put_ind line) (Text.split_lines m)

let pp_failure ~ansi ?(excerpt = false) ?filter ?(invocation = `Mirrors) ppf f =
  pp_gen ~ansi ~excerpt ~filter ~commands:true ~invocation ~ind:indent ppf f

(* Sub-case entries carry their identity as data (Run.subtest fills the
   [subtest] components); the msg text is never consulted. *)
let is_subtest_failure (f : Failure.t) = f.Failure.subtest <> []

(* Renderer state *)

type t = {
  out : Format.formatter;
  ansi : bool;
  mode : [ `Compact | `Verbose ];
  live : bool;
  columns : int;
  tail_lines : int;
  slow_threshold : float; (* seconds; 0. disables the slow machinery *)
  invocation : invocation;
      (* the hint context: every acceptance, replay and rerun line derives
         from the one value the driver computed at startup. *)
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
         under [`Verbose]. *)
  mutable total_tests : int;
  mutable seen : int;
  mutable live_pending : bool;
  mutable declared : int option;
      (* tests the suite declares, before selection — the denominator the
         empty-selection message needs; [total_tests] is what survived.
         [None] until [header] runs: an embedder that renders results
         without one gets the bare wording rather than a guess. *)
  mutable selection : string option;
      (* the active selection, described by the driver (which owns the
         config), used only to say why nothing ran. *)
  mutable suite : string option;
      (* recorded by [header] so that a compact run still deferred at the
         end can name itself in its one-line summary — nothing may print
         without a name. *)
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
    live = live && ansi;
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
    declared = None;
    selection = None;
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

let header t ~suite ~tests ?declared ?selection ~seed () =
  t.total_tests <- tests;
  t.declared <- Some (Option.value declared ~default:tests);
  t.selection <- selection;
  t.suite <- Some suite;
  t.seed <- seed;
  match t.mode with
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
  (* The failure headline already names every label that was never covered,
     so this list earns its place only by showing the ones that were —
     which is the question a reader asks next. *)
  if
    List.length s.coverage > 1
    && List.exists (fun c -> not c.Property.satisfied) s.coverage
  then begin
    put t (indent ^ "covered labels:");
    List.iter
      (fun (c : Property.cover_status) ->
        put t
          (indent
          ^ spf "  %s  %d%s" c.label c.hits
              (if c.satisfied then "" else " \u{2014} never covered")))
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
      (* One test can fail more than once — sibling subtests, or a body and
         its teardown, which report independently. Separate them, for the
         same reason blocks are separated: without it the only break inside
         a block falls between a failure's location and its detail, which
         reads as a boundary where there is none and hides the one that is
         actually there. *)
      List.iteri
        (fun i f ->
          if i > 0 then put t "";
          pp_gen ~ansi:t.ansi ~excerpt:true ~filter:(Some name) ~commands:true
            ~invocation:t.invocation ~ind:indent t.out f)
        failures;
      (match r.prop_stats with Some s -> pp_prop_stats t s | None -> ());
      match List.find_map (fun (f : Failure.t) -> f.output_tail) failures with
      | Some tail -> pp_tail t tail
      | None -> ())

(* The summary counts REPORTED RESULTS, which is not the header's count of
   selected tests: a failing fixture release is recorded as a verdict row
   after the header printed (Run.Fixture_release), so a one-test suite
   whose release raises reads "1 test" above and "1 passed, 1 failed" below.
   The two are answering different questions — what will run, what came
   back — and the extra row names itself in the block directly above, under
   a [release] phase tag. Dropping such a row from [failed] to make the
   arithmetic close would be the real defect: the run failed, and the
   summary would then disagree with the exit code. *)
let summary_line t ~passed ~failed ~skipped ~excused ~subtests ~duration =
  (* A compact run still deferred at the end (green and healthy, the
     one-line transcript) printed no header: the summary carries the suite
     name — nothing may print without a name. That line also appends the
     root seed the header would have shown, so property runs stay
     replayable from one line. *)
  let prefix =
    match t.suite with
    | Some suite when t.deferred -> sanitize_name suite ^ ": "
    | _ -> ""
  in
  if passed + failed + skipped + excused = 0 then begin
    (* Exit 2 either way, but the two causes call for different sentences:
       a suite with nothing in it is not a mistyped filter, and neither is
       a shard that legitimately drew an empty bucket. Naming the selection
       and the denominator is what turns a dead end into a next step. *)
    let reason =
      match (t.declared, t.selection) with
      | Some 0, _ -> Some "the suite declares none"
      | Some declared, Some selection ->
          Some
            (spf "%s matched none of %d test%s" selection declared
               (if declared = 1 then "" else "s"))
      | Some _, None | None, _ -> None
    in
    match reason with
    | None -> put t (prefix ^ "no tests ran.")
    | Some reason ->
        put t (spf "%sno tests ran: %s." prefix reason);
        if t.declared <> Some 0 then
          put t (st t `Faint "(list the suite's tests with -l)")
  end
  else begin
    let passed_part =
      if passed > 0 || (failed = 0 && skipped = 0 && excused = 0) then
        [
          (if failed = 0 then st t `Green (spf "%d passed" passed)
           else spf "%d passed" passed);
        ]
      else []
    in
    (* The counts wear the glyph row's colours: the row and the summary
       describe the same run, so one convention across both — green pass,
       red fail, yellow skip, faint excused — beats two. *)
    let skipped_part =
      if skipped > 0 then [ st t `Yellow (spf "%d skipped" skipped) ] else []
    in
    let excused_part =
      if excused > 0 then
        [
          st t `Faint
            (spf "%d expected failure%s" excused
               (if excused = 1 then "" else "s"));
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

(* Report sections (subsystem-neutral)

   The one vocabulary instrumentation reports are made of: styled lines,
   hint lines, aligned rows, source excerpts, and the failure section's
   rules. Coverage's per-file table and mutation's survivor blocks are
   two projections into it, and Render draws it knowing nothing about
   the runtimes that measured the data — the subsystem that owns the
   numbers builds section data, and every name the runtime owns (a
   mutant identifier, the arming variable) arrives pre-spelled with the
   runtime's own functions. Law 12: a second copy of any of these
   drawers is exactly the drift [bin/dune]'s own comment says the
   coverage command's structure exists to prevent. *)

let rstrip s =
  let n = ref (String.length s) in
  while !n > 0 && s.[!n - 1] = ' ' do
    decr n
  done;
  String.sub s 0 !n

type span = { style : Pp.style option; text : string }

let plain text = { style = None; text }
let styled style text = { style = Some style; text }

let span_str t { style; text } =
  match style with None -> text | Some style -> st t style text

let line_str t spans = String.concat "" (List.map (span_str t) spans)

type column = { gap : string; align : [ `Left | `Right ]; width : int option }

(* Line numbers as ranges ([88-94, 121]) — one dialect for the coverage
   table's uncovered lists and the mutation report's unreached list.
   Moved here from the coverage runtime with [excerpts]: layout lives
   with the vocabulary, not with the instrumentation that measured the
   lines. *)

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

let ranges lines = format_ranges (collapse_ranges lines)

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

let excerpt t ?(context = 1) ?(marker = true) ?(margin = "  ") ?number_width e =
  (match e.heading with
  | None -> ()
  | Some h ->
      put t "";
      put t (spf "%s \u{2014} %s" e.file (line_str t h));
      put t "");
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
    else if marked then st t `Red (margin ^ "\u{258c}")
    else margin ^ " "
  in
  let separator =
    (if marker then margin ^ " " else margin)
    ^ "\u{00b7}\u{00b7}\u{00b7}\u{00b7}\u{00b7}"
  in
  List.iteri
    (fun i region ->
      if i > 0 then put t separator;
      List.iter
        (fun l ->
          put t
            (rstrip (spf "%s%*d \u{2502} %s" (gutter l.marked) width l.number
                       l.text)))
        region)
    regions

(* The subsystem-neutral report-section vocabulary. Internal: every
   producer goes through the typed report entry points (coverage_report,
   mutation_report, admission_report), and no third-backend consumer
   exists. Priced like Failure.kind all the same — a new constructor is
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
let put_rows t ~margin ~columns rows =
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
              let text = span_str t cell in
              Buffer.add_string buf
                (match c.align with
                | `Right -> pad ^ text
                | `Left -> text ^ pad)
          | _, _ -> ())
        cells;
      put t (rstrip (Buffer.contents buf)))
    rows

let render_section t = function
  | Line spans -> put t (line_str t spans)
  | Hint line -> put t line
  | Rows { margin; columns; rows } -> put_rows t ~margin ~columns rows
  | Excerpt { context; marker; margin; number_width; excerpt = e } ->
      excerpt t ~context ~marker ~margin ?number_width e
  | Rule (Some label) -> put t (st t `Faint (labeled_rule t label))
  | Rule None -> put t (st t `Faint (dashes (min t.columns rule_width)))

let sections t l = List.iter (render_section t) l

(* Coverage (run data, rendered late)

   The one place the coverage layout lives: [finish]'s inline line, the
   [WINDTRAP_COVERAGE]/[--coverage] report and full modes, and — through
   [Private] — the [windtrap coverage] command, which renders the same
   [coverage_report] over merged files so the two reports cannot drift.
   The data arrives as the record below, built at the coverage seam
   ([Driver.coverage_data]) — this module orders nothing and counts
   nothing, and it no longer names the runtime. *)

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
  if total = 0 then 100.
  else 100. *. float_of_int visited /. float_of_int total

(* The frozen thresholds the runtime's data has always been styled by:
   green at 80% and above, yellow at 60%, red below. *)
let coverage_style ~visited ~total : Pp.style =
  let pct = coverage_percentage ~visited ~total in
  if pct >= 80. then `Green else if pct >= 60. then `Yellow else `Red

(* The one producer of the coverage line, shared by [finish]'s inline
   form (which points at the project aggregate) and [coverage_report]'s
   bare form, which already is the aggregate. *)
let coverage_line ?hint ~visited ~total () =
  let hint = match hint with None -> "" | Some h -> " \u{00b7} " ^ h in
  [
    plain "coverage: ";
    styled
      (coverage_style ~visited ~total)
      (spf "%.1f%%" (coverage_percentage ~visited ~total));
    plain (spf " (%d/%d points)%s" visited total hint);
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

let coverage_sections ~mode (c : coverage) =
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

let coverage_report t ~mode c =
  clear_live t;
  close_row t;
  sections t (coverage_sections ~mode c)

(* Mutation (run data, rendered late)

   A survivor is a failure block: the same 54-column labelled rule, the
   same [  VERB  subject] head row, the same excerpt row, the same red —
   because a survivor is a defect report about a named test. No second
   failure vocabulary is invented here, and no ordering, no cap and no
   witness list is decided here: the loop hands over what it measured and
   this projects it. *)

type witness = { test : string; loc : Loc.t option }

(* The identifier arrives spelled: the loop holds the runtime, whose
   [id_to_string] is the canonical spelling, so this module spends none
   of the Law-12 coupling budget re-spelling it — as the admission
   report's fault record always worked. *)
type survivor = {
  id : string;
  file : string;
  line : int;
  before : string;
  after : string;
  source : string option;
  witnesses : witness list;
}

type unreached = { file : string; lines : int list }

type mutation = {
  arm_variable : string;
  survivors : survivor list;
  unreached : unreached list;
  unreached_total : int;
  killed : int;
  total : int;
  duration : float option;
  seed : Seed.seed option;
  siblings : bool;
}

(* The command that arms this one mutant, in the invocation's spelling —
   the variable's name arrives on the record, spelled by the loop with
   the runtime's own function, so the report and the runtime cannot
   disagree about what to type. Under [`Mirrors] the instrumentation
   flag is part of the spelling: arming needs a build that carries the
   mutants, and a bare [dune runtest] builds one that does not, so the
   plain mirror would name a command that cannot do what its line
   says. *)
let arm_command t ~variable id =
  match t.invocation with
  | `Exe cmd -> spf "%s=%s %s" variable id cmd
  | `Mirrors ->
      (* [--force] is not decoration. Dune does not key an action's digest
         on an ambient variable it was not told about, so a warm tree
         replays the cached run and the arming silently does nothing —
         a hint that appears to work and does not is worse than none. *)
      spf "%s=%s dune runtest --force --instrument-with ppx_windtrap.mutate"
        variable id

let pad_to width s = String.make (max 0 (width - Text.length_utf8 s)) ' '

let survivor_sections t ~variable ~id_width ~witness_width ~number_width
    (s : survivor) =
  let head =
    Line
      [
        plain "  ";
        styled `Red "SURVIVED";
        plain "  ";
        styled `Bold s.id;
        plain
          (pad_to id_width s.id ^ "   " ^ s.before ^ "  \u{2192}  " ^ s.after);
      ]
  in
  (* Best-effort, as every excerpt is: a survivor whose source the loop
     could not read still names its line in the head row. *)
  let excerpt_row =
    match s.source with
    | Some source ->
        [
          Excerpt
            {
              context = 0;
              marker = false;
              margin = indent ^ "  ";
              number_width = Some number_width;
              excerpt =
                { file = s.file; heading = None; source;
                  marked_lines = [ s.line ] };
            };
        ]
    | None -> []
  in
  (* The sentence that is the product. A survivor always names at least
     one test (an unreached mutant is a different finding with a different
     remedy), so the empty case cannot arise from a verdict. *)
  let witness_rows =
    match s.witnesses with
    | [] -> []
    | witnesses ->
        let n = List.length witnesses in
        [
          Line
            [
              plain
                (indent
                ^
                if n = 1 then
                  "1 test ran this line and did not fail when it changed:"
                else
                  spf "%d tests ran this line and none failed when it changed:"
                    n);
            ];
          Rows
            {
              margin = indent ^ "  ";
              columns =
                [
                  { gap = ""; align = `Left; width = Some witness_width };
                  { gap = ""; align = `Left; width = None };
                ];
              rows =
                List.map
                  (fun w ->
                    [
                      plain (sanitize_name w.test);
                      styled `Faint
                        (match w.loc with
                        | Some l -> Loc.to_string l
                        | None -> "");
                    ])
                  witnesses;
            };
          Line [];
        ]
  in
  (* No color in either hint, as everywhere else, and both are one line a
     reader copies whole. *)
  (head :: excerpt_row)
  @ (Line [] :: witness_rows)
  @ [
      Hint (indent ^ spf "%-9s%s" "arm" (arm_command t ~variable s.id));
      Hint
        (indent ^ spf "%-9s((%s) [@mutate off \"reason\"])" "dismiss" s.before);
    ]

let mutation_summary_spans (m : mutation) =
  let total = List.length m.survivors in
  let survived =
    styled (if total = 0 then `Green else `Red) (spf "%d survived" total)
  in
  (* Zero terms are omitted, the way a passing suite prints no failure
     count: a run with nothing to report is one line. *)
  let terms =
    (if m.killed > 0 then [ plain (spf "%d killed" m.killed) ] else [])
    @
    if m.unreached_total > 0 then
      [ styled `Yellow (spf "%d unreached" m.unreached_total) ]
    else []
  in
  let rec separated = function
    | [] -> []
    | [ last ] -> [ last ]
    | term :: rest -> term :: plain ", " :: separated rest
  in
  [
    plain "mutants: ";
    survived;
    plain
      (spf " of %d%s" m.total
         (if m.siblings then " (this executable)" else ""));
  ]
  @ (if terms = [] then [] else plain " \u{00b7} " :: separated terms)
  @ [
      plain
        ((match m.duration with
         | Some d -> spf " in %s" (pp_duration d)
         | None -> "")
        ^ (match m.seed with
          | Some s -> spf " (seed %s)" (Seed.to_string s)
          | None -> "")
        ^ if m.siblings then " \u{00b7} project: dune build @mutate" else "");
    ]

(* The discovery line, and the two lines an armed run is owed.

   The discovery line is the mutation half of the coverage line's
   discoverability shape: what the instrumentation found, then the one
   spelling that asks it to do something. It prints only when there is
   something to discover; the armed announcement prints unconditionally,
   because Law 16(b) makes it the guarantee that a run whose output does
   not say so has no mutant armed. *)

let mutation_discovery t ~mutants ~files =
  if mutants > 0 then begin
    clear_live t;
    close_row t;
    put t
      (spf "mutants: %d in %d file%s \u{00b7} WINDTRAP_MUTATE=1 to test them"
         mutants files
         (if files = 1 then "" else "s"))
  end

let mutation_armed t ~id ~before ~after =
  clear_live t;
  close_row t;
  put t (spf "mutant %s armed: %s \u{2192} %s" (st t `Bold id) before after)

let mutation_killed t =
  clear_live t;
  close_row t;
  put t (st t `Green "mutant killed.")

let mutation_survived t ~hits =
  clear_live t;
  close_row t;
  put t
    (st t `Red
       (spf
          "mutant survived: the armed site was evaluated %d time(s) and no \
           test failed."
          hits))

let mutation_not_evaluated t =
  clear_live t;
  close_row t;
  put t (st t `Yellow "mutant not evaluated: no selected test ran the site.")

let mutation_not_saved t =
  clear_live t;
  close_row t;
  put t
    (st t `Yellow
       "verdicts not saved: this run's selection narrows the suite, and a \
        partial run's verdicts would stand in the project merge as the whole.")

let mutation_forced_fail t ~id ~tests =
  clear_live t;
  close_row t;
  put t
    (st t `Yellow
       (spf
          "arming %s changed nothing: %d test(s) ran it and none failed. If \
           the library under test was not built with --instrument-with \
           ppx_windtrap.mutate, every number below is about this executable's \
           own mutants."
          id tests))

let mutation_sections t (m : mutation) =
  let survivor_part =
    match m.survivors with
    | [] -> []
    | survivors ->
        let label = spf "survivors (%d)" (List.length survivors) in
        let id_width =
          List.fold_left
            (fun w (s : survivor) -> max w (Text.length_utf8 s.id))
            0 survivors
        in
        (* One column across the whole report, not one per block: the
           locations are meant to be read down. *)
        let witness_width =
          List.fold_left
            (fun w (s : survivor) ->
              List.fold_left
                (fun w (wit : witness) ->
                  max w (Text.length_utf8 (sanitize_name wit.test)))
                w s.witnesses)
            0 survivors
          + witness_gap
        in
        let number_width =
          List.fold_left
            (fun w (s : survivor) ->
              max w (String.length (string_of_int s.line)))
            1 survivors
        in
        [ Line []; Rule (Some label); Line [] ]
        @ List.concat
            (List.mapi
               (fun i s ->
                 (if i > 0 then [ Line [] ] else [])
                 @ survivor_sections t ~variable:m.arm_variable ~id_width
                     ~witness_width ~number_width s)
               survivors)
        @ [ Line []; Rule None ]
  in
  let unreached_part =
    match m.unreached with
    | [] -> []
    | unreached ->
        [
          Line [];
          Line
            [
              styled `Faint
                (spf "unreached (%d) \u{2014} no test evaluates these"
                   m.unreached_total);
            ];
          (* One compact line per file, in the shape of the coverage
             table's file column: an unreached mutant is a gap, not a
             block. *)
          Rows
            {
              margin = "   ";
              columns =
                [
                  { gap = ""; align = `Left; width = None };
                  { gap = "   "; align = `Left; width = None };
                ];
              rows =
                List.map
                  (fun (u : unreached) ->
                    [ plain u.file; plain (ranges u.lines) ])
                  unreached;
            };
        ]
  in
  survivor_part @ unreached_part
  @ (if m.survivors <> [] || m.unreached <> [] then [ Line [] ] else [])
  @ [ Line (mutation_summary_spans m) ]

let mutation_report t (m : mutation) =
  clear_live t;
  close_row t;
  sections t (mutation_sections t m)

(* Admission (run data, rendered late)

   Three per-test rulings, one summary line, and nothing project-level
   (Law 17e): no survivor list, no score. An UNJUSTIFIED ruling is a
   failure block about a named test, so it renders as one — the labelled
   rule, the [  VERB  subject] head row, red. As everywhere, the ordering,
   the caps and every count are the loop's; this projects them. *)

(* The identifier arrives spelled: the loop holds the runtime's canonical
   spelling, and a second caller of it here would spend the Law-12
   coupling budget on a string this record can carry. *)
type fault = {
  fault_id : string;
  fault_file : string;
  fault_line : int;
  fault_before : string;
  fault_after : string;
  fault_source : string option;
}

type admission_cause = [ `Failure | `Fixture | `Crashed | `Timed_out ]

type admitted = { admitted_test : string; witness : fault; cause : admission_cause }

type unjustified = {
  unjustified_test : string;
  unjustified_loc : Loc.t option;
  shown : fault list;
  tried : int;
  candidates : int;
  reached : int;
  capped : bool;
}

type no_sites = { no_sites_test : string; no_sites_loc : Loc.t option }

type admission = {
  admission_arm_variable : string;
  admitted : admitted list;
  unjustified : unjustified list;
  no_sites : no_sites list;
  designated : int;
  admission_forks : int;
  admission_reached : int;
  capped_rulings : int;
  tries : int;
  admission_duration : float;
  admission_seed : Seed.seed option;
  scope : string option;
}

(* The arm counter-check of an UNJUSTIFIED ruling: the survivor block's
   arm command, narrowed to the one test the ruling is about — the reader
   strengthens that test, then watches it catch the fault. Under [`Exe]
   the invocation already ends in [--], so the filter lands after it;
   under [`Mirrors] both bindings precede the one command that exists
   there. *)
let admission_arm_command t ~variable id ~test =
  match t.invocation with
  | `Exe _ -> spf "%s -f %s" (arm_command t ~variable id) (shell_quote test)
  | `Mirrors ->
      spf "WINDTRAP_FILTER=%s %s" (shell_quote test)
        (arm_command t ~variable id)

(* A head row is one aligned row: the styled verb, the name, and the
   faint declaration site, trailing space stripped after styling — a
   ruling with no site sheds the gap before it. *)
let admission_head ~verb ~style ~test ~loc =
  Rows
    {
      margin = "  ";
      columns =
        [
          { gap = ""; align = `Left; width = None };
          { gap = "  "; align = `Left; width = None };
          { gap = "    "; align = `Left; width = None };
        ];
      rows =
        [
          [
            styled style verb;
            plain (sanitize_name test);
            styled `Faint
              (match loc with Some l -> Loc.to_string l | None -> "");
          ];
        ];
    }

let fault_row ~id_width f =
  Line
    [
      plain (indent ^ "  ");
      styled `Bold f.fault_id;
      plain
        (pad_to id_width f.fault_id ^ "   " ^ f.fault_before ^ "  \u{2192}  "
       ^ f.fault_after);
    ]

let fault_excerpt ~number_width f =
  match f.fault_source with
  | Some source ->
      [
        Excerpt
          {
            context = 0;
            marker = false;
            margin = indent ^ "    ";
            number_width = Some number_width;
            excerpt =
              {
                file = f.fault_file;
                heading = None;
                source;
                marked_lines = [ f.fault_line ];
              };
          };
      ]
  | None -> []

let admitted_sections (a : admitted) =
  let verb =
    match a.cause with
    | `Failure -> "killed"
    | `Fixture -> "killed (fixture)"
    | `Crashed -> "killed (crash)"
    | `Timed_out -> "killed (timeout)"
  in
  [
    Line [];
    admission_head ~verb:"ADMITTED" ~style:`Green ~test:a.admitted_test
      ~loc:None;
    Line
      [
        plain indent;
        styled `Green verb;
        plain "  ";
        styled `Bold a.witness.fault_id;
        plain
          ("   " ^ a.witness.fault_before ^ "  \u{2192}  "
         ^ a.witness.fault_after);
      ];
  ]

let no_sites_sections ~scope (n : no_sites) =
  [
    Line [];
    admission_head ~verb:"NO SITES" ~style:`Yellow ~test:n.no_sites_test
      ~loc:n.no_sites_loc;
    Line
      [
        plain
          (indent
         ^ "this test evaluates no mutation site \u{2014} no condition, \
            comparison,");
      ];
    Line
      [
        plain
          (indent
         ^ "connective or arithmetic \u{2014} so there is nothing to admit it \
            against.");
      ];
  ]
  @
  match scope with
  | None -> []
  | Some binding ->
      [
        Line
          [
            plain
              (indent
              ^ spf "(%s is set: a site outside it does not exist for this \
                     run.)"
                  binding);
          ];
      ]

let unjustified_sections t ~variable (u : unjustified) =
  let plural n = if n = 1 then "" else "s" in
  let sentence =
    if u.capped then
      (* "Most-run" is a claim about the tried faults: when a skip kept a
         capped candidate unwatched they are something other than the
         most-run ones, and the sentence then counts only what was tried
         (Law 17e). *)
      [
        Line
          [
            plain
              (indent
              ^
              if u.tried = u.candidates then
                spf
                  "killed none of the %d most-run fault%s on its lines, of %d \
                   reached"
                  u.tried (plural u.tried) u.reached
              else
                spf
                  "killed none of the %d fault%s tried on its lines, of %d \
                   reached"
                  u.tried (plural u.tried) u.reached);
          ];
        Line [ plain (indent ^ "(WINDTRAP_MUTATE_TRY=0 tries them all):") ];
      ]
    else if u.tried = u.reached then
      [
        Line
          [
            plain
              (indent
              ^ spf "killed none of the %d fault%s it reaches:" u.reached
                  (plural u.reached));
          ];
      ]
    else
      [
        Line
          [
            plain
              (indent
              ^ spf
                  "killed none of the %d fault%s tried on its lines, of %d \
                   reached:"
                  u.tried (plural u.tried) u.reached);
          ];
      ]
  in
  let id_width =
    List.fold_left (fun w f -> max w (Text.length_utf8 f.fault_id)) 0 u.shown
  in
  let number_width =
    List.fold_left
      (fun w (f : fault) -> max w (String.length (string_of_int f.fault_line)))
      1 u.shown
  in
  let more =
    if u.tried > List.length u.shown then
      [
        Line
          [
            plain
              (indent ^ "  "
              ^ spf "\u{2026} %d more" (u.tried - List.length u.shown));
          ];
      ]
    else []
  in
  let remedies =
    match u.shown with
    | [] -> []
    | first :: _ ->
        [
          Line [];
          Line
            [
              plain (indent ^ "strengthen the assertion, then watch it catch \
                              one:");
            ];
          Hint
            (indent ^ "  "
            ^ spf "%-9s%s" "arm"
                (admission_arm_command t ~variable first.fault_id
                   ~test:u.unjustified_test));
          Line
            [
              plain
                (indent
               ^ "a fault whose two versions compute the same value is \
                  equivalent \u{2014} dismiss it in the source:");
            ];
          Hint
            (indent ^ "  "
            ^ spf "%-9s((%s) [@mutate off \"reason\"])" "dismiss"
                first.fault_before);
        ]
  in
  (admission_head ~verb:"UNJUSTIFIED" ~style:`Red ~test:u.unjustified_test
     ~loc:u.unjustified_loc
   :: sentence)
  @ [ Line [] ]
  @ List.concat_map
      (fun f -> fault_row ~id_width f :: fault_excerpt ~number_width f)
      u.shown
  @ more @ remedies

let admission_summary_spans (a : admission) =
  let plural n = if n = 1 then "" else "s" in
  let admitted_count = List.length a.admitted in
  let unjustified_count = List.length a.unjustified in
  let no_sites_count = List.length a.no_sites in
  (* Zero terms are elided as the survey's are, with one exception: beside
     an unjustified ruling, [0 admitted] is the answer, not noise. *)
  let terms =
    (if admitted_count > 0 || unjustified_count > 0 then
       [
         styled
           (if admitted_count > 0 then `Green else `Red)
           (spf "%d admitted" admitted_count);
       ]
     else [])
    @ (if unjustified_count > 0 then
         [ styled `Red (spf "%d unjustified" unjustified_count) ]
       else [])
    @
    if no_sites_count > 0 then
      [ styled `Yellow (spf "%d no sites" no_sites_count) ]
    else []
  in
  let rec separated = function
    | [] -> []
    | [ last ] -> [ last ]
    | term :: rest -> term :: plain ", " :: separated rest
  in
  (plain "admission: " :: separated terms)
  @ [
      plain
        (spf " of %d \u{00b7} %d fork%s%s in %s%s%s" a.designated
           a.admission_forks
           (plural a.admission_forks)
           (if a.admission_reached > 0 then
              spf " over %d reached" a.admission_reached
            else "")
           (pp_duration a.admission_duration)
           (if a.capped_rulings > 0 then
              spf " \u{00b7} %d ruling%s capped at %d" a.capped_rulings
                (plural a.capped_rulings) a.tries
            else "")
           (match a.admission_seed with
           | Some s -> spf " (seed %s)" (Seed.to_string s)
           | None -> ""));
    ]

let admission_report t (a : admission) =
  clear_live t;
  close_row t;
  sections t
    (List.concat_map admitted_sections a.admitted
    @ List.concat_map (no_sites_sections ~scope:a.scope) a.no_sites
    @ (if a.unjustified = [] then []
       else
         [
           Line [];
           Rule (Some (spf "unjustified (%d)" (List.length a.unjustified)));
           Line [];
         ]
         @ List.concat
             (List.mapi
                (fun i u ->
                  (if i > 0 then [ Line [] ] else [])
                  @ unjustified_sections t ~variable:a.admission_arm_variable u)
                a.unjustified)
         @ [ Line []; Rule None ])
    @ [ Line []; Line (admission_summary_spans a) ])

type coverage_summary = { visited : int; total : int }

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
        | Failure.Fail fs -> acc + List.length (List.filter is_subtest_failure fs)
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
    if slow_results <> [] then begin
      slow_warnings t slow_results;
      put t ""
    end;
    summary_line t ~passed ~failed ~skipped
      ~excused:(List.length excused_results)
      ~subtests ~duration;
    (* No rerun hint. [--failed] is an optimization, not a step a reader has
       to take, and a suite is meant to be fast enough that rerunning all of
       it costs nothing — so advertising the flag under every failing run is
       an ad, not a report. The acceptance commands stay: those name a verb
       nobody can guess (Law 3), which is a different thing entirely. *)
    (* Diagnosis, not signal: the slowest list is verbose-only. *)
    if t.mode = `Verbose then slowest t results
  end;
  (* An in-process number is always one executable's view of the code it
     links; the project number is the merge, so the line points at the
     aggregate rather than posing as the total. Unconditional: which
     other executables exist is not something a run can know, and a hint
     that is true either way needs no filesystem look to decide. *)
  (match coverage with
  | Some { visited; total } ->
      put t
        (line_str t
           (coverage_line ~hint:"project: dune build @cover" ~visited ~total ()))
  | None -> ());
  Pp.flush t.out ()

(* The snapshot report *)

(* Printed after [finish], so the transcript is settled: plain lines on
   the sink, never through [put] or the deferral machinery — a green
   compact run's one-line transcript is already committed, and rerouting
   these lines through the row/deferral paths would change their bytes. *)
let report_snapshots t ~orphans run =
  let writes = Snapshot.writes (Run.snapshots run) in
  List.iter
    (fun (path, status) ->
      let status =
        match status with
        | Snapshot.Created -> "new"
        | Snapshot.Updated -> "updated"
      in
      Format.fprintf t.out "wrote %s (%s)@." (Path_ops.display path) status)
    writes;
  List.iter (Format.fprintf t.out "%s@.") (stale_lines orphans)
