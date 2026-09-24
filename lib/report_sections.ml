(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   The failure blocks adapt windtrap v1's progress.ml, rebuilt over typed
   Failure payloads and Diff data — the report projects run data, never
   alters it.
  ---------------------------------------------------------------------------*)

let spf = Printf.sprintf

(* Caps and layout constants. [report_sections.mli] states the caps beside
   their values. The report is not a canvas: one width, so a pipe and a
   wide terminal are byte-identical. *)
let columns = 80
let rule_width = 58 (* of an instrumentation report's rules *)
let max_diff_lines = 200
let max_proposed_lines = 20
let max_lines = 10 (* of a backtrace and of a captured tail *)
let max_value_bytes = 800 (* ten full lines *)
let max_headline_chars = 80
let indent = "    "

(* Small helpers *)
let rec take n = function
  | [] -> []
  | _ when n <= 0 -> []
  | x :: rest -> x :: take (n - 1) rest

let dashes n = String.concat "" (List.init (max 0 n) (fun _ -> "\u{2500}"))

(* Which case a counterexample is. An example is never shrunk. *)
let case_desc ~examples ~case_index ~shrink_steps =
  if examples then spf "example %d" (case_index + 1)
  else
    spf "case %d%s" case_index
      (if shrink_steps = 0 then ""
       else
         spf ", shrunk %d step%s" shrink_steps
           (if shrink_steps = 1 then "" else "s"))

(* POSIX single-quoting: closes the quote around every embedded [']. A
   control byte would break the line the word sits on: such a word takes
   the [$'…'] form, which bash, zsh and ksh read. *)
let shell_quote s =
  let control c = c < ' ' || c = '\127' in
  if not (String.exists control s) then
    "'" ^ String.concat "'\\''" (String.split_on_char '\'' s) ^ "'"
  else begin
    let buf = Buffer.create (String.length s + 8) in
    Buffer.add_string buf "$'";
    String.iter
      (fun c ->
        match c with
        | '\'' -> Buffer.add_string buf "\\'"
        | '\\' -> Buffer.add_string buf "\\\\"
        | '\n' -> Buffer.add_string buf "\\n"
        | '\t' -> Buffer.add_string buf "\\t"
        | '\r' -> Buffer.add_string buf "\\r"
        | c when control c ->
            Buffer.add_string buf (spf "\\x%02x" (Char.code c))
        | c -> Buffer.add_char buf c)
      s;
    Buffer.add_char buf '\'';
    Buffer.contents buf
  end

(* Command hints

   A hint is a word, the run's launcher and the parameters: under [`Exe]
   the flags after the command the facade computed at startup, under
   [`Mirrors] (every run a build action drives, and the default) the
   [WINDTRAP_*] mirrors before [dune runtest]. No colour in any hint. *)

(* A command-line word, quoted only when a shell would split or expand
   it. *)
let shell_word s =
  let safe = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
    | '_' | '-' | '.' | '/' | ':' | '=' | '+' | ',' | '@' | '%' -> true
    | _ -> false
  in
  if s <> "" && String.for_all safe s then s else shell_quote s

(* An armed run's failures are the mutant's: a command line that runs the
   test without it passes. *)
let arm_flag = function Some id -> " --arm " ^ shell_word id | None -> ""

let arm_mirror = function
  | Some id -> spf "WINDTRAP_MUTATE_ARM=%s " (shell_word id)
  | None -> ""

(* [count] is a property failure's one config-sourced knob
   (Failure.kind.Property): the hint restates it — [--prop-count]/
   [WINDTRAP_PROP_COUNT] — because replaying a late case needs at least as
   many cases as the failing run generated. A declaration-site count needs
   no flag and never reaches here, and the shrink budget is fixed, so the
   seed alone descends to the same node. *)
let replay_line ?count ~armed invocation ~seed ~filter =
  let token = Seed.to_string seed in
  match invocation with
  | `Exe cmd ->
      spf "replay: %s%s --seed %s%s%s" cmd (arm_flag armed) token
        (match count with Some n -> spf " --prop-count %d" n | None -> "")
        (match filter with Some flt -> " -f " ^ shell_quote flt | None -> "")
  | `Mirrors ->
      spf "replay: %sWINDTRAP_SEED=%s %s%sdune runtest" (arm_mirror armed) token
        (match count with
        | Some n -> spf "WINDTRAP_PROP_COUNT=%d " n
        | None -> "")
        (match filter with
        | Some flt -> spf "WINDTRAP_FILTER=%s " (shell_quote flt)
        | None -> "")

(* What [dune promote] is given: the source file of a literal, the
   displayed path of a file baseline. *)
let promoted_file (f : Failure.t) = function
  | Failure.Literal _ -> Option.map (fun (l : Loc.t) -> l.Loc.file) f.loc
  | Failure.File path -> Some (Os.display_path path)

(* An executable run by hand accepts in place with [-u], narrowed to the
   block's test; a build action wrote a correction beside the file for
   [dune promote]. Promotion fills a file and never creates one: a missing
   file must exist before dune's [diff?] can register its correction. *)
let accept_line invocation ~filter (f : Failure.t) =
  let accept baseline ~missing =
    match invocation with
    | `Exe cmd ->
        spf "accept: %s -u%s" cmd
          (match filter with
          | Some flt -> " -f " ^ shell_quote flt
          | None -> "")
    | `Mirrors -> (
        match promoted_file f baseline with
        | None -> "accept: dune promote"
        | Some file -> (
            let promote = "dune promote " ^ shell_word file in
            match baseline with
            | Failure.File _ when missing ->
                spf "accept: touch %s && dune runtest; %s" (shell_quote file)
                  promote
            | Failure.File _ | Failure.Literal _ -> "accept: " ^ promote))
  in
  match f.kind with
  | Failure.Baseline { baseline; state = Failure.Missing _; withheld = None } ->
      Some (accept baseline ~missing:true)
  | Failure.Baseline { baseline; state = Failure.Mismatch _; withheld = None }
    ->
      Some (accept baseline ~missing:false)
  | Failure.Baseline { withheld = Some _; _ }
  | Failure.Baseline { state = Failure.Unresolvable _; _ }
  | Failure.Equality _ | Failure.Containment _ | Failure.Raise _
  | Failure.Property _ | Failure.Message _ ->
      None

(* Why a failure offers no [accept:]: the run kept none of the attempt's
   corrections (Run, Corrections), or this one could not be recorded, so
   the command would promote or rewrite nothing. *)
let withheld_fact (f : Failure.t) =
  let kept_none reason = Some ("no correction was kept: " ^ reason) in
  match f.kind with
  | Failure.Baseline
      { state = Failure.Missing _ | Failure.Mismatch _; withheld = Some why; _ }
    -> (
      match why with
      | Failure.Refused { line; reason } ->
          Some (spf "correction refused (line %d): %s" line reason)
      | Failure.Conflict ->
          kept_none
            "another check of this baseline produced a different text earlier \
             in the run"
      | Failure.Failed_outside ->
          kept_none
            "the test also failed outside its expectations; fix that failure \
             and rerun"
      | Failure.Skipped ->
          kept_none
            "the test also skipped; skip before the expectation or not at all, \
             and rerun")
  | Failure.Baseline _ | Failure.Equality _ | Failure.Containment _
  | Failure.Raise _ | Failure.Property _ | Failure.Message _ ->
      None

let replay_of ~armed invocation ~filter (f : Failure.t) =
  match f.kind with
  | Failure.Property { examples = false; root; count; _ } ->
      Some (replay_line ?count ~armed invocation ~seed:root ~filter)
  | Failure.Property { examples = true; _ }
  | Failure.Equality _ | Failure.Containment _ | Failure.Raise _
  | Failure.Baseline _ | Failure.Message _ ->
      None

let hints ?armed ?(invocation = `Mirrors) ~filter failures =
  let distinct lines =
    List.rev
      (List.fold_left
         (fun acc line -> if List.mem line acc then acc else line :: acc)
         [] lines)
  in
  (* An armed run checks its baselines and writes none: what differs is the
     mutant's output, never to be accepted, and no correction is its to
     keep. *)
  let withheld, accepts =
    match armed with
    | Some _ -> ([], [])
    | None ->
        ( List.filter_map withheld_fact failures,
          List.filter_map (accept_line invocation ~filter) failures )
  in
  distinct withheld
  @ distinct
      (accepts @ List.filter_map (replay_of ~armed invocation ~filter) failures)

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
          match Os.project_root () with
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

let release_title = "fixture release"

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

(* Spans and the sink

   A line reaches its sink as spans, and the sink applies the style. A
   report prints the values a test produced, the lines of its files and
   the names it chose, and a control byte in one of those drives the
   terminal instead of appearing in the report: ESC eats the label beside
   it and leaves the terminal coloured, CR overwrites the line the reader
   needed, and a grep for the reported value finds nothing. Windtrap's own
   text holds no control byte, so the sink escapes every span
   ([Text.escape_controls]) and thereby exactly what came from outside,
   under both [ansi] settings. TAB is the one exception, because
   indentation is the layout a block is built from; line structure is kept
   by splitting a text into lines before it becomes spans.

   This is a projection, exactly like colour: equality, containment, and
   baseline storage never see it. The escape is not injective (a value
   holding the four characters [\x1b] renders like one holding the byte),
   because the alternative is escaping the backslash, which would double
   every escape in the [%S] renderings that make up most of a transcript.
   Every structural decision (are the two renderings equal, do their line
   lists differ, which regions changed) is therefore made on the raw
   values, and only the column arithmetic ([cols]) counts escaped glyphs. *)
type span = { style : Pp.style option; text : string }

let plain text = { style = None; text }
let styled style text = { style = Some style; text }

let render ~ansi spans =
  String.concat ""
    (List.map
       (fun { style; text } ->
         let text = Text.escape_controls text in
         match style with
         | Some style -> Pp.styled_string ~ansi style text
         | None -> text)
       spans)

(* The columns of [text] as it prints: its code points once escaped. The
   escape is per byte, so the columns of a prefix are those of the prefix
   of the escaped text. *)
let cols text = Text.length_utf8 (Text.escape_controls text)
let width spans = List.fold_left (fun w { text; _ } -> w + cols text) 0 spans

(* Failure projections *)

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

(* Fact lines and the headline *)

(* Plain quotes, not [%S]: a path's UTF-8 must not be byte-escaped, and
   its control bytes are the sink's to escape, as every text's are. *)
let baseline_subject = function
  | Failure.Literal { exact = false } -> "expect"
  | Failure.Literal { exact = true } -> "expect_exact"
  | Failure.File path -> spf "expect_file \"%s\"" (Os.display_path path)

(* A chain break and a plain miss answer the same question, the first in
   more words. *)
let containment_verdict ~demand ~found_at =
  match (demand, found_at) with
  | Failure.Ordered { resumed_at; _ }, Some at ->
      spf "found at byte %d, before the search resumed at byte %d" at resumed_at
  | Failure.Ordered { resumed_at; _ }, None ->
      spf "not found at or after byte %d" resumed_at
  | Failure.Anywhere, Some at -> spf "found at byte %d" at
  | Failure.Anywhere, None -> "not found"

(* Line lists equal but bytes differ: the only such difference is a single
   trailing newline, which a line diff cannot show. *)
let newline_fact ~expected ~actual =
  spf "values differ only by a trailing newline (on the %s side)"
    (if String.length actual > String.length expected then "actual"
     else "expected")

let diff_lines hunks =
  List.fold_left (fun acc h -> acc + 1 + List.length h.Diff.lines) 0 hunks

(* The failure as one sentence after the label and the user's message,
   line breaks and tabs turned into spaces. Any other control byte is left
   to the escaping of the field that receives the sentence. A diff has no
   side short enough to quote and says how long it is. *)
let headline (f : Failure.t) =
  let fact =
    match f.kind with
    | Failure.Equality { not_ = true; expected; _ } ->
        "both sides equal: " ^ expected
    | Failure.Equality { expected; actual; diffable = false; _ } ->
        spf "expected %s, got %s" expected actual
    | Failure.Equality { expected; actual; _ } -> (
        if String.equal expected actual then "both sides render as: " ^ expected
        else if
          not (String.contains expected '\n' || String.contains actual '\n')
        then spf "expected %s, got %s" expected actual
        else
          match Diff.hunks ~expected ~actual () with
          | [] -> newline_fact ~expected ~actual
          | hunks ->
              spf "expected and actual differ (%d diff lines)"
                (diff_lines hunks))
    | Failure.Containment { needle; found_at; haystack_length; demand; _ } -> (
        (* A chain break is its own verdict: it reads as neither "found" nor
           "not found". *)
        match (demand, found_at) with
        | Failure.Ordered { index; resumed_at }, Some at ->
            spf "element %d %S out of order: at byte %d, before byte %d" index
              needle at resumed_at
        | Failure.Ordered { index; resumed_at }, None ->
            spf "element %d %S not found at or after byte %d (%d-byte haystack)"
              index needle resumed_at haystack_length
        | Failure.Anywhere, Some at ->
            spf "needle %S found at byte %d" needle at
        | Failure.Anywhere, None ->
            spf "needle %S not found (%d-byte haystack)" needle haystack_length)
    | Failure.Raise { expected = Some e; actual = Some a; _ } ->
        spf "expected exception %s, raised %s" e a
    | Failure.Raise { expected = Some e; actual = None; _ } ->
        spf "expected exception %s, none raised" e
    | Failure.Raise { expected = None; actual = Some a; predicate; _ } ->
        (* [predicate] tells a [raises_match] rejection from an exception
           nobody expected. *)
        if predicate then spf "exception did not satisfy the predicate: %s" a
        else spf "uncaught exception: %s" a
    | Failure.Raise { expected = None; actual = None; _ } ->
        "expected an exception, none raised"
    | Failure.Baseline { baseline; state; _ } -> (
        let subject = baseline_subject baseline in
        match state with
        | Failure.Missing _ -> subject ^ ": no baseline"
        | Failure.Mismatch _ -> subject ^ ": mismatch"
        | Failure.Unresolvable _ ->
            subject ^ ": cannot resolve the path under the project root")
    | Failure.Property
        {
          rendered;
          summary;
          case_index;
          shrink_steps;
          shrink_end;
          examples;
          rendering;
          _;
        } ->
        (* The block says why a search stopped in a line of its own; here
           the case carries it: [shrunk 100 steps] alone reads as a converged
           search. *)
        spf "property failed (%s%s):%s%s"
          (case_desc ~examples ~case_index ~shrink_steps)
          (if examples then ""
           else
             match shrink_end with
             | Failure.Converged -> ""
             | Failure.Budget_spent -> ", shrink limit reached"
             | Failure.Candidate_raised _ -> ", shrinking stopped"
             | Failure.Timed_out _ -> ", shrinking timed out")
          (match rendering with
          | Failure.Pre_image -> " computed from "
          | Failure.Value -> " ")
          (Option.value summary ~default:rendered)
    | Failure.Message "" -> "(empty failure message)"
    | Failure.Message m -> m
  in
  let line =
    String.map
      (function '\n' | '\r' | '\t' -> ' ' | c -> c)
      (match labeled_msg f with None -> fact | Some msg -> msg ^ ": " ^ fact)
  in
  let rec cut i chars =
    if i >= String.length line then line
    else if chars = max_headline_chars then String.sub line 0 i ^ "\u{2026}"
    else
      let decode = String.get_utf_8_uchar line i in
      cut (i + Uchar.utf_decode_length decode) (chars + 1)
  in
  cut 0 0

(* [s] as spans, the byte ranges [spans], ascending and disjoint, in
   [style]. *)
let highlight style s spans =
  let pieces, pos =
    List.fold_left
      (fun (pieces, pos) { Diff.start; length } ->
        ( styled style (String.sub s start length)
          :: plain (String.sub s pos (start - pos))
          :: pieces,
          start + length ))
      ([], 0) spans
  in
  List.rev (plain (String.sub s pos (String.length s - pos)) :: pieces)

(* Whether colour shows [spans] of [s]: a span of spaces, or an empty one,
   has no glyph to colour. *)
let colour_shows s spans =
  not
    (List.exists
       (fun { Diff.start; length } ->
         String.for_all (fun c -> c = ' ') (String.sub s start length))
       spans)

(* The [~~~] line under [s] as it prints: one column per code point of its
   escaped text. *)
let marker_line s spans =
  if spans = [] then None
  else begin
    let buf = Buffer.create (String.length s) in
    let col = ref 0 in
    List.iter
      (fun { Diff.start; length } ->
        let scol = cols (String.sub s 0 start) in
        let width = max 1 (cols (String.sub s start length)) in
        if scol > !col then
          Buffer.add_string buf (String.make (scol - !col) ' ');
        Buffer.add_string buf (String.make width '~');
        col := max scol !col + width)
      spans;
    Some (Buffer.contents buf)
  end

(* Whether a [~] line under [s], an escaped text, lands under the code
   points it marks: a code point past Latin Extended-B has no fixed width,
   and neither has a tab unless the [~] line repeats it ([tabs]). *)
let aligns ~tabs s =
  let rec scan i =
    i >= String.length s
    ||
    let decode = String.get_utf_8_uchar s i in
    let code = Uchar.to_int (Uchar.utf_decode_uchar decode) in
    Uchar.utf_decode_is_valid decode
    && ((0x20 <= code && code <= 0x24F) || (tabs && code = 0x09))
    && scan (i + Uchar.utf_decode_length decode)
  in
  scan 0

(* The [~] line for a [-] line whose [+] line differs from it only in
   trailing spaces and tabs, a pair that prints as two equal lines: the
   columns up to the mark, tabs repeated so that any tab stops align it,
   then one [~] per space or tab of the [-] line's that the [+] line lacks,
   or of the [+] line's when the [-] line has none. *)
let trailing_mark ~deleted ~inserted =
  let stem s =
    let rec scan i =
      if i > 0 && (s.[i - 1] = ' ' || s.[i - 1] = '\t') then scan (i - 1) else i
    in
    scan (String.length s)
  in
  let at = stem deleted in
  if
    String.equal deleted inserted
    || at <> stem inserted
    || not (String.equal (String.sub deleted 0 at) (String.sub inserted 0 at))
  then None
  else
    let rec shared i =
      if
        i < String.length deleted
        && i < String.length inserted
        && deleted.[i] = inserted.[i]
      then shared (i + 1)
      else i
    in
    let shared = shared at in
    let lead = Text.escape_controls (String.sub deleted 0 shared) in
    let marked =
      if String.length deleted > shared then String.length deleted - shared
      else String.length inserted - shared
    in
    if not (aligns ~tabs:true lead) then None
    else
      let buf = Buffer.create (String.length lead) in
      let rec pad i =
        if i < String.length lead then begin
          Buffer.add_char buf (if lead.[i] = '\t' then '\t' else ' ');
          pad (i + Uchar.utf_decode_length (String.get_utf_8_uchar lead i))
        end
      in
      pad 0;
      Some (Buffer.contents buf, String.make marked '~')

(* Hunks under a budget of [limit] lines, [@@] lines included. *)
let pp_hunks put ~ind ~limit hunks =
  let total = diff_lines hunks in
  let budget = ref limit in
  let emit line =
    if !budget > 0 then put line;
    decr budget
  in
  (* [after_delete] and the line after the [+] tell a one-to-one pair from
     a run of changes, whose lines answer each other in no fixed order. *)
  let rec lines ~after_delete = function
    | [] -> ()
    | Diff.Keep s :: rest ->
        emit [ plain (ind ^ "  " ^ s) ];
        lines ~after_delete:false rest
    | Diff.Insert s :: rest ->
        emit [ plain ind; styled `Red ("+ " ^ s) ];
        lines ~after_delete:false rest
    | Diff.Delete s :: rest ->
        (* Green is the expected side and red the actual one, here as
           everywhere else, not the diff tool's red-for-removed: the sigils
           say which side is which, the colour carries the report's own
           meaning. *)
        emit [ plain ind; styled `Green ("- " ^ s) ];
        let mark =
          match rest with
          | Diff.Insert inserted :: ([] | (Diff.Keep _ | Diff.Delete _) :: _)
            when not after_delete ->
              trailing_mark ~deleted:s ~inserted
          | Diff.Insert _ :: _ | (Diff.Keep _ | Diff.Delete _) :: _ | [] -> None
        in
        (* The mark belongs to its [-] line and is no diff line: it prints
           iff that line did, outside the budget. *)
        (match mark with
        | Some (pad, tildes) when !budget >= 0 ->
            put [ plain (ind ^ "  " ^ pad); styled `Red tildes ]
        | Some _ | None -> ());
        lines ~after_delete:true rest
  in
  (* Unified diff numbers a side with no line by the line before it:
     [-0,0] for lines inserted before the first. *)
  let range start count =
    spf "%d,%d" (if count = 0 then start - 1 else start) count
  in
  List.iter
    (fun (h : Diff.hunk) ->
      emit
        [
          plain ind;
          styled `Faint
            (spf "@@ -%s +%s @@"
               (range h.expected_start h.expected_count)
               (range h.actual_start h.actual_count));
        ];
      lines ~after_delete:false h.lines)
    hunks;
  if total > limit then
    put
      [
        plain ind;
        styled `Faint (spf "\u{2026} (+%d more diff lines)" (total - limit));
      ]

(* A single-line value as it prints: its middle elided past
   [max_value_bytes], cut in the carried bytes so the count is theirs. *)
let elided s = String.length s > max_value_bytes
let shown s = Text.elide_middle max_value_bytes ~show:Fun.id s

(* A located source line as a block shows it, the gutter and what follows
   it: dedented, and printed as a single-line value is, a file's bytes
   being no more the report's than a test's are. *)
let source_excerpt line text =
  ( spf "%d \u{2502}" line,
    match String.trim text with "" -> "" | text -> " " ^ shown text )

(* What changed in [s], a value printed after [before]. With colour the
   spans are [style]d inside the plain value and no [~] line prints, unless
   colour cannot show one of them; without it a [~] line marks them, when
   [aligned]. *)
let pp_marked ~ansi put ~aligned ~style ~before s spans =
  let tildes = (not ansi) || not (colour_shows s spans) in
  put (before @ highlight style s spans);
  if tildes && aligned then
    Option.iter
      (fun m ->
        let at = String.index m '~' in
        put
          [
            plain (String.make (width before + at) ' ');
            styled style (String.sub m at (String.length m - at));
          ])
      (marker_line s spans)

(* Two single-line renderings under their anchors. Refinement runs
   against the raw values, and the sink escapes what the spans cut. A pair
   that does not refine prints each side whole in its colour: so does one
   whose anchors already state the difference ([marked] is off), and one
   with an elided side, which has no columns left to mark. *)
let pp_sides ~ansi put ~ind ~anchors:(expected_anchor, actual_anchor) ~marked
    ~expected ~actual =
  let gutter =
    2 + max (String.length expected_anchor) (String.length actual_anchor)
  in
  let marked = marked && not (elided expected || elided actual) in
  let expected_spans, actual_spans =
    match if marked then Diff.refine ~expected ~actual else None with
    | None -> ([], [])
    | Some { Diff.expected_spans; actual_spans } ->
        (expected_spans, actual_spans)
  in
  let refined = expected_spans <> [] || actual_spans <> [] in
  let expected = shown expected and actual = shown actual in
  let aligned =
    aligns ~tabs:false (Text.escape_controls expected)
    && aligns ~tabs:false (Text.escape_controls actual)
  in
  let side anchor ~whole ~span value spans =
    let before =
      [
        plain ind;
        styled `Faint anchor;
        plain (String.make (gutter - String.length anchor) ' ');
      ]
    in
    if refined then pp_marked ~ansi put ~aligned ~style:span ~before value spans
    else put (before @ [ styled whole value ])
  in
  side expected_anchor ~whole:`Green ~span:`Bold_green expected expected_spans;
  side actual_anchor ~whole:`Red ~span:`Bold_red actual actual_spans

let pp_eq ~ansi put ~ind ~expected ~actual =
  if String.equal expected actual then begin
    (* The equality told the values apart and their printer did not
       ([equal float nan nan], a lossy pp). Decided on the raw renderings:
       escaping merges values it cannot tell apart, and a pair the printer
       did distinguish must never be reported as one it did not. *)
    if String.contains expected '\n' then begin
      put [ plain ind; styled `Faint "both sides render as:" ];
      List.iter
        (fun l -> put [ plain (ind ^ "  " ^ l) ])
        (Text.split_lines expected)
    end
    else
      put
        [
          plain ind;
          styled `Faint "both sides render as:";
          plain (" " ^ shown expected);
        ];
    put
      [
        plain ind;
        styled `Faint "the printer shows less than the equality compares";
      ]
  end
  else if String.contains expected '\n' || String.contains actual '\n' then
    begin match Diff.hunks ~expected ~actual () with
    | [] -> put [ plain (ind ^ newline_fact ~expected ~actual) ]
    | hunks ->
        put [ plain ind; styled `Faint "--- expected" ];
        put [ plain ind; styled `Faint "+++ actual" ];
        pp_hunks put ~ind ~limit:max_diff_lines hunks
    end
  else
    pp_sides ~ansi put ~ind ~anchors:("expected", "actual") ~marked:true
      ~expected ~actual

(* A rendering under the sentence or the anchor that names it: a block,
   each line in [style], so that no style spans a line. *)
let pp_value_block put ~ind style value =
  if String.contains value '\n' then
    List.iter
      (fun l -> put [ plain (ind ^ "  "); styled style l ])
      (Text.split_lines value)
  else put [ plain (ind ^ "  "); styled style (shown value) ]

(* The sides of a [raises] that named its exception; [actual] is [None]
   when nothing was raised. The anchors state the difference, so nothing is
   marked; a rendering that spans lines is a block under its anchor. *)
let pp_raise ~ansi put ~ind ~expected ~actual =
  let spans_lines s = String.contains s '\n' in
  match actual with
  | Some actual when not (spans_lines expected || spans_lines actual) ->
      pp_sides ~ansi put ~ind
        ~anchors:("expected exception", "raised")
        ~marked:false ~expected ~actual
  | Some _ | None ->
      let gutter = 2 + String.length "expected exception" in
      let side anchor style value =
        if spans_lines value then begin
          put [ plain ind; styled `Faint (anchor ^ ":") ];
          pp_value_block put ~ind style value
        end
        else
          put
            [
              plain ind;
              styled `Faint anchor;
              plain (String.make (gutter - String.length anchor) ' ');
              styled style (shown value);
            ]
      in
      side "expected exception" `Green expected;
      begin match actual with
      | Some actual -> side "raised" `Red actual
      | None -> put [ plain (ind ^ "but no exception was raised") ]
      end

(* The phase tag: on its own line when the failure has no location. *)
let phase_tag (f : Failure.t) =
  match f.phase with
  | Failure.Body -> None
  | Failure.Setup -> Some "[setup]"
  | Failure.Teardown -> Some "[teardown]"
  | Failure.Release -> Some "[release]"

let rec pp_gen ~ansi ~excerpt ~inner ~hints:hinted ~filter ~invocation ~armed
    ~ind ppf (f : Failure.t) =
  let put spans = Pp.pf ppf "%s@\n" (render ~ansi spans) in
  let put_ind spans = put (plain ind :: spans) in
  let put_text line = put_ind [ plain line ] in
  let put_block s =
    List.iter (fun line -> put_text ("  " ^ line)) (Text.split_lines s)
  in
  (match
     ( Option.map (styled `Yellow) (phase_tag f),
       Option.map (fun loc -> styled `Faint (Loc.to_string loc)) f.loc )
   with
  | None, None -> ()
  | Some part, None | None, Some part -> put_ind [ part ]
  | Some tag, Some loc -> put_ind [ tag; plain " "; loc ]);
  (* The located source line is best effort. The blank line after it
     closes a block's head; an inner entry has none. *)
  (match f.loc with
  | Some { Loc.file; line; _ } when excerpt ->
      Option.iter
        (fun text ->
          let gutter, text = source_excerpt line text in
          put_ind [ plain "  "; styled `Faint gutter; plain text ];
          if not inner then put [])
        (source_line file line)
  | Some _ | None -> ());
  (match f.subtest with
  | [] -> ()
  | [ leaf ] -> put_ind [ styled `Faint "subtest"; plain ("   " ^ leaf) ]
  | _ :: names ->
      put_ind
        [
          styled `Faint "subtest"; plain ("   " ^ Test_tree.path_to_string names);
        ]);
  Option.iter (fun msg -> List.iter put_text (Text.split_lines msg)) f.msg;
  (match f.kind with
  | Failure.Equality { not_ = true; expected; _ } ->
      if String.contains expected '\n' then begin
        put_text "both sides equal:";
        put_block expected
      end
      else put_text (spf "both sides equal: %s" (shown expected))
  | Failure.Equality { expected = claim; actual = value; diffable = false; _ }
    ->
      (* A claim is a description, not a rendering: never diff or refine the
         two. Colour still applies — green and red mark which side is
         which, and that is as true of a description as of a value, and so is
         visibility: a [~claim] may be built around a rendered bound
         ([greater than <x>]). *)
      put_ind
        [ styled `Faint "expected"; plain "  "; styled `Green (shown claim) ];
      if String.contains value '\n' then begin
        put_ind [ styled `Faint "actual:" ];
        pp_value_block put ~ind `Red value
      end
      else
        put_ind
          [ styled `Faint "actual"; plain "    "; styled `Red (shown value) ]
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
      (* The block is the containment payload, never a fake equality diff;
         the claim sentence is a description and stays out of it. Anchors pad
         to the [expected]/[actual] gutter. The element of the chain that
         broke it has its own line: the rest of the block is about it. *)
      (match demand with
      | Failure.Ordered { index; _ } ->
          put_ind
            [ styled `Faint "element"; plain ("   " ^ string_of_int index) ]
      | Failure.Anywhere -> ());
      (* [%S] is [String.escaped] between quotes, OCaml's decimal escapes.
         An elided needle is cut in its carried bytes and each end escaped:
         a cut in the quoted text would split an escape and count its
         digits. *)
      put_ind
        [
          styled `Faint "needle";
          plain
            ("    \""
            ^ Text.elide_middle max_value_bytes ~show:String.escaped needle
            ^ "\": "
            ^ containment_verdict ~demand ~found_at);
        ];
      (* The occurrence's byte range inside the excerpt, when it is there to
         mark: a failed [not_contains] window always contains it, and an
         out-of-order chain break carries one that a cursor-anchored window
         may have left behind, hence the bounds test. The span is the
         payload's, in raw bytes, as every span is. *)
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
      (* The occurrence is marked as a changed span is ([pp_marked]); the
         excerpt is the evidence and prints whole. *)
      let haystack ~before line spans =
        pp_marked ~ansi put
          ~aligned:(aligns ~tabs:false (Text.escape_controls line))
          ~style:`Bold_red ~before:(plain ind :: before) line spans
      in
      if not (String.contains excerpt '\n') then
        haystack
          ~before:[ styled `Faint "haystack"; plain "  " ]
          excerpt
          (Option.to_list occurrence)
      else begin
        put_ind [ styled `Faint "haystack:" ];
        (* [offset] is the line's first byte within the excerpt; an
           occurrence that spans lines is marked on its first. *)
        ignore
          (List.fold_left
             (fun offset line ->
               haystack
                 ~before:[ plain "  " ]
                 line
                 (match occurrence with
                 | Some { Diff.start; length }
                   when start >= offset && start < offset + String.length line
                   ->
                     [
                       {
                         Diff.start = start - offset;
                         length =
                           min length (offset + String.length line - start);
                       };
                     ]
                 | Some _ | None -> []);
               offset + String.length line + 1)
             0 (Text.split_lines excerpt))
      end;
      (* State what was omitted, iff the excerpt is partial. *)
      if
        excerpt_offset > 0
        || excerpt_offset + String.length excerpt < haystack_length
      then
        put_ind
          [
            styled `Faint
              (spf "(excerpt: bytes %d-%d of a %d-byte haystack)" excerpt_offset
                 (excerpt_offset + String.length excerpt - 1)
                 haystack_length);
          ]
  | Failure.Raise { expected; actual; predicate; backtrace; message_diff } -> (
      (match (message_diff, expected, actual) with
      | Some { Failure.constructor; expected_message; actual_message }, _, _ ->
          (* Right constructor, wrong payload: the messages are compared,
             the constructor said once. *)
          put_text (spf "raised %s with the wrong message:" constructor);
          pp_eq ~ansi put ~ind
            ~expected:(spf "%S" expected_message)
            ~actual:(spf "%S" actual_message)
      | None, Some expected, actual -> pp_raise ~ansi put ~ind ~expected ~actual
      | None, None, Some actual ->
          (* [predicate] tells a [raises_match] rejection from a test body's
             escape: the two demand different reactions. *)
          put_text
            (if predicate then
               "raised exception does not satisfy the predicate:"
             else "uncaught exception:");
          pp_value_block put ~ind `Red actual
      | None, None, None ->
          put_text "expected an exception, but none was raised");
      match backtrace with
      | Some bt ->
          let frames = Text.split_lines bt in
          List.iter
            (fun l -> put_ind [ styled `Faint l ])
            (take max_lines frames);
          let more = List.length frames - max_lines in
          if more > 0 then
            put_ind [ styled `Faint (spf "\u{2026} (+%d more frames)" more) ]
      | None -> ())
  | Failure.Baseline { baseline; state; withheld = _ } -> (
      let subject = baseline_subject baseline in
      match state with
      | Failure.Missing { proposed } ->
          put_text (subject ^ ": no baseline");
          (* The file does not exist: its proposed text is all [+] and has
             no hunk to head. *)
          let lines = Text.split_lines proposed in
          let n = List.length lines in
          put_text (spf "proposed (%d line%s):" n (if n = 1 then "" else "s"));
          List.iter
            (fun l -> put_ind [ plain "  "; styled `Red ("+ " ^ l) ])
            (take max_proposed_lines lines);
          if n > max_proposed_lines then
            put_ind
              [
                plain "  ";
                styled `Faint
                  (spf "\u{2026} (+%d more lines)" (n - max_proposed_lines));
              ]
      | Failure.Mismatch { expected; actual } -> (
          put_text (subject ^ ": mismatch");
          match Diff.hunks ~expected ~actual () with
          | [] -> put_text (newline_fact ~expected ~actual)
          | hunks -> pp_hunks put ~ind ~limit:max_diff_lines hunks)
      | Failure.Unresolvable { candidate } ->
          put_text
            (subject
           ^ ": the path cannot be proven to lie under the project root");
          put_text (spf "unverified path: %s" (Os.display_path candidate));
          put_text
            "(set WINDTRAP_PROJECT_ROOT to the directory the path is relative \
             to)")
  | Failure.Property
      {
        rendered;
        summary;
        case_index;
        shrink_steps;
        shrink_end;
        examples;
        rendering;
        inner = inner_failure;
        root = _;
        count = _;
      } -> (
      (* A pre-image is marked in the slot itself, [computed from], so a
         reader who stops at this line does not take it for the value the
         body received; the aside under it says what it is and what to do. *)
      let head =
        spf "counterexample (%s):%s"
          (case_desc ~examples ~case_index ~shrink_steps)
          (match rendering with
          | Failure.Value -> ""
          | Failure.Pre_image -> " computed from")
      in
      (* A summarized counterexample is a table: the summary takes the
         value's place on the head, and the header row is structure. *)
      (match (summary, Text.split_lines rendered) with
      | Some summary, header :: rows ->
          put_text (head ^ " " ^ shown summary);
          put_ind [ plain "  "; styled `Faint header ];
          List.iter (fun row -> put_text ("  " ^ row)) rows
      | Some _, [] | None, ([] | [ _ ]) -> put_text (head ^ " " ^ shown rendered)
      | None, (_ :: _ :: _ as lines) ->
          put_text head;
          List.iter (fun line -> put_text ("  " ^ line)) lines);
      (match rendering with
      | Failure.Value -> ()
      | Failure.Pre_image ->
          put_ind
            [
              plain "  ";
              styled `Faint
                "(the value has no printer, so this is the input that map and \
                 bind";
            ];
          put_ind
            [
              plain "  ";
              styled `Faint
                " computed it from; attach a printer with Gen.with_pp to see \
                 the value)";
            ]);
      (* What is reported is the best the search got to. [%gs] is the
         runner's [timed out after %gs], so one grep finds both. *)
      (match shrink_end with
      | Failure.Timed_out limit ->
          put_text
            (spf
               "timed out after %gs while shrinking; counterexample may not be \
                minimal"
               limit)
      | Failure.Budget_spent ->
          put_text
            (spf
               "shrinking stopped after %d steps; counterexample may not be \
                minimal"
               shrink_steps)
      | Failure.Candidate_raised text ->
          (* Two lines: the exception is the user's text, of any length. *)
          put_text
            (spf "shrinking stopped after %d steps: a candidate raised %s"
               shrink_steps text);
          put_text "counterexample may not be minimal"
      | Failure.Converged -> ());
      (* An inner failure raised in tail position has no site: [at:] over
         no location would misread. *)
      match inner_failure with
      | Some i ->
          put_text
            (match i.Failure.loc with
            | Some _ -> "which failed at:"
            | None -> "which failed with:");
          pp_gen ~ansi ~excerpt ~inner:true ~hints:false ~filter ~invocation
            ~armed ~ind:(ind ^ "  ") ppf i
      | None -> ())
  | Failure.Message "" -> put_text "(empty failure message)"
  | Failure.Message m -> List.iter put_text (Text.split_lines m));
  if hinted then List.iter put_text (hints ?armed ~invocation ~filter [ f ])

let pp_failure ~ansi ?(excerpt = false) ?(hints = true) ?filter
    ?(invocation = `Mirrors) ?armed ppf f =
  pp_gen ~ansi ~excerpt ~inner:false ~hints ~filter ~invocation ~armed
    ~ind:indent ppf f

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
   variable) arrives pre-spelled with the runtime's own functions. A source
   line has two drawers: [source_excerpt] for a failure block and a
   survivor block, [excerpt] for the coverage source view alone. *)

(* The sink: where sections print and whether they style. *)
type sink = { out : Format.formatter; ansi : bool }

let put_line k line = Pp.pf k.out "%s@\n" line
let put k spans = put_line k (render ~ansi:k.ansi spans)

let rule ~width = function
  | None -> dashes width
  | Some label ->
      let inner = Text.length_utf8 label + 2 in
      let left = max 2 ((width - inner) / 2) in
      let right = max 2 (width - inner - left) in
      dashes left ^ " " ^ label ^ " " ^ dashes right

let rstrip s =
  let n = ref (String.length s) in
  while !n > 0 && s.[!n - 1] = ' ' do
    decr n
  done;
  String.sub s 0 !n

let pad n s = s ^ String.make (max 0 (n - cols s)) ' '

type column = { gap : string; align : [ `Left | `Right ]; width : int option }

(* Line numbers as ranges ([88-94, 121]). *)

(* [lines] is ascending and without duplicates: a coverage producer's
   obligation for a row's ranges, [List.sort_uniq] at the two other callers. *)
let collapse_ranges lines =
  let rec loop acc range_start range_end = function
    | [] -> List.rev ((range_start, range_end) :: acc)
    | line :: rest ->
        if line <= range_end + 1 then
          loop acc range_start (max range_end line) rest
        else loop ((range_start, range_end) :: acc) line line rest
  in
  match lines with [] -> [] | first :: rest -> loop [] first first rest

(* The ranges of [lines], at most [max_ranges] of them and then
   [(+N more)]. The ranges are what a reader goes and tests, so a row keeps
   the eight that a well-covered file needs whole, whatever its width, and
   bounds only a barely tested file. *)
let max_ranges = 8

let bounded_ranges lines =
  let ranges =
    List.map
      (fun (s, e) -> if s = e then string_of_int s else spf "%d-%d" s e)
      (collapse_ranges lines)
  in
  let total = List.length ranges in
  if total <= max_ranges then String.concat ", " ranges
  else
    spf "%s (+%d more)"
      (String.concat ", " (take max_ranges ranges))
      (total - max_ranges)

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

let excerpts ~source marked =
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
    |> List.map (fun (s, e) -> (max 1 (s - 1), min total (e + 1)))
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

type excerpt = { source : string; marked_lines : int list }

(* The marker rides inside the margin, so a marked row's escape sequence
   opens at column zero. A file's bytes are no more the report's than a
   test's are: they print as a failure block's source line does. *)
let excerpt k e =
  let regions = excerpts ~source:e.source e.marked_lines in
  let digits =
    List.fold_left
      (List.fold_left (fun w l ->
           max w (String.length (string_of_int l.number))))
      4 regions
  in
  let gutter marked =
    if marked then styled `Red "  \u{258c}" else plain "   "
  in
  List.iteri
    (fun i region ->
      if i > 0 then
        put k [ styled `Faint "   \u{00b7}\u{00b7}\u{00b7}\u{00b7}\u{00b7}" ];
      List.iter
        (fun l ->
          put_line k
            (rstrip
               (render ~ansi:k.ansi
                  [
                    gutter l.marked;
                    plain (spf "%*d \u{2502} " digits l.number);
                    plain l.text;
                  ])))
        region)
    regions

(* Every producer goes through the typed report entry points below, so a
   new constructor is a design amendment, not a convenience. Hint carries
   no spans by construction: no color in any hint. *)
type section =
  | Line of span list
  | Hint of string
  | Rows of { margin : string; columns : column list; rows : span list list }
  | Excerpt of excerpt
  | Rule of string option

(* Padding sits outside a styled cell, and the rendered row is stripped of
   trailing spaces after styling, so a row whose last cell is empty sheds
   the padding before it. *)
let put_rows k ~margin ~columns rows =
  let widths =
    List.map
      (fun (i, (c : column)) ->
        List.fold_left
          (fun w cells ->
            match List.nth_opt cells i with
            | Some cell -> max w (width [ cell ])
            | None -> w)
          (Option.value c.width ~default:0)
          rows)
      (List.mapi (fun i c -> (i, c)) columns)
  in
  List.iter
    (fun cells ->
      let line =
        List.concat
          (List.mapi
             (fun i cell ->
               match (List.nth_opt columns i, List.nth_opt widths i) with
               | Some c, Some w ->
                   let pad =
                     plain (String.make (max 0 (w - width [ cell ])) ' ')
                   in
                   plain c.gap
                   ::
                   (match c.align with
                   | `Right -> [ pad; cell ]
                   | `Left -> [ cell; pad ])
               | _, _ -> [])
             cells)
      in
      put_line k (rstrip (render ~ansi:k.ansi (plain margin :: line))))
    rows

let render_section k = function
  | Line spans -> put k spans
  | Hint line -> put k [ plain line ]
  | Rows { margin; columns; rows } -> put_rows k ~margin ~columns rows
  | Excerpt e -> excerpt k e
  | Rule label -> put k [ styled `Faint (rule ~width:rule_width label) ]

let print ~out ~ansi sections =
  let k = { out; ansi } in
  List.iter (render_section k) sections;
  Pp.flush out ()

(* Non-empty parts, one blank line between two. *)
let join parts =
  List.concat
    (List.mapi
       (fun i part -> if i > 0 then Line [] :: part else part)
       (List.filter (function [] -> false | _ :: _ -> true) parts))

(* Coverage *)

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

(* Red says which numbers need work: those below the gate the project
   chose, or below 80 when it chose none. *)
let percentage_span ~min ~visited ~total text =
  if coverage_percentage ~visited ~total < Option.value min ~default:80. then
    styled `Red text
  else plain text

(* The gate compares the raw percentage, and the line states the
   measurement as a fraction of integers beside its rounding, so no
   printed comparison is one its own digits can contradict. *)
let coverage_line ~min ~visited ~total =
  let pct = coverage_percentage ~visited ~total in
  [
    plain "coverage: ";
    percentage_span ~min ~visited ~total (spf "%.1f%%" pct);
    plain (spf " (%d/%d points)" visited total);
  ]
  @
  match min with
  | None -> []
  | Some min ->
      [
        plain (spf ", minimum %g%%: " min);
        (if pct >= min then styled `Green "ok" else styled `Red "FAILED");
      ]

let coverage_report ~mode ~min (c : coverage) =
  let widest f = List.fold_left (fun w file -> max w (f file)) 0 c.files in
  let digits n = String.length (string_of_int n) in
  let visited_width = widest (fun (f : coverage_file) -> digits f.visited) in
  let total_width = widest (fun (f : coverage_file) -> digits f.total) in
  let points_width =
    max (String.length "points") (visited_width + 1 + total_width)
  in
  let file_width =
    max (String.length "file") (widest (fun (f : coverage_file) -> cols f.file))
  in
  (* A row after its eight columns of margin and percentage. *)
  let lead ~points ~file =
    spf "    %s   %s   " (pad points_width points) (pad file_width file)
  in
  let after_cover ~points ~file ~note = rstrip (lead ~points ~file ^ note) in
  (* The hint says how to see what [(+N more)] hides; under [`Full] the
     source follows the table. *)
  let header =
    Line
      [
        styled `Faint
          ("   cover"
          ^ after_cover ~points:"points" ~file:"file"
              ~note:
                (match mode with
                | `Report -> "uncovered lines (-u shows the source)"
                | `Full -> "uncovered lines"));
      ]
  in
  let row (f : coverage_file) =
    let note =
      if f.stale then "stale: the source changed; re-run the instrumented tests"
      else if f.uncovered <> [] then bounded_ranges f.uncovered
        (* Unvisited points without a line: the source was not found. *)
      else if f.visited < f.total then "(source not found)"
      else ""
    in
    Line
      [
        plain "  ";
        percentage_span ~min ~visited:f.visited ~total:f.total
          (spf "%5.1f%%"
             (coverage_percentage ~visited:f.visited ~total:f.total));
        plain
          (after_cover
             ~points:
               (spf "%*d/%-*d" visited_width f.visited total_width f.total)
             ~file:f.file ~note);
      ]
  in
  let source (f : coverage_file) =
    match (mode, f.source) with
    | `Full, Some source when f.uncovered <> [] ->
        [
          Line
            [
              styled `Bold f.file;
              plain ": ";
              percentage_span ~min ~visited:f.visited ~total:f.total
                (spf "%.1f%%"
                   (coverage_percentage ~visited:f.visited ~total:f.total));
              plain (spf " (%d/%d)" f.visited f.total);
            ];
          Line [];
          Excerpt { source; marked_lines = f.uncovered };
        ]
    | (`Full | `Report), (Some _ | None) -> []
  in
  let table =
    match c.files with [] -> [] | files -> header :: List.map row files
  in
  let outcome =
    [ Line (coverage_line ~min ~visited:c.visited ~total:c.total) ]
  in
  match join (List.map source c.files) with
  | [] -> table @ outcome
  | sources -> join [ table; sources; outcome ]

(* Mutation *)

type witness = { test : string; loc : Loc.t option; exe : string option }

(* The identifier arrives spelled by the runtime's [id_to_string]: the
   report prints it and hands it to [--arm] without re-spelling it. *)
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
  unreached : (string * int) list;
  killed : int;
  not_tested : int;
  scope : scope;
}

(* A build action's suite is reached through dune alone. [--force] is
   there because dune replays a cached test action, and an arming a
   replayed run swallows arms nothing; the backend is named because a
   plain build carries no mutant. *)
let reproduce_line ~invocation ~selection id =
  match invocation with
  | `Exe cmd -> spf "reproduce: %s%s%s" cmd (arm_flag (Some id)) selection
  | `Mirrors ->
      spf
        "reproduce: %s%sdune runtest --force --instrument-with \
         ppx_windtrap.mutate"
        (arm_mirror (Some id)) selection

(* A survivor of a narrowed run survived that selection only, and a test
   left out of it may kill the mutant: the command restates each
   selection flag of the run, in the spelling [reproduce_line] puts it.
   [--failed] has no mirror. *)
let selection_words invocation (c : Run.config) =
  let one flag var value =
    Option.to_list (Option.map (fun v -> (flag, var, [ v ])) value)
  in
  let many flag var = function [] -> [] | values -> [ (flag, var, values) ] in
  let items =
    one "-f" "WINDTRAP_FILTER" (Option.map shell_quote c.Run.filter)
    @ one "-e" "WINDTRAP_EXCLUDE" (Option.map shell_quote c.Run.exclude)
    @ many "--tag" "WINDTRAP_TAG" (List.map shell_word c.Run.tags)
    @ many "--exclude-tag" "WINDTRAP_EXCLUDE_TAG"
        (List.map shell_word c.Run.exclude_tags)
    @ one "--shard" "WINDTRAP_SHARD"
        (Option.map (fun (k, n) -> spf "%d/%d" k n) c.Run.shard)
  in
  match invocation with
  | `Exe _ ->
      String.concat ""
        (List.concat_map
           (fun (flag, _, values) -> List.map (spf " %s %s" flag) values)
           items)
      ^ if c.Run.failed_only then " --failed" else ""
  | `Mirrors ->
      String.concat ""
        (List.map
           (fun (_, var, values) -> spf "%s=%s " var (String.concat "," values))
           items)

(* A survivor is drawn as a failure block is, title, source line and
   facts, because it is a defect report about the tests it names. A
   survivor always names one: an unreached mutant is another finding. The
   executables are counted only when the reaching tests name several. *)
let survivor_block ~exe_width (s : survivor) =
  let m = s.mutant in
  let title =
    Line
      [
        plain "  ";
        styled `Red "SURVIVED";
        plain "  ";
        styled `Bold m.id;
        plain (spf "  %s \u{2192} %s" m.before m.after);
      ]
  in
  let source =
    match Option.map source_lines m.source with
    | Some lines when 1 <= m.line && m.line <= Array.length lines ->
        let gutter, text = source_excerpt m.line lines.(m.line - 1) in
        [ Line [ plain (indent ^ "  "); styled `Faint gutter; plain text ] ]
    | Some _ | None -> []
  in
  let reaching =
    match s.witnesses with
    | [] -> []
    | witnesses ->
        let n = List.length witnesses in
        let executables =
          List.length
            (List.sort_uniq String.compare
               (List.filter_map (fun (w : witness) -> w.exe) witnesses))
        in
        let sentence =
          if n = 1 then "1 test ran this line and did not fail:"
          else if executables > 1 then
            spf "%d tests in %d executables ran this line and none failed:" n
              executables
          else spf "%d tests ran this line and none failed:" n
        in
        let left gap width = { gap; align = `Left; width } in
        let test (w : witness) =
          [
            plain w.test;
            styled `Faint
              (match w.loc with Some loc -> Loc.to_string loc | None -> "");
          ]
        in
        let columns, cells =
          match exe_width with
          | None -> ([ left "" None; left "  " None ], test)
          | Some _ ->
              ( [ left "" exe_width; left "  " None; left "  " None ],
                fun (w : witness) ->
                  plain (Option.value w.exe ~default:"") :: test w )
        in
        [
          Line [];
          Line [ plain (indent ^ sentence) ];
          Rows
            { margin = indent ^ "  "; columns; rows = List.map cells witnesses };
        ]
  in
  (title :: source) @ reaching

(* Zero terms are omitted, as a passing suite prints no failure count. The
   reached count is a sum of the other terms and not a measurement, so the
   line cannot disagree with the blocks above it. *)
let mutation_summary_spans (m : mutation) =
  let survived = List.length m.survivors in
  let unreached = List.length m.unreached in
  let reached =
    let n = m.killed + survived + m.not_tested in
    match m.scope with
    | Suite -> spf "%d reached by this suite" n
    | Selected tests ->
        spf "%d reached by the %d selected test%s" n tests
          (if tests = 1 then "" else "s")
    | Executables _ -> spf "%d reached" n
  in
  let term n make = if n > 0 then [ make n ] else [] in
  let terms =
    (if survived > 0 then
       [
         [ styled `Red (spf "%d survived" survived); plain (" of " ^ reached) ];
       ]
     else [ [ plain reached ] ])
    @ term m.killed (fun n -> [ styled `Green (spf "%d killed" n) ])
    @ term unreached (fun n -> [ styled `Yellow (spf "%d never reached" n) ])
    @ term m.not_tested (fun n -> [ plain (spf "%d not tested" n) ])
    @
    match m.scope with
    | Executables n ->
        [ [ plain (spf "%d executable%s" n (if n = 1 then "" else "s")) ] ]
    | Suite | Selected _ -> []
  in
  let rec separated = function
    | [] -> []
    | [ last ] -> last
    | term :: rest -> term @ (plain ", " :: separated rest)
  in
  plain "mutants: " :: separated terms

(* One row per file, not a block per mutant: what a reader does with an
   unreached mutant is write a test for its lines, and a project has
   hundreds of them. *)
let unreached_section unreached =
  let files = List.sort_uniq String.compare (List.map fst unreached) in
  let lines_of file =
    List.filter_map
      (fun (f, line) -> if String.equal f file then Some line else None)
      unreached
  in
  let count_width =
    List.fold_left
      (fun w file ->
        max w (String.length (string_of_int (List.length (lines_of file)))))
      0 files
  in
  let file_width = List.fold_left (fun w file -> max w (cols file)) 0 files in
  let lead file = spf "  %s   lines " (pad file_width file) in
  let row file =
    let lines = lines_of file in
    Line
      [
        plain "  ";
        styled `Yellow (spf "%*d" count_width (List.length lines));
        plain (lead file ^ bounded_ranges (List.sort_uniq Int.compare lines));
      ]
  in
  match files with
  | [] -> []
  | _ :: _ ->
      Rule (Some (spf "never reached (%d)" (List.length unreached)))
      :: List.map row files
      @ [ Rule None ]

(* The command arms the first survivor printed, so it runs as pasted. *)
let outcome ~invocation ~selection (m : mutation) =
  (match m.survivors with
    | [] -> []
    | first :: _ ->
        [ Hint (reproduce_line ~invocation ~selection first.mutant.id) ])
  @ [ Line (mutation_summary_spans m) ]

(* The closing rule is decided on [m.survivors], not on what was printed: the
   caller hands over the survivors whose blocks it committed. *)
let mutation_closing ~(config : Run.config) (m : mutation) =
  let invocation = config.Run.invocation in
  let selection = selection_words invocation config in
  let rest =
    join [ unreached_section m.unreached; outcome ~invocation ~selection m ]
  in
  match (m.survivors, m.unreached) with
  | [], [] -> rest
  | [], _ :: _ -> Line [] :: rest
  | _ :: _, _ -> Rule None :: Line [] :: rest

(* The executables are one column for the report, read down it. *)
let mutation_report ~invocation (m : mutation) =
  let witnesses =
    List.concat_map (fun (s : survivor) -> s.witnesses) m.survivors
  in
  let exe_width =
    if List.exists (fun (w : witness) -> Option.is_some w.exe) witnesses then
      Some
        (List.fold_left
           (fun width (w : witness) ->
             max width (cols (Option.value w.exe ~default:"")))
           0 witnesses)
    else None
  in
  let survivors =
    match m.survivors with
    | [] -> []
    | survivors ->
        Rule (Some (spf "survivors (%d)" (List.length survivors)))
        :: join (List.map (survivor_block ~exe_width) survivors)
        @ [ Rule None ]
  in
  join
    [
      survivors;
      unreached_section m.unreached;
      outcome ~invocation ~selection:"" m;
    ]
