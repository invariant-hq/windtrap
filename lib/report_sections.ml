(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf

(* Every report has one width, so a pipe and a wide terminal print the same
   bytes. *)
let rule_width = 58
let indent = "    "
let max_lines = 10
let max_value_bytes = 800
let max_headline_chars = 80
let max_diff_lines = 200
let max_proposed_lines = 20
let max_ranges = 8
let take n l = List.filteri (fun i _ -> i < n) l
let counted n noun = strf "%d %s%s" n noun (if n = 1 then "" else "s")

let separated sep parts =
  List.concat
    (List.mapi (fun i part -> if i > 0 then sep :: part else part) parts)

let trim_end is_blank s =
  let rec stop i = if i > 0 && is_blank s.[i - 1] then stop (i - 1) else i in
  String.sub s 0 (stop (String.length s))

(* Spans *)

(* A span's text prints through [Text.escape_controls]. Windtrap's own text
   holds no control byte, so the escape reaches only what came from outside.
   It is not injective: every decision about a value is made on the raw bytes,
   and only column arithmetic counts escaped glyphs. *)
type span = { style : Pp.style option; text : string }

let plain text = { style = None; text }
let styled style text = { style = Some style; text }

let sgr = function
  | `Bold -> "\027[1m"
  | `Faint -> "\027[2m"
  | `Red -> "\027[31m"
  | `Green -> "\027[32m"
  | `Yellow -> "\027[33m"
  | `Bold_red -> "\027[1;31m"
  | `Bold_green -> "\027[1;32m"

(* An empty text stays bare, so a line assembled from optional fragments
   carries no empty style. *)
let render ~ansi spans =
  String.concat ""
    (List.map
       (fun { style; text } ->
         let text = Text.escape_controls text in
         match style with
         | Some style when ansi && text <> "" -> sgr style ^ text ^ "\027[0m"
         | Some _ | None -> text)
       spans)

(* The escape is per byte, so the columns of a prefix are those of the prefix
   of the escaped text. *)
let cols text = Text.length_utf8 (Text.escape_controls text)
let width spans = List.fold_left (fun w { text; _ } -> w + cols text) 0 spans

(* Names and command words *)

let release_title = "fixture release"
let is_control c = c < ' ' || c = '\127'

(* A control byte inside POSIX single quotes would break the line the word
   sits on; such a word takes the [$'…'] form. *)
let shell_quote s =
  if not (String.exists is_control s) then
    "'" ^ String.concat "'\\''" (String.split_on_char '\'' s) ^ "'"
  else
    let escape = function
      | '\'' -> "\\'"
      | '\\' -> "\\\\"
      | '\n' -> "\\n"
      | '\t' -> "\\t"
      | '\r' -> "\\r"
      | c when is_control c -> strf "\\x%02x" (Char.code c)
      | c -> String.make 1 c
    in
    "$'"
    ^ String.concat "" (List.map escape (List.of_seq (String.to_seq s)))
    ^ "'"

let shell_word s =
  let bare = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
    | '_' | '-' | '.' | '/' | ':' | '=' | '+' | ',' | '@' | '%' -> true
    | _ -> false
  in
  if s <> "" && String.for_all bare s then s else shell_quote s

(* [dune exec] looks a word without a [/] up as a program name. The [/] is
   the separator of every path [Os.display_path] prints. *)
let dune_exec ~mutate path : Run.invocation =
  let backend =
    if mutate then "--instrument-with ppx_windtrap.mutate " else ""
  in
  let path = if String.contains path '/' then path else "./" ^ path in
  `Exe (strf "dune exec %s%s --" backend (shell_word path))

(* Failure facts *)

let shown_text (t : Failure.text) =
  if Failure.is_cut t then Text.mark_truncated ~length:t.length t.kept
  else t.kept

(* A single-line value is cut in its carried bytes, so the elided count is
   theirs. *)
let elided s = String.length s > max_value_bytes
let shown s = Text.elide_middle max_value_bytes ~show:Fun.id s

(* An example is never shrunk. *)
let case_name ~examples ~case_index ~shrink_steps =
  if examples then strf "example %d" (case_index + 1)
  else if shrink_steps = 0 then strf "case %d" case_index
  else strf "case %d, shrunk %s" case_index (counted shrink_steps "step")

let timeout_fact ~limit (case : Failure.timed_case option) =
  match case with
  | None -> strf "timed out after %gs" limit
  | Some { case_index; examples; passed; _ } ->
      strf "timed out after %gs in %s (%d passed)" limit
        (case_name ~examples ~case_index ~shrink_steps:0)
        passed

(* Plain quotes, not [%S]: a path's UTF-8 is not byte-escaped. *)
let baseline_subject = function
  | Failure.Literal { exact = false } -> "expect"
  | Failure.Literal { exact = true } -> "expect_exact"
  | Failure.File path -> strf "expect_file \"%s\"" (Os.display_path path)

let needle_word = function
  | Failure.Prefix -> "prefix"
  | Failure.Suffix -> "suffix"
  | Failure.Anywhere | Failure.Ordered _ -> "needle"

let containment_verdict ~demand ~found_at =
  match (demand, found_at) with
  | Failure.Ordered { resumed_at; _ }, Some at ->
      strf "found at byte %d, before the search resumed at byte %d" at
        resumed_at
  | Failure.Ordered { resumed_at; _ }, None ->
      strf "not found at or after byte %d" resumed_at
  | Failure.Anywhere, Some at -> strf "found at byte %d" at
  | Failure.Prefix, Some at -> strf "found at byte %d, not at the start" at
  | Failure.Suffix, Some at -> strf "found at byte %d, not at the end" at
  | (Failure.Anywhere | Failure.Prefix | Failure.Suffix), None -> "not found"

(* Two renderings with equal lines differ by one trailing newline, which a
   line diff cannot show. *)
let newline_fact ~expected ~actual =
  strf "values differ only by a trailing newline (on the %s side)"
    (if String.length actual > String.length expected then "actual"
     else "expected")

(* A cut side is compared on what the failure kept of it. *)
let whole (expected : Failure.text) (actual : Failure.text) =
  not (Failure.is_cut expected || Failure.is_cut actual)

let agree_fact ~(expected : Failure.text) ~(actual : Failure.text) =
  strf
    "the sides agree on the %d bytes a failure keeps of each (expected %d \
     bytes, actual %d bytes)"
    (String.length expected.kept)
    expected.length actual.length

let cut_fact ~(expected : Failure.text) ~(actual : Failure.text) =
  let side (name, (t : Failure.text)) =
    if not (Failure.is_cut t) then None
    else
      Some
        (strf "the first %d of the %d bytes of %s" (String.length t.kept)
           t.length name)
  in
  "the diff covers "
  ^ String.concat " and "
      (List.filter_map side [ ("expected", expected); ("actual", actual) ])

let diff_lines hunks =
  List.fold_left (fun n h -> n + 1 + List.length h.Diff.lines) 0 hunks

(* Failure projections *)

let labeled_msg (f : Failure.t) =
  let msg = Option.map shown_text f.msg in
  match f.subtest with
  | [] -> msg
  | components -> (
      let label = Test_tree.path_to_string components in
      match msg with None -> Some label | Some m -> Some (label ^ ": " ^ m))

let is_subtest_failure (f : Failure.t) = f.subtest <> []

let headline (f : Failure.t) =
  let fact =
    match f.kind with
    | Failure.Equality { not_ = true; expected; _ } ->
        "both sides equal: " ^ shown_text expected
    | Failure.Equality { expected; actual; diffable = false; _ } ->
        strf "expected %s, got %s" (shown_text expected) (shown_text actual)
    | Failure.Equality { expected = e; actual = a; _ } -> (
        let whole = whole e a and expected = e.kept and actual = a.kept in
        if String.equal expected actual then
          if whole then "both sides render as: " ^ expected
          else agree_fact ~expected:e ~actual:a
        else if
          not (String.contains expected '\n' || String.contains actual '\n')
        then strf "expected %s, got %s" (shown_text e) (shown_text a)
        else
          match Diff.hunks ~expected ~actual () with
          | [] when whole -> newline_fact ~expected ~actual
          | [] -> cut_fact ~expected:e ~actual:a
          | hunks ->
              strf "expected and actual differ (%d diff lines)"
                (diff_lines hunks))
    | Failure.Containment { needle; found_at; haystack_length; demand; _ } -> (
        let needle = shown_text needle in
        match (demand, found_at) with
        | Failure.Ordered { index; resumed_at }, Some at ->
            strf "element %d %S out of order: at byte %d, before byte %d" index
              needle at resumed_at
        | Failure.Ordered { index; resumed_at }, None ->
            strf
              "element %d %S not found at or after byte %d (%d-byte haystack)"
              index needle resumed_at haystack_length
        | (Failure.Anywhere | Failure.Prefix | Failure.Suffix), Some _ ->
            strf "%s %S %s" (needle_word demand) needle
              (containment_verdict ~demand ~found_at)
        | (Failure.Anywhere | Failure.Prefix | Failure.Suffix), None ->
            strf "%s %S not found (%d-byte haystack)" (needle_word demand)
              needle haystack_length)
    | Failure.Raise { expected = Some e; actual = Some a; _ } ->
        strf "expected exception %s, raised %s" (shown_text e) (shown_text a)
    | Failure.Raise { expected = Some e; actual = None; _ } ->
        strf "expected exception %s, none raised" (shown_text e)
    | Failure.Raise { expected = None; actual = Some a; predicate; _ } ->
        if predicate then
          "exception did not satisfy the predicate: " ^ shown_text a
        else "uncaught exception: " ^ shown_text a
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
        (* [shrunk 100 steps] alone reads as a converged search. *)
        strf "property failed (%s%s):%s%s"
          (case_name ~examples ~case_index ~shrink_steps)
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
          (shown_text (Option.value summary ~default:rendered))
    | Failure.Timeout { limit; case } -> timeout_fact ~limit case
    | Failure.Message m -> (
        match shown_text m with "" -> "(empty failure message)" | m -> m)
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

(* Command hints *)

let filter_flag = function Some f -> " -f " ^ shell_quote f | None -> ""

(* An armed run's failures are the mutant's: a command that runs the test
   without it passes. *)
let arm_flag = function Some id -> " --arm " ^ shell_word id | None -> ""

let arm_mirror = function
  | Some id -> strf "WINDTRAP_MUTATE_ARM=%s " (shell_word id)
  | None -> ""

(* A baseline failure whose correction a command accepts, with whether its
   baseline is missing. A withheld correction accepts nothing. *)
let acceptable (f : Failure.t) =
  match f.kind with
  | Failure.Baseline { baseline; state = Failure.Missing _; withheld = None } ->
      Some (baseline, true)
  | Failure.Baseline { baseline; state = Failure.Mismatch _; withheld = None }
    ->
      Some (baseline, false)
  | Failure.Baseline { withheld = Some _; _ }
  | Failure.Baseline { state = Failure.Unresolvable _; _ }
  | Failure.Equality _ | Failure.Containment _ | Failure.Raise _
  | Failure.Property _ | Failure.Timeout _ | Failure.Message _ ->
      None

(* Promotion fills a file and never creates one: a missing file must exist
   before dune's [diff?] registers its correction. *)
let promote_line (f : Failure.t) =
  match acceptable f with
  | None -> None
  | Some (Failure.File path, true) ->
      let file = Os.display_path path in
      Some
        (strf "accept: touch %s && dune runtest; dune promote %s"
           (shell_quote file) (shell_word file))
  | Some (Failure.File path, false) ->
      Some ("accept: dune promote " ^ shell_word (Os.display_path path))
  | Some (Failure.Literal _, _) -> (
      match f.loc with
      | Some loc -> Some ("accept: dune promote " ^ shell_word loc.Loc.file)
      | None -> Some "accept: dune promote")

let withheld_fact (f : Failure.t) =
  let kept_none reason = Some ("no correction was kept: " ^ reason) in
  match f.kind with
  | Failure.Baseline
      { state = Failure.Missing _ | Failure.Mismatch _; withheld = Some why; _ }
    -> (
      match why with
      | Failure.Refused { line; reason } ->
          Some (strf "correction refused (line %d): %s" line reason)
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
  | Failure.Raise _ | Failure.Property _ | Failure.Timeout _ | Failure.Message _
    ->
      None

let hints ?armed ?(invocation = `Mirrors) failures =
  let distinct lines =
    List.rev
      (List.fold_left
         (fun acc line -> if List.mem line acc then acc else line :: acc)
         [] lines)
  in
  match armed with
  | Some _ -> []
  | None -> (
      distinct (List.filter_map withheld_fact failures)
      @
      match invocation with
      | `Mirrors -> distinct (List.filter_map promote_line failures)
      | `Exe _ -> [])

(* A command that runs a run's tests again restates the run's selection.
   [--failed] reads the last-failed store under the run's [-o]. It has no
   mirror, and a pattern's mirror holds one pattern: under the mirrors those
   are left out, and the command runs more tests, never fewer. *)
let selection_words invocation (c : Run.config) =
  let flag name var = function [] -> [] | values -> [ (name, var, values) ] in
  let patterns name var values =
    match invocation with
    | `Mirrors when List.compare_length_with values 1 > 0 -> []
    | `Mirrors | `Exe _ -> flag name var (List.map shell_quote values)
  in
  let flags =
    patterns "-f" "WINDTRAP_FILTER" c.filter
    @ patterns "-e" "WINDTRAP_EXCLUDE" c.exclude
    @ flag "--tag" "WINDTRAP_TAG" (List.map shell_word c.tags)
    @ flag "--exclude-tag" "WINDTRAP_EXCLUDE_TAG"
        (List.map shell_word c.exclude_tags)
    @ flag "--shard" "WINDTRAP_SHARD"
        (Option.to_list (Option.map (fun (k, n) -> strf "%d/%d" k n) c.shard))
  in
  match invocation with
  | `Exe _ ->
      String.concat ""
        (List.concat_map
           (fun (name, _, values) -> List.map (strf " %s %s" name) values)
           flags)
      ^
      if not c.failed_only then ""
      else if String.equal c.log_dir (Os.default_log_dir ()) then " --failed"
      else strf " -o %s --failed" (shell_word (Os.display_artifact c.log_dir))
  | `Mirrors ->
      String.concat ""
        (List.map
           (fun (_, var, values) ->
             strf "%s=%s " var (String.concat "," values))
           flags)

(* A build action's corrections are dune's to promote, file by file. *)
let accept ?armed ?(invocation = `Mirrors) ~tests failures =
  match (armed, invocation) with
  | Some _, _ | None, `Mirrors -> None
  | None, `Exe cmd ->
      if not (List.exists (fun f -> Option.is_some (acceptable f)) failures)
      then None
      else
        Some
          (strf "accept: %s -u%s" cmd
             (match tests with
             | `Run config -> selection_words invocation config
             | `Filter filter -> filter_flag filter))

(* The seed and the case count of a failure whose case was generated. *)
let drawn (f : Failure.t) =
  match f.kind with
  | Failure.Property { examples = false; root; count; _ }
  | Failure.Timeout { case = Some { examples = false; root; count; _ }; _ } ->
      Some (root, count)
  | Failure.Property { examples = true; _ }
  | Failure.Timeout { case = Some { examples = true; _ } | None; _ }
  | Failure.Equality _ | Failure.Containment _ | Failure.Raise _
  | Failure.Baseline _ | Failure.Message _ ->
      None

(* A late case replays only under at least as many cases as the failing run
   generated, so the largest count is restated. An armed action exists only
   in the instrumented build. *)
let replay ?armed ?(invocation = `Mirrors) ~tests failures =
  match List.filter_map drawn failures with
  | [] -> None
  | (root, _) :: _ as cases ->
      let count =
        List.fold_left
          (fun acc (_, count) ->
            match (acc, count) with
            | Some a, Some c -> Some (max a c)
            | None, c | c, None -> c)
          None cases
      in
      let seed = Seed.to_string root in
      Some
        (match invocation with
        | `Exe cmd ->
            strf "replay: %s%s --seed %s%s%s" cmd (arm_flag armed) seed
              (match count with
              | Some n -> strf " --prop-count %d" n
              | None -> "")
              (match tests with
              | `Run config -> selection_words invocation config
              | `Filter filter -> filter_flag filter)
        | `Mirrors ->
            strf "replay: %sWINDTRAP_SEED=%s %s%sdune runtest%s"
              (arm_mirror armed) seed
              (match count with
              | Some n -> strf "WINDTRAP_PROP_COUNT=%d " n
              | None -> "")
              (match tests with
              | `Run config -> selection_words invocation config
              | `Filter (Some f) -> strf "WINDTRAP_FILTER=%s " (shell_quote f)
              | `Filter None -> "")
              (match armed with
              | Some _ -> " --instrument-with ppx_windtrap.mutate"
              | None -> ""))

(* Entries *)

(* An entry is a list of lines, each a list of spans, at the entry's own
   column; a blank line stays bare when it is indented. *)
let indented pad = function [] -> [] | line -> plain pad :: line
let line s = [ plain s ]
let faint s = [ styled `Faint s ]
let lines ~lead s = List.map (fun l -> line (lead ^ l)) (Text.split_lines s)

(* Each line of a value in [style], so that no style spans a line. *)
let block style value =
  if String.contains value '\n' then
    List.map (fun l -> [ plain "  "; styled style l ]) (Text.split_lines value)
  else [ [ plain "  "; styled style (shown value) ] ]

(* Ten columns fit [expected], the longest name of a field but a raise's. *)
let field ?(gutter = 10) name =
  [ styled `Faint name; plain (String.make (gutter - String.length name) ' ') ]

let more what n = faint (strf "\u{2026} (+%d more %s)" n what)

let capped max items ~more =
  let rest = List.length items - max in
  take max items @ if rest > 0 then [ more rest ] else []

(* [s] with its byte ranges [spans], ascending and disjoint, in [style]. *)
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

(* A span of spaces, or an empty one, has no glyph to colour. *)
let colour_shows s spans =
  not
    (List.exists
       (fun { Diff.start; length } ->
         String.for_all (fun c -> c = ' ') (String.sub s start length))
       spans)

(* Whether a [~] line lands under the code points of [s] as it prints: a code
   point past Latin Extended-B has no fixed width, and neither has a tab
   unless the [~] line repeats it ([tabs]). *)
let aligns ~tabs s =
  let s = Text.escape_controls s in
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

(* The [~] line under [spans] of [s] printed [lead] columns in: as many [~] as
   a span has columns, at least one. *)
let marker ~lead ~style s spans =
  match spans with
  | [] -> []
  | { Diff.start = first; _ } :: _ ->
      let column i = cols (String.sub s 0 i) in
      let tildes = Buffer.create 16 in
      let mark col { Diff.start; length } =
        let from = column start
        and w = max 1 (cols (String.sub s start length)) in
        Buffer.add_string tildes (String.make (max 0 (from - col)) ' ');
        Buffer.add_string tildes (String.make w '~');
        max from col + w
      in
      ignore (List.fold_left mark (column first) spans);
      [
        [
          plain (String.make (lead + column first) ' ');
          styled style (Buffer.contents tildes);
        ];
      ]

let marked_value ~seen ~aligned ~style ~before s spans =
  (before @ highlight style s spans)
  ::
  (if aligned && ((not seen) || not (colour_shows s spans)) then
     marker ~lead:(width before) ~style s spans
   else [])

(* A [-] line and a [+] line that differ in trailing blanks alone print as two
   equal lines. The [~] line under the [-] line repeats its tabs, so that any
   tab stops align it, and marks the blanks of the [-] line that the [+] line
   lacks, or of the [+] line when the [-] line has none. *)
let trailing_mark ~deleted ~inserted =
  let stem = trim_end (fun c -> c = ' ' || c = '\t') in
  let rec shared i =
    if
      i < String.length deleted
      && i < String.length inserted
      && deleted.[i] = inserted.[i]
    then shared (i + 1)
    else i
  in
  let lead = String.sub deleted 0 (shared 0) in
  let marked =
    if String.length deleted > String.length lead then String.length deleted
    else String.length inserted
  in
  if
    String.equal deleted inserted
    || (not (String.equal (stem deleted) (stem inserted)))
    || not (aligns ~tabs:true lead)
  then None
  else
    let blank = function
      | '\t' -> Some '\t'
      | c when Char.code c land 0xC0 = 0x80 -> None
      | _ -> Some ' '
    in
    let pad =
      Seq.filter_map blank (String.to_seq (Text.escape_controls lead))
    in
    Some (String.of_seq pad, String.make (marked - String.length lead) '~')

(* A [~] line belongs to the [-] line above it and is no diff line. Only a
   lone [-] line followed by a lone [+] line is a pair: in a longer run the
   lines answer each other in no fixed order. *)
let hunk_lines hunks =
  let rec lines ~after_delete = function
    | [] -> []
    | Diff.Keep s :: rest ->
        [ line ("  " ^ s) ] :: lines ~after_delete:false rest
    | Diff.Insert s :: rest ->
        [ [ styled `Red ("+ " ^ s) ] ] :: lines ~after_delete:false rest
    | Diff.Delete s :: rest ->
        (* Green is the expected side, as everywhere in a report. *)
        let mark =
          match rest with
          | Diff.Insert inserted :: ([] | (Diff.Keep _ | Diff.Delete _) :: _)
            when not after_delete ->
              trailing_mark ~deleted:s ~inserted
          | Diff.Insert _ :: _ | (Diff.Keep _ | Diff.Delete _) :: _ | [] -> None
        in
        let mark =
          match mark with
          | Some (pad, tildes) -> [ [ plain ("  " ^ pad); styled `Red tildes ] ]
          | None -> []
        in
        ([ styled `Green ("- " ^ s) ] :: mark) :: lines ~after_delete:true rest
  in
  (* Unified diff numbers a side with no line by the line before it. *)
  let range start count =
    strf "%d,%d" (if count = 0 then start - 1 else start) count
  in
  let head (h : Diff.hunk) =
    [
      [
        styled `Faint
          (strf "@@ -%s +%s @@"
             (range h.expected_start h.expected_count)
             (range h.actual_start h.actual_count));
      ];
    ]
  in
  let rows =
    List.concat_map (fun h -> head h :: lines ~after_delete:false h.lines) hunks
  in
  List.concat
    (capped max_diff_lines rows ~more:(fun n -> [ more "diff lines" n ]))

(* Two single-line renderings after their anchors. What changed is marked
   when [marked] and no side is elided, since an elided side has no columns
   left to mark; otherwise each side prints whole in its colour. *)
let sides ~seen ~anchors:(expected_anchor, actual_anchor) ~marked ~expected
    ~actual =
  let gutter =
    2 + max (String.length expected_anchor) (String.length actual_anchor)
  in
  let expected_spans, actual_spans =
    match
      if marked && not (elided expected || elided actual) then
        Diff.refine ~expected ~actual
      else None
    with
    | None -> ([], [])
    | Some { Diff.expected_spans; actual_spans } ->
        (expected_spans, actual_spans)
  in
  let refined = expected_spans <> [] || actual_spans <> [] in
  let expected = shown expected and actual = shown actual in
  let aligned = aligns ~tabs:false expected && aligns ~tabs:false actual in
  let side anchor ~colour ~style value spans =
    let before = field ~gutter anchor in
    if refined then marked_value ~seen ~aligned ~style ~before value spans
    else [ before @ [ styled colour value ] ]
  in
  side expected_anchor ~colour:`Green ~style:`Bold_green expected expected_spans
  @ side actual_anchor ~colour:`Red ~style:`Bold_red actual actual_spans

(* A cut pair that kept the same bytes says so instead of a diff, and a
   difference in a final newline is a fact of two whole texts only. *)
let text_diff ~headers ~show ~(expected : Failure.text) ~(actual : Failure.text)
    =
  let whole = whole expected actual in
  if (not whole) && String.equal expected.kept actual.kept then
    [ line (agree_fact ~expected ~actual) ]
  else
    let e = show expected.kept and a = show actual.kept in
    let diff =
      match Diff.hunks ~expected:e ~actual:a () with
      | [] when whole -> [ line (newline_fact ~expected:e ~actual:a) ]
      | [] -> []
      | hunks when headers ->
          [ styled `Faint "--- expected" ]
          :: [ styled `Faint "+++ actual" ]
          :: hunk_lines hunks
      | hunks -> hunk_lines hunks
    in
    if whole then diff
    else diff @ [ faint ("(" ^ cut_fact ~expected ~actual ^ ")") ]

let equality ~seen ~show ~(expected : Failure.text) ~(actual : Failure.text) =
  if whole expected actual && String.equal expected.kept actual.kept then
    let value = show expected.kept in
    let render_as = styled `Faint "both sides render as:" in
    (if String.contains value '\n' then [ render_as ] :: lines ~lead:"  " value
     else [ [ render_as; plain (" " ^ shown value) ] ])
    @ [ faint "the printer shows less than the equality compares" ]
  else if
    String.equal expected.kept actual.kept
    || String.contains (show expected.kept) '\n'
    || String.contains (show actual.kept) '\n'
  then text_diff ~headers:true ~show ~expected ~actual
  else
    sides ~seen ~anchors:("expected", "actual") ~marked:true
      ~expected:(show (shown_text expected))
      ~actual:(show (shown_text actual))

(* The anchors of a raise state the difference, so nothing is marked; [actual]
   is [None] when nothing was raised. *)
let raise_sides ~seen ~expected ~actual =
  let spans_lines s = String.contains s '\n' in
  match actual with
  | Some actual when not (spans_lines expected || spans_lines actual) ->
      sides ~seen
        ~anchors:("expected exception", "raised")
        ~marked:false ~expected ~actual
  | Some _ | None -> (
      let side anchor style value =
        if spans_lines value then faint (anchor ^ ":") :: block style value
        else
          [
            field ~gutter:(2 + String.length "expected exception") anchor
            @ [ styled style (shown value) ];
          ]
      in
      side "expected exception" `Green expected
      @
      match actual with
      | Some actual -> side "raised" `Red actual
      | None -> [ line "but no exception was raised" ])

(* A location's path is relative to the project root, and under [dune
   runtest] the working directory is inside [_build]: the root comes first,
   then the path as given. Finding the root reads the working directory, which
   a test may have removed. *)
let source_line file n =
  let open_source file =
    match open_in file with ic -> Some ic | exception Sys_error _ -> None
  in
  let under_root () =
    match Os.project_root () with
    | root -> open_source (Filename.concat root file)
    | exception Sys_error _ -> None
  in
  let rec nth ic k =
    match In_channel.input_line ic with
    | Some line -> if k = 1 then Some line else nth ic (k - 1)
    | None -> None
  in
  let ic =
    if n < 1 then None
    else if not (Filename.is_relative file) then open_source file
    else
      match under_root () with Some ic -> Some ic | None -> open_source file
  in
  Option.bind ic (fun ic ->
      Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> nth ic n))

(* A file's bytes are no more the report's than a test's are: the line prints
   dedented and bounded as a single-line value. *)
let source_spans line text =
  [
    plain "  ";
    styled `Faint (strf "%d \u{2502}" line);
    plain (match String.trim text with "" -> "" | text -> " " ^ shown text);
  ]

let rec entry ~seen ~excerpt ~inner (f : Failure.t) =
  let phase =
    match f.phase with
    | Failure.Body -> None
    | Failure.Setup -> Some "[setup]"
    | Failure.Teardown -> Some "[teardown]"
    | Failure.Release -> Some "[release]"
  in
  let location =
    match
      ( Option.map (styled `Yellow) phase,
        Option.map (fun loc -> styled `Faint (Loc.to_string loc)) f.loc )
    with
    | None, None -> []
    | Some part, None | None, Some part -> [ [ part ] ]
    | Some tag, Some loc -> [ [ tag; plain " "; loc ] ]
  in
  (* The blank line closes a block's head; an inner entry has none. *)
  let source =
    match f.loc with
    | Some { Loc.file; line; _ } when excerpt -> (
        match source_line file line with
        | Some text -> source_spans line text :: (if inner then [] else [ [] ])
        | None -> [])
    | Some _ | None -> []
  in
  let subtest =
    match f.subtest with
    | [] -> []
    | ([ _ ] as names) | _ :: names ->
        [ field "subtest" @ [ plain (Test_tree.path_to_string names) ] ]
  in
  let msg =
    match f.msg with Some msg -> lines ~lead:"" (shown_text msg) | None -> []
  in
  location @ source @ subtest @ msg @ facts ~seen ~excerpt f.kind

and facts ~seen ~excerpt = function
  | Failure.Equality { not_ = true; expected; _ } ->
      let expected = shown_text expected in
      if String.contains expected '\n' then
        line "both sides equal:" :: lines ~lead:"  " expected
      else [ line ("both sides equal: " ^ shown expected) ]
  | Failure.Equality { expected = claim; actual = value; diffable = false; _ }
    ->
      (* A claim describes the expected value and is never diffed. *)
      let value = shown_text value in
      (field "expected" @ [ styled `Green (shown (shown_text claim)) ])
      ::
      (if String.contains value '\n' then faint "actual:" :: block `Red value
       else [ field "actual" @ [ styled `Red (shown value) ] ])
  | Failure.Equality { expected; actual; _ } ->
      equality ~seen ~show:Fun.id ~expected ~actual
  | Failure.Containment
      {
        needle;
        found_at;
        haystack_length;
        excerpt = haystack;
        excerpt_offset = offset;
        demand;
      } ->
      let element =
        match demand with
        | Failure.Ordered { index; _ } ->
            [ field "element" @ [ plain (string_of_int index) ] ]
        | Failure.Anywhere | Failure.Prefix | Failure.Suffix -> []
      in
      (* Each end is escaped after the cut, so that no escape is split. *)
      let needle_line =
        field (needle_word demand)
        @ [
            plain
              ("\""
              ^ Text.elide_middle max_value_bytes ~show:String.escaped
                  (shown_text needle)
              ^ "\": "
              ^ containment_verdict ~demand ~found_at);
          ]
      in
      (* A window anchored on the cursor may have left an out-of-order
         occurrence behind. *)
      let occurrence =
        Option.bind found_at (fun at ->
            let start = at - offset in
            let length =
              min needle.Failure.length (String.length haystack - start)
            in
            if start >= 0 && length > 0 then Some { Diff.start; length }
            else None)
      in
      let value ~before line spans =
        marked_value ~seen ~aligned:(aligns ~tabs:false line) ~style:`Bold_red
          ~before line spans
      in
      (* An occurrence that spans lines is marked on its first. *)
      let rec rows start = function
        | [] -> []
        | line :: rest ->
            let stop = start + String.length line in
            let spans =
              match occurrence with
              | Some { Diff.start = at; length } when at >= start && at < stop
                ->
                  [
                    { Diff.start = at - start; length = min length (stop - at) };
                  ]
              | Some _ | None -> []
            in
            value ~before:[ plain "  " ] line spans @ rows (stop + 1) rest
      in
      let shown_haystack =
        if String.contains haystack '\n' then
          [ styled `Faint "haystack:" ] :: rows 0 (Text.split_lines haystack)
        else
          value ~before:(field "haystack") haystack (Option.to_list occurrence)
      in
      let last = offset + String.length haystack in
      let range =
        if offset > 0 || last < haystack_length then
          [
            faint
              (strf "(excerpt: bytes %d-%d of a %d-byte haystack)" offset
                 (last - 1) haystack_length);
          ]
        else []
      in
      element @ (needle_line :: shown_haystack) @ range
  | Failure.Raise { expected; actual; predicate; backtrace; message_diff } ->
      let raised =
        match (message_diff, expected, actual) with
        | Some { Failure.constructor; expected_message; actual_message }, _, _
          ->
            line (strf "raised %s with the wrong message:" constructor)
            :: equality ~seen ~show:(strf "%S") ~expected:expected_message
                 ~actual:actual_message
        | None, Some expected, actual ->
            raise_sides ~seen ~expected:(shown_text expected)
              ~actual:(Option.map shown_text actual)
        | None, None, Some actual ->
            (* A rejected [raises_match] and an escaped exception demand
               different reactions. *)
            line
              (if predicate then
                 "raised exception does not satisfy the predicate:"
               else "uncaught exception:")
            :: block `Red (shown_text actual)
        | None, None, None ->
            [ line "expected an exception, but none was raised" ]
      in
      let frames =
        match backtrace with
        | None -> []
        | Some bt ->
            capped max_lines
              (List.map faint (Text.split_lines (shown_text bt)))
              ~more:(more "frames")
      in
      raised @ frames
  | Failure.Baseline { baseline; state; withheld = _ } -> (
      let subject = baseline_subject baseline in
      match state with
      | Failure.Missing { proposed } ->
          let proposed = Text.split_lines (shown_text proposed) in
          line (subject ^ ": no baseline")
          :: line
               (strf "proposed (%s):" (counted (List.length proposed) "line"))
          :: capped max_proposed_lines
               (List.map
                  (fun l -> [ plain "  "; styled `Red ("+ " ^ l) ])
                  proposed)
               ~more:(fun n -> plain "  " :: more "lines" n)
      | Failure.Mismatch { expected; actual } ->
          line (subject ^ ": mismatch")
          :: text_diff ~headers:false ~show:Fun.id ~expected ~actual
      | Failure.Unresolvable { candidate } ->
          List.map line
            [
              subject
              ^ ": the path cannot be proven to lie under the project root";
              "unverified path: " ^ Os.display_path candidate;
              "(set WINDTRAP_PROJECT_ROOT to the directory the path is \
               relative to)";
            ])
  | Failure.Property
      {
        rendered;
        summary;
        case_index;
        shrink_steps;
        shrink_end;
        examples;
        rendering;
        inner;
        root = _;
        count = _;
      } ->
      let rendered = shown_text rendered in
      (* A pre-image is marked on the head itself, so that a reader who stops
         there does not take it for the value the body received. *)
      let head =
        strf "counterexample (%s):%s"
          (case_name ~examples ~case_index ~shrink_steps)
          (match rendering with
          | Failure.Value -> ""
          | Failure.Pre_image -> " computed from")
      in
      (* A summary takes the value's place on the head, and the rendering
         under it is a table with a header row. *)
      let counterexample =
        match (Option.map shown_text summary, Text.split_lines rendered) with
        | Some summary, header :: rows ->
            line (head ^ " " ^ shown summary)
            :: [ plain "  "; styled `Faint header ]
            :: List.map (fun row -> line ("  " ^ row)) rows
        | Some _, [] | None, ([] | [ _ ]) ->
            [ line (head ^ " " ^ shown rendered) ]
        | None, _ :: _ :: _ -> line head :: lines ~lead:"  " rendered
      in
      let pre_image =
        match rendering with
        | Failure.Value -> []
        | Failure.Pre_image ->
            [
              [
                plain "  ";
                styled `Faint
                  "(the value has no printer, so this is the input that map \
                   and bind";
              ];
              [
                plain "  ";
                styled `Faint
                  " computed it from; attach a printer with Gen.with_pp to see \
                   the value)";
              ];
            ]
      in
      (* [%gs] is the runner's [timed out after %gs], so one search finds
         both. *)
      let stop =
        match shrink_end with
        | Failure.Converged -> []
        | Failure.Timed_out limit ->
            [
              line
                (strf
                   "timed out after %gs while shrinking; counterexample may \
                    not be minimal"
                   limit);
            ]
        | Failure.Budget_spent ->
            [
              line
                (strf
                   "shrinking stopped after %d steps; counterexample may not \
                    be minimal"
                   shrink_steps);
            ]
        | Failure.Candidate_raised text ->
            [
              line
                (strf "shrinking stopped after %d steps: a candidate raised %s"
                   shrink_steps (shown_text text));
              line "counterexample may not be minimal";
            ]
      in
      (* A failure raised in tail position has no location to be [at]. *)
      let inner =
        match inner with
        | None -> []
        | Some (i : Failure.t) ->
            line
              (if Option.is_some i.loc then "which failed at:"
               else "which failed with:")
            :: List.map (indented "  ") (entry ~seen ~excerpt ~inner:true i)
      in
      counterexample @ pre_image @ stop @ inner
  | Failure.Timeout { limit; case } -> [ line (timeout_fact ~limit case) ]
  | Failure.Message m -> (
      match shown_text m with
      | "" -> [ line "(empty failure message)" ]
      | m -> lines ~lead:"" m)

(* Colour alone shows a changed span only on a terminal a reader watches:
   elsewhere the escapes may be stripped (dune strips an action's output when
   its own is no terminal) or read raw. *)
let pp_failure ~ansi ?(terminal = false) ?(excerpt = false)
    ?hints:(hinted = true) ?filter ?(invocation = `Mirrors) ?armed ppf f =
  let entry = entry ~seen:(ansi && terminal) ~excerpt ~inner:false f in
  let hint_lines =
    if not hinted then []
    else
      List.map
        (fun h -> [ plain h ])
        (hints ?armed ~invocation [ f ]
        @ Option.to_list
            (accept ?armed ~invocation ~tests:(`Filter filter) [ f ])
        @ Option.to_list
            (replay ?armed ~invocation ~tests:(`Filter filter) [ f ]))
  in
  List.iter
    (fun line -> Pp.pf ppf "%s@\n" (render ~ansi (indented indent line)))
    (entry @ hint_lines)

(* The section vocabulary *)

type column = { gap : string; align : [ `Left | `Right ]; width : int option }
type excerpt = { source : string; marked_lines : int list }

type section =
  | Line of span list
  | Hint of string
  | Rows of { margin : string; columns : column list; rows : span list list }
  | Excerpt of excerpt
  | Rule of string option

let dashes n = String.concat "" (List.init (max 0 n) (fun _ -> "\u{2500}"))

let rule ~width = function
  | None -> dashes width
  | Some label ->
      let inner = Text.length_utf8 label + 2 in
      let left = max 2 ((width - inner) / 2) in
      dashes left ^ " " ^ label ^ " " ^ dashes (max 2 (width - inner - left))

(* The runs of [lines], ascending and without duplicates, as pairs of their
   first and last line: lines at most [within] apart share a run. *)
let runs ~within lines =
  let rec loop acc first last = function
    | [] -> List.rev ((first, last) :: acc)
    | line :: rest ->
        if line <= last + within then loop acc first line rest
        else loop ((first, last) :: acc) line line rest
  in
  match lines with [] -> [] | line :: rest -> loop [] line line rest

(* A cell is padded outside its style. *)
let table ~margin ~columns rows =
  let widths =
    List.mapi
      (fun i (c : column) ->
        List.fold_left
          (fun w cells ->
            match List.nth_opt cells i with
            | Some cell -> max w (width [ cell ])
            | None -> w)
          (Option.value c.width ~default:0)
          rows)
      columns
  in
  let rec cells columns row =
    match (columns, row) with
    | ((c : column), w) :: columns, cell :: row ->
        let pad = plain (String.make (w - width [ cell ]) ' ') in
        plain c.gap
        ::
        (match c.align with
        | `Right -> [ pad; cell ]
        | `Left -> [ cell; pad ])
        @ cells columns row
    | [], _ | _, [] -> []
  in
  List.map
    (fun row -> plain margin :: cells (List.combine columns widths) row)
    rows

(* One line of context on either side of a marked line, so marked lines at
   most three apart share a region. *)
let excerpt_rows { source; marked_lines } =
  let lines = Array.of_list (Text.split_lines source) in
  let total = Array.length lines in
  let marked =
    List.filter
      (fun l -> 1 <= l && l <= total)
      (List.sort_uniq Int.compare marked_lines)
  in
  let is_marked = Array.make (total + 1) false in
  List.iter (fun l -> is_marked.(l) <- true) marked;
  let regions =
    List.map
      (fun (first, last) -> (max 1 (first - 1), min total (last + 1)))
      (runs ~within:3 marked)
  in
  let digits =
    List.fold_left
      (fun w (_, last) -> max w (String.length (string_of_int last)))
      4 regions
  in
  let row n =
    [
      (if is_marked.(n) then styled `Red "  \u{258c}" else plain "   ");
      plain (strf "%*d \u{2502} " digits n);
      plain lines.(n - 1);
    ]
  in
  separated
    [ styled `Faint "   \u{00b7}\u{00b7}\u{00b7}\u{00b7}\u{00b7}" ]
    (List.map
       (fun (first, last) ->
         List.init (last - first + 1) (fun i -> row (first + i)))
       regions)

(* A table's and an excerpt's lines are stripped of trailing spaces once
   rendered, so a row whose last cell is empty sheds the padding before it. *)
let print ~out ~ansi sections =
  let put spans = Pp.pf out "%s@\n" (render ~ansi spans) in
  let put_trimmed spans =
    Pp.pf out "%s@\n" (trim_end (fun c -> c = ' ') (render ~ansi spans))
  in
  List.iter
    (function
      | Line spans -> put spans
      | Hint hint -> put [ plain hint ]
      | Rows { margin; columns; rows } ->
          List.iter put_trimmed (table ~margin ~columns rows)
      | Excerpt e -> List.iter put_trimmed (excerpt_rows e)
      | Rule label -> put [ styled `Faint (rule ~width:rule_width label) ])
    sections;
  Pp.flush out ()

let join parts =
  separated (Line [])
    (List.filter (function [] -> false | _ :: _ -> true) parts)

(* A row keeps the eight ranges a well-covered file needs whole, whatever its
   width, and bounds only a barely tested file. *)
let ranges lines =
  let ranges =
    List.map
      (fun (s, e) -> if s = e then string_of_int s else strf "%d-%d" s e)
      (runs ~within:1 lines)
  in
  let rest = List.length ranges - max_ranges in
  let kept = String.concat ", " (take max_ranges ranges) in
  if rest > 0 then strf "%s (+%d more)" kept rest else kept

let pad n s = s ^ String.make (max 0 (n - cols s)) ' '

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

let percent ~visited ~total =
  if total = 0 then 100. else 100. *. float_of_int visited /. float_of_int total

(* Red marks a number below the project's gate, or below 80 without one. *)
let percentage ?(width = 0) ~min ~visited ~total () =
  let pct = percent ~visited ~total in
  let text = strf "%*s" width (strf "%.1f%%" pct) in
  if pct < Option.value min ~default:80. then styled `Red text else plain text

(* The gate compares the unrounded percentage, and the line gives the counts
   beside its rounding, so no printed comparison contradicts its own digits.
   The minimum prints as given. *)
let coverage_line ~min ~visited ~total =
  [
    plain "coverage: ";
    percentage ~min ~visited ~total ();
    plain (strf " (%d/%d points)" visited total);
  ]
  @
  match min with
  | None -> []
  | Some min ->
      [
        plain (strf ", minimum %s%%: " (Pp.to_string Pp.decimal min));
        (if percent ~visited ~total >= min then styled `Green "ok"
         else styled `Red "FAILED");
      ]

let coverage_report ~mode ~min (c : coverage) =
  let widest f =
    List.fold_left (fun w (file : coverage_file) -> max w (f file)) 0 c.files
  in
  let digits n = String.length (string_of_int n) in
  let visited_width = widest (fun f -> digits f.visited) in
  let total_width = widest (fun f -> digits f.total) in
  let points_width =
    max (String.length "points") (visited_width + 1 + total_width)
  in
  let file_width = max (String.length "file") (widest (fun f -> cols f.file)) in
  (* A row after its margin and its percentage. *)
  let tail ~points ~file ~note =
    trim_end
      (fun c -> c = ' ')
      (strf "    %s   %s   %s" (pad points_width points) (pad file_width file)
         note)
  in
  let header =
    Line
      [
        styled `Faint
          ("   cover"
          ^ tail ~points:"points" ~file:"file"
              ~note:
                (match mode with
                | `Report -> "uncovered lines (-u shows the source)"
                | `Full -> "uncovered lines"));
      ]
  in
  let row (f : coverage_file) =
    let note =
      if f.stale then "stale: the source changed; re-run the instrumented tests"
      else if f.uncovered <> [] then ranges f.uncovered
        (* Unvisited points without a line: the source was not found. *)
      else if f.visited < f.total then "(source not found)"
      else ""
    in
    Line
      [
        plain "  ";
        percentage ~width:6 ~min ~visited:f.visited ~total:f.total ();
        plain
          (tail
             ~points:
               (strf "%*d/%-*d" visited_width f.visited total_width f.total)
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
              percentage ~min ~visited:f.visited ~total:f.total ();
              plain (strf " (%d/%d)" f.visited f.total);
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

type mutant = {
  id : string;
  line : int;
  before : string;
  after : string;
  source : string option;
}

type survivor = { mutant : mutant; witnesses : witness list }
type not_evaluated = { mutant : mutant; invocation : Run.invocation }
type scope = Suite | Selected of int | Executables of int

type mutation = {
  survivors : survivor list;
  not_evaluated : not_evaluated list;
  unreached : (string * int) list;
  outside_tests : (string * int) list;
  killed : int;
  not_tested : int;
  scope : scope;
}

(* A build action's suite is reached through dune alone: [--force] because
   dune replays a cached test action, which arms nothing, and the backend
   because a plain build carries no mutant. *)
let arm_command ~invocation ~selection id =
  match invocation with
  | `Exe cmd -> strf "%s%s%s" cmd (arm_flag (Some id)) selection
  | `Mirrors ->
      strf "%s%sdune runtest --force --instrument-with ppx_windtrap.mutate"
        (arm_mirror (Some id)) selection

let reproduce_line ~invocation ~selection id =
  "reproduce: " ^ arm_command ~invocation ~selection id

(* A survivor is drawn as a failure block is: it is a defect report about the
   tests it names. *)
let survivor_block ~exe_width (s : survivor) =
  let m = s.mutant in
  let title =
    Line
      [
        plain "  ";
        styled `Red "SURVIVED";
        plain "  ";
        styled `Bold m.id;
        plain (strf "  %s \u{2192} %s" m.before m.after);
      ]
  in
  let source =
    match Option.map (fun s -> Array.of_list (Text.split_lines s)) m.source with
    | Some lines when 1 <= m.line && m.line <= Array.length lines ->
        [ Line (plain indent :: source_spans m.line lines.(m.line - 1)) ]
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
            strf "%d tests in %d executables ran this line and none failed:" n
              executables
          else strf "%d tests ran this line and none failed:" n
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

(* The reached count is the sum of the other terms, so the line cannot
   disagree with the blocks above it. A zero term is omitted. *)
let mutation_summary (m : mutation) =
  let survived = List.length m.survivors in
  let reached =
    let n = m.killed + survived + List.length m.not_evaluated + m.not_tested in
    match m.scope with
    | Suite -> strf "%d reached by this suite" n
    | Selected tests ->
        strf "%d reached by the %s" n (counted tests "selected test")
    | Executables _ -> strf "%d reached" n
  in
  let term n make = if n > 0 then [ make n ] else [] in
  let terms =
    (if survived > 0 then
       [
         [ styled `Red (strf "%d survived" survived); plain (" of " ^ reached) ];
       ]
     else [ [ plain reached ] ])
    @ term m.killed (fun n -> [ styled `Green (strf "%d killed" n) ])
    @ term (List.length m.not_evaluated) (fun n ->
        [ styled `Yellow (strf "%d not evaluated" n) ])
    @ term (List.length m.unreached) (fun n ->
        [ styled `Yellow (strf "%d never reached" n) ])
    @ term (List.length m.outside_tests) (fun n ->
        [ styled `Yellow (strf "%d evaluated outside tests" n) ])
    @ term m.not_tested (fun n -> [ plain (strf "%d not tested" n) ])
    @
    match m.scope with
    | Executables n -> [ [ plain (counted n "executable") ] ]
    | Suite | Selected _ -> []
  in
  plain "mutants: " :: separated (plain ", ") terms

(* Each mutant with the command that tests it in a new process, which starts
   from no state that a dry run left. *)
let not_evaluated_section ~selection = function
  | [] -> []
  | not_evaluated ->
      let mutant (n : not_evaluated) =
        [
          Line
            [
              plain "  ";
              styled `Bold n.mutant.id;
              plain (strf "  %s \u{2192} %s" n.mutant.before n.mutant.after);
            ];
          Hint
            ("    arm: "
            ^ arm_command ~invocation:n.invocation ~selection n.mutant.id);
        ]
      in
      Rule (Some (strf "not evaluated (%d)" (List.length not_evaluated)))
      :: Line
           [
             plain
               "  Each site ran in the dry run and not in its mutant's child.";
           ]
      :: List.concat_map mutant not_evaluated
      @ [ Rule None ]

(* One row per file: what a reader does with an unreached mutant is write a
   test for its lines, and a project has hundreds of them. *)
let by_file ~title ~lead sites =
  let files = List.sort_uniq String.compare (List.map fst sites) in
  let lines_of file =
    List.filter_map
      (fun (f, line) -> if String.equal f file then Some line else None)
      sites
  in
  let count_width =
    List.fold_left
      (fun w file ->
        max w (String.length (string_of_int (List.length (lines_of file)))))
      0 files
  in
  let file_width = List.fold_left (fun w file -> max w (cols file)) 0 files in
  let row file =
    let lines = lines_of file in
    let distinct = List.sort_uniq Int.compare lines in
    Line
      [
        plain "  ";
        styled `Yellow (strf "%*d" count_width (List.length lines));
        plain
          (strf "  %s   %s %s" (pad file_width file)
             (if List.compare_length_with distinct 1 = 0 then "line"
              else "lines")
             (ranges distinct));
      ]
  in
  match files with
  | [] -> []
  | _ :: _ ->
      (Rule (Some (strf "%s (%d)" title (List.length sites))) :: lead)
      @ List.map row files @ [ Rule None ]

let unreached_section = by_file ~title:"never reached" ~lead:[]

(* A reader takes "never reached" for "no test covers the line", which is
   false of a site that module initialization evaluated. *)
let outside_tests_section =
  by_file ~title:"evaluated outside tests"
    ~lead:
      [
        Line
          [
            plain
              "  These sites ran outside every test, at module initialization \
               or in a fixture release.";
          ];
      ]

(* The command arms the first survivor printed. *)
let outcome ~invocation ~selection (m : mutation) =
  (match m.survivors with
    | [] -> []
    | first :: _ ->
        [ Hint (reproduce_line ~invocation ~selection first.mutant.id) ])
  @ [ Line (mutation_summary m) ]

(* The caller hands over the survivors whose blocks it printed. *)
let mutation_closing ~(config : Run.config) (m : mutation) =
  let invocation = config.invocation in
  let selection = selection_words invocation config in
  let sections =
    [
      not_evaluated_section ~selection m.not_evaluated;
      unreached_section m.unreached;
      outside_tests_section m.outside_tests;
    ]
  in
  let rest = join (sections @ [ outcome ~invocation ~selection m ]) in
  match m.survivors with
  | [] when List.for_all (function [] -> true | _ :: _ -> false) sections ->
      rest
  | [] -> Line [] :: rest
  | _ :: _ -> Rule None :: Line [] :: rest

let mutation_report ~invocation (m : mutation) =
  let exes =
    List.concat_map
      (fun (s : survivor) ->
        List.filter_map (fun (w : witness) -> w.exe) s.witnesses)
      m.survivors
  in
  let exe_width =
    match exes with
    | [] -> None
    | _ :: _ -> Some (List.fold_left (fun w exe -> max w (cols exe)) 0 exes)
  in
  let survivors =
    match m.survivors with
    | [] -> []
    | survivors ->
        Rule (Some (strf "survivors (%d)" (List.length survivors)))
        :: join (List.map (survivor_block ~exe_width) survivors)
        @ [ Rule None ]
  in
  join
    [
      survivors;
      not_evaluated_section ~selection:"" m.not_evaluated;
      unreached_section m.unreached;
      outside_tests_section m.outside_tests;
      outcome ~invocation ~selection:"" m;
    ]
