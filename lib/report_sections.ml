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
let rule_width = 54 (* of an instrumentation report's rules *)
let max_diff_lines = 200
let max_proposed_lines = 20
let max_lines = 10 (* of a backtrace and of a captured tail *)
let max_value_bytes = 800 (* ten full lines *)
let max_headline_chars = 80
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

(* Why a block offers no [accept:]: the run kept none of the attempt's
   corrections (Run, Corrections), so the command would promote or rewrite
   nothing. *)
let withheld_fact (f : Failure.t) =
  match f.kind with
  | Failure.Baseline
      { state = Failure.Missing _ | Failure.Mismatch _; withheld = Some why; _ }
    ->
      Some
        ("no correction was kept: "
        ^
        match why with
        | Failure.Failed_outside ->
            "the test also failed outside its expectations; fix that failure \
             and rerun"
        | Failure.Skipped ->
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
        ( Option.to_list (List.find_map withheld_fact failures),
          List.filter_map (accept_line invocation ~filter) failures )
  in
  withheld
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
   [sanitize_name] guards the line against control bytes as on every other
   name surface. *)
let baseline_subject = function
  | Failure.Literal { exact = false } -> "expect"
  | Failure.Literal { exact = true } -> "expect_exact"
  | Failure.File path ->
      spf "expect_file \"%s\"" (sanitize_name (Os.display_path path))

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
   padding collapsed, escape codes stripped. A diff has no side short enough
   to quote and says how long it is. *)
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
          shrink_exhausted;
          timed_out;
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
           else if Option.is_some timed_out then ", shrinking timed out"
           else if shrink_exhausted then ", shrink limit reached"
           else "")
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
      (Text.strip_ansi
         (match labeled_msg f with
         | None -> fact
         | Some msg -> msg ^ ": " ^ fact))
  in
  let rec cut i chars =
    if i >= String.length line then line
    else if chars = max_headline_chars then String.sub line 0 i ^ "\u{2026}"
    else
      let decode = String.get_utf_8_uchar line i in
      cut (i + Uchar.utf_decode_length decode) (chars + 1)
  in
  cut 0 0

(* [s] with the byte ranges [spans], ascending and disjoint, in [style]. *)
let highlight ~ansi style s spans =
  if (not ansi) || spans = [] then s
  else begin
    let buf = Buffer.create (String.length s + 16) in
    let pos =
      List.fold_left
        (fun pos { Diff.start; length } ->
          Buffer.add_string buf (String.sub s pos (start - pos));
          Buffer.add_string buf
            (Pp.styled_string ~ansi style (String.sub s start length));
          start + length)
        0 spans
    in
    Buffer.add_string buf (String.sub s pos (String.length s - pos));
    Buffer.contents buf
  end

(* Whether colour shows [spans] of [s]: a span of spaces, or an empty one,
   has no glyph to colour. *)
let colour_shows s spans =
  not
    (List.exists
       (fun { Diff.start; length } ->
         String.for_all (fun c -> c = ' ') (String.sub s start length))
       spans)

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

(* Whether a [~] line under [s] lands under the code points it marks: a
   code point past Latin Extended-B has no fixed width, and neither has a
   tab unless the [~] line repeats it ([tabs]). *)
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
    let lead = show_controls (String.sub deleted 0 shared) in
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
let pp_hunks ~ansi put ~ind ~limit hunks =
  let st style s = Pp.styled_string ~ansi style s in
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
        emit (ind ^ "  " ^ show_controls s);
        lines ~after_delete:false rest
    | Diff.Insert s :: rest ->
        emit (ind ^ st `Red ("+ " ^ show_controls s));
        lines ~after_delete:false rest
    | Diff.Delete s :: rest ->
        (* Green is the expected side and red the actual one, here as
           everywhere else, not the diff tool's red-for-removed: the sigils
           say which side is which, the colour carries the report's own
           meaning. *)
        emit (ind ^ st `Green ("- " ^ show_controls s));
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
            put (ind ^ "  " ^ pad ^ st `Red tildes)
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
        (ind
        ^ st `Faint
            (spf "@@ -%s +%s @@"
               (range h.expected_start h.expected_count)
               (range h.actual_start h.actual_count)));
      lines ~after_delete:false h.lines)
    hunks;
  if total > limit then
    put (ind ^ st `Faint (spf "\u{2026} (+%d more diff lines)" (total - limit)))

(* A single-line value as it prints: its middle elided past
   [max_value_bytes], cut in the carried bytes so the count is theirs, and
   the control bytes of what is left escaped. *)
let elided s = String.length s > max_value_bytes
let shown s = Text.elide_middle max_value_bytes ~show:show_controls s

(* What changed in [s], a value printed [lead] columns into its line. With
   colour the spans are [style]d inside the plain value and no [~] line
   prints, unless colour cannot show one of them; without it a [~] line
   marks them, when [aligned]. *)
let pp_marked ~ansi put ~lead ~aligned ~style ~before s spans =
  let tildes = (not ansi) || not (colour_shows s spans) in
  put (before ^ highlight ~ansi style s spans);
  if tildes && aligned then
    Option.iter
      (fun m ->
        let at = String.index m '~' in
        put
          (String.make (lead + at) ' '
          ^ Pp.styled_string ~ansi style
              (String.sub m at (String.length m - at))))
      (marker_line s spans)

(* Two single-line renderings under their anchors. Refinement ran against
   the raw values and the spans are drawn against the escaped ones. A pair
   that does not refine prints each side whole in its colour: so does one
   whose anchors already state the difference ([marked] is off), and one
   with an elided side, which has no columns left to mark. *)
let pp_sides ~ansi put ~ind ~anchors:(expected_anchor, actual_anchor) ~marked
    ~expected ~actual =
  let st style s = Pp.styled_string ~ansi style s in
  let width =
    2 + max (String.length expected_anchor) (String.length actual_anchor)
  in
  let marked = marked && not (elided expected || elided actual) in
  let expected_spans, actual_spans =
    match if marked then Diff.refine ~expected ~actual else None with
    | None -> ([], [])
    | Some { Diff.expected_spans; actual_spans } ->
        ( List.map (moved_span expected) expected_spans,
          List.map (moved_span actual) actual_spans )
  in
  let refined = expected_spans <> [] || actual_spans <> [] in
  let expected = shown expected and actual = shown actual in
  let aligned = aligns ~tabs:false expected && aligns ~tabs:false actual in
  let side anchor ~whole ~span value spans =
    let before =
      ind ^ st `Faint anchor ^ String.make (width - String.length anchor) ' '
    in
    if refined then
      pp_marked ~ansi put
        ~lead:(String.length ind + width)
        ~aligned ~style:span ~before value spans
    else put (before ^ st whole value)
  in
  side expected_anchor ~whole:`Green ~span:`Bold_green expected expected_spans;
  side actual_anchor ~whole:`Red ~span:`Bold_red actual actual_spans

let pp_eq ~ansi put ~ind ~expected ~actual =
  let st style s = Pp.styled_string ~ansi style s in
  if String.equal expected actual then begin
    (* The equality told the values apart and their printer did not
       ([equal float nan nan], a lossy pp). Decided on the raw renderings:
       escaping merges values it cannot tell apart, and a pair the printer
       did distinguish must never be reported as one it did not. *)
    if String.contains expected '\n' then begin
      put (ind ^ st `Faint "both sides render as:");
      List.iter
        (fun l -> put (ind ^ "  " ^ l))
        (Text.split_lines (show_controls expected))
    end
    else put (ind ^ st `Faint "both sides render as:" ^ " " ^ shown expected);
    put (ind ^ st `Faint "the printer shows less than the equality compares")
  end
  else if String.contains expected '\n' || String.contains actual '\n' then
    begin match Diff.hunks ~expected ~actual () with
    | [] -> put (ind ^ newline_fact ~expected ~actual)
    | hunks ->
        put (ind ^ st `Faint "--- expected");
        put (ind ^ st `Faint "+++ actual");
        pp_hunks ~ansi put ~ind ~limit:max_diff_lines hunks
    end
  else
    pp_sides ~ansi put ~ind ~anchors:("expected", "actual") ~marked:true
      ~expected ~actual

(* A rendering under the sentence or the anchor that names it: a block,
   each line in [style], so that no style spans a line. *)
let pp_value_block ~ansi put ~ind style value =
  let st style s = Pp.styled_string ~ansi style s in
  if String.contains value '\n' then
    List.iter
      (fun l -> put (ind ^ "  " ^ st style l))
      (Text.split_lines (show_controls value))
  else put (ind ^ "  " ^ st style (shown value))

(* The sides of a [raises] that named its exception; [actual] is [None]
   when nothing was raised. The anchors state the difference, so nothing is
   marked; a rendering that spans lines is a block under its anchor. *)
let pp_raise ~ansi put ~ind ~expected ~actual =
  let st style s = Pp.styled_string ~ansi style s in
  let spans_lines s = String.contains s '\n' in
  match actual with
  | Some actual when not (spans_lines expected || spans_lines actual) ->
      pp_sides ~ansi put ~ind
        ~anchors:("expected exception", "raised")
        ~marked:false ~expected ~actual
  | Some _ | None ->
      let width = 2 + String.length "expected exception" in
      let side anchor style value =
        if spans_lines value then begin
          put (ind ^ st `Faint (anchor ^ ":"));
          pp_value_block ~ansi put ~ind style value
        end
        else
          put
            (ind ^ st `Faint anchor
            ^ String.make (width - String.length anchor) ' '
            ^ st style (shown value))
      in
      side "expected exception" `Green expected;
      begin match actual with
      | Some actual -> side "raised" `Red actual
      | None -> put (ind ^ "but no exception was raised")
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
  let st style s = Pp.styled_string ~ansi style s in
  (* Payload strings from a user pp may carry escape codes: under
     [ansi:false] every line is stripped at the sink. *)
  let put line =
    Pp.pf ppf "%s@\n" (if ansi then line else Text.strip_ansi line)
  in
  let put_ind line = put (ind ^ line) in
  let put_block s =
    List.iter (fun line -> put_ind ("  " ^ line)) (Text.split_lines s)
  in
  (match
     Option.to_list (Option.map (st `Yellow) (phase_tag f))
     @ Option.to_list
         (Option.map (fun loc -> st `Faint (Loc.to_string loc)) f.loc)
   with
  | [] -> ()
  | parts -> put_ind (String.concat " " parts));
  (* The located source line, best-effort and dedented, printed as a value
     is: a file's bytes are no more the report's than a test's are. The
     blank line after it closes a block's head; an inner entry has none. *)
  (match f.loc with
  | Some { Loc.file; line; _ } when excerpt ->
      Option.iter
        (fun text ->
          let gutter = st `Faint (spf "%d \u{2502}" line) in
          put_ind
            (match String.trim text with
            | "" -> "  " ^ gutter
            | text -> spf "  %s %s" gutter (shown text));
          if not inner then put "")
        (source_line file line)
  | Some _ | None -> ());
  (match f.subtest with
  | [] -> ()
  | [ leaf ] -> put_ind (st `Faint "subtest" ^ "   " ^ sanitize_name leaf)
  | _ :: names ->
      put_ind
        (st `Faint "subtest" ^ "   "
        ^ sanitize_name (Test_tree.path_to_string names)));
  Option.iter
    (fun msg ->
      List.iter
        (fun line -> put_ind (sanitize_name line))
        (Text.split_lines msg))
    f.msg;
  (match f.kind with
  | Failure.Equality { not_ = true; expected; _ } ->
      if String.contains expected '\n' then begin
        put_ind "both sides equal:";
        put_block (show_controls expected)
      end
      else put_ind (spf "both sides equal: %s" (shown expected))
  | Failure.Equality { expected = claim; actual = value; diffable = false; _ }
    ->
      (* A claim is a description, not a rendering: never diff or refine the
         two. Colour still applies — green and red mark which side is
         which, and that is as true of a description as of a value, and so is
         visibility: a [~claim] may be built around a rendered bound
         ([greater than <x>]). *)
      put_ind (st `Faint "expected" ^ "  " ^ st `Green (shown claim));
      if String.contains value '\n' then begin
        put_ind (st `Faint "actual:");
        pp_value_block ~ansi put ~ind `Red value
      end
      else put_ind (st `Faint "actual" ^ "    " ^ st `Red (shown value))
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
          put_ind (st `Faint "element" ^ "   " ^ string_of_int index)
      | Failure.Anywhere -> ());
      (* [%S] is [String.escaped] between quotes, OCaml's decimal escapes.
         An elided needle is cut in its carried bytes and each end escaped:
         a cut in the quoted text would split an escape and count its
         digits. *)
      put_ind
        (st `Faint "needle" ^ "    \""
        ^ Text.elide_middle max_value_bytes ~show:String.escaped needle
        ^ "\": "
        ^ containment_verdict ~demand ~found_at);
      (* The occurrence's byte range inside the excerpt, when it is there to
         mark: a failed [not_contains] window always contains it, and an
         out-of-order chain break carries one that a cursor-anchored window
         may have left behind, hence the bounds test. The span is the
         payload's, in raw bytes, moved into display coordinates where it
         is drawn. *)
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
      let haystack ~before ~lead line spans =
        let shown = show_controls line in
        pp_marked ~ansi put
          ~lead:(String.length ind + lead)
          ~aligned:(aligns ~tabs:false shown) ~style:`Bold_red
          ~before:(ind ^ before) shown
          (List.map (moved_span line) spans)
      in
      if not (String.contains excerpt '\n') then
        haystack
          ~before:(st `Faint "haystack" ^ "  ")
          ~lead:10 excerpt
          (Option.to_list occurrence)
      else begin
        put_ind (st `Faint "haystack:");
        (* [offset] is the line's first byte within the excerpt; an
           occurrence that spans lines is marked on its first. *)
        ignore
          (List.fold_left
             (fun offset line ->
               haystack ~before:"  " ~lead:2 line
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
          (st `Faint
             (spf "(excerpt: bytes %d-%d of a %d-byte haystack)" excerpt_offset
                (excerpt_offset + String.length excerpt - 1)
                haystack_length))
  | Failure.Raise { expected; actual; predicate; backtrace; message_diff } -> (
      (match (message_diff, expected, actual) with
      | Some { Failure.constructor; expected_message; actual_message }, _, _ ->
          (* Right constructor, wrong payload: the messages are compared,
             the constructor said once. *)
          put_ind (spf "raised %s with the wrong message:" constructor);
          pp_eq ~ansi put ~ind
            ~expected:(spf "%S" expected_message)
            ~actual:(spf "%S" actual_message)
      | None, Some expected, actual -> pp_raise ~ansi put ~ind ~expected ~actual
      | None, None, Some actual ->
          (* [predicate] tells a [raises_match] rejection from a test body's
             escape: the two demand different reactions. *)
          put_ind
            (if predicate then
               "raised exception does not satisfy the predicate:"
             else "uncaught exception:");
          pp_value_block ~ansi put ~ind `Red actual
      | None, None, None -> put_ind "expected an exception, but none was raised");
      match backtrace with
      | Some bt ->
          let frames = Text.split_lines bt in
          List.iter (fun l -> put_ind (st `Faint l)) (take max_lines frames);
          let more = List.length frames - max_lines in
          if more > 0 then
            put_ind (st `Faint (spf "\u{2026} (+%d more frames)" more))
      | None -> ())
  | Failure.Baseline { baseline; state; withheld = _ } -> (
      let subject = baseline_subject baseline in
      match state with
      | Failure.Missing { proposed } ->
          put_ind (subject ^ ": no baseline");
          (* The file does not exist: its proposed text is all [+] and has
             no hunk to head. *)
          let lines = Text.split_lines (show_controls proposed) in
          let n = List.length lines in
          put_ind (spf "proposed (%d line%s):" n (if n = 1 then "" else "s"));
          List.iter
            (fun l -> put_ind ("  " ^ st `Red ("+ " ^ l)))
            (take max_proposed_lines lines);
          if n > max_proposed_lines then
            put_ind
              ("  "
              ^ st `Faint
                  (spf "\u{2026} (+%d more lines)" (n - max_proposed_lines)))
      | Failure.Mismatch { expected; actual } -> (
          put_ind (subject ^ ": mismatch");
          match Diff.hunks ~expected ~actual () with
          | [] -> put_ind (newline_fact ~expected ~actual)
          | hunks -> pp_hunks ~ansi put ~ind ~limit:max_diff_lines hunks)
      | Failure.Unresolvable { candidate } ->
          put_ind
            (subject
           ^ ": the path cannot be proven to lie under the project root");
          put_ind
            (spf "unverified path: %s"
               (sanitize_name (Os.display_path candidate)));
          put_ind
            "(set WINDTRAP_PROJECT_ROOT to the directory the path is relative \
             to)")
  | Failure.Property
      {
        rendered;
        summary;
        case_index;
        shrink_steps;
        shrink_exhausted;
        timed_out;
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
      (match (summary, Text.split_lines (show_controls rendered)) with
      | Some summary, header :: rows ->
          put_ind (head ^ " " ^ shown summary);
          put_ind ("  " ^ st `Faint header);
          List.iter (fun row -> put_ind ("  " ^ row)) rows
      | Some _, [] | None, ([] | [ _ ]) -> put_ind (head ^ " " ^ shown rendered)
      | None, (_ :: _ :: _ as lines) ->
          put_ind head;
          List.iter (fun line -> put_ind ("  " ^ line)) lines);
      (match rendering with
      | Failure.Value -> ()
      | Failure.Pre_image ->
          put_ind
            ("  "
            ^ st `Faint
                "(the value has no printer, so this is the input that map and \
                 bind");
          put_ind
            ("  "
            ^ st `Faint
                " computed it from; attach a printer with Gen.with_pp to see \
                 the value)"));
      (* What is reported is the best the search got to. [%gs] is the
         runner's [timed out after %gs], so one grep finds both. *)
      (match timed_out with
      | Some limit ->
          put_ind
            (spf
               "timed out after %gs while shrinking; counterexample may not be \
                minimal"
               limit)
      | None ->
          if shrink_exhausted then
            put_ind
              (spf
                 "shrinking stopped after %d steps; counterexample may not be \
                  minimal"
                 shrink_steps));
      (* An inner failure raised in tail position has no site: [at:] over
         no location would misread. *)
      match inner_failure with
      | Some i ->
          put_ind
            (match i.Failure.loc with
            | Some _ -> "which failed at:"
            | None -> "which failed with:");
          pp_gen ~ansi ~excerpt ~inner:true ~hints:false ~filter ~invocation
            ~armed ~ind:(ind ^ "  ") ppf i
      | None -> ())
  | Failure.Message "" -> put_ind "(empty failure message)"
  | Failure.Message m ->
      List.iter (fun line -> put_ind line) (Text.split_lines m));
  if hinted then List.iter put_ind (hints ?armed ~invocation ~filter [ f ])

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
   variable) arrives pre-spelled with the runtime's own functions. A second copy of
   any of these drawers is exactly the drift the coverage command's
   structure exists to prevent. *)

(* The sink: where sections print and whether they style. With
   [ansi:false] every line is stripped at the sink, so escape codes
   arriving inside payload strings never reach a plain transcript. *)
type sink = { out : Format.formatter; ansi : bool }

let put k line =
  Pp.pf k.out "%s@\n" (if k.ansi then line else Text.strip_ansi line)

let st k style s = Pp.styled_string ~ansi:k.ansi style s

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
  | Rule label -> put k (st k `Faint (rule ~width:rule_width label))

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
