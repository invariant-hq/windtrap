(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A rendering cannot be made again once the failure site is left, so it is
   captured there and bounded once, here. The cut is data, as a tail's is:
   [kept] holds no marker, and a renderer spells the cut from [length]. *)
let value_limit = 65_536

type text = { kept : string; length : int }

let text s =
  { kept = Text.prefix_bytes_utf8 value_limit s; length = String.length s }

let is_cut t = String.length t.kept < t.length

type phase = Body | Setup | Teardown | Release
type tail = { text : string; omitted_bytes : int; log_path : string option }
type baseline = Literal of { exact : bool } | File of string

type baseline_state =
  | Missing of { proposed : text }
  | Mismatch of { expected : text; actual : text }
  | Unresolvable of { candidate : string }

type withheld =
  | Failed_outside
  | Skipped
  | Refused of { line : int; reason : string }
  | Conflict

type message_diff = {
  constructor : string;
  expected_message : text;
  actual_message : text;
}

type containment_demand =
  | Anywhere
  | Prefix
  | Suffix
  | Ordered of { index : int; resumed_at : int }

type kind =
  | Equality of { expected : text; actual : text; not_ : bool; diffable : bool }
  | Containment of {
      needle : text;
      found_at : int option;
      haystack_length : int;
      excerpt : string;
      excerpt_offset : int;
      demand : containment_demand;
    }
  | Raise of {
      expected : text option;
      actual : text option;
      predicate : bool;
      backtrace : text option;
      message_diff : message_diff option;
    }
  | Baseline of {
      baseline : baseline;
      state : baseline_state;
      withheld : withheld option;
    }
  | Property of {
      rendered : text;
      summary : text option;
      case_index : int;
      shrink_steps : int;
      shrink_end : shrink_end;
      root : Seed.seed;
      count : int option;
      examples : bool;
      rendering : rendering;
      inner : t option;
    }
  | Timeout of { limit : float; case : timed_case option }
  | Message of text

and timed_case = {
  case_index : int;
  examples : bool;
  passed : int;
  root : Seed.seed;
  count : int option;
}

and shrink_end =
  | Converged
  | Budget_spent
  | Candidate_raised of text
  | Timed_out of float

and rendering = Value | Pre_image

and t = {
  kind : kind;
  phase : phase;
  loc : Loc.t option;
  msg : text option;
  subtest : string list;
  output_tail : tail option;
}

type control = [ `Skip of string option | `Timeout of float | `Exit | `Discard ]

exception Check_failure of t
exception Control of control

(* One printer for the four, so that every site that stringifies a control,
   a counterexample printer that timed out or a release that exited, prints
   the same words and never a [Windtrap__Failure] name. *)
let () =
  Printexc.register_printer (function
    | Control (`Skip None) -> Some "windtrap skip"
    | Control (`Skip (Some reason)) -> Some ("windtrap skip: " ^ reason)
    | Control (`Timeout limit) ->
        Some (Printf.sprintf "windtrap timeout after %gs" limit)
    | Control `Exit ->
        Some
          "Exit_attempt (code under test called exit; intercepted by windtrap)"
    | Control `Discard ->
        Some "windtrap discard (assume or reject outside a property)"
    | _ -> None)

(* Catching the user's code *)

type fault = [ `Assertion of t | `Exception of exn * Printexc.raw_backtrace ]
type caught = [ fault | control ]

(* An interrupt and an exhausted memory must stop the run, not fail one
   test, so no site ever sees them. A [Stack_overflow] is not among them:
   OCaml 5 recovers from it, and it is the failure of the recursion that
   raised it. *)
let is_fatal = function Sys.Break | Out_of_memory -> true | _ -> false

(* A [Fun.protect] whose finally was cut by the timeout, or by an interrupt,
   wraps what cut it: unwrapped, the timeout reaches the runner as itself and
   the interrupt stops the run. Any other exception of a finally stays
   wrapped, since it is the user's own. *)
let rec classify exn backtrace : caught =
  match exn with
  | Check_failure t -> `Assertion t
  | Control c -> (c :> caught)
  | Fun.Finally_raised (Control _ as inner) -> classify inner backtrace
  | Fun.Finally_raised inner when is_fatal inner -> classify inner backtrace
  | exn when is_fatal exn -> Printexc.raise_with_backtrace exn backtrace
  | exn -> `Exception (exn, backtrace)

let catch f =
  match f () with
  | value -> Ok value
  | exception exn ->
      (* Taken first: the runtime keeps one backtrace, and any raise inside
         [classify] would replace it. *)
      let backtrace = Printexc.get_raw_backtrace () in
      Error (classify exn backtrace)

let to_exn : [< caught ] -> exn = function
  | `Assertion t -> Check_failure t
  | `Exception (exn, _) -> exn
  | (`Skip _ | `Timeout _ | `Exit | `Discard) as c -> Control c

let reraise c =
  match (c :> caught) with
  | `Exception (exn, backtrace) -> Printexc.raise_with_backtrace exn backtrace
  | c -> raise (to_exn c)

let caught_to_string c = Printexc.to_string (to_exn c)

(* Backtraces *)

(* Below the deepest frame of the reader's own code sit windtrap's: the
   delimiter the runner wraps callbacks in, the attempt guard, the verb that
   raised. They are the same handful of lines under every failure, they name
   none of the reader's code, and on a short backtrace they outnumber it.
   Drop that trailing run.

   Only a trailing run. A user callback invoked by windtrap — a [bracket]
   teardown, a property body, a [such_that] predicate — sits below windtrap
   frames and above more of them, and both it and the machinery it names
   have to survive. A backtrace that is windtrap's all the way up is kept
   whole: it means the raise never crossed user code, and trimming would
   leave the reader nothing at all. *)
let backtrace_to_string raw =
  let whole () = Printexc.raw_backtrace_to_string raw in
  match Printexc.backtrace_slots raw with
  | None -> whole ()
  | Some slots ->
      (* A slot without a debug name cannot be proven to be ours, so it
         ends the run — the same "no guess" rule [Loc.capture] follows. *)
      let rec deepest_foreign i =
        if i < 0 then -1
        else
          match Printexc.Slot.name slots.(i) with
          | Some name when Loc.own_unit name -> deepest_foreign (i - 1)
          | Some _ | None -> i
      in
      let keep = deepest_foreign (Array.length slots - 1) in
      if keep < 0 || keep = Array.length slots - 1 then whole ()
      else begin
        let buffer = Buffer.create 256 in
        (* Formatted at its original index: [Slot.format] words position 0
           as "Raised at" and the rest as "Called from", and dropping a
           suffix leaves every kept frame's position unchanged. *)
        for i = 0 to keep do
          match Printexc.Slot.format i slots.(i) with
          | Some line ->
              Buffer.add_string buffer line;
              Buffer.add_char buffer '\n'
          | None -> ()
        done;
        Buffer.contents buffer
      end

(* Bounds. They are contract: [failure.mli] states each one, so a change here
   is a change to what users were told. [value_limit] is above, with
   [text]. *)

(* Captured-output tails retain at most this many final bytes; the cut is
   recorded in [omitted_bytes], not as a marker inside the text. *)
let tail_bytes = 8_192

(* Haystack excerpts in containment failures, when there is an occurrence
   or a cursor to centre on: enough context around it to read, small enough
   to store on every failure. Reuses the tail bound. *)
let excerpt_limit = tail_bytes

(* With nothing to centre on — a needle that occurs nowhere — the excerpt is
   context rather than evidence, so the head, or the end for a suffix, is
   bounded to what a reader scans past to reach the verdict: whichever of
   these is shorter. *)
let head_excerpt_bytes = 1_024
let head_excerpt_lines = 10

(* First byte index at or after [pos] that does not continue a UTF-8
   sequence. Continuation bytes are 0b10xxxxxx; a well-formed sequence has at
   most three of them, so the scan is bounded even on malformed input. *)
let utf8_boundary_at_or_after s pos =
  let len = String.length s in
  let is_continuation i = i < len && Char.code s.[i] land 0xC0 = 0x80 in
  let rec scan i steps =
    if steps = 0 || not (is_continuation i) then i else scan (i + 1) (steps - 1)
  in
  scan pos 3

(* Constructors *)

let make ?loc ?msg kind =
  {
    kind;
    phase = Body;
    loc;
    msg = Option.map text msg;
    subtest = [];
    output_tail = None;
  }

let equality ?loc ?msg ?(not_ = false) ~expected ~actual () =
  make ?loc ?msg
    (Equality
       { expected = text expected; actual = text actual; not_; diffable = true })

(* The head window, for a haystack with no anchor: at most
   [head_excerpt_lines] lines and [head_excerpt_bytes] bytes. Line-structured
   content cuts after its last complete line, so the block form never ends on
   a fragment; a long single line cuts at a code-point boundary at or before
   the byte bound, so a UTF-8 sequence is never split. *)
let head_window haystack =
  let len = String.length haystack in
  let after_line_stop =
    (* Byte index just after the [head_excerpt_lines]-th newline, when the
       haystack has that many. *)
    let rec go i remaining =
      if remaining = 0 then Some i
      else
        match String.index_from_opt haystack i '\n' with
        | Some j -> go (j + 1) (remaining - 1)
        | None -> None
    in
    go 0 head_excerpt_lines
  in
  let byte_stop =
    if len <= head_excerpt_bytes then len
    else
      (* Back off to a code-point boundary; [utf8_boundary_at_or_after]
         moves the other way, so the scan is written out here. *)
      let rec boundary i steps =
        if steps = 0 || i = 0 || Char.code haystack.[i] land 0xC0 <> 0x80 then i
        else boundary (i - 1) (steps - 1)
      in
      boundary head_excerpt_bytes 3
  in
  match after_line_stop with
  | Some line_stop -> min line_stop byte_stop
  | None -> byte_stop

(* The end window, for an absent suffix: the head window read from the
   other end. It starts after the newline that precedes the last
   [head_excerpt_lines] lines, where a final newline ends the last line and
   opens none, or at the byte bound moved forward to a code-point boundary,
   whichever is later. *)
let end_window haystack =
  let len = String.length haystack in
  let line_start =
    (* The newline before the last [remaining] lines, searched below [i]. *)
    let rec go i remaining =
      match String.rindex_from_opt haystack (i - 1) '\n' with
      | None -> None
      | Some j -> if remaining = 1 then Some (j + 1) else go j (remaining - 1)
    in
    go
      (if String.ends_with ~suffix:"\n" haystack then len - 1 else len)
      head_excerpt_lines
  in
  let byte_start =
    utf8_boundary_at_or_after haystack (max 0 (len - head_excerpt_bytes))
  in
  match line_start with
  | Some line_start -> max line_start byte_start
  | None -> byte_start

(* The bounded haystack window stored as a containment failure's [excerpt].
   One bound, applied here: renderers show what is stored whole. Around
   [anchor] when there is one — the surroundings are the evidence — and the
   bounded head, or end under [at_end], otherwise. Both cuts land on UTF-8
   code-point boundaries, so an anchored window may exceed its limit by the
   up to three bytes needed to complete a sequence. *)
let excerpt_window ~anchor ~at_end haystack =
  let len = String.length haystack in
  match anchor with
  | None when at_end ->
      let start = end_window haystack in
      if start = 0 then (0, haystack)
      else (start, String.sub haystack start (len - start))
  | None ->
      let stop = head_window haystack in
      if stop >= len then (0, haystack) else (0, String.sub haystack 0 stop)
  | Some _ when len <= excerpt_limit -> (0, haystack)
  | Some i ->
      let start =
        utf8_boundary_at_or_after haystack (max 0 (i - (excerpt_limit / 2)))
      in
      let stop =
        let raw = start + excerpt_limit in
        if raw >= len then len else utf8_boundary_at_or_after haystack raw
      in
      (start, String.sub haystack start (stop - start))

(* Which offset the excerpt centres on. An [Ordered] failure is about a
   search that began at the cursor, so the cursor wins over an occurrence
   that — being before it — is precisely the one that did not count. *)
let excerpt_anchor ~found_at ~demand =
  match demand with
  | Ordered { resumed_at; _ } -> Some resumed_at
  | Anywhere | Prefix | Suffix -> found_at

let containment ?loc ?msg ?found_at ~demand ~needle ~haystack () =
  let outside i = i < 0 || i > String.length haystack in
  (match found_at with
  | Some i when outside i ->
      invalid_arg "Failure.containment: found_at is outside the haystack"
  | Some _ | None -> ());
  (match demand with
  | Ordered { resumed_at; _ } when outside resumed_at ->
      invalid_arg "Failure.containment: resumed_at is outside the haystack"
  | Anywhere | Prefix | Suffix | Ordered _ -> ());
  let excerpt_offset, excerpt =
    excerpt_window
      ~anchor:(excerpt_anchor ~found_at ~demand)
      ~at_end:
        (match demand with
        | Suffix -> true
        | Anywhere | Prefix | Ordered _ -> false)
      haystack
  in
  make ?loc ?msg
    (Containment
       {
         needle = text needle;
         found_at;
         haystack_length = String.length haystack;
         excerpt;
         excerpt_offset;
         demand;
       })

(* A claim is a description, not a rendering of the expected value, so the
   payload is an equality whose sides must never be diffed against each
   other. Same shape, same wording in every renderer; only the refinement
   differs, which is what [diffable] says. *)
let predicate ?loc ?msg ~claim value =
  make ?loc ?msg
    (Equality
       {
         expected = text claim;
         actual = text value;
         not_ = false;
         diffable = false;
       })

(* [message_diff] is stored as given. The failure site decides it with both
   exceptions in hand, so that a renderer branches on the option alone. *)
let raised ?loc ?msg ?expected ?actual ?(predicate = false) ?backtrace
    ?message_diff () =
  make ?loc ?msg
    (Raise
       {
         expected = Option.map text expected;
         actual = Option.map text actual;
         predicate;
         backtrace =
           (match backtrace with
           | Some "" -> None
           | _ -> Option.map text backtrace);
         message_diff;
       })

let baseline ?loc baseline state =
  (* The path is an identity: renderers name the file from it, so it is
     stored unmodified. *)
  make ?loc (Baseline { baseline; state; withheld = None })

let property ?loc ?inner ?count ?summary ~rendered ~case_index ~shrink_steps
    ?(shrink_end = Converged) ~root ~examples ?(rendering = Value) () =
  make ?loc
    (Property
       {
         rendered = text rendered;
         summary = Option.map text summary;
         case_index;
         shrink_steps;
         shrink_end;
         root;
         count;
         examples;
         rendering;
         inner;
       })

let timeout ?loc ?case limit = make ?loc (Timeout { limit; case })
let message ?loc s = make ?loc (Message (text s))

(* Updating *)

let with_phase phase t = { t with phase }
let with_output_tail tail t = { t with output_tail = Some tail }

let with_withheld withheld t =
  match t.kind with
  | Baseline { withheld = Some (Refused _ | Conflict); _ } -> t
  | Baseline b -> { t with kind = Baseline { b with withheld = Some withheld } }
  | Equality _ | Containment _ | Raise _ | Property _ | Timeout _ | Message _ ->
      t

let tail ?log_path ?(omitted_bytes = 0) text =
  if omitted_bytes < 0 then
    invalid_arg "Failure.tail: omitted_bytes is negative";
  let len = String.length text in
  if len <= tail_bytes then { text; omitted_bytes; log_path }
  else
    let cut = utf8_boundary_at_or_after text (len - tail_bytes) in
    {
      text = String.sub text cut (len - cut);
      omitted_bytes = omitted_bytes + cut;
      log_path;
    }

(* Per-test outcomes *)

type outcome = Pass | Fail of t list | Skip of string option
