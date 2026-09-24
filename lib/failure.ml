(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type phase = Body | Setup | Teardown | Release
type tail = { text : string; omitted_bytes : int; log_path : string option }
type baseline = Literal of { exact : bool } | File of string

type baseline_state =
  | Missing of { proposed : string }
  | Mismatch of { expected : string; actual : string }
  | Unresolvable of { candidate : string }

type withheld = Failed_outside | Skipped

type message_diff = {
  constructor : string;
  expected_message : string;
  actual_message : string;
}

type containment_demand =
  | Anywhere
  | Ordered of { index : int; resumed_at : int }

type kind =
  | Equality of {
      expected : string;
      actual : string;
      not_ : bool;
      diffable : bool;
    }
  | Containment of {
      claim : string;
      needle : string;
      found_at : int option;
      haystack_length : int;
      excerpt : string;
      excerpt_offset : int;
      demand : containment_demand;
    }
  | Raise of {
      expected : string option;
      actual : string option;
      predicate : bool;
      backtrace : string option;
      message_diff : message_diff option;
    }
  | Baseline of {
      baseline : baseline;
      state : baseline_state;
      withheld : withheld option;
    }
  | Property of {
      rendered : string;
      summary : string option;
      case_index : int;
      shrink_steps : int;
      shrink_exhausted : bool;
      timed_out : float option;
      root : Seed.seed;
      count : int option;
      examples : bool;
      rendering : rendering;
      inner : t option;
    }
  | Message of string

and rendering = Value | Pre_image

and t = {
  kind : kind;
  phase : phase;
  loc : Loc.t option;
  msg : string option;
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

(* Boundary rules *)

let is_fatal = function
  | Sys.Break | Out_of_memory | Stack_overflow -> true
  | _ -> false

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

(* The backtrace of the most recently raised exception, when the runtime
   recorded one. Read before anything else can raise. *)
let recorded_backtrace () =
  if Printexc.backtrace_status () then
    match backtrace_to_string (Printexc.get_raw_backtrace ()) with
    | "" -> None
    | bt -> Some bt
  else None

(* Bounds. They are contract: [failure.mli] states each one, so a change here
   is a change to what users were told. *)

(* A rendering cannot be made again once the failure site is left, so it is
   captured there as a string and bounded once, here. Payload strings are
   pp-rendered values or user messages; past this many bytes they are cut
   with Text's explicit truncation marker. *)
let value_limit = 65_536

(* Captured-output tails retain at most this many final bytes; the cut is
   recorded in [omitted_bytes], not as a marker inside the text. *)
let tail_bytes = 8_192

(* Haystack excerpts in containment failures, when there is an occurrence
   or a cursor to centre on: enough context around it to read, small enough
   to store on every failure. Reuses the tail bound. *)
let excerpt_limit = tail_bytes

(* With nothing to centre on — a needle that occurs nowhere — the excerpt is
   context rather than evidence, so the head is bounded to what a reader
   scans past to reach the verdict: whichever of these comes first. *)
let head_excerpt_bytes = 1_024
let head_excerpt_lines = 10
let cap s = Text.truncate_bytes_utf8 value_limit s
let cap_opt o = Option.map cap o

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
    msg = cap_opt msg;
    subtest = [];
    output_tail = None;
  }

let equality ?loc ?msg ?(not_ = false) ~expected ~actual () =
  make ?loc ?msg
    (Equality
       { expected = cap expected; actual = cap actual; not_; diffable = true })

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

(* The bounded haystack window stored as a containment failure's [excerpt].
   One bound, applied here: renderers show what is stored whole. Around
   [anchor] when there is one — the surroundings are the evidence — and the
   bounded head otherwise. Both cuts land on UTF-8 code-point boundaries, so
   an anchored window may exceed its limit by the up to three bytes needed to
   complete a sequence. *)
let excerpt_window ~anchor haystack =
  let len = String.length haystack in
  match anchor with
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
  | Anywhere -> found_at

let containment ?loc ?msg ?found_at ?(demand = Anywhere) ~claim ~needle
    ~haystack () =
  let outside i = i < 0 || i > String.length haystack in
  (match found_at with
  | Some i when outside i ->
      invalid_arg "Failure.containment: found_at is outside the haystack"
  | Some _ | None -> ());
  (match demand with
  | Ordered { resumed_at; _ } when outside resumed_at ->
      invalid_arg "Failure.containment: resumed_at is outside the haystack"
  | Anywhere | Ordered _ -> ());
  let excerpt_offset, excerpt =
    excerpt_window ~anchor:(excerpt_anchor ~found_at ~demand) haystack
  in
  make ?loc ?msg
    (Containment
       {
         claim = cap claim;
         needle = cap needle;
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
         expected = cap claim;
         actual = cap value;
         not_ = false;
         diffable = false;
       })

let bound_message_diff { constructor; expected_message; actual_message } =
  {
    constructor = cap constructor;
    expected_message = cap expected_message;
    actual_message = cap actual_message;
  }

(* [message_diff] is stored as given. The failure site decides it with both
   exceptions in hand, so that a renderer branches on the option alone. *)
let raised ?loc ?msg ?expected ?actual ?(predicate = false) ?backtrace
    ?message_diff () =
  make ?loc ?msg
    (Raise
       {
         expected = cap_opt expected;
         actual = cap_opt actual;
         predicate;
         backtrace = cap_opt backtrace;
         message_diff = Option.map bound_message_diff message_diff;
       })

let bound_baseline_state = function
  | Missing { proposed } -> Missing { proposed = cap proposed }
  | Mismatch { expected; actual } ->
      Mismatch { expected = cap expected; actual = cap actual }
  | Unresolvable _ as state -> state

let baseline ?loc baseline state =
  (* The path is an identity: renderers name the file from it, so it is
     stored unmodified. *)
  make ?loc
    (Baseline { baseline; state = bound_baseline_state state; withheld = None })

let property ?loc ?inner ?timed_out ?count ?summary ~rendered ~case_index
    ~shrink_steps ?(shrink_exhausted = false) ~root ~examples
    ?(rendering = Value) () =
  make ?loc
    (Property
       {
         rendered = cap rendered;
         summary = cap_opt summary;
         case_index;
         shrink_steps;
         shrink_exhausted;
         timed_out;
         root;
         count;
         examples;
         rendering;
         inner;
       })

let message ?loc text = make ?loc (Message (cap text))

(* Updating *)

let with_phase phase t = { t with phase }
let with_output_tail tail t = { t with output_tail = Some tail }

let with_withheld withheld t =
  match t.kind with
  | Baseline b -> { t with kind = Baseline { b with withheld = Some withheld } }
  | Equality _ | Containment _ | Raise _ | Property _ | Message _ -> t

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
