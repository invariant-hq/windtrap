(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type phase = Body | Setup | Teardown | Release
type tail = { text : string; omitted_bytes : int; log_path : string option }

type snapshot_state =
  | Missing of { proposed : string }
  | Mismatch of { expected : string; actual : string }
  | Unresolvable
  | Duplicate of { first : Loc.t option; first_test : string }

type message_diff = {
  constructor : string;
  expected_message : string;
  actual_message : string;
}

type containment_demand =
  | Anywhere
  | Ordered of { index : int; resumed_at : int }

type kind =
  | Equality of { expected : string; actual : string; not_ : bool }
  | Containment of {
      claim : string;
      needle : string;
      found_at : int option;
      haystack_length : int;
      excerpt : string;
      excerpt_offset : int;
      demand : containment_demand;
    }
  | Predicate of { claim : string; value : string }
  | Raise of {
      expected : string option;
      actual : string option;
      predicate : bool;
      backtrace : string option;
      message_diff : message_diff option;
    }
  | Snapshot of { name : string; path : string; state : snapshot_state }
  | Property of {
      rendered : string;
      case_index : int;
      shrink_steps : int;
      shrink_exhausted : bool;
      timed_out : float option;
      root : Seed.seed;
      count : int option;
      max_shrink : int option;
      examples : bool;
      printerless : bool;
      inner : t option;
    }
  | Message of string
  | Stale_baselines of string list

and t = {
  kind : kind;
  phase : phase;
  loc : Loc.t option;
  msg : string option;
  subtest : string list;
  output_tail : tail option;
}

exception Check_failure of t
exception Skip_test of string option
exception Timeout of float
exception Exit_attempt

(* The printer is load-bearing for byte-consistency: release-failure
   messages, the property engine's raised-exception rendering, and every
   other stringification site agree without per-site special cases. *)
let () =
  Printexc.register_printer (function
    | Exit_attempt ->
        Some
          "Exit_attempt (code under test called exit; intercepted by windtrap)"
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

(* Bounds. Implementation constants, not contract — except [tail_bytes],
   which the .mli exposes because capture-side readers must size their
   reads by it. *)

(* Payload strings are pp-rendered values or user messages; past this many
   bytes they are cut with Text's explicit truncation marker. *)
let value_limit = 65_536

(* Captured-output tails retain at most this many final bytes; the cut is
   recorded in [omitted_bytes], not as a marker inside the text. *)
let tail_bytes = 8_192

(* Haystack excerpts in containment failures reuse the tail bound: enough
   context to read, small enough to store on every failure. *)
let excerpt_limit = tail_bytes
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
    (Equality { expected = cap expected; actual = cap actual; not_ })

(* The bounded haystack window stored as a containment failure's [excerpt]:
   around [anchor] when there is one, the head otherwise. Both cuts land on
   UTF-8 code-point boundaries, so the window may exceed the limit by the up
   to three bytes needed to complete a sequence. *)
let excerpt_window ~anchor haystack =
  let len = String.length haystack in
  if len <= excerpt_limit then (0, haystack)
  else
    let start =
      match anchor with
      | None -> 0
      | Some i ->
          let at_or_before = max 0 (i - (excerpt_limit / 2)) in
          utf8_boundary_at_or_after haystack at_or_before
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

let predicate ?loc ?msg ~claim value =
  make ?loc ?msg (Predicate { claim = cap claim; value = cap value })

let bound_message_diff { constructor; expected_message; actual_message } =
  {
    constructor = cap constructor;
    expected_message = cap expected_message;
    actual_message = cap actual_message;
  }

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

let bound_snapshot_state = function
  | Missing { proposed } -> Missing { proposed = cap proposed }
  | Mismatch { expected; actual } ->
      Mismatch { expected = cap expected; actual = cap actual }
  | (Unresolvable | Duplicate _) as state -> state

let snapshot ?loc ~name ~path state =
  (* [name] and [path] are identities: renderers derive acceptance commands
     from them, so they are stored unmodified. *)
  make ?loc (Snapshot { name; path; state = bound_snapshot_state state })

let property ?loc ?inner ?timed_out ?count ?max_shrink ~rendered ~case_index
    ~shrink_steps ?(shrink_exhausted = false) ~root ~examples
    ?(printerless = false) () =
  make ?loc
    (Property
       {
         rendered = cap rendered;
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
       })

let message ?loc text = make ?loc (Message (cap text))

let stale_baselines paths =
  if paths = [] then invalid_arg "Failure.stale_baselines: paths is empty";
  (* Paths are identities: renderers derive the display spelling and the
     removal-hint command from them, so they are stored unmodified. *)
  make (Stale_baselines paths)

(* Updating *)

let with_phase phase t = { t with phase }
let with_output_tail tail t = { t with output_tail = Some tail }

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
