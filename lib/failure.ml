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

type claim =
  | Equal
  | Contains of {
      needle : string;
      found_at : int option;
      haystack_length : int;
      excerpt_offset : int;
    }
  | Satisfies
  | Matches

type kind =
  | Equality of {
      expected : string;
      actual : string;
      not_ : bool;
      claim : claim;
    }
  | Raise of {
      expected : string option;
      actual : string option;
      predicate : bool;
      backtrace : string option;
      same_constructor : bool;
      expected_message : string option;
      actual_message : string option;
    }
  | Snapshot of { name : string; path : string; state : snapshot_state }
  | Property of {
      rendered : string;
      case_index : int;
      shrink_steps : int;
      timed_out : float option;
      root : Seed.seed;
      count : int option;
      examples : bool;
      inner : t option;
    }
  | Message of string

and t = {
  kind : kind;
  phase : phase;
  loc : Loc.t option;
  msg : string option;
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

(* ───── Boundary rules ───── *)

let is_fatal = function
  | Sys.Break | Out_of_memory | Stack_overflow -> true
  | _ -> false

(* The backtrace of the most recently raised exception, when the runtime
   recorded one. Read before anything else can raise. *)
let recorded_backtrace () =
  if Printexc.backtrace_status () then
    match Printexc.get_backtrace () with "" -> None | bt -> Some bt
  else None

(* ───── Bounds (implementation constants, not contract) ───── *)

(* Payload strings are pp-rendered values or user messages; past this many
   bytes they are cut with Text's explicit truncation marker. *)
let value_limit = 65_536

(* Captured-output tails retain at most this many final bytes; the cut is
   recorded in [omitted_bytes], not as a marker inside the text. *)
let tail_limit = 8_192

(* Haystack excerpts in containment failures reuse the tail bound: enough
   context to read, small enough to store on every failure. *)
let excerpt_limit = tail_limit
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

(* ───── Constructors ───── *)

let make ?loc ?msg kind =
  { kind; phase = Body; loc; msg = cap_opt msg; output_tail = None }

let equality ?loc ?msg ?(not_ = false) ?(claim = Equal) ~expected ~actual () =
  (* Renderers rely on negation and claim refinement never combining (see
     the [kind] doc); reject the combination where it would be built. *)
  if not_ && claim <> Equal then
    invalid_arg "Failure.equality: not_ applies only to the Equal claim";
  make ?loc ?msg
    (Equality { expected = cap expected; actual = cap actual; not_; claim })

(* The bounded haystack window stored as a containment failure's [actual]:
   around the match when there is one, the head otherwise. Both cuts land on
   UTF-8 code-point boundaries, so the window may exceed the limit by the up
   to three bytes needed to complete a sequence. *)
let excerpt_window ~found_at haystack =
  let len = String.length haystack in
  if len <= excerpt_limit then (0, haystack)
  else
    let start =
      match found_at with
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

let containment ?loc ?msg ?found_at ~expected ~needle ~haystack () =
  (match found_at with
  | Some i when i < 0 || i > String.length haystack ->
      invalid_arg "Failure.containment: found_at is outside the haystack"
  | Some _ | None -> ());
  let excerpt_offset, excerpt = excerpt_window ~found_at haystack in
  make ?loc ?msg
    (Equality
       {
         expected = cap expected;
         actual = excerpt;
         not_ = false;
         claim =
           Contains
             {
               needle = cap needle;
               found_at;
               haystack_length = String.length haystack;
               excerpt_offset;
             };
       })

let raised ?loc ?msg ?expected ?actual ?(predicate = false) ?backtrace
    ?(same_constructor = false) ?expected_message ?actual_message () =
  make ?loc ?msg
    (Raise
       {
         expected = cap_opt expected;
         actual = cap_opt actual;
         predicate;
         backtrace = cap_opt backtrace;
         same_constructor;
         expected_message = cap_opt expected_message;
         actual_message = cap_opt actual_message;
       })

let bound_snapshot_state = function
  | Missing { proposed } -> Missing { proposed = cap proposed }
  | Mismatch { expected; actual } ->
      Mismatch { expected = cap expected; actual = cap actual }
  | (Unresolvable | Duplicate _) as state -> state

let snapshot ?loc ~name ~path state =
  (* [name] and [path] are identities: renderers derive acceptance commands
     from them (Law 3), so they are stored unmodified. *)
  make ?loc (Snapshot { name; path; state = bound_snapshot_state state })

let property ?loc ?inner ?timed_out ?count ~rendered ~case_index ~shrink_steps
    ~root ~examples () =
  make ?loc
    (Property
       {
         rendered = cap rendered;
         case_index;
         shrink_steps;
         timed_out;
         root;
         count;
         examples;
         inner;
       })

let message ?loc text = make ?loc (Message (cap text))

(* ───── Updating ───── *)

let with_phase phase t = { t with phase }
let with_output_tail tail t = { t with output_tail = Some tail }

let tail ?log_path ?(omitted_bytes = 0) text =
  if omitted_bytes < 0 then
    invalid_arg "Failure.tail: omitted_bytes is negative";
  let len = String.length text in
  if len <= tail_limit then { text; omitted_bytes; log_path }
  else
    let cut = utf8_boundary_at_or_after text (len - tail_limit) in
    {
      text = String.sub text cut (len - cut);
      omitted_bytes = omitted_bytes + cut;
      log_path;
    }

(* ───── Per-test outcomes ───── *)

type outcome = Pass | Fail of t list | Skip of string option
