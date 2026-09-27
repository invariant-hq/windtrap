(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Texts *)

let value_limit = 65_536

type text = { kept : string; length : int }

(* Bounded once, where the value is rendered: the cut is data, and a
   renderer spells it from [length]. *)
let text s =
  {
    kept = snd (Text.window ~bytes:value_limit Head s);
    length = String.length s;
  }

let is_cut t = String.length t.kept < t.length

(* Types *)

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
      failed_again : bool option;
    }
  | Law of {
      law : string;
      clause : string option;
      equation : string;
      terms : law_term list;
    }
  | Timeout of { limit : float; case : timed_case option }
  | Message of text

and law_term =
  | Term of { name : string; value : text }
  | Side of { name : string; value : text }
  | Failed of { name : string; failure : t }

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

(* Control *)

type control = [ `Skip of string option | `Timeout of float | `Exit | `Discard ]

exception Check_failure of t
exception Control of control

(* One printer, so that no report names [Windtrap__Failure]. *)
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

(* An interrupt or exhausted memory stops the run. OCaml 5 recovers from a
   [Stack_overflow], which fails the test that raised it. *)
let is_fatal = function Sys.Break | Out_of_memory -> true | _ -> false

(* A finally cut by a control or a fatal exception is unwrapped, so the cut
   reaches its owner; a finally's own exception stays wrapped. *)
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
      (* Read first: a raise inside [classify] would replace it. *)
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

(* Dune compiles an executable's modules under [Dune__exe]. The prefix is cut
   wherever a name starts with it: in an exception's text, a printer's
   included, and in a backtrace's frames. *)
let exe_wrapper = "Dune__exe__"

let unwrapped s =
  let buf = Buffer.create (String.length s) in
  let starts_name i =
    i = 0
    ||
    match s.[i - 1] with
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> false
    | _ -> true
  in
  let rec scan i =
    match Text.first_occurrence ~start:i ~pattern:exe_wrapper s with
    | Some j when starts_name j ->
        Buffer.add_substring buf s i (j - i);
        scan (j + String.length exe_wrapper)
    | Some j ->
        Buffer.add_substring buf s i (j + 1 - i);
        scan (j + 1)
    | None -> Buffer.add_substring buf s i (String.length s - i)
  in
  scan 0;
  Buffer.contents buf

let exn_to_string exn = unwrapped (Printexc.to_string exn)
let caught_to_string c = exn_to_string (to_exn c)

(* Backtraces *)

(* Windtrap's own frames below the deepest frame of the reader's code name
   none of it: that trailing run is dropped, unless every frame is
   windtrap's. A frame without a name is not proven ours and ends the run. *)
let trimmed raw =
  let whole () = Printexc.raw_backtrace_to_string raw in
  match Printexc.backtrace_slots raw with
  | None -> whole ()
  | Some slots ->
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
        (* Each frame keeps its index, which words it "Raised at" or
           "Called from". *)
        for i = 0 to keep do
          match Printexc.Slot.format i slots.(i) with
          | Some line ->
              Buffer.add_string buffer line;
              Buffer.add_char buffer '\n'
          | None -> ()
        done;
        Buffer.contents buffer
      end

let backtrace_to_string raw = unwrapped (trimmed raw)

(* Constructors *)

let tail_bytes = 8_192

(* Without an anchor the excerpt is context, bounded to what a reader scans
   past to reach the verdict. *)
let context_lines = 10
let context_bytes = 1_024

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

(* An ordered search failed from its cursor: an occurrence before the cursor
   is the one that did not count. *)
let excerpt ~found_at ~demand haystack =
  match (demand, found_at) with
  | Ordered { resumed_at; _ }, _ ->
      Text.window ~bytes:tail_bytes (Around resumed_at) haystack
  | (Anywhere | Prefix | Suffix), Some i ->
      Text.window ~bytes:tail_bytes (Around i) haystack
  | Suffix, None ->
      Text.window ~lines:context_lines ~bytes:context_bytes Tail haystack
  | (Anywhere | Prefix), None ->
      Text.window ~lines:context_lines ~bytes:context_bytes Head haystack

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
  let excerpt_offset, excerpt = excerpt ~found_at ~demand haystack in
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

(* A claim describes the expected value: its sides are never diffed. *)
let predicate ?loc ?msg ~claim value =
  make ?loc ?msg
    (Equality
       {
         expected = text claim;
         actual = text value;
         not_ = false;
         diffable = false;
       })

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
  make ?loc (Baseline { baseline; state; withheld = None })

let property ?loc ?inner ?count ?summary ~rendered ~case_index ~shrink_steps
    ?(shrink_end = Converged) ~root ~examples ?(rendering = Value) ?failed_again
    () =
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
         failed_again;
       })

let law ?loc ?msg ?clause ~law ~equation terms =
  make ?loc ?msg (Law { law; clause; equation; terms })

let timeout ?loc ?case limit = make ?loc (Timeout { limit; case })
let message ?loc s = make ?loc (Message (text s))

let of_fault : fault -> t = function
  | `Assertion failure -> failure
  | `Exception (exn, backtrace) ->
      raised ~actual:(exn_to_string exn)
        ~backtrace:(backtrace_to_string backtrace)
        ()

(* Updating *)

let with_phase phase t = { t with phase }
let with_output_tail tail t = { t with output_tail = Some tail }

let with_withheld withheld t =
  match t.kind with
  | Baseline { withheld = Some (Refused _ | Conflict); _ } -> t
  | Baseline b -> { t with kind = Baseline { b with withheld = Some withheld } }
  | Equality _ | Containment _ | Raise _ | Property _ | Law _ | Timeout _
  | Message _ ->
      t

(* Captured-output tails *)

let tail ?log_path ?(omitted_bytes = 0) text =
  if omitted_bytes < 0 then
    invalid_arg "Failure.tail: omitted_bytes is negative";
  let cut, text = Text.window ~bytes:tail_bytes Tail text in
  { text; omitted_bytes = omitted_bytes + cut; log_path }

(* Per-test outcomes *)

type outcome = Pass | Fail of t list | Skip of string option
