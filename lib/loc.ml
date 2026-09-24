(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type pos = string * int * int * int
type t = { file : string; line : int; column : int }

let of_pos (file, line, column, _end_column) = { file; line; column }

(* Call-stack capture *)

(* Slots are classified by the compilation unit of their defname: the
   segment before the first '.' of [Printexc.Slot.name]. A wrapped
   windtrap module is "Windtrap__Foo" (the alias unit is "Windtrap"), a
   stdlib module is "Stdlib__Foo" or "Stdlib", and stdlib internals are
   "CamlinternalFoo". A slot without a name cannot be proven to be user
   code, so it is skipped: no location rather than a wrong one. *)
let unit_of name =
  match String.index_opt name '.' with
  | Some i -> String.sub name 0 i
  | None -> name

(* Windtrap's own units, and only those: the alias unit, its wrapped
   modules, and the instrumentation runtime's (its alias unit and its
   wrapped modules). The test matches whole unit names rather than a bare
   "Windtrap" prefix, so a user library called [Windtrap_helpers] stays
   foreign. *)
let own_unit name =
  let unit_name = unit_of name in
  unit_name = "Windtrap"
  || String.starts_with ~prefix:"Windtrap__" unit_name
  || unit_name = "Windtrap_runtime"
  || String.starts_with ~prefix:"Windtrap_runtime__" unit_name

let internal_unit name =
  let unit_name = unit_of name in
  own_unit name || unit_name = "Stdlib"
  || String.starts_with ~prefix:"Stdlib__" unit_name
  || String.starts_with ~prefix:"Camlinternal" unit_name

(* Failure sites sit a handful of frames below user code; 24 raw entries
   is comfortably enough while keeping capture cheap to call at every
   failure construction. *)
let callstack_depth = 24

(* The delimiter windtrap's runner wraps user callbacks in. Top-level so its
   debug name is the stable "Windtrap__Loc.delimit"; [@inline never] so the
   frame exists; the exception case makes the call to [fn] non-tail, so the
   frame persists while [fn] runs. *)
let[@inline never] delimit fn =
  match fn () with
  | v -> v
  | exception e ->
      Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ())

(* Recognition is by defname, pinned by test_loc.ml's recognition test: a
   toolchain defname change fails that test loudly and capture degrades to
   pre-delimiter behavior, never to something new. *)
let delimiter_name = "Windtrap__Loc.delimit"

let location_of_slot slot =
  match Printexc.Slot.name slot with
  | None -> None
  | Some name when internal_unit name -> None
  | Some _ -> (
      match Printexc.Slot.location slot with
      | None -> None
      | Some { Printexc.filename; line_number; start_char; _ } ->
          Some { file = filename; line = line_number; column = start_char })

(* Reaching the delimiter means that every frame since the failing call was
   windtrap's own, so a user frame beyond it belongs to the caller of the
   runner and not to the failure site. The walk stops there and answers
   [None]. *)
let capture () =
  let raw = Printexc.get_callstack callstack_depth in
  let entries = Printexc.raw_backtrace_entries raw in
  let exception Found of t in
  let exception Stop in
  try
    Array.iter
      (fun entry ->
        match Printexc.backtrace_slots_of_raw_entry entry with
        | None -> ()
        | Some slots ->
            (* Inlined frames of one entry appear innermost first, like
               the entries themselves. *)
            Array.iter
              (fun slot ->
                (* The delimiter check runs before the internal-unit skip:
                   the delimiter's own unit is windtrap's, so classification
                   would silently walk past it. *)
                match Printexc.Slot.name slot with
                | Some name when String.equal name delimiter_name ->
                    raise_notrace Stop
                | Some _ | None -> (
                    match location_of_slot slot with
                    | Some loc -> raise_notrace (Found loc)
                    | None -> ()))
              slots)
      entries;
    None
  with
  | Found loc -> Some loc
  | Stop -> None

let resolve ?__POS__ () =
  match __POS__ with Some p -> Some (of_pos p) | None -> capture ()

(* Observers *)

(* Not exported: nothing formats a location through [Format], and
   [to_string] is the one spelling reports use. *)
let pp ppf loc = Format.fprintf ppf "%s:%d" loc.file loc.line
let to_string loc = Format.asprintf "%a" pp loc

let equal a b =
  String.equal a.file b.file && a.line = b.line && a.column = b.column
