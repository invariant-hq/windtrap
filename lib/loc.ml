(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type pos = string * int * int * int
type t = { file : string; line : int; column : int }

let of_pos (file, line, column, _end_column) = { file; line; column }
let to_string loc = Pp.str "%s:%d" loc.file loc.line

(* A slot's compilation unit is the text before the first '.' of its
   defname. *)
let unit_of defname =
  match String.index_opt defname '.' with
  | Some i -> String.sub defname 0 i
  | None -> defname

(* Dune names the units of a wrapped library [lib] as [lib], its alias unit,
   and [lib__M] for each of its modules. *)
let in_library lib unit_name =
  unit_name = lib || String.starts_with ~prefix:(lib ^ "__") unit_name

let own_unit defname =
  let unit_name = unit_of defname in
  in_library "Windtrap" unit_name || in_library "Windtrap_runtime" unit_name

(* The standard library's internals are the units [CamlinternalFoo]. *)
let internal_unit defname =
  let unit_name = unit_of defname in
  own_unit defname
  || in_library "Stdlib" unit_name
  || String.starts_with ~prefix:"Camlinternal" unit_name

(* Top-level, so that its defname is [delimiter_name]. The exception case
   makes the call to [fn] non-tail. *)
let[@inline never] delimit fn =
  match fn () with
  | v -> v
  | exception e ->
      Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ())

(* A toolchain that changed this defname fails test_loc.ml's recognition
   test, and the walk then passes the delimiter as though it were absent. *)
let delimiter_name = "Windtrap__Loc.delimit"

let is_delimiter slot =
  Option.equal String.equal (Printexc.Slot.name slot) (Some delimiter_name)

(* A slot without a name cannot be proven to be user code, so it is skipped:
   no location rather than a wrong one. *)
let user_location slot =
  match Printexc.Slot.name slot with
  | Some name when not (internal_unit name) ->
      Option.map
        (fun { Printexc.filename; line_number; start_char; _ } ->
          { file = filename; line = line_number; column = start_char })
        (Printexc.Slot.location slot)
  | Some _ | None -> None

(* Failure sites sit a handful of frames below user code; 24 raw entries
   is comfortably enough while keeping capture cheap to call at every
   failure construction. *)
let callstack_depth = 24

(* The inlined slots of one entry come innermost first, as the entries do.
   The walk ends at the delimiter before it classifies a slot: the
   delimiter's unit is windtrap's, so classification would walk past it. *)
let capture () =
  let slots entry =
    Array.to_seq
      (Option.value ~default:[||] (Printexc.backtrace_slots_of_raw_entry entry))
  in
  Printexc.get_callstack callstack_depth
  |> Printexc.raw_backtrace_entries |> Array.to_seq |> Seq.concat_map slots
  |> Seq.take_while (Fun.negate is_delimiter)
  |> Seq.find_map user_location

let resolve ?__POS__ () =
  match __POS__ with Some p -> Some (of_pos p) | None -> capture ()

let equal a b =
  String.equal a.file b.file && a.line = b.line && a.column = b.column
