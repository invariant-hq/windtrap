(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type pos = string * int * int * int
type here = Lexing.position
type location = Pos of pos | Here of here | No_location
type diff_part = { text : string; highlight : bool }

type diff_output = {
  expected_parts : diff_part list;
  actual_parts : diff_part list;
}

type t = {
  message : string;
  expected : string option;
  actual : string option;
  diff : diff_output option;
  location : location;
}

exception Check_failure of t
exception Skip_test of string option
exception Timeout of float

let make ?here ?pos ?expected ?actual ?diff message =
  let location =
    match (here, pos) with
    | Some h, _ -> Here h
    | _, Some p -> Pos p
    | None, None -> No_location
  in
  { message; expected; actual; diff; location }

let raise_failure ?here ?pos ?expected ?actual ?diff message =
  raise (Check_failure (make ?here ?pos ?expected ?actual ?diff message))

let skip ?reason () = raise (Skip_test reason)

let pp_file_location ppf ~file ~line ~col ~end_col =
  Pp.(
    styled `Bold (fun ppf () ->
        pf ppf "File %S, line %d, characters %d-%d:" file line col end_col))
    ppf ();
  Pp.pf ppf "@."

let pp_location ppf = function
  | No_location -> ()
  | Pos (file, line, col, end_col) ->
      pp_file_location ppf ~file ~line ~col ~end_col
  | Here { pos_fname; pos_lnum; pos_cnum; pos_bol } ->
      let col = pos_cnum - pos_bol in
      pp_file_location ppf ~file:pos_fname ~line:pos_lnum ~col ~end_col:col

let pp_labeled ppf ~label ~style value =
  Pp.pf ppf "@.  %s: %a" label (Pp.styled style Pp.string) value

let pp_diff_parts ~style ppf parts =
  List.iter
    (fun { text; highlight } ->
      if highlight then Pp.pf ppf "%a" (Pp.styled style Pp.string) text
      else Pp.pf ppf "%s" text)
    parts

let pp_labeled_diff ppf ~label ~style parts =
  Pp.pf ppf "@.  %s: " label;
  pp_diff_parts ~style ppf parts

let pp ppf { message; expected; actual; diff; location } =
  pp_location ppf location;
  Pp.pf ppf "%s" message;
  match diff with
  | Some { expected_parts; actual_parts } ->
      pp_labeled_diff ppf ~label:"Expected" ~style:`Green expected_parts;
      pp_labeled_diff ppf ~label:"Actual  " ~style:`Red actual_parts
  | None ->
      Option.iter (pp_labeled ppf ~label:"Expected" ~style:`Green) expected;
      Option.iter (pp_labeled ppf ~label:"Actual  " ~style:`Red) actual
