(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   Portions adapted from Bisect_ppx (MIT license).
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Ppxlib

(* The exclusion-attribute grammar *)

type grammar = { namespace : string; reasons : bool }

let grammar ~namespace ~reasons = { namespace; reasons }

type directive = [ `None | `Off of string | `On | `Exclude_file ]

let recognize { namespace; reasons } { attr_name; attr_payload; attr_loc } =
  if not (String.equal attr_name.txt namespace) then `None
  else
    let bad () =
      Location.raise_errorf ~loc:attr_loc "Bad payload in %s attribute."
        namespace
    in
    match attr_payload with
    | PStr [ { pstr_desc = Pstr_eval (payload, _); _ } ] -> (
        match payload.pexp_desc with
        | Pexp_ident { txt = Lident "off"; _ } -> `Off ""
        | Pexp_ident { txt = Lident "on"; _ } -> `On
        | Pexp_ident { txt = Lident "exclude_file"; _ } -> `Exclude_file
        | Pexp_apply
            ( { pexp_desc = Pexp_ident { txt = Lident "off"; _ }; _ },
              [
                ( Nolabel,
                  {
                    pexp_desc = Pexp_constant (Pconst_string (reason, _, _));
                    _;
                  } );
              ] )
          when reasons ->
            `Off reason
        | _ -> bad ())
    | _ -> bad ()

(* Folds rather than short-circuits so every attribute is
   error-checked. *)
let off_reason g attributes =
  List.fold_left
    (fun found attribute ->
      match recognize g attribute with
      | `None -> found
      | `Off reason -> Some reason
      | `On ->
          Location.raise_errorf ~loc:attribute.attr_loc
            "%s on is not allowed here." g.namespace
      | `Exclude_file ->
          Location.raise_errorf ~loc:attribute.attr_loc
            "%s exclude_file is not allowed here." g.namespace)
    None attributes

let has_off_attribute g attributes = off_reason g attributes <> None

let has_exclude_file_attribute g structure =
  List.exists
    (function
      | { pstr_desc = Pstr_attribute attribute; _ } -> (
          match recognize g attribute with
          | `Exclude_file -> true
          | `None | `Off _ | `On -> false)
      | _ -> false)
    structure

(* Entry filtering *)

let always_ignore_paths = [ "//toplevel//"; "(stdin)" ]
let always_ignore_basenames = [ ".ocamlinit"; "topfind" ]

let excluded_file g ~file ast =
  List.mem file always_ignore_paths
  || List.mem (Filename.basename file) always_ignore_basenames
  || has_exclude_file_attribute g ast

(* Semantics preservation *)

let rec is_trivial_syntactic_value e =
  match e.pexp_desc with
  | Pexp_function _ | Pexp_poly _ | Pexp_ident _ | Pexp_constant _
  | Pexp_construct (_, None) ->
      true
  | Pexp_constraint (inner, _) | Pexp_coerce (inner, _, _) ->
      is_trivial_syntactic_value inner
  | _ -> false

(* The generated preamble *)

let ghost_loc ~file = { (Location.in_file file) with loc_ghost = true }

let mangled_module_name ~prefix ~file =
  let buffer = Buffer.create (String.length prefix + String.length file + 1) in
  Buffer.add_string buffer prefix;
  String.iter
    (function
      | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_') as c ->
          Buffer.add_char buffer c
      | _ -> Buffer.add_string buffer "___")
    file;
  Buffer.contents buffer

let preamble ~loc ~module_name ~opened bindings =
  let generated_module =
    Ast_helper.Str.module_ ~loc
      (Ast_helper.Mb.mk ~loc
         { txt = Some module_name; loc }
         (Ast_helper.Mod.structure ~loc bindings))
  in
  let stop_comment = [%stri [@@@ocaml.text "/*"]] in
  if opened then
    let module_open =
      Ast_helper.Str.open_ ~loc
        (Ast_helper.Opn.mk ~loc
           (Ast_helper.Mod.ident ~loc { txt = Lident module_name; loc }))
    in
    [ stop_comment; generated_module; module_open; stop_comment ]
  else [ stop_comment; generated_module; stop_comment ]
