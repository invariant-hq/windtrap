(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A stand-in for a deriver, for the goldens of the rewriters over generated
   code. It is an implementation transformation, so it runs before the
   instrumenting rewriters, as the expansion of a deriver does, and it turns
   written code into generated code by attribute:
   - an expression or a pattern that carries [[@generated]] gets a ghost
     location, as the code a deriver writes has;
   - an arm whose pattern carries [[@after_body]] gets a pattern located
     after the arm's body, as a pattern a deriver builds may be;
   - a value binding item that carries [[@@duplicate]] is followed by a copy
     of itself, with the same locations, as a deriver may copy code.
   The attributes stay, so a golden shows the nodes they changed. *)

open Ppxlib

let carries name attributes =
  List.exists (fun a -> String.equal a.attr_name.txt name) attributes

let ghost (loc : Location.t) = { loc with loc_ghost = true }

let transform =
  object
    inherit Ast_traverse.map as super

    method! expression e =
      let e = super#expression e in
      if carries "generated" e.pexp_attributes then
        { e with pexp_loc = ghost e.pexp_loc }
      else e

    method! pattern p =
      let p = super#pattern p in
      if carries "generated" p.ppat_attributes then
        { p with ppat_loc = ghost p.ppat_loc }
      else p

    method! case c =
      let c = super#case c in
      if carries "after_body" c.pc_lhs.ppat_attributes then
        let after = c.pc_rhs.pexp_loc.loc_end in
        {
          c with
          pc_lhs =
            {
              c.pc_lhs with
              ppat_loc =
                { c.pc_lhs.ppat_loc with loc_start = after; loc_end = after };
            };
        }
      else c

    method! structure items =
      super#structure items
      |> List.concat_map (fun item ->
          match item.pstr_desc with
          | Pstr_value (_, bindings)
            when List.exists
                   (fun b -> carries "duplicate" b.pvb_attributes)
                   bindings ->
              [ item; item ]
          | _ -> [ item ])
  end

let () =
  Driver.register_transformation "windtrap_test_generated"
    ~impl:transform#structure
