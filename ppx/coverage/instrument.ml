(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   Portions adapted from Bisect_ppx (MIT license).
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Ppxlib
open Ast_builder.Default
module Exp = Ast_helper.Exp

(* Attributes *)

(* The grammar is ppx/mutate/instrument.ml's too, under its own name;
   test/cram/ppx/coverage.t pins that the two agree. The reason of an [off] is
   for the reader of the source. *)
let coverage_attribute { attr_name; attr_payload; attr_loc = loc } =
  if not (String.equal attr_name.txt "coverage") then `None
  else
    let payload =
      match attr_payload with
      | PStr [ { pstr_desc = Pstr_eval (payload, _); _ } ] ->
          Some payload.pexp_desc
      | _ -> None
    in
    match payload with
    | Some (Pexp_ident { txt = Lident "off"; _ })
    | Some
        (Pexp_apply
           ( { pexp_desc = Pexp_ident { txt = Lident "off"; _ }; _ },
             [ (Nolabel, { pexp_desc = Pexp_constant (Pconst_string _); _ }) ]
           )) ->
        `Off
    | Some (Pexp_ident { txt = Lident "on"; _ }) -> `On
    | Some (Pexp_ident { txt = Lident "exclude_file"; _ }) -> `Exclude_file
    | _ -> Location.raise_errorf ~loc "Bad payload in coverage attribute."

let err_misplaced ~loc spelling =
  Location.raise_errorf ~loc "coverage %s is not allowed here." spelling

(* Every attribute is checked, not only those before an [off]. *)
let is_off attributes =
  List.fold_left
    (fun off attribute ->
      match coverage_attribute attribute with
      | `None -> off
      | `Off -> true
      | `On -> err_misplaced ~loc:attribute.attr_loc "on"
      | `Exclude_file -> err_misplaced ~loc:attribute.attr_loc "exclude_file")
    false attributes

let is_tail_mod_cons attributes =
  List.exists
    (fun { attr_name = { txt; _ }; _ } ->
      txt = "tail_mod_cons" || txt = "ocaml.tail_mod_cons")
    attributes

let excludes_file = function
  | { pstr_desc = Pstr_attribute attribute; _ } ->
      coverage_attribute attribute = `Exclude_file
  | _ -> false

(* Inline tests *)

(* ppx_windtrap marks each item it generates for a test with
   [[@@windtrap.test]]. Without ppx_windtrap a test stays an extension node,
   which is never traversed. *)
let is_test_item si =
  let marked =
    List.exists (fun a -> String.equal a.attr_name.txt "windtrap.test")
  in
  match si.pstr_desc with
  | Pstr_value (_, bindings) ->
      List.exists (fun b -> marked b.pvb_attributes) bindings
  | Pstr_module mb -> marked mb.pmb_attributes
  | _ -> false

(* Points *)

(* [key], the identity of a point, is one byte offset of the expression the
   point is attributed to; the extent is what a report paints. *)
type point = { key : int; start_ofs : int; end_ofs : int }

type state = {
  mutable rev_points : point list; (* newest first *)
  mutable count : int; (* the length of [rev_points] *)
  mutable uses_post : bool; (* an out-edge was marked *)
}

(* The index of the point at [key], allocated with [extent] unless there is
   one already, whose extent then stands. *)
let point st ~key (extent : location) =
  let rec find index = function
    | p :: _ when p.key = key -> index
    | _ :: older -> find (index - 1) older
    | [] ->
        let start_ofs = extent.loc_start.pos_cnum in
        let end_ofs = extent.loc_end.pos_cnum in
        st.rev_points <- { key; start_ofs; end_ofs } :: st.rev_points;
        st.count <- st.count + 1;
        st.count - 1
  in
  find (st.count - 1) st.rev_points

(* Marks *)

(* A mark is attributed to [anchor]: its point is keyed at the anchor's
   start, or at its last byte under [at_end], and a generated (ghost) or
   switched-off anchor takes no mark. The wrapper takes the location of [e],
   so a later mark of the same node keys and paints the source. *)
let mark st ~anchor ~at_end ~extent ~post e =
  let at = anchor.pexp_loc in
  if at.loc_ghost || is_off anchor.pexp_attributes then e
  else
    let key =
      if at_end then at.loc_end.pos_cnum - 1 else at.loc_start.pos_cnum
    in
    let loc = e.pexp_loc in
    let index = eint ~loc (point st ~key extent) in
    if post then begin
      st.uses_post <- true;
      [%expr ___windtrap_post_visit___ [%e index] [%e e]]
    end
    else
      [%expr
        ___windtrap_visit___ [%e index];
        [%e e]]

let entry st ?extent e =
  let extent = Option.value extent ~default:e.pexp_loc in
  mark st ~anchor:e ~at_end:false ~extent ~post:false e

(* The body of an arm paints the whole arm, from its pattern, so an arm never
   entered shows its [| pattern ->] line. The guard runs whenever the pattern
   matches, so it is a block of its own. An arm that cannot be entered
   ([assert false], a refutation) or is switched off keeps its guard unmarked
   too. *)
let arm st case =
  match case.pc_rhs with
  | [%expr assert false] | { pexp_desc = Pexp_unreachable; _ } -> case
  | { pexp_attributes; _ } when is_off pexp_attributes -> case
  | rhs ->
      let pat = case.pc_lhs.ppat_loc in
      let extent =
        if
          pat.loc_ghost
          || pat.loc_start.pos_cnum > rhs.pexp_loc.loc_start.pos_cnum
        then rhs.pexp_loc
        else { rhs.pexp_loc with loc_start = pat.loc_start }
      in
      let pc_guard = Option.map (entry st) case.pc_guard in
      let pc_rhs = entry st ~extent rhs in
      { case with pc_guard; pc_rhs }

(* Semantics guards *)

(* [lazy] compiles a trivial syntactic value as already forced, and a visit
   would make it a thunk. ppx/mutate/instrument.ml has the same test. *)
let rec is_trivial_syntactic_value e =
  match e.pexp_desc with
  | Pexp_function _ | Pexp_poly _ | Pexp_ident _ | Pexp_constant _
  | Pexp_construct (_, None) ->
      true
  | Pexp_constraint (inner, _) | Pexp_coerce (inner, _, _) ->
      is_trivial_syntactic_value inner
  | _ -> false

(* The primitives whose applications take no out-edge, from Bisect_ppx's
   list. They cannot fail interestingly, and an out-edge on every operator
   would double the table for no signal. *)
let trivial_primitives =
  String.split_on_char ' '
    "&& & not = <> < <= > >= == != ref ! := @ ^ + - * / +. -. *. /. mod land \
     lor lxor lsl lsr asr ignore ##"

let is_trivial_function e =
  match e.pexp_desc with
  | Pexp_ident { txt = Lident name; _ } -> List.mem name trivial_primitives
  | Pexp_ident
      {
        txt =
          Ldot (Lident "Sys", "opaque_identity") | Ldot (Lident "Obj", "magic");
        _;
      } ->
      true
  | _ -> false

(* The functions that never return, matched by spelling as the primitives
   are. A visit after their call could never run, so the call takes no
   out-edge, and as the right operand of [||] no point for being true. *)
let never_returning = [ "raise"; "raise_notrace"; "failwith" ]

let is_never_returning e =
  match e.pexp_desc with
  | Pexp_ident { txt = Lident name; _ } -> List.mem name never_returning
  | _ -> false

(* Whether [e] calls a function that never returns, directly or through
   [@@], [|>] or [|.]. *)
let calls_never_returning e =
  match e.pexp_desc with
  | Pexp_apply ([%expr ( @@ )], [ (_, f); _ ])
  | Pexp_apply (([%expr ( |> )] | [%expr ( |. )]), [ _; (_, f) ])
  | Pexp_apply (f, _) ->
      is_never_returning f
  | _ -> false

(* Whether a tail call can sit in [e] when [e] is in tail position. The
   right operand of [||] that can hold one keeps its position and gives up its
   point, since an [if] condition would take the call out of tail position. *)
let holds_tail_call e =
  match e.pexp_desc with
  | Pexp_apply (callee, _) -> not (is_trivial_function callee)
  | Pexp_send _ | Pexp_new _ | Pexp_let _ | Pexp_letmodule _
  | Pexp_letexception _ | Pexp_open _ | Pexp_match _ | Pexp_try _
  | Pexp_ifthenelse _ | Pexp_sequence _ | Pexp_letop _ | Pexp_constraint _
  | Pexp_coerce _ ->
      true
  | _ -> false

(* Traversal *)

(* The position of an expression decides its out-edge. In [Tail] position a
   call is never wrapped, since the wrap would take it out of tail position.
   Elsewhere the out-edge is keyed at the start of the expression control
   reaches next, when it is known ([Before]); is left to an enclosing form
   that observes the return itself ([Observed]); or is keyed at the
   expression's own callee ([Nontail]). *)
type position = Tail | Nontail | Before of expression | Observed

let is_tail = function Tail -> true | Nontail | Before _ | Observed -> false

(* A sub-expression that ends its parent's evaluation is in tail position
   when its parent is, and knows no successor. *)
let inherited position = if is_tail position then Tail else Nontail

(* The head function of a curried application. *)
let rec head_callee e =
  match e.pexp_desc with
  | Pexp_apply (f, _) when not (is_off e.pexp_attributes) -> head_callee f
  | _ -> e

let out_edge st position ~callee e =
  match position with
  | Tail | Observed -> e
  | Nontail ->
      mark st ~anchor:callee ~at_end:true ~extent:e.pexp_loc ~post:true e
  | Before next ->
      mark st ~anchor:next ~at_end:false ~extent:e.pexp_loc ~post:true e

(* A node's sub-expressions are traversed before its own blocks are marked,
   so a wrapper is never traversed and inner points are numbered first. *)
class instrumenter st =
  object (self)
    inherit Ast_traverse.map as super

    (* Set in a [[@@@coverage off]] region; [structure] restores it on exit. *)
    val mutable suppressed = false

    (* Set in the body of a [[@tail_mod_cons]] binding. TMC rewrites a call
       in a constructor argument of a tail expression, and an out-edge wrap
       leaves it no call to rewrite. The result is warning 71, an error in
       dune's dev profile, or else a function that silently consumes stack. *)
    val mutable in_tmc_body = false

    method private binding traverse (binding : value_binding) =
      let outer = in_tmc_body in
      if is_tail_mod_cons binding.pvb_attributes then in_tmc_body <- true;
      let pvb_expr = traverse binding.pvb_expr in
      in_tmc_body <- outer;
      { binding with pvb_expr }

    method! expression e =
      if suppressed then e
      else
        let rec traverse position e =
          if is_off e.pexp_attributes then e
          else
            let loc = e.pexp_loc and attrs = e.pexp_attributes in
            match e.pexp_desc with
            | Pexp_apply
                ( (([%expr ( |> )] | [%expr ( |. )]) as pipe),
                  [ (l, lhs); (l', rhs) ] ) ->
                let lhs' = traverse (Before rhs) lhs in
                let rhs' = traverse Observed rhs in
                let apply =
                  Exp.apply ~loc ~attrs pipe [ (l, lhs'); (l', rhs') ]
                in
                if calls_never_returning e then apply
                else out_edge st position ~callee:(head_callee rhs) apply
            | Pexp_apply
                (([%expr ( || )] | [%expr ( or )]), [ (_, left); (_, right) ])
              ->
                (* [(v; true)], where the visit [v] counts the times
                   [operand] was true. *)
                let was_true operand =
                  mark st ~anchor:operand ~at_end:true ~extent:operand.pexp_loc
                    ~post:false [%expr true]
                in
                let left_true = was_true left in
                let right' =
                  match right.pexp_desc with
                  | Pexp_apply (([%expr ( || )] | [%expr ( or )]), _) ->
                      traverse (inherited position) right
                  | _ when calls_never_returning right ->
                      traverse (inherited position) right
                  | _ when is_tail position && holds_tail_call right ->
                      traverse Tail right
                  | _ ->
                      let right' = traverse Nontail right in
                      let right_true = was_true right in
                      [%expr if [%e right'] then [%e right_true] else false]
                in
                let left' = traverse Nontail left in
                [%expr if [%e left'] then [%e left_true] else [%e right']]
            | Pexp_apply (fn, args) ->
                let args =
                  match (fn, args) with
                  | ([%expr ( && )] | [%expr ( & )]), [ (l, left); (l', right) ]
                    ->
                      (* [right] runs only when [left] was true, and an entry
                         never takes it out of tail position. *)
                      let left = traverse Nontail left in
                      let right = traverse (inherited position) right in
                      [ (l, left); (l', entry st right) ]
                  | ( [%expr ( @@ )],
                      [ (l, ({ pexp_desc = Pexp_apply _; _ } as f)); (l', x) ] )
                    ->
                      let f = traverse Observed f in
                      [ (l, f); (l', traverse Nontail x) ]
                  | _ -> List.map (fun (l, x) -> (l, traverse Nontail x)) args
                in
                (* A [new] or a method call as the callee returns into the
                   application, whose out-edge observes it. *)
                let fn' =
                  match fn.pexp_desc with
                  | Pexp_new _ -> fn
                  | Pexp_send _ -> traverse Observed fn
                  | _ -> traverse Nontail fn
                in
                let apply = Exp.apply ~loc ~attrs fn' args in
                (* An application whose every argument is labelled may be
                   partial, and its closure says nothing of the call; the
                   parsetree shows only the labels. *)
                let all_labelled =
                  List.for_all
                    (function
                      | Nolabel, _ -> false
                      | (Labelled _ | Optional _), _ -> true)
                    args
                in
                if
                  in_tmc_body || all_labelled || is_trivial_function fn
                  || calls_never_returning e
                then apply
                else
                  let callee =
                    match (fn, args) with
                    | [%expr ( @@ )], [ (_, f); _ ] -> f
                    | _ -> fn
                  in
                  out_edge st position ~callee apply
            | Pexp_send (obj, meth) ->
                let send = Exp.send ~loc ~attrs (traverse Nontail obj) meth in
                if in_tmc_body then send
                else out_edge st position ~callee:send send
            | Pexp_new _ -> out_edge st position ~callee:e e
            | Pexp_assert [%expr false] -> e
            | Pexp_assert inner ->
                let assertion =
                  Exp.assert_ ~loc ~attrs (traverse Nontail inner)
                in
                mark st ~anchor:inner ~at_end:false ~extent:loc ~post:true
                  assertion
            | Pexp_function (params, constraint_, body) ->
                let params = List.map param params in
                (* Only the leaf body of a curried chain is a block; a
                   constraint or a coercion on it stays around the visit. *)
                let rec leaf body =
                  match body.pexp_desc with
                  | Pexp_function _ -> body
                  | Pexp_constraint (inner, t) ->
                      { body with pexp_desc = Pexp_constraint (leaf inner, t) }
                  | Pexp_coerce (inner, t, t') ->
                      { body with pexp_desc = Pexp_coerce (leaf inner, t, t') }
                  | _ -> entry st body
                in
                let body =
                  match body with
                  | Pfunction_body body ->
                      Pfunction_body (leaf (traverse Tail body))
                  | Pfunction_cases (cases, cases_loc, cases_attrs) ->
                      Pfunction_cases (arms Tail cases, cases_loc, cases_attrs)
                in
                { e with pexp_desc = Pexp_function (params, constraint_, body) }
            | Pexp_match (scrutinee, cases) ->
                let cases = arms (inherited position) cases in
                Exp.match_ ~loc ~attrs (traverse Observed scrutinee) cases
            | Pexp_try (body, cases) ->
                let cases = arms (inherited position) cases in
                Exp.try_ ~loc ~attrs (traverse Nontail body) cases
            | Pexp_ifthenelse (cond, then_, else_) ->
                let cond = traverse Observed cond in
                let then_ = traverse (inherited position) then_ in
                let else_ =
                  Option.map
                    (fun else_ ->
                      entry st (traverse (inherited position) else_))
                    else_
                in
                Exp.ifthenelse ~loc ~attrs cond (entry st then_) else_
            | Pexp_while (cond, body) ->
                let cond = traverse Nontail cond in
                Exp.while_ ~loc ~attrs cond (entry st (traverse Nontail body))
            | Pexp_for (pat, init, bound, direction, body) ->
                let init = traverse Nontail init in
                let bound = traverse Nontail bound in
                Exp.for_ ~loc ~attrs pat init bound direction
                  (entry st (traverse Nontail body))
            | Pexp_lazy body ->
                let body = traverse Tail body in
                Exp.lazy_ ~loc ~attrs
                  (if is_trivial_syntactic_value body then body
                   else entry st body)
            | Pexp_poly (body, t) ->
                let body = traverse Tail body in
                let body =
                  match body.pexp_desc with
                  | Pexp_function _ -> body
                  | _ -> entry st body
                in
                Exp.poly ~loc ~attrs body t
            | Pexp_letop { let_; ands; body } ->
                let operand op =
                  { op with pbop_exp = traverse Nontail op.pbop_exp }
                in
                let let_ = operand let_ in
                let ands = List.map operand ands in
                Exp.letop ~loc ~attrs let_ ands (entry st (traverse Tail body))
            | Pexp_let (rec_flag, bindings, body) ->
                let bound =
                  match bindings with [ _ ] -> Before body | _ -> Nontail
                in
                let bindings =
                  List.map (self#binding (traverse bound)) bindings
                in
                Exp.let_ ~loc ~attrs rec_flag bindings
                  (traverse (inherited position) body)
            | Pexp_sequence (first, second) ->
                let second = traverse (inherited position) second in
                (* After [if c then e], reaching the rest is informative:
                   the point of [e] does not count when [c] is false. *)
                let second =
                  match first.pexp_desc with
                  | Pexp_ifthenelse (_, _, None) -> entry st second
                  | _ -> second
                in
                Exp.sequence ~loc ~attrs (traverse (Before second) first) second
            | Pexp_ident _ | Pexp_constant _ | Pexp_extension _
            | Pexp_unreachable ->
                e
            | Pexp_tuple es ->
                Exp.tuple ~loc ~attrs (List.map (traverse Nontail) es)
            | Pexp_construct (c, arg) ->
                Exp.construct ~loc ~attrs c (Option.map (traverse Nontail) arg)
            | Pexp_variant (c, arg) ->
                Exp.variant ~loc ~attrs c (Option.map (traverse Nontail) arg)
            | Pexp_record (fields, base) ->
                let fields =
                  List.map (fun (f, x) -> (f, traverse Nontail x)) fields
                in
                Exp.record ~loc ~attrs fields
                  (Option.map (traverse Nontail) base)
            | Pexp_field (record, f) ->
                Exp.field ~loc ~attrs (traverse Nontail record) f
            | Pexp_setfield (record, f, value) ->
                let record = traverse Nontail record in
                Exp.setfield ~loc ~attrs record f (traverse Nontail value)
            | Pexp_array es ->
                Exp.array ~loc ~attrs (List.map (traverse Nontail) es)
            | Pexp_constraint (inner, t) ->
                Exp.constraint_ ~loc ~attrs
                  (traverse (inherited position) inner)
                  t
            | Pexp_coerce (inner, t, t') ->
                Exp.coerce ~loc ~attrs
                  (traverse (inherited position) inner)
                  t t'
            | Pexp_setinstvar (f, value) ->
                Exp.setinstvar ~loc ~attrs f (traverse Nontail value)
            | Pexp_override fields ->
                Exp.override ~loc ~attrs
                  (List.map (fun (f, x) -> (f, traverse Nontail x)) fields)
            | Pexp_letmodule (m, module_expr, body) ->
                let module_expr = self#module_expr module_expr in
                Exp.letmodule ~loc ~attrs m module_expr
                  (traverse (inherited position) body)
            | Pexp_letexception (c, body) ->
                Exp.letexception ~loc ~attrs c
                  (traverse (inherited position) body)
            | Pexp_open (decl, body) ->
                let decl = self#open_declaration decl in
                Exp.open_ ~loc ~attrs decl (traverse (inherited position) body)
            | Pexp_newtype (t, body) ->
                Exp.newtype ~loc ~attrs t (traverse (inherited position) body)
            | Pexp_object c -> Exp.object_ ~loc ~attrs (self#class_structure c)
            | Pexp_pack m -> Exp.pack ~loc ~attrs (self#module_expr m)
        (* The default of an optional argument runs only when the caller
           omits the argument, so it is a block. *)
        and param p =
          match p.pparam_desc with
          | Pparam_val (label, Some default, pat) ->
              let default = entry st (traverse Nontail default) in
              { p with pparam_desc = Pparam_val (label, Some default, pat) }
          | Pparam_val (_, None, _) | Pparam_newtype _ -> p
        and arms position cases =
          let case c =
            let pc_guard = Option.map (traverse Nontail) c.pc_guard in
            { c with pc_guard; pc_rhs = traverse position c.pc_rhs }
          in
          List.map (arm st) (List.map case cases)
        in
        traverse Nontail e

    (* The optional-argument defaults of a class, its concrete method bodies
       and its initializers are blocks. *)
    method! class_expr ce =
      if suppressed then ce
      else
        let ce = super#class_expr ce in
        match ce.pcl_desc with
        | Pcl_fun (label, default, pat, body) ->
            let default = Option.map (entry st) default in
            { ce with pcl_desc = Pcl_fun (label, default, pat, body) }
        | _ -> ce

    method! class_field cf =
      if suppressed then cf
      else
        let cf = super#class_field cf in
        match cf.pcf_desc with
        | Pcf_method (name, private_, Cfk_concrete (override, body)) ->
            let body = Cfk_concrete (override, entry st body) in
            { cf with pcf_desc = Pcf_method (name, private_, body) }
        | Pcf_initializer body ->
            { cf with pcf_desc = Pcf_initializer (entry st body) }
        | _ -> cf

    method! module_binding mb =
      if is_off mb.pmb_attributes then mb else super#module_binding mb

    method! structure_item si =
      match si.pstr_desc with
      | Pstr_attribute attribute ->
          let loc = attribute.attr_loc in
          (match coverage_attribute attribute with
          | `None -> ()
          | `Off ->
              if suppressed then
                Location.raise_errorf ~loc "Coverage is already off.";
              suppressed <- true
          | `On ->
              if not suppressed then
                Location.raise_errorf ~loc "Coverage is already on.";
              suppressed <- false
          | `Exclude_file -> err_misplaced ~loc "exclude_file");
          si
      | Pstr_value (rec_flag, bindings) when not suppressed ->
          let binding b =
            if is_off b.pvb_attributes then b
            else self#binding self#expression b
          in
          {
            si with
            pstr_desc = Pstr_value (rec_flag, List.map binding bindings);
          }
      | _ -> super#structure_item si

    method! structure items =
      let outer = suppressed in
      let items =
        List.map
          (fun si -> if is_test_item si then si else self#structure_item si)
          items
      in
      suppressed <- outer;
      items

    method! extension x = x
    method! attribute x = x
  end

(* Generated module *)

(* Each compilation unit calls the visit functions of its own module, named
   after the file, since a bare binding could be shadowed by a later [open]
   and a file that includes another would collide with it.
   ppx/mutate/instrument.ml mangles the same way under its own prefix. *)
let module_name file =
  let b = Buffer.create (String.length file + 16) in
  Buffer.add_string b "Windtrap_cov___";
  String.iter
    (function
      | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_') as c -> Buffer.add_char b c
      | _ -> Buffer.add_string b "___")
    file;
  Buffer.contents b

(* The stop comments hide the module from odoc. *)
let generated_module st ~file =
  let loc = { (Location.in_file file) with loc_ghost = true } in
  let name = module_name file in
  (* Every field is qualified, since a bare one would resolve by
     type-directed disambiguation (warning 42). *)
  let point p =
    [%expr
      {
        Windtrap_runtime.Coverage.start_ofs = [%e eint ~loc p.start_ofs];
        Windtrap_runtime.Coverage.end_ofs = [%e eint ~loc p.end_ofs];
      }]
  in
  let visit =
    [%stri
      let ___windtrap_visit___ =
        let counts = Array.make [%e eint ~loc st.count] 0 in
        Windtrap_runtime.Coverage.register ~file:[%e estring ~loc file]
          ~points:[%e pexp_array ~loc (List.rev_map point st.rev_points)]
          ~counts;
        fun index -> Windtrap_runtime.Coverage.visit counts index]
  in
  let post_visit =
    [%stri
      let ___windtrap_post_visit___ point_index result =
        ___windtrap_visit___ point_index;
        result]
  in
  let items = if st.uses_post then [ visit; post_visit ] else [ visit ] in
  let stop_comment = [%stri [@@@ocaml.text "/*"]] in
  [
    stop_comment;
    pstr_module ~loc
      (module_binding ~loc ~name:{ txt = Some name; loc }
         ~expr:(pmod_structure ~loc items));
    pstr_open ~loc
      (open_infos ~loc ~override:Fresh
         ~expr:(pmod_ident ~loc { txt = Lident name; loc }));
    stop_comment;
  ]

(* Rewriting *)

(* Toplevel phrases and findlib's scripts; ppx/mutate/instrument.ml skips
   the same inputs. *)
let is_ignored file =
  List.mem file [ "//toplevel//"; "(stdin)" ]
  || List.mem (Filename.basename file) [ ".ocamlinit"; "topfind" ]

let transform_impl_file ctxt ast =
  let file = Expansion_context.Base.input_name ctxt in
  if is_ignored file || List.exists excludes_file ast then ast
  else
    let st = { rev_points = []; count = 0; uses_post = false } in
    let instrumented = (new instrumenter st)#structure ast in
    if st.count = 0 then ast else generated_module st ~file @ instrumented
