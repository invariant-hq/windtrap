(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   Portions adapted from Bisect_ppx (MIT license).
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The expression-grade instrumenter (RFC Laws 13-15; Law 14 as amended
   2026-07-28).

   The instrumented population is v1's Bisect-derived one: block entries
   (function leaf bodies, match/try/function arms and guards, if branches,
   while/for bodies, lazy and letop bodies, class bodies) *and* application
   out-edges - a point that fires only when an application *returns*, so an
   expression that raises instead of returning reports uncovered - plus
   [&&]/[||] condition arms.

   Two insertion shapes exist:

   - entry sequencing, [___windtrap_visit___ i; e] - cannot change
     tail-call status, lazy compilation, or evaluation order;
   - post-visit wrapping, [___windtrap_post_visit___ i e] - evaluates [e]
     first and visits only if it returned. Post-visit wrapping is what
     *can* alter tail-call status if mishandled, so [traverse] threads
     [is_in_tail_position] exactly as the donor does and never wraps an
     application in tail position; the mandatory semantics-preservation
     suite (test/coverage_ppx/semantics) pins this. The [successor]
     threading attributes an out-edge to the expression control flows into
     next when one is known, and suppresses redundant wraps ([`Redundant])
     where an enclosing form already observes the edge.

   Each point carries a byte extent for report painting. The semantics are
   mixed by design: entry points carry their block's extent (an arm's spans
   the whole arm, pattern start to body end); out-edge points carry the
   application's extent, so a raising call paints the call itself. Point
   *identity* is the donor's: a single attribution offset (start, or end-1
   under [at_end]) - two marks at one offset share a point, first extent
   wins. The table is emitted in the per-file initialization module that
   [transform_impl_file] prepends. *)

open Ppxlib
module Exp = Ast_helper.Exp
module Cl = Ast_helper.Cl
module Cf = Ast_helper.Cf

(* ── The [coverage] attributes (Bisect_ppx's spelling) ────────────────── *)

let recognize_coverage_attribute { attr_name; attr_payload; attr_loc } =
  if not (String.equal attr_name.txt "coverage") then `None
  else
    match attr_payload with
    | PStr
        [
          {
            pstr_desc =
              Pstr_eval
                ({ pexp_desc = Pexp_ident { txt = Lident payload; _ }; _ }, _);
            _;
          };
        ] -> (
        match payload with
        | "off" -> `Off
        | "on" -> `On
        | "exclude_file" -> `Exclude_file
        | _ ->
            Location.raise_errorf ~loc:attr_loc
              "Bad payload in coverage attribute.")
    | _ ->
        Location.raise_errorf ~loc:attr_loc "Bad payload in coverage attribute."

let has_off_attribute attributes =
  (* Fold rather than short-circuit so every attribute is error-checked. *)
  List.fold_left
    (fun found attribute ->
      match recognize_coverage_attribute attribute with
      | `None -> found
      | `Off -> true
      | `On ->
          Location.raise_errorf ~loc:attribute.attr_loc
            "coverage on is not allowed here."
      | `Exclude_file ->
          Location.raise_errorf ~loc:attribute.attr_loc
            "coverage exclude_file is not allowed here.")
    false attributes

let has_exclude_file_attribute structure =
  List.exists
    (function
      | { pstr_desc = Pstr_attribute attribute; _ } ->
          recognize_coverage_attribute attribute = `Exclude_file
      | _ -> false)
    structure

(* ── Points ───────────────────────────────────────────────────────────── *)

(* [key] is the donor's point identity - one byte offset inside the
   attribution expression; [start_ofs]/[end_ofs] the extent the report
   paints. [uses_post] records whether any out-edge wrap was emitted, so
   the initialization module only binds [___windtrap_post_visit___] when
   the file references it. *)
type point = { key : int; start_ofs : int; end_ofs : int }

type state = {
  mutable rev_points : point list; (* most recently allocated first *)
  mutable count : int;
  mutable uses_post : bool;
}

let create_state () = { rev_points = []; count = 0; uses_post = false }

(* Donor identity: reuse the existing point at [key] (its first-recorded
   extent wins); allocate otherwise. Linear search over the (small)
   per-file table, exactly as the donor. *)
let point_index st ~key ~start_ofs ~end_ofs =
  let rec find index = function
    | p :: _ when p.key = key -> index
    | _ :: rest -> find (index - 1) rest
    | [] ->
        let index = st.count in
        st.rev_points <- { key; start_ofs; end_ofs } :: st.rev_points;
        st.count <- st.count + 1;
        index
  in
  find (st.count - 1) st.rev_points

(* ── Mark insertion ───────────────────────────────────────────────────── *)

(* [instrument_expr st e] wraps [e] with a visit of a fresh (or shared)
   point:

   - not [post]: the entry sequence [___windtrap_visit___ i; e];
   - [post]: [___windtrap_post_visit___ i e] - [e] evaluates first, the
     visit fires only if it returned (the out-edge). Callers must never
     use [post] in tail position.

   The point's attribution location is [use_loc_of]'s (which also carries
   the [@coverage off] check), else [override_loc], else [e]'s own; its key
   offset is that location's start, or end-1 under [at_end]; its extent is
   [extent] when given, the attribution location otherwise. Ghost
   attribution locations (generated code) are not instrumented. The
   wrapper node carries [e]'s own location, so a later mark of the wrapped
   node (arm marking after out-edge wrapping, say) still keys and paints
   the original source. *)
let instrument_expr st ?override_loc ?use_loc_of ?(at_end = false)
    ?(post = false) ?extent e =
  let attr_holder = match use_loc_of with Some e' -> e' | None -> e in
  let point_loc =
    match use_loc_of with
    | Some e' -> e'.pexp_loc
    | None -> ( match override_loc with Some l -> l | None -> e.pexp_loc)
  in
  if point_loc.loc_ghost || has_off_attribute attr_holder.pexp_attributes then e
  else begin
    let key =
      if at_end then point_loc.loc_end.pos_cnum - 1
      else point_loc.loc_start.pos_cnum
    in
    let extent = match extent with Some l -> l | None -> point_loc in
    let index =
      point_index st ~key ~start_ofs:extent.loc_start.pos_cnum
        ~end_ofs:extent.loc_end.pos_cnum
    in
    let loc = e.pexp_loc in
    let index = Ast_builder.Default.eint ~loc index in
    if post then begin
      st.uses_post <- true;
      [%expr ___windtrap_post_visit___ [%e index] [%e e]]
    end
    else
      [%expr
        ___windtrap_visit___ [%e index];
        [%e e]]
  end

(* [instrument_cases st cases] gives each arm an entry point on its
   right-hand side and, when present, one on its guard (a guard runs
   whenever the pattern matches, whether or not the arm is then taken).
   The right-hand side's point is widened to the whole arm - pattern start
   to body end - so an untested arm's report paints its [| Pattern ->]
   line, not just the body. Arms that cannot be meaningfully entered -
   [assert false], refutation cases [.] - and [[@coverage off]] arms are
   left unmarked. Applied after traversal, which preserves every arm's
   root location. *)
let case_should_not_be_instrumented case =
  match case.pc_rhs with
  | [%expr assert false] -> true
  | { pexp_desc = Pexp_unreachable; _ } -> true
  | { pexp_attributes; _ } -> has_off_attribute pexp_attributes

let instrument_cases st cases =
  List.map
    (fun case ->
      if case_should_not_be_instrumented case then case
      else begin
        let arm_extent =
          let pat = case.pc_lhs.ppat_loc and rhs = case.pc_rhs.pexp_loc in
          if pat.loc_ghost || pat.loc_start.pos_cnum > rhs.loc_start.pos_cnum
          then rhs
          else { rhs with loc_start = pat.loc_start }
        in
        let pc_guard = Option.map (instrument_expr st) case.pc_guard in
        let pc_rhs = instrument_expr st ~extent:arm_extent case.pc_rhs in
        { case with pc_guard; pc_rhs }
      end)
    cases

(* ── Guards (Law 13) ──────────────────────────────────────────────────── *)

(* [lazy] applied to a trivial syntactic value compiles as already forced;
   a visit under such a [lazy] would turn the value into a thunk and change
   the compilation of the [lazy] - so it is not instrumented. *)
let rec is_trivial_syntactic_value e =
  match e.pexp_desc with
  | Pexp_function _ | Pexp_poly _ | Pexp_ident _ | Pexp_constant _
  | Pexp_construct (_, None) ->
      true
  | Pexp_constraint (inner, _) | Pexp_coerce (inner, _, _) ->
      is_trivial_syntactic_value inner
  | _ -> false

(* Applications of these never carry an out-edge point: they are
   primitives that cannot fail interestingly (or, for [raise] and friends,
   whose whole point is not returning), and wrapping every arithmetic
   operator would double the table for no signal. The donor's list,
   verbatim. *)
let is_trivial_function = function
  | [%expr ( && )]
  | [%expr ( & )]
  | [%expr not]
  | [%expr ( = )]
  | [%expr ( <> )]
  | [%expr ( < )]
  | [%expr ( <= )]
  | [%expr ( > )]
  | [%expr ( >= )]
  | [%expr ( == )]
  | [%expr ( != )]
  | [%expr ref]
  | [%expr ( ! )]
  | [%expr ( := )]
  | [%expr ( @ )]
  | [%expr ( ^ )]
  | [%expr ( + )]
  | [%expr ( - )]
  | [%expr ( * )]
  | [%expr ( / )]
  | [%expr ( +. )]
  | [%expr ( -. )]
  | [%expr ( *. )]
  | [%expr ( /. )]
  | [%expr ( mod )]
  | [%expr ( land )]
  | [%expr ( lor )]
  | [%expr ( lxor )]
  | [%expr ( lsl )]
  | [%expr ( lsr )]
  | [%expr ( asr )]
  | [%expr raise]
  | [%expr raise_notrace]
  | [%expr failwith]
  | [%expr ignore]
  | [%expr Sys.opaque_identity]
  | [%expr Obj.magic]
  | [%expr ( ## )] ->
      true
  | _ -> false

(* ── Traversal ────────────────────────────────────────────────────────── *)

(* [traverse] is the donor's engine: a manual walk over the whole
   expression language that threads [is_in_tail_position] (never post-wrap
   a tail application) and [successor] (attribute an out-edge to the
   expression control reaches next; [`Redundant] when an enclosing form
   already observes the edge). Marks are inserted bottom-up: children are
   traversed first, then the current node's slots are wrapped - generated
   wrappers are never re-traversed. Extension and attribute payloads are
   left entirely untouched. *)
class instrumenter st =
  let instrument_expr ?override_loc ?use_loc_of ?at_end ?post ?extent e =
    instrument_expr st ?override_loc ?use_loc_of ?at_end ?post ?extent e
  in
  let instrument_cases cases = instrument_cases st cases in
  object (self)
    inherit Ast_traverse.map as super

    (* Set by [[@@@coverage off]], cleared by [[@@@coverage on]]; nested
       structures inherit the flag and restore it on exit. *)
    val mutable suppressed = false

    method! expression e =
      (* The [suppressed] check matters for expressions reached outside the
         [structure_item] dispatch below - module expressions such as
         [(val ...)] inside a [[@@@coverage off]] region. *)
      if suppressed then e
      else begin
        let rec traverse ?(successor = `None) ~is_in_tail_position e =
          let attrs = e.pexp_attributes in
          if has_off_attribute attrs then e
          else begin
            let loc = e.pexp_loc in
            match e.pexp_desc with
            (* Expressions that invoke arbitrary code, and may not
               terminate: their out-edges are points. *)
            | Pexp_apply
                ( (([%expr ( |> )] | [%expr ( |. )]) as operator),
                  [ (l, lhs); (l', rhs) ] ) ->
                let lhs_traversed =
                  traverse ~successor:(`Expression rhs)
                    ~is_in_tail_position:false lhs
                in
                let rhs_traversed =
                  traverse ~successor:`Redundant ~is_in_tail_position:false rhs
                in
                let apply =
                  Exp.apply ~loc ~attrs operator
                    [ (l, lhs_traversed); (l', rhs_traversed) ]
                in
                if is_in_tail_position then apply
                else begin
                  match successor with
                  | `None ->
                      let rec head_callee e' =
                        match e'.pexp_desc with
                        | Pexp_apply (e'', _) ->
                            if has_off_attribute e'.pexp_attributes then e'
                            else head_callee e''
                        | _ -> e'
                      in
                      instrument_expr ~use_loc_of:(head_callee rhs) ~at_end:true
                        ~post:true ~extent:loc apply
                  | `Redundant -> apply
                  | `Expression succ ->
                      instrument_expr ~use_loc_of:succ ~post:true ~extent:loc
                        apply
                end
            | Pexp_apply
                (([%expr ( || )] | [%expr ( or )]), [ (_l, left); (_l', right) ])
              ->
                (* [a || b] becomes [if a then (mark; true) else (if b then
                   (mark; true) else false)]: each condition arm is a
                   point. A right arm that is a non-trivial application in
                   tail position is left as [else b] instead - marking it
                   would take the call out of tail position (its point is
                   given up, not its tail call). *)
                let left_mark =
                  instrument_expr ~use_loc_of:left ~at_end:true [%expr true]
                in
                let right_new =
                  match right.pexp_desc with
                  | Pexp_apply (([%expr ( || )] | [%expr ( or )]), _) ->
                      traverse ~is_in_tail_position right
                  | Pexp_apply (callee, _)
                    when is_in_tail_position && not (is_trivial_function callee)
                    ->
                      traverse ~is_in_tail_position:true right
                  | (Pexp_send _ | Pexp_new _) when is_in_tail_position ->
                      traverse ~is_in_tail_position:true right
                  | _ ->
                      let condition =
                        traverse ~is_in_tail_position:false right
                      in
                      [%expr
                        if [%e condition] then
                          [%e
                            instrument_expr ~use_loc_of:right ~at_end:true
                              [%expr true]]
                        else false]
                in
                let left_new = traverse ~is_in_tail_position:false left in
                [%expr if [%e left_new] then [%e left_mark] else [%e right_new]]
            | Pexp_apply (fn, arguments) ->
                let arguments =
                  match (fn, arguments) with
                  | ([%expr ( && )] | [%expr ( & )]), [ (ll, el); (lr, er) ] ->
                      (* [a && b] evaluates [b] only when [a] is true: [b]'s
                         entry is the condition-arm point. An entry sequence
                         never breaks [b]'s tail position. *)
                      let el_new = traverse ~is_in_tail_position:false el in
                      let er_new = traverse ~is_in_tail_position er in
                      [ (ll, el_new); (lr, instrument_expr er_new) ]
                  | ( [%expr ( @@ )],
                      [
                        (ll, ({ pexp_desc = Pexp_apply _; _ } as el)); (lr, er);
                      ] ) ->
                      let el_new =
                        traverse ~successor:`Redundant
                          ~is_in_tail_position:false el
                      in
                      let er_new = traverse ~is_in_tail_position:false er in
                      [ (ll, el_new); (lr, er_new) ]
                  | _ ->
                      List.map
                        (fun (label, arg) ->
                          (label, traverse ~is_in_tail_position:false arg))
                        arguments
                in
                let fn_new =
                  match fn.pexp_desc with
                  | Pexp_new _ -> fn
                  | Pexp_send _ ->
                      traverse ~successor:`Redundant ~is_in_tail_position:false
                        fn
                  | _ -> traverse ~is_in_tail_position:false fn
                in
                let apply = Exp.apply ~loc ~attrs fn_new arguments in
                let all_arguments_labeled =
                  List.for_all (fun (label, _) -> label <> Nolabel) arguments
                in
                if is_in_tail_position || all_arguments_labeled then apply
                else if is_trivial_function fn then apply
                else begin
                  match successor with
                  | `None ->
                      let use_loc_of =
                        match (fn, arguments) with
                        | [%expr ( @@ )], [ (_, e'); _ ] -> e'
                        | _ -> fn
                      in
                      instrument_expr ~use_loc_of ~at_end:true ~post:true
                        ~extent:loc apply
                  | `Redundant -> apply
                  | `Expression succ ->
                      instrument_expr ~use_loc_of:succ ~at_end:false ~post:true
                        ~extent:loc apply
                end
            | Pexp_send (obj, meth) ->
                let obj_new = traverse ~is_in_tail_position:false obj in
                let apply = Exp.send ~loc ~attrs obj_new meth in
                if is_in_tail_position then apply
                else begin
                  match successor with
                  | `None -> instrument_expr ~at_end:true ~post:true apply
                  | `Redundant -> apply
                  | `Expression succ ->
                      instrument_expr ~use_loc_of:succ ~post:true ~extent:loc
                        apply
                end
            | Pexp_new _ ->
                if is_in_tail_position then e
                else begin
                  match successor with
                  | `None -> instrument_expr ~at_end:true ~post:true e
                  | `Redundant -> e
                  | `Expression succ ->
                      instrument_expr ~use_loc_of:succ ~post:true ~extent:loc e
                end
            | Pexp_assert [%expr false] -> e
            | Pexp_assert inner ->
                let inner_new = traverse ~is_in_tail_position:false inner in
                instrument_expr ~use_loc_of:inner ~post:true ~extent:loc
                  (Exp.assert_ ~loc ~attrs inner_new)
            (* Expressions that have subexpressions that might not get
               visited: their blocks are entry points. *)
            | Pexp_function (params, constraint_, Pfunction_body body_expr) ->
                let body_expr = traverse ~is_in_tail_position:true body_expr in
                (* Only the leaf body of a curried chain is a block:
                   entering an intermediate body allocates the next closure
                   and nothing else. Constraints and coercions are
                   transparent - the point lands inside. *)
                let rec mark_leaf body =
                  match body.pexp_desc with
                  | Pexp_function _ -> body
                  | Pexp_constraint (inner, t) ->
                      {
                        body with
                        pexp_desc = Pexp_constraint (mark_leaf inner, t);
                      }
                  | Pexp_coerce (inner, t, t') ->
                      {
                        body with
                        pexp_desc = Pexp_coerce (mark_leaf inner, t, t');
                      }
                  | _ -> if params <> [] then instrument_expr body else body
                in
                {
                  e with
                  pexp_desc =
                    Pexp_function
                      (params, constraint_, Pfunction_body (mark_leaf body_expr));
                }
            | Pexp_function
                ( params,
                  constraint_,
                  Pfunction_cases (cases, cases_loc, cases_attrs) ) ->
                let cases =
                  instrument_cases
                    (traverse_cases ~is_in_tail_position:true cases)
                in
                {
                  e with
                  pexp_desc =
                    Pexp_function
                      ( params,
                        constraint_,
                        Pfunction_cases (cases, cases_loc, cases_attrs) );
                }
            | Pexp_match (scrutinee, cases) ->
                let cases =
                  instrument_cases (traverse_cases ~is_in_tail_position cases)
                in
                let scrutinee =
                  traverse ~successor:`Redundant ~is_in_tail_position:false
                    scrutinee
                in
                Exp.match_ ~loc ~attrs scrutinee cases
            | Pexp_try (body, cases) ->
                let cases =
                  instrument_cases (traverse_cases ~is_in_tail_position cases)
                in
                let body = traverse ~is_in_tail_position:false body in
                Exp.try_ ~loc ~attrs body cases
            | Pexp_ifthenelse (cond, then_, else_) ->
                let cond =
                  traverse ~successor:`Redundant ~is_in_tail_position:false cond
                in
                let then_ = traverse ~is_in_tail_position then_ in
                let else_ =
                  Option.map
                    (fun branch ->
                      instrument_expr (traverse ~is_in_tail_position branch))
                    else_
                in
                Exp.ifthenelse ~loc ~attrs cond (instrument_expr then_) else_
            | Pexp_while (cond, body) ->
                let cond = traverse ~is_in_tail_position:false cond in
                let body = traverse ~is_in_tail_position:false body in
                Exp.while_ ~loc ~attrs cond (instrument_expr body)
            | Pexp_for (pat, init, bound, direction, body) ->
                let init = traverse ~is_in_tail_position:false init in
                let bound = traverse ~is_in_tail_position:false bound in
                let body = traverse ~is_in_tail_position:false body in
                Exp.for_ ~loc ~attrs pat init bound direction
                  (instrument_expr body)
            | Pexp_lazy body ->
                let body = traverse ~is_in_tail_position:true body in
                let body =
                  if is_trivial_syntactic_value body then body
                  else instrument_expr body
                in
                Exp.lazy_ ~loc ~attrs body
            | Pexp_poly (body, t) ->
                let body = traverse ~is_in_tail_position:true body in
                let body =
                  match body.pexp_desc with
                  | Pexp_function _ -> body
                  | _ -> instrument_expr body
                in
                Exp.poly ~loc ~attrs body t
            | Pexp_letop { let_; ands; body } ->
                let traverse_binding_op op =
                  {
                    op with
                    pbop_exp = traverse ~is_in_tail_position:false op.pbop_exp;
                  }
                in
                let let_ = traverse_binding_op let_ in
                let ands = List.map traverse_binding_op ands in
                let body = traverse ~is_in_tail_position:true body in
                Exp.letop ~loc ~attrs let_ ands (instrument_expr body)
            (* Expressions that don't fit either of the above categories.
               These don't need to be instrumented. *)
            | Pexp_ident _ | Pexp_constant _ -> e
            | Pexp_let (rec_flag, bindings, body) ->
                let successor =
                  match bindings with
                  | [ _one ] -> `Expression body
                  | _ -> `None
                in
                let bindings =
                  List.map
                    (fun binding ->
                      {
                        binding with
                        pvb_expr =
                          traverse ~successor ~is_in_tail_position:false
                            binding.pvb_expr;
                      })
                    bindings
                in
                let body = traverse ~is_in_tail_position body in
                Exp.let_ ~loc ~attrs rec_flag bindings body
            | Pexp_tuple es ->
                Exp.tuple ~loc ~attrs
                  (List.map (traverse ~is_in_tail_position:false) es)
            | Pexp_construct (c, arg) ->
                Exp.construct ~loc ~attrs c
                  (Option.map (traverse ~is_in_tail_position:false) arg)
            | Pexp_variant (c, arg) ->
                Exp.variant ~loc ~attrs c
                  (Option.map (traverse ~is_in_tail_position:false) arg)
            | Pexp_record (fields, base) ->
                let fields =
                  List.map
                    (fun (f, e) -> (f, traverse ~is_in_tail_position:false e))
                    fields
                in
                Exp.record ~loc ~attrs fields
                  (Option.map (traverse ~is_in_tail_position:false) base)
            | Pexp_field (record, f) ->
                Exp.field ~loc ~attrs
                  (traverse ~is_in_tail_position:false record)
                  f
            | Pexp_setfield (record, f, value) ->
                let record = traverse ~is_in_tail_position:false record in
                let value = traverse ~is_in_tail_position:false value in
                Exp.setfield ~loc ~attrs record f value
            | Pexp_array es ->
                Exp.array ~loc ~attrs
                  (List.map (traverse ~is_in_tail_position:false) es)
            | Pexp_sequence (first, second) ->
                let second = traverse ~is_in_tail_position second in
                let second =
                  (* After [if c then e], reaching the rest of the sequence
                     is itself informative: [e]'s point does not fire when
                     the condition is false. *)
                  match first.pexp_desc with
                  | Pexp_ifthenelse (_, _, None) -> instrument_expr second
                  | _ -> second
                in
                let first =
                  traverse ~successor:(`Expression second)
                    ~is_in_tail_position:false first
                in
                Exp.sequence ~loc ~attrs first second
            | Pexp_constraint (inner, t) ->
                Exp.constraint_ ~loc ~attrs
                  (traverse ~is_in_tail_position inner)
                  t
            | Pexp_coerce (inner, t, t') ->
                Exp.coerce ~loc ~attrs
                  (traverse ~is_in_tail_position inner)
                  t t'
            | Pexp_setinstvar (f, value) ->
                Exp.setinstvar ~loc ~attrs f
                  (traverse ~is_in_tail_position:false value)
            | Pexp_override fields ->
                Exp.override ~loc ~attrs
                  (List.map
                     (fun (f, e) -> (f, traverse ~is_in_tail_position:false e))
                     fields)
            | Pexp_letmodule (m, module_expr, body) ->
                let module_expr = self#module_expr module_expr in
                let body = traverse ~is_in_tail_position body in
                Exp.letmodule ~loc ~attrs m module_expr body
            | Pexp_letexception (c, body) ->
                Exp.letexception ~loc ~attrs c
                  (traverse ~is_in_tail_position body)
            | Pexp_open (decl, body) ->
                let decl = self#open_declaration decl in
                let body = traverse ~is_in_tail_position body in
                Exp.open_ ~loc ~attrs decl body
            | Pexp_newtype (t, body) ->
                Exp.newtype ~loc ~attrs t (traverse ~is_in_tail_position body)
            (* Expressions that don't need instrumentation, and where AST
               traversal leaves the expression language. *)
            | Pexp_object c -> Exp.object_ ~loc ~attrs (self#class_structure c)
            | Pexp_pack m -> Exp.pack ~loc ~attrs (self#module_expr m)
            (* Expressions that are not recursively traversed at all. *)
            | Pexp_extension _ | Pexp_unreachable -> e
          end
        and traverse_cases ~is_in_tail_position cases =
          List.map
            (fun case ->
              let pc_guard =
                Option.map (traverse ~is_in_tail_position:false) case.pc_guard
              in
              let pc_rhs = traverse ~is_in_tail_position case.pc_rhs in
              { case with pc_guard; pc_rhs })
            cases
        in
        traverse ~is_in_tail_position:false e
      end

    (* Class bodies (Law 14): optional-argument defaults, concrete method
       bodies, and initializers are blocks; their inner expressions reach
       [expression] through the super traversal. *)
    method! class_expr ce =
      if suppressed then ce
      else begin
        let loc = ce.pcl_loc and attrs = ce.pcl_attributes in
        let ce = super#class_expr ce in
        match ce.pcl_desc with
        | Pcl_fun (l, default, p, body) ->
            Cl.fun_ ~loc ~attrs l (Option.map instrument_expr default) p body
        | _ -> ce
      end

    method! class_field cf =
      if suppressed then cf
      else begin
        let loc = cf.pcf_loc and attrs = cf.pcf_attributes in
        let cf = super#class_field cf in
        match cf.pcf_desc with
        | Pcf_method (name, private_, kind) ->
            Cf.method_ ~loc ~attrs name private_
              (match kind with
              | Cfk_virtual _ -> kind
              | Cfk_concrete (o, body) -> Cf.concrete o (instrument_expr body))
        | Pcf_initializer body ->
            Cf.initializer_ ~loc ~attrs (instrument_expr body)
        | _ -> cf
      end

    (* [[@@coverage off]] on a module binding - [module M = ... [@@coverage
       off]], plain or rec - skips the whole module, like the same attribute
       on a value binding. *)
    method! module_binding mb =
      if has_off_attribute mb.pmb_attributes then mb
      else super#module_binding mb

    method! structure_item si =
      match si.pstr_desc with
      | Pstr_attribute attribute ->
          (match recognize_coverage_attribute attribute with
          | `None -> ()
          | `Off ->
              if suppressed then
                Location.raise_errorf ~loc:attribute.attr_loc
                  "Coverage is already off.";
              suppressed <- true
          | `On ->
              if not suppressed then
                Location.raise_errorf ~loc:attribute.attr_loc
                  "Coverage is already on.";
              suppressed <- false
          | `Exclude_file ->
              Location.raise_errorf ~loc:attribute.attr_loc
                "coverage exclude_file is not allowed here.");
          si
      | Pstr_value (rec_flag, bindings) when not suppressed ->
          let bindings =
            List.map
              (fun binding ->
                if has_off_attribute binding.pvb_attributes then binding
                else
                  { binding with pvb_expr = self#expression binding.pvb_expr })
              bindings
          in
          { si with pstr_desc = Pstr_value (rec_flag, bindings) }
      | Pstr_eval (e, attrs) when not suppressed ->
          { si with pstr_desc = Pstr_eval (self#expression e, attrs) }
      | Pstr_value _ | Pstr_eval _ -> si
      | _ -> super#structure_item si

    method! structure items =
      let saved = suppressed in
      let result = super#structure items in
      suppressed <- saved;
      result

    (* Don't instrument payloads of extensions and attributes. *)
    method! extension ext = ext
    method! attribute attr = attr
  end

(* ── Per-file runtime initialization ──────────────────────────────────── *)

(* The generated preamble registers the point table and binds the visit
   functions the marks call:

     module Windtrap_cov___<mangled file> = struct
       let ___windtrap_visit___ =
         let counts = Array.make <n> 0 in
         Windtrap_coverage.register ~file:<file> ~points:<table> ~counts;
         fun index -> Windtrap_coverage.visit counts index
       let ___windtrap_post_visit___ point_index result =
         ___windtrap_visit___ point_index;
         result
     end
     open Windtrap_cov___<mangled file>

   [___windtrap_post_visit___ i e] visits after its argument [e] has been
   evaluated - the out-edge fires only if [e] returned; it is emitted only
   when the file has out-edge points. The functions live in a module
   mangled from the file name so that each compilation unit calls its own
   (an unscoped binding could be shadowed by a later [open], and two files
   could collide when one includes another). The [[@@@ocaml.text "/*"]]
   stop comments hide the generated code from odoc. *)
let runtime_initialization st ~file =
  let loc = { (Location.in_file file) with loc_ghost = true } in
  let module_name =
    let buffer = Buffer.create (String.length file + 16) in
    Buffer.add_string buffer "Windtrap_cov___";
    String.iter
      (function
        | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_') as c ->
            Buffer.add_char buffer c
        | _ -> Buffer.add_string buffer "___")
      file;
    Buffer.contents buffer
  in
  let points_table =
    Ast_builder.Default.pexp_array ~loc
      (List.rev_map
         (fun { key = _; start_ofs; end_ofs } ->
           [%expr
             {
               Windtrap_coverage.start_ofs =
                 [%e Ast_builder.Default.eint ~loc start_ofs];
               end_ofs = [%e Ast_builder.Default.eint ~loc end_ofs];
             }])
         st.rev_points)
  in
  let visit_binding =
    [%stri
      let ___windtrap_visit___ =
        let counts = Array.make [%e Ast_builder.Default.eint ~loc st.count] 0 in
        Windtrap_coverage.register
          ~file:[%e Ast_builder.Default.estring ~loc file]
          ~points:[%e points_table] ~counts;
        fun index -> Windtrap_coverage.visit counts index]
  in
  let post_visit_binding =
    [%stri
      let ___windtrap_post_visit___ point_index result =
        ___windtrap_visit___ point_index;
        result]
  in
  let bindings =
    if st.uses_post then [ visit_binding; post_visit_binding ]
    else [ visit_binding ]
  in
  let generated_module =
    Ast_helper.Str.module_ ~loc
      (Ast_helper.Mb.mk ~loc
         { txt = Some module_name; loc }
         (Ast_helper.Mod.structure ~loc bindings))
  in
  let module_open =
    Ast_helper.Str.open_ ~loc
      (Ast_helper.Opn.mk ~loc
         (Ast_helper.Mod.ident ~loc { txt = Lident module_name; loc }))
  in
  let stop_comment = [%stri [@@@ocaml.text "/*"]] in
  [ stop_comment; generated_module; module_open; stop_comment ]

(* ── Entry point ──────────────────────────────────────────────────────── *)

let always_ignore_paths = [ "//toplevel//"; "(stdin)" ]
let always_ignore_basenames = [ ".ocamlinit"; "topfind" ]

let transform_impl_file ctxt ast =
  let file = Expansion_context.Base.input_name ctxt in
  let excluded =
    List.mem file always_ignore_paths
    || List.mem (Filename.basename file) always_ignore_basenames
    || has_exclude_file_attribute ast
  in
  if excluded then ast
  else
    let st = create_state () in
    let instrumented = (new instrumenter st)#structure ast in
    if st.count = 0 then ast else runtime_initialization st ~file @ instrumented
