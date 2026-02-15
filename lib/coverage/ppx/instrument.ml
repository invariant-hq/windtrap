(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   Portions adapted from Bisect_ppx (MIT license).
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Overview

   This is the core of windtrap coverage: the instrumenter that runs on ASTs is
   defined here. The instrumenter is divided into two major pieces:

   1. The class [instrumenter] traverses ASTs. It decides where instrumentation
      should be inserted.
   2. The module [Generated_code] provides the helpers that actually insert the
      instrumentation. In other words, they insert new leaves into the AST at
      the places chosen by [instrumenter].

   The code is structured to strongly reflect this division. It is recommended
   to read this file with code folding.

   Instrumented locations are called {e points}. When the instrumentation code
   is executed, the point is {e visited}. Points appear as highlighted
   characters in coverage reports.

   All state is contained within instances of [instrumenter].

   Instances are created in [register.ml], which is the "top-level"
   side-effecting module. *)

module Parsetree = Ppxlib.Parsetree
module Location = Ppxlib.Location
module Ast_builder = Ppxlib.Ast_builder
module Longident = Ppxlib.Longident
module Pat = Ppxlib.Ast_helper.Pat
module Exp = Ppxlib.Ast_helper.Exp
module Str = Ppxlib.Ast_helper.Str
module Cl = Ppxlib.Ast_helper.Cl
module Cf = Ppxlib.Ast_helper.Cf

module Coverage_attributes : sig
  val recognize : Parsetree.attribute -> [ `None | `On | `Off | `Exclude_file ]
  val has_off_attribute : Parsetree.attributes -> bool
  val has_exclude_file_attribute : Parsetree.structure -> bool
end = struct
  let recognize { Parsetree.attr_name; attr_payload; attr_loc } =
    if attr_name.txt <> "coverage" then `None
    else
      match attr_payload with
      | Parsetree.PStr
          [ { pstr_desc =
                Pstr_eval
                  ({ pexp_desc = Pexp_ident { txt = Longident.Lident s; _ }; _ }, _);
              _
            }
          ] -> (
          match s with
          | "off" -> `Off
          | "on" -> `On
          | "exclude_file" -> `Exclude_file
          | _ ->
              Location.raise_errorf ~loc:attr_loc
                "Bad payload in coverage attribute.")
      | _ ->
          Location.raise_errorf ~loc:attr_loc
            "Bad payload in coverage attribute."

  let has_off_attribute attributes =
    (* Don't short-circuit the search, because we want to error-check all
       attributes. *)
    List.fold_left
      (fun found_off attribute ->
        match recognize attribute with
        | `None -> found_off
        | `Off -> true
        | `On ->
            Location.raise_errorf ~loc:attribute.attr_loc
              "coverage on is not allowed here."
        | `Exclude_file ->
            Location.raise_errorf ~loc:attribute.attr_loc
              "coverage exclude_file is not allowed here.")
      false attributes

  let has_exclude_file_attribute structure =
    structure
    |> List.exists (function
      | { Parsetree.pstr_desc = Pstr_attribute attribute; _ }
        when recognize attribute = `Exclude_file ->
          true
      | _ -> false)
end

module Generated_code : sig
  type points

  val init : unit -> points

  val instrument_expr :
    points ->
    ?override_loc:Location.t ->
    ?use_loc_of:Parsetree.expression ->
    ?at_end:bool ->
    ?post:bool ->
    Parsetree.expression ->
    Parsetree.expression

  val instrument_cases : points -> Parsetree.case list -> Parsetree.case list
  val runtime_initialization : points -> string -> Parsetree.structure_item list
end = struct
  type points = { mutable offsets : int list; mutable count : int }

  let init () = { offsets = []; count = 0 }

  (* Given an AST for an expression [e], replaces it by the sequence expression
     [instrumentation; e], where [instrumentation] is some code that tells
     the runtime, at runtime, that [e] has been visited. *)
  let instrument_expr points ?override_loc ?use_loc_of ?(at_end = false)
      ?(post = false) e =
    let rec outline () =
      let loc = choose_location_of_point ~override_loc ~use_loc_of e in
      if expression_should_not_be_instrumented ~point_loc:loc ~use_loc_of then e
      else
        let point_index = get_index_of_point_at_location ~point_loc:loc in
        let open Parsetree in
        if not post then
          [%expr
            ___windtrap_visit___ [%e point_index];
            [%e e]]
        else [%expr ___windtrap_post_visit___ [%e point_index] [%e e]]
    and choose_location_of_point ~override_loc ~use_loc_of e =
      match use_loc_of with
      | Some e -> Parsetree.(e.pexp_loc)
      | None -> (
          match override_loc with
          | Some override_loc -> override_loc
          | _ -> Parsetree.(e.pexp_loc))
    and expression_should_not_be_instrumented ~point_loc:loc ~use_loc_of =
      let e = match use_loc_of with Some e -> e | None -> e in
      Location.(loc.loc_ghost)
      || Coverage_attributes.has_off_attribute e.pexp_attributes
    and get_index_of_point_at_location ~point_loc:loc =
      let point_offset =
        if not at_end then Location.(Lexing.(loc.loc_start.pos_cnum))
        else Location.(Lexing.(loc.loc_end.pos_cnum - 1))
      in
      let point =
        let rec find_point points offset index offsets =
          match offsets with
          | offset' :: _ when offset' = offset -> index
          | _ :: rest -> find_point points offset (index - 1) rest
          | [] ->
              let index = points.count in
              points.offsets <- offset :: points.offsets;
              points.count <- points.count + 1;
              index
        in
        find_point points point_offset (points.count - 1) points.offsets
      in
      Ast_builder.Default.eint ~loc point
    in

    outline ()

  (* Simplified case instrumentation: instrument each case's guard and
     right-hand side without or-pattern rotation. *)
  let case_should_not_be_instrumented (case : Parsetree.case) =
    match case.pc_rhs with
    | [%expr assert false] -> true
    | { pexp_desc = Pexp_unreachable; _ } -> true
    | { pexp_attributes; _ }
      when Coverage_attributes.has_off_attribute pexp_attributes ->
        true
    | _ -> false

  let instrument_cases points (cases : Parsetree.case list) =
    List.map
      (fun (case : Parsetree.case) ->
        if case_should_not_be_instrumented case then case
        else
          match case.pc_guard with
          | None -> { case with pc_rhs = instrument_expr points case.pc_rhs }
          | Some guard ->
              {
                case with
                pc_guard = Some (instrument_expr points guard);
                pc_rhs = instrument_expr points case.pc_rhs;
              })
      cases

  let runtime_initialization points file =
    let loc = Location.in_file file in

    let mangled_module_name =
      let buffer = Buffer.create (String.length file * 2) in
      file
      |> String.iter (function
        | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_') as c ->
            Buffer.add_char buffer c
        | _ -> Buffer.add_string buffer "___");
      "Windtrap_cov___" ^ Buffer.contents buffer
    in

    let points_data =
      Ast_builder.Default.pexp_array ~loc
        (List.map
           (fun offset -> Ast_builder.Default.eint ~loc offset)
           (List.rev points.offsets))
    in
    let filename = Ast_builder.Default.estring ~loc file in

    (* ___windtrap_visit___ is a function with a reference to a point count
       array. It is called every time a point is visited.

       It is scoped in a local module, to ensure that each compilation unit
       calls its own ___windtrap_visit___ function. In particular, if
       ___windtrap_visit___ is unscoped, a later [open] can shadow it.

       To prevent this, we generate:

         module Windtrap_cov___<mangled_filename> =
         struct
           let ___windtrap_visit___ = (* ... *)
         end
         open Windtrap_cov___<mangled_filename>

       Module names are mangled to be unique per file, avoiding duplicate
       module errors when one file includes another. *)
    let generated_module =
      let visit_function =
        let open Parsetree in
        [%stri
          let ___windtrap_visit___ =
            let points = [%e points_data] in
            let (`Visit visit) =
              Windtrap_coverage.register_file ~filename:[%e filename] ~points
            in
            visit]
      in

      let post_visit =
        let open Parsetree in
        [%stri
          let ___windtrap_post_visit___ point_index result =
            ___windtrap_visit___ point_index;
            result]
      in

      let open Ppxlib.Ast_helper in
      Str.module_ ~loc
      @@ Mb.mk ~loc
           { txt = Some mangled_module_name; loc }
           (Mod.structure ~loc [ visit_function; post_visit ])
    in

    let module_open =
      let open Ppxlib.Ast_helper in
      Str.open_ ~loc @@ Opn.mk ~loc
      @@ Mod.ident ~loc { txt = Longident.parse mangled_module_name; loc }
    in

    let open Parsetree in
    let stop_comment = [%stri [@@@ocaml.text "/*"]] in

    [ stop_comment; generated_module; module_open; stop_comment ]
end

let ( >>= ) = Ppxlib.With_errors.( >>= )
let ( >>| ) = Ppxlib.With_errors.( >>| )
let collect_errors = Ppxlib.With_errors.combine_errors
let return = Ppxlib.With_errors.return

(* The actual "instrumenter" object, instrumenting expressions. *)
class instrumenter =
  let points = Generated_code.init () in
  let instrument_expr = Generated_code.instrument_expr points in
  let instrument_cases = Generated_code.instrument_cases points in

  object (self)
    inherit Ppxlib.Ast_traverse.map_with_expansion_context_and_errors as super

    method! class_expr ctxt ce =
      let loc = ce.pcl_loc in
      let attrs = ce.pcl_attributes in
      super#class_expr ctxt ce >>| fun ce ->
      match ce.pcl_desc with
      | Pcl_fun (l, e, p, ce) ->
          Cl.fun_ ~loc ~attrs l (Option.map instrument_expr e) p ce
      | _ -> ce

    method! class_field ctxt cf =
      let loc = cf.pcf_loc in
      let attrs = cf.pcf_attributes in
      super#class_field ctxt cf >>| fun cf ->
      match cf.pcf_desc with
      | Pcf_method (name, private_, cf) ->
          Cf.method_ ~loc ~attrs name private_
            (match cf with
            | Cfk_virtual _ -> cf
            | Cfk_concrete (o, e) -> Cf.concrete o (instrument_expr e))
      | Pcf_initializer e -> Cf.initializer_ ~loc ~attrs (instrument_expr e)
      | _ -> cf

    method! expression ctxt e =
      let is_trivial_function =
        Parsetree.(
          function
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
          | _ -> false)
      in

      let rec traverse ?(successor = `None) ~is_in_tail_position e =
        let attrs = e.Parsetree.pexp_attributes in
        if Coverage_attributes.has_off_attribute attrs then return e
        else begin
          let loc = e.pexp_loc in

          match e.pexp_desc with
          (* Expressions that invoke arbitrary code, and may not terminate. *)
          | Pexp_apply
              ( (([%expr ( |> )] | [%expr ( |. )]) as operator),
                [ (l, e); (l', e') ] ) ->
              traverse ~successor:(`Expression e') ~is_in_tail_position:false e
              >>= fun e_traversed ->
              traverse ~successor:`Redundant ~is_in_tail_position:false e'
              >>| fun e'_traversed ->
              let apply =
                Exp.apply ~loc ~attrs operator
                  [ (l, e_traversed); (l', e'_traversed) ]
              in
              if is_in_tail_position then apply
              else begin
                match successor with
                | `None ->
                    let rec fn e' =
                      match e'.Parsetree.pexp_desc with
                      | Pexp_apply (e'', _) ->
                          let attributes = e'.pexp_attributes in
                          if Coverage_attributes.has_off_attribute attributes
                          then e'
                          else fn e''
                      | _ -> e'
                    in
                    instrument_expr ~use_loc_of:(fn e') ~at_end:true ~post:true
                      apply
                | `Redundant -> apply
                | `Expression e ->
                    instrument_expr ~use_loc_of:e ~post:true apply
              end
          | Pexp_apply
              (([%expr ( || )] | [%expr ( or )]), [ (_l, e); (_l', e') ]) ->
              let e_mark =
                instrument_expr ~use_loc_of:e ~at_end:true [%expr true]
              in
              begin match e'.pexp_desc with
              | Pexp_apply (([%expr ( || )] | [%expr ( or )]), _) ->
                  traverse ~is_in_tail_position e'
              | Pexp_apply (e'', _)
                when is_in_tail_position && not (is_trivial_function e'') ->
                  traverse ~is_in_tail_position:true e'
              | (Pexp_send _ | Pexp_new _) when is_in_tail_position ->
                  traverse ~is_in_tail_position:true e'
              | _ ->
                  traverse ~is_in_tail_position:false e' >>| fun condition ->
                  let open Parsetree in
                  [%expr
                    if [%e condition] then
                      [%e
                        instrument_expr ~use_loc_of:e' ~at_end:true [%expr true]]
                    else false]
              end
              >>= fun e'_new ->
              let open Parsetree in
              traverse ~is_in_tail_position:false e >>| fun e_new ->
              [%expr if [%e e_new] then [%e e_mark] else [%e e'_new]]
          | Pexp_apply (e, arguments) ->
              begin match (e, arguments) with
              | ([%expr ( && )] | [%expr ( & )]), [ (ll, el); (lr, er) ] ->
                  traverse ~is_in_tail_position:false el >>= fun el_new ->
                  traverse ~is_in_tail_position er >>| fun er_new ->
                  [ (ll, el_new); (lr, instrument_expr er_new) ]
              | ( [%expr ( @@ )],
                  [ (ll, ({ pexp_desc = Pexp_apply _; _ } as el)); (lr, er) ] )
                ->
                  traverse ~successor:`Redundant ~is_in_tail_position:false el
                  >>= fun el_new ->
                  traverse ~is_in_tail_position:false er >>| fun er_new ->
                  [ (ll, el_new); (lr, er_new) ]
              | _ ->
                  arguments
                  |> List.map (fun (label, e) ->
                      traverse ~is_in_tail_position:false e >>| fun e_new ->
                      (label, e_new))
                  |> collect_errors
              end
              >>= fun arguments ->
              begin match e.pexp_desc with
              | Pexp_new _ -> return e
              | Pexp_send _ ->
                  traverse ~successor:`Redundant ~is_in_tail_position:false e
              | _ -> traverse ~is_in_tail_position:false e
              end
              >>| fun e ->
              let apply = Exp.apply ~loc ~attrs e arguments in
              let all_arguments_labeled =
                arguments
                |> List.for_all (fun (label, _) -> label <> Ppxlib.Nolabel)
              in
              if is_in_tail_position || all_arguments_labeled then apply
              else if is_trivial_function e then apply
              else begin
                match successor with
                | `None ->
                    let use_loc_of =
                      match (e, arguments) with
                      | [%expr ( @@ )], [ (_, e'); _ ] -> e'
                      | _ -> e
                    in
                    instrument_expr ~use_loc_of ~at_end:true ~post:true apply
                | `Redundant -> apply
                | `Expression e' ->
                    instrument_expr ~use_loc_of:e' ~at_end:false ~post:true
                      apply
              end
          | Pexp_send (e, m) ->
              traverse ~is_in_tail_position:false e >>| fun e_new ->
              let apply = Exp.send ~loc ~attrs e_new m in
              if is_in_tail_position then apply
              else begin
                match successor with
                | `None -> instrument_expr ~at_end:true ~post:true apply
                | `Redundant -> apply
                | `Expression e' ->
                    instrument_expr ~use_loc_of:e' ~post:true apply
              end
          | Pexp_new _ ->
              return
              @@
              if is_in_tail_position then e
              else begin
                match successor with
                | `None -> instrument_expr ~at_end:true ~post:true e
                | `Redundant -> e
                | `Expression e' -> instrument_expr ~use_loc_of:e' ~post:true e
              end
          | Pexp_assert [%expr false] -> return e
          | Pexp_assert e ->
              traverse ~is_in_tail_position:false e >>| fun e_new ->
              instrument_expr ~use_loc_of:e ~post:true (Exp.assert_ e_new)
          (* Expressions that have subexpressions that might not get visited. *)
          (* ppxlib 0.37.0: Pexp_function (params, constraint_, body)
             where body = Pfunction_body expr | Pfunction_cases (cases, loc, attrs)
             Pexp_fun no longer exists; it is represented as Pexp_function
             with a single Pparam_val parameter and Pfunction_body. *)
          | Pexp_function (params, constraint_, body) -> begin
              match body with
              | Pfunction_body body_expr ->
                  traverse ~is_in_tail_position:true body_expr
                  >>| fun body_expr ->
                  let body_expr =
                    match body_expr.pexp_desc with
                    | Pexp_function _ -> body_expr
                    | Pexp_constraint (inner, t) ->
                        {
                          body_expr with
                          pexp_desc = Pexp_constraint (instrument_expr inner, t);
                        }
                    | _ ->
                        (* Don't instrument if this is an intermediate fun param
                     (i.e. there are further params making it a chain of funs).
                     We detect "leaf" bodies by checking that there are params. *)
                        if params <> [] then instrument_expr body_expr
                        else body_expr
                  in
                  {
                    e with
                    pexp_desc =
                      Pexp_function
                        (params, constraint_, Pfunction_body body_expr);
                  }
              | Pfunction_cases (cases, cases_loc, cases_attrs) ->
                  traverse_cases ~is_in_tail_position:true cases
                  >>| fun cases_new ->
                  let cases_instrumented = instrument_cases cases_new in
                  {
                    e with
                    pexp_desc =
                      Pexp_function
                        ( params,
                          constraint_,
                          Pfunction_cases
                            (cases_instrumented, cases_loc, cases_attrs) );
                  }
            end
          | Pexp_match (e, cases) ->
              traverse_cases ~is_in_tail_position cases >>= fun cases ->
              let cases = instrument_cases cases in
              traverse ~successor:`Redundant ~is_in_tail_position:false e
              >>| fun e -> Exp.match_ ~loc ~attrs e cases
          | Pexp_try (e, cases) ->
              traverse_cases ~is_in_tail_position cases >>= fun cases ->
              let cases = instrument_cases cases in
              traverse ~is_in_tail_position:false e >>| fun e ->
              Exp.try_ ~loc ~attrs e cases
          | Pexp_ifthenelse (if_, then_, else_) ->
              traverse ~successor:`Redundant ~is_in_tail_position:false if_
              >>= fun if_ ->
              traverse ~is_in_tail_position then_ >>= fun then_ ->
              begin match else_ with
              | None -> return None
              | Some else_ ->
                  traverse ~is_in_tail_position else_ >>| fun else_ ->
                  Some (instrument_expr else_)
              end
              >>| fun else_ ->
              Exp.ifthenelse ~loc ~attrs if_ (instrument_expr then_) else_
          | Pexp_while (while_, do_) ->
              traverse ~is_in_tail_position:false while_ >>= fun while_ ->
              traverse ~is_in_tail_position:false do_ >>| fun do_ ->
              Exp.while_ ~loc ~attrs while_ (instrument_expr do_)
          | Pexp_for (v, init, to_, direction, do_) ->
              traverse ~is_in_tail_position:false init >>= fun init ->
              traverse ~is_in_tail_position:false to_ >>= fun to_ ->
              traverse ~is_in_tail_position:false do_ >>| fun do_ ->
              Exp.for_ ~loc ~attrs v init to_ direction (instrument_expr do_)
          | Pexp_lazy e ->
              let rec is_trivial_syntactic_value e =
                match e.Parsetree.pexp_desc with
                | Pexp_function _ | Pexp_poly _ | Pexp_ident _ | Pexp_constant _
                | Pexp_construct (_, None) ->
                    true
                | Pexp_constraint (e, _) | Pexp_coerce (e, _, _) ->
                    is_trivial_syntactic_value e
                | _ -> false
              in
              traverse ~is_in_tail_position:true e >>| fun e ->
              let e =
                (* lazy applied to certain syntactic values is compiled as already
                 forced. Since inserting instrumentation under such a lazy would
                 make the nested expression not a syntactic value, it would
                 change the compilation of the lazy. *)
                if is_trivial_syntactic_value e then e else instrument_expr e
              in
              Exp.lazy_ ~loc ~attrs e
          | Pexp_poly (e, t) ->
              traverse ~is_in_tail_position:true e >>| fun e ->
              let e =
                match e.pexp_desc with
                | Pexp_function _ -> e
                | _ -> instrument_expr e
              in
              Exp.poly ~loc ~attrs e t
          | Pexp_letop { let_; ands; body } ->
              let traverse_binding_op binding_op =
                traverse ~is_in_tail_position:false
                  binding_op.Parsetree.pbop_exp
                >>| fun pbop_exp -> { binding_op with Parsetree.pbop_exp }
              in
              traverse_binding_op let_ >>= fun let_ ->
              List.map traverse_binding_op ands |> collect_errors
              >>= fun ands ->
              traverse ~is_in_tail_position:true body >>| fun body ->
              Exp.letop ~loc ~attrs let_ ands (instrument_expr body)
          (* Expressions that don't fit either of the above categories. These
             don't need to be instrumented. *)
          | Pexp_ident _ | Pexp_constant _ -> return e
          | Pexp_let (rec_flag, bindings, e) ->
              let successor =
                match bindings with [ _one ] -> `Expression e | _ -> `None
              in
              bindings
              |> List.map (fun binding ->
                  traverse ~successor ~is_in_tail_position:false
                    binding.Parsetree.pvb_expr
                  >>| fun e -> Parsetree.{ binding with pvb_expr = e })
              |> collect_errors
              >>= fun bindings ->
              traverse ~is_in_tail_position e >>| fun e ->
              Exp.let_ ~loc ~attrs rec_flag bindings e
          | Pexp_tuple es ->
              List.map (traverse ~is_in_tail_position:false) es
              |> collect_errors
              >>| fun es -> Exp.tuple ~loc ~attrs es
          | Pexp_construct (c, e) ->
              begin match e with
              | None -> return None
              | Some e ->
                  traverse ~is_in_tail_position:false e >>| fun e -> Some e
              end
              >>| fun e -> Exp.construct ~loc ~attrs c e
          | Pexp_variant (c, e) ->
              begin match e with
              | None -> return None
              | Some e ->
                  traverse ~is_in_tail_position:false e >>| fun e -> Some e
              end
              >>| fun e -> Exp.variant ~loc ~attrs c e
          | Pexp_record (fields, e) ->
              fields
              |> List.map (fun (f, e) ->
                  traverse ~is_in_tail_position:false e >>| fun e -> (f, e))
              |> collect_errors
              >>= fun fields ->
              begin match e with
              | None -> return None
              | Some e ->
                  traverse ~is_in_tail_position:false e >>| fun e -> Some e
              end
              >>| fun e -> Exp.record ~loc ~attrs fields e
          | Pexp_field (e, f) ->
              traverse ~is_in_tail_position:false e >>| fun e ->
              Exp.field ~loc ~attrs e f
          | Pexp_setfield (e, f, e') ->
              traverse ~is_in_tail_position:false e >>= fun e ->
              traverse ~is_in_tail_position:false e' >>| fun e' ->
              Exp.setfield ~loc ~attrs e f e'
          | Pexp_array es ->
              List.map (traverse ~is_in_tail_position:false) es
              |> collect_errors
              >>| fun es -> Exp.array ~loc ~attrs es
          | Pexp_sequence (e, e') ->
              traverse ~is_in_tail_position e' >>= fun e' ->
              let e' =
                match e.pexp_desc with
                | Pexp_ifthenelse (_, _, None) -> instrument_expr e'
                | _ -> e'
              in
              traverse ~successor:(`Expression e') ~is_in_tail_position:false e
              >>| fun e -> Exp.sequence ~loc ~attrs e e'
          | Pexp_constraint (e, t) ->
              traverse ~is_in_tail_position e >>| fun e ->
              Exp.constraint_ ~loc ~attrs e t
          | Pexp_coerce (e, t, t') ->
              traverse ~is_in_tail_position e >>| fun e ->
              Exp.coerce ~loc ~attrs e t t'
          | Pexp_setinstvar (f, e) ->
              traverse ~is_in_tail_position:false e >>| fun e ->
              Exp.setinstvar ~loc ~attrs f e
          | Pexp_override fs ->
              fs
              |> List.map (fun (f, e) ->
                  traverse ~is_in_tail_position:false e >>| fun e -> (f, e))
              |> collect_errors
              >>| fun fs -> Exp.override ~loc ~attrs fs
          | Pexp_letmodule (m, e, e') ->
              self#module_expr ctxt e >>= fun e ->
              traverse ~is_in_tail_position e' >>| fun e' ->
              Exp.letmodule ~loc ~attrs m e e'
          | Pexp_letexception (c, e) ->
              traverse ~is_in_tail_position e >>| fun e ->
              Exp.letexception ~loc ~attrs c e
          | Pexp_open (m, e) ->
              self#open_declaration ctxt m >>= fun m ->
              traverse ~is_in_tail_position e >>| fun e ->
              Exp.open_ ~loc ~attrs m e
          | Pexp_newtype (t, e) ->
              traverse ~is_in_tail_position e >>| fun e ->
              Exp.newtype ~loc ~attrs t e
          (* Expressions that don't need instrumentation, and where AST
             traversal leaves the expression language. *)
          | Pexp_object c ->
              self#class_structure ctxt c >>| fun c -> Exp.object_ ~loc ~attrs c
          | Pexp_pack m ->
              self#module_expr ctxt m >>| fun m -> Exp.pack ~loc ~attrs m
          (* Expressions that are not recursively traversed at all. *)
          | Pexp_extension _ | Pexp_unreachable -> return e
        end
      and traverse_cases ~is_in_tail_position cases =
        cases
        |> List.map begin fun case ->
            begin match case.Parsetree.pc_guard with
            | None -> return None
            | Some guard ->
                traverse ~is_in_tail_position:false guard >>| fun guard ->
                Some guard
            end
            >>= fun pc_guard ->
            traverse ~is_in_tail_position case.pc_rhs >>| fun pc_rhs ->
            { case with pc_guard; pc_rhs }
          end
        |> collect_errors
      in

      traverse ~is_in_tail_position:false e

    (* Set to [true] upon encountering [[@@@coverage.off]], and back to
       [false] again upon encountering [[@@@coverage.on]]. *)
    val mutable structure_instrumentation_suppressed = false

    method! structure_item ctxt si =
      let loc = si.pstr_loc in

      match si.pstr_desc with
      | Pstr_value (rec_flag, bindings) ->
          if structure_instrumentation_suppressed then return si
          else
            bindings
            |> List.map begin fun (binding : Parsetree.value_binding) ->
                let do_not_instrument =
                  Coverage_attributes.has_off_attribute binding.pvb_attributes
                in
                if do_not_instrument then return binding
                else begin
                  self#expression ctxt binding.pvb_expr >>| fun e ->
                  { binding with pvb_expr = e }
                end
              end
            |> collect_errors
            >>| fun bindings -> Str.value ~loc rec_flag bindings
      | Pstr_eval (e, a) ->
          if structure_instrumentation_suppressed then return si
          else begin
            self#expression ctxt e >>| fun e -> Str.eval ~loc ~attrs:a e
          end
      | Pstr_attribute attribute ->
          let kind = Coverage_attributes.recognize attribute in
          begin match kind with
          | `None -> ()
          | `Off ->
              if structure_instrumentation_suppressed then
                Location.raise_errorf ~loc:attribute.attr_loc
                  "Coverage is already off.";
              structure_instrumentation_suppressed <- true
          | `On ->
              if not structure_instrumentation_suppressed then
                Location.raise_errorf ~loc:attribute.attr_loc
                  "Coverage is already on.";
              structure_instrumentation_suppressed <- false
          | `Exclude_file ->
              Location.raise_errorf ~loc:attribute.attr_loc
                "coverage exclude_file is not allowed here."
          end;
          return si
      | _ -> super#structure_item ctxt si

    (* Don't instrument payloads of extensions and attributes. *)
    method! extension _ e = return e
    method! attribute _ a = return a

    method! structure ctxt ast =
      let saved_structure_instrumentation_suppressed =
        structure_instrumentation_suppressed
      in
      let result = super#structure ctxt ast in
      structure_instrumentation_suppressed <-
        saved_structure_instrumentation_suppressed;
      result

    method transform_impl_file ctxt ast =
      let saved_structure_instrumentation_suppressed =
        structure_instrumentation_suppressed
      in

      let result =
        let path = Ppxlib.Expansion_context.Base.input_name ctxt in
        let file_should_not_be_instrumented =
          let always_ignore_paths = [ "//toplevel//"; "(stdin)" ] in
          let always_ignore_basenames = [ ".ocamlinit"; "topfind" ] in

          List.mem path always_ignore_paths
          || List.mem (Filename.basename path) always_ignore_basenames
          || Coverage_attributes.has_exclude_file_attribute ast
        in

        if file_should_not_be_instrumented then ast
        else begin
          let instrumented_ast, errors = super#structure ctxt ast in
          let errors =
            errors
            |> List.map (fun error ->
                Ast_builder.Default.pstr_extension
                  ~loc:(Location.Error.get_location error)
                  (Location.Error.to_extension error)
                  [])
          in
          let runtime_initialization =
            Generated_code.runtime_initialization points path
          in
          errors @ runtime_initialization @ instrumented_ast
        end
      in

      structure_instrumentation_suppressed <-
        saved_structure_instrumentation_suppressed;

      result
  end
