(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A test is a registration with the runtime at module load, and an expect
   node a [Windtrap.expect] call at the node's position; every other decision
   is the core's. A name of ppx_expect's family that is not expanded is a
   compile error, never left in place. *)

open Ppxlib
open Ast_builder.Default

(* Cookies *)

(* Dune sets [inline_tests] to ["disabled"] for a library whose
   (inline_tests) stanza is off and under a profile that disables them. *)
let enabled = ref true

let () =
  Driver.Cookies.add_simple_handler "inline_tests"
    Ast_pattern.(estring __')
    ~f:
      (Option.iter (fun (value : string loc) ->
           enabled :=
             match value.txt with
             | "enabled" -> true
             | "disabled" | "ignored" -> false
             | s ->
                 Location.raise_errorf ~loc:value.loc
                   "invalid 'inline_tests' cookie (%s), expected one of: \
                    enabled, disabled or ignored"
                   s))

let when_enabled items = if !enabled then items else []

(* Dune sets [library-name] for every library stanza it preprocesses. *)
let library_name = ref None

let () =
  Driver.Cookies.add_simple_handler "library-name"
    Ast_pattern.(estring __)
    ~f:(fun name -> library_name := name)

(* The expect family *)

let is_expect_family name =
  String.equal name "expect"
  || String.equal name "expect_exact"
  || String.equal name "expectation"
  || String.starts_with ~prefix:"expect." name
  || String.starts_with ~prefix:"expectation." name

(* What windtrap offers in place of a construct that has a counterpart. *)
let instead = function
  | "expect.unreachable" ->
      "; call Windtrap.fail at the point the body must not reach"
  | "expect.if_reached" -> "; use [%expect], which must be reached"
  | "expect.uncaught_exn" ->
      "; catch and print the exception before an [%expect]"
  | _ -> ""

let err_unsupported ~loc name =
  Location.raise_errorf ~loc "[%%%s] is not supported by ppx_windtrap%s" name
    (instead name)

(* No attribute of the family is implemented, wherever it is placed. *)
let reject_attribute attr =
  let name = attr.attr_name in
  if is_expect_family name.txt then
    Location.raise_errorf ~loc:name.loc
      "attribute %s is not supported by ppx_windtrap%s" name.txt
      (instead name.txt)

(* Names and tags *)

let test_name ~extension ~loc pat =
  match pat.ppat_desc with
  | Ppat_constant (Pconst_string (name, _, _)) -> name
  | Ppat_any -> Printf.sprintf "line_%d" loc.loc_start.pos_lnum
  | _ ->
      Location.raise_errorf ~loc
        "Expected let%%%s \"name\" = ... or let%%%s _ = ..." extension extension

let is_tags attr = String.equal attr.attr_name.txt "tags"

let tags_of attr =
  if not (is_tags attr) then []
  else
    let err () =
      Location.raise_errorf ~loc:attr.attr_loc
        "Expected [@tags \"...\"] or [@tags (\"...\", ...)]"
    in
    let tag e =
      match e.pexp_desc with
      | Pexp_constant (Pconst_string (s, _, _)) -> s
      | _ -> err ()
    in
    match attr.attr_payload with
    | PStr
        [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_tuple es; _ }, _); _ } ]
      ->
        List.map tag es
    | PStr [ { pstr_desc = Pstr_eval (e, _); _ } ] -> [ tag e ]
    | _ -> err ()

(* The name, tags and body of the test [let%EXTENSION NAME = BODY]. The tags
   are [NAME]'s, and its other attributes and the binding's are dropped. *)
let test_of_binding ~extension ~loc vb =
  List.iter reject_attribute vb.pvb_attributes;
  List.iter reject_attribute vb.pvb_pat.ppat_attributes;
  let tags = List.concat_map tags_of vb.pvb_pat.ppat_attributes in
  (test_name ~extension ~loc vb.pvb_pat, tags, vb.pvb_expr)

(* Registrations *)

(* [__POS_OF__]'s tuple for [l], the shape of [Windtrap.pos]: both columns
   count from the start line. *)
let pos_expr ~loc (l : Location.t) =
  let s = l.loc_start and e = l.loc_end in
  pexp_tuple ~loc
    [
      estring ~loc s.pos_fname;
      eint ~loc s.pos_lnum;
      eint ~loc (s.pos_cnum - s.pos_bol);
      eint ~loc (e.pos_cnum - s.pos_bol);
    ]

let tags_expr ~loc tags = elist ~loc (List.map (estring ~loc) tags)

(* [Ppx_runtime.fn args] as an item, [~library] first when dune names one. *)
let registration ~loc fn args =
  let library =
    match !library_name with
    | Some name -> [ (Labelled "library", estring ~loc name) ]
    | None -> []
  in
  let fn = evar ~loc ("Ppx_windtrap_runtime.Ppx_runtime." ^ fn) in
  [%stri let () = [%e pexp_apply ~loc fn (library @ args)]]

(* [body] out of tail position: the frame of the test's function stays on the
   stack while [body]'s last call runs, so an assertion that ends [body]
   captures its own line. The constraint types [body] against [unit] as the
   tail position did, and [raise_notrace] leaves the backtrace that [body]
   recorded as it is. *)
let out_of_tail body =
  let loc = { body.pexp_loc with loc_ghost = true } in
  [%expr
    match ([%e body] : unit) with
    | () -> ()
    | exception __windtrap_e -> Stdlib.raise_notrace __windtrap_e]

let add_test ~ctxt ~tags name body =
  let at = Expansion_context.Extension.extension_point_loc ctxt in
  let loc = { at with loc_ghost = true } in
  let file = Expansion_context.Extension.input_name ctxt in
  registration ~loc "add_test"
    [
      (Labelled "file", estring ~loc file);
      (Labelled "pos", pos_expr ~loc at);
      (Labelled "tags", tags_expr ~loc tags);
      (Nolabel, estring ~loc name);
      (Nolabel, [%expr fun () -> [%e body]]);
    ]

(* [M] between [enter_group] and [leave_group]: the tests that [M]'s
   initialization registers, nested groups included, are the group's. *)
let group ~ctxt name (mb : module_binding) =
  List.iter reject_attribute mb.pmb_attributes;
  let tags = List.concat_map tags_of mb.pmb_attributes in
  let at = Expansion_context.Extension.extension_point_loc ctxt in
  let loc = { at with loc_ghost = true } in
  let file = Expansion_context.Extension.input_name ctxt in
  let enter =
    registration ~loc "enter_group"
      [
        (Labelled "file", estring ~loc file);
        (Labelled "tags", tags_expr ~loc tags);
        (Nolabel, estring ~loc name);
      ]
  in
  let leave =
    [%stri let () = Ppx_windtrap_runtime.Ppx_runtime.leave_group ()]
  in
  let binding =
    {
      (module_binding ~loc
         ~name:(Located.mk ~loc (Some name))
         ~expr:mb.pmb_expr)
      with
      pmb_attributes = List.filter (fun a -> not (is_tags a)) mb.pmb_attributes;
    }
  in
  [ enter; pstr_module ~loc binding; leave ]

(* Expect nodes *)

let sanitized_output ~loc =
  [%expr Expect_test_config.sanitize (Windtrap.output ())]

(* [body] with each expect node lexically inside it replaced, and the
   positions of the nodes in source order. A node marks itself reached, then
   checks. An unsupported node is refused here too, so that a dropped test
   refuses it. *)
let expect_body body =
  let nodes = ref [] in
  let map =
    object
      inherit Ast_traverse.map as super

      method! expression e =
        let loc = { e.pexp_loc with loc_ghost = true } in
        let replace call = { call with pexp_attributes = e.pexp_attributes } in
        match e.pexp_desc with
        | Pexp_extension ({ txt = ("expect" | "expect_exact") as verb; _ }, p)
          ->
            let literal =
              match p with
              | PStr [] -> estring ~loc ""
              | PStr
                  [
                    {
                      pstr_desc =
                        Pstr_eval
                          ( ({ pexp_desc = Pexp_constant (Pconst_string _); _ }
                             as literal),
                            _ );
                      _;
                    };
                  ] ->
                  literal
              | _ ->
                  Location.raise_errorf ~loc:e.pexp_loc
                    "Expected a string literal payload"
            in
            let verb = evar ~loc ("Windtrap." ^ verb) in
            let pos = pos_expr ~loc e.pexp_loc in
            nodes := pos :: !nodes;
            replace
              [%expr
                Ppx_windtrap_runtime.Ppx_runtime.reach [%e pos];
                [%e verb] [%e sanitized_output ~loc] ([%e pos], [%e literal])]
        | Pexp_extension ({ txt = "expect.output"; _ }, PStr []) ->
            replace (sanitized_output ~loc)
        | Pexp_extension ({ txt = "expect.output"; loc; _ }, _) ->
            Location.raise_errorf ~loc "[%%expect.output] takes no payload"
        | Pexp_extension ({ txt = name; loc }, _) when is_expect_family name ->
            err_unsupported ~loc name
        | _ -> super#expression e
    end
  in
  let body = map#expression body in
  (body, List.rev !nodes)

(* Extensions *)

let expect_test =
  Extension.V3.declare_inline "expect_test" Extension.Context.structure_item
    Ast_pattern.(pstr __)
    (fun ~ctxt items ->
      let at = Expansion_context.Extension.extension_point_loc ctxt in
      match items with
      | [ { pstr_desc = Pstr_value (Nonrecursive, [ vb ]); _ } ] ->
          let name, tags, body =
            test_of_binding ~extension:"expect_test" ~loc:at vb
          in
          let stop = body.pexp_loc.loc_end in
          let body, nodes = expect_body body in
          let loc = { at with loc_ghost = true } in
          (* At the synchronous type, a monadic config's [run], which would
             drop the body's effects, is a type error at the test. *)
          let body =
            [%expr
              Ppx_windtrap_runtime.Ppx_runtime.expect_test
                ~pos:[%e pos_expr ~loc { at with loc_end = stop }]
                ~body_end:
                  [%e
                    pos_expr ~loc { at with loc_start = stop; loc_end = stop }]
                ~nodes:[%e elist ~loc nodes]
                (fun () ->
                  (Expect_test_config.run : (unit -> unit) -> unit) (fun () ->
                      [%e out_of_tail body]))
                (fun () -> [%e sanitized_output ~loc])]
          in
          when_enabled [ add_test ~ctxt ~tags name body ]
      | _ ->
          Location.raise_errorf ~loc:at
            "Expected let%%expect_test <name> = <expr>")

let test =
  Extension.V3.declare_inline "test" Extension.Context.structure_item
    Ast_pattern.(pstr __)
    (fun ~ctxt items ->
      let at = Expansion_context.Extension.extension_point_loc ctxt in
      match items with
      | [ { pstr_desc = Pstr_value (Nonrecursive, [ vb ]); _ } ] ->
          let name, tags, body = test_of_binding ~extension:"test" ~loc:at vb in
          when_enabled [ add_test ~ctxt ~tags name (out_of_tail body) ]
      | [
       {
         pstr_desc =
           Pstr_module ({ pmb_name = { txt = Some name; _ }; _ } as mb);
         _;
       };
      ] ->
          when_enabled (group ~ctxt name mb)
      | _ ->
          Location.raise_errorf ~loc:at
            "Expected let%%test \"name\" = ..., let%%test _ = ..., or \
             module%%test Name = ...")

(* After the context-free pass, a node of the family that remains is outside
   a let%expect_test body or unsupported. *)
let reject_leftovers =
  object
    inherit Ast_traverse.iter as super

    method! extension ((name, _) as ext) =
      match name.txt with
      | "expect" | "expect_exact" | "expect.output" ->
          Location.raise_errorf ~loc:name.loc
            "[%%%s] must appear inside a let%%expect_test body" name.txt
      | s when is_expect_family s -> err_unsupported ~loc:name.loc s
      | _ -> super#extension ext

    method! attribute attr =
      reject_attribute attr;
      super#attribute attr
  end

let () =
  Driver.register_transformation "ppx_windtrap"
    ~rules:
      [
        Context_free.Rule.extension expect_test;
        Context_free.Rule.extension test;
      ]
    ~impl:(fun str ->
      reject_leftovers#structure str;
      str)
