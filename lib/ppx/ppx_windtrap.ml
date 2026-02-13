(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Ppxlib
open Ast_builder.Default

type maybe_drop = Keep | Drop

let maybe_drop_mode = ref Keep

let () =
  Driver.Cookies.add_simple_handler "inline-test"
    Ast_pattern.(pexp_ident (lident __'))
    ~f:(function
      | None -> ()
      | Some id -> (
          match id.txt with
          | "drop" | "drop_with_deadcode" -> maybe_drop_mode := Drop
          | s ->
              Location.raise_errorf ~loc:id.loc
                "invalid 'inline-test' cookie (%s), expected one of: drop, \
                 drop_with_deadcode"
                s))

let () =
  Driver.Cookies.add_simple_handler "inline_tests"
    Ast_pattern.(estring __')
    ~f:(function
      | None -> ()
      | Some id -> (
          match id.txt with
          | "enabled" -> maybe_drop_mode := Keep
          | "disabled" | "ignored" -> maybe_drop_mode := Drop
          | s ->
              Location.raise_errorf ~loc:id.loc
                "invalid 'inline_tests' cookie (%s), expected one of: enabled, \
                 disabled or ignored"
                s))

let maybe_drop items = match !maybe_drop_mode with Keep -> items | Drop -> []

(* ───── Expect Extensions ───── *)

let extract_string_payload_with_loc ~loc payload =
  match payload with
  | PStr [] -> (None, None)
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ( {
                  pexp_desc = Pexp_constant (Pconst_string (s, _, _));
                  pexp_loc;
                  _;
                },
                _ );
          _;
        };
      ] ->
      (Some s, Some pexp_loc)
  | _ -> Location.raise_errorf ~loc "Expected a string literal payload"

let make_location_expr ~loc ~file (ploc : Location.t) =
  [%expr
    {
      Windtrap.Ppx_runtime.file = [%e estring ~loc file];
      line = [%e eint ~loc ploc.loc_start.pos_lnum];
      start_col =
        [%e eint ~loc (ploc.loc_start.pos_cnum - ploc.loc_start.pos_bol)];
      end_col = [%e eint ~loc (ploc.loc_end.pos_cnum - ploc.loc_end.pos_bol)];
    }]

let make_point_location_expr ~loc ~file (pos : Lexing.position) =
  [%expr
    {
      Windtrap.Ppx_runtime.file = [%e estring ~loc file];
      line = [%e eint ~loc pos.pos_lnum];
      start_col = [%e eint ~loc (pos.pos_cnum - pos.pos_bol)];
      end_col = [%e eint ~loc (pos.pos_cnum - pos.pos_bol)];
    }]

(* Both [%expect] and [%expect_exact] share the same expansion logic,
   differing only in which runtime function they call. *)
let declare_expect_extension name ~make_call =
  Extension.V3.declare name Extension.Context.expression
    Ast_pattern.(pstr __)
    (fun ~ctxt payload ->
      let loc = Expansion_context.Extension.extension_point_loc ctxt in
      let file = Expansion_context.Extension.input_name ctxt in
      let expected, payload_loc =
        extract_string_payload_with_loc ~loc (PStr payload)
      in
      let loc_expr =
        make_location_expr ~loc ~file (Option.value ~default:loc payload_loc)
      in
      let expected_expr =
        match expected with
        | None -> [%expr None]
        | Some s -> [%expr Some [%e estring ~loc s]]
      in
      make_call ~loc loc_expr expected_expr)

let expect_extension =
  declare_expect_extension "expect"
    ~make_call:(fun ~loc loc_expr expected_expr ->
      [%expr
        Windtrap.Ppx_runtime.expect ~loc:[%e loc_expr]
          ~expected:[%e expected_expr]])

let expect_exact_extension =
  declare_expect_extension "expect_exact"
    ~make_call:(fun ~loc loc_expr expected_expr ->
      [%expr
        Windtrap.Ppx_runtime.expect_exact ~loc:[%e loc_expr]
          ~expected:[%e expected_expr]])

let expect_output_extension =
  Extension.V3.declare "expect.output" Extension.Context.expression
    Ast_pattern.(pstr nil)
    (fun ~ctxt ->
      let loc = Expansion_context.Extension.extension_point_loc ctxt in
      [%expr Windtrap.Ppx_runtime.output ()])

(* ───── Inline Test Runner Extensions ───── *)

type test_name = Explicit of string | Anonymous

type expect_test_binding = {
  name : test_name;
  tags : string list;
  body : expression;
}

let parse_tags_attr ~loc attr =
  match attr with
  | {
   attr_name = { txt = "tags"; _ };
   attr_payload =
     PStr
       [
         {
           pstr_desc =
             Pstr_eval
               ({ pexp_desc = Pexp_constant (Pconst_string (s, _, _)); _ }, _);
           _;
         };
       ];
   _;
  } ->
      [ s ]
  | {
   attr_name = { txt = "tags"; _ };
   attr_payload =
     PStr
       [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_tuple exprs; _ }, _); _ } ];
   _;
  } ->
      List.map
        (function
          | { pexp_desc = Pexp_constant (Pconst_string (s, _, _)); _ } -> s
          | _ ->
              Location.raise_errorf ~loc
                "Expected [@tags \"...\"] or [@tags (\"...\", ...)]")
        exprs
  | { attr_name = { txt = "tags"; _ }; _ } ->
      Location.raise_errorf ~loc
        "Expected [@tags \"...\"] or [@tags (\"...\", ...)]"
  | _ -> []

let parse_test_name ~loc pat =
  match pat.ppat_desc with
  | Ppat_constant (Pconst_string (name, _, _)) -> Explicit name
  | Ppat_any -> Anonymous
  | _ ->
      Location.raise_errorf ~loc
        "Expected let%%expect_test \"name\" = ... or let%%expect_test _ = ..."

let parse_expect_test_binding ~loc items =
  match items with
  | [
   {
     pstr_desc = Pstr_value (Nonrecursive, [ { pvb_pat; pvb_expr = body; _ } ]);
     _;
   };
  ] ->
      let tags =
        List.concat_map (parse_tags_attr ~loc) pvb_pat.ppat_attributes
      in
      { name = parse_test_name ~loc pvb_pat; tags; body }
  | _ -> Location.raise_errorf ~loc "Expected let%%expect_test <name> = <expr>"

let count_must_reach_nodes expr =
  let counter =
    object
      inherit [int] Ast_traverse.fold as super

      method! expression e acc =
        let acc =
          match e.pexp_desc with
          | Pexp_extension ({ txt = "expect" | "expect_exact"; _ }, _) ->
              acc + 1
          | _ -> acc
        in
        super#expression e acc
    end
  in
  counter#expression expr 0

let expect_test_extension =
  Extension.V3.declare_inline "expect_test" Extension.Context.structure_item
    Ast_pattern.(pstr __)
    (fun ~ctxt items ->
      let loc = Expansion_context.Extension.extension_point_loc ctxt in
      let file = Expansion_context.Extension.input_name ctxt in
      let binding = parse_expect_test_binding ~loc items in
      let line = loc.loc_start.pos_lnum in
      let test_name =
        match binding.name with
        | Explicit name -> name
        | Anonymous -> Printf.sprintf "line_%d" line
      in
      let tags_expr = elist ~loc (List.map (estring ~loc) binding.tags) in
      let must_reach_count = count_must_reach_nodes binding.body in
      let trailing_loc_expr =
        make_point_location_expr ~loc ~file binding.body.pexp_loc.loc_end
      in
      [
        [%stri
          let () =
            Windtrap.Ppx_runtime.add_test ~file:[%e estring ~loc file]
              ~tags:[%e tags_expr] [%e estring ~loc test_name] (fun () ->
                Windtrap.Ppx_runtime.run_expect_test
                  ~must_reach_count:[%e eint ~loc must_reach_count]
                  ~trailing_loc:[%e trailing_loc_expr] (fun () ->
                    [%e binding.body]))];
      ]
      |> maybe_drop)

(* ───── Module-Based Test Syntax ───── *)

type test_item =
  | Test_case of test_name * string list * expression
  | Test_module of string * string list * module_expr

let parse_test_item ~loc items =
  match items with
  | [
   {
     pstr_desc = Pstr_value (Nonrecursive, [ { pvb_pat; pvb_expr = body; _ } ]);
     _;
   };
  ] ->
      let tags =
        List.concat_map (parse_tags_attr ~loc) pvb_pat.ppat_attributes
      in
      Test_case (parse_test_name ~loc pvb_pat, tags, body)
  | [
   {
     pstr_desc =
       Pstr_module
         {
           pmb_name = { txt = Some name; _ };
           pmb_expr = mod_expr;
           pmb_attributes;
           _;
         };
     _;
   };
  ] ->
      let tags = List.concat_map (parse_tags_attr ~loc) pmb_attributes in
      Test_module (name, tags, mod_expr)
  | _ ->
      Location.raise_errorf ~loc
        "Expected let%%test \"name\" = ..., let%%test _ = ..., or module%%test \
         Name = ..."

let test_extension =
  Extension.V3.declare_inline "test" Extension.Context.structure_item
    Ast_pattern.(pstr __)
    (fun ~ctxt items ->
      let loc = Expansion_context.Extension.extension_point_loc ctxt in
      let file = Expansion_context.Extension.input_name ctxt in
      let item = parse_test_item ~loc items in
      match item with
      | Test_case (name, tags, body) ->
          let line = loc.loc_start.pos_lnum in
          let test_name =
            match name with
            | Explicit n -> n
            | Anonymous -> Printf.sprintf "line_%d" line
          in
          let tags_expr = elist ~loc (List.map (estring ~loc) tags) in
          maybe_drop
            [
              [%stri
                let () =
                  Windtrap.Ppx_runtime.add_test ~file:[%e estring ~loc file]
                    ~tags:[%e tags_expr] [%e estring ~loc test_name] (fun () ->
                      [%e body])];
            ]
      | Test_module (name, tags, module_expr) ->
          (* Wrap the module body with enter_group/leave_group calls so that
             tests defined inside are scoped under this group name. *)
          let tags_expr = elist ~loc (List.map (estring ~loc) tags) in
          maybe_drop
            [
              [%stri
                let () =
                  Windtrap.Ppx_runtime.enter_group ~file:[%e estring ~loc file]
                    ~tags:[%e tags_expr] [%e estring ~loc name]];
              pstr_module ~loc
                (module_binding ~loc
                   ~name:(Located.mk ~loc (Some name))
                   ~expr:module_expr);
              [%stri let () = Windtrap.Ppx_runtime.leave_group ()];
            ])

let run_tests_extension =
  Extension.V3.declare_inline "run_tests" Extension.Context.structure_item
    Ast_pattern.(pstr __)
    (fun ~ctxt payload ->
      let loc = Expansion_context.Extension.extension_point_loc ctxt in
      let name_expr =
        match payload with
        | [] -> [%expr "Tests"]
        | [
         {
           pstr_desc =
             Pstr_eval
               ({ pexp_desc = Pexp_constant (Pconst_string (s, _, _)); _ }, _);
           _;
         };
        ] ->
            estring ~loc s
        | _ -> Location.raise_errorf ~loc "Expected optional string argument"
      in
      maybe_drop
        [ [%stri let () = Windtrap.Ppx_runtime.run_tests [%e name_expr]] ])

(* ───── Registration ───── *)

let () =
  Driver.register_transformation "ppx_windtrap"
    ~rules:
      [
        Context_free.Rule.extension expect_extension;
        Context_free.Rule.extension expect_exact_extension;
        Context_free.Rule.extension expect_output_extension;
        Context_free.Rule.extension expect_test_extension;
        Context_free.Rule.extension test_extension;
        Context_free.Rule.extension run_tests_extension;
      ]

(* ───── Coverage Instrumentation ───── *)

let coverage_enabled = ref false

let () =
  Driver.add_arg "--coverage" (Arg.Set coverage_enabled)
    ~doc:" Enable coverage instrumentation"

let () =
  let impl ctxt ast =
    if !coverage_enabled then
      (new Instrument.instrumenter)#transform_impl_file ctxt ast
    else ast
  in
  let instrument = Driver.Instrument.V2.make impl ~position:After in
  Driver.register_transformation ~instrument "ppx_windtrap_coverage"
