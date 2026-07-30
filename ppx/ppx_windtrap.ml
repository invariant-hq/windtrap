(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The ppx_windtrap rewriter: a location recorder and nothing more (RFC
   Law 10). It rewrites [let%test] / [module%test] / [let%expect_test]
   into registration calls, each [[%expect]] / [[%expect_exact]] node
   into [Ppx_runtime.expect ~id] with a declared node table, and
   [[%expect.output]] into [Ppx_runtime.expect_output ()]. Every
   semantic decision — matching, normalization, reachability,
   corrections, exit codes — lives in Ppx_runtime, ordinary OCaml.

   Adapted from windtrap 0.1's ppx/ppx_windtrap.ml (extension surface,
   cookie handling) and rebased on the v3 Ppx_runtime contract: per-node
   ids with exact payload extents replace v1's per-call locations, and
   the generated code references the ambient [Expect_test_config] (RFC
   compat mechanism (b)). Node and location shapes follow upstream
   ppx_expect's src/ppx_expect.ml at the pinned conformance commit
   (test/conformance/NOTICE) where the RFC cites it.

   Compat mechanism (a): expect-family attributes and extensions this
   PPX does not implement are rejected at expansion time with a "not
   supported by ppx_windtrap" error — never left unexpanded. *)

open Ppxlib
open Ast_builder.Default

(* ───── Jane Street cookies (inline-test drop modes) ───── *)

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

(* ───── The runtime path in generated code ───── *)

(* One place spells the path; generated references, constructors, and
   record fields all derive from it. *)
let runtime_lid name =
  Ldot (Ldot (Lident "Ppx_windtrap_runtime", "Ppx_runtime"), name)

let runtime_fn ~loc name = pexp_ident ~loc { txt = runtime_lid name; loc }

(* A record of a runtime-declared type: the first field is qualified so
   the type resolves without warnings in any user warning regime. *)
let runtime_record ~loc fields =
  match fields with
  | [] -> assert false
  | (first, e) :: rest ->
      pexp_record ~loc
        (({ txt = runtime_lid first; loc }, e)
        :: List.map (fun (f, e) -> ({ txt = Lident f; loc }, e)) rest)
        None

let runtime_construct ~loc name arg =
  pexp_construct ~loc { txt = runtime_lid name; loc } arg

(* ───── Locations as Ppx_runtime.loc expressions ───── *)

(* Ppx_runtime.loc is byte offsets: start_pos/end_pos are pos_cnum,
   start_bol is pos_bol, line is 1-based pos_lnum (see
   lib/ppx_runtime.mli; the shape is ppx_expect's Compact_loc plus the
   report line). *)
let compact_loc_expr ~loc (l : Location.t) =
  runtime_record ~loc
    [
      ("line", eint ~loc l.loc_start.pos_lnum);
      ("start_bol", eint ~loc l.loc_start.pos_bol);
      ("start_pos", eint ~loc l.loc_start.pos_cnum);
      ("end_pos", eint ~loc l.loc_end.pos_cnum);
    ]

(* A zero-width point, for trailing-output insertions. *)
let point_loc_expr ~loc (p : Lexing.position) =
  runtime_record ~loc
    [
      ("line", eint ~loc p.pos_lnum);
      ("start_bol", eint ~loc p.pos_bol);
      ("start_pos", eint ~loc p.pos_cnum);
      ("end_pos", eint ~loc p.pos_cnum);
    ]

(* ───── The expect-family compatibility envelope ───── *)

(* Names ppx_expect claims that this PPX implements. Everything else in
   the family is rejected loudly (mechanism (a)). *)
let is_implemented_expect = function
  | "expect" | "expect_exact" | "expect.output" -> true
  | _ -> false

let is_expect_family name =
  String.equal name "expect"
  || String.equal name "expect_exact"
  || String.equal name "expectation"
  || String.starts_with ~prefix:"expect." name
  || String.starts_with ~prefix:"expectation." name

let reject_expect_family_attributes attrs =
  List.iter
    (fun attr ->
      let name = attr.attr_name in
      if is_expect_family name.txt then
        Location.raise_errorf ~loc:name.loc
          "%s is not supported by ppx_windtrap"
          ("[@@" ^ name.txt ^ "]"))
    attrs

(* After the context-free pass every legitimate expect-family node has
   been consumed by [let%expect_test]. Whatever survives is either
   misplaced (supported syntax outside an expect test) or unimplemented
   (mechanism (a)); both are compile errors, never left unexpanded. *)
let reject_leftovers =
  object
    inherit Ast_traverse.iter as super

    method! extension ((name, _) as ext) =
      if is_expect_family name.txt then
        if is_implemented_expect name.txt then
          Location.raise_errorf ~loc:name.loc
            "[%%%s] must appear inside a let%%expect_test body" name.txt
        else
          Location.raise_errorf ~loc:name.loc
            "[%%%s] is not supported by ppx_windtrap" name.txt
      else super#extension ext

    method! attribute attr =
      reject_expect_family_attributes [ attr ];
      super#attribute attr
  end

(* ───── Shared parsing: names and [@tags] ───── *)

type test_name = Explicit of string | Anonymous

let parse_tags_attr attr =
  let loc = attr.attr_loc in
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

let test_name_string ~loc = function
  | Explicit name -> name
  | Anonymous -> Printf.sprintf "line_%d" loc.loc_start.pos_lnum

let tags_expr ~loc tags = elist ~loc (List.map (estring ~loc) tags)

(* ───── let%expect_test ───── *)

type expect_test_binding = {
  name : test_name;
  tags : string list;
  body : expression;
}

let parse_expect_test_binding ~loc items =
  match items with
  | [
   {
     pstr_desc =
       Pstr_value
         (Nonrecursive, [ { pvb_pat; pvb_expr = body; pvb_attributes; _ } ]);
     _;
   };
  ] ->
      (* [@@expect.uncaught_exn] and friends land here; mechanism (a).
         The name pattern's attributes are also checked: everything of
         the family must be rejected, never silently dropped. *)
      reject_expect_family_attributes pvb_attributes;
      reject_expect_family_attributes pvb_pat.ppat_attributes;
      let tags = List.concat_map parse_tags_attr pvb_pat.ppat_attributes in
      { name = parse_test_name ~loc pvb_pat; tags; body }
  | _ -> Location.raise_errorf ~loc "Expected let%%expect_test <name> = <expr>"

(* The payload of one [[%expect]] / [[%expect_exact]] node: the string
   literal's parsed contents, its delimiter, and the extent of the whole
   literal including delimiters — the exact range a correction
   overwrites. *)
type parsed_payload = {
  contents : string;
  delimiter : string option;  (** [None] = ["…"], [Some tag] = [{tag|…|tag}]. *)
  literal_loc : Location.t;
}

let parse_expect_payload ~loc (payload : payload) =
  match payload with
  | PStr [] -> None
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ( {
                  pexp_desc = Pexp_constant (Pconst_string (s, _, tag));
                  pexp_loc;
                  _;
                },
                _ );
          _;
        };
      ] ->
      Some { contents = s; delimiter = tag; literal_loc = pexp_loc }
  | _ -> Location.raise_errorf ~loc "Expected a string literal payload"

let payload_expr ~loc = function
  | None -> [%expr None]
  | Some { contents; delimiter; literal_loc } ->
      let delimiter_expr =
        match delimiter with
        | None -> runtime_construct ~loc "Quote" None
        | Some tag -> runtime_construct ~loc "Tag" (Some (estring ~loc tag))
      in
      let record =
        runtime_record ~loc
          [
            ("contents", estring ~loc contents);
            ("delimiter", delimiter_expr);
            ("loc", compact_loc_expr ~loc literal_loc);
          ]
      in
      [%expr Some [%e record]]

let node_expr ~loc ~id ~kind ~node_loc payload =
  runtime_record ~loc
    [
      ("id", eint ~loc id);
      ("kind", runtime_construct ~loc kind None);
      ("loc", compact_loc_expr ~loc node_loc);
      ("payload", payload_expr ~loc payload);
    ]

(* Replaces every expect node lexically inside [body] with its runtime
   call and collects the node table. Ids are assigned in traversal
   order, which is source order (sub-expressions are visited
   left-to-right). Unimplemented family extensions are rejected here so
   the error points at the node, not at a leftover. *)
let rewrite_expect_body body =
  let mapper =
    object
      inherit [int * expression list] Ast_traverse.fold_map as super

      method! expression e ((next_id, nodes_rev) as acc) =
        match e.pexp_desc with
        | Pexp_extension ({ txt = ("expect" | "expect_exact") as name; _ }, p)
          ->
            let node_loc = e.pexp_loc in
            let loc = { node_loc with loc_ghost = true } in
            let payload = parse_expect_payload ~loc:node_loc p in
            let kind =
              if String.equal name "expect" then "Expect" else "Expect_exact"
            in
            let node = node_expr ~loc ~id:next_id ~kind ~node_loc payload in
            let call =
              pexp_apply ~loc (runtime_fn ~loc "expect")
                [ (Labelled "id", eint ~loc next_id) ]
            in
            ( { call with pexp_attributes = e.pexp_attributes },
              (next_id + 1, node :: nodes_rev) )
        | Pexp_extension ({ txt = "expect.output"; _ }, PStr []) ->
            let loc = { e.pexp_loc with loc_ghost = true } in
            let call =
              pexp_apply ~loc
                (runtime_fn ~loc "expect_output")
                [ (Nolabel, eunit ~loc) ]
            in
            ({ call with pexp_attributes = e.pexp_attributes }, acc)
        | Pexp_extension ({ txt = "expect.output"; loc; _ }, _) ->
            Location.raise_errorf ~loc "[%%expect.output] takes no payload"
        | Pexp_extension ({ txt = name; loc; _ }, _)
          when is_expect_family name && not (is_implemented_expect name) ->
            Location.raise_errorf ~loc "[%%%s] is not supported by ppx_windtrap"
              name
        | _ -> super#expression e acc
    end
  in
  let body, (_, nodes_rev) = mapper#expression body (0, []) in
  (body, List.rev nodes_rev)

let expect_test_extension =
  Extension.V3.declare_inline "expect_test" Extension.Context.structure_item
    Ast_pattern.(pstr __)
    (fun ~ctxt items ->
      let ext_loc = Expansion_context.Extension.extension_point_loc ctxt in
      let file = Expansion_context.Extension.input_name ctxt in
      let binding = parse_expect_test_binding ~loc:ext_loc items in
      let name = test_name_string ~loc:ext_loc binding.name in
      let body, nodes = rewrite_expect_body binding.body in
      (* body_loc ends at the body expression; trailing corrections
         insert at the end of the extension point (ppx_expect's
         shapes). *)
      let body_loc =
        {
          loc_start = ext_loc.loc_start;
          loc_end = binding.body.pexp_loc.loc_end;
          loc_ghost = true;
        }
      in
      let loc = { ext_loc with loc_ghost = true } in
      let call =
        pexp_apply ~loc
          (runtime_fn ~loc "add_expect_test")
          [
            (Labelled "file", estring ~loc file);
            (Labelled "loc", compact_loc_expr ~loc ext_loc);
            (Labelled "tags", tags_expr ~loc binding.tags);
            (Labelled "run", [%expr Expect_test_config.run]);
            (Labelled "sanitize", [%expr Expect_test_config.sanitize]);
            (Labelled "nodes", elist ~loc nodes);
            (Labelled "body_loc", compact_loc_expr ~loc body_loc);
            (Labelled "trailing_loc", point_loc_expr ~loc ext_loc.loc_end);
            (Nolabel, estring ~loc name);
            (Nolabel, [%expr fun () -> [%e body]]);
          ]
      in
      maybe_drop [ [%stri let () = [%e call]] ])

(* ───── let%test and module%test ───── *)

(* [mod_attributes] is the binding's attributes minus the consumed
   [@tags], kept on the rebuilt module — doc comments and [@@warning]
   must survive the rewrite. *)
type test_module = {
  mod_name : string;
  mod_tags : string list;
  mod_expr : module_expr;
  mod_attributes : attributes;
}

type test_item =
  | Test_case of test_name * string list * expression
  | Test_module of test_module

let parse_test_item ~loc items =
  match items with
  | [
   {
     pstr_desc =
       Pstr_value
         (Nonrecursive, [ { pvb_pat; pvb_expr = body; pvb_attributes; _ } ]);
     _;
   };
  ] ->
      reject_expect_family_attributes pvb_attributes;
      reject_expect_family_attributes pvb_pat.ppat_attributes;
      let tags = List.concat_map parse_tags_attr pvb_pat.ppat_attributes in
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
      reject_expect_family_attributes pmb_attributes;
      let tags = List.concat_map parse_tags_attr pmb_attributes in
      let kept =
        List.filter
          (fun attr -> not (String.equal attr.attr_name.txt "tags"))
          pmb_attributes
      in
      Test_module
        { mod_name = name; mod_tags = tags; mod_expr; mod_attributes = kept }
  | _ ->
      Location.raise_errorf ~loc
        "Expected let%%test \"name\" = ..., let%%test _ = ..., or module%%test \
         Name = ..."

let test_extension =
  Extension.V3.declare_inline "test" Extension.Context.structure_item
    Ast_pattern.(pstr __)
    (fun ~ctxt items ->
      let ext_loc = Expansion_context.Extension.extension_point_loc ctxt in
      let file = Expansion_context.Extension.input_name ctxt in
      let loc = { ext_loc with loc_ghost = true } in
      match parse_test_item ~loc:ext_loc items with
      | Test_case (name, tags, body) ->
          let name = test_name_string ~loc:ext_loc name in
          let call =
            pexp_apply ~loc
              (runtime_fn ~loc "add_test")
              [
                (Labelled "file", estring ~loc file);
                (Labelled "loc", compact_loc_expr ~loc ext_loc);
                (Labelled "tags", tags_expr ~loc tags);
                (Nolabel, estring ~loc name);
                (Nolabel, [%expr fun () -> [%e body]]);
              ]
          in
          maybe_drop [ [%stri let () = [%e call]] ]
      | Test_module { mod_name; mod_tags; mod_expr; mod_attributes } ->
          (* Wrap the module with enter_group/leave_group: the module's
             initializers register its tests between the two calls, so
             they nest under the group — including nested
             module%test. *)
          let enter =
            pexp_apply ~loc
              (runtime_fn ~loc "enter_group")
              [
                (Labelled "file", estring ~loc file);
                (Labelled "tags", tags_expr ~loc mod_tags);
                (Nolabel, estring ~loc mod_name);
              ]
          in
          let leave =
            pexp_apply ~loc
              (runtime_fn ~loc "leave_group")
              [ (Nolabel, eunit ~loc) ]
          in
          let binding =
            {
              (module_binding ~loc
                 ~name:(Located.mk ~loc (Some mod_name))
                 ~expr:mod_expr)
              with
              pmb_attributes = mod_attributes;
            }
          in
          maybe_drop
            [
              [%stri let () = [%e enter]];
              pstr_module ~loc binding;
              [%stri let () = [%e leave]];
            ])

(* ───── Registration ───── *)

let () =
  Driver.register_transformation "ppx_windtrap"
    ~rules:
      [
        Context_free.Rule.extension expect_test_extension;
        Context_free.Rule.extension test_extension;
      ]
    ~impl:(fun str ->
      reject_leftovers#structure str;
      str)
