(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The ppx_windtrap rewriter: a desugaring into the core and nothing
   more. [let%test] and [let%expect_test] register a [Windtrap.test] at
   module load through the runtime's registry, [module%test] opens a
   group around a module, each [[%expect]] / [[%expect_exact]] node is a
   [Windtrap.expect] / [Windtrap.expect_exact] call over the sanitized
   captured output with the node's position as its baseline, and
   [[%expect.output]] is that sanitized read. Every semantic decision —
   matching, corrections, exit codes — is the core's.

   Two ppx_expect compatibility mechanisms live here: (a) expect-family
   attributes and extensions this PPX does not implement are rejected at
   expansion time with a "not supported by ppx_windtrap" error — never
   left unexpanded; (b) generated code references the ambient
   [Expect_test_config], so user code can shadow it exactly as with
   ppx_expect, and a monadic config fails to compile at that reference. *)

open Ppxlib
open Ast_builder.Default

(* Dune's inline_tests cookie

   The one cookie a build sets: dune passes [inline_tests="disabled"] for
   a library whose (inline_tests) stanza is off and for a profile that
   disables them, and the registrations are dropped rather than compiled
   into a runner nothing drives. *)

type maybe_drop = Keep | Drop

let maybe_drop_mode = ref Keep

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

(* Positions *)

(* [__POS_OF__]'s tuple for [l]: file, line, and both columns measured
   from the start line — the shape [Windtrap.pos] declares. *)
let pos_expr ~loc (l : Location.t) =
  let s = l.loc_start and e = l.loc_end in
  pexp_tuple ~loc
    [
      estring ~loc s.pos_fname;
      eint ~loc s.pos_lnum;
      eint ~loc (s.pos_cnum - s.pos_bol);
      eint ~loc (e.pos_cnum - s.pos_bol);
    ]

(* The expect-family compatibility envelope *)

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

(* Shared parsing: names and [@tags] *)

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

(* Registration calls *)

(* [add_test ~file ~pos ~tags name (fun () -> body)] at the extension
   point [ext_loc]: the runtime registers [Windtrap.test ~__POS__:pos ~tags
   name] under the file's group. *)
let registration ~ctxt ~tags name body =
  let ext_loc = Expansion_context.Extension.extension_point_loc ctxt in
  let file = Expansion_context.Extension.input_name ctxt in
  let loc = { ext_loc with loc_ghost = true } in
  let pos = pos_expr ~loc ext_loc in
  [%stri
    let () =
      Ppx_windtrap_runtime.Ppx_runtime.add_test ~file:[%e estring ~loc file]
        ~pos:[%e pos] ~tags:[%e tags_expr ~loc tags] [%e estring ~loc name]
        (fun () -> [%e body])]

(* let%expect_test *)

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

(* The payload of one node: its string literal as written, or nothing for
   a bare [[%expect]], whose literal is the empty string until the first
   correction inserts one. *)
let parse_expect_payload ~loc (payload : payload) =
  match payload with
  | PStr [] -> None
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ( ({ pexp_desc = Pexp_constant (Pconst_string _); _ } as literal),
                _ );
          _;
        };
      ] ->
      Some literal
  | _ -> Location.raise_errorf ~loc "Expected a string literal payload"

(* [Expect_test_config.sanitize (Windtrap.output ())]: every read of the
   captured output goes through the ambient config's sanitizer. *)
let sanitized_output ~loc =
  [%expr Expect_test_config.sanitize (Windtrap.output ())]

(* Replaces every expect node lexically inside [body] with its core call.
   The node's own position is the baseline's: what the core patches on a
   correction, and what a failure report names. Unimplemented family
   extensions are rejected here so the error points at the node, not at
   a leftover. *)
let rewrite_expect_body body =
  let mapper =
    object
      inherit Ast_traverse.map as super

      method! expression e =
        match e.pexp_desc with
        | Pexp_extension ({ txt = ("expect" | "expect_exact") as name; _ }, p)
          ->
            let node_loc = e.pexp_loc in
            let loc = { node_loc with loc_ghost = true } in
            let literal =
              match parse_expect_payload ~loc:node_loc p with
              | Some literal -> literal
              | None -> estring ~loc ""
            in
            let pos = pos_expr ~loc node_loc in
            let actual = sanitized_output ~loc in
            let call =
              if String.equal name "expect" then
                [%expr Windtrap.expect [%e actual] ([%e pos], [%e literal])]
              else
                [%expr
                  Windtrap.expect_exact [%e actual] ([%e pos], [%e literal])]
            in
            { call with pexp_attributes = e.pexp_attributes }
        | Pexp_extension ({ txt = "expect.output"; _ }, PStr []) ->
            let loc = { e.pexp_loc with loc_ghost = true } in
            { (sanitized_output ~loc) with pexp_attributes = e.pexp_attributes }
        | Pexp_extension ({ txt = "expect.output"; loc; _ }, _) ->
            Location.raise_errorf ~loc "[%%expect.output] takes no payload"
        | Pexp_extension ({ txt = name; loc; _ }, _)
          when is_expect_family name && not (is_implemented_expect name) ->
            Location.raise_errorf ~loc "[%%%s] is not supported by ppx_windtrap"
              name
        | _ -> super#expression e
    end
  in
  mapper#expression body

let expect_test_extension =
  Extension.V3.declare_inline "expect_test" Extension.Context.structure_item
    Ast_pattern.(pstr __)
    (fun ~ctxt items ->
      let ext_loc = Expansion_context.Extension.extension_point_loc ctxt in
      let binding = parse_expect_test_binding ~loc:ext_loc items in
      let name = test_name_string ~loc:ext_loc binding.name in
      let body = rewrite_expect_body binding.body in
      let loc = { ext_loc with loc_ghost = true } in
      (* The body runs under the ambient config's [run], constrained to
         the synchronous type so that a monadic (Async-style) config fails
         to compile at this reference rather than run its bodies with
         their effects dropped (mechanism (b)). *)
      let body =
        [%expr
          (Expect_test_config.run : (unit -> unit) -> unit) (fun () ->
              [%e body])]
      in
      maybe_drop [ registration ~ctxt ~tags:binding.tags name body ])

(* let%test and module%test *)

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
          maybe_drop [ registration ~ctxt ~tags name body ]
      | Test_module { mod_name; mod_tags; mod_expr; mod_attributes } ->
          (* Wrap the module with enter_group/leave_group: the module's
             initializers register its tests between the two calls, so
             they nest under the group — including nested
             module%test. *)
          let enter =
            [%stri
              let () =
                Ppx_windtrap_runtime.Ppx_runtime.enter_group
                  ~file:[%e estring ~loc file]
                  ~tags:[%e tags_expr ~loc mod_tags] [%e estring ~loc mod_name]]
          in
          let leave =
            [%stri let () = Ppx_windtrap_runtime.Ppx_runtime.leave_group ()]
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
          maybe_drop [ enter; pstr_module ~loc binding; leave ])

(* Registration *)

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
