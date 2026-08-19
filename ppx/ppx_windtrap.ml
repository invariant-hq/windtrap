(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The ppx_windtrap rewriter: a location recorder and nothing more.
   It rewrites [let%test] / [module%test] / [let%expect_test]
   into registration calls, each [[%expect]] / [[%expect_exact]] node
   into [Ppx_runtime.expect ~id] with a declared node table, and
   [[%expect.output]] into [Ppx_runtime.expect_output ()]. Every
   semantic decision — matching, normalization, reachability,
   corrections, exit codes — lives in Ppx_runtime, ordinary OCaml.

   Adapted from windtrap 0.1's ppx/ppx_windtrap.ml (extension surface,
   cookie handling) and rebased on the v3 Ppx_runtime contract: per-node
   ids with exact payload extents replace v1's per-call locations, and
   the generated code references the ambient [Expect_test_config]
   top-level module, matching ppx_expect (mechanism (b) below). Node and
   location shapes follow upstream ppx_expect's src/ppx_expect.ml at the
   pinned conformance commit (test/conformance/NOTICE).

   Two ppx_expect compatibility mechanisms live here: (a) expect-family
   attributes and extensions this PPX does not implement are rejected at
   expansion time with a "not supported by ppx_windtrap" error — never
   left unexpanded; (b) generated code references the ambient
   [Expect_test_config], so user code can shadow it exactly as with
   ppx_expect. *)

open Ppxlib
open Ast_builder.Default

(* Dune's inline_tests cookie

   The one cookie a build sets: dune passes [inline_tests="disabled"] for
   a library whose (inline_tests) stanza is off and for a profile that
   disables them, and the registrations are dropped rather than compiled
   into a runner nothing drives. ppx_inline_test's own [inline-test=drop]
   spelling is Jenga's, not dune's, and reached this rewriter from
   nowhere but the golden that tested it. *)

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

(* The runtime path in generated code *)

(* One place spells the path; generated references, constructors, and
   record fields all derive from it. *)
let runtime_lid name =
  Ldot (Ldot (Lident "Ppx_windtrap_runtime", "Ppx_runtime"), name)

let runtime_fn ~loc name = pexp_ident ~loc { txt = runtime_lid name; loc }

(* A record of a runtime-declared type: every field is qualified. A bare
   field would resolve by type-directed disambiguation (Ppx_runtime is
   never opened in user code, and [loc] names a field of several of its
   record types), which is warning 42 — fatal in a user library compiled
   with -w +a -warn-error +a. *)
let runtime_record ~loc fields =
  match fields with
  | [] -> assert false
  | _ :: _ ->
      pexp_record ~loc
        (List.map (fun (f, e) -> ({ txt = runtime_lid f; loc }, e)) fields)
        None

let runtime_construct ~loc name arg =
  pexp_construct ~loc { txt = runtime_lid name; loc } arg

(* Locations as Ppx_runtime.loc expressions *)

(* Ppx_runtime.loc is byte offsets: start_pos/end_pos are pos_cnum,
   start_bol is pos_bol, line is 1-based pos_lnum (see
   runtime/ppx_runtime.mli; the shape is ppx_expect's Compact_loc plus
   the report line). *)
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
            ("literal_loc", compact_loc_expr ~loc literal_loc);
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

(* Does a [;] appended to this expression bind to something inside it?

   A trailing correction writes [<body>; [%expect ...]]. After a [match],
   [try] or [function] the [;] joins the LAST ARM, so the node lands inside
   that arm: it runs on one branch only, the promoted source means something
   other than the correction intended, and the next run inserts another dead
   node beside it — the correction never converges.

   The hazard belongs to the expression the body ENDS with, not the one it
   starts with. [let x = ... in match ...] and [stmt; match ...] are the
   common shapes and both end in a match, so the walk descends every
   construct that carries a tail and asks the same question there. Anything
   that cannot swallow the [;] — an application, an ident, a constructor —
   ends the walk. *)
let rec swallows_semicolon e =
  match e.pexp_desc with
  | Pexp_match _ | Pexp_try _ -> true
  (* [function p -> e | ...] has arms; [fun x -> e] does not, and its tail
     is [e]. Both are Pexp_function since OCaml 5.2. *)
  | Pexp_function (_, _, Pfunction_cases _) -> true
  | Pexp_function (_, _, Pfunction_body body) -> swallows_semicolon body
  | Pexp_let (_, _, body)
  | Pexp_letmodule (_, _, body)
  | Pexp_letexception (_, body)
  | Pexp_open (_, body)
  | Pexp_sequence (_, body)
  | Pexp_constraint (body, _)
  | Pexp_coerce (body, _, _) ->
      swallows_semicolon body
  | Pexp_letop { body; _ } -> swallows_semicolon body
  | Pexp_ifthenelse (_, then_, else_) -> (
      (* Without an [else] the [then] branch is the tail. *)
      match else_ with
      | Some e -> swallows_semicolon e
      | None -> swallows_semicolon then_)
  | _ -> false

(* The preprocessed source, read once per input file. [None] when it cannot
   be read; see [already_delimited] for why the source is consulted at all
   and what an unreadable one costs. *)
let source_cache : (string, string option) Hashtbl.t = Hashtbl.create 1

let source_of file =
  match Hashtbl.find_opt source_cache file with
  | Some cached -> cached
  | None ->
      let contents =
        match open_in_bin file with
        | ic ->
            Fun.protect
              ~finally:(fun () -> close_in_noerr ic)
              (fun () -> Some (really_input_string ic (in_channel_length ic)))
        | exception Sys_error _ -> None
      in
      Hashtbl.add source_cache file contents;
      contents

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

(* Does the expression beginning at [start] carry its own delimiters?

   OCaml's parser gives a delimited expression the location of the
   DELIMITERS: [(match ... with ...)] parses to the same [Pexp_match] as the
   bare form, with [pexp_loc] starting at the [(], and [begin match ... end]
   likewise. Nothing in the AST tells the two apart, so the question is
   answered from the source the parser read: without it, the wrap below
   would insert a SECOND pair of parentheses around a body that already had
   one, on every round that corrects.

   One character can only answer for the whole body when the body's root is
   the node that swallows the [;] — [root_swallows_semicolon] below is the
   guard that decides whether this is asked at all, and states what goes
   wrong when it is asked of anything else.

   Best-effort, like every source read in this codebase: the file is a dune
   dependency of the preprocessing action and the offsets are the ones the
   runtime patches, so an unreadable source does not happen in practice — and
   if it did, falling back to wrapping keeps today's behaviour, where a
   redundant pair is cosmetic and a missing one strands the inserted node
   inside a match arm. *)
let already_delimited ~file (start : Lexing.position) =
  match source_of file with
  | None -> false
  | Some source ->
      let length = String.length source in
      let off = start.pos_cnum in
      let keyword kw =
        let n = String.length kw in
        off + n <= length
        && String.equal (String.sub source off n) kw
        && (off + n = length || not (is_ident_char source.[off + n]))
      in
      off >= 0 && off < length && (source.[off] = '(' || keyword "begin")

(* Is the body ITSELF the node that swallows the semicolon?

   [already_delimited] answers a question about the body's first character,
   so it may only be consulted when that character can belong to the whole
   body. For a root that swallows — [(match ... with ...)] — it does. For a
   root that merely ENDS in one, the leading delimiter belongs to something
   inside it: in

     (print_string "pre"); match x with ... -> ...

   the parentheses close after [print_string], and treating the body as
   delimited would skip the wrap and strand the inserted node in the last
   arm — the very defect the wrap exists to prevent. A body like
   [(let x = 1 in match ...)] is genuinely delimited and gets a redundant
   pair instead, which is the safe direction: a doubled pair is cosmetic,
   a missing one changes what the promoted file means. *)
let root_swallows_semicolon e =
  match e.pexp_desc with
  | Pexp_match _ | Pexp_try _ | Pexp_function (_, _, Pfunction_cases _) -> true
  | _ -> false

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
      (* A trailing correction sequences [;] onto the body. After a bare
         [match] or [try] that [;] binds to the LAST ARM, so the inserted
         [%expect] lands inside the arm instead of after the body: the
         promoted file means something else and the correction never
         converges. Such a body is parenthesized as part of the same patch,
         which needs its own start offset — [body_loc] starts at the
         extension point, not at the body. A body that already brought its
         own delimiters keeps them: a second pair would say the same thing
         twice, one pair deeper on every promoted round. *)
      let loc = { ext_loc with loc_ghost = true } in
      let body_wrap =
        if
          swallows_semicolon binding.body
          && not
               (root_swallows_semicolon binding.body
               && already_delimited ~file binding.body.pexp_loc.loc_start)
        then
          [%expr Some [%e eint ~loc binding.body.pexp_loc.loc_start.pos_cnum]]
        else [%expr None]
      in
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
            (Labelled "body_wrap", body_wrap);
            (Labelled "trailing_loc", point_loc_expr ~loc ext_loc.loc_end);
            (Nolabel, estring ~loc name);
            (Nolabel, [%expr fun () -> [%e body]]);
          ]
      in
      maybe_drop [ [%stri let () = [%e call]] ])

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
