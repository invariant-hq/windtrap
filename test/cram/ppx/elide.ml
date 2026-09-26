(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* [elide.exe] copies an instrumenting rewriter's output from standard input
   to standard output, with each registration module condensed to its
   table. A registration module is the items between two
   [[@@@ocaml.text "/*"]] lines; one that does not have the exact shape the
   rewriters emit is copied as it is. *)

open Ppxlib

let strf = Printf.sprintf
let marker = {|[@@@ocaml.text "/*"]|}

let integer = function
  | { pexp_desc = Pexp_constant (Pconst_integer (i, None)); _ } -> Some i
  | _ -> None

let string = function
  | { pexp_desc = Pexp_constant (Pconst_string (s, _, None)); _ } ->
      Some (strf "%S" s)
  | _ -> None

let all f es =
  let rec loop acc = function
    | [] -> Some (List.rev acc)
    | e :: es -> Option.bind (f e) (fun row -> loop (row :: acc) es)
  in
  loop [] es

let point = function
  | [%expr
      {
        Windtrap_runtime.Coverage.start_ofs = [%e? start];
        Windtrap_runtime.Coverage.end_ofs = [%e? stop];
      }] -> (
      match (integer start, integer stop) with
      | Some start, Some stop -> Some (strf "%s-%s" start stop)
      | _ -> None)
  | _ -> None

let dismissal = function
  | [%expr None] -> Some ""
  | [%expr Some [%e? reason]] ->
      Option.map (strf ", dismissed %s") (string reason)
  | _ -> None

let site = function
  | [%expr
      {
        line = [%e? line];
        col = [%e? col];
        rewrite = [%e? rewrite];
        before = [%e? before];
        after = [%e? after];
        dismissed = [%e? dismissed];
      }] -> (
      match
        ( integer line,
          integer col,
          string rewrite,
          string before,
          string after,
          dismissal dismissed )
      with
      | ( Some line,
          Some col,
          Some rewrite,
          Some before,
          Some after,
          Some dismissed ) ->
          Some
            (strf "%s:%s %s %s -> %s%s" line col rewrite before after dismissed)
      | _ -> None)
  | _ -> None

(* [helper] names the item a module carries only when the expansion uses it. *)
let table ~title ~name ~file ~helper rows =
  let row i r = strf "  %d: %s" i r in
  let helper = match helper with None -> "" | Some h -> ", with " ^ h in
  Option.map
    (fun file ->
      strf "%s of %s in %s%s:" title file name helper :: List.mapi row rows)
    (string file)

let coverage ~name = function
  | [%stri
      let ___windtrap_visit___ =
        let counts = Array.make [%e? count] 0 in
        Windtrap_runtime.Coverage.register
          ~file:[%e? file]
          ~points:[%e? { pexp_desc = Pexp_array points; _ }]
          ~counts;
        fun index -> Windtrap_runtime.Coverage.visit counts index]
    :: rest -> (
      let helper =
        match rest with
        | [] -> Some None
        | [
         [%stri
           let ___windtrap_post_visit___ point_index result =
             ___windtrap_visit___ point_index;
             result];
        ] ->
            Some (Some "___windtrap_post_visit___")
        | _ -> None
      in
      match (all point points, helper) with
      | Some rows, Some helper
        when Option.equal String.equal (integer count)
               (Some (string_of_int (List.length rows))) ->
          table ~title:"coverage points" ~name ~file ~helper rows
      | _ -> None)
  | _ -> None

let mutation ~name = function
  | [%stri
      type site = Windtrap_runtime.Mutate.site = {
        line : int;
        col : int;
        rewrite : string;
        before : string;
        after : string;
        dismissed : string option;
      }]
    :: rest -> (
      let helper, rest =
        match rest with
        | [%stri type 'a operands = 'a * 'a] :: rest ->
            (Some "type 'a operands", rest)
        | rest -> (None, rest)
      in
      match rest with
      | [
       [%stri
         let ___windtrap_armed___ =
           Windtrap_runtime.Mutate.register
             ~file:[%e? file]
             ~sites:[%e? { pexp_desc = Pexp_array sites; _ }]];
      ] ->
          Option.bind (all site sites)
            (table ~title:"mutation sites" ~name ~file ~helper)
      | _ -> None)
  | _ -> None

(* A metaquot pattern matches any attributes, so a module that carries one
   is not condensed. *)
let carries_attributes items =
  let attributes =
    object
      inherit [bool] Ast_traverse.fold
      method! attribute _ _ = true
    end
  in
  attributes#structure items false

let condensed = function
  | items when carries_attributes items -> None
  | [
      {
        pstr_desc =
          Pstr_module
            {
              pmb_name = { txt = Some name; _ };
              pmb_expr = { pmod_desc = Pmod_structure items; _ };
              _;
            };
        _;
      };
      [%stri
        open [%m? { pmod_desc = Pmod_ident { txt = Lident opened; _ }; _ }]];
    ]
    when String.equal name opened ->
      coverage ~name items
  | [
      {
        pstr_desc =
          Pstr_module
            {
              pmb_name = { txt = Some name; _ };
              pmb_expr = { pmod_desc = Pmod_structure items; _ };
              _;
            };
        _;
      };
    ] ->
      mutation ~name items
  | _ -> None

let rec elide acc = function
  | [] -> List.rev acc
  | line :: rest when String.equal line marker -> (
      let rec block items = function
        | [] -> None
        | l :: after when String.equal l marker -> Some (List.rev items, after)
        | l :: after -> block (l :: items) after
      in
      match block [] rest with
      | None -> List.rev_append acc (line :: rest)
      | Some (items, after) -> (
          let source = String.concat "\n" items in
          match
            condensed (Parse.implementation (Lexing.from_string source))
          with
          | Some rows -> elide (List.rev_append rows acc) after
          | None ->
              elide (List.rev_append ((line :: items) @ [ marker ]) acc) after))
  | line :: rest -> elide (line :: acc) rest

let () =
  let lines = String.split_on_char '\n' (In_channel.input_all stdin) in
  print_string (String.concat "\n" (elide [] lines))
