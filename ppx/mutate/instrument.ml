(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A guard replaces its site in place and its disarmed arm is the original
   expression, so the rewriter is a map that threads one context: whether an
   expression must be a boolean, and whether it is a condition. Every arm
   keeps the emission law of
   instrument.mli, because all the mutants share one binary and one ill-typed
   arm breaks the build of the whole project. *)

open Ppxlib
open Ast_builder.Default

(* Attributes *)

type directive = No_directive | Off of string | On | Exclude_file

(* The grammar is the [coverage] attribute's (ppx/coverage/instrument.ml). *)
let directive { attr_name; attr_payload; attr_loc } =
  let payload =
    match attr_payload with
    | PStr [ { pstr_desc = Pstr_eval (payload, _); _ } ] ->
        Some payload.pexp_desc
    | _ -> None
  in
  if not (String.equal attr_name.txt "mutate") then No_directive
  else
    match payload with
    | Some (Pexp_ident { txt = Lident "off"; _ }) -> Off ""
    | Some (Pexp_ident { txt = Lident "on"; _ }) -> On
    | Some (Pexp_ident { txt = Lident "exclude_file"; _ }) -> Exclude_file
    | Some
        (Pexp_apply
           ( { pexp_desc = Pexp_ident { txt = Lident "off"; _ }; _ },
             [
               ( Nolabel,
                 { pexp_desc = Pexp_constant (Pconst_string (reason, _, _)); _ }
               );
             ] )) ->
        Off reason
    | _ ->
        Location.raise_errorf ~loc:attr_loc "Bad payload in mutate attribute."

let err_misplaced attribute name =
  Location.raise_errorf ~loc:attribute.attr_loc "mutate %s is not allowed here."
    name

(* Every attribute is read, so a malformed one is an error beside an [off]
   too. *)
let off_reason attributes =
  List.fold_left
    (fun found attribute ->
      match directive attribute with
      | No_directive -> found
      | Off reason -> Some reason
      | On -> err_misplaced attribute "on"
      | Exclude_file -> err_misplaced attribute "exclude_file")
    None attributes

let is_off attributes = Option.is_some (off_reason attributes)

let excludes_file structure =
  List.exists
    (function
      | { pstr_desc = Pstr_attribute attribute; _ } -> (
          match directive attribute with
          | Exclude_file -> true
          | No_directive | Off _ | On -> false)
      | _ -> false)
    structure

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

(* Operators *)

type family = Ari | Cmp | Con

(* Each rewritten operator with its family, the name of its rewrite and the
   operator that replaces it. *)
let operators =
  [
    ("+", (Ari, "sub", "-"));
    ("-", (Ari, "add", "+"));
    ("+.", (Ari, "fsub", "-."));
    ("-.", (Ari, "fadd", "+."));
    ("<", (Cmp, "le", "<="));
    ("<=", (Cmp, "lt", "<"));
    (">", (Cmp, "ge", ">="));
    (">=", (Cmp, "gt", ">"));
    ("=", (Cmp, "neq", "<>"));
    ("<>", (Cmp, "eq", "="));
    ("&&", (Con, "or", "||"));
    ("||", (Con, "and", "&&"));
  ]

(* A family keeps its sites only where its operators are Stdlib's: [ari]
   names its replacement, [con] turns [&&] into an [if], and [cmp] swaps
   operands of one type. A binding that this pass sees loses the family of
   its name; what an [open] brings in is not seen. *)
let kept_families structure =
  let lose name lost =
    match List.assoc_opt name operators with
    | Some (family, _, _) -> family :: lost
    | None -> lost
  in
  let scan =
    object
      inherit [family list] Ast_traverse.fold as super

      method! pattern p lost =
        let lost =
          match p.ppat_desc with
          | Ppat_var { txt; _ } -> lose txt lost
          | _ -> lost
        in
        super#pattern p lost

      method! value_description vd lost =
        super#value_description vd (lose vd.pval_name.txt lost)
    end
  in
  let lost = scan#structure structure [] in
  fun family -> not (List.mem family lost)

(* Mutants *)

(* [Condition] is an [if] or [while] condition or an arm's guard, and
   [Boolean] a direct operand of [&&] or [||]. [cmp] fires in these two
   alone, where the expression must be a boolean, so that a user's comparison
   cannot give its two arms two types. *)
type context = Condition | Boolean | Ordinary

(* A bare operator applied to two unlabelled arguments. A qualified operator
   may be anything, and is no site. *)
type application = {
  name : string;
  operator : expression;
  left : expression;
  right : expression;
}

let application e =
  match e.pexp_desc with
  | Pexp_apply
      ( ({ pexp_desc = Pexp_ident { txt = Lident name; _ }; _ } as operator),
        [ (Nolabel, left); (Nolabel, right) ] ) ->
      Some { name; operator; left; right }
  | _ -> None

let applies name e =
  match application e with
  | Some app -> String.equal app.name name
  | None -> false

let is_connective ~keeps e = keeps Con && (applies "&&" e || applies "||" e)

(* [apply ~loc name args] applies the bare operator [name] to [args]. *)
let apply ~loc name args =
  pexp_apply ~loc
    (pexp_ident ~loc { txt = Lident name; loc })
    (List.map (fun arg -> (Nolabel, arg)) args)

(* A text is printed from the parsetree, since a preprocessor cannot rely on
   the path of its source, and each run of blanks becomes one space. *)
let render e =
  Pprintast.string_of_expression { e with pexp_attributes = [] }
  |> String.map (function '\t' | '\n' | '\r' -> ' ' | c -> c)
  |> String.split_on_char ' '
  |> List.filter (fun word -> word <> "")
  |> String.concat " "

(* How a guard arms its site. The emission law forbids naming a comparison's
   partner, so [cmp] negates the operator the source wrote: on a total order
   [a <= b] is [not (b < a)], so an ordering swaps its operands, and [a <> b]
   is [not (a = b)], so an equality does not. *)
type guard = Neg | Operator of application * operator_guard

and operator_guard =
  | Negate (* [cmp] on an equality *)
  | Swap (* [cmp] on an ordering *)
  | Branch (* [con] *)
  | Replace of string (* [ari], by this operator *)

type mutant = { guard : guard; rewrite : string; after : string }

(* A connective with a connective operand carries no mutant, since its guard
   would hold another site. *)
let mutant ~keeps context e =
  let loc = e.pexp_loc in
  let neg () =
    if context <> Condition then None
    else
      let negated = apply ~loc "not" [ { e with pexp_attributes = [] } ] in
      Some { guard = Neg; rewrite = "not"; after = render negated }
  in
  match application e with
  | None -> neg ()
  | Some app -> (
      match List.assoc_opt app.name operators with
      | Some (family, rewrite, replacement) when keeps family -> (
          let after = render (apply ~loc replacement [ app.left; app.right ]) in
          let mutant guard =
            Some { guard = Operator (app, guard); rewrite; after }
          in
          match family with
          | Con ->
              if is_connective ~keeps app.left || is_connective ~keeps app.right
              then None
              else mutant Branch
          | Cmp when context = Ordinary -> None
          | Cmp ->
              mutant
                (if app.name = "=" || app.name = "<>" then Negate else Swap)
          | Ari -> mutant (Replace replacement))
      | Some _ | None -> neg ())

(* Guards *)

(* A binder is site-indexed in a reserved namespace, so that a guard shadows
   neither a user's binding nor another guard's. *)
let binder index role = Printf.sprintf "__windtrap_mut_%d_%s" index role

(* The module is named after the file, so that each compilation unit calls its
   own. The coverage rewriter mangles its module in the same way. *)
let generated_module_name file =
  let buffer = Buffer.create (String.length file + 16) in
  Buffer.add_string buffer "Windtrap_mut___";
  String.iter
    (function
      | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_') as c ->
          Buffer.add_char buffer c
      | _ -> Buffer.add_string buffer "___")
    file;
  Buffer.contents buffer

let armed ~loc ~module_name index =
  pexp_apply ~loc
    (pexp_ident ~loc
       { txt = Ldot (Lident module_name, "___windtrap_armed___"); loc })
    [ (Nolabel, eint ~loc index) ]

(* [neg], and [cmp] on an equality. The value is bound once, so it is
   evaluated as often as before, and binding the whole comparison leaves its
   operands the order the compiler gives them. *)
let negation ~loc ~module_name ~index value =
  let p = binder index "p" in
  [%expr
    let [%p pvar ~loc p] = [%e value] in
    if [%e armed ~loc ~module_name index] then Stdlib.not [%e evar ~loc p]
    else [%e evar ~loc p]]

(* [cmp] on an ordering, and [ari]: [let l, r = left, right in if armed then
   ... else l op r], where the disarmed arm is the application of [e] rebuilt
   over the binders, with its location and its attributes.

   The type-checker reads the tuple left to right, as it reads the arguments
   of an application, so a qualified field of the left operand still resolves
   an unqualified field of the right one. The match compiler destructures the
   literal tuple without building it, into the [let] chain the application
   compiles to, right operand first: each operand is evaluated once, in the
   original order, with no allocation. A [let] chain written here would impose
   one order on both, and a right-to-left check breaks that disambiguation.

   An application also checks its right argument against the type of the left
   one, which resolves a constructor or a record literal whose name a later
   type declaration reuses. [~pin] restores that for an ordering by the
   annotation [_ operands], with [type 'a operands = 'a * 'a]. The swapped arm
   already needs one type for both operands, so the pin rejects nothing. A
   variable [: 'a] cannot say it: it is scoped to the whole toplevel phrase,
   so it would tie the phrase's sites together and keep a local
   [let lt x y = x < y] from generalizing. [ari] does not pin, since an
   [open]-provided [+] may take two types, and number types disambiguate
   nothing.

   An applied [(fun l r -> ...) left right] would keep its call and its
   closure in bytecode under [-g]. Binding the operator, as in [let op = if
   armed then ( <= ) else ( < )], names a second operator, and reaches the
   polymorphic comparison even when nothing is armed. *)
let binary ~loc ~module_name ~index ~pin e app ~armed_arm left right =
  let l = binder index "l" and r = binder index "r" in
  let operands =
    let tuple = pexp_tuple ~loc [ left; right ] in
    if not pin then tuple
    else
      pexp_constraint ~loc tuple
        (ptyp_constr ~loc
           { txt = Ldot (Lident module_name, "operands"); loc }
           [ ptyp_any ~loc ])
  in
  let disarmed =
    {
      (pexp_apply ~loc:e.pexp_loc app.operator
         [ (Nolabel, evar ~loc l); (Nolabel, evar ~loc r) ])
      with
      pexp_attributes = e.pexp_attributes;
    }
  in
  [%expr
    let [%p pvar ~loc l], [%p pvar ~loc r] = [%e operands] in
    if [%e armed ~loc ~module_name index] then
      [%e armed_arm (evar ~loc l) (evar ~loc r)]
    else [%e disarmed]]

(* [con] branches once on the armed flag, which keeps the short circuit, the
   tail position of [b] and one copy of each operand:

     a && b  ->  let p = a in
                 if Stdlib.( <> ) (p : Stdlib.Bool.t) (armed i) then b else p
     a || b  ->  let p = a in
                 if Stdlib.( = ) (p : Stdlib.Bool.t) (armed i) then b else p

   Armed, the first reads [if not p then b else p], which is [a || b]. The
   constraint makes the comparison an integer one, and it names
   [Stdlib.Bool.t] because a file may declare its own [bool]. No node of the
   guard is the original expression, so the whole guard takes its
   attributes. *)
let branch ~loc ~module_name ~index e app left right =
  let p = binder index "p" in
  let value = [%expr ([%e evar ~loc p] : Stdlib.Bool.t)] in
  let armed = armed ~loc ~module_name index in
  let test =
    if String.equal app.name "&&" then
      [%expr Stdlib.( <> ) [%e value] [%e armed]]
    else [%expr Stdlib.( = ) [%e value] [%e armed]]
  in
  {
    ([%expr
       let [%p pvar ~loc p] = [%e left] in
       if [%e test] then [%e right] else [%e evar ~loc p]])
    with
    pexp_attributes = e.pexp_attributes;
  }

(* Traversal *)

(* [lazy] of a trivial syntactic value compiles as already forced, and a
   guard under it would change that. Coverage's predicate, verbatim. No guard
   weakens the generalization of a binding, since every site is an
   application or a condition, which is never a syntactic value. *)
let rec is_trivial_syntactic_value e =
  match e.pexp_desc with
  | Pexp_function _ | Pexp_poly _ | Pexp_ident _ | Pexp_constant _
  | Pexp_construct (_, None) ->
      true
  | Pexp_constraint (inner, _) | Pexp_coerce (inner, _, _) ->
      is_trivial_syntactic_value inner
  | _ -> false

(* Nothing under an opaque expression is rewritten. The type-checker gives
   [assert false] every type only as written. *)
let is_opaque e =
  match e.pexp_desc with
  | Pexp_assert _ -> true
  | Pexp_lazy body -> is_trivial_syntactic_value body
  | _ -> false

type site = {
  line : int;
  col : int;
  rewrite : string;
  before : string;
  after : string;
  dismissed : string option;
}

class instrumenter ~keeps ~module_name =
  object (self)
    inherit Ast_traverse.map as super

    (* The identifier of every recorded site, so its length is the next
       index. *)
    val seen = Hashtbl.create 64
    val mutable sites = [] (* newest first *)
    val mutable pins = false (* a guard names [operands] *)
    val mutable suppressed = false (* inside a [[@@@mutate off]] region *)
    method sites = List.rev sites
    method pins = pins

    (* [site e mutant ~dismissed] records the site of [e] and is its index.
       It is [None], with nothing recorded, for generated code and for a site
       whose identifier another one has, as a deriver's copy does. *)
    method private site e (mutant : mutant) ~dismissed =
      let start = e.pexp_loc.loc_start in
      let line = start.pos_lnum and col = start.pos_cnum - start.pos_bol in
      let key = (line, col, mutant.rewrite) in
      if e.pexp_loc.loc_ghost || Hashtbl.mem seen key then None
      else begin
        let index = Hashtbl.length seen in
        Hashtbl.add seen key ();
        let site =
          {
            line;
            col;
            rewrite = mutant.rewrite;
            before = render e;
            after = mutant.after;
            dismissed;
          }
        in
        sites <- site :: sites;
        Some index
      end

    (* [chained] marks the left operand of a site that applies the same
       operator: a chain of one operator carries one mutant, and reading it
       from the tree keeps a bracket from changing the count. A dismissal
       leaves [e] as written. *)
    method private mutate ?(chained = false) context e =
      let off = off_reason e.pexp_attributes in
      if is_opaque e then e
      else
        match (off, mutant ~keeps context e) with
        | Some _, None -> e
        | Some reason, Some mutant ->
            if not chained then
              ignore (self#site e mutant ~dismissed:(Some reason));
            e
        | None, None -> self#descend e
        | None, Some mutant ->
            let index =
              if chained then None else self#site e mutant ~dismissed:None
            in
            self#guard e mutant index

    method private guard e mutant index =
      let loc = { e.pexp_loc with loc_ghost = true } in
      match mutant.guard with
      | Neg -> (
          let value = self#descend e in
          match index with
          | None -> value
          | Some index -> negation ~loc ~module_name ~index value)
      | Operator (app, guard) -> (
          let context =
            match guard with
            | Branch -> Boolean
            | Negate | Swap | Replace _ -> Ordinary
          in
          let chained = applies app.name app.left in
          let left = self#mutate ~chained context app.left in
          let right = self#mutate context app.right in
          let rebuilt =
            {
              e with
              pexp_desc =
                Pexp_apply (app.operator, [ (Nolabel, left); (Nolabel, right) ]);
            }
          in
          match index with
          | None -> rebuilt
          | Some index -> (
              match guard with
              | Negate -> negation ~loc ~module_name ~index rebuilt
              | Swap ->
                  pins <- true;
                  binary ~loc ~module_name ~index ~pin:true e app left right
                    ~armed_arm:(fun l r ->
                      let swapped =
                        pexp_apply ~loc app.operator
                          [ (Nolabel, r); (Nolabel, l) ]
                      in
                      [%expr Stdlib.not [%e swapped]])
              | Branch -> branch ~loc ~module_name ~index e app left right
              | Replace replacement ->
                  binary ~loc ~module_name ~index ~pin:false e app left right
                    ~armed_arm:(fun l r -> apply ~loc replacement [ l; r ])))

    (* [descend e] rewrites the children of [e], each in the context that [e]
       gives it. A connective that carries no mutant still has boolean
       operands. *)
    method private descend e =
      match e.pexp_desc with
      | Pexp_ifthenelse (condition, then_, else_) ->
          {
            e with
            pexp_desc =
              Pexp_ifthenelse
                ( self#mutate Condition condition,
                  self#expression then_,
                  Option.map self#expression else_ );
          }
      | Pexp_while (condition, body) ->
          {
            e with
            pexp_desc =
              Pexp_while (self#mutate Condition condition, self#expression body);
          }
      | Pexp_apply (operator, [ (Nolabel, left); (Nolabel, right) ])
        when is_connective ~keeps e ->
          {
            e with
            pexp_desc =
              Pexp_apply
                ( operator,
                  [
                    (Nolabel, self#mutate Boolean left);
                    (Nolabel, self#mutate Boolean right);
                  ] );
          }
      | _ -> super#expression e

    (* Inside a region, every structure item but a value binding reaches its
       expressions through here. *)
    method! expression e = if suppressed then e else self#mutate Ordinary e

    method! case c =
      {
        pc_lhs = self#pattern c.pc_lhs;
        pc_guard = Option.map (self#mutate Condition) c.pc_guard;
        pc_rhs = self#expression c.pc_rhs;
      }

    method! module_binding mb =
      if is_off mb.pmb_attributes then mb else super#module_binding mb

    method! structure_item si =
      match si.pstr_desc with
      | Pstr_attribute attribute ->
          (match directive attribute with
          | No_directive -> ()
          | Off _ ->
              if suppressed then
                Location.raise_errorf ~loc:attribute.attr_loc
                  "Mutation is already off.";
              suppressed <- true
          | On ->
              if not suppressed then
                Location.raise_errorf ~loc:attribute.attr_loc
                  "Mutation is already on.";
              suppressed <- false
          | Exclude_file -> err_misplaced attribute "exclude_file");
          si
      | Pstr_value (rec_flag, bindings) when not suppressed ->
          let binding b =
            if is_off b.pvb_attributes then b
            else { b with pvb_expr = self#expression b.pvb_expr }
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

    method! extension extension = extension
    method! attribute attribute = attribute
  end

(* The generated module *)

(* The module re-exports the runtime's [site] type, so that the table names
   its fields without type-directed disambiguation: the runtime declares these
   labels on three records, and warning 42 is fatal under [-w +a -warn-error
   +a]. The type equation turns a runtime record that drifted into a compile
   error. Because of its labels the module is never opened, and a file whose
   every site is dismissed has no unused [open].

   [register] allocates the arrays of the file, so indices are file-local, and
   the stop comments hide the module from odoc. The module stands above the
   user's code, so it names [int], [option] and [Some] unqualified, which a
   guard cannot. *)
let runtime_initialization ~file ~module_name ~sites ~pins =
  let loc = { (Location.in_file file) with loc_ghost = true } in
  let site { line; col; rewrite; before; after; dismissed } =
    let dismissed =
      match dismissed with
      | None -> [%expr None]
      | Some reason -> [%expr Some [%e estring ~loc reason]]
    in
    [%expr
      {
        line = [%e eint ~loc line];
        col = [%e eint ~loc col];
        rewrite = [%e estring ~loc rewrite];
        before = [%e estring ~loc before];
        after = [%e estring ~loc after];
        dismissed = [%e dismissed];
      }]
  in
  let site_type =
    [%stri
      type site = Windtrap_runtime.Mutate.site = {
        line : int;
        col : int;
        rewrite : string;
        before : string;
        after : string;
        dismissed : string option;
      }]
  in
  let operands = [%stri type 'a operands = 'a * 'a] in
  let armed =
    [%stri
      let ___windtrap_armed___ =
        Windtrap_runtime.Mutate.register ~file:[%e estring ~loc file]
          ~sites:[%e pexp_array ~loc (List.map site sites)]]
  in
  let items =
    if pins then [ site_type; operands; armed ] else [ site_type; armed ]
  in
  let generated =
    pstr_module ~loc
      (module_binding ~loc
         ~name:{ txt = Some module_name; loc }
         ~expr:(pmod_structure ~loc items))
  in
  let stop_comment = [%stri [@@@ocaml.text "/*"]] in
  [ stop_comment; generated; stop_comment ]

(* Rewriting *)

(* The ignore lists are the coverage rewriter's. *)
let always_ignore_paths = [ "//toplevel//"; "(stdin)" ]
let always_ignore_basenames = [ ".ocamlinit"; "topfind" ]

let transform_impl_file ctxt ast =
  let file = Expansion_context.Base.input_name ctxt in
  if
    List.mem file always_ignore_paths
    || List.mem (Filename.basename file) always_ignore_basenames
    || excludes_file ast
  then ast
  else
    let module_name = generated_module_name file in
    let instrumenter =
      new instrumenter ~keeps:(kept_families ast) ~module_name
    in
    let instrumented = instrumenter#structure ast in
    match instrumenter#sites with
    | [] -> ast
    | sites ->
        runtime_initialization ~file ~module_name ~sites ~pins:instrumenter#pins
        @ instrumented
