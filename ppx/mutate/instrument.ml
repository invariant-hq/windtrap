(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The mutation instrumenter. Every mutant of a file compiles into the
   same binary behind a runtime guard, so a single ill-typed arm is not
   one bad mutant - it is a broken build for the whole project. That is
   the constraint this file is shaped by:

     EMISSION LAW. Every arm of a guard must be well-typed without type
     information, and must mention only identifiers already present in
     the original expression, plus [Stdlib]-qualified names.

   Consequences, each implemented below:

   - [cmp] negates the operator the source already wrote instead of
     naming its partner ([comparison_rewrite]), and fires only where the
     expression is syntactically obliged to be [bool] (the [`Cond] and
     [`Bool] contexts), so a user-shadowed comparison cannot make the two
     arms disagree in type.
   - [con] uses the branch-on-the-armed-flag encoding ([con_guard]), the
     only shape that keeps [&&]/[||]'s short-circuiting without
     duplicating an operand.
   - [ari] is the one admitted exception - [a + b -> a - b] cannot be
     written without naming [-] - and is guarded by skipping the
     operators a file visibly rebinds ([capabilities_of_structure]).

   The engine is a plain [Ast_traverse.map]. Coverage's instrumenter
   needs tail-position and successor analysis because it wraps
   application out-edges; mutation replaces an expression in place with a
   guard whose disarmed arm is the original expression, so it needs
   neither. The only context threaded is one three-valued flag saying
   whether the expression under the cursor is syntactically obliged to be
   a boolean, and whether it is a condition. *)

open Ppxlib
open Ast_builder.Default

(* The [mutate] attributes, mirroring the coverage attribute grammar
   exactly - [[@mutate off]] on an expression, [[@@mutate off]] on a
   value or module binding, [[@@@mutate off]]/[[@@@mutate on]] around a
   region, [[@@@mutate exclude_file]] for a file - plus an optional
   reason string, which lands in the site table's [dismissed] field so a
   dismissal is reviewable rather than merely obeyed. *)

type directive = No_directive | Off of string | On | Exclude_file

let recognize_mutate_attribute { attr_name; attr_payload; attr_loc } =
  if not (String.equal attr_name.txt "mutate") then No_directive
  else
    let bad () =
      Location.raise_errorf ~loc:attr_loc "Bad payload in mutate attribute."
    in
    match attr_payload with
    | PStr [ { pstr_desc = Pstr_eval (payload, _); _ } ] -> (
        match payload.pexp_desc with
        | Pexp_ident { txt = Lident "off"; _ } -> Off ""
        | Pexp_ident { txt = Lident "on"; _ } -> On
        | Pexp_ident { txt = Lident "exclude_file"; _ } -> Exclude_file
        | Pexp_apply
            ( { pexp_desc = Pexp_ident { txt = Lident "off"; _ }; _ },
              [
                ( Nolabel,
                  {
                    pexp_desc = Pexp_constant (Pconst_string (reason, _, _));
                    _;
                  } );
              ] ) ->
            Off reason
        | _ -> bad ())
    | _ -> bad ()

(* [off_reason attrs] is [Some reason] when [attrs] carries
   [[@mutate off]]; [reason] is [""] when none was given. Folds rather
   than short-circuits so every attribute is error-checked. *)
let off_reason attributes =
  List.fold_left
    (fun found attribute ->
      match recognize_mutate_attribute attribute with
      | No_directive -> found
      | Off reason -> Some reason
      | On ->
          Location.raise_errorf ~loc:attribute.attr_loc
            "mutate on is not allowed here."
      | Exclude_file ->
          Location.raise_errorf ~loc:attribute.attr_loc
            "mutate exclude_file is not allowed here.")
    None attributes

let has_off_attribute attributes = off_reason attributes <> None

let has_exclude_file_attribute structure =
  List.exists
    (function
      | { pstr_desc = Pstr_attribute attribute; _ } -> (
          match recognize_mutate_attribute attribute with
          | Exclude_file -> true
          | No_directive | Off _ | On -> false)
      | _ -> false)
    structure

(* File-level exclusions *)

(* A file that declares inline tests is test code, and test code is not
   the population under test. Both spellings are recognized: the
   extension nodes themselves - this instrumenter can run in a driver
   that does not link ppx_windtrap, and the golden tests are such a
   driver - and the calls ppx_windtrap expands them into, which is what a
   real build sees, because instrumentation runs after every other
   rewriter. *)
let file_declares_inline_tests structure =
  let found = ref false in
  let scan =
    object
      inherit Ast_traverse.iter as super

      method! extension ((name, _) as ext) =
        (match name.txt with
        | "test" | "expect_test" -> found := true
        | _ -> ());
        super#extension ext

      method! longident lid =
        (match lid with
        | Ldot (Ldot (Lident "Ppx_windtrap_runtime", "Ppx_runtime"), _) ->
            found := true
        | _ -> ());
        super#longident lid
    end
  in
  scan#structure structure;
  !found

(* Which operator families the file may be instrumented for. [ari] emits
   [-] where the source wrote [+], so a file that gives [+] another
   meaning gets no [ari] sites; [con] rewrites [&&] into an [if], which
   is meaning-preserving only for Stdlib's [&&], and its operands are
   boolean positions only for Stdlib's; [cmp]'s operand swap assumes the
   comparison is symmetric in its argument types. A binding this pass can
   see is skipped; what an [open] brings in it cannot see, and that
   residue is documented rather than solved - the remedy is one compile
   error at the user's own source location and
   [[@@@mutate exclude_file]]. *)

let arithmetic_operators = [ "+"; "-"; "+."; "-." ]
let comparison_operators = [ "<"; "<="; ">"; ">="; "="; "<>" ]
let connective_operators = [ "&&"; "||" ]

type capabilities = { ari : bool; cmp : bool; con : bool }

let capabilities_of_structure structure =
  let rebound = Hashtbl.create 8 in
  let watched name =
    List.mem name arithmetic_operators
    || List.mem name comparison_operators
    || List.mem name connective_operators
  in
  let note name = if watched name then Hashtbl.replace rebound name () in
  let scan =
    object
      inherit Ast_traverse.iter as super

      method! pattern p =
        (match p.ppat_desc with Ppat_var { txt; _ } -> note txt | _ -> ());
        super#pattern p

      method! value_description vd =
        note vd.pval_name.txt;
        super#value_description vd
    end
  in
  scan#structure structure;
  let free ops = not (List.exists (Hashtbl.mem rebound) ops) in
  {
    ari = free arithmetic_operators;
    cmp = free comparison_operators;
    con = free connective_operators;
  }

(* Sites *)

(* One entry of the file's site table: [line] and [col] locate the
   mutated expression, [before] and [after] are its renderings for the
   report. *)
type site = {
  line : int;
  col : int;
  rewrite : string;
  before : string;
  after : string;
  dismissed : string option;
}

type state = {
  mutable rev_sites : site list; (* most recently allocated first *)
  mutable count : int;
  seen : (int * int * string, unit) Hashtbl.t;
  chained : (int * int, unit) Hashtbl.t;
      (* Byte extents of nodes suppressed by the chain rule below, keyed
         by extent because that is what identifies a node regardless of
         how the parser located it. *)
}

let create_state () =
  {
    rev_sites = [];
    count = 0;
    seen = Hashtbl.create 64;
    chained = Hashtbl.create 16;
  }

(* [suppress_chain st name left] records [left] as chained when it is
   itself an application of the operator [name] - that is, when [left] is
   the inner node of a left-associative chain of one operator, as [a + b]
   is in [a + b + c].

   A chain of n operators of one family carries one mutant, not n-1, and
   the outermost is the one kept: the traversal is top-down, so the outer
   node allocates its site before this suppresses the inner one, and the
   inner node in turn suppresses its own left operand, so the rule is
   transitive along the whole chain.

   Detecting the chain STRUCTURALLY, from the operator, is what makes the
   population independent of layout. Keying it on the line and column
   would happen to work for a bare chain - every node of [a + b + c]
   starts at [a]'s byte - but OCaml's parser gives a parenthesized
   expression a location that starts at its [(], so [f (a + b + c)] would
   carry two mutants where [a + b + c] carries one. Parenthesizing an
   expression must not change how many mutants it carries; a score whose
   denominator moves when brackets are added is not a score. Keying it on
   extent containment instead would be layout-independent but far too
   broad: it would swallow a genuinely distinct inner site, such as the
   [neg] on [a] inside the [neg] on [a && b]. *)
let suppress_chain st name left =
  match left.pexp_desc with
  | Pexp_apply
      ( { pexp_desc = Pexp_ident { txt = Lident inner; _ }; _ },
        [ (Nolabel, _); (Nolabel, _) ] )
    when String.equal inner name ->
      Hashtbl.replace st.chained
        (left.pexp_loc.loc_start.pos_cnum, left.pexp_loc.loc_end.pos_cnum)
        ()
  | _ -> ()

(* [add_site st ~loc …] records a site, and is [Some index] when a guard
   must be emitted for it.

   It is [None] - and, for the first two reasons, no table entry is made
   either - when:

   - the attribution location is a ghost one, which means generated code
     no user can act on;
   - another site of this file already claims this line, column and
     rewrite. [<file>:<line>:<col>:<rewrite>] must name at most one site,
     and rewriters such as [[@@deriving]] duplicate non-ghost locations,
     so a later collider is dropped rather than instrumented;
   - the node was suppressed by the chain rule ([suppress_chain]);
   - the expression carries [[@mutate off]]. The site is catalogued, so
     [report] mode can list the dismissal with its reason, but the
     expression is left exactly as written: a dismissal that still
     rewrote the code would be a dismissal in name only. *)
let add_site st ~(loc : Location.t) ~rewrite ~before ~after ~dismissed =
  if loc.loc_ghost then None
  else begin
    let line = loc.loc_start.pos_lnum
    and col = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
    let extent = (loc.loc_start.pos_cnum, loc.loc_end.pos_cnum) in
    let key = (line, col, rewrite) in
    if Hashtbl.mem st.chained extent || Hashtbl.mem st.seen key then None
    else begin
      Hashtbl.add st.seen key ();
      let index = st.count in
      st.rev_sites <- { line; col; rewrite; before; after; dismissed }
                      :: st.rev_sites;
      st.count <- index + 1;
      match dismissed with Some _ -> None | None -> Some index
    end
  end

(* The operator tables. [rewrite] names the REPLACEMENT, never the
   original.

   [cmp] is the place this file is easiest to get quietly wrong. The
   emission law forbids naming the partner operator, so each rewrite is
   expressed by negating the operator the source already wrote. For a
   totally ordered type the four ordering identities all SWAP their
   operands:

     a <= b  =  not (b <  a)      a <  b  =  not (b <= a)
     a >= b  =  not (b >  a)      a >  b  =  not (b >= a)

   and the two equality identities do NOT:

     a <> b  =  not (a =  b)      a =  b  =  not (a <> b)

   The third component of [comparison_rewrite] records which shape
   applies. *)
let comparison_rewrite = function
  | "<" -> Some ("le", "<=", true)
  | "<=" -> Some ("lt", "<", true)
  | ">" -> Some ("ge", ">=", true)
  | ">=" -> Some ("gt", ">", true)
  | "=" -> Some ("neq", "<>", false)
  | "<>" -> Some ("eq", "=", false)
  | _ -> None

let arithmetic_rewrite = function
  | "+" -> Some ("sub", "-")
  | "-" -> Some ("add", "+")
  | "+." -> Some ("fsub", "-.")
  | "-." -> Some ("fadd", "+.")
  | _ -> None

let connective_rewrite = function
  | "&&" -> Some ("or", "||")
  | "||" -> Some ("and", "&&")
  | _ -> None

(* [binary e] is [Some (name, operator, left, right)] when [e] applies a
   bare operator identifier to two unlabelled arguments. Qualified
   spellings ([Float.( < )]) and labelled or partial applications are not
   sites: the rewrite vocabulary names bare operators, and a qualified
   one may be anything at all. *)
let binary expression =
  match expression.pexp_desc with
  | Pexp_apply
      ( ({ pexp_desc = Pexp_ident { txt = Lident name; _ }; _ } as operator),
        [ (Nolabel, left); (Nolabel, right) ] ) ->
      Some (name, operator, left, right)
  | _ -> None

let is_connective_apply capabilities expression =
  capabilities.con
  &&
  match binary expression with
  | Some (name, _, _, _) -> connective_rewrite name <> None
  | None -> false

(* Renderings. The source text is never read: a preprocessor's working
   directory under sandboxing is not what one expects, and a catalogue
   that is a literal in the code it describes cannot be stale. Both
   strings are therefore printed from the parsetree, with whitespace runs
   collapsed so that a rendering is one line - including runs inside
   string literals, which is the one place a rendering differs from the
   source in more than layout. *)
let render expression =
  let text =
    Pprintast.string_of_expression { expression with pexp_attributes = [] }
  in
  let buffer = Buffer.create (String.length text) in
  let pending = ref false and started = ref false in
  String.iter
    (fun c ->
      match c with
      | ' ' | '\t' | '\n' | '\r' -> if !started then pending := true
      | c ->
          if !pending then Buffer.add_char buffer ' ';
          pending := false;
          started := true;
          Buffer.add_char buffer c)
    text;
  Buffer.contents buffer

let render_binary ~loc replacement left right =
  render
    (pexp_apply ~loc
       (pexp_ident ~loc { txt = Lident replacement; loc })
       [ (Nolabel, left); (Nolabel, right) ])

let render_negation ~loc expression =
  render
    (pexp_apply ~loc
       (pexp_ident ~loc { txt = Lident "not"; loc })
       [ (Nolabel, { expression with pexp_attributes = [] }) ])

(* Guards *)

(* Rule 4: every binder the instrumenter introduces is site-indexed and
   lives in a reserved namespace. A short name would shadow a user
   binding of the same name for the extent of the guarded operand, and
   nested guards would shadow each other. *)
let binder index role = Printf.sprintf "__windtrap_mut_%d_%s" index role

(* The generated module is named after the file so that each compilation
   unit calls its own guard - two files could otherwise collide when one
   includes another. It is referenced qualified rather than opened: it
   must declare the site record's type to name that record's fields
   without type-directed disambiguation (see [runtime_initialization]),
   and opening a module that carries a record type would put labels named
   [line], [col], [before] and [after] into the user's scope.
   The mangling itself mirrors coverage's (Windtrap_cov___, in
   ppx/coverage/instrument.ml's [runtime_initialization]); keep the two
   in sync. *)
let generated_module_name ~file =
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

let stdlib ~loc name =
  pexp_ident ~loc { txt = Ldot (Lident "Stdlib", name); loc }

(* [neg], and [cmp]'s self-negating form for [=] and [<>]: one shape, and
   the same one, because both negate a boolean the source already wrote.
   The value is bound once so that it is evaluated exactly as often as
   before; [=] and [<>] bind the whole comparison rather than its two
   operands, since their identities do not swap, which leaves the
   operands the evaluation order the compiler chose for them instead of
   fixing it here. [Stdlib.not] rather than [not]: the emission law
   admits identifiers of the original expression plus Stdlib-qualified
   names, and nothing else. *)
let negating_guard ~loc ~module_name ~index value =
  let p = binder index "p" in
  [%expr
    let [%p pvar ~loc p] = [%e value] in
    if [%e armed ~loc ~module_name index] then Stdlib.not [%e evar ~loc p]
    else [%e evar ~loc p]]

(* [cmp]'s swapping form and [ari], which are one skeleton. The operands are lifted through ONE tuple
   binding, [let (l, r) = (left, right)], and both arms apply the
   operator the source wrote - the armed one to the swapped operands,
   under [Stdlib.not].

   The tuple, not a chain of [let]s, is load-bearing on both sides of
   the type-checker, because the two sides read it in opposite orders
   and both orders matter:

   - CHECKING is left to right, component by component - the order the
     original application's arguments were checked in. That is what
     preserves the user's typing context: type-directed record
     disambiguation lets one qualified access ([rect.Layout.x]) teach
     the checker the type a later unqualified field of the same record
     ([rect.height]) resolves by, and only source order keeps the
     teaching operand ahead of the taught one. A chain of [let]s must
     pick ONE order for both checking and evaluation, and the
     right-to-left chain this replaced chose evaluation - real code
     stopped compiling with "Unbound record field" under
     instrumentation.

   - COMPILING destructures a literal tuple without ever building it:
     the match compiler emits exactly the [let]-chain this shape used to
     spell out, right operand bound first. So the guard still evaluates
     each operand exactly once, in the order the compiler gives the
     uninstrumented application, still allocates nothing, and Law 16(a)
     holds bit for bit; test/mutate_ppx/semantics/ checks all of it
     against an uninstrumented twin. *)
let binary_guard ~loc ~module_name ~index ~operator ~attrs ~original_loc
    ~armed_arm left right =
  let l = binder index "l" and r = binder index "r" in
  let disarmed =
    {
      (pexp_apply ~loc:original_loc operator
         [ (Nolabel, evar ~loc l); (Nolabel, evar ~loc r) ])
      with
      pexp_attributes = attrs;
    }
  in
  [%expr
    let [%p pvar ~loc l], [%p pvar ~loc r] = ([%e left], [%e right]) in
    if [%e armed ~loc ~module_name index] then
      [%e armed_arm ~l:(evar ~loc l) ~r:(evar ~loc r)]
    else [%e disarmed]]

(* [cmp], swapping form: the armed arm applies the operator the source
   wrote to the swapped operands, under [Stdlib.not]. *)
let cmp_guard_swapped ~loc ~module_name ~index ~operator ~attrs ~original_loc
    left right =
  binary_guard ~loc ~module_name ~index ~operator ~attrs ~original_loc
    ~armed_arm:(fun ~l ~r ->
      [%expr
        Stdlib.not
          [%e pexp_apply ~loc operator [ (Nolabel, r); (Nolabel, l) ]]])
    left right

(* [con]. [&&] and [||] cannot be lifted to values without losing
   short-circuiting, and branching on the armed flag around the whole
   expression duplicates both operands - exponential under nesting. But
   the two connectives differ only in their short-circuit value, so one
   branch expresses both:

     a && b  ->  let p = a in
                 if Stdlib.( <> ) (p : Stdlib.Bool.t) (armed i) then b else p
     a || b  ->  let p = a in
                 if Stdlib.( =  ) (p : Stdlib.Bool.t) (armed i) then b else p

   Disarmed, [armed i] is [false] and the first reads [if p then b else
   p], which is [a && b]; armed it reads [if not p then b else p], which
   is [a || b]. [b] appears once, stays in tail position, is evaluated on
   exactly the original schedule, and nothing is allocated.

   Both decorations are load-bearing. The constraint makes the compiler
   specialize the comparison to an integer compare instead of calling
   [caml_notequal] on the path the whole program runs. [Stdlib.] answers
   the emission law against a user-shadowed [( = )].

   The constraint is spelled [Stdlib.Bool.t] and NOT [bool], for the same
   reason the operator is qualified: [bool] is an ordinary type name and
   a file containing [type bool = ...] would fail to compile every [con]
   guard in it - a whole-project build break, which is exactly what the
   emission law exists to prevent. There is no [Stdlib.bool] (the
   predefined types are not re-exported from [Stdlib]), so [Bool.t] is
   the qualified spelling. *)
let con_guard ~loc ~module_name ~index ~is_and left right =
  let p = binder index "p" in
  let test =
    pexp_apply ~loc
      (stdlib ~loc (if is_and then "<>" else "="))
      [
        (Nolabel, pexp_constraint ~loc (evar ~loc p) [%type: Stdlib.Bool.t]);
        (Nolabel, armed ~loc ~module_name index);
      ]
  in
  [%expr
    let [%p pvar ~loc p] = [%e left] in
    if [%e test] then [%e right] else [%e evar ~loc p]]

(* [ari]. The one operator whose well-typedness is not structural: the
   armed arm names an operator the source did not write, which is why
   [capabilities.ari] must hold for it to be emitted at all. *)
let ari_guard ~loc ~module_name ~index ~operator ~replacement ~attrs
    ~original_loc left right =
  binary_guard ~loc ~module_name ~index ~operator ~attrs ~original_loc
    ~armed_arm:(fun ~l ~r ->
      pexp_apply ~loc
        (pexp_ident ~loc { txt = Lident replacement; loc })
        [ (Nolabel, l); (Nolabel, r) ])
    left right

(* [lazy] applied to a trivial syntactic value compiles as already
   forced, so a guard under such a [lazy] would change the compilation of
   the [lazy] itself. Coverage's predicate, verbatim; the subtree is left
   alone. *)
let rec is_trivial_syntactic_value e =
  match e.pexp_desc with
  | Pexp_function _ | Pexp_poly _ | Pexp_ident _ | Pexp_constant _
  | Pexp_construct (_, None) ->
      true
  | Pexp_constraint (inner, _) | Pexp_coerce (inner, _, _) ->
      is_trivial_syntactic_value inner
  | _ -> false

(* Traversal *)

(* The context an expression is looked at in:

   - [`Cond] - an [if] or [while] condition, or a [when] guard. [cmp],
     [con] and [neg] can all fire here; placement rule 1 gives [cmp] and
     [con] priority, so [neg] fires only on a condition that is neither.
   - [`Bool] - a direct operand of [&&] or [||]. Syntactically obliged to
     be a boolean, so [cmp] may fire; [neg] may not, because negating a
     connective's operand is not one of this slice's operators.
   - [`Ordinary] - everywhere else. Only [con] and [ari] fire, both of
     which are well-typed with no assumption about the context. *)
type context = [ `Cond | `Bool | `Ordinary ]

(* What guard, if any, [e] carries in a given context. Computed from the
   expression as written, so that the guard emitter and the dismissal
   recorder answer the same question. *)
(* A binary application as [binary] destructured it: the operator's name,
   the operator expression, and the two operands. Carried on the shape so
   that the guard emitter destructures nothing a second time — the four
   impossible-case handlers that cost, one per shape, are what a shape
   that forgets its own operands buys. *)
type application = {
  name : string;
  operator : expression;
  left : expression;
  right : expression;
}

type shape =
  | Neg
  | Cmp_swapped of application
  | Cmp_direct of application
  | Con of application (* [name] is ["&&"] or ["||"] *)
  | Ari of application * string (* the replacement operator's name *)

let shape_of capabilities (context : context) e =
  let loc = e.pexp_loc in
  match e.pexp_desc with
  | Pexp_assert _ -> None
  | Pexp_lazy body when is_trivial_syntactic_value body -> None
  | _ -> (
      match binary e with
      | Some (name, operator, left, right) -> (
          match
            ( connective_rewrite name,
              comparison_rewrite name,
              arithmetic_rewrite name )
          with
          | Some (rewrite, replacement), _, _ when capabilities.con ->
              (* Placement rule 2: no guard whose expansion duplicates
                 another mutation site. In [a && b && c] only the inner
                 connective is mutated. *)
              if
                is_connective_apply capabilities left
                || is_connective_apply capabilities right
              then None
              else
                Some
                  ( Con { name; operator; left; right },
                    rewrite,
                    render_binary ~loc replacement left right )
          | _, Some (rewrite, replacement, swap), _
            when capabilities.cmp && context <> `Ordinary ->
              let site = { name; operator; left; right } in
              Some
                ( (if swap then Cmp_swapped site else Cmp_direct site),
                  rewrite,
                  render_binary ~loc replacement left right )
          | _, _, Some (rewrite, replacement) when capabilities.ari ->
              Some
                ( Ari ({ name; operator; left; right }, replacement),
                  rewrite,
                  render_binary ~loc replacement left right )
          | _ ->
              if context = `Cond then Some (Neg, "not", render_negation ~loc e)
              else None)
      | None ->
          if context = `Cond then Some (Neg, "not", render_negation ~loc e)
          else None)

class instrumenter st capabilities module_name =
  object (self)
    inherit Ast_traverse.map as super

    (* Set by [[@@@mutate off]], cleared by [[@@@mutate on]]; nested
       structures inherit the flag and restore it on exit. *)
    val mutable suppressed = false

    method private mutate (context : context) e =
      match off_reason e.pexp_attributes with
      | Some reason ->
          (* [[@mutate off]] leaves the expression exactly as written -
             including everything inside it - and records what was
             dismissed, with its reason, for [report] mode. *)
          (match shape_of capabilities context e with
          | Some (_, rewrite, after) ->
              ignore
                (add_site st ~loc:e.pexp_loc ~rewrite ~before:(render e) ~after
                   ~dismissed:(Some reason))
          | None -> ());
          e
      | None -> (
          match shape_of capabilities context e with
          | None -> self#descend e
          | Some (shape, rewrite, after) ->
              let index =
                add_site st ~loc:e.pexp_loc ~rewrite ~before:(render e) ~after
                  ~dismissed:None
              in
              self#guard shape index e)

    (* Builds one guard around [e]'s traversed children. [index] is
       [None] when the site was dropped (a ghost location, or a position
       another site already claims), in which case the children are still
       traversed and the expression rebuilt unchanged. *)
    method private guard shape index e =
      let loc = { e.pexp_loc with loc_ghost = true } in
      let original_loc = e.pexp_loc in
      let attrs = e.pexp_attributes in
      let rebuild operator left right =
        {
          e with
          pexp_desc =
            Pexp_apply (operator, [ (Nolabel, left); (Nolabel, right) ]);
        }
      in
      match shape with
      | Neg -> (
          let inner = self#descend e in
          match index with
          | Some index -> negating_guard ~loc ~module_name ~index inner
          | None -> inner)
      | Con { name; operator; left; right } -> (
          suppress_chain st name left;
          let left = self#mutate `Bool left
          and right = self#mutate `Bool right in
          match index with
          | Some index ->
              (* The [con] guard is the one shape in which no single
                 generated node is the original expression, so the
                 original's attributes go on the outermost node rather
                 than on an arm. Every other shape has a disarmed arm to
                 carry them. *)
              {
                (con_guard ~loc ~module_name ~index
                   ~is_and:(String.equal name "&&") left right)
                with
                pexp_attributes = attrs;
              }
          | None -> rebuild operator left right)
      | Cmp_direct { operator; left; right; _ } -> (
          let left = self#mutate `Ordinary left
          and right = self#mutate `Ordinary right in
          let comparison = rebuild operator left right in
          match index with
          | Some index -> negating_guard ~loc ~module_name ~index comparison
          | None -> comparison)
      | Cmp_swapped { operator; left; right; _ } -> (
          let left = self#mutate `Ordinary left
          and right = self#mutate `Ordinary right in
          match index with
          | Some index ->
              cmp_guard_swapped ~loc ~module_name ~index ~operator ~attrs
                ~original_loc left right
          | None -> rebuild operator left right)
      | Ari ({ name; operator; left; right }, replacement) -> (
          suppress_chain st name left;
          let left = self#mutate `Ordinary left
          and right = self#mutate `Ordinary right in
          match index with
          | Some index ->
              ari_guard ~loc ~module_name ~index ~operator ~replacement ~attrs
                ~original_loc left right
          | None -> rebuild operator left right)

    (* Traverses [e]'s children without guarding [e] itself. The forms
       that create a context are handled here; everything else goes
       through the generic map, whose children reach [expression] and so
       are traversed in [`Ordinary] context. *)
    method private descend e =
      match e.pexp_desc with
      (* [assert] is rewritten to a polymorphic raise, so mutating
         anything under it breaks typing in exactly the arms where it
         appears. The whole subtree is left alone. *)
      | Pexp_assert _ -> e
      | Pexp_lazy body when is_trivial_syntactic_value body -> e
      | Pexp_ifthenelse (condition, then_, else_) ->
          {
            e with
            pexp_desc =
              Pexp_ifthenelse
                ( self#mutate `Cond condition,
                  self#expression then_,
                  Option.map self#expression else_ );
          }
      | Pexp_while (condition, body) ->
          {
            e with
            pexp_desc =
              Pexp_while (self#mutate `Cond condition, self#expression body);
          }
      | Pexp_apply (operator, [ (Nolabel, left); (Nolabel, right) ])
        when is_connective_apply capabilities e ->
          (* A connective rule 2 skipped: its operands are boolean
             positions all the same. *)
          {
            e with
            pexp_desc =
              Pexp_apply
                ( operator,
                  [
                    (Nolabel, self#mutate `Bool left);
                    (Nolabel, self#mutate `Bool right);
                  ] );
          }
      | _ -> super#expression e

    method! expression e =
      (* The [suppressed] check matters for expressions reached outside
         the [structure_item] dispatch below - module expressions such as
         [(val ...)] inside a [[@@@mutate off]] region. *)
      if suppressed then e else self#mutate `Ordinary e

    method! case c =
      if suppressed then c
      else
        {
          pc_lhs = self#pattern c.pc_lhs;
          pc_guard = Option.map (self#mutate `Cond) c.pc_guard;
          pc_rhs = self#expression c.pc_rhs;
        }

    (* [[@@mutate off]] on a module binding - [module M = ... [@@mutate
       off]], plain or rec - skips the whole module, like the same
       attribute on a value binding. *)
    method! module_binding mb =
      if has_off_attribute mb.pmb_attributes then mb
      else super#module_binding mb

    method! structure_item si =
      match si.pstr_desc with
      | Pstr_attribute attribute ->
          (match recognize_mutate_attribute attribute with
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
          | Exclude_file ->
              Location.raise_errorf ~loc:attribute.attr_loc
                "mutate exclude_file is not allowed here.");
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

(* Per-file runtime initialization *)

(* The generated preamble is one type declaration and one binding: the
   site record's type, re-exported so the table below can name its
   fields, and the guard closure the file's guards call.

     module Windtrap_mut___<mangled file> = struct
       type site = Windtrap_mutate.site = {
         line : int; col : int; rewrite : string;
         before : string; after : string; dismissed : string option;
       }

       let ___windtrap_armed___ =
         Windtrap_mutate.register ~file:<file> ~sites:<table>
     end

   [register] allocates the reach and epoch arrays itself and captures
   them in the closure, so site indices are file-local: a single global
   array indexed by an absolute id would be indexed before every file had
   registered - link order decides - and reading past its end is
   undefined behaviour rather than an exception. The module is mangled
   from the file name so that each compilation unit calls its own, and
   the [[@@@ocaml.text "/*"]] stop comments hide the generated code from
   odoc.

   Two decisions here differ from coverage's otherwise identical
   preamble, and both have the same cause: [Windtrap_mutate] declares
   [line], [col], [rewrite], [before], [after] and [dismissed] across
   three record types, so [Windtrap_mutate.before] resolves to [mutant]'s
   field and using it for a [site] is warning 42 - disambiguated-name,
   fatal in a library compiled with [-w +a -warn-error +a]. Qualifying
   every field, which is all coverage needs, is therefore not enough. Re-exporting the type makes its labels
   the only ones in scope inside the generated module, so the table names
   them with no type-directed disambiguation at all - and the equation
   makes a runtime whose record has drifted a loud compile error rather
   than a silent mis-registration.

   Because the module now carries a record type, it is referenced
   qualified instead of being opened: opening it would put labels named
   [line], [col], [before] and [after] into the user's scope,
   where they could shadow the user's own or make the user's records
   ambiguous. *)
let runtime_initialization st ~file ~module_name =
  let loc = { (Location.in_file file) with loc_ghost = true } in
  let site_type =
    [%stri
      type site = Windtrap_mutate.site = {
        line : int;
        col : int;
        rewrite : string;
        before : string;
        after : string;
        dismissed : string option;
      }]
  in
  let sites_table =
    pexp_array ~loc
      (List.rev_map
         (fun site ->
           [%expr
             {
               line = [%e eint ~loc site.line];
               col = [%e eint ~loc site.col];
               rewrite = [%e estring ~loc site.rewrite];
               before = [%e estring ~loc site.before];
               after = [%e estring ~loc site.after];
               dismissed =
                 [%e
                   match site.dismissed with
                   | None -> [%expr None]
                   | Some reason -> [%expr Some [%e estring ~loc reason]]];
             }])
         st.rev_sites)
  in
  let armed_binding =
    [%stri
      let ___windtrap_armed___ =
        Windtrap_mutate.register ~file:[%e estring ~loc file]
          ~sites:[%e sites_table]]
  in
  let generated_module =
    Ast_helper.Str.module_ ~loc
      (Ast_helper.Mb.mk ~loc
         { txt = Some module_name; loc }
         (Ast_helper.Mod.structure ~loc [ site_type; armed_binding ]))
  in
  let stop_comment = [%stri [@@@ocaml.text "/*"]] in
  [ stop_comment; generated_module; stop_comment ]

(* Entry point *)

(* The ignore lists mirror coverage's entry filter
   (ppx/coverage/instrument.ml); keep them in sync. Mutation adds one
   exclusion of its own: files declaring inline tests. *)
let always_ignore_paths = [ "//toplevel//"; "(stdin)" ]
let always_ignore_basenames = [ ".ocamlinit"; "topfind" ]

let transform_impl_file ctxt ast =
  let file = Expansion_context.Base.input_name ctxt in
  let excluded =
    List.mem file always_ignore_paths
    || List.mem (Filename.basename file) always_ignore_basenames
    || has_exclude_file_attribute ast
    || file_declares_inline_tests ast
  in
  if excluded then ast
  else
    let st = create_state () in
    let capabilities = capabilities_of_structure ast in
    let module_name = generated_module_name ~file in
    let instrumented =
      (new instrumenter st capabilities module_name)#structure ast
    in
    if st.count = 0 then ast
    else runtime_initialization st ~file ~module_name @ instrumented
