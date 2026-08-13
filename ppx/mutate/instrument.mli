(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Mutation instrumentation of implementation files.

    Rewrites four families of expression into a guard whose {e disarmed} arm is
    the original expression and whose {e armed} arm is a plausible defect, and
    prepends one generated module binding the guard closure
    [Windtrap_mutate.register] returns for the file. Every mutant of a project
    compiles into one binary; at most one is ever armed, and only in a forked
    child of a run that asked for it.

    {1:law The emission law}

    Because all mutants share one binary, an ill-typed arm is not one bad
    mutant, it is a broken build for the whole project. Hence:

    {b Every arm of a guard must be well-typed without type information, and
       must mention only identifiers already present in the original expression,
       plus [Stdlib]-qualified names.}

    A rewrite that names a {e second} operator is inadmissible: an
    [open Version] exporting [<] but not [<=] would silently redirect the armed
    arm to [Stdlib.( <= )], and [let ( < ) a b = compare a b] would make the two
    arms disagree in type and fail the build. Three consequences shape
    everything below: {!section:operators}' comparison rewrites negate the
    operator the source already wrote, the connective rewrite branches on the
    armed flag rather than lifting [&&] to a value, and [ari] — the one operator
    whose well-typedness is not structural — is gated on the file not rebinding
    the arithmetic operators.

    {1:operators The four operators}

    {2 [neg] — condition negation}

    Fires on the condition of an [if] or a [while], and on a [when] guard, when
    that condition is neither a comparison nor a connective (placement rule 1).
    Rewrite name: ["not"].

    {[
      (* if c then … *)
      if
        (let __windtrap_mut_0_p = c in
         if ___windtrap_armed___ 0 then Stdlib.not __windtrap_mut_0_p
         else __windtrap_mut_0_p)
      then …
    ]}

    The condition is bound once, so it is evaluated exactly as often as before,
    and [Stdlib.not] rather than [not] because a user-shadowed [not] would
    otherwise decide what the armed arm means.

    {2 [cmp] — boundary shift}

    Fires on an application of [<], [<=], [>], [>=], [=] or [<>] to two
    unlabelled arguments {b in a boolean context}: an [if] or [while] condition,
    a [when] guard, or a direct operand of [&&] or [||]. The context restriction
    is what makes [cmp] typing-closed. In such a position the original program
    can only typecheck if the comparison is [bool], so [Stdlib.not] applied to
    it is well-typed whatever [<] has been shadowed with; outside one,
    [let ( < ) a b = compare a b] would give the two arms different types and
    break the build. The cost is real — [let ok = a < b] carries no mutant — and
    the remedy, if it is ever judged too expensive, is to propagate the boolean
    context through tail-transparent forms ([let … in], [match] arms, [;], type
    constraints), not to drop the restriction.

    The rewrite is expressed by negating the operator the source already wrote.
    The six mappings, with [rewrite] naming the {e replacement}:

    {v
      source   rewrite   armed arm            operands
      a <  b   "le"      not (b <  a)         swapped
      a <= b   "lt"      not (b <= a)         swapped
      a >  b   "ge"      not (b >  a)         swapped
      a >= b   "gt"      not (b >= a)         swapped
      a =  b   "neq"     not (a =  b)         unchanged
      a <> b   "eq"      not (a <> b)         unchanged
    v}

    The four ordering identities swap their operands and the two equality
    identities do not; getting that backwards produces a mutant that is
    identical to the original for [=] and inverted for [<], which every test
    below would still pass. Because the equality pair needs no swap, its guard
    binds the whole comparison once rather than its two operands:

    {[
      (* a < b, in a boolean context *)
      let __windtrap_mut_0_r = b in
      let __windtrap_mut_0_l = a in
      if ___windtrap_armed___ 0 then
        Stdlib.not (__windtrap_mut_0_r < __windtrap_mut_0_l)
      else __windtrap_mut_0_l < __windtrap_mut_0_r

      (* a = b, in a boolean context *)
      let __windtrap_mut_1_p = a = b in
      if ___windtrap_armed___ 1 then Stdlib.not __windtrap_mut_1_p
      else __windtrap_mut_1_p
    ]}

    Operands are let-bound right to left, matching the order the compiler
    already uses. Lifting the operator to a value instead —
    [(if ___windtrap_armed___ 0 then ( <= ) else ( < )) a b] — is shorter and is
    rejected twice over: it names a second operator, and it forces the generic
    polymorphic comparison on both paths including the disarmed one, turning an
    inline integer compare into a call into [caml_lessthan] on the path the
    whole program runs.

    {b One divergence, stated rather than hidden.} The four ordering identities
    hold for every totally ordered type but not for floating-point NaN:
    [nan < 1.0] and [not (1.0 < nan)] differ, so a [cmp] mutant on floats
    behaves as its rendered [after] text says {e except} when an operand is NaN.
    The armed arm is still a well-typed, plausible defect — which is all a
    mutant must be — but a survivor block's [a < b → a <= b] is exact only away
    from NaN.

    {2 [con] — connective swap}

    Fires on [&&] and [||] (not on the deprecated [&] and [or]). Rewrite names:
    ["or"] for [&&], ["and"] for [||].

    [&&] and [||] cannot be lifted to values without losing short-circuiting,
    and [if armed then a || b else a && b] duplicates both operands, which is
    exponential under nesting. The two connectives differ only in their
    short-circuit value, so one branch expresses both:

    {[
      (* a && b *)
      let __windtrap_mut_0_p = a in
      if Stdlib.( <> ) (__windtrap_mut_0_p : Stdlib.Bool.t)
           (___windtrap_armed___ 0)
      then b
      else __windtrap_mut_0_p

      (* a || b *)
      let __windtrap_mut_0_p = a in
      if Stdlib.( = ) (__windtrap_mut_0_p : Stdlib.Bool.t)
           (___windtrap_armed___ 0)
      then b
      else __windtrap_mut_0_p
    ]}

    Disarmed, the guard is [false] and the first reads [if p then b else p],
    which is [a && b]; armed it reads [if not p then b else p], which is
    [a || b]. [b] appears once, stays in tail position, is evaluated on exactly
    the original schedule, and nothing is allocated.

    All three decorations are load-bearing. The constraint makes the compiler
    specialize the comparison to an integer compare instead of calling
    [caml_notequal] on the path the whole program runs; without it the guard
    would cost a C call at every evaluation of every [&&] in the program.
    [Stdlib.] on the operator satisfies the emission law against a user-shadowed
    [( = )] — which is not hypothetical, since a module defining a custom
    equality is ordinary OCaml. And the constraint is spelled [Stdlib.Bool.t],
    never [bool]: a type name is as shadowable as a value name, so a file
    containing [type bool = …] would fail to compile {e every} [con] guard in it
    — a whole-project build break, which is the failure the emission law exists
    to prevent. There is no [Stdlib.bool] (the predefined types are not
    re-exported from [Stdlib]), which is why the alias module is named instead.

    {2 [ari] — arithmetic}

    Fires on [+], [-], [+.] and [-.] applied to two unlabelled arguments,
    anywhere. Rewrite names: ["sub"], ["add"], ["fsub"], ["fadd"] — again the
    replacement, not the original.

    {[
      (* a + b *)
      let __windtrap_mut_0_r = b in
      let __windtrap_mut_0_l = a in
      if ___windtrap_armed___ 0 then __windtrap_mut_0_l - __windtrap_mut_0_r
      else __windtrap_mut_0_l + __windtrap_mut_0_r
    ]}

    This is the single admitted exception to the emission law: [a + b → a - b]
    cannot be expressed without naming [-]. It is guarded by skipping the
    operator family in any file that visibly rebinds [+], [-], [+.] or [-.] — a
    [let ( + ) …] or an [external ( + ) …] at any depth. What an [open] brings
    in cannot be seen, and that residue is documented rather than solved: it
    costs one compile error, at the user's own source location, in a build
    nobody ships, with [[@@@mutate exclude_file]] as the remedy.

    The same file-level check gates [cmp] on the comparison operators (whose
    operand swap assumes the comparison is symmetric in its argument types) and
    [con] on [&&] and [||] (whose rewrite into an [if] is meaning-preserving
    only for Stdlib's). [neg] emits nothing but [Stdlib.not] and is never gated.

    {1:placement Placement}

    + {b At most one mutant per site.} Where [cmp] and [neg] would both fire on
      one condition, only [cmp] does; where [con] and [neg] would, only [con]
      does. [neg] fires on a condition whose {e syntactic class} is neither, and
      it does so whether or not the [cmp] or [con] site actually materialized —
      a connective that placement rule 2 skipped is still a connective, and
      carries no [neg]. The one exception is the file-level operator gate below:
      when the file rebinds a whole family, that family's syntactic class stops
      being recognized at all, so [if a && b then …] in a file that rebinds [&&]
      does carry a [neg] mutant. That is deliberate and sound — such a condition
      is still obliged to be [bool], so [Stdlib.not] on it is still well-typed —
      and it is what keeps a file that rebinds one family worth mutating.
    + {b No guard whose expansion duplicates another mutation site.} A [&&] or
      [||] whose left or right operand is itself a [&&] or [||] carries no site:
      in [a && b && c] only the inner connective is mutated. With the encoding
      above the outer guard would in fact duplicate nothing, so this rule is
      conservative rather than forced here; it is applied as specified, and it
      keeps every guard's operands syntactically adjacent to it.
    + {b No guard on a value spine.}
      {b This rule is vacuous today and no machinery implements it.} Its purpose
      is the value restriction: making any part of a binding's value spine
      non-syntactic weakens [let flags = (true, [])] from [bool * 'a list] to
      [bool * '_weak1], so an [.mli] declaring [val flags : bool * 'a list]
      stops matching. Every site of the four operators above is an application
      or a conditional, which is never a syntactic value, so such a binding was
      already non-generalizable and the guard changes nothing. The operator that
      {e does} bind is [bool], whose site is a constructor and therefore {e is}
      a syntactic value. Whoever adds it must implement the rule as a positional
      flag threaded through the traversal — set at a binding's right-hand side
      and preserved through tuple components, constructor arguments, record
      fields, [lazy] bodies, [let … in] bodies and type constraints, cleared at
      function bodies — and must make [is_trivial_syntactic_value] gate guard
      placement rather than only subtree descent. The same reasoning makes the
      [lazy]-body exclusion below vacuous today.
    + {b Reserved binders.} Every binder introduced is
      [__windtrap_mut_<site>_<role>], with [<role>] one of [p], [l], [r]. Being
      site-indexed, it can neither shadow a user binding for the extent of a
      guarded operand nor collide with a nested guard.

    {1:exclusions Never mutated}

    - [assert] and everything under it: it is rewritten to a polymorphic raise,
      so mutating it breaks typing in exactly the arms where it appears.
    - Attribute and extension payloads.
    - [lazy] bodies that are trivial syntactic values, and everything under
      them: such a [lazy] compiles as already forced. No guard the four
      operators emit can land on such a body (see rule 3 above), so what this
      exclusion costs today is only the mutants {e inside} a body like
      [lazy (fun x -> x + 1)]; it is kept so that the exclusion exists before an
      operator that can reach it does.
    - Any file containing [let%test], [let%expect_test] or [module%test] — a
      file that declares inline tests is test code. Both spellings are
      recognized: the extension nodes, and the
      [Ppx_windtrap_runtime.Ppx_runtime] calls they expand into, which is what a
      real build sees since instrumentation runs after every other rewriter.
    - Generated code: a site whose attribution location is a ghost one is
      dropped, as is a site whose line, column and rewrite another site of the
      file already claims — the mutant identifier
      [<file>:<line>:<col>:<rewrite>] must name at most one site, and rewriters
      such as [[@@deriving]] duplicate non-ghost locations.
    - {b Every node of an operator chain but the outermost}, which is the same
      rule reaching hand-written code. [a + b + c] is [(a + b) + c]; both nodes
      are a [sub] rewrite {e starting at the same byte}, so the identifier
      cannot separate them and the inner one is dropped. A chain of n operators
      of one family carries one mutant, not n-1 — [a + b + c] carries one, while
      [a + b - c] carries two because [sub] and [add] are different rewrites.
      This is a real and permanent cost of naming mutants by the position of
      their expression's first byte, which is the runtime's fixed contract; it
      is not a bug to fix in the instrumenter. [fixture_chain] pins it.

    {b Module-initialization sites are not excluded here.} A toplevel binding's
    right-hand side is instrumented like any other; the runtime classifies it at
    run time from the reach epoch, as {e not armable} rather than {e unreached},
    and the instrumenter must not try to guess.

    {1:attributes Dismissal}

    The attribute grammar is the coverage one, spelled [mutate], so a user who
    has met one has met both: [[@mutate off]] on an expression, [[@@mutate off]]
    on a value or module binding, [[@@@mutate off]] / [[@@@mutate on]] around a
    region, [[@@@mutate exclude_file]] for a file. Each takes an optional reason
    string — [[@mutate off "both arms yield 16 at the boundary"]] — which lands
    in the site table's [dismissed] field and prints in the report, so a
    dismissal is reviewable rather than merely obeyed.

    [[@mutate off]] on an expression leaves that expression exactly as written,
    including everything inside it, and catalogues the site it suppressed with
    its reason. The three coarser spellings suppress the sites under them
    without cataloguing them: there is nothing to dismiss individually where a
    whole binding, region or file is out of scope. An [[@mutate off]] on an
    expression that carries no site of its own — [[@mutate off]] on a [match],
    say — is one of the coarser spellings in effect: it catalogues nothing and
    suppresses everything under it.

    A {e malformed} payload ([[@mutate bogus]]) and a {e misplaced} directive
    ([[@mutate on]] on an expression, [[@@@mutate exclude_file]] inside a nested
    structure rather than at the top level of the file, [[@@@mutate off]] inside
    a region already off, [[@@@mutate on]] outside one) are located ppxlib
    errors. A well-formed [[@mutate off]] in a position this pass does not read
    is {b silently ignored}, exactly as its coverage counterpart is — there are
    two such positions and both matter:

    - [[@@mutate off]] is read on {e structure-level} value and module bindings
      only. On a [let … in] binding it does nothing; the expression spelling
      [[@mutate off]] covers that case and is the one to reach for.
    - [[@@mutate off]] on any other structure item — a type, an [external], an
      [include] — does nothing.

    Both are pinned by [fixture_off_edges], so that the day the coverage
    attribute layer is factored out and shared, the change is visible rather
    than silent. A [[@@@mutate off]] that is never closed suppresses the rest of
    its enclosing structure and is not an error: an unbalanced region is a
    file-scoped decision, not a mistake the pass can distinguish from one.

    {1:generated The generated preamble}

    Per instrumented file, one type declaration and one binding, inside a module
    named after the file so that each compilation unit calls its own guard:

    {[
      module Windtrap_mut___lib___calc___ml = struct
        type site = Windtrap_mutate.site = {
          line : int;
          col : int;
          rewrite : string;
          span : int * int;
          before : string;
          after : string;
          dismissed : string option;
        }

        let ___windtrap_armed___ =
          Windtrap_mutate.register ~file:"lib/calc.ml" ~sites:[| … |]
      end
    ]}

    and every guard in the file names the closure {e qualified},
    [Windtrap_mut___lib___calc___ml.___windtrap_armed___ i].

    Two things differ from the coverage instrumenter's otherwise identical
    preamble, and both have one cause: [Windtrap_mutate] spreads the names
    [line], [col], [rewrite], [span], [before], [after] and [dismissed] across
    three record types, so [Windtrap_mutate.span] resolves to [mutant]'s field
    and using it for a [site] is warning 42 — disambiguated name, fatal in a
    library built with [-w +a -warn-error +a]. Qualifying every field, which is
    all coverage needs, is therefore not enough.

    + The generated module {b re-exports the record type}. That makes its labels
      the only ones in scope inside the module, so the table names them with no
      type-directed disambiguation at all — and the type equation turns a
      runtime whose record has drifted into a loud compile error rather than a
      silent mis-registration.
    + Because the module now carries a record type it is
      {b referenced qualified rather than opened}: opening it would put labels
      named [line], [col], [span], [before] and [after] into the user's scope,
      where they could shadow the user's own or make the user's records
      ambiguous. Not emitting an [open] also means a file whose every site is
      dismissed — which binds the guard and calls it from nowhere — trips no
      unused-[open] warning 33.

    [register] allocates the reach and epoch arrays itself and captures them in
    the closure, so site indices are file-local: a single global array indexed
    by an absolute identifier would be indexed before every file had registered
    — link order decides — and reading past its end is undefined behaviour
    rather than an exception. No side file is written: the catalogue and the
    code it describes are one artifact and cannot disagree.

    The preamble is prepended {e above} the user's own structure, which is what
    makes it safe for it to name [int], [string], [option], [None] and [Some]
    unqualified: a shadowing definition later in the file cannot reach it. No
    such shelter exists for the guards themselves, which is why every identifier
    they emit is [Stdlib]-qualified down to the [con] guard's type constraint.

    [before] and [after] are printed from the parsetree rather than sliced out
    of the source — a preprocessor's working directory under sandboxing is not
    what one expects — with whitespace runs collapsed so a rendering is one
    line. That collapsing also applies inside string literals, which is the one
    place a rendering differs from the source in more than layout. *)

val transform_impl_file :
  Ppxlib.Expansion_context.Base.t ->
  Ppxlib.Parsetree.structure ->
  Ppxlib.Parsetree.structure
(** [transform_impl_file ctxt ast] is [ast] with every mutant's guard inserted
    and the registration module prepended. [ast] is returned unchanged when the
    file is excluded ([[@@@mutate exclude_file]], a file declaring inline tests,
    toplevel and ocamlinit inputs) or when it yields no site at all — an empty
    table would register a file with nothing to say about it.

    Raises a ppxlib located error for a malformed or misplaced [mutate]
    attribute — never for any other input. *)
