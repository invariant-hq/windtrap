(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Mutation instrumentation of implementation files.

    Rewrites four families of expression into a guard whose {e disarmed} arm is
    the original expression and whose {e armed} arm is a plausible defect, and
    prepends one generated module binding the guard closure
    [Windtrap_runtime.Mutate.register] returns for the file. Every mutant of a
    project compiles into one binary; at most one is ever armed, and only in a
    forked child of a run that asked for it (guarantee 12).

    {b The emission law.} Because all mutants share one binary, an ill-typed arm
    is not one bad mutant, it is a broken build for the whole project. So
    {b every arm must be well-typed without type information, and must mention
       only identifiers already present in the original expression, plus
       [Stdlib]-qualified names.} It shapes all four operators: the comparison
    rewrites negate the operator the source already wrote rather than naming its
    partner, the connective rewrite branches on the armed flag rather than
    lifting [&&] to a value, and [ari] — the one admitted exception, since
    [a + b → a - b] cannot avoid naming [-] — is skipped in any file that
    visibly rebinds an arithmetic operator.

    The operators, each one rewrite per site:

    - [neg] — an [if] or [while] condition, or a [when] guard, that is neither a
      comparison nor a connective, negated.
    - [cmp] — [<], [<=], [>], [>=], [=] or [<>] on two unlabelled arguments
      {b in a boolean context}, shifted by one boundary. The context restriction
      is what makes the rewrite typing-closed, and it costs the mutants of
      [let ok = a < b].
    - [con] — [&&] and [||] swapped, through one branch that keeps
      short-circuiting and duplicates no operand.
    - [ari] — [+], [-], [+.] and [-.] on two unlabelled arguments, anywhere.

    {b Placement.} At most one mutant per site, [cmp] and [con] taking priority
    over [neg]; no guard whose expansion duplicates another site, so
    [a && b && c] mutates only the inner connective; no guard on a value spine,
    which is vacuous for these four operators and which no machinery implements
    (the [.ml] says what adding it would take); and every binder is
    [__windtrap_mut_<site>_<role>], site-indexed so it can neither shadow a user
    binding nor collide with a nested guard.

    {b Never mutated.} [assert] and everything under it; attribute and extension
    payloads; [lazy] bodies that are trivial syntactic values; any file
    declaring inline tests, since a file that declares them is test code;
    generated code — a site at a ghost location, or one whose line, column and
    rewrite another site of the file already claims, because the identifier
    [<file>:<line>:<col>:<rewrite>] must name at most one site; and every node
    of an operator chain but the outermost, so [a + b + c] carries one mutant
    rather than two. Module-initialization sites are {e not} excluded: the
    runtime classifies them at run time from the reach epoch, and the
    instrumenter must not guess.

    {b Dismissal} uses the coverage attribute grammar, spelled [mutate], so a
    user who has met one has met both: [[@mutate off]] on an expression,
    [[@@mutate off]] on a structure-level value or module binding,
    [[@@@mutate off]] / [[@@@mutate on]] around a region, and
    [[@@@mutate exclude_file]] for a file. Each takes an optional reason string,
    which lands in the site table and prints in the report, so a dismissal is
    reviewable rather than merely obeyed. Only the expression spelling
    catalogues the site it suppressed; the coarser three suppress without
    cataloguing, there being nothing to dismiss individually where a whole
    binding, region or file is out of scope. [[@@mutate off]] on a [let … in]
    binding or on a structure item that is neither a value nor a module is
    silently ignored, exactly as its coverage counterpart is, and
    [fixture_off_edges] pins the silence.

    Guarantee 12 in [doc/dev/architecture.md] states the contract this pass owes
    — an instrumented build with nothing armed is observationally identical to
    an uninstrumented one — and [test/ppx/mutate/semantics/] enforces it against
    a second, uninstrumented compilation. The emitted shapes and the reasoning
    behind each are in the [.ml], beside the code that emits them;
    [doc/manual/mutation.md] is the chapter a user reads. *)

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
