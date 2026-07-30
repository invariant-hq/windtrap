(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Expression-grade coverage instrumentation of implementation files.

    Instruments the frozen v1/Bisect population — scope grows only by deliberate
    design amendment — and prepends one initialization module that registers the
    file's point table with the [Windtrap_coverage] runtime at load time. Two
    insertion shapes exist:

    - {e entry points} — [___windtrap_visit___ i; e] at the entry of a block:
      function leaf bodies, [match]/[try]/[function] arms and their guards, [if]
      branches, [while]/[for] bodies, [lazy] bodies (except trivial syntactic
      values, whose compilation [lazy] would otherwise change), letop bodies,
      class bodies (method bodies, initializers, optional-argument defaults),
      and [&&]/[||] condition arms;
    - {e out-edge points} — [___windtrap_post_visit___ i e] around applications,
      method calls, [new], and [assert]: the point fires only when the
      expression {e returns}, so a call that raises reports uncovered. Out-edge
      wrapping is never applied in tail position — the traversal's tail-position
      analysis keeps every tail call a tail call, at the price of attributing a
      tail call's out-edge to its caller's first non-tail application.
      Applications of trivial primitives (operators, [raise], [ignore], …) and
      fully-labeled (partial) applications carry no out-edge.

    Extents are mixed by design: an entry point carries its block's byte extent
    — an arm's spans the entire arm, pattern start to body end — while an
    out-edge point carries the application's own extent, so a raising call
    paints the call. Point identity is a single attribution byte offset (the
    v1/Bisect model): two marks at one offset share a point and the
    first-recorded extent wins.

    Instrumentation is excluded by the Bisect_ppx attribute spelling:
    [[@coverage off]] on an expression, [[@@coverage off]] on a value or module
    binding, [[@@@coverage off]]/[[@@@coverage on]] around structure items, and
    [[@@@coverage exclude_file]] for a whole file. *)

val transform_impl_file :
  Ppxlib.Expansion_context.Base.t ->
  Ppxlib.Parsetree.structure ->
  Ppxlib.Parsetree.structure
(** [transform_impl_file ctxt ast] is [ast] with every point inserted and the
    runtime-registration module prepended. [ast] is returned unchanged when the
    file is excluded ([[@@@coverage exclude_file]], toplevel and ocamlinit
    inputs) or when it contains no instrumentable form.

    Raises a ppxlib located error for a malformed or misplaced [coverage]
    attribute — never for any other input. *)
