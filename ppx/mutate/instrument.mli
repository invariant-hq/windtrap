(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The mutation rewriter.

    {!transform_impl_file} maps the parsetree of an implementation file to the
    same parsetree with each {{!section-sites}site} replaced by a guard. The
    disarmed arm of a guard is the original expression and its armed arm is the
    mutant. It prepends a {{!section-generated}generated module} that registers
    the site table of the file with [Windtrap_runtime.Mutate] and binds the
    guard closure that the registration returns. The
    {{!section-dismissal}dismissal attributes} switch the rewriting off for an
    expression, a binding, a region or a file.

    Every mutant of a project is compiled into one binary, in which the runtime
    arms at most one. Guarantee 12 of [doc/dev/architecture.md] is that a mutant
    changes meaning only in the process that armed it. That process is a forked
    child of the [--mutate] loop, or the run itself under [--arm]. The part of
    the guarantee that this rewriter owes is that a program with no mutant armed
    computes what the uninstrumented one does. A guard evaluates each operand
    once and in the order of the original application, a tail call stays a tail
    call, and a [lazy] is compiled as it was. *)

(** {1:law The emission law}

    Every arm of a guard must be well typed without type information. It must
    mention only identifiers that the original expression already holds, and
    names qualified by [Stdlib]. All the mutants share one binary, so an arm
    that does not type-check breaks the build of the whole project. Whoever adds
    an operator must keep the law.

    [ari] is the one exception, because [a + b] cannot become [a - b] without
    naming [-]. A file that rebinds an operator where the rewriter can see it
    thus loses the whole family of that operator: [ari] for [+], [-], [+.] and
    [-.], [cmp] for the six comparisons, and [con] for [&&] and [||]. [neg]
    names [Stdlib.not] alone and is never lost. A rebinding is seen when a
    variable pattern anywhere in the file, or a value description such as an
    [external], bears the name of the operator. What an [open] brings into scope
    is not seen. When such an operator does not fit an arm, the file fails to
    compile under instrumentation, with the error at the expression in the
    source, and [[@@@mutate exclude_file]] is the way out. *)

(** {1:sites Sites}

    A site is an expression that carries one mutant. *)

(** {2:operators Operators}

    Each operator rewrites its sites in one way, and the name of the rewrite
    says what replaces the original. The sites of [cmp], [con] and [ari] are
    applications of a bare operator to two unlabelled arguments. A qualified
    spelling such as [Float.( < )], a labelled application and a partial one are
    not sites.
    - [neg] negates a condition, under the name [not]. Its sites are the
      condition of an [if] or of a [while] and the guard of an arm, when the
      expression is neither a comparison nor a connective.
    - [cmp] moves a comparison by one boundary. [<] becomes [<=] under the name
      [le], [<=] becomes [<] under [lt], [>] becomes [>=] under [ge], [>=]
      becomes [>] under [gt], [=] becomes [<>] under [neq], and [<>] becomes [=]
      under [eq]. Its sites lie in a boolean context alone.
    - [con] swaps [&&] and [||], under the names [or] and [and], and keeps the
      short-circuit evaluation. The deprecated [&] and [or] are not sites.
    - [ari] swaps [+] and [-] under the names [sub] and [add], and [+.] and [-.]
      under [fsub] and [fadd].

    A boolean context is the condition of an [if] or of a [while], the guard of
    an arm, or a direct operand of [&&] or [||] in a file that keeps the [con]
    family. It does not reach through a sequence, a [let], a type constraint or
    [not], so the comparison of [let ok = a < b] has no mutant. [con] and [ari]
    have sites in every context.

    The armed arm of an ordering rewrite applies the original operator to the
    swapped operands under [Stdlib.not], so [a < b] armed is [not (b < a)]. On
    floats this is the [after] text of the mutant except when an operand is NaN.
*)

(** {2:placement Placement}

    - An expression carries at most one mutant, and [neg] comes last. A
      condition that is a comparison carries [cmp], and one that is a connective
      carries [con].
    - No guard duplicates another site. A connective with a connective for an
      operand carries no mutant, neither [con] nor [neg], so [a && b && c]
      mutates [b && c] alone. Its operands are boolean contexts all the same.
    - In a file that lost the [con] or the [cmp] family, a condition of that
      family is no longer recognized as one, and carries [neg].
    - In a chain of one arithmetic operator, as [a + b + c], the outermost
      application alone is a site. The rule reads the tree and not the layout,
      so [(a + b) + c] and [f (a + b + c)] carry one mutant too. [a + b - c]
      carries two, because [sub] and [add] are two rewrites, and so does
      [a + b + (c + d)].
    - A guard binds its operands under the names [__windtrap_mut_<i>_<role>],
      where [<i>] is the index of the site and [<role>] is [p], [l] or [r]. *)

(** {2:exclusions Code that is never mutated}

    - An [assert], with everything under it.
    - A [lazy] whose body is a trivial syntactic value, with everything under
      it. Such a value is a function, an identifier, a constant or a constant
      constructor, with or without a type constraint or a coercion.
      [lazy (fun x -> x + 1)] carries no mutant.
    - The payloads of attributes and of extension nodes.
    - A file that declares inline tests, which is test code. Such a file holds
      an extension node named [test] or [expect_test], as [let%test], or an
      identifier under [Ppx_windtrap_runtime.Ppx_runtime], which is what the
      expansion of such a node leaves.
    - Generated code, which is a site at a ghost location.
    - A site whose line, column and rewrite an earlier site of the file already
      has, which happens in the code of a deriver. The first one keeps the
      identifier, which must name one site.

    The code that runs when a module is initialized is mutated like any other,
    so a top-level [let origin = 1 + 2] carries a site. The [--mutate] loop
    counts as never reached a site that was evaluated outside every test. *)

(** {1:dismissal Dismissal attributes}

    The attribute is named [mutate] and follows the grammar of the [coverage]
    attribute of the coverage rewriter. Its payload is [off], [on] or
    [exclude_file], and [off] alone takes a reason, as one string literal:
    [[@mutate off "reason"]]. The reason is for the reader of the source. No
    report prints it, and it is read back through
    [Windtrap_runtime.Mutate.catalogue] alone.
    - [[@mutate off]] on an expression leaves the expression as written, with
      everything inside it. When the expression is itself a site, the site is
      recorded in the table as dismissed, with the reason, or with [""] when
      none is given. It takes an index and no guard. When the expression is no
      site, as a [match], nothing is recorded. The sites inside the expression
      are never recorded.
    - [[@@mutate off]] leaves as written a value binding at the top level of a
      structure, and a module binding, recursive or not. On a [let … in] binding
      and on any other structure item it is ignored and its payload is not
      checked. [[@mutate off]] on the bound expression covers that case.
    - [[@@@mutate off]] opens a region of structure items that are left as
      written, and [[@@@mutate on]] closes it. A region belongs to the structure
      that it opens in. A nested structure inherits it, and the end of a nested
      structure restores the setting of the outer one. A region that is never
      closed runs to the end of its structure.
    - [[@@@mutate exclude_file]] among the top-level items of a file excludes
      the file.

    The last three record no site, and the reason of a [[@@mutate off]] or of a
    [[@@@mutate off]] is accepted and dropped. *)

(** {1:identification Identification}

    The identifier of a mutant is [<file>:<line>:<col>:<rewrite>], as
    [lib/calc.ml:9:12:add]. The rewriter records the last three and the runtime
    joins the file. [<line>] is the one-based line of the first byte of the
    site, and [<col>] is the zero-based column of that byte, counted in bytes. A
    site in brackets starts at its bracket, so bracketing a site, as
    [[@mutate off]] on an infix expression requires, moves its column by one.
    Two sites may share a line and a column under two rewrites.

    A site is numbered by the order of its allocation, from [0], and the numbers
    are local to the file. The traversal goes from the top down, so a site comes
    before the sites inside its operands, and those of the left operand before
    those of the right one.

    The [before] and [after] texts of a site are printed from the parsetree and
    never cut out of the source. The attributes of the site are left out, and
    each run of blanks becomes one space, inside a string literal too, so a text
    is one line. The brackets are those of the printer: [a + b + c] is recorded
    as [(a + b) + c], and its mutant as [(a + b) - c]. *)

(** {1:generated Generated code}

    The rewriter prepends three structure items, in this order: a stop comment
    [[@@@ocaml.text "/*"]], the module [Windtrap_mut___<name>], and a second
    stop comment. [<name>] is the input name of the file, as the driver gives
    it, with every byte other than an ASCII letter, a digit or [_] replaced by
    [___]. The module is never opened, and a guard calls
    [Windtrap_mut___<name>.___windtrap_armed___ i], which is [true] iff the site
    [i] is the armed one.

    The module holds, in this order:
    - [type site = Windtrap_runtime.Mutate.site = { … }], the record of the
      runtime with its six fields spelled out. A runtime whose record differs
      thus fails to compile against the generated code.
    - [type 'a operands = 'a * 'a], only in a file that holds the guard of an
      ordering rewrite, which annotates its pair of operands with it.
    - [___windtrap_armed___], bound to
      [Windtrap_runtime.Mutate.register ~file ~sites]. The call is made once,
      when the module is initialized, within the preconditions of [register].
      [file] is the input name and [sites] is the table in index order.

    Every generated node has a ghost location, with one exception. The disarmed
    arm of an ordering or [ari] guard is the original application rebuilt over
    the binders, and keeps its location and its attributes.

    In the driver that dune builds for a stanza naming both backends this
    rewriter runs first, so its sites are those stated here and the coverage
    rewriter receives its guards. When the coverage rewriter ran on the file
    first, which is the order of a driver that links this library before
    [ppx_windtrap.coverage], the file holds its visits:
    - [a || b] reaches this rewriter as the [if] of that rewriter. It carries no
      [con] mutant, and an operand that became a condition is mutated as one.
    - the [before] and [after] texts of a site hold the visits inside it, as
      [___windtrap_post_visit___ 0 (f x)] for [f x].

    The mutants of such a file are not those of the file under this rewriter
    alone. *)

(** {1:rewriting Rewriting} *)

val transform_impl_file :
  Ppxlib.Expansion_context.Base.t ->
  Ppxlib.Parsetree.structure ->
  Ppxlib.Parsetree.structure
(** [transform_impl_file ctxt ast] is [ast] with a guard at each site and the
    {{!section-generated}generated module} prepended. [ctxt] is read for the
    input name of the file alone.

    The result is [ast] itself in four cases:
    - [ast] has a top-level [[@@@mutate exclude_file]].
    - [ast] {{!section-exclusions}declares inline tests}.
    - The input name is [//toplevel//] or [(stdin)], or its base name is
      [.ocamlinit] or [topfind].
    - No site was recorded. A dismissed site is a recorded one, so a file whose
      every site is dismissed is registered, by a module that no guard refers
      to.

    The rewriter reads the [mutate] attributes of the expressions that it
    traverses, of the value bindings at the top level of a structure and of
    module bindings, and those that float in a structure. An attribute anywhere
    else, or inside code that is never mutated or is dismissed, is never
    examined.

    Raises a ppxlib located error, which the driver reports as a compile error
    at the attribute, if an attribute that the rewriter reads:
    - has a payload other than [off], [off] with one string literal, [on] or
      [exclude_file].
    - is [on] or [exclude_file] on an expression or on a binding.
    - is [exclude_file] floating inside a nested structure.
    - is [[@@@mutate off]] inside a region, or [[@@@mutate on]] outside one.

    No other input raises. *)
