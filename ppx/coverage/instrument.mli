(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The coverage rewriter.

    {!transform_impl_file} maps the parsetree of an implementation file to the
    same parsetree with a visit at each of its {{!section-points}points}. It
    prepends a {{!section-generated}generated module} that registers the point
    table of the file with [Windtrap_runtime.Coverage] when the file is
    initialized. The {{!section-exclusion}exclusion attributes} switch the
    rewriting off for an expression, a binding, a region or a file.

    Instrumentation never changes what a program or a test means. An
    instrumented file evaluates the expressions of the original in the same
    order, every tail call stays a tail call, and a [lazy] is compiled as it
    was.

    The set of instrumented forms is closed, and adding one to it is a design
    amendment. *)

(** {1:points Points}

    A point is a byte extent of the source that a report paints, with a counter.
    There are two kinds, which differ in when the counter moves. *)

(** {2:entries Entry points}

    An entry point counts each time a block is entered. The block [e] becomes
    [___windtrap_visit___ i; e], where [i] is the index of the point. The blocks
    are:
    - the body of a function. A curried chain has one point, at its innermost
      body, and a type constraint or a coercion on that body is kept around the
      visit.
    - the default of an optional argument, [?(x = e)], of a function or of a
      class.
    - the right-hand side of each arm of a [match], a [try] or a [function], and
      the guard of an arm that has one.
    - each branch of an [if].
    - the body of a [while] and of a [for].
    - the body of a [lazy], unless it is a trivial syntactic value: a function,
      an identifier, a constant or a constant constructor, with or without a
      type constraint or a coercion.
    - the body of a binding operator form such as [let*].
    - the body of a concrete method and of a class initializer.
    - the right operand of [&&].
    - each operand of [||]. [a || b] is rewritten to
      [if a then (v; true) else if b then (w; true) else false], in which the
      visits [v] and [w] count the times that [a] and that [b] were true. The
      deprecated [&] and [or] are handled as [&&] and [||].
    - what follows an [if] without [else] in a sequence, [rest] in
      [(if c then e); rest].

    Three arms have no entry point: one whose body is [assert false], a
    refutation arm, and one whose body carries [[@coverage off]]. A function
    whose body is [assert false] keeps its point.

    The right operand of a [||] in tail position gives up its point when a tail
    call can sit in it, and stays the [else] branch as written. This is the case
    of an application of a function that is not a
    {{!section-out_edges}trivial primitive}, and of a method call. It is also
    the case of a [let], [let module], [let exception], [let open], [match],
    [try], [if], sequence, binding operator form, type constraint and coercion.
    In any position, the right operand of a [||] that is a
    {{!section-out_edges}call of a function that never returns} gives up its
    point, since it is never true, and stays the [else] branch as written. *)

(** {2:out_edges Out-edge points}

    An out-edge point counts each time an expression returns. The expression [e]
    becomes [___windtrap_post_visit___ i e], which evaluates [e] and then
    visits, so a call that raises leaves its point unvisited. The expressions
    are applications, method calls, [new] and [assert e].

    An application, a method call, a [new] and a [|>] or [|.] pipeline are never
    wrapped in tail position. The return of a tail call is observed at the first
    application up the call chain that is not in tail position. An [assert] is
    wrapped in any position, and [assert false] in none.

    These expressions carry no out-edge either:
    - an application of one of the trivial primitives, which are matched by
      their spelling, so [Stdlib.( + ) a b] is wrapped. They are [&&], [&],
      [not], [=], [<>], [<], [<=], [>], [>=], [==], [!=], [ref], [!], [:=], [@],
      [^], [+], [-], [*], [/], [+.], [-.], [*.], [/.], [mod], [land], [lor],
      [lxor], [lsl], [lsr], [asr], [ignore], [Sys.opaque_identity], [Obj.magic]
      and [##].
    - a call of a function that never returns, whose out-edge could never be
      visited: an application of one, directly or through [@@], [|>] or [|.].
      These functions are matched by their spelling too, so
      [Stdlib.invalid_arg s] is wrapped and a call of a function of one's own
      named [exit] is not. They are [raise], [raise_notrace], [failwith],
      [invalid_arg] and [exit].
    - an application whose every argument is labelled or optional. The test
      reads the labels alone, so it holds for a total application of that shape
      as for a partial one.
    - an application or a method call in the body of a value binding that
      carries [[@tail_mod_cons]] or [[@ocaml.tail_mod_cons]], at the top level
      of a structure or in a [let … in]. A [new] and an [assert] there are
      wrapped, and entry points are unaffected.
    - an expression whose return an enclosing form already observes: the
      scrutinee of a [match], the condition of an [if], the applied left operand
      of [@@], the right operand of [|>] or [|.], and a method call in the
      position of a callee. *)

(** {2:identity Extents, identity and numbering}

    An extent is a [Windtrap_runtime.Coverage.point]. An entry point carries the
    extent of its block. That of an arm runs from the start of its pattern to
    the end of its body. It is the body alone when the pattern has a ghost
    location or starts after the body. An out-edge point carries the extent of
    the whole expression that it wraps.

    A point is identified by one attribution offset. Two marks at the same
    offset are one point, which keeps the extent that was recorded first. The
    offset is:
    - for an entry point, the start of the block.
    - for an operand of [||], its last byte.
    - for an out-edge whose successor is known, the start of the successor. The
      successor of the bound expression of a [let] with one binding is the body
      of the [let]. That of the first expression of a sequence is the second,
      and that of the left operand of a pipeline is the right one.
    - for any other out-edge of an application, the last byte of its callee. For
      [l @@ x] it is the last byte of [l], and for a pipeline that of the head
      function of its last stage.
    - for any other out-edge of a method call, the last byte of the expression,
      and for an [assert e] the start of [e].

    A mark whose offset is also that of a block entry thus shares the point of
    that entry, which counts when the block is entered. This is the case of a
    call that opens a block and whose callee is one byte long, as [p x] in an
    arm [p x || q], which counts as visited even when it raises. It is also the
    case of a left operand of [||] that is one byte long and opens a block, as
    [x] in [let either x y = x || y], which counts as visited even when it is
    never true.

    A point is numbered by the order of its first allocation, from [0], and the
    numbers are local to the file. The sub-expressions of a node are marked
    before the blocks of the node itself, so the arms of a [match] are numbered
    before the body of the function that holds it.

    A mark whose attribution location is a ghost one, which is that of generated
    code, is not inserted. The payloads of extension nodes and of attributes are
    never traversed.

    The inline tests of a file are test code and carry no point; the rest of the
    file is instrumented. An inline test is an extension node such as [let%test]
    or [module%test], which a driver without ppx_windtrap leaves in place, or
    the items that ppx_windtrap expands it into, each of which carries the
    attribute [[@@windtrap.test]]. A [let] or [module] item that carries it is
    left as written, the helpers inside a [module%test] included.

    When the mutation rewriter ran on the file first, which is the order of the
    driver that dune builds for a stanza naming both backends, the file holds
    its guards. A guard is generated code and takes no mark, with these effects:
    - the disarmed arm of an ordering or [ari] guard keeps the location of its
      site and is marked as the branch of an [if], whether or not the site is a
      block.
    - a block that is a [neg], an equality or a [con] site has no entry point.
    - an application that is a [neg] site, or the left operand of a [con] site,
      has no out-edge.
    - a [||] that is a site is not rewritten, and its right operand is marked as
      a branch of the guard.

    The points of such a file are not those of the file under this rewriter
    alone. *)

(** {1:exclusion Exclusion attributes}

    The attribute is Bisect_ppx's [coverage], and its payload is [off], [on] or
    [exclude_file]. [off] alone takes a reason, as one string literal:
    [[@coverage off "reason"]]. The reason is for the reader of the source, and
    the rewriter drops it.
    - [[@coverage off]] on an expression leaves the expression as written, with
      everything inside it.
    - [[@@coverage off]] does the same on a value binding at the top level of a
      structure, and on a module binding, recursive or not. On a [let … in]
      binding and on any other structure item it is ignored and its payload is
      not checked. [[@coverage off]] on the bound expression covers that case.
    - [[@@@coverage off]] opens a region of structure items that are left as
      written, and [[@@@coverage on]] closes it. A region belongs to the
      structure that it opens in. A nested structure inherits it, and the end of
      a nested structure restores the setting of the outer one. A region that is
      never closed runs to the end of its structure.
    - [[@@@coverage exclude_file]] among the top-level items of a file excludes
      the file. *)

(** {1:generated Generated code}

    The rewriter prepends four structure items, in this order: a stop comment
    [[@@@ocaml.text "/*"]], the module [Windtrap_cov___<name>], its [open], and
    a second stop comment. [<name>] is the input name of the file, as the driver
    gives it, with every byte other than an ASCII letter, a digit or [_]
    replaced by [___].

    The module binds [___windtrap_visit___]. When the module is initialized it
    allocates one counter per point and calls
    [Windtrap_runtime.Coverage.register ~file ~points ~counts] once, within the
    preconditions of [register]. [file] is the input name and [points] is the
    table of extents in index order. [___windtrap_visit___ i] is then
    [Windtrap_runtime.Coverage.visit counts i]. The module also binds
    [___windtrap_post_visit___] when the file has an out-edge point, and only
    then. *)

(** {1:rewriting Rewriting} *)

val transform_impl_file :
  Ppxlib.Expansion_context.Base.t ->
  Ppxlib.Parsetree.structure ->
  Ppxlib.Parsetree.structure
(** [transform_impl_file ctxt ast] is [ast] with a visit inserted at each point
    and the {{!section-generated}generated module} prepended. [ctxt] is read for
    the input name of the file alone.

    The result is [ast] itself in three cases:
    - [ast] has a top-level [[@@@coverage exclude_file]].
    - The input name is [//toplevel//] or [(stdin)], or its base name is
      [.ocamlinit] or [topfind].
    - No point was allocated, whether [ast] holds no instrumented form or all of
      them are switched off or generated.

    The rewriter reads the [coverage] attributes of the expressions that it
    traverses, of the value bindings at the top level of a structure and of
    module bindings, and those that float in a structure. An attribute anywhere
    else, or inside excluded code, is never examined.

    Raises a ppxlib located error, which the driver reports as a compile error
    at the attribute, if an attribute that the rewriter reads:
    - has a payload other than [off], [off] with one string literal, [on] or
      [exclude_file].
    - is [on] or [exclude_file] on an expression or on a binding.
    - is [exclude_file] floating inside a nested structure.
    - is [[@@@coverage off]] inside a region, or [[@@@coverage on]] outside one.

    No other input raises. *)
