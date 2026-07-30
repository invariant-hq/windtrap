(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The GitHub Actions renderer: workflow commands for failure annotations and
    log folding.

    [Render_github] projects failures into [::error::] workflow commands — so
    they surface as annotations on the PR diff — and provides the
    [::group::]/[::endgroup::] pair the runner wraps the terminal transcript in.
    The runner emits these on standard output only under GitHub Actions
    ({!Env.in_github_actions}); this module never reads the environment.

    The renderer owns its transport's validity: annotation messages are
    ANSI-stripped and percent-encode newlines as [%0A] (with [%25]/[%0D] for [%]
    and CR), and command properties — [file], [line], [title] — additionally
    encode [:] and [,] as [%3A] and [%2C], so no payload can terminate or
    restructure a workflow command.

    Every function is pure and returns complete command lines, each ending in a
    newline, ready to write verbatim at column zero. *)

val group_start : string -> string
(** [group_start name] is the [::group::<name>] command line opening a folded
    log section named [name]. *)

val group_end : string
(** [group_end] is the [::endgroup::] command line. *)

val annotation :
  ?invocation:Render.invocation -> path:string list -> Failure.t -> string
(** [annotation ~path f] is one [::error] command line for [f], raised by the
    test at [path]: [file=]/[line=] properties from [f]'s location when it has
    one, a [title] naming the test, and as message the unstyled
    {!Render.pp_failure} block — counterexample, replay line, and acceptance
    command included — with its newlines [%0A]-encoded.

    Subtest failure entries ({!Render.is_subtest_failure}) annotate at the
    parent test: the [title] names the test whose body ran them, the location is
    the entry's own — a line inside that body — and the [parent › name] label
    leads the message. Sub-cases are failure entries, not tests, and get no
    identity of their own here.

    [invocation], default [`Mirrors], is the hint context ({!Render.invocation})
    the embedded replay and acceptance lines are spelled from — pass the run's
    real invocation so annotation bodies match the terminal block bytes. *)

val annotations : ?invocation:Render.invocation -> Run.result list -> string
(** [annotations results] is the concatenated {!annotation} lines for every
    failure of every counted failed result in [results] ({!Run.result.counted}),
    in run order — the block the runner prints once at end of run. [""] when no
    test failed. [invocation] is passed to each {!annotation}.

    Excused expected failures — failing results that did not count — produce no
    annotations: an [::error] on a PR demands action, and an excused failure
    demands none (its run exited [0]). *)
