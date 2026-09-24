(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The mutation runtime: the mutant catalogue, the arming guard and the reach
    map.

    Code instrumented by [ppx_windtrap.mutate] calls {!register} once for each
    source file, when the module of the file loads, and binds the guard that it
    returns. Every site of the file then evaluates that guard with its own
    index.

    A caller reads the {!val-catalogue} and builds the reach map with
    {!next_epoch} and {!drain} during a dry run. A process that tests a mutant
    then calls {!arm} and runs the tests that reached it. The verdicts are in
    {!Verdicts}.

    A mutant changes what a program means only in an instrumented build, and
    only while it is armed. The loop of a run under [--mutate] arms it in a
    forked child, and a run under [--arm] arms it in the process itself. With no
    mutant armed, the guard answers [false] at every site.

    This module never chooses a mutant. Its caller does, with {!val-catalogue}
    and {!arm}. It touches no file, reads no environment variable and installs
    no [at_exit] function. It writes one warning on standard error (see
    {!register}), and it registers the printer of {!Runaway} when it loads. *)

(** {1:identity Identifiers}

    A mutant is named by the position of its site and by its rewrite. The name
    is what travels. The [--arm] flag and its mirror [WINDTRAP_MUTATE_ARM] take
    it, a verdict file records its fields, and reports print it. *)

type id = { file : string; line : int; col : int; rewrite : string }
(** The type for mutant identifiers. [file] is the path of the source file as
    the instrumenter read it, which under dune is relative to the workspace
    root, as [lib/calc.ml]. [line] is the 1-based line and [col] the 0-based
    column of the first byte of the mutated expression. [rewrite] is a name of
    {!rewrites}.

    The instrumenter emits at most one site for a line, a column and a rewrite
    of a file. This module does not rely on that. {!arm} answers {!Ambiguous}
    when an identifier matches several sites, so a site that no identifier names
    alone is never armed and never gets a verdict. *)

val rewrites : string list
(** [rewrites] is the closed vocabulary of rewrite names, in this order:
    ["not"], the comparisons ["lt"], ["le"], ["gt"], ["ge"], ["eq"] and ["neq"],
    the arithmetic ["add"], ["sub"], ["fadd"] and ["fsub"], the connectives
    ["and"] and ["or"], and ["drop"]. A name is that of the replacement, never
    that of the operator in the source.

    A name outside the list is refused wherever one enters: in a site table by
    {!register}, in an identifier by {!id_of_string} and in a verdict file by
    {!Verdicts.of_string}. {!arm} takes an {!type-id} and does not check its
    rewrite, so an unknown one matches no site. No instrumenter emits ["drop"],
    so no catalogue holds it, although the three places above accept it. *)

val id_to_string : id -> string
(** [id_to_string id] is [<file>:<line>:<col>:<rewrite>], as
    ["lib/calc.ml:9:12:add"]. It is the spelling that [--arm] and
    [WINDTRAP_MUTATE_ARM] accept, and the one that reports and diagnostics
    print. A verdict file records the four fields and not this string. *)

val compare_id : id -> id -> int
(** [compare_id a b] orders identifiers by [file], then by [line], [col] and
    [rewrite]. It is the order of {!val-catalogue} and of {!Verdicts}. *)

(** {1:catalogue Sites and the catalogue}

    Generated code is the only caller of {!register}. For each instrumented file
    the instrumenter generates one module. It restates {!type-site} by a type
    equation and binds the guard as
    [let ___windtrap_armed___ = Windtrap_runtime.Mutate.register ~file ~sites],
    where [sites] is an array literal. Every site of the file is a guard on
    [___windtrap_armed___ i], where [i] is the index of the site in [sites].
    Generated code names the labels of {!register} and the six fields of
    {!type-site}, in their order, so a record that drifts is a compile error in
    every instrumented file. *)

type site = {
  line : int;
      (** The 1-based line of the first byte of the mutated expression. *)
  col : int;  (** The 0-based column of that byte. *)
  rewrite : string;  (** The name of the replacement, from {!rewrites}. *)
  before : string;  (** The source text of the original expression. *)
  after : string;  (** The source text of the armed expression. *)
  dismissed : string option;
      (** [Some reason] when the expression carries [[@mutate off]]. The site is
          then catalogued and carries no guard, so a caller must leave it out of
          what it tests and counts. *)
}
(** The type for mutation sites: one entry of the site table of a file. *)

val register : file:string -> sites:site array -> int -> bool
(** [register ~file ~sites] records the site table of [file] and is its guard.
    [sites] is kept and not copied, so it must not be mutated afterwards. The
    indices of sites are local to [file], and no index spans files. An
    executable catalogues the instrumented modules that it links, so a module of
    a library that the executable never references has no mutant there.

    Applied to the index [i] of a site, the guard does three things in order.
    + It adds one to the count of evaluations of site [i], which saturates at
      [max_int].
    + If site [i] was not yet evaluated in the current epoch, it marks the site
      for the next {!drain}.
    + It is [true] iff site [i] is the armed one. It raises {!Runaway} instead
      if the count of the armed site is above the budget given to {!arm}.

    The guard is not synchronized. A site that several domains evaluate at once
    can lose evaluations and, when an epoch starts, its mark. The reach map is
    then a lower bound, which can miss a mutant and never invents one. The guard
    raises [Invalid_argument] if [i] is outside [sites], which only a broken
    instrumenter causes.

    When [file] is registered again with an equal table, as when one source is
    compiled into two modules, the two registrations arm together, and
    {!val-catalogue} and {!drain} report each mutant once. Two tables are equal
    when their sites agree on the six fields. A table that differs from an
    earlier one for [file] is dropped. One line then goes to standard error,
    behind [windtrap:], when the module loads and whatever the flags of a run.
    The guard that is returned answers [false] at every index, counts nothing
    and never raises. The executable links two incompatible instrumentations of
    one source, and rebuilding from scratch is the remedy.

    Raises [Invalid_argument] if a site has [line < 1], [col < 0] or a [rewrite]
    outside {!rewrites}. Only a broken instrumenter produces such a table, and
    the exception is raised when the module loads. [file] is not checked, and
    [""] is accepted. *)

type mutant = {
  id : id;  (** The identifier of the mutant. *)
  before : string;  (** The source text of the original expression. *)
  after : string;  (** The source text of the armed expression. *)
  dismissed : string option;  (** The reason of [[@mutate off]], if any. *)
}
(** The type for catalogued mutants: a {!type-site} with its file. *)

val compare_mutant : mutant -> mutant -> int
(** [compare_mutant a b] is [compare_id a.id b.id]. [before], [after] and
    [dismissed] take no part. *)

val catalogue : unit -> mutant list
(** [catalogue ()] is every mutant registered in this executable, the dismissed
    ones included, ordered by {!compare_mutant} and without duplicates. It does
    not depend on the link order, so two runs of one executable enumerate the
    mutants alike. It is complete once module initialization is over, and it is
    empty when the executable links no instrumented file. *)

(** {1:arming Arming}

    At most one mutant is armed in a process. An identifier that matches several
    sites, or no site of a catalogued file, is an error that names the
    candidates, because an arming that is silently ignored would turn a green
    run into a false survivor. {!Uncatalogued} is the one case that is no
    mistake, because an executable that holds no site of the file produces no
    verdict on the mutant and hides nothing by running on. *)

(** The type for arming errors. On {!Malformed}, {!Unmatched} and {!Ambiguous} a
    caller must refuse to run, and on {!Uncatalogued} it may run unarmed. *)
type arm_error =
  | Malformed of { spec : string; reason : string }
      (** [spec] is not a mutant identifier, and [reason] says why. Only
          {!id_of_string} produces it. *)
  | Uncatalogued of { id : id }
      (** This executable catalogues no site of [id.file]. That includes an
          executable built without the mutation backend, which catalogues
          nothing. Nothing is armed. *)
  | Unmatched of { id : id; candidates : mutant list }
      (** [id.file] is catalogued here, and none of its sites matches the line,
          the column and the rewrite of [id]. [candidates] is the mutants of
          that file, in {!compare_mutant} order and never empty. The identifier
          is wrong, or it comes from an older build. *)
  | Ambiguous of { id : id; candidates : mutant list }
      (** [id] matches several sites of one file. [candidates] has one entry for
          each, and the entries are equal as identifiers. The instrumenter
          cannot emit such a table, so another rewriter duplicated a location.
          The remedy is [[@mutate off]] on the expression, or the exclusion of
          the file. *)

val pp_arm_error : Format.formatter -> arm_error -> unit
(** [pp_arm_error ppf e] formats a message on [e] for a person. It names the
    identifier, and the remedy where there is one. For an {!Unmatched} and an
    {!Ambiguous} it lists the candidates on lines of their own, at most 6 of
    them, and then the number of those that remain. It prints nothing, and where
    its text shows is the contract of its caller. The message is not stable
    enough for a program to match. *)

val id_of_string : string -> (id, arm_error) result
(** [id_of_string s] is the identifier that [s] spells in the form of
    {!id_to_string}, or [Error (Malformed _)]. [s] is split from the right, so
    [file] may hold colons, as in [C:/x/calc.ml:9:12:add]. [s] is malformed when
    a part is missing, when [file] is empty, when the line is below [1], and
    when the rewrite is outside {!rewrites}. The line and the column must be
    plain decimal numerals, so a sign, [0x] and [_] are refused. An unknown
    rewrite is an error here, and never an identifier that matches nothing.

    [id_of_string (id_to_string id)] is [Ok id] for every identifier of
    {!val-catalogue}, unless its file was registered under the name [""]. *)

val arm : ?budget:int -> id -> (mutant, arm_error) result
(** [arm ?budget id] arms the mutant that [id] names and is that mutant, or an
    {!Uncatalogued}, {!Unmatched} or {!Ambiguous} error. The mutant that was
    armed before is disarmed first, whether or not [id] resolves. From then on
    the guard of the site answers [true], in every module that registered an
    equal table for its file. [arm] does not read [dismissed]. Arming a
    dismissed mutant succeeds, and no guard ever evaluates its site.

    [budget] bounds the evaluations of the armed site. With a budget of [n],
    evaluation [n + 1] raises {!Runaway}, which catches a mutant that spins
    where no timer can see it. There is no bound by default. The bound is
    compared with the count of each registration on its own, where {!armed_hits}
    adds them up. The count is the one since the last {!reset_reach}, and not
    since this call, so a caller that inherited counts, as a forked child does,
    must call {!reset_reach} after [arm].

    Raises [Invalid_argument] if [budget] is not positive, and nothing is
    disarmed then. *)

val disarm : unit -> unit
(** [disarm ()] disarms the armed mutant and removes the budget. The guard is
    [false] at every site afterwards. The counts of evaluations are kept. *)

val armed : unit -> mutant option
(** [armed ()] is the armed mutant, or [None]. While it is [None] every guard
    answers [false], and the instrumented program computes what the original one
    computes. *)

val armed_hits : unit -> int
(** [armed_hits ()] is the number of evaluations of the armed site since the
    last {!reset_reach}, added over every module that registered its file. It
    saturates at [max_int], and it is [0] when nothing is armed. With it, a run
    that armed a mutant and stayed green tells a site that the tests ran without
    checking from a site that no test ran. *)

exception Runaway of { id : id; hits : int; budget : int }
(** Raised by the guard of the armed site [id] when [hits], its count of
    evaluations since the last {!reset_reach}, is above [budget]. It escapes
    into the mutated program, and a caller that judges the mutant must count
    that as a kill, by the budget and not by the clock. The count keeps rising,
    so a program that catches the exception and evaluates the site again gets it
    again.

    Its registered printer gives the identifier, the count and the budget. *)

(** {1:reach The reach map}

    Which tests evaluate which mutants is measured, during a dry run. The
    runtime keeps an epoch counter and the list of the sites marked in the
    current epoch, and takes no position on what an epoch means. Its caller
    gives the meaning by the order of its calls.

    To learn which mutants one test evaluates, a caller must call {!drain} and
    then {!next_epoch} when the test starts, and {!drain} when it finishes. The
    first [drain] returns what was evaluated outside any test, as module
    initialization and the release of a fixture are. *)

type reached = {
  mutant : mutant;  (** The mutant that was evaluated. *)
  hits : int;
      (** The number of evaluations of its guard in the drained window, which
          saturates at [max_int]. *)
}
(** The type for one entry of a drained window. *)

val next_epoch : unit -> unit
(** [next_epoch ()] opens a new epoch. A site that is evaluated afterwards marks
    itself for the next {!drain}, whether or not it was evaluated before. The
    marks of the previous epoch that were not drained stay for the next
    {!drain}. *)

val drain : unit -> reached list
(** [drain ()] is the mutants marked since the previous [drain], ordered by
    {!compare_mutant} and without duplicates. Each comes with its evaluations,
    counted from the one that marked it up to this call. [drain] then empties
    the list of marks, so a second [drain ()] right after it is [[]]. A mutant
    that two modules registered for one file appears once, with its evaluations
    added up.

    A site marks itself at its first evaluation in an epoch, and only then. When
    it is evaluated again in that epoch after a [drain], as when the release of
    a fixture evaluates a site that the test evaluated, no later [drain] reports
    those evaluations. A drained count is a lower bound of what an armed run of
    the same tests evaluates, so a budget that is derived from one needs
    headroom. *)

val reset_reach : unit -> unit
(** [reset_reach ()] sets every count of evaluations to zero, empties the list
    of marks and opens a new epoch. It does not disarm. *)
