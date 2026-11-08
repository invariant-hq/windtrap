(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The mutation runtime: the mutant catalogue, the arming guard and the reach
    map.

    Code instrumented by [ppx_windtrap.mutate] calls {!register} once for each
    source file, when the module of the file loads, and binds the guard that it
    returns.

    A caller reads the {!val-catalogue} and builds the reach map with {!drain}
    during a dry run. A process that tests a mutant then calls {!arm} and runs
    the tests that reached it.

    With no mutant armed, the guard answers [false] at every site.

    This module touches no file, reads no environment variable and installs no
    [at_exit] function. It writes one warning on standard error (see
    {!register}), and it registers the printer of {!Runaway} when it loads. *)

(** {1:identity Identifiers} *)

type id = { file : string; line : int; col : int; rewrite : string }
(** The type for mutant identifiers. [file] is the path of the source file as
    the instrumenter read it, which under dune is relative to the workspace
    root, as [lib/calc.ml]. [line] is the 1-based line and [col] the 0-based
    column of the first byte of the mutated expression. [rewrite] is a name of
    {!rewrites}. *)

val rewrites : string list
(** [rewrites] is the closed vocabulary of rewrite names, in this order:
    ["not"], the comparisons ["lt"], ["le"], ["gt"], ["ge"], ["eq"] and ["neq"],
    the arithmetic ["add"], ["sub"], ["fadd"] and ["fsub"], the connectives
    ["and"] and ["or"], then ["del"], which drops a statement, ["int"], which
    nudges an integer literal, and ["bool"], which flips a boolean one. A name
    is that of the replacement, never that of the operator in the source, but
    for the last three, which name what they change. *)

val id_to_string : id -> string
(** [id_to_string id] is [<file>:<line>:<col>:<rewrite>], as
    ["lib/calc.ml:9:12:add"]. It is the spelling that [--arm] and
    [WINDTRAP_MUTATE_ARM] accept. *)

val compare_id : id -> id -> int
(** [compare_id a b] orders identifiers by [file], then by [line], [col] and
    [rewrite]. *)

(** {1:catalogue Sites and the catalogue}

    Generated code is the only caller of {!register}. Generated code names the
    labels of {!register} and the eight fields of {!type-site}, in their order,
    so a record that drifts is a compile error in every instrumented file. *)

type site = {
  line : int;
      (** The 1-based line of the first byte of the mutated expression. *)
  col : int;  (** The 0-based column of that byte. *)
  end_line : int;
      (** The 1-based line of the byte that follows the mutated expression. *)
  end_col : int;  (** The 0-based column of that byte. *)
  rewrite : string;  (** The name of the replacement, from {!rewrites}. *)
  before : string;  (** The original expression, printed from the parsetree. *)
  after : string;  (** The armed expression, printed from the parsetree. *)
  dismissed : string option;
      (** [Some reason] when the expression carries [[@mutate off]]. The site is
          then catalogued and carries no guard, so a caller must leave it out of
          what it tests and counts. *)
}
(** The type for mutation sites: one entry of the site table of a file. *)

val register : file:string -> sites:site array -> int -> bool
(** [register ~file ~sites] records the site table of [file] and is its guard.
    [sites] is kept and not copied, so it must not be mutated afterwards. An
    executable catalogues the instrumented modules that it links, so a module of
    a library that the executable never references has no mutant there.

    Applied to the index [i] of a site, the guard does two things in order.
    + It adds one to the count of evaluations of site [i].
    + It is [true] iff site [i] is the armed one. It raises {!Runaway} instead
      if the count of the armed site is above the budget given to {!arm}.

    The count is incremented atomically, so domains that evaluate sites at once
    lose no evaluation, and until it raises the guard allocates nothing. The
    guard raises [Invalid_argument] if [i] is outside [sites], which only a
    broken instrumenter causes.

    When [file] is registered again with an equal table, as when one source is
    compiled into two modules, the two registrations arm together, and
    {!val-catalogue} and {!drain} report each mutant once. Two tables are equal
    when their sites agree on the eight fields. A table that differs from an
    earlier one for [file] is dropped. One line then goes to standard error,
    behind [windtrap: warning:], when the module loads and whatever the flags of
    a run. The guard that is returned answers [false] at every index, counts
    nothing and never raises.

    Raises [Invalid_argument] if a site has [line < 1], [col < 0], an end before
    its start or a [rewrite] outside {!rewrites}. Only a broken instrumenter
    produces such a table, and the exception is raised when the module loads.
    [file] is not checked, and [""] is accepted. *)

type mutant = {
  id : id;  (** The identifier of the mutant. *)
  end_line : int;  (** The [end_line] of its site. *)
  end_col : int;  (** The [end_col] of its site. *)
  before : string;  (** The original expression, printed from the parsetree. *)
  after : string;  (** The armed expression, printed from the parsetree. *)
  dismissed : string option;  (** The reason of [[@mutate off]], if any. *)
}
(** The type for catalogued mutants: a {!type-site} with its file. *)

val catalogue : unit -> mutant list
(** [catalogue ()] is every mutant registered in this executable, the dismissed
    ones included, ordered by {!compare_id} on [id] and without duplicates. It
    is complete once module initialization is over. *)

(** {1:arming Arming}

    At most one mutant is armed in a process. *)

(** The type for arming errors. On {!Malformed}, {!Unmatched} and {!Ambiguous} a
    caller must refuse to run, and on {!Uncatalogued} it may run unarmed. *)
type arm_error =
  | Malformed of { spec : string; reason : string }
      (** [spec] is not a mutant identifier, and [reason] says why. Only
          {!id_of_string} produces it. *)
  | Uncatalogued of { id : id }
      (** This executable catalogues no site of [id.file]. Nothing is armed. *)
  | Unmatched of { id : id; candidates : mutant list }
      (** [id.file] is catalogued here, and none of its sites matches the line,
          the column and the rewrite of [id]. [candidates] is the mutants of
          that file, ordered by {!compare_id} on [id] and never empty. *)
  | Ambiguous of { id : id; candidates : mutant list }
      (** [id] matches several sites of one file. [candidates] has one entry for
          each, and the entries are equal as identifiers. *)

val pp_arm_error : Format.formatter -> arm_error -> unit
(** [pp_arm_error ppf e] formats a message on [e] for a person. The message is
    not stable enough for a program to match. *)

val id_of_string : string -> (id, arm_error) result
(** [id_of_string s] is the identifier that [s] spells in the form of
    {!id_to_string}, or [Error (Malformed _)]. [s] is split from the right, so
    [file] may hold colons, as in [C:/x/calc.ml:9:12:add]. [s] is malformed when
    a part is missing, when [file] is empty, when the line is below [1], and
    when the rewrite is outside {!rewrites}. The line and the column must be
    plain decimal numerals, so a sign, [0x] and [_] are refused.

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
    evaluation [n + 1] raises {!Runaway}. There is no bound by default. The
    bound is compared with the count of each registration on its own. The count
    is the one since the last {!reset_reach}, and not since this call, so a
    caller that inherited counts, as a forked child does, must call
    {!reset_reach} after [arm].

    Raises [Invalid_argument] if [budget] is not positive, and nothing is
    disarmed then. *)

exception Runaway of { id : id; hits : int; budget : int }
(** Raised by the guard of the armed site [id] when [hits], its count of
    evaluations since the last {!reset_reach}, is above [budget]. It escapes
    into the mutated program, and a caller that judges the mutant must count
    that as a kill, by the budget and not by the clock. The count keeps rising,
    so a program that catches the exception and evaluates the site again gets it
    again. *)

(** {1:reach The reach map}

    Every site counts its evaluations, and a {!drain} reports the sites whose
    count moved since the previous one. The span between two drains is a window.
    To learn which mutants one test evaluates, a caller drains when the test
    starts and when it finishes: the second drain is the test's window. The
    first drain of a process reports what was evaluated before it, as module
    initialization is, and a drain between two tests what no test evaluated, as
    the release of a fixture.

    {b Domains.} Each evaluation is counted by exactly one drain, and the OCaml
    memory model (the OCaml manual, "Memory model: the hard bits") says which:
    an evaluation that happens before a drain is counted by it or by an earlier
    one, and one that a drain happens before is counted by a later one. The
    order is that of the domain that drains, and the one that [Domain.spawn] and
    [Domain.join], an atomic, a mutex or a condition create between domains. So
    the window of a test that joins the domains it spawns, or waits for the pool
    it hands work to, counts every evaluation that test made and no other
    test's. A domain still running when the test's window closes has what it
    evaluates afterwards counted in a later window, or lost with its process if
    that ends first. *)

type reached = {
  mutant : mutant;  (** The mutant that was evaluated. *)
  hits : int;  (** The number of evaluations of its guard in the window. *)
}
(** The type for one entry of a drained window. *)

val drain : unit -> reached list
(** [drain ()] is the mutants evaluated since the previous [drain], or since
    {!reset_reach} if that came later, ordered by {!compare_id} on [id] and
    without duplicates, each with its evaluations in that window. A mutant that
    two modules registered for one file appears once, with their evaluations
    added up. [drain] reads the count of every registered site, whether or not
    it was evaluated. *)

val reset_reach : unit -> unit
(** [reset_reach ()] sets every count of evaluations to zero, so the next
    {!drain} reports what is evaluated after it. It does not disarm. *)
