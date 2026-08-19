(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Mutation runtime: the mutant catalogue, the arming guard, verdict files.

    Instrumented code (produced by [ppx_windtrap.mutate]) calls {!register} once
    per source file at module load time and binds the guard closure it returns;
    every mutation site in that file evaluates the guard with its own file-local
    index. The catalogue therefore {e is} the binary: no side file is written,
    and a catalogue can never be stale with respect to the code it describes.

    The mutation loop drives this module from the parent process: it reads the
    catalogue ({!catalogue}), builds the reach map from {!next_epoch} and
    {!drain} while the dry run executes, then forks one child per mutant, which
    {!arm}s a single site and runs the tests that reached it. Verdicts —
    {!type:verdict} — are collected into a {!type:t} and written to
    {!output_file} so that {b several test executables can be merged}: see
    {!merge_verdict}, which is the reason the file format exists at all.

    A mutant changes meaning only in a forked child, only when armed, and only
    in a build that asked for it. Nothing here writes to disk unless {!save} is
    called, nothing installs an [at_exit] handler, and with no mutant armed the
    guard's answer is [false] at every site. *)

(** {1:identity Mutant identity}

    A mutant is named by the source position of the expression it rewrites and
    by the rewrite applied there. That name is what travels: through
    {!arm_variable}, through verdict files, and into the report. *)

type id = { file : string; line : int; col : int; rewrite : string }
(** The type for mutant identifiers. [file] is the source path as recorded at
    instrumentation, [line] (1-based) and [col] (0-based) the position of the
    first byte of the mutated expression, and [rewrite] the name of the
    replacement, drawn from {!rewrites}.

    The instrumenter guarantees that at most one site per file carries a given
    [line], [col] and [rewrite] — later colliders (which rewriters such as
    [[@@deriving]] can produce by duplicating locations) are dropped rather than
    instrumented. This module does not trust that guarantee: {!arm} reports a
    collision as {!Ambiguous} rather than arming one of the candidates, and a
    site it cannot name unambiguously is never armed and therefore never
    receives a verdict. *)

val rewrites : string list
(** [rewrites] is the closed vocabulary of rewrite names: ["not"], the
    comparisons ["lt"], ["le"], ["gt"], ["ge"], ["eq"], ["neq"], the arithmetic
    ["add"], ["sub"], ["fadd"], ["fsub"], the connectives ["and"], ["or"], and
    the statement deletion ["drop"]. A name outside this list is rejected
    wherever it appears — in a site table, in {!arm_variable}, in a verdict file
    — because a rewrite nobody can render is a report nobody can act on. *)

val id_to_string : id -> string
(** [id_to_string id] is [id] in the canonical spelling ["lib/calc.ml:9:12:add"]
    — [file], [line], [col], [rewrite], separated by colons. This is the
    spelling {!arm_variable} documents and verdict files record. *)

val pp_id : Format.formatter -> id -> unit
(** [pp_id ppf id] formats [id] as {!id_to_string}. *)

val compare_id : id -> id -> int
(** [compare_id a b] orders identifiers lexicographically by [file], then
    [line], then [col], then [rewrite]. This is the order {!catalogue},
    {!records} and {!to_string} use, so equal collections serialize identically.
*)

val equal_id : id -> id -> bool
(** [equal_id a b] is [compare_id a b = 0]. *)

(** {1:catalogue Sites and registration}

    The functions of this section are the contract [ppx_windtrap.mutate]
    generates against; user code and the windtrap core never call them. The
    generated code per instrumented file is {b exactly one binding}:

    {[
      let ___windtrap_armed___ =
        Windtrap_mutate.register ~file:"lib/calc.ml"
          ~sites:
            [|
              {
                line = 9;
                col = 12;
                rewrite = "add";
                before = "a - b";
                after = "a + b";
                dismissed = None;
              };
            |]
    ]}

    and every site in that file expands to a guard on [___windtrap_armed___ i],
    where [i] is the site's index in [sites]. The instrumenter names no array
    and allocates nothing: the fewer literals it emits, the fewer ways it can be
    wrong. *)

type site = {
  line : int;  (** 1-based line of the mutated expression's first byte. *)
  col : int;  (** 0-based column of the mutated expression's first byte. *)
  rewrite : string;  (** The replacement's name, from {!rewrites}. *)
  before : string;  (** The original expression's source text. *)
  after : string;  (** The armed expression's source text. *)
  dismissed : string option;
      (** [Some reason] when the site carries [[@mutate off]]; the loop skips it
          and [report] mode lists it with its reason. [None] otherwise. *)
}
(** The type for mutation sites: one entry of a file's site table. *)

val register : file:string -> sites:site array -> int -> bool
(** [register ~file ~sites] records [file]'s site table and is that file's guard
    closure. The reach and epoch arrays are allocated here, not by the caller,
    and are captured by the closure — so {b site indices are file-local}. A
    single global array indexed by an absolute identifier would be indexed
    before every file had registered (link order decides) and would read out of
    bounds; there is no absolute index anywhere in this interface.

    [sites] is kept, not copied — {!catalogue} and {!drain} read it — so the
    generated literal must not be mutated afterwards. Registration happens at
    module load, and the linker drops modules the binary never references, so an
    executable catalogues exactly the instrumented files it links.

    The guard, applied to a site index [i], does three things, in this order:

    + increments site [i]'s reach count (saturating at [max_int]);
    + if site [i]'s epoch differs from the current epoch, sets it and marks the
      site for the next {!drain} — this is how the reach map is built, at one
      epoch compare per evaluated site;
    + is [true] iff site [i] is the armed one.

    It raises {!Runaway} when the armed site's reach count passes the budget
    given to {!arm}, and [Invalid_argument] if [i] is outside [sites] (an
    instrumenter bug — the index is a literal the instrumenter emits beside the
    table). With nothing armed it allocates nothing after each site's first
    evaluation in an epoch, and its answer is [false].

    The guard is not synchronized. A site evaluated from several domains at once
    can lose hits and, at an epoch boundary, its mark — an under-report, so a
    window can miss a mutant but can never invent one. Windtrap spawns no
    domains; code under test that does gets a reach map that is a lower bound.

    Registering [file] again with a site table equal to the previous one is
    allowed (the same source compiled into two modules): both registrations arm
    together, the catalogue counts the file's mutants once, and {!drain} reports
    each mutant once. A registration whose table {e differs} from an earlier one
    for the same [file] means the executable links two incompatible
    instrumentations of one source — arming it would be meaningless, so it is
    dropped with a warning on [stderr] and the returned guard is inert ([false]
    at every index). It is not an exception because [register] runs at module
    load inside the user's program.

    Raises [Invalid_argument] if any site has [line < 1], [col < 0], or a
    [rewrite] outside {!rewrites} — a malformed table can only come from a
    broken instrumenter, and fails fast. *)

type mutant = {
  id : id;  (** This mutant's identifier. *)
  before : string;  (** The original expression's source text. *)
  after : string;  (** The armed expression's source text. *)
  dismissed : string option;  (** The [[@mutate off]] reason, if any. *)
}
(** The type for catalogued mutants: a {!type:site} with its file, as everything
    outside the instrumenter sees it. *)

val compare_mutant : mutant -> mutant -> int
(** [compare_mutant a b] is {!compare_id} on their identifiers, which name the
    same rewrite of the same expression exactly when they are equal. *)

val catalogue : unit -> mutant list
(** [catalogue ()] is every mutant registered in this executable, ordered by
    {!compare_mutant} and without duplicates. The order does not depend on link
    order, so two runs of the same binary enumerate mutants identically. This is
    the population the loop iterates: it is complete only after module
    initialization, since registration happens at module load and the linker
    drops modules the binary never references. *)

(** {1:arming Arming}

    At most one mutant is armed per process. Arming is loud: an identifier that
    matches more than one site, or that names a site of a file this executable
    {e does} catalogue and matches none of them, is an error naming the
    candidates and never a silent no-op — a silently ignored arming turns a
    green run into a false survivor.

    The one case that is not a mistake is {!Uncatalogued}, and it is separated
    from {!Unmatched} here because only the registry can tell the two apart. One
    identifier is normally handed to {e every} test executable of a project at
    once — [WINDTRAP_MUTATE_ARM=<id> dune runtest] is the spelling the report
    prints, because a command that links no test executable has no single binary
    to name — and in a project with several [(test)] stanzas most of those
    executables were built from other sources entirely. Such an executable holds
    no such mutant, produces no verdict, and hides nothing by running on.
    Whether that is worth refusing over is the caller's decision — the mutation
    loop makes it, and lets such a run proceed — but only this module can say
    which of the two cases the identifier is in. *)

(** The type for arming errors. All are recoverable: the loop prints them via
    {!pp_arm_error}, and refuses to start on all but {!Uncatalogued}. *)
type arm_error =
  | Malformed of { spec : string; reason : string }
      (** [spec] is not a mutant identifier; [reason] says why. *)
  | Uncatalogued of { id : id }
      (** This executable catalogues no site at all in [id]'s file, so the
          identifier is about some other binary — or about no binary, if nothing
          was built with the mutation backend. Nothing is armed and nothing is
          concealed: an executable that catalogues none of a file's sites cannot
          produce a verdict about them either way. *)
  | Unmatched of { id : id; candidates : mutant list }
      (** [id]'s file is catalogued here, but no site in it matches the line,
          column and rewrite. [candidates] are that file's mutants and are never
          empty — the empty case is {!Uncatalogued}. This is a wrong or stale
          identifier: the caller believes it named a mutant of {e this}
          executable and it did not. *)
  | Ambiguous of { id : id; candidates : mutant list }
      (** [id] matches more than one site; [candidates] lists them, one entry
          per site. The instrumenter cannot emit such a table, so this is a
          rewriter that duplicated a location: no identifier can separate the
          sites, and the remedy is [[@mutate off]] on the expression or
          excluding the file. *)

val pp_arm_error : Format.formatter -> arm_error -> unit
(** [pp_arm_error ppf e] formats a human-readable message for [e], naming the
    candidates and, where there is one, the likely fix. *)

val arm_variable : string
(** [arm_variable] is ["WINDTRAP_MUTATE_ARM"], the environment variable
    {!arm_from_env} reads. Deliberately not ["WINDTRAP_MUTANT"]: two variables
    differing by two characters and meaning unrelated things is a defect. *)

val scope_variable : string
(** [scope_variable] is ["WINDTRAP_MUTATE_ONLY"]: a comma-separated list of
    source path prefixes limiting which files this process has mutants in.

    Applied at {!register}, not at reporting. A mutation run forks once per
    mutant, so a scope that only narrowed the report would still cost the whole
    run; narrowing the registry narrows the work, leaves the guard inert for
    every out-of-scope file (their reaches are not even counted), and makes an
    executable with nothing in scope indistinguishable from an uninstrumented
    one — {!catalogue} is empty and the seam declines by name. Because
    registration happens at module load, the variable is read once, at the first
    one; setting it later in the process changes nothing.

    Consequently it also bounds {!arm}: a mutant of a file out of scope was
    never registered, so it cannot be armed. Scoping a run is a statement about
    what that run's mutation surface {e is}, not a view over a larger one. *)

val id_of_string : string -> (id, arm_error) result
(** [id_of_string s] parses a mutant identifier in its canonical spelling. It is
    [Error (Malformed _)] if [s] has the wrong shape, if a number is missing or
    negative, if [line < 1], if [col < 0], or if the rewrite is not in
    {!rewrites} — an unrecognized rewrite is a parse error and never an
    identifier that silently matches nothing.

    Round trip: [id_of_string (id_to_string id)] is [Ok id] for every [id] this
    module produces. *)

val arm : ?budget:int -> id -> (mutant, arm_error) result
(** [arm id] arms the single mutant [id] names and is that mutant, and is
    {!Uncatalogued} when this executable holds no site of [id]'s file at all,
    {!Unmatched} when it holds that file's sites but none [id] names, and
    {!Ambiguous} when [id] names several. Any previously armed mutant is
    disarmed first, whether or not [id] resolves. From then on the guard of that
    site — and of every module registering an equal table for its file — answers
    [true]. At most one mutant is armed per process, so this is the only way the
    answer is ever [true].

    [budget] caps the armed site's reach count: the guard raises {!Runaway} on
    the evaluation that would take the count past [budget]. The count is the
    site's, {e since the last} {!reset_reach} — not since this call — so a child
    that inherited the dry run's counts must {!reset_reach} after arming, which
    is what the loop does. The loop sets [budget] from the hit count the dry run
    measured for the site, which is what catches a mutant that spins without
    consuming wall clock in a place a timer can see. It defaults to no cap.

    Two sites of one file that agree on position and rewrite are {!Ambiguous},
    not an arming: no identifier separates them and one of them would stay live,
    reporting a false survivor for code reached through it.

    Raises [Invalid_argument] if [budget] is not positive; nothing is disarmed
    in that case. *)

val arm_from_env : ?budget:int -> unit -> (mutant option, arm_error) result
(** [arm_from_env ()] is [Ok None] when {!arm_variable} is unset or empty, and
    otherwise {!arm}s the identifier it holds, as [Ok (Some m)]. Parse and
    resolution failures are reported, never ignored — including {!Uncatalogued},
    which is reported as the error it is a case of and left to the caller to
    read as "not mine" rather than turned into [Ok None] here: a process that
    ran with nothing armed and a process that was never asked to arm anything
    print different things. [budget] is as in {!arm}. *)

val disarm : unit -> unit
(** [disarm ()] clears the armed mutant and the runaway budget. The guard is
    [false] at every site afterwards. *)

val armed : unit -> mutant option
(** [armed ()] is the currently armed mutant, [None] when none is. A process
    whose [armed ()] is [None] is observationally the original program. *)

val armed_hits : unit -> int
(** [armed_hits ()] is how many times the armed site has been evaluated since
    the last {!reset_reach} — the same count {!arm}'s [budget] caps, summed over
    every module registering the site's file — and [0] when nothing is armed. It
    is what lets a run that armed a mutant and stayed green tell "the tests
    prove nothing about this site" from "no test ran it": a caller wanting the
    run's own count calls {!reset_reach} after arming, as the loop's children
    do. *)

exception Runaway of { id : id; hits : int; budget : int }
(** Raised by the guard when the armed site [id] has been evaluated [hits] times
    since the last {!reset_reach} and [hits > budget]. It escapes into the
    mutated program, where the child's top-level wrapper reduces it to a killed
    verdict: a mutant that turns a terminating loop into a spinning one is
    killed by its budget, not by the clock. The site's count keeps rising, so a
    caller that swallows the exception and evaluates the site again gets it
    again. *)

(** {1:reach The reach map}

    Which tests evaluate which mutants is measured, not guessed, and it is what
    reduces the loop from [M × T] to something a laptop finishes. The runtime
    provides two total operations over the epoch counter and the dirty list, and
    takes no position on what an epoch means; the loop supplies that by calling
    them from the runner's events:

    - on [Test_started]: {!drain} — whatever accumulated since the previous
      drain was evaluated {e outside} any test (module initialization before the
      first test, fixture release after the previous one), and is reported as
      not armable rather than unreached — then {!next_epoch};
    - on [Test_finished]: {!drain}, whose result is the set of mutants that test
      evaluated, with their hit counts;
    - at run end: {!drain} once more, for the last test's teardown.

    Cost is [O(sites that test touched)] per test, never [O(total sites)]. *)

type reached = {
  mutant : mutant;  (** The mutant evaluated. *)
  hits : int;
      (** How many times its guard was evaluated during the drained window,
          saturating at [max_int]. *)
}
(** The type for one entry of a drained window. *)

val next_epoch : unit -> unit
(** [next_epoch ()] opens a fresh observation window. Sites evaluated after it
    mark themselves for the next {!drain}, whether or not they were evaluated
    before. *)

val drain : unit -> reached list
(** [drain ()] is the mutants marked since the previous [drain], ordered by
    {!compare_mutant} and without duplicates, each with the hits it accumulated
    since it was marked; it then empties the mark list. A mutant registered by
    two modules for one source file appears once, with its hit counts added.
    [drain ()] immediately after another [drain ()] is [[]].

    A site marks itself the {e first} time it is evaluated in an epoch, so under
    the protocol above a drain at [Test_finished] is exactly the set that test
    evaluated. A consequence worth knowing: a site the test already evaluated
    and its teardown evaluates again is not marked a second time, so it is
    reported for the test (which is where it belongs — it is armable) and the
    teardown's extra hits are reported nowhere. A budget derived from a drained
    hit count therefore wants headroom. *)

val reset_reach : unit -> unit
(** [reset_reach ()] zeroes every site's reach count, empties the dirty list and
    opens a fresh epoch. A forked child calls it before its first test, so that
    the runaway budget measures the child's own hits and not the dry run's
    accumulated ones. It does not disarm. *)

(** {1:verdicts Verdicts}

    Three verdicts, never a boolean and never an exit code. There is
    deliberately no fourth {e errored} verdict: under the child's
    [bail = Some 1] a killed child always runs fewer tests than were selected,
    so any verdict keyed on "ran fewer tests than expected" would fire on every
    kill. A failure of the parent's own supervision — a [fork] or [waitpid] that
    fails — aborts the run and names the errno instead, because a score over an
    unknown number of unsupervised children is not a score. *)

type witness = string list
(** The type for test paths: the names from the run root inwards, e.g.
    [["calc"; "arithmetic"; "adds"]]. *)

(** The type for mutant verdicts. *)
type verdict =
  | Killed
      (** A test failed, or the child crashed or hung: divergence is a detected
          behaviour change however it arrived, and the report carries one
          killed count. *)
  | Survived of { witness : witness; others : witness list }
      (** Every test that reached the mutant passed; [witness] and [others] are
          those tests, sorted and without duplicates.
          {e Strengthen one of them.}

          The witnesses are split so that
          {b a survivor always names at least one test}: a mutant no test
          reached is {!Unreached} and is never forked, so a survivor with an
          empty witness list would be a report contradicting itself — "no test
          ran this line and none failed when it changed". Build one with
          {!survived} rather than by hand. *)
  | Unreached
      (** No test evaluated the site. Never scored as survived — the remedy is
          to write a test, not to strengthen one. *)

val survived : witness list -> verdict
(** [survived ws] is the {!Survived} verdict whose witnesses are [ws] sorted and
    without duplicates — the constructor for a caller holding the tests that
    reached the mutant and passed.

    Raises [Invalid_argument] if [ws] is empty. *)

val merge_verdict : verdict -> verdict -> verdict
(** [merge_verdict a b] is the verdict of a mutant observed as [a] by one test
    executable and [b] by another. {b Killed anywhere wins}: the result is
    [Killed] if either is, [Survived] only if every executable that reached it
    survived, and [Unreached] only if neither reached it. A merged [Survived]'s
    witnesses are the union.

    This is the load-bearing rule of the whole file format. A library covered by
    several [(test)] stanzas is the normal case, and a mutant killed by suite A
    while merely reached by suite B is {e killed}; reporting B's view alone
    produces a false survivor, which sends the reader to write a test that
    already exists.

    The operation is commutative, associative and idempotent, with [Unreached]
    as its unit — so merging any number of files in any order gives one
    answer. *)

val pp_verdict : Format.formatter -> verdict -> unit
(** [pp_verdict ppf v] formats [v] for diagnostics — ["killed"],
    ["survived by calc > adds"], ["unreached"]. The report renders its own
    layout; this output is not stable. *)

(** {1:files Verdict files}

    Each instrumented test executable's mutation run writes one verdict file
    under [_build/_mutants]; [windtrap mutate] loads them all, {!merge}s them,
    and renders the survivors that survive {e everywhere}. The catalogue never
    touches disk — only verdicts do.

    The format is versioned by the magic string [windtrap-mutants-v3] on the
    first line; {!of_string} and {!load} reject any other header loudly, and
    cross-version compatibility is not promised. The magic line may be followed
    by the writing executable's {!type:identity}, which the merge uses to
    exclude verdicts whose executable was deleted or rebuilt since the run.

    A file is {b self-describing}: each {!type:record} carries not only the
    mutant's identifier and verdict but the [before]/[after] renderings the
    report draws it with. The catalogue does not travel — it
    lives inside the instrumented binary, which the merging command never links
    — so a record naming only an identifier would produce a project-level report
    strictly worse than the per-executable one it replaces. It is also what lets
    a report outlive the executable that produced it. *)

(** The type for verdict-file errors. All are recoverable: the reporting command
    prints them via {!pp_error} and exits nonzero. There is no mismatch error
    here, unlike coverage's point tables: {!merge_verdict} is total, so two
    files can disagree about a mutant without either being corrupt. *)
type error =
  | Unknown_format of { path : string; header : string }
      (** [path] does not start with this version's magic string; [header] is
          its escaped first line. Files written by other windtrap versions are
          rejected, not converted. *)
  | Unreadable of { path : string; reason : string }
      (** [path] cannot be read; [reason] is the system message. *)
  | Corrupt of { path : string; reason : string }
      (** [path] has the right magic but malformed data. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf e] formats a human-readable message for [e], including the
    likely fix. *)

type record = {
  id : id;  (** The mutant's identifier. *)
  before : string;  (** The original expression's source text. *)
  after : string;  (** The armed expression's source text. *)
  verdict : verdict;  (** What the run made of the mutant. *)
}
(** The type for one verdict-file record: a mutant, as much of it as a report
    needs to draw, and its verdict. A mutant dismissed by [[@mutate off]] has no
    record: it is never forked and never receives a verdict. *)

val record_of_mutant : mutant -> verdict -> record
(** [record_of_mutant m v] is [m]'s record with verdict [v] — the identifier and
    renderings of [m], which is what the loop holds when a child reports. [m.dismissed] is dropped, having no meaning for a mutant that was
    tested. *)

type t
(** The type for verdict collections: a finite map from {!type:id} to its
    {!type:record}. Immutable. *)

val empty : t
(** [empty] is the collection with no records. *)

val is_empty : t -> bool
(** [is_empty t] is [true] iff [t] holds no records. *)

val add : t -> record -> t
(** [add t r] is [t] with [r] recorded, combined with any record already under
    [r.id]: the verdicts through {!merge_verdict}, and the renderings by keeping
    the lexicographically smaller [(before, after)] of the two.

    Two records for one identifier are expected to agree on the rendering, and
    can disagree only if they came from different builds of one source — where
    nothing in the data says which build the reader is looking at. The rule is
    therefore picked for determinism rather than for cleverness: it keeps {!add}
    and {!merge} commutative, associative and idempotent, so a report never
    depends on the order the files happened to be read in. Survivor witnesses
    are sorted and deduplicated for the same reason. *)

val find : t -> id -> record option
(** [find t id] is [id]'s record in [t], [None] when [t] has none. *)

val records : t -> record list
(** [records t] is [t]'s records ordered by {!compare_id}. *)

val merge : t -> t -> t
(** [merge a b] is the union of [a] and [b], combining shared identifiers as
    {!add} does. Commutative, associative and idempotent, with {!empty} as its
    unit — so merging any number of verdict files in any order gives one answer.
*)

type identity = Windtrap_instr.identity = { exe : string; digest : string }
(** The type for verdict-file writer identities — [Windtrap_instr]'s,
    re-exported, so the reporting command handles both runtimes' identities
    with one pass: [exe] is the writing executable's {!exe_identity} and
    [digest] the lowercase hex MD5 of its contents at write time. An
    executable at [exe] whose digest differs is {e not} the one that wrote the
    file — the content comparison survives rebuilds that dune's cache restores
    with their original timestamps, which mtimes do not. *)

val exe_identity : exe:string -> string
(** [exe_identity ~exe] is the [exe] field a verdict file records for the
    executable at path [exe]: its path below the topmost [_build] directory
    (with any [.sandbox/<digest>] prefix removed, so sandboxed and direct runs
    record the same identity), or its absolute path when [exe] is not under a
    [_build] directory. The reporting command resolves a relative identity
    against the file's own {!build_root} to detect deleted or rebuilt
    executables. *)

val writer_identity : exe:string -> identity option
(** [writer_identity ~exe] is the {!type:identity} to record when writing a
    verdict file on behalf of the executable at [exe]: its {!exe_identity} and
    the hex MD5 of its bytes, and [None] when [exe] cannot be read. Digesting
    reads the executable once (a few milliseconds for a typical test binary),
    off the test path. *)

val build_root : path:string -> string option
(** [build_root ~path] is the parent directory of the topmost [_build] component
    of [path] (resolved against the current directory when relative), and [None]
    when [path] has no [_build] component. This is the project-root rule shared
    by {!output_file}, {!exe_identity}, and the reporting command's file
    discovery — one rule, so a file written from inside dune's sandbox and a
    report run from anywhere in the checkout resolve the same root. *)

val output_file : exe:string -> string
(** [output_file ~exe] is the deterministic verdict-file path for the executable
    at path [exe] (resolved against the current directory when relative):
    [<root>/_build/_mutants/windtrap-<hash>.mutants], where [<root>] is the
    parent of the topmost [_build] component of [exe] and [<hash>] is the hex
    digest of [exe]'s path below [_build] (with any [.sandbox/<digest>] prefix
    removed, so sandboxed and direct runs write the same file). When [exe] is
    not under a [_build] directory, [<root>] is the current directory and the
    full path of [exe] is hashed.

    The name depends on the executable's path: renaming or moving a test
    executable orphans its previous verdict file. The reporting command detects
    orphans through the recorded {!exe_identity} and excludes them with a
    warning. *)

val to_string : ?identity:identity -> t -> string
(** [to_string t] is [t] serialized in the verdict-file format. Deterministic:
    records are ordered by {!compare_id} and witnesses are sorted, so equal
    collections serialize identically regardless of construction order.
    [identity] is recorded after the magic line when given; a merged collection,
    which has no single writer, serializes without one.

    Raises [Invalid_argument] if [identity.exe] is [""] or [identity.digest] is
    not 32 lowercase hex characters. *)

val of_string : ?path:string -> string -> (t * identity option, error) result
(** [of_string s] is [Ok (t, id)] when [s] parses: [t] the collection and [id]
    the recorded writer identity, [None] when [s] carries none. [path], used in
    errors, defaults to ["<string>"]. Errors: [Unknown_format] for a foreign
    header, [Corrupt] for truncated or invalid data — a negative or oversized
    count, a line that is not 1-based, a rewrite outside {!rewrites}, an
    unknown verdict tag, a survivor naming no test, a duplicate identifier, a
    malformed identity line, or trailing garbage.
    Nothing is repaired and nothing is guessed: a file this module cannot read
    exactly is not read at all.

    Round trip: [of_string (to_string ?identity t)] is [Ok (t, identity)]. *)

val load : string -> (t * identity option, error) result
(** [load path] reads and parses the verdict file at [path].
    [Error (Unreadable _)] when the file cannot be read; otherwise as
    {!of_string}. *)

val save : ?identity:identity -> string -> t -> unit
(** [save path t] writes [to_string ?identity t] to [path], creating [path]'s
    directory if needed. The write is atomic: the data goes to a uniquely named
    temporary file next to [path], which is then renamed over it, so a reader
    never observes a partial file and a crashed run never leaves a truncated
    one. Re-running replaces the file; verdicts never accumulate on disk.

    Raises [Sys_error] if the file cannot be written, and [Invalid_argument]
    under {!to_string}'s conditions. *)
