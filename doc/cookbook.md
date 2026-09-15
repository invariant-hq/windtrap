# Windtrap cookbook

Recipes for needs windtrap deliberately does not absorb: each is a few
lines of ordinary OCaml over the public surface, and keeping them out of
the API keeps the API small. A pattern that only composes windtrap's own
verbs belongs in the manual chapter that documents them, not here — when
a recipe becomes a way of using the API, it has stopped being a record
of declined surface. Code blocks that would need a dependency windtrap does not have
(Eio) are marked as fragments.

The recipes assume `open Windtrap`.

## 1. Testing under Eio

Windtrap has no Eio integration and needs none: `Eio_main.run` is
already a scoping function, so `scoped` takes it directly (fragment;
not compiled here):

```ocaml
(* fragment: requires eio_main *)
let with_eio = scoped Eio_main.run

let () =
  exit
  @@ run "net"
       [
         with_eio "connects" (fun env ->
             equal string "pong" (Client.ping ~net:(Eio.Stdenv.net env)));
       ]
```

The same applies one level down: `scoped (fun fn -> Eio_main.run @@ fun
env -> Eio.Switch.run @@ fun sw -> fn (env, sw))` hands the body an
environment and a switch. `scoped` calls the scope once and reclaims
nothing itself — cleanup is `Eio_main.run`'s, which it does on both
paths, and the body's failure is re-raised through it so cancellation
sees it ([Resources and structure](manual/resources-and-structure.md)).

The guarantees the combination rests on:

1. **Assertion failures are ordinary exceptions and classify by
   identity, not catch site.** An assertion raised in a non-main fiber
   (a server callback) can be stored in a `ref`, routed across the
   switch, and re-raised at the join point — it is still reported as
   that assertion's structured failure, not as an anonymous exception.
   Wrappers like `Eio.Cancel.Cancelled` and `Fun.protect` re-raises do
   not change how the failure is classified.
2. **`~timeout` still fires inside an event loop.** The per-test limit
   is SIGALRM-based (Unix only); a test blocked inside `Eio_main.run`
   times out, is reported as a timeout of that test, and the run
   continues. It cannot interrupt blocked C calls.

## 2. Subprocess workers: the role-env-var pattern

To test process-level behavior (locks, crashes, cache sharing), re-exec
the test binary itself as a worker, dispatching on an environment
variable *before* `run` is called — so the worker never reaches
windtrap's command-line parsing, and exits on its own terms:

```ocaml
let () =
  match Sys.getenv_opt "MYTEST_ROLE" with
  | Some "worker" ->
      Worker.main ();          (* prints its protocol on stdout *)
      exit 0
  | Some _ | None ->
      exit
      @@ run "locking"
           [
             test "two processes contend" (fun () ->
                 let out = spawn_self ~role:"worker" in
                 contains ~sub:"lock acquired" out);
           ]
```

where `spawn_self` runs `Sys.executable_name` with the role variable
set and drains its output (`Unix.create_process` + a pipe; see the
compiled mirror for a complete `spawn_self`).

This pattern is safe because `run` reads nothing but its `?argv`
parameter (default `Sys.argv`), the documented `WINDTRAP_*` variables,
and ambient CI/terminal detection (`CI`, `GITHUB_ACTIONS`,
`INSIDE_DUNE`, whether stdout is a terminal) — none of which the role
variable perturbs. One care: scrub `WINDTRAP_*` from the child's
environment if the child itself ever calls `run` — a leaked
`WINDTRAP_FILTER` or `WINDTRAP_STREAM` would change the child run's
behavior.

## 3. Two-phase keyed comparison: shape first, then values

For big structured values (tensors, matrices, tables), a mismatch in the
*shape* should fail with the shape diff — not a screenful of values that
differ everywhere because the shapes do. Assert the key first, with its
own message, then the payload:

```ocaml
type tensor = { shape : int array; data : float array }

let equal_tensor ?__POS__ expected actual =
  equal ?__POS__ ~msg:"shape" (array int) expected.shape actual.shape;
  equal ?__POS__ ~msg:"values" (array (float 1e-9)) expected.data actual.data
```

The first `equal` fails fast with `shape: [|3; 4|]` vs `[|4; 3|]`; the
value comparison only ever runs on same-shaped tensors, where the diff
marks the few values that differ instead of a wall of misaligned ones.
Thread `?__POS__` through helpers like this one — inside the helper the
parameter shadows the builtin, so forward it rather than recapture —
and call them with `equal_tensor ~__POS__ expected actual`, so failures
point at the caller.

## 4. A complex-tolerance testable

`float` and `float_rel` cover real tolerances; complex numbers are one
`Testable.make` away — componentwise tolerance, round-trippable
printing:

```ocaml
let complex ~rel ~abs : Complex.t testable =
  let close = Testable.equal (float_rel ~rel ~abs) in
  Testable.make
    ~pp:(fun ppf { Complex.re; im } ->
      Format.fprintf ppf "(%.17g %+.17gi)" re im)
    ~equal:(fun a b ->
      close a.Complex.re b.Complex.re && close a.Complex.im b.Complex.im)
```

The same shape scales to any component-tolerance record; `%.17g` keeps
unequal values from rendering identically. NaN components follow the
underlying witness: equal to nothing under `float_rel` — build on
`float_exact` instead when asserting NaN behavior.

## 5. Scripted seams: the tape

A test double for an effectful dependency wants three things: canned
responses dealt in order, a failure when the code under test asks for
more than the script holds, and a failure when the test ends with
entries never consumed — the silent case, where the interaction you
scripted simply did not happen and nothing said so. The whole of it is
a record and a few functions over the public surface:

```ocaml
type 'a tape = { name : string; mutable entries : 'a list; mutable dealt : int }

let next ?__POS__ t =
  match t.entries with
  | [] -> failf ?__POS__ "tape %s: exhausted after %d entries" t.name t.dealt
  | e :: rest ->
      t.entries <- rest;
      t.dealt <- t.dealt + 1;
      e

let check_consumed t =
  if t.entries <> [] then
    failf "tape %s: %d of %d entries never consumed" t.name
      (List.length t.entries)
      (t.dealt + List.length t.entries)

let with_tape name entries =
  bracket
    ~setup:(fun () -> { name; entries; dealt = 0 })
    ~teardown:check_consumed
```

`bracket` is what makes the end-of-test check unforgeable: the teardown
runs on every outcome, and a teardown assertion after a green body is a
counted failure of the test — the runner's boundary matrix pins exactly
this. Setup runs per attempt, so a retried test deals from a fresh
script. In use, the tape is the seam's implementation:

```ocaml
let () =
  exit
  @@ run "engine"
       [
         with_tape "provider"
           [ Error `Timeout; Ok "done" ]
           "a turn retries once past a transient provider error"
           (fun provider ->
             let engine =
               Engine.create ~provider:(fun _req -> next provider)
             in
             equal string "done" (Engine.run_turn engine "hi"));
       ]
```

If the retry logic is broken and the second entry is never dealt, the
teardown fails the test naming the tape and the counts — where a
hand-rolled fake passes silently, having proved only that the first
response was consumed. Two companions round it out:

```ocaml
let next_opt t =
  match t.entries with
  | [] -> None
  | e :: rest ->
      t.entries <- rest;
      t.dealt <- t.dealt + 1;
      Some e

let remainder t =
  let rest = t.entries in
  t.entries <- [];
  t.dealt <- t.dealt + List.length rest;
  rest
```

`next_opt` is the driver form — exhaustion as the normal stop condition
for a loop that feeds a system to the end of its script — and
`remainder` takes the rest and discharges the check: the explicit
spelling of "the rest may go unused", usually to assert on it directly.

Two boundaries, on purpose. Hold-and-release — delivering a scripted
response only when the test says so, to observe the in-flight state —
is scheduling, not sequencing: make the entry a promise
(`Eio.Promise.t`, or your runtime's equivalent) that the seam awaits,
and the concurrency library owns when it resolves. And a tape is not a
mock framework: there is no call matcher and no expectation DSL here
deliberately — a test that needs several interlocking fakes to check
one line is the over-mocked shape the skill's bad-test catalog rejects.
The tape verifies one thing, the thing hand-rolled fakes silently skip:
this finite interaction budget was consumed, exactly. Faults need no
machinery at all — script an `Error`, or a thunk that raises, at the
position where the failure should happen.

## 6. Counting occurrences

`contains ~sub` asks whether a needle occurs; it never counts. When the
count is the claim — exactly two retries, more than two, a ratio — fold
the count locally, leftmost-first and non-overlapping (each match
resumes the scan at its end, so `"aa"` occurs once in `"aaa"`):

```ocaml
let count ~sub s =
  let n = String.length sub in
  let rec go i acc =
    if n = 0 || i + n > String.length s then acc
    else if String.sub s i n = sub then go (i + n) (acc + 1)
    else go (i + 1) acc
  in
  go 0 0
```

Then assert about the number with the ordinary verbs:
`equal int 2 (count ~sub:"retry" log)` for an exact count, or
`greater int ~than:2 (count ~sub:"retry" log)` for a bound. Both
failures print the number they got; `not_contains ~sub` is still the verb for "never occurs",
and its failure marks the occurrence in the haystack.

## 7. Convergence: driving a system until it settles

Some assertions are about a system that *reaches* a state rather than
one already in it — a writer that flushes once the scheduler runs, a
cache that fills once the worker drains. Windtrap has no verb for it,
because the loop is seven lines and everything that matters is in how
you write the two callbacks:

```ocaml
let eventually ?(attempts = 100) ?diagnose ~step probe =
  let rec go n =
    match probe () with
    | Some v -> v
    | None when n >= attempts ->
        failf "no convergence in %d attempts%s" attempts
          (match diagnose with
          | None -> ""
          | Some d -> ": " ^ String.concat "; " (d ()))
    | None ->
        step ();
        go (n + 1)
  in
  go 1
```

**Probe first, then step.** A system already in the wanted state has
converged; a loop that stepped first would demand one change of a
system that needed none. So a budget of *n* probes drives *n − 1*
steps — the last probe is not followed by a step nothing would read.

**The vacuous-probe hazard.** Probe-first has a corollary you own: a
probe that is true of a system nobody started — `is_settled` on a
scheduler with no work, "queue is empty" before anything was enqueued,
"no errors logged" — converges on the very first probe, and the test
passes having driven nothing. Make the probe carry evidence that the
system actually ran:

```ocaml
(* not: Queue.is_empty pending — already true before anything starts *)
(fun () -> if !replies > 0 && Queue.is_empty pending then Some () else None)
```

**Windtrap never sleeps.** The budget counts probes, not seconds, and
`~step` is yours: put in it the thing that actually advances the system
— a mock clock tick, one turn of an event loop, a queue drained. Then
the convergence you assert is deterministic, and the test runs as fast
as the system does rather than as slowly as your worst-case guess. A
`~step` that only sleeps turns this into a retry loop that hides a race
by outlasting it; that race is a defect in the code under test, and the
loop exists to expose it rather than wait it out.

`failf` splits its message on newlines in the failure block, so a
multi-line `?diagnose` reads as a nested list. Nothing here is a failure
boundary: an exception from `probe` or `step` — a nested assertion's
failure included — propagates as it was raised, which is what you want.
An exception from `diagnose` would replace the verdict it was
decorating, so wrap that callback yourself if it touches state that may
already be broken.
