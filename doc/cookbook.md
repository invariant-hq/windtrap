# Windtrap cookbook

Recipes for needs windtrap deliberately does not absorb: each is a few
lines of ordinary OCaml over the public surface, and keeping them out of
the API keeps the API small. Every recipe here compiles — each is
mirrored as a test in `test/docs/test_cookbook.ml`, so a recipe that
rots breaks the build. Code blocks that would need a dependency windtrap
does not have (Eio) are marked as fragments; their *guarantees* are
tested instead.

The recipes assume `open Windtrap`.

## 1. Temporary directories and files

Prefer the built-ins: `temp_dir ()` and `temp_file ()` are created lazily
per test and removed by the runner after the test on every outcome —
failure, skip, and timeout included. There is no lifecycle to write:

```ocaml
test "writes a config" (fun () ->
    let dir = temp_dir () in
    let file = Filename.concat dir "config.json" in
    Config.write file;
    is_true (Sys.file_exists file))
```

Reach for a hand-rolled scope only when the directory must disappear
*before* the test ends (testing cleanup behavior itself) or outside a
run. The canonical shape — cleanup on the raise path included:

```ocaml
let rec rm_rf path =
  if Sys.is_directory path then begin
    Array.iter (fun name -> rm_rf (Filename.concat path name))
      (Sys.readdir path);
    Sys.rmdir path
  end
  else Sys.remove path

let with_temp_dir fn =
  let dir = Filename.temp_file "test-" ".dir" in
  Sys.remove dir;
  Sys.mkdir dir 0o700;
  Fun.protect ~finally:(fun () -> rm_rf dir) (fun () -> fn dir)
```

The `Fun.protect` is the point: a version that removes the directory
after `fn dir` leaks it on every failing test.

## 2. Scoped environment variables

Prefer the built-ins: `setenv name (Some v)` binds and `setenv name
None` unbinds — a real unbinding, `Sys.getenv_opt` answers `None` — for
the rest of the test, and the runner restores what the variable held
before the test's first `setenv` of it, on every outcome: failure,
skip, and timeout included. There is no lifecycle to write:

```ocaml
test "a missing token is refused, an empty one is not a token" (fun () ->
    setenv "API_TOKEN" (Some "test-token");
    equal string "test-token" (Client.token ());
    setenv "API_TOKEN" None;
    raises Missing_token (fun () -> ignore (Client.token ())))
```

Reach for a hand-rolled scope only outside a run — a setup script, a
tool. The canonical shape, and the limitation that keeps it inferior to
the built-in:

```ocaml
let with_env var value fn =
  let saved = Sys.getenv_opt var in
  Unix.putenv var value;
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv var (match saved with Some v -> v | None -> ""))
    fn
```

**`putenv` cannot unset.** If `var` was unset before the call, the
restore above leaves it *set to `""`* — the POSIX interface OCaml's
`Unix` exposes has no unset, which is exactly why `setenv`'s `None`
goes through a real `unsetenv` stub instead. Under this recipe, code
that distinguishes unset from empty stays untestable.

## 3. Testing under Eio

Windtrap has no Eio integration and needs none: `Eio_main.run` is
already a scoping function, so `scoped` takes it directly (fragment;
not compiled here):

```ocaml
(* fragment: requires eio_main *)
let with_eio = scoped Eio_main.run

let () =
  run "net"
    [ with_eio "connects" (fun env ->
          equal string "pong" (Client.ping ~net:(Eio.Stdenv.net env))) ]
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

## 4. Subprocess workers: the role-env-var pattern

To test process-level behavior (locks, crashes, cache sharing), re-exec
the test binary itself as a worker, dispatching on an environment
variable *before* `run` is called — so the worker never touches
windtrap's CLI parsing or process exit:

```ocaml
let () =
  match Sys.getenv_opt "MYTEST_ROLE" with
  | Some "worker" ->
      Worker.main ();          (* prints its protocol on stdout *)
      exit 0
  | Some _ | None ->
      run "locking"
        [ test "two processes contend" (fun () ->
              let out = spawn_self ~role:"worker" in
              contains ~sub:"lock acquired" out) ]
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
`WINDTRAP_UPDATE` or `WINDTRAP_STREAM` would change the child run's
behavior.

## 5. Comparing event sets: `slist` + `Testable.contramap`

"Did these events happen, in any order, ignoring the noisy fields" is a
projection followed by a multiset comparison — both already exist:

```ocaml
type event = { path : string; kind : string; timestamp : float }

let key e = (e.path, e.kind)                     (* drop the noise *)
let event = Testable.contramap key (pair string string)  (* on the key *)
let events = slist event (fun a b -> compare (key a) (key b))

(* order-insensitive, timestamp-insensitive: *)
equal events
  [ { path = "a"; kind = "created"; timestamp = 0. }
  ; { path = "b"; kind = "removed"; timestamp = 0. } ]
  observed
```

`slist` sorts both sides with the comparator before elementwise
comparison, so order is ignored but multiplicity is not; `contramap`
makes both equality and the failure rendering go through the projection,
so the diff shows exactly the fields the test is about.

## 6. Gating on generator reach with `cover`

`cover label cond` fails the property unless at least one passing case
marked the label — the CI gate on generator quality, where `classify`
only prints a table a human reads under `-v`:

```ocaml
prop "parity is exercised" ~count:200 Gen.small_int (fun n ->
    cover "even" (n mod 2 = 0);
    cover "odd" (n mod 2 <> 0);
    equal int n n)
```

Presence, not proportion, and deliberately: a percentage gate over a
random sample flakes near its threshold, and the margin that stops it
flaking is wide enough to stop it catching anything short of the region
vanishing. When the proportion is what you want to know, read
`classify`'s table.

Put the `cover` where the body always reaches it. The demand registers
at the call, so one written inside the branch it is meant to police
registers nothing on the runs where that branch is never taken — vacuous
exactly when it should fire.

## 7. Skipping a whole suite on a missing resource

A `skip` raised during fixture acquisition is cached as a skip: the
acquiring test skips with that reason, and every later use of the
fixture in the run skips with the same reason — the probe runs once, and
an unavailable device never turns the run red:

```ocaml
let cuda =
  fixture (fun () ->
      match Cuda.init () with
      | Ok device -> device
      | Error msg -> skip ~reason:msg ())

let tests =
  [ test "elementwise" (fun () -> check_elementwise (cuda ()))
  ; test "reduction" (fun () -> check_reduction (cuda ())) ]
```

For a gate that is not a resource (platform, missing binary), the
per-test spelling stays the honest one: a `require_foo ()` helper
calling `skip ~reason` as the body's first line.

## 8. Codec round-trips

Every codec gets one property: decoding inverts encoding. Generate the
*decoded* form, and assert with `equal` so the counterexample prints a
structured diff at the shrunk input:

```ocaml
let encode l = String.concat "," (List.map string_of_int l)
let decode = function
  | "" -> []
  | s -> List.map int_of_string (String.split_on_char ',' s)

let tests =
  [ prop "decode inverts encode" Gen.(list small_int) (fun l ->
        equal (list int) l (decode (encode l))) ]
```

When only some values are representable, generate the representable
subset by construction (not `assume`), and add the one-way property for
the rest (`decode` of arbitrary input never raises, or errors cleanly).

## 9. Two-phase keyed comparison: shape first, then values

For big structured values (tensors, matrices, tables), a mismatch in the
*shape* should fail with the shape diff — not a screenful of values that
differ everywhere because the shapes do. Assert the key first, with its
own message, then the payload:

```ocaml
type tensor = { shape : int array; data : float array }

let equal_tensor ?pos expected actual =
  equal ?pos ~msg:"shape" (array int) expected.shape actual.shape;
  equal ?pos ~msg:"values" (array (float 1e-9)) expected.data actual.data
```

The first `equal` fails fast with `shape: [|3; 4|]` vs `[|4; 3|]`; the
value comparison only ever runs on same-shaped tensors, where the diff
marks the few values that differ instead of a wall of misaligned ones.
Thread `?pos` through helpers like this one so failures point at the
caller.

## 10. A complex-tolerance testable

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

## 11. Scripted seams: the tape

A test double for an effectful dependency wants three things: canned
responses dealt in order, a failure when the code under test asks for
more than the script holds, and a failure when the test ends with
entries never consumed — the silent case, where the interaction you
scripted simply did not happen and nothing said so. The whole of it is
a record and a few functions over the public surface:

```ocaml
type 'a tape = { name : string; mutable entries : 'a list; mutable dealt : int }

let next ?pos t =
  match t.entries with
  | [] -> failf ?pos "tape %s: exhausted after %d entries" t.name t.dealt
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
  run "engine"
    [
      with_tape "provider"
        [ Error `Timeout; Ok "done" ]
        "a turn retries once past a transient provider error"
        (fun provider ->
          let engine = Engine.create ~provider:(fun _req -> next provider) in
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

## 12. Counting occurrences

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
`satisfies ~claim:"more than 2 retries" int (fun n -> n > 2)
(count ~sub:"retry" log)` for a bound. Both failures print the number
they got; `not_contains ~sub` is still the verb for "never occurs",
and its failure marks the occurrence in the haystack.

## 13. Convergence: driving a system until it settles

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
