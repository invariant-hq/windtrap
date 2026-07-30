# Windtrap cookbook

Recipes for needs windtrap deliberately does not absorb: each is a few
lines of ordinary OCaml over the public surface, and keeping them out of
the API keeps the API small. Every recipe here compiles — each is
mirrored as a test in `test/facade/test_cookbook.ml`, so a recipe that
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

Mutating the environment in a test must restore it on every path, or one
failing test poisons the rest of the run:

```ocaml
let with_env var value fn =
  let saved = Sys.getenv_opt var in
  Unix.putenv var value;
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv var (match saved with Some v -> v | None -> ""))
    fn
```

Two facts to know:

- **`putenv` cannot unset.** If `var` was unset before the call, the
  restore above leaves it *set to `""`* — the POSIX interface has no
  portable unset. Code under test should treat the empty string as
  absent (`match Sys.getenv_opt var with Some "" | None -> ... `), or
  the test should assert against a variable it sets in both branches.
- **The environment is per-process.** Windtrap's runner is sequential,
  so this is safe today; the recipe is a hazard class under any future
  parallel runner, which is one reason it stays a recipe.

## 3. Testing under Eio

Windtrap has no Eio integration — an event loop per test is one line —
but two contract points make the combination safe. The adapter every
Eio suite writes (fragment; not compiled here):

```ocaml
(* fragment: requires eio_main *)
let with_eio fn () = Eio_main.run @@ fun env -> fn env ()

let () =
  run "net"
    [ test "connects" (with_eio (fun env () ->
          equal string "pong" (Client.ping ~net:(Eio.Stdenv.net env)))) ]
```

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

## 5. Comparing event sets: `slist` + `contramap`

"Did these events happen, in any order, ignoring the noisy fields" is a
projection followed by a multiset comparison — both already exist:

```ocaml
type event = { path : string; kind : string; timestamp : float }

let key e = (e.path, e.kind)                     (* drop the noise *)
let event = contramap key (pair string string)   (* compare/print the key *)
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

## 6. `cover` thresholds and the noise floor

`cover ~label ~at_least` turns a distribution expectation into a test
outcome — and a threshold set too close to the true rate flakes on
unlucky seeds. At the default 100 cases the observed percentage of a
50% condition swings roughly ±10 points either way (binomial noise), so:

**Rule of thumb: keep `at_least` at least 10–15 percentage points below
the rate the generator actually achieves at `count = 100`, or raise
`~count` until the margin holds.**

```ocaml
(* even numbers are ~50% of small_int draws; demand far less *)
prop "parity is exercised" ~count:200 Gen.small_int (fun n ->
    cover ~label:"even" ~at_least:20. (n mod 2 = 0);
    cover ~label:"odd" ~at_least:20. (n mod 2 <> 0);
    equal int n n)
```

`classify` and `collect` report the achieved distribution without
failing anything — run with `-v` to see the distribution of a passing
property, then set the threshold with margin (a failing property's
block always includes the table).

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
value comparison only ever runs on same-shaped tensors, where the
renderer's first-mismatch reporting ("differ at 3 of 100 elements;
first at [37]") does its job. Thread `?pos` through helpers like this
one so failures point at the caller.

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
