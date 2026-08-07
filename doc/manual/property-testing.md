# Property testing

A property checks a law over generated inputs: `prop name gen law`
draws values from an `'a Gen.t`, runs `law` on each (100 cases by
default), and on failure shrinks the input to a minimal
counterexample. Shrinking is integrated — there is no shrink function
to write, ever. Bodies return `unit` and use the ordinary assertion
vocabulary, so an `equal` failing inside a property reports its
structured diff at the shrunk counterexample:

```ocaml
prop "decode inverts encode"
  Gen.(list string)
  (fun fields ->
    equal (list string) fields (decode (encode fields));
    classify "empty" (fields = []))
```

With `encode = String.concat ","`, this law is false for field values
containing commas — and for `[""]`:

```
$ dune runtest
mytool: 1 test (seed s1:fdf792804ac422b3)
F
──────────────────── failures (1) ────────────────────
  FAIL  decode inverts encode
    test/test_codec.ml:14
      14 │       prop "decode inverts encode"

    counterexample (case 0, shrunk 3 steps): [""]
    which failed at:
      test/test_codec.ml:17
      expected  [""]
                 ~~
      actual    []
    replay: dune exec test/test_codec.exe -- --seed s1:fdf792804ac422b3 -f 'decode inverts encode'
──────────────────────────────────────────────────────

1 failed in 0.000707s.
rerun failures only: dune exec test/test_codec.exe -- --failed
```

Not the 40-element list that first failed: the minimal one. The
replay line reruns exactly this failure.

## Seeds and replay

Every generated value derives deterministically from the run's root
seed (the `s1:…` token in the run header), the test's path, and the
case index. Consequences worth knowing:

- The printed root seed replays *every* failure of the run — paste the
  replay line from the report. It is spelled for the way the run was
  invoked: `dune exec … -- --seed s1:… -f '…'` under dune, argv0 when
  run directly, and a `WINDTRAP_SEED=… WINDTRAP_FILTER='…'
  dune runtest` prefix for inline (`ppx_windtrap`) suites.
- Adding, removing, or reordering other tests never perturbs a
  property's stream; renaming or regrouping the test re-keys it.
- `--seed s1:…` (or `WINDTRAP_SEED`) pins the whole run; otherwise
  each run draws a fresh root, so CI keeps exploring.

`~count` (or `--prop-count N` / `WINDTRAP_PROP_COUNT`) changes the
case budget; the declaration site wins over the flag.

## Generators

`Gen` is the generator vocabulary — each generator carries generation,
shrinking, *and* printing, inseparably:

- numeric: `int`, `nat`, `small_int`, `int_range`, `int32`, `int64`,
  `float`, `float_any`, `float_range`
- base: `bool`, `char`, `char_range`, `string`, `string_of ?size char`,
  `bytes`, `bytes_of`
- containers: `list ?size`, `array ?size`, `option`, `result`, `pair`,
  `triple`, `quad`
- choice: `constant`/`pure`, `of_list`, `one_of`, `frequency`,
  `sized`, `such_that`
- composition: `map`, `bind`, `let+`/`and+`/`let*`, `with_pp`

Prefer `small_int` or `nat` for sizes, indices, and arithmetic —
full-range `int` overflows most laws with noise. Build structured
generators with the binding operators:

```ocaml
let gen_rect =
  Gen.(
    let+ w = float_range 0. 10. and+ h = float_range 0. 10. in
    Rect (w, h))
  |> Gen.with_pp pp_shape
```

One `pp` feeds both worlds — `Testable.make ~pp` for assertions,
`Gen.with_pp pp` for counterexamples — so write it once:

```ocaml
let shape = Testable.make ~pp:pp_shape ~equal:( = )

let gen_shape =
  Gen.(
    one_of
      [
        map (fun r -> Circle r) (float_range 0. 100.);
        map
          (fun (w, h) -> Rect (w, h))
          (pair (float_range 0. 100.) (float_range 0. 100.));
      ])
  |> Gen.with_pp pp_shape
```

Composite generators (`list`, `pair`, …) derive their printing from
their components, so `Gen.(list string)` counterexamples print as the
list you expect without any `with_pp`.

## Regressions worth keeping: `~examples`

`~examples` inputs run before any generation, unshrunk — they are
already the reviewed minimal form. They are the home for
counterexamples you never want to regress on:

```ocaml
prop "rect area matches the formula"
  ~examples:[ Rect (2., 0.) ]
  gen_rect
  (fun s ->
    match s with
    | Rect (w, h) -> equal (float 1e-9) (w *. h) (area s)
    | Circle _ -> ())
```

## Preconditions: `assume` and `reject`

`assume cond` discards the current case unless `cond` holds; the case
is regenerated, and a property that discards too much gives up and
fails rather than silently testing nothing:

```ocaml
prop "division round-trips"
  Gen.(pair small_int small_int)
  (fun (a, b) ->
    assume (b <> 0);
    equal int a ((a / b * b) + (a mod b)))
```

Discarding is for rare, cheap preconditions. When the precondition is
structural — nonempty lists, sorted input — constrain the generator
instead (`Gen.such_that`, or a generator correct by construction).
`reject ()` discards unconditionally.

## Is the generator testing anything? `collect`, `classify`, `cover`

A property that never fails may just never reach the interesting
region. `classify label cond` (and `collect label`) report the
distribution of labels over passing cases: a failing property's block
always includes it, and a passing property prints it under `-v` — run
verbose to calibrate, then drop back to the one-line transcript.
`cover ~label ~at_least` turns a distribution expectation into a test
outcome:

```ocaml
prop "parity is exercised" ~count:200 Gen.small_int (fun n ->
    cover ~label:"even" ~at_least:20. (n mod 2 = 0);
    cover ~label:"odd" ~at_least:20. (n mod 2 <> 0);
    classify "zero" (n = 0);
    equal int n n)
```

Set `at_least` well below the rate the generator actually achieves —
at 100 cases the observed rate of a 50% condition swings by roughly
±10 points — or raise `~count`; the
[cookbook](../cookbook.md#6-cover-thresholds-and-the-noise-floor) has
the rule of thumb.

## Notes

- Property tests carry the `"prop"` tag: `--tag prop` selects them,
  `--exclude-tag prop` drops them.
- The per-test timeout (`~timeout`, or the runner's `--timeout`)
  bounds the whole property — generation and shrinking included. A
  timeout that expires before any case has failed fails the test as
  timed out; one that expires during shrinking ends the search and
  reports the best counterexample found so far, marked
  `timed out after Ns while shrinking; counterexample may not be
  minimal`.
- `subtest` inside a property body records labeled failures but
  bypasses the engine: the case completes unshrunk. Use assertions for
  anything you want shrunk; use `subtest` only to label multi-part
  checks whose failures are self-evident.
- For a plain test that wants stable stochastic inputs without the
  property machinery, `srandom ()` gives a `Random.State.t` seeded
  from the same root-seed derivation — replayable with `--seed`, and
  immune to suite reordering. A failing test that drew from `srandom`
  prints the replay command in its failure block, so the root token is
  in the log exactly when a stochastic failure needs replaying.
