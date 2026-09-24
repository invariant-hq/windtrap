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
──────────────────── failures (1) ────────────────────
  FAIL  decode inverts encode
    test/test_codec.ml:14
      14 │       prop "decode inverts encode"

    counterexample (case 0, shrunk 3 steps): [""]
    which failed at:
      test/test_codec.ml:17
      expected  [""]
      actual    []
    replay: dune exec test/test_codec.exe -- --seed s1:fdf792804ac422b3 -f 'decode inverts encode'
──────────────────────────────────────────────────────

1 failed in 0.000707s.
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
- The case index counts every generation attempt, discards included,
  so each attempt draws fresh values; a replay re-derives the same
  descent from the printed root, and a timeout during shrinking only
  moves the stopping point along that path.

A shrink search stops after 10,000 accepted steps — a fixed budget, so
a replay descends to the same node — or earlier, if forcing a
candidate raises and leaves the rest unreachable. When it stops before
converging the report says so — `shrinking stopped after 10000 steps;
counterexample may not be minimal` — so a truncated search never reads
like a converged one, and the step count tells the two stops apart. No
ordinary value spends the budget (a quad of `int64` converges within a
few hundred steps); a search that must not run away is bounded by the
test's `~timeout`.

`~count` (or `--prop-count N` / `WINDTRAP_PROP_COUNT`) changes the
case budget; the declaration site wins over the flag.

## Generators

`Gen` is the generator vocabulary — each generator carries generation,
shrinking, *and* printing, inseparably:

- numeric: `int`, `nat`, `small_int`, `int_range`, `int32`, `int64`,
  `nativeint`, `float`, `float_range`
- base: `unit`, `bool`, `char`, `char_range`, `string`,
  `string_of ?size char`, `bytes`, `bytes_of`
- containers: `list ?size`, `array ?size`, `option`, `result`, `either`,
  `pair`, `triple`, `quad`
- choice: `constant`, `of_list`, `one_of`, `frequency`, `such_that`
- composition: `map`, `bind`, `let+`/`and+`/`let*`, `with_pp`

Prefer `small_int` or `nat` for sizes, indices, and arithmetic —
full-range `int` overflows most laws with noise. Build structured
generators with the binding operators:

```ocaml
let gen_pair =
  Gen.(
    let* shape = gen_shape in
    let+ a = gen_f32 shape and+ b = gen_f32 shape in
    (a, b))
```

Composite generators (`list`, `pair`, …) derive their printing from
their components, so `Gen.(list string)` counterexamples print as the
list you expect without any `with_pp`. `map` and `bind` — and so
`let+`, `and+` and `let*`, which *are* `map` and `bind` — cannot: no
printer for the result type can be inferred from the one they consume.
Such a counterexample renders as its *pre-image*: the same shape, with
every printerless `map` or `bind` result replaced by what it was
computed from — the input the mapping function received, printed by
its own generator — down to the nearest generator that prints. With
`gen_shape` a `list` of `int_range`s and `gen_f32` a `map` building a
tensor over a list of floats, a failure of `gen_pair` reads:

```
counterexample (case 0, shrunk 3 steps): from [1] -> ([0.], [0.])
(the value has no printer — shown is its pre-image, what map and bind computed it from)
```

`[1]` is the shape drawn first, the pair is the float data each tensor
was built from, and `from` marks the line as the input of the mapping
functions rather than the value the body received. The rule per
combinator: a `map` renders its argument; a `bind` renders the inner
value alone when the inner generator prints, and `outer -> inner`
otherwise; `pair`, `list` and the other deriving combinators render
every component by its rule, so a pair of pre-images prints as a
pair. Shrinking walks the same tree, so the pre-image printed is the
pre-image of the shrunk value.

The rule stops at a leaf with nothing to print — `constant` or
`of_list` without a `with_pp` — and one such leaf forfeits the
rendering of the whole composition:

```
counterexample (case 2, shrunk 2 steps): <no printer: attach one with Gen.with_pp>
```

The seed is still enough to replay the failure. Attach `with_pp` to
the leaf to read it — or to the top of any generator whose value you
would rather see than its pre-image: an explicit printer always wins.
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
`reject ()` discards unconditionally. Both also work inside a
function given to a generator, such as `Gen.map` or `Gen.bind`: a
discard there discards the case, and a shrink candidate whose
generation discards is skipped.

The budget is twice the effective `~count`, and `~max_discard` raises
it where a precondition is genuinely rare:

```ocaml
prop ~count:500 ~max_discard:1500 "…" gen law
```

There is no run-wide knob for it: the discard rate is a fact about
this law's precondition, and nothing at the command line knows which
property needs the room. Giving up is a failure, not a silent pass on
however few cases got through — so a budget that is too small shows up
red rather than as a property that quietly tested nothing.

## Is the generator testing anything? `collect`, `classify`, `cover`

A property that never fails may just never reach the interesting
region. `classify label cond` (and `collect label`) report the
distribution of labels over passing cases: a failing property's block
always includes it, and a passing property prints it under `-v` — run
verbose to calibrate, then drop back to the one-line transcript.
`cover label cond` turns that from a table a human reads into a test
outcome: the property fails unless at least one passing case marked
the label.

```ocaml
prop "parity is exercised" ~count:200 Gen.small_int (fun n ->
    cover "even" (n mod 2 = 0);
    cover "odd" (n mod 2 <> 0);
    classify "zero" (n = 0);
    equal int n n)
```

Presence, not proportion. "This region is reached at all" is what
catches a generator that stopped reaching it — a percentage gate over
a random sample flakes near its threshold, and calibrating one costs
more margin than the signal is worth. When you want the proportion,
`classify` prints it and you read it.

The demand registers wherever `cover` is written, even on a case where
the condition is false, so put it somewhere the body always reaches: a
`cover` inside the branch it is meant to police is vacuous exactly when
it should fire.

## Notes

- Property tests carry the `"prop"` tag: `--tag prop` selects them,
  `--exclude-tag prop` drops them.
- `prop` and `stateful` take no `?retries`: a property replays
  deterministically from the root seed, the test's path and the case
  index, so a retry would re-run the identical failing stream.
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
