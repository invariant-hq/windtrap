# Property testing

This page shows how to state a law that must hold for generated values,
generate values of your own types, read and replay a counterexample,
keep one as a regression, and see what a generator reaches. The
reference is [`lib/windtrap.mli`](../../lib/windtrap.mli), under
`Windtrap.prop` and `Windtrap.Gen`.

The snippets test `Geo`, a module of shapes, `test/geo.ml`:

<!-- file examples/03-property-testing/geo.ml -->
```ocaml
type shape = Circle of float | Rect of float * float

let pp ppf = function
  | Circle r -> Format.fprintf ppf "Circle %g" r
  | Rect (w, h) -> Format.fprintf ppf "Rect (%g, %g)" w h

let area = function Circle r -> Float.pi *. r *. r | Rect (w, h) -> w *. h

let scale k = function
  | Circle r -> Circle (k *. r)
  | Rect (w, h) -> Rect (k *. w, k *. h)

let to_string = function
  | Circle r -> Printf.sprintf "circle %.17g" r
  | Rect (w, h) -> Printf.sprintf "rect %.17g %.17g" w h

let of_string s =
  match String.split_on_char ' ' s with
  | [ "circle"; r ] -> Some (Circle (float_of_string r))
  | [ "rect"; w; h ] -> Some (Rect (float_of_string w, float_of_string h))
  | _ -> None
```

The suite is `test/test_geo.ml`, built by a `(test)` stanza:

<!-- file examples/03-property-testing/dune -->
```lisp
(test
 (name test_geo)
 (modules test_geo geo)
 (libraries windtrap))
```

Its last line runs one group per section of this page:

<!-- file examples/03-property-testing/test_geo.ml from let () = -->
```ocaml
let () = exit (run "geo" [ area; to_string; scale; inverse ])
```

The files ship as `examples/03-property-testing/` in windtrap's
repository, and the transcripts print that directory's paths. The
failing transcripts run a version of `Geo` with the bug named above
them.

## Writing a property

`prop name gen law` is a test that runs `law` on the values `gen` draws
and asserts with the verbs of [Assertions](assertions.md). `~count` sets
its number of cases, and `--prop-count` sets it for the properties
without one, 100 by default. `Gen` has a generator for each type that
has a witness, under the same name, and composes them with `map`, `let+`
and `one_of`. `Gen.with_pp` gives a generator the printer its
counterexamples print with. Without one, a value that `map` or `let+`
computed prints as what it was computed from, after `computed from`.

<!-- file examples/03-property-testing/test_geo.ml from open Windtrap to let area -->
```ocaml
open Windtrap

let size = Gen.(map float_of_int (int_range 0 100))

let gen_shape =
  Gen.(
    one_of
      [
        map (fun r -> Geo.Circle r) size;
        (let+ w = size and+ h = size in
         Geo.Rect (w, h));
      ])
  |> Gen.with_pp Geo.pp

let area =
  group "area"
    [
      prop "is never negative" gen_shape (fun s ->
          at_least ~__POS__ float_exact ~than:0. (Geo.area s));
    ]
```

A run that holds a property prints its seed on the summary line:

<!-- run examples/03-property-testing -->
```
$ dune runtest
geo: 4 passed in 1.2ms (seed s1:0aacf67ada23754f).
```

## Reading a counterexample

When the law fails, the property shrinks the value to a counterexample
and prints it with the case that found it, the number of shrink steps,
and the assertion that failed. Its `replay:` line runs the test again
under the run's seed, which finds the same counterexample with the same
version of windtrap. The transcripts of this page pass that seed with
`--seed`. A run without it draws a new seed, and the case and the number
of shrink steps change. Renaming or regrouping the property changes the
values it draws, and the other tests of the suite do not.

<!-- file examples/03-property-testing/test_geo.ml from let shape to let to_string -->
```ocaml
let shape = Testable.make ~pp:Geo.pp ~equal:( = )

let to_string =
  group "to_string"
    [
      prop "is read back by of_string" gen_shape (fun s ->
          equal ~__POS__ (option shape) (Some s)
            (Geo.of_string (Geo.to_string s)));
    ]
```

If `of_string` swapped the sides of a rectangle, shrinking would reduce
the failing rectangle to `Rect (0, 1)`:

<!-- run examples/03-property-testing/failing as examples/03-property-testing -->
```
$ dune exec examples/03-property-testing/test_geo.exe -- --seed s1:5b58964be30f69a8 -f to_string
geo: 1 test (seed s1:5b58964be30f69a8)
──────────────────────── failures ────────────────────────
  FAIL  to_string › is read back by of_string
    examples/03-property-testing/test_geo.ml:27
      27 │ prop "is read back by of_string" gen_shape (fun s ->

    counterexample (case 0, shrunk 6 steps): Rect (0, 1)
    which failed at:
      examples/03-property-testing/test_geo.ml:28
        28 │ equal ~__POS__ (option shape) (Some s)
      expected  Some Rect (0, 1)
                           ~  ~
      actual    Some Rect (1, 0)
                           ~  ~
    replay: dune exec examples/03-property-testing/test_geo.exe -- --seed s1:5b58964be30f69a8 -f 'to_string › is read back by of_string'
──────────────────────────────────────────────────────────

1 failed in 0.7ms.
```

## Keeping a counterexample as a regression

`~examples` lists inputs the law runs on before any generated case,
whatever the seed. A counterexample pasted there stays tested. A failing
example is reported as `example N`, unshrunk and without a `replay:`
line.

<!-- file examples/03-property-testing/test_geo.ml from let close to let scale -->
```ocaml
let close = float_rel ~rel:1e-9 ~abs:1e-9

let scale =
  group "scale"
    [
      prop "multiplies the area by k squared"
        ~examples:[ (2., Geo.Rect (1., 3.)) ]
        Gen.(pair (float_range 0. 10.) gen_shape)
        (fun (k, s) ->
          equal ~__POS__ close (k *. k *. Geo.area s) (Geo.area (Geo.scale k s)));
    ]
```

If `scale` left the height of a rectangle alone, the example would fail
before any generated case:

<!-- run examples/03-property-testing/failing as examples/03-property-testing -->
```
$ dune exec examples/03-property-testing/test_geo.exe -- -f 'k squared'
geo: 1 test (seed s1:96b69c9ed18d0547)
──────────────────────── failures ────────────────────────
  FAIL  scale › multiplies the area by k squared
    examples/03-property-testing/test_geo.ml:37
      37 │ prop "multiplies the area by k squared"

    counterexample (example 1): (2., Rect (1, 3))
    which failed at:
      examples/03-property-testing/test_geo.ml:41
        41 │ equal ~__POS__ close (k *. k *. Geo.area s) (Geo.area (Geo.scale k s)));
      expected  12
      actual    6
──────────────────────────────────────────────────────────

1 failed in 1.4ms.
```

## Discarding and labelling cases

`assume cond` discards the case unless `cond` holds, and a property that
discards more than `~max_discard` cases gives up and fails. A
precondition on the value's structure belongs in the generator, built in
or with `Gen.such_that`. `classify` and `collect` label a case, and `-v`
prints the share of passing cases that carry each label. `cover` labels
a case too, and fails the property when no passing case carries its
label. Its demand registers when the law calls it, so a `cover` the law
does not always reach may demand nothing.

<!-- file examples/03-property-testing/test_geo.ml from let inverse -->
```ocaml
let inverse =
  group "scale by 1/k"
    [
      prop "undoes scale by k"
        Gen.(pair (float_range 0. 10.) gen_shape)
        (fun (k, s) ->
          assume (k > 0.);
          let back = Geo.scale (1. /. k) (Geo.scale k s) in
          classify "circle" (match s with Circle _ -> true | Rect _ -> false);
          cover "rect" (match s with Rect _ -> true | Circle _ -> false);
          equal ~__POS__ close (Geo.area s) (Geo.area back));
    ]
```

Under `-v`, over 1000 cases:

<!-- run examples/03-property-testing -->
```
$ dune exec examples/03-property-testing/test_geo.exe -- -v --seed s1:5b58964be30f69a8 --prop-count 1000 -f 1/k
geo: 1 test (seed s1:5b58964be30f69a8)
  PASS  scale by 1/k › undoes scale by k           2.5ms
    labels (1000 passing cases):
       52.9%  circle
       47.1%  rect
1 passed in 3.3ms.
```
