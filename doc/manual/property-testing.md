# Property testing

This page shows how to state a law that must hold for generated values,
generate values of your own types, read and replay a counterexample,
keep one as a regression, and see what a generator reaches. The
reference is [`lib/windtrap.mli`](../../lib/windtrap.mli), under
`Windtrap.prop` and `Windtrap.Gen`.

The snippets test `Geo`, a module of shapes.

`test/geo.ml`:

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

The suite is `test/test_geo.ml`, built by a `(test)` stanza.

`test/dune`:

```lisp
(test
 (name test_geo)
 (modules test_geo geo)
 (libraries windtrap))
```

Its last line runs one group per section of this page.

`test/test_geo.ml`:

```ocaml
let () = exit (run "geo" [ area; to_string; scale; inverse; total_area ])
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

`test/test_geo.ml`:

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

```
$ dune runtest
geo: 5 passed in 2.0ms (seed s1:2517c1601bf6fe73).
```

## Reading a counterexample

When the law fails, the property shrinks the value to a counterexample
and prints it with the case that found it, the number of shrink steps,
and the assertion that failed. The report closes on one `replay:` line,
above the summary, which runs the run's tests again under its seed, and
each failed test finds the same counterexample with the same version of
windtrap. The counterexample transcripts of this page pass
that seed with `--seed`. A run without it draws a new seed, and the case
and the number of shrink steps change. Renaming or regrouping the property changes the
values it draws, and the other tests of the suite do not. A search cut
short says so, as `shrinking stopped after N steps` or
`timed out after Ns while shrinking`, and adds that the counterexample
may not be minimal.

`test/test_geo.ml`:

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

```
$ dune exec examples/03-property-testing/test_geo.exe -- --seed s1:5b58964be30f69a8 -f to_string
geo: 1 test (seed s1:5b58964be30f69a8)
──────────────────────── failures ────────────────────────
  FAIL  to_string › is read back by of_string
    examples/03-property-testing/test_geo.ml:27
      27 │ prop "is read back by of_string" gen_shape (fun s ->

    counterexample (case 0, shrunk 7 steps): Rect (0, 1)
    which failed at:
      examples/03-property-testing/test_geo.ml:28
        28 │ equal ~__POS__ (option shape) (Some s)
      expected  Some Rect (0, 1)
                           ~  ~
      actual    Some Rect (1, 0)
                           ~  ~
──────────────────────────────────────────────────────────

replay: dune exec examples/03-property-testing/test_geo.exe -- --seed s1:5b58964be30f69a8 -f 'to_string'
1 failed in 2.1ms.
```

## Keeping a counterexample as a regression

`~examples` lists inputs the law runs on before any generated case,
whatever the seed. A counterexample pasted there stays tested. A failing
example is reported as `example N`, unshrunk, and fails again under any
seed.

`test/test_geo.ml`:

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

`assume cond` discards a case the law does not apply to (see
`Windtrap.assume`). A property that discards more than `~max_discard`
cases fails with `property gave up:` and its counts. A precondition on
the value's structure belongs in the generator, built in or with
`Gen.such_that`. `classify` and `collect` label a case, and `cover`
fails the property when no passing case carries its label (see
`Windtrap.cover`).

`test/test_geo.ml`:

```ocaml
let inverse =
  group "inverse"
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

Under `-v` the passing property prints the share of its cases under each
label:

```
$ dune exec examples/03-property-testing/test_geo.exe -- -v --seed s1:5b58964be30f69a8 --prop-count 1000 -f inverse
geo: 1 test (seed s1:5b58964be30f69a8)
  PASS  inverse › undoes scale by k                1.0ms
    labels (1000 passing cases):
       50.0%  circle
       50.0%  rect
1 passed in 1.5ms.
```

## Generating a recursive type

A generator of a recursive type takes a depth and draws a leaf at depth
zero. `let*` draws the depth first, from a small range. `~size` bounds the
length of a list, which is otherwise below 64 and about 5 on average.
`Geo` groups shapes in drawings.

`test/geo.ml`:

```ocaml
type drawing = Shape of shape | Group of drawing list

let rec pp_drawing ppf = function
  | Shape s -> pp ppf s
  | Group ds ->
      Format.fprintf ppf "Group [%a]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
           pp_drawing)
        ds

let rec total_area = function
  | Shape s -> area s
  | Group ds -> List.fold_left (fun sum d -> sum +. total_area d) 0. ds
```

`test/test_geo.ml`:

```ocaml
let rec gen_drawing depth =
  if depth = 0 then Gen.map (fun s -> Geo.Shape s) gen_shape
  else
    Gen.(
      one_of
        [
          map (fun s -> Geo.Shape s) gen_shape;
          map
            (fun ds -> Geo.Group ds)
            (list ~size:(int_range 0 4) (gen_drawing (depth - 1)));
        ])

let total_area =
  group "total_area"
    [
      prop "is never negative"
        Gen.(
          with_pp Geo.pp_drawing
            (let* depth = int_range 0 3 in
             gen_drawing depth))
        (fun d -> at_least ~__POS__ float_exact ~than:0. (Geo.total_area d));
    ]
```

If `total_area` subtracted the drawings of a group, shrinking would
reduce the failing drawing to a group of one circle:

```
$ dune exec examples/03-property-testing/test_geo.exe -- --seed s1:5b58964be30f69a8 -f total_area
geo: 1 test (seed s1:5b58964be30f69a8)
──────────────────────── failures ────────────────────────
  FAIL  total_area › is never negative
    examples/03-property-testing/test_geo.ml:72
      72 │ prop "is never negative"

    counterexample (case 6, shrunk 7 steps): Group [Circle 1]
    which failed at:
      examples/03-property-testing/test_geo.ml:77
        77 │ (fun d -> at_least ~__POS__ float_exact ~than:0. (Geo.total_area d));
      expected  at least 0.
      actual    -3.141592653589793
──────────────────────────────────────────────────────────

replay: dune exec examples/03-property-testing/test_geo.exe -- --seed s1:5b58964be30f69a8 -f 'total_area'
1 failed in 26ms.
```
