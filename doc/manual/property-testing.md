# Property testing

This page shows how to state a law that must hold for generated values,
generate values of your own types, read and replay a counterexample,
keep one as a regression, see what a generator reaches, and state a
textbook law by name. The reference is
[`lib/windtrap.mli`](../../lib/windtrap.mli), under `Windtrap.prop`,
`Windtrap.Gen` and `Windtrap.Law`.

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

Its last lines run the groups of this page's sections.

`test/test_geo.ml`:

```ocaml
let () =
  exit
    (run "geo"
       [
         area;
         to_string;
         scale;
         inverse;
         total_area;
         codec;
         witness;
         combine;
         scaling;
       ])
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
counterexamples print with, and `Gen.of_list ~pp` and `Gen.constant ~pp`
take it with the values they list. Without one, a value that `map` or `let+`
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
geo: 17 passed in 6.0ms (seed s1:296aaf2e3762014b).
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

## Stating a textbook law

`Law` states seventeen textbook laws by name, such as `Law.associative`
and `Law.round_trip`, and each is a verb of [Assertions](assertions.md).
Its failure names the law, states the equation that failed and prints
each of its terms. Partially applied, a law is the law of a `prop` and
the function of a `cases`, and applied to a value it is a line of a
`test`. `Law.round_trip` states that `of_string` reads back what
`to_string` prints and, with its arguments swapped, that a text is
canonical. `read` unwraps the option `of_string` returns with
`require_some`, and a `None` fails the law.

`test/test_geo.ml`:

```ocaml
let read s = require_some (Geo.of_string s)

let codec =
  group "codec"
    [
      prop "reads back what it prints" gen_shape
        (Law.round_trip shape string Geo.to_string read);
      cases ~name:Fun.id "prints what it reads"
        [ "circle 1"; "rect 2 0.5" ]
        (Law.round_trip string shape read Geo.to_string);
      test "reads back a unit circle" (fun () ->
          Law.round_trip shape string Geo.to_string read (Geo.Circle 1.));
    ]
```

If `to_string` printed a rectangle's width twice, the property and the
second row would fail, each naming its first function `f` and its second
`g`:

```
$ dune exec examples/03-property-testing/test_geo.exe -- --seed s1:5b58964be30f69a8 -f codec
geo: 4 tests (seed s1:5b58964be30f69a8)
──────────────────────── failures ────────────────────────
  FAIL  codec › reads back what it prints
    examples/03-property-testing/test_geo.ml:85
      85 │ prop "reads back what it prints" gen_shape

    counterexample (case 0, shrunk 2 steps): Rect (0, 1)
    which failed with:
      round trip: g (f x) = x
      f x      "rect 0 0"
      g (f x)  Rect (0, 0)
                        ~
      x        Rect (0, 1)
                        ~

  FAIL  codec › prints what it reads › rect 2 0.5
    examples/03-property-testing/test_geo.ml:87
      87 │ cases ~name:Fun.id "prints what it reads"

    round trip: g (f x) = x
    f x      Rect (2, 0.5)
    g (f x)  "rect 2 2"
                     ~
    x        "rect 2 0.5"
                     ~~~
──────────────────────────────────────────────────────────

replay: dune exec examples/03-property-testing/test_geo.exe -- --seed s1:5b58964be30f69a8 -f 'codec'
2 passed, 2 failed in 2.0ms.
```

### Checking a witness's equality and order

An equality that returns `true` for any two values passes every
`equal` that uses it. `Law.equivalence` and `Law.order` check a
witness's own equality and order. Two drawn values are rarely equal, and
`~respell` gives each law a respelling, a function that returns an equal
value built differently. A hash agrees with the equality when it ignores
a respelling `r`, which `Law.ignores w int hash r` checks.

`Geo` compares drawings by the shapes they hold, whatever their groups.

`test/geo.ml`:

```ocaml
let rec shapes = function
  | Shape s -> [ s ]
  | Group ds -> List.concat_map shapes ds

let equal_drawing a b = shapes a = shapes b
let compare_drawing a b = compare (shapes a) (shapes b)
let hash_drawing d = Hashtbl.hash (shapes d)
```

`regroup` respells a drawing as a group of one.

`test/test_geo.ml`:

```ocaml
let drawing =
  Testable.(
    with_compare Geo.compare_drawing
      (make ~pp:Geo.pp_drawing ~equal:Geo.equal_drawing))

let any_drawing =
  Gen.(
    with_pp Geo.pp_drawing
      (let* depth = int_range 0 3 in
       gen_drawing depth))

let regroup d = Geo.Group [ d ]

let witness =
  group "witness"
    [
      prop "equal_drawing is an equivalence"
        Gen.(pair any_drawing any_drawing)
        (Law.equivalence ~respell:regroup drawing);
      prop "compare_drawing is a total order"
        Gen.(triple any_drawing any_drawing any_drawing)
        (Law.order ~respell:regroup drawing);
      prop "hash_drawing ignores regrouping" any_drawing
        (Law.ignores drawing int Geo.hash_drawing regroup);
    ]
```

If `equal_drawing` returned `true` for any two drawings, the equivalence
would never meet an unequal pair, and the order would disagree with the
equality:

```
$ dune exec examples/03-property-testing/test_geo.exe -- --seed s1:5b58964be30f69a8 -f witness
geo: 3 tests (seed s1:5b58964be30f69a8)
──────────────────────── failures ────────────────────────
  FAIL  witness › equal_drawing is an equivalence
    examples/03-property-testing/test_geo.ml:110
      110 │ prop "equal_drawing is an equivalence"

    never covered: "equivalence: an unequal pair" (over 100 passing cases)
    labels (100 passing cases):
      100.0%  equivalence: r a differs from a
    covered labels:
      equivalence: an unequal pair  0  never covered
      equivalence: r a differs from a  100

  FAIL  witness › compare_drawing is a total order
    examples/03-property-testing/test_geo.ml:113
      113 │ prop "compare_drawing is a total order"

    counterexample (case 0, shrunk 12 steps): (Circle 0, Circle 0, Circle 1)
    which failed with:
      order (agrees with equal): cmp a c = 0 iff a = c
      a        Circle 0
      c        Circle 1
      cmp a c  -1
      a = c    true
──────────────────────────────────────────────────────────

replay: dune exec examples/03-property-testing/test_geo.exe -- --seed s1:5b58964be30f69a8 -f 'witness'
1 passed, 2 failed in 2.0ms.
```

### Choosing a law for what you wrote

Find what you wrote in the first column, and state each law of its row
in a `prop` of its own.

| You wrote | Its laws |
| --- | --- |
| A witness | `equivalence`, `order`, and `ignores w int hash r` for its hash |
| A relation that orders values, such as inclusion | `partial_order`, over three values drawn as a chain |
| A merge | `associative`; `commutative` when the order of the operands does not matter; `neutral` for its empty value; `absorbing` for a value that absorbs every other |
| An operation that can be undone | `associative`, `neutral` for its identity, and `invertible` for its inverse |
| Two operations of one type | `distributive op ~over` |
| A codec | `round_trip wa wb encode decode`, and `round_trip wb wa decode encode` over canonical texts |
| A normaliser | `idempotent`, and `ignores` for each difference it erases |
| A reversal | `involutive` |
| A map-like function | `commutes` with another transformation, and `homomorphic` from one operation to another |
| A cost | `monotone` |
| An invariant | `preserves` for each operation that must keep it |

`Geo.combine` is a merge, with `Geo.empty` as its neutral element.

`test/geo.ml`:

```ocaml
let empty = Group []
let combine a b = Group [ a; b ]
```

`total_area` takes `combine` to `+.`, which `homomorphic` states.

`test/test_geo.ml`:

```ocaml
let combine =
  group "combine"
    [
      prop "is associative"
        Gen.(triple any_drawing any_drawing any_drawing)
        (Law.associative drawing Geo.combine);
      prop "has empty as its neutral element" any_drawing
        (Law.neutral drawing Geo.combine Geo.empty);
      prop "adds the areas"
        Gen.(pair any_drawing any_drawing)
        (Law.homomorphic drawing close Geo.total_area Geo.combine ( +. ));
    ]
```

`Geo.turn` adds two rotations by quarter turns, `Geo.no_turn` is its
neutral element and `Geo.undo` returns the turn that cancels another.

`test/geo.ml`:

```ocaml
let no_turn = 0
let turn a b = (a + b) mod 4
let undo t = (4 - t) mod 4
```

`test/test_geo.ml`:

```ocaml
let turns = Gen.int_range 0 3

let turn =
  group "turn"
    [
      prop "is associative"
        Gen.(triple turns turns turns)
        (Law.associative int Geo.turn);
      prop "has no_turn as its neutral element" turns
        (Law.neutral int Geo.turn Geo.no_turn);
      prop "has undo as its inverse" turns
        (Law.invertible int Geo.turn Geo.no_turn Geo.undo);
    ]
```

If `undo` returned `3 - t`, one quarter turn short, the inverse would
fail with the turn it returned:

```
$ dune exec examples/03-property-testing/test_geo.exe -- --seed s1:5b58964be30f69a8 -f turn
geo: 3 tests (seed s1:5b58964be30f69a8)
──────────────────────── failures ────────────────────────
  FAIL  turn › has undo as its inverse
    examples/03-property-testing/test_geo.ml:143
      143 │ prop "has undo as its inverse" turns

    counterexample (case 0, shrunk 1 step): 0
    which failed with:
      invertible: op x (inv x) = e
      x             0
      inv x         3
      op x (inv x)  3
      e             0
──────────────────────────────────────────────────────────

replay: dune exec examples/03-property-testing/test_geo.exe -- --seed s1:5b58964be30f69a8 -f 'turn'
2 passed, 1 failed in 0.9ms.
```

### Stating a law with no name

A law that `Law` does not name is an `equal`, with the trusted side as
the expected value. These are the spellings of common ones:

| Law | Spelling |
| --- | --- |
| `f` agrees with a reference | `equal w (reference x) (f x)` |
| `f` is deterministic | `equal w (f x) (f x)` |
| `op a a = a` | `equal w a (op a a)` |
| `f` is antitone | `Law.monotone wa wb' f`, where `wb'` is `Testable.with_compare (fun x y -> cmp y x) wb` and `cmp` is `wb`'s order |
| `leq` is a preorder | `Law.partial_order w' leq`, where `w'` is `Testable.make ~pp ~equal:(fun x y -> leq x y && leq y x)` |
| `inv` is an inverse on one side only | `equal w e (op x (inv x))` |
| De Morgan's laws | `Law.homomorphic bool bool not ( && ) ( \|\| )`, and again with the two operations swapped |
| `get` and `set` form a lens | `Law.round_trip wv ws (fun v -> set v s) get v`, `Law.round_trip ws wv get (fun v -> set v s) s` and `Law.ignores ws ws (set v') (set v) s` |

`Gen` draws no functions. A law quantified over functions, such as a
functor law of a `map`, is stated for functions the test writes.

### Meeting a law's premise

A law never skips a case, and it builds the premises it can: `monotone`
sorts its pair, `order` and `partial_order` try the six orderings of
their three values, and a respelling builds equal values apart. A false
`inv x` fails `preserves w f inv x`, and the generator must then draw
only values inside `inv`. `equivalence`, `order`, `partial_order`,
`idempotent`, `involutive`, `monotone` and `ignores`, which could hold
trivially on every case, also demand, as `cover` does, a case that is
not, in a `prop` or a `stateful` test. The values of a `cases` row or a
`test` are chosen by hand, and a law demands nothing of them, nor on a
domain that the test spawned.

`test/geo.ml`:

```ocaml
let is_valid = function
  | Circle r -> r >= 0.
  | Rect (w, h) -> w >= 0. && h >= 0.
```

`test/test_geo.ml`:

```ocaml
let factor = Gen.float_range 0. 10.

let scaling =
  group "scaling"
    [
      prop "grows the area with the factor"
        Gen.(pair (pair factor factor) gen_shape)
        (fun (ks, s) ->
          Law.monotone float_exact float_exact
            (fun k -> Geo.area (Geo.scale k s))
            ks);
      prop "keeps a shape valid"
        Gen.(pair factor gen_shape)
        (fun (k, s) -> Law.preserves shape (Geo.scale k) Geo.is_valid s);
    ]
```

A partial order holds trivially on three values that it does not
relate, and three drawings drawn apart are rarely related.
`Geo.part_of a b` holds when `b` draws the shapes of `a` in their order,
and the property draws `b` and `c` from `a`.

`test/geo.ml`:

```ocaml
let rec subsequence xs ys =
  match (xs, ys) with
  | [], _ -> true
  | _ :: _, [] -> false
  | x :: xs', y :: ys' ->
      if x = y then subsequence xs' ys' else subsequence xs ys'

let part_of a b = subsequence (shapes a) (shapes b)
```

`test/test_geo.ml`:

```ocaml
let part_of =
  group "part_of"
    [
      prop "is a partial order"
        Gen.(triple any_drawing any_drawing any_drawing)
        (fun (a, x, y) ->
          let b = Geo.combine a x in
          Law.partial_order drawing Geo.part_of (a, b, Geo.combine b y));
    ]
```

If the property gave the law its three drawings as drawn, no case would
form a chain:

```
$ dune exec examples/03-property-testing/test_geo.exe -- --seed s1:5b58964be30f69a8 -f part_of
geo: 1 test (seed s1:5b58964be30f69a8)
──────────────────────── failures ────────────────────────
  FAIL  part_of › is a partial order
    examples/03-property-testing/test_geo.ml:166
      166 │ prop "is a partial order"

    never covered: "partial order: a strict chain" (over 100 passing cases)
──────────────────────────────────────────────────────────

1 failed in 2.2ms.
```

A demand that no passing case met fails the property with
`never covered:` and its label, the law's name followed by one of these:

- `an unequal pair`: the witness's equality holds on every pair, as in
  `equal_drawing is an equivalence` above, or the generator draws copies
  of one value.
- `r a differs from a`: the respelling returns its argument unchanged.
- `a strict chain`: no case drew three values, no two of them equal,
  that the relation orders one after another.
- `f x differs from x`: every drawn value is a fixed point of `f`, as a
  positive number is of `abs`.
- `a strict pair`: the generator draws only pairs that the first
  witness's order ties.
- `g x differs from x`: `g` returns every drawn value unchanged.

The label ends with the law's `~msg` when one is given, as in
`ignores: g x differs from x (sign)`. Two calls of one law in one
property share their demands unless their `~msg`s differ, and a case of
either meets them, so a call that tests nothing can hide behind the
other. Give each call its own `~msg`.

### Stating a law over floats

A law takes no tolerance and compares under its witness, as
`adds the areas` compares areas under `close`. Under `float 0.1`, `0.`
equals `0.06` and `0.06` equals `0.12`, but `0.` does not equal `0.12`,
and a witness with a tolerance is never an equivalence. Float addition
is not associative, under any tolerance. `(1e20 +. -1e20) +. 1.` is
`1.`, and `1e20 +. (-1e20 +. 1.)` is `0.`. `float_exact` satisfies
`Law.equivalence` and `Law.order`, with `-0.` below `0.` and every NaN
equal to every other.
