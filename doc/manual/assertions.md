# Assertions

This page shows how to check a value inside a test body: compare it with
an expected one, bound it, search it, unwrap it, or expect an exception.
Each section shows what the verb prints when its claim fails. The
reference for every verb and witness is
[`lib/windtrap.mli`](../../lib/windtrap.mli).

The snippets test `Shop`, a module of shopping carts.

`test/shop.ml`:

```ocaml
type item = { name : string; price : int; quantity : int }

let item name ~price ~quantity =
  if quantity <= 0 then invalid_arg "Shop.item: quantity must be positive";
  { name; price; quantity }

let label item = Printf.sprintf "%s: %d x %d" item.name item.quantity item.price

let pp_item ppf item =
  Format.fprintf ppf "%s x%d at %d" item.name item.quantity item.price

let subtotal cart =
  List.fold_left (fun sum item -> sum + (item.price * item.quantity)) 0 cart

let names cart = List.map (fun item -> item.name) cart
let find name cart = List.find_opt (fun item -> item.name = name) cart
let remove name cart = List.filter (fun item -> item.name <> name) cart
let with_tax ~rate cents = float_of_int cents *. (1. +. rate)
let discount cents = if cents >= 500 then cents / 100 * 5 else 0

let parse_quantity input =
  match int_of_string_opt (String.trim input) with
  | Some n when n > 0 -> Ok n
  | Some _ | None -> Error ("not a quantity: " ^ input)

let receipt cart =
  let line item =
    Printf.sprintf "%-12s %2d x %4d\n" item.name item.quantity item.price
  in
  String.concat "" (List.map line cart)
  ^ Printf.sprintf "total %16d\n" (subtotal cart)
```

The suite is `test/test_assertions.ml`, built by a `(test)` stanza.

`test/dune`:

```lisp
(test
 (name test_assertions)
 (modules test_assertions shop)
 (libraries windtrap))
```

It opens `Windtrap` and declares a cart its tests share.

`test/test_assertions.ml`:

```ocaml
open Windtrap

let bread = Shop.item "bread" ~price:250 ~quantity:2
let milk = Shop.item "milk" ~price:120 ~quantity:1
let cart = [ bread; milk ]
```

Its last line runs one group per section of this page.

`test/test_assertions.ml`:

```ocaml
let () =
  exit
    (run "shop"
       [
         subtotal;
         names;
         find;
         remove;
         with_tax;
         receipt;
         discount;
         parse_quantity;
         label;
         rates;
         validation;
       ])
```

The files ship as `examples/02-assertions/` in windtrap's repository,
and the transcripts print that directory's paths. Each transcript runs
one group against a version of `Shop` with the bug named above it.

## Comparing two values

`equal` takes a witness, then the expected value, then the actual one.
The witness says how to compare and print values of a type: `int`,
`string`, `bool` and the witnesses of the other base types are values of
`Windtrap`. `not_equal` asserts that two values differ.

`test/test_assertions.ml`:

```ocaml
let subtotal =
  group "subtotal"
    [
      test "is zero for an empty cart" (fun () ->
          equal int 0 (Shop.subtotal []));
      test "sums price times quantity" (fun () ->
          equal int 620 (Shop.subtotal cart));
    ]
```

If `subtotal` ignored the quantities, the failure would print both
values, expected first:

```
$ dune exec examples/02-assertions/test_assertions.exe -- -f subtotal
shop: 2 tests
──────────────────────── failures ────────────────────────
  FAIL  subtotal › sums price times quantity
    examples/02-assertions/test_assertions.ml:12
      12 │ test "sums price times quantity" (fun () ->

    expected  620
    actual    370
──────────────────────────────────────────────────────────

1 passed, 1 failed in 0.6ms.
```

## Locating a failing assertion

The assertion above is the last expression of its body, in tail
position, and its failure is located at the test's declaration line. To
locate a failure at the assertion's own line, pass the assertion
`~__POS__`. An assertion that ends a `let%test` or `let%expect_test`
body is located at its own line without it. A helper that wraps a verb
takes `?__POS__` and passes it on (see `Windtrap.pos`).

`test/test_assertions.ml`:

```ocaml
let names =
  group "names"
    [
      test "keeps the order of the cart" (fun () ->
          equal ~__POS__ (list string) [ "bread"; "milk" ] (Shop.names cart));
    ]
```

If `names` reversed the cart, the failure would be located at the
assertion and show its source line:

```
$ dune exec examples/02-assertions/test_assertions.exe -- -f names
shop: 1 test
──────────────────────── failures ────────────────────────
  FAIL  names › keeps the order of the cart
    examples/02-assertions/test_assertions.ml:20
      20 │ equal ~__POS__ (list string) [ "bread"; "milk" ] (Shop.names cart));

    expected  ["bread"; "milk"]
    actual    ["milk"; "bread"]
──────────────────────────────────────────────────────────

1 failed in 0.6ms.
```

## Comparing values of your own types

A container witness takes the witnesses of its components, as in
`list string`, `option item` or `pair string int`. For a type of your
own, `Testable.make` builds the witness from a printer and an equality,
and `Testable.with_compare` adds an order, which the ordering verbs
need. `Windtrap.Testable` has the other constructors. `Law.equivalence`
and `Law.order` check a witness's equality and order, and `Law` states
the other textbook laws (see
[Stating a textbook law](property-testing.md#stating-a-textbook-law)).
A printer that shows less than the equality compares leaves nothing to
diff, and the block then prints the one rendering under
`both sides render as:`.

`test/test_assertions.ml`:

```ocaml
let item = Testable.make ~pp:Shop.pp_item ~equal:( = )

let find =
  group "find"
    [
      test "returns the item of that name" (fun () ->
          equal ~__POS__ (option item) (Some milk) (Shop.find "milk" cart));
      test "returns None for an unknown name" (fun () ->
          equal ~__POS__ (option item) None (Shop.find "eggs" cart));
    ]
```

If `find` returned the first item of another name, the failures would
print the items with `Shop.pp_item`:

```
$ dune exec examples/02-assertions/test_assertions.exe -- -f find
shop: 2 tests
──────────────────────── failures ────────────────────────
  FAIL  find › returns the item of that name
    examples/02-assertions/test_assertions.ml:29
      29 │ equal ~__POS__ (option item) (Some milk) (Shop.find "milk" cart));

    expected  Some milk x1 at 120
                   ~~~~  ~    ~
    actual    Some bread x2 at 250
                   ~~~~~  ~     ~

  FAIL  find › returns None for an unknown name
    examples/02-assertions/test_assertions.ml:31
      31 │ equal ~__POS__ (option item) None (Shop.find "eggs" cart));

    expected  None
    actual    Some bread x2 at 250
──────────────────────────────────────────────────────────

2 failed in 0.8ms.
```

## Comparing part of a value

`Testable.contramap f w` compares and prints a value as `w` does `f` of
it. Here two items are equal when their names are. `slist w cmp`
compares two lists in any order, and `pass` in place of a component
ignores it, as in `pair string pass`.

`test/test_assertions.ml`:

```ocaml
let by_name = Testable.contramap (fun (item : Shop.item) -> item.name) string

let remove =
  group "remove"
    [
      test "drops the item of that name" (fun () ->
          equal ~__POS__ (list by_name) [ milk ] (Shop.remove "bread" cart));
    ]
```

If `remove` kept the named item instead of dropping it, the failure
would print the names alone:

```
$ dune exec examples/02-assertions/test_assertions.exe -- -f remove
shop: 1 test
──────────────────────── failures ────────────────────────
  FAIL  remove › drops the item of that name
    examples/02-assertions/test_assertions.ml:40
      40 │ equal ~__POS__ (list by_name) [ milk ] (Shop.remove "bread" cart));

    expected  ["milk"]
    actual    ["bread"]
──────────────────────────────────────────────────────────

1 failed in 0.5ms.
```

## Comparing floats

`float eps` compares with the absolute tolerance `eps`, and
`float_rel ~rel ~abs` with a relative and an absolute one. `float_exact`
compares bit for bit, and is the witness that asserts a NaN.

`test/test_assertions.ml`:

```ocaml
let with_tax =
  group "with_tax"
    [
      test "adds the rate to the price" (fun () ->
          equal ~__POS__ (float 1e-9) 744. (Shop.with_tax ~rate:0.2 620));
    ]
```

If `with_tax` multiplied the price by the rate alone, the failure would
print both floats:

```
$ dune exec examples/02-assertions/test_assertions.exe -- -f with_tax
shop: 1 test
──────────────────────── failures ────────────────────────
  FAIL  with_tax › adds the rate to the price
    examples/02-assertions/test_assertions.ml:47
      47 │ equal ~__POS__ (float 1e-9) 744. (Shop.with_tax ~rate:0.2 620));

    expected  744
    actual    124
──────────────────────────────────────────────────────────

1 failed in 0.6ms.
```

## Comparing multi-line text

`text` compares strings and prints them verbatim, and its failure is a
line-by-line diff. `string` prints a string quoted and escaped on one
line, which tells `""`, `" "` and `"\t"` apart.

`test/test_assertions.ml`:

```ocaml
let receipt =
  group "receipt"
    [
      test "lists the items then the total" (fun () ->
          equal ~__POS__ text
            "bread         2 x  250\n\
             milk          1 x  120\n\
             total              620\n"
            (Shop.receipt cart));
    ]
```

The `subtotal` bug of the first section changes the receipt's last line,
marked `-` for the expected text and `+` for the actual one:

```
$ dune exec examples/02-assertions/test_assertions.exe -- -f receipt
shop: 1 test
──────────────────────── failures ────────────────────────
  FAIL  receipt › lists the items then the total
    examples/02-assertions/test_assertions.ml:54
      54 │ equal ~__POS__ text

    --- expected
    +++ actual
    @@ -1,3 +1,3 @@
      bread         2 x  250
      milk          1 x  120
    - total              620
    + total              370
──────────────────────────────────────────────────────────

1 failed in 0.6ms.
```

## Checking a bound or a predicate

`less`, `at_most`, `greater` and `at_least` compare a value with the
bound `~than` under the witness's order. A range takes two of them. An
ordering verb needs a witness with an order (see the witnesses section
of `lib/windtrap.mli`). `satisfies` checks any other claim and prints
`~claim` in place of an expected value. `is_true` and `is_false` check a
`bool`, and their failure shows only the boolean.

`test/test_assertions.ml`:

```ocaml
let discount =
  group "discount"
    [
      test "is at most a tenth of the price" (fun () ->
          at_most ~__POS__ int ~than:62 (Shop.discount 620));
      test "is a multiple of 5 cents" (fun () ->
          satisfies ~__POS__ ~claim:"a multiple of 5" int
            (fun cents -> cents mod 5 = 0)
            (Shop.discount 620));
    ]
```

If `discount` took an eighth of the price, both tests would fail:

```
$ dune exec examples/02-assertions/test_assertions.exe -- -f discount
shop: 2 tests
──────────────────────── failures ────────────────────────
  FAIL  discount › is at most a tenth of the price
    examples/02-assertions/test_assertions.ml:65
      65 │ at_most ~__POS__ int ~than:62 (Shop.discount 620));

    expected  at most 62
    actual    77

  FAIL  discount › is a multiple of 5 cents
    examples/02-assertions/test_assertions.ml:67
      67 │ satisfies ~__POS__ ~claim:"a multiple of 5" int

    expected  a multiple of 5
    actual    77
──────────────────────────────────────────────────────────

2 failed in 0.6ms.
```

## Unwrapping an option or a result

`require_some`, `require_ok` and `require_error` return the payload of
the side they want and fail the test on the other. `~pp` prints the
other side's payload. `require_match` does the same for any variant,
through a function that returns an option. `is_some`, `is_none`,
`is_ok` and `is_error` check the side alone.

`test/test_assertions.ml`:

```ocaml
let parse_quantity =
  group "parse_quantity"
    [
      test "reads a number between spaces" (fun () ->
          let quantity =
            require_ok ~pp:(Testable.pp string) (Shop.parse_quantity " 3 ")
          in
          equal ~__POS__ int 3 quantity);
      test "reports the input it rejects" (fun () ->
          let message = require_error (Shop.parse_quantity "0") in
          equal ~__POS__ string "not a quantity: 0" message);
    ]
```

If `parse_quantity` did not trim its input, `require_ok` would print the
error with its `~pp`:

```
$ dune exec examples/02-assertions/test_assertions.exe -- -f parse_quantity
shop: 2 tests
──────────────────────── failures ────────────────────────
  FAIL  parse_quantity › reads a number between spaces
    examples/02-assertions/test_assertions.ml:77
      77 │ require_ok ~pp:(Testable.pp string) (Shop.parse_quantity " 3 ")

    expected  Ok _
    actual    Error "not a quantity:  3 "
──────────────────────────────────────────────────────────

1 passed, 1 failed in 0.6ms.
```

## Searching a string

`contains ~sub` checks that a string occurs in another, and
`not_contains` that it does not. `starts_with` and `ends_with` check an
`~affix`, and `in_order ~subs` checks that strings occur one after the
other. `mem` checks that a list holds an element.

`test/test_assertions.ml`:

```ocaml
let label =
  group "label"
    [
      test "starts with the name" (fun () ->
          starts_with ~__POS__ ~affix:"bread" (Shop.label bread));
      test "shows the quantity then the price" (fun () ->
          in_order ~__POS__ ~subs:[ "2"; "250" ] (Shop.label bread));
    ]
```

If `label` swapped the quantity and the price, the failure would name
the element that broke the chain and where the search found it:

```
$ dune exec examples/02-assertions/test_assertions.exe -- -f label
shop: 2 tests
──────────────────────── failures ────────────────────────
  FAIL  label › shows the quantity then the price
    examples/02-assertions/test_assertions.ml:91
      91 │ in_order ~__POS__ ~subs:[ "2"; "250" ] (Shop.label bread));

    element   1
    needle    "250": found at byte 7, before the search resumed at byte 8
    haystack  bread: 250 x 2
                     ~~~
──────────────────────────────────────────────────────────

1 passed, 1 failed in 0.8ms.
```

## Checking that code raises

`raises e f` checks that `f ()` raises an exception equal to `e`.
`raises_match p f` checks that it raises one that satisfies `p`, and
`Exn` has such predicates for the standard exceptions. For a whole
message, prefer `raises`. When the messages differ, its failure is a
diff of the two.

`test/test_assertions.ml`:

```ocaml
let validation =
  group "validation"
    [
      test "rejects a zero quantity" (fun () ->
          raises ~__POS__
            (Invalid_argument "Shop.item: quantity must be positive") (fun () ->
              Shop.item "eggs" ~price:30 ~quantity:0));
      test "rejects a negative quantity" (fun () ->
          raises_match ~__POS__ (Exn.invalid_arg ~substring:"quantity")
            (fun () -> Shop.item "eggs" ~price:30 ~quantity:(-1)));
    ]
```

If `Shop.item` accepted any quantity, both failures would say that
nothing was raised:

```
$ dune exec examples/02-assertions/test_assertions.exe -- -f validation
shop: 2 tests
──────────────────────── failures ────────────────────────
  FAIL  validation › rejects a zero quantity
    examples/02-assertions/test_assertions.ml:110
      110 │ raises ~__POS__

    expected exception  Invalid_argument("Shop.item: quantity must be positive")
    but no exception was raised

  FAIL  validation › rejects a negative quantity
    examples/02-assertions/test_assertions.ml:114
      114 │ raises_match ~__POS__ (Exn.invalid_arg ~substring:"quantity")

    expected an exception, but none was raised
──────────────────────────────────────────────────────────

2 failed in 0.6ms.
```

## Adding context to a failure

Every verb but `fail` and `failf` takes `~msg`, a line the failure
prints above the values. In a loop it names the iteration that failed,
and the loop stops there. `cases`, in
[Resources and structure](resources-and-structure.md), makes one test
per input instead, and
[`subtest`](resources-and-structure.md#naming-the-parts-of-one-test)
keeps the loop going and names each failing part.
`fail` and `failf` fail the test with a message of their own, in a
branch the test must not reach, and `skip` ends the test as skipped.

`test/test_assertions.ml`:

```ocaml
let rates =
  group "rates"
    [
      test "no rate lowers a price" (fun () ->
          List.iter
            (fun rate ->
              let msg = Printf.sprintf "rate %g" rate in
              at_least ~__POS__ ~msg (float 1e-9) ~than:100.
                (Shop.with_tax ~rate 100))
            [ 0.; 0.055; 0.2 ]);
    ]
```

With the `with_tax` bug above, the first rate fails:

```
$ dune exec examples/02-assertions/test_assertions.exe -- -f rates
shop: 1 test
──────────────────────── failures ────────────────────────
  FAIL  rates › no rate lowers a price
    examples/02-assertions/test_assertions.ml:101
      101 │ at_least ~__POS__ ~msg (float 1e-9) ~than:100.

    rate 0
    expected  at least 100
    actual    0
──────────────────────────────────────────────────────────

1 failed in 0.5ms.
```
