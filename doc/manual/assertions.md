# Assertions

Twenty-seven verbs, one design rule: a failure must print the data that
would let you fix the bug without adding a `Printf`. Every checking
verb takes optional `?msg` (an annotation shown in the report) and
`?pos` (a `__POS__` override for the automatic call-stack location);
of the escape hatches, `fail` and `failf` take only `?pos`, and `skip`
only `?reason`. Expected precedes actual, always.

## Equality: testables

`equal` compares through an `'a testable` — a printer and an equality
for `'a`. Witnesses for base types and containers are in scope after
`open Windtrap`, composing like the types they witness:

```ocaml
equal
  (list (pair string (list int)))
  [ ("alice", [ 1; 2; 3 ]); ("bob", [ 4 ]) ]
  [ ("alice", [ 1; 2; 3 ]); ("bob", [ 4 ]) ]
```

On failure both values render through the printer and the report
highlights their diff — for every type, not just strings (the
transcript is in [Getting started](getting-started.md)). The witness
inventory:

- `unit`, `bool`, `char`, `string`, `bytes`, `int`, `int32`, `int64`,
  `nativeint`
- `text` — a string printed verbatim rather than with `%S`; see
  [Multi-line strings](#multi-line-strings) below
- floats: `float eps` (absolute tolerance), `float_rel ~rel ~abs`
  (combined tolerance), `float_exact` (bit-for-bit; the only witness
  under which NaN equals NaN — use it to assert a function returns NaN)
- containers: `option`, `result`, `either`, `list`, `array`, `pair`,
  `triple`, `quad`
- `slist t cmp` — lists as multisets: order ignored, multiplicity
  kept; failures print both sides sorted, so the diff shows the
  multiset difference, never the incidental arrival order
- `pass` — everything equal; ignores a component: `pair string pass`
- `not_equal t a b` is the negation; its failure prints the value once

## Multi-line strings

`string` prints with `%S` — quoted, escaped, on one line — which is
what you want for a single-line value, where the quotes are what tell
`""`, `" "` and `"\t"` apart. It is the wrong shape for text that
spans lines: the difference ends up buried in `\n` soup.

```
expected  "{\n  \"version\": \"1.2.0\",\n  \"deps\": [\"a\", \"b\"]\n}\n"
                                 ~                             ~
actual    "{\n  \"version\": \"1.3.0\",\n  \"deps\": [\"a\", \"c\"]\n}\n"
                                 ~                             ~
```

`text` prints the same string verbatim. The rendering keeps its
newlines, and a rendering that spans lines is diffed line by line:

```ocaml
equal text expected actual
```

```
--- expected
+++ actual
@@ -1,4 +1,4 @@
  {
-   "version": "1.2.0",
-   "deps": ["a", "b"]
+   "version": "1.3.0",
+   "deps": ["a", "c"]
  }
```

Equality is unchanged — byte for byte, as with `string` — so a
trailing space or a missing final newline is still a failure; the
diff marks trailing whitespace with `·` so you can see which. Reach
for `text` for rendered output, serialized documents, and logs. When
the expected side is long enough that you would rather not write it
out inline, that is what [snapshots](snapshots-and-expect.md) are for.

Custom types need a printer and an equality:

```ocaml
type point = { x : int; y : int }

let pp_point ppf { x; y } = Format.fprintf ppf "(%d, %d)" x y
let point = Testable.make ~pp:pp_point ~equal:( = )
```

`equal` is applied **expected first, actual second** — `equal t x y`
calls your equality as `equal x y`. That only matters if yours is not
symmetric, and where it matters most is tolerances: a relative
tolerance that scales by its *second* argument scales by the computed
value, so a wrong answer that is large buys itself a proportionally
large tolerance and the assertion silently stops testing anything.
Windtrap's own `float_rel` scales by
`Float.max (abs_float a) (abs_float b)` — symmetric by construction.
Most libraries' `allclose` is not; wrap it accordingly.

A module with the conventional trio needs no ceremony:
`Testable.make ~pp:Point.pp ~equal:Point.equal`.
`Testable.structural ~pp` uses `( = )` for you;
`Testable.of_equal` compares without printing (failures show
`<abstract>` — prefer `Testable.make` as soon as anything is
printable). `Testable.contramap` projects before comparing *and*
printing:

```ocaml
let by_length = Testable.contramap String.length int in
equal by_length "abc" "xyz"
```

The witnesses are flat (`int`, `list`, `pair`); the constructors stay
behind `Testable.`, which is what keeps names like `contramap` and
`make` out of every test file's scope.

## Assert and unwrap: `require_*`

The `require_` verbs assert a shape and hand back its payload, so the
happy path keeps its value instead of drowning in `match`:

```ocaml
let id = require_some (find_user "alice") in
equal int 1 id;
let port = require_ok (parse_port "8080") in
equal int 8080 port;
let message = require_error (parse_port "0") in
equal string "invalid port: 0" message;
let port = require_match tcp_port (resolve "db") in
equal int 5432 port
```

`require_match extract v` is `require_some` for values that are not
already options: `extract : 'a -> 'b option` names the constructor you
demand (above, `tcp_port` maps `Tcp p` to `Some p`). On failure
`require_ok`/`require_error`/`require_match` render the rejected value
with `?pp_error`/`?pp_ok`/`?pp` when given, `<abstract>` otherwise.

## Predicates and containment

`is_true`/`is_false` are the bare bones. When the claim is about a
value, use `satisfies` — the failure renders the value a bare
`is_true` would hide, and `~msg` names the predicate:

```ocaml
satisfies ~msg:"positive" int (fun n -> n > 0) 42
```

`~claim` goes further: it replaces the expected side's default
sentence ("value satisfying the predicate") with your own, which is
what turns `satisfies` into a comparison assertion. A comparison
consumes both numbers and hands back a boolean, so `is_true (n > 0)`
can only fail with `expected true / actual false` — the number is
gone. A claim keeps the bound, and the value keeps the value:

```ocaml
satisfies ~claim:"greater than 0" int (fun n -> n > 0)
  (Source.omitted_bytes c)
```

```
expected  greater than 0
actual    0
```

Nothing checks that the claim describes the predicate — keep the two
next to each other, and build the claim with the witness when the
bound is not an `int`
(`Printf.sprintf "greater than %s" (Testable.to_string string bound)`).

String containment gets its own verbs because their failures print
the needle with its verdict (`needle "secret" — found at byte 10`)
over a bounded excerpt of the haystack, the occurrence marked when
there is one, instead of printing `false`:

```ocaml
contains ~sub:"user=alice" log;
not_contains ~sub:"secret" log
```

For an exact occurrence count, fold the count locally and assert
about the number — cookbook recipe 12 has the eight-line `count` and
the `satisfies ~claim` that goes with it.

When the order is the claim, `in_order ~subs` asserts a whole chain of
substrings at once. Each element must match at or after the end of the
previous one, which is exactly what a run of `contains` calls does not
check:

```ocaml
in_order ~subs:[ "connect"; "authenticate"; "disconnect" ] session_log
```

A break names the element that caused it — its index and its value —
and the byte the search had reached, over an excerpt of the region
still to be matched. The interesting failure is the element that *is*
in the string, only too early:

```
element   2
needle    "disconnect" — found at byte 13, before the search resumed at byte 36
haystack  connect send disconnect authenticate
                       ~~~~~~~~~~
```

Out of order and missing are different bugs; three `contains` calls
report neither, because all three needles are there.

`starts_with ~affix` and `ends_with ~affix` demand a position as well
as presence. When the affix is nowhere in the string they report what
`contains` would — the reason is the same — but when it is present in
the wrong place they say where, and mark it:

```
needle    "ghost" — found at byte 9
haystack  sessions/ghost/session.json
                   ~~~~~
```

`mem` is the same idea one type up — membership in a list, through a
witness, so the failure shows the element you wanted and the list you
got rather than a bare `false`:

```ocaml
mem int 42 [ 2; 3; 5 ]
```

```
expected  a list containing 42
actual    [2; 3; 5]
```

## Options

Asserting an option's *shape* needs no witness: `is_none` and
`is_some` never compare the value, so they take the same optional
printer the `require_*` verbs do — print the branch you did not want:

```ocaml
is_none ~pp:User.pp (Store.find store "nobody");
is_some (Store.find store "alice")
```

Without `~pp` the rejected value renders as `<abstract>`, which is
often all you need. Reach for `require_some` when you want the value
too; `is_some` exists so that asserting presence alone does not mean
discarding a result.

## Exceptions

`raises` asserts a structurally equal exception; its failure
distinguishes "nothing raised" from "raised something else", and when
only the message differs (`Invalid_argument`, `Failure`, `Sys_error`)
it reads as a message diff:

```ocaml
raises (Parse_error "empty") (fun () -> Calc.parse " ")
```

When the payload is not comparable, or you only care about part of the
message, use `raises_match` with a predicate — the `Exn` module has
the common ones, and `~substring` constrains the message:

```ocaml
raises_match (Exn.invalid_arg ~substring:"negative") (fun () ->
    invalid_arg "checkout: negative coupon")
```

A *whole* message is `raises`' job: `raises (Failure "boom")` says the
same thing, and because it holds both exceptions it reports a message
diff where a predicate could only reject.

## Escape hatches

`fail msg` (and `failf fmt …`) fails the current test and never
returns — for branches the test must not reach:

```ocaml
match find_user "alice" with
| Some _ -> ()
| None -> fail "alice must exist"
```

`skip ?reason ()` skips the current test — not a failure; a run whose
every selected test skipped still exits 0. Use it for unmet
environment preconditions (`if Sys.win32 then skip ~reason:"unix only" ()`);
see the [cookbook](../cookbook.md) for skipping a whole suite on a
missing resource.

Verbs work anywhere code runs inside a test — bodies, `bracket` setup
and teardown, fixture acquisition, property bodies. Outside a run they
raise as ordinary exceptions. For table-driven assertions over many
inputs, reach for `cases`
([Resources and structure](resources-and-structure.md)) so one bad
input does not mask the rest.
