# Assertions

One design rule behind every verb: a failure must print the data that
would let you fix the bug without adding a `Printf`. Every checking
verb takes optional `?msg` (an annotation shown in the report) and
`?__POS__` (an explicit position in place of the automatic call-stack
location); of the escape hatches, `fail` and `failf` take only
`?__POS__`, and `skip` only `?reason`. Expected precedes actual,
always.

The automatic location comes from the call stack, so it needs the
program compiled with debug information (`-g`, which dune passes by
default). `~__POS__` — the label puns with the builtin, so that is the
whole spelling — is the explicit form, for two places. A helper that
wraps a verb threads it through (`let equal_tensor ?__POS__ a b =
equal ?__POS__ ...`, see the cookbook), so a failure points at the
helper's caller. And an assertion in tail position — the last
expression of a body — has no frame left when it raises, so the report
attributes it to the test's declaration line and says so underneath:
`(assertion in tail position: its line is unknown; ~__POS__ names it)`.
Writing `equal ~__POS__ int 3 (f x)` there puts the assertion's own
line back.

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
transcript is in [Getting started](getting-started.md)). A highlight is
shown only when it points at a small part of a mostly shared value;
once the marks would cover half a side, the two values print whole,
because scattering marks over `Some _` against `None` says nothing the
plain pair does not. The witness inventory:

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
diff marks trailing whitespace with `·` so you can see which, and a
difference that is only the final newline is stated in words rather
than drawn (callers for whom it must not matter compare canonicalized
text: `expect_file` forces one, and `string` renders with `%S`). Reach
for `text` for rendered output, serialized documents, and logs. When
the expected side is long enough that you would rather not write it
out inline, that is what [baselines](baselines.md) are for.

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

The two compose into the assertion most event logs want — *did these
things happen, in any order, ignoring the noisy fields*:

```ocaml
type event = { path : string; kind : string; timestamp : float }

let key e = (e.path, e.kind)                             (* drop the noise *)
let event = Testable.contramap key (pair string string)  (* compare on the key *)
let events = slist event (fun a b -> compare (key a) (key b))

equal events
  [ { path = "a"; kind = "created"; timestamp = 0. };
    { path = "b"; kind = "removed"; timestamp = 0. } ]
  observed
```

`slist` sorts both sides with the comparator before comparing
elementwise, so order is ignored and multiplicity is not; `contramap`
sends both the equality and the failure rendering through the
projection, so the diff shows exactly the fields the test is about.

The witnesses are flat (`int`, `list`, `pair`); the constructors stay
behind `Testable.`, which is what keeps names like `contramap` and
`make` out of every test file's scope.

## Orders

A comparison written as `is_true (retries < 3)` consumes both numbers
into a boolean and can only fail with `expected true / actual false`.
The four ordering verbs keep them. Each takes a witness, the bound as
`~than`, and the value last:

```ocaml
less int ~than:3 (retries ());
at_least (float 1e-6) ~than:0.4 stats.accept_rate
```

```
expected  less than 3
actual    5

expected  at least 0.4
actual    0.38
```

`less` and `greater` are strict; `at_most` and `at_least` admit the
bound. The claim on the expected side is derived from the verb and the
bound, rendered by the witness, so there is nothing to keep in step
with the check — the drift a hand-written `satisfies ~claim` invites.
A range is two lines, each naming the bound it breaks:

```ocaml
greater (float 1e-9) ~than:30. v;
less (float 1e-9) ~than:70. v
```

The order is the witness's. The base-type witnesses carry their
module's (`Int.compare`, `String.compare`, …); the three float
witnesses all order with `Float.compare`, so tolerance plays no part —
it belongs to equality — and under `float 0.5` the values `1.0` and
`1.2` are equal *and* `1.0` is less than `1.2`. NaN sorts below every
float; assert a NaN result with `equal float_exact`.

A custom witness gets its order from `Testable.with_compare`, which
completes the conventional trio; `Testable.structural` carries
`Stdlib.compare` next to `Stdlib.( = )`, and `Testable.contramap` orders
through its projection, so `Testable.contramap String.length int`
orders strings by length:

```ocaml
let version =
  Testable.make ~pp:Version.pp ~equal:Version.equal
  |> Testable.with_compare Version.compare

at_least version ~than:(Version.make 1 2) (Version.of_string "1.4")
```

No container witness carries an order — an option or a list admits
several, and a guessed one would be accepted silently — and neither do
`pass`, `Testable.of_equal`, or a plain `Testable.make`. An ordering
verb over such a witness raises `Invalid_argument` naming
`Testable.with_compare`, whether or not the assertion would have held,
so the mistake surfaces on the first run rather than the first
failure.

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
with `?pp` when given, `<abstract>` otherwise.

## Predicates and containment

`is_true`/`is_false` are the bare bones. When the claim is about a
value and is not an order — a parity, a shape, a domain predicate —
use `satisfies`: the failure renders the value a bare `is_true` would
hide, and `~msg` names the predicate:

```ocaml
satisfies ~msg:"even" int (fun n -> n mod 2 = 0) 42
```

`~claim` replaces the expected side's default sentence ("value
satisfying the predicate") with your own:

```ocaml
satisfies ~claim:"a power of two" int (fun n -> n land (n - 1) = 0)
  (Buffer.capacity b)
```

```
expected  a power of two
actual    12
```

Nothing checks that the claim describes the predicate — keep the two
next to each other. For a comparison against a bound, reach for the
[ordering verbs](#orders) instead: their claim is derived from the
bound and cannot drift.

String containment gets its own verbs because their failures print
the needle with its verdict (`needle "secret" — found at byte 10`)
over a bounded excerpt of the haystack, the occurrence marked when
there is one, instead of printing `false`:

```ocaml
contains ~sub:"user=alice" session_log;
not_contains ~sub:"secret" session_log
```

For an exact occurrence count, fold the count locally and assert
about the number — [cookbook](../cookbook.md#6-counting-occurrences)
recipe 6 has the eight-line `count` and the `greater ~than` that goes
with it.

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

`mem` is not `contains` one type up in the report, either: membership
renders through the witness, while string containment records byte
offsets, so the two failures carry different payloads. And
`in_order ~subs:[]` raises `Invalid_argument` — an assertion that
demands nothing is a programmer error, not a passing test.

## Options and results

Asserting an option's or a result's *shape* needs no witness: `is_none`,
`is_some`, `is_ok` and `is_error` never compare the value, so they take
the same optional printer the `require_*` verbs do — print the branch
you did not want:

```ocaml
is_none ~pp:User.pp (Store.find store "nobody");
is_some (Store.find store "alice");
is_ok (parse_port "8080");
is_error ~pp:Format.pp_print_int (parse_port "0")
```

Without `~pp` the rejected value renders as `<abstract>`, which is
often all you need (`is_some` takes none: its rejected branch is
`None`). Reach for the `require_*` verb when you want the value too;
the shape verbs exist so that asserting presence alone does not mean
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
diff where a predicate could only reject — naming the shared
constructor itself rather than leaving the report to recover it from a
rendering. `Invalid_argument`, `Failure` and `Sys_error` are exactly the
three message-carrying stdlib exceptions `raises` diffs by message,
which is why `Exn` has exactly those three predicates.

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
to gate a whole suite on a resource that may be absent, raise the `skip`
in a `fixture`
([Resources and structure](resources-and-structure.md#run-scoped-resources-fixture)).

Verbs work anywhere code runs inside a test — bodies, `bracket` setup
and teardown, fixture acquisition, property bodies. Outside a run they
raise as ordinary exceptions. For table-driven assertions over many
inputs, reach for `cases`
([Resources and structure](resources-and-structure.md)) so one bad
input does not mask the rest.
