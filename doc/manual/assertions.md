# Assertions

Sixteen verbs, one design rule: a failure must print the data that
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

Custom types need a printer and an equality:

```ocaml
type point = { x : int; y : int }

let pp_point ppf { x; y } = Format.fprintf ppf "(%d, %d)" x y
let point = Testable.make ~pp:pp_point ~equal:( = )
```

`Testable.structural ~pp` uses `( = )` for you; `Testable.of_module`
takes a module with the conventional `t`/`pp`/`equal` trio;
`of_equal` compares without printing (failures show `<abstract>` —
prefer `Testable.make` as soon as anything is printable). `contramap`
projects before comparing *and* printing:

```ocaml
let by_length = contramap String.length int in
equal by_length "abc" "xyz"
```

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

String containment gets its own verbs because their failures print
the needle with its verdict (`needle "secret" — found at byte 10`)
over a bounded excerpt of the haystack, the occurrence marked when
there is one, instead of printing `false`:

```ocaml
contains ~sub:"user=alice" log;
not_contains ~sub:"secret" log
```

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
the common ones (`~substring` or `~exact` constrain the message):

```ocaml
raises_match (Exn.invalid_arg ~substring:"negative") (fun () ->
    invalid_arg "checkout: negative coupon")
```

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
