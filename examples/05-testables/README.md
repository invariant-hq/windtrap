# `05-testables`

Type-safe comparisons with testables. Testables combine equality checking with
pretty-printing, so failures show readable values.

```bash
dune exec ./examples/05-testables/main.exe
```

## What You'll Learn

- Using built-in testables for primitive types
- Combining testables with `list`, `option`, `result`, `pair`
- Creating custom testables with `of_equal` and `make`
- Transforming testables with `contramap`
- Unordered comparisons with `slist`

## Key Functions

| Function | Purpose |
|----------|---------|
| `Testable.int`, `.string`, `.bool` | Built-in testables for primitives |
| `Testable.float eps` | Float comparison with epsilon tolerance |
| `Testable.list t` | Testable for lists of `t` |
| `Testable.option t` | Testable for options of `t` |
| `Testable.result ok_t err_t` | Testable for results |
| `Testable.pair a b` | Testable for pairs |
| `Testable.of_equal eq` | Create testable from equality function |
| `Testable.make ~pp ~equal ()` | Create testable with custom printer |
| `Testable.contramap f t` | Transform values before comparing |
| `Testable.slist t cmp` | Compare lists ignoring order |

## Custom Types

For your own types, use `of_equal` for quick setup or `make` for full control:

```ocaml
(* Quick: equality only, opaque printing *)
let point_eq = Testable.of_equal (fun p1 p2 -> p1.x = p2.x && p1.y = p2.y)

(* Full: custom printer for readable failures *)
let point = Testable.make
  ~pp:(fun fmt p -> Format.fprintf fmt "(%d, %d)" p.x p.y)
  ~equal:(fun p1 p2 -> p1.x = p2.x && p1.y = p2.y)
  ()
```

## Try It

1. Create a testable for a record type with multiple fields.
2. Use `slist` to compare two lists that differ only in order.

## Next Steps

Continue to [06-expect](../06-expect/) to learn about output testing.
