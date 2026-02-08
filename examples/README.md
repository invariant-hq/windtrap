# Windtrap Examples

Learn Windtrap through progressively complex examples. Start with
`01-first-test` and work through the numbered examples in order.

## Examples

| Example                           | Concept                          | Key Functions                              |
| --------------------------------- | -------------------------------- | ------------------------------------------ |
| [`01-first-test`](01-first-test/) | Creating tests                   | `run`, `test`, `equal`                     |
| [`02-groups`](02-groups/)         | Organizing tests                 | `group`                                    |
| [`03-assertions`](03-assertions/) | Assertion toolkit                | `equal`, `is_true`, `is_some`, `is_ok`     |
| [`04-exceptions`](04-exceptions/) | Exception testing                | `raises`, `raises_match`, `no_raise`       |
| [`05-testables`](05-testables/)   | Type-safe comparisons            | `Testable.*`, `of_equal`, `contramap`      |
| [`06-expect`](06-expect/)         | Output testing                   | `expect`, `capture`, `output`              |
| [`07-snapshots`](07-snapshots/)   | Snapshot testing                 | `snapshot`, `snapshot_pp`                  |
| [`08-properties`](08-properties/) | Property & parameterized testing | `prop`, `cases`, `assume`                  |
| [`09-fixtures`](09-fixtures/)     | Setup and teardown               | `bracket`, `fixture`, `~setup`, `~retries` |
| [`10-tags`](10-tags/)             | Tagging and filtering            | `Tag.labels`, `--filter`                   |
| [`11-inline-tests`](11-inline-tests/) | Inline tests                 | `let%expect_test`, `[%expect]`             |
| [`12-ppx`](12-ppx/)               | PPX test syntax                  | `let%test`, `let%expect_test`, `[%%run_tests]` |
| [`13-cli`](13-cli/)               | Command-line options             | `--format`, `--junit`                      |
| [`x-demo`](x-demo/)               | Visual showcase                  | All features with intentional failures     |

## Running Examples

All examples can be run with:

```bash
dune exec ./examples/<name>/main.exe
```

For example:

```bash
dune exec ./examples/01-first-test/main.exe
```

The inline tests example (11-inline-tests) runs differently:

```bash
dune runtest
```

## Quick Reference

### Basic Test

```ocaml
open Windtrap

let () =
  run "My Tests"
    [
      test "addition" (fun () -> equal Testable.int 5 (2 + 3));
    ]
```

### With Groups

```ocaml
run "My Tests"
  [
    group "Math"
      [
        test "add" (fun () -> equal Testable.int 5 (2 + 3));
        test "sub" (fun () -> equal Testable.int 1 (3 - 2));
      ];
  ]
```

### Common Assertions

```ocaml
equal Testable.int 42 result          (* equality *)
is_true (x > 0)                       (* boolean *)
is_some (find key map)                (* option *)
is_ok (parse input)                   (* result *)
raises (Failure "error") (fun () -> f x)  (* exception *)
```

### Output Testing

```ocaml
test "prints greeting" (fun () ->
    Printf.printf "Hello, %s!" name;
    expect "Hello, World!")
```

### Property Testing

```ocaml
prop "reverse is involutive"
  Testable.(list int)
  (fun l -> List.rev (List.rev l) = l)
```

### Parameterized Tests

```ocaml
cases Testable.(triple int int int)
  [(2, 3, 5); (0, 0, 0); (-1, 1, 0)]
  "addition"
  (fun (a, b, expected) -> equal Testable.int expected (a + b))
```

### Fixtures

```ocaml
(* Per-test setup/teardown *)
let with_db = bracket ~setup:connect ~teardown:disconnect

group "Database" [
  with_db "can query" (fun db -> ...);
]

(* Lazy shared resource *)
let get_db = fixture (fun () -> connect ())

(* Suite-level hooks *)
group "Integration"
  ~setup:(fun () -> init_environment ())
  ~teardown:(fun () -> cleanup ())
  [ test "test1" (fun () -> ...); ]
```

## Command-Line Options

| Option                    | Purpose                                  |
| ------------------------- | ---------------------------------------- |
| `-s, --stream`            | Stream output to console (don't capture) |
| `-v, --verbose`           | Verbose output (alias for `--format verbose`) |
| `-q, --quick`             | Skip slow tests                          |
| `-x, --fail-fast`         | Stop on first failure                    |
| `-l, --list`              | List tests without running               |
| `-u, --update`            | Update snapshot files                    |
| `-f, --filter PATTERN`    | Filter tests by name                     |
| `PATTERN`                 | Positional filter (same as -f)           |
| `--format FMT`            | Output: verbose, compact, tap, junit     |
| `--junit PATH`            | Write JUnit XML report                   |
| `--seed N`                | Random seed for property tests           |
| `--timeout N`             | Default timeout in seconds               |
