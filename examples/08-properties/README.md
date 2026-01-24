# `08-properties`

Property-based testing. Instead of testing specific examples, properties verify
that invariants hold for randomly generated inputs.

```bash
dune exec ./examples/08-properties/main.exe
```

## What You'll Learn

- Writing properties with `prop`
- Multi-argument properties with `prop2`, `prop3`
- Skipping invalid inputs with `assume`
- Using assertions in properties with `prop'`

## Key Functions

| Function | Purpose |
|----------|---------|
| `prop` | Property with one generated argument, returns bool |
| `prop'` | Property with assertions instead of bool return |
| `prop2`, `prop3` | Properties with 2 or 3 arguments |
| `assume` | Skip test case if condition is false |
| `reject` | Explicitly reject the current test case |

## How Properties Work

1. Windtrap generates random inputs based on the testable's generator
2. Your property function checks an invariant
3. If the check fails, Windtrap shrinks the input to find a minimal counterexample

## Example Properties

```ocaml
(* Return true if property holds *)
prop "reverse is involutive"
  Testable.(list int)
  (fun l -> List.rev (List.rev l) = l)

(* Use assertions instead *)
prop' "list is sorted after sort"
  Testable.(list int)
  (fun l ->
    let sorted = List.sort compare l in
    is_true (is_sorted sorted))
```

## Skipping Invalid Inputs

Use `assume` to skip inputs that don't make sense:

```ocaml
prop "division works"
  Testable.(pair int int)
  (fun (a, b) ->
    assume (b <> 0);  (* Skip when divisor is zero *)
    (a * b) / b = a)
```

## Try It

1. Write a property that fails and observe the shrunk counterexample.
2. Create a property for your own function.

## Next Steps

Continue to [09-fixtures](../09-fixtures/) to learn about setup and teardown.
