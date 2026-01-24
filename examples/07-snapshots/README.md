# `07-snapshots`

Snapshot testing. Compare output against saved files that are automatically
created and updated.

```bash
dune exec ./examples/07-snapshots/main.exe
```

## What You'll Learn

- Creating snapshots with `snapshot`
- Using named snapshots for clarity
- Formatted snapshots with `snapshot_pp` and `snapshotf`
- Updating snapshots with `-u` (or `--update`)

## Key Functions

| Function | Purpose |
|----------|---------|
| `snapshot` | Compares string against saved snapshot file |
| `snapshot_pp` | Snapshot with a pretty-printer |
| `snapshotf` | Snapshot with printf-style formatting |

## How It Works

1. **First run**: Creates snapshot files in `__snapshots__/`
2. **Subsequent runs**: Compares output against saved snapshots
3. **Update**: Run with `-u` to accept new values

## Snapshot Location

Snapshots are stored in `__snapshots__/<test-name>/`:

```
examples/07-snapshots/
├── main.ml
└── __snapshots__/
    └── Snapshots/
        ├── L36_C29.snap
        └── greeting.snap
```

Named snapshots (using `~name`) create readable filenames.

## Updating Snapshots

When output intentionally changes:

```bash
dune exec ./examples/07-snapshots/main.exe -- -u
```

This accepts the current output as the new expected value.

## Try It

1. Run the example to create initial snapshots.
2. Modify a snapshot value in the code and run again to see the diff.
3. Use `-u` to accept the change.

## Next Steps

Continue to [08-properties](../08-properties/) to learn about property-based
testing.
