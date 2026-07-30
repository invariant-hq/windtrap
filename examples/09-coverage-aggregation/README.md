# Coverage across several test stanzas

Two test stanzas share one instrumented library. Each executable's inline
line is a *view*: it counts the points of the code linked into that binary.
The linker drops modules a binary never references — `test_a` carries no
trace of `Half_b` — and `test_b` links all of `Half_a` because it calls one
function from it, so the two views have different denominators and their
percentages never sum, average, or compare.

The project number is the merge of every executable's dump: the file set is
the union, a file in several dumps must carry identical point tables, counts
add per point, and the denominator is every instrumented point linked into
at least one test executable. The honest limit: code in libraries without
the `(instrumentation (backend ppx_windtrap))` stanza — and modules no test
executable links at all — never registers, so it is silently absent from
the denominator, not reported as 0%.

One rule, once, produces it (`--min` makes the alias a CI gate; test runs
themselves never fail on coverage):

    dune build @examples/09-coverage-aggregation/cover --instrument-with ppx_windtrap
