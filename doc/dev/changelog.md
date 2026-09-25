# The changelog

`CHANGES.md` is written for users. It has one entry per release, newest
first, and the entry for the next release is open at the top while the
work happens.

## Headings

A released entry is headed `## vX.Y.Z YYYY-MM-DD`, the tag's date.
`dune-release tag` reads the version from the first heading of the
file, so nothing stands above it. The open entry is headed
`## Unreleased`, and it exists only while it has lines: at release it is
renamed to the version's heading.

## While working

A change a user can see adds its line to the open entry in the same
commit: a new or changed value, flag, variable, file format, output line,
exit code, error message, or default. A change a user cannot see (an
internal refactor, a test, a comment) adds nothing.

One line per change, addressed to the user, in the present tense,
stating what is now true: "`expect_file` reads its baseline relative to
the project root", not "fixed baseline path resolution". A line leads
with the item it changes, in code font, or with the noun of a rule that
spans several ("Checking is read-only; …"). A removal reads "`x` is
removed", and a variable no longer read "`X` is not read".

A change after which code or a workflow of the previous release stops
working is breaking, and its line starts with `(breaking)`.

A line is one sentence, or two joined by a semicolon when the second is
the first's consequence or the way out. What needs more is linked: the
line closes on the manual section that teaches the new form, as
`(doc/manual/baselines.md#accepting-a-change)`, the heading in GitHub's
anchor form (lowercase, spaces to `-`, other punctuation dropped). A
reporter or a patch author is thanked at the end of the line: "Thanks to
A. B. for the report (#12)."

Lines go under the area they belong to, in this order: Declaring tests,
Assertions, Property testing, Stateful testing, Baselines and expect
tests, Resources and process state, Running tests, Coverage, Mutation
testing, Packages and libraries. An empty area is omitted.

## At release

The entry gets highlights, directly under its heading and above the
areas: three to six paragraphs, each one change that matters to most
users, in two or three sentences, with a two-line example where one
helps. The areas below keep every line. Migration notes for a breaking
release live in the manual (`doc/manual/migrating-from-<previous>.md`),
and the first paragraph links them; the changelog is not a migration
guide.
