# The changelog

`CHANGES.md` is written for users. It has one entry per release, newest
first, and the entry for the next release is open at the top while the
work happens.

## While working

A change a user can see adds its line to the open entry in the same
commit: a new or changed value, flag, variable, file format, output line,
exit code, error message, or default. A change a user cannot see (an
internal refactor, a test, a comment) adds nothing. One line per change,
addressed to the user, stating what is now true, not what was done:
"`expect_file` reads its baseline relative to the project root", not
"fixed baseline path resolution". A breaking change starts with
`(breaking)`. A line that needs more than one sentence links the manual
section that explains it.

Lines go under the area they belong to: Declaring tests, Assertions,
Property testing, Stateful testing, Baselines and expect tests, Resources
and process state, Running tests, Coverage, Mutation testing, Packages
and libraries.

## At release

The entry gets a short highlights section above the areas: three to six
paragraphs, each one change that matters to most users, in two or three
sentences with a two-line example where one helps. The areas below it
keep every line. Migration notes for a breaking release live in the
manual (`doc/manual/migrating-from-<previous>.md`) and the highlights
link them; the changelog is not a migration guide.
