# The rewriters' rules

Every rule the three rewriters implement, with the interface line that
states it and what pins it. Neither self-coverage nor self-mutation
measures the rewriters, so this list is their completeness measure: a
rule pinned by nothing is a rule any change may break unseen.

- **Interface**: the line of `ppx/coverage/instrument.mli` (`cov`) or
  `ppx/mutate/instrument.mli` (`mut`) that states the rule. The
  expect rewriter's rules cite `ppx/ppx_windtrap.mli` (`pwt`) or
  `ppx/config/expect_test_config.mli`.
- **Pinned by**: a golden fixture (a directory under `coverage/`,
  `mutate/` or `expect/`, one `.ml` and its `.expected`), a real
  inline-test library or runner (named by its `.ml` file), a test of a
  semantics or integration suite (named by its file and title), or a
  build that fails when the rule breaks. A rule no test pins says
  `STATED-NOT-TESTED` and why: the reason is a consequence of a pinned
  rule, named. A part marked `CUT` goes back to the interface's writer
  to be removed, with its L9 letter.

A fixture names each rule it pins by its id and interface line, as in
`(* C21, cov:51 *)`. `check_rules.ml` fails the family's `runtest` when
a fixture named here does not carry the rule's id, when a fixture cites
a rule whose row does not name it, and when a rule is unpinned without a
`STATED-NOT-TESTED` reason.

## Coverage (`ppx/coverage/instrument.ml`)

### Entry points

| id | rule | interface | pinned by |
| --- | --- | --- | --- |
| C1 | A function has one point per curried chain, at its innermost body. | cov:33-34 | `coverage/fixture_fun`; `test_coverage_semantics.ml` "an arm that is itself a function" |
| C2 | A type constraint on the leaf body is kept around the visit. | cov:34-35 | `coverage/fixture_fun` |
| C3 | A coercion on the leaf body is kept around the visit. | cov:34-35 | `coverage/fixture_entries` |
| C4 | The default of an optional argument of a function is an entry point, its inner calls traversed. | cov:36-37 | `coverage/fixture_fun` |
| C5 | The default of an optional argument of a class is an entry point. | cov:36-37 | `coverage/fixture_class` |
| C6 | Each arm of a `match`, `try` and `function` is an entry point, its extent from the pattern's start to the body's end. | cov:38, cov:100-102 | `coverage/fixture_match`, `coverage/fixture_fun` |
| C7 | An arm's extent is the body alone when the pattern is ghost or starts after the body. | cov:102-103 | `coverage/generated/fixture_generated` |
| C8 | The guard of an arm is an entry point. | cov:38-39 | `coverage/fixture_match` |
| C9 | An arm whose body is `assert false` has no point. | cov:55 | `coverage/fixture_match` |
| C10 | A refutation arm has no point. | cov:55-56 | `coverage/fixture_entries` |
| C11 | An arm whose body carries `[@coverage off]` has no point. | cov:56 | `coverage/fixture_entries` |
| C12 | Each branch of an `if` is an entry point; an `if` without `else` has its `then` point only. | cov:40 | `coverage/fixture_if_loops` |
| C13 | The bodies of `while` and `for` are entry points. | cov:41 | `coverage/fixture_if_loops` |
| C14 | A non-trivial `lazy` body is an entry point; a trivial one (function, identifier, constant, constant constructor, constrained) is left alone. | cov:42-44 | `coverage/fixture_lazy`; `test_coverage_semantics.ml` "lazy stays lazy" |
| C15 | A `lazy` of a trivial value under a coercion is left alone. | cov:44 | `coverage/fixture_entries` |
| C16 | A method body (`Pexp_poly`) is marked unless it is a function. | cov:46 | `coverage/fixture_class` |
| C17 | Each body of a binding operator form, nested and with `and*`, is an entry point. | cov:45 | `coverage/fixture_letop` |
| C18 | The body of a concrete method and of an initializer is an entry point. | cov:46 | `coverage/fixture_class` |
| C19 | A virtual method is left alone. | cov:46 | `coverage/fixture_class` |
| C20 | The right operand of `&&` is an entry point. | cov:47 | `coverage/fixture_and_or`; `test_coverage_semantics.ml` "\|\| and && short-circuit" |
| C21 | `&` is handled as `&&`. | cov:50-51 | `coverage/fixture_entries` |
| C22 | `a \|\| b` becomes `if a then (v; true) else if b then (w; true) else false`. | cov:48-50 | `coverage/fixture_and_or`; `test_coverage_semantics.ml` "\|\| and && short-circuit" |
| C23 | `or` is handled as `\|\|`. | cov:50-51 | `coverage/fixture_entries` |
| C24 | A nested `\|\|` right operand is recursed into, not demoted. | cov:48-50 | `coverage/fixture_and_or` |
| C25 | The right operand of `\|\|` in tail position stays the `else` branch when it is an application of a non-trivial function. | cov:59-62 | `coverage/fixture_and_or`; `test_coverage_semantics.ml` "deep tail recursion" |
| C26 | ... when it is a method call or a `new`. | cov:61-62 | `coverage/fixture_entries` (a method call); `new`: CUT (L9 e), a `new` is an object and never a `bool`, so no well-typed `\|\|` has one for its right operand |
| C27 | ... when it is a `let`, `let module`, `let exception`, `let open`, `match`, `try`, `if`, sequence, binding operator form, type constraint or coercion. | cov:62-65 | `coverage/fixture_or_tail_branch`, `coverage/fixture_or_tail_scope`, `coverage/fixture_or_tail_wrap`; `test_coverage_semantics.ml` "\|\| right arms that are not applications" |
| C28 | A right operand of `\|\|` in tail position that applies a trivial primitive is demoted and marked. | cov:60-62 | `coverage/fixture_entries` |
| C29 | What follows an `if` without `else` in a sequence is an entry point. | cov:52-53 | `coverage/fixture_entries` |
| C30 | A function whose body is `assert false` keeps its point. | cov:56-57 | `coverage/fixture_scope` |

### Out-edges

| id | rule | interface | pinned by |
| --- | --- | --- | --- |
| C31 | A non-tail application becomes `___windtrap_post_visit___ i e`. | cov:69-72 | `coverage/fixture_apply`, `coverage/fixture_match`; `test_coverage_semantics.ml` "a raising application's out-edge is not counted" |
| C32 | An application in tail position is not wrapped. | cov:74 | `coverage/fixture_apply`; `test_coverage_semantics.ml` "deep tail recursion" |
| C33 | A pipeline in tail position is not wrapped. | cov:74 | `coverage/fixture_pipeline`; `test_coverage_semantics.ml` "deep tail recursion" |
| C34 | A method call in tail position is not wrapped. | cov:74 | `coverage/fixture_class` |
| C35 | A `new` in tail position is not wrapped. | cov:74 | `coverage/fixture_out_edges` |
| C36 | A method call not in tail position is wrapped. | cov:72 | `coverage/fixture_class`; `test_coverage_semantics.ml` "pipelines and method calls" |
| C37 | A `new` that is not applied is wrapped. | cov:72 | `coverage/fixture_out_edges` |
| C38 | `assert e` is wrapped in any position. | cov:72, cov:76-77 | `coverage/fixture_out_edges` |
| C39 | `assert false` is never wrapped. | cov:77 | `coverage/fixture_match`, `coverage/fixture_scope` |
| C40 | An application of a trivial primitive, matched by spelling, is not wrapped. | cov:80-85 | `coverage/fixture_primitives` (all 38) |
| C41 | `Stdlib.( + ) a b` is wrapped (the match is by spelling). | cov:81 | `coverage/fixture_out_edges` |
| C42 | An application whose every argument is labelled or optional is not wrapped. | cov:86-88 | `coverage/fixture_scope` |
| C43 | An application or method call in the body of a `[@tail_mod_cons]` binding, top level or `let ... in`, is not wrapped; a `new` and an `assert` there are. | cov:89-92 | `coverage/fixture_tmc`; `test_coverage_semantics.ml` "tail_mod_cons survives instrumentation" |
| C44 | The scrutinee of a `match` has no out-edge. | cov:93-94 | `coverage/fixture_out_edges` |
| C45 | The condition of an `if` has no out-edge. | cov:94 | `coverage/fixture_out_edges` |
| C46 | The applied left operand of `@@` has no out-edge. | cov:95 | `coverage/fixture_out_edges` |
| C47 | The right operand of `\|>` or `\|.` has no out-edge. | cov:95 | `coverage/fixture_out_edges` |
| C48 | A method call in callee position has no out-edge. | cov:95-96 | `coverage/fixture_out_edges` |
| C49 | `\|.` is handled as `\|>`. | cov:74, cov:95 | `coverage/fixture_out_edges` |

### Extents, identity and numbering

| id | rule | interface | pinned by |
| --- | --- | --- | --- |
| C50 | An entry point is keyed at its block's start; an operand of `\|\|` at its last byte. | cov:109-110 | `coverage/fixture_and_or` |
| C51 | An out-edge with a known successor is keyed at the successor's start: a one-binding `let`'s body, a sequence's second expression, a pipeline's right operand. | cov:111-114 | `coverage/fixture_apply`, `coverage/fixture_pipeline` |
| C52 | A `let` with several bindings gives no successor. | cov:112-113 | `coverage/fixture_keys` |
| C53 | Any other out-edge of an application is keyed at its callee's last byte. | cov:115 | `coverage/fixture_apply` |
| C54 | ... at `l`'s last byte for `l @@ x`. | cov:116 | `coverage/fixture_keys` |
| C55 | ... at the last byte of the head function of a successor-less pipeline's last stage. | cov:116-117 | `coverage/fixture_keys` |
| C56 | A method call or `new` without successor is keyed at the expression's last byte. | cov:118-119 | `coverage/fixture_keys` (a method call); `new`: CUT (L9 e), a key shows only when another mark shares it, and no well-typed program puts one at the last byte of a `new` |
| C57 | Two marks at one offset are one point, keeping the extent recorded first. | cov:106-107, cov:121-127 | `coverage/fixture_and_or` |
| C58 | Points are numbered by first allocation, a node's sub-expressions before its own blocks. | cov:129-132 | every coverage golden (`coverage/fixture_match`) |
| C59 | A mark at a ghost location is not inserted. | cov:134-135 | `coverage/generated/fixture_generated` |
| C60 | The payloads of extension nodes and attributes are never traversed. | cov:135-136 | `coverage/fixture_keys` |
| C61 | The four effects on a file the mutation rewriter ran on first. | cov:138-151 | `coverage/after_mutate/fixture_guards` |

### Exclusion attributes

| id | rule | interface | pinned by |
| --- | --- | --- | --- |
| C62 | `[@coverage off]` on an expression leaves it as written. | cov:157-158 | `coverage/fixture_off` |
| C63 | `[@@coverage off]` on a top-level value binding. | cov:159-160 | `coverage/fixture_off` |
| C64 | `[@@coverage off]` on a module binding, recursive or not. | cov:160 | `coverage/fixture_off` (non-recursive), `coverage/fixture_off_structure` (recursive) |
| C65 | `[@@coverage off]` on a `let ... in` binding or another item is ignored, its payload unchecked. | cov:160-162 | `coverage/fixture_off_structure` |
| C66 | `[@@@coverage off]` ... `[@@@coverage on]` is a region, module expressions included. | cov:163-164 | `coverage/fixture_off` |
| C67 | A nested structure inherits a region, and its end restores the outer setting. | cov:164-166 | `coverage/fixture_off_structure` |
| C68 | A region never closed runs to the end of its structure. | cov:166-167 | `coverage/fixture_off_structure` |
| C69 | A top-level `[@@@coverage exclude_file]` returns the file as parsed. | cov:168-169, cov:199 | `coverage/fixture_exclude` |
| C70 | The input names `//toplevel//`, `(stdin)`, `.ocamlinit`, `topfind` return the file as parsed. | cov:200-201 | `coverage/fixture_input_name` and the input_name rules of coverage/dune |
| C71 | A file where no point was allocated is returned as parsed. | cov:202-203 | `coverage/fixture_empty` (no instrumented form), `coverage/fixture_all_off` (every form switched off); every form generated: STATED-NOT-TESTED, a consequence of C59 and of the switched-off case |
| C81 | An attribute inside excluded code is never examined. | cov:207-208 | `coverage/fixture_off_structure` |

### Generated code

| id | rule | interface | pinned by |
| --- | --- | --- | --- |
| C72 | Four items in order: stop comment, `Windtrap_cov___<name>`, its `open`, stop comment; `register ~file ~points ~counts` once. | cov:173-184 | every coverage golden |
| C73 | `___windtrap_post_visit___` is bound when the file has an out-edge, and only then. | cov:185-186 | `coverage/fixture_apply` (bound), `coverage/fixture_off` (not bound) |

### Rejections

| id | rule | interface | pinned by |
| --- | --- | --- | --- |
| C74 | A payload other than the three identifiers is refused. | cov:156, cov:212 | `coverage/reject_bad_payload`, `coverage/reject_off_reason`, `coverage/reject_empty_payload` |
| C75 | `on` on an expression is refused. | cov:213 | `coverage/reject_misplaced_on` |
| C76 | `on` on a binding is refused. | cov:213 | `coverage/reject_on_binding` |
| C77 | `exclude_file` on an expression or a binding is refused. | cov:213 | `coverage/reject_exclude_file_binding`, `coverage/reject_exclude_file_expr` |
| C78 | `exclude_file` floating in a nested structure is refused. | cov:214 | `coverage/reject_misplaced_exclude_file` |
| C79 | `[@@@coverage off]` inside a region is refused: "Coverage is already off." | cov:215 | `coverage/reject_double_off` |
| C80 | `[@@@coverage on]` outside a region is refused: "Coverage is already on." | cov:215 | `coverage/reject_on_outside` |

## Mutation (`ppx/mutate/instrument.ml`)

### Operators

| id | rule | interface | pinned by |
| --- | --- | --- | --- |
| M1 | `neg` on an `if` or `while` condition and an arm's guard that is neither comparison nor connective. | mut:56-58 | `mutate/fixture_neg`; `test_mutate_semantics.ml` "evaluates exactly as its twin does"; `integration/test_registration.ml` behaviour |
| M2 | `neg` nowhere else. | mut:56-58 | `mutate/fixture_con`, `mutate/fixture_nesting` |
| M3 | The six `cmp` rewrites and their names. | mut:59-62 | `mutate/fixture_cmp`; `integration/test_registration.ml` behaviour |
| M4 | An ordering's armed arm swaps its operands under `Stdlib.not`, pinned by `operands`; an equality's negates the whole comparison. | mut:74-75, mut:182-183 | `mutate/fixture_cmp`; `test_mutate_semantics.ml` "evaluates exactly as its twin does" |
| M5 | `cmp` sites lie in a boolean context alone. | mut:62, mut:68-72 | `mutate/fixture_cmp` |
| M6 | An operand of `\|\|` is a boolean context. | mut:68-69 | `mutate/fixture_contexts`, `mutate/fixture_lost_con` (a file without `con`); `test_mutate_semantics.ml` "evaluates exactly as its twin does" |
| M7 | A boolean context does not reach through a sequence. | mut:70 | `mutate/fixture_assert` |
| M8 | ... nor through a `let` body, a type constraint or `not`. | mut:70-71 | `mutate/fixture_contexts` |
| M9 | `con` swaps `&&` and `\|\|` in one branch, through `Stdlib.(<>)`/`Stdlib.(=)` and `Stdlib.Bool.t`, short-circuit kept. | mut:63-64 | `mutate/fixture_con`; `test_mutate_semantics.ml` "tail calls survive instrumentation" |
| M10 | `&` and `or` are not sites. | mut:64 | `mutate/fixture_contexts` |
| M11 | The four `ari` rewrites, in every context. | mut:65-66, mut:72 | `mutate/fixture_ari`; `integration/test_registration.ml` behaviour |
| M12 | Unary minus is not a site. | mut:52-53 | `mutate/fixture_ari` |
| M13 | A qualified operator is not a site. | mut:53-55 | `mutate/fixture_contexts` |
| M14 | A labelled or partial application is not a site. | mut:53-55 | `mutate/fixture_contexts` |
| M15 | An armed ordering differs from its `after` text on NaN. | mut:75-76 | STATED-NOT-TESTED: a consequence of M4 (the armed arm is `not (b < a)`) and of the float comparisons on NaN |

### Placement

| id | rule | interface | pinned by |
| --- | --- | --- | --- |
| M16 | One mutant per expression: a comparison condition carries `cmp`, a connective one `con`, any other `neg`. | mut:81-83 | `mutate/fixture_nesting` |
| M17 | A connective with a connective operand carries no mutant. | mut:84-86 | `mutate/fixture_nesting`, `mutate/fixture_chain` |
| M18 | The operands of such a connective are boolean contexts all the same. | mut:86 | `mutate/fixture_contexts` |
| M19 | In a file that lost `cmp` or `con`, such a condition carries `neg`. | mut:87-88 | `mutate/fixture_lost_cmp`, `mutate/fixture_lost_con` |
| M20 | In a chain of one arithmetic operator the outermost application alone is a site, read from the tree. | mut:89-93 | `mutate/fixture_chain`; `test_mutate_semantics.ml` "evaluates exactly as its twin does" |
| M21 | The chain rule applied to `con`. | none (unreachable code) | STATED-NOT-TESTED: unreachable, a consequence of M17 (a connective with a connective operand carries no mutant, so no `con` chain reaches the chain rule) |
| M22 | In `a < b < c` only the outer comparison is a site. | mut:68-72 | `mutate/fixture_chain` |
| M23 | Guards bind `__windtrap_mut_<i>_<role>`, distinct under nesting. | mut:94-95 | `mutate/fixture_chain` |

### Code that is never mutated

| id | rule | interface | pinned by |
| --- | --- | --- | --- |
| M24 | An `assert`, with everything under it. | mut:99 | `mutate/fixture_assert` |
| M25 | A `lazy` of a trivial value, with everything under it. | mut:100-103 | `mutate/fixture_lazy`; `test_mutate_semantics.ml` "lazy stays lazy" |
| M26 | The payloads of attributes and extension nodes. | mut:104 | `mutate/fixture_payloads` |
| M27 | A file holding an extension node named `test` or `expect_test`. | mut:105-106 | `mutate/fixture_inline_tests` (both names), `mutate/fixture_inline_test_only`, `mutate/fixture_inline_expect_only` |
| M28 | A file naming an identifier under `Ppx_windtrap_runtime.Ppx_runtime`. | mut:106-108 | `mutate/fixture_inline_expanded` |
| M29 | A site at a ghost location. | mut:109 | `mutate/generated/fixture_generated` |
| M30 | A site whose line, column and rewrite an earlier site has. | mut:110-112 | `mutate/generated/fixture_generated` |
| M31 | Module initialization code is mutated. | mut:114-115 | `mutate/fixture_ari` |

### The emission law

| id | rule | interface | pinned by |
| --- | --- | --- | --- |
| M32 | A variable pattern named after an operator removes its family (`ari` for `+`). | mut:34-40 | `mutate/fixture_shadow` |
| M33 | A value description (an `external`) named after an operator removes its family. | mut:39-40 | `mutate/fixture_lost_con` |
| M34 | The `cmp` and `con` families are removed as `ari` is. | mut:36-37 | `mutate/fixture_lost_cmp`, `mutate/fixture_lost_con` |
| M35 | `neg` names `Stdlib.not` and is never lost. | mut:37-38 | build of `mutate/integration/shadowed.ml`; never lost: `mutate/fixture_lost_cmp` |
| M36 | Guards name `Stdlib.Bool.t`, which survives a local `type bool`. | mut:28-30 | build of `mutate/integration/shadowed.ml` |
| M37 | Operands are typed left to right, the right one given the left one's type. | mut:182-183 (the annotation; the typing order is stated nowhere) | builds of `mutate/integration/disambiguate.ml`, `expected_type.ml`; `integration/test_registration.ml` typing_context, expected_type |

### Dismissal attributes

| id | rule | interface | pinned by |
| --- | --- | --- | --- |
| M38 | `[@mutate off "r"]` on a site records it dismissed with the reason, with an index and no guard. | mut:126-129 | `mutate/fixture_off`, `mutate/fixture_all_dismissed`, `mutate/fixture_off_edges` |
| M39 | `[@mutate off]` without a reason records `""`. | mut:128-129 | `mutate/fixture_off` |
| M40 | `[@mutate off]` on an expression that is no site records nothing and suppresses what is inside. | mut:129-131 | `mutate/fixture_off_edges` |
| M41 | `[@@mutate off]` on a top-level value binding and a module binding, recursive or not. | mut:132-133 | `mutate/fixture_off` (non-recursive), `mutate/fixture_off_structure` (recursive) |
| M42 | `[@@mutate off]` on a `let ... in` binding or another item is ignored, its payload unchecked. | mut:133-135 | `mutate/fixture_off_edges` (ignored), `mutate/fixture_off_structure` (payload not checked) |
| M43 | `[@@@mutate off]` ... `[@@@mutate on]` is a region; a nested structure inherits it and restores the outer setting; an unclosed one runs to the end. | mut:136-140 | `mutate/fixture_off`, `mutate/fixture_off_unclosed`, `mutate/fixture_off_structure` (nested) |
| M44 | A reason on `[@@mutate off]` or `[@@@mutate off]` is accepted and dropped. | mut:144-145 | `mutate/fixture_off_structure` |
| M45 | A top-level `[@@@mutate exclude_file]` returns the file as parsed. | mut:141-142, mut:217 | `mutate/fixture_exclude` |
| M46 | The input names `//toplevel//`, `(stdin)`, `.ocamlinit`, `topfind` return the file as parsed. | mut:219-220 | `mutate/fixture_input_name` and the input_name rules of mutate/dune |

### Identification

| id | rule | interface | pinned by |
| --- | --- | --- | --- |
| M47 | Line one-based, column zero-based in bytes, a bracketed site starting at its bracket. | mut:151-154 | every mutation golden (`mutate/fixture_off`, `mutate/fixture_ari`) |
| M48 | Two sites may share a line and column under two rewrites. | mut:155 | `mutate/fixture_cmp`, `mutate/fixture_chain` |
| M49 | Sites are numbered top-down, left operand before right. | mut:157-160 | `mutate/fixture_cmp`, `mutate/fixture_chain`, `mutate/fixture_neg` |
| M50 | `before` and `after` are printed from the parsetree, the site's attributes left out. | mut:162-163 | `mutate/fixture_off` |
| M51 | Each run of blanks in a text becomes one space. | mut:163-165 | `integration/test_registration.ml` typing_context |
| M52 | ... inside a string literal too. | mut:164 | `mutate/fixture_texts` |

### Generated code

| id | rule | interface | pinned by |
| --- | --- | --- | --- |
| M53 | Three items in order, the module `Windtrap_mut___<name>` never opened. | mut:170-176 | every mutation golden; build of `mutate/integration/no_guard.ml` |
| M54 | `type site = Windtrap_runtime.Mutate.site = { ... }` with its six fields. | mut:179-181 | every mutation golden |
| M55 | `type 'a operands` only in a file with an ordering guard. | mut:182-183 | `mutate/fixture_cmp` (present), `mutate/fixture_ari` (absent) |
| M56 | Every generated node is ghost; the disarmed arm keeps its location and attributes. | mut:189-191 | `mutate/fixture_texts` (attributes), `coverage/after_mutate/fixture_guards` (ghost: the disarmed arm alone takes a mark) |
| M57 | A file whose every site is dismissed is registered, by a module no guard refers to. | mut:221-223 | `mutate/fixture_all_dismissed` |
| M58 | The two effects on a file the coverage rewriter ran on first. | mut:195-201 | `mutate/after_coverage/fixture_visits` |

### Rejections

| id | rule | interface | pinned by |
| --- | --- | --- | --- |
| M59 | An unknown identifier payload is refused. | mut:233-234 | `mutate/reject_bad_payload` |
| M60 | Other payload shapes (empty, `off 42`, `off "a" "b"`) are refused. | mut:233-234 | `mutate/reject_empty_payload`, `mutate/reject_off_number`, `mutate/reject_off_two_reasons` |
| M61 | `on` on an expression is refused. | mut:235 | `mutate/reject_misplaced_on` |
| M62 | `on` or `exclude_file` on a binding, `exclude_file` on an expression, are refused. | mut:235 | `mutate/reject_on_binding`, `mutate/reject_exclude_file_binding`, `mutate/reject_exclude_file_expr` |
| M63 | `exclude_file` floating in a nested structure is refused. | mut:236 | `mutate/reject_misplaced_exclude_file` |
| M64 | `[@@@mutate off]` inside a region is refused: "Mutation is already off." | mut:237 | `mutate/reject_double_off` |
| M65 | `[@@@mutate on]` outside a region is refused: "Mutation is already on." | mut:237 | `mutate/reject_on_outside` |

## Expect (`ppx/ppx_windtrap.ml`)

| id | rule | interface | pinned by |
| --- | --- | --- | --- |
| E1 | `let%expect_test "n"` registers `add_test`, its body under `Expect_test_config.run` constrained to the synchronous type. | pwt:20-23, pwt:29-30 | `expect/expect_basic` |
| E2 | A `_` name becomes `line_<N>`. | pwt:23-25 | `expect/expect_basic`, `expect/test_basic` |
| E3 | Any other name pattern is refused. | pwt:69-71 | `expect/reject_name_pattern`, `expect/reject_test_name_pattern` |
| E4 | Anything but one non-recursive binding is refused. | pwt:72-73 | `expect/reject_two_bindings`, `expect/reject_rec_binding` |
| E5 | `[@tags "s"]` and `[@tags "a", "b"]` on the name pattern. | pwt:25-26 | `expect/expect_basic`, `expect/test_basic` |
| E6 | A malformed `[@tags]` is refused. | pwt:77-78 | `expect/reject_malformed_tags`, `expect/reject_malformed_tags_tuple` |
| E7 | A `[@@tags]` on the binding, not the pattern, is ignored. | pwt:26-27 | `expect/expect_attributes` |
| E8 | `pos` is file, line, and both columns from the start line. | pwt:23 (the shape: `Windtrap.pos`) | `expect/expect_basic`, `expect/test_basic` |
| E9 | `[%expect lit]` and `[%expect_exact lit]` become core calls, the literal kept with its delimiters. | pwt:35-38 | `expect/expect_basic`; `expect/config/config_shadow.ml` (`{%expect_exact\|...\|}`) |
| E10 | A bare `[%expect]` has the literal `""`. | pwt:38 | `expect/expect_basic`; `expect/inline/inline_expect.ml` "bare expect" |
| E11 | A node's attributes are carried onto its call. | pwt:41 | `expect/expect_attributes` |
| E12 | `[%expect.output]` is the sanitized read. | pwt:39 | `expect/expect_basic`; `expect/inline/inline_expect.ml` "output is consumed, not matched" |
| E13 | `[%expect.output]` with a payload is refused. | pwt:80-81 | `expect/reject_output_payload` |
| E14 | A payload that is not a string literal is refused. | pwt:79-80 | `expect/reject_bad_payload` |
| E15 | An unimplemented family node inside a body is refused. | pwt:84-86 | `expect/reject_unreachable`, `expect/reject_if_reached` |
| E16 | An implemented node outside a body is refused. | pwt:82-83 | `expect/reject_expect_outside` |
| E17 | An unimplemented node outside a body is refused. | pwt:84-86 | `expect/reject_expectation`, `expect/reject_expect_prefix`, `expect/reject_expectation_prefix` |
| E18 | A family attribute on the binding, the name pattern or a `module%test` is refused. | pwt:87-89 | `expect/reject_uncaught_exn`, `expect/reject_pattern_attr`, `expect/reject_module_attr`, `expect/reject_test_binding_attr`, `expect/reject_test_pattern_attr` |
| E19 | A family attribute anywhere else is refused by the leftover scan. | pwt:87-89 | `expect/reject_leftover_attr`, `expect/reject_dropped_body` |
| E20 | `let%test` registers `add_test` without `run`. | pwt:47-49 | `expect/test_basic`; `expect/config/config_shadow.ml` (at run time) |
| E21 | `module%test M` becomes `enter_group`, the module, `leave_group`; `[@@tags]` consumed, other attributes kept. | pwt:51-54 | `expect/test_basic` |
| E22 | `module%test _` or another item is refused. | pwt:73-76 | `expect/reject_test_anonymous_module`, `expect/reject_test_item` |
| E23 | The cookie `inline_tests`: `enabled` keeps, `disabled` drops, another value is refused. | pwt:58-62, pwt:90-92 | the cookie rules of expect/dune over `expect/expect_basic` and `expect/test_basic` (cookie_enabled, cookie_disabled, cookie_invalid) |
| E24 | The cookie value `ignored` drops. | pwt:58-59 | the rule cookie_ignored of expect/dune over `expect/test_basic` |
| E25 | The drop applies to `let%test` and `module%test`. | pwt:58-59 | the rule cookie_ignored of expect/dune over `expect/test_basic` |
| E26 | Generated code is warning-free under `-w +a -warn-error +a`. | pwt:15-16 | build of `expect/strict_flags/inline_strict.ml` |
| E27 | `Expect_test_config` is named unqualified, so a local module shadows it. | pwt:30-32; expect_test_config.mli:15-16 | `expect/config/config_shadow.ml`; `examples/05-baselines/timing.ml` |
| E28 | A monadic `run` fails to compile at the reference. | pwt:31-32 | `expect/wrong_run/wrong_run.ml` (located at the test); `test/conformance`, `hello_async.compile-rejected.expected` |
| E29 | Nothing is checked after the body: output written after its last node, and a node it never reaches, fail nothing. | pwt:41-43 | `expect/config/config_shadow.ml`; `test/conformance`, `negative-tests/trailing.ml` |
| E30 | A dropped form is still refused for a bad name, shape or `[@tags]`, and a `let%expect_test` for a bad node; the rest of a dropped body is not checked. | pwt:60-61 | the cookie_disabled rules of expect/dune over `expect/reject_name_pattern`, `expect/reject_two_bindings`, `expect/reject_malformed_tags`, `expect/reject_bad_payload`, `expect/reject_dropped_body` |
| E31 | A test registers when the structure that holds it is evaluated: at module load, or at each application of an enclosing functor. | pwt:20-22 | STATED-NOT-TESTED: a consequence of E1 (the registration is a `let () =` item in place of the test) |
| E32 | A body that calls `Windtrap.output` itself reads the output unsanitized. | expect_test_config.mli:50 | `expect/config/config_shadow.ml` |
| E33 | An override of `run` that never calls `f` passes its test with nothing checked; one that calls it twice runs every expectation of the body twice. | expect_test_config.mli:37-39 | `expect/config/config_calls.ml` |
| E34 | The sanitized text is the text compared and the text a correction writes. | expect_test_config.mli:48-49 | `expect/correction/sanitized.ml` (its correction and transcript) |
