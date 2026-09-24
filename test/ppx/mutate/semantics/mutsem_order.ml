(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The mutation-specific half of the guarantee 12 corpus. This file is
   compiled TWICE from one source (see dune and baseline/dune): once
   through [ppx_windtrap.mutate], as [Mutsem_fixtures.Mutsem_order], and
   once untouched, as [Mutsem_baseline.Mutsem_order]. Every observable
   below is produced by both copies and compared string for string, so
   the uninstrumented twin - not a hand-written expectation - is the
   baseline. A hand-written expectation can be edited to match a defect;
   a twin cannot.

   What it is here to settle. [cmp] and [ari] lift their operands into
   one tuple binding before branching:

     a < b  ->  let (__l, __r) = (a, b) in
                if armed then Stdlib.not (__r < __l) else __l < __r

   The instrumenter's manual claims the literal tuple is destructured
   without being built, into a let-chain binding the right component
   first - "the order the compiler gives the uninstrumented
   application". OCaml specifies neither argument nor tuple-component
   evaluation order, so both halves of that claim are statements about
   the compiler in the build, not about the language - and they are the
   ONE place where a disarmed guard could change meaning, because the
   guard FIXES an order the compiler was free to choose. Every operand
   below is a call to [note], which appends a tag to a log, so the
   traces say exactly which operand ran, in which order, and how many
   times. If the compiler ever evaluates the application and the
   destructured tuple in different orders, the [cmp]/[ari] witnesses
   diverge from their twin and this suite goes red at the site of the
   divergence.

   The witnesses also pin short-circuiting, the number of times each
   operand is evaluated, which operand's exception escapes, laziness,
   and - through [tail_depths] - that the [con] encoding keeps its right
   arm in tail position. That last one is measured with
   [Printexc.get_callstack] rather than by recursing until something
   overflows, which decides only under a bounded stack: under OCaml 5's
   default bound of 1 GiB a non-tail recursion of this shape returns
   normally at fifty million frames. [control_depths] is the positive
   control for that measurement. *)

(* {1 The trace} *)

let log : string list ref = ref []

let note tag v =
  log := tag :: !log;
  v

let trace () =
  let seen = List.rev !log in
  log := [];
  String.concat "," seen

(* [show r] is [r] with the trace the computation of [r] left behind.
   [r] is evaluated at the call site, before [trace] runs. *)
let show result = Printf.sprintf "%s | %s" result (trace ())

exception Boom of string

let boom : string -> int =
 fun tag ->
  log := tag :: !log;
  raise (Boom tag)

(* {1 [cmp]: operand order, in every context the operator admits}

   [cmp] fires only in a boolean context - an [if] or [while] condition,
   a [when] guard, or a direct operand of [&&] or [||] - so there is one
   witness per context. The four orderings take the swapping encoding
   (two binders, [__r] then [__l]); [=] and [<>] take the direct one
   (one binder around the whole comparison, which leaves the operand
   order to the compiler). Both must trace exactly as the twin does. *)

let cmp_lt a b = if note "l" a < note "r" b then "t" else "f"
let cmp_le a b = if note "l" a <= note "r" b then "t" else "f"
let cmp_gt a b = if note "l" a > note "r" b then "t" else "f"
let cmp_ge a b = if note "l" a >= note "r" b then "t" else "f"
let cmp_eq a b = if note "l" a = note "r" b then "t" else "f"
let cmp_ne a b = if note "l" a <> note "r" b then "t" else "f"

(* A [while] condition: the operands are re-evaluated every iteration,
   so the trace pins the count as well as the order. *)
let cmp_while limit =
  let i = ref 0 in
  while note "l" !i < note "r" limit do
    incr i
  done;
  string_of_int !i

(* A [when] guard. *)
let cmp_guard a b =
  match a with _ when note "l" a >= note "r" b -> "t" | _ -> "f"

(* Direct operands of the connectives: two sites on one expression, the
   comparison's and the connective's, whose expansions nest. *)
let cmp_under_and a b c =
  if note "l" a < note "r" b && note "c" c then "t" else "f"

let cmp_under_or a b c =
  if note "l" a > note "r" b || note "c" c then "t" else "f"

(* Which operand's exception escapes is the sharpest statement of
   evaluation order there is: it survives any optimization that could
   reorder two pure operands. *)
let cmp_exception_order () =
  match if boom "l" < boom "r" then "t" else "f" with
  | s -> s
  | exception Boom tag -> tag

(* A comparison nested inside another expression, so the guard's
   let-bindings sit under a surrounding application whose own arguments
   the compiler also orders. *)
let both_of x y = Printf.sprintf "(%s,%s)" x y

let cmp_in_args a b c d =
  both_of
    (if note "a" a < note "b" b then "t" else "f")
    (if note "c" c < note "d" d then "t" else "f")

(* A right operand the LEFT one types. [Green] is not in scope at all:
   only the expected type the comparison hands its right operand - the
   type the left one has just fixed, [Color.t] - resolves it, as
   [Color.Green]. The twin gets that expected type from [( < )]'s own
   signature; the guard must get it from the annotation on its tuple,
   and a guard that does not makes this an unbound constructor - a
   build failure of this library rather than a red witness. What the
   witness pins is that the annotation costs no evaluation and changes
   no answer. (test/ppx/mutate/integration/expected_type.ml carries the
   shape where a later type claims the name instead; it cannot live
   here, because test_mutate_semantics.ml coerces this module to its twin's
   signature, which strengthens every top-level datatype to the twin's
   own - so the type is local to the function, and each of its
   constructors is built, for warning 37.) *)
let cmp_sibling n =
  let module Color = struct
    type t = Red | Green | Blue
  end in
  let c : Color.t =
    match n with 0 -> Color.Red | 1 -> Color.Green | _ -> Color.Blue
  in
  if note "l" c < Green then "t" else "f"

(* {1 [ari]: operand order, anywhere}

   [ari] fires on [+], [-], [+.] and [-.] wherever they appear, so these
   witnesses are about position rather than context. *)

let ari_add a b = string_of_int (note "l" a + note "r" b)
let ari_sub a b = string_of_int (note "l" a - note "r" b)
let ari_fadd a b = Printf.sprintf "%.2f" (note "l" a +. note "r" b)
let ari_fsub a b = Printf.sprintf "%.2f" (note "l" a -. note "r" b)

(* A chain of one family, unparenthesized: [a + b + c] is [(a + b) + c],
   and only the outermost application of a chain is a site. The inner
   [+] therefore runs UNGUARDED, in the middle of the outer guard's
   operand bindings - which is the shape most likely to reorder
   something, and the reason it has its own witness. *)
let ari_chain a b c =
  let total = note "a" a + note "b" b + note "c" c in
  string_of_int total

(* The same chain inside an argument's brackets. OCaml's parser gives
   the bracketed expression a location starting at the [(], so the outer
   node no longer starts at the same byte as the inner; the chain rule
   reads the tree, not the layout, and this carries one site too
   (test/ppx/mutate/fixture_chain.ml pins it). The witness pins that the
   bracket changes no evaluation order either. *)
let ari_chain_parens a b c = string_of_int (note "a" a + note "b" b + note "c" c)

(* Right-nested, where both nodes carry a site and the expansions nest. *)
let ari_right_nested a b c =
  string_of_int (note "a" a + (note "b" b + note "c" c))

(* Two families in one chain: two sites, two different rewrites. *)
let ari_mixed a b c = string_of_int (note "a" a + note "b" b - note "c" c)

(* Two guarded operands as the two arguments of one application: the
   compiler orders the arguments, each guard orders its own operands,
   and the interleaving must not change. *)
let ari_in_args a b c d =
  both_of
    (string_of_int (note "a" a + note "b" b))
    (string_of_int (note "c" c + note "d" d))

let ari_exception_order () =
  match string_of_int (boom "l" + boom "r") with
  | s -> s
  | exception Boom tag -> tag

(* {1 [con]: short-circuiting}

   The connective encoding turns [a && b] into
   [let __p = a in if __p <> armed then b else __p]. Disarmed that is
   [if __p then b else __p], which must skip [b] on exactly the inputs
   the original skipped it on, and never evaluate it twice. *)

let con_and a b = if note "l" a && note "r" b then "t" else "f"
let con_or a b = if note "l" a || note "r" b then "t" else "f"

(* {1 [neg]: the condition runs once}

   [neg] binds the condition once and branches on the guard, so a
   condition with an effect must fire exactly as often as before -
   including in a [while], where "as often" is once per iteration plus
   one. *)

let neg_if a = if note "c" a then "t" else "f"

let neg_while limit =
  let i = ref 0 in
  while note "w" (!i < limit) do
    incr i
  done;
  string_of_int !i

let neg_guard a = match a with _ when note "g" a -> "t" | _ -> "f"

(* {1 Laziness}

   [lazy_thunk]'s body carries two [ari] sites, so it is not a trivial
   syntactic value and must stay a real suspension: nothing in it may
   run before the first force, and it must run exactly once however
   often it is forced. [lazy_trivial] is a trivial syntactic value and
   must still compile as already forced.

   [lazy_thunk ()] is a fresh suspension with its effect counter, built
   per call because a forced lazy stays forced for the rest of the
   process: [lazy_witness] then says the same thing however often it
   runs. It is not in [witnesses], which the suite replays under every
   mutant of this file. *)

let lazy_thunk () =
  let effects = ref 0 in
  ( lazy
      (effects := !effects + 1;
       20 + 22),
    effects )

let lazy_trivial = lazy 42

(* A [lazy] whose body is a function: a trivial syntactic value, so the
   compiler builds it already forced and the instrumenter excludes the
   body outright - the [+] inside carries no site. If a guard were ever
   placed there, the [lazy] would become a real suspension and
   [Lazy.is_val] would say so, in this copy and not in the twin. *)
let lazy_fun = lazy (fun x -> x + 1)

let lazy_witness () =
  let thunk, effects = lazy_thunk () in
  let val_before = Lazy.is_val thunk in
  let trivial_val = Lazy.is_val lazy_trivial in
  let fun_val = Lazy.is_val lazy_fun in
  let effects_before = !effects in
  let first = Lazy.force thunk in
  let second = Lazy.force thunk in
  Printf.sprintf
    "thunk_is_val_before=%b trivial_is_val=%b fun_is_val=%b effects=%d->%d \
     forced=%d,%d applied=%d"
    val_before trivial_val fun_val effects_before !effects first second
    (Lazy.force lazy_fun 41)

(* {1 Generalization}

   Placement rule 3 - "no guard on a value spine" - is vacuous today:
   every site of the four operators is an application or a conditional,
   which is never a syntactic value, so a binding that generalizes
   uninstrumented still generalizes instrumented. This binding is where
   that would break first: put a guard anywhere on its spine and it
   weakens to [bool * '_weak1 list]. Nothing here reads it at run time;
   the check is the module coercion in test_mutate_semantics.ml, which compares
   this copy's inferred signature against the twin's and fails to
   compile the day the two disagree. *)
let generalizes = (true, [])

(* {1 Tail position, measured}

   [Printexc.get_callstack] reports the frames live at the moment it is
   called, so the depth reached at the base case of a recursion is a
   direct reading of whether the recursive call was a tail call: constant
   in [n] if it was, linear in [n] if it was not. That is a decision at
   any stack bound, where recursing until the stack overflows decides
   only under a bound small enough for the depth.

   [accumulate] is the positive control: it is deliberately NOT tail
   recursive, so the suite can prove the measurement can tell the two
   apart before trusting it about the four that matter. *)

let stack_depth () =
  Printexc.raw_backtrace_length (Printexc.get_callstack 500_000)

let depth_seen = ref 0

let mark_depth () =
  depth_seen := stack_depth ();
  true

(* [&&]'s right arm, in tail position: the con encoding's whole claim. *)
let rec and_arm n = n >= 0 && if n = 0 then mark_depth () else and_arm (n - 1)

(* [||]'s right arm, in tail position. *)
let rec or_arm n = n < 0 || if n = 0 then mark_depth () else or_arm (n - 1)

(* The same arm in the two other shapes a tail position can take. The
   guard the instrumenter emits is the same whatever the arm looks like -
   [if p <> armed then b else p], with [b] copied once - so what these
   add is the COMPILER's side: [b] moves under a fresh [if], and a [let]
   body and a [match] arm are where that would cost a tail call first.
   ([try] has no third entry: [try e with _ -> _] is not a tail position
   in OCaml to begin with, so there is nothing instrumentation could
   take away.) *)
let rec or_let_arm n =
  n < 0
  ||
  let next = n - 1 in
  if n = 0 then mark_depth () else or_let_arm next

let rec and_match_arm n =
  n >= 0 && match n with 0 -> mark_depth () | _ -> and_match_arm (n - 1)

(* A plain tail call reached through an instrumented [cmp] condition,
   with an instrumented [ari] expression as its argument. *)
let rec countdown n = if n = 0 then mark_depth () else countdown (n - 1)

(* Mutual tail recursion, same shapes. *)
let rec ping n = if n = 0 then mark_depth () else pong (n - 1)
and pong n = if n = 0 then mark_depth () else ping (n - 1)

let sink = ref 0

let rec accumulate n =
  if n = 0 then (
    ignore (mark_depth () : bool);
    0)
  else
    let deeper = accumulate (n - 1) in
    sink := !sink + deeper;
    deeper + 1

let depth_of f n =
  depth_seen := 0;
  ignore (f n : bool);
  !depth_seen

let shallow = 1_000
let deep = 100_000

(* Each entry is [(name, depth at 1_000, depth at 100_000)]; the two
   must be equal, because a tail call leaves no frame behind. *)
let tail_depths () =
  [
    ("&& right arm", depth_of and_arm shallow, depth_of and_arm deep);
    ("|| right arm", depth_of or_arm shallow, depth_of or_arm deep);
    ( "|| right arm that is a let",
      depth_of or_let_arm shallow,
      depth_of or_let_arm deep );
    ( "&& right arm that is a match",
      depth_of and_match_arm shallow,
      depth_of and_match_arm deep );
    ( "if arm through a cmp condition",
      depth_of countdown shallow,
      depth_of countdown deep );
    ("mutual recursion", depth_of ping shallow, depth_of ping deep);
  ]

(* The control: the same measurement on a call that is NOT in tail
   position must grow with the depth, by one frame per level. *)
let control_depths () =
  let at n =
    depth_seen := 0;
    ignore (accumulate n : int);
    !depth_seen
  in
  (at shallow, at deep)

(* {1 The battery}

   Every witness is repeatable: the suite runs the whole list once per
   copy to compare the twins, and then runs it again under each mutant
   of this file in turn, to prove the comparison is not vacuous. A
   witness that could not be replayed would make the second use lie. *)

let witnesses : (string * (unit -> string)) list =
  [
    (* Each ordering is asked at the boundary too. Away from it a [cmp]
       mutant agrees with the original - [a < b] and [a <= b] differ only
       at [a = b] - and a battery that never visits the boundary would
       report the whole family as unobservable, which is the one thing
       that would make the differential below meaningless. *)
    ("cmp < true", fun () -> show (cmp_lt 1 2));
    ("cmp < false", fun () -> show (cmp_lt 2 1));
    ("cmp < at the boundary", fun () -> show (cmp_lt 2 2));
    ("cmp <= at the boundary", fun () -> show (cmp_le 2 2));
    ("cmp > false", fun () -> show (cmp_gt 1 2));
    ("cmp > at the boundary", fun () -> show (cmp_gt 2 2));
    ("cmp >= at the boundary", fun () -> show (cmp_ge 2 2));
    ("cmp = true", fun () -> show (cmp_eq 1 1));
    ("cmp = false", fun () -> show (cmp_eq 1 2));
    ("cmp <> true", fun () -> show (cmp_ne 1 2));
    ("cmp in a while condition", fun () -> show (cmp_while 3));
    ("cmp in a when guard", fun () -> show (cmp_guard 3 2));
    ("cmp under && , left true", fun () -> show (cmp_under_and 1 2 true));
    ("cmp under && , left false", fun () -> show (cmp_under_and 2 1 true));
    ("cmp under || , left false", fun () -> show (cmp_under_or 1 2 true));
    ("cmp under || , left true", fun () -> show (cmp_under_or 2 1 true));
    ("cmp operand exceptions", fun () -> show (cmp_exception_order ()));
    ("cmp under an application", fun () -> show (cmp_in_args 1 2 4 3));
    ("cmp constructor typed by the left operand", fun () -> show (cmp_sibling 0));
    ( "cmp constructor typed by the left operand, at the boundary",
      fun () -> show (cmp_sibling 1) );
    ("ari +", fun () -> show (ari_add 1 2));
    ("ari -", fun () -> show (ari_sub 1 2));
    ("ari +.", fun () -> show (ari_fadd 1.5 2.25));
    ("ari -.", fun () -> show (ari_fsub 1.5 2.25));
    ("ari chain of one family", fun () -> show (ari_chain 1 2 3));
    ("ari chain in parentheses", fun () -> show (ari_chain_parens 1 2 3));
    ("ari right-nested", fun () -> show (ari_right_nested 1 2 3));
    ("ari two families", fun () -> show (ari_mixed 1 2 3));
    ("ari under an application", fun () -> show (ari_in_args 1 2 3 4));
    ("ari operand exceptions", fun () -> show (ari_exception_order ()));
    ("con && skips b on false", fun () -> show (con_and false true));
    ("con && runs b on true", fun () -> show (con_and true false));
    ("con && both true", fun () -> show (con_and true true));
    ("con || skips b on true", fun () -> show (con_or true false));
    ("con || runs b on false", fun () -> show (con_or false true));
    ("con || both false", fun () -> show (con_or false false));
    ("neg in an if condition, true", fun () -> show (neg_if true));
    ("neg in an if condition, false", fun () -> show (neg_if false));
    ("neg in a while condition", fun () -> show (neg_while 3));
    ("neg in a when guard", fun () -> show (neg_guard true));
  ]
