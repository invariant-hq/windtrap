(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Instrumented by ppx_windtrap.coverage (see dune): every Law-14 form in
   this file carries visit calls when the semantics tests run. Each fixture
   makes one observable promise the instrumentation must not disturb
   (Law 13): tail calls stay tail calls - including through the out-edge
   machinery, whose post-visit wrapping is exactly what could break them -
   evaluation order is untouched, lazy stays lazy, trivial lazy stays a
   value, raising calls raise as before, and results are the ones an
   uninstrumented build computes. *)

(* Deep tail recursion through a match arm: the entry visit is a sequence
   prefix and the tail call must carry no out-edge wrap - a post-visit
   wrapper would overflow the stack at the depths the test uses. *)
let rec countdown n = match n with 0 -> "done" | n -> countdown (n - 1)

(* Mutual tail recursion through if branches. *)
let rec even n = if n = 0 then true else odd (n - 1)
and odd n = if n = 0 then false else even (n - 1)

(* CPS: the continuation chain builds through closures and unwinds through
   tail calls - both the recursive call and every [k] application must
   stay in tail position. *)
let rec cps_count n k =
  if n = 0 then k 0 else cps_count (n - 1) (fun acc -> k (acc + 1))

(* A pipeline in tail position: the final [|>] application must stay a
   tail call (inner stages are arguments, not tail calls). *)
let rec pipe_down n = if n = 0 then 0 else n - 1 |> pipe_down

(* [||] and [&&] with a recursive call as the right arm, in tail position:
   [||]'s donor guard gives the arm's point up rather than its tail call,
   and [&&]'s entry-sequenced right arm keeps its tail position. *)
let rec any_odd n = if n <= 0 then false else n mod 2 = 1 || any_odd (n - 2)

(* [||] right arms that are not bare applications. Each of these shapes
   inherits tail position in its own sub-expressions, so the recursive call
   inside is a tail call — and the arm used to be demoted to an [if]
   condition, which traversed it out of tail position and post-wrapped the
   call. Instrumented, these overflowed at the depths below. *)
let rec or_let n =
  n = 0
  ||
  let next = n - 1 in
  or_let next

let rec or_match n = n = 0 || match n with k -> or_match (k - 1)
let rec or_if n = n = 0 || if n > 0 then or_if (n - 1) else false
let rec or_try n = n = 0 || try or_try (n - 1) with Not_found -> false

(* [[@tail_mod_cons]]: the recursive call sits in a constructor argument of
   a tail expression, which is where TMC rewrites it. Out-edge wrapping
   there leaves the function with no TMC-able call — warning 71, fatal
   under stock dune, so an instrumented build of this file would not
   compile at all — or, with the warning disabled, silently turns the
   function stack-consuming. *)
let[@tail_mod_cons] rec tmc_map f = function
  | [] -> []
  | x :: xs -> f x :: tmc_map f xs

let rec all_even n =
  if n < 0 then false
  else if n = 0 then true
  else n mod 2 = 0 && all_even (n - 2)

(* Evaluation-order witness: [note] logs a tag and returns its value; the
   caller checks the tag order across an instrumented if, scrutinee, guard,
   and arm. *)
let log : string list ref = ref []

let note tag value =
  log := tag :: !log;
  value

let order_witness b =
  log := [];
  let x = if note "cond" b then note "then" 1 else note "else" 2 in
  let y =
    match note "scrutinee" x with
    | 1 when note "guard" true -> note "arm1" 10
    | _ -> note "arm2" 20
  in
  (y, List.rev !log)

(* Short-circuit order: the [||]/[&&] desugaring rebuilds the operands
   into marked conditionals - the left arm must still run first and the
   right arm only when the left one did not decide. *)
let or_trace a b =
  log := [];
  let r = note "left" a || note "right" b in
  (r, List.rev !log)

let and_trace a b =
  log := [];
  let r = note "left" a && note "right" b in
  (r, List.rev !log)

(* Argument evaluation order: out-edge wrapping of the argument calls must
   not reorder them - the test compares the trace against an
   uninstrumented baseline of the same shape. *)
let two_args a b = (a, b)

let arg_order () =
  log := [];
  let r = two_args (note "first" 1) (note "second" 2) in
  (r, List.rev !log)

(* Sequence order: statement out-edges are attributed to their successor;
   the statements must still run left to right, exactly once. *)
let seq_order () =
  log := [];
  note "one" ();
  note "two" ();
  List.rev !log

(* Laziness: [thunk]'s effect must not run until the caller forces it, and
   [trivial] must still compile as an already-forced value (the
   trivial-syntactic-value guard). *)
let forced = ref false
let force_count = ref 0

(* Trivial operators only ([:=], [!], [+]): the body is exactly one
   point, the lazy-body entry - the force test pins its delta at 1. *)
let thunk =
  lazy
    (forced := true;
     force_count := !force_count + 1;
     41 + 1)

let trivial = lazy 42

(* Exception-raising applications: the out-edge of [f ()] fires only when
   [f ()] returns. [tap_ok] and [tap_raise] are shape-identical, so their
   point populations match; the test pins that the raising path visits
   exactly one point fewer - the out-edge. *)
let ret_unit () = ()
let raise_unit () = raise Exit

let tap_ok f =
  let () = f () in
  true

let tap_raise f =
  let () = f () in
  true

(* Pipelines compute what they computed uninstrumented. *)
let double x = x * 2
let pipeline x = x |> double |> double

let pipeline_bound x =
  let y = x |> double in
  y + 1

(* Method calls: class bodies are entry points and sends are out-edges;
   the object's behavior must be untouched. *)
class adder =
  object
    val mutable total = 0
    method add x = total <- total + x
    method total = total
  end

let sum_object xs =
  let a = new adder in
  List.iter (fun x -> a#add x) xs;
  a#total

(* A send with a successor: the out-edge of [a#total] is attributed to the
   binding's body. *)
let poke (a : adder) =
  let v = a#total in
  v + 1

(* Loops, letops, guards, and exception arms compute known values. *)
let sum_while n =
  let total = ref 0 and i = ref 1 in
  while !i <= n do
    total := !total + !i;
    incr i
  done;
  !total

let sum_for n =
  let total = ref 0 in
  for i = 1 to n do
    total := !total + i
  done;
  !total

let ( let* ) x f = f x

let letop_sum a b =
  let* x = a in
  let* y = b in
  x + y

let bucket n =
  match n with
  | n when n < 10 -> "small"
  | n when n < 100 -> "medium"
  | _ -> "large"

let safe_div a b = try a / b with Division_by_zero -> 0

(* An arm that is itself a function: selecting the arm is one point (the
   closure allocates), applying the result enters the leaf body - two
   distinct points, visited at two distinct moments. The caller checks
   the visit deltas at each step. *)
let dispatch = function `Add -> fun a b -> a + b | `Sub -> fun a b -> a - b
