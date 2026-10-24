(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(* The reducer, alone over hand-written tests of choice sequences, then the
   eight benchmarks of _autoresearch/audits/shrinking.md through the real
   property engine: every seed is fixed, so their table is a baseline. *)

open Windtrap
module Choice = Windtrap.Private.Choice
module Shrink = Windtrap.Private.Shrink
module Property = Windtrap.Private.Property
module Failure = Windtrap.Private.Failure

let strf = Printf.sprintf
let choices = Testable.(array int64)
let case ?(spans = []) ?(dependents = []) choices : Shrink.case =
  { choices; spans; dependents }

(* A test that fails when [fails] holds of the choices, reading them all. *)
let failing fails choices =
  if fails choices then
    Shrink.Fails { parsed = case choices; keep = ignore }
  else Shrink.Passes

let shrunk ?(budget = 10_000) ?(cost = 1) fails start =
  let best, _, ending = Shrink.shrink ~budget ~cost (failing fails) start in
  (best.choices, ending)

let sum = Array.fold_left Int64.add 0L

let reducer =
  group "The reducer"
    [
      test "shortlex orders by length, then unsigned at the first difference"
        (fun () ->
          equal (list int) [ -1; 1; -1; 0 ]
            (List.map
               (fun (a, b) -> Int.compare (Shrink.shortlex a b) 0)
               [
                 ([| 9L |], [| 0L; 0L |]);
                 ([| 1L; 0L |], [| 0L; 9L |]);
                 ([| 1L |], [| -1L |]);
                 ([| 3L |], [| 3L |]);
               ]));
      test "a threshold on a sum shrinks to one choice at the threshold"
        (fun () ->
          equal
            (pair choices string)
            ([| 100L |], "converged")
            (let best, ending =
               shrunk
                 (fun c -> Int64.compare (sum c) 100L >= 0)
                 (case [| 7L; 300L; 12L; 5_000L |])
             in
             (best, match ending with Converged -> "converged" | _ -> "spent")));
      test "a span is deleted whole" (fun () ->
          (* Two choices that only fail together, inside one span. *)
          let fails c = Array.length c >= 2 && c.(0) = 1L && c.(1) = 1L in
          let spans = [ { Choice.start = 2; stop = 4 } ] in
          equal choices [| 1L; 1L |]
            (fst (shrunk fails (case ~spans [| 1L; 1L; 5L; 5L |]))));
      test
        "a deletion lowers a dependent by the positions it counts past inside"
        (fun () ->
          (* The last choice counts past the two before it: deleting one of
             them lowers it by one, both by two, and the dependent itself
             nothing. *)
          let start =
            case
              ~dependents:[ { Choice.at = 2; over = [ 1; 0 ] } ]
              [| 7L; 8L; 2L |]
          in
          let deletions = List.of_seq (Seq.take 5 (Shrink.candidates start)) in
          equal (list choices)
            [ [| 7L |]; [| 0L |]; [| 7L; 8L |]; [| 7L; 1L |]; [| 8L; 1L |] ]
            deletions);
      test "every test spends the cost, a discard one, a repeat nothing"
        (fun () ->
          let runs = ref 0 in
          let test c =
            incr runs;
            if Array.length c = 1 then Shrink.Discards
            else failing (fun c -> sum c > 0L) c
          in
          let _, _, ending =
            Shrink.shrink ~budget:7 ~cost:3 test (case [| 1L; 1L |])
          in
          equal
            (pair int string)
            (3, "spent")
            ( !runs,
              match ending with Converged -> "converged" | _ -> "spent" ));
      test "a search that spends the budget ends at the next candidate"
        (fun () ->
          let runs = ref 0 in
          let test c =
            incr runs;
            failing (fun c -> sum c > 0L) c
          in
          let best, steps, ending =
            Shrink.shrink ~budget:1 ~cost:1 test (case [| 5L; 5L |])
          in
          equal
            (triple int int string)
            (1, steps, "spent")
            ( !runs,
              (if best.choices = [| 5L; 5L |] then 0 else 1),
              match ending with Converged -> "converged" | _ -> "spent" ));
    ]

(* The benchmarks *)

type 'v bench = {
  name : string;
  gen : 'v Gen.t;
  holds : 'v -> bool;
  size : 'v -> int;
  minimum : int;
}

type any = Bench : 'v bench -> any

(* The final size and the law runs after the first failure, the last run on
   the counterexample excluded, of the case that seed [seed] finds. *)
let measure (type v) (b : v bench) seed =
  let runs = ref 0 and first = ref 0 and last = ref None in
  let law _ (v : v) =
    incr runs;
    if not (b.holds v) then begin
      if !first = 0 then first := !runs;
      last := Some v;
      failwith "counterexample"
    end
  in
  let root = Int64.of_int ((seed * 7919) + 13) in
  match
    Property.run ~count:(`Declared 10_000) ~root ~path:b.name b.gen law
  with
  | Fail { failure = { Failure.kind = Property p; _ }; _ } ->
      let again = if p.failed_again = Some true then 1 else 0 in
      let final = Option.get !last in
      Some (b.size final, !runs - !first - again)
  | _ -> None

let median values =
  let sorted = Array.of_list (List.sort Int.compare values) in
  sorted.(Array.length sorted / 2)

let row (Bench b) =
  let found = List.filter_map (measure b) (List.init 100 succ) in
  let sizes = List.map fst found and runs = List.map snd found in
  let at_minimum = List.length (List.filter (( = ) b.minimum) sizes) in
  strf "%s, minimum %d: %d%% there%s, worst %d, %d runs (max %d)" b.name
    b.minimum
    (100 * at_minimum / List.length found)
    (if List.length found = 100 then ""
     else strf " of %d found" (List.length found))
    (List.fold_left max 0 sizes) (median runs) (List.fold_left max 0 runs)

(* bound5: five lists of 16-bit integers, each summing below 256, whose
   total in 16 bits stays below 5 * 256. Overflow breaks it. *)
let i16 x = ((x + 32768) land 0xFFFF) - 32768
let sum16 = List.fold_left (fun a x -> i16 (a + x)) 0

let bound5 =
  let l = Gen.(list (int_range (-32768) 32767)) in
  {
    name = "bound5";
    gen =
      Gen.(
        map
          (fun (a, (b, (c, (d, e)))) -> [ a; b; c; d; e ])
          (pair l (pair l (pair l (pair l l)))));
    holds =
      (fun ls ->
        (not (List.for_all (fun l -> sum16 l < 256) ls))
        || sum16 (List.map sum16 ls) < 5 * 256);
    size = (fun ls -> List.length (List.concat ls));
    minimum = 2;
  }

let lists_size ls = List.length ls + List.length (List.concat ls)

let union =
  {
    name = "large union list";
    gen = Gen.(list (list small_int));
    holds =
      (fun ls -> List.length (List.sort_uniq compare (List.concat ls)) <= 4);
    size = lists_size;
    minimum = 6;
  }

let nested =
  {
    name = "nested lists";
    gen = Gen.(list (list nat));
    holds = (fun ls -> List.length (List.concat ls) <= 10);
    size = lists_size;
    minimum = 12;
  }

let reverse =
  {
    name = "reverse";
    gen = Gen.(list small_int);
    holds = (fun l -> List.rev l = l);
    size = List.length;
    minimum = 2;
  }

(* An expression with no literal division by zero evaluates without
   raising. *)
type expr = Int of int | Add of expr * expr | Div of expr * expr

let rec eval = function
  | Int n -> n
  | Add (a, b) -> eval a + eval b
  | Div (a, b) -> eval a / eval b

let rec no_zero_literal = function
  | Int _ -> true
  | Div (_, Int 0) -> false
  | Add (a, b) | Div (a, b) -> no_zero_literal a && no_zero_literal b

let rec nodes = function
  | Int _ -> 1
  | Add (a, b) | Div (a, b) -> 1 + nodes a + nodes b

let rec expr depth =
  let leaf = Gen.map (fun n -> Int n) Gen.small_int in
  if depth = 0 then leaf
  else
    let sub = Gen.pair (expr (depth - 1)) (expr (depth - 1)) in
    Gen.one_of
      [
        leaf;
        Gen.map (fun (a, b) -> Add (a, b)) sub;
        Gen.map (fun (a, b) -> Div (a, b)) sub;
      ]

let calculator =
  {
    name = "calculator";
    gen = expr 4;
    holds =
      (fun e ->
        (not (no_zero_literal e))
        || match eval e with _ -> true | exception Division_by_zero -> false);
    size = nodes;
    minimum = 5;
  }

(* A program drawn call by call with [bind] from the state so far, ended by
   [None]: each call can depend on what the calls before it made. *)
let chain step init =
  let rec calls k state =
    if k = 20 then Gen.constant []
    else
      Gen.bind (Gen.option (step state)) (function
        | None -> Gen.constant []
        | Some (call, state) ->
            Gen.map (fun rest -> call :: rest) (calls (k + 1) state))
  in
  calls 0 init

(* A ring buffer whose pop wraps the head to 1 instead of 0, against a list. *)
type queue_call = Push of int | Pop | Size

let queue capacity =
  let fails program =
    let buffer = Array.make capacity 0 and head = ref 0 and length = ref 0 in
    let model = ref [] in
    let step = function
      | Push x ->
          let pushed =
            if !length = capacity then false
            else begin
              buffer.((!head + !length) mod capacity) <- x;
              incr length;
              true
            end
          in
          let modelled =
            if List.length !model = capacity then false
            else begin
              model := !model @ [ x ];
              true
            end
          in
          pushed <> modelled
      | Pop ->
          let popped =
            if !length = 0 then None
            else begin
              let x = buffer.(!head) in
              head := if !head = capacity - 1 then 1 else !head + 1;
              decr length;
              Some x
            end
          in
          let modelled =
            match !model with
            | [] -> None
            | x :: rest ->
                model := rest;
                Some x
          in
          popped <> modelled
      | Size -> !length <> List.length !model
    in
    List.exists step program
  in
  let call =
    Gen.(one_of [ map (fun x -> Push x) nat; constant Pop; constant Size ])
  in
  {
    name = strf "queue, capacity %d" capacity;
    gen = chain (fun () -> Gen.map (fun c -> (c, ())) call) ();
    holds = (fun p -> not (fails p));
    size = List.length;
    minimum = 2 * capacity + 2;
  }

(* A store whose put takes the number of live entries as its fresh key, so
   it reuses a key after a delete. A key is named newest first. *)
type store_call = Put of int | Get of int | Delete of int

let key_value =
  let fails program =
    let store = Hashtbl.create 8 and model = Hashtbl.create 8 in
    let keys = ref [||] in
    let key j = !keys.(Array.length !keys - 1 - j) in
    let step = function
      | Put v ->
          let k = Hashtbl.length store in
          Hashtbl.replace store k v;
          Hashtbl.replace model (Array.length !keys) (Some v);
          keys := Array.append !keys [| k |];
          false
      | Get j ->
          Hashtbl.find_opt store (key j)
          <> Hashtbl.find model (Array.length !keys - 1 - j)
      | Delete j ->
          Hashtbl.remove store (key j);
          Hashtbl.replace model (Array.length !keys - 1 - j) None;
          false
    in
    List.exists step program
  in
  let call puts =
    if puts = 0 then Gen.map (fun v -> (Put v, 1)) Gen.nat
    else
      Gen.one_of
        [
          Gen.map (fun v -> (Put v, puts + 1)) Gen.nat;
          Gen.map (fun j -> (Get j, puts)) (Gen.int_range 0 (puts - 1));
          Gen.map (fun j -> (Delete j, puts)) (Gen.int_range 0 (puts - 1));
        ]
  in
  {
    name = "key-value";
    gen = chain call 0;
    holds = (fun p -> not (fails p));
    size = List.length;
    minimum = 4;
  }

let benchmarks =
  [
    Bench bound5;
    Bench union;
    Bench nested;
    Bench reverse;
    Bench calculator;
    Bench (queue 2);
    Bench (queue 3);
    Bench key_value;
  ]

(* shrinking.md's prototype, with these three passes, reached the minimum on
   37% of bound5's seeds and on all the others' (its reverse and queue of
   capacity 3 ran all six passes). *)
let table () =
  expect (String.concat "\n" (List.map row benchmarks))
  @@ __POS_OF__
       {|
    bound5, minimum 2: 33% there, worst 11, 192 runs (max 1664)
    large union list, minimum 6: 100% there, worst 6, 144 runs (max 263)
    nested lists, minimum 12: 100% there, worst 12, 103 runs (max 180)
    reverse, minimum 2: 100% there, worst 2, 31 runs (max 67)
    calculator, minimum 5: 100% there, worst 5, 93 runs (max 175)
    queue, capacity 2, minimum 6: 100% there, worst 6, 98 runs (max 116)
    queue, capacity 3, minimum 8: 100% there, worst 8, 136 runs (max 158)
    key-value, minimum 4: 100% there, worst 4, 86 runs (max 238)
    |}

let () =
  exit
    (run "shrink"
       [
         reducer;
         test "the benchmarks of shrinking.md reach the prototype's minima" table;
       ])
