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
  if fails choices then Shrink.Fails { parsed = case choices; keep = ignore }
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
          equal (pair choices string) ([| 100L |], "converged")
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
          equal (pair int string) (3, "spent")
            (!runs, match ending with Converged -> "converged" | _ -> "spent"));
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
          equal (triple int int string) (1, steps, "spent")
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
  match Property.run ~count:(`Declared 10_000) ~root ~path:b.name b.gen law with
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
    (List.fold_left max 0 sizes)
    (median runs)
    (List.fold_left max 0 runs)

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
      Gen.bind
        (Gen.option (step state))
        (function
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
    minimum = (2 * capacity) + 2;
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
    bound5, minimum 2: 36% there, worst 20, 157 runs (max 3508)
    large union list, minimum 6: 100% there, worst 6, 141 runs (max 292)
    nested lists, minimum 12: 100% there, worst 12, 102 runs (max 316)
    reverse, minimum 2: 100% there, worst 2, 32 runs (max 256)
    calculator, minimum 5: 100% there, worst 5, 87 runs (max 200)
    queue, capacity 2, minimum 6: 100% there, worst 6, 98 runs (max 118)
    queue, capacity 3, minimum 8: 100% there, worst 8, 135 runs (max 158)
    key-value, minimum 4: 100% there, worst 4, 86 runs (max 253)
    |}

(* Stateful programs *)

(* The cases of _autoresearch/audits/spike-choice-stateful.md and
   gap-stateful-refs.md, each system made by a [create] call, so the known
   minima are shrinking.md's plus one. Their calls are cheap, so a law run
   costs 1. *)

module Stateful = Windtrap.Private.Stateful

let abstract = Stateful.abstract
let command = Stateful.command
let ( @-> ) = Stateful.( @-> )
let ( ^-> ) = Stateful.( ^-> )
let returns = Stateful.returns
let makes = Stateful.makes

type program = {
  name : string;
  commands : unit -> Stateful.command list;
  minimum : int;
  at_minimum : string -> bool; (* of the counterexample's record *)
}

(* The ring buffer of [queue], against a list, as two abstract values. *)
let ring capacity () =
  let q = abstract "q" in
  let create () = (Array.make capacity 0, ref 0, ref 0) in
  let push (buffer, head, length) x =
    if !length = capacity then false
    else begin
      buffer.((!head + !length) mod capacity) <- x;
      incr length;
      true
    end
  in
  let pop (buffer, head, length) () =
    if !length = 0 then None
    else begin
      let x = buffer.(!head) in
      head := if !head = capacity - 1 then 1 else !head + 1;
      decr length;
      Some x
    end
  in
  let model_push m x =
    if List.length !m = capacity then false
    else begin
      m := !m @ [ x ];
      true
    end
  in
  let model_pop m () =
    match !m with
    | [] -> None
    | x :: rest ->
        m := rest;
        Some x
  in
  [
    command "create" (Gen.unit @-> makes q) (fun () -> ref []) create;
    command "push" (q ^-> Gen.nat @-> returns bool) model_push push;
    command "pop" (q ^-> Gen.unit @-> returns (option int)) model_pop pop;
    command "size"
      (q ^-> returns int)
      (fun m -> List.length !m)
      (fun (_, _, length) -> !length);
  ]

(* The store of [key_value], whose put takes the number of live entries as
   its fresh key. *)
let store () =
  let s = abstract "s" and k = abstract "k" in
  let put store v =
    let key = Hashtbl.length store in
    Hashtbl.replace store key v;
    (store, key)
  in
  let model_put model v =
    let key = Hashtbl.length model in
    Hashtbl.replace model key (Some v);
    (model, key)
  in
  [
    command "create"
      (Gen.unit @-> makes s)
      (fun () -> Hashtbl.create 8)
      (fun () -> Hashtbl.create 8);
    command "put" (s ^-> Gen.nat @-> makes k) model_put put;
    command "get"
      (k ^-> returns (option int))
      (fun (m, key) -> Hashtbl.find m key)
      (fun (s, key) -> Hashtbl.find_opt s key);
    command "delete"
      (k ^-> returns unit)
      (fun (m, key) -> Hashtbl.replace m key None)
      (fun (s, key) -> Hashtbl.remove s key);
  ]

(* A value lists [0 … 999], and [get] is wrong from index 500 on. *)
let element () =
  let d = abstract "d" in
  let index = Stateful.among int d Fun.id in
  [
    command "create"
      (Gen.unit @-> makes d)
      (fun () -> List.init 1000 Fun.id)
      ignore;
    command "get"
      (d ^-> index ^-> returns int)
      (fun _ i -> i)
      (fun () i -> if i >= 500 then i + 1 else i);
    command "size" (d ^-> returns int) List.length (fun () -> 1000);
  ]

(* The guide's set, whose [union] drops its first argument's elements when
   the second is not empty. *)
module Ints = Set.Make (Int)

let set () =
  let s = abstract "s" in
  let elt = Gen.int_range 0 15 in
  let rec add x = function
    | [] -> [ x ]
    | y :: rest as l ->
        if x = y then l else if x < y then x :: l else y :: add x rest
  in
  [
    command "empty" (Gen.unit @-> makes s) (fun () -> Ints.empty) (fun () -> []);
    command "add" (elt @-> s ^-> makes s) Ints.add add;
    command "union"
      (s ^-> s ^-> makes s)
      Ints.union
      (fun a b -> if b = [] then a else b);
    command "mem" (elt @-> s ^-> returns bool) Ints.mem List.mem;
    command "elements" (s ^-> returns (list int)) Ints.elements Fun.id;
  ]

(* Accounts in a bank, whose [transfer] moves the whole balance of its first
   account to its second, and forgets what the second held. *)
let bank () =
  let b = abstract "b" and a = abstract "a" in
  let open_ bank =
    let id = Hashtbl.length bank in
    Hashtbl.replace bank id 0;
    (bank, id)
  in
  let deposit (bank, a) n = Hashtbl.replace bank a (Hashtbl.find bank a + n) in
  let transfer ~forgets (bank, a) (bank', b) =
    if bank == bank' then begin
      let x = Hashtbl.find bank a in
      Hashtbl.replace bank a 0;
      Hashtbl.replace bank b (if forgets then x else Hashtbl.find bank b + x)
    end
  in
  [
    command "create"
      (Gen.unit @-> makes b)
      (fun () -> Hashtbl.create 8)
      (fun () -> Hashtbl.create 8);
    command "open" (b ^-> makes a) open_ open_;
    command "deposit" (a ^-> Gen.nat @-> returns unit) deposit deposit;
    command "transfer"
      (a ^-> a ^-> returns unit)
      (transfer ~forgets:false) (transfer ~forgets:true);
    command "balance"
      (a ^-> returns int)
      (fun (bank, a) -> Hashtbl.find bank a)
      (fun (bank, a) -> Hashtbl.find bank a);
  ]

(* Union-find whose [union] links its first element, not that element's
   root, under the second's root. *)
let union_find () =
  let f = abstract "f" and e = abstract "e" in
  let make table =
    let id = Hashtbl.length table in
    Hashtbl.replace table id id;
    (table, id)
  in
  let rec find table x =
    let parent = Hashtbl.find table x in
    if parent = x then x else find table parent
  in
  let union (table, a) (table', b) =
    if table == table' then Hashtbl.replace table a (find table b)
  in
  let model_union (classes, a) (classes', b) =
    if classes == classes' then begin
      let ca = Hashtbl.find classes a and cb = Hashtbl.find classes b in
      Hashtbl.filter_map_inplace
        (fun _ c -> Some (if c = ca then cb else c))
        classes
    end
  in
  let same lookup (t, a) (t', b) = t == t' && lookup t a = lookup t b in
  [
    command "create"
      (Gen.unit @-> makes f)
      (fun () -> Hashtbl.create 8)
      (fun () -> Hashtbl.create 8);
    command "make" (f ^-> makes e) make make;
    command "union" (e ^-> e ^-> returns unit) model_union union;
    command "same" (e ^-> e ^-> returns bool) (same Hashtbl.find) (same find);
  ]

let calls record = List.length (String.split_on_char '\n' record) - 1
let sized minimum record = calls record = minimum

let queue_2 =
  {
    name = "queue, capacity 2";
    commands = ring 2;
    minimum = 7;
    at_minimum = sized 7;
  }

let queue_3 =
  {
    name = "queue, capacity 3";
    commands = ring 3;
    minimum = 9;
    at_minimum = sized 9;
  }

let key_value_store =
  { name = "key-value"; commands = store; minimum = 5; at_minimum = sized 5 }

let element_500 =
  {
    name = "element 500";
    commands = element;
    minimum = 2;
    at_minimum =
      (fun record ->
        sized 2 record && String.ends_with ~suffix:"get d1 500" record);
  }

let guide_set =
  {
    name = "the guide's set";
    commands = set;
    minimum = 5;
    at_minimum = sized 5;
  }

let bank_transfer =
  { name = "bank"; commands = bank; minimum = 6; at_minimum = sized 6 }

let union_find_program =
  {
    name = "union-find";
    commands = union_find;
    minimum = 7;
    at_minimum = sized 7;
  }

(* The record of the counterexample that seed [seed] finds, the law runs
   after the first failure, the run again excluded, and whether its token,
   replayed alone, fails with the same record. *)
let run_program p seed =
  let runs = ref 0 and first = ref 0 in
  let law _ program =
    incr runs;
    match Stateful.execute program with
    | (_ : Stateful.record) -> ()
    | exception e ->
        if !first = 0 then first := !runs;
        raise e
  in
  let check ?replay ~root law =
    Property.run ?replay ~count:(`Declared 10_000) ~root ~path:"bench"
      (Stateful.program (p.commands ()))
      law
  in
  (* The counterexample's record, its token and whether it failed again. *)
  let found = function
    | Property.Fail { failure = { Failure.kind = Property f; _ }; _ } ->
        Some (f.rendered.kept, f.token, f.failed_again = Some true)
    | _ -> None
  in
  let root = Int64.of_int ((seed * 7919) + 13) in
  Option.map
    (fun (record, token, again) ->
      let runs = !runs - !first - if again then 1 else 0 in
      let replays =
        match
          Option.bind token (fun replay -> found (check ~replay ~root:0L law))
        with
        | Some (replayed, _, _) -> replayed = record
        | None -> false
      in
      (record, runs, replays))
    (found (check ~root law))

let program_row p =
  let found = List.filter_map (run_program p) (List.init 100 succ) in
  let sizes = List.map (fun (record, _, _) -> calls record) found in
  let runs = List.map (fun (_, runs, _) -> runs) found in
  let at_minimum =
    List.length (List.filter (fun (record, _, _) -> p.at_minimum record) found)
  in
  let replayed = List.length (List.filter (fun (_, _, r) -> r) found) in
  strf
    "%s, minimum %d: %d%% there%s, mean %.2f, worst %d, %d runs (max %d), %d \
     tokens replay"
    p.name p.minimum
    (100 * at_minimum / List.length found)
    (if List.length found = 100 then ""
     else strf " of %d found" (List.length found))
    (Float.of_int (List.fold_left ( + ) 0 sizes)
    /. Float.of_int (List.length found))
    (List.fold_left max 0 sizes)
    (median runs)
    (List.fold_left max 0 runs)
    replayed

(* Each program pins its own row, so that the rows run in parallel under
   the stanza's [-j]. gap-stateful-refs.md's reducer reached the same on
   every case, at most six runs apart, but the guide's set: 88%, mean 5.12.
   Here every generator call is a span, an argument of one choice included,
   and deleting such a span leads four more seeds to six calls. *)
let pinned p row = test p.name (fun () -> expect (program_row p) row)

let programs =
  group "stateful programs reach the spikes' minima"
    [
      pinned queue_2
      @@ __POS_OF__
           {| queue, capacity 2, minimum 7: 100% there, mean 7.00, worst 7, 178 runs (max 188), 100 tokens replay |};
      pinned queue_3
      @@ __POS_OF__
           {| queue, capacity 3, minimum 9: 100% there, mean 9.00, worst 9, 229 runs (max 239), 100 tokens replay |};
      pinned key_value_store
      @@ __POS_OF__
           {| key-value, minimum 5: 100% there, mean 5.00, worst 5, 131 runs (max 163), 100 tokens replay |};
      pinned element_500
      @@ __POS_OF__
           {| element 500, minimum 2: 100% there, mean 2.00, worst 2, 105 runs (max 106), 100 tokens replay |};
      pinned guide_set
      @@ __POS_OF__
           {| the guide's set, minimum 5: 84% there, mean 5.16, worst 6, 125 runs (max 179), 100 tokens replay |};
      pinned bank_transfer
      @@ __POS_OF__
           {| bank, minimum 6: 94% there, mean 6.06, worst 7, 147 runs (max 195), 100 tokens replay |};
      pinned union_find_program
      @@ __POS_OF__
           {| union-find, minimum 7: 100% there, mean 7.00, worst 7, 114 runs (max 327), 100 tokens replay |};
    ]

let () =
  exit
    (run "shrink"
       [
         reducer;
         test "the benchmarks of shrinking.md reach the prototype's minima"
           table;
         programs;
       ])
