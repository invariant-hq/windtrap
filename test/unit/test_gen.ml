(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(* Every generator under test is built inside a test body, rows hold it as a
   function: a constructor computes its origin and its printer when it is
   applied, and a mutant of gen.ml is armed only inside a test.

   [state i] is the state that [Property.run] draws case [i] from under [root]
   and [path], so the search that [shrink] runs descends from the tree that
   [sample gen i] is. *)

open Windtrap
module Seed = Windtrap.Private.Seed
module Gen_engine = Windtrap.Private.Gen_engine
module Shrink_tree = Gen_engine.Shrink_tree
module Property = Windtrap.Private.Property
module Failure = Windtrap.Private.Failure
module Pp = Windtrap.Private.Pp

let strf = Printf.sprintf
let root = 0x00c0ffee1234abcdL
let path = "test_gen"
let state index = Seed.make (Seed.derive ~root ~path ~index)
let sample gen index = Gen_engine.sample gen (state index)
let at_seed gen seed = Gen_engine.sample gen (Seed.make seed)
let value tree = Gen_engine.value (Shrink_tree.root tree)
let samples gen count = List.init count (fun index -> value (sample gen index))
let candidates tree = List.of_seq (Shrink_tree.children tree)
let candidate_values tree = List.map value (candidates tree)

let first_candidate tree =
  fst (require_some ~msg:"a candidate" (Seq.uncons (Shrink_tree.children tree)))

let least values = List.fold_left min (List.hd values) values
let greatest values = List.fold_left max (List.hd values) values

let share values p =
  float_of_int (List.length (List.filter p values))
  /. float_of_int (List.length values)

let rec words n state =
  if n = 0 then []
  else
    let w, state = Seed.bits64 state in
    w :: words (n - 1) state

let successor gen state = snd (Gen_engine.run gen state)
let placeholder = "<no printer: attach one with Gen.with_pp>"
let pp_n ppf n = Format.fprintf ppf "N=%d" n
let ints values = String.concat "; " (List.map string_of_int values)

(* A pre-image is marked as the report marks it, so an assertion on a value
   cannot pass on one. *)
let shown tree =
  match Gen_engine.render (Shrink_tree.root tree) with
  | Value text -> text
  | Pre_image text -> "from " ^ text

let find_index gen accept =
  let accepted index = if accept (sample gen index) then Some index else None in
  require_some ~msg:"a state within 10000 meets the premise"
    (Seq.find_map accepted (Seq.init 10_000 Fun.id))

let find gen accept =
  sample gen (find_index gen (fun tree -> accept (value tree)))

(* [project] of each node of [tree], depth first, the first [limit] of them. *)
let visit ?(limit = 200) project tree =
  let seen = ref [] and count = ref 0 in
  let rec visit tree =
    if !count = limit then raise_notrace Exit;
    incr count;
    seen := project tree :: !seen;
    Seq.iter visit (Shrink_tree.children tree)
  in
  (try visit tree with Exit -> ());
  List.rev !seen

let nodes ?limit tree = visit ?limit value tree

(* The (parent, candidate) pairs of [tree], depth first, the first [limit]. *)
let edges ?(limit = 200) tree =
  let seen = ref [] and count = ref 0 in
  let rec visit tree =
    Seq.iter
      (fun child ->
        if !count = limit then raise_notrace Exit;
        incr count;
        seen := (value tree, value child) :: !seen;
        visit child)
      (Shrink_tree.children tree)
  in
  (try visit tree with Exit -> ());
  List.rev !seen

let counterexample = function
  | Property.Fail { failure = { Failure.kind = Property p; _ }; _ } ->
      let text =
        match p.rendering with
        | Value -> p.rendered.kept
        | Pre_image -> "from " ^ p.rendered.kept
      in
      let text =
        match p.shrink_end with
        | Converged -> text
        | Budget_spent | Candidate_raised _ | Timed_out _ ->
            text ^ " (not minimal)"
      in
      Some (text, p.shrink_steps)
  | _ -> None

(* [shrink ?from ?failing gen] is the counterexample and the steps of the
   search a property gets: its law fails on the first case whose value
   satisfies [from] and [failing], then on every candidate that satisfies
   [failing]. *)
let shrink ?(from = fun _ -> true) ?(failing = fun _ -> true) gen =
  let started = ref false in
  let law _ v =
    if (!started || from v) && failing v then begin
      started := true;
      fail "the predicate holds"
    end
  in
  require_match ~msg:"a case in 10000 meets the premise" counterexample
    (Property.run ~count:(`Declared 10_000) ~root ~path gen law)

let shrinks_to ?from ?failing gen = fst (shrink ?from ?failing gen)

(* A discard and the exceptions [Failure.catch] never returns escape the
   verbs, which run the code under that guard, so they are caught here. *)
let ended f =
  match f () with
  | _ -> "returned"
  | exception Failure.Control `Discard -> "discarded"
  | exception e -> Printexc.to_string e

let always _ = true

let within w low high v =
  at_least w ~than:low v;
  at_most w ~than:high v

let even n = n mod 2 = 0

(* Tables *)

type shrinks =
  | Shrinks : string * (unit -> 'a Gen.t) * ('a -> bool) * string -> shrinks

type literal =
  | Literal : string * (unit -> 'a Gen.t) * ('a -> string) -> literal

type pick = Root | First

type rendered =
  | Rendered :
      string * (unit -> 'a Gen.t) * ('a -> bool) * pick * ('a -> string)
      -> rendered

type printer = Printer : string * (unit -> 'a Gen.t) * 'a * string -> printer

let shrinks claim rows =
  cases claim
    ~name:(fun (Shrinks (name, _, _, _)) -> name)
    rows
    (fun (Shrinks (_, gen, from, expected)) ->
      equal string expected (shrinks_to ~from (gen ())))

let literals claim rows =
  cases claim
    ~name:(fun (Literal (name, _, _)) -> name)
    rows
    (fun (Literal (_, gen, print)) ->
      let trees = List.init 60 (sample (gen ())) in
      equal (list string)
        (List.map (fun tree -> print (value tree)) trees)
        (List.map shown trees))

let renderings claim rows =
  cases claim
    ~name:(fun (Rendered (name, _, _, _, _)) -> name)
    rows
    (fun (Rendered (_, gen, accept, pick, expected)) ->
      let tree = find (gen ()) accept in
      let node =
        match pick with Root -> tree | First -> first_candidate tree
      in
      equal string (expected (value node)) (shown node))

let printers claim rows =
  cases claim
    ~name:(fun (Printer (name, _, _, _)) -> name)
    rows
    (fun (Printer (_, gen, v, expected)) ->
      equal string expected (Gen_engine.render_value (gen ()) v))

(* Generators *)

type kept = Kept : string * (unit -> 'a Gen.t) * ('a -> 'a -> unit) -> kept

let letters () =
  Gen.map (fun c -> Char.chr (97 + (Char.code c mod 16))) Gen.char

let identifier () =
  Gen.(
    string_of ~size:(int_range 1 8)
      (frequency [ (8, char_range 'a' 'z'); (1, of_list [ '-'; '_' ]) ]))

let not_aaa () =
  Gen.(
    such_that
      (fun s -> not (String.equal s "aaa"))
      (string_of ~size:(constant 3) (char_range 'a' 'z')))

let is_ident c = (c >= 'a' && c <= 'z') || c = '-' || c = '_'
let side_of_result = function Ok _ -> "Ok" | Error _ -> "Error"
let side_of_either = function Either.Left _ -> "Left" | Right _ -> "Right"

let kept =
  [
    Kept
      ( "int_range 10 100",
        (fun () -> Gen.int_range 10 100),
        fun _ -> within int 10 100 );
    Kept
      ( "int_range (-100) (-10)",
        (fun () -> Gen.int_range (-100) (-10)),
        fun _ -> within int (-100) (-10) );
    Kept
      ( "int32_range (-5l) 70_000l",
        (fun () -> Gen.int32_range (-5l) 70_000l),
        fun _ -> within int32 (-5l) 70_000l );
    Kept
      ( "int64_range (-1L) Int64.max_int, a span past Int64.max_int",
        (fun () -> Gen.int64_range (-1L) Int64.max_int),
        fun _ -> within int64 (-1L) Int64.max_int );
    Kept
      ( "nativeint_range 3n 900n",
        (fun () -> Gen.nativeint_range 3n 900n),
        fun _ -> within nativeint 3n 900n );
    Kept
      ( "float_range 2. 5.",
        (fun () -> Gen.float_range 2. 5.),
        fun _ -> within float_exact 2. 5. );
    Kept
      ( "char_range 'b' 'y'",
        (fun () -> Gen.char_range 'b' 'y'),
        fun _ -> within char 'b' 'y' );
    Kept
      ( "char_range '0' '9'",
        (fun () -> Gen.char_range '0' '9'),
        fun _ -> within char '0' '9' );
    Kept
      ( "string_of a character generator",
        (fun () -> Gen.string_of (letters ())),
        fun _ -> String.iter (within char 'a' 'p') );
    Kept
      ( "string_of ~size:(int_range 2 5)",
        (fun () -> Gen.(string_of ~size:(int_range 2 5) char)),
        fun _ s -> within int 2 5 (String.length s) );
    Kept
      ( "bytes_of ~size:(constant 3) (char_range 'a' 'z')",
        (fun () -> Gen.(bytes_of ~size:(constant 3) (char_range 'a' 'z'))),
        fun _ b ->
          equal int 3 (Bytes.length b);
          Bytes.iter (within char 'a' 'z') b );
    Kept
      ( "list ~size:(int_range 2 5)",
        (fun () -> Gen.(list ~size:(int_range 2 5) nat)),
        fun _ l -> within int 2 5 (List.length l) );
    Kept
      ( "list ~size:(such_that even nat)",
        (fun () -> Gen.(list ~size:(such_that even nat) nat)),
        fun _ l -> satisfies ~claim:"an even length" int even (List.length l) );
    Kept
      ( "result, which keeps its constructor",
        (fun () -> Gen.(result nat nat)),
        fun root v -> equal string (side_of_result root) (side_of_result v) );
    Kept
      ( "either, which keeps its constructor",
        (fun () -> Gen.(either nat nat)),
        fun root v -> equal string (side_of_either root) (side_of_either v) );
    Kept
      ( "frequency, a value of one of its generators",
        (fun () ->
          Gen.(frequency [ (1, int_range 1000 2000); (1, int_range 1 100) ])),
        fun _ ->
          satisfies ~claim:"in 1..100 or 1000..2000" int (fun v ->
              (1 <= v && v <= 100) || (1000 <= v && v <= 2000)) );
    Kept
      ( "such_that even int",
        (fun () -> Gen.(such_that even int)),
        fun _ -> satisfies ~claim:"even" int even );
    Kept
      ( "such_that over a sized string",
        not_aaa,
        fun _ s ->
          equal int 3 (String.length s);
          not_equal string "aaa" s;
          String.iter (within char 'a' 'z') s );
    Kept
      ( "bind into a list of the drawn length",
        (fun () ->
          Gen.(
            let* n = int_range 1 3 in
            list ~size:(constant n) nat)),
        fun _ l -> within int 1 3 (List.length l) );
    Kept
      ( "string_of over a frequency of char_range and of_list",
        identifier,
        fun _ s ->
          within int 1 8 (String.length s);
          String.iter (satisfies ~claim:"a letter, - or _" char is_ident) s );
  ]

let keeps (Kept (name, gen, check)) =
  prop name ~count:10 Gen.int64 (fun seed ->
      let tree = at_seed (gen ()) seed in
      List.iter (check (value tree)) (nodes ~limit:100 tree))

type malformed =
  | Malformed : string * (unit -> 'a Gen.t) * string option -> malformed

let finite = Some "Gen.float_range: bounds must be finite"
let negative_size = Some "Gen.list: negative size"
let negative_weight = Some "Gen.frequency: negative weight"

let malformed =
  [
    Malformed ("int_range 10 (-10)", (fun () -> Gen.int_range 10 (-10)), None);
    Malformed ("char_range 'z' 'a'", (fun () -> Gen.char_range 'z' 'a'), None);
    Malformed
      ( "int32_range 10l (-10l)",
        (fun () -> Gen.int32_range 10l (-10l)),
        Some "Gen.int32_range: high < low" );
    Malformed
      ( "int64_range Int64.max_int Int64.min_int",
        (fun () -> Gen.int64_range Int64.max_int Int64.min_int),
        Some "Gen.int64_range: high < low" );
    Malformed
      ( "nativeint_range 0n (-1n)",
        (fun () -> Gen.nativeint_range 0n (-1n)),
        Some "Gen.nativeint_range: high < low" );
    Malformed
      ( "float_range 1. 0.",
        (fun () -> Gen.float_range 1. 0.),
        Some "Gen.float_range: high < low" );
    Malformed
      ("float_range nan 1.", (fun () -> Gen.float_range Float.nan 1.), finite);
    Malformed
      ( "float_range 0. infinity",
        (fun () -> Gen.float_range 0. Float.infinity),
        finite );
    Malformed
      ( "float_range 1. neg_infinity, not finite before high < low",
        (fun () -> Gen.float_range 1. Float.neg_infinity),
        finite );
    Malformed
      ( "float_range (-.max_float) max_float",
        (fun () -> Gen.float_range (-.Float.max_float) Float.max_float),
        Some "Gen.float_range: high -. low > max_float" );
    Malformed
      ( "list ~size:(constant (-1))",
        (fun () -> Gen.(list ~size:(constant (-1)) unit)),
        negative_size );
    Malformed
      ( "array ~size:(constant (-1))",
        (fun () -> Gen.(array ~size:(constant (-1)) unit)),
        negative_size );
    Malformed
      ( "string_of ~size:(constant (-1))",
        (fun () -> Gen.(string_of ~size:(constant (-1)) char)),
        negative_size );
    Malformed
      ( "bytes_of ~size:(constant (-1))",
        (fun () -> Gen.(bytes_of ~size:(constant (-1)) char)),
        negative_size );
    Malformed ("of_list []", (fun () -> Gen.of_list []), None);
    Malformed ("one_of []", (fun () -> Gen.one_of []), None);
    Malformed
      ( "frequency []",
        (fun () -> Gen.frequency []),
        Some "Gen.frequency: empty list" );
    Malformed
      ( "frequency [ (-1, nat) ], negative before a total below 1",
        (fun () -> Gen.(frequency [ (-1, nat) ])),
        negative_weight );
    Malformed
      ( "frequency [ (2, nat); (-1, nat) ]",
        (fun () -> Gen.(frequency [ (2, nat); (-1, nat) ])),
        negative_weight );
    Malformed
      ( "frequency [ (0, nat) ]",
        (fun () -> Gen.(frequency [ (0, nat) ])),
        Some "Gen.frequency: total weight < 1" );
  ]

let raises_when_sampled (Malformed (_, make, message)) =
  let gen = make () in
  let draw () = Gen_engine.sample gen (state 0) in
  match message with
  | Some message -> raises (Invalid_argument message) draw
  | None -> raises_match (fun e -> Exn.invalid_arg e) draw

type any = Any : string * (unit -> 'a Gen.t) -> any

let pure =
  [
    Any ("int", fun () -> Gen.int);
    Any ("string", fun () -> Gen.string);
    Any ("list int", fun () -> Gen.(list int));
    Any ("one_of", fun () -> Gen.(one_of [ int_range 0 9; int_range 100 109 ]));
    Any ("bind", fun () -> Gen.(bind nat (fun n -> int_range 0 (n + 1))));
    Any
      ( "of_list",
        fun () -> Gen.(with_pp Format.pp_print_int (of_list [ 1; 2; 3 ])) );
    Any
      ("of_list ~pp", fun () -> Gen.of_list ~pp:Format.pp_print_int [ 1; 2; 3 ]);
    Any ("char_range", fun () -> Gen.char_range 'a' 'z');
    Any ("list ~size", fun () -> Gen.(list ~size:(int_range 0 6) nat));
    Any ("such_that", fun () -> Gen.(such_that even int));
  ]

(* [layout ~reversed depth tree] is [tree]'s renderings to [depth] levels and
   4 candidates a node. [reversed] forces the subtrees of a node last first. *)
let rec layout ~reversed depth tree =
  let subtrees =
    if depth = 0 then []
    else List.of_seq (Seq.take 4 (Shrink_tree.children tree))
  in
  let order = if reversed then List.rev else Fun.id in
  let laid = order (List.map (layout ~reversed (depth - 1)) (order subtrees)) in
  shown tree ^ "(" ^ String.concat " " laid ^ ")"

let forced_in_any_order (Any (_, gen)) =
  let gen = gen () in
  let index =
    find_index gen (fun tree -> not (Seq.is_empty (Shrink_tree.children tree)))
  in
  equal string
    (layout ~reversed:false 3 (sample gen index))
    (layout ~reversed:true 3 (sample gen index))

let replayed_values () =
  let row name gen index = name ^ ": " ^ shown (sample gen index) in
  expect
    (String.concat "\n"
       [
         row "int" Gen.int 0;
         row "nat" Gen.nat 1;
         row "float" Gen.float 2;
         row "string_of"
           Gen.(string_of ~size:(int_range 0 6) (char_range 'a' 'z'))
           3;
         row "list of small_int" Gen.(list ~size:(int_range 0 5) small_int) 4;
         row "option, one_of and frequency"
           Gen.(
             triple (option bool)
               (one_of [ int_range 0 9; int_range 100 109 ])
               (frequency [ (1, int_range 0 9); (3, int_range 100 109) ]))
           5;
         row "list of the default length" Gen.(list bool) 6;
       ])
  @@ __POS_OF__
       {|
    int: 3814646949886580551
    nat: 254
    float: 9.32137030625773e+307
    string_of: "evmm"
    list of small_int: [1809; 5; -31; -839; 0]
    option, one_of and frequency: (Some (false), 9, 8)
    list of the default length: [false; true]
    |}

let replayed_choices () =
  let count gen p = List.length (List.filter p (samples gen 2_000)) in
  expect
    (strf "sum of nat: %d\nNone: %d\nError: %d\nLeft: %d"
       (List.fold_left ( + ) 0 (samples Gen.nat 2_000))
       (count Gen.(option unit) Option.is_none)
       (count Gen.(result unit unit) Result.is_error)
       (count Gen.(either unit unit) Either.is_left))
  @@ __POS_OF__
       {|
    sum of nat: 734774
    None: 270
    Error: 473
    Left: 1026
    |}

let generators =
  group "Generators"
    [
      group "every candidate satisfies the constraints of its generator"
        (List.map keeps kept);
      cases
        "a malformed generator is built, and raises Invalid_argument when it \
         samples"
        ~name:(fun (Malformed (name, _, _)) -> name)
        malformed raises_when_sampled;
      cases
        "a sampled tree is a function of the generator and the state, whatever \
         the order its cells are forced in"
        ~name:(fun (Any (name, _)) -> name)
        pure forced_in_any_order;
      test "a recorded seed replays these values" replayed_values;
      test "a recorded seed replays these choices" replayed_choices;
    ]

(* Numbers *)

let numbers_shrink =
  let nonzero v = v <> 0 in
  [
    Shrinks ("int", (fun () -> Gen.int), nonzero, "0");
    Shrinks ("nat", (fun () -> Gen.nat), nonzero, "0");
    Shrinks
      ( "small_int, from a negative value",
        (fun () -> Gen.small_int),
        (fun v -> v < 0),
        "0" );
    Shrinks
      ( "int_range 10 100",
        (fun () -> Gen.int_range 10 100),
        (fun v -> v > 10),
        "10" );
    Shrinks
      ( "int_range (-100) (-10)",
        (fun () -> Gen.int_range (-100) (-10)),
        (fun v -> v < -10),
        "-10" );
    Shrinks
      ( "int_range min_int max_int",
        (fun () -> Gen.int_range min_int max_int),
        nonzero,
        "0" );
    Shrinks
      ("int32", (fun () -> Gen.int32), (fun v -> not (Int32.equal v 0l)), "0l");
    Shrinks
      ("int64", (fun () -> Gen.int64), (fun v -> not (Int64.equal v 0L)), "0L");
    Shrinks
      ( "nativeint",
        (fun () -> Gen.nativeint),
        (fun v -> not (Nativeint.equal v 0n)),
        "0n" );
    Shrinks
      ( "int32_range 10l 100l",
        (fun () -> Gen.int32_range 10l 100l),
        (fun v -> v > 10l),
        "10l" );
    Shrinks
      ( "int64_range (-100L) (-10L)",
        (fun () -> Gen.int64_range (-100L) (-10L)),
        (fun v -> v < -10L),
        "-10L" );
    Shrinks
      ( "nativeint_range Nativeint.min_int Nativeint.max_int",
        (fun () -> Gen.nativeint_range Nativeint.min_int Nativeint.max_int),
        (fun v -> not (Nativeint.equal v 0n)),
        "0n" );
    Shrinks
      ("float", (fun () -> Gen.float), (fun v -> not (Float.equal v 0.)), "0.");
    Shrinks
      ("float_range 2. 5.", (fun () -> Gen.float_range 2. 5.), always, "2.");
    Shrinks
      ( "float_range (-5.) (-2.)",
        (fun () -> Gen.float_range (-5.) (-2.)),
        always,
        "-2." );
    Shrinks
      ( "float_range (-0.) 1., to 0.",
        (fun () -> Gen.float_range (-0.) 1.),
        always,
        "0." );
    Shrinks
      ( "float_range (-1.) (-0.), to 0.",
        (fun () -> Gen.float_range (-1.) (-0.)),
        always,
        "0." );
  ]

let number_literals =
  let exact = Pp.to_string Pp.float_exact in
  [
    Literal ("int", (fun () -> Gen.int), string_of_int);
    Literal ("small_int", (fun () -> Gen.small_int), string_of_int);
    Literal ("int32", (fun () -> Gen.int32), strf "%ldl");
    Literal ("int64", (fun () -> Gen.int64), strf "%LdL");
    Literal ("nativeint", (fun () -> Gen.nativeint), strf "%ndn");
    Literal
      ("int32_range", (fun () -> Gen.int32_range (-1000l) 1000l), strf "%ldl");
    Literal
      ("int64_range", (fun () -> Gen.int64_range Int64.min_int 0L), strf "%LdL");
    Literal
      ("nativeint_range", (fun () -> Gen.nativeint_range (-7n) 7n), strf "%ndn");
    Literal ("float", (fun () -> Gen.float), exact);
    Literal ("float_range", (fun () -> Gen.float_range (-1e6) 1e6), exact);
  ]

let bounds =
  Gen.(triple (int_range (-1000) 1000) (int_range (-1000) 1000) int64)

let ranged (a, b, seed) =
  let low = min a b and high = max a b in
  (low, high, at_seed (Gen.int_range low high) seed)

(* A corner is a neighbour of the origin only inside the range, so a range
   whose origin is a bound draws no value past it. *)
let origin_at_a_bound (low, high) =
  let values = samples (Gen.int_range low high) 2_000 in
  at_least int ~than:low (least values);
  at_most int ~than:high (greatest values)

let draws_within triple =
  let low, high, tree = ranged triple in
  within int low high (value tree)

let nearer_the_origin triple =
  let low, high, tree = ranged triple in
  let origin = max low (min high 0) in
  let gap v = abs (v - origin) in
  List.iter
    (fun (parent, child) ->
      within int (min origin parent) (max origin parent) child;
      less int ~than:(gap parent) (gap child))
    (edges tree)

(* Each stratum is uniform below its bound, so the bands overlap: below 10 is
   0.5 + 0.25 * 0.1 + 0.2 * 0.01 + 0.05 * 0.001, and so on. *)
let nat_strata () =
  let values = samples Gen.nat 20_000 in
  let band (low, high) = share values (fun v -> low <= v && v < high) in
  at_least int ~than:0 (least values);
  less int ~than:10_000 (greatest values);
  equal
    (list (float 0.015))
    [ 0.52705; 0.24345; 0.1845; 0.045 ]
    (List.map band [ (0, 10); (10, 100); (100, 1_000); (1_000, 10_000) ])

let small_int_range () =
  let values = samples Gen.small_int 500 in
  at_least int ~than:(-9_999) (least values);
  less int ~than:0 (least values);
  greater int ~than:0 (greatest values);
  at_most int ~than:9_999 (greatest values)

(* On a 64-bit platform a hundred uniform draws cannot all fit in 32 bits. *)
let nativeint_word () =
  let values = samples Gen.nativeint 100 in
  let wide =
    if Nativeint.size = 64 then Nativeint.of_int32 Int32.max_int else 0n
  in
  less nativeint ~than:(Nativeint.neg wide)
    (List.fold_left Nativeint.min 0n values);
  greater nativeint ~than:wide (List.fold_left Nativeint.max 0n values)

type corners =
  | Corners : string * (unit -> 'a Gen.t) * 'a testable * 'a list -> corners

let corners =
  [
    Corners ("int", (fun () -> Gen.int), int, [ 0; 1; -1; min_int; max_int ]);
    Corners
      ("int_range 0 1000", (fun () -> Gen.int_range 0 1000), int, [ 0; 1; 1000 ]);
    Corners
      ( "int_range (-5) 5",
        (fun () -> Gen.int_range (-5) 5),
        int,
        [ -5; -1; 0; 1; 5 ] );
    Corners ("int_range 3 9", (fun () -> Gen.int_range 3 9), int, [ 3; 4; 9 ]);
    Corners
      ( "int32",
        (fun () -> Gen.int32),
        int32,
        [ 0l; 1l; -1l; Int32.min_int; Int32.max_int ] );
    Corners
      ( "int64",
        (fun () -> Gen.int64),
        int64,
        [ 0L; 1L; -1L; Int64.min_int; Int64.max_int ] );
    Corners
      ( "nativeint",
        (fun () -> Gen.nativeint),
        nativeint,
        [ 0n; 1n; -1n; Nativeint.min_int; Nativeint.max_int ] );
    Corners
      ( "int32_range Int32.min_int (-7l)",
        (fun () -> Gen.int32_range Int32.min_int (-7l)),
        int32,
        [ Int32.min_int; -8l; -7l ] );
    Corners
      ( "int64_range (-1_000_000L) 1_000_000_000_000L",
        (fun () -> Gen.int64_range (-1_000_000L) 1_000_000_000_000L),
        int64,
        [ -1_000_000L; -1L; 0L; 1L; 1_000_000_000_000L ] );
    Corners
      ( "int64_range Int64.min_int Int64.max_int",
        (fun () -> Gen.int64_range Int64.min_int Int64.max_int),
        int64,
        [ 0L; 1L; -1L; Int64.min_int; Int64.max_int ] );
    Corners
      ( "nativeint_range 5n Nativeint.max_int",
        (fun () -> Gen.nativeint_range 5n Nativeint.max_int),
        nativeint,
        [ 5n; 6n; Nativeint.max_int ] );
  ]

(* One draw in ten is a corner, each corner equally likely, so a corner of
   [int] comes about once in 50 draws; 2000 draws miss one with probability
   below 1e-17. *)
let reaches_its_corners (Corners (_, gen, w, corners)) =
  let values = samples (gen ()) 2_000 in
  let drawn c = List.exists (Testable.equal w c) values in
  equal (list w) corners (List.filter drawn corners)

(* In a wide range the uniform draws almost never hit a corner. *)
let corner_share () =
  let values = samples (Gen.int_range (-1_000_000) 1_000_000) 20_000 in
  let corner v = List.mem v [ -1_000_000; -1; 0; 1; 1_000_000 ] in
  equal (float 0.01) 0.1 (share values corner)

type sized =
  | Sized :
      string * ('a -> 'a -> 'a Gen.t) * 'a Gen.t * 'a testable * 'a
      -> sized

let sized_ranges =
  [
    Sized ("int32_range", Gen.int32_range, Gen.int32, int32, 0l);
    Sized ("int64_range", Gen.int64_range, Gen.int64, int64, 0L);
    Sized ("nativeint_range", Gen.nativeint_range, Gen.nativeint, nativeint, 0n);
  ]

(* Bounds drawn over the whole type reach its extremes, and a range of 2^63
   values or more. *)
let sized_range (Sized (name, range, whole, w, zero)) =
  prop
    (name
   ^ " low high draws within [low;high], each candidate between the origin and \
      its parent")
    ~count:50
    Gen.(triple whole whole int64)
    (fun (a, b, seed) ->
      let low = min a b and high = max a b in
      let origin = max low (min high zero) in
      let tree = at_seed (range low high) seed in
      within w low high (value tree);
      List.iter
        (fun (parent, child) ->
          within w (min origin parent) (max origin parent) child;
          not_equal w parent child)
        (edges ~limit:100 tree))

(* [-2^62; Int64.max_int] holds a third of its values below 0, and two of its
   five corners. *)
let wide_int64_range () =
  let values =
    samples (Gen.int64_range (Int64.div Int64.min_int 2L) Int64.max_int) 4_000
  in
  equal (float 0.02) 0.34 (share values (fun v -> Int64.compare v 0L < 0))

(* Half the finite bit patterns are negative, and half have a magnitude of at
   least 1; a float uniform over the reals would have nearly none below 1. *)
let float_bits () =
  let values = samples Gen.float 4_000 in
  equal int 0
    (List.length (List.filter (fun v -> not (Float.is_finite v)) values));
  equal
    (list (float 0.04))
    [ 0.5; 0.5 ]
    [ share values Float.sign_bit; share values (fun v -> Float.abs v >= 1.) ]

(* A float far from 0 halves its gap more than 15 times before it converges,
   so a far one reaches the cut. *)
let float_cut () =
  let counts =
    List.init 20 (fun i ->
        Seq.length (Shrink_tree.children (sample Gen.float i)))
  in
  at_most int ~than:15 (greatest counts);
  mem int 15 counts

let float_edges () =
  let draw gen = value (sample gen 0) in
  equal float_exact 1.5 (draw (Gen.float_range 1.5 1.5));
  satisfies ~claim:"finite" float_exact Float.is_finite
    (draw (Gen.float_range 0. Float.max_float));
  equal int 0
    (List.length
       (List.filter (Float.equal 2.) (samples (Gen.float_range 1. 2.) 1_000)))

let numbers =
  group "Numbers"
    [
      shrinks "a number shrinks toward its origin" numbers_shrink;
      test "a search for a value above 50 in int_range 0 1000 stops at 51"
        (fun () ->
          equal string "51"
            (shrinks_to ~failing:(fun v -> v > 50) (Gen.int_range 0 1000)));
      prop "int_range low high draws within [low;high]" bounds draws_within;
      cases
        "int_range low high draws within [low;high] when its origin is a bound"
        ~name:(fun (low, high) -> strf "int_range %d %d" low high)
        [ (5, 5); (-100, -10); (3, 9) ]
        origin_at_a_bound;
      prop
        "an integer's candidates lie between the origin and their parent, \
         strictly nearer the origin"
        ~count:30 bounds nearer_the_origin;
      group "a range of int32, int64 or nativeint behaves as int_range"
        (List.map sized_range sized_ranges);
      test "int64_range draws uniformly over a range of more than 2^63 values"
        wide_int64_range;
      literals
        "a number prints as an OCaml literal, a float as its shortest round \
         trip"
        number_literals;
      test
        "nat draws below 10_000, 50%, 25%, 20% and 5% below 10, 100, 1_000 and \
         10_000"
        nat_strata;
      test "small_int draws in [-9_999;9_999], either sign" small_int_range;
      test "nativeint draws over the whole native word" nativeint_word;
      cases "an integer generator draws each of its corners"
        ~name:(fun (Corners (name, _, _, _)) -> name)
        corners reaches_its_corners;
      test "one draw in ten is a corner" corner_share;
      test "float draws finite floats, uniformly over their bit patterns"
        float_bits;
      test "a float node has at most 15 candidates" float_cut;
      test "float_range draws inside its edges" float_edges;
    ]

(* Unit, booleans, characters and strings *)

let consumes =
  [
    (Any ("unit", fun () -> Gen.unit), false);
    (Any ("constant 7", fun () -> Gen.constant 7), false);
    (Any ("bool, which does", fun () -> Gen.bool), true);
  ]

let draws_words (Any (_, gen), consumes) =
  let s = state 0 in
  let words_after = words 2 (successor (gen ()) s) in
  if consumes then not_equal (list int64) (words 2 s) words_after
  else equal (list int64) (words 2 s) words_after

let bool_values () =
  equal (list bool) [ false; true ]
    (List.sort_uniq Bool.compare (samples Gen.bool 100));
  equal (list bool) [ false ] (candidate_values (find Gen.bool Fun.id));
  equal (list bool) [] (candidate_values (find Gen.bool not))

let base_literals =
  let quoted = strf "%S" in
  [
    Literal ("unit", (fun () -> Gen.unit), fun () -> "()");
    Literal ("char", (fun () -> Gen.char), strf "%C");
    Literal ("char_range 'a' 'z'", (fun () -> Gen.char_range 'a' 'z'), strf "%C");
    Literal
      ( "uchar",
        (fun () -> Gen.uchar),
        fun u -> strf "Uchar.of_int 0x%X" (Uchar.to_int u) );
    Literal
      ( "char_range '\\000' '\\031', escaped",
        (fun () -> Gen.char_range '\000' '\031'),
        strf "%C" );
    Literal ("string", (fun () -> Gen.string), quoted);
    Literal
      ( "string_of a printerless char",
        (fun () -> Gen.string_of (letters ())),
        quoted );
    Literal ("string_of a printerless frequency", identifier, quoted);
    Literal
      ( "bytes",
        (fun () -> Gen.bytes),
        fun b -> strf "Bytes.of_string %S" (Bytes.to_string b) );
    Literal
      ( "bytes_of ~size",
        (fun () -> Gen.(bytes_of ~size:(int_range 0 4) char)),
        fun b -> strf "Bytes.of_string %S" (Bytes.to_string b) );
  ]

let base_shrink =
  let longer s = String.length s >= 2 in
  [
    Shrinks ("char", (fun () -> Gen.char), (fun c -> c <> 'a'), "'a'");
    Shrinks
      ( "char_range 'b' 'y'",
        (fun () -> Gen.char_range 'b' 'y'),
        (fun c -> c > 'b'),
        "'b'" );
    Shrinks
      ( "char_range 'A' 'Z'",
        (fun () -> Gen.char_range 'A' 'Z'),
        (fun c -> c < 'Z'),
        "'Z'" );
    Shrinks
      ( "char_range '0' '9'",
        (fun () -> Gen.char_range '0' '9'),
        (fun c -> c < '9'),
        "'9'" );
    Shrinks ("uchar", (fun () -> Gen.uchar), always, "Uchar.of_int 0x61");
    Shrinks ("string", (fun () -> Gen.string), longer, {|""|});
    Shrinks
      ( "string_of ~size:(int_range 2 5)",
        (fun () -> Gen.(string_of ~size:(int_range 2 5) char)),
        always,
        {|"aa"|} );
    Shrinks
      ( "bytes",
        (fun () -> Gen.bytes),
        (fun b -> Bytes.length b >= 1),
        {|Bytes.of_string ""|} );
  ]

let high_and_low (_, gen) =
  let codes = List.map Char.code (samples (gen ()) 300) in
  less int ~than:32 (least codes);
  greater int ~than:127 (greatest codes)

let uchar_corners =
  List.map Uchar.of_int
    [
      0x0;
      0x7F;
      0x80;
      0x7FF;
      0x800;
      0xD7FF;
      0xE000;
      0xFFFD;
      0xFFFF;
      0x10000;
      0x10FFFF;
    ]

(* Each length draws 0.9 / 4 of the values, and the corners add 2, 2, 5 and
   2 elevenths of 0.1. *)
let uchar_lengths () =
  let values = samples Gen.uchar 8_000 in
  let length n u = Uchar.utf_8_byte_length u = n in
  equal
    (list (float 0.015))
    [ 0.2432; 0.2432; 0.2705; 0.2432 ]
    (List.map (fun n -> share values (length n)) [ 1; 2; 3; 4 ])

let uchar_nearer_a seed =
  let tree = at_seed Gen.uchar seed in
  let a = Uchar.of_char 'a' in
  List.iter
    (fun (parent, child) ->
      within uchar (min a parent) (max a parent) child;
      not_equal uchar parent child)
    (edges tree)

let past code u = Uchar.to_int u > code

let base =
  group "Unit, booleans, characters and strings"
    [
      cases "unit and constant consume no randomness"
        ~name:(fun (Any (n, _), _) -> n)
        consumes draws_words;
      test
        "bool draws both values, true's one candidate is false, and false has \
         none"
        bool_values;
      cases "char draws below 32 and above 127, as does the full char_range"
        ~name:fst
        [
          ("char", fun () -> Gen.char);
          ("char_range '\\000' '\\255'", fun () -> Gen.char_range '\000' '\255');
        ]
        high_and_low;
      shrinks
        "a character shrinks toward the character of its range closest to 'a', \
         a string as a list"
        base_shrink;
      literals "unit, a character, a string and bytes print as OCaml literals"
        base_literals;
      test
        "uchar draws each length of UTF-8 encoding, 1 to 4 bytes, with equal \
         probability"
        uchar_lengths;
      test "uchar draws each of its corners" (fun () ->
          reaches_its_corners
            (Corners ("uchar", (fun () -> Gen.uchar), uchar, uchar_corners)));
      prop
        "a uchar's candidates lie between U+0061 and their parent, never a \
         surrogate"
        ~count:50 Gen.int64 uchar_nearer_a;
      cases
        "a search for a uchar past a code point stops at the next scalar value"
        ~name:(fun (code, _) -> strf "past 0x%X" code)
        [
          (0x7F, "Uchar.of_int 0x80");
          (0xD7FF, "Uchar.of_int 0xE000");
          (0xFFFF, "Uchar.of_int 0x10000");
        ]
        (fun (code, expected) ->
          equal string expected (shrinks_to ~failing:(past code) Gen.uchar));
    ]

(* Containers *)

let container_shrink =
  let nonzero = List.exists (fun v -> v <> 0) in
  [
    Shrinks
      ( "list int",
        (fun () -> Gen.(list int)),
        (fun l -> List.length l >= 2),
        "[]" );
    Shrinks
      ( "list ~size:(int_range 2 5) nat",
        (fun () -> Gen.(list ~size:(int_range 2 5) nat)),
        always,
        "[0; 0]" );
    Shrinks
      ( "list ~size:(such_that even nat)",
        (fun () -> Gen.(list ~size:(such_that even nat) nat)),
        (fun l -> List.length l >= 2),
        "[]" );
    Shrinks
      ( "array nat",
        (fun () -> Gen.(array nat)),
        (fun a -> Array.length a >= 1),
        "[||]" );
    Shrinks
      ( "pair nat nat",
        (fun () -> Gen.(pair nat nat)),
        (fun (a, _) -> a > 0),
        "(0, 0)" );
    Shrinks
      ( "triple",
        (fun () -> Gen.(triple nat nat nat)),
        (fun (a, b, c) -> nonzero [ a; b; c ]),
        "(0, 0, 0)" );
    Shrinks
      ( "quad",
        (fun () -> Gen.(quad nat nat nat nat)),
        (fun (a, b, c, d) -> nonzero [ a; b; c; d ]),
        "(0, 0, 0, 0)" );
  ]

let option_text f = function None -> "None" | Some v -> strf "Some (%s)" (f v)

let container_literals =
  [
    Literal
      ( "list",
        (fun () -> Gen.(list ~size:(int_range 0 4) small_int)),
        fun l -> "[" ^ ints l ^ "]" );
    Literal
      ( "array",
        (fun () -> Gen.(array ~size:(int_range 0 4) small_int)),
        fun a -> "[|" ^ ints (Array.to_list a) ^ "|]" );
    Literal
      ("pair", (fun () -> Gen.(pair nat nat)), fun (a, b) -> strf "(%d, %d)" a b);
    Literal
      ( "pair over unit",
        (fun () -> Gen.(pair unit nat)),
        fun ((), n) -> strf "((), %d)" n );
    Literal ("option", (fun () -> Gen.(option nat)), option_text string_of_int);
    Literal
      ( "result",
        (fun () -> Gen.(result nat nat)),
        function Ok v -> strf "Ok (%d)" v | Error v -> strf "Error (%d)" v );
    Literal
      ( "either",
        (fun () -> Gen.(either nat nat)),
        function
        | Either.Left v -> strf "Left (%d)" v
        | Right v -> strf "Right (%d)" v );
  ]

let mapped_nat () = Gen.(map succ nat)

let container_pre_images =
  [
    Rendered
      ( "a list of mapped nats",
        (fun () -> Gen.(list ~size:(int_range 2 2) (mapped_nat ()))),
        always,
        Root,
        fun l -> "from [" ^ ints (List.map pred l) ^ "]" );
    Rendered
      ( "Some of a mapped nat",
        (fun () -> Gen.option (mapped_nat ())),
        Option.is_some,
        Root,
        fun o -> "from " ^ option_text (fun n -> string_of_int (n - 1)) o );
    Rendered
      ( "None, a value",
        (fun () -> Gen.option (mapped_nat ())),
        Option.is_none,
        Root,
        fun _ -> "None" );
    Rendered
      ( "Left of a mapped nat",
        (fun () -> Gen.(either (mapped_nat ()) nat)),
        Either.is_left,
        Root,
        function
        | Either.Left n -> strf "from Left (%d)" (n - 1)
        | Right n -> strf "Right (%d)" n );
    Rendered
      ( "a pair of a mapped nat and a string, by and+",
        (fun () ->
          Gen.(
            let+ a = mapped_nat ()
            and+ b = string_of ~size:(int_range 1 1) (char_range 'x' 'x') in
            (a, b))),
        always,
        Root,
        fun (a, b) -> strf "from (%d, %S)" (a - 1) b );
  ]

let container_printers =
  let printed_of_list () =
    Gen.(with_pp Format.pp_print_int (of_list [ 10; 20; 30 ]))
  in
  [
    Printer ("list nat", (fun () -> Gen.(list nat)), [ 1; 2 ], "[1; 2]");
    Printer
      ( "list of a printed of_list",
        (fun () -> Gen.list (printed_of_list ())),
        [ 10; 20 ],
        "[10; 20]" );
    Printer
      ( "list of an of_list ~pp",
        (fun () -> Gen.list (Gen.of_list ~pp:Format.pp_print_int [ 10; 20 ])),
        [ 20; 10 ],
        "[20; 10]" );
    Printer
      ( "pair of a constant ~pp and nat",
        (fun () -> Gen.(pair (constant ~pp:Format.pp_print_string "x") nat)),
        ("x", 3),
        "(x, 3)" );
    Printer
      ("pair unit nat", (fun () -> Gen.(pair unit nat)), ((), 3), "((), 3)");
    Printer
      ( "pair (constant ()) nat",
        (fun () -> Gen.(pair (constant ()) nat)),
        ((), 0),
        placeholder );
    Printer
      ( "either nat (constant 'k')",
        (fun () -> Gen.(either nat (constant 'k'))),
        Either.Left 1,
        placeholder );
  ]

(* Each candidate is labelled against the drawn list: shorter, or the count of
   elements it changed. *)
let list_candidates () =
  let tree =
    find
      Gen.(list ~size:(int_range 0 6) (int_range 1 9))
      (fun xs -> List.length xs >= 4 && List.for_all (fun x -> x > 1) xs)
  in
  let drawn = value tree in
  let label xs =
    if List.length xs < List.length drawn then "shorter"
    else if List.length xs > List.length drawn then "longer"
    else
      strf "%d changed"
        (List.length (List.filter Fun.id (List.map2 ( <> ) drawn xs)))
  in
  equal (list string) [ "1 changed"; "shorter" ]
    (List.sort_uniq String.compare (List.map label (candidate_values tree)))

(* A size generator whose tree is given by hand: a root length and its
   candidate lengths. *)
let sized root lengths =
  Gen_engine.make (fun state ->
      ( Shrink_tree.make ~root
          ~children:(List.to_seq (List.map Shrink_tree.leaf lengths)),
        state ))

let negative_candidate_length () =
  let tree = sample (Gen.list ~size:(sized 2 [ -1 ]) Gen.unit) 0 in
  equal int 2 (List.length (value tree));
  raises (Invalid_argument "Gen.list: negative size") (fun () ->
      Shrink_tree.children tree ())

(* The strata of [nat] over the bounds 4, 8, 16 and 64, overlapping as
   [nat]'s do. *)
let default_length () =
  let lengths = List.map List.length (samples Gen.(list unit) 20_000) in
  let band (low, high) = share lengths (fun n -> low <= n && n < high) in
  less int ~than:64 (greatest lengths);
  equal
    (list (float 0.015))
    [ 0.678125; 0.178125; 0.10625; 0.0375 ]
    (List.map band [ (0, 4); (4, 8); (8, 16); (16, 64) ]);
  equal (float 0.3) 4.7
    (float_of_int (List.fold_left ( + ) 0 lengths) /. 20_000.)

let constructor_shares () =
  let at gen p = share (samples gen 8_000) p in
  equal
    (list (float 0.02))
    [ 0.15; 0.25; 0.5 ]
    [
      at Gen.(option unit) Option.is_none;
      at Gen.(result unit unit) Result.is_error;
      at Gen.(either unit unit) Either.is_left;
    ]

type tuple =
  | Tuple :
      string * (unit -> 'a Gen.t) * ('a -> bool) * 'a testable * ('a -> 'a)
      -> tuple

let tuples =
  let all_nonzero = List.for_all (fun v -> v <> 0) in
  [
    Tuple
      ( "pair",
        (fun () -> Gen.(pair nat nat)),
        (fun (a, _) -> a > 0),
        pair int int,
        fun (_, b) -> (0, b) );
    Tuple
      ( "triple",
        (fun () -> Gen.(triple nat nat nat)),
        (fun (a, b, c) -> all_nonzero [ a; b; c ]),
        triple int int int,
        fun (_, b, c) -> (0, b, c) );
    Tuple
      ( "quad",
        (fun () -> Gen.(quad nat nat nat nat)),
        (fun (a, b, c, d) -> all_nonzero [ a; b; c; d ]),
        quad int int int int,
        fun (_, b, c, d) -> (0, b, c, d) );
  ]

let first_reduces_the_left (Tuple (_, gen, accept, w, expected)) =
  let tree = find (gen ()) accept in
  equal w (expected (value tree)) (value (first_candidate tree))

let first_then_second () =
  let draws index =
    let s = state index in
    let a, after_a = Gen_engine.run Gen.int s in
    let b, after_b = Gen_engine.run Gen.int after_a in
    let ab, after_ab = Gen_engine.run Gen.(pair int int) s in
    ( (Shrink_tree.root a, Shrink_tree.root b),
      Shrink_tree.root ab,
      words 2 after_b,
      words 2 after_ab )
  in
  let rows = List.init 10 draws in
  equal
    (list (pair int int))
    (List.map (fun (e, _, _, _) -> e) rows)
    (List.map (fun (_, a, _, _) -> a) rows);
  equal
    (list (list int64))
    (List.map (fun (_, _, e, _) -> e) rows)
    (List.map (fun (_, _, _, a) -> a) rows)

let containers =
  group "Containers"
    [
      shrinks
        "a container shrinks to its shortest length, its parts to their origins"
        container_shrink;
      test "a list's candidate is shorter, or reduces one element"
        list_candidates;
      test "a sized list's candidate whose elements discard is skipped"
        (fun () ->
          let gen =
            Gen.list
              ~size:(sized 0 [ 3; 0; 2 ])
              (Gen.such_that (fun _ -> false) Gen.unit)
          in
          equal (list (list unit)) [ [] ] (candidate_values (sample gen 0)));
      test "a negative candidate length raises when its cell is forced"
        negative_candidate_length;
      test
        "the default length is below 64, 50%, 25%, 20% and 5% below 4, 8, 16 \
         and 64, about 5 on average"
        default_length;
      test
        "option, result and either draw None at 0.15, Error at 0.25 and Left \
         at 0.5"
        constructor_shares;
      test "None is the first candidate of a Some" (fun () ->
          equal (option int) None
            (value (first_candidate (find Gen.(option nat) Option.is_some))));
      cases "a tuple shrinks its first component first"
        ~name:(fun (Tuple (n, _, _, _, _)) -> n)
        tuples first_reduces_the_left;
      test "pair draws its first component, then its second" first_then_second;
      literals "a container prints its parts in OCaml syntax" container_literals;
      renderings
        "a container renders as a pre-image when a part is one, None as a value"
        container_pre_images;
      printers "a container has a printer iff every component has one"
        container_printers;
    ]

(* Constants, choices and filters *)

type one = One : string * (unit -> 'a Gen.t) * 'a testable * 'a -> one

let ones =
  [
    One ("constant 42", (fun () -> Gen.constant 42), int, 42);
    One ("unit", (fun () -> Gen.unit), unit, ());
    One ("int_range 5 5", (fun () -> Gen.int_range 5 5), int, 5);
    One ("char_range 'x' 'x'", (fun () -> Gen.char_range 'x' 'x'), char, 'x');
    One ("of_list [ 7 ]", (fun () -> Gen.of_list [ 7 ]), int, 7);
  ]

let draws_one (One (_, gen, w, v)) =
  let tree = sample (gen ()) 0 in
  equal (list w) [ v ] (value tree :: candidate_values tree)

let of_list_values () =
  let gen = Gen.of_list [ 10; 20; 30 ] in
  equal (list int) [ 10; 20; 30 ] (List.sort_uniq Int.compare (samples gen 100));
  equal (list int) [ 10; 20 ] (candidate_values (find gen (fun v -> v = 30)));
  equal string "10"
    (shrinks_to ~from:(fun v -> v = 30) (Gen.with_pp Format.pp_print_int gen))

let one_of_candidates () =
  let gen =
    Gen.(one_of [ constant 10; constant 20; constant 30; int_range 40 50 ])
  in
  let tree = find gen (fun v -> v > 41) in
  let later = List.filteri (fun i _ -> i >= 3) (candidate_values tree) in
  equal (list int) [ 10; 20; 30 ]
    (List.filteri (fun i _ -> i < 3) (candidate_values tree));
  not_equal (list int) [] later;
  equal (list int)
    (List.filter (fun v -> 40 <= v && v < value tree) later)
    later

let weights () =
  let values =
    samples Gen.(frequency [ (1, constant `A); (3, constant `B) ]) 400
  in
  equal (float 0.05) 0.75 (share values (fun v -> v = `B))

let as_it_stands () =
  let direct index =
    value (Gen_engine.sample Gen.int (snd (Seed.below ~bound:1L (state index))))
  in
  equal (list int) (List.init 10 direct)
    (samples Gen.(frequency [ (1, int) ]) 10)

let such_that_draws () =
  let draws = ref 0 in
  let counted =
    Gen_engine.make ~pp:Format.pp_print_int (fun state ->
        incr draws;
        Gen_engine.run Gen.int state)
  in
  equal string "discarded"
    (ended (fun () -> sample (Gen.such_that (fun _ -> false) counted) 0));
  equal int 100 !draws

let choice_shrink =
  let printed gen = Gen.with_pp Format.pp_print_int gen in
  [
    Shrinks
      ( "such_that even int",
        (fun () -> Gen.(such_that even int)),
        (fun v -> v <> 0),
        "0" );
    Shrinks
      ( "bind whose candidate 1 discards, at 2",
        (fun () ->
          printed
            Gen.(
              bind (int_range 1 10) (fun n ->
                  if n = 1 then such_that (fun _ -> false) nat else constant n))),
        (fun v -> v >= 3),
        "2" );
  ]

type shape = Circle of float | Rect of float * float

let shapes () =
  Gen.(
    one_of
      [
        map (fun r -> Circle r) (float_range 0.0 100.0);
        map
          (fun (w, h) -> Rect (w, h))
          (pair (float_range 0.0 100.0) (float_range 0.0 100.0));
      ])

let pp_shape ppf = function
  | Circle r -> Format.fprintf ppf "Circle %g" r
  | Rect (w, h) -> Format.fprintf ppf "Rect (%g, %g)" w h

let exact = Pp.to_string Pp.float_exact

let choice_renderings =
  let printed () = Gen.(one_of [ int_range 0 9; int_range 100 199 ]) in
  let mixed () = Gen.(one_of [ with_pp pp_n (constant 5); constant 9 ]) in
  [
    Rendered
      ( "one_of over printing branches",
        printed,
        (fun v -> v >= 100),
        Root,
        string_of_int );
    Rendered
      ( "one_of over printing branches, a candidate",
        printed,
        (fun v -> v >= 100),
        First,
        string_of_int );
    Rendered
      ( "frequency over printing branches",
        (fun () -> Gen.(frequency [ (1, nat); (3, int_range 100 199) ])),
        always,
        Root,
        string_of_int );
    Rendered
      ("a branch with with_pp", mixed, (fun v -> v = 5), Root, fun _ -> "N=5");
    Rendered
      ( "a branch without printer",
        mixed,
        (fun v -> v = 9),
        Root,
        fun _ -> placeholder );
    Rendered
      ( "a map branch, as its pre-image",
        shapes,
        (function Rect _ -> true | Circle _ -> false),
        Root,
        function
        | Rect (w, h) -> strf "from (%s, %s)" (exact w) (exact h)
        | Circle r -> exact r );
    Rendered
      ( "a pair over one_of",
        (fun () -> Gen.(pair (printed ()) nat)),
        always,
        Root,
        fun (a, b) -> strf "(%d, %d)" a b );
  ]

let choice_printers =
  [
    Printer
      ( "one_of over printing branches",
        (fun () -> Gen.(one_of [ int_range 0 9; int_range 100 199 ])),
        5,
        "5" );
    Printer
      ( "frequency over printing branches",
        (fun () -> Gen.(frequency [ (1, nat); (3, int_range 100 199) ])),
        7,
        "7" );
    Printer
      ( "one_of with a printerless branch",
        (fun () -> Gen.(one_of [ with_pp pp_n (constant 5); constant 9 ])),
        5,
        placeholder );
    Printer
      ( "frequency with a printerless branch",
        (fun () -> Gen.(frequency [ (1, nat); (1, constant 5) ])),
        5,
        placeholder );
  ]

(* The quotes sort first. *)
let next_to_aaa index =
  let gen = not_aaa () in
  let minimum =
    shrinks_to ~from:(String.equal (value (sample gen index))) gen
  in
  equal string {|""aab|}
    (String.of_seq
       (List.to_seq
          (List.sort Char.compare (List.of_seq (String.to_seq minimum)))))

let choices =
  group "Constants, choices and filters"
    [
      cases "a generator of one value draws it, with no candidates"
        ~name:(fun (One (n, _, _, _)) -> n)
        ones draws_one;
      test "of_list draws each value, and shrinks toward its head, by position"
        of_list_values;
      test "one_of draws each branch" (fun () ->
          equal (list int) [ 1; 2 ]
            (List.sort_uniq Int.compare
               (samples Gen.(one_of [ constant 1; constant 2 ]) 100)));
      test
        "one_of offers the earlier branches first, then the drawn value's \
         candidates"
        one_of_candidates;
      test "a one_of branch whose value discards is skipped" (fun () ->
          let gen =
            Gen.(one_of [ such_that (fun _ -> false) nat; constant 7 ])
          in
          equal (pair string int) ("7", 0)
            (shrink
               ~from:(fun v -> v = 7)
               (Gen.with_pp Format.pp_print_int gen)));
      test "frequency draws a branch in proportion to its weight" weights;
      test "frequency never draws a branch of weight 0" (fun () ->
          equal (list int) [ 2 ]
            (List.sort_uniq Int.compare
               (samples
                  Gen.(frequency [ (0, constant 1); (1, constant 2) ])
                  100)));
      test "frequency runs the drawn generator on the state as it stands"
        as_it_stands;
      test
        "the candidates of frequency's value are those of the generator that \
         drew it" (fun () ->
          let gen =
            Gen.(frequency [ (1, int_range 1000 2000); (1, int_range 1 100) ])
          in
          mem int 1 (candidate_values (find gen (fun v -> v > 2 && v <= 100))));
      renderings "a choice's value renders as the branch that drew it renders"
        choice_renderings;
      printers
        "a choice has the first branch's printer iff every branch has one"
        choice_printers;
      cases "such_that discards when no draw satisfies its predicate"
        ~name:(fun (Any (n, _)) -> n)
        [
          Any ("such_that", fun () -> Gen.(such_that (fun _ -> false) nat));
          Any
            ( "a list sized by it",
              fun () -> Gen.(list ~size:(such_that (fun _ -> false) nat) nat) );
        ]
        (fun (Any (_, gen)) ->
          let gen = gen () in
          equal string "discarded" (ended (fun () -> sample gen 0)));
      test "such_that draws at most 100 times, the first included"
        such_that_draws;
      shrinks
        "such_that drops a candidate that fails its predicate, with its subtree"
        choice_shrink;
      cases
        "a search over such_that (<> \"aaa\") on 3 letters stops next to \
         \"aaa\""
        ~name:(strf "state %d") [ 0; 1; 2; 3; 4 ] next_to_aaa;
      test
        "a search over a frequency of letters and of_list stops at one \
         character" (fun () ->
          let gen = identifier () in
          let root = value (sample gen 0) in
          mem string
            (shrinks_to ~from:(String.equal root) gen)
            [ {|"a"|}; {|"-"|} ]);
    ]

(* Composition *)

let map_forcing () =
  let calls = ref 0 in
  let counting v =
    incr calls;
    v
  in
  let tree = sample (Gen.map counting Gen.int) 3 in
  let at_sampling = !calls in
  let count = Seq.length (Shrink_tree.children tree) in
  ignore (Seq.length (Shrink_tree.children tree));
  equal (list int) [ 1; 1 + count ] [ at_sampling; !calls ]

let map_raises () =
  raises Exit (fun () -> sample (Gen.map (fun _ -> raise Exit) Gen.int) 3);
  let tree =
    sample (Gen.map (fun v -> if v = 0 then raise Exit else v) Gen.int) 3
  in
  raises Exit (fun () -> Shrink_tree.children tree ())

let discarding_map () =
  let keep_even n =
    assume (even n);
    n
  in
  let gen = Gen.map keep_even Gen.nat in
  let rec draw index =
    match sample gen index with
    | tree when value tree >= 10 -> tree
    | _ | (exception Failure.Control `Discard) -> draw (index + 1)
  in
  let values = nodes ~limit:500 (draw 0) in
  equal (list int) [] (List.filter (fun n -> not (even n)) values);
  greater int ~than:1 (List.length values)

let composition_shrink =
  [
    Shrinks
      ( "bind into a list of the drawn length",
        (fun () ->
          Gen.(
            let* n = int_range 1 3 in
            list ~size:(constant n) nat)),
        always,
        "[0]" );
    Shrinks
      ( "a let+ and+ sum, as the pre-image of 0",
        (fun () ->
          Gen.(
            let+ a = nat and+ b = nat in
            a + b)),
        (fun v -> v > 0),
        "from (0, 0)" );
  ]

let chain l = String.concat "; " (List.map string_of_int l)

let composition_renderings =
  let custom_map () = Gen.with_pp pp_n (Gen.map succ Gen.int) in
  let printed_of_list () =
    Gen.(with_pp Format.pp_print_int (of_list [ 10; 20; 30 ]))
  in
  let overridden () = Gen.(with_pp pp_n (one_of [ int_range 0 9; nat ])) in
  let chained () =
    Gen.(
      let* n = int_range 1 3 in
      let+ xs = list ~size:(constant n) (int_range 7 7) in
      (n, xs))
  in
  [
    Rendered
      ( "map succ int",
        (fun () -> Gen.(map succ int)),
        always,
        Root,
        fun v -> strf "from %d" (v - 1) );
    Rendered
      ( "map succ int, a candidate",
        (fun () -> Gen.(map succ int)),
        (fun v -> v <> 1),
        First,
        fun v -> strf "from %d" (v - 1) );
    Rendered
      ( "a map over a printed of_list",
        (fun () -> Gen.map (fun v -> (v, ())) (printed_of_list ())),
        (fun (v, ()) -> v = 30),
        Root,
        fun _ -> "from 30" );
    Rendered
      ( "a map over a printing one_of",
        (fun () ->
          Gen.(map Fun.id (one_of [ int_range 0 9; int_range 100 199 ]))),
        always,
        Root,
        strf "from %d" );
    Rendered
      ( "a map over with_pp",
        (fun () -> Gen.map (fun n -> -n) (custom_map ())),
        always,
        Root,
        fun v -> strf "from N=%d" (-v) );
    Rendered
      ( "a map over a with_pp choice",
        (fun () -> Gen.map Fun.id (overridden ())),
        always,
        Root,
        strf "from N=%d" );
    Rendered ("with_pp over map succ int", custom_map, always, Root, strf "N=%d");
    Rendered
      ( "with_pp over a map over with_pp",
        (fun () -> Gen.with_pp pp_n (Gen.map (fun n -> -n) (custom_map ()))),
        always,
        Root,
        strf "N=%d" );
    Rendered ("with_pp over a choice", overridden, always, Root, strf "N=%d");
    Rendered
      ( "with_pp over a choice of maps",
        (fun () -> Gen.with_pp pp_shape (shapes ())),
        always,
        Root,
        Format.asprintf "%a" pp_shape );
    Rendered
      ( "with_pp over of_list",
        printed_of_list,
        (fun v -> v = 30),
        Root,
        fun _ -> "30" );
    Rendered
      ( "with_pp over constant",
        (fun () -> Gen.(with_pp Format.pp_print_int (constant 7))),
        always,
        Root,
        fun _ -> "7" );
    Rendered
      ( "bind into a printing generator",
        (fun () -> Gen.(bind nat (fun n -> int_range n (n + 1)))),
        always,
        Root,
        string_of_int );
    Rendered
      ( "a bind chain, outer -> inner",
        chained,
        (fun (n, _) -> n > 1),
        Root,
        fun (n, xs) -> strf "from %d -> [%s]" n (chain xs) );
    Rendered
      ( "a bind chain, a candidate",
        chained,
        (fun (n, _) -> n > 1),
        First,
        fun (n, xs) -> strf "from %d -> [%s]" n (chain xs) );
    Rendered
      ( "nested binds, left to right",
        (fun () ->
          Gen.(
            let* a = int_range 1 1 in
            let* b = int_range 2 2 in
            let+ c = int_range 3 3 in
            a + b + c)),
        always,
        Root,
        fun _ -> "from 1 -> 2 -> 3" );
    Rendered
      ( "a bind whose outer is a bind, parenthesised",
        (fun () ->
          Gen.(
            let* ab =
              let* a = int_range 1 1 in
              let+ b = int_range 2 2 in
              a + b
            in
            let+ c = int_range 3 3 in
            ab + c)),
        always,
        Root,
        fun _ -> "from (1 -> 2) -> 3" );
  ]

let composition =
  group "Composition"
    [
      test
        "map runs f on the root at sampling, and on a candidate once, when \
         forced"
        map_forcing;
      test
        "what map's f raises escapes sampling at the root, and a candidate's \
         forcing"
        map_raises;
      test
        "a map candidate that discards is skipped, and the others stay \
         reachable"
        discarding_map;
      test
        "a search over map succ int stops at the pre-image of the least value \
         above 10" (fun () ->
          equal string "from 10"
            (shrinks_to
               ~from:(fun v -> v <> 1)
               ~failing:(fun v -> v > 10)
               (Gen.map succ Gen.int)));
      test "a map over a map renders as the innermost pre-image" (fun () ->
          let gen =
            Gen.(map String.length (map String.uppercase_ascii string))
          in
          starts_with ~affix:"from \"" (shown (find gen (fun n -> n > 0))));
      test "what bind's function raises on a candidate escapes its forcing"
        (fun () ->
          let gen =
            Gen.bind (Gen.int_range 0 10) (fun v ->
                if v = 0 then raise Exit else Gen.constant v)
          in
          raises Exit (fun () ->
              Shrink_tree.children (find gen (fun v -> v > 0)) ()));
      shrinks "a composition shrinks its outer value first, then its inner one"
        composition_shrink;
      renderings
        "a map renders its argument as a pre-image, a bind its inner value, \
         with_pp its value"
        composition_renderings;
    ]

(* Shrink trees *)

let node root children = Shrink_tree.make ~root ~children:(List.to_seq children)
let leaves values = List.map Shrink_tree.leaf values
let same pp = Testable.make ~pp ~equal:( == )

let pp_tree ppf tree =
  Format.fprintf ppf "the tree of %d" (Shrink_tree.root tree)

let head_of cell =
  require_match
    (function Seq.Cons (head, _) -> Some head | Seq.Nil -> None)
    cell

let tail_of cell =
  require_match
    (function Seq.Cons (_, tail) -> Some tail | Seq.Nil -> None)
    cell

(* [dump show depth tree] is [tree] to [depth] levels, each node its root then
   its candidates in parentheses. *)
let rec dump show depth tree =
  let subtrees = if depth = 0 then [] else candidates tree in
  show (Shrink_tree.root tree)
  ^ "("
  ^ String.concat " " (List.map (dump show (depth - 1)) subtrees)
  ^ ")"

let int_tree seed = fst (Gen_engine.run (Gen.int_range 0 20) (Seed.make seed))
let show_pair (a, b) = strf "%d,%d" a b

let unforced () =
  let log = ref [] in
  let tree =
    Shrink_tree.make ~root:42 ~children:(fun () ->
        log := "children" :: !log;
        Seq.Nil)
  in
  equal int 42 (Shrink_tree.root tree);
  equal (list string) [] !log

let tail_unforced () =
  let log = ref [] in
  let tail () =
    log := "tail" :: !log;
    Seq.Nil
  in
  let tree =
    Shrink_tree.make ~root:0 ~children:(fun () ->
        log := "head" :: !log;
        Seq.Cons (Shrink_tree.leaf 1, tail))
  in
  ignore (Shrink_tree.children tree ());
  equal (list string) [ "head" ] !log

let forced_once () =
  let log = ref [] in
  let note name = log := name :: !log in
  let first = Shrink_tree.leaf 1 and second = Shrink_tree.leaf 2 in
  let tree =
    Shrink_tree.make ~root:0 ~children:(fun () ->
        note "head";
        Seq.Cons
          ( first,
            fun () ->
              note "tail";
              Seq.Cons
                ( second,
                  fun () ->
                    note "end";
                    Seq.Nil ) ))
  in
  let cells = Shrink_tree.children tree in
  let heads = [ cells (); cells () ] in
  let tails = List.map tail_of heads in
  let seconds = List.concat_map (fun tail -> [ tail (); tail () ]) tails in
  List.iter
    (fun cell ->
      ignore ((tail_of cell) ());
      ignore ((tail_of cell) ()))
    seconds;
  equal (list string) [ "head"; "tail"; "end" ] (List.rev !log);
  equal (list (same pp_tree)) [ first; first ] (List.map head_of heads);
  equal
    (list (same pp_tree))
    [ second; second; second; second ]
    (List.map head_of seconds);
  equal
    (same (fun ppf _ -> Format.pp_print_string ppf "<tail>"))
    (List.hd tails) (List.nth tails 1)

let raised_once () =
  let log = ref [] in
  let note name = log := name :: !log in
  let error = Stdlib.Failure "forced" in
  let raising =
    Shrink_tree.make ~root:0 ~children:(fun () ->
        note "head";
        raise error)
  in
  let raising_tail =
    Shrink_tree.make ~root:0 ~children:(fun () ->
        note "second head";
        Seq.Cons
          ( Shrink_tree.leaf 1,
            fun () ->
              note "second tail";
              raise error ))
  in
  let caught f = match f () with _ -> None | exception e -> Some e in
  let cells = Shrink_tree.children raising_tail in
  let tails = [ tail_of (cells ()); tail_of (cells ()) ] in
  let raised =
    List.map
      (fun f -> require_some (caught f))
      [
        Shrink_tree.children raising;
        Shrink_tree.children raising;
        List.hd tails;
        List.nth tails 1;
      ]
  in
  equal
    (list
       (same (fun ppf e -> Format.pp_print_string ppf (Printexc.to_string e))))
    [ error; error; error; error ]
    raised;
  equal
    (same (fun ppf _ -> Format.pp_print_string ppf "<tail>"))
    (List.hd tails) (List.nth tails 1);
  equal (list string) [ "second head"; "head"; "second tail" ] (List.rev !log)

let map_once () =
  let log = ref [] in
  let note name = log := name :: !log in
  let source =
    Shrink_tree.make ~root:10 ~children:(fun () ->
        note "source";
        Seq.Cons (Shrink_tree.leaf 5, Seq.empty))
  in
  let mapped =
    Shrink_tree.map
      (fun v ->
        note (strf "f %d" v);
        v + 1)
      source
  in
  let at_map = List.rev !log in
  let children = Shrink_tree.children mapped in
  let child = head_of (children ()) in
  ignore (children ());
  equal (list string) [ "f 10" ] at_map;
  equal (list string) [ "f 10"; "source"; "f 5" ] (List.rev !log);
  equal int 6 (Shrink_tree.root child)

let map_raised_once () =
  let log = ref [] in
  let note name = log := name :: !log in
  let source =
    Shrink_tree.make ~root:1 ~children:(fun () ->
        note "source";
        Seq.Cons (Shrink_tree.leaf 2, Seq.empty))
  in
  let mapped =
    Shrink_tree.map
      (fun v ->
        note (strf "f %d" v);
        if v = 2 then raise Exit else v)
      source
  in
  let children = Shrink_tree.children mapped in
  raises Exit children;
  raises Exit children;
  equal (list string) [ "f 1"; "source"; "f 2" ] (List.rev !log)

let map_laws seed =
  let tree = int_tree seed in
  let f v = v + 3 and g v = v * 2 in
  equal string
    (dump string_of_int 3 tree)
    (dump string_of_int 3 (Shrink_tree.map Fun.id tree));
  equal string
    (dump string_of_int 3 (Shrink_tree.map (fun v -> f (g v)) tree))
    (dump string_of_int 3 (Shrink_tree.map f (Shrink_tree.map g tree)))

let pair_lazily () =
  let log = ref [] in
  let note name = log := name :: !log in
  let left =
    Shrink_tree.make ~root:1 ~children:(fun () ->
        note "left head";
        Seq.Cons
          ( Shrink_tree.leaf 0,
            fun () ->
              note "left tail";
              Seq.Nil ))
  in
  let right =
    Shrink_tree.make ~root:2 ~children:(fun () ->
        note "right";
        Seq.Cons (Shrink_tree.leaf 0, Seq.empty))
  in
  let cell = Shrink_tree.children (Shrink_tree.pair left right) () in
  let at_first = List.rev !log in
  ignore (tail_of cell ());
  equal (pair int int) (0, 2) (Shrink_tree.root (head_of cell));
  equal (list string) [ "left head" ] at_first;
  equal (list string) [ "left head"; "left tail"; "right" ] (List.rev !log)

let pair_natural (l, r) =
  let f v = v + 1 and g v = v * 2 in
  let left = int_tree l and right = int_tree r in
  equal string
    (dump show_pair 2
       (Shrink_tree.map
          (fun (a, b) -> (f a, g b))
          (Shrink_tree.pair left right)))
    (dump show_pair 2
       (Shrink_tree.pair (Shrink_tree.map f left) (Shrink_tree.map g right)))

let list_orders =
  [
    ( "two elements with candidates",
      (fun () -> [ node 10 (leaves [ 0; 5 ]); node 20 (leaves [ 2 ]) ]),
      ([ 10; 20 ], [ []; [ 20 ]; [ 10 ]; [ 0; 20 ]; [ 5; 20 ]; [ 10; 2 ] ]) );
    ( "five leaves: chunks of 4, 2 and 1",
      (fun () -> leaves [ 1; 2; 3; 4; 5 ]),
      ( [ 1; 2; 3; 4; 5 ],
        [
          [];
          [ 5 ];
          [ 3; 4; 5 ];
          [ 1; 2; 5 ];
          [ 2; 3; 4; 5 ];
          [ 1; 3; 4; 5 ];
          [ 1; 2; 4; 5 ];
          [ 1; 2; 3; 5 ];
          [ 1; 2; 3; 4 ];
        ] ) );
    ( "three leaves: chunks of 2 and 1",
      (fun () -> leaves [ 1; 2; 3 ]),
      ([ 1; 2; 3 ], [ []; [ 3 ]; [ 2; 3 ]; [ 1; 3 ]; [ 1; 2 ] ]) );
    ( "one element: no chunk",
      (fun () -> [ node 7 (leaves [ 3 ]) ]),
      ([ 7 ], [ []; [ 3 ] ]) );
    ("no element", (fun () -> []), ([], []));
  ]

let list_order (_, trees, expected) =
  let tree = Shrink_tree.list (trees ()) in
  equal
    (pair (list int) (list (list int)))
    expected
    (Shrink_tree.root tree, List.map Shrink_tree.root (candidates tree))

let list_defers () =
  let log = ref [] in
  let element name root =
    Shrink_tree.make ~root ~children:(fun () ->
        log := name :: !log;
        Seq.Cons (Shrink_tree.leaf 0, Seq.empty))
  in
  let cells =
    Shrink_tree.children
      (Shrink_tree.list [ element "first" 1; element "second" 2 ])
  in
  ignore (List.of_seq (Seq.take 3 cells));
  let at_structure = !log in
  let reduced = head_of (Seq.drop 3 cells ()) in
  equal (list string) [] at_structure;
  equal (list int) [ 0; 2 ] (Shrink_tree.root reduced);
  equal (list string) [ "first" ] !log

let list_scans_once () =
  let log = ref [] in
  let note name = log := name :: !log in
  let first =
    Shrink_tree.make ~root:1 ~children:(fun () ->
        note "first";
        Seq.Nil)
  in
  let second =
    Shrink_tree.make ~root:2 ~children:(fun () ->
        note "second";
        Seq.Cons (Shrink_tree.leaf 0, Seq.empty))
  in
  let elements =
    Seq.drop 3 (Shrink_tree.children (Shrink_tree.list [ first; second ]))
  in
  let a = head_of (elements ()) and b = head_of (elements ()) in
  equal (list string) [ "first"; "second" ] (List.rev !log);
  equal
    (same (fun ppf t ->
         Format.fprintf ppf "the tree of [%s]" (ints (Shrink_tree.root t))))
    a b;
  equal (list int) [ 1; 0 ] (Shrink_tree.root a)

let long_list () =
  let tree = Shrink_tree.list (List.init 100_000 Shrink_tree.leaf) in
  equal int 100_000 (List.length (Shrink_tree.root tree));
  equal (list int) [] (Shrink_tree.root (first_candidate tree))

let infinite_depth () =
  let forced = ref 0 in
  let rec ascending v =
    Shrink_tree.make ~root:v ~children:(fun () ->
        incr forced;
        Seq.Cons (ascending (v + 1), Seq.empty))
  in
  let rec roots depth tree acc =
    let acc = Shrink_tree.root tree :: acc in
    if depth = 1 then List.rev acc
    else roots (depth - 1) (first_candidate tree) acc
  in
  let depth = 50_000 in
  equal (list int) (List.init depth succ)
    (roots depth (Shrink_tree.map succ (ascending 0)) []);
  equal int (depth - 1) !forced

(* [n]'s candidates are [0] then [n - 1]. *)
let local_minimum () =
  let rec count_down v =
    Shrink_tree.make ~root:v ~children:(fun () ->
        if v = 0 then Seq.Nil
        else
          Seq.Cons
            ( Shrink_tree.leaf 0,
              fun () -> Seq.Cons (count_down (v - 1), Seq.empty) ))
  in
  let start = Shrink_tree.list (List.map count_down [ 9; 7; 5; 9 ]) in
  let pp ppf values = Format.fprintf ppf "[%s]" (ints values) in
  let gen = Gen_engine.make ~pp (fun state -> (start, state)) in
  let minimum, steps = shrink ~failing:(List.exists (fun v -> v >= 5)) gen in
  equal string "[5]" minimum;
  at_most int ~than:20 steps

let list_root_unforced () =
  let forced = ref 0 in
  let element root =
    Shrink_tree.make ~root ~children:(fun () ->
        incr forced;
        Seq.Nil)
  in
  let tree = Shrink_tree.list [ element 1; element 2; element 3 ] in
  equal (list int) [ 1; 2; 3 ] (Shrink_tree.root tree);
  equal int 0 !forced

(* Every operation of the module, then one draw of the global state against a
   copy taken before them: a random operation would have moved it. *)
let no_random () =
  let before = Random.get_state () in
  let rec count_down n =
    Shrink_tree.make ~root:n ~children:(fun () ->
        if n = 0 then Seq.Nil else Seq.Cons (count_down (n - 1), Seq.empty))
  in
  let tree =
    Shrink_tree.pair
      (Shrink_tree.map succ (count_down 4))
      (Shrink_tree.list [ count_down 2; Shrink_tree.leaf 9 ])
  in
  let rec visit tree = Seq.iter visit (Shrink_tree.children tree) in
  visit tree;
  equal int (Random.State.bits before) (Random.bits ())

let shrink_trees =
  group "Shrink trees"
    [
      test "make and root force no cell" unforced;
      test "a leaf has its root and no candidates" (fun () ->
          let leaf = Shrink_tree.leaf 7 in
          equal
            (pair int (list int))
            (7, [])
            (Shrink_tree.root leaf, List.map Shrink_tree.root (candidates leaf)));
      test "forcing a cell forces none of its tail" tail_unforced;
      test
        "every cell is forced at most once, and yields the same child each time"
        forced_once;
      test
        "a cell whose forcing raised raises the same exception again, without \
         forcing again"
        raised_once;
      test "map keeps the shape and the order" (fun () ->
          let tree = node 10 [ node 4 (leaves [ 0; 2 ]); node 8 [] ] in
          equal string "30(12(0() 6()) 24())"
            (dump string_of_int 3 (Shrink_tree.map (fun v -> v * 3) tree)));
      prop "map obeys identity and composition" ~count:20 Gen.int64 map_laws;
      test
        "map applies f to the root at once, and to a candidate once, when \
         forced"
        map_once;
      test "what map's f raises on a candidate is cached with its cell"
        map_raised_once;
      test "pair reduces the left tree, then the right" (fun () ->
          let tree =
            Shrink_tree.pair
              (node 10 (leaves [ 0; 5 ]))
              (node 20 (leaves [ 2; 4 ]))
          in
          equal
            (pair (pair int int) (list (pair int int)))
            ((10, 20), [ (0, 20); (5, 20); (10, 2); (10, 4) ])
            (Shrink_tree.root tree, List.map Shrink_tree.root (candidates tree)));
      test "pair forces the right tree only once the left one is exhausted"
        pair_lazily;
      prop "pair of mapped trees is the map of their pair" ~count:20
        Gen.(pair int64 int64)
        pair_natural;
      cases
        "list's candidates are the empty list, the chunks removed, then the \
         elements reduced"
        ~name:(fun (n, _, _) -> n)
        list_orders list_order;
      test "list's root forces no cell" list_root_unforced;
      test "list forces no element before its chunks are exhausted" list_defers;
      test "list forces each element's cells once across its candidates"
        list_scans_once;
      test "list's root over 100_000 trees builds without overflowing the stack"
        long_list;
      test "a tree of any depth forces one cell a level" infinite_depth;
      test "the search over a list tree stops at a local minimum, in few steps"
        local_minimum;
      test "no operation reads or writes the global Random state" no_random;
    ]

(* Rendering *)

let printers_rows =
  [
    Printer ("int", (fun () -> Gen.int), 42, "42");
    Printer ("unit", (fun () -> Gen.unit), (), "()");
    Printer
      ( "such_that, the underlying printer",
        (fun () -> Gen.(such_that even int)),
        4,
        "4" );
    Printer
      ("with_pp", (fun () -> Gen.with_pp pp_n (Gen.map succ Gen.int)), 7, "N=7");
    Printer
      ( "with_pp over a choice",
        (fun () -> Gen.(with_pp pp_n (one_of [ int_range 0 9; nat ]))),
        5,
        "N=5" );
    Printer
      ( "with_pp over of_list",
        (fun () -> Gen.(with_pp Format.pp_print_int (of_list [ 10; 20; 30 ]))),
        20,
        "20" );
    Printer
      ( "with_pp over a map",
        (fun () -> Gen.(with_pp Format.pp_print_int (map succ int))),
        3,
        "3" );
    Printer ("map", (fun () -> Gen.(map succ int)), 3, placeholder);
    Printer
      ( "map over a printed of_list",
        (fun () ->
          Gen.(
            map
              (fun v -> (v, ()))
              (with_pp Format.pp_print_int (of_list [ 10; 20; 30 ])))),
        (30, ()),
        placeholder );
    Printer
      ( "float, an example it never draws",
        (fun () -> Gen.float),
        Float.neg_infinity,
        "neg_infinity" );
    Printer
      ( "float_range, an example it never draws",
        (fun () -> Gen.float_range 0. 1.),
        Float.nan,
        "nan" );
    Printer ("constant", (fun () -> Gen.constant 42), 42, placeholder);
    Printer ("of_list", (fun () -> Gen.of_list [ 10; 20; 30 ]), 20, placeholder);
  ]

let nothing_rows =
  let nothing _ = placeholder in
  [
    Rendered ("constant", (fun () -> Gen.constant 42), always, Root, nothing);
    Rendered
      ( "of_list",
        (fun () -> Gen.of_list [ 10; 20; 30 ]),
        (fun v -> v = 30),
        Root,
        nothing );
    Rendered
      ( "one_of over constants",
        (fun () -> Gen.(one_of [ constant 1; constant 2 ])),
        (fun v -> v = 2),
        Root,
        nothing );
    Rendered
      ( "frequency over constants",
        (fun () -> Gen.(frequency [ (1, constant 1); (3, constant 2) ])),
        (fun v -> v = 2),
        Root,
        nothing );
    Rendered
      ( "a map over a pair with a constant",
        (fun () -> Gen.(map Fun.id (pair (constant 'k') nat))),
        always,
        Root,
        nothing );
    Rendered
      ( "a bind into a constant",
        (fun () -> Gen.(bind nat (fun n -> constant n))),
        always,
        Root,
        nothing );
    Rendered
      ( "a bind into a map over a constant",
        (fun () ->
          Gen.(bind nat (fun n -> map (fun c -> (c, n)) (constant 'k')))),
        always,
        Root,
        nothing );
  ]

let raising exn = Gen.with_pp (fun _ _ -> raise exn) Gen.nat

let raised_rows =
  [
    ("Failure", Stdlib.Failure "boom", {|<printer raised Failure("boom")>|});
    ( "a timeout",
      Failure.Control (`Timeout 1.5),
      "<printer raised windtrap timeout after 1.5s>" );
    ("Stack_overflow", Stack_overflow, "<printer raised Stack overflow>");
  ]

let formats_again () =
  let calls = ref 0 in
  let counting ppf v =
    incr calls;
    Format.pp_print_int ppf v
  in
  let tree = sample (Gen.with_pp counting Gen.int) 0 in
  let at_sampling = !calls in
  ignore (shown tree);
  ignore (shown tree);
  let long =
    Gen.with_pp
      (Pp.brackets (Pp.list Pp.int))
      (Gen.constant (List.init 40 (fun i -> 1000 + i)))
  in
  let lines = String.split_on_char '\n' (shown (sample long 0)) in
  equal (list int) [ 0; 2 ] [ at_sampling; !calls ];
  greater int ~than:1 (List.length lines);
  at_most int ~than:78 (greatest (List.map String.length lines))

let never_empty (Any (_, gen)) =
  let gen = gen () in
  let texts =
    List.concat_map
      (fun i -> visit ~limit:50 shown (sample gen i))
      [ 0; 1; 2; 3; 4 ]
  in
  equal (list string) [] (List.filter (String.equal "") texts)

(* [prints] says whether [render] gives the placeholder, and formats
   nothing to say it. *)
let prints_rows =
  [
    Any ("int", fun () -> Gen.int);
    Any ("unit", fun () -> Gen.unit);
    Any ("a printerless map, as its pre-image", fun () -> Gen.(map succ int));
    Any ("with_pp over a constant", fun () -> Gen.(with_pp pp_n (constant 3)));
    Any ("a raising printer", fun () -> raising Not_found);
    Any ("constant", fun () -> Gen.constant 42);
    Any ("of_list", fun () -> Gen.of_list [ 10; 20; 30 ]);
    Any ("a map over a constant", fun () -> Gen.(map succ (constant 1)));
  ]

let prints_iff_rendered (Any (_, gen)) =
  let tree = sample (gen ()) 0 in
  equal bool
    (not (String.equal placeholder (shown tree)))
    (Gen_engine.prints (Shrink_tree.root tree))

let prints_formats_nothing () =
  let calls = ref 0 in
  let counting ppf v =
    incr calls;
    Format.pp_print_int ppf v
  in
  let tree = sample (Gen.with_pp counting Gen.int) 0 in
  let before = !calls in
  is_true (Gen_engine.prints (Shrink_tree.root tree));
  equal int before !calls

let rendering =
  group "Rendering"
    [
      printers
        "render_value prints with the generator's printer, and never a node's \
         rendering"
        printers_rows;
      renderings "a sample with nothing to print renders as the placeholder"
        nothing_rows;
      cases "a raising printer renders as <printer raised EXN>, sampled or bare"
        ~name:(fun (n, _, _) -> n)
        raised_rows
        (fun (_, exn, text) ->
          equal (pair string string) (text, text)
            ( shown (sample (raising exn) 0),
              Gen_engine.render_value (raising exn) 3 ));
      cases "Sys.Break and Out_of_memory escape a printer's guard"
        ~name:Printexc.to_string [ Sys.Break; Out_of_memory ] (fun exn ->
          equal string (Printexc.to_string exn)
            (ended (fun () -> shown (sample (raising exn) 0))));
      test "render formats on every call, at the default margin" formats_again;
      cases "a sample renders as a non-empty text"
        ~name:(fun (Any (n, _)) -> n)
        [
          Any ("int", fun () -> Gen.int);
          Any ("list int", fun () -> Gen.(list int));
          Any ("a choice of maps", shapes);
          Any
            ( "a map over an option of a map",
              fun () -> Gen.(map Fun.id (option (map succ nat))) );
        ]
        never_empty;
      prop "a pair of a string and a list of ints prints every value it draws"
        Gen.(pair string (list int))
        (fun v ->
          let text = Gen_engine.render_value Gen.(pair string (list int)) v in
          not_equal string "" text;
          not_equal string placeholder text);
      cases "prints is false exactly where render gives the placeholder"
        ~name:(fun (Any (n, _)) -> n)
        prints_rows prints_iff_rendered;
      test "prints formats nothing" prints_formats_nothing;
    ]

(* Building generators *)

let unprinted () =
  let gen =
    Gen_engine.make (fun state ->
        ( Shrink_tree.make ~root:1 ~children:(Seq.return (Shrink_tree.leaf 0)),
          state ))
  in
  let tree = sample gen 0 in
  equal (list string)
    [ placeholder; placeholder ]
    [ shown tree; shown (first_candidate tree) ]

let building =
  group "Building generators"
    [
      test "make without pp gives every node nothing to print" unprinted;
      test
        "draw returns the tree that sample draws and the state that run returns"
        (fun () ->
          let gen = Gen.(list ~size:(int_range 0 4) small_int) in
          let drawn i = Gen_engine.draw gen (state i) in
          equal
            (list (pair (list int) (list int64)))
            (List.init 10 (fun i ->
                 (value (sample gen i), words 3 (successor gen (state i)))))
            (List.init 10 (fun i ->
                 let tree, next = drawn i in
                 (value tree, words 3 next))));
      test "run returns the values that sample draws" (fun () ->
          let gen = Gen.(list ~size:(int_range 0 4) small_int) in
          equal
            (list (list int))
            (samples gen 10)
            (List.init 10 (fun i ->
                 Shrink_tree.root (fst (Gen_engine.run gen (state i))))));
    ]

let () =
  exit
    (run "gen"
       [
         generators;
         numbers;
         base;
         containers;
         choices;
         composition;
         shrink_trees;
         rendering;
         building;
       ])
