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
module Choice = Windtrap.Private.Choice
module Shrink = Windtrap.Private.Shrink
module Property = Windtrap.Private.Property
module Failure = Windtrap.Private.Failure
module Pp = Windtrap.Private.Pp

let strf = Printf.sprintf
let root = 0x00c0ffee1234abcdL
let path = "test_gen"
let state index = Seed.make (Seed.derive ~root ~path ~index)

(* A sample with its candidates as a search reads them: the edits of one step
   of the reducer whose parse reads a shortlex smaller sequence, each
   sequence once, parsed when first asked for. An edit whose parse discards
   or raises a fault is none. *)
type 'a tree = { node : 'a Gen_engine.sample; children : 'a tree Seq.t }

let case_of src : Shrink.case =
  {
    choices = Choice.choices src;
    spans = Choice.spans src;
    dependents = Choice.dependents src;
  }

let rec tree_of gen (case : Shrink.case) node =
  let seen = Hashtbl.create 16 in
  let candidate choices =
    let src = Choice.replaying choices in
    match Failure.catch (fun () -> Gen_engine.parse gen src) with
    | Ok node
      when Shrink.shortlex (Choice.choices src) case.choices < 0
           && not (Hashtbl.mem seen (Choice.choices src)) ->
        Hashtbl.add seen (Choice.choices src) ();
        Some (tree_of gen (case_of src) node)
    | Ok _ | Error (`Discard | #Failure.fault) -> None
    | Error c -> Failure.reraise c
  in
  {
    node;
    children = Seq.memoize (Seq.filter_map candidate (Shrink.candidates case));
  }

(* The tree of the value that [gen] draws from [state], and the state after
   it. *)
let drawn gen state =
  let src = Choice.generating state in
  let node = Gen_engine.parse gen src in
  (tree_of gen (case_of src) node, Option.get (Choice.state src))

let sample gen index = fst (drawn gen (state index))
let at_seed gen seed = fst (drawn gen (Seed.make seed))
let value tree = Gen_engine.value tree.node
let samples gen count = List.init count (fun index -> value (sample gen index))
let candidates tree = List.of_seq tree.children
let candidate_values tree = List.map value (candidates tree)

let first_candidate tree =
  fst (require_some ~msg:"a candidate" (Seq.uncons tree.children))

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

let successor gen state = snd (drawn gen state)
let placeholder = "<no printer: attach one with Gen.with_pp>"
let pp_n ppf n = Format.fprintf ppf "N=%d" n
let ints values = String.concat "; " (List.map string_of_int values)

(* A pre-image is marked as the report marks it, so an assertion on a value
   cannot pass on one. *)
let shown tree =
  match Gen_engine.render tree.node with
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
    Seq.iter visit tree.children
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
      tree.children
  in
  (try visit tree with Exit -> ());
  List.rev !seen

let counterexample = function
  | Property.Fail { failure = { Failure.kind = Property p; _ }; _ } ->
      let text =
        match p.rendering with
        | Ocaml | Value -> p.rendered.kept
        | Pre_image -> "from " ^ p.rendered.kept
      in
      let text =
        match p.shrink_end with
        | Converged -> text
        | Budget_spent | Timed_out _ -> text ^ " (not minimal)"
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

(* A domain of at most the count is checked whole and nothing shrinks, so a
   search over a small range draws an [int64] beside the value: the domain
   is then endless, and [gen]'s printer [pp] prints the value. *)
let endless pp gen = Gen.with_pp pp (Gen.map fst (Gen.pair gen Gen.int64))
let pp_char ppf c = Format.fprintf ppf "%C" c

(* A discard and the exceptions [Failure.catch] never returns escape the
   verbs, which run the code under that guard, so they are caught here. *)
let ended f =
  match f () with
  | _ -> "returned"
  | exception Failure.Control `Discard -> "discarded"
  | exception e -> Printexc.to_string e

let always _ = true
let is_negative_zero v = v = 0. && Float.sign_bit v

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
    ~id:(fun (Shrinks (name, _, _, _)) -> name)
    rows
    (fun (Shrinks (_, gen, from, expected)) ->
      equal string expected (shrinks_to ~from (gen ())))

let literals claim rows =
  cases claim
    ~id:(fun (Literal (name, _, _)) -> name)
    rows
    (fun (Literal (_, gen, print)) ->
      let trees = List.init 60 (sample (gen ())) in
      equal (list string)
        (List.map (fun tree -> print (value tree)) trees)
        (List.map shown trees))

let renderings claim rows =
  cases claim
    ~id:(fun (Rendered (name, _, _, _, _)) -> name)
    rows
    (fun (Rendered (_, gen, accept, pick, expected)) ->
      let tree = find (gen ()) accept in
      let node =
        match pick with Root -> tree | First -> first_candidate tree
      in
      equal string (expected (value node)) (shown node))

let printers claim rows =
  cases claim
    ~id:(fun (Printer (name, _, _, _)) -> name)
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
      ( "float_range (-1.) (-0.), never 0.",
        (fun () -> Gen.float_range (-1.) (-0.)),
        fun _ -> within float_exact (-1.) (-0.) );
    Kept
      ( "float, finite",
        (fun () -> Gen.float),
        fun _ -> satisfies ~claim:"finite" float_exact Float.is_finite );
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
      ( "result, whose payload is a value of its side's generator",
        (fun () -> Gen.(result nat (int_range 20_000 20_009))),
        fun _ -> function
          | Ok n -> within int 0 9_999 n | Error n -> within int 20_000 20_009 n
      );
    Kept
      ( "either, whose payload is a value of its side's generator",
        (fun () -> Gen.(either nat (int_range 20_000 20_009))),
        fun _ -> function
          | Either.Left n -> within int 0 9_999 n
          | Right n -> within int 20_000 20_009 n );
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
  let draw () = sample gen 0 in
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
    if depth = 0 then [] else List.of_seq (Seq.take 4 tree.children)
  in
  let order = if reversed then List.rev else Fun.id in
  let laid = order (List.map (layout ~reversed (depth - 1)) (order subtrees)) in
  shown tree ^ "(" ^ String.concat " " laid ^ ")"

let forced_in_any_order (Any (_, gen)) =
  let gen = gen () in
  let index = find_index gen (fun tree -> not (Seq.is_empty tree.children)) in
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
    int: 70368744177663
    nat: 254
    float: 3.626460398842157e-116
    string_of: "emev"
    list of small_int: [1809; 3; -7; 0; 2]
    option, one_of and frequency: (Some (false), 9, 0)
    list of the default length: [false; false]
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

(* The distributions. Each row draws 2,000 values from the states that
   [sample] reads and summarises them: a numeric projection's mean, standard
   deviation, extremes and an eight-bin histogram between them, the number
   of distinct printings and a digest of the printings in draw order. The
   samplers consume a frozen stream, so every row is exact and the tolerance
   is zero: a changed bin is a changed distribution.

   What each one is for. A whole integer type comes small or at an edge
   first, as the small scope hypothesis argues (a program that fails almost
   always fails in some simple case): a side of at most 256 values is
   uniform, a wider one draws half its distances below 256 and one in four at
   an edge of a power of two. A range is uniform, since it states the domain
   its values come from. One draw in ten is a corner. [nat]
   and [small_int] keep their strata, which favour small values too. The
   lengths of a collection keep theirs, small for cost and for small
   counterexamples, with a stratum of 1% below 256 so that no length below
   it is out of reach (a cap at 63 made a law that fails only on longer
   lists pass at any count). A float spreads over its bit patterns. A call
   repeats an earlier value of its generator in the case one time in eight,
   so that a collection holds duplicates and two components collide;
   [in_case] below pins what that does inside a case, which a summary of one
   value cannot see. *)
type summary =
  | Summary : string * (unit -> 'a Gen.t) * ('a -> float) -> summary

let summarise (Summary (name, gen, project)) =
  let gen = gen () in
  let trees = List.init 2_000 (sample gen) in
  let xs = List.map (fun tree -> project (value tree)) trees in
  let texts = List.map shown trees in
  let n = float_of_int (List.length xs) in
  let mean = List.fold_left ( +. ) 0. xs /. n in
  let variance =
    List.fold_left (fun acc x -> acc +. ((x -. mean) ** 2.)) 0. xs /. n
  in
  let low = List.fold_left Float.min Float.infinity xs in
  let high = List.fold_left Float.max Float.neg_infinity xs in
  let bins = Array.make 8 0 in
  List.iter
    (fun x ->
      let bin =
        if high = low then 0
        else Float.to_int (8. *. ((x -. low) /. (high -. low)))
      in
      bins.(Int.min 7 bin) <- bins.(Int.min 7 bin) + 1)
    xs;
  let distinct = List.length (List.sort_uniq String.compare texts) in
  strf "%s\n  mean %.4g, sd %.4g, in [%.4g;%.4g]\n  bins %s, %d distinct, %s"
    name mean (Float.sqrt variance) low high
    (String.concat " " (Array.to_list (Array.map string_of_int bins)))
    distinct
    (String.sub (Digest.to_hex (Digest.string (String.concat "\n" texts))) 0 8)

let int_summary name gen = Summary (name, gen, float_of_int)
let size_of length v = float_of_int (length v)

let summaries =
  Gen.
    [
      int_summary "int" (fun () -> int);
      int_summary "nat" (fun () -> nat);
      int_summary "small_int" (fun () -> small_int);
      int_summary "int_range -5 5" (fun () -> int_range (-5) 5);
      int_summary "int_range 3 1000" (fun () -> int_range 3 1000);
      Summary ("int32", (fun () -> int32), Int32.to_float);
      Summary
        ("int32_range -7l 9l", (fun () -> int32_range (-7l) 9l), Int32.to_float);
      Summary ("int64", (fun () -> int64), Int64.to_float);
      Summary
        ( "int64_range min_int max_int",
          (fun () -> int64_range Int64.min_int Int64.max_int),
          Int64.to_float );
      Summary
        ("int64_range 0L 99L", (fun () -> int64_range 0L 99L), Int64.to_float);
      Summary ("nativeint", (fun () -> nativeint), Nativeint.to_float);
      Summary
        ( "nativeint_range -3n 3n",
          (fun () -> nativeint_range (-3n) 3n),
          Nativeint.to_float );
      Summary
        ( "float, by binary exponent",
          (fun () -> float),
          fun x -> float_of_int (snd (Float.frexp x)) );
      Summary
        ( "any_float, by class",
          (fun () -> any_float),
          fun x ->
            if Float.is_finite x then 0. else if Float.is_nan x then 2. else 1.
        );
      Summary ("float_range -1. 2.", (fun () -> float_range (-1.) 2.), Fun.id);
      Summary
        ("float_range -3. -0.", (fun () -> float_range (-3.) (-0.)), Fun.id);
      Summary
        ( "float_range 1e300 max_float",
          (fun () -> float_range 1e300 Float.max_float),
          Fun.id );
      Summary ("unit", (fun () -> unit), fun () -> 0.);
      Summary ("bool", (fun () -> bool), fun b -> if b then 1. else 0.);
      Summary ("char", (fun () -> char), size_of Char.code);
      Summary
        ("char_range 'a' 'f'", (fun () -> char_range 'a' 'f'), size_of Char.code);
      Summary ("uchar", (fun () -> uchar), size_of Uchar.to_int);
      Summary ("string", (fun () -> string), size_of String.length);
      Summary
        ( "string_of ~size:(int_range 0 6) (char_range 'a' 'c')",
          (fun () -> string_of ~size:(int_range 0 6) (char_range 'a' 'c')),
          size_of String.length );
      Summary ("bytes", (fun () -> bytes), size_of Bytes.length);
      Summary
        ( "bytes_of ~size:(int_range 0 3) char",
          (fun () -> bytes_of ~size:(int_range 0 3) char),
          size_of Bytes.length );
      Summary ("list bool", (fun () -> list bool), size_of List.length);
      Summary
        ("list (list nat)", (fun () -> list (list nat)), size_of List.length);
      Summary
        ( "list ~size:(int_range 2 4) small_int, by sum",
          (fun () -> list ~size:(int_range 2 4) small_int),
          size_of (List.fold_left ( + ) 0) );
      Summary ("array nat", (fun () -> array nat), size_of Array.length);
      Summary
        ( "option nat",
          (fun () -> option nat),
          function None -> -1. | Some n -> float_of_int n );
      Summary
        ( "result nat bool",
          (fun () -> result nat bool),
          function Ok n -> float_of_int n | Error _ -> -1. );
      Summary
        ( "either nat bool",
          (fun () -> either nat bool),
          function Either.Left n -> float_of_int n | Right _ -> -1. );
      Summary
        ( "pair nat small_int, by difference",
          (fun () -> pair nat small_int),
          fun (a, b) -> float_of_int (a - b) );
      Summary
        ( "triple bool nat char, by sum",
          (fun () -> triple bool nat char),
          fun (_, n, c) -> float_of_int (n + Char.code c) );
      Summary
        ( "quad bool bool bool bool, by trues",
          (fun () -> quad bool bool bool bool),
          fun (a, b, c, d) ->
            size_of List.length (List.filter Fun.id [ a; b; c; d ]) );
      int_summary "constant 7" (fun () -> constant ~pp:Pp.int 7);
      int_summary "of_list [3; 1; 4; 1; 5]" (fun () ->
          of_list ~pp:Pp.int [ 3; 1; 4; 1; 5 ]);
      Summary
        ( "subsequence 0..9, by sum",
          (fun () -> subsequence ~pp:Pp.int (List.init 10 Fun.id)),
          size_of (List.fold_left ( + ) 0) );
      Summary
        ( "permutation 0..5, by head",
          (fun () -> permutation ~pp:Pp.int (List.init 6 Fun.id)),
          size_of List.hd );
      int_summary "one_of [int_range 0 9; int_range 100 109]" (fun () ->
          one_of [ int_range 0 9; int_range 100 109 ]);
      int_summary "frequency [1, 0..9; 3, 100..109; 0, 1000]" (fun () ->
          frequency
            [ (1, int_range 0 9); (3, int_range 100 109); (0, constant 1000) ]);
      int_summary "such_that even nat" (fun () -> such_that even nat);
      int_summary "map succ nat" (fun () -> map succ nat);
      int_summary "bind (int_range 0 20) (fun n -> int_range 0 n)" (fun () ->
          bind (int_range 0 20) (fun n -> int_range 0 n));
      int_summary "bind (list ~size:(int_range 1 4) nat) of_list" (fun () ->
          bind (list ~size:(int_range 1 4) nat) (of_list ~pp:Pp.int));
      int_summary "with_pp nat" (fun () -> with_pp Pp.int nat);
    ]

let distributions () =
  expect (String.concat "\n" (List.map summarise summaries))
  @@ __POS_OF__
       {|
    int
      mean -1.497e+16, sd 1.001e+18, in [-4.612e+18;4.612e+18]
      bins 47 3 11 372 1516 5 3 43, 942 distinct, 27517cb5
    nat
      mean 367.4, sd 1272, in [0;9978]
      bins 1911 7 15 19 12 13 10 13, 486 distinct, a76cc67c
    small_int
      mean -21.85, sd 1361, in [-9904;9967]
      bins 16 10 14 1553 372 10 12 13, 627 distinct, 07ca8332
    int_range -5 5
      mean -0.0855, sd 3.182, in [-5;5]
      bins 386 164 161 213 397 176 149 354, 11 distinct, 07f3ad10
    int_range 3 1000
      mean 488, sd 314.3, in [3;1000]
      bins 354 225 241 209 207 250 227 287, 812 distinct, d173cf9c
    int32
      mean -6.509e+06, sd 4.982e+08, in [-2.147e+09;2.147e+09]
      bins 52 6 19 913 943 16 3 48, 816 distinct, 55c9feef
    int32_range -7l 9l
      mean 0.8395, sd 4.982, in [-7;9]
      bins 258 239 206 301 223 213 185 375, 17 distinct, 2285489d
    int64
      mean -2.099e+15, sd 2.014e+18, in [-9.223e+18;9.223e+18]
      bins 47 2 6 368 1519 10 3 45, 950 distinct, 54e351cd
    int64_range min_int max_int
      mean -2.099e+15, sd 2.014e+18, in [-9.223e+18;9.223e+18]
      bins 47 2 6 368 1519 10 3 45, 950 distinct, 54e351cd
    int64_range 0L 99L
      mean 48.84, sd 31.48, in [0;99]
      bins 369 174 254 209 198 257 226 313, 100 distinct, 63f5c1cf
    nativeint
      mean -2.099e+15, sd 2.014e+18, in [-9.223e+18;9.223e+18]
      bins 47 2 6 368 1519 10 3 45, 950 distinct, b4ef4698
    nativeint_range -3n 3n
      mean 0.031, sd 2.05, in [-3;3]
      bins 309 270 267 0 281 270 283 320, 7 distinct, de860e77
    float, by binary exponent
      mean -18.29, sd 614.6, in [-1073;1024]
      bins 278 211 227 235 310 239 234 266, 1818 distinct, 272e5780
    any_float, by class
      mean 0.123, sd 0.4023, in [0;2]
      bins 1808 0 0 0 138 0 0 54, 1617 distinct, 0cd1de10
    float_range -1. 2.
      mean 0.4578, sd 0.8785, in [-1;2]
      bins 263 233 346 233 216 227 229 253, 1814 distinct, 419865bc
    float_range -3. -0.
      mean -1.462, sd 0.9488, in [-3;-0]
      bins 295 233 218 233 216 227 229 349, 1811 distinct, c445286b
    float_range 1e300 max_float
      mean inf, sd inf, in [1e+300;1.798e+308]
      bins 359 233 218 233 216 227 229 285, 1811 distinct, 5eec09c2
    unit
      mean 0, sd 0, in [0;0]
      bins 2000 0 0 0 0 0 0 0, 1 distinct, eab2fc4d
    bool
      mean 0.487, sd 0.4998, in [0;1]
      bins 1026 0 0 0 0 0 0 974, 2 distinct, 32f22f55
    char
      mean 127.8, sd 73.5, in [0;255]
      bins 242 257 238 253 271 238 254 247, 256 distinct, 575f596d
    char_range 'a' 'f'
      mean 99.49, sd 1.718, in [97;102]
      bins 353 310 0 335 333 0 338 331, 6 distinct, b8ea10cd
    uchar
      mean 1.468e+05, sd 2.865e+05, in [0;1.114e+06]
      bins 1571 66 68 47 65 55 57 71, 1421 distinct, cb3a7ad8
    string
      mean 5.273, sd 14.52, in [0;247]
      bins 1948 40 2 3 0 2 3 2, 1494 distinct, e4e4b612
    string_of ~size:(int_range 0 6) (char_range 'a' 'c')
      mean 2.982, sd 2.115, in [0;6]
      bins 319 344 229 0 243 237 283 345, 548 distinct, 8ed1b0fa
    bytes
      mean 5.273, sd 14.52, in [0;247]
      bins 1948 40 2 3 0 2 3 2, 1494 distinct, 5ae3e739
    bytes_of ~size:(int_range 0 3) char
      mean 1.473, sd 1.138, in [0;3]
      bins 527 0 518 0 0 437 0 518, 1176 distinct, 06a1c990
    list bool
      mean 5.273, sd 14.52, in [0;247]
      bins 1948 40 2 3 0 2 3 2, 436 distinct, f576ce6c
    list (list nat)
      mean 5.273, sd 14.52, in [0;247]
      bins 1948 40 2 3 0 2 3 2, 1530 distinct, 6f39241a
    list ~size:(int_range 2 4) small_int, by sum
      mean -12.27, sd 2564, in [-2.975e+04;2.029e+04]
      bins 2 3 3 58 1805 92 34 3, 1931 distinct, 16d09d8f
    array nat
      mean 5.273, sd 14.52, in [0;247]
      bins 1948 40 2 3 0 2 3 2, 1400 distinct, 5d114e81
    option nat
      mean 341.7, sd 1278, in [-1;9967]
      bins 1921 10 8 13 10 11 12 15, 461 distinct, 7fbb0318
    result nat bool
      mean 297.1, sd 1189, in [-1;9904]
      bins 1932 7 8 11 10 7 11 14, 420 distinct, 2e506b9e
    either nat bool
      mean 177.2, sd 915, in [-1;9967]
      bins 1958 7 4 6 6 6 6 7, 305 distinct, edd3a6d0
    pair nat small_int, by difference
      mean 368.7, sd 1771, in [-9776;9976]
      bins 13 5 16 1315 537 43 37 34, 1611 distinct, c16849cb
    triple bool nat char, by sum
      mean 505.6, sd 1310, in [1;1.021e+04]
      bins 1912 13 11 13 12 12 13 14, 1903 distinct, bf76dc1c
    quad bool bool bool bool, by trues
      mean 1.97, sd 1.105, in [0;4]
      bins 180 0 536 0 626 0 481 177, 16 distinct, b67e6d70
    constant 7
      mean 7, sd 0, in [7;7]
      bins 2000 0 0 0 0 0 0 0, 1 distinct, c576b848
    of_list [3; 1; 4; 1; 5]
      mean 2.792, sd 1.614, in [1;5]
      bins 816 0 0 0 375 0 402 407, 4 distinct, ca99eb1c
    subsequence 0..9, by sum
      mean 21.51, sd 15.35, in [0;45]
      bins 418 251 163 210 241 179 191 347, 651 distinct, a1472574
    permutation 0..5, by head
      mean 2.493, sd 1.718, in [0;5]
      bins 353 310 0 335 333 0 338 331, 681 distinct, 7f93fcda
    one_of [int_range 0 9; int_range 100 109]
      mean 53.07, sd 50.1, in [0;109]
      bins 1026 0 0 0 0 0 0 974, 20 distinct, 6305e483
    frequency [1, 0..9; 3, 100..109; 0, 1000]
      mean 78.02, sd 44.12, in [0;109]
      bins 527 0 0 0 0 0 0 1473, 20 distinct, 7bab34ee
    such_that even nat
      mean 342.1, sd 1224, in [0;9840]
      bins 1916 16 11 10 10 12 12 13, 379 distinct, aae110f1
    map succ nat
      mean 368.4, sd 1272, in [1;9979]
      bins 1911 7 15 19 12 13 10 13, 486 distinct, 0907295b
    bind (int_range 0 20) (fun n -> int_range 0 n)
      mean 4.774, sd 5.072, in [0;20]
      bins 916 270 316 129 171 71 73 54, 21 distinct, 2f43b56f
    bind (list ~size:(int_range 1 4) nat) of_list
      mean 373.4, sd 1296, in [0;9787]
      bins 1906 16 12 12 13 14 10 17, 482 distinct, 95c8a1d9
    with_pp nat
      mean 367.4, sd 1272, in [0;9978]
      bins 1911 7 15 19 12 13 10 13, 486 distinct, a76cc67c
    |}

(* What the distributions do inside a case, over 10,000 cases each, counted
   exactly. Under 0.2's uniform integers and independent draws the first four
   shares were about 5.9%, 0.15%, 6.3% and 24%: a bug that needs a small
   integer, two equal ones or a repeated element was found through the
   corners alone. A range stays as uniform as 0.2's: [int_range 3 1000]
   draws above 200 about three times in four. *)
let in_case () =
  let n = 10_000 in
  let row name values p =
    strf "%s: %d of %d" name
      (List.length (List.filter p values))
      (List.length values)
  in
  let duplicate l = List.length (List.sort_uniq compare l) < List.length l in
  let long l = List.length l >= 2 in
  let lists gen = List.filter long (samples gen n) in
  expect
    (String.concat "\n"
       [
         row "int within 256" (samples Gen.int n) (fun x ->
             -256 <= x && x <= 256);
         row "pair int int, equal"
           (samples Gen.(pair int int) n)
           (fun (a, b) -> a = b);
         row "list int of length 2 or more, a duplicate"
           (lists Gen.(list int))
           duplicate;
         row "list string of length 2 or more, a duplicate"
           (lists Gen.(list string))
           duplicate;
         row "int_range 3 1000, at most 100"
           (samples (Gen.int_range 3 1000) n)
           (fun x -> x <= 100);
         row "int_range 3 1000, above 200"
           (samples (Gen.int_range 3 1000) n)
           (fun x -> x > 200);
       ])
  @@ __POS_OF__
       {|
    int within 256: 5819 of 10000
    pair int int, equal: 1352 of 10000
    list int of length 2 or more, a duplicate: 2792 of 6573
    list string of length 2 or more, a duplicate: 3076 of 6573
    int_range 3 1000, at most 100: 1578 of 10000
    int_range 3 1000, above 200: 7586 of 10000
    |}

let generators =
  group "Generators"
    [
      group "every candidate satisfies the constraints of its generator"
        (List.map keeps kept);
      cases
        "a malformed generator is built, and raises Invalid_argument when it \
         samples"
        ~id:(fun (Malformed (name, _, _)) -> name)
        malformed raises_when_sampled;
      cases
        "a sampled tree is a function of the generator and the state, whatever \
         the order its cells are forced in"
        ~id:(fun (Any (name, _)) -> name)
        pure forced_in_any_order;
      test "a recorded seed replays these values" replayed_values;
      test "a recorded seed replays these choices" replayed_choices;
      test "every generator draws the distribution of its row" distributions;
      test "a case draws small integers and repeats values" in_case;
    ]

(* Numbers *)

let numbers_shrink =
  let nonzero v = v <> 0 in
  [
    Shrinks ("int", (fun () -> Gen.int), nonzero, "0");
    Shrinks ("nat", (fun () -> endless Pp.int Gen.nat), nonzero, "0");
    Shrinks
      ( "small_int, from a negative value",
        (fun () -> Gen.small_int),
        (fun v -> v < 0),
        "0" );
    Shrinks
      ( "int_range 10 100",
        (fun () -> endless Pp.int (Gen.int_range 10 100)),
        (fun v -> v > 10),
        "10" );
    Shrinks
      ( "int_range (-100) (-10)",
        (fun () -> endless Pp.int (Gen.int_range (-100) (-10))),
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
        (fun () ->
          endless
            (fun ppf n -> Format.fprintf ppf "%ldl" n)
            (Gen.int32_range 10l 100l)),
        (fun v -> v > 10l),
        "10l" );
    Shrinks
      ( "int64_range (-100L) (-10L)",
        (fun () ->
          endless
            (fun ppf n -> Format.fprintf ppf "%LdL" n)
            (Gen.int64_range (-100L) (-10L))),
        (fun v -> v < -10L),
        "-10L" );
    Shrinks
      ( "nativeint_range Nativeint.min_int Nativeint.max_int",
        (fun () -> Gen.nativeint_range Nativeint.min_int Nativeint.max_int),
        (fun v -> not (Nativeint.equal v 0n)),
        "0n" );
    Shrinks
      ("float", (fun () -> Gen.float), (fun v -> not (Float.equal v 0.)), "0.");
    Shrinks ("float, from -0.", (fun () -> Gen.float), is_negative_zero, "0.");
    Shrinks
      ("any_float, from nan", (fun () -> Gen.any_float), Float.is_nan, "0.");
    Shrinks
      ( "any_float, from neg_infinity",
        (fun () -> Gen.any_float),
        Float.equal Float.neg_infinity,
        "0." );
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
      ( "float_range (-1.) (-0.), to -0.",
        (fun () -> Gen.float_range (-1.) (-0.)),
        always,
        "-0." );
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
    Literal ("any_float", (fun () -> Gen.any_float), exact);
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

(* A candidate lowers its parent's distance to the origin or its side, and
   the origin is spelled on either side. *)
let nearer_the_origin triple =
  let low, high, tree = ranged triple in
  let origin = max low (min high 0) in
  let gap v = abs (v - origin) in
  List.iter
    (fun (parent, child) ->
      within int low high child;
      at_most int ~than:(gap parent) (gap child))
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

(* On a 64-bit platform a hundred draws reach past 32 bits on both sides: a
   wide draw's bit length runs up to the word's half the time. *)
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
    Corners
      ( "float",
        (fun () -> Gen.float),
        float_exact,
        [
          0.;
          -0.;
          1.;
          -1.;
          Float.succ 0.;
          Float.pred 0.;
          Float.min_float;
          -.Float.min_float;
          Float.max_float;
          -.Float.max_float;
        ] );
    Corners
      ( "any_float",
        (fun () -> Gen.any_float),
        float_exact,
        [ Float.nan; Float.infinity; Float.neg_infinity; -0.; Float.max_float ]
      );
    Corners
      ( "float_range (-1.) 1.",
        (fun () -> Gen.float_range (-1.) 1.),
        float_exact,
        [ -1.; Float.pred 0.; -0.; 0.; Float.succ 0.; 1. ] );
    Corners
      ( "float_range 2. 5.",
        (fun () -> Gen.float_range 2. 5.),
        float_exact,
        [ 2.; Float.succ 2.; 5. ] );
    Corners
      ( "float_range (-1.) (-0.)",
        (fun () -> Gen.float_range (-1.) (-0.)),
        float_exact,
        [ -1.; Float.pred 0.; -0. ] );
  ]

(* One draw in ten is a corner, each corner equally likely, so a corner of
   [int] comes about once in 50 draws, and one of the ten of [float] once in
   100; 2000 draws miss one with probability below 1e-17 and 1e-8. *)
let reaches_its_corners (Corners (_, gen, w, corners)) =
  let values = samples (gen ()) 2_000 in
  let drawn c = List.exists (Testable.equal w c) values in
  equal (list w) corners (List.filter drawn corners)

(* The corners of [int_range 0 255] are [0], [1] and [255]: the corner draw,
   and the uniform draws of the other nine in ten that land on one. *)
let corner_share () =
  let values = samples (Gen.int_range 0 255) 20_000 in
  let corner v = List.mem v [ 0; 1; 255 ] in
  equal (float 0.01) (0.1 +. (0.9 *. 3. /. 256.)) (share values corner)

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
    (name ^ " low high draws within [low;high], and so does each candidate")
    ~count:50
    Gen.(triple whole whole int64)
    (fun (a, b, seed) ->
      let low = min a b and high = max a b in
      let tree = at_seed (range low high) seed in
      within w low high (value tree);
      List.iter
        (fun (_, child) -> within w low high child)
        (edges ~limit:100 tree))

(* [-2^62; Int64.max_int] counts more than 2^63 values, which [Seed.below]
   cannot bound, and it is a range, not the whole type, so it draws uniformly:
   a third of the draws but the corners are negative, and two of the five
   corners. *)
let wide_int64_range () =
  let values =
    samples (Gen.int64_range (Int64.div Int64.min_int 2L) Int64.max_int) 4_000
  in
  equal (float 0.02)
    ((0.9 *. 1. /. 3.) +. (0.1 *. 2. /. 5.))
    (share values (fun v -> Int64.compare v 0L < 0))

(* Half the finite bit patterns are negative, and half have a magnitude of at
   least 1; a float uniform over the reals would have nearly none below 1. The
   corners, a tenth of the draws, are half negative and 4 in 10 at least 1. *)
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
(* [-0. +. 0.] is [0.], so a uniform draw at [high] must take its sign. *)
let float_edges () =
  let draw gen = value (sample gen 0) in
  equal float_exact 1.5 (draw (Gen.float_range 1.5 1.5));
  satisfies ~claim:"finite" float_exact Float.is_finite
    (draw (Gen.float_range 0. Float.max_float));
  equal (list float_exact)
    (List.init 100 (fun _ -> -0.))
    (samples (Gen.float_range (-0.) (-0.)) 100)

(* A range draws the zero of its bounds' sign: [float_range 0. 10.] states a
   non-negative domain, and [float_range (-1.) (-0.)] a non-positive one. *)
let zero_of_its_sign () =
  let signs gen =
    List.sort_uniq Bool.compare
      (List.map Float.sign_bit
         (List.filter (fun v -> v = 0.) (samples gen 2_000)))
  in
  equal (list bool) [ false ] (signs (Gen.float_range 0. 10.));
  equal (list bool) [ true ] (signs (Gen.float_range (-1.) (-0.)))

(* One draw in ten is not finite, each of the three values equally likely. *)
let any_float_share () =
  let values = samples Gen.any_float 20_000 in
  equal
    (list (float 0.01))
    [ 0.1; 0.1 /. 3. ]
    [
      share values (fun v -> not (Float.is_finite v)); share values Float.is_nan;
    ]

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
        ~id:(fun (low, high) -> strf "int_range %d %d" low high)
        [ (5, 5); (-100, -10); (3, 9) ]
        origin_at_a_bound;
      prop
        "an integer's candidates lie in its range, no farther from the origin \
         than their parent"
        ~count:30 bounds nearer_the_origin;
      group "a range of int32, int64 or nativeint behaves as int_range"
        (List.map sized_range sized_ranges);
      test
        "int64_range over more than 2^63 values draws uniformly apart from its \
         corners"
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
      cases "a number generator draws each of its corners"
        ~id:(fun (Corners (name, _, _, _)) -> name)
        corners reaches_its_corners;
      test "one draw in ten is a corner" corner_share;
      test
        "float draws finite floats, uniformly over their bit patterns apart \
         from its corners"
        float_bits;
      test
        "any_float draws nan, infinity or neg_infinity one draw in ten, each \
         equally likely"
        any_float_share;
      test "float_range draws inside its edges" float_edges;
      test "float_range draws the zero of its bounds' sign" zero_of_its_sign;
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
    Shrinks
      ("char", (fun () -> endless pp_char Gen.char), (fun c -> c <> 'a'), "'a'");
    Shrinks
      ( "char_range 'b' 'y'",
        (fun () -> endless pp_char (Gen.char_range 'b' 'y')),
        (fun c -> c > 'b'),
        "'b'" );
    Shrinks
      ( "char_range 'A' 'Z'",
        (fun () -> endless pp_char (Gen.char_range 'A' 'Z')),
        (fun c -> c < 'Z'),
        "'Z'" );
    Shrinks
      ( "char_range '0' '9'",
        (fun () -> endless pp_char (Gen.char_range '0' '9')),
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
  let index u = Uchar.to_int u - if Uchar.to_int u > 0xD7FF then 0x800 else 0 in
  let gap u = abs (index u - Char.code 'a') in
  List.iter
    (fun (parent, child) -> at_most int ~than:(gap parent) (gap child))
    (edges tree)

let past code u = Uchar.to_int u > code

let base =
  group "Unit, booleans, characters and strings"
    [
      cases "unit and constant consume no randomness"
        ~id:(fun (Any (n, _), _) -> n)
        consumes draws_words;
      test
        "bool draws both values, true's one candidate is false, and false has \
         none"
        bool_values;
      cases "char draws below 32 and above 127, as does the full char_range"
        ~id:fst
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
        "a uchar's candidates are scalar values no farther from U+0061 than \
         their parent"
        ~count:50 Gen.int64 uchar_nearer_a;
      cases
        "a search for a uchar past a code point stops at the next scalar value"
        ~id:(fun (code, _) -> strf "past 0x%X" code)
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
(* The strata 50%, 25%, 20%, 4% and 1% below 4, 8, 16, 64 and 256,
   overlapping as [nat]'s do. The last is for the laws that fail only on a
   long collection: 0.75% of the lists are 64 or longer, which a cap at 63
   would make unreachable at any count. *)
let default_length () =
  let lengths = List.map List.length (samples Gen.(list unit) 20_000) in
  let band (low, high) = share lengths (fun n -> low <= n && n < high) in
  less int ~than:256 (greatest lengths);
  at_least int ~than:128 (greatest lengths);
  equal
    (list (float 0.015))
    [ 0.67765625; 0.17765625; 0.1053125; 0.031875 ]
    (List.map band [ (0, 4); (4, 8); (8, 16); (16, 64) ]);
  equal (float 0.003) 0.0075 (band (64, 256));
  equal (float 0.3) 5.66
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

(* Two generators of their own, so that the second call has no earlier call of
   its generator to repeat (see [in_case]). *)
let first_then_second () =
  let draws index =
    let s = state index in
    let a, after_a = drawn Gen.int s in
    let b, after_b = drawn Gen.small_int after_a in
    let ab, after_ab = drawn Gen.(pair int small_int) s in
    ((value a, value b), value ab, words 2 after_b, words 2 after_ab)
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
      test
        "the default length is below 256, 50%, 25%, 20%, 4% and 1% below 4, 8, \
         16, 64 and 256, about 5.7 on average"
        default_length;
      test
        "option, result and either draw None at 0.15, Error at 0.25 and Left \
         at 0.5"
        constructor_shares;
      test "None is the first candidate of a Some" (fun () ->
          equal (option int) None
            (value (first_candidate (find Gen.(option nat) Option.is_some))));
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
    (shrinks_to ~from:(fun v -> v = 30) (endless Format.pp_print_int gen))

let one_of_candidates () =
  let gen =
    Gen.(one_of [ constant 10; constant 20; constant 30; int_range 40 50 ])
  in
  let tree = find gen (fun v -> v > 41) in
  let candidates = candidate_values tree in
  mem int 10 candidates;
  List.iter
    (fun v ->
      satisfies ~claim:"an earlier branch or below the drawn value" int
        (fun v -> v = 10 || v = 20 || v = 30 || (40 <= v && v < value tree))
        v)
    candidates

let weights () =
  let values =
    samples Gen.(frequency [ (1, constant `A); (3, constant `B) ]) 400
  in
  equal (float 0.05) 0.75 (share values (fun v -> v = `B))

let as_it_stands () =
  let direct index =
    value (fst (drawn Gen.int (snd (Seed.below ~bound:1L (state index)))))
  in
  equal (list int) (List.init 10 direct)
    (samples Gen.(frequency [ (1, int) ]) 10)

let such_that_draws () =
  let draws = ref 0 in
  let counted =
    Gen_engine.make ~pp:Format.pp_print_int (fun src ->
        incr draws;
        Gen_engine.value (Gen_engine.parse Gen.int src))
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
        ~id:(fun (One (n, _, _, _)) -> n)
        ones draws_one;
      test "of_list draws each value, and shrinks toward its head, by position"
        of_list_values;
      test "one_of draws each branch" (fun () ->
          equal (list int) [ 1; 2 ]
            (List.sort_uniq Int.compare
               (samples Gen.(one_of [ constant 1; constant 2 ]) 100)));
      test
        "one_of's candidates are the first branch, an earlier branch or a \
         smaller value of the drawn one"
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
        ~id:(fun (Any (n, _)) -> n)
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
        ~id:(strf "state %d") [ 0; 1; 2; 3; 4 ] next_to_aaa;
      test
        "a search over a frequency of letters and of_list stops at one \
         character" (fun () ->
          let gen = identifier () in
          let root = value (sample gen 0) in
          mem string
            (shrinks_to ~from:(String.equal root) gen)
            [ {|"a"|}; {|"-"|} ]);
    ]

(* Permutations and subsequences *)

let one_to n = List.init n (fun i -> i + 1)

(* The inversions of a list: its pairs out of increasing order. *)
let rec inversions = function
  | [] -> 0
  | x :: rest ->
      List.length (List.filter (fun y -> y < x) rest) + inversions rest

(* The lists that swap two adjacent elements of [l] out of order. *)
let adjacent_swaps l =
  let a = Array.of_list l in
  List.filter_map
    (fun i ->
      if a.(i) < a.(i + 1) then None
      else
        let b = Array.copy a in
        b.(i) <- a.(i + 1);
        b.(i + 1) <- a.(i);
        Some (Array.to_list b))
    (List.init (Int.max 0 (Array.length a - 1)) Fun.id)

let rec increasing = function
  | a :: (b :: _ as rest) -> a < b && increasing rest
  | [ _ ] | [] -> true

let pp_int = Format.pp_print_int
let count_of ppf l = Format.pp_print_int ppf (List.length l)
let inversions_of ppf l = Format.pp_print_int ppf (inversions l)

let selection_kept =
  [
    Kept
      ( "permutation (one_to 8), an ordering of its values",
        (fun () -> Gen.permutation (one_to 8)),
        fun _ l -> equal (list int) (one_to 8) (List.sort Int.compare l) );
    Kept
      ( "subsequence (one_to 20), its values in their order",
        (fun () -> Gen.subsequence (one_to 20)),
        fun _ l ->
          satisfies ~claim:"increasing" (list int) increasing l;
          List.iter (within int 1 20) l );
  ]

let fewer_inversions seed =
  List.iter
    (fun (parent, child) ->
      less int ~than:(inversions parent) (inversions child))
    (edges ~limit:300 (at_seed (Gen.permutation (one_to 8)) seed))

let offers_every_swap seed =
  List.iter
    (fun (node, candidates) ->
      List.iter
        (fun swap -> mem (list int) swap candidates)
        (adjacent_swaps node))
    (visit ~limit:100
       (fun tree -> (value tree, candidate_values tree))
       (at_seed (Gen.permutation (one_to 8)) seed))

(* The six orderings of three values, 1000 draws each on average. *)
let orderings () =
  let values = samples (Gen.permutation [ 1; 2; 3 ]) 6_000 in
  let orders =
    [
      [ 1; 2; 3 ];
      [ 1; 3; 2 ];
      [ 2; 1; 3 ];
      [ 2; 3; 1 ];
      [ 3; 1; 2 ];
      [ 3; 2; 1 ];
    ]
  in
  equal
    (list (float 0.02))
    (List.map (fun _ -> 1. /. 6.) orders)
    (List.map (fun o -> share values (( = ) o)) orders)

(* [int_range 0 10] draws [0], [1] and [10] as corners, and each length of
   the range. *)
let subsequence_lengths () =
  let lengths =
    List.map List.length (samples (Gen.subsequence (one_to 10)) 4_000)
  in
  equal (list int) (List.init 11 Fun.id) (List.sort_uniq Int.compare lengths);
  equal (float 0.3) 5. (float_of_int (List.fold_left ( + ) 0 lengths) /. 4_000.)

(* The six choices of two positions among four, among the draws of length
   2, which are a fifth of the uniform draws. *)
let subsequence_choices () =
  let pairs =
    List.filter
      (fun l -> List.length l = 2)
      (samples (Gen.subsequence [ 1; 2; 3; 4 ]) 12_000)
  in
  let choices =
    [ [ 1; 2 ]; [ 1; 3 ]; [ 1; 4 ]; [ 2; 3 ]; [ 2; 4 ]; [ 3; 4 ] ]
  in
  equal
    (list (float 0.03))
    (List.map (fun _ -> 1. /. 6.) choices)
    (List.map (fun c -> share pairs (( = ) c)) choices)

let ends_apart l = List.hd l > List.nth l (List.length l - 1)

let rec before a b = function
  | [] -> false
  | x :: rest -> x = a || (x <> b && before a b rest)

let collide l =
  List.exists (fun a -> List.exists (fun b -> a < b && a mod 64 = b mod 64) l) l

let selection_shrink =
  [
    Shrinks
      ( "permutation, to the values as given",
        (fun () -> Gen.permutation ~pp:pp_int (one_to 5)),
        always,
        "[1; 2; 3; 4; 5]" );
    Shrinks
      ( "subsequence, to []",
        (fun () -> Gen.subsequence ~pp:pp_int (one_to 5)),
        always,
        "[]" );
  ]

let selection_literals =
  [
    Literal
      ( "permutation ~pp",
        (fun () -> Gen.permutation ~pp:pp_int (one_to 5)),
        fun l -> "[" ^ ints l ^ "]" );
    Literal
      ( "subsequence ~pp",
        (fun () -> Gen.subsequence ~pp:pp_int (one_to 6)),
        fun l -> "[" ^ ints l ^ "]" );
  ]

let selections =
  group "Permutations and subsequences"
    [
      group "every candidate is a selection of the values"
        (List.map keeps selection_kept);
      cases
        "a selection from no value or one value draws it, with no candidates"
        ~id:(fun (One (n, _, _, _)) -> n)
        [
          One ("permutation []", (fun () -> Gen.permutation []), list int, []);
          One
            ( "permutation [ 7 ]",
              (fun () -> Gen.permutation [ 7 ]),
              list int,
              [ 7 ] );
          One ("subsequence []", (fun () -> Gen.subsequence []), list int, []);
        ]
        draws_one;
      test "permutation draws each ordering with equal probability" orderings;
      prop "a permutation's candidates each have fewer inversions" ~count:30
        Gen.int64 fewer_inversions;
      prop
        "a permutation's every node offers each swap of two adjacent elements \
         out of order"
        ~count:30 Gen.int64 offers_every_swap;
      shrinks "a selection shrinks toward its values as given, or toward []"
        selection_shrink;
      test
        "a search for a permutation of 8 with 5 before 3 stops at 2 inversions"
        (fun () ->
          equal string "2"
            (shrinks_to ~failing:(before 5 3)
               (Gen.with_pp inversions_of (Gen.permutation (one_to 8)))));
      test
        "a search for a permutation of 64 whose first element exceeds its last \
         stops at 63 inversions, within the budget" (fun () ->
          equal string "63"
            (shrinks_to ~failing:ends_apart
               (Gen.with_pp inversions_of (Gen.permutation (one_to 64)))));
      test
        "a search for a permutation of 100 whose first element exceeds its \
         last stops at 99 inversions, within the budget" (fun () ->
          equal string "99"
            (shrinks_to ~failing:ends_apart
               (Gen.with_pp inversions_of (Gen.permutation (one_to 100)))));
      test "subsequence draws every length of int_range 0 n" subsequence_lengths;
      test
        "subsequence draws each choice of positions of a length with equal \
         probability"
        subsequence_choices;
      test
        "a search for a subsequence of 0..99 holding two keys equal modulo 64 \
         stops at two keys" (fun () ->
          equal string "2"
            (shrinks_to ~failing:collide
               (Gen.with_pp count_of (Gen.subsequence (List.init 100 Fun.id)))));
      literals "a selection given ~pp prints as a list" selection_literals;
      test "a selection without ~pp has no printer" (fun () ->
          equal (list string)
            [ placeholder; placeholder ]
            [
              shown (sample (Gen.permutation [ 1; 2 ]) 0);
              shown (sample (Gen.subsequence [ 1; 2 ]) 0);
            ]);
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
  ignore (Seq.length tree.children);
  let forced = !calls in
  ignore (Seq.length tree.children);
  equal (list int) [ 1; forced ] [ at_sampling; !calls ]

let map_raises () =
  raises Exit (fun () -> sample (Gen.map (fun _ -> raise Exit) Gen.int) 3);
  let tree =
    sample (Gen.map (fun v -> if v = 0 then raise Exit else v) Gen.int) 3
  in
  not_equal int 0 (value tree);
  List.iter (not_equal int 0) (candidate_values tree)

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
        "map runs f on the root at sampling, and a candidate's cell once, when \
         forced"
        map_forcing;
      test
        "what map's f raises escapes sampling, and a candidate on which it \
         raises is skipped"
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
          starts_with ~prefix:"from \"" (shown (find gen (fun n -> n > 0))));
      test "a bind candidate on which the function raises is skipped" (fun () ->
          let gen =
            Gen.bind (Gen.int_range 0 10) (fun v ->
                if v = 0 then raise Exit else Gen.constant v)
          in
          List.iter (not_equal int 0)
            (candidate_values (find gen (fun v -> v > 0))));
      shrinks "a composition shrinks its outer value first, then its inner one"
        composition_shrink;
      renderings
        "a map renders its argument as a pre-image, a bind its inner value, \
         with_pp its value"
        composition_renderings;
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

(* What [render_value] gives when its printer raises [exn]: the text, or
   what left it. *)
let bare exn =
  match Gen_engine.render_value (raising exn) 3 with
  | text -> text
  | exception e -> "raised " ^ Printexc.to_string e

let bare_rows =
  [
    ("Failure", Stdlib.Failure "boom", {|<printer raised Failure("boom")>|});
    ("Stack_overflow", Stack_overflow, "<printer raised Stack overflow>");
    ( "a timeout",
      Failure.Control (`Timeout 1.5),
      "raised windtrap timeout after 1.5s" );
    ( "a skip",
      Failure.Control (`Skip (Some "later")),
      "raised windtrap skip: later" );
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
    (Gen_engine.prints tree.node)

let prints_formats_nothing () =
  let calls = ref 0 in
  let counting ppf v =
    incr calls;
    Format.pp_print_int ppf v
  in
  let tree = sample (Gen.with_pp counting Gen.int) 0 in
  let before = !calls in
  is_true (Gen_engine.prints tree.node);
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
      cases
        "a raising printer renders a sample as <printer raised EXN>, a control \
         included"
        ~id:(fun (n, _, _) -> n)
        raised_rows
        (fun (_, exn, text) ->
          equal string text (shown (sample (raising exn) 0)));
      cases
        "render_value renders a printer's fault as <printer raised EXN> and \
         raises a control again"
        ~id:(fun (n, _, _) -> n)
        bare_rows
        (fun (_, exn, text) -> equal string text (bare exn));
      cases "Sys.Break and Out_of_memory escape a printer's guard"
        ~id:Printexc.to_string [ Sys.Break; Out_of_memory ] (fun exn ->
          equal string (Printexc.to_string exn)
            (ended (fun () -> shown (sample (raising exn) 0))));
      test "render formats on every call, at the default margin" formats_again;
      cases "a sample renders as a non-empty text"
        ~id:(fun (Any (n, _)) -> n)
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
        ~id:(fun (Any (n, _)) -> n)
        prints_rows prints_iff_rendered;
      test "prints formats nothing" prints_formats_nothing;
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
         selections;
         composition;
         rendering;
       ])
