(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Stateful tests on several domains. The systems under test give results
   that depend on the domain a call runs on, never on a race, so every
   verdict here is deterministic.

   A process that has spawned a domain can never fork again, and the
   mutation loop forks every mutant from this one. Under mutation testing a
   stateful test on several domains spawns nothing, and every test here that
   spawns a pool skips; the scenarios that leave a domain running forever
   or spawn every domain the runtime has run in forked children, before any
   domain exists. *)

open Windtrap
module Failure = Windtrap.Private.Failure
module Gen_engine = Windtrap.Private.Gen_engine
module Property = Windtrap.Private.Property
module Run = Windtrap.Private.Run
module Sections = Windtrap.Private.Report_sections
module Seed = Windtrap.Private.Seed
module Shrink_tree = Windtrap.Private.Gen_engine.Shrink_tree
module Stateful = Windtrap.Private.Stateful
module Test_tree = Windtrap.Private.Test_tree
module Workers = Windtrap.Private.Workers
module Loc = Windtrap.Private.Loc
module Scratch = Windtrap_test_support.Scratch

let strf = Printf.sprintf
let abstract = Stateful.abstract
let ( @-> ) = Stateful.( @-> )
let ( ^-> ) = Stateful.( ^-> )
let returns = Stateful.returns
let makes = Stateful.makes
let judges = Stateful.judges
let command = Stateful.command
let among = Stateful.among

(* Programs *)

let root = 0x00c0ffee1234abcdL

let state ?(path = "test_stateful_domains") index =
  Seed.make (Seed.derive ~root ~path ~index)

let drawn ?path gen index = Gen_engine.sample gen (state ?path index)
let value node = Gen_engine.value (Shrink_tree.root node)
let printed gen program = Gen_engine.render_value gen program
let self () = (Domain.self () :> int)
let main_domain = Atomic.make (self ())

(* How [fn ()] ended, as in the one-domain suite. *)
let label (f : Failure.t) =
  match f.msg with None -> "" | Some m -> strf "[%s] " m.kept

let kind (f : Failure.t) =
  let side name = function
    | None -> ""
    | Some (t : Failure.text) -> strf " %s %s" name t.kept
  in
  match f.kind with
  | Message m -> strf "message %S" m.kept
  | Raise { expected; actual; _ } ->
      "raise" ^ side "expected" expected ^ side "actual" actual
  | Equality { expected; actual; _ } ->
      strf "equality %s, %s" expected.kept actual.kept
  | Law _ -> "law"
  | Timeout _ -> "timeout"
  | Containment _ | Baseline _ | Property _ -> "other"

let ended fn =
  match fn () with
  | () -> "returned"
  | exception Failure.Check_failure f -> "failure " ^ label f ^ kind f
  | exception Property.Oracle_failure f -> "oracle " ^ label f ^ kind f
  | exception e -> "raised " ^ Failure.exn_to_string e

(* The one test of a tree, and its body. *)
let single tree =
  require_match
    (function [ c ] -> Some c | _ -> None)
    (Test_tree.flatten [ tree ])

let body tree =
  require_match
    (function Test_tree.Body f -> Some f | Test_tree.Scoped _ -> None)
    (single tree).Test_tree.body

(* Workers *)

let mutating () =
  match (Run.config (Run.current ())).mutation with
  | Run.No_mutation -> false
  | Run.Loop _ | Run.Armed _ -> true

let with_pool n fn =
  if mutating () then
    skip ~reason:"under mutation testing no test spawns a domain" ();
  let pool = Workers.spawn n in
  Fun.protect ~finally:(fun () -> Workers.join pool) (fun () -> fn pool)

let execute ?workers program () = Stateful.execute ?workers program

(* [recorded gen program] runs [program], on the calling domain without
   [workers], and is its record, however the run ended. *)
let recorded ?workers gen program =
  ignore (ended (execute ?workers program) : string);
  printed gen program

(* The first program of [gen] whose record, run on the calling domain after
   [reset ()], [accept] holds. *)
let find ?(path = "find") ?(reset = ignore) gen accept =
  let at index =
    let tree = drawn ~path gen index in
    reset ();
    if accept (recorded gen (value tree)) then Some tree else None
  in
  require_some ~msg:"a program among the first 2000 meets the premise"
    (Seq.find_map at (Seq.init 2000 Fun.id))

(* A record's rows as cells: [#], then each column under its header. *)
let table record =
  match String.split_on_char '\n' record with
  | [] -> []
  | header :: rows ->
      let starts =
        List.filter_map
          (fun name ->
            Option.map
              (fun i -> (name, i))
              (Seq.find
                 (fun i ->
                   i + String.length name <= String.length header
                   && String.sub header i (String.length name) = name
                   && (i = 0 || header.[i - 1] = ' '))
                 (Seq.init (String.length header) Fun.id)))
          [ "reference before"; "domain"; "call"; "result" ]
      in
      let cell row (name, start) =
        let next =
          List.fold_left
            (fun next (_, i) -> if i > start && i < next then i else next)
            max_int starts
        in
        let stop = min next (String.length row) in
        if start >= String.length row then (name, "")
        else (name, String.trim (String.sub row start (stop - start)))
      in
      List.map (fun row -> List.map (cell row) starts) rows

(* A row's cell under [name], [""] when the table has no such column. *)
let cell name row = Option.value ~default:"" (List.assoc_opt name row)
let in_branch row = cell "domain" row <> ""

let calls record =
  List.map (fun row -> (cell "domain" row, cell "call" row)) (table record)

(* The words of a record's header row. *)
let header record =
  let line = List.hd (String.split_on_char '\n' record) in
  List.filter (fun w -> w <> "") (String.split_on_char ' ' line)

(* Systems *)

(* A queue whose length loses updates between domains other than the one
   that made it: [length] counts the owner's pushes, then only the largest
   count among the other domains. Its outcomes depend on where the calls ran,
   never on when. *)
module Lossy = struct
  type t = { owner : int; lock : Mutex.t; counts : (int, int) Hashtbl.t }

  let create () =
    { owner = self (); lock = Mutex.create (); counts = Hashtbl.create 4 }

  let locked t f =
    Mutex.lock t.lock;
    Fun.protect ~finally:(fun () -> Mutex.unlock t.lock) f

  let count t domain =
    Option.value ~default:0 (Hashtbl.find_opt t.counts domain)

  let push t (_ : int) =
    locked t (fun () ->
        Hashtbl.replace t.counts (self ()) (count t (self ()) + 1))

  let length t =
    locked t (fun () ->
        let others =
          Hashtbl.fold
            (fun domain n m -> if domain = t.owner then m else max n m)
            t.counts 0
        in
        count t t.owner + others)
end

let lossy_commands () =
  let at line = ("test/test_mpmc.ml", line, 4, 80) in
  let queue = abstract "q" in
  [
    command ~__POS__:(at 7) "create"
      (Gen.unit @-> makes queue)
      Queue.create Lossy.create;
    command ~__POS__:(at 8) "push"
      (queue ^-> Gen.int_range 0 9 @-> returns unit)
      (fun q x -> Queue.push x q)
      Lossy.push;
    command ~__POS__:(at 10) "length"
      (queue ^-> returns int)
      Queue.length Lossy.length;
  ]

(* A log of what a system saw, safe from any domain. *)
module Log = struct
  let lock = Mutex.create ()
  let entries = ref []

  let add x =
    Mutex.lock lock;
    entries := x :: !entries;
    Mutex.unlock lock

  let take () =
    Mutex.lock lock;
    let l = List.rev !entries in
    entries := [];
    Mutex.unlock lock;
    l
end

(* Checking *)

let tags t =
  List.filter
    (fun n -> Test_tree.Tag.mem n (single t).tags)
    [ "custom"; "parallel"; "prop"; "stateful" ]

let tick = command "tick" (Gen.unit @-> returns unit) ignore ignore

let refused =
  "Windtrap.stateful: on several domains every command makes a value, has a \
   ~pre or takes an element, so no call can run after the prefix"

(* Every command makes a value or has a ~pre, so no call could run after the
   prefix. *)
let prefix_only () =
  let counter = abstract "c" in
  [
    command "create"
      (Gen.unit @-> makes counter)
      (fun () -> ref 1)
      (fun () -> ref 1);
    command "decr" ~pre:(fun c -> !c > 0) (counter ^-> returns unit) decr decr;
  ]

(* Every command makes a value or takes an element. *)
let listing_only () =
  let d = abstract "d" in
  let index = among int d (fun m -> List.init (List.length m) Fun.id) in
  [
    command "create" (Gen.unit @-> makes d) (fun () -> [ 0 ]) ignore;
    command "get"
      (d ^-> index ^-> returns unit)
      (fun _ _ -> ())
      (fun () _ -> ());
  ]

let checking =
  group "Checking"
    [
      test "a test on several domains carries the tag parallel" (fun () ->
          equal (list string)
            [ "custom"; "parallel"; "prop"; "stateful" ]
            (tags
               (Stateful.stateful ~domains:2 ~tags:[ "custom" ] "t" [ tick ]));
          equal (list string) [ "prop"; "stateful" ]
            (tags (Stateful.stateful "t" [ tick ])));
      test "a test on several domains takes no retries, a group's included"
        (fun () ->
          let retries t =
            List.map
              (fun (c : Test_tree.case) -> c.retries)
              (Test_tree.flatten [ group ~retries:3 "g" [ t ] ])
          in
          equal (list int) [ 0 ]
            (retries (Stateful.stateful ~domains:2 "t" [ tick ]));
          equal (list int) [ 3 ] (retries (Stateful.stateful "t" [ tick ])));
      test "domains below 1 raise inside the test" (fun () ->
          let t = Stateful.stateful ~domains:0 "t" [ tick ] in
          equal string
            "raised Invalid_argument(\"Windtrap.stateful: domains below 1\")"
            (ended (body t)));
      test
        "a command list that can run nothing after the prefix is refused on \
         several domains" (fun () ->
          let t = Stateful.stateful ~domains:2 "t" (prefix_only ()) in
          equal string
            (strf "raised Invalid_argument(%S)" refused)
            (ended (body t)));
      test "and allowed on one" (fun () ->
          let t = Stateful.stateful ~count:3 "t" (prefix_only ()) in
          equal string "returned" (ended (body t)));
      test
        "a command list whose commands make a value or take an element is \
         refused on several domains" (fun () ->
          let t = Stateful.stateful ~domains:2 "t" (listing_only ()) in
          equal string
            (strf "raised Invalid_argument(%S)" refused)
            (ended (body t)));
    ]

(* Drawing *)

let counter_commands () =
  let counter = abstract "c" in
  [
    command "create"
      (Gen.unit @-> makes counter)
      (fun () -> ref 0)
      (fun () -> ref 0);
    command "incr" (counter ^-> returns unit) incr incr;
    command "get" (counter ^-> returns int) ( ! ) ( ! );
  ]

let shape_rows = [ (2, 5); (3, 3); (4, 2); (10, 1); (12, 1) ]

(* The rows from the first parallel one on: the branches and the suffix. *)
let rec after_prefix = function
  | row :: rest when not (in_branch row) -> after_prefix rest
  | rows -> rows

let shape (domains, per_branch) () =
  let gen = Stateful.program ~steps:6 ~domains (counter_commands ()) in
  Seq.iter
    (fun index ->
      let rows = table (recorded gen (value (drawn gen index))) in
      let branches = List.filter in_branch rows in
      let per domain =
        List.length
          (List.filter (fun row -> List.assoc "domain" row = domain) branches)
      in
      for d = 1 to domains do
        is_true ~msg:"a branch holds at most its share"
          (per (string_of_int d) <= per_branch)
      done;
      is_true ~msg:"the prefix and the suffix hold at most steps calls"
        (List.length rows - List.length branches <= 6);
      List.iter
        (fun row ->
          is_false ~msg:"no call after the prefix makes a value"
            (String.starts_with ~prefix:"let " (cell "call" row)))
        (after_prefix rows))
    (Seq.init 200 Fun.id)

(* A counter that must not go below zero: [decr]'s ~pre asks the reference
   for a positive count. *)
let guarded_commands () =
  let counter = abstract "c" in
  [
    command "create"
      (Gen.unit @-> makes counter)
      (fun () -> ref 0)
      (fun () -> ref 0);
    command "incr" (counter ^-> returns unit) incr incr;
    command "decr" ~pre:(fun c -> !c > 0) (counter ^-> returns unit) decr decr;
  ]

let guarded_after_prefix () =
  let gen = Stateful.program ~steps:3 ~domains:2 (guarded_commands ()) in
  let parallel = ref 0 in
  Seq.iter
    (fun index ->
      let rows = table (recorded gen (value (drawn gen index))) in
      if List.exists in_branch rows then incr parallel;
      List.iter
        (fun row ->
          starts_with ~msg:"only incr runs after the prefix" ~affix:"incr "
            (cell "call" row))
        (after_prefix rows))
    (Seq.init 300 Fun.id);
  is_true ~msg:"some program has parallel calls" (!parallel > 0)

(* A counter that lists the counts below it; [below] takes one. *)
let listing_commands () =
  let counter = abstract "c" in
  let below = among int counter (fun c -> List.init !c Fun.id) in
  [
    command "create"
      (Gen.unit @-> makes counter)
      (fun () -> ref 0)
      (fun () -> ref 0);
    command "incr" (counter ^-> returns unit) incr incr;
    command "below"
      (counter ^-> below ^-> returns bool)
      (fun c i -> i < !c)
      (fun c i -> i < !c);
  ]

let listing_after_prefix () =
  let gen = Stateful.program ~steps:3 ~domains:2 (listing_commands ()) in
  let parallel = ref 0 and listed = ref 0 in
  Seq.iter
    (fun index ->
      let rows = table (recorded gen (value (drawn gen index))) in
      if List.exists in_branch rows then incr parallel;
      List.iter
        (fun row ->
          if String.starts_with ~prefix:"below " (cell "call" row) then
            incr listed)
        rows;
      List.iter
        (fun row ->
          starts_with ~msg:"only incr runs after the prefix" ~affix:"incr "
            (cell "call" row))
        (after_prefix rows))
    (Seq.init 300 Fun.id);
  is_true ~msg:"some program has parallel calls" (!parallel > 0);
  is_true ~msg:"some prefix takes an element" (!listed > 0)

let drawing =
  group "Drawing"
    [
      cases
        "each branch holds at most its share of calls, and nothing after the \
         prefix makes a value"
        ~name:(fun (d, n) -> strf "%d domains, %d each" d n)
        shape_rows
        (fun row -> shape row ());
      test "a branch and the suffix draw no command that has a ~pre"
        guarded_after_prefix;
      test "a branch and the suffix draw no command that takes an element"
        listing_after_prefix;
    ]

(* Shrinking moves *)

(* The records of [program]'s candidates, each run on the calling domain. *)
let candidates gen tree =
  List.of_seq
    (Seq.map
       (fun child -> recorded gen (value child))
       (Shrink_tree.children tree))

let moved () =
  let gen = Stateful.program ~steps:3 ~domains:2 (counter_commands ()) in
  let tree =
    find gen (fun record ->
        let rows = table record in
        List.length (List.filter in_branch rows) >= 2)
  in
  let before = calls (recorded gen (value tree)) in
  let parallel = List.filter (fun (d, _) -> d <> "") before in
  let children = List.map calls (candidates gen tree) in
  (* A move keeps every call and runs one parallel call alone. *)
  let moves =
    List.filter
      (fun after ->
        List.length after = List.length before
        && List.length (List.filter (fun (d, _) -> d <> "") after)
           = List.length parallel - 1)
      children
  in
  is_true ~msg:"some candidate moves a parallel call out of its branch"
    (moves <> []);
  (* Never into a branch. *)
  List.iter
    (fun after ->
      is_true ~msg:"no candidate adds a parallel call"
        (List.length (List.filter (fun (d, _) -> d <> "") after)
        <= List.length parallel))
    children

let first_moves () =
  let gen = Stateful.program ~steps:3 ~domains:2 (counter_commands ()) in
  let tree =
    find gen (fun record ->
        let domains = List.map (cell "domain") (table record) in
        List.mem "1" domains && List.mem "2" domains)
  in
  let before = calls (recorded gen (value tree)) in
  let branch_one = List.filter (fun (d, _) -> d = "1") before in
  let first = snd (List.hd branch_one) in
  let last = snd (List.nth branch_one (List.length branch_one - 1)) in
  (* The calls before the first parallel one, and the first after the last. *)
  let rec prefix = function ("", c) :: rest -> c :: prefix rest | _ -> [] in
  let to_prefix after =
    match List.rev (prefix after) with c :: _ -> c = first | [] -> false
  in
  let to_suffix after =
    match prefix (List.rev after) with
    | [] -> false
    | suffix -> List.nth suffix (List.length suffix - 1) = last
  in
  let children = List.map calls (candidates gen tree) in
  is_true
    ~msg:"a candidate moves branch 1's first call to the end of the prefix"
    (List.exists to_prefix children);
  is_true
    ~msg:"a candidate moves branch 1's last call to the start of the suffix"
    (List.exists to_suffix children)

let shrinking =
  group "Shrinking moves"
    [
      test "a candidate moves a parallel call out of its branch, never into one"
        moved;
      test
        "a branch's first call moves to the prefix's end, its last to the \
         suffix's start"
        first_moves;
    ]

(* The judge *)

(* A reference that appends to a list; a call is [push x] or [observe l],
   which differs unless the list is [l]. Each fresh state records the calls
   it saw. *)
type fake = { mutable seen : string list }

let fakes = ref []

let fresh () =
  let s = { seen = [] } in
  fakes := s :: !fakes;
  (s, ref [])

let push n x =
  ( n,
    fun ((s, l) : fake * int list ref) ->
      s.seen <- strf "push %d" x :: s.seen;
      l := !l @ [ x ];
      None )

let observe n expected =
  ( n,
    fun ((s, l) : fake * int list ref) ->
      s.seen <- "observe" :: s.seen;
      if !l = expected then None
      else
        Some
          (Failure.equality
             ~expected:(String.concat ";" (List.map string_of_int expected))
             ~actual:(String.concat ";" (List.map string_of_int !l))
             ()) )

let verdict = function
  | Stateful.Explained order ->
      "explained " ^ String.concat " " (List.map string_of_int order)
  | Unexplained { order; at; failure } ->
      strf "unexplained %s, at %d: %s"
        (String.concat " " (List.map string_of_int order))
        at (kind failure)

let judged ~branches ~suffix =
  fakes := [];
  verdict (Stateful.judge ~fresh ~branches ~suffix)

let states () =
  List.rev_map (fun s -> String.concat ", " (List.rev s.seen)) !fakes

let judge_rows =
  [
    ( "an order that gives every outcome is accepted",
      [ [ push 1 1; push 2 2 ]; [ push 3 3 ] ],
      [ observe 4 [ 1; 3; 2 ] ],
      "explained 1 3 2 4" );
    ( "an order must keep each branch's order",
      [ [ push 1 1; push 2 2 ]; [ push 3 3 ] ],
      [ observe 4 [ 2; 1; 3 ] ],
      "unexplained 1 2 3 4, at 4: equality 2;1;3, 1;2;3" );
    ( "the suffix runs after every branch",
      [ [ push 1 1 ]; [ push 2 2 ] ],
      [ observe 3 [ 1; 2 ]; push 4 4; observe 5 [ 1; 2; 4 ] ],
      "explained 1 2 3 4 5" );
    ( "with no parallel call the one order is the suffix",
      [ []; [] ],
      [ push 1 1; observe 2 [ 1 ] ],
      "explained 1 2" );
    ( "the closest order is the one whose difference comes latest",
      [ [ push 1 1; observe 2 [ 1 ] ]; [ push 3 3 ] ],
      [ observe 4 [ 3; 1 ] ],
      "unexplained 1 2 3 4, at 4: equality 3;1, 1;3" );
    ( "and it is completed with the calls it did not reach",
      [ [ observe 1 [ 9 ]; push 2 2 ]; [ push 3 3 ] ],
      [ observe 4 [] ],
      "unexplained 3 1 2 4, at 1: equality 9, 3" );
  ]

let replayed () =
  (* Orders tried: 1 2 3 (fails), 1 3 2 (fails), 3 1 2 (passes). *)
  let branches = [ [ push 1 1; push 2 2 ]; [ push 3 3 ] ] in
  let suffix = [ observe 4 [ 3; 1; 2 ] ] in
  equal string "explained 3 1 2 4" (judged ~branches ~suffix);
  equal (list string)
    [
      "push 1, push 2, push 3, observe";
      "push 1, push 3, push 2, observe";
      "push 3, push 1, push 2, observe";
    ]
    (states ())

let judge_group =
  group "The judge"
    [
      cases "judges outcome vectors over program order"
        ~name:(fun (n, _, _, _) -> n)
        judge_rows
        (fun (_, branches, suffix, expected) ->
          equal string expected (judged ~branches ~suffix));
      test
        "each branch point after the first starts from a fresh state replayed \
         along its path"
        replayed;
    ]

(* The report *)

let property_failure = function
  | Property.Fail { failure; _ } -> Some failure
  | Pass _ | Coverage_failed _ | Gave_up _ -> None

(* The entry of the failure that [commands] give on two domains, as the
   report prints it, under the test's declaration at [loc]. *)
let screen ?workers ~loc commands =
  let loc = Loc.of_pos loc in
  let law _ p = Stateful.execute ?workers p in
  let outcome =
    Property.run ~loc ~summary:Stateful.summary ~root ~path:"screen"
      (Stateful.program ~domains:2 commands)
      law
  in
  let f = require_match property_failure outcome in
  Format.asprintf "%a"
    (fun ppf f -> Sections.pp_failure ~ansi:false ~hints:false ppf f)
    f

(* The case that first loses an update, and the shrink steps that follow it,
   depend on how the domains are scheduled; the counterexample they end on
   does not. [unscheduled s] is [s] with the two counts replaced by [_]. *)
let unscheduled s =
  let open_ = "(case " in
  let rec find i =
    if i + String.length open_ > String.length s then s
    else if String.sub s i (String.length open_) = open_ then
      let close = String.index_from s i ')' in
      String.sub s 0 i ^ "(case _, shrunk _ steps"
      ^ String.sub s close (String.length s - close)
    else find (i + 1)
  in
  find 0

let lossy_screen () =
  with_pool 2 @@ fun workers ->
  expect_exact
    (unscheduled
       (screen ~workers
          ~loc:("test/test_mpmc.ml", 21, 0, 10)
          (lossy_commands ())))
  @@ __POS_OF__
       {|    test/test_mpmc.ml:21
    counterexample (case _, shrunk _ steps): 4 calls, 2 in parallel
       #  domain  call                result
       1          let q1 = create ()
       2  1       push q1 0           ()
       3  2       push q1 0           ()
       4          length q1           1
    which failed with:
      no order of the calls gives these results
      the closest order, 2 then 3, differs at call 4: length q1
      expected  2
      actual    1
|}

(* A counter whose reference sides print, so the prefix's rows have cells. *)
let printed_counter () =
  let counter = abstract "c" ~pp:(fun ppf c -> Format.pp_print_int ppf !c) in
  [
    command "create"
      (Gen.unit @-> makes counter)
      (fun () -> ref 0)
      (fun () -> ref 0);
    command "incr" (counter ^-> returns unit) incr incr;
    command "get" (counter ^-> returns int) ( ! ) ( ! );
  ]

let parallel_table () =
  let gen = Stateful.program ~steps:4 ~domains:2 (printed_counter ()) in
  let parallel rows = List.exists in_branch rows in
  let suffix rows =
    match List.rev rows with row :: _ -> not (in_branch row) | [] -> false
  in
  let tree =
    find gen (fun record ->
        let rows = table record in
        parallel rows && suffix rows
        && List.exists (fun row -> cell "reference before" row <> "") rows)
  in
  let record = recorded gen (value tree) in
  equal (list string)
    [ "#"; "reference"; "before"; "domain"; "call"; "result" ]
    (header record);
  (* Cells only before the branches. *)
  List.iter
    (fun row -> equal string "" (cell "reference before" row))
    (after_prefix (table record));
  List.iter
    (fun row ->
      let call = cell "call" row and result = cell "result" row in
      if String.starts_with ~prefix:"incr" call then equal string "()" result
      else if String.starts_with ~prefix:"let" call then equal string "" result
      else is_some (int_of_string_opt result))
    (table record);
  is_false ~msg:"no line ends on a blank"
    (List.exists
       (fun line -> String.ends_with ~suffix:" " line)
       (String.split_on_char '\n' record))

let summaries () =
  let gen = Stateful.program ~steps:4 ~domains:2 (printed_counter ()) in
  let parallel record = List.length (List.filter in_branch (table record)) in
  let tree = find gen (fun record -> parallel record >= 2) in
  let program = value tree in
  let record = recorded gen program in
  equal (option string)
    (Some
       (strf "%d calls, %d in parallel"
          (List.length (table record))
          (parallel record)))
    (Stateful.summary program)

let one_domain_table () =
  let gen = Stateful.program ~steps:4 ~domains:2 (printed_counter ()) in
  let tree =
    find gen (fun record ->
        List.length (table record) >= 2
        && not (List.exists in_branch (table record)))
  in
  let program = value tree in
  let record = recorded gen program in
  is_false ~msg:"no domain column" (List.mem "domain" (header record));
  is_false ~msg:"no result column" (List.mem "result" (header record));
  is_true
    (String.starts_with
       ~prefix:(strf "%d calls, last: " (List.length (table record)))
       (Option.get (Stateful.summary program)))

let report =
  group "The report"
    [
      test "a lost update shrinks to two pushes in parallel" lossy_screen;
      test
        "a parallel record adds domain and result, and cells before the \
         branches only"
        parallel_table;
      test "the summary counts the parallel calls" summaries;
      test "a record without parallel calls prints as on one domain"
        one_domain_table;
    ]

(* Judging runs *)

(* [where] returns the domain its system ran on, which its reference
   accepts; the system logs it. *)
let where () =
  command "where"
    (Gen.unit @-> judges int)
    (fun () _ -> ())
    (fun () ->
      Log.add (self ());
      self ())

(* A program of [commands] whose record, run on the calling domain, has
   calls in both branches and on the test's domain. *)
let spread ?(steps = 2) commands =
  let gen = Stateful.program ~steps ~domains:2 commands in
  let tree =
    find gen (fun record ->
        let domains = List.map (cell "domain") (table record) in
        List.mem "1" domains && List.mem "2" domains && List.mem "" domains)
  in
  (gen, value tree)

let placed () =
  with_pool 2 @@ fun workers ->
  let gen, program = spread [ where () ] in
  ignore (Log.take ());
  let rows = table (recorded ~workers gen program) in
  let seen = List.sort_uniq Int.compare (Log.take ()) in
  let on domain =
    List.sort_uniq String.compare
      (List.filter_map
         (fun row ->
           if cell "domain" row = domain then Some (cell "result" row) else None)
         rows)
  in
  match (on "", on "1", on "2") with
  | [ test ], [ one ], [ two ] ->
      equal string (string_of_int (self ())) test;
      is_true ~msg:"each branch on a domain of its own"
        (one <> two && one <> test && two <> test);
      equal (list int)
        (List.sort Int.compare (List.map int_of_string [ test; one; two ]))
        seen
  | _ ->
      fail
        ("one domain per branch:\n"
        ^ String.concat "\n" (List.concat_map (List.map snd) rows))

let run_counter = Atomic.make 0

(* [get] returns the number of the run that made its value, which prints
   alike in every run: only the first of a program's runs gives the
   reference's [0]. *)
let alike () =
  let made = abstract "m" in
  let hidden = Testable.of_equal Int.equal in
  [
    command "create"
      (Gen.unit @-> makes made)
      (fun () -> ())
      (fun () -> Atomic.fetch_and_add run_counter 1);
    command "get" (made ^-> returns hidden) (fun () -> 0) Fun.id;
  ]

let witnessed () =
  let gen = Stateful.program ~steps:1 ~domains:2 (alike ()) in
  let tree =
    find gen (fun record ->
        match calls record with
        | ("", "let m1 = create ()") :: (_ :: _ as rest) ->
            List.for_all (fun (d, c) -> d <> "" && c = "get m1") rest
        | _ -> false)
  in
  with_pool 2 @@ fun workers ->
  Atomic.set run_counter 0;
  starts_with ~affix:"failure [no order of the calls gives these results\n"
    (ended (execute ~workers (value tree)))

(* A bag of 1 and 2 whose [take] gives 2 in a program's first run and 1 in
   every later one, and whose [mem1] always holds: the second run's
   outcomes after the prefix are the first's, and only the prefix's [take]
   tells them apart. *)
let prefix_judged () =
  let runs = ref 0 in
  let bag = abstract "b" in
  let commands =
    [
      command "create"
        (Gen.unit @-> makes bag)
        (fun () -> ref [ 1; 2 ])
        (fun () -> incr runs);
      command "take"
        (bag ^-> judges int)
        (fun b seen ->
          match seen with
          | Ok x when List.mem x !b -> b := List.filter (fun y -> y <> x) !b
          | Ok _ | Error _ -> fail "not in the bag")
        (fun () -> if !runs = 1 then 2 else 1);
      command "mem1"
        (bag ^-> returns bool)
        (fun b -> List.mem 1 !b)
        (fun () -> true);
    ]
  in
  let gen = Stateful.program ~steps:2 ~domains:2 commands in
  let tree =
    find gen (fun record ->
        match calls record with
        | ("", "let b1 = create ()") :: ("", "take b1") :: (_ :: _ as rest) ->
            List.for_all (fun (d, c) -> d <> "" && c = "mem1 b1") rest
        | _ -> false)
  in
  with_pool 2 @@ fun workers ->
  runs := 0;
  starts_with ~affix:"failure [no order of the calls gives these results\n"
    (ended (execute ~workers (value tree)))

let judged () =
  let next = ref 0 in
  let take =
    command "take"
      (Gen.unit @-> judges int)
      (fun () seen ->
        match seen with Ok v -> Log.add (-v) | Error _ -> Log.add 0)
      (fun () ->
        incr next;
        Log.add !next;
        !next)
  in
  let _, program = spread [ take ] in
  ignore (Log.take ());
  equal string "returned" (ended (execute program));
  let log = Log.take () in
  let returned =
    List.sort_uniq Int.compare (List.filter (fun v -> v > 0) log)
  in
  let seen =
    List.sort_uniq Int.compare
      (List.map (fun v -> -v) (List.filter (fun v -> v <= 0) log))
  in
  equal (list int) returned seen

(* The reference's [get] counts every call to it, so its replay by the
   judge gives the prefix's [get] another result than the run did. *)
let drifting () =
  let reference_calls = ref 0 and system_calls = ref 0 in
  let made = abstract "m" in
  let commands =
    [
      command "create" (Gen.unit @-> makes made) ignore ignore;
      command "get"
        (made ^-> returns int)
        (fun () ->
          incr reference_calls;
          !reference_calls)
        (fun () ->
          incr system_calls;
          !system_calls);
    ]
  in
  let gen = Stateful.program ~steps:2 ~domains:2 commands in
  let reset () =
    reference_calls := 0;
    system_calls := 0
  in
  let tree =
    find ~reset gen (fun record ->
        match calls record with
        | ("", "let m1 = create ()") :: ("", "get m1") :: (_ :: _ as rest) ->
            List.for_all (fun (d, _) -> d <> "") rest
        | _ -> false)
  in
  reset ();
  let ending = ended (execute (value tree)) in
  starts_with ~affix:"oracle [reference of call 2 of " ending;
  ends_with
    ~affix:
      "a replay of the reference differs from this run; the reference must \
       behave the same from run to run\""
    ending

let invariants () =
  let checks = ref 0 in
  let counter = abstract "c" ~invariant:(fun _ _ -> incr checks) in
  let commands =
    [
      command "create"
        (Gen.unit @-> makes counter)
        (fun () -> ref 0)
        (fun () -> ref 0);
      command "incr" (counter ^-> returns unit) incr incr;
    ]
  in
  let gen = Stateful.program ~steps:3 ~domains:2 commands in
  let tree =
    find gen (fun record ->
        match calls record with
        | ("", "let c1 = create ()")
          :: ("", "incr c1")
          :: (("1" | "2"), _)
          :: rest ->
            List.exists (fun (d, _) -> d = "") rest
        | _ -> false)
  in
  checks := 0;
  ignore (recorded gen (value tree));
  equal int 2 !checks

let released () =
  let released = ref [] in
  let counter = abstract "c" ~release:(fun c -> released := !c :: !released) in
  let made = ref 0 in
  let commands =
    [
      command "create"
        (Gen.unit @-> makes counter)
        ignore
        (fun () ->
          incr made;
          ref !made);
      command "tick" (counter ^-> returns unit) ignore ignore;
    ]
  in
  let gen = Stateful.program ~steps:2 ~domains:2 commands in
  let tree =
    find gen (fun record ->
        match calls record with
        | ("", "let c1 = create ()") :: ("", "let c2 = create ()") :: parallel
          ->
            parallel <> [] && List.for_all (fun (d, _) -> d <> "") parallel
        | _ -> false)
  in
  made := 0;
  released := [];
  ignore (recorded gen (value tree));
  equal (list int) [ 1; 2 ] !released

let judging =
  group "Judging runs"
    [
      test
        "a judges reference receives the system's recorded outcome in every \
         replay"
        judged;
      test "every run is judged on its own outcomes" witnessed;
      test "a judges call in the prefix is replayed on each run's outcome"
        prefix_judged;
      test "a reference that drifts from the run breaks, and blames no system"
        drifting;
      test "the invariant runs after the prefix's calls only" invariants;
      test "every system side a run made is released, newest first" released;
    ]

(* Judging calls *)

(* A queue whose [pop] always gives [Some popped]: beside a push of
   [popped] in parallel, the outcome of a run in which the push went first.
   The reference is the queue's contents, and [!judge] rules on each pop. *)
let lying_queue ~popped judge =
  let queue = abstract "q" in
  [
    command "create" (Gen.unit @-> makes queue) (fun () -> ref []) ignore;
    command "push"
      (queue ^-> Gen.int_range 0 9 @-> returns unit)
      (fun q x -> q := !q @ [ x ])
      (fun () _ -> ());
    command "pop"
      (queue ^-> judges (option int))
      (fun q seen -> !judge q seen)
      (fun () -> Some popped);
  ]

(* A judge that rejects, with a verb, every outcome it does not accept. *)
let strict q = function
  | Ok None -> equal (list int) [] !q
  | Ok (Some v) -> (
      match !q with
      | [] -> failf "pop gave %d from an empty queue" v
      | x :: rest ->
          equal int x v;
          q := rest)
  | Error e -> raise e

(* A judge that reads the queue without checking it. *)
let careless q = function
  | Ok None -> equal (list int) [] !q
  | Ok (Some v) ->
      equal int (List.hd !q) v;
      q := List.tl !q
  | Error e -> raise e

(* The descendant of [tree] that taking the first child whose record
   [accept] holds reaches, run on the calling domain. *)
let rec smallest gen tree accept =
  match
    Seq.find
      (fun child -> accept (recorded gen (value child)))
      (Shrink_tree.children tree)
  with
  | None -> tree
  | Some child -> smallest gen child accept

(* The program [create], then [pop] in branch 1 beside [push 0] in branch 2,
   under a strict judge until [judge] is set. *)
let pop_beside_push ~popped judge =
  let gen = Stateful.program ~steps:2 ~domains:2 (lying_queue ~popped judge) in
  let accept record =
    let calls = calls record in
    List.mem ("1", "pop q1") calls
    && List.exists
         (fun (d, c) -> d = "2" && String.starts_with ~prefix:"push q1" c)
         calls
  in
  let tree = smallest gen (find gen accept) accept in
  equal
    (list (pair string string))
    [ ("", "let q1 = create ()"); ("1", "pop q1"); ("2", "push q1 0") ]
    (calls (recorded gen (value tree)));
  (gen, tree)

let rejected_order () =
  let judge = ref strict in
  let _, tree = pop_beside_push ~popped:0 judge in
  equal string "returned" (ended (execute (value tree)))

let rejected_everywhere () =
  let judge = ref strict in
  let _, tree = pop_beside_push ~popped:7 judge in
  equal string
    "failure [no order of the calls gives these results\n\
     the closest order, 3 then 2, differs at call 2: pop q1] equality 0, 7"
    (ended (execute (value tree)))

let crashed_order () =
  let judge = ref strict in
  let _, tree = pop_beside_push ~popped:0 judge in
  judge := careless;
  equal string
    {|oracle [reference of call 2 of 3, in the order 2 then 3: pop q1] raise actual Failure("hd")|}
    (ended (execute (value tree)))

(* With the pop moved to the suffix and the push deleted, no call is
   parallel and one order remains. *)
let crashed_alone () =
  let judge = ref strict in
  let gen, tree = pop_beside_push ~popped:0 judge in
  let moved record =
    calls record
    = [ ("", "let q1 = create ()"); ("2", "push q1 0"); ("", "pop q1") ]
  in
  let alone record =
    calls record = [ ("", "let q1 = create ()"); ("", "pop q1") ]
  in
  let tree = smallest gen (smallest gen tree moved) alone in
  judge := careless;
  equal string
    {|oracle [reference of call 2 of 2: pop q1] raise actual Failure("hd")|}
    (ended (execute (value tree)))

let judging_calls =
  group "Judging calls"
    [
      test "a rejection rules an order out, and the judge tries the next"
        rejected_order;
      test "a judge that rejects in every order fails at the closest"
        rejected_everywhere;
      test
        "a crash breaks the reference in an order another would explain, and \
         names that order"
        crashed_order;
      test "a crash names no order when no call is parallel" crashed_alone;
    ]

(* Workers *)

let armed = Atomic.make false

(* [odd] runs [misbehave] when [armed], and returns otherwise. *)
let odd misbehave =
  command "odd"
    (Gen.unit @-> returns unit)
    ignore
    (fun () -> if Atomic.get armed then misbehave ())

let tick_command = command "tick" (Gen.unit @-> returns unit) ignore ignore

(* A program with [odd] in branch 1 and calls in branch 2. *)
let with_odd misbehave =
  let gen =
    Stateful.program ~steps:1 ~domains:2 [ tick_command; odd misbehave ]
  in
  Atomic.set armed false;
  let tree =
    find gen (fun record ->
        let c = calls record in
        List.mem ("1", "odd ()") c && List.exists (fun (d, _) -> d = "2") c)
  in
  (gen, value tree)

let armed_run ?workers program =
  Atomic.set armed true;
  Fun.protect ~finally:(fun () -> Atomic.set armed false) @@ fun () ->
  ended (execute ?workers program)

let branch_failure () =
  let gen, program = with_odd (fun () -> fail "odd") in
  Atomic.set armed true;
  let alone = recorded gen program in
  Atomic.set armed false;
  with_pool 2 @@ fun workers ->
  starts_with ~affix:"failure [call " (armed_run ~workers program);
  Atomic.set armed true;
  equal string alone (recorded ~workers gen program);
  Atomic.set armed false

let other_domain_error =
  "a function that reads the running test was called from a domain other than \
   the one running the tests; hand the result back to the test's domain"

let reads_the_test () =
  let _, program = with_odd (fun () -> ignore (current_test ())) in
  with_pool 2 @@ fun workers ->
  let ending = armed_run ~workers program in
  starts_with ~affix:"failure [call " ending;
  ends_with ~affix:(strf "message %S" other_domain_error) ending

let controls () =
  let _, skipping = with_odd (fun () -> skip ~reason:"why" ()) in
  let _, breaking = with_odd (fun () -> raise Sys.Break) in
  let _, exhausted = with_odd (fun () -> raise Out_of_memory) in
  let _, fine = with_odd ignore in
  with_pool 2 @@ fun workers ->
  let ending program =
    Atomic.set armed true;
    Fun.protect ~finally:(fun () -> Atomic.set armed false) @@ fun () ->
    match Stateful.execute ~workers program with
    | () -> "returned"
    | exception Failure.Control (`Skip (Some why)) -> "skip " ^ why
    | exception Sys.Break -> "Sys.Break"
    | exception Out_of_memory -> "Out_of_memory"
    | exception e -> Printexc.to_string e
  in
  equal (list string)
    [ "skip why"; "Sys.Break"; "Out_of_memory"; "returned" ]
    (List.map ending [ skipping; breaking; exhausted; fine ])

exception Worker_raised

let backtrace () =
  let _, program = with_odd (fun () -> raise Worker_raised) in
  with_pool 2 @@ fun workers ->
  Atomic.set armed true;
  let f =
    Fun.protect ~finally:(fun () -> Atomic.set armed false) @@ fun () ->
    match Stateful.execute ~workers program with
    | () -> None
    | exception Failure.Check_failure f -> Some f
  in
  match require_some f with
  | { kind = Raise { backtrace = Some bt; _ }; _ } ->
      contains ~sub:"test_stateful_domains.ml" bt.Failure.kept
  | f -> fail ("a raise with a backtrace: " ^ kind f)

let printed_output () =
  let _, program = with_odd (fun () -> Format.printf "printed on a worker") in
  with_pool 2 @@ fun workers ->
  ignore (output ());
  equal string "returned" (armed_run ~workers program);
  contains ~sub:"printed on a worker" (output ())

let alarmed () =
  if Sys.win32 then skip ~reason:"no limit is enforced on Windows" ();
  let once = Atomic.make true in
  let _, program =
    with_odd (fun () ->
        if Atomic.exchange once false then
          Unix.kill (Unix.getpid ()) Sys.sigalrm)
  in
  with_pool 2 @@ fun workers ->
  Atomic.set once true;
  Atomic.set armed true;
  let ending =
    Fun.protect ~finally:(fun () -> Atomic.set armed false) @@ fun () ->
    match Stateful.execute ~workers program with
    | () -> "returned"
    | exception Failure.Control (`Timeout limit) ->
        strf "timed out after %gs" limit
  in
  equal string "timed out after 60s" ending

(* A worker's own mask, and that of a domain it spawns, read from a job. *)
let masked () =
  if Sys.win32 then skip ~reason:"Windows has no signal masks" ();
  with_pool 2 @@ fun workers ->
  let mask () = Unix.sigprocmask Unix.SIG_BLOCK [] in
  let masks = Array.make 2 [] and spawned = Array.make 2 [] in
  let job i () =
    masks.(i) <- mask ();
    spawned.(i) <- Domain.join (Domain.spawn mask)
  in
  Workers.run workers ~grace:(Fun.const 0.) [| job 0; job 1 |];
  let blocks mask =
    List.for_all
      (fun signal -> List.mem signal mask)
      [ Sys.sigalrm; Sys.sigint; Sys.sigterm; Sys.sighup ]
  in
  is_true ~msg:"every worker blocks the runner's signals"
    (Array.for_all blocks masks);
  is_true ~msg:"so does a domain a worker spawns" (Array.for_all blocks spawned);
  is_false ~msg:"the test's domain blocks none of them"
    (List.mem Sys.sigalrm (mask ()))

exception Job of int

(* Jobs 1 and 2 raise; every job runs to its end, and the pool lives on. *)
let first_raise () =
  with_pool 3 @@ fun workers ->
  let ended = Array.make 3 false in
  let job i () =
    ended.(i) <- true;
    if i > 0 then raise (Job i)
  in
  let ending =
    match Workers.run workers ~grace:(Fun.const 0.) (Array.init 3 job) with
    | () -> "returned"
    | exception Job i -> strf "raised Job %d" i
  in
  equal
    (pair string (list bool))
    ("raised Job 1", [ true; true; true ])
    (ending, Array.to_list ended);
  Workers.run workers ~grace:(Fun.const 0.) (Array.make 3 ignore)

(* [first] waits, on a worker, until [second] has run, so the one order
   that explains its outcome runs [second] first, which the judge tries
   second. *)
type meeting = { second_ran : bool Atomic.t }

let meeting_commands ~cover_when =
  let meeting = abstract "m" in
  [
    command "create"
      (Gen.unit @-> makes meeting)
      (fun () -> ref [])
      (fun () -> { second_ran = Atomic.make false });
    command "first"
      (meeting ^-> Gen.unit @-> returns bool)
      (fun l () ->
        let saw = List.mem "second" !l in
        if saw = cover_when then
          cover (strf "first, second before: %b" saw) false;
        l := "first" :: !l;
        saw)
      (fun m () ->
        if self () <> Atomic.get main_domain then
          while not (Atomic.get m.second_ran) do
            Domain.cpu_relax ()
          done;
        Atomic.get m.second_ran);
    command "second"
      (meeting ^-> Gen.unit @-> returns unit)
      (fun l () -> l := "second" :: !l)
      (fun m () -> Atomic.set m.second_ran true);
  ]

(* [tree] shrunk while its record on the calling domain keeps [keep], as
   the search of a failing case descends. *)
let rec shrunk gen keep tree =
  let kept child = keep (recorded gen (value child)) in
  match Seq.find kept (Shrink_tree.children tree) with
  | Some child -> shrunk gen keep child
  | None -> tree

let met ~cover_when () =
  let gen =
    Stateful.program ~steps:1 ~domains:2 (meeting_commands ~cover_when)
  in
  let exact record =
    calls record
    = [
        ("", "let m1 = create ()"); ("1", "first m1 ()"); ("2", "second m1 ()");
      ]
  in
  let drawn_both record =
    let c = calls record in
    List.mem ("1", "first m1 ()") c && List.mem ("2", "second m1 ()") c
  in
  let tree = shrunk gen drawn_both (find gen drawn_both) in
  let program = value tree in
  is_true ~msg:"the program shrinks to create, first and second"
    (exact (recorded gen program));
  with_pool 2 @@ fun workers ->
  ended (fun () ->
      Run.property ~count:1
        (Gen.map (fun () -> program) Gen.unit)
        (fun p -> Stateful.execute ~workers p))

let labels =
  [
    ("an order the judge rejects counts nothing", false, "returned");
    ( "the order it accepts counts",
      true,
      {|failure message "never covered: \"first, second before: true\" (over 1 passing cases)"|}
    );
  ]

(* [create]'s system prints the number of the case's run, and [check]
   fails in its third run, so a failing case runs three times. *)
let failing_run_output () =
  let runs = Atomic.make 0 in
  let r = abstract "r" in
  let gen =
    Stateful.program ~steps:1 ~domains:2
      [
        command "create"
          (Gen.unit @-> makes r)
          ignore
          (fun () ->
            let n = Atomic.fetch_and_add runs 1 + 1 in
            Printf.printf "run %d\n" n;
            n);
        command "check" (r ^-> returns bool) (fun () -> true) (fun n -> n <> 3);
      ]
  in
  with_pool 2 @@ fun workers ->
  let law program =
    Atomic.set runs 0;
    Stateful.execute ~workers program
  in
  match Run.property gen law with
  | () -> fail "the property passed"
  | exception Failure.Check_failure f ->
      equal (option string) (Some "run 3\n")
        (Option.map (fun (t : Failure.tail) -> t.text) f.output_tail)

(* On the workers a counterexample does not run again, though [check]
   fails in every run. *)
let not_run_again () =
  if mutating () then
    skip ~reason:"under mutation testing no test spawns a domain" ();
  let r = abstract "r" in
  let t =
    Stateful.stateful ~steps:1 ~domains:2 "t"
      [
        command "create" (Gen.unit @-> makes r) ignore ignore;
        command "check" (r ^-> returns bool) (fun () -> true) (fun () -> false);
      ]
  in
  match body t () with
  | () -> fail "the test passed"
  | exception Failure.Check_failure { kind = Property p; _ } ->
      equal (option bool) None p.failed_again
  | exception Failure.Check_failure f -> fail ("another failure: " ^ kind f)

let executor =
  group "Workers"
    [
      test
        "branch i runs on worker i, the rest on the test's domain, the same in \
         every run"
        placed;
      test
        "a system's failure in a branch fails the case at its call, and the \
         other branches run"
        branch_failure;
      test
        "reading the running test from a branch fails the call, and says it \
         ran on another domain"
        reads_the_test;
      test
        "a control, Sys.Break and Out_of_memory from a branch are raised \
         again, and the workers live on"
        controls;
      test "an exception on a worker keeps its backtrace" backtrace;
      test "what a worker prints reaches the test's output" printed_output;
      test "a failing case's output is that of the run that failed"
        failing_run_output;
      test "a counterexample does not run again on the workers" not_run_again;
      test ~timeout:60.
        "an alarm handled on any domain times the test out on its own" alarmed;
      test "a worker blocks the runner's signals, so their handlers run here"
        masked;
      test
        "Workers.run raises the first job's exception, by index, once every \
         job has ended"
        first_raise;
      cases "labels count along the order the judge accepts"
        ~name:(fun (n, _, _) -> n)
        labels
        (fun (_, cover_when, expected) ->
          equal string expected (met ~cover_when ()));
    ]

(* Under mutation testing, recorded as the module initialises: a program
   runs once on the test's domain and nothing spawns. *)
let under_mutation, seen_under_mutation =
  let r =
    Recorded.execute
      ~config:(fun c -> { c with mutation = Run.Loop [] })
      [ Stateful.stateful ~count:5 ~domains:2 "sequential" [ where () ] ]
  in
  (r, List.sort_uniq Int.compare (Log.take ()))

(* One after the other, a control from branch 1 ends the run before any
   later call runs. *)
let stops_at_a_control () =
  let ticked = ref 0 in
  let tick =
    command "tick" (Gen.unit @-> returns unit) ignore (fun () -> incr ticked)
  in
  let gen =
    Stateful.program ~steps:1 ~domains:2
      [ tick; odd (fun () -> skip ~reason:"why" ()) ]
  in
  Atomic.set armed false;
  let tree =
    find gen (fun record ->
        match calls record with
        | ("1", "odd ()") :: rest ->
            List.mem ("2", "tick ()") rest
            && List.for_all
                 (fun (d, c) -> d = "1" || (d = "2" && c = "tick ()"))
                 rest
        | _ -> false)
  in
  ticked := 0;
  equal string "raised windtrap skip: why" (armed_run (value tree));
  equal int 0 !ticked

let mutation =
  group "Under mutation testing"
    [
      test "each program runs on the test's domain, and nothing spawns"
        (fun () ->
          equal string "pass" (Recorded.row under_mutation [ "sequential" ]);
          equal (list int) [ self () ] seen_under_mutation);
      test "a control from a branch ends the run before the next branch"
        stops_at_a_control;
    ]

(* A call that never returns *)

(* The run is forked before any domain exists: its worker sleeps on until
   the child leaves by [_exit], and hands back the run's facts on a pipe. *)
let in_fork fn =
  if Sys.win32 then None
  else begin
    Format.pp_print_flush Format.std_formatter ();
    Format.pp_print_flush Format.err_formatter ();
    flush stdout;
    flush stderr;
    let read_fd, write_fd = Unix.pipe ~cloexec:true () in
    match Unix.fork () with
    | 0 ->
        Unix.close read_fd;
        let text = try fn () with e -> "raised " ^ Printexc.to_string e in
        let oc = Unix.out_channel_of_descr write_fd in
        output_string oc text;
        close_out oc;
        Unix._exit 0
    | pid ->
        Unix.close write_fd;
        let ic = Unix.in_channel_of_descr read_fd in
        let text = In_channel.input_all ic in
        close_in ic;
        ignore (Unix.waitpid [] pid);
        Some text
  end

(* The timeout covers the whole body, the spawn of the workers included: it
   must outlast that spawn on a loaded builder, or it fires before the call
   starts, and the test then times out without a stuck call. *)
let stuck =
  in_fork (fun () ->
      let block =
        command "block"
          (Gen.unit @-> returns unit)
          ignore
          (fun () -> Unix.sleepf 3600.)
      in
      let r =
        Recorded.execute
          [
            Stateful.stateful ~steps:0 ~domains:2 ~count:1 ~timeout:1. "stuck"
              [ block ];
            test "after" ignore;
          ]
      in
      let stopped =
        match Run.stopped (Recorded.outcome r).run with
        | Some path -> String.concat " / " path
        | None -> "none"
      in
      let lines =
        [
          "stuck: " ^ Recorded.row r [ "stuck" ];
          "failures: "
          ^ String.concat ", " (List.map kind (Recorded.failures r [ "stuck" ]));
          "executed: " ^ String.concat ", " (Recorded.executed r);
          "stopped after: " ^ stopped;
          "exit: " ^ string_of_int (Recorded.exit_code r);
        ]
      in
      Scratch.remove_tree (Filename.dirname (Recorded.log_dir r));
      String.concat "\n" lines)

(* More workers than the runtime has domains, then a test that spawns two. *)
let unspawnable =
  in_fork (fun () ->
      let r =
        Recorded.execute
          [
            Stateful.stateful ~count:1 ~domains:200 "too many" [ tick ];
            Stateful.stateful ~count:1 ~domains:2 "two" [ tick ];
          ]
      in
      let failure (f : Failure.t) =
        match f.kind with
        | Message m -> m.kept
        | _ -> "not a message: " ^ kind f
      in
      let lines =
        [
          "too many: " ^ Recorded.row r [ "too many" ];
          "failures: "
          ^ String.concat ", "
              (List.map failure (Recorded.failures r [ "too many" ]));
          "two: " ^ Recorded.row r [ "two" ];
        ]
      in
      Scratch.remove_tree (Filename.dirname (Recorded.log_dir r));
      String.concat "\n" lines)

let spawn_fails () =
  match unspawnable with
  | None -> skip ~reason:"POSIX only: the run is forked" ()
  | Some text -> (
      match String.split_on_char '\n' text with
      | [ too_many; failures; two ] ->
          equal string "too many: fail body" too_many;
          starts_with ~affix:"failures: cannot spawn a worker domain: " failures;
          equal string "two: pass" two
      | _ -> fail text)

let never_returns () =
  match stuck with
  | None -> skip ~reason:"POSIX only: the run is forked" ()
  | Some text ->
      equal string
        "stuck: fail body\n\
         failures: timeout\n\
         executed: stuck\n\
         stopped after: stuck\n\
         exit: 1"
        text

let stopping =
  group "Spawning and stopping"
    [
      test
        "a call that never returns fails its test as timed out and stops the \
         run after it, as data"
        never_returns;
      test "a spawn that fails fails the test, and the next test spawns"
        spawn_fails;
    ]

let () =
  exit
    (run "stateful_domains"
       [
         checking;
         drawing;
         shrinking;
         judge_group;
         judging;
         judging_calls;
         report;
         executor;
         mutation;
         stopping;
       ])
