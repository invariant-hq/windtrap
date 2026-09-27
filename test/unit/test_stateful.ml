(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Every abstract type, command and program under test is built inside a test
   body, and a row holds it as a function: a mutant of stateful.ml is armed
   only inside a test. *)

open Windtrap
module Failure = Windtrap.Private.Failure
module Gen_engine = Windtrap.Private.Gen_engine
module Loc = Windtrap.Private.Loc
module Property = Windtrap.Private.Property
module Sections = Windtrap.Private.Report_sections
module Seed = Windtrap.Private.Seed
module Shrink_tree = Windtrap.Private.Gen_engine.Shrink_tree
module Stateful = Windtrap.Private.Stateful
module Test_tree = Windtrap.Private.Test_tree

let strf = Printf.sprintf

(* The facade's types are abstract, so the commands under test are built with
   the module's own values. *)
let abstract = Stateful.abstract
let ( @-> ) = Stateful.( @-> )
let ( ^-> ) = Stateful.( ^-> )
let returns = Stateful.returns
let makes = Stateful.makes
let judges = Stateful.judges
let command = Stateful.command
let among = Stateful.among

(* Drawing and reading programs *)

let root = 0x00c0ffee1234abcdL

let state ?(path = "test_stateful") index =
  Seed.make (Seed.derive ~root ~path ~index)

let drawn ?path gen index = Gen_engine.sample gen (state ?path index)
let value node = Gen_engine.value (Shrink_tree.root node)
let printed gen program = Gen_engine.render_value gen program

(* The functions under test note what they see. *)
let noted = ref []
let note text = noted := text :: !noted

let notes fn =
  noted := [];
  fn ();
  List.rev !noted

(* Failures as rows *)

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
  | Containment _ -> "containment"
  | Baseline _ -> "baseline"
  | Property _ -> "property"
  | Law _ -> "law"
  | Timeout _ -> "timeout"

let label (f : Failure.t) =
  match f.msg with None -> "" | Some m -> strf "[%s] " m.kept

let row f = label f ^ kind f

(* How [fn ()] ended: it returned, failed, broke the reference or raised. *)
let ended fn =
  match fn () with
  | () -> "returned"
  | exception Failure.Check_failure f -> "failure " ^ row f
  | exception Property.Oracle_failure f -> "oracle " ^ row f
  | exception e -> "raised " ^ Failure.exn_to_string e

let raised e = ended (fun () -> raise e)
let execute program () = Stateful.execute program

let failure fn =
  match fn () with
  | () -> None
  | exception (Failure.Check_failure f | Property.Oracle_failure f) -> Some f
  | exception _ -> None

let site (loc : Loc.t option) =
  match loc with None -> "no site" | Some l -> strf "%s:%d" l.file l.line

let failure_site fn = site (Option.bind (failure fn) (fun f -> f.Failure.loc))
let here (_, line, _, _) = strf "%s:%d" __FILE__ line

(* [recorded gen program] runs [program] after [reset ()] and is its record,
   however the run ended. *)
let recorded ?(reset = ignore) gen program =
  reset ();
  ignore (ended (execute program) : string);
  printed gen program

(* The number of calls of a record. *)
let rows record = List.length (String.split_on_char '\n' record) - 1

(* The calls of a record without a [reference before] column. *)
let calls record =
  let call row =
    let row = String.trim row in
    match String.index_opt row ' ' with
    | None -> row
    | Some i -> String.trim (String.sub row i (String.length row - i))
  in
  match String.split_on_char '\n' record with
  | [] | [ _ ] -> []
  | _header :: rows -> List.map call rows

(* The first program of [gen] whose record, once run, [accept] holds. *)
let find ?reset gen accept =
  let at index =
    let tree = drawn gen index in
    if accept (recorded ?reset gen (value tree)) then Some tree else None
  in
  require_some ~msg:"a program among the first 1000 meets the premise"
    (Seq.find_map at (Seq.init 1000 Fun.id))

let one_call command = value (drawn (Stateful.program ~steps:1 [ command ]) 0)

(* Commands *)

let unit_call ?__POS__ ?pre name reference system =
  command ?__POS__ ?pre name (Gen.unit @-> returns unit) reference system

let int_call ?__POS__ ?pre name reference system =
  command ?__POS__ ?pre name (Gen.unit @-> returns int) reference system

let tick () = unit_call "tick" ignore ignore
let dead name = unit_call name ~pre:(fun () -> false) ignore ignore
let ticks steps = Stateful.program ~steps [ tick () ]
let made name t = command name (Gen.unit @-> makes t) ignore ignore

(* The controls, and the two exceptions that [Failure.catch] never returns. *)
let controls =
  [
    ("a skip", Failure.Control (`Skip (Some "why")));
    ("a timeout", Failure.Control (`Timeout 0.5));
    ("an exit", Failure.Control `Exit);
    ("Sys.Break", Sys.Break);
    ("Out_of_memory", Out_of_memory);
  ]

let asserted ?msg text =
  let f = Failure.message text in
  Failure.Check_failure { f with msg = Option.map Failure.text msg }

let discarded = "assume or reject in a command; a call's legality is its ~pre"

(* Checking the command list *)

let lowercase prefix =
  strf
    "Windtrap.stateful: the prefix '%s' of an abstract type is not a lowercase \
     OCaml identifier"
    prefix

(* [create] makes a value of [d], which lists one index, and [build d index]
   is one more command. *)
let with_index build () =
  let d = abstract "d" in
  let index = among int d (fun () -> [ 0 ]) in
  [ made "create" d; build d index ]

let unlisted_element element listing =
  strf
    "Windtrap.stateful: get takes %s without %s; an element is listed by a \
     value its call takes"
    element listing

let malformed =
  let prefixed prefix () = [ made "make" (abstract prefix) ] in
  [
    ("no command", (fun () -> []), "Windtrap.stateful: no commands to draw from");
    ("an empty prefix", prefixed "", lowercase "");
    ("an uppercase prefix", prefixed "Q", lowercase "Q");
    ("a prefix that starts with a digit", prefixed "1q", lowercase "1q");
    ("a prefix with a dash", prefixed "q-r", lowercase "q-r");
    ( "a prefix of a type that no command makes",
      (fun () ->
        [ command "use" (abstract "Q" ^-> returns unit) ignore ignore ]),
      lowercase "Q" );
    ( "a prefix that ends with a digit",
      prefixed "q1",
      "Windtrap.stateful: the prefix 'q1' of an abstract type ends with a digit"
    );
    ( "two types of one prefix",
      (fun () -> [ made "a" (abstract "q"); made "b" (abstract "q") ]),
      "Windtrap.stateful: two abstract types have the prefix 'q'" );
    ( "an element with a value of another type only",
      with_index (fun _ index ->
          let o = abstract "o" in
          command "get"
            (o ^-> index ^-> returns unit)
            (fun () _ -> ())
            (fun () _ -> ())),
      unlisted_element "an element of 'd'" "a value of 'd'" );
    ( "an element with a drawn argument only",
      with_index (fun _ index ->
          command "get"
            (Gen.int_range 0 3 @-> index ^-> returns unit)
            (fun _ _ -> ())
            (fun _ _ -> ())),
      unlisted_element "an element of 'd'" "a value of 'd'" );
    ( "an element of another type beside an element",
      with_index (fun d index ->
          let o = abstract "o" in
          let other = among int o (fun () -> [ 0 ]) in
          command "get"
            (d ^-> index ^-> other ^-> returns unit)
            (fun () _ _ -> ())
            (fun () _ _ -> ())),
      unlisted_element "an element of 'o'" "a value of 'o'" );
    ( "an element of an element with a value only",
      with_index (fun d index ->
          let cell = among int index (fun i -> [ i ]) in
          command "get"
            (d ^-> cell ^-> returns unit)
            (fun () _ -> ())
            (fun () _ -> ())),
      unlisted_element "an element of an element of 'd'" "an element of 'd'" );
    ( "a command that makes an element",
      with_index (fun d index ->
          command "pick" (d ^-> makes index) (fun () -> 0) (fun () -> 0)),
      "Windtrap.stateful: pick makes an element of 'd'; an element is listed \
       by a value, never made" );
  ]

let malformed_rows =
  List.concat_map
    (fun (n, commands, message) ->
      [
        (n, 20, commands, message); (n ^ " under steps 0", 0, commands, message);
      ])
    malformed
  @ [
      ( "a negative steps",
        -1,
        (fun () -> [ tick () ]),
        "Windtrap.stateful: negative steps" );
    ]

let accepted prefix =
  let gen = Stateful.program ~steps:1 [ made "make" (abstract prefix) ] in
  equal string
    (strf " #  call\n 1  let %s1 = make ()" prefix)
    (recorded gen (value (drawn gen 0)))

let one_type_two_commands () =
  let q = abstract "q" in
  let gen = Stateful.program ~steps:2 [ made "a" q; made "b" q ] in
  equal int 2 (rows (recorded gen (value (drawn gen 0))))

let single tree =
  require_match
    (function [ c ] -> Some c | _ -> None)
    (Test_tree.flatten [ tree ])

let body tree =
  require_match
    (function Test_tree.Body f -> Some f | Test_tree.Scoped _ -> None)
    (single tree).Test_tree.body

let checking =
  group "Checking the command list"
    [
      cases "sampling raises Invalid_argument with the rule the list breaks"
        ~name:(fun (n, _, _, _) -> n)
        malformed_rows
        (fun (_, steps, commands, message) ->
          let gen = Stateful.program ~steps (commands ()) in
          raises (Invalid_argument message) (fun () -> drawn gen 0));
      cases "a prefix is any lowercase identifier that ends with no digit"
        ~name:Fun.id
        [ "q'"; "_q"; "cell_a"; "q1a" ]
        accepted;
      test "one type that two commands make is one prefix" one_type_two_commands;
      test "stateful raises in its body, before any case, under count 0 too"
        (fun () ->
          let t = Stateful.stateful ~count:0 "t" [] in
          raises
            (Invalid_argument "Windtrap.stateful: no commands to draw from")
            (body t));
      test
        "stateful refuses an element without a value to list it in its body, \
         before any case" (fun () ->
          let unlisted _ index =
            command "get"
              (Gen.unit @-> index ^-> returns unit)
              (fun () _ -> ())
              (fun () _ -> ())
          in
          let t = Stateful.stateful ~count:0 "t" (with_index unlisted ()) in
          raises
            (Invalid_argument
               (unlisted_element "an element of 'd'" "a value of 'd'"))
            (body t));
    ]

(* Commands *)

let captured_site () =
  let fn = Gen.unit @-> returns int and one () = 1 and two () = 2 in
  let pos, c = (__POS__, command "f" fn one two) in
  equal string (here pos) (failure_site (execute (one_call c)))

(* The failures are raised directly: [Check]'s capture succeeds in this test
   and would give every failure a location. *)
let located =
  let body loc () =
    raise
      (Failure.Check_failure
         (Failure.equality ?loc ~expected:"1" ~actual:"2" ()))
  in
  let with_location = Some (Loc.of_pos ("body.ml", 9, 0, 4)) in
  [
    ( "a mismatch, which records none",
      (fun __POS__ -> int_call ~__POS__ "f" (fun () -> 1) (fun () -> 2)),
      "declared.ml:42" );
    ( "a failure without a location",
      (fun __POS__ -> unit_call ~__POS__ "f" ignore (body None)),
      "declared.ml:42" );
    ( "a failure with one",
      (fun __POS__ -> unit_call ~__POS__ "f" ignore (body with_location)),
      "body.ml:9" );
    ( "a broken reference",
      (fun __POS__ -> unit_call ~__POS__ "f" (body None) ignore),
      "declared.ml:42" );
  ]

let located_site (_, declare, _) =
  let c = declare ("declared.ml", 42, 7, 11) in
  failure_site (execute (one_call c))

let flattened_name () =
  let gen =
    Stateful.program ~steps:1
      [ int_call "two\nlines" (fun () -> 1) (fun () -> 2) ]
  in
  let p = value (drawn gen 0) in
  let failed = require_some (failure (execute p)) in
  let summary = Option.value ~default:"no summary" (Stateful.summary p) in
  let label = (require_some failed.msg).kept in
  expect_exact (String.concat "\n" [ printed gen p; summary; label ])
  @@ __POS_OF__
       {| #  call
 1  two lines ()
1 call, last: two lines
call 1 of 1: two lines ()|}

let commands =
  group "Commands"
    [
      test "a command without pre is legal everywhere" (fun () ->
          let gen = ticks 12 in
          equal int 12 (rows (recorded gen (value (drawn gen 0)))));
      test "command defaults its location to the line that applies it"
        captured_site;
      cases
        "a failure that recorded no location gets its command's, and one that \
         did keeps it"
        ~name:(fun (n, _, _) -> n)
        located
        (fun ((_, _, site) as r) -> equal string site (located_site r));
      test "a name's newlines become spaces in the record, summary and label"
        flattened_name;
    ]

(* Drawing *)

let index = Gen.int_range 0 1_000_000

(* Every command returns, so every call that the draw makes resolves. *)
let chain () =
  let s = abstract "s" and t = abstract "t" in
  [
    made "make" s;
    command "use" (s ^-> returns unit) ignore ignore;
    command "both"
      (s ^-> s ^-> returns unit)
      (fun () () -> ())
      (fun () () -> ());
    command "cast" (s ^-> makes t) ignore ignore;
    command "peek"
      (t ^-> s ^-> returns unit)
      (fun () () -> ())
      (fun () () -> ());
  ]

let resolving () =
  let gen = Stateful.program ~steps:15 (chain ()) in
  let run i = rows (recorded gen (value (drawn gen i))) in
  equal (list int) (List.init 200 (Fun.const 15)) (List.init 200 run)

(* A case holding [peek] alone holds [cast], which takes [s] and so holds
   [make]. *)
let transitive () =
  let s = abstract "s" and t = abstract "t" in
  let gen =
    Stateful.program ~steps:4
      [
        made "make" s;
        command "cast" (s ^-> makes t) ignore ignore;
        command "peek" (t ^-> returns unit) ignore ignore;
      ]
  in
  let run i = rows (recorded gen (value (drawn gen i))) in
  equal (list int) (List.init 100 (Fun.const 4)) (List.init 100 run)

let unmade () =
  let gen =
    Stateful.program ~steps:6
      [
        tick (); command "orphan" (abstract "t" ^-> returns unit) ignore ignore;
      ]
  in
  let drawn_calls i = calls (recorded gen (value (drawn gen i))) in
  let strays =
    List.filter
      (fun call -> not (String.equal call "tick ()"))
      (List.concat_map drawn_calls (List.init 50 Fun.id))
  in
  equal (list string) [] strays

let weights () =
  let a = unit_call "a" ignore ignore and b = unit_call "b" ignore ignore in
  let gen = Stateful.program ~steps:20 [ a; a; b ] in
  let all =
    List.concat_map
      (fun i -> calls (recorded gen (value (drawn gen i))))
      (List.init 30 Fun.id)
  in
  let count name = List.length (List.filter (String.equal name) all) in
  greater int ~than:0 (count "b ()");
  greater int ~than:(count "b ()") (count "a ()")

(* Four commands that take nothing, over 40 calls: a case calls every command
   of its subset. *)
let joining () =
  let names = [ "a"; "b"; "c"; "d" ] in
  let gen =
    Stateful.program ~steps:40
      (List.map (fun name -> unit_call name ignore ignore) names)
  in
  let joined i =
    let called = calls (recorded gen (value (drawn gen i))) in
    List.map (fun name -> List.mem (name ^ " ()") called) names
  in
  let all = List.concat_map joined (List.init 400 Fun.id) in
  let held = List.length (List.filter Fun.id all) in
  equal (float 0.05) 0.75 (float_of_int held /. float_of_int (List.length all))

(* One command stays out of a case's subset one time in four. *)
let none_joins () =
  let gen = ticks 5 in
  let run i = rows (recorded gen (value (drawn gen i))) in
  equal (list int) (List.init 100 (Fun.const 5)) (List.init 100 run)

(* The reference side of a value is its number, so a call notes the value it
   received. *)
let received () =
  let s = abstract "s" and count = ref 0 in
  let gen =
    Stateful.program ~steps:12
      [
        command "make"
          (Gen.unit @-> makes s)
          (fun () ->
            incr count;
            !count)
          ignore;
        command "use"
          (s ^-> returns unit)
          (fun n -> note (strf "use s%d" n))
          ignore;
      ]
  in
  List.iter
    (fun i ->
      let p = value (drawn gen i) in
      count := 0;
      let seen = notes (execute p) in
      let used =
        List.filter (String.starts_with ~prefix:"use") (calls (printed gen p))
      in
      equal ~msg:(strf "program %d" i) (list string) used seen)
    (List.init 40 Fun.id)

let differing a b =
  List.filter_map
    (fun (i, (x, y)) -> if String.equal x y then None else Some (i, x, y))
    (List.mapi (fun i pair -> (i, pair)) (List.combine a b))

(* The value that [use sN] takes. *)
let number call = int_of_string (String.sub call 5 (String.length call - 5))

(* Every argument is [()] or a value, so a candidate that runs as many calls
   as its parent moved one index. *)
let index_moves index =
  let s = abstract "s" in
  let gen =
    Stateful.program ~steps:8
      [ made "make" s; command "use" (s ^-> returns unit) ignore ignore ]
  in
  let tree = drawn gen index in
  let parent = calls (recorded gen (value tree)) in
  let moved child =
    let child = calls (recorded gen (value child)) in
    if List.length child <> List.length parent then None
    else
      match differing parent child with
      | [ moved ] -> Some moved
      | changes -> failf "a candidate moved %d calls" (List.length changes)
  in
  let moves = List.filter_map moved (List.of_seq (Shrink_tree.children tree)) in
  let newest i =
    List.length
      (List.filter
         (String.starts_with ~prefix:"let ")
         (List.filteri (fun j _ -> j < i) parent))
  in
  cover "an index moved" (moves <> []);
  List.iter
    (fun (_, before, after) -> greater int ~than:(number before) (number after))
    moves;
  let rec firsts seen = function
    | [] -> ()
    | (i, _, after) :: moves ->
        if not (List.mem i seen) then
          equal ~msg:(strf "call %d" (i + 1)) int (newest i) (number after);
        firsts (i :: seen) moves
  in
  firsts [] moves

let no_printer name commands =
  let gen = Stateful.program commands in
  let raised i =
    match drawn gen i with
    | _ -> None
    | exception Invalid_argument message -> Some message
  in
  equal (option string)
    (Some (name ^ " has no printer; attach one with Gen.with_pp"))
    (Seq.find_map raised (Seq.init 100 Fun.id))

let printerless =
  [
    ( "a constant",
      "put: argument 2",
      fun () ->
        [
          command "put"
            (Gen.unit @-> Gen.constant 3 @-> returns unit)
            (fun () _ -> ())
            (fun () _ -> ());
        ] );
    ( "an of_list",
      "put: argument 1",
      fun () ->
        [ command "put" (Gen.of_list [ 1; 2 ] @-> returns unit) ignore ignore ]
    );
    ( "after an abstract argument",
      "put: argument 2",
      fun () ->
        let s = abstract "s" in
        [
          made "make" s;
          command "put"
            (s ^-> Gen.constant 3 @-> returns unit)
            (fun () _ -> ())
            (fun () _ -> ());
        ] );
  ]

let drawing =
  group "Drawing"
    [
      test
        "a call is drawn only when its types have values, so a program whose \
         calls return runs its steps"
        resolving;
      test
        "a case holds the makers of every type its commands take, transitively"
        transitive;
      test "a command whose type no command makes is never drawn" unmade;
      test "a command listed twice is drawn more often than one listed once"
        weights;
      test "a case's subset holds each command three times in four" joining;
      test "a case that no command joins holds every command" none_joins;
      test "the record names the value that each call received" received;
      prop
        "an index candidate moves its argument to a newer value, the newest \
         first"
        index index_moves;
      cases
        "a printerless argument raises Invalid_argument when first drawn, \
         counted from one"
        ~name:(fun (n, _, _) -> n)
        printerless
        (fun (_, name, commands) -> no_printer name (commands ()));
    ]

(* Legality *)

let skipped () =
  let never =
    unit_call "never"
      ~pre:(fun () ->
        note "pre";
        false)
      (fun () -> note "reference")
      (fun () -> note "system")
  in
  let gen = Stateful.program ~steps:3 [ never ] in
  let p = value (drawn gen 0) in
  let seen = notes (execute p) in
  equal
    (pair (list string) string)
    ([ "pre"; "pre"; "pre" ], "(no calls)")
    (seen, printed gen p)

(* The system cannot count past [cap]: only [pre] keeps it there. *)
let capped () =
  let cap = 2 and c = abstract "c" in
  let gen =
    Stateful.program ~steps:12
      [
        command "new" (Gen.unit @-> makes c) (fun () -> ref 0) (fun () -> ref 0);
        command "inc"
          ~pre:(fun r -> !r < cap)
          (c ^-> returns int)
          (fun r ->
            incr r;
            !r)
          (fun r ->
            if !r >= cap then failwith "past the cap";
            incr r;
            !r);
      ]
  in
  let outcomes =
    List.init 100 (fun i -> ended (execute (value (drawn gen i))))
  in
  let short =
    List.filter
      (fun i ->
        let p = value (drawn gen i) in
        rows (recorded gen p) < 12)
      (List.init 100 Fun.id)
  in
  equal (list string) (List.init 100 (Fun.const "returned")) outcomes;
  greater int ~than:0 (List.length short)

let order () =
  let r =
    abstract "r"
      ~invariant:(fun () () -> note "invariant")
      ~release:(fun () -> note "release")
  in
  let c =
    command "open"
      ~pre:(fun () ->
        note "pre";
        true)
      (Gen.unit @-> makes r)
      (fun () -> note "reference")
      (fun () -> note "system")
  in
  equal (list string)
    [ "pre"; "system"; "reference"; "invariant"; "release" ]
    (notes (execute (one_call c)))

let orphaned () =
  let s = abstract "s" in
  let gen =
    Stateful.program ~steps:2
      [ made "make" s; command "use" (s ^-> returns unit) ignore ignore ]
  in
  let tree =
    find gen (String.equal " #  call\n 1  let s1 = make ()\n 2  use s1")
  in
  equal (list string)
    [ "(no calls)"; "(no calls)"; " #  call\n 1  let s1 = make ()" ]
    (List.map
       (fun child -> recorded gen (value child))
       (List.of_seq (Shrink_tree.children tree)))

(* The candidates of [make; make; make; use s2], in order. Deleting another
   [make] leaves [use] on the value of the second, whatever its name, and
   deleting the second leaves it on the newest value. *)
let kept_choice () =
  let s = abstract "s" in
  let gen =
    Stateful.program ~steps:4
      [ made "make" s; command "use" (s ^-> returns unit) ignore ignore ]
  in
  let makes n =
    List.init n (fun i -> strf "\n %d  let s%d = make ()" (i + 1) (i + 1))
  in
  let record ?use n =
    let use =
      match use with None -> [] | Some v -> [ strf "\n %d  use %s" (n + 1) v ]
    in
    String.concat "" ((" #  call" :: makes n) @ use)
  in
  let tree = find gen (String.equal (record 3 ~use:"s2")) in
  equal (list string)
    [
      "(no calls)";
      record 1 ~use:"s1";
      record 2;
      record 2 ~use:"s1";
      record 2 ~use:"s2";
      record 2 ~use:"s2";
      record 3;
      record 3 ~use:"s3";
    ]
    (List.map
       (fun child -> recorded gen (value child))
       (List.of_seq (Shrink_tree.children tree)))

let legality =
  group "Legality"
    [
      test
        "a call whose pre fails runs neither side and is absent from the record"
        skipped;
      test
        "pre is asked of the reference as the run left it, so no call it \
         forbids reaches the system"
        capped;
      test
        "a call asks pre, runs the system, then the reference, then the \
         invariants, and the run releases"
        order;
      test
        "a candidate that deletes the only maker of a type skips the calls \
         that took its value"
        orphaned;
      test
        "a candidate that deletes another call keeps a choice on the value its \
         maker made, and one that deletes the maker takes the newest"
        kept_choice;
    ]

(* Outcomes *)

module A = struct
  exception Empty
  exception Full
end

module B = struct
  exception Empty
end

let outcome_rows =
  [
    ("two equal results", (fun () -> 1), (fun () -> 1), "returned");
    ( "two different results",
      (fun () -> 1),
      (fun () -> 2),
      "failure [call 1 of 1: f ()] equality 1, 2" );
    ( "one constructor in two modules",
      (fun () -> raise A.Empty),
      (fun () -> raise B.Empty),
      "returned" );
    ( "one constructor in Stdlib and in this module",
      (fun () -> raise Queue.Empty),
      (fun () -> raise B.Empty),
      "returned" );
    ( "one constructor, two payloads",
      (fun () -> failwith "a"),
      (fun () -> failwith "b"),
      "returned" );
    ( "two constructors",
      (fun () -> raise A.Empty),
      (fun () -> raise A.Full),
      "failure [call 1 of 1: f ()] raise expected Test_stateful.A.Empty actual \
       Test_stateful.A.Full" );
    ( "an exception where a result",
      (fun () -> raise A.Full),
      (fun () -> 1),
      "failure [call 1 of 1: f ()] raise expected Test_stateful.A.Full" );
    ( "a result where an exception",
      (fun () -> 1),
      (fun () -> raise A.Full),
      "failure [call 1 of 1: f ()] raise actual Test_stateful.A.Full" );
  ]

let witness_order () =
  let seen = ref [] in
  let w =
    Testable.make ~pp:Format.pp_print_int ~equal:(fun a b ->
        seen := (a, b) :: !seen;
        true)
  in
  let c = command "f" (Gen.unit @-> returns w) (fun () -> 1) (fun () -> 2) in
  equal string "returned" (ended (execute (one_call c)));
  equal (list (pair int int)) [ (1, 2) ] !seen

let system_backtrace () =
  let[@inline never] system () = raise A.Full in
  let f =
    require_some
      (failure (execute (one_call (int_call "f" (fun () -> 1) system))))
  in
  let backtrace =
    match f.kind with
    | Raise { backtrace = Some bt; _ } -> Some bt.kept
    | _ -> None
  in
  contains ~sub:"test_stateful.ml" (require_some backtrace)

(* One call of [open], whose type releases what the system made. *)
let making reference system =
  let r = abstract "r" ~release:(fun s -> note (strf "released %d" s)) in
  let c = command "open" (Gen.int_range 2 2 @-> makes r) reference system in
  let gen = Stateful.program ~steps:1 [ c ] in
  let p = value (drawn gen 0) in
  let seen = notes (fun () -> note (ended (execute p))) in
  String.concat "; " (seen @ calls (printed gen p))

let making_rows =
  [
    ( "two results make a value",
      Fun.id,
      Fun.id,
      "released 2; returned; let r1 = open 2" );
    ( "two equal exceptions make none",
      (fun _ -> raise Not_found),
      (fun _ -> raise Not_found),
      "returned; open 2" );
    ( "a result where an exception makes a value",
      (fun _ -> raise Not_found),
      Fun.id,
      "released 2; failure [call 1 of 1: open 2] raise expected Not_found; let \
       r1 = open 2" );
    ( "an exception where a result makes none",
      Fun.id,
      (fun _ -> raise Not_found),
      "failure [call 1 of 1: open 2] raise actual Not_found; open 2" );
  ]

let outcomes =
  group "Outcomes"
    [
      cases
        "two results compare under the witness and two exceptions by \
         constructor name, the module path removed"
        ~name:(fun (n, _, _, _) -> n)
        outcome_rows
        (fun (_, reference, system, expected) ->
          equal string expected
            (ended (execute (one_call (int_call "f" reference system)))));
      test "the witness receives the reference's result first" witness_order;
      test "the system's exception keeps its backtrace" system_backtrace;
      cases
        "makes makes a value when the system returns, and the run releases it"
        ~name:(fun (n, _, _, _) -> n)
        making_rows
        (fun (_, reference, system, expected) ->
          equal string expected (making reference system));
    ]

(* Never outcomes *)

let nope () = fail "nope"
let broken () = raise (Assert_failure ("x.ml", 1, 2))
let unmatched () = raise (Match_failure ("x.ml", 3, 4))
let discarding () = assume false
let fine () = ()

let never_rows =
  let assertion =
    {|raise actual File "x.ml", line 1, characters 2-8: Assertion failed|}
  in
  let match_ =
    {|raise actual File "x.ml", line 3, characters 4-9: Pattern matching failed|}
  in
  let discard = strf "message %S" discarded in
  let call = "failure [call 1 of 1: f ()] " in
  let reference = "oracle [reference of call 1 of 1: f ()] " in
  let pre = "oracle [~pre of call 1 of 1: f ()] " in
  [
    ( "a verb's failure in the system",
      None,
      fine,
      nope,
      call ^ {|message "nope"|} );
    ( "one with a msg of two lines",
      None,
      fine,
      (fun () -> equal ~msg:"note\nmore" int 1 2),
      "failure [call 1 of 1: f (); note more] equality 1, 2" );
    ("Assert_failure in the system", None, fine, broken, call ^ assertion);
    ("Match_failure in the system", None, fine, unmatched, call ^ match_);
    ("assume in the system", None, fine, discarding, call ^ discard);
    ("reject in the system", None, fine, (fun () -> reject ()), call ^ discard);
    ( "a verb's failure in the system, whose reference raises",
      None,
      (fun () -> raise Not_found),
      nope,
      call ^ {|message "nope"|} );
    ( "a verb's failure in the reference",
      None,
      nope,
      fine,
      reference ^ {|message "nope"|} );
    ( "a verb's failure in the reference, whose system raises",
      None,
      nope,
      (fun () -> raise Not_found),
      reference ^ {|message "nope"|} );
    ( "Assert_failure in the reference",
      None,
      broken,
      fine,
      reference ^ assertion );
    ("Match_failure in the reference", None, unmatched, fine, reference ^ match_);
    ("assume in the reference", None, discarding, fine, reference ^ discard);
    ("a broken contract on both sides", None, broken, broken, call ^ assertion);
    ( "an exception in pre",
      Some (fun () -> raise Not_found),
      fine,
      fine,
      pre ^ "raise actual Not_found" );
    ( "a verb's failure in pre",
      Some (fun () -> fail "nope"),
      fine,
      fine,
      pre ^ {|message "nope"|} );
    ( "assume in pre",
      Some
        (fun () ->
          assume false;
          true),
      fine,
      fine,
      pre ^ discard );
  ]

let system_first () =
  let c =
    unit_call "f"
      (fun () -> note "reference")
      (fun () ->
        note "system";
        fail "nope")
  in
  equal (list string) [ "system" ]
    (notes (fun () -> ignore (ended (execute (one_call c)) : string)))

let broken_pre_row () =
  let c = unit_call "f" ~pre:(fun () -> raise Not_found) ignore ignore in
  let gen = Stateful.program ~steps:1 [ c ] in
  let p = value (drawn gen 0) in
  ignore (ended (execute p) : string);
  equal
    (pair string (option string))
    (" #  call\n 1  f ()", Some "1 call, last: f")
    (printed gen p, Stateful.summary p)

(* [raising_in place e] is a program whose [place] raises [e] once. *)
let raising_in place e =
  let raise_it _ = raise e in
  match place with
  | `Pre -> one_call (unit_call "f" ~pre:raise_it ignore ignore)
  | `Reference -> one_call (unit_call "f" raise_it ignore)
  | `System -> one_call (unit_call "f" ignore raise_it)
  | `Judging ->
      one_call
        (command "f" (Gen.unit @-> judges unit) (fun () _ -> raise e) ignore)
  | `Invariant ->
      one_call (made "f" (abstract "r" ~invariant:(fun () () -> raise e)))
  | `Release -> one_call (made "f" (abstract "r" ~release:raise_it))

let places =
  [
    ("pre", `Pre);
    ("the reference", `Reference);
    ("the system", `System);
    ("a judges reference", `Judging);
    ("an invariant", `Invariant);
    ("a release", `Release);
  ]

let passing_rows =
  List.concat_map
    (fun (place, p) ->
      List.map (fun (n, e) -> (strf "%s in %s" n place, p, e)) controls)
    places

(* A cell's printer raises only once armed, so that finding the program
   runs it safely. *)
let raising_cell e =
  let armed = ref false in
  let s = abstract "s" ~pp:(fun _ () -> if !armed then raise e) in
  let gen =
    Stateful.program ~steps:2
      [ made "make" s; command "use" (s ^-> returns unit) ignore ignore ]
  in
  let tree =
    find gen (fun record ->
        rows record = 2 && String.ends_with ~suffix:"use s1" record)
  in
  armed := true;
  ended (execute (value tree))

let never_outcomes =
  group "Never outcomes"
    [
      cases
        "a verb's failure, a broken contract or a discard fails at the call \
         from the system, and breaks the reference from the reference or pre"
        ~name:(fun (n, _, _, _, _) -> n)
        never_rows
        (fun (_, pre, reference, system, expected) ->
          equal string expected
            (ended (execute (one_call (unit_call "f" ?pre reference system)))));
      test "a never-outcome of the system ends its call before the reference"
        system_first;
      test "the call whose pre raised is the last of the record" broken_pre_row;
      cases "a control or a fatal exception passes as itself"
        ~name:(fun (n, _, _) -> n)
        passing_rows
        (fun (_, place, e) ->
          equal string (raised e) (ended (execute (raising_in place e))));
      cases "a control or a fatal exception passes a cell's printer as itself"
        ~name:fst controls (fun (_, e) ->
          equal string (raised e) (raising_cell e));
    ]

(* Judging *)

let judge reference system =
  one_call (command "take" (Gen.unit @-> judges int) reference system)

let judges_order () =
  let reference () seen =
    note
      (match seen with
      | Ok v -> strf "reference Ok %d" v
      | Error e -> "reference Error " ^ Printexc.to_string e)
  in
  let system raises () =
    note "system";
    if raises then raise Not_found else 1
  in
  equal
    (list (list string))
    [
      [ "system"; "reference Ok 1" ]; [ "system"; "reference Error Not_found" ];
    ]
    [
      notes (execute (judge reference (system false)));
      notes (execute (judge reference (system true)));
    ]

let verdict_rows =
  let call = "failure [call 1 of 1: take ()] " in
  let reference = "oracle [reference of call 1 of 1: take ()] " in
  let returning () = 1 and raising () = raise Not_found in
  (* A string made at run time: two raises, two values. *)
  let alike () = failwith (String.make 1 'a') in
  [
    ("returning accepts a result", (fun () _ -> ()), returning, "returned");
    ("returning accepts an exception", (fun () _ -> ()), raising, "returned");
    ( "a verb's failure rejects",
      (fun () _ -> fail "nope"),
      returning,
      call ^ {|message "nope"|} );
    ( "Assert_failure rejects",
      (fun () _ -> broken ()),
      returning,
      call
      ^ {|raise actual File "x.ml", line 1, characters 2-8: Assertion failed|}
    );
    ( "Match_failure rejects",
      (fun () _ -> unmatched ()),
      returning,
      call
      ^ {|raise actual File "x.ml", line 3, characters 4-9: Pattern matching failed|}
    );
    ( "the system's exception raised again rejects",
      (fun () -> function Ok _ -> () | Error e -> raise e),
      raising,
      call ^ "raise actual Not_found" );
    ( "the system's constant raised by the judge itself rejects",
      (fun () _ -> raise Not_found),
      raising,
      call ^ "raise actual Not_found" );
    ( "an exception built alike breaks the reference",
      (fun () _ -> alike ()),
      alike,
      reference ^ {|raise actual Failure("a")|} );
    ( "another exception breaks the reference",
      (fun () _ -> raise Not_found),
      returning,
      reference ^ "raise actual Not_found" );
    ( "assume breaks the reference",
      (fun () _ -> assume false),
      returning,
      reference ^ strf "message %S" discarded );
    ( "reject breaks the reference",
      (fun () _ -> reject ()),
      returning,
      reference ^ strf "message %S" discarded );
  ]

(* The system's exception raised again prints as under [returns]. *)
let reraised () =
  let[@inline never] system () = raise A.Full in
  let failed c = require_some (failure (execute (one_call c))) in
  let predicted = failed (int_call "take" (fun () -> 1) system) in
  let judged =
    failed
      (command "take"
         (Gen.unit @-> judges int)
         (fun () -> function Ok _ -> () | Error e -> raise e)
         system)
  in
  let backtrace (f : Failure.t) =
    match f.kind with
    | Raise { backtrace = Some bt; _ } -> Some bt.kept
    | _ -> None
  in
  equal string (row predicted) (row judged);
  contains ~sub:"test_stateful.ml" (require_some (backtrace judged));
  equal (option string) (backtrace predicted) (backtrace judged)

let judging =
  group "Judging"
    [
      test "the reference receives the system's outcome" judges_order;
      cases "returning accepts; a verb, a broken contract or a re-raise rejects"
        ~name:(fun (n, _, _, _) -> n)
        verdict_rows
        (fun (_, reference, system, expected) ->
          equal string expected (ended (execute (judge reference system))));
      test "a re-raise prints the system's exception with its backtrace"
        reraised;
    ]

(* Invariants *)

let invariant_trace () =
  let count = ref 0 in
  let r = abstract "r" ~invariant:(fun n () -> note (strf "invariant r%d" n)) in
  let c =
    command "open"
      (Gen.unit @-> makes r)
      (fun () ->
        incr count;
        note "open";
        !count)
      ignore
  in
  equal (list string)
    [
      "open";
      "invariant r1";
      "open";
      "invariant r1";
      "invariant r2";
      "open";
      "invariant r1";
      "invariant r2";
      "invariant r3";
    ]
    (notes (execute (value (drawn (Stateful.program ~steps:3 [ c ]) 0))))

let invariant_after_use () =
  let r = abstract "r" ~invariant:(fun () () -> note "invariant") in
  let gen =
    Stateful.program ~steps:2
      [ made "open" r; unit_call "tick" (fun () -> note "tick") ignore ]
  in
  let tree =
    find gen (String.equal " #  call\n 1  let r1 = open ()\n 2  tick ()")
  in
  equal (list string)
    [ "invariant"; "tick"; "invariant" ]
    (notes (execute (value tree)))

(* [checked ~visit f] runs three [open] calls, and [f] at the [visit]th check
   of the invariant: after call 1 it checks r1, after call 2 r1 and r2. *)
let checked ~visit f =
  let visits = ref 0 in
  let r =
    abstract "r" ~invariant:(fun () () ->
        incr visits;
        if !visits = visit then f ())
  in
  let gen = Stateful.program ~steps:3 [ made "open" r ] in
  let p = value (drawn gen 0) in
  let ending = ended (execute p) in
  strf "%s; a record of %d" ending (rows (printed gen p))

let invariant_failures =
  [
    ( "an assertion after call 1",
      1,
      asserted ~msg:"note" "nope",
      {|failure [after call 1 of 1, on r1; note] message "nope"; a record of 1|}
    );
    ( "an assertion on the second value",
      3,
      asserted "nope",
      {|failure [after call 2 of 2, on r2] message "nope"; a record of 2|} );
    ( "an exception",
      2,
      Not_found,
      "failure [after call 2 of 2, on r1] raise actual Not_found; a record of 2"
    );
    ( "a discard",
      4,
      Failure.Control `Discard,
      strf "failure [after call 3 of 3, on r1] message %S; a record of 3"
        discarded );
  ]

let sites =
  [
    ( "an exception in an invariant",
      `Invariant,
      (fun () -> raise Exit),
      "no site" );
    ( "a located failure in an invariant",
      `Invariant,
      (fun () -> fail ~__POS__:("invariant.ml", 3, 0, 5) "wrong"),
      "invariant.ml:3" );
    ("an exception in a release", `Release, (fun () -> raise Exit), "no site");
    ( "a located failure in a release",
      `Release,
      (fun () -> fail ~__POS__:("release.ml", 5, 0, 5) "wrong"),
      "release.ml:5" );
  ]

let own_site (_, place, f, _) =
  let pos = ("declared.ml", 7, 0, 3) in
  let r =
    match place with
    | `Invariant -> abstract "r" ~invariant:(fun () _ -> f ())
    | `Release -> abstract "r" ~release:(fun _ -> f ())
  in
  let c =
    command ~__POS__:pos "open" (Gen.unit @-> makes r) ignore (fun () -> ref ())
  in
  failure_site (execute (one_call c))

let invariants =
  group "Invariants"
    [
      test "every invariant runs after every call, on each value oldest first"
        invariant_trace;
      test "an invariant runs after a call that makes no value"
        invariant_after_use;
      cases "an invariant's failure ends the run under its label"
        ~name:(fun (n, _, _, _) -> n)
        invariant_failures
        (fun (_, visit, e, expected) ->
          equal string expected (checked ~visit (fun () -> raise e)));
      cases
        "an invariant's and a release's failure keep their own location, and \
         have none without one"
        ~name:(fun (n, _, _, _) -> n)
        sites
        (fun ((_, _, _, site) as r) -> equal string site (own_site r));
    ]

(* Releases *)

(* Three calls of [open], each making a value whose system side is its
   number. [third] says what goes wrong at the third call. *)
let released_on third =
  let n = ref 0 and references = ref 0 in
  let r =
    abstract "r"
      ~invariant:(fun () s -> if third = `Invariant && s = 3 then fail "inv")
      ~release:(fun s -> note (strf "release %d" s))
  in
  let reference () =
    incr references;
    if third = `Broken && !references = 3 then fail "broken"
  in
  let system () =
    incr n;
    if !n = 3 then begin
      if third = `Mismatch then raise Not_found;
      if third = `Skip then skip ~reason:"why" ();
      if third = `Fatal then raise Sys.Break
    end;
    !n
  in
  let c = command "open" (Gen.unit @-> makes r) reference system in
  let p = value (drawn (Stateful.program ~steps:3 [ c ]) 0) in
  String.concat "; " (notes (fun () -> note (ended (execute p))))

let release_paths =
  [
    ("a passing run", `Passing, "release 3; release 2; release 1; returned");
    ( "a mismatch",
      `Mismatch,
      "release 2; release 1; failure [call 3 of 3: open ()] raise actual \
       Not_found" );
    ( "a broken reference",
      `Broken,
      {|release 3; release 2; release 1; oracle [reference of call 3 of 3: open ()] message "broken"|}
    );
    ( "a failing invariant",
      `Invariant,
      {|release 3; release 2; release 1; failure [after call 3 of 3, on r3] message "inv"|}
    );
    ( "a skip",
      `Skip,
      "release 2; release 1; " ^ raised (Failure.Control (`Skip (Some "why")))
    );
    ("a fatal exception, which releases nothing", `Fatal, raised Sys.Break);
  ]

(* The system sides of three values: [shared] makes them one. *)
let deduplicated ~shared =
  let one = ref () in
  let r = abstract "r" ~release:(fun _ -> note "release") in
  let system () = if shared then one else ref () in
  let c = command "open" (Gen.unit @-> makes r) ignore system in
  let p = value (drawn (Stateful.program ~steps:3 [ c ]) 0) in
  List.length (notes (execute p))

(* [h] and [g] name the system side they release. *)
let across record =
  let h = abstract "h" ~release:(fun n -> note (strf "h %d" !n)) in
  let g = abstract "g" ~release:(fun n -> note (strf "g %d" !n)) in
  let count = ref 0 in
  let gen =
    Stateful.program ~steps:2
      [
        command "open"
          (Gen.unit @-> makes h)
          ignore
          (fun () ->
            incr count;
            ref !count);
        command "same" (h ^-> makes h) Fun.id Fun.id;
        command "cast" (h ^-> makes g) Fun.id Fun.id;
      ]
  in
  let reset () = count := 0 in
  let tree = find ~reset gen (String.equal record) in
  reset ();
  notes (execute (value tree))

(* Three calls of [open] whose values have their number as system side;
   [fails s] is what the release of [s] does, and under [mismatch] the third
   call makes no value and fails. *)
let releasing ~mismatch fails =
  let n = ref 0 in
  let r =
    abstract "r" ~release:(fun s ->
        note (strf "release %d" s);
        fails s)
  in
  let system () =
    incr n;
    if mismatch && !n = 3 then raise Not_found;
    !n
  in
  let c = command "open" (Gen.unit @-> makes r) ignore system in
  let p = value (drawn (Stateful.program ~steps:3 [ c ]) 0) in
  String.concat "; " (notes (fun () -> note (ended (execute p))))

let skip_why = Failure.Control (`Skip (Some "why"))
let timeout = Failure.Control (`Timeout 0.5)

let release_failures =
  let all = "release 3; release 2; release 1; " in
  [
    ( "a failure over a passing run fails it",
      false,
      (fun s -> if s = 2 then raise Not_found),
      all ^ "failure [release of r2] raise actual Not_found" );
    ( "a verb's failure",
      false,
      (fun s -> if s = 2 then fail "nope"),
      all ^ {|failure [release of r2] message "nope"|} );
    ( "a discard",
      false,
      (fun s -> if s = 2 then assume false),
      all ^ strf "failure [release of r2] message %S" discarded );
    ( "the first of two failures, newest first",
      false,
      (fun s -> if s <> 2 then fail (strf "r%d" s)),
      all ^ {|failure [release of r3] message "r3"|} );
    ( "a control over a passing run",
      false,
      (fun s -> if s = 2 then raise skip_why),
      all ^ raised skip_why );
    ( "a failure over a failing run is dropped",
      true,
      (fun s -> if s = 1 then raise Not_found),
      "release 2; release 1; failure [call 3 of 3: open ()] raise actual \
       Not_found" );
    ( "a control over a failing run replaces its failure",
      true,
      (fun s -> if s = 1 then raise skip_why),
      "release 2; release 1; " ^ raised skip_why );
    ( "the first of two controls",
      false,
      (fun s -> if s = 3 then raise timeout else if s = 1 then raise skip_why),
      all ^ raised timeout );
    ( "a control after a failure",
      false,
      (fun s -> if s = 3 then raise Not_found else if s = 1 then raise skip_why),
      all ^ raised skip_why );
  ]

let releases =
  group "Releases"
    [
      cases
        "a run releases the system side of each value, newest first, on every \
         path but a fatal exception"
        ~name:(fun (n, _, _) -> n)
        release_paths
        (fun (_, third, expected) -> equal string expected (released_on third));
      test "a system side that several values of one type hold is released once"
        (fun () -> equal int 1 (deduplicated ~shared:true));
      test "physically distinct system sides are each released" (fun () ->
          equal int 3 (deduplicated ~shared:false));
      test "a value that another of its type holds is not released again"
        (fun () ->
          equal (list string) [ "h 1" ]
            (across " #  call\n 1  let h1 = open ()\n 2  let h2 = same h1"));
      test "a system side that two types hold is released by each, newest first"
        (fun () ->
          equal (list string) [ "g 1"; "h 1" ]
            (across " #  call\n 1  let h1 = open ()\n 2  let g1 = cast h1"));
      cases
        "every release runs; the first control wins, then the run's failure, \
         then the first release's"
        ~name:(fun (n, _, _, _) -> n)
        release_failures
        (fun (_, mismatch, fails, expected) ->
          equal string expected (releasing ~mismatch fails));
    ]

(* The record *)

let table gen = recorded gen (value (drawn gen 0))

(* The call of one [f] over an argument drawn from [gen]. *)
let argument gen =
  let gen =
    Stateful.program ~steps:1
      [ command "f" (gen @-> returns unit) ignore ignore ]
  in
  String.concat "\n" (calls (table gen))

let argument_rows =
  let printed_as pp v = argument (Gen.with_pp pp (Gen.constant v)) in
  [
    ("a positive int", (fun () -> argument (Gen.int_range 3 3)), "f 3");
    ("a negative int", (fun () -> argument (Gen.int_range (-3) (-3))), "f (-3)");
    ( "a string with a space",
      (fun () -> printed_as (fun ppf s -> Format.fprintf ppf "%S" s) "a b"),
      {|f ("a b")|} );
    ( "a list",
      (fun () -> printed_as (Testable.pp (list int)) [ 1; 2 ]),
      "f ([1; 2])" );
    ( "a value of two lines",
      (fun () -> printed_as Format.pp_print_string "a\nb"),
      "f (a b)" );
    ( "a printerless map, as its pre-image",
      (fun () -> argument (Gen.map succ (Gen.int_range 4 4))),
      "f 4" );
  ]

let long_argument () =
  let big =
    Gen.with_pp Format.pp_print_string (Gen.constant (String.make 300 'x'))
  in
  expect_exact (argument big)
  @@ __POS_OF__
       {|f (xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx... (truncated; 300 bytes total))|}

(* [counter ?pp ?swap ()] has a type [c] whose sides count the calls of
   [bump] that took them; [swap] exchanges two counts. *)
let counter ?pp ?(swap = false) () =
  let c = abstract "c" ?pp in
  let bump r =
    incr r;
    !r
  in
  let swap_ a b =
    let x = !a in
    a := !b;
    b := x
  in
  command "new" (Gen.unit @-> makes c) (fun () -> ref 0) (fun () -> ref 0)
  :: command "bump" (c ^-> returns int) bump bump
  ::
  (if swap then [ command "swap" (c ^-> c ^-> returns unit) swap_ swap_ ]
   else [])

let pp_count ppf r = Format.pp_print_int ppf !r

(* Whether [record] bumps some counter twice. *)
let bumps_twice record =
  let bumped row =
    match List.rev (String.split_on_char ' ' row) with
    | c :: "bump" :: _ -> Some c
    | _ -> None
  in
  let bumps = List.filter_map bumped (String.split_on_char '\n' record) in
  List.length (List.sort_uniq String.compare bumps) < List.length bumps

let reference_before () =
  let gen = Stateful.program ~steps:8 (counter ~pp:pp_count ()) in
  expect_exact (printed gen (value (find gen bumps_twice)))
  @@ __POS_OF__
       {| #  reference before  call
 1                    let c1 = new ()
 2  0                 bump c1
 3  1                 bump c1
 4                    let c2 = new ()
 5                    let c3 = new ()
 6                    let c4 = new ()
 7  0                 bump c4
 8                    let c5 = new ()|}

let occurs sub s =
  let n = String.length sub in
  let rec at i =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || at (i + 1))
  in
  at 0

(* A program whose swap took two values with different reference sides. *)
let joined_cell () =
  let gen = Stateful.program ~steps:8 (counter ~pp:pp_count ~swap:true ()) in
  let tree = find gen (fun r -> occurs "0, 1" r || occurs "1, 0" r) in
  expect_exact (printed gen (value tree))
  @@ __POS_OF__
       {| #  reference before  call
 1                    let c1 = new ()
 2                    let c2 = new ()
 3                    let c3 = new ()
 4  0, 0              swap c3 c3
 5                    let c4 = new ()
 6  0                 bump c1
 7  0                 bump c4
 8  0, 1              swap c2 c4|}

let long_cell () =
  let e_acute = String.concat "" (List.init 100 (fun _ -> "\u{00e9}")) in
  let pp ppf _ = Format.pp_print_string ppf e_acute in
  let gen = Stateful.program ~steps:3 (counter ~pp ()) in
  expect_exact (printed gen (value (find gen (occurs "bump"))))
  @@ __POS_OF__
       {| #  reference before                                              call
 1                                                                let c1 = new ()
 2  ééééééééééééééééééééééééééééééééééééééééééééééééééééééééé...  bump c1
 3  ééééééééééééééééééééééééééééééééééééééééééééééééééééééééé...  bump c1|}

let raising_pp () =
  let pp ppf r = if !r = 1 then raise Not_found else pp_count ppf r in
  let gen = Stateful.program ~steps:8 (counter ~pp ()) in
  expect_exact (printed gen (value (find gen bumps_twice)))
  @@ __POS_OF__
       {| #  reference before       call
 1                         let c1 = new ()
 2  0                      bump c1
 3  <pp raised Not_found>  bump c1
 4                         let c2 = new ()
 5                         let c3 = new ()
 6                         let c4 = new ()
 7  0                      bump c4
 8                         let c5 = new ()|}

(* Stdlib's queue, whose [pop] ends in [result], under a reference that
   accepts or gives every outcome. *)
let popped result =
  let q = abstract "q" in
  [
    command "create" (Gen.unit @-> makes q) ignore Queue.create;
    command "push"
      (q ^-> Gen.int_range 0 9 @-> returns unit)
      (fun () _ -> ())
      (fun q x -> Queue.push x q);
    (match result with
    | `Returns -> command "pop" (q ^-> returns int) (fun () -> 0) Queue.pop
    | `Judges -> command "pop" (q ^-> judges int) (fun () _ -> ()) Queue.pop);
  ]

let result_column () =
  let gen = Stateful.program ~steps:5 (popped `Judges) in
  let lines record = String.split_on_char '\n' record in
  let popped_value line =
    occurs "pop q1" line && not (occurs "exception" line)
  in
  let tree =
    find gen (fun record ->
        occurs "exception Stdlib.Queue.Empty" record
        && List.exists popped_value (lines record))
  in
  expect_exact (printed gen (value tree))
  @@ __POS_OF__
       {| #  call                result
 1  let q1 = create ()
 2  push q1 7           ()
 3  pop q1              7
 4  pop q1              exception Stdlib.Queue.Empty
 5  pop q1              exception Stdlib.Queue.Empty|}

(* The words of a record's header row. *)
let header record =
  let line = List.hd (String.split_on_char '\n' record) in
  List.filter (fun w -> w <> "") (String.split_on_char ' ' line)

let result_headers =
  [
    ("returns", `Returns, [ "#"; "call" ]);
    ("judges", `Judges, [ "#"; "call"; "result" ]);
  ]

let cut steps =
  let rows = String.split_on_char '\n' (table (ticks steps)) in
  let omission = List.filter (String.starts_with ~prefix:"\u{2026}") rows in
  strf "%d lines, %s" (List.length rows) (String.concat "" omission)

(* A sample of a printerless [Gen.map] renders as a pre-image. *)
let rendering () =
  let gen =
    Stateful.program ~steps:1
      [ command "set" (Gen.map succ Gen.nat @-> returns unit) ignore ignore ]
  in
  let rendering =
    match Gen_engine.render (Shrink_tree.root (drawn gen 0)) with
    | Value s -> "value " ^ s
    | Pre_image s -> "pre-image " ^ s
  in
  equal string "value (not run)" rendering

let the_record =
  group "The record"
    [
      test "a program that has not run prints (not run)" (fun () ->
          let gen = ticks 3 in
          equal string "(not run)" (printed gen (value (drawn gen 0))));
      test "a run without calls prints (no calls)" (fun () ->
          equal (pair string string)
            ("(no calls)", "(no calls)")
            (table (ticks 0), table (Stateful.program ~steps:4 [ dead "never" ])));
      test "a record is a table of the calls, numbered from one" (fun () ->
          expect_exact (table (ticks 3))
          @@ __POS_OF__ {| #  call
 1  tick ()
 2  tick ()
 3  tick ()|});
      test "a call that made a value reads let" (fun () ->
          expect_exact
            (table (Stateful.program ~steps:2 [ made "open" (abstract "r") ]))
          @@ __POS_OF__ {| #  call
 1  let r1 = open ()
 2  let r2 = open ()|});
      cases
        "an argument prints as its sample renders, in parentheses on a space \
         or a leading -"
        ~name:(fun (n, _, _) -> n)
        argument_rows
        (fun (_, call, expected) -> equal string expected (call ()));
      test "an argument is cut at 200 bytes, with its size" long_argument;
      test "a reference before column holds the printed reference sides"
        reference_before;
      test "a cell joins the reference sides of two arguments" joined_cell;
      test "a reference before cell is cut at 60 code points" long_cell;
      test "a printer that raises costs its own cell" raising_pp;
      test
        "a judging call adds a result column, each call's outcome as its \
         witness prints it"
        result_column;
      cases "only a judging call adds a result column"
        ~name:(fun (n, _, _) -> n)
        result_headers
        (fun (_, result, expected) ->
          let gen = Stateful.program ~steps:5 (popped result) in
          let tree = find gen (occurs "pop q1") in
          equal (list string) expected (header (printed gen (value tree))));
      test "a record of more than 40 calls prints its first and last 20"
        (fun () ->
          expect_exact (table (ticks 50))
          @@ __POS_OF__
               {| #  call
 1  tick ()
 2  tick ()
 3  tick ()
 4  tick ()
 5  tick ()
 6  tick ()
 7  tick ()
 8  tick ()
 9  tick ()
10  tick ()
11  tick ()
12  tick ()
13  tick ()
14  tick ()
15  tick ()
16  tick ()
17  tick ()
18  tick ()
19  tick ()
20  tick ()
… (10 calls omitted)
31  tick ()
32  tick ()
33  tick ()
34  tick ()
35  tick ()
36  tick ()
37  tick ()
38  tick ()
39  tick ()
40  tick ()
41  tick ()
42  tick ()
43  tick ()
44  tick ()
45  tick ()
46  tick ()
47  tick ()
48  tick ()
49  tick ()
50  tick ()|});
      cases "the calls omitted start past 40"
        ~name:(fun (n, _) -> strf "%d calls" n)
        [ (40, "41 lines, "); (41, "42 lines, \u{2026} (1 call omitted)") ]
        (fun (n, row) -> equal string row (cut n));
      test "a program renders as a value, never a pre-image" rendering;
    ]

(* Summaries *)

let summary_rows =
  [
    (0, None);
    (1, Some "1 call, last: tick");
    (3, Some "3 calls, last: tick");
    (50, Some "50 calls, last: tick");
  ]

(* The last call of a failing run is the one that failed. *)
let last_failing () =
  let n = ref 0 in
  let reset () = n := 0 in
  let gen =
    Stateful.program ~steps:6
      [
        unit_call "ok" ignore ignore;
        unit_call "fails" ignore (fun () ->
            incr n;
            if !n = 2 then fail "second");
      ]
  in
  let tree =
    find ~reset gen (fun record ->
        rows record < 6 && List.mem "fails ()" (calls record))
  in
  let p = value tree in
  ignore (recorded ~reset gen p : string);
  equal (option string)
    (Some (strf "%d calls, last: fails" (rows (printed gen p))))
    (Stateful.summary p)

let summaries =
  group "Summaries"
    [
      test "a program that has not run has no summary" (fun () ->
          is_none (Stateful.summary (value (drawn (ticks 3) 0))));
      cases "the summary is the record in one line, and none without calls"
        ~name:(fun (n, _) -> strf "steps %d" n)
        summary_rows
        (fun (n, s) ->
          let gen = ticks n in
          let p = value (drawn gen 0) in
          ignore (recorded gen p : string);
          equal (option string) s (Stateful.summary p));
      test "the summary of a failing run ends on the call that failed"
        last_failing;
    ]

(* Screens: the guide's two failures *)

module Int_set = Set.Make (Int)

(* The guide's set, whose [union] drops its first argument's elements when
   the second is not empty. *)
module Fast_set = struct
  type t = int list

  let empty = []

  let rec add x = function
    | [] -> [ x ]
    | y :: r as l -> if x = y then l else if x < y then x :: l else y :: add x r

  let mem = List.mem
  let union a b = if b = [] then a else b
  let elements s = s
  let balanced _ = true
end

(* The manual's bounded queue, which takes one element too many. *)
module Bounded_queue = struct
  exception Full
  exception Empty

  type t = {
    data : int array;
    capacity : int;
    mutable head : int;
    mutable size : int;
  }

  let create capacity =
    { data = Array.make (capacity + 1) 0; capacity; head = 0; size = 0 }

  let size q = q.size

  let push q x =
    if q.size > q.capacity then raise Full;
    q.data.((q.head + q.size) mod (q.capacity + 1)) <- x;
    q.size <- q.size + 1

  let peek q = if q.size = 0 then raise Empty else q.data.(q.head)

  let pop q =
    let x = peek q in
    q.head <- (q.head + 1) mod (q.capacity + 1);
    q.size <- q.size - 1;
    x
end

module Model = struct
  type t = { capacity : int; mutable items : int list }

  let create capacity = { capacity; items = [] }
  let size m = List.length m.items

  let peek m =
    match m.items with [] -> raise Bounded_queue.Empty | x :: _ -> x

  let pop m =
    let x = peek m in
    m.items <- List.tl m.items;
    x

  let push m x =
    if size m = m.capacity then raise Bounded_queue.Full;
    m.items <- m.items @ [ x ]
end

(* The declarations carry the guide's locations, so that the screens do not
   move with this file. *)
let fast_set_commands () =
  let at line = ("test/test_fast_set.ml", line, 4, 80) in
  let set =
    abstract "s" ~invariant:(fun _ s -> is_true (Fast_set.balanced s))
  in
  let elt = Gen.int_range 0 15 in
  [
    command ~__POS__:(at 11) "empty"
      (Gen.unit @-> makes set)
      (fun () -> Int_set.empty)
      (fun () -> Fast_set.empty);
    command ~__POS__:(at 12) "add"
      (elt @-> set ^-> makes set)
      Int_set.add Fast_set.add;
    command ~__POS__:(at 13) "union"
      (set ^-> set ^-> makes set)
      Int_set.union Fast_set.union;
    command ~__POS__:(at 14) "mem"
      (elt @-> set ^-> returns bool)
      Int_set.mem Fast_set.mem;
    command ~__POS__:(at 15) "elements"
      (set ^-> returns (list int))
      Int_set.elements Fast_set.elements;
  ]

let queue_commands () =
  let at line = ("test/test_bounded_queue.ml", line, 4, 80) in
  let queue =
    abstract "q" ~pp:(fun ppf m -> Testable.pp (list int) ppf m.Model.items)
  in
  [
    command ~__POS__:(at 22) "create"
      (Gen.int_range 1 4 @-> makes queue)
      Model.create Bounded_queue.create;
    command ~__POS__:(at 24) "push"
      (queue ^-> Gen.int_range 0 9 @-> returns unit)
      Model.push Bounded_queue.push;
    command ~__POS__:(at 26) "pop"
      (queue ^-> returns int)
      Model.pop Bounded_queue.pop;
    command ~__POS__:(at 27) "peek"
      (queue ^-> returns int)
      Model.peek Bounded_queue.peek;
    command ~__POS__:(at 28) "size"
      (queue ^-> returns int)
      Model.size Bounded_queue.size;
  ]

(* A queue whose [pop] invents a value when it holds three. *)
module Inventing_queue = struct
  let pop q =
    let x = Queue.pop q in
    if Queue.length q >= 2 then 42 else x
end

(* The monitor of a queue: a pop returns a value pushed and not yet taken, in
   any order. The reference is the values pushed and not yet taken. *)
module Pushed = struct
  let at line = ("test/test_queue_monitor.ml", line, 4, 80)
  let create () = ref []
  let push m x = m := x :: !m

  let rec remove x = function
    | [] -> []
    | y :: l -> if y = x then l else y :: remove x l

  let pop m = function
    | Ok v ->
        mem ~__POS__:(at 16) ~msg:"pop returns a value pushed and not yet taken"
          int v !m;
        m := remove v !m
    | Error Queue.Empty ->
        equal ~__POS__:(at 19) ~msg:"pop raises Empty only when empty"
          (list int) [] !m
    | Error e -> raise e
end

let monitor_commands () =
  let queue = abstract "q" ~pp:(fun ppf m -> Testable.pp (list int) ppf !m) in
  [
    command ~__POS__:(Pushed.at 25) "create"
      (Gen.unit @-> makes queue)
      Pushed.create Queue.create;
    command ~__POS__:(Pushed.at 26) "push"
      (queue ^-> Gen.int_range 0 9 @-> returns unit)
      Pushed.push
      (fun q x -> Queue.push x q);
    command ~__POS__:(Pushed.at 28) "pop"
      (queue ^-> judges int)
      Pushed.pop Inventing_queue.pop;
  ]

(* An array whose [get] of the last element, from the second, returns the
   one before it. *)
module Off_by_one = struct
  type t = { mutable items : int list }

  let create () = { items = [] }
  let add_last d x = d.items <- d.items @ [ x ]

  let get d i =
    let last = List.length d.items - 1 in
    List.nth d.items (if i = last && i > 0 then i - 1 else i)
end

let darr_commands () =
  let at line = ("test/test_darr.ml", line, 4, 80) in
  let darr = abstract "d" ~pp:(fun ppf m -> Testable.pp (list int) ppf !m) in
  let index = among int darr (fun m -> List.init (List.length !m) Fun.id) in
  [
    command ~__POS__:(at 8) "create"
      (Gen.unit @-> makes darr)
      (fun () -> ref [])
      Off_by_one.create;
    command ~__POS__:(at 9) "add_last"
      (darr ^-> Gen.int_range 0 9 @-> returns unit)
      (fun m x -> m := !m @ [ x ])
      Off_by_one.add_last;
    command ~__POS__:(at 11) "get"
      (darr ^-> index ^-> returns int)
      (fun m i -> List.nth !m i)
      Off_by_one.get;
  ]

let property_failure = function
  | Property.Fail { failure; _ } -> Some failure
  | Pass _ | Coverage_failed _ | Gave_up _ -> None

(* The entry of the failure that [commands] give, as the report prints it,
   under the test's declaration at [loc]. *)
let screen ~loc commands =
  let loc = Loc.of_pos loc in
  let law _ p = Stateful.execute p in
  let outcome =
    Property.run ~loc ~summary:Stateful.summary ~root ~path:"screen"
      (Stateful.program commands)
      law
  in
  let f = require_match property_failure outcome in
  Format.asprintf "%a"
    (fun ppf f -> Sections.pp_failure ~ansi:false ~hints:false ppf f)
    f

let screens =
  group "Screens"
    [
      test "a mismatch of two results under the guide's set" (fun () ->
          expect_exact
            (screen
               ~loc:("test/test_fast_set.ml", 17, 0, 10)
               (fast_set_commands ()))
          @@ __POS_OF__
               {|    test/test_fast_set.ml:17
    counterexample (case 27, shrunk 12 steps): 5 calls, last: elements
       #  call
       1  let s1 = empty ()
       2  let s2 = add 0 s1
       3  let s3 = add 1 s2
       4  let s4 = union s3 s2
       5  elements s4
    which failed at:
      test/test_fast_set.ml:15
      call 5 of 5: elements s4
      expected  [0; 1]
      actual    [0]
|});
      test "a mismatch of an exception under the manual's bounded queue"
        (fun () ->
          expect_exact
            (screen
               ~loc:("test/test_bounded_queue.ml", 32, 0, 10)
               (queue_commands ()))
          @@ __POS_OF__
               {|    test/test_bounded_queue.ml:32
    counterexample (case 1, shrunk 7 steps): 3 calls, last: push
       #  reference before  call
       1                    let q1 = create 1
       2  []                push q1 0
       3  [0]               push q1 0
    which failed at:
      test/test_bounded_queue.ml:24
      call 3 of 3: push q1 0
      expected exception  Test_stateful.Bounded_queue.Full
      but no exception was raised
|});
      test "a judge's rejection under a queue's monitor" (fun () ->
          expect_exact
            (screen
               ~loc:("test/test_queue_monitor.ml", 31, 0, 10)
               (monitor_commands ()))
          @@ __POS_OF__
               {|    test/test_queue_monitor.ml:31
    counterexample (case 10, shrunk 7 steps): 5 calls, last: pop
       #  reference before  call                result
       1                    let q1 = create ()
       2  []                push q1 0           ()
       3  [0]               push q1 0           ()
       4  [0; 0]            push q1 0           ()
       5  [0; 0; 0]         pop q1              42
    which failed at:
      test/test_queue_monitor.ml:16
      call 5 of 5: pop q1; pop returns a value pushed and not yet taken
      expected  a list containing 42
      actual    [0; 0; 0]
|});
      test "an off-by-one at an index an array has" (fun () ->
          expect_exact
            (screen ~loc:("test/test_darr.ml", 14, 0, 10) (darr_commands ()))
          @@ __POS_OF__
               {|    test/test_darr.ml:14
    counterexample (case 18, shrunk 11 steps): 4 calls, last: get
       #  reference before  call
       1                    let d1 = create ()
       2  []                add_last d1 0
       3  [0]               add_last d1 1
       4  [0; 1]            get d1 1
    which failed at:
      test/test_darr.ml:11
      call 4 of 4: get d1 1
      expected  1
      actual    0
|});
    ]

(* Shrinking *)

(* A queue whose [pop] returns the newest element: right for one element,
   wrong from two on. *)
module Bad_queue = struct
  type t = { mutable items : int list }

  let create () = { items = [] }
  let push q x = q.items <- q.items @ [ x ]

  let pop q =
    match List.rev q.items with
    | [] -> None
    | newest :: rest ->
        q.items <- List.rev rest;
        Some newest
end

let bad_queue_commands () =
  let queue = abstract "q" in
  [
    command "create" (Gen.unit @-> makes queue) Queue.create Bad_queue.create;
    command "push"
      (queue ^-> Gen.int_range 0 9 @-> returns unit)
      (fun q x -> Queue.push x q)
      Bad_queue.push;
    command "pop" (queue ^-> returns (option int)) Queue.take_opt Bad_queue.pop;
  ]

let bad_queue () = Stateful.program ~steps:8 (bad_queue_commands ())

let rendered (f : Failure.t) =
  match f.kind with Property { rendered; _ } -> Some rendered.kept | _ -> None

let inner (f : Failure.t) =
  match f.kind with Property { inner; _ } -> inner | _ -> None

let names = [ "alpha"; "bravo"; "charlie"; "delta"; "echo" ]

let rec is_subsequence sub whole =
  match (sub, whole) with
  | [], _ -> true
  | _, [] -> false
  | x :: sub', y :: whole' ->
      if String.equal x y then is_subsequence sub' whole'
      else is_subsequence sub whole'

(* Every argument is [()], so a candidate that put one command in place of
   another would show as calls that are not a subsequence of its root's. *)
let subsequences () =
  let gen =
    Stateful.program ~steps:14
      (List.map (fun name -> unit_call name ignore ignore) names)
  in
  let nodes = ref 0 and strays = ref [] in
  for index = 0 to 4 do
    let tree = drawn gen index in
    let whole = calls (recorded gen (value tree)) in
    let budget = !nodes + 400 in
    let rec visit node =
      if !nodes < budget then begin
        incr nodes;
        let calls = calls (recorded gen (value node)) in
        if not (is_subsequence calls whole) then strays := calls :: !strays;
        Seq.iter visit (Shrink_tree.children node)
      end
    in
    visit tree
  done;
  equal int 2_000 !nodes;
  equal (list (list string)) [] !strays

let search (f : Failure.t) =
  match f.kind with
  | Property { case_index; shrink_steps; _ } -> Some (case_index, shrink_steps)
  | _ -> None

let run_property ?(count = 100) ~path commands =
  let gen = Stateful.program commands in
  let law _ p = Stateful.execute p in
  let outcome =
    Property.run ~count:(`Declared count) ~summary:Stateful.summary ~root ~path
      gen law
  in
  (gen, require_match property_failure outcome)

let shrunk_queue () =
  let _, f = run_property ~count:40 ~path:"bad_queue" (bad_queue_commands ()) in
  expect_exact
    (strf "%s\n%s\n%s" (require_match rendered f)
       (Option.value ~default:"no summary"
          (Option.map
             (fun (s : Failure.text) -> s.kept)
             (match f.kind with
             | Property { summary; _ } -> summary
             | _ -> None)))
       (row (require_some (inner f))))
  @@ __POS_OF__
       {| #  call
 1  let q1 = create ()
 2  push q1 0
 3  push q1 1
 4  pop q1
4 calls, last: pop
[call 4 of 4: pop q1] equality Some 0, Some 1|}

(* [f]'s reference, or its pre, breaks from [15] on: the search keeps to the
   candidates that break it. *)
let broken_reference broken =
  let reference, pre =
    match broken with
    | `Reference ->
        ( (fun x ->
            if x >= 15 then fail "broken";
            x),
          None )
    | `Pre ->
        (Fun.id, Some (fun x -> if x >= 15 then failwith "broken" else true))
  in
  let commands () =
    [ command "f" ?pre (Gen.int_range 0 20 @-> returns int) reference Fun.id ]
  in
  let _, f = run_property ~path:"broken" (commands ()) in
  strf "%s\n%s" (require_match rendered f) (row (require_some (inner f)))

(* Draws from 3 to 20 and offers 0 first, which it never draws. *)
let beyond_its_draws =
  let rec tree x =
    let smaller = List.filter (fun c -> c < x) [ 0; x / 2; x - 1 ] in
    Shrink_tree.make ~root:x ~children:(Seq.map tree (List.to_seq smaller))
  in
  Gen_engine.make ~pp:Format.pp_print_int (fun state ->
      let x, state = Seed.below ~bound:18L state in
      (tree (3 + Int64.to_int x), state))

let rejected () =
  let commands =
    [
      command "f"
        (beyond_its_draws @-> returns int)
        (fun x ->
          if x = 0 then fail "no zero";
          x)
        (fun x -> if x >= 3 then x + 1 else x);
    ]
  in
  let _, f = run_property ~path:"rejected" commands in
  equal (pair string string)
    (" #  call\n 1  f 3", "[call 1 of 1: f 3] equality 3, 4")
    (require_match rendered f, row (require_some (inner f)))

let replayed () =
  let trace = ref [] in
  let gen = bad_queue () in
  let law _ p =
    Fun.protect
      ~finally:(fun () -> trace := printed gen p :: !trace)
      (fun () -> Stateful.execute p)
  in
  let outcome =
    Property.run ~count:(`Declared 40) ~root ~path:"replay" gen law
  in
  let f = require_match property_failure outcome in
  let case, steps = require_match search f in
  ( List.rev !trace,
    strf "%s\ncase %d, shrunk %d steps" (require_match rendered f) case steps )

let replay () =
  let first = replayed () in
  let second = replayed () in
  greater int ~than:20 (List.length (fst first));
  equal (pair (list string) string) first second

let shrinking =
  group "Shrinking"
    [
      test
        "a candidate deletes calls or moves an argument, and never changes a \
         call's command"
        subsequences;
      test "a buggy queue shrinks to the shortest program that shows its bug"
        shrunk_queue;
      test "a broken reference shrinks to the smallest program that breaks it"
        (fun () ->
          expect_exact (broken_reference `Reference)
          @@ __POS_OF__
               {| #  call
 1  f 15
[reference of call 1 of 1: f 15] message "broken"|});
      test "a pre that raises shrinks to the smallest program that raises it"
        (fun () ->
          expect_exact (broken_reference `Pre)
          @@ __POS_OF__
               {| #  call
 1  f 15
[~pre of call 1 of 1: f 15] raise actual Failure("broken")|});
      test
        "a candidate that breaks the reference is rejected in the search of \
         the system's failure"
        rejected;
      test "a root seed replays the same programs and counterexample" replay;
    ]

(* Elements *)

(* Two calls over a value whose reference side is [listed], and [use],
   which takes one of its elements and notes it everywhere. *)
let using listed =
  let d = abstract "d" in
  let element =
    among int d (fun listed ->
        note "candidates";
        listed)
  in
  let noting what _ x = note (strf "%s %d" what x) in
  Stateful.program ~steps:2
    [
      command "create" (Gen.unit @-> makes d) (fun () -> listed) ignore;
      command "use"
        ~pre:(fun d x ->
          noting "pre" d x;
          true)
        (d ^-> element ^-> returns unit)
        (noting "reference") (noting "system");
    ]

let element_trace () =
  let gen = using [ 20 ] in
  let tree =
    find gen (String.equal " #  call\n 1  let d1 = create ()\n 2  use d1 20")
  in
  equal (list string)
    [ "candidates"; "pre 20"; "system 20"; "reference 20" ]
    (notes (execute (value tree)))

(* Two calls are drawn, so a record of one skipped a [use]. *)
let unlisted () =
  let gen = using [] in
  let tree = find gen (String.equal " #  call\n 1  let d1 = create ()") in
  equal (list string) [ "candidates" ] (notes (execute (value tree)))

(* [broken] makes no value, both sides raising alike, so a [use] after it
   never resolves its value of [o] and never lists [d]'s elements. *)
let unresolved () =
  let d = abstract "d" and o = abstract "o" in
  let element =
    among int d (fun listed ->
        note "candidates";
        listed)
  in
  let raising () = raise Not_found in
  let gen =
    Stateful.program ~steps:3
      [
        command "create" (Gen.unit @-> makes d) (fun () -> [ 1 ]) ignore;
        command "broken" (Gen.unit @-> makes o) raising raising;
        command "use"
          (d ^-> element ^-> o ^-> returns unit)
          (fun _ _ () -> ())
          (fun () _ () -> ());
      ]
  in
  let tree =
    find gen (String.equal " #  call\n 1  let d1 = create ()\n 2  broken ()")
  in
  equal (list string) [] (notes (execute (value tree)))

let reachable () =
  let gen = using [ 0; 1; 2 ] in
  let use i =
    match calls (recorded gen (value (drawn gen i))) with
    | [ _; use ] when String.starts_with ~prefix:"use " use -> Some use
    | _ -> None
  in
  equal (list string)
    [ "use d1 0"; "use d1 1"; "use d1 2" ]
    (List.sort_uniq String.compare (List.filter_map use (List.init 200 Fun.id)))

(* An array as a list, which [create] makes of three elements and [push]
   grows by one; [get] takes an index. *)
let indexed_commands () =
  let d = abstract "d" in
  let index = among int d (fun m -> List.init (List.length !m) Fun.id) in
  let push m = m := !m @ [ List.length !m ] in
  let create () = ref [ 0; 1; 2 ] in
  [
    command "create" (Gen.unit @-> makes d) create create;
    command "push" (d ^-> returns unit) push push;
    command "get"
      (d ^-> index ^-> returns int)
      (fun m i -> List.nth !m i)
      (fun m i -> List.nth !m i);
  ]

(* [get d1 3] takes the last of four; once the [push] is deleted, the last
   of three. *)
let kept_place () =
  let gen = Stateful.program ~steps:3 (indexed_commands ()) in
  let tree =
    find gen
      (String.equal
         " #  call\n 1  let d1 = create ()\n 2  push d1\n 3  get d1 3")
  in
  let without_push record =
    match calls record with
    | [ "let d1 = create ()"; get ] when String.starts_with ~prefix:"get" get ->
        Some get
    | _ -> None
  in
  let gets =
    List.filter_map
      (fun child -> without_push (recorded gen (value child)))
      (List.of_seq (Shrink_tree.children tree))
  in
  equal (list string) [ "get d1 2" ] gets

(* A value lists [0 … 999], and [get] is wrong from [least] on. Programs
   are two calls long, so that every step shrinks the element. *)
let shrunk_element least =
  let d = abstract "d" in
  let index = among int d Fun.id in
  let commands =
    [
      command "create"
        (Gen.unit @-> makes d)
        (fun () -> List.init 1000 Fun.id)
        ignore;
      command "get"
        (d ^-> index ^-> returns int)
        (fun _ i -> i)
        (fun () i -> if i >= least then i + 1 else i);
    ]
  in
  let outcome =
    Property.run ~summary:Stateful.summary ~root ~path:"element"
      (Stateful.program ~steps:2 commands) (fun _ p -> Stateful.execute p)
  in
  let f = require_match property_failure outcome in
  let _, steps = require_match search f in
  strf "%s\n%s\nshrunk %d steps" (require_match rendered f)
    (row (require_some (inner f)))
    steps

(* How each candidate that reduces the element of [get d1 k], over a value
   that lists ten, ends: the element it takes, or [unplaced] when it takes
   none. *)
let unplaced = "raised windtrap discard (assume or reject outside a property)"

let element_candidates k =
  let d = abstract "d" in
  let index = among int d Fun.id in
  let gen =
    Stateful.program ~steps:2
      [
        command "create"
          (Gen.unit @-> makes d)
          (fun () -> List.init 10 Fun.id)
          ignore;
        command "get"
          (d ^-> index ^-> returns unit)
          (fun _ _ -> ())
          (fun () _ -> ());
      ]
  in
  let tree =
    find gen
      (String.equal (strf " #  call\n 1  let d1 = create ()\n 2  get d1 %d" k))
  in
  let ending child =
    let p = value child in
    match ended (execute p) with
    | "returned" -> (
        match calls (printed gen p) with
        | [ _; get ] when String.starts_with ~prefix:"get" get -> Some get
        | _ -> None)
    | ending -> Some ending
  in
  List.filter_map ending (List.of_seq (Shrink_tree.children tree))

(* [create] makes a value that lists [0], whose candidates raise what
   [raising] holds once it is set. *)
let listing_breaks e =
  let raising = ref None in
  let d = abstract "d" in
  let index =
    among int d (fun listed ->
        Option.iter raise !raising;
        listed)
  in
  let gen =
    Stateful.program ~steps:2
      [
        command "create" (Gen.unit @-> makes d) (fun () -> [ 0 ]) ignore;
        command "get"
          (d ^-> index ^-> returns int)
          (fun _ i -> i)
          (fun () i -> i);
      ]
  in
  let tree =
    find gen (String.equal " #  call\n 1  let d1 = create ()\n 2  get d1 0")
  in
  raising := Some e;
  let p = value tree in
  let ending = ended (execute p) in
  strf "%s\n%s" ending (printed gen p)

let listing_failures =
  let broken = " #  call\n 1  let d1 = create ()\n 2  get d1 _" in
  [
    ( "an assertion",
      asserted "nope",
      {|oracle [reference of call 2 of 2: get d1 _] message "nope"|} ^ "\n"
      ^ broken );
    ( "an exception",
      Not_found,
      "oracle [reference of call 2 of 2: get d1 _] raise actual Not_found\n"
      ^ broken );
    ( "a discard",
      Failure.Control `Discard,
      strf "oracle [reference of call 2 of 2: get d1 _] message %S\n%s"
        discarded broken );
    ( "a skip",
      Failure.Control (`Skip (Some "why")),
      "raised windtrap skip: why\n #  call\n 1  let d1 = create ()" );
  ]

(* The call of [f] over the one element [v] of a value, printed by [w]. *)
let element_call w v =
  let d = abstract "d" in
  let element = among w d (fun () -> [ v ]) in
  let gen =
    Stateful.program ~steps:2
      [
        made "create" d;
        command "f"
          (d ^-> element ^-> returns unit)
          (fun () _ -> ())
          (fun () _ -> ());
      ]
  in
  let tree =
    find gen (fun record ->
        match calls record with
        | [ _; call ] -> String.starts_with ~prefix:"f " call
        | _ -> false)
  in
  List.nth (calls (recorded gen (value tree))) 1

let element_rows =
  let quoted =
    Testable.make
      ~pp:(fun ppf s -> Format.fprintf ppf "%S" s)
      ~equal:String.equal
  in
  let raising = Testable.make ~pp:(fun _ _ -> raise Not_found) ~equal:( = ) in
  [
    ("an int", (fun () -> element_call int 3), "f d1 3");
    ("a negative int", (fun () -> element_call int (-3)), "f d1 (-3)");
    ( "a string with a space",
      (fun () -> element_call quoted "a b"),
      {|f d1 ("a b")|} );
    ("a list", (fun () -> element_call (list int) [ 1; 2 ]), "f d1 ([1; 2])");
    ( "a printer that raises",
      (fun () -> element_call raising ()),
      "f d1 (<pp raised Not_found>)" );
  ]

(* [one] and [two] make a value whose one element is [1] and [2], and [pick]
   takes one beside two values, [d1] then [d2]. *)
let two_values pick =
  let d = abstract "d" in
  let tag = among int d (fun r -> [ r ]) in
  let gen =
    Stateful.program ~steps:3
      [
        command "one" (Gen.unit @-> makes d) (fun () -> 1) ignore;
        command "two" (Gen.unit @-> makes d) (fun () -> 2) ignore;
        pick d tag;
      ]
  in
  let is_value word = String.starts_with ~prefix:"d" word in
  let tree =
    find gen (fun record ->
        match calls record with
        | [ "let d1 = one ()"; "let d2 = two ()"; pick ] ->
            List.filter is_value (String.split_on_char ' ' pick)
            = [ "d1"; "d2" ]
        | _ -> false)
  in
  List.nth (calls (recorded gen (value tree))) 2

let ignored _ _ _ = ()

let two_values_rows =
  [
    ( "after both, the nearer",
      (fun d tag ->
        command "pick" (d ^-> d ^-> tag ^-> returns unit) ignored ignored),
      "pick d1 d2 2" );
    ( "between them, the one before",
      (fun d tag ->
        command "pick" (d ^-> tag ^-> d ^-> returns unit) ignored ignored),
      "pick d1 1 d2" );
    ( "before both, the first after",
      (fun d tag ->
        command "pick" (tag ^-> d ^-> d ^-> returns unit) ignored ignored),
      "pick 1 d1 d2" );
  ]

module Int_map = Map.Make (Int)

(* A map as an association list, the key added last first. It notes every
   key it misses. *)
module Assoc = struct
  let empty = []
  let add k v m = (k, v) :: List.remove_assoc k m

  let find k m =
    match List.assoc_opt k m with
    | Some v -> v
    | None ->
        note (strf "missed %d" k);
        raise Not_found
end

(* [Map.find k m] in the API's order: the key reads the map after it. *)
let map_find () =
  let m = abstract "m" in
  let key = among int m (fun r -> List.map fst (Int_map.bindings r)) in
  let commands =
    [
      command "empty"
        (Gen.unit @-> makes m)
        (fun () -> Int_map.empty)
        (fun () -> Assoc.empty);
      command "add"
        (Gen.int_range 0 999 @-> Gen.int_range 0 9 @-> m ^-> makes m)
        Int_map.add Assoc.add;
      command "find" (key ^-> m ^-> returns int) Int_map.find Assoc.find;
    ]
  in
  let t = Stateful.stateful ~count:50 "find" commands in
  equal (list string) [ "returned" ] (notes (fun () -> note (ended (body t))))

(* [Set.mem x s] in the API's order, over a set as a sorted list, which
   notes every element it misses. *)
let set_mem () =
  let s = abstract "s" in
  let member = among int s Int_set.elements in
  let mem x l =
    List.mem x l
    ||
    (note (strf "missed %d" x);
     false)
  in
  let commands =
    [
      command "empty"
        (Gen.unit @-> makes s)
        (fun () -> Int_set.empty)
        (fun () -> []);
      command "add"
        (Gen.int_range 0 999 @-> s ^-> makes s)
        Int_set.add
        (fun x l -> List.sort_uniq Int.compare (x :: l));
      command "mem" (member ^-> s ^-> returns bool) Int_set.mem mem;
    ]
  in
  let t = Stateful.stateful ~count:50 "mem" commands in
  equal (list string) [ "returned" ] (notes (fun () -> note (ended (body t))))

(* [blit src i dst j] copies one element: [i] reads [src] and [j] reads
   [dst]. The system notes an index beyond its array. *)
let blit () =
  let d = abstract "d" in
  let index = among int d (fun m -> List.init (List.length !m) Fun.id) in
  let push m = m := !m @ [ List.length !m ] in
  let create () = ref [ 0 ] in
  let copy src i dst j =
    dst := List.mapi (fun k x -> if k = j then List.nth !src i else x) !dst
  in
  let checked src i dst j =
    if i >= List.length !src || j >= List.length !dst then
      note
        (strf "beyond: %d of %d, %d of %d" i (List.length !src) j
           (List.length !dst));
    copy src i dst j
  in
  let commands =
    [
      command "create" (Gen.unit @-> makes d) create create;
      command "push" (d ^-> returns unit) push push;
      command "blit" (d ^-> index ^-> d ^-> index ^-> returns unit) copy checked;
    ]
  in
  let t = Stateful.stateful ~count:100 "blit" commands in
  equal (list string) [ "returned" ] (notes (fun () -> note (ended (body t))))

(* A value lists rows, and a row lists its cells. The system notes a cell
   that is not in its row. [cell] takes the three in the order [signature]
   gives. *)
let nested signature =
  let t = abstract "t" in
  let row = among (list int) t Fun.id in
  let cell = among int row Fun.id in
  let commands =
    [
      command "create"
        (Gen.unit @-> makes t)
        (fun () -> [ [ 1; 2 ]; [ 3 ] ])
        ignore;
      signature t row cell (fun row x ->
          if not (List.mem x row) then note "outside");
    ]
  in
  let s = Stateful.stateful ~count:50 "nested" commands in
  equal (list string) [ "returned" ] (notes (fun () -> note (ended (body s))))

let nested_rows =
  [
    ( "a value, a row, a cell",
      fun t row cell check ->
        command "cell"
          (t ^-> row ^-> cell ^-> returns unit)
          (fun _ _ _ -> ())
          (fun () row x -> check row x) );
    ( "a cell, a row, a value",
      fun t row cell check ->
        command "cell"
          (cell ^-> row ^-> t ^-> returns unit)
          (fun _ _ _ -> ())
          (fun x row () -> check row x) );
  ]

let elements =
  group "Elements"
    [
      test
        "a call lists the value's elements after its values resolve, then asks \
         pre, and the element goes to pre, the system and the reference"
        element_trace;
      test "a value that lists no element skips the call, as a refused pre"
        unlisted;
      test "a call whose value does not resolve lists nothing" unresolved;
      test "every candidate can be taken" reachable;
      test
        "a candidate that deletes an earlier call keeps the element's relative \
         place"
        kept_place;
      test
        "an element's candidates take the earlier elements an index shrinks \
         through, each once" (fun () ->
          equal (list string)
            ([ "get d1 0"; "get d1 3"; "get d1 5"; "get d1 6" ]
            @ List.init 5 (fun _ -> unplaced))
            (element_candidates 7));
      test "no candidate of the head's element runs" (fun () ->
          equal (list string)
            (List.init 9 (fun _ -> unplaced))
            (element_candidates 0));
      cases "an element shrinks toward the head, to the least failing index"
        ~name:(fun (n, _, _) -> n)
        [
          ( "wrong from the second",
            1,
            " #  call\n\
            \ 1  let d1 = create ()\n\
            \ 2  get d1 1\n\
             [call 2 of 2: get d1 1] equality 1, 2\n\
             shrunk 8 steps" );
          ( "wrong from the middle",
            500,
            " #  call\n\
            \ 1  let d1 = create ()\n\
            \ 2  get d1 500\n\
             [call 2 of 2: get d1 500] equality 500, 501\n\
             shrunk 5 steps" );
        ]
        (fun (_, least, expected) ->
          equal string expected (shrunk_element least));
      cases
        "what the candidates raise breaks the reference, the element printed \
         as _, and a control keeps its meaning"
        ~name:(fun (n, _, _) -> n)
        listing_failures
        (fun (_, e, expected) -> equal string expected (listing_breaks e));
      cases "an element prints through its witness, as a drawn argument does"
        ~name:(fun (n, _, _) -> n)
        element_rows
        (fun (_, call, expected) -> equal string expected (call ()));
      cases
        "an element reads the nearest value of its type before it, else the \
         first after it"
        ~name:(fun (n, _, _) -> n)
        two_values_rows
        (fun (_, pick, expected) -> equal string expected (two_values pick));
      test "Map.find takes a key the map holds, in the API's order" map_find;
      test "Set.mem takes an element the set holds, in the API's order" set_mem;
      test "blit takes an index of each of its two arrays" blit;
      cases "an element may list elements in turn, before or after it" ~name:fst
        nested_rows (fun (_, signature) -> nested signature);
    ]

(* Declaring *)

let known_tags = [ "absent"; "custom"; "prop"; "stateful" ]

let declared () =
  let pos = ("spec.ml", 42, 0, 7) in
  let t =
    Stateful.stateful ~__POS__:pos ~tags:[ "custom" ] ~timeout:2.5 "spec"
      [ tick () ]
  in
  let c = single t in
  let tags = List.filter (fun n -> Test_tree.Tag.mem n c.tags) known_tags in
  let limit =
    match c.timeout with None -> "no limit" | Some s -> strf "limit %gs" s
  in
  equal string "spec: tags [custom, prop, stateful], limit 2.5s, spec.ml:42"
    (strf "%s: tags [%s], %s, %s" (String.concat "/" c.path)
       (String.concat ", " tags) limit (site c.loc))

let wiring () =
  let calls = ref 0 and checks = ref 0 and closes = ref 0 in
  let r =
    abstract "r"
      ~invariant:(fun () _ -> incr checks)
      ~release:(fun _ -> incr closes)
  in
  let c =
    command "open"
      (Gen.unit @-> makes r)
      (fun () -> incr calls)
      (fun () -> ref ())
  in
  body (Stateful.stateful ~count:3 ~steps:3 "wiring" [ c ]) ();
  equal string "9 calls, 18 invariant checks, 9 releases"
    (strf "%d calls, %d invariant checks, %d releases" !calls !checks !closes)

let never_called ?(elements = false) names =
  strf
    "never called: %s (over 20 passing cases); a call runs only where its \
     arguments resolve%s and its ~pre holds"
    names
    (if elements then ", its value lists an element" else "")

(* Seeds are the running test's, so every row holds over any seed. *)
let judged =
  [
    ( "a command listed twice",
      20,
      (fun () ->
        let never = dead "never" in
        [ never; never ]),
      never_called {|"never"|} );
    ( "beside a command called",
      20,
      (fun () -> [ tick (); dead "never" ]),
      never_called {|"never"|} );
    ( "two commands, in the order of the list",
      20,
      (fun () -> [ dead "b"; tick (); dead "a\nz" ]),
      never_called {|"b", "a z"|} );
    ( "two commands of one name",
      20,
      (fun () -> [ dead "x"; dead "x" ]),
      never_called {|"x", "x"|} );
    ( "a command whose type no command makes",
      20,
      (fun () ->
        [
          tick ();
          command "orphan" (abstract "t" ^-> returns unit) ignore ignore;
        ]),
      never_called {|"orphan"|} );
    ( "a command whose value lists no element",
      20,
      (fun () ->
        let d = abstract "d" in
        let index = among int d (fun () -> []) in
        [
          made "create" d;
          command "get"
            (d ^-> index ^-> returns unit)
            (fun () _ -> ())
            (fun () _ -> ());
        ]),
      never_called ~elements:true {|"get"|} );
    ( "a command some programs call",
      20,
      (fun () ->
        [
          command "mostly"
            ~pre:(fun x -> x < 3)
            (Gen.int_range 0 3 @-> returns unit)
            ignore ignore;
        ]),
      "passed" );
    ("no case", 0, (fun () -> [ dead "never" ]), "passed");
  ]

let verdict (_, count, commands, _) =
  let t = Stateful.stateful ~count ~steps:3 "spec" (commands ()) in
  match body t () with
  | () -> "passed"
  | exception Failure.Check_failure { kind = Message m; _ } -> m.kept

let declaration_site () =
  let pos = ("spec.ml", 42, 0, 7) in
  let t = Stateful.stateful ~__POS__:pos ~count:1 "spec" [ dead "never" ] in
  let f = require_some (failure (body t)) in
  equal string "spec.ml:42" (site f.loc)

let reading_places =
  let read _ = ignore (current_test ()) in
  [
    ("the reference", fun () -> one_call (unit_call "f" read ignore));
    ("the system", fun () -> one_call (unit_call "f" ignore read));
    ( "pre",
      fun () ->
        one_call
          (unit_call "f"
             ~pre:(fun () ->
               read ();
               true)
             ignore ignore) );
    ( "an invariant",
      fun () ->
        one_call (made "f" (abstract "r" ~invariant:(fun () () -> read ()))) );
    ("a release", fun () -> one_call (made "f" (abstract "r" ~release:read)));
    ( "the candidates of an among type",
      fun () ->
        let d = abstract "d" in
        let index =
          among int d (fun () ->
              read ();
              [ 0 ])
        in
        let gen =
          Stateful.program ~steps:2
            [
              made "create" d;
              command "get"
                (d ^-> index ^-> returns unit)
                (fun () _ -> ())
                (fun () _ -> ());
            ]
        in
        value
          (find gen
             (String.equal " #  call\n 1  let d1 = create ()\n 2  get d1 0")) );
  ]

let declaring =
  group "Declaring"
    [
      test "stateful declares a test tagged prop and stateful, at its site"
        declared;
      test "stateful runs count cases, each from no value" wiring;
      cases
        "a command that a passing case held and no passing case called fails \
         the test, named in the order of the commands"
        ~name:(fun (n, _, _, _) -> n)
        judged
        (fun ((_, _, _, expected) as r) -> equal string expected (verdict r));
      test "the declaration site locates a command never called"
        declaration_site;
      test "a timeout that is not finite and positive raises" (fun () ->
          raises_match Exn.invalid_arg (fun () ->
              Stateful.stateful ~timeout:0. "t" [ tick () ]));
      cases
        "a command's functions, pre, the candidates of an among type, an \
         invariant and a release may read the running test"
        ~name:fst reading_places (fun (_, program) ->
          equal string "returned" (ended (execute (program ()))));
    ]

let () =
  exit
    (run "stateful"
       [
         checking;
         commands;
         drawing;
         legality;
         outcomes;
         never_outcomes;
         judging;
         invariants;
         releases;
         the_record;
         summaries;
         screens;
         shrinking;
         elements;
         declaring;
       ])
