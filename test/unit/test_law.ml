(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* [Run.execute] refuses to start while a run is active, and every test body
   runs inside this suite's own run. The runs that show where a law demands,
   where its failure is located and how a report prints it are therefore
   recorded as the module initialises, before the [run] that ends the file. *)

open Windtrap
module Failure = Windtrap.Private.Failure
module Loc = Windtrap.Private.Loc
module Report = Windtrap.Private.Report
module Run = Windtrap.Private.Run

let strf = Printf.sprintf

exception Bug

let[@inline never] boom _ = raise (Stdlib.Failure "boom")

(* The screens of the proposal: a hard-to-state law in a prop, a decoder
   failing in a cases row, an always-true equality meeting its demand. They
   come first in the file, so that the lines a report quotes stay put. *)

module Version = struct
  type t = { nums : int list; pre : string option }

  let v ?pre nums = { nums; pre }

  let to_string t =
    String.concat "." (List.map string_of_int t.nums)
    ^ match t.pre with None -> "" | Some p -> "-" ^ p

  let pp ppf t = Format.pp_print_string ppf (to_string t)

  (* Structural: 1.2 and 1.2.0 differ. *)
  let equal = ( = )

  let pad t =
    {
      t with
      nums = t.nums @ List.init (max 0 (3 - List.length t.nums)) (fun _ -> 0);
    }

  (* Padded: 1.2 and 1.2.0 compare equal. *)
  let compare a b =
    let a = pad a and b = pad b in
    match List.compare Int.compare a.nums b.nums with
    | 0 -> Option.compare String.compare a.pre b.pre
    | c -> c

  let of_string s =
    let nums = List.map int_of_string_opt (String.split_on_char '.' s) in
    if List.for_all Option.is_some nums then Some (v (List.map Option.get nums))
    else None
end

module Path = struct
  type t = string list

  let of_segments l = l
  let segments p = p
  let pp ppf p = Format.pp_print_string ppf ("/" ^ String.concat "/" p)

  (* The defect: every pair is equal. *)
  let equal (_ : t) (_ : t) = true
end

let version =
  Testable.(
    with_compare Version.compare (make ~pp:Version.pp ~equal:Version.equal))

let v =
  Gen.(
    with_pp Version.pp
      (map
         (fun nums -> Version.v nums)
         (list ~size:(int_range 2 3) (int_range 0 9))))

let fixtures = [ Version.v [ 1; 2; 0 ]; Version.v ~pre:"a.1" [ 0; 0; 0 ] ]
let path = Testable.make ~pp:Path.pp ~equal:Path.equal
let dotted p = Path.of_segments ("." :: Path.segments p)

let g_path =
  Gen.(
    with_pp Path.pp
      (list (string_of ~size:(int_range 1 3) (char_range 'a' 'c'))))

(* The report of a recorded run, as [Report.run] renders it for dune. *)
let screens =
  let b = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer b in
  let report = Report.create ~out:ppf ~ansi:false (Run.default_config ()) in
  let on_event = Report.observe report ~seed:Recorded.seed ~selection:None in
  ignore
  @@ Recorded.execute ~on_event ~suite:"laws"
       [
         group "version"
           [
             prop "Version.compare is a total order"
               Gen.(triple v v v)
               (Law.order ~respell:Version.pad version);
             cases ~name:Version.to_string "of_string reads these back" fixtures
               (Law.round_trip version string Version.to_string (fun s ->
                    require_some (Version.of_string s)));
           ];
         group "path"
           [
             prop "Path.equal is an equivalence"
               Gen.(pair g_path g_path)
               (Law.equivalence ~respell:dotted path);
           ];
       ];
  Format.pp_print_flush ppf ();
  Buffer.contents b

(* What a law did *)

let located (f : Failure.t) =
  match f.loc with Some _ -> "located" | None -> "no location"

(* A nested failure as its location and its kind. *)
let nested (f : Failure.t) =
  let kind =
    match f.kind with
    | Equality { expected; actual; _ } ->
        strf "equality, expected %s, actual %s" expected.kept actual.kept
    | Raise
        {
          expected = None;
          actual = Some actual;
          predicate = false;
          backtrace;
          message_diff = None;
        } ->
        strf "uncaught %s%s" actual.kept
          (if Option.is_some backtrace then ", with a backtrace" else "")
    | Message m -> "message " ^ m.kept
    | Raise _ | Containment _ | Baseline _ | Property _ | Law _ | Timeout _ ->
        "another failure"
  in
  strf "%s, %s" (located f) kind

let term = function
  | Failure.Term { name; value } -> strf "  term [%s] %s" name value.kept
  | Side { name; value } -> strf "  side [%s] %s" name value.kept
  | Failed { name; failure } -> strf "  failed [%s] %s" name (nested failure)

(* A law's payload as rows: its name, its clause and its equation, then each
   term by kind, name and value. *)
let payload (f : Failure.t) =
  match f.kind with
  | Law { law; clause; equation; terms } ->
      let clause =
        match clause with Some c -> strf " (clause %s)" c | None -> ""
      in
      String.concat "\n"
        (strf "%s%s: %s" law clause equation :: List.map term terms)
  | Equality _ | Containment _ | Raise _ | Baseline _ | Property _ | Timeout _
  | Message _ ->
      "not a law's failure: " ^ nested f

let control = function
  | `Skip None -> "skip"
  | `Skip (Some reason) -> "skip " ^ reason
  | `Timeout limit -> strf "timeout %g" limit
  | `Exit -> "exit"
  | `Discard -> "discard"

let outcome law =
  match law () with
  | () -> "holds"
  | exception Failure.Check_failure f -> payload f
  | exception Failure.Control c -> control c
  | exception e -> "raised " ^ Failure.exn_to_string e

let failure_of law =
  match law () with () -> None | exception Failure.Check_failure f -> Some f

let failed law = require_some ~msg:"the law failed" (failure_of law)

(* The clause and the equation a failure names. *)
let head law =
  match failure_of law with
  | None -> "holds"
  | Some { kind = Law { clause; equation; _ }; _ } ->
      strf "%s: %s" (Option.value clause ~default:"-") equation
  | Some f -> "not a law's failure: " ^ nested f

let last_failed (f : Failure.t) =
  require_match
    (function
      | Failure.Law { terms; _ } -> (
          match List.rev terms with
          | Failure.Failed { failure; _ } :: _ -> Some failure
          | _ -> None)
      | _ -> None)
    f.kind

(* Witnesses *)

let pp_int = Format.pp_print_int

(* Equal and ordered modulo 10, so that [respell] builds an equal value
   apart. [say] hears each call of the equality and of the order. *)
let modulo ?(say = fun (_ : string) -> ()) () =
  Testable.make ~pp:pp_int ~equal:(fun x y ->
      say (strf "eq %d %d" x y);
      x mod 10 = y mod 10)
  |> Testable.with_compare (fun x y ->
      say (strf "cmp %d %d" x y);
      Int.compare (x mod 10) (y mod 10))

let mod10 = modulo ()
let respell x = x + 10

(* An order that ties the values its equality separates. *)
let tied_mod10 =
  Testable.with_compare (fun x y -> Int.compare (x mod 10) (y mod 10)) int

let by_abs = Testable.contramap abs int
let lower = Testable.make ~pp:pp_int ~equal:(fun a b -> a <= b)
let near = Testable.make ~pp:pp_int ~equal:(fun a b -> abs (a - b) <= 1)
let unordered = Testable.make ~pp:pp_int ~equal:Int.equal
let explosive = Testable.make ~pp:pp_int ~equal:(fun _ _ -> raise Bug)
let bad_pp = Testable.make ~pp:(fun _ _ -> raise Bug) ~equal:Int.equal
let ordered_bad_pp = Testable.with_compare Int.compare bad_pp
let raising_order = Testable.with_compare (fun _ _ -> raise Bug) int

let within_one =
  Testable.with_compare
    (fun x y -> if abs (x - y) <= 1 then 0 else Int.compare x y)
    int

(* Rock, paper, scissors over 1, 2 and 3: 1 below 2 below 3 below 1. *)
let cyclic =
  Testable.with_compare
    (fun x y -> if x = y then 0 else if (y - x + 3) mod 3 = 1 then -1 else 1)
    int

(* Ordered modulo 10, but a value of 10 or more is ordered the other way. *)
let reversed_above_ten =
  Testable.with_compare
    (fun x y ->
      if x >= 10 then Int.compare (y mod 10) (x mod 10)
      else Int.compare (x mod 10) (y mod 10))
    (Testable.make ~pp:pp_int ~equal:(fun x y -> x mod 10 = y mod 10))

(* An int witness with an order whose printer counts its calls. *)
let counting () =
  let calls = ref 0 in
  let pp ppf n =
    incr calls;
    pp_int ppf n
  in
  (Testable.with_compare Int.compare (Testable.make ~pp ~equal:Int.equal), calls)

(* Functions under test *)

let decode s = require_some ~__POS__ (int_of_string_opt s)
let loses_sign s = abs (int_of_string s)
let left a _ = a
let right _ b = b
let divides a b = b mod a = 0
let abs_leq a b = abs a <= abs b

(* Reflexive and antisymmetric, not transitive: 1 below 2 below 3, and 1 not
   below 3. *)
let up_one a b = b = a || b = a + 1

(* Laws that hold *)

let lawful =
  [
    ("equivalence", fun () -> Law.equivalence int (1, 2));
    ("equivalence, an equal pair", fun () -> Law.equivalence int (3, 3));
    ("equivalence, respelled", fun () -> Law.equivalence ~respell mod10 (1, 21));
    ("order", fun () -> Law.order int (3, 1, 2));
    ("order, ties", fun () -> Law.order int (2, 2, 1));
    ("order, respelled", fun () -> Law.order ~respell mod10 (1, 12, 3));
    ("partial_order", fun () -> Law.partial_order int divides (2, 3, 12));
    ( "partial_order, a preorder under the equality it induces",
      fun () -> Law.partial_order by_abs abs_leq (-2, 2, 3) );
    ("associative", fun () -> Law.associative string ( ^ ) ("a", "b", "c"));
    ("commutative", fun () -> Law.commutative int ( + ) (1, 2));
    ("neutral", fun () -> Law.neutral string ( ^ ) "" "a");
    ("absorbing", fun () -> Law.absorbing int ( * ) 0 5);
    ("invertible", fun () -> Law.invertible int ( + ) 0 Int.neg 5);
    ( "invertible, each value its own inverse",
      fun () -> Law.invertible int ( lxor ) 0 Fun.id 6 );
    ("distributive", fun () -> Law.distributive int ( * ) ~over:( + ) (2, 3, 4));
    ("idempotent", fun () -> Law.idempotent int abs (-3));
    ("involutive", fun () -> Law.involutive (list int) List.rev [ 1; 2; 3 ]);
    ("commutes", fun () -> Law.commutes int (( * ) 2) (( * ) 3) 5);
    ( "homomorphic",
      fun () ->
        Law.homomorphic (list int) int List.length ( @ ) ( + ) ([ 1 ], [ 2; 3 ])
    );
    ( "round_trip",
      fun () -> Law.round_trip int string string_of_int int_of_string (-42) );
    ( "round_trip, a partial decoder made total by require_some",
      fun () -> Law.round_trip int string string_of_int decode 7 );
    ( "round_trip, a partial decoder made total by require_ok",
      fun () ->
        Law.round_trip int string string_of_int
          (fun s ->
            require_ok ~pp:Format.pp_print_string
              (Option.to_result ~none:s (int_of_string_opt s)))
          7 );
    ("monotone", fun () -> Law.monotone int int succ (1, 3));
    ( "monotone, a pair given in reverse",
      fun () -> Law.monotone int int succ (3, 1) );
    ( "monotone, a strict pair whose images tie",
      fun () -> Law.monotone int int (fun _ -> 0) (1, 2) );
    ( "monotone, a pair the order ties, whose images tie",
      fun () -> Law.monotone by_abs int abs (-2, 2) );
    ( "ignores",
      fun () -> Law.ignores (list int) int List.length List.rev [ 1; 2; 3 ] );
    ("preserves", fun () -> Law.preserves int succ (fun x -> x > 0) 3);
  ]

let holding =
  group "Laws that hold"
    [
      cases "a law returns when its equation holds" ~name:fst lawful
        (fun (_, law) -> equal string "holds" (outcome law));
      prop "a law is the law of a prop"
        Gen.(triple small_int small_int small_int)
        (Law.associative int ( + ));
      prop "a law with a demand met is the law of a prop"
        Gen.(pair (int_range 0 3) (int_range 0 3))
        (Law.equivalence ~respell mod10);
      prop "monotone over drawn pairs"
        Gen.(pair nat nat)
        (Law.monotone int int succ);
      prop "partial_order over a chain drawn from its least value"
        Gen.(triple (int_range 1 9) (int_range 2 3) (int_range 2 3))
        (fun (a, k, l) -> Law.partial_order int divides (a, a * k, a * k * l));
      prop "invertible over drawn values" Gen.int
        (Law.invertible int ( + ) 0 Int.neg);
      prop "idempotent over drawn values, fixed points among them" Gen.int
        (Law.idempotent int abs);
      prop "involutive over drawn lists, palindromes among them"
        Gen.(list (int_range 0 1))
        (Law.involutive (list int) List.rev);
      cases "a law is the function of a cases" ~name:string_of_int [ -1; 0; 2 ]
        (Law.idempotent int abs);
    ]

(* Laws that fail *)

let broken =
  [
    ( "equivalence, not reflexive",
      (fun () ->
        Law.equivalence
          (Testable.make ~pp:pp_int ~equal:(fun _ _ -> false))
          (1, 2)),
      __POS_OF__
        {|
        equivalence (clause reflexive): a = a
          term [a] 1
          term [a = a] false
        |}
    );
    ( "equivalence, not symmetric",
      (fun () -> Law.equivalence lower (1, 2)),
      __POS_OF__
        {|
        equivalence (clause symmetric): a = b iff b = a
          term [a] 1
          term [b] 2
          term [a = b] true
          term [b = a] false
        |}
    );
    ( "equivalence, a respelling that is no equal value",
      (fun () -> Law.equivalence ~respell:succ int (1, 2)),
      __POS_OF__
        {|
        equivalence (clause respelled): a = r a
          term [a] 1
          term [r a] 2
          term [a = r a] false
        |}
    );
    ( "equivalence, a tolerance is not transitive",
      (fun () -> Law.equivalence ~respell:succ near (1, 5)),
      __POS_OF__
        {|
        equivalence (clause transitive): a = r (r a)
          term [a] 1
          term [r a] 2
          term [r (r a)] 3
          term [a = r (r a)] false
        |}
    );
    ( "equivalence, a respelling on the other side of b",
      (fun () ->
        Law.equivalence ~respell:(fun x -> if x = 1 then 2 else 1) near (1, 3)),
      __POS_OF__
        {|
        equivalence (clause transitive): r a = b iff a = b
          term [a] 1
          term [b] 3
          term [r a] 2
          term [r a = b] true
          term [a = b] false
        |}
    );
    ( "order, not reflexive",
      (fun () -> Law.order (Testable.with_compare (fun _ _ -> 1) int) (1, 2, 3)),
      __POS_OF__
        {|
        order (clause reflexive): cmp a a = 0
          term [a] 1
          term [cmp a a] 1
        |}
    );
    ( "order, not reflexive on c alone",
      (fun () ->
        Law.order
          (Testable.with_compare
             (fun x y -> if x = 3 && y = 3 then 1 else Int.compare x y)
             int)
          (1, 2, 3)),
      __POS_OF__
        {|
        order (clause reflexive): cmp c c = 0
          term [c] 3
          term [cmp c c] 1
        |}
    );
    ( "order, not antisymmetric",
      (fun () ->
        Law.order
          (Testable.with_compare (fun x y -> if x = y then 0 else 1) int)
          (1, 2, 3)),
      __POS_OF__
        {|
        order (clause antisymmetric): sign (cmp a b) = -sign (cmp b a)
          term [a] 1
          term [b] 2
          term [cmp a b] 1
          term [cmp b a] 1
        |}
    );
    ( "order, not antisymmetric on b and c alone",
      (fun () ->
        Law.order
          (Testable.with_compare
             (fun x y -> if (x, y) = (3, 2) then -1 else Int.compare x y)
             int)
          (1, 2, 3)),
      __POS_OF__
        {|
        order (clause antisymmetric): sign (cmp b c) = -sign (cmp c b)
          term [b] 2
          term [c] 3
          term [cmp b c] -1
          term [cmp c b] -1
        |}
    );
    ( "order, a tolerance is not transitive, at the last ordering",
      (fun () -> Law.order within_one (1, 2, 3)),
      __POS_OF__
        {|
        order (clause transitive): cmp c b <= 0 and cmp b a <= 0 imply cmp c a <= 0
          term [a] 1
          term [b] 2
          term [c] 3
          term [cmp c b] 0
          term [cmp b a] 0
          term [cmp c a] 1
        |}
    );
    ( "order, a cycle is not transitive, at the second ordering",
      (fun () -> Law.order cyclic (1, 3, 2)),
      __POS_OF__
        {|
        order (clause transitive): cmp a c <= 0 and cmp c b <= 0 imply cmp a b <= 0
          term [a] 1
          term [b] 3
          term [c] 2
          term [cmp a c] -1
          term [cmp c b] -1
          term [cmp a b] 1
        |}
    );
    ( "order, an order that ties unequal values",
      (fun () -> Law.order tied_mod10 (1, 11, 2)),
      __POS_OF__
        {|
        order (clause agrees with equal): cmp a b = 0 iff a = b
          term [a] 1
          term [b] 11
          term [cmp a b] 0
          term [a = b] false
        |}
    );
    ( "order, an order that ties unequal values b and c",
      (fun () -> Law.order tied_mod10 (1, 2, 12)),
      __POS_OF__
        {|
        order (clause agrees with equal): cmp b c = 0 iff b = c
          term [b] 2
          term [c] 12
          term [cmp b c] 0
          term [b = c] false
        |}
    );
    ( "order, Float.compare under float_exact's equality",
      (fun () ->
        Law.order (Testable.with_compare Float.compare float_exact) (0., -0., 1.)),
      __POS_OF__
        {|
        order (clause agrees with equal): cmp a b = 0 iff a = b
          term [a] 0.
          term [b] -0.
          term [cmp a b] 0
          term [a = b] false
        |}
    );
    ( "order, a respelling that is no equal value",
      (fun () -> Law.order ~respell:succ int (1, 2, 3)),
      __POS_OF__
        {|
        order (clause respelled): cmp a (r a) = 0
          term [a] 1
          term [r a] 2
          term [cmp a (r a)] -1
        |}
    );
    ( "order, a respelling the order ties and the equality separates",
      (fun () -> Law.order ~respell tied_mod10 (1, 2, 3)),
      __POS_OF__
        {|
        order (clause agrees with equal): cmp a (r a) = 0 iff a = r a
          term [a] 1
          term [r a] 11
          term [cmp a (r a)] 0
          term [a = r a] false
        |}
    );
    ( "order, a respelling ordered apart from its original",
      (fun () -> Law.order ~respell reversed_above_ten (1, 2, 3)),
      __POS_OF__
        {|
        order (clause respelled): sign (cmp (r a) b) = sign (cmp a b)
          term [a] 1
          term [b] 2
          term [r a] 11
          term [cmp a b] -1
          term [cmp (r a) b] 1
        |}
    );
    ( "partial_order, not reflexive",
      (fun () -> Law.partial_order int ( < ) (1, 2, 3)),
      __POS_OF__
        {|
        partial order (clause reflexive): leq a a
          term [a] 1
          term [leq a a] false
        |}
    );
    ( "partial_order, not antisymmetric",
      (fun () -> Law.partial_order int abs_leq (-2, 2, 3)),
      __POS_OF__
        {|
        partial order (clause antisymmetric): leq a b and leq b a imply a = b
          term [a] -2
          term [b] 2
          term [leq a b] true
          term [leq b a] true
          term [a = b] false
        |}
    );
    ( "partial_order, not antisymmetric on b and c alone",
      (fun () -> Law.partial_order int abs_leq (1, -2, 2)),
      __POS_OF__
        {|
        partial order (clause antisymmetric): leq b c and leq c b imply b = c
          term [b] -2
          term [c] 2
          term [leq b c] true
          term [leq c b] true
          term [b = c] false
        |}
    );
    ( "partial_order, not transitive",
      (fun () -> Law.partial_order int up_one (1, 2, 3)),
      __POS_OF__
        {|
        partial order (clause transitive): leq a b and leq b c imply leq a c
          term [a] 1
          term [b] 2
          term [c] 3
          term [leq a b] true
          term [leq b c] true
          term [leq a c] false
        |}
    );
    ( "partial_order, not transitive at the fourth ordering",
      (fun () -> Law.partial_order int up_one (3, 1, 2)),
      __POS_OF__
        {|
        partial order (clause transitive): leq b c and leq c a imply leq b a
          term [a] 3
          term [b] 1
          term [c] 2
          term [leq b c] true
          term [leq c a] true
          term [leq b a] false
        |}
    );
    ( "associative",
      (fun () -> Law.associative int ( - ) (1, 2, 3)),
      __POS_OF__
        {|
        associative: op (op a b) c = op a (op b c)
          term [a] 1
          term [b] 2
          term [c] 3
          term [op a b] -1
          term [op b c] -1
          side [op (op a b) c] -4
          side [op a (op b c)] 2
        |}
    );
    ( "commutative",
      (fun () -> Law.commutative int ( - ) (1, 2)),
      __POS_OF__
        {|
        commutative: op a b = op b a
          term [a] 1
          term [b] 2
          side [op a b] -1
          side [op b a] 1
        |}
    );
    ( "neutral, on the left",
      (fun () -> Law.neutral int ( - ) 0 5),
      __POS_OF__
        {|
        neutral: op e x = x
          term [e] 0
          side [op e x] -5
          side [x] 5
        |}
    );
    ( "neutral, on the right alone",
      (fun () -> Law.neutral int right 0 5),
      __POS_OF__
        {|
        neutral: op x e = x
          term [e] 0
          side [op x e] 0
          side [x] 5
        |}
    );
    ( "absorbing, on the left",
      (fun () -> Law.absorbing int ( + ) 0 1),
      __POS_OF__
        {|
        absorbing: op z x = z
          term [x] 1
          side [op z x] 1
          side [z] 0
        |}
    );
    ( "absorbing, on the right alone",
      (fun () -> Law.absorbing int left 0 5),
      __POS_OF__
        {|
        absorbing: op x z = z
          term [x] 5
          side [op x z] 5
          side [z] 0
        |}
    );
    ( "invertible, the inverse on the right",
      (fun () -> Law.invertible int ( + ) 0 Fun.id 5),
      __POS_OF__
        {|
        invertible: op x (inv x) = e
          term [x] 5
          term [inv x] 5
          side [op x (inv x)] 10
          side [e] 0
        |}
    );
    ( "invertible, the inverse on the left alone",
      (fun () -> Law.invertible int right 0 (fun _ -> 0) 5),
      __POS_OF__
        {|
        invertible: op (inv x) x = e
          term [x] 5
          term [inv x] 0
          side [op (inv x) x] 5
          side [e] 0
        |}
    );
    ( "distributive, on the left",
      (fun () -> Law.distributive int ( + ) ~over:( * ) (1, 2, 3)),
      __POS_OF__
        {|
        distributive: op a (over b c) = over (op a b) (op a c)
          term [a] 1
          term [b] 2
          term [c] 3
          term [over b c] 6
          term [op a b] 3
          term [op a c] 4
          side [op a (over b c)] 7
          side [over (op a b) (op a c)] 12
        |}
    );
    ( "distributive, on the right alone",
      (fun () -> Law.distributive int right ~over:( + ) (1, 2, 3)),
      __POS_OF__
        {|
        distributive: op (over a b) c = over (op a c) (op b c)
          term [a] 1
          term [b] 2
          term [c] 3
          term [over a b] 3
          term [op a c] 3
          term [op b c] 3
          side [op (over a b) c] 3
          side [over (op a c) (op b c)] 6
        |}
    );
    ( "idempotent",
      (fun () -> Law.idempotent int succ 1),
      __POS_OF__
        {|
      idempotent: f (f x) = f x
        term [x] 1
        side [f (f x)] 3
        side [f x] 2
      |}
    );
    ( "involutive",
      (fun () -> Law.involutive int succ 1),
      __POS_OF__
        {|
      involutive: f (f x) = x
        term [f x] 2
        side [f (f x)] 3
        side [x] 1
      |}
    );
    ( "commutes",
      (fun () -> Law.commutes int succ (( * ) 2) 1),
      __POS_OF__
        {|
      commutes: f (g x) = g (f x)
        term [x] 1
        term [g x] 2
        term [f x] 2
        side [f (g x)] 3
        side [g (f x)] 4
      |}
    );
    ( "homomorphic, the domain under the first witness",
      (fun () ->
        Law.homomorphic (list int) int List.length ( @ ) ( * ) ([ 1 ], [ 2; 3 ])),
      __POS_OF__
        {|
        homomorphic: f (op a b) = op' (f a) (f b)
          term [a] [1]
          term [b] [2; 3]
          term [op a b] [1; 2; 3]
          term [f a] 1
          term [f b] 2
          side [f (op a b)] 3
          side [op' (f a) (f b)] 2
        |}
    );
    ( "round_trip, the encoding under the second witness",
      (fun () -> Law.round_trip int string string_of_int loses_sign (-5)),
      __POS_OF__
        {|
        round trip: g (f x) = x
          term [f x] "-5"
          side [g (f x)] 5
          side [x] -5
        |}
    );
    ( "monotone, the pair sorted by the first witness",
      (fun () -> Law.monotone int int Int.neg (3, 1)),
      __POS_OF__
        {|
        monotone: a <= b implies f a <= f b
          term [a] 1
          term [b] 3
          term [cmp a b] -1
          term [f a] -1
          term [f b] -3
          term [cmp (f a) (f b)] 1
        |}
    );
    ( "monotone, the images under the second witness",
      (fun () -> Law.monotone int string string_of_int (9, 10)),
      __POS_OF__
        {|
        monotone: a <= b implies f a <= f b
          term [a] 9
          term [b] 10
          term [cmp a b] -1
          term [f a] "9"
          term [f b] "10"
          term [cmp (f a) (f b)] 1
        |}
    );
    ( "monotone, a pair the order ties whose images do not",
      (fun () -> Law.monotone by_abs int Fun.id (-2, 2)),
      __POS_OF__
        {|
        monotone: a <= b implies f a <= f b
          term [a] 2
          term [b] 2
          term [cmp a b] 0
          term [f a] -2
          term [f b] 2
          term [cmp (f a) (f b)] -1
        |}
    );
    ( "ignores",
      (fun () -> Law.ignores (list int) int List.length List.tl [ 1; 2 ]),
      __POS_OF__
        {|
        ignores: f (g x) = f x
          term [x] [1; 2]
          term [g x] [2]
          side [f (g x)] 1
          side [f x] 2
        |}
    );
    ( "preserves, a false premise",
      (fun () -> Law.preserves int succ (fun x -> x > 0) (-1)),
      __POS_OF__
        {|
        preserves (clause premise): inv x
          term [x] -1
          term [inv x] false
        |}
    );
    ( "preserves, a false implication",
      (fun () -> Law.preserves int Int.neg (fun x -> x > 0) 3),
      __POS_OF__
        {|
        preserves: inv x implies inv (f x)
          term [x] 3
          term [inv x] true
          term [f x] -3
          term [inv (f x)] false
        |}
    );
  ]

let failing =
  group "Laws that fail"
    [
      cases
        "a law raises one failure that names it, states its equation and lists \
         its terms"
        ~name:(fun (name, _, _) -> name)
        broken
        (fun (_, law, literal) -> expect (outcome law) literal);
    ]

(* Clause order *)

let heard () =
  let log = ref [] in
  ((fun s -> log := s :: !log), fun () -> List.rev !log)

let equivalence_calls () =
  let say, calls = heard () in
  let respell x =
    say (strf "r %d" x);
    x + 10
  in
  Law.equivalence ~respell (modulo ~say ()) (1, 2);
  equal (list string)
    [
      "eq 1 1";
      "eq 2 2";
      "eq 1 2";
      "eq 2 1";
      "r 1";
      "eq 1 11";
      "eq 11 1";
      "r 11";
      "eq 1 21";
      "eq 11 2";
    ]
    (calls ())

(* The witness of [modulo] whose [k]th equality answers wrong. *)
let wrong_at k =
  let calls = ref 0 in
  Testable.make ~pp:pp_int ~equal:(fun x y ->
      incr calls;
      let eq = x mod 10 = y mod 10 in
      if !calls = k then not eq else eq)

let equivalence_clauses =
  [
    (1, "reflexive: a = a");
    (2, "reflexive: b = b");
    (3, "symmetric: a = b iff b = a");
    (4, "symmetric: a = b iff b = a");
    (5, "respelled: a = r a");
    (6, "respelled: r a = a");
    (7, "transitive: a = r (r a)");
    (8, "transitive: r a = b iff a = b");
    (9, "holds");
  ]

let order_calls () =
  let say, calls = heard () in
  let respell x =
    say (strf "r %d" x);
    x + 10
  in
  Law.order ~respell (modulo ~say ()) (1, 2, 3);
  let compared =
    List.filter (fun s -> not (String.starts_with ~prefix:"eq" s))
  in
  equal (list string)
    [
      (* reflexive *)
      "cmp 1 1";
      "cmp 2 2";
      "cmp 3 3";
      (* antisymmetric *)
      "cmp 1 2";
      "cmp 2 1";
      "cmp 1 3";
      "cmp 3 1";
      "cmp 2 3";
      "cmp 3 2";
      (* transitive: a b c, then a c b, b a c, b c a, c a b, c b a *)
      "cmp 1 2";
      "cmp 2 3";
      "cmp 1 3";
      "cmp 1 3";
      "cmp 3 2";
      "cmp 2 1";
      "cmp 2 3";
      "cmp 3 1";
      "cmp 3 1";
      "cmp 3 2";
      (* agrees with equal *)
      "cmp 1 2";
      "cmp 1 3";
      "cmp 2 3";
      (* respelled, agrees with equal, respelled *)
      "r 1";
      "cmp 1 11";
      "cmp 1 11";
      "cmp 1 2";
      "cmp 11 2";
    ]
    (compared (calls ()))

let partial_order_calls () =
  let say, calls = heard () in
  let leq x y =
    say (strf "leq %d %d" x y);
    x <= y
  in
  Law.partial_order (modulo ~say ()) leq (2, 1, 3);
  equal (list string)
    [
      (* reflexive *)
      "leq 2 2";
      "leq 1 1";
      "leq 3 3";
      (* antisymmetric: a b, a c, b c *)
      "leq 2 1";
      "leq 2 3";
      "leq 3 2";
      "leq 1 3";
      "leq 3 1";
      (* transitive: a b c, then a c b, b a c, b c a, c a b, c b a *)
      "leq 2 1";
      "leq 2 3";
      "leq 3 1";
      "leq 1 2";
      "leq 2 3";
      "leq 1 3";
      "leq 1 3";
      "leq 3 2";
      "leq 3 2";
      "leq 3 1";
      (* a strict chain: no two values equal *)
      "eq 2 1";
      "eq 2 3";
      "eq 1 3";
    ]
    (calls ())

let invertible_calls () =
  let say, calls = heard () in
  let op a b =
    say (strf "op %d %d" a b);
    a + b
  and inv x =
    say (strf "inv %d" x);
    -x
  in
  Law.invertible int op 0 inv 5;
  equal (list string) [ "inv 5"; "op 5 -5"; "op -5 5" ] (calls ())

let distributive_calls () =
  let say, calls = heard () in
  let op a b =
    say (strf "op %d %d" a b);
    a * b
  and over a b =
    say (strf "over %d %d" a b);
    a + b
  in
  Law.distributive int op ~over (2, 3, 4);
  equal (list string)
    [
      "over 3 4";
      "op 2 7";
      "op 2 3";
      "op 2 4";
      "over 6 8";
      "over 2 3";
      "op 5 4";
      "op 3 4";
      "over 8 12";
    ]
    (calls ())

let monotone_calls () =
  let say, calls = heard () in
  let f x =
    say (strf "f %d" x);
    x
  in
  Law.monotone int int f (3, 1);
  equal (list string) [ "f 1"; "f 3" ] (calls ())

let clause_order =
  group "Clause order"
    [
      test
        "equivalence checks a, then b, then symmetry, then with r the \
         respelled and transitive clauses"
        equivalence_calls;
      cases "a wrong equality fails the clause of the call it answered"
        ~name:(fun (k, _) -> strf "call %d" k)
        equivalence_clauses
        (fun (k, row) ->
          equal string row
            (head (fun () -> Law.equivalence ~respell (wrong_at k) (1, 2))));
      test
        "order compares reflexively, antisymmetrically, over six orderings \
         with cmp x z only after its premise, then for agreement, then with r"
        order_calls;
      test
        "partial_order relates reflexively, then antisymmetrically and over \
         six orderings with each premise first, then compares a chain's values"
        partial_order_calls;
      test "invertible computes inv x once, the inverse on the right first"
        invertible_calls;
      test "distributive computes op a c once, the left law first"
        distributive_calls;
      test "monotone applies f to the pair it sorted, the smaller first"
        monotone_calls;
    ]

(* Failed terms *)

let nested_as_raised () =
  let raised = ref None in
  let decode s =
    match require_some (int_of_string_opt s) with
    | n -> n
    | exception (Failure.Check_failure f as e) ->
        raised := Some f;
        raise e
  in
  let f =
    failed (fun () -> Law.round_trip int string (fun _ -> "x") decode 7)
  in
  is_true ~msg:"the failed term holds the failure raised"
    (last_failed f == require_some !raised)

let backtrace_of_uncaught () =
  let f = failed (fun () -> Law.idempotent int boom 1) in
  let backtrace =
    require_match
      (function
        | Failure.Raise { backtrace = Some b; _ } -> Some b.kept | _ -> None)
      (last_failed f).kind
  in
  contains ~sub:"Test_law.boom" backtrace

let failed_rows =
  [
    ( "a require_some failure is nested at its own location",
      (fun () -> Law.round_trip int string (fun _ -> "x") decode 7),
      __POS_OF__
        {|
        round trip: g (f x) = x
          term [x] 7
          term [f x] "x"
          failed [g (f x)] located, equality, expected Some _, actual None
        |}
    );
    ( "a require_ok failure is nested at its own location",
      (fun () ->
        Law.round_trip int string string_of_int
          (fun s -> require_ok ~__POS__ ~pp:Format.pp_print_string (Error s))
          7),
      __POS_OF__
        {|
        round trip: g (f x) = x
          term [x] 7
          term [f x] "7"
          failed [g (f x)] located, equality, expected Ok _, actual Error 7
        |}
    );
    ( "any other exception is uncaught, with a backtrace and no location",
      (fun () -> Law.involutive int boom 1),
      __POS_OF__
        {|
        involutive: f (f x) = x
          term [x] 1
          failed [f x] no location, uncaught Failure("boom"), with a backtrace
        |}
    );
    ( "the terms before the failed one are kept, and none after",
      (fun () ->
        Law.associative int
          (fun a b -> if (a, b) = (2, 3) then boom () else a + b)
          (1, 2, 3)),
      __POS_OF__
        {|
        associative: op (op a b) c = op a (op b c)
          term [a] 1
          term [b] 2
          term [c] 3
          term [op a b] 3
          term [op (op a b) c] 6
          failed [op b c] no location, uncaught Failure("boom"), with a backtrace
        |}
    );
    ( "a respelling that raises",
      (fun () -> Law.equivalence ~respell:boom int (1, 2)),
      __POS_OF__
        {|
        equivalence (clause respelled): a = r a
          term [a] 1
          failed [r a] no location, uncaught Failure("boom"), with a backtrace
        |}
    );
    ( "a premise that raises",
      (fun () -> Law.preserves int succ boom 1),
      __POS_OF__
        {|
        preserves (clause premise): inv x
          term [x] 1
          failed [inv x] no location, uncaught Failure("boom"), with a backtrace
        |}
    );
    ( "a relation that raises",
      (fun () -> Law.partial_order int boom (1, 2, 3)),
      __POS_OF__
        {|
        partial order (clause reflexive): leq a a
          term [a] 1
          failed [leq a a] no location, uncaught Failure("boom"), with a backtrace
        |}
    );
    ( "an inverse that raises",
      (fun () -> Law.invertible int ( + ) 0 boom 1),
      __POS_OF__
        {|
        invertible: op x (inv x) = e
          term [e] 0
          term [x] 1
          failed [inv x] no location, uncaught Failure("boom"), with a backtrace
        |}
    );
    ( "a function that fails an assertion",
      (fun () -> Law.idempotent int (fun _ -> fail ~__POS__ "no image") 1),
      __POS_OF__
        {|
        idempotent: f (f x) = f x
          term [x] 1
          failed [f x] located, message no image
        |}
    );
  ]

let passed_through =
  [
    ("a skip", Failure.Control (`Skip (Some "r")));
    ("a timeout", Failure.Control (`Timeout 2.5));
    ("an exit", Failure.Control `Exit);
    ("a discard", Failure.Control `Discard);
    ("an interrupt", Sys.Break);
    ("exhausted memory", Out_of_memory);
  ]

let failed_terms =
  group "Failed terms"
    [
      cases "a function under test that raises or fails ends the law there"
        ~name:(fun (name, _, _) -> name)
        failed_rows
        (fun (_, law, literal) -> expect (outcome law) literal);
      test "a failed term holds the very failure its function raised"
        nested_as_raised;
      test "an uncaught exception keeps the backtrace of its raise"
        backtrace_of_uncaught;
      cases "a control or a fatal exception passes through the law as raised"
        ~name:fst passed_through (fun (_, e) ->
          equal string
            (outcome (fun () -> raise e))
            (outcome (fun () -> Law.idempotent int (fun _ -> raise e) 1)));
      test "a skip in the function under test skips" (fun () ->
          equal string "skip why"
            (outcome (fun () ->
                 Law.round_trip int int Fun.id
                   (fun _ -> skip ~reason:"why" ())
                   1)));
    ]

(* Witnesses *)

let over_ints : (string * (int Testable.t -> unit)) list =
  [
    ("equivalence", fun w -> Law.equivalence ~respell:Fun.id w (1, 2));
    ("order", fun w -> Law.order ~respell:Fun.id w (3, 1, 2));
    ("partial_order", fun w -> Law.partial_order w ( <= ) (3, 1, 2));
    ("associative", fun w -> Law.associative w ( + ) (1, 2, 3));
    ("commutative", fun w -> Law.commutative w ( + ) (1, 2));
    ("neutral", fun w -> Law.neutral w ( + ) 0 5);
    ("absorbing", fun w -> Law.absorbing w ( * ) 0 5);
    ("invertible", fun w -> Law.invertible w ( + ) 0 Int.neg 5);
    ("distributive", fun w -> Law.distributive w ( * ) ~over:( + ) (2, 3, 4));
    ("idempotent", fun w -> Law.idempotent w abs (-3));
    ("involutive", fun w -> Law.involutive w Int.neg 4);
    ("commutes", fun w -> Law.commutes w (( * ) 2) (( * ) 3) 5);
    ("homomorphic", fun w -> Law.homomorphic w w (( * ) 2) ( + ) ( + ) (1, 2));
    ("round_trip", fun w -> Law.round_trip w w Int.neg Int.neg 7);
    ("monotone", fun w -> Law.monotone w w succ (3, 1));
    ("ignores", fun w -> Law.ignores w w abs Int.neg 5);
    ("preserves", fun w -> Law.preserves w succ (fun x -> x > 0) 3);
  ]

let printer_calls law =
  let w, calls = counting () in
  (match law w with () -> () | exception Failure.Check_failure _ -> ());
  !calls

let escapes =
  [
    ( "associative, the equality",
      fun () -> Law.associative explosive ( + ) (1, 2, 3) );
    ( "equivalence, the equality under test",
      fun () -> Law.equivalence explosive (1, 2) );
    ("order, the order under test", fun () -> Law.order raising_order (1, 2, 3));
    ( "order, the equality under test",
      fun () -> Law.order (Testable.with_compare Int.compare explosive) (1, 2, 3)
    );
    ( "partial_order, the equality",
      fun () -> Law.partial_order explosive ( <= ) (1, 1, 2) );
    ( "invertible, the equality",
      fun () -> Law.invertible explosive ( + ) 0 Int.neg 1 );
    ( "monotone, the first order",
      fun () -> Law.monotone raising_order int succ (1, 2) );
    ( "monotone, the second order",
      fun () -> Law.monotone int raising_order succ (1, 2) );
    ( "associative, the printer",
      fun () -> Law.associative bad_pp ( - ) (1, 2, 3) );
    ( "partial_order, the printer",
      fun () -> Law.partial_order bad_pp ( < ) (1, 2, 3) );
    ( "equivalence, the printer",
      fun () -> Law.equivalence ~respell:succ bad_pp (1, 2) );
    ( "round_trip, the second witness's printer",
      fun () -> Law.round_trip int bad_pp Fun.id succ 1 );
    ( "monotone, the first witness's printer",
      fun () -> Law.monotone ordered_bad_pp int Int.neg (1, 2) );
  ]

let witnesses =
  group "Witnesses"
    [
      cases "what a witness's equality, order or printer raises escapes the law"
        ~name:fst escapes (fun (_, law) ->
          equal string "raised Test_law.Bug" (outcome law));
      cases "a law that holds calls no printer" ~name:fst over_ints
        (fun (_, law) -> equal int 0 (printer_calls law));
      test "a failing law prints each of its terms once" (fun () ->
          equal int 7
            (printer_calls (fun w -> Law.associative w ( - ) (1, 2, 3)));
          equal int 3 (printer_calls (fun w -> Law.idempotent w succ 1)));
    ]

(* Locations and messages *)

let pos = ("fake.ml", 42, 3, 9)
let site = "why at fake.ml:42:3"

let annotation law =
  match law () with
  | () -> "returned"
  | exception Failure.Check_failure f ->
      strf "%s at %s"
        (match f.msg with Some m -> m.kept | None -> "nothing")
        (match f.loc with
        | Some l -> strf "%s:%d:%d" l.file l.line l.column
        | None -> "nowhere")

let every_law =
  [
    ( "equivalence",
      fun () -> Law.equivalence ~__POS__:pos ~msg:"why" ~respell:succ int (1, 2)
    );
    ( "order",
      fun () -> Law.order ~__POS__:pos ~msg:"why" ~respell:succ int (1, 2, 3) );
    ( "partial_order",
      fun () -> Law.partial_order ~__POS__:pos ~msg:"why" int ( < ) (1, 2, 3) );
    ( "associative",
      fun () -> Law.associative ~__POS__:pos ~msg:"why" int ( - ) (1, 2, 3) );
    ( "commutative",
      fun () -> Law.commutative ~__POS__:pos ~msg:"why" int ( - ) (1, 2) );
    ("neutral", fun () -> Law.neutral ~__POS__:pos ~msg:"why" int ( - ) 0 5);
    ("absorbing", fun () -> Law.absorbing ~__POS__:pos ~msg:"why" int ( + ) 0 1);
    ( "invertible",
      fun () -> Law.invertible ~__POS__:pos ~msg:"why" int ( + ) 0 Fun.id 5 );
    ( "distributive",
      fun () ->
        Law.distributive ~__POS__:pos ~msg:"why" int ( + ) ~over:( * ) (1, 2, 3)
    );
    ("idempotent", fun () -> Law.idempotent ~__POS__:pos ~msg:"why" int succ 1);
    ("involutive", fun () -> Law.involutive ~__POS__:pos ~msg:"why" int succ 1);
    ( "commutes",
      fun () -> Law.commutes ~__POS__:pos ~msg:"why" int succ (( * ) 2) 1 );
    ( "homomorphic",
      fun () ->
        Law.homomorphic ~__POS__:pos ~msg:"why" int int succ ( + ) ( + ) (1, 2)
    );
    ( "round_trip",
      fun () -> Law.round_trip ~__POS__:pos ~msg:"why" int int succ succ 1 );
    ( "monotone",
      fun () -> Law.monotone ~__POS__:pos ~msg:"why" int int Int.neg (1, 2) );
    ( "ignores",
      fun () -> Law.ignores ~__POS__:pos ~msg:"why" int int Fun.id succ 1 );
    ( "preserves",
      fun () ->
        Law.preserves ~__POS__:pos ~msg:"why" int succ (fun _ -> false) 1 );
    ( "a failed term",
      fun () ->
        Law.round_trip ~__POS__:pos ~msg:"why" int string
          (fun _ -> "x")
          decode 7 );
  ]

let term_keeps_its_site () =
  let f =
    failed (fun () ->
        Law.round_trip ~__POS__:pos int string (fun _ -> "x") decode 7)
  in
  equal (option string) (Some "fake.ml")
    (Option.map (fun (l : Loc.t) -> l.file) f.loc);
  starts_with ~affix:"test/unit/test_law.ml"
    (require_some (last_failed f).loc).file

let located_rows =
  group "Locations and messages"
    [
      cases "?msg and ?__POS__ are the failure's msg and site" ~name:fst
        every_law (fun (_, law) -> equal string site (annotation law));
      test "without ?__POS__ a law is located in the caller's file" (fun () ->
          starts_with ~affix:"nothing at test/unit/test_law.ml:"
            (annotation (fun () ->
                 Law.idempotent int succ 1;
                 ())));
      test "a law in tail position has no location of its own" (fun () ->
          equal string "nothing at nowhere"
            (annotation (fun () ->
                 Loc.delimit (fun () -> Law.idempotent int succ 1))));
      test "a failed term's failure keeps its own site, not the law's"
        term_keeps_its_site;
    ]

(* No order *)

let orderless =
  let refused law witness =
    strf
      "Windtrap.Law.%s: %s has no order; give it one with Testable.with_compare"
      law witness
  in
  [
    ("order", (fun w _ -> Law.order w (1, 2, 3)), refused "order" "the witness");
    ( "order, respelled",
      (fun w f -> Law.order ~respell:f w (1, 2, 3)),
      refused "order" "the witness" );
    ( "monotone, the first witness",
      (fun w f -> Law.monotone w int f (1, 2)),
      refused "monotone" "the first witness" );
    ( "monotone, the second witness",
      (fun w f ->
        Law.monotone (Testable.with_compare Int.compare w) unordered f (1, 2)),
      refused "monotone" "the second witness" );
    ( "monotone, both witnesses",
      (fun w f -> Law.monotone w unordered f (1, 2)),
      refused "monotone" "the first witness" );
  ]

let refuses_before_any_clause (_, law, message) =
  let say, calls = heard () in
  let w =
    Testable.make ~pp:pp_int ~equal:(fun x y ->
        say (strf "eq %d %d" x y);
        x = y)
  in
  let f x =
    say (strf "f %d" x);
    x
  in
  raises (Invalid_argument message) (fun () -> law w f);
  equal (list string) [] (calls ())

let no_order =
  group "No order"
    [
      cases "order and monotone refuse a witness without an order, first"
        ~name:(fun (name, _, _) -> name)
        orderless refuses_before_any_clause;
    ]

(* Demands *)

(* The law of a property that draws [value] in each of five cases. *)
let demanded value law =
  match Run.property ~count:5 (Gen.constant value) law with
  | () -> "covered"
  | exception Failure.Check_failure { kind = Message m; _ } -> m.kept

let fn =
  Testable.make
    ~pp:(fun ppf _ -> Format.pp_print_string ppf "<fun>")
    ~equal:( == )

let applied h = h 0
let wrapped h y = h y
let paired = Testable.make ~pp:(fun ppf (n, _) -> pp_int ppf n) ~equal:( == )
let never label = strf "never covered: %S (over 5 passing cases)" label

let demand_rows =
  [
    ( "equivalence, an equal pair",
      (fun () -> demanded (1, 1) (Law.equivalence int)),
      never "equivalence: an unequal pair" );
    ( "equivalence, an unequal pair",
      (fun () -> demanded (1, 2) (Law.equivalence int)),
      "covered" );
    ( "equivalence, the always-true equality",
      (fun () -> demanded (1, 2) (Law.equivalence pass)),
      never "equivalence: an unequal pair" );
    ( "equivalence, a respelling that builds the same value",
      (fun () -> demanded (1, 2) (Law.equivalence ~respell:Fun.id int)),
      never "equivalence: r a differs from a" );
    ( "equivalence, a respelling that copies its value",
      (fun () ->
        demanded ([ 1 ], [ 2 ])
          (Law.equivalence ~respell:(List.map Fun.id) (list int))),
      never "equivalence: r a differs from a" );
    ( "equivalence, a respelling apart",
      (fun () -> demanded (1, 2) (Law.equivalence ~respell mod10)),
      "covered" );
    ( "equivalence, both demands unmet",
      (fun () -> demanded (1, 1) (Law.equivalence ~respell:Fun.id int)),
      {|never covered: "equivalence: an unequal pair", "equivalence: r a differs from a" (over 5 passing cases)|}
    );
    ( "order, a equal to b and apart from c",
      (fun () -> demanded (1, 1, 2) (Law.order int)),
      never "order: an unequal pair" );
    ( "order, a apart from b",
      (fun () -> demanded (1, 2, 2) (Law.order int)),
      "covered" );
    ( "order, a respelling that builds the same value",
      (fun () -> demanded (1, 2, 3) (Law.order ~respell:Fun.id int)),
      never "order: r a differs from a" );
    ( "order, a respelling apart",
      (fun () -> demanded (1, 2, 3) (Law.order ~respell mod10)),
      "covered" );
    ( "partial_order, three values no two related",
      (fun () -> demanded (2, 3, 5) (Law.partial_order int divides)),
      never "partial order: a strict chain" );
    ( "partial_order, a chain over two equal values",
      (fun () -> demanded (2, 2, 4) (Law.partial_order int divides)),
      never "partial order: a strict chain" );
    ( "partial_order, a strict chain",
      (fun () -> demanded (2, 4, 12) (Law.partial_order int divides)),
      "covered" );
    ( "partial_order, a strict chain in another order",
      (fun () -> demanded (12, 2, 4) (Law.partial_order int divides)),
      "covered" );
    ( "idempotent, f the identity",
      (fun () -> demanded 1 (Law.idempotent int Fun.id)),
      never "idempotent: f x differs from x" );
    ( "idempotent, x a fixed point of f",
      (fun () -> demanded 3 (Law.idempotent int abs)),
      never "idempotent: f x differs from x" );
    ( "idempotent, f moves x",
      (fun () -> demanded (-3) (Law.idempotent int abs)),
      "covered" );
    ( "involutive, f the identity",
      (fun () -> demanded 1 (Law.involutive int Fun.id)),
      never "involutive: f x differs from x" );
    ( "involutive, x a fixed point of f",
      (fun () -> demanded [ 1; 2; 1 ] (Law.involutive (list int) List.rev)),
      never "involutive: f x differs from x" );
    ( "involutive, f moves x",
      (fun () -> demanded 1 (Law.involutive int Int.neg)),
      "covered" );
    ( "monotone, an equal pair",
      (fun () -> demanded (1, 1) (Law.monotone int int succ)),
      never "monotone: a strict pair" );
    ( "monotone, a pair the order ties",
      (fun () -> demanded (-2, 2) (Law.monotone by_abs int abs)),
      never "monotone: a strict pair" );
    ( "monotone, a strict pair",
      (fun () -> demanded (1, 2) (Law.monotone int int succ)),
      "covered" );
    ( "ignores, g the identity",
      (fun () -> demanded 1 (Law.ignores int int Fun.id Fun.id)),
      never "ignores: g x differs from x" );
    ( "ignores, g a copy",
      (fun () ->
        demanded [ 1 ]
          (Law.ignores (list int) int List.length (List.map Fun.id))),
      never "ignores: g x differs from x" );
    ( "ignores, g moves x",
      (fun () -> demanded 1 (Law.ignores int int abs Int.neg)),
      "covered" );
    ( "ignores, g returns the function it was given",
      (fun () -> demanded succ (Law.ignores fn int applied Fun.id)),
      never "ignores: g x differs from x" );
    ( "ignores, g returns another closure",
      (fun () -> demanded succ (Law.ignores fn int applied wrapped)),
      "covered" );
    ( "ignores, g copies a value that holds the same function",
      (fun () ->
        demanded (0, succ)
          (Law.ignores paired int (fun (n, h) -> h n) (fun (n, h) -> (n, h)))),
      never "ignores: g x differs from x" );
    ( "ignores, g copies a value that holds another closure",
      (fun () ->
        demanded (0, succ)
          (Law.ignores paired int
             (fun (n, h) -> h n)
             (fun (n, h) -> (n, wrapped h)))),
      "covered" );
    ( "associative demands nothing",
      (fun () -> demanded (1, 1, 1) (Law.associative int ( + ))),
      "covered" );
    ( "commutative demands nothing",
      (fun () -> demanded (1, 1) (Law.commutative int ( + ))),
      "covered" );
    ( "neutral demands nothing",
      (fun () -> demanded 0 (Law.neutral int ( + ) 0)),
      "covered" );
    ( "absorbing demands nothing",
      (fun () -> demanded 0 (Law.absorbing int ( * ) 0)),
      "covered" );
    ( "invertible demands nothing",
      (fun () -> demanded 0 (Law.invertible int ( + ) 0 Int.neg)),
      "covered" );
    ( "distributive demands nothing",
      (fun () -> demanded (0, 0, 0) (Law.distributive int ( * ) ~over:( + ))),
      "covered" );
    ( "commutes demands nothing",
      (fun () -> demanded 1 (Law.commutes int Fun.id Fun.id)),
      "covered" );
    ( "homomorphic demands nothing",
      (fun () -> demanded (0, 0) (Law.homomorphic int int Fun.id ( + ) ( + ))),
      "covered" );
    ( "round_trip demands nothing",
      (fun () -> demanded 1 (Law.round_trip int int Fun.id Fun.id)),
      "covered" );
    ( "preserves demands nothing",
      (fun () -> demanded 1 (Law.preserves int Fun.id (fun _ -> true))),
      "covered" );
  ]

(* Two calls of [ignores] on one case, the first vacuous. *)
let twice ?first ?second x =
  Law.ignores ?msg:first int int abs Fun.id x;
  Law.ignores ?msg:second int int abs Int.neg x

let labelled =
  [
    ( "a demand's label ends with the call's msg",
      (fun () -> demanded 1 (Law.ignores ~msg:"sign" int int Fun.id Fun.id)),
      never "ignores: g x differs from x (sign)" );
    ( "two calls without a msg share a demand, met by either",
      (fun () -> demanded 1 twice),
      "covered" );
    ( "two calls with a msg each demand apart",
      (fun () -> demanded 1 (twice ~first:"id" ~second:"neg")),
      never "ignores: g x differs from x (id)" );
    ( "a call with a msg and one without demand apart",
      (fun () -> demanded 1 (twice ~second:"neg")),
      never "ignores: g x differs from x" );
  ]

(* A law that holds and would demand a moved value. *)
let unmoved () = Law.ignores int int Fun.id Fun.id 1
let at_top_level = match unmoved () with () -> None | exception e -> Some e
let released = fixture ~teardown:(fun () -> unmoved ()) (fun () -> ())
let context_at_top_level = Run.prop_context ()

let where =
  Recorded.execute
    [
      prop ~count:10 "prop" (Gen.constant 1) (Law.ignores int int Fun.id Fun.id);
      stateful ~count:10 "stateful"
        [ command "law" (Gen.unit @-> returns unit) unmoved ignore ];
      stateful ~count:10 "system"
        [ command "law" (Gen.unit @-> returns unit) ignore unmoved ];
      test "test" unmoved;
      cases ~name:string_of_int "cases" [ 1; 2 ]
        (Law.ignores int int Fun.id Fun.id);
      test "fixture" released;
      test "context in a test" (fun () -> is_none (Run.prop_context ()));
    ]

let on_domain fn = Domain.join (Domain.spawn fn)

let on_domains =
  [
    prop ~count:10 "a law" (Gen.constant 1) (fun x ->
        on_domain (fun () -> Law.ignores int int Fun.id Fun.id x));
    prop ~count:10 "context" (Gen.constant 1) (fun _ ->
        is_none (on_domain Run.prop_context));
  ]

(* The tests that spawn a domain run in a forked child, which hands back
   each test's row, one line per test. The child's scratch directory is
   removed before it leaves by [_exit]. *)
let from_domains =
  Windtrap_test_support.Child.forked (fun () ->
      let r = Recorded.execute on_domains in
      let row name = name ^ " -> " ^ Recorded.row r [ name ] in
      let rows = List.map row [ "a law"; "context" ] in
      Windtrap_test_support.Scratch.remove_tree
        (Filename.dirname (Recorded.log_dir r));
      String.concat "\n" rows)

let from_domain name =
  match from_domains with
  | None -> skip ~reason:"POSIX only: the domains run in a forked child" ()
  | Some text ->
      let prefix = name ^ " -> " in
      require_some ~msg:text
        (List.find_opt
           (String.starts_with ~prefix)
           (String.split_on_char '\n' text))

let message r path =
  match Recorded.failures r path with
  | [ { kind = Message m; _ } ] -> m.kept
  | _ -> "not one message"

let prop_stats r path =
  let at (row : Run.result) = List.equal String.equal row.path path in
  match List.find_opt at (Run.results (Recorded.outcome r).run) with
  | Some row -> Option.is_some row.prop_stats
  | None -> false

let demands =
  group "Demands"
    [
      cases "a law demands, by label, the case that could be trivial"
        ~name:(fun (name, _, _) -> name)
        demand_rows
        (fun (_, run, row) -> equal string row (run ()));
      test "a prop's law registers its demands" (fun () ->
          equal string "fail body" (Recorded.row where [ "prop" ]);
          equal string
            {|never covered: "ignores: g x differs from x" (over 10 passing cases)|}
            (message where [ "prop" ]));
      test "a stateful test's law registers its demands" (fun () ->
          equal string "fail body" (Recorded.row where [ "stateful" ]);
          equal string
            {|never covered: "ignores: g x differs from x" (over 10 passing cases)|}
            (message where [ "stateful" ]));
      test "a stateful system function's law registers its demands" (fun () ->
          equal string "fail body" (Recorded.row where [ "system" ]);
          equal string
            {|never covered: "ignores: g x differs from x" (over 10 passing cases)|}
            (message where [ "system" ]));
      cases "two calls of one law share a demand unless their msgs differ"
        ~name:(fun (name, _, _) -> name)
        labelled
        (fun (_, run, row) -> equal string row (run ()));
      test "a test's law demands nothing and holds" (fun () ->
          equal string "pass" (Recorded.row where [ "test" ]);
          is_false (prop_stats where [ "test" ]));
      test "a cases row's law demands nothing and holds" (fun () ->
          equal (list string) [ "pass"; "pass" ]
            [
              Recorded.row where [ "cases"; "1" ];
              Recorded.row where [ "cases"; "2" ];
            ]);
      test "a law in a fixture's release demands nothing and holds" (fun () ->
          equal string "pass" (Recorded.row where [ "fixture" ]);
          equal int 0 (List.length (Recorded.outcome where).release_failures));
      test "a law outside any run demands nothing and holds" (fun () ->
          equal (option string) None
            (Option.map Printexc.to_string at_top_level));
      test "a law on another domain demands nothing and holds" (fun () ->
          equal string "a law -> pass" (from_domain "a law"));
      test "prop_context is None outside any run and in a test's body"
        (fun () ->
          is_none context_at_top_level;
          equal string "pass" (Recorded.row where [ "context in a test" ]));
      test "prop_context is None on another domain, which records nothing"
        (fun () -> equal string "context -> pass" (from_domain "context"));
    ]

(* Where a failure is reported *)

let declared = ("decl.ml", 7, 0, 0)

let sites =
  Recorded.execute
    [
      test ~__POS__:declared "tail position" (fun () ->
          Law.idempotent int succ 1);
      test ~__POS__:declared "a position" (fun () ->
          Law.idempotent ~__POS__:("law.ml", 3, 0, 0) int succ 1);
    ]

let site_of path =
  match Recorded.failures sites path with
  | [ { loc = Some l; _ } ] -> strf "%s:%d" l.file l.line
  | _ -> "not one located failure"

let uncaught =
  match Law.associative int ( - ) (1, 2, 3) with
  | () -> "returned"
  | exception e -> Printexc.to_string e

let uncaught_term =
  match Law.round_trip int string (fun _ -> "x") decode 7 with
  | () -> "returned"
  | exception e -> Printexc.to_string e

let reported =
  group "Where a failure is reported"
    [
      test "a law in tail position takes the declaration site" (fun () ->
          equal string "decl.ml:7" (site_of [ "tail position" ]));
      test "a law's ?__POS__ wins over the declaration site" (fun () ->
          equal string "law.ml:3" (site_of [ "a position" ]));
      test "raised outside a run, a law's failure prints as its headline"
        (fun () ->
          equal string
            "windtrap assertion failure: associative: op (op a b) c = op a (op \
             b c)"
            uncaught;
          equal string "windtrap assertion failure: round trip: g (f x) failed"
            uncaught_term);
    ]

let rule l = String.starts_with ~prefix:"\u{2500}" l

(* The report's failure blocks, between its two rules. *)
let blocks out =
  let rec before = function
    | [] -> []
    | l :: ls -> if rule l then within ls else before ls
  and within = function
    | [] -> []
    | l :: ls -> if rule l then [] else l :: within ls
  in
  String.concat "\n" (before (String.split_on_char '\n' out))

let proposal =
  group "The proposal's screens"
    [
      test "a law's failure reads in a report as the proposal shows it"
        (fun () ->
          expect (blocks screens)
          @@ __POS_OF__
               {|
            FAIL  version › Version.compare is a total order
              test/unit/test_law.ml:102
                102 │ prop "Version.compare is a total order"

              counterexample (case 1, shrunk 5 steps): (0.0, 0.0, 0.0)
              which failed with:
                order (agrees with equal): cmp a (r a) = 0 iff a = r a
                a            0.0
                r a          0.0.0
                cmp a (r a)  0
                a = r a      false
              labels (1 passing case):
                100.0%  order: an unequal pair

            FAIL  version › of_string reads these back › 0.0.0-a.1
              test/unit/test_law.ml:105
                105 │ cases ~name:Version.to_string "of_string reads these back" fixtures

              round trip: g (f x) = x
              x    0.0.0-a.1
              f x  "0.0.0-a.1"
              g (f x) failed with:
                expected  Some _
                actual    None

            FAIL  path › Path.equal is an equivalence
              test/unit/test_law.ml:111
                111 │ prop "Path.equal is an equivalence"

              never covered: "equivalence: an unequal pair" (over 100 passing cases)
              labels (100 passing cases):
                100.0%  equivalence: r a differs from a
              covered labels:
                equivalence: an unequal pair  0  never covered
                equivalence: r a differs from a  100
            |});
    ]

let () =
  exit
    (run "law"
       [
         holding;
         failing;
         clause_order;
         failed_terms;
         witnesses;
         located_rows;
         no_order;
         demands;
         reported;
         proposal;
       ])
