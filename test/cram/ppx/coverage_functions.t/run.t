The coverage rewriter's expansions of functions whose parameters, return
constraint or coercion the compiler's printer spells by its version, as
pp.exe prints them, condensed by elide.exe as in coverage.t.

  $ export OCAML_COLOR=never
  $ cov() { ../pp.exe -apply windtrap_coverage "$@"; }

  $ cov --impl ./fixture_entries.ml | ../elide.exe
  coverage points of "./fixture_entries.ml" in Windtrap_cov_________fixture_entries___ml, with ___windtrap_post_visit___:
    0: 159-160
    1: 311-322
    2: 298-337
    3: 428-439
    4: 415-472
    5: 679-680
    6: 675-680
    7: 786-787
    8: 791-792
    9: 941-942
    10: 1095-1096
    11: 1100-1105
    12: 1241-1257
    13: 1213-1237
    14: 1213-1237
    15: 1200-1257
  let widen (x : [ `A ]) = (___windtrap_visit___ 0; x :> [ `A  | `B ])
  type empty = |
  let refute (x : (int, empty) Either.t) =
    ___windtrap_visit___ 2;
    (match x with | Left n -> (___windtrap_visit___ 1; n) | Right _ -> .)
  let quiet n =
    ___windtrap_visit___ 4;
    (match n with
     | 0 -> (___windtrap_visit___ 3; "zero")
     | _ -> (("other")[@coverage off]))
  let nothing = lazy (None :> int option)
  let both x y = ___windtrap_visit___ 6; x & ((___windtrap_visit___ 5; y))
  let either x y =
    ___windtrap_visit___ 7;
    if x
    then (___windtrap_visit___ 7; true)
    else if y then (___windtrap_visit___ 8; true) else false
  let ask x (o : < ok: bool   > ) =
    ___windtrap_visit___ 9; if x then (___windtrap_visit___ 9; true) else o#ok
  let either_not x y =
    ___windtrap_visit___ 10;
    if x
    then (___windtrap_visit___ 10; true)
    else if not y then (___windtrap_visit___ 11; true) else false
  let warn flag =
    ___windtrap_visit___ 15;
    if flag
    then
      (___windtrap_visit___ 14;
       ___windtrap_post_visit___ 13 (print_string "watch out"));
    ___windtrap_visit___ 12;
    print_newline ()

  $ cov --impl ./fixture_forms.ml | ../elide.exe
  coverage points of "./fixture_forms.ml" in Windtrap_cov_________fixture_forms___ml, with ___windtrap_post_visit___:
    0: 289-292
    1: 294-297
    2: 288-298
    3: 320-325
    4: 317-325
    5: 356-361
    6: 343-363
    7: 380-385
    8: 380-387
    9: 414-419
    10: 407-419
    11: 439-442
    12: 436-445
    13: 515-518
    14: 588-593
    15: 466-594
    16: 620-679
    17: 752-755
    18: 747-755
    19: 782-785
    20: 776-787
    21: 839-842
    22: 815-848
    23: 931-934
    24: 1074-1088
    25: 1069-1089
    26: 1059-1089
  type r = {
    a: int ;
    mutable b: int }
  let tuple f x =
    ___windtrap_visit___ 2;
    ((___windtrap_post_visit___ 0 (f x)), (___windtrap_post_visit___ 1 (f x)))
  let variant f x =
    ___windtrap_visit___ 4; `V (___windtrap_post_visit___ 3 (f x))
  let record f r =
    ___windtrap_visit___ 6;
    { r with a = (___windtrap_post_visit___ 5 (f r.a)) }
  let field f r = ___windtrap_visit___ 8; (___windtrap_post_visit___ 7 (f r)).a
  let setfield f r =
    ___windtrap_visit___ 10; r.b <- (___windtrap_post_visit___ 9 (f r.b))
  let array f x =
    ___windtrap_visit___ 12; [|(___windtrap_post_visit___ 11 (f x))|]
  let scopes f x =
    ___windtrap_visit___ 15;
    ignore
      (let module M = struct let y = ___windtrap_post_visit___ 13 (f x) end in
         let exception E of int  in
           let open M in E (___windtrap_post_visit___ 14 (f y)))
  let tail_scopes f x =
    ___windtrap_visit___ 16;
    (let module M = struct  end in let exception E  in let open M in f x)
  class counter =
    object
      val mutable v = 0
      method set f =
        ___windtrap_visit___ 18; v <- ___windtrap_post_visit___ 17 (f v)
      method copy f =
        ___windtrap_visit___ 20; {<v = ___windtrap_post_visit___ 19 (f v)>}
    end
  let immediate f =
    ___windtrap_visit___ 22;
    object method get = (___windtrap_visit___ 21; f 1) end
  module type S  = sig val v : int end
  let pack f = ((module
    struct let v = ___windtrap_post_visit___ 23 (f 1) end) : (module S))
  ;;___windtrap_post_visit___ 26
      (print_int
         (___windtrap_post_visit___ 25
            (fst (___windtrap_post_visit___ 24 (tuple succ 1)))))
  [@@@coverage off]
  ;;print_int (fst (tuple succ 1))
  class dark = object method get = succ 1 end
  [@@@coverage on]

  $ cov --impl ./fixture_fun.ml | ../elide.exe
  coverage points of "./fixture_fun.ml" in Windtrap_cov_________fixture_fun___ml, with ___windtrap_post_visit___:
    0: 202-207
    1: 240-245
    2: 270-275
    3: 304-309
    4: 337-348
    5: 351-365
    6: 389-395
    7: 398-408
    8: 431-447
    9: 431-467
    10: 745-764
    11: 745-764
    12: 771-772
    13: 804-806
    14: 794-806
    15: 813-814
  let add a b = ___windtrap_visit___ 0; a + b
  let curried a = fun b -> ___windtrap_visit___ 1; a + b
  let annotated x : int= ___windtrap_visit___ 2; x + 1
  let constrained x = (___windtrap_visit___ 3; x + 1 : int)
  let arms =
    function
    | 0 -> (___windtrap_visit___ 4; "zero")
    | _ -> (___windtrap_visit___ 5; "nonzero")
  let mixed x =
    function
    | 0 -> (___windtrap_visit___ 6; x)
    | n -> (___windtrap_visit___ 7; n + x)
  let sequenced () =
    ___windtrap_visit___ 9;
    ___windtrap_post_visit___ 8 (print_string "a");
    print_string "b"
  let defaulted ?(x=
    ___windtrap_visit___ 11; ___windtrap_post_visit___ 10 (String.length "abc"))
    () = ___windtrap_visit___ 12; x
  let default_fn ?(l=
    ___windtrap_visit___ 14; (fun () -> ___windtrap_visit___ 13; ())) () =
    ___windtrap_visit___ 15; l
