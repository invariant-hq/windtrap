The coverage rewriter's expansions and refusals, as pp.exe prints them.
pp.exe is a standalone ppxlib driver linking every rewriter, and -apply
names the ones a run applies. The fixtures are read, never compiled.

  $ export OCAML_COLOR=never
  $ cov() { ../pp.exe -apply windtrap_coverage "$@"; }

An instrumented file opens with a module that registers its points,
shown here in full:

  $ cov --impl ./fixture_pipeline.ml
  [@@@ocaml.text "/*"]
  module Windtrap_cov_________fixture_pipeline___ml =
    struct
      let ___windtrap_visit___ =
        let counts = Array.make 5 0 in
        Windtrap_runtime.Coverage.register ~file:"./fixture_pipeline.ml"
          ~points:[|{
                      Windtrap_runtime.Coverage.start_ofs = 294;
                      Windtrap_runtime.Coverage.end_ofs = 299
                    };{
                        Windtrap_runtime.Coverage.start_ofs = 315;
                        Windtrap_runtime.Coverage.end_ofs = 326
                      };{
                          Windtrap_runtime.Coverage.start_ofs = 315;
                          Windtrap_runtime.Coverage.end_ofs = 336
                        };{
                            Windtrap_runtime.Coverage.start_ofs = 362;
                            Windtrap_runtime.Coverage.end_ofs = 373
                          };{
                              Windtrap_runtime.Coverage.start_ofs = 354;
                              Windtrap_runtime.Coverage.end_ofs = 384
                            }|] ~counts;
        (fun index -> Windtrap_runtime.Coverage.visit counts index)
      let ___windtrap_post_visit___ point_index result =
        ___windtrap_visit___ point_index; result
    end
  open Windtrap_cov_________fixture_pipeline___ml
  [@@@ocaml.text "/*"]
  let double x = ___windtrap_visit___ 0; x * 2
  let staged x =
    ___windtrap_visit___ 2;
    (___windtrap_post_visit___ 1 (x |> double)) |> double
  let bound x =
    ___windtrap_visit___ 4;
    (let y = ___windtrap_post_visit___ 3 (x |> double) in y + 1)

Every other expansion shows that module condensed by elide.exe to its
file and its points, each a byte extent of the source. elide.exe copies a
module of any other shape as it is.

  $ cov --impl ./fixture_all_off.ml | ../elide.exe
  let sign n = if n > 0 then 1 else 0[@@coverage off]
  let arms = ((function | 0 -> "zero" | _ -> "other")[@coverage off])
  module Off = struct let loop n = for _ = 1 to n do print_newline () done end
  [@@coverage off]
  [@@@coverage off]
  let rest n = match n with | 0 -> succ n | _ -> n

  $ cov --impl ./fixture_and_or.ml | ../elide.exe
  coverage points of "./fixture_and_or.ml" in Windtrap_cov_________fixture_and_or___ml, with ___windtrap_post_visit___:
    0: 479-480
    1: 474-480
    2: 498-499
    3: 503-504
    4: 523-524
    5: 528-529
    6: 533-534
    7: 590-593
    8: 590-593
    9: 563-574
  let both x y = ___windtrap_visit___ 1; x && ((___windtrap_visit___ 0; y))
  let either x y =
    ___windtrap_visit___ 2;
    if x
    then (___windtrap_visit___ 2; true)
    else if y then (___windtrap_visit___ 3; true) else false
  let chain a b c =
    ___windtrap_visit___ 4;
    if a
    then (___windtrap_visit___ 4; true)
    else
      if b
      then (___windtrap_visit___ 5; true)
      else if c then (___windtrap_visit___ 6; true) else false
  let rec search p =
    function
    | [] -> (___windtrap_visit___ 9; false)
    | x::rest ->
        (___windtrap_visit___ 8;
         if ___windtrap_post_visit___ 8 (p x)
         then (___windtrap_visit___ 7; true)
         else search p rest)

  $ cov --impl ./fixture_apply.ml | ../elide.exe
  coverage points of "./fixture_apply.ml" in Windtrap_cov_________fixture_apply___ml, with ___windtrap_post_visit___:
    0: 432-437
    1: 537-547
    2: 530-547
    3: 638-646
    4: 630-657
    5: 749-757
    6: 739-757
    7: 874-890
    8: 854-870
    9: 854-910
  let helper x = ___windtrap_visit___ 0; x + 1
  let nested x =
    ___windtrap_visit___ 2; helper (___windtrap_post_visit___ 1 (helper x))
  let bound () =
    ___windtrap_visit___ 4;
    (let r = ___windtrap_post_visit___ 3 (helper 1) in r + 1)
  let at_op x =
    ___windtrap_visit___ 6; helper @@ (___windtrap_post_visit___ 5 (helper x))
  let sequenced () =
    ___windtrap_visit___ 9;
    ___windtrap_post_visit___ 8 (print_string "a");
    ___windtrap_post_visit___ 7 (print_string "b");
    print_newline ()

  $ cov --impl ./fixture_class.ml | ../elide.exe
  coverage points of "./fixture_class.ml" in Windtrap_cov_________fixture_class___ml, with ___windtrap_post_visit___:
    0: 378-391
    1: 411-412
    2: 429-435
    3: 321-322
    4: 466-480
    5: 486-492
    6: 458-503
    7: 584-591
  class counter ?(step= ___windtrap_visit___ 3; 1)  () =
    object
      val mutable n = 0
      method bump = ___windtrap_visit___ 0; n <- n + step
      method value = ___windtrap_visit___ 1; n
      initializer ___windtrap_visit___ 2; n <- 0
    end
  let use () =
    ___windtrap_visit___ 6;
    (let c = ___windtrap_post_visit___ 4 ((new counter) ()) in
     ___windtrap_post_visit___ 5 c#bump; c#value)
  class virtual shape =
    object
      method virtual  area : int
      method name = ___windtrap_visit___ 7; "shape"
    end

  $ cov --impl ./fixture_empty.ml | ../elide.exe
  type t =
    | A 
    | B 
  let x = 1
  let s = "hello"

  $ cov --impl ./fixture_entries.ml | ../elide.exe
  coverage points of "./fixture_entries.ml" in Windtrap_cov_________fixture_entries___ml, with ___windtrap_post_visit___:
    0: 271-272
    1: 439-450
    2: 426-465
    3: 569-580
    4: 556-613
    5: 852-853
    6: 848-853
    7: 975-976
    8: 980-981
    9: 1146-1147
    10: 1316-1317
    11: 1321-1326
    12: 1481-1497
    13: 1453-1477
    14: 1453-1477
    15: 1440-1497
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

  $ cov --impl ./fixture_exclude.ml | ../elide.exe
  [@@@coverage exclude_file]
  let f n = if n > 0 then 1 else 0

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

  $ cov --impl ./fixture_if_loops.ml | ../elide.exe
  coverage points of "./fixture_if_loops.ml" in Windtrap_cov_________fixture_if_loops___ml, with ___windtrap_post_visit___:
    0: 184-185
    1: 176-178
    2: 162-185
    3: 155-156
    4: 141-185
    5: 215-240
    6: 202-240
    7: 300-306
    8: 300-306
    9: 261-319
    10: 380-399
    11: 335-416
  let sign n =
    ___windtrap_visit___ 4;
    if n > 0
    then (___windtrap_visit___ 3; 1)
    else
      (___windtrap_visit___ 2;
       if n < 0
       then (___windtrap_visit___ 1; (-1))
       else (___windtrap_visit___ 0; 0))
  let warn flag =
    ___windtrap_visit___ 6;
    if flag then (___windtrap_visit___ 5; print_endline "watch out")
  let count_up n =
    ___windtrap_visit___ 9;
    (let i = ref 0 in
     while (!i) < n do
       (___windtrap_visit___ 8; ___windtrap_post_visit___ 7 (incr i)) done;
     !i)
  let sum n =
    ___windtrap_visit___ 11;
    (let total = ref 0 in
     for i = 1 to n do (___windtrap_visit___ 10; total := ((!total) + i)) done;
     !total)

  $ cov --impl ./fixture_keys.ml | ../elide.exe
  coverage points of "./fixture_keys.ml" in Windtrap_cov_________fixture_keys___ml, with ___windtrap_post_visit___:
    0: 655-658
    1: 635-671
    2: 711-714
    3: 723-726
    4: 735-740
    5: 691-741
    6: 913-917
    7: 925-931
    8: 907-938
    9: 1270-1276
    10: 1280-1281
    11: 1270-1281
    12: 1304-1312
    13: 1316-1317
    14: 1304-1312
    15: 1304-1317
    16: 1547-1548
    17: 1552-1557
    18: 1539-1558
  let one f h =
    ___windtrap_visit___ 1;
    ignore
      (let a = ___windtrap_post_visit___ 0 (f 1) in
       ___windtrap_post_visit___ 0 (h a))
  let two f g h =
    ___windtrap_visit___ 5;
    ignore
      (let a = ___windtrap_post_visit___ 2 (f 1)
       and b = ___windtrap_post_visit___ 3 (g 2) in
       ___windtrap_post_visit___ 4 (h a b))
  let loop c f x =
    ___windtrap_visit___ 8;
    while ___windtrap_post_visit___ 6 (c ()) do
      (___windtrap_visit___ 7; ___windtrap_post_visit___ 7 (f @@ x)) done
  let bare x f y =
    ___windtrap_visit___ 11;
    if ___windtrap_post_visit___ 9 (x |> f)
    then (___windtrap_visit___ 9; true)
    else if y then (___windtrap_visit___ 10; true) else false
  let applied x f z y =
    ___windtrap_visit___ 15;
    if ___windtrap_post_visit___ 14 (x |> (f z))
    then (___windtrap_visit___ 12; true)
    else if y then (___windtrap_visit___ 13; true) else false
  let sent a (o : < get: bool   > ) =
    ___windtrap_visit___ 18;
    ignore
      (if a
       then (___windtrap_visit___ 16; true)
       else
         if ___windtrap_post_visit___ 17 o#get
         then (___windtrap_visit___ 17; true)
         else false)
  let extended = [%ext if true then succ 1 else 0]
  let attributed = ((0)[@attr if true then succ 1 else 0])
  [%%ext let x = if true then succ 1 else 0]
  type t = int[@@attr if true then succ 1 else 0]

  $ cov --impl ./fixture_lazy.ml | ../elide.exe
  coverage points of "./fixture_lazy.ml" in Windtrap_cov_________fixture_lazy___ml:
    0: 228-235
    1: 327-328
  let computed = lazy (___windtrap_visit___ 0; 1 + 2)
  let const = lazy 42
  let alias = lazy const
  let none = lazy None
  let thunk = lazy (fun x -> ___windtrap_visit___ 1; x)
  let constrained = lazy (42 : int)

  $ cov --impl ./fixture_letop.ml | ../elide.exe
  coverage points of "./fixture_letop.ml" in Windtrap_cov_________fixture_letop___ml:
    0: 144-147
    1: 167-173
    2: 219-224
    3: 203-224
    4: 266-272
  let ( let* ) x f = ___windtrap_visit___ 0; f x
  let ( and* ) a b = ___windtrap_visit___ 1; (a, b)
  let sum =
    let* a = 1
     in ___windtrap_visit___ 3; (let* b = 2
                                  in ___windtrap_visit___ 2; a + b)
  let pair = let* a = 1
             and* b = 2 in ___windtrap_visit___ 4; (a, b)

  $ cov --impl ./fixture_match.ml | ../elide.exe
  coverage points of "./fixture_match.ml" in Windtrap_cov_________fixture_match___ml, with ___windtrap_post_visit___:
    0: 150-161
    1: 173-178
    2: 166-192
    3: 204-209
    4: 197-223
    5: 133-245
    6: 284-304
    7: 269-278
    8: 265-304
  let classify n =
    ___windtrap_visit___ 5;
    (match n with
     | 0 -> (___windtrap_visit___ 0; "zero")
     | n when ___windtrap_visit___ 1; n > 0 ->
         (___windtrap_visit___ 2; "positive")
     | n when ___windtrap_visit___ 3; n < 0 ->
         (___windtrap_visit___ 4; "negative")
     | _ -> assert false)
  let safe_head l =
    ___windtrap_visit___ 8;
    (try ___windtrap_post_visit___ 7 (List.hd l)
     with | Failure _ -> (___windtrap_visit___ 6; "empty"))

  $ cov --impl ./fixture_off.ml | ../elide.exe
  coverage points of "./fixture_off.ml" in Windtrap_cov_________fixture_off___ml:
    0: 270-273
    1: 261-264
    2: 247-273
    3: 943-951
    4: 954-962
    5: 930-962
  let visible n =
    ___windtrap_visit___ 2;
    if n > 0
    then (___windtrap_visit___ 1; "p")
    else (___windtrap_visit___ 0; "n")
  let hidden n = ((if n > 0 then "p" else "n")[@coverage off])
  let skipped n = if n > 0 then "p" else "n"[@@coverage off]
  module Dark_module = struct let inside n = if n > 0 then "p" else "n" end
  [@@coverage off]
  [@@@coverage off]
  let dark n = match n with | 0 -> "z" | _ -> "x"
  module type T  = sig val v : int end
  module Packed = (val
    if Sys.word_size = 64
    then ((module struct let v = 1 end) : (module T))
    else ((module struct let v = 2 end) : (module T)))
  [@@@coverage on]
  let light n =
    ___windtrap_visit___ 5;
    (match n with
     | 0 -> (___windtrap_visit___ 3; "z")
     | _ -> (___windtrap_visit___ 4; "x"))

  $ cov --impl ./fixture_off_structure.ml | ../elide.exe
  coverage points of "./fixture_off_structure.ml" in Windtrap_cov_________fixture_off_structure___ml, with ___windtrap_post_visit___:
    0: 627-628
    1: 620-621
    2: 610-628
    3: 678-679
    4: 671-672
    5: 661-679
    6: 704-707
    7: 710-713
    8: 600-713
    9: 1110-1111
    10: 1103-1104
    11: 1089-1111
    12: 1211-1212
    13: 1204-1205
    14: 1190-1212
    15: 1405-1406
    16: 1398-1399
    17: 1384-1406
    18: 1507-1508
    19: 1500-1501
    20: 1486-1508
  module rec Dark:sig val f : int -> int end =
    struct let f n = if n > 0 then 1 else 0 end[@@coverage off]
  
  let local n =
    ___windtrap_visit___ 8;
    (let g x =
       ___windtrap_visit___ 2;
       if x then (___windtrap_visit___ 1; 1) else (___windtrap_visit___ 0; 0)
       [@@coverage off] in
     let h x =
       ___windtrap_visit___ 5;
       if x then (___windtrap_visit___ 4; 1) else (___windtrap_visit___ 3; 0)
       [@@coverage bogus] in
     (___windtrap_post_visit___ 6 (g n)) + (___windtrap_post_visit___ 7 (h n)))
  type t = int[@@coverage bogus]
  [@@@coverage off]
  module Inherits =
    struct
      let dark n = if n > 0 then 1 else 0
      [@@@coverage on]
      let lit_inside n =
        ___windtrap_visit___ 11;
        if n > 0
        then (___windtrap_visit___ 10; 1)
        else (___windtrap_visit___ 9; 0)
    end
  let still_dark n = if n > 0 then 1 else 0
  [@@@coverage on]
  let lit n =
    ___windtrap_visit___ 14;
    if n > 0
    then (___windtrap_visit___ 13; 1)
    else (___windtrap_visit___ 12; 0)
  module Unclosed =
    struct
      let lit_before n =
        ___windtrap_visit___ 17;
        if n > 0
        then (___windtrap_visit___ 16; 1)
        else (___windtrap_visit___ 15; 0)
      [@@@coverage off]
      let dark n = if n > 0 then 1 else 0
    end
  let after n =
    ___windtrap_visit___ 20;
    if n > 0
    then (___windtrap_visit___ 19; 1)
    else (___windtrap_visit___ 18; 0)
  let excluded = ((fun x -> ((x)[@coverage on]))[@coverage off])
  let excluded_binding x = ((x)[@coverage bogus])[@@coverage off]
  [@@@coverage off]
  let in_region x = ((x)[@coverage exclude_file])
  [@@@coverage on]

  $ cov --impl ./fixture_or_tail_branch.ml | ../elide.exe
  coverage points of "./fixture_or_tail_branch.ml" in Windtrap_cov_________fixture_or_tail_branch___ml, with ___windtrap_post_visit___:
    0: 796-801
    1: 818-839
    2: 796-839
    3: 858-863
    4: 900-905
    5: 881-894
    6: 858-905
    7: 1214-1219
    8: 1247-1274
    9: 1227-1241
    10: 1214-1274
  let rec or_match n =
    ___windtrap_visit___ 2;
    if n = 0
    then (___windtrap_visit___ 0; true)
    else (match n with | k -> (___windtrap_visit___ 1; or_match (k - 1)))
  let rec or_if n =
    ___windtrap_visit___ 6;
    if n = 0
    then (___windtrap_visit___ 3; true)
    else
      if n > 0
      then (___windtrap_visit___ 5; or_if (n - 1))
      else (___windtrap_visit___ 4; false)
  let rec or_try n =
    ___windtrap_visit___ 10;
    if n = 0
    then (___windtrap_visit___ 7; true)
    else
      (try ___windtrap_post_visit___ 9 (or_try (n - 1))
       with | Not_found -> (___windtrap_visit___ 8; or_try (n - 2)))

  $ cov --impl ./fixture_or_tail_scope.ml | ../elide.exe
  coverage points of "./fixture_or_tail_scope.ml" in Windtrap_cov_________fixture_or_tail_scope___ml:
    0: 516-521
    1: 516-562
    2: 586-591
    3: 586-635
    4: 664-669
    5: 664-724
    6: 756-761
    7: 756-813
    8: 834-837
    9: 862-867
    10: 895-905
    11: 862-905
  let rec or_let n =
    ___windtrap_visit___ 1;
    if n = 0
    then (___windtrap_visit___ 0; true)
    else (let next = n - 1 in or_let next)
  let rec or_open n =
    ___windtrap_visit___ 3;
    if n = 0
    then (___windtrap_visit___ 2; true)
    else (let open Stdlib in or_open (n - 1))
  let rec or_letmodule n =
    ___windtrap_visit___ 5;
    if n = 0
    then (___windtrap_visit___ 4; true)
    else (let module M = Stdlib in or_letmodule (n - 1))
  let rec or_letexception n =
    ___windtrap_visit___ 7;
    if n = 0
    then (___windtrap_visit___ 6; true)
    else (let exception E  in or_letexception (n - 1))
  let ( let* ) x f = ___windtrap_visit___ 8; f x
  let rec or_letop n =
    ___windtrap_visit___ 11;
    if n = 0
    then (___windtrap_visit___ 9; true)
    else (let* m = n - 1
           in ___windtrap_visit___ 10; or_letop m)

  $ cov --impl ./fixture_or_tail_wrap.ml | ../elide.exe
  coverage points of "./fixture_or_tail_wrap.ml" in Windtrap_cov_________fixture_or_tail_wrap___ml:
    0: 414-419
    1: 414-456
    2: 484-489
    3: 484-523
    4: 546-551
    5: 546-582
  let rec or_seq n =
    ___windtrap_visit___ 1;
    if n = 0
    then (___windtrap_visit___ 0; true)
    else (ignore n; or_seq (n - 1))
  let rec or_constraint n =
    ___windtrap_visit___ 3;
    if n = 0
    then (___windtrap_visit___ 2; true)
    else (or_constraint (n - 1) : bool)
  let rec or_coerce n =
    ___windtrap_visit___ 5;
    if n = 0
    then (___windtrap_visit___ 4; true)
    else (or_coerce (n - 1) :> bool)

  $ cov --impl ./fixture_out_edges.ml | ../elide.exe
  coverage points of "./fixture_out_edges.ml" in Windtrap_cov_________fixture_out_edges___ml, with ___windtrap_post_visit___:
    0: 299-300
    1: 382-393
    2: 493-504
    3: 485-515
    4: 647-661
    5: 647-661
    6: 686-700
    7: 686-705
    8: 860-876
    9: 852-883
    10: 902-922
    11: 1030-1037
    12: 1040-1051
    13: 1008-1051
    14: 1160-1161
    15: 1153-1154
    16: 1132-1161
    17: 1336-1342
    18: 1313-1342
    19: 1305-1349
    20: 1437-1455
    21: 1429-1462
    22: 1611-1614
    23: 1641-1659
    24: 1633-1666
    25: 1688-1706
    26: 1875-1882
    27: 1867-1889
  class counter = object method get = ___windtrap_visit___ 0; 0 end
  let make () = ___windtrap_visit___ 1; new counter
  let kept () =
    ___windtrap_visit___ 3;
    (let c = ___windtrap_post_visit___ 2 (new counter) in c#get)
  let checked x =
    ___windtrap_visit___ 5; ___windtrap_post_visit___ 4 (assert (x > 0))
  let checked_then x =
    ___windtrap_visit___ 7; ___windtrap_post_visit___ 6 (assert (x > 0)); x
  let qualified a b =
    ___windtrap_visit___ 9;
    (let r = ___windtrap_post_visit___ 8 (Stdlib.(+) a b) in r)
  let bare a b = ___windtrap_visit___ 10; (let r = a + b in r)
  let scrutinee l =
    ___windtrap_visit___ 13;
    (match List.rev l with
     | [] -> (___windtrap_visit___ 11; 0)
     | x::_ -> (___windtrap_visit___ 12; x))
  let condition l =
    ___windtrap_visit___ 16;
    if List.mem 0 l
    then (___windtrap_visit___ 15; 1)
    else (___windtrap_visit___ 14; 2)
  let at x =
    ___windtrap_visit___ 19;
    (let r =
       ___windtrap_post_visit___ 18
         ((Printf.sprintf "%d") @@ (___windtrap_post_visit___ 17 (succ x))) in
     r)
  let piped l =
    ___windtrap_visit___ 21;
    (let r = ___windtrap_post_visit___ 20 (l |> (List.map succ)) in r)
  let (|.) x f = ___windtrap_visit___ 22; f x
  let dotted l =
    ___windtrap_visit___ 24;
    (let r = ___windtrap_post_visit___ 23 (l |. (List.map succ)) in r)
  let dotted_tail l = ___windtrap_visit___ 25; l |. (List.map succ)
  let callee (o : < get: int -> int   > ) x =
    ___windtrap_visit___ 27;
    (let r = ___windtrap_post_visit___ 26 (o#get x) in r)

  $ cov --impl ./fixture_primitives.ml | ../elide.exe
  coverage points of "./fixture_primitives.ml" in Windtrap_cov_________fixture_primitives___ml, with ___windtrap_post_visit___:
    0: 320-321
    1: 339-340
    2: 1110-1113
    3: 307-1121
  let primitives a b x y r l s e f =
    ___windtrap_visit___ 3;
    (let _ = a && (___windtrap_visit___ 0; b) in
     let _ = a & (___windtrap_visit___ 1; b) in
     let _ = not a in
     let _ = x = y in
     let _ = x <> y in
     let _ = x < y in
     let _ = x <= y in
     let _ = x > y in
     let _ = x >= y in
     let _ = x == y in
     let _ = x != y in
     let _ = ref x in
     let _ = !r in
     let _ = r := x in
     let _ = l @ l in
     let _ = s ^ s in
     let _ = x + y in
     let _ = x - y in
     let _ = x * y in
     let _ = x / y in
     let _ = 1. +. 2. in
     let _ = 1. -. 2. in
     let _ = 1. *. 2. in
     let _ = 1. /. 2. in
     let _ = x mod y in
     let _ = x land y in
     let _ = x lor y in
     let _ = x lxor y in
     let _ = x lsl y in
     let _ = x lsr y in
     let _ = x asr y in
     let _ = raise e in
     let _ = raise_notrace e in
     let _ = failwith s in
     let _ = ignore x in
     let _ = Sys.opaque_identity x in
     let _ = Obj.magic x in
     let _ = x ## y in let _ = ___windtrap_post_visit___ 2 (f x) in ())

  $ cov --impl ./fixture_scope.ml | ../elide.exe
  coverage points of "./fixture_scope.ml" in Windtrap_cov_________fixture_scope___ml:
    0: 679-684
    1: 703-723
    2: 742-767
    3: 793-814
    4: 834-841
    5: 857-869
  let top_level = 1
  let greeting = "hello"
  let add a b = ___windtrap_visit___ 0; a + b
  let negate b = ___windtrap_visit___ 1; (let r = not b in r)
  let vanish x = ___windtrap_visit___ 2; (let () = ignore x in ())
  let labeled_only ~f = ___windtrap_visit___ 3; (let r = f ~x:1 in r)
  let tail_call x = ___windtrap_visit___ 4; add x 1
  let never () = ___windtrap_visit___ 5; assert false

  $ cov --impl ./fixture_tmc.ml | ../elide.exe
  coverage points of "./fixture_tmc.ml" in Windtrap_cov_________fixture_tmc___ml, with ___windtrap_post_visit___:
    0: 395-403
    1: 408-438
    2: 492-500
    3: 505-540
    4: 602-610
    5: 617-647
    6: 558-659
    7: 700-701
    8: 833-841
    9: 795-809
    10: 789-861
    11: 779-781
    12: 765-861
    13: 914-917
    14: 921-933
    15: 890-898
  let rec map f =
    function
    | [] -> (___windtrap_visit___ 0; [])
    | x::rest -> (___windtrap_visit___ 1; (f x) :: (map f rest))[@@tail_mod_cons
                                                                  ]
  let rec double =
    function
    | [] -> (___windtrap_visit___ 2; [])
    | x::rest -> (___windtrap_visit___ 3; (x * 2) :: (double rest))[@@ocaml.tail_mod_cons
                                                                     ]
  let local l =
    ___windtrap_visit___ 6;
    (let rec go =
       function
       | [] -> (___windtrap_visit___ 4; [])
       | x::rest -> (___windtrap_visit___ 5; (succ x) :: (go rest))[@@tail_mod_cons
                                                                     ] in
     go l)
  class cell = object method get = ___windtrap_visit___ 7; 0 end
  let rec cells (o : < get: int   > ) n =
    ___windtrap_visit___ 12;
    if n = 0
    then (___windtrap_visit___ 11; [])
    else
      (___windtrap_visit___ 10;
       ___windtrap_post_visit___ 9 (assert (n > 0));
       ignore o#get;
       (___windtrap_post_visit___ 8 (new cell))
       ::
       (cells o (n - 1)))[@@tail_mod_cons ]
  let rec plain f =
    function
    | [] -> (___windtrap_visit___ 15; [])
    | x::rest ->
        (___windtrap_visit___ 13;
         (___windtrap_post_visit___ 13 (f x))
         ::
         (___windtrap_post_visit___ 14 (plain f rest)))

Over code a deriver generated, for which generated.ml stands in:

  $ ../pp.exe -apply windtrap_test_generated,windtrap_coverage --impl ./fixture_generated.ml | ../elide.exe
  coverage points of "./fixture_generated.ml" in Windtrap_cov_________fixture_generated___ml, with ___windtrap_post_visit___:
    0: 461-491
    1: 515-523
    2: 508-523
    3: 782-783
    4: 810-811
    5: 840-855
    6: 858-867
  let entry x = ((print_int x)[@generated ])
  let edge x = ___windtrap_visit___ 0; ignore (((succ)[@generated ]) x)
  let written x =
    ___windtrap_visit___ 2; ignore (___windtrap_post_visit___ 1 (succ x))
  let arms =
    function
    | ((Some x)[@generated ]) -> (___windtrap_visit___ 3; x)
    | ((None)[@after_body ]) -> (___windtrap_visit___ 4; 0)
  let written_arms =
    function
    | Some n -> (___windtrap_visit___ 5; n + 1)
    | None -> (___windtrap_visit___ 6; 0)

Over a file the mutation rewriter ran on first, the order of the driver
dune builds for a stanza naming both backends:

  $ ../pp.exe -apply windtrap_mutate,windtrap_coverage --impl ./fixture_guards.ml | ../elide.exe
  coverage points of "./fixture_guards.ml" in Windtrap_cov_________fixture_guards___ml, with ___windtrap_post_visit___:
    0: 492-497
    1: 510-511
    2: 503-504
    3: 489-511
    4: 526-531
    5: 782-783
    6: 715-734
    7: 739-765
    8: 770-792
    9: 797-809
    10: 698-809
    11: 843-844
    12: 850-855
    13: 828-855
    14: 995-1003
    15: 995-1003
    16: 978-1010
    17: 1044-1047
    18: 1029-1048
    19: 1179-1180
  mutation sites of "./fixture_guards.ml" in Windtrap_mut_________fixture_guards___ml, with type 'a operands:
    0: 9:16 "le" "a < b" -> "a <= b"
    1: 10:14 "sub" "a + b" -> "a - b"
    2: 16:11 "not" "p x" -> "not (p x)"
    3: 17:11 "neq" "a = b" -> "a <> b"
    4: 18:11 "or" "a && b" -> "a || b"
    5: 21:27 "or" "a && b" -> "a || b"
    6: 21:20 "not" "c" -> "not c"
    7: 26:8 "not" "f x" -> "not (f x)"
    8: 30:24 "or" "(f x) && (g x)" -> "(f x) || (g x)"
    9: 34:17 "and" "a || b" -> "a && b"
  let lt a b =
    ___windtrap_visit___ 3;
    if
      (let (__windtrap_mut_0_l, __windtrap_mut_0_r) =
         ((a, b) : _ Windtrap_mut_________fixture_guards___ml.operands) in
       if Windtrap_mut_________fixture_guards___ml.___windtrap_armed___ 0
       then Stdlib.not (__windtrap_mut_0_r < __windtrap_mut_0_l)
       else (___windtrap_visit___ 0; __windtrap_mut_0_l < __windtrap_mut_0_r))
    then (___windtrap_visit___ 2; 1)
    else (___windtrap_visit___ 1; 0)
  let add a b =
    let (__windtrap_mut_1_l, __windtrap_mut_1_r) = (a, b) in
    if Windtrap_mut_________fixture_guards___ml.___windtrap_armed___ 1
    then __windtrap_mut_1_l - __windtrap_mut_1_r
    else (___windtrap_visit___ 4; __windtrap_mut_1_l + __windtrap_mut_1_r)
  let arms x p a b =
    ___windtrap_visit___ 10;
    (match x with
     | 0 when
         let __windtrap_mut_2_p = p x in
         if Windtrap_mut_________fixture_guards___ml.___windtrap_armed___ 2
         then Stdlib.not __windtrap_mut_2_p
         else __windtrap_mut_2_p -> (___windtrap_visit___ 6; "neg")
     | 1 when
         let __windtrap_mut_3_p = a = b in
         if Windtrap_mut_________fixture_guards___ml.___windtrap_armed___ 3
         then Stdlib.not __windtrap_mut_3_p
         else __windtrap_mut_3_p -> (___windtrap_visit___ 7; "equality")
     | 2 when
         let __windtrap_mut_4_p = a in
         if
           Stdlib.(<>) (__windtrap_mut_4_p : Stdlib.Bool.t)
             (Windtrap_mut_________fixture_guards___ml.___windtrap_armed___ 4)
         then (___windtrap_visit___ 5; b)
         else __windtrap_mut_4_p -> (___windtrap_visit___ 8; "con")
     | _ -> (___windtrap_visit___ 9; "other"))
  let both c a b =
    ___windtrap_visit___ 13;
    if
      (let __windtrap_mut_6_p = c in
       if Windtrap_mut_________fixture_guards___ml.___windtrap_armed___ 6
       then Stdlib.not __windtrap_mut_6_p
       else __windtrap_mut_6_p)
    then
      (let __windtrap_mut_5_p = a in
       if
         Stdlib.(<>) (__windtrap_mut_5_p : Stdlib.Bool.t)
           (Windtrap_mut_________fixture_guards___ml.___windtrap_armed___ 5)
       then (___windtrap_visit___ 11; b)
       else __windtrap_mut_5_p)
    else (___windtrap_visit___ 12; false)
  let rec loop f x =
    ___windtrap_visit___ 16;
    while
      (let __windtrap_mut_7_p = f x in
       if Windtrap_mut_________fixture_guards___ml.___windtrap_armed___ 7
       then Stdlib.not __windtrap_mut_7_p
       else __windtrap_mut_7_p)
      do (___windtrap_visit___ 15; ___windtrap_post_visit___ 14 (loop f x))
      done
  let left f g x =
    ___windtrap_visit___ 18;
    ignore
      (let __windtrap_mut_8_p = f x in
       if
         Stdlib.(<>) (__windtrap_mut_8_p : Stdlib.Bool.t)
           (Windtrap_mut_________fixture_guards___ml.___windtrap_armed___ 8)
       then (___windtrap_visit___ 17; ___windtrap_post_visit___ 17 (g x))
       else __windtrap_mut_8_p)
  let either a b =
    let __windtrap_mut_9_p = a in
    if
      Stdlib.(=) (__windtrap_mut_9_p : Stdlib.Bool.t)
        (Windtrap_mut_________fixture_guards___ml.___windtrap_armed___ 9)
    then (___windtrap_visit___ 19; b)
    else __windtrap_mut_9_p

A file is instrumented under its own name, and returned as parsed under
an input name that is not a source file's:

  $ cov --impl ./fixture_input_name.ml | ../elide.exe
  coverage points of "./fixture_input_name.ml" in Windtrap_cov_________fixture_input_name___ml:
    0: 295-296
    1: 288-289
    2: 274-296
  let sign n =
    ___windtrap_visit___ 2;
    if n > 0 then (___windtrap_visit___ 1; 1) else (___windtrap_visit___ 0; 0)

  $ for name in //toplevel// '(stdin)' lib/.ocamlinit lib/topfind; do
  >   cov -loc-filename "$name" --impl ./fixture_input_name.ml
  > done
  let sign n = if n > 0 then 1 else 0
  let sign n = if n > 0 then 1 else 0
  let sign n = if n > 0 then 1 else 0
  let sign n = if n > 0 then 1 else 0

A refusal is an error located at the attribute, and the driver exits 1:

  $ cov --impl ./reject_bad_payload.ml
  File "./reject_bad_payload.ml", line 1, characters 18-35:
  1 | let f n = (n + 1) [@coverage bogus]
                        ^^^^^^^^^^^^^^^^^
  Error: Bad payload in coverage attribute.
  [1]

  $ cov --impl ./reject_double_off.ml
  File "./reject_double_off.ml", line 5, characters 0-17:
  5 | [@@@coverage off]
      ^^^^^^^^^^^^^^^^^
  Error: Coverage is already off.
  [1]

  $ cov --impl ./reject_empty_payload.ml
  File "./reject_empty_payload.ml", line 1, characters 18-29:
  1 | let f n = (n + 1) [@coverage]
                        ^^^^^^^^^^^
  Error: Bad payload in coverage attribute.
  [1]

  $ cov --impl ./reject_exclude_file_binding.ml
  File "./reject_exclude_file_binding.ml", line 1, characters 16-41:
  1 | let f n = n + 1 [@@coverage exclude_file]
                      ^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: coverage exclude_file is not allowed here.
  [1]

  $ cov --impl ./reject_exclude_file_expr.ml
  File "./reject_exclude_file_expr.ml", line 1, characters 18-42:
  1 | let f n = (n + 1) [@coverage exclude_file]
                        ^^^^^^^^^^^^^^^^^^^^^^^^
  Error: coverage exclude_file is not allowed here.
  [1]

  $ cov --impl ./reject_misplaced_exclude_file.ml
  File "./reject_misplaced_exclude_file.ml", line 2, characters 2-28:
  2 |   [@@@coverage exclude_file]
        ^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: coverage exclude_file is not allowed here.
  [1]

  $ cov --impl ./reject_misplaced_on.ml
  File "./reject_misplaced_on.ml", line 1, characters 18-32:
  1 | let f n = (n + 1) [@coverage on]
                        ^^^^^^^^^^^^^^
  Error: coverage on is not allowed here.
  [1]

  $ cov --impl ./reject_off_reason.ml
  File "./reject_off_reason.ml", line 1, characters 18-42:
  1 | let f n = (n + 1) [@coverage off "reason"]
                        ^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Bad payload in coverage attribute.
  [1]

  $ cov --impl ./reject_on_binding.ml
  File "./reject_on_binding.ml", line 1, characters 16-31:
  1 | let f n = n + 1 [@@coverage on]
                      ^^^^^^^^^^^^^^^
  Error: coverage on is not allowed here.
  [1]

  $ cov --impl ./reject_on_outside.ml
  File "./reject_on_outside.ml", line 3, characters 0-16:
  3 | [@@@coverage on]
      ^^^^^^^^^^^^^^^^
  Error: Coverage is already on.
  [1]

The two instrumenting rewriters read one attribute grammar, each under
its own name. Both accept every legal spelling:

  $ cov --impl ./spellings.ml > /dev/null
  $ sed s/coverage/mutate/g spellings.ml > mutate_spellings.ml
  $ ../pp.exe -apply windtrap_mutate --impl ./mutate_spellings.ml > /dev/null

The mutation rewriter refuses what the coverage rewriter refuses, in the
same words once the names are swapped and the width a name gives a span
is erased. reject_off_reason.ml has no twin: only the mutation rewriter's
off takes a reason.

  $ ns() { sed -E 's/[Cc]overage|[Mm]utate|[Mm]utation/NS/g; s/(characters [0-9]+-)[0-9]+/\1/; s/^( *)\^+$/\1^/'; }
  $ mkdir twin
  $ for f in reject_*.ml; do
  >   test "$f" = reject_off_reason.ml && continue
  >   sed s/coverage/mutate/g "$f" > "twin/$f"
  >   cov --impl "./$f" 2>&1 | ns > coverage.out
  >   (cd twin && ../../pp.exe -apply windtrap_mutate --impl "./$f" 2>&1) | ns > mutate.out
  >   diff coverage.out mutate.out && echo "$f: same"
  > done
  reject_bad_payload.ml: same
  reject_double_off.ml: same
  reject_empty_payload.ml: same
  reject_exclude_file_binding.ml: same
  reject_exclude_file_expr.ml: same
  reject_misplaced_exclude_file.ml: same
  reject_misplaced_on.ml: same
  reject_on_binding.ml: same
  reject_on_outside.ml: same
