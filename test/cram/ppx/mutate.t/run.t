The mutation rewriter's expansions and refusals, as pp.exe prints them
(see ../coverage.t).

  $ export OCAML_COLOR=never
  $ mut() { ../pp.exe -apply windtrap_mutate "$@"; }

A mutated file opens with a module that registers its sites, shown here
in full. The file is mutated under its own name:

  $ mut --impl ./fixture_input_name.ml
  [@@@ocaml.text "/*"]
  module Windtrap_mut_________fixture_input_name___ml =
    struct
      type site = Windtrap_runtime.Mutate.site =
        {
        line: int ;
        col: int ;
        rewrite: string ;
        before: string ;
        after: string ;
        dismissed: string option }
      type 'a operands = ('a * 'a)
      let ___windtrap_armed___ =
        Windtrap_runtime.Mutate.register ~file:"./fixture_input_name.ml"
          ~sites:[|{
                     line = 5;
                     col = 16;
                     rewrite = "ge";
                     before = "n > 0";
                     after = "n >= 0";
                     dismissed = None
                   }|]
    end
  [@@@ocaml.text "/*"]
  let sign n =
    if
      let (__windtrap_mut_0_l, __windtrap_mut_0_r) =
        ((n, 0) : _ Windtrap_mut_________fixture_input_name___ml.operands) in
      (if Windtrap_mut_________fixture_input_name___ml.___windtrap_armed___ 0
       then Stdlib.not (__windtrap_mut_0_r > __windtrap_mut_0_l)
       else __windtrap_mut_0_l > __windtrap_mut_0_r)
    then 1
    else 0

Every other expansion shows that module condensed by elide.exe to its
file and its sites: the line and column, the rewrite, the text before and
after, and the reason of a dismissed site.

  $ mut --impl ./fixture_all_dismissed.ml | ../elide.exe
  mutation sites of "./fixture_all_dismissed.ml" in Windtrap_mut_________fixture_all_dismissed___ml:
    0: 8:5 "ge" "want > 16" -> "want >= 16", dismissed "both arms yield 16 at the boundary"
  let cap want =
    if ((want > 16)[@mutate off "both arms yield 16 at the boundary"])
    then want
    else 16

  $ mut --impl ./fixture_ari.ml | ../elide.exe
  mutation sites of "./fixture_ari.ml" in Windtrap_mut_________fixture_ari___ml:
    0: 5:14 "sub" "a + b" -> "a - b"
    1: 6:15 "add" "a - b" -> "a + b"
    2: 7:15 "fsub" "a +. b" -> "a -. b"
    3: 8:16 "fadd" "a -. b" -> "a +. b"
    4: 13:13 "sub" "1 + 2" -> "1 - 2"
    5: 17:17 "sub" "a + b" -> "a - b"
  let sum a b =
    let (__windtrap_mut_0_l, __windtrap_mut_0_r) = (a, b) in
    if Windtrap_mut_________fixture_ari___ml.___windtrap_armed___ 0
    then __windtrap_mut_0_l - __windtrap_mut_0_r
    else __windtrap_mut_0_l + __windtrap_mut_0_r
  let diff a b =
    let (__windtrap_mut_1_l, __windtrap_mut_1_r) = (a, b) in
    if Windtrap_mut_________fixture_ari___ml.___windtrap_armed___ 1
    then __windtrap_mut_1_l + __windtrap_mut_1_r
    else __windtrap_mut_1_l - __windtrap_mut_1_r
  let fsum a b =
    let (__windtrap_mut_2_l, __windtrap_mut_2_r) = (a, b) in
    if Windtrap_mut_________fixture_ari___ml.___windtrap_armed___ 2
    then __windtrap_mut_2_l -. __windtrap_mut_2_r
    else __windtrap_mut_2_l +. __windtrap_mut_2_r
  let fdiff a b =
    let (__windtrap_mut_3_l, __windtrap_mut_3_r) = (a, b) in
    if Windtrap_mut_________fixture_ari___ml.___windtrap_armed___ 3
    then __windtrap_mut_3_l +. __windtrap_mut_3_r
    else __windtrap_mut_3_l -. __windtrap_mut_3_r
  let origin =
    let (__windtrap_mut_4_l, __windtrap_mut_4_r) = (1, 2) in
    if Windtrap_mut_________fixture_ari___ml.___windtrap_armed___ 4
    then __windtrap_mut_4_l - __windtrap_mut_4_r
    else __windtrap_mut_4_l + __windtrap_mut_4_r
  let negate x = - x
  let scaled a b =
    (let (__windtrap_mut_5_l, __windtrap_mut_5_r) = (a, b) in
     if Windtrap_mut_________fixture_ari___ml.___windtrap_armed___ 5
     then __windtrap_mut_5_l - __windtrap_mut_5_r
     else __windtrap_mut_5_l + __windtrap_mut_5_r) * 2

  $ mut --impl ./fixture_assert.ml | ../elide.exe
  mutation sites of "./fixture_assert.ml" in Windtrap_mut_________fixture_assert___ml, with type 'a operands:
    0: 7:2 "sub" "a + b" -> "a - b"
    1: 17:21 "or" "(a < b) && (assert false)" -> "(a < b) || (assert false)"
    2: 17:21 "le" "a < b" -> "a <= b"
    3: 25:4 "not" "assert (a < b); a > b" -> "not (assert (a < b); a > b)"
  let checked a b =
    assert (a < b);
    (let (__windtrap_mut_0_l, __windtrap_mut_0_r) = (a, b) in
     if Windtrap_mut_________fixture_assert___ml.___windtrap_armed___ 0
     then __windtrap_mut_0_l - __windtrap_mut_0_r
     else __windtrap_mut_0_l + __windtrap_mut_0_r)
  let conjunction a b = assert (a && b)
  let arithmetic a b = assert ((a + b) > 0)
  let as_condition () = if assert false then 1 else 0
  let as_operand a b =
    let __windtrap_mut_1_p =
      let (__windtrap_mut_2_l, __windtrap_mut_2_r) =
        ((a, b) : _ Windtrap_mut_________fixture_assert___ml.operands) in
      if Windtrap_mut_________fixture_assert___ml.___windtrap_armed___ 2
      then Stdlib.not (__windtrap_mut_2_r < __windtrap_mut_2_l)
      else __windtrap_mut_2_l < __windtrap_mut_2_r in
    if
      Stdlib.(<>) (__windtrap_mut_1_p : Stdlib.Bool.t)
        (Windtrap_mut_________fixture_assert___ml.___windtrap_armed___ 1)
    then assert false
    else __windtrap_mut_1_p
  let sequenced a b =
    if
      let __windtrap_mut_3_p = assert (a < b); a > b in
      (if Windtrap_mut_________fixture_assert___ml.___windtrap_armed___ 3
       then Stdlib.not __windtrap_mut_3_p
       else __windtrap_mut_3_p)
    then 1
    else 0

  $ mut --impl ./fixture_chain.ml | ../elide.exe
  mutation sites of "./fixture_chain.ml" in Windtrap_mut_________fixture_chain___ml, with type 'a operands:
    0: 19:18 "sub" "(a + b) + c" -> "(a + b) - c"
    1: 20:18 "add" "(a - b) - c" -> "(a - b) + c"
    2: 21:19 "fsub" "(a +. b) +. c" -> "(a +. b) -. c"
    3: 22:25 "or" "b && c" -> "b || c"
    4: 31:26 "sub" "(a + b) + c" -> "(a + b) - c"
    5: 32:23 "sub" "(a + b) + c" -> "(a + b) - c"
    6: 33:31 "or" "b && c" -> "b || c"
    7: 39:23 "sub" "(a + b) + (c + d)" -> "(a + b) - (c + d)"
    8: 39:31 "sub" "c + d" -> "c - d"
    9: 43:18 "add" "(a + b) - c" -> "(a + b) + c"
    10: 43:18 "sub" "a + b" -> "a - b"
    11: 48:23 "le" "(a < b) < c" -> "(a < b) <= c"
  let three a b c =
    let (__windtrap_mut_0_l, __windtrap_mut_0_r) = ((a + b), c) in
    if Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 0
    then __windtrap_mut_0_l - __windtrap_mut_0_r
    else __windtrap_mut_0_l + __windtrap_mut_0_r
  let minus a b c =
    let (__windtrap_mut_1_l, __windtrap_mut_1_r) = ((a - b), c) in
    if Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 1
    then __windtrap_mut_1_l + __windtrap_mut_1_r
    else __windtrap_mut_1_l - __windtrap_mut_1_r
  let floats a b c =
    let (__windtrap_mut_2_l, __windtrap_mut_2_r) = ((a +. b), c) in
    if Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 2
    then __windtrap_mut_2_l -. __windtrap_mut_2_r
    else __windtrap_mut_2_l +. __windtrap_mut_2_r
  let conn a b c =
    if
      a &&
        (let __windtrap_mut_3_p = b in
         (if
            Stdlib.(<>) (__windtrap_mut_3_p : Stdlib.Bool.t)
              (Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 3)
          then c
          else __windtrap_mut_3_p))
    then 1
    else 0
  let parens a b c =
    ignore
      (let (__windtrap_mut_4_l, __windtrap_mut_4_r) = ((a + b), c) in
       if Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 4
       then __windtrap_mut_4_l - __windtrap_mut_4_r
       else __windtrap_mut_4_l + __windtrap_mut_4_r)
  let in_arg f a b c =
    f
      (let (__windtrap_mut_5_l, __windtrap_mut_5_r) = ((a + b), c) in
       if Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 5
       then __windtrap_mut_5_l - __windtrap_mut_5_r
       else __windtrap_mut_5_l + __windtrap_mut_5_r)
  let conn_arg f a b c =
    f
      (a &&
         (let __windtrap_mut_6_p = b in
          if
            Stdlib.(<>) (__windtrap_mut_6_p : Stdlib.Bool.t)
              (Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 6)
          then c
          else __windtrap_mut_6_p))
  let siblings a b c d =
    let (__windtrap_mut_7_l, __windtrap_mut_7_r) =
      ((a + b),
        (let (__windtrap_mut_8_l, __windtrap_mut_8_r) = (c, d) in
         if Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 8
         then __windtrap_mut_8_l - __windtrap_mut_8_r
         else __windtrap_mut_8_l + __windtrap_mut_8_r)) in
    if Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 7
    then __windtrap_mut_7_l - __windtrap_mut_7_r
    else __windtrap_mut_7_l + __windtrap_mut_7_r
  let mixed a b c =
    let (__windtrap_mut_9_l, __windtrap_mut_9_r) =
      ((let (__windtrap_mut_10_l, __windtrap_mut_10_r) = (a, b) in
        if Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 10
        then __windtrap_mut_10_l - __windtrap_mut_10_r
        else __windtrap_mut_10_l + __windtrap_mut_10_r), c) in
    if Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 9
    then __windtrap_mut_9_l + __windtrap_mut_9_r
    else __windtrap_mut_9_l - __windtrap_mut_9_r
  let chained a b c =
    if
      let (__windtrap_mut_11_l, __windtrap_mut_11_r) =
        (((a < b), c) : _ Windtrap_mut_________fixture_chain___ml.operands) in
      (if Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 11
       then Stdlib.not (__windtrap_mut_11_r < __windtrap_mut_11_l)
       else __windtrap_mut_11_l < __windtrap_mut_11_r)
    then 1
    else 0

  $ mut --impl ./fixture_chain_off.ml | ../elide.exe
  mutation sites of "./fixture_chain_off.ml" in Windtrap_mut_________fixture_chain_off___ml:
    0: 4:19 "sub" "((a + (b - d))[@mutate off]) + c" -> "((a + (b - d))[@mutate off]) - c"
  let link a b c d =
    let (__windtrap_mut_0_l, __windtrap_mut_0_r) =
      (((a + (b - d))[@mutate off]), c) in
    if Windtrap_mut_________fixture_chain_off___ml.___windtrap_armed___ 0
    then __windtrap_mut_0_l - __windtrap_mut_0_r
    else __windtrap_mut_0_l + __windtrap_mut_0_r

  $ mut --impl ./fixture_cmp.ml | ../elide.exe
  mutation sites of "./fixture_cmp.ml" in Windtrap_mut_________fixture_cmp___ml, with type 'a operands:
    0: 7:19 "le" "a < b" -> "a <= b"
    1: 8:21 "lt" "a <= b" -> "a < b"
    2: 11:8 "ge" "a > b" -> "a >= b"
    3: 15:39 "gt" "a >= b" -> "a > b"
    4: 16:18 "neq" "a = b" -> "a <> b"
    5: 17:21 "eq" "a <> b" -> "a = b"
    6: 20:21 "or" "(x >= lo) && (x <= hi)" -> "(x >= lo) || (x <= hi)"
    7: 20:21 "gt" "x >= lo" -> "x > lo"
    8: 20:32 "lt" "x <= hi" -> "x < hi"
  let below a b =
    if
      let (__windtrap_mut_0_l, __windtrap_mut_0_r) =
        ((a, b) : _ Windtrap_mut_________fixture_cmp___ml.operands) in
      (if Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 0
       then Stdlib.not (__windtrap_mut_0_r < __windtrap_mut_0_l)
       else __windtrap_mut_0_l < __windtrap_mut_0_r)
    then 1
    else 0
  let at_most a b =
    if
      let (__windtrap_mut_1_l, __windtrap_mut_1_r) =
        ((a, b) : _ Windtrap_mut_________fixture_cmp___ml.operands) in
      (if Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 1
       then Stdlib.not (__windtrap_mut_1_r <= __windtrap_mut_1_l)
       else __windtrap_mut_1_l <= __windtrap_mut_1_r)
    then 1
    else 0
  let above a b =
    while
      let (__windtrap_mut_2_l, __windtrap_mut_2_r) =
        ((a, b) : _ Windtrap_mut_________fixture_cmp___ml.operands) in
      if Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 2
      then Stdlib.not (__windtrap_mut_2_r > __windtrap_mut_2_l)
      else __windtrap_mut_2_l > __windtrap_mut_2_r do () done
  let at_least a b =
    match a with
    | _ when
        let (__windtrap_mut_3_l, __windtrap_mut_3_r) =
          ((a, b) : _ Windtrap_mut_________fixture_cmp___ml.operands) in
        if Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 3
        then Stdlib.not (__windtrap_mut_3_r >= __windtrap_mut_3_l)
        else __windtrap_mut_3_l >= __windtrap_mut_3_r -> 1
    | _ -> 0
  let same a b =
    if
      let __windtrap_mut_4_p = a = b in
      (if Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 4
       then Stdlib.not __windtrap_mut_4_p
       else __windtrap_mut_4_p)
    then 1
    else 0
  let differs a b =
    if
      let __windtrap_mut_5_p = a <> b in
      (if Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 5
       then Stdlib.not __windtrap_mut_5_p
       else __windtrap_mut_5_p)
    then 1
    else 0
  let window lo hi x =
    let __windtrap_mut_6_p =
      let (__windtrap_mut_7_l, __windtrap_mut_7_r) =
        ((x, lo) : _ Windtrap_mut_________fixture_cmp___ml.operands) in
      if Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 7
      then Stdlib.not (__windtrap_mut_7_r >= __windtrap_mut_7_l)
      else __windtrap_mut_7_l >= __windtrap_mut_7_r in
    if
      Stdlib.(<>) (__windtrap_mut_6_p : Stdlib.Bool.t)
        (Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 6)
    then
      let (__windtrap_mut_8_l, __windtrap_mut_8_r) =
        ((x, hi) : _ Windtrap_mut_________fixture_cmp___ml.operands) in
      (if Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 8
       then Stdlib.not (__windtrap_mut_8_r <= __windtrap_mut_8_l)
       else __windtrap_mut_8_l <= __windtrap_mut_8_r)
    else __windtrap_mut_6_p
  let ok a b = a < b
  let count a b = List.length (List.filter (fun x -> x < a) b)

  $ mut --impl ./fixture_con.ml | ../elide.exe
  mutation sites of "./fixture_con.ml" in Windtrap_mut_________fixture_con___ml:
    0: 8:15 "or" "a && b" -> "a || b"
    1: 9:17 "and" "a || b" -> "a && b"
    2: 10:22 "or" "ready && x" -> "ready || x"
    3: 11:55 "and" "(p x) || (search p rest)" -> "(p x) && (search p rest)"
  let both a b =
    let __windtrap_mut_0_p = a in
    if
      Stdlib.(<>) (__windtrap_mut_0_p : Stdlib.Bool.t)
        (Windtrap_mut_________fixture_con___ml.___windtrap_armed___ 0)
    then b
    else __windtrap_mut_0_p
  let either a b =
    let __windtrap_mut_1_p = a in
    if
      Stdlib.(=) (__windtrap_mut_1_p : Stdlib.Bool.t)
        (Windtrap_mut_________fixture_con___ml.___windtrap_armed___ 1)
    then b
    else __windtrap_mut_1_p
  let gate ready x =
    if
      let __windtrap_mut_2_p = ready in
      (if
         Stdlib.(<>) (__windtrap_mut_2_p : Stdlib.Bool.t)
           (Windtrap_mut_________fixture_con___ml.___windtrap_armed___ 2)
       then x
       else __windtrap_mut_2_p)
    then 1
    else 0
  let rec search p =
    function
    | [] -> false
    | x::rest ->
        let __windtrap_mut_3_p = p x in
        if
          Stdlib.(=) (__windtrap_mut_3_p : Stdlib.Bool.t)
            (Windtrap_mut_________fixture_con___ml.___windtrap_armed___ 3)
        then search p rest
        else __windtrap_mut_3_p

  $ mut --impl ./fixture_contexts.ml | ../elide.exe
  mutation sites of "./fixture_contexts.ml" in Windtrap_mut_________fixture_contexts___ml, with type 'a operands:
    0: 5:19 "and" "(a < b) || c" -> "(a < b) && c"
    1: 5:19 "le" "a < b" -> "a <= b"
    2: 12:4 "not" "let x = a in x < b" -> "not (let x = a in x < b)"
    3: 17:32 "not" "(a < b : bool)" -> "not (a < b : bool)"
    4: 18:25 "not" "not (a < b)" -> "not (not (a < b))"
    5: 23:34 "and" "c || d" -> "c && d"
    6: 23:25 "le" "a < b" -> "a <= b"
    7: 29:27 "not" "a & b" -> "not (a & b)"
    8: 34:33 "not" "Float.(<) a b" -> "not (Float.(<) a b)"
    9: 40:16 "sub" "a + b" -> "a - b"
  let either a b c =
    let __windtrap_mut_0_p =
      let (__windtrap_mut_1_l, __windtrap_mut_1_r) =
        ((a, b) : _ Windtrap_mut_________fixture_contexts___ml.operands) in
      if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 1
      then Stdlib.not (__windtrap_mut_1_r < __windtrap_mut_1_l)
      else __windtrap_mut_1_l < __windtrap_mut_1_r in
    if
      Stdlib.(=) (__windtrap_mut_0_p : Stdlib.Bool.t)
        (Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 0)
    then c
    else __windtrap_mut_0_p
  let through_let a b =
    if
      let __windtrap_mut_2_p = let x = a in x < b in
      (if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 2
       then Stdlib.not __windtrap_mut_2_p
       else __windtrap_mut_2_p)
    then 1
    else 0
  let through_constraint a b =
    if
      let __windtrap_mut_3_p = (a < b : bool) in
      (if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 3
       then Stdlib.not __windtrap_mut_3_p
       else __windtrap_mut_3_p)
    then 1
    else 0
  let through_not a b =
    if
      let __windtrap_mut_4_p = not (a < b) in
      (if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 4
       then Stdlib.not __windtrap_mut_4_p
       else __windtrap_mut_4_p)
    then 1
    else 0
  let skipped a b c d =
    if
      (let (__windtrap_mut_6_l, __windtrap_mut_6_r) =
         ((a, b) : _ Windtrap_mut_________fixture_contexts___ml.operands) in
       if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 6
       then Stdlib.not (__windtrap_mut_6_r < __windtrap_mut_6_l)
       else __windtrap_mut_6_l < __windtrap_mut_6_r) &&
        (let __windtrap_mut_5_p = c in
         (if
            Stdlib.(=) (__windtrap_mut_5_p : Stdlib.Bool.t)
              (Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___
                 5)
          then d
          else __windtrap_mut_5_p))
    then 1
    else 0
  let old_and a b = a & b
  let old_or a b = a or b
  let old_condition a b =
    if
      let __windtrap_mut_7_p = a & b in
      (if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 7
       then Stdlib.not __windtrap_mut_7_p
       else __windtrap_mut_7_p)
    then 1
    else 0
  let qualified a b = Stdlib.(+) a b
  let qualified_condition a b =
    if
      let __windtrap_mut_8_p = Float.(<) a b in
      (if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 8
       then Stdlib.not __windtrap_mut_8_p
       else __windtrap_mut_8_p)
    then 1
    else 0
  let partial = (+) 1
  let labelled a b = (+) ~a b
  let total a b =
    let (__windtrap_mut_9_l, __windtrap_mut_9_r) = (a, b) in
    if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 9
    then __windtrap_mut_9_l - __windtrap_mut_9_r
    else __windtrap_mut_9_l + __windtrap_mut_9_r

  $ mut --impl ./fixture_empty.ml | ../elide.exe
  type t =
    | A 
    | B 
  let x = 1
  let s = "hello"
  let pair a b = (a, b)

  $ mut --impl ./fixture_exclude.ml | ../elide.exe
  [@@@mutate exclude_file]
  let sum a b = a + b
  let ordered a b = if a < b then 1 else 0

  $ mut --impl ./fixture_inline_tests.ml | ../elide.exe
  mutation sites of "./fixture_inline_tests.ml" in Windtrap_mut_________fixture_inline_tests___ml, with type 'a operands:
    0: 6:14 "sub" "a + b" -> "a - b"
    1: 7:21 "le" "a < b" -> "a <= b"
  let sum a b =
    let (__windtrap_mut_0_l, __windtrap_mut_0_r) = (a, b) in
    if Windtrap_mut_________fixture_inline_tests___ml.___windtrap_armed___ 0
    then __windtrap_mut_0_l - __windtrap_mut_0_r
    else __windtrap_mut_0_l + __windtrap_mut_0_r
  let ordered a b =
    if
      let (__windtrap_mut_1_l, __windtrap_mut_1_r) =
        ((a, b) : _ Windtrap_mut_________fixture_inline_tests___ml.operands) in
      (if Windtrap_mut_________fixture_inline_tests___ml.___windtrap_armed___ 1
       then Stdlib.not (__windtrap_mut_1_r < __windtrap_mut_1_l)
       else __windtrap_mut_1_l < __windtrap_mut_1_r)
    then 1
    else 0
  [%%test let "sums" = if (sum 1 2) = 3 then () else failwith "sum"]
  [%%test
    module Grouped =
      struct
        let twice x = x + x
        [%%test
          let "orders" =
            if (ordered 1 (twice 1)) > 0 then () else failwith "order"]
      end]
  [%%expect_test let "prints" = print_int ((sum 1 2) - 1); [%expect {| 2 |}]]

  $ ../pp.exe -apply ppx_windtrap,windtrap_mutate --impl ./fixture_inline_tests.ml | ../elide.exe
  mutation sites of "./fixture_inline_tests.ml" in Windtrap_mut_________fixture_inline_tests___ml, with type 'a operands:
    0: 6:14 "sub" "a + b" -> "a - b"
    1: 7:21 "le" "a < b" -> "a <= b"
  let sum a b =
    let (__windtrap_mut_0_l, __windtrap_mut_0_r) = (a, b) in
    if Windtrap_mut_________fixture_inline_tests___ml.___windtrap_armed___ 0
    then __windtrap_mut_0_l - __windtrap_mut_0_r
    else __windtrap_mut_0_l + __windtrap_mut_0_r
  let ordered a b =
    if
      let (__windtrap_mut_1_l, __windtrap_mut_1_r) =
        ((a, b) : _ Windtrap_mut_________fixture_inline_tests___ml.operands) in
      (if Windtrap_mut_________fixture_inline_tests___ml.___windtrap_armed___ 1
       then Stdlib.not (__windtrap_mut_1_r < __windtrap_mut_1_l)
       else __windtrap_mut_1_l < __windtrap_mut_1_r)
    then 1
    else 0
  let () =
    Ppx_windtrap_runtime.Ppx_runtime.add_test ~file:"./fixture_inline_tests.ml"
      ~pos:("./fixture_inline_tests.ml", 8, 0, 60) ~tags:[] "sums"
      (fun () -> if (sum 1 2) = 3 then () else failwith "sum")
  let () =
    Ppx_windtrap_runtime.Ppx_runtime.enter_group
      ~file:"./fixture_inline_tests.ml" ~tags:[] "Grouped"
  module Grouped =
    struct
      let twice x = x + x
      let () =
        Ppx_windtrap_runtime.Ppx_runtime.add_test
          ~file:"./fixture_inline_tests.ml"
          ~pos:("./fixture_inline_tests.ml", 12, 2, 78) ~tags:[] "orders"
          (fun () -> if (ordered 1 (twice 1)) > 0 then () else failwith "order")
    end
  let () = Ppx_windtrap_runtime.Ppx_runtime.leave_group ()
  let () =
    Ppx_windtrap_runtime.Ppx_runtime.add_test ~file:"./fixture_inline_tests.ml"
      ~pos:("./fixture_inline_tests.ml", 15, 0, 73) ~tags:[] "prints"
      (fun () ->
         Ppx_windtrap_runtime.Ppx_runtime.expect_test
           ~pos:("./fixture_inline_tests.ml", 15, 0, 73)
           ~body_end:("./fixture_inline_tests.ml", 17, 19, 19)
           ~nodes:[("./fixture_inline_tests.ml", 17, 2, 19)]
           (fun () ->
              (Expect_test_config.run : (unit -> unit) -> unit)
                (fun () ->
                   print_int ((sum 1 2) - 1);
                   Ppx_windtrap_runtime.Ppx_runtime.reach
                     ("./fixture_inline_tests.ml", 17, 2, 19);
                   Windtrap.expect
                     (Expect_test_config.sanitize (Windtrap.output ()))
                     (("./fixture_inline_tests.ml", 17, 2, 19), {| 2 |})))
           (fun () -> Expect_test_config.sanitize (Windtrap.output ())))

  $ mut --impl ./fixture_lazy.ml | ../elide.exe
  mutation sites of "./fixture_lazy.ml" in Windtrap_mut_________fixture_lazy___ml:
    0: 10:22 "sub" "a + b" -> "a - b"
    1: 17:25 "sub" "a + b" -> "a - b"
  let thunk a b = lazy (fun () -> a + b)
  let forced a b =
    lazy
      (let (__windtrap_mut_0_l, __windtrap_mut_0_r) = (a, b) in
       if Windtrap_mut_________fixture_lazy___ml.___windtrap_armed___ 0
       then __windtrap_mut_0_l - __windtrap_mut_0_r
       else __windtrap_mut_0_l + __windtrap_mut_0_r)
  let plain x = lazy x
  let annotated a b = lazy (fun () -> a + b : unit -> int)
  let computed a b =
    lazy
      (let (__windtrap_mut_1_l, __windtrap_mut_1_r) = (a, b) in
       if Windtrap_mut_________fixture_lazy___ml.___windtrap_armed___ 1
       then __windtrap_mut_1_l - __windtrap_mut_1_r
       else __windtrap_mut_1_l + __windtrap_mut_1_r : int)

  $ mut --impl ./fixture_lazy_condition.ml | ../elide.exe
  mutation sites of "./fixture_lazy_condition.ml" in Windtrap_mut_________fixture_lazy_condition___ml:
    0: 4:17 "not" "x" -> "not x"
  let deferred x = if lazy x then 1 else 0
  let plain x =
    if
      let __windtrap_mut_0_p = x in
      (if
         Windtrap_mut_________fixture_lazy_condition___ml.___windtrap_armed___
           0
       then Stdlib.not __windtrap_mut_0_p
       else __windtrap_mut_0_p)
    then 1
    else 0

  $ mut --impl ./fixture_lost_cmp.ml | ../elide.exe
  mutation sites of "./fixture_lost_cmp.ml" in Windtrap_mut_________fixture_lost_cmp___ml:
    0: 10:43 "add" "a - b" -> "a + b"
    1: 10:32 "sub" "a + b" -> "a - b"
    2: 10:21 "not" "a < b" -> "not (a < b)"
    3: 11:18 "or" "a && b" -> "a || b"
  let (<=) a b = (compare a b) <= 0
  let not b = b
  let ordered a b =
    if
      let __windtrap_mut_2_p = a < b in
      (if Windtrap_mut_________fixture_lost_cmp___ml.___windtrap_armed___ 2
       then Stdlib.not __windtrap_mut_2_p
       else __windtrap_mut_2_p)
    then
      let (__windtrap_mut_1_l, __windtrap_mut_1_r) = (a, b) in
      (if Windtrap_mut_________fixture_lost_cmp___ml.___windtrap_armed___ 1
       then __windtrap_mut_1_l - __windtrap_mut_1_r
       else __windtrap_mut_1_l + __windtrap_mut_1_r)
    else
      (let (__windtrap_mut_0_l, __windtrap_mut_0_r) = (a, b) in
       if Windtrap_mut_________fixture_lost_cmp___ml.___windtrap_armed___ 0
       then __windtrap_mut_0_l + __windtrap_mut_0_r
       else __windtrap_mut_0_l - __windtrap_mut_0_r)
  let both a b =
    if
      let __windtrap_mut_3_p = a in
      (if
         Stdlib.(<>) (__windtrap_mut_3_p : Stdlib.Bool.t)
           (Windtrap_mut_________fixture_lost_cmp___ml.___windtrap_armed___ 3)
       then b
       else __windtrap_mut_3_p)
    then 1
    else 0

  $ mut --impl ./fixture_lost_con.ml | ../elide.exe
  mutation sites of "./fixture_lost_con.ml" in Windtrap_mut_________fixture_lost_con___ml, with type 'a operands:
    0: 11:18 "not" "a && b" -> "not (a && b)"
    1: 12:20 "not" "a || b" -> "not (a || b)"
    2: 13:24 "not" "(a < b) && c" -> "not ((a < b) && c)"
    3: 14:43 "add" "a - b" -> "a + b"
    4: 14:32 "sub" "a + b" -> "a - b"
    5: 14:21 "le" "a < b" -> "a <= b"
  external (&&) : bool -> bool -> bool = "%sequand"
  let both a b =
    if
      let __windtrap_mut_0_p = a && b in
      (if Windtrap_mut_________fixture_lost_con___ml.___windtrap_armed___ 0
       then Stdlib.not __windtrap_mut_0_p
       else __windtrap_mut_0_p)
    then 1
    else 0
  let either a b =
    if
      let __windtrap_mut_1_p = a || b in
      (if Windtrap_mut_________fixture_lost_con___ml.___windtrap_armed___ 1
       then Stdlib.not __windtrap_mut_1_p
       else __windtrap_mut_1_p)
    then 1
    else 0
  let compared a b c =
    if
      let __windtrap_mut_2_p = (a < b) && c in
      (if Windtrap_mut_________fixture_lost_con___ml.___windtrap_armed___ 2
       then Stdlib.not __windtrap_mut_2_p
       else __windtrap_mut_2_p)
    then 1
    else 0
  let ordered a b =
    if
      let (__windtrap_mut_5_l, __windtrap_mut_5_r) =
        ((a, b) : _ Windtrap_mut_________fixture_lost_con___ml.operands) in
      (if Windtrap_mut_________fixture_lost_con___ml.___windtrap_armed___ 5
       then Stdlib.not (__windtrap_mut_5_r < __windtrap_mut_5_l)
       else __windtrap_mut_5_l < __windtrap_mut_5_r)
    then
      let (__windtrap_mut_4_l, __windtrap_mut_4_r) = (a, b) in
      (if Windtrap_mut_________fixture_lost_con___ml.___windtrap_armed___ 4
       then __windtrap_mut_4_l - __windtrap_mut_4_r
       else __windtrap_mut_4_l + __windtrap_mut_4_r)
    else
      (let (__windtrap_mut_3_l, __windtrap_mut_3_r) = (a, b) in
       if Windtrap_mut_________fixture_lost_con___ml.___windtrap_armed___ 3
       then __windtrap_mut_3_l + __windtrap_mut_3_r
       else __windtrap_mut_3_l - __windtrap_mut_3_r)

  $ mut --impl ./fixture_neg.ml | ../elide.exe
  mutation sites of "./fixture_neg.ml" in Windtrap_mut_________fixture_neg___ml:
    0: 8:23 "not" "flag" -> "not flag"
    1: 9:23 "not" "flag" -> "not flag"
    2: 12:8 "not" "ready ()" -> "not (ready ())"
    3: 16:39 "not" "p y" -> "not (p y)"
    4: 20:20 "not" "if a then b else false" -> "not (if a then b else false)"
    5: 20:23 "not" "a" -> "not a"
  let pick flag x y =
    if
      let __windtrap_mut_0_p = flag in
      (if Windtrap_mut_________fixture_neg___ml.___windtrap_armed___ 0
       then Stdlib.not __windtrap_mut_0_p
       else __windtrap_mut_0_p)
    then x
    else y
  let announce flag =
    if
      let __windtrap_mut_1_p = flag in
      (if Windtrap_mut_________fixture_neg___ml.___windtrap_armed___ 1
       then Stdlib.not __windtrap_mut_1_p
       else __windtrap_mut_1_p)
    then print_string "on"
  let drain ready step =
    while
      let __windtrap_mut_2_p = ready () in
      if Windtrap_mut_________fixture_neg___ml.___windtrap_armed___ 2
      then Stdlib.not __windtrap_mut_2_p
      else __windtrap_mut_2_p do step () done
  let classify p x =
    match x with
    | y when
        let __windtrap_mut_3_p = p y in
        if Windtrap_mut_________fixture_neg___ml.___windtrap_armed___ 3
        then Stdlib.not __windtrap_mut_3_p
        else __windtrap_mut_3_p -> "yes"
    | _ -> "no"
  let nested a b =
    if
      let __windtrap_mut_4_p =
        if
          let __windtrap_mut_5_p = a in
          (if Windtrap_mut_________fixture_neg___ml.___windtrap_armed___ 5
           then Stdlib.not __windtrap_mut_5_p
           else __windtrap_mut_5_p)
        then b
        else false in
      (if Windtrap_mut_________fixture_neg___ml.___windtrap_armed___ 4
       then Stdlib.not __windtrap_mut_4_p
       else __windtrap_mut_4_p)
    then 1
    else 0

  $ mut --impl ./fixture_nesting.ml | ../elide.exe
  mutation sites of "./fixture_nesting.ml" in Windtrap_mut_________fixture_nesting___ml, with type 'a operands:
    0: 6:23 "or" "b && c" -> "b || c"
    1: 7:18 "or" "a && b" -> "a || b"
    2: 8:30 "and" "c || d" -> "c && d"
    3: 11:20 "le" "a < b" -> "a <= b"
    4: 12:20 "or" "a && b" -> "a || b"
    5: 13:18 "not" "a" -> "not a"
  let chain a b c =
    a &&
      (let __windtrap_mut_0_p = b in
       if
         Stdlib.(<>) (__windtrap_mut_0_p : Stdlib.Bool.t)
           (Windtrap_mut_________fixture_nesting___ml.___windtrap_armed___ 0)
       then c
       else __windtrap_mut_0_p)
  let mixed a b c =
    (let __windtrap_mut_1_p = a in
     if
       Stdlib.(<>) (__windtrap_mut_1_p : Stdlib.Bool.t)
         (Windtrap_mut_________fixture_nesting___ml.___windtrap_armed___ 1)
     then b
     else __windtrap_mut_1_p) || c
  let deep a b c d =
    a ||
      (b &&
         (let __windtrap_mut_2_p = c in
          if
            Stdlib.(=) (__windtrap_mut_2_p : Stdlib.Bool.t)
              (Windtrap_mut_________fixture_nesting___ml.___windtrap_armed___ 2)
          then d
          else __windtrap_mut_2_p))
  let by_cmp a b =
    if
      let (__windtrap_mut_3_l, __windtrap_mut_3_r) =
        ((a, b) : _ Windtrap_mut_________fixture_nesting___ml.operands) in
      (if Windtrap_mut_________fixture_nesting___ml.___windtrap_armed___ 3
       then Stdlib.not (__windtrap_mut_3_r < __windtrap_mut_3_l)
       else __windtrap_mut_3_l < __windtrap_mut_3_r)
    then 1
    else 0
  let by_con a b =
    if
      let __windtrap_mut_4_p = a in
      (if
         Stdlib.(<>) (__windtrap_mut_4_p : Stdlib.Bool.t)
           (Windtrap_mut_________fixture_nesting___ml.___windtrap_armed___ 4)
       then b
       else __windtrap_mut_4_p)
    then 1
    else 0
  let by_neg a =
    if
      let __windtrap_mut_5_p = a in
      (if Windtrap_mut_________fixture_nesting___ml.___windtrap_armed___ 5
       then Stdlib.not __windtrap_mut_5_p
       else __windtrap_mut_5_p)
    then 1
    else 0

  $ mut --impl ./fixture_off.ml | ../elide.exe
  mutation sites of "./fixture_off.ml" in Windtrap_mut_________fixture_off___ml:
    0: 9:5 "ge" "want > 16" -> "want >= 16", dismissed "both arms yield 16 at the boundary"
    1: 12:19 "or" "a && b" -> "a || b", dismissed ""
    2: 26:18 "sub" "a + b" -> "a - b"
  let cap want =
    if ((want > 16)[@mutate off "both arms yield 16 at the boundary"])
    then want
    else 16
  let plain a b = if ((a && b)[@mutate off]) then 1 else 0
  let sum a b = a + b[@@mutate off]
  module Hidden = struct let diff a b = a - b end[@@mutate off]
  [@@@mutate off]
  let suppressed a b = a + b
  [@@@mutate on]
  let visible a b =
    let (__windtrap_mut_2_l, __windtrap_mut_2_r) = (a, b) in
    if Windtrap_mut_________fixture_off___ml.___windtrap_armed___ 2
    then __windtrap_mut_2_l - __windtrap_mut_2_r
    else __windtrap_mut_2_l + __windtrap_mut_2_r

  $ mut --impl ./fixture_off_edges.ml | ../elide.exe
  mutation sites of "./fixture_off_edges.ml" in Windtrap_mut_________fixture_off_edges___ml:
    0: 15:4 "ge" "want > 16" -> "want >= 16", dismissed "a \"quoted\" reason,\nspanning lines \\ containing a backslash"
    1: 30:10 "sub" "a + b" -> "a - b"
    2: 39:16 "sub" "a + b" -> "a - b"
  let quoted want =
    if ((want > 16)
      [@mutate
        off "a \"quoted\" reason,\nspanning lines \\ containing a backslash"])
    then want
    else 16
  let coarse a b = ((match a with | 0 -> b | n -> n + b)
    [@mutate off "not a site"])
  let local a b =
    let s =
      let (__windtrap_mut_1_l, __windtrap_mut_1_r) = (a, b) in
      if Windtrap_mut_________fixture_off_edges___ml.___windtrap_armed___ 1
      then __windtrap_mut_1_l - __windtrap_mut_1_r
      else __windtrap_mut_1_l + __windtrap_mut_1_r[@@mutate off "ignored here"] in
    s
  type t = int[@@mutate off]
  let after a b =
    let (__windtrap_mut_2_l, __windtrap_mut_2_r) = (a, b) in
    if Windtrap_mut_________fixture_off_edges___ml.___windtrap_armed___ 2
    then __windtrap_mut_2_l - __windtrap_mut_2_r
    else __windtrap_mut_2_l + __windtrap_mut_2_r

  $ mut --impl ./fixture_off_structure.ml | ../elide.exe
  mutation sites of "./fixture_off_structure.ml" in Windtrap_mut_________fixture_off_structure___ml:
    0: 16:12 "add" "x - 1" -> "x + 1"
    1: 41:23 "sub" "a + b" -> "a - b"
    2: 48:14 "sub" "a + b" -> "a - b"
  module rec Dark:sig val f : int -> int end = struct let f n = n + 1 end
  [@@mutate off]
  
  let local n =
    let h x =
      let (__windtrap_mut_0_l, __windtrap_mut_0_r) = (x, 1) in
      if Windtrap_mut_________fixture_off_structure___ml.___windtrap_armed___ 0
      then __windtrap_mut_0_l + __windtrap_mut_0_r
      else __windtrap_mut_0_l - __windtrap_mut_0_r[@@mutate bogus] in
    h n
  type t = int[@@mutate bogus]
  let reasoned a b = a + b[@@mutate off "binding reason"]
  [@@@mutate off "region reason"]
  let in_region a b = a - b
  [@@@mutate on]
  [@@@mutate off]
  module Inherits =
    struct
      let dark a b = a + b
      [@@@mutate on]
      let lit_inside a b =
        let (__windtrap_mut_1_l, __windtrap_mut_1_r) = (a, b) in
        if
          Windtrap_mut_________fixture_off_structure___ml.___windtrap_armed___
            1
        then __windtrap_mut_1_l - __windtrap_mut_1_r
        else __windtrap_mut_1_l + __windtrap_mut_1_r
    end
  let still_dark a b = a + b
  [@@@mutate on]
  let lit a b =
    let (__windtrap_mut_2_l, __windtrap_mut_2_r) = (a, b) in
    if Windtrap_mut_________fixture_off_structure___ml.___windtrap_armed___ 2
    then __windtrap_mut_2_l - __windtrap_mut_2_r
    else __windtrap_mut_2_l + __windtrap_mut_2_r

  $ mut --impl ./fixture_off_unclosed.ml | ../elide.exe
  mutation sites of "./fixture_off_unclosed.ml" in Windtrap_mut_________fixture_off_unclosed___ml:
    0: 17:18 "sub" "a + b" -> "a - b"
  module Quiet = struct [@@@mutate off]
                        let hidden a b = a + b end
  let visible a b =
    let (__windtrap_mut_0_l, __windtrap_mut_0_r) = (a, b) in
    if Windtrap_mut_________fixture_off_unclosed___ml.___windtrap_armed___ 0
    then __windtrap_mut_0_l - __windtrap_mut_0_r
    else __windtrap_mut_0_l + __windtrap_mut_0_r
  [@@@mutate off]
  let suppressed a b = a + b
  let also_suppressed a b = if a && b then 1 else 0

  $ mut --impl ./fixture_payloads.ml | ../elide.exe
  mutation sites of "./fixture_payloads.ml" in Windtrap_mut_________fixture_payloads___ml:
    0: 6:18 "sub" "a + b" -> "a - b"
  let extended = [%ext a + b]
  let attributed = ((0)[@attr a + b])
  let written a b =
    let (__windtrap_mut_0_l, __windtrap_mut_0_r) = (a, b) in
    if Windtrap_mut_________fixture_payloads___ml.___windtrap_armed___ 0
    then __windtrap_mut_0_l - __windtrap_mut_0_r
    else __windtrap_mut_0_l + __windtrap_mut_0_r

  $ mut --impl ./fixture_shadow.ml | ../elide.exe
  mutation sites of "./fixture_shadow.ml" in Windtrap_mut_________fixture_shadow___ml, with type 'a operands:
    0: 16:21 "le" "a < b" -> "a <= b"
    1: 17:15 "or" "a && b" -> "a || b"
  module Vec =
    struct
      type t = {
        x: int ;
        y: int }
      let (+) a b = { x = (a.x + b.x); y = (a.y + b.y) }
    end
  let move a b = Vec.(+) a b
  let ordered a b =
    if
      let (__windtrap_mut_0_l, __windtrap_mut_0_r) =
        ((a, b) : _ Windtrap_mut_________fixture_shadow___ml.operands) in
      (if Windtrap_mut_________fixture_shadow___ml.___windtrap_armed___ 0
       then Stdlib.not (__windtrap_mut_0_r < __windtrap_mut_0_l)
       else __windtrap_mut_0_l < __windtrap_mut_0_r)
    then 1
    else 0
  let both a b =
    let __windtrap_mut_1_p = a in
    if
      Stdlib.(<>) (__windtrap_mut_1_p : Stdlib.Bool.t)
        (Windtrap_mut_________fixture_shadow___ml.___windtrap_armed___ 1)
    then b
    else __windtrap_mut_1_p
  let untouched a b = a - b

  $ mut --impl ./fixture_texts.ml | ../elide.exe
  mutation sites of "./fixture_texts.ml" in Windtrap_mut_________fixture_texts___ml, with type 'a operands:
    0: 6:18 "neq" "s = \"a b\"" -> "s <> \"a b\""
    1: 10:15 "sub" "a + b" -> "a - b"
    2: 11:22 "le" "a < b" -> "a <= b"
  let spaced s =
    if
      let __windtrap_mut_0_p = s = "a   b" in
      (if Windtrap_mut_________fixture_texts___ml.___windtrap_armed___ 0
       then Stdlib.not __windtrap_mut_0_p
       else __windtrap_mut_0_p)
    then 1
    else 0
  let kept a b =
    let (__windtrap_mut_1_l, __windtrap_mut_1_r) = (a, b) in
    if Windtrap_mut_________fixture_texts___ml.___windtrap_armed___ 1
    then __windtrap_mut_1_l - __windtrap_mut_1_r
    else ((__windtrap_mut_1_l + __windtrap_mut_1_r)[@kept ])
  let kept_cmp a b =
    if
      let (__windtrap_mut_2_l, __windtrap_mut_2_r) =
        ((a, b) : _ Windtrap_mut_________fixture_texts___ml.operands) in
      (if Windtrap_mut_________fixture_texts___ml.___windtrap_armed___ 2
       then Stdlib.not (__windtrap_mut_2_r < __windtrap_mut_2_l)
       else ((__windtrap_mut_2_l < __windtrap_mut_2_r)[@kept ]))
    then 1
    else 0

Over code a deriver generated, for which generated.ml stands in:

  $ ../pp.exe -apply windtrap_test_generated,windtrap_mutate --impl ./fixture_generated.ml | ../elide.exe
  mutation sites of "./fixture_generated.ml" in Windtrap_mut_________fixture_generated___ml:
    0: 6:18 "add" "a - b" -> "a + b"
    1: 11:21 "sub" "a + b" -> "a - b"
  let generated a b = ((a + b)[@generated ])
  let written a b =
    let (__windtrap_mut_0_l, __windtrap_mut_0_r) = (a, b) in
    if Windtrap_mut_________fixture_generated___ml.___windtrap_armed___ 0
    then __windtrap_mut_0_l + __windtrap_mut_0_r
    else __windtrap_mut_0_l - __windtrap_mut_0_r
  let copied a b =
    a *
      (let (__windtrap_mut_1_l, __windtrap_mut_1_r) = (a, b) in
       if Windtrap_mut_________fixture_generated___ml.___windtrap_armed___ 1
       then __windtrap_mut_1_l - __windtrap_mut_1_r
       else __windtrap_mut_1_l + __windtrap_mut_1_r)[@@duplicate ]
  let copied a b = a * (a + b)[@@duplicate ]

  $ ../pp.exe -apply windtrap_test_generated,windtrap_mutate --impl ./fixture_generated_chain.ml | ../elide.exe
  mutation sites of "./fixture_generated_chain.ml" in Windtrap_mut_________fixture_generated_chain___ml:
    0: 6:23 "sub" "(a + b) + c" -> "(a + b) - c"
  let generated a b c = (((a + b) + c)[@generated ])
  let copied f a b c =
    f
      (let (__windtrap_mut_0_l, __windtrap_mut_0_r) = ((a + b), c) in
       if
         Windtrap_mut_________fixture_generated_chain___ml.___windtrap_armed___
           0
       then __windtrap_mut_0_l - __windtrap_mut_0_r
       else __windtrap_mut_0_l + __windtrap_mut_0_r)[@@duplicate ]
  let copied f a b c = f ((a + b) + c)[@@duplicate ]

Over a file the coverage rewriter ran on first. pp.exe runs the mutation
rewriter first whatever order -apply names, so this order has its own
driver, coverage_first.exe:

  $ ../coverage_first.exe -apply windtrap_coverage,windtrap_mutate --impl ./fixture_visits.ml | ../elide.exe
  mutation sites of "./fixture_visits.ml" in Windtrap_mut_________fixture_visits___ml, with type 'a operands:
    0: 7:22 "not" "b" -> "not b"
    1: 7:17 "not" "a" -> "not a"
    2: 10:18 "le" "(___windtrap_post_visit___ 2 (f x)) < (___windtrap_post_visit___ 3 (g x))" -> "(___windtrap_post_visit___ 2 (f x)) <= (___windtrap_post_visit___ 3 (g x))"
  coverage points of "./fixture_visits.ml" in Windtrap_cov_________fixture_visits___ml, with ___windtrap_post_visit___:
    0: 262-263
    1: 267-268
    2: 362-365
    3: 368-371
    4: 384-385
    5: 377-378
    6: 359-385
  let either a b =
    ___windtrap_visit___ 0;
    if
      (let __windtrap_mut_1_p = a in
       if Windtrap_mut_________fixture_visits___ml.___windtrap_armed___ 1
       then Stdlib.not __windtrap_mut_1_p
       else __windtrap_mut_1_p)
    then (___windtrap_visit___ 0; true)
    else
      if
        (let __windtrap_mut_0_p = b in
         if Windtrap_mut_________fixture_visits___ml.___windtrap_armed___ 0
         then Stdlib.not __windtrap_mut_0_p
         else __windtrap_mut_0_p)
      then (___windtrap_visit___ 1; true)
      else false
  let lt f g x =
    ___windtrap_visit___ 6;
    if
      (let (__windtrap_mut_2_l, __windtrap_mut_2_r) =
         (((___windtrap_post_visit___ 2 (f x)),
            (___windtrap_post_visit___ 3 (g x))) : _
                                                     Windtrap_mut_________fixture_visits___ml.operands) in
       if Windtrap_mut_________fixture_visits___ml.___windtrap_armed___ 2
       then Stdlib.not (__windtrap_mut_2_r < __windtrap_mut_2_l)
       else __windtrap_mut_2_l < __windtrap_mut_2_r)
    then (___windtrap_visit___ 5; 1)
    else (___windtrap_visit___ 4; 0)

A file under an input name that is not a source file's is returned as
parsed:

  $ for name in //toplevel// '(stdin)' lib/.ocamlinit lib/topfind; do
  >   mut -loc-filename "$name" --impl ./fixture_input_name.ml
  > done
  let sign n = if n > 0 then 1 else 0
  let sign n = if n > 0 then 1 else 0
  let sign n = if n > 0 then 1 else 0
  let sign n = if n > 0 then 1 else 0

A refusal is an error located at the attribute, and the driver exits 1:

  $ mut --impl ./reject_bad_payload.ml
  File "./reject_bad_payload.ml", line 1, characters 18-33:
  1 | let f n = (n + 1) [@mutate bogus]
                        ^^^^^^^^^^^^^^^
  Error: Bad payload in mutate attribute.
  [1]

  $ mut --impl ./reject_double_off.ml
  File "./reject_double_off.ml", line 5, characters 0-15:
  5 | [@@@mutate off]
      ^^^^^^^^^^^^^^^
  Error: Mutation is already off.
  [1]

  $ mut --impl ./reject_empty_payload.ml
  File "./reject_empty_payload.ml", line 1, characters 18-27:
  1 | let f n = (n + 1) [@mutate]
                        ^^^^^^^^^
  Error: Bad payload in mutate attribute.
  [1]

  $ mut --impl ./reject_exclude_file_binding.ml
  File "./reject_exclude_file_binding.ml", line 1, characters 16-39:
  1 | let f n = n + 1 [@@mutate exclude_file]
                      ^^^^^^^^^^^^^^^^^^^^^^^
  Error: mutate exclude_file is not allowed here.
  [1]

  $ mut --impl ./reject_exclude_file_expr.ml
  File "./reject_exclude_file_expr.ml", line 1, characters 18-40:
  1 | let f n = (n + 1) [@mutate exclude_file]
                        ^^^^^^^^^^^^^^^^^^^^^^
  Error: mutate exclude_file is not allowed here.
  [1]

  $ mut --impl ./reject_misplaced_exclude_file.ml
  File "./reject_misplaced_exclude_file.ml", line 2, characters 2-26:
  2 |   [@@@mutate exclude_file]
        ^^^^^^^^^^^^^^^^^^^^^^^^
  Error: mutate exclude_file is not allowed here.
  [1]

  $ mut --impl ./reject_misplaced_on.ml
  File "./reject_misplaced_on.ml", line 1, characters 18-30:
  1 | let f n = (n + 1) [@mutate on]
                        ^^^^^^^^^^^^
  Error: mutate on is not allowed here.
  [1]

  $ mut --impl ./reject_off_number.ml
  File "./reject_off_number.ml", line 1, characters 18-34:
  1 | let f n = (n + 1) [@mutate off 42]
                        ^^^^^^^^^^^^^^^^
  Error: Bad payload in mutate attribute.
  [1]

  $ mut --impl ./reject_off_rewrite.ml
  File "./reject_off_rewrite.ml", line 3, characters 18-51:
  3 | let f n = (n + 1) [@mutate off sub "equal at zero"]
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Bad payload in mutate attribute.
  [1]

  $ mut --impl ./reject_off_two_reasons.ml
  File "./reject_off_two_reasons.ml", line 1, characters 18-39:
  1 | let f n = (n + 1) [@mutate off "a" "b"]
                        ^^^^^^^^^^^^^^^^^^^^^
  Error: Bad payload in mutate attribute.
  [1]

  $ mut --impl ./reject_on_assert.ml
  File "./reject_on_assert.ml", line 1, characters 19-31:
  1 | let f x = assert x [@mutate on]
                         ^^^^^^^^^^^^
  Error: mutate on is not allowed here.
  [1]

  $ mut --impl ./reject_on_binding.ml
  File "./reject_on_binding.ml", line 1, characters 16-29:
  1 | let f n = n + 1 [@@mutate on]
                      ^^^^^^^^^^^^^
  Error: mutate on is not allowed here.
  [1]

  $ mut --impl ./reject_on_outside.ml
  File "./reject_on_outside.ml", line 3, characters 0-14:
  3 | [@@@mutate on]
      ^^^^^^^^^^^^^^
  Error: Mutation is already on.
  [1]
