(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(* The passes are three of Hypothesis's (MacIver and Donaldson, ECOOP 2020),
   chosen by the ablation of _autoresearch/audits/shrinking.md: deleting
   spans and runs carries most of the quality, and minimising finishes. *)

type case = {
  choices : int64 array;
  spans : Choice.span list;
  dependents : Choice.dependent list;
}

type verdict =
  | Fails of { parsed : case; keep : unit -> unit }
  | Passes
  | Discards
  | Not_smaller
type ending = Converged | Budget_spent

let shortlex a b =
  let rec from i =
    if i = Array.length a then 0
    else
      match Int64.unsigned_compare a.(i) b.(i) with 0 -> from (i + 1) | c -> c
  in
  match Int.compare (Array.length a) (Array.length b) with
  | 0 -> from 0
  | c -> c

(* Spans by start, the longest first; one call nested in another can mark
   the same choices twice. *)
let sorted spans =
  Array.of_list
    (List.sort_uniq
       (fun (a : Choice.span) (b : Choice.span) ->
         match Int.compare a.start b.start with
         | 0 -> Int.compare b.stop a.stop
         | c -> c)
       spans)

let cut choices i j =
  Array.append (Array.sub choices 0 i)
    (Array.sub choices j (Array.length choices - j))

(* The cut of \[[i];[j]\[, with each dependent after it lowered by the
   positions it counts past inside, so it counts what it counted. *)
let deletion case i j =
  let choices = cut case.choices i j in
  List.iter
    (fun (d : Choice.dependent) ->
      if d.at >= j then begin
        let inside p = i <= p && p < j in
        let removed = Int64.of_int (List.length (List.filter inside d.over)) in
        let at = d.at - (j - i) in
        choices.(at) <-
          (if Int64.unsigned_compare removed choices.(at) > 0 then 0L
           else Int64.sub choices.(at) removed)
      end)
    case.dependents;
  choices

let lowered choices i v =
  let choices = Array.copy choices in
  choices.(i) <- v;
  choices

exception Spent

type state = {
  test : int64 array -> verdict;
  cost : int;
  budget : int;
  mutable best : case;
  mutable spans : Choice.span array;
  mutable spent : int;
  mutable steps : int;
  seen : (int64 array, unit) Hashtbl.t;
}

let length st = Array.length st.best.choices

(* A candidate tried before runs no law and costs nothing. *)
let consider st choices =
  if Hashtbl.mem st.seen choices then false
  else begin
    if st.spent >= st.budget then raise_notrace Spent;
    Hashtbl.add st.seen choices ();
    match st.test choices with
    | Fails { parsed; keep } ->
        st.spent <- st.spent + st.cost;
        Hashtbl.replace st.seen parsed.choices ();
        if shortlex parsed.choices st.best.choices < 0 then begin
          keep ();
          st.best <- parsed;
          st.spans <- sorted parsed.spans;
          st.steps <- st.steps + 1;
          true
        end
        else false
    | Passes ->
        st.spent <- st.spent + st.cost;
        false
    | Discards ->
        st.spent <- st.spent + 1;
        false
    | Not_smaller -> false
  end

(* After a deletion the index stays, on the new spans. *)
let delete_spans st =
  let rec at i =
    if i < Array.length st.spans then
      let ({ start; stop } : Choice.span) = st.spans.(i) in
      if stop <= length st && consider st (deletion st.best start stop) then
        at i
      else at (i + 1)
  in
  at 0

let delete_runs st =
  List.iter
    (fun run ->
      let rec at i =
        if i >= 0 then begin
          if i + run <= length st then
            ignore (consider st (deletion st.best i (i + run)));
          at (i - 1)
        end
      in
      at (length st - run))
    [ 8; 4; 2; 1 ]

(* To [0], else the least value that a binary search finds failing, between
   the greatest that passed and the current one. *)
let minimise st =
  let rec at i =
    if i < length st then begin
      let set v = consider st (lowered st.best.choices i v) in
      if (not (Int64.equal st.best.choices.(i) 0L)) && not (set 0L) then begin
        let rec search low =
          if i < length st then
            let high = st.best.choices.(i) in
            let gap = Int64.sub high low in
            if Int64.unsigned_compare gap 1L > 0 then
              let mid = Int64.add low (Int64.shift_right_logical gap 1) in
              search (if set mid then low else mid)
        in
        search 0L
      end;
      at (i + 1)
    end
  in
  at 0

let shrink ~budget ~cost test case =
  let st =
    {
      test;
      cost;
      budget;
      best = case;
      spans = sorted case.spans;
      spent = 0;
      steps = 0;
      seen = Hashtbl.create 256;
    }
  in
  Hashtbl.add st.seen case.choices ();
  let ending =
    try
      let rec pass () =
        let before = st.best.choices in
        delete_spans st;
        delete_runs st;
        minimise st;
        if shortlex st.best.choices before < 0 then pass ()
      in
      pass ();
      Converged
    with Spent -> Budget_spent
  in
  (st.best, st.steps, ending)

(* The edits of one step, for a reader that searches a tree. *)
let candidates case =
  let n = Array.length case.choices in
  let deletions =
    Seq.map
      (fun ({ start; stop } : Choice.span) -> deletion case start stop)
      (Array.to_seq (sorted case.spans))
  in
  let runs =
    Seq.concat_map
      (fun run ->
        Seq.map
          (fun i -> deletion case i (i + run))
          (Seq.init (Int.max 0 (n - run + 1)) (fun k -> n - run - k)))
      (List.to_seq [ 8; 4; 2; 1 ])
  in
  (* [0], then values that each close half the gap to the current one. *)
  let towards i =
    let v = case.choices.(i) in
    let rec steps low () =
      let gap = Int64.sub v low in
      if Int64.equal gap 0L then Seq.Nil
      else
        let half = Int64.max 1L (Int64.shift_right_logical gap 1) in
        Seq.Cons (lowered case.choices i low, steps (Int64.add low half))
    in
    steps 0L
  in
  Seq.append deletions
    (Seq.append runs (Seq.concat_map towards (Seq.init n Fun.id)))
