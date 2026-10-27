(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf

(* A row shows in one line, its id cut in the middle past this many
   bytes and its label past [label_bytes], as the report cuts a label. *)
let shown_bytes = 80
let label_bytes = 60
let one_line n s = Text.elide_middle n ~show:Text.escape_controls s

let refuse ?loc message =
  raise (Failure.Check_failure (Failure.message ?loc message))

(* The ids that more than one row has, each with the positions of its
   rows, in the order of their first row. *)
let duplicates ids =
  let rows = Hashtbl.create (Array.length ids) in
  Array.iteri
    (fun i text ->
      let seen = Option.value ~default:[] (Hashtbl.find_opt rows text) in
      Hashtbl.replace rows text (i :: seen))
    ids;
  List.filter_map
    (fun (i, text) ->
      match List.rev (Hashtbl.find rows text) with
      | first :: (_ :: _ as others) when first = i ->
          Some (text, first :: others)
      | _ -> None)
    (List.mapi (fun i text -> (i, text)) (Array.to_list ids))

let numbered positions =
  String.concat " and " (List.map (fun i -> string_of_int (i + 1)) positions)

let check ?loc ?label id rows fn =
  let rows = Array.of_list rows in
  let n = Array.length rows in
  if n = 0 then
    raise
      (Failure.Check_failure
         (Failure.vacuous ?loc "the table has no row, so it checks nothing"));
  let ids = Array.map id rows in
  (match duplicates ids with
  | [] -> ()
  | duplicates ->
      let duplicate (text, positions) =
        strf "rows %s have the id %s" (numbered positions)
          (one_line shown_bytes text)
      in
      refuse ?loc
        (strf
           "duplicate rows: %s; a row is identified by its id, so no two rows \
            may share one"
           (String.concat ", " (List.map duplicate duplicates))));
  (* Under --failed the table reruns the rows it recorded. A recorded row
     that it no longer has, or a counterexample, which no table records, is
     stale: the table then fails, and never passes on the rows that remain. *)
  let only =
    match Run.recorded () with
    | [] -> None
    | cases ->
        let has id = Array.exists (String.equal id) ids in
        let stale =
          List.filter_map
            (function
              | Run.Row id when has id -> None
              | Run.Row id -> Some ("row " ^ one_line shown_bytes id)
              | Run.Replay token -> Some ("counterexample " ^ token))
            cases
        in
        if stale <> [] then
          refuse ?loc
            (strf
               "--failed: the last-failed record names a case that this table \
                no longer has: %s; run it without --failed"
               (String.concat ", " stale));
        let failed = Hashtbl.create 8 in
        List.iter
          (function
            | Run.Row id -> Hashtbl.replace failed id () | Replay _ -> ())
          cases;
        Some (fun i -> Hashtbl.mem failed ids.(i))
  in
  let labels =
    Array.map
      (fun row ->
        match Option.map (fun f -> f row) label with
        | Some "" | None -> None
        | Some _ as label -> label)
      rows
  in
  let labelled i =
    match labels.(i) with
    | Some label -> ", " ^ one_line label_bytes label
    | None -> ""
  in
  let tag i =
    strf "row %d of %d%s: %s" (i + 1) n (labelled i)
      (one_line shown_bytes ids.(i))
  in
  (* The domain is the rows' positions, each ids as its row's id, so the
     engine checks every row once, in order, and reports each it refutes. *)
  let gen =
    Gen.of_list ~pp:(fun ppf i -> Pp.string ppf ids.(i)) (List.init n Fun.id)
  in
  let law i = Run.row ~tag:(tag i) ~id:ids.(i) (fun () -> fn rows.(i)) in
  let as_row (f : Failure.t) =
    match f.kind with
    | Property ({ case_index; _ } as p) ->
        let label = Option.map Failure.text labels.(case_index) in
        { f with kind = Property { p with table = true; label } }
    | _ -> f
  in
  match Run.property ?loc ~count:n ?only ~table:true gen law with
  | (_ : Property.stats) -> ()
  | exception Failure.Check_failure ({ kind = Property p; _ } as f)
    when Option.is_some p.domain ->
      let backtrace = Printexc.get_raw_backtrace () in
      let f = as_row f in
      let f =
        match f.kind with
        | Property p ->
            { f with kind = Property { p with also = List.map as_row p.also } }
        | _ -> f
      in
      Printexc.raise_with_backtrace (Failure.Check_failure f) backtrace

let cases ?__POS__ ?timeout ?retries ?label name id rows fn =
  let loc = Loc.resolve ?__POS__ () in
  Test_tree.test ?__POS__ ?timeout ?retries name (fun () ->
      check ?loc ?label id rows fn)
