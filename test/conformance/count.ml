(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* [count.exe CORPUS] prints the corpus's numbers, computed from its files
   so that no document restates them by hand:

   - the pass set: the modules of every (inline_tests) library, which
     dune's own backend runs, and the fixtures a runner drives whose
     golden is the driver's placeholder and which upstream does not
     correct;
   - the corrections: every fixture with upstream's correction vendored
     beside it (<f>.ml.corrected.upstream), split by whether windtrap's
     golden (<f>.ml.corrected.expected) is the same bytes, and the ones
     where windtrap writes no correction at all;
   - the refused: every <f>.rejected.expected (refused at expansion) and
     <f>.compile-rejected.expected (refused by the compiler).

   Every number is also a runtest outcome: this only counts what the
   rules check. *)

let placeholder = "=== no correction produced ===\n"
let read path = In_channel.with_open_bin path In_channel.input_all

let rec files dir =
  Sys.readdir dir |> Array.to_list |> List.sort compare
  |> List.concat_map (fun name ->
      let path = Filename.concat dir name in
      if Sys.is_directory path then files path else [ path ])

(* Just enough of dune's syntax to find a library's modules: atoms, lists
   and line comments. *)
type sexp = Atom of string | List of sexp list

let parse text =
  let n = String.length text in
  let rec skip i =
    if i >= n then i
    else
      match text.[i] with
      | ' ' | '\n' | '\t' | '\r' -> skip (i + 1)
      | ';' -> (
          match String.index_from_opt text i '\n' with
          | Some j -> skip (j + 1)
          | None -> n)
      | _ -> i
  in
  let rec items acc i =
    let i = skip i in
    if i >= n || text.[i] = ')' then (List.rev acc, i + 1)
    else
      let item, i = sexp i in
      items (item :: acc) i
  and sexp i =
    if text.[i] = '(' then
      let children, i = items [] (i + 1) in
      (List children, i)
    else
      let rec stop j =
        if j >= n then j
        else
          match text.[j] with
          | ' ' | '\n' | '\t' | '\r' | '(' | ')' -> j
          | _ -> stop (j + 1)
      in
      let j = stop i in
      (Atom (String.sub text i (j - i)), j)
  in
  fst (items [] 0)

let inline_test_modules dune_file =
  List.concat_map
    (function
      | List (Atom "library" :: fields)
        when List.mem (List [ Atom "inline_tests" ]) fields ->
          List.concat_map
            (function
              | List (Atom "modules" :: modules) ->
                  List.filter_map
                    (function Atom m -> Some m | List _ -> None)
                    modules
              | _ -> [])
            fields
      | _ -> [])
    (parse (read dune_file))

let has_suffix suffix path = Filename.check_suffix path suffix
let chop suffix path = Filename.basename (Filename.chop_suffix path suffix)

let () =
  let corpus =
    match Sys.argv with
    | [| _; corpus |] -> corpus
    | _ ->
        prerr_endline "usage: count.exe CORPUS";
        exit 2
  in
  let all = files corpus in
  let inline =
    List.concat_map inline_test_modules
      (List.filter (fun p -> Filename.basename p = "dune") all)
  in
  let goldens = List.filter (has_suffix ".ml.corrected.expected") all in
  let upstream path = Filename.chop_suffix path ".expected" ^ ".upstream" in
  let driven_passes, corrections =
    List.partition (fun g -> not (List.mem (upstream g) all)) goldens
  in
  let driven_passes =
    List.filter (fun g -> read g = placeholder) driven_passes
  in
  let uncorrected, corrected =
    List.partition (fun g -> read g = placeholder) corrections
  in
  let identical, different =
    List.partition (fun g -> read g = read (upstream g)) corrected
  in
  let expansion = List.filter (has_suffix ".rejected.expected") all in
  let compilation = List.filter (has_suffix ".compile-rejected.expected") all in
  let names suffix paths =
    String.concat ", " (List.sort compare (List.map (chop suffix) paths))
  in
  let fixtures = ".ml.corrected.expected" in
  Printf.printf "pass set: %d files pass unchanged\n"
    (List.length inline + List.length driven_passes);
  Printf.printf "  %d under dune's inline_tests backend\n" (List.length inline);
  Printf.printf "  %d through a runner: %s\n"
    (List.length driven_passes)
    (names fixtures driven_passes);
  Printf.printf "corrections: %d files corrected\n" (List.length corrected);
  Printf.printf "  %d byte-identical to upstream's: %s\n"
    (List.length identical) (names fixtures identical);
  Printf.printf "  %d not: %s\n" (List.length different)
    (names fixtures different);
  Printf.printf "  %d not corrected where upstream corrects: %s\n"
    (List.length uncorrected)
    (names fixtures uncorrected);
  Printf.printf "refused: %d files\n"
    (List.length expansion + List.length compilation);
  Printf.printf "  %d at expansion\n" (List.length expansion);
  Printf.printf "  %d by the compiler: %s\n" (List.length compilation)
    (names ".compile-rejected.expected" compilation)
