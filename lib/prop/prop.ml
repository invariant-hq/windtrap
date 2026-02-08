(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

exception Reject

let assume b = if not b then raise Reject
let reject () = raise Reject

type config = {
  count : int;
  max_gen : int;
  max_shrink : int;
  seed : int option;
}

let default_config =
  { count = 100; max_gen = 300; max_shrink = 100; seed = None }

type result =
  | Success of { count : int; discarded : int }
  | Failed of {
      count : int;
      discarded : int;
      seed : int;
      counterexample : string;
      shrunk_counterexample : string;
      shrink_steps : int;
    }
  | Error of {
      count : int;
      seed : int;
      counterexample : string;
      exn : exn;
      backtrace : string;
    }
  | Gave_up of { count : int; discarded : int; seed : int }

type 'a test_outcome = Pass | Fail | Discard | Exception of exn * string
type shrink_goal = Shrink_failures | Shrink_exceptions

let run_test prop x =
  try if prop x then Pass else Fail with
  | Reject -> Discard
  | exn ->
      let bt = Printexc.get_raw_backtrace () in
      Exception (exn, Printexc.raw_backtrace_to_string bt)

(* Greedy descent: at each node, try shrink candidates left-to-right and take
   the first one that still matches the current failure mode, then recurse. *)
let shrink ~max_shrink ~goal ~prop tree =
  let accepts = function
    | Pass | Discard -> false
    | Fail -> goal = Shrink_failures
    | Exception _ -> goal = Shrink_exceptions
  in
  let rec go steps current_tree =
    if steps >= max_shrink then (current_tree, steps)
    else
      let rec try_shrinks seq =
        match seq () with
        | Seq.Nil -> None
        | Seq.Cons (candidate_tree, rest) ->
            let candidate = Tree.root candidate_tree in
            if accepts (run_test prop candidate) then Some candidate_tree
            else try_shrinks rest
      in
      match try_shrinks (Tree.children current_tree) with
      | None -> (current_tree, steps)
      | Some smaller_tree -> go (steps + 1) smaller_tree
  in
  go 0 tree

let random_seed () =
  Random.self_init ();
  Random.bits ()

let default_seed = ref None
let set_default_seed s = default_seed := s
let default_count = ref None
let set_default_count c = default_count := c

let get_seed config =
  match config.seed with
  | Some s -> s
  | None -> (
      match !default_seed with
      | Some s -> s
      | None ->
          Option.bind (Sys.getenv_opt "WINDTRAP_SEED") int_of_string_opt
          |> Option.value ~default:(random_seed ()))

let check ?(config = default_config) ?rand arb prop =
  let config =
    match !default_count with
    | Some c when config.count = default_config.count ->
        let max_gen =
          if config.max_gen = default_config.max_gen then c * 3
          else config.max_gen
        in
        { config with count = c; max_gen }
    | _ -> config
  in
  let seed = get_seed config in
  let rand =
    match rand with Some r -> r | None -> Random.State.make [| seed |]
  in
  let gen = Arbitrary.gen arb in
  let print = Arbitrary.print arb in

  let rec loop ~passed ~discarded ~generated =
    if passed >= config.count then Success { count = passed; discarded }
    else if generated >= config.max_gen then
      Gave_up { count = passed; discarded; seed }
    else
      match
        try Ok (gen rand)
        with exn ->
          let bt = Printexc.get_raw_backtrace () in
          Stdlib.Error (exn, Printexc.raw_backtrace_to_string bt)
      with
      | Stdlib.Error (exn, backtrace) ->
          Error
            {
              count = passed;
              seed;
              counterexample = "<generator raised before producing a value>";
              exn;
              backtrace;
            }
      | Ok tree -> (
          let x = Tree.root tree in
          match run_test prop x with
          | Pass ->
              loop ~passed:(passed + 1) ~discarded ~generated:(generated + 1)
          | Discard ->
              loop ~passed ~discarded:(discarded + 1) ~generated:(generated + 1)
          | Fail ->
              let counterexample = print x in
              let shrunk_tree, shrink_steps =
                shrink ~max_shrink:config.max_shrink ~goal:Shrink_failures ~prop
                  tree
              in
              let shrunk_counterexample = print (Tree.root shrunk_tree) in
              Failed
                {
                  count = passed;
                  discarded;
                  seed;
                  counterexample;
                  shrunk_counterexample;
                  shrink_steps;
                }
          | Exception (exn, backtrace) ->
              let shrunk_tree, _ =
                shrink ~max_shrink:config.max_shrink ~goal:Shrink_exceptions
                  ~prop tree
              in
              let shrunk_value = Tree.root shrunk_tree in
              let counterexample = print shrunk_value in
              let exn, backtrace =
                match run_test prop shrunk_value with
                | Exception (shrunk_exn, shrunk_bt) -> (shrunk_exn, shrunk_bt)
                | _ -> (exn, backtrace)
              in
              Error { count = passed; seed; counterexample; exn; backtrace })
  in
  loop ~passed:0 ~discarded:0 ~generated:0
