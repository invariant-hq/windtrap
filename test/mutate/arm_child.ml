(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Stands in for an instrumented executable: it holds exactly the binding
   ppx_windtrap.mutate generates - one [register] call whose result is the
   file's guard closure - and three guarded expressions in the three
   shapes the RFC specifies. The parent test drives it through
   WINDTRAP_MUTATE_ARM and reads its stdout, which is how the whole
   arm-from-the-environment path gets exercised in a real process rather
   than in the test's own image.

   Modes: [run] arms from the environment and prints what the guards
   answer; [budget n times] arms with a runaway budget and evaluates a
   guard in a loop; [reach] drains the reach map in a process whose epoch
   counter is untouched, which is the only place module initialization is
   distinguishable. *)

module M = Windtrap_mutate

let guard =
  M.register ~file:"lib/child.ml"
    ~sites:
      [|
        {
          M.line = 3;
          col = 10;
          rewrite = "lt";
          before = "a < b";
          after = "not (b < a)";
          dismissed = None;
        };
        {
          M.line = 7;
          col = 4;
          rewrite = "add";
          before = "a + b";
          after = "a - b";
          dismissed = None;
        };
        {
          M.line = 11;
          col = 6;
          rewrite = "not";
          before = "n > 0";
          after = "not (n > 0)";
          dismissed = None;
        };
      |]

(* The three expansion shapes, by hand: a self-negating comparison, an
   arithmetic swap, and a negated condition. *)
let less a b =
  let r = b and l = a in
  if guard 0 then not (r < l) else l < r

let sum a b = if guard 1 then a - b else a + b

let positives ns =
  List.fold_left
    (fun n x ->
      let p = x > 0 in
      if if guard 2 then not p else p then n + 1 else n)
    0 ns

(* Module initialization: a guard evaluated before [main], hence before
   anything can be armed and before any observation window is opened. The
   fork happens after module init, so a mutant here can never be killed by
   a child; the loop must be able to tell such a site from one no test
   reached, and the only thing that lets it is the first drain reporting
   it. This is why epochs start at 1 - a fresh epoch array is all zeroes,
   so no site starts out looking already seen - and it can only be
   observed in a process no test has bumped the epoch of. *)
let () = ignore (less 1 2 : bool)

let announce : M.mutant option -> unit = function
  | None -> print_string "armed: none\n"
  | Some m ->
      Printf.printf "armed: %s %s -> %s\n" (M.id_to_string m.M.id) m.M.before
        m.M.after

let refuse e =
  Format.eprintf "windtrap mutate: %a@." M.pp_arm_error e;
  exit 1

let () =
  match Array.to_list Sys.argv with
  | _ :: "run" :: _ -> (
      match M.arm_from_env () with
      | Error e -> refuse e
      | Ok armed ->
          announce armed;
          (* [less 2 2] is where the boundary shift shows: disarmed it is
             [2 < 2], armed it is [not (2 < 2)], which is [2 <= 2]. *)
          Printf.printf "less 2 2 = %b\n" (less 2 2);
          Printf.printf "sum 3 4 = %d\n" (sum 3 4);
          Printf.printf "positives = %d\n" (positives [ 1; -2; 3 ]))
  | _ :: "budget" :: budget :: times :: _ -> (
      let budget = int_of_string budget and times = int_of_string times in
      match M.arm_from_env ~budget () with
      | Error e -> refuse e
      | Ok armed -> (
          announce armed;
          match positives (List.init times (fun i -> i - 1)) with
          | n -> Printf.printf "positives = %d\n" n
          | exception M.Runaway { id; hits; budget } ->
              Printf.printf "runaway %s after %d hits (budget %d)\n"
                (M.id_to_string id) hits budget))
  | _ :: "reach" :: _ ->
      let show label reached =
        Printf.printf "%s:%s\n" label
          (String.concat ""
             (List.map
                (fun (r : M.reached) ->
                  Printf.sprintf " %s x%d"
                    (M.id_to_string r.M.mutant.M.id)
                    r.M.hits)
                reached))
      in
      show "module-init" (M.drain ());
      M.next_epoch ();
      ignore (less 2 2 : bool);
      show "window" (M.drain ());
      show "drained" (M.drain ())
  | _ ->
      prerr_endline "arm_child: expected run | budget <n> <times> | reach";
      exit 2
