(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type pos = Loc.pos
type 'a printer = Format.formatter -> 'a -> unit
type 'a testable = 'a Testable.t

(* Each payload has one raising helper, which resolves the location within
   the verb's call, as [Loc.capture] requires. *)
let fail_equality ?__POS__ ?msg ?not_ ~expected actual =
  Failure.refuse
    (Failure.equality ?loc:(Loc.resolve ?__POS__ ()) ?msg ?not_ ~expected
       ~actual ())

let fail_predicate ?__POS__ ?msg ~claim value =
  Failure.refuse
    (Failure.predicate ?loc:(Loc.resolve ?__POS__ ()) ?msg ~claim value)

(* The rejected side of a shape verb, through its [?pp] when it has one. *)
let rendering pp v =
  match pp with Some pp -> Pp.to_string pp v | None -> Pp.abstract

(* Tolerances *)

type tolerance = {
  site : Loc.t option;
  comparisons : int;
  error : float;
  tolerance : float;
}

(* Process-wide, as one test at a time runs in a process, and locked, as a
   domain the test spawned compares too. *)
let sites : (Loc.t option, tolerance) Hashtbl.t = Hashtbl.create 8
let sites_lock = Mutex.create ()

let locked fn =
  Mutex.lock sites_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock sites_lock) fn

let share e t = if e = 0. then 0. else e /. t

(* Two records of one site: their calls added, and the comparison that came
   closer to its bound, the larger error between two equal shares. *)
let merge a b =
  let wider =
    share b.error b.tolerance > share a.error a.tolerance
    || share b.error b.tolerance = share a.error a.tolerance
       && b.error > a.error
  in
  let worst = if wider then b else a in
  { worst with site = a.site; comparisons = a.comparisons + b.comparisons }

let add table (t : tolerance) =
  match Hashtbl.find_opt table t.site with
  | None -> Hashtbl.replace table t.site t
  | Some s -> Hashtbl.replace table t.site (merge s t)

(* A walk of the stack that reads debug information costs a microsecond or
   two, so a site is resolved once per return addresses of the verb's
   innermost frames, which locate it: the walk reads them, and the same
   addresses read the same slots. *)
let resolved : (int array, Loc.t option) Hashtbl.t = Hashtbl.create 8

(* Located within the verb's call, as [Loc.capture] requires. *)
let note ?__POS__ (e : Testable.error) =
  let site =
    match __POS__ with
    | Some _ -> Loc.resolve ?__POS__ ()
    | None -> (
        let key =
          Array.map
            (fun (slot : Printexc.raw_backtrace_entry) -> (slot :> int))
            (Printexc.raw_backtrace_entries (Printexc.get_callstack 8))
        in
        match locked (fun () -> Hashtbl.find_opt resolved key) with
        | Some site -> site
        | None ->
            let site = Loc.capture () in
            locked (fun () -> Hashtbl.replace resolved key site);
            site)
  in
  locked (fun () ->
      add sites
        { site; comparisons = 1; error = e.error; tolerance = e.tolerance })

let tolerances ?at () =
  let all =
    locked (fun () ->
        let all = List.of_seq (Hashtbl.to_seq_values sites) in
        Hashtbl.reset sites;
        all)
  in
  let located = Hashtbl.create 8 in
  List.iter
    (fun (t : tolerance) ->
      add located
        (match t.site with None -> { t with site = at } | Some _ -> t))
    all;
  List.sort
    (fun a b -> compare a.site b.site)
    (List.of_seq (Hashtbl.to_seq_values located))

let reset_tolerances () = locked (fun () -> Hashtbl.reset sites)

(* Equalities *)

let equal ?__POS__ ?msg t expected actual =
  let holds, error =
    Testable.observed (fun () -> Testable.equal t expected actual)
  in
  Option.iter (note ?__POS__) error;
  if not holds then
    fail_equality ?__POS__ ?msg
      ~expected:(Testable.to_string t expected)
      (Testable.to_string t actual)

(* Both sides hold [a]'s rendering: a tolerance can equate two values that
   print differently. *)
let not_equal ?__POS__ ?msg t a b =
  if Testable.equal t a b then
    let rendered = Testable.to_string t a in
    fail_equality ?__POS__ ?msg ~not_:true ~expected:rendered rendered

let is_true ?__POS__ ?msg b =
  if not b then fail_equality ?__POS__ ?msg ~expected:"true" "false"

let is_false ?__POS__ ?msg b =
  if b then fail_equality ?__POS__ ?msg ~expected:"false" "true"

let is_none ?__POS__ ?msg ?pp = function
  | None -> ()
  | Some v ->
      fail_equality ?__POS__ ?msg ~expected:"None" ("Some " ^ rendering pp v)

(* Unwrapping *)

let require_some ?__POS__ ?msg = function
  | Some v -> v
  | None -> fail_equality ?__POS__ ?msg ~expected:"Some _" "None"

let require_ok ?__POS__ ?msg ?pp = function
  | Ok v -> v
  | Error e ->
      fail_equality ?__POS__ ?msg ~expected:"Ok _" ("Error " ^ rendering pp e)

let require_error ?__POS__ ?msg ?pp = function
  | Error e -> e
  | Ok v ->
      fail_equality ?__POS__ ?msg ~expected:"Error _" ("Ok " ^ rendering pp v)

let is_some ?__POS__ ?msg o = ignore (require_some ?__POS__ ?msg o)
let is_ok ?__POS__ ?msg ?pp r = ignore (require_ok ?__POS__ ?msg ?pp r)
let is_error ?__POS__ ?msg ?pp r = ignore (require_error ?__POS__ ?msg ?pp r)

let require_match ?__POS__ ?msg ?pp extract v =
  match extract v with
  | Some b -> b
  | None -> fail_predicate ?__POS__ ?msg ~claim:"a match" (rendering pp v)

(* Predicates *)

let satisfies ?__POS__ ?msg ?(claim = "value satisfying the predicate") t pred v
    =
  if not (pred v) then
    fail_predicate ?__POS__ ?msg ~claim (Testable.to_string t v)

(* A [Failure.Containment] holds byte offsets into a string, so membership in
   a list is a predicate. *)
let mem ?__POS__ ?msg t x xs =
  if not (List.exists (Testable.equal t x) xs) then
    fail_predicate ?__POS__ ?msg
      ~claim:(Pp.str "a list containing %s" (Testable.to_string t x))
      (Testable.to_string (Testable.list t) xs)

(* Orders *)

let ordered verb ~relation ~holds ?__POS__ ?msg t ~than v =
  match Testable.compare t with
  | None ->
      invalid_arg
        (Pp.str
           "Windtrap.%s: the witness has no order; give it one with \
            Testable.with_compare"
           verb)
  | Some compare ->
      if not (holds (compare v than)) then
        fail_predicate ?__POS__ ?msg
          ~claim:(Pp.str "%s %s" relation (Testable.to_string t than))
          (Testable.to_string t v)

let less ?__POS__ ?msg t ~than v =
  ordered "less" ~relation:"less than"
    ~holds:(fun c -> c < 0)
    ?__POS__ ?msg t ~than v

let at_most ?__POS__ ?msg t ~than v =
  ordered "at_most" ~relation:"at most"
    ~holds:(fun c -> c <= 0)
    ?__POS__ ?msg t ~than v

let greater ?__POS__ ?msg t ~than v =
  ordered "greater" ~relation:"greater than"
    ~holds:(fun c -> c > 0)
    ?__POS__ ?msg t ~than v

let at_least ?__POS__ ?msg t ~than v =
  ordered "at_least" ~relation:"at least"
    ~holds:(fun c -> c >= 0)
    ?__POS__ ?msg t ~than v

(* String containment *)

(* [found_at] is the needle's first occurrence in the whole haystack under
   every demand: "absent" and "present, but elsewhere" are different bugs. *)
let fail_containment ?__POS__ ?msg ~demand ~needle haystack =
  Failure.refuse
    (Failure.containment ?loc:(Loc.resolve ?__POS__ ()) ?msg
       ?found_at:(Text.first_occurrence ~pattern:needle haystack)
       ~demand ~needle ~haystack ())

let contains ?__POS__ ?msg ~sub haystack =
  if not (Text.contains_substring ~pattern:sub haystack) then
    fail_containment ?__POS__ ?msg ~demand:Failure.Anywhere ~needle:sub haystack

let not_contains ?__POS__ ?msg ~sub haystack =
  if Text.contains_substring ~pattern:sub haystack then
    fail_containment ?__POS__ ?msg ~demand:Failure.Anywhere ~needle:sub haystack

let starts_with ?__POS__ ?msg ~prefix haystack =
  if not (String.starts_with ~prefix haystack) then
    fail_containment ?__POS__ ?msg ~demand:Failure.Prefix ~needle:prefix
      haystack

let ends_with ?__POS__ ?msg ~suffix haystack =
  if not (String.ends_with ~suffix haystack) then
    fail_containment ?__POS__ ?msg ~demand:Failure.Suffix ~needle:suffix
      haystack

let in_order ?__POS__ ?msg ~subs haystack =
  if subs = [] then invalid_arg "Windtrap.in_order: subs is empty";
  let rec walk index cursor = function
    | [] -> ()
    | sub :: rest -> (
        match Text.first_occurrence ~start:cursor ~pattern:sub haystack with
        | Some at -> walk (index + 1) (at + String.length sub) rest
        | None ->
            fail_containment ?__POS__ ?msg
              ~demand:(Failure.Ordered { index; resumed_at = cursor })
              ~needle:sub haystack)
  in
  walk 0 0 subs

(* Exceptions *)

(* What [fn] raised, or [None] when it returned. An assertion failure and a
   control are raised again, so a [raises] around an [equal] reports the
   failed [equal], and no predicate intercepts an [exit]. *)
let raised_by fn =
  match Failure.catch fn with
  | Ok _ -> None
  | Error (`Exception raised) -> Some raised
  | Error c -> Failure.reraise c

(* Only [raises] names the [expected] exception, and only one unequal to the
   [raised] one, so a shared constructor carries two different messages. *)
let message_diff expected raised =
  let diff constructor e a =
    Some
      {
        Failure.constructor;
        expected_message = Failure.text e;
        actual_message = Failure.text a;
      }
  in
  match (expected, raised) with
  | Some (Invalid_argument e), Some (Invalid_argument a, _) ->
      diff "Invalid_argument" e a
  | Some (Stdlib.Failure e), Some (Stdlib.Failure a, _) -> diff "Failure" e a
  | Some (Sys_error e), Some (Sys_error a, _) -> diff "Sys_error" e a
  | _ -> None

let fail_raise ?__POS__ ?msg ?expected ~predicate raised =
  Failure.refuse
    (Failure.raised ?loc:(Loc.resolve ?__POS__ ()) ?msg
       ?expected:(Option.map Failure.exn_to_string expected)
       ?actual:(Option.map (fun (exn, _) -> Failure.exn_to_string exn) raised)
       ~predicate
       ?backtrace:
         (Option.map (fun (_, bt) -> Failure.backtrace_to_string bt) raised)
       ?message_diff:(message_diff expected raised)
       ())

let raises ?__POS__ ?msg expected fn =
  match raised_by fn with
  | Some (exn, _) when exn = expected -> ()
  | raised -> fail_raise ?__POS__ ?msg ~expected ~predicate:false raised

let raises_match ?__POS__ ?msg pred fn =
  match raised_by fn with
  | Some (exn, _) when pred exn -> ()
  | raised -> fail_raise ?__POS__ ?msg ~predicate:true raised

module Exn = struct
  let has sub m =
    match sub with
    | None -> true
    | Some pattern -> Text.contains_substring ~pattern m

  let invalid_arg ?sub = function Invalid_argument m -> has sub m | _ -> false
  let failure ?sub = function Stdlib.Failure m -> has sub m | _ -> false
  let sys_error ?sub = function Sys_error m -> has sub m | _ -> false
end

(* Polling *)

let poll_pause = 0.01

(* Each call but the last runs in a scope of its own, which owns what it
   records; the last one's failure is raised again, and so recorded. *)
let eventually ~within fn =
  if not (Float.is_finite within && within > 0.) then
    invalid_arg "eventually: within must be finite and positive";
  let start = Os.counter () in
  let rec poll () =
    match Failure.catch fn with
    | Ok value -> value
    | Error (#Failure.control as c) -> Failure.reraise c
    | Error (#Failure.fault as fault) ->
        let left = within -. Os.count_s start in
        if left <= 0. then Failure.reraise fault
        else begin
          Unix.sleepf (Float.min poll_pause left);
          poll ()
        end
  in
  poll ()

(* Escape hatches *)

let fail ?__POS__ msg =
  Failure.refuse (Failure.message ?loc:(Loc.resolve ?__POS__ ()) msg)

let failf ?__POS__ fmt = Format.kasprintf (fun msg -> fail ?__POS__ msg) fmt
let skip ?reason () = raise (Failure.Control (`Skip reason))
