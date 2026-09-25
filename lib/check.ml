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
  raise
    (Failure.Check_failure
       (Failure.equality ?loc:(Loc.resolve ?__POS__ ()) ?msg ?not_ ~expected
          ~actual ()))

let fail_predicate ?__POS__ ?msg ~claim value =
  raise
    (Failure.Check_failure
       (Failure.predicate ?loc:(Loc.resolve ?__POS__ ()) ?msg ~claim value))

(* The rejected side of a shape verb, through its [?pp] when it has one. *)
let rendering pp v =
  match pp with Some pp -> Pp.to_string pp v | None -> Pp.abstract

(* Equalities *)

let equal ?__POS__ ?msg t expected actual =
  if not (Testable.equal t expected actual) then
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
  raise
    (Failure.Check_failure
       (Failure.containment ?loc:(Loc.resolve ?__POS__ ()) ?msg
          ?found_at:(Text.first_occurrence ~pattern:needle haystack)
          ~demand ~needle ~haystack ()))

let contains ?__POS__ ?msg ~sub haystack =
  if not (Text.contains_substring ~pattern:sub haystack) then
    fail_containment ?__POS__ ?msg ~demand:Failure.Anywhere ~needle:sub haystack

let not_contains ?__POS__ ?msg ~sub haystack =
  if Text.contains_substring ~pattern:sub haystack then
    fail_containment ?__POS__ ?msg ~demand:Failure.Anywhere ~needle:sub haystack

let starts_with ?__POS__ ?msg ~affix haystack =
  if not (String.starts_with ~prefix:affix haystack) then
    fail_containment ?__POS__ ?msg ~demand:Failure.Prefix ~needle:affix haystack

let ends_with ?__POS__ ?msg ~affix haystack =
  if not (String.ends_with ~suffix:affix haystack) then
    fail_containment ?__POS__ ?msg ~demand:Failure.Suffix ~needle:affix haystack

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
  raise
    (Failure.Check_failure
       (Failure.raised ?loc:(Loc.resolve ?__POS__ ()) ?msg
          ?expected:(Option.map Failure.exn_to_string expected)
          ?actual:
            (Option.map (fun (exn, _) -> Failure.exn_to_string exn) raised)
          ~predicate
          ?backtrace:
            (Option.map (fun (_, bt) -> Failure.backtrace_to_string bt) raised)
          ?message_diff:(message_diff expected raised)
          ()))

let raises ?__POS__ ?msg expected fn =
  match raised_by fn with
  | Some (exn, _) when exn = expected -> ()
  | raised -> fail_raise ?__POS__ ?msg ~expected ~predicate:false raised

let raises_match ?__POS__ ?msg pred fn =
  match raised_by fn with
  | Some (exn, _) when pred exn -> ()
  | raised -> fail_raise ?__POS__ ?msg ~predicate:true raised

module Exn = struct
  let has substring m =
    match substring with
    | None -> true
    | Some pattern -> Text.contains_substring ~pattern m

  let invalid_arg ?substring = function
    | Invalid_argument m -> has substring m
    | _ -> false

  let failure ?substring = function
    | Stdlib.Failure m -> has substring m
    | _ -> false

  let sys_error ?substring = function
    | Sys_error m -> has substring m
    | _ -> false
end

(* Escape hatches *)

let fail ?__POS__ msg =
  raise
    (Failure.Check_failure (Failure.message ?loc:(Loc.resolve ?__POS__ ()) msg))

let failf ?__POS__ fmt = Format.kasprintf (fun msg -> fail ?__POS__ msg) fmt
let skip ?reason () = raise (Failure.Control (`Skip reason))
