(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   Verb semantics — expected-before-actual, the payload conventions, and the
   control-exception re-raise guard in the exception verbs — adapted from
   windtrap v1's lib/check.ml.
  ---------------------------------------------------------------------------*)

type pos = Loc.pos
type 'a printer = Format.formatter -> 'a -> unit
type 'a testable = 'a Testable.t

(* ───── Failure construction ─────

   Every failing verb builds one Failure.t and raises Check_failure. The
   location comes from Loc.resolve: [?pos] wins, else call-stack capture,
   else none. Payload strings are bounded by the Failure constructors. *)

let fail_equality ?pos ?msg ?not_ ?claim ~expected ~actual () =
  raise
    (Failure.Check_failure
       (Failure.equality ?loc:(Loc.resolve ?pos ()) ?msg ?not_ ?claim ~expected
          ~actual ()))

let fail_raise ?pos ?msg ?expected ?actual ?predicate ?backtrace
    ?same_constructor ?expected_message ?actual_message () =
  raise
    (Failure.Check_failure
       (Failure.raised ?loc:(Loc.resolve ?pos ()) ?msg ?expected ?actual
          ?predicate ?backtrace ?same_constructor ?expected_message
          ?actual_message ()))

let abstract = "<abstract>"

(* ───── Comparisons ───── *)

let equal ?pos ?msg t expected actual =
  if not (Testable.equal t expected actual) then
    fail_equality ?pos ?msg
      ~expected:(Testable.to_string t expected)
      ~actual:(Testable.to_string t actual)
      ()

let not_equal ?pos ?msg t a b =
  if Testable.equal t a b then
    (* One rendering, stored on both sides: the witness equality may be
       coarser than printing (float tolerance), and renderers print the
       value once. *)
    let rendered = Testable.to_string t a in
    fail_equality ?pos ?msg ~not_:true ~expected:rendered ~actual:rendered ()

(* ───── Booleans ───── *)

let is_true ?pos ?msg b =
  if not b then fail_equality ?pos ?msg ~expected:"true" ~actual:"false" ()

let is_false ?pos ?msg b =
  if b then fail_equality ?pos ?msg ~expected:"false" ~actual:"true" ()

(* ───── String containment ───── *)

let fail_containment ?pos ?msg ?found_at ~expected ~needle ~haystack () =
  raise
    (Failure.Check_failure
       (Failure.containment ?loc:(Loc.resolve ?pos ()) ?msg ?found_at ~expected
          ~needle ~haystack ()))

let contains ?pos ?msg ~sub haystack =
  match Text.first_occurrence ~pattern:sub haystack with
  | Some _ -> ()
  | None ->
      fail_containment ?pos ?msg
        ~expected:(Pp.str "string containing %S" sub)
        ~needle:sub ~haystack ()

let not_contains ?pos ?msg ~sub haystack =
  match Text.first_occurrence ~pattern:sub haystack with
  | None -> ()
  | Some found_at ->
      fail_containment ?pos ?msg ~found_at
        ~expected:(Pp.str "string not containing %S" sub)
        ~needle:sub ~haystack ()

(* ───── Predicates ───── *)

let satisfies ?pos ?msg t pred v =
  if not (pred v) then
    fail_equality ?pos ?msg ~claim:Failure.Satisfies
      ~expected:"value satisfying the predicate"
      ~actual:(Testable.to_string t v) ()

(* ───── Unwrapping ───── *)

let require_some ?pos ?msg = function
  | Some v -> v
  | None -> fail_equality ?pos ?msg ~expected:"Some _" ~actual:"None" ()

let require_ok ?pos ?msg ?pp_error = function
  | Ok v -> v
  | Error e ->
      let rendered =
        match pp_error with Some pp -> Pp.to_string pp e | None -> abstract
      in
      fail_equality ?pos ?msg ~expected:"Ok _" ~actual:("Error " ^ rendered) ()

let require_error ?pos ?msg ?pp_ok = function
  | Error e -> e
  | Ok v ->
      let rendered =
        match pp_ok with Some pp -> Pp.to_string pp v | None -> abstract
      in
      fail_equality ?pos ?msg ~expected:"Error _" ~actual:("Ok " ^ rendered) ()

let require_match ?pos ?msg ?pp extract v =
  match extract v with
  | Some b -> b
  | None ->
      let rendered =
        match pp with Some pp -> Pp.to_string pp v | None -> abstract
      in
      fail_equality ?pos ?msg ~claim:Failure.Matches ~expected:"a match"
        ~actual:rendered ()

(* ───── Exceptions ─────

   The control exceptions are re-raised from inside the thunk: without the
   guard, a [raises] over code that itself calls [equal] would swallow the
   assertion failure and report "wrong exception" instead of the real
   error (v1's guard). *)

(* The exception's message payload, for the stdlib's string-carrying
   exceptions — the enrichment that lets renderers diff messages instead of
   near-identical renderings. ([Stdlib.Failure] is qualified for the reader:
   windtrap's [Failure] module shadows only the module namespace, not the
   exception constructor.) *)
let exn_message = function
  | Invalid_argument m | Stdlib.Failure m | Sys_error m -> Some m
  | _ -> None

let raises ?pos ?msg expected_exn fn =
  match fn () with
  | _ ->
      fail_raise ?pos ?msg
        ~expected:(Printexc.to_string expected_exn)
        ?expected_message:(exn_message expected_exn) ()
  | exception
      ((Failure.Check_failure _ | Failure.Skip_test _ | Failure.Timeout _) as e)
    ->
      raise e
  | exception raised ->
      let backtrace = Failure.recorded_backtrace () in
      if raised <> expected_exn then
        (* Equality already ruled out, so same constructor means "right
           exception, wrong payload" — recorded for the message diff. *)
        let same_constructor =
          Printexc.exn_slot_id raised = Printexc.exn_slot_id expected_exn
        in
        fail_raise ?pos ?msg
          ~expected:(Printexc.to_string expected_exn)
          ~actual:(Printexc.to_string raised)
          ?backtrace ~same_constructor
          ?expected_message:(exn_message expected_exn)
          ?actual_message:(exn_message raised) ()

let raises_match ?pos ?msg pred fn =
  match fn () with
  | _ -> fail_raise ?pos ?msg ~predicate:true ()
  | exception
      ((Failure.Check_failure _ | Failure.Skip_test _ | Failure.Timeout _) as e)
    ->
      raise e
  | exception raised ->
      let backtrace = Failure.recorded_backtrace () in
      if not (pred raised) then
        fail_raise ?pos ?msg ~predicate:true
          ~actual:(Printexc.to_string raised)
          ?backtrace ?actual_message:(exn_message raised) ()

module Exn = struct
  (* The message constraint resolves once, when the predicate is built:
     passing both constraints is a programmer error reported there, never
     gated on the raised exception's constructor. Defined before the
     predicates so [invalid_arg] below is unambiguously Stdlib's. *)
  let message_check ?substring ?exact () =
    match (substring, exact) with
    | Some _, Some _ ->
        invalid_arg
          "Exn predicate: ~substring and ~exact are mutually exclusive"
    | Some sub, None -> fun m -> Text.contains_substring ~pattern:sub m
    | None, Some exact -> String.equal exact
    | None, None -> Fun.const true

  let invalid_arg ?substring ?exact =
    let ok = message_check ?substring ?exact () in
    function Invalid_argument m -> ok m | _ -> false

  let failure ?substring ?exact =
    let ok = message_check ?substring ?exact () in
    function Stdlib.Failure m -> ok m | _ -> false
end

(* ───── Escape hatches ───── *)

let fail ?pos msg =
  raise (Failure.Check_failure (Failure.message ?loc:(Loc.resolve ?pos ()) msg))

let failf ?pos fmt = Format.kasprintf (fun msg -> fail ?pos msg) fmt
let skip ?reason () = raise (Failure.Skip_test reason)
