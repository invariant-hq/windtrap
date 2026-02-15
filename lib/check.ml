(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* ───── Types ───── *)

type pos = Failure.pos
type here = Failure.here

(* ───── Internal Helpers ───── *)

let fail_with_values ?here ?pos ?msg ?diff ~expected ~actual default_msg =
  Failure.raise_failure ?here ?pos ~expected ~actual ?diff
    (Option.value ~default:default_msg msg)

let fail_simple ?here ?pos ?msg default_msg =
  Failure.raise_failure ?here ?pos (Option.value ~default:default_msg msg)

(* Shared logic for [some], [ok], and [error]: compare inner values using the
   testable's [check] when available, wrapping result strings with a constructor
   prefix (e.g., "Some ", "Ok ", "Error "). *)
let check_inner ?here ?pos ?msg ~prefix ~fail_msg testable expected actual =
  match Testable.check testable with
  | Some check_fn -> (
      match check_fn expected actual with
      | Testable.Pass -> ()
      | Testable.Fail { expected_str; actual_str; diff } ->
          fail_with_values ?here ?pos ?msg ?diff
            ~expected:(Pp.str "%s%s" prefix expected_str)
            ~actual:(Pp.str "%s%s" prefix actual_str)
            fail_msg)
  | None ->
      if not (Testable.equal testable expected actual) then
        fail_with_values ?here ?pos ?msg
          ~expected:
            (Pp.str "%s%s" prefix
               (Pp.to_string (Testable.pp testable) expected))
          ~actual:
            (Pp.str "%s%s" prefix (Pp.to_string (Testable.pp testable) actual))
          fail_msg

(* ───── Equality Assertions ───── *)

(* Uses the testable's [check] function when available for structured diff
   output (e.g., highlighted character-level diffs for strings). Falls back to
   [equal] + [pp] for testables without a custom checker. *)
let equal ?here ?pos ?msg testable expected actual =
  match Testable.check testable with
  | Some check_fn -> (
      match check_fn expected actual with
      | Testable.Pass -> ()
      | Testable.Fail { expected_str; actual_str; diff } ->
          fail_with_values ?here ?pos ?msg ?diff ~expected:expected_str
            ~actual:actual_str "Values are not equal")
  | None ->
      if not (Testable.equal testable expected actual) then
        fail_with_values ?here ?pos ?msg
          ~expected:(Pp.to_string (Testable.pp testable) expected)
          ~actual:(Pp.to_string (Testable.pp testable) actual)
          "Values are not equal"

let not_equal ?here ?pos ?msg testable expected actual =
  if Testable.equal testable expected actual then
    let value_str = Pp.to_string (Testable.pp testable) expected in
    fail_simple ?here ?pos ?msg
      (Pp.str "Expected values to be different, but both are: %s" value_str)

(* ───── Boolean Assertions ───── *)

let is_true ?here ?pos ?msg = function
  | true -> ()
  | false ->
      fail_with_values ?here ?pos ?msg ~expected:"true" ~actual:"false"
        "Boolean mismatch"

let is_false ?here ?pos ?msg = function
  | false -> ()
  | true ->
      fail_with_values ?here ?pos ?msg ~expected:"false" ~actual:"true"
        "Boolean mismatch"

(* ───── Option Assertions ───── *)

let is_some ?here ?pos ?msg = function
  | Some _ -> ()
  | None ->
      fail_with_values ?here ?pos ?msg ~expected:"Some _" ~actual:"None"
        "Option mismatch"

let is_none ?here ?pos ?msg = function
  | None -> ()
  | Some _ ->
      fail_with_values ?here ?pos ?msg ~expected:"None" ~actual:"Some _"
        "Option mismatch"

let some ?here ?pos ?msg testable expected = function
  | None ->
      fail_with_values ?here ?pos ?msg
        ~expected:
          (Pp.str "Some %s" (Pp.to_string (Testable.pp testable) expected))
        ~actual:"None" "Option mismatch"
  | Some actual ->
      check_inner ?here ?pos ?msg ~prefix:"Some "
        ~fail_msg:"Option values are not equal" testable expected actual

(* ───── Result Assertions ───── *)

let is_ok ?here ?pos ?msg = function
  | Ok _ -> ()
  | Error _ ->
      fail_with_values ?here ?pos ?msg ~expected:"Ok _" ~actual:"Error _"
        "Result mismatch"

let is_error ?here ?pos ?msg = function
  | Error _ -> ()
  | Ok _ ->
      fail_with_values ?here ?pos ?msg ~expected:"Error _" ~actual:"Ok _"
        "Result mismatch"

let ok ?here ?pos ?msg testable expected = function
  | Error _ ->
      fail_with_values ?here ?pos ?msg
        ~expected:
          (Pp.str "Ok %s" (Pp.to_string (Testable.pp testable) expected))
        ~actual:"Error _" "Result mismatch"
  | Ok actual ->
      check_inner ?here ?pos ?msg ~prefix:"Ok "
        ~fail_msg:"Result values are not equal" testable expected actual

let error ?here ?pos ?msg testable expected = function
  | Ok _ ->
      fail_with_values ?here ?pos ?msg
        ~expected:
          (Pp.str "Error %s" (Pp.to_string (Testable.pp testable) expected))
        ~actual:"Ok _" "Result mismatch"
  | Error actual ->
      check_inner ?here ?pos ?msg ~prefix:"Error "
        ~fail_msg:"Error values are not equal" testable expected actual

(* ───── Exception Assertions ───── *)

(* All exception assertions must re-raise [Check_failure] to avoid swallowing
   assertion failures from within the tested function itself. Without this,
   a test using [raises] that internally calls [equal] would silently catch
   the [Check_failure] and report "wrong exception" instead of the real error. *)

let raises ?here ?pos ?msg expected_exn fn =
  try
    let _ = fn () in
    fail_with_values ?here ?pos ?msg
      ~expected:(Pp.str "exception %s" (Printexc.to_string expected_exn))
      ~actual:"no exception" "Exception mismatch"
  with
  | Failure.Check_failure _ as e -> raise e
  | exn when exn = expected_exn -> ()
  | exn ->
      fail_with_values ?here ?pos ?msg
        ~expected:(Printexc.to_string expected_exn)
        ~actual:(Printexc.to_string exn) "Wrong exception raised"

let raises_match ?here ?pos ?msg pred fn =
  try
    let _ = fn () in
    fail_with_values ?here ?pos ?msg ~expected:"an exception"
      ~actual:"no exception" "Exception mismatch"
  with
  | Failure.Check_failure _ as e -> raise e
  | exn when pred exn -> ()
  | exn ->
      fail_with_values ?here ?pos ?msg ~expected:"exception matching predicate"
        ~actual:(Pp.str "exception %s" (Printexc.to_string exn))
        "Exception did not match predicate"

let no_raise ?here ?pos ?msg fn =
  try fn () with
  | Failure.Check_failure _ as e -> raise e
  | exn ->
      fail_with_values ?here ?pos ?msg ~expected:"no exception"
        ~actual:(Pp.str "exception %s" (Printexc.to_string exn))
        "Unexpected exception"

let raises_invalid_arg ?here ?pos ?msg expected_msg fn =
  try
    let _ = fn () in
    fail_with_values ?here ?pos ?msg
      ~expected:(Pp.str "Invalid_argument %S" expected_msg)
      ~actual:"no exception" "Exception mismatch"
  with
  | Failure.Check_failure _ as e -> raise e
  | Invalid_argument actual_msg when String.equal actual_msg expected_msg -> ()
  | Invalid_argument actual_msg ->
      fail_with_values ?here ?pos ?msg
        ~expected:(Pp.str "Invalid_argument %S" expected_msg)
        ~actual:(Pp.str "Invalid_argument %S" actual_msg)
        "Wrong exception message"
  | exn ->
      fail_with_values ?here ?pos ?msg
        ~expected:(Pp.str "Invalid_argument %S" expected_msg)
        ~actual:(Printexc.to_string exn) "Wrong exception raised"

let raises_failure ?here ?pos ?msg expected_msg fn =
  try
    let _ = fn () in
    fail_with_values ?here ?pos ?msg
      ~expected:(Pp.str "Failure %S" expected_msg)
      ~actual:"no exception" "Exception mismatch"
  with
  | Failure.Check_failure _ as e -> raise e
  | Stdlib.Failure actual_msg when String.equal actual_msg expected_msg -> ()
  | Stdlib.Failure actual_msg ->
      fail_with_values ?here ?pos ?msg
        ~expected:(Pp.str "Failure %S" expected_msg)
        ~actual:(Pp.str "Failure %S" actual_msg)
        "Wrong exception message"
  | exn ->
      fail_with_values ?here ?pos ?msg
        ~expected:(Pp.str "Failure %S" expected_msg)
        ~actual:(Printexc.to_string exn) "Wrong exception raised"

(* ───── Custom Failures ───── *)

let fail ?here ?pos msg = Failure.raise_failure ?here ?pos msg
let failf ?here ?pos fmt = Format.kasprintf (fail ?here ?pos) fmt
