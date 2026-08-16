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

(* Failure construction

   Every failing verb builds one Failure.t and raises Check_failure. The
   location comes from Loc.resolve: [?pos] wins, else call-stack capture,
   else none. Payload strings are bounded by the Failure constructors. *)

let fail_equality ?pos ?msg ?not_ ~expected ~actual () =
  raise
    (Failure.Check_failure
       (Failure.equality ?loc:(Loc.resolve ?pos ()) ?msg ?not_ ~expected ~actual
          ()))

(* Shared by [satisfies] and [require_match]: the claim sentence is the
   whole of the difference between them. *)
let fail_predicate ?pos ?msg ~claim value =
  raise
    (Failure.Check_failure
       (Failure.predicate ?loc:(Loc.resolve ?pos ()) ?msg ~claim value))

let fail_raise ?pos ?msg ?expected ?actual ?predicate ?backtrace ?message_diff
    () =
  raise
    (Failure.Check_failure
       (Failure.raised ?loc:(Loc.resolve ?pos ()) ?msg ?expected ?actual
          ?predicate ?backtrace ?message_diff ()))

let abstract = "<abstract>"

(* Comparisons *)

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

(* Booleans *)

let is_true ?pos ?msg b =
  if not b then fail_equality ?pos ?msg ~expected:"true" ~actual:"false" ()

let is_false ?pos ?msg b =
  if b then fail_equality ?pos ?msg ~expected:"false" ~actual:"true" ()

(* String containment *)

let fail_containment ?pos ?msg ?found_at ?demand ~claim ~needle ~haystack () =
  raise
    (Failure.Check_failure
       (Failure.containment ?loc:(Loc.resolve ?pos ()) ?msg ?found_at ?demand
          ~claim ~needle ~haystack ()))

(* Counting is the same containment question asked of every occurrence
   rather than the first, so it is the same payload with the two counts
   attached. [found_at] stays the first occurrence: it is what the excerpt
   marks, and the counts are what the verdict says. *)
let contains_count ?pos ?msg ~expected ~sub haystack =
  if expected < 0 then invalid_arg "Check.contains: ~count is negative";
  let found = Text.count_occurrences ~pattern:sub haystack in
  if found <> expected then
    fail_containment ?pos ?msg
      ?found_at:(Text.first_occurrence ~pattern:sub haystack)
      ~demand:(Failure.Counted { expected; found })
      ~claim:(Pp.str "string containing %S exactly %d times" sub expected)
      ~needle:sub ~haystack ()

let contains ?pos ?msg ?count ~sub haystack =
  match count with
  | Some expected -> contains_count ?pos ?msg ~expected ~sub haystack
  | None -> (
      match Text.first_occurrence ~pattern:sub haystack with
      | Some _ -> ()
      | None ->
          fail_containment ?pos ?msg
            ~claim:(Pp.str "string containing %S" sub)
            ~needle:sub ~haystack ())

(* Each element is searched for from the end of the previous element's
   match, so the chain never re-uses bytes and never runs backwards. On a
   break, [found_at] is the element's first occurrence in the WHOLE string
   — [starts_with]'s rule — because "absent" and "present, but too early"
   are different bugs and the second is the one the reader would otherwise
   have to scan a long string to discover. *)
let in_order ?pos ?msg ~subs haystack =
  if subs = [] then invalid_arg "Check.in_order: subs is empty";
  let rec walk index cursor = function
    | [] -> ()
    | sub :: rest -> (
        match Text.first_occurrence ~start:cursor ~pattern:sub haystack with
        | Some at -> walk (index + 1) (at + String.length sub) rest
        | None ->
            fail_containment ?pos ?msg
              ?found_at:(Text.first_occurrence ~pattern:sub haystack)
              ~demand:(Failure.Ordered { index; resumed_at = cursor })
              ~claim:
                (Pp.str "string containing %S at or after byte %d" sub cursor)
              ~needle:sub ~haystack ())
  in
  walk 0 0 subs

let not_contains ?pos ?msg ~sub haystack =
  match Text.first_occurrence ~pattern:sub haystack with
  | None -> ()
  | Some found_at ->
      fail_containment ?pos ?msg ~found_at
        ~claim:(Pp.str "string not containing %S" sub)
        ~needle:sub ~haystack ()

(* Prefix and suffix are containment with a position demanded. Reporting
   the affix's first occurrence when there is one is the whole value here:
   "not found" and "found, but at byte 12" are different bugs, and the
   second is the one a reader would otherwise stare at a long string to
   discover. *)

let starts_with ?pos ?msg ~affix haystack =
  if not (String.starts_with ~prefix:affix haystack) then
    fail_containment ?pos ?msg
      ?found_at:(Text.first_occurrence ~pattern:affix haystack)
      ~claim:(Pp.str "string starting with %S" affix)
      ~needle:affix ~haystack ()

let ends_with ?pos ?msg ~affix haystack =
  if not (String.ends_with ~suffix:affix haystack) then
    fail_containment ?pos ?msg
      ?found_at:(Text.first_occurrence ~pattern:affix haystack)
      ~claim:(Pp.str "string ending with %S" affix)
      ~needle:affix ~haystack ()

(* Membership is containment over a witnessed element type, so it cannot
   reuse [Failure.Containment] — that payload is byte offsets into a
   haystack. The claim sentence names the element, the value is the list
   the reader has to look at. *)
let mem ?pos ?msg t x xs =
  if not (List.exists (Testable.equal t x) xs) then
    fail_predicate ?pos ?msg
      ~claim:(Pp.str "a list containing %s" (Testable.to_string t x))
      (Testable.to_string (Testable.list t) xs)

(* Predicates *)

let satisfies ?pos ?msg t pred v =
  if not (pred v) then
    fail_predicate ?pos ?msg ~claim:"value satisfying the predicate"
      (Testable.to_string t v)

(* Ordering

   [is_true (n > 0)] is the shape these replace, and its failure says
   [expected true / actual false] — the comparison consumed the number.
   Here the bound is the claim and the value is the value, so the report
   reads [expected greater than 0 / actual 0].

   The ordering comes from the witness ([Testable.with_order]) rather than
   an argument, which is what keeps the call shorter than the [is_true] it
   replaces. A witness without one is a programmer error reported where it
   is made, naming the fix. *)

let ordering verb t =
  match Testable.order t with
  | Some compare -> compare
  | None ->
      invalid_arg
        (* Plain ASCII: [Printexc.to_string] renders the payload with [%S],
           which would escape a dash of any other kind into decimal bytes. *)
        (Pp.str
           "Check.%s: this witness carries no ordering; attach one with \
            Testable.with_order"
           verb)

let greater ?pos ?msg t ~than value =
  let compare = ordering "greater" t in
  if compare value than <= 0 then
    fail_predicate ?pos ?msg
      ~claim:(Pp.str "greater than %s" (Testable.to_string t than))
      (Testable.to_string t value)

let greater_equal ?pos ?msg t ~than value =
  let compare = ordering "greater_equal" t in
  if compare value than < 0 then
    fail_predicate ?pos ?msg
      ~claim:(Pp.str "greater than or equal to %s" (Testable.to_string t than))
      (Testable.to_string t value)

let less ?pos ?msg t ~than value =
  let compare = ordering "less" t in
  if compare value than >= 0 then
    fail_predicate ?pos ?msg
      ~claim:(Pp.str "less than %s" (Testable.to_string t than))
      (Testable.to_string t value)

let less_equal ?pos ?msg t ~than value =
  let compare = ordering "less_equal" t in
  if compare value than > 0 then
    fail_predicate ?pos ?msg
      ~claim:(Pp.str "less than or equal to %s" (Testable.to_string t than))
      (Testable.to_string t value)

(* Options

   The shape assertions, for when the value is not wanted: a witness would
   be a printer and an equality for a type these never compare, so they take
   the same optional printer the unwrapping verbs do — "render the branch
   you did not want" — and nothing more. *)

let is_none ?pos ?msg ?pp = function
  | None -> ()
  | Some v ->
      let rendered =
        match pp with Some pp -> Pp.to_string pp v | None -> abstract
      in
      fail_equality ?pos ?msg ~expected:"None" ~actual:("Some " ^ rendered) ()

(* No [?pp]: the failing side is [None], which has nothing to render. *)
let is_some ?pos ?msg = function
  | Some _ -> ()
  | None -> fail_equality ?pos ?msg ~expected:"Some _" ~actual:"None" ()

(* Unwrapping *)

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
      fail_predicate ?pos ?msg ~claim:"a match" rendered

(* Exceptions

   The control exceptions are re-raised from inside the thunk: without the
   guard, a [raises] over code that itself calls [equal] would swallow the
   assertion failure and report "wrong exception" instead of the real
   error (v1's guard). *)

(* The exception's constructor name and message payload, for the stdlib's
   string-carrying exceptions — the only ones whose message a renderer can
   diff. ([Stdlib.Failure] is qualified for the reader: windtrap's [Failure]
   module shadows only the module namespace, not the exception
   constructor.) *)
let exn_message = function
  | Invalid_argument m -> Some ("Invalid_argument", m)
  | Stdlib.Failure m -> Some ("Failure", m)
  | Sys_error m -> Some ("Sys_error", m)
  | _ -> None

(* The message diff, when the two exceptions differ only in their message:
   here, where both exceptions are in hand, the constructor is named rather
   than recovered from a rendering. *)
let message_diff expected_exn raised =
  match (exn_message expected_exn, exn_message raised) with
  | Some (constructor, expected_message), Some (ctor, actual_message)
    when String.equal constructor ctor
         && not (String.equal expected_message actual_message) ->
      Some { Failure.constructor; expected_message; actual_message }
  | _ -> None

let raises ?pos ?msg expected_exn fn =
  match fn () with
  | _ -> fail_raise ?pos ?msg ~expected:(Printexc.to_string expected_exn) ()
  | exception
      ((Failure.Check_failure _ | Failure.Skip_test _ | Failure.Timeout _) as e)
    ->
      raise e
  | exception raised ->
      let backtrace = Failure.recorded_backtrace () in
      if raised <> expected_exn then
        fail_raise ?pos ?msg
          ~expected:(Printexc.to_string expected_exn)
          ~actual:(Printexc.to_string raised)
          ?backtrace
          ?message_diff:(message_diff expected_exn raised)
          ()

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
          ?backtrace ()

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

  let sys_error ?substring ?exact =
    let ok = message_check ?substring ?exact () in
    function Sys_error m -> ok m | _ -> false
end

(* Escape hatches *)

let fail ?pos msg =
  raise (Failure.Check_failure (Failure.message ?loc:(Loc.resolve ?pos ()) msg))

let failf ?pos fmt = Format.kasprintf (fun msg -> fail ?pos msg) fmt
let skip ?reason () = raise (Failure.Skip_test reason)
