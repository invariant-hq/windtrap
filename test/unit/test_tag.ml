(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Tag = Windtrap.Private.Tag

let tags = slist string compare

let tests =
  [
    test "tag sets" (fun () ->
        is_true ~msg:"empty has no tags" (Tag.is_empty Tag.empty);
        equal ~msg:"empty to_list" tags [] (Tag.to_list Tag.empty);
        is_true ~msg:"of_list mem" (Tag.mem "a" (Tag.of_list [ "a"; "b" ]));
        is_false ~msg:"mem absent" (Tag.mem "c" (Tag.of_list [ "a"; "b" ]));
        is_true ~msg:"add" (Tag.mem "x" (Tag.add "x" Tag.empty));
        equal ~msg:"of_list dedups and sorts" (list string) [ "a"; "b" ]
          (Tag.to_list (Tag.of_list [ "b"; "a"; "b" ]));
        equal ~msg:"union combines" (list string) [ "a"; "b" ]
          (Tag.to_list (Tag.union (Tag.of_list [ "a" ]) (Tag.of_list [ "b" ])));
        equal ~msg:"union dedups" (list string) [ "a" ]
          (Tag.to_list (Tag.union (Tag.of_list [ "a" ]) (Tag.of_list [ "a" ])));
        equal ~msg:"well-known slow" string "slow" Tag.slow;
        equal ~msg:"well-known disabled" string "disabled" Tag.disabled);
    test "accept_all accepts anything" (fun () ->
        is_true ~msg:"accepts empty" (Tag.accepts Tag.accept_all Tag.empty);
        is_true ~msg:"accepts anything"
          (Tag.accepts Tag.accept_all (Tag.of_list [ "disabled"; "slow" ])));
    test "default_predicate drops disabled only" (fun () ->
        is_true ~msg:"accepts untagged"
          (Tag.accepts Tag.default_predicate Tag.empty);
        is_true ~msg:"accepts ordinary tags"
          (Tag.accepts Tag.default_predicate (Tag.of_list [ "slow" ]));
        is_false ~msg:"drops disabled"
          (Tag.accepts Tag.default_predicate (Tag.of_list [ Tag.disabled ]));
        is_false ~msg:"drops disabled among others"
          (Tag.accepts Tag.default_predicate
             (Tag.of_list [ "a"; Tag.disabled ])));
    test "require and drop semantics" (fun () ->
        let p = Tag.require "net" Tag.accept_all in
        is_false ~msg:"require rejects missing tag" (Tag.accepts p Tag.empty);
        is_true ~msg:"require accepts present tag"
          (Tag.accepts p (Tag.of_list [ "net" ]));
        is_true ~msg:"require accepts superset"
          (Tag.accepts p (Tag.of_list [ "net"; "x" ]));
        let p = Tag.require "a" (Tag.require "b" Tag.accept_all) in
        is_false ~msg:"multiple requires need all"
          (Tag.accepts p (Tag.of_list [ "a" ]));
        is_true ~msg:"multiple requires satisfied"
          (Tag.accepts p (Tag.of_list [ "a"; "b" ]));
        let p = Tag.drop Tag.slow Tag.accept_all in
        is_false ~msg:"drop rejects tagged"
          (Tag.accepts p (Tag.of_list [ "slow" ]));
        is_true ~msg:"drop accepts untagged"
          (Tag.accepts p (Tag.of_list [ "fast" ])));
    test "last flag wins when a tag is both required and dropped" (fun () ->
        let p = Tag.drop "x" (Tag.require "x" Tag.accept_all) in
        is_false ~msg:"drop after require rejects the tag"
          (Tag.accepts p (Tag.of_list [ "x" ]));
        is_true ~msg:"drop after require does not still require it"
          (Tag.accepts p Tag.empty);
        let p = Tag.require "x" (Tag.drop "x" Tag.accept_all) in
        is_true ~msg:"require after drop accepts the tag"
          (Tag.accepts p (Tag.of_list [ "x" ]));
        is_false ~msg:"require after drop still requires it"
          (Tag.accepts p Tag.empty);
        (* Re-enabling disabled tests is expressible. *)
        let p = Tag.require Tag.disabled Tag.default_predicate in
        is_true ~msg:"requiring disabled overrides the default drop"
          (Tag.accepts p (Tag.of_list [ Tag.disabled ])));
  ]
