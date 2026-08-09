(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Tag = Windtrap.Private.Tag

let tests =
  [
    test "tag sets" (fun () ->
        is_true ~msg:"empty has no tags" (Tag.is_empty Tag.empty);
        is_false ~msg:"a populated set is not empty"
          (Tag.is_empty (Tag.of_list [ "a" ]));
        is_true ~msg:"of_list mem" (Tag.mem "a" (Tag.of_list [ "a"; "b" ]));
        is_false ~msg:"mem absent" (Tag.mem "c" (Tag.of_list [ "a"; "b" ]));
        let u = Tag.union (Tag.of_list [ "a" ]) (Tag.of_list [ "b" ]) in
        is_true ~msg:"union keeps the left side" (Tag.mem "a" u);
        is_true ~msg:"union keeps the right side" (Tag.mem "b" u);
        is_false ~msg:"union invents nothing" (Tag.mem "c" u);
        is_true ~msg:"union with empty is identity"
          (Tag.mem "a" (Tag.union Tag.empty (Tag.of_list [ "a" ])));
        equal ~msg:"well-known slow" string "slow" Tag.slow;
        equal ~msg:"well-known disabled" string "disabled" Tag.disabled);
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
        let p = Tag.require "net" Tag.default_predicate in
        is_false ~msg:"require rejects missing tag" (Tag.accepts p Tag.empty);
        is_true ~msg:"require accepts present tag"
          (Tag.accepts p (Tag.of_list [ "net" ]));
        is_true ~msg:"require accepts superset"
          (Tag.accepts p (Tag.of_list [ "net"; "x" ]));
        let p = Tag.require "a" (Tag.require "b" Tag.default_predicate) in
        is_false ~msg:"multiple requires need all"
          (Tag.accepts p (Tag.of_list [ "a" ]));
        is_true ~msg:"multiple requires satisfied"
          (Tag.accepts p (Tag.of_list [ "a"; "b" ]));
        let p = Tag.drop Tag.slow Tag.default_predicate in
        is_false ~msg:"drop rejects tagged"
          (Tag.accepts p (Tag.of_list [ "slow" ]));
        is_true ~msg:"drop accepts untagged"
          (Tag.accepts p (Tag.of_list [ "fast" ])));
    test "refining does not lift the default disabled drop" (fun () ->
        is_false ~msg:"--tag keeps disabled dropped"
          (Tag.accepts
             (Tag.require "net" Tag.default_predicate)
             (Tag.of_list [ "net"; Tag.disabled ]));
        is_false ~msg:"--exclude-tag keeps disabled dropped"
          (Tag.accepts
             (Tag.drop "db" Tag.default_predicate)
             (Tag.of_list [ Tag.disabled ])));
    test "last flag wins when a tag is both required and dropped" (fun () ->
        let p = Tag.drop "x" (Tag.require "x" Tag.default_predicate) in
        is_false ~msg:"drop after require rejects the tag"
          (Tag.accepts p (Tag.of_list [ "x" ]));
        is_true ~msg:"drop after require does not still require it"
          (Tag.accepts p Tag.empty);
        let p = Tag.require "x" (Tag.drop "x" Tag.default_predicate) in
        is_true ~msg:"require after drop accepts the tag"
          (Tag.accepts p (Tag.of_list [ "x" ]));
        is_false ~msg:"require after drop still requires it"
          (Tag.accepts p Tag.empty);
        (* Re-enabling disabled tests is expressible. *)
        let p = Tag.require Tag.disabled Tag.default_predicate in
        is_true ~msg:"requiring disabled overrides the default drop"
          (Tag.accepts p (Tag.of_list [ Tag.disabled ])));
  ]
