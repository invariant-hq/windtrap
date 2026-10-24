(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Lines = Windtrap.Private.Lines

let spec s =
  match Lines.of_spec s with
  | Ok t -> Lines.to_string t
  | Error expected -> "error: " ^ expected

let expected = "error: FILE:A-B,... with 1 <= A <= B (e.g. lib/a.ml:3-7,12)"

(* [--lines] *)

let specs =
  [
    ("one range", "lib/a.ml:3-7", "lib/a.ml:3-7");
    ("one line", "lib/a.ml:4", "lib/a.ml:4");
    ( "bare ranges continue the file",
      "lib/a.ml:3-4,9,12-13",
      "lib/a.ml:3-4,9,12-13" );
    ("several files, sorted", "lib/b.ml:1,lib/a.ml:2", "lib/a.ml:2,lib/b.ml:1");
    ("overlapping and adjacent ranges merge", "a.ml:3-5,4-8,9,11", "a.ml:3-9,11");
    ("a leading ./ is dropped", "./lib/a.ml:2", "lib/a.ml:2");
    ("a path may hold a colon", "C:/x/a.ml:2", "C:/x/a.ml:2");
    ("a bare range before any file", "3-4", expected);
    ("an empty range", "a.ml:7-3", expected);
    ("line 0", "a.ml:0", expected);
    ("no range", "a.ml:", expected);
    ("no file", ":3", expected);
    ("a signed line", "a.ml:+3", expected);
    ("nothing", "", expected);
  ]

let membership () =
  match Lines.of_spec "lib/a.ml:3-5,9" with
  | Error e -> fail e
  | Ok t ->
      equal (list bool)
        [ false; true; true; true; false; true; false; true ]
        [
          Lines.mem t ~file:"lib/a.ml" ~line:2;
          Lines.mem t ~file:"lib/a.ml" ~line:3;
          Lines.mem t ~file:"lib/a.ml" ~line:5;
          Lines.mem t ~file:"./lib/a.ml" ~line:4;
          Lines.mem t ~file:"lib/a.ml" ~line:6;
          Lines.mem t ~file:"lib/a.ml" ~line:9;
          Lines.mem t ~file:"lib/b.ml" ~line:4;
          Lines.mem (Lines.union Lines.empty t) ~file:"lib/a.ml" ~line:9;
        ]

(* [git diff -U0] *)

let diff =
  {|diff --git a/lib/a.ml b/lib/a.ml
index 1111111..2222222 100644
--- a/lib/a.ml
+++ b/lib/a.ml
@@ -3 +3 @@ let f x =
-  x + 1
+  x + 2
@@ -10,0 +11,3 @@ let g y =
+let h z =
+++ z
+  z
@@ -20,2 +23,0 @@ let k = 1
-let gone = 1
-let gone_too = 2
diff --git a/lib/old.ml b/lib/old.ml
deleted file mode 100644
--- a/lib/old.ml
+++ /dev/null
@@ -1,2 +0,0 @@
-let a = 1
-let b = 2
diff --git a/lib/new.ml b/lib/new.ml
new file mode 100644
--- /dev/null
+++ b/lib/new.ml
@@ -0,0 +1,2 @@
+let a = 1
+let b = 2
|}

let diffs =
  [
    ( "a git diff -U0 is the lines it adds or changes",
      diff,
      "lib/a.ml:3,11-13,lib/new.ml:1-2" );
    ( "a header without b/ names its file as written",
      "--- lib/a.ml\n+++ lib/a.ml\n@@ -1 +1,2 @@\n",
      "lib/a.ml:1-2" );
    ("no hunk, no line", "--- a/x.ml\n+++ b/x.ml\n", "");
    ("text that is no diff", "hello\nworld\n", "");
  ]

let () =
  exit
    (run "lines"
       [
         group "Specs"
           [
             cases "of_spec"
               ~name:(fun (n, _, _) -> n)
               specs
               (fun (_, s, e) -> equal string e (spec s));
             test "mem holds the lines of the ranges of a file" membership;
           ];
         group "Diffs"
           [
             cases "of_diff"
               ~name:(fun (n, _, _) -> n)
               diffs
               (fun (_, d, e) ->
                 equal string e (Lines.to_string (Lines.of_diff d)));
           ];
       ])
