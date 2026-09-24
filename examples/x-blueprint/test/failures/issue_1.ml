(* Issue #1: slugify treats UTF-8 letters as separators. "Café" should
   slugify to "café"; today the é is dropped. It is intentionally
   unfixed; this suite demonstrates the backlog convention. *)

open Windtrap
module Slug = Windtrap_example_blueprint.Slug

let () =
  exit
  @@ run "issue-1"
       [
         xfail ~reason:"issue #1"
           (test "keeps UTF-8 letters" (fun () ->
                equal string "café" (Slug.slugify "Café")));
       ]
