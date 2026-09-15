(* Everything that constrains Slug, in one file — its own suite: the
   laws first (the normative core), then the specified points. Slug's
   known bug lives in ../failures/issue_1.ml, not here. *)

open Windtrap
module Slug = Windtrap_example_blueprint.Slug

let out_alnum c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')

let () =
  exit
  @@ run "slug"
       [
         group "slugify"
           [
             prop "is idempotent" Gen.string (fun s ->
                 equal string (Slug.slugify s) (Slug.slugify (Slug.slugify s)));
             prop "emits lowercase alphanumerics and single inner dashes"
               Gen.string (fun s ->
                 let out = Slug.slugify s in
                 satisfies ~msg:"chars are [a-z0-9-]" string
                   (String.for_all (fun c -> out_alnum c || c = '-'))
                   out;
                 not_contains ~sub:"--" out;
                 satisfies ~msg:"no leading or trailing dash" string
                   (fun o ->
                     o = "" || (o.[0] <> '-' && o.[String.length o - 1] <> '-'))
                   out);
             cases "specified points"
               ~name:(fun (input, _) -> Printf.sprintf "%S" input)
               [
                 ("Hello, World!", "hello-world");
                 ("  OCaml 5.x  ", "ocaml-5-x");
                 ("a--b", "a-b");
                 ("", "");
                 ("---", "");
                 ("MiXeD", "mixed");
                 (* The alphabet boundaries: without this row, the mutation
                 loop reports boundary survivors (c <= 'z' vs c < 'z' and
                 friends) — every character class edge in one input. *)
                 ("Az Za 09", "az-za-09");
               ]
               (fun (input, expected) ->
                 equal string expected (Slug.slugify input));
           ];
       ]
