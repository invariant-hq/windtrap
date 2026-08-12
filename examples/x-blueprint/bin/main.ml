let () =
  match Array.to_list Sys.argv with
  | _ :: (_ :: _ as args) ->
      print_endline
        (Windtrap_example_blueprint.Slug.slugify (String.concat " " args))
  | _ ->
      prerr_endline "usage: slug TEXT...";
      exit 2
