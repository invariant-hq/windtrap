open Windtrap

let write_file path text =
  Out_channel.with_open_text path (fun oc -> Out_channel.output_string oc text)

let token () = Sys.getenv_opt "API_TOKEN"

let a_config_is_written () =
  let path = Filename.concat (temp_dir ()) "config.json" in
  write_file path "{}";
  is_true (Sys.file_exists path)

let the_token_is_read () =
  setenv "API_TOKEN" (Some "t-123");
  equal (option string) (Some "t-123") (token ());
  setenv "API_TOKEN" None;
  equal (option string) None (token ())

let a_build_writes_in_place () =
  chdir (temp_dir ());
  write_file "built.txt" "ok";
  is_true (Sys.file_exists "built.txt")

let process =
  group "process state"
    [
      test "a config is written to a fresh directory" a_config_is_written;
      test "the token is read from the environment" the_token_is_read;
      test "a build writes in the working directory" a_build_writes_in_place;
    ]
