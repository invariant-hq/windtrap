(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Scratch = struct
  (* [lstat], not [Sys.is_directory]: the runner leaves a [latest] symlink
     in every log directory, and following it would delete outside the
     scratch tree. *)
  let rec remove_tree path =
    match Unix.lstat path with
    | exception Unix.Unix_error _ -> ()
    | { Unix.st_kind = Unix.S_DIR; _ } -> (
        Array.iter
          (fun name -> remove_tree (Filename.concat path name))
          (try Sys.readdir path with Sys_error _ -> [||]);
        try Unix.rmdir path with Unix.Unix_error _ -> ())
    | _ -> ( try Unix.unlink path with Unix.Unix_error _ -> ())

  (* One [at_exit] for every directory, registered as the library
     initialises. [exit] runs the newest functions first, and a run's exit
     guard stops it with an exception: a removal registered after the guard
     would run at every [exit] that a run intercepts, and one registered
     before it never runs early. Only the creating process removes a
     directory: a forked child that leaves through [exit] would delete the
     tree its parent is still using. *)
  let made = ref []

  let () =
    at_exit (fun () ->
        let self = Unix.getpid () in
        List.iter
          (fun (owner, path) -> if owner = self then remove_tree path)
          !made)

  let dir prefix =
    (* [Filename.temp_dir] is OCaml 5.1; the project supports 5.0. *)
    let path = Filename.temp_file prefix "" in
    Sys.remove path;
    Unix.mkdir path 0o700;
    made := (Unix.getpid (), path) :: !made;
    path
end

module Child = struct
  let inherited =
    [ "PATH"; "HOME"; "TMPDIR"; "TEMP"; "TMP"; "SYSTEMROOT"; "LANG"; "LC_ALL" ]

  let environment bindings =
    let from_parent =
      List.filter_map
        (fun name -> Option.map (fun v -> (name, v)) (Sys.getenv_opt name))
        inherited
    in
    let all =
      List.fold_left
        (fun acc (name, value) -> (name, value) :: List.remove_assoc name acc)
        []
        (from_parent @ [ ("WINDTRAP_COLOR", "never") ] @ bindings)
    in
    Array.of_list (List.rev_map (fun (n, v) -> n ^ "=" ^ v) all)

  type result = { status : Unix.process_status; out : string; err : string }

  let read_file path = In_channel.with_open_bin path In_channel.input_all

  (* The streams go to files, not pipes: a child that fills one pipe while
     the parent reads the other would block them both. [cwd] is entered by
     the parent around the spawn, since [create_process_env] has no
     directory of its own and [fork] does not exist on Windows. *)
  let run ?cwd ?(env = []) exe args =
    let out_path = Filename.temp_file "windtrap-child" ".out" in
    let err_path = Filename.temp_file "windtrap-child" ".err" in
    let open_out path =
      Unix.openfile path [ Unix.O_WRONLY; Unix.O_TRUNC; Unix.O_CLOEXEC ] 0o600
    in
    let stdin = Unix.openfile Filename.null [ Unix.O_RDONLY; Unix.O_CLOEXEC ] 0
    and out = open_out out_path
    and err = open_out err_path in
    let spawn () =
      Unix.create_process_env exe
        (Array.of_list (exe :: args))
        (environment env) stdin out err
    in
    let pid =
      Fun.protect
        ~finally:(fun () -> List.iter Unix.close [ stdin; out; err ])
        (fun () ->
          match cwd with
          | None -> spawn ()
          | Some dir ->
              let here = Sys.getcwd () in
              Sys.chdir dir;
              Fun.protect ~finally:(fun () -> Sys.chdir here) spawn)
    in
    let _, status = Unix.waitpid [] pid in
    let result =
      { status; out = read_file out_path; err = read_file err_path }
    in
    Sys.remove out_path;
    Sys.remove err_path;
    result

  let exit_code r =
    match r.status with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
        invalid_arg "Child.exit_code: the child did not exit"
end
