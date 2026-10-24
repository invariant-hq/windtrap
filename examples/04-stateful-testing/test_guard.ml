open Windtrap

(* Three spellings of one secret file, and a file that holds no secret. *)
let secrets =
  [ "secrets/api.key"; "./secrets/api.key"; "notes/../secrets/api.key" ]

let path = Gen.of_list ~pp:Format.pp_print_string ("notes.md" :: secrets)
let url = Gen.of_list ~pp:Format.pp_print_string [ "https://example.com/" ]

module Policy = struct
  type t = { mutable secret : string option } (* the first secret read *)

  let create () = { secret = None }

  let read_file m p = function
    | Ok (Guard.Ran _) ->
        if List.mem p secrets && m.secret = None then m.secret <- Some p
    | Ok (Guard.Blocked why) -> failf "read_file %s was blocked: %s" p why
    | Error e -> raise e

  let http_get m _ = function
    | Ok (Guard.Ran _) ->
        Option.iter (failf "http_get ran after %s was read") m.secret
    | Ok (Guard.Blocked why) ->
        if m.secret = None then
          failf "http_get was blocked in a clean session: %s" why
    | Error e -> raise e
end

let session =
  abstract "g" ~pp:(fun ppf m ->
      match m.Policy.secret with
      | None -> Format.pp_print_string ppf "clean"
      | Some p -> Format.fprintf ppf "read %s" p)

let reply = Testable.structural ~pp:Guard.pp_reply

let commands =
  [
    command "create" (Gen.unit @-> makes session) Policy.create Guard.create;
    command "read_file"
      (session ^-> path @-> judges reply)
      Policy.read_file Guard.read_file;
    command "http_get"
      (session ^-> url @-> judges reply)
      Policy.http_get Guard.http_get;
  ]

let guards = group "Guard" [ stateful "keeps its policy" commands ]
let () = exit (run "guard" [ guards ])
