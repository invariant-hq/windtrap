(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* [transcript.exe PAGE], run by a dune rule, prints the Markdown page PAGE
   with every marked block regenerated from the examples, for dune to diff
   with PAGE. The examples are those of the build context that dune names
   in INSIDE_DUNE. The markers, the commands a session may hold and the
   environment they run in are stated in doc/dev/testing.md, "The manual's
   transcripts". *)

let strf = Printf.sprintf

exception Error of string

let fail fmt = Printf.ksprintf (fun msg -> raise (Error msg)) fmt

let read_file path =
  try In_channel.with_open_bin path In_channel.input_all
  with Sys_error e ->
    fail "cannot read %s (the page's rule must depend on it)" e

(* The lines of [s], each without its newline; a last line with no newline
   is a line. *)
let lines s =
  match String.split_on_char '\n' s with
  | [] -> []
  | l -> ( match List.rev l with "" :: rest -> List.rev rest | _ -> l)

(* Markers *)

type marker =
  | File of string  (** the block is this file *)
  | Run of { dir : string; example : string }
      (** the block is a session in [dir], printed as if in [example] *)

let words s = List.filter (fun w -> w <> "") (String.split_on_char ' ' s)

let marker line =
  let line = String.trim line in
  let is_comment =
    String.starts_with ~prefix:"<!--" line
    && String.ends_with ~suffix:"-->" line
    && String.length line >= 7
  in
  if not is_comment then None
  else
    match words (String.sub line 4 (String.length line - 7)) with
    | [ "file"; path ] -> Some (File path)
    | [ "run"; dir ] -> Some (Run { dir; example = dir })
    | [ "run"; dir; "as"; example ] -> Some (Run { dir; example })
    | ("file" | "run") :: _ ->
        fail
          "malformed marker %s: expected <!-- file PATH -->, <!-- run DIR --> \
           or <!-- run DIR as EXAMPLE -->"
          line
    | _ -> None

(* Dune's test stanzas

   [dune runtest] runs the (test) and (tests) stanzas of a dune file, and
   frames the output of one that fails with the location of its name. A
   dune file is read as the s-expressions it is: atoms, quoted strings,
   lists and [;] comments. *)

type atom = { text : string; line : int; first : int }
type sexp = Atom of atom | List of sexp list

let parse_sexps text =
  let n = String.length text in
  let line = ref 1 and bol = ref 0 in
  let newline i =
    incr line;
    bol := i + 1
  in
  let is_atom_char = function
    | ' ' | '\t' | '\r' | '\n' | '(' | ')' | '"' | ';' -> false
    | _ -> true
  in
  let rec items i acc =
    if i >= n then (List.rev acc, i)
    else
      match text.[i] with
      | '\n' ->
          newline i;
          items (i + 1) acc
      | ' ' | '\t' | '\r' -> items (i + 1) acc
      | ';' ->
          let j = try String.index_from text i '\n' with Not_found -> n in
          items j acc
      | ')' -> (List.rev acc, i)
      | '(' ->
          let inner, j = items (i + 1) [] in
          if j >= n then fail "unbalanced parenthesis in a dune file";
          items (j + 1) (List inner :: acc)
      | '"' ->
          let rec close j =
            if j >= n then fail "unterminated string in a dune file"
            else if text.[j] = '\\' then close (j + 2)
            else if text.[j] = '"' then j
            else (
              if text.[j] = '\n' then newline j;
              close (j + 1))
          in
          let j = close (i + 1) in
          let atom =
            Atom
              {
                text = String.sub text (i + 1) (j - i - 1);
                line = !line;
                first = i - !bol;
              }
          in
          items (j + 1) (atom :: acc)
      | _ ->
          let j = ref i in
          while !j < n && is_atom_char text.[!j] do
            incr j
          done;
          let atom =
            Atom
              {
                text = String.sub text i (!j - i);
                line = !line;
                first = i - !bol;
              }
          in
          items !j (atom :: acc)
  in
  let sexps, i = items 0 [] in
  if i < n then fail "unbalanced parenthesis in a dune file";
  sexps

(* The name atoms of the test stanzas of [sexps], in file order. *)
let test_names sexps =
  let atoms = List.filter_map (function Atom a -> Some a | List _ -> None) in
  let field name = function
    | List (Atom { text; _ } :: values) when text = name -> Some values
    | Atom _ | List _ -> None
  in
  let stanza = function
    | List (Atom { text = "test"; _ } :: fields) -> (
        match List.find_map (field "name") fields with
        | Some [ Atom a ] -> [ a ]
        | Some _ | None -> [])
    | List (Atom { text = "tests"; _ } :: fields) ->
        atoms (Option.value ~default:[] (List.find_map (field "names") fields))
    | Atom _ | List _ -> []
  in
  List.concat_map stanza sexps

(* What dune prints above the output of a test that exits nonzero. *)
let dune_location ~file ~text { text = name; line; first } =
  let source = List.nth (lines text) (line - 1) in
  let gutter = strf "%d | " line in
  strf "File \"%s\", line %d, characters %d-%d:\n%s%s\n%s%s\n" file line first
    (first + String.length name)
    gutter source
    (String.make (String.length gutter + first) ' ')
    (String.make (String.length name) '^')

(* Commands *)

(* The words of a command line as a POSIX shell splits it, where the only
   quoting is ['...'] and ["..."] and nothing expands. A byte the shell
   would give a meaning to is refused rather than guessed. *)
let shell_words cmd =
  let n = String.length cmd in
  let b = Buffer.create 16 in
  let rec word i acc in_word =
    if i >= n then List.rev (if in_word then Buffer.contents b :: acc else acc)
    else
      match cmd.[i] with
      | ' ' | '\t' ->
          let acc = if in_word then Buffer.contents b :: acc else acc in
          Buffer.clear b;
          word (i + 1) acc false
      | ('\'' | '"') as q ->
          let j =
            try String.index_from cmd (i + 1) q
            with Not_found -> fail "unterminated quote in: %s" cmd
          in
          let quoted = String.sub cmd (i + 1) (j - i - 1) in
          if q = '"' && String.exists (fun c -> String.contains "$`\\" c) quoted
          then fail "the shell would expand %S in: %s" quoted cmd;
          Buffer.add_string b quoted;
          word (j + 1) acc true
      | '|' | '&' | ';' | '<' | '>' | '$' | '`' | '\\' | '*' | '?' | '#' ->
          fail "%C needs a shell, which the checker does not run: %s" cmd.[i]
            cmd
      | c ->
          Buffer.add_char b c;
          word (i + 1) acc true
  in
  word 0 [] false

type command =
  | Runtest  (** [dune runtest] or [dune test] *)
  | Exec of { path : string; args : string list }
      (** [dune exec PATH -- ARGS] *)

let is_assignment w =
  match String.index_opt w '=' with
  | None | Some 0 -> false
  | Some i ->
      String.for_all
        (function
          | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true | _ -> false)
        (String.sub w 0 i)

let parse_command cmd =
  let rec assignments acc = function
    | w :: rest when is_assignment w -> assignments (w :: acc) rest
    | rest -> (List.rev acc, rest)
  in
  let env, argv = assignments [] (shell_words cmd) in
  let command =
    match argv with
    | "dune" :: ("runtest" | "test") :: flags
      when List.for_all (String.equal "--force") flags ->
        Runtest
    | "dune" :: "exec" :: path :: rest
      when String.contains path '/' && not (String.starts_with ~prefix:"-" path)
      -> (
        match rest with
        | [] -> Exec { path; args = [] }
        | "--" :: args -> Exec { path; args }
        | _ -> fail "dune exec takes its program's arguments after --: %s" cmd)
    | _ ->
        fail
          "no rule runs %s: a session holds dune runtest, dune test and dune \
           exec PATH, after VAR=value assignments"
          cmd
  in
  (env, command)

(* Running *)

(* The environment of a clean shell under dune: dune's INSIDE_DUNE, and
   PATH, HOME and TMPDIR. Nothing else of the caller's reaches the run, so
   neither a developer's WINDTRAP_* nor CI's CI and GITHUB_ACTIONS changes
   what it prints. *)
let base_env () =
  List.filter_map
    (fun var -> Option.map (fun v -> var ^ "=" ^ v) (Sys.getenv_opt var))
    [ "INSIDE_DUNE"; "PATH"; "HOME"; "TMPDIR" ]

(* Runs [prog] with [argv] from [cwd], standard output and standard error
   on one pipe, so the text is in the order the program wrote it. *)
let spawn ~cwd ~env ~prog argv =
  let r, w = Unix.pipe ~cloexec:true () in
  let pid =
    Fun.protect ~finally:(fun () -> Unix.close w) @@ fun () ->
    let back = Sys.getcwd () in
    Sys.chdir cwd;
    Fun.protect ~finally:(fun () -> Sys.chdir back) @@ fun () ->
    try
      Unix.create_process_env prog (Array.of_list argv) (Array.of_list env)
        Unix.stdin w w
    with Unix.Unix_error (e, _, _) ->
      fail "cannot run %s: %s (the page's rule must depend on it)" prog
        (Unix.error_message e)
  in
  let out = Buffer.create 4096 and chunk = Bytes.create 4096 in
  let rec drain () =
    match Unix.read r chunk 0 (Bytes.length chunk) with
    | 0 -> ()
    | k ->
        Buffer.add_subbytes out chunk 0 k;
        drain ()
  in
  Fun.protect ~finally:(fun () -> Unix.close r) drain;
  match snd (Unix.waitpid [] pid) with
  | Unix.WEXITED code -> (Buffer.contents out, code)
  | Unix.WSIGNALED s | Unix.WSTOPPED s ->
      fail "%s was killed by signal %d" prog s

(* Removes the escape sequences that style a terminal, as dune does to what
   it relays when its own output is not a terminal. *)
let strip_escapes s =
  let n = String.length s in
  let b = Buffer.create n in
  let rec go i =
    if i >= n then ()
    else if s.[i] = '\027' && i + 1 < n && s.[i + 1] = '[' then
      let rec final j =
        if j >= n then j
        else match s.[j] with '@' .. '~' -> j + 1 | _ -> final (j + 1)
      in
      go (final (i + 2))
    else (
      Buffer.add_char b s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents b

let replace_all s ~sub ~by =
  let n = String.length s and k = String.length sub in
  let b = Buffer.create n in
  let rec go i =
    if i >= n then ()
    else if k > 0 && i + k <= n && String.sub s i k = sub then (
      Buffer.add_string b by;
      go (i + k))
    else (
      Buffer.add_char b s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents b

(* The output of one command of a session in [dir], printed as if in
   [example]. [root] is the build context, an absolute path. *)
let run ~root ~dir ~example (assignments, command) =
  let env = assignments @ base_env () in
  let out =
    match command with
    | Runtest ->
        let file = Filename.concat example "dune" in
        let text = read_file (Filename.concat root file) in
        let names = test_names (parse_sexps text) in
        if names = [] then fail "%s declares no test stanza" file;
        let cwd = Filename.concat root dir in
        let test name_atom =
          let exe = name_atom.text ^ ".exe" in
          let out, code =
            spawn ~cwd ~env ~prog:(Filename.concat cwd exe) [ "./" ^ exe ]
          in
          if code = 0 then out else dune_location ~file ~text name_atom ^ out
        in
        String.concat "" (List.map test names)
    | Exec { path; args } ->
        let prefix = example ^ "/" in
        let path =
          if String.starts_with ~prefix path then
            dir ^ "/"
            ^ String.sub path (String.length prefix)
                (String.length path - String.length prefix)
          else path
        in
        let build = Filename.dirname root
        and context = Filename.basename root in
        let source_root = Filename.dirname build in
        let argv0 = strf "./%s/%s/%s" (Filename.basename build) context path in
        fst
          (spawn ~cwd:source_root ~env
             ~prog:(Filename.concat root path)
             (argv0 :: args))
  in
  let out = strip_escapes out in
  let out =
    if dir = example then out
    else replace_all out ~sub:(dir ^ "/") ~by:(example ^ "/")
  in
  lines out

(* Masks

   What varies from one run to the next, masked on both sides when a
   block is compared and never in what the page shows: the number of a
   duration (digits directly followed by the unit [ms] or [s], which
   stays), a seed ([s1:] and sixteen hexadecimal digits), and an absolute
   path under the temporary directory or the repository. *)

let is_word_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '.' -> true
  | _ -> false

let is_digit c = '0' <= c && c <= '9'
let is_hex c = is_digit c || ('a' <= c && c <= 'f')

(* [varying ~roots s i] is the end of what varies at [i] in [s], and its
   mask, if something does. *)
let varying ~roots s i =
  let n = String.length s in
  let starts_word = i = 0 || not (is_word_char s.[i - 1]) in
  let rec skip p j = if j < n && p s.[j] then skip p (j + 1) else j in
  let duration () =
    if not (starts_word && is_digit s.[i]) then None
    else
      let j = skip is_digit i in
      let j =
        if j + 1 < n && s.[j] = '.' && is_digit s.[j + 1] then
          skip is_digit (j + 1)
        else j
      in
      let unit_end =
        if j + 1 < n && s.[j] = 'm' && s.[j + 1] = 's' then Some (j + 2)
        else if j < n && s.[j] = 's' then Some (j + 1)
        else None
      in
      match unit_end with
      | Some e when e >= n || (not (is_word_char s.[e])) || s.[e] = '.' ->
          Some (j, "<number>")
      | Some _ | None -> None
  in
  let seed () =
    if
      starts_word
      && i + 19 <= n
      && String.sub s i 3 = "s1:"
      && String.for_all is_hex (String.sub s (i + 3) 16)
    then Some (i + 19, "s1:<seed>")
    else None
  in
  let path () =
    List.find_map
      (fun r ->
        let k = String.length r in
        if i + k <= n && String.sub s i k = r then
          Some
            ( skip
                (fun c -> c <> ' ' && c <> '\n' && c <> '\'' && c <> '"')
                (i + k),
              "<path>" )
        else None)
      roots
  in
  match path () with
  | Some _ as m -> m
  | None -> ( match seed () with Some _ as m -> m | None -> duration ())

let mask ~roots s =
  let n = String.length s in
  let b = Buffer.create n in
  let rec go i =
    if i >= n then ()
    else
      match varying ~roots s i with
      | Some (j, m) ->
          Buffer.add_string b m;
          go j
      | None ->
          Buffer.add_char b s.[i];
          go (i + 1)
  in
  go 0;
  Buffer.contents b

(* Pages *)

let regenerate ~root ~roots marker block =
  match marker with
  | File path -> lines (read_file (Filename.concat root path))
  | Run { dir; example } ->
      let session = function
        | cmd when String.starts_with ~prefix:"$ " cmd ->
            let text = String.sub cmd 2 (String.length cmd - 2) in
            Some (cmd :: run ~root ~dir ~example (parse_command text))
        | _ -> None
      in
      (match block with
      | first :: _ when not (String.starts_with ~prefix:"$ " first) ->
          fail
            "a session's first line must be a command line, \"$ \" then the \
             command"
      | [] -> fail "a session holds at least one command line"
      | _ -> ());
      List.concat (List.filter_map session block)

let page ~root path =
  let roots =
    List.filter
      (fun r -> String.length r > 1)
      [
        Filename.dirname (Filename.dirname root);
        (let t = Filename.get_temp_dir_name () in
         if String.ends_with ~suffix:"/" t then
           String.sub t 0 (String.length t - 1)
         else t);
      ]
  in
  let src = Array.of_list (lines (read_file path)) in
  let n = Array.length src in
  let out = Buffer.create 4096 in
  let emit l =
    Buffer.add_string out l;
    Buffer.add_char out '\n'
  in
  let rec go i =
    if i >= n then ()
    else
      let at_line f =
        try f () with Error msg -> fail "%s:%d: %s" path (i + 1) msg
      in
      match at_line (fun () -> marker src.(i)) with
      | None ->
          emit src.(i);
          go (i + 1)
      | Some m ->
          let fence = i + 1 in
          if fence >= n || not (String.starts_with ~prefix:"```" src.(fence))
          then
            fail "%s:%d: a marker must stand on the line above a fence" path
              (i + 1);
          let rec close j =
            if j >= n then
              fail "%s:%d: the fence is never closed" path (fence + 1)
            else if src.(j) = "```" then j
            else close (j + 1)
          in
          let last = close (fence + 1) in
          let block =
            Array.to_list (Array.sub src (fence + 1) (last - fence - 1))
          in
          let fresh = at_line (fun () -> regenerate ~root ~roots m block) in
          let same =
            mask ~roots (String.concat "\n" block)
            = mask ~roots (String.concat "\n" fresh)
          in
          List.iter emit
            (src.(i) :: src.(fence) :: (if same then block else fresh));
          emit src.(last);
          go (last + 1)
  in
  go 0;
  Buffer.contents out

(* The build context: dune runs a rule's action with INSIDE_DUNE set to it,
   an absolute path such as [/w/_build/default]. *)
let build_context () =
  match Sys.getenv_opt "INSIDE_DUNE" with
  | Some root when (not (Filename.is_relative root)) && Sys.is_directory root ->
      root
  | Some _ | None ->
      fail
        "INSIDE_DUNE names no build context: the transcripts are the runs dune \
         makes, checked by dune build @runtest"

let () =
  match Sys.argv with
  | [| _; path |] -> (
      match page ~root:(build_context ()) path with
      | text ->
          set_binary_mode_out stdout true;
          print_string text
      | exception Error msg ->
          prerr_endline ("transcript: " ^ msg);
          exit 1)
  | _ ->
      prerr_endline "usage: transcript.exe PAGE";
      exit 2
