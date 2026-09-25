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
  | File of { path : string; excerpt : (string * string option) option }
      (** the block is this file, or the excerpt [(first, last)]: its lines from
          the first that starts with [first] to the end of the paragraph of the
          next that starts with [last], or of the first *)
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
    | [ "file"; path ] -> Some (File { path; excerpt = None })
    | "file" :: path :: "from" :: (_ :: _ as words) -> (
        let text = String.concat " " in
        match List.find_index (String.equal "to") words with
        | None -> Some (File { path; excerpt = Some (text words, None) })
        | Some i ->
            let first = List.filteri (fun j _ -> j < i) words
            and last = List.filteri (fun j _ -> j > i) words in
            if first = [] || last = [] then
              fail "malformed marker %s: expected from TEXT to TEXT" line;
            Some (File { path; excerpt = Some (text first, Some (text last)) }))
    | [ "run"; dir ] -> Some (Run { dir; example = dir })
    | [ "run"; dir; "as"; example ] -> Some (Run { dir; example })
    | ("file" | "run") :: _ ->
        fail
          "malformed marker %s: expected <!-- file PATH [from TEXT [to TEXT]] \
           -->, <!-- run DIR --> or <!-- run DIR as EXAMPLE -->"
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

(* What [dune runtest] runs for a dune file: a test executable with the
   arguments of its action's [(run %{test} ...)] and the [(diff? a b)]
   that follow it, or the partitions of a library's inline tests. *)
type runnable =
  | Test of { name : atom; args : string list; diffs : (string * string) list }
  | Inline of string  (** the library's name *)

(* The runnables of [sexps], in file order. An action of another shape
   than [(run %{test} ...)], alone or first in a [progn] of [diff?], is
   refused rather than guessed. *)
let runnables sexps =
  let atoms = List.filter_map (function Atom a -> Some a | List _ -> None) in
  let field name = function
    | List (Atom { text; _ } :: values) when text = name -> Some values
    | Atom _ | List _ -> None
  in
  let action (name : atom) = function
    | None -> ([], [])
    | Some
        [
          List (Atom { text = "run"; _ } :: Atom { text = "%{test}"; _ } :: args);
        ] ->
        (List.map (fun (a : atom) -> a.text) (atoms args), [])
    | Some
        [
          List
            (Atom { text = "progn"; _ }
            :: List
                 (Atom { text = "run"; _ }
                 :: Atom { text = "%{test}"; _ }
                 :: args)
            :: diffs);
        ] ->
        let diff = function
          | List [ Atom { text = "diff?"; _ }; Atom a; Atom b ] ->
              (a.text, b.text)
          | Atom _ | List _ ->
              fail "the action of test %s holds a step no rule models" name.text
        in
        (List.map (fun (a : atom) -> a.text) (atoms args), List.map diff diffs)
    | Some _ ->
        fail "the action of test %s has a shape no rule models" name.text
  in
  let stanza = function
    | List (Atom { text = "test"; _ } :: fields) -> (
        match List.find_map (field "name") fields with
        | Some [ Atom name ] ->
            let args, diffs =
              action name (List.find_map (field "action") fields)
            in
            [ Test { name; args; diffs } ]
        | Some _ | None -> [])
    | List (Atom { text = "tests"; _ } :: fields) ->
        if List.exists (fun f -> Option.is_some (field "action" f)) fields then
          fail "a (tests) stanza with an action is not modeled";
        List.map
          (fun name -> Test { name; args = []; diffs = [] })
          (atoms
             (Option.value ~default:[] (List.find_map (field "names") fields)))
    | List (Atom { text = "library"; _ } :: fields)
      when List.exists (fun f -> Option.is_some (field "inline_tests" f)) fields
      -> (
        match List.find_map (field "name") fields with
        | Some [ Atom name ] -> [ Inline name.text ]
        | Some _ | None -> [])
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

(* The instrumentation backends a session may name with dune's
   [--instrument-with]. The tree's own build is not instrumented, so such a
   session runs a variant whose dune file applies the backend with
   [(preprocess (pps BACKEND -loc-filename=...))]; see [check_backend]. *)
let backends = [ "ppx_windtrap.coverage"; "ppx_windtrap.mutate" ]

(* The executables the repository installs, by public name, and the built
   file [dune exec NAME] runs. *)
let public_names = [ ("windtrap", "bin/main.exe") ]

type command =
  | Runtest of { backend : string option }  (** [dune runtest] or [dune test] *)
  | Exec of { backend : string option; path : string; args : string list }
      (** [dune exec PATH -- ARGS], [PATH] resolved from a public name *)

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
  (* [--force] changes nothing when every run is a new one. *)
  let rec options backend = function
    | "--force" :: rest -> options backend rest
    | "--instrument-with" :: b :: rest when backend = None ->
        if not (List.mem b backends) then
          fail "--instrument-with names %s, not one of %s: %s" b
            (String.concat ", " backends)
            cmd;
        options (Some b) rest
    | rest -> (backend, rest)
  in
  let env, argv = assignments [] (shell_words cmd) in
  let command =
    match argv with
    | "dune" :: ("runtest" | "test") :: rest -> (
        match options None rest with
        | backend, [] -> Runtest { backend }
        | _ -> fail "dune runtest takes --force and --instrument-with: %s" cmd)
    | "dune" :: "exec" :: rest -> (
        match options None rest with
        | backend, path :: rest when not (String.starts_with ~prefix:"-" path)
          -> (
            let path =
              match List.assoc_opt path public_names with
              | Some built -> built
              | None when String.contains path '/' -> path
              | None -> fail "%s is not a public name of the repository" path
            in
            match rest with
            | [] -> Exec { backend; path; args = [] }
            | "--" :: args -> Exec { backend; path; args }
            | _ ->
                fail "dune exec takes its program's arguments after --: %s" cmd)
        | _ -> fail "dune exec takes --instrument-with, then PATH: %s" cmd)
    | _ ->
        fail
          "no rule runs %s: a session holds dune runtest, dune test and dune \
           exec PATH, after VAR=value assignments"
          cmd
  in
  (env, command)

(* Running *)

(* The environment of a clean shell under dune: INSIDE_DUNE naming the
   scratch copy's build context, and the caller's PATH, HOME and TMPDIR.
   Nothing else of the caller's reaches the run, so neither a developer's
   WINDTRAP_* nor CI's CI and GITHUB_ACTIONS changes what it prints. *)
let base_env ~context =
  ("INSIDE_DUNE=" ^ context)
  :: List.filter_map
       (fun var -> Option.map (fun v -> var ^ "=" ^ v) (Sys.getenv_opt var))
       [ "PATH"; "HOME"; "TMPDIR" ]

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

(* The scratch copy

   A page's sessions run in a scratch directory laid out as the
   repository. The directory DIR a session runs is copied as built, once
   per page, to [<top>/_build/default/DIR], and the sources of its
   EXAMPLE, DIR included, to [<top>/EXAMPLE], so a run's build directory
   is [<top>/_build] and its project root is [<top>]. What the runs write
   there (capture logs, the last failed tests, coverage dumps, verdict
   files, corrections) is seen by the page's later sessions and never by
   another page, and is removed with the copy. *)

type scratch = { top : string; mutable copied : string list }

let scratch_context s =
  Filename.concat (Filename.concat s.top "_build") "default"

(* The scratch directory is named by its real path, the one a process in it
   reads as its current directory, so that INSIDE_DUNE and the current
   directory agree as they do under dune (on macOS, /var is a link). *)
let create_scratch () =
  let tmp = Filename.get_temp_dir_name () in
  if
    List.exists
      (String.starts_with ~prefix:"_build")
      (String.split_on_char '/' tmp)
  then
    fail
      "the temporary directory %s lies below a build directory, where the runs \
       would write into the tree's own"
      tmp;
  let rand = Random.State.make_self_init () in
  let rec attempt n =
    let top =
      Filename.concat tmp
        (strf "transcript-%d-%06x" (Unix.getpid ())
           (Random.State.bits rand land 0xffffff))
    in
    match Unix.mkdir top 0o700 with
    | () -> Unix.realpath top
    | exception Unix.Unix_error (Unix.EEXIST, _, _) when n > 0 -> attempt (n - 1)
    | exception Unix.Unix_error (e, _, _) ->
        fail "cannot create a scratch directory in %s: %s" tmp
          (Unix.error_message e)
  in
  { top = attempt 10; copied = [] }

let rec mkdir_p dir =
  if not (Sys.file_exists dir) then (
    mkdir_p (Filename.dirname dir);
    Sys.mkdir dir 0o755)

(* A copy is writable, as a checkout's file is, whatever dune made of
   the one it builds. *)
let copy_file src dst =
  let perm = (Unix.stat src).Unix.st_perm lor 0o200 in
  let data = read_file src in
  try
    Out_channel.with_open_gen
      [ Open_wronly; Open_creat; Open_trunc; Open_binary ] perm dst (fun oc ->
        Out_channel.output_string oc data)
  with Sys_error e -> fail "cannot copy to the scratch directory: %s" e

(* Copies the tree [src] to [dst], leaving out the entries whose name
   starts with a dot, which are dune's, but for the directories of inline
   test runners, and those [skip] names. *)
let rec copy_tree ~skip src dst =
  mkdir_p dst;
  let dune's name =
    name.[0] = '.' && not (Filename.check_suffix name ".inline-tests")
  in
  Array.iter
    (fun name ->
      if (not (dune's name)) && not (skip name) then
        let s = Filename.concat src name and d = Filename.concat dst name in
        match (Unix.stat s).Unix.st_kind with
        | Unix.S_DIR -> copy_tree ~skip s d
        | Unix.S_REG -> copy_file s d
        | _ -> ())
    (Sys.readdir src)

let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Array.iter
        (fun name -> remove_tree (Filename.concat path name))
        (Sys.readdir path);
      Unix.rmdir path
  | _ -> Sys.remove path
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()

(* Copies [path] of the build context below [dst], once per page. *)
let mirror ~root scratch ~skip ~dst path =
  let dst = Filename.concat dst path in
  if not (List.mem dst scratch.copied) then (
    copy_tree ~skip (Filename.concat root path) dst;
    scratch.copied <- dst :: scratch.copied)

let copy_example ~root scratch ~dir ~example =
  mirror ~root scratch ~skip:(fun _ -> false) ~dst:(scratch_context scratch) dir;
  mirror ~root scratch
    ~skip:(fun name -> Filename.check_suffix name ".exe")
    ~dst:scratch.top example

(* The backends that [dir]'s dune file applies with [(pps ...)] must be
   the one the command names with [--instrument-with], or none. *)
let check_backend ~root ~dir backend =
  let rec applied = function
    | Atom _ -> []
    | List (Atom { text = "pps"; _ } :: args) ->
        List.filter_map
          (function
            | Atom { text; _ } when List.mem text backends -> Some text
            | Atom _ | List _ -> None)
          args
    | List l -> List.concat_map applied l
  in
  let file = Filename.concat dir "dune" in
  let applied =
    List.sort_uniq String.compare
      (List.concat_map applied
         (parse_sexps (read_file (Filename.concat root file))))
  in
  if applied <> Option.to_list backend then
    match backend with
    | Some b ->
        fail
          "--instrument-with %s runs a variant that applies %s with (pps %s), \
           and %s does not"
          b b b file
    | None ->
        fail "%s applies %s, which the command must name with --instrument-with"
          file
          (String.concat ", " applied)

(* The output of one command of a session in [dir], printed as if in
   [example]. [root] is the build context, an absolute path. *)
let run ~root ~scratch ~dir ~example (assignments, command) =
  let context = scratch_context scratch in
  let env = assignments @ base_env ~context in
  let out =
    match command with
    | Runtest { backend } ->
        check_backend ~root ~dir backend;
        let file = Filename.concat example "dune" in
        let text = read_file (Filename.concat root file) in
        let runnables = runnables (parse_sexps text) in
        if runnables = [] then fail "%s declares no test" file;
        copy_example ~root scratch ~dir ~example;
        let cwd = Filename.concat context dir in
        let in_build f = String.concat "/" [ "_build"; "default"; dir; f ] in
        (* dune's [diff?] after a run that exited 0: the first that
           differs fails the [progn] with git's diff, and each consumes
           its corrected file. *)
        let rec diffs = function
          | [] -> ""
          | (a, b) :: rest ->
              let pb = Filename.concat cwd b in
              if not (Sys.file_exists pb) then diffs rest
              else
                let same = read_file (Filename.concat cwd a) = read_file pb in
                let shown =
                  if same then ""
                  else
                    strf "File \"%s/%s\", line 1, characters 0-0:\n" dir a
                    ^ fst
                        (spawn ~cwd:scratch.top
                           ~env:
                             ("GIT_CONFIG_NOSYSTEM=1"
                            :: "GIT_CONFIG_GLOBAL=/dev/null" :: env)
                           ~prog:"/usr/bin/env"
                           [
                             "env";
                             "git";
                             "--no-pager";
                             "diff";
                             "--no-index";
                             "--color=always";
                             "-u";
                             "--ignore-cr-at-eol";
                             in_build a;
                             in_build b;
                           ])
                in
                Sys.remove pb;
                if same then diffs rest else shown
        in
        let run_one = function
          | Test { name; args; diffs = expected } ->
              let exe = name.text ^ ".exe" in
              List.iter
                (fun (_, b) ->
                  let pb = Filename.concat cwd b in
                  if Sys.file_exists pb then Sys.remove pb)
                expected;
              let out, code =
                spawn ~cwd ~env ~prog:(Filename.concat cwd exe)
                  (("./" ^ exe) :: args)
              in
              if code = 0 then out ^ diffs expected
              else dune_location ~file ~text name ^ out
          | Inline lib ->
              (* A variant builds test executables only: the library and
                 its inline tests are the example's, copied as built. *)
              mirror ~root scratch ~skip:(fun _ -> false) ~dst:context example;
              let cwd = Filename.concat context example in
              let runner = strf ".%s.inline-tests/inline-test-runner.exe" lib in
              let prog = Filename.concat cwd runner in
              let argv = [ runner; "inline-test-runner"; lib ] in
              let partitions, code =
                spawn ~cwd ~env ~prog (argv @ [ "-list-partitions" ])
              in
              if code <> 0 then
                fail "the inline tests of %s list no partitions" lib;
              let partition p =
                let out, code =
                  spawn ~cwd ~env ~prog (argv @ [ "-partition"; p ])
                in
                if code <> 0 then
                  fail "an inline test of %s failed, which no rule models" lib;
                out
              in
              String.concat "" (List.map partition (lines partitions))
        in
        String.concat "" (List.map run_one runnables)
    | Exec { backend; path; args } ->
        let prefix = example ^ "/" in
        let path =
          if String.starts_with ~prefix path then
            dir ^ "/"
            ^ String.sub path (String.length prefix)
                (String.length path - String.length prefix)
          else path
        in
        let prog =
          if String.starts_with ~prefix:(dir ^ "/") path then (
            check_backend ~root ~dir backend;
            copy_example ~root scratch ~dir ~example;
            Filename.concat context path)
          else if backend <> None then
            fail "--instrument-with applies to the executables of %s" example
          else Filename.concat root path
        in
        fst
          (spawn ~cwd:scratch.top ~env ~prog
             (("./_build/default/" ^ path) :: args))
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

let regenerate ~root ~roots ~scratch marker block =
  match marker with
  | File { path; excerpt = None } ->
      lines (read_file (Filename.concat root path))
  | File { path; excerpt = Some (first, last) } ->
      let starts prefix l = String.starts_with ~prefix l in
      let rec paragraph = function
        | "" :: _ | [] -> []
        | l :: rest -> l :: paragraph rest
      in
      let rec upto prefix = function
        | [] -> fail "%s has no line that starts with %S" path prefix
        | l :: _ as rest when starts prefix l -> paragraph rest
        | l :: rest -> l :: upto prefix rest
      in
      let rec from = function
        | [] -> fail "%s has no line that starts with %S" path first
        | l :: rest when starts first l -> (
            match last with
            | None -> paragraph (l :: rest)
            | Some last -> l :: upto last rest)
        | _ :: rest -> from rest
      in
      from (lines (read_file (Filename.concat root path)))
  | Run { dir; example } ->
      let session = function
        | cmd when String.starts_with ~prefix:"$ " cmd ->
            let text = String.sub cmd 2 (String.length cmd - 2) in
            let scratch = Lazy.force scratch in
            Some (cmd :: run ~root ~scratch ~dir ~example (parse_command text))
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
  let scratch = lazy (create_scratch ()) in
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
          let fresh =
            at_line (fun () -> regenerate ~root ~roots ~scratch m block)
          in
          let same =
            mask ~roots (String.concat "\n" block)
            = mask ~roots (String.concat "\n" fresh)
          in
          List.iter emit
            (src.(i) :: src.(fence) :: (if same then block else fresh));
          emit src.(last);
          go (last + 1)
  in
  Fun.protect
    ~finally:(fun () ->
      if Lazy.is_val scratch then remove_tree (Lazy.force scratch).top)
    (fun () -> go 0);
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
