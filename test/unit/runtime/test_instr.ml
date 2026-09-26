(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Instr = Windtrap_runtime.Instr

let strf = Printf.sprintf
let read path = In_channel.with_open_bin path In_channel.input_all

let write path contents =
  Out_channel.with_open_bin path (fun oc -> output_string oc contents)

let md5 s = Digest.to_hex (Digest.string s)

(* A format of the suite's own, so that each claim shows which field of the
   format a value reads. *)
let format =
  {
    Instr.magic = "windtrap-sample-v1";
    kind = "sample";
    dir = "samples";
    ext = "smp";
    remedy = "delete the stale sample files";
    who = "Sample";
  }

(* Writer identities *)

let identities =
  group "Writer identities"
    [
      test "file_digest is the lowercase hexadecimal MD5 of the file" (fun () ->
          let path = temp_file () in
          write path "a\r\nb\000";
          equal (option string)
            (Some (md5 "a\r\nb\000"))
            (Instr.file_digest path));
      test "file_digest of a file that cannot be read is None" (fun () ->
          equal (option string) None
            (Instr.file_digest (Filename.concat (temp_dir ()) "missing")));
    ]

(* Build paths *)

let in_build ?(build = "/w/p/_build") identity =
  strf "%s/_samples/windtrap-%s.smp" build (md5 identity)

(* The spellings of one executable, and of others, below a build directory. *)
let below_a_build =
  [
    ("/w/p/_build/default/test/a.exe", in_build "default/test/a.exe");
    ( "/w/p/_build/.sandbox/0abc12/default/test/a.exe",
      in_build "default/test/a.exe" );
    ("/w/p/_build/default/test/./a.exe", in_build "default/test/a.exe");
    ("/w/p/_build/default/test/sub/../a.exe", in_build "default/test/a.exe");
    ("/w/p/_build/default/test/b.exe", in_build "default/test/b.exe");
    ("/w/p/_build/alt/test/a.exe", in_build "alt/test/a.exe");
    ( "/elsewhere/_build/default/test/a.exe",
      in_build ~build:"/elsewhere/_build" "default/test/a.exe" );
    ( "/w/p/_build_ci/default/test/a.exe",
      in_build ~build:"/w/p/_build_ci" "default/test/a.exe" );
  ]

let below_none = [ "/opt/t.exe"; "/w/p/_build_x.exe"; "/opt/tools/mytool.exe" ]
let standalone () = Sys.getcwd () ^ "/_windtrap/samples"

let normalized =
  [
    ("/w/p/_build/default/test/a.exe", "default/test/a.exe");
    ("/w/p/_build/default/test/./a.exe", "default/test/a.exe");
    ("/w/p/./_build/./default/test/a.exe", "default/test/a.exe");
    ("/w/p/_build/default/test/sub/../a.exe", "default/test/a.exe");
    ("/w/p/_build/default//test/a.exe", "default/test/a.exe");
    ("/w/p\\_build\\default\\test\\a.exe", "default/test/a.exe");
    ("/w/_build/default/a\\b.exe", "default/a/b.exe");
    ("/w/_build/../a.exe", "/w/a.exe");
    ("/opt/tools/mytool.exe", "/opt/tools/mytool.exe");
    ("/opt/tools/bin/./../mytool.exe", "/opt/tools/mytool.exe");
    ("/../../opt/mytool.exe", "/opt/mytool.exe");
  ]

let sandboxed =
  [
    ("/w/p/_build/.sandbox/0abc12/default/test/a.exe", "default/test/a.exe");
    ("/w/_build/.sandbox/3f/_build_x.exe", "_build_x.exe");
    ("/w/_build/.sandbox/3f", ".sandbox/3f");
    ("/w/_build/.sandbox/a.exe", ".sandbox/a.exe");
    ("/w/_build/default/.sandbox/3f/a.exe", "default/.sandbox/3f/a.exe");
  ]

let named_build =
  [
    ("/w/p/_build", "/w/p/_build");
    ("/w/p/_build/", "/w/p/_build");
    ("/w/p/_build_x.exe", "/w/p/_build_x.exe");
  ]

let build_dirs =
  [
    ("/w/p/_build/default/test", Some "/w/p/_build");
    ("/w/p/_build_ci/default/t.exe", Some "/w/p/_build_ci");
    ("/w/p/_build_ci/.sandbox/3f/default", Some "/w/p/_build_ci");
    ("/w/p\\_build\\default", Some "/w/p/_build");
    ("/w/p/src/lib", None);
  ]

let build_roots =
  [
    ("/w/p/_build/default/test", Some "/w/p");
    ("/w/p/_build/.sandbox/0abc12/default/test", Some "/w/p");
    ("/w/p/_build/.sandbox/_build/_coverage", Some "/w/p");
    ("/w/p/_build_ci/default/test/t.exe", Some "/w/p");
    ("/w/p/src/lib", None);
  ]

let relative_path () =
  let cwd = Filename.concat (temp_dir ()) "_build/default" in
  Sys.mkdir (Filename.dirname cwd) 0o755;
  Sys.mkdir cwd 0o755;
  chdir cwd;
  equal (option string)
    (Some (Windtrap_test_support.slashed (Filename.dirname (Sys.getcwd ()))))
    (Instr.build_dir ~path:"t.exe")

let needs_cwd f = raises_match Exn.sys_error f

let unreadable_cwd () =
  if Sys.win32 then
    skip ~reason:"Windows cannot remove a process's working directory" ();
  let gone = Filename.concat (temp_dir ()) "gone" in
  Sys.mkdir gone 0o755;
  chdir gone;
  Sys.rmdir gone;
  needs_cwd (fun () -> Instr.absolute "a.exe");
  needs_cwd (fun () -> Instr.build_dir ~path:"_build/default");
  needs_cwd (fun () -> Instr.exe_identity ~exe:"_build/default/a.exe");
  needs_cwd (fun () -> Instr.output_file format ~exe:"/opt/t.exe");
  equal string
    (in_build ~build:"/w/_build" "default/a.exe")
    (Instr.output_file format ~exe:"/w/_build/default/a.exe")

let build_paths =
  group "Build paths"
    [
      test
        "absolute keeps an absolute path, and puts a relative one below the \
         current directory without normalizing it" (fun () ->
          equal (list string)
            [ "/w/./a"; Filename.concat (Sys.getcwd ()) "./a/../b" ]
            [ Instr.absolute "/w/./a"; Instr.absolute "./a/../b" ]);
      cases "build_dir cuts the path after its first component named _build*"
        ~name:fst build_dirs (fun (path, dir) ->
          equal (option string) dir (Instr.build_dir ~path));
      test "a relative path is made absolute against the current directory"
        relative_path;
      cases "build_root is the parent of the build directory" ~name:fst
        build_roots (fun (path, root) ->
          equal (option string) root (Instr.build_root ~path));
      cases "exe_identity is the path below the build directory, normalized"
        ~name:fst normalized (fun (exe, identity) ->
          equal string identity (Instr.exe_identity ~exe));
      cases
        "exe_identity drops the .sandbox/<digest> right below the build \
         directory, and nothing else"
        ~name:fst sandboxed (fun (exe, identity) ->
          equal string identity (Instr.exe_identity ~exe));
      cases
        "an executable's own name never makes it lie below a build directory"
        ~name:fst named_build (fun (exe, identity) ->
          equal string identity (Instr.exe_identity ~exe));
      test "data_dir and standalone_data_dir name the format's directory"
        (fun () ->
          equal (list string)
            [ "/w/p/_build/_samples"; "/w/p/_windtrap/samples" ]
            [
              Instr.data_dir format ~build_dir:"/w/p/_build";
              Instr.standalone_data_dir format ~root:"/w/p";
            ]);
      cases
        "output_file is windtrap-<md5 of the identity>.<ext> in the data \
         directory of the build directory"
        ~name:fst below_a_build (fun (exe, file) ->
          equal string file (Instr.output_file format ~exe));
      cases
        "below no build directory, output_file is in the standalone data \
         directory of the current directory, hashed from the absolute path"
        ~name:Fun.id below_none (fun exe ->
          equal string
            (strf "%s/windtrap-%s.smp" (standalone ()) (md5 exe))
            (Instr.output_file format ~exe));
      cases "output_dir is output_file without its extension" ~name:Fun.id
        [ "/w/p/_build/default/test/a.exe"; "/opt/t.exe" ] (fun exe ->
          equal string
            (Filename.remove_extension (Instr.output_file format ~exe))
            (Instr.output_dir format ~exe));
      test "a value that needs an unreadable current directory raises Sys_error"
        unreadable_cwd;
    ]

(* Errors and warnings *)

let errors =
  group "Errors and warnings"
    [
      test "the message of an unknown format says what to do, in its words"
        (fun () ->
          contains ~sub:format.remedy
            (Format.asprintf "%a" (Instr.pp_error format)
               (Unknown_format { path = "old.smp"; header = "V1" })));
      test "warn writes one line behind windtrap's prefix on standard error"
        (fun () ->
          ignore (output ());
          Instr.warn "%s: %d" "lib/a.ml" 3;
          equal string "windtrap: warning: lib/a.ml: 3\n" (output ()));
    ]

(* Reading and writing *)

let read_file_error = function
  | Ok contents -> strf "read %S" contents
  | Error (Instr.Unreadable { path; _ }) -> "unreadable " ^ path
  | Error (Corrupt { path; reason }) -> strf "corrupt %s: %s" path reason
  | Error (Unknown_format { path; _ }) -> "unknown format " ^ path

let entries dir = List.sort String.compare (Array.to_list (Sys.readdir dir))

let replaces () =
  let path = Filename.concat (temp_dir ()) "made/on/demand/target" in
  Instr.write_file path "one";
  Instr.write_file path "two";
  equal string "two" (read path);
  equal (list string) [ "target" ] (entries (Filename.dirname path))

let rename_fails () =
  let dir = temp_dir () in
  let target = Filename.concat dir "target" in
  Sys.mkdir target 0o755;
  raises_match Exn.sys_error (fun () -> Instr.write_file target "data");
  equal (list string) [ "target" ] (entries dir)

let is_lower_hex = function '0' .. '9' | 'a' .. 'f' -> true | _ -> false

(* A name with the six bytes after its four-byte prefix masked when they are
   lowercase hexadecimal digits. *)
let token_shape name =
  String.mapi
    (fun i c -> if i >= 4 && i < 10 && is_lower_hex c then '#' else c)
    name

let new_files () =
  let dir = Filename.concat (temp_dir ()) "made/on/demand" in
  let paths =
    List.map
      (Instr.write_new_file dir ~prefix:"run-" ~ext:"smp")
      [ "one"; "two" ]
  in
  equal (list string) [ dir; dir ] (List.map Filename.dirname paths);
  equal (list string)
    [ "run-######.smp"; "run-######.smp" ]
    (List.map (fun p -> token_shape (Filename.basename p)) paths);
  equal (list string) [ "one"; "two" ] (List.map read paths)

let every_call_new () =
  let dir = temp_dir () in
  let paths =
    List.init 20 (fun i ->
        Instr.write_new_file dir ~prefix:"" ~ext:"smp" (string_of_int i))
  in
  equal (list string)
    (List.sort String.compare (List.map Filename.basename paths))
    (entries dir);
  equal (list string) (List.init 20 string_of_int) (List.map read paths)

let reading_and_writing =
  group "Reading and writing"
    [
      test "read_file is the bytes of the file" (fun () ->
          let path = temp_file () in
          write path "a\r\nb\000";
          equal string "read \"a\\r\\nb\\000\""
            (read_file_error (Instr.read_file path)));
      test "read_file of a missing file is Unreadable, naming it" (fun () ->
          let path = Filename.concat (temp_dir ()) "missing" in
          equal string ("unreadable " ^ path)
            (read_file_error (Instr.read_file path)));
      test "write_file makes the directory and replaces the file" replaces;
      test "write_file that cannot rename leaves no temporary file" rename_fails;
      test
        "write_new_file names a new file by its prefix, six lowercase \
         hexadecimal digits and its extension, in a directory it makes"
        new_files;
      test "every write_new_file is a new file, and leaves nothing else"
        every_call_new;
      test
        "write_new_file under a directory that cannot be made raises Sys_error"
        (fun () ->
          let blocked = Filename.concat (temp_file ()) "under-a-file" in
          raises_match Exn.sys_error (fun () ->
              Instr.write_new_file blocked ~prefix:"" ~ext:"smp" "data"));
    ]

(* The header *)

let digest = md5 "x"

let header identity =
  let b = Buffer.create 64 in
  Instr.add_header format b identity;
  Buffer.contents b

(* What [add_header] raised for a malformed [identity], and what the buffer
   held then. *)
let refusal identity =
  let b = Buffer.create 64 in
  match Instr.add_header format b (Some identity) with
  | () -> ("no exception", Buffer.contents b)
  | exception Invalid_argument message -> (message, Buffer.contents b)

let malformed =
  [
    ("an empty exe", { Instr.exe = ""; digest });
    ("a short digest", { Instr.exe = "a.exe"; digest = "abc" });
    ( "an uppercase digest",
      { Instr.exe = "a.exe"; digest = String.uppercase_ascii digest } );
  ]

let the_header =
  group "The header"
    [
      test
        "add_header writes the magic line, then exe, the digest, the length \
         and the path" (fun () ->
          equal (list string)
            [
              "windtrap-sample-v1\n";
              strf "windtrap-sample-v1\nexe %s 22 default/my tests/a.exe\n"
                digest;
            ]
            [
              header None;
              header (Some { exe = "default/my tests/a.exe"; digest });
            ]);
      cases
        "add_header refuses a malformed identity in a message the format's \
         owner prefixes, after the magic line"
        ~name:fst malformed (fun (_, identity) ->
          let message, written = refusal identity in
          starts_with ~affix:"Sample: " message;
          equal string "windtrap-sample-v1\n" written);
      test "add_header names what is malformed" (fun () ->
          expect
            (String.concat "\n"
               (List.map
                  (fun (_, identity) -> fst (refusal identity))
                  malformed))
          @@ __POS_OF__
               {|
            Sample: empty identity exe
            Sample: identity digest is not 32 hex characters
            Sample: identity digest is not 32 hex characters
            |});
    ]

(* Parsing *)

let cursor s =
  require_ok ~pp:(Instr.pp_error format) (Instr.start format ~path:"<test>" s)

let after s = cursor (format.magic ^ s)

let started = function
  | Ok _ -> "started"
  | Error (Instr.Unknown_format { path; header }) ->
      strf "unknown format in %s, header %S" path header
  | Error (Unreadable { path; _ }) -> "unreadable " ^ path
  | Error (Corrupt { path; _ }) -> "corrupt " ^ path

let long_line = "\t\"" ^ String.make 100 'x'

(* Each reader on an input it cannot read. *)
let refused =
  [
    ( "read_nat, no number",
      fun () -> ignore (Instr.read_nat (after " x") "size") );
    ( "read_nat, a sign alone",
      fun () -> ignore (Instr.read_nat (after " -") "size") );
    ( "read_nat, past max_int",
      fun () -> ignore (Instr.read_nat (after " 99999999999999999999") "size")
    );
    ( "read_nat, a negative number",
      fun () -> ignore (Instr.read_nat (after " -1") "size") );
    ( "read_nat, the end of the input",
      fun () -> ignore (Instr.read_nat (after " ") "size") );
    ( "read_count, past the input",
      fun () -> ignore (Instr.read_count (after " 99") "records") );
    ( "read_name, no length",
      fun () -> ignore (Instr.read_name (after " abc") "file") );
    ( "read_name, a tab for the space",
      fun () -> ignore (Instr.read_name (after " 3\tabc") "file") );
    ( "read_name, fewer bytes than the length",
      fun () -> ignore (Instr.read_name (after " 9 abc") "file") );
    ( "read_word, the end of the input",
      fun () -> ignore (Instr.read_word (after "  ") "verdict") );
    ( "read_identity, a short digest",
      fun () -> ignore (Instr.read_identity (after "\nexe abc 1 a")) );
    ( "read_identity, an uppercase digest",
      fun () ->
        ignore
          (Instr.read_identity
             (after ("\nexe " ^ String.uppercase_ascii digest ^ " 1 a"))) );
    ( "read_identity, an empty path",
      fun () -> ignore (Instr.read_identity (after ("\nexe " ^ digest ^ " 0 ")))
    );
    ( "read_identity, a truncated path",
      fun () ->
        ignore (Instr.read_identity (after ("\nexe " ^ digest ^ " 9 a"))) );
    ("finish, a trailing byte", fun () -> Instr.finish (after "\nx"));
  ]

let parse_error = function Instr.Parse_error _ -> true | _ -> false

let reason f =
  match f () with
  | () -> "no Parse_error"
  | exception Instr.Parse_error reason -> reason

let read_identity_line () =
  let c = after ("\nexe " ^ digest ^ " 7 a b.exe") in
  equal
    (option (pair string string))
    (Some ("a b.exe", digest))
    (Option.map
       (fun (i : Instr.identity) -> (i.exe, i.digest))
       (Instr.read_identity c))

let pp_identity ppf (i : Instr.identity) =
  Format.fprintf ppf "exe %s %s" i.digest i.exe

let no_identity () =
  let c = after " \n 3 lib" in
  is_none ~pp:pp_identity (Instr.read_identity c);
  equal int 3 (Instr.read_nat c "count")

(* Inputs that start with a prefix of exe, or share its first byte. *)
let not_exe = [ ("e", "\ne 3"); ("ex", "\nex 3"); ("exa", "\nexa 3") ]

let parsing =
  group "Parsing"
    [
      test "parse_fail raises Parse_error with the formatted reason" (fun () ->
          raises (Instr.Parse_error "bad 3") (fun () ->
              Instr.parse_fail "bad %d" 3));
      test "start accepts the magic alone, or followed by whitespace" (fun () ->
          Instr.finish (cursor format.magic);
          equal int 7 (Instr.read_nat (after "\t7") "n"));
      test "start refuses a longer magic as an unknown format, naming the path"
        (fun () ->
          equal string
            "unknown format in f.smp, header \"windtrap-sample-v10 1\""
            (started (Instr.start format ~path:"f.smp" (format.magic ^ "0 1"))));
      test "an unknown header is the first line cut to 64 bytes, then escaped"
        (fun () ->
          equal string
            (strf "unknown format in f.smp, header %S"
               (String.escaped (String.sub long_line 0 64)))
            (started (Instr.start format ~path:"f.smp" (long_line ^ "\n0\n"))));
      test "read_nat reads a decimal natural after spaces, tabs, CRs and LFs"
        (fun () ->
          let c = after " \t\r\n 42 7" in
          let first = Instr.read_nat c "a" in
          equal (pair int int) (42, 7) (first, Instr.read_nat c "b"));
      test "read_count accepts the length of the whole input, and no more"
        (fun () ->
          (* The input is 21 bytes, the magic included. *)
          equal int 21 (Instr.read_count (after " 21") "records");
          raises_match parse_error (fun () ->
              Instr.read_count (after " 22") "records"));
      test "read_name reads a length, one space and that many bytes" (fun () ->
          let c = after " 5 a b\tc 0 " in
          let first = Instr.read_name c "file" in
          equal (pair string string) ("a b\tc", "")
            (first, Instr.read_name c "file"));
      test "read_word reads the bytes up to the next whitespace" (fun () ->
          let c = after "  killed\tsurvived" in
          let first = Instr.read_word c "verdict" in
          equal (pair string string) ("killed", "survived")
            (first, Instr.read_word c "verdict"));
      test "read_identity reads the line that starts with exe"
        read_identity_line;
      test "without exe, read_identity is None past the whitespace only"
        no_identity;
      cases "read_identity is None when the first three bytes are not exe"
        ~name:fst not_exe (fun (_, input) ->
          is_none ~pp:pp_identity (Instr.read_identity (after input)));
      test "finish accepts trailing whitespace" (fun () ->
          Instr.finish (after " \t\r\n"));
      cases "a reader refuses what it cannot read with Parse_error" ~name:fst
        refused (fun (_, f) -> raises_match parse_error f);
      test "the reasons name what was read, and where" (fun () ->
          expect
            (String.concat "\n"
               (List.map (fun (name, f) -> name ^ ": " ^ reason f) refused))
          @@ __POS_OF__
               {|
            read_nat, no number: expected size at offset 19
            read_nat, a sign alone: invalid size at offset 19
            read_nat, past max_int: invalid size at offset 19
            read_nat, a negative number: negative size
            read_nat, the end of the input: expected size at offset 19
            read_count, past the input: records exceeds data
            read_name, no length: expected file length at offset 19
            read_name, a tab for the space: expected space before file at offset 20
            read_name, fewer bytes than the length: truncated file
            read_word, the end of the input: expected verdict at offset 20
            read_identity, a short digest: identity digest is not 32 hex characters at offset 23
            read_identity, an uppercase digest: identity digest is not 32 hex characters at offset 23
            read_identity, an empty path: empty executable identity
            read_identity, a truncated path: truncated executable identity
            finish, a trailing byte: trailing data at offset 19
            |});
    ]

let () =
  exit
    (run "instr"
       [
         identities;
         build_paths;
         errors;
         reading_and_writing;
         the_header;
         parsing;
       ])
