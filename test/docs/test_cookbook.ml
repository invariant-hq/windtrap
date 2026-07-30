(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The compiled mirror of doc/cookbook.md: every recipe in the cookbook
   appears here as a running test, in the cookbook's order and shape, so a
   recipe that rots breaks the build. The Eio adapter itself needs a
   dependency windtrap does not have; its guarantees are tested instead
   (recipe 3). A plain windtrap suite: this executable is a runtest test and
   must exit 0. *)

open Windtrap

(* Recipe 1: temporary directories and files *)

let rec rm_rf path =
  if Sys.is_directory path then begin
    Array.iter
      (fun name -> rm_rf (Filename.concat path name))
      (Sys.readdir path);
    Sys.rmdir path
  end
  else Sys.remove path

let with_temp_dir fn =
  let dir = Filename.temp_file "test-" ".dir" in
  Sys.remove dir;
  Sys.mkdir dir 0o700;
  Fun.protect ~finally:(fun () -> rm_rf dir) (fun () -> fn dir)

(* The built-ins need no lifecycle: the runner removes them after the
   test. [scratch] lets a later test observe the removal. *)
let scratch = ref ""

let temp_tests =
  group "temporary directories"
    [
      test "temp_dir gives fresh runner-owned directories" (fun () ->
          let dir = temp_dir () in
          scratch := dir;
          let file = Filename.concat dir "config.json" in
          Out_channel.with_open_text file (fun oc ->
              Out_channel.output_string oc "{}");
          is_true (Sys.file_exists file);
          not_equal string dir (temp_dir ());
          is_true (Sys.file_exists (temp_file ~suffix:".json" ())));
      test "the runner removed the previous test's scratch" (fun () ->
          is_false (Sys.file_exists !scratch));
      test "with_temp_dir scopes a directory to one call" (fun () ->
          let seen = ref "" in
          with_temp_dir (fun dir ->
              seen := dir;
              is_true (Sys.is_directory dir));
          is_false (Sys.file_exists !seen));
      test "with_temp_dir removes on the raise path" (fun () ->
          let seen = ref "" in
          raises Exit (fun () ->
              with_temp_dir (fun dir ->
                  seen := dir;
                  raise Exit));
          is_false (Sys.file_exists !seen));
    ]

(* Recipe 2: scoped environment variables *)

let with_env var value fn =
  let saved = Sys.getenv_opt var in
  Unix.putenv var value;
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv var (match saved with Some v -> v | None -> ""))
    fn

let env_tests =
  group "scoped environment"
    [
      test "with_env sets inside and restores after" (fun () ->
          with_env "COOKBOOK_ENV" "inner" (fun () ->
              equal (option string) (Some "inner")
                (Sys.getenv_opt "COOKBOOK_ENV"));
          (* putenv cannot unset: an initially-unset variable restores to
             unset-or-empty, never to the inner value. *)
          satisfies ~msg:"unset-or-empty" (option string)
            (fun v -> v = None || v = Some "")
            (Sys.getenv_opt "COOKBOOK_ENV"));
      test "with_env restores on the raise path" (fun () ->
          raises Exit (fun () ->
              with_env "COOKBOOK_ENV_RAISE" "inner" (fun () -> raise Exit));
          satisfies ~msg:"unset-or-empty" (option string)
            (fun v -> v = None || v = Some "")
            (Sys.getenv_opt "COOKBOOK_ENV_RAISE"));
    ]

(* Recipe 3: the Eio guarantees

   The [with_eio] adapter is a fragment (windtrap has no eio dependency);
   what the cookbook guarantees is that assertion failures are ordinary
   exceptions classified by identity, not catch site — storing one in a
   ref and re-raising it outside the assertion's dynamic extent keeps its
   structured payload. *)

let eio_tests =
  group "eio guarantees"
    [
      test "assertion failures survive store-and-reraise" (fun () ->
          let stored = ref None in
          (try equal int 1 2 with e -> stored := Some e);
          match !stored with
          | Some
              (Private.Failure.Check_failure
                 { Private.Failure.kind = Private.Failure.Equality _; _ }) ->
              ()
          | Some e -> raise e
          | None -> fail "the assertion did not raise");
    ]

(* Recipe 4: subprocess workers via a role env var

   The dispatch on COOKBOOK_ROLE happens at the bottom of this file,
   before [run] — the worker path never touches windtrap's CLI parsing or
   process exit. *)

let spawn_self ~role =
  with_env "COOKBOOK_ROLE" role @@ fun () ->
  let read_end, write_end = Unix.pipe ~cloexec:false () in
  let pid =
    Unix.create_process Sys.executable_name [| Sys.executable_name |] Unix.stdin
      write_end Unix.stderr
  in
  Unix.close write_end;
  let buffer = Buffer.create 256 in
  let bytes = Bytes.create 4096 in
  let rec drain () =
    match Unix.read read_end bytes 0 (Bytes.length bytes) with
    | 0 -> ()
    | n ->
        Buffer.add_subbytes buffer bytes 0 n;
        drain ()
  in
  drain ();
  Unix.close read_end;
  let _, status = Unix.waitpid [] pid in
  (match status with Unix.WEXITED 0 -> () | _ -> fail "worker did not exit 0");
  Buffer.contents buffer

let subprocess_tests =
  group "subprocess role pattern"
    [
      test "the worker role re-executes this binary" (fun () ->
          contains ~sub:"lock acquired" (spawn_self ~role:"worker"));
    ]

(* Recipe 5: event sets via slist + contramap *)

type event = { path : string; kind : string; timestamp : float }

let key e = (e.path, e.kind)
let event = contramap key (pair string string)
let events = slist event (fun a b -> compare (key a) (key b))

let projection_tests =
  group "event-set comparison"
    [
      test "order- and timestamp-insensitive" (fun () ->
          equal events
            [
              { path = "a"; kind = "created"; timestamp = 0. };
              { path = "b"; kind = "removed"; timestamp = 0. };
            ]
            [
              { path = "b"; kind = "removed"; timestamp = 17.3 };
              { path = "a"; kind = "created"; timestamp = 42.1 };
            ]);
    ]

(* Recipe 6: cover with a noise-floor margin *)

let cover_tests =
  group "cover noise floor"
    [
      prop "parity is exercised" ~count:200 Gen.small_int (fun n ->
          cover ~label:"even" ~at_least:20. (n mod 2 = 0);
          cover ~label:"odd" ~at_least:20. (n mod 2 <> 0);
          equal int n n);
    ]

(* Recipe 7: suite-level skip through a skipping fixture *)

let probes = ref 0

let device : unit -> unit =
  fixture (fun () ->
      incr probes;
      skip ~reason:"no device in this environment" ())

let suite_skip_tests =
  group "suite-level skip"
    [
      test "gpu elementwise" (fun () -> device ());
      test "gpu reduction" (fun () -> device ());
      test "the probe ran exactly once" (fun () -> equal int 1 !probes);
    ]

(* Recipe 8: codec round-trip *)

let encode l = String.concat "," (List.map string_of_int l)

let decode = function
  | "" -> []
  | s -> List.map int_of_string (String.split_on_char ',' s)

let codec_tests =
  group "codec round-trip"
    [
      prop "decode inverts encode"
        Gen.(list small_int)
        (fun l -> equal (list int) l (decode (encode l)));
    ]

(* Recipe 9: two-phase keyed comparison *)

type tensor = { shape : int array; data : float array }

let equal_tensor ?pos expected actual =
  equal ?pos ~msg:"shape" (array int) expected.shape actual.shape;
  equal ?pos ~msg:"values" (array (float 1e-9)) expected.data actual.data

let keyed_tests =
  group "two-phase comparison"
    [
      test "equal tensors pass both phases" (fun () ->
          equal_tensor
            { shape = [| 2; 2 |]; data = [| 1.; 2.; 3.; 4. |] }
            { shape = [| 2; 2 |]; data = [| 1.; 2.; 3.; 4. |] });
      test "a shape mismatch fails in phase one" (fun () ->
          match
            equal_tensor
              { shape = [| 2 |]; data = [| 1.; 2. |] }
              { shape = [| 1; 2 |]; data = [| 1.; 2. |] }
          with
          | () -> fail "shape mismatch must fail"
          | exception Private.Failure.Check_failure failure ->
              equal (option string) (Some "shape") failure.Private.Failure.msg);
    ]

(* Recipe 10: complex tolerance testable *)

let complex ~rel ~abs : Complex.t testable =
  let close = Testable.equal (float_rel ~rel ~abs) in
  Testable.make
    ~pp:(fun ppf { Complex.re; im } ->
      Format.fprintf ppf "(%.17g %+.17gi)" re im)
    ~equal:(fun a b ->
      close a.Complex.re b.Complex.re && close a.Complex.im b.Complex.im)

let complex_tests =
  group "complex tolerance"
    [
      test "componentwise tolerance" (fun () ->
          equal
            (complex ~rel:1e-9 ~abs:1e-12)
            { Complex.re = 1.; im = 2. }
            { Complex.re = 1. +. 1e-13; im = 2. -. 1e-13 });
    ]

(* The role dispatch and the suite *)

let () =
  match Sys.getenv_opt "COOKBOOK_ROLE" with
  | Some "worker" ->
      print_string "lock acquired\n";
      exit 0
  | Some _ | None ->
      run "cookbook"
        [
          temp_tests;
          env_tests;
          eio_tests;
          subprocess_tests;
          projection_tests;
          cover_tests;
          suite_skip_tests;
          codec_tests;
          keyed_tests;
          complex_tests;
        ]
