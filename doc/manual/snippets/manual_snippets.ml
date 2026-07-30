(* Compiled mirrors of the manual's passing snippets (doc/manual/): each
   chapter's example code is declared here and the whole set runs green as
   one suite under runtest — a snippet that rots breaks the build. Failing
   walkthroughs live in transcript_fail.ml; expect-test snippets in
   manual_expect.ml. *)

open Windtrap

(* ───── getting-started.md ───── *)

module Calc = struct
  exception Parse_error of string

  let add a b = a + b

  let parse input =
    let trimmed = String.trim input in
    if trimmed = "" then raise (Parse_error "empty")
    else
      match int_of_string_opt trimmed with
      | Some n -> n
      | None -> raise (Parse_error ("not a number: " ^ trimmed))
end

open Calc

let getting_started =
  group "mylib"
    [
      test "addition" (fun () -> equal int 5 (Calc.add 2 3));
      group "parser"
        [
          test "empty input" (fun () ->
              raises (Parse_error "empty") (fun () -> Calc.parse ""));
        ];
    ]

(* ───── assertions.md ───── *)

type point = { x : int; y : int }

let pp_point ppf { x; y } = Format.fprintf ppf "(%d, %d)" x y
let point = Testable.make ~pp:pp_point ~equal:( = )
let find_user = function "alice" -> Some 1 | _ -> None

let parse_port input =
  match int_of_string_opt input with
  | Some port when port > 0 && port < 65_536 -> Ok port
  | Some _ | None -> Error ("invalid port: " ^ input)

type addr = Tcp of int | Unix_socket of string

let tcp_port = function Tcp port -> Some port | Unix_socket _ -> None
let resolve = function "db" -> Tcp 5432 | sock -> Unix_socket sock

let assertions =
  group "assertions"
    [
      test "equal composes witnesses" (fun () ->
          equal
            (list (pair string (list int)))
            [ ("alice", [ 1; 2; 3 ]); ("bob", [ 4 ]) ]
            [ ("alice", [ 1; 2; 3 ]); ("bob", [ 4 ]) ]);
      test "unwrap, then keep asserting" (fun () ->
          let id = require_some (find_user "alice") in
          equal int 1 id;
          let port = require_ok (parse_port "8080") in
          equal int 8080 port;
          let message = require_error (parse_port "0") in
          equal string "invalid port: 0" message;
          let port = require_match tcp_port (resolve "db") in
          equal int 5432 port);
      test "satisfies names the predicate and prints the value" (fun () ->
          satisfies ~msg:"positive" int (fun n -> n > 0) 42);
      test "contains excerpts the haystack" (fun () ->
          let log = "user=alice token=REDACTED\n" in
          contains ~sub:"user=alice" log;
          not_contains ~sub:"secret" log);
      test "raises compares structurally" (fun () ->
          raises (Parse_error "empty") (fun () -> Calc.parse " "));
      test "raises_match takes a predicate" (fun () ->
          raises_match (Exn.invalid_arg ~substring:"negative") (fun () ->
              invalid_arg "checkout: negative coupon"));
      test "fail marks unreachable branches" (fun () ->
          match find_user "alice" with
          | Some _ -> ()
          | None -> fail "alice must exist");
      test "skip under unmet preconditions" (fun () ->
          if Sys.win32 then skip ~reason:"unix only" ());
      test "custom testables print like their pp" (fun () ->
          equal point { x = 1; y = 2 } { x = 1; y = 2 });
      test "float takes an absolute tolerance" (fun () ->
          equal (float 1e-9) 0.3 (0.1 +. 0.2));
      test "float_exact can assert NaN" (fun () ->
          equal float_exact Float.nan (0. /. 0.));
      test "slist compares as a multiset" (fun () ->
          equal (slist int compare) [ 3; 1; 2 ] [ 1; 2; 3 ]);
      test "contramap projects before comparing" (fun () ->
          let by_length = contramap String.length int in
          equal by_length "abc" "xyz");
      cases "ports parse" ~name:Fun.id [ "1"; "80"; "8080"; "65535" ]
        (fun input -> ignore (require_ok (parse_port input)));
    ]

(* ───── property-testing.md ───── *)

type shape = Circle of float | Rect of float * float

let pp_shape ppf = function
  | Circle r -> Format.fprintf ppf "Circle %g" r
  | Rect (w, h) -> Format.fprintf ppf "Rect (%g, %g)" w h

let area = function Circle r -> Float.pi *. r *. r | Rect (w, h) -> w *. h

let gen_shape =
  Gen.(
    one_of
      [
        map (fun r -> Circle r) (float_range 0. 100.);
        map
          (fun (w, h) -> Rect (w, h))
          (pair (float_range 0. 100.) (float_range 0. 100.));
      ])
  |> Gen.with_pp pp_shape

let gen_rect =
  Gen.(
    let+ w = float_range 0. 10. and+ h = float_range 0. 10. in
    Rect (w, h))
  |> Gen.with_pp pp_shape

let encode l = String.concat "," (List.map string_of_int l)

let decode = function
  | "" -> []
  | s -> List.map int_of_string (String.split_on_char ',' s)

let properties =
  group "properties"
    [
      prop "area non-negative" gen_shape (fun s ->
          is_true (Float.compare (area s) 0. >= 0));
      prop "rect area matches the formula"
        ~examples:[ Rect (2., 0.) ]
        gen_rect
        (fun s ->
          match s with
          | Rect (w, h) -> equal (float 1e-9) (w *. h) (area s)
          | Circle _ -> ());
      prop "decode inverts encode"
        Gen.(list small_int)
        (fun l -> equal (list int) l (decode (encode l)));
      prop "division round-trips"
        Gen.(pair small_int small_int)
        (fun (a, b) ->
          assume (b <> 0);
          equal int a ((a / b * b) + (a mod b)));
      prop "parity is exercised" ~count:200 Gen.small_int (fun n ->
          cover ~label:"even" ~at_least:20. (n mod 2 = 0);
          cover ~label:"odd" ~at_least:20. (n mod 2 <> 0);
          classify "zero" (n = 0);
          equal int n n);
    ]

(* ───── snapshots-and-expect.md ───── *)

let help () =
  String.concat "\n"
    [
      "Usage: mytool [OPTIONS] COMMAND";
      "";
      "Commands:";
      "  build    Build the project";
      "  test     Run the tests";
    ]

let snapshots =
  group "cli"
    [
      test "cli help" (fun () -> snapshot "help" (help ()));
      test "greeting goes through capture" (fun () ->
          print_string "Hello, World!\n";
          equal string "Hello, World!\n" (output ()));
    ]

(* ───── resources-and-structure.md ───── *)

module Db = struct
  type t = { mutable rows : string list; mutable open_ : bool }

  let connect () = { rows = []; open_ = true }
  let close db = db.open_ <- false
  let insert db row = db.rows <- row :: db.rows
  let count db = List.length db.rows
end

module Server = struct
  type t = { mutable running : bool }

  let start () = { running = true }
  let stop server = server.running <- false
  let ping server = server.running
end

let with_db = bracket ~setup:Db.connect ~teardown:Db.close
let server = fixture ~teardown:Server.stop Server.start
let backends = [ ("list", 12); ("array", 12); ("bigarray", 12) ]

let resources =
  group "storage"
    [
      with_db "insert then get" (fun db ->
          Db.insert db "alice";
          equal int 1 (Db.count db));
      test "responds" (fun () -> is_true (Server.ping (server ())));
      test "writes a config" (fun () ->
          let dir = temp_dir () in
          let file = Filename.concat dir "config.json" in
          Out_channel.with_open_text file (fun oc ->
              Out_channel.output_string oc "{}");
          is_true (Sys.file_exists file));
      test "backend contract" (fun () ->
          List.iter
            (fun (name, count) -> subtest name (fun () -> equal int 12 count))
            backends);
      test "stable stochastic input" (fun () ->
          let st = srandom () in
          let a = Random.State.int st 1000 in
          let b = Random.State.int st 1000 in
          is_true (a >= 0 && b >= 0));
      test "artifacts keyed by test identity" (fun () ->
          let key = String.concat "-" (current_test ()) in
          is_true (String.length key > 0));
      xfail ~reason:"issue #42"
        (test "known bug stays in-tree" (fun () ->
             equal int 8080 (require_match tcp_port (resolve "http"))));
      slow "big input" ~timeout:60. (fun () -> is_true true);
    ]

let () =
  run "manual" [ getting_started; assertions; properties; snapshots; resources ]
