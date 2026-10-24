(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf

(* Values *)

type value =
  | Int of int
  | Float of float
  | Bool of bool
  | String of string
  | List of value list
  | Object of (string * value) list

(* A malformed sequence is U+FFFD, so the document is always UTF-8. *)
let add_string b s =
  Buffer.add_char b '"';
  let rec loop i =
    if i < String.length s then begin
      let d = String.get_utf_8_uchar s i in
      let n = Uchar.utf_decode_length d in
      (if not (Uchar.utf_decode_is_valid d) then Buffer.add_string b "\u{FFFD}"
       else
         match s.[i] with
         | '"' -> Buffer.add_string b "\\\""
         | '\\' -> Buffer.add_string b "\\\\"
         | '\n' -> Buffer.add_string b "\\n"
         | '\r' -> Buffer.add_string b "\\r"
         | '\t' -> Buffer.add_string b "\\t"
         | c when c < ' ' || c = '\127' ->
             Buffer.add_string b (strf "\\u%04x" (Char.code c))
         | _ -> Buffer.add_substring b s i n);
      loop (i + n)
    end
  in
  loop 0;
  Buffer.add_char b '"'

(* The shortest of the three that reads back. *)
let number f =
  if not (Float.is_finite f) then "null"
  else
    let reads p = float_of_string (strf "%.*g" p f) = f in
    strf "%.*g" (if reads 15 then 15 else if reads 16 then 16 else 17) f

let rec add b = function
  | Int n -> Buffer.add_string b (string_of_int n)
  | Float f -> Buffer.add_string b (number f)
  | Bool v -> Buffer.add_string b (string_of_bool v)
  | String s -> add_string b s
  | List vs ->
      Buffer.add_char b '[';
      List.iteri
        (fun i v ->
          if i > 0 then Buffer.add_string b ", ";
          add b v)
        vs;
      Buffer.add_char b ']'
  | Object fields ->
      Buffer.add_char b '{';
      List.iteri
        (fun i (k, v) ->
          if i > 0 then Buffer.add_string b ", ";
          add_string b k;
          Buffer.add_string b ": ";
          add b v)
        fields;
      Buffer.add_char b '}'

(* Tests *)

let vacuous failures =
  failures <> [] && List.for_all Failure.is_vacuous failures

let failures (r : Run.result) =
  match r.outcome with Fail failures -> failures | Pass | Skip _ -> []

let verdict (r : Run.result) =
  match r.outcome with
  | Pass -> "pass"
  | Skip _ -> "skip"
  | Fail _ when not r.counted -> "xfail"
  | Fail failures when vacuous failures -> "vacuous"
  | Fail _ -> "fail"

let reason (r : Run.result) =
  match r.outcome with
  | Skip (Some reason) -> [ ("reason", String reason) ]
  | Fail failures when r.counted && vacuous failures ->
      let text (f : Failure.t) =
        match f.kind with Vacuous t -> t.kept | _ -> ""
      in
      [ ("reason", String (String.concat "; " (List.map text failures))) ]
  | Pass | Skip None | Fail _ -> []

let failure (f : Failure.t) =
  Object
    (("headline", String (Report_sections.headline f))
    ::
    (match f.loc with
    | Some loc -> [ ("file", String loc.file); ("line", Int loc.line) ]
    | None -> []))

let property (r : Run.result) (s : Property.stats) =
  let label (label, cases) =
    Object [ ("label", String label); ("cases", Int cases) ]
  in
  let cover (c : Property.cover_status) =
    let share =
      match c.share with
      | None -> []
      | Some s ->
          [
            ( "share",
              Object
                [
                  ("demand", Float s.demand);
                  ("hits", Int s.hits);
                  ("cases", Int s.cases);
                  ("low", Float s.low);
                  ("high", Float s.high);
                ] );
          ]
    in
    Object
      ([
         ("label", String c.label);
         ("cases", Int c.hits);
         ("met", Bool c.satisfied);
       ]
      @ share)
  in
  [
    ("cases", Int (s.cases - s.examples));
    ("examples", Int s.examples);
    ("count", Int s.count);
    ("discards", Int s.discards);
  ]
  @ (match s.whole with
    | Some n -> [ ((if r.table then "rows" else "values"), Int n) ]
    | None -> [])
  @ [
      ("labels", List (List.map label s.collected));
      ("cover", List (List.map cover s.coverage));
    ]

let scope (r : Run.result) =
  Object (match r.prop_stats with Some s -> property r s | None -> [])

let test (r : Run.result) =
  Object
    ([ ("path", List (List.map (fun s -> String s) r.path)) ]
    @ [ ("verdict", String (verdict r)) ]
    @ reason r
    @ [
        ("attempts", Int r.attempts);
        ("duration", Float r.duration);
        ("failures", List (List.map failure (failures r)));
        ("scope", scope r);
      ])

(* Counts *)

let counts ~selected ~duration ~release_failures results =
  let count p = List.length (List.filter p results) in
  let is v (r : Run.result) = String.equal (verdict r) v in
  Object
    [
      ("tests", Int selected);
      ("passed", Int (count (is "pass")));
      ("failed", Int (count (is "fail") + List.length release_failures));
      ("vacuous", Int (count (is "vacuous")));
      ("skipped", Int (count (is "skip")));
      ("expected_failures", Int (count (is "xfail")));
      ( "flaky",
        Int (count (fun (r : Run.result) -> is "pass" r && r.attempts > 1)) );
      ("not_run", Int (max 0 (selected - List.length results)));
      ("release_failures", Int (List.length release_failures));
      ("duration", Float duration);
    ]

(* The document *)

(* One test per line, so a line-oriented tool reads a test at a time. *)
let render ~suite ~seed ~selected ~duration ~results ~release_failures () =
  let b = Buffer.create 4096 in
  Buffer.add_string b "{\"version\": 1, \"suite\": ";
  add_string b suite;
  Buffer.add_string b ", \"seed\": ";
  add_string b (Seed.to_string seed);
  Buffer.add_string b ", \"tests\": [";
  List.iteri
    (fun i r ->
      Buffer.add_string b (if i = 0 then "\n  " else ",\n  ");
      add b (test r))
    results;
  Buffer.add_string b "\n], \"counts\": ";
  add b (counts ~selected ~duration ~release_failures results);
  Buffer.add_string b "}\n";
  Buffer.contents b

(* Writing *)

let names_file target = Filename.check_suffix target ".json"

let path ~suite target =
  if names_file target then target
  else Filename.concat target (Os.sanitize_component suite ^ ".json")

let write ~suite ~seed ~selected ~duration ~results ~release_failures target =
  let file = path ~suite target in
  let document =
    render ~suite ~seed ~selected ~duration ~results ~release_failures ()
  in
  try
    if not (names_file target) then Os.mkdir_p (Filename.dirname file);
    Os.atomic_write ~path:file document
  with (Sys_error _ | Unix.Unix_error _) as e ->
    Os.warn
      (strf "could not write JSON report to %s: %s" (Os.display_path file)
         (Os.failure_reason ~path:file e))
