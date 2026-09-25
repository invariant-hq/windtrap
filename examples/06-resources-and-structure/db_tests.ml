open Windtrap

let with_db = bracket ~setup:Db.connect ~teardown:Db.close

let every_row_is_found db =
  let names = [ "alice"; "bob"; "carol" ] in
  List.iter (Db.insert db) names;
  List.iter
    (fun name -> subtest name (fun () -> is_true (Db.mem db name)))
    names

let a_duplicate_counts_once db =
  Db.insert db "alice";
  Db.insert db "alice";
  equal int 1 (Db.count db)

let database =
  group "database"
    [
      with_db "an insert adds one row" (fun db ->
          Db.insert db "alice";
          equal int 1 (Db.count db));
      cases ~name:Fun.id "a name is stored" [ "alice"; "bob"; "carol" ]
        (fun name ->
          let db = Db.connect () in
          Db.insert db name;
          is_true (Db.mem db name));
      with_db "every inserted row is found" every_row_is_found;
      xfail ~reason:"issue #42"
        (with_db "a duplicate row counts once" a_duplicate_counts_once);
    ]
