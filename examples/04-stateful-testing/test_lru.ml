open Windtrap

module Model = struct
  (* The bindings, most recently used first. *)
  type t = { capacity : int; mutable entries : (int * int) list }

  let create capacity = { capacity; entries = [] }

  let add m k v =
    let rest = List.remove_assoc k m.entries in
    m.entries <- (k, v) :: List.filteri (fun i _ -> i < m.capacity - 1) rest

  let find m k =
    let found = List.assoc_opt k m.entries in
    Option.iter (add m k) found;
    found

  let length m = List.length m.entries
  let to_list m = m.entries
end

let bindings = list (pair int int)

let cache =
  abstract "c"
    ~pp:(fun ppf m -> Testable.pp bindings ppf m.Model.entries)
    ~invariant:(fun m c -> equal bindings (Model.to_list m) (Lru.to_list c))

let key = Gen.int_range 0 3
let value = Gen.int_range 0 9

let commands =
  [
    command "create" (Gen.int_range 1 3 @-> makes cache) Model.create Lru.create;
    command "add" (cache ^-> key @-> value @-> returns unit) Model.add Lru.add;
    command "find" (cache ^-> key @-> returns (option int)) Model.find Lru.find;
    command "length" (cache ^-> returns int) Model.length Lru.length;
    command "to_list" (cache ^-> returns bindings) Model.to_list Lru.to_list;
  ]

let caches = group "Lru" [ stateful "behaves like a list" commands ]
let () = exit (run "lru" [ caches ])
