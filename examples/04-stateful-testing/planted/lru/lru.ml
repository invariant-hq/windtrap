(* Planted: [find] returns a hit without making its key the most recently
   used. *)

(* A cache of integer bindings that evicts its least recently used key.
   Each binding carries the tick of its last use. *)

type t = {
  capacity : int;
  table : (int, int * int) Hashtbl.t;
  mutable clock : int;
}

let create capacity = { capacity; table = Hashtbl.create 16; clock = 0 }

let tick c =
  c.clock <- c.clock + 1;
  c.clock

let evict c =
  let oldest =
    Hashtbl.fold
      (fun k (_, used) oldest ->
        match oldest with
        | Some (_, before) when before <= used -> oldest
        | Some _ | None -> Some (k, used))
      c.table None
  in
  Option.iter (fun (k, _) -> Hashtbl.remove c.table k) oldest

let add c k v =
  if (not (Hashtbl.mem c.table k)) && Hashtbl.length c.table >= c.capacity then
    evict c;
  Hashtbl.replace c.table k (v, tick c)

let find c k =
  match Hashtbl.find_opt c.table k with None -> None | Some (v, _) -> Some v

let length c = Hashtbl.length c.table

(* Most recently used first. *)
let to_list c =
  Hashtbl.fold (fun k (v, used) acc -> (used, (k, v)) :: acc) c.table []
  |> List.sort (fun (a, _) (b, _) -> Int.compare b a)
  |> List.map snd
