module type Scheduler = sig
  type t
  val empty : t
  val next : t -> t
  val current : t -> cont
  val is_empty : t -> bool
  val pop : t -> t
  val push : cont -> t -> t
end

module RoundRobin : Scheduler = struct
  type t = cont list * cont list

  let empty = [], []

  let next = function
    | l, h :: [] -> [], List.rev (h :: l)
    | l, h :: t -> h :: l, t
    | _ -> assert false

  let current = function
    | _, h :: t -> h
    | [], [] -> raise Not_found
    | _ -> assert false

  let is_empty (l, p) = l = [] && p = []

  let pop sch =
    print_endline "pop";
    match sch with
    | l, h :: [] -> [], List.rev l
    | l, h :: t -> l, t
    | _ -> assert false

  let push c sch =
    print_endline "push";
    match sch with
    | l, h :: t -> l, h :: c :: t
    | l, [] -> l, [c]
end
