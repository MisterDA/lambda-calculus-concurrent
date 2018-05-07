type cont = Ast.term * Ast.env

module type Scheduler = sig
  type t
  val empty : t
  val current : t -> cont
  val is_empty : t -> bool
  val push_next : cont -> t -> t
  val push_back : cont -> t -> t
  val pop : t -> cont * t
  val size : t -> int
end

module RoundRobin : Scheduler = struct
  type t = cont list * cont list

  let empty = [], []
  let current = function
    | l, [] -> List.hd (List.rev l)
    | _, h :: _ -> h
  let is_empty sched = sched = empty
  let push_next term = function
    | l, r -> l, term :: r
  let push_back term = function
    | l, r -> term :: l, r
  let pop = function
    | l, h :: [] -> h, ([], List.rev l)
    | l, h :: r -> h, (l, r)
    | _ -> assert false
  let size (l, r) = List.length l + List.length r
end

module R = Random

module Random : Scheduler = struct
  type t = int * cont list

  let empty = 0, []
  let current (_, l) = List.hd l
  let is_empty sched = sched = empty
  let push_next term (s, l) = (s + 1, term :: l)
  let push_back = push_next
  let pop (s, l) =
    let n = R.int s in
    let rec aux i acc = function
      | h :: t when i = n -> h, List.rev_append acc t
      | h :: t -> aux (i + 1) (h :: acc) t
      | [] -> assert false
    in
    let h, l = aux 0 [] l in
    h, ((s - 1), l)
  let size (s, _) = s
end
