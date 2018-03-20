type var = string
and env = (var * value) list
and coroutines = exp list * exp list

and value =
  | VInt of int
  | VBool of bool
  | VString of string
  | VUnit
  | VFun of (var * exp)
  | VRef of value ref

and exp =
  | Val of value
  | Var of string
  | Unop of unop * exp
  | BinOp of binop * exp * exp
  | If of exp * exp * exp
  | Ref of exp
  | Unref of exp
  | Fork of exp
  | Yield of exp
  | Let of var * exp * exp
  | Apply of exp * exp
  | Assign of exp * exp
  | Seq of exp list
  | Print of exp
and unop = Not
and binop =
  | Add | Sub | Mul | Div
  | Lt | Gt | Le | Ge | Eq | Ne
  | Concat

let bind v x env = (v, x) :: env
let find x env = List.assoc x env

let forward = function
  | l, h :: [] -> [], List.rev (h :: l)
  | l, h :: t -> h :: l, t
  | _ -> assert false

let current = function
  | _, h :: t -> h
  | [], [] -> raise Not_found
  | _ -> assert false

let is_empty (l, p) = l = [] && p = []

let pop = function
  | l, h :: [] -> [], List.rev l
  | l, h :: t -> l, t
  | _ -> assert false

let pushfront c = function
  | l, h :: t -> l, h :: c :: t
  | l, [] -> l, [c]

let pushback c (l, p) = c :: l, p
