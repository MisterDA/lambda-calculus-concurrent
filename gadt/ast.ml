type var = string
and _ env = (var * 'a value) list
and runtime = env



and  _ value =
  | VInt : int -> int value
  | VBool : bool -> bool value
  | VString : string -> string value
  | VUnit : unit -> unit value
  | VFun : ('a -> 'b) -> ('a -> 'b) value
  | VRef : 'a value ->'a ref value

and evalue = EVal : 'a value -> evalue
and l = evalue

and _ exp =
  | Val : 'a value -> 'a exp

  | VarInt : var -> 'a exp
  | VarBool : var -> 'a exp
  | VarString : var -> 'a exp

  | Gt : int exp * int exp -> bool exp
  | Lt : int exp * int exp -> bool exp
  | Le : int exp * int exp -> bool exp
  | Ge : int exp * int exp -> bool exp
  | Eq : 'a exp * 'a exp -> bool exp
  | Ne : 'a exp * 'a exp -> bool exp
  | Not : bool exp -> bool exp
  | Concat : string exp * string exp -> string exp

  | Add : int exp * int exp -> int exp
  | Sub : int exp * int exp -> int exp
  | Mul : int exp * int exp -> int exp
  | Div : int exp * int exp -> int exp
  | Neg : int exp -> int exp

  | If : bool exp * 'a exp * 'a exp -> 'a exp
  | Ref : 'a exp -> 'a ref exp
  | Unref : 'a ref exp -> 'a exp
  | Fork : 'a exp -> unit exp
  | Yield : unit -> unit exp
  | Let : var * 'a exp * 'b exp -> 'b exp
  (* | Apply : ('a exp -> 'b exp) exp * 'a exp -> 'b exp *)
  | PrintInt : int exp -> unit exp
  | PrintBool : bool exp -> unit exp
  | PrintString : string exp -> unit exp
  | PrintRef : 'a ref exp -> unit exp
  | PrintUnit : unit exp -> unit exp
  | PrintFun : ('a -> 'b) exp -> unit exp
