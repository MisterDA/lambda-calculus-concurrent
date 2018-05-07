(* The Abstract Syntax Tree for the language. *)

type value =
  VInt of int
| VBool of bool
| VString of string
| VRef of value ref
| VFun of env * var * term
| VUnit

and term =
  Variable of var
| Value of value
| App of term * term
| Unop of string * term
| Binop of string * term * term
| Ref of term
| Unref of term
| Assign of term * term
| LetIn of var * term * term
| IfThenElse of term * term * term
| Print of term
| Fork of term
| Yield

and var = string
and env = (var * value) list
