type value =
  VInt of int
| VBool of bool
| VString of string
| VRef of value ref
| VFun of var * term
| VUnit

and var = string

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

and env = (var * value) list

let error str =
  failwith ("Typing mismatch: " ^ str ^ " expected.")

let rec eval env = function
  | Variable v -> List.assoc v env
  | Value v -> v
  | App (t1, t2) ->
     let v = eval env t2 in
     begin match eval env t1 with
     | VFun (x, t1) ->
        let env = (x, v) :: env in
        eval env t1
     | _ -> error "fun"
     end
  | Unop (op, t) ->
     let unop_bool op t =
       match eval env t with
       | VBool b -> VBool (op b)
       | _ -> error "bool"
     and unop_int op t =
       match eval env t with
       | VInt i -> VInt (op i)
       | _ -> error "int"
     in
     begin match op with
     | "not" -> unop_bool ( not ) t
     | "-" -> unop_int (( - )  0) t
     | _ -> failwith ("Unsupported unop: " ^ op)
     end
  | Binop (op, t1, t2) ->
     let binop_int_bool env op t1 t2 =
       match eval env t1, eval env t2 with
       | VInt i, VInt j -> VBool (op i j)
       | _ -> error "int"
     and binop_bool env op t1 t2 =
       match eval env t1, eval env t2 with
       | VBool i, VBool j -> VBool (op i j)
       | _ -> error "bool"
     and binop_string env op t1 t2 =
       match eval env t1, eval env t2 with
       | VString s, VString t -> VString (op s t)
       | _ -> error "string"
     in
     begin match op with
     | "+" -> binop_int_int env ( + ) t1 t2
     | "-" -> binop_int_int env ( - ) t1 t2
     | "*" -> binop_int_int env ( * ) t1 t2
     | "/" -> binop_int_int env ( / ) t1 t2
     | "<" -> binop_int_bool env ( < ) t1 t2
     | ">" -> binop_int_bool env ( > ) t1 t2
     | "<=" -> binop_int_bool env ( <= ) t1 t2
     | ">=" -> binop_int_bool env ( >= ) t1 t2
     | "&&" -> binop_bool env ( && ) t1 t2
     | "||" -> binop_bool env ( || ) t1 t2
     | "=" -> VBool (eval env t1 = eval env t2)
     | "<>" -> VBool (eval env t1 <> eval env t2)
     | "^" -> binop_string env ( ^ ) t1 t2
     | _ -> failwith ("Unsupported binop: " ^ op)
     end
  | Ref t -> VRef (ref (eval env t))
  | Unref t ->
     begin match eval env t with
     | VRef r -> !r
     | _ -> error "value ref"
     end
  | Assign (t1, t2) ->
     begin match eval env t1 with
     | VRef r -> r := eval env t2; VRef r
     | _ -> error "value ref"
     end
  | LetIn (v, t1, t2) ->
     let env = (v, eval env t1) :: env in
     eval env t2
  | IfThenElse (t1, t2, t3) ->
     begin match eval env t1 with
     | VBool b -> eval env (if b then t2 else t3)
     | _ -> error "bool"
     end
  | Print t -> print (eval env t)


and binop_int_int env op t1 t2 =
  match eval env t1, eval env t2 with
  | VInt i, VInt j -> VInt (op i j)
  | _ -> error "int"

and print v =
  begin match v with
  | VInt i -> Printf.printf "%d" i
  | VBool b -> Printf.printf "%b" b
  | VString s -> Printf.printf "\"%s\"" s
  | VRef r -> ignore (print !r); Printf.printf "ref"
  | VFun (x, t) -> Printf.printf "fun x -> _"
  | VUnit -> Printf.printf "()"
  end;
  VUnit


let p = Value (VInt 42)
let p = LetIn ("x", Value (VInt 42), Variable "x")
let id = Value (VFun ("x", Variable "x"))
let p = App (id, Value (VInt 42))
let p = App (Value (VFun ("_", Print (Value (VInt 43)))), Print (Value (VInt 42)))
let _ = eval [] p
