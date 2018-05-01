open Ast

let error str =
  failwith ("Typing mismatch: " ^ str ^ " expected.")

let rec eval env = function
  | Variable v -> List.assoc v env
  | Value (VFun (e, x, t)) -> VFun (e @ env, x, t)
  | Value v -> v
  | App (t1, t2) ->
     let v = eval env t2 in
     begin match eval env t1 with
     | VFun (e, x, t1) ->
        let env = (x, v) :: e in
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
     and binop_int_int env op t1 t2 =
       match eval env t1, eval env t2 with
       | VInt i, VInt j -> VInt (op i j)
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
  | Fork t -> failwith "fork"
  | Yield -> failwith "yield"

and print v =
  begin match v with
  | VInt i -> Printf.printf "%d" i
  | VBool b -> Printf.printf "%b" b
  | VString s -> Printf.printf "\"%s\"" s
  | VRef r -> ignore (print !r); Printf.printf "ref"
  | VFun (_, x, _) -> Printf.printf " x -> _"
  | VUnit -> Printf.printf "()"
  end;
  VUnit

let fresh =
  let i = ref 0 in
  fun () -> let v = "_" ^ string_of_int !i in incr i; v

let freshk =
  let i = ref 0 in
  fun () -> let v = "k" ^ string_of_int !i in incr i; v

let id =
  let x = fresh () in
  Value (VFun ([], x, Variable x))

let rec cps t =
  let cps' t =
    let k = freshk () in
    Value (VFun ([], k, App (Variable k, t)))
  in
  match t with
  | Variable _ -> cps' t
  | App (t1, t2) ->
     let k, m, n = freshk (), freshk (), freshk () in
     let t' = VFun ([], n, App (App (Variable m, Variable n), Variable k)) in
     let t'' = VFun ([], m, App (cps t2, Value t')) in
     Value (VFun ([], k, App (cps t1, Value t'')))
  | LetIn (x, t1, t2) ->
     cps (App (Value (VFun ([], x, t2)), t1))
  | Value (VFun (e, x, t)) -> cps' (Value (VFun (e, x, cps t)))
  | Value _ -> cps' t
  | Ref t -> cps' (Ref (App (cps t, id)))
  | Unref t -> cps' (Unref (App (cps t, id)))
  | Print t -> cps' (Print (App (cps t, id)))
  | Unop (op, t) -> cps' (Unop (op, App (cps t, id)))
  | Binop (op, t1, t2) -> cps' (Binop (op, App (cps t1, id), App (cps t2, id)))
  | Assign (t1, t2) -> cps' (Assign (App (cps t1, id), App (cps t2, id)))
  | IfThenElse (t, t1, t2) ->
     cps' (IfThenElse (App (cps t, id), App (cps t1, id), App (cps t2, id)))
  | Fork t -> cps' (Fork (App (cps t, id)))
  | Yield -> cps' Yield

let eval' p = eval [] (App (cps p, id))

let rec pprint = function
  | Variable x -> print_string x
  | App (t1, t2) ->
     pprint t1; print_string " "; pprint t2
  | LetIn (x, t1, t2) ->
     Printf.printf "let %s = " x; pprint t1; print_string " in "; pprint t2
  | Value v ->
     begin match v with
     | VInt i -> Printf.printf "%d" i
     | VBool b -> Printf.printf "%b" b
     | VString s -> Printf.printf "\"%s\"" s
     | VRef r -> Printf.printf "ref _"
     | VFun (_, x, t) -> Printf.printf "λ%s. (" x; pprint t; Printf.printf ")"
     | VUnit -> Printf.printf "()"
     end
  | _ -> print_string "___"
