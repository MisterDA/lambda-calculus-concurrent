open Ast

let coroutines : coroutines ref = ref ([], [])

let rec eval env = function
  | Val v -> v
  | Var x -> find x env
  | Unop (op, e) ->
     begin match op with
     | Not -> begin match eval env e with
              | VBool b -> VBool (not b)
              | _ -> failwith "Typing mismatch. 1"
              end
     end
  | BinOp (op, e1, e2) ->
     let opint op e1 e2 =
       begin match eval env e1, eval env e2 with
       | VInt i, VInt j -> VInt (op i j)
       | _ -> failwith "Typing mismatch. 2"
       end in
     let opbool op e1 e2 =
       begin match eval env e1, eval env e2 with
       | VBool b, VBool c -> VBool (op b c)
       | _ -> failwith "Typing mismatch. 3"
       end in
     let opstring op e1 e2 =
       begin match eval env e2, eval env e2 with
       | VString s, VString t -> VString (op s t)
       | _ -> failwith "Typing mismatch. 4"
       end in
     begin match op with
     | Add -> opint ( + ) e1 e2  | Sub -> opint ( - ) e1 e2
     | Mul -> opint ( * ) e1 e2  | Div -> opint ( / ) e1 e2
     | Lt -> opbool ( < ) e1 e2  | Gt -> opbool ( > ) e1 e2
     | Le -> opbool ( <= ) e1 e2 | Ge -> opbool ( >= ) e1 e2
     | Eq -> opbool ( = ) e1 e2  | Ne -> opbool ( <> ) e1 e2
     | Concat -> opstring ( ^ ) e1 e2
     end
  | If (e, e1, e2) ->
     begin match eval env e with
     | VBool b -> eval env (if b then e1 else e2)
     | _ -> failwith "Typing mismatch. 5"
     end
  | Ref e -> VRef (ref (eval env e))
  | Unref e ->
     begin match eval env e with
     | VRef e -> !e
     | _ -> failwith "Typing mismatch. 6"
     end
  | Fork e -> coroutines := pushfront e !coroutines; VUnit
  | Yield e ->
     coroutines := pop !coroutines;
     coroutines := pushback e !coroutines;
     VUnit
  | Let (v, e1, e2) ->
     let env = bind v (eval env e1) env in
     eval env e2
  | Apply (e1, e2) ->
     begin match eval env e1 with
     | VFun (v, e) ->
        let env = bind v (eval env e2) env in
        eval env e
     | _ -> failwith "Typing mismatch. 7"
     end
  | Assign (r, e) ->
     begin match eval env r with
     | VRef r -> r := eval env e; VUnit
     | _ -> failwith "Typing mismatch. 8"
     end
  | Seq l -> List.fold_left (fun a i ->
                 match eval env i with
                 | VUnit -> VUnit
                 | _ -> failwith "Typing mismatch. 9") VUnit l
  | Print e ->
     begin match eval env e with
     | VInt i -> print_int i
     | VBool b -> print_string (if b then "true" else "false")
     | VString s -> print_string s
     | VUnit -> print_string "()"
     | VFun _ -> print_string "fun"
     | VRef _ -> print_string "ref"
     end;
     VUnit

let eval_concurrent p =
  coroutines := pushfront p !coroutines;
  while not (is_empty !coroutines) do
    ignore(eval [] (current !coroutines))
  done

let p = Let ("x", Ref (Val (VInt 0)),
             Seq [Print (Unref (Var "x"));
                  Assign (Var ("x"), Val (VInt 3));
                  Print (Unref (Var "x"))])
let p = Seq [ Print (Val (VInt 0));
              Fork (Seq [Print (Val (VInt 1)); Yield (Val (VUnit))]);
              Fork (Seq [Print (Val (VInt 2)); Yield (Val (VUnit))]);
              Yield (Seq [Print (Val (VInt 3)); Yield (Val (VUnit))])]
let _ = eval_concurrent p
