open Ast

let empty = {ints = []; bools = []; strings = []}
let bind env var value = (var, value) :: env
let find env var = List.assoc var env

let rec get : type a. a value -> a =
  function
  | VInt i -> i
  | VBool b -> b
  | VString s -> s
  | VUnit u -> u
  | VFun f -> f
  | VRef r -> ref (get r)

let rec eval : type a b. runtime -> a exp -> a value = fun rt ->
  function
  | Val v -> v
  | VarInt v -> find rt.ints v
  | VarBool v -> find rt.bools v
  | VarString v -> find rt.strings v

  | If (b, l, r) -> eval rt (if get (eval rt b) then l else r)
  | Gt (a, b) -> VBool (get (eval rt a) >  get (eval rt b))
  | Lt (a, b) -> VBool (get (eval rt a) <  get (eval rt b))
  | Le (a, b) -> VBool (get (eval rt a) <= get (eval rt b))
  | Ge (a, b) -> VBool (get (eval rt a) >= get (eval rt b))
  | Eq (a, b) -> VBool (get (eval rt a) =  get (eval rt b))
  | Ne (a, b) -> VBool (get (eval rt a) <> get (eval rt b))
  | Not a -> VBool (not (get (eval rt a)))
  | Concat (s, t) -> VString (get (eval rt s) ^ get (eval rt t))

  | Add (i, j) -> VInt (get (eval rt i) + get (eval rt j))
  | Sub (i, j) -> VInt (get (eval rt i) - get (eval rt j))
  | Mul (i, j) -> VInt (get (eval rt i) * get (eval rt j))
  | Div (i, j) -> VInt (get (eval rt i) / get (eval rt j))
  | Neg (i) -> VInt (- get (eval rt i))

  | If (b, e1, e2) -> eval rt (if get (eval rt b) then e1 else e2)
  | Ref e -> VRef (eval rt e)
  | Unref e ->
     begin match eval rt e with
     | VRef r -> r
     end

  | Fork v -> VUnit ()
  | Yield v -> VUnit ()
  | Let (v, e1, e2) ->
     let rt = begin match eval rt e1 with
              | VInt _ as e -> { rt with ints = bind rt.ints v e }
              | VBool _ as e -> { rt with bools = bind rt.bools v e }
              | VString _ as e -> { rt with bools = bind rt.strings v e }
              | _ -> failwith "Cannot bind."
              end in
     eval rt e2

  | PrintInt i ->
     let VInt i = eval rt i in print_int i; VUnit ()
  | PrintBool b ->
     let VBool b = eval rt b in
     print_string (if b then "true" else "false");
     VUnit ()
  | PrintString s ->
     let VString s = eval rt s in
     print_string s;
     VUnit ()
  | PrintRef _ -> print_string "ref"; VUnit ()
  | PrintUnit _ -> print_string "()"; VUnit ()
  | PrintFun _ -> print_string "fun"; VUnit ()
  | _ -> failwith "Not implemented"

let p = PrintInt (Val (VInt 42))
let p = Unref (Val (VRef (VInt 42)))
let _ = eval empty p

(* let main () =
 *   try
 *     let lexbuf = Lexing.from_channel stdin in
 *     while true do
 *       let result = Parser.main Lexer.token lexbuf in
 *       eval [] result
 *     done
 *   with Lexer.Eof ->
 *     exit 0 *)
