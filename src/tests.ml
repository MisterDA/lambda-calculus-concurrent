open Ast
open Lc

let p = LetIn ("x", Value (VInt 42), Variable "x")
let v = eval' p

(* let x = 42 in
 * let f = fun _ -> x in
 * let x = 1337 in
 * print_int (f ()) *)
let p = LetIn ("x", Value (VInt 42),
               LetIn ("f", Value (VFun ([], fresh (), Variable "x")),
                      LetIn ("x", Value (VInt 1337),
                             Print (App (Variable "f", Value VUnit)))))
let p' = cps p

let v = eval' p

(* (fun x -> fun y -> y x) 42 (fun z -> z) *)
let p = App (App (Value (VFun ([], "x",
                               Value (VFun ([], "y",
                                            App (Variable "y", Variable "x"))))),
                  Value (VInt 42)),
             Value (VFun ([], "z", Variable "z")))
let v = eval' p


(* let id = Value (VFun ("x", Variable "x"))
 * let print_int i = Print (Value (VInt i))
 * let seq t1 t2 = App (Value (VFun (fresh (), t2)), t1)
 * let seqs terms =
 *   (\* let terms = List.rev terms in *\)
 *   match terms with
 *   | [] -> raise (Invalid_argument "terms expected")
 *   | h :: [] -> h
 *   | h :: t -> List.fold_left seq h t
 *
 * let p = seqs [print_int 0; print_int 1; print_int 2]
 *
 * (\* print 0; fork (print 1); fork (print 2); yield; print 3 *\)
 * let p = seqs [print_int 0; Fork (print_int 1); Fork (print_int 2);
 *               Yield; print_int 3]
 * let _ = eval' p
 * let p' = cps p
 * let _ = eval'' p *)
