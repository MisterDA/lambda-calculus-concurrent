open Ast
open Lc

let () = Random.self_init ()

module RRI = Interpreter (Sched.RoundRobin)
module RI = Interpreter (Sched.Random)

let vint i = Value (VInt i)
let vunit = Value VUnit
let vstr s = Value (VString s)
let eq x y = Binop ("=", x, y)

(* print 0; fork (print 1); fork (print 2); yield; print 3 *)
let p = seqs [Print (Value (VInt 0));
              Fork (Print (Value (VInt 1)));
              Fork (Print (Value (VInt 2)));
              Yield;
              Print (Value (VInt 3))]

let _ = RRI.eval' p
let _ = RI.eval' p
let _ = RI.eval' p
let _ = RI.eval' p

(* let x = ref 1 in
   fork (if !x = 1 then x := 2);
   fork (if !x = 2 then x := 3);
   print !x
 *)
let p = LetIn ("x", Ref (vint 1),
               seqs [Fork (IfThenElse (eq (Unref (Variable "x")) (vint 1),
                                       Assign (Variable "x", vint 2),
                                       vunit));
                     Fork (IfThenElse (eq (Unref (Variable "x")) (vint 2),
                                       Assign (Variable "x", vint 3),
                                       vunit));
                     Print (Unref (Variable "x"))])
let _ = RRI.eval' p
let _ = RI.eval' p
let _ = RI.eval' p
let _ = RI.eval' p

(* let x = ref 3 in
   fork (if !x = 0 then print "done");
   fork (x := !x - 1)
 *)
let p = LetIn ("x", Ref (vint 3),
               seqs [Fork (IfThenElse (eq (Unref (Variable "x")) (vint 1),
                                       Print (vstr "done"), vunit));
                     Fork (Assign (Variable "x",
                                   Binop ("+",
                                          Unref (Variable "x"),
                                          vint (-1))))])
let _ = RRI.eval' p
let _ = RI.eval' p
