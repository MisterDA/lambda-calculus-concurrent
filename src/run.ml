open Ast
open Lc

module RRI = Interpreter (Sched.RoundRobin)
module RI = Interpreter (Sched.Random)

let main () =
  let sched = Sys.argv.(1) in
  let filename = Sys.argv.(2) in
  let input = open_in filename in
  let filebuf = Lexing.from_channel input in
  Random.self_init ();

  try
    let p = Parser.prog Lexer.read filebuf in
    if sched = "RRI" then RRI.eval' p
    else if sched = "RI" then RI.eval' p
    else failwith "Available interpreters: RRI (RoundRobinInterpreter), RI (RandomInterpreter)"
  with
  | Parser.Error ->
     Printf.eprintf "At offset %d: syntax error.\n%!"
       (Lexing.lexeme_start filebuf);
  close_in input

let () = main ()
