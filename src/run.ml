open Ast
open Lc

module RRI = Interpreter (Sched.RoundRobin)
module RI = Interpreter (Sched.Random)

let main () =
  let n = Array.length (Sys.argv) in
  if n < 2 || n > 3 then
    failwith "Usage: ./run.native <sched> [file]";
  let sched = Sys.argv.(1) in
  let toplevel = n = 2 in
  let input =
    if toplevel then begin
      print_endline "Ctrl-D to execute.";
      stdin
      end
    else
      let filename = Sys.argv.(2) in
      open_in filename
  in

  let filebuf = Lexing.from_channel input in
  Random.self_init ();

  try
    let rec aux () =
      if toplevel then begin print_string "> "; flush stdout end;
      let p = Parser.prog Lexer.read filebuf in
      if sched = "RRI" then RRI.eval' p
      else if sched = "RI" then RI.eval' p
      else failwith "Available interpreters: RRI (RoundRobinInterpreter), RI (RandomInterpreter)";
      if toplevel then aux ()
    in aux ()
  with
  | Parser.Error ->
     Printf.eprintf "At offset %d: syntax error.\n%!"
       (Lexing.lexeme_start filebuf);
  close_in input

let () = main ()
