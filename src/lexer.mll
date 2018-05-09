{
open Lexing
open Parser

exception SyntaxError of string
}

let int = ['0'-'9'] ['0'-'9']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = parse
| white    { read lexbuf }
| newline  { Lexing.new_line lexbuf; read lexbuf }
| int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
| "true"   { BOOL true }
| "false"  { BOOL false }
| '"'      { read_string (Buffer.create 17) lexbuf }
| '('      { LPAREN }
| ')'      { RPAREN }
| ';'      { SEMICOLON }
| "fun"    { FUN }
| "->"     { ARROW }
| "let"    { LET }
| "="      { EQ }
| "in"     { IN }
| "if"     { IF }
| "then"   { THEN }
| "else"   { ELSE }
| "ref"    { REF }
| "!"      { UNREF }
| ":="     { ASSIGN }
| "print"  { PRINT }
| "fork"   { FORK }
| "yield"  { YIELD }
| "wait"   { WAIT }
| "not" as f { UNOP f }
| "+"      { BINOP "+" }
| "-"      { BINOP "-" }
| "*"      { BINOP "*" }
| "/"      { BINOP "/" }
| "<"      { BINOP "<" }
| ">"      { BINOP ">" }
| "<=" as f { BINOP f }
| ">=" as f { BINOP f }
| "==" as f { BINOP f }
| "!=" as f { BINOP f }
| "||" as f { BINOP f }
| "&&" as f { BINOP f }
| "^"      { BINOP "^" }
| id       { ID (Lexing.lexeme lexbuf) }
| eof      { EOF }
| _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
