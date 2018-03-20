{
  open Parser
  exception Error of string
}

let digit = ['0'-'9']
let alpha = ['a'-'z''A'-'Z']

let var = alpha (alpha | digit)+
let str = '"' (alpha | digit)* '"'

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }

  | var as v    { VAR v }
  | digit+ as d { INT (int_of_string d) }
  | "true"      { BOOL true }
  | "false"     { BOOL false }
  | str as s    { STR s }
  | "()"        { UNIT }
  | "\\"        { LAMBDA }

  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }

  | "^" { CONCAT }

  | ">"  { GT }
  | "<"  { LT }
  | "<=" { LE }
  | ">=" { GE }
  | "="  { EQ }
  | "<>" { NE }

  | "if"    { IF     }
  | "then"  { THEN   }
  | "else"  { ELSE   }
  | "end"   { END    }
  | "ref"   { REF    }
  | "fork"  { FORK   }
  | "yield" { YIELD  }
  | "not"   { NOT    }
  | ":="    { ASSIGN }
  | "!"     { UNREF  }
  | "let"   { LET    }
  | "in"    { IN     }

  | "print" { PRINT  }

  | eof { EOF }
  | _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n"
                        (Lexing.lexeme_start lexbuf))) }
