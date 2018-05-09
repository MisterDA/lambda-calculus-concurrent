%{
  open Ast
  open Lc
%}

%token <int> INT
%token <string> ID STRING UNOP BINOP
%token <bool> BOOL
%token <unit> UNIT
%token LPAREN RPAREN SEMICOLON REF UNREF ASSIGN PRINT LET IN EQ FORK YIELD WAIT
   EOF IF THEN ELSE FUN ARROW

%start <Ast.term> prog
%%

prog:
  e = expr EOF { e }

expr:
  e = expr_seq { e }

expr_seq:
  l = separated_nonempty_list(SEMICOLON, expr_noseq) { seqs l }

expr_noseq:
  e = expr_simple { e }
| e1 = expr_simple e2 = expr_simple { App (e1, e2) }
| e1 = expr_simple ASSIGN e2 = expr_simple { Assign (e1, e2) }
| e1 = expr_simple op = BINOP e2 = expr_simple { Binop (op, e1, e2) }

expr_simple:
  LPAREN e = expr RPAREN { e }
| x = ID { Variable x }
| v = value { Value v }
| op = UNOP e = expr_simple { Unop (op, e) }
| REF e = expr_simple { Ref e }
| UNREF e = expr_simple { Unref e }
| LET x = ID EQ e1 = expr_simple IN e2 = expr_simple { LetIn (x, e1, e2) }
| IF b = expr_simple THEN e1 = expr_simple ELSE e2 = expr_simple
  { IfThenElse (b, e1, e2) }
| PRINT e = expr_simple { Print e }
| FORK e = expr_simple { Fork e }
| WAIT e = expr_simple { Wait e }
| YIELD { Yield }

value:
  i = INT { VInt i }
| s = STRING { VString s }
| b = BOOL { VBool b }
| UNIT { VUnit }
| FUN x = ID ARROW e = expr_simple { VFun ([], x, e) }
