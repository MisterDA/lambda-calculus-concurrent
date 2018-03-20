%{ open Ast %}

%token <int> INT
%token <bool> BOOL
%token <string> VAR
%token <string> STR
%token <unit> UNIT
%token ADD SUB MUL DIV CONCAT GT LT LE GE EQ NE IF THEN ELSE END REF UNREF FORK
       YIELD NOT ASSIGN LET IN PRINT EOF

(* FIXME: what type should I use? *)
%start <Ast.eexp> main

%left GT LT LE GE EQ NE

%left ADD SUB
%left DIV MUL

%%

main:
  e = exp EOF { e }

exp:
  v = VAR   { Var v }

| i = INT    { EExp (Val (VInt i))    }
| b = BOOL   { EExp (Val (VBool b))   }
| s = STR    { EExp (Val (VString s)) }
| u = UNIT   { EExp (Val (VUnit u))   }

| i = exp ADD j = exp  { let EExp i, EExp j = i, j in EExp (Add (i, j)) }
| i = exp SUB j = exp  { let EExp i, EExp j = i, j in EExp (Sub (i, j)) }
| i = exp MUL j = exp  { let EExp i, EExp j = i, j in EExp (Mul (i, j)) }
| i = exp DIV j = exp  { let EExp i, EExp j = i, j in EExp (Div (i, j)) }
| i = exp LT j = exp   { let EExp i, EExp j = i, j in EExp (Lt (i, j)) }
| i = exp GT j = exp   { let EExp i, EExp j = i, j in EExp (Gt (i, j)) }
| i = exp LE j = exp   { let EExp i, EExp j = i, j in EExp (Le (i, j)) }
| i = exp GE j = exp   { let EExp i, EExp j = i, j in EExp (Ge (i, j)) }
| e1 = exp EQ e2 = exp { let EExp e1, EExp e2 = e1, e2 in EExp (Eq (e1, e2)) }
| e1 = exp NE e2 = exp { let EExp e1, EExp e2 = e1, e2 in EExp (Ne (e1, e2)) }
| NOT b = exp          { let EExp b = b in EExp (Not b) }
| s1 = exp CONCAT s2 = exp
  { let EExp s1, EExp s2 = s1, s2 in EExp (Concat (s1, s2)) }

(* | IF b = exp THEN l = exp ELSE r = exp END
 *   { let EExp b, EExp l, EExp r = b, l, r in EExp (If (b, l, r)) } *)
| REF e = exp
  { let EExp e = e in EExp (Ref e) }
| FORK e = exp
  { let EExp e = e in EExp (Fork e) }
| YIELD
  { EExp (Yield ()) }
| LET v = VAR EQ e1 = exp IN e2 = exp
  { let EExp e1, EExp e2 = e1, e2 in EExp (Let (v, e1, e2)) }
(* | e1 = exp e2 = exp
 *   { let EExp e1, EExp e2 = e1, e2 in EExp (Apply (e1, e2)) } *)
| PRINT e = exp
  { let EExp e = e in EExp (Print e) }
