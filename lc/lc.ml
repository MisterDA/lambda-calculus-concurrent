type _ exp =
  | Var : string -> 'a exp
  | Abs : string * 'a exp -> 'a exp
  | App : 'a exp * 'a exp -> 'a exp
and eexp = Exp : 'a exp -> eexp
and env = (string * eexp) list

let eval : type a. env -> a exp -> a exp = fun env ->
  function
  | Var v -> let Exp x = List.assoc v env in x
