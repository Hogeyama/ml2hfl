(*
sort nat == {n:int | n >= 0}
*)

type cl_list = Nil | Cons of closure * cl_list

(*
refine 'a list with nat =
  nil(0) | {n:nat} cons(n+1) of 'a * 'a list(n)
*)

(*
datatype lambda_exp =
  {n:nat} One(n+1)
| {n:nat} Shift(n+1) of lambda_exp(n)
| {n:nat} Lam(n) of lambda_exp(n+1)
| {n:nat} App(n) of lambda_exp(n) * lambda_exp(n)
*)
and lambda_exp =
  One
| Shift of lambda_exp
| Lam of lambda_exp
| App of lambda_exp * lambda_exp

(*
datatype closure =  {n:nat} Closure of lambda_exp(n) * closure list(n);;
*)
and closure = Closure of lambda_exp * cl_list

exception Unreachable

let callbyvalue(exp) =
  let rec cbv = function
      (One, Cons(clo, _)) -> clo
    | (Shift(exp), Cons(_, env)) -> cbv(exp, env)
    | (Lam _ as exp, env) -> Closure(exp, env)
    | (App(exp1, exp2), env) ->
        let (Closure(Lam body, env1)) = cbv(exp1, env) and clo = cbv(exp2, env) in
        cbv(body, Cons(clo, env1))
    | _ -> raise Unreachable (* this can be safely eliminated *)
(*
    withtype {n:nat} lambda_exp(n) * closure list(n) -> closure
*)
  in cbv(exp, Nil)
(*
withtype lambda_exp(0) -> closure
*)
(* Note: callbyvalue can only apply to CLOSED lambda expressions *)
