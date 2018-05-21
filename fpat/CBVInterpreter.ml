open Util
open Combinator

(** Call-by-value interpreter *)

let ext_env v env = v :: List.map (Term.shift 1) env

(* @require e uses De Bruijn index *)
let rec eval env e =
  MLExp.para
    (object
      method fvar (Idnt.N(n)) = fun env ->
        try eval env (List.nth env n) with Not_found -> assert false
      method fcon c = fun env -> MLExp.mk_const c
      method fif ty _ r1 _ r2 _ r3 = fun env ->
        let v1 = r1 env in
        if v1 |> Formula.of_term |> Formula.is_true then r2 env
        else if v1 |> Formula.of_term |> Formula.is_false then r3 env
        else assert false
      method flet _ _ _ r1 _ r2 = fun env ->
        let v1 = r1 env in r2 (ext_env v1 env)
      method fletrec _ _ _ = fun env -> assert false
      method fevent _ _ _ = fun env -> assert false
      method fapp _ r1 _ rs = fun env ->
        let v1 = r1 env in
        let vs = List.map (feed env) rs in
        if MLExp.is_closure v1 then
          MLExp.let_closure v1
            (fun env' xty e [] ->
               let v = eval (ext_env (List.hd vs) env') e in
               eval env (MLExp.mk_app v (List.tl vs)))
        else if MLExp.is_const v1 then
          MLExp.let_const v1
            (fun c ->
               let cs = List.map (fun v -> MLExp.let_const v id) vs in
               match Const.eval (c :: cs) with
               | [c'] -> MLExp.mk_const c'
               | _ ->
                 MLExp.mk_app (MLExp.mk_const c) (List.map MLExp.mk_const cs))
        else
          begin
            Format.printf "error in %a@," MLExp.pr v1;
            assert false
          end
      method ffun xty e1 _ = fun env -> MLExp.mk_closure env xty e1
      method ffix xty e1 r1 = fun env -> r1 (ext_env (MLExp.mk_fix xty e1) env)
      method fcls env xty e1 _ = fun env -> MLExp.mk_closure env xty e1
      method ftuple _ _ _ = fun env -> assert false
      method fkon _ _ _ = fun env -> assert false
      method farray _ rs = fun env -> assert false
      method faget a _ n  = fun env -> assert false
      method faset a _ n _ m _ e  = fun env -> assert false
    end)
    e
    env

(** @test fib *)
let test_fib () =
  Format.printf "%a@," MLExp.pr MLExp.fib;
  let t = eval [] MLExp.fib in
  Format.printf "result: %a@," MLExp.pr t

(** @test fib in CPS *)
let test_fib_cps () =
  Format.printf "%a@," MLExp.pr MLExp.fib_cps;
  let t = eval [] MLExp.fib_cps in
  Format.printf "result: %a@," MLExp.pr t
