open Util
open Combinator
open Syntax

(* cが何番目か調べる *)
(*  c -> expr list -> int *)
let find_num c list =
  let f = function
    | Lambda (_, App (Const c', _)) -> c'
    | _ -> failwith "in_f error"
  in
  (* expr list -> c list *)
  let cs = List.map f list in
  let rec loop n = function
  | [] -> 0
  | x :: rest ->
    if x = c then n + 1
    else loop (n + 1) rest
  in loop 0 cs

let is_listfnct f =
  f = Sum [Constant "1"; Product [Constant "A"; Identity]]

(*
 ψ = out_Fであるかを判定する
*)
let is_out psi lf' =
  let cis =
    if is_listfnct lf' then 
      [Lambda (Tuple [], App (Const Nil, Tuple []));
       Lambda (Tuple [EVar (Var "a"); EVar (Var "as")],
	       App (Const Cons, Tuple [EVar (Var "a"); EVar (Var "as")]))]
    else failwith "F is not list's functor"
  in 
  let check = function
    | (App (Const con, arg), Tag (j, t)) ->
      arg = t && find_num con cis = j
    | _ -> false
  in
  match psi with
  | Lambda (arg, body) ->
    begin
      match arg with
      | EVar (Var vs) ->
	begin
	  match body with
	  | Case (EVar t0, ps) when t0 = Var vs ->
	    List.for_all check ps
	  | _ -> false
	end
      | _ -> false
    end
  | _ -> false
    
