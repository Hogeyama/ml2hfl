open Util
open Combinator
open Syntax

let isnot_recval = function
  | Var v -> not (Str.string_match (Str.regexp "v_[0-9]*'") v 0)
  | _ -> true

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

(* expr中の変数を全て取る *)
(* expr -> variable list *)
let rec get_variable = function
  | App (x, y) -> get_variable x @ get_variable y
  | Circle (x, y) -> get_variable x @ get_variable y
  | Plus xs -> List.concat (List.map get_variable xs)
  | InvTri xs -> List.concat (List.map get_variable xs)
  | EVar v -> [v]
  | Tuple xs -> List.concat (List.map get_variable xs)
  | _ -> []

(* phiがderive_tauを適用できる形か調べる *)
(* expr -> bool *)
let phi_check = function
  | InvTri phis ->
    let rec check = function
      | (EVar v, boundvars) -> List.mem v boundvars
      | (App (e, t), boundvars) ->
	begin
	  match e with
	  | Const c -> true
	  | EVar f -> List.for_all isnot_recval (get_variable t)
	  | _ -> failwith "false error"
	end
      | _ -> failwith "false error"
    in
    let get_lambda_body = function
      | Lambda (arg, body) ->
	(body, get_variable arg)
      | _ -> failwith "phi_i is not lambda"
    in
    List.for_all (fun x -> check (get_lambda_body x)) phis
  | _ -> failwith "phi is not InvTri"

let is_listfnct f =
  f = Sum [Constant "1"; Product [Constant "A"; Identity]]

(* τを求める関数 *)
let derive_tau (Hylo (phi, eta, psi, lg, lf)) lf' =
  let fs =
    match lf' with
    | Sum x -> x
    | _ -> [lf']
  in
  let cis =
    if is_listfnct lf' then 
      [Lambda (Tuple [], App (Const Nil, Tuple []));
       Lambda (Tuple [EVar (Var "a"); EVar (Var "as")],
	       App (Const Cons, Tuple [EVar (Var "a"); EVar (Var "as")]))]
    else failwith "F is not list's functor"
  in
  let c_num =
    match lf' with
    | Sum fs -> List.length fs
    | _ -> 1
  in
  let rec gen_clist = function
    | 0 -> []

    | n -> gen_clist (n - 1) @ [EVar (Var (Printf.sprintf "c_%d" n))]
  in
  let clist = gen_clist c_num in
  (* expr -> expr *)
  let rec algo_f = function
    | InvTri phis ->
        let body = List.map (fun p -> App (algo_f p, EVar (Var "cs"))) phis in
        Lambda (EVar (Var "cs"), InvTri body)
    | Lambda (args, ti) -> Lambda (InvTri clist, Lambda (args, algo_f' ti))
    | _ -> failwith "phi error"
  (* expr -> expr *)
  and algo_f' = function
    | EVar v ->
      if isnot_recval v then
	App (Banana (InvTri clist, lf'), EVar v)
      else EVar v
    | Tuple ts ->
      App (Banana (InvTri clist, lf'), Tuple ts)
    | App (s, t) ->
      begin
	match s with
	| Const c ->
	  let rec fi_algo_f' fi t =
	    match fi with
	    | Identity -> algo_f' t
	    | Constant s -> t
	    | Product fs' ->
	      let ts =
		match t with
		| Tuple xs -> xs
		| _ -> failwith "functor error"
	      in Tuple (List.map2 (fun f t -> fi_algo_f' f t) fs' ts)
	    | _ -> failwith "functor error"
	  in
	  let i = find_num c cis in
	  let fi = List.nth fs (i - 1) in
	  App (EVar (Var (Printf.sprintf "c_%d" i)), fi_algo_f' fi t)
(*	| EVar f ->
	  App (Banana (InvTri clist, lf'), App (s, t)) *)
	| _ -> failwith "App error"
      end
    | _ -> failwith "algo_f error"
  in
  if phi_check phi then algo_f phi else failwith "tau is not found"

(* 旧バージョン(Fとin_Fの定義を受け取る)
let derive_tau (Hylo (phi, eta, psi, lg, lf)) lf' in_f =
  let fs =
    match lf' with
    | Sum x -> x
    | _ -> [lf']
  in
  let cis =
    match in_f with
    | InvTri x -> x
    | _ -> [in_f]
  in
  let c_num =
    match lf' with
    | Sum fs -> List.length fs
    | _ -> 1
  in
  let rec gen_clist = function
    | 0 -> []
    | n -> gen_clist (n - 1) @ [Term (Tv (Var (Printf.sprintf "c_%d" n)))]
  in
  let clist = gen_clist c_num in
  let rec algo_f = function
    | InvTri phis ->
        let body = List.map (fun p -> App (algo_f p, Term (Tv (Var "cs")))) phis in
        Lambda (Term (Tt [Tv (Var "cs")]), InvTri body)
    | Lambda (args, ti) ->
      begin
	match ti with
	| Term t -> Lambda (InvTri clist, Lambda (args, algo_f' t))
	| _ -> failwith "lambda body error"
      end
    | _ -> failwith "phi error"
  (* term -> expr *)
  and algo_f' = function
    | Tv v ->
      if isnot_recval v then
	App (Banana (InvTri clist, lf'), Term (Tv v))
      else Term (Tv v)
    | Tt ts ->
      App (Banana (InvTri clist, lf'), Term (Tt ts))
    | Tc (c, t) ->
      let rec fi_algo_f' fi t =
	match fi with
	| Identity -> algo_f' t
	| Constant s -> Term t
	| Product fs' ->
	  let ts =
	    match t with
	    | Tt xs -> xs
	    | _ -> failwith "functor error"
	  in Tuple (List.map2 (fun f t -> fi_algo_f' f t) fs' ts)
	| _ -> failwith "functor error"
      in
      let i = find_num c cis in
      let fi = List.nth fs (i - 1) in
      App (Term (Tv (Var (Printf.sprintf "c_%d" i))), fi_algo_f' fi t)
    | Ta (f, t) ->
      App (Banana (InvTri clist, lf'), Term (Ta (f, t)))
  in
  algo_f phi
*)
