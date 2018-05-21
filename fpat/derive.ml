(* 今後は副作用をどう扱うかも考える *)
open Util
open Combinator
open Syntax

(* アルゴリズムA *)
let algo_a ((declaration : decl), (global : string list)) =
  let composition list =
    let rec compose = function
      | [] -> (VSet.empty, VTSet.empty, [])
      | (v_set, vt_set, t) :: rest ->
	let (v_set', vt_set', t') = compose rest in
	(VSet.union v_set v_set', VTSet.union vt_set vt_set', t :: t')
    in
    let (ss, cs, ts) = compose list in
    match ts with
    | [t] -> (ss, cs, t)
    | t   -> (ss, cs, Tt t)
  in
  let Def (f, B (vs, t0, Al ps)) = declaration in

  let gen i =
    Printf.sprintf "v_%d'" i in

  (* アルゴリズムD *)
  let rec algo_d term =
    let i = ref 0 in
    match term with
    | Tv v ->
      begin
	match v with
     	| Var s    ->
	  if List.mem s global then (VSet.empty, VTSet.empty, Tv v)
	  else (VSet.singleton v, VTSet.empty, Tv v)
	| _ -> (VSet.empty, VTSet.empty, Tv v)
      end
    | Tt ts -> composition (List.map algo_d ts)
    | Ta (v, t) ->
      if v = f then
	let u = (incr i; gen !i) in
	(VSet.empty, VTSet.singleton (Var u, Ta (v, t)), Tv (Var u))
      else
	let (sv, cv, _)  = algo_d (Tv v)
	and (st, ct, tt) = algo_d t in
	(VSet.union sv st, VTSet.union cv ct, Ta (v, tt))
    | Tc (con, t) ->
      let (s, c, t') = algo_d t in
      (s, c, Tc (con, t'))
  in

  (* 各tiからパラメータFi, φi, ti'を導出 *)
  let ti_to_param ti =
    let (s, c, ti'') = algo_d ti in
    (* tiからFiを導出 *)
    let fi =
      if VSet.is_empty s && VTSet.is_empty c then [Constant "1"]
      else
	let rec loop s = function
	  | 0 -> []
	  | n -> s :: loop s (n - 1) in
      (* gammaを書く必要あり *)
	loop (Constant "A") (VSet.cardinal s) @ loop Identity (VTSet.cardinal c) in
    (* tiからphiiを導出 *)
    let phii =
      let vi_i_list  = VSet.elements s
      and v'i_i_list = List.map (fun (v'i_i, _) -> v'i_i) (VTSet.elements c) in
      (vi_i_list @ v'i_i_list, ti'') in
    (* tiからti'を導出 *)
    let ti' =
      let get_ti_i = function
	| (_, Ta (f, ti_i)) -> ti_i
	| _ -> failwith "ti' error"
      in
      let vi_i_list = List.map (fun v -> Tv v) (VSet.elements s)
      and ti_i_list = List.map get_ti_i (VTSet.elements c) in
      vi_i_list @ ti_i_list
    in
    (fi, phii, ti')
  in

  let get_param i (p, ti) =
    let (fi, phii, ti') = ti_to_param ti in
    ((fi, phii, ti'), (p, (i + 1, ti')))
  in
  
  let (fi_phii_ti'_list, patterns) = List.split (List.mapi get_param ps) in
  let psi = (vs, t0, patterns) in
  (* 出力 *)
  (f, (fi_phii_ti'_list, psi))


(* 三つ組リストを三つのリストにする *)
let rec unzip3  = function
  | [] -> ([], [], [])
  | (x, y, z) :: rest ->
    let (xs, ys, zs) = unzip3 rest in
    (x :: xs, y :: ys, z :: zs)

(* d to Hylo *)
let d_to_hylo d_result =
  let (name, (fi_phii_ti'_list, psi)) = d_result in
  match name with
  | Var s ->
    let varlist_to_exprlist vs = List.map (fun v -> EVar v) vs in
    let (fis, phiis, ti's) = unzip3 fi_phii_ti'_list in
    let newfis = List.map (fun xs -> match xs with [x] -> x | _ -> Product xs) fis in
    let gen_F = Sum newfis in
    let newphiis = List.map (fun (a, b) -> Lambda (Tuple (varlist_to_exprlist a), expr_of_term b)) phiis in
    let gen_phi = InvTri newphiis in
    let (arg, term, body) = psi in
    let newbody = List.map (fun (pattern, (tag, term)) -> (expr_of_p pattern, Tag (tag, expr_of_term (Tt term)))) body in
    let gen_psi =
      begin
	match arg with
	| V var -> Lambda (EVar var, Case (expr_of_term term, newbody))
	| Vs vars ->  Lambda (Tuple (varlist_to_exprlist vars), Case (expr_of_term term, newbody))
      end
    in
    (name, 
     Hylo (gen_phi,
	   Id,
	   gen_psi,
	   gen_F, gen_F))
  | _ -> failwith "The function's name is wrong."
    

(* test *)
let test1 = algo_a (Def (Var "sum",
		    B (V (Var "xs"),
		       Tv (Var "xs"),
		       Al ([(Pc (Nil, Pt []),
			     Tv (Intlit 0));
			    (Pc (Cons, Pt [Pv (Var "a"); Pv (Var "as")]),
			     Ta (Plus, Tt [Tv (Var "a"); Ta (Var "sum", Tv (Var "as"))]))
			   ])
		    )), []);;

let test2 = algo_a (Def (Var "rev",
		    B (V (Var "xs"),
		       Tv (Var "xs"),
		       Al ([(Pc (Nil, Pt []),
			     Tc (Nil, Tt []));
			    (Pc (Cons, Pt [Pv (Var "a"); Pv (Var "as")]),
			     Ta (Plus, Tt [Ta (Var "rev", Tv (Var "as")); Tc (Cons, Tt [Tv (Var "a"); Tc (Nil, Tt [])])]))
			   ])
		    )), []);;


let test3 = algo_a (Def (Var "map_g",
  B (V (Var "xs"), Tv (Var "xs"),
   Al
    [(Pc (Nil, Pt []), Tc (Nil, Tt []));
     (Pc (Cons, Pt [Pv (Var "a"); Pv (Var "as")]),
      Tc (Cons,
       Tt [Ta (Var "g", Tv (Var "a")); Ta (Var "map_g", Tv (Var "as"))]))])),
 ["g"])
