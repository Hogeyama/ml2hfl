open Util
open Combinator
open Syntax

let isnot_recval = function
  | Var v -> not (Str.string_match (Str.regexp "v_[0-9]*'") v 0)
  | _ -> true

let rec unzip = function
  | [] -> ([], [])
  | (x, y) :: rest ->
    let (x', y') = unzip rest in
    (x :: x', y :: y')

let rec zip = function
  | ([], []) -> []
  | (x :: xs, y :: ys) ->
    (x, y) :: zip (xs, ys)
  | _ -> failwith "zip error"

let rec zip3 = function
  | ([], [], []) -> []
  | (x :: xs, y :: ys, z :: zs) ->
    (x, y, z) :: zip3 (xs, ys, zs)
  | _ -> failwith "zip3 error"

let composition list =
  let rec compose = function
    | [] -> (VESet.empty, [])
    | (ve_set, t) :: rest ->
      let (ve_set', t') = compose rest in
      (VESet.union ve_set ve_set', t :: t')
    in
    let (ms, ts) = compose list in
    match ts with
    | [t] -> (ms, t)
    | t   -> (ms, Tuple t)

let gen i j =
  Printf.sprintf "u%d_%d" i j

(* アルゴリズムS *)
let algo_s (Hylo (phi, eta, psi, lg, lf)) =
  let i = ref 0
  and j = ref 0 in

  (* アルゴリズムE *)
  (* expr -> VSet.t -> expr * expr *)
  let rec algo_e term sr =
    (* expr -> bool *)
    let rec var_sr = function
      | EVar v ->
	not (VSet.mem v sr)
    (*    | Tuple ts -> List.for_all var_sr ts *)
      | _ -> false
    in
    match term with
    | EVar v ->
      if var_sr term then 
	let u = (incr j; gen !i !j) in
	(VESet.singleton (Var u, term), EVar (Var u))
      else (VESet.empty, term)
    | Tuple ts ->
      let ts' = List.map (fun ti -> algo_e ti sr) ts in
      let ti's = List.map snd ts' in
      if ts <> [] && List.for_all var_sr ti's then
	let u = (j := !j - (List.length ti's); incr j; gen !i !j) in
	(VESet.singleton (Var u, term), EVar (Var u))
      else
	composition ts'
    | App (s, t) ->
      begin
	match s with
	| EVar v ->
	  let (wv, tv') = algo_e s sr
	  and (wt, tt') = algo_e t sr in
	  if var_sr tv' && var_sr tt' then
	    let u = (decr j; decr j; incr j; gen !i !j) in
	    (VESet.singleton (Var u, term), EVar (Var u))
	  else
	    (VESet.union wv wt, App (tv', tt'))
	| Const con ->
	  let (w, t') = algo_e t sr in
	  if var_sr t' then
	    let u = (decr j; incr j; gen !i !j) in
	    (VESet.singleton (Var u, term), EVar (Var u))
	  else
	    (w, App (s, t'))
	| _ -> failwith "App error"
      end
    | _ -> failwith "term error"
  in
  let phiis =
    match phi with
    | InvTri l -> l
    | e -> [e]
  in
  (* phiis' = (([var], [recvar]), body) list *)
  let phiis' =
    List.map
      (fun x ->
	match x with
	| Lambda (a, b) ->
	  begin
	    match a with
	    | Tuple arg ->
	      let exprlist_to_varlist ts =
		List.map (fun t -> match t with EVar v -> v | _ -> failwith "lambda arg error") ts in
	      (List.partition isnot_recval (exprlist_to_varlist arg), b)
 	    | _ -> failwith "phi's structure error"
	  end
	| _ -> failwith "phi's structure error"
      ) phiis in
  let rec vset_of_list = function
    | [] -> VSet.empty
    | x :: rest -> VSet.add x (vset_of_list rest) in
  let vars = List.map (fun ((x, _), _) -> x) phiis' in
  let recvars = List.map (fun ((_, x), _) -> x) phiis' in
  let algo_phiis =
    List.mapi (fun n ((vars, recvars), body) -> i := n + 1; j := 0; algo_e body (vset_of_list recvars)) phiis' in
  (* 下の引数のphii = algo_phiis の要素 *)
  let get_uistis_ti' phii =
    let (w, t) = phii in
    (unzip (VESet.elements w), t) in
  let phii' =
    List.map (fun ((w, ti'), recvar) -> (fst w @ recvar, ti')) (zip (List.map get_uistis_ti' algo_phiis, recvars)) in
  let etais =
    List.map
      (fun ((w, ti'), var, recvar) ->
	let trecvar = List.map (fun v -> EVar v) recvar in
	(var @ recvar, Tuple (snd w @ trecvar))
      )
      (zip3 (List.map get_uistis_ti' algo_phiis, vars, recvars)) in
  let gi's =
    List.map
      (fun ((w, _), recvar) ->
	if fst w = [] && List.length recvar = 0 then [Constant "1"]
	else
	  let rec gamma_loop = function
	    | [] -> []
	    | u :: rest -> (*gamma u*)(Constant "A") :: gamma_loop rest
	  in
	  let rec id_loop = function
	    | 0 -> []
	    | n -> Identity :: id_loop (n - 1)
	  in
	  gamma_loop (fst w) @ id_loop (List.length recvar)
      )
      (zip (List.map get_uistis_ti' algo_phiis, recvars)) in
  let varlist_to_expr = function
    | [v] -> EVar v
    | vs -> Tuple (List.map (fun v -> EVar v) vs)
  in
  let newphii's = List.map (fun (arg, body) -> Lambda (varlist_to_expr arg, body)) phii' in
  let gen_phi' = InvTri newphii's in
  let neweta = List.map (fun (arg, body) -> Lambda (varlist_to_expr arg, body)) etais in
  let gen_eta = Circle (Plus neweta, eta) in
  let newgi's = List.map (fun xs -> match xs with [x] -> x | _ -> Product xs) gi's in
  let gen_G' = Sum newgi's in
  Hylo (gen_phi', gen_eta, psi, gen_G', lf)
