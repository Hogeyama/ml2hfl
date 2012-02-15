open ExtList
open Term
open Formula
open NonLinArith

(** Farkas lemma *)

(** @param an unsatisfiable formula *)
let farkas t =
  let tss = dnf (elim_int_equality t) in
  let arss = List.map (List.map (fun t -> canonize_aif (aif_of t))) tss in
  let _ =
		  List.iter
		    (fun ars ->
		      Format.printf "canonized unsatisfiable constraints:@.  @[<v>%a@]@." (Util.pr_list pr_caif "@,") ars)
		    arss
  in
		List.map
				(fun ars ->
						let l0 = make_var (Var.new_var ()) in
						let ls = List.map (fun _ -> make_var (Var.new_var ())) ars in
						let vs = List.unique (Util.concat_map (fun (nxs, _) -> List.map snd nxs) ars) in
						let _ = if false then Format.printf "vars: %a@." (Util.pr_list Var.pr ",") vs in
						let tss =
						  (l0 :: List.map (fun _ -> tint 0) vs) ::
								List.map2
								  (fun (nxs, n) l ->
								    Term.mul l n ::
								    List.map
								      (fun v ->
								        try
								          Term.mul l
										          (Util.find_map
										            (fun (n, x) -> if Var.equiv x v then n else raise Not_found)
										            nxs)
								        with Not_found ->
								          tint 0)
								      vs)
								  ars ls
						in
						let cs::css = Util.transpose tss in
      let ts =
        List.map (fun l -> geq l (tint 0)) (l0::ls) @
        eqInt (sum cs) (tint (-1)) ::
								List.map (fun cs -> eqInt (sum cs) (tint 0)) css
      in
      let ts = List.map Formula.simplify ts in
      let ts = Formula.eqelim Var.is_coeff ts in
      exists
        (List.map (fun (Var(_, x)) -> x, SimType.Int) (l0::ls))
        (Formula.simplify (band ts)))
				arss
