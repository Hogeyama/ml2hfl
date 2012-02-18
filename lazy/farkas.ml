open ExtList
open Term
open Formula
open ParLinArith

(** Farkas lemma *)

(** @param an unsatisfiable formula *)
let farkas t =
  let tss = dnf (elim_neq_int t) in
  let aifss = List.map (List.map (fun t -> canonize_aif (try aif_of t with Invalid_argument _ -> assert false))) tss in
  let _ =
		  List.iter
		    (fun aifs ->
		      Format.printf "canonized unsatisfiable constraints:@.  @[<v>%a@]@." (Util.pr_list pr_aif "@,") aifs)
		    aifss
  in
		List.map
				(fun aifs ->
      let aifs = (Const.Geq, [], tint 1) :: aifs in
						let ls = List.map (fun _ -> make_var (Var.new_var ())) aifs in
						let vs = List.unique (Util.concat_map (fun (_, nxs, _) -> List.map snd nxs) aifs) in
						let _ = if false then Format.printf "vars: %a@." (Util.pr_list Var.pr ",") vs in
						let tss =
								List.map2
								  (fun (_, nxs, n) l ->
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
								  aifs ls
						in
						let cs::css = Util.transpose tss in
      let ts =
        Util.filter_map2 (fun (c, _, _) l -> match c with Const.EqInt -> None | Const.Geq -> Some(geq l (tint 0)) | _ -> assert false) aifs ls @
        eqInt (sum cs) (tint (-1)) ::
								List.map (fun cs -> eqInt (sum cs) (tint 0)) css
      in
(*
      let _ = Format.printf "constraint1:@.  @[<v>%a@]@." Term.pr (Formula.band ts) in
*)
      let ts = List.map Formula.simplify ts in
(*
      let _ = Format.printf "constraint2:@.  @[<v>%a@]@." Term.pr (Formula.band ts) in
*)
      let ts = Formula.eqelim Var.is_coeff ts in
      exists
        (List.map (fun (Var(_, x)) -> x, SimType.Int) ls)
        (Formula.simplify (band ts)))
				aifss
