open ExtList
open ExtString

(** Trace constraint solving *)

let interpolate_chk t1 t2 =
  try
    Formula.simplify (CsisatInterface.interpolate t1 t2)
  with CsisatInterface.No_interpolant ->
				if !Flags.debug && Cvc3Interface.implies t1 (Formula.bnot t2) then
				  let _ = Format.printf "an error has occurred because of CSIsat@." in
				  assert false
				else
						raise CsisatInterface.No_interpolant

let interpolate_widen closed t1 t2 tw1 tw2 =
  let interp =
				try
				  (try
				    let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr tw1 Term.pr tw2 in
				    interpolate_chk tw1 tw2
				  with CsisatInterface.No_interpolant ->
						  if closed then
						    if Term.equiv t2 tw2 then
						      raise CsisatInterface.No_interpolant
						    else
						      let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr tw1 Term.pr t2 in
						      interpolate_chk tw1 t2
						  else
						    if Term.equiv t1 tw1 then
						      raise CsisatInterface.No_interpolant
						    else
						      let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr t1 Term.pr tw2 in
						      interpolate_chk t1 tw2)
				with CsisatInterface.No_interpolant ->
				  (try
				    if (closed && Term.equiv t1 tw1) || (not closed && Term.equiv t2 tw2) then
				      raise CsisatInterface.No_interpolant
				    else
				      let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr t1 Term.pr t2 in
				      interpolate_chk t1 t2
				  with CsisatInterface.No_interpolant ->
				    raise CsisatInterface.No_interpolant)
  in
		let _ = Format.printf "interp_out: %a@ " Term.pr interp in
  interp

(** require: t1 and t2 share only variables that satisfy p *)
let interpolate_widen_bvs p closed t1 t2 tw1 tw2 =
		let t1 = Term.rename_fresh p t1 in
		let t2 = Term.rename_fresh p t2 in
		let tw1 = Term.rename_fresh p tw1 in
		let tw2 = Term.rename_fresh p tw2 in
  interpolate_widen closed t1 t2 tw1 tw2

let interpolate t1 t2 =
		let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr t1 Term.pr t2 in
		let interp = interpolate_chk t1 t2 in
		let _ = Format.printf "interp_out: %a@ " Term.pr interp in
  interp

(* require: t1 and t2 share only variables that satisfy p *)
let interpolate_bvs p t1 t2 =
		let t1 = Term.rename_fresh p t1 in
		let t2 = Term.rename_fresh p t2 in
  interpolate t1 t2


let widen xss ts =
  match ts with
    [t] -> t
  | _ ->
			  	(*
				  List.iter (fun xs -> Format.printf "%a@ " (Util.pr_list Var.pr ", ") xs) xss;
	  			*)
				  let xs = List.hd xss in
				  let ts =
				    List.map2
				      (fun ys t ->
				        let sub = List.combine ys xs in
				        Term.subst (fun x -> Term.make_var (List.assoc x sub)) t)
				      xss
				      ts
				  in
				  ApronInterface.widen (List.map Formula.bor (Util.nonemp_prefixes ts))
