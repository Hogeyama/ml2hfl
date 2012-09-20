open ExtList
open ExtString

(** Trace constraint solving *)

let interpolate_widen closed t1 t2 tw1 tw2 =
  let interp =
    try
      (try
        let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr tw1 Term.pr tw2 in
        CsisatInterface.interpolate_chk tw1 tw2
      with CsisatInterface.NoInterpolant ->
        if closed then
          if Term.equiv t2 tw2 then
            raise CsisatInterface.NoInterpolant
          else
            let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr tw1 Term.pr t2 in
            CsisatInterface.interpolate_chk tw1 t2
        else
          if Term.equiv t1 tw1 then
            raise CsisatInterface.NoInterpolant
          else
            let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr t1 Term.pr tw2 in
            CsisatInterface.interpolate_chk t1 tw2)
    with CsisatInterface.NoInterpolant ->
      (try
        if (closed && Term.equiv t1 tw1) || (not closed && Term.equiv t2 tw2) then
          raise CsisatInterface.NoInterpolant
        else
          let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr t1 Term.pr t2 in
          CsisatInterface.interpolate_chk t1 t2
      with CsisatInterface.NoInterpolant ->
        raise CsisatInterface.NoInterpolant)
  in
  let _ = Format.printf "interp_out: %a@ " Term.pr interp in
  interp

(** require: t1 and t2 share only variables that satisfy p *)
let interpolate_widen_bvs p closed t1 t2 tw1 tw2 =
  let t1 = TypSubst.fresh p t1 in
  let t2 = TypSubst.fresh p t2 in
  let tw1 = TypSubst.fresh p tw1 in
  let tw2 = TypSubst.fresh p tw2 in
  interpolate_widen closed t1 t2 tw1 tw2

let widen xss ts =
  match ts with
    [t] -> t
  | _ ->
      (*
      List.iter (fun xs -> Format.printf "%a@ " Var.pr_list xs) xss;
      *)
      let xs = List.hd xss in
      let ts =
        List.map2
          (fun ys t ->
            let sub = List.combine ys xs in
            TypSubst.subst (fun x -> Term.make_var (List.assoc x sub)) t)
          xss
          ts
      in
      ApronInterface.widen (List.map Formula.bor (Util.nonemp_prefixes ts))
