open ExtList
open Term
open Formula
open ParLinArith

(** Farkas lemma *)

let farkas_conjunct coeffs aifs =
  let _ = Global.log (fun () -> Format.printf "unsatisfiable constraints:@,  @[<v>%a@]@," (Util.pr_list pr_aif "@,") aifs) in
  let aifs = (Const.Geq, [], tint 1) :: aifs in
  let ls = List.map (fun _ -> make_var (Var.new_var ())) aifs in
  let vs = List.unique (Util.concat_map (fun (_, nxs, _) -> List.map snd nxs) aifs) in
  let _ = Global.log (fun () -> Format.printf "vars: %a@," Var.pr_list vs) in
  let tss =
    List.map2
      (fun (_, nxs, n) l ->
        Term.mul l n ::
        List.map
          (fun v ->
            try
              Term.mul l
                (Util.find_app
                  (fun (n, x) -> if Var.equiv x v then n else raise Not_found)
                  nxs)
            with Not_found ->
              tint 0)
          vs)
      aifs ls
  in
  let cs::css = Util.transpose tss in
  let ts =
    (if !Global.use_bit_vector then
      []
    else
      List.map (fun p -> geq (make_var p) (tint 0)) coeffs @
      Util.filter_map2 (fun (c, _, _) l -> match c with Const.EqInt -> None | Const.Geq -> Some(geq l (tint 0)) | _ -> assert false) aifs ls) @
    eqInt (sum cs) (tint (-1)) ::
    List.map (fun cs -> eqInt (sum cs) (tint 0)) css
  in
(*
  let ts = if !Global.use_bit_vector then ts else FormulaUtil.eqelim Var.is_coeff ts in
*)
  FormulaUtil.simplify (band ts)

(** @param t an unsatisfiable formula *)
let farkas t =
  let _ = Global.log_begin "farkas" in
  let _ = Global.log (fun () -> Format.printf "input: %a@," Term.pr t) in
  let coeffs = List.unique (coeffs t) in
  let t = FormulaUtil.simplify t in
  let t = if !Global.use_bit_vector then FormulaUtil.elim_eq_neq_int t else FormulaUtil.elim_neq_int t in
  let t = FormulaUtil.elim_eq_neq_boolean t in
  let t = FormulaUtil.elim_imply_iff t in
  let _ = Global.log (fun () -> Format.printf "imply&iff eliminated:%a@," Term.pr t) in
  let tss = FormulaUtil.dnf t in
  let _ = Global.log (fun () -> Format.printf "dnf: %a@," Term.pr (FormulaUtil.of_dnf tss)) in
  let aifss =
    List.map
      (List.map
        (fun t ->
          canonize_aif
            (try
              aif_of t
            with Invalid_argument _ ->
              let _ = Format.printf "%a@," Term.pr t in
              assert false)))
      tss
  in
  let res = List.map (farkas_conjunct coeffs) aifss in
  let _ = Global.log_end "farkas" in
  res
