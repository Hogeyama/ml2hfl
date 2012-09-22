open ExtList
open ExtString
open HornClause
open HornClauseEc

(** Utility functions on Horn clauses *)

let subst_hcs hcs (Hc(popt, atms, t) as hc) =
  let _ = Global.log_begin "HornClauseUtil.subst_hcs" in
  let _ = Global.log (fun () -> Format.printf "input:@,  @[<v>%a@]@," pr_elem hc) in
  let hc =
    if !Global.subst_hcs_inc then
      let rec aux atms t =
        try
          let latms, (pid, ttys), ratms =
            Util.pick (fun (pid, _) -> mem pid hcs) atms
          in
          let _ = Global.log (fun () -> Format.printf "%a is substituted@," Var.pr pid) in
          let Hc(_, atms, t) =
            let atms, t' = lookup_nd (pid, ttys) hcs in
            simplify [] (Hc(popt, latms @ atms @ ratms, Formula.band [t; t']))
          in
          aux atms t
        with Not_found ->
          Hc(popt, atms, t)
      in
      aux atms t
    else
      let atms, t =
        let atmss, ts =
          Util.unzip
            (List.map
              (fun (pid, ttys) ->
                try
                  let _ = Global.log (fun () -> Format.printf "%a is being substituted@," Var.pr pid) in
                  lookup_nd (pid, ttys) hcs
                with Not_found ->
                  [pid, ttys], Formula.ttrue)
              atms)
        in
        List.flatten atmss, Formula.band (t :: ts)
      in
      simplify [] (Hc(popt, atms, t))
  in
  let _ = Global.log (fun () -> Format.printf "output:@,  @[<v>%a@]" pr_elem hc) in
  let _ = Global.log_end "HornClauseUtil.subst_hcs" in
  hc

let subst_hcs_fixed hcs hc =
  Util.fixed_point
    (fun hc ->
      (*Format.printf "%a@," pr_elem hc;*)
      subst_hcs hcs hc)
    (fun hc1 hc2 ->
      match hc1, hc2 with
        Hc(_, atms1, _), Hc(_, atms2, _) -> Util.set_equiv atms1 atms2)
    hc
(*
let rec fixpoint hcs =
  match hcs with
    [] -> []
  | hc::hcs' ->
      let hc' = subst_hcs_fixed hcs' hc in
      hc' :: fixpoint (List.map (subst_hcs [hc']) hcs')
  
let subst_hcs_fixed hcs hc =
  subst_hcs (fixpoint hcs) hc
*)
