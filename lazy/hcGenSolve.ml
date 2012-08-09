open ExtList
open ExtString
open HornClause
open HcSolve

let solve hcs =
  let _ = Global.log_begin "solving Horn clauses" in
  let lbs, ubs =
    let lbs = compute_lbs hcs in
    let _ = Global.log (fun () -> Format.printf "lower bounds:@,  %a@," TypPredSubst.pr lbs) in
    let ubs = compute_ubs lbs hcs in
    let _ = Global.log (fun () -> Format.printf "upper bounds:@,  %a@," TypPredSubst.pr ubs) in
    lbs, ubs
  in
  let pids = List.unique (pids hcs) in
  let pidss = Util.classify Var.cong pids in
  let _ = List.iter (fun pids -> Format.printf "pids: %a@," (Util.pr_list Var.pr ",") pids) pidss in
  let sol = assert false in
  let _ = Global.log_end "solving Horn clauses" in
  sol
