
open Utilities
open CEGAR_syntax
open CEGAR_type
open CEGAR_print
open CEGAR_util

type result =
    Feasible of (string * CEGAR_syntax.typ) list * string list
  | Infeasible of CEGAR_syntax.ce

let init_cont ce sat n constr env _ = assert (ce=[]); constr, n, env

let assoc_def defs n t =
  let defs' = List.filter (fun (f,_,_,_,_) -> Var f = t) defs in
    List.nth defs' n

(* sat=true denotes constr is satisfiable *)
let rec check_aux pr ce sat n constr env defs t k =
  if false then Format.printf "check_aux[%d]: %a@." (List.length ce) print_term t;
  match t with
    | Const RandInt -> assert false
    | Const c -> k ce sat n constr env (Const c)
    | Var x -> k ce sat n constr env (Var x)
    | App(App(Const (And|Or|Lt|Gt|Leq|Geq|EqUnit|EqBool|EqInt|Add|Sub|Mul as op),t1),t2) ->
        check_aux pr ce sat n constr env defs t1 (fun ce sat n constr env t1 ->
        check_aux pr ce sat n constr env defs t2 (fun ce sat n constr env t2 ->
          k ce sat n constr env (make_app (Const op) [t1;t2])))
(*
  | App(Const (Event s), t) -> check_aux pr ce sat n constr env defs (App(t,Const Unit)) k
*)
    | App(Const RandInt, t) ->
        let r = new_id "r" in
        let env' = (r,typ_int)::env in
          check_aux pr ce sat n constr env' defs (App(t,Var r)) k
    | App(t1,t2) ->
        check_aux pr ce sat n constr env defs t1 (fun ce sat n constr env t1 ->
        check_aux pr ce sat n constr env defs t2 (fun ce sat n constr env t2 ->
          let t1',ts = decomp_app (App(t1,t2)) in
          let _,xs,_,_,_ = List.find (fun (f,_,_,_,_) -> Var f = t1') defs in
            if List.length xs > List.length ts
            then k ce sat n constr env (App(t1,t2))
            else
              let f,xs,tf1,e,tf2 = assoc_def defs (List.hd ce) t1' in
              let ts1,ts2 = take2 ts (List.length xs) in
              let aux = List.fold_right2 subst xs ts1 in
              let tf1' = aux tf1 in
              let tf2' = make_app (aux tf2) ts2 in
              let constr' = make_and tf1' constr in
              let ce' = List.tl ce in
              let n' = if sat then n+1 else n in
              let sat' = sat && Wrapper2.checksat env constr' in
                assert (List.length xs = List.length ts);
                assert (ts2 = []);
                pr t1' e;
                if e = [Event "fail"]
                then init_cont ce' sat' n' constr' env tf2'
                else (assert (e=[]); check_aux pr ce' sat' n' constr' env defs tf2' k)))
    | Let _ -> assert false
    | Fun _ -> assert false

let rec get_prefix ce n =
  match ce with
      [] -> assert false
    | [EventNode "fail" | EventNode "unit"] -> assert (n=0); ce
    | (BranchNode _)::ce' when n = 0 -> []
    | (BranchNode _ as c)::ce' -> c::get_prefix ce' (n-1)
    | c::ce' -> c::get_prefix ce' n

let check ce ((env,defs,main):prog) =
  let () = Format.printf "Spurious counter-example::\n%a\n@." CEGAR_print.print_ce ce in
  let tmp = get_time () in
  let () = if Flag.print_progress then Format.printf "\n(%d-3) Checking counter-example ... " !Flag.cegar_loop in
  let () = if false then Format.printf "ce:        %a@." CEGAR_print.print_ce ce in
  let ce' = flatten_map (function BranchNode n -> [n] | _ -> []) (List.tl ce) in
  let _,_,_,_,t = List.find (fun (f,_,_,_,_) -> f = main) defs in
  let pr _ _ = () in
  let constr,n,env' = check_aux pr ce' true 0 (Const True) [] defs t init_cont in
  let prefix = get_prefix ce (n+1) in
  let () = if false then Format.printf "prefix(%d): %a@." n CEGAR_print.print_ce prefix in
  let result =
    if Wrapper2.checksat env' constr
    then Feasible (env', Wrapper2.get_solution env' constr)
    else Infeasible prefix
  in
    if Flag.print_progress then print_msg "DONE!\n";
    add_time tmp Flag.time_cegar;
    result












let init_cont ce ce_br env _ = assert (ce=[]); List.rev ce_br

let assoc_def defs n t ce_br =
  let defs' = List.filter (fun (f,_,_,_,_) -> Var f = t) defs in
  let len = List.length defs' in
  let ce_br' = if len > 1 then (n=0)::ce_br else ce_br in
    ce_br', List.nth defs' n

let rec trans_ce ce ce_br env defs t k =
  if false then Format.printf "trans_ce[%d]: %a@." (List.length ce) print_term t;
  match t with
      Const RandInt -> assert false
    | Const c -> k ce ce_br env (Const c)
    | Var x -> k ce ce_br env (Var x)
    | App(App(Const (And|Or|Lt|Gt|Leq|Geq|EqUnit|EqBool|EqInt|Add|Sub|Mul as op),t1),t2) ->
        trans_ce ce ce_br env defs t1 (fun ce ce_br env t1 ->
        trans_ce ce ce_br env defs t2 (fun ce ce_br env t2 ->
          k ce ce_br env (make_app (Const op) [t1;t2])))
(*
    | App(Const (Event s), t) -> trans_ce ce constr env defs (App(t,Const Unit)) k
*)
    | App(Const RandInt, t) ->
        let r = new_id "r" in
        let env' = (r,typ_int)::env in
          trans_ce ce ce_br env' defs (App(t,Var r)) k
    | App(t1,t2) ->
        trans_ce ce ce_br env defs t1 (fun ce ce_br env t1 ->
        trans_ce ce ce_br env defs t2 (fun ce ce_br env t2 ->
          let t1',ts = decomp_app (App(t1,t2)) in
          let _,xs,_,_,_ = List.find (fun (f,_,_,_,_) -> Var f = t1') defs in
            if List.length xs > List.length ts
            then k ce ce_br env (App(t1,t2))
            else
              let ce_br',(f,xs,tf1,e,tf2) = assoc_def defs (List.hd ce) t1' ce_br in
              let ts1,ts2 = take2 ts (List.length xs) in
              let aux = List.fold_right2 subst xs ts1 in
              let tf2' = make_app (aux tf2) ts2 in
              let ce' = List.tl ce in
                assert (List.length xs = List.length ts);
                assert (ts2 = []);
                if e = [Event "fail"]
                then init_cont ce' ce_br' env tf2'
                else (assert (e=[]); trans_ce ce' ce_br' env defs tf2' k)))
    | Let _ -> assert false
    | Fun _ -> assert false

let trans_ce ce ((env,defs,main):prog) =
  if false then Format.printf "ce:        %a@." CEGAR_print.print_ce ce;
  let ce' = flatten_map (function BranchNode n -> [n] | _ -> []) (List.tl ce) in
  let _,_,_,_,t = List.find (fun (f,_,_,_,_) -> f = main) defs in
  let ce_br = trans_ce ce' [] [] defs t init_cont in
    ce_br





let print_ce_reduction ce ((_,defs,main):prog) =
  let ce' = flatten_map (function BranchNode n -> [n] | _ -> []) (List.tl ce) in
  let _,_,_,_,t = List.find (fun (f,_,_,_,_) -> f = main) defs in
  let pr t e =
    let s = match e with [] -> "" | [Event s] -> s | _ -> assert false in
      Format.printf "  %a ... -%s->@." print_term t s
  in
    Format.printf "Error trace::@.";
    Format.printf "  %a ... -->@." print_term (Var main);
    ignore (check_aux pr ce' true 0 (Const True) [] defs t (fun _ -> assert false));
    Format.printf "  ERROR!@.@."
