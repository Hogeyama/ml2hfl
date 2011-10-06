
open Utilities
open CEGAR_syntax
open CEGAR_type
open CEGAR_print
open CEGAR_util

type result =
    Feasible of (string * CEGAR_syntax.typ) list * string list
  | Infeasible of CEGAR_syntax.ce

let init_cont ce sat n constr env _ = assert (ce=[]); constr, n, env

(* sat=true denotes constr is satisfiable *)
let rec check ce sat n constr env defs t k =
  if false then Format.printf "check: %a@." print_term t;
  match t with
    Const (Event _) -> assert false
  | Const RandInt -> assert false
  | Const c -> k ce sat n constr env (Const c)
  | Var x -> k ce sat n constr env (Var x)
  | App(App(Const (And|Or|Lt|Gt|Leq|Geq|EqUnit|EqBool|EqInt|Add|Sub|Mul as op),t1),t2) ->
      check ce sat n constr env defs t1 (fun ce sat n constr env t1 ->
      check ce sat n constr env defs t2 (fun ce sat n constr env t2 ->
        k ce sat n constr env (make_app (Const op) [t1;t2])))
  | App(Const (Event "fail"), t) -> init_cont ce sat n constr env t
  | App(Const (Event s), t) -> check ce sat n constr env defs (App(t,Const Unit)) k
  | App(Const RandInt, t) ->
      let r = new_id "r" in
      let env' = (r,typ_int)::env in
        check ce sat n constr env' defs (App(t,Var r)) k
  | App(t1,t2) ->
      check ce sat n constr env defs t1 (fun ce sat n constr env t1 ->
      check ce sat n constr env defs t2 (fun ce sat n constr env t2 ->
        let t1',ts = decomp_app (App(t1,t2)) in
        let _,xs,_,_ = List.find (fun (f,_,_,_) -> Var f = t1') defs in
          if List.length xs > List.length ts
          then k ce sat n constr env (App(t1,t2))
          else
            let f,xs,tf1,tf2 = List.nth defs (List.hd ce) in
            let ts1,ts2 = take2 ts (List.length xs) in
            let aux = List.fold_right2 subst xs ts1 in
            let tf1' = aux tf1 in
            let tf2' = make_app (aux tf2) ts2 in
            let constr' = make_and tf1' constr in
            let ce' = List.tl ce in
            let n' = if sat then n+1 else n in
            let sat' = sat && Wrapper2.checksat env constr' in
              assert (Var f = t1');
              assert (List.length xs = List.length ts);
              assert (ts2 = []);
              check ce' sat' n' constr' env defs tf2' k))

let rec get_prefix ce n =
  match ce with
      [] -> assert false
    | [EventNode "fail" | EventNode "unit"] -> assert (n=0); ce
    | (LineNode _)::ce' when n = 0 -> []
    | (LineNode _ as c)::ce' -> c::get_prefix ce' (n-1)
    | c::ce' -> c::get_prefix ce' n

let check ce (env,defs,main) =
  if false then Format.printf "ce:        %a@." CEGAR_print.print_ce ce;
  let ce' = flatten_map (function LineNode n -> [n] | _ -> []) (List.tl ce) in
  let _,_,_,t = List.find (fun (f,_,_,_) -> f = main) defs in
  let constr,n,env' = check ce' true 0 (Const True) [] defs t init_cont in
  let prefix = get_prefix ce (n+1) in
    if false then Format.printf "prefix(%d): %a@." n CEGAR_print.print_ce prefix;
    if Wrapper2.checksat env' constr
    then Feasible (env', Wrapper2.get_solution env' constr)
    else Infeasible prefix

