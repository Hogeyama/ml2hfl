
open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_print
open CEGAR_util

type result =
    Feasible of bool * (string * CEGAR_syntax.typ) list * int list
  | Infeasible of CEGAR_syntax.ce

let debug () = List.mem "Feasibility" !Flag.debug_module

let checksat env t =
  Fpat.SMTProver.is_sat_dyn (FpatInterface.conv_formula t)

let get_solution env t =
  t |> FpatInterface.conv_formula |> Fpat.PolyConstrSolver.solve |> List.sort |> List.map snd |> List.map Fpat.IntTerm.int_of

let init_cont ce sat n constr env _ = assert (ce=[]); constr, n, env

let assoc_def defs n t =
  let defs' = List.filter (fun (f,_,_,_,_) -> Var f = t) defs in
    List.length defs', List.nth defs' n

let add_randint_precondition map_randint_to_preds ext_ce rand_precond r = function
  | None -> rand_precond, ext_ce
  | Some(n) ->
    let new_var = Var(r) in
    let abst_preds = try (List.assoc n map_randint_to_preds) new_var with Not_found -> Format.printf "not found: %d@." n; [] in
    let rand_var = FpatInterface.conv_var r in
    match ext_ce with
      | (m, bs)::ext_ce' when m=n ->
	let asm_cond = List.fold_left2 (fun acc p b -> make_and (if b then p else make_not p) acc) (Const True) abst_preds bs in
	make_and rand_precond asm_cond, ext_ce'
      | _ -> assert false

let map_randint_to_preds_ref = ref []
let ext_ce_ref = ref []
let rand_precond_ref = ref (Const True)
let init_refs mrtp ec = begin
  map_randint_to_preds_ref := mrtp;
  ext_ce_ref := ec;
  rand_precond_ref := Const True;
end
let add_randint_precondition r n =
  let (c, e) = add_randint_precondition !map_randint_to_preds_ref !ext_ce_ref !rand_precond_ref r n in
  ext_ce_ref := e;
  rand_precond_ref := c

(* sat=true denotes constr is satisfiable *)
let rec check_aux pr ce sat n constr env defs t k =
  if debug() then Format.printf "check_aux[%d]: %a@." (List.length ce) CEGAR_print.term t;
  match t with
  | Const (RandInt _) -> assert false
  | Const c -> k ce sat n constr env (Const c)
  | Var x -> k ce sat n constr env (Var x)
  | App(Const Not, t) ->
      check_aux pr ce sat n constr env defs t (fun ce sat n constr env t ->
      k ce sat n constr env (make_app (Const Not) [t]))
  | App(App(Const (And|Or|Lt|Gt|Leq|Geq|EqUnit|EqBool|EqInt|Add|Sub|Mul as op),t1),t2) ->
      check_aux pr ce sat n constr env defs t1 (fun ce sat n constr env t1 ->
      check_aux pr ce sat n constr env defs t2 (fun ce sat n constr env t2 ->
      k ce sat n constr env (make_app (Const op) [t1;t2])))
  | App _ when is_app_randint t ->
      let t',randnum =
        let t_rand,ts = decomp_app t in
        match t_rand with
        | Const (RandInt randnum) -> List.last ts, randnum
        | _ -> assert false
      in
      let r = new_id "r" in
      add_randint_precondition r randnum;
      let env' = (r,typ_int)::env in
      check_aux pr ce sat n constr env' defs (App(t',Var r)) k
  | App(t1,t2) ->
      check_aux pr ce sat n constr env defs t1 (fun ce sat n constr env t1 ->
      check_aux pr ce sat n constr env defs t2 (fun ce sat n constr env t2 ->
      let t1',ts = decomp_app (App(t1,t2)) in
      let _,xs,_,_,_ = List.find (fun (f,_,_,_,_) -> Var f = t1') defs in
        if List.length xs > List.length ts
        then k ce sat n constr env (App(t1,t2))
        else
          let num,(f,xs,tf1,e,tf2) = assoc_def defs (List.hd ce) t1' in
          let ts1,ts2 = List.split_nth (List.length xs) ts in
          let aux = List.fold_right2 subst xs ts1 in
	  rand_precond_ref := aux !rand_precond_ref;
          let tf1' = aux tf1 in
          let tf2' = make_app (aux tf2) ts2 in
          let constr' = make_and tf1' constr in
          let ce' = List.tl ce in
          let n' = if sat then n+1 else n in
          let sat' = sat && checksat env constr' in
          assert (List.length xs = List.length ts);
          assert (ts2 = []);
          pr t1' (List.hd ce) num e;
          if e = [Event "fail"]
          then init_cont ce' sat' n' constr' env tf2'
          else (assert (e=[]); check_aux pr ce' sat' n' constr' env defs tf2' k)))
    | Let _ -> assert false
    | Fun _ -> assert false

let rec get_prefix ce n =
  match ce with
  | [] -> assert (n=0); ce
  | c::ce' when n = 0 -> []
  | c::ce' -> c::get_prefix ce' (n-1)

let check ?(map_randint_to_preds = []) ?(ext_ce = []) ce {defs; main} =

  List.iter (fun (n, bs) -> Format.printf "C.E.: %d: %a@." n (print_list Format.pp_print_bool ",") bs) ext_ce;

  let () = if !Flag.print_progress then Format.printf "Spurious counterexample::@.  %a@.@." CEGAR_print.ce ce in
  let time_tmp = get_time () in
  let () = if !Flag.print_progress then Color.printf Color.Green "(%d-3) Checking counterexample ... @?" !Flag.cegar_loop in
  let () = if false then Format.printf "ce:        %a@." CEGAR_print.ce ce in
  let ce' = List.tl ce in
  let _,_,_,_,t = List.find (fun (f,_,_,_,_) -> f = main) defs in
  let pr _ _ _ _ = () in
  init_refs map_randint_to_preds ext_ce; (* initialize abstraction predicate path of randint *)
  let constr,n,env' = check_aux pr ce' true 0 (Const True) [] defs t init_cont in

  let prefix = get_prefix ce (n+1) in
  let result =
    if checksat env' (make_and constr !rand_precond_ref)
    then Feasible (FpatInterface.implies [FpatInterface.conv_formula !rand_precond_ref] [FpatInterface.conv_formula constr], env', get_solution env' constr)
    else Infeasible prefix
  in
  if !Flag.print_progress then Color.printf Color.Green "Feasibility constraint: %a@." CEGAR_print.term constr;
  if !Flag.print_progress then Color.printf Color.Green "Randint constraint: %a@." CEGAR_print.term !rand_precond_ref;
  if !Flag.print_progress then Color.printf Color.Green "DONE!@.@.";
  add_time time_tmp Flag.time_cegar;
  result












let init_cont ce ce_br env _ = assert (ce=[]); List.rev ce_br

let assoc_def defs n t ce_br =
  let defs' = List.filter (fun (f,_,_,_,_) -> Var f = t) defs in
  let len = List.length defs' in
  let ce_br' = if len > 1 then (n=0)::ce_br else ce_br in
  ce_br', List.nth defs' n

let rec trans_ce ce ce_br env defs t k =
  if debug() then Format.printf "trans_ce[%d]: %a@." (List.length ce) CEGAR_print.term t;
  match t with
  | Const (RandInt _) -> assert false
  | Const c -> k ce ce_br env (Const c)
  | Var x -> k ce ce_br env (Var x)
  | App(App(Const (And|Or|Lt|Gt|Leq|Geq|EqUnit|EqBool|EqInt|Add|Sub|Mul as op),t1),t2) ->
      trans_ce ce ce_br env defs t1 (fun ce ce_br env t1 ->
      trans_ce ce ce_br env defs t2 (fun ce ce_br env t2 ->
      k ce ce_br env (make_app (Const op) [t1;t2])))
(*
  | App(Const (Event s), t) -> trans_ce ce constr env defs (App(t,Const Unit)) k
*)
  | App(Const (RandInt None), t) ->
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
        let ts1,ts2 = List.split_nth (List.length xs) ts in
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

let trans_ce ce {defs=defs;main=main} =
  if debug() then Format.printf "ce:        %a@." CEGAR_print.ce ce;
  let ce' = List.tl ce in
  let _,_,_,_,t = List.find (fun (f,_,_,_,_) -> f = main) defs in
  trans_ce ce' [] [] defs t init_cont







let print_ce_reduction ?(map_randint_to_preds = []) ?(ext_ce = []) ce {defs=defs;main=main} =
  let ce' = List.tl ce in
  let _,_,_,e,t = List.find (fun (f,_,_,_,_) -> f = main) defs in
  let pr t br n e =
    let s1 = if n = 1 then "" else " [" ^ string_of_int (br+1) ^ "/" ^ string_of_int n ^ "]" in
    let s2 = match e with [] -> "" | [Event s] -> s ^ " -->" | _ -> assert false in
      Format.printf "%a%s ... --> %s@\n" CEGAR_print.term t s1 s2
  in
    init_refs map_randint_to_preds ext_ce; (* initialize abstraction predicate path of randint *)
    Format.printf "Error trace::@\n  @[";
    pr (Var main) 0 1 e;
    if e <> [Event "fail"] then
      ignore (check_aux pr ce' true 0 (Const True) [] defs t (fun _ -> assert false));
    Format.printf "ERROR!@.@."
