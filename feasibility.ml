
open Utilities
open CEGAR_syntax
open CEGAR_type
open CEGAR_print
open CEGAR_util


let rec check ce constr env defs t k =
  if false then Format.printf "check: %a@." print_term t;
  match t with
    App(Const (Event "fail"), t) ->
      check ce constr env defs t (fun env ce' _ -> assert (ce'=[]); constr, env)
  | Const RandInt ->
      let x = new_id "r" in
        k ((x,TBase(TInt,fun _ -> []))::env) ce (Var x)
  | Const c -> k env ce (Const c)
  | Var x -> k env ce (Var x)
  | App(App(Const (And|Or|Lt|Gt|Leq|Geq|EqUnit|EqBool|EqInt|Add|Sub|Mul as op),t1),t2) ->
      check ce constr env defs t1 (fun env ce1 t1' ->
      check ce1 constr env defs t2 (fun env ce2 t2' ->
        k env ce2 (make_app (Const op) [t1';t2'])))
  | App(Const (Event s), t) -> check ce constr env defs t k
  | App(Const RandInt, t) ->
      let r = new_id "r" in
      let env' = (r,typ_int)::env in
        check ce constr env' defs (App(t,Var r)) k
  | App(t1,t2) ->
      check ce constr env defs t1 (fun env ce1 t1' ->
      check ce1 constr env defs t2 (fun env ce2 t2' ->
        let t1'',ts = decomp_app (App(t1',t2')) in
        let _,xs,_,_ = List.find (fun (f,_,_,_) -> Var f = t1'') defs in
          if List.length xs > List.length ts
          then k env ce2 (App(t1',t2'))
          else
            let n = List.hd ce2 in
            let ce2' = List.tl ce2 in
            let f,xs,tf1,tf2 = List.nth defs n in
            assert (Var f = t1'');
            let ts1,ts2 = take2 ts (List.length xs) in
            assert (List.length xs = List.length ts);
            assert (ts2 = []);
            let aux = List.fold_right2 subst xs ts1 in
            let tf1' = aux tf1 in
            let tf2' = make_app (aux tf2) ts2 in
              check ce2' (make_and tf1' constr) env defs tf2' k))

let check ce (env,defs,main) =
(*
  let rec aux = function
      TFun typ ->
        let x = new_id "x" in
        let typ1,typ2 = typ (Var x) in
          if is_base_typ typ1
          then (x,typ1) :: aux typ2
          else aux typ2
    | typ -> []
  in
  let env = aux (List.assoc main env) in
  let xs = List.map (fun (x,_) -> Var x) env in
  let () = assert (xs = []) in
  let t = make_app (Var main) xs in
*)
  let ce' = flatten_map (fun n -> if n>=2 then [n-2] else []) (List.tl ce) in
  let _,_,_,t = List.find (fun (f,_,_,_) -> f = main) defs in
  let constr,env' = check ce' (Const True) [] defs t (fun _ -> assert false) in
    if Wrapper2.checksat env' constr
    then Some (env', Wrapper2.get_solution env' constr)
    else None




(*
let rec get_prefix ce ce_prefix defs constr t =
  match t.desc,ce with
    | Unit,[] -> (*???*)
        if Wrapper.checksat constr then
          raise (Feasible constr)
        else
          ce_prefix
    | Unit,[EventNode "unit"] ->
        if Wrapper.checksat constr then
          raise (Feasible constr)
        else
          List.rev ce_prefix
    | Unit,[FailNode] ->
        if Wrapper.checksat constr then
          raise (Feasible constr)
        else
          List.rev ce_prefix
    | Var x, _ -> get_prefix ce ce_prefix defs constr {desc=App({desc=Var x;typ=Id.typ x}, []);typ=t.typ}
    | App({desc=Fail}, _), [FailNode] ->
        if Wrapper.checksat constr then
          raise (Feasible constr)
        else
          List.rev ce_prefix
    | App({desc=Event s}, _), FailNode::_ ->
        if Wrapper.checksat constr then
          raise (Feasible constr)
        else
          List.rev ce_prefix
    | App({desc=Event s}, [t]), EventNode s'::ce' when s=s' ->
        get_prefix ce' (EventNode s'::ce_prefix) defs constr t
    | App({desc=Var x}, ts), _ ->
        let _,t' = List.assoc x defs in
        let xs = get_args (Id.typ x) in
        let t'' = List.fold_right2 subst xs ts t' in
          get_prefix ce ce_prefix defs constr t''
    | If(t1, t2, _), LabNode(true)::ce' ->
        let constr' = {desc=BinOp(And, t1, constr);typ=TBool} in
        let ce_prefix' = LabNode(true)::ce_prefix in
          if Wrapper.checksat constr'
          then get_prefix ce' ce_prefix' defs constr' t2
          else List.rev ce_prefix'
    | If(t1, _, t3), LabNode(false)::ce' ->
        let constr' = {desc=BinOp(And, {desc=Not t1;typ=TBool}, constr);typ=TBool} in
        let ce_prefix' = LabNode(false)::ce_prefix in
          if Wrapper.checksat constr'
          then get_prefix ce' ce_prefix' defs constr' t3
          else List.rev ce_prefix'
(*
    | Match(t1, t2, _, _, _), LabNode(true)::ce' ->
        let constr' = BinOp(And, BinOp(Eq,t1,Nil), constr) in
        let ce_prefix' = LabNode(true)::ce_prefix in
          if Wrapper.checksat constr'
          then get_prefix ce' ce_prefix' defs constr' t2
          else List.rev ce_prefix'
    | Match(t1, _, x, y, t3), LabNode(false)::ce' ->
        let constr' = BinOp(And, BinOp(Eq,t1,Cons(Var x,Var y)), constr) in
        let ce_prefix' = LabNode(false)::ce_prefix in
          if Wrapper.checksat constr'
          then get_prefix ce' ce_prefix' defs constr' t3
          else List.rev ce_prefix'
    | Match_(t1,pats), PatNode(n)::ce' ->
        let pat,cond,t2 = List.nth pats n in
        let constr' = BinOp(And, BinOp(Eq, t1, term_of_pattern pat), constr) in
        let constr'' =
          match cond with
              None -> constr'
            | Some cond -> BinOp(And, cond, constr')
        in
        let ce_prefix' = PatNode(n)::ce_prefix in
          if Wrapper.checksat constr''
          then get_prefix ce' ce_prefix' defs constr'' t2
          else List.rev ce_prefix'
*)
    | _ ->
        Format.printf "feasibility.ml:@.%a@." (print_term_fm ML false) t;
        let () = List.iter (fun node -> print_msg (string_of_node node ^ " --> ")) ce in
        let () = print_msg ".\n" in
        assert false

let get_prefix ce defs t = get_prefix ce [] defs true_term t
*)

(*
let rec check_int ce ce_used defs constr t =
  match t.desc,ce with
      Var x, _ -> check_int ce ce_used defs constr {desc=App({desc=Var x;typ=Id.typ x}, []);typ=t.typ}
    | App({desc=Fail}, _), [] -> raise (Feasible constr)
    | App({desc=Var x}, ts), _ ->
        let _,t' = List.assoc x defs in
        let xs = get_args (Id.typ x) in
        let t'' = List.fold_right2 subst xs ts t' in
          check_int ce ce_used defs constr t''
    | If(t1, _, _), [EventNode "then_fail"] ->
        let constr' = {desc=BinOp(And, t1, constr);typ=TBool} in
        let ce_used' = ce_used@[LabNode(true)] in
        if Wrapper.checksat constr'
        then raise (Feasible constr)
        else Wrapper.interpolation [constr] [t1](*???*), ce_used'
    | If(t1, _, _), [EventNode "else_fail"] ->
        let constr' = {desc=BinOp(And, {desc=Not t1;typ=TBool}, constr);typ=TBool} in
        let ce_used' = ce_used@[LabNode(false)] in
        if Wrapper.checksat constr'
        then raise (Feasible constr)
        else Wrapper.interpolation [constr] [{desc=Not t1;typ=TBool}](*???*), ce_used'
    | If(t1, t2, _), LabNode(true)::ce' ->
        let constr' = {desc=BinOp(And, t1, constr);typ=TBool} in
        let ce_used' = ce_used@[LabNode(true)] in
          if Wrapper.checksat constr'
          then check_int ce' ce_used' defs constr' t2
          else Wrapper.interpolation [constr] [t1](*???*), ce_used'
    | If(t1, _, t3), LabNode(false)::ce' ->
        let constr' = {desc=BinOp(And, {desc=Not t1;typ=TBool}, constr);typ=TBool} in
        let ce_used' = ce_used@[LabNode(false)] in
          if Wrapper.checksat constr'
          then check_int ce' ce_used' defs constr' t3
          else Wrapper.interpolation [constr] [{desc=Not t1;typ=TBool}](*???*), ce_used'
    | _ ->
        Format.printf "feasibility.ml:@.%a@." (print_term_fm ML false) t;
        let () = List.iter (fun node -> print_msg (string_of_node node ^ " --> ")) ce in
        let () = print_msg ".\n" in
        assert false



let check_int ce defs t = check_int ce [] defs true_term t
*)
