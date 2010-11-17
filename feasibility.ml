
open Util
open Syntax

(*
let rec get_scmap_typ scope = function
  | TUnit -> []
  | TAbsBool -> []
  | TBool -> []
  | TInt ps -> []
  | TVar _ -> assert false
  | TFun((x,TInt ps),typ2) ->
      let scope' = x::scope in
      let scmap2 = get_scmap_typ scope' typ2 in
        (x,scope')::scmap2
  | TFun((x,typ1),typ2) ->
      let scmap1 = get_scmap_typ scope typ1 in
      let scmap2 = get_scmap_typ scope typ2 in
        scmap1@@scmap2
  | TUnknown -> []


let rec get_scmap scope = function
    Unit -> []
  | True -> []
  | False -> []
  | Unknown -> []
  | Int n -> []
  | NInt x -> []
  | Var x -> []
  | App(t, ts) ->
      List.flatten (List.map (get_scmap scope) (t::ts))
  | If(t1, t2, t3, _) ->
      get_scmap scope t1 @@ get_scmap scope t2 @@ get_scmap scope t3
  | Branch(t1, t2) -> get_scmap scope t1 @@ get_scmap scope t2
  | Let(f, xs, t1, t2) ->
      let xs' = List.filter (fun x -> Wrapper.isTInt x.typ) xs in
      let scope' = xs'@@scope in
      let scmap1 = get_scmap scope' t1 in
      let scmap2 = get_scmap scope t2 in
        scmap1 @@ scmap2 @@ get_scmap_typ scope f.typ
  | Letrec(f, xs, t1, t2) ->
      let xs' = List.filter (fun x -> Wrapper.isTInt x.typ) xs in
      let scope' = xs'@@scope in
      let scmap1 = get_scmap scope' t1 in
      let scmap2 = get_scmap scope t2 in
        scmap1 @@ scmap2 @@ get_scmap_typ scope f.typ
  | BinOp(op, t1, t2) -> get_scmap scope t1 @@ get_scmap scope t2
  | Not t -> get_scmap scope t
  | Fail -> []
  | Fun _ -> assert false
  | Label(_,t) -> get_scmap scope t

let get_scmap = get_scmap []


let rec get_trace ce scmap env map rmap scopes trace t =
  (*Format.printf "%a@.@." (print_term_fm ML false) t;*)
  match t,ce with
      Var x, _ -> get_trace ce scmap env map rmap scopes trace (App(Var x, []))
    | App(Fail, _), [FailNode,0] -> trace, env, rmap, scopes
    | App(Var x, ts), _ ->
        let map_x,t',_ =
          try List.assoc x env
          with Not_found ->
            Format.printf "Feasibility.get_trace:@.%a@." (print_term_fm ML false) (Var x);
            assert false
        in
        let xs = get_args x.typ in
        let xs' = List.map (fun x -> {(new_var' x.origin) with typ = x.typ}) xs in
        let t'' = List.fold_right2 subst xs (List.map (fun x -> Var x) xs') t' in
        let filter = List.filter (fun (x,_) -> match x.typ with TInt _ -> true | _ -> false) in
        let map_xs = filter (List.combine xs xs') in
        let map' = map_xs @@ map in
        let map_x' = map_xs @@ map_x in
        let rmap' = filter (List.combine xs' xs) @@ rmap in
        let scopes' =
          let aux scopes x x' =
            match x.typ with
                TInt _ ->
                  let scope = List.assoc x scmap in
                  let scope' = List.map (fun x -> List.assoc x map_x') scope in
                    (x', scope')::scopes
              | _ -> scopes
          in
            List.fold_left2 aux scopes xs xs'
        in
        let trace' =
          let aux trace x t =
            match x.typ,trace with
                (TBool | TInt _), (constr,t',t'')::trace' -> (BinOp(Eq, Var x, t)::constr, t',t'')::trace'
              | _ -> trace
          in
            List.fold_left2 aux trace xs' ts
        in
        let env' =
          let aux env x t =
            match x.typ with
                TFun _ -> (x, (map_x', expand t x.typ, (**)x(**)))::env
              | _ -> env
          in
            List.fold_left2 aux env xs' ts
        in
        let t''' = List.fold_right2 subst xs' ts t'' in
          get_trace ce scmap env' map' rmap' scopes' (([],t'',t''')::trace') t''
    | Let(f, xs, t1, t2), _ ->
        let f' = {(new_var' f.origin) with typ = f.typ} in
        let t2' = subst f (Var f') t2 in
        let env' = (f',(map,t1,f))::env in
        let trace' =
          match trace with
              (constr,_,t)::trace' -> (constr,t2',t)::trace'
            | _ -> assert false
        in
          get_trace ce scmap env' map rmap scopes trace' t2'
    | Letrec(f, xs, t1, t2), _ ->
        let f' = {(new_var' f.origin) with typ = f.typ} in
        let t1' = subst f (Var f') t1 in
        let t2' = subst f (Var f') t2 in
        let env' = (f',(map,t1',f))::env in
        let trace' =
          match trace with
              (constr,_,t)::trace' -> (constr,t2',t)::trace'
            | _ -> assert false
        in
          get_trace ce scmap env' map rmap scopes trace' t2'
    | If(t1, t2, _, _), (LabNode true, _)::ce' ->
        let trace' =
          match trace with
              (constr,_,t)::trace' -> (t1::constr,t2,t)::trace'
            | _ -> assert false
        in
          get_trace ce' scmap env map rmap scopes trace' t2
    | If(t1, _, t3, _), (LabNode false, _)::ce' ->
        let trace' =
          match trace with
              (constr,_,t)::trace' -> (Not t1::constr,t3,t)::trace'
            | _ -> assert false
        in
          get_trace ce' scmap env map rmap scopes trace' t3
(*
    | App(t, []), ce -> get_trace ce scmap env map rmap scopes trace t
*)
    | t, _ -> begin
        Format.printf "Feasibility.get_trace:@.%a@." (print_term_fm ML false) t;
        assert false
    end

let get_trace ce scmap t = get_trace ce scmap [] [] [] [] [[],t,t] t


(* DO NOT USE *)
let rec conj = function
    [] -> False
  | [t] -> t
  | t::ts -> BinOp(And, t, conj ts)


(*let check ce t =
  Format.printf "Feasibility.check:@.%a@." (print_term_fm ML false) t;
  let scmap = get_scmap t in
  let trace,env,rmap,scopes = get_trace ce scmap t in
  let constr = List.fold_left (fun acc (constr,_,_) -> constr@@acc) [] trace in
    match constr with
        [] -> raise (Feasible True)
      | [t] ->
          if Wrapper.checksat t
          then raise (Feasible t)
          else raise Infeasible
      | _ ->
          let p = conj constr in
            if Wrapper.checksat p
            then raise (Feasible p)
*)

let check ce t =
  (*
    Format.printf "Feasibility.check:@.%a@." (print_term_fm ML false) t;
  *)
  let rec aux env ce cond t =
    (*
      Format.printf "%a@." (print_term_fm ML false) t;
      List.iter (function (FailNode, _) -> Format.printf "fail:" | (LabNode true, _) -> Format.printf "true:" | (LabNode false, _) -> Format.printf "false:") ce;
      Format.printf "@.@.";
    *)
    match t with
        Unit -> ce, cond, Unit, env
      | True -> ce, cond, True, env
      | False -> ce, cond, False, env
      | Unknown -> ce, cond, Unknown, env
      | Int n -> ce, cond, Int n, env
      | NInt x -> ce, cond, NInt x, env
      | Var x -> ce, cond, Var x, env
      | App(App(t, ts1), ts2) ->
          aux env ce cond (App(t, ts1 @ ts2))
            (*		  | App(Fail, _) ->
                          if ce = [FailNode,0] then [], cond, Fail, env else assert false*)
      | App(t, ts) ->
          let ce', cond', t', env' = aux env ce cond t in
          let ce', cond', ts', env' =
            List.fold_left (fun (ce, cond, ts, env) t ->
                              let ce', cond', t', env' = aux env ce cond t in ce', cond', t'::ts, env') (ce', cond', [], env') ts in
          let ts' = List.rev ts' in
            if t' = Fail then
              if ce' = [] then [], cond, Fail, env else assert false
            else
              let ids, t =
                match t' with
                  Var f -> (try List.assoc f env' with Not_found -> Format.printf "Feasibility.check:@.%a@." (print_term_fm ML false) t'; assert false)
                | Fun(id, t) -> [id], t
                | _ -> Format.printf "Feasibility.check:@.%a@." (print_term_fm ML false) t'; assert false
              in
                if List.length ids = List.length ts then
		  let rec cut_at n ls =
		    if n <= 0 then
		      [], ls
		    else
		      match ls with
		          [] -> assert false
		        | l::ls' ->
		            let ls1, ls2 = cut_at (n - 1) ls' in
		              l::ls1, ls2
		  in
		  let ts1, ts2 = cut_at (List.length ids) ts' in
		  let t' = subst_term (List.combine ids ts1) t in
		  let t' = if ts2 = [] then t' else App(t', ts2) in
		    aux env' ce' cond' t'
                else
                  ce', cond', App(t', ts'), env'
      | If(t1, t2, t3, _) ->
	  let ce', cond', t1', env' = aux env ce cond t1 in
            (match ce' with
                 (LabNode true, _)::ce' -> aux env' ce' (t1'::cond') t2
               | (LabNode false, _)::ce' -> aux env' ce' ((Not t1')::cond') t3
               | _ -> assert false)
      | Let(f, xs, t1, t2) ->
          (     (*Format.printf "adding %a@."  (print_term_fm ML false) (Var f);*)
            if xs = [] then
  	      let ce', cond', t1', env' = aux env ce cond t1 in
                aux ((f, (xs, t1'))::env') ce' cond' t2
            else
              aux ((f, (xs, t1))::env) ce cond t2)
      | Letrec(f, xs, t1, t2) ->
          (      (*Format.printf "adding %a@."  (print_term_fm ML false) (Var f);*)
            assert (xs <> []);
            aux ((f, (xs, t1))::env) ce cond t2)
      | BinOp(op, t1, t2) ->
	  let ce', cond', t1', env' = aux env ce cond t1 in
	  let ce', cond', t2', env' = aux env' ce' cond' t2 in
            ce', cond', BinOp(op, t1', t2'), env'
      | Not t ->
	  let ce', cond', t', env' = aux env ce cond t in
            ce', cond', Not t', env'
      | Fail ->
          if ce = [FailNode,0] then [], cond, Fail, env else assert false
      | Fun(id, t) ->
          ce, cond, Fun(id, t), env
      | _ -> assert false
  in
    match aux [] ce [] t with
        [], cond, Fail, _ ->
	  let t = conj cond in
            (*
              Format.printf "%s@." (string_of_term CVC3 t);
            *)
	    if Wrapper.checksat t
	    then raise (Feasible t)
(*
	    else ((*Format.printf "OK@.";*) ())(*raise Infeasible*)
*)
            else 0
      | ce, cond, t, _ ->
          Format.printf "%a@." (print_term_fm ML false) t;
          assert false





















let rec check ce env constr t counter =
  match t,ce with
      Var x, _ -> check ce env constr (App(Var x, [])) (counter+1)
    | App(Fail, _), [FailNode,0] -> raise (Feasible constr)
    | App(Var x, ts), _ ->
        let _,t' = List.assoc x env in
        let xs = get_args x.typ in
        let xs' = List.map (fun x -> {(new_var' x.origin) with typ = x.typ}) xs in
        let t'' = List.fold_right2 subst xs (List.map (fun x -> Var x) xs') t' in
        let constr' =
          let aux constr x t =
            match x.typ with
                TBool | TInt _ -> BinOp(And, BinOp(Eq, Var x, t), constr)
              | _ -> constr
          in
            List.fold_left2 aux constr xs' ts
        in
        let env' =
          let aux env x t =
            match x.typ with
                TFun _ -> (x, ([], expand t x.typ))::env
              | _ -> env
          in
            List.fold_left2 aux env xs' ts
        in
          check ce env' constr' t'' (counter+1)
(*
    | Let(f, xs, t1, t2), _ ->
        let f' = {(new_var' f.origin) with typ = f.typ} in
        let t2' = subst f (Var f') t2 in
        let env' = (f',(map,t1))::env in
          check ce scmap env' map rmap scopes constr unsat t2'
    | Letrec(f, xs, t1, t2), _ ->
        let f' = {(new_var' f.origin) with typ = f.typ} in
        let t1' = subst f (Var f') t1 in
        let t2' = subst f (Var f') t2 in
        let env' = (f',(map,t1'))::env in
          check ce scmap env' map rmap scopes constr unsat t2'
*)
    | If(t1, t2, _, _), (LabNode true, _)::ce' ->
        let constr' = BinOp(And, t1, constr) in
          if Wrapper.checksat constr'
          then check ce' env constr' t2 counter
          else counter
    | If(t1, _, t3, _), (LabNode false, _)::ce' ->
        let constr' = BinOp(And, Not t1, constr) in
          if Wrapper.checksat constr'
          then check ce' env constr' t3 counter
          else counter
    | _ -> assert false

let rec check ce defs constr t counter =
  match t,ce with
      Var x, _ -> check ce defs constr (App(Var x, [])) counter
    | App(Fail, _), [FailNode,0] -> raise (Feasible constr)
    | App(Var x, ts), _ ->
        let _,t' = List.assoc x defs in
        let xs = get_args x.typ in
        let t'' = List.fold_right2 subst xs ts t' in
          check ce defs constr t'' (counter+1)
    | If(t1, t2, _, _), (LabNode true, _)::ce' ->
        let constr' = BinOp(And, t1, constr) in
          if Wrapper.checksat constr'
          then check ce' defs constr' t2 counter
          else Wrapper.interpolation [constr] [t1], counter
    | If(t1, _, t3, _), (LabNode false, _)::ce' ->
        let constr' = BinOp(And, Not t1, constr) in
          if Wrapper.checksat constr'
          then check ce' defs constr' t3 counter
          else Wrapper.interpolation [constr] [Not t1], counter
    | _ -> assert false



let check ce defs t = check ce defs True t 0


















let rec check ce ce_used defs constr t =
  match t,ce with
      Var x, _ -> check ce ce_used defs constr (App(Var x, []))
    | App(Fail, _), [] -> raise (Feasible constr)
    | App(Var x, ts), _ ->
        let _,t' = List.assoc x defs in
        let xs = get_args x.typ in
        let t'' = List.fold_right2 subst xs ts t' in
          check ce ce_used defs constr t''
    | If(t1, t2, _, _), true::ce' ->
        let constr' = BinOp(And, t1, constr) in
        let ce_used' = ce_used@[true] in
          if Wrapper.checksat constr'
          then check ce' ce_used' defs constr' t2
          else Wrapper.interpolation [constr] [t1], ce_used'
    | If(t1, _, t3, _), false::ce' ->
        let constr' = BinOp(And, Not t1, constr) in
        let ce_used' = ce_used@[false] in
          if Wrapper.checksat constr'
          then check ce' ce_used' defs constr' t3
          else Wrapper.interpolation [constr] [Not t1], ce_used'
    | _ -> assert false



let check ce defs t = check ce [] defs True t
*)


let rec check ce defs constr t =
  match t,ce with
    | Unit,[] -> (*???*)
        if Wrapper.checksat constr then
          raise (Feasible constr)
        else
          ()
    | Unit,[EventNode "unit"] ->
        if Wrapper.checksat constr then
          raise (Feasible constr)
        else
          ()
    | Var x, _ -> check ce defs constr (App(Var x, []))
    | App(Fail, _), [FailNode] ->
        if Wrapper.checksat constr then
          raise (Feasible constr)
        else
          ()
    | App(Event s, _), FailNode::_ ->
        if Wrapper.checksat constr then
          raise (Feasible constr)
        else
          ()
    | App(Event s, [t]), EventNode s'::ce' when s=s' ->
        check ce' defs constr t
    | App(Var x, ts), _ ->
        let _,t' = List.assoc x defs in
        let xs = get_args x.typ in
        let t'' = List.fold_right2 subst xs ts t' in
          check ce defs constr t''
    | If(t1, t2, _, _), LabNode(true)::ce' ->
        let constr' = BinOp(And, t1, constr) in
        check ce' defs constr' t2
    | If(t1, _, t3, _), LabNode(false)::ce' ->
        let constr' = BinOp(And, Not t1, constr) in
        check ce' defs constr' t3
    | _ ->
        Format.printf "feasibility.ml:@.%a@." (print_term_fm ML false) t;
        let () = List.iter (fun node -> print_msg (string_of_node node ^ " --> ")) ce in
        let () = print_msg ".\n" in
        assert false



let check ce defs t = check ce defs True t
