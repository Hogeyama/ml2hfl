
open Util
open Syntax




let rec term_of_pattern = function
    PVar x -> Var x
  | PConst c -> c
  | PConstruct(c,pats) -> Constr(c, List.map term_of_pattern pats)
  | PRecord(b,fields) -> assert false
  | POr(pat1,pat2) -> assert false



let rec check ce defs constr t =
(*
  Format.printf "constraint: %a@." (print_term_fm ML false) constr;
*)
  match t,ce with
    | Unit,[] -> (*???*)
        if Wrapper.checksat constr then
          raise (Feasible constr)
        else
          ()
    | Unit, [FailNode] ->
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
    | App(Event s, _), [FailNode] ->
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
    | If(t1, _, _), [EventNode "then_fail"] ->
        let constr' = BinOp(And, t1, constr) in
        if Wrapper.checksat constr' then
          raise (Feasible constr')
        else
          ()
    | If(t1, _, _), [EventNode "else_fail"] ->
        let constr' = BinOp(And, Not t1, constr) in
        if Wrapper.checksat constr' then
          raise (Feasible constr')
        else
          ()
    | If(t1, t2, _), LabNode(true)::ce' ->
        let constr' = BinOp(And, t1, constr) in
        check ce' defs constr' t2
    | If(t1, _, t3), LabNode(false)::ce' ->
        let constr' = BinOp(And, Not t1, constr) in
        check ce' defs constr' t3
    | Match_(t1,pats), [EventNode s] when (Str.string_match (Str.regexp "br\\([0-9]+\\)_fail") s 0) ->
        let n = int_of_string (Str.matched_group 1 s) in
        let pat,_,_ = List.nth pats n in
        let constr' = BinOp(And, BinOp(Eq, t1, term_of_pattern pat), constr) in
        if Wrapper.checksat constr' then
          raise (Feasible constr')
    | Match_(t1,pats), PatNode n::ce' ->
        let pat,cond,t2 = List.nth pats n in
        let constr' = BinOp(And, BinOp(Eq, t1, term_of_pattern pat), constr) in
        let constr'' =
          match cond with
              None -> constr'
            | Some cond -> BinOp(And, cond, constr')
        in
          check ce' defs constr'' t2
    | RandInt _, ce ->
        check ce defs constr (init_rand_int t)
    | _ ->
        Format.printf "feasibility.ml:@.%a@." (print_term_fm ML false) t;
        let () = List.iter (fun node -> print_msg (string_of_node node ^ " --> ")) ce in
        let () = print_msg ".\n" in
        assert false



let check ce defs t = check ce defs True t





let rec get_prefix ce ce_prefix defs constr t =
  match t,ce with
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
    | Var x, _ -> get_prefix ce ce_prefix defs constr (App(Var x, []))
    | App(Fail, _), [FailNode] ->
        if Wrapper.checksat constr then
          raise (Feasible constr)
        else
          List.rev ce_prefix
    | App(Event s, _), FailNode::_ ->
        if Wrapper.checksat constr then
          raise (Feasible constr)
        else
          List.rev ce_prefix
    | App(Event s, [t]), EventNode s'::ce' when s=s' ->
        get_prefix ce' (EventNode s'::ce_prefix) defs constr t
    | App(Var x, ts), _ ->
        let _,t' = List.assoc x defs in
        let xs = get_args x.typ in
        let t'' = List.fold_right2 subst xs ts t' in
          get_prefix ce ce_prefix defs constr t''
    | If(t1, t2, _), LabNode(true)::ce' ->
        let constr' = BinOp(And, t1, constr) in
        let ce_prefix' = LabNode(true)::ce_prefix in
          if Wrapper.checksat constr'
          then get_prefix ce' ce_prefix' defs constr' t2
          else List.rev ce_prefix'
    | If(t1, _, t3), LabNode(false)::ce' ->
        let constr' = BinOp(And, Not t1, constr) in
        let ce_prefix' = LabNode(false)::ce_prefix in
          if Wrapper.checksat constr'
          then get_prefix ce' ce_prefix' defs constr' t3
          else List.rev ce_prefix'
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
    | _ ->
        Format.printf "feasibility.ml:@.%a@." (print_term_fm ML false) t;
        let () = List.iter (fun node -> print_msg (string_of_node node ^ " --> ")) ce in
        let () = print_msg ".\n" in
        assert false

let get_prefix ce defs t = get_prefix ce [] defs True t



let rec check_int ce ce_used defs constr t =
  match t,ce with
      Var x, _ -> check_int ce ce_used defs constr (App(Var x, []))
    | App(Fail, _), [] -> raise (Feasible constr)
    | App(Var x, ts), _ ->
        let _,t' = List.assoc x defs in
        let xs = get_args x.typ in
        let t'' = List.fold_right2 subst xs ts t' in
          check_int ce ce_used defs constr t''
    | If(t1, _, _), [EventNode "then_fail"] ->
        let constr' = BinOp(And, t1, constr) in
        let ce_used' = ce_used@[LabNode(true)] in
        if Wrapper.checksat constr'
        then raise (Feasible constr)
        else Wrapper.interpolation [constr] [t1](*???*), ce_used'
    | If(t1, _, _), [EventNode "else_fail"] ->
        let constr' = BinOp(And, Not t1, constr) in
        let ce_used' = ce_used@[LabNode(false)] in
        if Wrapper.checksat constr'
        then raise (Feasible constr)
        else Wrapper.interpolation [constr] [Not t1](*???*), ce_used'
    | If(t1, t2, _), LabNode(true)::ce' ->
        let constr' = BinOp(And, t1, constr) in
        let ce_used' = ce_used@[LabNode(true)] in
          if Wrapper.checksat constr'
          then check_int ce' ce_used' defs constr' t2
          else Wrapper.interpolation [constr] [t1](*???*), ce_used'
    | If(t1, _, t3), LabNode(false)::ce' ->
        let constr' = BinOp(And, Not t1, constr) in
        let ce_used' = ce_used@[LabNode(false)] in
          if Wrapper.checksat constr'
          then check_int ce' ce_used' defs constr' t3
          else Wrapper.interpolation [constr] [Not t1](*???*), ce_used'
    | _ ->
        Format.printf "feasibility.ml:@.%a@." (print_term_fm ML false) t;
        let () = List.iter (fun node -> print_msg (string_of_node node ^ " --> ")) ce in
        let () = print_msg ".\n" in
        assert false



let check_int ce defs t = check_int ce [] defs True t
