open Util
open Syntax

exception RaiseExcep of int list * typed_term
exception EventFail

let fix f v = {desc=Label(InfoId f, v); typ = v.typ}
let rec fun_info args_rev f t =
  match t.desc with
      Fun(x,t1) -> make_fun x (fun_info (x::args_rev) f t1)
    | _ ->
        if args_rev = []
        then t
        else
          let t' = List.fold_left (fun t x -> make_label (InfoId x) t) t args_rev in
            make_label (InfoString (Id.name f)) t'
let fun_info f v = fun_info [] f v

let rec subst_arg x t t' =
  match t'.desc with
      Const c -> t'
    | Unknown -> t'
    | Bottom -> t'
    | RandInt _ -> t'
    | Var y -> make_var y
    | Fun(y, t1) when Id.same x y -> t'
    | Fun(y, t1) ->
        let t1' = subst_arg x t t1 in
          make_fun y t1'
    | App(t1, ts) ->
        let t1' = subst_arg x t t1 in
        let ts' = List.map (subst_arg x t) ts in
          make_app t1' ts'
    | If(t1, t2, t3) ->
        let t1' = subst_arg x t t1 in
        let t2' = subst_arg x t t2 in
        let t3' = subst_arg x t t3 in
          make_if t1' t2' t3'
    | Branch(t1, t2) ->
        let t1' = subst_arg x t t1 in
        let t2' = subst_arg x t t2 in
          make_branch t1' t2'
    | Let(flag, bindings, t2) ->
        let aux (f,xs,t1) = f, xs, subst_arg x t t1 in
        let bindings' = List.map aux bindings in
        let t2' = subst_arg x t t2 in
          make_let_f flag bindings' t2'
    | BinOp(op, t1, t2) ->
        let t1' = subst_arg x t t1 in
        let t2' = subst_arg x t t2 in
          {desc=BinOp(op, t1', t2'); typ=t'.typ}
    | Not t1 ->
        let t1' = subst_arg x t t1 in
          make_not t1'
    | Event(s,_) -> t'
    | Record fields -> {desc=Record (List.map (fun (f,(s,t1)) -> f,(s,subst_arg x t t1)) fields); typ=t'.typ}
    | Proj(i,s,f,t1) -> {desc=Proj(i,s,f,subst_arg x t t1); typ=t'.typ}
    | SetField(n,i,s,f,t1,t2) -> {desc=SetField(n,i,s,f,subst_arg x t t1,subst_arg x t t2); typ=t'.typ}
    | Nil -> t'
    | Cons(t1,t2) -> {desc=Cons(subst_arg x t t1, subst_arg x t t2); typ=t'.typ}
    | Constr(s,ts) -> {desc=Constr(s, List.map (subst_arg x t) ts); typ=t'.typ}
    | Match(t1,pats) ->
        let aux (pat,cond,t1) = pat, subst_arg x t cond, subst_arg x t t1 in
          {desc=Match(subst_arg x t t1, List.map aux pats); typ=t'.typ}
    | Raise t1 -> {desc=Raise(subst_arg x t t1); typ=t'.typ}
    | TryWith(t1,t2) -> {desc=TryWith(subst_arg x t t1, subst_arg x t t2); typ=t'.typ}
    | Pair(t1,t2) -> make_pair (subst_arg x t t1) (subst_arg x t t2)
    | Fst t1 -> make_fst (subst_arg x t t1)
    | Snd t1 -> make_snd (subst_arg x t t1)
    | RandValue _ -> assert false
    | Label(InfoId y, t1) when Id.same x y -> make_label (InfoTerm t) t1
    | Label(info, t1) -> make_label info (subst_arg x t t1)


let rec take_args = function
    {desc=Label(InfoTerm t1, t2)} ->
      let args,t2' = take_args t2 in
        t1 :: args, t2'
  | t -> [], t


let subst' x t t1 = subst x t (subst_arg x t t1)


let rec print_value fm t =
  match t.desc with
      Const (Int n) when n < 0 -> Format.fprintf fm "(%d)" n
    | Fun _ -> Format.pp_print_string fm "<fun>"
    | Cons _ ->
        let rec aux t =
          match t.desc with
              Nil -> []
            | Cons(t1,t2) -> t1 :: aux t2
            | _ -> assert false
        in
          Format.fprintf fm "[%a]" (print_list print_value ";") (aux t)
    | Pair(t_1,t_2) -> Format.fprintf fm "(@[@[%a@],@ @[%a@]@])" print_value t_1 print_value t_2
    | _ -> pp_print_term fm t


let rec eval_print fm rands t =
  if false then Format.printf "EVAL:%a@.RANDS:%a@.@." pp_print_term t
    (print_list Format.pp_print_int ";") rands;
  match t.desc with
      Const c -> rands, t
    | Unknown -> assert false
    | RandInt false ->
        let x = Id.new_var "" Type.TUnit in
          List.tl rands, make_fun x (make_int (List.hd rands))
    | RandInt true -> assert false
    | RandValue _ -> assert false
    | Var y -> assert false
    | Fun _ -> rands, t
    | App(t1, ts) ->
        let aux t (rands,vs) =
          let rands',v = eval_print fm rands t in
            rands', v::vs
        in
        let rands',vs = List.fold_right aux (t1::ts) (rands,[]) in
          begin
            match vs with
                {desc=Fun(x,t)}::v1::vs' ->
                  eval_print fm rands' (make_app (subst' x v1 t) vs')
              | _ -> assert false
          end
    | If(t1, t2, t3) ->
        let rands', v = eval_print fm rands t1 in
        let b =
          match v.desc with
              Const True -> true
            | Const False -> false
            | _ -> assert false
        in
          Format.fprintf fm "@\nif %b then ... ->" b;
          if b
          then eval_print fm rands' t2
          else eval_print fm rands' t3
    | Branch(t1, t2) -> assert false
    | Let(flag, bindings, t2) ->
        let aux (rands,vs) (f,xs,t) =
          let rands',v = eval_print fm rands (List.fold_right make_fun xs t) in
            rands', vs@[f,v]
        in
        let rands',vs = List.fold_left aux (rands,[]) bindings in
        let subst' (x,v) t =
          match flag with
              Nonrecursive -> subst x (fun_info x v) t
            | Recursive -> subst x (fix x (fun_info x v)) t
        in
          eval_print fm rands' (List.fold_right subst' vs t2)
    | BinOp(And, t1, t2) ->
        let rands',v1 = eval_print fm rands t1 in
          begin
            match v1.desc with
                Const False -> rands', false_term
              | Const True -> eval_print fm rands' t2
              | _ -> assert false
          end
    | BinOp(Or, t1, t2) ->
        let rands',v1 = eval_print fm rands t1 in
          begin
            match v1.desc with
                Const True -> rands', true_term
              | Const False -> eval_print fm rands' t2
              | _ -> assert false
          end
    | BinOp(op, t1, t2) ->
        let rands',v2 = eval_print fm rands t2 in
        let rands'',v1 = eval_print fm rands' t1 in
        let v =
          match op, v1.desc, v2.desc with
              Eq, v1, v2 -> if v1 = v2 then true_term else false_term
            | Lt, Const (Int n1), Const (Int n2) -> if n1 < n2 then true_term else false_term
            | Gt, Const (Int n1), Const (Int n2) -> if n1 > n2 then true_term else false_term
            | Leq, Const (Int n1), Const (Int n2) -> if n1 <= n2 then true_term else false_term
            | Geq, Const (Int n1), Const (Int n2) -> if n1 >= n2 then true_term else false_term
            | Add, Const (Int n1), Const (Int n2) -> make_int (n1 + n2)
            | Sub, Const (Int n1), Const (Int n2) -> make_int (n1 - n2)
            | Mult, Const (Int n1), Const (Int n2) -> make_int (n1 * n2)
            | _ -> assert false
        in
          rands'', v
    | Not t1 ->
        let rands',v1 = eval_print fm rands t1 in
        let v =
          match v1.desc with
              Const True -> false_term
            | Const False -> true_term
            | _ -> assert false
        in
          rands', v
    | Event("fail",false) -> raise EventFail
    | Event _ -> assert false
    | Record fields -> raise (Fatal "Not implemented: Eval_Print Fm Record")
        (*        Record (List.map (fun (f,(s,t1)) -> f,(s,id__ t1)) fields)*)
    | Proj(i,s,f,t1) -> raise (Fatal "Not implemented: Eval_Print Fm Record")
    | SetField(n,i,s,f,t1,t2) -> raise (Fatal "Not implemented: Eval_Print Fm Record")
    | Nil -> rands, t
    | Cons(t1,t2) ->
        let rands',v2 = eval_print fm rands t2 in
        let rands'',v1 = eval_print fm rands' t1 in
          rands'', make_cons v1 v2
    | Constr(s,ts) ->
        let aux t (rands,vs) =
          let rands',v = eval_print fm rands t in
            rands', v::vs
        in
        let rands',vs = List.fold_right aux ts (rands,[]) in
          rands', {desc=Constr(s,vs); typ=t.typ}
    | Match(t1,pat::pats) ->
        let rec check v p =
          match v.desc, p.pat_desc with
              _, PAny -> Some (fun t -> t)
            | _, PVar x -> Some (subst x v)
            | _, PConst v' -> if v = v' then Some (fun t -> t) else None
            | Constr(s, vs), PConstruct(s', ps) ->
                if s <> s'
                then None
                else
                  let aux bind v p =
                    match bind with
                        None -> None
                      | Some f ->
                          match check v p with
                              None -> None
                            | Some f' -> Some (fun t -> f (f' t))
                  in
                    List.fold_left2 aux (Some (fun t -> t)) vs ps
            | Nil, PNil -> Some (fun t -> t)
            | Cons(v1,v2), PCons(p1,p2) ->
                begin
                  match check v1 p1, check v2 p2 with
                      None, _
                    | _, None -> None
                    | Some f1, Some f2 -> Some (fun t -> f1 (f2 t))
                end
            | Pair(v1,v2), PPair(p1,p2) ->
                begin
                  match check v1 p1, check v2 p2 with
                      None, _
                    | _, None -> None
                    | Some f1, Some f2 -> Some (fun t -> f1 (f2 t))
                end
            | Record _, PRecord _ -> assert false
            | _, POr(p1, p2) ->
                begin
                  match check v p1 with
                      None -> check v p2
                    | Some f -> Some f
                end
            | _ -> None
        in
        let rands',v = eval_print fm rands t1 in
        let p,cond,t = pat in
          begin
            match check v p with
                None -> eval_print fm rands' (make_match v pats)
              | Some f ->
                  let rands'',v = eval_print fm rands' (f cond) in
                    match v.desc with
                        Const True -> eval_print fm rands'' (f t)
                      | Const False -> eval_print fm rands'' (make_match v pats)
                      | _ -> assert false
          end
    | Match(t1,[]) -> assert false
    | Raise t ->
        let rands',v = eval_print fm rands t in
          raise (RaiseExcep(rands',v))
    | TryWith(t1,t2) ->
        begin
          try
            eval_print fm rands t1
          with RaiseExcep(rands',e) -> eval_print fm rands' (make_app t2 [e])
        end
    | Pair(t1,t2) ->
        let rands',v2 = eval_print fm rands t2 in
        let rands'',v1 = eval_print fm rands' t1 in
          rands'', make_pair v1 v2
    | Fst t ->
        let rands',v = eval_print fm rands t in
          begin
            match v.desc with
                Pair(v1,v2) -> rands', v1
              | _ -> assert false
          end
    | Snd t ->
        let rands',v = eval_print fm rands t in
          begin
            match v.desc with
                Pair(v1,v2) -> rands', v2
              | _ -> assert false
          end
    | Bottom -> assert false
    | Label(InfoId f, v) -> eval_print fm rands (subst f (fix f v) v)
    | Label(InfoString f, t) ->
        let args,t' = take_args t in
        let () =
          Format.fprintf fm "@\n@[<v 2>%s %a ->"
            f (print_list print_value " ") args
        in
        let r = eval_print fm rands t' in
          Format.fprintf fm "@]";
          r
    | Label _ -> assert false

let print fm (ce, t) =
  try
    ignore (eval_print fm ce t);
    assert false
  with
    RaiseExcep _ -> Format.fprintf fm "@\nUNCAUGHT EXCEPTION OCCUR!@."
  | EventFail -> Format.fprintf fm "@\nFAIL!@."
