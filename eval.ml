open Util
open Syntax
open Term_util

exception RaiseExcep of int list * typed_term
exception EventFail

let fix f v = {desc=Label(InfoId f, v); typ = v.typ; attr=[]}
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


let subst_arg = make_trans2 ()
let subst_arg_term (x,t) t' =
  match t'.desc with
  | Label(InfoId y, t1) when Id.same x y -> make_label (InfoTerm t) t1
  | _ -> subst_arg.tr2_term_rec (x,t) t'

let () = subst_arg.tr2_term <- subst_arg_term
let subst_arg x t t' = subst_arg.tr2_term (x,t) t'


let rec take_args = function
    {desc=Label(InfoTerm t1, t2)} ->
      let args,t2' = take_args t2 in
        t1 :: args, t2'
  | t -> [], t


let subst' x t t1 = subst x t (subst_arg x t t1)


let rec print_value fm t =
  match t.desc with
  | Const (Int n) when n < 0 -> Format.fprintf fm "(%d)" n
  | Fun _ -> Format.pp_print_string fm "<fun>"
  | Cons _ ->
      let rec aux t =
        match t.desc with
        | Nil -> []
        | Cons(t1,t2) -> t1 :: aux t2
        | _ -> assert false
      in
      Format.fprintf fm "[%a]" (print_list print_value ";") (aux t)
  | Tuple[t_1;t_2] -> Format.fprintf fm "(@[@[%a@],@ @[%a@]@])" print_value t_1 print_value t_2
  | _ -> Print.term fm t


let rec eval_print fm rands t =
  if false then Format.printf "EVAL:%a@.RANDS:%a@.@." Print.term t
                              (print_list Format.pp_print_int ";") rands;
  match t.desc with
  | Const(RandInt false) ->
      let x = Id.new_var Type.TUnit in
      List.tl rands, make_fun x (make_int (List.hd rands))
  | Const(RandInt true) -> assert false
  | Const(RandValue _) -> unsupported "eval: RandValue"
  | Const c -> rands, t
  | Var y -> unsupported "error trace with external funcitons"
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
  | Let(flag, bindings, t2) ->
      let aux (rands,vs) (f,xs,t) =
        let rands',v = eval_print fm rands (List.fold_right make_fun xs t) in
        rands', vs@[f,v]
      in
      let rands',vs = List.fold_left aux (rands,[]) bindings in
      let subst' (x,v) t =
        match flag with
        | Nonrecursive -> subst x (fun_info x v) t
        | Recursive -> subst x (fix x @@ fun_info x v) t
      in
      eval_print fm rands' (List.fold_right subst' vs t2)
  | BinOp(And, t1, t2) ->
      let rands',v1 = eval_print fm rands t1 in
      begin
        match v1.desc with
        | Const False -> rands', false_term
        | Const True -> eval_print fm rands' t2
        | _ -> assert false
      end
  | BinOp(Or, t1, t2) ->
      let rands',v1 = eval_print fm rands t1 in
      begin
        match v1.desc with
        | Const True -> rands', true_term
        | Const False -> eval_print fm rands' t2
        | _ -> assert false
      end
  | BinOp(op, t1, t2) ->
      let rands',v2 = eval_print fm rands t2 in
      let rands'',v1 = eval_print fm rands' t1 in
      let v =
        match op, v1.desc, v2.desc with
        | Eq, v1, v2 -> if v1 = v2 then true_term else false_term
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
        | Const True -> false_term
        | Const False -> true_term
        | _ -> assert false
      in
      rands', v
  | Event("fail",false) -> raise EventFail
  | Event _ -> assert false
  | Record fields -> raise (Fatal "Not implemented: eval_print Record")
  | Field(s,t1) -> raise (Fatal "Not implemented: eval_print Record")
  | SetField(s,t1,t2) -> raise (Fatal "Not implemented: eval_print Record")
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
      rands', {desc=Constr(s,vs); typ=t.typ; attr=[]}
  | Match(t1,pat::pats) ->
      let rec check v p =
        match v.desc, p.pat_desc with
        | _, PAny -> Some Fun.id
        | _, PVar x -> Some (subst x v)
        | _, PConst v' -> if v = v' then Some Fun.id else None
        | _, PAlias(p,x) ->
            begin
              match check v p with
              | None -> None
              | Some f -> Some (f -| subst x v)
            end
        | Constr(s, vs), PConstruct(s', ps) ->
            if s <> s'
            then None
            else
              let aux bind v p =
                match bind with
                | None -> None
                | Some f ->
                    match check v p with
                    | None -> None
                    | Some f' -> Some (f -| f')
              in
              List.fold_left2 aux (Some Fun.id) vs ps
        | Nil, PNil -> Some Fun.id
        | Cons(v1,v2), PCons(p1,p2) ->
            begin
              match check v1 p1, check v2 p2 with
              | None, _
              | _, None -> None
              | Some f1, Some f2 -> Some (f1 -| f2)
            end
        | Tuple[v1;v2], PTuple[p1;p2] ->
            begin
              match check v1 p1, check v2 p2 with
              | None, _
              | _, None -> None
              | Some f1, Some f2 -> Some (f1 -| f2)
            end
        | Record _, PRecord _ -> assert false
        | _, POr(p1, p2) ->
            begin
              match check v p1 with
              | None -> check v p2
              | Some f -> Some f
            end
        | _ -> assert false
      in
      let rands',v = eval_print fm rands t1 in
      let p,cond,t = pat in
      begin
        match check v p with
        | None -> eval_print fm rands' (make_match v pats)
        | Some f ->
            let rands'',v = eval_print fm rands' (f cond) in
            match v.desc with
            | Const True -> eval_print fm rands'' (f t)
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
  | Tuple[t1;t2] ->
      let rands',v2 = eval_print fm rands t2 in
      let rands'',v1 = eval_print fm rands' t1 in
      rands'', make_pair v1 v2
  | Proj(i,t) ->
      let rands',v = eval_print fm rands t in
      begin
        match v.desc with
        | Tuple vs -> rands', List.nth vs i
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
  | _ -> Format.printf "inlined_f: %a@." Print.constr t; assert false

let print fm (ce, t) =
  try
    ignore (eval_print fm ce t);
    assert false
  with
    RaiseExcep _ -> Format.fprintf fm "@\nUNCAUGHT EXCEPTION OCCUR!@."
  | EventFail -> Format.fprintf fm "@\nFAIL!@."
