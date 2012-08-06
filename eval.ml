open Utilities
open Syntax

exception RaiseExcep of int list * typed_term
exception EventFail

let fix f v = {desc=Label(InfoId f, v); typ = v.typ}
let rec funname top f t =
  match t.desc with
      Fun(x,t1) -> make_fun x (funname false f t1)
    | _ ->
        if top
        then t
        else make_label (InfoString (Id.name f)) t
let funname f v = funname true f v

let rec eval_print fm rands t =
  if false then Format.printf "EVAL:%a@.RANDS:%a@.@." pp_print_term t
  (print_list Format.pp_print_int ";" false) rands;
  match t.desc with
      Unit -> rands, t
    | True -> rands, t
    | False -> rands, t
    | Unknown -> assert false
    | Int _ -> rands, t
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
                  eval_print fm rands' (make_app (subst x v1 t) vs')
              | _ -> assert false
          end
    | If(t1, t2, t3) ->
        let rands', v = eval_print fm rands t1 in
          begin
            match v.desc with
                True -> eval_print fm rands' t2
              | False -> eval_print fm rands' t3
              | _ -> assert false
          end
    | Branch(t1, t2) -> assert false
    | Let(flag, bindings, t2) ->
        let aux (rands,vs) (f,xs,t) =
          let rands',v = eval_print fm rands (List.fold_right make_fun xs t) in
            rands', vs@[f,v]
        in
        let rands',vs = List.fold_left aux (rands,[]) bindings in
        let subst' (x,v) t =
          match flag with
              Nonrecursive -> subst x (funname x v) t
            | Recursive -> subst x (fix x (funname x v)) t
        in
          eval_print fm rands' (List.fold_right subst' vs t2)
    | BinOp(And, t1, t2) ->
        let rands',v1 = eval_print fm rands t1 in
          begin
            match v1.desc with
                False -> rands', false_term
              | True -> eval_print fm rands' t2
              | _ -> assert false
          end
    | BinOp(Or, t1, t2) ->
        let rands',v1 = eval_print fm rands t1 in
          begin
            match v1.desc with
                True -> rands', true_term
              | False -> eval_print fm rands' t2
              | _ -> assert false
          end
    | BinOp(op, t1, t2) ->
        let rands',v2 = eval_print fm rands t2 in
        let rands'',v1 = eval_print fm rands' t1 in
        let v =
          match op, v1.desc, v2.desc with
              Eq, v1, v2 -> if v1 = v2 then true_term else false_term
            | Lt, Int n1, Int n2 -> if n1 < n2 then true_term else false_term
            | Gt, Int n1, Int n2 -> if n1 > n2 then true_term else false_term
            | Leq, Int n1, Int n2 -> if n1 <= n2 then true_term else false_term
            | Geq, Int n1, Int n2 -> if n1 >= n2 then true_term else false_term
            | Add, Int n1, Int n2 -> make_int (n1 + n2)
            | Sub, Int n1, Int n2 -> make_int (n1 - n2)
            | Mult, Int n1, Int n2 -> make_int (n1 * n2)
            | _ -> assert false
        in
          rands'', v
    | Not t1 ->
        let rands',v1 = eval_print fm rands t1 in
        let v =
          match v1.desc with
              True -> false_term
            | False -> true_term
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
                        True -> eval_print fm rands'' (f t)
                      | False -> eval_print fm rands'' (make_match v pats)
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
    | Label(InfoString f, v) ->
        let () = Format.fprintf fm "@\n@[<v 2>%s ... ->" f in
        let r = eval_print fm rands v in
          Format.fprintf fm "@]";
          r
    | Label _ -> assert false

let print fm (ce, t) =
  try
    ignore (eval_print fm ce t);
    assert false
  with EventFail -> Format.fprintf fm "FAIL!@."
