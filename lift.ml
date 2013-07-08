
open Util
open Syntax
open Type

module RT = Ref_type


let get_rtyp_lift t f rtyp =
  let rec aux rtyp typ =
    match rtyp with
        RT.Inter rtyps -> RT.Inter (List.map (fun rtyp1 -> aux rtyp1 typ) rtyps)
      | RT.Union rtyps -> RT.Union (List.map (fun rtyp1 -> aux rtyp1 typ) rtyps)
      | RT.Fun(x,rtyp1,rtyp2) ->
          if RT.arg_num rtyp = arg_num typ
          then rtyp
          else
            let rtyp' = aux rtyp2 typ in
              if RT.occur x rtyp'
              then RT.ExtArg(x, rtyp1, rtyp')
              else rtyp'
      | _ -> assert false
  in
    aux rtyp (Trans.assoc_typ f t)

let get_rtyp_lift t f rtyp =
  let rtyp' = get_rtyp_lift t f rtyp in
  if !Flag.print_ref_typ
  then Format.printf "LIFT: %a: @[@[%a@]@ ==>@ @[%a@]@]@." Id.print f RT.print rtyp RT.print rtyp';
  rtyp'

let filter_base = List.filter (fun x -> is_base_typ (Id.typ x))

let compare_id x y =
  let aux x = not (is_base_typ (Id.typ x)), Id.to_string x in
    compare (aux x) (aux y)

let rec lift_aux post xs t =
  let defs,desc =
    match t.desc with
        Const _
      | Unknown
      | RandInt _
      | RandValue _
      | Var _ -> [], t.desc
      | Fun _ ->
          let f = Id.new_var ("f" ^ post) t.typ in
          let aux f ys t1 t2 =
            let fv = inter ~cmp:Id.compare (get_fv t1) xs in
            let fv = if !Flag.lift_fv_only then fv else uniq ~cmp:Id.compare (filter_base xs @@@ fv) in
            let fv = List.sort compare_id fv in
            let ys' = fv @ ys in
            let typ = List.fold_right (fun x typ -> TFun(x,typ)) fv (Id.typ f) in
            let f' = Id.set_typ f typ in
            let f'' = List.fold_left (fun t x -> make_app t [make_var x]) (make_var f') fv in
            let defs1,t1' = lift_aux post ys' t1 in
            let defs2,t2' = lift_aux post xs (subst f f'' t2) in
              defs1 @ [(f',(ys',t1'))] @ defs2, t2'
          in
          let xs,t1 = decomp_fun t in
          let defs,t' = aux f xs t1 (make_var f) in
            defs, t'.desc
      | App(t, ts) ->
          let defs,t' = lift_aux post xs t in
          let defss,ts' = List.split (List.map (lift_aux post xs) ts) in
            defs @ (List.flatten defss), App(t', ts')
      | If(t1,t2,t3) ->
          let defs1,t1' = lift_aux post xs t1 in
          let defs2,t2' = lift_aux post xs t2 in
          let defs3,t3' = lift_aux post xs t3 in
            defs1 @ defs2 @ defs3, If(t1',t2',t3')
      | Branch(t1,t2) ->
          let defs1,t1' = lift_aux post xs t1 in
          let defs2,t2' = lift_aux post xs t2 in
            defs1 @ defs2, Branch(t1',t2')
      | Let(Nonrecursive,bindings,t2) ->
          let aux (f,ys,t1) =
            let fv = inter ~cmp:Id.compare (get_fv t1) xs in
            let fv = if !Flag.lift_fv_only then fv else uniq ~cmp:Id.compare (filter_base xs @@@ fv) in
            let fv = List.sort compare_id fv in
            let ys' = fv @ ys in
            let typ = List.fold_right (fun x typ -> TFun(x,typ)) fv (Id.typ f) in
            let f' = Id.set_typ f typ in
            let f'' = List.fold_left (fun t x -> make_app t [make_var x]) (make_var f') fv in
            let defs1,t1' = lift_aux ("_" ^ Id.name f) ys' t1 in
              (f',(ys',t1'))::defs1,  f''
          in
          let defss,fs = List.split (List.map aux bindings) in
          let subst_f t = List.fold_left2 (fun t f'' (f,_,_) -> subst f f'' t) t fs bindings in
          let defs2,t2' = lift_aux post xs (subst_f t2) in
            List.flatten defss @ defs2, t2'.desc
      | Let(Recursive,bindings,t2) ->
          let fv = rev_map_flatten (fun (_,_,t) -> get_fv t) bindings in
          let fv = inter ~cmp:Id.compare (uniq ~cmp:Id.compare fv) xs in
          let fv = if !Flag.lift_fv_only then fv else uniq ~cmp:Id.compare (filter_base xs @@@ fv) in
          let fv = List.sort compare_id fv in
          let aux (f,_,_) =
            let f' = Id.set_typ f (List.fold_right (fun x typ -> TFun(x,typ)) fv (Id.typ f)) in
              f, (f', List.fold_left (fun t x -> make_app t [make_var x]) (make_var f') fv)
          in
          let fs = List.map aux bindings in
          let subst_f t = List.fold_left2 (fun t (_,(_,f'')) (f,_,_) -> subst f f'' t) t fs bindings in
          let aux (f,ys,t1) =
            let ys' = fv @ ys in
            let f' = fst (List.assoc f fs) in
            let defs1,t1' = lift_aux ("_" ^ Id.name f) ys' (subst_f t1) in
              (f',(ys',t1'))::defs1
          in
          let defs = flatten_map aux bindings in
          let defs2,t2' = lift_aux post xs (subst_f t2) in
            defs @ defs2, t2'.desc
      | BinOp(op,t1,t2) ->
          let defs1,t1' = lift_aux post xs t1 in
          let defs2,t2' = lift_aux post xs t2 in
            defs1 @ defs2, BinOp(op,t1',t2')
      | Not t ->
          let defs,t' = lift_aux post xs t in
            defs, Not t'
      | Event(s,b) -> [], Event(s,b)
      | Record fields ->
          let aux (s,(f,t)) =
            let defs,t' = lift_aux post xs t in
              defs, (s,(f,t'))
          in
          let defss,fields' = List.split (List.map aux fields) in
            List.flatten defss, Record fields'
      | Proj(i,s,f,t) ->
          let defs,t' = lift_aux post xs t in
            defs, Proj(i,s,f,t')
      | Nil -> [], Nil
      | Cons(t1,t2) ->
          let defs1,t1' = lift_aux post xs t1 in
          let defs2,t2' = lift_aux post xs t2 in
            defs1 @ defs2, Cons(t1',t2')
      | Constr(c,ts) ->
          let defss,ts' = List.split (List.map (lift_aux post xs) ts) in
            List.flatten defss, Constr(c,ts')
      | Match(t,pats) ->
          let defs,t' = lift_aux post xs t in
          let aux (pat,cond,t) (defs,pats) =
            let xs' = get_vars_pat pat @@@ xs in
            let defs',cond' = lift_aux post xs' t in
            let defs'',t' = lift_aux post xs' t in
              defs''@defs'@defs, (pat,cond',t')::pats
          in
          let defs',pats' = List.fold_right aux pats (defs,[]) in
            defs', Match(t',pats')
      | Pair(t1,t2) ->
          let defs1,t1' = lift_aux post xs t1 in
          let defs2,t2' = lift_aux post xs t2 in
            defs1 @ defs2, Pair(t1',t2')
      | Fst t ->
          let defs,t' = lift_aux post xs t in
            defs, Fst t'
      | Snd t ->
          let defs,t' = lift_aux post xs t in
            defs, Snd t'
      | Bottom -> [], Bottom
      | _ -> Format.printf "lift: %a@." pp_print_term t; assert false
  in
    defs, {desc=desc; typ=t.typ}

let lift ?(args=[]) t =
  lift_aux "" args(*(get_fv2 t)*) t, get_rtyp_lift t
