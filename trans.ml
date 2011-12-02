
open Utilities
open Syntax
open Type



(*
let rec unify_pat p typ =
  let map,desc =
    match p.pat_desc with
        PVar x ->
          let x' = Id.set_typ x typ in
            [x,x'], PVar x'
      | PConst t -> [], PConst (unify_sub_term t typ)
      | PConstruct(s,ps) ->
          assert (List.for_all (fun p -> not (is_poly_typ p.pat_typ)) ps);
          [], PConstruct(s,ps)
      | PNil -> [], PNil
      | PCons(p1,p2) ->
          let typ' =
            match typ with
                TList typ -> typ
              | _ -> assert false
          in
          let map1,p1' = unify_pat p1 typ' in
          let map2,p2' = unify_pat p2 typ in
            map1@@map2, PCons(p1',p2')
      | PPair(p1,p2) ->
          let typ1,typ2 =
            match typ with
                TPair(typ1,typ2) -> typ1, typ2
              | _ -> assert false
          in
          let map1,p1' = unify_pat p1 typ1 in
          let map2,p2' = unify_pat p2 typ2 in
            map1@@map2, PPair(p1', p2')
      | PRecord _ -> assert false
      | POr(p1,p2) ->
          let map1,p1' = unify_pat p1 typ in
          let map2,p2' = unify_pat p2 typ in
            map1@@map2, POr(p1',p2')
  in
    map, {pat_desc=desc; pat_typ=typ}

and unify_sub_term t typ =
  assert (not (is_poly_typ typ));
  if not (Type.can_unify t.typ typ) then (Format.printf "%a <=/=> %a@." print_typ t.typ print_typ typ; assert false);
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | NInt x -> NInt x
      | RandInt b -> RandInt b
      | Var x -> Var x
      | Fun(x, t1) ->
          let typ' =
            match typ with
                TFun(_,typ) -> typ
              | _ -> assert false
          in
            Fun(x, unify_sub_term t1 typ')
      | App(t, ts) ->
          assert (not (is_poly_typ t.typ));
          let t' = unify_sub_term t t.typ in
          let xs = get_args t.typ in
          let typs = take (List.map Id.typ xs) (List.length ts) in
          let ts' = List.map2 unify_sub_term ts typs in
            App(t',ts')
      | If(t1, t2, t3) ->
          let t1' = unify_sub_term t1 TBool in
          let t2' = unify_sub_term t2 typ in
          let t3' = unify_sub_term t3 typ in
            If(t1', t2', t3')
      | Branch(t1, t2) ->
          let t1' = unify_sub_term t1 typ in
          let t2' = unify_sub_term t2 typ in
            Branch(t1', t2')
      | Let(flag, bindings, t2) ->
          let aux (f,xs,t) =
            let typ' = List.fold_right (fun x typ -> match typ with TFun(x,typ) -> typ | _ -> assert false) xs (Id.typ f) in
              f, xs, unify_sub_term t typ'
          in
          let bindings' = List.map aux bindings in
          let t2' = unify_sub_term t2 typ in
            Let(flag, bindings', t2')
      | BinOp(op, t1, t2) ->
          let t1' = unify_sub_term t1 t1.typ in
          let t2' = unify_sub_term t2 t2.typ in
            BinOp(op, t1', t2')
      | Not t -> Not (unify_sub_term t t.typ)
      | Event(s,b) -> Event(s,b)
      | Record fields -> assert false
      | Proj(i,s,f,t) -> assert false
      | SetField(n,i,s,f,t1,t2) -> assert false
      | Nil -> Nil
      | Cons(t1,t2) ->
          let typ' =
            match typ with
                TList typ -> typ
              | _ -> assert false
          in
          let t1' = unify_sub_term t1 typ' in
          let t2' = unify_sub_term t2 typ in
            Cons(t1', t2')
      | Constr(s,ts) ->
          let ts' = List.map (fun t -> unify_sub_term t t.typ) ts in
            Constr(s, ts')
      | Match(t,pats) ->
          let t' = unify_sub_term t t.typ in
          let aux (p,c,t1) =
            let map,p' = unify_pat p t.typ in
            let map' = List.map (fun (x,x') -> x, make_var x') map in
            let c' =
              match c with
                  None -> None
                | Some c -> Some (unify_sub_term c TBool)
            in
              p', c', unify_sub_term (subst_term map' t1) typ
          in
          let pats' = List.map aux pats in
            Match(t', pats')
      | Raise t -> Raise (unify_sub_term t !typ_excep)
      | TryWith(t1,t2) ->
          let t1' = unify_sub_term t1 typ in
          let t2' = unify_sub_term t2 t2.typ in
            TryWith(t1', t2')
      | Bottom -> Bottom
      | Pair(t1,t2) ->
          let typ1,typ2 =
            match typ with
                TPair(typ1,typ2) -> typ1, typ2
              | _ -> assert false
          in
          let t1' = unify_sub_term t1 typ1 in
          let t2' = unify_sub_term t2 typ2 in
            Pair(t1', t2')
      | Fst t ->
          let typ' =
            match typ with
                TPair(typ,_) -> typ
              | _ -> assert false
          in
            Fst (unify_sub_term t typ')
      | Snd t ->
          let typ' =
            match typ with
                TPair(_,typ) -> typ
              | _ -> assert false
          in
            Snd (unify_sub_term t typ')
      | RandValue (_, _) -> assert false
  in
    {desc=desc; typ=typ}
*)


let rec rename_tvar_typ map = function
    TUnit -> TUnit
  | TBool -> TBool
  | TAbsBool -> TAbsBool
  | TInt ps -> TInt ps
  | TRInt p -> TRInt p
  | TVar({contents=None} as x) when List.mem_assq x map -> TVar (List.assq x map)
  | TVar({contents=None} as x) -> TVar x
  | TVar{contents=Some typ} -> rename_tvar_typ map typ
  | TFun(x,typ) -> TFun(Id.set_typ x (rename_tvar_typ map (Id.typ x)), rename_tvar_typ map typ)
  | TList typ -> TList (rename_tvar_typ map typ)
  | TPair(typ1,typ2) -> TPair(rename_tvar_typ map typ1, rename_tvar_typ map typ2)
  | TConstr(s,b) -> TConstr(s,b)
  | TUnknown -> TUnknown
  | TVariant _ -> assert false

and rename_tvar_var map x = Id.set_typ x (rename_tvar_typ map (Id.typ x))

and rename_tvar_pat map p =
  let typ = rename_tvar_typ map p.pat_typ in
  let desc =
    match p.pat_desc with
        PVar x -> PVar (rename_tvar_var map x)
      | PConst t -> PConst (rename_tvar map t)
      | PConstruct(s,ps) -> PConstruct(s, List.map (rename_tvar_pat map) ps)
      | PNil -> PNil
      | PCons(p1,p2) -> PCons(rename_tvar_pat map p1, rename_tvar_pat map p2)
      | PPair(p1,p2) -> PPair(rename_tvar_pat map p1, rename_tvar_pat map p2)
      | PRecord pats -> PRecord(List.map (fun (i,(s,f,p)) -> i,(s,f,rename_tvar_pat map p)) pats)
      | POr(p1,p2) -> POr(rename_tvar_pat map p1, rename_tvar_pat map p2)
  in
    {pat_desc=desc; pat_typ=typ}

and rename_tvar map t =
  let typ = rename_tvar_typ map t.typ in
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | NInt y -> NInt y
      | RandInt b -> RandInt b
      | RandValue(typ,b) -> RandValue(rename_tvar_typ map typ,b)
      | Var y -> Var (rename_tvar_var map y)
      | Fun(y, t) -> Fun(rename_tvar_var map y, rename_tvar map t)
      | App(t1, ts) -> App(rename_tvar map t1, List.map (rename_tvar map) ts)
      | If(t1, t2, t3) -> If(rename_tvar map t1, rename_tvar map t2, rename_tvar map t3)
      | Branch(t1, t2) -> Branch(rename_tvar map t1, rename_tvar map t2)
      | Let(flag, bindings, t2) ->
          let aux (f,xs,t) = rename_tvar_var map f, List.map (rename_tvar_var map) xs, rename_tvar map t in
          let bindings' = List.map aux bindings in
          let t2' = rename_tvar map t2 in
            Let(flag, bindings', t2')
      | BinOp(op, t1, t2) -> BinOp(op, rename_tvar map t1, rename_tvar map t2)
      | Not t1 ->Not (rename_tvar map t1)
      | Event(s,b) -> Event(s,b)
      | Record fields ->  Record (List.map (fun (f,(s,t1)) -> f,(s,rename_tvar map t1)) fields)
      | Proj(i,s,f,t1) -> Proj(i,s,f,rename_tvar map t1)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,rename_tvar map t1,rename_tvar map t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(rename_tvar map t1, rename_tvar map t2)
      | Constr(s,ts) -> Constr(s, List.map (rename_tvar map) ts)
      | Match(t1,pats) ->
          let aux (pat,cond,t1) = rename_tvar_pat map pat, apply_opt (rename_tvar map) cond, rename_tvar map t1 in
            Match(rename_tvar map t1, List.map aux pats)
      | Raise t -> Raise (rename_tvar map t)
      | TryWith(t1,t2) -> TryWith(rename_tvar map t1, rename_tvar map t2)
      | Pair(t1,t2) -> Pair(rename_tvar map t1, rename_tvar map t2)
      | Fst t -> Fst(rename_tvar map t)
      | Snd t -> Snd(rename_tvar map t)
      | Bottom -> Bottom
  in
    {desc=desc; typ=typ}



let rec get_tvars typ =
  let (@@@) xs ys = List.fold_left (fun xs y -> if List.memq y xs then xs else y::xs) xs ys in
    match typ with
        TUnit -> []
      | TBool -> []
      | TAbsBool -> assert false
      | TInt ps -> []
      | TRInt p -> []
      | TVar({contents=None} as x) -> [x]
      | TVar{contents=Some typ} -> get_tvars typ
      | TFun(x,typ2) -> get_tvars (Id.typ x) @@@ get_tvars typ2
      | TList typ -> get_tvars typ
      | TPair(typ1,typ2) -> get_tvars typ1 @@@ get_tvars typ2
      | TConstr(s,b) -> []
      | TUnknown _ -> assert false
      | TVariant _ -> assert false



let rec rename_poly_funs f t =
  let map,desc =
    match t.desc with
        Unit
      | True
      | False
      | Unknown
      | Int _
      | NInt _
      | RandInt _ -> [], t.desc
      | Var x when Id.same x f ->
          let x' = Id.new_var_id x in
            [x,x'], Var x'
      | Var x -> [], Var x
      | Fun(x, t) ->
          let map,t' = rename_poly_funs f t in
            map, Fun(x, t')
      | App({desc=Var x}, ts) when Id.same x f ->
          if is_poly_typ (Id.typ x)
          then
            let xs = take (get_args (Id.typ f)) (List.length ts) in
            let typ = List.fold_right2 (fun t x typ -> TFun(Id.set_typ x t.typ, typ)) ts xs t.typ in
            let x' = Id.new_var (Id.name x) typ in
            let maps,ts' = List.split (List.map (rename_poly_funs f) ts) in
              (x,x') :: List.flatten maps, App(make_var x', ts')
          else
            let maps,ts' = List.split (List.map (rename_poly_funs f) ts) in
            let x' = Id.new_var_id x in
              (x,x') :: List.flatten maps, App(make_var x', ts')
      | App({desc=Var x}, ts) ->
          let maps,ts' = List.split (List.map (rename_poly_funs f) ts) in
            List.flatten maps, App(make_var x, ts')
      | App(t, ts) ->
          let maps,ts' = List.split (List.map (rename_poly_funs f) ts) in
          let map,t' = rename_poly_funs f t in
            map @@ rev_flatten maps, App(t',ts')
      | If(t1, t2, t3) ->
          let map1,t1' = rename_poly_funs f t1 in
          let map2,t2' = rename_poly_funs f t2 in
          let map3,t3' = rename_poly_funs f t3 in
            map1@@map2@@map3, If(t1', t2', t3')
      | Branch(t1, t2) ->
          let map1,t1' = rename_poly_funs f t1 in
          let map2,t2' = rename_poly_funs f t2 in
            map1@@map2, Branch(t1', t2')
      | Let(flag, bindings, t2) ->
          let maps,bindings' = List.split (List.map (fun (g,xs,t) -> let map,t' = rename_poly_funs f t in map,(g,xs,t')) bindings) in
          let map2,t2' = rename_poly_funs f t2 in
            map2 @@ rev_flatten maps, Let(flag, bindings', t2')
      | BinOp(op, t1, t2) ->
          let map1,t1' = rename_poly_funs f t1 in
          let map2,t2' = rename_poly_funs f t2 in
            map1@@map2, BinOp(op, t1', t2')
      | Not t ->
          let map,t' = rename_poly_funs f t in
            map, Not t'
      | Event(s,b) -> [], Event(s,b)
      | Record fields -> assert false
      | Proj(i,s,f,t) -> assert false
      | SetField(n,i,s,f,t1,t2) -> assert false
      | Nil -> [], Nil
      | Cons(t1,t2) ->
          let map1,t1' = rename_poly_funs f t1 in
          let map2,t2' = rename_poly_funs f t2 in
            map1@@map2, Cons(t1', t2')
      | Constr(s,ts) ->
          let maps,ts' = List.split (List.map (rename_poly_funs f) ts) in
            rev_flatten maps, Constr(s, ts')
      | Match(t,pats) ->
          let map,t' = rename_poly_funs f t in
          let maps,pats' = List.split (List.map (fun (p,c,t) -> let map,t' = rename_poly_funs f t in map,(p,c,t')) pats) in
            map @@ rev_flatten maps, Match(t', pats')
      | Raise t ->
          let map,t' = rename_poly_funs f t in
            map, Raise t'
      | TryWith(t1,t2) ->
          let map1,t1' = rename_poly_funs f t1 in
          let map2,t2' = rename_poly_funs f t2 in
            map1@@map2, TryWith(t1', t2')
      | Bottom -> [], Bottom
      | Pair(t1,t2) ->
          let map1,t1' = rename_poly_funs f t1 in
          let map2,t2' = rename_poly_funs f t2 in
            map1@@map2, Pair(t1', t2')
      | Fst t ->
          let map,t' = rename_poly_funs f t in
            map, Fst t'
      | Snd t ->
          let map,t' = rename_poly_funs f t in
            map, Snd t'
      | RandValue (_, _) -> assert false
  in
    map, {desc=desc; typ=t.typ}


let rec copy_poly_funs top t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | NInt x -> NInt x
      | RandInt b -> RandInt b
      | Var x -> Var x
      | Fun(x, t) -> Fun(x, copy_poly_funs false t)
      | App(t, ts) -> App(copy_poly_funs false t, List.map (copy_poly_funs false) ts)
      | If(t1, t2, t3) -> If(copy_poly_funs false t1, copy_poly_funs false t2, copy_poly_funs false t3)
      | Branch(t1, t2) -> Branch(copy_poly_funs false t1, copy_poly_funs false t2)
      | Let(flag, [f, xs, t1], t2) when is_poly_typ (Id.typ f) ->
          let tvars = get_tvars (Id.typ f) in
          let () = assert (tvars > []) in
          let t2' = copy_poly_funs top t2 in
          let map,t2'' = rename_poly_funs f t2' in
          let n = List.length map in
          let () = if n >= 2 then Format.printf "COPY: %s(%d)@." (Id.name f) n in
            if map = [] && top && t2.desc = Unit
            then Let(flag, [f, xs, copy_poly_funs false t1], t2')
            else
              let aux t (_,f') =
                let tvar_map = List.map (fun v -> v, ref None) tvars in
                let () = Type.unify (rename_tvar_typ tvar_map (Id.typ f)) (Id.typ f') in
                let xs = List.map (rename_tvar_var tvar_map) xs in
                let t1 = rename_tvar tvar_map t1 in
                let typs = get_argtyps (Id.typ f') in
                let xs' = List.map2 (fun x typ -> Id.new_var (Id.name x) typ) xs (take typs (List.length xs)) in
                let xs_map = List.map2 (fun x x' -> x, make_var x') xs xs' in
                let t1 = subst_term xs_map t1 in
                let t1 =
                  match flag with
                      Flag.Nonrecursive -> t1
                    | Flag.Recursive -> subst f (make_var f') t1
                in
                let t1 = copy_poly_funs false t1 in
                  make_let_f flag [f', xs', t1] t
              in
                (List.fold_left aux t2'' map).desc
      | Let(flag, [f, xs, t1], t2) -> Let(flag, [f, xs, copy_poly_funs false t1], copy_poly_funs top t2)
      | Let _ -> assert false
      | BinOp(op, t1, t2) -> BinOp(op, copy_poly_funs false t1, copy_poly_funs false t2)
      | Not t -> Not (copy_poly_funs false t)
      | Event(s,b) -> Event(s,b)
      | Record fields -> Record (List.map (fun (f,(s,t)) -> f,(s,copy_poly_funs false t)) fields)
      | Proj(i,s,f,t) -> Proj(i,s,f,copy_poly_funs false t)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,copy_poly_funs false t1,copy_poly_funs false t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(copy_poly_funs false t1, copy_poly_funs false t2)
      | Constr(s,ts) -> Constr(s, List.map (copy_poly_funs false) ts)
      | Match(t,pats) -> Match(copy_poly_funs false t, List.map (fun (pat,cond,t) -> pat,cond,copy_poly_funs false t) pats)
      | Raise t -> Raise (copy_poly_funs false t)
      | TryWith(t1,t2) -> TryWith(copy_poly_funs false t1, copy_poly_funs false t2)
      | Bottom -> Bottom
      | Pair(t1,t2) -> Pair(copy_poly_funs false t1, copy_poly_funs false t2)
      | Fst t -> Fst (copy_poly_funs false t)
      | Snd t -> Snd (copy_poly_funs false t)
      | RandValue (_, _) -> assert false
  in
    {desc=desc; typ=t.typ}

let copy_poly_funs t =
  let t' = copy_poly_funs true t in
  let () = Type_check.check t' Type.TUnit in
    t'




let set_target t =
  let rec get_last_definition f t =
    match t.desc with
        Let(_, bindings, t2) ->
          let f,_,_ = last bindings in
            get_last_definition (Some f) t2
      | Fun _ -> assert false
      | _ -> f
  in
  let rec replace_main main t =
    match t.desc with
        Let(flag, bindings, t2) -> make_let_f flag bindings (replace_main main t2)
      | Fun _ -> assert false
      | _ -> main
  in
  let f = get_opt_val (get_last_definition None t) in
  let xs = get_args (Id.typ f) in
    match xs, Id.typ f with
        [], TUnit -> replace_main (make_var f) t
      | _ ->
          let aux x =
            match Id.typ x with
                TInt _ -> make_app randint_term [unit_term]
              | TUnit -> unit_term
              | TVar _ -> unit_term
              | typ -> {desc=RandValue(typ, false); typ=typ}
          in
          let args = List.map aux xs in
          let main = make_app {desc=Var f;typ=Id.typ f} args in
          let main' =
            let u = Id.new_var "main" main.typ in
              make_let [u, [], main] unit_term
          in
            replace_main main' t












(** [let f ... = fun x -> t] や [let f ... = let g x = t in g] を [let f ... x = t] に *)
let rec merge_let_fun t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | NInt x -> NInt x
      | RandInt b -> RandInt b
      | Var x -> Var x
      | App(t, ts) -> App(merge_let_fun t, List.map merge_let_fun ts)
      | If(t1, t2, t3) -> If(merge_let_fun t1, merge_let_fun t2, merge_let_fun t3)
      | Branch(t1, t2) -> Branch(merge_let_fun t1, merge_let_fun t2)
      | Let(flag, bindings, t2) ->
          let aux (f,xs,t) =
            let ys,t' = decomp_fun t in
              f, xs@ys, merge_let_fun t'
          in
            Let(flag, List.map aux bindings, merge_let_fun t2)
      | Fun(x, t) -> Fun(x, merge_let_fun t)
      | BinOp(op, t1, t2) -> BinOp(op, merge_let_fun t1, merge_let_fun t2)
      | Not t -> Not (merge_let_fun t)
      | Event(s,b) -> Event(s,b)
      | Record fields -> Record (List.map (fun (f,(s,t)) -> f,(s,merge_let_fun t)) fields)
      | Proj(i,s,f,t) -> Proj(i,s,f,merge_let_fun t)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,merge_let_fun t1,merge_let_fun t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(merge_let_fun t1, merge_let_fun t2)
      | Constr(s,ts) -> Constr(s, List.map merge_let_fun ts)
      | Match(t,pats) -> Match(merge_let_fun t, List.map (fun (pat,cond,t) -> pat,cond,merge_let_fun t) pats)
      | Raise t -> Raise (merge_let_fun t)
      | TryWith(t1,t2) -> TryWith(merge_let_fun t1, merge_let_fun t2)
      | Bottom -> Bottom
      | Pair(t1,t2) -> Pair(merge_let_fun t1, merge_let_fun t2)
      | Fst t -> Fst (merge_let_fun t)
      | Snd t -> Snd (merge_let_fun t)
      | RandValue _ -> assert false
  in
    {desc=desc; typ=t.typ}




let filter_base = List.filter (fun x -> is_base_typ (Id.typ x))

let rec lift_aux xs t =
  let defs,desc =
    match t.desc with
        Unit -> [], Unit
      | True -> [], True
      | False -> [], False
      | Unknown -> [], Unknown
      | Int n -> [], Int n
      | NInt x -> [], NInt x
      | RandInt b -> [], RandInt b
      | Var x -> [], Var x
      | Fun(x,t1) ->
          let f = Id.new_var "f" t.typ in
          let defs,t1' = lift_aux xs (make_let [f,[x],t1] (make_var f)) in
            defs,t1'.desc
      | App(t, ts) ->
          let defs,t' = lift_aux xs t in
          let defss,ts' = List.split (List.map (lift_aux xs) ts) in
            defs @ (List.flatten defss), App(t', ts')
      | If(t1,t2,t3) ->
          let defs1,t1' = lift_aux xs t1 in
          let defs2,t2' = lift_aux xs t2 in
          let defs3,t3' = lift_aux xs t3 in
            defs1 @ defs2 @ defs3, If(t1',t2',t3')
      | Branch(t1,t2) ->
          let defs1,t1' = lift_aux xs t1 in
          let defs2,t2' = lift_aux xs t2 in
            defs1 @ defs2, Branch(t1',t2')
      | Let(_,[_,[],_],_) -> assert false
      | Let(Flag.Nonrecursive,[f,ys,t1],t2) ->
          let fv = union' Id.compare (filter_base xs) (inter' Id.compare (get_fv t1) xs) in
          let ys' = fv @ ys in
          let typ = List.fold_right (fun x typ -> TFun(x,typ)) fv (Id.typ f) in
          let f' = Id.set_typ f typ in
          let f'' = List.fold_left (fun t x -> make_app t [make_var x]) (make_var f') fv in
          let defs1,t1' = lift_aux ys' t1 in
          let defs2,t2' = lift_aux xs (subst f f'' t2) in
            defs1 @ [(f',(ys',t1'))] @ defs2, t2'.desc
      | Let(Flag.Recursive,[f,ys,t1],t2) ->
          let fv = union (filter_base xs) (inter (get_fv t1) xs) in
          let ys' = fv @ ys in
          let typ = List.fold_right (fun x typ -> TFun(x,typ)) fv (Id.typ f) in
          let f' = Id.set_typ f typ in
          let f'' = List.fold_left (fun t x -> make_app t [make_var x]) (make_var f') fv in
          let defs1,t1' = lift_aux ys' (subst f f'' t1) in
          let defs2,t2' = lift_aux xs (subst f f'' t2) in
            defs1 @ [(f',(ys',t1'))] @ defs2, t2'.desc
(*
      | Let(flag,bindings,t2) ->
          let fv = xs in
          let lift_binding (f,ys,t) =
            let ys' = fv @ ys in
            let typ = List.fold_right (fun x typ -> TFun(x,typ)) fv (Id.typ f) in
            let f' = Id.new_var (Id.name f) typ in
            let t' =
              match flag with
                  Flag.Nonrecursive -> t
                | Flag.Recursive ->
                    (*** buggy ***)
                    let f'' = make_app (make_var f') (List.map make_var fv) in
                      subst f f'' t
            in
            let defs1,t'' = lift_aux ys' t' in
              defs1, (f', ys', t'')
          in
          let defss,bindings' = List.split (List.map lift_binding bindings) in
          let aux t (f,_,_) (f',_,_) =
            let f'' = make_app (make_var f') (List.map make_var fv) in
              subst f f'' t
          in
          let defs2,t2' = lift_aux xs (List.fold_left2 aux t2 bindings bindings') in
            List.flatten defss @ defs2, t2'.desc
*)
      | BinOp(op,t1,t2) ->
          let defs1,t1' = lift_aux xs t1 in
          let defs2,t2' = lift_aux xs t2 in
            defs1 @ defs2, BinOp(op,t1',t2')
      | Not t ->
          let defs,t' = lift_aux xs t in
            defs, Not t'
      | Event(s,b) -> [], Event(s,b)
      | Record fields ->
          let aux (s,(f,t)) =
            let defs,t' = lift_aux xs t in
              defs, (s,(f,t'))
          in
          let defss,fields' = List.split (List.map aux fields) in
            List.flatten defss, Record fields'
      | Proj(i,s,f,t) ->
          let defs,t' = lift_aux xs t in
            defs, Proj(i,s,f,t')
      | Nil -> [], Nil
      | Cons(t1,t2) ->
          let defs1,t1' = lift_aux xs t1 in
          let defs2,t2' = lift_aux xs t2 in
            defs1 @ defs2, Cons(t1',t2')
      | Constr(c,ts) ->
          let defss,ts' = List.split (List.map (lift_aux xs) ts) in
            List.flatten defss, Constr(c,ts')
      | Match(t,pats) ->
          let defs,t' = lift_aux xs t in
          let aux (pat,cond,t) (defs,pats) =
            let xs' = get_vars_pat pat @@ xs in
            let defs',cond' =
              match cond with
                  None -> [], None
                | Some t ->
                    let defs',t' = lift_aux xs' t in
                      defs', Some t'
            in
            let defs'',t' = lift_aux xs' t in
              defs''@defs'@defs, (pat,cond',t')::pats
          in
          let defs',pats' = List.fold_right aux pats (defs,[]) in
            defs', Match(t',pats')
      | Pair(t1,t2) -> 
          let defs1,t1' = lift_aux xs t1 in
          let defs2,t2' = lift_aux xs t2 in
            defs1 @ defs2, Pair(t1',t2')
      | Fst t -> 
          let defs,t' = lift_aux xs t in
            defs, Fst t'
      | Snd t -> 
          let defs,t' = lift_aux xs t in
            defs, Snd t'
      | Bottom -> [], Bottom
      | _ -> Format.printf "lift: %a@." pp_print_term t; assert false
  in
    defs, {desc=desc; typ=t.typ}
(** [lift t] で，[t] をlambda-lift する．the definitions of let expressions must be side-effect free *)
let lift t = lift_aux [](*(get_fv2 t)*) t



let rec canonize t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | NInt x -> NInt x
      | RandInt b -> RandInt b
      | Var x -> Var x
      | App(t, ts) ->
          let t' = canonize t in
          let ts' = List.map canonize ts in
            App(t', ts')
      | If(t1, t2, t3) ->
          let t1' = canonize t1 in
          let t2' = canonize t2 in
          let t3' = canonize t3 in
            If(t1', t2', t3')
      | Branch(t1, t2) ->
          let t1' = canonize t1 in
          let t2' = canonize t2 in
            Branch(t1', t2')
      | Let(flag, bindings, t) ->
          let bindings' = List.map (fun (f,xs,t) -> f,xs,canonize t) bindings in
          let t' = canonize t in
            Let(flag, bindings', t')
      | BinOp(Eq, {desc=Not t1}, t2)
      | BinOp(Eq, t1, {desc=Not t2}) ->
          let t1' = canonize t1 in
          let t2' = canonize t2 in
          let t1 = {desc=BinOp(Or, t1, t2); typ=TBool} in
          let t2 = {desc=BinOp(Or, {desc=Not t1';typ=TBool}, {desc=Not t2';typ=TBool}); typ=TBool} in
            BinOp(And, t1, t2)
      | BinOp(Eq, {desc=BinOp((Eq|Lt|Gt|Leq|Geq|And|Or) as bop, t1, t2)}, t3) ->
          let t1' = canonize t1 in
          let t2' = canonize t2 in
          let t3' = canonize t3 in
          let t1 = {desc=BinOp(Or, {desc=Not t3';typ=TBool}, {desc=BinOp(bop, t1',t2');typ=TBool}); typ=TBool} in
          let t2 = {desc=BinOp(Or, t3', {desc=Not{desc=BinOp(bop, t1', t2');typ=TBool};typ=TBool}); typ=TBool} in
            BinOp(And, t1, t2)
      | BinOp(Eq, t3, {desc=BinOp((Eq|Lt|Gt|Leq|Geq|And|Or) as bop, t1, t2)}) ->
          let t1' = canonize t1 in
          let t2' = canonize t2 in
          let t3' = canonize t3 in
          let t1 = {desc=BinOp(Or, {desc=Not t3';typ=TBool}, {desc=BinOp(bop, t1', t2');typ=TBool}); typ=TBool} in
          let t2 = {desc=BinOp(Or, t3', {desc=Not{desc=BinOp(bop, t1', t2');typ=TBool};typ=TBool}); typ=TBool} in
            BinOp(And, t1, t2)
      | BinOp(op, t1, t2) ->
          let t1' = canonize t1 in
          let t2' = canonize t2 in
            BinOp(op, t1', t2')
      | Not t ->
          let t' = canonize t in
            Not t'
      | Fun(x,t) ->
          let t' = canonize t in
            Fun(x, t')
      | Event(s,b) -> Event(s,b)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(canonize t1, canonize t2)
      | Snd _ -> assert false
      | Fst _ -> assert false
      | Pair (_, _) -> assert false
      | TryWith (_, _) -> assert false
      | Raise _ -> assert false
      | Match (_, _) -> assert false
      | Constr (_, _) -> assert false
      | SetField (_, _, _, _, _, _) -> assert false
      | Proj (_, _, _, _) -> assert false
      | Record _ -> assert false
      | RandValue (_, _) -> assert false
      | Bottom -> assert false
  in
    {desc=desc; typ=t.typ}















let part_eval t =
  let is_apply xs = function
      Var x -> xs = [x]
    | App(t, ts) ->
        let rec aux xs ts =
          match xs,ts with
              [], [] -> true
            | x::xs', {desc=Var y}::ts' when Id.same x y -> aux xs' ts'
            | _ -> false
        in
          aux xs (t::ts)
    | _ -> false
  in
  let is_alias xs = function
      Var x ->
        if xs = []
        then Some x
        else None
    | App({desc=Var f}, ts) ->
        let rec aux xs ts =
          match xs,ts with
              [], [] -> true
            | x::xs',{desc=Var y}::ts' when Id.same x y -> aux xs' ts'
            | _ -> false
        in
          if aux xs ts
          then Some f
          else None
    | _ -> None
  in
  let () = ignore (is_alias [] True) in
  let rec aux apply t =
    let desc =
      match t.desc with
          Unit -> Unit
        | True -> True
        | False -> False
        | Int n -> Int n
        | NInt x -> NInt x
        | RandInt b -> RandInt b
        | Var x ->
            begin
              try
                let xs, t1 = List.assoc x apply in
                  Let(Flag.Nonrecursive, [x, xs, t1], make_var x)
              with Not_found -> Var x
            end
        | Fun(x, t) -> Fun(x, aux apply t)
        | App({desc=Var f}, ts) ->
            if List.mem_assoc f apply
            then
              match ts with
                  [] ->
                    let xs, t1 = List.assoc f apply in
                      Let(Flag.Nonrecursive, [f, xs, t1], (make_var f))
                | [t] -> t.desc
                | t::ts' -> App(t, ts')
            else
              let ts' = List.map (aux apply) ts in
                App(make_var f, ts')
        | App({desc=Fun(x,t);typ=typ'}, ts) ->
            if is_apply [x] t.desc
            then
              match ts with
                  [] -> Fun(x,t)
                | [t] -> t.desc
                | t::ts' -> App(t, ts')
            else
              begin
                match ts with
                    [{desc=True|False}] -> (aux apply (subst x (List.hd ts) t)).desc
                  | _ ->
                      let t' = aux apply t in
                      let ts' = List.map (aux apply) ts in
                        App({desc=Fun(x,t');typ=typ'}, ts')
              end
        | App(t, ts) -> App(aux apply t, List.map (aux apply) ts)
        | If({desc=True}, t2, _) -> (aux apply t2).desc
        | If({desc=False}, _, t3) -> (aux apply t3).desc
        | If({desc=Not t1}, t2, t3) -> If(aux apply t1, aux apply t3, aux apply t2)
        | If(t1, t2, t3) ->
            if t2 = t3
            then t2.desc
            else If(aux apply t1, aux apply t2, aux apply t3)
        | Branch(t1, t2) -> Branch(aux apply t1, aux apply t2)
        | Let(flag, [f, xs, t1], t2) ->
            if is_apply xs t1.desc
            then (aux ((f,(xs,t1))::apply) (aux apply t2)).desc
            else
              begin
                match flag, is_alias xs t1.desc  with
                    Flag.Nonrecursive, None -> Let(flag, [f, xs, aux apply t1], aux apply t2)
                  | Flag.Nonrecursive, Some x -> (subst f (make_var x) (aux apply t2)).desc
                  | Flag.Recursive, Some x when not (List.mem f (get_fv t1)) -> (subst f {desc=Var x;typ=Id.typ x} (aux apply t2)).desc
                  | Flag.Recursive, _ -> Let(flag, [f, xs, aux apply t1], aux apply t2)
              end
        | Let _ -> assert false
        | BinOp(op, t1, t2) -> BinOp(op, aux apply t1, aux apply t2)
        | Not t -> Not (aux apply t)
        | Unknown -> Unknown
        | Event(s,b) -> Event(s,b)
        | Record fields -> Record (List.map (fun (s,(f,t)) -> s,(f,aux apply t)) fields)
        | Proj(i,s,f,t) -> Proj(i, s, f, aux apply t)
        | Nil -> Nil
        | Cons(t1,t2) -> Cons(aux apply t1, aux apply t2)
        | Constr(c,ts) -> Constr(c, List.map (aux apply) ts)
        | Match(t,pats) ->
            let aux' (pat,cond,t) = pat, apply_opt (aux apply) cond, aux apply t in
              Match(aux apply t, List.map aux' pats)
        | Snd _ -> assert false
        | Fst _ -> assert false
        | Pair (_, _) -> assert false
        | TryWith (_, _) -> assert false
        | Raise _ -> assert false
        | SetField (_, _, _, _, _, _) -> assert false
        | RandValue (_, _) -> assert false
        | Bottom -> assert false
    in
      {desc=desc; typ=t.typ}
  in
    aux [] t







(*
let part_eval2 t =
  let is_alias xs = function
      Var x ->
        if xs = []
        then Some x
        else None
    | App(Var f, ts) ->
        let rec aux xs ts =
          match xs,ts with
              [], [] -> true
            | x::xs',(Var y)::ts' -> x.id = y.id && aux xs' ts'
            | _ -> false
        in
          if aux xs ts
          then Some f
          else None
    | _ -> None
  in
  let () = ignore (is_alias [] True) in
  let rec aux = function
      Unit -> Unit
    | True -> True
    | False -> False
    | Int n -> Int n
    | NInt x -> NInt x
    | Var x -> Var x
    | Fun(x, t) ->
        let t' = aux t in
          Fun(x, t')
    | App(t, ts) ->
        let t' = aux t in
        let ts' = List.map (aux) ts in
          App(t', ts')
    | If(t1, t2, t3) ->
        let t1' = aux t1 in
        let t2' = aux t2 in
        let t3' = aux t3 in
          If(t1', t2', t3')
    | Branch(t1, t2) ->
        let t1' = aux t1 in
        let t2' = aux t2 in
          Branch(t1', t2')
    | Let _ -> Format.printf "Not implemented@."; assert false
(*
    | Let(f, xs, t1, t2) ->
        begin
          match is_alias xs t1 with
              None ->
                let t1' = aux t1 in
                let t2' = aux t2 in
                  Let(f, xs, t1', t2')
            | Some x ->
                aux (subst f (Var x) t2)
        end
    | Letrec(f, xs, t1, t2) ->
        begin
          match is_alias xs t1 with
              None ->
                let t1' = aux t1 in
                let t2' = aux t2 in
                  Letrec(f, xs, t1', t2')
            | Some x ->
                aux (subst f (Var x) t2)
        end
*)
    | BinOp(op, t1, t2) ->
        let t1' = aux t1 in
        let t2' = aux t2 in
          BinOp(op, t1', t2')
    | Not t ->
        let t' = aux t in
          Not t'
    | Fail -> Fail
    | Unknown -> Unknown
    | Label(b, t) ->
        let t' = aux t in
          Label(b, t')
    | Event(s,None) -> Event(s,None)
  in
  let t' = aux t in
  let t'' = simplify t' in
    t''
*)


















(** returns a term whose definitions of let expressions are side-effect free *)
let rec trans_let t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | NInt y -> NInt y
      | RandInt b -> RandInt b
      | Var y -> Var y
      | Fun(y, t) -> Fun(y, trans_let t)
      | App(t1, ts) ->
          let t1' = trans_let t1 in
          let ts' = List.map trans_let ts in
            App(t1', ts')
      | If(t1, t2, t3) ->
          let t1' = trans_let t1 in
          let t2' = trans_let t2 in
          let t3' = trans_let t3 in
            If(t1', t2', t3')
      | Branch(t1, t2) ->
          let t1' = trans_let t1 in
          let t2' = trans_let t2 in
            Branch(t1', t2')
      | Let(Flag.Nonrecursive, [f, [], t1], t2) ->
          App(make_fun f (trans_let t2), [trans_let t1])
      | Let(Flag.Nonrecursive, bindings, t2) when List.exists (fun (_,xs,_) -> xs=[]) bindings -> assert false
      | Let(flag, bindings, t2) ->
          let bindings' = List.map (fun (f,xs,t) -> f, xs, trans_let t) bindings in
          let t2' = trans_let t2 in
            Let(flag, bindings', t2')
      | BinOp(op, t1, t2) ->
          let t1' = trans_let t1 in
          let t2' = trans_let t2 in
            BinOp(op, t1', t2')
      | Not t1 ->
          let t1' = trans_let t1 in
            Not t1'
      | Event(s,b) -> Event(s,b)
      | Record fields ->  Record (List.map (fun (f,(s,t1)) -> f,(s,trans_let t1)) fields)
      | Proj(i,s,f,t1) -> Proj(i,s,f,trans_let t1)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,trans_let t1,trans_let t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(trans_let t1, trans_let t2)
      | Constr(s,ts) -> Constr(s, List.map trans_let ts)
      | Match(t1,pats) ->
          let aux (pat,cond,t1) = pat, cond, trans_let t1 in
            Match(trans_let t1, List.map aux pats)
      | TryWith(t1,t2) -> TryWith(trans_let t1, trans_let t2)
      | Pair(t1,t2) -> Pair(trans_let t1, trans_let t2)
      | Fst t -> Fst(trans_let t)
      | Snd t -> Snd(trans_let t)
      | Bottom -> Bottom
      | Raise _ -> assert false
      | RandValue _ -> assert false
  in
    {desc=desc; typ=t.typ}




