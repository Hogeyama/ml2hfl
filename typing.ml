
open Syntax


exception CannotUnify


let new_tvar () = TVar (ref None)
let new_var x =
  let typ = new_tvar () in
    {x with typ = typ}, typ


let (@@) = List.rev_append


let rec flatten typ =
  match typ with
      TFun((x,typ1), typ2) -> TFun((x,flatten typ1), flatten typ2) (*???*)
    | TVar{contents = None} -> TUnknown
    | TVar{contents = Some typ'} -> flatten typ'
    | TList(typ,ps) -> TList(flatten typ, ps)
    | _ -> typ

let rec flatten2 typ =
  match typ with
      TVar{contents = Some typ'} -> flatten2 typ'
    | _ -> typ

let rec occurs r typ =
  match flatten2 typ with
    TFun((_,typ1), typ2) -> occurs r typ1 || occurs r typ2
  | TVar({contents = None} as r') -> r == r'
  | _ -> false

let rec unify typ1 typ2 =
(*
Format.printf "unify %a %a@." (print_typ ML) (flatten typ1) (print_typ ML) (flatten typ2);
*)
  match flatten2 typ1, flatten2 typ2 with
      TUnit, TUnit
    | TBool, TBool
    | TInt _, TInt _ -> ()
    | TRInt _, TRInt _ -> ()
    | TFun((_,typ11), typ12), TFun((_,typ21), typ22) ->
        unify typ11 typ21;
        unify typ12 typ22
    | TList(typ1,_), TList(typ2,_) -> unify typ1 typ2
    | TVariant ctypss1, TVariant ctypss2 ->
        assert (ctypss1 = ctypss2)
    | TVar r1, TVar r2 when r1 == r2 -> ()
(*
    | TVar{contents = None}, TVar{contents = Some typ'} -> unify typ1 typ'
    | TVar{contents = Some typ'}, TVar{contents = None} -> unify typ' typ2
    | TVar{contents = Some(typ')}, typ
    | typ, TVar{contents = Some(typ')} -> unify typ typ'
*)
    | TVar({contents = None} as r), typ
    | typ, TVar({contents = None} as r) ->
        if occurs r typ then
          (Format.printf "occurs check failure: %a, %a@." print_typ (flatten typ1) print_typ (flatten typ2);
          raise CannotUnify)
        else
          r := Some typ
    | _ -> begin
        (if Flag.debug
        then Format.printf "unification error: %a, %a@." print_typ (flatten typ1) print_typ (flatten typ2));
        raise CannotUnify
    end

let dummy = new_var' "dummy"
(*let dummy () = fst (new_var (new_var' "dummy"))*)

let rec find_var_typ x env =
  try
    match assoc_id x env with
        TAbs s -> find_var_typ (Syntax.new_var s) env
      | typ -> typ
  with Not_found ->
    if Flag.debug
    then Format.printf "@.not found: %s@." x.name;
    assert false

let rec infer env t = 
  (*Format.printf "inferring %a@." (print_term_fm ML false) t;*)
  match t with
      Unit -> Unit, TUnit
    | True -> True, TBool
    | False -> False, TBool
    | Unknown -> Unknown, TBool
    | Int n -> Int n, TInt []
    | NInt x ->
        let typ = TInt [] in
          NInt {x with typ = typ}, typ
    | Var x ->
        let typ = find_var_typ x env in
          Var {x with typ = typ}, typ
    | Fun(x, t) ->
        let x', typ_x = new_var x in
        let env' = (x, typ_x) :: env in
        let t', typ = infer env' t in
          Fun(x', t'), TFun((x',typ_x), typ)
    | App(t, ts) ->
        let t', typ1 = infer env t in
        let ts', typs = List.split (List.map (infer env) ts) in
        let typ = new_tvar () in
        let typ' = List.fold_right (fun t1 t2 -> TFun((dummy,t1), t2)) typs typ in
          unify typ1 typ';
          App(t', ts'), typ
    | If(t1, t2, t3) ->
        let t1', typ1 = infer env t1 in
        let t2', typ2 = infer env t2 in
        let t3', typ3 = infer env t3 in
          unify typ1 TBool;
          unify typ2 typ3;
          If(t1', t2', t3'), typ2
    | Branch(t1, t2) ->
        let t1', typ1 = infer env t1 in
        let t2', typ2 = infer env t2 in
          unify typ1 typ2;
          Branch(t1', t2'), typ1
    | Let(flag, f, xs, t1, t2) ->
        if flag = Nonrecursive
        then
          let f', typ_f = new_var f in
          let xs', typs = List.split (List.map new_var xs) in
          let env1 = (List.combine xs' typs) @@ env in
          let env2 = (f, typ_f) :: env in
          let t1', typ1 = infer env1 t1 in
          let t2', typ2 = infer env2 t2 in
            unify typ_f (List.fold_right (fun typ1 typ2 -> TFun((dummy,typ1), typ2)) typs typ1);
            Let(flag, f', xs', t1', t2'), typ2
        else
          let f', typ_f = new_var f in
          let xs', typs = List.split (List.map new_var xs) in
          let env2 = (f, typ_f) :: env in
          let env1 = (List.combine xs' typs) @@ env2 in
          let t1', typ1 = infer env1 t1 in
          let t2', typ2 = infer env2 t2 in
            unify typ_f (List.fold_right (fun typ1 typ2 -> TFun((dummy,typ1), typ2)) typs typ1);
            Let(flag, f', xs', t1', t2'), typ2
    | BinOp(Eq as op, t1, t2) ->
        let t1', typ1 = infer env t1 in
        let t2', typ2 = infer env t2 in
          unify typ1 typ2;
          BinOp(op, t1', t2'), TBool
    | BinOp(Lt as op, t1, t2)
    | BinOp(Gt as op, t1, t2)
    | BinOp(Leq as op, t1, t2)
    | BinOp(Geq as op, t1, t2) ->
        let t1', typ1 = infer env t1 in
        let t2', typ2 = infer env t2 in
          unify typ1 (TInt[]);
          unify typ2 (TInt[]);
          BinOp(op, t1', t2'), TBool
    | BinOp(And as op, t1, t2)
    | BinOp(Or as op, t1, t2) ->
        let t1', typ1 = infer env t1 in
        let t2', typ2 = infer env t2 in
          unify typ1 TBool;
          unify typ2 TBool;
          BinOp(op, t1', t2'), TBool
    | BinOp(Add as op, t1, t2)
    | BinOp(Sub as op, t1, t2)
    | BinOp(Mult as op, t1, t2) ->
        let t1', typ1 = infer env t1 in
        let t2', typ2 = infer env t2 in
          unify typ1 (TInt[]);
          unify typ2 (TInt[]);
          BinOp(op, t1', t2'), (TInt[])
    | Not t ->
        let t', typ = infer env t in
          unify typ TBool;
          Not t', TBool
    | Fail ->
        let typ = new_tvar () in
          Fail, TFun((dummy,TUnit), typ)
    | Label(b,t) ->
        (*assert false*)
        let t', typ = infer env t in
          Label(b, t'), typ
    | LabelInt(n,t) ->
        let t', typ = infer env t in
          LabelInt(n, t'), typ
    | Event s ->
        Event s, TFun((dummy,TUnit), TUnit)
    | Nil ->
        let typ = new_tvar () in
          Nil, TList(typ,[])
    | Cons(t1,t2) ->
        let t1',typ1 = infer env t1 in
        let t2',typ2 = infer env t2 in
          unify (TList(typ1,[])) typ2;
          Cons(t1',t2'), typ2
    | Constr(c, ts) ->
        let aux typ t =
          let t',typ_t = infer env t in
            unify typ typ_t;
            t'
        in
        let typ = find_var_typ (Syntax.new_var c) env in
        let typs =
          match typ with
              TVariant vtyps -> List.assoc c vtyps
            | _ -> assert false
        in
          Constr(c, List.map2 aux typs ts), typ
    | Match(t1,t2,x,y,t3) ->
        let x',x_typ = new_var x in
        let y',y_typ = new_var y in
        let env' = (x,x_typ)::(y,y_typ)::env in
        let t1',typ1 = infer env t1 in
        let t2',typ2 = infer env t2 in
        let t3',typ3 = infer env' t3 in
          unify (TList(x_typ,[])) y_typ;
          unify typ1 y_typ;
          unify typ2 typ3;
          Match(t1',t2',x',y',t3'), typ2
    | Match_(t,pats) ->
        let t',typ_t = infer env t in
        let typ = new_tvar () in
        let aux (pat,t) pats =
          let pat',typ_pat,env' = infer_pattern env pat in
          let t',typ' = infer env' t in
            unify typ_t typ_pat;
            unify typ typ';
            (pat',t')::pats
        in
        let pats' = List.fold_right aux pats [] in
          Match_(t',pats'), typ
    | Type_decl(decls,t) ->
        let rec aux env (x,kind) =
          let typ = TAbs x in
            match kind with
                Variant ctypss ->
                  let env0 = (Syntax.new_var x,TVariant ctypss)::env in
                    List.fold_left (fun env (y,_) -> (Syntax.new_var y,typ)::env) env0 ctypss
              | Record fields -> assert false
        in
        let env' = List.fold_left aux env decls in
        let t',typ = infer env' t in
          Type_decl(decls,t'), typ

and infer_pattern env = function
    PVar x ->
      let x',typ = new_var x in
        PVar x', typ, (x,typ)::env
  | PConst c ->
      let c',typ = infer env c in
        assert (c = c');
        PConst c, typ, env
  | PTuple pats -> assert false
  | PConstruct(x,pats) ->
      let typ = find_var_typ (Syntax.new_var x) env in
      let vtyps =
        match typ with
            TVariant vtyps -> vtyps
          | _ -> assert false
      in
      let typs = List.assoc x vtyps in
      let aux pat typ (pats,env) =
        let pat',typ',env' = infer_pattern env pat in
          unify typ typ';
          pat'::pats, env'
      in
      let pats',env' = List.fold_right2 aux pats typs ([],env) in
        PConstruct(x,pats'), typ, env'
  | PRecord fields -> assert false
  | POr(pat1,pat2) ->
      let pat1',typ1,env1 = infer_pattern env pat1 in
      let pat2',typ2,env2 = infer_pattern env1 pat2 in
        unify typ1 typ2;
        POr(pat1',pat2'), typ1, env2



let simplify_id x = {x with typ = flatten x.typ}

let rec simplify = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | Var x -> Var (simplify_id x)
  | Fun(x, t) ->
      let x' = simplify_id x in
      let t' = simplify t in
        Fun(x', t')
  | App(t, ts) ->
      let t' = simplify t in
      let ts' = List.map simplify ts in
        App(t', ts')
  | If(t1, t2, t3) ->
      let t1' = simplify t1 in
      let t2' = simplify t2 in
      let t3' = simplify t3 in
        If(t1', t2', t3')
  | Branch(t1, t2) ->
      let t1' = simplify t1 in
      let t2' = simplify t2 in
        Branch(t1', t2')
  | Let(flag, f, xs, t1, t2) ->
      let t1' = simplify t1 in
      let t2' = simplify t2 in
        Let(flag, simplify_id f, List.map simplify_id xs, t1', t2')      
(*
  | Let(flag, bindings, t) ->
      let bindings' = List.map (fun (f,xs,t) -> simplify_id f, List.map simplify_id xs, simplify t) bindings in
      let t' = simplify t in
        Let(flag, bindings', t')
*)
  | BinOp (op, t1, t2) ->
      let t1' = simplify t1 in
      let t2' = simplify t2 in
        BinOp(op, t1', simplify t2')
  | Not t ->
      let t' = simplify t in
        Not t'
  | Label(b, t) -> Label(b, simplify t)
  | LabelInt(n, t) -> LabelInt(n, simplify t)
  | Fail -> Fail
  | Event s -> Event s
  | Nil -> Nil
  | Cons(t1,t2) -> Cons(simplify t1, simplify t2)
  | Constr(s,ts) -> Constr(s, List.map simplify ts)
  | Match(t1,t2,x,y,t3) -> Match(simplify t1, simplify t2, x, y, simplify t3)
  | Match_(t,pats) ->
      let aux (pat,t) = pat, simplify t in
        Match_(simplify t, List.map aux pats)
  | Type_decl(decls,t) ->
      Type_decl(decls, simplify t)

let rec match_arg_typ typ xs =
  match flatten typ,xs with
      TUnit,[] -> TUnit
    | TInt ps,[] -> TInt ps
    | TRInt p,[] -> TRInt p
    | TBool,[] -> TBool
    | TFun _,[] -> typ
    | TFun(_,typ2),x::xs' ->
        let typ2' = match_arg_typ typ2 xs' in
          TFun((x,x.typ),typ2')
    | TUnknown, _ -> TUnknown
    | TList _,[] -> typ
    | TVariant _,[] -> typ
    | TRecord _,[] -> typ
    | TAbs _,[] -> typ
    | typ, _ -> Format.printf "%a@." print_typ typ; assert false

let new_var_typ x =
  let rec aux = function
      TUnit -> TUnit
    | TAbsBool -> TAbsBool
    | TBool -> TBool
    | TInt ps -> TInt ps
    | TRInt p -> TRInt p
    | TVar _ -> assert false
    | TFun((_,typ1),typ2) ->
        let typ1' = aux typ1 in
        let typ2' = aux typ2 in
        let x = {(new_var' "x") with typ = typ1'} in
          TFun((x,typ1'),typ2')
    | TList(typ,ps) -> TList(aux typ,ps)
    | TConstr _ -> assert false
    | TVariant ctypss ->
        let aux (x,typs) = x, List.map aux typs in
          TVariant (List.map aux ctypss)
    | TRecord _ -> assert false
    | TAbs s -> assert false
    | TUnknown -> TUnknown
  in
    {x with typ = aux x.typ}

let rec match_arg = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | Var x -> Var x
  | Fun(x, t) ->
      let t' = match_arg t in
        Fun(x, t')
  | App(t, ts) ->
      let t' = match_arg t in
      let ts' = List.map match_arg ts in
        App(t', ts')
  | If(t1, t2, t3) ->
      let t1' = match_arg t1 in
      let t2' = match_arg t2 in
      let t3' = match_arg t3 in
        If(t1', t2', t3')
  | Branch(t1, t2) ->
      let t1' = match_arg t1 in
      let t2' = match_arg t2 in
        Branch(t1', t2')
  | Let(flag, f, xs, t1, t2) ->
      if flag = Nonrecursive
      then
        let xs' = List.map new_var_typ xs in
        let typ = match_arg_typ f.typ xs' in
        let f' = {f with typ = typ} in
        let t1' = List.fold_right2 subst xs (List.map (fun x -> Var x) xs') t1 in
        let t2' = subst f (Var f') t2 in
        let t1'' = match_arg t1' in
        let t2'' = match_arg t2' in
          Let(flag, f', xs', t1'', t2'')
      else
        let xs' = List.map new_var_typ xs in
        let typ = match_arg_typ f.typ xs' in
        let f' = {f with typ = typ} in
        let t1' = List.fold_right2 subst (f::xs) (List.map (fun x -> Var x) (f'::xs')) t1 in
        let t2' = subst f (Var f') t2 in
        let t1'' = match_arg t1' in
        let t2'' = match_arg t2' in
          Let(flag, f', xs', t1'', t2'')
  | BinOp(op, t1, t2) ->
      let t1' = match_arg t1 in
      let t2' = match_arg t2 in
        BinOp(op, t1', t2')
  | Not t ->
      let t' = match_arg t in
        Not t'
  | Fail -> Fail
  | Label(b, t) -> Label(b, match_arg t)
  | LabelInt(n, t) -> LabelInt(n, match_arg t)
  | Event s -> Event s
  | Nil -> Nil
  | Cons(t1,t2) -> Cons(match_arg t1, match_arg t2)
  | Constr(s,ts) -> Constr(s, List.map match_arg ts)
  | Match(t1,t2,x,y,t3) -> Match(match_arg t1, match_arg t2, x, y, match_arg t3)
  | Match_(t,pats) ->
      let aux (pat,t) = pat, match_arg t in
        Match_(match_arg t, List.map aux pats)        
  | Type_decl(decls,t) ->
      Type_decl(decls, match_arg t)



let typing t0 =
  let t1, typ = infer [] t0 in
  let () = unify typ TUnit in
  let t2 = simplify t1 in
    match_arg t2


let typing_defs defs t0 =
(*
  let () = Format.printf "%a@." (print_term_fm ML true) (List.fold_left (fun acc (f,(xs,t)) -> Letrec(f,xs,t,acc)) t0 defs) in
*)
  
  let fsub_pre = List.map (fun (f, _) -> f, fst (new_var f)) defs in
  let env = List.map (fun (f,f') -> f, f'.typ) fsub_pre in
  let fsub = List.map (fun (f, f') -> f, Var f) defs in
  let t0', typ = infer env t0 in
  let () = unify typ TUnit in
  let defs =
    List.map2
      (fun (_, (xs, t)) (f', _) ->
         let xs', typs = List.split (List.map new_var xs) in
(*
Format.printf "a: %a@." (print_term_fm ML false) t;
*)
         let t', typ = infer
           ((List.combine xs' typs) @@ env)
           (subst_term (fsub @ (List.map2 (fun id id' -> id, Var(id')) xs xs')) t) in
(*
Format.printf "<%a:%a>@." (print_term_fm ML false) t' (print_typ ML) (flatten typ);
Format.printf "<%a:%a>@." (print_term_fm ML false) (Var f') (print_typ ML) (flatten f'.typ);
*)
         let ft =  List.fold_right (fun x typ -> TFun((x,x.typ), typ)) xs' typ in
(*
List.iter (fun (f, t) -> Format.printf "%a:%a@." (print_term_fm ML false) (Var f) (print_typ ML) (flatten t)) env;
Format.printf "<%a:%a>@." (print_term_fm ML false) (Var f') (print_typ ML) (flatten ft);
*)
           unify f'.typ ft;
           f', (xs', t'))
      defs fsub
  in
    List.map (fun (f, (xs, t)) -> simplify_id f, (List.map simplify_id xs, match_arg (simplify t))) defs,
    match_arg (simplify t0')

let typing_defs defs t0 =
(*
  let () = Format.printf "%a@." (print_term_fm ML true) (List.fold_left (fun acc (f,(xs,t)) -> Letrec(f,xs,t,acc)) t0 defs) in
*)

  let env = List.map (fun (f, _) -> new_var f) defs in
  let fsub = List.map2 (fun (f, _) (f', _) -> f, Var(f')) defs env in
  let t0', typ = infer env t0 in
  let () = unify typ TUnit in
  let defs =
    List.map2
      (fun (_, (xs, t)) (f', _) ->
         let xs', typs = List.split (List.map new_var xs) in
(*
Format.printf "a: %a@." (print_term_fm ML false) t;
*)
         let t', typ = infer
           ((List.combine xs' typs) @@ env)
           (subst_term (fsub @ (List.map2 (fun id id' -> id, Var(id')) xs xs')) t) in
(*
Format.printf "<%a:%a>@." (print_term_fm ML false) t' (print_typ ML) (flatten typ);
Format.printf "<%a:%a>@." (print_term_fm ML false) (Var f') (print_typ ML) (flatten f'.typ);
*)
         let ft =  List.fold_right (fun x typ -> TFun((x,x.typ), typ)) xs' typ in
(*
List.iter (fun (f, t) -> Format.printf "%a:%a@." (print_term_fm ML false) (Var f) (print_typ ML) (flatten t)) env;
Format.printf "<%a:%a>@." (print_term_fm ML false) (Var f') (print_typ ML) (flatten ft);
*)
           unify f'.typ ft;
           f', (xs', t'))
      defs env
  in
    List.map (fun (f, (xs, t)) -> simplify_id f, (List.map simplify_id xs, match_arg (simplify t))) defs,
    match_arg (simplify t0')



let rec get_typ = function
    Unit -> TUnit
  | True -> TBool
  | False -> TBool
  | Unknown -> TBool
  | Int n -> TInt []
  | NInt x -> x.typ
  | Var x -> x.typ
  | Fun(x, t) -> TFun ((x,x.typ), get_typ t)
  | App(f, ts) ->
      let rec aux typ ts =
        match typ,ts with
            _,[] -> typ
          | TFun(_,typ2), t::ts' -> aux typ2 ts'
          | _ -> assert false
      in
        aux (get_typ f) ts
  | If(_, t2, _) -> get_typ t2
  | Branch(t1, _) -> get_typ t1
  | Let(_, _, _, _, t2) -> get_typ t2
  | BinOp((Eq|Lt|Gt|Leq|Geq|And|Or), _, _) -> TBool
  | BinOp((Add|Sub|Mult), _, _) -> TInt []
  | Not _ -> TBool
  | Fail -> TFun((dummy,TUnit), TUnit)
  | Label(_,t) -> get_typ t
  | Event s -> TFun((dummy,TUnit), TUnit)
  | Nil -> TList(TUnknown,[])
  | Cons(t1,_) -> TList(get_typ t1,[])



let type_checking t =
  try
    ignore (typing t)
  with CannotUnify ->
    Format.printf "Typing error:@.  %a@." Syntax.pp_print_term t;
    assert false


let is_int_term t =
  match get_typ t with
      TInt _ | TRInt _ -> true
    | _ -> false

let is_list_term t =
  match get_typ t with
      TList _ -> true
    | _ -> false

