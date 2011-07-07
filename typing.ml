
open Util
open Syntax


exception CannotUnify


type key = KeyVar of int * string | KeyLabelResult of string | KeyTypeEntity of string


let type_decls = ref []

let new_tvar () = TVar (ref None)
let new_var x =
  let typ = new_tvar () in
    {x with typ = typ}, typ


let in_env k =
  List.mem_assoc k !type_decls

let add_type_env k typ =
  type_decls := (k,typ)::!type_decls

let add_exc_env exc typs =
  let rec aux = function
      [] -> [KeyTypeEntity "exn",TVariant[exc,typs]]
    | (KeyTypeEntity "exn",TVariant stypss)::xs -> (KeyTypeEntity "exn",TVariant ((exc,typs)::stypss)) :: xs
    | x::xs -> x :: aux xs
  in
    type_decls := aux !type_decls


let rec find_var_typ x env f_ext =
  try
    List.assoc x env
  with Not_found ->
    if f_ext
    then raise Not_found
    else
      let name =
        match x with
            KeyVar(_,x) -> "KeyVar " ^ x
          | KeyLabelResult s -> "KeyLabelResult " ^ s
          | KeyTypeEntity s -> "KeyTypeEntity " ^ s
      in
        if Flag.debug
        then Format.printf "@.not found: %s@." name;
        assert false


let get_type_of_constr s =
  find_var_typ (KeyLabelResult s) !type_decls false

let rec get_constrs_from_type = function
    TConstr(s,true) ->
      begin
        match find_var_typ (KeyTypeEntity s) !type_decls false with
            TVariant typs -> typs
      end
  | TVariant typs -> get_constrs_from_type (get_type_of_constr (fst (List.hd typs)))
  | _ -> assert false


let get_type_entity env = function
    TConstr(s, false) -> raise Not_found
  | TConstr(s, true) -> find_var_typ (KeyTypeEntity s) env false
  | typ -> typ

let get_label_arg s env =
  let typ = get_type_entity env (find_var_typ (KeyLabelResult s) env false) in
    match typ with
        TRecord(false,typs) -> snd (List.assoc s typs)
      | _ -> assert false

let get_constr_args s env =
  let typ = get_type_entity env (find_var_typ (KeyLabelResult s) env false) in
    match typ with
        TVariant stypss -> List.assoc s stypss
      | _ -> assert false


let rec simplify_typ typ =
  match typ with
      TUnit -> TUnit
    | TBool -> TBool
    | TAbsBool -> TAbsBool
    | TInt ps -> TInt ps
    | TRInt p -> TRInt p
    | TFun((x,typ1), typ2) -> TFun((x,simplify_typ typ1), simplify_typ typ2) (*???*)
    | TVar{contents = None} -> TUnknown
    | TVar{contents = Some typ'} -> simplify_typ typ'
    | TList(typ,ps) -> TList(simplify_typ typ, ps)
    | TConstr(c,b) -> TConstr(c,b)
    | TRecord(b,typs) -> TRecord(b, List.map (fun (s,(f,typ)) -> s,(f,simplify_typ typ)) typs)
    | TVariant ctypss -> TVariant (List.map (fun (s,typs) -> s, List.map simplify_typ typs) ctypss)
    | TUnknown -> TUnknown

let rec flatten2 typ =
  match typ with
      TVar{contents = Some typ'} -> flatten2 typ'
    | _ -> typ

let rec occurs r typ =
  match flatten2 typ with
    TFun((_,typ1), typ2) -> occurs r typ1 || occurs r typ2
  | TVar({contents = None} as r') -> r == r'
  | _ -> false

let rec unify env typ1 typ2 =
(*
Format.printf "unify env %a %a@." (print_typ ML) (simplify_typ typ1) (print_typ ML) (simplify_typ typ2);
*)
  match flatten2 typ1, flatten2 typ2 with
      TUnit, TUnit
    | TBool, TBool
    | TInt _, TInt _ -> ()
    | TRInt _, TRInt _ -> ()
    | TFun((_,typ11), typ12), TFun((_,typ21), typ22) ->
        unify env typ11 typ21;
        unify env typ12 typ22
    | TList(typ1,_), TList(typ2,_) -> unify env typ1 typ2
    | TRecord(_,typs1), TRecord(_,typs2) ->
        List.iter2 (fun (_,(_,typ1)) (_,(_,typ2)) -> unify env typ1 typ2) typs1 typs2
    | TVar r1, TVar r2 when r1 == r2 -> ()
(*
    | TVar{contents = None}, TVar{contents = Some typ'} -> unify env typ1 typ'
    | TVar{contents = Some typ'}, TVar{contents = None} -> unify env typ' typ2
    | TVar{contents = Some(typ')}, typ
    | typ, TVar{contents = Some(typ')} -> unify env typ typ'
*)
    | TVar({contents = None} as r), typ
    | typ, TVar({contents = None} as r) ->
        if occurs r typ then
          (Format.printf "occurs check failure: %a, %a@." print_typ (simplify_typ typ1) print_typ (simplify_typ typ2);
          raise CannotUnify)
        else
          r := Some typ
    | TConstr(s1,_), TConstr(s2,_) when s1=s2 -> ()
    | TConstr(s,true), typ
    | typ, TConstr(s,true) -> unify env typ (find_var_typ (KeyTypeEntity s) env false)
    | TConstr _, _
    | _, TConstr _
    | TVariant _, TVariant _ -> ()
    | _ -> begin
        (if Flag.debug
        then Format.printf "unification error: %a, %a@." print_typ (simplify_typ typ1) print_typ (simplify_typ typ2));
        let env' = List.filter (function (KeyVar(_,"store_cltype"),_) -> true | _ -> false) env in
          match env' with [] -> assert false | _ ->
        raise CannotUnify
    end

let dummy = new_var' "dummy" TUnknown
(*let dummy () = fst (new_var (new_var' "dummy"))*)

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
    | RandInt None -> RandInt None, TInt []
    | RandInt (Some t) ->
        let t',typ = infer env t in
        let typ_r = new_tvar () in
        let typ' = TFun((dummy,TInt[]),typ_r) in
          unify env typ typ';
          RandInt (Some t'), typ_r
    | Var x ->
        if x.name = "diff_keys" then (Format.printf "diff_keys typ: %a@.%a@." print_typ x.typ print_typ 
                                      (find_var_typ (KeyVar (2230, "diff_keys")) env true));
        let typ =
          try
            find_var_typ (KeyVar(x.id,x.name)) env true
          with Not_found ->
            if not (is_external x)
            then (Format.printf "\nNOT FOUND: %a@." print_id x; assert false);
            let rec new_tvars = function
                TUnit -> TUnit
              | TBool -> TBool
              | TAbsBool -> TAbsBool
              | TInt ps -> TInt ps
              | TRInt p -> TRInt p
              | TVar x -> new_tvar ()
              | TFun((x,typ1),typ2) ->
                  let typ1' = new_tvars typ1 in
                  let typ2' = new_tvars typ2 in
                    TFun(({x with typ=typ1}, typ1'), typ2')
              | TList(typ,ps) -> TList(new_tvars typ, ps)
              | TConstr(c,b) -> TConstr(c,b)
              | TVariant ctypss -> TVariant(List.map (fun (c,typs) -> c,List.map new_tvars typs) ctypss)
              | TRecord(b,fields) -> TRecord(b, List.map (fun (c,(f,typ)) -> c,(f,new_tvars typ)) fields)
              | TUnknown -> assert false
            in
              new_tvars x.typ
        in
        if x.name = "diff_keys" then (Format.printf "diff_keys typ: %a@.%a@." print_typ x.typ print_typ 
                                      (find_var_typ (KeyVar (2230, "diff_keys")) env true));
          Var {x with typ = typ}, typ
    | Fun(x, t) ->
        let x', typ_x = new_var x in
        let env' = (KeyVar(x.id,x.name), typ_x) :: env in
        let t', typ = infer env' t in
          Fun(x', t'), TFun((x',typ_x), typ)
    | App(t, ts) ->
        let t', typ1 = infer env t in
        let ts', typs = List.split (List.map (infer env) ts) in
        let typ = new_tvar () in
        let typ' = List.fold_right (fun t1 t2 -> TFun((dummy,t1), t2)) typs typ in
          unify env typ1 typ';
          App(t', ts'), typ
    | If(t1, t2, t3) ->
        let t1', typ1 = infer env t1 in
        let t2', typ2 = infer env t2 in
        let t3', typ3 = infer env t3 in
          unify env typ1 TBool;
          unify env typ2 typ3;
          If(t1', t2', t3'), typ2
    | Branch(t1, t2) ->
        let t1', typ1 = infer env t1 in
        let t2', typ2 = infer env t2 in
          unify env typ1 typ2;
          Branch(t1', t2'), typ1
    | Let(flag, f, xs, t1, t2) ->
        if flag = Nonrecursive
        then
          let f', typ_f = new_var f in
          let xs', typs = List.split (List.map new_var xs) in
          let env1 = (List.map2 (fun x typ -> KeyVar(x.id,x.name), typ) xs' typs) @@ env in
          let env2 = (KeyVar(f.id,f.name), typ_f) :: env in
          let t1', typ1 = infer env1 t1 in
          let t2', typ2 = infer env2 t2 in
            unify env typ_f (List.fold_right (fun typ1 typ2 -> TFun((dummy,typ1), typ2)) typs typ1);
            Let(flag, f', xs', t1', t2'), typ2
        else
          let f', typ_f = new_var f in
          let xs', typs = List.split (List.map new_var xs) in
          let env2 = (KeyVar(f.id,f.name), typ_f) :: env in
          let env1 = (List.map2 (fun x typ -> KeyVar(x.id,x.name), typ) xs' typs) @@ env2 in
          let t1', typ1 = infer env1 t1 in
          let t2', typ2 = infer env2 t2 in
            unify env typ_f (List.fold_right (fun typ1 typ2 -> TFun((dummy,typ1), typ2)) typs typ1);
            Let(flag, f', xs', t1', t2'), typ2
    | BinOp(Eq as op, t1, t2) ->
        let t1', typ1 = infer env t1 in
        let t2', typ2 = infer env t2 in
          unify env typ1 typ2;
          BinOp(op, t1', t2'), TBool
    | BinOp(Lt as op, t1, t2)
    | BinOp(Gt as op, t1, t2)
    | BinOp(Leq as op, t1, t2)
    | BinOp(Geq as op, t1, t2) ->
        let t1', typ1 = infer env t1 in
        let t2', typ2 = infer env t2 in
          unify env typ1 (TInt[]);
          unify env typ2 (TInt[]);
          BinOp(op, t1', t2'), TBool
    | BinOp(And as op, t1, t2)
    | BinOp(Or as op, t1, t2) ->
        let t1', typ1 = infer env t1 in
        let t2', typ2 = infer env t2 in
          unify env typ1 TBool;
          unify env typ2 TBool;
          BinOp(op, t1', t2'), TBool
    | BinOp(Add as op, t1, t2)
    | BinOp(Sub as op, t1, t2)
    | BinOp(Mult as op, t1, t2) ->
        let t1', typ1 = infer env t1 in
        let t2', typ2 = infer env t2 in
          unify env typ1 (TInt[]);
          unify env typ2 (TInt[]);
          BinOp(op, t1', t2'), (TInt[])
    | Not t ->
        let t', typ = infer env t in
          unify env typ TBool;
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
    | Event s -> Event s, TFun((dummy,TUnit), TUnit)
    | Record(true,fields) ->
        let aux (s,(f,t)) =
          let t',typ = infer env t in
            (s,(f,t')), (s,(f,typ))
        in
        let fields',typs = List.split (List.map aux fields) in
          Record(true,fields'), TRecord(true,typs)
    | Record(false,fields) ->
        let typ = find_var_typ (KeyLabelResult (fst (List.hd fields))) env false in
        let aux (s,(f,t)) =
          let t',typ = infer env t in
            (s,(f,t')), (s,(f,typ))
        in
        let fields',typs = List.split (List.map aux fields) in
          unify env typ (TRecord(false,typs));
          Record(false,fields'), typ
    | Proj(Some n,i,s,f,t) ->
        let rec aux j =
          if j >= n
          then []
          else (string_of_int j, (Immutable, new_tvar())) :: aux (j+1)
        in
        let typs = aux 0 in
        let t', typ' = infer env t in
        let typ_i = snd (snd (List.nth typs i)) in
          unify env (TRecord(true,typs)) typ';
          Proj(Some n,i,s,f,t'), typ_i
    | Proj(None,i,s,f,t) ->
        let typ = get_label_arg s env in
        let t', typ' = infer env t in
          Proj(None,i,s,f,t'), typ
    | SetField(None,i,s,f,t1,t2) ->
        let typ = get_label_arg s env in
        let t1', typ1 = infer env t1 in
        let t2', typ2 = infer env t2 in
          unify env typ1 typ;
          SetField(None,i,s,f,t1',t2'), TUnit
    | Nil ->
        let typ = new_tvar () in
          Nil, TList(typ,[])
    | Cons(t1,t2) ->
        let t1',typ1 = infer env t1 in
        let t2',typ2 = infer env t2 in
          unify env (TList(typ1,[])) typ2;
          Cons(t1',t2'), typ2
    | Constr(c, ts) ->
        let aux typ t =
          let t',typ_t = infer env t in
            unify env typ typ_t;
            t'
        in
        let typ = find_var_typ (KeyLabelResult c) env false in
        let typs = get_constr_args c env in
          Constr(c, List.map2 aux typs ts), typ
    | Match(t1,t2,x,y,t3) ->
        let x',x_typ = new_var x in
        let y',y_typ = new_var y in
        let env' = (KeyVar(x.id,x.name),x_typ)::(KeyVar(y.id,y.name),y_typ)::env in
        let t1',typ1 = infer env t1 in
        let t2',typ2 = infer env t2 in
        let t3',typ3 = infer env' t3 in
          unify env (TList(x_typ,[])) y_typ;
          unify env typ1 y_typ;
          unify env typ2 typ3;
          Match(t1',t2',x',y',t3'), typ2
    | Match_(t,pats) ->
        let t',typ_t = infer env t in
        let typ = new_tvar () in
        let aux (pat,cond,t) pats =
          let pat',typ_pat,env' = infer_pattern env pat in
          let t',typ' = infer env' t in
          let cond' =
            match cond with
                None -> None
              | Some t ->
                  let t',typ = infer env' t in
                    unify env typ TBool;
                    Some t'
          in
            unify env typ_t typ_pat;
            unify env typ typ';
            (pat',cond',t')::pats
        in
        let pats' = List.fold_right aux pats [] in
          Match_(t',pats'), typ
    | TryWith(t,pats) ->
        let t',typ = infer env t in
        let aux (pat,cond,t) pats =
          let pat',typ_pat,env' = infer_pattern env pat in
          let t',typ' = infer env' t in
          let cond' =
            match cond with
                None -> None
              | Some t ->
                  let t',typ = infer env t in
                    unify env typ TBool;
                    Some t'
          in
            unify env typ typ';
            (pat',cond',t')::pats
        in
        let pats' = List.fold_right aux pats [] in
          TryWith(t',pats'), typ
    | Type_decl(decls,t) ->
        let rec aux env (x,(_,kind)) =
          let typ = TConstr(x,true) in
            match kind with
                KAbstract -> env
              | KVariant ctypss ->
                  let aux typ' typs =
                    let env0 = (KeyTypeEntity x,typ')::env in
                      List.fold_left (fun env (y,typs) -> (KeyLabelResult y,typ)::env) env0 typs
                  in
                    aux (TVariant ctypss) ctypss
              | KRecord fields ->
                  let aux typ' typs =
                    let env0 = (KeyTypeEntity x,typ')::env in
                      List.fold_left (fun env (y,(_,typ'')) -> (KeyLabelResult y,typ)::env) env0 typs
                  in
                    aux (TRecord(false,fields)) fields
        in
        let env' = List.fold_left aux env decls in
        let t',typ = infer env' t in
          Type_decl(decls,t'), typ
    | Exception(exc,typs,t) ->
        let env' = (KeyTypeEntity exc,TVariant[exc,typs])::env in
        let t',typ = infer env' t in
          Exception(exc,typs,t'),typ

and infer_pattern env = function
    PVar x ->
      let x',typ = new_var x in
        PVar x', typ, (KeyVar(x.id,x.name),typ)::env
  | PConst c ->
      let c',typ = infer env c in
        assert (c = c');
        PConst c, typ, env
  | PConstruct(x,pats) ->
      let typ = find_var_typ (KeyLabelResult x) env false in
      let typs = get_constr_args x env in
      let aux pat typ (pats,env) =
        let pat',typ',env' = infer_pattern env pat in
          unify env typ typ';
          pat'::pats, env'
      in
      let pats',env' = List.fold_right2 aux pats typs ([],env) in
        PConstruct(x,pats'), typ, env'
  | PNil -> PNil, TList(new_tvar(),[]), env
  | PCons(pat1,pat2) ->
      let pat1',typ1,env1 = infer_pattern env pat1 in
      let pat2',typ2,env2 = infer_pattern env1 pat2 in
        unify env (TList(typ1,[])) typ2;
        PCons(pat1',pat2'), typ2, env2
  | PRecord(false,fields) ->
      let typ = find_var_typ (KeyLabelResult (let _,(l,_,_) = List.hd fields in l)) env false in
      let aux (_,(l,_,pat)) (pats,env) =
        let typ = get_label_arg l env in
        let pat',typ',env' = infer_pattern env pat in
          unify env typ typ';
          pat'::pats, env'
      in
      let fields',env' = List.fold_right aux fields ([],env) in
        PRecord(false,fields), typ, env'
  | PRecord(true,fields) ->
      let aux (_,(l,f,pat)) (pats,typs,env) =
        let pat',typ',env' = infer_pattern env pat in
          pat'::pats, (l,(f,typ'))::typs, env'
      in
      let fields',typs,env' = List.fold_right aux fields ([],[],env) in
        PRecord(true,fields), TRecord(true,typs), env'
  | POr(pat1,pat2) ->
      let pat1',typ1,env1 = infer_pattern env pat1 in
      let pat2',typ2,env2 = infer_pattern env1 pat2 in
        unify env typ1 typ2;
        POr(pat1',pat2'), typ1, env2



let simplify_id x = {x with typ = simplify_typ x.typ}

let rec simplify = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | RandInt None -> RandInt None
  | RandInt (Some t) -> RandInt (Some (simplify t))
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
  | Record(b,fields) -> Record(b, List.map (fun (f,(s,t)) -> f,(s,simplify t)) fields)
  | Proj(n,i,f,s,t) -> Proj(n,i,f,s,simplify t)
  | SetField(n,i,f,s,t1,t2) -> SetField(n,i,f,s,simplify t1,simplify t2)
  | Nil -> Nil
  | Cons(t1,t2) -> Cons(simplify t1, simplify t2)
  | Constr(s,ts) -> Constr(s, List.map simplify ts)
  | Match(t1,t2,x,y,t3) -> Match(simplify t1, simplify t2, x, y, simplify t3)
  | Match_(t,pats) ->
      let aux (pat,cond,t) = pat, apply_opt simplify cond, simplify t in
        Match_(simplify t, List.map aux pats)
  | TryWith(t,pats) ->
      let aux (pat,cond,t) = pat, apply_opt simplify cond, simplify t in
        TryWith(t,List.map aux pats)
  | Type_decl(decls,t) -> Type_decl(decls, simplify t)
  | Exception(exc,typs,t) -> Exception(exc,typs,simplify t)

let rec match_arg_typ typ xs =
  match simplify_typ typ,xs with
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
    | TConstr _,[] -> typ
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
        let x = new_var' "x" typ1' in
          TFun((x,typ1'),typ2')
    | TList(typ,ps) -> TList(aux typ,ps)
    | TVariant ctypss ->
        let aux (x,typs) = x, List.map aux typs in
          TVariant (List.map aux ctypss)
    | TRecord(b,typs) ->
        let typs' = List.map (fun (s,(f,typ)) -> s,(f,aux typ)) typs in
          TRecord(b,typs')
    | TConstr(s,b) -> TConstr(s,b)
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
  | RandInt None -> RandInt None
  | RandInt (Some t) -> RandInt (Some (match_arg t))
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
  | Record(b,fields) -> Record(b, List.map (fun (f,(s,t)) -> f,(s,match_arg t)) fields)
  | Proj(n,i,f,s,t) -> Proj(n,i,f,s,match_arg t)
  | SetField(n,i,f,s,t1,t2) -> SetField(n,i,f,s,match_arg t1,match_arg t2)
  | Nil -> Nil
  | Cons(t1,t2) -> Cons(match_arg t1, match_arg t2)
  | Constr(s,ts) -> Constr(s, List.map match_arg ts)
  | Match(t1,t2,x,y,t3) -> Match(match_arg t1, match_arg t2, x, y, match_arg t3)
  | Match_(t,pats) ->
      let aux (pat,cond,t) = pat, apply_opt match_arg cond, match_arg t in
        Match_(match_arg t, List.map aux pats)
  | TryWith(t,pats) ->
      let aux (pat,cond,t) = pat, apply_opt match_arg cond, match_arg t in
        TryWith(t,List.map aux pats)
  | Type_decl(decls,t) -> Type_decl(decls, match_arg t)
  | Exception(exc,typs,t) -> Exception(exc,typs,match_arg t)


let typing t0 =
(*
  let aux (k,typ) =
    let s = match k with
        KeyVar(_,s) -> "KeyVal " ^ s
      | KeyLabelArg s -> "KeyLabelArg" ^ s
      | KeyLabelResult s -> "KeyLabelResult" ^ s
      | KeyTypeEntity s -> "KeyTypeEntity" ^ s
    in
      Format.printf "%s, %a@." s print_typ typ
  in
    List.iter aux !type_decls;



*)
  let env = !type_decls in
  let t1, typ = infer env t0 in
  let () = unify env typ TUnit in
  let t2 = simplify t1 in
    match_arg t2


let typing_defs defs t0 =
(*
  let () = Format.printf "%a@." (print_term_fm ML true) (List.fold_left (fun acc (f,(xs,t)) -> Letrec(f,xs,t,acc)) t0 defs) in
*)

  let env = List.map (fun (f, _) -> KeyVar(f.id,f.name), new_tvar()) defs in
  let fsub = List.map2 (fun (f, _) (_,typ) -> f, Var {f with typ=typ}) defs env in
  let t0', typ = infer env t0 in
  let () = unify env typ TUnit in
  let defs =
    List.map2
      (fun (f, (xs, t)) (_, f_typ) ->
         let xs', typs = List.split (List.map new_var xs) in
(*
Format.printf "a: %a@." (print_term_fm ML false) t;
*)
         let t', typ = infer
           ((List.map2 (fun x typ -> KeyVar(x.id,x.name),typ) xs' typs) @@ env)
           (subst_term (fsub @ (List.map2 (fun id id' -> id, Var(id')) xs xs')) t) in
(*
Format.printf "<%a:%a>@." (print_term_fm ML false) t' (print_typ ML) (simplify_typ typ);
Format.printf "<%a:%a>@." (print_term_fm ML false) (Var f') (print_typ ML) (simplify_typ f'.typ);
*)
         let ft =  List.fold_right (fun x typ -> TFun((x,x.typ), typ)) xs' typ in
(*
List.iter (fun (f, t) -> Format.printf "%a:%a@." (print_term_fm ML false) (Var f) (print_typ ML) (simplify_typ t)) env;
Format.printf "<%a:%a>@." (print_term_fm ML false) (Var f') (print_typ ML) (simplify_typ ft);
*)
           unify env f_typ ft;
           {f with typ=f_typ}, (xs', t'))
      defs env
  in
    List.map (fun (f, (xs, t)) -> simplify_id f, (List.map simplify_id xs, match_arg (simplify t))) defs,
    match_arg (simplify t0')


let rec get_typ_pat = function
    PVar x -> x.typ
  | PRecord(b,pats) -> TRecord(b,List.map (fun (_,(s,f,p)) -> s,(f,get_typ_pat p)) pats)
  | _ -> assert false

let rec get_typ = function
    Unit -> TUnit
  | True -> TBool
  | False -> TBool
  | Unknown -> TBool
  | Int n -> TInt []
  | NInt x -> x.typ
  | RandInt None -> TInt []
  | RandInt (Some t) ->
      begin
        match get_typ t with
            TFun(_,typ) -> typ
          | _ -> assert false
      end
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




let rec set_var_unit = function
    TUnit -> TUnit
  | TBool -> TBool
  | TAbsBool -> TAbsBool
  | TInt ps -> TInt ps
  | TRInt p -> TRInt p
  | TFun((x,typ1), typ2) ->
      let typ1' = set_var_unit typ1 in
        TFun(({x with typ=typ1'},typ1'), set_var_unit typ2)
  | TVar{contents = None} -> TUnit
  | TVar{contents = Some typ'} -> set_var_unit typ'
  | TList(typ,ps) -> TList(set_var_unit typ, ps)
  | TConstr(c,b) -> TConstr(c,b)
  | TRecord(b,typs) -> TRecord(b, List.map (fun (s,(f,typ)) -> s,(f,set_var_unit typ)) typs)
  | TVariant ctypss -> TVariant (List.map (fun (s,typs) -> s, List.map set_var_unit typs) ctypss)
  | TUnknown -> TUnknown




let rec copy_poly_funs = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | RandInt None -> RandInt None
  | RandInt (Some t) -> RandInt (Some (copy_poly_funs t))
  | Var x -> Var x
  | Fun(x, t) -> assert false
  | App(t, ts) -> App(copy_poly_funs t, List.map copy_poly_funs ts)
  | If(t1, t2, t3) -> If(copy_poly_funs t1, copy_poly_funs t2, copy_poly_funs t3)
  | Branch(t1, t2) -> Branch(copy_poly_funs t1, copy_poly_funs t2)
  | Let(flag, f, xs, t1, t2) ->
      let t1' = copy_poly_funs t1 in
      let t2' = copy_poly_funs t2 in
      let fs =
        match flag with
            Nonrecursive -> List.filter (same_ident f) (get_fv t1' @@ get_fv t2')
          | Recursive -> List.filter (same_ident f) (get_fv t2')
      in
        if fs = []
        then Let(flag, f, xs, t1', t2')
        else
          let fs =
            if List.for_all (fun f -> same_type f.typ (List.hd fs).typ) (List.tl fs)
            then [List.hd fs]
            else fs
          in
          let n = List.length fs in
          let () = if n >= 2 then Format.printf "COPY: %s(%d)@." f.name n in
          let aux t f =
            let f' = new_var' f.name (set_var_unit f.typ) in
            let typs = get_argtyps f.typ in
            let xs' = List.map2 (fun x typ -> {(new_var_id x) with typ=typ}) xs (take typs (List.length xs)) in
            let map = List.map2 (fun x x' -> x, Var x') xs xs' in
            let t1'' = subst_term map t1' in
            let t1''' = subst f (Var f') t1'' in
            let t' = subst f (Var f') t in
              Let(flag, f', xs', t1''', t')
          in
            List.fold_left aux t2' fs
  | BinOp(op, t1, t2) -> BinOp(op, copy_poly_funs t1, copy_poly_funs t2)
  | Not t -> Not (copy_poly_funs t)
  | Fail -> Fail
  | Label(b, t) -> Label(b, copy_poly_funs t)
  | Event s -> Event s
  | Record(b,fields) -> Record(b, List.map (fun (f,(s,t)) -> f,(s,copy_poly_funs t)) fields)
  | Proj(n,i,s,f,t) -> Proj(n,i,s,f,copy_poly_funs t)
  | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,copy_poly_funs t1,copy_poly_funs t2)
  | Nil -> Nil
  | Cons(t1,t2) -> Cons(copy_poly_funs t1, copy_poly_funs t2)
  | Constr(s,ts) -> Constr(s, List.map copy_poly_funs ts)
  | Match(t1,t2,x,y,t3) -> Match(copy_poly_funs t1, copy_poly_funs t2, x, y, copy_poly_funs t3)
  | Match_(t,pats) -> Match_(copy_poly_funs t, List.map (fun (pat,cond,t) -> pat,cond,copy_poly_funs t) pats)
  | TryWith(t,pats) -> TryWith(copy_poly_funs t, List.map (fun (pat,cond,t) -> pat,cond,copy_poly_funs t) pats)
  | Type_decl(decls,t) -> Type_decl(decls, copy_poly_funs t)
  | Exception(exc,typs,t) -> Exception(exc, typs, copy_poly_funs t)
