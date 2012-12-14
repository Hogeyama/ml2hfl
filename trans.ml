
open Utilities
open Syntax
open Type
open VHorn

module RT = Ref_type


let rec id___typ = function
    TUnit -> TUnit
  | TBool -> TBool
  | TAbsBool -> TAbsBool
  | TInt -> TInt
  | TRInt p -> TRInt (id__ p)
  | TVar({contents=None} as x) -> TVar x
  | TVar{contents=Some typ} -> id___typ typ
  | TFun(x,typ) -> TFun(Id.set_typ x (id___typ (Id.typ x)), id___typ typ)
  | TList typ -> TList (id___typ typ)
  | TPair(typ1,typ2) -> TPair(id___typ typ1, id___typ typ2)
  | TConstr(s,b) -> TConstr(s,b)
  | TPred(typ,ps) -> TPred(id___typ typ, List.map id__ ps)

and id___var x = Id.set_typ x (id___typ (Id.typ x))

and id___pat p =
  let typ = id___typ p.pat_typ in
  let desc =
    match p.pat_desc with
        PAny -> PAny
      | PVar x -> PVar (id___var x)
      | PConst t -> PConst (id__ t)
      | PConstruct(s,ps) -> PConstruct(s, List.map id___pat ps)
      | PNil -> PNil
      | PCons(p1,p2) -> PCons(id___pat p1, id___pat p2)
      | PPair(p1,p2) -> PPair(id___pat p1, id___pat p2)
      | PRecord pats -> PRecord(List.map (fun (i,(s,f,p)) -> i,(s,f,id___pat p)) pats)
      | POr(p1,p2) -> POr(id___pat p1, id___pat p2)
  in
    {pat_desc=desc; pat_typ=typ}

and id___info = function
    InfoInt n -> InfoInt n
  | InfoString s -> InfoString s
  | InfoId x -> InfoId (id___var x)
  | InfoTerm t -> InfoTerm (id__ t)

and id__ t =
  let typ = id___typ t.typ in
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | RandInt b -> RandInt b
      | RandValue(typ,b) -> RandValue(id___typ typ,b)
      | Var y -> Var (id___var y)
      | Fun(y, t) -> Fun(id___var y, id__ t)
      | App(t1, ts) -> App(id__ t1, List.map id__ ts)
      | If(t1, t2, t3) -> If(id__ t1, id__ t2, id__ t3)
      | Branch(t1, t2) -> Branch(id__ t1, id__ t2)
      | Let(flag, bindings, t2) ->
          let bindings' = List.map (fun (f,xs,t) -> id___var f, List.map id___var xs, id__ t) bindings in
          let t2' = id__ t2 in
            Let(flag, bindings', t2')
      | BinOp(op, t1, t2) -> BinOp(op, id__ t1, id__ t2)
      | Not t1 -> Not (id__ t1)
      | Event(s,b) -> Event(s,b)
      | Record fields ->  Record (List.map (fun (f,(s,t1)) -> f,(s,id__ t1)) fields)
      | Proj(i,s,f,t1) -> Proj(i,s,f,id__ t1)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,id__ t1,id__ t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(id__ t1, id__ t2)
      | Constr(s,ts) -> Constr(s, List.map id__ ts)
      | Match(t1,pats) ->
          let aux (pat,cond,t1) = id___pat pat, id__ cond, id__ t1 in
            Match(id__ t1, List.map aux pats)
      | Raise t -> Raise (id__ t)
      | TryWith(t1,t2) -> TryWith(id__ t1, id__ t2)
      | Pair(t1,t2) -> Pair(id__ t1, id__ t2)
      | Fst t -> Fst(id__ t)
      | Snd t -> Snd(id__ t)
      | Bottom -> Bottom
      | Label(info, t) -> Label(id___info info, id__ t)
  in
    {desc=desc; typ=typ}




let rec id2___typ env = function
    TUnit -> TUnit
  | TBool -> TBool
  | TAbsBool -> TAbsBool
  | TInt -> TInt
  | TRInt p -> TRInt (id2__ env p)
  | TVar({contents=None} as x) -> TVar x
  | TVar{contents=Some typ} -> id2___typ env typ
  | TFun(x,typ) -> TFun(Id.set_typ x (id2___typ env (Id.typ x)), id2___typ env typ)
  | TList typ -> TList (id2___typ env typ)
  | TPair(typ1,typ2) -> TPair(id2___typ env typ1, id2___typ env typ2)
  | TConstr(s,b) -> TConstr(s,b)
  | TPred(typ,ps) -> TPred(id2___typ env typ, List.map (id2__ env) ps)

and id2___var env x = Id.set_typ x (id2___typ env (Id.typ x))

and id2___pat env p =
  let typ = id2___typ env p.pat_typ in
  let desc =
    match p.pat_desc with
        PAny -> PAny
      | PVar x -> PVar (id2___var env x)
      | PConst t -> PConst (id2__ env t)
      | PConstruct(s,ps) -> PConstruct(s, List.map (id2___pat env) ps)
      | PNil -> PNil
      | PCons(p1,p2) -> PCons(id2___pat env p1, id2___pat env p2)
      | PPair(p1,p2) -> PPair(id2___pat env p1, id2___pat env p2)
      | PRecord pats -> PRecord(List.map (fun (i,(s,f,p)) -> i,(s,f,id2___pat env p)) pats)
      | POr(p1,p2) -> POr(id2___pat env p1, id2___pat env p2)
  in
    {pat_desc=desc; pat_typ=typ}

and id2___info env = function
    InfoInt n -> InfoInt n
  | InfoString s -> InfoString s
  | InfoId x -> InfoId (id2___var env x)
  | InfoTerm t -> InfoTerm (id2__ env t)

and id2__ env t =
  let typ = id2___typ env t.typ in
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | RandInt b -> RandInt b
      | RandValue(typ,b) -> RandValue(id2___typ env typ,b)
      | Var y -> Var (id2___var env y)
      | Fun(y, t) -> Fun(id2___var env y, id2__ env t)
      | App(t1, ts) -> App(id2__ env t1, List.map (id2__ env) ts)
      | If(t1, t2, t3) -> If(id2__ env t1, id2__ env t2, id2__ env t3)
      | Branch(t1, t2) -> Branch(id2__ env t1, id2__ env t2)
      | Let(flag, bindings, t2) ->
          let bindings' = List.map (fun (f,xs,t) -> id2___var env f, List.map (id2___var env) xs, id2__ env t) bindings in
          let t2' = id2__ env t2 in
            Let(flag, bindings', t2')
      | BinOp(op, t1, t2) -> BinOp(op, id2__ env t1, id2__ env t2)
      | Not t1 -> Not (id2__ env t1)
      | Event(s,b) -> Event(s,b)
      | Record fields ->  Record (List.map (fun (f,(s,t1)) -> f,(s,id2__ env t1)) fields)
      | Proj(i,s,f,t1) -> Proj(i,s,f,id2__ env t1)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,id2__ env t1,id2__ env t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(id2__ env t1, id2__ env t2)
      | Constr(s,ts) -> Constr(s, List.map (id2__ env) ts)
      | Match(t1,pats) ->
          let aux (pat,cond,t1) = id2___pat env pat, id2__ env cond, id2__ env t1 in
            Match(id2__ env t1, List.map aux pats)
      | Raise t -> Raise (id2__ env t)
      | TryWith(t1,t2) -> TryWith(id2__ env t1, id2__ env t2)
      | Pair(t1,t2) -> Pair(id2__ env t1, id2__ env t2)
      | Fst t -> Fst(id2__ env t)
      | Snd t -> Snd(id2__ env t)
      | Bottom -> Bottom
      | Label(info, t) -> Label(id2___info env info, id2__ env t)
  in
    {desc=desc; typ=typ}




let rec assoc_typ f t =
  match t.desc with
      Unit -> []
    | True -> []
    | False -> []
    | Unknown -> []
    | Int _ -> []
    | RandInt _ -> []
    | RandValue _ -> []
    | Var _ -> []
    | Fun(_, t) -> assoc_typ f t
    | App(t1, ts) -> assoc_typ f t1 @@ rev_flatten_map (assoc_typ f) ts
    | If(t1, t2, t3) -> assoc_typ f t1 @@ assoc_typ f t2 @@ assoc_typ f t3
    | Branch(t1, t2) -> assoc_typ f t1 @@ assoc_typ f t2
    | Let(flag, bindings, t1) ->
        let aux (g,_,t) =
          let typs1 = if Id.same f g then [Id.typ g] else [] in
            typs1 @@ assoc_typ f t
        in
          assoc_typ f t1 @@ rev_flatten_map aux bindings
    | BinOp(_, t1, t2) -> assoc_typ f t1 @@ assoc_typ f t2
    | Not t1 -> assoc_typ f t1
    | Event _ -> []
    | Record fields -> rev_flatten_map (fun (_,(_,t1)) -> assoc_typ f t1) fields
    | Proj(_,_,_,t1) -> assoc_typ f t1
    | SetField(_,_,_,_,t1,t2) -> assoc_typ f t1 @@ assoc_typ f t2
    | Nil -> []
    | Cons(t1,t2) -> assoc_typ f t1 @@ assoc_typ f t2
    | Constr(s,ts) -> rev_flatten_map (assoc_typ f) ts
    | Match(t1,pats) ->
        let aux (_,cond,t) = assoc_typ f cond @@ assoc_typ f t in
          assoc_typ f t1 @@ rev_flatten_map aux pats
    | Raise t -> assoc_typ f t
    | TryWith(t1,t2) -> assoc_typ f t1 @@ assoc_typ f t2
    | Pair(t1,t2) -> assoc_typ f t1 @@ assoc_typ f t2
    | Fst t -> assoc_typ f t
    | Snd t -> assoc_typ f t
    | Bottom -> []
    | Label(_,t) -> assoc_typ f t

let assoc_typ f t =
  let typs = assoc_typ f t in
    match typs with
        [] -> raise Not_found
      | [typ] -> typ
      | _ -> Format.printf "VAR:%a@.PROG:%a@." Id.print f pp_print_term t; assert false



let rec flatten_tvar_typ = function
    TUnit -> TUnit
  | TBool -> TBool
  | TAbsBool -> TAbsBool
  | TInt -> TInt
  | TRInt p -> TRInt (flatten_tvar p)
  | TVar({contents=None} as x) -> TVar x
  | TVar{contents=Some typ} -> flatten_tvar_typ typ
  | TFun(x,typ) -> TFun(Id.set_typ x (flatten_tvar_typ (Id.typ x)), flatten_tvar_typ typ)
  | TList typ -> TList(flatten_tvar_typ typ)
  | TPair(typ1,typ2) -> TPair(flatten_tvar_typ typ1, flatten_tvar_typ typ2)
  | TConstr(s,b) -> TConstr(s,b)
  | TPred(typ,ps) -> TPred(flatten_tvar_typ typ, List.map flatten_tvar ps)

and flatten_tvar_var x = Id.set_typ x (flatten_tvar_typ (Id.typ x))

and flatten_tvar_pat p =
  let typ = flatten_tvar_typ p.pat_typ in
  let desc =
    match p.pat_desc with
        PAny -> PAny
      | PVar x -> PVar (flatten_tvar_var x)
      | PConst t -> PConst (flatten_tvar t)
      | PConstruct(s,ps) -> PConstruct(s, List.map flatten_tvar_pat ps)
      | PNil -> PNil
      | PCons(p1,p2) -> PCons(flatten_tvar_pat p1, flatten_tvar_pat p2)
      | PPair(p1,p2) -> PPair(flatten_tvar_pat p1, flatten_tvar_pat p2)
      | PRecord pats -> PRecord(List.map (fun (i,(s,f,p)) -> i,(s,f,flatten_tvar_pat p)) pats)
      | POr(p1,p2) -> POr(flatten_tvar_pat p1, flatten_tvar_pat p2)
  in
    {pat_desc=desc; pat_typ=typ}

and flatten_tvar t =
  let typ = flatten_tvar_typ t.typ in
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | RandInt b -> RandInt b
      | RandValue(typ,b) -> RandValue(flatten_tvar_typ typ,b)
      | Var y -> Var (flatten_tvar_var y)
      | Fun(y, t) -> Fun(flatten_tvar_var y, flatten_tvar t)
      | App(t1, ts) -> App(flatten_tvar t1, List.map flatten_tvar ts)
      | If(t1, t2, t3) -> If(flatten_tvar t1, flatten_tvar t2, flatten_tvar t3)
      | Branch(t1, t2) -> Branch(flatten_tvar t1, flatten_tvar t2)
      | Let(flag, bindings, t2) ->
          let bindings' = List.map (fun (f,xs,t) -> flatten_tvar_var f, List.map flatten_tvar_var xs, flatten_tvar t) bindings in
          let t2' = flatten_tvar t2 in
            Let(flag, bindings', t2')
      | BinOp(op, t1, t2) -> BinOp(op, flatten_tvar t1, flatten_tvar t2)
      | Not t1 -> Not (flatten_tvar t1)
      | Event(s,b) -> Event(s,b)
      | Record fields ->  Record (List.map (fun (f,(s,t1)) -> f,(s,flatten_tvar t1)) fields)
      | Proj(i,s,f,t1) -> Proj(i,s,f,flatten_tvar t1)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,flatten_tvar t1,flatten_tvar t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(flatten_tvar t1, flatten_tvar t2)
      | Constr(s,ts) -> Constr(s, List.map flatten_tvar ts)
      | Match(t1,pats) ->
          let aux (pat,cond,t1) = flatten_tvar_pat pat, flatten_tvar cond, flatten_tvar t1 in
            Match(flatten_tvar t1, List.map aux pats)
      | Raise t -> Raise (flatten_tvar t)
      | TryWith(t1,t2) -> TryWith(flatten_tvar t1, flatten_tvar t2)
      | Pair(t1,t2) -> Pair(flatten_tvar t1, flatten_tvar t2)
      | Fst t -> Fst(flatten_tvar t)
      | Snd t -> Snd(flatten_tvar t)
      | Bottom -> Bottom
      | Label(info,t) -> Label(info, flatten_tvar t)
  in
    {desc=desc; typ=typ}







let rec inst_tvar_tunit_typ = function
    TUnit -> TUnit
  | TBool -> TBool
  | TAbsBool -> TAbsBool
  | TInt -> TInt
  | TRInt _ -> assert false
  | TVar({contents=None} as r) -> r := Some TUnit; TUnit
  | TVar{contents=Some typ} -> inst_tvar_tunit_typ typ
  | TFun(x,typ) -> TFun(Id.set_typ x (inst_tvar_tunit_typ (Id.typ x)), inst_tvar_tunit_typ typ)
  | TList typ -> TList (inst_tvar_tunit_typ typ)
  | TConstr(s,b) -> TConstr(s,b)
  | TPair(typ1,typ2) -> TPair(inst_tvar_tunit_typ typ1, inst_tvar_tunit_typ typ2)
  | TPred(typ,ps) -> TPred(inst_tvar_tunit_typ typ, ps)

and inst_tvar_tunit_var x =
  Id.set_typ x (inst_tvar_tunit_typ (Id.typ x))

and inst_tvar_tunit_pat p =
  let typ = inst_tvar_tunit_typ p.pat_typ in
  let desc =
    match p.pat_desc with
        PAny -> PAny
      | PVar x -> PVar (inst_tvar_tunit_var x)
      | PConst t -> PConst (inst_tvar_tunit t)
      | PConstruct(c,ps) -> PConstruct(c, List.map inst_tvar_tunit_pat ps)
      | PNil -> PNil
      | PCons(p1,p2) -> PCons(inst_tvar_tunit_pat p1, inst_tvar_tunit_pat p2)
      | PRecord _ -> assert false
      | POr _ -> assert false
      | PPair(p1,p2) -> PPair(inst_tvar_tunit_pat  p1, inst_tvar_tunit_pat p2)
  in
    {pat_desc=desc; pat_typ=typ}

and inst_tvar_tunit t =
  let typ' = inst_tvar_tunit_typ t.typ in
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | Var x -> Var (inst_tvar_tunit_var x)
      | RandInt b -> RandInt b
      | RandValue(typ,b) -> RandValue(typ,b)
      | Fun(x,t) -> Fun(inst_tvar_tunit_var x, inst_tvar_tunit t)
      | App(t, ts) -> App(inst_tvar_tunit t, List.map inst_tvar_tunit ts)
      | If(t1, t2, t3) -> If(inst_tvar_tunit t1, inst_tvar_tunit t2, inst_tvar_tunit t3)
      | Branch(t1, t2) -> Branch(inst_tvar_tunit t1, inst_tvar_tunit t2)
      | Let(flag, bindings, t) ->
          let aux (f,xs,t) = inst_tvar_tunit_var f, List.map inst_tvar_tunit_var xs, inst_tvar_tunit t in
          let bindings' = List.map aux bindings in
            Let(flag, bindings', inst_tvar_tunit t)
      | BinOp(op, t1, t2) -> BinOp(op, inst_tvar_tunit t1, inst_tvar_tunit t2)
      | Not t -> Not (inst_tvar_tunit t)
      | Event(s,b) -> Event(s,b)
      | Record _ -> assert false
      | Proj _ -> assert false
      | SetField _ -> assert false
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(inst_tvar_tunit t1, inst_tvar_tunit t2)
      | Constr(c,ts) -> Constr(c, List.map inst_tvar_tunit ts)
      | Match(t1,pats) ->
          let aux (p,t1,t2) = inst_tvar_tunit_pat p, inst_tvar_tunit t1, inst_tvar_tunit t2 in
          Match(inst_tvar_tunit t1, List.map aux pats)
      | Raise t -> Raise (inst_tvar_tunit t)
      | TryWith(t1,t2) -> TryWith(inst_tvar_tunit t1, inst_tvar_tunit t2)
      | Bottom -> Bottom
      | Pair(t1,t2) -> Pair(inst_tvar_tunit t1, inst_tvar_tunit t2)
      | Fst t -> Fst (inst_tvar_tunit t)
      | Snd t -> Snd (inst_tvar_tunit t)
      | Label(info,t) -> Label(info, inst_tvar_tunit t)
  in
    {desc=desc; typ=typ'}



let rec rename_tvar_typ map = function
    TUnit -> TUnit
  | TBool -> TBool
  | TAbsBool -> TAbsBool
  | TInt -> TInt
  | TRInt p -> TRInt p
  | TVar({contents=None} as x) when List.mem_assq x map -> TVar (List.assq x map)
  | TVar({contents=None} as x) -> TVar x
  | TVar{contents=Some typ} -> rename_tvar_typ map typ
  | TFun(x,typ) -> TFun(Id.set_typ x (rename_tvar_typ map (Id.typ x)), rename_tvar_typ map typ)
  | TList typ -> TList(rename_tvar_typ map typ)
  | TPair(typ1,typ2) -> TPair(rename_tvar_typ map typ1, rename_tvar_typ map typ2)
  | TConstr(s,b) -> TConstr(s,b)
  | TPred(typ,ps) -> TPred(rename_tvar_typ map typ, ps)

and rename_tvar_var map x = Id.set_typ x (rename_tvar_typ map (Id.typ x))

and rename_tvar_pat map p =
  let typ = rename_tvar_typ map p.pat_typ in
  let desc =
    match p.pat_desc with
        PAny -> PAny
      | PVar x -> PVar (rename_tvar_var map x)
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
      | Not t1 -> Not (rename_tvar map t1)
      | Event(s,b) -> Event(s,b)
      | Record fields ->  Record (List.map (fun (f,(s,t1)) -> f,(s,rename_tvar map t1)) fields)
      | Proj(i,s,f,t1) -> Proj(i,s,f,rename_tvar map t1)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,rename_tvar map t1,rename_tvar map t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(rename_tvar map t1, rename_tvar map t2)
      | Constr(s,ts) -> Constr(s, List.map (rename_tvar map) ts)
      | Match(t1,pats) ->
          let aux (pat,cond,t1) = rename_tvar_pat map pat, rename_tvar map cond, rename_tvar map t1 in
            Match(rename_tvar map t1, List.map aux pats)
      | Raise t -> Raise (rename_tvar map t)
      | TryWith(t1,t2) -> TryWith(rename_tvar map t1, rename_tvar map t2)
      | Pair(t1,t2) -> Pair(rename_tvar map t1, rename_tvar map t2)
      | Fst t -> Fst(rename_tvar map t)
      | Snd t -> Snd(rename_tvar map t)
      | Bottom -> Bottom
      | Label(info,t) -> Label(info, rename_tvar map t)
  in
    {desc=desc; typ=typ}



let rec get_tvars typ =
  let (@@@) xs ys = List.fold_left (fun xs y -> if List.memq y xs then xs else y::xs) xs ys in
    match typ with
        TUnit -> []
      | TBool -> []
      | TAbsBool -> assert false
      | TInt -> []
      | TRInt _ -> []
      | TVar({contents=None} as x) -> [x]
      | TVar{contents=Some typ} -> get_tvars typ
      | TFun(x,typ2) -> get_tvars (Id.typ x) @@@ get_tvars typ2
      | TList typ -> get_tvars typ
      | TPair(typ1,typ2) -> get_tvars typ1 @@@ get_tvars typ2
      | TConstr(s,b) -> []
      | TPred(typ,_) -> get_tvars typ


let rec rename_poly_funs_list f map ts =
  let aux t (map,ts) =
    let map',t' = rename_poly_funs f map t in
      map', t'::ts
  in
    List.fold_right aux ts (map,[])

and rename_poly_funs f map t =
  let map',desc =
    match t.desc with
        Unit
      | True
      | False
      | Unknown
      | Int _
      | RandInt _ -> map, t.desc
      | Var x when Id.same x f ->
          if is_poly_typ t.typ
          then raise (Fatal "Not implemented: Trans.rename_poly_funs")
          else
            begin
              try
                let _,x' = List.find (fun (_,f') -> Type.can_unify (Id.typ f') (Id.typ x)) map in
                  map, Var x'
              with Not_found ->
                let x' = Id.new_var_id x in
                  (x,x')::map, Var x'
            end
      | Var x -> map, Var x
      | Fun(x, t) ->
          let map',t' = rename_poly_funs f map t in
            map', Fun(x, t')
      | App({desc=Var x}, ts) when Id.same x f ->
          let x' =
            if is_poly_typ (Id.typ x)
            then
              let xs = take (get_args (Id.typ f)) (List.length ts) in
              let typ = List.fold_right2 (fun t x typ -> TFun(Id.set_typ x t.typ, typ)) ts xs t.typ in
                Id.new_var (Id.name x) typ
            else
              Id.new_var_id x
          in
          let map',ts' = rename_poly_funs_list f map ts in
          let check (_,f') = Type.can_unify (Id.typ f') (Id.typ x') in
            if List.exists check map'
            then
              let _,x'' = List.find check map' in
                map', App(make_var x'', ts')
            else (x,x')::map', App(make_var x', ts')
      | App({desc=Var x}, ts) ->
          let map',ts' = rename_poly_funs_list f map ts in
            map', App(make_var x, ts')
      | App(t, ts) ->
          let map',ts' = rename_poly_funs_list f map ts in
          let map'',t' = rename_poly_funs f map' t in
            map'', App(t',ts')
      | If(t1, t2, t3) ->
          let map1,t1' = rename_poly_funs f map t1 in
          let map2,t2' = rename_poly_funs f map1 t2 in
          let map3,t3' = rename_poly_funs f map2 t3 in
            map3, If(t1', t2', t3')
      | Branch(t1, t2) ->
          let map1,t1' = rename_poly_funs f map t1 in
          let map2,t2' = rename_poly_funs f map1 t2 in
            map2, Branch(t1', t2')
      | Let(flag, bindings, t2) ->
          let aux (g,xs,t) (map,bindings) =
            let map',t' = rename_poly_funs f map t in
              map', (g,xs,t')::bindings
          in
          let map',bindings' = List.fold_right aux bindings (map,[]) in
          let map'',t2' = rename_poly_funs f map' t2 in
            map'', Let(flag, bindings', t2')
      | BinOp(op, t1, t2) ->
          let map1,t1' = rename_poly_funs f map t1 in
          let map2,t2' = rename_poly_funs f map1 t2 in
            map2, BinOp(op, t1', t2')
      | Not t ->
          let map',t' = rename_poly_funs f map t in
            map', Not t'
      | Event(s,b) -> map, Event(s,b)
      | Record fields -> assert false
      | Proj(i,s,f,t) -> assert false
      | SetField(n,i,s,f,t1,t2) -> assert false
      | Nil -> map, Nil
      | Cons(t1,t2) ->
          let map1,t1' = rename_poly_funs f map t1 in
          let map2,t2' = rename_poly_funs f map1 t2 in
            map2, Cons(t1', t2')
      | Constr(s,ts) ->
          let map',ts' = rename_poly_funs_list f map ts in
            map', Constr(s, ts')
      | Match(t,pats) ->
          let aux (p,c,t) (map,bindings) =
            let map',t' = rename_poly_funs f map t in
              map', (p,c,t')::bindings
          in
          let map',pats' = List.fold_right aux pats (map,[]) in
          let map'',t' = rename_poly_funs f map' t in
            map'', Match(t', pats')
      | Raise t ->
          let map',t' = rename_poly_funs f map t in
            map', Raise t'
      | TryWith(t1,t2) ->
          let map1,t1' = rename_poly_funs f map t1 in
          let map2,t2' = rename_poly_funs f map1 t2 in
            map2, TryWith(t1', t2')
      | Bottom -> map, Bottom
      | Pair(t1,t2) ->
          let map1,t1' = rename_poly_funs f map t1 in
          let map2,t2' = rename_poly_funs f map1 t2 in
            map2, Pair(t1', t2')
      | Fst t ->
          let map',t' = rename_poly_funs f map t in
            map', Fst t'
      | Snd t ->
          let map',t' = rename_poly_funs f map t in
            map', Snd t'
      | RandValue (_, _) -> assert false
      | Label _ -> assert false
  in
    map', {desc=desc; typ=t.typ}
let rename_poly_funs f t = rename_poly_funs f [] t

let rec copy_poly_funs t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | RandInt b -> RandInt b
      | Var x -> Var x
      | Fun(x, t) -> Fun(x, copy_poly_funs t)
      | App(t, ts) -> App(copy_poly_funs t, List.map (copy_poly_funs) ts)
      | If(t1, t2, t3) -> If(copy_poly_funs t1, copy_poly_funs t2, copy_poly_funs t3)
      | Branch(t1, t2) -> Branch(copy_poly_funs t1, copy_poly_funs t2)
      | Let(flag, [f, xs, t1], t2) when is_poly_typ (Id.typ f) ->
          let tvars = get_tvars (Id.typ f) in
          let () = assert (tvars > []) in
          let t2' = copy_poly_funs t2 in
          let map,t2'' = rename_poly_funs f t2' in
          let t2''' = inst_tvar_tunit t2'' in
          let n = List.length map in
            if n >= 2
            then
              begin
                Format.printf "COPY: @[";
                List.iter (fun (_,x) -> Format.printf "%a;@ " print_id_typ x) map;
                Format.printf "@.";
              end;
            if map = []
            then (inst_tvar_tunit (make_let_f flag [f, xs, copy_poly_funs t1] t2')).desc
            else
              let aux t (_,f') =
                let tvar_map = List.map (fun v -> v, ref None) tvars in
                let () = Type.unify (rename_tvar_typ tvar_map (Id.typ f)) (Id.typ f') in
                let xs = List.map (rename_tvar_var tvar_map) xs in
                let t1 = rename_tvar tvar_map t1 in
                let xs' = xs in
                let t1 =
                  match flag with
                      Nonrecursive -> t1
                    | Recursive -> subst f (make_var f') t1
                in
                let t1 = copy_poly_funs t1 in
                  make_let_f flag [f', xs', t1] t
              in
                (List.fold_left aux t2''' map).desc
      | Let(flag, defs, t) ->
          if List.for_all (fun (f,_,_) -> not (is_poly_typ (Id.typ f))) defs
          then
            let defs' = List.map (fun (f,xs,t) -> f, xs, copy_poly_funs t) defs in
              Let(flag, defs', copy_poly_funs t)
          else
            raise (Fatal "Not implemented: let [rec] ... and ... with polymorphic type")
      | BinOp(op, t1, t2) -> BinOp(op, copy_poly_funs t1, copy_poly_funs t2)
      | Not t -> Not (copy_poly_funs t)
      | Event(s,b) -> Event(s,b)
      | Record fields -> Record (List.map (fun (f,(s,t)) -> f,(s,copy_poly_funs t)) fields)
      | Proj(i,s,f,t) -> Proj(i,s,f,copy_poly_funs t)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,copy_poly_funs t1,copy_poly_funs t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(copy_poly_funs t1, copy_poly_funs t2)
      | Constr(s,ts) -> Constr(s, List.map (copy_poly_funs) ts)
      | Match(t,pats) -> Match(copy_poly_funs t, List.map (fun (pat,cond,t) -> pat,cond,copy_poly_funs t) pats)
      | Raise t -> Raise (copy_poly_funs t)
      | TryWith(t1,t2) -> TryWith(copy_poly_funs t1, copy_poly_funs t2)
      | Bottom -> Bottom
      | Pair(t1,t2) -> Pair(copy_poly_funs t1, copy_poly_funs t2)
      | Fst t -> Fst (copy_poly_funs t)
      | Snd t -> Snd (copy_poly_funs t)
      | RandValue (_, _) -> assert false
      | Label _ -> assert false
  in
    {desc=desc; typ=t.typ}

let copy_poly_funs t =
  let t' = flatten_tvar (copy_poly_funs t) in
  let () = Type_check.check t' Type.TUnit in
    t'



let rec inst_randvalue env defs typ =
  match typ with
      _ when List.mem_assoc typ env ->
        env, defs, make_app (make_var (List.assoc typ env)) [unit_term]
    | TUnit -> env, defs, unit_term
    | TBool -> env, defs, randbool_unit_term
    | TInt -> env, defs, randint_unit_term
    | TVar({contents=None} as r) -> r := Some typ_abst; inst_randvalue env defs typ_abst
    | TVar{contents=Some typ} -> inst_randvalue env defs typ
    | TFun(x,typ) ->
        let env',defs',t = inst_randvalue env defs typ in
          env', defs', make_fun x t
    | TList (TVar({contents=None} as r)) ->
        r := Some typ_abst; inst_randvalue env defs typ
    | TList typ' ->
        let u = Id.new_var "u" TUnit in
        let f = Id.new_var ("make_" ^ to_id_string typ) (TFun(u,typ)) in
        let env' = (typ,f)::env in
        let env'',defs',t_typ' = inst_randvalue env' defs typ' in
        let t_typ =
          make_if randbool_unit_term (make_nil typ') (make_cons t_typ' (make_app (make_var f) [unit_term]))
        in
          env'', (f,[u],t_typ)::defs', make_app (make_var f) [unit_term]
    | TPair(typ1,typ2) ->
        let env',defs',t1 = inst_randvalue env defs typ1 in
        let env'',defs'',t2 = inst_randvalue env' defs' typ2 in
          env'', defs'', make_pair t1 t2
    | TConstr(s,false) -> env, defs, abst_term
    | TConstr(s,true) ->
        let u = Id.new_var "u" TUnit in
        let f = Id.new_var ("make_" ^ to_id_string typ) (TFun(u,typ)) in
        let env' = (typ,f)::env in
        let env'',defs',t =
          match Type_decl.assoc_typ s with
              Type_decl.TKVariant stypss ->
                let n = List.length stypss in
                let aux1 (s,typs) (env,defs,itss,i) =
                  let aux2 typ (env,defs,ts) =
                    let env', defs',t = inst_randvalue env defs typ in
                      env', defs', t::ts
                  in
                  let env',defs',ts' = List.fold_right aux2 typs (env,defs,[]) in
                    env', defs', (i-1,ts')::itss, i-1
                in
                let env'',defs',itss,_ = List.fold_right aux1 stypss (env',defs,[],n) in
                let aux (s,typs) (i,ts) =
                  let p = if i < n-1 then make_pconst (make_int i) else make_pany TInt in
                    p, true_term, {desc=Constr(s,ts); typ=typ}
                in
                  env'', defs', make_match randint_unit_term (List.map2 aux stypss itss)
            | Type_decl.TKRecord sftyps -> raise (Fatal "Not implemented: inst_randvalue(TKRecord)")
        in
          env'', (f,[u],t)::defs', make_app (make_var f) [unit_term]
    | _ -> Format.printf "inst_randvalue: %a@." print_typ typ; assert false


let rec get_last_definition f t =
  match t.desc with
      Let(_, bindings, t2) ->
        let f,_,_ = last bindings in
          get_last_definition (Some f) t2
    | Fun _ -> assert false
    | _ -> f

let rec replace_main main t =
  match t.desc with
      Let(flag, bindings, t2) -> make_let_f flag bindings (replace_main main t2)
    | Fun _ -> assert false
    | _ -> main

let set_target t =
  match get_last_definition None t with
      None ->
        let u = Id.new_var "main" t.typ in
          "", 0, make_let [u, [], t] unit_term
    | Some f ->
        let xs = get_args (Id.typ f) in
        let main =
          if xs = [] && Id.typ f = TUnit
          then replace_main (make_var f) t
          else
            let rec aux x (env,defs,args) =
              let env',defs',arg = inst_randvalue [] defs (Id.typ x) in
                env',defs', arg::args
            in
            let _,defs,args = List.fold_right aux xs ([],[],[]) in
            let aux arg =
              let x = Id.new_var "arg" arg.typ in
                x, [], arg
            in
            let bindings = List.map aux args in
            let main = make_app (make_var f) (List.map (fun (x,_,_) -> make_var x) bindings) in
            let main = make_lets bindings main in
            let main = make_letrec defs main in
            let u = Id.new_var "main" main.typ in
            let main = make_let [u, [], main] unit_term in
              replace_main main t
        in
          Id.name f, List.length xs, main












(** [let f ... = fun x -> t] や [let f ... = let g x = t in g] を [let f ... x = t] に *)
let rec merge_let_fun t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
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
      | Label _ -> assert false
  in
    {desc=desc; typ=t.typ}



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
    aux rtyp (assoc_typ f t)



let filter_base = List.filter (fun x -> is_base_typ (Id.typ x))

let compare_id x y =
  let aux x = not (is_base_typ (Id.typ x)), Id.to_string x in
    compare (aux x) (aux y)

let rec lift_aux post xs t =
  let defs,desc =
    match t.desc with
        Unit -> [], Unit
      | True -> [], True
      | False -> [], False
      | Unknown -> [], Unknown
      | Int n -> [], Int n
      | RandInt b -> [], RandInt b
      | Var x -> [], Var x
      | Fun _ ->
          let f = Id.new_var ("f" ^ post) t.typ in
          let aux f ys t1 t2 =
            let fv = inter' Id.compare (get_fv t1) xs in
            let fv = if !Flag.lift_fv_only then fv else uniq' Id.compare (filter_base xs @@ fv) in
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
            let fv = inter' Id.compare (get_fv t1) xs in
            let fv = if !Flag.lift_fv_only then fv else uniq' Id.compare (filter_base xs @@ fv) in
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
          let fv = inter' Id.compare (uniq' Id.compare fv) xs in
          let fv = if !Flag.lift_fv_only then fv else uniq' Id.compare (filter_base xs @@ fv) in
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
            let xs' = get_vars_pat pat @@ xs in
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

(** [lift t] で，[t] をlambda-lift する．
    the definitions of let expressions must be side-effect free *)
let lift t =
  lift_aux "" [](*(get_fv2 t)*) t, get_rtyp_lift t



let rec canonize t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
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
      | Label _ -> assert false
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
        | RandInt b -> RandInt b
        | Var x ->
            begin
              try
                let xs, t1 = List.assoc x apply in
                  Let(Nonrecursive, [x, xs, t1], make_var x)
              with Not_found -> Var x
            end
        | Fun(x, t) -> Fun(x, aux apply t)
        | App({desc=Var f}, ts) ->
            if List.mem_assoc f apply
            then
              match ts with
                  [] ->
                    let xs, t1 = List.assoc f apply in
                      Let(Nonrecursive, [f, xs, t1], (make_var f))
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
                    Nonrecursive, None -> Let(flag, [f, xs, aux apply t1], aux apply t2)
                  | Nonrecursive, Some x -> (subst f (make_var x) (aux apply t2)).desc
                  | Recursive, Some x when not (List.mem f (get_fv t1)) ->
                      (subst f {desc=Var x;typ=Id.typ x} (aux apply t2)).desc
                  | Recursive, _ -> Let(flag, [f, xs, aux apply t1], aux apply t2)
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
            let aux' (pat,cond,t) = pat, aux apply cond, aux apply t in
              Match(aux apply t, List.map aux' pats)
        | Snd _ -> assert false
        | Fst _ -> assert false
        | Pair (_, _) -> assert false
        | TryWith (_, _) -> assert false
        | Raise _ -> assert false
        | SetField (_, _, _, _, _, _) -> assert false
        | RandValue (_, _) -> assert false
        | Bottom -> assert false
        | Label _ -> assert false
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


















(** return a term whose let-expressions' bodies are side-effect free *)
let rec trans_let t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
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
      | Let(Nonrecursive, [f, [], t1], t2) ->
          App(make_fun f (trans_let t2), [trans_let t1])
      | Let(Nonrecursive, bindings, t2) when List.exists (fun (_,xs,_) -> xs=[]) bindings -> assert false
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
      | Label _ -> assert false
  in
    {desc=desc; typ=t.typ}




let rec propagate_typ_arg t =
  match t.desc with
      Unit -> unit_term
    | True -> true_term
    | False -> false_term
    | Unknown -> assert false
    | Int n -> make_int n
    | RandInt b -> {desc=RandInt b; typ=t.typ}
    | RandValue(typ,b) -> {desc=RandValue(typ,b); typ=t.typ}
    | Var y -> make_var y
    | Fun(y, t) -> make_fun y (propagate_typ_arg t)
    | App(t1, ts) -> make_app (propagate_typ_arg t1) (List.map propagate_typ_arg ts)
    | If(t1, t2, t3) -> make_if (propagate_typ_arg t1) (propagate_typ_arg t2) (propagate_typ_arg t3)
    | Branch(t1, t2) -> make_branch (propagate_typ_arg t1) (propagate_typ_arg t2)
    | Let(flag, bindings, t2) ->
        let aux (f,xs,t) =
          let xs' =
            let ys = take (get_args (Id.typ f)) (List.length xs) in
            let aux x y ys =
              let ys' = List.map (fun z -> Id.set_typ z (subst_type y (make_var x) (Id.typ z))) ys in
                Id.set_typ x (Id.typ y) :: ys'
            in
              List.fold_right2 aux xs ys []
          in
          let t' = propagate_typ_arg t in
          let t'' = List.fold_left2 (fun t x x' -> subst x (make_var x') t) t' xs xs' in
            f, xs', t''
        in
        let bindings' = List.map aux bindings in
        let t2' = propagate_typ_arg t2 in
          make_let_f flag bindings' t2'
    | BinOp(op, t1, t2) ->
        {desc=BinOp(op, propagate_typ_arg t1, propagate_typ_arg t2); typ=t.typ}
    | Not t1 -> make_not (propagate_typ_arg t1)
    | Event(s,b) -> {desc=Event(s,b); typ=t.typ}
    | Record fields ->
        {desc=Record (List.map (fun (f,(s,t1)) -> f,(s,propagate_typ_arg t1)) fields); typ=t.typ}
    | Proj(i,s,f,t1) ->
        {desc=Proj(i,s,f,propagate_typ_arg t1); typ=t.typ}
    | SetField(n,i,s,f,t1,t2) ->
        {desc=SetField(n,i,s,f,propagate_typ_arg t1,propagate_typ_arg t2); typ=t.typ}
    | Nil -> make_nil2 t.typ
    | Cons(t1,t2) -> make_cons (propagate_typ_arg t1) (propagate_typ_arg t2)
    | Constr(s,ts) -> {desc=Constr(s, List.map propagate_typ_arg ts); typ=t.typ}
    | Match(t1,pats) ->
        let aux (pat,cond,t1) = pat, propagate_typ_arg cond, propagate_typ_arg t1 in
          make_match (propagate_typ_arg t1) (List.map aux pats)
    | Raise t1 -> {desc=Raise (propagate_typ_arg t1); typ=t.typ}
    | TryWith(t1,t2) -> {desc=TryWith(propagate_typ_arg t1, propagate_typ_arg t2); typ=t.typ}
    | Pair(t1,t2) -> make_pair (propagate_typ_arg t1) (propagate_typ_arg t2)
    | Fst t -> make_fst (propagate_typ_arg t)
    | Snd t -> make_snd (propagate_typ_arg t)
    | Bottom -> make_bottom t.typ
    | Label _ -> assert false




let replace_typ_var env x =
  try
    let typ = List.assoc x env in
      Id.set_typ x typ
  with Not_found -> x

let rec replace_typ_aux env t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | RandInt b -> RandInt b
      | RandValue(typ,b) -> RandValue(typ,b)
      | Var y -> Var y
      | Fun(y, t) -> Fun(y, replace_typ_aux env t)
      | App(t1, ts) -> App(replace_typ_aux env t1, List.map (replace_typ_aux env) ts)
      | If(t1, t2, t3) -> If(replace_typ_aux env t1, replace_typ_aux env t2, replace_typ_aux env t3)
      | Branch(t1, t2) -> Branch(replace_typ_aux env t1, replace_typ_aux env t2)
      | Let(flag, bindings, t2) ->
          let aux (f,xs,t) =
            let f' = replace_typ_var env f in
            let () =
              if not (Type.can_unify (Id.typ f) (Id.typ f'))
              then
                let () = Format.printf "Prog: %a@.Spec: %a@." print_id_typ f print_id_typ f' in
                let msg = Format.sprintf "Type of %s in spec. is wrong!" (Id.name f) in
                  raise (Fatal msg)
            in
            let xs' =
              let ys = take (get_args (Id.typ f')) (List.length xs) in
                List.map2 (fun x y -> Id.set_typ x (Id.typ y)) xs ys
            in
            let t' = replace_typ_aux env t in
            let t'' =
              if flag = Nonrecursive
              then t'
              else subst f (make_var f') t'
            in
            let t''' = List.fold_left2 (fun t x x' -> subst x (make_var x') t) t'' xs xs' in
              f', xs', t'''
          in
          let bindings' = List.map aux bindings in
          let t2' = replace_typ_aux env t2 in
          let t2'' = List.fold_left2 (fun t (f,_,_) (f',_,_) -> subst f (make_var f') t) t2' bindings bindings' in
            Let(flag, bindings', t2'')
      | BinOp(op, t1, t2) -> BinOp(op, replace_typ_aux env t1, replace_typ_aux env t2)
      | Not t1 -> Not (replace_typ_aux env t1)
      | Event(s,b) -> Event(s,b)
      | Record fields ->  Record (List.map (fun (f,(s,t1)) -> f,(s,replace_typ_aux env t1)) fields)
      | Proj(i,s,f,t1) -> Proj(i,s,f,replace_typ_aux env t1)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,replace_typ_aux env t1,replace_typ_aux env t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(replace_typ_aux env t1, replace_typ_aux env t2)
      | Constr(s,ts) -> Constr(s, List.map (replace_typ_aux env) ts)
      | Match(t1,pats) ->
          let aux (pat,cond,t1) = pat, replace_typ_aux env cond, replace_typ_aux env t1 in
            Match(replace_typ_aux env t1, List.map aux pats)
      | Raise t -> Raise (replace_typ_aux env t)
      | TryWith(t1,t2) -> TryWith(replace_typ_aux env t1, replace_typ_aux env t2)
      | Pair(t1,t2) -> Pair(replace_typ_aux env t1, replace_typ_aux env t2)
      | Fst t -> Fst(replace_typ_aux env t)
      | Snd t -> Snd(replace_typ_aux env t)
      | Bottom -> Bottom
      | Label _ -> assert false
  in
    {desc=desc; typ=t.typ}

let replace_typ env t =
  let t1 = replace_typ_aux env t in
  let t2 = propagate_typ_arg t1 in
    t2








let rec eval t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | Var x -> Var x
      | App({desc=Fun(x, t)}, t'::ts) ->
          (eval ({desc=App(subst_map [x, t'] t, ts);typ=t.typ})).desc
      | App(t, []) -> (eval t).desc
      | App(t, ts) ->
          App(eval t, List.map eval ts)
      | If({desc=True}, t2, t3) ->
          (eval t2).desc
      | If({desc=False}, t2, t3) ->
          (eval t3).desc
      | If(t1, t2, t3) ->
          If(eval t1, eval t2, eval t3)
      | Branch(t1, t2) ->
          Branch(eval t1, eval t2)
      | Let _ -> assert false
          (*
            | Let(flag, f, xs, t1, t2) -> (*** assume that evaluation of t1 does not fail ***)
            if flag = Flag.Nonrecursive
            then
            if (*safe t1*)true then
            let t1' = List.fold_right (fun x t -> {desc=Fun(x, t);typ=TFun(x,t.typ)}) xs (eval t1) in
            (eval (subst_map [f, t1'] t2)).desc
            else
            Let(flag, f, xs, eval t1, eval t2)
            else
          (*if not (List.mem f (get_fv t1)) then
            let t1' = List.fold_right (fun x t -> Fun(x, t)) xs (eval t1) in
            eval (subst_map [f, t1'] t2)
            else*)
            Let(flag, f, xs, eval t1, eval t2)
          *)
      | BinOp(Add, {desc=Int 0}, t) ->
          (eval t).desc
      | BinOp(Mult, {desc=Int 1}, t) ->
          (eval t).desc
      | BinOp(Sub, t1, t2) ->
          (eval (make_add (eval t1) (eval (make_mul (make_int (-1)) t2)))).desc
      | BinOp(Mult, {desc=Int n}, {desc=BinOp(Mult, {desc=Int m}, t)}) ->
          (eval (make_mul (make_int (n*m)) t)).desc
      | BinOp(op, t1, t2) ->
          BinOp(op, eval t1, eval t2)
      | Not t ->
          Not(eval t)
      | Fun(x,{desc=App(t,ts);typ=typ}) ->
          let t' = eval t in
          let ts' = List.map eval ts in
            if ts' <> [] then
              let l, r = Utilities.list_last_and_rest ts' in
                if l.desc = Var x && List.for_all (fun t -> not (List.mem x (get_fv t))) (t'::r) then
                  (eval {desc=App(t', r);typ=t.typ}).desc
                else
                  Fun(x,{desc=App(t', ts');typ=typ})
            else
              Fun(x,{desc=App(t', ts');typ=typ})
      | Fun(x,t) ->
          Fun(x, eval t)
      | Event(s,b) -> Event(s,b)
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
      | Cons _ -> assert false
      | RandInt _ -> assert false
      | Nil -> assert false
      | Label _ -> assert false
  in
    {desc=desc; typ=t.typ}






(* reduce only terms of the form "(fun x -> t1) t2" *)
(* t is assumed to be a CBN-program *)
let rec eta_reduce t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | RandInt b -> RandInt b
      | Var x -> Var x
      | Fun(x, t) -> Fun(x, eta_reduce t)
      | App(t, []) -> (eta_reduce t).desc
      | App(t1, t2::ts) ->
          begin
            match eta_reduce t1 with
                {desc=Fun(x,t1')} ->
                  (eta_reduce {desc=App(subst x t2 t1', ts); typ=t.typ}).desc
              | t1' ->
                  let ts' = List.map eta_reduce (t2::ts) in
                    (make_app t1' ts').desc
          end
      | If(t1, t2, t3) -> If(eta_reduce t1, eta_reduce t2, eta_reduce t3)
      | Let(flag,bindings,t) ->
          let bindings' = List.map (fun (f,xs,t) -> f, xs, eta_reduce t) bindings in
            Let(flag, bindings', eta_reduce t)
      | BinOp(op, t1, t2) -> BinOp(op, eta_reduce t1, eta_reduce t2)
      | Not t1 -> Not (eta_reduce t1)
      | Event(s,b) -> Event(s,b)
      | Pair(t1,t2) -> Pair(eta_reduce t1, eta_reduce t2)
      | Fst t1 -> Fst (eta_reduce t1)
      | Snd t1 -> Snd (eta_reduce t1)
      | Bottom -> Bottom
      | _ -> Format.printf "%a@." pp_print_term t; assert false
  in
    if false then Format.printf "%a ===> %a@." pp_print_term t pp_print_term {desc=desc; typ=t.typ};
    {desc=desc; typ=t.typ}





let normalize_binop_exp op t1 t2 =
  let neg xs = List.map (fun (x,n) -> x,-n) xs in
  let rec decomp t =
    match t.desc with
        Int n -> [None, n]
      | Var x -> [Some {desc=Var x;typ=Id.typ x}, 1]
      | BinOp(Add, t1, t2) ->
          decomp t1 @@ decomp t2
      | BinOp(Sub, t1, t2) ->
          decomp t1 @@ neg (decomp t2)
      | BinOp(Mult, t1, t2) ->
          let xns1 = decomp t1 in
          let xns2 = decomp t2 in
          let reduce xns = List.fold_left (fun acc (_,n) -> acc+n) 0 xns in
          let aux (x,_) = x <> None in
            begin
              match List.exists aux xns1, List.exists aux xns2 with
                  true, true ->
                    Format.printf "Nonlinear expression not supported: %a@."
                      pp_print_term {desc=BinOp(op,t1,t2);typ=TInt};
                    assert false
                | false, true ->
                    let k = reduce xns1 in
                      List.rev_map (fun (x,n) -> x,n*k) xns2
                | true, false ->
                    let k = reduce xns2 in
                      List.rev_map (fun (x,n) -> x,n*k) xns1
                | false, false ->
                    [None, reduce xns1 + reduce xns2]
            end
      | _ -> assert false
  in
  let xns1 = decomp t1 in
  let xns2 = decomp t2 in
  let compare (x1,_) (x2,_) =
    let aux = function
        None -> "\255"
      | Some {desc=Var x} -> Id.to_string x
      | _ -> assert false
    in
      compare (aux x1) (aux x2)
  in
  let xns = List.sort compare (xns1 @@ (neg xns2)) in
  let rec aux = function
      [] -> []
    | (x,n)::xns ->
        let xns1,xns2 = List.partition (fun (y,_) -> x=y) xns in
        let n' = List.fold_left (fun acc (_,n) -> acc+n) 0 ((x,n)::xns1) in
          (x,n') :: aux xns2
  in
  let xns' = aux xns in
  let xns'' = List.filter (fun (x,n) -> n<>0) xns' in
  let op',t1',t2' =
    match xns'' with
        [] -> assert false
      | (x,n)::xns ->
          let aux :typed_term option * int -> typed_term= function
              None,n -> {desc=Int n; typ=TInt}
            | Some x,n -> if n=1 then x else make_mul (make_int n) x
          in
          let t1,xns',op' =
            if n<0
            then
              let op' =
                match op with
                    Eq -> Eq
                  | Lt -> Gt
                  | Gt -> Lt
                  | Leq -> Geq
                  | Geq -> Leq
                  | _ -> assert false
              in
                aux (x,-n), xns, op'
            else
              aux (x,n), neg xns, op
          in
          let ts = List.map aux xns' in
          let t2 =
            match ts with
                [] -> make_int 0
              | t::ts' -> List.fold_left make_add t ts'
          in
            op', t1, t2
  in
  let rec simplify t =
    let desc =
      match t.desc with
          BinOp(Add, t1, {desc=BinOp(Mult, {desc=Int n}, t2)}) when n < 0 ->
            let t1' = simplify t1 in
              BinOp(Sub, t1', make_mul (make_int (-n)) t2)
        | BinOp(Add, t1, {desc=Int n}) when n < 0 ->
            let t1' = simplify t1 in
              BinOp(Sub, t1', make_int (-n))
        | BinOp(Add, t1, t2) ->
            let t1' = simplify t1 in
              BinOp(Add, t1', t2)
        | t -> t
    in
      {desc=desc; typ=t.typ}
  in
    BinOp(op', t1', simplify t2')

let rec normalize_bool_exp t =
  let desc =
    match t.desc with
        True -> True
      | False -> False
      | Unknown -> Unknown
      | Var x -> Var x
      | BinOp(Or|And as op, t1, t2) ->
          let t1' = normalize_bool_exp t1 in
          let t2' = normalize_bool_exp t2 in
            BinOp(op, t1', t2')
      | BinOp(Eq, {desc=True|False|Unknown}, _)
      | BinOp(Eq, _, {desc=True|False|Unknown})
      | BinOp(Eq, {desc=Nil|Cons _}, _)
      | BinOp(Eq, _, {desc=Nil|Cons _}) as t -> t
      | BinOp(Eq|Lt|Gt|Leq|Geq as op, t1, t2) -> normalize_binop_exp op t1 t2
      | Not t -> Not (normalize_bool_exp t)
      | Unit
      | Int _
      | Fun _
      | App _
      | If _
      | Branch _
      | Let _
      | BinOp((Add|Sub|Mult), _, _)
      | Event _ -> assert false
      | Snd _ -> assert false
      | Fst _ -> assert false
      | Pair (_, _) -> assert false
      | TryWith (_, _) -> assert false
      | Raise _ -> assert false
      | Match (_, _) -> assert false
      | Constr (_, _) -> assert false
      | Cons (_, _) -> assert false
      | SetField (_, _, _, _, _, _) -> assert false
      | Proj (_, _, _, _) -> assert false
      | Record _ -> assert false
      | RandValue (_, _) -> assert false
      | RandInt _ -> assert false
      | Bottom -> assert false
      | Nil -> assert false
      | Label _ -> assert false
  in
    {desc=desc; typ=t.typ}



let rec get_and_list t =
  match t.desc with
      True -> [{desc=True; typ=t.typ}]
    | False -> [{desc=False; typ=t.typ}]
    | Unknown -> [{desc=Unknown; typ=t.typ}]
    | Var x -> [{desc=Var x; typ=t.typ}]
    | BinOp(And, t1, t2) -> get_and_list t1 @@ get_and_list t2
    | BinOp(op, t1, t2) -> [{desc=BinOp(op, t1, t2); typ=t.typ}]
    | Not t -> [{desc=Not t; typ=t.typ}]
    | Unit
    | Int _
    | Fun _
    | App _
    | If _
    | Branch _
    | Let _
    | Event _ -> assert false
    | Snd _ -> assert false
    | Fst _ -> assert false
    | Pair (_, _) -> assert false
    | TryWith (_, _) -> assert false
    | Raise _ -> assert false
    | Match (_, _) -> assert false
    | Constr (_, _) -> assert false
    | Cons (_, _) -> assert false
    | SetField (_, _, _, _, _, _) -> assert false
    | Proj (_, _, _, _) -> assert false
    | Record _ -> assert false
    | RandValue (_, _) -> assert false
    | RandInt _ -> assert false
    | Bottom -> assert false
    | Nil -> assert false
      | Label _ -> assert false

let rec merge_geq_leq t =
  let desc =
    match t.desc with
        True -> True
      | False -> False
      | Unknown -> Unknown
      | Var x -> Var x
      | BinOp(And, t1, t2) ->
          let ts = get_and_list t in
          let is_dual t1 t2 = match t1.desc,t2.desc with
              BinOp(op1,t11,t12), BinOp(op2,t21,t22) when t11=t21 && t12=t22 -> op1=Leq && op2=Geq || op1=Geq && op2=Leq
            | _ -> false
          in
          let get_eq t =
            match t.desc with
                BinOp((Leq|Geq),t1,t2) -> {desc=BinOp(Eq,t1,t2); typ=t.typ}
              | _ -> assert false
          in
          let rec aux = function
              [] -> []
            | t::ts ->
                if List.exists (is_dual t) ts
                then
                  let t' = get_eq t in
                  let ts' = List.filter (fun t' -> not (is_dual t t')) ts in
                    t' :: aux ts'
                else
                  t :: aux ts
          in
          let ts' = aux ts in
          let t =
            match ts' with
                [] -> assert false
              | [t] -> t
              | t::ts -> List.fold_left (fun t1 t2 -> {desc=BinOp(And,t1,t2);typ=TBool}) t ts
          in
            t.desc
      | BinOp(Or, t1, t2) ->
          let t1' = merge_geq_leq t1 in
          let t2' = merge_geq_leq t2 in
            BinOp(Or, t1', t2')
      | BinOp(Eq|Lt|Gt|Leq|Geq as op, t1, t2) -> BinOp(op, t1, t2)
      | Not t -> Not (merge_geq_leq t)
      | Unit
      | Int _
      | Fun _
      | App _
      | If _
      | Branch _
      | Let _
      | BinOp((Add|Sub|Mult), _, _)
      | Event _ -> Format.printf "%a@." pp_print_term t; assert false
      | Snd _ -> assert false
      | Fst _ -> assert false
      | Pair (_, _) -> assert false
      | TryWith (_, _) -> assert false
      | Raise _ -> assert false
      | Match (_, _) -> assert false
      | Constr (_, _) -> assert false
      | Cons (_, _) -> assert false
      | SetField (_, _, _, _, _, _) -> assert false
      | Proj (_, _, _, _) -> assert false
      | Record _ -> assert false
      | RandValue (_, _) -> assert false
      | RandInt _ -> assert false
      | Bottom -> assert false
      | Nil -> assert false
      | Label _ -> assert false
  in
    {desc=desc; typ=t.typ}











let rec elim_fun fun_name t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | RandInt b -> RandInt b
      | RandValue(typ,b) -> RandValue(typ,b)
      | Var y -> Var y
      | Fun(y, t1) ->
          let f = Id.new_var fun_name t.typ in
            Let(Nonrecursive, [f, [y], elim_fun fun_name t1], make_var f)
      | App(t1, ts) -> App(elim_fun fun_name t1, List.map (elim_fun fun_name) ts)
      | If(t1, t2, t3) -> If(elim_fun fun_name t1, elim_fun fun_name t2, elim_fun fun_name t3)
      | Branch(t1, t2) -> Branch(elim_fun fun_name t1, elim_fun fun_name t2)
      | Let(flag, bindings, t2) ->
          let aux (f,xs,t) =
            let fun_name' = "f_" ^ Id.name f in
              f, xs, elim_fun fun_name' t
          in
          let bindings' = List.map aux bindings in
          let t2' = elim_fun fun_name t2 in
            Let(flag, bindings', t2')
      | BinOp(op, t1, t2) -> BinOp(op, elim_fun fun_name t1, elim_fun fun_name t2)
      | Not t1 -> Not (elim_fun fun_name t1)
      | Event(s,b) -> Event(s,b)
      | Record fields ->  Record (List.map (fun (f,(s,t1)) -> f,(s,elim_fun fun_name t1)) fields)
      | Proj(i,s,f,t1) -> Proj(i,s,f,elim_fun fun_name t1)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,elim_fun fun_name t1,elim_fun fun_name t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(elim_fun fun_name t1, elim_fun fun_name t2)
      | Constr(s,ts) -> Constr(s, List.map (elim_fun fun_name) ts)
      | Match(t1,pats) ->
          let aux (pat,cond,t1) = pat, elim_fun fun_name cond, elim_fun fun_name t1 in
            Match(elim_fun fun_name t1, List.map aux pats)
      | Raise t -> Raise (elim_fun fun_name t)
      | TryWith(t1,t2) -> TryWith(elim_fun fun_name t1, elim_fun fun_name t2)
      | Pair(t1,t2) -> Pair(elim_fun fun_name t1, elim_fun fun_name t2)
      | Fst t -> Fst(elim_fun fun_name t)
      | Snd t -> Snd(elim_fun fun_name t)
      | Bottom -> Bottom
      | Label _ -> assert false
  in
    {desc=desc; typ=t.typ}

let elim_fun t = elim_fun "f" t





let rec make_ext_env t =
  match t.desc with
      Unit -> []
    | True -> []
    | False -> []
    | Unknown -> []
    | Int n -> []
    | RandInt _ -> []
    | Var x -> if is_external x then [x, Id.typ x] else []
    | App(t, ts) -> make_ext_env t @@ (rev_map_flatten (make_ext_env) ts)
    | If(t1, t2, t3) -> make_ext_env t1 @@ make_ext_env t2 @@ make_ext_env t3
    | Branch(t1, t2) -> make_ext_env t1 @@ make_ext_env t2
    | Let(flag, bindings, t2) ->
        let aux fv (_,xs,t) = make_ext_env t @@ fv in
          List.fold_left aux (make_ext_env t2) bindings
    | BinOp(op, t1, t2) -> make_ext_env t1 @@ make_ext_env t2
    | Not t -> make_ext_env t
    | Fun(x,t) -> make_ext_env t
    | Event(s,_) -> []
    | Record fields -> List.fold_left (fun acc (_,(_,t)) -> make_ext_env t @@ acc) [] fields
    | Proj(_,_,_,t) -> make_ext_env t
    | SetField(_,_,_,_,t1,t2) -> make_ext_env t1 @@ make_ext_env t2
    | Nil -> []
    | Cons(t1, t2) -> make_ext_env t1 @@ make_ext_env t2
    | Constr(_,ts) -> List.fold_left (fun acc t -> make_ext_env t @@ acc) [] ts
    | Match(t,pats) ->
        let aux acc (_,_,t) = make_ext_env t @@ acc in
          List.fold_left aux (make_ext_env t) pats
    | TryWith(t1,t2) -> make_ext_env t1 @@ make_ext_env t2
    | Bottom -> []
    | Pair(t1,t2) -> make_ext_env t1 @@ make_ext_env t2
    | Fst t -> make_ext_env t
    | Snd t -> make_ext_env t
    | Raise t -> make_ext_env t
    | RandValue _ -> assert false
    | Label _ -> assert false



let rec init_rand_int t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | Var x -> Var x
      | RandInt false -> assert false
      | App({desc=RandInt false},[{desc=Unit}]) -> Var (Id.new_var "_r" TInt)
      | Fun(x,t) -> Fun(x, init_rand_int t)
      | App(t,ts) -> App(init_rand_int t, List.map init_rand_int ts)
      | If(t1,t2,t3) -> If(init_rand_int t1, init_rand_int t2, init_rand_int t3)
      | Branch(t1,t2) -> Branch(init_rand_int t1, init_rand_int t2)
      | Let(flag,bindings,t2) ->
          let bindings' = List.map (fun (f,xs,t) -> f,xs,init_rand_int t) bindings in
            Let(flag, bindings', init_rand_int t2)
      | BinOp(op, t1, t2) -> BinOp(op, init_rand_int t1, init_rand_int t2)
      | Not t -> Not (init_rand_int t)
      | Event(s,b) -> Event(s,b)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(init_rand_int t1, init_rand_int t2)
      | Constr(s,ts) -> Constr(s, List.map init_rand_int ts)
      | Match(t,pats) ->
          Match(init_rand_int t, List.map (fun (pat,cond,t) -> pat, init_rand_int cond,init_rand_int t) pats)
      | Bottom -> Bottom
      | Snd _ -> assert false
      | Fst _ -> assert false
      | Pair (_, _) -> assert false
      | TryWith (_, _) -> assert false
      | Raise _ -> assert false
      | SetField (_, _, _, _, _, _) -> assert false
      | Proj (_, _, _, _) -> assert false
      | Record _ -> assert false
      | RandValue (_, _) -> assert false
      | RandInt _ -> assert false
      | Label _ -> assert false
  in
    {desc=desc; typ=t.typ}



let rec inlined_f inlined fs t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | RandInt b -> RandInt b
      | RandValue(typ,b) -> RandValue(typ,b)
      | Var y ->
          if List.exists (fun (x, _, _) -> Id.same x y) fs then
            let (f, xs, t') = try List.find (fun (x, _, _) -> Id.same x y) fs with Not_found -> assert false in
              (*let _ = List.iter (fun (x, t) -> Format.printf "%a -> %a@." print_id x pp_print_term t) [f, t'] in*)
            let f, _ =
              List.fold_left
                (fun (f, ty) y ->
                   (fun t ->
                      f {desc=Fun(y, t); typ=ty}),
                   match ty with
                       Type.TFun(_, ty') -> ty'
                     | _ ->
                         let _ = Format.printf "%a@." print_typ ty in assert false)
                ((fun t -> t), t.typ)
                xs
            in
            let t' = inlined_f inlined fs t' in
              (f t').desc
          else
            Var y
      | Fun(y, t1) -> Fun(y, inlined_f inlined fs t1)
      | App(t1, ts) ->
          (*let _ = Format.printf "func: %a@." pp_print_term t1' in*)
          (match t1.desc with
               Var f when List.exists (fun (f', _, _) -> Id.same f f') fs ->
                 let (f, xs, t) = try List.find (fun (f', _, _) -> Id.same f f') fs with Not_found -> assert false in
                 let ts = List.map (inlined_f inlined fs) ts in
                 let ys = List.map (fun t -> match t.desc with Unit | True | False | Int _ | Var _ -> `L(t) | _ -> `R(Id.new_var "arg" t.typ)) ts in
                 let ys1, ys2 = if List.length ys <= List.length xs then ys, [] else ExtList.List.split_nth (List.length xs) ys in
                 let xs1, xs2 = ExtList.List.split_nth (List.length ys1) xs in
                 let map = List.map2 (fun x y -> match y with `L(t) -> x, t | `R(y) -> x, make_var y) xs1 ys1 in
                 let t' = subst_map map t in
                 let f, _ =
                   List.fold_left
                     (fun (f, ty) x -> (fun t -> f {desc=Fun(x, t); typ=ty}), match ty with Type.TFun(_, ty') -> ty' | _ -> assert false)
                     ((fun t -> t), Type.app_typ t1.typ (List.map (fun t -> t.typ) ts))
                     xs2
                 in
                 let bindings = Util.filter_map2 (fun y t -> match y with `L(_) -> None | `R(y) -> Some(y, [], t)) ys ts in
                   (make_lets bindings (make_app (f t') (List.map (fun y -> match y with `L(t) -> t | `R(y) -> make_var y) ys2))).desc
             | _ ->
                 let t1' = inlined_f inlined fs t1 in
                 let ts' = List.map (inlined_f inlined fs) ts in
                   App(t1', ts'))
      | If(t1, t2, t3) -> If(inlined_f inlined fs t1, inlined_f inlined fs t2, inlined_f inlined fs t3)
      | Branch(t1, t2) -> Branch(inlined_f inlined fs t1, inlined_f inlined fs t2)
      | Let(flag, bindings, t2) ->
          let aux (f,xs,t) =
            (*let _ = List.iter (fun f -> Format.printf "f: %a@." print_id f) inlined in*)
            let rec lift t =
              match t.desc with
                  Fun(x, t') ->
                    let xs, t' = lift t' in
                      x::xs, t'
                | _ -> [], t
            in
              if flag = Nonrecursive then
                if List.exists (fun f' -> Id.same f' f) inlined then
                  let t' = inlined_f inlined fs t in
                  let xs', t' = lift t' in
                    (*let _ = Format.printf "inlined: %a, %a, %a@." print_id f (Util.pr_list print_id ",") xs pp_print_term t' in*)
                    `R(f, xs @ xs', t')
                else if xs = [] && (match t.desc with Fst(t) | Snd(t) -> (match t.desc with Var _ -> true | _ -> false) | _ -> false) then
                  (*let _ = Format.printf "fst/snd: %a@." print_id f in*)
                  `R(f, xs, t)
                else
                  let t' = inlined_f inlined fs t in
                  let xs', t' = lift t' in
                    `L(f, xs @ xs', t')
              else
                `L(f, xs, inlined_f inlined fs t)
          in
          let bindings', fs' = Util.partition_map aux bindings in
          let t2' = inlined_f inlined (fs @ fs') t2 in
            if bindings' = [] then
              t2'.desc
            else
              Let(flag, bindings', t2')
      | BinOp(op, t1, t2) -> BinOp(op, inlined_f inlined fs t1, inlined_f inlined fs t2)
      | Not t1 -> Not (inlined_f inlined fs t1)
      | Event(s,b) -> Event(s,b)
      | Record fields ->  Record (List.map (fun (f,(s,t1)) -> f,(s,inlined_f inlined fs t1)) fields)
      | Proj(i,s,f,t1) -> Proj(i,s,f,inlined_f inlined fs t1)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,inlined_f inlined fs t1,inlined_f inlined fs t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(inlined_f inlined fs t1, inlined_f inlined fs t2)
      | Constr(s,ts) -> Constr(s, List.map (inlined_f inlined fs) ts)
      | Match(t1,pats) ->
          let aux (pat,cond,t1) = pat, inlined_f inlined fs cond, inlined_f inlined fs t1 in
            Match(inlined_f inlined fs t1, List.map aux pats)
      | Raise t -> Raise (inlined_f inlined fs t)
      | TryWith(t1,t2) -> TryWith(inlined_f inlined fs t1, inlined_f inlined fs t2)
      | Pair(t1,t2) -> Pair(inlined_f inlined fs t1, inlined_f inlined fs t2)
      | Fst t ->
          let t' = inlined_f inlined fs t in
            begin
              match t'.desc with
                  Pair(t1, _) -> t1.desc
                | _ -> Fst t'
            end
      | Snd t ->
          let t' = inlined_f inlined fs t in
            begin
              match t'.desc with
                  Pair(_, t2) -> t2.desc
                | _ -> Snd t'
            end
      | Bottom -> Bottom
      | Label _ -> assert false
          (*
            | _ -> Format.printf "inlined_f: %a@." pp_print_term t; assert false
          *)
  in
    {desc=desc; typ=t.typ}

let inlined_f inlined t = inlined_f inlined [] t

let rec lift_fst_snd fs t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | RandInt b -> RandInt b
      | RandValue(typ,b) -> RandValue(typ,b)
      | Var y -> Var y
      | Fun(y, t1) -> Fun(y, lift_fst_snd fs t1)(* ommit the case where y is a pair *)
      | App(t1, ts) -> App(lift_fst_snd fs t1, List.map (lift_fst_snd fs) ts)
      | If(t1, t2, t3) -> If(lift_fst_snd fs t1, lift_fst_snd fs t2, lift_fst_snd fs t3)
      | Branch(t1, t2) -> Branch(lift_fst_snd fs t1, lift_fst_snd fs t2)
      | Let(flag, bindings, t2) ->
          let bindings' =
                      List.map
                          (fun (f,xs,t) ->
                              f, xs,
                                let fs' =
                                  List.flatten
                                      (ExtList.List.filter_map
                                          (fun x ->
                                              match x.Id.typ with
                                                  TPair(_, _) ->
                                                      Some([Id.new_var x.Id.name (fst_typ x.Id.typ), true, x; Id.new_var x.Id.name (snd_typ x.Id.typ), false, x])
                                                | _ -> None)
                                          xs)
                                in
                                if fs' = [] then
                                  lift_fst_snd fs t
                                else
                                  make_lets
                                        (List.map
                                            (fun (x, bfst, xorig) ->
                                                (* ommit the case where x is a pair *)
                                                x, [], if bfst then { desc = Fst(make_var xorig); typ = x.Id.typ} else { desc = Snd(make_var xorig); typ = x.Id.typ})
                                            fs')
                                        (lift_fst_snd (fs @ fs') t)
                                (* ommit the case where f is a pair *))
                            bindings
                    in
          Let(flag, bindings', lift_fst_snd fs t2)
      | BinOp(op, t1, t2) -> BinOp(op, lift_fst_snd fs t1, lift_fst_snd fs t2)
      | Not t1 -> Not (lift_fst_snd fs t1)
      | Event(s,b) -> Event(s,b)
      | Record fields ->  Record (List.map (fun (f,(s,t1)) -> f,(s,lift_fst_snd fs t1)) fields)
      | Proj(i,s,f,t1) -> Proj(i,s,f,lift_fst_snd fs t1)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,lift_fst_snd fs t1,lift_fst_snd fs t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(lift_fst_snd fs t1, lift_fst_snd fs t2)
      | Constr(s,ts) -> Constr(s, List.map (lift_fst_snd fs) ts)
      | Match(t1,pats) ->
          let aux (pat,cond,t1) = pat, lift_fst_snd fs cond, lift_fst_snd fs t1 in
            Match(lift_fst_snd fs t1, List.map aux pats)
      | Raise t -> Raise (lift_fst_snd fs t)
      | TryWith(t1,t2) -> TryWith(lift_fst_snd fs t1, lift_fst_snd fs t2)
      | Pair(t1,t2) -> Pair(lift_fst_snd fs t1, lift_fst_snd fs t2)
      | Fst t ->
                (match t.desc with
                      Var(x) ->
                          (try
                            let (x, _, _) = List.find (fun (_, bfst, x') -> bfst && Id.same x' x) fs in
                                (make_var x).desc
                            with Not_found ->
                              Fst(lift_fst_snd fs t))
                    | _ ->
                    Fst(lift_fst_snd fs t))
      | Snd t ->
                (match t.desc with
                      Var(x) ->
                          (try
                            let (x, _, _) = List.find (fun (_, bfst, x') -> not bfst && Id.same x' x) fs in
                                (make_var x).desc
                            with Not_found ->
                              Snd(lift_fst_snd fs t))
                    | _ ->
                    Snd(lift_fst_snd fs t))
      | Bottom -> Bottom
      | Label _ -> assert false
(*
      | _ -> Format.printf "lift_fst_snd: %a@." pp_print_term t; assert false
*)
  in
    {desc=desc; typ=t.typ}

let lift_fst_snd t = lift_fst_snd [] t


(* t is assumed to be a CBN-program *)
let rec expand_let_val t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | RandInt b -> RandInt b
      | RandValue(typ,b) -> RandValue(typ,b)
      | Var y -> Var y
      | Fun(y, t) -> Fun(y, expand_let_val t)
      | App(t1, ts) -> App(expand_let_val t1, List.map expand_let_val ts)
      | If(t1, t2, t3) -> If(expand_let_val t1, expand_let_val t2, expand_let_val t3)
      | Branch(t1, t2) -> Branch(expand_let_val t1, expand_let_val t2)
      | Let(Recursive, bindings, t2) ->
          let bindings' = List.map (fun (f,xs,t) -> f, xs, expand_let_val t) bindings in
          let t2' = expand_let_val t2 in
            Let(Recursive, bindings', t2')
      | Let(flag, bindings, t2) ->
          let bindings' = List.map (fun (f,xs,t) -> f, xs, expand_let_val t) bindings in
          let t2' = expand_let_val t2 in
          let bindings1,bindings2 = List.partition (fun (_,xs,_) -> xs = []) bindings' in
          let t2'' = List.fold_left (fun t (f,_,t') -> subst f t' t) t2' bindings1 in
            if bindings2 = []
            then t2''.desc
            else Let(flag, bindings2, t2'')
      | BinOp(op, t1, t2) -> BinOp(op, expand_let_val t1, expand_let_val t2)
      | Not t1 -> Not (expand_let_val t1)
      | Event(s,b) -> Event(s,b)
      | Record fields ->  Record (List.map (fun (f,(s,t1)) -> f,(s,expand_let_val t1)) fields)
      | Proj(i,s,f,t1) -> Proj(i,s,f,expand_let_val t1)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,expand_let_val t1,expand_let_val t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(expand_let_val t1, expand_let_val t2)
      | Constr(s,ts) -> Constr(s, List.map expand_let_val ts)
      | Match(t1,pats) ->
          let aux (pat,cond,t1) = pat, expand_let_val cond, expand_let_val t1 in
            Match(expand_let_val t1, List.map aux pats)
      | Raise t -> Raise (expand_let_val t)
      | TryWith(t1,t2) -> TryWith(expand_let_val t1, expand_let_val t2)
      | Pair(t1,t2) -> Pair(expand_let_val t1, expand_let_val t2)
      | Fst t -> Fst(expand_let_val t)
      | Snd t -> Snd(expand_let_val t)
      | Bottom -> Bottom
      | Label _ -> assert false
  in
    {desc=desc; typ=t.typ}


let rec simplify_match t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | RandInt b -> RandInt b
      | RandValue(typ,b) -> RandValue(typ,b)
      | Var y -> Var y
      | Fun(y, t) -> Fun(y, simplify_match t)
      | App(t1, ts) -> App(simplify_match t1, List.map simplify_match ts)
      | If(t1, t2, t3) -> If(simplify_match t1, simplify_match t2, simplify_match t3)
      | Branch(t1, t2) -> Branch(simplify_match t1, simplify_match t2)
      | Let(flag, bindings, t2) ->
          let bindings' = List.map (fun (f,xs,t) -> f, xs, simplify_match t) bindings in
          let t2' = simplify_match t2 in
            Let(flag, bindings', t2')
      | BinOp(op, t1, t2) -> BinOp(op, simplify_match t1, simplify_match t2)
      | Not t1 -> Not (simplify_match t1)
      | Event(s,b) -> Event(s,b)
      | Record fields ->  Record (List.map (fun (f,(s,t1)) -> f,(s,simplify_match t1)) fields)
      | Proj(i,s,f,t1) -> Proj(i,s,f,simplify_match t1)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,simplify_match t1,simplify_match t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(simplify_match t1, simplify_match t2)
      | Constr(s,ts) -> Constr(s, List.map simplify_match ts)
      | Match(t1,pats) ->
          let aux (pat,cond,t1) = pat, simplify_match cond, simplify_match t1 in
          let pats' = List.map aux pats in
          let rec elim_unused = function
              [] -> []
            | ({pat_desc=PAny|PVar _} as pat, cond, t)::_ when cond = true_term -> [pat, cond, t]
            | pct::pats -> pct :: elim_unused pats
          in
          let pats'' = elim_unused pats' in
            begin
              match pats'' with
                  [] -> assert false
                | [{pat_desc=PAny}, cond, t] when cond = true_term ->
                    let x = Id.new_var "u" t1.typ in
                    assert (cond = true_term);
                    (make_let [x, [], t1] t).desc
                | [{pat_desc=PVar x}, cond, t] when cond = true_term ->
                    (make_let [x, [], t1] t).desc
                | _ -> Match(simplify_match t1, pats'')
            end
      | Raise t -> Raise (simplify_match t)
      | TryWith(t1,t2) -> TryWith(simplify_match t1, simplify_match t2)
      | Pair(t1,t2) -> Pair(simplify_match t1, simplify_match t2)
      | Fst t -> Fst(simplify_match t)
      | Snd t -> Snd(simplify_match t)
      | Bottom -> Bottom
      | Label _ -> assert false
  in
    {desc=desc; typ=t.typ}


let rec simplify_match t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | RandInt b -> RandInt b
      | RandValue(typ,b) -> RandValue(typ,b)
      | Var y -> Var y
      | Fun(y, t) -> Fun(y, simplify_match t)
      | App(t1, ts) -> App(simplify_match t1, List.map simplify_match ts)
      | If(t1, t2, t3) -> If(simplify_match t1, simplify_match t2, simplify_match t3)
      | Branch(t1, t2) -> Branch(simplify_match t1, simplify_match t2)
      | Let(flag, bindings, t2) ->
          let bindings' = List.map (fun (f,xs,t) -> f, xs, simplify_match t) bindings in
          let t2' = simplify_match t2 in
            Let(flag, bindings', t2')
      | BinOp(op, t1, t2) -> BinOp(op, simplify_match t1, simplify_match t2)
      | Not t1 -> Not (simplify_match t1)
      | Event(s,b) -> Event(s,b)
      | Record fields ->  Record (List.map (fun (f,(s,t1)) -> f,(s,simplify_match t1)) fields)
      | Proj(i,s,f,t1) -> Proj(i,s,f,simplify_match t1)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,simplify_match t1,simplify_match t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(simplify_match t1, simplify_match t2)
      | Constr(s,ts) -> Constr(s, List.map simplify_match ts)
      | Match(t1,pats) ->
          let aux (pat,cond,t1) = pat, simplify_match cond, simplify_match t1 in
          let pats' = List.map aux pats in
          let rec elim_unused = function
              [] -> []
            | ({pat_desc=PAny|PVar _} as pat, cond, t)::_ when cond = true_term -> [pat, cond, t]
            | pct::pats -> pct :: elim_unused pats
          in
          let pats'' = elim_unused pats' in
            begin
              match pats'' with
                  [] -> assert false
                | [{pat_desc=PAny}, cond, t] when cond = true_term ->
                    let x = Id.new_var "u" t1.typ in
                    assert (cond = true_term);
                    (make_let [x, [], t1] t).desc
                | [{pat_desc=PVar x}, cond, t] when cond = true_term ->
                    (make_let [x, [], t1] t).desc
                | _ -> Match(simplify_match t1, pats'')
            end
      | Raise t -> Raise (simplify_match t)
      | TryWith(t1,t2) -> TryWith(simplify_match t1, simplify_match t2)
      | Pair(t1,t2) -> Pair(simplify_match t1, simplify_match t2)
      | Fst t -> Fst(simplify_match t)
      | Snd t -> Snd(simplify_match t)
      | Bottom -> Bottom
      | Label _ -> assert false
  in
    {desc=desc; typ=t.typ}


let should_insert typs = List.for_all (function TFun _ -> true | _ -> false) typs

let rec insert_param_funarg_typ = function
    TUnit -> TUnit
  | TBool -> TBool
  | TAbsBool -> TAbsBool
  | TInt -> TInt
  | TRInt p -> TRInt p
  | TVar _ -> assert false
  | TFun _ as typ ->
      let xs,typ' = decomp_tfun typ in
      let xs' = List.map insert_param_funarg_var xs in
      let xs'' =
        if should_insert (List.map Id.typ xs)
        then (Id.new_var "u" TUnit) :: xs'
        else xs'
      in
        List.fold_right (fun x typ -> TFun(x,typ)) xs'' (insert_param_funarg_typ typ')
  | TList typ -> TList(insert_param_funarg_typ typ)
  | TPair(typ1,typ2) -> TPair(insert_param_funarg_typ typ1, insert_param_funarg_typ typ2)
  | TConstr(s,b) -> TConstr(s,b)
  | TPred(typ,ps) -> TPred(insert_param_funarg_typ typ, ps)

and insert_param_funarg_var x = Id.set_typ x (insert_param_funarg_typ (Id.typ x))

(* Insert extra parameters into functions with only function arguments.
   Input must be CPS *)
and insert_param_funarg t =
  let typ = insert_param_funarg_typ t.typ in
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | RandInt b -> RandInt b
      | Var x -> Var (insert_param_funarg_var x)
      | Fun _ ->
          let xs,t' = decomp_fun t in
          let xs' = List.map insert_param_funarg_var xs in
          let xs'' =
            if should_insert (List.map Id.typ xs)
            then (Id.new_var "u" TUnit) :: xs'
            else xs'
          in
            (List.fold_right make_fun xs'' (insert_param_funarg t')).desc
      | App(t1, ts) ->
          let ts' = List.map (insert_param_funarg) ts in
          let ts'' =
            if should_insert (get_argtyps t1.typ)
            then unit_term :: ts'
            else ts'
          in
            App(insert_param_funarg t1, ts'')
      | If(t1, t2, t3) -> If(insert_param_funarg t1, insert_param_funarg t2, insert_param_funarg t3)
      | Branch(t1, t2) -> Branch(insert_param_funarg t1, insert_param_funarg t2)
      | Let(flag, defs, t) ->
          let aux (f,xs,t) =
            let xs' = List.map insert_param_funarg_var xs in
            let xs'' =
              if should_insert (List.map Id.typ xs)
              then Id.new_var "u" TUnit :: xs'
              else xs'
            in
              insert_param_funarg_var f, xs'', insert_param_funarg t
          in
            Let(flag, List.map aux defs, insert_param_funarg t)
      | BinOp(op, t1, t2) -> BinOp(op, insert_param_funarg t1, insert_param_funarg t2)
      | Not t -> Not (insert_param_funarg t)
      | Event(s,b) -> Event(s,b)
      | Record fields -> Record (List.map (fun (f,(s,t)) -> f,(s,insert_param_funarg t)) fields)
      | Proj(i,s,f,t) -> Proj(i,s,f,insert_param_funarg t)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,insert_param_funarg t1,insert_param_funarg t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(insert_param_funarg t1, insert_param_funarg t2)
      | Constr(s,ts) -> Constr(s, List.map (insert_param_funarg) ts)
      | Match(t,pats) -> Match(insert_param_funarg t, List.map (fun (pat,cond,t) -> pat,cond,insert_param_funarg t) pats)
      | Raise t -> Raise (insert_param_funarg t)
      | TryWith(t1,t2) -> TryWith(insert_param_funarg t1, insert_param_funarg t2)
      | Bottom -> Bottom
      | Pair(t1,t2) -> Pair(insert_param_funarg t1, insert_param_funarg t2)
      | Fst t -> Fst (insert_param_funarg t)
      | Snd t -> Snd (insert_param_funarg t)
      | RandValue (_, _) -> assert false
      | Label _ -> assert false
  in
    {desc=desc; typ=typ}


let rec search_fail path t =
  match t.desc with
    Unit -> []
  | True -> []
  | False -> []
  | Unknown -> []
  | Int n -> []
  | RandInt b -> []
  | Var x -> []
  | Fun(x,t) -> search_fail path t
  | App(t1, ts) ->
    let rec aux acc i ts =
      match ts with
        [] -> acc
      | t::ts' -> aux (search_fail (i::path) t @ acc) (i+1) ts'
    in
    aux [] 0 (t1::ts)
  | If(t1, t2, t3) -> search_fail (1::path) t1 @ search_fail (2::path) t2 @ search_fail (3::path) t3
  | Branch(t1, t2) -> search_fail (1::path) t1 @ search_fail (2::path) t2
  | Let(_, defs, t) ->
    let rec aux acc i ts =
      match ts with
        [] -> acc
      | t::ts' -> aux (search_fail (i::path) t @ acc) (i+1) ts'
    in
    let ts = List.map (fun (_,_,t) -> t) defs in
    aux [] 0 (ts@[t])
  | BinOp(_, t1, t2) -> search_fail (1::path) t1 @ search_fail (2::path) t2
  | Not t -> search_fail path t
  | Event("fail",_) -> [path]
  | Event(s,b) -> []
  | Record fields -> assert false
  | Proj(i,s,f,t) -> assert false
  | SetField(n,i,s,f,t1,t2) -> assert false
  | Nil -> []
  | Cons(t1,t2) -> search_fail (1::path) t1 @ search_fail (2::path) t2
  | Constr(_,ts) ->
    let rec aux acc i ts =
      match ts with
        [] -> acc
      | t::ts' -> aux (search_fail (i::path) t @ acc) (i+1) ts'
    in
    aux [] 0 ts
  | Match(t,pats) ->
    let rec aux acc i ts =
      match ts with
        [] -> acc
      | t::ts' -> aux (search_fail (i::path) t @ acc) (i+1) ts'
    in
    let ts = rev_flatten_map (fun (_,cond,t) -> [t;cond]) pats in
    aux [] 0 (t::ts)
  | Raise t -> search_fail path t
  | TryWith(t1,t2) -> search_fail (1::path) t1 @ search_fail (2::path) t2
  | Bottom -> []
  | Pair(t1,t2) -> search_fail (1::path) t1 @ search_fail (2::path) t2
  | Fst t -> search_fail path t
  | Snd t -> search_fail path t
  | RandValue _ -> []
  | Label(_,t) -> search_fail path t

let search_fail t = search_fail [] t


let rec screen_fail path target t =
  let desc =
  match t.desc with
    Unit -> t.desc
  | True -> t.desc
  | False -> t.desc
  | Unknown -> t.desc
  | Int n -> t.desc
  | RandInt b -> t.desc
  | Var x -> t.desc
  | Fun(x,t) -> t.desc
  | App(t1, ts) ->
    let aux i t = screen_fail (i::path) target t in
    let t1ts' = mapi aux (t1::ts) in
    App(List.hd t1ts', List.tl t1ts')
  | If(t1, t2, t3) ->
    let aux i t = screen_fail (i::path) target t in
    If(aux 1 t1, aux 2 t2, aux 3 t3)
  | Branch(t1, t2) ->
    let aux i t = screen_fail (i::path) target t in
    Branch(aux 1 t1, aux 2 t2)
  | Let(flag, defs, t) ->
    let aux i t = screen_fail (i::path) target t in
    let aux_def i (f,xs,t) = f, xs, aux i t in
    Let(flag, mapi aux_def defs, aux (List.length defs) t)
  | BinOp(op, t1, t2) ->
    let aux i t = screen_fail (i::path) target t in
    BinOp(op, aux 1 t1, aux 2 t2)
  | Not t -> Not (screen_fail path target t)
  | Event("fail",_) ->
    if path = target
    then t.desc
    else Bottom
  | Event(s,b) -> t.desc
  | Record fields -> assert false
  | Proj(i,s,f,t) -> assert false
  | SetField(n,i,s,f,t1,t2) -> assert false
  | Nil -> t.desc
  | Cons(t1,t2) ->
    let aux i t = screen_fail (i::path) target t in
    Cons(aux 1 t1, aux 2 t2)
  | Constr(s,ts) ->
    let aux i t = screen_fail (i::path) target t in
    Constr(s, mapi aux ts)
  | Match(t,pats) ->
    let aux i t = screen_fail (i::path) target t in
    let aux_pat i (p,cond,t) = p, aux (2*i+1) cond, aux (2*i+2) t in
    Match(aux 0 t, mapi aux_pat pats)
  | Raise t -> Raise (screen_fail path target t)
  | TryWith(t1,t2) ->
    let aux i t = screen_fail (i::path) target t in
    TryWith(aux 1 t1, aux 2 t2)
  | Bottom -> t.desc
  | Pair(t1,t2) ->
    let aux i t = screen_fail (i::path) target t in
    Pair(aux 1 t1, aux 2 t2)
  | Fst t -> Fst (screen_fail path target t)
  | Snd t -> Snd (screen_fail path target t)
  | RandValue _ -> t.desc
  | Label(info,t) -> Label(info, screen_fail path target t)
  in
  {desc=desc; typ=t.typ}

let screen_fail target t = screen_fail [] target t
