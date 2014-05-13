open Util
open Syntax
open Type




type trans =
  {mutable tr_term: typed_term -> typed_term;
   mutable tr_term_rec: typed_term -> typed_term;
   mutable tr_desc: term -> term;
   mutable tr_desc_rec: term -> term;
   mutable tr_typ: typ -> typ;
   mutable tr_typ_rec: typ -> typ;
   mutable tr_var: id -> id;
   mutable tr_var_rec: id -> id;
   mutable tr_pat: typed_pattern -> typed_pattern;
   mutable tr_pat_rec: typed_pattern -> typed_pattern;
   mutable tr_info: info -> info;
   mutable tr_info_rec: info -> info;
   mutable tr_const: const -> const;
   mutable tr_const_rec: const -> const}

let trans_typ trans = function
    TUnit -> TUnit
  | TBool -> TBool
  | TAbsBool -> TAbsBool
  | TInt -> TInt
  | TRInt p -> TRInt (trans.tr_term p)
  | TVar({contents=None} as x) -> TVar x
  | TVar{contents=Some typ} -> trans.tr_typ typ
  | TFun(x,typ) -> TFun(Id.set_typ x (trans.tr_typ (Id.typ x)), trans.tr_typ typ)
  | TList typ -> TList (trans.tr_typ typ)
  | TPair(x,typ) -> TPair(trans.tr_var x, trans.tr_typ typ)
  | TConstr(s,b) -> TConstr(s,b)
  | TPred(x,ps) -> TPred(trans.tr_var x, List.map trans.tr_term ps)

let trans_var trans x = Id.set_typ x (trans.tr_typ (Id.typ x))

let trans_pat trans p =
  let typ = trans.tr_typ p.pat_typ in
  let desc =
    match p.pat_desc with
        PAny -> PAny
      | PVar x -> PVar (trans.tr_var x)
      | PAlias(p,x) -> PAlias(trans.tr_pat p, trans.tr_var x)
      | PConst t -> PConst (trans.tr_term t)
      | PConstruct(s,ps) -> PConstruct(s, List.map trans.tr_pat ps)
      | PNil -> PNil
      | PCons(p1,p2) -> PCons(trans.tr_pat p1, trans.tr_pat p2)
      | PPair(p1,p2) -> PPair(trans.tr_pat p1, trans.tr_pat p2)
      | PRecord pats -> PRecord(List.map (fun (i,(s,f,p)) -> i,(s,f,trans.tr_pat p)) pats)
      | POr(p1,p2) -> POr(trans.tr_pat p1, trans.tr_pat p2)
  in
    {pat_desc=desc; pat_typ=typ}

let trans_info trans = function
    InfoInt n -> InfoInt n
  | InfoString s -> InfoString s
  | InfoId x -> InfoId (trans.tr_var x)
  | InfoTerm t -> InfoTerm (trans.tr_term t)

let trans_const trans = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Int n -> Int n
  | Char c -> Char c
  | String s -> String s
  | Float s -> Float s
  | Int32 n -> Int32 n
  | Int64 n -> Int64 n
  | Nativeint n -> Nativeint n
  | CPS_result -> CPS_result

let trans_desc trans = function
    Const c -> Const c
  | Unknown -> Unknown
  | RandInt b -> RandInt b
  | RandValue(typ,b) -> RandValue(trans.tr_typ typ,b)
  | Var y -> Var (trans.tr_var y)
  | Fun(y, t) -> Fun(trans.tr_var y, trans.tr_term t)
  | App(t1, ts) -> App(trans.tr_term t1, List.map trans.tr_term ts)
  | If(t1, t2, t3) -> If(trans.tr_term t1, trans.tr_term t2, trans.tr_term t3)
  | Branch(t1, t2) -> Branch(trans.tr_term t1, trans.tr_term t2)
  | Let(flag, bindings, t2) ->
      let bindings' = List.map (fun (f,xs,t) -> trans.tr_var f, List.map trans.tr_var xs, trans.tr_term t) bindings in
      let t2' = trans.tr_term t2 in
        Let(flag, bindings', t2')
  | BinOp(op, t1, t2) -> BinOp(op, trans.tr_term t1, trans.tr_term t2)
  | Not t1 -> Not (trans.tr_term t1)
  | Event(s,b) -> Event(s,b)
  | Record fields ->  Record (List.map (fun (f,(s,t1)) -> f,(s,trans.tr_term t1)) fields)
  | Proj(i,s,f,t1) -> Proj(i,s,f,trans.tr_term t1)
  | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,trans.tr_term t1,trans.tr_term t2)
  | Nil -> Nil
  | Cons(t1,t2) -> Cons(trans.tr_term t1, trans.tr_term t2)
  | Constr(s,ts) -> Constr(s, List.map trans.tr_term ts)
  | Match(t1,pats) ->
      let aux (pat,cond,t1) = trans.tr_pat pat, trans.tr_term cond, trans.tr_term t1 in
        Match(trans.tr_term t1, List.map aux pats)
  | Raise t -> Raise (trans.tr_term t)
  | TryWith(t1,t2) -> TryWith(trans.tr_term t1, trans.tr_term t2)
  | Pair(t1,t2) -> Pair(trans.tr_term t1, trans.tr_term t2)
  | Fst t -> Fst(trans.tr_term t)
  | Snd t -> Snd(trans.tr_term t)
  | Bottom -> Bottom
  | Label(info, t) -> Label(trans.tr_info info, trans.tr_term t)

let trans_term trans t =
  {desc = trans.tr_desc t.desc; typ = trans.tr_typ t.typ}



let make_trans () =
  let trans =
    {tr_term = id;
     tr_term_rec = id;
     tr_desc = id;
     tr_desc_rec = id;
     tr_typ = id;
     tr_typ_rec = id;
     tr_var = id;
     tr_var_rec = id;
     tr_pat = id;
     tr_pat_rec = id;
     tr_info = id;
     tr_info_rec = id;
     tr_const = id;
     tr_const_rec = id}
  in
  trans.tr_term <- trans_term trans;
  trans.tr_term_rec <- trans_term trans;
  trans.tr_desc <- trans_desc trans;
  trans.tr_desc_rec <- trans_desc trans;
  trans.tr_typ <- trans_typ trans;
  trans.tr_typ_rec <- trans_typ trans;
  trans.tr_var <- trans_var trans;
  trans.tr_var_rec <- trans_var trans;
  trans.tr_pat <- trans_pat trans;
  trans.tr_pat_rec <- trans_pat trans;
  trans.tr_info <- trans_info trans;
  trans.tr_info_rec <- trans_info trans;
  trans.tr_const <- trans_const trans;
  trans.tr_const_rec <- trans_const trans;
  trans







type 'a trans2 =
  {mutable tr2_term: 'a -> typed_term -> typed_term;
   mutable tr2_term_rec: 'a -> typed_term -> typed_term;
   mutable tr2_desc: 'a -> term -> term;
   mutable tr2_desc_rec: 'a -> term -> term;
   mutable tr2_typ: 'a -> typ -> typ;
   mutable tr2_typ_rec: 'a -> typ -> typ;
   mutable tr2_var: 'a -> id -> id;
   mutable tr2_var_rec: 'a -> id -> id;
   mutable tr2_pat: 'a -> typed_pattern -> typed_pattern;
   mutable tr2_pat_rec: 'a -> typed_pattern -> typed_pattern;
   mutable tr2_info: 'a -> info -> info;
   mutable tr2_info_rec: 'a -> info -> info;
   mutable tr2_const: 'a -> const -> const;
   mutable tr2_const_rec: 'a -> const -> const}

let trans2_gen_typ tr env = function
    TUnit -> TUnit
  | TBool -> TBool
  | TAbsBool -> TAbsBool
  | TInt -> TInt
  | TRInt p -> TRInt (tr.tr2_term env p)
  | TVar({contents=None} as x) -> TVar x
  | TVar{contents=Some typ} -> tr.tr2_typ env typ
  | TFun(x,typ) -> TFun(Id.set_typ x (tr.tr2_typ env (Id.typ x)), tr.tr2_typ env typ)
  | TList typ -> TList (tr.tr2_typ env typ)
  | TPair(x,typ) -> TPair(tr.tr2_var env x, tr.tr2_typ env typ)
  | TConstr(s,b) -> TConstr(s,b)
  | TPred(x,ps) -> TPred(tr.tr2_var env x, List.map (tr.tr2_term env) ps)

let trans2_gen_var tr env x = Id.set_typ x (tr.tr2_typ env (Id.typ x))

let trans2_gen_pat tr env p =
  let typ = tr.tr2_typ env p.pat_typ in
  let desc =
    match p.pat_desc with
        PAny -> PAny
      | PVar x -> PVar (tr.tr2_var env x)
      | PAlias(p,x) -> PAlias(tr.tr2_pat env p, tr.tr2_var env x)
      | PConst t -> PConst (tr.tr2_term env t)
      | PConstruct(s,ps) -> PConstruct(s, List.map (tr.tr2_pat env) ps)
      | PNil -> PNil
      | PCons(p1,p2) -> PCons(tr.tr2_pat env p1, tr.tr2_pat env p2)
      | PPair(p1,p2) -> PPair(tr.tr2_pat env p1, tr.tr2_pat env p2)
      | PRecord pats -> PRecord(List.map (fun (i,(s,f,p)) -> i,(s,f,tr.tr2_pat env p)) pats)
      | POr(p1,p2) -> POr(tr.tr2_pat env p1, tr.tr2_pat env p2)
  in
    {pat_desc=desc; pat_typ=typ}

let trans2_gen_info tr env = function
    InfoInt n -> InfoInt n
  | InfoString s -> InfoString s
  | InfoId x -> InfoId (tr.tr2_var env x)
  | InfoTerm t -> InfoTerm (tr.tr2_term env t)

let trans2_gen_const tr env = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Int n -> Int n
  | Char c -> Char c
  | String s -> String s
  | Float s -> Float s
  | Int32 n -> Int32 n
  | Int64 n -> Int64 n
  | Nativeint n -> Nativeint n
  | CPS_result -> CPS_result

let trans2_gen_desc tr env = function
    Const c -> Const c
  | Unknown -> Unknown
  | RandInt b -> RandInt b
  | RandValue(typ,b) -> RandValue(tr.tr2_typ env typ, b)
  | Var y -> Var (tr.tr2_var env y)
  | Fun(y, t) -> Fun(tr.tr2_var env y, tr.tr2_term env t)
  | App(t1, ts) -> App(tr.tr2_term env t1, List.map (tr.tr2_term env) ts)
  | If(t1, t2, t3) -> If(tr.tr2_term env t1, tr.tr2_term env t2, tr.tr2_term env t3)
  | Branch(t1, t2) -> Branch(tr.tr2_term env t1, tr.tr2_term env t2)
  | Let(flag, bindings, t2) ->
      let aux (f,xs,t) = tr.tr2_var env f, List.map (tr.tr2_var env) xs, tr.tr2_term env t in
      Let(flag, List.map aux bindings, tr.tr2_term env t2)
  | BinOp(op, t1, t2) -> BinOp(op, tr.tr2_term env t1, tr.tr2_term env t2)
  | Not t1 -> Not (tr.tr2_term env t1)
  | Event(s,b) -> Event(s,b)
  | Record fields ->  Record (List.map (fun (f,(s,t1)) -> f,(s,tr.tr2_term env t1)) fields)
  | Proj(i,s,f,t1) -> Proj(i,s,f,tr.tr2_term env t1)
  | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,tr.tr2_term env t1,tr.tr2_term env t2)
  | Nil -> Nil
  | Cons(t1,t2) -> Cons(tr.tr2_term env t1, tr.tr2_term env t2)
  | Constr(s,ts) -> Constr(s, List.map (tr.tr2_term env) ts)
  | Match(t1,pats) ->
      let aux (pat,cond,t1) = tr.tr2_pat env pat, tr.tr2_term env cond, tr.tr2_term env t1 in
        Match(tr.tr2_term env t1, List.map aux pats)
  | Raise t -> Raise (tr.tr2_term env t)
  | TryWith(t1,t2) -> TryWith(tr.tr2_term env t1, tr.tr2_term env t2)
  | Pair(t1,t2) -> Pair(tr.tr2_term env t1, tr.tr2_term env t2)
  | Fst t -> Fst(tr.tr2_term env t)
  | Snd t -> Snd(tr.tr2_term env t)
  | Bottom -> Bottom
  | Label(info, t) -> Label(tr.tr2_info env info, tr.tr2_term env t)

let trans2_gen_term tr env t = {desc = tr.tr2_desc env t.desc; typ = tr.tr2_typ env t.typ}


let make_trans2 () =
  let id' env = id in
  let tr =
    {tr2_term = id';
     tr2_term_rec = id';
     tr2_desc = id';
     tr2_desc_rec = id';
     tr2_typ = id';
     tr2_typ_rec = id';
     tr2_var = id';
     tr2_var_rec = id';
     tr2_pat = id';
     tr2_pat_rec = id';
     tr2_info = id';
     tr2_info_rec = id';
     tr2_const = id';
     tr2_const_rec = id'}
  in
  tr.tr2_term <- trans2_gen_term tr;
  tr.tr2_term_rec <- trans2_gen_term tr;
  tr.tr2_desc <- trans2_gen_desc tr;
  tr.tr2_desc_rec <- trans2_gen_desc tr;
  tr.tr2_typ <- trans2_gen_typ tr;
  tr.tr2_typ_rec <- trans2_gen_typ tr;
  tr.tr2_var <- trans2_gen_var tr;
  tr.tr2_var_rec <- trans2_gen_var tr;
  tr.tr2_pat <- trans2_gen_pat tr;
  tr.tr2_pat_rec <- trans2_gen_pat tr;
  tr.tr2_info <- trans2_gen_info tr;
  tr.tr2_info_rec <- trans2_gen_info tr;
  tr.tr2_const <- trans2_gen_const tr;
  tr.tr2_const_rec <- trans2_gen_const tr;
  tr






type 'a col =
  {mutable col_term: typed_term -> 'a;
   mutable col_term_rec: typed_term -> 'a;
   mutable col_desc: term -> 'a;
   mutable col_desc_rec: term -> 'a;
   mutable col_typ: typ -> 'a;
   mutable col_typ_rec: typ -> 'a;
   mutable col_var: id -> 'a;
   mutable col_var_rec: id -> 'a;
   mutable col_pat: typed_pattern -> 'a;
   mutable col_pat_rec: typed_pattern -> 'a;
   mutable col_info: info -> 'a;
   mutable col_info_rec: info -> 'a;
   mutable col_const: const -> 'a;
   mutable col_const_rec: const -> 'a;
   mutable col_app: 'a -> 'a -> 'a;
   mutable col_empty: 'a}

let col_typ col = function
    TUnit -> col.col_empty
  | TBool -> col.col_empty
  | TAbsBool -> col.col_empty
  | TInt -> col.col_empty
  | TRInt p -> col.col_empty
  | TVar{contents=None} -> col.col_empty
  | TVar{contents=Some typ} -> col.col_typ typ
  | TFun(x,typ) -> col.col_app (col.col_typ (Id.typ x)) (col.col_typ typ)
  | TList typ -> col.col_typ typ
  | TPair(x,typ) -> col.col_app (col.col_var x) (col.col_typ typ)
  | TConstr(s,b) -> col.col_empty
  | TPred(x,ps) -> List.fold_left (fun acc p -> col.col_app acc @@ col.col_term p) (col.col_var x) ps

let col_var col x = col.col_typ (Id.typ x)

let col_pat col p =
  let r1 = col.col_typ p.pat_typ in
  let r2 =
    match p.pat_desc with
        PAny -> col.col_empty
      | PVar x -> col.col_var x
      | PAlias(p,x) -> col.col_app (col.col_pat p) (col.col_var x)
      | PConst t -> col.col_term t
      | PConstruct(s,ps) -> List.fold_left (fun acc p -> col.col_app acc @@ col.col_pat p) col.col_empty ps
      | PNil -> col.col_empty
      | PCons(p1,p2) -> col.col_app (col.col_pat p1) (col.col_pat p2)
      | PPair(p1,p2) -> col.col_app (col.col_pat p1) (col.col_pat p2)
      | PRecord pats -> List.fold_left (fun acc (i,(s,f,p)) -> col.col_app acc @@ col.col_pat p) col.col_empty pats
      | POr(p1,p2) -> col.col_app (col.col_pat p1) (col.col_pat p2)
  in
  col.col_app r1 r2

let col_info col = function
    InfoInt n -> col.col_empty
  | InfoString s -> col.col_empty
  | InfoId x -> col.col_var x
  | InfoTerm t -> col.col_term t

let col_const col _ = col.col_empty

let col_desc col = function
    Const c -> col.col_empty
  | Unknown -> col.col_empty
  | RandInt b -> col.col_empty
  | RandValue(typ,b) -> col.col_typ typ
  | Var y -> col.col_var y
  | Fun(y, t) -> col.col_app (col.col_var y) (col.col_term t)
  | App(t1, ts) -> List.fold_left (fun acc t -> col.col_app acc @@ col.col_term t) (col.col_term t1) ts
  | If(t1, t2, t3) -> col.col_app (col.col_term t1) @@ col.col_app (col.col_term t2) (col.col_term t3)
  | Branch(t1, t2) -> col.col_app (col.col_term t1) (col.col_term t2)
  | Let(flag, bindings, t2) ->
      let aux acc (f,xs,t) =
        col.col_app acc @@
        col.col_app (col.col_var f) @@
        List.fold_left (fun acc x -> col.col_app acc @@ col.col_var x) (col.col_term t) xs
      in
      List.fold_left aux (col.col_term t2) bindings
  | BinOp(op, t1, t2) -> col.col_app (col.col_term t1) (col.col_term t2)
  | Not t1 -> col.col_term t1
  | Event(s,b) -> col.col_empty
  | Record fields -> List.fold_left (fun acc (f,(s,t1)) -> col.col_app acc @@ col.col_term t1) col.col_empty fields
  | Proj(i,s,f,t1) -> col.col_term t1
  | SetField(n,i,s,f,t1,t2) -> col.col_app (col.col_term t1) (col.col_term t2)
  | Nil -> col.col_empty
  | Cons(t1,t2) -> col.col_app (col.col_term t1) (col.col_term t2)
  | Constr(s,ts) -> List.fold_left (fun acc t -> col.col_app acc @@ col.col_term t) col.col_empty ts
  | Match(t1,pats) ->
      let aux acc (pat,cond,t1) =
        col.col_app acc @@
        col.col_app (col.col_pat pat) @@
        col.col_app (col.col_term cond) @@
        col.col_term t1
      in
      List.fold_left aux (col.col_term t1) pats
  | Raise t -> col.col_term t
  | TryWith(t1,t2) -> col.col_app (col.col_term t1) (col.col_term t2)
  | Pair(t1,t2) -> col.col_app (col.col_term t1) (col.col_term t2)
  | Fst t -> col.col_term t
  | Snd t -> col.col_term t
  | Bottom -> col.col_empty
  | Label(info, t) -> col.col_app (col.col_info info) (col.col_term t)

let col_term col t = col.col_app (col.col_desc t.desc) (col.col_typ t.typ)


let make_col empty app =
  let f _ = empty in
  let col =
    {col_term = f;
     col_term_rec = f;
     col_desc = f;
     col_desc_rec = f;
     col_typ = f;
     col_typ_rec = f;
     col_var = f;
     col_var_rec = f;
     col_pat = f;
     col_pat_rec = f;
     col_info = f;
     col_info_rec = f;
     col_const = f;
     col_const_rec = f;
     col_app = app;
     col_empty = empty}
  in
  col.col_term <- col_term col;
  col.col_term_rec <- col_term col;
  col.col_desc <- col_desc col;
  col.col_desc_rec <- col_desc col;
  col.col_typ <- col_typ col;
  col.col_typ_rec <- col_typ col;
  col.col_var <- col_var col;
  col.col_var_rec <- col_var col;
  col.col_pat <- col_pat col;
  col.col_pat_rec <- col_pat col;
  col.col_info <- col_info col;
  col.col_info_rec <- col_info col;
  col.col_const <- col_const col;
  col.col_const_rec <- col_const col;
  col






type ('a,'b) col2 =
  {mutable col2_term: 'b -> typed_term -> 'a;
   mutable col2_term_rec: 'b -> typed_term -> 'a;
   mutable col2_desc: 'b -> term -> 'a;
   mutable col2_desc_rec: 'b -> term -> 'a;
   mutable col2_typ: 'b -> typ -> 'a;
   mutable col2_typ_rec: 'b -> typ -> 'a;
   mutable col2_var: 'b -> id -> 'a;
   mutable col2_var_rec: 'b -> id -> 'a;
   mutable col2_pat: 'b -> typed_pattern -> 'a;
   mutable col2_pat_rec: 'b -> typed_pattern -> 'a;
   mutable col2_info: 'b -> info -> 'a;
   mutable col2_info_rec: 'b -> info -> 'a;
   mutable col2_const: 'b -> const -> 'a;
   mutable col2_const_rec: 'b -> const -> 'a;
   mutable col2_app: 'a -> 'a -> 'a;
   mutable col2_empty: 'a}

let col2_typ col env = function
    TUnit -> col.col2_empty
  | TBool -> col.col2_empty
  | TAbsBool -> col.col2_empty
  | TInt -> col.col2_empty
  | TRInt p -> col.col2_empty
  | TVar{contents=None} -> col.col2_empty
  | TVar{contents=Some typ} -> col.col2_typ env typ
  | TFun(x,typ) -> col.col2_app (col.col2_typ env (Id.typ x)) (col.col2_typ env typ)
  | TList typ -> col.col2_typ env typ
  | TPair(x,typ) -> col.col2_app (col.col2_var env x) (col.col2_typ env typ)
  | TConstr(s,b) -> col.col2_empty
  | TPred(x,ps) -> List.fold_left (fun acc p -> col.col2_app acc @@ col.col2_term env p) (col.col2_var env x) ps

let col2_var col env x = col.col2_typ env (Id.typ x)

let col2_pat col env p =
  let r1 = col.col2_typ env p.pat_typ in
  let r2 =
    match p.pat_desc with
        PAny -> col.col2_empty
      | PVar x -> col.col2_var env x
      | PAlias(p,x) -> col.col2_app (col.col2_pat env p) (col.col2_var env x)
      | PConst t -> col.col2_term env t
      | PConstruct(s,ps) -> List.fold_left (fun acc p -> col.col2_app acc @@ col.col2_pat env p) col.col2_empty ps
      | PNil -> col.col2_empty
      | PCons(p1,p2) -> col.col2_app (col.col2_pat env p1) (col.col2_pat env p2)
      | PPair(p1,p2) -> col.col2_app (col.col2_pat env p1) (col.col2_pat env p2)
      | PRecord pats -> List.fold_left (fun acc (i,(s,f,p)) -> col.col2_app acc @@ col.col2_pat env p) col.col2_empty pats
      | POr(p1,p2) -> col.col2_app (col.col2_pat env p1) (col.col2_pat env p2)
  in
  col.col2_app r1 r2

let col2_info col env = function
    InfoInt n -> col.col2_empty
  | InfoString s -> col.col2_empty
  | InfoId x -> col.col2_var env x
  | InfoTerm t -> col.col2_term env t

let col2_const col _ _ = col.col2_empty

let col2_desc col env = function
    Const c -> col.col2_empty
  | Unknown -> col.col2_empty
  | RandInt b -> col.col2_empty
  | RandValue(typ,b) -> col.col2_typ env typ
  | Var y -> col.col2_var env y
  | Fun(y, t) -> col.col2_app (col.col2_var env y) (col.col2_term env t)
  | App(t1, ts) -> List.fold_left (fun acc t -> col.col2_app acc @@ col.col2_term env t) (col.col2_term env t1) ts
  | If(t1, t2, t3) -> col.col2_app (col.col2_term env t1) @@ col.col2_app (col.col2_term env t2) (col.col2_term env t3)
  | Branch(t1, t2) -> col.col2_app (col.col2_term env t1) (col.col2_term env t2)
  | Let(flag, bindings, t2) ->
      let aux acc (f,xs,t) =
        col.col2_app acc @@
        col.col2_app (col.col2_var env f) @@
        List.fold_left (fun acc x -> col.col2_app acc @@ col.col2_var env x) (col.col2_term env t) xs
      in
      List.fold_left aux (col.col2_term env t2) bindings
  | BinOp(op, t1, t2) -> col.col2_app (col.col2_term env t1) (col.col2_term env t2)
  | Not t1 -> col.col2_term env t1
  | Event(s,b) -> col.col2_empty
  | Record fields -> List.fold_left (fun acc (f,(s,t1)) -> col.col2_app acc @@ col.col2_term env t1) col.col2_empty fields
  | Proj(i,s,f,t1) -> col.col2_term env t1
  | SetField(n,i,s,f,t1,t2) -> col.col2_app (col.col2_term env t1) (col.col2_term env t2)
  | Nil -> col.col2_empty
  | Cons(t1,t2) -> col.col2_app (col.col2_term env t1) (col.col2_term env t2)
  | Constr(s,ts) -> List.fold_left (fun acc t -> col.col2_app acc @@ col.col2_term env t) col.col2_empty ts
  | Match(t1,pats) ->
      let aux acc (pat,cond,t1) =
        col.col2_app acc @@
        col.col2_app (col.col2_pat env pat) @@
        col.col2_app (col.col2_term env cond) @@
        col.col2_term env t1
      in
      List.fold_left aux (col.col2_term env t1) pats
  | Raise t -> col.col2_term env t
  | TryWith(t1,t2) -> col.col2_app (col.col2_term env t1) (col.col2_term env t2)
  | Pair(t1,t2) -> col.col2_app (col.col2_term env t1) (col.col2_term env t2)
  | Fst t -> col.col2_term env t
  | Snd t -> col.col2_term env t
  | Bottom -> col.col2_empty
  | Label(info, t) -> col.col2_app (col.col2_info env info) (col.col2_term env t)

let col2_term col env t = col.col2_app (col.col2_desc env t.desc) (col.col2_typ env t.typ)


let make_col2 empty app =
  let f _ _ = empty in
  let col =
    {col2_term = f;
     col2_term_rec = f;
     col2_desc = f;
     col2_desc_rec = f;
     col2_typ = f;
     col2_typ_rec = f;
     col2_var = f;
     col2_var_rec = f;
     col2_pat = f;
     col2_pat_rec = f;
     col2_info = f;
     col2_info_rec = f;
     col2_const = f;
     col2_const_rec = f;
     col2_app = app;
     col2_empty = empty}
  in
  col.col2_term <- col2_term col;
  col.col2_term_rec <- col2_term col;
  col.col2_desc <- col2_desc col;
  col.col2_desc_rec <- col2_desc col;
  col.col2_typ <- col2_typ col;
  col.col2_typ_rec <- col2_typ col;
  col.col2_var <- col2_var col;
  col.col2_var_rec <- col2_var col;
  col.col2_pat <- col2_pat col;
  col.col2_pat_rec <- col2_pat col;
  col.col2_info <- col2_info col;
  col.col2_info_rec <- col2_info col;
  col.col2_const <- col2_const col;
  col.col2_const_rec <- col2_const col;
  col






let occur = Syntax.occur
let get_vars_pat = Syntax.get_vars_pat
let get_fv = Syntax.get_fv

(*** TERM CONSTRUCTORS ***)

let typ_result = TConstr("X", false)
let typ_event = TFun(Id.new_var "" TUnit, TUnit)
let typ_event' = TFun(Id.new_var "" TUnit, typ_result)
let typ_event_cps =
  let u = Id.new_var "" TUnit in
  let r = Id.new_var "" TUnit in
  let k = Id.new_var "" (TFun(r,typ_result)) in
    TFun(u, TFun(k, typ_result))
let typ_excep_init = TConstr("exn",true)
let typ_excep = ref typ_excep_init

let dummy_var = Id.make (-1) "" TInt
let abst_var = Id.make (-1) "v" typ_unknown
let abst_var_int = Id.set_typ abst_var TInt
let abst_var_bool = Id.set_typ abst_var TBool
let length_var =
  let x = Id.make (-1) "l" (TList typ_unknown) in
    Id.make (-1) "length" (TFun(x, TInt))

let unit_term = {desc=Const Unit; typ=TUnit}
let true_term = {desc=Const True;typ=TBool}
let false_term = {desc=Const False;typ=TBool}
let cps_result = {desc=Const CPS_result; typ=typ_result}
let fail_term = {desc=Event("fail",false);typ=typ_event}
let fail_term_cps = {desc=Event("fail",false);typ=typ_event'}
let randint_term = {desc=RandInt false; typ=TFun(Id.new_var "" TUnit,TInt)}
let randint_unit_term = {desc=App(randint_term,[unit_term]); typ=TInt}
let randbool_unit_term =
  {desc=BinOp(Eq, {desc=App(randint_term, [unit_term]);typ=TInt}, {desc=Const(Int 0);typ=TInt}); typ=TBool}
let make_bottom typ = {desc=Bottom;typ=typ}
let make_event s = {desc=Event(s,false);typ=typ_event}
let make_event_cps s = {desc=Event(s,true);typ=typ_event_cps}
let make_var x = {desc=Var x; typ=Id.typ x}
let make_int n = {desc=Const(Int n); typ=TInt}
let make_randvalue typ = {desc=RandValue(typ,false); typ=typ}
let make_randvalue_cps typ =
  let r = Id.new_var "" typ in
  let k = Id.new_var "" (TFun(r,typ_result)) in
  {desc=RandValue(typ,true); typ=TFun(k,typ_result)}
let make_randint_cps () =
  let u = Id.new_var "" TUnit in
  let r = Id.new_var "" TInt in
  let k = Id.new_var "" (TFun(r,typ_result)) in
    {desc=RandInt true; typ=TFun(u,TFun(k,typ_result))}
let rec make_app t ts =
  match t,ts with
    | t,[] -> t
    | {desc=App(t1,ts1);typ=TFun(x,typ)}, t2::ts2 ->
        if not (not Flag.check_typ || Type.can_unify (Id.typ x) t2.typ)
        then
          begin
            Format.printf "make_app:@ %a@ <=/=>@ %a@.%a@.%a@."
              print_typ (Id.typ x)
              print_typ t2.typ
              pp_print_term t
              pp_print_term t2;
            assert false
          end;
        make_app {desc=App(t1,ts1@[t2]); typ=typ} ts2
    | {typ=TFun(x,typ)}, t2::ts
    | {typ=TPred({Id.typ=TFun(x,typ)},_)}, t2::ts ->
        if not (not Flag.check_typ || Type.can_unify (Id.typ x) t2.typ)
        then (Color.printf Color.Red "make_app:@ %a@ <=/=>@ %a,@.fun: %a@.arg: %a@."
                print_typ (Id.typ x)
                print_typ t2.typ
                pp_print_term' t
                pp_print_term' t2;
              assert false);
        make_app {desc=App(t,[t2]); typ=typ} ts
    | _ when not Flag.check_typ -> {desc=App(t,ts); typ=typ_unknown}
    | _ ->
        Format.printf "Untypable(make_app): %a@." pp_print_term' {desc=App(t,ts);typ=typ_unknown};
        assert false
let make_lets bindings t2 =
  List.fold_right
    (fun binding t2 ->
       {desc=Let(Nonrecursive,[binding],t2); typ=t2.typ})
    bindings
    t2
let make_let_f flag bindings t2 =
  if bindings = []
  then t2
  else
    let rec aux (f,xs,t) =
      match t.desc with
          Fun(x,t') -> aux (f, xs@[x], t')
        | _ -> f, xs, t
    in
    let bindings' = List.map aux bindings in
      {desc=Let(flag,bindings',t2); typ=t2.typ}
let make_lets_f bindings t2 =
  List.fold_right (fun (flag,binding) -> make_let_f flag [binding]) bindings t2
let make_let bindings t2 = make_let_f Nonrecursive bindings t2
let make_letrec bindings t2 = make_let_f Recursive bindings t2
let make_loop typ =
  let u = Id.new_var "u" TUnit in
  let f = Id.new_var "loop" (TFun(u,typ)) in
  let t = make_app (make_var f) [make_var u] in
    make_letrec [f, [u], t] (make_app (make_var f) [unit_term])
let make_fail typ =
  let u = Id.new_var "u" typ_event in
    make_let [u, [], fail_term] (make_bottom typ)
let make_fun x t = {desc=Fun(x,t); typ=TFun(x,t.typ)}
let make_not t = {desc=Not t; typ=TBool}
let make_and t1 t2 =
  if t1 = true_term
  then t2
  else
    if t1 = false_term
    then false_term
    else
      if t2 = true_term
      then t1
      else {desc=BinOp(And, t1, t2); typ=TBool}
let make_or t1 t2 =
  if t1 = true_term
  then true_term
  else
    if t1 = false_term
    then t2
    else {desc=BinOp(Or, t1, t2); typ=TBool}
let make_add t1 t2 = {desc=BinOp(Add, t1, t2); typ=TInt}
let make_sub t1 t2 = {desc=BinOp(Sub, t1, t2); typ=TInt}
let make_mul t1 t2 = {desc=BinOp(Mult, t1, t2); typ=TInt}
let make_neg t = make_sub (make_int 0) t
let make_if_ t1 t2 t3 =
  assert (not Flag.check_typ || Type.can_unify t1.typ TBool);
  assert (not Flag.check_typ || Type.can_unify t2.typ t3.typ);
  match t1.desc with
      Const True -> t2
    | Const False -> t3
    | _ ->
        let typ =
          match has_pred t2.typ, has_pred t3.typ with
              false, false -> t2.typ
            | true, false -> t2.typ
            | false, true -> t3.typ
            | true, true ->
                if t2.typ <> t3.typ
                then Format.printf "@[<hv 2>Warning: if-branches have different types@ %a and@ %a@]@." pp_print_typ t2.typ pp_print_typ t3.typ;
                t2.typ
        in
          {desc=If(t1, t2, t3); typ=typ}
let make_branch t2 t3 =
  assert (not Flag.check_typ || Type.can_unify t2.typ t3.typ);
  {desc=Branch(t2, t3); typ=t2.typ}
let make_eq t1 t2 =
  assert (not Flag.check_typ || Type.can_unify t1.typ t2.typ);
  {desc=BinOp(Eq, t1, t2); typ=TBool}
let make_neq t1 t2 =
  make_not (make_eq t1 t2)
let make_lt t1 t2 =
  assert (true || not Flag.check_typ || Type.can_unify t1.typ TInt);
  assert (true || not Flag.check_typ || Type.can_unify t2.typ TInt);
  {desc=BinOp(Lt, t1, t2); typ=TBool}
let make_gt t1 t2 =
  assert (true || not Flag.check_typ || Type.can_unify t1.typ TInt);
  assert (true || not Flag.check_typ || Type.can_unify t2.typ TInt);
  {desc=BinOp(Gt, t1, t2); typ=TBool}
let make_leq t1 t2 =
  assert (true || not Flag.check_typ || Type.can_unify t1.typ TInt);
  assert (true || not Flag.check_typ || Type.can_unify t2.typ TInt);
  {desc=BinOp(Leq, t1, t2); typ=TBool}
let make_geq t1 t2 =
  assert (true || not Flag.check_typ || Type.can_unify t1.typ TInt);
  assert (true || not Flag.check_typ || Type.can_unify t2.typ TInt);
  {desc=BinOp(Geq, t1, t2); typ=TBool}
let make_fst t =
  let typ =
    match elim_tpred t.typ with
        TPair(x,_) -> Id.typ x
      | typ when typ = typ_unknown -> typ_unknown
      | typ -> Format.printf "make_fst: %a@." print_typ typ; assert false
  in
    {desc=Fst t; typ=typ}
let make_snd t =
  let typ =
    match elim_tpred t.typ with
        TPair(_,typ) -> typ
      | typ when typ = typ_unknown -> typ_unknown
      | typ -> Format.printf "make_snd: %a@." print_typ typ; assert false
  in
    {desc=Snd t; typ=typ}
let make_tpair ?(s="x") typ1 typ2 = TPair(Id.new_var s typ1, typ2)
let make_pair ?(s="x") t1 t2 = {desc=Pair(t1,t2); typ=make_tpair ~s t1.typ t2.typ}
let make_nil typ = {desc=Nil; typ=TList typ}
let make_nil2 typ = {desc=Nil; typ=typ}
let make_cons t1 t2 =
  assert (not Flag.check_typ || Type.can_unify (TList t1.typ) t2.typ);
  {desc=Cons(t1,t2); typ=t2.typ}
let make_pany typ = {pat_desc=PAny; pat_typ=typ}
let make_pvar x = {pat_desc=PVar x; pat_typ=Id.typ x}
let make_pconst t = {pat_desc=PConst t; pat_typ=t.typ}
let make_ppair p1 p2 = {pat_desc=PPair(p1, p2); pat_typ=TPair(Id.new_var "x" p1.pat_typ, p2.pat_typ)}
let make_pnil typ = {pat_desc=PNil; pat_typ=TList typ}
let make_pnil2 typ = {pat_desc=PNil; pat_typ=typ}
let make_pcons p1 p2 = {pat_desc=PCons(p1,p2); pat_typ=p2.pat_typ}
let make_match t1 pats = {desc=Match(t1,pats); typ=(fun (_,_,t) -> t.typ) (List.hd pats)}
let make_single_match t1 p t2 =
  make_match t1 [p, true_term, t2; make_pany p.pat_typ, true_term, make_fail t2.typ]
let rec make_nth i n t =
  match i,n with
      0,1 -> t
    | 0,2 -> make_fst t
    | _ -> make_nth (i-1) (n-1) (make_snd t)
let make_seq t1 t2 = make_let [Id.new_var "u" t1.typ, [], t1] t2
let make_assert t = make_if_ t unit_term (make_app fail_term [unit_term])
let make_assume t1 t2 = make_if_ t1 t2 (make_bottom t2.typ)
let make_label info t = {desc=Label(info,t); typ=t.typ}

let imply t1 t2 = {desc=BinOp(Or, {desc=Not t1;typ=TBool}, t2); typ=TBool}
let and_list ts = match ts with
    [] -> {desc=Const True; typ=TBool}
  | [t] -> t
  | t::ts -> List.fold_left (fun t1 t2 -> {desc=BinOp(And,t1,t2);typ=TBool}) t ts



let rec make_term typ =
  match elim_tpred typ with
    TUnit -> unit_term
  | TBool -> true_term
  | TInt -> make_int 0
  | TFun(x,typ) -> make_fun x (make_term typ)
  | TPair(x,typ) -> make_pair (make_term @@ Id.typ x) (make_term typ)
  | TConstr("X", false) -> cps_result
  | _ -> Format.printf "ERROR:@.%a@." Syntax.pp_print_typ typ; assert false


let none_flag = false_term
let some_flag = true_term
(*
let none_flag = make_int 0
let some_flag = make_int 1
 *)
let opt_typ typ = TPair(Id.new_var "x" none_flag.typ, typ)
let get_opt_typ typ = snd_typ typ
let is_none t =
  match t.desc with
    Pair(t1,t2) -> t1 = none_flag
  | _ -> false
let make_none typ = make_pair none_flag (make_term typ)
let make_some t = make_pair some_flag t
let make_is_none t = make_eq (make_fst t) none_flag
let make_is_some t = make_not (make_is_none t)
let make_get_val t = make_snd t
let is_is_none t =
  match t.desc with
    BinOp(Eq, {desc=Fst t1}, t2) when t2 = none_flag -> Some t1
  | _ -> None
let is_get_val t =
  match t.desc with
    Snd t -> Some t
  | _ -> None
let is_some t =
  match t.desc with
    Pair(t1,t2) when t1 = some_flag -> Some t2
  | _ -> None

let make_tuple ts =
  match ts with
    []
  | [_] -> raise (Invalid_argument "make_tuple")
  | t::ts' -> List.fold_left (make_pair) t ts'
let rec decomp_tuple top t =
  match t.desc with
    Pair(t1,t2) -> t1 :: decomp_tuple false t2
  | _ when top -> raise (Invalid_argument "decomp_tuple")
  | _ -> [t]
let decomp_tuple = decomp_tuple true

let make_ttuple typs =
  match typs with
    []
  | [_] -> raise (Invalid_argument "tuple_typ")
  | typ::typs' -> List.fold_left (make_tpair) typ typs'
let rec decomp_ttuple top typ =
  match typ with
    TPair(x,typ) -> Id.typ x :: decomp_ttuple false typ
  | _ when top -> raise (Invalid_argument "make_tuple")
  | _ -> [typ]
let decomp_ttuple = decomp_ttuple true

let make_proj i t =
  let n = List.length @@ decomp_ttuple t.typ in
  let t' = repeat make_fst (n-i) t in
  if i = 1 then t' else make_snd t'


(*** AUXILIARY FUNCTIONS ***)

let rec decomp_fun = function
    {desc=Fun(x,t)} ->
      let xs,t' = decomp_fun t in
        x::xs, t'
  | t -> [], t

let rec decomp_let t =
  match t.desc with
    Let(flag, bindings, t2) ->
      let fbindings,t2' = decomp_let t2 in
      (flag,bindings)::fbindings, t2'
  | _ -> [], t


let rec get_int t =
  match t.desc with
      Const (Int n) -> [n]
    | Const _ -> []
    | Unknown -> []
    | Var x -> []
    | App(t, ts) -> get_int t @@@ (rev_map_flatten get_int ts)
    | If(t1, t2, t3) -> get_int t1 @@@ get_int t2 @@@ get_int t3
    | Branch(t1, t2) -> get_int t1 @@@ get_int t2
    | Let(flag, bindings, t2) -> List.fold_left (fun acc (_,_,t) -> get_int t @@@ acc) (get_int t2) bindings
    | BinOp(Mult, t1, t2) -> [] (* non-linear expressions not supported *)
    | BinOp(_, t1, t2) -> get_int t1 @@@ get_int t2
    | Not t -> get_int t
    | Fun(_,t) -> get_int t
    | Event _ -> []
    | Nil -> []
    | Cons(t1,t2) -> get_int t1 @@@ get_int t2
    | RandInt _ -> []
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
    | Bottom -> []
    | Label _ -> assert false
let get_int t = uniq (get_int t)




let rec get_args = function
    TFun(x,typ) -> x :: get_args typ
  | _ -> []

let rec get_argvars = function
    TFun(x,typ) -> x :: get_argvars (Id.typ x) @ get_argvars typ
  | _ -> []

let rec get_argtyps = function
    TFun(x,typ) -> Id.typ x :: get_argtyps typ
  | _ -> []

let arg_num typ = List.length (get_args typ)






let rec subst_var x t y = Id.set_typ y (subst_type x t (Id.typ y))

(* [x |-> t], [t/x] *)
and subst x t t' =
    match t'.desc with
        Const _ -> t'
      | Unknown -> t'
      | Bottom -> t'
      | RandInt _ -> t'
      | Var y when Id.same x y -> t
      | Var y -> make_var (subst_var x t y)
      | Fun(y, t1) when Id.same x y -> t'
      | Fun(y, t1) ->
          let t1' = subst x t t1 in
          let y' = subst_var x t y in
            make_fun y' t1'
      | App(t1, ts) ->
          let t1' = subst x t t1 in
          let ts' = List.map (subst x t) ts in
            make_app t1' ts'
      | If(t1, t2, t3) ->
          let t1' = subst x t t1 in
          let t2' = subst x t t2 in
          let t3' = subst x t t3 in
            make_if_ t1' t2' t3'
      | Branch(t1, t2) ->
          let t1' = subst x t t1 in
          let t2' = subst x t t2 in
            make_branch t1' t2'
      | Let(Nonrecursive, bindings, t2) ->
          let aux (f,xs,t1) =
            subst_var x t f,
            List.map (subst_var x t) xs,
            if List.exists (Id.same x) xs then t1 else subst x t t1 in
          let bindings' = List.map aux bindings in
          let t2' =
            if List.exists (fun (f,_,_) -> Id.same f x) bindings
            then t2
            else subst x t t2
          in
            make_let bindings' t2'
      | Let(Recursive, bindings, t2) when List.exists (fun (f,_,_) -> Id.same f x) bindings -> t'
      | Let(Recursive, bindings, t2) ->
          let aux (f,xs,t1) =
            subst_var x t f,
            List.map (subst_var x t) xs,
            if List.exists (Id.same x) xs then t1 else subst x t t1
          in
          let bindings' = List.map aux bindings in
          let t2' = subst x t t2 in
            make_letrec bindings' t2'
      | BinOp(op, t1, t2) ->
          let t1' = subst x t t1 in
          let t2' = subst x t t2 in
            {desc=BinOp(op, t1', t2'); typ=t'.typ}
      | Not t1 ->
          let t1' = subst x t t1 in
            make_not t1'
      | Event(s,_) -> t'
      | Record fields -> {desc=Record (List.map (fun (f,(s,t1)) -> f,(s,subst x t t1)) fields); typ=t'.typ}
      | Proj(i,s,f,t1) -> {desc=Proj(i,s,f,subst x t t1); typ=t'.typ}
      | SetField(n,i,s,f,t1,t2) -> {desc=SetField(n,i,s,f,subst x t t1,subst x t t2); typ=t'.typ}
      | Nil -> t'
      | Cons(t1,t2) -> {desc=Cons(subst x t t1, subst x t t2); typ=t'.typ}
      | Constr(s,ts) -> {desc=Constr(s, List.map (subst x t) ts); typ=t'.typ}
      | Match(t1,pats) ->
          let aux (pat,cond,t1) =
            let xs = get_vars_pat pat in
            if List.exists (Id.same x) xs
            then pat, cond, t1
            else pat, subst x t cond, subst x t t1
          in
            {desc=Match(subst x t t1, List.map aux pats); typ=t'.typ}
      | Raise t1 -> {desc=Raise(subst x t t1); typ=t'.typ}
      | TryWith(t1,t2) -> {desc=TryWith(subst x t t1, subst x t t2); typ=t'.typ}
      | Pair(t1,t2) -> make_pair (subst x t t1) (subst x t t2)
      | Fst t1 -> make_fst (subst x t t1)
      | Snd t1 -> make_snd (subst x t t1)
      | RandValue(typ,b) -> {desc=RandValue(typ,b); typ=t'.typ}
      | Label(info, t1) -> make_label info (subst x t t1)


and subst_int n t t' =
  let desc =
    match t'.desc with
        Const (Int m) -> if n = m then t.desc else BinOp(Add, t, {desc=Const(Int(m-n)); typ=TInt})
      | Const c -> Const c
      | Unknown -> Unknown
      | Var y -> Var y
      | Bottom -> Bottom
      | Fun(y, t1) -> Fun(y, subst_int n t t1)
      | App(t1, ts) ->
          let t1' = subst_int n t t1 in
          let ts' = List.map (subst_int n t) ts in
            begin
              match t1'.desc with
                  App(t, ts) -> App(t, ts@ts')
                | _ -> App(t1', ts')
            end
      | If(t1, t2, t3) ->
          let t1' = subst_int n t t1 in
          let t2' = subst_int n t t2 in
          let t3' = subst_int n t t3 in
            If(t1', t2', t3')
      | Branch(t1, t2) ->
          let t1' = subst_int n t t1 in
          let t2' = subst_int n t t2 in
            Branch(t1', t2')
              (*
                | Let(flag, bindings, t2) ->
                let bindings' = List.map (fun (f,xs,t1) -> f,xs,subst_int n t t1) bindings in
                let t2' = subst_int n t t2 in
                Let(flag, bindings', t2')
              *)
      | Let(flag, bindings, t2) ->
          let bindings' = List.map (fun (f,xs,t1) -> f, xs, subst_int n t t1) bindings in
          let t2' = subst_int n t t2 in
            Let(flag, bindings', t2')
      | BinOp(Mult, t1, t2) -> (* non-linear expressions not supported *)
          BinOp(Mult, t1, t2)
      | BinOp(op, t1, t2) ->
          let t1' = subst_int n t t1 in
          let t2' = subst_int n t t2 in
            BinOp(op, t1', t2')
      | Not t1 ->
          let t1' = subst_int n t t1 in
            Not t1'
      | Event(s,b) -> Event(s,b)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(subst_int n t t1, subst_int n t t2)
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
      | RandInt _ -> assert false
      | RandValue (_, _) -> assert false
      | Label _ -> assert false
  in
    {desc=desc; typ=t'.typ}

and subst_map map t =
  match t.desc with
      Const c -> t
    | Unknown -> t
    | Bottom -> t
    | RandInt _ -> t
    | Var y -> if Id.mem_assoc y map then Id.assoc y map else t
    | Fun(y, t1) ->
        let map' = List.filter (fun (x,_) -> not (Id.same x y)) map in
        let t1' = subst_map map' t1 in
          make_fun y t1'
    | App(t1, ts) ->
        let t1' = subst_map map t1 in
        let ts' = List.map (subst_map map) ts in
          make_app t1' ts'
    | If(t1, t2, t3) ->
        let t1' = subst_map map t1 in
        let t2' = subst_map map t2 in
        let t3' = subst_map map t3 in
          make_if_ t1' t2' t3'
    | Branch(t1, t2) ->
        let t1' = subst_map map t1 in
        let t2' = subst_map map t2 in
          make_branch t1' t2'
    | Let(Nonrecursive, bindings, t2) ->
        let rec aux map acc = function
            [] -> map, List.rev acc
          | (f,xs,t1)::bindings ->
              let map' = List.filter (fun (x,_) -> not (Id.mem x xs)) map in
                aux map' ((f, xs, subst_map map' t1)::acc) bindings in
        let map',bindings' = aux map [] bindings in
        let t2' = subst_map map' t2 in
          make_let bindings' t2'
    | Let(Recursive, bindings, t2) ->
        let map' = List.filter (fun (x,_) -> not (List.exists (fun (f,_,_) -> Id.same f x) bindings)) map in
        let aux (f,xs,t1) =
          let map'' = List.filter (fun (x,_) -> not (Id.mem x xs)) map' in
            f, xs, subst_map map'' t1
        in
        let bindings' = List.map aux bindings in
        let t2' = subst_map map' t2 in
          make_letrec bindings' t2'
    | BinOp(op, t1, t2) ->
        let t1' = subst_map map t1 in
        let t2' = subst_map map t2 in
          {desc=BinOp(op, t1', t2'); typ=t.typ}
    | Not t1 ->
        let t1' = subst_map map t1 in
          make_not t1'
    | Event(s,_) -> t
    | Record fields -> {desc=Record (List.map (fun (f,(s,t1)) -> f,(s,subst_map map t1)) fields); typ=t.typ}
    | Proj(i,s,f,t1) -> {desc=Proj(i,s,f,subst_map map t1); typ=t.typ}
    | SetField(n,i,s,f,t1,t2) -> {desc=SetField(n,i,s,f,subst_map map t1,subst_map map t2); typ=t.typ}
    | Nil -> t
    | Cons(t1,t2) -> {desc=Cons(subst_map map t1, subst_map map t2); typ=t.typ}
    | Constr(s,ts) -> {desc=Constr(s, List.map (subst_map map) ts); typ=t.typ}
    | Match(t1,pats) ->
        let aux (pat,cond,t1) = pat, cond, subst_map map t1 in
          {desc=Match(subst_map map t1, List.map aux pats); typ=t.typ}
    | Raise t1 -> {desc=Raise(subst_map map t1); typ=t.typ}
    | TryWith(t1,t2) -> {desc=TryWith(subst_map map t1, subst_map map t2); typ=t.typ}
    | Pair(t1,t2) -> make_pair (subst_map map t1) (subst_map map t2)
    | Fst t1 -> make_fst (subst_map map t1)
    | Snd t1 -> make_snd (subst_map map t1)
    | RandValue _ -> assert false
    | Label _ -> assert false

and subst_type x t = function
    TUnit -> TUnit
  | TAbsBool -> TAbsBool
  | TBool -> TBool
  | TInt -> TInt
  | TPred(y,ps) ->
      let y' = Id.set_typ y (subst_type x t (Id.typ y)) in
      let ps' = List.map (subst x t) ps in
        TPred(y', ps')
  | TRInt t' -> TRInt (subst x t t')
  | TVar y -> TVar y
  | TFun(y,typ) ->
      let y' = Id.set_typ y (subst_type x t (Id.typ y)) in
      let typ' = subst_type x t typ in
        TFun(y', typ')
  | TList typ -> TList (subst_type x t typ)
  | TConstr(s,b) -> TConstr(s,b)
  | TPair(y,typ) ->
      let y' = Id.set_typ y (subst_type x t (Id.typ y)) in
      let typ' = subst_type x t typ in
        TPair(y', typ')












let rec max_pat_num t =
  match t.desc with
      Const _ -> 0
    | Unknown -> 0
    | Var _ -> 0
    | Fun(_, t) -> max_pat_num t
    | App(t, ts) -> List.fold_left (fun acc t -> max acc (max_pat_num t)) (max_pat_num t) ts
    | If(t1, t2, t3) -> max (max (max_pat_num t1) (max_pat_num t2)) (max_pat_num t3)
    | Branch(t1, t2) -> max (max_pat_num t1) (max_pat_num t2)
    | Let(_, bindings, t2) -> List.fold_left (fun m (_,_,t) -> max m (max_pat_num t)) (max_pat_num t2) bindings
    | BinOp(_, t1, t2) -> max (max_pat_num t1) (max_pat_num t2)
    | Not t -> max_pat_num t
    | Event _ -> 0
    | Nil -> 0
    | Cons(t1,t2) -> max (max_pat_num t1) (max_pat_num t2)
    | Constr(_,ts) -> List.fold_left (fun acc t -> max acc (max_pat_num t)) 0 ts
    | Match(t,pats) ->
        let m = max (List.length pats) (max_pat_num t) in
        let aux acc = function
          | (_,cond,t) -> max acc (max (max_pat_num t) (max_pat_num cond))
        in
          List.fold_left aux m pats
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
    | Bottom -> assert false
    | Label _ -> assert false

let rec max_label_num t =
  match t.desc with
      Const _ -> -1
    | Unknown -> -1
    | RandInt _ -> -1
    | Var _ -> -1
    | Fun(_, t) -> max_label_num t
    | App(t, ts) ->
        List.fold_left (fun acc t -> max acc (max_label_num t)) (max_label_num t) ts
    | If(t1, t2, t3) ->
        max (max (max_label_num t1) (max_label_num t2)) (max_label_num t3)
    | Branch(t1, t2) -> max (max_label_num t1) (max_label_num t2)
    | Let(_, bindings, t2) -> List.fold_left (fun m (_,_,t) -> max m (max_pat_num t)) (max_pat_num t2) bindings
    | BinOp(_, t1, t2) -> max (max_label_num t1) (max_label_num t2)
    | Not t -> max_label_num t
    | Event _ -> -1
    | Nil -> -1
    | Cons(t1,t2) -> max (max_label_num t1) (max_label_num t2)
    | Constr(_,ts) ->
        List.fold_left (fun acc t -> max acc (max_label_num t)) (-1) ts
    | Match(t,pats) ->
        let aux acc = function
          | (_,cond,t) -> max acc (max (max_label_num t) (max_label_num cond))
        in
          List.fold_left aux (-1) pats
    | Snd _ -> assert false
    | Fst _ -> assert false
    | Pair (_, _) -> assert false
    | TryWith (_, _) -> assert false
    | Raise _ -> assert false
    | SetField (_, _, _, _, _, _) -> assert false
    | Proj (_, _, _, _) -> assert false
    | Record _ -> assert false
    | RandValue (_, _) -> assert false
    | Bottom -> assert false
    | Label _ -> assert false


let is_parameter x = Fpat.Util.String.starts_with (Id.name x) Flag.extpar_header


let rec is_value t =
  match t.desc with
      Const _ | Var _ | Nil -> true
    | _ -> false



let rec merge_typ typ1 typ2 =
  match typ1,typ2 with
      TVar{contents=Some typ1}, typ2
    | typ1, TVar{contents=Some typ2} -> merge_typ typ1 typ2
    | TVar({contents=None}), _ -> typ2
    | _, TVar({contents=None}) -> typ1
    | TUnit, TUnit -> TUnit
    | TBool, TBool -> TBool
    | TInt, TInt -> TInt
    | TPred(x1,ps1), TPred(x2,ps2) ->
        let typ = merge_typ (Id.typ x1) (Id.typ x2) in
        let x1' = Id.set_typ x1 typ in
        let x1_no_pred = Id.set_typ x1 (elim_tpred typ) in
        let ps2' = List.map (subst x2 (make_var x1_no_pred)) ps2 in
          TPred(x1', ps1 @@@ ps2')
    | TPred(x, ps), typ
    | typ, TPred(x, ps) -> TPred(Id.set_typ x (merge_typ (Id.typ x) typ), ps)
    | TFun(x1,typ1), TFun(x2,typ2) ->
        let x_typ = merge_typ (Id.typ x1) (Id.typ x2) in
        let x = Id.new_var (Id.name x1) x_typ in
        let typ = merge_typ (subst_type x1 (make_var x) typ1) (subst_type x2 (make_var x) typ2) in
          TFun(x, typ)
    | TList typ1, TList typ2 -> TList(merge_typ typ1 typ2)
    | TPair(x1,typ1), TPair(x2,typ2) ->
        let x_typ = merge_typ (Id.typ x1) (Id.typ x2) in
        let x = Id.new_var (Id.name x1) x_typ in
        let typ = merge_typ (subst_type x1 (make_var x) typ1) (subst_type x2 (make_var x) typ2) in
          TPair(x, typ)
    | _ when typ1 = typ_unknown -> typ2
    | _ when typ2 = typ_unknown -> typ1
    | TConstr _, TConstr _ -> assert (typ1 = typ2); typ1
    | _ -> Format.printf "typ1:%a, typ2:%a@." pp_print_typ typ1 pp_print_typ typ2; assert false


let make_if t1 t2 t3 =
  assert (not Flag.check_typ || Type.can_unify t1.typ TBool);
  if Flag.check_typ && not (Type.can_unify t2.typ t3.typ)
  then Format.printf "%a <=/=> %a@." print_typ t2.typ print_typ t3.typ;
  assert (not Flag.check_typ || Type.can_unify t2.typ t3.typ);
  match t1.desc with
      Const True -> t2
    | Const False -> t3
    | _ -> {desc=If(t1, t2, t3); typ=merge_typ t2.typ t3.typ}

let rec get_top_funs acc = function
    {desc=Let(flag, defs, t)} ->
      let acc' = List.fold_left (fun acc (f,_,_) -> f::acc) acc defs in
        get_top_funs acc' t
  | _ -> acc
let get_top_funs = get_top_funs []


let rec get_typ_default = function
    TUnit -> unit_term
  | TBool -> true_term
  | TAbsBool -> assert false
  | TInt -> make_int 0
  | TRInt _ -> assert false
  | TVar _ -> assert false
  | TFun(x,typ) -> make_fun x (get_typ_default typ)
  | TList typ -> make_nil typ
  | TPair(x,typ) -> make_pair ~s:(Id.name x) (get_typ_default (Id.typ x)) (get_typ_default typ)
  | TConstr(s,b) -> assert false
  | TPred _ -> assert false




let subst_rev = make_trans2 ()

(* [t1 |-> x] *)
let subst_rev_term (t1,x) t2 =
  if t1 = t2
  then make_var x
  else subst_rev.tr2_term_rec (t1,x) t2

let () = subst_rev.tr2_term <- subst_rev_term

let subst_rev t1 x t2 = subst_rev.tr2_term (t1,x) t2


(* replace t1 with t2 in t3 *)
let replace_term t1 t2 t3 =
  let x = Id.new_var "x" t1.typ in
  subst x t2 @@ subst_rev t1 x t3



let rec has_no_effect t =
  match t.desc with
    Const _ -> true
  | Unknown -> false
  | RandInt _ -> true
  | RandValue _ -> true
  | Var _ -> true
  | Fun _ -> true
  | App _ -> false
  | If(t1, t2, t3) -> has_no_effect t1 && has_no_effect t2 && has_no_effect t3
  | Branch _ -> false
  | Let(_,bindings,t) ->
      has_no_effect t && List.for_all (fun (f,xs,t) -> xs <> [] || has_no_effect t) bindings
  | BinOp(op, t1, t2) -> has_no_effect t1 && has_no_effect t2
  | Not t -> has_no_effect t
  | Event _ -> true
  | Record _ -> false
  | Proj _ -> false
  | SetField _ -> false
  | Nil -> true
  | Cons(t1, t2) -> has_no_effect t1 && has_no_effect t2
  | Constr(_, ts) -> List.for_all has_no_effect ts
  | Match _ -> false
  | Raise _ -> false
  | TryWith _ -> false
  | Pair(t1, t2) -> has_no_effect t1 && has_no_effect t2
  | Fst t -> has_no_effect t
  | Snd t -> has_no_effect t
  | Bottom -> false
  | Label _ -> false


let rec is_simple_aexp t =
  if t.typ <> TInt
  then false
  else
    match t.desc with
      Const _ -> true
    | Var _ -> true
    | BinOp(_, t1, t2) -> is_simple_aexp t1 && is_simple_aexp t2
    | _ -> false

and is_simple_bexp t =
  if t.typ <> TInt
  then false
  else
    match t.desc with
      Const _ -> true
    | Var _ -> true
    | BinOp(_, t1, t2) ->
        is_simple_bexp t1 && is_simple_bexp t2 ||
        is_simple_aexp t1 && is_simple_aexp t2
    | Not t -> is_simple_bexp t
    | _ -> false



let same_list same xs ys = List.length xs = List.length ys && List.for_all2 same xs ys

let rec same_const = (=)
and same_term t1 t2 = same_desc t1.desc t2.desc
and same_desc t1 t2 =
  match t1,t2 with
    Const c1, Const c2 -> same_const c1 c2
  | Unknown, Unknown -> true
  | RandInt b1, RandInt b2 -> b1 = b2
  | RandValue _, RandValue _ -> unsupported "same_term 1 "
  | Var x, Var y -> Id.same x y
  | Fun(x,t1), Fun(y,t2) -> Id.same x y && same_term t1 t2
  | App(t1,ts1), App(t2,ts2) -> same_list same_term (t1::ts1) (t2::ts2)
  | If(t11,t12,t13), If(t21,t22,t23) -> same_term t11 t21 && same_term t12 t22 && same_term t13 t23
  | Branch(t11,t12), Branch(t21,t22) -> same_term t11 t21 && same_term t12 t22
  | Let(flag1,bindings1,t1), Let(flag2,bindings2,t2) ->
     let same_binding (f,xs,t1) (g,ys,t2) = Id.same f g && same_list Id.same xs ys && same_term t1 t2 in
     flag1 = flag1 && same_list same_binding bindings1 bindings2 && same_term t1 t2
  | BinOp(op1,t11,t12), BinOp(op2,t21,t22) -> op1 = op2 && same_term t11 t21 && same_term t12 t22
  | Not t1, Not t2 -> same_term t1 t2
  | Event(s1,b1), Event(s2,b2) -> s1 = s2 && b1 = b2
  | Record _, Record _ -> unsupported "same_term 2"
  | Proj _, Proj _ -> unsupported "same_term 3"
  | SetField _, SetField _ -> unsupported "same_term 4"
  | Nil, Nil -> true
  | Cons _, Cons _ -> unsupported "same_term 5"
  | Constr _, Constr _ -> unsupported "same_term 6"
  | Match _, Match _ -> unsupported "same_term 7"
  | Raise _, Raise _ -> unsupported "same_term 8"
  | TryWith _, TryWith _ -> unsupported "same_term 9"
  | Pair(t11,t12), Pair(t21,t22) -> same_term t11 t21 && same_term t12 t22
  | Fst t1, Fst t2 -> same_term t1 t2
  | Snd t1, Snd t2 -> same_term t1 t2
  | Bottom, Bottom -> true
  | Label _, Label _ -> unsupported "same_term 11"
  | _ -> false

and same_info i1 i2 = unsupported "same_term"
and same_type_kind k1 k2 = unsupported "same_term"

and same_typed_pattern p1 p2 = same_pattern p1.desc p2.desc
and same_pattern p1 p2 = unsupported "same_term"


let rec var_name_of_term t =
  match t.desc, elim_tpred t.typ with
    Var x, _ -> Id.name x
  | _,     TUnit _ -> "u"
  | _,     TBool _ -> "b"
  | _,     TInt _  -> "n"
  | _,     TFun _  -> "f"
  | _,     TPair _ -> "p"
  | App _, _       -> "r"
  | Fst t, _       -> var_name_of_term t ^ "_l"
  | Snd _, _       -> var_name_of_term t ^ "_r"
  | Fun _, _       -> assert false
  | _,     _       -> "x"

let var_of_term t = Id.new_var (var_name_of_term t) t.typ

let is_dependend t x = Id.mem x @@ get_fv t
