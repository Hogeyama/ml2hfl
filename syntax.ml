open Format
open Util
open Type

type label = Read | Write | Close
type binop = Eq | Lt | Gt | Leq | Geq | And | Or | Add | Sub | Mult

type typ = typed_term Type.t
and id = typ Id.t

and const = (* only base type constants *)
  | Unit
  | True
  | False
  | Int of int
  | Char of char
  | String of string
  | Float of string
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
  | CPS_result
  | RandInt of bool
  | RandValue of typ * bool

and attr =
  | ANone
  | AAbst_under

and typed_term = {desc:term; typ:typ; attr:attr}
and term =
  | Const of const
  | Var of id
  | Fun of id * typed_term
  | App of typed_term * typed_term list
  | If of typed_term * typed_term * typed_term
  | Branch of typed_term * typed_term
  | Let of rec_flag * (id * id list * typed_term) list * typed_term
  | BinOp of binop * typed_term * typed_term
  | Not of typed_term
  | Event of string * bool
  | Record of (string * (mutable_flag * typed_term)) list
  | Field of int * string * mutable_flag * typed_term
  | SetField of int option * int * string * mutable_flag * typed_term * typed_term
  | Nil
  | Cons of typed_term * typed_term
  | Constr of string * typed_term list
  | Match of typed_term * (typed_pattern * typed_term * typed_term) list
  | Raise of typed_term
  | TryWith of typed_term * typed_term
  | Tuple of typed_term list
  | Proj of int * typed_term
  | Bottom
  | Label of info * typed_term
  | Ref of typed_term
  | Deref of typed_term
  | SetRef of typed_term * typed_term
  | TNone
  | TSome of typed_term

and info =
  | InfoInt of int
  | InfoString of string
  | InfoId of id
  | InfoTerm of typed_term
  | InfoIdTerm of id * typed_term

and rec_flag = Nonrecursive | Recursive
and mutable_flag = Immutable | Mutable


and type_kind =
    KAbstract
  | KVariant of (string * typ list) list
  | KRecord of (string * (mutable_flag * typ)) list

and pred = term

and typed_pattern = {pat_desc:pattern; pat_typ:typ}
and pattern =
    PAny
  | PVar of id
  | PAlias of typed_pattern * id
  | PConst of typed_term
  | PConstruct of string * typed_pattern list
  | PNil
  | PCons of typed_pattern * typed_pattern
  | PTuple of typed_pattern list
  | PRecord of (int * (string * mutable_flag * typed_pattern)) list
  | PNone
  | PSome of typed_pattern
  | POr of typed_pattern * typed_pattern

type node = BrNode | LabNode of bool | FailNode | EventNode of string | PatNode of int



type trans =
  {mutable tr_term:      typed_term    -> typed_term;
   mutable tr_term_rec:  typed_term    -> typed_term;
   mutable tr_desc:      term          -> term;
   mutable tr_desc_rec:  term          -> term;
   mutable tr_typ:       typ           -> typ;
   mutable tr_typ_rec:   typ           -> typ;
   mutable tr_var:       id            -> id;
   mutable tr_var_rec:   id            -> id;
   mutable tr_pat:       typed_pattern -> typed_pattern;
   mutable tr_pat_rec:   typed_pattern -> typed_pattern;
   mutable tr_info:      info          -> info;
   mutable tr_info_rec:  info          -> info;
   mutable tr_const:     const         -> const;
   mutable tr_const_rec: const         -> const}

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
  | TTuple xs -> TTuple (List.map trans.tr_var xs)
  | TConstr(s,b) -> TConstr(s,b)
  | TPred(x,ps) -> TPred(trans.tr_var x, List.map trans.tr_term ps)
  | TRef typ -> TRef (trans.tr_typ typ)
  | TOption typ -> TOption (trans.tr_typ typ)

let trans_var trans x = Id.set_typ x (trans.tr_typ (Id.typ x))

let trans_pat trans p =
  let typ = trans.tr_typ p.pat_typ in
  let desc =
    match p.pat_desc with
    | PAny -> PAny
    | PVar x -> PVar (trans.tr_var x)
    | PAlias(p,x) -> PAlias(trans.tr_pat p, trans.tr_var x)
    | PConst t -> PConst (trans.tr_term t)
    | PConstruct(s,ps) -> PConstruct(s, List.map trans.tr_pat ps)
    | PNil -> PNil
    | PCons(p1,p2) -> PCons(trans.tr_pat p1, trans.tr_pat p2)
    | PTuple ps -> PTuple (List.map trans.tr_pat ps)
    | PRecord pats -> PRecord(List.map (fun (i,(s,f,p)) -> i,(s,f,trans.tr_pat p)) pats)
    | POr(p1,p2) -> POr(trans.tr_pat p1, trans.tr_pat p2)
    | PNone -> PNone
    | PSome p -> PSome (trans.tr_pat p)
  in
  {pat_desc=desc; pat_typ=typ}

let trans_info trans = function
  | InfoInt n -> InfoInt n
  | InfoString s -> InfoString s
  | InfoId x -> InfoId (trans.tr_var x)
  | InfoTerm t -> InfoTerm (trans.tr_term t)
  | InfoIdTerm(x, t) ->  InfoIdTerm(trans.tr_var x, trans.tr_term t)

let trans_const trans = function
  | Unit -> Unit
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
  | RandInt _ -> assert false
  | RandValue(typ,b) -> RandValue(trans.tr_typ typ,b)

let trans_desc trans = function
  | Const c -> Const c
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
  | Field(i,s,f,t1) -> Field(i,s,f,trans.tr_term t1)
  | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,trans.tr_term t1,trans.tr_term t2)
  | Nil -> Nil
  | Cons(t1,t2) -> Cons(trans.tr_term t1, trans.tr_term t2)
  | Constr(s,ts) -> Constr(s, List.map trans.tr_term ts)
  | Match(t1,pats) ->
      let aux (pat,cond,t1) = trans.tr_pat pat, trans.tr_term cond, trans.tr_term t1 in
        Match(trans.tr_term t1, List.map aux pats)
  | Raise t -> Raise (trans.tr_term t)
  | TryWith(t1,t2) -> TryWith(trans.tr_term t1, trans.tr_term t2)
  | Tuple ts -> Tuple (List.map trans.tr_term ts)
  | Proj(i,t) -> Proj(i, trans.tr_term t)
  | Bottom -> Bottom
  | Label(info, t) -> Label(trans.tr_info info, trans.tr_term t)
  | Ref t -> Ref(trans.tr_term t)
  | Deref t -> Deref(trans.tr_term t)
  | SetRef(t1, t2) -> SetRef(trans.tr_term t1, trans.tr_term t2)
  | TNone -> TNone
  | TSome t -> TSome (trans.tr_term t)

let trans_term trans t =
  {desc = trans.tr_desc t.desc; typ = trans.tr_typ t.typ; attr=t.attr}



let make_trans () =
  let trans =
    {tr_term = Std.identity;
     tr_term_rec = Std.identity;
     tr_desc = Std.identity;
     tr_desc_rec = Std.identity;
     tr_typ = Std.identity;
     tr_typ_rec = Std.identity;
     tr_var = Std.identity;
     tr_var_rec = Std.identity;
     tr_pat = Std.identity;
     tr_pat_rec = Std.identity;
     tr_info = Std.identity;
     tr_info_rec = Std.identity;
     tr_const = Std.identity;
     tr_const_rec = Std.identity}
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
  | TUnit -> TUnit
  | TBool -> TBool
  | TAbsBool -> TAbsBool
  | TInt -> TInt
  | TRInt p -> TRInt (tr.tr2_term env p)
  | TVar({contents=None} as x) -> TVar x
  | TVar{contents=Some typ} -> tr.tr2_typ env typ
  | TFun(x,typ) -> TFun(Id.set_typ x (tr.tr2_typ env (Id.typ x)), tr.tr2_typ env typ)
  | TList typ -> TList (tr.tr2_typ env typ)
  | TTuple xs -> TTuple (List.map (tr.tr2_var env) xs)
  | TConstr(s,b) -> TConstr(s,b)
  | TPred(x,ps) -> TPred(tr.tr2_var env x, List.map (tr.tr2_term env) ps)
  | TRef typ -> TRef (tr.tr2_typ env typ)
  | TOption typ -> TOption (tr.tr2_typ env typ)

let trans2_gen_var tr env x = Id.set_typ x (tr.tr2_typ env (Id.typ x))

let trans2_gen_pat tr env p =
  let typ = tr.tr2_typ env p.pat_typ in
  let desc =
    match p.pat_desc with
    | PAny -> PAny
    | PVar x -> PVar (tr.tr2_var env x)
    | PAlias(p,x) -> PAlias(tr.tr2_pat env p, tr.tr2_var env x)
    | PConst t -> PConst (tr.tr2_term env t)
    | PConstruct(s,ps) -> PConstruct(s, List.map (tr.tr2_pat env) ps)
    | PNil -> PNil
    | PCons(p1,p2) -> PCons(tr.tr2_pat env p1, tr.tr2_pat env p2)
    | PTuple ps -> PTuple (List.map (tr.tr2_pat env) ps)
    | PRecord pats -> PRecord(List.map (fun (i,(s,f,p)) -> i,(s,f,tr.tr2_pat env p)) pats)
    | POr(p1,p2) -> POr(tr.tr2_pat env p1, tr.tr2_pat env p2)
    | PNone -> PNone
    | PSome p -> PSome (tr.tr2_pat env p)
  in
  {pat_desc=desc; pat_typ=typ}

let trans2_gen_info tr env = function
  | InfoInt n -> InfoInt n
  | InfoString s -> InfoString s
  | InfoId x -> InfoId (tr.tr2_var env x)
  | InfoTerm t -> InfoTerm (tr.tr2_term env t)
  | InfoIdTerm(x, t) ->  InfoIdTerm(tr.tr2_var env x, tr.tr2_term env t)

let trans2_gen_const tr env = function
  | Unit -> Unit
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
  | RandInt b -> RandInt b
  | RandValue(typ,b) -> RandValue(tr.tr2_typ env typ, b)

let trans2_gen_desc tr env = function
  | Const c -> Const c
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
  | Field(i,s,f,t1) -> Field(i,s,f,tr.tr2_term env t1)
  | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,tr.tr2_term env t1,tr.tr2_term env t2)
  | Nil -> Nil
  | Cons(t1,t2) -> Cons(tr.tr2_term env t1, tr.tr2_term env t2)
  | Constr(s,ts) -> Constr(s, List.map (tr.tr2_term env) ts)
  | Match(t1,pats) ->
      let aux (pat,cond,t1) = tr.tr2_pat env pat, tr.tr2_term env cond, tr.tr2_term env t1 in
      Match(tr.tr2_term env t1, List.map aux pats)
  | Raise t -> Raise (tr.tr2_term env t)
  | TryWith(t1,t2) -> TryWith(tr.tr2_term env t1, tr.tr2_term env t2)
  | Tuple ts -> Tuple (List.map (tr.tr2_term env) ts)
  | Proj(i,t) -> Proj(i, tr.tr2_term env t)
  | Bottom -> Bottom
  | Label(info, t) -> Label(tr.tr2_info env info, tr.tr2_term env t)
  | Ref t -> Ref (tr.tr2_term env t)
  | Deref t -> Deref (tr.tr2_term env t)
  | SetRef(t1,t2) -> SetRef(tr.tr2_term env t1, tr.tr2_term env t2)
  | TNone -> TNone
  | TSome t -> TSome (tr.tr2_term env t)

let trans2_gen_term tr env t = {desc = tr.tr2_desc env t.desc; typ = tr.tr2_typ env t.typ; attr = t.attr}


let make_trans2 () =
  let id' env = Std.identity in
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
  | TUnit -> col.col_empty
  | TBool -> col.col_empty
  | TAbsBool -> col.col_empty
  | TInt -> col.col_empty
  | TRInt p -> col.col_empty
  | TVar{contents=None} -> col.col_empty
  | TVar{contents=Some typ} -> col.col_typ typ
  | TFun(x,typ) -> col.col_app (col.col_typ (Id.typ x)) (col.col_typ typ)
  | TList typ -> col.col_typ typ
  | TTuple xs -> List.fold_left (fun acc x -> col.col_app acc @@ col.col_var x) col.col_empty xs
  | TConstr(s,b) -> col.col_empty
  | TPred(x,ps) -> List.fold_left (fun acc p -> col.col_app acc @@ col.col_term p) (col.col_var x) ps
  | TRef typ -> col.col_typ typ
  | TOption typ -> col.col_typ typ

let col_var col x = col.col_typ (Id.typ x)

let col_pat col p =
  let r1 = col.col_typ p.pat_typ in
  let r2 =
    match p.pat_desc with
    | PAny -> col.col_empty
    | PVar x -> col.col_var x
    | PAlias(p,x) -> col.col_app (col.col_pat p) (col.col_var x)
    | PConst t -> col.col_term t
    | PConstruct(s,ps) -> List.fold_left (fun acc p -> col.col_app acc @@ col.col_pat p) col.col_empty ps
    | PNil -> col.col_empty
    | PCons(p1,p2) -> col.col_app (col.col_pat p1) (col.col_pat p2)
    | PTuple ps -> List.fold_left (fun acc p -> col.col_app acc @@ col.col_pat p) col.col_empty ps
    | PRecord pats -> List.fold_left (fun acc (i,(s,f,p)) -> col.col_app acc @@ col.col_pat p) col.col_empty pats
    | POr(p1,p2) -> col.col_app (col.col_pat p1) (col.col_pat p2)
    | PNone -> col.col_empty
    | PSome p -> col.col_pat p
  in
  col.col_app r1 r2

let col_info col = function
    InfoInt n -> col.col_empty
  | InfoString s -> col.col_empty
  | InfoId x -> col.col_var x
  | InfoTerm t -> col.col_term t
  | InfoIdTerm(x, t) -> col.col_app (col.col_var x) (col.col_term t)

let col_const col c =
  match c with
  | RandValue(typ,b) -> col.col_typ typ
  | _ -> col.col_empty

let col_desc col = function
  | Const c -> col.col_empty
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
  | Field(i,s,f,t1) -> col.col_term t1
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
  | Tuple ts -> List.fold_left (fun acc t -> col.col_app acc @@ col.col_term t) col.col_empty ts
  | Proj(i,t) -> col.col_term t
  | Bottom -> col.col_empty
  | Label(info, t) -> col.col_app (col.col_info info) (col.col_term t)
  | Ref t -> col.col_term t
  | Deref t -> col.col_term t
  | SetRef(t1,t2) -> col.col_app (col.col_term t1) (col.col_term t2)
  | TNone -> col.col_empty
  | TSome t -> col.col_term t

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
  | TUnit -> col.col2_empty
  | TBool -> col.col2_empty
  | TAbsBool -> col.col2_empty
  | TInt -> col.col2_empty
  | TRInt p -> col.col2_empty
  | TVar{contents=None} -> col.col2_empty
  | TVar{contents=Some typ} -> col.col2_typ env typ
  | TFun(x,typ) -> col.col2_app (col.col2_var env x) (col.col2_typ env typ)
  | TList typ -> col.col2_typ env typ
  | TTuple xs -> List.fold_left (fun acc x -> col.col2_app acc @@ col.col2_var env x) col.col2_empty xs
  | TConstr(s,b) -> col.col2_empty
  | TPred(x,ps) -> List.fold_left (fun acc p -> col.col2_app acc @@ col.col2_term env p) (col.col2_var env x) ps
  | TRef typ -> col.col2_typ env typ
  | TOption typ -> col.col2_typ env typ

let col2_var col env x = col.col2_typ env (Id.typ x)

let col2_pat col env p =
  let r1 = col.col2_typ env p.pat_typ in
  let r2 =
    match p.pat_desc with
    | PAny -> col.col2_empty
    | PVar x -> col.col2_var env x
    | PAlias(p,x) -> col.col2_app (col.col2_pat env p) (col.col2_var env x)
    | PConst t -> col.col2_term env t
    | PConstruct(s,ps) -> List.fold_left (fun acc p -> col.col2_app acc @@ col.col2_pat env p) col.col2_empty ps
    | PNil -> col.col2_empty
    | PCons(p1,p2) -> col.col2_app (col.col2_pat env p1) (col.col2_pat env p2)
    | PTuple ps -> List.fold_left (fun acc p -> col.col2_app acc @@ col.col2_pat env p) col.col2_empty ps
    | PRecord pats -> List.fold_left (fun acc (i,(s,f,p)) -> col.col2_app acc @@ col.col2_pat env p) col.col2_empty pats
    | POr(p1,p2) -> col.col2_app (col.col2_pat env p1) (col.col2_pat env p2)
    | PNone -> col.col2_empty
    | PSome p -> col.col2_pat env p
  in
  col.col2_app r1 r2

let col2_info col env = function
  | InfoInt n -> col.col2_empty
  | InfoString s -> col.col2_empty
  | InfoId x -> col.col2_var env x
  | InfoTerm t -> col.col2_term env t
  | InfoIdTerm(x, t) -> col.col2_app (col.col2_var env x) (col.col2_term env t)

let col2_const col env c =
  match c with
  | RandValue(typ,b) -> col.col2_typ env typ
  | _ -> col.col2_empty

let col2_desc col env = function
  | Const c -> col.col2_empty
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
  | Field(i,s,f,t1) -> col.col2_term env t1
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
  | Tuple ts -> List.fold_left (fun acc t -> col.col2_app acc @@ col.col2_term env t) col.col2_empty ts
  | Proj(i,t) -> col.col2_term env t
  | Bottom -> col.col2_empty
  | Label(info, t) -> col.col2_app (col.col2_info env info) (col.col2_term env t)
  | Ref t -> col.col2_term env t
  | Deref t -> col.col2_term env t
  | SetRef(t1,t2) -> col.col2_app (col.col2_term env t1) (col.col2_term env t2)
  | TNone -> col.col2_empty
  | TSome t -> col.col2_term env t

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




type ('a,'b) tr_col2 =
  {mutable tr_col2_term:      'b -> typed_term    -> 'a * typed_term;
   mutable tr_col2_term_rec:  'b -> typed_term    -> 'a * typed_term;
   mutable tr_col2_desc:      'b -> term          -> 'a * term;
   mutable tr_col2_desc_rec:  'b -> term          -> 'a * term;
   mutable tr_col2_typ:       'b -> typ           -> 'a * typ;
   mutable tr_col2_typ_rec:   'b -> typ           -> 'a * typ;
   mutable tr_col2_var:       'b -> id            -> 'a * id;
   mutable tr_col2_var_rec:   'b -> id            -> 'a * id;
   mutable tr_col2_pat:       'b -> typed_pattern -> 'a * typed_pattern;
   mutable tr_col2_pat_rec:   'b -> typed_pattern -> 'a * typed_pattern;
   mutable tr_col2_info:      'b -> info          -> 'a * info;
   mutable tr_col2_info_rec:  'b -> info          -> 'a * info;
   mutable tr_col2_const:     'b -> const         -> 'a * const;
   mutable tr_col2_const_rec: 'b -> const         -> 'a * const;
   mutable tr_col2_app: 'a -> 'a -> 'a;
   mutable tr_col2_empty: 'a}

let tr_col2_list tc tr_col ?(init=tc.tr_col2_empty) env xs =
  let aux x (acc,xs) =
    let acc',x' = tr_col env x in
    tc.tr_col2_app acc acc', x'::xs
  in
  List.fold_right aux xs (init,[])

let tr_col2_typ tc env = function
  | TUnit -> tc.tr_col2_empty, TUnit
  | TBool -> tc.tr_col2_empty, TBool
  | TAbsBool -> tc.tr_col2_empty, TAbsBool
  | TInt -> tc.tr_col2_empty, TInt
  | TRInt p ->
      let acc,p' = tc.tr_col2_term env p in
      acc, TRInt p'
  | TVar({contents=None} as x) -> tc.tr_col2_empty, TVar x
  | TVar{contents=Some typ} -> tc.tr_col2_typ env typ
  | TFun(x,typ) ->
      let acc1,x' = tc.tr_col2_var env x in
      let acc2,typ' = tc.tr_col2_typ env typ in
      tc.tr_col2_app acc1 acc2, TFun(x', typ')
  | TList typ ->
      let acc,typ' = tc.tr_col2_typ env typ in
      acc, TList typ'
  | TTuple xs ->
      let acc,xs' = tr_col2_list tc tc.tr_col2_var ~init:tc.tr_col2_empty env xs in
      acc, TTuple xs'
  | TConstr(s,b) -> tc.tr_col2_empty, TConstr(s,b)
  | TPred(x,ps) ->
      let acc,x' = tc.tr_col2_var env x in
      let acc',ps' = tr_col2_list tc tc.tr_col2_term ~init:acc env ps in
      acc', TPred(x',ps')
  | TRef typ ->
      let acc,typ' = tc.tr_col2_typ env typ in
      acc, TRef typ'
  | TOption typ ->
      let acc,typ' = tc.tr_col2_typ env typ in
      acc, TOption typ'

let tr_col2_var tc env x =
  let acc,typ' = tc.tr_col2_typ env (Id.typ x) in
  acc, Id.set_typ x typ'

let tr_col2_pat tc env p =
  let acc1,typ = tc.tr_col2_typ env p.pat_typ in
  let acc2,desc =
    match p.pat_desc with
    | PAny -> tc.tr_col2_empty, PAny
    | PVar x ->
        let acc,x' = tc.tr_col2_var env x in
        acc, PVar x'
    | PAlias(p,x) ->
        let acc1,p' = tc.tr_col2_pat env p in
        let acc2,x' = tc.tr_col2_var env x in
        tc.tr_col2_app acc1 acc2, PAlias(p,x')
    | PConst t ->
        let acc,t' = tc.tr_col2_term env t in
        acc, PConst t'
    | PConstruct(s,ps) ->
        let acc,ps' = tr_col2_list tc tc.tr_col2_pat env ps in
        acc, PConstruct(s,ps')
    | PNil -> tc.tr_col2_empty, PNil
    | PCons(p1,p2) ->
        let acc1,p1' = tc.tr_col2_pat env p1 in
        let acc2,p2' = tc.tr_col2_pat env p2 in
        tc.tr_col2_app acc1 acc2, PCons(p1', p2')
    | PTuple ps ->
        let acc,ps' = tr_col2_list tc tc.tr_col2_pat env ps in
        acc, PTuple ps'
    | PRecord pats ->
        let aux env (i,(s,f,p)) =
          let acc',p' = tc.tr_col2_pat env p in
          acc', (i,(s,f,p'))
        in
        let acc,pats' = tr_col2_list tc aux env pats in
        acc, PRecord pats'
    | POr(p1,p2) ->
        let acc1,p1' = tc.tr_col2_pat env p1 in
        let acc2,p2' = tc.tr_col2_pat env p2 in
        tc.tr_col2_app acc1 acc2, POr(p1', p2')
    | PNone -> tc.tr_col2_empty, PNone
    | PSome p ->
        let acc,p' = tc.tr_col2_pat env p in
        acc, PSome p'
  in
  tc.tr_col2_app acc1 acc2, {pat_desc=desc; pat_typ=typ}

let tr_col2_info tc env = function
  | InfoInt n -> tc.tr_col2_empty, InfoInt n
  | InfoString s -> tc.tr_col2_empty, InfoString s
  | InfoId x ->
      let acc,x' = tc.tr_col2_var env x in
      acc, InfoId x'
  | InfoTerm t ->
      let acc,t' = tc.tr_col2_term env t in
      acc, InfoTerm t'
  | InfoIdTerm(x,t) ->
      let acc1,x' = tc.tr_col2_var env x in
      let acc2,t' = tc.tr_col2_term env t in
      tc.tr_col2_app acc1 acc2, InfoIdTerm(x',t')

let tr_col2_const tc env c =
  match c with
  | RandValue(typ,b) ->
      let acc,typ' = tc.tr_col2_typ env typ in
      acc, RandValue(typ',b)
  | _ -> tc.tr_col2_empty, c

let tr_col2_desc tc env = function
  | Const c -> tc.tr_col2_empty, Const c
  | Var y ->
      let acc,y' = tc.tr_col2_var env y in
      acc, Var y'
  | Fun(y, t) ->
      let acc1,y' = tc.tr_col2_var env y in
      let acc2,t' = tc.tr_col2_term env t in
      tc.tr_col2_app acc1 acc2, Fun(y',t')
  | App(t1, ts) ->
      let acc,t1' = tc.tr_col2_term env t1 in
      let acc',ts' = tr_col2_list tc tc.tr_col2_term ~init:acc env ts in
      acc', App(t1', ts')
  | If(t1, t2, t3) ->
      let acc1,t1' = tc.tr_col2_term env t1 in
      let acc2,t2' = tc.tr_col2_term env t2 in
      let acc3,t3' = tc.tr_col2_term env t3 in
      tc.tr_col2_app acc1 @@ tc.tr_col2_app acc2 acc3, If(t1',t2',t3')
  | Branch(t1, t2) ->
      let acc1,t1' = tc.tr_col2_term env t1 in
      let acc2,t2' = tc.tr_col2_term env t2 in
      tc.tr_col2_app acc1 acc2, Branch(t1',t2')
  | Let(flag, bindings, t2) ->
      let aux env (f,xs,t) =
        let acc1,f' = tc.tr_col2_var env f in
        let acc1',xs' = tr_col2_list tc tc.tr_col2_var ~init:acc1 env xs in
        let acc2,t' = tc.tr_col2_term env t in
        tc.tr_col2_app acc1' acc2, (f',xs',t')
      in
      let acc1,bindings' = tr_col2_list tc aux env bindings in
      let acc2,t2' = tc.tr_col2_term env t2 in
      tc.tr_col2_app acc1 acc2, Let(flag,bindings',t2')
  | BinOp(op, t1, t2) ->
      let acc1,t1' = tc.tr_col2_term env t1 in
      let acc2,t2' = tc.tr_col2_term env t2 in
      tc.tr_col2_app acc1 acc2, BinOp(op,t1',t2')
  | Not t1 ->
      let acc,t1' = tc.tr_col2_term env t1 in
      acc, Not t1'
  | Event(s,b) -> tc.tr_col2_empty, Event(s,b)
  | Record fields ->
      let aux env (f,(s,t1)) =
        let acc,t1' = tc.tr_col2_term env t1 in
        acc, (f,(s,t1'))
      in
      let acc,fields' = tr_col2_list tc aux env fields in
      acc, Record fields'
  | Field(i,s,f,t1) ->
      let acc,t1' = tc.tr_col2_term env t1 in
      acc, Field(i,s,f,t1')
  | SetField(n,i,s,f,t1,t2) ->
      let acc1,t1' = tc.tr_col2_term env t1 in
      let acc2,t2' = tc.tr_col2_term env t2 in
      tc.tr_col2_app acc1 acc2, SetField(n,i,s,f,t1',t2')
  | Nil -> tc.tr_col2_empty, Nil
  | Cons(t1,t2) ->
      let acc1,t1' = tc.tr_col2_term env t1 in
      let acc2,t2' = tc.tr_col2_term env t2 in
      tc.tr_col2_app acc1 acc2, Cons(t1',t2')
  | Constr(s,ts) ->
      let acc,ts' = tr_col2_list tc tc.tr_col2_term env ts in
      acc, Constr(s,ts')
  | Match(t1,pats) ->
      let aux env (pat,cond,t1) =
        let acc1,pat' = tc.tr_col2_pat env pat in
        let acc2,cond' = tc.tr_col2_term env cond in
        let acc3,t1' = tc.tr_col2_term env t1 in
        tc.tr_col2_app acc1 @@ tc.tr_col2_app acc2 acc3, (pat',cond',t1')
      in
      let acc,t1' = tc.tr_col2_term env t1 in
      let acc',pats' = tr_col2_list tc aux ~init:acc env pats in
      acc', Match(t1',pats')
  | Raise t ->
      let acc,t' = tc.tr_col2_term env t in
      acc, Raise t'
  | TryWith(t1,t2) ->
      let acc1,t1' = tc.tr_col2_term env t1 in
      let acc2,t2' = tc.tr_col2_term env t2 in
      tc.tr_col2_app acc1 acc2, TryWith(t1',t2')
  | Tuple ts ->
      let acc,ts' = tr_col2_list tc tc.tr_col2_term env ts in
      acc, Tuple ts'
  | Proj(i,t) ->
      let acc,t' = tc.tr_col2_term env t in
      acc, Proj(i,t')
  | Bottom -> tc.tr_col2_empty, Bottom
  | Label(info, t) ->
      let acc1,t' = tc.tr_col2_term env t in
      let acc2,info' = tc.tr_col2_info env info in
      tc.tr_col2_app acc1 acc2, Label(info',t')
  | Ref t ->
      let acc,t' = tc.tr_col2_term env t in
      acc, Ref t'
  | Deref t ->
      let acc,t' = tc.tr_col2_term env t in
      acc, Deref t'
  | SetRef(t1,t2) ->
      let acc1,t1' = tc.tr_col2_term env t1 in
      let acc2,t2' = tc.tr_col2_term env t2 in
      tc.tr_col2_app acc1 acc2, SetRef(t1',t2')
  | TNone -> tc.tr_col2_empty, TNone
  | TSome t ->
      let acc,t' = tc.tr_col2_term env t in
      acc, TSome t'

let tr_col2_term tc env t =
  let acc1,desc' = tc.tr_col2_desc env t.desc in
  let acc2,typ' = tc.tr_col2_typ env t.typ in
  tc.tr_col2_app acc1 acc2, {desc=desc'; typ=typ'; attr=t.attr}


let make_tr_col2 empty app =
  let f _ x = empty, x in
  let tc =
    {tr_col2_term = f;
     tr_col2_term_rec = f;
     tr_col2_desc = f;
     tr_col2_desc_rec = f;
     tr_col2_typ = f;
     tr_col2_typ_rec = f;
     tr_col2_var = f;
     tr_col2_var_rec = f;
     tr_col2_pat = f;
     tr_col2_pat_rec = f;
     tr_col2_info = f;
     tr_col2_info_rec = f;
     tr_col2_const = f;
     tr_col2_const_rec = f;
     tr_col2_app = app;
     tr_col2_empty = empty}
  in
  tc.tr_col2_term <- tr_col2_term tc;
  tc.tr_col2_term_rec <- tr_col2_term tc;
  tc.tr_col2_desc <- tr_col2_desc tc;
  tc.tr_col2_desc_rec <- tr_col2_desc tc;
  tc.tr_col2_typ <- tr_col2_typ tc;
  tc.tr_col2_typ_rec <- tr_col2_typ tc;
  tc.tr_col2_var <- tr_col2_var tc;
  tc.tr_col2_var_rec <- tr_col2_var tc;
  tc.tr_col2_pat <- tr_col2_pat tc;
  tc.tr_col2_pat_rec <- tr_col2_pat tc;
  tc.tr_col2_info <- tr_col2_info tc;
  tc.tr_col2_info_rec <- tr_col2_info tc;
  tc.tr_col2_const <- tr_col2_const tc;
  tc.tr_col2_const_rec <- tr_col2_const tc;
  tc







type 'a fold_tr =
  {mutable fold_tr_term:      'a -> typed_term    -> 'a * typed_term;
   mutable fold_tr_term_rec:  'a -> typed_term    -> 'a * typed_term;
   mutable fold_tr_desc:      'a -> term          -> 'a * term;
   mutable fold_tr_desc_rec:  'a -> term          -> 'a * term;
   mutable fold_tr_typ:       'a -> typ           -> 'a * typ;
   mutable fold_tr_typ_rec:   'a -> typ           -> 'a * typ;
   mutable fold_tr_var:       'a -> id            -> 'a * id;
   mutable fold_tr_var_rec:   'a -> id            -> 'a * id;
   mutable fold_tr_pat:       'a -> typed_pattern -> 'a * typed_pattern;
   mutable fold_tr_pat_rec:   'a -> typed_pattern -> 'a * typed_pattern;
   mutable fold_tr_info:      'a -> info          -> 'a * info;
   mutable fold_tr_info_rec:  'a -> info          -> 'a * info;
   mutable fold_tr_const:     'a -> const         -> 'a * const;
   mutable fold_tr_const_rec: 'a -> const         -> 'a * const}

let fold_tr_list fld tr_col env xs =
  let aux x (env,xs) =
    let env',x' = tr_col env x in
    env', x'::xs
  in
  List.fold_right aux xs (env,[])

let fold_tr_typ fld env = function
  | TUnit -> env, TUnit
  | TBool -> env, TBool
  | TAbsBool -> env, TAbsBool
  | TInt -> env, TInt
  | TRInt p ->
      let env',p' = fld.fold_tr_term env p in
      env', TRInt p'
  | TVar({contents=None} as x) -> env, TVar x
  | TVar{contents=Some typ} -> fld.fold_tr_typ env typ
  | TFun(x,typ) ->
      let env',x' = fld.fold_tr_var env x in
      let env'',typ' = fld.fold_tr_typ env' typ in
      env'', TFun(x', typ')
  | TList typ ->
      let env',typ' = fld.fold_tr_typ env typ in
      env', TList typ'
  | TTuple xs ->
      let env',xs' = fold_tr_list fld fld.fold_tr_var env xs in
      env', TTuple xs'
  | TConstr(s,b) -> env, TConstr(s,b)
  | TPred(x,ps) ->
      let env',x' = fld.fold_tr_var env x in
      let env'',ps' = fold_tr_list fld fld.fold_tr_term env' ps in
      env'', TPred(x',ps')
  | TRef typ ->
      let env',typ' = fld.fold_tr_typ env typ in
      env', TRef typ'
  | TOption typ ->
      let env',typ' = fld.fold_tr_typ env typ in
      env', TOption typ'

let fold_tr_var fld env x =
  let env',typ' = fld.fold_tr_typ env (Id.typ x) in
  env', Id.set_typ x typ'

let fold_tr_pat fld env p =
  let env',typ = fld.fold_tr_typ env p.pat_typ in
  let env'',desc =
    match p.pat_desc with
    | PAny -> env', PAny
    | PVar x ->
        let env'',x' = fld.fold_tr_var env' x in
        env'', PVar x'
    | PAlias(p,x) ->
        let env'',p' = fld.fold_tr_pat env' p in
        let env''',x' = fld.fold_tr_var env'' x in
        env''', PAlias(p,x')
    | PConst t ->
        let env'',t' = fld.fold_tr_term env' t in
        env'', PConst t'
    | PConstruct(s,ps) ->
        let env'',ps' = fold_tr_list fld fld.fold_tr_pat env ps in
        env'', PConstruct(s,ps')
    | PNil -> env', PNil
    | PCons(p1,p2) ->
        let env'',p1' = fld.fold_tr_pat env' p1 in
        let env''',p2' = fld.fold_tr_pat env'' p2 in
        env''', PCons(p1', p2')
    | PTuple ps ->
        let env'',ps' = fold_tr_list fld fld.fold_tr_pat env' ps in
        env'', PTuple ps'
    | PRecord pats ->
        let aux env (i,(s,f,p)) =
          let env',p' = fld.fold_tr_pat env p in
          env', (i,(s,f,p'))
        in
        let env'',pats' = fold_tr_list fld aux env' pats in
        env'', PRecord pats'
    | POr(p1,p2) ->
        let env'',p1' = fld.fold_tr_pat env' p1 in
        let env''',p2' = fld.fold_tr_pat env'' p2 in
        env''', POr(p1', p2')
    | PNone -> env', PNone
    | PSome p ->
        let env'',p' = fld.fold_tr_pat env' p in
        env'', PSome p'
  in
  env'', {pat_desc=desc; pat_typ=typ}

let fold_tr_info fld env = function
  | InfoInt n -> env, InfoInt n
  | InfoString s -> env, InfoString s
  | InfoId x ->
      let env',x' = fld.fold_tr_var env x in
      env', InfoId x'
  | InfoTerm t ->
      let env',t' = fld.fold_tr_term env t in
      env', InfoTerm t'
  | InfoIdTerm(x,t) ->
      let env',x' = fld.fold_tr_var env x in
      let env'',t' = fld.fold_tr_term env' t in
      env'', InfoIdTerm(x',t')

let fold_tr_const fld env c =
  match c with
  | RandValue(typ,b) ->
      let env',typ' = fld.fold_tr_typ env typ in
      env', RandValue(typ',b)
  | _ -> env, c

let fold_tr_desc fld env = function
  | Const c -> env, Const c
  | Var y ->
      let env',y' = fld.fold_tr_var env y in
      env', Var y'
  | Fun(y, t) ->
      let env',y' = fld.fold_tr_var env y in
      let env'',t' = fld.fold_tr_term env' t in
      env'', Fun(y',t')
  | App(t1, ts) ->
      let env',t1' = fld.fold_tr_term env t1 in
      let env'',ts' = fold_tr_list fld fld.fold_tr_term env' ts in
      env'', App(t1', ts')
  | If(t1, t2, t3) ->
      let env',t1' = fld.fold_tr_term env t1 in
      let env'',t2' = fld.fold_tr_term env' t2 in
      let env''',t3' = fld.fold_tr_term env'' t3 in
      env''', If(t1',t2',t3')
  | Branch(t1, t2) ->
      let env',t1' = fld.fold_tr_term env t1 in
      let env'',t2' = fld.fold_tr_term env' t2 in
      env'', Branch(t1',t2')
  | Let(flag, bindings, t2) ->
      let aux env (f,xs,t) =
        let env',f' = fld.fold_tr_var env f in
        let env'',xs' = fold_tr_list fld fld.fold_tr_var env' xs in
        let env''',t' = fld.fold_tr_term env'' t in
        env''', (f',xs',t')
      in
      let env',bindings' = fold_tr_list fld aux env bindings in
      let env'',t2' = fld.fold_tr_term env' t2 in
      env'', Let(flag,bindings',t2')
  | BinOp(op, t1, t2) ->
      let env',t1' = fld.fold_tr_term env t1 in
      let env'',t2' = fld.fold_tr_term env' t2 in
      env'', BinOp(op,t1',t2')
  | Not t1 ->
      let env',t1' = fld.fold_tr_term env t1 in
      env', Not t1'
  | Event(s,b) -> env, Event(s,b)
  | Record fields ->
      let aux env (f,(s,t1)) =
        let env',t1' = fld.fold_tr_term env t1 in
        env', (f,(s,t1'))
      in
      let env',fields' = fold_tr_list fld aux env fields in
      env', Record fields'
  | Field(i,s,f,t1) ->
      let env',t1' = fld.fold_tr_term env t1 in
      env', Field(i,s,f,t1')
  | SetField(n,i,s,f,t1,t2) ->
      let env',t1' = fld.fold_tr_term env t1 in
      let env'',t2' = fld.fold_tr_term env' t2 in
      env'', SetField(n,i,s,f,t1',t2')
  | Nil -> env, Nil
  | Cons(t1,t2) ->
      let env',t1' = fld.fold_tr_term env t1 in
      let env'',t2' = fld.fold_tr_term env' t2 in
      env'', Cons(t1',t2')
  | Constr(s,ts) ->
      let env',ts' = fold_tr_list fld fld.fold_tr_term env ts in
      env', Constr(s,ts')
  | Match(t1,pats) ->
      let aux env (pat,cond,t1) =
        let env',pat' = fld.fold_tr_pat env pat in
        let env'',cond' = fld.fold_tr_term env' cond in
        let env''',t1' = fld.fold_tr_term env'' t1 in
        env''', (pat',cond',t1')
      in
      let env',t1' = fld.fold_tr_term env t1 in
      let env'',pats' = fold_tr_list fld aux env' pats in
      env'', Match(t1',pats')
  | Raise t ->
      let env',t' = fld.fold_tr_term env t in
      env', Raise t'
  | TryWith(t1,t2) ->
      let env',t1' = fld.fold_tr_term env t1 in
      let env'',t2' = fld.fold_tr_term env' t2 in
      env'', TryWith(t1',t2')
  | Tuple ts ->
      let env',ts' = fold_tr_list fld fld.fold_tr_term env ts in
      env', Tuple ts'
  | Proj(i,t) ->
      let env',t' = fld.fold_tr_term env t in
      env', Proj(i,t')
  | Bottom -> env, Bottom
  | Label(info, t) ->
      let env',t' = fld.fold_tr_term env t in
      let env'',info' = fld.fold_tr_info env' info in
      env'', Label(info',t')
  | Ref t ->
      let env',t' = fld.fold_tr_term env t in
      env', Ref t'
  | Deref t ->
      let env',t' = fld.fold_tr_term env t in
      env', Deref t'
  | SetRef(t1,t2) ->
      let env',t1' = fld.fold_tr_term env t1 in
      let env'',t2' = fld.fold_tr_term env' t2 in
      env'', SetRef(t1',t2')
  | TNone -> env, TNone
  | TSome t ->
      let env',t' = fld.fold_tr_term env t in
      env', TSome t'

let fold_tr_term fld env t =
  let env',desc' = fld.fold_tr_desc env t.desc in
  let env'',typ' = fld.fold_tr_typ env' t.typ in
  env'', {desc=desc'; typ=typ'; attr=t.attr}


let make_fold_tr () =
  let f env x = env, x in
  let fld =
    {fold_tr_term = f;
     fold_tr_term_rec = f;
     fold_tr_desc = f;
     fold_tr_desc_rec = f;
     fold_tr_typ = f;
     fold_tr_typ_rec = f;
     fold_tr_var = f;
     fold_tr_var_rec = f;
     fold_tr_pat = f;
     fold_tr_pat_rec = f;
     fold_tr_info = f;
     fold_tr_info_rec = f;
     fold_tr_const = f;
     fold_tr_const_rec = f}
  in
  fld.fold_tr_term <- fold_tr_term fld;
  fld.fold_tr_term_rec <- fold_tr_term fld;
  fld.fold_tr_desc <- fold_tr_desc fld;
  fld.fold_tr_desc_rec <- fold_tr_desc fld;
  fld.fold_tr_typ <- fold_tr_typ fld;
  fld.fold_tr_typ_rec <- fold_tr_typ fld;
  fld.fold_tr_var <- fold_tr_var fld;
  fld.fold_tr_var_rec <- fold_tr_var fld;
  fld.fold_tr_pat <- fold_tr_pat fld;
  fld.fold_tr_pat_rec <- fold_tr_pat fld;
  fld.fold_tr_info <- fold_tr_info fld;
  fld.fold_tr_info_rec <- fold_tr_info fld;
  fld.fold_tr_const <- fold_tr_const fld;
  fld.fold_tr_const_rec <- fold_tr_const fld;
  fld







let rec get_vars_pat pat =
  match pat.pat_desc with
  | PAny -> []
  | PVar x -> [x]
  | PAlias(p,x) -> x :: get_vars_pat p
  | PConst _ -> []
  | PConstruct(_,pats) -> List.fold_left (fun acc pat -> get_vars_pat pat @@@ acc) [] pats
  | PRecord pats -> List.fold_left (fun acc (_,(_,_,pat)) -> get_vars_pat pat @@@ acc) [] pats
  | POr(p1,p2) -> get_vars_pat p1 @@@ get_vars_pat p2
  | PTuple ps -> List.fold_left (fun acc p -> get_vars_pat p @@@ acc) [] ps
  | PNil -> []
  | PCons(p1,p2) -> get_vars_pat p1 @@@ get_vars_pat p2
  | PNone -> []
  | PSome p -> get_vars_pat p

let get_fv = make_col2 [] (@@@)

let get_fv_typ vars typ = []
(*
  match typ with
  | TPred(x, preds) -> List.fold_left (fun acc t -> get_fv.col2_term (x::vars) t @@@ acc) preds
  | TFun(x, typ) -> ...
  | TTuple xs -> ...
  | _ -> get_fv.col2_typ_rec vars typ
*)

let get_fv_term vars t =
  match t.desc with
  | Var x -> if Id.mem x vars then [] else [x]
  | Let(flag, bindings, t2) ->
      let vars_with_fun = List.fold_left (fun vars (f,_,_) -> f::vars) vars bindings in
      let vars' = match flag with Nonrecursive -> vars | Recursive -> vars_with_fun in
      let aux fv (_,xs,t) = get_fv.col2_term (xs@@@vars') t @@@ fv in
      let fv_t2 = get_fv.col2_term vars_with_fun t2 in
      List.fold_left aux fv_t2 bindings
  | Fun(x,t) -> get_fv.col2_term (x::vars) t
  | Match(t,pats) ->
      let aux acc (pat,cond,t) =
        let vars' = get_vars_pat pat @@@ vars in
        get_fv.col2_term vars' cond @@@ get_fv.col2_term vars' t @@@ acc
      in
      List.fold_left aux (get_fv.col2_term vars t) pats
  | _ -> get_fv.col2_term_rec vars t

let () = get_fv.col2_term <- get_fv_term
let () = get_fv.col2_typ <- get_fv_typ
let get_fv ?(cmp=Id.same) t =
  List.unique ~cmp @@ get_fv.col2_term [] t



let occur = make_col2 false (||)

let occur_typ x typ =
  match typ with
  | TPred(y,ps) -> List.exists (fun p -> List.exists (Id.same x) (get_fv p)) ps || occur.col2_var x y
  | _ -> occur.col2_typ_rec x typ

let () = occur.col2_typ <- occur_typ
let occur = occur.col2_typ



(*** PRINTING FUNCTIONS ***)

let rec print_typ fm t = Type.print ~occur (print_term 0 false) fm t
and print_ids typ fm xs =
  if !Flag.web then
    let rec aux xs =
      match xs with
	[] -> ()
      | [x] ->
	  fprintf fm "%a" print_id x
      | x1 :: x2 :: xs ->
	  let _ =
	    if is_fun_typ (Id.typ x2) then
	      fprintf fm "$%a$ " print_id x1
	    else
	      fprintf fm "%a " print_id x1
	  in
	  aux (x2 :: xs)
    in
    aux xs
  else
    if xs <> []
    then
      let p_id = if typ then print_id_typ else print_id in
      print_list p_id "@ " ~first:true fm xs

(*
  and print_id fm x = fprintf fm "(%a:%a)" Id.print x print_typ (Id.typ x)
 *)
and print_id = Id.print

and print_id_typ fm x = fprintf fm "(%a:%a)" print_id x (Color.cyan print_typ) (Id.typ x)

(* priority (low -> high)
   10 : Let, Letrec, If, Match, TryWith
   15 : Fun
   20 : Pair
   30 : Or
   40 : And
   50 : Eq, Lt, Gt, Leq, Geq
   60 : Add, Sub
   70 : Cons, Raise
   80 : App, Ref, SetRef
   90 : Deref
 *)

and paren pri p = if pri < p then "","" else "(",")"

and print_binop fm = function
    Eq -> fprintf fm "="
  | Lt -> fprintf fm "<"
  | Gt -> fprintf fm ">"
  | Leq -> fprintf fm "<="
  | Geq -> fprintf fm ">="
  | And -> fprintf fm "&&"
  | Or -> fprintf fm "||"
  | Add -> fprintf fm "+"
  | Sub -> fprintf fm "-"
  | Mult -> fprintf fm "*"

and print_termlist pri typ fm ts =
  if !Flag.web then
    let rec aux ts =
      match ts with
      | [] -> ()
      | [t] -> fprintf fm "@ %a" (print_term pri typ) t
      | t1 :: t2 :: ts' ->
          let _ =
            if is_fun_typ t2.typ then
              fprintf fm "@ $%a$" (print_term pri typ) t1
            else
              fprintf fm "@ %a" (print_term pri typ) t1
          in
          aux (t2 :: ts')
    in
    aux ts
  else
    List.iter (fprintf fm "@ %a" (print_term pri typ)) ts
and print_const fm = function
  | Unit -> fprintf fm "()"
  | True -> fprintf fm "true"
  | False -> fprintf fm "false"
  | Int n when n<0 -> fprintf fm "(%d)" n
  | Int n -> fprintf fm "%d" n
  | Char c -> fprintf fm "%C" c
  | String s -> fprintf fm "%S" s
  | Float s -> fprintf fm "%s" s
  | Int32 n -> fprintf fm "%ldl" n
  | Int64 n -> fprintf fm "%LdL" n
  | Nativeint n -> fprintf fm "%ndn" n
  | CPS_result -> fprintf fm "{end}"
  | RandInt false -> fprintf fm "rand_int"
  | RandInt true -> fprintf fm "rand_int_cps"
  | RandValue(typ',false) -> fprintf fm "rand_val[%a]" print_typ typ'
  | RandValue(typ',true) -> fprintf fm "rand_val_cps[%a]" print_typ typ'

and print_attr fm = function
  | ANone -> Format.fprintf fm "ANone"
  | AAbst_under -> Format.fprintf fm "AAbst_under"

and print_term pri typ fm t =
  if t.attr = ANone
  then print_desc pri typ fm t.desc
  else Format.fprintf fm "(@[%a@ #@ %a@])" (print_desc pri typ) t.desc print_attr t.attr

and print_desc pri typ fm desc =
  match desc with
  | Const c -> print_const fm c
  | Var x -> print_id fm x
  | Fun(x, t) ->
      let p = 15 in
      let s1,s2 = paren pri (p+1) in
      let p_id = if typ then print_id_typ else print_id in
      fprintf fm "%s@[<hov 2>fun %a ->@ %a%s@]" s1 p_id x (print_term p typ) t s2
  | App(t, ts) ->
      let p = 80 in
      let s1,s2 = paren pri p in
      fprintf fm "@[<hov 2>%s%a%a%s@]" s1 (print_term p typ) t (print_termlist p typ) ts s2
  | If(t1, t2, t3) ->
      let p = 10 in
      let s1,s2 = paren pri (p+1) in
      fprintf fm "%s@[<v>if %a then@   @[%a@]@ else@   @[%a@]@]%s"
              s1 (print_term p typ) t1 (print_term p typ) t2 (print_term p typ) t3 s2
  | Branch(t1, t2) ->
      let p = 80 in
      let s1,s2 = paren pri p in
      fprintf fm "@[%sbr %a %a%s@]" s1 (print_term p typ) t1 (print_term p typ) t2 s2
  | Let(_, [], _) -> assert false
  | Let(flag, bindings, t2) ->
      let p = 10 in
      let s1,s2 = paren pri (p+1) in
      let s_rec = match flag with Nonrecursive -> "" | Recursive -> " rec" in
      let b = ref true in
      let print_binding fm (f,xs,t1) =
        let pre = if !b then "let" ^ s_rec else "and" in
        fprintf fm "@[<hov 2>%s@ %a%a =@ %a@]" pre print_id f (print_ids typ) xs (print_term 0 typ) t1;
        b := false
      in
      let print_bindings bs = print_list print_binding "" bs in
      fprintf fm "%s@[<v>@[<hv>%a@ @]in@ @[<hov>%a@]@]%s" s1 print_bindings bindings (print_term p typ) t2 s2
  | Not{desc = BinOp(Eq, t1, t2)} ->
      let p = 50 in
      let s1,s2 = paren pri p in
      fprintf fm "%s@[%a@ <>@ %a@]%s" s1 (print_term p typ) t1 (print_term p typ) t2 s2
  | BinOp(Mult, {desc=Const (Int -1)}, {desc=Var x}) ->
      let p = 60 in
      let s1,s2 = paren pri p in
      fprintf fm "%s@[-%a@]%s" s1 print_id x s2
  | BinOp(op, t1, t2) ->
      let p = match op with Add|Sub|Mult -> 60 | And -> 40 | Or -> 30 | _ -> 50 in
      let s1,s2 = paren pri p in
      fprintf fm "%s@[%a@ %a@ %a@]%s" s1 (print_term p typ) t1 print_binop op (print_term p typ) t2 s2
  | Not t ->
      let p = 60 in
      let s1,s2 = paren pri p in
      fprintf fm "%s@[not@ %a@]%s" s1 (print_term p typ) t s2
  | Event(s,false) -> fprintf fm "{%s}" s
  | Event(s,true) -> fprintf fm "{|%s|}" s
  | Record fields ->
      let rec aux fm = function
        | [] -> ()
        | (s,(f,t))::fields ->
            if fields=[]
            then fprintf fm "%s=%a" s (print_term 0 typ) t
            else fprintf fm "%s=%a;@ %a" s (print_term 0 typ) t aux fields
      in
      fprintf fm "{%a}" aux fields
  | Field(_,s,_,t) -> fprintf fm "%a.%s" (print_term 9 typ) t s
  | SetField(_,_,s,_,t1,t2) -> fprintf fm "%a.%s@ <-@ %a" (print_term 9 typ) t1 s (print_term 3 typ) t2
  | Nil -> fprintf fm "[]"
  | Cons(t1,t2) ->
      let p = 70 in
      let s1,s2 = paren pri p in
      fprintf fm "%s@[%a::@,%a@]%s" s1 (print_term p typ) t1 (print_term p typ) t2 s2
  | Constr(s,ts) ->
      let p = 80 in
      let s1,s2 = paren pri p in
      if ts = []
      then pp_print_string fm s
      else fprintf fm "%s@[%s(%a)@]%s" s1 s (print_list (print_term 20 typ) ",") ts s2
  | Match(t,pats) ->
      let p = 10 in
      let s1,s2 = paren pri p in
      let aux (pat,cond,t) =
        let print_cond fm =
          match cond.desc with
          | Const True -> ()
          | _ -> fprintf fm "when@ @[<hov 2>%a@]@ " (print_term p typ) cond
        in
        fprintf fm "@ @[<hov 4>| @[<hov 2>%a %t->@ %a@]@]" print_pattern pat print_cond (print_term p typ) t
      in
      fprintf fm "%s@[<v>match @[%a@] with" s1 (print_term p typ) t;
      List.iter aux pats;
      fprintf fm "@]%s" s2
  | Raise t ->
      let p = 70 in
      let s1,s2 = paren pri p in
      fprintf fm "%s@[raise@ %a@]%s" s1 (print_term p typ) t s2
  | TryWith(t1,t2) ->
      let p = 10 in
      let s1,s2 = paren pri (p+1) in
      fprintf fm "%s@[try@ %a@ with@ %a@]%s" s1 (print_term p typ) t1 (print_term p typ) t2 s2
  | Tuple ts ->
      let p = 20 in
      fprintf fm "@[(%a)@]" (print_list (print_term p typ) ",@ ") ts
  | Proj(0,t) when tuple_num t.typ = Some 2 ->
      let p = 80 in
      let s1,s2 = paren pri p in
      fprintf fm "%s@[fst@ %a@]%s" s1 (print_term p typ) t s2
  | Proj(1,t) when tuple_num t.typ = Some 2 ->
      let p = 80 in
      let s1,s2 = paren pri p in
      fprintf fm "%s@[snd@ %a@]%s" s1 (print_term p typ) t s2
  | Proj(i,t) ->
      let p = 80 in
      let s1,s2 = paren pri p in
      fprintf fm "%s@[#%d@ %a@]%s" s1 i (print_term p typ) t s2
  | Bottom -> fprintf fm "_|_"
  | Label(info, t) ->
      fprintf fm "(@[label[@[%a@]]@ %a@])" print_info info (print_term 80 typ) t
  | Ref t ->
      let p = 80 in
      let s1,s2 = paren pri p in
      fprintf fm "%s@[ref@ %a@]%s" s1 (print_term p typ) t s2
  | Deref t ->
      let p = 90 in
      let s1,s2 = paren pri p in
      fprintf fm "%s@[!%a@]%s" s1 (print_term p typ) t s2
  | SetRef(t1, t2) ->
      let p = 80 in
      let s1,s2 = paren pri p in
      fprintf fm "%s@[%a := %a@]%s" s1 (print_term p typ) t1 (print_term p typ) t2 s2
  | TNone -> fprintf fm "None"
  | TSome t ->
      let p = 80 in
      let s1,s2 = paren pri p in
      fprintf fm "%s@[Ref %a@]%s" s1 (print_term p typ) t s2

and print_info fm info =
  match info with
  | InfoId x ->
      fprintf fm "Id %a" print_id x
  | InfoString s ->
      fprintf fm "String %S" s
  | InfoInt n ->
      fprintf fm "Int %d" n
  | InfoTerm t ->
      fprintf fm "Term %a" (print_term 80 false) t
  | InfoIdTerm(x,t) ->
      fprintf fm "IdTerm(%a,@ %a)" print_id x (print_term 80 false) t



and print_pattern fm pat =
  match pat.pat_desc with
  | PAny -> pp_print_string fm "_"
  | PVar x -> print_id fm x
  | PAlias(p,x) -> fprintf fm "(%a as %a)" print_pattern p print_id x
  | PConst c -> print_term 1 false fm c
  | PConstruct(c,pats) ->
      let aux' = function
          [] -> ()
        | [pat] -> fprintf fm "(%a)" print_pattern pat
        | pat::pats ->
            fprintf fm "(%a" print_pattern pat;
            List.iter (fun pat -> fprintf fm ",%a" print_pattern pat) pats;
            pp_print_string fm ")"
      in
      pp_print_string fm c;
      aux' pats
  | PNil -> fprintf fm "[]"
  | PCons(p1,p2) -> fprintf fm "%a::%a" print_pattern p1 print_pattern p2
  | PRecord pats ->
      let aux' = function
          [] -> ()
        | [_,(_,_,pat)] -> fprintf fm "(%a)" print_pattern pat
        | (_,(_,_,pat))::pats ->
            fprintf fm "(%a" print_pattern pat;
            List.iter (fun (_,(_,_,pat)) -> fprintf fm ",%a" print_pattern pat) pats;
            pp_print_string fm ")"
      in
      aux' pats
  | POr(pat1,pat2) -> fprintf fm "(%a | %a)" print_pattern pat1 print_pattern pat2
  | PTuple pats -> fprintf fm "(%a)" (print_list print_pattern ", ") pats
  | PNone -> fprintf fm "None"
  | PSome p -> fprintf fm "(Some %a)" print_pattern p
let print_term typ fm = print_term 0 typ fm

let rec print_term' pri fm t =
  fprintf fm "(";(
    match t.desc with
    | Const c -> print_const fm c
    | Var x when t.typ = Id.typ x -> print_id fm x
    | Var x -> print_id_typ fm x
    | Fun(x, t) ->
        let p = 2 in
        let s1,s2 = paren pri p in
        fprintf fm "%sfun %a -> %a%s" s1 print_id x (print_term' p) t s2
    | App(t, ts) ->
        let p = 8 in
        let s1,s2 = paren pri p in
        fprintf fm "%s%a%a%s" s1 (print_term' p) t (print_termlist' p) ts s2
    | If(t1, t2, t3) ->
        let p = 1 in
        let s1,s2 = paren pri (p+1) in
        fprintf fm "%s@[@[if %a@]@ then @[%a@]@ else @[%a@]@]%s"
                s1 (print_term' p) t1 (print_term' p) t2 (print_term' p) t3 s2
    | Branch(t1, t2) ->
        let p = 8 in
        let s1,s2 = paren pri p in
        fprintf fm "%sbr %a %a%s" s1 (print_term' p) t1 (print_term' p) t2 s2
    | Let(flag, bindings, t2) ->
        let s_rec = match flag with Nonrecursive -> "" | Recursive -> " rec" in
        let p = 10 in
        let s1,s2 = paren pri (p+1) in
        let b = ref true in
        let print_binding fm (f,xs,t1) =
          let pre = if !b then "let" ^ s_rec else "and" in
          fprintf fm "@[<hov 2>%s%a =@ %a@ @]" pre (print_ids true) (f::xs) (print_term' p) t1;
          b := false
        in
        let print_bindings bs = print_list print_binding "" bs in
        begin
          match t2.desc with
          | Let _ -> fprintf fm "%s@[<v>@[<hov 2>%a@]@ in@ %a@]%s"
                             s1 print_bindings bindings (print_term' p) t2 s2
          | _ -> fprintf fm     "%s@[<v>@[<hov 2>%a@]@ @[<v 2>in@ @]@[<hov>%a@]@]%s"
                         s1 print_bindings bindings (print_term' p) t2 s2
        end
    | BinOp(op, t1, t2) ->
        let p = match op with Add|Sub|Mult -> 6 | And -> 4 | Or -> 3 | _ -> 5 in
        let s1,s2 = paren pri p in
        fprintf fm "%s%a %a %a%s" s1 (print_term' p) t1 print_binop op (print_term' p) t2 s2
    | Not t ->
        let p = 6 in
        let s1,s2 = paren pri p in
        fprintf fm "%snot %a%s" s1 (print_term' p) t s2
    | Event(s,b) -> fprintf fm "{%s}" s
    | Record fields ->
        let rec aux fm = function
          | [] -> ()
          | (s,(f,t))::fields ->
              if fields=[]
              then fprintf fm "%s=%a" s (print_term' 0) t
              else fprintf fm "%s=%a; %a" s (print_term' 0) t aux fields
        in
        fprintf fm "{%a}" aux fields
    | Field(_,s,_,t) -> fprintf fm "%a.%s" (print_term' 9) t s
    | SetField(_,_,s,_,t1,t2) -> fprintf fm "%a.%s <- %a" (print_term' 9) t1 s (print_term' 3) t2
    | Nil -> fprintf fm "[]"
    | Cons(t1,t2) ->
        let p = 7 in
        let s1,s2 = paren pri (p+1) in
        fprintf fm "%s%a::%a%s" s1 (print_term' p) t1 (print_term' p) t2 s2
    | Constr(s,ts) ->
        let p = 8 in
        let s1,s2 = paren pri p in
        let aux fm = function
            [] -> ()
          | [t] -> fprintf fm "(%a)" (print_term' 1) t
          | t::ts ->
              fprintf fm "(%a" (print_term' 1) t;
              List.iter (fun t -> fprintf fm ",%a" (print_term' 1) t) ts;
              pp_print_string fm ")"
        in
        fprintf fm "%s%s%a%s" s1 s aux ts s2
    | Match(t,pats) ->
        let p = 1 in
        let s1,s2 = paren pri (p+1) in
        let aux = function
            (pat,{desc=Const True},t) -> fprintf fm "%a -> %a@ " print_pattern' pat (print_term' p) t
          | (pat,cond,t) -> fprintf fm "%a when %a -> %a@ "
                                    print_pattern' pat (print_term' p) cond (print_term' p) t
        in
        fprintf fm "%smatch %a with@ " s1 (print_term' p) t;
        List.iter aux pats;
        pp_print_string fm s2
    | Raise t ->
        let p = 4 in
        let s1,s2 = paren pri (p+1) in
        fprintf fm "%sraise %a%s" s1 (print_term' 1) t s2
    | TryWith(t1,t2) ->
        let p = 1 in
        let s1,s2 = paren pri (p+1) in
        fprintf fm "%stry %a with@ %a%s" s1 (print_term' p) t1 (print_term' p) t2 s2
    | Tuple ts ->
        fprintf fm "@[(%a)@]" (print_list (print_term' 0) ",@ ") ts
    | Proj(i,t) ->
        let p = 4 in
        let s1,s2 = paren pri (p+1) in
        fprintf fm "%s#%d %a%s" s1 i (print_term' 1) t s2
    | Bottom -> fprintf fm "_|_"
    | Label(info, t) ->
        fprintf fm "(@[label[%a]@ %a@])" print_info info (print_term' 0) t
    | Ref t ->
        let p = 4 in
        let s1,s2 = paren pri (p+1) in
        fprintf fm "%sref %a%s" s1 (print_term' 1) t s2
    | Deref t ->
        let p = 4 in
        let s1,s2 = paren pri (p+1) in
        fprintf fm "%s!%a%s" s1 (print_term' 1) t s2
    | SetRef(t1,t2) ->
        let p = 4 in
        let s1,s2 = paren pri (p+1) in
        fprintf fm "%s%a := %a%s" s1 (print_term' 1) t1 (print_term' 1) t2 s2
    | TNone -> fprintf fm "None"
    | TSome t ->
        let p = 4 in
        let s1,s2 = paren pri (p+1) in
        fprintf fm "%sSome %a%s" s1 (print_term' 1) t s2
  );fprintf fm ":%a)" (Color.cyan print_typ) t.typ
and print_pattern' fm pat =
  let rec aux fm pat =
    match pat.pat_desc with
    | PAny -> pp_print_string fm "_"
    | PVar x -> print_id_typ fm x
    | PAlias(p,x) -> fprintf fm "(%a as %a)" aux p print_id x
    | PConst c -> print_term' 1 fm c
    | PConstruct(c,pats) ->
        let aux' = function
            [] -> ()
          | [pat] -> fprintf fm "(%a)" aux pat
          | pat::pats ->
              fprintf fm "(%a" aux pat;
              List.iter (fun pat -> fprintf fm ",%a" aux pat) pats;
              pp_print_string fm ")"
        in
        pp_print_string fm c;
        aux' pats
    | PNil -> fprintf fm "[]"
    | PCons(p1,p2) -> fprintf fm "%a::%a" aux p1 aux p2
    | PRecord pats ->
        let aux' = function
            [] -> ()
          | [_,(_,_,pat)] -> fprintf fm "(%a)" aux pat
          | (_,(_,_,pat))::pats ->
              fprintf fm "(%a" aux pat;
              List.iter (fun (_,(_,_,pat)) -> fprintf fm ",%a" aux pat) pats;
              pp_print_string fm ")"
        in
        aux' pats
    | POr(pat1,pat2) -> fprintf fm "(%a | %a)" aux pat1 aux pat2
    | PTuple pats -> fprintf fm "(%a)" (print_list aux ", ") pats
    | PNone -> fprintf fm "None"
    | PSome p -> fprintf fm "(Some %a)" aux p
  in
  fprintf fm "| %a" aux pat

and print_termlist' pri fm = List.iter (fun bd -> fprintf fm "@ %a" (print_term' pri) bd)
let print_term' fm = print_term' 0 fm


let string_of_const = make_string_of print_const
let string_of_binop = make_string_of print_binop
let string_of_typ = make_string_of print_typ
let string_of_node = function
    BrNode -> assert false
  | LabNode true -> "then"
  | LabNode false -> "else"
  | FailNode -> "fail"
  | PatNode n -> "br" ^ string_of_int n
  | EventNode s -> s

let print_term_typ = print_term true
let print_term = print_term false
let print_desc = print_desc 0 false

let print_defs fm (defs:(id * (id list * typed_term)) list) =
  let print_fundef (f, (xs, t)) =
    fprintf fm "%a %a-> %a.\n" print_id f (print_ids false) xs print_term t
  in
    List.iter print_fundef defs



let string_of_constr t =
  match t.desc with
  | Const _ -> "Const"
  | Var _ -> "Var"
  | Fun _ -> "Fun"
  | App _ -> "App"
  | If _ -> "If"
  | Branch _ -> "Branch"
  | Let _ -> "Let"
  | BinOp _ -> "BinOp"
  | Not _ -> "Not"
  | Event _ -> "Event"
  | Record _ -> "Record"
  | Field _ -> "Field"
  | SetField _ -> "SetField"
  | Nil -> "Nil"
  | Cons _ -> "Cons"
  | Constr _ -> "Constr"
  | Match _ -> "Match"
  | Raise _ -> "Raise"
  | TryWith _ -> "TryWith"
  | Tuple _ -> "Tuple"
  | Proj _ -> "Proj"
  | Bottom -> "Bottom"
  | Label _ -> "Label"
  | Ref _ -> "Ref"
  | Deref _ -> "Deref"
  | SetRef _ -> "SetRef"
  | TNone -> "TNone"
  | TSome _ -> "TSome"

let print_constr fm t = pp_print_string fm @@ string_of_constr t
