open Format
open Util
open Type

type label = Read | Write | Close
type binop = Eq | Lt | Gt | Leq | Geq | And | Or | Add | Sub | Mult

type typ = term Type.t
and id = typ Id.t

and const = (* only base type constants *)
  | Unit
  | True
  | False
  | Int of int
  | Char of char
  | String of string
  | Float of float
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
  | CPS_result
  | RandValue of typ * bool

and attr =
  | ACPS
  | AAbst_under
  | ATerminate
  | ANotFail
  | ADeterministic
  | AComment of string
  | AId of int
  | AMark
  | ADoNotInline

and term = {desc:desc; typ:typ; attr:attr list}
and desc =
  | Const of const
  | Var of id
  | Fun of id * term
  | App of term * term list
  | If of term * term * term
  | Let of (id * id list * term) list * term
  | BinOp of binop * term * term
  | Not of term
  | Event of string * bool
  | Record of (string * term) list
  | Field of term * string
  | SetField of term * string * term
  | Nil
  | Cons of term * term
  | Constr of string * term list
  | Match of term * (pattern * term * term) list
  | Raise of term
  | TryWith of term * term
  | Tuple of term list
  | Proj of int * term
  | Bottom
  | Label of info * term
  | Ref of term
  | Deref of term
  | SetRef of term * term
  | TNone
  | TSome of term

and info =
  | InfoInt of int
  | InfoString of string
  | InfoId of id
  | InfoTerm of term
  | InfoIdTerm of id * term


and type_kind =
  | KAbstract
  | KVariant of (string * typ list) list
  | KRecord of (string * (mutable_flag * typ)) list
  | KOpen

and pred = desc

and pattern = {pat_desc:pat_desc; pat_typ:typ}
and pat_desc =
  | PAny
  | PVar of id
  | PAlias of pattern * id
  | PConst of term
  | PConstruct of string * pattern list
  | PNil
  | PCons of pattern * pattern
  | PTuple of pattern list
  | PRecord of (string * pattern) list
  | PNone
  | PSome of pattern
  | POr of pattern * pattern

type env = (id * typ) list

module ID = struct
  type t = id
  let compare = Id.compare
  let print = Id.print
end

let typ t = t.typ
let desc t = t.desc
let attr t = t.attr


module Id = struct
  include Id
  let new_var ?name ?attr typ =
    let name =
      match name with
      | None -> Type.var_name_of typ
      | Some name -> name
    in
    make (new_int()) name (Option.default [] attr) typ
end


let const_attr = [ANotFail; ATerminate; ADeterministic]


type trans =
  {mutable tr_term:      term -> term;
   mutable tr_term_rec:  term -> term;
   mutable tr_desc:      desc -> desc;
   mutable tr_desc_rec:  desc -> desc;
   mutable tr_typ:       typ -> typ;
   mutable tr_typ_rec:   typ -> typ;
   mutable tr_var:       id -> id;
   mutable tr_var_rec:   id -> id;
   mutable tr_pat:       pattern -> pattern;
   mutable tr_pat_rec:   pattern -> pattern;
   mutable tr_info:      info -> info;
   mutable tr_info_rec:  info -> info;
   mutable tr_const:     const -> const;
   mutable tr_const_rec: const -> const;
   mutable tr_attr:      attr list -> attr list}

let trans_typ trans = function
  | TUnit -> TUnit
  | TBool -> TBool
  | TInt -> TInt
  | TVar({contents=None} as x) -> TVar x
  | TVar{contents=Some typ} -> trans.tr_typ typ
  | TFun(x,typ) -> TFun(Id.map_typ trans.tr_typ x, trans.tr_typ typ)
  | TFuns(xs,typ) -> TFuns(List.map (Id.map_typ trans.tr_typ) xs, trans.tr_typ typ)
  | TApp(c, typs) -> TApp(c, List.map trans.tr_typ typs)
  | TTuple xs -> TTuple (List.map trans.tr_var xs)
  | TData s -> TData s
  | TAttr(attr, typ) ->
      let tr a =
        match a with
        | TAPred(x,ps) -> TAPred(trans.tr_var x, List.map trans.tr_term ps)
        | TAPureFun -> TAPureFun
      in
      TAttr(List.map tr attr, trans.tr_typ typ)
  | TVariant labels -> TVariant (List.map (Pair.map_snd @@ List.map trans.tr_typ) labels)
  | TRecord fields -> TRecord (List.map (Pair.map_snd @@ Pair.map_snd trans.tr_typ) fields)
  | Type(decls, s) -> Type(List.map (Pair.map_snd trans.tr_typ) decls, s)

let trans_var trans x = Id.map_typ trans.tr_typ x

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
    | PRecord pats -> PRecord(List.map (Pair.map_snd trans.tr_pat) pats)
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
  | RandValue(typ,b) -> RandValue(trans.tr_typ typ,b)

let trans_desc trans = function
  | Const c -> Const(trans.tr_const c)
  | Var y -> Var (trans.tr_var y)
  | Fun(y, t) -> Fun(trans.tr_var y, trans.tr_term t)
  | App(t1, ts) -> App(trans.tr_term t1, List.map trans.tr_term ts)
  | If(t1, t2, t3) -> If(trans.tr_term t1, trans.tr_term t2, trans.tr_term t3)
  | Let(bindings, t2) ->
      let bindings' = List.map (Triple.map trans.tr_var (List.map trans.tr_var) trans.tr_term) bindings in
      let t2' = trans.tr_term t2 in
      Let(bindings', t2')
  | BinOp(op, t1, t2) -> BinOp(op, trans.tr_term t1, trans.tr_term t2)
  | Not t1 -> Not (trans.tr_term t1)
  | Event(s,b) -> Event(s,b)
  | Record fields ->  Record (List.map (Pair.map_snd trans.tr_term) fields)
  | Field(t1,s) -> Field(trans.tr_term t1, s)
  | SetField(t1,s,t2) -> SetField(trans.tr_term t1,s,trans.tr_term t2)
  | Nil -> Nil
  | Cons(t1,t2) -> Cons(trans.tr_term t1, trans.tr_term t2)
  | Constr(s,ts) -> Constr(s, List.map trans.tr_term ts)
  | Match(t1,pats) -> Match(trans.tr_term t1, List.map (Triple.map trans.tr_pat trans.tr_term trans.tr_term) pats)
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
     tr_const_rec = Std.identity;
     tr_attr = Std.identity}
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
  {mutable tr2_term: 'a -> term -> term;
   mutable tr2_term_rec: 'a -> term -> term;
   mutable tr2_desc: 'a -> desc -> desc;
   mutable tr2_desc_rec: 'a -> desc -> desc;
   mutable tr2_typ: 'a -> typ -> typ;
   mutable tr2_typ_rec: 'a -> typ -> typ;
   mutable tr2_var: 'a -> id -> id;
   mutable tr2_var_rec: 'a -> id -> id;
   mutable tr2_pat: 'a -> pattern -> pattern;
   mutable tr2_pat_rec: 'a -> pattern -> pattern;
   mutable tr2_info: 'a -> info -> info;
   mutable tr2_info_rec: 'a -> info -> info;
   mutable tr2_const: 'a -> const -> const;
   mutable tr2_const_rec: 'a -> const -> const;
   mutable tr2_attr: 'a -> attr list -> attr list}

let trans2_gen_typ tr env = function
  | TUnit -> TUnit
  | TBool -> TBool
  | TInt -> TInt
  | TVar({contents=None} as x) -> TVar x
  | TVar{contents=Some typ} -> tr.tr2_typ env typ
  | TFun(x,typ) -> TFun(Id.map_typ (tr.tr2_typ env) x, tr.tr2_typ env typ)
  | TFuns(xs,typ) -> TFuns(List.map (Id.map_typ @@ tr.tr2_typ env) xs, tr.tr2_typ env typ)
  | TApp(c, typs) -> TApp(c, List.map (tr.tr2_typ env) typs)
  | TTuple xs -> TTuple (List.map (tr.tr2_var env) xs)
  | TData s -> TData s
  | TAttr(attr, typ) ->
      let aux a =
        match a with
        | TAPred(x,ps) -> TAPred(tr.tr2_var env x, List.map (tr.tr2_term env) ps)
        | TAPureFun -> TAPureFun
      in
      TAttr(List.map aux attr, tr.tr2_typ env typ)
  | TVariant labels -> TVariant (List.map (Pair.map_snd @@ List.map @@ tr.tr2_typ env) labels)
  | TRecord fields -> TRecord (List.map (Pair.map_snd @@ Pair.map_snd @@ tr.tr2_typ env) fields)
  | Type(decls, s) -> Type(List.map (Pair.map_snd @@ tr.tr2_typ env) decls, s)


let trans2_gen_var tr env x = Id.map_typ (tr.tr2_typ env) x

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
    | PRecord pats -> PRecord(List.map (Pair.map_snd @@ tr.tr2_pat env) pats)
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
  | InfoIdTerm(x, t) -> InfoIdTerm(tr.tr2_var env x, tr.tr2_term env t)

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
  | RandValue(typ,b) -> RandValue(tr.tr2_typ env typ, b)

let trans2_gen_desc tr env = function
  | Const c -> Const(tr.tr2_const env c)
  | Var y -> Var (tr.tr2_var env y)
  | Fun(y, t) -> Fun(tr.tr2_var env y, tr.tr2_term env t)
  | App(t1, ts) -> App(tr.tr2_term env t1, List.map (tr.tr2_term env) ts)
  | If(t1, t2, t3) -> If(tr.tr2_term env t1, tr.tr2_term env t2, tr.tr2_term env t3)
  | Let(bindings, t2) ->
      let aux (f,xs,t) = tr.tr2_var env f, List.map (tr.tr2_var env) xs, tr.tr2_term env t in
      Let(List.map aux bindings, tr.tr2_term env t2)
  | BinOp(op, t1, t2) -> BinOp(op, tr.tr2_term env t1, tr.tr2_term env t2)
  | Not t1 -> Not (tr.tr2_term env t1)
  | Event(s,b) -> Event(s,b)
  | Record fields ->  Record (List.map (Pair.map_snd @@ tr.tr2_term env) fields)
  | Field(t1,s) -> Field(tr.tr2_term env t1,s)
  | SetField(t1,s,t2) -> SetField(tr.tr2_term env t1,s,tr.tr2_term env t2)
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
     tr2_const_rec = id';
     tr2_attr = id'}
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
  {mutable col_term: term -> 'a;
   mutable col_term_rec: term -> 'a;
   mutable col_desc: desc -> 'a;
   mutable col_desc_rec: desc -> 'a;
   mutable col_typ: typ -> 'a;
   mutable col_typ_rec: typ -> 'a;
   mutable col_var: id -> 'a;
   mutable col_var_rec: id -> 'a;
   mutable col_pat: pattern -> 'a;
   mutable col_pat_rec: pattern -> 'a;
   mutable col_info: info -> 'a;
   mutable col_info_rec: info -> 'a;
   mutable col_const: const -> 'a;
   mutable col_const_rec: const -> 'a;
   mutable col_attr: attr list -> 'a;
   mutable col_app: 'a -> 'a -> 'a;
   mutable col_empty: 'a}

let col_typ col typ =
  let (-@-) = col.col_app in
  match typ with
  | TUnit -> col.col_empty
  | TBool -> col.col_empty
  | TInt -> col.col_empty
  | TVar{contents=None} -> col.col_empty
  | TVar{contents=Some typ} -> col.col_typ typ
  | TFun(x,typ) -> col.col_typ (Id.typ x) -@- col.col_typ typ
  | TFuns(xs,typ) -> List.fold_left (fun acc x -> acc -@- col.col_var x) col.col_empty xs -@- col.col_typ typ
  | TApp(_, typs) -> List.fold_left (fun acc typ -> acc -@- col.col_typ typ) col.col_empty typs
  | TTuple xs -> List.fold_left (fun acc x -> acc -@- col.col_var x) col.col_empty xs
  | TData s -> col.col_empty
  | TAttr(attr, typ) ->
      let aux acc a =
        match a with
        | TAPred(x,ps) ->
            let acc' = col.col_var x -@- acc in
            List.fold_left (fun acc p -> acc -@- col.col_term p) acc' ps
        | TAPureFun -> acc
      in
      List.fold_left aux (col.col_typ typ) attr
  | TVariant labels -> List.fold_left (fun acc (_,typs) -> List.fold_left (fun acc' typ -> acc' -@- col.col_typ typ) acc typs) col.col_empty labels
  | TRecord fields -> List.fold_left (fun acc (_,(_,typ)) -> acc -@- col.col_typ typ) col.col_empty fields
  | Type(decls, _) -> List.fold_left (fun acc (_,typ) -> acc -@- col.col_typ typ) col.col_empty decls

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
    | PRecord pats -> List.fold_left (fun acc (s,p) -> col.col_app acc @@ col.col_pat p) col.col_empty pats
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
  | Const c -> col.col_const c
  | Var y -> col.col_var y
  | Fun(y, t) -> col.col_app (col.col_var y) (col.col_term t)
  | App(t1, ts) -> List.fold_left (fun acc t -> col.col_app acc @@ col.col_term t) (col.col_term t1) ts
  | If(t1, t2, t3) -> col.col_app (col.col_term t1) @@ col.col_app (col.col_term t2) (col.col_term t3)
  | Let(bindings, t2) ->
      let aux acc (f,xs,t) =
        col.col_app acc @@
        col.col_app (col.col_var f) @@
        List.fold_left (fun acc x -> col.col_app acc @@ col.col_var x) (col.col_term t) xs
      in
      List.fold_left aux (col.col_term t2) bindings
  | BinOp(op, t1, t2) -> col.col_app (col.col_term t1) (col.col_term t2)
  | Not t1 -> col.col_term t1
  | Event(s,b) -> col.col_empty
  | Record fields -> List.fold_left (fun acc (_,t1) -> col.col_app acc @@ col.col_term t1) col.col_empty fields
  | Field(t1,s) -> col.col_term t1
  | SetField(t1,_,t2) -> col.col_app (col.col_term t1) (col.col_term t2)
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
     col_attr = f;
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
  {mutable col2_term: 'b -> term -> 'a;
   mutable col2_term_rec: 'b -> term -> 'a;
   mutable col2_desc: 'b -> desc -> 'a;
   mutable col2_desc_rec: 'b -> desc -> 'a;
   mutable col2_typ: 'b -> typ -> 'a;
   mutable col2_typ_rec: 'b -> typ -> 'a;
   mutable col2_var: 'b -> id -> 'a;
   mutable col2_var_rec: 'b -> id -> 'a;
   mutable col2_pat: 'b -> pattern -> 'a;
   mutable col2_pat_rec: 'b -> pattern -> 'a;
   mutable col2_info: 'b -> info -> 'a;
   mutable col2_info_rec: 'b -> info -> 'a;
   mutable col2_const: 'b -> const -> 'a;
   mutable col2_const_rec: 'b -> const -> 'a;
   mutable col2_attr: 'b -> attr list -> 'a;
   mutable col2_app: 'a -> 'a -> 'a;
   mutable col2_empty: 'a}

let col2_typ col env typ =
  let (-@-) = col.col2_app in
  match typ with
  | TUnit -> col.col2_empty
  | TBool -> col.col2_empty
  | TInt -> col.col2_empty
  | TVar{contents=None} -> col.col2_empty
  | TVar{contents=Some typ} -> col.col2_typ env typ
  | TFun(x,typ) -> col.col2_var env x -@- col.col2_typ env typ
  | TFuns(xs,typ) -> List.fold_left (fun acc x -> acc -@- col.col2_var env x) col.col2_empty xs -@- col.col2_typ env typ
  | TApp(_,typs) -> List.fold_left (fun acc typ -> acc -@- col.col2_typ env typ) col.col2_empty typs
  | TTuple xs -> List.fold_left (fun acc x -> acc -@- col.col2_var env x) col.col2_empty xs
  | TData s -> col.col2_empty
  | TAttr(attr, typ) ->
      let aux acc a =
        match a with
        | TAPred(x,ps) ->
            let acc' = col.col2_var env x -@- acc in
            List.fold_left (fun acc p -> acc -@- col.col2_term env p) acc' ps
        | TAPureFun -> acc
      in
      List.fold_left aux (col.col2_typ env typ) attr
  | TVariant labels -> List.fold_left (fun acc (_, typs) -> List.fold_left (fun acc' typ -> acc -@- col.col2_typ env typ) acc typs) col.col2_empty labels
  | TRecord fields -> List.fold_left (fun acc (_,(_,typ)) -> acc -@- col.col2_typ env typ) col.col2_empty fields
  | Type(decls, s) -> List.fold_left (fun acc (_,typ) -> acc -@- col.col2_typ env typ) col.col2_empty decls

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
    | PRecord pats -> List.fold_left (fun acc (s,p) -> col.col2_app acc @@ col.col2_pat env p) col.col2_empty pats
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
  | Const c -> col.col2_const env c
  | Var y -> col.col2_var env y
  | Fun(y, t) -> col.col2_app (col.col2_var env y) (col.col2_term env t)
  | App(t1, ts) -> List.fold_left (fun acc t -> col.col2_app acc @@ col.col2_term env t) (col.col2_term env t1) ts
  | If(t1, t2, t3) -> col.col2_app (col.col2_term env t1) @@ col.col2_app (col.col2_term env t2) (col.col2_term env t3)
  | Let(bindings, t2) ->
      let aux acc (f,xs,t) =
        col.col2_app acc @@
        col.col2_app (col.col2_var env f) @@
        List.fold_left (fun acc x -> col.col2_app acc @@ col.col2_var env x) (col.col2_term env t) xs
      in
      List.fold_left aux (col.col2_term env t2) bindings
  | BinOp(op, t1, t2) -> col.col2_app (col.col2_term env t1) (col.col2_term env t2)
  | Not t1 -> col.col2_term env t1
  | Event(s,b) -> col.col2_empty
  | Record fields -> List.fold_left (fun acc (_,t1) -> col.col2_app acc @@ col.col2_term env t1) col.col2_empty fields
  | Field(t1,s) -> col.col2_term env t1
  | SetField(t1,s,t2) -> col.col2_app (col.col2_term env t1) (col.col2_term env t2)
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
     col2_attr = f;
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
  {mutable tr_col2_term:      'b -> term    -> 'a * term;
   mutable tr_col2_term_rec:  'b -> term    -> 'a * term;
   mutable tr_col2_desc:      'b -> desc          -> 'a * desc;
   mutable tr_col2_desc_rec:  'b -> desc          -> 'a * desc;
   mutable tr_col2_typ:       'b -> typ           -> 'a * typ;
   mutable tr_col2_typ_rec:   'b -> typ           -> 'a * typ;
   mutable tr_col2_var:       'b -> id            -> 'a * id;
   mutable tr_col2_var_rec:   'b -> id            -> 'a * id;
   mutable tr_col2_pat:       'b -> pattern -> 'a * pattern;
   mutable tr_col2_pat_rec:   'b -> pattern -> 'a * pattern;
   mutable tr_col2_info:      'b -> info          -> 'a * info;
   mutable tr_col2_info_rec:  'b -> info          -> 'a * info;
   mutable tr_col2_const:     'b -> const         -> 'a * const;
   mutable tr_col2_const_rec: 'b -> const         -> 'a * const;
   mutable tr_col2_attr:      'b -> attr list     -> 'a * attr list;
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
  | TInt -> tc.tr_col2_empty, TInt
  | TVar({contents=None} as x) -> tc.tr_col2_empty, TVar x
  | TVar{contents=Some typ} -> tc.tr_col2_typ env typ
  | TFun(x,typ) ->
      let acc1,x' = tc.tr_col2_var env x in
      let acc2,typ' = tc.tr_col2_typ env typ in
      tc.tr_col2_app acc1 acc2, TFun(x', typ')
  | TFuns(xs,typ) ->
      let acc1,xs' = tr_col2_list tc tc.tr_col2_var env xs in
      let acc2,typ' = tc.tr_col2_typ env typ in
      tc.tr_col2_app acc1 acc2, TFuns(xs', typ')
  | TApp(c, typs) ->
      let acc,typs' = tr_col2_list tc tc.tr_col2_typ env typs in
      acc, TApp(c, typs')
  | TTuple xs ->
      let acc,xs' = tr_col2_list tc tc.tr_col2_var env xs in
      acc, TTuple xs'
  | TData s -> tc.tr_col2_empty, TData s
  | TAttr(attr, typ) ->
      let aux env a =
        match a with
        | TAPred(x,ps) ->
            let acc,x' = tc.tr_col2_var env x in
            let acc',ps' = tr_col2_list tc tc.tr_col2_term ~init:acc env ps in
            acc', TAPred(x',ps')
        | TAPureFun -> tc.tr_col2_empty, TAPureFun
      in
      let acc,typ' = tc.tr_col2_typ env typ in
      let acc',attr' = tr_col2_list tc aux ~init:acc env attr in
      acc', TAttr(attr', typ')
  | TVariant labels ->
      let acc,labels' =
        let aux (s,typs) (acc,labels') =
          let acc',typs' = tr_col2_list tc tc.tr_col2_typ ~init:acc env typs in
          acc', (s,typs')::labels'
        in
        List.fold_right aux labels (tc.tr_col2_empty,[])
      in
      acc, TVariant labels'
  | TRecord fields ->
      let acc,fields' =
        let aux env' (s,(f,typ)) =
          let acc,typ' = tc.tr_col2_typ env typ in
          acc, (s,(f,typ'))
        in
        tr_col2_list tc aux env fields
      in
      acc, TRecord fields'
  | Type(decls, s) ->
      let acc,decls' =
        let aux env' (s,typ) =
          let acc,typ' = tc.tr_col2_typ env typ in
          acc, (s,typ')
        in
        tr_col2_list tc aux env decls
      in
      acc, Type(decls', s)

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
        let aux env (s,p) =
          let acc',p' = tc.tr_col2_pat env p in
          acc', (s,p')
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
  | Const c ->
      let acc,c' = tc.tr_col2_const env c in
      acc, Const c'
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
  | Let(bindings, t2) ->
      let aux env (f,xs,t) =
        let acc1,f' = tc.tr_col2_var env f in
        let acc1',xs' = tr_col2_list tc tc.tr_col2_var ~init:acc1 env xs in
        let acc2,t' = tc.tr_col2_term env t in
        tc.tr_col2_app acc1' acc2, (f',xs',t')
      in
      let acc1,bindings' = tr_col2_list tc aux env bindings in
      let acc2,t2' = tc.tr_col2_term env t2 in
      tc.tr_col2_app acc1 acc2, Let(bindings',t2')
  | BinOp(op, t1, t2) ->
      let acc1,t1' = tc.tr_col2_term env t1 in
      let acc2,t2' = tc.tr_col2_term env t2 in
      tc.tr_col2_app acc1 acc2, BinOp(op,t1',t2')
  | Not t1 ->
      let acc,t1' = tc.tr_col2_term env t1 in
      acc, Not t1'
  | Event(s,b) -> tc.tr_col2_empty, Event(s,b)
  | Record fields ->
      let aux env (s,t1) =
        let acc,t1' = tc.tr_col2_term env t1 in
        acc, (s,t1')
      in
      let acc,fields' = tr_col2_list tc aux env fields in
      acc, Record fields'
  | Field(t1,s) ->
      let acc,t1' = tc.tr_col2_term env t1 in
      acc, Field(t1',s)
  | SetField(t1,s,t2) ->
      let acc1,t1' = tc.tr_col2_term env t1 in
      let acc2,t2' = tc.tr_col2_term env t2 in
      tc.tr_col2_app acc1 acc2, SetField(t1',s,t2')
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
     tr_col2_attr = f;
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
  {mutable fold_tr_term:      'a -> term      -> 'a * term;
   mutable fold_tr_term_rec:  'a -> term      -> 'a * term;
   mutable fold_tr_desc:      'a -> desc      -> 'a * desc;
   mutable fold_tr_desc_rec:  'a -> desc      -> 'a * desc;
   mutable fold_tr_typ:       'a -> typ       -> 'a * typ;
   mutable fold_tr_typ_rec:   'a -> typ       -> 'a * typ;
   mutable fold_tr_var:       'a -> id        -> 'a * id;
   mutable fold_tr_var_rec:   'a -> id        -> 'a * id;
   mutable fold_tr_pat:       'a -> pattern   -> 'a * pattern;
   mutable fold_tr_pat_rec:   'a -> pattern   -> 'a * pattern;
   mutable fold_tr_info:      'a -> info      -> 'a * info;
   mutable fold_tr_info_rec:  'a -> info      -> 'a * info;
   mutable fold_tr_const:     'a -> const     -> 'a * const;
   mutable fold_tr_const_rec: 'a -> const     -> 'a * const;
   mutable fold_tr_attr:      'a -> attr list -> 'a * attr list}

let fold_tr_list tr_col env xs =
  let aux x (env,xs) =
    let env',x' = tr_col env x in
    env', x'::xs
  in
  List.fold_right aux xs (env,[])

let fold_tr_typ fld env = function
  | TUnit -> env, TUnit
  | TBool -> env, TBool
  | TInt -> env, TInt
  | TVar({contents=None} as x) -> env, TVar x
  | TVar{contents=Some typ} -> fld.fold_tr_typ env typ
  | TFun(x,typ) ->
      let env',x' = fld.fold_tr_var env x in
      let env'',typ' = fld.fold_tr_typ env' typ in
      env'', TFun(x', typ')
  | TApp(c, typs) ->
      let env',typs' = fold_tr_list fld.fold_tr_typ env typs in
      env', TApp(c, typs')
  | TTuple xs ->
      let env',xs' = fold_tr_list fld.fold_tr_var env xs in
      env', TTuple xs'
  | TData s -> env, TData s
  | TAttr(attr, typ) ->
      let aux env a =
        match a with
        | TAPred(x,ps) ->
            let env',x' = fld.fold_tr_var env x in
            let env'',ps' = fold_tr_list fld.fold_tr_term env' ps in
            env'', TAPred(x',ps')
        | TAPureFun -> env, TAPureFun
      in
      let env',attr' = fold_tr_list aux env attr in
      let env'',typ' = fld.fold_tr_typ env' typ in
      env'', TAttr(attr', typ')
  | TVariant labels ->
      let env,labels' =
        let aux (s,typs) (env,labels') =
          let env',typs' = fold_tr_list fld.fold_tr_typ env typs in
          env', (s,typs')::labels'
        in
        List.fold_right aux labels (env,[])
      in
      env, TVariant labels'
  | TRecord fields ->
      let env,fields' =
        let aux env' (s,(f,typ)) =
          let env,typ' = fld.fold_tr_typ env typ in
          env, (s,(f,typ'))
        in
        fold_tr_list aux env fields
      in
      env, TRecord fields'
  | Type(decls, s) ->
      let env,decls' =
        let aux env' (s,typ) =
          let env,typ' = fld.fold_tr_typ env typ in
          env, (s,typ')
        in
        fold_tr_list aux env decls
      in
      env, Type(decls', s)
  | TFuns _ -> unsupported "fold_tr"

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
        let env'',ps' = fold_tr_list fld.fold_tr_pat env ps in
        env'', PConstruct(s,ps')
    | PNil -> env', PNil
    | PCons(p1,p2) ->
        let env'',p1' = fld.fold_tr_pat env' p1 in
        let env''',p2' = fld.fold_tr_pat env'' p2 in
        env''', PCons(p1', p2')
    | PTuple ps ->
        let env'',ps' = fold_tr_list fld.fold_tr_pat env' ps in
        env'', PTuple ps'
    | PRecord pats ->
        let aux env (s,p) =
          let env',p' = fld.fold_tr_pat env p in
          env', (s,p')
        in
        let env'',pats' = fold_tr_list aux env' pats in
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
  | Const c ->
      let env',c' = fld.fold_tr_const env c in
      env', Const c'
  | Var y ->
      let env',y' = fld.fold_tr_var env y in
      env', Var y'
  | Fun(y, t) ->
      let env',y' = fld.fold_tr_var env y in
      let env'',t' = fld.fold_tr_term env' t in
      env'', Fun(y',t')
  | App(t1, ts) ->
      let env',t1' = fld.fold_tr_term env t1 in
      let env'',ts' = fold_tr_list fld.fold_tr_term env' ts in
      env'', App(t1', ts')
  | If(t1, t2, t3) ->
      let env',t1' = fld.fold_tr_term env t1 in
      let env'',t2' = fld.fold_tr_term env' t2 in
      let env''',t3' = fld.fold_tr_term env'' t3 in
      env''', If(t1',t2',t3')
  | Let(bindings, t2) ->
      let aux env (f,xs,t) =
        let env',f' = fld.fold_tr_var env f in
        let env'',xs' = fold_tr_list fld.fold_tr_var env' xs in
        let env''',t' = fld.fold_tr_term env'' t in
        env''', (f',xs',t')
      in
      let env',bindings' = fold_tr_list aux env bindings in
      let env'',t2' = fld.fold_tr_term env' t2 in
      env'', Let(bindings',t2')
  | BinOp(op, t1, t2) ->
      let env',t1' = fld.fold_tr_term env t1 in
      let env'',t2' = fld.fold_tr_term env' t2 in
      env'', BinOp(op,t1',t2')
  | Not t1 ->
      let env',t1' = fld.fold_tr_term env t1 in
      env', Not t1'
  | Event(s,b) -> env, Event(s,b)
  | Record fields ->
      let aux env (s,t1) =
        let env',t1' = fld.fold_tr_term env t1 in
        env', (s,t1')
      in
      let env',fields' = fold_tr_list aux env fields in
      env', Record fields'
  | Field(t1,s) ->
      let env',t1' = fld.fold_tr_term env t1 in
      env', Field(t1',s)
  | SetField(t1,s,t2) ->
      let env',t1' = fld.fold_tr_term env t1 in
      let env'',t2' = fld.fold_tr_term env' t2 in
      env'', SetField(t1',s,t2')
  | Nil -> env, Nil
  | Cons(t1,t2) ->
      let env',t1' = fld.fold_tr_term env t1 in
      let env'',t2' = fld.fold_tr_term env' t2 in
      env'', Cons(t1',t2')
  | Constr(s,ts) ->
      let env',ts' = fold_tr_list fld.fold_tr_term env ts in
      env', Constr(s,ts')
  | Match(t1,pats) ->
      let aux env (pat,cond,t1) =
        let env',pat' = fld.fold_tr_pat env pat in
        let env'',cond' = fld.fold_tr_term env' cond in
        let env''',t1' = fld.fold_tr_term env'' t1 in
        env''', (pat',cond',t1')
      in
      let env',t1' = fld.fold_tr_term env t1 in
      let env'',pats' = fold_tr_list aux env' pats in
      env'', Match(t1',pats')
  | Raise t ->
      let env',t' = fld.fold_tr_term env t in
      env', Raise t'
  | TryWith(t1,t2) ->
      let env',t1' = fld.fold_tr_term env t1 in
      let env'',t2' = fld.fold_tr_term env' t2 in
      env'', TryWith(t1',t2')
  | Tuple ts ->
      let env',ts' = fold_tr_list fld.fold_tr_term env ts in
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
  let env',desc = fld.fold_tr_desc env t.desc in
  let env'',typ = fld.fold_tr_typ env' t.typ in
  env'', {desc; typ; attr=t.attr}


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
     fold_tr_const_rec = f;
     fold_tr_attr = f}
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
  | PRecord pats -> List.fold_left (fun acc (_,pat) -> get_vars_pat pat @@@ acc) [] pats
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
  | Let(bindings, t2) ->
      let vars' = List.fold_left (fun vars (f,_,_) -> f::vars) vars bindings in
      let aux fv (_,xs,t) = get_fv.col2_term (xs@@@vars') t @@@ fv in
      let fv_t2 = get_fv.col2_term vars' t2 in
      List.fold_left aux fv_t2 bindings
  | Fun(x,t) -> get_fv.col2_term (x::vars) t
  | Match(t,pats) ->
      let aux acc (pat,cond,t) =
        let vars' = get_vars_pat pat @@@ vars in
        get_fv.col2_term vars' cond @@@ get_fv.col2_term vars' t @@@ acc
      in
      List.fold_left aux (get_fv.col2_term vars t) pats
  | _ -> get_fv.col2_term_rec vars t

let () = get_fv.col2_typ <- Fun.const @@ Fun.const []
let () = get_fv.col2_term <- get_fv_term
let () = get_fv.col2_typ <- get_fv_typ
let get_fv ?(eq=Id.same) t =
  List.unique ~eq @@ get_fv.col2_term [] t



let occur = make_col2 false (||)

let occur_typ x typ =
  match typ with
  | TAttr(attr, typ) ->
      let aux a =
        match a with
        | TAPred(y,ps) -> List.exists (List.exists (Id.same x) -| get_fv) ps
        | TAPureFun -> false
      in
      List.exists aux attr || occur.col2_typ x typ
  | _ -> occur.col2_typ_rec x typ

let () = occur.col2_typ <- occur_typ
let occur = occur.col2_typ


let make_extra_coeff ?(name="c") ?(typ=TInt) () = Id.new_var ~name:name ~attr:[Id.Coefficient] typ
let is_extra_coeff = List.mem Id.Coefficient -| Id.attr



let is_non_rec bindings =
  match bindings with
  | [] -> assert false
  | [f, _, t] -> not @@ Id.mem f @@ get_fv t
  | _ -> false
