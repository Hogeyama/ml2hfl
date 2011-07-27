
open Format
open Utilities
open Type

type label = Read | Write | Close
type binop = Eq | Lt | Gt | Leq | Geq | And | Or | Add | Sub | Mult

type typ = typed_term Type.t
and id = typ Id.t

and typed_term = {desc:term; typ:typ}
and term =
    Unit
  | True
  | False
  | Unknown
  | Int of int
  | NInt of id
  | RandInt of typed_term option
  | RandValue of typ * typed_term option
  | Var of id
  | Fun of id * typed_term
  | App of typed_term * typed_term list
  | If of typed_term * typed_term * typed_term
  | Branch of typed_term * typed_term
  | Let of Flag.rec_flag * id * id list * typed_term * typed_term
  | BinOp of binop * typed_term * typed_term
  | Not of typed_term
  | Fail
  | Label of bool * typed_term
  | LabelInt of int * typed_term
  | Event of string
  | Record of bool * (string * (Flag.mutable_flag * typed_term)) list
  | Proj of int option * int * string * Flag.mutable_flag * typed_term
  | SetField of int option * int * string * Flag.mutable_flag * typed_term * typed_term
  | Nil
  | Cons of typed_term * typed_term
  | Constr of string * typed_term list
  | Match of typed_term * typed_term * id * id * typed_term
  | Match_ of typed_term * (typed_pattern * typed_term option * typed_term) list
  | TryWith of typed_term * (typed_pattern * typed_term option * typed_term) list


and type_kind =
    KAbstract
  | KVariant of (string * typ list) list
  | KRecord of (string * (Flag.mutable_flag * typ)) list

and pred = term

and typed_pattern = {pat_desc:pattern; pat_typ:typ}
and pattern =
    PVar of id
  | PConst of typed_term
  | PConstruct of string * typed_pattern list
  | PNil
  | PCons of typed_pattern * typed_pattern
  | PRecord of bool * (int * (string * Flag.mutable_flag * typed_pattern)) list
  | POr of typed_pattern * typed_pattern

type syntax = ML | TRecS | CVC3 | CSIsat

type node = BrNode | LabNode of bool | FailNode | EventNode of string | PatNode of int


exception Feasible of typed_term
exception Infeasible

type literal = Cond of typed_term | Pred of (id * int * id * typed_term list)



let dummy_var = {Id.id=0; Id.name=""; Id.typ=TInt[]}
let abst_var = {Id.id=0; Id.name="v"; Id.typ=TInt[]}
let abst_list_var = {Id.id=0; Id.name="v"; Id.typ=TList(TUnknown,[])}

let unit_term = {desc=Unit; typ=TUnit}
let true_term = {desc=True;typ=TBool}
let false_term = {desc=False;typ=TBool}
let event_term s =
  let x = Id.new_var "u" TUnit in
    {desc=Event s;typ=TFun(x,TUnit)}

let make_var x = {desc=Var x; typ=Id.typ x}


(* [x |-> t], [t/x] *)
let rec subst x t t' =
  let desc =
    match t'.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | NInt y -> if Id.same x y then t.desc else NInt y
      | RandInt None -> RandInt None
      | RandInt (Some t1) -> RandInt (Some (subst x t t1))
      | Var y -> if Id.same x y then t.desc else Var y
      | Fun(y, t1) ->
          let t1' = if Id.same x y then t1 else subst x t t1 in
            Fun(y, t1')
      | App(t1, ts) ->
          let t1' = subst x t t1 in
          let ts' = List.map (subst x t) ts in
            begin
              match t1'.desc with
                  App(t, ts) -> App(t, ts@ts')
                | _ -> App(t1', ts')
            end
      | If(t1, t2, t3) ->
          let t1' = subst x t t1 in
          let t2' = subst x t t2 in
          let t3' = subst x t t3 in
            If(t1', t2', t3')
      | Branch(t1, t2) ->
          let t1' = subst x t t1 in
          let t2' = subst x t t2 in
            Branch(t1', t2')
      | Let(Flag.Nonrecursive, f, xs, t1, t2) ->
          let t1' = subst x t t1 in
          let t2' = if Id.same f x then t2 else subst x t t2 in
            Let(Flag.Nonrecursive, f, xs, t1', t2')
      | Let(Flag.Recursive, f, xs, t1, t2) ->
          let t1' = if Id.same f x then t1 else subst x t t1 in
          let t2' = if Id.same f x then t2 else subst x t t2 in
            Let(Flag.Recursive, f, xs, t1', t2')
      | BinOp(op, t1, t2) ->
          let t1' = subst x t t1 in
          let t2' = subst x t t2 in
            BinOp(op, t1', t2')
      | Not t1 ->
          let t1' = subst x t t1 in
            Not t1'
      | Fail -> Fail
      | Label(b, t1) ->
          let t1' = subst x t t1 in
            Label(b, t1')
      | LabelInt(n, t1) ->
          let t1' = subst x t t1 in
            LabelInt(n, t1')
      | Event s -> Event s
      | Record(b,fields) ->  Record (b, List.map (fun (f,(s,t1)) -> f,(s,subst x t t1)) fields)
      | Proj(n,i,s,f,t1) -> Proj(n,i,s,f,subst x t t1)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,subst x t t1,subst x t t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(subst x t t1, subst x t t2)
      | Constr(s,ts) -> Constr(s, List.map (subst x t) ts)
      | Match(t1,t2,y,z,t3) -> Match(subst x t t1, subst x t t2, y, z, subst x t t3)
      | Match_(t1,pats) ->
          let aux (pat,cond,t1) = pat, cond, subst x t t1 in
            Match_(subst x t t1, List.map aux pats)
      | TryWith(t1,pats) ->
          let aux (pat,cond,t1) = pat, cond, subst x t t1 in
            TryWith(subst x t t1, List.map aux pats)
  in
    {desc=desc; typ=t'.typ}

let rec subst_int n t t' =
  let desc =
    match t'.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int m -> if n = m then t.desc else BinOp(Add, t, {desc=Int(m-n); typ=TInt[]})
      | NInt y -> NInt y
      | Var y -> Var y
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
      | Let(flag, f, xs, t1, t2) ->
          let t1' = subst_int n t t1 in
          let t2' = subst_int n t t2 in
            Let(flag, f, xs, t1', t2')
      | BinOp(Mult, t1, t2) -> (* non-linear expressions not supported *)
          BinOp(Mult, t1, t2)
      | BinOp(op, t1, t2) ->
          let t1' = subst_int n t t1 in
          let t2' = subst_int n t t2 in
            BinOp(op, t1', t2')
      | Not t1 ->
          let t1' = subst_int n t t1 in
            Not t1'
      | Fail -> Fail
      | Label(b, t1) ->
          let t1' = subst_int n t t1 in
            Label(b, t1')
      | Event s -> Event s
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(subst_int n t t1, subst_int n t t2)
      | Match(t1,t2,x,y,t3) -> Match(subst_int n t t1, subst_int n t t2, x, y, subst_int n t t3)
  in
    {desc=desc; typ=t'.typ}

let subst_term sub term =
  let ids, terms = List.split sub in
  List.fold_right2 subst ids terms term

let rec subst_type x t = function
    TUnit -> TUnit
  | TAbsBool -> TAbsBool
  | TBool -> TBool
  | TInt ts -> TInt (List.map (subst x t) ts)
  | TRInt t' -> TRInt (subst x t t')
  | TVar _ -> assert false
  | TFun(y,typ) ->
      let y' = Id.set_typ y (subst_type x t (Id.typ y)) in
      let typ' = subst_type x t typ in
        TFun(y', typ')
  | TUnknown -> TUnknown
  | TList(typ,ps) -> TList(subst_type x t typ, List.map (subst x t) ps)
  | TRecord(b,typs) -> TRecord(b, List.map (fun (s,(f,typ)) -> s,(f,subst_type x t typ)) typs)

(*
let rec subst_orig x t = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt y ->
      if Id.name x = Id.name y then t else NInt y
  | Var y ->
      if Id.name x = Id.name y then t else Var y
  | Fun(y, t1) ->
      let t1' = if Id.name x = Id.name y then t1 else subst_orig x t t1 in
        Fun(y, t1')
  | App(t1, ts) ->
      let t1' = subst_orig x t t1 in
      let ts' = List.map (subst_orig x t) ts in
        begin
          match t1' with
              App(t, ts) -> App(t, ts@ts')
            | _ -> App(t1', ts')
        end
  | If(t1, t2, t3) ->
      let t1' = subst_orig x t t1 in
      let t2' = subst_orig x t t2 in
      let t3' = subst_orig x t t3 in
        If(t1', t2', t3')
  | Branch(t1, t2) ->
      let t1' = subst_orig x t t1 in
      let t2' = subst_orig x t t2 in
        Branch(t1', t2')
(*
  | Let(Nonrecursive, bindings, t2) ->
      let bindings' = List.map (fun (f,xs,t1) -> f,xs,subst_orig x t t1) bindings in
      let t2' = 
        if List.exists (fun (f,_,_) -> x.id = f.id) bindings
        then t2
        else subst_orig x t t2
      in
        Let(Nonrecursive, bindings', t2')
  | Let(Recursive, bindings, t2) ->
      if List.exists (fun (f,_,_) -> x.id = f.id) bindings
      then Let(Recursive, bindings, t2)
      else
        let bindings' = List.map (fun (f,xs,t1) -> f,xs,subst_orig x t t1) bindings in
        let t2' = subst_orig x t t2 in
          Let(Recursive, bindings', t2')
*)
  | Let(flag, f, xs, t1, t2) ->
      if flag = Nonrecursive
      then
        let t1' = subst_orig x t t1 in
        let t2' = if f.name = x.name then t2 else subst_orig x t t2 in
          Let(flag, f, xs, t1', t2')
      else
        let t1' = if f.name = x.name then t1 else subst_orig x t t1 in
        let t2' = if f.name = x.name then t2 else subst_orig x t t2 in
          Let(flag, f, xs, t1', t2')
  | BinOp(op, t1, t2) ->
      let t1' = subst_orig x t t1 in
      let t2' = subst_orig x t t2 in
        BinOp(op, t1', t2')
  | Not t1 ->
      let t1' = subst_orig x t t1 in
        Not t1'
  | Fail -> Fail
  | Label(b, t1) ->
      let t1' = subst_orig x t t1 in
        Label(b, t1')
  | Event s -> Event s

let rec subst_type_orig x t = function
    TUnit -> TUnit
  | TAbsBool -> TAbsBool
  | TBool -> TBool
  | TInt ts -> TInt (List.map (subst_orig x t) ts)
  | TRInt t' -> TRInt (subst_orig x t t')
  | TVar _ -> assert false
  | TFun((y,typ1),typ2) ->
      let typ1' = subst_type_orig x t typ1 in
      let typ2' = subst_type_orig x t typ2 in
      let y' = {y with typ = typ1'} in
        TFun((y',typ1'),typ2')
  | TUnknown -> TUnknown
*)

let rec fff _ = assert false(*function
    TUnit -> TUnit
  | TAbsBool -> TAbsBool
  | TBool -> TBool
  | TInt ts -> TInt ts
  | TRInt t -> TRInt t
  | TVar _ -> assert false
  | TFun((x,typ1),typ2) ->
      let x' = new_var_id x in
        TFun(({x' with typ = typ1}, typ1), subst_type_orig x (Var x') typ2)
  | TUnknown -> TUnknown
*)



let rec get_nint t =
  match t.desc with
      Unit -> []
    | True -> []
    | False -> []
    | Unknown -> []
    | Int n -> []
    | NInt x -> [x]
    | Var x -> []
    | App(t, ts) -> get_nint t @@@ (rev_map_flatten get_nint ts)
    | If(t1, t2, t3) -> get_nint t1 @@@ get_nint t2 @@@ get_nint t3
    | Branch(t1, t2) -> get_nint t1 @@@ get_nint t2
    | Let(flag, _, _, t1, t2) -> get_nint t1 @@@ get_nint t2
    | BinOp(op, t1, t2) -> get_nint t1 @@@ get_nint t2
    | Not t -> get_nint t
    | Fail -> []
    | Fun(x,t) -> diff (get_nint t) [x]
    | Label(_,t) -> get_nint t
    | Event _ -> []
    | Nil -> []
    | Cons(t1,t2) -> get_nint t1 @@@ get_nint t2

let rec get_int t =
  match t.desc with
      Unit -> []
    | True -> []
    | False -> []
    | Unknown -> []
    | Int n -> [n]
    | NInt x -> []
    | Var x -> []
    | App(t, ts) -> get_int t @@@ (rev_map_flatten get_int ts)
    | If(t1, t2, t3) -> get_int t1 @@@ get_int t2 @@@ get_int t3
    | Branch(t1, t2) -> get_int t1 @@@ get_int t2
    | Let(_, _, _, t1, t2) -> get_int t1 @@@ get_int t2
    | BinOp(Mult, t1, t2) -> [] (* non-linear expressions not supported *)
    | BinOp(_, t1, t2) -> get_int t1 @@@ get_int t2
    | Not t -> get_int t
    | Fail -> []
    | Fun(_,t) -> get_int t
    | Label(_,t) -> get_int t
    | Event s -> []
    | Nil -> []
    | Cons(t1,t2) -> get_int t1 @@@ get_int t2

let rec get_fv vars t =
  match t.desc with
      Unit -> []
    | True -> []
    | False -> []
    | Unknown -> []
    | Int n -> []
    | NInt x -> []
    | RandInt None -> []
    | RandInt (Some t) -> get_fv vars t
    | Var x -> if List.mem x vars then [] else [x]
    | App(t, ts) -> get_fv vars t @@@ (rev_map_flatten (get_fv vars) ts)
    | If(t1, t2, t3) -> get_fv vars t1 @@@ get_fv vars t2 @@@ get_fv vars t3
    | Branch(t1, t2) -> get_fv vars t1 @@@ get_fv vars t2
    | Let(flag, f, xs, t1, t2) ->
        let bindings = [f,xs,t1] in
        let vars_with_fun = List.fold_left (fun vars (f,_,_) -> f::vars) vars bindings in
        let vars' = match flag with Flag.Nonrecursive -> vars | Flag.Recursive -> vars_with_fun in
        let aux fv (_,xs,t) = get_fv (xs@@vars') t @@@ fv in
        let fv_t2 = get_fv vars_with_fun t2 in
          List.fold_left aux fv_t2 bindings
    | BinOp(op, t1, t2) -> get_fv vars t1 @@@ get_fv vars t2
    | Not t -> get_fv vars t
    | Fail -> []
    | Fun(x,t) -> get_fv (x::vars) t
    | Label(_,t) -> get_fv vars t
    | LabelInt(_,t) -> get_fv vars t
    | Event s -> []
    | Record(_,fields) -> List.fold_left (fun acc (_,(_,t)) -> get_fv vars t @@@ acc) [] fields
    | Proj(_,_,_,_,t) -> get_fv vars t
    | SetField(_,_,_,_,t1,t2) -> get_fv vars t1 @@@ get_fv vars t2
    | Nil -> []
    | Cons(t1, t2) -> get_fv vars t1 @@@ get_fv vars t2
    | Match(t1,t2,x,y,t3) -> get_fv vars t1 @@@ get_fv vars t2 @@@ get_fv (x::y::vars) t3
    | Constr(_,ts) -> List.fold_left (fun acc t -> get_fv vars t @@@ acc) [] ts
    | Match_(t,pats) ->
        let aux acc (_,_,t) = get_fv vars t @@@ acc in
          List.fold_left aux (get_fv vars t) pats
    | TryWith(t,pats) ->
        let aux acc (_,_,t) = get_fv vars t @@@ acc in
          List.fold_left aux (get_fv vars t) pats
let get_fv = get_fv []

let rec get_fv2 vars t =
  match t.desc with
      Unit -> []
    | True -> []
    | False -> []
    | Unknown -> []
    | Int n -> []
    | NInt x -> [x]
    | RandInt None -> []
    | RandInt (Some t) -> get_fv2 vars t
    | Var x -> if List.mem x vars then [] else [x]
    | App(t, ts) -> get_fv2 vars t @@@ (rev_map_flatten (get_fv2 vars) ts)
    | If(t1, t2, t3) -> get_fv2 vars t1 @@@ get_fv2 vars t2 @@@ get_fv2 vars t3
    | Branch(t1, t2) -> get_fv2 vars t1 @@@ get_fv2 vars t2
    | Let(flag, f, xs, t1, t) ->
        let bindings = [f,xs,t1] in
        let vars_with_fun = List.fold_left (fun vars (f,_,_) -> f::vars) vars bindings in
        let vars' = match flag with Flag.Nonrecursive -> vars | Flag.Recursive -> vars_with_fun in
        let aux fv (_,xs,t) = get_fv2 (xs@@vars') t @@@ fv in
        let fv_t = get_fv2 vars_with_fun t in
          List.fold_left aux fv_t bindings
            (*
              | Let(flag, bindings, t) ->
              let vars_with_fun = List.fold_left (fun vars (f,_,_) -> f::vars) vars bindings in
              let vars' = match flag with Nonrecursive -> vars | Recursive -> vars_with_fun in
              let aux fv (_,xs,t) = get_fv2 (xs@@vars') t @@@ fv in
              let fv_t = get_fv2 vars_with_fun t in
              List.fold_left aux fv_t bindings
            *)
    | BinOp(op, t1, t2) -> get_fv2 vars t1 @@@ get_fv2 vars t2
    | Not t -> get_fv2 vars t
    | Fail -> []
    | Fun(x,t) -> get_fv2 (x::vars) t
    | Label(_,t) -> get_fv2 vars t
    | Event s -> []
    | Nil -> []
    | Cons(t1,t2) -> get_fv2 vars t1 @@@ get_fv2 vars t2
    | Match(t1,t2,x,y,t3) -> get_fv2 vars t1 @@@ get_fv2 vars t2 @@@ get_fv2 (x::y::vars) t3
    | Constr(_,ts) -> List.fold_left (fun acc t -> get_fv2 vars t @@@ acc) [] ts
    | Match_(t,pats) ->
        let aux acc (_,_,t) = get_fv2 vars t @@@ acc in
          List.fold_left aux (get_fv2 vars t) pats
let get_fv2 = get_fv2 []

let rec get_vars_pat pat =
  match pat.pat_desc with
      PVar x -> [x]
    | PConst _ -> []
    | PConstruct(_,pats) -> List.fold_left (fun acc pat -> get_vars_pat pat @@ acc) [] pats
    | PRecord(b,pats) -> List.fold_left (fun acc (_,(_,_,pat)) -> get_vars_pat pat @@ acc) [] pats
    | POr(p1,p2) -> get_vars_pat p1 @@ get_vars_pat p2





let rec get_args = function
    TFun(x,typ) -> x :: get_args typ
  | _ -> []

let rec get_argvars = function
    TFun(x,typ) -> x :: get_argvars (Id.typ x) @ get_argvars typ
  | _ -> []

let rec get_argtyps = function
    TFun(x,typ) -> Id.typ x :: get_argtyps typ
  | _ -> []

let rec app2app t ts =
  match t,ts with
    | t,[]_ -> t
    | {desc=App(t1,ts1);typ=TFun(x,typ)}, t2::ts2 ->
      assert (Type.can_unify (Id.typ x) t2.typ);
      app2app {desc=App(t1,ts1@[t2]); typ=typ} ts2
    | {desc=t;typ=TFun(x,typ)}, t2::ts ->
      assert (Type.can_unify (Id.typ x) t2.typ);
      app2app {desc=App({desc=t;typ=TFun(x,typ)},[t2]); typ=typ} ts
    | _ -> assert false


(*
let rec is_unique_ident_name x = function
    Unit
  | True
  | False
  | Unknown
  | Int _
  | Var _
  | NInt _
  | RandInt None -> true
  | RandInt (Some t) -> is_unique_ident_name x t
  | Fun(y,t) when x.name = y.name -> not (List.exists (same x) (get_fv t))
  | Fun(y,t) -> is_unique_ident_name x t
  | App(t, ts) -> List.for_all (is_unique_ident_name x) (t::ts)
  | If(t1, t2, t3) -> List.for_all (is_unique_ident_name x) [t1;t2;t3]
  | Branch(t1, t2) -> is_unique_ident_name x t1 && is_unique_ident_name x t2
  | Let(flag, f, xs, t1, t2) ->
      let xs' = if flag = Nonrecursive then xs else f::xs in
      let b1 =
        if List.exists (fun y -> x.name = y.name) xs'
        then not (List.exists (same x) (get_fv t1))
        else is_unique_ident_name x t1
      in
      let b2 =
        if x.name = f.name
        then not (List.exists (same x) (get_fv t2))
        else is_unique_ident_name x t2
      in
        b1 && b2
  | BinOp(_, t1, t2) -> is_unique_ident_name x t1 && is_unique_ident_name x t2
  | Not t -> is_unique_ident_name x t
  | Fail -> true
  | Label(_, t) -> is_unique_ident_name x t
  | Event _ -> true
  | Record(_,fields) -> List.for_all (fun (_,(_,t)) -> is_unique_ident_name x t) fields
  | Proj(n,i,s,f,t) -> is_unique_ident_name x t
  | Nil -> true
  | Cons(t1,t2) -> is_unique_ident_name x t1 && is_unique_ident_name x t2
  | Constr(s,ts) -> List.for_all (is_unique_ident_name x) ts
  | Match(t1,t2,x,y,t3) -> assert false
  | Match_(t,pats) ->
      let aux (pat,cond,t) =
        let xs = get_vars_pat pat in
        if List.exists (fun y -> x.name = y.name) xs
        then
          let b1 =
            match cond with
                None -> true
              | Some cond -> not (List.exists (same x) (get_fv cond))
          in
          let b2 = not (List.exists (same x) (get_fv t)) in
            b1 && b2
        else is_unique_ident_name x t
      in
        is_unique_ident_name x t && List.for_all aux pats
*)



(*
let rec safe = function
    Unit
  | True
  | False
  | Unknown
  | Int _
  | Var _
  | Fail -> true
  | NInt _ -> false
  | App(t, ts) -> safe t && List.for_all safe ts
  | If(t1, t2, t3, t4) -> safe t1 && safe t2 && safe t3 && safe t4
  | Branch(t1, t2) -> safe t1 && safe t2
  | Let(_, _, t1, t2) -> safe t1 && safe t2
  | Letrec(_, _, t1, t2) -> safe t1 && safe t2
  | BinOp(_, t1, t2) -> safe t1 && safe t2
  | Not t -> safe t
  | Fun(_, t) -> safe t
  | Label(_, t) -> safe t
*)


let rec eval t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | NInt x -> NInt x
      | Var x -> Var x
      | App({desc=Fun(x, t)}, t'::ts) ->
          (match t'.desc with
               NInt _ ->
                 App({desc=Fun(x, eval t);typ=TFun(x,t.typ)}, List.map eval (t'::ts))
             | _ ->
                 (eval ({desc=App(subst_term [x, t'] t, ts);typ=t.typ})).desc)
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
      | Let(flag, f, xs, t1, t2) -> (*assume that evaluation of t1 does not fail*)
          if flag = Flag.Nonrecursive
          then
            if (*safe t1*)true then
              let t1' = List.fold_right (fun x t -> {desc=Fun(x, t);typ=TFun(x,t.typ)}) xs (eval t1) in
                (eval (subst_term [f, t1'] t2)).desc
            else
              Let(flag, f, xs, eval t1, eval t2)
          else
            (*if not (List.mem f (get_fv t1)) then
              let t1' = List.fold_right (fun x t -> Fun(x, t)) xs (eval t1) in
              eval (subst_term [f, t1'] t2)
              else*)
            Let(flag, f, xs, eval t1, eval t2)
      | BinOp(Add, {desc=Int 0}, t) ->
          (eval t).desc
      | BinOp(Mult, {desc=Int 1}, t) ->
          (eval t).desc
      | BinOp(Sub, t1, t2) ->
          (eval {desc=BinOp(Add, eval t1, eval {desc=BinOp(Mult, {desc=Int(-1);typ=TInt[]}, t2);typ=TInt[]});typ=TInt[]}).desc
      | BinOp(Mult, {desc=Int n}, {desc=BinOp(Mult, {desc=Int m}, t)}) ->
          (eval {desc=BinOp(Mult, {desc=Int(n * m);typ=TInt[]}, t);typ=TInt[]}).desc
      | BinOp(op, t1, t2) ->
          BinOp(op, eval t1, eval t2)
      | Not t ->
          Not(eval t)
      | Fail ->
          Fail
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
      | Label(b,t) ->
          Label(b, eval t)
      | Event s -> Event s
  in
    {desc=desc; typ=t.typ}



(*
let rec eta = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | Var x -> Var x
  | App(t, ts) ->
      App(eta t, List.map eta ts)
  | If(t1, t2, t3, t4) ->
      If(eta t1, eta t2, eta t3, eta t4)
  | Branch(t1, t2) ->
      Branch(eta t1, eta t2)
  | Let(f, xs, t1, t2) ->
      Let(f, xs, eta t1, eta t2)
  | Letrec(f, xs, t1, t2) ->
      Letrec(f, xs, eta t1, eta t2)
  | BinOp(op, t1, t2) ->
      BinOp(op, eta t1, eta t2)
  | Not t ->
      Not(eta t)
  | Fail ->
      Fail
  | Fun(x,t) ->
      Fun(x, eta t)
  | Label(b,t) ->
      Label(b, eta t)
*)


(*
let rec simplify = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | Var x -> Var x
  | App(t, []) ->
      simplify t
  | App(App(t, ts1), ts2) ->
      simplify (App(t, ts1@ts2))
  | App(t, ts) ->
      let t' = simplify t in
      let ts' = List.map simplify ts in
        App(t', ts')
  | If(t1, t2, t3, t4) ->
      let t1' = simplify t1 in
      let t2' = simplify t2 in
      let t3' = simplify t3 in
      let t4' = simplify t4 in
        If(t1', t2', t3', t4')
  | Branch(t1, t2) ->
      let t1' = simplify t1 in
      let t2' = simplify t2 in
        Branch(t1', t2')
  | Let(flag, [f, xs, Let(Nonrecursive, [g, ys, t1], Var h)], t2) when g = h ->
      let t1' = simplify t1 in
      let t2' = simplify t2 in
        Let(flag, [f, xs@ys, t1'], t2')
  | Let(flag, bindings, t) ->
      let bindings' = List.map (fun (f,xs,t) -> f,xs,simplify t) bindings in
      let t' = simplify t in
        Let(flag, bindings', t')
  | BinOp(op, t1, t2) ->
      let t1' = simplify t1 in
      let t2' = simplify t2 in
        BinOp(op, t1', t2')
  | Not t ->
      let t' = simplify t in
        Not t'
  | Fail -> Fail
  | Fun(x,t) ->
      let t' = simplify t in
        Fun(x, t')
  | Label(b,t) ->
      let t' = simplify t in
        Label(b, t')
  | Event s -> Event s
*)
(*
let simplify _ = Format.printf "Not implemented@."; assert false
*)



let rec merge_let_fun t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | NInt x -> NInt x
      | RandInt None -> RandInt None
      | RandInt (Some t) -> RandInt (Some (merge_let_fun t))
      | Var x -> Var x
      | App(t, ts) -> App(merge_let_fun t, List.map merge_let_fun ts)
      | If(t1, t2, t3) -> If(merge_let_fun t1, merge_let_fun t2, merge_let_fun t3)
      | Branch(t1, t2) -> Branch(merge_let_fun t1, merge_let_fun t2)
      | Let(flag, f, xs, {desc=Fun(x,t1)}, t2) -> (merge_let_fun {desc=Let(flag, f, xs@[x], t1, t2); typ=t.typ}).desc
      | Let(flag, f, xs, {desc=Let(Flag.Nonrecursive, g1, ys, t1, {desc=Var g2})}, t2) when Id.same g1 g2 ->
          (merge_let_fun {desc=Let(flag, f, xs@ys, t1, t2); typ=t.typ}).desc
      | Let(flag, f, xs, t1, t2) -> Let(flag, f, xs, merge_let_fun t1, merge_let_fun t2)
      | Fun(x, t) -> Fun(x, merge_let_fun t)
      | BinOp(op, t1, t2) -> BinOp(op, merge_let_fun t1, merge_let_fun t2)
      | Not t -> Not (merge_let_fun t)
      | Fail -> Fail
      | Label(b, t) -> Label(b, merge_let_fun t)
      | Event s -> Event s
      | Record(b,fields) -> Record(b, List.map (fun (f,(s,t)) -> f,(s,merge_let_fun t)) fields)
      | Proj(n,i,s,f,t) -> Proj(n,i,s,f,merge_let_fun t)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,merge_let_fun t1,merge_let_fun t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(merge_let_fun t1, merge_let_fun t2)
      | Constr(s,ts) -> Constr(s, List.map merge_let_fun ts)
      | Match(t1,t2,x,y,t3) -> Match(merge_let_fun t1, merge_let_fun t2, x, y, merge_let_fun t3)
      | Match_(t,pats) -> Match_(merge_let_fun t, List.map (fun (pat,cond,t) -> pat,cond,merge_let_fun t) pats)
      | TryWith(t,pats) -> TryWith(merge_let_fun t, List.map (fun (pat,cond,t) -> pat,cond,merge_let_fun t) pats)
  in
    {desc=desc; typ=t.typ}




let imply t1 t2 = {desc=BinOp(Or, {desc=Not t1;typ=TBool}, t2); typ=TBool}
let and_list ts = match ts with
    [] -> {desc=True; typ=TBool}
  | [t] -> t
  | t::ts -> List.fold_left (fun t1 t2 -> {desc=BinOp(And,t1,t2);typ=TBool}) t ts


(*
let rec decomp_fun = function
    {desc=Fun(x,t)} ->
      let xs,t' = decomp_fun t in
        x::xs, t'
  | _ -> [], t
*)


let rec lift_aux xs t =
  let defs,desc =
    match t.desc with
        Unit -> [], Unit
      | True -> [], True
      | False -> [], False
      | Unknown -> [], Unknown
      | Int n -> [], Int n
      | NInt x -> [], NInt x
      | RandInt None -> [], RandInt None
      | RandInt (Some t) ->
          let defs,t' = lift_aux xs t in
            defs, RandInt (Some t')
      | Var x -> [], Var x
      | Fun(x,t1) ->
          let f = Id.new_var "f" t.typ in
          let defs,t1' = lift_aux xs {desc=Let(Flag.Nonrecursive,f,[x],t1,{desc=Var f;typ=t.typ});typ=t.typ} in
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
      | Let(Flag.Nonrecursive,f,ys,t1,t2) ->
          let fv = xs in
          let ys' = fv @ ys in
          let typ = List.fold_right (fun x typ -> TFun(x,typ)) fv (Id.typ f) in
          let f' = Id.new_var (Id.name f) typ in
          let defs1,t1' = lift_aux ys' t1 in
          let f'' = List.fold_left (fun t x -> app2app t [make_var x]) (make_var f') fv in
(*
          let f''',ys'',t1'' =
            if ys' = []
            then
              let u = Id.new_var "u" TUnit in
                Id.set_typ f' (TFun(u,Id.typ f')), [u], app2app t1' [unit_term]
            else f'', ys', t1'
          in
*)
          let defs2,t2' = lift_aux xs (subst f f'' t2) in
            defs1 @ [(f',(ys',t1'))] @ defs2, t2'.desc
      | Let(Flag.Recursive,f,ys,t1,t2) ->
          let fv = xs in
          let ys' = fv @ ys in
          let typ = List.fold_right (fun x typ -> TFun(x,typ)) fv (Id.typ f) in
          let f' = Id.new_var (Id.name f) typ in
          let f'' = List.fold_left (fun t x -> app2app t [{desc=Var x;typ=Id.typ x}]) (make_var f') fv in
          let defs1,t1' = lift_aux ys' (subst f f'' t1) in
          let defs2,t2' = lift_aux xs (subst f f'' t2) in
          let ys'',t1'' = if ys' = [] then [Id.new_var "u" TUnit], app2app t1' [unit_term] else ys', t1' in
            defs1 @ [(f',(ys',t1'))] @ defs2, t2'.desc
      | BinOp(op,t1,t2) ->
          let defs1,t1' = lift_aux xs t1 in
          let defs2,t2' = lift_aux xs t2 in
            defs1 @ defs2, BinOp(op,t1',t2')
      | Not t ->
          let defs,t' = lift_aux xs t in
            defs, Not t'
      | Fail -> [], Fail
      | Label(b,t) ->
          let defs,t' = lift_aux xs t in
            defs, Label(b,t')
      | Event s -> [], Event s
      | Record(b,fields) ->
          let aux (s,(f,t)) =
            let defs,t' = lift_aux xs t in
              defs, (s,(f,t'))
          in
          let defss,fields' = List.split (List.map aux fields) in
            List.flatten defss, Record(b,fields')
      | Proj(n,i,s,f,t) ->
          let defs,t' = lift_aux xs t in
            defs, Proj(n,i,s,f,t')
      | Nil -> [], Nil
      | Cons(t1,t2) ->
          let defs1,t1' = lift_aux xs t1 in
          let defs2,t2' = lift_aux xs t2 in
            defs1 @ defs2, Cons(t1',t2')
      | Match(t1,t2,x,y,t3) ->
          let defs1,t1' = lift_aux xs t1 in
          let defs2,t2' = lift_aux xs t2 in
          let defs3,t3' = lift_aux xs t3 in
            defs1 @ defs2 @ defs3, Match(t1',t2',x,y,t3')
      | Constr(c,ts) ->
          let defss,ts' = List.split (List.map (lift_aux xs) ts) in
            List.flatten defss, Constr(c,ts')
      | Match_(t,pats) ->
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
            defs', Match_(t',pats')
  in
    defs, {desc=desc; typ=t.typ}
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
      | RandInt None -> RandInt None
      | RandInt (Some t) -> RandInt (Some (canonize t))
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
      | Let(flag, f, xs, t1, t2) ->
          let t1' = canonize t1 in
          let t2' = canonize t2 in
            Let(flag, f, xs, t1', t2')
              (*
                | Let(flag, bindings, t) ->
                let bindings' = List.map (fun (f,xs,t) -> f,xs,canonize t) bindings in
                let t' = canonize t in
                Let(flag, bindings', t')
              *)
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
      | Fail -> Fail
      | Fun(x,t) ->
          let t' = canonize t in
            Fun(x, t')
      | Label(b,t) ->
          let t' = canonize t in
            Label(b, t')
      | Event s -> Event s
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(canonize t1, canonize t2)
      | Match(t1,t2,x,y,t3) -> Match(canonize t1, canonize t2, x, y, canonize t3)
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
        | RandInt None -> RandInt None
        | RandInt (Some t) -> RandInt (Some (aux apply t))
        | Var x ->
            begin
              try
                let xs, t1 = List.assoc x apply in
                  Let(Flag.Nonrecursive, x, xs, t1, {desc=Var x;typ=t.typ})
              with Not_found -> Var x
            end
        | Fun(x, t) -> Fun(x, aux apply t)
        | App({desc=Var f}, ts) ->
            if List.mem_assoc f apply
            then
              match ts with
                  [] ->
                    let xs, t1 = List.assoc f apply in
                      Let(Flag.Nonrecursive, f, xs, t1, {desc=Var f;typ=Id.typ f})
                | [t] -> t.desc
                | t::ts' -> App(t, ts')
            else
              let ts' = List.map (aux apply) ts in
                App({desc=Var f;typ=Id.typ f}, ts')
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
        | Let(flag, f, xs, t1, t2) ->
            if is_apply xs t1.desc
            then (aux ((f,(xs,t1))::apply) (aux apply t2)).desc
            else
              begin
                match flag, is_alias xs t1.desc  with
                    Flag.Nonrecursive, None -> Let(flag, f, xs, aux apply t1, aux apply t2)
                  | Flag.Nonrecursive, Some x -> (subst f {desc=Var x;typ=Id.typ x} (aux apply t2)).desc
                  | Flag.Recursive, Some x when not (List.mem f (get_fv t1)) -> (subst f {desc=Var x;typ=Id.typ x} (aux apply t2)).desc
                  | Flag.Recursive, _ -> Let(flag, f, xs, aux apply t1, aux apply t2)
              end
        | BinOp(op, t1, t2) -> BinOp(op, aux apply t1, aux apply t2)
        | Not t -> Not (aux apply t)
        | Fail -> Fail
        | Unknown -> Unknown
        | Label(b, t) -> Label(b, aux apply t)
        | LabelInt(n, t) -> LabelInt(n, aux apply t)
        | Event s -> Event s
        | Record(b,fields) -> Record(b, List.map (fun (s,(f,t)) -> s,(f,aux apply t)) fields)
        | Proj(n, i,s,f,t) -> Proj(n, i, s, f, aux apply t)
        | Nil -> Nil
        | Cons(t1,t2) -> Cons(aux apply t1, aux apply t2)
        | Match(t1,t2,x,y,t3) -> Match(aux apply t1, aux apply t2, x, y, aux apply t3)
        | Constr(c,ts) -> Constr(c, List.map (aux apply) ts)
        | Match_(t,pats) ->
            let aux' (pat,cond,t) = pat, apply_opt (aux apply) cond, aux apply t in
              Match_(aux apply t, List.map aux' pats)
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
    | Event s -> Event s
  in
  let t' = aux t in
  let t'' = simplify t' in
    t''
*)



















let rec add_string str t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | NInt x -> NInt (Id.add_name x str)
      | Var x -> Var (Id.add_name x str)
      | App(t, ts) ->
          let t' = add_string str t in
          let ts' = List.map (add_string str) ts in
            App(t', ts')
      | If(t1, t2, t3) ->
          let t1' = add_string str t1 in
          let t2' = add_string str t2 in
          let t3' = add_string str t3 in
            If(t1', t2', t3')
      | Branch(t1, t2) ->
          let t1' = add_string str t1 in
          let t2' = add_string str t2 in
            Branch(t1', t2')
      | Let _ -> Format.printf "Not implemented@."; assert false
          (*
            | Let(f, xs, t1, t2) ->
            let f' = add_string_to_var str f in
            let xs' = List.map (add_string_to_var str) xs in
            let t1' = add_string str t1 in
            let t2' = add_string str t2 in
            Let(f', xs', t1', t2')
            | Letrec(f, xs, t1, t2) ->
            let f' = add_string_to_var str f in
            let xs' = List.map (add_string_to_var str) xs in
            let t1' = add_string str t1 in
            let t2' = add_string str t2 in
            Letrec(f', xs', t1', t2')
          *)
      | BinOp(op, t1, t2) ->
          let t1' = add_string str t1 in
          let t2' = add_string str t2 in
            BinOp(op, t1', t2')
      | Not t ->
          let t' = add_string str t in
            Not t'
      | Fail -> Fail
      | Fun(x,t) ->
          let x' = Id.add_name x str in
          let t' = add_string str t in
            Fun(x', t')
      | Label(b,t) ->
          let t' = add_string str t in
            Label(b, t')
      | Event s -> Event s
  in
    {desc=desc; typ=t.typ}







let rec remove_unused t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | NInt x -> NInt x
      | Var x -> Var x
      | Fun _ -> assert false
      | App(t, ts) ->
          let t' = remove_unused t in
          let ts' = List.map remove_unused ts in
            App(t', ts')
      | If(t1, t2, t3) ->
          let t1' = remove_unused t1 in
          let t2' = remove_unused t2 in
          let t3' = remove_unused t3 in
            If(t1', t2', t3')
      | Branch(t1, t2) ->
          let t1' = remove_unused t1 in
          let t2' = remove_unused t2 in
            Branch(t1', t2')
      | Let(flag, f, xs, t1, t2) ->
          if List.mem f (get_fv t2) || xs = []
          then
            let t1' = remove_unused t1 in
            let t2' = remove_unused t2 in
              Let(flag, f, xs, t1', t2')
          else
            (remove_unused t2).desc
      | BinOp(op, t1, t2) ->
          let t1' = remove_unused t1 in
          let t2' = remove_unused t2 in
            BinOp(op, t1', t2')
      | Not t ->
          let t' = remove_unused t in
            Not t'
      | Fail -> Fail
      | Label _ -> assert false
      | Event s -> Event s
  in
    {desc=desc; typ=t.typ}




(*
let rec update_ident_uniq env = function
    Unit -> ()
  | True -> ()
  | False -> ()
  | Unknown -> ()
  | Int n -> ()
  | Var x -> (try x.uniq <- List.assoc x.name env = x.id with Not_found -> ()(****))
  | NInt x -> ()
  | RandInt None -> ()
  | RandInt (Some t) -> update_ident_uniq env t
  | Fun(x,t) -> update_ident_uniq ((x.name,x.id)::env) t
  | App(t, ts) -> List.iter (update_ident_uniq env) (t::ts)
  | If(t1, t2, t3) -> List.iter (update_ident_uniq env) [t1;t2;t3]
  | Branch(t1, t2) -> update_ident_uniq env t1; update_ident_uniq env t2
  | Let(flag, f, xs, t1, t2) ->
      let xs' = if flag = Nonrecursive then xs else f::xs in
      let env1 = List.map (fun x -> x.name, x.id) xs' @ env in
      let env2 = (f.name, f.id) :: env in
        update_ident_uniq env1 t1;
        update_ident_uniq env2 t2
  | BinOp(op, t1, t2) -> update_ident_uniq env t1; update_ident_uniq env t2
  | Not t -> update_ident_uniq env t
  | Fail -> ()
  | Label(b, t) -> update_ident_uniq env t
  | Event s -> ()
  | Record(b,fields) -> List.iter (fun (_,(_,t)) -> update_ident_uniq env t) fields
  | Proj(n,i,s,f,t) -> update_ident_uniq env t
  | Nil -> ()
  | Cons(t1,t2) -> update_ident_uniq env t1; update_ident_uniq env t2
  | Constr(s,ts) -> List.iter (update_ident_uniq env) ts
  | Match(t1,t2,x,y,t3) -> assert false
  | Match_(t,pats) ->
      let aux (pat,cond,t) =
        let xs = get_vars_pat pat in
        let env' = List.map (fun x -> x.name, x.id) xs @ env in
          match cond with None -> () | Some cond -> update_ident_uniq env' cond;
          update_ident_uniq env' t
      in
        update_ident_uniq env t;
        List.iter aux pats
  | Type_decl(decls,t) -> update_ident_uniq env t
let update_ident_uniq = update_ident_uniq []
*)



let rec print_typ t = Type.print (print_term ML 0 false) t
and print_ids fm = function
    [] -> ()
  | x::xs -> fprintf fm "%a %a" Id.print x print_ids xs

(*
and print_id fm x = fprintf fm "(%a:%a)" Id.print x print_typ (Id.typ x)
*)
and print_id = Id.print

and print_id_typ fm x =
  match Id.typ x with
      TVar _ | TUnknown -> fprintf fm "%a" Id.print x
    | _ -> fprintf fm "(%a:%a)" Id.print x print_typ (Id.typ x)

and print_ids_typ fm = function
    [] -> ()
  | x::xs -> fprintf fm "%a %a" print_id_typ x print_ids_typ xs




(* priority (low -> high)
   1 : If, Let, Letrec, Match
   2 : Fun
   3 : Or
   4 : And
   5 : Eq, Lt, Gt, Leq, Geq
   6 : Add, Sub
   7 : Cons
   8 : App
*)

and paren pri p = if pri < p then "","" else "(",")"

and print_binop syntax typ fm op =
  match syntax with
      ML | TRecS ->
        begin
          match op with
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
        end
    | CVC3 ->
        begin
          match op with
              Eq ->
                if typ = TBool
                then fprintf fm "<=>"
                else fprintf fm "="
            | Lt -> fprintf fm "<"
            | Gt -> fprintf fm ">"
            | Leq -> fprintf fm "<="
            | Geq -> fprintf fm ">="
            | And -> fprintf fm "AND"
            | Or -> fprintf fm "OR"
            | Add -> fprintf fm "+"
            | Sub -> fprintf fm "-"
            | Mult -> fprintf fm "*"
        end
    | CSIsat ->
        begin
          match op with
              Eq -> fprintf fm "="
            | Lt -> fprintf fm "<"
            | Gt -> fprintf fm ">"
            | Leq -> fprintf fm "<="
            | Geq -> fprintf fm ">="
            | And -> fprintf fm "&"
            | Or -> fprintf fm "|"
            | Add -> fprintf fm "+"
            | Sub -> fprintf fm "-"
            | Mult -> fprintf fm "*"
        end

and print_termlist syntax pri typ fm = List.iter (fun bd -> fprintf fm "@;%a" (print_term syntax pri typ) bd)
and print_term syntax pri typ fm t =
  match t.desc with
      Unit -> fprintf fm "unit"
    | True ->
        begin
          match syntax with
              TRecS -> fprintf fm "0"
            | CVC3 -> fprintf fm "TRUE"
            | _ -> fprintf fm "true"
        end
    | False ->
        begin
          match syntax with
              TRecS -> fprintf fm "1"
            | CVC3 -> fprintf fm "FALSE"
            | _ -> fprintf fm "false"
        end
    | Unknown ->
        begin
          match syntax with
              TRecS -> fprintf fm "2"
            | _ -> fprintf fm "***"
        end
    | Int n -> fprintf fm "%d" n
    | NInt x ->
        begin
          match syntax with
              ML -> fprintf fm "?%a?" print_id x
            | _ -> print_id fm x
        end
    | RandInt None ->
        let p = 8 in
        let s1,s2 = paren pri p in
          fprintf fm "%srand_int()%s" s1 s2
    | RandInt (Some t) ->
        let p = 8 in
        let s1,s2 = paren pri p in
          fprintf fm "%srand_int %a%s" s1 (print_term syntax p typ) t s2
    | RandValue(typ',None) ->
        let p = 8 in
        let s1,s2 = paren pri p in
          fprintf fm "%srand_val(%a)%s" s1 print_typ typ' s2
    | RandValue(typ',Some t) ->
        let p = 8 in
        let s1,s2 = paren pri p in
          fprintf fm "%srand_val(%a,%a)%s" s1 print_typ typ' (print_term syntax p typ) t s2
    | Var x -> print_id fm x
    | Fun(x, t) ->
        let p = 2 in
        let s1,s2 = paren pri p in
          fprintf fm "%sfun %a -> %a%s" s1 print_id x (print_term syntax p typ) t s2
    | App(t, ts) ->
        let p = 8 in
        let s1,s2 = paren pri p in
          fprintf fm "%s%a%a%s" s1 (print_term syntax p typ) t (print_termlist syntax p typ) ts s2
    | If(t1, t2, t3) ->
        begin
          match syntax with
              TRecS ->
                let p = 8 in
                let s1,s2 = paren pri (p+1) in
                  fprintf fm "%s_case 2 %a %a %a%s"
                    s1 (print_term syntax p typ) t1 (print_term syntax p typ) t2 (print_term syntax p typ) t3 s2
            | _ ->
                let p = 1 in
                let s1,s2 = paren pri (p+1) in
                  fprintf fm "%s@[@[if %a@]@;then @[%a@]@;else @[%a@]@]%s"
                    s1 (print_term syntax p typ) t1 (print_term syntax p typ) t2 (print_term syntax p typ) t3 s2
        end
    | Branch(t1, t2) ->
        let p = 8 in
        let s1,s2 = paren pri p in
          fprintf fm "%sbr %a %a%s" s1 (print_term syntax p typ) t1 (print_term syntax p typ) t2 s2
    | Let(flag, f, xs, t1, t2) ->
        let s_rec = match flag with Flag.Nonrecursive -> "" | Flag.Recursive -> " rec" in
        let p = 1 in
        let s1,s2 = paren pri (p+1) in
        let p_ids fm () =
          if typ
          then fprintf fm "%a %a" print_id f print_ids_typ xs
          else fprintf fm "%a" print_ids (f::xs)
        in
          begin
            match t2.desc with
                Let _ -> fprintf fm "%s@[<v>@[<hov 2>let%s %a= @,%a@]@;in@;%a@]%s"
                  s1 s_rec p_ids () (print_term syntax p typ) t1 (print_term syntax p typ) t2 s2
              | _ -> fprintf fm "%s@[<v>@[<hov 2>let%s %a= @,%a @]@;@[<v 2>in@;@]@[<hov>%a@]@]%s"
                  s1 s_rec p_ids () (print_term syntax p typ) t1 (print_term syntax p typ) t2 s2
          end
    | BinOp(Mult, t1, t2) when
        syntax = CVC3 &&
        (match t1.desc with Int(_) -> false | _ -> (match t2.desc with Int(_) -> false | _ -> true)) ->
        let t = {desc=BinOp(Mult, t1, t2);typ=TInt[]} in
          Format.printf "Nonlinear expression not supported: %a@." (print_term ML 0 true) t;
          assert false
    | BinOp(op, t1, t2) ->
        if op = Eq && syntax = CVC3 && t1.typ = TBool
        then
          let p = 5 in
          let s1,s2 = paren pri p in
            fprintf fm "%s%a <=> %a%s" s1 (print_term syntax p typ) t1 (print_term syntax p typ) t2 s2
        else
          let p = match op with Add|Sub|Mult -> 6 | And -> 4 | Or -> 3 | _ -> 5 in
          let s1,s2 = paren pri p in
            fprintf fm "%s%a %a %a%s" s1 (print_term syntax p typ) t1 (print_binop syntax t1.typ) op (print_term syntax p typ) t2 s2
    | Not t ->
        let p = 6 in
        let s1,s2 = paren pri p in
        let nt = match syntax with CVC3 -> "NOT" | _ -> "not" in
          fprintf fm "%s%s %a%s" s1 nt (print_term syntax p typ) t s2
    | Fail -> fprintf fm "fail"
    | Label(true, t) ->
        let p = 8 in
        let s1,s2 = paren pri p in
        let s =
          match syntax with
              TRecS -> "then"
            | _ -> "l_then"
        in
          fprintf fm "%s%s %a%s" s1 s (print_term syntax p typ) t s2
    | Label(false, t) ->
        let p = 8 in
        let s1,s2 = paren pri p in
        let s =
          match syntax with
              TRecS -> "else"
            | _ -> "l_else"
        in
          fprintf fm "%s%s %a%s" s1 s (print_term syntax p typ) t s2
    | LabelInt(n, t) ->
        let p = 8 in
        let s1,s2 = paren pri p in
          fprintf fm "%sbr%d %a%s" s1 n (print_term syntax p typ) t s2
    | Event s ->
        begin
          match syntax with
              TRecS -> fprintf fm "%s" s
            | _ -> fprintf fm "{%s}" s
        end
    | Record(b,fields) ->
        let rec aux fm = function
            [] -> ()
          | (s,(f,t))::fields ->
              match b, fields=[] with
                  true,true -> fprintf fm "%a" (print_term syntax 0 typ) t
                | true,false -> fprintf fm "%a, %a" (print_term syntax 0 typ) t aux fields
                | false,true -> fprintf fm "%s=%a" s (print_term syntax 0 typ) t
                | false,false -> fprintf fm "%s=%a; %a" s (print_term syntax 0 typ) t aux fields
        in
          if b
          then fprintf fm "(%a)" aux fields
          else fprintf fm "{%a}" aux fields
    | Proj(_,_,s,_,t) -> fprintf fm "%a.%s" (print_term syntax 9 typ) t s
    | SetField(_,_,s,_,t1,t2) -> fprintf fm "%a.%s <- %a" (print_term syntax 9 typ) t1 s (print_term syntax 3 typ) t2
    | Nil ->
        let s =
          match syntax with
              CVC3 -> "nil"
            | _ -> "[]"
        in
          fprintf fm "%s" s
    | Cons(t1,t2) ->
        let p = 7 in
        let s1,s2 = paren pri (p+1) in
          begin
            match syntax with
                CVC3 -> fprintf fm "%scons(%a)%s" s1 (print_term syntax p typ) t2 s2
              | _ -> fprintf fm "%s%a::%a%s" s1 (print_term syntax p typ) t1 (print_term syntax p typ) t2 s2
          end
    | Constr(s,ts) ->
        let p = 8 in
        let s1,s2 = paren pri p in
        let aux fm = function
            [] -> ()
          | [t] -> fprintf fm "(%a)" (print_term syntax 1 typ) t
          | t::ts ->
              fprintf fm "(%a" (print_term syntax 1 typ) t;
              List.iter (fun t -> fprintf fm ",%a" (print_term syntax 1 typ) t) ts;
              pp_print_string fm ")"
        in
          fprintf fm "%s%s%a%s" s1 s aux ts s2
    | Match(t1,t2,x,y,t3) ->
        let p = 1 in
        let s1,s2 = paren pri (p+1) in
          (*
            fprintf fm "%s@[<v>@[<hov 2>let%s %a= @,%a @]@;@[<v 2>in@;@]@[<hov>%a@]@]%s" s1 s_rec p_ids () (print_term syntax p typ) t1 (print_term syntax p typ) t2 s2
          *)
          fprintf fm "%s@[match %a with@;[] -> %a@;| %a::%a -> %a@]%s"
            s1
            (print_term syntax p typ) t1
            (print_term syntax p typ) t2
            print_id x
            print_id y
            (print_term syntax p typ) t3
            s2
    | Match_(t,pats) ->
        let p = 1 in
        let s1,s2 = paren pri (p+1) in
        let aux = function
            (pat,None,t) -> fprintf fm "%a -> %a@;" print_pattern pat (print_term syntax p typ) t
          | (pat,Some cond,t) -> fprintf fm "%a when %a -> %a@;"
              print_pattern pat (print_term syntax p typ) cond (print_term syntax p typ) t
        in
          fprintf fm "%smatch %a with@;" s1 (print_term syntax p typ) t;
          List.iter aux pats;
          pp_print_string fm s2
    | TryWith(t,pats) ->
        let p = 1 in
        let s1,s2 = paren pri (p+1) in
        let aux = function
            (pat,None,t) -> fprintf fm "%a -> %a@;" print_pattern pat (print_term syntax p typ) t
          | (pat,Some cond,t) -> fprintf fm "%a when %a -> %a@;"
              print_pattern pat (print_term syntax p typ) cond (print_term syntax p typ) t
        in
          fprintf fm "%stry %a with@;" s1 (print_term syntax p typ) t;
          List.iter aux pats;
          pp_print_string fm s2


and print_pattern fm pat =
  let rec aux fm pat =
    match pat.pat_desc with
      PVar x -> print_id fm x
    | PConst c -> print_term ML 1 false fm c
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
    | PCons(p1,p2) -> fprintf fm "%a::%a" print_pattern p1 print_pattern p2
    | PRecord(true,pats) ->
        let aux' = function
            [] -> ()
          | [_,(_,_,pat)] -> fprintf fm "(%a)" aux pat
          | (_,(_,_,pat))::pats ->
              fprintf fm "(%a" aux pat;
              List.iter (fun (_,(_,_,pat)) -> fprintf fm ",%a" aux pat) pats;
              pp_print_string fm ")"
        in
          aux' pats
    | PRecord(false,pats) ->
        let aux' = function
            [] -> ()
          | [_,(s,_,pat)] -> fprintf fm "(%s=%a)" s aux pat
          | (_,(s,_,pat))::pats ->
              fprintf fm "(%s=%a" s aux pat;
              List.iter (fun (_,(s,_,pat)) -> fprintf fm ",%s=%a" s aux pat) pats;
              pp_print_string fm ")"
        in
          aux' pats
    | POr(pat1,pat2) ->
        fprintf fm "(%a | %a)" aux pat1 aux pat2
  in
    fprintf fm "| %a" aux pat


let string_of_ident x =
  print_id str_formatter x;
  flush_str_formatter ()
let string_of_term syntax t =
  print_term syntax 0 false str_formatter t;
  flush_str_formatter ()

let string_of_node = function
    BrNode -> assert false
  | LabNode true -> "then"
  | LabNode false -> "else"
  | FailNode -> "fail"
  | PatNode n -> "br" ^ string_of_int n
  | EventNode s -> s




let print_hors fm (funs, spec) =
  let print_fundef (f, (xs, t)) =
(*
    let () = if true then Format.printf "DEBUG:%a %a-> %a.\n" print_id f print_ids xs (print_term ML 0 false) t in
     if f.id<>815 then () else ();
*)
    fprintf fm "%a %a-> %a.\n" print_id f print_ids xs (print_term TRecS 0 false) t
  in
    fprintf fm "%%BEGING\n";
    List.iter print_fundef funs;
    fprintf fm "%%ENDG\n\n";
    fprintf fm "%%BEGINA\n";
    Automata.print_buchi fm spec;
    fprintf fm "%%ENDA\n\n"



let print_term_fm syntax typ fm t =
  print_term syntax 0 typ fm t;
  printf "@?"

and print_term syntax typ t =
  print_term syntax 0 typ std_formatter t;
  printf "@?"

and print_term_fm_break syntax typ fm t =
  print_term syntax 0 typ fm t;
  printf "@."

and print_term_break syntax typ t =
  print_term syntax 0 typ std_formatter t;
  printf "@."






let print_constr fm = function
    Cond t -> print_term_fm ML false fm t
  | Pred(f,n,x,ts) -> Format.printf "P_{%a_%d}^%a(%a)" print_id f n print_id x (print_termlist ML 0 false) ts
let print_constr_list fm = List.iter (fun c -> Format.fprintf fm "@;[%a]" print_constr c)



let pp_print_typ = print_typ
let pp_print_term = print_term_fm ML false



let rec eta_expand t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Int n -> Int n
      | NInt x -> NInt x
      | Var x -> Var x
      | Fun(x, t) -> assert false
          (*let f = new_var' "f" in
            eta_expand (Let(f, [x], t, Var f))*)
      | App(t, ts) ->
          let t' = eta_expand t in
          let ts' = List.map eta_expand ts in
            App(t', ts')
      | If(t1, t2, t3) ->
          let t1' = eta_expand t1 in
          let t2' = eta_expand t2 in
          let t3' = eta_expand t3 in
            If(t1', t2', t3')
      | Let _ -> Format.printf "Not implemented@."; assert false
          (*
            | Let(f, xs, t1, t2) ->
            let t1' = eta_expand t1 in
            let t2' = eta_expand t2 in

            let n = (List.length (get_args f.typ)) - (List.length xs) in
            let ds = tabulate n (fun _ -> new_var' "d") in
            let xs' = List.map2 (fun x x' -> {x with typ = x'.typ}) (xs @ ds) (get_args f.typ) in

            Let(f, xs', App(t1', List.map (fun d -> Var(d)) ds), t2')
            | Letrec(f, xs, t1, t2) ->
            let t1' = eta_expand t1 in
            let t2' = eta_expand t2 in

            let n = (List.length (get_args f.typ)) - (List.length xs) in
            let ds = tabulate n (fun _ -> new_var' "d") in
            let xs' = List.map2 (fun x x' -> {x with typ = x'.typ}) (xs @ ds) (get_args f.typ) in

            Letrec(f, xs', App(t1', List.map (fun d -> Var(d)) ds), t2')
          *)
      | BinOp(op, t1, t2) ->
          let t1' = eta_expand t1 in
          let t2' = eta_expand t2 in
            BinOp(op, t1', t2')
      | Fail -> Fail
      | Unknown -> Unknown
      | _ -> assert false
  in
    {desc=desc; typ=t.typ}


(*
let rec get_trace ce env trace t =
  match t,ce with
      Var x, _ -> get_trace ce env trace (App(Var x, []))
    | Unit, [FailNode] -> List.rev trace
    | App(Fail, _), [FailNode] -> List.rev trace
    | App(Event(s), [t]), [FailNode] -> List.rev trace
    | App(Event(s), [t]), EventNode(s')::ce' when s = s' ->
        let trace' = Event(s)::trace in
          get_trace ce' env trace' t
    | App(Var x, ts), _ ->
        (try
           let b,t' = List.assoc x env in
           let xs = get_args x.typ in
           let xs' = List.map (fun x -> {(new_var' x.name) with typ = x.typ}) xs in
           let t'' = List.fold_right2 subst xs (List.map (fun x -> Var x) xs') t' in
           let trace' = if b then (Var x)::trace else trace in
           let env' =
             let aux env x t =
               match x.typ with
                   TFun _ -> (x, (false,expand t x.typ))::env
                 | _ -> env
             in
               List.fold_left2 aux env xs' ts
           in
             get_trace ce env' trace' t''
         with Not_found -> List.rev trace(*assert false*))
    | App(Let(flag, f, xs, t1, t2), ts), _ ->
        get_trace ce env trace (Let(flag, f, xs, t1, App(t2, ts)))
    | Let(Nonrecursive, f, xs, t1, t2), _ ->
        let f' = {(new_var' f.name) with typ = f.typ} in
        let t2' = subst f (Var f') t2 in
        let env' = (f',(true,t1))::env in
          get_trace ce env' trace t2'
    | Let(Recursive, f, xs, t1, t2), _ ->
        let f' = {(new_var' f.name) with typ = f.typ} in
        let t1' = subst f (Var f') t1 in
        let t2' = subst f (Var f') t2 in
        let env' = (f',(true,t1'))::env in
          get_trace ce env' trace t2'
    | If(t1, t2, _), LabNode(true)::ce' ->
        let trace' = True::trace in
          get_trace ce' env trace' t2
    | If(t1, _, t3), LabNode(false)::ce' ->
        let trace' = False::trace in
          get_trace ce' env trace' t3
    | _ ->
        Format.printf "syntax.ml:@.%a@." (print_term_fm ML false) t;
        let () = List.iter (fun node -> print_msg (string_of_node node ^ " --> ")) ce in
        let () = print_msg ".\n" in
          assert false

let get_trace ce t = get_trace ce [] [] t
*)




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
      | BinOp(Eq|Lt|Gt|Leq|Geq as op, t1, t2) ->
          let neg xs = List.map (fun (x,n) -> x,-n) xs in
          let rec decomp t =
            match t.desc with
                Int n -> [None, n]
              | Var x -> [Some {desc=Var x;typ=Id.typ x}, 1]
              | NInt x -> [Some {desc=NInt x;typ=Id.typ x}, 1]
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
                            Format.printf "Nonlinear expression not supported: %a@." pp_print_term {desc=BinOp(op,t1,t2);typ=TInt[]};
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
                None -> max_int
              | Some {desc=Var x} -> Id.id x
              | Some {desc=NInt n} -> Id.id n
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
                      None,n -> {desc=Int n; typ=TInt[]}
                    | Some x,n -> if n=1 then x else {desc=BinOp(Mult, {desc=Int n;typ=TInt[]}, x); typ=TInt[]}
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
                        [] -> {desc=Int 0; typ=TInt[]}
                      | t::ts' -> List.fold_left (fun t2 t -> {desc=BinOp(Add, t2, t);typ=TInt[]}) t ts'
                  in
                    op', t1, t2
          in
          let rec simplify t =
            let desc =
              match t.desc with
                  BinOp(Add, t1, {desc=BinOp(Mult, {desc=Int n}, t2)}) when n < 0 ->
                    let t1' = simplify t1 in
                      BinOp(Sub, t1', {desc=BinOp(Mult, {desc=Int(-n);typ=TInt[]}, t2); typ=TInt[]})
                | BinOp(Add, t1, {desc=Int n}) when n < 0 ->
                    let t1' = simplify t1 in
                      BinOp(Sub, t1', {desc=Int(-n);typ=TInt[]})
                | BinOp(Add, t1, t2) ->
                    let t1' = simplify t1 in
                      BinOp(Add, t1', t2)
                | t -> t
            in
              {desc=desc; typ=t.typ}
          in
            BinOp(op', t1', simplify t2')
      | Not t -> Not (normalize_bool_exp t)
      | Unit
      | Int _
      | NInt _
      | Fun _
      | App _
      | If _
      | Branch _
      | Let _
      | BinOp((Add|Sub|Mult), _, _)
      | Fail
      | Label _
      | Event _ -> assert false
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
    | NInt _
    | Fun _
    | App _
    | If _
    | Branch _
    | Let _
    | Fail
    | Label _
    | Event _ -> assert false

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
      | NInt _
      | Fun _
      | App _
      | If _
      | Branch _
      | Let _
      | BinOp((Add|Sub|Mult), _, _)
      | Fail
      | Label _
      | Event _ -> Format.printf "%a@." pp_print_term t; assert false
  in
    {desc=desc; typ=t.typ}
    

(*
let string_of_typ syntax t =
  print_typ syntax str_formatter t;
  flush_str_formatter ()
*)




let set_target t =
  let rec get_last_definition f t =
    match t.desc with
        Let(_, f, _, _, t2) -> get_last_definition (Some f) t2
      | Fun _ -> assert false
      | _ -> f
  in
  let rec replace_main main typ t =
    match t.desc with
        Let(flag, f, xs, t1, t2) -> {desc=Let(flag, f, xs, t1, replace_main main typ t2); typ=typ}
      | Fun _ -> assert false
      | _ -> {desc=main; typ=typ}
  in
  let rec get_base_typ = function
      TFun(_,typ) -> get_base_typ typ
    | typ -> typ
  in
    match get_last_definition None t with
        None -> assert false
      | Some f ->
          let xs = get_args (Id.typ f) in
          let aux x =
            match Id.typ x with
                TInt _ -> {desc=RandInt None; typ=TInt[]}
              | typ -> {desc=RandValue(typ, None); typ=typ}
          in
          let args = List.map aux xs in
          let main = app2app {desc=Var f;typ=Id.typ f} args in
          let main' =
            let u = Id.new_var "main" main.typ in
              Let(Flag.Nonrecursive, u, [], main, {desc=Unit;typ=TUnit})
          in
            replace_main main' TUnit t


(* Unit is used as base values *)
let rec eval_and_print_ce ce t =
  match t.desc with
      Unit -> ce, {desc=Unit; typ=TUnit}
    | True -> ce, {desc=Unit; typ=TUnit}
    | False -> ce, {desc=Unit; typ=TUnit}
    | Unknown -> ce, {desc=Unit; typ=TUnit}
    | Int n -> ce, {desc=Unit; typ=TUnit}
    | NInt x -> ce, {desc=Unit; typ=TUnit}
    | RandInt None -> ce, {desc=Unit; typ=TUnit}
    | RandInt (Some t) -> eval_and_print_ce ce (app2app t [{desc=Unit;typ=TUnit}])
    | Var x -> assert false
    | Fun(x,t) -> ce, t
    | App(t1,[]) -> assert false
    | App(t1,[t2]) ->
        let ce',t1' = eval_and_print_ce ce t1 in
        let ce'',t2' = eval_and_print_ce ce' t2 in
          begin
            match t1'.desc with
                Fun(x,t) -> eval_and_print_ce ce'' (subst x t2' t)
              | Event s -> eval_and_print_ce ce'' t2'
              | Fail -> ce'', {desc=Unit; typ=TUnit}
              | _ -> assert false
          end
    | App(t1,t2::ts) -> eval_and_print_ce ce {desc=App({desc=App(t1,[t2]);typ=TUnknown},ts);typ=t.typ}
    | If({desc=Unit},t2,t3)
    | Branch(t2,t3) ->
        begin
          match ce with
              LabNode true::ce' -> Format.printf "-then->@."; eval_and_print_ce ce' t2
            | LabNode false::ce' -> Format.printf "-else->@."; eval_and_print_ce ce' t3
            | [FailNode] -> ce, {desc=Fail;typ=TUnknown}
            | _ -> Format.printf "ce:%s@." (string_of_node (List.hd ce)); assert false
        end
    | If(t1,t2,t3) ->
        let ce',t1' = eval_and_print_ce ce t1 in
          eval_and_print_ce ce' {desc=If(t1',t2,t3);typ=t.typ}
    | Let(flag,f,xs,t1,t2) ->
        let t0 = {desc=App({desc=Event (Id.name f);typ=TUnknown}, [t1]);typ=t1.typ} in
        let t1' = List.fold_right (fun x t -> {desc=Fun(x,t);typ=TFun(x,t.typ)}) xs t0 in
        let ce',t1'' = eval_and_print_ce ce t1' in
        let t1''' =
          if flag = Flag.Nonrecursive
          then t1''
          else {desc=Label(true,{desc=Fun(f,t1'');typ=TUnknown});typ=TUnknown}
        in (* Label is used as fix *)
          eval_and_print_ce ce' (subst f t1''' t2)
    | BinOp(op, t1, t2) ->
        let ce',t2' = eval_and_print_ce ce t2 in
        let ce'',t1' = eval_and_print_ce ce' t1 in
          ce'', {desc=Unit; typ=TUnit}
    | Not t ->
        let ce',t' = eval_and_print_ce ce t in
          ce', {desc=Unit; typ=TUnit}
    | Fail -> ce, {desc=Fail; typ=TUnknown}
    | Label(b,t) ->
        let ce',t' = eval_and_print_ce ce t in
          begin
            match t'.desc with
                Fun(f,t'') -> eval_and_print_ce ce' (subst f {desc=Label(b,t);typ=TUnknown} t'')
              | _ -> assert false
          end
    | Event s ->
        Format.printf "%s -->@." s;
        ce, {desc=Event s; typ=TUnknown}
    | Record(b,fields) ->
        let ce' = List.fold_right (fun (_,(_,t)) ce -> fst (eval_and_print_ce ce t)) fields ce in
          ce', {desc=Unit; typ=TUnit}
    | Proj(_,_,_,_,t) ->
        let ce',t' = eval_and_print_ce ce t in
          ce', {desc=Unit; typ=TUnit}
    | Nil -> ce, {desc=Unit; typ=TUnit}
    | Cons(t1,t2) ->
        let ce',_ = eval_and_print_ce ce t2 in
        let ce'',_ = eval_and_print_ce ce' t1 in
          ce'', {desc=Unit; typ=TUnit}
    | Constr(c,ts) ->
        let ce' = List.fold_right (fun t ce -> fst (eval_and_print_ce ce t)) ts ce in
          ce', {desc=Unit; typ=TUnit}
    | Match_(t,pats) ->
        let ce',_ = eval_and_print_ce ce t in
          begin
            match ce' with
                PatNode n::ce'' -> assert false
                  (*;
                    Format.printf "-%d->@." n; eval_and_print_ce ce'' ( (List.nth pats n))*)
              | _ -> assert false
          end


let print_ce ce t =
  ignore (eval_and_print_ce ce t);
  Format.printf "error\n@."






let rec max_pat_num t =
  match t.desc with
      Unit -> 0
    | True -> 0
    | False -> 0
    | Unknown -> 0
    | Int _ -> 0
    | NInt _ -> 0
    | Var _ -> 0
    | Fun(_, t) -> max_pat_num t
    | App(t, ts) -> List.fold_left (fun acc t -> max acc (max_pat_num t)) (max_pat_num t) ts
    | If(t1, t2, t3) -> max (max (max_pat_num t1) (max_pat_num t2)) (max_pat_num t3)
    | Branch(t1, t2) -> max (max_pat_num t1) (max_pat_num t2)
    | Let(_, _, _, t1, t2) -> max (max_pat_num t1) (max_pat_num t2)
    | BinOp(_, t1, t2) -> max (max_pat_num t1) (max_pat_num t2)
    | Not t -> max_pat_num t
    | Fail -> 0
    | Label(_, t) -> max_pat_num t
    | LabelInt(_, t) -> max_pat_num t
    | Event _ -> 0
    | Nil -> 0
    | Cons(t1,t2) -> max (max_pat_num t1) (max_pat_num t2)
    | Constr(_,ts) -> List.fold_left (fun acc t -> max acc (max_pat_num t)) 0 ts
    | Match(t1,t2,_,_,t3) -> max (max (max_pat_num t1) (max_pat_num t2)) (max_pat_num t3)
    | Match_(t,pats) ->
        let m = max (List.length pats) (max_pat_num t) in
        let aux acc = function
            (_,None,t) -> max acc (max_pat_num t)
          | (_,Some cond,t) -> max acc (max (max_pat_num t) (max_pat_num cond))
        in
          List.fold_left aux m pats
    
let rec max_label_num t =
  match t.desc with
      Unit -> -1
    | True -> -1
    | False -> -1
    | Unknown -> -1
    | Int _ -> -1
    | NInt _ -> -1
    | RandInt None -> -1
    | RandInt (Some t) -> max_label_num t
    | Var _ -> -1
    | Fun(_, t) -> max_label_num t
    | App(t, ts) ->
        List.fold_left (fun acc t -> max acc (max_label_num t)) (max_label_num t) ts
    | If(t1, t2, t3) ->
        max (max (max_label_num t1) (max_label_num t2)) (max_label_num t3)
    | Branch(t1, t2) -> max (max_label_num t1) (max_label_num t2)
    | Let(_, _, _, t1, t2) -> max (max_label_num t1) (max_label_num t2)
    | BinOp(_, t1, t2) -> max (max_label_num t1) (max_label_num t2)
    | Not t -> max_label_num t
    | Fail -> -1
    | Label(_, t) -> max_label_num t
    | LabelInt(n, t) -> max n (max_label_num t)
    | Event _ -> -1
    | Nil -> -1
    | Cons(t1,t2) -> max (max_label_num t1) (max_label_num t2)
    | Constr(_,ts) ->
        List.fold_left (fun acc t -> max acc (max_label_num t)) (-1) ts
    | Match(t1,t2,_,_,t3) ->
        max (max (max_label_num t1) (max_label_num t2)) (max_label_num t3)
    | Match_(t,pats) ->
        let aux acc = function
            (_,None,t) -> max acc (max_label_num t)
          | (_,Some cond,t) -> max acc (max (max_label_num t) (max_label_num cond))
        in
          List.fold_left aux (-1) pats
    




let is_external x = String.contains (Id.name x) '.'


let rec init_rand_int t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | Var x -> Var x
      | NInt x -> NInt x
      | RandInt None -> Var (Id.new_var "_r" (TInt[]))
      | RandInt (Some t) -> (app2app t [{desc=Var (Id.new_var "_r" (TInt[])); typ=TInt[]}]).desc
      | Fun(x,t) -> Fun(x, init_rand_int t)
      | App(t,ts) -> App(init_rand_int t, List.map init_rand_int ts)
      | If(t1,t2,t3) -> If(init_rand_int t1, init_rand_int t2, init_rand_int t3)
      | Branch(t1,t2) -> Branch(init_rand_int t1, init_rand_int t2)
      | Let(flag,f,xs,t1,t2) -> Let(flag, f, xs, init_rand_int t1, init_rand_int t2)
      | BinOp(op, t1, t2) -> BinOp(op, init_rand_int t1, init_rand_int t2)
      | Not t -> Not (init_rand_int t)
      | Fail -> Fail
      | Label(b,t) -> Label(b, init_rand_int t)
      | Event s -> Event s
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(init_rand_int t1, init_rand_int t2)
      | Constr(s,ts) -> Constr(s, List.map init_rand_int ts)
      | Match(t1,t2,x,y,t3) -> Match(init_rand_int t1, init_rand_int t2, x, y, init_rand_int t3)
      | Match_(t,pats) -> Match_(init_rand_int t, List.map (fun (pat,cond,t) -> pat,apply_opt init_rand_int cond,init_rand_int t) pats)
  in
    {desc=desc; typ=t.typ}

    















let print_defs fm (defs:(id * (id list * typed_term)) list) =
  let print_fundef (f, (xs, t)) =
    fprintf fm "%a %a-> %a.\n" print_id f print_ids xs pp_print_term t
  in
    List.iter print_fundef defs
  








let rec copy_poly_funs t =
  let desc =
    match t.desc with
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
                Flag.Nonrecursive -> List.filter (Id.same f) (get_fv t1' @@ get_fv t2')
              | Flag.Recursive -> List.filter (Id.same f) (get_fv t2')
          in
            if fs = []
            then Let(flag, f, xs, t1', t2')
            else
              let fs =
                if List.for_all (fun f -> Type.can_unify (Id.typ f) (Id.typ (List.hd fs))) (List.tl fs)
                then [List.hd fs]
                else fs
              in
              let n = List.length fs in
              let () = if n >= 2 then Format.printf "COPY: %s(%d)@." (Id.name f) n in
              let aux t f =
                let f' = Id.new_var_id f in
                let typs = get_argtyps (Id.typ f) in
                let xs' = List.map2 (fun x typ -> Id.new_var (Id.name x) typ) xs (take typs (List.length xs)) in
                let map = List.map2 (fun x x' -> x, make_var x') xs xs' in
                let t1'' = subst_term map t1' in
                let t1''' = subst f (make_var f') t1'' in
                let t' = subst f (make_var f') t in
                  {desc=Let(flag, f', xs', t1''', t'); typ=t'.typ}
              in
                (List.fold_left aux t2' fs).desc
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
  in
    {desc=desc; typ=t.typ}























let rec print_term' syntax pri typ fm t =
fprintf fm "(";(
  match t.desc with
      Unit -> fprintf fm "unit"
    | True ->
        begin
          match syntax with
              TRecS -> fprintf fm "0"
            | CVC3 -> fprintf fm "TRUE"
            | _ -> fprintf fm "true"
        end
    | False ->
        begin
          match syntax with
              TRecS -> fprintf fm "1"
            | CVC3 -> fprintf fm "FALSE"
            | _ -> fprintf fm "false"
        end
    | Unknown ->
        begin
          match syntax with
              TRecS -> fprintf fm "2"
            | _ -> fprintf fm "***"
        end
    | Int n -> fprintf fm "%d" n
    | NInt x ->
        begin
          match syntax with
              ML -> fprintf fm "?%a?" print_id x
            | _ -> print_id fm x
        end
    | RandInt None ->
        let p = 8 in
        let s1,s2 = paren pri p in
          fprintf fm "%srand_int()%s" s1 s2
    | RandInt (Some t) ->
        let p = 8 in
        let s1,s2 = paren pri p in
          fprintf fm "%srand_int %a%s" s1 (print_term' syntax p typ) t s2
    | RandValue(typ',None) ->
        let p = 8 in
        let s1,s2 = paren pri p in
          fprintf fm "%srand_val(%a)%s" s1 print_typ typ' s2
    | RandValue(typ',Some t) ->
        let p = 8 in
        let s1,s2 = paren pri p in
          fprintf fm "%srand_val(%a,%a)%s" s1 print_typ typ' (print_term' syntax p typ) t s2
    | Var x -> print_id_typ fm x
    | Fun(x, t) ->
        let p = 2 in
        let s1,s2 = paren pri p in
          fprintf fm "%sfun %a -> %a%s" s1 print_id x (print_term' syntax p typ) t s2
    | App(t, ts) ->
        let p = 8 in
        let s1,s2 = paren pri p in
          fprintf fm "%s%a%a%s" s1 (print_term' syntax p typ) t (print_termlist' syntax p typ) ts s2
    | If(t1, t2, t3) ->
        begin
          match syntax with
              TRecS ->
                let p = 8 in
                let s1,s2 = paren pri (p+1) in
                  fprintf fm "%s_case 2 %a %a %a%s"
                    s1 (print_term' syntax p typ) t1 (print_term' syntax p typ) t2 (print_term' syntax p typ) t3 s2
            | _ ->
                let p = 1 in
                let s1,s2 = paren pri (p+1) in
                  fprintf fm "%s@[@[if %a@]@;then @[%a@]@;else @[%a@]@]%s"
                    s1 (print_term' syntax p typ) t1 (print_term' syntax p typ) t2 (print_term' syntax p typ) t3 s2
        end
    | Branch(t1, t2) ->
        let p = 8 in
        let s1,s2 = paren pri p in
          fprintf fm "%sbr %a %a%s" s1 (print_term' syntax p typ) t1 (print_term' syntax p typ) t2 s2
    | Let(flag, f, xs, t1, t2) ->
        let s_rec = match flag with Flag.Nonrecursive -> "" | Flag.Recursive -> " rec" in
        let p = 1 in
        let s1,s2 = paren pri (p+1) in
        let p_ids fm () =
          fprintf fm "%a" print_ids_typ (f::xs)
        in
          begin
            match t2.desc with
                Let _ -> fprintf fm "%s@[<v>@[<hov 2>let%s %a= @,%a@]@;in@;%a@]%s"
                  s1 s_rec p_ids () (print_term' syntax p typ) t1 (print_term' syntax p typ) t2 s2
              | _ -> fprintf fm "%s@[<v>@[<hov 2>let%s %a= @,%a @]@;@[<v 2>in@;@]@[<hov>%a@]@]%s"
                  s1 s_rec p_ids () (print_term' syntax p typ) t1 (print_term' syntax p typ) t2 s2
          end
    | BinOp(Mult, t1, t2) when
        syntax = CVC3 &&
        (match t1.desc with Int(_) -> false | _ -> (match t2.desc with Int(_) -> false | _ -> true)) ->
        let t = {desc=BinOp(Mult, t1, t2);typ=TInt[]} in
          Format.printf "Nonlinear expression not supported: %a@." (print_term' ML 0 true) t;
          assert false
    | BinOp(op, t1, t2) ->
        if op = Eq && syntax = CVC3 && t1.typ = TBool
        then
          let p = 5 in
          let s1,s2 = paren pri p in
            fprintf fm "%s%a <=> %a%s" s1 (print_term' syntax p typ) t1 (print_term' syntax p typ) t2 s2
        else
          let p = match op with Add|Sub|Mult -> 6 | And -> 4 | Or -> 3 | _ -> 5 in
          let s1,s2 = paren pri p in
            fprintf fm "%s%a %a %a%s" s1 (print_term' syntax p typ) t1 (print_binop syntax t1.typ) op (print_term' syntax p typ) t2 s2
    | Not t ->
        let p = 6 in
        let s1,s2 = paren pri p in
        let nt = match syntax with CVC3 -> "NOT" | _ -> "not" in
          fprintf fm "%s%s %a%s" s1 nt (print_term' syntax p typ) t s2
    | Fail -> fprintf fm "fail"
    | Label(true, t) ->
        let p = 8 in
        let s1,s2 = paren pri p in
        let s =
          match syntax with
              TRecS -> "then"
            | _ -> "l_then"
        in
          fprintf fm "%s%s %a%s" s1 s (print_term' syntax p typ) t s2
    | Label(false, t) ->
        let p = 8 in
        let s1,s2 = paren pri p in
        let s =
          match syntax with
              TRecS -> "else"
            | _ -> "l_else"
        in
          fprintf fm "%s%s %a%s" s1 s (print_term' syntax p typ) t s2
    | LabelInt(n, t) ->
        let p = 8 in
        let s1,s2 = paren pri p in
          fprintf fm "%sbr%d %a%s" s1 n (print_term' syntax p typ) t s2
    | Event s ->
        begin
          match syntax with
              TRecS -> fprintf fm "%s" s
            | _ -> fprintf fm "{%s}" s
        end
    | Record(b,fields) ->
        let rec aux fm = function
            [] -> ()
          | (s,(f,t))::fields ->
              match b, fields=[] with
                  true,true -> fprintf fm "%a" (print_term' syntax 0 typ) t
                | true,false -> fprintf fm "%a, %a" (print_term' syntax 0 typ) t aux fields
                | false,true -> fprintf fm "%s=%a" s (print_term' syntax 0 typ) t
                | false,false -> fprintf fm "%s=%a; %a" s (print_term' syntax 0 typ) t aux fields
        in
          if b
          then fprintf fm "(%a)" aux fields
          else fprintf fm "{%a}" aux fields
    | Proj(_,_,s,_,t) -> fprintf fm "%a.%s" (print_term' syntax 9 typ) t s
    | SetField(_,_,s,_,t1,t2) -> fprintf fm "%a.%s <- %a" (print_term' syntax 9 typ) t1 s (print_term' syntax 3 typ) t2
    | Nil ->
        let s =
          match syntax with
              CVC3 -> "nil"
            | _ -> "[]"
        in
          fprintf fm "%s" s
    | Cons(t1,t2) ->
        let p = 7 in
        let s1,s2 = paren pri (p+1) in
          begin
            match syntax with
                CVC3 -> fprintf fm "%scons(%a)%s" s1 (print_term' syntax p typ) t2 s2
              | _ -> fprintf fm "%s%a::%a%s" s1 (print_term' syntax p typ) t1 (print_term' syntax p typ) t2 s2
          end
    | Constr(s,ts) ->
        let p = 8 in
        let s1,s2 = paren pri p in
        let aux fm = function
            [] -> ()
          | [t] -> fprintf fm "(%a)" (print_term' syntax 1 typ) t
          | t::ts ->
              fprintf fm "(%a" (print_term' syntax 1 typ) t;
              List.iter (fun t -> fprintf fm ",%a" (print_term' syntax 1 typ) t) ts;
              pp_print_string fm ")"
        in
          fprintf fm "%s%s%a%s" s1 s aux ts s2
    | Match(t1,t2,x,y,t3) ->
        let p = 1 in
        let s1,s2 = paren pri (p+1) in
          (*
            fprintf fm "%s@[<v>@[<hov 2>let%s %a= @,%a @]@;@[<v 2>in@;@]@[<hov>%a@]@]%s" s1 s_rec p_ids () (print_term' syntax p typ) t1 (print_term' syntax p typ) t2 s2
          *)
          fprintf fm "%s@[match %a with@;[] -> %a@;| %a::%a -> %a@]%s"
            s1
            (print_term' syntax p typ) t1
            (print_term' syntax p typ) t2
            print_id x
            print_id y
            (print_term' syntax p typ) t3
            s2
    | Match_(t,pats) ->
        let p = 1 in
        let s1,s2 = paren pri (p+1) in
        let aux = function
            (pat,None,t) -> fprintf fm "%a -> %a@;" print_pattern pat (print_term' syntax p typ) t
          | (pat,Some cond,t) -> fprintf fm "%a when %a -> %a@;"
              print_pattern pat (print_term' syntax p typ) cond (print_term' syntax p typ) t
        in
          fprintf fm "%smatch %a with@;" s1 (print_term' syntax p typ) t;
          List.iter aux pats;
          pp_print_string fm s2
    | TryWith(t,pats) ->
        let p = 1 in
        let s1,s2 = paren pri (p+1) in
        let aux = function
            (pat,None,t) -> fprintf fm "%a -> %a@;" print_pattern pat (print_term' syntax p typ) t
          | (pat,Some cond,t) -> fprintf fm "%a when %a -> %a@;"
              print_pattern pat (print_term' syntax p typ) cond (print_term' syntax p typ) t
        in
          fprintf fm "%stry %a with@;" s1 (print_term' syntax p typ) t;
          List.iter aux pats;
          pp_print_string fm s2
);fprintf fm ":%a)" print_typ t.typ
and print_termlist' syntax pri typ fm = List.iter (fun bd -> fprintf fm "@;%a" (print_term' syntax pri typ) bd)


let rec trans_let t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | NInt y -> NInt y
      | RandInt None -> RandInt None
      | RandInt (Some t) -> RandInt (Some (trans_let t))
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
      | Let(Flag.Nonrecursive, f, [], t1, t2) ->
          App({desc=Fun(f,trans_let t2);typ=TFun(f,t2.typ)}, [trans_let t1])
      | Let(Flag.Nonrecursive, f, xs, t1, t2) ->
          Let(Flag.Nonrecursive, f, xs, trans_let t1, trans_let t2)
      | Let(Flag.Recursive, f, xs, t1, t2) ->
          let t1' = trans_let t1 in
          let t2' = trans_let t2 in
            Let(Flag.Recursive, f, xs, t1', t2')
      | BinOp(op, t1, t2) ->
          let t1' = trans_let t1 in
          let t2' = trans_let t2 in
            BinOp(op, t1', t2')
      | Not t1 ->
          let t1' = trans_let t1 in
            Not t1'
      | Fail -> Fail
      | Label(b, t1) ->
          let t1' = trans_let t1 in
            Label(b, t1')
      | LabelInt(n, t1) ->
          let t1' = trans_let t1 in
            LabelInt(n, t1')
      | Event s -> Event s
      | Record(b,fields) ->  Record (b, List.map (fun (f,(s,t1)) -> f,(s,trans_let t1)) fields)
      | Proj(n,i,s,f,t1) -> Proj(n,i,s,f,trans_let t1)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,trans_let t1,trans_let t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(trans_let t1, trans_let t2)
      | Constr(s,ts) -> Constr(s, List.map trans_let ts)
      | Match(t1,t2,y,z,t3) -> Match(trans_let t1, trans_let t2, y, z, trans_let t3)
      | Match_(t1,pats) ->
          let aux (pat,cond,t1) = pat, cond, trans_let t1 in
            Match_(trans_let t1, List.map aux pats)
      | TryWith(t1,pats) ->
          let aux (pat,cond,t1) = pat, cond, trans_let t1 in
            TryWith(trans_let t1, List.map aux pats)
  in
    {desc=desc; typ=t.typ}
