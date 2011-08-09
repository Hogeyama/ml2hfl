
open Utilities
open Syntax
open Type


module PredSet =
  Set.Make(
    struct
      type t = Syntax.typed_term * Syntax.typed_term
      let compare = compare
    end)
module PredSetSet =
  Set.Make(
    struct
      type t = PredSet.t
      let compare = PredSet.compare
    end)



let hd = function
    [x] -> x
  | _ -> assert false



let get_preds = function
    TInt ps -> ps
  | _ -> assert false



let rec abstract_mutable t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | NInt x -> NInt x
      | RandInt None -> RandInt None
      | RandInt (Some t) -> RandInt (Some (abstract_mutable t))
      | Var x -> Var x
      | Fun(x, t) -> Fun(x, abstract_mutable t)
      | App(t, ts) -> App(abstract_mutable t, List.map abstract_mutable ts)
      | If(t1, t2, t3) -> If(abstract_mutable t1, abstract_mutable t2, abstract_mutable t3)
      | Branch(t1, t2) -> Branch(abstract_mutable t1, abstract_mutable t2)
      | Let(flag, f, xs, t1, t2) -> Let(flag, f, xs, abstract_mutable t1, abstract_mutable t2)
      | BinOp(op, t1, t2) -> BinOp(op, abstract_mutable t1, abstract_mutable t2)
      | Not t -> Not (abstract_mutable t)
      | Fail -> Fail
      | Label(b, t) -> Label(b, abstract_mutable t)
      | Event s -> Event s
      | Record fields -> Record (List.map (fun (f,(s,t)) -> f,(s,abstract_mutable t)) fields)
      | Proj(i,s,Flag.Immutable,t) -> Proj(i, s, Flag.Immutable, abstract_mutable t)
      | Proj(i,s,Flag.Mutable,t) ->
          let u = Id.new_var "u" t.typ in
            Let(Flag.Nonrecursive, u, [], abstract_mutable t, {desc=RandInt None;typ=TInt[]})
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(abstract_mutable t1, abstract_mutable t2)
      | Constr(s,ts) -> Constr(s, List.map abstract_mutable ts)
      | Match(t1,t2,x,y,t3) -> Match(abstract_mutable t1, abstract_mutable t2, x, y, abstract_mutable t3)
      | Match_(t,pats) ->
          let aux (pat,cond,t) = pat,apply_opt abstract_mutable cond, abstract_mutable t in
            Match_(abstract_mutable t, List.map aux pats)
  in
    {desc=desc; typ=t.typ}

(*
let rec get_abst_val env typ =
  match typ with
      TUnit -> unit_term
    | TBool -> {desc=BinOp(Eq, {desc=Int 0;typ=TInt[]}, {desc=RandInt None;typ=TInt[]});typ=TBool}
    | TInt _ -> {desc=RandInt None;typ=TInt[]}
    | TFun(x,typ2) -> assert false
        (*
          let typs = List.map Id.typ (get_args (Id.typ x)) in
          let ts = List.map get_abst_val typs in
          let x' = Id.new_var_id x in
          let f = Id.new_var "f" typ in
          let u = Id.new_var "u" TUnit in
          let y = Id.new_var "y" typ2 in
          let t1 = {desc=Let(Flag.Nonrecursive, u, [], app2app (make_var x') ts, make_var y);typ=typ} in
          let t2 = make_var y in
          let t = {desc=Let(Flag.Nonrecursive, y, [], get_abst_val typ2, {desc=Branch(t1, t2); typ=typ}); typ=typ} in
          {desc=Let(Flag.Nonrecursive, f, [x'], t, make_var f); typ=typ}
        *)
    | TList(typ,_) -> assert false
        (*
          let u = Id.new_var "u" TUnit in
          let f = Id.new_var "f" (TFun(u,typ)) in
          let t = If(get_abst_val TBool, {desc=Nil;typ=TList, Cons(get_abst_val typ, App(make_var f, [Unit]))) in
          Let(Recursive, f, [u], t, App(make_var f, [Unit]))
        *)
    | TRecord(b,typs) ->
        let u = Id.new_var "u"  TUnit in
        let f = Id.new_var "f"  (TFun(u,typ)) in
        let fields = List.map (fun (s,(f,typ)) -> s,(f,get_abst_val typ)) typs in
          Record(b,fields)
    | TVariant _ as typ ->
        let stypss = Typing.get_constrs_from_type typ in
        let aux (s,typs) = Constr(s, List.map get_abst_val typs) in
          List.fold_left (fun t styps -> If(Unknown, t, aux styps)) (aux (List.hd stypss)) (List.tl stypss)
    | TVar x -> assert false
    | TConstr(s,true) -> assert false
    | TConstr(s,false) -> RandValue(TConstr(s,false), None)
    | typ -> print_typ Format.std_formatter typ; assert false
let rec abst_ext_funs t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | Var x ->
          if is_external x
          then
            let x' = Id.new_var_id x in
              Let(Flag.Nonrecursive, x', [], get_abst_val (Id.typ x), make_var x')
          else Var x
      | NInt x -> NInt x
      | RandInt None -> RandInt None
      | RandInt (Some t) -> RandInt (Some (abst_ext_funs t))
      | RandValue(typ,None) -> RandValue(typ,None)
      | RandValue(typ,Some t) -> RandValue(typ,Some (abst_ext_funs t))
      | Fun(x,t) -> Fun(x, abst_ext_funs t)
      | App(t, ts) -> App(abst_ext_funs t, List.map abst_ext_funs ts)
      | If(t1, t2, t3) -> If(abst_ext_funs t1, abst_ext_funs t2, abst_ext_funs t3)
      | Branch(t1, t2) -> Branch(abst_ext_funs t1, abst_ext_funs t2)
      | Let(flag, f, xs, t1, t2) -> Let(flag, f, xs, abst_ext_funs t1, abst_ext_funs t2)
      | BinOp(op, t1, t2) -> BinOp(op, abst_ext_funs t1, abst_ext_funs t2)
      | Not t -> Not (abst_ext_funs t)
      | Fail -> Fail
      | Label(b, t) -> Label(b, abst_ext_funs t)
      | Event s -> Event s
      | Record(b,fields) -> Record(b, List.map (fun (f,(s,t)) -> f,(s,abst_ext_funs t)) fields)
      | Proj(n,i,s,f,t) -> Proj(n,i,s,f,abst_ext_funs t)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,abst_ext_funs t1,abst_ext_funs t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(abst_ext_funs t1, abst_ext_funs t2)
      | Constr(s,ts) -> Constr(s, List.map abst_ext_funs ts)
      | Match(t1,t2,x,y,t3) -> Match(abst_ext_funs t1, abst_ext_funs t2, x, y, abst_ext_funs t3)
      | Match_(t,pats) ->
          let aux (pat,cond,t) = pat, apply_opt abst_ext_funs cond, abst_ext_funs t in
            Match_(abst_ext_funs t, List.map aux pats)
      | TryWith(t,pats) ->
          let aux (pat,cond,t) = pat, apply_opt abst_ext_funs cond, abst_ext_funs t in
            TryWith(abst_ext_funs t, List.map aux pats)
  in
    {desc=desc; typ=t.typ}
*)







let make_tl n t =
  let x = Id.new_var "x" (TInt[]) in
  let t1 = make_fun x (make_app (make_fst t) (make_sub (make_var x) (make_int n))) in
  let t2 = make_add (make_snd t) (make_int n) in
    make_pair t1 t2

let rec abst_list_typ = function
    TUnit -> TUnit
  | TBool -> TBool
  | TAbsBool -> assert false
  | TInt ps -> TInt ps
  | TRInt _ -> assert false
  | TVar _ -> assert false
  | TFun(x,typ) -> TFun(Id.set_typ x (abst_list_typ (Id.typ x)), abst_list_typ typ)
  | TList typ -> TPair(TFun(Id.new_var "x" (TInt[]), abst_list_typ typ), TInt[])
  | TConstr(s,b) -> TConstr(s,b)
  | TUnknown -> assert false

let rec get_match_bind_cond t p =
  match p.pat_desc with
      PVar x -> [x, t], true_term
    | PConst t' -> [], make_eq t t'
    | PConstruct _ -> assert false
    | PNil -> [], make_eq (make_snd t) (make_int 0)
    | PCons _ ->
        let rec decomp = function
            {pat_desc=PCons(p1,p2)} ->
              let ps,p = decomp p2 in
                p1::ps, p
          | p -> [], p
        in
        let ps,p' = decomp p in
        let rec aux bind cond i = function
            [] -> bind, cond
          | p::ps ->
              let bind',cond' = get_match_bind_cond (make_app (make_fst t) (make_int i)) p in
                aux (bind'@@bind) (make_and cond' cond) (i+1) ps
        in
        let len = List.length ps in
        let bind, cond = get_match_bind_cond (make_tl len t) p' in
          aux bind (make_and (make_leq (make_int len) (make_snd t)) cond) 0 ps
    | PRecord _ -> assert false
    | POr _ -> assert false
          

let rec abst_list t =
  let typ' = abst_list_typ t.typ in
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | Var x -> Var x
      | NInt x -> NInt x
      | RandInt None -> RandInt None
      | RandInt (Some t) -> RandInt (Some (abst_list t))
      | RandValue(typ,None) -> RandValue(typ,None)
      | RandValue(typ,Some t) -> RandValue(typ,Some (abst_list t))
      | Fun(x,t) -> Fun(x, abst_list t)
      | App(t, ts) -> App(abst_list t, List.map abst_list ts)
      | If(t1, t2, t3) -> If(abst_list t1, abst_list t2, abst_list t3)
      | Branch(t1, t2) -> Branch(abst_list t1, abst_list t2)
      | Let(flag, f, xs, t1, t2) -> Let(flag, f, xs, abst_list t1, abst_list t2)
      | BinOp(op, t1, t2) -> BinOp(op, abst_list t1, abst_list t2)
      | Not t -> Not (abst_list t)
      | Fail -> Fail
      | Label(b, t) -> Label(b, abst_list t)
      | Event s -> Event s
      | Record _ -> assert false
      | Proj _ -> assert false
      | SetField _ -> assert false
      | Nil ->
          let typ'' = match t.typ with TList typ -> abst_list_typ typ in
            Pair(make_fun (Id.new_var "x" (TInt[])) (make_fail typ''), make_int 0)
      | Cons(t1,t2) ->
          let t1' = abst_list t1 in
          let t2' = abst_list t2 in
          let i = Id.new_var "i" (TInt[]) in
          let xs = Id.new_var "xs" typ' in
          let t11 = make_eq (make_var i) (make_int 0) in
          let t12 = t1' in
          let t13 = make_app (make_fst (make_var xs)) (make_sub (make_var i) (make_int 1)) in
          let t1'' = make_fun i (make_if t11 t12 t13) in
          let t2'' = make_add (make_snd (make_var xs)) (make_int 1) in
            (make_let xs [] t2' (make_pair t1'' t2'')).desc
      | Constr(s,ts) -> assert false
      | Match(t1,t2,x,y,t3) -> assert false
      | Match_(t1,pats) ->
          let x = Id.new_var "x" (abst_list_typ t1.typ) in
          let aux (p,cond,t) t' =
            let bind,cond' = get_match_bind_cond (make_var x) p in
            let add_bind t = List.fold_left (fun t' (x,t) -> make_let x [] t t') t bind in
            let t_cond =
              match cond with
                  None -> true_term
                | Some cond -> add_bind (abst_list cond)
            in
              make_if (make_and cond' t_cond) (add_bind (abst_list t)) t'
          in
          let t_pats = List.fold_right aux pats (make_fail t.typ) in
            (make_let x [] (abst_list t1) t_pats).desc
      | TryWith(t1,t2) -> TryWith(t1,t2)
  in
    {desc=desc; typ=typ'}
    


let rec encode_pair_typ = function
    TUnit -> TUnit
  | TBool -> TBool
  | TAbsBool -> assert false
  | TInt ps -> TInt ps
  | TRInt _ -> assert false
  | TVar _ -> assert false
  | TFun(x,typ) -> TFun(Id.set_typ x (encode_pair_typ (Id.typ x)), encode_pair_typ typ)
  | TList typ -> assert false
  | TConstr _ -> assert false
  | TUnknown -> assert false
  | TPair(typ1,typ2) ->
      let f typ =
        let fst = Id.new_var "f" (encode_pair_typ typ1) in
        let snd = Id.new_var "s" (encode_pair_typ typ2) in
        let g = Id.new_var "g" (TFun(fst, TFun(snd, typ))) in
          TFun(g, typ)
      in
        TAbs f
(*
let rec encode_pair t =
  let typ' = encode_pair_typ t.typ in
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | Var x -> Var x
      | NInt x -> NInt x
      | RandInt t -> RandInt (apply_opt encode_pair t)
      | RandValue(typ,None) -> RandValue(typ,None)
      | RandValue(typ,Some t) -> RandValue(typ,Some (encode_pair t))
      | Fun(x,t) -> Fun(x, encode_pair t)
      | App(t, ts) -> App(encode_pair t, List.map encode_pair ts)
      | If(t1, t2, t3) -> If(encode_pair t1, encode_pair t2, encode_pair t3)
      | Branch(t1, t2) -> Branch(encode_pair t1, encode_pair t2)
      | Let(flag, f, xs, t1, t2) -> Let(flag, f, xs, encode_pair t1, encode_pair t2)
      | BinOp(op, t1, t2) -> BinOp(op, encode_pair t1, encode_pair t2)
      | Not t -> Not (encode_pair t)
      | Fail -> Fail
      | Label(b, t) -> Label(b, encode_pair t)
      | Event s -> Event s
      | Record _ -> assert false
      | Proj _ -> assert false
      | SetField _ -> assert false
      | Nil -> assert false
      | Cons(t1,t2) -> assert false
      | Constr(s,ts) -> assert false
      | Match(t1,t2,x,y,t3) -> assert false
      | Match_(t1,pats) -> assert false
      | TryWith(t,pats) -> assert false
      | Pair(t1,t2) -> {desc=
      | Fs
  in
    {desc=desc; typ=typ'}
*)
let abstract_list t =
  (*encode_pair*) (abst_list t)



let rec abst_datatype_typ = function
    TUnit -> TUnit
  | TBool -> TBool
  | TAbsBool -> TAbsBool
  | TInt ps -> TInt ps
  | TRInt p -> TRInt p
  | TVar _ -> assert false
  | TFun(x,typ) ->
      let x' = Id.set_typ x (abst_datatype_typ (Id.typ x)) in
        TFun(x', abst_datatype_typ typ)
  | TList _ -> assert false
  | TPair _ -> assert false
  | TConstr(s,false) -> assert false
  | TConstr(s,true) -> assert false
  | TUnknown -> assert false
  | TAbs _ -> assert false

let rec abst_datatype_typ = function
    TUnit -> TUnit
  | TBool -> TBool
  | TAbsBool -> TAbsBool
  | TInt ps -> TInt ps
  | TRInt p -> TRInt p
  | TVar _ -> assert false
  | TFun(x,typ) ->
      let x' = Id.set_typ x (abst_datatype_typ (Id.typ x)) in
        TFun(x', abst_datatype_typ typ)
  | TList _ -> assert false
  | TPair _ -> assert false
  | TConstr(s,false) -> assert false
  | TConstr(s,true) -> assert false
  | TUnknown -> assert false
  | TAbs _ -> assert false

let record_of_term_list ts =
  let fields,_ = List.fold_left (fun (fields,i) t -> (string_of_int i, (Flag.Immutable, t))::fields, i+1) ([],0) ts in
    {desc=Record fields; typ=TConstr("",false)}

let rec abst_datatype' t =
  match t.desc with
      Constr(s,ts) ->
        let is = Id.new_var "is" (TList(TInt[])) in
        let i = Id.new_var "i" (TInt[]) in
        let is' = Id.new_var "is'" (TList(TInt[])) in
        let bind,ts' = abst_datatype' (record_of_term_list ts) in
        let pt1 = make_pnil(Id.typ is'), None, make_variant (make_int (Type_decl.constr_pos s)) in
        let pt2 = make_pcons (make_pvar i) (make_pvar is'), None, make_app ts' (make_var is') in
          bind, make_fun is (make_match (make_var is) [pt1;pt2])
    | Pair(t1,t2) ->
        let is = Id.new_var "is" (TList(TInt[])) in
        let is' = Id.new_var "is'" (TList(TInt[])) in
        let bind1,t1' = abst_datatype' t1 in
        let bind2,t2' = abst_datatype' t2 in
        let pt1 = make_pcons (make_pconst (make_int 0)) (make_pvar is'), None, make_app t1' (make_var is) in
        let pt2 = make_pcons (make_pconst (make_int 1)) (make_pvar is'), None, make_app t2' (make_var is) in
          bind1@@bind2, make_fun is (make_match (make_var is) [pt1;pt2])
    | Record fields ->
        let is = Id.new_var "is" (TList(TInt[])) in
        let is' = Id.new_var "is'" (TList(TInt[])) in
        let binds,ts = List.split (List.map (fun (_,(_,t)) -> abst_datatype' t) fields) in
        let aux (pts,i) t = (make_pcons (make_pconst (make_int i)) (make_pvar is'), None, make_app (List.nth ts i) (make_var is))::pts, i+1 in
        let pts,_ = List.fold_left aux ([],0) ts in
          List.flatten binds, make_fun is (make_match (make_var is) pts)
    | _ ->
        let t' = abst_datatype t in
          match t.typ with
              TConstr _ | TPair _ | TConstr(_,true) -> [], t'
            | _ -> 
                if is_value t'
                then [], make_variant t'
                else
                  let x = Id.new_var "x" (t'.typ) in
                    [x,t'], make_variant (make_var x)

and abst_datatype t =
  let typ' = abst_datatype_typ t.typ in
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | Var x -> Var x
      | NInt x -> NInt x
      | RandInt None -> RandInt None
      | RandInt (Some t) -> RandInt (Some (abst_datatype t))
      | RandValue(typ,None) -> RandValue(typ,None)
      | RandValue(typ,Some t) -> RandValue(typ,Some (abst_datatype t))
      | Fun(x,t) -> Fun(x, abst_datatype t)
      | App(t, ts) -> App(abst_datatype t, List.map abst_datatype ts)
      | If(t1, t2, t3) -> If(abst_datatype t1, abst_datatype t2, abst_datatype t3)
      | Branch(t1, t2) -> Branch(abst_datatype t1, abst_datatype t2)
      | Let(flag, f, xs, t1, t2) -> Let(flag, f, xs, abst_datatype t1, abst_datatype t2)
      | BinOp(op, t1, t2) -> BinOp(op, abst_datatype t1, abst_datatype t2)
      | Not t -> Not (abst_datatype t)
      | Fail -> Fail
      | Label(b, t) -> Label(b, abst_datatype t)
      | Event s -> Event s
      | Record _ -> assert false
      | Proj _ -> assert false
      | SetField _ -> assert false
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(abst_datatype t1, abst_datatype t2)
      | Constr _
      | Record _ ->
          let bind,t' = abst_datatype' t in
            
      | Match _ -> assert false
      | Match_(t1,pats) ->
          let x = Id.new_var "x" (abst_datatype_typ t1.typ) in
            assert false;
          let aux (p,cond,t) t' =
            let bind,cond' = get_match_bind_cond (make_var x) p in
            let add_bind t = List.fold_left (fun t' (x,t) -> make_let x [] t t') t bind in
            let t_cond =
              match cond with
                  None -> true_term
                | Some cond -> add_bind (abst_datatype cond)
            in
              make_if (make_and cond' t_cond) (add_bind (abst_datatype t)) t'
          in
          let t_pats = List.fold_right aux pats (make_fail t.typ) in
            (make_let x [] (abst_datatype t1) t_pats).desc
      | TryWith(t1,t2) -> TryWith(t1,t2)
  in
    {desc=desc; typ=typ'}
        

let abstract_data_type t = abst_data_type t



