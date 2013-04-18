
open Util
open Syntax
open Type
open Type_decl


module RT = Ref_type


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



let rec abst_recdata_typ = function
    TUnit -> TUnit
  | TBool -> TBool
  | TAbsBool -> assert false
  | TInt -> TInt
  | TRInt _ -> assert false
  | TVar{contents=None} -> assert false
  | TVar{contents=Some typ} -> abst_recdata_typ typ
  | TFun(x,typ) -> TFun(Id.set_typ x (abst_recdata_typ (Id.typ x)), abst_recdata_typ typ)
  | TList typ -> TList (abst_recdata_typ typ)
  | TConstr(s,true) ->
      let typs = TInt :: get_ground_types s in
      let r_typ = List.fold_right (fun typ1 typ2 -> TPair(typ1,typ2)) (init typs) (last typs) in
        TPair(TUnit, TFun(Id.new_var "path" (TList TInt), r_typ))
  | TConstr(s,false) -> TInt
  | TPair(typ1,typ2) -> TPair(abst_recdata_typ typ1, abst_recdata_typ typ2)
  | TPred(typ,ps) -> TPred(abst_recdata_typ typ, ps)

let abst_recdata_var x = Id.set_typ x (abst_recdata_typ (Id.typ x))

let abst_label c = make_int (1 + Type_decl.constr_pos c)

let rec abst_recdata_pat p =
  let typ = abst_recdata_typ p.pat_typ in
  let desc,cond,bind =
    match p.pat_desc with
        PAny -> PAny, true_term, []
      | PVar x -> PVar (abst_recdata_var x), true_term, []
      | PAlias(p,x) ->
          let p',cond,bind = abst_recdata_pat p in
            PAlias(p', abst_recdata_var x), cond, bind
      | PConst t -> PConst t, true_term, []
      | PConstruct(c,ps) ->
          let f = Id.new_var "f" (abst_recdata_typ p.pat_typ) in
          let ppcbs = List.map (fun p -> p, abst_recdata_pat p) ps in
          let typ_name = match p.pat_typ with TConstr(s,true) -> s | _ -> assert false in
          let ground_types = Type_decl.get_ground_types typ_name in
          let make_bind i (p,(p',_,_)) =
            let t =
              if List.mem p.pat_typ ground_types
              then
                let rec find c = function
                    [] -> assert false
                  | typ::typs -> if typ = p.pat_typ then c else find (c+1) typs
                in
                let j = find 0 ground_types in
                  make_nth (1 + j) (1 + List.length ground_types)
                    (make_app (make_snd (make_var f)) [make_cons (make_int i) (make_nil TInt)])
              else
                let path = Id.new_var "path" (TList TInt) in
                  make_pair unit_term
                    (make_fun path (make_app (make_snd (make_var f)) [make_cons (make_int i) (make_var path)]))
            in
              t, p'
          in
          let binds = mapi make_bind ppcbs in
          let make_cond (t,pt) (_,(p,cond,_)) =
            match p.pat_desc with
                PAny
              | PVar _ -> true_term
              | _ -> make_match t [pt, true_term, true_term; make_pany p.pat_typ, true_term, false_term]
          in
          let conds' = List.map2 make_cond binds ppcbs in
          let cond0 = make_eq (make_nth 0 (1 + List.length ground_types)
                                 (make_app (make_snd (make_var f)) [make_nil TInt])) (abst_label c) in
          let cond = List.fold_left make_and true_term (cond0 :: conds') in
          let bind = binds @ flatten_map (fun (_,(_,_,bind)) -> bind) ppcbs in
            PVar f, cond, bind
      | PNil -> PNil, true_term, []
      | PCons(p1,p2) ->
          let p1',cond1,bind1 = abst_recdata_pat p1 in
          let p2',cond2,bind2 = abst_recdata_pat p2 in
            PCons(p1',p2'), make_and cond1 cond2, bind1@bind2
      | PRecord _ -> assert false
      | POr _ -> assert false
      | PPair(p1,p2) ->
          let p1',cond1,bind1 = abst_recdata_pat p1 in
          let p2',cond2,bind2 = abst_recdata_pat p2 in
            PPair(p1',p2'), make_and cond1 cond2, bind1@bind2
  in
    {pat_desc=desc; pat_typ=typ}, cond, bind

let rec abst_recdata t =
  let typ' = abst_recdata_typ t.typ in
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | Var x -> Var (abst_recdata_var x)
      | RandInt b -> RandInt b
      | RandValue(typ,b) -> RandValue(typ,b)
      | Fun(x,t) -> Fun(abst_recdata_var x, abst_recdata t)
      | App(t, ts) -> App(abst_recdata t, List.map abst_recdata ts)
      | If(t1, t2, t3) -> If(abst_recdata t1, abst_recdata t2, abst_recdata t3)
      | Branch(t1, t2) -> Branch(abst_recdata t1, abst_recdata t2)
      | Let(flag, bindings, t) ->
          let aux (f,xs,t) = abst_recdata_var f, List.map abst_recdata_var xs, abst_recdata t in
          let bindings' = List.map aux bindings in
            Let(flag, bindings', abst_recdata t)
      | BinOp(op, t1, t2) -> BinOp(op, abst_recdata t1, abst_recdata t2)
      | Not t -> Not (abst_recdata t)
      | Event(s,b) -> Event(s,b)
      | Record _ -> assert false
      | Proj _ -> assert false
      | SetField _ -> assert false
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(abst_recdata t1, abst_recdata t2)
      | Constr("Abst",[]) -> (make_app randint_term [unit_term]).desc
      | Constr(c,ts) ->
          let ts' = List.map abst_recdata ts in
          let typ_name = match t.typ with TConstr(s,true) -> s | _ -> assert false in
          let ground_types = get_ground_types typ_name in
          let make_return label typ t =
            let aux typ' = if typ = typ' then t else get_typ_default (abst_recdata_typ typ') in
            let head =
              match label with
                  None -> make_int 0
                | Some t -> t
            in
              make_tuple (head :: List.map aux ground_types)
          in
          let path = Id.new_var "path" (TList TInt) in
          let path' = Id.new_var "path'" (TList TInt) in
          let pat0 = make_pnil TInt, true_term, make_return (Some (abst_label c)) TUnit unit_term in
          let xtyps = List.map (fun t -> Id.new_var "x" (abst_recdata_typ t.typ), t.typ) ts in
          let make_pat i (x,typ) =
            let t =
              if List.mem typ ground_types
              then make_return None typ (make_var x)
              else make_app (make_snd (make_var x)) [make_var path']
            in
              make_pcons (make_pconst (make_int i)) (make_pvar path'), true_term, t
          in
          let pats = mapi make_pat xtyps in
          let defs = List.map2 (fun (x,_) t -> x, [], t) xtyps ts' in
            (make_lets defs (make_pair unit_term (make_fun path (make_match (make_var path) (pat0::pats))))).desc
      | Match(t1,pats) ->
          let aux (p,c,t) =
            let p',c',bind = abst_recdata_pat p in
            let t' = abst_recdata t in
            let aux (t,p) t' =
              make_match t [p, true_term, t'; make_pany p.pat_typ, true_term, make_loop t'.typ]
            in
              p', make_and c c', List.fold_right aux bind t'
          in
          let t1' = abst_recdata t1 in
          let pats' = List.map aux pats in
            Match(t1', pats')
      | Raise t -> Raise (abst_recdata t)
      | TryWith(t1,t2) -> TryWith(abst_recdata t1, abst_recdata t2)
      | Bottom -> Bottom
      | Pair(t1,t2) -> Pair(abst_recdata t1, abst_recdata t2)
      | Fst t -> Fst (abst_recdata t)
      | Snd t -> Snd (abst_recdata t)
      | Label(info,t) -> Label(info, abst_recdata t)
  in
    {desc=desc; typ=typ'}




let abstract_recdata t =
  let t = abst_recdata t in
  let t = Trans.simplify_match t in
    typ_excep := abst_recdata_typ !typ_excep;
    Type_check.check t TUnit;
    t











let rec abstract_mutable t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | RandInt b -> RandInt b
      | Var x -> Var x
      | Fun(x, t) -> Fun(x, abstract_mutable t)
      | App(t, ts) -> App(abstract_mutable t, List.map abstract_mutable ts)
      | If(t1, t2, t3) -> If(abstract_mutable t1, abstract_mutable t2, abstract_mutable t3)
      | Branch(t1, t2) -> Branch(abstract_mutable t1, abstract_mutable t2)
      | Let(flag, bindings, t2) ->
          let bindings' = List.map (fun (f,xs,t) -> f, xs, abstract_mutable t) bindings in
            Let(flag, bindings', abstract_mutable t2)
      | BinOp(op, t1, t2) -> BinOp(op, abstract_mutable t1, abstract_mutable t2)
      | Not t -> Not (abstract_mutable t)
      | Event(s,b) -> Event(s,b)
      | Record fields -> Record (List.map (fun (f,(s,t)) -> f,(s,abstract_mutable t)) fields)
      | Proj(i,s,Immutable,t) -> Proj(i, s, Immutable, abstract_mutable t)
      | Proj(i,s,Mutable,t) ->
          let u = Id.new_var "u" t.typ in
            Let(Nonrecursive, [u, [], abstract_mutable t], randint_term)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(abstract_mutable t1, abstract_mutable t2)
      | Constr(s,ts) -> Constr(s, List.map abstract_mutable ts)
      | Match(t,pats) ->
          let aux (pat,cond,t) = pat, abstract_mutable cond, abstract_mutable t in
            Match(abstract_mutable t, List.map aux pats)
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




let rec get_rtyp_list rtyp typ =
  match rtyp, typ with
      RT.Inter rtyps, _ ->
        RT.Inter (List.map (fun rtyp1 -> get_rtyp_list rtyp1 typ) rtyps)
    | RT.Union rtyps, _ ->
        RT.Union (List.map (fun rtyp1 -> get_rtyp_list rtyp1 typ) rtyps)
    | RT.Pair(x, RT.Base(RT.Int, x', p_len), RT.Fun(y, RT.Base(RT.Int, y', p_i), typ2)), TList typ ->
        let p_len' = subst x' (make_var x) p_len in
        let p_i' = subst y' (make_var y) p_i in
        RT.List(x, p_len', y, p_i', get_rtyp_list typ2 typ)
    | RT.Pair(x, RT.Base(RT.Int, x', p_len), RT.Inter []), TList typ ->
        let p_len' = subst x' (make_var x) p_len in
        RT.List(x, p_len', Id.new_var "" typ_unknown, true_term, RT.Inter [])
    | RT.Pair(x, RT.Base(RT.Int, x', p_len), RT.Inter typs), TList typ ->
        let typs' = List.map (fun typ -> RT.Pair(x, RT.Base(RT.Int, x', p_len), typ)) typs in
          get_rtyp_list (RT.Inter typs') (TList typ)
    | _, TList typ ->
        Format.printf "%a@." RT.print rtyp;
        raise (Fatal "not implemented get_rtyp_list")
    | RT.Base(b,x,ps), _ -> RT.Base(b,x,ps)
    | RT.Fun(x,rtyp1,rtyp2), TFun(y,typ2) ->
        let rtyp1' = get_rtyp_list rtyp1 (Id.typ y) in
        let rtyp2' = get_rtyp_list rtyp2 typ2 in
          RT.Fun(x, rtyp1', rtyp2')
    | RT.Pair(x,rtyp1,rtyp2), TPair(typ1,typ2) ->
        let rtyp1' = get_rtyp_list rtyp1 typ1 in
        let rtyp2' = get_rtyp_list rtyp2 typ2 in
          RT.Pair(x, rtyp1', rtyp2')
    | RT.ExtArg(x,rtyp1,rtyp2), _ ->
        RT.ExtArg(x, rtyp1, get_rtyp_list rtyp2 typ)
    | _ ->
        Format.printf "rtyp:%a@.typ:%a@." RT.print rtyp pp_print_typ typ;
        assert false

let get_rtyp_list_of typed f rtyp =
  let typ = Trans.assoc_typ f typed in
    get_rtyp_list rtyp typ


let make_tl n t =
  let x = Id.new_var "x" TInt in
  let t1 = make_sub (make_fst t) (make_int n) in
  let t2 = make_fun x (make_app (make_snd t) [make_add (make_var x) (make_int n)]) in
    make_pair t1 t2



let rec abst_list_typ = function
    TUnit -> TUnit
  | TBool -> TBool
  | TAbsBool -> assert false
  | TInt -> TInt
  | TRInt _ -> assert false
  | TVar{contents=None} -> raise (Fatal "Polymorphic types occur! (Abstract.abst_list_typ)")
  | TVar{contents=Some typ} -> abst_list_typ typ
  | TFun(x,typ) -> TFun(Id.set_typ x (abst_list_typ (Id.typ x)), abst_list_typ typ)
  | TList typ -> TPair(TInt, TFun(Id.new_var "x" TInt, abst_list_typ typ))
  | TConstr(s,b) -> TConstr(s,b)
  | TPair(typ1,typ2) -> TPair(abst_list_typ typ1, abst_list_typ typ2)
  | TPred(typ,ps) ->
      let ps' = List.map (abst_list "") ps in
        TPred(abst_list_typ typ, ps')

and abst_list_var x = Id.set_typ x (abst_list_typ (Id.typ x))

and get_match_bind_cond t p =
  match p.pat_desc with
      PAny -> [], true_term
    | PVar x -> [abst_list_var x, t], true_term
    | PAlias(p,x) ->
        let bind,cond = get_match_bind_cond t p in
        (abst_list_var x, t)::bind, cond
    | PConst {desc=Unit} -> [], true_term
    | PConst t' -> [], make_eq t t'
    | PConstruct _ -> assert false
    | PNil -> [], make_eq (make_fst t) (make_int 0)
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
              let bind',cond' = get_match_bind_cond (make_app (make_snd t) [make_int i]) p in
                aux (bind'@@bind) (make_and cond cond') (i+1) ps
        in
        let len = List.length ps in
        let bind, cond = get_match_bind_cond (make_tl len t) p' in
          aux bind (make_and (make_leq (make_int len) (make_fst t)) cond) 0 ps
    | PRecord _ -> assert false
    | POr _ -> assert false
    | PPair(p1,p2) ->
        let bind1,cond1 = get_match_bind_cond (make_fst t) p1 in
        let bind2,cond2 = get_match_bind_cond (make_snd t) p2 in
          bind1@@bind2, make_and cond1 cond2

and make_cons post t1 t2 =
  let i = Id.new_var "i" TInt in
  let x = Id.new_var "x" t1.typ in
  let xs = Id.new_var "xs" t2.typ in
  let t11 = make_eq (make_var i) (make_int 0) in
  let t12 = make_var x in
  let t13 = make_app (make_snd (make_var xs)) [make_sub (make_var i) (make_int 1)] in
  let t_f = make_fun i (make_if t11 t12 t13) in
  let t_len = make_add (make_fst (make_var xs)) (make_int 1) in
  let cons = Id.new_var ("cons"^post) (TFun(x,TFun(xs,t2.typ))) in
    make_let [cons, [x;xs], make_pair t_len t_f] (make_app (make_var cons) [t1; t2])


and abst_list post t =
  let typ' = abst_list_typ t.typ in
    match t.desc with
        Unit -> unit_term
      | True -> true_term
      | False -> false_term
      | Unknown -> assert false
      | Int n -> make_int n
      | Var x -> make_var (abst_list_var x)
      | RandInt b -> randint_term
      | RandValue(typ,b) -> raise (Fatal "Not implemented (Abstract.abst_list)")
      | Fun(x,t) -> make_fun (abst_list_var x) (abst_list post t)
      | App(t, ts) -> make_app (abst_list post t) (List.map (abst_list post) ts)
      | If(t1, t2, t3) -> make_if (abst_list post t1) (abst_list post t2) (abst_list post t3)
      | Branch(t1, t2) -> make_branch (abst_list post t1) (abst_list post t2)
      | Let(flag, bindings, t2) ->
          let aux (f,xs,t) =
            let post' = "_" ^ Id.name f in
              abst_list_var f, List.map abst_list_var xs, abst_list post' t
          in
          let bindings' = List.map aux bindings in
            make_let_f flag bindings' (abst_list post t2)
      | BinOp(op, t1, t2) -> {desc=BinOp(op, abst_list post t1, abst_list post t2); typ=typ'}
      | Not t -> make_not (abst_list post t)
      | Event(s,b) -> {desc=Event(s,b); typ=typ'}
      | Record _ -> assert false
      | Proj _ -> assert false
      | SetField _ -> assert false
      | Nil ->
          let typ'' = match t.typ with TList typ -> abst_list_typ typ | _ -> assert false in
            make_pair (make_int 0) (make_fun (Id.new_var "x" TInt) (make_bottom typ''))
      | Cons(t1,t2) ->
          let t1' = abst_list post t1 in
          let t2' = abst_list post t2 in
            make_cons post t1' t2'
      | Constr(s,ts) -> assert false
      | Match(t1,pats) ->
          let x,bindx =
            match t1.desc with
                Var x -> Id.set_typ x (abst_list_typ t1.typ), fun t -> t
              | _ ->
                  let x = Id.new_var "xs" (abst_list_typ t1.typ) in
                    x, fun t -> make_let [x, [], abst_list post t1] t
          in
          let aux (p,cond,t) t' =
            let bind,cond' = get_match_bind_cond (make_var x) p in
            let add_bind t = List.fold_left (fun t' (x,t) -> make_let [x, [], t] t') t bind in
            let t_cond =
              if cond = true_term
              then cond
              else add_bind (abst_list post cond)
            in
              make_if (make_and cond' t_cond) (add_bind (abst_list post t)) t'
          in
          let t_pats = List.fold_right aux pats (make_bottom typ') in
            bindx t_pats
      | Raise t -> {desc=Raise (abst_list post t); typ=typ'}
      | TryWith(t1,t2) -> {desc=TryWith(abst_list post t1, abst_list post t2); typ=typ'}
      | Bottom -> {desc=Bottom; typ=typ'}
      | Pair(t1,t2) -> make_pair(abst_list post t1) (abst_list post t2)
      | Fst t -> make_fst (abst_list post t)
      | Snd t -> make_snd (abst_list post t)
      | Label(info,t) -> make_label info t

let abstract_list t =
  let t' = abst_list "" t in
  let () = if false then Format.printf "abst_list::@. @[%a@.@." Syntax.pp_print_term t' in
  let () = typ_excep := abst_list_typ !typ_excep in
  let () = Type_check.check t' Type.TUnit in
    t', get_rtyp_list_of t




let rec abst_datatype_typ = function
    TUnit -> TUnit
  | TBool -> TBool
  | TAbsBool -> TAbsBool
  | TInt -> TInt
  | TRInt p -> TRInt p
  | TVar _ -> assert false
  | TFun(x,typ) ->
      let x' = Id.set_typ x (abst_datatype_typ (Id.typ x)) in
        TFun(x', abst_datatype_typ typ)
  | TList _ -> assert false
  | TPair _ -> assert false
  | TConstr(s,false) -> assert false
  | TConstr(s,true) -> assert false
  | TPred(typ,ps) -> TPred(abst_datatype_typ typ, ps)

let record_of_term_list ts =
  let fields,_ = List.fold_left (fun (fields,i) t -> (string_of_int i, (Immutable, t))::fields, i+1) ([],0) ts in
    {desc=Record fields; typ=TConstr("",false)}
