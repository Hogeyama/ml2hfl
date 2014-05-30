
open Util
open Syntax
open Term_util
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
      let typs',typ = List.decomp_snoc typs in
      let r_typ = List.fold_right (fun typ1 typ2 -> TPair(Id.new_var "x" typ1,typ2)) typs' typ in
      TPair(Id.new_var "u" TUnit, TFun(Id.new_var "path" (TList TInt), r_typ))
  | TConstr(s,false) -> TConstr(s,false)
  | TPair(x,typ) -> TPair(abst_recdata_var x, abst_recdata_typ typ)
  | TPred(x,ps) -> TPred(Id.set_typ x (abst_recdata_typ (Id.typ x)), ps)

and abst_recdata_var x = Id.set_typ x (abst_recdata_typ (Id.typ x))

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
          let binds = List.mapi make_bind ppcbs in
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
          let bind = binds @ List.flatten_map (fun (_,(_,_,bind)) -> bind) ppcbs in
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
    | Const c -> Const c
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
    | Constr("Abst",[]) -> Constr("Abst",[])
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
        let pats = List.mapi make_pat xtyps in
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




let trans t =
  let t = abst_recdata t in
  let t = Trans.simplify_match t in
    typ_excep := abst_recdata_typ !typ_excep;
    Type_check.check t TUnit;
    t
