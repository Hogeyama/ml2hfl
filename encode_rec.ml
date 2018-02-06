open Util
open Syntax
open Term_util
open Type

module Debug = Debug.Make(struct let check = make_debug_check __MODULE__ end)

let abst_recdata = make_trans2 ()

let abst_recdata_leaves env typs =
  let typs' = List.map (abst_recdata.tr2_typ env) typs in
  let r_typ =
    if typs = []
    then TInt
    else make_ttuple [TInt; make_ttuple typs']
  in
  make_ttuple [TUnit; (* extra-param *)
               pureTFun(Id.new_var ~name:"path" @@ make_tlist TInt, r_typ)]

let encode_recdata_typ env s ty =
  match ty with
  | TVariant labels when List.mem s @@ get_data_type ty ->
      let tys =
        let aux ty =
          match ty with
          | TData s' ->
              if s' = s then
                None
              else
                unsupported "encode_variant: non-simple recursion"
          | _ ->
              if get_data_type ty = [] then
                Some (abst_recdata.tr2_typ env ty)
              else
                unsupported "encode_variant: non-simple recursion"
        in
        List.map (make_tpair TBool) @@ List.map (fun (_,tys) -> make_ttuple @@ List.filter_map aux tys) labels
      in
      make_tpair TUnit @@ pureTFun(Id.new_var ~name:"path" @@ make_tlist TInt, make_ttuple tys)
  | _ -> abst_recdata.tr2_typ env ty

let abst_recdata_typ env typ =
  match typ with
  | TRecord fields -> unsupported "abst_recdata_typ TRecord"
  | TVariant labels ->
      let aux (s,tys) = make_tpair TBool @@ make_ttuple @@ List.map (abst_recdata.tr2_typ env) tys in
      make_ttuple @@ List.map aux labels
  | TApp(TOption, [typ]) -> opt_typ @@ abst_recdata.tr2_typ env typ
  | _ -> abst_recdata.tr2_typ_rec env typ

let is_rec_type env ty =
  match ty with
  | TData s -> Triple.snd @@ List.assoc s env
  | _ -> false

let expand_typ env ty =
  match ty with
  | TData s -> Triple.fst @@ List.assoc s env
  | _ -> ty

let expand_enc_typ env ty =
  match ty with
  | TData s -> Triple.trd @@ List.assoc s env
  | _ -> ty

let rec abst_recdata_pat env p =
  let typ =
    match abst_recdata.tr2_typ env p.pat_typ with
    | TData s -> Triple.trd @@ List.assoc s env
    | typ -> typ
  in
  let desc,cond,bind =
    match p.pat_desc with
    | PAny -> PAny, true_term, []
    | PNondet -> PNondet, true_term, []
    | PVar x -> PVar (abst_recdata.tr2_var env x), true_term, []
    | PAlias(p,x) ->
        let p',cond,bind = abst_recdata_pat env p in
        PAlias(p', abst_recdata.tr2_var env x), cond, bind
    | PConst t -> PConst t, true_term, []
    | PConstr(c,ps) when not (is_rec_type env p.pat_typ) ->
        let p',cond,bind =
          let aux p (ps, cond, bind) =
            let p', cond', bind' = abst_recdata_pat env p in
            p'::ps, make_and cond' cond, bind'@bind
          in
          let ps,cond,bind = List.fold_right aux ps ([],true_term,[]) in
          make_ptuple ps, cond, bind
        in
        let ps' =
          let aux (c',tys) =
            if c = c' then
              Pat.(pair true_ p')
            else
              let ty = make_ttuple @@ List.map (abst_recdata.tr2_typ env) tys in
              Pat.(pair false_ (__ ty))
          in
          List.map aux @@ decomp_tvariant @@ expand_typ env p.pat_typ
        in
        PTuple ps', true_term, []
    | PConstr(c,ps) ->
        let f = Id.new_var typ in
        let pcbs = List.map (abst_recdata_pat env) ps in
        let binds =
          let make_bind (acc,i) p (p',_,_) =
            let i',t' =
              if is_rec_type env p.pat_typ then
                let path = Id.new_var ~name:"path" (make_tlist TInt) in
                i+1, Term.(pair unit (* extra-param *)
                                (fun_ path (snd (var f) @ [list [int i]])))
              else
                i, Term.(proj i (snd (snd (var f) @ [list [int i]])))
            in
            acc@[t', p'], i'
          in
          fst @@ List.fold_left2 make_bind ([],0) ps pcbs
        in
        let cond =
          let conds' =
            let make_cond (t,pt) (p,cond,_) =
              match p.pat_desc with
              | PAny
              | PVar _ -> true_term
              | _ -> make_match t [pt,true_term,true_term; make_pany p.pat_typ,true_term,false_term]
            in
            List.map2 make_cond binds pcbs
          in
          let cond0 =
            let i = List.find_pos (fun _ (c',_) -> c = c') @@ decomp_tvariant @@ expand_typ env p.pat_typ in
            Term.(fst (proj i (snd (var f) @ [nil TInt])))
          in
          make_ands (cond0 :: conds')
        in
        Debug.printf "cond: %a@." Print.term cond;
        let bind = binds @ List.flatten_map Triple.trd pcbs in
        PVar f, cond, bind
    | PNil -> PNil, true_term, []
    | PCons(p1,p2) ->
        let p1',cond1,bind1 = abst_recdata_pat env p1 in
        let p2',cond2,bind2 = abst_recdata_pat env p2 in
        PCons(p1',p2'), make_and cond1 cond2, bind1@bind2
    | PRecord fields ->
        let aux (s,p) (ps, cond, bind) =
          let p', cond', bind' = abst_recdata_pat env p in
          p'::ps, make_and cond' cond, bind'@bind
        in
        let ps,cond,bind = List.fold_right aux fields ([],true_term,[]) in
        PTuple ps, cond, bind
    | POr({pat_desc=PConst _},_)
    | POr(_,{pat_desc=PConst _}) -> p.pat_desc, true_term, []
    | POr(p1,p2) ->
        let p1',cond1,bind1 = abst_recdata_pat env p1 in
        let p2',cond2,bind2 = abst_recdata_pat env p2 in
        let rec get_bind_map p1' p2' =
          match p1'.pat_desc, p2'.pat_desc with
          | PVar x1, PVar x2 -> [x2, x1]
          | PTuple ps1, PTuple ps2 -> unsupported "POr1"
          | _ ->
              Format.printf"%a,%a@." Print.pattern p1' Print.pattern p2';
              unsupported "POr2"
        in
        let map = get_bind_map p1' p2' in
        let cond2' = List.fold_right (Fun.uncurry subst_var) map cond2 in
        p1'.pat_desc, make_or cond1 cond2', bind1
    | PTuple ps ->
        let aux p (ps,cond,bind) =
          let p',cond',bind' = abst_recdata_pat env p in
          p'::ps, make_and cond' cond, bind' @ bind
        in
        let ps',cond,bind = List.fold_right aux ps ([],true_term,[]) in
        PTuple ps', cond, bind
    | PNone ->
        let x = Id.new_var typ in
        PVar x, make_is_none @@ make_var x, []
    | PSome p ->
        let p',cond,bind = abst_recdata_pat env p in
        let x = Id.new_var typ in
        PVar x, make_is_some @@ make_var x, (make_get_val @@ make_var x, p')::bind
  in
  {pat_desc=desc; pat_typ=typ}, cond, bind


let abst_recdata_term env t =
  match t.desc with
  | Constr("Abst",[]) -> {desc=Constr("Abst",[]); typ=abst_recdata.tr2_typ env t.typ; attr=[]}
  | Constr(c,ts) when not (is_rec_type env t.typ) ->
      let aux (c',tys) =
        if c = c' then
          let t' = make_tuple @@ List.map (abst_recdata.tr2_term env) ts in
          Term.(pair true_ t')
        else
          let ty = make_ttuple @@ List.map (abst_recdata.tr2_typ env) tys in
          Term.(pair false_ (rand ty))
      in
      make_tuple @@ List.map aux @@ decomp_tvariant @@ expand_typ env t.typ
  | Constr(c,ts) ->
      let ts' = List.map (abst_recdata.tr2_term env) ts in
      let xtys = List.map2 (fun t' t -> Id.new_var @@ expand_enc_typ env t'.typ, t.typ) ts' ts in
      let labels = decomp_tvariant @@ expand_typ env t.typ in
      let path = Id.new_var ~name:"path" @@ make_tlist TInt in
      let pat0 =
        let top =
          let make_return ts' =
            let aux (c',tys) =
              if c = c' then
                Term.(pair true_ (tuple ts'))
              else
                let tys' = List.filter_out ((=) t.typ) tys in
                let ty = make_ttuple @@ List.map (abst_recdata.tr2_typ env) tys' in
                Term.(pair false_ (rand ty))
            in
            make_tuple @@ List.map aux labels
          in
          List.combine ts xtys
          |> List.filter_out (fun (t',_) -> t.typ = t'.typ)
          |> List.map (snd |- fst |- make_var)
          |> make_return
        in
        make_pnil TInt, true_term, top
      in
      let make_pat (i,acc) (x,ty) =
        if ty = t.typ then
          let path' = Id.new_var ~name:"path'" @@ make_tlist TInt in
          let t = Term.(snd (var x) @ [var path']) in
          i+1, acc @ [Pat.(cons (int i) (var path')), true_term, t]
        else
          i, acc
      in
      let _,pats = List.fold_left make_pat (0,[]) xtys in
      let defs = List.map2 (fun (x,_) t -> x, t) xtys ts' in
      Term.(lets defs (pair unit (* for adding predicates *)
                            (fun_ path (match_ (var path) (pat0::pats)))))
  | Match(t1,pats) ->
      let aux (p,c,t) =
        let p',c',bind = abst_recdata_pat env p in
        let t' = abst_recdata.tr2_term env t in
        let aux (t,p) t' =
          make_match t [p, true_term, t']
        in
        p', make_and c c', List.fold_right aux bind t'
      in
      let t1' = abst_recdata.tr2_term env t1 in
      let pats' = List.map aux pats in
      make_match t1' pats'
  | TNone -> make_none @@ abst_recdata.tr2_typ env @@ option_typ t.typ
  | TSome t -> make_some @@ abst_recdata.tr2_term env t
  | Record [] -> assert false
  | Record fields ->
      if List.exists (fun (_,(f,_)) -> f = Mutable) @@ decomp_trecord t.typ
      then unsupported "Mutable records"
      else make_tuple @@ List.map (abst_recdata.tr2_term env -| snd) fields
  | Field(t,s) ->
      let fields = decomp_trecord t.typ in
      if Mutable = fst @@ List.assoc s fields then
        unsupported "Mutable records"
      else
        make_proj (List.find_pos (fun _ (s',_) -> s = s') fields) @@ abst_recdata.tr2_term env t
  | SetField _ -> assert false
  | Local(Decl_type [s,ty], t) ->
      let ty' = encode_recdata_typ env s ty in
      let env' = (s, (ty, List.mem s @@ get_data_type ty, ty')) :: env in
      subst_data_type_term s ty' @@ abst_recdata.tr2_term_rec env' t
  | Local(Decl_type decls, t) ->
      unsupported "encode_rec: Decl_type"
  | _ -> abst_recdata.tr2_term_rec env t

let () = abst_recdata.tr2_term <- abst_recdata_term
let () = abst_recdata.tr2_typ <- abst_recdata_typ


let typ_in_env ty tys =
  match ty with
  | TData s -> List.mem s tys
  | _ -> false

let pr s t = Debug.printf "##[encode_rec] %a:@.%a@.@." Color.s_red s Print.term_typ t

let trans_typ = abst_recdata.tr2_typ []
let trans t =
  let typ = trans_typ t.typ in
  t
  |@> pr "input"
  |*> Trans.remove_top_por
  |*@> pr "remove_top_por"
  |*@> Type_check.check -$- t.typ
  |> Trans.abst_ext_recdata
  |@> Type_check.check -$- typ
  |@> pr "abst_ext_rec"
  |> abst_recdata.tr2_term []
  |@> pr "abst_rec"
  |@> Type_check.check -$- typ
  |> Trans.simplify_match
  |@> Type_check.check -$- typ
