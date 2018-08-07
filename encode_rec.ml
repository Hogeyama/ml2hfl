open Util
open Syntax
open Term_util
open Type

module Debug = Debug.Make(struct let check = Flag.Debug.make_check __MODULE__ end)

type env = (string * (typ * bool * typ)) list
  (* string -- 's' of 'TData s'
     typ    -- type before encoding
     bool   -- whether recursive or not
     typ    -- type after encoding
   *)
let abst_recdata : env trans2 = make_trans2 ()

(* Example:
     type tree = Leaf of int | Node of tree * tree
   i.e.,
     s = "tree",
     ty = TVariant(false, [ ("Leaf", int)
                          ; ("Node", TData "tree" * TData "tree")])
   is encoded into
     unit * (path:int list -> TTuple [ TTuple [ TTuple [bool; TTuple [int]]
                                              ; TTuple [bool; TTuple [] ] ;
                                       unit (* <- for predicate *)
                                     ])
  *)
let encode_recdata_typ env s ty =
  match ty with
  | TVariant(false,labels) when List.mem s @@ get_tdata ty ->
      let ty: typ =
        let aux ty =
          match ty with
          | TData s' when s' = s ->
              None
          | _ when get_tdata ty = [] ->
              Some (abst_recdata.tr2_typ env ty)
          | _ ->
              unsupported "encode_variant: non-simple recursion"
        in
        Ty.(pair
          (tuple (List.map (pair bool -| tuple -| List.filter_map aux -| snd) labels))
          unit) (* for predicate *)
      in
      Ty.(pair unit
               (pureTFun(Id.new_var ~name:"path" @@ list Ty.int, ty)))
  | _ -> abst_recdata.tr2_typ env ty

(* e.g.
     type foo = Foo of int | Bar of bool * string
       => (bool * (int)) * (bool * (bool * string)) * unit
                  ^^^^^ one element tuple             ^^^^ for predicate
*)
let abst_recdata_typ env typ =
  match typ with
  | TRecord fields -> unsupported "abst_recdata_typ TRecord"
  | TVariant(false,labels) ->
      let aux (s,tys) =
        Ty.(pair bool (tuple @@ List.map (abst_recdata.tr2_typ env) tys))
      in
        Ty.(pair (make_ttuple @@ List.map aux labels) unit)
  | TApp(TOption, [typ]) -> opt_typ @@ abst_recdata.tr2_typ env typ
  | _ -> abst_recdata.tr2_typ_rec env typ

let is_rec_type (env: env) ty =
  match ty with
  | TData s when not @@ List.mem s prim_base_types ->
      begin
        try Triple.snd @@ List.assoc s env
        with Not_found -> assert false
      end
  | _ -> false

let expand_typ (env: env) ty =
  match ty with
  | TData s when not @@ List.mem s prim_base_types ->
      begin
        try Triple.fst @@ List.assoc s env
        with Not_found -> assert false
      end
  | _ -> ty

let expand_enc_typ (env: env) ty =
  match ty with
  | TData s when not @@ List.mem s prim_base_types ->
      begin
        try Triple.trd @@ List.assoc s env
        with Not_found -> assert false
      end
  | _ -> ty

let rec abst_recdata_pat (env: env) p =
  let typ =
    match abst_recdata.tr2_typ env p.pat_typ with
    | TData s when not @@ List.mem s prim_base_types ->
        begin
          try Triple.trd @@ List.assoc s env
          with Not_found ->
            Format.printf "%s@." s;
            assert false
        end
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
          let poly,decls = decomp_tvariant @@ expand_typ env p.pat_typ in
          if poly then unsupported "encode_rec: polymorphic variant";
          List.map aux decls
        in
        PTuple [Pat.tuple ps'; Pat.(__ Ty.unit)], true_term, []
    | PConstr(c,ps) ->
        let f = Id.new_var typ in
        (* [c] is [constr_ix]th constructor of type TData s *)
        let constr_ix =
          let poly,decls = decomp_tvariant @@ expand_typ env p.pat_typ in
          if poly then unsupported "encode_rec: polymorphic variant";
          List.find_pos (fun _ (c',_) -> c = c') decls
        in
        Debug.printf "f: %a@." Print.id_typ f;
        let pcbs = List.map (abst_recdata_pat env) ps in

        Debug.printf "HERE1@.";
        let binds =
          (* i: index of non-recursive types
             j: index of recursive types
             e.g.,
                type tree = Node of tree * int * tree * int
                                    _      0     _       1   <- i
                                    0      _     1       _   <- j
                match t: tree with
                | Node(p1,p2,p3,p4) as p -> ...
              =>
                p1 path = (), snd p (0::path)
                p2      = proj 0 @@ snd @@ proj constr_ix @@ fst @@ snd p []
                p3 path = (), snd p (1::path)
                p4      = proj 1 @@ snd @@ proj constr_ix @@ fst @@ snd p []

             TODO refactor
           *)
          let make_bind (acc,i,j) p (p',_,_) =
            let i',j',t' = match () with
              | _ when is_rec_type env p.pat_typ ->
                Debug.printf "%a has rec type %a@." Print.pattern p Print.typ p.pat_typ;
                let path = Id.new_var ~name:"path" Ty.(list int) in
                begin
                  let t = Term.(pair unit (* extra-param *)
                                     (fun_ path (snd (var f) @ [cons (int j) (var path)])))
                  in
                    Debug.printf "%a@." Print.term' t;
                    i, j+1, t
                end
              | _ ->
                Debug.printf "%a has non-rec type %a@." Print.pattern p Print.typ p.pat_typ;
                let t = Term.(proj i (snd (proj constr_ix (fst (snd (var f) @ [nil Ty.int]))))) in
                i+1, j, t
            in
              List.snoc acc (t', p'), i', j'
          in
          Triple.fst @@ List.fold_left2 make_bind ([],0,0) ps pcbs
        in
        Debug.printf "binds = %a@."
          Print.(list (
            fun fm (x,p) ->
              triple pattern typ term fm (p, x.typ, x)
          ))
          binds;
        let cond =
          let cond0 =
            Term.(fst (proj constr_ix (fst (snd (var f) @ [nil Ty.int]))))
          in
          let conds' =
            let make_cond (t,pt) (p,cond,_) =
              match p.pat_desc with
              | PAny
              | PVar _ -> true_term
              | _ -> make_match t [pt, true_term, true_term;
                                   make_pany p.pat_typ, true_term, false_term]
            in
            List.map2 make_cond binds pcbs
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
          | PTuple ps1, PTuple ps2 ->
              Format.eprintf"%a,%a@." Print.pattern p1' Print.pattern p2';
              unsupported "POr1"
          | _ ->
              Format.eprintf"%a,%a@." Print.pattern p1' Print.pattern p2';
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
        PVar x, make_and cond (make_is_some @@ make_var x), (make_get_val @@ make_var x, p')::bind
  in
  {pat_desc=desc; pat_typ=typ}, cond, bind


let abst_recdata_term (env: env) t =
  match t.desc with
  | Constr("Abst",[]) ->
      {desc = Constr("Abst",[]);
       typ  = abst_recdata.tr2_typ env t.typ;
       attr = []}
  | Constr(c,ts) when not (is_rec_type env t.typ) ->
      let aux (c',tys) =
        if c = c' then
          let t' = make_tuple @@ List.map (abst_recdata.tr2_term env) ts in
          Term.(pair true_ t')
        else
          let ty = make_ttuple @@ List.map (abst_recdata.tr2_typ env) tys in
          Term.(pair false_ (rand ty))
      in
      make_tuple
        [ make_tuple @@
            List.map aux @@ snd @@ decomp_tvariant @@ expand_typ env t.typ;
          Term.unit
        ]
  | Constr(c,ts) ->
      let ts' = List.map (abst_recdata.tr2_term env) ts in
      let xtys = List.map2 (fun t' t -> Id.new_var @@ expand_enc_typ env t'.typ, t.typ) ts' ts in
      let poly,labels = decomp_tvariant @@ expand_typ env t.typ in
      if poly then unsupported "encode_rec: polymorphic variant";
      let path = Id.new_var ~name:"path" Ty.(list int) in
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
          make_tuple
            [ List.combine ts xtys
              |> List.filter_out (fun (t',_) -> t.typ = t'.typ)
              |> List.map (snd |- fst |- make_var)
              |> make_return;
              Term.unit ]
        in
        make_pnil Ty.int, true_term, top
      in
      let make_pat (i,acc) (x,ty) =
        if ty = t.typ then
          let path' = Id.new_var ~name:"path'" Ty.(list int) in
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
        p', List.fold_right aux bind (make_and c c'), List.fold_right aux bind t'
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
      if Mutable =
          try fst @@ List.assoc s fields
          with Not_found -> assert false then
        unsupported "Mutable records"
      else
        make_proj (List.find_pos (fun _ (s',_) -> s = s') fields) @@ abst_recdata.tr2_term env t
  | SetField _ -> assert false
  | Local(Decl_type [s,ty], t) ->
      (* TODO subst_tdataは最初にまとめてやる *)
      let ty' = encode_recdata_typ env s ty in
      let env' = (s, (ty, List.mem s @@ get_tdata ty, ty')) :: env in
      subst_tdata s ty' @@ abst_recdata.tr2_term env' t
  | Local(Decl_type decls, t) ->
      (* TODO *)
      unsupported "encode_rec: Decl_type"
  (*| Var x ->*)
      (*Format.printf "VV_: %a@." Print.id_typ x;*)
      (*let x' = Id.map_typ (abst_recdata.tr2_typ env) x in*)
      (*Format.printf "WW_: %a@." Print.id_typ x';*)
      (*{ t with desc = Var x' }*)
  | _ -> abst_recdata.tr2_term_rec env t

(*let abst_recdata_var : env -> Syntax.id -> Syntax.id = fun env x ->*)
  (*Format.printf "VVV: %a@." Print.id_typ x;*)
  (*let x' = Id.map_typ (abst_recdata.tr2_typ env) x in*)
  (*Format.printf "WWW: %a@." Print.id_typ x';*)
  (*x'*)

let () = abst_recdata.tr2_term <- abst_recdata_term
let () = abst_recdata.tr2_typ <- abst_recdata_typ
(*let () = abst_recdata.tr2_var <- abst_recdata_var*)


let typ_in_env ty tys =
  match ty with
  | TData s -> List.mem s tys
  | _ -> false

let pr s t = Debug.printf "##[encode_rec] %a:@.%a@.@." Color.s_red s Print.term_typ t

let trans_typ = abst_recdata.tr2_typ []
let trans_term t =
  let ty = trans_typ t.typ in
  t
  |@> pr "input"
  |> Trans.abst_ext_recdata
  |@> pr "abst_ext_rec"
  |@> Type_check.check ~ty
  |> abst_recdata.tr2_term []
  |@> pr "abst_rec"
  |@> Type_check.check ~ty
  |> Trans.simplify_match
  |@> pr "simpify_match"
  |@> Type_check.check ~ty

(* ********************************************** *
 * Encode in refinement type enviroment from here *
 * ********************************************** *)

let gather_env : Syntax.term -> env =
  let rec go env t = match t.desc with
    | Local(Decl_type [s,ty], t) ->
        let ty' = encode_recdata_typ env s ty in
        let env' = (s, (ty, List.mem s @@ get_tdata ty, ty')) :: env in
        go env' t
    (*| Local(_, t) -> go env t*) (* can be ignored because of Preprocess.Lift_type_decl *)
    | _ -> env
  in
    go []

let subst_all_tdata =
  subst_tdata_map -| List.map (Pair.map_snd Triple.trd)

type sym_env = (string * (term * (int, term) Map.t)) list

let trans_pred =
  let tr = make_trans2 () in
  let term (x, sym_env) t =
    match t.desc with
    | Match({desc=Var(y)}, pts) when Id.(x=y) ->
        let labels = List.map fst sym_env in
        let preds =
          snd @@ List.Labels.fold_right pts ~init:(labels,[]) ~f:(
            fun (p, cond, t) (remain, arms) ->
              assert (cond = true_term);
              match p.pat_desc with
              | PConstr(c, ps) when not (List.mem c remain) ->
                  (* already matched (would it be better to raise an error?) *)
                  (remain, List.snoc arms false_term)
              | PConstr(c, ps) ->
                  let is_c, map = try List.assoc c sym_env with _ -> assert false in
                  let remain' = List.remove remain c in
                  let sbst = List.Labels.filter_map (List.mapi Pair.pair ps) ~f:(
                    fun (j, p) ->
                      match p.pat_desc with
                      | PAny -> None
                      | PVar(x) -> (try Some (x, Map.find j map)
                                    with Not_found -> None)
                          (* TODO assert x is not in FV(t) when Not_found *)
                      | _ -> assert false
                  ) in
                  let t' = subst_map sbst t in
                  (remain', List.snoc arms (make_and is_c t'))
              | PVar(y) ->
                  let match_ = make_ors @@
                    try List.Labels.map (fun l -> fst @@ List.assoc l sym_env) remain
                    with Not_found -> assert false
                  in
                  let t' = subst y (make_var x) t in
                  ([], List.snoc arms (make_and match_ t'))
              | PAny ->
                  let match_ = make_ors @@
                    try List.Labels.map (fun l -> fst @@ List.assoc l sym_env) remain
                    with Not_found -> assert false
                  in
                  ([], List.snoc arms (make_and match_ t))
              | _ -> assert false
          )
        in make_ors preds
    | _ -> tr.tr2_term_rec (x,sym_env) t
  in
  tr.tr2_term <- term;
  term

(* e.g.1:
      type foo = Foo of (int * string) | Bar of (bool * int)
        => foo = ((bool * (int * string)) * (bool * (bool * int))) * unit
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      [ Foo, ([int, string], [int, string]);
        Bar, ([int, string], [int, string]);
      ]
  e.g.2:
      type tree = Leaf of int | Node of tree * int * bool * tree
        => tree = unit * (int list -> ((bool * (int)) * (bool * (int * bool))) * unit)
                                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      [ Leaf, ([int], [int]);
        Node, ([tree, int, bool, tree], [int, bool]);
      ]
*)
type lts = (string * (typ list * typ list)) list

let mk_sym_env s t (lts:lts) =
  List.Labels.mapi lts ~f:(
    fun i (l,(tys,tys')) ->
      let t_base = Term.(proj i t) in
      let is_l = Term.(fst t_base) in
      Debug.printf "tys  = %a@." Print.(list typ) tys;
      Debug.printf "tys' = %a@." Print.(list typ) tys';
      let map : (int, term) Map.t =
        (* i: index in tys
           j: index in tys' *)
        let rec go m i j tys tys' =
          match tys, tys' with
          | _, [] -> m
          | ty::tys, _ when ty = TData s ->
              go m (i+1) j tys tys'
          | _::tys, ty'::tys' ->
              let m' = Map.add i Term.(proj j (snd t_base)) m in
              go m' (i+1) (j+1) tys tys'
          | _ -> assert false
        in go Map.empty 0 0 tys tys'
      in
      (l, (is_l, map))
  )

let trans_rty_rec_data (s,x,t) (ty_before: typ) (ty_after: typ) =
  begin match ty_before, ty_after with
  | TVariant(false,ts), TTuple([{Id.typ=ty1};{Id.typ=ty2}]) ->
      assert (ty1 = Ty.unit);
      (* ty_after should is the form of [unit * (int list -> t_variant * unit)]
         where t_variant = (bool * _) * (bool * _) * ...
      *)
      let t_variant =
        let arg, ret_ty = decomp_tfun ty2 in
        assert (List.length arg = 1 && Id.typ (List.hd arg) = Ty.(list int));
        match decomp_ttuple ret_ty with
        | [t_variant; t_unit] -> assert (t_unit = Ty.unit); t_variant
        | _ -> assert false
      in
      let v = Id.new_var t_variant in
      let lts = List.Labels.map2 ts (decomp_ttuple t_variant) ~f:(
        fun (l,tys) x -> (l,(tys, decomp_ttuple (snd_typ x)))
      ) in
      let sym_env : sym_env= mk_sym_env s (make_var v) lts in
      Debug.printf "sym_env = %a@."
        Print.(list (pair
          string
          (fun fm (t, map) ->
            Print.(pair term (list (pair int term)))
              fm (t, Map.bindings map))))
        sym_env;
      let path = Id.new_var ~name:"path" Ty.(list int) in
      let u = Id.new_var Ty.unit in
      let t' = Term.(var path <> nil Ty.int || trans_pred (x,sym_env) t) in
      Debug.printf "%a@." Print.term' t';
      Type_check.check ~ty:Ty.bool t';
      let rty =
        Ref_type.(Tuple(
          [ v, of_simple t_variant;
            u, Base(TUnit, u, t')
          ]))
      in
      let x = Ref_type.Fun(path, Ref_type.of_simple Ty.(list int), rty) in
      Ref_type.Tuple
        [ Id.new_var Ty.unit,
            Ref_type.of_simple Ty.unit
        ; Id.new_var (Ref_type.to_simple x),
            x
        ]

  | _ -> assert false
  end

let trans_rty_nonrec_data (s,x,t) (ty_before: typ) (ty_after: typ) =
  match ty_before, ty_after with
  | TVariant(false,ts), TTuple([{Id.typ=ty1};{Id.typ=ty2}]) ->
      let xs = match ty1 with TTuple(xs) -> xs | _ -> assert false in
      assert (ty2 = Ty.unit);
      let v = Id.new_var ty1 in
      let lts = List.Labels.map2 ts xs ~f:(
        fun (l,tys) x -> (l,(tys, decomp_ttuple (snd_typ @@ Id.typ x)))
      ) in
      let sym_env = mk_sym_env s (make_var v) lts in
      Debug.printf "sym_env = %a@."
        Print.(list (pair
          string
          (fun fm (t, map) ->
            Print.(pair term (list (pair int term)))
              fm (t, Map.bindings map))))
        sym_env;
      let u = Id.new_var Ty.unit in
      let t' = trans_pred (x,sym_env) t in
      Type_check.check ~ty:Ty.bool t';
      Ref_type.(Tuple(
        [ v, of_simple ty1;
          u, Base(TUnit, u, t')
        ]))
  | _, _ -> assert false

let rec trans_rty env =
  let tr = make_trans() in
  tr.tr_typ <- expand_enc_typ env;
  let open Ref_type in
  function
  | ADT(s,x,t) ->
      let ty_before = expand_typ env (TData s) in
      let ty_after = expand_enc_typ env (TData s) in
      if is_rec_type env (TData s)
      then trans_rty_rec_data (s,x,t) ty_before ty_after
      else trans_rty_nonrec_data (s,x,t) ty_before ty_after
  | Base(base,x,t) -> Base(base, tr.tr_var x, tr.tr_term t)
  | Fun(x,ty1,ty2) -> Fun(tr.tr_var x, trans_rty env ty1, trans_rty env ty2)
  | Tuple xtys -> Tuple(List.map (Pair.map tr.tr_var (trans_rty env)) xtys)
  | Inter(sty,tys) -> Inter(tr.tr_typ sty, List.map (trans_rty env) tys)
  | Union(sty,tys) -> Union(tr.tr_typ sty, List.map (trans_rty env) tys)
  | ExtArg(x,ty1,ty2) -> ExtArg(tr.tr_var x, trans_rty env ty1, trans_rty env ty2)
  | List(x,p_len,y,p_i,ty2) -> List(tr.tr_var x,
                                    tr.tr_term p_len,
                                    tr.tr_var y,
                                    tr.tr_term p_i,
                                    trans_rty env ty2)
  | Exn(ty1,ty2) -> Exn(trans_rty env ty1, trans_rty env ty2)


let trans_rid = abst_recdata.tr2_var

let trans_env : env -> (Syntax.id * Ref_type.t) list -> (Syntax.id * Ref_type.t) list = fun env renv ->
  List.map (Pair.map (trans_rid env) (trans_rty env)) renv

(* TODO: support records in refinement types *)
let trans p =
  let env = gather_env @@ Problem.term p in
  Problem.map ~tr_env:(trans_env env) trans_term p

