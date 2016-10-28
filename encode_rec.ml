open Util
open Syntax
open Term_util
open Type

module RT = Ref_type

module Debug = Debug.Make(struct let check () = List.mem "Encode_rec" !Flag.debug_module end)

let rec extract_decls_typ ?prev_type env typ =
  match typ with
  | Type(_, s) when List.mem s env ->
      [], TData s
  | Type(decls, s) ->
      let typ' = List.assoc s decls in
      let ds',typ'' = extract_decls_typ ~prev_type:s (s::env) typ' in
      (s,typ'')::ds', TData s
  | TVariant labels ->
      let ds,labels' =
        let aux (s,typs) (ds,ls) =
          let ds',typs' = List.fold_right (fun typ (ds,typs) -> let ds',typ' = extract_decls_typ env typ in ds'@ds, typ'::typs) typs (ds,[]) in
          ds', (s, typs')::ls
        in
        List.fold_right aux labels ([],[])
      in
      let typ' = TVariant labels' in
      if prev_type <> None then
        ds, typ'
      else
        if get_free_data_name typ = [] then
          ds, typ'
        else
          begin
            try
              let s = List.find (data_occurs -$- typ) env ^ "'" in
              (s,typ')::ds, TData s
            with Not_found -> ds, typ'
          end
  | TRecord fields ->
      unsupported "extract_decls_typ"
  | _ -> [], typ
let extract_decls_typ typ = extract_decls_typ [] typ


let flatten_recdata_typ typ =
  Debug.printf "flatten_recdata_typ IN: %a@." Print.typ typ;
  let typ' = fold_data_type typ in
  match typ' with
  | Type(_,s) ->
      let decls',_ = extract_decls_typ typ' in
      Type(decls', s)
      |@> Debug.printf "flatten_recdata_typ OUT: %a@." Print.typ
  | _ -> invalid_arg "flatten_recdata_typ"


let rec collect_leaf_aux typ =
  match typ with
  | Type _ -> invalid_arg "collect_leaf_aux"
  | TData _ -> []
  | TTuple [x] -> collect_leaf_aux @@ Id.typ x
  | TVariant labels -> List.flatten_map (snd |- List.flatten_map collect_leaf_aux) labels
  | TRecord fields -> List.flatten_map (snd |- snd |- collect_leaf_aux) fields
  | _ when get_free_data_name typ = [] -> [typ]
  | _ -> unsupported "non-regular data types"
let collect_leaf_aux typ =
  Debug.printf "CLA: %a@." Print.typ typ;
  List.unique @@ collect_leaf_aux typ
let collect_leaf typ =
  match typ with
  | Type(decls, s) ->
      let leaves = List.unique @@ List.flatten_map (snd |- collect_leaf_aux) decls in
      if List.exists (data_occurs s) leaves then
        unsupported "non-regular data types";
      leaves
  | TVariant _ -> collect_leaf_aux typ
  | _ ->
      Debug.printf "%a@." Print.typ typ;
      invalid_arg "collect_leaf"


let abst_recdata = make_trans ()

let abst_recdata_leaves typs =
  let typs' = List.map abst_recdata.tr_typ typs in
  let r_typ =
    if typs = []
    then TInt
    else make_ttuple [TInt; make_ttuple typs']
  in
  make_ttuple [TUnit; (* extra-param *)
               pureTFun(Id.new_var ~name:"path" @@ make_tlist TInt, r_typ)]

let abst_recdata_typ typ =
  match fold_data_type typ with
  | TRecord fields -> unsupported "abst_recdata_typ TRecord"
  | TVariant labels ->
      labels
      |> List.flatten_map (snd |- List.flatten_map collect_leaf_aux)
      |> List.unique
      |> abst_recdata_leaves
  | Type _ ->
      typ
      |> flatten_recdata_typ
      |> collect_leaf
      |> abst_recdata_leaves
  (*
  | TData(s,true) when Type_decl.is_record s ->
      if Type_decl.is_mutable s
      then unsupported "Mutable records"
      else
        let sftyps =
          match Type_decl.assoc s with
          | Type_decl.TKRecord sftyps -> sftyps
          | _ -> assert false
        in
        make_ttuple @@ List.map (abst_recdata.tr_typ -| snd -| snd) sftyps
  | TData(s,true) -> assert false
 *)
  | TApp(TOption, [typ]) -> opt_typ (abst_recdata.tr_typ typ)
  | _ -> abst_recdata.tr_typ_rec typ

let abst_label typ s =
  let rec pos typ =
    match typ with
    | TVariant labels -> List.find_pos (fun _ (s',_) -> s = s') labels
    | Type(decls, s') -> pos @@ List.assoc s' decls
    | _ ->
        Format.printf "%a@." Print.typ typ;
        assert false
  in
  make_int (1 + pos typ)

let get_ground_types typ =
  match abst_recdata.tr_typ typ with
  | TTuple (_::[{Id.typ=TFun(_, TInt)}]) -> []
  | TTuple (_::[{Id.typ=TFun(_, TTuple [_; {Id.typ=TTuple xs}])}]) -> List.map Id.typ xs
  | _ -> assert false

let rec abst_recdata_pat p =
  let typ = abst_recdata.tr_typ p.pat_typ in
  let desc,cond,bind =
    match p.pat_desc with
    | PAny -> PAny, true_term, []
    | PVar x -> PVar (abst_recdata.tr_var x), true_term, []
    | PAlias(p,x) ->
        let p',cond,bind = abst_recdata_pat p in
        PAlias(p', abst_recdata.tr_var x), cond, bind
    | PConst t -> PConst t, true_term, []
    | PConstruct(c,ps) ->
        let f = Id.new_var typ in
        let ppcbs = List.map (Pair.add_right abst_recdata_pat) ps in
        let ground_types = get_ground_types p.pat_typ in
        let binds =
          let make_bind i (p,(p',_,_)) =
            let t =
              if List.mem ~eq:Type.same_shape p.pat_typ ground_types then
                let rec find c = function
                  | [] -> assert false
                  | typ::typs -> if Type.same_shape typ p.pat_typ then c else find (c+1) typs
                in
                let j = find 0 ground_types in
                let t = make_app (make_snd @@ make_var f) [make_cons (make_int i) (make_nil TInt)] in
                make_proj j @@ make_snd t
              else
                let path = Id.new_var ~name:"path" (make_tlist TInt) in
                make_pair unit_term @@ (* extra-param *)
                  make_fun path @@
                    make_app (make_snd @@ make_var f) [make_cons (make_int i) (make_var path)]
            in
            t, p'
          in
          List.mapi make_bind ppcbs
        in
        let cond =
          let conds' =
            let make_cond (t,pt) (_,(p,cond,_)) =
              match p.pat_desc with
              | PAny
              | PVar _ -> true_term
              | _ -> make_match t [pt,true_term,true_term; make_pany p.pat_typ,true_term,false_term]
            in
            List.map2 make_cond binds ppcbs
          in
          let cond0 =
            let t = make_app (make_snd @@ make_var f) [make_nil TInt] in
            Format.printf "ground_types: %a@." (List.print Print.typ) ground_types;
            let t' = if ground_types = [] then t else make_proj 0 t in
            make_eq t' (abst_label p.pat_typ c)
          in
          List.fold_left make_and true_term (cond0 :: conds')
        in
        let bind = binds @ List.flatten_map (fun (_,(_,_,bind)) -> bind) ppcbs in
        PVar f, cond, bind
    | PNil -> PNil, true_term, []
    | PCons(p1,p2) ->
        let p1',cond1,bind1 = abst_recdata_pat p1 in
        let p2',cond2,bind2 = abst_recdata_pat p2 in
        PCons(p1',p2'), make_and cond1 cond2, bind1@bind2
    | PRecord fields ->
        let aux (s,p) (ps, cond, bind) =
          let p', cond', bind' = abst_recdata_pat p in
          p'::ps, make_and cond' cond, bind'@bind
        in
        let ps,cond,bind = List.fold_right aux fields ([],true_term,[]) in
        PTuple ps, cond, bind
    | POr(p1,p2) -> assert false
    | PTuple ps ->
        let aux p (ps,cond,bind) =
          let p',cond',bind' = abst_recdata_pat p in
          p'::ps, make_and cond' cond, bind' @ bind
        in
        let ps',cond,bind = List.fold_right aux ps ([],true_term,[]) in
        PTuple ps', cond, bind
    | PNone ->
        let x = Id.new_var typ in
        PVar x, make_is_none @@ make_var x, []
    | PSome p ->
        let p',cond,bind = abst_recdata_pat p in
        let x = Id.new_var typ in
        PVar x, make_is_some @@ make_var x, (make_get_val @@ make_var x, p')::bind
  in
  {pat_desc=desc; pat_typ=typ}, cond, bind


let abst_recdata_term t =
  match t.desc with
  | Constr("Abst",[]) -> {desc=Constr("Abst",[]); typ=abst_recdata.tr_typ t.typ; attr=[]}
  | Constr(c,ts) ->
      let ts' = List.map abst_recdata.tr_term ts in
      let ground_types = get_ground_types t.typ in
      let make_return label typ t =
        let head = Option.default (make_int 0) label in
        match ground_types with
        | [] -> head
        | _ ->
            let bodies =
              let aux typ' = if same_shape typ typ' then t else make_term @@ abst_recdata.tr_typ typ' in
              List.map aux ground_types
            in
            make_pair head @@ make_tuple bodies
      in
      let path = Id.new_var ~name:"path" @@ make_tlist TInt in
      let pat0 = make_pnil TInt, true_term, make_return (Some (abst_label t.typ c)) TUnit unit_term in
      let make_pat i (x,typ) =
        let path' = Id.new_var ~name:"path'" @@ make_tlist TInt in
        let t =
          if List.exists (same_shape @@ Id.typ x) ground_types then
            make_return None typ @@ make_var x
          else
            make_app (make_snd @@ make_var x) [make_var path']
        in
        make_pcons (make_pconst @@ make_int i) (make_pvar path'), true_term, t
      in
      let xtyps = List.map (fun t -> Id.new_var @@ abst_recdata.tr_typ t.typ |@> Format.printf "%a, %a@." Print.typ t.typ Print.id_typ, t.typ) ts in
      let pats = List.mapi make_pat xtyps in
      let defs = List.map2 (fun (x,_) t -> x, [], t) xtyps ts' in
      make_lets defs @@
        make_pair unit_term @@ (* for adding predicates *)
          make_fun path @@
            make_match (make_var path) (pat0::pats)
  | Match(t1,pats) ->
      let aux (p,c,t) =
        let p',c',bind = abst_recdata_pat p in
        let t' = abst_recdata.tr_term t in
        let aux (t,p) t' =
          make_match t [p, true_term, t';
                        make_pany p.pat_typ, true_term, make_bottom t'.typ]
        in
        p', make_and c c', List.fold_right aux bind t'
      in
      let t1' = abst_recdata.tr_term t1 in
      let pats' = List.map aux pats in
      make_match t1' pats'
  | TNone -> make_none @@ abst_recdata.tr_typ @@ option_typ t.typ
  | TSome t -> make_some @@ abst_recdata.tr_term t
  | Record [] -> assert false
  | Record fields ->
      if List.exists (fun (_,(f,_)) -> f = Mutable) @@ decomp_trecord t.typ
      then unsupported "Mutable records"
      else make_tuple @@ List.map (abst_recdata.tr_term -| snd) fields
  | Field(t,s) ->
      let fields = decomp_trecord t.typ in
      if Mutable = fst @@ List.assoc s fields then
        unsupported "Mutable records"
      else
        make_proj (List.find_pos (fun _ (s',_) -> s = s') fields) @@ abst_recdata.tr_term t
  | SetField _ -> assert false
  | _ -> abst_recdata.tr_term_rec t

let () = abst_recdata.tr_term <- abst_recdata_term
let () = abst_recdata.tr_typ <- abst_recdata_typ






let pr s t = Debug.printf "##[encode_rec] %a:@.%a@.@." Color.s_red s Print.term' t

let trans_typ = abst_recdata.tr_typ
let trans t =
  let typ = trans_typ t.typ in
  t
  |@> pr "input"
  |> Trans.remove_top_por
  |@> pr "remove_top_por"
  |@> Type_check.check -$- t.typ
  |> abst_recdata.tr_term
  |@> pr "abst_rec"
  |@> Type_check.check -$- typ
  |> Trans.simplify_match
  |@> Type_check.check -$- typ
