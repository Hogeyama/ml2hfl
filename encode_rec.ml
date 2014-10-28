
open Util
open Syntax
open Term_util
open Type


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



let debug () = List.mem "Encode_rec" !Flag.debug_module

let hd = function
  | [x] -> x
  | _ -> assert false



let abst_recdata = make_trans ()

let abst_recdata_typ typ =
  match typ with
  | TConstr(s,true) ->
      let typs = TInt :: Type_decl.get_ground_types s in
      let typs',typ = List.decomp_snoc typs in
      let r_typ = List.fold_right (fun typ1 typ2 -> TTuple[Id.new_var typ1; Id.new_var typ2]) typs' typ in
      TTuple [Id.new_var TUnit; (* extra-param *)
              Id.new_var @@ TFun(Id.new_var ~name:"path" (TList TInt), r_typ)]
  | TOption typ -> opt_typ (abst_recdata.tr_typ typ)
  | _ -> abst_recdata.tr_typ_rec typ

let abst_label c = make_int (1 + Type_decl.constr_pos c)

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
        let f = Id.new_var ~name:"f" typ in
        let ppcbs = List.map (fun p -> p, abst_recdata_pat p) ps in
        let typ_name = match p.pat_typ with TConstr(s,true) -> s | _ -> assert false in
        let ground_types = Type_decl.get_ground_types typ_name in
        let make_bind i (p,(p',_,_)) =
          let t =
            if List.mem p.pat_typ ground_types
            then
              let rec find c = function
                | [] -> assert false
                | typ::typs -> if typ = p.pat_typ then c else find (c+1) typs
              in
              let j = find 0 ground_types in
              let t = make_app (make_snd (make_var f)) [make_cons (make_int i) (make_nil TInt)] in
              make_nth (1 + j) (1 + List.length ground_types) t
            else
              let path = Id.new_var ~name:"path" (TList TInt) in
              make_pair unit_term (* extra-param *)
                        (make_fun path (make_app (make_snd (make_var f)) [make_cons (make_int i) (make_var path)]))
          in
          t, p'
        in
        let binds = List.mapi make_bind ppcbs in
        let make_cond (t,pt) (_,(p,cond,_)) =
          match p.pat_desc with
          | PAny
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
        PVar x, make_is_none (make_var x), []
    | PSome p ->
        let p',cond,bind = abst_recdata_pat p in
        let x = Id.new_var typ in
        PVar x, make_is_some (make_var x), (make_get_val (make_var x), p')::bind
  in
  {pat_desc=desc; pat_typ=typ}, cond, bind


let abst_recdata_term t =
  match t.desc with
  | Constr("Abst",[]) -> {desc=Constr("Abst",[]); typ=abst_recdata.tr_typ t.typ; attr=ANone}
  | Constr(c,ts) ->
      let ts' = List.map abst_recdata.tr_term ts in
      let typ_name = match t.typ with TConstr(s,true) -> s | _ -> assert false in
      let ground_types = Type_decl.get_ground_types typ_name in
      let make_return label typ t =
        let aux typ' = if typ = typ' then t else get_typ_default (abst_recdata.tr_typ typ') in
        let head = Option.default (make_int 0) label in
        let bodies = List.map aux ground_types in
        match bodies with
        | [] -> head
        | [t] -> make_pair head t
        | _ -> make_pair head (make_tuple bodies)
      in
      let path = Id.new_var ~name:"path" (TList TInt) in
      let pat0 = make_pnil TInt, true_term, make_return (Some (abst_label c)) TUnit unit_term in
      let xtyps = List.map (fun t -> Id.new_var (abst_recdata.tr_typ t.typ), t.typ) ts in
      let make_pat i (x,typ) =
        let path' = Id.new_var ~name:"path'" (TList TInt) in
        let t =
          if List.mem typ ground_types
          then make_return None typ (make_var x)
          else make_app (make_snd (make_var x)) [make_var path']
        in
        make_pcons (make_pconst (make_int i)) (make_pvar path'), true_term, t
      in
      let pats = List.mapi make_pat xtyps in
      let defs = List.map2 (fun (x,_) t -> x, [], t) xtyps ts' in
      make_lets defs (make_pair unit_term (make_fun path (make_match (make_var path) (pat0::pats))))
  | Match(t1,pats) ->
      let aux (p,c,t) =
        let p',c',bind = abst_recdata_pat p in
        let t' = abst_recdata.tr_term t in
        let aux (t,p) t' =
          make_match t [p, true_term, t'; make_pany p.pat_typ, true_term, make_loop t'.typ]
        in
        p', make_and c c', List.fold_right aux bind t'
      in
      let t1' = abst_recdata.tr_term t1 in
      let pats' = List.map aux pats in
      make_match t1' pats'
  | TNone -> make_none @@ abst_recdata.tr_typ @@ option_typ t.typ
  | TSome t -> make_some (abst_recdata.tr_term t)
  | _ -> abst_recdata.tr_term_rec t

let () = abst_recdata.tr_term <- abst_recdata_term
let () = abst_recdata.tr_typ <- abst_recdata_typ


let trans t =
  t
  |> Trans.remove_top_por
  |@> Fun.flip Type_check.check TUnit
  |> abst_recdata.tr_term
  |@> (fun _ -> typ_excep := abst_recdata.tr_typ !typ_excep)
  |@debug()&> Format.printf "%a:@.%a@.@." Color.s_red "abst_rec" print_term_typ
  |@> Fun.flip Type_check.check TUnit
  |> Trans.simplify_match
  |@> Fun.flip Type_check.check TUnit
