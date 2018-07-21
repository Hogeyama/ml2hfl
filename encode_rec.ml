open Util
open Syntax
open Term_util
open Type

module Debug = Debug.Make(struct let check = Flag.Debug.make_check __MODULE__ end)

let make_tuple' ts =
  match ts with
  | [t] -> t
  | _ -> make_tuple ts
let make_ttuple' : Syntax.typ list -> Syntax.typ =
  fun tys -> match tys with
  | [ty] -> ty
  | _ -> make_ttuple tys
let make_ptuple' ps =
  match ps with
  | [p] -> p
  | _ -> make_ptuple ps
module Term = struct
  include Term
  let tuple' = make_tuple'
end
module Ty = struct
  include Ty
  let tuple' = make_ttuple'
end

(******************************************************************************)
(* abst_recdata ここから *)

type env = (string * (Syntax.typ * bool * Syntax.typ)) list
  (* string      -- 's' of 'TData s'
   * Syntax.type -- type before encoding
   * bool        -- whether recursive or not
   * Syntax.type -- type after encoding
   * *)
type 'a transducer = env -> 'a -> 'a
let abst_recdata : env Syntax.trans2 = make_trans2 ()

(*
let abst_recdata_leaves env typs =
  let typs' = List.map (abst_recdata.tr2_typ env) typs in
  let r_typ =
    if typs = []
    then Ty.int
    else Ty.(tuple [int; tuple' typs'])
  in
  Ty.(tuple [unit; (* extra-param *)
             pureTFun(Id.new_var ~name:"path" @@ list int, r_typ)])
*)

(* Example:
 *    type tree = Leaf of int | Node of tree * int * tree
 * i.e.,
 *    s = "tree",
 *    ty = TVariant(false, [ ("Leaf",int)
 *                         ; ("Node", TData "tree" * int * TData "tree")])
 * is encoded into
 *    unit * (path:int list -> TTuple [ TTuple [bool; int]
 *                                    ; TTuple [bool; TTuple [] ])
 * Note that
 *    make_tuple' []  = TTuple [] and
 *    make_tuple' int = int
 * *)
(* 今考えているのはrecを含まないのでアレでした．まあいずれやるので *)
let encode_recdata_typ env s ty =
  match ty with
  | TVariant(false,labels) when List.mem s @@ get_tdata ty ->
      let tys =
        (* これなにやってんだ -> 完全に理解した *)
        let aux ty =
          match ty with
          | TData s' ->
              if s' = s then
                None
              else
                unsupported "encode_variant: non-simple recursion"
          | _ ->
              if get_tdata ty = [] then
                Some (abst_recdata.tr2_typ env ty)
              else
                unsupported "encode_variant: non-simple recursion"
        in
        List.map (make_tpair Ty.bool) @@ List.map (make_ttuple' -| List.filter_map aux -| snd) labels
      in
      Ty.(pair unit (pureTFun(Id.new_var ~name:"path" @@ list Ty.int, tuple' tys)))
  | _ -> abst_recdata.tr2_typ env ty

(* recを含まない場合（上のwildcardのケースからも呼ばれる） *)
let abst_recdata_typ : Syntax.typ transducer = fun env typ ->
  match typ with
  | TRecord fields -> unsupported "abst_recdata_typ TRecord"
  | TVariant(false,labels) ->
      let aux (s,tys) = Ty.(pair bool (tuple' @@ List.map (abst_recdata.tr2_typ env) tys)) in
      make_ttuple @@ List.map aux labels
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
          with Not_found -> assert false
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
          make_ptuple' ps, cond, bind
        in
        let ps' =
          let aux (c',tys) =
            if c = c' then
              Pat.(pair true_ p')
            else
              let ty = make_ttuple' @@ List.map (abst_recdata.tr2_typ env) tys in
              Pat.(pair false_ (__ ty))
          in
          let poly,decls = decomp_tvariant @@ expand_typ env p.pat_typ in
          if poly then unsupported "encode_rec: polymorphic variant";
          List.map aux decls
        in
        PTuple ps', true_term, []
    | PConstr(c,ps) ->
        let f = Id.new_var typ in
        Format.printf "f: %a@." Print.id_typ f;
        let pcbs = List.map (abst_recdata_pat env) ps in
        let binds =
          let make_bind (acc,i) p (p',_,_) =
            let i',t' =
              if is_rec_type env p.pat_typ then
                let path = Id.new_var ~name:"path" Ty.(list int) in
                i+1, Term.(pair unit (* extra-param *)
                                (fun_ path (snd (var f) @ [list [int i]])))
              else
                i, Term.(snd (proj i (snd (var f) @ [list [int i]])))
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
            let poly,decls = decomp_tvariant @@ expand_typ env p.pat_typ in
            if poly then unsupported "encode_rec: polymorphic variant";
            let i = List.find_pos (fun _ (c',_) -> c = c') decls in
            Term.(fst (proj i (snd (var f) @ [nil Ty.int])))
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
  | Constr("Abst",[]) -> {desc=Constr("Abst",[]); typ=abst_recdata.tr2_typ env t.typ; attr=[]}
  | Constr(c,ts) when not (is_rec_type env t.typ) ->
      let aux (c',tys) =
        if c = c' then
          let t' = make_tuple' @@ List.map (abst_recdata.tr2_term env) ts in
          Term.(pair true_ t')
        else
          let ty = make_ttuple' @@ List.map (abst_recdata.tr2_typ env) tys in
          Term.(pair false_ (rand ty))
      in
      make_tuple @@ List.map aux @@ snd @@ decomp_tvariant @@ expand_typ env t.typ
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
                Term.(pair true_ (tuple' ts'))
              else
                let tys' = List.filter_out ((=) t.typ) tys in
                let ty = make_ttuple' @@ List.map (abst_recdata.tr2_typ env) tys' in
                Term.(pair false_ (rand ty))
            in
            make_tuple @@ List.map aux labels
          in
          List.combine ts xtys
          |> List.filter_out (fun (t',_) -> t.typ = t'.typ)
          |> List.map (snd |- fst |- make_var)
          |> make_return
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
      let ty' = encode_recdata_typ env s ty in
      let env' = (s, (ty, List.mem s @@ get_tdata ty, ty')) :: env in
      subst_tdata s ty' @@ abst_recdata.tr2_term_rec env' t
  | Local(Decl_type decls, t) ->
      (* iwayama TODO *)
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

(* abst_recdata ここまで *)
(******************************************************************************)

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
  (* TODO これ何やってるかわからん
   * trans.ml:l2717辺りを見た感じ不明な型をintにしている？
   *)
  |@> pr "abst_ext_rec"
  |@> Type_check.check ~ty
  |> abst_recdata.tr2_term []
  |@> pr "abst_rec"
  |@> Type_check.check ~ty
  |> Trans.simplify_match
  |@> pr "simpify_match"
  |@> Type_check.check ~ty

(******************************************************************************)

(* toplevelにしかtypeの定義は出てこないのでこれで大丈夫なはず
 * TODO: merge with trans_term
 *)
let gather_env : Syntax.term -> env =
  let rec go env t = match t.desc with
    | Local(Decl_type [s,ty], t) ->
        let ty' = encode_recdata_typ env s ty in
        let env' = (s, (ty, List.mem s @@ get_tdata ty, ty')) :: env in
        env'
    | Local(_, t) -> go env t
    | _ -> env
  in
    go []

let rec is_DNF =
  let rec is_prim t =
    match t.desc with
    | BinOp ((Or|And), _, _) | Not(_) -> false
    | _ -> true
  and is_literal t =
    match t.desc with
    | BinOp ((Or|And), _, _) -> false
    | Not(t) -> is_prim t
    | _ -> is_prim t
  and is_conjunctive t =
    match t.desc with
    | BinOp (Or, t1, t2) -> false
    | BinOp (And, t1, t2) -> is_conjunctive t1 && is_conjunctive t2
    | _ -> is_literal t
  and is_disjunctive t =
    match t.desc with
    | BinOp (Or, t1, t2) -> is_disjunctive t1 && is_disjunctive t2
    | _ -> is_conjunctive t
  in
  is_disjunctive

let rec decompose_DNF : Syntax.term -> Syntax.term list list =
  let rec de_literal t =
    match t.desc with
    | BinOp ((Or|And), _, _) -> assert false
    | _ -> t
  and de_conjunctive t =
    match t.desc with
    | BinOp (Or, t1, t2) -> assert false
    | BinOp (And, t1, t2) -> de_conjunctive t1 @ de_conjunctive t2
    | _ -> [de_literal t]
  and de_disjunctive t =
    match t.desc with
    | BinOp (Or, t1, t2) -> de_disjunctive t1 @ de_disjunctive t2
    | _ -> [de_conjunctive t]
  in
  de_disjunctive

exception UnMatch
(** Assumption:
      [x] is a variable of type [s] where [type s = ... | l of sty' | ...] and [s] is not recursive,
      [sty] is an encoding of sty', and
      [t_in_DNF] is a predicate for [x] in DNF.
    [trans_rty_one_case all_labels x t_in_DNF l sty] returns refinement type
    for the case of [l].
*)
let trans_rty_nonrec_data_one_case all_labels x t_in_DNF l sty =
  let ty_bool = Type.Ty.bool in
  let x_is_l = Id.new_var ~name:(Id.name x ^ "_is_" ^ l) ty_bool in
  let x_un_l = Id.new_var ~name:(Id.name x ^ "_un_" ^ l) sty in

  let check_is_label l' =
    if List.mem l' all_labels then ()
    else failwith @@ "unknown label: " ^ l' ^ " in SPEC"
  in

  let tr = make_trans() in
  begin
    let tr_term t : Syntax.term = match t.desc with
      | App({desc=Var(f)},[{desc=Var(y)}]) when x=y ->
          let f = Id.name f in
          let l'= BatString.lchop ~n:3 f in
          begin match () with
          | () when BatString.starts_with f "is_" && l = l' -> true_term
          | () when BatString.starts_with f "un_" && l = l' -> make_var x_un_l
          | () when BatString.starts_with f "is_" ||
                    BatString.starts_with f "un_" -> check_is_label l'; raise UnMatch
          | _ -> tr.tr_term_rec t
          end
      | _ -> tr.tr_term_rec t
    in
    tr.tr_term <- tr_term
  end;
  let trans_literal_pred pred =
    try tr.tr_term pred with UnMatch -> false_term
  in

  let rty = match sty with
    | Type.TBase base ->
        let pred = make_ors @@
            Term.(not (var x_is_l)) ::
            List.map
              (make_ands -| List.map trans_literal_pred)
              t_in_DNF
        in
        Ref_type.Base(base, x_un_l, pred)
    | ty ->
        Format.eprintf
          "non-base type in Ref_type: @.%a@."
          Syntax.pp_typ ty;
        unsupported "TODO: tuple in Data (e.g. Foo of int * bool)"
  in
    Ref_type.Tuple([(x_is_l, Ref_type.of_simple ty_bool);(x_un_l, rty)])


(** Assumption:
      [x] is a variable of non-recursive variant type [s].
      [t] is a predicate for [x].
      [ty_before] and [ty_after] is the body of [s] before/after encoding.
    for example,
      if [type s = Foo of int | Bar of bool], then
      [ty_before = TVariant(false, [("Foo",[int]);("Bar",[bool])])] and
      [ty_after  = (bool * int) * (bool * bool)]
*)
let trans_rty_nonrec_data (s, x, t) ty_before ty_after: Ref_type.t =
  let rts : (Syntax.id * Ref_type.t) list =
    match ty_before, ty_after with
    | TVariant(false,ts), Type.TTuple(xts) ->
        if not (is_DNF t) then
          unsupported @@ Format.asprintf "non DNF predicate for variant@.%a@." Syntax.pp_term t;
        let labels = List.map fst ts in
        let stypes = List.map (Type.snd_typ -| Id.typ) xts in
        let t_in_DNF = decompose_DNF t in
        begin try
          List.map2
            begin fun l sty ->
              let x_l = Id.new_var (make_ttuple [Type.Ty.bool; sty]) in
              let rty = trans_rty_nonrec_data_one_case labels x t_in_DNF l sty in
              x_l, rty
            end
            labels stypes
        with e ->
          print_endline (Printexc.to_string e);
          assert false
        end
    | _ -> assert false
  in
  Ref_type.Tuple(rts)


let rec trans_rty env =
  let tr = make_trans() in
  tr.tr_typ <-
    begin fun ty ->
      if ty = Type.typ_unknown then ty (* TODO typ_unknownはどうして残ってるんだろう *)
      else expand_enc_typ env ty
    end;
  let open Ref_type in
  function
  | ADT(s,x,t) when is_rec_type env (TData s) ->
      unsupported "encode of recursive data in refinement type"
  | ADT(s,x,t) when not @@ List.mem s prim_base_types ->
      let ty_before = expand_typ env (TData s) in
      let ty_after = expand_enc_typ env (TData s) in
      trans_rty_nonrec_data (s,x,t) ty_before ty_after
  | ADT(s,x,t) -> assert false
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

let trans_rid : env -> Syntax.id -> Syntax.id = fun env ->
  abst_recdata.tr2_var env

let trans_env : env -> (Syntax.id * Ref_type.t) list -> (Syntax.id * Ref_type.t) list = fun env renv ->
  List.map (Pair.map (trans_rid env) (trans_rty env)) renv

(* TODO: support records in refinement types *)
let trans p =
  let env = gather_env @@ Problem.term p in
  let p' = Problem.map ~tr_env:(trans_env env) trans_term p in
  p'

(* TODO: recdata, type check *)

