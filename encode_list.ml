open Util
open Syntax
open Term_util
open Type
open Type_decl


module RT = Ref_type


let rec get_rtyp_list rtyp typ =
  match rtyp, elim_tpred typ with
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
    | RT.Pair(x,rtyp1,rtyp2), TPair(y,typ) ->
        let rtyp1' = get_rtyp_list rtyp1 (Id.typ y) in
        let rtyp2' = get_rtyp_list rtyp2 typ in
          RT.Pair(x, rtyp1', rtyp2')
    | RT.ExtArg(x,rtyp1,rtyp2), _ ->
        RT.ExtArg(x, rtyp1, get_rtyp_list rtyp2 typ)
    | _ ->
        Format.printf "rtyp:%a@.typ:%a@." RT.print rtyp pp_print_typ typ;
        assert false

let get_rtyp_list_of typed f rtyp =
  let typ = Trans.assoc_typ f typed in
  let rtyp' = get_rtyp_list rtyp typ in
  if Flag.print_ref_typ_debug
  then Format.printf "LIST: %a: @[@[%a@]@ ==>@ @[%a@]@]@." Id.print f RT.print rtyp RT.print rtyp';
  rtyp'


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
  | TList typ -> TPair(Id.new_var "l" TInt, TFun(Id.new_var "i" TInt, abst_list_typ typ))
  | TConstr(s,b) -> TConstr(s,b)
  | TPair(x,typ) -> TPair(abst_list_var x, abst_list_typ typ)
  | TPred(x,ps) ->
      let ps' = List.map (abst_list "") ps in
        TPred(Id.set_typ x (abst_list_typ (Id.typ x)), ps')

and abst_list_var x = Id.set_typ x (abst_list_typ (Id.typ x))

and get_match_bind_cond t p =
  match p.pat_desc with
      PAny -> [], true_term
    | PVar x -> [abst_list_var x, t], true_term
    | PAlias(p,x) ->
        let bind,cond = get_match_bind_cond t p in
        (abst_list_var x, t)::bind, cond
    | PConst {desc=Const Unit} -> [], true_term
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
                aux (bind'@@@bind) (make_and cond cond') (i+1) ps
        in
        let len = List.length ps in
        let bind, cond = get_match_bind_cond (make_tl len t) p' in
          aux bind (make_and (make_leq (make_int len) (make_fst t)) cond) 0 ps
    | PRecord _ -> assert false
    | POr _ -> assert false
    | PPair(p1,p2) ->
        let bind1,cond1 = get_match_bind_cond (make_fst t) p1 in
        let bind2,cond2 = get_match_bind_cond (make_snd t) p2 in
          bind1@@@bind2, make_and cond1 cond2


and abst_list post t =
  let typ' = abst_list_typ t.typ in
    match t.desc with
        Const c -> t
      | Unknown -> assert false
      | Var x -> make_var (abst_list_var x)
      | RandInt b -> randint_term
      | RandValue(typ,b) -> t
      | Fun(x,t) -> make_fun (abst_list_var x) (abst_list post t)
      | App({desc=Var x}, [t]) when x = length_var -> make_fst (abst_list post t)
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
          let i = Id.new_var "i" TInt in
          let x = Id.new_var "x" t1'.typ in
          let xs = Id.new_var "xs" t2'.typ in
          let t11 = make_eq (make_var i) (make_int 0) in
          let t12 = make_var x in
          let t13 = make_app (make_snd (make_var xs)) [make_sub (make_var i) (make_int 1)] in
          let t_f = make_fun i (make_if t11 t12 t13) in
          let t_len = make_add (make_fst (make_var xs)) (make_int 1) in
          let cons = Id.new_var ("cons"^post) (TFun(x,TFun(xs,t2'.typ))) in
          make_let [cons, [x;xs], make_pair t_len t_f] (make_app (make_var cons) [t1'; t2'])
      | Constr("Abst",[]) -> t
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

let trans t =
  let t' = abst_list "" t in
  let () = if false then Format.printf "abst_list::@. @[%a@.@." Syntax.pp_print_term t' in
  let () = typ_excep := abst_list_typ !typ_excep in
  let () = Type_check.check t' Type.TUnit in
    t', get_rtyp_list_of t

























let rec make_term typ =
  match typ with
    TUnit -> unit_term
  | TBool -> true_term
  | TInt -> make_int 0
  | TFun(x,typ) -> make_fun x (make_term typ)
  | TPair(x,typ) -> make_pair (make_term @@ Id.typ x) (make_term typ)
  | _ -> Format.printf "ERROR:@.%a@." Syntax.pp_print_typ typ; assert false

let opt_typ typ = TPair(Id.new_var "x" TInt, typ)
let make_none typ = make_pair (make_int 0) (make_term typ)
let make_some t = make_pair (make_int 1) t
let make_is_none t = make_eq (make_fst t) (make_int 0)
let make_is_some t = make_not (make_is_none t)
let make_get_val t = make_snd t

let rec get_rtyp_list_opt rtyp typ = raise (Fatal "not implemented get_rtyp_list_opt")

let get_rtyp_list_of typed f rtyp =
  let typ = Trans.assoc_typ f typed in
  let rtyp' = get_rtyp_list_opt rtyp typ in
  if Flag.print_ref_typ_debug
  then Format.printf "LIST: %a: @[@[%a@]@ ==>@ @[%a@]@]@." Id.print f RT.print rtyp RT.print rtyp';
  rtyp'


let make_tl_opt n t =
  let x = Id.new_var "x" TInt in
  make_fun x (make_app t [make_add (make_var x) (make_int n)])


let abst_list_opt = make_trans ()

let abst_list_opt_typ typ =
  match typ with
    TVar{contents=None} -> raise (Fatal "Polymorphic types occur! (Abstract.abst_list_opt_typ)")
  | TList typ -> TFun(Id.new_var "i" TInt, opt_typ @@ abst_list_opt.tr_typ typ)
  | _ -> abst_list_opt.tr_typ_rec typ

let rec get_match_bind_cond t p =
  match p.pat_desc with
      PAny -> [], true_term
    | PVar x -> [abst_list_opt.tr_var x, t], true_term
    | PAlias(p,x) ->
        let bind,cond = get_match_bind_cond t p in
        (abst_list_opt.tr_var x, t)::bind, cond
    | PConst {desc=Const Unit} -> [], true_term
    | PConst t' -> [], make_eq t t'
    | PConstruct _ -> assert false
    | PNil -> [], make_is_none (make_app t [make_int 0])
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
              let bind',cond' = get_match_bind_cond (make_get_val (make_app t [make_int i])) p in
              aux (bind'@@@bind) (make_and cond cond') (i+1) ps
        in
        let len = List.length ps in
        let bind, cond = get_match_bind_cond (make_tl_opt len t) p' in
        aux bind (make_and (make_is_some (make_app t [make_int (len-1)])) cond) 0 ps
    | PRecord _ -> assert false
    | POr _ -> assert false
    | PPair(p1,p2) ->
        let bind1,cond1 = get_match_bind_cond (make_fst t) p1 in
        let bind2,cond2 = get_match_bind_cond (make_snd t) p2 in
        bind1@@@bind2, make_and cond1 cond2

let abst_list_opt_term t =
  let typ' = abst_list_opt.tr_typ t.typ in
  match t.desc with
    Nil ->
      let el_typ =
        match typ' with
          TFun(_, TPair(_,typ)) -> typ
        | _ -> Format.printf "ERROR:@.%a@." Syntax.pp_print_typ typ'; assert false
      in
      make_fun (Id.new_var "x" TInt) (make_none el_typ)
  | Cons(t1,t2) ->
      let t1' = abst_list_opt.tr_term t1 in
      let t2' = abst_list_opt.tr_term t2 in
      let i = Id.new_var "i" TInt in
      let x = Id.new_var "x" t1'.typ in
      let xs = Id.new_var "xs" t2'.typ in
      let t11 = make_eq (make_var i) (make_int 0) in
      let t12 = make_some (make_var x) in
      let t13 = make_app (make_var xs) [make_sub (make_var i) (make_int 1)] in
      let cons = Id.new_var "cons" (TFun(x,TFun(xs,t2'.typ))) in
      make_let [cons, [x;xs], make_fun i (make_if t11 t12 t13)] (make_app (make_var cons) [t1'; t2'])
  | Match(t1,pats) ->
      let x = Id.new_var "xs" (abst_list_opt.tr_typ t1.typ) in
      let aux (p,cond,t) t' =
        let bind,cond' = get_match_bind_cond (make_var x) p in
        let add_bind t = List.fold_left (fun t' (x,t) -> make_let [x, [], t] t') t bind in
        let t_cond =
          if cond = true_term
          then cond
          else add_bind (abst_list_opt.tr_term cond)
        in
        make_if (make_and cond' t_cond) (add_bind (abst_list_opt.tr_term t)) t'
      in
      let t_pats = List.fold_right aux pats (make_bottom typ') in
      make_let [x, [], abst_list_opt.tr_term t1] t_pats
  | _ -> abst_list_opt.tr_term_rec t

let () = abst_list_opt.tr_typ <- abst_list_opt_typ
let () = abst_list_opt.tr_term <- abst_list_opt_term

let trans_opt t =
  let t' = abst_list_opt.tr_term t in
  let t'' = Trans.subst_let_xy t' in
  let t'' = t' in
  let () = if false then Format.printf "abst_list::@. @[%a@.@." Syntax.pp_print_term t'' in
  let () = typ_excep := abst_list_opt.tr_typ !typ_excep in
  let () = Type_check.check t'' Type.TUnit in
  t'', get_rtyp_list_of t



let trans t =
  if !Flag.encode_list_opt
  then trans_opt t
  else trans t
