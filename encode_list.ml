open Util
open Syntax
open Term_util
open Type


module RT = Ref_type


module Debug = Debug.Make(struct let check = Flag.Debug.make_check __MODULE__ end)



let rec is_filled_pattern p =
  try
    match p.pat_desc with
    | PAny -> None
    | PNondet -> None
    | PVar x -> Some (make_var x)
    | PAlias(p1,x) ->
        Some (Option.default (make_var x) @@ is_filled_pattern p1)
    | PConst c -> Some c
    | PConstr(c, ps) ->
        let ts = List.map is_filled_pattern ps in
        Some (make_construct c (List.map Option.get ts) p.pat_typ)
    | PNil -> Some (make_nil @@ list_typ p.pat_typ)
    | PCons(p1,p2) ->
        let t1 = Option.get @@ is_filled_pattern p1 in
        let t2 = Option.get @@ is_filled_pattern p2 in
        Some (make_cons t1 t2)
    | PTuple ps ->
        let ts = List.map is_filled_pattern ps in
        Some (make_tuple @@ List.map Option.get ts)
    | PRecord fields ->
        let sftyps = decomp_trecord p.pat_typ in
        assert (List.length fields = List.length sftyps);
        Some (make_record (List.map (Pair.map_snd @@ Option.get -| is_filled_pattern) fields) p.pat_typ)
    | PNone -> Some (make_none @@ option_typ p.pat_typ)
    | PSome p1 -> Some (make_some @@ Option.get @@ is_filled_pattern p1)
    | POr _ -> None
  with Option.No_value -> None


let subst_matched_var =
  let tr = make_trans () in
  let tr_desc desc =
    match desc with
    | Match({desc=Var x}, pats) ->
        let aux (p,t1,t2) =
          let t1' = tr.tr_term t1 in
          let t2' = tr.tr_term t2 in
          let sbst =
            match is_filled_pattern p with
            | None -> Fun.id
            | Some t' ->
                fun t0 ->
                  match t0 with
                  | {desc=Const True} -> t0
                  | _ when !Flag.Method.tupling -> make_label (InfoIdTerm(x, t')) t0
                  | _ -> subst x t' t0
          in
          p, sbst t1', sbst t2'
        in
        Match(make_var x, List.map aux pats)
    | _ -> tr.tr_desc_rec desc
  in
  tr.tr_desc <- tr_desc;
  tr.tr_term



let rec get_rtyp_list rtyp typ =
  match rtyp, elim_tattr typ with
  | RT.Inter(_, rtyps), _ ->
     RT.Inter(typ, List.map (get_rtyp_list -$- typ) rtyps)
  | RT.Union(_, rtyps), _ ->
      RT.Union(typ, List.map (get_rtyp_list -$- typ) rtyps)
  (* TODO RT.Data *)
  | RT.Tuple[x, RT.Base(TInt, x', p_len); _, RT.Fun(y, RT.Base(TInt, y', p_i), typ2)], TApp(TList, [typ]) ->
      let p_len' = subst_var x' x p_len in
      let p_i' = subst_var y' y p_i in
      RT.List(x, p_len', y, p_i', get_rtyp_list typ2 typ)
  | RT.Tuple[x, RT.Base(TInt, x', p_len); _, RT.Inter(_, [])], TApp(TList, [typ]) ->
      let p_len' = subst_var x' x p_len in
      RT.List(x, p_len', Id.new_var typ_unknown, true_term, RT.Inter(typ, []))
  | RT.Tuple[x, RT.Base(TInt, x', p_len); _, RT.Inter(_, typs)], TApp(TList, [typ]) ->
      let typs' = List.map (fun typ -> RT.Tuple [x, RT.Base(TInt, x', p_len); Id.new_var typ_unknown, typ]) typs in
      get_rtyp_list (RT.Inter(typ_unknown, typs')) (make_tlist typ)
  | _, TApp(TList, [typ]) ->
      Format.eprintf "%a@." RT.print rtyp;
      raise (Fatal "not implemented get_rtyp_list")
  | RT.ADT(_,_,_), _ -> assert false
  | RT.Base(b,x,ps), _ -> RT.Base(b,x,ps)
  | RT.Fun(x,rtyp1,rtyp2), TFun(y,typ2) ->
      let rtyp1' = get_rtyp_list rtyp1 (Id.typ y) in
      let rtyp2' = get_rtyp_list rtyp2 typ2 in
      let rtyp2'' =
        match rtyp1' with
        | RT.List _ -> RT.replace_term Term.(fst (var x)) Term.(length (var x)) rtyp2'
        | _ -> rtyp2'
      in
      RT.Fun(x, rtyp1', rtyp2'')
  | RT.Tuple xrtyps, TTuple ys ->
      RT.Tuple (List.map2 (fun (x,rtyp) y -> x, get_rtyp_list rtyp (Id.typ y)) xrtyps ys)
  | RT.ExtArg(x,rtyp1,rtyp2), _ ->
      RT.ExtArg(x, rtyp1, get_rtyp_list rtyp2 typ)
  | RT.Exn(rtyp1,rtyp2), _ ->
      RT.Exn(get_rtyp_list rtyp1 typ, rtyp2)
  | _ ->
      Format.eprintf "rtyp:%a@.typ:%a@." RT.print rtyp Print.typ typ;
      assert false

let make_get_rtyp_list_of typed get_rtyp f =
  let typ = Trans.assoc_typ f typed in
  let rtyp = get_rtyp f in
  let rtyp' = get_rtyp_list rtyp typ in
  if !!Flag.Debug.print_ref_typ
  then Format.printf "LIST: %a: @[@[%a@]@ ==>@ @[%a@]@]@." Id.print f RT.print rtyp RT.print rtyp';
  rtyp'


let make_tl n t =
  let x = Id.new_var Ty.int in
  Term.(pair (fst t - int n) (fun_ x (snd t @ [var x + int n])))



let rec decomp_literal t =
  match t.desc with
  | Nil -> []
  | Cons(t1,t2) -> t1 :: decomp_literal t2
  | _ -> raise (Invalid_argument "decomp_literal")

let is_literal t =
 try
   ignore (decomp_literal t); true
 with Invalid_argument _ -> false

let abst_list = make_trans2 ()

let abst_list_typ post typ =
  match typ with
  | TVar({contents=None},_) -> fatal "Polymorphic types occur! (Encode_list.abst_list_typ)"
  | TApp(TList, [typ]) ->
      let l = Id.new_var ~name:"l" Ty.int in
      TTuple[l; Id.new_var @@ pureTFun(Id.new_var ~name:"i" Ty.int, abst_list.tr2_typ post typ)]
  | _ -> abst_list.tr2_typ_rec post typ

let print_bind fm bind =
  Format.fprintf fm "@[[";
  List.iter (fun (x,t) -> Format.fprintf fm "%a := %a;@ " Id.print x Print.term t) bind;
  Format.fprintf fm "]@]"

let add_bind bind t =
  let aux (x,t) t' =
    let x' =
      if Id.mem x @@ get_fv t then
        Id.new_var_id x
      else
        x
    in
    make_let_s [x',t] @@ subst_var x x' t'
  in
  List.fold_right aux bind t

(* "t" must have no side-effects *)
let rec get_match_bind_cond t p =
  match p.pat_desc with
  | PAny -> [], true_term
  | PVar x -> [abst_list.tr2_var "" x, t], true_term
  | PAlias(p,x) ->
      let bind,cond = get_match_bind_cond t p in
      let bind' = bind @ [abst_list.tr2_var "" x, t] in
      bind', add_bind bind' cond
  | PConst {desc=Const Unit} -> [], true_term
  | PConst t' -> [], make_eq t t'
  | PNil -> [], Term.(fst t <= int 0)
  | PCons _ ->
      let rec decomp = function
        | {pat_desc=PCons(p1,p2)} ->
            let ps,p = decomp p2 in
            p1::ps, p
        | p -> [], p
      in
      let ps,p' = decomp p in
      let rec aux bind cond i = function
        | [] -> bind, cond
        | p::ps ->
            let t' = Term.(snd t @ [int i]) in
            let x = new_var_of_term t' in
            let bind',cond' = get_match_bind_cond (make_var x) p in
            aux ((x,t')::bind'@bind) (make_and cond @@ add_bind [x,t'] cond') (i+1) ps
      in
      let len = List.length ps in
      let bind, cond = get_match_bind_cond (make_tl len t) p' in
      aux bind Term.(int len <= fst t && cond) 0 ps
  | PTuple ps ->
      let binds,conds = List.split @@ List.mapi (fun i p -> get_match_bind_cond (make_proj i t) p) ps in
      List.flatten binds, make_ands conds
  | PRecord fields -> assert false
  | POr(p1, p2) ->
      let bind1,cond1 = get_match_bind_cond t p1 in
      let bind2,cond2 = get_match_bind_cond t p2 in
      let cond2' = List.fold_right2 (fun (x1,_) (x2,_) -> subst_var x2 x1) bind1 bind2 cond2 in
      bind1, make_or cond1 cond2'
  | _ -> Format.eprintf "get_match_bind_cond: %a@." Print.pattern p; assert false

let rec make_rand typ =
  match typ with
  | TApp(TList, [typ']) ->
      let l = Id.new_var ~name:"l" Ty.int in
      make_let [l, randint_unit_term] @@
        make_assume
          (make_leq (make_int 0) (make_var l))
          (make_pair (make_var l) @@ make_fun (Id.new_var Ty.int) @@ make_rand typ')
  | _ -> make_rand_unit typ

let abst_list_term post t =
  match t.desc with
  | App({desc=Const(Rand(TBase TInt,false)); attr}, t2) when List.mem AAbst_under attr -> (* for disproving termination  *)
      assert (t2 = [unit_term]);
      t
  | App({desc=Const(Rand(typ,false))}, t2) ->
      assert (t2 = [unit_term]);
      make_rand_unit @@ abst_list.tr2_typ post t.typ
  | App({desc=Var x}, [t1; t2]) when Id.name x = "List.nth" ->
      let t1' = abst_list.tr2_term post t1 in
      let t2' = abst_list.tr2_term post t2 in
      make_app (make_snd t1') [t2']
  | App({desc=Var x}, [t]) when is_length_var x -> make_fst @@ abst_list.tr2_term post t
  | Local(Decl_let bindings, t2) ->
      let aux (f,t) =
        let post' = "_" ^ Id.name f in
        abst_list.tr2_var post f, abst_list.tr2_term post' t
      in
      let bindings' = List.map aux bindings in
      make_let bindings' (abst_list.tr2_term post t2)
  | Nil ->
      let typ'' = abst_list.tr2_typ post @@ list_typ t.typ in
      make_pair (make_int 0) (make_fun (Id.new_var Ty.int) (make_bottom typ''))
  | Cons _ when is_literal t ->
      let typ'' = abst_list.tr2_typ post @@ list_typ t.typ in
      let ts = decomp_literal t in
      let ts' = List.map (abst_list.tr2_term post) ts in
      let xs = List.map new_var_of_term ts' in
      let bindings = List.rev_map2 Pair.pair xs ts' in
      let x = Id.new_var ~name:"i" Ty.int in
      let aux y (i,t) =
        i-1, Term.(if_ (var x = int i) (var y) t)
      in
      let n = List.length ts in
      let _,t = List.fold_right aux xs (n-1, make_bottom typ'') in
      make_lets bindings Term.(pair (int n) (fun_ x t))
  | Cons(t1,t2) ->
      let t1' = abst_list.tr2_term post t1 in
      let t2' = abst_list.tr2_term post t2 in
      let i = Id.new_var ~name:"i" Ty.int in
      let x = Id.new_var ~name:"x" t1'.typ in
      let xs = Id.new_var ~name:"xs" t2'.typ in
      let t_f = Term.(fun_ i (if_ (var i = int 0) (var x) (snd (var xs) @ [var i - int 1]))) in
      let t_len = Term.(fst (var xs) + int 1) in
      let cns = Id.new_var ~name:("cons"^post) (TFun(x,TFun(xs,t2'.typ))) in
      Term.(let_ [cns, funs [x;xs] (pair t_len t_f)] (var cns @ [t1'; t2']))
  | Constr("Abst",[]) -> t
  | Constr(s,ts) -> assert false
  | Match(t1,pats) ->
      let x,bindx =
        let x = Id.new_var ~name:"xs" @@ abst_list.tr2_typ post t1.typ in
        x, fun t -> make_let [x, abst_list.tr2_term post t1] t
      in
      let aux (p,cond,t2) t3 =
        let cond' = abst_list.tr2_term post cond in
        let bind,cond2 = get_match_bind_cond (make_var x) p in
        let cond'' = add_bind bind cond' in
        let t_cond = make_and cond2 cond'' in (* The order of cond2 and cond'' is matter *)
        let t2' = abst_list.tr2_term post t2 in
        make_if t_cond (add_bind bind t2') t3
      in
      let t_pats = List.fold_right aux pats (make_bottom @@ abst_list.tr2_typ post t.typ) in
      bindx t_pats
  | _ -> abst_list.tr2_term_rec post t

let () = abst_list.tr2_term <- abst_list_term
let () = abst_list.tr2_typ <- abst_list_typ

let trans t =
  let t' =
    t
    |@> Debug.printf "[abst_list] input:@. @[%a@.@." Print.term'
    |> abst_list.tr2_term ""
    |@> Debug.printf "[abst_list] abst_list:@. @[%a@.@." Print.term_typ
    |> Trans.inline_var_const
    |@> Debug.printf "[abst_list] inline_var_const:@. @[%a@.@." Print.term_typ
  in
  let ty = abst_list.tr2_typ "" t.typ in
  Type_check.check t' ~ty;
  t', make_get_rtyp_list_of t





let make_list_eq typ =
  let f = Id.new_var ~name:"Primitive.list_eq" @@ pureTFun(Id.new_var ~name:"xs" @@ make_tlist typ, pureTFun(Id.new_var ~name:"xs" @@ make_tlist typ, Ty.bool)) in
  let xs = Id.new_var ~name:"xs'" @@ make_tlist typ in
  let ys = Id.new_var ~name:"ys'" @@ make_tlist typ in
  let t_eq =
    let pat_nil =
      let p1 = make_ppair (make_pnil typ) (make_pnil typ) in
      let t1 = true_term in
      p1, true_term, t1
    in
    let pat_cons =
      let x = Id.new_var ~name:"x" typ in
      let xs' = Id.new_var ~name:"xs'" @@ make_tlist typ in
      let y = Id.new_var ~name:"y" typ in
      let ys' = Id.new_var ~name:"ys'" @@ make_tlist typ in
      let p2 = make_ppair (make_pcons (make_pvar x) (make_pvar xs')) (make_pcons (make_pvar y) (make_pvar ys')) in
      let t2 = make_and (make_eq (make_var x) (make_var y)) (make_app (make_var f) [make_var xs'; make_var ys']) in
      p2, true_term, t2
    in
    let pat_any =
      let p3 = make_ppair (make_pany (make_tlist typ)) (make_pany (make_tlist typ)) in
      let t3 = false_term in
      p3, true_term, t3
    in
    make_match (make_pair (make_var xs) (make_var ys)) [pat_nil; pat_cons; pat_any]
  in
  f, make_funs [xs;ys] t_eq

(* TODO: support other types *)
let inst_list_eq = make_trans2 ()
let inst_list_eq_term map t =
  match t.desc with
  | BinOp(Eq, ({desc=Var(path)}), {desc=Nil; typ=TApp(TList, [typ])}) when path.Id.name = "path" ->
      (* XXX temporary measure
         path = nil
       *)
      let t1' = make_var @@ Id.set_typ path Ty.(pair int (fun_ int typ)) in
      Term.(fst t1' = int 0)
  | BinOp(Eq, t1, t2) ->
      let t1' = inst_list_eq.tr2_term map t1 in
      let t2' = inst_list_eq.tr2_term map t2 in
      begin
        match t1.typ with
        | TApp(TList, [TBase TInt|TData _ as typ]) when List.mem_assoc typ map ->
            if !Flag.Encode.abst_list_eq then
              (Flag.add_use_abst "List equality";
               randbool_unit_term)
            else
              make_app (make_var @@ List.assoc typ map) [t1'; t2']
        | TApp(TList, _) ->
            Flag.add_use_abst "List equality";
            randbool_unit_term
        | _ -> inst_list_eq.tr2_term_rec map t
      end
  | _ -> inst_list_eq.tr2_term_rec map t
let () = inst_list_eq.tr2_term <- inst_list_eq_term
let inst_list_eq t =
  let defs = List.map (Pair.add_right make_list_eq) [Ty.int; TData "string"] in(* TODO *)
  let map = List.map (Pair.map_snd fst) defs in
  let t' = inst_list_eq.tr2_term map t in
  let fv = get_fv t' in
  let defs' = List.filter (fun (_,(f,_)) -> Id.mem f fv) defs in
  if !Flag.Encode.abst_list_eq then
    t'
  else
    make_lets (List.map snd defs') t'




let rec get_rtyp_list_opt rtyp typ = raise (Fatal "not implemented get_rtyp_list_opt")

let get_rtyp_list_of typed f rtyp =
  let typ = Trans.assoc_typ f typed in
  let rtyp' = get_rtyp_list_opt rtyp typ in
  if !!Flag.Debug.print_ref_typ
  then Format.printf "LIST: %a: @[@[%a@]@ ==>@ @[%a@]@]@." Id.print f RT.print rtyp RT.print rtyp';
  rtyp'


let make_tl_opt n t =
  let x = Id.new_var ~name:"x" Ty.int in
  make_fun x (make_app t [make_add (make_var x) (make_int n)])


let abst_list_opt = make_trans ()

let abst_list_opt_typ typ =
  match typ with
  | TVar({contents=None},_) -> raise (Fatal "Polymorphic types occur! (Encode_list.abst_list_opt_typ)")
  | TApp(TList, [typ]) -> TFun(Id.new_var ~name:"i" Ty.int, opt_typ @@ abst_list_opt.tr_typ typ)
  | _ -> abst_list_opt.tr_typ_rec typ

let rec get_match_bind_cond_opt t p =
  match p.pat_desc with
  | PAny -> [], true_term
  | PVar x -> [abst_list_opt.tr_var x, t], true_term
  | PAlias(p,x) ->
      let bind,cond = get_match_bind_cond_opt t p in
      (abst_list_opt.tr_var x, t)::bind, cond
  | PConst {desc=Const Unit} -> [], true_term
  | PConst t' -> [], make_eq t t'
  | PConstr _ -> assert false
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
            let bind',cond' = get_match_bind_cond_opt (make_get_val (make_app t [make_int i])) p in
            aux (bind'@bind) (make_and cond cond') (i+1) ps
      in
      let len = List.length ps in
      let bind, cond = get_match_bind_cond_opt (make_tl_opt len t) p' in
      aux bind (make_and (make_is_some (make_app t [make_int (len-1)])) cond) 0 ps
  | PTuple ps ->
      let binds,conds = List.split @@ List.mapi (fun i p -> get_match_bind_cond_opt (make_proj i t) p) ps in
      List.rev_flatten binds,
      List.fold_left make_and true_term conds
  | _ -> Format.eprintf "get_match_bind_cond_opt: %a@." Print.pattern p; assert false

let abst_list_opt_term t =
  let typ' = abst_list_opt.tr_typ t.typ in
  match t.desc with
  | App({desc=Var x}, [t1; t2]) when Id.name x = "List.nth" ->
      let t1' = abst_list_opt.tr_term t1 in
      let t2' = abst_list_opt.tr_term t2 in
      let t = make_app t1' [t2'] in
      let x = Id.new_var t.typ in
      make_let [x,t] @@ make_get_val @@ make_var x
  | Nil ->
      let el_typ = snd_typ @@ result_typ typ' in
      make_fun (Id.new_var Ty.int) (make_none el_typ)
  | Cons(t1,t2) ->
      let t1' = abst_list_opt.tr_term t1 in
      let t2' = abst_list_opt.tr_term t2 in
      let i = Id.new_var ~name:"i" Ty.int in
      let x = Id.new_var ~name:"x" t1'.typ in
      let xs = Id.new_var ~name:"xs" t2'.typ in
      let t11 = make_eq (make_var i) (make_int 0) in
      let t12 = make_some (make_var x) in
      let t13 = make_app (make_var xs) [make_sub (make_var i) (make_int 1)] in
      if true
      then
        Term.(lets [x,t1'; xs,t2'] (fun_ i (if_ t11 t12 t13)))
      else
        let cns = Id.new_var ~name:"cons" (TFun(x,TFun(xs,t2'.typ))) in
        Term.(let_ [cns, funs [x;xs;i] (if_ t11 t12 t13)] (var cns @ [t1'; t2']))
  | Match(t1,pats) ->
      let x = Id.new_var ~name:"xs" (abst_list_opt.tr_typ t1.typ) in
      let aux (p,cond,t) t' =
        let bind,cond' = get_match_bind_cond_opt (make_var x) p in
        let add_bind t = List.fold_left (fun t' (x,t) -> make_let [x, t] t') t bind in
        let t_cond =
          if cond = true_term
          then cond
          else add_bind (abst_list_opt.tr_term cond)
        in
        make_if (make_and cond' t_cond) (add_bind (abst_list_opt.tr_term t)) t'
      in
      let t_pats = List.fold_right aux pats (make_bottom typ') in
      make_let [x, abst_list_opt.tr_term t1] t_pats
  | _ -> abst_list_opt.tr_term_rec t

let () = abst_list_opt.tr_typ <- abst_list_opt_typ
let () = abst_list_opt.tr_term <- abst_list_opt_term

let trans_opt t =
  let t' = abst_list_opt.tr_term t in
  let t' = Trans.inline_var_const t' in
(*
  let t' = Trans.subst_let_xy t' in
*)
  if false then Format.printf "abst_list::@. @[%a@.@." Print.term t';
  Type_check.check t';
  t', fun _ _ -> raise Not_found



let pr s t = Debug.printf "##[encode_list] %s:@.%a@.@." s Print.term t

let trans_term t =
  let tr =
    if !Flag.Encode.encode_list_opt then
      trans_opt
    else
      trans
  in
  t
  |@> Type_check.check ~ty:t.typ
  |> inst_list_eq
  |@> pr "inst_list_eq"
  |> subst_matched_var
  |@> pr "subst_matched_var"
  |@> Type_check.check ~ty:t.typ
  |*> Trans.remove_top_por
  |*@> pr "remove_top_por"
  |*@> Type_check.check ~ty:t.typ
  |> tr
  |@> (fun t -> Type_check.check t ~ty:t.typ) -| fst
  |@> pr "trans" -| fst
(*
  |> Pair.map_fst Trans.inst_randval
  |@> pr "inst_randval" -| fst
 *)
  |> Pair.map_fst Trans.eta_tuple
  |@> pr "eta_tuple" -| fst
  |*> Pair.map_fst Trans.simplify_if_cond
  |*@> pr "simplify_if" -| fst
  |*@> (fun t -> Type_check.check t ~ty:t.typ) -| fst

let trans_var x =
  if !Flag.Encode.encode_list_opt then
    abst_list_opt.tr_var x
  else
    abst_list.tr2_var "" x

let trans_typ typ =
  if !Flag.Encode.encode_list_opt then
    abst_list_opt.tr_typ typ
  else
    abst_list.tr2_typ "" typ

let rec trans_rty ty =
  let open Ref_type in
  match ty with
  | Base(base,x,t) -> Base(base, x, fst @@ trans_term t)
  | ADT(_,_,_) -> assert false
  | Fun(x,ty1,ty2) -> Fun(trans_var x, trans_rty ty1, trans_rty ty2)
  | Tuple xtys -> Tuple (List.map (Pair.map trans_var trans_rty) xtys)
  | Inter(sty,tys) -> Inter(trans_typ sty, List.map trans_rty tys)
  | Union(sty,tys) -> Union(trans_typ sty, List.map trans_rty tys)
  | ExtArg(x,ty1,ty2) -> ExtArg(trans_var x, trans_rty ty1, trans_rty ty2)
  | List(x,p_len,y,p_i,ty2) ->
      if !Flag.Encode.encode_list_opt then
        unsupported "encode_list_opt"
      else
        let p_len',_ = trans_term p_len in
        let p_i',_ = trans_term p_i in
        let ty2' = trans_rty ty2 in
        let ty_f = Fun(y, Base(TInt,y,p_i'), ty2') in
        let f = Id.new_var @@ to_simple ty_f in
        Tuple [x,Base(TInt,x,p_len'); f, ty_f]
  | Exn(ty1,ty2) -> Exn(trans_rty ty1, trans_rty ty2)

let trans_env env =
  List.map (Pair.map_snd trans_rty) env

let trans = Problem.map_on Focus.fst ~tr_env:trans_env trans_term
