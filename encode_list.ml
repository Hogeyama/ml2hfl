open Util
open Syntax
open Term_util
open Type
open Type_decl


module RT = Ref_type


let debug () = List.mem "Encode_list" !Flag.debug_module



let rec is_filled_pattern p =
  try
    match p.pat_desc with
    | PAny -> None
    | PVar x -> Some (make_var x)
    | PAlias(p1,x) ->
        Some (Option.default (make_var x) @@ is_filled_pattern p1)
    | PConst c -> Some c
    | PConstruct(c, ps) ->
        let ts = List.map is_filled_pattern ps in
        Some (make_construct c @@ List.map Option.get ts)
    | PNil -> Some (make_nil @@ list_typ p.pat_typ)
    | PCons(p1,p2) ->
        let t1 = Option.get @@ is_filled_pattern p1 in
        let t2 = Option.get @@ is_filled_pattern p2 in
        Some (make_cons t1 t2)
    | PTuple ps ->
        let ts = List.map is_filled_pattern ps in
        Some (make_tuple @@ List.map Option.get ts)
    | PRecord fields ->
        let sftyps =
          match Type_decl.kind_of_field (fst @@ List.hd fields) with
          | _, TKRecord sftyps -> sftyps
          | _ -> assert false
        in
        assert (List.length fields = List.length sftyps);
        Some (make_record @@ List.map (Pair.map_snd @@ Option.get -| is_filled_pattern) fields)
    | PNone -> Some (make_none @@ option_typ p.pat_typ)
    | PSome p1 -> Some (make_some @@ Option.get @@ is_filled_pattern p1)
    | POr _ -> None
  with Option.No_value -> None


let subst_matched_var = make_trans ()

let subst_matched_var_desc desc =
  match desc with
  | Match({desc=Var x}, pats) ->
      let aux (p,t1,t2) =
        let t1' = subst_matched_var.tr_term t1 in
        let t2' = subst_matched_var.tr_term t2 in
        let sbst =
          match is_filled_pattern p with
          | None -> Std.identity
          | Some t' ->
              fun t0 ->
                match t0 with
                | {desc=Const True} -> t0
                | _ when !Flag.tupling -> make_label (InfoIdTerm(x, t')) t0
                | _ -> subst x t' t0
        in
        p, sbst t1', sbst t2'
      in
      Match(make_var x, List.map aux pats)
  | _ -> subst_matched_var.tr_desc_rec desc

let () = subst_matched_var.tr_desc <- subst_matched_var_desc
let subst_matched_var = subst_matched_var.tr_term



let rec get_rtyp_list rtyp typ =
  match rtyp, elim_tpred typ with
  | RT.Inter(_, rtyps), _ ->
     RT.Inter(typ, List.map (get_rtyp_list -$- typ) rtyps)
  | RT.Union(_, rtyps), _ ->
      RT.Union(typ, List.map (get_rtyp_list -$- typ) rtyps)
  | RT.Tuple[x, RT.Base(RT.Int, x', p_len); _, RT.Fun(y, RT.Base(RT.Int, y', p_i), typ2)], TList typ ->
      let p_len' = subst_var x' x p_len in
      let p_i' = subst_var y' y p_i in
      RT.List(x, p_len', y, p_i', get_rtyp_list typ2 typ)
  | RT.Tuple[x, RT.Base(RT.Int, x', p_len); _, RT.Inter(_, [])], TList typ ->
      let p_len' = subst_var x' x p_len in
      RT.List(x, p_len', Id.new_var typ_unknown, true_term, RT.Inter(typ, []))
  | RT.Tuple[x, RT.Base(RT.Int, x', p_len); _, RT.Inter(_, typs)], TList typ ->
      let typs' = List.map (fun typ -> RT.Tuple [x, RT.Base(RT.Int, x', p_len); Id.new_var typ_unknown, typ]) typs in
      get_rtyp_list (RT.Inter(typ_unknown, typs')) (TList typ)
  | _, TList typ ->
      Format.printf "%a@." RT.print rtyp;
      raise (Fatal "not implemented get_rtyp_list")
  | RT.Base(b,x,ps), _ -> RT.Base(b,x,ps)
  | RT.Fun(x,rtyp1,rtyp2), TFun(y,typ2) ->
      let rtyp1' = get_rtyp_list rtyp1 (Id.typ y) in
      let rtyp2' = get_rtyp_list rtyp2 typ2 in
      let rtyp2'' =
        match rtyp1' with
        | RT.List _ -> RT.replace_term (make_fst @@ make_var x) (make_app (make_var length_var) [make_var x]) rtyp2'
        | _ -> rtyp2'
      in
      RT.Fun(x, rtyp1', rtyp2'')
  | RT.Tuple xrtyps, TTuple ys ->
      RT.Tuple (List.map2 (fun (x,rtyp) y -> x, get_rtyp_list rtyp (Id.typ y)) xrtyps ys)
  | RT.ExtArg(x,rtyp1,rtyp2), _ ->
      RT.ExtArg(x, rtyp1, get_rtyp_list rtyp2 typ)
  | _ ->
      Format.printf "rtyp:%a@.typ:%a@." RT.print rtyp Print.typ typ;
      assert false

let make_get_rtyp_list_of typed get_rtyp f =
  let typ = Trans.assoc_typ f typed in
  let rtyp = get_rtyp f in
  let rtyp' = get_rtyp_list rtyp typ in
  if !!Flag.print_ref_typ_debug
  then Format.printf "LIST: %a: @[@[%a@]@ ==>@ @[%a@]@]@." Id.print f RT.print rtyp RT.print rtyp';
  rtyp'


let make_tl n t =
  let x = Id.new_var TInt in
  let t1 = make_sub (make_fst t) (make_int n) in
  let t2 = make_fun x (make_app (make_snd t) [make_add (make_var x) (make_int n)]) in
  make_pair t1 t2



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
  | TVar{contents=None} -> raise (Fatal "Polymorphic types occur! (Abstract.abst_list_typ)")
  | TList typ ->
      let l = Id.new_var ~name:"l" TInt in
      TTuple[l; Id.new_var @@ TFun(Id.new_var  ~name:"i" TInt, abst_list.tr2_typ post typ)]
  | _ -> abst_list.tr2_typ_rec post typ

let rec get_match_bind_cond t p =
  match p.pat_desc with
  | PAny -> [], true_term
  | PVar x -> [abst_list.tr2_var "" x, t], true_term
  | PAlias(p,x) ->
      let bind,cond = get_match_bind_cond t p in
      (abst_list.tr2_var "" x, t)::bind, cond
  | PConst {desc=Const Unit} -> [], true_term
  | PConst t' -> [], make_eq t t'
  | PNil -> [], make_eq (make_fst t) (make_int 0)
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
            let bind',cond' = get_match_bind_cond (make_app (make_snd t) [make_int i]) p in
            aux (bind'@@@bind) (make_and cond cond') (i+1) ps
      in
      let len = List.length ps in
      let bind, cond = get_match_bind_cond (make_tl len t) p' in
      aux bind (make_and (make_leq (make_int len) (make_fst t)) cond) 0 ps
  | PTuple ps ->
      let binds,conds = List.split @@ List.mapi (fun i p -> get_match_bind_cond (make_proj i t) p) ps in
      List.rev_flatten binds,
      List.fold_left make_and true_term conds
  | PRecord fields -> assert false
  | _ -> Format.printf "get_match_bind_cond: %a@." Print.pattern p; assert false

let print_bind fm bind =
  Format.fprintf fm "@[[";
  List.iter (fun (x,t) -> Format.fprintf fm "%a := %a;@ " Id.print x Print.term t) bind;
  Format.fprintf fm "]@]"

let rec make_rand typ =
  match typ with
  | TList typ' ->
      let l = Id.new_var ~name:"l" TInt in
      make_let [l, [], randint_unit_term] @@
        make_assume
          (make_leq (make_int 0) (make_var l))
          (make_pair (make_var l) @@ make_fun (Id.new_var TInt) @@ make_rand typ')
  | _ -> make_randvalue_unit typ

let abst_list_term post t =
  match t.desc with
  | App({desc=Const(RandValue(TInt,false)); attr}, t2) when List.mem AAbst_under attr -> (* for disproving termination  *)
      assert (t2 = [unit_term]);
      t
  | App({desc=Const(RandValue(typ,false))}, t2) ->
      assert (t2 = [unit_term]);
      make_rand typ
  | App({desc=Var x}, [t1; t2]) when Id.name x = "List.nth" ->
      let t1' = abst_list.tr2_term post t1 in
      let t2' = abst_list.tr2_term post t2 in
      make_app (make_snd t1') [t2']
  | App({desc=Var x}, [t]) when x = length_var -> make_fst @@ abst_list.tr2_term post t
  | Let(flag, bindings, t2) ->
      let aux (f,xs,t) =
        let post' = "_" ^ Id.name f in
        abst_list.tr2_var post f, List.map (abst_list.tr2_var post) xs, abst_list.tr2_term post' t
      in
      let bindings' = List.map aux bindings in
      make_let_f flag bindings' (abst_list.tr2_term post t2)
  | Nil ->
      let typ'' = abst_list.tr2_typ post @@ list_typ t.typ in
      make_pair (make_int 0) (make_fun (Id.new_var TInt) (make_bottom typ''))
  | Cons _ when is_literal t ->
      let typ'' = abst_list.tr2_typ post @@ list_typ t.typ in
      let ts = decomp_literal t in
      let ts' = List.map (abst_list.tr2_term post) ts in
      let xs = List.map new_var_of_term ts' in
      let bindings = List.rev_map2 (fun x t -> x, [], t) xs ts' in
      let x = Id.new_var ~name:"i" TInt in
      let aux y (i,t) =
        i-1, make_if (make_eq (make_var x) @@ make_int i) (make_var y) t
      in
      let n = List.length ts in
      let _,t = List.fold_right aux xs (n-1, make_bottom typ'') in
      make_lets bindings @@ make_pair (make_int n) (make_fun x t)
  | Cons(t1,t2) ->
      let t1' = abst_list.tr2_term post t1 in
      let t2' = abst_list.tr2_term post t2 in
      let i = Id.new_var ~name:"i" TInt in
      let x = Id.new_var ~name:"x" t1'.typ in
      let xs = Id.new_var ~name:"xs" t2'.typ in
      let t11 = make_eq (make_var i) (make_int 0) in
      let t12 = make_var x in
      let t13 = make_app (make_snd (make_var xs)) [make_sub (make_var i) (make_int 1)] in
      let t_f = make_fun i (make_if t11 t12 t13) in
      let t_len = make_add (make_fst (make_var xs)) (make_int 1) in
      let cons = Id.new_var ~name:("cons"^post) (TFun(x,TFun(xs,t2'.typ))) in
      make_let [cons, [x;xs], make_pair t_len t_f] (make_app (make_var cons) [t1'; t2'])
  | Constr("Abst",[]) -> t
  | Constr(s,ts) -> assert false
  | Match(t1,pats) ->
      let x,bindx =
        let x = Id.new_var ~name:"xs" (abst_list.tr2_typ post t1.typ) in
        x, fun t -> make_let [x, [], abst_list.tr2_term post t1] t
      in
      let aux (p,cond,t2) t3 =
        let add_bind bind t = List.fold_left (fun t' (x,t) -> make_let [x, [], t] t') t bind in
        let bind,cond' = get_match_bind_cond (make_var x) p in
        if debug() then Format.printf "@[bind:%a,@ %a@." print_bind bind Print.term cond;
        let t_cond,bind' =
          if cond = true_term
          then cond, bind
          else
            let cond' = Trans.alpha_rename @@ add_bind bind (abst_list.tr2_term post cond) in
            cond', bind
        in
        if debug() then Format.printf "@[bind':%a,@ %a@." print_bind bind' Print.term t_cond;
        let t2' = abst_list.tr2_term post t2 in
        make_if (make_and cond' t_cond) (add_bind bind' t2') t3
      in
      let t_pats = List.fold_right aux pats (make_bottom @@ abst_list.tr2_typ post t.typ) in
      bindx t_pats
  | _ -> abst_list.tr2_term_rec post t

let () = abst_list.tr2_term <- abst_list_term
let () = abst_list.tr2_typ <- abst_list_typ

let trans t =
  Type_check.check t Type.TUnit;
  let t' = abst_list.tr2_term "" t in
  if debug() then Format.printf "abst_list::@. @[%a@.@." Print.term_typ t';
  let t' = Trans.inline_var_const t' in
  if debug() then Format.printf "abst_list::@. @[%a@.@." Print.term_typ t';
  typ_excep := abst_list.tr2_typ "" !typ_excep;
  Type_check.check t' Type.TUnit;
  t', make_get_rtyp_list_of t







let inst_list_eq_flag = ref false

let inst_list_eq = make_trans2 ()

let inst_list_eq_term f t =
  match t.desc with
  | BinOp(Eq, t1, t2) ->
      let t1' = inst_list_eq.tr2_term f t1 in
      let t2' = inst_list_eq.tr2_term f t2 in
      begin
        match t1.typ with
        | TList TInt -> inst_list_eq_flag := true; make_app (make_var f) [t1'; t2']
        | TList _ ->
            Format.printf "%a@." Print.typ t1.typ;
            unsupported "inst_list_eq"
        | _ -> inst_list_eq.tr2_term_rec f t
      end
  | _ -> inst_list_eq.tr2_term_rec f t

let () = inst_list_eq.tr2_term <- inst_list_eq_term
let inst_list_eq t =
  let f = Id.new_var ~name:"list_eq" @@ TFun(Id.new_var ~name:"xs" @@ TList TInt, TFun(Id.new_var ~name:"xs" @@ TList TInt, TBool)) in
  let xs = Id.new_var ~name:"xs'" (TList TInt) in
  let ys = Id.new_var ~name:"ys'" (TList TInt) in
  let p1 = make_ppair (make_pnil TInt) (make_pnil TInt) in
  let t1 = true_term in
  let x = Id.new_var ~name:"x" TInt in
  let xs' = Id.new_var ~name:"xs'" (TList TInt) in
  let y = Id.new_var ~name:"y" TInt in
  let ys' = Id.new_var ~name:"ys'" (TList TInt) in
  let p2 = make_ppair (make_pcons (make_pvar x) (make_pvar xs')) (make_pcons (make_pvar y) (make_pvar ys')) in
  let t2 = make_and (make_eq (make_var x) (make_var y)) (make_app (make_var f) [make_var xs'; make_var ys']) in
  let p3 = make_ppair (make_pany (TList TInt)) (make_pany (TList TInt)) in
  let t3 = false_term in
  let t_eq = make_match (make_pair (make_var xs) (make_var ys)) [p1,true_term,t1; p2,true_term,t2; p3,true_term,t3] in
  inst_list_eq_flag := false;
  let r = make_letrec [f,[xs;ys],t_eq] @@ inst_list_eq.tr2_term f t in
  if !inst_list_eq_flag then r else t
















let rec get_rtyp_list_opt rtyp typ = raise (Fatal "not implemented get_rtyp_list_opt")

let get_rtyp_list_of typed f rtyp =
  let typ = Trans.assoc_typ f typed in
  let rtyp' = get_rtyp_list_opt rtyp typ in
  if !!Flag.print_ref_typ_debug
  then Format.printf "LIST: %a: @[@[%a@]@ ==>@ @[%a@]@]@." Id.print f RT.print rtyp RT.print rtyp';
  rtyp'


let make_tl_opt n t =
  let x = Id.new_var ~name:"x" TInt in
  make_fun x (make_app t [make_add (make_var x) (make_int n)])


let abst_list_opt = make_trans ()

let abst_list_opt_typ typ =
  match typ with
    TVar{contents=None} -> raise (Fatal "Polymorphic types occur! (Abstract.abst_list_opt_typ)")
  | TList typ -> TFun(Id.new_var ~name:"i" TInt, opt_typ @@ abst_list_opt.tr_typ typ)
  | _ -> abst_list_opt.tr_typ_rec typ

let rec get_match_bind_cond t p =
  match p.pat_desc with
  | PAny -> [], true_term
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
  | PTuple ps ->
      let binds,conds = List.split @@ List.mapi (fun i p -> get_match_bind_cond (make_proj i t) p) ps in
      List.rev_flatten binds,
      List.fold_left make_and true_term conds
  | _ -> Format.printf "get_match_bind_cond: %a@." Print.pattern p; assert false

let abst_list_opt_term t =
  let typ' = abst_list_opt.tr_typ t.typ in
  match t.desc with
  | App({desc=Var x}, [t1; t2]) when Id.name x = "List.nth" ->
      let t1' = abst_list_opt.tr_term t1 in
      let t2' = abst_list_opt.tr_term t2 in
      let t = make_app t1' [t2'] in
      let x = Id.new_var t.typ in
      make_let [x,[],t] @@ make_get_val @@ make_var x
  | Nil ->
      let el_typ = snd_typ @@ result_typ typ' in
      make_fun (Id.new_var TInt) (make_none el_typ)
  | Cons(t1,t2) ->
      let t1' = abst_list_opt.tr_term t1 in
      let t2' = abst_list_opt.tr_term t2 in
      let i = Id.new_var ~name:"i" TInt in
      let x = Id.new_var ~name:"x" t1'.typ in
      let xs = Id.new_var ~name:"xs" t2'.typ in
      let t11 = make_eq (make_var i) (make_int 0) in
      let t12 = make_some (make_var x) in
      let t13 = make_app (make_var xs) [make_sub (make_var i) (make_int 1)] in
      if true
      then
        make_lets [x,[],t1'; xs,[],t2'] @@ make_fun i (make_if t11 t12 t13)
      else
        let cons = Id.new_var ~name:"cons" (TFun(x,TFun(xs,t2'.typ))) in
        make_let [cons, [x;xs], make_fun i (make_if t11 t12 t13)] (make_app (make_var cons) [t1'; t2'])
  | Match(t1,pats) ->
      let x = Id.new_var ~name:"xs" (abst_list_opt.tr_typ t1.typ) in
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
  let t' = Trans.inline_var_const t' in
(*
  let t' = Trans.subst_let_xy t' in
*)
  if false then Format.printf "abst_list::@. @[%a@.@." Print.term t';
  typ_excep := abst_list_opt.tr_typ !typ_excep;
  Type_check.check t' Type.TUnit;
  t', fun _ _ -> raise Not_found



let pr s t = if debug () then Format.printf "##[encode_list] %s:@.%a@.@." s Print.term t

let trans t =
  t
  |> inst_list_eq
  |@> pr "inst_list_eq"
  |> subst_matched_var
  |@> pr "subst_matched_var"
  |@> Type_check.check -$- TUnit
  |> Trans.remove_top_por
  |@> pr "remove_top_por"
  |@> Type_check.check -$- TUnit
  |> if !Flag.encode_list_opt
     then trans_opt
     else trans
