open Util
open Syntax
open Term_util
open Type


let debug () = List.mem "Trans" !Flag.debug_module


let flatten_tvar = (make_trans ()).tr_term



let alpha_rename = make_trans ()

let alpha_rename_term t =
  match t.desc with
  | Let(flag, bindings, t) ->
      let bindings' =
        let aux (f,xs,t) =
          let f' = Id.new_var_id f in
          let xs' = List.map Id.new_var_id xs in
          let t' = alpha_rename.tr_term t in
          let t'' = subst_var f f' t' in
          let t''' = List.fold_right2 subst_var xs xs' t'' in
          (f', xs', t''')
        in
        List.map aux bindings
      in
      let sbst t = List.fold_left2 (fun t' (f,_,_) (f',_,_) -> subst_var f f' t') t bindings bindings' in
      let bindings'' = List.map (Triple.map_trd sbst) bindings' in
      let t' = sbst @@ alpha_rename.tr_term t in
      make_let_f flag bindings'' t'
  | Fun(x, t) ->
      let x' = Id.new_var_id x in
      make_fun x' @@ subst_var x x' @@ alpha_rename.tr_term t
  | _ -> alpha_rename.tr_term_rec t

let () = alpha_rename.tr_term <- alpha_rename_term
let alpha_rename = alpha_rename.tr_term


let inst_tvar_tunit = make_trans ()

let inst_tvar_tunit_typ typ =
  match typ with
  | TVar({contents=None} as r) -> r := Some TUnit; TUnit
  | _ -> inst_tvar_tunit.tr_typ_rec typ

let () = inst_tvar_tunit.tr_typ <- inst_tvar_tunit_typ
let inst_tvar_tunit = inst_tvar_tunit.tr_term




let rename_tvar = make_trans2 ()

let rename_tvar_typ map typ =
  match typ with
  | TVar({contents=None} as x) when List.mem_assq x map -> TVar (List.assq x map)
  | _ -> rename_tvar.tr2_typ_rec map typ

let () = rename_tvar.tr2_typ <- rename_tvar_typ






let get_tvars = make_col [] (List.fold_left (fun xs y -> if List.memq y xs then xs else y::xs))
let get_tvars_typ typ =
  match typ with
  | TVar({contents=None} as x) -> [x]
  | TPred(x,_) -> get_tvars.col_var x
  | _ -> get_tvars.col_typ_rec typ

let () = get_tvars.col_typ <- get_tvars_typ
let get_tvars = get_tvars.col_typ




let rename_poly_funs = make_fold_tr ()

let rec rename_poly_funs_list f map ts =
  let aux t ((_,map),ts) =
    let (_,map'),t' = rename_poly_funs.fold_tr_term (f, map) t in
    (f,map'), t'::ts
  in
  List.fold_right aux ts ((f,map),[])

let rename_poly_funs_term (f,map) t =
  match t.desc with
  | Var x when Id.same x f ->
      if is_poly_typ t.typ
      then fatal "Cannot occur? @ Trans.rename_poly_funs"
      else
        let map',x' =
          try
            let _,x' = List.find (fun (_,f') -> Type.can_unify (Id.typ f') (Id.typ x)) map in
            map, x'
          with Not_found ->
            let x' = Id.new_var_id x in
            (x,x')::map, x'
        in
        (f,map'), make_var x'
  | Var x -> (f,map), make_var x
  | Fun(x, t) ->
      let (_,map'),t' = rename_poly_funs.fold_tr_term (f,map) t in
      (f,map'), make_fun x t'
  | App({desc=Var x; typ=typ}, ts) when Id.same x f ->
      let x' = Id.new_var ~name:(Id.name x) typ in
      let (_,map'),ts' = rename_poly_funs_list f map ts in
      let check (_,f') = Type.can_unify (Id.typ f') (Id.typ x') in
      if List.exists check map'
      then
        let _,x'' = List.find check map' in
        unify (Id.typ x') (Id.typ x'');
        (f,map'), make_app (make_var x'') ts'
      else (f,(x,x')::map'), make_app (make_var x') ts'
  | _ -> rename_poly_funs.fold_tr_term_rec (f,map) t

let () = rename_poly_funs.fold_tr_term <- rename_poly_funs_term
let rename_poly_funs f t =
  let (_,map),t' = rename_poly_funs.fold_tr_term (f,[]) t in
  map, t'



let unify_pattern_var = make_col () (fun _ _ -> ())

let unify_pattern_var_term t =
  match t.desc with
  | Match(t, pats) ->
      let aux1 (pat,t1,t2) =
        let aux2 x =
          get_fv t1 @@@ get_fv t2
          |> List.filter (Id.same x)
          |> List.iter (fun y -> unify (Id.typ x) (Id.typ y))
        in
        List.iter aux2 @@ get_vars_pat pat
      in
      List.iter aux1 pats
  | _ -> unify_pattern_var.col_term_rec t

let () = unify_pattern_var.col_term <- unify_pattern_var_term
let unify_pattern_var = unify_pattern_var.col_term








let rec define_randvalue env defs typ =
  if List.mem_assoc typ env
  then env, defs, make_app (make_var @@ List.assoc typ env) [unit_term]
  else
    match typ with
    | TUnit -> env, defs, unit_term
    | TBool -> env, defs, randbool_unit_term
    | TInt -> env, defs, randint_unit_term
    | TVar({contents=None} as r) -> r := Some TUnit; define_randvalue env defs TUnit
    | TVar{contents=Some typ} -> define_randvalue env defs typ
    | TFun(x,typ) ->
        let env',defs',t = define_randvalue env defs typ in
        env', defs', make_fun x t
    | TList (TVar({contents=None} as r)) ->
        r := Some TUnit; define_randvalue env defs typ
    | TList typ' ->
        let u = Id.new_var ~name:"u" TUnit in
        let f = Id.new_var ~name:("make_" ^ to_id_string typ) (TFun(u,typ)) in
        let env' = (typ,f)::env in
        let env'',defs',t_typ' = define_randvalue env' defs typ' in
        let t_typ = make_br (make_nil typ') (make_cons t_typ' (make_app (make_var f) [unit_term])) in
        env'', (f,[u],t_typ)::defs', make_app (make_var f) [unit_term]
    | TTuple xs ->
        let aux x (env,defs,ts) =
          let env',defs',t = define_randvalue env defs @@ Id.typ x in
          env', defs', t::ts
        in
        let env', defs', ts = List.fold_right aux xs (env,defs,[]) in
        env', defs', make_tuple ts
    | TData(s,false) -> env, defs, make_randvalue_unit typ
    | TData(s,true) ->
        let u = Id.new_var ~name:"u" TUnit in
        let f = Id.new_var ~name:("make_" ^ to_id_string typ) (TFun(u,typ)) in
        let env' = (typ,f)::env in
        let env'',defs',t =
          match Type_decl.assoc s with
          | Type_decl.TKVariant stypss ->
              let n = List.length stypss in
              let aux1 (s,typs) (env,defs,itss,i) =
                let aux2 typ (env,defs,ts) =
                  let env', defs',t = define_randvalue env defs typ in
                  env', defs', t::ts
                in
                let env',defs',ts' = List.fold_right aux2 typs (env,defs,[]) in
                env', defs', (i-1,ts')::itss, i-1
              in
              let env'',defs',itss,_ = List.fold_right aux1 stypss (env',defs,[],n) in
              let aux (s,typs) (i,ts) =
                let p = if i < n-1 then make_pconst (make_int i) else make_pany TInt in
                p, true_term, {desc=Constr(s,ts); typ=typ; attr=[]}
              in
              env'', defs', make_match randint_unit_term (List.map2 aux stypss itss)
          | Type_decl.TKRecord sftyps ->
              let aux (field,(flag,typ)) (env,defs,sfts) =
                let env', defs', t = define_randvalue env defs typ in
                env', defs', (field, (flag, t))::sfts
              in
              let env'',defs',sfts = List.fold_right aux sftyps (env',defs,[]) in
              env'', defs', {desc=Record sfts; typ=typ; attr=[]}
        in
        env'', (f,[u],t)::defs', make_app (make_var f) [unit_term]
    | _ -> Format.printf "define_randvalue: %a@." Print.typ typ; assert false




let inst_randval = make_fold_tr ()

let inst_randval_term (env,defs) t =
  match t.desc with
  | App({desc=Const(RandValue(typ,false))}, [t']) when t' = unit_term ->
      let env',defs',t'' = define_randvalue env defs typ in
      (env',defs'), t''
  | Const(RandValue _) -> assert false
  | _ -> inst_randval.fold_tr_term_rec (env,defs) t

let () = inst_randval.fold_tr_term <- inst_randval_term
let inst_randval t =
  let (_,defs),t' = inst_randval.fold_tr_term ([],[]) t in
  make_letrec defs t'







(** [let f ... = fun x -> t] や [let f ... = let g x = t in g] を [let f ... x = t] に *)
let merge_let_fun = make_trans ()

let merge_let_fun_desc desc =
  match desc with
  | Let(flag, bindings, t2) ->
      let aux (f,xs,t) =
        let ys,t' = decomp_funs t in
        f, xs@ys, merge_let_fun.tr_term t'
      in
      Let(flag, List.map aux bindings, merge_let_fun.tr_term t2)
  | _ -> merge_let_fun.tr_desc_rec desc

let () = merge_let_fun.tr_desc <- merge_let_fun_desc
let merge_let_fun = merge_let_fun.tr_term



let canonize = make_trans ()

let canonize_desc desc =
  match desc with
  | BinOp(Eq, {desc=Not t1}, t2)
  | BinOp(Eq, t1, {desc=Not t2}) ->
      let t1' = canonize.tr_term t1 in
      let t2' = canonize.tr_term t2 in
      let t1 = make_or t1 t2 in
      let t2 = make_or (make_not t1') (make_not t2') in
      BinOp(And, t1, t2)
  | BinOp(Eq, {desc=BinOp((Eq|Lt|Gt|Leq|Geq|And|Or) as bop, t1, t2)}, t3) ->
      let t1' = canonize.tr_term t1 in
      let t2' = canonize.tr_term t2 in
      let t3' = canonize.tr_term t3 in
      let t12 = {desc=BinOp(bop, t1',t2');typ=TBool; attr=[]} in
      let t1 = make_or (make_not t3') t12 in
      let t2 = make_or t3' (make_not t12) in
      BinOp(And, t1, t2)
  | BinOp(Eq, t3, {desc=BinOp((Eq|Lt|Gt|Leq|Geq|And|Or) as bop, t1, t2)}) ->
      let t1' = canonize.tr_term t1 in
      let t2' = canonize.tr_term t2 in
      let t3' = canonize.tr_term t3 in
      let t12 = {desc=BinOp(bop, t1', t2');typ=TBool; attr=[]} in
      let t1 = make_or (make_not t3') t12 in
      let t2 = make_or t3' (make_not t12) in
      BinOp(And, t1, t2)
  | _ -> canonize.tr_desc_rec desc

let () = canonize.tr_desc <- canonize_desc
let canonize = canonize.tr_term







let part_eval t =
  let is_apply xs = function
    | Var x -> xs = [x]
    | App(t, ts) ->
        let rec aux xs ts =
          match xs,ts with
          | [], [] -> true
          | x::xs', {desc=Var y}::ts' when Id.same x y -> aux xs' ts'
          | _ -> false
        in
        aux xs (t::ts)
    | _ -> false
  in
  let is_alias xs = function
    | Var x ->
        if xs = []
        then Some x
        else None
    | App({desc=Var f}, ts) ->
        let rec aux xs ts =
          match xs,ts with
            [], [] -> true
          | x::xs',{desc=Var y}::ts' when Id.same x y -> aux xs' ts'
          | _ -> false
        in
        if aux xs ts
        then Some f
        else None
    | _ -> None
  in
  let rec aux apply t =
    let desc =
      match t.desc with
      | Const c -> Const c
      | Var x ->
          begin
            try
              let xs, t1 = List.assoc x apply in
              Let(Nonrecursive, [x, xs, t1], make_var x)
            with Not_found -> Var x
          end
      | Fun(x, t) -> Fun(x, aux apply t)
      | App({desc=Var f}, ts) ->
          if List.mem_assoc f apply
          then
            match ts with
            | [] ->
                let xs, t1 = List.assoc f apply in
                Let(Nonrecursive, [f, xs, t1], (make_var f))
            | [t] -> t.desc
            | t::ts' -> App(t, ts')
          else
            let ts' = List.map (aux apply) ts in
            App(make_var f, ts')
      | App({desc=Fun(x,t);typ=typ'}, ts) ->
          if is_apply [x] t.desc
          then
            match ts with
            | [] -> Fun(x,t)
            | [t] -> t.desc
            | t::ts' -> App(t, ts')
          else
            begin
              match ts with
              | [{desc=Const(True|False)}] -> (aux apply (subst x (List.hd ts) t)).desc
              | _ ->
                  let t' = aux apply t in
                  let ts' = List.map (aux apply) ts in
                  App({desc=Fun(x,t');typ=typ'; attr=[]}, ts')
            end
      | App(t, ts) -> App(aux apply t, List.map (aux apply) ts)
      | If({desc=Const True}, t2, _) -> (aux apply t2).desc
      | If({desc=Const False}, _, t3) -> (aux apply t3).desc
      | If({desc=Not t1}, t2, t3) -> If(aux apply t1, aux apply t3, aux apply t2)
      | If(t1, t2, t3) ->
          if t2 = t3
          then t2.desc
          else If(aux apply t1, aux apply t2, aux apply t3)
      | Let(flag, [f, xs, t1], t2) ->
          if is_apply xs t1.desc
          then (aux ((f,(xs,t1))::apply) (aux apply t2)).desc
          else
            begin
              match flag, is_alias xs t1.desc  with
              | Nonrecursive, None -> Let(flag, [f, xs, aux apply t1], aux apply t2)
              | Nonrecursive, Some x -> (subst_var f x @@ aux apply t2).desc
              | Recursive, Some x when not @@ List.mem f @@ get_fv t1 ->
                  (subst_var f x @@ aux apply t2).desc
              | Recursive, _ -> Let(flag, [f, xs, aux apply t1], aux apply t2)
            end
      | Let _ -> assert false
      | BinOp(op, t1, t2) -> BinOp(op, aux apply t1, aux apply t2)
      | Not t -> Not (aux apply t)
      | Event(s,b) -> Event(s,b)
      | Record fields -> Record (List.map (fun (s,(f,t)) -> s,(f,aux apply t)) fields)
      | Field(i,s,f,t) -> Field(i, s, f, aux apply t)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(aux apply t1, aux apply t2)
      | Constr(c,ts) -> Constr(c, List.map (aux apply) ts)
      | Match(t,pats) ->
          let aux' (pat,cond,t) = pat, aux apply cond, aux apply t in
          Match(aux apply t, List.map aux' pats)
      | Proj _ -> assert false
      | Tuple _ -> assert false
      | TryWith (_, _) -> assert false
      | Raise _ -> assert false
      | SetField (_, _, _, _, _, _) -> assert false
      | Bottom -> assert false
      | Label _ -> assert false
      | Ref _ -> assert false
      | Deref _ -> assert false
      | SetRef _ -> assert false
      | TNone -> assert false
      | TSome _ -> assert false
    in
    {desc=desc; typ=t.typ; attr=[]}
  in
  aux [] t





let trans_let = make_trans ()

let trans_let_term t =
  match t.desc with
  | Let(Nonrecursive, [f, [], t1], t2) ->
      make_app (make_fun f @@ trans_let.tr_term t2) [trans_let.tr_term t1]
  | Let(Nonrecursive, bindings, t2) when List.exists (fun (_,xs,_) -> xs=[]) bindings -> assert false
  | _ -> trans_let.tr_term_rec t

let () = trans_let.tr_term <- trans_let_term
let trans_let = trans_let.tr_term





let propagate_typ_arg = make_trans ()

let propagate_typ_arg_desc desc =
  match desc with
  | Let(flag, bindings, t2) ->
      let aux (f,xs,t) =
        let xs' =
          let ys = List.take (List.length xs) (get_args @@ Id.typ f) in
          let aux x y ys =
            let ys' = List.map (Id.map_typ @@ subst_type_var y x) ys in
            Id.set_typ x (Id.typ y) :: ys'
          in
          List.fold_right2 aux xs ys []
        in
        let t' = propagate_typ_arg.tr_term t in
        let t'' = List.fold_right2 subst_var xs xs' t' in
        f, xs', t''
      in
      let bindings' = List.map aux bindings in
      let t2' = propagate_typ_arg.tr_term t2 in
      Let(flag, bindings', t2')
  | _ -> propagate_typ_arg.tr_desc_rec desc

let () = propagate_typ_arg.tr_desc <- propagate_typ_arg_desc
let propagate_typ_arg = propagate_typ_arg.tr_term




let replace_typ_var env x =
  Id.set_typ x @@ List.assoc_default (Id.typ x) x env

let replace_typ = make_trans2 ()

let replace_typ_desc env desc =
  match desc with
  | Let(flag, bindings, t2) ->
      let aux (f,xs,t) =
        let f' = replace_typ_var env f in
        if not @@ Type.can_unify (Id.typ f) (Id.typ f')
        then
          begin
            let f'' = Id.set_typ f @@ elim_tpred_all @@ Id.typ f' in
            Format.printf "Prog: %a@.Spec: %a@." Print.id_typ f Print.id_typ f'';
            let msg = Format.sprintf "Type of %s in %s is wrong?" (Id.name f) !Flag.spec_file in
            fatal @@ msg ^ " (please specify monomorphic types if polymorphic types exist)"
          end;
        let xs' =
          let ys = List.take (List.length xs) (get_args @@ Id.typ f') in
          List.map2 (fun x y -> Id.set_typ x @@ Id.typ y) xs ys
        in
        let t' = replace_typ.tr2_term env t in
        let t'' =
          if flag = Nonrecursive
          then t'
          else subst_var f f' t'
        in
        let t''' = List.fold_right2 subst_var xs xs' t'' in
        f', xs', t'''
      in
      let bindings' = List.map aux bindings in
      let t2' = replace_typ.tr2_term env t2 in
      let t2'' = List.fold_left2 (fun t (f,_,_) (f',_,_) -> subst_var f f' t) t2' bindings bindings' in
      Let(flag, bindings', t2'')
  | _ -> replace_typ.tr2_desc_rec env desc

let () = replace_typ.tr2_desc <- replace_typ_desc
let replace_typ env t =
  t
  |> replace_typ.tr2_term env
  |> propagate_typ_arg









let rec eval t =
  let desc =
    match t.desc with
    | Const c -> Const c
    | Var x -> Var x
    | App({desc=Fun(x, t)}, t'::ts) ->
        (eval ({desc=App(subst_map [x, t'] t, ts);typ=t.typ; attr=[]})).desc
    | App(t, []) -> (eval t).desc
    | App(t, ts) ->
        App(eval t, List.map eval ts)
    | If({desc=Const True}, t2, t3) ->
        (eval t2).desc
    | If({desc=Const False}, t2, t3) ->
        (eval t3).desc
    | If(t1, t2, t3) ->
        If(eval t1, eval t2, eval t3)
    | Let _ -> assert false
    (*
    | Let(flag, f, xs, t1, t2) -> (*** assume that evaluation of t1 does not fail ***)
        if flag = Flag.Nonrecursive
        then
          if (*safe t1*)true then
            let t1' = List.fold_right (fun x t -> {desc=Fun(x, t);typ=TFun(x,t.typ)}) xs (eval t1) in
            (eval (subst_map [f, t1'] t2)).desc
          else
            Let(flag, f, xs, eval t1, eval t2)
        else
          (*if not (List.mem f (get_fv t1)) then
            let t1' = List.fold_right (fun x t -> Fun(x, t)) xs (eval t1) in
            eval (subst_map [f, t1'] t2)
            else*)
          Let(flag, f, xs, eval t1, eval t2)
     *)
    | BinOp(Add, {desc=Const (Int 0)}, t) ->
        (eval t).desc
    | BinOp(Mult, {desc=Const (Int 1)}, t) ->
        (eval t).desc
    | BinOp(Sub, t1, t2) ->
        (eval (make_add (eval t1) (eval (make_mul (make_int (-1)) t2)))).desc
    | BinOp(Mult, {desc=Const (Int n)}, {desc=BinOp(Mult, {desc=Const (Int m)}, t)}) ->
        (eval (make_mul (make_int (n*m)) t)).desc
    | BinOp(op, t1, t2) ->
        BinOp(op, eval t1, eval t2)
    | Not t ->
        Not(eval t)
    | Fun(x, {desc=App(t,ts)}) ->
        let t' = eval t in
        let ts' = List.map eval ts in
        let ts'',t_last = List.decomp_snoc ts' in
        if t_last.desc = Var x && not @@ List.mem x @@ get_fv @@ make_app t' ts'' then
          (eval @@ make_app t' ts'').desc
        else
          Fun(x, make_app t' ts')
    | Fun(x,t) ->
        Fun(x, eval t)
    | Event(s,b) -> Event(s,b)
    | _ -> assert false
  in
  {t with desc}






let normalize_binop_exp op t1 t2 =
  let neg xs = List.map (fun (x,n) -> x,-n) xs in
  let rec decomp t =
    match t.desc with
    | Const (Int n) -> [None, n]
    | Var x -> [Some {desc=Var x;typ=Id.typ x; attr=[]}, 1]
    | BinOp(Add, t1, t2) ->
        decomp t1 @@@ decomp t2
    | BinOp(Sub, t1, t2) ->
        decomp t1 @@@ neg (decomp t2)
    | BinOp(Mult, t1, t2) ->
        let xns1 = decomp t1 in
        let xns2 = decomp t2 in
        let reduce xns = List.fold_left (fun acc (_,n) -> acc+n) 0 xns in
        let aux (x,_) = x <> None in
        begin
          match List.exists aux xns1, List.exists aux xns2 with
          | true, true ->
              Format.printf "Nonlinear expression not supported: %a@."
                            Print.term {desc=BinOp(op,t1,t2);typ=TInt; attr=[]};
              assert false
          | false, true ->
              let k = reduce xns1 in
              List.rev_map (fun (x,n) -> x,n*k) xns2
          | true, false ->
              let k = reduce xns2 in
              List.rev_map (fun (x,n) -> x,n*k) xns1
          | false, false ->
              [None, reduce xns1 + reduce xns2]
        end
    | _ -> assert false
  in
  let xns1 = decomp t1 in
  let xns2 = decomp t2 in
  let xns =
    let compare (x1,_) (x2,_) =
      let aux = function
        | None -> "\255"
        | Some {desc=Var x} -> Id.to_string x
        | _ -> assert false
      in
      compare (aux x2) (aux x1)
    in
    List.sort ~cmp:compare (xns1 @@@ (neg xns2))
  in
  let rec aux = function
    | [] -> []
    | (x,n)::xns ->
        let xns1,xns2 = List.partition (fun (y,_) -> x=y) xns in
        let n' = List.fold_left (fun acc (_,n) -> acc+n) 0 ((x,n)::xns1) in
        (x,n') :: aux xns2
  in
  let xns' = aux xns in
  let xns'' = List.filter (fun (x,n) -> n<>0) xns' in
  let x,n = List.hd xns'' in
  let xns = List.rev @@ List.tl xns'' in
  let op',t1',t2' =
    let aux = function
      | None,n -> {desc=Const (Int n); typ=TInt; attr=[]}
      | Some x,n -> if n=1 then x else make_mul (make_int n) x
    in
    let t1,xns',op' =
      if n<0
      then
        let op' =
          match op with
          | Eq -> Eq
          | Lt -> Gt
          | Gt -> Lt
          | Leq -> Geq
          | Geq -> Leq
          | _ -> assert false
        in
        aux (x,-n), xns, op'
      else
        aux (x,n), neg xns, op
    in
    let ts = List.map aux xns' in
    let t2 =
      match ts with
      | [] -> make_int 0
      | t::ts' -> List.fold_left make_add t ts'
    in
    op', t1, t2
  in
  let rec simplify t =
    let desc =
      match t.desc with
      | BinOp(Add, t1, {desc=BinOp(Mult, {desc=Const (Int n)}, t2)}) when n < 0 ->
          let t1' = simplify t1 in
          BinOp(Sub, t1', make_mul (make_int (-n)) t2)
      | BinOp(Add, t1, {desc=Const (Int n)}) when n < 0 ->
          let t1' = simplify t1 in
          BinOp(Sub, t1', make_int (-n))
      | BinOp(Add, t1, t2) ->
          let t1' = simplify t1 in
          BinOp(Add, t1', t2)
      | t -> t
    in
    {desc=desc; typ=t.typ; attr=[]}
  in
  BinOp(op', t1', simplify t2')

let rec normalize_bool_exp t =
  let desc =
    match t.desc with
    | Const True -> Const True
    | Const False -> Const False
    | Var x -> Var x
    | BinOp(Or|And as op, t1, t2) ->
        let t1' = normalize_bool_exp t1 in
        let t2' = normalize_bool_exp t2 in
        BinOp(op, t1', t2')
    | BinOp(Eq, {desc=Const(True|False)}, _)
    | BinOp(Eq, _, {desc=Const(True|False)})
    | BinOp(Eq, {desc=Nil|Cons _}, _)
    | BinOp(Eq, _, {desc=Nil|Cons _}) as t -> t
    | BinOp(Eq|Lt|Gt|Leq|Geq as op, t1, t2) -> normalize_binop_exp op t1 t2
    | Not t -> Not (normalize_bool_exp t)
    | _ -> assert false
  in
  {t with desc}



let rec get_and_list t =
  match t.desc with
  | Const True -> [t]
  | Const False -> [t]
  | Var _ -> [t]
  | BinOp(And, t1, t2) -> get_and_list t1 @@@ get_and_list t2
  | BinOp _ -> [t]
  | Not _ -> [t]
  | _ -> assert false

let rec merge_geq_leq t =
  let desc =
    match t.desc with
    | Const True -> Const True
    | Const False -> Const False
    | Var x -> Var x
    | BinOp(And, t1, t2) ->
        let ts = get_and_list t in
        let is_dual t1 t2 =
          match t1.desc,t2.desc with
          | BinOp(op1,t11,t12), BinOp(op2,t21,t22) when t11=t21 && t12=t22 -> op1=Leq && op2=Geq || op1=Geq && op2=Leq
          | _ -> false
        in
        let get_eq t =
          match t.desc with
          | BinOp((Leq|Geq),t1,t2) -> make_eq t1 t2
          | _ -> assert false
        in
        let rec aux = function
          | [] -> []
          | t::ts ->
              if List.exists (is_dual t) ts
              then
                let t' = get_eq t in
                let ts' = List.filter_out (is_dual t) ts in
                t' :: aux ts'
              else
                t :: aux ts
        in
        let ts' = aux ts in
        let t =
          match ts' with
          | [] -> assert false
          | [t] -> t
          | t::ts -> List.fold_left make_and t ts
        in
        t.desc
    | BinOp(Or, t1, t2) ->
        let t1' = merge_geq_leq t1 in
        let t2' = merge_geq_leq t2 in
        BinOp(Or, t1', t2')
    | BinOp(Eq|Lt|Gt|Leq|Geq as op, t1, t2) -> BinOp(op, t1, t2)
    | Not t -> Not (merge_geq_leq t)
    | _ -> Format.printf "%a@." Print.term t; assert false
  in
  {t with desc=desc}










let elim_fun = make_trans2 ()

let elim_fun_term fun_name t =
  match t.desc with
  | Fun(y, t1) ->
      let f = Id.new_var ~name:fun_name t.typ in
      make_let [f, [y], elim_fun.tr2_term fun_name t1] @@ make_var f
  | Let(flag, bindings, t2) ->
      let aux (f,xs,t) = f, xs, elim_fun.tr2_term ("f_" ^ Id.name f) t in
      let bindings' = List.map aux bindings in
      let t2' = elim_fun.tr2_term fun_name t2 in
      make_let_f flag bindings' t2'
  | _ -> elim_fun.tr2_term_rec fun_name t

let () = elim_fun.tr2_term <- elim_fun_term
let elim_fun t = elim_fun.tr2_term "f" t





let make_ext_env = make_col2 [] (@@@)

let make_ext_env_desc funs desc =
  match desc with
  | Var x when Fpat.RefTypInfer.is_parameter (Id.name x) -> []
  | Var x when Id.mem x funs -> [x, Id.typ x]
  | Var x -> []
  | _ -> make_ext_env.col2_desc_rec funs desc

let () = make_ext_env.col2_desc <- make_ext_env_desc
let make_ext_env t = make_ext_env.col2_term (get_fv t) t



let init_rand_int = make_trans ()

let init_rand_int_term t =
  match t.desc with
  | App({desc=Const(RandInt false)},[{desc=Const Unit}]) -> make_var @@ Id.new_var ~name:"_r" TInt
  | Const(RandInt _) -> assert false
  | _ -> init_rand_int.tr_term_rec t

let () = init_rand_int.tr_term <- init_rand_int_term
let init_rand_int = init_rand_int.tr_term



let rec inlined_f inlined fs t =
  let desc =
    match t.desc with
    | Const c -> Const c
    | Var y ->
        if List.exists (fun (x,_,_) -> Id.same x y) fs then
          let (f, xs, t') = List.find (fun (x,_,_) -> Id.same x y) fs in
          (*let _ = List.iter (fun (x, t) -> Format.printf "%a -> %a@." print_id x pp_print_term t) [f, t'] in*)
          let f, _ =
            List.fold_left
              (fun (f, ty) y ->
               (fun t ->
                f {desc=Fun(y, t); typ=ty; attr=[]}),
                match ty with
                | Type.TFun(_, ty') -> ty'
                | _ -> Format.printf "%a@." Print.typ ty; assert false)
              (Fun.id, t.typ)
              xs
          in
          let t' = inlined_f inlined fs t' in
          (f t').desc
        else
          Var y
    | Fun(y, t1) -> Fun(y, inlined_f inlined fs t1)
    | App(t1, ts) ->
        (*let _ = Format.printf "func: %a@." pp_print_term t1' in*)
        (match t1.desc with
         | Var f when List.exists (fun (f', _, _) -> Id.same f f') fs ->
             let (f, xs, t) = try List.find (fun (f', _, _) -> Id.same f f') fs with Not_found -> assert false in
             let ts = List.map (inlined_f inlined fs) ts in
             let ys = List.map (fun t -> match t.desc with Const (Unit | True | False | Int _) | Var _ -> `L(t) | _ -> `R(Id.new_var ~name:"arg" t.typ)) ts in
             let ys1, ys2 = if List.length ys <= List.length xs then ys, [] else Fpat.Util.List.split_nth (List.length xs) ys in
             let xs1, xs2 = Fpat.Util.List.split_nth (List.length ys1) xs in
             let map = List.map2 (fun x y -> match y with `L(t) -> x, t | `R(y) -> x, make_var y) xs1 ys1 in
             let t' = subst_map map t in
             let f, _ =
               List.fold_left
                 (fun (f, ty) x -> (fun t -> f {desc=Fun(x, t); typ=ty; attr=[]}), match ty with Type.TFun(_, ty') -> ty' | _ -> assert false)
                 ((fun t -> t), Type.app_typ t1.typ (List.map Syntax.typ ts))
                 xs2
             in
             let bindings = Fpat.Util.List.filter_map2 (fun y t -> match y with `L(_) -> None | `R(y) -> Some(y, [], t)) ys ts in
             (make_lets bindings (make_app (f t') (List.map (fun y -> match y with `L(t) -> t | `R(y) -> make_var y) ys2))).desc
         | _ ->
             let t1' = inlined_f inlined fs t1 in
             let ts' = List.map (inlined_f inlined fs) ts in
             App(t1', ts'))
    | If(t1, t2, t3) -> If(inlined_f inlined fs t1, inlined_f inlined fs t2, inlined_f inlined fs t3)
    | Let(flag, bindings, t2) ->
        let aux (f,xs,t) =
          (*let _ = List.iter (fun f -> Format.printf "f: %a@." print_id f) inlined in*)
          let rec lift t =
            match t.desc with
            | Fun(x, t') ->
                let xs, t' = lift t' in
                x::xs, t'
            | _ -> [], t
          in
          if flag = Nonrecursive then
            if List.exists (fun f' -> Id.same f' f) inlined then
              let t' = inlined_f inlined fs t in
              let xs', t' = lift t' in
              (*let _ = Format.printf "inlined: %a, %a, %a@." print_id f (Fpat.Util.List.pr print_id ",") xs pp_print_term t' in*)
              `R(f, xs @ xs', t')
            else if xs = [] && (match t.desc with Proj(_,{desc=Var _}) -> true | _ -> false) then
              (*let _ = Format.printf "fst/snd: %a@." print_id f in*)
              `R(f, xs, t)
            else
              let t' = inlined_f inlined fs t in
              let xs', t' = lift t' in
              `L(f, xs @ xs', t')
          else
            `L(f, xs, inlined_f inlined fs t)
        in
        let bindings', fs' = Fpat.Util.List.partition_map aux bindings in
        let t2' = inlined_f inlined (fs @ fs') t2 in
        if bindings' = [] then
          t2'.desc
        else
          Let(flag, bindings', t2')
    | BinOp(op, t1, t2) -> BinOp(op, inlined_f inlined fs t1, inlined_f inlined fs t2)
    | Not t1 -> Not (inlined_f inlined fs t1)
    | Event(s,b) -> Event(s,b)
    | Record fields ->  Record (List.map (fun (f,(s,t1)) -> f,(s,inlined_f inlined fs t1)) fields)
    | Field(i,s,f,t1) -> Field(i,s,f,inlined_f inlined fs t1)
    | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,inlined_f inlined fs t1,inlined_f inlined fs t2)
    | Nil -> Nil
    | Cons(t1,t2) -> Cons(inlined_f inlined fs t1, inlined_f inlined fs t2)
    | Constr(s,ts) -> Constr(s, List.map (inlined_f inlined fs) ts)
    | Match(t1,pats) ->
        let aux (pat,cond,t1) = pat, inlined_f inlined fs cond, inlined_f inlined fs t1 in
        Match(inlined_f inlined fs t1, List.map aux pats)
    | Raise t -> Raise (inlined_f inlined fs t)
    | TryWith(t1,t2) -> TryWith(inlined_f inlined fs t1, inlined_f inlined fs t2)
    | Tuple ts -> Tuple (List.map (inlined_f inlined fs) ts)
    | Proj(i,t) -> Proj(i, inlined_f inlined fs t)
    | Bottom -> Bottom
    | _ -> Format.printf "inlined_f: %a@." Print.constr t; assert false
  in
  {t with desc}

let inlined_f inlined t = inlined_f inlined [] t |@> Type_check.check -$- TUnit




let lift_fst_snd = make_trans2 ()

let lift_fst_snd_term fs t =
  match t.desc with
  | Fun(y, t1) -> make_fun y @@ lift_fst_snd.tr2_term fs t1(* ommit the case where y is a pair *)
  | Let(flag, bindings, t2) ->
      let bindings' =
        List.map
          (fun (f,xs,t) ->
           f, xs,
           let fs' =
             List.flatten
               (List.filter_map
                  (fun x ->
                   match x.Id.typ with
                   | TTuple [_; _] ->
                       Some([Id.new_var ~name:x.Id.name (fst_typ x.Id.typ), true, x; Id.new_var ~name:x.Id.name (snd_typ x.Id.typ), false, x])
                   | _ -> None)
                  xs)
           in
           if fs' = [] then
             lift_fst_snd.tr2_term fs t
           else
             make_lets
               (List.map
                  (fun (x, bfst, xorig) ->
                   (* ommit the case where x is a pair *)
                   x, [], if bfst then make_fst (make_var xorig) else make_snd (make_var xorig))
                  fs')
               (lift_fst_snd.tr2_term (fs @ fs') t)
          (* ommit the case where f is a pair *))
          bindings
      in
      make_let_f flag bindings' @@ lift_fst_snd.tr2_term fs t2
  | Proj(0, {desc=Var x}) when tuple_num (Id.typ x) = Some 2 ->
      (try
          let (x, _, _) = List.find (fun (_, bfst, x') -> bfst && Id.same x' x) fs in
          make_var x
        with Not_found ->
          make_fst @@ lift_fst_snd.tr2_term fs t)
  | Proj(1, {desc=Var x}) when tuple_num (Id.typ x) = Some 2 ->
      (try
          let (x, _, _) = List.find (fun (_, bfst, x') -> not bfst && Id.same x' x) fs in
          make_var x
        with Not_found ->
          make_snd @@ lift_fst_snd.tr2_term fs t)
  | _ -> lift_fst_snd.tr2_term_rec fs t

let () = lift_fst_snd.tr2_term <- lift_fst_snd_term
let lift_fst_snd t = lift_fst_snd.tr2_term [] t





let simplify_match = make_trans ()

let simplify_match_term t =
  match t.desc with
  | Match(t1,pats) ->
      let aux (pat,cond,t1) = pat, simplify_match.tr_term cond, simplify_match.tr_term t1 in
      let pats' = List.map aux pats in
      let rec elim_unused = function
        | [] -> []
        | (({pat_desc=PAny|PVar _}, cond, t) as pct)::_ when cond = true_term -> [pct]
        | pct::pats -> pct :: elim_unused pats
      in
      let pats'' = elim_unused pats' in
      begin
        match pats'' with
        | [] -> assert false
        | [{pat_desc=PAny}, cond, t] when cond = true_term ->
            make_seq t1 t
        | [{pat_desc=PConst {desc=Const Unit}}, cond, t] when cond = true_term ->
            make_seq t1 t
        | [{pat_desc=PVar x}, cond, t] when cond = true_term ->
            make_let [x, [], t1] t
        | _ -> make_match (simplify_match.tr_term t1) pats''
      end
  | _ -> simplify_match.tr_term_rec t

let () = simplify_match.tr_term <- simplify_match_term
let simplify_match = simplify_match.tr_term



let should_insert typs = List.for_all (function TFun _ -> true | _ -> false) typs

(* Insert extra parameters into functions with only function arguments.
   Input must be CPS *)

let insert_param_funarg = make_trans ()

let insert_param_funarg_typ typ =
  match typ with
  | TFun _ as typ ->
      let xs,typ' = decomp_tfun typ in
      let xs' = List.map insert_param_funarg.tr_var xs in
      let xs'' =
        if should_insert (List.map Id.typ xs)
        then (Id.new_var ~name:"u" TUnit) :: xs'
        else xs'
      in
      List.fold_right (fun x typ -> TFun(x,typ)) xs'' (insert_param_funarg.tr_typ typ')
  | _ -> insert_param_funarg.tr_typ_rec typ

let insert_param_funarg_term t =
  let typ = insert_param_funarg_typ t.typ in
  let desc =
    match t.desc with
    | Fun _ ->
        let xs,t' = decomp_funs t in
        let xs' = List.map insert_param_funarg.tr_var xs in
        let xs'' =
          if should_insert @@ List.map Id.typ xs
          then (Id.new_var ~name:"u" TUnit) :: xs'
          else xs'
        in
        (List.fold_right make_fun xs'' (insert_param_funarg.tr_term t')).desc
    | App(t1, ts) ->
        let ts' = List.map insert_param_funarg.tr_term ts in
        let ts'' =
          if should_insert (get_argtyps t1.typ)
          then unit_term :: ts'
          else ts'
        in
        App(insert_param_funarg.tr_term t1, ts'')
    | Let(flag, defs, t) ->
        let aux (f,xs,t) =
          let xs' = List.map insert_param_funarg.tr_var xs in
          let xs'' =
            if should_insert @@ List.map Id.typ xs
            then Id.new_var ~name:"u" TUnit :: xs'
            else xs'
          in
          insert_param_funarg.tr_var f, xs'', insert_param_funarg.tr_term t
        in
        Let(flag, List.map aux defs, insert_param_funarg.tr_term t)
    | _ -> insert_param_funarg.tr_desc_rec t.desc
  in
  {desc; typ; attr=t.attr}

let () = insert_param_funarg.tr_typ <- insert_param_funarg_typ
let () = insert_param_funarg.tr_term <- insert_param_funarg_term
let insert_param_funarg t = t
  |> insert_param_funarg.tr_term
  |@> Type_check.check -$- Term_util.typ_result







let rec search_fail path t =
  match t.desc with
  | Const c -> []
  | Var x -> []
  | Fun(x,t) -> search_fail path t
  | App(t1, ts) ->
      let rec aux acc i ts =
        match ts with
          [] -> acc
        | t::ts' -> aux (search_fail (i::path) t @ acc) (i+1) ts'
      in
      aux [] 0 (t1::ts)
  | If(t1, t2, t3) -> search_fail (1::path) t1 @ search_fail (2::path) t2 @ search_fail (3::path) t3
  | Let(_, defs, t) ->
      let rec aux acc i ts =
        match ts with
          [] -> acc
        | t::ts' -> aux (search_fail (i::path) t @ acc) (i+1) ts'
      in
      let ts = List.map (fun (_,_,t) -> t) defs in
      aux [] 0 (ts@[t])
  | BinOp(_, t1, t2) -> search_fail (1::path) t1 @ search_fail (2::path) t2
  | Not t -> search_fail path t
  | Event("fail",_) -> [path]
  | Event(s,b) -> []
  | Record fields -> assert false
  | Field(i,s,f,t) -> assert false
  | SetField(n,i,s,f,t1,t2) -> assert false
  | Nil -> []
  | Cons(t1,t2) -> search_fail (1::path) t1 @ search_fail (2::path) t2
  | Constr(_,ts) ->
      let rec aux acc i ts =
        match ts with
          [] -> acc
        | t::ts' -> aux (search_fail (i::path) t @ acc) (i+1) ts'
      in
      aux [] 0 ts
  | Match(t,pats) ->
      let rec aux acc i ts =
        match ts with
          [] -> acc
        | t::ts' -> aux (search_fail (i::path) t @ acc) (i+1) ts'
      in
      let ts = List.rev_flatten_map (fun (_,cond,t) -> [t;cond]) pats in
      aux [] 0 (t::ts)
  | Raise t -> search_fail path t
  | TryWith(t1,t2) -> search_fail (1::path) t1 @ search_fail (2::path) t2
  | Bottom -> []
  | Tuple ts -> List.flatten @@ List.mapi (fun i t -> search_fail (i::path) t) ts
  | Proj(i,t) -> search_fail path t
  | Label(_,t) -> search_fail path t
  | Ref _ -> assert false
  | Deref _ -> assert false
  | SetRef _ -> assert false
  | TNone -> assert false
  | TSome _ -> assert false


let search_fail t = search_fail [] t


let rec screen_fail path target t =
  let desc =
    match t.desc with
    | Const c -> t.desc
    | Var x -> t.desc
    | Fun(x,t) -> t.desc
    | App(t1, ts) ->
        let aux i t = screen_fail (i::path) target t in
        let t1ts' = List.mapi aux (t1::ts) in
        App(List.hd t1ts', List.tl t1ts')
    | If(t1, t2, t3) ->
        let aux i t = screen_fail (i::path) target t in
        If(aux 1 t1, aux 2 t2, aux 3 t3)
    | Let(flag, defs, t) ->
        let aux i t = screen_fail (i::path) target t in
        let aux_def i (f,xs,t) = f, xs, aux i t in
        Let(flag, List.mapi aux_def defs, aux (List.length defs) t)
    | BinOp(op, t1, t2) ->
        let aux i t = screen_fail (i::path) target t in
        BinOp(op, aux 1 t1, aux 2 t2)
    | Not t -> Not (screen_fail path target t)
    | Event("fail",_) ->
        if path = target
        then t.desc
        else Bottom
    | Event(s,b) -> t.desc
    | Record fields -> assert false
    | Field(i,s,f,t) -> assert false
    | SetField(n,i,s,f,t1,t2) -> assert false
    | Nil -> t.desc
    | Cons(t1,t2) ->
        let aux i t = screen_fail (i::path) target t in
        Cons(aux 1 t1, aux 2 t2)
    | Constr(s,ts) ->
        let aux i t = screen_fail (i::path) target t in
        Constr(s, List.mapi aux ts)
    | Match(t,pats) ->
        let aux i t = screen_fail (i::path) target t in
        let aux_pat i (p,cond,t) = p, aux (2*i+1) cond, aux (2*i+2) t in
        Match(aux 0 t, List.mapi aux_pat pats)
    | Raise t -> Raise (screen_fail path target t)
    | TryWith(t1,t2) ->
        let aux i t = screen_fail (i::path) target t in
        TryWith(aux 1 t1, aux 2 t2)
    | Bottom -> t.desc
    | Tuple ts ->
        Tuple (List.mapi (fun i t -> screen_fail (i::path) target t) ts)
    | Proj(i,t) -> Proj(i, screen_fail path target t)
    | Label(info,t) -> Label(info, screen_fail path target t)
    | Ref _ -> assert false
    | Deref _ -> assert false
    | SetRef _ -> assert false
    | TNone -> assert false
    | TSome _ -> assert false
  in
  {t with desc}

let screen_fail target t = screen_fail [] target t




let rename_ext_funs = make_fold_tr ()

let rename_ext_funs_desc (funs,map) desc =
  match desc with
  | Var x when Id.mem x funs ->
      let map',x' =
        try
          map, List.find (fun f' -> Type.can_unify (Id.typ f') (Id.typ x)) map
        with Not_found ->
          let x' = Id.new_var_id x in
          x'::map, x'
      in
      (funs,map'), Var x'
  | _ -> rename_ext_funs.fold_tr_desc_rec (funs,map) desc

let () = rename_ext_funs.fold_tr_desc <- rename_ext_funs_desc
let rename_ext_funs funs t =
  let (_,map),t' = rename_ext_funs.fold_tr_term (funs,[]) t in
  map, t'


let make_ext_fun_def f =
  let xs,typ' = decomp_tfun @@ Id.typ f in
  let xs' = List.map Id.new_var_id xs in
  let make_fun_arg_call f (env,defs,t) =
    let xs,typ = decomp_tfun @@ Id.typ f in
    let aux typ (env,defs,args) =
      let env',defs',arg = define_randvalue env defs typ in
      env', defs', arg::args
    in
    let env',defs',args = List.fold_right aux (List.map Id.typ xs) (env,defs,[]) in
    let t'' =
      if xs = []
      then t
      else make_seq (make_br unit_term (make_ignore @@ make_app (make_var f) args)) t
    in
    env', defs', t''
  in
  let env,defs,t = define_randvalue [] [] typ' in
  let _,defs',t' = List.fold_right make_fun_arg_call xs' (env,defs,t) in
  f, xs', make_letrec defs' t'

let make_ext_funs env t =
  let funs =
    get_fv t
    |> List.filter_out (Fpat.RefTypInfer.is_parameter -| Id.name)
    |*> List.filter (fun x -> Id.id x > 0)
    |> List.filter_out (Id.mem_assoc -$- env)
  in
  if List.exists (is_poly_typ -| Id.typ) funs
  then unsupported "Trans.make_ext_funs";
  let map,t' = rename_ext_funs funs t in
  let defs1 = List.map make_ext_fun_def map in
  let genv,cenv,defs2 =
    let aux (genv,cenv,defs) (f,typ) =
      let genv',cenv',t = Ref_type.generate genv cenv typ in
      let f' = Id.set_typ f @@ Ref_type.to_abst_typ typ in
      genv', cenv', (f',[],t)::defs
    in
    List.fold_left aux ([],[],[]) env
  in
  let defs = List.map snd (genv @ cenv) in
  make_letrecs defs @@ make_lets defs2 @@ make_lets defs1 t'


let assoc_typ = make_col2 [] (@@@)

let assoc_typ_desc f desc =
  match desc with
  | Let(flag, bindings, t1) ->
      let aux (g,_,t) =
        let typs1 = if Id.same f g then [Id.typ g] else [] in
        typs1 @@@ assoc_typ.col2_term f t
      in
      assoc_typ.col2_term f t1 @@@ List.rev_flatten_map aux bindings
  | _ -> assoc_typ.col2_desc_rec f desc

let () = assoc_typ.col2_desc <- assoc_typ_desc

let assoc_typ f t =
  match assoc_typ.col2_term f t with
  | [] -> raise Not_found
  | [typ] -> typ
  | _ -> Format.printf "VAR:%a@.PROG:%a@." Id.print f Print.term t; assert false





let let2fun = make_trans ()

let let2fun_desc desc =
  match desc with
  | Let(flag, bindings, t2) ->
      let aux (f,xs,t) = let2fun.tr_var f, [], List.fold_right make_fun xs (let2fun.tr_term t) in
      let bindings' = List.map aux bindings in
      let t2' = let2fun.tr_term t2 in
      Let(flag, bindings', t2')
  | _ -> let2fun.tr_desc_rec desc

let () = let2fun.tr_desc <- let2fun_desc
let let2fun = let2fun.tr_term



(* This function does not insert new let-expressions. *)
let fun2let = make_trans ()

let fun2let_desc desc =
  match desc with
  | Let(flag, bindings, t2) ->
      let aux (f,xs,t) =
        let ys,t' = decomp_funs t in
        fun2let.tr_var f, xs@ys, fun2let.tr_term t'
      in
      let bindings' = List.map aux bindings in
      let t2' = fun2let.tr_term t2 in
      Let(flag, bindings', t2')
  | _ -> fun2let.tr_desc_rec desc

let () = fun2let.tr_desc <- fun2let_desc
let fun2let = fun2let.tr_term



let inline_no_effect = make_trans ()

let inline_no_effect_desc desc =
  match desc with
  | Let(Nonrecursive, [x,[],t], {desc=Var y}) when x = y ->
      (inline_no_effect.tr_term t).desc
  | Let(Nonrecursive, [x,[],t], t2) when Id.mem x (get_fv t2) && has_no_effect t ->
      let t' = inline_no_effect.tr_term t in
      let t2' = inline_no_effect.tr_term t2 in
      (subst x t' t2').desc
  | Let(flag, bindings, t) ->
      let aux (f,xs,t) =
        inline_no_effect.tr_var f,
        List.map inline_no_effect.tr_var xs,
        inline_no_effect.tr_term t
      in
      let bindings' = List.map aux bindings in
      let t' = inline_no_effect.tr_term t in
      Let(flag, bindings', t')
  | _ -> inline_no_effect.tr_desc_rec desc

let () = inline_no_effect.tr_desc <- inline_no_effect_desc
let inline_no_effect = inline_no_effect.tr_term




let beta_no_effect = make_trans ()

let beta_no_effect_desc desc =
  match desc with
  | App(t1, [t2]) ->
      let t1' = beta_no_effect.tr_term t1 in
      let t2' = beta_no_effect.tr_term t2 in
      begin
        match t1'.desc with
        | Fun(x,t1'') when has_no_effect t2' -> (subst x t2' t1'').desc
        | _ -> App(t1', [t2'])
      end
  | _ -> beta_no_effect.tr_desc_rec desc

let () = beta_no_effect.tr_desc <- beta_no_effect_desc
let beta_no_effect = beta_no_effect.tr_term



let rec diff_terms t1 t2 =
  match t1.desc, t2.desc with
  | Const(RandValue(typ1,b1)), Const(RandValue(typ2,b2)) ->
      if Type.same_shape typ1 typ2 && b1 = b2
      then []
      else [t1,t2]
  | Const c1, Const c2 -> if c1 = c2 then [] else [t1,t2]
  | Var x1, Var x2 -> if Id.same x1 x2 then [] else [t1,t2]
  | Fun _, Fun _ -> [t1,t2]
  | App(t11,[t12]), App(t21,[t22]) -> diff_terms t11 t21 @ diff_terms t12 t22
  | App(t1,ts1), App(t2,ts2) ->
      let ts1',t12 = List.decomp_snoc ts1 in
      let ts2',t22 = List.decomp_snoc ts2 in
      let t1' = {desc=App(make_app t1 ts1', [t12]); typ=t1.typ; attr=[]} in
      let t2' = {desc=App(make_app t2 ts2', [t22]); typ=t2.typ; attr=[]} in
      diff_terms t1' t2'
  | If(t11,t12,t13), If(t21,t22,t23) ->
      diff_terms t11 t21 @ diff_terms t12 t22 @ diff_terms t13 t23
  | Let(flag1,bindings1,t1), Let(flag2,bindings2,t2) -> [t1,t2]
  | BinOp(op1,t11,t12), BinOp(op2,t21,t22) ->
      if op1 = op2
      then diff_terms t11 t21 @ diff_terms t12 t22
      else [t1,t2]
  | Not t1, Not t2 -> diff_terms t1 t2
  | Event(s1,b1), Event(s2,b2) -> if s1 = s2 && b1 = b2 then [] else [t1,t2]
  | Record _, Record _ -> [t1,t2] (* Not implemented *)
  | Field _, Field _ -> [t1,t2] (* Not implemented *)
  | SetField _, SetField _ -> [t1,t2] (* Not implemented *)
  | Nil, Nil -> []
  | Cons(t11,t12), Cons(t21,t22) ->
      diff_terms t11 t21 @ diff_terms t12 t22
  | Constr _, Constr _ -> [t1,t2] (* Not implemented *)
  | Match _, Match _ -> [t1,t2] (* Not implemented *)
  | Raise _, Raise _ -> [t1,t2] (* Not implemented *)
  | TryWith _, TryWith _ -> [t1,t2] (* Not implemented *)
  | Tuple ts1, Tuple ts2 ->
      List.flatten @@ List.map2 diff_terms ts1 ts2
  | Proj(i,t1), Proj(j,t2) when i = j -> diff_terms t1 t2
  | Bottom, Bottom -> []
  | Label _, Label _ -> [t1,t2]
  | _ -> [t1, t2]




let subst_let_xy = make_trans ()

let subst_let_xy_desc desc =
  let desc' = subst_let_xy.tr_desc_rec desc in
  match desc with
  | Let(Nonrecursive, bindings, t) ->
      let bindings',t' =
        match desc' with
        | Let(Nonrecursive, bindings', t') -> bindings', t'
        | _ -> assert false
      in
      let sbst bind t =
        match bind with
        | (x, [], ({desc=Var y} as t')) -> subst x t' t
        | _ -> raise Not_found
      in
      let check bind =
        try
          ignore (sbst bind unit_term);
          true
        with Not_found -> false
      in
      let bindings1,bindings2 = List.partition check bindings' in
      (make_let bindings2 @@ List.fold_right sbst bindings1 t').desc
  | _ -> desc'

let () = subst_let_xy.tr_desc <- subst_let_xy_desc
let subst_let_xy = subst_let_xy.tr_term






let flatten_let = make_trans ()

let flatten_let_term t =
  match t.desc with
  | Let(Nonrecursive, [x,[],t1], t2) ->
      let t1' = flatten_let.tr_term t1 in
      let t2' = flatten_let.tr_term t2 in
      begin match t1'.desc with
      | Let _ ->
          let fbindings,t12 = decomp_lets t1' in
          let fbindings' = fbindings@[Nonrecursive,[x,[],t12]] in
          List.fold_right (Fun.uncurry make_let_f) fbindings' t2'
      | _ ->
          make_let [x,[],t1'] t2'
      end
  | _ -> flatten_let.tr_term_rec t

let () = flatten_let.tr_term <- flatten_let_term
let flatten_let = flatten_let.tr_term


let normalize_let = make_trans ()

let normalize_let_aux t =
  let post t' =
    match t'.desc with
    | BinOp _ | App _ | Tuple _ | Proj _ ->
        let y = new_var_of_term t' in
        make_lets [y,[],t'] @@ make_var y
    | _ -> t'
  in
  match t.desc with
  | Var x -> x, post
  | _ ->
     let x = new_var_of_term t in
     let t' = normalize_let.tr_term t in
     let post' t'' = make_let [x,[],t'] @@ post t'' in
     x, post'

let normalize_let_term t =
  match t.desc with
  | BinOp(op,t1,t2) ->
      let x1,post1 = normalize_let_aux t1 in
      let x2,post2 = normalize_let_aux t2 in
      post1 @@ post2 @@ {desc=BinOp(op, make_var x1, make_var x2); typ=t.typ; attr=[]}
  | App(t, ts) ->
     let ts' = List.map normalize_let.tr_term ts in
     let x,post = normalize_let_aux t in
     post @@ make_app (make_var x) ts'
  | Tuple ts ->
      let xs,posts = List.split_map normalize_let_aux ts in
      List.fold_right (@@) posts @@ make_tuple @@ List.map make_var xs
  | Proj(i,t) ->
     let x,post = normalize_let_aux t in
     post @@ make_proj i @@ make_var x
  | Let(flag,bindings,t1) ->
      let aux (f,xs,t) = f, xs, normalize_let.tr_term t in
      let bindings' = List.map aux bindings in
      let t1' = normalize_let.tr_term t1 in
      make_let_f flag bindings' t1'
  | _ -> normalize_let.tr_term_rec t

let () = normalize_let.tr_term <- normalize_let_term
let normalize_let = normalize_let.tr_term



let inline_var = make_trans ()

let inline_var_term t =
  match t.desc with
  | Let(Nonrecursive, [x,[],({desc=Var _} as t1)], t2) ->
      subst x t1 @@ inline_var.tr_term t2
  | _ -> inline_var.tr_term_rec t

let () = inline_var.tr_term <- inline_var_term
let inline_var = inline_var.tr_term


let inline_var_const = make_trans ()

let inline_var_const_term t =
  match t.desc with
  | Let(Nonrecursive, [x,[],({desc=Var _|Const _} as t1)], t2) ->
      subst x t1 @@ inline_var_const.tr_term t2
  | _ -> inline_var_const.tr_term_rec t

let () = inline_var_const.tr_term <- inline_var_const_term
let inline_var_const = inline_var_const.tr_term




let remove_label = make_trans2 ()

let remove_label_term label t =
  match label, t.desc with
  | None, Label(_, t1) -> remove_label.tr2_term label t1
  | Some l, Label(InfoString l', {desc=Label(_, t1)}) when l = l' -> remove_label.tr2_term label t1
  | Some l, Label(InfoString l', t1) when l = l' -> remove_label.tr2_term label t1
  | _ -> remove_label.tr2_term_rec label t

let () = remove_label.tr2_term <- remove_label_term
let remove_label ?(label="") t =
  remove_label.tr2_term (if label="" then None else Some label) t



let decomp_pair_eq = make_trans ()

let decomp_pair_eq_term t =
  match t.desc with
  | BinOp(Eq, t1, t2) ->
      begin match t1.typ with
      | TTuple xs ->
          let aux t =
            match t with
            | {desc=Var y} -> y, Std.identity
            | _ ->
                let y = new_var_of_term t in
                y, make_let [y,[],t]
          in
          let y1,post1 = aux @@ decomp_pair_eq.tr_term t1 in
          let y2,post2 = aux @@ decomp_pair_eq.tr_term t2 in
          let ts = List.mapi (fun i _ -> make_eq (make_proj i @@ make_var y1) (make_proj i @@ make_var y2)) xs in
          post2 @@ post1 @@ List.fold_left make_and true_term ts
      | _ -> decomp_pair_eq.tr_term_rec t
      end
  | _ -> decomp_pair_eq.tr_term_rec t

let () = decomp_pair_eq.tr_term <- decomp_pair_eq_term
let decomp_pair_eq = decomp_pair_eq.tr_term



let elim_unused_let = make_trans2 ()

let elim_unused_let_desc cbv desc =
  match desc with
  | Let(Nonrecursive, bindings, t) ->
      let t' = elim_unused_let.tr2_term cbv t in
      let bindings' = List.map (Triple.map_trd @@ elim_unused_let.tr2_term cbv) bindings in
      let fv = get_fv t' in
      let used (f,xs,t) =
        Id.mem f fv ||
        cbv && not @@ has_no_effect @@ List.fold_right make_fun xs t
      in
      let bindings'' = List.filter used bindings' in
      (make_let bindings'' t').desc
  | Let(Recursive, bindings, t) ->
      let t' = elim_unused_let.tr2_term cbv t in
      let bindings' = List.map (Triple.map_trd @@ elim_unused_let.tr2_term cbv) bindings in
      let fv = get_fv t' in
      let used (f,xs,t) =
        Id.mem f fv ||
        cbv && not @@ has_no_effect @@ List.fold_right make_fun xs t
      in
      if List.exists used bindings'
      then (make_letrec bindings' t').desc
      else t'.desc
  | _ -> elim_unused_let.tr2_desc_rec cbv desc

let () = elim_unused_let.tr2_desc <- elim_unused_let_desc
let elim_unused_let ?(cbv=true) = elim_unused_let.tr2_term cbv



let subst_with_rename = make_trans2 ()

let subst_with_rename_term (x,t) t' =
  match t'.desc with
  | Var y when Id.same x y -> alpha_rename t
  | Fun(y, t1) when Id.same x y -> t'
  | Let(Nonrecursive, bindings, t2) ->
      let aux (f,xs,t1) =
        subst_with_rename.tr2_var (x,t) f,
        List.map (subst_with_rename.tr2_var (x,t)) xs,
        if List.exists (Id.same x) xs then t1 else subst_with_rename.tr2_term (x,t) t1 in
      let bindings' = List.map aux bindings in
      let t2' =
        if List.exists (fun (f,_,_) -> Id.same f x) bindings
        then t2
        else subst_with_rename.tr2_term (x,t) t2
      in
      make_let bindings' t2'
  | Let(Recursive, bindings, t2) when List.exists (fun (f,_,_) -> Id.same f x) bindings -> t'
  | Let(Recursive, bindings, t2) ->
      let aux (f,xs,t1) =
        subst_with_rename.tr2_var (x,t) f,
        List.map (subst_with_rename.tr2_var (x,t)) xs,
        if List.exists (Id.same x) xs then t1 else subst_with_rename.tr2_term (x,t) t1
      in
      let bindings' = List.map aux bindings in
      let t2' = subst_with_rename.tr2_term (x,t) t2 in
      make_letrec bindings' t2'
  | Match(t1,pats) ->
      let aux (pat,cond,t1) =
        let xs = get_vars_pat pat in
        if List.exists (Id.same x) xs
        then pat, cond, t1
        else pat, subst_with_rename.tr2_term (x,t) cond, subst_with_rename.tr2_term (x,t) t1
      in
      make_match (subst_with_rename.tr2_term (x,t) t1) (List.map aux pats)
  | _ -> subst_with_rename.tr2_term_rec (x,t) t'

let () = subst_with_rename.tr2_term <- subst_with_rename_term

let subst_with_rename x t1 t2 = subst_with_rename.tr2_term (x,t1) t2


let elim_unused_branch = make_trans ()

let elim_unused_branch_term t =
  match t.desc with
  | If({desc=Const True}, t1, t2) -> elim_unused_branch.tr_term t1
  | If({desc=Const False}, t1, t2) -> elim_unused_branch.tr_term t2
  | If({desc=BinOp(Eq,{desc=Var x},{desc=Var y})}, t1, t2) when Id.same x y -> elim_unused_branch.tr_term t1
  | _ -> elim_unused_branch.tr_term_rec t

let () = elim_unused_branch.tr_term <- elim_unused_branch_term
let elim_unused_branch = elim_unused_branch.tr_term



let inline_simple_exp = make_trans ()

let inline_simple_exp_term t =
  match t.desc with
  | Let(Nonrecursive, [x,[],t1],t2) when is_simple_aexp t1 || is_simple_bexp t1 ->
      inline_simple_exp.tr_term @@ subst x t1 t2
  | _ -> inline_simple_exp.tr_term_rec t

let () = inline_simple_exp.tr_term <- inline_simple_exp_term
let inline_simple_exp = inline_simple_exp.tr_term



let replace_base_with_int = make_trans ()

let base_types = ["char"; "string"; "float"; "int32"; "int64"; "nativeint"]
let is_base_typ s = List.mem s base_types

let replace_base_with_int_desc desc =
  match desc with
  | Const(Char c) -> Const (Int (int_of_char c))
  | Const(String _ | Float _ | Int32 _ | Int64 _ | Nativeint _) -> randint_unit_term.desc
  | Const(RandValue(TData(s,_), b)) when is_base_typ s -> Const (RandInt b)
  | _ -> replace_base_with_int.tr_desc_rec desc

let replace_base_with_int_typ typ =
  match typ with
  | TData(s, b) when is_base_typ s -> TInt
  | _ -> replace_base_with_int.tr_typ_rec typ

let () = replace_base_with_int.tr_desc <- replace_base_with_int_desc
let () = replace_base_with_int.tr_typ <- replace_base_with_int_typ
let replace_base_with_int t =
  typ_excep := replace_base_with_int.tr_typ !typ_excep;
  replace_base_with_int.tr_term t



let abst_ref = make_trans ()

let abst_ref_term t =
  match t.desc with
  | Ref t1 ->
      make_ignore @@ abst_ref.tr_term t1
  | Deref t1 ->
      let t1' = abst_ref.tr_term t1 in
      let typ = abst_ref.tr_typ t.typ in
      make_seq t1' @@ make_randvalue_unit typ
  | SetRef(t1, t2) ->
      let t1' = abst_ref.tr_term t1 in
      let t2' = abst_ref.tr_term t2 in
      make_ignore @@ make_pair t1' t2'
  | _ -> abst_ref.tr_term_rec t

let abst_ref_typ typ =
  match typ with
  | TRef _ -> TUnit
  | _ -> abst_ref.tr_typ_rec typ

let () = abst_ref.tr_term <- abst_ref_term
let () = abst_ref.tr_typ <- abst_ref_typ
let abst_ref t =
  typ_excep := abst_ref.tr_typ !typ_excep;
  Type_decl.map abst_ref.tr_typ;
  t |> abst_ref.tr_term |> inst_randval


let remove_top_por = make_trans ()

let remove_top_por_term t =
  match t.desc with
  | Match(t, pats) ->
      let aux (p,t1,t2) =
        let rec flatten p =
          match p.pat_desc with
          | POr(p1,p2) -> flatten p1 @ flatten p2
          | _ -> [p]
        in
        let t1' = alpha_rename @@ remove_top_por.tr_term t1 in
        let t2' = alpha_rename @@ remove_top_por.tr_term t2 in
        List.map (fun p -> p, t1', t2') @@ flatten p
      in
      let t' = remove_top_por.tr_term t in
      make_match t' @@ List.flatten_map aux pats
  | _ -> remove_top_por.tr_term_rec t

let () = remove_top_por.tr_term <- remove_top_por_term
let remove_top_por = remove_top_por.tr_term



let short_circuit_eval = make_trans ()

let short_circuit_eval_term t =
  match t.desc with
  | BinOp(And, t1, t2) ->
      let t1' = short_circuit_eval.tr_term t1 in
      let t2' = short_circuit_eval.tr_term t2 in
      make_if t1' t2' false_term
  | BinOp(Or, t1, t2) ->
      let t1' = short_circuit_eval.tr_term t1 in
      let t2' = short_circuit_eval.tr_term t2 in
      make_if t1' true_term t2'
  | _ -> short_circuit_eval.tr_term_rec t

let () = short_circuit_eval.tr_term <- short_circuit_eval_term
let short_circuit_eval = short_circuit_eval.tr_term



(* input is assumed to be a CBN-program *)
let expand_let_val = make_trans ()

let expand_let_val_term t =
  match t.desc with
  | Let(flag, bindings, t2) ->
      let bindings' = List.map (fun (f,xs,t) -> f, xs, expand_let_val.tr_term t) bindings in
      let t2' = expand_let_val.tr_term t2 in
      let bindings1,bindings2 = List.partition (fun (_,xs,_) -> xs = []) bindings' in
      let t2'' = List.fold_left (fun t (f,_,t') -> subst_with_rename f t' t) t2' bindings1 in
      {(make_let_f flag bindings2 t2'') with attr=t.attr}
  | _ -> expand_let_val.tr_term_rec t

let () = expand_let_val.tr_term <- expand_let_val_term
let expand_let_val t =
  assert (List.mem ACPS t.attr);
  expand_let_val.tr_term t



(* reduce only terms of the form "(fun x -> t1) t2" *)
(* t is assumed to be a CBN-program *)
let rec beta_reduce t =
  let desc =
    match t.desc with
    | Const c -> Const c
    | Var x -> Var x
    | Fun(x, t) -> Fun(x, beta_reduce t)
    | App(t, []) -> (beta_reduce t).desc
    | App(t1, t2::ts) ->
        begin
          match beta_reduce t1 with
          | {desc=Fun(x,t1')} ->
              (beta_reduce {desc=App(subst_with_rename x t2 t1', ts); typ=t.typ; attr=t.attr}).desc
          | t1' ->
              let ts' = List.map beta_reduce (t2::ts) in
              (make_app t1' ts').desc
        end
    | If(t1, t2, t3) -> If(beta_reduce t1, beta_reduce t2, beta_reduce t3)
    | Let(flag,bindings,t) ->
        let bindings' = List.map (fun (f,xs,t) -> f, xs, beta_reduce t) bindings in
        Let(flag, bindings', beta_reduce t)
    | BinOp(op, t1, t2) -> BinOp(op, beta_reduce t1, beta_reduce t2)
    | Not t1 -> Not (beta_reduce t1)
    | Event(s,b) -> Event(s,b)
    | Tuple ts -> Tuple (List.map beta_reduce ts)
    | Proj(i, t1) -> Proj(i, beta_reduce t1)
    | Bottom -> Bottom
    | _ -> Format.printf "%a@." Print.term t; assert false
  in
  let t' = {desc; typ=t.typ; attr=t.attr} in
  if false && t<>t' then Format.printf "%a ===> %a@.@." Print.term t Print.term t';
  t'




let replace_bottom_def = make_trans ()

let replace_bottom_def_desc desc =
  match desc with
  | Let(flag, [f,xs,t1], t2) when is_bottom_def flag f xs t1 ->
      Let(flag, [f,xs,make_bottom t1.typ], replace_bottom_def.tr_term t2)
  | _ -> replace_bottom_def.tr_desc_rec desc

let () = replace_bottom_def.tr_desc <- replace_bottom_def_desc
let replace_bottom_def = replace_bottom_def.tr_term



let flatten_tuple = make_trans ()

let flatten_tuple_typ typ =
  match typ with
  | TTuple xs ->
      let xs' = List.map flatten_tuple.tr_var xs in
      let ys = List.flatten_map (fun x -> match Id.typ x with TTuple xs -> xs | _ -> [x]) xs' in
      TTuple ys
  | _ -> flatten_tuple.tr_typ_rec typ

let make_proj' i t =
  match t.typ with
  | TTuple _ -> make_proj i t
  | _ -> assert (i=0); t
let make_tuple' ts =
  match ts with
  | [] -> assert false
  | [t] -> t
  | _ -> make_tuple ts

let flatten_tuple_term t =
  match t.desc with
  | Match _ -> unsupported "not implemented: flatten_tuple (match)"
  | Proj(i,t1) ->
      let t1' = flatten_tuple.tr_term t1 in
      let x = Id.add_name_after (new_var_of_term t1') @@ string_of_int i in
      let ns = List.map (fun typ -> match flatten_tuple.tr_typ typ with TTuple xs' -> List.length xs' | _ -> 1) @@ decomp_ttuple t1.typ in
      let rec new_pos i j acc ns =
        match ns with
        | [] -> assert false
        | n::ns' ->
            if i = j
            then List.map ((+) acc) @@ List.fromto 0 n
            else new_pos i (j+1) (n+acc) ns'
      in
      make_let [x,[],t1'] @@ make_tuple' @@ List.map (make_proj' -$- make_var x) @@ new_pos i 0 0 ns
  | Tuple ts ->
      let ts' = List.map flatten_tuple.tr_term ts in
      let xs' = List.map new_var_of_term ts' in
      let aux y t =
        let ys = match Id.typ y with TTuple ys -> ys | _ -> [y] in
        let aux2 i _ =
          let t = make_proj' i @@ make_var y in
          let y' = new_var_of_term t in
          y', (y', [], t) in
        let ys',defs = List.split @@ List.mapi aux2 ys in
        make_lets defs,
        List.map make_var ys'
      in
      let conts,tss = List.split @@ List.map2 aux xs' ts' in
      make_lets (List.map2 (fun x t -> x,[],t) xs' ts') @@ List.fold_left (|>) (make_tuple' @@ List.flatten tss) conts
  | _ -> flatten_tuple.tr_term_rec t

let () = flatten_tuple.tr_typ <- flatten_tuple_typ
let () = flatten_tuple.tr_term <- flatten_tuple_term
let flatten_tuple = flatten_tuple.tr_term


let rec is_in_redex x t =
  match t.desc with
  | Var y -> Some (Id.same x y)
  | Const _ -> Some false
  | Tuple ts ->
      let rs = List.map (is_in_redex x) ts in
      List.fold_right (fun r acc -> match acc with None -> None | Some b -> Option.map ((||) b) r) rs (Some false)
  | Proj(i,t1) -> is_in_redex x t1
  | Let(flag, bindings, t1) when List.for_all ((<>) [] -| Triple.snd) bindings ->
      is_in_redex x t1
  | _ -> None

let can_inline x t =
  let n = List.length @@ List.filter (Id.same x) @@ get_fv ~cmp:(fun _ _ -> false) t in
  n = 1 && Option.default false @@ is_in_redex x t

let inline_next_redex = make_trans ()

let inline_next_redex_term t =
  match t.desc with
  | Let(Nonrecursive, [x,[],t1], t2) ->
      let t1' = inline_next_redex.tr_term t1 in
      let t2' = inline_next_redex.tr_term t2 in
      if can_inline x t2'
      then subst x t1' t2'
      else inline_next_redex.tr_term_rec t
  | _ -> inline_next_redex.tr_term_rec t

let () = inline_next_redex.tr_term <- inline_next_redex_term
let inline_next_redex = inline_next_redex.tr_term


let beta_var_tuple = make_trans2 ()

let beta_var_tuple_term env t =
  match t.desc with
  | Let(Nonrecursive, [x,[],({desc=Tuple ts} as t1)], t2) ->
      let xs = List.map (function {desc=Var x} -> Some x | _ -> None) ts in
      if List.for_all Option.is_some xs
      then
        let xs' = List.map Option.get xs in
        make_let [x,[],t1] @@ beta_var_tuple.tr2_term ((x,xs')::env) t2
      else beta_var_tuple.tr2_term_rec env t
  | Proj(i,{desc=Var x}) when Id.mem_assoc x env -> make_var @@ List.nth (Id.assoc x env) i
  | _ -> beta_var_tuple.tr2_term_rec env t

let () = beta_var_tuple.tr2_term <- beta_var_tuple_term
let beta_var_tuple = beta_var_tuple.tr2_term []


let beta_no_effect_tuple = make_trans2 ()

let beta_no_effect_tuple_term env t =
  match t.desc with
  | Let(Nonrecursive, [x,[],({desc=Tuple ts} as t1)], t2) ->
      if List.for_all has_no_effect ts
      then make_let [x,[],t1] @@ beta_no_effect_tuple.tr2_term ((x,ts)::env) t2
      else beta_no_effect_tuple.tr2_term_rec env t
  | Proj(i,{desc=Var x}) when Id.mem_assoc x env -> List.nth (Id.assoc x env) i
  | _ -> beta_no_effect_tuple.tr2_term_rec env t

let () = beta_no_effect_tuple.tr2_term <- beta_no_effect_tuple_term
let beta_no_effect_tuple = beta_no_effect_tuple.tr2_term []


let reduce_bottom = make_trans ()

let reduce_bottom_term t =
  let t' = reduce_bottom.tr_term_rec t in
  match t'.desc with
  | Let(_, [x,[],{desc=Bottom}], _) -> make_bottom t.typ
  | _ -> t'

let () = reduce_bottom.tr_term <- reduce_bottom_term
let reduce_bottom = reduce_bottom.tr_term



let merge_bound_var_typ = make_trans2 ()

let merge_bound_var_typ_desc map desc =
  match desc with
  | Let(flag,bindings,t) ->
      let aux (f,xs,t) =
        let f' =
          try
            let typ = Id.assoc f map in
            Id.map_typ (merge_typ typ) f
          with Not_found -> f
        in
        let t' = merge_bound_var_typ.tr2_term map t in
        f', xs, t'
      in
      let bindings' = List.map aux bindings in
      let t' = merge_bound_var_typ.tr2_term map t in
      Let(flag, bindings', t')
  | _ -> merge_bound_var_typ.tr2_desc_rec map desc

let () = merge_bound_var_typ.tr2_desc <- merge_bound_var_typ_desc
let merge_bound_var_typ map t =
  t
  |> merge_bound_var_typ.tr2_term map
  |> propagate_typ_arg



let copy_poly_funs = make_trans ()

let copy_poly_funs_desc desc =
  match desc with
  | Let(flag, [f, xs, t1], t2) when is_poly_typ (Id.typ f) ->
      let tvars = get_tvars (Id.typ f) in
      assert (tvars <> []);
      let t2' = copy_poly_funs.tr_term t2 in
      let t2'' = inst_tvar_tunit t2' in
      let map,t2''' = rename_poly_funs f t2'' in
      let n = List.length map in
      if debug() && n >= 2
      then
        begin
          Format.printf "COPY: @[";
          List.iter (fun (_,x) -> Format.printf "%a;@ " Print.id_typ x) map;
          Format.printf "@.";
        end;
      if map = []
      then (inst_tvar_tunit (make_let_f flag [f, xs, copy_poly_funs.tr_term t1] t2')).desc
      else
        let aux t (_,f') =
          let tvar_map = List.map (fun v -> v, ref None) tvars in
          Type.unify (rename_tvar.tr2_typ tvar_map @@ Id.typ f) (Id.typ f');
          let xs = List.map (rename_tvar.tr2_var tvar_map) xs in
          let t1 = rename_tvar.tr2_term tvar_map t1 in
          let t1 =
            match flag with
            | Nonrecursive -> t1
            | Recursive -> subst_var f f' t1
          in
          let t1 = copy_poly_funs.tr_term t1 in
          let t1 = alpha_rename t1 in
          make_let_f flag [f', xs, t1] t
        in
        (List.fold_left aux t2''' map).desc
  | Let(flag, defs, t) ->
      if List.for_all (not -| is_poly_typ -| Id.typ -| Triple.fst) defs
      then
        let defs' = List.map (Triple.map_trd copy_poly_funs.tr_term) defs in
        Let(flag, defs', copy_poly_funs.tr_term t)
      else
        raise (Fatal "Not implemented: let [rec] ... and ... with polymorphic types.\nPlease use type annotations.")
  | _ -> copy_poly_funs.tr_desc_rec desc

let () = copy_poly_funs.tr_desc <- copy_poly_funs_desc
let copy_poly_funs t =
  t
  |@> unify_pattern_var
  |> copy_poly_funs.tr_term
  |> flatten_tvar
  |> inline_var_const
  |@> Type_check.check -$- Type.TUnit




let rec get_last_definition f t =
  match t.desc with
  | Let(_, bindings, t2) ->
      let f,_,_ = List.last bindings in
      get_last_definition (Some f) t2
  | Fun _ -> assert false
  | _ -> f




let rec replace_main main t =
  match t.desc with
  | Let(flag, bindings, t2) ->
      make_let_f flag bindings @@ replace_main main t2
  | _ ->
      assert (t = unit_term);
      main


let set_main t =
  match get_last_definition None t with
  | None ->
      let u = Id.new_var ~name:"main" t.typ in
      None, make_let [u, [], t] unit_term
  | Some f ->
      let xs = get_args (Id.typ f) in
      let t' =
        if xs = [] && Id.typ f = TUnit
        then replace_main (make_var f) t
        else
          let aux i x =
            let x' = Id.new_var ~name:("arg" ^ string_of_int @@ i+1) @@ Id.typ x in
            let t = make_randvalue_unit @@ Id.typ x in
            x', [], t
          in
          let bindings = List.mapi aux xs in
          let main = make_app (make_var f) @@ List.map make_var @@ List.map Triple.fst bindings in
          let main = make_lets bindings main in
          let u = Id.new_var ~name:"main" main.typ in
          let main = make_let [u, [], main] unit_term in
          replace_main main t
      in
      let t'' = inst_randval t' in
      Some (Id.name f, List.length xs), t''
let set_main = set_main |- Pair.map_snd (flatten_tvar |- inline_var_const)


let ref_to_assert ref_env t =
  let main =
    let aux (f, typ) =
      if not @@ Type.same_shape (Id.typ f) (Ref_type.to_simple typ) then
        begin
          Format.printf "VAR: %a@." Id.print f;
          Format.printf "  Prog: %a@." Print.typ @@ Id.typ f;
          Format.printf "  Spec: %a@." Ref_type.print typ;
          fatal @@ Format.sprintf "Type of %s in the specification is wrong?" @@ Id.name f
        end;
      let genv',cenv',t_typ = Ref_type.generate_check [] [] f typ in
      let defs = List.map snd (genv' @ cenv') in
      make_letrecs defs @@ make_assert t_typ
    in
    List.fold_right (make_seq -| aux) ref_env unit_term
  in
  let map = List.map (Pair.map_snd Ref_type.to_abst_typ) ref_env in
  merge_bound_var_typ map @@ replace_main main t
