
open Util
open Syntax
open Term_util
open Type


let flatten_tvar = (make_trans ()).tr_term



let inst_tvar_tunit = make_trans ()

let inst_tvar_tunit_typ typ =
  match typ with
    TVar({contents=None} as r) -> r := Some TUnit; TUnit
  | _ -> inst_tvar_tunit.tr_typ_rec typ

let () = inst_tvar_tunit.tr_typ <- inst_tvar_tunit_typ

let inst_tvar_tunit = inst_tvar_tunit.tr_term




let rename_tvar = make_trans2 ()

let rename_tvar_typ map typ =
  match typ with
    TVar({contents=None} as x) when List.mem_assq x map -> TVar (List.assq x map)
  | _ -> rename_tvar.tr2_typ_rec map typ

let () = rename_tvar.tr2_typ <- rename_tvar_typ







let rec get_tvars typ =
  let (@@@) xs ys = List.fold_left (fun xs y -> if List.memq y xs then xs else y::xs) xs ys in
    match typ with
        TUnit -> []
      | TBool -> []
      | TAbsBool -> assert false
      | TInt -> []
      | TRInt _ -> []
      | TVar({contents=None} as x) -> [x]
      | TVar{contents=Some typ} -> get_tvars typ
      | TFun(x,typ) -> get_tvars (Id.typ x) @@@ get_tvars typ
      | TList typ -> get_tvars typ
      | TPair(x,typ) -> get_tvars (Id.typ x) @@@ get_tvars typ
      | TConstr(s,b) -> []
      | TPred(x,_) -> get_tvars (Id.typ x)







let rec rename_poly_funs_list f map ts =
  let aux t (map,ts) =
    let map',t' = rename_poly_funs f map t in
      map', t'::ts
  in
    List.fold_right aux ts (map,[])

and rename_poly_funs f map t =
  let map',desc =
    match t.desc with
        Const _
      | Unknown
      | RandInt _ -> map, t.desc
      | Var x when Id.same x f ->
          if is_poly_typ t.typ
          then raise (Fatal "Cannot occur? @ Trans.rename_poly_funs")
          else
            begin
              try
                let _,x' = List.find (fun (_,f') -> Type.can_unify (Id.typ f') (Id.typ x)) map in
                  map, Var x'
              with Not_found ->
                let x' = Id.new_var_id x in
                  (x,x')::map, Var x'
            end
      | Var x -> map, Var x
      | Fun(x, t) ->
          let map',t' = rename_poly_funs f map t in
            map', Fun(x, t')
      | App({desc=Var x; typ=typ}, ts) when Id.same x f ->
          let x' = Id.new_var (Id.name x) typ in
          let map',ts' = rename_poly_funs_list f map ts in
          let check (_,f') = Type.can_unify (Id.typ f') (Id.typ x') in
            if List.exists check map'
            then
              let _,x'' = List.find check map' in
              unify (Id.typ x') (Id.typ x'');
                map', App(make_var x'', ts')
            else (x,x')::map', App(make_var x', ts')
      | App({desc=Var x}, ts) ->
          let map',ts' = rename_poly_funs_list f map ts in
            map', App(make_var x, ts')
      | App(t, ts) ->
          let map',ts' = rename_poly_funs_list f map ts in
          let map'',t' = rename_poly_funs f map' t in
            map'', App(t',ts')
      | If(t1, t2, t3) ->
          let map1,t1' = rename_poly_funs f map t1 in
          let map2,t2' = rename_poly_funs f map1 t2 in
          let map3,t3' = rename_poly_funs f map2 t3 in
            map3, If(t1', t2', t3')
      | Branch(t1, t2) ->
          let map1,t1' = rename_poly_funs f map t1 in
          let map2,t2' = rename_poly_funs f map1 t2 in
            map2, Branch(t1', t2')
      | Let(flag, bindings, t2) ->
          let aux (g,xs,t) (map,bindings) =
            let map',t' = rename_poly_funs f map t in
              map', (g,xs,t')::bindings
          in
          let map',bindings' = List.fold_right aux bindings (map,[]) in
          let map'',t2' = rename_poly_funs f map' t2 in
            map'', Let(flag, bindings', t2')
      | BinOp(op, t1, t2) ->
          let map1,t1' = rename_poly_funs f map t1 in
          let map2,t2' = rename_poly_funs f map1 t2 in
            map2, BinOp(op, t1', t2')
      | Not t ->
          let map',t' = rename_poly_funs f map t in
            map', Not t'
      | Event(s,b) -> map, Event(s,b)
      | Record fields -> assert false
      | Proj(i,s,f,t) -> assert false
      | SetField(n,i,s,f,t1,t2) -> assert false
      | Nil -> map, Nil
      | Cons(t1,t2) ->
          let map1,t1' = rename_poly_funs f map t1 in
          let map2,t2' = rename_poly_funs f map1 t2 in
            map2, Cons(t1', t2')
      | Constr(s,ts) ->
          let map',ts' = rename_poly_funs_list f map ts in
            map', Constr(s, ts')
      | Match(t,pats) ->
          let aux (p,c,t) (map,bindings) =
            let map',t' = rename_poly_funs f map t in
              map', (p,c,t')::bindings
          in
          let map',pats' = List.fold_right aux pats (map,[]) in
          let map'',t' = rename_poly_funs f map' t in
            map'', Match(t', pats')
      | Raise t ->
          let map',t' = rename_poly_funs f map t in
            map', Raise t'
      | TryWith(t1,t2) ->
          let map1,t1' = rename_poly_funs f map t1 in
          let map2,t2' = rename_poly_funs f map1 t2 in
            map2, TryWith(t1', t2')
      | Bottom -> map, Bottom
      | Pair(t1,t2) ->
          let map1,t1' = rename_poly_funs f map t1 in
          let map2,t2' = rename_poly_funs f map1 t2 in
            map2, Pair(t1', t2')
      | Fst t ->
          let map',t' = rename_poly_funs f map t in
            map', Fst t'
      | Snd t ->
          let map',t' = rename_poly_funs f map t in
            map', Snd t'
      | RandValue (_, _) -> assert false
      | Label _ -> assert false
  in
    map', {desc=desc; typ=t.typ}
let rename_poly_funs f t = rename_poly_funs f [] t
















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
      if !Flag.debug_level > 0 && n >= 2
      then
        begin
          Format.printf "COPY: @[";
          List.iter (fun (_,x) -> Format.printf "%a;@ " print_id_typ x) map;
          Format.printf "@.";
        end;
      if map = []
      then (inst_tvar_tunit (make_let_f flag [f, xs, copy_poly_funs.tr_term t1] t2')).desc
      else
        let aux t (_,f') =
          let tvar_map = List.map (fun v -> v, ref None) tvars in
          let () = Type.unify (rename_tvar.tr2_typ tvar_map (Id.typ f)) (Id.typ f') in
          let xs = List.map (rename_tvar.tr2_var tvar_map) xs in
          let t1 = rename_tvar.tr2_term tvar_map t1 in
          let xs' = xs in
          let t1 =
            match flag with
              Nonrecursive -> t1
            | Recursive -> subst f (make_var f') t1
          in
          let t1 = copy_poly_funs.tr_term t1 in
          make_let_f flag [f', xs', t1] t
        in
        (List.fold_left aux t2''' map).desc
  | Let(flag, defs, t) ->
      if List.for_all (fun (f,_,_) -> not (is_poly_typ (Id.typ f))) defs
      then
        let defs' = List.map (fun (f,xs,t) -> f, xs, copy_poly_funs.tr_term t) defs in
        Let(flag, defs', copy_poly_funs.tr_term t)
      else
        raise (Fatal "Not implemented: let [rec] ... and ... with polymorphic type.\nPlease use type annotations.")
  | _ -> copy_poly_funs.tr_desc_rec desc

let () = copy_poly_funs.tr_desc <- copy_poly_funs_desc

let copy_poly_funs t =
  let t' = flatten_tvar (copy_poly_funs.tr_term t) in
  Type_check.check t' Type.TUnit;
  t'












let rec inst_randvalue env defs typ =
  if List.mem_assoc typ env
  then env, defs, make_app (make_var (List.assoc typ env)) [unit_term]
  else
    match typ with
      | TUnit -> env, defs, unit_term
      | TBool -> env, defs, randbool_unit_term
      | TInt -> env, defs, randint_unit_term
      | TVar({contents=None} as r) -> r := Some TUnit; inst_randvalue env defs TUnit
      | TVar{contents=Some typ} -> inst_randvalue env defs typ
      | TFun(x,typ) ->
          let env',defs',t = inst_randvalue env defs typ in
          env', defs', make_fun x t
      | TList (TVar({contents=None} as r)) ->
          r := Some TUnit; inst_randvalue env defs typ
      | TList typ' ->
          let u = Id.new_var "u" TUnit in
          let f = Id.new_var ("make_" ^ to_id_string typ) (TFun(u,typ)) in
          let env' = (typ,f)::env in
          let env'',defs',t_typ' = inst_randvalue env' defs typ' in
          let t_typ =
            make_if randbool_unit_term (make_nil typ') (make_cons t_typ' (make_app (make_var f) [unit_term]))
          in
          env'', (f,[u],t_typ)::defs', make_app (make_var f) [unit_term]
      | TPair(x,typ) ->
          let env',defs',t1 = inst_randvalue env defs (Id.typ x) in
          let env'',defs'',t2 = inst_randvalue env' defs' typ in
          env'', defs'', make_pair t1 t2
      | TConstr(s,false) -> env, defs, make_randvalue typ
      | TConstr(s,true) ->
          let u = Id.new_var "u" TUnit in
          let f = Id.new_var ("make_" ^ to_id_string typ) (TFun(u,typ)) in
          let env' = (typ,f)::env in
          let env'',defs',t =
            match Type_decl.assoc_typ s with
                Type_decl.TKVariant stypss ->
                  let n = List.length stypss in
                  let aux1 (s,typs) (env,defs,itss,i) =
                    let aux2 typ (env,defs,ts) =
                      let env', defs',t = inst_randvalue env defs typ in
                      env', defs', t::ts
                    in
                    let env',defs',ts' = List.fold_right aux2 typs (env,defs,[]) in
                    env', defs', (i-1,ts')::itss, i-1
                  in
                  let env'',defs',itss,_ = List.fold_right aux1 stypss (env',defs,[],n) in
                  let aux (s,typs) (i,ts) =
                    let p = if i < n-1 then make_pconst (make_int i) else make_pany TInt in
                    p, true_term, {desc=Constr(s,ts); typ=typ}
                  in
                  env'', defs', make_match randint_unit_term (List.map2 aux stypss itss)
              | Type_decl.TKRecord sftyps -> raise (Fatal "Not implemented: inst_randvalue(TKRecord)")
          in
          env'', (f,[u],t)::defs', make_app (make_var f) [unit_term]
      | _ -> Format.printf "inst_randvalue: %a@." print_typ typ; assert false




let rec get_last_definition f t =
  match t.desc with
      Let(_, bindings, t2) ->
        let f,_,_ = last bindings in
          get_last_definition (Some f) t2
    | Fun _ -> assert false
    | _ -> f




let rec replace_main main t =
  match t.desc with
      Let(flag, bindings, t2) -> make_let_f flag bindings (replace_main main t2)
    | Fun _ -> assert false
    | _ -> main




let gen_id =
  let cnt = ref 0 in
  fun () -> cnt := !cnt + 1; string_of_int !cnt

let set_target t =
  match get_last_definition None t with
      None ->
        let u = Id.new_var "main" t.typ in
          "", 0, make_let [u, [], t] unit_term
    | Some f ->
        let xs = get_args (Id.typ f) in
        let main =
          if xs = [] && Id.typ f = TUnit
          then replace_main (make_var f) t
          else
            let rec aux x (env,defs,args) =
              let env',defs',arg = inst_randvalue [] defs (Id.typ x) in
                env',defs', arg::args
            in
            let _,defs,args = List.fold_right aux xs ([],[],[]) in
            let aux arg =
              let x = Id.new_var ("arg" ^ gen_id ()) arg.typ in
                x, [], arg
            in
            let bindings = List.map aux args in
            let main = make_app (make_var f) (List.map (fun (x,_,_) -> make_var x) bindings) in
            let main = make_lets bindings main in
            let main = make_letrec defs main in
            let u = Id.new_var "main" main.typ in
            let main = make_let [u, [], main] unit_term in
              replace_main main t
        in
          Id.name f, List.length xs, main












(** [let f ... = fun x -> t] や [let f ... = let g x = t in g] を [let f ... x = t] に *)
let rec merge_let_fun t =
  let desc =
    match t.desc with
        Const c -> Const c
      | Unknown -> Unknown
      | RandInt b -> RandInt b
      | Var x -> Var x
      | App(t, ts) -> App(merge_let_fun t, List.map merge_let_fun ts)
      | If(t1, t2, t3) -> If(merge_let_fun t1, merge_let_fun t2, merge_let_fun t3)
      | Branch(t1, t2) -> Branch(merge_let_fun t1, merge_let_fun t2)
      | Let(flag, bindings, t2) ->
          let aux (f,xs,t) =
            let ys,t' = decomp_fun t in
              f, xs@ys, merge_let_fun t'
          in
            Let(flag, List.map aux bindings, merge_let_fun t2)
      | Fun(x, t) -> Fun(x, merge_let_fun t)
      | BinOp(op, t1, t2) -> BinOp(op, merge_let_fun t1, merge_let_fun t2)
      | Not t -> Not (merge_let_fun t)
      | Event(s,b) -> Event(s,b)
      | Record fields -> Record (List.map (fun (f,(s,t)) -> f,(s,merge_let_fun t)) fields)
      | Proj(i,s,f,t) -> Proj(i,s,f,merge_let_fun t)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,merge_let_fun t1,merge_let_fun t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(merge_let_fun t1, merge_let_fun t2)
      | Constr(s,ts) -> Constr(s, List.map merge_let_fun ts)
      | Match(t,pats) -> Match(merge_let_fun t, List.map (fun (pat,cond,t) -> pat,cond,merge_let_fun t) pats)
      | Raise t -> Raise (merge_let_fun t)
      | TryWith(t1,t2) -> TryWith(merge_let_fun t1, merge_let_fun t2)
      | Bottom -> Bottom
      | Pair(t1,t2) -> Pair(merge_let_fun t1, merge_let_fun t2)
      | Fst t -> Fst (merge_let_fun t)
      | Snd t -> Snd (merge_let_fun t)
      | RandValue _ -> assert false
      | Label _ -> assert false
  in
    {desc=desc; typ=t.typ}



let rec canonize t =
  let desc =
    match t.desc with
        Const c -> Const c
      | Unknown -> Unknown
      | RandInt b -> RandInt b
      | Var x -> Var x
      | App(t, ts) ->
          let t' = canonize t in
          let ts' = List.map canonize ts in
            App(t', ts')
      | If(t1, t2, t3) ->
          let t1' = canonize t1 in
          let t2' = canonize t2 in
          let t3' = canonize t3 in
            If(t1', t2', t3')
      | Branch(t1, t2) ->
          let t1' = canonize t1 in
          let t2' = canonize t2 in
            Branch(t1', t2')
      | Let(flag, bindings, t) ->
          let bindings' = List.map (fun (f,xs,t) -> f,xs,canonize t) bindings in
          let t' = canonize t in
            Let(flag, bindings', t')
      | BinOp(Eq, {desc=Not t1}, t2)
      | BinOp(Eq, t1, {desc=Not t2}) ->
          let t1' = canonize t1 in
          let t2' = canonize t2 in
          let t1 = {desc=BinOp(Or, t1, t2); typ=TBool} in
          let t2 = {desc=BinOp(Or, {desc=Not t1';typ=TBool}, {desc=Not t2';typ=TBool}); typ=TBool} in
            BinOp(And, t1, t2)
      | BinOp(Eq, {desc=BinOp((Eq|Lt|Gt|Leq|Geq|And|Or) as bop, t1, t2)}, t3) ->
          let t1' = canonize t1 in
          let t2' = canonize t2 in
          let t3' = canonize t3 in
          let t1 = {desc=BinOp(Or, {desc=Not t3';typ=TBool}, {desc=BinOp(bop, t1',t2');typ=TBool}); typ=TBool} in
          let t2 = {desc=BinOp(Or, t3', {desc=Not{desc=BinOp(bop, t1', t2');typ=TBool};typ=TBool}); typ=TBool} in
            BinOp(And, t1, t2)
      | BinOp(Eq, t3, {desc=BinOp((Eq|Lt|Gt|Leq|Geq|And|Or) as bop, t1, t2)}) ->
          let t1' = canonize t1 in
          let t2' = canonize t2 in
          let t3' = canonize t3 in
          let t1 = {desc=BinOp(Or, {desc=Not t3';typ=TBool}, {desc=BinOp(bop, t1', t2');typ=TBool}); typ=TBool} in
          let t2 = {desc=BinOp(Or, t3', {desc=Not{desc=BinOp(bop, t1', t2');typ=TBool};typ=TBool}); typ=TBool} in
            BinOp(And, t1, t2)
      | BinOp(op, t1, t2) ->
          let t1' = canonize t1 in
          let t2' = canonize t2 in
            BinOp(op, t1', t2')
      | Not t ->
          let t' = canonize t in
            Not t'
      | Fun(x,t) ->
          let t' = canonize t in
            Fun(x, t')
      | Event(s,b) -> Event(s,b)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(canonize t1, canonize t2)
      | Snd _ -> assert false
      | Fst _ -> assert false
      | Pair (_, _) -> assert false
      | TryWith (_, _) -> assert false
      | Raise _ -> assert false
      | Match (_, _) -> assert false
      | Constr (_, _) -> assert false
      | SetField (_, _, _, _, _, _) -> assert false
      | Proj (_, _, _, _) -> assert false
      | Record _ -> assert false
      | RandValue (_, _) -> assert false
      | Bottom -> assert false
      | Label _ -> assert false
  in
    {desc=desc; typ=t.typ}















let part_eval t =
  let is_apply xs = function
      Var x -> xs = [x]
    | App(t, ts) ->
        let rec aux xs ts =
          match xs,ts with
              [], [] -> true
            | x::xs', {desc=Var y}::ts' when Id.same x y -> aux xs' ts'
            | _ -> false
        in
          aux xs (t::ts)
    | _ -> false
  in
  let is_alias xs = function
      Var x ->
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
  let () = ignore (is_alias [] (Const True)) in
  let rec aux apply t =
    let desc =
      match t.desc with
          Const c -> Const c
        | RandInt b -> RandInt b
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
                  [] ->
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
                  [] -> Fun(x,t)
                | [t] -> t.desc
                | t::ts' -> App(t, ts')
            else
              begin
                match ts with
                    [{desc=Const(True|False)}] -> (aux apply (subst x (List.hd ts) t)).desc
                  | _ ->
                      let t' = aux apply t in
                      let ts' = List.map (aux apply) ts in
                        App({desc=Fun(x,t');typ=typ'}, ts')
              end
        | App(t, ts) -> App(aux apply t, List.map (aux apply) ts)
        | If({desc=Const True}, t2, _) -> (aux apply t2).desc
        | If({desc=Const False}, _, t3) -> (aux apply t3).desc
        | If({desc=Not t1}, t2, t3) -> If(aux apply t1, aux apply t3, aux apply t2)
        | If(t1, t2, t3) ->
            if t2 = t3
            then t2.desc
            else If(aux apply t1, aux apply t2, aux apply t3)
        | Branch(t1, t2) -> Branch(aux apply t1, aux apply t2)
        | Let(flag, [f, xs, t1], t2) ->
            if is_apply xs t1.desc
            then (aux ((f,(xs,t1))::apply) (aux apply t2)).desc
            else
              begin
                match flag, is_alias xs t1.desc  with
                    Nonrecursive, None -> Let(flag, [f, xs, aux apply t1], aux apply t2)
                  | Nonrecursive, Some x -> (subst f (make_var x) (aux apply t2)).desc
                  | Recursive, Some x when not (List.mem f (get_fv t1)) ->
                      (subst f {desc=Var x;typ=Id.typ x} (aux apply t2)).desc
                  | Recursive, _ -> Let(flag, [f, xs, aux apply t1], aux apply t2)
              end
        | Let _ -> assert false
        | BinOp(op, t1, t2) -> BinOp(op, aux apply t1, aux apply t2)
        | Not t -> Not (aux apply t)
        | Unknown -> Unknown
        | Event(s,b) -> Event(s,b)
        | Record fields -> Record (List.map (fun (s,(f,t)) -> s,(f,aux apply t)) fields)
        | Proj(i,s,f,t) -> Proj(i, s, f, aux apply t)
        | Nil -> Nil
        | Cons(t1,t2) -> Cons(aux apply t1, aux apply t2)
        | Constr(c,ts) -> Constr(c, List.map (aux apply) ts)
        | Match(t,pats) ->
            let aux' (pat,cond,t) = pat, aux apply cond, aux apply t in
              Match(aux apply t, List.map aux' pats)
        | Snd _ -> assert false
        | Fst _ -> assert false
        | Pair (_, _) -> assert false
        | TryWith (_, _) -> assert false
        | Raise _ -> assert false
        | SetField (_, _, _, _, _, _) -> assert false
        | RandValue (_, _) -> assert false
        | Bottom -> assert false
        | Label _ -> assert false
    in
      {desc=desc; typ=t.typ}
  in
    aux [] t


























(** return a term whose let-expressions' bodies are side-effect free *)
let rec trans_let t =
  let desc =
    match t.desc with
        Const _
      | Unknown
      | RandInt _
      | RandValue _
      | Var _ -> t.desc
      | Fun(y, t) -> Fun(y, trans_let t)
      | App(t1, ts) ->
          let t1' = trans_let t1 in
          let ts' = List.map trans_let ts in
            App(t1', ts')
      | If(t1, t2, t3) ->
          let t1' = trans_let t1 in
          let t2' = trans_let t2 in
          let t3' = trans_let t3 in
            If(t1', t2', t3')
      | Branch(t1, t2) ->
          let t1' = trans_let t1 in
          let t2' = trans_let t2 in
            Branch(t1', t2')
      | Let(Nonrecursive, [f, [], t1], t2) ->
          App(make_fun f (trans_let t2), [trans_let t1])
      | Let(Nonrecursive, bindings, t2) when List.exists (fun (_,xs,_) -> xs=[]) bindings -> assert false
      | Let(flag, bindings, t2) ->
          let bindings' = List.map (fun (f,xs,t) -> f, xs, trans_let t) bindings in
          let t2' = trans_let t2 in
            Let(flag, bindings', t2')
      | BinOp(op, t1, t2) ->
          let t1' = trans_let t1 in
          let t2' = trans_let t2 in
            BinOp(op, t1', t2')
      | Not t1 ->
          let t1' = trans_let t1 in
            Not t1'
      | Event(s,b) -> Event(s,b)
      | Record fields ->  Record (List.map (fun (f,(s,t1)) -> f,(s,trans_let t1)) fields)
      | Proj(i,s,f,t1) -> Proj(i,s,f,trans_let t1)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,trans_let t1,trans_let t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(trans_let t1, trans_let t2)
      | Constr(s,ts) -> Constr(s, List.map trans_let ts)
      | Match(t1,pats) ->
          let aux (pat,cond,t1) = pat, cond, trans_let t1 in
            Match(trans_let t1, List.map aux pats)
      | TryWith(t1,t2) -> TryWith(trans_let t1, trans_let t2)
      | Pair(t1,t2) -> Pair(trans_let t1, trans_let t2)
      | Fst t -> Fst(trans_let t)
      | Snd t -> Snd(trans_let t)
      | Bottom -> Bottom
      | Raise _ -> assert false
      | Label _ -> assert false
  in
    {desc=desc; typ=t.typ}




let rec propagate_typ_arg t =
  match t.desc with
      Const Unit -> unit_term
    | Const True -> true_term
    | Const False -> false_term
    | Unknown -> assert false
    | Const (Int n) -> make_int n
    | Const c -> t
    | RandInt b -> {desc=RandInt b; typ=t.typ}
    | RandValue(typ,b) -> {desc=RandValue(typ,b); typ=t.typ}
    | Var y -> make_var y
    | Fun(y, t) -> make_fun y (propagate_typ_arg t)
    | App(t1, ts) -> make_app (propagate_typ_arg t1) (List.map propagate_typ_arg ts)
    | If(t1, t2, t3) -> make_if (propagate_typ_arg t1) (propagate_typ_arg t2) (propagate_typ_arg t3)
    | Branch(t1, t2) -> make_branch (propagate_typ_arg t1) (propagate_typ_arg t2)
    | Let(flag, bindings, t2) ->
        let aux (f,xs,t) =
          let xs' =
            let ys = take (get_args (Id.typ f)) (List.length xs) in
            let aux x y ys =
              let ys' = List.map (fun z -> Id.set_typ z (subst_type y (make_var x) (Id.typ z))) ys in
                Id.set_typ x (Id.typ y) :: ys'
            in
              List.fold_right2 aux xs ys []
          in
          let t' = propagate_typ_arg t in
          let t'' = List.fold_left2 (fun t x x' -> subst x (make_var x') t) t' xs xs' in
            f, xs', t''
        in
        let bindings' = List.map aux bindings in
        let t2' = propagate_typ_arg t2 in
          make_let_f flag bindings' t2'
    | BinOp(op, t1, t2) ->
        {desc=BinOp(op, propagate_typ_arg t1, propagate_typ_arg t2); typ=t.typ}
    | Not t1 -> make_not (propagate_typ_arg t1)
    | Event(s,b) -> {desc=Event(s,b); typ=t.typ}
    | Record fields ->
        {desc=Record (List.map (fun (f,(s,t1)) -> f,(s,propagate_typ_arg t1)) fields); typ=t.typ}
    | Proj(i,s,f,t1) ->
        {desc=Proj(i,s,f,propagate_typ_arg t1); typ=t.typ}
    | SetField(n,i,s,f,t1,t2) ->
        {desc=SetField(n,i,s,f,propagate_typ_arg t1,propagate_typ_arg t2); typ=t.typ}
    | Nil -> make_nil2 t.typ
    | Cons(t1,t2) -> make_cons (propagate_typ_arg t1) (propagate_typ_arg t2)
    | Constr(s,ts) -> {desc=Constr(s, List.map propagate_typ_arg ts); typ=t.typ}
    | Match(t1,pats) ->
        let aux (pat,cond,t1) = pat, propagate_typ_arg cond, propagate_typ_arg t1 in
          make_match (propagate_typ_arg t1) (List.map aux pats)
    | Raise t1 -> {desc=Raise (propagate_typ_arg t1); typ=t.typ}
    | TryWith(t1,t2) -> {desc=TryWith(propagate_typ_arg t1, propagate_typ_arg t2); typ=t.typ}
    | Pair(t1,t2) -> make_pair (propagate_typ_arg t1) (propagate_typ_arg t2)
    | Fst t -> make_fst (propagate_typ_arg t)
    | Snd t -> make_snd (propagate_typ_arg t)
    | Bottom -> make_bottom t.typ
    | Label _ -> assert false




let replace_typ_var env x =
  try
    let typ = List.assoc x env in
      Id.set_typ x typ
  with Not_found -> x

let rec replace_typ_aux env t =
  let desc =
    match t.desc with
        Const c -> Const c
      | Unknown -> Unknown
      | RandInt b -> RandInt b
      | RandValue(typ,b) -> RandValue(typ,b)
      | Var y -> Var y
      | Fun(y, t) -> Fun(y, replace_typ_aux env t)
      | App(t1, ts) -> App(replace_typ_aux env t1, List.map (replace_typ_aux env) ts)
      | If(t1, t2, t3) -> If(replace_typ_aux env t1, replace_typ_aux env t2, replace_typ_aux env t3)
      | Branch(t1, t2) -> Branch(replace_typ_aux env t1, replace_typ_aux env t2)
      | Let(flag, bindings, t2) ->
          let aux (f,xs,t) =
            let f' = replace_typ_var env f in
            let () =
              if not (Type.can_unify (Id.typ f) (Id.typ f'))
              then
                let f'' = Id.set_typ f @@ elim_tpred_all @@ Id.typ f' in
                Format.printf "Prog: %a@.Spec: %a@." print_id_typ f print_id_typ f'';
                let msg = Format.sprintf "Type of %s in %s is wrong?" (Id.name f) !Flag.spec_file in
                let msg = msg ^ " (please specify monomorphic types if polymorphic types exist)" in
                raise (Fatal msg)
            in
            let xs' =
              let ys = take (get_args (Id.typ f')) (List.length xs) in
                List.map2 (fun x y -> Id.set_typ x (Id.typ y)) xs ys
            in
            let t' = replace_typ_aux env t in
            let t'' =
              if flag = Nonrecursive
              then t'
              else subst f (make_var f') t'
            in
            let t''' = List.fold_left2 (fun t x x' -> subst x (make_var x') t) t'' xs xs' in
              f', xs', t'''
          in
          let bindings' = List.map aux bindings in
          let t2' = replace_typ_aux env t2 in
          let t2'' = List.fold_left2 (fun t (f,_,_) (f',_,_) -> subst f (make_var f') t) t2' bindings bindings' in
            Let(flag, bindings', t2'')
      | BinOp(op, t1, t2) -> BinOp(op, replace_typ_aux env t1, replace_typ_aux env t2)
      | Not t1 -> Not (replace_typ_aux env t1)
      | Event(s,b) -> Event(s,b)
      | Record fields ->  Record (List.map (fun (f,(s,t1)) -> f,(s,replace_typ_aux env t1)) fields)
      | Proj(i,s,f,t1) -> Proj(i,s,f,replace_typ_aux env t1)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,replace_typ_aux env t1,replace_typ_aux env t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(replace_typ_aux env t1, replace_typ_aux env t2)
      | Constr(s,ts) -> Constr(s, List.map (replace_typ_aux env) ts)
      | Match(t1,pats) ->
          let aux (pat,cond,t1) = pat, replace_typ_aux env cond, replace_typ_aux env t1 in
            Match(replace_typ_aux env t1, List.map aux pats)
      | Raise t -> Raise (replace_typ_aux env t)
      | TryWith(t1,t2) -> TryWith(replace_typ_aux env t1, replace_typ_aux env t2)
      | Pair(t1,t2) -> Pair(replace_typ_aux env t1, replace_typ_aux env t2)
      | Fst t -> Fst(replace_typ_aux env t)
      | Snd t -> Snd(replace_typ_aux env t)
      | Bottom -> Bottom
      | Label _ -> assert false
  in
    {desc=desc; typ=t.typ}

let replace_typ env t =
  let t1 = replace_typ_aux env t in
  let t2 = propagate_typ_arg t1 in
    t2








let rec eval t =
  let desc =
    match t.desc with
        Const c -> Const c
      | Unknown -> Unknown
      | Var x -> Var x
      | App({desc=Fun(x, t)}, t'::ts) ->
          (eval ({desc=App(subst_map [x, t'] t, ts);typ=t.typ})).desc
      | App(t, []) -> (eval t).desc
      | App(t, ts) ->
          App(eval t, List.map eval ts)
      | If({desc=Const True}, t2, t3) ->
          (eval t2).desc
      | If({desc=Const False}, t2, t3) ->
          (eval t3).desc
      | If(t1, t2, t3) ->
          If(eval t1, eval t2, eval t3)
      | Branch(t1, t2) ->
          Branch(eval t1, eval t2)
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
      | Fun(x,{desc=App(t,ts);typ=typ}) ->
          let t' = eval t in
          let ts' = List.map eval ts in
            if ts' <> [] then
              let l, r = list_last_and_rest ts' in
                if l.desc = Var x && List.for_all (fun t -> not (List.mem x (get_fv t))) (t'::r) then
                  (eval {desc=App(t', r);typ=t.typ}).desc
                else
                  Fun(x,{desc=App(t', ts');typ=typ})
            else
              Fun(x,{desc=App(t', ts');typ=typ})
      | Fun(x,t) ->
          Fun(x, eval t)
      | Event(s,b) -> Event(s,b)
      | Snd _ -> assert false
      | Fst _ -> assert false
      | Pair (_, _) -> assert false
      | TryWith (_, _) -> assert false
      | Raise _ -> assert false
      | Match (_, _) -> assert false
      | Constr (_, _) -> assert false
      | SetField (_, _, _, _, _, _) -> assert false
      | Proj (_, _, _, _) -> assert false
      | Record _ -> assert false
      | RandValue (_, _) -> assert false
      | Bottom -> assert false
      | Cons _ -> assert false
      | RandInt _ -> assert false
      | Nil -> assert false
      | Label _ -> assert false
  in
    {desc=desc; typ=t.typ}






(* reduce only terms of the form "(fun x -> t1) t2" *)
(* t is assumed to be a CBN-program *)
let rec beta_reduce t =
  let desc =
    match t.desc with
    | Const c -> Const c
    | Unknown -> Unknown
    | RandInt b -> RandInt b
    | RandValue(typ,b) -> RandValue(typ,b)
    | Var x -> Var x
    | Fun(x, t) -> Fun(x, beta_reduce t)
    | App(t, []) -> (beta_reduce t).desc
    | App(t1, t2::ts) ->
        begin
          match beta_reduce t1 with
          | {desc=Fun(x,t1')} ->
              (beta_reduce {desc=App(subst x t2 t1', ts); typ=t.typ}).desc
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
    | Pair(t1,t2) -> Pair(beta_reduce t1, beta_reduce t2)
    | Fst t1 -> Fst (beta_reduce t1)
    | Snd t1 -> Snd (beta_reduce t1)
    | Bottom -> Bottom
    | _ -> Format.printf "%a@." pp_print_term t; assert false
  in
  let t' = {desc=desc; typ=t.typ} in
  if false && t<>t' then Format.printf "%a ===> %a@.@." pp_print_term t pp_print_term t';
  t'





let normalize_binop_exp op t1 t2 =
  let neg xs = List.map (fun (x,n) -> x,-n) xs in
  let rec decomp t =
    match t.desc with
        Const (Int n) -> [None, n]
      | Var x -> [Some {desc=Var x;typ=Id.typ x}, 1]
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
                  true, true ->
                    Format.printf "Nonlinear expression not supported: %a@."
                      pp_print_term {desc=BinOp(op,t1,t2);typ=TInt};
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
  let compare (x1,_) (x2,_) =
    let aux = function
        None -> "\255"
      | Some {desc=Var x} -> Id.to_string x
      | _ -> assert false
    in
      compare (aux x1) (aux x2)
  in
  let xns = List.sort compare (xns1 @@@ (neg xns2)) in
  let rec aux = function
      [] -> []
    | (x,n)::xns ->
        let xns1,xns2 = List.partition (fun (y,_) -> x=y) xns in
        let n' = List.fold_left (fun acc (_,n) -> acc+n) 0 ((x,n)::xns1) in
          (x,n') :: aux xns2
  in
  let xns' = aux xns in
  let xns'' = List.filter (fun (x,n) -> n<>0) xns' in
  let op',t1',t2' =
    match xns'' with
        [] -> assert false
      | (x,n)::xns ->
          let aux :typed_term option * int -> typed_term= function
              None,n -> {desc=Const (Int n); typ=TInt}
            | Some x,n -> if n=1 then x else make_mul (make_int n) x
          in
          let t1,xns',op' =
            if n<0
            then
              let op' =
                match op with
                    Eq -> Eq
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
                [] -> make_int 0
              | t::ts' -> List.fold_left make_add t ts'
          in
            op', t1, t2
  in
  let rec simplify t =
    let desc =
      match t.desc with
          BinOp(Add, t1, {desc=BinOp(Mult, {desc=Const (Int n)}, t2)}) when n < 0 ->
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
      {desc=desc; typ=t.typ}
  in
    BinOp(op', t1', simplify t2')

let rec normalize_bool_exp t =
  let desc =
    match t.desc with
        Const True -> Const True
      | Const False -> Const False
      | Unknown -> Unknown
      | Var x -> Var x
      | BinOp(Or|And as op, t1, t2) ->
          let t1' = normalize_bool_exp t1 in
          let t2' = normalize_bool_exp t2 in
            BinOp(op, t1', t2')
      | BinOp(Eq, {desc=Const(True|False)|Unknown}, _)
      | BinOp(Eq, _, {desc=Const(True|False)|Unknown})
      | BinOp(Eq, {desc=Nil|Cons _}, _)
      | BinOp(Eq, _, {desc=Nil|Cons _}) as t -> t
      | BinOp(Eq|Lt|Gt|Leq|Geq as op, t1, t2) -> normalize_binop_exp op t1 t2
      | Not t -> Not (normalize_bool_exp t)
      | Const _
      | Fun _
      | App _
      | If _
      | Branch _
      | Let _
      | BinOp((Add|Sub|Mult), _, _)
      | Event _ -> assert false
      | Snd _ -> assert false
      | Fst _ -> assert false
      | Pair (_, _) -> assert false
      | TryWith (_, _) -> assert false
      | Raise _ -> assert false
      | Match (_, _) -> assert false
      | Constr (_, _) -> assert false
      | Cons (_, _) -> assert false
      | SetField (_, _, _, _, _, _) -> assert false
      | Proj (_, _, _, _) -> assert false
      | Record _ -> assert false
      | RandValue (_, _) -> assert false
      | RandInt _ -> assert false
      | Bottom -> assert false
      | Nil -> assert false
      | Label _ -> assert false
  in
    {desc=desc; typ=t.typ}



let rec get_and_list t =
  match t.desc with
      Const True -> [{desc=Const True; typ=t.typ}]
    | Const False -> [{desc=Const False; typ=t.typ}]
    | Unknown -> [{desc=Unknown; typ=t.typ}]
    | Var x -> [{desc=Var x; typ=t.typ}]
    | BinOp(And, t1, t2) -> get_and_list t1 @@@ get_and_list t2
    | BinOp(op, t1, t2) -> [{desc=BinOp(op, t1, t2); typ=t.typ}]
    | Not t -> [{desc=Not t; typ=t.typ}]
    | Const _
    | Fun _
    | App _
    | If _
    | Branch _
    | Let _
    | Event _ -> assert false
    | Snd _ -> assert false
    | Fst _ -> assert false
    | Pair (_, _) -> assert false
    | TryWith (_, _) -> assert false
    | Raise _ -> assert false
    | Match (_, _) -> assert false
    | Constr (_, _) -> assert false
    | Cons (_, _) -> assert false
    | SetField (_, _, _, _, _, _) -> assert false
    | Proj (_, _, _, _) -> assert false
    | Record _ -> assert false
    | RandValue (_, _) -> assert false
    | RandInt _ -> assert false
    | Bottom -> assert false
    | Nil -> assert false
      | Label _ -> assert false

let rec merge_geq_leq t =
  let desc =
    match t.desc with
        Const True -> Const True
      | Const False -> Const False
      | Unknown -> Unknown
      | Var x -> Var x
      | BinOp(And, t1, t2) ->
          let ts = get_and_list t in
          let is_dual t1 t2 = match t1.desc,t2.desc with
              BinOp(op1,t11,t12), BinOp(op2,t21,t22) when t11=t21 && t12=t22 -> op1=Leq && op2=Geq || op1=Geq && op2=Leq
            | _ -> false
          in
          let get_eq t =
            match t.desc with
                BinOp((Leq|Geq),t1,t2) -> {desc=BinOp(Eq,t1,t2); typ=t.typ}
              | _ -> assert false
          in
          let rec aux = function
              [] -> []
            | t::ts ->
                if List.exists (is_dual t) ts
                then
                  let t' = get_eq t in
                  let ts' = List.filter (fun t' -> not (is_dual t t')) ts in
                    t' :: aux ts'
                else
                  t :: aux ts
          in
          let ts' = aux ts in
          let t =
            match ts' with
                [] -> assert false
              | [t] -> t
              | t::ts -> List.fold_left (fun t1 t2 -> {desc=BinOp(And,t1,t2);typ=TBool}) t ts
          in
            t.desc
      | BinOp(Or, t1, t2) ->
          let t1' = merge_geq_leq t1 in
          let t2' = merge_geq_leq t2 in
            BinOp(Or, t1', t2')
      | BinOp(Eq|Lt|Gt|Leq|Geq as op, t1, t2) -> BinOp(op, t1, t2)
      | Not t -> Not (merge_geq_leq t)
      | Const _
      | Fun _
      | App _
      | If _
      | Branch _
      | Let _
      | BinOp((Add|Sub|Mult), _, _)
      | Event _ -> Format.printf "%a@." pp_print_term t; assert false
      | Snd _ -> assert false
      | Fst _ -> assert false
      | Pair (_, _) -> assert false
      | TryWith (_, _) -> assert false
      | Raise _ -> assert false
      | Match (_, _) -> assert false
      | Constr (_, _) -> assert false
      | Cons (_, _) -> assert false
      | SetField (_, _, _, _, _, _) -> assert false
      | Proj (_, _, _, _) -> assert false
      | Record _ -> assert false
      | RandValue (_, _) -> assert false
      | RandInt _ -> assert false
      | Bottom -> assert false
      | Nil -> assert false
      | Label _ -> assert false
  in
    {desc=desc; typ=t.typ}











let rec elim_fun fun_name t =
  let desc =
    match t.desc with
        Const c -> Const c
      | Unknown -> Unknown
      | RandInt b -> RandInt b
      | RandValue(typ,b) -> RandValue(typ,b)
      | Var y -> Var y
      | Fun(y, t1) ->
          let f = Id.new_var fun_name t.typ in
            Let(Nonrecursive, [f, [y], elim_fun fun_name t1], make_var f)
      | App(t1, ts) -> App(elim_fun fun_name t1, List.map (elim_fun fun_name) ts)
      | If(t1, t2, t3) -> If(elim_fun fun_name t1, elim_fun fun_name t2, elim_fun fun_name t3)
      | Branch(t1, t2) -> Branch(elim_fun fun_name t1, elim_fun fun_name t2)
      | Let(flag, bindings, t2) ->
          let aux (f,xs,t) =
            let fun_name' = "f_" ^ Id.name f in
              f, xs, elim_fun fun_name' t
          in
          let bindings' = List.map aux bindings in
          let t2' = elim_fun fun_name t2 in
            Let(flag, bindings', t2')
      | BinOp(op, t1, t2) -> BinOp(op, elim_fun fun_name t1, elim_fun fun_name t2)
      | Not t1 -> Not (elim_fun fun_name t1)
      | Event(s,b) -> Event(s,b)
      | Record fields ->  Record (List.map (fun (f,(s,t1)) -> f,(s,elim_fun fun_name t1)) fields)
      | Proj(i,s,f,t1) -> Proj(i,s,f,elim_fun fun_name t1)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,elim_fun fun_name t1,elim_fun fun_name t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(elim_fun fun_name t1, elim_fun fun_name t2)
      | Constr(s,ts) -> Constr(s, List.map (elim_fun fun_name) ts)
      | Match(t1,pats) ->
          let aux (pat,cond,t1) = pat, elim_fun fun_name cond, elim_fun fun_name t1 in
            Match(elim_fun fun_name t1, List.map aux pats)
      | Raise t -> Raise (elim_fun fun_name t)
      | TryWith(t1,t2) -> TryWith(elim_fun fun_name t1, elim_fun fun_name t2)
      | Pair(t1,t2) -> Pair(elim_fun fun_name t1, elim_fun fun_name t2)
      | Fst t -> Fst(elim_fun fun_name t)
      | Snd t -> Snd(elim_fun fun_name t)
      | Bottom -> Bottom
      | Label _ -> assert false
  in
    {desc=desc; typ=t.typ}

let elim_fun t = elim_fun "f" t





let rec make_ext_env funs t =
  match t.desc with
      Const c -> []
    | Unknown -> []
    | RandInt _ -> []
    | RandValue _ -> []
    | Var x -> if List.mem x funs then [x, Id.typ x] else []
    | App(t, ts) -> make_ext_env funs t @@@ (rev_map_flatten (make_ext_env funs) ts)
    | If(t1, t2, t3) -> make_ext_env funs t1 @@@ make_ext_env funs t2 @@@ make_ext_env funs t3
    | Branch(t1, t2) -> make_ext_env funs t1 @@@ make_ext_env funs t2
    | Let(flag, bindings, t2) ->
        let aux fv (_,xs,t) = make_ext_env funs t @@@ fv in
          List.fold_left aux (make_ext_env funs t2) bindings
    | BinOp(op, t1, t2) -> make_ext_env funs t1 @@@ make_ext_env funs t2
    | Not t -> make_ext_env funs t
    | Fun(x,t) -> make_ext_env funs t
    | Event(s,_) -> []
    | Record fields -> List.fold_left (fun acc (_,(_,t)) -> make_ext_env funs t @@@ acc) [] fields
    | Proj(_,_,_,t) -> make_ext_env funs t
    | SetField(_,_,_,_,t1,t2) -> make_ext_env funs t1 @@@ make_ext_env funs t2
    | Nil -> []
    | Cons(t1, t2) -> make_ext_env funs t1 @@@ make_ext_env funs t2
    | Constr(_,ts) -> List.fold_left (fun acc t -> make_ext_env funs t @@@ acc) [] ts
    | Match(t,pats) ->
        let aux acc (_,_,t) = make_ext_env funs t @@@ acc in
          List.fold_left aux (make_ext_env funs t) pats
    | TryWith(t1,t2) -> make_ext_env funs t1 @@@ make_ext_env funs t2
    | Bottom -> []
    | Pair(t1,t2) -> make_ext_env funs t1 @@@ make_ext_env funs t2
    | Fst t -> make_ext_env funs t
    | Snd t -> make_ext_env funs t
    | Raise t -> make_ext_env funs t
    | Label _ -> assert false

let make_ext_env t =
  let funs = get_fv ~cmp:compare t in
  make_ext_env funs t



let rec init_rand_int t =
  let desc =
    match t.desc with
        Const c -> Const c
      | Unknown -> Unknown
      | Var x -> Var x
      | RandInt false -> assert false
      | App({desc=RandInt false},[{desc=Const Unit}]) -> Var (Id.new_var "_r" TInt)
      | Fun(x,t) -> Fun(x, init_rand_int t)
      | App(t,ts) -> App(init_rand_int t, List.map init_rand_int ts)
      | If(t1,t2,t3) -> If(init_rand_int t1, init_rand_int t2, init_rand_int t3)
      | Branch(t1,t2) -> Branch(init_rand_int t1, init_rand_int t2)
      | Let(flag,bindings,t2) ->
          let bindings' = List.map (fun (f,xs,t) -> f,xs,init_rand_int t) bindings in
            Let(flag, bindings', init_rand_int t2)
      | BinOp(op, t1, t2) -> BinOp(op, init_rand_int t1, init_rand_int t2)
      | Not t -> Not (init_rand_int t)
      | Event(s,b) -> Event(s,b)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(init_rand_int t1, init_rand_int t2)
      | Constr(s,ts) -> Constr(s, List.map init_rand_int ts)
      | Match(t,pats) ->
          Match(init_rand_int t, List.map (fun (pat,cond,t) -> pat, init_rand_int cond,init_rand_int t) pats)
      | Bottom -> Bottom
      | Snd _ -> assert false
      | Fst _ -> assert false
      | Pair (_, _) -> assert false
      | TryWith (_, _) -> assert false
      | Raise _ -> assert false
      | SetField (_, _, _, _, _, _) -> assert false
      | Proj (_, _, _, _) -> assert false
      | Record _ -> assert false
      | RandValue (_, _) -> assert false
      | RandInt _ -> assert false
      | Label _ -> assert false
  in
    {desc=desc; typ=t.typ}



let rec inlined_f inlined fs t =
  let desc =
    match t.desc with
        Const c -> Const c
      | Unknown -> Unknown
      | RandInt b -> RandInt b
      | RandValue(typ,b) -> RandValue(typ,b)
      | Var y ->
          if List.exists (fun (x, _, _) -> Id.same x y) fs then
            let (f, xs, t') = try List.find (fun (x, _, _) -> Id.same x y) fs with Not_found -> assert false in
              (*let _ = List.iter (fun (x, t) -> Format.printf "%a -> %a@." print_id x pp_print_term t) [f, t'] in*)
            let f, _ =
              List.fold_left
                (fun (f, ty) y ->
                   (fun t ->
                      f {desc=Fun(y, t); typ=ty}),
                   match ty with
                       Type.TFun(_, ty') -> ty'
                     | _ ->
                         let _ = Format.printf "%a@." print_typ ty in assert false)
                ((fun t -> t), t.typ)
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
               Var f when List.exists (fun (f', _, _) -> Id.same f f') fs ->
                 let (f, xs, t) = try List.find (fun (f', _, _) -> Id.same f f') fs with Not_found -> assert false in
                 let ts = List.map (inlined_f inlined fs) ts in
                 let ys = List.map (fun t -> match t.desc with Const (Unit | True | False | Int _) | Var _ -> `L(t) | _ -> `R(Id.new_var ("arg" ^ gen_id ()) t.typ)) ts in
                 let ys1, ys2 = if List.length ys <= List.length xs then ys, [] else Fpat.Util.List.split_nth (List.length xs) ys in
                 let xs1, xs2 = Fpat.Util.List.split_nth (List.length ys1) xs in
                 let map = List.map2 (fun x y -> match y with `L(t) -> x, t | `R(y) -> x, make_var y) xs1 ys1 in
                 let t' = subst_map map t in
                 let f, _ =
                   List.fold_left
                     (fun (f, ty) x -> (fun t -> f {desc=Fun(x, t); typ=ty}), match ty with Type.TFun(_, ty') -> ty' | _ -> assert false)
                     ((fun t -> t), Type.app_typ t1.typ (List.map (fun t -> t.typ) ts))
                     xs2
                 in
                 let bindings = Fpat.Util.List.filter_map2 (fun y t -> match y with `L(_) -> None | `R(y) -> Some(y, [], t)) ys ts in
                   (make_lets bindings (make_app (f t') (List.map (fun y -> match y with `L(t) -> t | `R(y) -> make_var y) ys2))).desc
             | _ ->
                 let t1' = inlined_f inlined fs t1 in
                 let ts' = List.map (inlined_f inlined fs) ts in
                   App(t1', ts'))
      | If(t1, t2, t3) -> If(inlined_f inlined fs t1, inlined_f inlined fs t2, inlined_f inlined fs t3)
      | Branch(t1, t2) -> Branch(inlined_f inlined fs t1, inlined_f inlined fs t2)
      | Let(flag, bindings, t2) ->
          let aux (f,xs,t) =
            (*let _ = List.iter (fun f -> Format.printf "f: %a@." print_id f) inlined in*)
            let rec lift t =
              match t.desc with
                  Fun(x, t') ->
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
                else if xs = [] && (match t.desc with Fst(t) | Snd(t) -> (match t.desc with Var _ -> true | _ -> false) | _ -> false) then
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
      | Proj(i,s,f,t1) -> Proj(i,s,f,inlined_f inlined fs t1)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,inlined_f inlined fs t1,inlined_f inlined fs t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(inlined_f inlined fs t1, inlined_f inlined fs t2)
      | Constr(s,ts) -> Constr(s, List.map (inlined_f inlined fs) ts)
      | Match(t1,pats) ->
          let aux (pat,cond,t1) = pat, inlined_f inlined fs cond, inlined_f inlined fs t1 in
            Match(inlined_f inlined fs t1, List.map aux pats)
      | Raise t -> Raise (inlined_f inlined fs t)
      | TryWith(t1,t2) -> TryWith(inlined_f inlined fs t1, inlined_f inlined fs t2)
      | Pair(t1,t2) -> Pair(inlined_f inlined fs t1, inlined_f inlined fs t2)
      | Fst t ->
          let t' = inlined_f inlined fs t in
            begin
              match t'.desc with
                  Pair(t1, _) -> t1.desc
                | _ -> Fst t'
            end
      | Snd t ->
          let t' = inlined_f inlined fs t in
            begin
              match t'.desc with
                  Pair(_, t2) -> t2.desc
                | _ -> Snd t'
            end
      | Bottom -> Bottom
      | Label _ -> assert false
          (*
            | _ -> Format.printf "inlined_f: %a@." pp_print_term t; assert false
          *)
  in
    {desc=desc; typ=t.typ}

let inlined_f inlined t = inlined_f inlined [] t




let rec lift_fst_snd fs t =
  let desc =
    match t.desc with
      Const c -> Const c
    | Unknown -> Unknown
    | RandInt b -> RandInt b
    | RandValue(typ,b) -> RandValue(typ,b)
    | Var y -> Var y
    | Fun(y, t1) -> Fun(y, lift_fst_snd fs t1)(* ommit the case where y is a pair *)
    | App(t1, ts) -> App(lift_fst_snd fs t1, List.map (lift_fst_snd fs) ts)
    | If(t1, t2, t3) -> If(lift_fst_snd fs t1, lift_fst_snd fs t2, lift_fst_snd fs t3)
    | Branch(t1, t2) -> Branch(lift_fst_snd fs t1, lift_fst_snd fs t2)
    | Let(flag, bindings, t2) ->
        let bindings' =
          List.map
              (fun (f,xs,t) ->
                  f, xs,
                    let fs' =
                      List.flatten
                          (Fpat.Util.List.filter_map
                              (fun x ->
                                  match x.Id.typ with
                                      TPair(_, _) ->
                                          Some([Id.new_var x.Id.name (fst_typ x.Id.typ), true, x; Id.new_var x.Id.name (snd_typ x.Id.typ), false, x])
                                    | _ -> None)
                              xs)
                    in
                    if fs' = [] then
                      lift_fst_snd fs t
                    else
                      make_lets
                            (List.map
                                (fun (x, bfst, xorig) ->
                                    (* ommit the case where x is a pair *)
                                    x, [], if bfst then { desc = Fst(make_var xorig); typ = x.Id.typ} else { desc = Snd(make_var xorig); typ = x.Id.typ})
                                fs')
                            (lift_fst_snd (fs @ fs') t)
                    (* ommit the case where f is a pair *))
                bindings
        in
        Let(flag, bindings', lift_fst_snd fs t2)
    | BinOp(op, t1, t2) -> BinOp(op, lift_fst_snd fs t1, lift_fst_snd fs t2)
    | Not t1 -> Not (lift_fst_snd fs t1)
    | Event(s,b) -> Event(s,b)
    | Record fields ->  Record (List.map (fun (f,(s,t1)) -> f,(s,lift_fst_snd fs t1)) fields)
    | Proj(i,s,f,t1) -> Proj(i,s,f,lift_fst_snd fs t1)
    | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,lift_fst_snd fs t1,lift_fst_snd fs t2)
    | Nil -> Nil
    | Cons(t1,t2) -> Cons(lift_fst_snd fs t1, lift_fst_snd fs t2)
    | Constr(s,ts) -> Constr(s, List.map (lift_fst_snd fs) ts)
    | Match(t1,pats) ->
        let aux (pat,cond,t1) = pat, lift_fst_snd fs cond, lift_fst_snd fs t1 in
          Match(lift_fst_snd fs t1, List.map aux pats)
    | Raise t -> Raise (lift_fst_snd fs t)
    | TryWith(t1,t2) -> TryWith(lift_fst_snd fs t1, lift_fst_snd fs t2)
    | Pair(t1,t2) -> Pair(lift_fst_snd fs t1, lift_fst_snd fs t2)
    | Fst t ->
              (match t.desc with
                    Var(x) ->
                        (try
                          let (x, _, _) = List.find (fun (_, bfst, x') -> bfst && Id.same x' x) fs in
                              (make_var x).desc
                          with Not_found ->
                            Fst(lift_fst_snd fs t))
                  | _ ->
                  Fst(lift_fst_snd fs t))
    | Snd t ->
              (match t.desc with
                    Var(x) ->
                        (try
                          let (x, _, _) = List.find (fun (_, bfst, x') -> not bfst && Id.same x' x) fs in
                              (make_var x).desc
                          with Not_found ->
                            Snd(lift_fst_snd fs t))
                  | _ ->
                  Snd(lift_fst_snd fs t))
    | Bottom -> Bottom
    | Label _ -> assert false
(*
    | _ -> Format.printf "lift_fst_snd: %a@." pp_print_term t; assert false
*)
  in
    {desc=desc; typ=t.typ}

let lift_fst_snd t = lift_fst_snd [] t





(* t is assumed to be a CBN-program *)
let rec expand_let_val t =
  let desc =
    match t.desc with
        Const c -> Const c
      | Unknown -> Unknown
      | RandInt b -> RandInt b
      | RandValue(typ,b) -> RandValue(typ,b)
      | Var y -> Var y
      | Fun(y, t) -> Fun(y, expand_let_val t)
      | App(t1, ts) -> App(expand_let_val t1, List.map expand_let_val ts)
      | If(t1, t2, t3) -> If(expand_let_val t1, expand_let_val t2, expand_let_val t3)
      | Branch(t1, t2) -> Branch(expand_let_val t1, expand_let_val t2)
      | Let(Recursive, bindings, t2) ->
          let bindings' = List.map (fun (f,xs,t) -> f, xs, expand_let_val t) bindings in
          let t2' = expand_let_val t2 in
            Let(Recursive, bindings', t2')
      | Let(flag, bindings, t2) ->
          let bindings' = List.map (fun (f,xs,t) -> f, xs, expand_let_val t) bindings in
          let t2' = expand_let_val t2 in
          let bindings1,bindings2 = List.partition (fun (_,xs,_) -> xs = []) bindings' in
          let t2'' = List.fold_left (fun t (f,_,t') -> subst f t' t) t2' bindings1 in
            if bindings2 = []
            then t2''.desc
            else Let(flag, bindings2, t2'')
      | BinOp(op, t1, t2) -> BinOp(op, expand_let_val t1, expand_let_val t2)
      | Not t1 -> Not (expand_let_val t1)
      | Event(s,b) -> Event(s,b)
      | Record fields ->  Record (List.map (fun (f,(s,t1)) -> f,(s,expand_let_val t1)) fields)
      | Proj(i,s,f,t1) -> Proj(i,s,f,expand_let_val t1)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,expand_let_val t1,expand_let_val t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(expand_let_val t1, expand_let_val t2)
      | Constr(s,ts) -> Constr(s, List.map expand_let_val ts)
      | Match(t1,pats) ->
          let aux (pat,cond,t1) = pat, expand_let_val cond, expand_let_val t1 in
            Match(expand_let_val t1, List.map aux pats)
      | Raise t -> Raise (expand_let_val t)
      | TryWith(t1,t2) -> TryWith(expand_let_val t1, expand_let_val t2)
      | Pair(t1,t2) -> Pair(expand_let_val t1, expand_let_val t2)
      | Fst t -> Fst(expand_let_val t)
      | Snd t -> Snd(expand_let_val t)
      | Bottom -> Bottom
      | Label _ -> assert false
  in
    {desc=desc; typ=t.typ}


let simplify_match = make_trans ()

let simplify_match_desc desc =
  match desc with
    Match(t1,pats) ->
      let aux (pat,cond,t1) = pat, simplify_match.tr_term cond, simplify_match.tr_term t1 in
      let pats' = List.map aux pats in
      let rec elim_unused = function
          [] -> []
        | (({pat_desc=PAny|PVar _}, cond, t) as pct)::_ when cond = true_term -> [pct]
        | pct::pats -> pct :: elim_unused pats
      in
      let pats'' = elim_unused pats' in
        begin
          match pats'' with
              [] -> assert false
            | [{pat_desc=PAny}, cond, t] when cond = true_term ->
                let x = Id.new_var "u" t1.typ in
                assert (cond = true_term);
                (make_let [x, [], t1] t).desc
            | [{pat_desc=PVar x}, cond, t] when cond = true_term ->
                (make_let [x, [], t1] t).desc
            | _ -> Match(simplify_match.tr_term t1, pats'')
        end
  | _ -> simplify_match.tr_desc_rec desc

let () = simplify_match.tr_desc <- simplify_match_desc

let simplify_match = simplify_match.tr_term



let should_insert typs = List.for_all (function TFun _ -> true | _ -> false) typs

(* Insert extra parameters into functions with only function arguments.
   Input must be CPS *)

let insert_param_funarg = make_trans ()

let insert_param_funarg_typ typ =
  match typ with
    TFun _ as typ ->
      let xs,typ' = decomp_tfun typ in
      let xs' = List.map insert_param_funarg.tr_var xs in
      let xs'' =
        if should_insert (List.map Id.typ xs)
        then (Id.new_var "u" TUnit) :: xs'
        else xs'
      in
        List.fold_right (fun x typ -> TFun(x,typ)) xs'' (insert_param_funarg.tr_typ typ')
  | _ -> insert_param_funarg.tr_typ_rec typ

let insert_param_funarg_term t =
  let typ = insert_param_funarg_typ t.typ in
  let desc =
    match t.desc with
      Fun _ ->
        let xs,t' = decomp_fun t in
        let xs' = List.map insert_param_funarg.tr_var xs in
        let xs'' =
          if should_insert (List.map Id.typ xs)
          then (Id.new_var "u" TUnit) :: xs'
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
            if should_insert (List.map Id.typ xs)
            then Id.new_var "u" TUnit :: xs'
            else xs'
          in
            insert_param_funarg.tr_var f, xs'', insert_param_funarg.tr_term t
        in
          Let(flag, List.map aux defs, insert_param_funarg.tr_term t)
    | _ -> insert_param_funarg.tr_desc_rec t.desc
  in
    {desc=desc; typ=typ}

let () = insert_param_funarg.tr_typ <- insert_param_funarg_typ
let () = insert_param_funarg.tr_term <- insert_param_funarg_term

let insert_param_funarg = insert_param_funarg.tr_term




















let rec search_fail path t =
  match t.desc with
    Const c -> []
  | Unknown -> []
  | RandInt b -> []
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
  | Branch(t1, t2) -> search_fail (1::path) t1 @ search_fail (2::path) t2
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
  | Proj(i,s,f,t) -> assert false
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
    let ts = rev_flatten_map (fun (_,cond,t) -> [t;cond]) pats in
    aux [] 0 (t::ts)
  | Raise t -> search_fail path t
  | TryWith(t1,t2) -> search_fail (1::path) t1 @ search_fail (2::path) t2
  | Bottom -> []
  | Pair(t1,t2) -> search_fail (1::path) t1 @ search_fail (2::path) t2
  | Fst t -> search_fail path t
  | Snd t -> search_fail path t
  | RandValue _ -> []
  | Label(_,t) -> search_fail path t

let search_fail t = search_fail [] t


let rec screen_fail path target t =
  let desc =
    match t.desc with
        Const c -> t.desc
      | Unknown -> t.desc
      | RandInt b -> t.desc
      | Var x -> t.desc
      | Fun(x,t) -> t.desc
      | App(t1, ts) ->
          let aux i t = screen_fail (i::path) target t in
          let t1ts' = mapi aux (t1::ts) in
          App(List.hd t1ts', List.tl t1ts')
      | If(t1, t2, t3) ->
          let aux i t = screen_fail (i::path) target t in
          If(aux 1 t1, aux 2 t2, aux 3 t3)
      | Branch(t1, t2) ->
          let aux i t = screen_fail (i::path) target t in
          Branch(aux 1 t1, aux 2 t2)
      | Let(flag, defs, t) ->
          let aux i t = screen_fail (i::path) target t in
          let aux_def i (f,xs,t) = f, xs, aux i t in
          Let(flag, mapi aux_def defs, aux (List.length defs) t)
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
      | Proj(i,s,f,t) -> assert false
      | SetField(n,i,s,f,t1,t2) -> assert false
      | Nil -> t.desc
      | Cons(t1,t2) ->
          let aux i t = screen_fail (i::path) target t in
          Cons(aux 1 t1, aux 2 t2)
      | Constr(s,ts) ->
          let aux i t = screen_fail (i::path) target t in
          Constr(s, mapi aux ts)
      | Match(t,pats) ->
          let aux i t = screen_fail (i::path) target t in
          let aux_pat i (p,cond,t) = p, aux (2*i+1) cond, aux (2*i+2) t in
          Match(aux 0 t, mapi aux_pat pats)
      | Raise t -> Raise (screen_fail path target t)
      | TryWith(t1,t2) ->
          let aux i t = screen_fail (i::path) target t in
          TryWith(aux 1 t1, aux 2 t2)
      | Bottom -> t.desc
      | Pair(t1,t2) ->
          let aux i t = screen_fail (i::path) target t in
          Pair(aux 1 t1, aux 2 t2)
      | Fst t -> Fst (screen_fail path target t)
      | Snd t -> Snd (screen_fail path target t)
      | RandValue _ -> t.desc
      | Label(info,t) -> Label(info, screen_fail path target t)
  in
  {desc=desc; typ=t.typ}

let screen_fail target t = screen_fail [] target t





let rec rename_ext_funs_list funs map ts =
  let aux t (map,ts) =
    let map',t' = rename_ext_funs funs map t in
    map', t'::ts
  in
  List.fold_right aux ts (map,[])

and rename_ext_funs funs map t =
  let map',desc =
    match t.desc with
        Const _
      | Unknown
      | RandInt _ -> map, t.desc
      | Var x when Id.mem x funs ->
          begin
            try
              let x' = List.find (fun f' -> Type.can_unify (Id.typ f') (Id.typ x)) map in
              map, Var x'
            with Not_found ->
              let x' = Id.new_var_id x in
              x'::map, Var x'
          end
      | Var x -> map, Var x
      | Fun(x, t) ->
          let map',t' = rename_ext_funs funs map t in
          map', Fun(x, t')
      | App(t, ts) ->
          let map',ts' = rename_ext_funs_list funs map ts in
          let map'',t' = rename_ext_funs funs map' t in
          map'', App(t',ts')
      | If(t1, t2, t3) ->
          let map1,t1' = rename_ext_funs funs map t1 in
          let map2,t2' = rename_ext_funs funs map1 t2 in
          let map3,t3' = rename_ext_funs funs map2 t3 in
          map3, If(t1', t2', t3')
      | Branch(t1, t2) ->
          let map1,t1' = rename_ext_funs funs map t1 in
          let map2,t2' = rename_ext_funs funs map1 t2 in
          map2, Branch(t1', t2')
      | Let(flag, bindings, t2) ->
          let aux (g,xs,t) (map,bindings) =
            let map',t' = rename_ext_funs funs map t in
            map', (g,xs,t')::bindings
          in
          let map',bindings' = List.fold_right aux bindings (map,[]) in
          let map'',t2' = rename_ext_funs funs map' t2 in
          map'', Let(flag, bindings', t2')
      | BinOp(op, t1, t2) ->
          let map1,t1' = rename_ext_funs funs map t1 in
          let map2,t2' = rename_ext_funs funs map1 t2 in
          map2, BinOp(op, t1', t2')
      | Not t ->
          let map',t' = rename_ext_funs funs map t in
          map', Not t'
      | Event(s,b) -> map, Event(s,b)
      | Record fields -> assert false
      | Proj(i,s,f,t) -> assert false
      | SetField(n,i,s,f,t1,t2) -> assert false
      | Nil -> map, Nil
      | Cons(t1,t2) ->
          let map1,t1' = rename_ext_funs funs map t1 in
          let map2,t2' = rename_ext_funs funs map1 t2 in
          map2, Cons(t1', t2')
      | Constr(s,ts) ->
          let map',ts' = rename_ext_funs_list funs map ts in
          map', Constr(s, ts')
      | Match(t,pats) ->
          let aux (p,c,t) (map,bindings) =
            let map',t' = rename_ext_funs funs map t in
            map', (p,c,t')::bindings
          in
          let map',pats' = List.fold_right aux pats (map,[]) in
          let map'',t' = rename_ext_funs funs map' t in
          map'', Match(t', pats')
      | Raise t ->
          let map',t' = rename_ext_funs funs map t in
          map', Raise t'
      | TryWith(t1,t2) ->
          let map1,t1' = rename_ext_funs funs map t1 in
          let map2,t2' = rename_ext_funs funs map1 t2 in
          map2, TryWith(t1', t2')
      | Bottom -> map, Bottom
      | Pair(t1,t2) ->
          let map1,t1' = rename_ext_funs funs map t1 in
          let map2,t2' = rename_ext_funs funs map1 t2 in
          map2, Pair(t1', t2')
      | Fst t ->
          let map',t' = rename_ext_funs funs map t in
          map', Fst t'
      | Snd t ->
          let map',t' = rename_ext_funs funs map t in
          map', Snd t'
      | RandValue (_, _) -> assert false
      | Label _ -> assert false
  in
  map', {desc=desc; typ=t.typ}
let rename_ext_funs funs t = rename_ext_funs funs [] t

let make_ext_fun_def f =
  let xs,typ' = decomp_tfun (Id.typ f) in
  let xs' = List.map Id.new_var_id xs in
  let make_fun_arg_call f (env,defs,t) =
    let xs,typ = decomp_tfun (Id.typ f) in
    let aux typ (env,defs,args) =
      let env',defs',arg = inst_randvalue env defs typ in
      env', defs', arg::args
    in
    let env',defs',args = List.fold_right aux (List.map Id.typ xs) (env,defs,[]) in
    if xs = []
    then env',defs',t
    else
      let u = Id.new_var "u" TUnit in
      let x = Id.new_var "x" typ in
      let t' = make_if randbool_unit_term unit_term (make_let [x,[],make_app (make_var f) args] unit_term) in
      env', defs', make_let [u,[],t'] t
  in
  let env,defs,t = inst_randvalue [] [] typ' in
  let _,defs',t' = List.fold_right make_fun_arg_call xs' (env,defs,t) in
  f, xs', make_letrec defs' t'

let make_ext_funs t =
  let funs = get_fv t in
  if List.exists (fun x -> is_poly_typ (Id.typ x)) funs
  then raise (Fatal "Not implemented: Trans.make_ext_funs funs");
  let map,t' = rename_ext_funs funs t in
  let defs = List.map make_ext_fun_def map in
  make_let defs t'


let rec assoc_typ f t =
  match t.desc with
      Const _ -> []
    | Unknown -> []
    | RandInt _ -> []
    | RandValue _ -> []
    | Var _ -> []
    | Fun(_, t) -> assoc_typ f t
    | App(t1, ts) -> assoc_typ f t1 @@@ rev_flatten_map (assoc_typ f) ts
    | If(t1, t2, t3) -> assoc_typ f t1 @@@ assoc_typ f t2 @@@ assoc_typ f t3
    | Branch(t1, t2) -> assoc_typ f t1 @@@ assoc_typ f t2
    | Let(flag, bindings, t1) ->
        let aux (g,_,t) =
          let typs1 = if Id.same f g then [Id.typ g] else [] in
            typs1 @@@ assoc_typ f t
        in
          assoc_typ f t1 @@@ rev_flatten_map aux bindings
    | BinOp(_, t1, t2) -> assoc_typ f t1 @@@ assoc_typ f t2
    | Not t1 -> assoc_typ f t1
    | Event _ -> []
    | Record fields -> rev_flatten_map (fun (_,(_,t1)) -> assoc_typ f t1) fields
    | Proj(_,_,_,t1) -> assoc_typ f t1
    | SetField(_,_,_,_,t1,t2) -> assoc_typ f t1 @@@ assoc_typ f t2
    | Nil -> []
    | Cons(t1,t2) -> assoc_typ f t1 @@@ assoc_typ f t2
    | Constr(s,ts) -> rev_flatten_map (assoc_typ f) ts
    | Match(t1,pats) ->
        let aux (_,cond,t) = assoc_typ f cond @@@ assoc_typ f t in
          assoc_typ f t1 @@@ rev_flatten_map aux pats
    | Raise t -> assoc_typ f t
    | TryWith(t1,t2) -> assoc_typ f t1 @@@ assoc_typ f t2
    | Pair(t1,t2) -> assoc_typ f t1 @@@ assoc_typ f t2
    | Fst t -> assoc_typ f t
    | Snd t -> assoc_typ f t
    | Bottom -> []
    | Label(_,t) -> assoc_typ f t

let assoc_typ f t =
  let typs = assoc_typ f t in
    match typs with
        [] -> raise Not_found
      | [typ] -> typ
      | _ -> Format.printf "VAR:%a@.PROG:%a@." Id.print f pp_print_term t; assert false





let let2fun = make_trans ()

let let2fun_desc desc =
  match desc with
    Let(flag, bindings, t2) ->
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
    Let(flag, bindings, t2) ->
      let aux (f,xs,t) =
        let ys,t' = decomp_fun t in
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
    Let(Nonrecursive, [x,[],t], {desc=Var y}) when x = y ->
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
    App(t1, [t2]) ->
      let t1' = beta_no_effect.tr_term t1 in
      let t2' = beta_no_effect.tr_term t2 in
      begin
        match t1'.desc with
          Fun(x,t1'') when has_no_effect t2' -> (subst x t2' t1'').desc
        | _ -> App(t1', [t2'])
      end
  | _ -> beta_no_effect.tr_desc_rec desc

let () = beta_no_effect.tr_desc <- beta_no_effect_desc

let beta_no_effect = beta_no_effect.tr_term



let rec diff_terms t1 t2 =
  match t1.desc, t2.desc with
    Const c1, Const c2 -> if c1 = c2 then [] else [t1,t2]
  | Unknown, Unknown -> []
  | RandInt b1, RandInt b2 -> if b1 = b2 then [] else [t1,t2]
  | RandValue(typ1,b1), RandValue(typ2,b2) ->
      if Type.same_shape typ1 typ2 && b1 = b2
      then []
      else [t1,t2]
  | Var x1, Var x2 -> if Id.same x1 x2 then [] else [t1,t2]
  | Fun _, Fun _ -> [t1,t2]
  | App(t11,[t12]), App(t21,[t22]) -> diff_terms t11 t21 @ diff_terms t12 t22
  | App(t1,ts1), App(t2,ts2) ->
      let ts1',t12 = decomp_snoc ts1 in
      let ts2',t22 = decomp_snoc ts2 in
      let t1' = {desc=App(make_app t1 ts1', [t12]); typ=t1.typ} in
      let t2' = {desc=App(make_app t2 ts2', [t22]); typ=t2.typ} in
      diff_terms t1' t2'
  | If(t11,t12,t13), If(t21,t22,t23) ->
      diff_terms t11 t21 @ diff_terms t12 t22 @ diff_terms t13 t23
  | Branch(t11,t12), Branch(t21,t22) ->
      diff_terms t11 t21 @ diff_terms t12 t22
  | Let(flag1,bindings1,t1), Let(flag2,bindings2,t2) -> [t1,t2]
  | BinOp(op1,t11,t12), BinOp(op2,t21,t22) ->
      if op1 = op2
      then diff_terms t11 t21 @ diff_terms t12 t22
      else [t1,t2]
  | Not t1, Not t2 -> diff_terms t1 t2
  | Event(s1,b1), Event(s2,b2) -> if s1 = s2 && b1 = b2 then [] else [t1,t2]
  | Record _, Record _ -> [t1,t2] (* Not implemented *)
  | Proj _, Proj _ -> [t1,t2] (* Not implemented *)
  | SetField _, SetField _ -> [t1,t2] (* Not implemented *)
  | Nil, Nil -> []
  | Cons(t11,t12), Cons(t21,t22) ->
      diff_terms t11 t21 @ diff_terms t12 t22
  | Constr _, Constr _ -> [t1,t2] (* Not implemented *)
  | Match _, Match _ -> [t1,t2] (* Not implemented *)
  | Raise _, Raise _ -> [t1,t2] (* Not implemented *)
  | TryWith _, TryWith _ -> [t1,t2] (* Not implemented *)
  | Pair(t11,t12), Pair(t21,t22) -> diff_terms t11 t21 @ diff_terms t12 t22
  | Fst t1, Fst t2 -> diff_terms t1 t2
  | Snd t1, Snd t2 -> diff_terms t1 t2
  | Bottom, Bottom -> []
  | Label _, Label _ -> [t1,t2]
  | _ -> [t1, t2]




let subst_let_xy = make_trans ()

let subst_let_xy_desc desc =
  let desc' = subst_let_xy.tr_desc_rec desc in
  match desc with
    Let(Nonrecursive, bindings, t) ->
      let bindings',t' =
        match desc' with
          Let(Nonrecursive, bindings', t') -> bindings', t'
        | _ -> assert false
      in
      let sbst bind t =
        match bind with
          (x, [], ({desc=Var y} as t')) -> subst x t' t
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
    Let(Nonrecursive, [x,[],t1], t2) ->
      let t1' = flatten_let.tr_term t1 in
      let t2' = flatten_let.tr_term t2 in
      begin match t1'.desc with
      | Let _ ->
          let fbindings,t12 = decomp_let t1' in
          let fbindings' = fbindings@[Nonrecursive,[x,[],t12]] in
          List.fold_right (uncurry make_let_f) fbindings' t2'
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
      BinOp _ | App _ | Pair _ | Fst _ | Snd _ ->
        let y = Id.new_var "x" t'.typ in
        make_lets [y,[],t'] @@ make_var y
    | _ -> t'
  in
  match t.desc with
    Var x -> x, post
  | _ ->
     let x = Id.new_var "x" t.typ in
     let t' = normalize_let.tr_term t in
     let post' t'' = make_let [x,[],t'] @@ post t'' in
     x, post'

let normalize_let_term t =
  match t.desc with
  | BinOp(op,t1,t2) ->
      let x1,post1 = normalize_let_aux t1 in
      let x2,post2 = normalize_let_aux t2 in
      post1 @@ post2 @@ {desc=BinOp(op, make_var x1, make_var x2); typ=t.typ}
  | App(t, ts) ->
     let ts' = List.map normalize_let.tr_term ts in
     let x,post = normalize_let_aux t in
     post @@ make_app (make_var x) ts'
  | Pair(t1, t2) ->
      let x1,post1 = normalize_let_aux t1 in
      let x2,post2 = normalize_let_aux t2 in
      let r = post1 @@ post2 @@ make_pair (make_var x1) (make_var x2)in
      Color.printf Color.Reverse "%a ==> %a@." pp_print_term t pp_print_term r;r
  | Fst t ->
     let x,post = normalize_let_aux t in
     post @@ make_fst @@ make_var x
  | Snd t ->
     let x,post = normalize_let_aux t in
     post @@ make_snd @@ make_var x
  | Let(flag,bindings,t1) ->
      let aux (f,xs,t) = f, xs, normalize_let.tr_term t in
      let bindings' = List.map aux bindings in
      let t1' = normalize_let.tr_term t1 in
      let t1'' =
        match t1.desc with
          BinOp _ | App _ | Pair _ ->
            let x = Id.new_var "x" t1.typ in
            make_let [x,[],t1'] (make_var x)
        | _ -> t1'
      in
      make_let_f flag bindings' t1'
  | _ -> normalize_let.tr_term_rec t

let () = normalize_let.tr_term <- normalize_let_term

let normalize_let = normalize_let.tr_term



let inline_let_var = make_trans ()

let inline_let_var_term t =
  match t.desc with
    Let(Nonrecursive, [x,[],{desc=Var y}], t) ->
      let t' = inline_let_var.tr_term t in
      subst x (make_var y) t'
  | _ -> inline_let_var.tr_term_rec t

let () = inline_let_var.tr_term <- inline_let_var_term

let inline_let_var = inline_let_var.tr_term




let remove_label = make_trans ()

let remove_label_term t =
  match t.desc with
  | Label(_, t) -> remove_label.tr_term t
  | _ -> remove_label.tr_term_rec t

let () = remove_label.tr_term <- remove_label_term

let remove_label = remove_label.tr_term



let decomp_pair_eq = make_trans ()

let decomp_pair_eq_term t =
  match t.desc with
  | BinOp(Eq, t1, t2) ->
      begin match t1.typ with
      | TPair(x,typ2) ->
          let aux t =
            match t with
            | {desc=Var y} -> y, id
            | _ ->
                let y = var_of_term t in
                y, make_let [y,[],t]
          in
          let y1,post1 = aux t1 in
          let y2,post2 = aux t2 in
          let t1 = make_eq (make_fst @@ make_var y1) (make_fst @@ make_var y2) in
          let t2 = make_eq (make_snd @@ make_var y1) (make_snd @@ make_var y2) in
          post2 @@ post1 @@ decomp_pair_eq.tr_term @@ make_and t1 t2
      | _ -> decomp_pair_eq.tr_term_rec t
      end
  | _ -> decomp_pair_eq.tr_term_rec t

let () = decomp_pair_eq.tr_term <- decomp_pair_eq_term

let decomp_pair_eq = decomp_pair_eq.tr_term



let elim_unused_let = make_trans ()

let elim_unused_let_term t =
  match t.desc with
  | Let(Nonrecursive, bindings, t) ->
      let fv = get_fv t in
      let bindings' = List.filter (fun (f,_,_) -> Id.mem f fv) bindings in
      make_let bindings' t
  | _ -> elim_unused_let.tr_term_rec t

let () = elim_unused_let.tr_term <- elim_unused_let_term

let elim_unused_let = elim_unused_let.tr_term
