


open Util
open Syntax



let new_tvar () = TVar (ref None)

let rec expand t = function
    TFun((x,typ1),typ2) ->
      expand (app2app t [Var x]) typ2
  | _ -> t

let rec conj = function
    [] -> False
  | [t] -> t
  | t::ts -> BinOp(And, t, conj ts)





let reduce_trace ps scope =
  let rec aux ps1 t ps2 =
    match t,ps2 with
        t, [] -> t::ps1
      | BinOp(Eq, Var x, Var y), p::ps ->
          if List.mem x scope || List.mem y scope
          then aux (t::ps1) p ps
          else
            let ps1' = List.map (subst x (Var y)) ps1 in
            let ps' = List.map (subst x (Var y)) ps in
              aux ps1' p ps'
      | _, p::ps -> aux (t::ps1) p ps
  in
    aux [] (List.hd ps) (List.tl ps)






let rec rename_map map = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | RandInt None -> RandInt None
  | RandInt (Some t) -> RandInt (Some (rename_map map t))
  | Var x -> Var (assoc_exn x map x)
  | Fun(x,t) -> assert false
  | App(t,ts) ->
      let t' = rename_map map t in
      let ts' = List.map (rename_map map) ts in
        App(t', ts')
  | If(t1,t2,t3) ->
      let t1' = rename_map map t1 in
      let t2' = rename_map map t2 in
      let t3' = rename_map map t3 in
        If(t1', t2', t3')
  | Branch(t1,t2) ->
      let t1' = rename_map map t1 in
      let t2' = rename_map map t2 in
        Branch(t1', t2')
  | Let _ -> assert false
  | BinOp(op,t1,t2) ->
      let t1' = rename_map map t1 in
      let t2' = rename_map map t2 in
        BinOp(op, t1', t2')
  | Not t ->
      let t' = rename_map map t in
        Not t'
  | Fail -> Fail
  | Label(b,t) ->
      let t' = rename_map map t in
        Label(b,t')
  | Event s -> Event s
  | Nil -> Nil
  | Cons(t1,t2) -> Cons(rename_map map t1, rename_map map t2)
  | Match(t1,t2,x,y,t3) ->
      Match(rename_map map t1, rename_map map t2, x, y, rename_map map t3)
  | Constr(c,ts) -> Constr(c, List.map (rename_map map) ts)
  | Match_(t,pats) ->
      Match_(rename_map map t, List.map (fun (pat,cond,t) -> pat,apply_opt (rename_map map) cond,rename_map map t) pats)
  | Type_decl(decls,t) -> Type_decl(decls, rename_map map t)

let rec rename_typ = function
    TUnit -> [], TUnit
  | TBool -> [], TBool
  | TAbsBool -> [], TAbsBool
  | TInt _ -> [], TInt []
  | TRInt p -> [], TRInt p
  | TVar _ -> assert false
  | TFun((x,typ1),typ2) ->
      let map1,typ1' = rename_typ typ1 in
      let x' = {(new_var_id x) with typ=typ1'} in
      let map2,typ2' = rename_typ typ2 in
        (x,x')::map1@@map2, TFun((x',typ1'),typ2')
  | TList(typ,_) ->
      let map,typ' = rename_typ typ in
        map, TList(typ',[])
  | TVariant ctypss ->
      let aux1 (c,typs) (map,ctypss) =
        let aux2 typ (map,typs) =
          let map',typ' = rename_typ typ in
            map'@@map, typ'::typs
        in
        let map',typs' = List.fold_right aux2 typs (map,[]) in
          map', (c,typs')::ctypss
      in
      let map,ctypss' = List.fold_right aux1 ctypss ([],[]) in
        map, TVariant ctypss'
  | TRecord _ -> assert false
  | TConstr(s,true) -> [], TConstr(s,true)
  | TUnknown -> [], TUnknown

let rename defs t =
  let aux (f,(xs,t)) =
    let map, typ = rename_typ f.typ in
    let f' = {f with typ=typ} in
    let xs' = List.map (fun x -> assoc_exn x map x) xs in
    let t' = rename_map map t in
      (f,f'), (f',(xs',t'))
  in
  let map,defs' = List.split (List.map aux defs) in
  let aux t = List.fold_left (fun t (f,f') -> subst f (Var f') t) t map in
  let defs'' = List.map (fun (f,(xs,t)) -> f,(xs,aux t)) defs' in
  let t' = aux t in
  let map' = rev_map_flatten (fun (f,f') -> List.combine (get_argvars f.typ) (get_argvars f'.typ)) map in
    map',aux,defs'',t'







let rec get_subterm = function
    True -> [True]
  | False -> [False]
  | BinOp((And|Or), t1, t2) ->
      get_subterm t1 @@ get_subterm t2
  | BinOp((Eq|Lt|Gt|Leq|Geq) as op, t1, t2) ->
      [BinOp(op, t1, t2)]
  | Not(BinOp((And|Or), t1, t2)) ->
      get_subterm (Not t1) @@ get_subterm (Not t2)
  | Not t -> [Not t]
  | _ -> assert false


let rec add_preds_typ sol typ1 typ2 =
  (*
    Format.printf "%a and " (print_typ ML) typ1;
    Infer.print_rty typ2;
    Format.printf "@.";
  *)
  match typ1, typ2 with
      TUnit, Infer.RTunit _ -> TUnit
    | TAbsBool, Infer.RTbool _ -> TAbsBool
    | TBool, Infer.RTbool _ -> TBool
    | TInt ps, Infer.RTint _ -> TInt ps (***)
    | TRInt _, _ -> assert false
    | TVar _, _ -> assert false
    | TFun((x,TInt ps),rtyp1), Infer.RTifun(pred, rtyp2) ->
        let Infer.Pred(pid, terms) = pred (Var abst_var) in
        let typ =
          try
            let p =
              let ids, t = List.assoc pid sol in
                (*Format.printf "%a,%a,%a@." (print_term_fm ML true) t (print_ids) ids  (print_termlist ML 0 false) terms; *)
                subst_term (List.combine ids terms) t
            in
            let ps =
              let aux ps p =
                if List.exists (Wrapper.equiv [] p) ps ||
                  List.exists (Wrapper.equiv [] (Not p)) ps ||
                  Wrapper.equiv [] p True || Wrapper.equiv [] p False
                then ps
                else ((*Format.printf "adding %a@." (print_term_fm ML true) p;*) p::ps)
              in
              let p' = Syntax.merge_geq_leq (Syntax.normalize_bool_exp p) in
                List.fold_left aux ps (if Flag.use_subterm then get_subterm p' else [p'])
            in
              TInt ps
          with Not_found -> (*assert false*)
            TInt ps
        in
        let rtyp = add_preds_typ sol rtyp1 (rtyp2 (Var x)) in
          TFun(({x with typ = typ}, typ), rtyp)
    | TFun((x,TList(typ11,ps)),rtyp1), Infer.RTlfun(pred, rtyp2) ->
        let Infer.Pred(pid, terms) = pred (Var abst_list_var) in
        let typ =
          try
            let p =
              let ids, t = List.assoc pid sol in
                (*Format.printf "%a,%a,%a@." (print_term_fm ML true) t (print_ids) ids  (print_termlist ML 0 false) terms; *)
                subst_term (List.combine ids terms) t
            in
            let ps =
              let aux ps p =
                if List.exists (Wrapper.equiv [] p) ps ||
                  List.exists (Wrapper.equiv [] (Not p)) ps ||
                  Wrapper.equiv [] p True || Wrapper.equiv [] p False
                then ps
                else ((*Format.printf "adding %a@." (print_term_fm ML true) p;*) p::ps)
              in
                List.fold_left aux ps (if Flag.use_subterm then get_subterm p else [p])
            in
              TList(typ11,ps)
          with Not_found -> (*assert false*)
            TList(typ11,ps)
        in
        let rtyp = add_preds_typ sol rtyp1 (rtyp2 (Var x)) in
          TFun(({x with typ = typ}, typ), rtyp)
    | TFun((x,TRInt p),rtyp1), Infer.RTifun(pred, rtyp2) ->
        let rtyp = add_preds_typ sol rtyp1 (rtyp2 (Var x)) in
          TFun((x, TRInt p), rtyp)
    | TFun((x,typ),rtyp1), Infer.RTbfun(_, rtyp2) ->
        let rtyp = add_preds_typ sol rtyp1 (rtyp2 (Var x)) in
          TFun((x, typ), rtyp)
    | TFun((x,typ),rtyp1), Infer.RTfun(typs, rtyp2) ->
        let typ = List.fold_left (add_preds_typ sol) typ typs in
        let rtyp = add_preds_typ sol rtyp1 rtyp2 in
          TFun(({x with typ = typ}, typ), rtyp)
    | TUnknown, _ -> TUnknown
    | _, _ ->
        Format.printf "%a and " print_typ typ1;
        Infer.print_rty typ2;
        assert false

let rec add_preds rte sol = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | RandInt None -> RandInt None
  | RandInt (Some t) -> RandInt (Some (add_preds rte sol t))
  | Var x ->
      (try 
        let typ = List.fold_left (add_preds_typ sol) x.typ
          (snd (List.find (fun (x', _) -> x'.id = x.id) rte)) in
        Var({x with typ = typ})
      with Not_found -> Var x)
  | Fun _ -> assert false
  | App(t, ts) ->
      let t' = add_preds rte sol t in
      let ts' = List.map (add_preds rte sol) ts in
        App(t', ts')
  | If(t1, t2, t3) ->
      let t1' = add_preds rte sol t1 in
      let t2' = add_preds rte sol t2 in
      let t3' = add_preds rte sol t3 in
        If(t1', t2', t3')
  | Branch(t1, t2) ->
      let t1' = add_preds rte sol t1 in
      let t2' = add_preds rte sol t2 in
        Branch(t1', t2')
  | Let(Nonrecursive, f, xs, t1, t2) ->
(*
      Format.printf "%a," (print_term_fm ML true) (Var f);
*)
      let typ = List.fold_left (add_preds_typ sol) f.typ
        (try snd (List.find (fun (f', _) -> f'.id = f.id) rte) with Not_found ->
          [](*List.iter (fun (f, _) -> Format.printf "%a," (print_term_fm ML true) (Var f)) rte;
          Format.printf "%a@." (print_term_fm ML true) (Var f); assert false*)) in
      let f' = {f with typ = typ} in
      let xs' = get_args typ in
      let t1' = add_preds rte sol t1 in
      let t2' = add_preds rte sol t2 in
      let t1'' = List.fold_right2 subst xs (List.map (fun x -> Var x) xs') t1' in
      let t2'' = subst f (Var f') t2' in
        Let(Nonrecursive, f', xs', t1'', t2'')
  | Let(Recursive, f, xs, t1, t2) ->
(*
      Format.printf "%a," (print_term_fm ML true) (Var f);
*)
      let typ = List.fold_left (add_preds_typ sol) f.typ
        (try snd (List.find (fun (f', _) -> f'.id = f.id) rte) with Not_found ->
          [](*List.iter (fun (f, _) -> Format.printf "%a," (print_term_fm ML true) (Var f)) rte;
          Format.printf "%a@." (print_term_fm ML true) (Var f); assert false*)) in
      let f' = {f with typ = typ} in
      let xs' = get_args typ in
      let t1' = add_preds rte sol t1 in
      let t2' = add_preds rte sol t2 in
      let t1'' = List.fold_right2 subst (f::xs) (List.map (fun x -> Var x) (f'::xs')) t1' in
      let t2'' = subst f (Var f') t2' in
        Let(Recursive, f', xs', t1'', t2'')
  | BinOp(op, t1, t2) ->
      let t1' = add_preds rte sol t1 in
      let t2' = add_preds rte sol t2 in
        BinOp(op, t1', t2')
  | Not t ->
      let t' = add_preds rte sol t in
        Not t'
  | Fail -> Fail
  | Label _ -> assert false
  | Event s -> Event s
  | Nil -> Nil
  | Cons(t1, t2) -> Cons(add_preds rte sol t1, add_preds rte sol t2)
  | Match(t1,t2,x,y,t3) -> assert false
  | Constr(c,ts) -> Constr(c, List.map (add_preds rte sol) ts)
  | Match_(t,pats) ->
      let aux = (fun (pat,cond,t) -> pat, apply_opt (add_preds rte sol) cond, add_preds rte sol t) in
        Match_(add_preds rte sol t, List.map aux pats)
  | Type_decl(decls,t) -> Type_decl(decls, add_preds rte sol t)

let rec remove_preds_typ typ =
  match typ with
      TUnit -> TUnit
    | TAbsBool -> TAbsBool
    | TBool -> TBool
    | TInt _ -> TInt []
    | TRInt p -> TRInt p
    | TVar _ -> assert false
    | TFun((x,typ1),typ2) ->
        TFun(({x with typ = remove_preds_typ x.typ},remove_preds_typ typ1), remove_preds_typ typ2)
    | TUnknown -> TUnknown

let rec remove_preds = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | Var x -> Var({x with typ = remove_preds_typ x.typ})
  | Fun _ -> assert false
  | App(t, ts) ->
      let t' = remove_preds t in
      let ts' = List.map remove_preds ts in
        App(t', ts')
  | If(t1, t2, t3) ->
      let t1' = remove_preds t1 in
      let t2' = remove_preds t2 in
      let t3' = remove_preds t3 in
        If(t1', t2', t3')
  | Branch(t1, t2) ->
      let t1' = remove_preds t1 in
      let t2' = remove_preds t2 in
        Branch(t1', t2')
  | Let _ -> Format.printf "Not implemented"; assert false
(*
  | Let(f, xs, t1, t2) ->
      let f' = {f with typ = remove_preds_typ f.typ} in
      let xs' = List.map (fun x -> {x with typ = remove_preds_typ x.typ}) xs in
      let t1' = remove_preds t1 in
      let t2' = remove_preds t2 in
        Let(f', xs', t1', t2')
  | Letrec(f, xs, t1, t2) ->
      let f' = {f with typ = remove_preds_typ f.typ} in
      let xs' = List.map (fun x -> {x with typ = remove_preds_typ x.typ}) xs in
      let t1' = remove_preds t1 in
      let t2' = remove_preds t2 in
        Letrec(f', xs', t1', t2')
*)
  | BinOp(op, t1, t2) ->
      let t1' = remove_preds t1 in
      let t2' = remove_preds t2 in
        BinOp(op, t1', t2')
  | Not t ->
      let t' = remove_preds t in
        Not t'
  | Fail -> Fail
  | Label _ -> assert false
  | Event s -> Event s

let refine tdefs ces t0 =
  (*
    let () = Format.printf "%a@." (print_term_fm ML true) t in
  *)
  let defs,t = lift t0 in
    (*
      let () = Format.printf "%a@." (print_term_fm ML true) (List.fold_left (fun acc (f,(xs,t)) -> Letrec(f,xs,t,acc)) t defs) in
    *)
  let map,_,defs',t' = rename defs t in
    (*
      let scmap = get_scmap (List.fold_left (fun acc (f,(xs,t)) -> Letrec(f,xs,t,acc)) t' defs') in
      let () = Format.printf "\n\n%a\n\n@." (print_term_fm ML true) (List.fold_left (fun acc (f,(xs,t)) -> Letrec(f,xs,t,acc)) t' defs') in
    *)

  let s = {id = 0; name = "Main"; typ = TUnit} in
  let defs1 = (s,([], t'))::defs' in
  let rte, sol = Infer.test tdefs s defs1 ces None in
    if Flag.use_dor && List.exists (fun (_, (_, t)) -> get_nint t <> []) sol then begin
      assert (not !Flag.merge_counterexample);
      let _ = Infer.cgen_flag := false in
      let ce = match ces with [ce] -> ce | _ -> assert false in
      let defs,t = lift t0 in
      let pred, ce = Feasibility.check_int ce defs t in
      let rec aux a = function
          [] -> assert false
        | [Syntax.LabNode true] -> a @ [Syntax.EventNode "then_fail"]
        | [Syntax.LabNode false] -> a @ [Syntax.EventNode "else_fail"]
        | [Syntax.PatNode i] -> a @ [Syntax.EventNode ("br" ^ string_of_int i ^ "_fail")]
        | [t] -> a @ [t]
        | t::ce -> aux (a @ [t]) ce
      in
      let ce = aux [] ce in
      let rte, sol = Infer.test tdefs s defs1 [ce] (Some pred) in
      let _ = Infer.cgen_flag := true in
        add_preds rte sol t0
    end else
      add_preds rte sol t0

(*
  let () = Format.printf "AAA@." in
  let constr,map2 = get_constr ce defs' t' in
  let () = Format.printf "BBB@." in
  let solution = solve_constr constr in
  let () = Format.printf "CCCn@." in
  let () = List.iter (fun (c,p) -> Format.printf "%a = [%a]@." print_constr (Pred c) (print_term_fm ML false) p) solution in

  let pred_map =
    let rev_map = List.map (fun (x,x') -> x',x) map in
    let rev_map2 = List.map (fun (x,x') -> x',x) map2 in
    let aux ((_,_,x,_),p) =
      let x' = assoc_exn (assoc_exn x rev_map2 x) rev_map x in
      let p' = List.fold_left (fun p (y,y') -> if x=y then subst y (Var abst_var) p else subst y (Var y') p) p rev_map2 in
      let p'' = List.fold_left (fun p (y,y') -> if x=y then subst y (Var abst_var) p else subst y (Var y') p) p' rev_map in
        x', p''
    in
      List.map aux solution
  in
    add_preds pred_map t0
*)



let rec add_preds_typ_ typ1 typ2 =
(*
  Format.printf "%a and %a@." (print_typ ML) typ1 (print_typ ML) typ2;
*)
  match typ1, typ2 with
      TUnit, TUnit -> TUnit
    | TAbsBool, TAbsBool -> TAbsBool
    | TBool, TBool -> TBool
    | TInt _, _ -> assert false
    | TRInt _, _ -> assert false
    | TVar _, _ -> assert false
    | TFun((x,TInt ps),rtyp1), TFun((y,TInt qs),rtyp2) ->
        let rtyp2 = subst_type y (Var x) rtyp2 in
        let typ =
          try
            let ps =
              let aux ps p =
                if List.exists (Wrapper.equiv [] p) ps ||
                  List.exists (Wrapper.equiv [] (Not p)) ps ||
                  Wrapper.equiv [] p True || Wrapper.equiv [] p False
                then ps
                else ((*Format.printf "adding %a@." (print_term_fm ML true) p;*) p::ps)
              in
              List.fold_left (fun ps p -> aux ps ((*Syntax.normalize_bool_exp*) p)) ps qs
            in
            TInt ps
          with Not_found -> (*assert false*)
            TInt ps
        in
        let rtyp = add_preds_typ_ rtyp1 rtyp2 in
          TFun(({x with typ = typ}, typ), rtyp)
    | TFun((x,TInt _),rtyp1), TFun((y,TRInt p),rtyp2) ->
        let typ = TRInt p in
        let rtyp2 = subst_type y (Var x) rtyp2 in
        let rtyp = add_preds_typ_ rtyp1 rtyp2 in
          TFun(({x with typ = typ}, typ), rtyp)
    | TFun((x,TRInt p),rtyp1), TFun((y,TInt _),rtyp2)
    | TFun((x,TRInt p),rtyp1), TFun((y,TRInt _),rtyp2) ->
        let typ = TRInt p in
        let rtyp2 = subst_type y (Var x) rtyp2 in
        let rtyp = add_preds_typ_ rtyp1 rtyp2 in
          TFun(({x with typ = typ}, typ), rtyp)
    | TFun((x,typ1),rtyp1), TFun((y,typ2),rtyp2) ->
        let rtyp2 = subst_type y (Var x) rtyp2 in
        let typ = add_preds_typ_ typ1 typ2 in
        let rtyp = add_preds_typ_ rtyp1 rtyp2 in
          TFun(({x with typ = typ}, typ), rtyp)
    | TUnknown, _ -> TUnknown
    | _, _ ->
        Format.printf "%a and %a" print_typ typ1 print_typ typ2;
        assert false

let rec add_preds_ typedefs = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | RandInt None -> RandInt None
  | RandInt (Some t) -> RandInt (Some (add_preds_ typedefs t))
  | Var x ->
      let typ = 
        try add_preds_typ_ x.typ (snd (List.find (fun (x', _) -> x'.id = x.id) typedefs))
        with Not_found ->
          x.typ
      in
        Var({x with typ = typ})
  | Fun _ -> assert false
  | App(t, ts) ->
      let t' = add_preds_ typedefs t in
      let ts' = List.map (add_preds_ typedefs) ts in
        App(t', ts')
  | If(t1, t2, t3) ->
      let t1' = add_preds_ typedefs t1 in
      let t2' = add_preds_ typedefs t2 in
      let t3' = add_preds_ typedefs t3 in
        If(t1', t2', t3')
  | Branch(t1, t2) ->
      let t1' = add_preds_ typedefs t1 in
      let t2' = add_preds_ typedefs t2 in
        Branch(t1', t2')
  | Let(flag, f, xs, t1, t2) ->
      if flag = Nonrecursive
      then
        let typ = 
          try add_preds_typ_ f.typ (snd (List.find (fun (f', _) -> f'.id = f.id) typedefs))
          with Not_found ->
            f.typ
        in
        let f' = {f with typ = typ} in
        let xs' = get_args typ in
        let t1' = add_preds_ typedefs t1 in
        let t2' = add_preds_ typedefs t2 in
        let t1'' = List.fold_right2 subst xs (List.map (fun x -> Var x) xs') t1' in
        let t2'' = subst f (Var f') t2' in
          Let(flag, f', xs', t1'', t2'')
      else
        let typ = 
          try add_preds_typ_ f.typ (snd (List.find (fun (f', _) -> f'.id = f.id) typedefs))
          with Not_found ->
            f.typ
        in
        let f' = {f with typ = typ} in
        let xs' = get_args typ in
        let t1' = add_preds_ typedefs t1 in
        let t2' = add_preds_ typedefs t2 in
        let t1'' = List.fold_right2 subst (f::xs) (List.map (fun x -> Var x) (f'::xs')) t1' in
        let t2'' = subst f (Var f') t2' in
          Let(flag, f', xs', t1'', t2'')
  | BinOp(op, t1, t2) ->
      let t1' = add_preds_ typedefs t1 in
      let t2' = add_preds_ typedefs t2 in
        BinOp(op, t1', t2')
  | Not t ->
      let t' = add_preds_ typedefs t in
        Not t'
  | Fail -> Fail
  | Label _ -> assert false
  | Event s -> Event s
  | Record(b,fields) -> Record(b, List.map (fun (f,(s,t)) -> f,(s,add_preds_ typedefs t)) fields)
  | Proj(n,i,f,s,t) -> Proj(n,i,f,s,add_preds_ typedefs t)
  | Nil -> Nil
  | Cons(t1,t2) -> Cons(add_preds_ typedefs t1, add_preds_ typedefs t2)
  | Match(t1,t2,x,y,t3) -> Match(add_preds_ typedefs t1, add_preds_ typedefs t2, x, y, add_preds_ typedefs t3)
  | Constr(c,ts) -> Constr(c, List.map (add_preds_ typedefs) ts)
  | Match_(t,pats) ->
      let aux (pat,cond,t) = pat, apply_opt (add_preds_ typedefs) cond, add_preds_ typedefs t in
        Match_(add_preds_ typedefs t, List.map aux pats)
  | Type_decl(decls,t) -> Type_decl(decls, add_preds_ typedefs t)

