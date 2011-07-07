


open Util
open Syntax
open Type



let new_tvar () = TVar (ref None)

let rec expand t = function
    TFun(x,typ2) ->
      expand (app2app t [make_var x]) typ2
  | _ -> t

let rec conj = function
    [] -> false_term
  | [t] -> t
  | t::ts -> {desc=BinOp(And, t, conj ts); typ=TBool}





let reduce_trace ps scope =
  let rec aux ps1 t ps2 =
    match t.desc,ps2 with
        _, [] -> t::ps1
      | BinOp(Eq, {desc=Var x}, {desc=Var y}), p::ps ->
          if List.mem x scope || List.mem y scope
          then aux (t::ps1) p ps
          else
            let ps1' = List.map (subst x (make_var y)) ps1 in
            let ps' = List.map (subst x (make_var y)) ps in
              aux ps1' p ps'
      | _, p::ps -> aux (t::ps1) p ps
  in
    aux [] (List.hd ps) (List.tl ps)






let rec rename_map map t =
  let desc =
    match t.desc with
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
  in
    {desc=desc; typ=t.typ}

let rec rename_typ = function
    TUnit -> [], TUnit
  | TBool -> [], TBool
  | TAbsBool -> [], TAbsBool
  | TInt _ -> [], TInt []
  | TRInt p -> [], TRInt p
  | TVar _ -> assert false
  | TFun(x,typ2) ->
      let map1,typ1' = rename_typ (Id.typ x) in
      let x' = Id.new_var (Id.name x) typ1' in
      let map2,typ2' = rename_typ typ2 in
        (x,x')::map1@@map2, TFun(x',typ2')
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
    let map, typ = rename_typ (Id.typ f) in
    let f' = Id.set_typ f typ in
    let xs' = List.map (fun x -> assoc_exn x map x) xs in
    let t' = rename_map map t in
      (f,f'), (f',(xs',t'))
  in
  let map,defs' = List.split (List.map aux defs) in
  let aux t = List.fold_left (fun t (f,f') -> subst f (make_var f') t) t map in
  let defs'' = List.map (fun (f,(xs,t)) -> f,(xs,aux t)) defs' in
  let t' = aux t in
  let map' = rev_map_flatten (fun (f,f') -> List.combine (get_argvars (Id.typ f)) (get_argvars (Id.typ f))) map in
    map',aux,defs'',t'







let rec get_subterm t = assert false(*function
    True -> [True]
  | False -> [False]
  | BinOp((And|Or), t1, t2) -> get_subterm t1 @@ get_subterm t2
  | BinOp((Eq|Lt|Gt|Leq|Geq) as op, t1, t2) -> [BinOp(op, t1, t2)]
  | Not({desc=BinOp((And|Or), t1, t2)}) -> get_subterm (Not t1) @@ get_subterm (Not t2)
  | Not t -> [Not t]
  | _ -> assert false*)


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
    | TFun({Id.typ=TInt ps} as x,rtyp1), Infer.RTifun(pred, rtyp2) ->
        let Infer.Pred(pid, terms) = pred (make_var abst_var) in
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
                  List.exists (Wrapper.equiv [] {desc=Not p;typ=TBool}) ps ||
                  Wrapper.equiv [] p true_term || Wrapper.equiv [] p false_term
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
        let rtyp = add_preds_typ sol rtyp1 (rtyp2 (make_var x)) in
          TFun(Id.set_typ x typ, rtyp)
    | TFun({Id.typ=TList(typ11,ps)},rtyp1), Infer.RTlfun(pred, rtyp2) -> assert false(*
        let Infer.Pred(pid, terms) = pred (make_var abst_list_var) in
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
          TFun(({x with typ = typ}, typ), rtyp)*)
    | TFun({Id.typ=TRInt p} as x,rtyp1), Infer.RTifun(pred, rtyp2) ->
        let rtyp = add_preds_typ sol rtyp1 (rtyp2 (make_var x)) in
          TFun(x, rtyp)
    | TFun(x,rtyp1), Infer.RTbfun(_, rtyp2) ->
        let rtyp = add_preds_typ sol rtyp1 (rtyp2 (make_var x)) in
          TFun(x, rtyp)
    | TFun(x,rtyp1), Infer.RTfun(typs, rtyp2) ->
        let typ = List.fold_left (add_preds_typ sol) (Id.typ x) typs in
        let rtyp = add_preds_typ sol rtyp1 rtyp2 in
          TFun(Id.set_typ x typ, rtyp)
    | TUnknown, _ -> TUnknown
    | _, _ ->
        Format.printf "%a and " print_typ typ1;
        Infer.print_rty typ2;
        assert false

let rec add_preds rte sol t =
  let desc =
  match t.desc with
      Unit -> Unit
    | True -> True
    | False -> False
    | Unknown -> Unknown
    | Int n -> Int n
    | NInt x -> NInt x
    | RandInt None -> RandInt None
    | RandInt (Some t) -> RandInt (Some (add_preds rte sol t))
    | Var x ->
        Var
          (try 
             let typ = List.fold_left (add_preds_typ sol) (Id.typ x)
               (snd (List.find (fun (x', _) -> Id.same x' x) rte)) in
               (Id.set_typ x typ)
           with Not_found -> x)
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
    | Let(Flag.Nonrecursive, f, xs, t1, t2) ->
        (*
          Format.printf "%a," (print_term_fm ML true) (Var f);
        *)
        let typ = List.fold_left (add_preds_typ sol) (Id.typ f)
          (try snd (List.find (fun (f', _) -> Id.same f' f) rte) with Not_found ->
             [](*List.iter (fun (f, _) -> Format.printf "%a," (print_term_fm ML true) (Var f)) rte;
                 Format.printf "%a@." (print_term_fm ML true) (Var f); assert false*)) in
        let f' = Id.set_typ f typ in
        let xs' = get_args typ in
        let t1' = add_preds rte sol t1 in
        let t2' = add_preds rte sol t2 in
        let t1'' = List.fold_right2 subst xs (List.map make_var xs') t1' in
        let t2'' = subst f (make_var f') t2' in
          Let(Flag.Nonrecursive, f', xs', t1'', t2'')
    | Let(Flag.Recursive, f, xs, t1, t2) ->
        (*
          Format.printf "%a," (print_term_fm ML true) (Var f);
        *)
        let typ = List.fold_left (add_preds_typ sol) (Id.typ f)
          (try snd (List.find (fun (f', _) -> Id.same f' f) rte) with Not_found ->
             [](*List.iter (fun (f, _) -> Format.printf "%a," (print_term_fm ML true) (Var f)) rte;
                 Format.printf "%a@." (print_term_fm ML true) (Var f); assert false*)) in
        let f' = Id.set_typ f typ in
        let xs' = get_args typ in
        let t1' = add_preds rte sol t1 in
        let t2' = add_preds rte sol t2 in
        let t1'' = List.fold_right2 subst (f::xs) (List.map make_var (f'::xs')) t1' in
        let t2'' = subst f (make_var f') t2' in
          Let(Flag.Recursive, f', xs', t1'', t2'')
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
    | Nil -> assert false
    | Cons(t1, t2) -> assert false
    | Match(t1,t2,x,y,t3) -> assert false
    | Constr(c,ts) -> assert false
    | Match_(t,pats) -> assert false
  in
    {desc=desc; typ=t.typ}


let rec match_typ t =
  match t.desc with
      Unit -> unit_term
    | True -> true_term
    | False -> false_term
    | Unknown -> {desc=Unknown; typ=TBool}
    | Int n -> {desc=Int n; typ=TInt[]}
    | NInt x -> {desc=NInt x; typ=TInt[]}
    | RandInt None -> {desc=RandInt None; typ=TInt[]}
    | RandInt (Some t) ->
        let t' = match_typ t in
          {desc=RandInt (Some t'); typ=t'.typ}
    | Var x -> make_var x
    | Fun _ -> assert false
    | App(t, ts) ->
        let t' = match_typ t in
        let rec aux = function
            [], typ -> [], typ
          | t::ts, TFun(x,typ) ->
              let ts',typ' = aux (ts, typ) in
                {(match_typ t) with typ=Id.typ x}::ts', typ'
          | _ -> assert false
        in
        let ts',typ' = aux (ts, t'.typ) in
          {desc=App(t', ts'); typ=typ'}
    | If(t1, t2, t3) ->
        let t1' = match_typ t1 in
        let t2' = match_typ t2 in
        let t3' = match_typ t3 in
          assert (Wrapper.congruent [] t2'.typ t3'.typ);
          {desc=If(t1', t2', t3'); typ=t2'.typ}
    | Branch(t1, t2) ->
        let t1' = match_typ t1 in
        let t2' = match_typ t2 in
          assert (Wrapper.congruent [] t1'.typ t2'.typ);
          {desc=Branch(t1', t2'); typ=t2'.typ}
    | Let(flag, f, xs, t1, t2) ->
        let t1' = match_typ t1 in
        let t2' = match_typ t2 in
          {desc=Let(flag, f, xs, t1', t2'); typ=t2'.typ}
    | BinOp(op, t1, t2) ->
        let t1' = match_typ t1 in
        let t2' = match_typ t2 in
          {desc=BinOp(op, t1', t2'); typ=t.typ}
    | Not t ->
        let t' = match_typ t in
          {desc=Not t'; typ=t.typ}
    | Fail -> fail_term
    | Label _ -> assert false
    | Event s -> event_term s
    | Nil -> assert false
    | Cons(t1, t2) -> assert false
    | Match(t1,t2,x,y,t3) -> assert false
    | Constr(c,ts) -> assert false
    | Match_(t,pats) -> assert false


let rec remove_preds_typ typ =
  match typ with
      TUnit -> TUnit
    | TAbsBool -> TAbsBool
    | TBool -> TBool
    | TInt _ -> TInt []
    | TRInt p -> TRInt p
    | TVar _ -> assert false
    | TFun(x,typ2) -> TFun(Id.set_typ x (remove_preds_typ (Id.typ x)), remove_preds_typ typ2)
    | TUnknown -> TUnknown

let rec remove_preds t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | NInt x -> NInt x
      | Var x -> Var(Id.set_typ x (remove_preds_typ (Id.typ x)))
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
  in
    {desc=desc; typ=t.typ}

let refine tdefs ces t0 =
  (*
    let () = Format.printf "%a@." (print_term_fm ML true) t in
  *)
  let defs,t = lift t0 in
  let defs,t =
    let rec aux = function
        {desc=Let(_,f,xs,t1,t2)} ->
          let defs,t = aux t2 in
            (f,(xs,t1))::defs, t
      | t -> [], t
    in
      aux t0
  in
    (*
      let () = Format.printf "%a@." (print_term_fm ML true) (List.fold_left (fun acc (f,(xs,t)) -> Letrec(f,xs,t,acc)) t defs) in
    *)
    print_defs Format.std_formatter defs;
  let map,_,defs',t' = rename defs t in
    (*
      let scmap = get_scmap (List.fold_left (fun acc (f,(xs,t)) -> Letrec(f,xs,t,acc)) t' defs') in
      let () = Format.printf "\n\n%a\n\n@." (print_term_fm ML true) (List.fold_left (fun acc (f,(xs,t)) -> Letrec(f,xs,t,acc)) t' defs') in
    *)

  let s = Id.new_var "Main" TUnit in
  let defs1 = (s,([], t'))::defs' in
    print_defs Format.std_formatter defs1;
  let rte, sol = Infer.test tdefs s defs1 ces None in
  let t =
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
  in
    match_typ t


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
    | TFun({Id.typ=TInt ps} as x,rtyp1), TFun({Id.typ=TInt qs} as y,rtyp2) ->
        let rtyp2 = subst_type y (make_var x) rtyp2 in
        let typ =
          try
            let ps =
              let aux ps p =
                if List.exists (Wrapper.equiv [] p) ps ||
                  List.exists (Wrapper.equiv [] {desc=Not p;typ=TBool}) ps ||
                  Wrapper.equiv [] p true_term || Wrapper.equiv [] p false_term
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
          TFun(Id.set_typ x typ, rtyp)
    | TFun({Id.typ=TInt _} as x,rtyp1), TFun({Id.typ=TRInt p} as y,rtyp2) ->
        let typ = TRInt p in
        let rtyp2 = subst_type y (make_var x) rtyp2 in
        let rtyp = add_preds_typ_ rtyp1 rtyp2 in
          TFun(Id.set_typ x typ, rtyp)
    | TFun({Id.typ=TRInt p} as x,rtyp1), TFun({Id.typ=TInt _} as y,rtyp2)
    | TFun({Id.typ=TRInt p} as x,rtyp1), TFun({Id.typ=TRInt _} as y,rtyp2) ->
        let typ = TRInt p in
        let rtyp2 = subst_type y (make_var x) rtyp2 in
        let rtyp = add_preds_typ_ rtyp1 rtyp2 in
          TFun(Id.set_typ x typ, rtyp)
    | TFun(x,rtyp1), TFun(y,rtyp2) ->
        let rtyp2 = subst_type y (make_var x) rtyp2 in
        let typ = add_preds_typ_ (Id.typ x) (Id.typ y) in
        let rtyp = add_preds_typ_ rtyp1 rtyp2 in
          TFun(Id.set_typ x typ, rtyp)
    | TUnknown, _ -> TUnknown
    | _, _ ->
        Format.printf "%a and %a" print_typ typ1 print_typ typ2;
        assert false

let rec add_preds_ typedefs t =
  let desc =
    match t.desc with
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
            try add_preds_typ_ (Id.typ x) (snd (List.find (fun (x', _) -> Id.same x' x) typedefs))
            with Not_found -> Id.typ x
          in
            Var(Id.set_typ x typ)
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
          if flag = Flag.Nonrecursive
          then
            let typ = 
              try add_preds_typ_ (Id.typ f) (snd (List.find (fun (f', _) -> Id.same f' f) typedefs))
              with Not_found -> Id.typ f
            in
            let f' = Id.set_typ f typ in
            let xs' = get_args typ in
            let t1' = add_preds_ typedefs t1 in
            let t2' = add_preds_ typedefs t2 in
            let t1'' = List.fold_right2 subst xs (List.map make_var xs') t1' in
            let t2'' = subst f (make_var f') t2' in
              Let(flag, f', xs', t1'', t2'')
          else
            let typ = 
              try add_preds_typ_ (Id.typ f) (snd (List.find (fun (f', _) -> Id.same f' f) typedefs))
              with Not_found -> Id.typ f
            in
            let f' = Id.set_typ f typ in
            let xs' = get_args typ in
            let t1' = add_preds_ typedefs t1 in
            let t2' = add_preds_ typedefs t2 in
            let t1'' = List.fold_right2 subst (f::xs) (List.map make_var (f'::xs')) t1' in
            let t2'' = subst f (make_var f') t2' in
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
  in
    {desc=desc; typ=t.typ}

