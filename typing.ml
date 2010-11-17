
open Syntax


exception CannotUnify


let new_tvar () = TVar (ref None)
let new_var x =
  let typ = new_tvar () in
    {x with typ = typ}, typ


let (@@) = List.rev_append


let rec flatten typ =
  match typ with
      TFun((x,typ1), typ2) -> TFun((x,flatten typ1), flatten typ2) (*???*)
    | TVar{contents = None} -> TUnknown
    | TVar{contents = Some typ'} -> flatten typ'
    | _ -> typ

let rec flatten2 typ =
  match typ with
      TVar{contents = Some typ'} -> flatten2 typ'
    | _ -> typ

let rec occurs r typ =
  match flatten2 typ with
    TFun((_,typ1), typ2) -> occurs r typ1 || occurs r typ2
  | TVar({contents = None} as r') -> r == r'
  | _ -> false

let rec unify typ1 typ2 =
(*
Format.printf "unify %a %a@." (print_typ ML) (flatten typ1) (print_typ ML) (flatten typ2);
*)
  match flatten2 typ1, flatten2 typ2 with
      TUnit, TUnit
    | TBool, TBool
    | TInt _, TInt _ -> ()
    | TRInt _, TRInt _ -> ()
    | TFun((_,typ11), typ12), TFun((_,typ21), typ22) ->
        unify typ11 typ21;
        unify typ12 typ22
    | TVar r1, TVar r2 when r1 == r2 -> ()
(*
    | TVar{contents = None}, TVar{contents = Some typ'} -> unify typ1 typ'
    | TVar{contents = Some typ'}, TVar{contents = None} -> unify typ' typ2
    | TVar{contents = Some(typ')}, typ
    | typ, TVar{contents = Some(typ')} -> unify typ typ'
*)
    | TVar({contents = None} as r), typ
    | typ, TVar({contents = None} as r) ->
        if occurs r typ then
          (Format.printf "occurs check failure: %a, %a@." (print_typ ML) (flatten typ1) (print_typ ML) (flatten typ2);
          raise CannotUnify)
        else
          r := Some typ
    | _ -> begin
        (if Flag.debug
        then Format.printf "unification error: %a, %a@." (print_typ ML) (flatten typ1) (print_typ ML) (flatten typ2));
        raise CannotUnify
    end

let dummy = new_var' "dummy"
(*let dummy () = fst (new_var (new_var' "dummy"))*)

let rec infer env t = 
  (*Format.printf "inferring %a@." (print_term_fm ML false) t;*)
  match t with
      Unit -> Unit, TUnit
    | True -> True, TBool
    | False -> False, TBool
    | Unknown -> Unknown, TBool
    | Int n -> Int n, TInt []
    | NInt x ->
        let typ = TInt [] in
          NInt {x with typ = typ}, typ
    | Var x ->
        let typ =
          try
            snd (List.find (fun (x', _) -> x.id = x'.id) env) (*???*)
          with Not_found ->
            if Flag.debug
            then Format.printf "@.not found: %a@." (print_term_fm ML false) (Var x); assert false in
          (*
            Format.printf ":%a:@." (print_typ ML) typ;
          *)
          Var {x with typ = typ}, typ
    | Fun(x, t) ->
        let x', typ_x = new_var x in
        let env' = (x', typ_x) :: env in
        let t', typ = infer env' t in
          Fun(x', t'), TFun((x',typ_x), typ)
    | App(t, ts) ->
        let t', typ1 = infer env t in
        let ts', typs = List.split (List.map (infer env) ts) in
        let typ = new_tvar () in
        let typ' = List.fold_right (fun t1 t2 -> TFun((dummy,t1), t2)) typs typ in
          unify typ1 typ';
          App(t', ts'), typ
    | If(t1, t2, t3, _) ->
        let t1', typ1 = infer env t1 in
        let t2', typ2 = infer env t2 in
        let t3', typ3 = infer env t3 in
          unify typ1 TBool;
          unify typ2 typ3;
          If(t1', t2', t3', Unit), typ2
    | Branch(t1, t2) ->
        let t1', typ1 = infer env t1 in
        let t2', typ2 = infer env t2 in
          unify typ1 typ2;
          Branch(t1', t2'), typ1
            (*assert false*)
    | Let(f, xs, t1, t2) ->
(*
(if Flag.debug then Format.printf "typing %a@." (print_term_fm ML false) (Var f));
*)
        let f', typ_f = new_var f in
        let xs', typs = List.split (List.map new_var xs) in
        let env1 = (List.combine xs' typs) @@ env in
        let env2 = (f', typ_f) :: env in
        let t1', typ1 = infer env1 t1 in
        let t2', typ2 = infer env2 t2 in
          unify typ_f (List.fold_right (fun typ1 typ2 -> TFun((dummy,typ1), typ2)) typs typ1);
          Let(f', xs', t1', t2'), typ2
    | Letrec(f, xs, t1, t2) ->
(*
(if Flag.debug then Format.printf "typing %a@." (print_term_fm ML false) (Var f));
*)
        let f', typ_f = new_var f in
        let xs', typs = List.split (List.map new_var xs) in

        let env2 = (f', typ_f) :: env in
        let env1 = (List.combine xs' typs) @@ env2 in
        let t1', typ1 = infer env1 t1 in
        let t2', typ2 = infer env2 t2 in
          unify typ_f (List.fold_right (fun typ1 typ2 -> TFun((dummy,typ1), typ2)) typs typ1);
          Letrec(f', xs', t1', t2'), typ2
    | BinOp(Eq as op, t1, t2) ->
        let t1', typ1 = infer env t1 in
        let t2', typ2 = infer env t2 in
          unify typ1 typ2;
          BinOp(op, t1', t2'), TBool
    | BinOp(Lt as op, t1, t2)
    | BinOp(Gt as op, t1, t2)
    | BinOp(Leq as op, t1, t2)
    | BinOp(Geq as op, t1, t2) ->
        let t1', typ1 = infer env t1 in
        let t2', typ2 = infer env t2 in
          unify typ1 (TInt[]);
          unify typ2 (TInt[]);
          BinOp(op, t1', t2'), TBool
    | BinOp(And as op, t1, t2)
    | BinOp(Or as op, t1, t2) ->
        let t1', typ1 = infer env t1 in
        let t2', typ2 = infer env t2 in
          unify typ1 TBool;
          unify typ2 TBool;
          BinOp(op, t1', t2'), TBool
    | BinOp(Add as op, t1, t2)
    | BinOp(Sub as op, t1, t2)
    | BinOp(Mult as op, t1, t2) ->
        let t1', typ1 = infer env t1 in
        let t2', typ2 = infer env t2 in
          unify typ1 (TInt[]);
          unify typ2 (TInt[]);
          BinOp(op, t1', t2'), (TInt[])
    | Not t ->
        let t', typ = infer env t in
          unify typ TBool;
          Not t', TBool
    | Fail ->
        let typ = new_tvar () in
          Fail, TFun((dummy,TUnit), typ)
    | Label(b,t) ->
        (*assert false*)
        let t', typ = infer env t in
          Label(b, t'), typ
    | Event s ->
        Event s, TFun((dummy,TUnit), TUnit)





let simplify_id x = {x with typ = flatten x.typ}

let rec simplify = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | Var x -> Var (simplify_id x)
  | Fun(x, t) ->
      let x' = simplify_id x in
      let t' = simplify t in
        Fun(x', t')
  | App(t, ts) ->
      let t' = simplify t in
      let ts' = List.map simplify ts in
        App(t', ts')
  | If(t1, t2, t3, _) ->
      let t1' = simplify t1 in
      let t2' = simplify t2 in
      let t3' = simplify t3 in
        If(t1', t2', t3', Unit)
  | Branch(t1, t2) -> (*assert false*)
      let t1' = simplify t1 in
      let t2' = simplify t2 in
        Branch(t1', t2')
  | Let(f, xs, t1, t2) ->
      let f' = simplify_id f in
      let xs' = List.map simplify_id xs in
      let t1' = simplify t1 in
      let t2' = simplify t2 in
        Let(f', xs', t1', t2')
  | Letrec(f, xs, t1, t2) ->
      let f' = simplify_id f in
      let xs' = List.map simplify_id xs in
      let t1' = simplify t1 in
      let t2' = simplify t2 in
        Letrec(f', xs', t1', t2')
  | BinOp (op, t1, t2) ->
      let t1' = simplify t1 in
      let t2' = simplify t2 in
        BinOp(op, t1', simplify t2')
  | Not t ->
      let t' = simplify t in
        Not t'
  | Label(b, t) ->
      let t' = simplify t in
        Label(b, t')
  | Fail -> Fail
  | Event s -> Event s


let rec match_arg_typ typ xs =
  match flatten typ,xs with
      TUnit,[] -> TUnit
    | TInt ps,[] -> TInt ps
    | TRInt p,[] -> TRInt p
    | TBool,[] -> TBool
    | TFun(_,_),[] -> typ
    | TFun(_,typ2),x::xs' ->
        let typ2' = match_arg_typ typ2 xs' in
          TFun((x,x.typ),typ2')
    | TUnknown, _ -> TUnknown
    | typ, _ -> Format.printf "%a@." (print_typ ML) typ; assert false

let new_var_typ x =
  let rec aux = function
      TUnit -> TUnit
    | TAbsBool -> TAbsBool
    | TBool -> TBool
    | TInt ps -> TInt ps
    | TRInt p -> TRInt p
    | TVar _ -> assert false
    | TFun((_,typ1),typ2) ->
        let typ1' = aux typ1 in
        let typ2' = aux typ2 in
        let x = {(new_var' "x") with typ = typ1'} in
          TFun((x,typ1'),typ2')
    | TUnknown -> TUnknown
  in
    {x with typ = aux x.typ}

let rec match_arg = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | Var x -> Var x
  | Fun(x, t) ->
      let t' = match_arg t in
        Fun(x, t')
  | App(t, ts) ->
      let t' = match_arg t in
      let ts' = List.map match_arg ts in
        App(t', ts')
  | If(t1, t2, t3, _) ->
      let t1' = match_arg t1 in
      let t2' = match_arg t2 in
      let t3' = match_arg t3 in
        If(t1', t2', t3', Unit)
  | Branch(t1, t2) ->
      let t1' = match_arg t1 in
      let t2' = match_arg t2 in
        Branch(t1', t2')
  | Let(f, xs, t1, t2) ->
      let xs' = List.map new_var_typ xs in
      let typ = match_arg_typ f.typ xs' in
      let f' = {f with typ = typ} in
      let t1' = List.fold_right2 subst xs (List.map (fun x -> Var x) xs') t1 in
      let t2' = subst f (Var f') t2 in
      let t1'' = match_arg t1' in
      let t2'' = match_arg t2' in
        Let(f', xs', t1'', t2'')
  | Letrec(f, xs, t1, t2) ->
      let xs' = List.map new_var_typ xs in
      let typ = match_arg_typ f.typ xs' in
      let f' = {f with typ = typ} in
      let t1' = List.fold_right2 subst (f::xs) (List.map (fun x -> Var x) (f'::xs')) t1 in
      let t2' = subst f (Var f') t2 in
      let t1'' = match_arg t1' in
      let t2'' = match_arg t2' in
        Letrec(f', xs', t1'', t2'')
  | BinOp(op, t1, t2) ->
      let t1' = match_arg t1 in
      let t2' = match_arg t2 in
        BinOp(op, t1', t2')
  | Not t ->
      let t' = match_arg t in
        Not t'
  | Fail -> Fail
  | Label(b, t) ->
      (*assert false*)
      let t' = match_arg t in
        Label(b, t')
  | Event s -> Event s



let typing cps t0 =
  let t1, typ = infer [] t0 in
  let () = unify typ TUnit in
  let t2 = simplify t1 in
    match_arg t2


let typing_defs cps defs t0 =
(*
  let () = Format.printf "%a@." (print_term_fm ML true) (List.fold_left (fun acc (f,(xs,t)) -> Letrec(f,xs,t,acc)) t0 defs) in
*)

  let env = List.map (fun (f, _) -> new_var f) defs in
  let fsub = List.map2 (fun (f, _) (f', _) -> f, Var(f')) defs env in
  let t0', typ = infer env t0 in
  let () = unify typ TUnit in
  let defs =
		  List.map2
		    (fun (_, (xs, t)) (f', _) ->
		      let xs', typs = List.split (List.map new_var xs) in
(*
Format.printf "a: %a@." (print_term_fm ML false) t;
*)
		      let t', typ = infer
          ((List.combine xs' typs) @@ env)
          (subst_term (fsub @ (List.map2 (fun id id' -> id, Var(id')) xs xs')) t) in
(*
Format.printf "<%a:%a>@." (print_term_fm ML false) t' (print_typ ML) (flatten typ);
Format.printf "<%a:%a>@." (print_term_fm ML false) (Var f') (print_typ ML) (flatten f'.typ);
*)
        let ft =  List.fold_right (fun x typ -> TFun((x,x.typ), typ)) xs' typ in
(*
List.iter (fun (f, t) -> Format.printf "%a:%a@." (print_term_fm ML false) (Var f) (print_typ ML) (flatten t)) env;
Format.printf "<%a:%a>@." (print_term_fm ML false) (Var f') (print_typ ML) (flatten ft);
*)
		      unify f'.typ ft;
		      f', (xs', t'))
		    defs env
  in
  List.map (fun (f, (xs, t)) -> simplify_id f, (List.map simplify_id xs, match_arg (simplify t))) defs,
  match_arg (simplify t0')




let rec get_typ = function
    Unit -> TUnit
  | True -> TBool
  | False -> TBool
  | Unknown -> TBool
  | Int n -> TInt []
  | NInt x -> x.typ
  | Var x -> x.typ
  | Fun(x, t) -> TFun ((x,x.typ), get_typ t)
  | App(f, ts) ->
      let rec aux typ ts =
        match typ,ts with
            _,[] -> typ
          | TFun(_,typ2), t::ts' -> aux typ2 ts'
          | _ -> assert false
      in
        aux (get_typ f) ts
  | If(_, t2, _, _) ->
      get_typ t2
  | Branch(t1, _) ->
      get_typ t1
  | Let(_, _, _, t2) ->
      get_typ t2
  | Letrec(_, _, _, t2) ->
      get_typ t2
  | BinOp((Eq|Lt|Gt|Leq|Geq|And|Or), _, _) -> TBool
  | BinOp((Add|Sub|Mult), _, _) -> TInt []
  | Not _ -> TBool
  | Fail -> TFun((dummy,TUnit), TUnit)
  | Label(_,t) -> get_typ t
  | Event s -> TFun((dummy,TUnit), TUnit)

