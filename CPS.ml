open Syntax
open Term_util
open Type
open Util

module RT = Ref_type

module Debug = Debug.Make(struct let check = make_debug_check __MODULE__ end)

let counter = ref 0
let new_evar () = incr counter; !counter

type term = {t_orig:Syntax.term; t_cps:t_cps; typ_cps:typ_cps; effect:effect_var}
and typed_ident = {id_cps:id; id_typ:typ_cps}
and t_cps =
  | ConstCPS of const
  | BottomCPS
  | RandIntCPS of bool
  | RandValueCPS of typ
  | VarCPS of typed_ident
  | FunCPS of typed_ident * term
  | AppCPS of term * term
  | IfCPS of term * term * term
  | LetCPS of (typed_ident * term) list * term
  | BinOpCPS of binop * term * term
  | NotCPS of term
  | EventCPS of string
  | ProjCPS of int * term
  | TupleCPS of term list
  | RaiseCPS of term
  | TryWithCPS of term * term
and typ_cps =
  | TBaseCPS of Syntax.typ
  | TFunCPS of effect_var * typ_cps * typ_cps
  | TTupleCPS of typ_cps list
and effect = EUnknown (* for debug *) | ENone | ECont | EExcep
and effect_var = int
and effect_constr =
  | CGeq of effect_var * effect
  | CGeqVar of effect_var * effect_var

type env =
    {mutable constraints: effect_constr list;
     mutable counter: int}

let effect_max x y =
  match x, y with
  | EUnknown, _
  | _, EUnknown -> assert false
  | ENone, _ -> y
  | _, ENone -> x
  | _, EExcep -> EExcep
  | EExcep, _ -> EExcep
  | ECont, ECont -> ECont

let effect_cont = 0

let rec print_typ_cps sol fm typ =
  match typ with
  | TBaseCPS typ -> Format.fprintf fm "%a" Print.typ typ
  | TFunCPS _ ->
      let rec decomp typ =
        match typ with
        | TFunCPS(e,typ1,typ2) ->
            let etyps,typ2' = decomp typ2 in
            (e,typ1)::etyps, typ2'
        | _ -> [], typ
      in
      let pr fm (e,typ1) =
        let ar =
          match sol e with
          | EUnknown -> Format.asprintf "-%a->" print_evar e
          | ENone -> "->"
          | ECont -> "=>"
          | EExcep -> "-=>"
        in
        Format.fprintf fm "%a %s@ " (print_typ_cps sol) typ1 ar
      in
      let etyps,typ2 = decomp typ in
      Format.fprintf fm "(@[%a%a@])" (print_list pr "") etyps (print_typ_cps sol) typ2
  | TTupleCPS typs ->
      Format.fprintf fm "{%a}" (print_list (print_typ_cps sol) " *@ ") typs


and print_termlist sol fm = List.iter (fun bd -> Format.fprintf fm "@;%a" (print_term sol) bd)

and print_term sol fm {t_cps=t; typ_cps=typ; effect=e} =
  match true, sol e with
  | true, EUnknown -> Format.fprintf fm "(%a :%a: %a)" (print_t_cps sol) t print_evar e (print_typ_cps sol) typ
  | true, e -> Format.fprintf fm "(%a :%a: %a)" (print_t_cps sol) t (Color.green print_effect) e (Color.cyan @@ print_typ_cps sol) typ
  | _ -> Format.fprintf fm "(%a : %a)" (print_t_cps sol) t (print_typ_cps sol) typ

and print_t_cps sol fm = function
  | ConstCPS c -> Format.fprintf fm "%a" Print.const c
  | BottomCPS -> Format.fprintf fm "_|_"
  | RandIntCPS b -> Format.fprintf fm "rand_int(%b)" b
  | RandValueCPS typ -> Format.fprintf fm "rand_value(%a)" Print.typ typ
  | VarCPS x -> Print.id fm x.id_cps
  | FunCPS(x, t) ->
      Format.fprintf fm "@[<hov 2>fun %a : %a ->@ %a@]" Print.id x.id_cps (print_typ_cps sol) x.id_typ (print_term sol) t
  | AppCPS(t1, t2) ->
      Format.fprintf fm "%a%a" (print_term sol) t1 (print_term sol) t2
  | IfCPS(t1, t2, t3) ->
      Format.fprintf fm "@[@[if %a@]@;then @[%a@]@;else @[%a@]@]"
                     (print_term sol) t1 (print_term sol) t2 (print_term sol) t3
  | LetCPS(bindings, t) ->
      let head = ref "let rec" in
      let pr fm (f,t) =
        Format.fprintf fm "@[<hov 2>%s %a : %a =@ @[%a@]@]@;"
                       !head Print.id f.id_cps (print_typ_cps sol) f.id_typ (print_term sol) t;
        head := "and"
      in
      Format.fprintf fm "@[<v>%a@;in@;%a@]" (print_list pr "") bindings (print_term sol) t
  | BinOpCPS(op, t1, t2) ->
      Format.fprintf fm "%a %s %a" (print_term sol) t1 (Print.string_of_binop op) (print_term sol) t2
  | NotCPS t ->
      Format.fprintf fm "not %a" (print_term sol) t
  | EventCPS s -> Format.fprintf fm "{%s}" s
  | ProjCPS(i,t) ->
      Format.fprintf fm "#%d %a" i (print_term sol) t
  | TupleCPS ts ->
      Format.fprintf fm "(%a)" (print_list (print_term sol) ",@ ") ts
  | RaiseCPS t ->
      Format.fprintf fm "@[raise %a@]" (print_term sol) t
  | TryWithCPS(t1,t2) ->
      Format.fprintf fm "@[<hov 2>@[<hov 2>try@ %a@]@ with@ %a@]" (print_term sol) t1 (print_term sol) t2

and print_effect fm = function
  | EUnknown -> Format.fprintf fm "EUnknown"
  | ENone -> Format.fprintf fm "ENone"
  | ECont -> Format.fprintf fm "ECont"
  | EExcep -> Format.fprintf fm "EExcep"

and print_evar fm x = Format.fprintf fm "e%d" x

let print_econstr fm = function
  | CGeqVar(x, y) -> Format.fprintf fm "%a :> %a" print_evar x print_evar y
  | CGeq(x, e) -> Format.fprintf fm "%a :> %a" print_evar x print_effect e

let constraints = ref []
let constraints = ()

let rec unify env typ1 typ2 =
  match typ1, typ2 with
  | TBaseCPS typ1, TBaseCPS typ2 -> ()
  | TFunCPS(x1,typ11,typ12), TFunCPS(x2,typ21,typ22) ->
      env.constraints <- CGeqVar(x1, x2) :: CGeqVar(x2, x1) :: env.constraints;
      unify env typ11 typ21;
      unify env typ12 typ22
  | TTupleCPS typs1, TTupleCPS typs2 ->
      List.iter2 (unify env) typs1 typs2
  | _ ->
      Format.eprintf "Bug?@.typ1: %a@.typ2: %a@."
                     (print_typ_cps (fun _ -> ENone)) typ1
                     (print_typ_cps (fun _ -> ENone)) typ2;
      assert false

let rec typ_of_etyp etyp =
  match etyp with
  | TBaseCPS typ -> typ
  | TFunCPS(x, etyp1, etyp2) -> make_tfun (typ_of_etyp etyp1) (typ_of_etyp etyp2)
  | TTupleCPS etyps -> make_ttuple (List.map typ_of_etyp etyps)

let rec lift_letrec_typ env typed =
  match typed.t_cps, typed.typ_cps with
  | FunCPS(_, ({t_cps=FunCPS _} as typed1)), _ ->
      lift_letrec_typ env typed1
  | FunCPS _, TFunCPS(e, _, _) ->
      env.constraints <- CGeq(e, ECont) :: env.constraints
  | FunCPS _, _ -> assert false
  | _ -> ()

let rec etyp_of_typ ty =
  match ty with
  | TBase _
  | TData _ -> TBaseCPS ty
  | TTuple xs -> TTupleCPS (List.map (etyp_of_typ -| Id.typ) xs)
  | TAttr(_,ty') -> etyp_of_typ ty'
  | _ -> Format.eprintf "%a@." Print.typ ty; assert false

let rec infer_effect_typ env typ =
  match typ with
  | TBase _
  | TData _ -> TBaseCPS typ
  | TFun(x,typ2) ->
      let typ1 = Id.typ x in
      let e = new_evar () in
      (match typ2 with TFun _ -> () | _ -> env.constraints <- CGeq(e, ECont) :: env.constraints);
      TFunCPS(e, infer_effect_typ env typ1, infer_effect_typ env typ2)
  | TTuple xs -> TTupleCPS (List.map (infer_effect_typ env -| Id.typ) xs)
  | TAttr(_,typ) -> infer_effect_typ env typ
  | _ -> Format.eprintf "%a@." Print.typ typ; assert false

let new_var env x = {id_cps=x; id_typ=infer_effect_typ env (Id.typ x)}

let _TFunCPS env (e, typ1, typ2) =
  if !Flag.Method.cps_simpl then env.constraints <- CGeq(e, ECont) :: env.constraints;
  TFunCPS(e, typ1, typ2)

let rec infer_effect env tenv t =
  match t.desc with
  | Const(RandValue(TBase TInt,true)) -> assert false
  | Const(RandValue(TBase TInt,false)) ->
      let e = new_evar () in
      let typ = _TFunCPS env (e, TBaseCPS Ty.unit, TBaseCPS Ty.int) in
      env.constraints <- CGeq(e, ECont) :: env.constraints;
      {t_orig=t; t_cps=RandIntCPS(List.mem AAbst_under t.attr); typ_cps=typ; effect=new_evar()}
  | Const(RandValue(typ, true)) -> assert false
  | Const(RandValue(typ, false)) ->
      let e = new_evar () in
      Format.printf "typ: %a@." Print.typ typ;
      let typ' = _TFunCPS env (e, TBaseCPS Ty.unit, etyp_of_typ typ) in
      env.constraints <- CGeq(e, ECont) :: env.constraints;
      {t_orig=t; t_cps=RandValueCPS typ; typ_cps=typ'; effect=new_evar()}
  | Const c -> {t_orig=t; t_cps=ConstCPS c; typ_cps=TBaseCPS t.typ; effect=new_evar()}
  | Bottom ->
      let e = new_evar () in
      env.constraints <- CGeq(e, ECont) :: env.constraints;
      {t_orig=t; t_cps=BottomCPS; typ_cps=infer_effect_typ env t.typ; effect=e}
  | Var x ->
      let typ =
	try
	  List.assoc (Id.to_string x) tenv
	with
	| Not_found when Fpat.RefTypInfer.is_parameter (Id.name x) -> TBaseCPS Ty.int
	| Not_found -> Format.eprintf "%a@." Print.id x; assert false
      in
      {t_orig=t; t_cps=VarCPS{id_cps=x;id_typ=typ}; typ_cps=typ; effect=new_evar()}
  | Fun(x, t1) ->
      let x_typ = infer_effect_typ env (Id.typ x) in
      let x' = {id_cps=x; id_typ=x_typ} in
      let tenv' = (Id.to_string x, x_typ) :: tenv in
      let typed = infer_effect env tenv' t1 in
      let typ' = infer_effect_typ env t.typ in
      let e,a_typ,r_typ = match typ' with TFunCPS(e,typ1,typ2) -> e,typ1,typ2 | _ -> assert false in
      if !Flag.Method.cps_simpl then env.constraints <- CGeq(e, ECont) :: env.constraints;
      env.constraints <- CGeqVar(e, typed.effect) :: env.constraints;
      unify env a_typ x_typ;
      unify env r_typ typed.typ_cps;
      {t_orig=t; t_cps=FunCPS(x',typed); typ_cps=typ'; effect=new_evar()}
  | App(t1, []) -> assert false
  | App(t1, t2::t3::ts) ->
      let typ = (make_app t1 [t2]).typ in
      infer_effect env tenv {desc=App({desc=App(t1,[t2]);typ;attr=[]}, t3::ts); typ=t.typ; attr=[]}
  | App(t1, [t2]) ->
      let typed1 = infer_effect env tenv t1 in
      let typed2 = infer_effect env tenv t2 in
      let rtyp = infer_effect_typ env t.typ in
      let e0 = new_evar () in
      let typ = _TFunCPS env (e0, typed2.typ_cps, rtyp) in
      let e = new_evar () in
      env.constraints <- CGeqVar(e, typed1.effect) :: env.constraints;
      env.constraints <- CGeqVar(e, typed2.effect) :: env.constraints;
      env.constraints <- CGeqVar(e, e0) :: env.constraints;
      unify env typed1.typ_cps typ;
      {t_orig=t; t_cps=AppCPS(typed1,typed2); typ_cps=rtyp; effect=e}
  | If(t1, t2, t3) ->
      let typed1 = infer_effect env tenv t1 in
      let typed2 = infer_effect env tenv t2 in
      let typed3 = infer_effect env tenv t3 in
      let e = new_evar () in
      env.constraints <- CGeqVar(e, typed1.effect) :: env.constraints;
      env.constraints <- CGeqVar(e, typed2.effect) :: env.constraints;
      env.constraints <- CGeqVar(e, typed3.effect) :: env.constraints;
      env.constraints <- CGeq(e, ECont) :: env.constraints; (* for TRecS *)
      unify env typed2.typ_cps typed3.typ_cps;
      {t_orig=t; t_cps=IfCPS(typed1,typed2,typed3); typ_cps=typed2.typ_cps; effect=e}
  | Local(Decl_let bindings, t1) ->
      let make_env (f,_) = Id.to_string f, infer_effect_typ env (Id.typ f) in
      let tenv_f = List.map make_env bindings in
      let tenv' = tenv_f @@@ tenv in
      let aux (f, t1) =
        let f' = {id_cps=f; id_typ=List.assoc (Id.to_string f) tenv_f} in
        let typed = infer_effect env tenv' t1 in
        let () = lift_letrec_typ env typed in
        unify env f'.id_typ typed.typ_cps;
        f', typed
      in
      let bindings' = List.map aux bindings in
      let typed = infer_effect env tenv' t1 in
      let aux (_,typed) e =
        let e' = new_evar () in
        env.constraints <- CGeqVar(e', typed.effect) :: env.constraints;
        env.constraints <- CGeqVar(e', e) :: env.constraints;
        e'
      in
      let e = List.fold_right aux bindings' typed.effect in
      {t_orig=t; t_cps=LetCPS(bindings', typed); typ_cps=typed.typ_cps; effect=e}
  | BinOp(op, t1, t2) ->
      let typed1 = infer_effect env tenv t1 in
      let typed2 = infer_effect env tenv t2 in
      let e = new_evar () in
      unify env typed1.typ_cps typed2.typ_cps;
      env.constraints <- CGeqVar(e, typed1.effect) :: env.constraints;
      env.constraints <- CGeqVar(e, typed2.effect) :: env.constraints;
      {t_orig=t; t_cps=BinOpCPS(op,typed1,typed2); typ_cps=TBaseCPS t.typ; effect=e}
  | Not t1 ->
      let typed = infer_effect env tenv t1 in
      unify env typed.typ_cps (TBaseCPS t.typ);
      {t_orig=t; t_cps=NotCPS typed; typ_cps=TBaseCPS t.typ; effect=typed.effect}
  | Event(s,true) -> assert false
  | Event(s,false) ->
      let e = new_evar () in
      let typ = _TFunCPS env (e, TBaseCPS Ty.unit, TBaseCPS Ty.unit) in
      env.constraints <- CGeq(e, ECont) :: env.constraints;
      {t_orig=t; t_cps=EventCPS s; typ_cps=typ; effect=new_evar()}
  | Proj(i,t1) ->
      let typed = infer_effect env tenv t1 in
      let typ = infer_effect_typ env t1.typ in
      let typ1 = match typ with TTupleCPS typs -> List.nth typs i | _ -> Format.eprintf "%a@." Print.term' t1; assert false in
      unify env typed.typ_cps typ;
      {t_orig=t; t_cps=ProjCPS(i,typed); typ_cps=typ1; effect=typed.effect}
  | Tuple ts ->
      let typeds = List.map (infer_effect env tenv) ts in
      let typ = TTupleCPS (List.map (fun typed -> typed.typ_cps) typeds) in
      let e = new_evar () in
      List.iter (fun typed -> env.constraints <- CGeqVar(e, typed.effect) :: env.constraints) typeds;
      env.constraints <- CGeq(e, ECont) :: env.constraints; (* for remove_pair *)
      {t_orig=t; t_cps=TupleCPS typeds; typ_cps=typ; effect=e}
  | TryWith(t1, t2) ->
      let typed1 = infer_effect env tenv t1 in
      let typed2 = infer_effect env tenv t2 in
      let e = new_evar () in
      let typ = infer_effect_typ env t.typ in
      let e2,typ2 = match typed2.typ_cps with TFunCPS(e,typ1,typ2) -> e,typ2 | _ -> assert false in
      unify env typed1.typ_cps typ2;
      unify env typed1.typ_cps typ;
      env.constraints <- CGeqVar(e, typed1.effect) :: env.constraints;
      {t_orig=t; t_cps=TryWithCPS(typed1,typed2); typ_cps=typ; effect=e}
  | Raise t1 ->
      let typed = infer_effect env tenv t1 in
      let e = new_evar () in
      env.constraints <- CGeq(e, EExcep) :: env.constraints;
      {t_orig=t; t_cps=RaiseCPS typed; typ_cps=infer_effect_typ env t.typ; effect=e}
  | _ ->
      Format.eprintf "t: @[%a@." Print.term t;
      assert false


exception Loop of effect_var list

let solve_constraints constrs =
  if 0=1 then
    begin
      Debug.printf "@.CONSTRAINTS:@.";
      List.iter (Debug.printf " %a@." print_econstr) constrs;
      Debug.printf "@."
    end;
  let num = !counter + 1 in
  let tbl = Array.make num [] in
  let sol = Array.make num None in
  let cgeqvars = List.filter_map (function CGeqVar(x,y) when x <> y -> Some (x,y) | _ -> None) constrs in
  let cgeqs = List.filter_map (function CGeq(x,e) -> Some (x,e) | _ -> None) constrs in
  List.iter (fun (x,y) -> tbl.(y) <- x::tbl.(y)) cgeqvars;
  List.iter (fun (_,e) -> assert (e = EExcep || e = ECont)) cgeqs;
  let cgeqs_excep = List.filter_map (fun (x,e) -> if e = EExcep then Some x else None) cgeqs in
  let cgeqs_cont = List.filter_map (fun (x,e) -> if e = ECont then Some x else None) cgeqs in
  let solve_const c inits =
    let rec aux x =
      match sol.(x) with
      | None ->
          sol.(x) <- Some c;
          List.iter aux tbl.(x)
      | Some e -> ()
    in
    List.iter aux inits
  in
  solve_const EExcep cgeqs_excep;
  solve_const ECont cgeqs_cont;
  fun e ->
    match sol.(e) with
    | None -> ENone
    | Some e -> e

let check_solution sol env =
  let dbg = 0 = 1 in
  let check e1 e2 = assert (effect_max e1 e2 = e1) in
  let aux = function
    | CGeqVar(x, y) ->
        let e1 = sol x in
        let e2 = sol y in
        if dbg then Format.printf "%a(%a) :> %a(%a)@." print_evar x print_effect e1 print_evar y print_effect e2;
        check e1 e2
    | CGeq(x, e) ->
        let e1 = sol x in
        if dbg then Format.printf "%a(%a) :> %a@." print_evar x print_effect e1 print_effect e;
        check e1 e
  in
  List.iter aux env.constraints



let rec app_typ typ typs =
  match typ,typs with
  | TFunCPS(_,_,typ2), _::typs' -> app_typ typ2 typs'
  | _, [] -> typ
  | _ -> raise (Fatal "bug? (CPS.app_typ)")




let rec add_preds_cont_aux k t =
  let desc =
    match t.desc with
    | Const c -> Const c
    | Var y -> Var y
    | Fun(y, t) -> Fun(y, add_preds_cont_aux k t)
    | App(t1, ts) ->
        let aux t (typ,ts) =
          let x, typ' =
            match typ with
              TFun(x,typ) -> x, subst_type x t typ
            | _ -> assert false
          in
          let t' =
            if t.desc = Var k
            then make_var (Id.set_typ k @@ Id.typ x)
            else add_preds_cont_aux k t
          in
          typ', t'::ts
        in
        let _,ts' = List.fold_right aux ts (t1.typ,[]) in
        App(add_preds_cont_aux k t1, ts')
    | If(t1, t2, t3) -> If(add_preds_cont_aux k t1, add_preds_cont_aux k t2, add_preds_cont_aux k t3)
    | Local(Decl_let bindings, t2) ->
        let bindings' = List.map (fun (f,t) -> f, add_preds_cont_aux k t) bindings in
        let t2' = add_preds_cont_aux k t2 in
        Local(Decl_let bindings', t2')
    | BinOp(op, t1, t2) -> BinOp(op, add_preds_cont_aux k t1, add_preds_cont_aux k t2)
    | Not t1 -> Not (add_preds_cont_aux k t1)
    | Event(s,b) -> Event(s,b)
    | Record fields ->  Record (List.map (Pair.map_snd @@ add_preds_cont_aux k) fields)
    | Field(t1,s) -> Field(add_preds_cont_aux k t1,s)
    | SetField(t1,s,t2) -> SetField(add_preds_cont_aux k t1,s,add_preds_cont_aux k t2)
    | Nil -> Nil
    | Cons(t1,t2) -> Cons(add_preds_cont_aux k t1, add_preds_cont_aux k t2)
    | Constr(s,ts) -> Constr(s, List.map (add_preds_cont_aux k) ts)
    | Match(t1,pats) ->
        let aux (pat,cond,t1) = pat, add_preds_cont_aux k cond, add_preds_cont_aux k t1 in
        Match(add_preds_cont_aux k t1, List.map aux pats)
    | Raise t -> Raise (add_preds_cont_aux k t)
    | TryWith(t1,t2) -> TryWith(add_preds_cont_aux k t1, add_preds_cont_aux k t2)
    | Tuple ts -> Tuple (List.map (add_preds_cont_aux k) ts)
    | Proj(i,t) -> Proj(i, add_preds_cont_aux k t)
    | Bottom -> Bottom
    | _ -> assert false
  in
  {t with desc}

let add_preds_cont k t =
  let t' = add_preds_cont_aux k t in
  let ks = List.filter (Id.same k) (get_fv t') in
  Format.printf "APC: %a, %a ===> %a@." Id.print k Print.term t Print.term t';
  if List.length ks = 0
  then (assert (t.desc = Bottom); k, t')
  else (assert (List.length ks = 1); List.hd ks, t')


let rec force_cont = function
  | TBaseCPS typ -> TBaseCPS typ
  | TFunCPS(_,typ1,typ2) -> TFunCPS(effect_cont, force_cont typ1, force_cont typ2)
  | TTupleCPS typs -> TTupleCPS (List.map force_cont typs)

let rec trans_typ sol typ_excep typ_orig typ =
  match typ_orig,typ with
  | _, TBaseCPS _ -> typ_orig
  | TFun(x_orig,typ), TFunCPS(e,typ1,typ2) when sol e = EExcep ->
      let typ1' = trans_typ sol typ_excep (Id.typ x_orig) typ1 in
      let x = Id.new_var typ1' in
      let r = Id.new_var ~name:"r" @@ subst_type_var x_orig x @@ trans_typ sol typ_excep typ typ2 in
      let k = Id.new_var ~name:"k" @@ TFun(r,typ_result) in
      let e = Id.new_var ~name:"e" typ_excep in
      let h = Id.new_var ~name:"h" @@ TFun(e,typ_result) in
      TFun(x, TFun(k, TFun(h, typ_result)))
  | TFun(x_orig,typ), TFunCPS(e,typ1,typ2) when sol e = ECont ->
      let typ1' = trans_typ sol typ_excep (Id.typ x_orig) typ1 in
      let x = Id.new_var typ1' in
      let r = Id.new_var ~name:"r" @@ subst_type_var x_orig x (trans_typ sol typ_excep typ typ2) in
      let k = Id.new_var ~name:"k" @@ TFun(r,typ_result) in
      TFun(x, TFun(k, typ_result))
  | TFun(x_orig,typ), TFunCPS(_,typ1,typ2) ->
      let typ1' = trans_typ sol typ_excep (Id.typ x_orig) typ1 in
      let x = Id.new_var typ1' in
      let typ2' = subst_type_var x_orig x @@ trans_typ sol typ_excep typ typ2 in
      TFun(x, typ2')
  | TTuple xs, TTupleCPS typs ->
      TTuple (List.map2 (fun x typ -> Id.map_typ (trans_typ sol typ_excep -$- typ) x) xs typs)
  | TAttr(attr,typ_orig'), _ ->
      let aux a =
        match a with
        | TAPureFun -> false
        | TAEffect _ -> false
        | _ -> true
      in
      let attr' = List.filter aux attr in
      _TAttr attr' @@ trans_typ sol typ_excep typ_orig' typ
  | _ ->
      Format.eprintf "%a,%a@." Print.typ typ_orig (print_typ_cps sol) typ;
      raise (Fatal "bug? (CPS.trans_typ)")

let trans_var sol typ_excep x = Id.map_typ (trans_typ sol typ_excep -$- x.id_typ) x.id_cps
let trans_var' sol typ_excep x typ = (* for predicates *)
  let x' = trans_var sol typ_excep x in
  if same_shape typ @@ Id.typ x'
  then x'
  else Id.set_typ x' typ

let get_tfun_effect = function
  | TFunCPS(e, _, _) -> e
  | _ -> assert false

let make_app' t1 ts =
  match t1.desc, ts with
  | Fun(x,t1'), [t2] -> subst x t2 t1'
  | Fun(x1,{desc=Fun(x2,t1')}), [t2;t3] ->
      if count_occurrence x2 t1' >= 2 then (* t3 must be a hanadler *)
        subst x1 t2 @@ make_let [x2,t3] t1'
      else
        subst x1 t2 @@ subst x2 t3 t1'
  | _ -> make_app t1 ts

let app e ?h t ~k =
  match e with
  | EUnknown -> assert false
  | ENone -> make_app' k [t]
  | ECont -> make_app' t [k]
  | EExcep -> make_app' t [k; Option.get h]

let new_k_var k_post typ =
  let r = Id.new_var ~name:"r" typ in
  Id.new_var ~name:("k" ^ k_post) @@ TFun(r,typ_result)

let rec transform sol typ_excep k_post {t_orig; t_cps=t; typ_cps=typ; effect=e} =
  let typ_orig = t_orig.typ in
  let t' =
    match t, sol e with
    | ConstCPS c, ENone -> {desc=Const c; typ=typ_orig; attr=const_attr}
    | BottomCPS, ECont ->
        let k = new_k_var k_post @@ trans_typ sol typ_excep typ_orig typ in
        make_fun k @@ make_bottom typ_result
    | RandIntCPS b, ENone ->
        let e = get_tfun_effect typ in
        begin
          match sol e with
          | ECont -> make_randint_cps b
          | EExcep ->
              let e = Id.new_var ~name:"e" typ_excep in
              let h = Id.new_var ~name:"h" @@ TFun(e,typ_result) in
              make_fun h @@ make_randint_cps b
          | _ -> assert false
        end
    | RandValueCPS typ', ENone ->
        let e = get_tfun_effect typ in
        begin
          match sol e with
          | ECont -> make_randvalue_cps typ'
          | EExcep ->
              let e = Id.new_var ~name:"e" typ_excep in
              let h = Id.new_var ~name:"h" (TFun(e,typ_result)) in
              make_fun h (make_randvalue_cps typ_orig)
          | _ -> assert false
        end
    | VarCPS x, ENone -> make_var @@ trans_var sol typ_excep x
    | FunCPS(x, t1), ENone when sol (get_tfun_effect typ) = ENone ->
        let x' = trans_var sol typ_excep x in
        make_fun x' @@ transform sol typ_excep k_post t1
    | FunCPS(x, t1), ENone when sol (get_tfun_effect typ) = ECont ->
        let x' = trans_var sol typ_excep x in
        let k = new_k_var k_post @@ trans_typ sol typ_excep t1.t_orig.typ t1.typ_cps in
        let t1' = transform sol typ_excep k_post t1 in
        Term.(fun_ x' (fun_ k (app (sol t1.effect) t1' ~k:(var k))))
    | FunCPS(x, t1), ENone when sol (get_tfun_effect typ) = EExcep ->
        let x' = trans_var sol typ_excep x in
        let k = new_k_var k_post @@ trans_typ sol typ_excep t1.t_orig.typ t1.typ_cps in
        let e = Id.new_var ~name:"e" typ_excep in
        let h = Id.new_var ~name:"h" @@ TFun(e,typ_result) in
        let t1' = transform sol typ_excep k_post t1 in
        Term.(fun_ x' (fun_ k (fun_ h (app (sol t1.effect) t1' ~k:(var k) ~h:(var h)))))
    | AppCPS(t1, t2), ENone ->
        let t1' = transform sol typ_excep k_post t1 in
        let t2' = transform sol typ_excep k_post t2 in
        make_app t1' [t2']
    | AppCPS(t1, t2), ECont ->
        let t1' = transform sol typ_excep k_post t1 in
        let t2' = transform sol typ_excep k_post t2 in
        let k = new_k_var k_post @@ trans_typ sol typ_excep typ_orig typ in
        let x1 = Id.new_var (trans_typ sol typ_excep t1.t_orig.typ t1.typ_cps) in
        let x2 = Id.new_var (trans_typ sol typ_excep t2.t_orig.typ t2.typ_cps) in
        let e0 = get_tfun_effect t1.typ_cps in
        let open Term in
        fun_ k
          (app (sol t2.effect) t2'
             (fun_ x2
                (app (sol t1.effect) t1'
                   ~k:(fun_ x1 (app (sol e0) (var x1 @ [var x2]) ~k:(var k))))))
    | AppCPS(t1, t2), EExcep ->
        let t1' = transform sol typ_excep k_post t1 in
        let t2' = transform sol typ_excep k_post t2 in
        let k = new_k_var k_post @@ trans_typ sol typ_excep typ_orig typ in
        let x1 = Id.new_var (trans_typ sol typ_excep t1.t_orig.typ t1.typ_cps) in
        let x2 = Id.new_var (trans_typ sol typ_excep t2.t_orig.typ t2.typ_cps) in
        let e = Id.new_var ~name:"e" typ_excep in
        let h = Id.new_var ~name:"h" (TFun(e,typ_result)) in
        let h' = Id.new_var_id h in
        let e0 = get_tfun_effect t1.typ_cps in
        let open Term in
        fun_ k
          (fun_ h
             (let_ [h', var h] (* to prevent the increase of code size in eta-reduction *)
                (app (sol t1.effect) t1'
                   ~h:(var h')
                   ~k:(fun_ x1
                         (app (sol t2.effect) t2'
                            ~h:(var h')
                            ~k:(fun_ x2 (app (sol e0) (var x1 @ [var x2]) ~k:(var k) ~h:(var h'))))))))
    | IfCPS(t1, t2, t3), ENone ->
        let t1' = transform sol typ_excep k_post t1 in
        let t2' = transform sol typ_excep k_post t2 in
        let t3' = transform sol typ_excep k_post t3 in
        make_if t1' t2' t3'
    | IfCPS(t1, t2, t3), ECont ->
        let t1' = transform sol typ_excep k_post t1 in
        let t2' = transform sol typ_excep k_post t2 in
        let t3' = transform sol typ_excep k_post t3 in
        let r = Id.new_var ~name:"r" (trans_typ sol typ_excep typ_orig typ) in
        let k = Id.new_var ~name:("k" ^ k_post) (TFun(r,typ_result)) in
        let k' = Id.new_var ~name:("k" ^ k_post) (TFun(r,typ_result)) in
        let b = Id.new_var Ty.bool in
        let open Term in
        fun_ k
          (let_ [k', var k]
             (app (sol t1.effect) t1'
                ~k:(fun_ b
                      (add_attrs t_orig.attr
                         (if_
                            (var b)
                            (app (sol t2.effect) t2' ~k:(var k'))
                            (app (sol t3.effect) t3' ~k:(var k')))))))
    | IfCPS(t1, t2, t3), EExcep ->
        let t1' = transform sol typ_excep k_post t1 in
        let t2' = transform sol typ_excep k_post t2 in
        let t3' = transform sol typ_excep k_post t3 in
        let r = Id.new_var ~name:"r" (trans_typ sol typ_excep typ_orig typ) in
        let k = Id.new_var ~name:("k" ^ k_post) (TFun(r,typ_result)) in
        let k' = Id.new_var ~name:("k" ^ k_post) (TFun(r,typ_result)) in
        let b = Id.new_var Ty.bool in
        let e = Id.new_var ~name:"e" typ_excep in
        let h = Id.new_var ~name:"h" (TFun(e,typ_result)) in
        let h' = Id.new_var_id h in
        let open Term in
        fun_ k
          (let_ [k', var k] (* to prevent the increase of code size in eta-reduction *)
            (fun_ h
              (let_ [h', var h] (* to prevent the increase of code size in eta-reduction *)
                (app (sol t1.effect) t1' ~h:(var h')
                  ~k:(fun_ b
                        (add_attrs t_orig.attr
                          (if_
                            (var b)
                            (app (sol t2.effect) t2' ~k:(var k') ~h:(var h'))
                            (app (sol t3.effect) t3' ~k:(var k') ~h:(var h')))))))))
    | LetCPS(bindings, t1), ENone ->
        let aux (f,t) =
          let f' = trans_var sol typ_excep f in
          f', transform sol typ_excep (k_post ^ "_" ^ Id.name f') t
        in
        let bindings' = List.map aux bindings in
        let t1' = transform sol typ_excep k_post t1 in
        make_let bindings' t1'
    | LetCPS(bindings, t1), ECont ->
        let r = Id.new_var ~name:"r" @@ trans_typ sol typ_excep typ_orig typ in
        let k = Id.new_var ~name:("k" ^ k_post) @@ TFun(r,typ_result) in
        let aux (f,t) =
          let t' = transform sol typ_excep (k_post ^ "_" ^ Id.name f.id_cps) t in
          let f' = trans_var sol typ_excep f in
          let f'' =
            if sol t.effect = ENone
            then f'
            else Id.set_typ f' t'.typ
          in
          f'', t'
        in
        let bindings' = List.map aux bindings in
        let t1' = transform sol typ_excep k_post t1 in
        let aux (_,t_orig) (f,_) t' =
          let f' = Id.new_var ~name:(Id.name f) (trans_typ sol typ_excep t_orig.t_orig.typ t_orig.typ_cps) in
          let t'' = subst_var f f' t' in
          app (sol t_orig.effect) (make_var f) ~k:(make_fun f' t'')
        in
        let t1'' = List.fold_right2 aux bindings bindings' @@ app (sol t1.effect) t1' ~k:(make_var k) in
        make_fun k {(make_let bindings' t1'') with attr=t_orig.attr}
    | LetCPS(bindings, t1), EExcep ->
        let r = Id.new_var ~name:"r" @@ trans_typ sol typ_excep typ_orig typ in
        let k = Id.new_var ~name:("k" ^ k_post) @@ TFun(r,typ_result) in
        let e = Id.new_var ~name:"e" typ_excep in
        let h = Id.new_var ~name:"h" (TFun(e,typ_result)) in
        let aux (f,t) =
          let t' = transform sol typ_excep (k_post ^ "_" ^ Id.name f.id_cps) t in
          let f' = trans_var sol typ_excep f in
          let f'' =
            if sol t.effect = ENone
            then f'
            else Id.set_typ f' t'.typ
          in
          f'', t'
        in
        let bindings' = List.map aux bindings in
        let t1' = transform sol typ_excep k_post t1 in
        let aux (_,t_orig) (f,t) t' =
          let f' = Id.new_var ~name:(Id.name f) (trans_typ sol typ_excep t_orig.t_orig.typ t_orig.typ_cps) in
          let t'' = subst_var f f' t' in
          app (sol t_orig.effect) (make_var f) ~k:(make_fun f' t'') ~h:(make_var h)
        in
        make_fun k @@
          make_fun h @@
            {(make_let bindings' @@
                List.fold_right2 aux bindings bindings' @@
                  app (sol t1.effect) t1' ~k:(make_var k) ~h:(make_var h)) with attr=t_orig.attr}
    | BinOpCPS(op, t1, t2), ENone ->
        let t1' = transform sol typ_excep k_post t1 in
        let t2' = transform sol typ_excep k_post t2 in
        make_binop op  t1' t2'
    | BinOpCPS(op, t1, t2), ECont ->
        let r = Id.new_var ~name:"r" (trans_typ sol typ_excep typ_orig typ) in
        let k = Id.new_var ~name:("k" ^ k_post) (TFun(r,typ_result)) in
        let x1 = Id.new_var (trans_typ sol typ_excep t1.t_orig.typ t1.typ_cps) in
        let x2 = Id.new_var (trans_typ sol typ_excep t2.t_orig.typ t2.typ_cps) in
        let t1' = transform sol typ_excep k_post t1 in
        let t2' = transform sol typ_excep k_post t2 in
        let open Term in
        fun_ k
          (app (sol t1.effect) t1'
             (fun_ x1
                (app (sol t2.effect) t2'
                   ~k:(fun_ x2 (var k @ [var x1 <|op|> var x2])))))
    | BinOpCPS(op, t1, t2), EExcep ->
        let r = Id.new_var ~name:"r" (trans_typ sol typ_excep typ_orig typ) in
        let k = Id.new_var ~name:("k" ^ k_post) (TFun(r,typ_result)) in
        let x1 = Id.new_var (trans_typ sol typ_excep t1.t_orig.typ t1.typ_cps) in
        let x2 = Id.new_var (trans_typ sol typ_excep t2.t_orig.typ t2.typ_cps) in
        let e = Id.new_var ~name:"e" typ_excep in
        let h = Id.new_var ~name:"h" (TFun(e,typ_result)) in
        let h' = Id.new_var_id h in
        let t1' = transform sol typ_excep k_post t1 in
        let t2' = transform sol typ_excep k_post t2 in
        let open Term in
        fun_ k
          (fun_ h
             (let_ [h', var h] (* to prevent the increase of code size in eta-reduction *)
                (app (sol t1.effect) t1'
                   ~h:(var h')
                   ~k:(fun_ x1
                         (app (sol t2.effect) t2'
                            ~h:(var h')
                            ~k:(fun_ x2 (var k @ [var x1 <|op|> var x2])))))))
    | NotCPS t1, ENone ->
        let t1' = transform sol typ_excep k_post t1 in
        make_not t1'
    | NotCPS t1, ECont ->
        let r = Id.new_var ~name:"r" (trans_typ sol typ_excep typ_orig typ) in
        let k = Id.new_var ~name:("k" ^ k_post) (TFun(r,typ_result)) in
        let b = Id.new_var Ty.bool in
        let t1' = transform sol typ_excep k_post t1 in
        Term.(fun_ k (app (sol t1.effect) t1' ~k:(fun_ b (var k @ [not (var b)]))))
    | NotCPS t1, EExcep ->
        let r = Id.new_var ~name:"r" (trans_typ sol typ_excep typ_orig typ) in
        let k = Id.new_var ~name:("k" ^ k_post) (TFun(r,typ_result)) in
        let b = Id.new_var Ty.bool in
        let e = Id.new_var ~name:"e" typ_excep in
        let h = Id.new_var ~name:"h" (TFun(e,typ_result)) in
        let t1' = transform sol typ_excep k_post t1 in
        let open Term in
        fun_ k
          (fun_ h
             (app (sol t1.effect) t1'
                ~h:(var h)
                ~k:(fun_ b (var k @ [not (var b)]))))
    | EventCPS s, ENone -> make_event_cps s
    | ProjCPS(i,t1), ENone ->
        make_proj i @@ transform sol typ_excep k_post t1
    | ProjCPS(i,t1), ECont ->
        let r = Id.new_var ~name:"r" @@ trans_typ sol typ_excep typ_orig typ in
        let k = Id.new_var ~name:("k" ^ k_post) (TFun(r,typ_result)) in
        let p = Id.new_var ~name:"p" (trans_typ sol typ_excep t1.t_orig.typ t1.typ_cps) in
        let t1' = transform sol typ_excep k_post t1 in
        Term.(fun_ k (app (sol t1.effect) t1' ~k:(fun_ p (var k @ [proj i (var p)]))))
    | ProjCPS(i,t1), EExcep ->
        let r = Id.new_var ~name:"r" (trans_typ sol typ_excep typ_orig typ) in
        let k = Id.new_var ~name:("k" ^ k_post) (TFun(r,typ_result)) in
        let p = Id.new_var ~name:"p" (trans_typ sol typ_excep t1.t_orig.typ t1.typ_cps) in
        let e = Id.new_var ~name:"e" typ_excep in
        let h = Id.new_var ~name:"h" (TFun(e,typ_result)) in
        let t1' = transform sol typ_excep k_post t1 in
        let open Term in
        fun_ k
          (fun_ h
             (app (sol t1.effect) t1'
                ~h:(var h)
                ~k:(fun_ p (var k @ [proj i (var p)]))))
    | TupleCPS ts, ENone ->
        make_tuple @@ List.map (transform sol typ_excep k_post) ts
    | TupleCPS ts, ECont ->
        let r = Id.new_var ~name:"r" (trans_typ sol typ_excep typ_orig typ) in
        let k = Id.new_var ~name:("k" ^ k_post) (TFun(r,typ_result)) in
        let xs = List.map (fun t -> Id.new_var @@ trans_typ sol typ_excep t.t_orig.typ t.typ_cps) ts in
        let t' = Term.(var k @ [tuple (vars xs)]) in
        let aux t_acc x t = app (sol t.effect) (transform sol typ_excep k_post t) ~k:(make_fun x t_acc) in
        make_fun k @@ List.fold_left2 aux t' xs ts
    | TupleCPS ts, EExcep ->
        let r = Id.new_var ~name:"r" (trans_typ sol typ_excep typ_orig typ) in
        let k = Id.new_var ~name:("k" ^ k_post) (TFun(r,typ_result)) in
        let xs = List.map (fun t -> Id.new_var @@ trans_typ sol typ_excep t.t_orig.typ t.typ_cps) ts in
        let e = Id.new_var ~name:"e" typ_excep in
        let h = Id.new_var ~name:"h" (TFun(e,typ_result)) in
        let h' = Id.new_var_id h in
        let t' = Term.(var k @ [tuple (vars xs)]) in
        let aux t_acc x t = app (sol t.effect) (transform sol typ_excep k_post t) ~k:(make_fun x t_acc) ~h:(make_var h') in
        let open Term in
        fun_ k
          (fun_ h
             (let_ [h', var h] (* to prevent the increase of code size in eta-reduction(???) *)
                (List.fold_left2 aux t' xs ts)))
    | RaiseCPS t1, EExcep ->
        let u = Id.new_var ~name:"u" (trans_typ sol typ_excep typ_orig typ) in
        let k = Id.new_var ~name:"k" (TFun(u,typ_result)) in
        let e = Id.new_var ~name:"e" typ_excep in
        let h = Id.new_var ~name:"h" (TFun(e,typ_result)) in
        let h' = Id.new_var_id h in
        let t1' = transform sol typ_excep k_post t1 in
        let open Term in
        fun_ k
          (fun_ h
             (let_ [h', var h] (* to prevent the increase of code size in eta-reduction *)
                (app (sol t1.effect) t1' ~k:(var h') ~h:(var h'))))
    | TryWithCPS(t1,t2), ENone ->
        transform sol typ_excep k_post t1
    | TryWithCPS(t1,t2), ECont ->
        transform sol typ_excep k_post t1
    | TryWithCPS(t1,t2), EExcep ->
        let r = Id.new_var ~name:"r" (trans_typ sol typ_excep t1.t_orig.typ t1.typ_cps) in
        let f = Id.new_var ~name:"h" (trans_typ sol typ_excep t2.t_orig.typ t2.typ_cps) in
        let k = Id.new_var ~name:"k" (TFun(r,typ_result)) in
        let e = Id.new_var ~name:"e" typ_excep in
        let h = Id.new_var ~name:"h" (TFun(e,typ_result)) in
        let t1' = transform sol typ_excep k_post t1 in
        let t2' = transform sol typ_excep k_post t2 in
        assert (sol t2.effect = ENone); (* bind h' to h when eliminating this assertion *)
        let open Term in
        fun_ k
          (fun_ h
             (app (sol t1.effect) t1'
                ~k:(var k)
                ~h:(fun_ e
                      (app (sol t2.effect) t2'
                         ~h:(var h)
                         ~k:(fun_ f
                               (app (sol (get_tfun_effect t2.typ_cps))
                                  (var f @ [var e]) ~k:(var k) ~h:(var h)))))))
    | t, e -> (Format.eprintf "%a, %a@." (print_t_cps sol) t print_effect e; assert false)
  in
  {t' with attr=t_orig.attr}


let rec col_exn_typ {t_cps=t} =
  match t with
  | ConstCPS _ -> []
  | BottomCPS -> []
  | RandIntCPS _ -> []
  | RandValueCPS _ -> []
  | VarCPS _ -> []
  | FunCPS(_, t1) -> col_exn_typ t1
  | AppCPS(t1, t2) -> col_exn_typ t1 @ col_exn_typ t2
  | IfCPS(t1, t2, t3) -> col_exn_typ t1 @ col_exn_typ t2 @ col_exn_typ t3
  | LetCPS(bindings, t2) -> List.fold_left (fun acc (_,t) -> col_exn_typ t @ acc) (col_exn_typ t2) bindings
  | BinOpCPS(_, t1, t2) -> col_exn_typ t1 @ col_exn_typ t2
  | NotCPS t1 -> col_exn_typ t1
  | EventCPS _ -> []
  | ProjCPS(_, t1) -> col_exn_typ t1
  | TupleCPS ts -> List.fold_left (fun acc t -> col_exn_typ t @ acc) [] ts
  | RaiseCPS t -> t.typ_cps :: col_exn_typ t
  | TryWithCPS(t1, t2) -> col_exn_typ t1 @ col_exn_typ t2
let unify_exn_typ env typ_exn typed =
  let typs = col_exn_typ typed in
  List.iter (unify env typ_exn) typs


let rec assoc_typ_cps f {t_cps=t; typ_cps=typ; effect=e} =
  match t with
  | ConstCPS _ -> []
  | BottomCPS -> []
  | RandIntCPS _ -> []
  | RandValueCPS typ -> []
  | VarCPS x -> []
  | FunCPS(x, t1) ->
      assoc_typ_cps f t1
  | AppCPS(t1, t2) ->
      assoc_typ_cps f t1 @@@ assoc_typ_cps f t2
  | IfCPS(t1, t2, t3) ->
      assoc_typ_cps f t1 @@@ assoc_typ_cps f t2 @@@ assoc_typ_cps f t3
  | LetCPS(bindings, t1) ->
      let aux (g,t) =
        let typs1 = if Id.(f = g.id_cps) then [g.id_typ] else [] in
        typs1 @@@ assoc_typ_cps f t
      in
      assoc_typ_cps f t1 @@@ List.rev_flatten_map aux bindings
  | BinOpCPS(op, t1, t2) ->
      assoc_typ_cps f t1 @@@ assoc_typ_cps f t2
  | NotCPS t1 ->
      assoc_typ_cps f t1
  | EventCPS s -> []
  | ProjCPS(_, t1) ->
      assoc_typ_cps f t1
  | TupleCPS ts ->
      List.rev_flatten @@ List.map (assoc_typ_cps f) ts
  | RaiseCPS t1 ->
      assoc_typ_cps f t1
  | TryWithCPS(t1,t2) ->
      assoc_typ_cps f t1 @@@ assoc_typ_cps f t2

let assoc_typ_cps f typed =
  match assoc_typ_cps f typed with
  | [] -> raise Not_found
  | [typ] -> typ
  | typs ->
      Debug.printf "%a: %d@." Id.print f (List.length typs);
      assert false


let rec uncps_ref_type sol typ_exn rtyp e etyp =
  let dbg = 0=0 in
  let pr () =
    Debug.printf "rtyp:%a@." RT.print rtyp;
    Debug.printf "ST(rtyp):%a@." Print.typ @@ RT.to_simple rtyp;
    Debug.printf "e:%a@." print_effect e;
    Debug.printf "etyp:%a@.@." (print_typ_cps sol) etyp
  in
  pr ();
  match rtyp, e, etyp with
  | RT.Inter(styp, rtyps), e, _ ->
      if dbg then Debug.printf "%s@.@." __LOC__;
      let typs = List.map (fun rtyp1 -> uncps_ref_type sol typ_exn rtyp1 e etyp) rtyps in
      let styp' =
        match typs with
        | [] -> RT.to_simple @@ uncps_ref_type sol typ_exn (RT.of_simple styp) e etyp
        | typ'::_ -> RT.to_simple typ'
      in
      RT.Inter(styp', typs)
  | RT.Base(b,x,ps), ENone, TBaseCPS _ ->
      if dbg then Debug.printf "%s@.@." __LOC__;
      RT.Base(b,x,ps)
  | RT.Fun(x,rtyp1,rtyp2), ENone, TFunCPS(e,etyp1,etyp2) when sol e <> EExcep ->
      if dbg then Debug.printf "%s@.@." __LOC__;
      let rtyp1' = uncps_ref_type sol typ_exn rtyp1 ENone etyp1 in
      let x' = Id.set_typ x @@ RT.to_simple rtyp1' in
      let rtyp2' = RT.subst_var x x' @@ uncps_ref_type sol typ_exn rtyp2 (sol e) etyp2 in
      RT.Fun(x', rtyp1', rtyp2')
  | RT.Fun(x,rtyp1,rtyp2), ENone, TFunCPS(e,etyp1,etyp2) ->
      if dbg then Debug.printf "%s@.@." __LOC__;
      assert (sol e = EExcep);
      let rtyp1' = uncps_ref_type sol typ_exn rtyp1 ENone etyp1 in
      let x' = Id.set_typ x @@ RT.to_simple rtyp1' in
      let rtyp2' = RT.subst_var x x' @@ uncps_ref_type sol typ_exn rtyp2 EExcep etyp2 in
      RT.Fun(x', rtyp1', rtyp2')
  | RT.Fun(_, RT.Fun(_,rtyp,RT.Base(RT.Unit,_,_)), RT.Base(RT.Unit,_,_)),
    ECont, _ ->
      if dbg then Debug.printf "%s@.@." __LOC__;
      uncps_ref_type sol typ_exn rtyp ENone etyp
  | RT.Fun(_, RT.Fun(_,rtyp1, RT.Base(RT.Unit,_,_)), RT.Fun(_,RT.Fun(_,rtyp2,RT.Base(RT.Unit,_,_)), RT.Base(RT.Unit,_,_))),
    EExcep, _ ->
      if dbg then Debug.printf "%s@.@." __LOC__;
      let rtyp1' = uncps_ref_type sol typ_exn rtyp1 ENone etyp in
      let rtyp2' = uncps_ref_type sol typ_exn rtyp2 ENone typ_exn in
      RT.Exn(rtyp1', rtyp2')
  | RT.Fun(_, RT.Fun(_,rtyp1, RT.Base(RT.Unit,_,_)), RT.Fun(_,RT.Inter(_,rtyps), RT.Base(RT.Unit,_,_))),
    EExcep, _ ->
      if dbg then Debug.printf "%s@.@." __LOC__;
      let rtyp1' = uncps_ref_type sol typ_exn rtyp1 ENone etyp in
      let aux = function
        | RT.Fun(_,rtyp2,RT.Base(RT.Unit,_,_)) -> uncps_ref_type sol typ_exn rtyp2 ENone typ_exn
        | _ -> assert false
      in
      let styp' =
        match List.map aux rtyps with
        | [] -> typ_of_etyp etyp
        | rtyp'::_ -> RT.to_simple rtyp'
      in
      RT.Exn(rtyp1', RT.union styp' @@ List.map aux rtyps)
  | RT.Fun(_, RT.Inter(typ,rtyps), RT.Base(RT.Unit,_,_)), ECont, _ ->
      if dbg then Debug.printf "%s@.@." __LOC__;
      let aux = function
        | RT.Fun(_,rtyp1,RT.Base(RT.Unit,_,_)) -> uncps_ref_type sol typ_exn rtyp1 ENone etyp
        | _ -> assert false
      in
      let styp' =
        match List.map aux rtyps with
        | [] -> typ_of_etyp etyp
        | rtyp'::_ -> RT.to_simple rtyp'
      in
      RT.union styp' @@ List.map aux rtyps
  | RT.Tuple xrtyps, _, TTupleCPS etyps ->
      if dbg then Debug.printf "%s@.@." __LOC__;
      RT.Tuple (List.map2 (fun (x,rtyp) etyp -> x, uncps_ref_type sol typ_exn rtyp e etyp) xrtyps etyps)
  | RT.ExtArg(x,rtyp1,rtyp2), _, _ ->
      if dbg then Debug.printf "%s@.@." __LOC__;
      RT.ExtArg(x, rtyp1, uncps_ref_type sol typ_exn rtyp2 e etyp)
  | _, _, TBaseCPS styp when RT.is_top' rtyp ->
      RT.top styp
  | _, _, TBaseCPS styp when RT.is_bottom' rtyp ->
      RT.bottom styp
(*
  | RT.Fun(x, RT.Inter(_,[rtyp]), rtyp'), _, _ -> uncps_ref_type sol typ_exn (RT.Fun(x, rtyp, rtyp')) e etyp
  | RT.Fun(x, RT.Inter(styp,rtyp::rtyps), rtyp'), _, _ when List.exists (Ref_type.equiv rtyp) rtyps -> uncps_ref_type sol typ_exn (RT.Fun(x, RT.Inter(styp,rtyps), rtyp')) e etyp
  | RT.Fun(x, RT.Inter(styp,(RT.Tuple _::_ as rtyps)), rtyp'), _, _ ->
      assert false;
      let xtypss = List.map (function RT.Tuple xtyps -> xtyps | _ -> assert false) rtyps in
      let xs,typss =
        match xtypss with
        | [] -> assert false
        | xtyps::ytypss ->
            let xs = List.map fst xtyps in
            let rename ytyps =
              List.fold_right2 (fun x (y,typ) acc -> let sbst = RT.subst_var x y in sbst typ :: List.map sbst acc) xs ytyps []
            in
            xs, List.map snd xtyps :: List.map rename ytypss
      in
      let typss' = List.transpose typss in
      let styp = RT.to_simple @@ List.hd rtyps in
      let rtyp'' = RT.Tuple (List.map2 (fun x typs -> x, RT.Inter(styp, typs)) xs typss') in
      uncps_ref_type sol typ_exn rtyp'' e etyp
*)
  | RT.Fun(x, RT.Inter(styp,[]), rtyp'), _, _ ->
      assert false
  | RT.Fun(x, RT.Inter(styp,rtyps), rtyp'), _, _ ->
      let rtyps = List.map (fun rtyp -> uncps_ref_type sol typ_exn (RT.Fun(x, rtyp, rtyp')) e etyp) rtyps in
      RT.Union(RT.to_simple @@ List.hd rtyps, rtyps)
  | _ -> assert false

let infer_effect env t =
  let eq x y = Id.(x = y) && (can_unify (Id.typ x) (Id.typ y) || Id.typ x = Id.typ y) in
  let ext_funs = get_fv ~eq t in
  if List.length ext_funs <> List.length (List.unique ~eq:Id.eq ext_funs) then
    begin
      List.iter (fun x -> Format.eprintf "%a: %a@." Id.print x Print.typ (Id.typ x)) ext_funs;
      unsupported "polymorphic use of external functions";
    end;
  let tenv = List.map (Pair.make Id.to_string (Id.typ |- infer_effect_typ env |- force_cont)) ext_funs in
  infer_effect env tenv t

let make_get_rtyp sol typ_exn typed get_rtyp f =
  let etyp = assoc_typ_cps f typed in
  let rtyp = get_rtyp f in
  Debug.printf "%a:@.rtyp:%a@.etyp:%a@.@." Id.print f RT.print rtyp (print_typ_cps sol) etyp;
  let rtyp' = Ref_type.map_pred Trans.reconstruct @@ uncps_ref_type sol typ_exn rtyp ENone etyp in
  if !!Flag.Debug.print_ref_typ then
    Format.eprintf "CPS ref_typ: %a: @[@[%a@]@ ==>@ @[%a@]@]@." Id.print f RT.print rtyp RT.print rtyp';
  rtyp'

let exists_let = make_col false (||)
let exists_let_desc desc =
  match desc with
  | Local(Decl_let _, _) -> true
  | _ -> exists_let.col_desc_rec desc

let () = exists_let.col_desc <- exists_let_desc
let exists_let = exists_let.col_term

(*
let inline_affine = make_trans2 ()

let inline_affine_term (vars,env) t =
  let t' =
    match t.desc with
    | App({desc=Var f}, ts) ->
        begin
          try
            let xs,t = Id.assoc f env in
            let ts' = List.map (inline_affine.tr2_term (vars,env)) ts in
            if List.length xs = List.length ts
            then List.fold_right2 (Trans.subst_with_rename ~check:true) xs ts' t
            else raise Not_found
          with Not_found -> inline_affine.tr2_term_rec (vars,env) t
        end
    | Let(bindings, t1) ->
        let not_rand_int t =
          match t.desc with
          | App({desc=Const(RandValue(TInt,_)); attr}, _) -> not @@ List.mem AAbst_under attr
          | _ -> true
        in
        let affine f xs t =
          let fv = get_fv ~cmp:(fun _ _ -> false) t in
          fv = List.unique fv && List.Set.subset ~eq:Id.eq fv (xs@vars) && not @@ exists_let t
        in
        let check f xs t1 = not_rand_int t1 && affine f xs t1 && not @@ List.mem ADoNotInline t.attr in
        let vars' = List.map Triple.fst bindings @ vars in
        let env' = List.filter_map (fun (f,xs,t) -> if check f xs t then Some (f,(xs,t)) else None) bindings @ env in
        let bindings' = List.map (fun (f,xs,t) -> f, xs, inline_affine.tr2_term (xs@vars',env') t) bindings in
        make_let bindings' @@ inline_affine.tr2_term (vars',env') t1
    | Fun(x, t) -> make_fun x @@ inline_affine.tr2_term (x::vars,env) t
    | _ -> inline_affine.tr2_term_rec (vars,env) t
  in
  {t' with attr=t.attr}

let () = inline_affine.tr2_term <- inline_affine_term
let inline_affine t =
  t
  |> inline_affine.tr2_term ([],[])
  |> Trans.elim_unused_let
  |> Trans.inline_var_const
 *)

let has_typ_result =
  let has_typ_result = make_col false (||) in
  let has_typ_result_typ typ =
    if typ = typ_result then
      true
    else
      has_typ_result.col_typ_rec typ
  in
  has_typ_result.col_typ <- has_typ_result_typ;
  has_typ_result.col_typ


let initial_sol n = if n = 0 then ECont else EUnknown

let initial_env () =
  let counter = 0 in
  let constraints = [CGeq(0, ECont)] in
  {counter; constraints}


let pr2 s p t = Debug.printf "##[CPS] %a:@.%a@.@." Color.s_red s p t
let pr s t = pr2 s Print.term_typ t

let trans_force_typ typ_excep typ =
  infer_effect_typ !!initial_env typ
  |> force_cont
  |> trans_typ initial_sol typ_excep typ

let rec trans_ref_typ is_CPS typ =
  let open Ref_type in
  match typ with
  | Base(base, x, t) -> Base(base, x, t)
  | Fun(x, typ1, typ2) ->
      let x' = Id.map_typ (trans_force_typ typ_unknown) x in
      let typ1' = trans_ref_typ is_CPS typ1 in
      let typ2' = trans_ref_typ is_CPS typ2 in
      let r = Id.new_var @@ to_simple typ2' in
      let ret_typ =
        if is_CPS then
          typ_result
        else
          Base(Unit, Id.new_var Ty.unit, true_term)
      in
      let typ' = Fun(r, typ2', ret_typ) in
      let k = Id.new_var @@ to_simple typ' in
      Fun(x', typ1', Fun(k, typ', ret_typ))
  | Tuple xtyps -> Tuple (List.map (Pair.map_snd @@ trans_ref_typ is_CPS) xtyps)
  | Inter(styp, typs) ->
      let typs' = List.map (trans_ref_typ is_CPS) typs in
      let styp' =
        match typs' with
        | [] -> typ_unknown
        | typ'::_ -> RT.to_simple typ'
      in
      Inter(styp', typs')
  | Union(styp, typs) ->
      let typs' = List.map (trans_ref_typ is_CPS) typs in
      let styp' =
        match typs' with
        | [] -> typ_unknown
        | typ'::_ -> RT.to_simple typ'
      in
      Union(styp', typs')
  | Exn(typ1, typ2) ->
      let typ1' = trans_ref_typ is_CPS typ1 in
      let typ2' = trans_ref_typ is_CPS typ2 in
      let r1 = Id.new_var @@ to_simple typ1' in
      let r2 = Id.new_var @@ to_simple typ2' in
      let ret_typ =
        if is_CPS then
          typ_result
        else
          Base(Unit, Id.new_var Ty.unit, true_term)
      in
      let typ_k = Fun(r1, typ1', ret_typ) in
      let typ_h = Fun(r2, typ2', ret_typ) in
      let k = Id.new_var @@ to_simple typ_k in
      let h = Id.new_var @@ to_simple typ_h in
      Fun(k, typ_k, Fun(h, typ_h, ret_typ))
  | _ ->
      Format.eprintf "%a@." Ref_type.print typ;
      assert false

let trans {Problem.term=t; env=rtenv; attr; kind} =
  pr "INPUT" t;
  let env = initial_env () in
  let t =
    t
    |> Trans.short_circuit_eval
    |> Trans.name_read_int (* for disproving termination *)
  in
  let typ_excep = Option.default typ_unknown @@ find_exn_typ t in
  if typ_excep <> typ_unknown && order typ_excep > 0 then unsupported "higher-order exceptions";
  let typ_exn = infer_effect_typ env typ_excep in
  let typed = infer_effect env t in
  pr2 "infer_effect" (print_term initial_sol) typed;
  unify_exn_typ env typ_exn typed;
  let sol = solve_constraints env.constraints in
  if !!Debug.check then check_solution sol env;
  pr2 "infer_effect" (print_term sol) typed;
  let t =
    let typ_excep' =
      Debug.eprintf "typ_excep: %a@." Print.typ typ_excep;
      trans_typ sol typ_unknown typ_excep typ_exn
    in
    let t = transform sol typ_excep' "" typed in
    let x = Id.new_var Ty.unit in
    let e = Id.new_var ~name:"e" typ_excep' in
    let k = make_fun x cps_result in
    let h = make_fun e @@ make_app fail_term_cps [unit_term; k] in
    app (sol typed.effect) t ~k ~h
  in
  let t' =
    t
    |@> pr "CPS"
    |@> Type_check.check ~ty:typ_result
    |> Trans.propagate_typ_arg
    |@> pr2 "propagate_typ_arg" Print.term
    |@> Type_check.check ~ty:typ_result
    |> Trans.beta_reduce
    |@> pr "beta reduce"
    |@> Type_check.check ~ty:typ_result
    |> Trans.beta_affine_fun
    |@> pr "inline affine functions"
    |@> Type_check.check ~ty:typ_result
    |> Trans.expand_let_val
    |@> pr "expand_let_val"
    |@> Type_check.check ~ty:typ_result
    |> Trans.elim_unused_let ~cbv:false
    |> Trans.elim_unused_branch
    |@> pr "elim_unused_let"
    |> Trans.eta_reduce
    |@> pr "elim_reduce"
  in
  let rtenv = List.map (Pair.map_snd @@ trans_ref_typ true) rtenv in
  let attr = Problem.ACPS::attr in
  {Problem.term=t'; env=rtenv; attr; kind}, make_get_rtyp sol typ_exn typed


let trans_as_direct t =
  t
  |> Problem.safety
  |> trans
  |> Pair.map_fst Problem.term
  |> Pair.map_fst Trans.direct_from_CPS


let trans_ref_typ typ = trans_ref_typ true typ
and trans_ref_typ_as_direct typ = trans_ref_typ false typ
