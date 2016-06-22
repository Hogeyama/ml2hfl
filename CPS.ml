open Syntax
open Term_util
open Type
open Util

module RT = Ref_type

let debug () = List.mem "CPS" !Flag.debug_module

let counter = ref 0
let new_evar () = incr counter; !counter

type typed_term = {t_cps:t_cps; typ_cps:typ_cps; typ_orig:typ; effect:effect_var}
and typed_ident = {id_cps:id; id_typ:typ_cps}
and t_cps =
  | ConstCPS of const
  | BottomCPS
  | RandIntCPS of bool
  | RandValueCPS of typ
  | VarCPS of typed_ident
  | FunCPS of typed_ident * typed_term
  | AppCPS of typed_term * typed_term
  | IfCPS of typed_term * typed_term * typed_term
  | LetCPS of rec_flag * (typed_ident * typed_term) list * typed_term
  | BinOpCPS of binop * typed_term * typed_term
  | NotCPS of typed_term
  | EventCPS of string
  | ProjCPS of int * typed_term
  | TupleCPS of typed_term list
  | RaiseCPS of typed_term
  | TryWithCPS of typed_term * typed_term
and typ_cps =
  | TBaseCPS of Syntax.typ
  | TFunCPS of effect_var * typ_cps * typ_cps
  | TTupleCPS of typ_cps list
and effect = EUnknown (* for debug *) | ENone | ECont | EExcep
and effect_var = int
and effect_constr =
  | CGeq of effect_var * effect
  | CGeqVar of effect_var * effect_var

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
let init_sol n = if n = 0 then ECont else EUnknown
let sol = ref init_sol

let rec print_typ_cps' fm = function
  | TBaseCPS typ -> Format.fprintf fm "%a" Print.typ typ
  | TFunCPS(e,typ1,typ2) ->
      Format.fprintf fm "(%a -e%d->@ %a)" print_typ_cps' typ1 e print_typ_cps' typ2
  | TTupleCPS typs ->
      Format.fprintf fm "(%a)" (print_list print_typ_cps' " *@ ") typs

let rec print_typ_cps fm = function
  | TBaseCPS typ -> Format.fprintf fm "%a" Print.typ typ
  | TFunCPS(e,typ1,typ2) when !sol e = EUnknown ->
      Format.fprintf fm "(@[%a -%a->@ %a@])" print_typ_cps typ1 print_evar e print_typ_cps typ2
  | TFunCPS(e,typ1,typ2) when !sol e = ENone ->
      Format.fprintf fm "(@[%a ->@ %a@])" print_typ_cps typ1 print_typ_cps typ2
  | TFunCPS(e,typ1,typ2) when !sol e = ECont ->
      Format.fprintf fm "(@[%a =>@ %a@])" print_typ_cps typ1 print_typ_cps typ2
  | TFunCPS(e,typ1,typ2) when !sol e = EExcep ->
      Format.fprintf fm "(@[%a -=>@ %a@])" print_typ_cps typ1 print_typ_cps typ2
  | TFunCPS _ -> assert false
  | TTupleCPS typs ->
      Format.fprintf fm "(%a)" (print_list print_typ_cps " *@ ") typs


and print_typed_termlist fm = List.iter (fun bd -> Format.fprintf fm "@;%a" print_typed_term bd)

and print_typed_term fm {t_cps=t; typ_cps=typ; effect=e} =
  match true, !sol e with
  | true, EUnknown -> Format.fprintf fm "(%a :%a: %a)" print_t_cps t print_evar e print_typ_cps typ
  | true, e -> Format.fprintf fm "(%a :%a: %a)" print_t_cps t (Color.green print_effect) e (Color.cyan print_typ_cps) typ
  | _ -> Format.fprintf fm "(%a : %a)" print_t_cps t print_typ_cps typ

and print_t_cps fm = function
  | ConstCPS c -> Format.fprintf fm "%a" Print.const c
  | BottomCPS -> Format.fprintf fm "_|_"
  | RandIntCPS b -> Format.fprintf fm "rand_int(%b)" b
  | RandValueCPS typ -> Format.fprintf fm "rand_value(%a)" Print.typ typ
  | VarCPS x -> Print.id fm x.id_cps
  | FunCPS(x, t) ->
      Format.fprintf fm "@[<hov 2>fun %a : %a ->@ %a@]" Print.id x.id_cps print_typ_cps x.id_typ print_typed_term t
  | AppCPS(t1, t2) ->
      Format.fprintf fm "%a%a" print_typed_term t1 print_typed_term t2
  | IfCPS(t1, t2, t3) ->
      Format.fprintf fm "@[@[if %a@]@;then @[%a@]@;else @[%a@]@]"
                     print_typed_term t1 print_typed_term t2 print_typed_term t3
  | LetCPS(flag, bindings, t) ->
      let is_rec = match flag with Nonrecursive -> false | Recursive -> true in
      let head = ref (if is_rec then "let rec" else "let") in
      let pr fm (f,t) =
        Format.fprintf fm "@[<hov 2>%s %a : %a =@ @[%a@]@]@;"
                       !head Print.id f.id_cps print_typ_cps f.id_typ print_typed_term t;
        head := "and"
      in
      Format.fprintf fm "@[<v>%a@;in@;%a@]" (print_list pr "") bindings print_typed_term t
  | BinOpCPS(op, t1, t2) ->
      Format.fprintf fm "%a %s %a" print_typed_term t1 (Print.string_of_binop op) print_typed_term t2
  | NotCPS t ->
      Format.fprintf fm "not %a" print_typed_term t
  | EventCPS s -> Format.fprintf fm "{%s}" s
  | ProjCPS(i,t) ->
      Format.fprintf fm "#%d %a" i print_typed_term t
  | TupleCPS ts ->
      Format.fprintf fm "(%a)" (print_list print_typed_term ",@ ") ts
  | RaiseCPS t ->
      Format.fprintf fm "@[raise %a@]" print_typed_term t
  | TryWithCPS(t1,t2) ->
      Format.fprintf fm "@[<hov 2>@[<hov 2>try@ %a@]@ with@ %a@]" print_typed_term t1 print_typed_term t2

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

let rec unify typ1 typ2 =
  match typ1, typ2 with
  | TBaseCPS typ1, TBaseCPS typ2 -> ()
  | TFunCPS(x1,typ11,typ12), TFunCPS(x2,typ21,typ22) ->
      constraints := CGeqVar(x1, x2) :: CGeqVar(x2, x1) :: !constraints;
      unify typ11 typ21;
      unify typ12 typ22
  | TTupleCPS typs1, TTupleCPS typs2 ->
      List.iter2 unify typs1 typs2
  | _ ->
      Format.printf "Bug?@.typ1: %a@.typ2: %a@."
                    print_typ_cps typ1 print_typ_cps typ2;
      assert false

let rec lift_letrec_typ typed =
  match typed.t_cps, typed.typ_cps with
  | FunCPS(_, ({t_cps=FunCPS _} as typed1)), _ ->
      lift_letrec_typ typed1
  | FunCPS _, TFunCPS(e, _, _) ->
      constraints := CGeq(e, ECont) :: !constraints
  | FunCPS _, _ -> assert false
  | _ -> ()

let rec infer_effect_typ typ =
  match typ with
  | TUnit
  | TInt
  | TBool
  | TData _ -> TBaseCPS typ
  | TFun(x,typ2) ->
      let typ1 = Id.typ x in
      let e = new_evar () in
      (match typ2 with TFun _ -> () | _ -> constraints := CGeq(e, ECont) :: !constraints);
      TFunCPS(e, infer_effect_typ typ1, infer_effect_typ typ2)
  | TTuple xs -> TTupleCPS (List.map (infer_effect_typ -| Id.typ) xs)
  | TPred(x,ps) -> infer_effect_typ (Id.typ x)
  | _ -> Format.printf "%a@." Print.typ typ; assert false

let new_var x = {id_cps=x; id_typ=infer_effect_typ (Id.typ x)}

let _TFunCPS(e, typ1, typ2) =
  if !Flag.cps_simpl then constraints := CGeq(e, ECont) :: !constraints;
  TFunCPS(e, typ1, typ2)

let rec infer_effect env t =
  match t.desc with
  | Const(RandValue(TInt,true)) -> assert false
  | Const(RandValue(TInt,false)) ->
      let e = new_evar () in
      let typ = _TFunCPS(e, TBaseCPS TUnit, TBaseCPS TInt) in
      constraints := CGeq(e, ECont) :: !constraints;
      {t_cps=RandIntCPS(List.mem AAbst_under t.attr); typ_cps=typ; typ_orig=t.typ; effect=new_evar()}
  | Const(RandValue(typ, true)) -> assert false
  | Const(RandValue(typ, false)) ->
      let e = new_evar () in
      let typ' = _TFunCPS(e, TBaseCPS TUnit, TBaseCPS typ) in
      constraints := CGeq(e, ECont) :: !constraints;
      {t_cps=RandValueCPS typ; typ_cps=typ'; typ_orig=t.typ; effect=new_evar()}
  | Const c -> {t_cps=ConstCPS c; typ_cps=TBaseCPS t.typ; typ_orig=t.typ; effect=new_evar()}
  | Bottom ->
      let e = new_evar () in
      constraints := CGeq(e, ECont) :: !constraints;
      {t_cps=BottomCPS; typ_cps=infer_effect_typ t.typ; typ_orig=t.typ; effect=e}
  | Var x ->
      let typ =
	try
	  List.assoc (Id.to_string x) env
	with
	| Not_found when Fpat.RefTypInfer.is_parameter (Id.name x) -> TBaseCPS(TInt)
	| Not_found -> Format.printf "%a@." Print.id x; assert false
      in
      {t_cps=VarCPS{id_cps=x;id_typ=typ}; typ_cps=typ; typ_orig=t.typ; effect=new_evar()}
  | Fun(x, t1) ->
      let x_typ = infer_effect_typ (Id.typ x) in
      let x' = {id_cps=x; id_typ=x_typ} in
      let env' = (Id.to_string x, x_typ) :: env in
      let typed = infer_effect env' t1 in
      let typ' = infer_effect_typ t.typ in
      let e,a_typ,r_typ = match typ' with TFunCPS(e,typ1,typ2) -> e,typ1,typ2 | _ -> assert false in
      if !Flag.cps_simpl then constraints := CGeq(e, ECont) :: !constraints;
      constraints := CGeqVar(e, typed.effect) :: !constraints;
      unify a_typ x_typ;
      unify r_typ typed.typ_cps;
      {t_cps=FunCPS(x',typed); typ_cps=typ'; typ_orig=t.typ; effect=new_evar()}
  | App(t1, []) -> assert false
  | App(t1, t2::t3::ts) ->
      let typ = (make_app t1 [t2]).typ in
      infer_effect env {desc=App({desc=App(t1,[t2]);typ;attr=[]}, t3::ts); typ=t.typ; attr=[]}
  | App(t1, [t2]) ->
      let typed1 = infer_effect env t1 in
      let typed2 = infer_effect env t2 in
      let rtyp = infer_effect_typ t.typ in
      let e0 = new_evar () in
      let typ = _TFunCPS(e0, typed2.typ_cps, rtyp) in
      let e = new_evar () in
      constraints := CGeqVar(e, typed1.effect) :: !constraints;
      constraints := CGeqVar(e, typed2.effect) :: !constraints;
      constraints := CGeqVar(e, e0) :: !constraints;
      unify typed1.typ_cps typ;
      {t_cps=AppCPS(typed1,typed2); typ_cps=rtyp; typ_orig=t.typ; effect=e}
  | If(t1, t2, t3) ->
      let typed1 = infer_effect env t1 in
      let typed2 = infer_effect env t2 in
      let typed3 = infer_effect env t3 in
      let e = new_evar () in
      constraints := CGeqVar(e, typed1.effect) :: !constraints;
      constraints := CGeqVar(e, typed2.effect) :: !constraints;
      constraints := CGeqVar(e, typed3.effect) :: !constraints;
      constraints := CGeq(e, ECont) :: !constraints; (* for TRecS *)
      unify typed2.typ_cps typed3.typ_cps;
      {t_cps=IfCPS(typed1,typed2,typed3); typ_cps=typed2.typ_cps; typ_orig=t.typ; effect=e}
  | Let(flag, bindings, t1) ->
      let make_env (f,_,_) = Id.to_string f, infer_effect_typ (Id.typ f) in
      let env_f = List.map make_env bindings in
      let env' = env_f @@@ env in
      let env'' = match flag with Nonrecursive -> env | Recursive -> env' in
      let aux (f, xs, t1) =
        let f' = {id_cps=f; id_typ=List.assoc (Id.to_string f) env_f} in
        let t1' = List.fold_right make_fun xs t1 in
        let typed = infer_effect env'' t1' in
        let () =
          match flag with
            Nonrecursive -> ()
          | Recursive -> lift_letrec_typ typed
        in
        unify f'.id_typ typed.typ_cps;
        f', typed
      in
      let bindings' = List.map aux bindings in
      let typed = infer_effect env' t1 in
      let aux (_,typed) e =
        let e' = new_evar () in
        constraints := CGeqVar(e', typed.effect) :: !constraints;
        constraints := CGeqVar(e', e) :: !constraints;
        e'
      in
      let e = List.fold_right aux bindings' typed.effect in
      {t_cps=LetCPS(flag, bindings', typed); typ_cps=typed.typ_cps; typ_orig=t.typ; effect=e}
  | BinOp(op, t1, t2) ->
      let typed1 = infer_effect env t1 in
      let typed2 = infer_effect env t2 in
      let e = new_evar () in
      unify typed1.typ_cps typed2.typ_cps;
      constraints := CGeqVar(e, typed1.effect) :: !constraints;
      constraints := CGeqVar(e, typed2.effect) :: !constraints;
      {t_cps=BinOpCPS(op,typed1,typed2); typ_cps=TBaseCPS t.typ; typ_orig=t.typ; effect=e}
  | Not t1 ->
      let typed = infer_effect env t1 in
      unify typed.typ_cps (TBaseCPS t.typ);
      {t_cps=NotCPS typed; typ_cps=TBaseCPS t.typ; typ_orig=t.typ; effect=typed.effect}
  | Event(s,true) -> assert false
  | Event(s,false) ->
      let e = new_evar () in
      let typ = _TFunCPS(e, TBaseCPS TUnit, TBaseCPS TUnit) in
      constraints := CGeq(e, ECont) :: !constraints;
      {t_cps=EventCPS s; typ_cps=typ; typ_orig=t.typ; effect=new_evar()}
  | Proj(i,t1) ->
      let typed = infer_effect env t1 in
      let typ = infer_effect_typ t1.typ in
      let typ1 = match typ with TTupleCPS typs -> List.nth typs i | _ -> Format.printf "%a@." Print.term' t1; assert false in
      unify typed.typ_cps typ;
      {t_cps=ProjCPS(i,typed); typ_cps=typ1; typ_orig=t.typ; effect=typed.effect}
  | Tuple ts ->
      let typeds = List.map (infer_effect env) ts in
      let typ = TTupleCPS (List.map (fun typed -> typed.typ_cps) typeds) in
      let e = new_evar () in
      List.iter (fun typed -> constraints := CGeqVar(e, typed.effect) :: !constraints) typeds;
      constraints := CGeq(e, ECont) :: !constraints; (* for remove_pair *)
      {t_cps=TupleCPS typeds; typ_cps=typ; typ_orig=t.typ; effect=e}
  | TryWith(t1, t2) ->
      let typed1 = infer_effect env t1 in
      let typed2 = infer_effect env t2 in
      let e = new_evar () in
      constraints := CGeqVar(e, typed1.effect) :: !constraints;
      {t_cps=TryWithCPS(typed1,typed2); typ_cps=infer_effect_typ t.typ; typ_orig=t.typ; effect=e}
  | Raise t1 ->
      let typed = infer_effect env t1 in
      let e = new_evar () in
      constraints := CGeq(e, EExcep) :: !constraints;
      {t_cps=RaiseCPS typed; typ_cps=infer_effect_typ t.typ; typ_orig=t.typ; effect=e}
  | _ -> assert false


exception Loop of effect_var list

let solve_constraints constrs =
  if debug() then
    begin
      Format.printf "@.CONSTRAINTS:@.";
      List.iter (Format.printf " %a@." print_econstr) constrs;
      Format.printf "@."
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

let check_solution () =
  let check e1 e2 = assert (effect_max e1 e2 = e1) in
  let aux = function
    | CGeqVar(x, y) ->
        let e1 = !sol x in
        let e2 = !sol y in
        Format.printf "%a(%a) :> %a(%a)@." print_evar x print_effect e1 print_evar y print_effect e2;
        check e1 e2
    | CGeq(x, e) ->
        let e1 = !sol x in
        Format.printf "%a(%a) :> %a@." print_evar x print_effect e1 print_effect e;
        check e1 e
  in
  List.iter aux !constraints



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
    | Let(flag, bindings, t2) ->
        let bindings' = List.map (fun (f,xs,t) -> f, xs, add_preds_cont_aux k t) bindings in
        let t2' = add_preds_cont_aux k t2 in
        Let(flag, bindings', t2')
    | BinOp(op, t1, t2) -> BinOp(op, add_preds_cont_aux k t1, add_preds_cont_aux k t2)
    | Not t1 -> Not (add_preds_cont_aux k t1)
    | Event(s,b) -> Event(s,b)
    | Record fields ->  Record (List.map (Pair.map_snd @@ add_preds_cont_aux k) fields)
    | Field(s,t1) -> Field(s,add_preds_cont_aux k t1)
    | SetField(s,t1,t2) -> SetField(s,add_preds_cont_aux k t1,add_preds_cont_aux k t2)
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

let rec trans_typ typ_orig typ =
  match typ_orig,typ with
  | _, TBaseCPS _ -> typ_orig
  | TFun(x_orig,typ), TFunCPS(e,typ1,typ2) when !sol e = EExcep ->
      let typ1' = trans_typ (Id.typ x_orig) typ1 in
      let x = Id.new_var typ1' in
      let r = Id.new_var ~name:"r" (subst_type_var x_orig x (trans_typ typ typ2)) in
      let k = Id.new_var ~name:"k" (TFun(r,typ_result)) in
      let e = Id.new_var ~name:"e" !typ_excep in
      let h = Id.new_var ~name:"h" (TFun(e,typ_result)) in
      TFun(x, TFun(k, TFun(h, typ_result)))
  | TFun(x_orig,typ), TFunCPS(e,typ1,typ2) when !sol e = ECont ->
      let typ1' = trans_typ (Id.typ x_orig) typ1 in
      let x = Id.new_var typ1' in
      let r = Id.new_var ~name:"r" (subst_type_var x_orig x (trans_typ typ typ2)) in
      let k = Id.new_var ~name:"k" (TFun(r,typ_result)) in
      TFun(x, TFun(k, typ_result))
  | TFun(x_orig,typ), TFunCPS(_,typ1,typ2) ->
      let typ1' = trans_typ (Id.typ x_orig) typ1 in
      let x = Id.new_var typ1' in
      let typ2' = subst_type_var x_orig x (trans_typ typ typ2) in
      TFun(x, typ2')
  | TTuple xs, TTupleCPS typs ->
      TTuple (List.map2 (fun x typ -> Id.map_typ (trans_typ -$- typ) x) xs typs)
  | TPred(x,ps), typ -> TPred(Id.map_typ (trans_typ -$- typ) x, ps)
  | _ ->
      Format.printf "%a,%a@." Print.typ typ_orig print_typ_cps typ;
      raise (Fatal "bug? (CPS.trans_typ)")

let trans_var x = Id.map_typ (trans_typ -$- x.id_typ) x.id_cps
let trans_var' x typ = (* for predicates *)
  let x' = trans_var x in
  if same_shape typ @@ Id.typ x'
  then x'
  else Id.set_typ x' typ

let get_tfun_effect = function
  | TFunCPS(e, _, _) -> e
  | _ -> assert false

let make_app_cont e t k =
  match !sol e with
  | EUnknown -> assert false
  | ENone -> make_app k [t]
  | ECont -> make_app t [k]
  | EExcep -> assert false

let make_app_excep e t k h =
  match !sol e with
  | EUnknown -> assert false
  | ENone -> make_app k [t]
  | ECont -> make_app t [k]
  | EExcep -> make_app t [k; h]

let new_k_var k_post typ =
  let r = Id.new_var ~name:"r" typ in
  Id.new_var ~name:("k" ^ k_post) @@ TFun(r,typ_result)

let rec transform k_post {t_cps=t; typ_cps=typ; typ_orig; effect=e} =
  match t, !sol e with
  | ConstCPS c, ENone -> {desc=Const c; typ=typ_orig; attr=const_attr}
  | BottomCPS, ECont ->
      let k = new_k_var k_post @@ trans_typ typ_orig typ in
      make_fun k @@ make_bottom typ_result
  | RandIntCPS b, ENone ->
      let e = get_tfun_effect typ in
      begin
        match !sol e with
        | ECont -> make_randint_cps b
        | EExcep ->
            let e = Id.new_var ~name:"e" !typ_excep in
            let h = Id.new_var ~name:"h" @@ TFun(e,typ_result) in
            make_fun h @@ make_randint_cps b
        | _ -> assert false
      end
  | RandValueCPS typ', ENone ->
      let e = get_tfun_effect typ in
      begin
        match !sol e with
        | ECont -> make_randvalue_cps typ'
        | EExcep ->
            let e = Id.new_var ~name:"e" !typ_excep in
            let h = Id.new_var ~name:"h" (TFun(e,typ_result)) in
            make_fun h (make_randvalue_cps typ_orig)
        | _ -> assert false
      end
  | VarCPS x, ENone -> make_var (trans_var x)
  | FunCPS(x, t1), ENone when !sol (get_tfun_effect typ) = ENone ->
      let x' = trans_var x in
      make_fun x' (transform k_post t1)
  | FunCPS(x, t1), ENone when !sol (get_tfun_effect typ) = ECont ->
      let x' = trans_var x in
      let k = new_k_var k_post @@ trans_typ t1.typ_orig t1.typ_cps in
      let t1' = transform k_post t1 in
      make_fun x' @@ make_fun k @@ make_app_cont t1.effect t1' @@ make_var k
  | FunCPS(x, t1), ENone when !sol (get_tfun_effect typ) = EExcep ->
      let x' = trans_var x in
      let k = new_k_var k_post @@ trans_typ t1.typ_orig t1.typ_cps in
      let e = Id.new_var ~name:"e" !typ_excep in
      let h = Id.new_var ~name:"h" @@ TFun(e,typ_result) in
      let t1' = transform k_post t1 in
      make_fun x' @@ make_fun k @@ make_fun h @@ make_app_excep t1.effect t1' (make_var k) (make_var h)
  | AppCPS(t1, t2), ENone ->
      let t1' = transform k_post t1 in
      let t2' = transform k_post t2 in
      make_app t1' [t2']
  | AppCPS(t1, t2), ECont ->
      let t1' = transform k_post t1 in
      let t2' = transform k_post t2 in
      let k = new_k_var k_post @@ trans_typ typ_orig typ in
      let x1 = Id.new_var (trans_typ t1.typ_orig t1.typ_cps) in
      let x2 = Id.new_var (trans_typ t2.typ_orig t2.typ_cps) in
      let e0 = get_tfun_effect t1.typ_cps in
      make_fun k
        (make_app_cont t2.effect t2'
           (make_fun x2
              (make_app_cont t1.effect t1'
                 (make_fun x1
                    (make_app_cont e0 (make_app (make_var x1) [make_var x2]) (make_var k))))))
  | AppCPS(t1, t2), EExcep ->
      let t1' = transform k_post t1 in
      let t2' = transform k_post t2 in
      let k = new_k_var k_post @@ trans_typ typ_orig typ in
      let x1 = Id.new_var (trans_typ t1.typ_orig t1.typ_cps) in
      let x2 = Id.new_var (trans_typ t2.typ_orig t2.typ_cps) in
      let e = Id.new_var ~name:"e" !typ_excep in
      let h = Id.new_var ~name:"h" (TFun(e,typ_result)) in
      let h' = Id.new_var ~name:"h" (TFun(e,typ_result)) in
      let e0 = get_tfun_effect t1.typ_cps in
      make_fun k
        (make_fun h
           (make_let [h', [], make_var h] (* to prevent the increase of code size in eta-reduction *)
              (make_app_excep t1.effect t1'
                 (make_fun x1
                    (make_app_excep t2.effect t2'
                       (make_fun x2
                          (make_app_excep e0
                             (make_app (make_var x1) [make_var x2]) (make_var k) (make_var h')))
                       (make_var h')))
                 (make_var h'))))
  | IfCPS(t1, t2, t3), ENone ->
      let t1' = transform k_post t1 in
      let t2' = transform k_post t2 in
      let t3' = transform k_post t3 in
      make_if t1' t2' t3'
  | IfCPS(t1, t2, t3), ECont ->
      let t1' = transform k_post t1 in
      let t2' = transform k_post t2 in
      let t3' = transform k_post t3 in
      let r = Id.new_var ~name:"r" (trans_typ typ_orig typ) in
      let k = Id.new_var ~name:("k" ^ k_post) (TFun(r,typ_result)) in
      let k' = Id.new_var ~name:("k" ^ k_post) (TFun(r,typ_result)) in
      let b = Id.new_var ~name:"b" (trans_typ t1.typ_orig t1.typ_cps) in
      make_fun k
        (make_let [k', [], make_var k]
           (make_app_cont t1.effect t1'
              (make_fun b
                 (make_if (make_var b)
                    (make_app_cont t2.effect t2' (make_var k'))
                    (make_app_cont t3.effect t3' (make_var k'))))))
  | IfCPS(t1, t2, t3), EExcep ->
      let t1' = transform k_post t1 in
      let t2' = transform k_post t2 in
      let t3' = transform k_post t3 in
      let r = Id.new_var ~name:"r" (trans_typ typ_orig typ) in
      let k = Id.new_var ~name:("k" ^ k_post) (TFun(r,typ_result)) in
      let k' = Id.new_var ~name:("k" ^ k_post) (TFun(r,typ_result)) in
      let b = Id.new_var ~name:"b" (trans_typ t1.typ_orig t1.typ_cps) in
      let e = Id.new_var ~name:"e" !typ_excep in
      let h = Id.new_var ~name:"h" (TFun(e,typ_result)) in
      let h' = Id.new_var ~name:"h" (TFun(e,typ_result)) in
      make_fun k
        (make_let [k', [], make_var k] (* to prevent the increase of code size in eta-reduction *)
           (make_fun h
              (make_let [h', [], make_var h] (* to prevent the increase of code size in eta-reduction *)
                 (make_app_excep t1.effect t1'
                    (make_fun b
                       (make_if (make_var b)
                          (make_app_excep t2.effect t2' (make_var k') (make_var h'))
                          (make_app_excep t3.effect t3' (make_var k') (make_var h'))))
                    (make_var h')))))
  | LetCPS(flag, bindings, t1), ENone ->
      let aux (f,t) =
        let f' = trans_var f in
        f', [], transform (k_post ^ "_" ^ Id.name f') t
      in
      let bindings' = List.map aux bindings in
      let t1' = transform k_post t1 in
      make_let_f flag bindings' t1'
  | LetCPS(flag, bindings, t1), ECont ->
      let r = Id.new_var ~name:"r" @@ trans_typ typ_orig typ in
      let k = Id.new_var ~name:("k" ^ k_post) @@ TFun(r,typ_result) in
      let aux (f,t) =
        let t' = transform (k_post ^ "_" ^ Id.name f.id_cps) t in
        let f' = trans_var f in
        let f'' =
          if !sol t.effect = ENone
          then f'
          else Id.set_typ f' t'.typ
        in
        f'', [], t'
      in
      let bindings' = List.map aux bindings in
      let t1' = transform k_post t1 in
      let aux (_,t_orig) (f,_,t) t' =
        let f' = Id.new_var ~name:(Id.name f) (trans_typ t_orig.typ_orig t_orig.typ_cps) in
        let t'' = subst_var f f' t' in
        make_app_cont t_orig.effect (make_var f) (make_fun f' t'')
      in
      let t1'' = List.fold_right2 aux bindings bindings' @@ make_app_cont t1.effect t1' (make_var k) in
      make_fun k @@ make_let_f flag bindings' t1''
  | LetCPS(flag, bindings, t1), EExcep ->
      let r = Id.new_var ~name:"r" @@ trans_typ typ_orig typ in
      let k = Id.new_var ~name:("k" ^ k_post) @@ TFun(r,typ_result) in
      let e = Id.new_var ~name:"e" !typ_excep in
      let h = Id.new_var ~name:"h" (TFun(e,typ_result)) in
      let aux (f,t) =
        let t' = transform (k_post ^ "_" ^ Id.name f.id_cps) t in
        let f' = trans_var f in
        let f'' =
          if !sol t.effect = ENone
          then f'
          else Id.set_typ f' t'.typ
        in
        f'', [], t'
      in
      let bindings' = List.map aux bindings in
      let t1' = transform k_post t1 in
      let aux (_,t_orig) (f,_,t) t' =
        let f' = Id.new_var ~name:(Id.name f) (trans_typ t_orig.typ_orig t_orig.typ_cps) in
        let t'' = subst_var f f' t' in
        make_app_excep t_orig.effect (make_var f) (make_fun f' t'') (make_var h)
      in
      make_fun k @@
        make_fun h @@
          make_let_f flag bindings' @@
            List.fold_right2 aux bindings bindings' @@
            make_app_excep t1.effect t1' (make_var k) (make_var h)
  | BinOpCPS(op, t1, t2), ENone ->
      let t1' = transform k_post t1 in
      let t2' = transform k_post t2 in
      {desc=BinOp(op, t1', t2'); typ=typ_orig; attr=[]}
  | BinOpCPS(op, t1, t2), ECont ->
      let r = Id.new_var ~name:"r" (trans_typ typ_orig typ) in
      let k = Id.new_var ~name:("k" ^ k_post) (TFun(r,typ_result)) in
      let x1 = Id.new_var (trans_typ t1.typ_orig t1.typ_cps) in
      let x2 = Id.new_var (trans_typ t2.typ_orig t2.typ_cps) in
      let t1' = transform k_post t1 in
      let t2' = transform k_post t2 in
        make_fun k
          (make_app_cont t1.effect t1'
             (make_fun x1
                (make_app_cont t2.effect t2'
                   (make_fun x2
                      (make_app (make_var k) [{desc=BinOp(op, make_var x1, make_var x2); typ=typ_orig; attr=[]}])))))
  | BinOpCPS(op, t1, t2), EExcep ->
      let r = Id.new_var ~name:"r" (trans_typ typ_orig typ) in
      let k = Id.new_var ~name:("k" ^ k_post) (TFun(r,typ_result)) in
      let x1 = Id.new_var (trans_typ t1.typ_orig t1.typ_cps) in
      let x2 = Id.new_var (trans_typ t2.typ_orig t2.typ_cps) in
      let e = Id.new_var ~name:"e" !typ_excep in
      let h = Id.new_var ~name:"h" (TFun(e,typ_result)) in
      let h' = Id.new_var ~name:"h" (TFun(e,typ_result)) in
      let t1' = transform k_post t1 in
      let t2' = transform k_post t2 in
        make_fun k
          (make_fun h
             (make_let [h', [], make_var h] (* to prevent the increase of code size in eta-reduction *)
                (make_app_excep t1.effect t1'
                   (make_fun x1
                      (make_app_excep t2.effect t2'
                         (make_fun x2
                            (make_app (make_var k)
                               [{desc=BinOp(op, make_var x1, make_var x2); typ=typ_orig; attr=[]}]))
                         (make_var h')))
                   (make_var h'))))
  | NotCPS t1, ENone ->
      let t1' = transform k_post t1 in
        make_not t1'
  | NotCPS t1, ECont ->
      let r = Id.new_var ~name:"r" (trans_typ typ_orig typ) in
      let k = Id.new_var ~name:("k" ^ k_post) (TFun(r,typ_result)) in
      let b = Id.new_var ~name:"b" (trans_typ t1.typ_orig t1.typ_cps) in
      let t1' = transform k_post t1 in
        make_fun k (make_app_cont t1.effect t1' (make_fun b (make_app (make_var k) [make_not (make_var b)])))
  | NotCPS t1, EExcep ->
      let r = Id.new_var ~name:"r" (trans_typ typ_orig typ) in
      let k = Id.new_var ~name:("k" ^ k_post) (TFun(r,typ_result)) in
      let b = Id.new_var ~name:"b" (trans_typ t1.typ_orig t1.typ_cps) in
      let e = Id.new_var ~name:"e" !typ_excep in
      let h = Id.new_var ~name:"h" (TFun(e,typ_result)) in
      let t1' = transform k_post t1 in
      make_fun k
        (make_fun h
           (make_app_excep t1.effect t1'
              (make_fun b (make_app (make_var k) [make_not (make_var b)]))
              (make_var h)))
  | EventCPS s, ENone -> make_event_cps s
  | ProjCPS(i,t1), ENone ->
      make_proj i @@ transform k_post t1
  | ProjCPS(i,t1), ECont ->
      let r = Id.new_var ~name:"r" @@ trans_typ typ_orig typ in
      let k = Id.new_var ~name:("k" ^ k_post) (TFun(r,typ_result)) in
      let p = Id.new_var ~name:"p" (trans_typ t1.typ_orig t1.typ_cps) in
      let t1' = transform k_post t1 in
      make_fun k (make_app_cont t1.effect t1' (make_fun p (make_app (make_var k) [make_proj i (make_var p)])))
  | ProjCPS(i,t1), EExcep ->
      let r = Id.new_var ~name:"r" (trans_typ typ_orig typ) in
      let k = Id.new_var ~name:("k" ^ k_post) (TFun(r,typ_result)) in
      let p = Id.new_var ~name:"p" (trans_typ t1.typ_orig t1.typ_cps) in
      let e = Id.new_var ~name:"e" !typ_excep in
      let h = Id.new_var ~name:"h" (TFun(e,typ_result)) in
      let t1' = transform k_post t1 in
      make_fun k
        (make_fun h
           (make_app_excep t1.effect t1'
              (make_fun p @@ make_app (make_var k) [make_proj i @@ make_var p])
              (make_var h)))
  | TupleCPS ts, ENone ->
      make_tuple @@ List.map (transform k_post) ts
  | TupleCPS ts, ECont ->
      let r = Id.new_var ~name:"r" (trans_typ typ_orig typ) in
      let k = Id.new_var ~name:("k" ^ k_post) (TFun(r,typ_result)) in
      let xs = List.map (fun t -> Id.new_var @@ trans_typ t.typ_orig t.typ_cps) ts in
      let t' = make_app (make_var k) [make_tuple @@ List.map make_var xs] in
      let aux t_acc x t = make_app_cont t.effect (transform k_post t) @@ make_fun x t_acc in
      make_fun k @@ List.fold_left2 aux t' xs ts
  | TupleCPS ts, EExcep ->
      let r = Id.new_var ~name:"r" (trans_typ typ_orig typ) in
      let k = Id.new_var ~name:("k" ^ k_post) (TFun(r,typ_result)) in
      let xs = List.map (fun t -> Id.new_var @@ trans_typ t.typ_orig t.typ_cps) ts in
      let e = Id.new_var ~name:"e" !typ_excep in
      let h = Id.new_var ~name:"h" (TFun(e,typ_result)) in
      let h' = Id.new_var ~name:"h" (TFun(e,typ_result)) in
      let t' = make_app (make_var k) [make_tuple @@ List.map make_var xs] in
      let aux t_acc x t = make_app_excep t.effect (transform k_post t) (make_fun x t_acc) (make_var h') in
      make_fun k
      @@ make_fun h
      @@ make_let [h', [], make_var h] (* to prevent the increase of code size in eta-reduction(???) *)
      @@ List.fold_left2 aux t' xs ts
  | RaiseCPS t1, EExcep ->
      let u = Id.new_var ~name:"u" (trans_typ typ_orig typ) in
      let k = Id.new_var ~name:"k" (TFun(u,typ_result)) in
      let e = Id.new_var ~name:"e" !typ_excep in
      let h = Id.new_var ~name:"h" (TFun(e,typ_result)) in
      let h' = Id.new_var ~name:"h" (TFun(e,typ_result)) in
      let t1' = transform k_post t1 in
      make_fun k
        (make_fun h
           (make_let [h', [], make_var h] (* to prevent the increase of code size in eta-reduction *)
              (make_app_excep t1.effect t1' (make_var h') (make_var h'))))
  | TryWithCPS(t1,t2), ENone ->
      transform k_post t1
  | TryWithCPS(t1,t2), ECont ->
      transform k_post t1
  | TryWithCPS(t1,t2), EExcep ->
      let r = Id.new_var ~name:"r" (trans_typ t1.typ_orig t1.typ_cps) in
      let f = Id.new_var ~name:"h" (trans_typ t2.typ_orig t2.typ_cps) in
      let k = Id.new_var ~name:"k" (TFun(r,typ_result)) in
      let e = Id.new_var ~name:"e" !typ_excep in
      let h = Id.new_var ~name:"h" (TFun(e,typ_result)) in
      let t1' = transform k_post t1 in
      let t2' = transform k_post t2 in
      assert (!sol t2.effect = ENone); (* bind h' to h when eliminating this assertion *)
      make_fun k
        (make_fun h
           (make_app_excep t1.effect t1'
              (make_var k)
              (make_fun e
                 (make_app_excep t2.effect t2'
                    (make_fun f
                       (make_app_excep (get_tfun_effect t2.typ_cps)
                          (make_app (make_var f) [make_var e]) (make_var k) (make_var h)))
                    (make_var h)))))
  | t, e -> (Format.printf "%a, %a@." print_t_cps t print_effect e; assert false)








let rec assoc_typ_cps f {t_cps=t; typ_cps=typ; typ_orig; effect=e} =
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
  | LetCPS(flag, bindings, t1) ->
      let aux (g,t) =
        let typs1 = if Id.same f g.id_cps then [g.id_typ] else [] in
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
  | typs -> Format.printf "%a: %d@." Id.print f (List.length typs); assert false


let rec uncps_ref_type rtyp e etyp =
  let dbg = 0=10 in
  if !!debug then
    Format.printf "rtyp:%a@.e:%a@.etyp:%a@." RT.print rtyp print_effect e print_typ_cps etyp;
  match rtyp, e, etyp with
  | RT.Inter rtyps, ENone, _ ->
      if dbg then Format.printf "%s@.@." __LOC__;
      RT.Inter (List.map (fun rtyp1 -> uncps_ref_type rtyp1 e etyp) rtyps)
  | RT.Base(b,x,ps), ENone, TBaseCPS _ ->
      if dbg then Format.printf "%s@.@." __LOC__;
      RT.Base(b,x,ps)
  | RT.Fun(x,rtyp1,rtyp2), ENone, TFunCPS(e,etyp1,etyp2) ->
      if dbg then Format.printf "%s@.@." __LOC__;
      let rtyp1' = uncps_ref_type rtyp1 ENone etyp1 in
      let rtyp2' = uncps_ref_type rtyp2 (!sol e) etyp2 in
      RT.Fun(x, rtyp1', rtyp2')
  | RT.Fun(_, RT.Fun(_,rtyp,RT.Base(RT.Unit,_,_)), RT.Base(RT.Unit,_,_)),
    ECont, _ ->
      if dbg then Format.printf "%s@.@." __LOC__;
      uncps_ref_type rtyp ENone etyp
  | RT.Fun(_, RT.Fun(_,rtyp, RT.Base(RT.Unit,_,_)), RT.Fun(_,_,RT.Base(RT.Unit,_,_))),
    EExcep, _ -> (* TODO: refine *)
      if dbg then Format.printf "%s@.@." __LOC__;
      uncps_ref_type rtyp ENone etyp
  | RT.Fun(_, RT.Inter rtyps, RT.Base(RT.Unit,_,_)), ECont, _ ->
      if dbg then Format.printf "%s@.@." __LOC__;
      let aux = function
        | RT.Fun(_,rtyp1,RT.Base(RT.Unit,_,_)) -> uncps_ref_type rtyp1 ENone etyp
        | _ -> assert false
      in
      RT.Union (List.map aux rtyps)
  | RT.Tuple xrtyps, _, TTupleCPS etyps ->
      if dbg then Format.printf "%s@.@." __LOC__;
      RT.Tuple (List.map2 (fun (x,rtyp) etyp -> x, uncps_ref_type rtyp e etyp) xrtyps etyps)
  | RT.ExtArg(x,rtyp1,rtyp2), _, _ ->
      if dbg then Format.printf "%s@.@." __LOC__;
      RT.ExtArg(x, rtyp1, uncps_ref_type rtyp2 e etyp)
  | _ ->
      if !!debug then
        Format.printf "@.rtyp:%a@.e:%a@.etyp:%a@." RT.print rtyp print_effect e print_typ_cps etyp;
      assert false

let infer_effect t =
  let cmp x y = Id.same x y && (can_unify (Id.typ x) (Id.typ y) || Id.typ x = Id.typ y) in
  let ext_funs = get_fv ~cmp t in
  if List.length ext_funs <> List.length (List.unique ~cmp:Id.same ext_funs) then
    begin
      List.iter (fun x -> Format.printf "%a: %a@." Id.print x Print.typ (Id.typ x)) ext_funs;
      unsupported "polymorphic use of external functions";
    end;
  let env = List.map (Pair.make Id.to_string (Id.typ |- infer_effect_typ |- force_cont)) ext_funs in
  infer_effect env t

let make_get_rtyp typed get_rtyp f =
  let etyp = assoc_typ_cps f typed in
  let rtyp = get_rtyp f in
  if !!debug then
    Format.printf "%a:@.rtyp:%a@.etyp:%a@.@." Id.print f RT.print rtyp print_typ_cps etyp;
  let rtyp' = uncps_ref_type rtyp ENone etyp in
  if Flag.print_ref_typ_debug then
    Format.printf "CPS: %a: @[@[%a@]@ ==>@ @[%a@]@]@." Id.print f RT.print rtyp RT.print rtyp';
  rtyp'

let initialize () =
  counter := 0;
  sol := init_sol;
  constraints := [CGeq(0, ECont)]


let exists_let = make_col false (||)

let exists_let_desc desc =
  match desc with
  | Let _ -> true
  | _ -> exists_let.col_desc_rec desc

let () = exists_let.col_desc <- exists_let_desc
let exists_let = exists_let.col_term


let inline_affine = make_trans2 ()

let inline_affine_term env t =
  let t' =
  match t.desc with
  | App({desc=Var f}, ts) ->
      begin
        try
          let xs,t = Id.assoc f env in
          let ts' = List.map (inline_affine.tr2_term env) ts in
          if List.length xs = List.length ts
          then List.fold_right2 Trans.subst_with_rename xs ts' t
          else raise Not_found
        with Not_found -> inline_affine.tr2_term_rec env t
      end
  | Let(flag, bindings, t1) ->
      let not_rand_int t =
        match t.desc with
        | App({desc=Const(RandValue(TInt,_)); attr}, _) -> not @@ List.mem AAbst_under attr
        | _ -> true
      in
      let linear f xs t =
        let fv = get_fv ~cmp:(fun _ _ -> false) t in
        fv = List.unique fv && List.Set.subset fv xs && not @@ exists_let t
      in
      let env' = List.filter_map (fun (f,xs,t) -> if not_rand_int t && linear f xs t then Some (f,(xs,t)) else None) bindings @ env in
      let bindings' = List.map (Triple.map_trd @@ inline_affine.tr2_term env') bindings in
      make_let_f flag bindings' @@ inline_affine.tr2_term env' t1
  | _ -> inline_affine.tr2_term_rec env t
  in
  let attr = if List.mem ACPS t.attr then ACPS::t'.attr else t'.attr in
  {t' with attr}

let () = inline_affine.tr2_term <- inline_affine_term
let inline_affine t =
  t
  |> inline_affine.tr2_term []
  |> Trans.elim_unused_let
  |> Trans.inline_var_const


let trans_typ typ = trans_typ typ @@ force_cont @@ infer_effect_typ typ

let pr2 s p t = if !!debug then Format.printf "##[CPS] %a:@.%a@.@." Color.s_red s p t
let pr s t = pr2 s Print.term_typ t

let trans t =
  pr "INPUT" t;
  initialize ();
  typ_excep := trans_typ !typ_excep;
  let t = Trans.short_circuit_eval t in
  let typed = infer_effect t in
  pr2 "infer_effect" print_typed_term typed;
  sol := solve_constraints !constraints;
  if !!debug then check_solution ();
  pr2 "infer_effect" print_typed_term typed;
  let t = transform "" typed in
  let t =
    let x = Id.new_var TUnit in
    let e = Id.new_var ~name:"e" !typ_excep in
    let k = make_fun x cps_result in
    let h = make_fun e @@ make_app fail_term_cps [unit_term; k] in
    make_app_excep typed.effect t k h
  in
  let t' =
    {t with attr = [ACPS]}
    |@> pr "CPS"
    |@> Type_check.check -$- typ_result
    |> Trans.propagate_typ_arg
    |@> pr2 "propagate_typ_arg" Print.term
    |@> Type_check.check -$- typ_result
    |> Trans.beta_reduce
    |@> pr "beta reduce"
    |@> Type_check.check -$- typ_result
    |> inline_affine
    |@> pr "inline affine functions"
    |@> Type_check.check -$- typ_result
    |> Trans.expand_let_val
    |@> pr "expand_let_val"
    |@> Type_check.check -$- typ_result
    |> Trans.elim_unused_let ~cbv:false
    |> Trans.elim_unused_branch
    |@> pr "elim_unused_let"
  in
  t', make_get_rtyp typed


let trans_as_direct t =
  t
  |> trans
  |> Pair.map_fst Trans.direct_from_CPS


let rec trans_ref_typ is_CPS typ =
  let open Ref_type in
  match typ with
  | Base(base, x, t) -> Base(base, x, t)
  | Fun(x, typ1, typ2) ->
      let x' = Id.map_typ trans_typ x in
      let typ1' = trans_ref_typ is_CPS typ1 in
      let typ2' = trans_ref_typ is_CPS typ2 in
      let r = Id.new_var @@ to_simple typ2' in
      let ret_typ =
        if is_CPS then
          typ_result
        else
          Base(Unit, U.dummy_var, true_term)
      in
      let typ' = Fun(r, typ2', ret_typ) in
      let k = Id.new_var @@ to_simple typ' in
      Fun(x', typ1', Fun(k, typ', ret_typ))
  | Tuple xtyps -> Tuple (List.map (Pair.map_snd @@ trans_ref_typ is_CPS) xtyps)
  | Inter typs -> Inter (List.map (trans_ref_typ is_CPS) typs)
  | Union typs -> Union (List.map (trans_ref_typ is_CPS) typs)
  | _ -> assert false

let trans_ref_typ typ = trans_ref_typ true typ
and trans_ref_typ_as_direct typ = trans_ref_typ false typ

let uncps_ref_type typ_cps typ = uncps_ref_type typ_cps ENone @@ force_cont @@ infer_effect_typ @@ Ref_type.to_simple typ
