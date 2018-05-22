open Util
open Syntax
open Type
open Term_util

module RT = Ref_type
module Debug = Debug.Make(struct let check = Flag.Debug.make_check __MODULE__ end)

exception Ref_type_not_found

type mode = Default | Allow_recursive | Use_empty_pred

type sub_constr = RT.env * (RT.t * RT.t)

let rec lift env sty =
  match sty with
  | TBase base ->
      let x = Id.new_var sty in
      let base_env = RT.Env.filter_value RT.is_base env in
      let args = x :: RT.Env.keys base_env in
      let pred =
        let pvar = PredVar.new_pvar @@ List.map Id.typ args in
        Term.(var pvar @ vars args)
      in
      RT.Base(base, x, pred)
  | TFun(x,sty2) ->
      let rty1 = lift env @@ Id.typ x in
      let rty2 = lift (RT.Env.add x rty1 env) sty2 in
      RT.Fun(x, rty1, rty2)
  | TTuple xs ->
      let xtyps,_ =
        let aux (acc,env) x =
          let rty = lift env @@ Id.typ x in
          let env' = RT.Env.add x rty env in
          let acc' = acc @ [x, rty] in
          acc', env'
        in
        List.fold_left aux ([],env) xs
      in
      RT.Tuple xtyps
  | TAttr(_,sty') -> lift env sty'
  | _ ->
      Format.eprintf "LIFT: %a@." Print.typ sty;
      assert false
let lift env t =
  lift env t.typ
  |@> Debug.printf "LIFT: (%a): %a@." Print.term t RT.print

let print_env = Print.(list (pair id RT.print))

let is_simple_expr t = is_simple_bexp t || is_simple_aexp t

let print_sub_constrs fm (cs:sub_constr list) =
  let pr fm (env,(rty1,rty2)) =
    let env' = RT.Env.filter_value RT.is_base env in
    Format.fprintf fm "@[<hov 2>%a |- %a <:@ %a@]" RT.Env.print env' RT.print rty1 RT.print rty2
  in
  List.print pr fm cs

let print_constrs fm cs =
  let pr fm (pre,ant) =
    Format.fprintf fm "@[<hov 2>%a |=@ %a@]" (List.print Print.term) pre Print.term ant
  in
  List.print pr fm cs

let simple_expr_ty t =
  let x = new_var_of_term t in
  let base = Option.get @@ decomp_base t.typ in
  RT.Base(base, x, Term.(var x = t))

let const_ty c =
  match c with
  | Unit -> RT.make_base TUnit
  | RandValue(TBase TInt, false) ->
      RT.make_fun (RT.make_base TUnit) (RT.make_base TInt)
  | _ ->
      Format.printf "c: %a@." Print.const c;
      assert false

let rec gen_sub mode env t ty : sub_constr list =
  Debug.printf "env: %a@." RT.Env.print env;
  Debug.printf "t: %a@.@." Print.term t;
  match t.desc with
  | _ when is_simple_expr t ->
      let aty = simple_expr_ty t in
      [env, (aty, ty)]
  | Const c ->
      let aty = const_ty c in
      [env, (aty, ty)]
  | Var x ->
      [env, (RT.Env.assoc x env, ty)]
  | Fun(x, t') ->
      begin
        match ty with
        | RT.Fun(y,ty1,ty2) ->
            let ty2' = RT.subst_var y x ty2 in
            gen_sub mode (RT.Env.add x ty1 env) t' ty2'
        | _ ->
            Format.eprintf "%a cannot unify with %a@." Print.typ (TFun(x,t'.typ)) RT.print ty;
            invalid_arg "gen_sub"
      end
  | App(t1,ts) ->
      let tys = List.map (lift env) ts in
      let t1_ty = List.fold_right RT.make_fun tys ty in
      List.flatten @@ List.map2 (gen_sub mode env) (t1::ts) (t1_ty::tys)
  | If(t1,t2,t3) when is_simple_expr t1 ->
      let y = Id.new_var Ty.int in
      let env2 = RT.Env.add y (RT.Base(TInt, y, t1)) env in
      let env3 = RT.Env.add y (RT.Base(TInt, y, make_not t1)) env in
      gen_sub mode env2 t2 ty @ gen_sub mode env3 t3 ty
  | If(t1,t2,t3) ->
      let x = Id.new_var Ty.bool in
      gen_sub mode env Term.(let_ [x,t1] (if_ (var x) t2 t3)) ty
  | Local(Decl_let bindings, t1) ->
      let tys =
        match mode with
        | Default ->
            let aux (x,t) =
              if Id.mem x @@ get_fv t then raise Ref_type_not_found;
              lift env t
            in
            List.map aux bindings
        | Allow_recursive ->
            List.map (snd |- lift env) bindings
        | Use_empty_pred -> unsupported "Ref_type_check.Use_empty_pred"
      in
      let sub1 =
        let env' = List.fold_right2 (fst |- RT.Env.add) bindings tys env in
        gen_sub mode env' t1 ty
      in
      let sub2 =
        let ts = List.map snd bindings in
        let env' =
          let cons (x,_) ty env = RT.Env.add x ty env in
          List.fold_right2 cons bindings tys env
        in
        List.flatten @@ List.map2 (gen_sub mode env') ts tys
      in
      sub1 @ sub2
  | BinOp(op,t1,t2) when is_simple_expr t1 ->
      let x2 = new_var_of_term t2 in
      gen_sub mode env Term.(let_ [x2,t2] (make_binop op t1 (var x2))) ty
  | BinOp(op,t1,t2) ->
      let x1 = new_var_of_term t1 in
      gen_sub mode env Term.(let_ [x1,t1] (make_binop op (var x1) t2)) ty
  | Not t1 ->
      let x = new_var_of_term t1 in
      gen_sub mode env Term.(let_ [x,t1] (not (var x))) ty
  | Event("fail",false) ->
      let x = Id.new_var Ty.unit in
      let aty = RT.Fun(x, RT.Base(TUnit, x, Term.false_), RT.Base(TUnit, x, Term.false_)) in
      [env, (aty, ty)]
  | Tuple ts when List.for_all is_var ts ->
      let aty =
        let xs = List.map (Option.get -| decomp_var) ts in
        let atys = List.map (RT.Env.assoc -$- env) xs in
        RT.Tuple (List.combine xs atys)
      in
      [env, (aty, ty)]
  | Tuple ts ->
      let xs = List.map new_var_of_term ts in
      let t0 = Term.(tuple (vars xs)) in
      let t' = List.fold_right2 (fun x t acc -> Term.(let_ [x,t] acc)) xs ts t0 in
      gen_sub mode env t' ty
  | Proj(i,{desc=Var x}) ->
      let env',aty =
        match RT.Env.assoc x env with
        | RT.Tuple xtys ->
            let xtys1,xtys2 = List.takedrop i xtys in
            RT.Env.of_list @@ List.rev xtys1,
            snd @@ List.hd xtys2
        | _ -> assert false
      in
      assert (List.Set.disjoint ~eq:Id.eq (RT.Env.dom env') (RT.Env.dom env));
      [RT.Env.merge env' env, (aty, ty)]
  | Proj(i,t1) ->
      let x = new_var_of_term t1 in
      gen_sub mode env Term.(let_ [x,t1] (proj i (var x))) ty
  | Bottom -> []
  | Local _ -> unsupported __LOC__
  | Label _ -> unsupported __LOC__
  | Module _ -> unsupported __LOC__
  | Event _ -> unsupported __LOC__
  | Record _ -> unsupported __LOC__
  | Field _ -> unsupported __LOC__
  | SetField _ -> unsupported __LOC__
  | Nil -> unsupported __LOC__
  | Cons _ -> unsupported __LOC__
  | Constr _ -> unsupported __LOC__
  | Match _ -> unsupported __LOC__
  | Raise _ -> unsupported __LOC__
  | TryWith _ -> unsupported __LOC__
  | Ref _ -> unsupported __LOC__
  | Deref _ -> unsupported __LOC__
  | SetRef _ -> unsupported __LOC__
  | TNone -> unsupported __LOC__
  | TSome _ -> unsupported __LOC__


let denote_ty x ty =
  match ty with
  | RT.Base(b,y,p) -> subst_var y x p
  | RT.Fun _ -> true_term
  | RT.Tuple _ -> true_term
  | _ -> unsupported __LOC__

let denote_env env =
  env
  |> RT.Env.to_list
  |> List.map (Fun.uncurry denote_ty)
  |> List.filter_out (fun p -> p.desc = Const True)

let rec flatten env (ty1,ty2) =
  match ty1, ty2 with
  | RT.Base(b1,x,_), RT.Base(b2,_,_) ->
      if b1 <> b2 then (Format.eprintf "%a cannot unify with %a@." RT.print ty1 RT.print ty2; invalid_arg "flatten");
      [denote_ty x ty1::denote_env env, denote_ty x ty2]
  | RT.Fun(x1,ty11,ty12), RT.Fun(x2,ty21,ty22) ->
      flatten env (ty21,ty11) @
      flatten (RT.Env.add x2 ty21 env) (RT.subst_var x1 x2 ty12, ty22)
  | RT.Tuple xtys1, RT.Tuple xtys2 ->
      let aux (x1,ty1) (x2,ty2) (env,sbst,acc) =
        let env' = RT.Env.add x2 ty1 env in
        let sbst' = RT.subst_var x1 x2 -| sbst in
        let acc' = flatten env (ty1,ty2) @ acc in
        env', sbst', acc'
      in
      Triple.trd @@ List.fold_right2 aux xtys1 xtys2 (env,Fun.id,[])
  | _ ->
      Format.eprintf "ty1: %a@." RT.print ty1;
      Format.eprintf "ty2: %a@." RT.print ty2;
      unsupported __LOC__

let rec simplify pre1 pre2 ant =
  match pre2 with
  | [] ->
      if ant = true_term then
        []
      else
        [pre1, ant]
  | p::pre2' ->
      match p.desc with
      | Const True -> simplify pre1 pre2' ant
      | BinOp(And, p1, p2) -> simplify pre1 (p1::p2::pre2') ant
      | BinOp(Eq, {desc=Var x}, t) ->
          let sb = subst x t in
          let sbs = List.map sb in
          simplify (sbs pre1) (sbs pre2') (sb ant)
      | _ -> simplify (p::pre1) pre2' ant
let simplify (pre,ant) =
  simplify [] pre ant

let gen_hcs mode env t ty =
  let ty = RT.rename ~full:true ty in
  let env = RT.Env.map_value (RT.rename ~full:true) env in
  Debug.printf "Ref_type_check:@.";
  Debug.printf "  t: %a@." Print.term t;
  Debug.printf "  env: %a@." RT.Env.print env;
  gen_sub mode env t ty
  |@> Debug.printf "Subtyping constraints:@.  @[%a@.@." print_sub_constrs
  |> List.flatten_map (Fun.uncurry flatten)
  |@> Debug.printf "Constraints:@.  @[%a@.@." print_constrs
  |> List.flatten_map simplify
  |@> Debug.printf "Simplified:@.  @[%a@.@." print_constrs
  |> FpatInterface.to_hcs
  |@> Debug.printf "Constraints:@.  @[%a@.@." Fpat.HCCS.pr
  |*@> Fpat.HCCS.save_graphviz "test.dot"

let check ?(mode=Default) env t ty =
  let hcs = gen_hcs mode env t ty in
  Fpat.HCCS.save_smtlib2 "test.smt2" hcs;
  try
    Rec_HCCS_solver.check_sat hcs
  with _ -> false

let print cout ?(mode=Default) env t ty =
  let hcs = gen_hcs mode env t ty in
  let filename = Filename.change_extension !!Flag.mainfile "smt2" in
  Fpat.HCCS.save_smtlib2 filename hcs;
  Printf.fprintf cout "%s" @@ IO.input_file filename
