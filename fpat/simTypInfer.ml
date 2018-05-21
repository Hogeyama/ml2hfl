open Util
open Combinator

(** Simple type inference *)

(*
Hindley-Milner
let rec f_exp env =
  Exp.visit
    (object
        method fvar a id =
          (*
         Format.printf "%a" pr_type_schema (List.assoc id env);
           *)
          let ts, t = instantiate (List.assoc id env) in
          Exp.Var(set_typeinst (set_mltype a t) ts, id)
        method fcon a id =
          let ts, t = instantiate (type_schema_of_con id) in
          Exp.Con(set_typeinst (set_mltype a t) ts, id)
        method ftuple a exps =
          let exps' = List.map (f_exp env) exps in
          Exp.Tuple(set_mltype a (Tuple(List.map (fun exp' -> mltype_of (Exp.attribute exp')) exps')), exps')
        (*‚±‚ê‚Íexp'‚ÌŒ^‚ªVar‚Ì‚Æ‚«‚Ü‚¸‚¢?
        method fsel a exp n =
          let exp' = f_exp env exp in
          let Tuple(ts) = mltype_of (Exp.attribute exp') in
          Exp.Sel(set_mltype a (List.nth ts (n - 1)), exp', n)
         *)
        method fabs a id exp =
          let t1 = gen_tvar () in
          let exp' = f_exp ((id, Forall([], t1))::env) exp in
          let t2 = mltype_of (Exp.attribute exp') in
          Exp.Abs(set_mltype a (Fun(t1, t2)), id, exp')
        method fapp a exp1 exp2 =
          let exp1' = f_exp env exp1 in
          let t1 = mltype_of (Exp.attribute exp1') in
          let exp2' = f_exp env exp2 in
          let t2 = mltype_of (Exp.attribute exp2') in
          let t = gen_tvar () in
          let _ = unify t1 (Fun(t2, t)) in
          Exp.App(set_mltype a t, exp1', exp2')
        method ffix a id exp =
          let t = gen_tvar () in
          let exp' = f_exp ((id, Forall([], t))::env) exp in
          let _ = unify t (mltype_of (Exp.attribute exp')) in
          Exp.Fix(set_mltype a t, id, exp')
        method flet a id exp1 exp2 =
          let exp1' = f_exp env exp1 in
          let t1 = mltype_of (Exp.attribute exp1') in
          let tsc = generalize t1 (TypEnv.fvs env) in
          let exp2' = f_exp ((id, tsc)::env) exp2 in
          let t2 = mltype_of (Exp.attribute exp2') in
          Exp.Let(set_mltype_schema (set_mltype a t2) tsc, id, exp1', exp2')
        method fkon a id x =
          match x with
          | None ->
             let ts, t = instantiate (type_schema_of_kon id) in
             begin
               match shorten t with
               | Adt _ -> ()
               | _ -> raise (Global.Semantic_error (id ^ " takes an argument"))
             end;
             Exp.Kon(set_typeinst (set_mltype a t) ts, id, None)
          | Some(exp) ->
             let ts, t = instantiate (type_schema_of_kon id) in
             let exp' = f_exp env exp in
             let t1 = mltype_of (Exp.attribute exp') in
             let t2 = gen_tvar () in
             let _ = unify t (Fun(t1, t2)) in
             begin
               match shorten t2 with
               | Adt _ -> ()
               | _ -> failwith ("the type of " ^ id ^ " is illegal")
             end;
             Exp.Kon(set_typeinst (set_mltype a t2) ts, id, Some(exp'))
        method fmatch a id ms =
          let ts, t1 = instantiate (List.assoc id env) in
          let t2 = gen_tvar () in
          let ms' =
            List.map
              (function
                | (Pattern.Var(id), exp) ->
                   let exp' = f_exp ((id, Forall([], t1))::env) exp in
                   let _ = unify (mltype_of (Exp.attribute exp')) t2 in
                   Pattern.Var(id), exp'
                | (Pattern.Kon(id, x), exp) ->
                   let ts', t1' = instantiate (type_schema_of_kon id) in
                   (match x with
                      None ->
                      let _ = unify t1 t1' in
                      let exp' = f_exp env exp in
                      let _ = unify (mltype_of (Exp.attribute exp')) t2 in
                      Pattern.Kon((*ts'*)id, None), exp'
                    | Some(id') ->
                       let t = gen_tvar () in
                       let _ = unify (Fun(t, t1)) t1' in
                       let exp' = f_exp ((id', Forall([], t))::env) exp in
                       let _ = unify (mltype_of (Exp.attribute exp')) t2 in
                       Pattern.Kon((*ts'*)id, Some(id')), exp')
                | (Pattern.Tuple(ids), exp) ->
                   let ts = List.map (fun _ -> gen_tvar ()) ids in
                   let _ = unify t1 (Tuple(ts)) in
                   let env' = List.rev (List.map2 (fun id ts -> id, Forall([], ts)) ids ts) in
                   let exp' = f_exp (env' @ env) exp in
                   let _ = unify (mltype_of (Exp.attribute exp')) t2 in
                   Pattern.Tuple(ids), exp')
              ms in
          Exp.Match(set_typeinst (set_mltype a t2) ts, id, ms')
        method ffail a = Exp.Fail(set_mltype a (gen_tvar ()))
        method fascr a exp tau =
          let exp' = f_exp env exp in
          let t = mltype_of (Exp.attribute exp') in
          let _ = unify t (strip tau) in
          Exp.Ascr(set_mltype a t, exp', tau)
      end)
*)

let cgen_mlexp env exp ty = assert false
let cgen_mlexp env exp ty =
  Logger.log_block3 "SimTypInfer.cgen_mlexp" cgen_mlexp env exp ty

let infer_mlexp env exp =
  let ty = Type.new_var () in
  let constr, exp' = cgen_mlexp env exp ty in
  let tsub = constr |> Type.unify in
  TypEnv.subst tsub env,
  Term.subst_tvars tsub exp,
  Type.subst tsub ty
let infer_mlexp env exp =
  Logger.log_block2 "SimTypInfer.infer_mlexp" infer_mlexp env exp

let unify ty1 ty2 =
  assert (ty1 <> Type.mk_top && ty2 <> Type.mk_top);
  if ty1 = ty2 then []
  else
    begin
      Logger.printf2 "adding constraint %a = %a@," Type.pr ty1 Type.pr ty2;
      [ty1, ty2]
    end

let cgen_term f_formula env t ty =
  CunTerm.fold
    (object
      method fvar x rs = fun env ty ->
        let ty' =
          List.assoc_fail
            ~on_failure:(fun () ->
                Format.printf
                  "SimTypInfer.cgen_term:@,%a is not found in {%a}@,"
                  Idnt.pr x
                  TypEnv.pr env)
            x
            env
        in
        let args = List.map (fun _ -> Type.new_var ()) rs in
        let constr, ts =
          List.map2 (fun r ty -> r env ty) rs args
          |> List.split
          |> Pair.map List.concat id
        in
        constr @ unify (Type.mk_fun_args_ret args ty) ty',
        Term.mk_app (Term.mk_var x) ts
      method funit () = fun env ty ->
        unify ty Type.mk_unit, UnitTerm.make
      method ftrue () = fun env ty ->
        unify ty Type.mk_bool, BoolTerm.mk_true
      method ffalse () = fun env ty ->
        unify ty Type.mk_bool, BoolTerm.mk_false
      method fint n = fun env ty ->
        unify ty Type.mk_int, IntTerm.make n
      method fbigint n = fun env ty ->
        unify ty Type.mk_int, IntTerm.of_big_int n
      method frational n1 n2 = fun env ty ->
        unify ty Type.mk_rational, RationalTerm.make n1 n2
      method freal f = fun env ty ->
        unify ty Type.mk_real, RealTerm.make f
      method fstring s = fun env ty ->
        unify ty Type.mk_string, StringTerm.make s
      method fneg _ r1 = fun env ty ->
        let constr1, t1 = r1 env ty in
        constr1, NumTerm.neg ty t1
      method fadd _ r1 r2 = fun env ty ->
        let constr1, t1 = r1 env ty in
        let constr2, t2 = r2 env ty in
        constr1 @ constr2, NumTerm.add ty t1 t2
      method fsub _ r1 r2 = fun env ty ->
        let constr1, t1 = r1 env ty in
        let constr2, t2 = r2 env ty in
        constr1 @ constr2, NumTerm.sub ty t1 t2
      method fmul _ r1 r2 = fun env ty ->
        let constr1, t1 = r1 env ty in
        let constr2, t2 = r2 env ty in
        constr1 @ constr2, NumTerm.mul ty t1 t2
      method fdiv _ r1 r2 = fun env ty ->
        let constr1, t1 = r1 env ty in
        let constr2, t2 = r2 env ty in
        constr1 @ constr2, NumTerm.div ty t1 t2
      method fmax _ r1 r2 = fun env ty ->
        let constr1, t1 = r1 env ty in
        let constr2, t2 = r2 env ty in
        constr1 @ constr2, NumTerm.max ty t1 t2
      method fmin _ r1 r2 = fun env ty ->
        let constr1, t1 = r1 env ty in
        let constr2, t2 = r2 env ty in
        constr1 @ constr2, NumTerm.min ty t1 t2
      method fmod r1 r2 = fun env ty ->
        let constr1, t1 = r1 env Type.mk_int in
        let constr2, t2 = r2 env Type.mk_int in
        constr1 @ constr2 @ unify ty Type.mk_int, IntTerm.mk_mod t1 t2
      method fkon _ x rs = fun env ty ->
        let ty' =
          List.assoc_fail
            ~on_failure:(fun () ->
                Format.printf
                  "SimTypInfer.cgen_term:@,%a is not found in {%a}@,"
                  Idnt.pr x
                  TypEnv.pr env)
            x
            env
        in
        let args = List.map (fun _ -> Type.new_var ()) rs in
        let constr, ts =
          List.map2 (fun r ty -> r env ty) rs args
          |> List.split
          |> Pair.map List.concat id
        in
        constr @ unify (Type.mk_fun_args_ret args ty) ty',
        ADTTerm.mk_kon (x, ty') ts
      method fufun _ x rs = fun env ty ->
        let ty' =
          List.assoc_fail
            ~on_failure:(fun () ->
                Format.printf
                  "SimTypInfer.cgen_term:@,%a is not found in {%a}@,"
                  Idnt.pr x
                  TypEnv.pr env)
            x
            env
        in
        let args = List.map (fun _ -> Type.new_var ()) rs in
        let constr, ts =
          List.map2 (fun r ty -> r env ty) rs args
          |> List.split
          |> Pair.map List.concat id
        in
        constr @ unify (Type.mk_fun_args_ret args ty) ty',
        CunTerm.mk_ufun (x, ty') ts
      method ftuple tys rs = fun env ty ->
        let tys' = List.gen (List.length tys) (fun _ -> Type.new_var ()) in
        let constr, ts =
          List.map2 (fun r ty -> r env ty) rs tys'
          |> List.split
          |> Pair.map List.concat id
        in
        constr @ unify ty (Type.mk_tuple tys'),
        TupTerm.make tys' ts
      method fproj tys i r1 = fun env ty ->
        let tys' = List.gen (List.length tys) (fun _ -> Type.new_var ()) in
        let constr1, t1 = r1 env (Type.mk_tuple tys') in
        constr1 @ unify ty (List.nth tys' i),
        TupTerm.mk_proj tys' i t1
      (* TODO *)
      method faccessor _ x i r = fun env ty ->
        let targ, tret =
          List.assoc_fail
            ~on_failure: (fun () ->
                Format.printf
                  "SimTypInfer.cgen_term:@,%a is not found in {%a}@,"
                  Idnt.pr x
                  TypEnv.pr env)
            x
            env
          |> Type.args_ret
          |> Pair.map (flip List.nth i) id
        in
        let constr, ts = r env tret in
        constr @ unify ty targ,
        ADTTerm.mk_accessor (Type.mk_fun [tret;targ]) x i ts
      method fsempty _ = fun env ty ->
        let ty' = Type.new_var () in
        unify ty (Type.mk_set ty'),
        SetTerm.mk_empty ty'
      method fsadd _ r1 r2 = fun env ty ->
        let ty' = Type.new_var () in
        let constr1, t1 = r1 env ty' in
        let constr2, t2 = r2 env (Type.mk_set ty') in
        constr1 @ constr2 @ unify (Type.mk_set ty') ty,
        SetTerm.mk_add ty' t1 t2
      method fsunion _ r1 r2 = fun env ty ->
        let ty' = Type.new_var () in
        let constr1, t1 = r1 env (Type.mk_set ty') in
        let constr2, t2 = r2 env (Type.mk_set ty') in
        constr1 @ constr2 @ unify (Type.mk_set ty') ty,
        SetTerm.mk_union ty' t1 t2 
      method fsintersect _ r1 r2 = fun env ty ->
        let ty' = Type.new_var () in
        let constr1, t1 = r1 env (Type.mk_set ty') in
        let constr2, t2 = r2 env (Type.mk_set ty') in
        constr1 @ constr2 @ unify (Type.mk_set ty') ty,
        SetTerm.mk_intersect ty' t1 t2 
      method fsdiff _ r1 r2 = fun env ty ->
        let ty' = Type.new_var () in
        let constr1, t1 = r1 env (Type.mk_set ty') in
        let constr2, t2 = r2 env (Type.mk_set ty') in
        constr1 @ constr2 @ unify (Type.mk_set ty') ty,
        SetTerm.mk_diff ty' t1 t2 
      method fscomplement _ r = fun env ty ->
        let ty' = Type.new_var () in
        let constr, t = r env (Type.mk_set ty') in
        constr @ unify (Type.mk_set ty') ty,
        SetTerm.mk_comp ty' t
      method farray ty n rs = assert false
      method faget a n = assert false
      method faset a n m e = assert false
      method fcoerce _ r = fun env ty ->
        let ty' = Type.new_var () in
        let constr, t = r env ty' in
        constr, CunTerm.mk_coerce (Type.mk_fun [ty'; ty]) t
      method fformula phi = fun env ty ->
        (*ty must be bool*)
        let constr, phi = f_formula env phi in
        constr @ unify ty Type.mk_bool, Formula.term_of phi
    end)
    t env ty
let cgen_term =
  Logger.log_block4 "SimTypInfer.cgen_term"
    ~before:(fun _ _ t _ -> Logger.printf "input:@, %a" Term.pr t)
    cgen_term

let cgen_atom f_formula env =
  CunAtom.fold_brel
    (object
      method fvar x ts =
        let ty =
          List.assoc_fail ~on_failure:(fun () ->
              Format.printf
                "SimTypInfer.cgen_atom:@,%a is not found in {%a}@,"
                Idnt.pr x
                TypEnv.pr env)
            x
            env
        in
        let args = List.map (fun _ -> Type.new_var ()) ts in
        let constr, ts' =
          List.map2 (cgen_term f_formula env) ts args
          |> List.split
          |> Pair.map List.concat id
        in
        constr @ unify (Type.mk_fun_args_ret args Type.mk_bool) ty,
        Atom.mk_var x ts'
      method fbrel c t1 t2 =
        let c', ty1, ty2, ret =
          (match c with
           | Const.Eq(ty) ->
             let x =
               (*if Type.is_int ty || Type.is_real ty then
                 ty
                 else*)
               Type.new_var ()
             in
             Const.Eq(x), x, x, Type.mk_bool
           | Const.Neq(ty) ->
             let x =
               (*if Type.is_int ty || Type.is_real ty then
                 ty
                 else*)
               Type.new_var ()
             in
             Const.Neq(x), x, x, Type.mk_bool
           | Const.Lt(ty) ->
             let x =
               (*if Type.is_int ty || Type.is_real ty then
                 ty
                 else*)
               Type.new_var ()
             in
             Const.Lt(x), x, x, Type.mk_bool
           | Const.Gt(ty) ->
             let x =
               (*if Type.is_int ty || Type.is_real ty then
                 ty
                 else*)
                 Type.new_var ()
             in
             Const.Gt(x), x, x, Type.mk_bool
           | Const.Leq(ty) ->
             let x =
               (*if Type.is_int ty || Type.is_real ty then
                 ty
                 else*)
               Type.new_var ()
             in
             Const.Leq(x), x, x, Type.mk_bool
           | Const.Geq(ty) ->
             let x =
               (*if Type.is_int ty || Type.is_real ty then
                 ty
                 else*)
               Type.new_var ()
             in
             Const.Geq(x), x, x, Type.mk_bool
           | c ->
             Logger.printf0 "unexpected error?@,";
             assert false
             (*Const.type_of c*))
        in
        let constr1, t1' = cgen_term f_formula env t1 ty1 in
        let constr2, t2' = cgen_term f_formula env t2 ty2 in
        constr1 @ constr2 @ unify ret Type.mk_bool, Atom.mk_brel c' t1' t2'
      method fdivides n t =
        let constr, t' = cgen_term f_formula env t Type.mk_int in
        constr, IntAtom.divides n t'
      method frecognizer _ x t =
        let _, tret =
          List.assoc_fail
            ~on_failure:(fun () ->
                Format.printf
                  "SimTypInfer.cgen_atom:@,%a is not found in {%a}@,"
                  Idnt.pr x
                  TypEnv.pr env)
            x
            env
          |> Type.args_ret
        in
        let constr1, t' = cgen_term f_formula env t tret in
        constr1, ADTAtom.mk_recognizer (Type.mk_fun [tret; Type.mk_bool]) x t'
      method fsmem _ t1 t2 = 
        let tye = Type.new_var () in
        let constr1, t1' = cgen_term f_formula env t1 tye in
        let constr2, t2' = cgen_term f_formula env t2 (Type.mk_set tye) in
        constr1 @ constr2, SetAtom.mk_mem tye t1' t2'
      method fssubset _ t1 t2 = 
        let ty = Type.new_var () in
        let constr1, t1' = cgen_term f_formula env t1 (Type.mk_set ty) in
        let constr2, t2' = cgen_term f_formula env t2 (Type.mk_set ty) in
        constr1 @ constr2, SetAtom.mk_subset ty t1' t2'
      method fterm c ts =
        let t = Term.mk_app (Term.mk_const c) ts in
        let constr1, t1 = cgen_term f_formula env t Type.mk_bool in
        constr1, t1 |> Atom.of_term
    end)
let cgen_atom = Logger.log_block3 "SimTypInfer.cgen_atom" cgen_atom

let rec cgen_formula env phi =
  Formula.fold
    (object
      method fatom atm = fun env ->
        let constr, atm = cgen_atom cgen_formula env atm in
        constr, Formula.of_atom atm
      method ftrue () = fun env ->
        [], Formula.mk_true
      method ffalse () = fun env ->
        [], Formula.mk_false
      method fnot r1 = fun env ->
        let constr1, phi1 = r1 env in
        constr1, Formula.bnot phi1
      method fand r1 r2 = fun env ->
        let constr1, phi1 = r1 env in
        let constr2, phi2 = r2 env in
        constr1 @ constr2, Formula.band [phi1; phi2]
      method for_ r1 r2 = fun env ->
        let constr1, phi1 = r1 env in
        let constr2, phi2 = r2 env in
        constr1 @ constr2, Formula.bor [phi1; phi2]
      method fimply r1 r2 = fun env ->
        let constr1, phi1 = r1 env in
        let constr2, phi2 = r2 env in
        constr1 @ constr2, Formula.imply phi1 phi2
      method fiff r1 r2 = fun env ->
        let constr1, phi1 = r1 env in
        let constr2, phi2 = r2 env in
        constr1 @ constr2, Formula.mk_iff phi1 phi2
      method fforall (x, _) r1 = fun env ->
        let ty = Type.new_var () in
        let constr1, phi1 = r1 ((x, ty) :: env) in
        constr1, Formula.forall [x, ty] phi1
      method fexists (x, _) r1 = fun env ->
        let ty = Type.new_var () in
        let constr1, phi1 = r1 ((x, ty) :: env) in
        constr1, Formula.exists [x, ty] phi1
      method fbox idx r1 = fun env -> assert false
      method fdiamond idx r1 = fun env -> assert false
      method fmu x r1 = fun env -> assert false
      method fnu x r1 = fun env -> assert false
    end)
    phi env
let cgen_formula = Logger.log_block2 "SimTypInfer.cgen_formula" cgen_formula

let infer_formula tenv phi =
  let tenv' =
    tenv
    @ (Formula.fvs phi @ Formula.coeffs phi @ CunFormula.ufuns_of phi
       |> List.filter (flip List.mem_assoc tenv >> not)
       |> List.unique
       |> TypEnv.of_vars)
  in
  let constr, phi' = cgen_formula tenv' phi in
  let tsub =
    try constr |> Type.unify
    with AbsTerm.CannotUnify ->
      Format.printf
        "unification error:@,  %a@,  @[<v>%a@]@,"
        (List.pr (Pair.pr Type.pr Type.pr) "@,") constr
        Formula.pr phi';
      assert false
  in
  TypEnv.subst tsub tenv', phi' |> Formula.subst_tvars tsub
let infer_formula =
  Logger.log_block2 "SimTypInfer.infer_formula"
    ~after:(Logger.printf "output:@, %a" (Pair.pr TypEnv.pr Formula.pr))
    infer_formula

let infer_formulas tenv phis =
  let tenv' =
    tenv
    @ (List.concat_map
         (fun phi ->
            Formula.fvs phi @ Formula.coeffs phi @ CunFormula.ufuns_of phi)
         phis
       |> List.filter (fun x -> not (List.mem_assoc x tenv))
       |> List.unique
       |> TypEnv.of_vars)
  in
  let ty = Type.new_var () in
  let constr, phis' =
    phis
    |> List.map (cgen_formula tenv')
    |> List.split
    |> Pair.map_fst List.concat
  in
  let tsub =
    try constr |> Type.unify
    with AbsTerm.CannotUnify ->
      Format.printf "unification error:@.  %a@." Formula.pr_list phis;
      assert false
  in
  TypEnv.subst tsub tenv',
  phis' |> List.map (Formula.subst_tvars tsub)
let infer_formulas =
  Logger.log_block2 "SimTypInfer.infer_formulas" infer_formulas

let cgen_pv env pv =
  PredVar.fold
    (fun p tenv ->
       let ty =
         List.assoc_fail
           ~on_failure:(fun () ->
               Format.printf
                 "SimTypInfer.cgen_pv:@,%a is not found in {%a}@,"
                 Idnt.pr p
                 TypEnv.pr env)
           p
           env
       in
       let args = List.map (fun _ -> Type.new_var ()) tenv in
       let constr, tenv' =
         List.map2
           (fun ty (x, _) ->
              unify ty
                (List.assoc_fail
                   ~on_failure:(fun () ->
                       Format.printf
                         "SimTypInfer.cgen_pv:@,%a is not found in {%a}@,"
                         Idnt.pr x
                         TypEnv.pr env)
                   x
                   env),
              (x, ty))
           args tenv
         |> List.split
         |> Pair.map List.concat id
       in
       constr @ unify (Type.mk_fun_args_ret args Type.mk_bool) ty,
       PredVar.make p tenv')
    pv
let cgen_pv = Logger.log_block2 "SimTypInfer.cgen_pv" cgen_pv

let cgen_pva env pva =
  Pva.fold
    (fun p tts ->
       let ty =
         List.assoc_fail
           ~on_failure:(fun () ->
               Format.printf
                 "SimTypInfer.cgen_pva:@,%a is not found in {%a}@,"
                 Idnt.pr p
                 TypEnv.pr env)
           p
           env
       in
       let args = List.map (fun _ -> Type.new_var ()) tts in
       let constr, tts' =
         List.map2
           (fun ty (t, _) ->
              cgen_term cgen_formula env t ty
              |> Pair.map_snd (flip Pair.make ty))
           args tts
         |> List.split
         |> Pair.map List.concat id
       in
       constr @ unify (Type.mk_fun_args_ret args Type.mk_bool) ty,
       Pva.make p tts')
    pva
let cgen_pva = Logger.log_block2 "SimTypInfer.cgen_pva" cgen_pva

let gen_tvar_if_unknown ty =
  if Type.is_unknown ty then Type.mk_var (Idnt.new_tvar ()) else ty

let cgen_horn_clause tenv hc =
  let tenv' =
    (hc |> HornClause.htenv_of |> List.map @@ Pair.map_snd gen_tvar_if_unknown)
    @ (hc |> HornClause.fvs |> TypEnv.of_vars)
    @ tenv
  in
  HornClause.fold
    (fun pvas phi ->
       let constr1, pvas' =
         pvas
         |> List.map (cgen_pva tenv')
         |> List.split
         |> Pair.map List.concat id
       in
       let constr2, phi' = cgen_formula tenv' phi in
       constr1 @ constr2, HornClause.mk_goal pvas' phi')
    (fun pv pvas phi ->
       let constr0, pv' = cgen_pv tenv' pv in
       let constr1, pvas' =
         pvas
         |> List.map (cgen_pva tenv')
         |> List.split
         |> Pair.map List.concat id
       in
       let constr2, phi' = cgen_formula tenv' phi in
       constr0 @ constr1 @ constr2,
       HornClause.mk_def ~tenv:(HornClause.htenv_of hc) pv' pvas' phi')
    hc
let cgen_horn_clause =
  Logger.log_block2 "SimTypInfer.cgen_horn_clause" cgen_horn_clause

let cgen_hccs tenv hcs =
  hcs
  |> List.map (cgen_horn_clause tenv)
  |> List.split
  |> Pair.map List.concat id
let cgen_hccs = Logger.log_block2 "SimTypInfer.cgen_hccs" cgen_hccs

(* type inference *)
(* tenv: constructor's type *)
let infer_hccs tenv hcs =
  let tenv' =
    tenv
    @ (hcs |> HCCS.pvs |> TypEnv.of_vars)
    @ (hcs |> HCCS.coeffs |> TypEnv.of_vars)
    @ (hcs |> HCCS.ufuns |> TypEnv.of_vars)
  in
  (*TypEnv.pr Format.std_formatter tenv';*)
  let constr, hcs' =
    hcs
    |> List.map
      (fun hc ->
         let constr, hc' = cgen_horn_clause tenv' hc in
         let gvs = TypEnv.ftvs tenv' in
         let tsub =
           try
             constr |> Type.unify ~gvs:gvs
           with AbsTerm.CannotUnify ->
             Format.printf "unification error:@.  %a@.  %a@." TypEnv.pr tenv HornClause.pr hc;
             assert false
         in
         List.filter_map
           (fun (x, ty) ->
              if List.mem x gvs then Some(Type.mk_var x, ty) else None)
           tsub,
         HornClause.subst_tvars tsub hc')
    |> List.split
    |> Pair.map List.concat id
  in
  let tsub =
    try
      constr |> Type.unify
    with AbsTerm.CannotUnify ->
      Format.printf "unification error:@.  %a@.  %a@." TypEnv.pr tenv HCCS.pr hcs;
      assert false
  in
  TypEnv.subst tsub tenv',
  HCCS.subst_tvars tsub hcs'
let infer_hccs =
  Logger.log_block2
    "SimTypInfer.infer_hccs"
    ~before:(fun _ -> Logger.printf "input:@,  %a@," HCCS.pr)
    infer_hccs
