
open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_print
open CEGAR_util
open Fpat
open Fpat.Combinator
open Fpat.ExtFormula
open Fpat.ExtHornClause

let conv_const c =
  match c with
  | Unit -> Const.Unit
  | True -> Const.True
  | False -> Const.False
  | And -> Const.And
  | Or -> Const.Or
  | Not -> Const.Not
  | Lt -> Const.Lt SimType.int_type
  | Gt -> Const.Gt SimType.int_type
  | Leq -> Const.Leq SimType.int_type
  | Geq -> Const.Geq SimType.int_type
  | EqUnit -> Const.Eq SimType.unit_type
  | EqBool -> Const.Eq SimType.bool_type
  | EqInt -> Const.Eq SimType.int_type
  | Int(n) -> Const.Int(n)
  | RandInt -> Const.RandInt
  | Add -> Const.Add SimType.int_type
  | Sub -> Const.Sub SimType.int_type
  | Mul -> Const.Mul SimType.int_type
  | _ -> Format.printf "%a@." CEGAR_print.const c; assert false

let rec conv_term t =
  match t with
  | Const(Bottom) -> Term.make_var (Var.make (Idnt.make "bottom")) (***)
  | Const(c) -> Term.Const([], conv_const c)
  | Var(x) ->
      if is_parameter x then
        Term.make_var (Var.make_coeff (Idnt.make x))
      else
        Term.make_var (Var.make (Idnt.make x))
  | App(t1, t2) -> Term.apply (conv_term t1) [conv_term t2]
  | Fun _ -> assert false
  | Let _ -> assert false

let inv_const c =
  match c with
  | Const.Unit -> Unit
  | Const.True -> True
  | Const.False -> False
  | Const.And -> And
  | Const.Or -> Or
  | Const.Not -> Not
  | Const.Lt (SimType.Base BaseType.Int) -> Lt
  | Const.Gt (SimType.Base BaseType.Int) -> Gt
  | Const.Leq (SimType.Base BaseType.Int) -> Leq
  | Const.Geq (SimType.Base BaseType.Int) -> Geq
  | Const.Eq (SimType.Base BaseType.Bool) -> EqBool
  | Const.Eq (SimType.Base BaseType.Int) -> EqInt
  | Const.Int(n) -> Int(n)
  | Const.RandInt -> RandInt
  | Const.Add (SimType.Base BaseType.Int) -> Add
  | Const.Sub (SimType.Base BaseType.Int) -> Sub
  | Const.Mul (SimType.Base BaseType.Int) -> Mul
  | _ -> Format.printf "%a@." Const.pr c; assert false

let rec inv_term t =
  match t with
    Term.Const(_, c) -> Const(inv_const c)
  | Term.Var(_, x) -> Var(Var.string_of x)
  | Term.App(_, Term.App(_, t1, t2), t3) ->
      (match t1 with
        Term.Const(_, Const.Neq (SimType.Base BaseType.Unit)) ->
          App(Const(Not), App(App(Const(EqUnit), inv_term t2), inv_term t3))
      | Term.Const(_, Const.Neq (SimType.Base BaseType.Bool)) ->
          App(Const(Not), App(App(Const(EqBool), inv_term t2), inv_term t3))
      | Term.Const(_, Const.Neq (SimType.Base BaseType.Int)) ->
          App(Const(Not), App(App(Const(EqInt), inv_term t2), inv_term t3))
      | _ ->
          App(App(inv_term t1, inv_term t2), inv_term t3))
  | Term.App(_, t1, t2) -> App(inv_term t1, inv_term t2)
  | Term.Forall (_, _, _) -> assert false
  | Term.Error _ -> assert false
  | Term.Ret (_, _, _, _) -> assert false
  | Term.Call (_, _, _) -> assert false
  | Term.Exists (_, _, _) -> assert false
  | Term.If (_, _, _, _) -> assert false
  | Term.LetVal (_, _, _, _) -> assert false
  | Term.LetFun (_, _, _, _, _) -> assert false
  | Term.LetRec (_, _, _, _) -> assert false
  | Term.Lambda (_, _, _) -> assert false



let conv_event e = (***)
  match e with
    Event(x) ->
      assert (x = "fail");
      Term.Const([], Const.Event(Idnt.make x))
  | Branch(_) -> assert false

let conv_fdef (f, args, guard, events, body) =
  { Fdef.attr = [];
    Fdef.name = Idnt.make f;
    Fdef.args = Util.List.map Idnt.make args;
    Fdef.guard = conv_term guard;
    Fdef.body = Util.List.fold_right (fun e t -> Term.apply (conv_event e) [Term.Const([],Const.Unit)]) events (conv_term body) } (***)

let inv_fdef fdef =
  Idnt.string_of fdef.Fdef.name,
  Util.List.map Idnt.string_of fdef.Fdef.args,
  inv_term fdef.Fdef.guard,
  [],
  inv_term fdef.Fdef.body

let rec conv_typ ty =
  match ty with
    TBase(TUnit, _) -> SimType.unit_type
  | TBase(TInt, _) -> SimType.int_type
  | TBase(TBool, _) -> SimType.bool_type
  | TFun(ty1,tmp) ->
      let ty2 = tmp (Const True) in
      SimType.Fun(conv_typ ty1, conv_typ ty2)
  | _ ->
      Format.printf "%a@." CEGAR_print.typ ty;
      assert false

let conv_prog (typs, fdefs, main) =
  { Prog.attr = [];
    Prog.fdefs = Util.List.map conv_fdef fdefs;
    Prog.types = Util.List.map (fun (x, ty) -> Idnt.make x, conv_typ ty) typs;
    Prog.main = Idnt.make main }

let verify fs (*cexs*) prog =
  let prog = prog.CEGAR_syntax.env, prog.CEGAR_syntax.defs, prog.CEGAR_syntax.main in
  let prog = conv_prog prog in
  Format.printf "@[<v>BEGIN verification:@,  @[%a@]@," Prog.pr prog;
  assert false(*Verifier.verify fs prog*);
  Format.printf "END verification@,@]"

let rec inv_abst_type aty =
  match aty with
    AbsType.Base(BaseType.Unit, x, ts) ->
      let x = Var.string_of x in
      TBase(TUnit, fun s -> Util.List.map (fun t -> subst x s (inv_term t)) ts)
  | AbsType.Base(BaseType.Bool, x, ts) ->
      let x = Var.string_of x in
      TBase(TBool, fun s -> Util.List.map (fun t -> subst x s (inv_term t)) ts)
  | AbsType.Base(BaseType.Int, x, ts) ->
      let x = Var.string_of x in
      TBase(TInt, fun s -> Util.List.map (fun t -> subst x s (inv_term t)) ts)
  | AbsType.Fun(aty1, aty2) ->
      let x = if AbsType.is_base aty1 then Var.string_of (AbsType.bv_of aty1) else "_dummy" in
      TFun(inv_abst_type aty1, fun t -> subst_typ x t (inv_abst_type aty2))
(* for add_const
  | AbsType. ... -> TBase(TAbst ..., ...)
*)


let infer flags labeled cexs prog =
  Global.solve_preds_left_to_right := flags land 2 <> 0;
  Global.subst_hcs_inc := flags land 4 <> 0;
  Global.no_inlining := flags land 8 <> 0 || not !Flag.expand_nonrec;
  Global.inline_after_ncs := flags land 16 <> 0;
  Global.fol_backward := flags land 32 <> 0;
  Global.disable_pred_sharing1 := flags land 64 <> 0;
  Global.enable_pred_sharing2 := flags land 128 <> 0;
  Global.flag_coeff := flags land 256 <> 0;

  let prog = conv_prog prog in
  let env = AbsTypeInfer.refine prog labeled cexs in
  Flag.time_parameter_inference := !Flag.time_parameter_inference +. !ParamSubstInfer.elapsed_time;
  Util.List.map
   (fun (f, rty) ->
     match f with Var.V(id) -> Idnt.string_of id, inv_abst_type rty | _ -> assert false)
   env
(*
  Util.List.map
    (fun (f, _) ->
      try
        f, conv_siz_type (Util.List.assoc (Var.make f) env)
      with Not_found ->
        assert false)
    prog.Prog.types
*)

let params = ref []
(** ToDo: exs may contain extra parameters that are not related to the recursive call *)
let new_params recursive bvs exs =
  Util.List.unfold
    (fun i ->
       if i < !Global.number_of_extra_params then
  let bvs' = Util.List.filter (fun x -> x.Id.typ = Type.TInt) bvs in
  let ps =
    Util.List.unfold
      (fun i ->
         if i < (Util.List.length bvs' + if !Global.enable_coeff_const then 1 else 0) then
           Some(Id.new_var Flag.extpar_header Type.TInt, i + 1)
         else
           None)
      0
  in
  params := !params @ ps;
  let xs =
    match recursive with
        None -> []
      | Some(xs) -> xs
  in
  let ts =
    (if !Global.enable_coeff_const (*&& recursive = None*) then
      ParamSubstInfer.masked_params := Var.make_coeff (Idnt.make (Id.to_string (Util.List.hd ps))) :: !ParamSubstInfer.masked_params);
    (if !Global.enable_coeff_const then [Syntax.make_var (Util.List.hd ps)] else []) @
    (*
    let b = recursive <> None && xs = [] && Util.Set.subset bvs' exs in
    *)
    Util.List.map2
      (fun p x ->
        let _ =
          (*if b then
            ()
          else*) if recursive <> None then
          (if xs = [] then
             (if Util.List.mem x exs then
               ParamSubstInfer.masked_params := Var.make_coeff (Idnt.make (Id.to_string p)) :: !ParamSubstInfer.masked_params (*this is necessary for l-length_cps-append.ml*))
          else if not (Util.List.mem x xs) then
            ParamSubstInfer.masked_params := Var.make_coeff (Idnt.make (Id.to_string p)) :: !ParamSubstInfer.masked_params)
           (* how to deal with non-recursive function calls here? *)
          (*else
            if Util.List.mem x exs then
              ParamSubstInfer.masked_params := Var.make_coeff (Idnt.make (Id.to_string p)) :: !ParamSubstInfer.masked_params*)
        in
        Syntax.make_mul (Syntax.make_var p) (Syntax.make_var x))
      (if !Global.enable_coeff_const then Util.List.tl ps else ps)
      bvs'
  in
  if ts = [] then
    Some(Syntax.make_int 0, i + 1)
  else
    Some(Util.List.fold_left Syntax.make_add (Util.List.hd ts) (Util.List.tl ts), i + 1)
  else
    None)
  0

let gen_id =
  let cnt = ref 0 in
  fun () -> cnt := !cnt + 1; string_of_int !cnt

let rec trans_type typ =
  let xs, tyret = Type.decomp_tfun typ in
  let xs' =
    Util.List.flatten
      (Util.List.map
        (fun x ->
          let x' = trans_id x in
          (match x'.Id.typ with
            Type.TFun(_, _) | Type.TPair(_, _)(* ToDo: fix it *) ->
              Util.List.unfold
                (fun i ->
                  if i < !Global.number_of_extra_params then
                    Some(Id.new_var "ex" Type.TInt, i + 1)
                  else
                    None)
                0
          | _ ->
              []) @ [x'])
        xs)
  in
  Util.List.fold_right (fun x ty -> Type.TFun(x,ty)) xs' tyret
and trans_id x = Id.make x.Id.id x.Id.name (trans_type x.Id.typ)


let insert_extra_param t =
  let tmp = get_time() in
  let debug = !Flag.debug_level > 0 in
  ParamSubstInfer.masked_params := [];
  let rec aux rfs bvs exs t =
    let desc =
      match t.Syntax.desc with
        Syntax.Const c -> Syntax.Const c
      | Syntax.Unknown -> Syntax.Unknown
      | Syntax.RandInt b -> Syntax.RandInt b
      | Syntax.RandValue(typ,b) -> Syntax.RandValue(typ,b)
      | Syntax.Var y -> Syntax.Var (trans_id y)
      | Syntax.Fun(y, t1) ->
          let y' = trans_id y in
          let ys =
            match y'.Id.typ with
              Type.TFun(_, _) | Type.TPair(_, _)(* ToDo: fix it *) ->
                Util.List.unfold
                  (fun i ->
                    if i < !Global.number_of_extra_params then
                      Some(Id.new_var ("ex" ^ gen_id ()) Type.TInt, i + 1)
                    else
                      None)
                  0
            | _ ->
                []
          in
          let ys' = ys @ [y'] in
          let rfs = match rfs with [] -> assert false | (f, xxs, recursive)::rfs' -> (f, xxs @ [y', ys], recursive)::rfs' in
          let f, _ =
            Util.List.fold_left
              (fun (f, ty) y -> (fun t -> f {Syntax.desc=Syntax.Fun(y, t); Syntax.typ=ty}), match ty with Type.TFun(_, ty') -> ty' | _ -> assert false)
              ((fun t -> t), trans_type t.Syntax.typ)
              ys'
          in
          let bvs, exs =
            (if true then
              bvs @ ys'
            else
              bvs @ [y']),
              exs @ ys
          in
          (f (aux rfs bvs exs t1)).Syntax.desc
      | Syntax.App(t1, ts) ->
          (match t1.Syntax.desc with Syntax.App(_, _) -> assert false | _ -> ());
          let t1' = aux rfs bvs exs t1 in
          let recursive, xss =
            match t1'.Syntax.desc with
              Syntax.Var(f) ->
                (try
                  let _, xxss, _ = Util.List.find (fun (f', _, recursive) -> recursive && Id.same f' f) rfs in
                  (if debug then
                    Format.printf "rec: %a@." Syntax.pp_print_term t1');
                  let xxss = Util.List.take (Util.List.length ts) xxss in
                  true,
                  Util.List.map2
                    (fun t (x, xs) ->
                      match t.Syntax.typ with
                        Type.TFun(_, _) | Type.TPair(_, _)(* ToDo: fix it *) ->
                          (match t.Syntax.desc with Syntax.Var(y) when Id.same x y ->
                            let _ = if debug then Format.printf "arg %a of %a not changed@," Syntax.print_id x Syntax.print_id f in xs | _ -> [])
                      | _ -> [])
                    ts xxss
                with Not_found ->
                  (*let _ = Util.List.iter (fun f -> Format.printf "r: %s@." f) rfs in*)
                  let _ = if debug then Format.printf "nonrec: %a@." Syntax.pp_print_term t1' in
                  false, [])
            | _ ->
                let _ = if debug then Format.printf "nonrec: %a@." Syntax.pp_print_term t1' in
                false, []
          in
          let ts' = Util.List.map (aux rfs bvs exs) ts in
          let tss =
            Util.List.mapi
             (fun i t ->
               match t.Syntax.typ with
                 Type.TFun(_, _) | Type.TPair(_, _)(* ToDo: fix it *) ->
                   new_params (if recursive then Some(Util.List.nth xss i) else None) bvs exs
               | _ -> [])
             ts'
          in
          let ts'' = Util.List.flatten (Util.List.map2 (fun ts t -> ts @ [t]) tss ts') in
        Syntax.App(t1', ts'')
      | Syntax.If(t1, t2, t3) -> Syntax.If(aux rfs bvs exs t1, aux rfs bvs exs t2, aux rfs bvs exs t3)
      | Syntax.Branch(t1, t2) -> Syntax.Branch(aux rfs bvs exs t1, aux rfs bvs exs t2)
      | Syntax.Let(flag, bindings, t2) ->
          let bvs' =
            bvs @
            (if flag = Syntax.Nonrecursive then
              []
            else
              Util.List.map
                Util.Triple.fst bindings)
          in
          let aux' (f,xs,t) =
            let f' = trans_id f in
            let xs' = Util.List.map trans_id xs in

            let xss =
              Util.List.map
                (fun x ->
                   match x.Id.typ with
                     Type.TFun(_, _) | Type.TPair(_, _)(* ToDo: fix it *) ->
                       Util.List.unfold
                         (fun i ->
                           if i < !Global.number_of_extra_params then
                             Some(Id.new_var ("ex" ^ gen_id ()) Type.TInt, i + 1)
                           else
                             None)
                         0
                   | _ ->
                       [])
                xs'
            in
            let xs'' = Util.List.flatten (Util.List.map2 (fun xs x -> xs @ [x]) xss xs') in
            let bvs, exs =
              (if true then
                 bvs' @ xs''
               else
                 bvs' @ xs'),
              exs @ Util.List.flatten xss
            in
            let rfs' = (f, Util.List.map2 (fun xs x -> x, xs) xss xs', flag <> Syntax.Nonrecursive) :: rfs in
            (* mutual recursion and binding partial applied functions are not supported
              let rfs' = (if flag = Flag.Nonrecursive then [] else Util.List.map (fun (f, _, _) -> Id.to_string f) bindings) @ rfs in
             *)
            f', xs'', aux rfs' bvs exs t
          in
          let bindings' = Util.List.map aux' bindings in
          Syntax.Let
            (flag, bindings',
            aux rfs
              (bvs @
              Util.List.map
                Util.Triple.fst
                bindings')
              exs t2)
      | Syntax.BinOp(op, t1, t2) -> Syntax.BinOp(op, aux rfs bvs exs t1, aux rfs bvs exs t2)
      | Syntax.Not t1 -> Syntax.Not (aux rfs bvs exs t1)
      | Syntax.Event(s,b) -> Syntax.Event(s,b)
      | Syntax.Record fields -> Syntax.Record (Util.List.map (fun (f,(s,t1)) -> f,(s,aux rfs bvs exs t1)) fields)
      | Syntax.Proj(i,s,f,t1) -> Syntax.Proj(i,s,f,aux rfs bvs exs t1)
      | Syntax.SetField(n,i,s,f,t1,t2) -> Syntax.SetField(n,i,s,f,aux rfs bvs exs t1,aux rfs bvs exs t2)
      | Syntax.Nil -> Syntax.Nil
      | Syntax.Cons(t1,t2) -> Syntax.Cons(aux rfs bvs exs t1, aux rfs bvs exs t2)
      | Syntax.Constr(s,ts) -> Syntax.Constr(s, Util.List.map (aux rfs bvs exs) ts)
      | Syntax.Match(t1,pats) ->
          let aux' (pat, cond, t) =
            (* ToDo: need to update pat!? *)
            pat,
            aux rfs (bvs @ Syntax.get_vars_pat pat) exs cond,
            aux rfs (bvs @ Syntax.get_vars_pat pat) exs t
          in
          Syntax.Match(aux rfs bvs exs t1, Util.List.map aux' pats)
      | Syntax.Raise t -> Syntax.Raise (aux rfs bvs exs t)
      | Syntax.TryWith(t1,t2) -> Syntax.TryWith(aux rfs bvs exs t1, aux rfs bvs exs t2)
      | Syntax.Pair(t1,t2) -> Syntax.Pair(aux rfs bvs exs t1, aux rfs bvs exs t2)
      | Syntax.Fst t -> Syntax.Fst(aux rfs bvs exs t)
      | Syntax.Snd t -> Syntax.Snd(aux rfs bvs exs t)
      | Syntax.Bottom -> Syntax.Bottom
      | Syntax.Label(info,t) -> Syntax.Label(info, aux rfs bvs exs t)
    in
    {Syntax.desc=desc; Syntax.typ=trans_type t.Syntax.typ}
  in
  let res = aux [] [] [] t in
  let _ = add_time tmp Flag.time_parameter_inference in
  res

let instantiate_param (typs, fdefs, main as prog) =
  let tmp = get_time() in
  (if !ParamSubstInfer.ext_coeffs = [] then
    ParamSubstInfer.init_coeffs (conv_prog prog));
  let map =
    Util.List.map
      (fun (x, n) ->
        Var.string_of x, inv_term (IntTerm.make n))
      !ParamSubstInfer.ext_coeffs
  in
  let res =
		  typs,
				Util.List.map
				  (fun (f, args, guard, events, body) ->
						  (f,
               args,
               CEGAR_util.subst_map map guard,
               events,
               CEGAR_util.subst_map map body))
						fdefs,
				main
  in
  add_time tmp Flag.time_parameter_inference;
  res





let simplify_term t =
  if false then
	  let _, t = trans_term "" [] [] {Syntax.desc = t; Syntax.typ = Type.TBool } in
	  let t = conv_term t in
	  let t = Formula.simplify t in
			let t = inv_term t in
	  (trans_inv_term t).Syntax.desc
  else
    t

let simplify_typed_term p =
  { p with Syntax.desc = simplify_term p.Syntax.desc }

let rec simplify typ =
  match typ with
    Ref_type.Base(base, x, p) ->
        Ref_type.Base(base, x, simplify_typed_term p)
  | Ref_type.Fun(x,typ1,typ2) ->
        Ref_type.Fun(x, simplify typ1, simplify typ2)
  | Ref_type.Pair(x,typ1,typ2) ->
        Ref_type.Pair(x, simplify typ1, simplify typ2)
  | Ref_type.Inter typs ->
		      Ref_type.Inter (Util.List.map simplify typs)
  | Ref_type.Union typs ->
		      Ref_type.Union (Util.List.map simplify typs)
  | Ref_type.ExtArg(x,typ1,typ2) ->
        Ref_type.ExtArg(x, simplify typ1, simplify typ2)
  | Ref_type.List(x,p_len,y,p_i,typ) ->
        Ref_type.List(x, simplify_typed_term p_len, y, simplify_typed_term p_i, typ)



let compute_strongest_post prog ce =
  let prog = conv_prog prog in
  let [etr] = CompTreeExpander.error_traces_of prog [ce] in
  let _, hcs = HcGenRefType.cgen (Prog.type_of prog) etr in
  let hcs = List.map (HornClause.simplify []) hcs in
  Format.printf "Horn clauses:@,  %a@," HornClause.pr hcs;
  let lbs = HcSolver.compute_lbs hcs in
  let env, spc =
    let is_fail pid = Idnt.string_of (Var.base pid) = "fail" in
    if List.exists is_fail (HornClause.rhs_pids hcs) then
      let [HornClause.Hc(Some(_), [atm], t) as hc] =
        List.filter
          (function
            HornClause.Hc(Some(pred), _, _) when is_fail (Pred.pid_of pred) ->
              true
          | _ ->
              false)
          hcs
      in
      RefType.visible_vars (Prog.type_of prog) (fst atm),
      let t' =
        try
          TypPredSubst.lookup_map_fresh atm lbs
        with Not_found ->
          assert false
      in
      Formula.simplify (Formula.band [t; t'])
    else
      let [HornClause.Hc(None, [atm], _)] = List.filter HornClause.is_root hcs in
      RefType.visible_vars (Prog.type_of prog) (fst atm),
      TypPredSubst.lookup_map_fresh atm lbs
  in
  let f = Var.base (fst (Util.List.hd env)) in
  let typ = Prog.type_of prog (Var.make f) in
  let targs, _ = SimType.args_ret typ in
  let fdef = Util.List.hd (Prog.fdefs_of prog f) in
  let args =
    Util.List.filter_map2
      (fun x ty -> if SimType.is_base ty then Some(x) else None)
      fdef.Fdef.args
      targs
  in
  assert (Util.List.length args = Util.List.length env);
  let map = Util.List.combine (Util.List.map fst env) (Util.List.map (Var.make >> Term.make_var) args) in
  let spc = Term.subst map spc in
  let env = Util.List.map2 (fun (_, ty) x -> Var.make x, ty) env args in
  let env = List.filter (fun (x, _) -> not (Util.String.starts_with (Idnt.string_of (Var.base x)) "prev_set_flag")) env in
  let fvs_bool = Util.Set.diff (Formula.fvs_bool spc) (List.map fst env) in (* require that only free variable is <fail:??:0> or prev_set_flag_* *)
  let bvs_bool = List.map fst (List.filter (fun (_, ty) -> ty = SimType.bool_type) env) in
  let env = List.filter (fun (_, ty) -> ty <> SimType.bool_type) env in
  let env = List.filter (fun (_, ty) -> ty <> SimType.unit_type) env in
  let spc = Formula.elim_unit spc in
  let spc = Formula.eqelim_boolean (fvs_bool @ bvs_bool) spc in
  Format.printf "strongest post condition:@,  %a@," Term.pr spc;
  Format.printf "variables in the scope:@,  %a@," TypEnv.pr env;
  env, spc


let report_error ppf = function
    AbsTypeInfer.FailedToRefineTypes ->
      Format.fprintf ppf "Failure of abstraction type refinement"
  | ExtFormula.Formula.Unknown ->
      Format.fprintf ppf "Failure of SMT prover"
  | InterpProver.Fail ->
      Format.fprintf ppf "Failure of interpolating prover (integer domain not fully supported)"
  | InterpProver.Unknown ->
      Format.fprintf ppf "Failure of interpolating prover"
  | ParamSubstInfer.FailedToRefineExtraParameters ->
      Format.fprintf ppf  "Failure of parameter substitution refinement"
  | PolyConstrSolver.Unknown ->
      Format.fprintf ppf "Failure of polynomial constraint solver"
  | Util.NotImplemented msg ->
      Format.fprintf ppf "Not implemented: %s" msg
  | _ -> raise Not_found

let string_of_error = make_string_of report_error

let is_fpat_exception = function
    AbsTypeInfer.FailedToRefineTypes
  | ExtFormula.Formula.Unknown
  | InterpProver.Fail
  | InterpProver.Unknown
  | ParamSubstInfer.FailedToRefineExtraParameters
  | PolyConstrSolver.Unknown
  | Util.NotImplemented _ -> true
  | _ -> false
