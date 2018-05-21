open Util
open Combinator

(** Refinement type checker *)

let game_solving = ref false
let ho_ufuns = ref true

type question = QCheck of RefTyp.t | QInfer of Type.t
type answer = ACheck of bool | AInfer of RefTyp.t

let fdummy = Fdef.make "" [Pattern.U] Formula.mk_true MLExp.mk_undef
let func = ref fdummy
let rev_consts = ref [fdummy, Formula.mk_true]
let nondet_pvars = ref []
let nondet_cnt = ref 0
let wf_pvars = ref []

let init () =
  rev_consts := [fdummy, Formula.mk_true];
  nondet_pvars := [];
  wf_pvars := []

let wf_pid f1 n1 f2 n2 =
  Idnt.make ("Rec$" ^
             f1 ^ "$" ^ string_of_int n1 ^ "$" ^
             f2 ^ "$" ^ string_of_int n2)
let is_wf_pvar x = Str.string_match (Str.regexp "Rec\\$") (Idnt.string_of x) 0

(* sync wf_pvars with hccs *)
let sync_wf_pvars hccs =
  wf_pvars :=
    HCCS.pvs hccs
    |> List.filter is_wf_pvar
    |> List.unique

let pr_question ppf = function
  | QCheck rty -> Format.fprintf ppf "@[QCheck(%a)@]" RefTyp.pr rty
  | QInfer ty -> Format.fprintf ppf "@[QInfer(%a)@]" Type.pr ty
let pr_answer ppf = function
  | ACheck b -> Format.fprintf ppf "@[ACheck(%b)@]" b
  | AInfer rty -> Format.fprintf ppf "@[AInfer(%a)@]" RefTyp.pr rty

let valid tenv phi =
  rev_consts :=
    (!func, Formula.imply (RefTypEnv.formula_of tenv) phi) :: !rev_consts;
  true
let valid =
  Logger.log_block2 "RefTypCheck.valid"
    ~before:(fun tenv phi ->
        Logger.printf "|= %a ?"
          Formula.pr (Formula.imply (RefTypEnv.formula_of tenv) phi))
    valid

let rec subty tenv ty1 ty2 =
  match ty1, ty2 with
  | RefTyp.Base(x1, ty1, phi1), RefTyp.Base(x2, ty2, phi2) ->
    (* @todo unknown *)
    if Type.equiv_mod_unknown ty1 ty2 |> not then begin
      Format.printf
        "Type.equiv_mod_unknown: ty1: %a, ty2: %a@."
        Type.pr ty1 Type.pr ty2;
      assert false
    end;
    valid tenv (Formula.imply (Formula.subst [x1, Term.mk_var x2] phi1) phi2)
  | RefTyp.Fun(x1, ty11, ty12, (r1, phi1)),
    RefTyp.Fun(x2, ty21, ty22, (r2, phi2)) ->
    subty tenv ty21 ty11 &&
    subty (RefTypEnv.Env(x2, ty21) :: tenv)
      (RefTyp.subst [x1, Term.mk_var x2] ty12)
      ty22 &&
    valid tenv (Formula.imply (Formula.subst [r1, Term.mk_var r2] phi1) phi2)
  | RefTyp.Tuple(xrtys1, rty1), RefTyp.Tuple(xrtys2, rty2) ->
    subty_list tenv []
      (xrtys1 @ [Idnt.new_var (), rty1])
      (xrtys2 @ [Idnt.new_var (), rty2])
  | _, _ ->
    Format.printf
      "error in RefTypCheck.subty:@,  %a and %a@,"
      RefTyp.pr ty1
      RefTyp.pr ty2;
    assert false
and subty_list tenv vmap xrtys1 xrtys2 =
  match xrtys1, xrtys2 with
  | [], [] -> true
  | (x1, rty1) :: xrtys1', (x2, rty2) :: xrtys2' ->
    subty tenv rty1 (RefTyp.subst vmap rty2) &&
    subty_list
      (RefTypEnv.Env(x1, rty1) :: tenv)
      ((x2, Term.mk_var x1) :: vmap)
      xrtys1'
      xrtys2'
  | _, _ -> assert false
let subty tenv ty1 ty2 = subty tenv (RefTyp.alpha [] ty1) (RefTyp.alpha [] ty2)
let subty =
  Logger.log_block3 "RefTypCheck.subty"
    ~before:(fun tenv ty1 ty2 ->
        Logger.printf3
          "tenv: %a@,ty1: %a@,ty2: %a@,"
          RefTypEnv.pr tenv
          RefTyp.pr ty1
          RefTyp.pr ty2)
    subty

let type_of ctenv rtenv c =
  match c with
  | Const.Div ty when Type.is_int ty -> 
    let x1 = Idnt.new_var () in
    let x2 = Idnt.new_var () in
    let x3 = Idnt.new_var () in
    let x = Term.mk_var x1 in
    let y = Term.mk_var x2 in
    let z = Term.mk_var x3 in
    RefTyp.mk_fun_args_ret_rty
      [x1, RefTyp.of_simple_type ty;
       x2, RefTyp.of_simple_type ty]
      (RefTyp.mk_base x3 ty
         (Formula.mk_and
            (IntFormula.leq (IntTerm.mul y z) x)
            (IntFormula.lt x (IntTerm.mul y (IntTerm.add z IntTerm.one)))))
  | Const.Add ty | Const.Sub ty | Const.Mul ty | Const.Div ty ->
    let x1 = Idnt.new_var () in
    let x2 = Idnt.new_var () in
    let f =
      match c with
      | Const.Add _ -> NumTerm.add ty
      | Const.Sub _ -> NumTerm.sub ty
      | Const.Mul _ -> NumTerm.mul ty
      | Const.Div _ -> NumTerm.div ty
      | _ -> assert false
    in
    RefTyp.mk_fun_args_ret_rty
      [x1, RefTyp.of_simple_type ty;
       x2, RefTyp.of_simple_type ty]
      (RefTyp.mk_singleton ty (f (Term.mk_var x1) (Term.mk_var x2)))
  | Const.Neg ty ->
    let x1 = Idnt.new_var () in
    RefTyp.mk_fun_args_ret_rty
      [x1, RefTyp.of_simple_type ty]
      (RefTyp.mk_singleton ty (NumTerm.neg ty (Term.mk_var x1)))
  | Const.Eq ty | Const.Neq ty
  | Const.Gt ty | Const.Lt ty | Const.Geq ty | Const.Leq ty ->
    let x1 = Idnt.new_var () in
    let x2 = Idnt.new_var () in
    let f =
      match c with
      | Const.Eq ty -> Formula.eq ty
      | Const.Neq ty -> Formula.neq ty
      | Const.Gt ty -> NumFormula.gt ty
      | Const.Lt ty -> NumFormula.lt ty
      | Const.Geq ty -> NumFormula.geq ty
      | Const.Leq ty -> NumFormula.leq ty
      | _ -> assert false
    in
    RefTyp.mk_fun_args_ret_rty
      [x1, RefTyp.of_simple_type ty;
       x2, RefTyp.of_simple_type ty]
      (RefTyp.mk_singleton Type.mk_bool
         (f (Term.mk_var x1) (Term.mk_var x2) |> Formula.term_of))
  | Const.Not ->
    let x1 = Idnt.new_var () in
    let x2 = Idnt.new_var () in
    RefTyp.mk_fun_args_ret_rty
      [x1, RefTyp.of_simple_type Type.mk_bool]
      (RefTyp.mk_base x2 Type.mk_bool
         (BoolFormula.neq (Term.mk_var x2) (Term.mk_var x1)))
  | Const.And | Const.Or ->
    let x1 = Idnt.new_var () in
    let x2 = Idnt.new_var () in
    let f =
      match c with
      | Const.And -> Formula.mk_and
      | Const.Or -> Formula.mk_or
      | _ -> assert false
    in
    RefTyp.mk_fun_args_ret_rty
      [x1, RefTyp.of_simple_type Type.mk_bool;
       x2, RefTyp.of_simple_type Type.mk_bool]
      (RefTyp.mk_singleton Type.mk_bool
         (f (BoolFormula.mk_var x1) (BoolFormula.mk_var x2)|> Formula.term_of))
  | Const.Unit -> RefTyp.mk_singleton Type.mk_unit (Term.mk_const c)
  | Const.True -> RefTyp.mk_singleton Type.mk_bool (Term.mk_const c)
  | Const.False -> RefTyp.mk_singleton Type.mk_bool (Term.mk_const c)
  | Const.Int _ -> RefTyp.mk_singleton Type.mk_int (Term.mk_const c)
  | Const.Real _ -> RefTyp.mk_singleton Type.mk_real (Term.mk_const c)
  | Const.Con (ty, id) ->
    ignore (Type.base_or_adt_of ty);
    RefTyp.mk_singleton ty (Term.mk_const c)
  | Const.Recognizer(ty, x) ->
    let ty =
      if Type.is_unknown ty then
        List.assoc_fail ~on_failure:(fun () ->
            Logger.printf2 "Idnt %a is not found in %a@,"
              Idnt.pr (Const.string_of c |> Idnt.make)
              TypEnv.pr ctenv)
          (Const.string_of c |> Idnt.make) ctenv
      else ty
    in
    let [arg], ret = ty |> Type.args_ret in
    assert (Type.is_adt arg && Type.is_bool ret);
    let x1 = Idnt.new_var () in
    RefTyp.mk_fun_args_ret_rty
      [x1, RefTyp.of_simple_type arg]
      (RefTyp.mk_singleton Type.mk_bool
         (Term.mk_app (Term.mk_const c) [Term.mk_var x1]))
  | Const.Accessor(ty, x, i) ->
    let has_rty = RefTypEnv.member rtenv x in
    let ty =
      if Type.is_unknown ty then
        List.assoc_fail (Const.string_of c |> Idnt.make) ctenv
      else ty
    in
    let x1 = Idnt.new_var () in
    let adt_ty, rest_ty =
      match Type.fun_args ty with
      | Type.Const(TypConst.Arrow), [ty1; ty2] -> ty1, ty2
      | _ -> assert false
    in
    let rty_ret =
      if has_rty then
        let args = RefTypEnv.lookupF rtenv x |> RefTyp.args_ret |> fst in
        let var_map =
          args
          |> List.map fst
          |> List.mapi
            (fun idx id ->
               let acc_id =
                 Idnt.make @@
                 "_get_" ^ (Idnt.base x) ^ "_" ^ (string_of_int idx)
               in
               let ty = TypEnv.lookup ctenv acc_id in
               let ret_ty =
                 match Type.fun_args ty with
                 | Type.Const(TypConst.Arrow), [ty1;ty2] -> ty2
                 | _ -> assert false
               in
               id, Formula.eq ret_ty
                 (Term.mk_var id)
                 (Term.mk_app
                    (Term.mk_const (Const.Accessor(ty, x, idx)))
                    [Term.mk_var x1]))
        in
        let rty = List.nth args i |> snd in
        match rty with
        | RefTyp.Base(v, typ, phi) ->
          let fvs = Formula.fvs phi in
          let var_phi =
            var_map
            |> List.filter (fun (x, p) -> List.mem x fvs)
            |> List.map snd
          in
          let acc_phi =
            Formula.eq rest_ty
              (Term.mk_var v)
              (Term.mk_app (Term.mk_const c) [Term.mk_var x1])
          in
          RefTyp.mk_base v typ (Formula.band (acc_phi::phi::var_phi))
        | _ -> assert false
      else begin
        match Type.fun_args rest_ty with
        | Type.Const(TypConst.Tuple(_)), _ ->
          let tys = Type.tuple_of rest_ty in
          RefTyp.mk_tuple
            (List.mapi
               (fun i t ->
                  (RefTyp.mk_singleton t
                     (TupTerm.mk_proj tys i
                        (Term.mk_app (Term.mk_const c) [Term.mk_var x1]))))
               tys)
        | Type.Const(TypConst.Arrow), _ ->
          (* function refinement *)
          RefTyp.mk_singleton rest_ty
            (Term.mk_app (Term.mk_const c) [Term.mk_var x1])
        | _ ->
          ignore (Type.base_or_adt_ret_of rest_ty);
          RefTyp.mk_singleton rest_ty
            (Term.mk_app (Term.mk_const c) [Term.mk_var x1])
      end
    in
    RefTyp.mk_fun_args_ret_rty [x1, RefTyp.of_simple_type adt_ty] rty_ret
  | Const.Proj(tys, i) ->
    let ty = List.nth tys i in
    if Type.is_base ty || Type.is_adt ty then
      let x = Idnt.new_var () in
      RefTyp.mk_fun_args_ret_rty
        [x, RefTyp.of_simple_type (Type.mk_tuple tys)]
        (RefTyp.mk_singleton ty (TupTerm.mk_proj tys i (Term.mk_var x)))
    else assert false
  | Const.RandBool -> RefTyp.of_simple_type Type.mk_bool
  | Const.RandInt -> RefTyp.of_simple_type Type.mk_int
  | Const.RandReal -> RefTyp.of_simple_type Type.mk_real
  | _ -> raise Not_found

(** type checking if [mode = QCheck rty]
    type inference if [mode = QInfer ty] *)
let dbg_pr t = function
  | QCheck rty ->
    Logger.printf2
      "Type checking:@,  @[<hov>\"%a\" against@ %a@]@,"
      MLExp.pr t
      RefTyp.pr rty;
  | QInfer ty ->
    Logger.printf2
      "Type inference:@,  @[<hov>\"%a\" with the ML type %a@]@,"
      MLExp.pr t
      Type.pr ty

let type_of_kon ctenv rtenv k =
  try RefTypEnv.lookup rtenv k
  with Not_found ->
    List.assoc_fail k ctenv
      ~on_failure:(fun () ->
          Format.printf
            "RefTypCheck.rtenv_of_pattern:@.  %a is not found@.  ctenv: %a@."
            Idnt.pr k TypEnv.pr ctenv)
    |> RefTyp.of_simple_type

let rec rtenv_of_patterns ctenv rtenv xrtys pats =
  match xrtys, pats with
  | [], [] -> []
  | (x, rty) :: xrtys', pat :: pats' ->
    let tpat = MLExp.of_pattern ctenv pat (RefTyp.to_simple_type rty) in
    rtenv_of_patterns ctenv rtenv
      (xrtys' |> List.map @@ Pair.map_snd (RefTyp.subst [x, tpat]))
      pats'
    @ rtenv_of_pattern ctenv rtenv rty pat
  | _ -> assert false
and rtenv_of_pattern ctenv rtenv rty pat =
  match pat, rty with
  | Pattern.V x, _ -> [RefTypEnv.Env(x, rty)]
  | Pattern.W, rty -> [RefTypEnv.Env(Idnt.new_var(), rty)]
  | Pattern.U, RefTyp.Base(_, ty, _) ->
    assert (Type.is_unit_unknown ty);
    [RefTypEnv.Env(Idnt.new_var(), rty)]
  | Pattern.T(pats), RefTyp.Tuple(xrtys, rty) ->
    rtenv_of_patterns ctenv rtenv (xrtys @ [Idnt.new_var (), rty]) pats
  | Pattern.K(k, p), RefTyp.Base(x, ty, phi) ->
    let xrtys, RefTyp.Base(x', ty', phi') =
      type_of_kon ctenv rtenv k |> RefTyp.args_ret
    in
    let pats = Pattern.list_of p(*@todo*) in
    (* assert (ty = ty'); *)
    let tpat = MLExp.of_pattern ctenv pat ty in
    let rty =
      RefTyp.mk_base (Idnt.new_var ()) ty
        (Formula.mk_and
           (Formula.subst [x, tpat] phi)
           (Formula.subst [x', tpat] phi'))
    in
    rtenv_of_patterns ctenv rtenv
      (xrtys @ [Idnt.new_var (), rty]) (pats @ [Pattern.W])
  | _, _ -> assert false

let rec apply ctenv rtenv rty pats =
  match rty, pats with
  | RefTyp.Fun(x, rty1, rty2, _), p :: rest ->
    let tpat = MLExp.of_pattern ctenv p (RefTyp.to_simple_type rty1) in
    let rty_rest, g = apply ctenv rtenv (RefTyp.subst [x, tpat] rty2) rest in
    rty_rest, rtenv_of_pattern ctenv rtenv rty1 p @ g
  | _, [] -> rty, []
  | _, _ -> failwith "too many argument."

let rec strengthen (t, ty) tup = function
  | RefTyp.Base(x, ty', phi) ->
    RefTyp.mk_base x ty' (Formula.band [phi(*@todo*); Formula.eq ty (tup (Term.mk_var x, ty')) t])
  | RefTyp.Fun(x, rty1, rty2, (y, phi)) ->
    if !ho_ufuns then
      let ty2 = RefTyp.to_simple_type rty2 in
      let ty' = Type.mk_fun [RefTyp.to_simple_type rty1; ty2] in
      let phi' = Formula.band [phi; Formula.eq ty (tup (Term.mk_var y, ty')) t] in
      RefTyp.Fun (x, rty1, strengthen (Term.mk_app t [Term.mk_var x], ty2) tup rty2, (y, phi'))
    else RefTyp.Fun(x, rty1, rty2, (y, phi))
  | RefTyp.Tuple(xrtys, rty) ->
    let tup' (t, ty) =
      let ts = List.map (fst >> Term.mk_var) xrtys @ [t] in
      let tys = List.map (snd >> RefTyp.to_simple_type) xrtys @ [ty] in
      TupTerm.make tys ts
    in
    RefTyp.Tuple(xrtys, strengthen (t, ty) tup' rty)
  | rty -> rty

let tcheck prog fname wf strategies =
  MLExp.para
    (object
      method fvar x = fun mode ctenv rtenv ->
        dbg_pr (Term.mk_var x) mode;
        let rty1 = RefTypEnv.lookupF rtenv x |> RefTyp.alpha [] in
        let rty1 =
          if Prog.is_defined prog x then
            rty1
          else
            rty1 |> strengthen (Term.mk_var x, RefTyp.to_simple_type rty1) fst
        in
        match mode with
        | QCheck rty2 -> ACheck (subty rtenv rty1 rty2)
        | QInfer(_)  -> AInfer rty1
        | _ ->
          Format.printf
            "error: rty1 = %a@, mode = %a@," RefTyp.pr rty1 pr_question mode;
          assert false
      method flet _ (pat, pty) e1 r1 e2 r2 = fun mode ctenv rtenv ->
        dbg_pr (MLExp.mk_let_pat Type.mk_unknown (pat, pty) e1 e2) mode;
        let AInfer rty = r1 (QInfer pty) ctenv rtenv in
        let rtenv' = rtenv_of_pattern ctenv rtenv rty pat @ rtenv in
        match mode with
        | QCheck _ -> r2 mode ctenv rtenv'
        | QInfer ty ->
          let rty =
            assert (ty <> Type.mk_unknown);
            RefTyp.mk_template
              (Idnt.new_var ())
              (RefTypEnv.domain rtenv)
              (RefTyp.of_simple_type ty)
          in
          let ACheck true = r2 (QCheck(rty)) ctenv rtenv' in
          AInfer rty
      method fletrec _ _ _ = fun mode ctenv rtenv ->
        Format.printf "inner functions not supported@.";
        assert false
      method fevent str e r = fun mode ctenv rtenv ->
        dbg_pr (MLExp.mk_event str e) mode;
        match e with
        | Term.Const Const.Unit
        | Term.Const Const.Bot ->
          begin
            match str, mode with
            | "fail", QCheck _ -> ACheck(valid rtenv Formula.mk_false)
            | "fail", QInfer ty ->
              let rty = RefTyp.mk_base (Idnt.new_var ()) ty Formula.mk_false in
              (* @todo ty may be non-based type *)
              let ACheck true = r (QCheck rty) ctenv rtenv in
              AInfer rty
            | _, _ -> assert false
          end
        | _ -> assert false
      method ftuple tys ts rs = fun mode ctenv rtenv ->
        dbg_pr (TupTerm.mk_tuple2 ts) mode;
        assert (List.for_all (MLExp.is_value (RefTypEnv.arity_of rtenv)) ts);
        let inf ctenv rtenv rs =
          assert (rs <> []);
          let rec aux acc rs tys =
            match rs, tys with
            | [], [] -> acc
            | r :: rs', ty :: tys' ->
              let AInfer rty = r (QInfer ty) ctenv (acc @ rtenv) in
              aux (RefTypEnv.Env(Idnt.new_var (), rty) :: acc) rs' tys'
            | _ -> assert false
          in
          let tls = aux [] rs tys |> RefTypEnv.rty_env_of |> List.rev in
          RefTyp.Tuple (List.initial tls, List.last tls |> snd)
        in
        match mode with
        | QCheck rty -> ACheck(subty rtenv (inf ctenv rtenv rs) rty)
        | QInfer _ -> AInfer(inf ctenv rtenv rs)
      method fkon (id, ty) cs rs = fun mode ctenv rtenv ->
        dbg_pr (ADTTerm.mk_kon (id, ty) cs) mode;
        assert (List.for_all (MLExp.is_value (RefTypEnv.arity_of rtenv)) cs);
        let has_reftyp : bool = RefTypEnv.member rtenv id in
        let c = Const.Con (ty, id) in
        let args, ret = Type.args_ret ty in
        let ref_ty =
          if has_reftyp then
            let rty = RefTypEnv.lookupF rtenv id in
            let ref_args, ref_ret = RefTyp.args_ret rty in
            let () =
              match ref_ret with
              | RefTyp.Base(_, _, phi) when Formula.is_true phi -> ()
              | _ -> assert false(* refinement for return type is not allowed *)
            in
            let vars = List.map (fst >> Term.mk_var) ref_args in
            RefTyp.mk_fun_args_ret_rty
              ref_args
              (RefTyp.mk_singleton ret (Term.mk_app (Term.mk_const c) vars))
          else
            RefTyp.mk_fun_args_ret
              (List.map (RefTyp.of_simple_type) args)
              (RefTyp.mk_singleton ret (Term.mk_app (Term.mk_const c) cs))
        in
        if ty <> RefTyp.to_simple_type ref_ty then
          (Logger.printf3 ~kind:Logger.Error
             "Wrong type is given to %a: %a <> %a@,"
             Idnt.pr id Type.pr ty
             Type.pr (RefTyp.to_simple_type ref_ty);
           assert false);
        match mode with
        | QCheck rty ->
          if (RefTyp.to_simple_type (RefTyp.ret ref_ty)
              <> RefTyp.to_simple_type rty) then begin
            Logger.printf2 ~kind:Logger.Warning
              "warning: base type is wrong %a <> %a@,"
              RefTyp.pr (RefTyp.ret ref_ty)
              RefTyp.pr rty
              (*assert false*)
          end;
          let tl =
            List.map2
              (fun r ty ->
                 match r (QInfer ty) ctenv rtenv with
                 | AInfer rty -> rty
                 | _ -> assert false)
              rs args
          in
          let rty = RefTyp.mk_fun_args_ret tl rty in
          ACheck(subty rtenv ref_ty rty)
        | QInfer _ ->
          ignore (Type.base_or_adt_of ret);
          let ref_args, ref_ret = RefTyp.args_ret ref_ty in
          let sub = List.map2 (fun (id,_) c -> id, c) ref_args cs in
          assert
            (List.for_all2
               (fun r (_, ref_arg) ->
                  let ref_arg = RefTyp.subst sub ref_arg in
                  match r (QCheck ref_arg) ctenv rtenv with
                  | ACheck b -> b
                  | _ -> assert false) (* is this ok? *)
               rs
               ref_args);
          AInfer(RefTyp.subst sub ref_ret)
      method fapp e r es rs = fun mode ctenv rtenv ->
        dbg_pr (Term.mk_app e es) mode;
        assert (List.for_all (MLExp.is_value (RefTypEnv.arity_of rtenv)) es);
        match mode with
        | QCheck rty ->
          let rtys =
            List.map
              (fun f  ->
                 match f (QInfer Type.mk_unknown(*@todo*)) ctenv rtenv with
                 | AInfer rty -> rty
                 | _ -> assert false)
              rs
          in
          r (QCheck(RefTyp.mk_fun_args_ret rtys rty)) ctenv rtenv
        | QInfer _ ->
          let AInfer ans = r (QInfer Type.mk_unknown(*@todo*)) ctenv rtenv in
          let rec aux acc es rty =
            match es with
            | [] -> (acc, rty)
            | e :: es' ->
              match rty with
              | RefTyp.Fun(x, rty1, rty2, phi) -> (* @todo check *)
                aux (rty1::acc) es' (RefTyp.subst [x, e] rty2)
              | _ ->
                Format.printf "e: %a@.rty: %a@." MLExp.pr e RefTyp.pr rty;
                assert false
          in
          let tl, rty' = aux [] es ans in
          assert
            (List.for_all2
               (fun f ty ->
                  match f (QCheck ty) ctenv rtenv with
                  | ACheck b -> b
                  | _ -> assert false)
               rs (List.rev tl));
          AInfer rty'
      method fcon c = fun mode ctenv rtenv ->
        dbg_pr (MLExp.mk_const c) mode;
        match c, mode with
        | Const.Con _, QCheck(RefTyp.Base(x, ty, phi))
        | Const.Unit, QCheck(RefTyp.Base(x, ty, phi))
        | Const.True, QCheck(RefTyp.Base(x, ty, phi))
        | Const.False, QCheck(RefTyp.Base(x, ty, phi))
        | Const.Int _, QCheck(RefTyp.Base(x, ty, phi))
        | Const.Real _, QCheck(RefTyp.Base(x, ty, phi)) ->
          ACheck(valid rtenv (Formula.subst [x ,Term.mk_const c] phi))

        | Const.ReadBool(_), _ ->
          raise (Global.NotImplemented "RefTypCheck: Const.ReadBool")
        | Const.ReadInt(id, _), QInfer(_) ->
          let p =
            Idnt.make (fname ^ "_nondet" ^ string_of_int !nondet_cnt)
            (*Idnt.new_pvar()*)
          in
          nondet_cnt := !nondet_cnt + 1;
          nondet_pvars := !nondet_pvars @ [p];
          let phi =
            let args =
              rtenv |> RefTypEnv.domain |> List.map (Pair.map_fst Term.mk_var)
              |> flip (@) [Term.mk_var id, Type.mk_int]
            in
            if List.mem_assoc p strategies
            then Pred.apply (List.assocF p strategies) args
            else Pva.make p args |> Pva.to_formula
          in
          assert (valid rtenv (Formula.exists [(id, Type.mk_int)] phi));
          (* assert (valid rtenv phi); *)
          AInfer(RefTyp.Base(id, Type.mk_int, phi))
        | Const.ReadReal(id, _), QInfer _ ->
          let p =
            Idnt.make (fname ^ "_nondet" ^ string_of_int !nondet_cnt)
            (*Idnt.new_pvar()*)
          in
          nondet_cnt := !nondet_cnt + 1;
          nondet_pvars := !nondet_pvars @ [p];
          let phi =
            let args =
              rtenv |> RefTypEnv.domain |> List.map (Pair.map_fst Term.mk_var)
              |> flip (@) [Term.mk_var id, Type.mk_real]
            in
            if List.mem_assoc p strategies
            then Pred.apply (List.assocF p strategies) args
            else Pva.make p args |> Pva.to_formula
          in
          assert (valid rtenv (Formula.exists [(id, Type.mk_real)] phi));
          (* assert (valid rtenv phi); *)
          AInfer(RefTyp.Base(id, Type.mk_real, phi))

        | Const.Undef, QCheck rty -> ACheck true
        (* accept any types. use in expressing if-encoded total
           pattern matching. is it ok? *)
        | Const.Undef, QInfer ty -> AInfer(RefTyp.of_simple_type ty)
        | _, QCheck rty ->
          let rty' =
            try type_of ctenv rtenv c
            with Not_found ->
              Format.printf "@[\nuncaught const: %a, reftyp: %a@]@?"
                (Const.pr []) c
                pr_question mode;
              assert false
          in
          ACheck(subty rtenv rty' rty)
        | _, QInfer _ ->
          let rty' =
            try type_of ctenv rtenv c
            with Not_found ->
              Format.printf "@[\nuncaught const: %a, reftyp: %a@]@?"
                (Const.pr []) c
                pr_question mode;
              assert false
          in
          AInfer rty'
      method fif ty_br e1 r1 e2 r2 e3 r3  = fun mode ctenv rtenv ->
        dbg_pr (MLExp.mk_if ty_br e1 e2 e3) mode;
        let mode =
          match mode with
          | QInfer ty_f when Type.is_unknown ty_f -> QInfer ty_br
          | _ -> mode
        in
        match mode with
        | QCheck _ ->
          begin
            match r1 (QInfer Type.mk_bool) ctenv rtenv with
            | AInfer(RefTyp.Base(x, ty, phi))
              when Type.is_bool ty || Type.is_unknown ty ->
              let ACheck b1 =
                r2 mode ctenv
                  (RefTypEnv.EFormula
                     (Formula.subst [x, BoolTerm.make true] phi) :: rtenv)
              in
              let ACheck b2 =
                r3 mode ctenv
                  (RefTypEnv.EFormula
                     (Formula.subst [x, BoolTerm.make false] phi) :: rtenv)
              in
              ACheck (b1 && b2)
            | AInfer rty ->
              Format.printf_force "error: %a@." RefTyp.pr rty;
              failwith "Not Implemented. (RefTypCheck.tcheck.fif)"
            | _ -> assert false
          end
        | QInfer _ ->
          begin
            match r1 (QInfer Type.mk_bool) ctenv rtenv with
            | AInfer(RefTyp.Base(x, ty, phi))
              when Type.is_bool ty || Type.is_unknown ty ->
              if Type.is_base ty_br || Type.is_adt ty_br then
                let AInfer rty1 =
                  r2 mode ctenv
                    (RefTypEnv.EFormula
                       (Formula.subst [x, BoolTerm.make true] phi) :: rtenv)
                in
                let AInfer rty2 =
                  r3 mode ctenv
                    (RefTypEnv.EFormula
                       (Formula.subst [x, BoolTerm.make false] phi) :: rtenv)
                in
                (** need to assume that fpvs phi = []? maybe no *)
                AInfer(RefTyp.merge_base x phi rty1 rty2)
              else
                let rty =
                  assert (ty_br <> Type.mk_unknown);
                  RefTyp.mk_template
                    (Idnt.new_var ())
                    (RefTypEnv.domain rtenv)
                    (RefTyp.of_simple_type ty_br)
                in
                let ACheck b1 =
                  r2 (QCheck rty) ctenv
                    (RefTypEnv.EFormula
                       (Formula.subst [x, BoolTerm.make true] phi) :: rtenv)
                in
                let ACheck b2 =
                  r3 (QCheck rty) ctenv
                    (RefTypEnv.EFormula
                       (Formula.subst [x, BoolTerm.make false] phi) :: rtenv)
                in
                AInfer rty
            | AInfer rty ->
              Format.printf_force "error: %a@." RefTyp.pr rty;
              failwith "Not Implemented. (RefTypCheck.tcheck.fif)"
            | _ -> assert false
          end
      method fcls _ _ _ _ = fun mode ctenv rtenv -> assert false
      method ffun _ _ _ = fun mode ctenv rtenv ->
        assert false (* @todo *)
      method ffix _ _ _ = fun mode ctenv rtenv -> assert false
      method farray ts rs = fun mode ctenv rtenv ->
        dbg_pr (ArrayTerm.mk_array ts) mode;
        let tys =
          List.map
            (fun r ->
               match r (QInfer Type.mk_unknown(*@todo*)) ctenv rtenv with
               | AInfer(RefTyp.Base(_, ty, _)) -> ty
               | _ -> failwith "all array elements must have same type.")
            rs
        in
        let ety = List.hd tys in
        assert (List.for_all ((=) ety) tys);
        match mode with
        | QCheck rty ->
          ACheck(subty rtenv (RefTyp.of_simple_type @@ Type.mk_array ety) rty)
        | QInfer _ -> AInfer(RefTyp.of_simple_type @@ Type.mk_array ety)
      method faget a r1 n r2 = fun mode ctenv rtenv ->
        assert false
        (*dbg_pr (ArrayTerm.mk_aget a n) mode;
        let ans = RefTypEnv.lookupF rtenv a in
        let TypConst.Array =
          ans
          |> RefTyp.to_simple_type
          |> Type.base_or_adt_of
        in

        let ACheck(b1) =
          r2 (QCheck(RefTyp.of_simple_type Type.mk_int)) ctenv rtenv
        in
        match mode with
        | QCheck(rty) -> ACheck(b1)
        | QInfer(ty) ->
          assert b1;
          AInfer(RefTyp.mk_base (Idnt.new_var ()) ty Formula.mk_true)*)
      method faset a r1 n r2 m r3 e r4 = fun mode ctenv rtenv ->
        assert false
        (*dbg_pr (ArrayTerm.mk_aset a n m e) mode;
        let ans = RefTypEnv.lookupF rtenv a in
        let TypConst.Array =
          ans
          |> RefTyp.to_simple_type
          |> Type.base_or_adt_of
        in
        let ACheck(b1) =
          r2 (QCheck(RefTyp.of_simple_type Type.mk_int)) ctenv rtenv
        in
        let ACheck(b2) =
          r3 (QCheck(RefTyp.of_simple_type (Type.mk_unknown (*@todo*)))) ctenv rtenv
        in
        match mode with
        | QCheck(_) ->
          let ACheck(b3) = r4 mode ctenv rtenv in
          ACheck(b1 && b2 && b3)
        | QInfer(_) ->
          (** @todo *)
          r4 mode ctenv rtenv*)
    end)

let tcheck prog fname wf strategies ctenv rtenv t rty =
  let ACheck b = tcheck prog fname wf strategies t (QCheck rty) ctenv rtenv in b
let tcheck prog fname ?(wf=false) strategies =
  Logger.log_block4 "RefTypCheck.tcheck"
    ~before:(fun ctenv rtenv t rty ->
        Logger.printf4
          "ctenv: %a@,rtenv: %a@,t: %a@,rty: %a@,"
          TypEnv.pr ctenv
          RefTypEnv.pr rtenv
          MLExp.pr t
          RefTyp.pr rty)
    (tcheck prog fname wf strategies)

let make_rtenv_wf prog rtenv f =
  if Fdef.name_of f = "main"
  then rtenv (* @todo only work for the game solver *)
  else
    List.map
      (function
        | RefTypEnv.Env(x, rty) as elem ->
          if !game_solving && Prog.is_defined prog x
          || Idnt.make (Fdef.name_of f) = x then
            let args_rty, ret_rty =
              let args_new, ret_new =
                rty |> RefTyp.alpha [] |> RefTyp.args_ret
              in
              let ar = Prog.arity_of prog x in
              List.take ar args_new,
              RefTyp.mk_fun_args_ret_rty (List.drop ar args_new) ret_new
            in
            (* add the WF condition to the last element of args_rty *)
            let args_rty =
              assert (args_rty <> []);
              let initial, last = List.initial_last args_rty in
              let wf =
                let tenv_old =
                  List.map2
                    (fun pat ty ->
                       match pat with
                       | Pattern.V(id) -> Term.mk_var id, ty
                       | Pattern.U -> UnitTerm.make, ty
                       | _ -> raise (Global.NotImplemented "RefTypCheck (wf)"))
                    (Fdef.args_of f)
                    (TypEnv.lookup prog.Prog.types (f.Fdef.name |> Idnt.make)
                     |> Type.args_of
                     |> List.take (Fdef.arity_of f))
                  |> List.filter (snd >> Type.is_fun >> not)
                  (* @todo disable this line when calling context constructor is ready *)
                in
                let tenv_new =
                  List.map (Pair.map Term.mk_var RefTyp.to_simple_type) initial
                  @ [last |> snd |> RefTyp.get_term,
                     RefTyp.to_simple_type (snd last)]
                  |> List.filter (snd >> Type.is_fun >> not)
                  (* @todo disable this line when calling context constructor is ready *)
                in
                tenv_old @ tenv_new
                |> Pva.make
                  (wf_pid
                     (Fdef.name_of f)
                     (List.length (TupHCCSSolver.flatten_tts tenv_old |> fst))
                     (Idnt.string_of x)
                     (List.length (TupHCCSSolver.flatten_tts tenv_new |> fst)))
                |> Pva.to_formula
              in
              initial
              @ [fst last,
                 RefTyp.set_phi
                   (last |> snd |> RefTyp.get_phi |> Formula.mk_and wf)
                   (snd last)]
            in
            RefTypEnv.Env(x, RefTyp.mk_fun_args_ret_rty args_rty ret_rty)
          else elem
        | elem -> elem)
      rtenv

(* @todo to be tcheck_ml *)
let tcheck_fdef ?(wf=false) strategies prog ctenv rtenv f =
  let rty_rest, rtenv' =
    apply ctenv rtenv
      (RefTypEnv.lookupF rtenv (Idnt.make f.Fdef.name))
      f.Fdef.args
  in
  let ctenv_wo_tvar = (* @todo? *)
    List.filter (snd >> Type.contain_tvar >> not) ctenv
  in
  func := f;
  nondet_cnt := 0;
  let rtenv =
    if wf && (!game_solving || Fdef.rec_check f)
    then make_rtenv_wf prog rtenv f
    else rtenv
  in
  tcheck prog f.Fdef.name ~wf strategies
    ctenv_wo_tvar
    (RefTypEnv.EFormula f.Fdef.guard :: (List.rev rtenv') @ rtenv)
    f.Fdef.body rty_rest
