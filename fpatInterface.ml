
open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_print
open CEGAR_util
open Fpat.Combinator
open Fpat.ExtFormula

let conv_const c =
  match c with
  | Bottom -> Fpat.Const.Bot
  | Unit -> Fpat.Const.Unit
  | True -> Fpat.Const.True
  | False -> Fpat.Const.False
  | And -> Fpat.Const.And
  | Or -> Fpat.Const.Or
  | Not -> Fpat.Const.Not
  | Lt -> Fpat.Const.Lt Fpat.Type.mk_int
  | Gt -> Fpat.Const.Gt Fpat.Type.mk_int
  | Leq -> Fpat.Const.Leq Fpat.Type.mk_int
  | Geq -> Fpat.Const.Geq Fpat.Type.mk_int
  | EqUnit -> Fpat.Const.Eq Fpat.Type.mk_unit
  | EqBool -> Fpat.Const.Eq Fpat.Type.mk_bool
  | EqInt -> Fpat.Const.Eq Fpat.Type.mk_int
  | CmpPoly(typ,"=") -> Fpat.Const.Eq (Fpat.Type.mk_const (Fpat.TypConst.Ext typ))
  | CmpPoly(typ,"<>") -> Fpat.Const.Neq (Fpat.Type.mk_const (Fpat.TypConst.Ext typ))
  | CmpPoly(typ,"<") -> Fpat.Const.Lt (Fpat.Type.mk_const (Fpat.TypConst.Ext typ))
  | CmpPoly(typ,">") -> Fpat.Const.Gt (Fpat.Type.mk_const (Fpat.TypConst.Ext typ))
  | CmpPoly(typ,"<=") -> Fpat.Const.Leq (Fpat.Type.mk_const (Fpat.TypConst.Ext typ))
  | CmpPoly(typ,">=") -> Fpat.Const.Geq (Fpat.Type.mk_const (Fpat.TypConst.Ext typ))
  | Int(n) -> Fpat.Const.Int(n)
  | RandInt -> Fpat.Const.RandInt
  | Add -> Fpat.Const.Add Fpat.Type.mk_int
  | Sub -> Fpat.Const.Sub Fpat.Type.mk_int
  | Mul -> Fpat.Const.Mul Fpat.Type.mk_int
  | Char c -> Fpat.Const.Int (int_of_char c)
  | String s -> Fpat.Const.String s
  | Float s -> Fpat.Const.Float (float_of_string s)
  | Int32 n -> Fpat.Const.Int (Int32.to_int n)
  | Int64 n -> Fpat.Const.Int (Int64.to_int n)
  | Nativeint n -> Fpat.Const.Int (Nativeint.to_int n)
  | CPS_result -> Fpat.Const.Unint(Fpat.Type.mk_const (Fpat.TypConst.Ext "X"), "end")
  | _ -> Format.printf "%a@." CEGAR_print.const c; assert false

let rec conv_term t =
  match t with
  | Const(RandVal s) -> Fpat.Term.mk_var (Fpat.Idnt.make (new_id "r")) (***)
  | Const(c) -> Fpat.Term.mk_const (conv_const c)
  | Var(x) ->
     if is_parameter x || isEX_COEFFS x then
       Fpat.Term.mk_var (Fpat.Idnt.mk_coeff x)
     else
       Fpat.Term.mk_var (Fpat.Idnt.make x)
  | App(t1, t2) -> Fpat.Term.mk_app (conv_term t1) [conv_term t2]
  | Fun _ -> assert false
  | Let _ -> assert false

let conv_formula t = t |> conv_term |> Formula.of_term

let inv_const c =
  match c with
  | Fpat.Const.Unit -> Unit
  | Fpat.Const.True -> True
  | Fpat.Const.False -> False
  | Fpat.Const.And -> And
  | Fpat.Const.Or -> Or
  | Fpat.Const.Not -> Not
  | Fpat.Const.Lt ty when Fpat.Type.is_int ty -> Lt
  | Fpat.Const.Gt ty when Fpat.Type.is_int ty -> Gt
  | Fpat.Const.Leq ty when Fpat.Type.is_int ty -> Leq
  | Fpat.Const.Geq ty when Fpat.Type.is_int ty -> Geq
  | Fpat.Const.Eq ty when Fpat.Type.is_unit ty -> EqUnit
  | Fpat.Const.Eq ty when Fpat.Type.is_bool ty -> EqBool
  | Fpat.Const.Eq ty when Fpat.Type.is_int ty -> EqInt
  | Fpat.Const.Int(n) -> Int(n)
  | Fpat.Const.RandInt -> RandInt
  | Fpat.Const.Add ty when Fpat.Type.is_int ty -> Add
  | Fpat.Const.Sub ty when Fpat.Type.is_int ty -> Sub
  | Fpat.Const.Mul ty when Fpat.Type.is_int ty -> Mul
  | Fpat.Const.Eq ty when Fpat.Type.is_ext ty -> Fpat.Type.let_ext ty (fun typ -> CmpPoly(typ,"="))
  | Fpat.Const.Neq ty when Fpat.Type.is_ext ty -> Fpat.Type.let_ext ty (fun typ -> CmpPoly(typ,"<>"))
  | Fpat.Const.Lt ty when Fpat.Type.is_ext ty -> Fpat.Type.let_ext ty (fun typ -> CmpPoly(typ,"<"))
  | Fpat.Const.Gt ty when Fpat.Type.is_ext ty -> Fpat.Type.let_ext ty (fun typ -> CmpPoly(typ,">"))
  | Fpat.Const.Leq ty when Fpat.Type.is_ext ty -> Fpat.Type.let_ext ty (fun typ -> CmpPoly(typ,"<="))
  | Fpat.Const.Geq ty when Fpat.Type.is_ext ty -> Fpat.Type.let_ext ty (fun typ -> CmpPoly(typ,">="))
  | Fpat.Const.String s -> String s
  | Fpat.Const.Float x -> Float (string_of_float x)
  | Fpat.Const.Unint(ty, "end") when Fpat.Type.is_ext ty && Fpat.Type.let_ext ty ((=) "X") -> CPS_result
  | _ -> Format.printf "%s@." (Fpat.Const.string_of c); assert false

let rec inv_term t =
  match t with
  | Fpat.Term.Const(c) -> Const(inv_const c)
  | Fpat.Term.Var(x) -> Var(Fpat.Idnt.string_of x)
  | Fpat.Term.App(Fpat.Term.App(t1, t2), t3) ->
     (match t1 with
      | Fpat.Term.Const(Fpat.Const.Neq (ty)) when Fpat.Type.is_unit ty ->
         App(Const(Not), App(App(Const(EqUnit), inv_term t2), inv_term t3))
      | Fpat.Term.Const(Fpat.Const.Neq (ty)) when Fpat.Type.is_bool ty ->
         App(Const(Not), App(App(Const(EqBool), inv_term t2), inv_term t3))
      | Fpat.Term.Const(Fpat.Const.Neq (ty)) when Fpat.Type.is_int ty ->
         App(Const(Not), App(App(Const(EqInt), inv_term t2), inv_term t3))
      | _ ->
         App(App(inv_term t1, inv_term t2), inv_term t3))
  | Fpat.Term.App(t1, t2) -> App(inv_term t1, inv_term t2)
  | Fpat.Term.Binder(_, _, _) -> assert false

let inv_formula t = t |> Formula.term_of |> inv_term


let conv_event e = (***)
  match e with
  | Event(x) ->
     assert (x = "fail");
     Fpat.Term.mk_const (Fpat.Const.Event(x))
  | Branch(_) -> assert false

let conv_fdef (f, args, guard, events, body) =
  { Fpat.Fdef.name = f;
    Fpat.Fdef.args = args;
    Fpat.Fdef.guard = conv_formula guard;
    Fpat.Fdef.body =
      Fpat.Util.List.fold_right
        (fun e t ->
         Fpat.Term.mk_app
           (conv_event e)
           [Fpat.Term.mk_const Fpat.Const.Unit])
        events (conv_term body) } (***)

let inv_fdef fdef =
  fdef.Fpat.Fdef.name,
  fdef.Fpat.Fdef.args,
  inv_formula fdef.Fpat.Fdef.guard,
  [],
  inv_term fdef.Fpat.Fdef.body

let rec conv_typ ty =
  match ty with
  | TBase(TUnit, _) -> Fpat.Type.mk_unit
  | TBase(TInt, _) -> Fpat.Type.mk_int
  | TBase(TBool, _) -> Fpat.Type.mk_bool
  | TBase(TAbst "string", _) -> Fpat.Type.mk_string
  | TBase(TAbst "float", _) -> Fpat.Type.mk_float
  | TBase(TAbst s, _) ->
     Fpat.Type.mk_const (Fpat.TypConst.Ext s)
  | TFun(ty1,tmp) ->
     let ty2 = tmp (Const True) in
     Fpat.Type.mk_fun [conv_typ ty1; conv_typ ty2]
  | _ ->
     Format.printf "%a@." CEGAR_print.typ ty;
     assert false

let conv_prog (typs, fdefs, main) =
  { Fpat.Prog.fdefs = Fpat.Util.List.map conv_fdef fdefs;
    Fpat.Prog.types = Fpat.Util.List.map (fun (x, ty) -> Fpat.Idnt.make x, conv_typ ty) typs;
    Fpat.Prog.main = main }

let init prog =
  let prog =
    conv_prog
      (prog.CEGAR_syntax.env,
       prog.CEGAR_syntax.defs,
       prog.CEGAR_syntax.main)
  in
  prog |>
    Fpat.RefTypJudge.mk_temp_env |>
    Fpat.Util.List.map snd |>
    Fpat.Util.List.concat_map Fpat.RefType.pvars |>
    Fpat.Util.List.map (Fpat.PredVar.reset_uid >> Fpat.PredVar.normalize_args) |>
    Fpat.Util.List.unique |>
    Fpat.HCCSSolver.init_rsrefine

let verify fs (*cexs*) prog =
  let prog =
    conv_prog
      (prog.CEGAR_syntax.env,
       prog.CEGAR_syntax.defs,
       prog.CEGAR_syntax.main)
  in
  Format.printf "@[<v>BEGIN verification:@,  @[%a@]@," Fpat.Prog.pr prog;
  let _ = assert false(*Verifier.verify fs prog*) in
  Format.printf "END verification@,@]"

let rec inv_abst_type aty =
  match aty with
  | Fpat.AbsType.Base(Fpat.TypConst.Ext(id), x, ts) ->
     let x = Fpat.Idnt.string_of x in
     TBase(TAbst(id), fun s -> Fpat.Util.List.map (fun t -> subst x s (inv_formula t)) ts)
  | Fpat.AbsType.Base(Fpat.TypConst.Unit, x, ts) ->
     let x = Fpat.Idnt.string_of x in
     TBase(TUnit, fun s -> Fpat.Util.List.map (fun t -> subst x s (inv_formula t)) ts)
  | Fpat.AbsType.Base(Fpat.TypConst.Bool, x, ts) ->
     let x = Fpat.Idnt.string_of x in
     TBase(TBool, fun s -> Fpat.Util.List.map (fun t -> subst x s (inv_formula t)) ts)
  | Fpat.AbsType.Base(Fpat.TypConst.Int, x, ts) ->
     let x = Fpat.Idnt.string_of x in
     TBase(TInt, fun s -> Fpat.Util.List.map (fun t -> subst x s (inv_formula t)) ts)
  | Fpat.AbsType.Base(Fpat.TypConst.Float, x, ts) ->
     let x = Fpat.Idnt.string_of x in
     TBase(TAbst("float"), fun s -> Fpat.Util.List.map (fun t -> subst x s (inv_formula t)) ts)
  | Fpat.AbsType.Base(Fpat.TypConst.String, x, ts) ->
     let x = Fpat.Idnt.string_of x in
     TBase(TAbst("string"), fun s -> Fpat.Util.List.map (fun t -> subst x s (inv_formula t)) ts)
  | Fpat.AbsType.Fun(aty1, aty2) ->
     let x = if Fpat.AbsType.is_base aty1 then Fpat.Idnt.string_of (Fpat.AbsType.bv_of aty1) else "_dummy" in
     TFun(inv_abst_type aty1, fun t -> subst_typ x t (inv_abst_type aty2))
  | _ ->
     Format.printf "%a@." Fpat.AbsType.pr aty;
     assert false


let is_cp {env=env;defs=defs;main=main} =
  let prog = conv_prog (env, defs, main) in
  Fpat.RefTypInfer.is_cut_point prog

let infer flags labeled is_cp cexs prog =
  (*
  Fpat.Global.solve_rhs_left_to_right := flags land 2 <> 0;
  Fpat.Global.subst_hcs_inc := flags land 4 <> 0;
  Fpat.Global.no_inlining := flags land 8 <> 0 || not !Flag.expand_nonrec;
  Fpat.Global.fol_backward := flags land 32 <> 0;
  Fpat.Global.disable_pred_sharing1 := flags land 64 <> 0;
  Fpat.Global.enable_pred_sharing2 := flags land 128 <> 0;
  Fpat.Global.flag_coeff := flags land 256 <> 0;
   *)

  let prog = conv_prog prog in
  let env = Fpat.AbsTypInfer.refine prog labeled is_cp cexs in
  Flag.time_parameter_inference := !Flag.time_parameter_inference +. !Fpat.EHCCSSolver.elapsed_time;
  Fpat.Util.List.map
    (fun (f, rty) ->
     match f with Fpat.Idnt.V(id) -> id, inv_abst_type rty | _ -> assert false)
    env
(*
  Fpat.Util.List.map
    (fun (f, _) ->
      try
        f, conv_siz_type (Fpat.Util.List.assoc (Fpat.Idnt.make f) env)
      with Not_found ->
        assert false)
    prog.Fpat.Prog.types
 *)

let params = ref []
(** ToDo: exs may contain extra parameters that are not related to the recursive call *)
let new_params recursive bvs exs =
  Fpat.Util.List.unfold
    (fun i ->
     if i < !Fpat.Global.number_of_extra_params then
       let bvs' = Fpat.Util.List.filter (fun x -> x.Id.typ = Type.TInt) bvs in
       let ps =
         Fpat.Util.List.unfold
           (fun i ->
            if i < (Fpat.Util.List.length bvs' + if !Fpat.Global.enable_coeff_const then 1 else 0) then
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
         (if !Fpat.Global.enable_coeff_const (*&& recursive = None*) then
            Fpat.RefTypInfer.masked_params :=
              Fpat.Idnt.mk_coeff (Id.to_string (Fpat.Util.List.hd ps)) ::
                !Fpat.RefTypInfer.masked_params);
         (if !Fpat.Global.enable_coeff_const then [Term_util.make_var (Fpat.Util.List.hd ps)] else []) @
           (*
    let b = recursive <> None && xs = [] && Fpat.Util.Set.subset bvs' exs in
            *)
           Fpat.Util.List.map2
             (fun p x ->
              let _ =
                (*if b then
            ()
          else*) if recursive <> None then
                  (if xs = [] then
                     (if Fpat.Util.List.mem x exs then
                        Fpat.RefTypInfer.masked_params :=
                          Fpat.Idnt.mk_coeff (Id.to_string p) ::
                            !Fpat.RefTypInfer.masked_params (*this is necessary for l-length_cps-append.ml*))
                   else if not (Fpat.Util.List.mem x xs) then
                     Fpat.RefTypInfer.masked_params :=
                       Fpat.Idnt.mk_coeff (Id.to_string p) ::
                         !Fpat.RefTypInfer.masked_params)
              (* how to deal with non-recursive function calls here? *)
              (*else
            if Fpat.Util.List.mem x exs then
              Fpat.RefTypInfer.masked_params := Fpat.Idnt.mk_coeff (Fpat.Idnt.make (Id.to_string p)) :: !Fpat.RefTypInfer.masked_params*)
              in
              Term_util.make_mul (Term_util.make_var p) (Term_util.make_var x))
             (if !Fpat.Global.enable_coeff_const then Fpat.Util.List.tl ps else ps)
             bvs'
       in
       if ts = [] then
         Some(Term_util.make_int 0, i + 1)
       else
         Some(Fpat.Util.List.fold_left Term_util.make_add (Fpat.Util.List.hd ts) (Fpat.Util.List.tl ts), i + 1)
     else
       None)
    0

let gen_id =
  let cnt = ref 0 in
  fun () -> cnt := !cnt + 1; string_of_int !cnt

let rec trans_type typ =
  let xs, tyret = Type.decomp_tfun typ in
  let xs' =
    Fpat.Util.List.flatten
      (Fpat.Util.List.map
         (fun x ->
          let x' = trans_id x in
          (match x'.Id.typ with
           | Type.TFun(_, _)
           | Type.TPair(_, _)(* ToDo: fix it *) ->
              Fpat.Util.List.unfold
                (fun i ->
                 if i < !Fpat.Global.number_of_extra_params then
                   Some(Id.new_var "ex" Type.TInt, i + 1)
                 else
                   None)
                0
           | _ ->
              []) @ [x'])
         xs)
  in
  Fpat.Util.List.fold_right (fun x ty -> Type.TFun(x,ty)) xs' tyret
and trans_id x = Id.make x.Id.id x.Id.name (trans_type x.Id.typ)


let insert_extra_param t =
  let tmp = get_time() in
  let debug = !Flag.debug_level > 0 in
  Fpat.RefTypInfer.masked_params := [];
  let rec aux rfs bvs exs t =
    let desc =
      match t.Syntax.desc with
      | Syntax.Const c -> Syntax.Const c
      | Syntax.RandInt b -> Syntax.RandInt b
      | Syntax.RandValue(typ,b) -> Syntax.RandValue(typ,b)
      | Syntax.Var y -> Syntax.Var (trans_id y)
      | Syntax.Fun(y, t1) ->
         let y' = trans_id y in
         let ys =
           match y'.Id.typ with
           | Type.TFun(_, _)
           | Type.TPair(_, _)(* ToDo: fix it *) ->
              Fpat.Util.List.unfold
                (fun i ->
                 if i < !Fpat.Global.number_of_extra_params then
                   Some(Id.new_var ("ex" ^ gen_id ()) Type.TInt, i + 1)
                 else
                   None)
                0
           | _ ->
              []
         in
         let ys' = ys @ [y'] in
         let rfs =
           match rfs with
           | [] -> assert false
           | (f, xxs, recursive)::rfs' -> (f, xxs @ [y', ys], recursive)::rfs' in
         let f, _ =
           Fpat.Util.List.fold_left
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
           | Syntax.Var(f) ->
              (try
                  let _, xxss, _ = Fpat.Util.List.find (fun (f', _, recursive) -> recursive && Id.same f' f) rfs in
                  (if debug then
                     Format.printf "rec: %a@." Syntax.print_term t1');
                  let xxss = Fpat.Util.List.take (Fpat.Util.List.length ts) xxss in
                  true,
                  Fpat.Util.List.map2
                    (fun t (x, xs) ->
                     match t.Syntax.typ with
                     | Type.TFun(_, _)
                     | Type.TPair(_, _)(* ToDo: fix it *) ->
                        (match t.Syntax.desc with
                         | Syntax.Var(y) when Id.same x y ->
                            let _ = if debug then Format.printf "arg %a of %a not changed@," Syntax.print_id x Syntax.print_id f in xs
                         | _ -> [])
                     | _ -> [])
                    ts xxss
                with Not_found ->
                  (*let _ = Fpat.Util.List.iter (fun f -> Format.printf "r: %s@." f) rfs in*)
                  let _ = if debug then Format.printf "nonrec: %a@." Syntax.print_term t1' in
                  false, [])
           | _ ->
              let _ = if debug then Format.printf "nonrec: %a@." Syntax.print_term t1' in
              false, []
         in
         let ts' = Fpat.Util.List.map (aux rfs bvs exs) ts in
         let tss =
           Fpat.Util.List.mapi
             (fun i t ->
              match t.Syntax.typ with
              | Type.TFun(_, _)
              | Type.TPair(_, _)(* ToDo: fix it *) ->
                 new_params (if recursive then Some(Fpat.Util.List.nth xss i) else None) bvs exs
              | _ -> [])
             ts'
         in
         let ts'' = Fpat.Util.List.flatten (Fpat.Util.List.map2 (fun ts t -> ts @ [t]) tss ts') in
         Syntax.App(t1', ts'')
      | Syntax.If(t1, t2, t3) -> Syntax.If(aux rfs bvs exs t1, aux rfs bvs exs t2, aux rfs bvs exs t3)
      | Syntax.Branch(t1, t2) -> Syntax.Branch(aux rfs bvs exs t1, aux rfs bvs exs t2)
      | Syntax.Let(flag, bindings, t2) ->
         let bvs' =
           bvs @
             (if flag = Syntax.Nonrecursive then
                []
              else
                Fpat.Util.List.map
                  Fpat.Triple.fst bindings)
         in
         let aux' (f,xs,t) =
           let f' = trans_id f in
           let xs' = Fpat.Util.List.map trans_id xs in

           let xss =
             Fpat.Util.List.map
               (fun x ->
                match x.Id.typ with
                | Type.TFun(_, _)
                | Type.TPair(_, _)(* ToDo: fix it *) ->
                   Fpat.Util.List.unfold
                     (fun i ->
                      if i < !Fpat.Global.number_of_extra_params then
                        Some(Id.new_var ("ex" ^ gen_id ()) Type.TInt, i + 1)
                      else
                        None)
                     0
                | _ ->
                   [])
               xs'
           in
           let xs'' = Fpat.Util.List.flatten (Fpat.Util.List.map2 (fun xs x -> xs @ [x]) xss xs') in
           let bvs, exs =
             (if true then
                bvs' @ xs''
              else
                bvs' @ xs'),
             exs @ Fpat.Util.List.flatten xss
           in
           let rfs' = (f, Fpat.Util.List.map2 (fun xs x -> x, xs) xss xs', flag <> Syntax.Nonrecursive) :: rfs in
           (* mutual recursion and binding partial applied functions are not supported
              let rfs' = (if flag = Flag.Nonrecursive then [] else Fpat.Util.List.map (fun (f, _, _) -> Id.to_string f) bindings) @ rfs in
            *)
           f', xs'', aux rfs' bvs exs t
         in
         let bindings' = Fpat.Util.List.map aux' bindings in
         Syntax.Let
           (flag, bindings',
            aux rfs
                (bvs @
                   Fpat.Util.List.map
                     Fpat.Triple.fst
                     bindings')
                exs t2)
      | Syntax.BinOp(op, t1, t2) -> Syntax.BinOp(op, aux rfs bvs exs t1, aux rfs bvs exs t2)
      | Syntax.Not t1 -> Syntax.Not (aux rfs bvs exs t1)
      | Syntax.Event(s,b) -> Syntax.Event(s,b)
      | Syntax.Record fields -> Syntax.Record (Fpat.Util.List.map (fun (f,(s,t1)) -> f,(s,aux rfs bvs exs t1)) fields)
      | Syntax.Proj(i,s,f,t1) -> Syntax.Proj(i,s,f,aux rfs bvs exs t1)
      | Syntax.SetField(n,i,s,f,t1,t2) -> Syntax.SetField(n,i,s,f,aux rfs bvs exs t1,aux rfs bvs exs t2)
      | Syntax.Nil -> Syntax.Nil
      | Syntax.Cons(t1,t2) -> Syntax.Cons(aux rfs bvs exs t1, aux rfs bvs exs t2)
      | Syntax.Constr(s,ts) -> Syntax.Constr(s, Fpat.Util.List.map (aux rfs bvs exs) ts)
      | Syntax.Match(t1,pats) ->
         let aux' (pat, cond, t) =
           (* ToDo: need to update pat!? *)
           pat,
           aux rfs (bvs @ Syntax.get_vars_pat pat) exs cond,
           aux rfs (bvs @ Syntax.get_vars_pat pat) exs t
         in
         Syntax.Match(aux rfs bvs exs t1, Fpat.Util.List.map aux' pats)
      | Syntax.Raise t -> Syntax.Raise (aux rfs bvs exs t)
      | Syntax.TryWith(t1,t2) -> Syntax.TryWith(aux rfs bvs exs t1, aux rfs bvs exs t2)
      | Syntax.Pair(t1,t2) -> Syntax.Pair(aux rfs bvs exs t1, aux rfs bvs exs t2)
      | Syntax.Fst t -> Syntax.Fst(aux rfs bvs exs t)
      | Syntax.Snd t -> Syntax.Snd(aux rfs bvs exs t)
      | Syntax.Bottom -> Syntax.Bottom
      | Syntax.Label(info,t) -> Syntax.Label(info, aux rfs bvs exs t)
      | Syntax.Ref t -> Syntax.Ref(aux rfs bvs exs t)
      | Syntax.Deref t -> Syntax.Deref(aux rfs bvs exs t)
      | Syntax.SetRef(t1,t2) -> Syntax.SetRef(aux rfs bvs exs t1, aux rfs bvs exs t2)
      | Syntax.TNone -> Syntax.TNone
      | Syntax.TSome t -> Syntax.TSome(aux rfs bvs exs t)
    in
    {Syntax.desc=desc; Syntax.typ=trans_type t.Syntax.typ}
  in
  let res = aux [] [] [] t in
  let _ = add_time tmp Flag.time_parameter_inference in
  res

let instantiate_param (typs, fdefs, main as prog) =
  let tmp = get_time() in
  (if !Fpat.RefTypInfer.prev_sol = [] then
     Fpat.RefTypInfer.init_sol (conv_prog prog));
  let map =
    Fpat.Util.List.map
      (fun (x, n) ->
       Fpat.Idnt.string_of x, inv_term (Fpat.IntTerm.make n))
      !Fpat.RefTypInfer.prev_sol
  in
  let res =
	typs,
	Fpat.Util.List.map
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
(*
  if false then
	let _, t = CEGAR_trans.trans_term {Syntax.desc = t; Syntax.typ = Type.TBool } in
	let t = conv_formula t in
	let t = Formula.simplify t in
	let t = inv_formula t in
	(CEGAR_trans.trans_inv_term t).Syntax.desc
  else
 *)
    t

let simplify_typed_term p =
  { p with Syntax.desc = simplify_term p.Syntax.desc }

let rec simplify typ =
  match typ with
  | Ref_type.Base(base, x, p) ->
     Ref_type.Base(base, x, simplify_typed_term p)
  | Ref_type.Fun(x,typ1,typ2) ->
     Ref_type.Fun(x, simplify typ1, simplify typ2)
  | Ref_type.Pair(x,typ1,typ2) ->
     Ref_type.Pair(x, simplify typ1, simplify typ2)
  | Ref_type.Inter typs ->
	 Ref_type.Inter (Fpat.Util.List.map simplify typs)
  | Ref_type.Union typs ->
	 Ref_type.Union (Fpat.Util.List.map simplify typs)
  | Ref_type.ExtArg(x,typ1,typ2) ->
     Ref_type.ExtArg(x, simplify typ1, simplify typ2)
  | Ref_type.List(x,p_len,y,p_i,typ) ->
     Ref_type.List(x, simplify_typed_term p_len, y, simplify_typed_term p_i, typ)



let compute_strongest_post prog ce =
  Fpat.RankFunInfer.compute_strongest_post (conv_prog prog) ce


let report_error ppf = function
  | Fpat.AbsTypInfer.FailedToRefineTypes ->
     Format.fprintf ppf "Failure of abstraction type refinement"
  | Fpat.SMTProver.Unknown ->
     Format.fprintf ppf "Failure of SMT prover"
  | Fpat.InterpProver.Fail ->
     Format.fprintf ppf "Failure of interpolating prover (integer domain not fully supported)"
  | Fpat.InterpProver.Unknown ->
     Format.fprintf ppf "Failure of interpolating prover"
  | Fpat.RefTypInfer.FailedToRefineExtraParameters ->
     Format.fprintf ppf  "Failure of parameter substitution refinement"
  | Fpat.PolyConstrSolver.Unknown ->
     Format.fprintf ppf "Failure of polynomial constraint solver"
  | Fpat.Util.NotImplemented msg ->
     Format.fprintf ppf "Not implemented: %s" msg
  | _ -> raise Not_found

let string_of_error = make_string_of report_error

let is_fpat_exception = function
  | Fpat.AbsTypInfer.FailedToRefineTypes
  | Fpat.SMTProver.Unknown
  | Fpat.InterpProver.Fail
  | Fpat.InterpProver.Unknown
  | Fpat.RefTypInfer.FailedToRefineExtraParameters
  | Fpat.PolyConstrSolver.Unknown
  | Fpat.Util.NotImplemented _ -> true
  | _ -> false


let implies = Fpat.SMTProver.implies_dyn
let is_sat = Fpat.SMTProver.is_sat_dyn

module String = Fpat.Util.String
module List = Fpat.Util.List
module Array = Fpat.Util.Array
