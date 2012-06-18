open ExtList

open CEGAR_syntax
open CEGAR_type
open CEGAR_print
open CEGAR_util

let conv_const c =
  match c with
  | Unit -> Const.Unit
  | True -> Const.True
  | False -> Const.False
  | And -> Const.And
  | Or -> Const.Or
  | Not -> Const.Not
  | Lt -> Const.Lt
  | Gt -> Const.Gt
  | Leq -> Const.Leq
  | Geq -> Const.Geq
  | EqUnit -> Const.EqUnit
  | EqBool -> Const.EqBool
  | EqInt -> Const.EqInt
  | Int(n) -> Const.Int(n)
  | RandInt -> Const.RandInt
  | Add -> Const.Add
  | Sub -> Const.Sub
  | Mul -> Const.Mul
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
  | Const.Lt -> Lt
  | Const.Gt -> Gt
  | Const.Leq -> Leq
  | Const.Geq -> Geq
  | Const.EqBool -> EqBool
  | Const.EqInt -> EqInt
  | Const.Int(n) -> Int(n)
  | Const.RandInt -> RandInt
  | Const.Add -> Add
  | Const.Sub -> Sub
  | Const.Mul -> Mul
  | _ -> Format.printf "%a@." Const.pr c; assert false

let rec inv_term t =
  match t with
    Term.Const(_, c) -> Const(inv_const c)
  | Term.Var(_, x) -> Var(Var.string_of x)
  | Term.App(_, Term.App(_, t1, t2), t3) ->
      (match t1 with
        Term.Const(_, Const.NeqUnit) -> App(Const(Not), App(App(Const(EqUnit), inv_term t2), inv_term t3))
      | Term.Const(_, Const.NeqBool) -> App(Const(Not), App(App(Const(EqBool), inv_term t2), inv_term t3))
      | Term.Const(_, Const.NeqInt) -> App(Const(Not), App(App(Const(EqInt), inv_term t2), inv_term t3))
      | _ -> App(App(inv_term t1, inv_term t2), inv_term t3))
  | Term.App(_, t1, t2) -> App(inv_term t1, inv_term t2)
  | Term.Forall (_, _, _) -> assert false
  | Term.Error _ -> assert false
  | Term.Ret (_, _, _, _) -> assert false
  | Term.Call (_, _, _) -> assert false


let conv_event e = (***)
  match e with
      Event(x) ->
        assert (x = "fail");
        Term.Const([], Const.Event(Idnt.make x))
    | Branch(_) -> assert false

let conv_fdef (f, args, guard, events, body) =
  { Fdef.attr = [];
    Fdef.name = Idnt.make f;
    Fdef.args = List.map Idnt.make args;
    Fdef.guard = conv_term guard;
    Fdef.body = List.fold_right (fun e t -> Term.apply (conv_event e) [Term.Const([],Const.Unit)]) events (conv_term body) } (***)

let inv_fdef fdef =
  Idnt.string_of fdef.Fdef.name,
  List.map Idnt.string_of fdef.Fdef.args,
  inv_term fdef.Fdef.guard,
  [],
  inv_term fdef.Fdef.body

let rec conv_typ ty =
  match ty with
    TBase(TUnit, _) -> SimType.Unit
  | TBase(TInt, _) -> SimType.Int
  | TBase(TBool, _) -> SimType.Bool
  | TFun(ty1,tmp) ->
      let ty2 = tmp (Const True) in
      SimType.Fun(conv_typ ty1, conv_typ ty2)
  | _ ->
      let _ = Format.printf "%a@." CEGAR_print.typ ty in
      assert false

let conv_prog (typs, fdefs, main) =
  { Prog.attr = [];
    Prog.fdefs = List.map conv_fdef fdefs;
    Prog.types = List.map (fun (x, ty) -> Idnt.make x, conv_typ ty) typs;
    Prog.main = Idnt.make main }

let verify fs (*cexs*) prog =
  let prog = conv_prog prog in
  Format.printf "@[<v>BEGIN verification:@,  @[%a@]@," Prog.pr prog;
  let _ = Verifier.verify fs prog in
  Format.printf "END verification@,@]"

let rec inv_abst_type aty =
		match aty with
    AbsType.Base(AbsType.Unit, x, ts) ->
      let x = Var.string_of x in
      TBase(TUnit, fun s -> List.map (fun t -> subst x s (inv_term t)) ts)
  | AbsType.Base(AbsType.Bool, x, ts) ->
      let x = Var.string_of x in
      TBase(TBool, fun s -> List.map (fun t -> subst x s (inv_term t)) ts)
  | AbsType.Base(AbsType.Int, x, ts) ->
      let x = Var.string_of x in
      TBase(TInt, fun s -> List.map (fun t -> subst x s (inv_term t)) ts)
  | AbsType.Fun(aty1, aty2) ->
      let x = if AbsType.is_base aty1 then Var.string_of (AbsType.bv_of aty1) else "_dummy" in
      TFun(inv_abst_type aty1, fun t -> subst_typ x t (inv_abst_type aty2))


let infer flags labeled cexs prog =
  let _ = Global.generalize_predicates_simple := flags land 1 <> 0 in
  let _ = Global.find_preds_forward := flags land 2 <> 0 in
  let _ = Global.subst_hcs_inc := flags land 4 <> 0 in
  let _ = Global.no_inlining := flags land 8 <> 0 || not !Flag.expand_nonrec in
  let _ = Global.inline_after_ncs := flags land 16 <> 0 in
  let _ = Global.fol_backward := flags land 32 <> 0 in
  let _ = Global.disable_pred_sharing1 := flags land 64 <> 0 in
  let _ = Global.enable_pred_sharing2 := flags land 128 <> 0 in
  let _ = Global.flag_coeff := flags land 256 <> 0 in

  let prog = conv_prog prog in
  let env = Verifier.refine labeled cexs prog in
  List.map
   (fun (f, rty) ->
     match f with Var.V(id) -> Idnt.string_of id, inv_abst_type rty)
   env
(*
  List.map
    (fun (f, _) ->
      try
        f, conv_siz_type (List.assoc (Var.make f) env)
      with Not_found ->
        assert false)
    prog.Prog.types
*)

let params = ref []
let number_of_extra_params = ref 1
let new_params bvs =
  Util.unfold
    (fun i ->
      if i < !number_of_extra_params then
						  let bts = List.map Syntax.make_var (List.filter (fun x -> x.Id.typ = Type.TInt) bvs) in
						  let ps =
          Util.unfold
            (fun i ->
              if i < (List.length bts + if !Global.coeff_const then 1 else 0) then
                Some(Id.new_var Flag.extpar_header Type.TInt, i + 1)
              else
                None)
            0
        in
						  let _ = params := !params @ ps in
						  let tps = List.map Syntax.make_var ps in
						  let ts =
          if !Global.coeff_const then
            List.hd tps :: List.map2 Syntax.make_mul (List.tl tps) bts
          else
            List.map2 Syntax.make_mul tps bts
        in
        Some(List.fold_left Syntax.make_add (Syntax.make_int 0) ts, i + 1)
      else
        None)
    0

let rec trans_type typ =
  let xs, tyret = Type.decomp_tfun typ in
  let xs' =
    List.flatten
      (List.map
        (fun x ->
          let x' = trans_id x in
          (match x'.Id.typ with
            Type.TFun(_, _) | Type.TPair(_, _)(* ToDo: fix it *) ->
				          Util.unfold
				            (fun i ->
				              if i < !number_of_extra_params then
				                Some(Id.new_var "ex" Type.TInt, i + 1)
				              else
				                None)
				            0
          | _ ->
              []) @ [x'])
        xs)
  in
  List.fold_right (fun x ty -> Type.TFun(x,ty)) xs' tyret
and trans_id x = Id.make x.Id.id x.Id.name (trans_type x.Id.typ)


let insert_extra_param t =
		let rec aux bvs t =
		  let desc =
		    match t.Syntax.desc with
		        Syntax.Unit -> Syntax.Unit
		      | Syntax.True -> Syntax.True
		      | Syntax.False -> Syntax.False
		      | Syntax.Unknown -> Syntax.Unknown
		      | Syntax.Int n -> Syntax.Int n
		      | Syntax.NInt y -> Syntax.NInt y
		      | Syntax.RandInt b -> Syntax.RandInt b
		      | Syntax.RandValue(typ,b) -> Syntax.RandValue(typ,b)
		      | Syntax.Var y -> Syntax.Var (trans_id y)
		      | Syntax.Fun(y, t1) ->
            let y' = trans_id y in
            Syntax.Fun(y', aux (bvs @ [y']) t1)
		
		      | Syntax.App(t1, ts) ->
		          let t1' = aux bvs t1 in
		          let ts' = List.map (aux bvs) ts in
            let tss =
              List.map
                (fun t ->
                  match t.Syntax.typ with
                    Type.TFun(_, _) | Type.TPair(_, _)(* ToDo: fix it *) -> new_params bvs
                  | _ -> [])
                ts'
            in
            let ts'' = List.flatten (List.map2 (fun ts t -> ts @ [t]) tss ts') in
		          Syntax.App(t1', ts'')
		
		      | Syntax.If(t1, t2, t3) -> Syntax.If(aux bvs t1, aux bvs t2, aux bvs t3)
		      | Syntax.Branch(t1, t2) -> Syntax.Branch(aux bvs t1, aux bvs t2)
		
		      | Syntax.Let(flag, bindings, t2) ->
		          let aux' (f,xs,t) =
              let f' = trans_id f in
              let xs' = List.map trans_id xs in

              let xs'' =
                List.flatten
                  (List.map
                    (fun x ->
                      (match x.Id.typ with
                        Type.TFun(_, _) | Type.TPair(_, _)(* ToDo: fix it *) ->
				                      Util.unfold
				                        (fun i ->
				                          if i < !number_of_extra_params then
				                            Some(Id.new_var "ex" Type.TInt, i + 1)
				                          else
				                            None)
				                        0
                      | _ ->
                          []) @ [x])
                    xs')
              in
		            f', xs'', aux (bvs @ (if flag = Flag.Nonrecursive then [] else [f']) @ xs'') t
		          in
            let bindings' = List.map aux' bindings in
		            Syntax.Let(flag, bindings', aux (bvs @ List.map (fun (f,_,_) -> f) bindings') t2)
		
		      | Syntax.BinOp(op, t1, t2) -> Syntax.BinOp(op, aux bvs t1, aux bvs t2)
		      | Syntax.Not t1 -> Syntax.Not (aux bvs t1)
		      | Syntax.Event(s,b) -> Syntax.Event(s,b)
		      | Syntax.Record fields -> Syntax.Record (List.map (fun (f,(s,t1)) -> f,(s,aux bvs t1)) fields)
		      | Syntax.Proj(i,s,f,t1) -> Syntax.Proj(i,s,f,aux bvs t1)
		      | Syntax.SetField(n,i,s,f,t1,t2) -> Syntax.SetField(n,i,s,f,aux bvs t1,aux bvs t2)
		      | Syntax.Nil -> Syntax.Nil
		      | Syntax.Cons(t1,t2) -> Syntax.Cons(aux bvs t1, aux bvs t2)
		      | Syntax.Constr(s,ts) -> Syntax.Constr(s, List.map (aux bvs) ts)
		      | Syntax.Match(t1,pats) ->
		          let aux' (pat,cond,t1) =
              (* need to update pat!? *)
              pat, Utilities.apply_opt (aux (bvs @ Syntax.get_vars_pat pat)) cond, aux bvs t1
            in
		            Syntax.Match(aux bvs t1, List.map aux' pats)
		      | Syntax.Raise t -> Syntax.Raise (aux bvs t)
		      | Syntax.TryWith(t1,t2) -> Syntax.TryWith(aux bvs t1, aux bvs t2)
		      | Syntax.Pair(t1,t2) -> Syntax.Pair(aux bvs t1, aux bvs t2)
		      | Syntax.Fst t -> Syntax.Fst(aux bvs t)
		      | Syntax.Snd t -> Syntax.Snd(aux bvs t)
		      | Syntax.Bottom -> Syntax.Bottom
		  in
		    {Syntax.desc=desc; Syntax.typ=trans_type t.Syntax.typ}
		in
		aux [] t

let instantiate_param (typs, fdefs, main as prog) =
  let _ = if !Verifier.ext_coeffs = [] then Verifier.init_coeffs (conv_prog prog) in
  let map = List.map (fun (x, n) -> Var.string_of x, inv_term (Term.tint n)) !Verifier.ext_coeffs in
  (typs, List.map (fun (f, args, guard, events, body) -> (f, args, CEGAR_util.subst_map map guard, events, CEGAR_util.subst_map map body)) fdefs, main)
