open ExtList

open Utilities
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
		let _ = Flag.time_parameter_inference := !Flag.time_parameter_inference +. !Verifier.elapsed_time in
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
(** ToDo: exs may contain extra parameters that are not related to the recursive call *)
let new_params recursive bvs exs =
  Util.unfold
    (fun i ->
      if i < !Global.number_of_extra_params then
						  let bvs' = List.filter (fun x -> x.Id.typ = Type.TInt) bvs in
						  let ps =
          Util.unfold
            (fun i ->
              if i < (List.length bvs' + if !Global.enable_coeff_const then 1 else 0) then
                Some(Id.new_var Flag.extpar_header Type.TInt, i + 1)
              else
                None)
            0
        in
						  let _ = params := !params @ ps in
								let xs =
										match recursive with
										  None -> []
										| Some(xs) -> xs
								in
					   let ts =
										let _ =
										  if !Global.enable_coeff_const (*&& recursive = None*) then
												  Verifier.masked_params := Var.make_coeff (Idnt.make (Id.to_string (List.hd ps))) :: !Verifier.masked_params
										in
          (if !Global.enable_coeff_const then [Syntax.make_var (List.hd ps)] else []) @
										let b = recursive <> None && xs = [] && Util.subset bvs' exs in
										List.map2
												(fun p x ->
													 let _ =
														  (*if b then
																  ()
																else*) if recursive <> None then
																  (if xs = [] then
																		  (if List.mem x exs then
      						  								Verifier.masked_params := Var.make_coeff (Idnt.make (Id.to_string p)) :: !Verifier.masked_params (*this is necessary for l-length_cps-append.ml*))
																		else if not (List.mem x xs) then
    						  								Verifier.masked_params := Var.make_coeff (Idnt.make (Id.to_string p)) :: !Verifier.masked_params)
																(* how to deal with non-recursive function calls here? *)
																(*else
																		if List.mem x exs then
      						  						Verifier.masked_params := Var.make_coeff (Idnt.make (Id.to_string p)) :: !Verifier.masked_params*)
														in
													 Syntax.make_mul (Syntax.make_var p) (Syntax.make_var x))
												(if !Global.enable_coeff_const then List.tl ps else ps)
												bvs'
        in
								if ts = [] then
          Some(Syntax.make_int 0, i + 1)
								else
          Some(List.fold_left Syntax.make_add (List.hd ts) (List.tl ts), i + 1)
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
				              if i < !Global.number_of_extra_params then
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
  let tmp = get_time() in
  let debug = !Global.debug in
  let _ = Verifier.masked_params := [] in
		let rec aux rfs bvs exs t =
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
            let ys =
              match y'.Id.typ with
                Type.TFun(_, _) | Type.TPair(_, _)(* ToDo: fix it *) ->
				              Util.unfold
				                (fun i ->
				                  if i < !Global.number_of_extra_params then
				                    Some(Id.new_var "ex" Type.TInt, i + 1)
				                  else
				                    None)
				                0
              | _ ->
                  []
            in
												let ys' = ys @ [y'] in
												let rfs = match rfs with [] -> assert false | (f, xxs, recursive)::rfs' -> (f, xxs @ [y', ys], recursive)::rfs' in
												let f, _ =
		            List.fold_left
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
								    let _ = match t1.Syntax.desc with Syntax.App(_, _) -> assert false | _ -> () in
		          let t1' = aux rfs bvs exs t1 in
												let recursive, xss =
												  match t1'.Syntax.desc with
														  Syntax.Var(f) ->
																  (try
		    												  let _, xxss, _ = List.find (fun (f', _, recursive) -> recursive && Id.same f' f) rfs in
																				let _ =
																				  if debug then
																						  Format.printf "rec: %a@." Syntax.pp_print_term t1'
																				in
																				let xxss = List.take (List.length ts) xxss in
																				true,
																				List.map2
																				  (fun t (x, xs) ->
																						  match t.Syntax.typ with
																								  Type.TFun(_, _) | Type.TPair(_, _)(* ToDo: fix it *) ->
																										  (match t.Syntax.desc with Syntax.Var(y) when Id.same x y ->
																												  let _ = if debug then Format.printf "arg %a of %a not changed@," Syntax.print_id x Syntax.print_id f in xs | _ -> [])
																								| _ -> [])
																						ts xxss
																		with Not_found ->
																				(*let _ = List.iter (fun f -> Format.printf "r: %s@." f) rfs in*)
			      											let _ = if debug then Format.printf "nonrec: %a@." Syntax.pp_print_term t1' in
																				false, [])
														| _ ->
      												let _ = if debug then Format.printf "nonrec: %a@." Syntax.pp_print_term t1' in
														    false, []
												in
				        let ts' = List.map (aux rfs bvs exs) ts in
		          let tss =
		            List.mapi
		              (fun i t ->
		                match t.Syntax.typ with
		                  Type.TFun(_, _) | Type.TPair(_, _)(* ToDo: fix it *) ->
																				  new_params (if recursive then Some(List.nth xss i) else None) bvs exs
		                | _ -> [])
		              ts'
		          in
		          let ts'' = List.flatten (List.map2 (fun ts t -> ts @ [t]) tss ts') in
				        Syntax.App(t1', ts'')
		
		      | Syntax.If(t1, t2, t3) -> Syntax.If(aux rfs bvs exs t1, aux rfs bvs exs t2, aux rfs bvs exs t3)
		      | Syntax.Branch(t1, t2) -> Syntax.Branch(aux rfs bvs exs t1, aux rfs bvs exs t2)
		
		      | Syntax.Let(flag, bindings, t2) ->
												let bvs' = bvs @ (if flag = Flag.Nonrecursive then [] else List.map Util.fst3 bindings) in
		          let aux' (f,xs,t) =
              let f' = trans_id f in
              let xs' = List.map trans_id xs in

              let xss =
                List.map
                  (fun x ->
                    match x.Id.typ with
                      Type.TFun(_, _) | Type.TPair(_, _)(* ToDo: fix it *) ->
				                    Util.unfold
				                      (fun i ->
				                        if i < !Global.number_of_extra_params then
				                          Some(Id.new_var "ex" Type.TInt, i + 1)
				                        else
				                          None)
				                      0
                    | _ ->
                        [])
                  xs'
              in
														let xs'' = List.flatten (List.map2 (fun xs x -> xs @ [x]) xss xs') in
														let bvs, exs =
														  (if true then
																  bvs' @ xs''
																else
		  														bvs' @ xs'),
																exs @ List.flatten xss
														in
  												let rfs' = (f, List.map2 (fun xs x -> x, xs) xss xs', flag <> Flag.Nonrecursive) :: rfs in
										    (*mutual recursion and binding partial applied functions are not supported
														let rfs' = (if flag = Flag.Nonrecursive then [] else List.map (fun (f, _, _) -> Id.to_string f) bindings) @ rfs in
														*)
		            f', xs'', aux rfs' bvs exs t
		          in
            let bindings' = List.map aux' bindings in
            Syntax.Let(flag, bindings', aux rfs (bvs @ List.map Util.fst3 bindings') exs t2)
		
		      | Syntax.BinOp(op, t1, t2) -> Syntax.BinOp(op, aux rfs bvs exs t1, aux rfs bvs exs t2)
		      | Syntax.Not t1 -> Syntax.Not (aux rfs bvs exs t1)
		      | Syntax.Event(s,b) -> Syntax.Event(s,b)
		      | Syntax.Record fields -> Syntax.Record (List.map (fun (f,(s,t1)) -> f,(s,aux rfs bvs exs t1)) fields)
		      | Syntax.Proj(i,s,f,t1) -> Syntax.Proj(i,s,f,aux rfs bvs exs t1)
		      | Syntax.SetField(n,i,s,f,t1,t2) -> Syntax.SetField(n,i,s,f,aux rfs bvs exs t1,aux rfs bvs exs t2)
		      | Syntax.Nil -> Syntax.Nil
		      | Syntax.Cons(t1,t2) -> Syntax.Cons(aux rfs bvs exs t1, aux rfs bvs exs t2)
		      | Syntax.Constr(s,ts) -> Syntax.Constr(s, List.map (aux rfs bvs exs) ts)
		      | Syntax.Match(t1,pats) ->
		          let aux' (pat, cond, t) =
              (* ToDo: need to update pat!? *)
              pat,
														Utilities.apply_opt (aux rfs (bvs @ Syntax.get_vars_pat pat) exs) cond,
														aux rfs (bvs @ Syntax.get_vars_pat pat) exs t
            in
		            Syntax.Match(aux rfs bvs exs t1, List.map aux' pats)
		      | Syntax.Raise t -> Syntax.Raise (aux rfs bvs exs t)
		      | Syntax.TryWith(t1,t2) -> Syntax.TryWith(aux rfs bvs exs t1, aux rfs bvs exs t2)
		      | Syntax.Pair(t1,t2) -> Syntax.Pair(aux rfs bvs exs t1, aux rfs bvs exs t2)
		      | Syntax.Fst t -> Syntax.Fst(aux rfs bvs exs t)
		      | Syntax.Snd t -> Syntax.Snd(aux rfs bvs exs t)
		      | Syntax.Bottom -> Syntax.Bottom
		  in
		    {Syntax.desc=desc; Syntax.typ=trans_type t.Syntax.typ}
		in
		let res = aux [] [] [] t in
  let _ = add_time tmp Flag.time_parameter_inference in
  res

let instantiate_param (typs, fdefs, main as prog) =
  let tmp = get_time() in
  let _ = if !Verifier.ext_coeffs = [] then Verifier.init_coeffs (conv_prog prog) in
  let map = List.map (fun (x, n) -> Var.string_of x, inv_term (Term.tint n)) !Verifier.ext_coeffs in
  let res = (typs, List.map (fun (f, args, guard, events, body) -> (f, args, CEGAR_util.subst_map map guard, events, CEGAR_util.subst_map map body)) fdefs, main) in
  let _ = add_time tmp Flag.time_parameter_inference in
  res
