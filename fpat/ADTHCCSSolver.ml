open Util
open Combinator

(** An HCCS solver over ADTs *)

(** add >= 1 bounds to variables named Size_hoge@fuga (@todo improve the way to judge)*)
let add_bound hc =
  let re = Str.regexp "Size_[A-Za-z]" in
  let default_sizes =
    hc
    |> HornClause.fvs
    |> List.filter (fun x -> Str.string_match re (Idnt.string_of x) 0)
  in
  let bounds =
    default_sizes
    |> List.map (fun x -> IntFormula.geq (Term.mk_var x) (IntTerm.one))
    |> Formula.band
  in
  let head, body = HornClause.head_of hc, HornClause.body_of hc in
  let phi = Formula.mk_and (HornClause.phi_of_body body) bounds in
  let body' = HornClause.mk_body (HornClause.pvas_of_body body) phi in
  HornClause.make head body'

(** evaluate size functions given as uninterpreted functions *)
let eval_sizes phi =
  let extra_phis = ref [] in
  let rec aux t =
    match Term.fun_args t with
    | Term.Const(Const.UFun(ty, id)), [t] when SizeFun.is_size id ->
      if Term.is_var t |> not then
        let var = Term.new_var () in
        extra_phis := (Formula.neq Type.mk_unknown var t) :: !extra_phis; (* @NNF *)
        Logger.printf "extra: %a@," Formula.pr_list !extra_phis;
        Term.var_of_string (Idnt.string_of id ^ "@" ^ Term.string_of var)
      else Term.var_of_string (Idnt.string_of id ^ "@" ^ Term.string_of t)
    | t, ts -> Term.mk_app t (List.map aux ts)
  in
  phi
  |> Formula.map_atom (Atom.term_of >> aux >> Formula.of_term)
  |> Formula.mk_or (Formula.bor !extra_phis)
let eval_sizes =
  Logger.log_block1 "ADTHCCSSolver.eval_sizes"
    ~before:(Logger.printf "input: %a@," Formula.pr)
    ~after:(Logger.printf "output: %a" Formula.pr)
    eval_sizes

(* @todo refactor *)
let elim_constructors_atom tenv sizes var_tenv atm =
  let rec aux t =
    match Term.fun_args t with
    | Term.Var(x), [] ->
      let ty =
        if List.mem_assoc x var_tenv
        then List.assoc x var_tenv
        else Type.mk_int (* @todo var_tenv does not contain variables such as  __v9 *)
      in
      if Type.is_adt ty then begin
        (* Logger.printf "%a is adt@," Type.pr ty; *)
        let sizes' = SizeFun.of_ty sizes tenv ty in
        let size_x = (* names for size functions *)
          List.map
            (fun size ->
               Idnt.make @@
               (Idnt.string_of (fst size)) ^ "@" ^ (Idnt.string_of x))
            sizes'
        in
        let size_x = (* @todo make tuple *)
          if size_x = [] then
            (Logger.printf "can not eliminate constructors of %a@," Type.pr ty;
             assert false)
          else List.hd size_x
        in
        Term.mk_app (Term.mk_var size_x) []
      end else Term.mk_app (Term.mk_var x) []
    | Term.Const(Const.Con(ty, id)), ts ->
      (* get size *)
      let ty = if Type.is_unknown ty then TypEnv.lookup tenv id else ty in
      let sizes' = SizeFun.of_ty sizes tenv (Type.ret_of ty) in
      let defs = SizeFun.of_con sizes' id in
      (* @todo use tuple for multiple size functions *)
      let env, def = List.hd defs in
      let ts' = List.map aux ts in
      let mapping = List.zip env ts' in
      Term.subst mapping def

    (* @todo tricky *)
    | Term.Const(Const.Neq(ty)), [t1; t2] when Type.is_adt ty ->
      IntFormula.neq (aux t1) (aux t2) |> Formula.term_of
    | Term.Const(Const.Eq(ty)), [t1; t2] when Type.is_adt ty ->
      BoolTerm.mk_false

    | t, ts -> Term.mk_app t (List.map aux ts)
  in
  aux (atm |> Atom.term_of) |> Formula.of_term

(** abstract constructors using size functions
    e.g., "l = Cons(x, xs)" is abstracted to "size@l = 1 + size@xs"
    @todo support abstraction using multiple size functions *)
let elim_constructors tenv (sizes:SizeFun.t) phi =
  (* gather ADT typed variables *)
  let var_tenv = phi |> Formula.fvs_ty in
  Formula.map_atom (elim_constructors_atom tenv sizes var_tenv) phi
let elim_constructors =
  Logger.log_block3 "ADTHCCSSolver.elim_constructors"
    ~before:(fun _ sizes phi ->
        Logger.printf2 "input:@, sizes: %a@, phi: %a@," SizeFun.pr sizes Formula.pr phi)
    ~after:(Logger.printf "output: %a" Formula.pr)
    elim_constructors

let elim_accessors_recognizers (tenv, hccs) =
  hccs
  |> List.concat_map
    (Logger.pprintf "hc: %a@," HornClause.pr
     (* >> HornClause.map_phi (Formula.map_atom CunAtom.elim_beq_bneq) *)
     >> HornClause.formula_of
     >> CunFormula.elim_accessors_recognizers tenv
     >> HCCS.of_formula0 (HCCS.pvs hccs)
     >> HCCS.simplify >> HCCS.simplify_light []
     (* to rewrite Node(l1, r1) = Node(l2, r2) into l1 = l2 && r1=r2 *)
     >> HCCS.simplify
     >> SimTypInfer.infer_hccs tenv (* @todo do not infer *)
     >> snd
     >> Logger.pprintf "hccs: %a@," HCCS.pr)
  |> SimTypInfer.infer_hccs tenv |> snd
let elim_accessors_recognizers =
  Logger.log_block1 "ADTHCCSSolver.elim_accessors_recognizers"
    elim_accessors_recognizers

(** eliminate ADTs by abstracting them using size functions *)
let elim_adts sizes (tenv, hccs) =
  let sizes = if sizes = [] then SizeFun.make_sizes tenv else sizes in
  if sizes <> [] then Format.printf "size functions: %a@," SizeFun.pr sizes;
  hccs
  |> List.concat_map
    (Logger.pprintf "hc: %a@," HornClause.pr
     >> HornClause.map_phi (Formula.map_atom CunAtom.elim_beq_bneq)
     >> HornClause.formula_of
     >> CunFormula.elim_accessors_recognizers tenv
     (* >> FormulaSimplifier.simplify (\* assume NNF *\) *)
     >> HCCS.of_formula0 (HCCS.pvs hccs)
     >> HCCS.simplify >> HCCS.simplify_light []
     (* to rewrite Node(l1, r1) = Node(l2, r2) into l1 = l2 && r1=r2 *)
     >> HCCS.simplify
     >> SimTypInfer.infer_hccs tenv (* @todo do not infer *)
     >> snd
     >> HCCS.extract_constr_arg_equality
     >> List.map HornClause.formula_of >> Formula.band
     >> NNF.of_formula >> NNF.map_literal CunLiteral.simplify >> NNF.formula_of
     >> eval_sizes
     >> HCCS.of_formula0 (HCCS.pvs hccs)
     >> SimTypInfer.infer_hccs tenv >> snd
     >> List.map HornClause.formula_of >> Formula.band
     >> NNF.of_formula >> NNF.map_literal CunLiteral.simplify >> NNF.formula_of
     >> elim_constructors tenv sizes (* assume NNF *)
     >> FormulaSimplifier.simplify
     >> HCCS.of_formula0 (HCCS.pvs hccs)
     >> Logger.pprintf "hccs: %a@," HCCS.pr)
  |> List.map add_bound
  |> SimTypInfer.infer_hccs [](*@todo tenv*) |> snd
let elim_adts ?(sizes=[]) =
  Logger.log_block1 "ADTHCCSSolver.elim_adts"
    ~after:(Logger.printf "output: %a" HCCS.pr)
    (elim_adts sizes)
