open Util
open Combinator

(** Formulas on tuples *)

(** {6 Auxiliary constructors} *)

(* val has_proj : t -> bool *)
let has_proj = Formula.fold_bor TupAtom.has_proj

(* val get_proj_var : t -> TypEnv.t *)
let get_proj_var = Formula.fold_set TupAtom.get_proj_var

(* val eval_proj : t -> t *)
let eval_proj = Formula.map_atom (TupAtom.eval_proj >> Formula.of_atom)

(** eliminate projections of tuples *)
(* val elim_proj : t -> t *)
let rec elim_proj phi =
  phi
  |> (Pair.unfold (get_proj_var >> List.unique >> TermSubst.of_tuple_tenv) id)
  |> uncurry2 Formula.subst
  |> eval_proj
  |> if_ has_proj elim_proj id
let elim_proj =
  Logger.log_block1 "TupFormula.elim_proj"
    ~before:(Logger.printf "input: %a@," Formula.pr)
    ~after:(Logger.printf "output: %a" Formula.pr)
    elim_proj

(** transform atoms so that the argument of each projector is a variable
    @require each projector occurs in a formula of the form x = proj t *)
let normalize_proj =
  CunAtom.fold
    (object
      method fvar = Formula.mk_var
      method feq ty t1 t2 =
        match Term.fun_args t1, Term.fun_args t2 with
        | (Term.Var(x), []), (Term.Const(Const.Proj(tys, i)), [arg])
        | (Term.Const(Const.Proj(tys, i)), [arg]), (Term.Var(x), [])
          when not (Term.is_var arg) ->
          let y = Idnt.new_var () |> Term.mk_var in
          Formula.mk_and
            (Formula.eq (Type.mk_tuple tys) y arg)
            (Formula.eq ty (Term.mk_var x) (TupTerm.mk_proj tys i y))
        | _ -> Formula.eq ty t1 t2
      method fneq = Formula.neq
      method flt = NumFormula.lt
      method fgt = NumFormula.gt
      method fleq = NumFormula.leq
      method fgeq = NumFormula.geq
      method fdivides = IntFormula.divides
      method frecognizer = ADTFormula.mk_recognizer
      method fsmem = SetFormula.mk_mem
      method fssubset = SetFormula.mk_subset
      method fterm c ts = Term.mk_app (Term.mk_const c) ts |> Formula.of_term
    end)
