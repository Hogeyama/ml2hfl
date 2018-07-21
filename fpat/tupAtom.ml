open Util
open Combinator

(** Atoms on tuples *)

let get_proj_var = Atom.term_of >> TupTerm.get_proj_var
let has_proj = Atom.term_of >> TupTerm.has_proj
let eval_proj = Atom.term_of >> TupTerm.eval_proj >> Atom.of_term

(** eliminate equalities and disequalities of tuple terms
    both lhs and rhs must be of the form (t1,...,tn) *)
let elim_teq_tneq =
  CunAtom.fold
    (object
      method fvar = Formula.mk_var
      method feq ty t1 t2 =
        if Type.is_tuple_unknown ty then
          TupTerm.let_tuple t1 (fun tys1 ts1 ->
              TupTerm.let_tuple t2 (fun tys2 ts2 ->
                  List.map3 Formula.eq tys1 ts1 ts2 |> Formula.band)
                (fun () -> NumFormula.eq ty t1 t2))
            (fun () -> NumFormula.eq ty t1 t2)
        else NumFormula.eq ty t1 t2
      method fneq ty t1 t2 =
        if Type.is_tuple_unknown ty then
          TupTerm.let_tuple t1 (fun tys1 ts1 ->
              TupTerm.let_tuple t2 (fun tys2 ts2 ->
                  List.map3 Formula.neq tys1 ts1 ts2 |> Formula.bor)
                (fun () -> NumFormula.neq ty t1 t2))
            (fun () -> NumFormula.neq ty t1 t2)
        else NumFormula.neq ty t1 t2
      method flt = NumFormula.lt
      method fgt = NumFormula.gt
      method fleq = NumFormula.leq
      method fgeq = NumFormula.geq
      method fdivides = IntFormula.divides
      method frecognizer = ADTFormula.mk_recognizer
      method fsmem = SetFormula.mk_mem
      method fssubset = SetFormula.mk_subset
      method fterm = Formula.mk_atom
    end)
let elim_teq_tneq =
  Logger.log_block1 "TupAtom.elim_teq_tneq"
    ~before:(Logger.printf "input: %a@," Atom.pr)
    ~after:(Logger.printf "output: %a" Formula.pr)
    elim_teq_tneq
