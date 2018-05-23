open Util
open Combinator

(** Atoms on unit, booleans, integers, tuples, uninterpreted
    functions, and algebraic data structures *)
(** support uninterpreted predicates and user-defined predicates *)

(*include NumAtom*)

let disable_elim_lt_gt = ref false

(** {6 Morphisms} *)

(* does not support t1 =b t2 and t1 <>b t2 *)
let fold f atm =
  (*Logger.printf "folding %a@," Atom.pr atm;*)
  match atm |> Atom.term_of |> Term.fun_args with
  | Term.Var(x), ts -> f#fvar x ts
  | Term.Const(Const.Eq(ty)), [t1; t2] -> f#feq ty t1 t2
  | Term.Const(Const.Neq(ty)), [t1; t2] -> f#fneq ty t1 t2
  | Term.Const(Const.Lt(ty)), [t1; t2] -> f#flt ty t1 t2
  | Term.Const(Const.Gt(ty)), [t1; t2] -> f#fgt ty t1 t2
  | Term.Const(Const.Leq(ty)), [t1; t2] -> f#fleq ty t1 t2
  | Term.Const(Const.Geq(ty)), [t1; t2] -> f#fgeq ty t1 t2
  | Term.Const(Const.Divides(n)), [t] -> f#fdivides n t
  | Term.Const(Const.Recognizer(ty, x)), [t1] -> f#frecognizer ty x t1
  | Term.Const(Const.SMem(ty)), [t1; t2] -> f#fsmem ty  t1 t2
  | Term.Const(Const.SSubset(ty)), [t1; t2] -> f#fssubset ty t1 t2
  | Term.Const(c), ts -> f#fterm c ts
  | _ ->
    Logger.debug_assert_false ~on_failure:(fun () ->
        Format.printf "not supported (in CunAtom.fold): %a@," Atom.pr atm)
      ()
(*let fold f atm = Logger.log_block2 "CunAtom.fold" fold f atm*)

let fold_brel f =
  fold
    (object
      method fvar x ts = f#fvar x ts
      method feq ty t1 t2 = f#fbrel (Const.Eq(ty)) t1 t2
      method fneq ty t1 t2 = f#fbrel (Const.Neq(ty)) t1 t2
      method flt ty t1 t2 = f#fbrel (Const.Lt(ty)) t1 t2
      method fgt ty t1 t2 = f#fbrel (Const.Gt(ty)) t1 t2
      method fleq ty t1 t2 = f#fbrel (Const.Leq(ty)) t1 t2
      method fgeq ty t1 t2 = f#fbrel (Const.Geq(ty)) t1 t2
      method fdivides n t = f#fdivides n t
      method frecognizer ty x t = f#frecognizer ty x t
      method fsmem ty t1 t2 = f#fsmem ty t1 t2
      method fssubset ty t1 t2 = f#fssubset ty t1 t2
      method fterm c ts = f#fterm c ts
    end)

let fold_brel_ty f =
  fold_brel
    (object
      method fvar = f#fvar
      method fbrel c t1 t2 =
        if Const.is_ubrel c then f#fubrel c t1 t2
        else if Const.is_bbrel c then f#fbbrel c t1 t2
        else if Const.is_ibrel c then f#fibrel c t1 t2
        else if Const.is_rbrel c then f#frbrel c t1 t2
        else if Const.is_brel c then f#fbrel c t1 t2
        else
          Logger.debug_assert_false ~on_failure:(fun () ->
              Format.printf
                "error in CunAtom.find_brel_ty: %a@,"
                Atom.pr (Atom.mk_brel c t1 t2))
            ()
      method fdivides n t = f#fdivides n t
      method frecognizer ty x t = f#frecognizer ty x t
      method fsmem ty t1 t2 = f#fsmem ty t1 t2
      method fssubset ty t1 t2 = f#fssubset ty t1 t2
      method fterm c ts = f#fterm c ts
    end)

(** {6 Printers} *)

let pr ppf =
  fold_brel
    (object
      method fvar x ts =
        Format.fprintf ppf "%a(%a)" Idnt.pr x (List.pr Term.pr ",") ts
      method fbrel c t1 t2 =
        Format.fprintf ppf
          "%a %s %a"
          Term.pr t1 (Const.string_of_infix c) Term.pr t2
      method fdivides n t = Format.fprintf ppf "%d | %a" n Term.pr t
      method frecognizer ty x t =
        Format.fprintf ppf "_is_%a(%a)" Idnt.pr x Term.pr t
      method fsmem ty t1 t2 =
        Format.fprintf ppf "%a in %a" Term.pr t1 Term.pr t2
      method fssubset ty t1 t2 =
        Format.fprintf ppf "%a sub %a" Term.pr t1 Term.pr t2
      method fterm c ts =
        Format.fprintf ppf "%s %a" (Const.string_of c) (List.pr Term.pr " ") ts
    end)
let _ = Atom.ext_pr := pr

(** {6 Operators} *)

let bnot =
  fold_brel
    (object
      method fvar x ts = Literal.mk_var x ts |> Literal.bnot
      (* Atom.eq Type.Bool t Formula.mk_false *)
      method fbrel c t1 t2 = Literal.mk_brel (Const.not c) t1 t2
      method fdivides n t = IntLiteral.divides n t |> Literal.bnot
      method frecognizer ty x t =
        ADTLiteral.mk_recognizer ty x t |> Literal.bnot
      method fsmem ty t1 t2 = SetLiteral.mk_mem ty t1 t2 |> Literal.bnot
      method fssubset ty t1 t2 = SetLiteral.mk_subset ty t1 t2 |> Literal.bnot
      method fterm c ts = assert false
    end)

(** replace atoms [t1 < t2] and [t1 > t2] respectively with
    [t1 + 1 <= t2] and [t1 >= t2 + 1] *)
let elim_lt_gt =
  fold
    (object
      method fvar = Atom.mk_var
      method feq = Atom.eq
      method fneq = Atom.neq
      method flt ty t1 t2 =
        if not !disable_elim_lt_gt && Type.is_int ty then begin
          Logger.printf0 "elim_lt!!@,";
          NumAtom.leq ty (IntTerm.add t1 (IntTerm.one)) t2
        end else NumAtom.lt ty t1 t2
      method fgt ty t1 t2 =
        if not !disable_elim_lt_gt && Type.is_int ty then begin
          Logger.printf0 "elim_gt!!@,";
          NumAtom.geq ty t1 (IntTerm.add t2 (IntTerm.one))
        end else NumAtom.gt ty t1 t2
      method fleq =  NumAtom.leq
      method fgeq = NumAtom.geq
      method fdivides = IntAtom.divides
      method frecognizer = ADTAtom.mk_recognizer
      method fsmem = SetAtom.mk_mem
      method fssubset = SetAtom.mk_subset
      method fterm = Atom.make
    end)
let elim_lt_gt =
  Logger.log_block1 "CunAtom.elim_lt_gt"
    ~before:(Logger.printf "input: %a@," Atom.pr)
    ~after:(Logger.printf "output: %a" Atom.pr)
    elim_lt_gt

(** replace atoms [t1 =b t2] and [t1 <>b t2]
    with [t1 iff t2] and [not (t1 iff t2)] respectively
    if [no_iff], then [iff] is encoded by [and, or, not]
    @require types inferred *)
let rec elim_beq_bneq no_iff atm =
  fold
    (object
      method fvar = Formula.mk_var
      method feq ty t1 t2 =
        if Type.is_bool ty then
          (t1, t2)
          |> Pair.lift Formula.of_term
          |> Pair.lift (Formula.map_atom (elim_beq_bneq no_iff))
          |> (if no_iff
              then uncurry2 Formula.mk_iff_disj
              else uncurry2 Formula.mk_iff)
        else NumFormula.eq ty t1 t2
      method fneq ty t1 t2 =
        if Type.is_bool ty then
          (t1, t2)
          |> Pair.lift Formula.of_term
          |> Pair.lift (Formula.map_atom (elim_beq_bneq no_iff))
          |> (if no_iff
              then uncurry2 Formula.mk_not_iff_conj
              else uncurry2 Formula.mk_iff >> Formula.bnot)
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
    atm
let elim_beq_bneq ?(no_iff = true) =
  Logger.log_block1 "CunAtom.elim_beq_bneq"
    ~before:(Logger.printf "input: %a@," Atom.pr)
    ~after:(Logger.printf "output: %a" Formula.pr)
    (elim_beq_bneq no_iff)

(** replace atoms [t1 <> t2] with [not t1 = t2]
    @note does not transform [t1] and [t2] in [t1 =b t2] and [t1 <>b t2] *)
let elim_neq_by_not_eq =
  fold
    (object
      method fvar = Literal.mk_var
      method feq = Literal.eq
      method fneq ty t1 t2 = Literal.eq ty t1 t2 |> Literal.bnot
      method flt = NumLiteral.lt
      method fgt = NumLiteral.gt
      method fleq = NumLiteral.leq
      method fgeq = NumLiteral.geq
      method fdivides = IntLiteral.divides
      method frecognizer = ADTLiteral.mk_recognizer
      method fsmem = SetLiteral.mk_mem
      method fssubset = SetLiteral.mk_subset
      method fterm = Literal.mk_atom
    end)

(** replace atoms [t1 > t2] with [t2 < t1] *)
let elim_gt =
  fold
    (object
      method fvar = Atom.mk_var
      method feq = Atom.eq
      method fneq = Atom.neq
      method flt = NumAtom.lt
      method fgt ty t1 t2 = NumAtom.lt ty t2 t1
      method fleq = NumAtom.leq
      method fgeq = NumAtom.geq
      method fdivides = IntAtom.divides
      method frecognizer = ADTAtom.mk_recognizer
      method fsmem = SetAtom.mk_mem
      method fssubset = SetAtom.mk_subset
      method fterm = Atom.make
    end)

(** replace atoms [t1 >= t2] with [t2 <= t1] *)
let elim_geq =
  fold
    (object
      method fvar = Atom.mk_var
      method feq = Atom.eq
      method fneq = Atom.neq
      method flt = NumAtom.lt
      method fgt = NumAtom.gt
      method fleq = NumAtom.leq
      method fgeq ty t1 t2 = NumAtom.leq ty t2 t1
      method fdivides = IntAtom.divides
      method frecognizer = ADTAtom.mk_recognizer
      method fsmem = SetAtom.mk_mem
      method fssubset = SetAtom.mk_subset
      method fterm = Atom.make
    end)

(** replace atoms [t1 <> t2] with [t1 < t2 || t1 > t2] *)
let elim_neq =
  fold
    (object
      method fvar = Formula.mk_var
      method feq = NumFormula.eq
      method fneq ty t1 t2 =
        if ty = Type.mk_int || ty = Type.mk_real || Type.is_var(*@todo*) ty
        then Formula.bor [NumFormula.lt ty t1 t2; NumFormula.gt ty t1 t2]
        else Formula.neq ty t1 t2
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

(** eliminate atoms "t1 = t2" and "t1 <> t2" respectively with
    "t1 <= t2 && t1 >= t2" and "t1 < t2 || t1 > t2" *)
let elim_eq_neq =
  fold
    (object
      method fvar = Formula.mk_var
      method feq ty t1 t2 =
        if ty = Type.mk_int || ty = Type.mk_real || Type.is_var(*@todo*) ty
        then Formula.band [NumFormula.leq ty t1 t2; NumFormula.geq ty t1 t2]
        else Formula.eq ty t1 t2
      method fneq ty t1 t2 =
        if ty = Type.mk_int || ty = Type.mk_real || Type.is_var(*@todo*) ty
        then Formula.bor [NumFormula.lt ty t1 t2; NumFormula.gt ty t1 t2]
        else Formula.neq ty t1 t2
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

let elim_neg =
  fold_brel_ty
    (object
      method fvar = Formula.mk_var
      method fubrel = Formula.mk_brel
      method fbbrel = Formula.mk_brel
      method fibrel c t1 t2 =
        try Formula.mk_brel c t1 t2 |> PolyIntRel.simplify_formula
        with Invalid_argument _ ->
          Format.printf "???: %a@," Formula.pr (Formula.mk_brel c t1 t2);
          assert false;
          Formula.mk_brel c t1 t2
      method frbrel = Formula.mk_brel
      method fbrel c t1 t2 =(*@todo*)
        try Formula.mk_brel c t1 t2 |> PolyIntRel.simplify_formula
        with Invalid_argument _ ->
          Format.printf "???: %a@," Formula.pr (Formula.mk_brel c t1 t2);
          assert false;
          Formula.mk_brel c t1 t2
      method fdivides = IntFormula.divides
      method frecognizer = ADTFormula.mk_recognizer
      method fsmem = SetFormula.mk_mem
      method fssubset = SetFormula.mk_subset
      method fterm = Formula.mk_atom
    end)

let elim_ifte =
  fold_brel
    (object
      method fvar = Formula.mk_var
      method fbrel c t1 t2 =
        let bs1 = CunTerm.branches_of Formula.mk_true t1 in
        let bs2 = CunTerm.branches_of Formula.mk_true t2 in
        Vector.multiply
          (fun (g1, t1) (g2, t2) ->
             Formula.imply (Formula.mk_and g1 g2) (Formula.mk_brel c t1 t2))
          bs1 bs2
        |> Formula.band
      (*Vector.multiply
        (fun (g1, t1) (g2, t2) ->
           Formula.band [g1; g2; Formula.mk_brel c t1 t2])
        bs1 bs2
        |> Formula.bor*)
      method fdivides = IntFormula.divides
      method frecognizer = ADTFormula.mk_recognizer
      method fsmem = SetFormula.mk_mem
      method fssubset = SetFormula.mk_subset
      method fterm = Formula.mk_atom
    end)

let linearize =
  fold_brel_ty
    (object
      method fvar = Formula.mk_var
      method fubrel = Formula.mk_brel
      method fbbrel = Formula.mk_brel
      method fibrel c t1 t2 = Atom.mk_brel c t1 t2 |> IntAtom.linearize
      method frbrel = Formula.mk_brel(*@todo*)
      method fbrel = Formula.mk_brel
      method fdivides = IntFormula.divides
      method frecognizer = ADTFormula.mk_recognizer
      method fsmem = SetFormula.mk_mem
      method fssubset = SetFormula.mk_subset
      method fterm = Formula.mk_atom
    end)

let rat_to_int =
  fold_brel
    (object
      method fvar = Atom.mk_var
      method fbrel c t1 t2 =
        try
          Formula.mk_brel c t1 t2
          |> LinRationalRel.of_formula
          |> LinRationalRel.lin_int_rel_of
          |> LinIntRel.formula_of
          |> Formula.atom_of
        with Invalid_argument(_) -> Atom.mk_brel c t1 t2
      method fdivides = IntAtom.divides
      method frecognizer = ADTAtom.mk_recognizer
      method fsmem = SetAtom.mk_mem
      method fssubset = SetAtom.mk_subset
      method fterm = Atom.make
    end)

let real_to_int =
  fold_brel
    (object
      method fvar = Atom.mk_var
      method fbrel c t1 t2 =
        try
          Formula.mk_brel c t1 t2
          |> LinRealRel.of_formula
          |> LinRealRel.lin_rat_rel_of
          |> LinRationalRel.lin_int_rel_of
          |> LinIntRel.formula_of
          |> Formula.atom_of
        with Invalid_argument(_) ->
        try
          Atom.mk_brel
            (Const.real_to_int c)
            (CunTerm.real_to_int t1)
            (CunTerm.real_to_int t2)
        with Invalid_argument(_) -> Atom.mk_brel c t1 t2
      method fdivides = IntAtom.divides
      method frecognizer = ADTAtom.mk_recognizer
      method fsmem = SetAtom.mk_mem
      method fssubset = SetAtom.mk_subset
      method fterm = Atom.make
    end)

let int_to_real =
  fold_brel
    (object
      method fvar = Atom.mk_var
      method fbrel c t1 t2 =
        Atom.mk_brel
          (Const.int_to_real c)
          (CunTerm.int_to_real t1)
          (CunTerm.int_to_real t2)
      method fdivides = IntAtom.divides
      method frecognizer = ADTAtom.mk_recognizer
      method fsmem = SetAtom.mk_mem
      method fssubset = SetAtom.mk_subset
      method fterm = Atom.make
    end)

(** @require psub is a map *)
let subst_pvars psub =
  fold_brel
    (object
      method fvar p ts =
        try
          let tenv, phi = List.assoc p psub in
          Formula.subst
            (try List.map2 (fun (x, _) t -> x, t) tenv ts
             with _ ->
               Format.printf "p: %a@.tenv: %a@.ts: %a@."
                 Idnt.pr p TypEnv.pr tenv Term.pr_list ts;
               assert false)
            phi
        with Not_found -> Formula.mk_var p ts
      method fbrel = Formula.mk_brel
      method fdivides = IntFormula.divides
      method frecognizer = ADTFormula.mk_recognizer
      method fsmem = SetFormula.mk_mem
      method fssubset = SetFormula.mk_subset
      method fterm = Formula.mk_atom
    end)

(** {6 Simplifiers} *)

let simplify_ub =
  fold_brel_ty
    (object
      method fvar = Atom.mk_var
      method fubrel = UnitAtom.simplify
      method fbbrel = BoolAtom.simplify
      method fibrel = Atom.mk_brel
      method frbrel = Atom.mk_brel
      method fbrel = Atom.mk_brel
      method fdivides = IntAtom.divides
      method frecognizer = ADTAtom.mk_recognizer
      method fsmem = SetAtom.mk_mem
      method fssubset = SetAtom.mk_subset
      method fterm = Atom.make
    end)

let simplify =
  fold_brel_ty
    (object
      method fvar = Atom.mk_var
      method fubrel = UnitAtom.simplify
      method fbbrel = BoolAtom.simplify
      method fibrel c t1 t2 =
        (*
        let (f1, ts1) = Term.fun_args t1 in
        let (f2, ts2) = Term.fun_args t2 in
        let t1', t2' =
          if Term.is_const f1 &&
             Term.let_const f1 (function Const.Div(_) -> true | _ -> false)
          then
            let [t11; t12] = ts1 in
            t11, (IntTerm.mul t2 t12)
          else t1, t2
        in
        let t1'', t2'' =
          if Term.is_const f2 &&
             Term.let_const f2 (function Const.Div(_) -> true | _ -> false)
          then
            let [t21; t22] = ts2 in
            (IntTerm.mul t1' t22), (IntTerm.mul t2' t22)
          else t1', t2'
        in
        *)
        Atom.mk_brel c (CunTerm.simplify t1) (CunTerm.simplify t2)
        |> elim_lt_gt |> IntAtom.simplify
      method frbrel c t1 t2 =
        Atom.mk_brel c (CunTerm.simplify t1) (CunTerm.simplify t2)
        |> RealAtom.simplify
      method fbrel c t1 t2 =
        let t1 = CunTerm.simplify t1 in
        let t2 = CunTerm.simplify t2 in
        match c with
        | Const.Eq ty ->
          if t1 = t2 then Atom.mk_true
          else begin
            try
              let kon1, args1 = ADTTerm.let_kon t1 (fun _ -> Pair.make) in
              let kon2, args2 = ADTTerm.let_kon t2 (fun _ -> Pair.make) in
              if kon1 = kon2 then
                if args1 = args2 then Atom.mk_true
                else
                  List.combine args1 args2
                  |> List.map (Pair.fold (Formula.eq Type.mk_unknown(*@todo*)))
                  |> Formula.band
                  |> Formula.atom_of
              else Atom.mk_false
            with _ -> Atom.eq ty t1 t2
          end
        |  Const.Neq ty ->
          if t1 = t2 then Atom.mk_false
          else begin
            try
              let kon1, args1 = ADTTerm.let_kon t1 (fun  _ -> Pair.make) in
              let kon2, args2 = ADTTerm.let_kon t2 (fun  _ -> Pair.make) in
              if kon1 = kon2 then
                if args1 = args2 then Atom.mk_false
                else
                  Atom.neq ty t1 t2
                  (*List.combine args1 args2
                    |> List.map (Pair.fold (Formula.neq Type.mk_unknown(*@todo*)))
                    |> Formula.bor*)
              else Atom.mk_true
            with _ -> Atom.neq ty t1 t2
          end
        | Const.Lt ty -> if t1 = t2 then Atom.mk_false else NumAtom.lt ty t1 t2
        | Const.Gt ty -> if t1 = t2 then Atom.mk_false else NumAtom.gt ty t1 t2
        | Const.Leq ty -> if t1 = t2 then Atom.mk_true else NumAtom.leq ty t1 t2
        | Const.Geq ty -> if t1 = t2 then Atom.mk_true else NumAtom.geq ty t1 t2
        | _ -> Atom.mk_brel c t1 t2
      method fdivides = IntAtom.divides
      method frecognizer = ADTAtom.mk_recognizer
      method fsmem = SetAtom.mk_mem
      method fssubset = SetAtom.mk_subset
      method fterm = Atom.make
    end)
let simplify =
  Logger.log_block1 "CunAtom.simplify"
    ~before:(Logger.printf "input: %a@," Atom.pr)
    ~after:(Logger.printf "output: %a" Atom.pr)
    simplify

let negate =
  fold_brel
    (object
      method fvar x ts = Literal.mk_var x ts |> Literal.bnot
      method fbrel c t1 t2 =
        Atom.mk_brel (Const.not c) t1 t2 |> simplify |> Literal.of_atom
      method fdivides n t = IntLiteral.divides n t |> Literal.bnot
      method frecognizer ty x t =
        ADTLiteral.mk_recognizer ty x t |> Literal.bnot
      method fsmem ty t1 t2 = SetLiteral.mk_mem ty t1 t2 |> Literal.bnot
      method fssubset ty t1 t2 = SetLiteral.mk_subset ty t1 t2 |> Literal.bnot
      method fterm c ts = Literal.mk_atom c ts |> Literal.bnot
    end)

(** {6 Inspectors} *)

let size =
  fold_brel
    (object
      method fvar x ts = Integer.sum_list (List.map CunTerm.size ts) + 1
      method fbrel c t1 t2 = CunTerm.size t1 + CunTerm.size t2 + 1
      method fdivides n t = CunTerm.size t + 1
      method frecognizer ty x t = assert false
      method fsmem ty t1 t2 = assert false
      method fssubset ty t1 t2 = assert false
      method fterm c ts = assert false
    end)

let sexp_of =
  fold
    (object
      method fvar x ts =
        if ts = [] then Idnt.string_of x
        else
          "(" ^
          Idnt.string_of x ^ " " ^
          String.concat " " (List.map CunTerm.sexp_of ts) ^
          ")"
      method feq ty t1 t2 =
        "(= " ^ CunTerm.sexp_of t1 ^ " " ^ CunTerm.sexp_of t2 ^ ")"
      method fneq ty t1 t2 =
        "(not (= " ^ CunTerm.sexp_of t1 ^ " " ^ CunTerm.sexp_of t2 ^ "))"
      method flt ty t1 t2 =
        "(< " ^ CunTerm.sexp_of t1 ^ " " ^ CunTerm.sexp_of t2 ^ ")"
      method fgt ty t1 t2 =
        "(> " ^ CunTerm.sexp_of t1 ^ " " ^ CunTerm.sexp_of t2 ^ ")"
      method fleq ty t1 t2 =
        "(<= " ^ CunTerm.sexp_of t1 ^ " " ^ CunTerm.sexp_of t2 ^ ")"
(*
          "(or " ^
            "(< " ^ CunTerm.sexp_of t1 ^ " " ^ CunTerm.sexp_of t2 ^ ") " ^
              "(= " ^ CunTerm.sexp_of t1 ^ " " ^ CunTerm.sexp_of t2 ^ "))"
 *)
      method fgeq ty t1 t2 =
        "(>= " ^ CunTerm.sexp_of t1 ^ " " ^ CunTerm.sexp_of t2 ^ ")"
(*
          "(or " ^
            "(< " ^ CunTerm.sexp_of t1 ^ " " ^ CunTerm.sexp_of t2 ^ ") " ^
              "(= " ^ CunTerm.sexp_of t1 ^ " " ^ CunTerm.sexp_of t2 ^ "))"
 *)
      method fdivides n t = assert false
      method frecognizer ty x t = assert false
      method fsmem ty t1 t2 = assert false
      method fssubset ty t1 t2 = assert false
      method fterm c ts = assert false
    end)

let is_linear =
  fold_brel_ty
    (object
      method fvar _ ts = List.for_all CunTerm.is_linear ts
      method fubrel _ t1 t2 = CunTerm.is_linear t1 && CunTerm.is_linear t2
      method fbbrel _ t1 t2 = CunTerm.is_linear t1 && CunTerm.is_linear t2
      method fibrel c t1 t2 = LinIntRel.is_linear (Formula.mk_brel c t1 t2)
      method frbrel c t1 t2 = CunTerm.is_linear t1 && CunTerm.is_linear t2
      (*@todo*)
      method fbrel _ t1 t2 = CunTerm.is_linear t1 && CunTerm.is_linear t2
      method fdivides _ t = CunTerm.is_linear t (* @todo*)
      method frecognizer ty x t = false
      method fsmem ty t1 t2 = assert false
      method fssubset ty t1 t2 = assert false
      method fterm c ts = false
    end)

let get_adts =
  fold_brel
    (object
      method fvar x xs = [](* @todo is this ok? *)
      method fbrel _ t1 t2 = CunTerm.get_adts t1 @ CunTerm.get_adts t2
      method fdivides n t = CunTerm.get_adts t
      method frecognizer ty _ t1 =
        let arg = Type.args_ret ty |> fst |> List.hd |> Type.base_or_adt_of in
        assert (TypConst.is_adt arg);
        arg :: CunTerm.get_adts t1
      method fsmem ty t1 t2 = CunTerm.get_adts t1 @ CunTerm.get_adts t2
      method fssubset ty t1 t2 = CunTerm.get_adts t1 @ CunTerm.get_adts t2
      method fterm c ts = List.concat_map CunTerm.get_adts ts
    end)

let kons =
  fold_brel
    (object
      method fvar x [] = []
      method fbrel _ t1 t2 = CunTerm.kons t1 @ CunTerm.kons t2
      method fdivides n t = CunTerm.kons t
      method frecognizer ty x t1 = CunTerm.kons t1
      method fsmem ty t1 t2 = CunTerm.kons t1 @ CunTerm.kons t2
      method fssubset ty t1 t2 = CunTerm.kons t1 @ CunTerm.kons t2
      method fterm c ts = List.concat_map CunTerm.kons ts
    end)

let ufuns_of f_formula =
  fold_brel
    (object
      method fvar _ ts = List.concat_map (CunTerm.ufuns_of f_formula) ts
      method fbrel _ t1 t2 =
        CunTerm.ufuns_of f_formula t1 @ CunTerm.ufuns_of f_formula t2
      method fdivides _ t1 = CunTerm.ufuns_of f_formula t1
      method frecognizer _ _ t1 = CunTerm.ufuns_of f_formula t1
      method fsmem ty e t = CunTerm.ufuns_of f_formula t
      method fssubset ty t1 t2 =
        CunTerm.ufuns_of f_formula t1 @ CunTerm.ufuns_of f_formula t2
      method fterm c ts = List.concat_map (CunTerm.ufuns_of f_formula) ts
    end)

let recognizers_of ty tenv =
  tenv
  |> (List.filter
        (Pair.map
           (Idnt.string_of >> flip (Str.string_match (Str.regexp "_is_")) 0)
           ((=) ty)
         >> Pair.fold (&&)))

let make_decl_ty tenv kon =
  let kon_ty = TypEnv.lookup tenv kon in
  kon_ty
  |> Type.args_ret
  |> Pair.map_fst (List.map (fun _ -> Term.new_var ())
                   >> ADTTerm.mk_kon (kon, kon_ty))

let rec elim_recognizers pos tenv =
  fold_brel
    (object
      method fvar = Atom.mk_var
      method fbrel = Atom.mk_brel
      method fdivides = IntAtom.divides
      method frecognizer ty kon t =
        if ADTTerm.is_kon t then
          ADTTerm.let_kon t (fun _ kon' _ ->
              BoolTerm.make (kon = kon') |> Atom.of_term)
        else if pos then
          (* simplify the following code *)
          recognizers_of ty tenv
          |> List.filter
            (fst >> (<>) (Const.Recognizer(ty, kon)
                          |> Const.string_of
                          |> Idnt.make))
          |> List.map
            (fun (id, typ) ->
               let id =
                 id
                 |> Idnt.string_of
                 |> Str.replace_first (Str.regexp "_is_") ""
                 |> Idnt.make
               in
               ADTAtom.mk_recognizer typ id t
               |> elim_recognizers (not pos) tenv
               |> Formula.of_atom
               |> Formula.bnot)
          |> Formula.band
          |> Formula.atom_of(*@todo*)
        else
          let decl, ty = make_decl_ty tenv kon in
          Atom.eq ty t decl
      method fsmem = SetAtom.mk_mem
      method fssubset = SetAtom.mk_subset
      method fterm = Atom.make
    end)

let elim_accessors tenv =
  fold_brel
    (object
      method fvar = Atom.mk_var
      method fbrel c t1 t2 =
        (* ex: replace
            "(_get_Cons_0 t1) = (1 + (_get_Cons_1 t2))" with "t1 = Cons(x1, x2) /\ t2 = Cons(x3,x4) => x1 = 1 + x4"
            "(_get_Cons_0 t) = tts"       with  "t = Cons(x1, x2) => x1 = tts"
            "(_get_Cons_0 t) + 1 <= tts"  with  "t = Cons(x1, x2) => x1 + 1 <= tts"
            "tts  = (_get_Cons_0 t)"      with  "t = Cons(x1, x2) => tts = x1"
            "tts <= (_get_Cons_0 t) + 1"  with  "t = Cons(x1, x2) => tts <= x1 + 1"
            for fresh x1 and x2 *)
        (match Term.fun_args t1, Term.fun_args t2 with
         | (Term.Const Const.Accessor(typ1, x1, idx1), [t1']),
           (Term.Const Const.Accessor(typ2, x2, idx2), [t2'])
         | (Term.Const Const.Accessor(typ1, x1, idx1), [t1']),
           (Term.Const Const.Add _, (Term.App(Term.Const Const.Accessor(typ2, x2, idx2),t2'))::_)
         | (Term.Const Const.Add _, (Term.App(Term.Const Const.Accessor(typ1, x1, idx1),t1'))::_),
           (Term.Const(Const.Accessor(typ2, x2, idx2)), [t2'])
         (* "(_get_Cons_0 t1) + 1 = (_get_Cons_1 t2)) + 1"
            "(_get_Cons_0 t1) + 1 = 1 + (_get_Cons_1 t2))"
            "1 + (_get_Cons_0 t1) = (_get_Cons_1 t2)) + 1"
            "1 + (_get_Cons_0 t1) = 1 + (_get_Cons_1 t2))" *)
         | (Term.Const Const.Add _,    (Term.App(Term.Const Const.Accessor(typ1, x1, idx1),t1'))::_),
           (Term.Const Const.Add _,    (Term.App(Term.Const Const.Accessor(typ2, x2, idx2),t2'))::_)
         | (Term.Const Const.Add _,    (Term.App(Term.Const Const.Accessor(typ1, x1, idx1),t1'))::_),
           (Term.Const Const.Add _, _::(Term.App(Term.Const Const.Accessor(typ2, x2, idx2),t2'))::_)
         | (Term.Const Const.Add _, _::(Term.App(Term.Const Const.Accessor(typ1, x1, idx1),t1'))::_),
           (Term.Const Const.Add _,    (Term.App(Term.Const Const.Accessor(typ2, x2, idx2),t2'))::_)
         | (Term.Const Const.Add _, _::(Term.App(Term.Const Const.Accessor(typ1, x1, idx1),t1'))::_),
           (Term.Const Const.Add _, _::(Term.App(Term.Const Const.Accessor(typ2, x2, idx2),t2'))::_) ->
           let decl1 = make_decl_ty tenv x1 |> fst in
           let decl2 = make_decl_ty tenv x2 |> fst in
           Formula.imply
             (Formula.mk_and
                (Formula.eq (TypEnv.lookup tenv x1 |> Type.ret_of) t1' decl1)
                (Formula.eq (TypEnv.lookup tenv x2 |> Type.ret_of) t2' decl2))
             (Formula.mk_brel c
                (decl1 |> Term.fun_args |> snd |> flip List.nth idx1)
                (decl2 |> Term.fun_args |> snd |> flip List.nth idx2))
           |> Formula.atom_of
         | (Term.Const(Const.Accessor(typ, x, idx)), [t]), tts
         | (Term.Const(Const.Add _),    (Term.App(Term.Const(Const.Accessor(typ, x, idx)),t))::_), tts
         | (Term.Const(Const.Add _), _::(Term.App(Term.Const(Const.Accessor(typ, x, idx)),t))::_), tts ->
           let decl = make_decl_ty tenv x |> fst in
           Formula.imply
             (Formula.eq (TypEnv.lookup tenv x |> Type.ret_of) t decl)
             (Formula.mk_brel c
                (decl |> Term.fun_args |> snd |> flip List.nth idx)
                (uncurry2 Term.mk_app tts))
           |> Formula.atom_of
         | tts, (Term.Const(Const.Accessor(typ, x, idx)), [t])
         | tts, (Term.Const(Const.Add _),    (Term.App(Term.Const(Const.Accessor(typ, x, idx)),t))::_)
         | tts, (Term.Const(Const.Add _), _::(Term.App(Term.Const(Const.Accessor(typ, x, idx)),t))::_) ->
           let decl = make_decl_ty tenv x |> fst in
           Formula.imply
             (Formula.eq (TypEnv.lookup tenv x |> Type.ret_of) t decl)
             (Formula.mk_brel c
                (uncurry2 Term.mk_app tts)
                (decl |> Term.fun_args |> snd |> flip List.nth idx))
           |> Formula.atom_of
         | _ -> Atom.mk_brel c t1 t2)
      method fdivides = IntAtom.divides
      method frecognizer = ADTAtom.mk_recognizer
      method fsmem = SetAtom.mk_mem
      method fssubset = SetAtom.mk_subset
      method fterm = Atom.make
    end)

(* assume NNF *)
let elim_ufuns =
  fold
    (object
      method fvar = Atom.mk_var
      method feq ty t1 t2 =
        if CunTerm.has_ufun t1 || CunTerm.has_ufun t2
        then Atom.mk_true
        else Atom.eq ty t1 t2
      method fneq ty t1 t2 =
        if CunTerm.has_ufun t1 || CunTerm.has_ufun t2
        then Atom.mk_true
        else Atom.neq ty t1 t2
      method flt = NumAtom.lt
      method fgt = NumAtom.gt
      method fleq = NumAtom.leq
      method fgeq = NumAtom.geq
      method fdivides = IntAtom.divides
      method frecognizer = ADTAtom.mk_recognizer
      method fsmem = SetAtom.mk_mem
      method fssubset = SetAtom.mk_subset
      method fterm = Atom.make
    end)
