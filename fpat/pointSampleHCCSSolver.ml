open Util
open Combinator

(** Point sampling based HCCS Solver *)

let use_integer_counterexamples = ref false
let linear_farkas = ref true
let ceg_samp_part = ref false
let ceg_dag_expand = ref false (* not implemented *)
let ceg_rec_unwind = ref false (* not implemented *)
let inc_temp_refine = ref false (* not implemented *)

(** constraint generation *)

(* this is complete only for convex constraints *)
let ray_convex = ref false
let ignore_origin = ref true

(** constraint solving *)

let complete = ref false

let complement tsub tenv =
  if !complete then tsub
  else
    tsub @
    List.filter_map
      (fun (x, ty) ->
         if List.mem_assoc x tsub || Idnt.is_coeff x then
           None
         else
           Some(x,
                if Type.is_real ty then RealTerm.zero
                else if Type.is_int ty || Type.is_var ty then IntTerm.zero
                else IntTerm.zero(*assert false*)))
      tenv

let dir_constr_of phi =
  phi
  |> Formula.atoms
  |> List.map @@ CunAtom.fold_brel_ty
    (object
      method fvar = assert false
      method fubrel = assert false
      method fbbrel = assert false
      method fibrel c t1 t2 =
        try
          let c, (nxs, n) =
            LinTermIntRel.of_formula (Formula.mk_brel c t1 t2)
          in
          match c with
          | Const.Eq _ | Const.Leq _| Const.Geq _ ->
            LinTermIntRel.formula_of (c, (nxs, IntTerm.zero))
          | Const.Neq _ -> Formula.mk_false
          | _ ->
            Format.printf "c: %a@." (Const.pr []) c;
            assert false
        with Invalid_argument(_) -> assert false
      method frbrel = assert false
      method fbrel = assert false
      method fdivides = assert false
      method frecognizer = assert false
      method fsmem = assert false
      method fssubset = assert false
      method fterm = assert false
    end)
  |> Formula.band
  |> FormulaSimplifier.simplify

let gen_constr psub samples =
  samples
  |> List.concat_map
    (fun (hc, ps, rs, pts) ->
       let phi0 =
         hc
         |> HornClause.simplify
         |> HornClause.map_phi (fun _ -> Formula.mk_true)
         |> HornClause.subst psub
         |> HornClause.formula_of_forward []
       in
       let tenv, phi =
         phi0
         |> Formula.bnot
         |> FormulaSimplifier.simplify
         |> (if !use_integer_counterexamples then id
             else Formula.map_atom (CunAtom.int_to_real >> Formula.of_atom))
         |> SimTypInfer.infer_formula []
       in
       let phi_norm =
         phi0
         |> Formula.bnot
         |> FormulaSimplifier.simplify (* assume NNF and without not *)
         (*|> Formula.map_atom CunAtom.elim_eq_neq @note better comment out*)
         |> Formula.map_atom (CunAtom.elim_lt_gt >> Formula.of_atom)
       in
       let point_constr =
         (* point samples *)
         List.map (fun p -> phi |> Formula.subst (complement p tenv)) ps
       in
       let ray_constr =
         (* ray samples *)
         if !ray_convex then
           let dphi =
             phi_norm
             |> dir_constr_of
             |> (if !use_integer_counterexamples then id
                 else Formula.map_atom (CunAtom.int_to_real >> Formula.of_atom))
           in
           List.map (fun (r, _) -> Formula.subst (complement r tenv) dphi) rs
         else
           List.map
             (fun (r, o) ->
                phi_norm
                |> Formula.map_atom @@ CunAtom.fold_brel_ty
                  (object
                    method fvar = assert false
                    method fubrel = assert false
                    method fbbrel = assert false
                    method fibrel c t1 t2 =
                      try
                        let c, (nxs, n) =
                          LinTermIntRel.of_formula (Formula.mk_brel c t1 t2)
                        in
                        match c with
                        | Const.Eq _ | Const.Leq _| Const.Geq _ ->
                          (if !ignore_origin then []
                           else
                             [LinTermIntRel.formula_of (c, (nxs, n))
                              |> Formula.subst (complement o tenv)])
                          |> (@)
                            [LinTermIntRel.formula_of (c, (nxs, IntTerm.zero))
                             |> Formula.subst (complement r tenv)]
                          |> Formula.band
                        | Const.Neq ty ->
                          Formula.mk_or
                            ((if !ignore_origin then []
                              else
                                [(Const.Lt ty, (nxs, n))
                                 |> LinTermIntRel.formula_of
                                 |> Formula.subst (complement o tenv)])
                             |> (@)
                               [(Const.Leq ty, (nxs, IntTerm.zero))
                                |> LinTermIntRel.formula_of
                                |> Formula.subst (complement r tenv)]
                             |> Formula.band)
                            ((if !ignore_origin then []
                              else
                                [(Const.Gt ty, (nxs, n))
                                 |> LinTermIntRel.formula_of
                                 |> Formula.subst (complement o tenv)])
                             |> (@)
                               [(Const.Geq ty, (nxs, IntTerm.zero))
                                |> LinTermIntRel.formula_of
                                |> Formula.subst (complement r tenv)]
                             |> Formula.band)
                        | _ ->
                          Format.printf "c: %a@." (Const.pr []) c;
                          assert false
                      with Invalid_argument(_) -> assert false
                    method frbrel = assert false
                    method fbrel = assert false
                    method fdivides = assert false
                    method frecognizer = assert false
                    method fsmem = assert false
                    method fssubset = assert false
                    method fterm = assert false
                  end)
                |> (if !use_integer_counterexamples then id
                    else Formula.map_atom (CunAtom.int_to_real
                                           >> Formula.of_atom)))
             rs
       in
       let polytope_constr =
         (* polytope samples *)
         List.map
           (fun phi' ->
              Formula.mk_and phi' phi0
              |> FormulaSimplifier.simplify
              |> DNF.of_formula |> DNF.formula_of
              |> (PolyConstrSolver.gen_coeff_constr
                    ~pos:false ~linear:!linear_farkas)
              |> (if !use_integer_counterexamples then id
                  else Formula.map_atom (CunAtom.int_to_real
                                         >> Formula.of_atom)))
           pts
       in
       point_constr @ ray_constr @ polytope_constr)
  |> Formula.band
  |> FormulaSimplifier.simplify
  |> (fun phi ->
      Formula.forall
        (Formula.fvs phi |> List.unique
         |> List.map
           (fun x -> x,
                     if !use_integer_counterexamples
                     then Type.mk_int
                     else Type.mk_real))
        phi)

let solve =
  Logger.pprintf "constraints on the coefficients: %a@," Formula.pr
  >> PolyConstrSolver.solve_coeffs_opt PolyConstrSolver.solve_dyn
    !use_integer_counterexamples
    (not !Polyhedron.enable_pol || !linear_farkas)

let cand_sol_from samples =
  if !ceg_samp_part then
    assert false
  else
    let psub =
      samples |> List.map Quadruple.fst |> HCCS.tenv |> Template.mk_psub
    in
    try
      let tsub =
        samples
        |> gen_constr psub
        |> solve
        |> TermSubst.complement ~real:false (PredSubst.coeffs psub)
      in
      psub
      |> List.map @@ Pair.map_snd @@ Pair.map
        id
        (Formula.subst tsub >> FormulaSimplifier.simplify)
    with
    | PolyConstrSolver.NoSolution | PolyConstrSolver.Unknown ->
      raise HCCSSolver.Unknown


let rec solve_main i ex samples =
  let sol = cand_sol_from samples in
  Format.printf "candidate solution:@.  %a@.@." PredSubst.pr sol;
  let updated = ref false in
  let count = ref 0 in
  let samples' =
    samples
    |> List.map
      (fun (hc, ps, rs, pts) ->
         match HornClause.find_point_cex
                 ~retint:!use_integer_counterexamples
                 ~extremal:ex hc sol
                 ~refute_all:!complete with
         | None -> hc, ps, rs, pts
         | Some(Polyhedron.Point p) ->
           count := !count + 1;
           Format.printf
             "a point counterexample found (%d):@.  @[<v>%a@]@."
             (i + !count)
             TermSubst.pr p;
           Format.printf "for the clause:@.  @[<v>%a@]@.@." HornClause.pr hc;
           updated := true; hc, p :: ps, rs, pts
         | Some(Polyhedron.ExPoint(p, dist)) ->
           count := !count + 1;
           Format.printf
             "an extremal point counterexample found (%d):@.  @[<v>%a@,distance: %a@]@."
             (i + !count)
             TermSubst.pr p
             Term.pr dist;
           Format.printf "for the clause:@.  @[<v>%a@]@.@." HornClause.pr hc;
           updated := true; hc, p :: ps, rs, pts
         | Some(Polyhedron.Ray(d, o)) ->
           count := !count + 1;
           Format.printf
             "a ray counterexample found (%d):@.  @[<v>%a@]@."
             (i + !count)
             (Pair.pr TermSubst.pr TermSubst.pr) (d, o);
           Format.printf "for the clause:@.  @[<v>%a@]@.@." HornClause.pr hc;
           updated := true; hc, ps, (d, o) :: rs, pts
         | Some(Polyhedron.Polytope(phi, p)) ->
           count := !count + 1;
           Format.printf
             "a point counterexample found (%d):@.  @[<v>%a@]@."
             (i + !count)
             TermSubst.pr p;
           Format.printf
             "a polytope counterexample found (%d):@.  @[<v>%a@]@."
             (i + !count)
             Formula.pr phi;
           Format.printf "for the clause:@.  @[<v>%a@]@.@." HornClause.pr hc;
           updated := true; hc, ps, rs, phi :: pts
         | Some(Polyhedron.ExPolytope(phi, p)) ->
           count := !count + 1;
           Format.printf
             "an extremal point counterexample found (%d):@.  @[<v>%a@]@."
             (i + !count)
             TermSubst.pr p;
           Format.printf
             "an extremal polytope counterexample found (%d):@.  @[<v>%a@]@."
             (i + !count)
             Formula.pr phi;
           Format.printf "for the clause:@.  @[<v>%a@]@.@." HornClause.pr hc;
           updated := true; hc, ps, rs, phi :: pts)
  in
  if !updated then solve_main (i + !count) ex samples'
  else begin
    Format.printf "genuine solution found using %d samples!@.@." i;
    sol
  end

let solve ex = List.map (fun hc -> hc, [], [], []) >> solve_main 0 ex
let solve ?(ex=false) =
  Logger.log_block1
    "PointSampleHCCSSolver.solve"
    ~before:(Logger.printf "input: %a@," HCCS.pr)
    (solve ex)

let solve_rhccs ?(ex=false) hcs = solve ~ex hcs, []
