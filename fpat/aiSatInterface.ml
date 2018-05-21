open Util
open Combinator

module C = AiSat.MyPolynomial.Coeff
module P = AiSat.MyPolynomial.Poly

let of_term vars =
  CunTerm.fold
    (object
      method fvar x [] =
        try P.var (List.assoc (Idnt.string_of x) vars)
        with Not_found -> failwith "Reference to a variable that is not declared."
      method funit () = assert false
      method ftrue () = assert false
      method ffalse () = assert false
      method fint n = P.const (C.const (C.Coeff.of_float (float_of_int n)))
      method fbigint n = P.const (C.const (C.Coeff.of_float (Big_int_Z.float_of_big_int n)))
      method frational _ _ = assert false
      method freal f = P.const (C.const (C.Coeff.of_float f))
      method fstring _ = assert false
      method fneg _ r = P.(~- r)
      method fadd _ r1 r2 = P.(r1 + r2)
      method fsub _ r1 r2 = P.(r1 - r2)
      method fmul _ r1 r2 = P.(r1 * r2)
      method fdiv _ r1 r2 = assert false
      (*@todo P.(r1 / C.const (C.Coeff.of_float r2)) *)
      method fmax _ r1 r2 = assert false
      method fmin _ r1 r2 = assert false
      method fmod r1 r2 = assert false
      method ftuple _ rs = assert false
      method fproj _ _ _ = assert false
      method fkon _ _ _ = assert false
      method faccessor _ _ _ _ = assert false
      method fufun _ _ _ = assert false
      method fsempty ty = assert false
      method fsadd ty r1 r2 = assert false
      method fsunion ty r1 r2 = assert false
      method fsintersect ty r1 r2 = assert false
      method fsdiff ty r1 r2 = assert false
      method fscomplement ty r = assert false
      method farray n rs = assert false
      method faget a n = assert false
      method faset a n m e = assert false
      method fcoerce ty t = assert false
      method fformula phi = assert false
    end)

let of_atom vars =
  CunAtom.fold
    (object
      method fvar x [] = assert false
      method feq ty t1 t2 = AiSat.Constraint.EqZ (of_term vars (NumTerm.sub ty t1 t2))
      method fneq ty t1 t2 = AiSat.Constraint.NeqZ (of_term vars (NumTerm.sub ty t1 t2))
      method flt ty t1 t2 = AiSat.Constraint.GtZ (of_term vars (NumTerm.sub ty t2 t1))
      method fgt ty t1 t2 = AiSat.Constraint.GtZ (of_term vars (NumTerm.sub ty t1 t2))
      method fleq ty t1 t2 = AiSat.Constraint.GeZ (of_term vars (NumTerm.sub ty t2 t1))
      method fgeq ty t1 t2 = AiSat.Constraint.GeZ (of_term vars (NumTerm.sub ty t1 t2))
      method fdivides n t = assert false
      method frecognizer ty x t1 = assert false
      method fsmem ty e s = assert false
      method fssubset ty s1 s2 =  assert false
      method fterm c ts = assert false
    end)

let of_formula vars =
  Formula.elim_imply_iff
  >> Formula.map_atom CunAtom.elim_beq_bneq
  >> Formula.map_atom CunAtom.elim_neq
  >> Formula.map_atom (CunAtom.elim_gt >> Formula.of_atom)
  >> Formula.map_atom (CunAtom.elim_geq >> Formula.of_atom)
  >> DNF.of_formula
  >> DNF.cubes_of
  >> List.map (Cube.conjunction_of
               >> List.map (Literal.of_formula
                            >> Literal.fold (of_atom vars) (fun _ -> assert false)))
let of_formula = Logger.log_block2 "AiSatInterface.of_formula" of_formula

let of_poly polyvars p =
  P.to_list p
  |> List.map
    (fun (m, c) ->
       Osdp.Monomial.to_list m
       |> List.concat_mapi
         (fun i d ->
            let x = List.nth polyvars i |> fst |> Idnt.make |> Term.mk_var in
            List.duplicate x d)
       |> RealTerm.prod
       |> RealTerm.mul (C.to_float c |> RealTerm.make))
  |> RealTerm.sum

let atom_of polyvars = function
  | AiSat.Constraint.EqZ(p) ->
    RealFormula.eq (of_poly polyvars p) RealTerm.zero
  | AiSat.Constraint.NeqZ(p) ->
    RealFormula.neq (of_poly polyvars p) RealTerm.zero
  | AiSat.Constraint.GeZ(p) ->
    RealFormula.geq (of_poly polyvars p) RealTerm.zero
  | AiSat.Constraint.GtZ(p) ->
    RealFormula.gt (of_poly polyvars p) RealTerm.zero

let formula_of_dnf polyvars =
  List.map (List.map (atom_of polyvars) >> Formula.band) >> Formula.bor

let max_degree = ref 10
let rec aisat_interpolate phi1 phi2 degree trunc bits polyvars p1 p2 =
  AiSat.Util.reset ();
  AiSat.Util.mathematicaCheck := true;
  AiSat.Util.grad := !InterpProver.grad;
  AiSat.Util.debug := !InterpProver.aisat_debug;
  AiSat.Util.nocomb := !InterpProver.nocomb;
  AiSat.Util.truncate := trunc;
  AiSat.Util.num_of_bits := bits;
  AiSat.Util.sdpoption := 
    AiSat.Util.{ !sdpoption with verbose = 1 };
  if !InterpProver.use_mosek then
    AiSat.Util.sdpoption :=
      AiSat.Util.{ !sdpoption with solver = Osdp.Sdp.Mosek };
  try
    let dnf =
      AiSat.Constraint.interpolant_dnf
        (List.map (snd >> AiSat.MyPolynomial.progsym_of_int) polyvars)
        p1 p2 degree
    in
    AiSat.Util.reset ();
    let interp = dnf |> formula_of_dnf polyvars in
    Format.printf "An interpolant found:@.  %a@." Formula.pr interp;
    if CheckInterpProver.is_interpolant interp phi1 phi2 then interp
    else raise AiSat.Constraint.InterpolantNotFound
  with
  | AiSat.Constraint.InterpolantNotFound ->
    if degree > !max_degree then begin
        Format.printf "No interpolant found@.";
        raise InterpProver.Unknown(*NoInterpolant*)
    end else aisat_interpolate phi1 phi2 (degree + 1) trunc bits polyvars p1 p2

let interpolate p phi1 phi2 =
  Format.printf
    "Finding an interpolant of:@.  (1) %a@.  (2) %a@."
    Formula.pr phi1
    Formula.pr phi2;
  let tenv = (Formula.fvs phi1 @ Formula.fvs phi2)
             |> List.map (fun x -> x, Type.mk_real)
  in
  Format.printf "--- SMT-LIB2 code ---@.";
  Format.printf "%a@.@.(assert %a)@.(assert %a)@.@.(check-interpolant %d)@."
    (List.pr String.pr "@.") (TypEnv.sexp_of tenv)
    String.pr (CunFormula.sexp_of phi1)
    String.pr (CunFormula.sexp_of phi2)
    !InterpProver.degree;
  Format.printf "---------------------@.";
  let fvs = Formula.fvs phi1 @ Formula.fvs phi2 |> List.unique in
  let polyvars = List.mapi (fun i x -> Idnt.string_of x, i) fvs in
  aisat_interpolate
    phi1 phi2
    !InterpProver.degree !InterpProver.truncate !InterpProver.round_bits
    polyvars (of_formula polyvars phi1) (of_formula polyvars phi2)
let interpolate =
  Logger.log_block3
    ~before:(fun _ phi1 phi2 ->
        Logger.printf2
          "@[<v>input1: %a@ input2: %a@ @]"
          Formula.pr phi1
          Formula.pr phi2)
    ~after:(Logger.printf "output: %a" Formula.pr)
    "AiSatInterface.interpolate"
    interpolate

let _ = InterpProver.ext_interpolate_aisat := interpolate
