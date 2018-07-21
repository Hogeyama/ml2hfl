open Util
open Combinator
open CQP

(** Interface to GSL *)

let _ = Gsl.Error.init ()

(** solves a convex quadratic programming by
    the Mehrotra's Predictor-Corrector interior point method
    @return a triple of
      minimum point,
      Lagrange-multipliers for A x = b,
      Lagrange-multipliers for C x >= d
*)
let solve_cqp_gsl cqp =
  try
    let n1 = List.length cqp.mQ in
    let mQ = cqp.mQ |> Matrix.array_of |> Gsl.Matrix.of_arrays in
    let vq = cqp.vq |> Vector.array_of |> Gsl.Vector.of_array in
    let n2 = List.length cqp.mA in
    let mA = cqp.mA |> Matrix.array_of |> Gsl.Matrix.of_arrays in
    let vb = cqp.vb |> Vector.array_of |> Gsl.Vector.of_array in
    let n3 = List.length cqp.mC in
    let mC = cqp.mC |> Matrix.array_of |> Gsl.Matrix.of_arrays in
    let vd = cqp.vd |> Vector.array_of |> Gsl.Vector.of_array in

    let v1 = Gsl.Vector.create n1 in
    let v2 = Gsl.Vector.create n2 in
    let v3 = Gsl.Vector.create n3 in
    ExtGSL.solve_cqp
      n1 mQ vq
      n2 mA vb
      n3 mC vd
      v1 v2 v3;
    v1 |> Gsl.Vector.to_array |> Vector.of_array,
    v2 |> Gsl.Vector.to_array |> Vector.of_array,
    v3 |> Gsl.Vector.to_array |> Vector.of_array
  with Gsl.Error.Gsl_exn _ -> raise PolyConstrSolver.Unknown

let solve_cqp_gsl =
  Logger.log_block1
    "GSLInterface.solve_cqp_gsl"
    ~before:(Logger.printf "%a@," CQP.pr)
    ~after:(fun (v1, v2, v3) ->
        Logger.printf "Minimum is found at:@,%a@," Vector.pr_float v1;
        Logger.printf "Lagrange-multipliers for Ax=b:@,%a@,"  Vector.pr_float v2;
        Logger.printf "Lagrange-multipliers for Cx>=d:@,%a@," Vector.pr_float v3)
    solve_cqp_gsl
    
let test_beale () =
  CQP.make
    [ Idnt.make "x1"; Idnt.make "x2"; Idnt.make "x3" ]
    [ [ 4.0; 2.0; 2.0 ];
      [ 2.0; 4.0; 0.0 ];
      [ 2.0; 0.0; 2.0 ] ]
    [ -8.0; -6.0; -4.0 ]
    [ [ -1.0; -1.0; -2.0 ] ]
    [ -3.0 ]
    [ [ 1.0; 0.0; 0.0 ];
      [ 0.0; 1.0; 0.0 ];
      [ 0.0; 0.0; 1.0 ] ]
    [ 0.0; 0.0; 0.0 ]
  |> solve_cqp_gsl


let solve_cqp_int cqp =
  cqp
  |> solve_cqp_gsl
  |> Triple.fst
  |> List.map2
    (fun var f ->
       let n = Float.round f(*@todo*) in
       Logger.printf3 "%a = %a (%a)@," Idnt.pr var Integer.pr n Float.pr f;
       var, IntTerm.make n)
    cqp.vars

(** A linear constraint solver based on convex quadratic programming *)
let solve_int phi =
  try
    let sol =
      if !PolyConstrSolver.cqp_mode < 2 then
        phi
        |> ((CQP.of_formula !PolyConstrSolver.cqp_mode >> solve_cqp_int)
            |> PolyConstrSolver.solve_quick)
      else
        phi
        |> ((CQP.of_formula !PolyConstrSolver.cqp_mode >> solve_cqp_int)
            |> PolyConstrSolver.solve_quick
            |> PolyConstrSolver.solve_int_by_nat)
    in
    if PolyConstrSolver.check phi sol then
      begin
        Logger.printf0 "CQP succeeded@,";
        sol
      end
    else
      begin
        Logger.printf0 "CQP failed@,";
        if !PolyConstrSolver.cqp_mode < 2 then PolyConstrSolver.solve_dyn phi
        else
          (* @todo *)
          Formula.fvs phi
          |> PolyConstrSolver.nat_constrs_of
          |> (@) [phi]
          |> Formula.band
          |> PolyConstrSolver.solve_dyn
      end
  with PolyConstrSolver.Unknown ->
    Logger.printf0 "CQP failed (unknown)@,";
    if !PolyConstrSolver.cqp_mode < 2 then PolyConstrSolver.solve_dyn phi
    else
      (* @todo *)
      Formula.fvs phi
      |> PolyConstrSolver.nat_constrs_of
      |> (@) [phi]
      |> Formula.band
      |> PolyConstrSolver.solve_dyn

let solve_int =
  Logger.log_block1
    "GSLInterface.solve_int"
    ~before:(Logger.printf "input: %a@," Formula.pr)
    solve_int

let _ = PolyConstrSolver.ext_solve_gsl := solve_int
