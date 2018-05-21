open Util
open Combinator

(** Mixed integer linear programming problems

      max/min c^t x
         s.t. b1 <= A x <= b2,
              l <= x <= u.

    where vc is a n1-vector
          mA is a (n2 x n1)-matrix
          vb1 is a n2-vector
          vb2 is a n2-vector
          vl is a n1-vector
          vu is a n1-vector
    @ensure |env| = n1
*)
type t =
  { env: TypEnv.t;
    max: bool;
    vc: float Vector.t;
    mA: float Matrix.t;
    vb1: float Vector.t;
    vb2: float Vector.t;
    vl: float Vector.t;
    vu: float Vector.t }

let make env max vc mA vb1 vb2 vl vu =
  { env = env; max = max; vc = vc; mA = mA;
    vb1 = vb1; vb2 = vb2; vl = vl; vu = vu }

let rec pr ppf mip =
  Format.fprintf
    ppf
    "env:@,%a@,%s@,c:@,%a@,A:@,%a@,b1:@,%a@,b2:@,%a@,l:@,%a@,u:@,%a@,"
    TypEnv.pr mip.env
    (if mip.max then "maximize" else "minimize")
    Vector.pr_float mip.vc
    Matrix.pr_float mip.mA
    Vector.pr_float mip.vb1
    Vector.pr_float mip.vb2
    Vector.pr_float mip.vl
    Vector.pr_float mip.vu


let minimize_lambdas = true

exception NonMIP

(** @require phi is cube and contains only QFLIA atoms
    @require the coefficients and the variables in [phi] are non-negative *)
let of_formula_nat phi =
  let cs = phi |> Formula.coeffs |> List.unique in
  let xs = cs @ (phi |> Formula.fvs |> List.unique) in
  try
    phi
    |> FormulaSimplifier.simplify (* this is necessary to eliminate negation *)
    |> Cube.of_formula
    |> List.map (LinIntRel.of_literal >> LinIntRel.coeffs_of xs)
    |> List.partition_map
      (function
        | (ns, Const.Geq ty) when Type.is_int ty -> `L(ns |> List.hd_tl)
        | (ns, Const.Leq ty) when Type.is_int ty -> `L(ns |> List.map (~-)
                                                       |> List.hd_tl)
        | (ns, Const.Eq ty) when Type.is_int ty ->  `R(ns |> List.hd_tl)
        | (_, c) ->
          Format.printf "c = %s@," (Const.string_of c);
          assert false)
    |> (fun (pnss, znss) ->
        let pns, pnss = List.unzip pnss in
        let zns, znss = List.unzip znss in
        let env =
          Logger.printf "xs: %a@," (List.pr Idnt.pr " ") xs;
          List.mapi
            (fun i x ->
               x, if i < List.length cs then Type.mk_int else Type.mk_real)
            xs
        in
        let vc = (* @todo *)
          let n = (if minimize_lambdas then xs else cs) |> List.length in
          List.gen (List.length xs) (fun i -> if i < n then 1.0 else 0.0)
        in
        let mA = pnss @ znss |> Matrix.map float_of_int in
        let vb1, vb2 =
          let pns' =
            pns |> Vector.map (fun x -> float_of_int (-x), infinity)
          in
          let zns' =
            zns |> Vector.map (fun x -> float_of_int (-x), float_of_int (-x))
          in
          List.unzip (pns' @ zns')
        in
        let vl = List.gen (List.length xs) (fun i -> 0.0 (* @todo *)) in
        let vu = List.gen (List.length xs) (fun i -> infinity) in
        make env false vc mA vb1 vb2 vl vu)
  with Cube.NonCube | Invalid_argument _ ->
    Logger.printf "non-mip formula: %a@," Formula.pr phi;
    raise NonMIP
