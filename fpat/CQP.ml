open Util
open Combinator

(** Convex quadratic programming problems

      min x^t Q x + q^t x
      s.t. A x = b,
           C x >= d.

    where mQ is a (n1 x n1)-matrix
          vq is a n1-vector
          mA is a (n2 x n1)-matrix
          vb is a n2-vector
          mC is a (n3 x n1)-matrix
          vd is a n3-vector
*)

type t =
  { vars: Idnt.t list;
    mQ: float Matrix.t;
    vq: float Vector.t;
    mA: float Matrix.t;
    vb: float Vector.t;
    mC: float Matrix.t;
    vd: float Vector.t }

let make vars mQ vq mA vb mC vd =
  { vars = vars; mQ = mQ; vq = vq; mA = mA; vb = vb; mC = mC; vd = vd }

let rec pr ppf cqp =
  Format.fprintf
    ppf
    "Q:@,%a@,q:@,%a@,A:@,%a@,b:@,%a@,C:@,%a@,d:@,%a@,"
    Matrix.pr_float cqp.mQ
    Vector.pr_float cqp.vq
    Matrix.pr_float cqp.mA
    Vector.pr_float cqp.vb
    Matrix.pr_float cqp.mC
    Vector.pr_float cqp.vd

(** @require phi is cube *)
let of_formula cqp_mode phi =
  let cs = List.unique (Formula.coeffs phi) in
  let vs = cs @ List.unique (Formula.fvs phi) in
  let lenQ, lenq =
    match cqp_mode with
    | 0 -> List.length vs, 0 (* A *)
    | 1 -> List.length cs, 0 (* B *)
    | 2 -> 0, List.length vs (* @todo negative coeffs are ignored? *)
    | 3 -> 0, List.length cs (* @todo negative coeffs are ignored? *)
    | _ -> assert false
  in
  phi
  |> Cube.of_formula
  |> List.map (LinIntRel.of_literal >> LinIntRel.coeffs_of vs)
  |> List.partition_map
    (function
      | (ns, Const.Geq(ty)) when Type.is_int ty ->
        `L(ns |> List.hd_tl)
      | (ns, Const.Leq(ty)) when Type.is_int ty ->
        `L(ns |> List.map (~-) |> List.hd_tl)
      | (ns, Const.Eq(ty)) when Type.is_int ty ->
        `R(ns |> List.hd_tl)
      | (_, c) ->
        Format.printf "%s@," (Const.string_of c);
        assert false)
  |> (fun (pnss, znss) ->
      let pns, pnss = List.unzip pnss in
      let zns, znss = List.unzip znss in
      let mQ =
        (*Matrix.id 1.0 0.0 (List.length vs)*)
        List.gen
          (List.length vs)
          (fun i ->
             List.gen
               (List.length vs)
               (fun j -> if i = j && i < lenQ then 1.0 else 0.0))
      in
      let vq =
        (*Vector.make 0.0 (List.length vs)*)
        List.gen
          (List.length vs)
          (fun i -> if i < lenq then 1.0 else 0.0)
      in
      let mA = znss |> Matrix.map float_of_int in
      let vb = zns |> Vector.map ((~-) >> float_of_int) in
      let mC = pnss |> Matrix.map float_of_int in
      let vd = pns |> Vector.map ((~-) >> float_of_int) in
      make vs mQ vq mA vb mC vd)
