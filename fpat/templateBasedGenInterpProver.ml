open Util
open Combinator
open GenInterpProver

(** A template based generalized interpolating prover *)

let interpolate pids tenv lbs nubs =
  let max_int =
    lbs @ nubs
    |> List.map Formula.constants
    |> List.flatten
    |> List.filter_map (function Const.Int(n) -> Some(n) | _ -> None)
    |> List.map abs
    |> List.cons 0
    |> Integer.max_list
  in
  if max_int >= 100 (* @todo too ad hoc *) then
    (* resort to generalized interpolating proved based on convex hull *)
    CHGenInterpProver.interpolate
      true pids tenv
      lbs nubs
  else
    let lb = Formula.bor lbs in
    if Formula.is_false lb then
      List.map
        (flip Pair.make (tenv, Formula.mk_false))
        pids
    else
      begin
        let nub = Formula.bor nubs in
        Logger.printf2
          "lb:%a@,nub:%a@,"
          Formula.pr lb
          Formula.pr nub;
        try
          let interp =
            (TemplateBasedInterpProver.interpolate_exc
             |> UnitBoolInterpProver.interpolate)
              lb nub
          in
          List.map (flip Pair.make (tenv, interp)) pids
        with
        | Not_found ->
          (* resort to generalized interpolating proved based on convex hull *)
          CHGenInterpProver.interpolate true pids tenv lbs nubs
      end
let interpolate =
  Logger.log_block4
    "TemplateBasedGenInterpProver.interpolate"
    ~after:(Logger.printf "output:@,  %a" PredSubst.pr)
    interpolate
