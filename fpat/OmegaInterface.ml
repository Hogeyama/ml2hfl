open Util
open Combinator

(** Interface to Omega library *)

open SPPoC

let test () =
  [Ineq.make_eq (Form.of_string "x") (Form.of_string "y");
   Ineq.make_ge (Form.of_string "x") (Form.of_int 10)]
  |> System.make
  |> List.return
  |> Presburger.of_systems ["x"; "y"] [] []
  |> sef Presburger.print
  |> sef Presburger.simplify
  |> Presburger.print
