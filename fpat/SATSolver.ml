open Util
open Combinator

(** SAT solvers *)

let ext_solve_minisat =
  ref (fun _ -> assert false : string -> (Formula.t * bool) list)
let solve_minisat_dyn filename = !ext_solve_minisat filename
