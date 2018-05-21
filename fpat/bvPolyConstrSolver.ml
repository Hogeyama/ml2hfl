open Util
open Combinator

(** A polynomial constraint solver based on bit-blasting
    (see [Gulwani+ PLDI 2008] for details) *)

(* Can we use the result by Seshia&Bryant to compute a precise threshold?
   No: unless unknown coefficients are bound, threshold cannot be bound *)

let bits_threshold = ref 3

let strategy () =
  List.init !bits_threshold (fun i -> i + 1, true (* nat *))
  @ List.init (!bits_threshold - 1(**@todo*)) (fun i -> i + 1, false (* int *))

let solve phi =
  try
    List.find_map
      (fun (bit, nat) ->
         try
           if nat then Some(PolyConstrSolver.solve_nat_bv bit phi)
           else Some(PolyConstrSolver.solve_bv bit phi)
         with
         | PolyConstrSolver.NoSolution
         | PolyConstrSolver.Unknown -> None)
      (strategy ())
  with Not_found -> raise PolyConstrSolver.Unknown
(** [solve phi] returns a map from each variable and coefficient in
    [phi] to an integer
    @require each variable and coefficient in [phi] has the integer type
    @raise PolyConstrSolver.Unknown if [solve] fails to sove [phi] *)
let solve = Logger.log_block1 "BvPolyConstrSolver.solve" solve
