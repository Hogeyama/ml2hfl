open Gsl

(** Additional functions for GSL *)

external solve_cqp :
  int -> Matrix.matrix -> Vector.vector ->
  int -> Matrix.matrix -> Vector.vector ->
  int -> Matrix.matrix -> Vector.vector ->
  Vector.vector -> Vector.vector -> Vector.vector ->
  unit
  = "solve_cqp_bytecode"
    "solve_cqp_native"
