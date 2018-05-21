open Llvm

(** Additional functions for LLVM *)

(* This program is written by Naoki Takashima *)

external float_of_const : llvalue -> float option = "llvm_float_of_const"
external get_indices : llvalue -> int array = "llvm_get_indices"
