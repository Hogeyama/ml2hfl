(***********************************************************************)
(*                                                                     *)
(*                        Caml examples                                *)
(*                                                                     *)
(*            Pierre Weis                                              *)
(*                                                                     *)
(*                        INRIA Rocquencourt                           *)
(*                                                                     *)
(*  Copyright (c) 1994-2011, INRIA                                     *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  Distributed under the BSD license.                                 *)
(*                                                                     *)
(***********************************************************************)
type registre = int;;

type opérande =
   | Reg of registre
   | Imm of int;;

type instruction =
   | Op of opération * registre * opérande * registre
   | Jmp of opérande * registre
   | Braz of registre * int
   | Branz of registre * int
   | Scall of int
   | Stop

and opération =
  | Load | Store | Add | Mult | Sub | Div
  | And | Or | Xor | Shl | Shr
  | Slt | Sle | Seq;;

val nombre_de_registres : int;;
val sp : int;;
val ra : int;;
val taille_du_mot : int;;
