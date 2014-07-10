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

(* $Id: code.ml,v 1.2 2011-08-08 19:31:17 weis Exp $ *)

type registre = int;;

type op�rande =
  | Reg of registre
  | Imm of int
;;

type instruction =
  | Op of op�ration * registre * op�rande * registre
  | Jmp of op�rande * registre
  | Braz of registre * int
  | Branz of registre * int
  | Scall of int
  | Stop

and op�ration =
  | Load | Store | Add | Mult | Sub | Div
  | And | Or | Xor | Shl | Shr
  | Slt | Sle | Seq
;;

let nombre_de_registres = 32
and sp = 30
and ra = 31
and taille_du_mot = 4
;;
