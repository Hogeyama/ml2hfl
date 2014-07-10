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

(* $Id: loadall.ml,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

(* To load everything into a Caml interactive system. *)

#use "hanoi-fra.ml";;
print_string "Pour lancer: jeu <nombre de disques>;;"; print_newline();;
#use "grahanoi-fra.ml";;
print_string "Pour lancer: jeu <nombre de disques>;;"; print_newline();;
#use "hanoi-eng.ml";;
print_string "To run: game <number of discus>;;"; print_newline();;
#use "grahanoi-eng.ml";;
print_string "To run: game <number of discus>;;"; print_newline();;
