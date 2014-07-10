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

let compile f =
 match Sys.command ("ocamlc -c " ^ f) with
 | 0 -> ()
 | _ -> failwith ("Cannot compile " ^ f)
;;

compile "crayon.mli";;
compile "crayon.ml";;
#load "crayon.cmo";;

compile "langage.mli";;
compile "langage.ml";;
#load "langage.cmo";;

compile "alex.mli";;
compile "alex.ml";;
#load "alex.cmo";;

compile "asynt.mli";;
compile "asynt.ml";;
#load "asynt.cmo";;

compile "logo.ml";;
#load "logo.cmo";;

open Logo;;

print_string
 "Pour lancer l'interpréteur: boucle();;\n\
  Taper par exemple:\n\
  repete 4 [av 30 dr 90] .\n\
  Ou encore\n\
  pour carre :c\n \
   repete 4 [av 30 dr 90]\n\
  .\n\
 repete 30 [carre 30.0 dr 90.0]\n\
 ";
print_newline ();;
