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

compile "syntaxe.mli";;

compile "eval.mli";;
compile "eval.ml";;
#load "eval.cmo";;

compile "lexuniv.mli";;
compile "lexuniv.ml";;
#load "lexuniv.cmo";;

compile "syntaxe.ml";;
#load "syntaxe.cmo";;

compile "types.mli";;
compile "types.ml";;
#load "types.cmo";;

compile "synthese.mli";;
compile "synthese.ml";;
#load "synthese.cmo";;

compile "caml.ml";;
#load "caml.cmo";;
open Caml;;

print_string
   "Pour lancer: boucle();;\
\n  Essayez par exemple:\
\n  let rec fib =\
\n    function x ->\
\n      if x <= 1 then 1 else fib (x - 1) + fib (x - 2)\
\n  ;;";
print_newline ()
;;



