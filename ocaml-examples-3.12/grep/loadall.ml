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

(* $Id: loadall.ml,v 1.2 2011-08-08 19:31:17 weis Exp $ *)

let compile f =
  match Sys.command ("ocamlc -c " ^ f) with
  | 0 -> ()
  | _ -> failwith ("Cannot compile " ^ f)
;;

compile "expr.mli";;
compile "expr.ml";;
#load "expr.cmo";;
open Expr;;

compile "auto.mli";;
compile "auto.ml";;
#load "auto.cmo";;
open Auto;;

compile "ensent.mli";;
compile "ensent.ml";;
#load "ensent.cmo";;
open Ensent;;

compile "determ.mli";;
compile "determ.ml";;
#load "determ.cmo";;
open Determ;;

compile "grep.ml";;
#load "grep.cmo";;
open Grep;;

print_string "Pour lancer: grep \"expression rationnelle\" \"nom de fichier\"";
print_newline();;
