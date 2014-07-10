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

compile "esbit.mli";;
compile "esbit.ml";;
#load "esbit.cmo";;

compile "fileprio.mli";;
compile "fileprio.ml";;
#load "fileprio.cmo";;

compile "huffman.mli";;
compile "huffman.ml";;
#load "huffman.cmo";;

compile "compr.ml";;
#load "compr.cmo";;

open Compr;;

print_string
  "Pour lancer: compresse_fichier \"nom du fichier\" ou
                décompresse_fichier \"nom du fichier\"";
print_newline()
;;
