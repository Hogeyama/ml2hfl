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
 | _ -> failwith ("Cannot compile " ^ f);;

compile "prelude.ml";;
#load "prelude.cmo";;
open Prelude;;

compile "terms.ml";;
#load "terms.cmo";;
open Terms;;

compile "equation.ml";;
#load "equation.cmo";;
open Equation;;

compile "order.ml";;
#load "order.cmo";;
open Order;;

compile "kb.ml";;
#load "kb.cmo";;
open Kb;;

compile "go.ml";;
#load "go.ml";;
