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

(* $Id: ensent.ml,v 1.2 2011-08-08 19:31:17 weis Exp $ *)

type t = int list;;

let vide = [];;

let rec appartient n = function
  | [] -> false
  | m :: reste ->
     if m = n then true else
     if m > n then false else appartient n reste
;;

let rec ajoute n = function
  | [] -> [n]
  | m :: reste as ens ->
     if m = n then ens else
     if m > n then n :: ens else m :: ajoute n reste
;;
