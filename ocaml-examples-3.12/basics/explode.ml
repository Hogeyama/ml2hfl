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

(* $Id: explode.ml,v 1.5 2011-08-08 19:31:17 weis Exp $ *)

(** This is the implementation of the [Explode] module.

   In this file, the values listed in the interface must be defined.

   The type of each value should match the type declared in the interface file.

*)

let explode s =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) []
;;

let implode l =
  let result = String.create (List.length l) in
  let rec imp i = function
    | [] -> result
    | c :: l -> result.[i] <- c; imp (i + 1) l in
  imp 0 l
;;
