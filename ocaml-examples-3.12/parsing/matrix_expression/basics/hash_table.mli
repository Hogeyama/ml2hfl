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

(* $Id: hash_table.mli,v 1.1 2011-08-08 19:09:58 weis Exp $ *)

type ('a, 'b) t = ('a, 'b) Hashtbl.t;;

val bindings : ('a, 'b) t -> ('a * 'b) list;;
val values : ('a, 'b) t -> 'b list;;
val of_bindings : ('a * 'b) list -> ('a, 'b) t;;
