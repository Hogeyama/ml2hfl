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

(* $Id: hash_table_print.mli,v 1.1 2011-08-08 19:09:58 weis Exp $ *)

val print :
  (Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b -> unit) ->
  Format.formatter -> ('a, 'b) Hash_table.t -> unit
;;

val print_t : (* Format.formatter -> *)
  ('a -> unit) -> ('b -> unit) -> ('a, 'b) Hash_table.t -> unit
;;
