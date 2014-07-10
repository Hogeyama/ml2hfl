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

(* $Id: mdl_basics.mli,v 1.1 2011-08-08 19:09:58 weis Exp $ *)

open Ident;;

(* In fact natural should be private, to ensure
  that a natural is indeed positive. *)
type natural = int
;;

type file_name = string
and dir_name = string
and explicit_file_name = string
and suffix = string
;;

type 'a ident_binding = {
  bound_ident : ident;
  bound_value : 'a;
}

and 'a ident_bindings = ('a ident_binding) list

and 'a ident_binding_table = (ident, 'a ident_binding) Hash_table.t
;;

type font = {
  font_name : font_name;
  font_angle : string;
  font_size : natural;
  font_weight : string;
}

and font_name = string
;;
