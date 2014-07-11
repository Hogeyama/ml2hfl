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

(* $Id: name.mli,v 1.1 2011-08-08 19:09:58 weis Exp $ *)

type name = string;;

val print_name : (* Format.formatter -> *)
  name -> unit
;;

type path = string;;

val print_path :
  path -> unit
;;