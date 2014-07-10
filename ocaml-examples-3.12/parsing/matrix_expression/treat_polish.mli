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

(* $Id: treat_polish.mli,v 1.2 2011-08-11 15:51:27 weis Exp $ *)

type operator = string;;

val set_debug : unit -> unit;;
val treat_file : string -> (string list * operator list);;
val treat_string : string -> (string list * operator list);;
