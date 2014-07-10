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
(* $Id: main.mli,v 1.2 2011-08-08 19:31:17 weis Exp $ *)

val read_fun : int -> char option;;
val go : unit -> unit;;
val input_stream : in_channel ref;;
val trace_parsing : bool ref;;


