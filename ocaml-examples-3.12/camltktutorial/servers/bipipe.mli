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

(* $Id: bipipe.mli,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

(* The bipipe module to launch two programs connected via stdin/stdout. *)

val launch_connected_processes : string -> string -> unit;;
 (** [launch_connected_processes prog1 prog2] launches [prog1] and
 [prog2] with each other reading and writing to the corresponding
 [stdin/stdout] of the other program. *)
