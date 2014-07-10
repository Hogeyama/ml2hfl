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

(* $Id: restype.mli,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

(* Cannot use a functor at compile time to define the type of something. *)
module type RES = Fapplytype.Functor_type_apply (Imod.I);;
