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
open Code;;

exception Erreur of string * int;;

val lire_m�moire : int -> int;;
val �crire_m�moire : int -> int -> unit;;
val lire_registre : int -> int;;
val �crire_registre : int -> int -> unit;;
val tableau_des_appels_syst�me: (int -> int) array;;

val ex�cute: instruction array -> int -> unit;;
