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
open Syntaxe;;

type 'a env;;

val environnement_initial :
      (string * décl_proc) list -> (string * décl_fonc) list -> 'a env;;
val ajoute_variable : string -> 'a -> 'a env -> 'a env;;
val cherche_variable : string -> 'a env -> 'a;;
val cherche_fonction : string -> 'a env -> décl_fonc;;
val cherche_procédure : string -> 'a env -> décl_proc;;

exception Pas_trouvé of string;;
