(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*               Pierre Weis, projet Cristal, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  only by permission.                                                *)
(*                                                                     *)
(***********************************************************************)

(* $Id: modtype_fun.ml,v 1.1 2009-11-14 12:03:51 weis Exp $ *)

module type TYP_MOD = sig
  module type TYP;;
end
;;

module Fun_type (F : TYP_MOD) : TYP_MOD = struct
  module type TYP = F.TYP;;
end
;;

