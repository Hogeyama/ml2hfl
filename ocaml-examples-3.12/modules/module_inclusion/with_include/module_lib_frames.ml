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

(* $Id: module_lib_frames.ml,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

module Make (AInfo : Module_types.INFO) = struct

  include Module_backbone_link.Make (AInfo);;

  let help = function
    | "version" -> "0.0"
    | s -> failwith (Printf.sprintf "help: no help for %s" s)
  ;;

end
;;
