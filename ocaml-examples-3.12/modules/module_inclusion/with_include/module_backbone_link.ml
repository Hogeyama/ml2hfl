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

(* $Id: module_backbone_link.ml,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

module Make (AInfo : Module_types.INFO) = struct

  include Module_chemical_group.Make (AInfo);;

  module Backbone_link = struct

    type backbone_link = {
      n : Aggregate.site;
      p : Path.path;
    }
    ;;

    let make site path = {
      n = Chemical_group.double_site site;
      p = path;
     }
    ;;

  end
  ;;

end
;;
