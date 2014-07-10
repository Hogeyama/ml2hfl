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

(* $Id: module_chemical_group.ml,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

module Make (AInfo : Module_types.INFO) = struct

  include Module_aggregate.Make (AInfo);;

  module Chemical_group = struct

    let double_site site = Aggregate.link site site
    ;;

  end
  ;;

end
;;
