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

(* $Id: module_aggregate.ml,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

module Make (AInfo : Module_types.INFO) = struct

  module Info = AInfo;;

  module Aggregate = struct

      type site = {
        path : Path.path;
        info : Info.info;
      }
      ;;

      let free_site path = { path = path; info = Info.get path; }
      ;;

      let print_site ppf site =
         Format.fprintf ppf "{ path = %a; info = %a; }"
            Path.print site.path Info.print site.info
      ;;

      let add i = function
        | [] -> [i]
        | n :: _ as l -> (n + i) :: l
      ;;

      let link site1 site2 =
        let p1 = add 1 site1.path in
        let p2 =  add 2 site1.path in
        free_site (p1 @ p2)
      ;;

  end
  ;;

end
;;
