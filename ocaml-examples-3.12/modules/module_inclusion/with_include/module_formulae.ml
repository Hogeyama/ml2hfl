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

(* $Id: module_formulae.ml,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

module Make (AInfo : Module_types.INFO) = struct

  include Module_lib_frames.Make (AInfo);;

  module Formulae = struct

    open Aggregate;;

    type formula =
       | Base of Path.path
       | Bond of formula * formula
    ;;

    let interp f path =

      let rec interp f path =
        match f with
        | Base p ->
          Backbone_link.make (free_site p) (0 :: path)
        | Bond (f1, f2) ->
          let lnk1 = interp f1 (1 :: path) in
          let lnk2 = interp f1 (2 :: path) in
          Backbone_link.make
            (Aggregate.link lnk1.Backbone_link.n lnk2.Backbone_link.n)
            (3 :: path) in
       (interp f path).Backbone_link.n
    ;;

  end
  ;;

end
;;
