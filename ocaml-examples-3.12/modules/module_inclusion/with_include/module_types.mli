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

(* $Id: module_types.mli,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

(** Something global to the whole construction.
    We globally use everything defined in the module [Path]. *)
open Path;;

(** The basic module we want to abstract in the entire program;
   we will define a function to create the program according to
   the particular instance of this module. *)
module type INFO = sig

    type info;;
    val print : Format.formatter -> info -> unit;;
    val get : path -> info;;

end
;;

module type MODULE_INFO = sig

  module Info : INFO;;

end
;;

(** The type of a module that simply uses the abstracted module.
    More precisely, the type of a module that defines a module
    that simply uses the abstracted module. *)
module type MODULE_AGGREGATE = sig

  include MODULE_INFO;;

  module Aggregate : sig

    type site = {
      path : path;
      info : Info.info;
    }
    ;;

    val free_site : path -> site
    ;;

    val print_site : Format.formatter -> site -> unit
    ;;

    val link : site -> site -> site
    ;;

  end
  ;;

end
;;

(** The type of the functor to build the [Aggregate] module
    given the inner module. *)
module type MODULE_AGGREGATE_MAKER =
  functor (AInfo : INFO) -> MODULE_AGGREGATE with module Info = AInfo
;;

(** The type of a module that uses the preceding module (that uses the
    abstracted module).
    More precisely, this module defines a module that uses a type defined in
    the module that uses the base module. *)
module type MODULE_CHEMICAL_GROUP = sig

  include MODULE_AGGREGATE;;

  module Chemical_group : sig

    val double_site : Aggregate.site -> Aggregate.site
    ;;

  end
  ;;

end
;;

(** The type of the functor to build the [Chemical_group] module
    given the inner module. *)
module type MODULE_CHEMICAL_GROUP_MAKER =
  functor (AInfo : INFO) -> MODULE_CHEMICAL_GROUP with module Info = AInfo
;;

(** The type of a module that defines a new module that uses the preceding
    module to define a new type that is private and is mandatory for other
    modules defined below. *)
module type MODULE_BACKBONE_LINK = sig

  include MODULE_CHEMICAL_GROUP;;

  module Backbone_link : sig

    type backbone_link = private {
      n : Aggregate.site;
      p : path;
    }
    ;;

    val make : Aggregate.site -> path -> backbone_link
    ;;

  end
  ;;

end
;;

(** The type of the functor to build the [Backbone_link] module
    given the inner module. *)
module type MODULE_BACKBONE_LINK_MAKER =
  functor (AInfo : INFO) -> MODULE_BACKBONE_LINK with module Info = AInfo
;;

(** The type of the module that defines the complete library.
    Given that way of defining successive modules enclosing the definition of
    the preceding module (hence the definition of all the preceding modules
    that it depends upon), the entire library is simply defined as the (last)
    richer module. *)
module type MODULE_LIB_FRAMES = sig

  include MODULE_BACKBONE_LINK;;

  val help : string -> string
  ;;

end
;;

(** The type of the functor to build the library given the inner module. *)
module type MODULE_LIB_FRAMES_MAKER =
  functor (AInfo : INFO) -> MODULE_LIB_FRAMES with module Info = AInfo
;;

(** Further extensions of the library should use a functor to be as general as
    the library itself: they should build the library in addition to their
    proper extension module. *)
module type MODULE_FORMULAE = sig

  include MODULE_LIB_FRAMES;;

  module Formulae : sig

    open Aggregate;;

    type formula =
       | Base of path
       | Bond of formula * formula
    ;;

    val interp : formula -> path -> site
    ;;

  end
;;

end
;;

(** The type of a functor to build a library with the [Formulae] module
    extension. *)
module type MODULE_FORMULAE_MAKER =
  functor (AInfo : INFO) -> MODULE_FORMULAE with module Info = AInfo
;;
