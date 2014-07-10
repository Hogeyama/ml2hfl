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

(* $Id: path.mli,v 1.1 2011-08-08 19:09:58 weis Exp $ *)

(** {3 Defining a path machinery to find files in the file system} *)

exception Empty;;
(** This exception is raised when [pop] is called and the current
   directory path is empty. *)

type file_name = string
and dir_name = string
and explicit_file_name = string;;
(** The name of a file, the name of a directory, and the explicit name of a
    file. *)

type t = dir_name list;;
(** A directory path is a list of directory names. *)

val get : unit -> t;;
val push : dir_name -> unit;;
val pop : unit -> unit;;
val init : t -> unit;;
(** The path machinery:
   define a current list of directories to search files into. *)

val open_in : file_name -> in_channel;;
val open_in_bin : file_name -> in_channel;;
val open_out : file_name -> out_channel;;
val open_out_bin : file_name -> out_channel;;
(** Opening files in the current path. Equivalent to the regular open
   primitives if the file name argument is not implicit.
   Raise [Sys_error] if the file cannot be found or cannot be opened. *)

val close_in : in_channel -> unit;;
val close_out : out_channel -> unit;;

val find : file_name -> explicit_file_name;;
(** Return the explicit file name of the first file with the given name
    in the current directory path.
    Raise [Sys_error] if no such file can be found in the current
    directory path. *)

val exists : file_name -> bool;;
(** Test if a file with the given name exists in the current directory path. *)

val print : Format.formatter -> t -> unit;;
(** Print the given [Path.t] into the [Formatter.formatter]. *)
