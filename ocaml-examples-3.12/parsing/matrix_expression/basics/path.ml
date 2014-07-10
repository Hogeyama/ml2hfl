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

(* $Id: path.ml,v 1.1 2011-08-08 19:09:58 weis Exp $ *)

(** {3 The path machinery} *)

(** We define a current list of directories to look up for finding files. *)

exception Empty;;

type file_name = string
and dir_name = string
and explicit_file_name = string
;;

type t = dir_name list;;
(** A directory path is a list of directory names. *)

let push, pop, init, get =

  let path = ref [] in

  (fun p -> path := p :: !path),
  (fun () ->
   match !path with
   | [] -> raise Empty
   | _ :: rest -> path := rest),
  (fun l -> path := l),
  (fun () -> !path)
;;

let kfind_in_path f path s =
 try f s with
 | Sys_error _ as x ->

   let rec do_in = function
   | [] -> raise x
   | p :: pts ->
     let fname = Filename.concat p s in
     (* In case of debugging:
     Printf.eprintf "find_path %s\n" fname; *)
     try f fname with
     | Sys_error _ -> do_in pts in
   do_in path
(** val kfind_in_path : (explicit_file_name -> 'a) -> t -> file_name -> 'a;;
  [kfind_in_path f path s] applies [f] on any file named [s] in
   the path, and returns the result of the first call that does not
   fail by raising the exception [Sys_error]. For instance,
   [kfind_in_path open_in (Path.get ()) "foo"] searches the current path for a file
   ["foo"] and returns an input channel to that file. *)
;;

let kfind f fname =
  if Filename.is_implicit fname
  then kfind_in_path f (get ()) fname
  else f fname
;;

let open_in = kfind Pervasives.open_in;;
let open_in_bin = kfind Pervasives.open_in_bin;;
let open_out = kfind Pervasives.open_out;;
let open_out_bin = kfind Pervasives.open_out_bin;;

let find = kfind (fun s -> s);;

let exists fname =
  try kfind
        (fun s -> Sys.file_exists s || raise (Sys_error s)) fname with
  | Sys_error _ -> false
;;

let close_in ic =
  if ic != stdin then Pervasives.close_in ic
;;
let close_out ic =
  if ic != stdout then Pervasives.close_out ic
;;

let print ppf path =
  let rec print_path_elems ppf = function
  | [] -> ()
  | dir :: dirs -> Format.fprintf ppf "%S;" dir; print_path_elems ppf dirs in
  Format.fprintf ppf "@[[ %a ]@]" print_path_elems path
;;
