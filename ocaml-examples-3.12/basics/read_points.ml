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

(* $Id: read_points.ml,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

(** Reading data from text files.

   We suppose having some data record in a text file, we read them in
   and fill the list of records contained in the file.

   Afterwards, we compute a simple value from this list of points:
   the maximum square distance to the origin of the points in the list. *)

type point = {
  x : float;
  y : float;
  z : float;
}
(** The data we want to read in the input file. *)
;;

(** The source file has the following format:
    - a point is defined by three consecutive lines,
    - each line just contains one coordinate of the point,
    - points are just written in sequence with no marker or delimiter between
      points. *)

(**
 - Input_line reads a line in an input channel, i.e.some characters followed
   by an end of line mark (a newline character in Unix or Mac OS X, a newline
   followed by a return in Windows).
 - [input_line] returns a string of characters with the end of line marker
   omitted. *)

let read_float ic =
  let line = input_line ic in
  float_of_string line
;;

let read_point ic =
  let x = read_float ic in
  try
    let y = read_float ic in
    let z = read_float ic in
    { x = x; y = y; z = z; }
  with
  | End_of_file ->
    failwith "read_point: number of lines is not a multiple of 3."
;;

let read_points_ic ic =
  let rec loop read_pts =
    try
      let pt = read_point ic in
      loop (pt :: read_pts) with
    | End_of_file -> read_pts in
  List.rev (loop [])
(** We read the points contained in the input channel [ic].
    We read points until the end of the input channel, i.e. until
    an [End_of_file] exception is raised when reading.

    The inner recursive function [loop] accumulate the lines read so far into
    the argument [read_pts]; those points are accumulated while reading the
    file from the beginning to the end; hence, the accumulated list is in
    reverse order and the final result is the reverse of the accumulated list
    of points. *)
;;

let read_points fname =
  let ic = open_in fname in
  let pts = read_points_ic ic in
  close_in ic;
  pts
(** To read the points in file [fname], we just call [read_points_ic] with the
  appropriate input channel. *)
;;

let norm2 { x = x; y = y; z = z; } =
  x *. x +. y *. y +. z *. z
(** We compute the square norm of a vector. *)
;;

let max_distance points =
  let rec max m = function
    | [] -> m
    | pt :: pts ->
      let d = norm2 pt in
      if d >= m then max d pts else max m pts in
  sqrt (max 0. points)
(** We compute the maximum squared distance to the origin of the point list
  given. *)
;;

let main () =
  if Array.length Sys.argv <> 2 then begin
    Printf.eprintf "Usage: read_points <filename>\n";
    exit 1
  end else begin
    let fname = Sys.argv.(1) in
    try
      let points = read_points fname in
      let d = max_distance points in
      Printf.printf
        "Maximum distance to origin of points read is %f.\n" d;
      exit 0;
    with
    | Failure s ->
      Printf.eprintf "read_points: unrecoverable failure %s.\n" s;
      exit 2
  end
;;

main ()
;;
