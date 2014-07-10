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

(* $Id: resolve.ml,v 1.5 2011-08-08 19:31:17 weis Exp $ *)

(** {3 The bactracking procedure to solve the Sudoku problem} *)

let worst_choice = Utils.digits
;;

let choose_place board =
  (* Find the first non assigned place with smallest number of choices and
     return it. *)
  let rec loop (lenb, best, ib, jb as accu) i j =
    if j > 8 then loop accu (i + 1) 0 else
    if i > 8 then (best, ib, jb) else
    match board.(i).(j) with
    | [d] -> loop accu i (j + 1)
    | [d1; d2] as l -> (l, i, j)
    | l ->
      let lenl = List.length l in
      if lenl < lenb
      then loop (lenl, l, i, j) i (j + 1)
      else loop accu i (j + 1) in

  let (best, ib, jb) =
    loop (List.length worst_choice, worst_choice, 0, 0) 0 0 in
  if List.length best = 9 then raise (Types.Solution board) else
  (List.hd best, ib, jb)
;;

let copy_board board =
  let v =
  Array.init (Array.length board)
    (fun i -> Array.init (Array.length board.(i))
      (fun j -> board.(i).(j))) in
  assert (v = board);
  v
;;

let rec resolve nr boards board =
  Utils.debug (Printf.sprintf "Resolve %i 0 (before deduce)." nr) board;
  Solve.deduce_board board;
  Utils.debug (Printf.sprintf "Resolve %i 1 (after deduce)." nr) board;
  (* Choose one possible assignment of a digit to a still unknown place. *)
  let (d, i, j) = choose_place board in
  (* Copy the board to the list of backtrack points
     (omitting the digit from the list of the corresponding place). *)
  Utils.debug
    (Printf.sprintf "Resolve %i 2 (after choosing %i, %i, %i)."
       nr (Utils.int_of_digit d) i j)
    board;
  let board_copy = copy_board board in
  let b = Solve.remove_digit d board_copy i j in
  Utils.debug
    (Printf.sprintf "Resolve %i 3 (after choosing %i)."
       nr (Utils.int_of_digit d))
    board_copy;
  if not b then assert false;

  (* Assign it to the current [board] and go on. *)
  board.(i).(j) <- [d];
  Utils.debug
    (Printf.sprintf "Resolve %i 4 (after saturation)." nr)
    board_copy;
  try resolve (nr + 1) (board_copy :: boards) board with
  | Types.Backtrack -> resolve (nr + 1) boards board_copy
;;

let resolve_board board =
 try resolve 0 [] board with
 | Types.Backtrack -> failwith "Cannot solve this sudoku!"
 | Types.Solution board -> board
;;
