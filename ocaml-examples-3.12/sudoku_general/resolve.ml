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

(* $Id: resolve.ml,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

(** {3 The bactracking procedure to solve the Sudoku problem} *)

open Types;;

let choose_place board =
  let width = board.in_width in
  let height = board.in_height in
  let iboard = board.in_board in
  (* Find the location with the minimum branching factor:
     find the first place in the board that figures the smallest number of
     digit candidates, then return its coordinates and the list of its still
     available digit candidates. *)
  let rec loop (lenb, place as accu) i j =
    if j >= width then loop accu (i + 1) 0 else
    if i >= height then place else
    match iboard.(i).(j) with
      (* No more available digit here: the board cannot be solved;
         so escape to backtracking. *)
    | [] -> Backtrack
      (* Only one digit: this place is already solved,
         so skip it and go on. *)
    | [d] -> loop accu i (j + 1)
      (* Two digits is the minimal branching factor anyway,
         so this is the result. *)
    | [d1; d2] -> Place (d1, i, j)
      (* Neither one nor two digits, this place may be the result,
         so test the length of the still available choices here
         w.r.t. the current minimal branching factor to check if this place
         is the choice point with minimal branching, and go on
         accordingly. *)
    | d1 :: dl ->
      let lenl = 1 + List.length dl in
      if lenl < lenb
      then loop (lenl, Place (d1, i, j)) i (j + 1)
      else loop accu i (j + 1) in
  (* The starting guess is wild: ``no known place at all in the board'';
     this is indeed the worst guess (hence it has the maximum
     number of possible choices). *)
  loop (max_int, No_place board) 0 0
;;

let copy_board board =
  let height = board.in_height in
  let width = board.in_width in
  let iboard = board.in_board in
  let in_board =
    Array.init height
      (fun i -> Array.init width (fun j -> iboard.(i).(j))) in
  { board with in_board = in_board; }
;;

(**
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
*)

let rec resolve_all nr sols boards board =
  Solve.deduce_board board;
  (* Try to choose one possible assignment of a digit
     to a still unknown place. *)
  let chosen_place = choose_place board in
  match chosen_place with
  | Types.Backtrack ->
    Utils.debug_message "Wrong guess: backtracking";
    begin match boards with
    | [] -> sols
    | brd :: boards ->
      resolve_all (nr + 1) sols boards brd
    end
  | No_place board ->
    Utils.debug "Found a solution" board;
    begin match boards with
    | [] -> board :: sols
    | brd :: boards ->
      Utils.debug_message "Try to find other solutions";
      resolve_all (nr + 1) (board :: sols) boards brd end
  | Place (d, i, j) ->
    (* Copy the board to the list of backtrack points
       (carefully omitting the digit from the list of digits
        of the corresponding position). *)
    let board_copy = copy_board board in
    let b = Solve.remove_digit d board_copy i j in
    if not b then assert false;

    (* Assign the chosen digit to the current [board] and go on. *)
    Utils.debug_message (
      Printf.sprintf
        "Trying digit %d at position (%i, %i)"
        (Utils.int_of_digit d) i j);
    board.in_board.(i).(j) <- [d];

    resolve_all (nr + 1) sols (board_copy :: boards) board
;;

let resolve_board_all board = resolve_all 0 [] [] board
;;

let resolve_board board =
  match resolve_board_all board with
  | [] -> No_solution
  | [sol] -> Unique_solution sol
  | sol :: sols -> Multiple_solutions (sol, sols)
;;
