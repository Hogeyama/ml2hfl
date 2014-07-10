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

(* $Id: solve.ml,v 1.2 2011-08-08 19:31:17 weis Exp $ *)

open Types;;

let rec remove_from_list d = function
  | [] -> []
  | d' :: l ->
    if d = d' then l else d' :: remove_from_list d l
;;

let remove_digit d board i j =
  let iboard = board.in_board in
  let l = iboard.(i).(j) in
  let rl = remove_from_list d l in
(*  if rl = [] then raise Types.Backtrack else*)
  rl <> l && (iboard.(i).(j) <- rl; true)
;;

let suppress_digit_in_square d id jd (board, i0, j0) =
(*prerr_endline (Printf.sprintf "suppress_digit: %i %i" i0 j0);*)
  let go_on = ref false in
  for i = i0 to i0 + 2 do
    for j = j0 to j0 + 2 do
      (* Don't ask for this very place: we have not to remove it anyway! *)
      if i = id && j = jd then () else
      go_on := remove_digit d board i j || !go_on
    done
  done;
  !go_on
;;

let sature_square (board, i0, j0) =
(*  Format.printf "sature_square: %i %i@." i0 j0; *)
  let go_on = ref false in
  let iboard = board.in_board in
  for i = i0 to i0 + 2 do
    for j = j0 to j0 + 2 do
      let c = iboard.(i).(j) in
      match c with
      | [d] ->
        go_on := suppress_digit_in_square d i j (board, i0, j0) || !go_on
      | _ -> ()
    done
  done;
  !go_on
;;

let sature_squares board =
(*  Format.printf "sature_squares:@." i0 j0; *)
  let go_on = ref false in
  for i = 0 to 2 do 
    for j = 0 to 2 do
      let square = (board, (3 * i), (3 * j)) in
      go_on := sature_square square || !go_on
    done;
  done;
  !go_on
;;

let suppress_on_column d jd (board, i0) =
  let go_on = ref false in
  for j = 0 to 8 do
    if j = jd then () else
    go_on := remove_digit d board i0 j || !go_on
  done;
  !go_on
;;

let suppress_on_row d id (board, j0) =
  let go_on = ref false in
  for i = 0 to 8 do
    if i = id then () else
    go_on := remove_digit d board i j0 || !go_on
  done;
  !go_on
;;

let rec sature_board board =
  let go_on = ref false in
  let iboard = board.in_board in
  for i = 0 to 8 do
    for j = 0 to 8 do
(*prerr_endline (Printf.sprintf "sature_board %i %i" i j);*)
      match iboard.(i).(j) with
      | [d] ->
(*prerr_endline (Printf.sprintf "suppress_on_line %i" i);*)
        go_on := suppress_on_column d j (board, i) || !go_on;
(*prerr_endline (Printf.sprintf "suppress_on_row %i" j);*)
        go_on := suppress_on_row d i (board, j) || !go_on
      | _ -> ()
    done;
  done;
  go_on := sature_squares board || !go_on;
  if !go_on then sature_board board
;;

let deduce board with_lists =
  let rec except (i0, j0) = function
    | [] -> []
    | (l, i, j as list) :: lists ->
      if i = i0 && j = j0 then lists else
      list :: except (i0, j0) lists in

  let iboard = board.in_board in

  let digit_not_in_other_lists i j d =
    List.for_all
      (fun (l, _, _) -> not (List.mem d l))
      (except (i, j) with_lists) &&
    (iboard.(i).(j) <- [d];
     true) in

  (** Find if we can reduce a list of possible digits at place i, j. *)
  let reducible (l, i, j) =
    List.exists (fun d -> digit_not_in_other_lists i j d) l in

  List.exists reducible with_lists
;;

let deduce_square board i0 j0 =
  let iboard = board.in_board in
  let with_lists =
    let rec loop accu i j =
      if j > j0 + 2 then loop accu (i + 1) j0 else
      if i > i0 + 2 then accu else
      match iboard.(i).(j) with
      | [d] -> loop accu i (j + 1)
      | l -> loop ((l, i, j) :: accu) i (j + 1) in
    loop [] i0 j0 in

  deduce board with_lists
;;

let deduce_row board i0 =
  let iboard = board.in_board in
  let with_lists =
    let rec loop accu j =
      if j > 8 then accu else
      match iboard.(i0).(j) with
      | [d] -> loop accu (j + 1)
      | l -> loop ((l, i0, j) :: accu) (j + 1) in
    loop [] 0 in

  deduce board with_lists
;;

let deduce_column board j0 =
  let iboard = board.in_board in
  let with_lists =
    let rec loop accu i =
      if i > 8 then accu else
      match iboard.(i).(j0) with
      | [d] -> loop accu (i + 1)
      | l -> loop ((l, i, j0) :: accu) (i + 1) in
    loop [] 0 in

  deduce board with_lists
;;

let all_squares = [
  (0, 0); (0, 3); (0, 6);
  (3, 0); (3, 3); (3, 6);
  (6, 0); (6, 3); (6, 6);
];;

let all_rows = [
  0; 1; 2; 3; 4; 5; 6; 7; 8;
]
;;

let all_columns = all_rows
;;

let deduce_squares board =
  List.exists (fun (i, j) -> deduce_square board i j) all_squares
;;

let deduce_rows board =
  List.exists (fun i -> deduce_row board i) all_rows
;;

let deduce_columns board =
  List.exists (fun j -> deduce_column board j) all_columns
;;

let rec deduce_board board =
  sature_board board;
  (* For each square *)
  let found_something = deduce_squares board in
  if found_something then deduce_board board else
  (* For each row *)
  let found_something = deduce_rows board in
  if found_something then deduce_board board else
  (* For each column *)
  let found_something = deduce_columns board in
  if found_something then deduce_board board
;;
