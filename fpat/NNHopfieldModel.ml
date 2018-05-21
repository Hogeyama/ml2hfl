open Util
open Combinator

(** Hopfield model of neural network *)

(* solve N-Queen problem *)
let queens number_of_queens =
  let rec queens_sub n =
    if n == 0 then [[]]
    else
      let safe q n =
        let m = List.length q + 1 in
        let check n (i, j) = j <> n && i + j <> m + n && i - j <> m - n in
        List.zip (List.from_to 1 (List.length q)) q
        |> List.for_all (check n)
      in
      let f y =
        List.from_to 1 number_of_queens
        |> List.filter (fun x -> safe y x)
        |> List.map (fun x -> y @ [x])
      in
      List.concat (List.map f (queens_sub (n - 1)))
  in
  queens_sub number_of_queens

(* solve N-Queen problem by using neural network *)
let queensnn number_of_queens max =
  let check xss =
    List.from_to 0 (number_of_queens - 1)
    |> List.map
         (fun i ->
          List.from_to 0 (number_of_queens - 1)
          |> List.map
               (fun j ->
                Integer.sum_list (Matrix.row i xss)
                + Integer.sum_list (Matrix.column j xss) == 2
                && Integer.sum_list (Matrix.diag_rd (i, j) xss) < 2
                && Integer.sum_list (Matrix.diag_ld (i, j) xss) < 2))
    |> List.concat
    |> List.for_all id
  in
  let umax = 6 in
  let rec next t i j xss yss =
    if i >= number_of_queens then
      if check yss then
        yss
      else if t >= max then
        []
      else
        next (t + 1) 0 0 xss yss
    else if j >= number_of_queens then
      next t (i + 1) 0 xss yss
    else
      let a = 1 in
      let b = 1 in
      let c = if t mod 20 < 4 then 4 else 1 in
      let sr = Integer.sum_list (Matrix.row i yss) in
      let sc = Integer.sum_list (Matrix.column j yss) in
      let yij = Matrix.elem i j yss in
      let dr = Integer.sum_list (Matrix.diag_rd (i, j) yss) - yij in
      let dl = Integer.sum_list (Matrix.diag_ld (i, j) yss) - yij in
      let h x = if x == 0 then 1 else 0 in
      let dw = -a * (sr + sc - 2) - b * (dr + dl) + c * (h sr + h sc) in
      let xm = Matrix.elem i j xss + dw in
      let xssm =
        if xm < -umax then
          Matrix.replace i j (-umax) xss
        else if xm > umax then
          Matrix.replace i j umax xss
        else
          Matrix.replace i j xm xss
      in
      let yssm =
        let ltp = -2 in
        let utp = 1 in
        if xm > utp then
          Matrix.replace i j 1 yss
        else if xm < ltp then
          Matrix.replace i j 0 yss
        else
          yss
      in
      next t i (j + 1) xssm yssm
  in
  let f x = if x > 0 then 1 else 0 in
  let initial_list =
    let rand_list seed = fun n ->
      List.map
        (fun i ->
         List.from_to (seed * i + 1) (seed * i + n)
         |> List.map Integer.rand
         |> List.map (fun s -> s mod (2 * umax + 1) - umax))
        (List.from_to 1 n)
    in
    rand_list 13 number_of_queens
  in
  (* List.map (fun x -> 1 + (List.length (List.takewhile (fun y -> y == 0) x))) *)
  next 1 0 0 initial_list (List.map (List.map f) initial_list)

let test () =
  Format.printf "@[";
  let size = (*if Array.length Sys.argv >= 2 then int_of_string Sys.argv.(1) else*) 8 in
  let res = queensnn size 10000 in
  if res == [] then
    Format.printf "failed to solve.@,"
  else
    begin
      Format.printf "successfully solved!!@,";
      for i = 0 to size - 1 do
        for j = 0 to size - 1 do
          if Matrix.elem j i res == 1 then
            Format.printf "Q "
          else
            Format.printf ". "
        done;
        Format.printf "@,"
      done;
    end;
  Format.printf "@]"
