open Util
open Combinator

(** Permutations *)

let permutations n =
  let rec aux = function
    | [] -> []
    | xs ->
      List.init
        (List.length xs)
        (fun i ->
           let xs1, x :: xs2 = List.split_nth i xs in
           xs1 @ xs2 |> aux |> List.map (List.cons x))
      |> List.concat
  in
  aux (List.init n id)

let maps n1 n2 =
  let xs = List.init n1 id in
  permutations n2
  |> List.map (List.combine xs)

let rec perm xs n =
  if n <= 0 then [[]]
  else
    xs
    |> List.mapc (fun f x -> List.map (List.cons x) (perm (f []) (n - 1)))
    |> List.concat
