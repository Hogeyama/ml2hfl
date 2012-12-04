let rec count n xs =
  match xs with
    [] -> 0
  | x::xs' -> if x = n then 1 + count n xs' else count n xs'

let main () = assert (count 7 [] = 0 && count 7 [7] = 1 && count 7 [1;7] = 1 && count 7 [7;1;7] = 2)
