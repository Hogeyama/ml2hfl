let rec count n xs =
  match xs with
    [] -> 0
  | x::xs' -> if x = n then 1 + count n xs' else count n xs'

let rec main n xs = assert (count n xs + 1 = count n (n :: xs))
