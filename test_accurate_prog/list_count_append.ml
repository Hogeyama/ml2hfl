let rec count n xs =
  match xs with
    [] -> 0
  | x::xs' -> if x = n then 1 + count n xs' else count n xs'

let rec append xs ys =
  match xs with
    [] -> ys
  | x::xs' -> x :: append xs' ys

let rec main n xs ys = assert (count n xs + count n ys = count n (append xs ys))
