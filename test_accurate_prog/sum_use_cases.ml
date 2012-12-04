let rec sum n =
  if n <= 0
  then 0
  else n + sum (n-1)

let main () = assert (sum (-1) = 0 && sum 0 = 0 && sum 1 = 1 && sum 2 = 3 && sum 3 = 6 && sum 4 = 10)
