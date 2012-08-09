let rec rev n m =
  if n = 0 then m else rev (n - 1) (m + 1)
let main n k = assert (rev n k >= n+k)
(*assert (rev n 0 >= n)@‚É‚·‚é‚Æ‚¾‚ß*)