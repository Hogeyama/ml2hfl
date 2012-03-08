let hd xs = xs 0
let tl xs i = xs (i + 1)
let nil x = assert false
let cons x xs i = if i = 0 then x else xs (i - 1)

let rec reverse len1 xs1 len2 xs2 =
  if len2 = 0 then
    xs1
  else
    reverse (len1 + 1) (cons (hd xs2) xs1) (len2 - 1) (tl xs2)

let main len =
  if len > 0 then
    let _ = hd (reverse 0 nil len (fun x -> 0)) in ()
