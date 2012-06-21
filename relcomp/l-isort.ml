let fst (x, y) = x
let rec loop (u:unit) = loop u
let hd (len, l) = l 0
let tl (len, l) = (len - 1, fun i -> l (i + 1))
let nil = (0, fun i -> loop ())
let cons a (len, l) = (len + 1, fun i -> if i = 0 then a else l (i - 1))

let rec insert (x:int) ys =
  if fst ys = 0 then
    cons x nil
  else
    if x <= hd ys then
      cons x ys
    else
      cons (hd ys) (insert x (tl ys))

let rec isort xs =
  if fst xs = 0 then
    nil
  else
    insert (hd xs) (isort (tl xs))

let rec make_list n =
  if n = 0 then
    nil
  else
    cons n (make_list (n - 1))

let rec check xs =
  if fst xs <= 1 then
    true
  else
    hd xs <= hd (tl xs) && check (tl xs)

let main n = assert (check (isort (make_list n)))
