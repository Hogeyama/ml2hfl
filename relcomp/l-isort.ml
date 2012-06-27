let nil = (0, fun i -> assert false)
let cons a (len, l) =
  (len + 1, fun i -> if i = 0 then a else l (i - 1))
let hd (len, l) = l 0
let tl (len, l) = (len - 1, fun i -> l (i + 1))
let is_nil (len, l) = len = 0

let rec insert (x:int) ys =
  if is_nil ys then
    cons x nil
  else
    if x <= hd ys then
      cons x ys
    else
      cons (hd ys) (insert x (tl ys))

let rec isort xs =
  if is_nil xs then
    nil
  else
    insert (hd xs) (isort (tl xs))

let rec make_list n =
  if n = 0 then
    nil
  else
    cons n (make_list (n - 1))

let rec check xs =
  if is_nil xs then
    true
  else if is_nil (tl xs) then
    true
  else
    hd xs <= hd (tl xs) && check (tl xs)

let main len = assert (check (isort (len, fun i -> len - i)))
