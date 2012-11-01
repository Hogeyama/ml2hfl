let hd xs = xs 0
let tl xs i = xs (i + 1)
let nil x = assert false
let cons x xs i = if i = 0 then x else xs (i - 1)

(** len:int ->
    i:{0 <= i < len} -> j:{i <= j < len} -> (y:{0 <= y < len - i} -> r:int{r = y + i}) ->
    (x:int -> b:{b iff x = j}) ->
    true *)
let rec exists len xs cond =
  if len = 0 then
    false
  else
    cond (hd xs) || exists (len - 1) (tl xs) cond

let rec make_list i n =
  if i >= n then
    nil
  else
    cons i (make_list (i + 1) n)

let main i (n:int) =
  if 0 <= i && i < n then
    assert (exists n (make_list 0 n) (fun x -> x = i))
