let hd xs = xs 0
let tl a xs i = xs (i + 1)

(** len:{len > 0} ->
    i:{0 <= i} -> j:{i <= j && j - i < len} -> (x:int -> r:int{r = x + i}) ->
    (y:{y = j} -> b:{b = true} /\ y:int -> b:bool) ->
    true *)
let rec exists len i j xs cond =
  if len = 0 then
    false
  else
    (* i = j or i <> j (cf. D-Or) *)
    if i = j then
      cond (hd xs) || (let _ = assert false in false)
    else
      cond (hd xs) || exists (len - 1) (i + 1) j (tl i xs) cond

let main len x =
  if 0 <= x && x < len then
    assert (exists len 0 x (fun i -> i) (fun y -> y = x))
