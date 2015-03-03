(*
http://www7.in.tum.de/~ruslan/binreach/sereni81.ml
*)
let rec foldr (h : int -> (int -> int) -> int -> int) (e : int -> int) l =
  if l = 0 then e
           else h (Random.int 0) (foldr h e (l-1))

let step (f : int -> int -> int) (a : int) (g : int -> int) (x : int) = g (f x a)

let id (z:int) = z

let foldl (f : int -> int -> int) (b : int) (zs : int) = (foldr (step f) id zs) b

let sum m n = m + n

let main (u:unit) =
  let l = Random.int 0 in
  if l >= 0 then
    foldl sum (Random.int 0) l
  else 0
