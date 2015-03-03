let rec app (f: int -> int -> unit) x i =
  if i>0 then app f x (i-1) else f x i
let id (u:int) = ()
let rec g x = if x>0 then id else app g (x-1)
let main (u:unit) =
  let t = Random.int 0 in
  g t t
