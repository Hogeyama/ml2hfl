let rec down x = if x = 0 then () else down (x-1)
let rec up x = if x = 0 then () else up (x+1)
let app (f : int -> unit) x = f x
let rec appn n (f : int->unit) = if n<=0 then f else appn (n-1) (app f)
let wrap (f: int->unit) = appn (Random.int(0)) f
let main (u:unit) =
  let t1 = Random.int(0) in let t2 = Random.int(0) in
  let d = (if t1>0 then (wrap down) t1 else ()) in
  if t2<0 then (wrap up) t2 else ()
