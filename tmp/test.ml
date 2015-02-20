
let rec append xs__ys =
  let b,_ = xs__ys true in
  if b then
    append (fun _ -> true, 0)
  else
    let _,r2 = xs__ys false in
    assert (r2 = 0)

let mynot b = if b then false else true

<<<<<<< HEAD
let main_1017 () = append (fun b -> mynot b, 0)
=======

(*{SPEC}
val f : (x:int[x > 0]) -> r:int
{SPEC}*)

let rec f n =
  let r = read_int () in
  if n = 0
  then r
  else f (n-1)
let main n = f n
>>>>>>> 3e6c8cf... fix to distinguish two versions of rand_int
