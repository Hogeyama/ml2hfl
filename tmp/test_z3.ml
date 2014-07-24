let rec loop () = loop ()
let rand_int() = Random.int 0


let rec append_1061 (xs__ys:(((bool * int) * (bool * int)) -> (( (bool * int)) * ((bool * int))))) i =
  let (b,_),_ = xs__ys ((true, 0), (false, 0)) in
  if b = false then
    let _,r = xs__ys ((false, 0), (true, i)) in
    (r, (false,0))
  else
    let xs'__ys _ = (true, 0), (true, 0) in
    append_1061 xs'__ys i


let main_1017 (i_1018:int) (n_1019:int) =
  let f ((b,x),_) =
    if b then
      ((not b, 0),
       (true, 0))
    else
      ((not b, 0),
       (true, 0))
  in
  let (_,x),(_,y) = append_1061 f 0 in
  assert (x = y)
