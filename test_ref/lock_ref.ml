let ref n (loc,store) = loc, (loc+1, fun i -> if loc = i then n else store i)
let read i (_,store) = store i
let update i n (loc,store) = (loc, fun j -> if i = j then n else store j)

let rec loop () = loop ()
let init_world = (0, fun _ -> loop ())

let main_aux n world =
  let _LOCK,world = ref 0 world in

  let lock world =
    assert (read _LOCK world = 0);
    update _LOCK 1 world
  in
  let unlock world =
    assert (read _LOCK world = 1);
    update _LOCK 0 world
  in

  let f n world =
    if n > 0
    then lock world
    else world
  in
  let g n world =
    if n > 0
    then unlock world
    else world
  in
  let world = f n world in
  let world = g n world in
    assert (read _LOCK world = 0)

let main n = main_aux n init_world
