let ref n (loc,store) = loc, (loc+1, fun i -> if loc = i then n else store i)
let read i (_,store) = store i
let update i n (loc,store) = (loc, fun j -> if i = j then n else store j)

let rec loop () = loop ()
let init_world = (0, fun _ -> loop ())
let sum n world =
  let s,world = ref 0 world in
  let rec for_ i world =
    if i > n
    then world
    else
      let world = update s ((read s world) + i) world in
        for_ (i+1) world
  in
  let world = for_ 0 world in
    read s world

let main n = assert (sum n init_world >= 0)
