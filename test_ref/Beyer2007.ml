(*
assume (n >= 0);
i = 0;
a = 0;
b = 0;
while (i < n ) {
  if * {
    a = a + 1;
    b = b + 2;
  } else {
    a = a + 2;
    b = b + 1;
  }
  i = i + 1;
}
assert (a+b = 3*n)
*)

let ref n (loc,store) = loc, (loc+1, fun i -> if loc = i then n else store i)
let read i (_,store) = store i
let update i n (loc,store) = (loc, fun j -> if i = j then n else store j)

let rec loop () = loop ()
let init_world = (0, fun _ -> loop ())
let f n world =
  let i,world = ref 0 world in
  let a,world = ref 0 world in
  let b,world = ref 0 world in
  let rec while_ world =
    if read i world >= n
    then world
    else
      let world =
        if Random.bool ()
        then
          let world = update a (read a world + 1) world in
          let world = update b (read b world + 2) world in
            world
        else
          let world = update a (read a world + 2) world in
          let world = update b (read b world + 1) world in
            world
      in
      let world = update i (read i world + 1) world in
        while_ world
  in
  let world = while_ world in
    assert (read a world + read b world = 3 * n)

let main n = if n >= 0 then f n init_world
