(*
lock(){
  if (LOCK == 0){
     LOCK = 1;
  } else {
     ERROR
  }
}

unlock(){
  if (LOCK == 1){
     LOCK = 0;
  } else {
     ERROR
  }
}

Example() {
  if ( * ){
    do {
      got_lock = 0;
      if ( * ){
        lock();
        got_lock++;
      }
      if (got_lock){
        unlock();
      }
    } while ( * )
  }
  do {
    lock();
    old = new;
    if ( * ){
      unlock();
      new++;
    }
  } while (new != old);
  unlock();
  return;
}
*)

let ref n (loc,store) = loc, (loc+1, fun i -> if loc = i then n else store i)
let read i (_,store) = store i
let update i n (loc,store) = (loc, fun j -> if i = j then n else store j)

let incr i world = update i (read i world + 1) world

let rec loop () = loop ()
let world = (0, fun _ -> loop ())
let x = ref 0 world
let _LOCK = fst x
let world = snd x

let lock world =
  if read _LOCK world = 0
  then update _LOCK 1 world
  else assert false

let unlock world =
  if read _LOCK world = 1
  then update _LOCK 0 world
  else assert false

let _Example world =
  let got_lock,world = ref 0 world in
  let old,world = ref 0 world in
  let new_,world = ref 0 world in
  let world =
    if Random.bool ()
    then
      let rec do_while world =
        let world = update got_lock 0 world in
        let world =
          if Random.bool ()
          then
            let world = lock world in
            let world = incr got_lock world in
              world
          else world
        in
        let world =
          if read got_lock world <> 0
          then unlock world
          else world
        in
          if Random.bool ()
          then do_while world
          else world
      in
        do_while world
    else world
  in
  let rec do_while world =
    let world = lock world in
    let world =
      if Random.bool ()
      then
        let world = unlock world in
        let world = incr new_ world in
          world
      else world
    in
      if read new_ world <> read old world
      then do_while world
      else world
  in
    do_while world

let main () = _Example world
