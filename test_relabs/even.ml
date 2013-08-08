(*
val to_even: {f:int->int | forall x. let r = f x in r = f r}
let rec to_even x =
  match x with
  if x = 0 || x = 1
  then 0
  else 2 + to_even (x-2)

let main n =
  let e = to_even n in
  let e' = to_even e in
    assert (e' = e)
*)
(*
val to_even: int*(x:int)->{(r1:int)*(r2:int) | x = r1 -> let r = r1 in r1 = r2}
let rec to_even x =
  match x with
    None -> None
  | Some x ->
  if x = 0 || x = 1
  then 0
  else 2 + to_even (x-2)
let to_even (x1,x2) = to_even x1, to_even x2
let main n =
  let e,_ = to_even (n, None) in
  let e',e'' = to_even (n, e) in
  if e = e'
  then assert (e' = e'')
  else ()
*)
(*
val to_even: int*(x:int)->{(r1:int)*(r2:int) | x = r1 -> let r = r1 in r = r2}
let rec to_even x =
  match x with
    None -> None
  | Some x ->
  if x = 0 || x = 1
  then Some 0
  else
    let r = to_even (Some (x-2)) in
    match r with
      None -> None
    | Some r -> Some (2 + r)
let rec to_even2 x1 x2 =
  match x1 with
    None -> None, to_even x2
  | Some x1 ->
    match x2 with
      None -> to_even (Some x1), None
    | Some x2 ->
      if x1 = 0 || x1 = 1
      then Some 0, to_even (Some x2)
      else
        if x2 = 0 || x2 = 1
        then to_even (Some x1), Some 0
        else
          let r1,r2 = to_even2 (Some (x1-2)) (Some (x2-2)) in
          let r1' =
          match r1 with
            None -> None
          | Some r1 -> Some (2 + r1)
          in
          let r2' =
          match r2 with
            None -> None
          | Some r2 -> Some (2 + r2)
          in
          r1', r2'
let main n =
  let e,_ = to_even2 (Some n) None in
  let e',e'' = to_even2 (Some n) e in
  if e = e'
  then assert (e' = e'')
  else ()
*)

let none = true, 0
let some x = false, x

let is_none (b,_) = b
let get_val (_,x) = x

let rec to_even x =
  if is_none x
  then none
  else
    let x = get_val x in
  if x = 0 || x = 1
  then some 0
  else
    let r = to_even (some (x-2)) in
    if is_none r then none
    else let r = get_val r in
     some (2 + r)
let rec to_even2 x1 x2 =
  if is_none x1
  then none, to_even x2
  else
    let x1 = get_val x1 in
    if is_none x2
    then to_even (some x1), none
    else
      let x2 = get_val x2 in
      if x1 = 0 || x1 = 1
      then some 0, to_even (some x2)
      else
        if x2 = 0 || x2 = 1
        then to_even (some x1), some 0
        else
          let r1,r2 = to_even2 (some (x1-2)) (some (x2-2)) in
          let r1' =
            if is_none r1
            then none
            else
              let r1 = get_val r1 in
              some (2 + r1)
          in
          let r2' =
            if is_none r2
            then none
            else
              let r2 = get_val r2 in
              some (2 + r2)
          in
          r1', r2'
let main n =
  let e,_ = to_even2 (some n) none in
  let e',e'' = to_even2 (some n) e in
  if snd e = snd e'
  then assert (snd e' = snd e'')
  else ()

(*
let rec to_even x =
  if x = 0 || x = 1
  then 0
  else 2 + to_even (x-2)
let rec to_even2 x1 x2 =
  if x1 = 0 || x1 = 1
  then 0, to_even x2
  else
    if x2 = 0 || x2 = 1
    then to_even x1, 0
    else
      let r1,r2 = to_even2 (x1-2) (x2-2) in
      2 + r1, 2 + r2
let main n =
  let e,_ = to_even2 n n in
  let e',e'' = to_even2 n e in
  if e = e'
  then assert (e' = e'')
  else ()
*)
