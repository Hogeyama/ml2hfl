
let rec append xs__ys =
  let b,_ = xs__ys true in
  if b then
    append (fun _ -> true, 0)
  else
    let _,r2 = xs__ys false in
    assert (r2 = 0)

let mynot b = if b then false else true

let main_1017 () = append (fun b -> mynot b, 0)
