let is_nil (xs:int list) =
  match xs with
      [] -> true
    | _ -> false

let rec make_list n =
  if n = 0
  then []
  else n :: make_list (n-1)

let rec rev (xs:int list) (acc:int list) =
  match xs with
      [] -> acc
    | x::xs' -> rev xs' (x::acc)
let reverse xs = rev xs []

let main n =
  let xs = make_list n in
    assert (is_nil xs || not (is_nil (reverse xs)))
