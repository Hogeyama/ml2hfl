
let rec make_vector n m =
  if n < 0
  then []
  else
    let x = if n = m then 1 else 0 in
      x :: make_vector (n-1) m

let rec make_matrix n m =
  if n < 0
  then []
  else make_vector n m :: make_matrix (n-1) m

let rec nth_vec n (xs:int list) =
  match xs with
    | [] -> let rec loop (u:unit) : int = loop u in loop ()
    | x::xs' -> if n = 0 then x else nth_vec (n-1) xs'

let rec nth_mat n (xs:int list list) =
  match xs with
    | [] -> let rec loop (u:unit) : int list = loop u in loop ()
    | x::xs' -> if n = 0 then x else nth_mat (n-1) xs'

let main n m i j =
  let mat = make_matrix n m in
  let x = nth_vec j (nth_mat i mat) in
    assert (i = j || x = 0)




