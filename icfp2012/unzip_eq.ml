(* unzip_eq.ml: verification failed *)
let rec unzip (xys:(int*int) list) =
  match xys with
      [] -> [], []
    | (x,y)::xys' ->
        let xs,ys = unzip xys' in
          x::xs, y::ys

let rec eq_list (xs:int list) (ys:int list) =
  match xs with
      [] ->
        begin
          match ys with
              [] -> true
            | _ -> false
        end
    | x::xs' ->
        match ys with
            [] -> false
          | y::ys' -> x = y && eq_list xs' ys'

let rec make_list n =
  if n < 0
  then []
  else (n,n) :: make_list (n-1)

let main n =
  let xs = make_list n in
  let xs1,xs2 = unzip xs in
    assert (eq_list xs1 xs2)
