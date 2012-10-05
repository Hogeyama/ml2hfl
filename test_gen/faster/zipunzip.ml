
let rec unzip (xys:(int*int) list) =
  match xys with
      [] -> [], []
    | (x,y)::xys' ->
        let xs,ys = unzip xys' in
          x::xs, y::ys

let rec zip (xs:int list) (ys:int list) =
  match xs with
      [] ->
        begin
          match ys with
              [] -> []
            | _ -> assert false
        end
    | x::xs' ->
        match ys with
            [] -> assert false
          | y::ys' -> (x,y)::zip xs' ys'

let rec make_list n =
  if n < 0
  then []
  else (n,n) :: make_list (n-1)

let main n =
  let xs = make_list n in
  let xs1,xs2 = unzip xs in
    zip xs1 xs2


