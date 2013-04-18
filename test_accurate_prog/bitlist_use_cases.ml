let rec eq xs ys =
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
      | y::ys' -> x = y && eq xs' ys'

let rec (mod) x y =
  if x < y
  then x
  else (x - y) mod y

let rec (/) x y =
  if x < y
  then 0
  else 1 + (x - y) / y

let rec encode n =
  if n <= 0
  then []
  else (n mod 2 = 1) :: encode (n / 2)

let main () =
  assert (eq (encode 2) [false;true]);
  assert (eq (encode 3) [true;true]);
  assert (eq (encode 8) [true;false;false])
