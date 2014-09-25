let rec insert ((x:int), ys) =
  match ys with
  | [] -> [x]
  | y::ys' ->
      if x < y then x::ys
      else y::(insert (x, ys'))

(*{SPEC}

specification insert: int * sorted_list -> sorted_list

{SPEC}*)
