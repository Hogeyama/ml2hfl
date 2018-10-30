
type bool_list =
  | Nil
  | Cons of bool * bool_list

let rec f = function
  | Nil -> ()
  | Cons _ -> assert false

let main l =
  if (match l with
      | Nil -> true
      | Cons _ -> false)
  then f l
  else ()

