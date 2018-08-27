
type bool_list =
  | Nil
  | Cons of bool * bool_list

(*{SPEC}
type f
  : { l : bool_list |
        match l with
        | Nil -> true
        | Cons(b,_) -> b
    }
  -> unit
{SPEC}*)
let f = function
  | Nil -> ()
  | Cons(b,_) -> assert b; ()

