let rec iter (f:int -> unit) xs =
  match xs with
      [] -> ()
    | x::xs' -> f x; iter f xs'

(*{SPEC}
type iter : ({n:int | n >= 0} -> unit) -> {n:int | n >= 0} list -> unit
{SPEC}*)
