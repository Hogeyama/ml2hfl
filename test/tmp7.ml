array_max : int -> int -> (int[v.v=0] -> unit) -> unit;

let rec array_max n i =
  if i >= n
  then 0
  else
    let x = i in
    let y = array_max n (i+1) in
      if x > y then x else y
in
  (*if 0 <= i && i < n then*) if 0 < n then assert (n - 1 <= array_max n 0) else ()
  (*else ()*)

