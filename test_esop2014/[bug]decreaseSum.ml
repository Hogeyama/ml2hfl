let return (n:int) (u:unit) = n
let pred (f:unit -> int) (u:unit) = f () - 1
let rec f (n:unit->int) = if n () = 0 then f (pred n) else ()
let main (n:int) = f (return n)
