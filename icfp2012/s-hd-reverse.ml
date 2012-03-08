(** need extra parameter but progress infinitely *)

let hd xs = xs 0
let tl xs i = xs (i + 1)
let nil i = 0
let cons x xs i = if i = 0 then x else xs (i - 1)

let rec reverse xs1 xs2 =
  if hd xs2 = 0 then
    xs1
  else
    reverse (cons (hd xs2) xs1) (tl xs2)

let rec init_string x len =
  if len = 0 then
    fun i -> 0
  else
    cons x (init_string x (len - 1))

let main len =
  if len > 0 then
    assert (hd (reverse nil (init_string 1 len)) <> 0)
