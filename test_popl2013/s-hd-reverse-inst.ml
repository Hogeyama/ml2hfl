(** need extra parameter? typable? *)

let hd len s = s 0
let tl len s i = s (i + 1)
let make n i = if i < n then true else false
let nil i = false
let cons x len s i = if i = 0 then x else s (i - 1)

(*
let rec init_string x len =
  if len = 0 then
    nil
  else
    cons x (init_string x (len - 1))
*)

let rec reverse len1 xs1 len2 xs2 =
  if not (hd len2 xs2) then
    xs1
  else
    reverse (len1 + 1) (cons (hd len2 xs2) len1 xs1) (len2 - 1) (tl len2 xs2)

let main len =
  if len > 0 then
    assert (hd len (reverse 0 nil len (make len(*init_string 1 len*))))
