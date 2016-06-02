(*
  手動で述語を与えたが、何故か動かない
  HorSatPがおかしい?
*)

(*{SPEC}
  fairness: (A, B)

  valcegar randint_1:
  (x:int[x=1; x<=-1] -> (r:int[r>0] -> X) -> X)

  valcegar f_1010:
  (x:int[x=1; x<=-1] -> (unit -> X) -> X)

  {SPEC}*)

let rec f x =
  if x = 0 then
    ()
  else
    if read_int () > 0 then
      (event "B"; f (x-2))
    else
      (event "A"; f x)

let main () =
  f 1
(* 下が理想
let rec main () =
  let r = read_int () in
    if r > 0 then
      f r
    else
      (event "A"; main ())
*)
