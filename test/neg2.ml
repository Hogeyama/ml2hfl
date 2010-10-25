(*
let twice f x = f (f x) in
let foo x = if x>=0 then -2*x else -x in
  if n>=0
  then
    let x = twice (twice foo) n in
      assert (x>=0)
  else ()
*)

let twice f x = f (f x) in
let foo x = -x in
  if n>=0
  then
    let x = twice (twice foo) n in
      assert (x>=0)
  else ()
