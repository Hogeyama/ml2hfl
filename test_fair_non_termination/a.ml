
(*{SPEC}

val app :
  m:int[m > 1] ->
  (a:int[a > 1] -> unit) ->
  unit

val loop :
  (a:int[a > 1] -> (b:int[b > 1] -> unit) -> unit) ->
  x:int[x > 1] ->
  unit

valcegar #randint_1 :
  (unit -> int -> (x:int[x = 0] -> X) -> X)

valcegar loop_1013 :
  ((x_3:int[x_3 > 42] -> (x_5:int[x_5 > 1] -> (unit -> X) -> X) -> (unit -> X) -> X) ->
    x_14:int[x_14 > 1] -> (unit -> X) -> X)


{SPEC}*)



let app m k = k m

let rec loop h x =
  let b = x > 0 in
  if b then
    (event "A";
    let d = read_int () in
    let y = x + d in
    h y (loop app))
  else
    ()

let main () =
  let r = read_int () in
  loop app r

(*{SPEC}
  fairness: (A,Never)
  {SPEC}*)
