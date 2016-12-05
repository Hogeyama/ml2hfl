
let rec is_zero x = x = 0

let rec associate x y =
  let _ = y 0 in
  if is_zero x then 0 else assert false

let main () =
  associate 0 (fun _ -> let rec loop() = loop() in loop())
