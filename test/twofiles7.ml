
let read x = if x then {read} () else () in
let write x = if x then {write} () else () in
let close x = if x then {close} () else () in
let rec f i n x y = if i=0 then (if n>0 then close y else close x) else ((if n>0 then read x else write y); f (i+1) n x y) in
let g n x = if not (n>0) && b3>0 then ({neww} (); f i0 n x true) else f i0 n x false in
  if n>0 && b2>0 then ({newr}(); g n true) else (g n false)


