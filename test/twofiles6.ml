
let read x = if x then {read} () else () in
let write x = if x then {write} () else () in
let close x = if x then {close} () else () in
let rec f i x y = if i=0 then (close y; close x) else (read x; read x; f (i+1) x y) in
let g x = if b3>0 then ({neww} (); f i0 x true) else f i0 x false in
let z = if b2>0 then ({newr}(); g true) else (g false) in
 ()

