
let read x = if x=1 then {read} () else () in
let write x = if x=1 then {write} () else () in
let close x = if x=1 then {close} () else () in
let rec f n i x y = if i=0 then ((if n >= 0 then close y else ()); close x) else (read x; (if n >= 0 then write y else ()); f n (i+1) x y) in
let g n x = if b3>0 then ((if n >= 0 then {neww} () else ()); f n i0 x 1) else f n i0 x 0 in
let z = if b2>0 then ({newr}(); g n 1) else (g n 0) in
 ()

