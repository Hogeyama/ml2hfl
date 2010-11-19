let f n = if n >= 0 then {open} () else () in
let g n = if n >= 0 then {close} () else () in
f 1; g 1

