(*
Leroy & Pessaux, "Type-Based Analysis of Uncaught Exceptionis, " TOPLAS, 2000

let ff n = if n >= 0 then () else raise 0 in
  try ff ?n? with Failer 0 -> ()
*)

let f n k = if n >= 0 then () else k 0 in
let g n = assert (n = 0) in
  f n g
