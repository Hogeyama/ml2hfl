let rec loop x = loop () in
let init = 0 in
let opened = 1 in
let closed = 2 in
let ignore = 3 in
let readit st =
 if st = 1 then 1 else (if st = 3 then st else (fail (); 0)) in
let read x st =
 if x then readit st
 else st in
let closeit st =
 if st = 1 then 2 else (if st = 3 then st else (loop (); 0)) in
let close x st =
 if x then closeit st else st in
let rec f x y st =
 close y (close x st); f x y (read y (read x st)) in
let next st = if st=0 then 1 else 3 in
let g x st = if b3 > 0 then f x true (next st) else f x false st in
let z = if b2 > 0 then g true 1 else g false 0 in
 ()

