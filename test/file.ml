(* camlp4/boot/camlp4boot.ml *)

let open q x = if q=0 then x (stdin+1) 1 else fail() in
let read ci q x = if ci=stdin || q=1 then x q else fail() in
let close ci q x = if not(ci=stdin) && q=1 then x 0 else fail() in
let fin q = if q=0 then () else fail() in

let rec f2 ci q = if name = -1 then fin q else close ci q fin in
let rec f1 n ci q = if n<=0 then f2 ci q else read ci q (f1 (n-1) ci) in
let main n q = if name = -1 then f1 n stdin q else open q (f1 n) in
  main n 0


