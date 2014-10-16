(* camlp4/boot/camlp4boot.ml *)

let stdin_ = Random.int 0
let name = Random.int 0
let open_ q x = if q=0 then x (stdin_+1) 1 else assert false
let read ci q x = if ci=stdin_ || q=1 then x q else assert false
let close ci q x = if not(ci=stdin_) && q=1 then x 0 else assert false
let fin q = assert (q=0)

let rec f2 ci q = if name = -1 then fin q else close ci q fin
let rec f1 n ci q = if n<=0 then f2 ci q else read ci q (f1 (n-1) ci)
let main n q = if name = -1 then f1 n stdin_ q else open_ q (f1 n)
let main n = main n 0
