(* succ�̈����̏��Ԃ�ς���̂͒P���ȕ��ёւ��ł͖��� *)
let succ f x = f (x + 1)
let rec app x f = if Random.int 0 = 0 then app (x - 1) (succ f) else f x
let check x y = if x = y then () else assert false
let main n = app n (check n)
