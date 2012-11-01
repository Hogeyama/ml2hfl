let app (ex:int) f x = f x
let chk x y = assert (x <= y)
let id x = x
let main i = app (id i) (chk i) i
