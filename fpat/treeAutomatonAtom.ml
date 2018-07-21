let mem t1 t2 = Atom.mk_brel (Const.In) t1 t2
let sub t1 t2 = Atom.mk_brel (Const.Subset) t1 t2
let ins t1 t2 = Atom.mk_brel (Const.InterSection) t1 t2
let uni t1 t2 = Atom.mk_brel (Const.Union) t1 t2
let cmp t1 t2 = Atom.mk_brel (Const.Complement) t1 t2
