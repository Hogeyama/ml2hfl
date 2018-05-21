let mem ty1 ty2 = Formula.of_atom (TreeAutomatonAtom.mem ty1 ty2)
let sub ty1 ty2 = Formula.of_atom (TreeAutomatonAtom.sub ty1 ty2)
