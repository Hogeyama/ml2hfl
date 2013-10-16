type bexp = Var of int | And of bexp * bexp | Or of bexp * bexp

let rec cnf e =
  match e with
    Var x -> Var x
  | And(e1,e2) ->
    begin
      match cnf e1 with
        Or(e11,e12) -> Or(cnf (And(e11,e2)), cnf (And(e12,e2)))
      | e1' ->
        match cnf e2 with
          Or(e21,e22) -> Or(cnf (And(e1', e21)), cnf (And(e1', e22)))
        | e2' -> And(e1',e2')
    end
  | Or(e1,e2) -> Or(cnf e1, cnf e2)

let rec bexp_eq e1 e2 =
  match e1,e2 with
    Var x, Var y -> x = y
  | And(e11,e12), And(e21,e22) -> bexp_eq e11 e21 && bexp_eq e12 e22
  | Or(e11,e12), Or(e21,e22) -> bexp_eq e11 e21 && bexp_eq e12 e22

let main e = assert (bexp_eq (cnf e) (cnf (cnf e)))
