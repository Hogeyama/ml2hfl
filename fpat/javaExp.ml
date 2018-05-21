type 'a t =
  | Var    of 'a * Idnt.t
  | Not    of 'a * 'a t
  | Int    of 'a * int
  | Int_   of 'a
  | Bool   of 'a * bool
  | Eq     of 'a * 'a t * 'a t
  | Add    of 'a * 'a t * 'a t
  | Sub    of 'a * 'a t * 'a t
  | Mul    of 'a * 'a t * 'a t
  | Field  of 'a * 'a t * Idnt.t
  | Method of 'a * 'a t * Idnt.t * 'a t list
  | New    of 'a * Idnt.t * 'a t list
  | Cast   of 'a * Idnt.t * 'a t
  | IF     of 'a * 'a t * 'a t * 'a t
  | IF_    of 'a * 'a t * 'a t
  | Assert of 'a * 'a t
  | Fail   of 'a
