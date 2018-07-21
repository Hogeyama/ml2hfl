type 'a t =
  | Main   of 'a * (Idnt.t * Idnt.t) list * 'a JavaExp.t
