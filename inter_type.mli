type base =
    True
  | False
  | State of int
  | Top

type t =
    Base of base list
  | Inter of t list
  | Fun of t * t
