
type aty = ITbase of string | ITfun of ty * aty 
         | ITcase of int * int  (* (arity, position of non-top argument) *)
         | ITpair1 of aty | ITpair2 of aty
and ty = aty list
type name = string

