val nth : n:int[n>=0] -> l:(int list)[length l = n+1] -> int
val make_list : n:int -> l:(int list)[length l = n]

val unzip : (int * int) list -> int list * int list[]
val zip : l1:_:int list[length l1 >= 0] -> l2:(int list)[length l1 = length l2] -> (int * int) list
val make_list : int -> (int * int) list
val main : int -> (int * int) list
