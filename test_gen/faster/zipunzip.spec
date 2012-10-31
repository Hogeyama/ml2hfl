nth : n:int[n>=0] -> l:_:int list[length l = n+1] -> int
make_list : n:int -> l:_:int list[length l = n]

let rec unzip (xys:(int*int) list) =

unzip : (int * int) list -> int list * int list[]
zip : l1:_:int list[length l1 >= 0] -> l2:_:int list[length l1 = length l2] -> (int * int) list
make_list : int -> (int * int) list
main : int -> (int * int) list
