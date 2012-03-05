zip : l1:_:int list[length l1 >= 0] -> l2:_:int list[length l1 = length l2] -> (int * int) list
make_list : int -> l:_:int list[length l >= 0]
(* "length _ >= 0" is not necessary when assuming it by encoding *)
