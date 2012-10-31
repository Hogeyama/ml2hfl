make_list : x:int -> l:_:int list[length l = x]
length_aux : acc:int -> l:_:int list -> r:int[r = acc + length l]

