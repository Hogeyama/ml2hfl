val append : l1:(int list) -> l2:(int list) -> r:(int list)[length r = length l1 + length l2]
val length : l:(int list) -> r:int[r = length l]

(*
valcegar append :
  (x_1:int ->
   (int -> (int -> X) -> X) ->
   x_8:int ->
   (int -> (int -> X) -> X) ->
   (x_16:int[x_1 + x_8 = x_16]
    -> (int -> (int -> X) -> X) -> X)
   -> X)

valcegar length : (x_1:int -> (int -> (int -> X) -> X) -> (x_9:int[x_1 = x_9] -> X) -> X)
*)
