
valcegar append :
  ((bool -> int ->
    b1:bool -> int ->
    (bool -> bool -> int ->
     b2:bool[not b1 || b2] -> b3:bool -> x:int[not b2 || (not b3 && x = 0)] -> X) -> X)
   ->
   ((b1:bool -> i:int ->
     b2:bool -> j:int ->
     b3:bool -> int ->
     (b41:bool[not b1 || b41] -> b42:bool -> x:int ->
      b51:bool[not b2 || b51] -> b52:bool -> y:int[not (b41 && b51 && i = j) || x = y] ->
      b61:bool[not b3 || b61] -> b62:bool -> z:int[not b61 || (not b62 && z = 0)] -> X) -> X)
    -> X) -> X)


valcegar ys :
    ((bool -> int ->
      b2:bool -> int ->
      (bool -> bool -> int ->
       b41:bool[not b2 || b41] -> b42:bool -> x:int[not b41 || (not b42 && x = 0)] -> X) -> X) ->
     int -> (b3:bool -> x:int[not b3 && x = 0] -> X) -> X)
