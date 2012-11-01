exists :
  len:int[len > 0] ->
  i:int[0 <= i] ->
  j:int[i <= j; j - i < len] ->
  (x:int -> r:int[r = x + i]) ->
  (y:int[y = j] -> b:bool) -> bool
