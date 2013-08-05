
val array_max :
  i:int ->
  n:int ->
  (int->int) ->
  (m:int -> (j:int-> r:int[(i <= j) ;(j < n); m >= r]) -> unit) ->
  unit
