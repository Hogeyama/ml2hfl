(* "i <= j && j < n" should not be splitted because of cartesian abstraction *)
val array_max :
  i:int ->
  n:int ->
  (int->int) ->
  (m:int -> (j:int-> r:int[not (i <= j && j < n) || m >= r]) -> unit) ->
  unit
