valcegar aux1 : (n:int[0 <= n] ->
                 (x_3:int[0 <= x_3; 0 <= x_8; x_8 < n] -> (r:int[r >= 0] -> X) -> X) ->
                 x_8:int[0 <= x_8; x_8 < n] -> (unit -> X) -> X)
valcegar aux2 : (j:int[0 <= j] ->
                 n:int[j < n] ->
                 i:int[0 <= i] ->
                 (k:int[0 <= k; k < n] -> (r:int[r >= 0] -> X) -> X) ->
                 i:int[0 <= i] -> (bool -> X) -> X)
valcegar loop : (n:int[0 <= n] ->
                 row:int[1 <= n; row <= n; 0 <= row] ->
                 (i:int[0 <= i; i < n] -> (x:int[0 <= x] -> X) -> X) -> (unit -> X) -> X)
