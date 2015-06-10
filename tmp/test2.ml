
let failwith s = raise (Failure s)

external grille_vers_array : int array array -> int array array = "%grille_vers_array"
external check : int array array -> (int * int) -> bool = "%check"

let verif g =
  let a = grille_vers_array g in
  try
    for i = 0 to 7 do
      for j = 0 to 7 do
      	if a.(i).(j) = 0 then begin
	  a.(i).(j) <- 1;
          if not (check a (i,j)) then failwith "bad"
	end
      done
    done; true
  with Failure _ -> false
