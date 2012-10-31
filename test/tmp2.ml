let make_array n i = assert (i < n); 0 in

let rec bcopy_aux src i =
  if i >= n
  then ()
  else
    begin
      src i;
      bcopy_aux src (i+1)
    end
in
let array1 = make_array n in
  bcopy_aux array1 0
