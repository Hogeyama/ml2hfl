let make_array n i = assert (i < n); 0

let rec bcopy_aux src i n =
  if i >= n
  then ()
  else
    begin
      src i;
      bcopy_aux src (i+1) n
    end

let main n =
  let array1 = make_array n in
  bcopy_aux array1 0 n
