let make_array n i = assert (0 <= i && i < n); 0 in
let update i n des x = let _ = des i in () in

let rec bcopy_aux m src des i =
  if i >= m
  then ()
  else
    begin
      update i m des (src i);
      bcopy_aux m src des (i+1)
    end
in
let array1 = make_array n in
let array2 = make_array m in
  if n<=m && n>0 then bcopy_aux n array1 array2 0 else ()

