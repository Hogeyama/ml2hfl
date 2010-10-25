
let make_array n i = assert (0 <= i && i < n); 0 in
let update des i x = let _ = des i in () in

let rec bcopy_aux src des i m =
  if i = m
  then ()
  else
    begin
      update des i (src i);
      bcopy_aux src des (i+1) m
    end
in
let array1 = make_array n in
let array2 = make_array m in
  if 0<=n && 0<=m && n<=m then bcopy_aux array1 array2 0 n else ()








