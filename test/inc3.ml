let make_array n i = assert(0<=i && i<n); 0 in
let update i n des x = let _ = des i in () in
let rec inc3 m src i =
 if i>=m
 then ()
 else
   begin
     update i m src ((src i)+1);
     inc3 m src (i+1)
   end
in
 if n>0 then inc3 n (make_array n) i else ()
