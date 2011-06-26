let make_array n i = assert (0 <= i && i < n); 0
let update i n des x =
  des i;
  let a j = if i=j then x else des i in a
let print_int n = ()
let f m src des =
  let rec bcopy m src des i =
    if i >= m then
      des
    else
      let des = update i m des (src i) in
      bcopy m src des (i+1)
  in
  let rec print_array m array i =
    if i >= m then
      ()
    else
      (print_int (des i);
       print_array m array (i + 1))
  in
  let array = bcopy m src des 0 in
  print_array m array 0
let main n =
  let array1 = make_array n in
  let array2 = make_array n in
  if n > 0 then f n array1 array2
