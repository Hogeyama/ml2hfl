let make_array n i = assert (0 <= i && i < n); 0 in
let update i n des x =
  des i;
  let a j = if i=j then x else des i in a
in
let print_int n = () in
let f m src des =
  let rec bcopy m src des i =
    if i >= m
    then des
    else
      let des = update i m des (src i) in
        bcopy m src des (i+1)
  in
  let rec print_array m array i =
    if i >= m
    then ()
    else
      (print_int (des i);
       print_array m array (i+1))
  in
    if c0 = 0
    then
      let array = bcopy m src des c0 in
        print_array m array c0
    else ()
in
let array1 = make_array n in
let array2 = make_array n in
  if n>0
  then f n array1 array2
  else ()
