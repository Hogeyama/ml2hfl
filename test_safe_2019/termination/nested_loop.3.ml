let rec loop1 (set_flag_loop2_51:bool) (s_loop2_n2_48:int) (n1:int) =
  if n1 > 0 then loop1 set_flag_loop2_51 s_loop2_n2_48 (n1 - 1) else 0
and loop2 (prev_set_flag_loop2_50:bool) (s_prev_loop2_n2_49:int)
         (n2:int) =
  if prev_set_flag_loop2_50
  then
    if s_prev_loop2_n2_49 > n2 && n2 >= 0
    then
      ()
    else
      assert false;
  loop2_without_checking_64
    prev_set_flag_loop2_50 s_prev_loop2_n2_49 n2
and loop2_without_checking_64 (_:bool) (_:int) (n2:int) =
  let set_flag_loop2_51 = true
  in
  let s_loop2_n2_48 = n2
  in
  if n2 > 0
  then
    loop1 set_flag_loop2_51 s_loop2_n2_48 n2 +
    loop2_without_checking_64
      set_flag_loop2_51 s_loop2_n2_48 (n2 - 1)
  else
    0
and u_46 = loop2 false 0 (Random.int 0)
let u_1786 = ()
