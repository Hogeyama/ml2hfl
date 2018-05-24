let rec loop1 (set_flag_loop2_50:bool) (s_loop2_n2_47:int) (n1:int) =
  if n1 > 0 then loop1 set_flag_loop2_50 s_loop2_n2_47 (n1 - 1) else 0
and loop2 (prev_set_flag_loop2_49:bool) (s_prev_loop2_n2_48:int)
         (n2:int) =
  if prev_set_flag_loop2_49 then assert false;
  loop2_without_checking_63
    prev_set_flag_loop2_49 s_prev_loop2_n2_48 n2
and loop2_without_checking_63 (_:bool) (_:int) (n2:int) =
  let set_flag_loop2_50 = true
  in
  let s_loop2_n2_47 = n2
  in
  if n2 > 0
  then
    loop1 set_flag_loop2_50 s_loop2_n2_47 n2 +
    loop2 set_flag_loop2_50 s_loop2_n2_47 (n2 - 1)
  else
    0
and u_45 = loop2_without_checking_63 false 0 (Random.int 0)
let u_168 = ()
