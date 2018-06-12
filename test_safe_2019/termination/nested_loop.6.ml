let rec loop1 (prev_set_flag_loop1_75:bool) (s_prev_loop1_n1_74:int) (n1:int) =
  if prev_set_flag_loop1_75
  then
    if s_prev_loop1_n1_74 > n1 && n1 >= 0 then () else assert false;
  loop1_without_checking_83
    prev_set_flag_loop1_75 s_prev_loop1_n1_74 n1
and loop1_without_checking_83 (_:bool) (_:int) (n1:int) =
  let set_flag_loop1_76 = true
  in
  let s_loop1_n1_73 = n1
  in
  if n1 > 0
  then
    loop1_without_checking_83
      set_flag_loop1_76 s_loop1_n1_73 (n1 - 1)
  else
    0
and loop2 (set_flag_loop1_76:bool) (s_loop1_n1_73:int) (n2:int) =
  if n2 > 0
  then
    loop1 set_flag_loop1_76 s_loop1_n1_73 n2 +
    loop2 set_flag_loop1_76 s_loop1_n1_73 (n2 - 1)
  else
    0
and u_46 = loop2 false 0 (Random.int 0)
let u_3926 = ()
