let rec f (prev_set_flag_f_78:bool) (s_prev_f_x_77:int) (x:int) =
  if prev_set_flag_f_78
  then
    if s_prev_f_x_77 > x && x >= 0 then () else assert false;
  f_without_checking_88 prev_set_flag_f_78 s_prev_f_x_77 x
and f_without_checking_88 (_:bool) (_:int) (x:int) =
  let set_flag_f_79 = true
  in
  let s_f_x_76 = x
  in
  if x > 0
  then
    f_without_checking_88 set_flag_f_79 s_f_x_76 (x - 1)
  else
    lambda
and lambda (_:bool) (_:int) (x:int) = x + 1
let g = f false 0 1
let main (set_flag_f_79:bool) (s_f_x_76:int) (():unit) =
  g set_flag_f_79 s_f_x_76 2
let u_3090 = main false 0 ()
