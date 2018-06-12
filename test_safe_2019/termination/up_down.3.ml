let rec app (_:bool) (_:int) (_:int) (_:bool) (_:int)
           (f:(bool -> int -> int -> unit)) (set_flag_up_94:bool)
           (s_up_x_91:int) (x:int) = f set_flag_up_94 s_up_x_91 x
and down (set_flag_up_94:bool) (s_up_x_91:int) (x:int) =
  if x = 0 then () else down set_flag_up_94 s_up_x_91 (x - 1)
and up (prev_set_flag_up_93:bool) (s_prev_up_x_92:int) (x:int) =
  if prev_set_flag_up_93
  then
    if -s_prev_up_x_92 > -x && -x >= 0 then () else assert false;
  up_without_checking_120
    prev_set_flag_up_93 s_prev_up_x_92 x
and up_without_checking_120 (_:bool) (_:int) (x:int) =
  let set_flag_up_94 = true
  in
  let s_up_x_91 = x
  in
  if x = 0
  then
    ()
  else
    up_without_checking_120 set_flag_up_94 s_up_x_91 (x + 1)
let main (set_flag_up_94:bool) (s_up_x_91:int) (():unit) =
  let t1 = Random.int 0
  in
  let t2 = Random.int 0
  in
  if t1 > 0
  then
    app
      set_flag_up_94 s_up_x_91
      (0 * t2 + (0 * t2 + (0 * t1 + (0 * t1 + 0))))
      set_flag_up_94 s_up_x_91 down set_flag_up_94 s_up_x_91
      t1
  else
    (if t2 < 0
     then
       app
         set_flag_up_94 s_up_x_91
         (0 * t2 + (0 * t2 + (0 * t1 + (0 * t1 + 0))))
         set_flag_up_94 s_up_x_91 up set_flag_up_94 s_up_x_91
         t2)
let u_3416 = main false 0 ()
