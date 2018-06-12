let rec app (_:bool) (_:int) (_:int) (_:bool) (_:int)
           (f:(bool -> int -> int -> unit)) (set_flag_down_141:bool)
           (s_down_x_138:int) (x:int) = f set_flag_down_141 s_down_x_138 x
and down (prev_set_flag_down_140:bool) (s_prev_down_x_139:int) (x:int) =
  if prev_set_flag_down_140
  then
    if s_prev_down_x_139 > x && x >= 0 then () else assert false;
  down_without_checking_161
    prev_set_flag_down_140 s_prev_down_x_139 x
and down_without_checking_161 (_:bool) (_:int) (x:int) =
  let set_flag_down_141 = true
  in
  let s_down_x_138 = x
  in
  if x = 0
  then
    ()
  else
    down set_flag_down_141 s_down_x_138 (x - 1)
and up (set_flag_down_141:bool) (s_down_x_138:int) (x:int) =
  if x = 0 then () else up set_flag_down_141 s_down_x_138 (x + 1)
let main (set_flag_down_141:bool) (s_down_x_138:int)
        (():unit) =
  let t1 = Random.int 0
  in
  let t2 = Random.int 0
  in
  if t1 > 0
  then
    app
      set_flag_down_141 s_down_x_138
      (0 * t2 + (0 * t2 + (0 * t1 + (0 * t1 + 0))))
      set_flag_down_141 s_down_x_138
      down_without_checking_161 set_flag_down_141
      s_down_x_138 t1
  else
    (if t2 < 0
     then
       app
         set_flag_down_141 s_down_x_138
         (0 * t2 + (0 * t2 + (0 * t1 + (0 * t1 + 0))))
         set_flag_down_141 s_down_x_138 up set_flag_down_141
         s_down_x_138 t2)
let u_11359 = main false 0 ()
