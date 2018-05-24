let c9_COEFFICIENT_86 = 0
let c8_COEFFICIENT_85 = 0
let c7_COEFFICIENT_84 = 0
let c6_COEFFICIENT_83 = 0
let c5_COEFFICIENT_82 = 0
let c4_COEFFICIENT_80 = 0
let c3_COEFFICIENT_79 = 0
let c2_COEFFICIENT_78 = 0
let c1_COEFFICIENT_77 = 0
let c0_COEFFICIENT_76 = 0
let rec app (_:bool) (_:int) (_:int) (_:bool) (_:int)
           (f:(bool -> int -> int -> unit)) (set_flag_down_140:bool)
           (s_down_x_137:int) (x:int) = f set_flag_down_140 s_down_x_137 x
and down (prev_set_flag_down_139:bool) (s_prev_down_x_138:int) (x:int) =
  if prev_set_flag_down_139 then assert false;
  down_without_checking_160 prev_set_flag_down_139 s_prev_down_x_138 x
and down_without_checking_160 (_:bool) (_:int) (x:int) =
  let set_flag_down_140 = true
  in
  let s_down_x_137 = x
  in
  if x = 0 then () else down set_flag_down_140 s_down_x_137 (x - 1)
and up (set_flag_down_140:bool) (s_down_x_137:int) (x:int) =
  if x = 0
  then
    ()
  else
    up set_flag_down_140 s_down_x_137 (x + 1)
let main (set_flag_down_140:bool) (s_down_x_137:int) (():unit) =
  let t1 = Random.int 0
  in
  let t2 = Random.int 0
  in
  if t1 > 0
  then
    app
      set_flag_down_140 s_down_x_137
      (c9_COEFFICIENT_86 * t2 +
       (c8_COEFFICIENT_85 * t2 +
        (c7_COEFFICIENT_84 * t1 +
         (c6_COEFFICIENT_83 * t1 + c5_COEFFICIENT_82))))
      set_flag_down_140 s_down_x_137 down_without_checking_160
      set_flag_down_140 s_down_x_137 t1
  else
    (if t2 < 0
     then
       app
         set_flag_down_140 s_down_x_137
         (c4_COEFFICIENT_80 * t2 +
          (c3_COEFFICIENT_79 * t2 +
           (c2_COEFFICIENT_78 * t1 +
            (c1_COEFFICIENT_77 * t1 + c0_COEFFICIENT_76))))
         set_flag_down_140 s_down_x_137 up set_flag_down_140
         s_down_x_137 t2)
let u_8459 = main false 0 ()
