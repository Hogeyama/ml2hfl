let rec map (x_DO_NOT_CARE_180:bool) (x_DO_NOT_CARE_181:int)
           (x_DO_NOT_CARE_182:int) (f_EXPARAM_127:int)
           (x_DO_NOT_CARE_177:bool) (x_DO_NOT_CARE_178:int)
           (x_DO_NOT_CARE_179:int) (f:(bool -> int -> int -> int -> int))
           (prev_set_flag_map_139:bool) (s_prev_map_f_EXPARAM_136:int)
           (s_prev_map_xs_138:int) (xs:int) =
  if prev_set_flag_map_139 then assert false;
  map_without_checking_175
    x_DO_NOT_CARE_180 x_DO_NOT_CARE_181 x_DO_NOT_CARE_182 f_EXPARAM_127
    x_DO_NOT_CARE_177 x_DO_NOT_CARE_178 x_DO_NOT_CARE_179 f
    prev_set_flag_map_139 s_prev_map_f_EXPARAM_136 s_prev_map_xs_138 
    xs
and map_without_checking_175 (_:bool) (_:int) (_:int) (f_EXPARAM_127:int)
                            (_:bool) (_:int) (_:int)
                            (f:(bool -> int -> int -> int -> int))
                            (_:bool) (_:int) (_:int) (xs:int) =
  let set_flag_map_140 = true
  in
  let s_map_xs_135 = xs
  in
  let s_map_f_EXPARAM_133 = f_EXPARAM_127
  in
  if xs = 0
  then
    0
  else
    f set_flag_map_140 s_map_f_EXPARAM_133 s_map_xs_135 (Random.int 0) +
    map_without_checking_175
      set_flag_map_140 s_map_f_EXPARAM_133 s_map_xs_135
      (0 * xs + (0 * f_EXPARAM_127 + 0)) set_flag_map_140
      s_map_f_EXPARAM_133 s_map_xs_135 f set_flag_map_140
      s_map_f_EXPARAM_133 s_map_xs_135 (xs - 1)
let compose (_:bool) (_:int) (_:int) (_:int) (_:bool) (_:int) (_:int)
           (f:(bool -> int -> int -> int -> int)) (_:bool) (_:int)
           (_:int) (_:int) (_:bool) (_:int) (_:int)
           (g:(bool -> int -> int -> int -> int)) (set_flag_map_140:bool)
           (s_map_f_EXPARAM_133:int) (s_map_xs_135:int) (x:int) =
  f
    set_flag_map_140 s_map_f_EXPARAM_133 s_map_xs_135
    (g set_flag_map_140 s_map_f_EXPARAM_133 s_map_xs_135 x)
let add (_:bool) (_:int) (_:int) (x:int) (_:bool) (_:int) (_:int) (y:int) =
  x + y
let main (set_flag_map_140:bool) (s_map_f_EXPARAM_133:int)
        (s_map_xs_135:int) (():unit) =
  let l = Random.int 0
  in
  if l >= 0
  then
    map
      set_flag_map_140 s_map_f_EXPARAM_133 s_map_xs_135
      (0 * l + (0 * l + 0)) set_flag_map_140 s_map_f_EXPARAM_133
      s_map_xs_135
      (compose
        set_flag_map_140 s_map_f_EXPARAM_133 s_map_xs_135
        (0 * l + (0 * l + 0)) set_flag_map_140 s_map_f_EXPARAM_133
        s_map_xs_135
        (add set_flag_map_140 s_map_f_EXPARAM_133 s_map_xs_135 1)
        set_flag_map_140 s_map_f_EXPARAM_133 s_map_xs_135
        (0 * l + (0 * l + 0)) set_flag_map_140 s_map_f_EXPARAM_133
        s_map_xs_135
        (add set_flag_map_140 s_map_f_EXPARAM_133 s_map_xs_135 2))
      set_flag_map_140 s_map_f_EXPARAM_133 s_map_xs_135 l
  else
    0
let u_1395 = main false 0 0 ()
