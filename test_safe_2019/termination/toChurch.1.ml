let rec compose (x_DO_NOT_CARE_199:bool) (x_DO_NOT_CARE_200:int)
               (x_DO_NOT_CARE_201:int) (x_DO_NOT_CARE_202:int)
               (f_EXPARAM_130:int) (x_DO_NOT_CARE_195:bool)
               (x_DO_NOT_CARE_196:int) (x_DO_NOT_CARE_197:int)
               (x_DO_NOT_CARE_198:int)
               (f:(bool -> int -> int -> int -> int -> int))
               (x_DO_NOT_CARE_191:bool) (x_DO_NOT_CARE_192:int)
               (x_DO_NOT_CARE_193:int) (x_DO_NOT_CARE_194:int)
               (g_EXPARAM_131:int) (x_DO_NOT_CARE_187:bool)
               (x_DO_NOT_CARE_188:int) (x_DO_NOT_CARE_189:int)
               (x_DO_NOT_CARE_190:int)
               (g:(bool -> int -> int -> int -> int -> int))
               (prev_set_flag_compose_144:bool)
               (s_prev_compose_f_EXPARAM_139:int)
               (s_prev_compose_g_EXPARAM_141:int) (s_prev_compose_x_143:int)
               (x:int) =
  if prev_set_flag_compose_144 then assert false;
  compose_without_checking_185
    x_DO_NOT_CARE_199 x_DO_NOT_CARE_200 x_DO_NOT_CARE_201
    x_DO_NOT_CARE_202 f_EXPARAM_130 x_DO_NOT_CARE_195 x_DO_NOT_CARE_196
    x_DO_NOT_CARE_197 x_DO_NOT_CARE_198 f x_DO_NOT_CARE_191
    x_DO_NOT_CARE_192 x_DO_NOT_CARE_193 x_DO_NOT_CARE_194 g_EXPARAM_131
    x_DO_NOT_CARE_187 x_DO_NOT_CARE_188 x_DO_NOT_CARE_189
    x_DO_NOT_CARE_190 g prev_set_flag_compose_144
    s_prev_compose_f_EXPARAM_139 s_prev_compose_g_EXPARAM_141
    s_prev_compose_x_143 x
and compose_without_checking_185 (_:bool) (_:int) (_:int) (_:int)
                                (f_EXPARAM_130:int) (_:bool) (_:int)
                                (_:int) (_:int)
                                (f:(bool ->
                                      int -> int -> int -> int -> int))
                                (_:bool) (_:int) (_:int) (_:int)
                                (g_EXPARAM_131:int) (_:bool) (_:int)
                                (_:int) (_:int)
                                (g:(bool ->
                                      int -> int -> int -> int -> int))
                                (_:bool) (_:int) (_:int) (_:int) (x:int) =
  let set_flag_compose_145 = true
  in
  let s_compose_x_138 = x
  in
  let s_compose_g_EXPARAM_136 = g_EXPARAM_131
  in
  let s_compose_f_EXPARAM_134 = f_EXPARAM_130
  in
  f
    set_flag_compose_145 s_compose_f_EXPARAM_134 s_compose_g_EXPARAM_136
    s_compose_x_138
    (g
      set_flag_compose_145 s_compose_f_EXPARAM_134
      s_compose_g_EXPARAM_136 s_compose_x_138 x)
let id (_:bool) (_:int) (_:int) (_:int) (x:int) = x
let succ (_:bool) (_:int) (_:int) (_:int) (x:int) = x + 1
let rec toChurch (_:bool) (_:int) (_:int) (_:int) (n:int) (_:bool)
                (_:int) (_:int) (_:int) (f_EXPARAM_116:int)
                (set_flag_compose_145:bool) (s_compose_f_EXPARAM_134:int)
                (s_compose_g_EXPARAM_136:int) (s_compose_x_138:int)
                (f:(bool -> int -> int -> int -> int -> int)) =
  if n = 0
  then
    id
  else
    compose
      set_flag_compose_145 s_compose_f_EXPARAM_134
      s_compose_g_EXPARAM_136 s_compose_x_138
      (0 * f_EXPARAM_116 + (0 * n + 0)) set_flag_compose_145
      s_compose_f_EXPARAM_134 s_compose_g_EXPARAM_136 s_compose_x_138 
      f set_flag_compose_145 s_compose_f_EXPARAM_134
      s_compose_g_EXPARAM_136 s_compose_x_138
      (0 * f_EXPARAM_116 + (0 * n + 0)) set_flag_compose_145
      s_compose_f_EXPARAM_134 s_compose_g_EXPARAM_136 s_compose_x_138
      (toChurch
        set_flag_compose_145 s_compose_f_EXPARAM_134
        s_compose_g_EXPARAM_136 s_compose_x_138 (n - 1)
        set_flag_compose_145 s_compose_f_EXPARAM_134
        s_compose_g_EXPARAM_136 s_compose_x_138
        (0 * f_EXPARAM_116 + (0 * n + 0)) set_flag_compose_145
        s_compose_f_EXPARAM_134 s_compose_g_EXPARAM_136 s_compose_x_138 
        f)
let main (set_flag_compose_145:bool) (s_compose_f_EXPARAM_134:int)
        (s_compose_g_EXPARAM_136:int) (s_compose_x_138:int) (():unit) =
  let x = Random.int 0
  in
  (if x >= 0
   then
     let tos =
       toChurch
         set_flag_compose_145 s_compose_f_EXPARAM_134
         s_compose_g_EXPARAM_136 s_compose_x_138 x set_flag_compose_145
         s_compose_f_EXPARAM_134 s_compose_g_EXPARAM_136 s_compose_x_138
         (0 * x + (0 * x + 0)) set_flag_compose_145
         s_compose_f_EXPARAM_134 s_compose_g_EXPARAM_136 s_compose_x_138
         succ
     in
     ())
let u_2310 = main false 0 0 0 ()
