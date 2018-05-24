let c11_COEFFICIENT_125 = 0
let c10_COEFFICIENT_124 = 0
let c9_COEFFICIENT_123 = 0
let c8_COEFFICIENT_121 = 0
let c7_COEFFICIENT_120 = 0
let c6_COEFFICIENT_119 = 0
let c5_COEFFICIENT_118 = 0
let c4_COEFFICIENT_117 = 0
let c3_COEFFICIENT_116 = 0
let c2_COEFFICIENT_113 = 0
let c1_COEFFICIENT_112 = 0
let c0_COEFFICIENT_111 = 0
let rec compose (x_DO_NOT_CARE_198:bool) (x_DO_NOT_CARE_199:int)
               (x_DO_NOT_CARE_200:int) (x_DO_NOT_CARE_201:int)
               (f_EXPARAM_129:int) (x_DO_NOT_CARE_194:bool)
               (x_DO_NOT_CARE_195:int) (x_DO_NOT_CARE_196:int)
               (x_DO_NOT_CARE_197:int)
               (f:(bool -> int -> int -> int -> int -> int))
               (x_DO_NOT_CARE_190:bool) (x_DO_NOT_CARE_191:int)
               (x_DO_NOT_CARE_192:int) (x_DO_NOT_CARE_193:int)
               (g_EXPARAM_130:int) (x_DO_NOT_CARE_186:bool)
               (x_DO_NOT_CARE_187:int) (x_DO_NOT_CARE_188:int)
               (x_DO_NOT_CARE_189:int)
               (g:(bool -> int -> int -> int -> int -> int))
               (prev_set_flag_compose_143:bool)
               (s_prev_compose_f_EXPARAM_138:int)
               (s_prev_compose_g_EXPARAM_140:int) (s_prev_compose_x_142:int)
               (x:int) =
  if prev_set_flag_compose_143 then assert false;
  compose_without_checking_184
    x_DO_NOT_CARE_198 x_DO_NOT_CARE_199 x_DO_NOT_CARE_200
    x_DO_NOT_CARE_201 f_EXPARAM_129 x_DO_NOT_CARE_194 x_DO_NOT_CARE_195
    x_DO_NOT_CARE_196 x_DO_NOT_CARE_197 f x_DO_NOT_CARE_190
    x_DO_NOT_CARE_191 x_DO_NOT_CARE_192 x_DO_NOT_CARE_193 g_EXPARAM_130
    x_DO_NOT_CARE_186 x_DO_NOT_CARE_187 x_DO_NOT_CARE_188
    x_DO_NOT_CARE_189 g prev_set_flag_compose_143
    s_prev_compose_f_EXPARAM_138 s_prev_compose_g_EXPARAM_140
    s_prev_compose_x_142 x
and compose_without_checking_184 (_:bool) (_:int) (_:int) (_:int)
                                (f_EXPARAM_129:int) (_:bool) (_:int)
                                (_:int) (_:int)
                                (f:(bool ->
                                      int -> int -> int -> int -> int))
                                (_:bool) (_:int) (_:int) (_:int)
                                (g_EXPARAM_130:int) (_:bool) (_:int)
                                (_:int) (_:int)
                                (g:(bool ->
                                      int -> int -> int -> int -> int))
                                (_:bool) (_:int) (_:int) (_:int) (x:int) =
  let set_flag_compose_144 = true
  in
  let s_compose_x_137 = x
  in
  let s_compose_g_EXPARAM_135 = g_EXPARAM_130
  in
  let s_compose_f_EXPARAM_133 = f_EXPARAM_129
  in
  f
    set_flag_compose_144 s_compose_f_EXPARAM_133 s_compose_g_EXPARAM_135
    s_compose_x_137
    (g
      set_flag_compose_144 s_compose_f_EXPARAM_133
      s_compose_g_EXPARAM_135 s_compose_x_137 x)
let id (_:bool) (_:int) (_:int) (_:int) (x:int) = x
let succ (_:bool) (_:int) (_:int) (_:int) (x:int) = x + 1
let rec toChurch (_:bool) (_:int) (_:int) (_:int) (n:int) (_:bool)
                (_:int) (_:int) (_:int) (f_EXPARAM_115:int)
                (set_flag_compose_144:bool) (s_compose_f_EXPARAM_133:int)
                (s_compose_g_EXPARAM_135:int) (s_compose_x_137:int)
                (f:(bool -> int -> int -> int -> int -> int)) =
  if n = 0
  then
    id
  else
    compose
      set_flag_compose_144 s_compose_f_EXPARAM_133
      s_compose_g_EXPARAM_135 s_compose_x_137
      (c5_COEFFICIENT_118 * f_EXPARAM_115 +
       (c4_COEFFICIENT_117 * n + c3_COEFFICIENT_116))
      set_flag_compose_144 s_compose_f_EXPARAM_133
      s_compose_g_EXPARAM_135 s_compose_x_137 f set_flag_compose_144
      s_compose_f_EXPARAM_133 s_compose_g_EXPARAM_135 s_compose_x_137
      (c11_COEFFICIENT_125 * f_EXPARAM_115 +
       (c10_COEFFICIENT_124 * n + c9_COEFFICIENT_123))
      set_flag_compose_144 s_compose_f_EXPARAM_133
      s_compose_g_EXPARAM_135 s_compose_x_137
      (toChurch
        set_flag_compose_144 s_compose_f_EXPARAM_133
        s_compose_g_EXPARAM_135 s_compose_x_137 (n - 1)
        set_flag_compose_144 s_compose_f_EXPARAM_133
        s_compose_g_EXPARAM_135 s_compose_x_137
        (c8_COEFFICIENT_121 * f_EXPARAM_115 +
         (c7_COEFFICIENT_120 * n + c6_COEFFICIENT_119))
        set_flag_compose_144 s_compose_f_EXPARAM_133
        s_compose_g_EXPARAM_135 s_compose_x_137 f)
let main (set_flag_compose_144:bool) (s_compose_f_EXPARAM_133:int)
        (s_compose_g_EXPARAM_135:int) (s_compose_x_137:int) (():unit) =
  let x = Random.int 0
  in
  (if x >= 0
   then
     let tos =
       toChurch
         set_flag_compose_144 s_compose_f_EXPARAM_133
         s_compose_g_EXPARAM_135 s_compose_x_137 x set_flag_compose_144
         s_compose_f_EXPARAM_133 s_compose_g_EXPARAM_135 s_compose_x_137
         (c2_COEFFICIENT_113 * x +
          (c1_COEFFICIENT_112 * x + c0_COEFFICIENT_111))
         set_flag_compose_144 s_compose_f_EXPARAM_133
         s_compose_g_EXPARAM_135 s_compose_x_137 succ
     in
     ())
let u_2309 = main false 0 0 0 ()
