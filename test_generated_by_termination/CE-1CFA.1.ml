let c7_COEFFICIENT_79 = 0
let c6_COEFFICIENT_78 = 0
let c5_COEFFICIENT_77 = 0
let c4_COEFFICIENT_76 = 0
let c3_COEFFICIENT_73 = 0
let c2_COEFFICIENT_72 = 0
let c1_COEFFICIENT_71 = 0
let c0_COEFFICIENT_70 = 0
let rec f_64 (_:bool) (_:int) (_:int) (_:int) (x:int) = x
and omega (set_flag_f_97:bool) (s_f_x_EXPARAM_86:int) (s_f_y_EXPARAM_88:int)
         (s_f_z_90:int) (x:int) =
  omega set_flag_f_97 s_f_x_EXPARAM_86 s_f_y_EXPARAM_88 s_f_z_90 x
and f_65 (x_DO_NOT_CARE_140:bool) (x_DO_NOT_CARE_141:int)
        (x_DO_NOT_CARE_142:int) (x_DO_NOT_CARE_143:int) (x_EXPARAM_66:int)
        (x_DO_NOT_CARE_136:bool) (x_DO_NOT_CARE_137:int)
        (x_DO_NOT_CARE_138:int) (x_DO_NOT_CARE_139:int)
        (x:(bool -> int -> int -> int -> int -> int))
        (x_DO_NOT_CARE_132:bool) (x_DO_NOT_CARE_133:int)
        (x_DO_NOT_CARE_134:int) (x_DO_NOT_CARE_135:int) (y_EXPARAM_67:int)
        (x_DO_NOT_CARE_128:bool) (x_DO_NOT_CARE_129:int)
        (x_DO_NOT_CARE_130:int) (x_DO_NOT_CARE_131:int)
        (y:(bool -> int -> int -> int -> int -> int))
        (prev_set_flag_f_96:bool) (s_prev_f_x_EXPARAM_91:int)
        (s_prev_f_y_EXPARAM_93:int) (s_prev_f_z_95:int) (z:int) =
  if prev_set_flag_f_96 then assert false;
  f_without_checking_126
    x_DO_NOT_CARE_140 x_DO_NOT_CARE_141 x_DO_NOT_CARE_142
    x_DO_NOT_CARE_143 x_EXPARAM_66 x_DO_NOT_CARE_136 x_DO_NOT_CARE_137
    x_DO_NOT_CARE_138 x_DO_NOT_CARE_139 x x_DO_NOT_CARE_132
    x_DO_NOT_CARE_133 x_DO_NOT_CARE_134 x_DO_NOT_CARE_135 y_EXPARAM_67
    x_DO_NOT_CARE_128 x_DO_NOT_CARE_129 x_DO_NOT_CARE_130
    x_DO_NOT_CARE_131 y prev_set_flag_f_96 s_prev_f_x_EXPARAM_91
    s_prev_f_y_EXPARAM_93 s_prev_f_z_95 z
and f_without_checking_126 (_:bool) (_:int) (_:int) (_:int)
                          (x_EXPARAM_66:int) (_:bool) (_:int) (_:int)
                          (_:int)
                          (_:(bool -> int -> int -> int -> int -> int))
                          (_:bool) (_:int) (_:int) (_:int)
                          (y_EXPARAM_67:int) (_:bool) (_:int) (_:int)
                          (_:int)
                          (y:(bool -> int -> int -> int -> int -> int))
                          (_:bool) (_:int) (_:int) (_:int) (z:int) =
  let set_flag_f_97 = true
  in
  let s_f_z_90 = z
  in
  let s_f_y_EXPARAM_88 = y_EXPARAM_67
  in
  let s_f_x_EXPARAM_86 = x_EXPARAM_66
  in
  y set_flag_f_97 s_f_x_EXPARAM_86 s_f_y_EXPARAM_88 s_f_z_90 z
and u_62 =
  let id = f_64
  in
  let f = f_65
  in
  f
    false 0 0 0 (c5_COEFFICIENT_77 * u_62 + c4_COEFFICIENT_76) false 
    0 0 0
    (f
      false 0 0 0 (c1_COEFFICIENT_71 * u_62 + c0_COEFFICIENT_70) false 
      0 0 0 id false 0 0 0 (c3_COEFFICIENT_73 * u_62 + c2_COEFFICIENT_72)
      false 0 0 0 omega)
    false 0 0 0 (c7_COEFFICIENT_79 * u_62 + c6_COEFFICIENT_78) false 
    0 0 0 id false 0 0 0 1
let u_1793 = ()
