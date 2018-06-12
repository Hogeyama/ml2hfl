let rec f_65 (_:bool) (_:int) (_:int) (_:int) (x:int) = x
and omega (set_flag_f_98:bool) (s_f_x_EXPARAM_87:int) (s_f_y_EXPARAM_89:int)
         (s_f_z_91:int) (x:int) =
  omega set_flag_f_98 s_f_x_EXPARAM_87 s_f_y_EXPARAM_89 s_f_z_91 x
and f_66 (x_DO_NOT_CARE_141:bool) (x_DO_NOT_CARE_142:int)
        (x_DO_NOT_CARE_143:int) (x_DO_NOT_CARE_144:int) (x_EXPARAM_67:int)
        (x_DO_NOT_CARE_137:bool) (x_DO_NOT_CARE_138:int)
        (x_DO_NOT_CARE_139:int) (x_DO_NOT_CARE_140:int)
        (x:(bool -> int -> int -> int -> int -> int))
        (x_DO_NOT_CARE_133:bool) (x_DO_NOT_CARE_134:int)
        (x_DO_NOT_CARE_135:int) (x_DO_NOT_CARE_136:int) (y_EXPARAM_68:int)
        (x_DO_NOT_CARE_129:bool) (x_DO_NOT_CARE_130:int)
        (x_DO_NOT_CARE_131:int) (x_DO_NOT_CARE_132:int)
        (y:(bool -> int -> int -> int -> int -> int))
        (prev_set_flag_f_97:bool) (s_prev_f_x_EXPARAM_92:int)
        (s_prev_f_y_EXPARAM_94:int) (s_prev_f_z_96:int) (z:int) =
  if prev_set_flag_f_97 then assert false;
  f_without_checking_127
    x_DO_NOT_CARE_141 x_DO_NOT_CARE_142 x_DO_NOT_CARE_143
    x_DO_NOT_CARE_144 x_EXPARAM_67 x_DO_NOT_CARE_137 x_DO_NOT_CARE_138
    x_DO_NOT_CARE_139 x_DO_NOT_CARE_140 x x_DO_NOT_CARE_133
    x_DO_NOT_CARE_134 x_DO_NOT_CARE_135 x_DO_NOT_CARE_136 y_EXPARAM_68
    x_DO_NOT_CARE_129 x_DO_NOT_CARE_130 x_DO_NOT_CARE_131
    x_DO_NOT_CARE_132 y prev_set_flag_f_97 s_prev_f_x_EXPARAM_92
    s_prev_f_y_EXPARAM_94 s_prev_f_z_96 z
and f_without_checking_127 (_:bool) (_:int) (_:int) (_:int)
                          (x_EXPARAM_67:int) (_:bool) (_:int) (_:int)
                          (_:int)
                          (_:(bool -> int -> int -> int -> int -> int))
                          (_:bool) (_:int) (_:int) (_:int)
                          (y_EXPARAM_68:int) (_:bool) (_:int) (_:int)
                          (_:int)
                          (y:(bool -> int -> int -> int -> int -> int))
                          (_:bool) (_:int) (_:int) (_:int) (z:int) =
  let set_flag_f_98 = true
  in
  let s_f_z_91 = z
  in
  let s_f_y_EXPARAM_89 = y_EXPARAM_68
  in
  let s_f_x_EXPARAM_87 = x_EXPARAM_67
  in
  y set_flag_f_98 s_f_x_EXPARAM_87 s_f_y_EXPARAM_89 s_f_z_91 z
and u_63 =
  let id = f_65
  in
  let f = f_66
  in
  f
    false 0 0 0 (0 * u_63 + 0) false 0 0 0
    (f
      false 0 0 0 (0 * u_63 + 0) false 0 0 0 id false 0 0 0
      (0 * u_63 + 0) false 0 0 0 omega)
    false 0 0 0 (0 * u_63 + 0) false 0 0 0 id false 0 0 0 1
let u_1794 = ()
