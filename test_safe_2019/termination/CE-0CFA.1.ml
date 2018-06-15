let rec f_65 (_:bool) (_:int) (_:int) (_:int) (x:int) = x
and omega (set_flag_f_94:bool) (s_f_x_EXPARAM_83:int) (s_f_y_EXPARAM_85:int)
         (s_f_z_87:int) (x:int) =
  omega set_flag_f_94 s_f_x_EXPARAM_83 s_f_y_EXPARAM_85 s_f_z_87 x
and f_66 (x_DO_NOT_CARE_137:bool) (x_DO_NOT_CARE_138:int)
        (x_DO_NOT_CARE_139:int) (x_DO_NOT_CARE_140:int) (x_EXPARAM_67:int)
        (x_DO_NOT_CARE_133:bool) (x_DO_NOT_CARE_134:int)
        (x_DO_NOT_CARE_135:int) (x_DO_NOT_CARE_136:int)
        (x:(bool -> int -> int -> int -> int -> int))
        (x_DO_NOT_CARE_129:bool) (x_DO_NOT_CARE_130:int)
        (x_DO_NOT_CARE_131:int) (x_DO_NOT_CARE_132:int) (y_EXPARAM_68:int)
        (x_DO_NOT_CARE_125:bool) (x_DO_NOT_CARE_126:int)
        (x_DO_NOT_CARE_127:int) (x_DO_NOT_CARE_128:int)
        (y:(bool -> int -> int -> int -> int -> int))
        (prev_set_flag_f_93:bool) (s_prev_f_x_EXPARAM_88:int)
        (s_prev_f_y_EXPARAM_90:int) (s_prev_f_z_92:int) (z:int) =
  if prev_set_flag_f_93 then assert false;
  f_without_checking_123
    x_DO_NOT_CARE_137 x_DO_NOT_CARE_138 x_DO_NOT_CARE_139
    x_DO_NOT_CARE_140 x_EXPARAM_67 x_DO_NOT_CARE_133 x_DO_NOT_CARE_134
    x_DO_NOT_CARE_135 x_DO_NOT_CARE_136 x x_DO_NOT_CARE_129
    x_DO_NOT_CARE_130 x_DO_NOT_CARE_131 x_DO_NOT_CARE_132 y_EXPARAM_68
    x_DO_NOT_CARE_125 x_DO_NOT_CARE_126 x_DO_NOT_CARE_127
    x_DO_NOT_CARE_128 y prev_set_flag_f_93 s_prev_f_x_EXPARAM_88
    s_prev_f_y_EXPARAM_90 s_prev_f_z_92 z
and f_without_checking_123 (_:bool) (_:int) (_:int) (_:int)
                          (x_EXPARAM_67:int) (_:bool) (_:int) (_:int)
                          (_:int)
                          (_:(bool -> int -> int -> int -> int -> int))
                          (_:bool) (_:int) (_:int) (_:int)
                          (y_EXPARAM_68:int) (_:bool) (_:int) (_:int)
                          (_:int)
                          (y:(bool -> int -> int -> int -> int -> int))
                          (_:bool) (_:int) (_:int) (_:int) (z:int) =
  let set_flag_f_94 = true
  in
  let s_f_z_87 = z
  in
  let s_f_y_EXPARAM_85 = y_EXPARAM_68
  in
  let s_f_x_EXPARAM_83 = x_EXPARAM_67
  in
  y set_flag_f_94 s_f_x_EXPARAM_83 s_f_y_EXPARAM_85 s_f_z_87 z
and u_63 =
  let id = f_65
  in
  let f = f_66
  in
  f
    false 0 0 0 0 false 0 0 0
    (f false 0 0 0 0 false 0 0 0 id false 0 0 0 0 false 0 0 0 omega)
    false 0 0 0 0 false 0 0 0 id false 0 0 0 1
let u_1746 = ()
