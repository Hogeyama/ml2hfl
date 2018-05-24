let c6_COEFFICIENT_97 = 0
let c5_COEFFICIENT_96 = 0
let c4_COEFFICIENT_95 = 0
let c3_COEFFICIENT_94 = 0
let c2_COEFFICIENT_91 = 0
let c1_COEFFICIENT_90 = 0
let c0_COEFFICIENT_89 = 0
let rec foldr (x_DO_NOT_CARE_142:bool) (x_DO_NOT_CARE_143:int)
             (x_DO_NOT_CARE_144:int) (x_DO_NOT_CARE_145:int)
             (h_EXPARAM_93:int) (x_DO_NOT_CARE_138:bool)
             (x_DO_NOT_CARE_139:int) (x_DO_NOT_CARE_140:int)
             (x_DO_NOT_CARE_141:int)
             (h:(bool ->
                   int ->
                     int ->
                       int -> int -> bool -> int -> int -> int -> int -> int))
             (x_DO_NOT_CARE_134:bool) (x_DO_NOT_CARE_135:int)
             (x_DO_NOT_CARE_136:int) (x_DO_NOT_CARE_137:int) (e:int)
             (prev_set_flag_foldr_108:bool) (s_prev_foldr_h_EXPARAM_104:int)
             (s_prev_foldr_e_106:int) (s_prev_foldr_l_107:int) (l:int) =
  if prev_set_flag_foldr_108
  then
    if s_prev_foldr_l_107 > l && l >= 0 then () else assert false;
  foldr_without_checking_132
    x_DO_NOT_CARE_142 x_DO_NOT_CARE_143 x_DO_NOT_CARE_144
    x_DO_NOT_CARE_145 h_EXPARAM_93 x_DO_NOT_CARE_138
    x_DO_NOT_CARE_139 x_DO_NOT_CARE_140 x_DO_NOT_CARE_141 h
    x_DO_NOT_CARE_134 x_DO_NOT_CARE_135 x_DO_NOT_CARE_136
    x_DO_NOT_CARE_137 e prev_set_flag_foldr_108
    s_prev_foldr_h_EXPARAM_104 s_prev_foldr_e_106 s_prev_foldr_l_107
    l
and foldr_without_checking_132 (_:bool) (_:int) (_:int) (_:int)
                              (h_EXPARAM_93:int) (_:bool) (_:int)
                              (_:int) (_:int)
                              (h:(bool ->
                                    int ->
                                      int ->
                                        int ->
                                          int ->
                                            bool ->
                                              int ->
                                                int ->
                                                  int -> int -> int))
                              (_:bool) (_:int) (_:int) (_:int)
                              (e:int) (_:bool) (_:int) (_:int)
                              (_:int) (l:int) =
  let set_flag_foldr_109 = true
  in
  let s_foldr_l_103 = l
  in
  let s_foldr_e_102 = e
  in
  let s_foldr_h_EXPARAM_100 = h_EXPARAM_93
  in
  if l = 0
  then
    e
  else
    h
      set_flag_foldr_109 s_foldr_h_EXPARAM_100 s_foldr_e_102
      s_foldr_l_103 (Random.int 0) set_flag_foldr_109
      s_foldr_h_EXPARAM_100 s_foldr_e_102 s_foldr_l_103
      (foldr_without_checking_132
        set_flag_foldr_109 s_foldr_h_EXPARAM_100 s_foldr_e_102
        s_foldr_l_103
        (c6_COEFFICIENT_97 * l +
         (c5_COEFFICIENT_96 * e +
          (c4_COEFFICIENT_95 * h_EXPARAM_93 + c3_COEFFICIENT_94)))
        set_flag_foldr_109 s_foldr_h_EXPARAM_100 s_foldr_e_102
        s_foldr_l_103 h set_flag_foldr_109 s_foldr_h_EXPARAM_100
        s_foldr_e_102 s_foldr_l_103 e set_flag_foldr_109
        s_foldr_h_EXPARAM_100 s_foldr_e_102 s_foldr_l_103 (l - 1))
let sum (_:bool) (_:int) (_:int) (_:int) (m:int) (_:bool) (_:int)
       (_:int) (_:int) (n:int) = m + n
let main (set_flag_foldr_109:bool) (s_foldr_h_EXPARAM_100:int)
        (s_foldr_e_102:int) (s_foldr_l_103:int) (():unit) =
  let l = Random.int 0
  in
  if l >= 0
  then
    foldr
      set_flag_foldr_109 s_foldr_h_EXPARAM_100 s_foldr_e_102
      s_foldr_l_103
      (c2_COEFFICIENT_91 * l +
       (c1_COEFFICIENT_90 * l + c0_COEFFICIENT_89))
      set_flag_foldr_109 s_foldr_h_EXPARAM_100 s_foldr_e_102
      s_foldr_l_103 sum set_flag_foldr_109 s_foldr_h_EXPARAM_100
      s_foldr_e_102 s_foldr_l_103 (Random.int 0) set_flag_foldr_109
      s_foldr_h_EXPARAM_100 s_foldr_e_102 s_foldr_l_103 l
  else
    0
let u_8200 = main false 0 0 0 ()
