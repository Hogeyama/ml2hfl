let rec foldr (x_DO_NOT_CARE_143:bool) (x_DO_NOT_CARE_144:int)
             (x_DO_NOT_CARE_145:int) (x_DO_NOT_CARE_146:int)
             (h_EXPARAM_94:int) (x_DO_NOT_CARE_139:bool)
             (x_DO_NOT_CARE_140:int) (x_DO_NOT_CARE_141:int)
             (x_DO_NOT_CARE_142:int)
             (h:(bool ->
                   int ->
                     int ->
                       int -> int -> bool -> int -> int -> int -> int -> int))
             (x_DO_NOT_CARE_135:bool) (x_DO_NOT_CARE_136:int)
             (x_DO_NOT_CARE_137:int) (x_DO_NOT_CARE_138:int) (e:int)
             (prev_set_flag_foldr_109:bool) (s_prev_foldr_h_EXPARAM_105:int)
             (s_prev_foldr_e_107:int) (s_prev_foldr_l_108:int) (l:int) =
  if prev_set_flag_foldr_109
  then
    if s_prev_foldr_l_108 > l && l >= 0 then () else assert false;
  foldr_without_checking_133
    x_DO_NOT_CARE_143 x_DO_NOT_CARE_144 x_DO_NOT_CARE_145
    x_DO_NOT_CARE_146 h_EXPARAM_94 x_DO_NOT_CARE_139
    x_DO_NOT_CARE_140 x_DO_NOT_CARE_141 x_DO_NOT_CARE_142 h
    x_DO_NOT_CARE_135 x_DO_NOT_CARE_136 x_DO_NOT_CARE_137
    x_DO_NOT_CARE_138 e prev_set_flag_foldr_109
    s_prev_foldr_h_EXPARAM_105 s_prev_foldr_e_107 s_prev_foldr_l_108
    l
and foldr_without_checking_133 (_:bool) (_:int) (_:int) (_:int)
                              (h_EXPARAM_94:int) (_:bool) (_:int)
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
  let set_flag_foldr_110 = true
  in
  let s_foldr_l_104 = l
  in
  let s_foldr_e_103 = e
  in
  let s_foldr_h_EXPARAM_101 = h_EXPARAM_94
  in
  if l = 0
  then
    e
  else
    h
      set_flag_foldr_110 s_foldr_h_EXPARAM_101 s_foldr_e_103
      s_foldr_l_104 (Random.int 0) set_flag_foldr_110
      s_foldr_h_EXPARAM_101 s_foldr_e_103 s_foldr_l_104
      (foldr
        set_flag_foldr_110 s_foldr_h_EXPARAM_101 s_foldr_e_103
        s_foldr_l_104 (0 * l + (0 * e + (0 * h_EXPARAM_94 + 0)))
        set_flag_foldr_110 s_foldr_h_EXPARAM_101 s_foldr_e_103
        s_foldr_l_104 h set_flag_foldr_110 s_foldr_h_EXPARAM_101
        s_foldr_e_103 s_foldr_l_104 e set_flag_foldr_110
        s_foldr_h_EXPARAM_101 s_foldr_e_103 s_foldr_l_104 (l - 1))
let sum (_:bool) (_:int) (_:int) (_:int) (m:int) (_:bool) (_:int)
       (_:int) (_:int) (n:int) = m + n
let main (set_flag_foldr_110:bool) (s_foldr_h_EXPARAM_101:int)
        (s_foldr_e_103:int) (s_foldr_l_104:int) (():unit) =
  let l = Random.int 0
  in
  if l >= 0
  then
    foldr_without_checking_133
      set_flag_foldr_110 s_foldr_h_EXPARAM_101 s_foldr_e_103
      s_foldr_l_104 (0 * l + (0 * l + 0)) set_flag_foldr_110
      s_foldr_h_EXPARAM_101 s_foldr_e_103 s_foldr_l_104 sum
      set_flag_foldr_110 s_foldr_h_EXPARAM_101 s_foldr_e_103
      s_foldr_l_104 (Random.int 0) set_flag_foldr_110
      s_foldr_h_EXPARAM_101 s_foldr_e_103 s_foldr_l_104 l
  else
    0
let u_11708 = main false 0 0 0 ()
