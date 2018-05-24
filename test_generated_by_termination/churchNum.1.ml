let c12_COEFFICIENT_243 = 0
let c11_COEFFICIENT_242 = 0
let c10_COEFFICIENT_241 = 0
let c9_COEFFICIENT_240 = 0
let c8_COEFFICIENT_226 = 0
let c7_COEFFICIENT_225 = 0
let c6_COEFFICIENT_224 = 0
let c5_COEFFICIENT_220 = 0
let c4_COEFFICIENT_219 = 0
let c3_COEFFICIENT_218 = 0
let c2_COEFFICIENT_201 = 0
let c1_COEFFICIENT_200 = 0
let c0_COEFFICIENT_198 = 0
let rec succ (x_DO_NOT_CARE_317:bool) (x_DO_NOT_CARE_318:int)
            (x_DO_NOT_CARE_319:int) (x_DO_NOT_CARE_320:int)
            (m_EXPARAM_237:int) (x_DO_NOT_CARE_313:bool)
            (x_DO_NOT_CARE_314:int) (x_DO_NOT_CARE_315:int)
            (x_DO_NOT_CARE_316:int)
            (m:(bool ->
                  int ->
                    int ->
                      int ->
                        int ->
                          bool ->
                            int ->
                              int ->
                                int ->
                                  (bool -> int -> int -> int -> int -> int) ->
                                    bool -> int -> int -> int -> int -> int))
            (x_DO_NOT_CARE_309:bool) (x_DO_NOT_CARE_310:int)
            (x_DO_NOT_CARE_311:int) (x_DO_NOT_CARE_312:int)
            (s_EXPARAM_239:int) (x_DO_NOT_CARE_305:bool)
            (x_DO_NOT_CARE_306:int) (x_DO_NOT_CARE_307:int)
            (x_DO_NOT_CARE_308:int)
            (s:(bool -> int -> int -> int -> int -> int))
            (prev_set_flag_succ_258:bool) (s_prev_succ_m_EXPARAM_253:int)
            (s_prev_succ_s_EXPARAM_255:int) (s_prev_succ_z_257:int) (
            z:int) =
  if prev_set_flag_succ_258 then assert false;
  succ_without_checking_303
    x_DO_NOT_CARE_317 x_DO_NOT_CARE_318 x_DO_NOT_CARE_319
    x_DO_NOT_CARE_320 m_EXPARAM_237 x_DO_NOT_CARE_313 x_DO_NOT_CARE_314
    x_DO_NOT_CARE_315 x_DO_NOT_CARE_316 m x_DO_NOT_CARE_309
    x_DO_NOT_CARE_310 x_DO_NOT_CARE_311 x_DO_NOT_CARE_312 s_EXPARAM_239
    x_DO_NOT_CARE_305 x_DO_NOT_CARE_306 x_DO_NOT_CARE_307
    x_DO_NOT_CARE_308 s prev_set_flag_succ_258 s_prev_succ_m_EXPARAM_253
    s_prev_succ_s_EXPARAM_255 s_prev_succ_z_257 z
and succ_without_checking_303 (_:bool) (_:int) (_:int) (_:int)
                             (m_EXPARAM_237:int) (_:bool) (_:int) (_:int)
                             (_:int)
                             (m:(bool ->
                                   int ->
                                     int ->
                                       int ->
                                         int ->
                                           bool ->
                                             int ->
                                               int ->
                                                 int ->
                                                   (bool ->
                                                      int ->
                                                        int ->
                                                          int ->
                                                            int -> int) ->
                                                     bool ->
                                                       int ->
                                                         int ->
                                                           int ->
                                                             int -> int))
                             (_:bool) (_:int) (_:int) (_:int)
                             (s_EXPARAM_239:int) (_:bool) (_:int) (_:int)
                             (_:int)
                             (s:(bool -> int -> int -> int -> int -> int))
                             (_:bool) (_:int) (_:int) (_:int) (z:int) =
  let set_flag_succ_259 = true
  in
  let s_succ_z_252 = z
  in
  let s_succ_s_EXPARAM_250 = s_EXPARAM_239
  in
  let s_succ_m_EXPARAM_248 = m_EXPARAM_237
  in
  m
    set_flag_succ_259 s_succ_m_EXPARAM_248 s_succ_s_EXPARAM_250
    s_succ_z_252
    (c12_COEFFICIENT_243 * z +
     (c11_COEFFICIENT_242 * s_EXPARAM_239 +
      (c10_COEFFICIENT_241 * m_EXPARAM_237 + c9_COEFFICIENT_240)))
    set_flag_succ_259 s_succ_m_EXPARAM_248 s_succ_s_EXPARAM_250
    s_succ_z_252 s set_flag_succ_259 s_succ_m_EXPARAM_248
    s_succ_s_EXPARAM_250 s_succ_z_252
    (s
      set_flag_succ_259 s_succ_m_EXPARAM_248 s_succ_s_EXPARAM_250
      s_succ_z_252 z)
let id (_:bool) (_:int) (_:int) (_:int) (x:int) = x
let two (_:bool) (_:int) (_:int) (_:int) (f_EXPARAM_211:int) (_:bool)
       (_:int) (_:int) (_:int)
       (f:(bool ->
             int ->
               int ->
                 int ->
                   int ->
                     bool ->
                       int ->
                         int ->
                           int ->
                             (bool ->
                                int ->
                                  int ->
                                    int ->
                                      int ->
                                        bool ->
                                          int ->
                                            int ->
                                              int ->
                                                (bool ->
                                                   int ->
                                                     int ->
                                                       int -> int -> int) ->
                                                  bool ->
                                                    int ->
                                                      int ->
                                                        int -> int -> int) ->
                               bool ->
                                 int ->
                                   int ->
                                     int ->
                                       int ->
                                         bool ->
                                           int ->
                                             int ->
                                               int ->
                                                 (bool ->
                                                    int ->
                                                      int ->
                                                        int -> int -> int) ->
                                                   bool ->
                                                     int ->
                                                       int ->
                                                         int ->
                                                           int -> int))
       (_:bool) (_:int) (_:int) (_:int) (z_EXPARAM_215:int)
       (set_flag_succ_259:bool) (s_succ_m_EXPARAM_248:int)
       (s_succ_s_EXPARAM_250:int) (s_succ_z_252:int)
       (z:(bool ->
             int ->
               int ->
                 int ->
                   int ->
                     bool ->
                       int ->
                         int ->
                           int ->
                             (bool -> int -> int -> int -> int -> int) ->
                               bool -> int -> int -> int -> int -> int)) =
  f
    set_flag_succ_259 s_succ_m_EXPARAM_248 s_succ_s_EXPARAM_250
    s_succ_z_252
    (c8_COEFFICIENT_226 * z_EXPARAM_215 +
     (c7_COEFFICIENT_225 * f_EXPARAM_211 + c6_COEFFICIENT_224))
    set_flag_succ_259 s_succ_m_EXPARAM_248 s_succ_s_EXPARAM_250
    s_succ_z_252
    (f
      set_flag_succ_259 s_succ_m_EXPARAM_248 s_succ_s_EXPARAM_250
      s_succ_z_252
      (c5_COEFFICIENT_220 * z_EXPARAM_215 +
       (c4_COEFFICIENT_219 * f_EXPARAM_211 + c3_COEFFICIENT_218))
      set_flag_succ_259 s_succ_m_EXPARAM_248 s_succ_s_EXPARAM_250
      s_succ_z_252 z)
let zero (_:bool) (_:int) (_:int) (_:int) (_:int) (_:bool) (_:int)
        (_:int) (_:int) (_:(bool -> int -> int -> int -> int -> int))
        (_:bool) (_:int) (_:int) (_:int) (z:int) = z
let main (set_flag_succ_259:bool) (s_succ_m_EXPARAM_248:int)
        (s_succ_s_EXPARAM_250:int) (s_succ_z_252:int) (():unit) =
  two
    set_flag_succ_259 s_succ_m_EXPARAM_248 s_succ_s_EXPARAM_250
    s_succ_z_252 c0_COEFFICIENT_198 set_flag_succ_259
    s_succ_m_EXPARAM_248 s_succ_s_EXPARAM_250 s_succ_z_252 succ
    set_flag_succ_259 s_succ_m_EXPARAM_248 s_succ_s_EXPARAM_250
    s_succ_z_252 c1_COEFFICIENT_200 set_flag_succ_259
    s_succ_m_EXPARAM_248 s_succ_s_EXPARAM_250 s_succ_z_252 zero
    set_flag_succ_259 s_succ_m_EXPARAM_248 s_succ_s_EXPARAM_250
    s_succ_z_252 c2_COEFFICIENT_201 set_flag_succ_259
    s_succ_m_EXPARAM_248 s_succ_s_EXPARAM_250 s_succ_z_252 id
    set_flag_succ_259 s_succ_m_EXPARAM_248 s_succ_s_EXPARAM_250
    s_succ_z_252 0
let u_4910 = main false 0 0 0 ()
