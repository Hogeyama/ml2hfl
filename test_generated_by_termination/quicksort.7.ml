let c39_COEFFICIENT_240 = 0
let c38_COEFFICIENT_239 = 0
let c37_COEFFICIENT_238 = 0
let c36_COEFFICIENT_237 = 0
let c35_COEFFICIENT_236 = 0
let c34_COEFFICIENT_235 = 0
let c33_COEFFICIENT_233 = 0
let c32_COEFFICIENT_232 = 0
let c31_COEFFICIENT_231 = 0
let c30_COEFFICIENT_230 = 0
let c29_COEFFICIENT_229 = 0
let c28_COEFFICIENT_228 = 0
let c27_COEFFICIENT_226 = 0
let c26_COEFFICIENT_225 = 0
let c25_COEFFICIENT_224 = 0
let c24_COEFFICIENT_223 = 0
let c23_COEFFICIENT_222 = 0
let c22_COEFFICIENT_221 = 0
let c21_COEFFICIENT_220 = 0
let c20_COEFFICIENT_219 = 0
let c19_COEFFICIENT_218 = 0
let c18_COEFFICIENT_217 = 0
let c17_COEFFICIENT_215 = 0
let c16_COEFFICIENT_214 = 0
let c15_COEFFICIENT_213 = 0
let c14_COEFFICIENT_212 = 0
let c13_COEFFICIENT_211 = 0
let c12_COEFFICIENT_210 = 0
let c11_COEFFICIENT_209 = 0
let c10_COEFFICIENT_208 = 0
let c9_COEFFICIENT_207 = 0
let c8_COEFFICIENT_206 = 0
let c7_COEFFICIENT_202 = 0
let c6_COEFFICIENT_201 = 0
let c5_COEFFICIENT_200 = 0
let c4_COEFFICIENT_199 = 0
let c3_COEFFICIENT_198 = 0
let c2_COEFFICIENT_195 = 0
let c1_COEFFICIENT_194 = 0
let c0_COEFFICIENT_193 = 0
let rec qs (_:bool) (_:int) (_:int) (_:int) (_:int) (_:int)
          (cmp_EXPARAM_197:int) (_:bool) (_:int) (_:int) (_:int) (_:int)
          (_:int)
          (cmp:(bool ->
                  int ->
                    int ->
                      int ->
                        int ->
                          int ->
                            int ->
                              bool ->
                                int ->
                                  int -> int -> int -> int -> int -> bool))
          (set_flag_par_256:bool) (s_par_cmp_EXPARAM_243:int)
          (s_par_x_245:int) (s_par_l_246:int) (s_par_r_247:int)
          (s_par_xs_248:int) (n:int) =
  if n <= 0
  then
    0
  else
    let xs' = n - 1
    in
    par
      set_flag_par_256 s_par_cmp_EXPARAM_243 s_par_x_245 s_par_l_246
      s_par_r_247 s_par_xs_248
      (c7_COEFFICIENT_202 * xs' +
       (c6_COEFFICIENT_201 * xs' +
        (c5_COEFFICIENT_200 * n +
         (c4_COEFFICIENT_199 * cmp_EXPARAM_197 + c3_COEFFICIENT_198))))
      set_flag_par_256 s_par_cmp_EXPARAM_243 s_par_x_245 s_par_l_246
      s_par_r_247 s_par_xs_248 cmp set_flag_par_256 s_par_cmp_EXPARAM_243
      s_par_x_245 s_par_l_246 s_par_r_247 s_par_xs_248 (Random.int 0)
      set_flag_par_256 s_par_cmp_EXPARAM_243 s_par_x_245 s_par_l_246
      s_par_r_247 s_par_xs_248 0 set_flag_par_256 s_par_cmp_EXPARAM_243
      s_par_x_245 s_par_l_246 s_par_r_247 s_par_xs_248 0 set_flag_par_256
      s_par_cmp_EXPARAM_243 s_par_x_245 s_par_l_246 s_par_r_247 s_par_xs_248
      xs'
and par (x_DO_NOT_CARE_322:bool) (x_DO_NOT_CARE_323:int)
       (x_DO_NOT_CARE_324:int) (x_DO_NOT_CARE_325:int)
       (x_DO_NOT_CARE_326:int) (x_DO_NOT_CARE_327:int) (cmp_EXPARAM_205:int)
       (x_DO_NOT_CARE_316:bool) (x_DO_NOT_CARE_317:int)
       (x_DO_NOT_CARE_318:int) (x_DO_NOT_CARE_319:int)
       (x_DO_NOT_CARE_320:int) (x_DO_NOT_CARE_321:int)
       (cmp:(bool ->
               int ->
                 int ->
                   int ->
                     int ->
                       int ->
                         int ->
                           bool ->
                             int -> int -> int -> int -> int -> int -> bool))
       (x_DO_NOT_CARE_310:bool) (x_DO_NOT_CARE_311:int)
       (x_DO_NOT_CARE_312:int) (x_DO_NOT_CARE_313:int)
       (x_DO_NOT_CARE_314:int) (x_DO_NOT_CARE_315:int) (x:int)
       (x_DO_NOT_CARE_304:bool) (x_DO_NOT_CARE_305:int)
       (x_DO_NOT_CARE_306:int) (x_DO_NOT_CARE_307:int)
       (x_DO_NOT_CARE_308:int) (x_DO_NOT_CARE_309:int) (l:int)
       (x_DO_NOT_CARE_298:bool) (x_DO_NOT_CARE_299:int)
       (x_DO_NOT_CARE_300:int) (x_DO_NOT_CARE_301:int)
       (x_DO_NOT_CARE_302:int) (x_DO_NOT_CARE_303:int) (r:int)
       (prev_set_flag_par_255:bool) (s_prev_par_cmp_EXPARAM_249:int)
       (s_prev_par_x_251:int) (s_prev_par_l_252:int) (s_prev_par_r_253:int)
       (s_prev_par_xs_254:int) (xs:int) =
  if prev_set_flag_par_255
  then
    if
      s_prev_par_xs_254 > xs && xs >= 0 ||
      (s_prev_par_xs_254 >= xs && (s_prev_par_r_253 > r && r >= 0) ||
       s_prev_par_r_253 >= r &&
       (s_prev_par_xs_254 >= xs && (s_prev_par_l_252 > l && l >= 0)))
    then
      ()
    else
      assert false;
  par_without_checking_296
    x_DO_NOT_CARE_322 x_DO_NOT_CARE_323 x_DO_NOT_CARE_324 x_DO_NOT_CARE_325
    x_DO_NOT_CARE_326 x_DO_NOT_CARE_327 cmp_EXPARAM_205 x_DO_NOT_CARE_316
    x_DO_NOT_CARE_317 x_DO_NOT_CARE_318 x_DO_NOT_CARE_319 x_DO_NOT_CARE_320
    x_DO_NOT_CARE_321 cmp x_DO_NOT_CARE_310 x_DO_NOT_CARE_311
    x_DO_NOT_CARE_312 x_DO_NOT_CARE_313 x_DO_NOT_CARE_314 x_DO_NOT_CARE_315 
    x x_DO_NOT_CARE_304 x_DO_NOT_CARE_305 x_DO_NOT_CARE_306 x_DO_NOT_CARE_307
    x_DO_NOT_CARE_308 x_DO_NOT_CARE_309 l x_DO_NOT_CARE_298 x_DO_NOT_CARE_299
    x_DO_NOT_CARE_300 x_DO_NOT_CARE_301 x_DO_NOT_CARE_302 x_DO_NOT_CARE_303 
    r prev_set_flag_par_255 s_prev_par_cmp_EXPARAM_249 s_prev_par_x_251
    s_prev_par_l_252 s_prev_par_r_253 s_prev_par_xs_254 xs
and par_without_checking_296 (_:bool) (_:int) (_:int) (_:int) (_:int) 
                            (_:int) (cmp_EXPARAM_205:int) (_:bool) (_:int)
                            (_:int) (_:int) (_:int) (_:int)
                            (cmp:(bool ->
                                    int ->
                                      int ->
                                        int ->
                                          int ->
                                            int ->
                                              int ->
                                                bool ->
                                                  int ->
                                                    int ->
                                                      int ->
                                                        int ->
                                                          int -> int -> bool))
                            (_:bool) (_:int) (_:int) (_:int) (_:int) 
                            (_:int) (x:int) (_:bool) (_:int) (_:int) 
                            (_:int) (_:int) (_:int) (l:int) (_:bool) 
                            (_:int) (_:int) (_:int) (_:int) (_:int) (r:int)
                            (_:bool) (_:int) (_:int) (_:int) (_:int) 
                            (_:int) (xs:int) =
  let set_flag_par_256 = true
  in
  let s_par_xs_248 = xs
  in
  let s_par_r_247 = r
  in
  let s_par_l_246 = l
  in
  let s_par_x_245 = x
  in
  let s_par_cmp_EXPARAM_243 = cmp_EXPARAM_205
  in
  if xs <= 0
  then
    qs
      set_flag_par_256 s_par_cmp_EXPARAM_243 s_par_x_245 s_par_l_246
      s_par_r_247 s_par_xs_248
      (c39_COEFFICIENT_240 * xs +
       (c38_COEFFICIENT_239 * r +
        (c37_COEFFICIENT_238 * l +
         (c36_COEFFICIENT_237 * x +
          (c35_COEFFICIENT_236 * cmp_EXPARAM_205 + c34_COEFFICIENT_235)))))
      set_flag_par_256 s_par_cmp_EXPARAM_243 s_par_x_245 s_par_l_246
      s_par_r_247 s_par_xs_248 cmp set_flag_par_256 s_par_cmp_EXPARAM_243
      s_par_x_245 s_par_l_246 s_par_r_247 s_par_xs_248 l
    +
    (1 +
     qs
       set_flag_par_256 s_par_cmp_EXPARAM_243 s_par_x_245 s_par_l_246
       s_par_r_247 s_par_xs_248
       (c33_COEFFICIENT_233 * xs +
        (c32_COEFFICIENT_232 * r +
         (c31_COEFFICIENT_231 * l +
          (c30_COEFFICIENT_230 * x +
           (c29_COEFFICIENT_229 * cmp_EXPARAM_205 + c28_COEFFICIENT_228)))))
       set_flag_par_256 s_par_cmp_EXPARAM_243 s_par_x_245 s_par_l_246
       s_par_r_247 s_par_xs_248 cmp set_flag_par_256 s_par_cmp_EXPARAM_243
       s_par_x_245 s_par_l_246 s_par_r_247 s_par_xs_248 r)
  else
    let x' = Random.int 0
    in
    let xs' = xs - 1
    in
    if
      cmp
        set_flag_par_256 s_par_cmp_EXPARAM_243 s_par_x_245 s_par_l_246
        s_par_r_247 s_par_xs_248 x' set_flag_par_256 s_par_cmp_EXPARAM_243
        s_par_x_245 s_par_l_246 s_par_r_247 s_par_xs_248 x
    then
      par_without_checking_296
        set_flag_par_256 s_par_cmp_EXPARAM_243 s_par_x_245 s_par_l_246
        s_par_r_247 s_par_xs_248
        (c27_COEFFICIENT_226 * xs' +
         (c26_COEFFICIENT_225 * xs' +
          (c25_COEFFICIENT_224 * x' +
           (c24_COEFFICIENT_223 * x' +
            (c23_COEFFICIENT_222 * xs +
             (c22_COEFFICIENT_221 * r +
              (c21_COEFFICIENT_220 * l +
               (c20_COEFFICIENT_219 * x +
                (c19_COEFFICIENT_218 * cmp_EXPARAM_205 + c18_COEFFICIENT_217)))))))))
        set_flag_par_256 s_par_cmp_EXPARAM_243 s_par_x_245 s_par_l_246
        s_par_r_247 s_par_xs_248 cmp set_flag_par_256 s_par_cmp_EXPARAM_243
        s_par_x_245 s_par_l_246 s_par_r_247 s_par_xs_248 x set_flag_par_256
        s_par_cmp_EXPARAM_243 s_par_x_245 s_par_l_246 s_par_r_247
        s_par_xs_248 (1 + l) set_flag_par_256 s_par_cmp_EXPARAM_243
        s_par_x_245 s_par_l_246 s_par_r_247 s_par_xs_248 r set_flag_par_256
        s_par_cmp_EXPARAM_243 s_par_x_245 s_par_l_246 s_par_r_247
        s_par_xs_248 xs'
    else
      par_without_checking_296
        set_flag_par_256 s_par_cmp_EXPARAM_243 s_par_x_245 s_par_l_246
        s_par_r_247 s_par_xs_248
        (c17_COEFFICIENT_215 * xs' +
         (c16_COEFFICIENT_214 * xs' +
          (c15_COEFFICIENT_213 * x' +
           (c14_COEFFICIENT_212 * x' +
            (c13_COEFFICIENT_211 * xs +
             (c12_COEFFICIENT_210 * r +
              (c11_COEFFICIENT_209 * l +
               (c10_COEFFICIENT_208 * x +
                (c9_COEFFICIENT_207 * cmp_EXPARAM_205 + c8_COEFFICIENT_206)))))))))
        set_flag_par_256 s_par_cmp_EXPARAM_243 s_par_x_245 s_par_l_246
        s_par_r_247 s_par_xs_248 cmp set_flag_par_256 s_par_cmp_EXPARAM_243
        s_par_x_245 s_par_l_246 s_par_r_247 s_par_xs_248 x set_flag_par_256
        s_par_cmp_EXPARAM_243 s_par_x_245 s_par_l_246 s_par_r_247
        s_par_xs_248 l set_flag_par_256 s_par_cmp_EXPARAM_243 s_par_x_245
        s_par_l_246 s_par_r_247 s_par_xs_248 (1 + r) set_flag_par_256
        s_par_cmp_EXPARAM_243 s_par_x_245 s_par_l_246 s_par_r_247
        s_par_xs_248 xs'
let cmp (_:bool) (_:int) (_:int) (_:int) (_:int) (_:int) (x:int) (_:bool)
       (_:int) (_:int) (_:int) (_:int) (_:int) (y:int) = x >= y
let main (set_flag_par_256:bool) (s_par_cmp_EXPARAM_243:int)
        (s_par_x_245:int) (s_par_l_246:int) (s_par_r_247:int)
        (s_par_xs_248:int) (():unit) =
  let n = Random.int 0
  in
  qs
    set_flag_par_256 s_par_cmp_EXPARAM_243 s_par_x_245 s_par_l_246
    s_par_r_247 s_par_xs_248
    (c2_COEFFICIENT_195 * n + (c1_COEFFICIENT_194 * n + c0_COEFFICIENT_193))
    set_flag_par_256 s_par_cmp_EXPARAM_243 s_par_x_245 s_par_l_246
    s_par_r_247 s_par_xs_248 cmp set_flag_par_256 s_par_cmp_EXPARAM_243
    s_par_x_245 s_par_l_246 s_par_r_247 s_par_xs_248 n
let u_117272 = main false 0 0 0 0 0 ()
