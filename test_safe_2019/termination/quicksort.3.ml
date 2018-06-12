let rec qs (_:bool) (_:int) (_:int) (_:int) (_:int) (_:int)
          (cmp_EXPARAM_198:int) (_:bool) (_:int) (_:int) (_:int) (_:int)
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
          (set_flag_par_257:bool) (s_par_cmp_EXPARAM_244:int)
          (s_par_x_246:int) (s_par_l_247:int) (s_par_r_248:int)
          (s_par_xs_249:int) (n:int) =
  if n <= 0
  then
    0
  else
    let xs' = n - 1
    in
    par
      set_flag_par_257 s_par_cmp_EXPARAM_244 s_par_x_246 s_par_l_247
      s_par_r_248 s_par_xs_249
      (0 * xs' + (0 * xs' + (0 * n + (0 * cmp_EXPARAM_198 + 0))))
      set_flag_par_257 s_par_cmp_EXPARAM_244 s_par_x_246 s_par_l_247
      s_par_r_248 s_par_xs_249 cmp set_flag_par_257 s_par_cmp_EXPARAM_244
      s_par_x_246 s_par_l_247 s_par_r_248 s_par_xs_249 (Random.int 0)
      set_flag_par_257 s_par_cmp_EXPARAM_244 s_par_x_246 s_par_l_247
      s_par_r_248 s_par_xs_249 0 set_flag_par_257 s_par_cmp_EXPARAM_244
      s_par_x_246 s_par_l_247 s_par_r_248 s_par_xs_249 0 set_flag_par_257
      s_par_cmp_EXPARAM_244 s_par_x_246 s_par_l_247 s_par_r_248 s_par_xs_249
      xs'
and par (x_DO_NOT_CARE_323:bool) (x_DO_NOT_CARE_324:int)
       (x_DO_NOT_CARE_325:int) (x_DO_NOT_CARE_326:int)
       (x_DO_NOT_CARE_327:int) (x_DO_NOT_CARE_328:int) (cmp_EXPARAM_206:int)
       (x_DO_NOT_CARE_317:bool) (x_DO_NOT_CARE_318:int)
       (x_DO_NOT_CARE_319:int) (x_DO_NOT_CARE_320:int)
       (x_DO_NOT_CARE_321:int) (x_DO_NOT_CARE_322:int)
       (cmp:(bool ->
               int ->
                 int ->
                   int ->
                     int ->
                       int ->
                         int ->
                           bool ->
                             int -> int -> int -> int -> int -> int -> bool))
       (x_DO_NOT_CARE_311:bool) (x_DO_NOT_CARE_312:int)
       (x_DO_NOT_CARE_313:int) (x_DO_NOT_CARE_314:int)
       (x_DO_NOT_CARE_315:int) (x_DO_NOT_CARE_316:int) (x:int)
       (x_DO_NOT_CARE_305:bool) (x_DO_NOT_CARE_306:int)
       (x_DO_NOT_CARE_307:int) (x_DO_NOT_CARE_308:int)
       (x_DO_NOT_CARE_309:int) (x_DO_NOT_CARE_310:int) (l:int)
       (x_DO_NOT_CARE_299:bool) (x_DO_NOT_CARE_300:int)
       (x_DO_NOT_CARE_301:int) (x_DO_NOT_CARE_302:int)
       (x_DO_NOT_CARE_303:int) (x_DO_NOT_CARE_304:int) (r:int)
       (prev_set_flag_par_256:bool) (s_prev_par_cmp_EXPARAM_250:int)
       (s_prev_par_x_252:int) (s_prev_par_l_253:int) (s_prev_par_r_254:int)
       (s_prev_par_xs_255:int) (xs:int) =
  if prev_set_flag_par_256
  then
    if
      s_prev_par_r_254 > r && r >= 0 ||
      s_prev_par_r_254 >= r && (s_prev_par_l_253 > l && l >= 0)
    then
      ()
    else
      assert false;
  par_without_checking_297
    x_DO_NOT_CARE_323 x_DO_NOT_CARE_324 x_DO_NOT_CARE_325 x_DO_NOT_CARE_326
    x_DO_NOT_CARE_327 x_DO_NOT_CARE_328 cmp_EXPARAM_206 x_DO_NOT_CARE_317
    x_DO_NOT_CARE_318 x_DO_NOT_CARE_319 x_DO_NOT_CARE_320 x_DO_NOT_CARE_321
    x_DO_NOT_CARE_322 cmp x_DO_NOT_CARE_311 x_DO_NOT_CARE_312
    x_DO_NOT_CARE_313 x_DO_NOT_CARE_314 x_DO_NOT_CARE_315 x_DO_NOT_CARE_316 
    x x_DO_NOT_CARE_305 x_DO_NOT_CARE_306 x_DO_NOT_CARE_307 x_DO_NOT_CARE_308
    x_DO_NOT_CARE_309 x_DO_NOT_CARE_310 l x_DO_NOT_CARE_299 x_DO_NOT_CARE_300
    x_DO_NOT_CARE_301 x_DO_NOT_CARE_302 x_DO_NOT_CARE_303 x_DO_NOT_CARE_304 
    r prev_set_flag_par_256 s_prev_par_cmp_EXPARAM_250 s_prev_par_x_252
    s_prev_par_l_253 s_prev_par_r_254 s_prev_par_xs_255 xs
and par_without_checking_297 (_:bool) (_:int) (_:int) (_:int) (_:int) 
                            (_:int) (cmp_EXPARAM_206:int) (_:bool) (_:int)
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
  let set_flag_par_257 = true
  in
  let s_par_xs_249 = xs
  in
  let s_par_r_248 = r
  in
  let s_par_l_247 = l
  in
  let s_par_x_246 = x
  in
  let s_par_cmp_EXPARAM_244 = cmp_EXPARAM_206
  in
  if xs <= 0
  then
    qs
      set_flag_par_257 s_par_cmp_EXPARAM_244 s_par_x_246 s_par_l_247
      s_par_r_248 s_par_xs_249
      (0 * xs + (0 * r + (0 * l + (0 * x + (0 * cmp_EXPARAM_206 + 0)))))
      set_flag_par_257 s_par_cmp_EXPARAM_244 s_par_x_246 s_par_l_247
      s_par_r_248 s_par_xs_249 cmp set_flag_par_257 s_par_cmp_EXPARAM_244
      s_par_x_246 s_par_l_247 s_par_r_248 s_par_xs_249 l
    +
    (1 +
     qs
       set_flag_par_257 s_par_cmp_EXPARAM_244 s_par_x_246 s_par_l_247
       s_par_r_248 s_par_xs_249
       (0 * xs + (0 * r + (0 * l + (0 * x + (0 * cmp_EXPARAM_206 + 0)))))
       set_flag_par_257 s_par_cmp_EXPARAM_244 s_par_x_246 s_par_l_247
       s_par_r_248 s_par_xs_249 cmp set_flag_par_257 s_par_cmp_EXPARAM_244
       s_par_x_246 s_par_l_247 s_par_r_248 s_par_xs_249 r)
  else
    let x' = Random.int 0
    in
    let xs' = xs - 1
    in
    if
      cmp
        set_flag_par_257 s_par_cmp_EXPARAM_244 s_par_x_246 s_par_l_247
        s_par_r_248 s_par_xs_249 x' set_flag_par_257 s_par_cmp_EXPARAM_244
        s_par_x_246 s_par_l_247 s_par_r_248 s_par_xs_249 x
    then
      par_without_checking_297
        set_flag_par_257 s_par_cmp_EXPARAM_244 s_par_x_246 s_par_l_247
        s_par_r_248 s_par_xs_249
        (0 * xs' +
         (0 * xs' +
          (0 * x' +
           (0 * x' +
            (0 * xs + (0 * r + (0 * l + (0 * x + (0 * cmp_EXPARAM_206 + 0)))))))))
        set_flag_par_257 s_par_cmp_EXPARAM_244 s_par_x_246 s_par_l_247
        s_par_r_248 s_par_xs_249 cmp set_flag_par_257 s_par_cmp_EXPARAM_244
        s_par_x_246 s_par_l_247 s_par_r_248 s_par_xs_249 x set_flag_par_257
        s_par_cmp_EXPARAM_244 s_par_x_246 s_par_l_247 s_par_r_248
        s_par_xs_249 (1 + l) set_flag_par_257 s_par_cmp_EXPARAM_244
        s_par_x_246 s_par_l_247 s_par_r_248 s_par_xs_249 r set_flag_par_257
        s_par_cmp_EXPARAM_244 s_par_x_246 s_par_l_247 s_par_r_248
        s_par_xs_249 xs'
    else
      par_without_checking_297
        set_flag_par_257 s_par_cmp_EXPARAM_244 s_par_x_246 s_par_l_247
        s_par_r_248 s_par_xs_249
        (0 * xs' +
         (0 * xs' +
          (0 * x' +
           (0 * x' +
            (0 * xs + (0 * r + (0 * l + (0 * x + (0 * cmp_EXPARAM_206 + 0)))))))))
        set_flag_par_257 s_par_cmp_EXPARAM_244 s_par_x_246 s_par_l_247
        s_par_r_248 s_par_xs_249 cmp set_flag_par_257 s_par_cmp_EXPARAM_244
        s_par_x_246 s_par_l_247 s_par_r_248 s_par_xs_249 x set_flag_par_257
        s_par_cmp_EXPARAM_244 s_par_x_246 s_par_l_247 s_par_r_248
        s_par_xs_249 l set_flag_par_257 s_par_cmp_EXPARAM_244 s_par_x_246
        s_par_l_247 s_par_r_248 s_par_xs_249 (1 + r) set_flag_par_257
        s_par_cmp_EXPARAM_244 s_par_x_246 s_par_l_247 s_par_r_248
        s_par_xs_249 xs'
let cmp (_:bool) (_:int) (_:int) (_:int) (_:int) (_:int) (x:int) (_:bool)
       (_:int) (_:int) (_:int) (_:int) (_:int) (y:int) = x >= y
let main (set_flag_par_257:bool) (s_par_cmp_EXPARAM_244:int)
        (s_par_x_246:int) (s_par_l_247:int) (s_par_r_248:int)
        (s_par_xs_249:int) (():unit) =
  let n = Random.int 0
  in
  qs
    set_flag_par_257 s_par_cmp_EXPARAM_244 s_par_x_246 s_par_l_247
    s_par_r_248 s_par_xs_249 (0 * n + (0 * n + 0)) set_flag_par_257
    s_par_cmp_EXPARAM_244 s_par_x_246 s_par_l_247 s_par_r_248 s_par_xs_249
    cmp set_flag_par_257 s_par_cmp_EXPARAM_244 s_par_x_246 s_par_l_247
    s_par_r_248 s_par_xs_249 n
let u_44883 = main false 0 0 0 0 0 ()
