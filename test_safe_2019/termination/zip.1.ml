let rec zip (x_DO_NOT_CARE_73:bool) (x_DO_NOT_CARE_74:int)
           (x_DO_NOT_CARE_75:int) (xs:int) (prev_set_flag_zip_56:bool)
           (s_prev_zip_xs_54:int) (s_prev_zip_ys_55:int) (ys:int) =
  if prev_set_flag_zip_56 then assert false;
  zip_without_checking_71
    x_DO_NOT_CARE_73 x_DO_NOT_CARE_74 x_DO_NOT_CARE_75 xs
    prev_set_flag_zip_56 s_prev_zip_xs_54 s_prev_zip_ys_55 ys
and zip_without_checking_71 (_:bool) (_:int) (_:int) (xs:int) (_:bool)
                           (_:int) (_:int) (ys:int) =
  let set_flag_zip_57 = true
  in
  let s_zip_ys_53 = ys
  in
  let s_zip_xs_52 = xs
  in
  if xs <= 0
  then
    0
  else
    let xs' = xs - 1
    in
    if ys <= 0
    then
      0
    else
      let ys' = ys - 1
      in
      1 +
      zip_without_checking_71
        set_flag_zip_57 s_zip_xs_52 s_zip_ys_53 xs' set_flag_zip_57
        s_zip_xs_52 s_zip_ys_53 ys'
let main (set_flag_zip_57:bool) (s_zip_xs_52:int) (s_zip_ys_53:int)
        (():unit) =
  let l1 = Random.int 0
  in
  let l2 = Random.int 0
  in
  zip
    set_flag_zip_57 s_zip_xs_52 s_zip_ys_53 l1 set_flag_zip_57
    s_zip_xs_52 s_zip_ys_53 l2
let u_264 = main false 0 0 ()
