let rec f (set_flag_lambda_54:bool) (s_lambda_x_51:int) (x:int) =
  if x > 0 then f set_flag_lambda_54 s_lambda_x_51 (x - 1) else lambda
and lambda (prev_set_flag_lambda_53:bool) (s_prev_lambda_x_52:int)
          (x:int) =
  if prev_set_flag_lambda_53 then assert false;
  lambda_without_checking_69
    prev_set_flag_lambda_53 s_prev_lambda_x_52 x
and lambda_without_checking_69 (_:bool) (_:int) (x:int) =
  let set_flag_lambda_54 = true
  in
  let s_lambda_x_51 = x
  in
  x + 1
let g = f false 0 1
let main (set_flag_lambda_54:bool) (s_lambda_x_51:int) (():unit) =
  g set_flag_lambda_54 s_lambda_x_51 2
let u_218 = main false 0 ()
