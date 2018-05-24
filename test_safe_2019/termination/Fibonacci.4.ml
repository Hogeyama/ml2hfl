let rec fib (prev_set_flag_fib_42:bool) (s_prev_fib_n_41:int) (n:int) =
  if prev_set_flag_fib_42
  then
    if s_prev_fib_n_41 > n && n >= 0 then () else assert false;
  fib_without_checking_51 prev_set_flag_fib_42 s_prev_fib_n_41 n
and fib_without_checking_51 (_:bool) (_:int) (n:int) =
  let set_flag_fib_43 = true
  in
  let s_fib_n_40 = n
  in
  if n < 2
  then
    1
  else
    fib_without_checking_51 set_flag_fib_43 s_fib_n_40 (n - 1) +
    fib set_flag_fib_43 s_fib_n_40 (n - 2)
let main (set_flag_fib_43:bool) (s_fib_n_40:int) (():unit) =
  fib_without_checking_51 set_flag_fib_43 s_fib_n_40 (Random.int 0)
let u_2181 = main false 0 ()
