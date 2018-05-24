let c1_COEFFICIENT_82 = 0
let c0_COEFFICIENT_81 = 0
let rec succ (prev_set_flag_succ_88:bool) (s_prev_succ_n_87:int) (n:int) =
  if prev_set_flag_succ_88 then assert false;
  succ_without_checking_113 prev_set_flag_succ_88 s_prev_succ_n_87 n
and succ_without_checking_113 (_:bool) (_:int) (n:int) =
  let set_flag_succ_89 = true
  in
  let s_succ_n_86 = n
  in
  n + 1
let g (_:bool) (_:int) (_:int) (_:bool) (_:int)
     (r:(bool -> int -> int -> int)) (set_flag_succ_89:bool)
     (s_succ_n_86:int) (a:int) =
  r set_flag_succ_89 s_succ_n_86 (r set_flag_succ_89 s_succ_n_86 a)
let rec f (set_flag_succ_89:bool) (s_succ_n_86:int) (n:int) =
  if n = 0
  then
    succ
  else
    g
      set_flag_succ_89 s_succ_n_86
      (c1_COEFFICIENT_82 * n + c0_COEFFICIENT_81) set_flag_succ_89
      s_succ_n_86 (f set_flag_succ_89 s_succ_n_86 (n - 1))
let main (_:bool) (_:int) (n:int) (set_flag_succ_89:bool)
        (s_succ_n_86:int) (x:int) =
  if n >= 0 && x >= 0
  then
    f set_flag_succ_89 s_succ_n_86 n set_flag_succ_89 s_succ_n_86 x
  else
    0
let u_491 = main false 0 (Random.int 0) false 0 (Random.int 0)
