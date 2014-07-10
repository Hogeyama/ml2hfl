MoCHi: Model Checker for Higher-Order Programs
  Build: c81d999 (2014-06-27 16:26:06 +0900)
  TRecS version: 1.33
  OCaml version: 4.01.0
  Command: ./mochi.opt test/lock.ml -debug 2

parsed:
 let lock_1008 st_1009 = let u_1014 = if st_1009 = 0 then
                                        ()
                                      else
                                        {fail} () in
                         1 in
 let unlock_1010 st_1011 = let u_1020 = if st_1011 = 1 then
                                          ()
                                        else
                                          {fail} () in
                           0 in
 let f_1012 n_1013 st_1014 = if n_1013 > 0 then
                               lock_1008 st_1014
                             else
                               st_1014 in
 let g_1015 n_1016 st_1017 = if n_1016 > 0 then
                               unlock_1010 st_1017
                             else
                               st_1017 in
 let main_1018 n_1019 = if g_1015 n_1019 (f_1012 n_1019 0) = 0 then
                          ()
                        else
                          {fail} () in
 ()

set_target:
 let lock_1008 (st_1009:int) = let u_1014 = if st_1009 = 0 then
                                              ()
                                            else
                                              {fail} () in
                               1 in
 let unlock_1010 (st_1011:int) = let u_1020 = if st_1011 = 1 then
                                                ()
                                              else
                                                {fail} () in
                                 0 in
 let f_1012 (n_1013:int) (st_1014:int) = if n_1013 > 0 then
                                           lock_1008 st_1014
                                         else
                                           st_1014 in
 let g_1015 (n_1016:int) (st_1017:int) = if n_1016 > 0 then
                                           unlock_1010 st_1017
                                         else
                                           st_1017 in
 let main_1018 (n_1019:int) = if g_1015 n_1019 (f_1012 n_1019 0) = 0 then
                                ()
                              else
                                {fail} () in
 let main_1054 = let arg1_1052 = rand_int () in
                 main_1018 arg1_1052 in
 ()

CPS:
 let lock_1008 (st_1009:int) (k_lock_1081:(int -> X)) =
   let u_1014 (k_lock_u_1092:(unit -> X)) = if st_1009 = 0 then
                                              k_lock_u_1092 ()
                                            else
                                              {|fail|} () k_lock_u_1092 in
   u_1014 (fun (u_1095:unit) -> k_lock_1081 1)
 in
 let unlock_1010 (st_1011:int) (k_unlock_1102:(int -> X)) =
   let u_1020 (k_unlock_u_1113:(unit -> X)) =
     if st_1011 = 1 then
       k_unlock_u_1113 ()
     else
       {|fail|} () k_unlock_u_1113
   in
   u_1020 (fun (u_1116:unit) -> k_unlock_1102 0)
 in
 let f_1012 (n_1013:int) (st_1014:int) (k_f_1123:(int -> X)) =
   if n_1013 > 0 then
     lock_1008 st_1014 k_f_1123
   else
     k_f_1123 st_1014
 in
 let g_1015 (n_1016:int) (st_1017:int) (k_g_1145:(int -> X)) =
   if n_1016 > 0 then
     unlock_1010 st_1017 k_g_1145
   else
     k_g_1145 st_1017
 in
 let main_1018 (n_1019:int) (k_main_1167:(unit -> X)) =
   f_1012 n_1019 0
     (fun (x_1258:int) ->
        g_1015 n_1019 x_1258 (fun (x_1260:int) -> (if x_1260 = 0 then
                                                     k_main_1167 ()
                                                   else
                                                     {|fail|} () k_main_1167)))
 in
 let main_1054 (k_main_1211:(unit -> X)) =
   let arg1_1052 (k_main_arg1_1216:(int -> X)) = rand_int_cps () k_main_arg1_1216 in
   arg1_1052 (fun (arg1_1232:int) -> main_1018 arg1_1232 k_main_1211)
 in
 main_1054 (fun (main_1233:unit) -> {end})

Program with abstraction types (CEGAR-cycle 0)::
Main: main_1269
  main_1269 -> (main_1054 f_1275)
  arg1_1052 k_main_arg1_1216 -> (rand_int k_main_arg1_1216)
  f_1012 n_1013 st_1014 k_f_1123 when (n_1013 > 0) -> (lock_1008 st_1014 k_f_1123)
  f_1012 n_1013 st_1014 k_f_1123 when (not (n_1013 > 0)) -> (k_f_1123 st_1014)
  f_1275 main_1233 -> end
  f_lock_1270 st_1009 k_lock_1081 u_1095 -> (k_lock_1081 1)
  f_main_1272 n_1019 k_main_1167 x_1258 -> (g_1015 n_1019 x_1258 (f_main_1273 n_1019 x_1258 k_main_1167))
  f_main_1273 n_1019 x_1258 k_main_1167 x_1260 when (x_1260 = 0) -> (k_main_1167 ())
  f_main_1273 n_1019 x_1258 k_main_1167 x_1260 when (not (x_1260 = 0)) -> (fail_1278 true k_main_1167)
  f_main_1274 k_main_1211 arg1_1232 -> (main_1018 arg1_1232 k_main_1211)
  f_unlock_1271 st_1011 k_unlock_1102 u_1116 -> (k_unlock_1102 0)
  fail_1276 b k -> {fail} => (k ())
  fail_1277 b k -> {fail} => (k ())
  fail_1278 b k -> {fail} => (k ())
  g_1015 n_1016 st_1017 k_g_1145 when (n_1016 > 0) -> (unlock_1010 st_1017 k_g_1145)
  g_1015 n_1016 st_1017 k_g_1145 when (not (n_1016 > 0)) -> (k_g_1145 st_1017)
  lock_1008 st_1009 k_lock_1081 -> (u_1014 st_1009 (f_lock_1270 st_1009 k_lock_1081))
  main_1018 n_1019 k_main_1167 -> (f_1012 n_1019 0 (f_main_1272 n_1019 k_main_1167))
  main_1054 k_main_1211 -> (arg1_1052 (f_main_1274 k_main_1211))
  u_1014 st_1009 k_lock_u_1092 when (st_1009 = 0) -> (k_lock_u_1092 ())
  u_1014 st_1009 k_lock_u_1092 when (not (st_1009 = 0)) -> (fail_1276 true k_lock_u_1092)
  u_1020 st_1011 k_unlock_u_1113 when (st_1011 = 1) -> (k_unlock_u_1113 ())
  u_1020 st_1011 k_unlock_u_1113 when (not (st_1011 = 1)) -> (fail_1277 true k_unlock_u_1113)
  unlock_1010 st_1011 k_unlock_1102 -> (u_1020 st_1011 (f_unlock_1271 st_1011 k_unlock_1102))
Types:
  main_1269 : X
  fail_1276 : (x_1:bool[x_1] -> (unit -> X) -> X)
  fail_1277 : (x_1:bool[x_1] -> (unit -> X) -> X)
  fail_1278 : (x_1:bool[x_1] -> (unit -> X) -> X)

(0-1) Abstracting ... Fatal error: exception Division_by_zero
