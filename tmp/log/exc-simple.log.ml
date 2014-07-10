MoCHi: Model Checker for Higher-Order Programs
  Build: c81d999 (2014-06-27 16:26:06 +0900)
  TRecS version: 1.33
  OCaml version: 4.01.0
  Command: ./mochi.opt test/exc-simple.ml -debug 2

parsed:
 let f_1008 n_1009 k_1010 = if n_1009 >= 0 then
                              ()
                            else
                              k_1010 0 in
 let g_1011 n_1012 = if n_1012 = 0 then
                       ()
                     else
                       {fail} () in
 let main_1013 n_1014 = f_1008 n_1014 g_1011 in
 ()

set_target:
 let f_1008 (n_1009:int) (k_1010:(int -> unit)) = if n_1009 >= 0 then
                                                    ()
                                                  else
                                                    k_1010 0 in
 let g_1011 (n_1012:int) = if n_1012 = 0 then
                             ()
                           else
                             {fail} () in
 let main_1013 (n_1014:int) = f_1008 n_1014 g_1011 in
 let main_1039 = let arg1_1037 = rand_int () in
                 main_1013 arg1_1037 in
 ()

CPS:
 let f_1008 (n_1009:int) (k_1010:(int -> (unit -> X) -> X)) (k_f_1069:(unit -> X)) =
   if n_1009 >= 0 then
     k_f_1069 ()
   else
     k_1010 0 k_f_1069
 in
 let g_1011 (n_1012:int) (k_g_1094:(unit -> X)) = if n_1012 = 0 then
                                                    k_g_1094 ()
                                                  else
                                                    {|fail|} () k_g_1094 in
 let main_1013 (n_1014:int) (k_main_1112:(unit -> X)) = f_1008 n_1014 g_1011 k_main_1112 in
 let main_1039 (k_main_1142:(unit -> X)) =
   let arg1_1037 (k_main_arg1_1147:(int -> X)) = rand_int_cps () k_main_arg1_1147 in
   arg1_1037 (fun (arg1_1163:int) -> main_1013 arg1_1163 k_main_1142)
 in
 main_1039 (fun (main_1164:unit) -> {end})

Program with abstraction types (CEGAR-cycle 0)::
Main: main_1186
  main_1186 -> (main_1039 f_1188)
  arg1_1037 k_main_arg1_1147 -> (rand_int k_main_arg1_1147)
  f_1008 n_1009 k_1010 k_f_1069 when (n_1009 >= 0) -> (k_f_1069 ())
  f_1008 n_1009 k_1010 k_f_1069 when (not (n_1009 >= 0)) -> (k_1010 0 k_f_1069)
  f_1188 main_1164 -> end
  f_main_1187 k_main_1142 arg1_1163 -> (main_1013 arg1_1163 k_main_1142)
  fail_1189 b k -> {fail} => (k ())
  g_1011 n_1012 k_g_1094 when (n_1012 = 0) -> (k_g_1094 ())
  g_1011 n_1012 k_g_1094 when (not (n_1012 = 0)) -> (fail_1189 true k_g_1094)
  main_1013 n_1014 k_main_1112 -> (f_1008 n_1014 g_1011 k_main_1112)
  main_1039 k_main_1142 -> (arg1_1037 (f_main_1187 k_main_1142))
Types:
  main_1186 : X
  fail_1189 : (x_1:bool[x_1] -> (unit -> X) -> X)

(0-1) Abstracting ... Fatal error: exception Division_by_zero
