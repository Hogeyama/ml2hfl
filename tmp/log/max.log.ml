MoCHi: Model Checker for Higher-Order Programs
  Build: c81d999 (2014-06-27 16:26:06 +0900)
  TRecS version: 1.33
  OCaml version: 4.01.0
  Command: ./mochi.opt test/max.ml -debug 2

parsed:
 let max_1008 max2_1009 x_1029 =
   match x_1029 with
   | (_ as x_1010) ->
       fun x_1031 ->
         (match x_1031 with
          | (_ as y_1011) ->
              fun x_1032 -> (match x_1032 with
                             | (_ as z_1012) -> max2_1009 (max2_1009 x_1010 y_1011) z_1012))
 in
 let f_1013 x_1014 y_1015 = if x_1014 >= y_1015 then
                              x_1014
                            else
                              y_1015 in
 let main_1016 x_1052 =
   match x_1052 with
   | (_ as x_1017) ->
       fun y_1018 ->
         fun z_1019 ->
           (let m_1020 = max_1008 f_1013 x_1017 y_1018 z_1019 in
            if f_1013 x_1017 m_1020 = m_1020 then
              ()
            else
              {fail} ())
 in
 ()

set_target:
 let max_1008 (max2_1009:(int -> int -> int)) (x_1029:int) =
   match x_1029 with
   | (_ as x_1010) ->
       fun (x_1031:int) ->
         (match x_1031 with
          | (_ as y_1011) ->
              fun (x_1032:int) -> (match x_1032 with
                                   | (_ as z_1012) -> max2_1009 (max2_1009 x_1010 y_1011) z_1012))
 in
 let f_1013 (x_1014:int) (y_1015:int) = if x_1014 >= y_1015 then
                                          x_1014
                                        else
                                          y_1015 in
 let main_1016 (x_1052:int) =
   match x_1052 with
   | (_ as x_1017) ->
       fun (y_1018:int) ->
         fun (z_1019:int) ->
           (let m_1020 = max_1008 f_1013 x_1017 y_1018 z_1019 in
            if f_1013 x_1017 m_1020 = m_1020 then
              ()
            else
              {fail} ())
 in
 let main_1077 =
   let arg1_1071 = rand_int () in
   let arg2_1073 = rand_int () in
   let arg3_1075 = rand_int () in
   main_1016 arg1_1071 arg2_1073 arg3_1075
 in
 ()

encode_list:
 let max_1008 (max2_1009:(int -> int -> int)) (x_1029:int) =
   fun (x_1031:int) -> fun (x_1032:int) -> max2_1009 (max2_1009 x_1029 x_1031) x_1032
 in
 let f_1013 (x_1014:int) (y_1015:int) = if x_1014 >= y_1015 then
                                          x_1014
                                        else
                                          y_1015 in
 let main_1016 (x_1052:int) =
   fun (y_1018:int) ->
     fun (z_1019:int) ->
       (let m_1020 = max_1008 f_1013 x_1052 y_1018 z_1019 in
        if f_1013 x_1052 m_1020 = m_1020 then
          ()
        else
          {fail} ())
 in
 let main_1077 =
   let arg1_1071 = rand_int () in
   let arg2_1073 = rand_int () in
   let arg3_1075 = rand_int () in
   main_1016 arg1_1071 arg2_1073 arg3_1075
 in
 ()

inlined:
 let max_1008 (max2_1009:(int -> int -> int)) (x_1029:int) (x_1031:int) (x_1032:int) =
   max2_1009 (max2_1009 x_1029 x_1031) x_1032
 in
 let f_1013 (x_1014:int) (y_1015:int) = if x_1014 >= y_1015 then
                                          x_1014
                                        else
                                          y_1015 in
 let main_1016 (x_1052:int) (y_1018:int) (z_1019:int) =
   let m_1020 = max_1008 f_1013 x_1052 y_1018 z_1019 in
   if f_1013 x_1052 m_1020 = m_1020 then
     ()
   else
     {fail} ()
 in
 let main_1077 =
   let arg1_1071 = rand_int () in
   let arg2_1073 = rand_int () in
   let arg3_1075 = rand_int () in
   main_1016 arg1_1071 arg2_1073 arg3_1075
 in
 ()

CPS:
 let
   max_1008 (max2_1009:(int -> int -> (int -> X) -> X)) (x_1029:int) (x_1031:int) (x_1032:int) (k_max_1112:(int -> X)) =
   max2_1009 x_1029 x_1031 (fun (x_1305:int) -> max2_1009 x_1305 x_1032 k_max_1112)
 in
 let f_1013 (x_1014:int) (y_1015:int) (k_f_1159:(int -> X)) =
   if x_1014 >= y_1015 then
     k_f_1159 x_1014
   else
     k_f_1159 y_1015
 in
 let main_1016 (x_1052:int) (y_1018:int) (z_1019:int) (k_main_1171:(unit -> X)) =
   let m_1020 (k_main_m_1189:(int -> X)) = max_1008 f_1013 x_1052 y_1018 z_1019 k_main_m_1189 in
   m_1020
     (fun (m_1221:int) ->
        f_1013 x_1052 m_1221
          (fun (x_1308:int) -> (if x_1308 = m_1221 then
                                  k_main_1171 ()
                                else
                                  {|fail|} () k_main_1171)))
 in
 let main_1077 (k_main_1230:(unit -> X)) =
   let arg1_1071 (k_main_arg1_1235:(int -> X)) = rand_int_cps () k_main_arg1_1235 in
   arg1_1071
     (fun (arg1_1279:int) ->
        (let arg2_1073 (k_main_arg2_1247:(int -> X)) = rand_int_cps () k_main_arg2_1247 in
         arg2_1073
           (fun (arg2_1278:int) ->
              (let arg3_1075 (k_main_arg3_1259:(int -> X)) = rand_int_cps () k_main_arg3_1259 in
               arg3_1075 (fun (arg3_1277:int) -> main_1016 arg1_1279 arg2_1278 arg3_1277 k_main_1230)))))
 in
 main_1077 (fun (main_1280:unit) -> {end})

Program with abstraction types (CEGAR-cycle 0)::
Main: main_1321
  main_1321 -> (main_1077 f_1328)
  arg1_1071 k_main_arg1_1235 -> (rand_int k_main_arg1_1235)
  arg2_1073 arg1_1279 k_main_arg2_1247 -> (rand_int k_main_arg2_1247)
  arg3_1075 arg1_1279 arg2_1278 k_main_arg3_1259 -> (rand_int k_main_arg3_1259)
  f_1013 x_1014 y_1015 k_f_1159 when (x_1014 >= y_1015) -> (k_f_1159 x_1014)
  f_1013 x_1014 y_1015 k_f_1159 when (not (x_1014 >= y_1015)) -> (k_f_1159 y_1015)
  f_1328 main_1280 -> end
  f_main_1323 x_1052 y_1018 z_1019 k_main_1171 m_1221 ->
      (f_1013 x_1052 m_1221 (f_main_1324 m_1221 x_1052 y_1018 z_1019 k_main_1171))
  f_main_1324 m_1221 x_1052 y_1018 z_1019 k_main_1171 x_1308 when (x_1308 = m_1221) -> (k_main_1171 ())
  f_main_1324 m_1221 x_1052 y_1018 z_1019 k_main_1171 x_1308 when (not (x_1308 = m_1221)) ->
      (fail_1329 true k_main_1171)
  f_main_1325 k_main_1230 arg1_1279 -> (arg2_1073 arg1_1279 (f_main_1326 arg1_1279 k_main_1230))
  f_main_1326 arg1_1279 k_main_1230 arg2_1278 ->
      (arg3_1075 arg1_1279 arg2_1278 (f_main_1327 arg1_1279 arg2_1278 k_main_1230))
  f_main_1327 arg1_1279 arg2_1278 k_main_1230 arg3_1277 -> (main_1016 arg1_1279 arg2_1278 arg3_1277 k_main_1230)
  f_max_1322 x_1029 x_1031 x_1032 k_max_1112 max2_1009 x_1305 -> (max2_1009 x_1305 x_1032 k_max_1112)
  fail_1329 b k -> {fail} => (k ())
  m_1020 x_1052 y_1018 z_1019 k_main_m_1189 -> (max_1008 f_1013 x_1052 y_1018 z_1019 k_main_m_1189)
  main_1016 x_1052 y_1018 z_1019 k_main_1171 ->
      (m_1020 x_1052 y_1018 z_1019 (f_main_1323 x_1052 y_1018 z_1019 k_main_1171))
  main_1077 k_main_1230 -> (arg1_1071 (f_main_1325 k_main_1230))
  max_1008 max2_1009 x_1029 x_1031 x_1032 k_max_1112 ->
      (max2_1009 x_1029 x_1031 (f_max_1322 x_1029 x_1031 x_1032 k_max_1112 max2_1009))
Types:
  main_1321 : X
  fail_1329 : (x_1:bool[x_1] -> (unit -> X) -> X)

(0-1) Abstracting ... Fatal error: exception ExtList.List.Empty_list
