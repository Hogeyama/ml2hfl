MoCHi: Model Checker for Higher-Order Programs
  Build: _482925e (after 2014-07-29 09:55:42 +0900)
  FPAT version: 053139e
  TRecS version: 1.33
  OCaml version: 4.01.0
  Command: ./mochi.opt input.cegar -ignore-conf -debug-module CEGAR_abst_CPS,CEGAR_abst_util -abs-filter

Program with abstraction types (CEGAR-cycle 0)::
Main: main_1340
  main_1340 -> (main_1091 f_1346);;
  arg1_1087 k_main_arg1_1268 -> (rand_int k_main_arg1_1268);;
  arg2_1089 arg1_1298 k_main_arg2_1280 -> (rand_int k_main_arg2_1280);;
  array_max_1011 x_1028 i_1123 x_1124 m_1125 k_array_max_1153 when (i_1123 >= x_1028) -> (k_array_max_1153 m_1125);;
  array_max_1011 x_1028 i_1123 x_1124 m_1125 k_array_max_1153 when (not (i_1123 >= x_1028)) ->
      (x_1126 i_1123 m_1125 x_1028 x_1124 (f_array_max_1341 i_1123 m_1125 x_1028 k_array_max_1153 x_1124));;
  br_main_1347 b_1348 n_1019 i_1020 k_main_1209 when b_1348 -> (k_main_1216 i_1020 n_1019 k_main_1209 (i_1020 <= 0));;
  br_main_1347 b_1348 n_1019 i_1020 k_main_1209 when (not b_1348) -> (k_main_1216 i_1020 n_1019 k_main_1209 false);;
  f_1346 main_1299 -> end;;
  f_array_max_1341 i_1123 m_1125 x_1028 k_array_max_1153 x_1124 x_1192 ->
      (z_1127 i_1123 m_1125 x_1028 x_1192 (f_array_max_1342 i_1123 m_1125 x_1028 x_1192 k_array_max_1153 x_1124));;
  f_array_max_1342 i_1123 m_1125 x_1028 x_1192 k_array_max_1153 x_1124 z_1191 ->
      (array_max_1011 x_1028 (i_1123 + 1) x_1124 z_1191 k_array_max_1153);;
  f_k_main_1343 b_1322 i_1020 n_1019 k_main_1209 m_1328 when (m_1328 >= n_1019) -> (k_main_1209 ());;
  f_k_main_1343 b_1322 i_1020 n_1019 k_main_1209 m_1328 when (not (m_1328 >= n_1019)) -> (fail_1349 true k_main_1209);;
  f_main_1344 k_main_1263 arg1_1298 -> (arg2_1089 arg1_1298 (f_main_1345 arg1_1298 k_main_1263));;
  f_main_1345 arg1_1298 k_main_1263 arg2_1297 -> (main_1018 arg1_1298 arg2_1297 k_main_1263);;
  fail_1349 b k -> {fail} => (k ());;
  k_main_1216 i_1020 n_1019 k_main_1209 b_1322 when b_1322 ->
      (m_1324 b_1322 i_1020 n_1019 (f_k_main_1343 b_1322 i_1020 n_1019 k_main_1209));;
  k_main_1216 i_1020 n_1019 k_main_1209 b_1322 when (not b_1322) -> (k_main_1209 ());;
  m_1324 b_1322 i_1020 n_1019 k_main_m_1325 ->
      (array_max_1011 n_1019 i_1020 (make_array_1008 n_1019) (-1) k_main_m_1325);;
  main_1018 n_1019 i_1020 k_main_1209 when (n_1019 > 0) -> (br_main_1347 (i_1020 >= 0) n_1019 i_1020 k_main_1209);;
  main_1018 n_1019 i_1020 k_main_1209 when (not (n_1019 > 0)) -> (k_main_1216 i_1020 n_1019 k_main_1209 false);;
  main_1091 k_main_1263 -> (arg1_1087 (f_main_1344 k_main_1263));;
  make_array_1008 n_1009 i_1010 k_make_array_1142 -> (k_make_array_1142 (n_1009 - i_1010));;
  x_1126 i_1123 m_1125 x_1028 x_1124 k_array_max_x_1160 -> (x_1124 i_1123 k_array_max_x_1160);;
  z_1127 i_1123 m_1125 x_1028 x_1192 k_array_max_z_1169 when (x_1192 > m_1125) -> (k_array_max_z_1169 x_1192);;
  z_1127 i_1123 m_1125 x_1028 x_1192 k_array_max_z_1169 when (not (x_1192 > m_1125)) -> (k_array_max_z_1169 m_1125);;
Types:
  main_1340 : X
  array_max_1011 : (x_1:int -> x_2:int -> (int -> (int -> X) -> X) -> int -> (int[x_2 >= x_1] -> X) -> X)
  fail_1349 : (x_1:bool[x_1] -> (unit -> X) -> X)
  k_main_1216 : (x_1:int -> x_2:int -> (unit -> X) -> x_6:bool[1 <= -x_1 + x_2 && x_6 || (not x_6); x_6] -> X)

(0-1) Abstracting ... EXPAND_NONREC:
Main: main_1340
  main_1340 ->
      (rand_int
        (fun arg1_113 ->
         (rand_int
           (fun arg2_116 ->
            (if (arg1_113 > 0)
              (l0
                (if (arg2_116 >= 0) (l0 (k_main_1216 arg2_116 arg1_113 (fun main_88 -> end) (arg2_116 <= 0)))
                  (l1 (k_main_1216 arg2_116 arg1_113 (fun main_88 -> end) false))))
              (l1 (k_main_1216 arg2_116 arg1_113 (fun main_88 -> end) false)))))));;
  array_max_1011 x_1028 i_1123 x_1124 m_1125 k_array_max_1153 when (i_1123 >= x_1028) ->
      (l0 (k_array_max_1153 m_1125));;
  array_max_1011 x_1028 i_1123 x_1124 m_1125 k_array_max_1153 when (not (i_1123 >= x_1028)) ->
      (l1
        (x_1124 i_1123
          (fun x_94 ->
           (if (x_94 > m_1125) (l0 (array_max_1011 x_1028 (i_1123 + 1) x_1124 x_94 k_array_max_1153))
             (l1 (array_max_1011 x_1028 (i_1123 + 1) x_1124 m_1125 k_array_max_1153))))));;
  fail_1349 b k -> {fail} => (k ());;
  k_main_1216 i_1020 n_1019 k_main_1209 b_1322 when b_1322 ->
      (l0
        (array_max_1011 n_1019 i_1020 (fun i_129 k_make_array_130 -> (k_make_array_130 (n_1019 - i_129))) (-1)
          (fun m_111 -> (if (n_1019 <= m_111) (l0 (k_main_1209 ())) (l1 (fail_1349 true k_main_1209))))));;
  k_main_1216 i_1020 n_1019 k_main_1209 b_1322 when (not b_1322) -> (l1 (k_main_1209 ()));;

ETA_EXPAND:
Main: main_1340
  main_1340 ->
      (rand_int
        (fun (arg1_113:int) ->
         (rand_int
           (fun (arg2_116:int) ->
            (if (arg1_113 > 0)
              (l0
                (if (arg2_116 >= 0) (l0 (k_main_1216 arg2_116 arg1_113 (fun (main_88:unit) -> end) (arg2_116 <= 0)))
                  (l1 (k_main_1216 arg2_116 arg1_113 (fun (main_88:unit) -> end) false))))
              (l1 (k_main_1216 arg2_116 arg1_113 (fun (main_88:unit) -> end) false)))))));;
  array_max_1011 x_1028 i_1123 x_1124 m_1125 k_array_max_1153 when (i_1123 >= x_1028) ->
      (l0 (k_array_max_1153 m_1125));;
  array_max_1011 x_1028 i_1123 x_1124 m_1125 k_array_max_1153 when (not (i_1123 >= x_1028)) ->
      (l1
        (x_1124 i_1123
          (fun (x_94:int) ->
           (if (x_94 > m_1125)
             (l0
               (array_max_1011 x_1028 (i_1123 + 1)
                 (fun (x__151:int) (x__152:(int -> X)) -> (x_1124 x__151 (fun (x__153:int) -> (x__152 x__153)))) x_94
                 (fun (x__150:int[i_1123 + 1 >= x_1028]) -> (k_array_max_1153 x__150))))
             (l1
               (array_max_1011 x_1028 (i_1123 + 1)
                 (fun (x__147:int) (x__148:(int -> X)) -> (x_1124 x__147 (fun (x__149:int) -> (x__148 x__149)))) m_1125
                 (fun (x__146:int[i_1123 + 1 >= x_1028]) -> (k_array_max_1153 x__146))))))));;
  fail_1349 b k -> {fail} => (k ());;
  k_main_1216 i_1020 n_1019 k_main_1209 b_1322 when b_1322 ->
      (l0
        (array_max_1011 n_1019 i_1020
          (fun (i_129:int) (k_make_array_130:(int -> X)) -> (k_make_array_130 (n_1019 - i_129))) (-1)
          (fun (m_111:int[i_1020 >= n_1019]) ->
           (if (n_1019 <= m_111) (l0 (k_main_1209 ()))
             (l1 (fail_1349 true (fun (x__154:unit) -> (k_main_1209 x__154))))))));;
  k_main_1216 i_1020 n_1019 k_main_1209 b_1322 when (not b_1322) -> (l1 (k_main_1209 ()));;

main_1340: ENV: 

main_1340: (rand_int
             (fun (arg1_113:int) ->
              (rand_int
                (fun (arg2_116:int) ->
                 (if (arg1_113 > 0)
                   (l0
                     (if (arg2_116 >= 0)
                       (l0 (k_main_1216 arg2_116 arg1_113 (fun (main_88:unit) -> end) (arg2_116 <= 0)))
                       (l1 (k_main_1216 arg2_116 arg1_113 (fun (main_88:unit) -> end) false))))
                   (l1 (k_main_1216 arg2_116 arg1_113 (fun (main_88:unit) -> end) false))))))) ===> (
rand_int
 (fun (arg1_113:int) ->
  (rand_int
    (fun (arg2_116:int) ->
     (if (arg1_113 > 0)
       (l0
         (if (arg2_116 >= 0) (l0 (k_main_1216 arg2_116 arg1_113 (fun (main_88:unit) -> end) (arg2_116 <= 0)))
           (l1 (k_main_1216 arg2_116 arg1_113 (fun (main_88:unit) -> end) false))))
       (l1 (k_main_1216 arg2_116 arg1_113 (fun (main_88:unit) -> end) false)))))))
main_1340:: (rand_int
              (fun (arg1_113:int) ->
               (rand_int
                 (fun (arg2_116:int) ->
                  (if (arg1_113 > 0)
                    (l0
                      (if (arg2_116 >= 0)
                        (l0 (k_main_1216 arg2_116 arg1_113 (fun (main_88:unit) -> end) (arg2_116 <= 0)))
                        (l1 (k_main_1216 arg2_116 arg1_113 (fun (main_88:unit) -> end) false))))
                    (l1 (k_main_1216 arg2_116 arg1_113 (fun (main_88:unit) -> end) false)))))))
abst_arg: arg1_113, int;;
abst_arg: arg1_113, int;;
abst_arg: arg2_116, int;;
abst_arg: arg2_116, int;;
cond: true
pbs: 
p:(arg1_113 > 0)
xs: 0, qs: 0
nxs: 0, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
tt:false
ff:false

cond: (arg1_113 > 0); true
pbs: 
p:(arg2_116 >= 0)
xs: 0, qs: 0
nxs: 0, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
tt:false
ff:false

abst_arg: main_88, unit;;
abst_arg: main_88, unit;;
filter
  cond: (arg2_116 >= 0); (arg1_113 > 0); true
  orig pbs: 
  unsat:false

cond: (arg2_116 >= 0); (arg1_113 > 0); true
pbs: 
p:(((1 <= (((-1) * arg2_116) + arg1_113)) && (arg2_116 <= 0)) || (not (arg2_116 <= 0)))
tt:true
ff:false

cond: (arg2_116 >= 0); (arg1_113 > 0); true
pbs: 
p:(arg2_116 <= 0)
xs: 0, qs: 0
nxs: 0, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
tt:false
ff:false

filter
  cond: (arg2_116 >= 0); (arg1_113 > 0); true
  orig pbs: 
  unsat:false

abst_arg: main_88, unit;;
abst_arg: main_88, unit;;
filter
  cond: (not (arg2_116 >= 0)); (arg1_113 > 0); true
  orig pbs: 
  unsat:false

cond: (not (arg2_116 >= 0)); (arg1_113 > 0); true
pbs: 
p:(((1 <= (((-1) * arg2_116) + arg1_113)) && false) || (not false))
tt:true
ff:false

cond: (not (arg2_116 >= 0)); (arg1_113 > 0); true
pbs: 
p:false
tt:false
ff:true

filter
  cond: (not (arg2_116 >= 0)); (arg1_113 > 0); true
  orig pbs: 
  unsat:false

abst_arg: main_88, unit;;
abst_arg: main_88, unit;;
filter
  cond: (not (arg1_113 > 0)); true
  orig pbs: 
  unsat:false

cond: (not (arg1_113 > 0)); true
pbs: 
p:(((1 <= (((-1) * arg2_116) + arg1_113)) && false) || (not false))
tt:true
ff:false

cond: (not (arg1_113 > 0)); true
pbs: 
p:false
tt:false
ff:true

filter
  cond: (not (arg1_113 > 0)); true
  orig pbs: 
  unsat:false

filter
  cond: true
  orig pbs: 
  unsat:false

filter
  cond: true
  orig pbs: 
  unsat:false

ASSUME:
  cond: 
  pbs: 
  t1: true
filter
  cond: true
  orig pbs: 
  unsat:false

array_max_1011: ENV: x_1028:int, i_1123:int, x_1124:(int -> (int -> X) -> X), m_1125:int,
k_array_max_1153:(int[i_1123 >= x_1028] -> X),


abst_arg: x_1028, int;;
abst_arg: i_1123, int;;
abst_arg: x_1124, (int -> (int -> X) -> X);;
abst_arg: m_1125, int;;
abst_arg: k_array_max_1153, (int[i_1123 >= x_1028] ->
X);;
abst_arg: x_1028, int;;
abst_arg: i_1123, int;;
abst_arg: x_1124, (int -> (int -> X) -> X);;
abst_arg: m_1125, int;;
abst_arg: k_array_max_1153, (int[i_1123 >= x_1028] ->
X);;
array_max_1011: (l0 (k_array_max_1153 m_1125)) ===> (l0 (k_array_max_1153 m_1125))
array_max_1011:: (l0 (k_array_max_1153 m_1125))
cond: (i_1123 >= x_1028)
pbs: 
p:(i_1123 >= x_1028)
tt:true
ff:false

filter
  cond: (i_1123 >= x_1028)
  orig pbs: 
  unsat:false

ASSUME:
  cond: 
  pbs: 
  t1: (i_1123 >= x_1028)
filter
  cond: (i_1123 >= x_1028)
  orig pbs: 
  unsat:false

array_max_1011: ENV: x_1028:int, i_1123:int, x_1124:(int -> (int -> X) -> X), m_1125:int,
k_array_max_1153:(int[i_1123 >= x_1028] -> X),


abst_arg: x_1028, int;;
abst_arg: i_1123, int;;
abst_arg: x_1124, (int -> (int -> X) -> X);;
abst_arg: m_1125, int;;
abst_arg: k_array_max_1153, (int[i_1123 >= x_1028] ->
X);;
abst_arg: x_1028, int;;
abst_arg: i_1123, int;;
abst_arg: x_1124, (int -> (int -> X) -> X);;
abst_arg: m_1125, int;;
abst_arg: k_array_max_1153, (int[i_1123 >= x_1028] ->
X);;
array_max_1011: (l1
                  (x_1124 i_1123
                    (fun (x_94:int) ->
                     (if (x_94 > m_1125)
                       (l0
                         (array_max_1011 x_1028 (i_1123 + 1)
                           (fun (x__151:int) (x__152:(int -> X)) ->
                            (x_1124 x__151 (fun (x__153:int) -> (x__152 x__153)))) x_94
                           (fun (x__150:int[i_1123 + 1 >= x_1028]) -> (k_array_max_1153 x__150))))
                       (l1
                         (array_max_1011 x_1028 (i_1123 + 1)
                           (fun (x__147:int) (x__148:(int -> X)) ->
                            (x_1124 x__147 (fun (x__149:int) -> (x__148 x__149)))) m_1125
                           (fun (x__146:int[i_1123 + 1 >= x_1028]) -> (k_array_max_1153 x__146)))))))) ===> (
l1
 (x_1124 i_1123
   (fun (x_94:int) ->
    (if (x_94 > m_1125)
      (l0
        (array_max_1011 x_1028 (i_1123 + 1)
          (fun (x__151:int) (x__152:(int -> X)) -> (x_1124 x__151 (fun (x__153:int) -> (x__152 x__153)))) x_94
          (fun (x__150:int[i_1123 + 1 >= x_1028]) -> (k_array_max_1153 x__150))))
      (l1
        (array_max_1011 x_1028 (i_1123 + 1)
          (fun (x__147:int) (x__148:(int -> X)) -> (x_1124 x__147 (fun (x__149:int) -> (x__148 x__149)))) m_1125
          (fun (x__146:int[i_1123 + 1 >= x_1028]) -> (k_array_max_1153 x__146))))))))
array_max_1011:: (l1
                   (x_1124 i_1123
                     (fun (x_94:int) ->
                      (if (x_94 > m_1125)
                        (l0
                          (array_max_1011 x_1028 (i_1123 + 1)
                            (fun (x__151:int) (x__152:(int -> X)) ->
                             (x_1124 x__151 (fun (x__153:int) -> (x__152 x__153)))) x_94
                            (fun (x__150:int[i_1123 + 1 >= x_1028]) -> (k_array_max_1153 x__150))))
                        (l1
                          (array_max_1011 x_1028 (i_1123 + 1)
                            (fun (x__147:int) (x__148:(int -> X)) ->
                             (x_1124 x__147 (fun (x__149:int) -> (x__148 x__149)))) m_1125
                            (fun (x__146:int[i_1123 + 1 >= x_1028]) -> (k_array_max_1153 x__146))))))))
abst_arg: x_94, int;;
abst_arg: x_94, int;;
cond: (not (i_1123 >= x_1028))
pbs: 
p:(x_94 > m_1125)
xs: 0, qs: 0
nxs: 0, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
tt:false
ff:false

abst_arg: x__151, int;;
abst_arg: x__152, (int ->
X);;
abst_arg: x__151, int;;
abst_arg: x__152, (int ->
X);;
abst_arg: x__153, int;;
abst_arg: x__153, int;;
filter
  cond: (x_94 > m_1125); (not (i_1123 >= x_1028))
  orig pbs: 
  unsat:false

filter
  cond: (x_94 > m_1125); (not (i_1123 >= x_1028))
  orig pbs: 
  unsat:false

filter
  cond: (x_94 > m_1125); (not (i_1123 >= x_1028))
  orig pbs: 
  unsat:false

filter
  cond: (x_94 > m_1125); (not (i_1123 >= x_1028))
  orig pbs: 
  unsat:false

abst_arg: x__150, int[i_1123 + 1 >= x_1028];;
abst_arg: x__150, int[i_1123 + 1 >= x_1028];;
cond: (x_94 > m_1125); (not (i_1123 >= x_1028))
pbs: x__150 := ((i_1123 + 1) >= x_1028)
p:(i_1123 >= x_1028)
tt:false
ff:true

filter
  cond: (x_94 > m_1125); (not (i_1123 >= x_1028))
  orig pbs: x__150 := ((i_1123 + 1) >= x_1028)
  pbs: x__150 := ((i_1123 + 1) >= x_1028)
fvsp: 2
xs: 0, qs: 1
nxs: 1, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (x_94 > m_1125); (not (i_1123 >= x_1028))
  orig pbs: x__150 := ((i_1123 + 1) >= x_1028)
  pbs: x__150 := ((i_1123 + 1) >= x_1028)
fvsp: 2
xs: 0, qs: 1
nxs: 1, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (x_94 > m_1125); (not (i_1123 >= x_1028))
  orig pbs: 
  unsat:false

abst_arg: x__147, int;;
abst_arg: x__148, (int ->
X);;
abst_arg: x__147, int;;
abst_arg: x__148, (int ->
X);;
abst_arg: x__149, int;;
abst_arg: x__149, int;;
filter
  cond: (not (x_94 > m_1125)); (not (i_1123 >= x_1028))
  orig pbs: 
  unsat:false

filter
  cond: (not (x_94 > m_1125)); (not (i_1123 >= x_1028))
  orig pbs: 
  unsat:false

filter
  cond: (not (x_94 > m_1125)); (not (i_1123 >= x_1028))
  orig pbs: 
  unsat:false

filter
  cond: (not (x_94 > m_1125)); (not (i_1123 >= x_1028))
  orig pbs: 
  unsat:false

abst_arg: x__146, int[i_1123 + 1 >= x_1028];;
abst_arg: x__146, int[i_1123 + 1 >= x_1028];;
cond: (not (x_94 > m_1125)); (not (i_1123 >= x_1028))
pbs: x__146 := ((i_1123 + 1) >= x_1028)
p:(i_1123 >= x_1028)
tt:false
ff:true

filter
  cond: (not (x_94 > m_1125)); (not (i_1123 >= x_1028))
  orig pbs: x__146 := ((i_1123 + 1) >= x_1028)
  pbs: x__146 := ((i_1123 + 1) >= x_1028)
fvsp: 2
xs: 0, qs: 1
nxs: 1, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (not (x_94 > m_1125)); (not (i_1123 >= x_1028))
  orig pbs: x__146 := ((i_1123 + 1) >= x_1028)
  pbs: x__146 := ((i_1123 + 1) >= x_1028)
fvsp: 2
xs: 0, qs: 1
nxs: 1, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (not (x_94 > m_1125)); (not (i_1123 >= x_1028))
  orig pbs: 
  unsat:false

filter
  cond: (not (i_1123 >= x_1028))
  orig pbs: 
  unsat:false

filter
  cond: (not (i_1123 >= x_1028))
  orig pbs: 
  unsat:false

ASSUME:
  cond: 
  pbs: 
  t1: (not (i_1123 >= x_1028))
filter
  cond: (not (i_1123 >= x_1028))
  orig pbs: 
  unsat:false

fail_1349: ENV: b:x_1:bool[x_1], k:(unit -> X),


abst_arg: b, x_1:bool[x_1];;
abst_arg: k, (unit ->
X);;
abst_arg: b, x_1:bool[x_1];;
abst_arg: k, (unit ->
X);;
fail_1349: (k ()) ===> (k ())
fail_1349:: (k ())
filter
  cond: true
  orig pbs: b := b
  pbs: b := b
fvsp: 1
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 1
ws2: 1
ws3: 0
ws4: 0
  unsat:false

ASSUME:
  cond: 
  pbs: b := b
  t1: true
filter
  cond: true
  orig pbs: b := b
  pbs: b := b
fvsp: 1
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 1
ws2: 1
ws3: 0
ws4: 0
  unsat:false

k_main_1216: ENV: i_1020:int, n_1019:int, k_main_1209:(unit -> X),
b_1322:bool[1 <= -i_1020 + n_1019 && i_1020 || (not i_1020); i_1020],


abst_arg: i_1020, int;;
abst_arg: n_1019, int;;
abst_arg: k_main_1209, (unit ->
X);;
abst_arg: b_1322, bool[1 <= -i_1020 + n_1019 && i_1020 || (not i_1020); i_1020];;
abst_arg: i_1020, int;;
abst_arg: n_1019, int;;
abst_arg: k_main_1209, (unit ->
X);;
abst_arg: b_1322, bool[1 <= -i_1020 + n_1019 && i_1020 || (not i_1020); i_1020];;
k_main_1216: (l0
               (array_max_1011 n_1019 i_1020
                 (fun (i_129:int) (k_make_array_130:(int -> X)) -> (k_make_array_130 (n_1019 - i_129))) (-1)
                 (fun (m_111:int[i_1020 >= n_1019]) ->
                  (if (n_1019 <= m_111) (l0 (k_main_1209 ()))
                    (l1 (fail_1349 true (fun (x__154:unit) -> (k_main_1209 x__154)))))))) ===> (
l0
 (array_max_1011 n_1019 i_1020 (fun (i_129:int) (k_make_array_130:(int -> X)) -> (k_make_array_130 (n_1019 - i_129)))
   (-1)
   (fun (m_111:int[i_1020 >= n_1019]) ->
    (if (n_1019 <= m_111) (l0 (k_main_1209 ())) (l1 (fail_1349 true (fun (x__154:unit) -> (k_main_1209 x__154))))))))
k_main_1216:: (l0
                (array_max_1011 n_1019 i_1020
                  (fun (i_129:int) (k_make_array_130:(int -> X)) -> (k_make_array_130 (n_1019 - i_129))) (-1)
                  (fun (m_111:int[i_1020 >= n_1019]) ->
                   (if (n_1019 <= m_111) (l0 (k_main_1209 ()))
                     (l1 (fail_1349 true (fun (x__154:unit) -> (k_main_1209 x__154))))))))
abst_arg: i_129, int;;
abst_arg: k_make_array_130, (int ->
X);;
abst_arg: i_129, int;;
abst_arg: k_make_array_130, (int ->
X);;
filter
  cond: b_1322
  orig pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b2_1322 := b_1322
  pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b2_1322 := b_1322
fvsp: 3
fvsp: 1
xs: 0, qs: 2
nxs: 2, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: b_1322
  orig pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b2_1322 := b_1322
  pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b2_1322 := b_1322
fvsp: 3
fvsp: 1
xs: 0, qs: 2
nxs: 2, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

abst_arg: m_111, int[i_1020 >= n_1019];;
abst_arg: m_111, int[i_1020 >= n_1019];;
cond: b_1322
pbs: m_111 := (i_1020 >= n_1019);
     b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
     b2_1322 := b_1322
p:(n_1019 <= m_111)
fvsp: 2
fvsp: 3
fvsp: 1
xs: 0, qs: 3
nxs: 0, ys: 3
ws1: 6
ws2: 6
ws3: 3
ws4: 3
fvsp: 5
fvsp: 3
fvsp: 4
xs: 1, qs: 2
nxs: 1, ys: 2
ws1: 7
ws2: 7
ws3: 0
ws4: 0
tt:false
ff:false

filter
  cond: (n_1019 <= m_111); b_1322
  orig pbs: m_111 := (i_1020 >= n_1019);
            b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b2_1322 := b_1322
  pbs: m_111 := (i_1020 >= n_1019);
       b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b2_1322 := b_1322
fvsp: 2
fvsp: 3
fvsp: 1
xs: 0, qs: 3
nxs: 3, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

cond: (not (n_1019 <= m_111)); b_1322
pbs: m_111 := (i_1020 >= n_1019);
     b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
     b2_1322 := b_1322
p:true
tt:true
ff:false

abst_arg: x__154, unit;;
abst_arg: x__154, unit;;
filter
  cond: (not (n_1019 <= m_111)); b_1322
  orig pbs: m_111 := (i_1020 >= n_1019);
            b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b2_1322 := b_1322
  pbs: m_111 := (i_1020 >= n_1019);
       b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b2_1322 := b_1322
fvsp: 2
fvsp: 3
fvsp: 1
xs: 0, qs: 3
nxs: 3, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (not (n_1019 <= m_111)); b_1322
  orig pbs: m_111 := (i_1020 >= n_1019);
            b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b2_1322 := b_1322
  pbs: m_111 := (i_1020 >= n_1019);
       b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b2_1322 := b_1322
fvsp: 2
fvsp: 3
fvsp: 1
xs: 0, qs: 3
nxs: 3, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (not (n_1019 <= m_111)); b_1322
  orig pbs: m_111 := (i_1020 >= n_1019);
            b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b2_1322 := b_1322
  pbs: m_111 := (i_1020 >= n_1019);
       b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b2_1322 := b_1322
fvsp: 2
fvsp: 3
fvsp: 1
xs: 0, qs: 3
nxs: 3, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: b_1322
  orig pbs: m_111 := (i_1020 >= n_1019);
            b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b2_1322 := b_1322
  pbs: m_111 := (i_1020 >= n_1019);
       b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b2_1322 := b_1322
fvsp: 2
fvsp: 3
fvsp: 1
xs: 0, qs: 3
nxs: 2, ys: 1
ws1: 1
ws2: 1
ws3: 0
ws4: 0
  unsat:false

filter
  cond: b_1322
  orig pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b2_1322 := b_1322
  pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b2_1322 := b_1322
fvsp: 3
fvsp: 1
xs: 0, qs: 2
nxs: 2, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

ASSUME:
  cond: 
  pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b2_1322 := b_1322
  t1: b_1322
filter
  cond: b_1322
  orig pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b2_1322 := b_1322
  pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b2_1322 := b_1322
fvsp: 3
fvsp: 1
xs: 0, qs: 2
nxs: 2, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

k_main_1216: ENV: i_1020:int, n_1019:int, k_main_1209:(unit -> X),
b_1322:bool[1 <= -i_1020 + n_1019 && i_1020 || (not i_1020); i_1020],


abst_arg: i_1020, int;;
abst_arg: n_1019, int;;
abst_arg: k_main_1209, (unit ->
X);;
abst_arg: b_1322, bool[1 <= -i_1020 + n_1019 && i_1020 || (not i_1020); i_1020];;
abst_arg: i_1020, int;;
abst_arg: n_1019, int;;
abst_arg: k_main_1209, (unit ->
X);;
abst_arg: b_1322, bool[1 <= -i_1020 + n_1019 && i_1020 || (not i_1020); i_1020];;
k_main_1216: (l1 (k_main_1209 ())) ===> (l1 (k_main_1209 ()))
k_main_1216:: (l1 (k_main_1209 ()))
filter
  cond: (not b_1322)
  orig pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b2_1322 := b_1322
  pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b2_1322 := b_1322
fvsp: 3
fvsp: 1
xs: 1, qs: 1
nxs: 2, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:(b2_1322 || b2_1322)

ASSUME:
  cond: 
  pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b2_1322 := b_1322
  t1: (not b_1322)
filter
  cond: (not b_1322)
  orig pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b2_1322 := b_1322
  pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b2_1322 := b_1322
fvsp: 3
fvsp: 1
xs: 1, qs: 1
nxs: 2, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:(b2_1322 || b2_1322)

ABST:
Main: main_1340
  main_1340 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1216 () true rand_bool)) (l1 (k_main_1216 () true false))))
        (l1 (k_main_1216 () true false)));;
  array_max_1011 x_1124 k_array_max_1153 -> (l0 (k_array_max_1153 true));;
  array_max_1011 x_1124 k_array_max_1153 ->
      (l1
        (x_1124
          (if rand_bool (l0 (array_max_1011 x_1124 (fun x__150 -> (k_array_max_1153 false))))
            (l1 (array_max_1011 x_1124 (fun x__146 -> (k_array_max_1153 false)))))));;
  fail_1349 b k -> {fail} => k;;
  k_main_1216 k_main_1209 b1_1322 b2_1322 ->
      (l0
        (array_max_1011 (fun k_make_array_130 -> k_make_array_130)
          (fun m_111 -> (if rand_bool (l0 k_main_1209) (l1 (fail_1349 true k_main_1209))))));;
  k_main_1216 k_main_1209 b1_1322 b2_1322 ->
      (if (b2_1322 || b2_1322) _|_ (l1 (if (b2_1322 || b2_1322) _|_ k_main_1209)));;
Types:
  main_1340 : unit
  array_max_1011 : ((unit -> unit) -> (bool -> unit) -> unit)
  array_max_1011 : ((unit -> unit) -> (bool -> unit) -> unit)
  fail_1349 : (bool -> unit -> unit)
  k_main_1216 : (unit -> bool -> bool -> unit)
  k_main_1216 : (unit -> bool -> bool -> unit)

LIFT:
Main: main_1340
  main_1340 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1216 () true rand_bool)) (l1 (k_main_1216 () true false))))
        (l1 (k_main_1216 () true false)));;
  array_max_1011 x_1124 k_array_max_1153 -> (l0 (k_array_max_1153 true));;
  array_max_1011 x_1124 k_array_max_1153 ->
      (l1
        (x_1124
          (if rand_bool (l0 (array_max_1011 x_1124 (f_157 k_array_max_1153)))
            (l1 (array_max_1011 x_1124 (f_160 k_array_max_1153))))));;
  f_157 k_array_max_156 x__150 -> (k_array_max_156 false);;
  f_160 k_array_max_159 x__146 -> (k_array_max_159 false);;
  fail_1349 b k -> {fail} => k;;
  k_main_1216 k_main_1209 b1_1322 b2_1322 -> (l0 (array_max_1011 f_162 (f_165 k_main_1209)));;
  f_162 k_make_array_130 -> k_make_array_130;;
  f_165 k_main_164 m_111 -> (if rand_bool (l0 k_main_164) (l1 (fail_1349 true k_main_164)));;
  k_main_1216 k_main_1209 b1_1322 b2_1322 ->
      (if (b2_1322 || b2_1322) _|_ (l1 (if (b2_1322 || b2_1322) _|_ k_main_1209)));;

TRANS_EAGER:
Main: main_1340
  main_1340 ->
      (let f_167 b_166 =
       (if b_166
         (l0
           (let f_169 b_168 =
            (if b_168 (l0 (let f_171 b_170 = (k_main_1216 () true b_170) in (if rand_bool (f_171 true) (f_171 false))))
              (l1 (k_main_1216 () true false)))
            in (if rand_bool (f_169 true) (f_169 false)))) (l1 (k_main_1216 () true false)))
       in (if rand_bool (f_167 true) (f_167 false)));;
  array_max_1011 x_1124 k_array_max_1153 -> (l0 (k_array_max_1153 true));;
  array_max_1011 x_1124 k_array_max_1153 ->
      (l1
        (x_1124
          (let f_173 b_172 =
           (if b_172 (l0 (array_max_1011 x_1124 (f_157 k_array_max_1153)))
             (l1 (array_max_1011 x_1124 (f_160 k_array_max_1153))))
           in (if rand_bool (f_173 true) (f_173 false)))));;
  f_157 k_array_max_156 x__150 -> (k_array_max_156 false);;
  f_160 k_array_max_159 x__146 -> (k_array_max_159 false);;
  fail_1349 b k -> {fail} => k;;
  k_main_1216 k_main_1209 b1_1322 b2_1322 -> (l0 (array_max_1011 f_162 (f_165 k_main_1209)));;
  f_162 k_make_array_130 -> k_make_array_130;;
  f_165 k_main_164 m_111 ->
      (let f_175 b_174 = (if b_174 (l0 k_main_164) (l1 (fail_1349 true k_main_164))) in
       (if rand_bool (f_175 true) (f_175 false)));;
  k_main_1216 k_main_1209 b1_1322 b2_1322 ->
      (let f_177 b_176 =
       (if b_176 _|_
         (l1
           (let f_179 b_178 = (if b_178 _|_ k_main_1209) in
            (let f_181 b_180 = (if b_180 (f_179 true) (f_179 b2_1322)) in (f_181 b2_1322)))))
       in (let f_183 b_182 = (if b_182 (f_177 true) (f_177 b2_1322)) in (f_183 b2_1322)));;

PUT_INTO_IF:
Main: main_1340
  main_1340 ->
      (let f_167 b_166 =
       (if b_166
         (l0
           (let f_169 b_168 =
            (if b_168 (l0 (let f_171 b_170 = (k_main_1216 () true b_170) in (if rand_bool (f_171 true) (f_171 false))))
              (l1 (k_main_1216 () true false)))
            in (if rand_bool (f_169 true) (f_169 false)))) (l1 (k_main_1216 () true false)))
       in (if rand_bool (f_167 true) (f_167 false)));;
  array_max_1011 x_1124 k_array_max_1153 -> (l0 (k_array_max_1153 true));;
  array_max_1011 x_1124 k_array_max_1153 ->
      (l1
        (x_1124
          (let f_173 b_172 =
           (if b_172 (l0 (array_max_1011 x_1124 (f_157 k_array_max_1153)))
             (l1 (array_max_1011 x_1124 (f_160 k_array_max_1153))))
           in (if rand_bool (f_173 true) (f_173 false)))));;
  f_157 k_array_max_156 x__150 -> (k_array_max_156 false);;
  f_160 k_array_max_159 x__146 -> (k_array_max_159 false);;
  fail_1349 b k -> {fail} => k;;
  k_main_1216 k_main_1209 b1_1322 b2_1322 -> (l0 (array_max_1011 f_162 (f_165 k_main_1209)));;
  f_162 k_make_array_130 -> k_make_array_130;;
  f_165 k_main_164 m_111 ->
      (let f_175 b_174 = (if b_174 (l0 k_main_164) (l1 (fail_1349 true k_main_164))) in
       (if rand_bool (f_175 true) (f_175 false)));;
  k_main_1216 k_main_1209 b1_1322 b2_1322 ->
      (let f_177 b_176 =
       (if b_176 _|_
         (l1
           (let f_179 b_178 = (if b_178 _|_ k_main_1209) in
            (let f_181 b_180 = (if b_180 (f_179 true) (f_179 b2_1322)) in (f_181 b2_1322)))))
       in (let f_183 b_182 = (if b_182 (f_177 true) (f_177 b2_1322)) in (f_183 b2_1322)));;

DONE!

(0-2) Checking HORS ... DONE!

Error trace::
  main_1340 ... --> 
  main_1091 ... --> 
  arg1_1087 ... --> 
  f_main_1344 ... --> 
  arg2_1089 ... --> 
  f_main_1345 ... --> 
  main_1018 [1/2] ... --> 
  br_main_1347 [1/2] ... --> 
  k_main_1216 [1/2] ... --> 
  m_1324 ... --> 
  array_max_1011 [1/2] ... --> 
  f_k_main_1343 [2/2] ... --> 
  fail_1349 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0

(0-3) Checking counterexample ... DONE!

(0-4) Discovering predicates ... 
DONE!

Prefix of spurious counterexample::
0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0

Program with abstraction types (CEGAR-cycle 1)::
Main: main_1340
  main_1340 -> (main_1091 f_1346);;
  arg1_1087 k_main_arg1_1268 -> (rand_int k_main_arg1_1268);;
  arg2_1089 arg1_1298 k_main_arg2_1280 -> (rand_int k_main_arg2_1280);;
  array_max_1011 x_1028 i_1123 x_1124 m_1125 k_array_max_1153 when (i_1123 >= x_1028) -> (k_array_max_1153 m_1125);;
  array_max_1011 x_1028 i_1123 x_1124 m_1125 k_array_max_1153 when (not (i_1123 >= x_1028)) ->
      (x_1126 i_1123 m_1125 x_1028 x_1124 (f_array_max_1341 i_1123 m_1125 x_1028 k_array_max_1153 x_1124));;
  br_main_1347 b_1348 n_1019 i_1020 k_main_1209 when b_1348 -> (k_main_1216 i_1020 n_1019 k_main_1209 (i_1020 <= 0));;
  br_main_1347 b_1348 n_1019 i_1020 k_main_1209 when (not b_1348) -> (k_main_1216 i_1020 n_1019 k_main_1209 false);;
  f_1346 main_1299 -> end;;
  f_array_max_1341 i_1123 m_1125 x_1028 k_array_max_1153 x_1124 x_1192 ->
      (z_1127 i_1123 m_1125 x_1028 x_1192 (f_array_max_1342 i_1123 m_1125 x_1028 x_1192 k_array_max_1153 x_1124));;
  f_array_max_1342 i_1123 m_1125 x_1028 x_1192 k_array_max_1153 x_1124 z_1191 ->
      (array_max_1011 x_1028 (i_1123 + 1) x_1124 z_1191 k_array_max_1153);;
  f_k_main_1343 b_1322 i_1020 n_1019 k_main_1209 m_1328 when (m_1328 >= n_1019) -> (k_main_1209 ());;
  f_k_main_1343 b_1322 i_1020 n_1019 k_main_1209 m_1328 when (not (m_1328 >= n_1019)) -> (fail_1349 true k_main_1209);;
  f_main_1344 k_main_1263 arg1_1298 -> (arg2_1089 arg1_1298 (f_main_1345 arg1_1298 k_main_1263));;
  f_main_1345 arg1_1298 k_main_1263 arg2_1297 -> (main_1018 arg1_1298 arg2_1297 k_main_1263);;
  fail_1349 b k -> {fail} => (k ());;
  k_main_1216 i_1020 n_1019 k_main_1209 b_1322 when b_1322 ->
      (m_1324 b_1322 i_1020 n_1019 (f_k_main_1343 b_1322 i_1020 n_1019 k_main_1209));;
  k_main_1216 i_1020 n_1019 k_main_1209 b_1322 when (not b_1322) -> (k_main_1209 ());;
  m_1324 b_1322 i_1020 n_1019 k_main_m_1325 ->
      (array_max_1011 n_1019 i_1020 (make_array_1008 n_1019) (-1) k_main_m_1325);;
  main_1018 n_1019 i_1020 k_main_1209 when (n_1019 > 0) -> (br_main_1347 (i_1020 >= 0) n_1019 i_1020 k_main_1209);;
  main_1018 n_1019 i_1020 k_main_1209 when (not (n_1019 > 0)) -> (k_main_1216 i_1020 n_1019 k_main_1209 false);;
  main_1091 k_main_1263 -> (arg1_1087 (f_main_1344 k_main_1263));;
  make_array_1008 n_1009 i_1010 k_make_array_1142 -> (k_make_array_1142 (n_1009 - i_1010));;
  x_1126 i_1123 m_1125 x_1028 x_1124 k_array_max_x_1160 -> (x_1124 i_1123 k_array_max_x_1160);;
  z_1127 i_1123 m_1125 x_1028 x_1192 k_array_max_z_1169 when (x_1192 > m_1125) -> (k_array_max_z_1169 x_1192);;
  z_1127 i_1123 m_1125 x_1028 x_1192 k_array_max_z_1169 when (not (x_1192 > m_1125)) -> (k_array_max_z_1169 m_1125);;
Types:
  main_1340 : X
  array_max_1011 : (x_1:int ->
                    x_2:int -> (int -> (int -> X) -> X) -> int[1 <= x_1 - x_2] -> (int[x_2 >= x_1] -> X) -> X)
  fail_1349 : (x_1:bool[x_1] -> (unit -> X) -> X)
  k_main_1216 : (x_1:int -> x_2:int -> (unit -> X) -> x_6:bool[1 <= -x_1 + x_2 && x_6 || (not x_6); x_6] -> X)

(1-1) Abstracting ... EXPAND_NONREC:
Main: main_1340
  main_1340 ->
      (rand_int
        (fun arg1_636 ->
         (rand_int
           (fun arg2_639 ->
            (if (arg1_636 > 0)
              (l0
                (if (arg2_639 >= 0) (l0 (k_main_1216 arg2_639 arg1_636 (fun main_611 -> end) (arg2_639 <= 0)))
                  (l1 (k_main_1216 arg2_639 arg1_636 (fun main_611 -> end) false))))
              (l1 (k_main_1216 arg2_639 arg1_636 (fun main_611 -> end) false)))))));;
  array_max_1011 x_1028 i_1123 x_1124 m_1125 k_array_max_1153 when (i_1123 >= x_1028) ->
      (l0 (k_array_max_1153 m_1125));;
  array_max_1011 x_1028 i_1123 x_1124 m_1125 k_array_max_1153 when (not (i_1123 >= x_1028)) ->
      (l1
        (x_1124 i_1123
          (fun x_617 ->
           (if (x_617 > m_1125) (l0 (array_max_1011 x_1028 (i_1123 + 1) x_1124 x_617 k_array_max_1153))
             (l1 (array_max_1011 x_1028 (i_1123 + 1) x_1124 m_1125 k_array_max_1153))))));;
  fail_1349 b k -> {fail} => (k ());;
  k_main_1216 i_1020 n_1019 k_main_1209 b_1322 when b_1322 ->
      (l0
        (array_max_1011 n_1019 i_1020 (fun i_652 k_make_array_653 -> (k_make_array_653 (n_1019 - i_652))) (-1)
          (fun m_634 -> (if (n_1019 <= m_634) (l0 (k_main_1209 ())) (l1 (fail_1349 true k_main_1209))))));;
  k_main_1216 i_1020 n_1019 k_main_1209 b_1322 when (not b_1322) -> (l1 (k_main_1209 ()));;

ETA_EXPAND:
Main: main_1340
  main_1340 ->
      (rand_int
        (fun (arg1_636:int) ->
         (rand_int
           (fun (arg2_639:int) ->
            (if (arg1_636 > 0)
              (l0
                (if (arg2_639 >= 0) (l0 (k_main_1216 arg2_639 arg1_636 (fun (main_611:unit) -> end) (arg2_639 <= 0)))
                  (l1 (k_main_1216 arg2_639 arg1_636 (fun (main_611:unit) -> end) false))))
              (l1 (k_main_1216 arg2_639 arg1_636 (fun (main_611:unit) -> end) false)))))));;
  array_max_1011 x_1028 i_1123 x_1124 m_1125 k_array_max_1153 when (i_1123 >= x_1028) ->
      (l0 (k_array_max_1153 m_1125));;
  array_max_1011 x_1028 i_1123 x_1124 m_1125 k_array_max_1153 when (not (i_1123 >= x_1028)) ->
      (l1
        (x_1124 i_1123
          (fun (x_617:int) ->
           (if (x_617 > m_1125)
             (l0
               (array_max_1011 x_1028 (i_1123 + 1)
                 (fun (x__674:int) (x__675:(int -> X)) -> (x_1124 x__674 (fun (x__676:int) -> (x__675 x__676)))) x_617
                 (fun (x__673:int[i_1123 + 1 >= x_1028]) -> (k_array_max_1153 x__673))))
             (l1
               (array_max_1011 x_1028 (i_1123 + 1)
                 (fun (x__670:int) (x__671:(int -> X)) -> (x_1124 x__670 (fun (x__672:int) -> (x__671 x__672)))) m_1125
                 (fun (x__669:int[i_1123 + 1 >= x_1028]) -> (k_array_max_1153 x__669))))))));;
  fail_1349 b k -> {fail} => (k ());;
  k_main_1216 i_1020 n_1019 k_main_1209 b_1322 when b_1322 ->
      (l0
        (array_max_1011 n_1019 i_1020
          (fun (i_652:int) (k_make_array_653:(int -> X)) -> (k_make_array_653 (n_1019 - i_652))) (-1)
          (fun (m_634:int[i_1020 >= n_1019]) ->
           (if (n_1019 <= m_634) (l0 (k_main_1209 ()))
             (l1 (fail_1349 true (fun (x__677:unit) -> (k_main_1209 x__677))))))));;
  k_main_1216 i_1020 n_1019 k_main_1209 b_1322 when (not b_1322) -> (l1 (k_main_1209 ()));;

main_1340: ENV: 

main_1340: (rand_int
             (fun (arg1_636:int) ->
              (rand_int
                (fun (arg2_639:int) ->
                 (if (arg1_636 > 0)
                   (l0
                     (if (arg2_639 >= 0)
                       (l0 (k_main_1216 arg2_639 arg1_636 (fun (main_611:unit) -> end) (arg2_639 <= 0)))
                       (l1 (k_main_1216 arg2_639 arg1_636 (fun (main_611:unit) -> end) false))))
                   (l1 (k_main_1216 arg2_639 arg1_636 (fun (main_611:unit) -> end) false))))))) ===> (
rand_int
 (fun (arg1_636:int) ->
  (rand_int
    (fun (arg2_639:int) ->
     (if (arg1_636 > 0)
       (l0
         (if (arg2_639 >= 0) (l0 (k_main_1216 arg2_639 arg1_636 (fun (main_611:unit) -> end) (arg2_639 <= 0)))
           (l1 (k_main_1216 arg2_639 arg1_636 (fun (main_611:unit) -> end) false))))
       (l1 (k_main_1216 arg2_639 arg1_636 (fun (main_611:unit) -> end) false)))))))
main_1340:: (rand_int
              (fun (arg1_636:int) ->
               (rand_int
                 (fun (arg2_639:int) ->
                  (if (arg1_636 > 0)
                    (l0
                      (if (arg2_639 >= 0)
                        (l0 (k_main_1216 arg2_639 arg1_636 (fun (main_611:unit) -> end) (arg2_639 <= 0)))
                        (l1 (k_main_1216 arg2_639 arg1_636 (fun (main_611:unit) -> end) false))))
                    (l1 (k_main_1216 arg2_639 arg1_636 (fun (main_611:unit) -> end) false)))))))
abst_arg: arg1_636, int;;
abst_arg: arg1_636, int;;
abst_arg: arg2_639, int;;
abst_arg: arg2_639, int;;
cond: true
pbs: 
p:(arg1_636 > 0)
xs: 0, qs: 0
nxs: 0, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
tt:false
ff:false

cond: (arg1_636 > 0); true
pbs: 
p:(arg2_639 >= 0)
xs: 0, qs: 0
nxs: 0, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
tt:false
ff:false

abst_arg: main_611, unit;;
abst_arg: main_611, unit;;
filter
  cond: (arg2_639 >= 0); (arg1_636 > 0); true
  orig pbs: 
  unsat:false

cond: (arg2_639 >= 0); (arg1_636 > 0); true
pbs: 
p:(((1 <= (((-1) * arg2_639) + arg1_636)) && (arg2_639 <= 0)) || (not (arg2_639 <= 0)))
tt:true
ff:false

cond: (arg2_639 >= 0); (arg1_636 > 0); true
pbs: 
p:(arg2_639 <= 0)
xs: 0, qs: 0
nxs: 0, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
tt:false
ff:false

filter
  cond: (arg2_639 >= 0); (arg1_636 > 0); true
  orig pbs: 
  unsat:false

abst_arg: main_611, unit;;
abst_arg: main_611, unit;;
filter
  cond: (not (arg2_639 >= 0)); (arg1_636 > 0); true
  orig pbs: 
  unsat:false

cond: (not (arg2_639 >= 0)); (arg1_636 > 0); true
pbs: 
p:(((1 <= (((-1) * arg2_639) + arg1_636)) && false) || (not false))
tt:true
ff:false

cond: (not (arg2_639 >= 0)); (arg1_636 > 0); true
pbs: 
p:false
tt:false
ff:true

filter
  cond: (not (arg2_639 >= 0)); (arg1_636 > 0); true
  orig pbs: 
  unsat:false

abst_arg: main_611, unit;;
abst_arg: main_611, unit;;
filter
  cond: (not (arg1_636 > 0)); true
  orig pbs: 
  unsat:false

cond: (not (arg1_636 > 0)); true
pbs: 
p:(((1 <= (((-1) * arg2_639) + arg1_636)) && false) || (not false))
tt:true
ff:false

cond: (not (arg1_636 > 0)); true
pbs: 
p:false
tt:false
ff:true

filter
  cond: (not (arg1_636 > 0)); true
  orig pbs: 
  unsat:false

filter
  cond: true
  orig pbs: 
  unsat:false

filter
  cond: true
  orig pbs: 
  unsat:false

ASSUME:
  cond: 
  pbs: 
  t1: true
filter
  cond: true
  orig pbs: 
  unsat:false

array_max_1011: ENV: x_1028:int, i_1123:int, x_1124:(int -> (int -> X) -> X), m_1125:int[
1 <= x_1028 - i_1123], k_array_max_1153:(int[i_1123 >= x_1028] -> X),


abst_arg: x_1028, int;;
abst_arg: i_1123, int;;
abst_arg: x_1124, (int -> (int -> X) -> X);;
abst_arg: m_1125, int[1 <= x_1028 - i_1123];;
abst_arg: k_array_max_1153, (int[i_1123 >= x_1028] ->
X);;
abst_arg: x_1028, int;;
abst_arg: i_1123, int;;
abst_arg: x_1124, (int -> (int -> X) -> X);;
abst_arg: m_1125, int[1 <= x_1028 - i_1123];;
abst_arg: k_array_max_1153, (int[i_1123 >= x_1028] ->
X);;
array_max_1011: (l0 (k_array_max_1153 m_1125)) ===> (l0 (k_array_max_1153 m_1125))
array_max_1011:: (l0 (k_array_max_1153 m_1125))
cond: (i_1123 >= x_1028)
pbs: 
p:(i_1123 >= x_1028)
tt:true
ff:false

filter
  cond: (i_1123 >= x_1028)
  orig pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
xs: 1, qs: 0
nxs: 1, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:(m_1125 || m_1125)

ASSUME:
  cond: 
  pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
  t1: (i_1123 >= x_1028)
filter
  cond: (i_1123 >= x_1028)
  orig pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
xs: 1, qs: 0
nxs: 1, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:(m_1125 || m_1125)

array_max_1011: ENV: x_1028:int, i_1123:int, x_1124:(int -> (int -> X) -> X), m_1125:int[
1 <= x_1028 - i_1123], k_array_max_1153:(int[i_1123 >= x_1028] -> X),


abst_arg: x_1028, int;;
abst_arg: i_1123, int;;
abst_arg: x_1124, (int -> (int -> X) -> X);;
abst_arg: m_1125, int[1 <= x_1028 - i_1123];;
abst_arg: k_array_max_1153, (int[i_1123 >= x_1028] ->
X);;
abst_arg: x_1028, int;;
abst_arg: i_1123, int;;
abst_arg: x_1124, (int -> (int -> X) -> X);;
abst_arg: m_1125, int[1 <= x_1028 - i_1123];;
abst_arg: k_array_max_1153, (int[i_1123 >= x_1028] ->
X);;
array_max_1011: (l1
                  (x_1124 i_1123
                    (fun (x_617:int) ->
                     (if (x_617 > m_1125)
                       (l0
                         (array_max_1011 x_1028 (i_1123 + 1)
                           (fun (x__674:int) (x__675:(int -> X)) ->
                            (x_1124 x__674 (fun (x__676:int) -> (x__675 x__676)))) x_617
                           (fun (x__673:int[i_1123 + 1 >= x_1028]) -> (k_array_max_1153 x__673))))
                       (l1
                         (array_max_1011 x_1028 (i_1123 + 1)
                           (fun (x__670:int) (x__671:(int -> X)) ->
                            (x_1124 x__670 (fun (x__672:int) -> (x__671 x__672)))) m_1125
                           (fun (x__669:int[i_1123 + 1 >= x_1028]) -> (k_array_max_1153 x__669)))))))) ===> (
l1
 (x_1124 i_1123
   (fun (x_617:int) ->
    (if (x_617 > m_1125)
      (l0
        (array_max_1011 x_1028 (i_1123 + 1)
          (fun (x__674:int) (x__675:(int -> X)) -> (x_1124 x__674 (fun (x__676:int) -> (x__675 x__676)))) x_617
          (fun (x__673:int[i_1123 + 1 >= x_1028]) -> (k_array_max_1153 x__673))))
      (l1
        (array_max_1011 x_1028 (i_1123 + 1)
          (fun (x__670:int) (x__671:(int -> X)) -> (x_1124 x__670 (fun (x__672:int) -> (x__671 x__672)))) m_1125
          (fun (x__669:int[i_1123 + 1 >= x_1028]) -> (k_array_max_1153 x__669))))))))
array_max_1011:: (l1
                   (x_1124 i_1123
                     (fun (x_617:int) ->
                      (if (x_617 > m_1125)
                        (l0
                          (array_max_1011 x_1028 (i_1123 + 1)
                            (fun (x__674:int) (x__675:(int -> X)) ->
                             (x_1124 x__674 (fun (x__676:int) -> (x__675 x__676)))) x_617
                            (fun (x__673:int[i_1123 + 1 >= x_1028]) -> (k_array_max_1153 x__673))))
                        (l1
                          (array_max_1011 x_1028 (i_1123 + 1)
                            (fun (x__670:int) (x__671:(int -> X)) ->
                             (x_1124 x__670 (fun (x__672:int) -> (x__671 x__672)))) m_1125
                            (fun (x__669:int[i_1123 + 1 >= x_1028]) -> (k_array_max_1153 x__669))))))))
abst_arg: x_617, int;;
abst_arg: x_617, int;;
cond: (not (i_1123 >= x_1028))
pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
p:(x_617 > m_1125)
xs: 0, qs: 0
nxs: 0, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
tt:false
ff:false

abst_arg: x__674, int;;
abst_arg: x__675, (int ->
X);;
abst_arg: x__674, int;;
abst_arg: x__675, (int ->
X);;
abst_arg: x__676, int;;
abst_arg: x__676, int;;
filter
  cond: (x_617 > m_1125); (not (i_1123 >= x_1028))
  orig pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
xs: 0, qs: 1
nxs: 1, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (x_617 > m_1125); (not (i_1123 >= x_1028))
  orig pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
xs: 0, qs: 1
nxs: 1, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (x_617 > m_1125); (not (i_1123 >= x_1028))
  orig pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
xs: 0, qs: 1
nxs: 1, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (x_617 > m_1125); (not (i_1123 >= x_1028))
  orig pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
xs: 0, qs: 1
nxs: 1, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

cond: (x_617 > m_1125); (not (i_1123 >= x_1028))
pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
p:(1 <= (x_1028 - (1 * (i_1123 + 1))))
fvsp: 2
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 1
ws2: 1
ws3: 0
ws4: 0
tt:false
ff:false

abst_arg: x__673, int[i_1123 + 1 >= x_1028];;
abst_arg: x__673, int[i_1123 + 1 >= x_1028];;
cond: (x_617 > m_1125); (not (i_1123 >= x_1028))
pbs: x__673 := ((i_1123 + 1) >= x_1028);
     m_1125 := (1 <= (x_1028 - (1 * i_1123)))
p:(i_1123 >= x_1028)
tt:false
ff:true

filter
  cond: (x_617 > m_1125); (not (i_1123 >= x_1028))
  orig pbs: x__673 := ((i_1123 + 1) >= x_1028);
            m_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: x__673 := ((i_1123 + 1) >= x_1028);
       m_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
fvsp: 2
xs: 0, qs: 2
nxs: 2, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (x_617 > m_1125); (not (i_1123 >= x_1028))
  orig pbs: x__673 := ((i_1123 + 1) >= x_1028);
            m_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: x__673 := ((i_1123 + 1) >= x_1028);
       m_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
fvsp: 2
xs: 0, qs: 2
nxs: 2, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (x_617 > m_1125); (not (i_1123 >= x_1028))
  orig pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
xs: 0, qs: 1
nxs: 1, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

abst_arg: x__670, int;;
abst_arg: x__671, (int ->
X);;
abst_arg: x__670, int;;
abst_arg: x__671, (int ->
X);;
abst_arg: x__672, int;;
abst_arg: x__672, int;;
filter
  cond: (not (x_617 > m_1125)); (not (i_1123 >= x_1028))
  orig pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
xs: 0, qs: 1
nxs: 1, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (not (x_617 > m_1125)); (not (i_1123 >= x_1028))
  orig pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
xs: 0, qs: 1
nxs: 1, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (not (x_617 > m_1125)); (not (i_1123 >= x_1028))
  orig pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
xs: 0, qs: 1
nxs: 1, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (not (x_617 > m_1125)); (not (i_1123 >= x_1028))
  orig pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
xs: 0, qs: 1
nxs: 1, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

cond: (not (x_617 > m_1125)); (not (i_1123 >= x_1028))
pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
p:(1 <= (x_1028 - (1 * (i_1123 + 1))))
fvsp: 2
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 1
ws2: 1
ws3: 0
ws4: 0
tt:false
ff:false

abst_arg: x__669, int[i_1123 + 1 >= x_1028];;
abst_arg: x__669, int[i_1123 + 1 >= x_1028];;
cond: (not (x_617 > m_1125)); (not (i_1123 >= x_1028))
pbs: x__669 := ((i_1123 + 1) >= x_1028);
     m_1125 := (1 <= (x_1028 - (1 * i_1123)))
p:(i_1123 >= x_1028)
tt:false
ff:true

filter
  cond: (not (x_617 > m_1125)); (not (i_1123 >= x_1028))
  orig pbs: x__669 := ((i_1123 + 1) >= x_1028);
            m_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: x__669 := ((i_1123 + 1) >= x_1028);
       m_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
fvsp: 2
xs: 0, qs: 2
nxs: 2, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (not (x_617 > m_1125)); (not (i_1123 >= x_1028))
  orig pbs: x__669 := ((i_1123 + 1) >= x_1028);
            m_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: x__669 := ((i_1123 + 1) >= x_1028);
       m_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
fvsp: 2
xs: 0, qs: 2
nxs: 2, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (not (x_617 > m_1125)); (not (i_1123 >= x_1028))
  orig pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
xs: 0, qs: 1
nxs: 1, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (not (i_1123 >= x_1028))
  orig pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
xs: 0, qs: 1
nxs: 1, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (not (i_1123 >= x_1028))
  orig pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
xs: 0, qs: 1
nxs: 1, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

ASSUME:
  cond: 
  pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
  t1: (not (i_1123 >= x_1028))
filter
  cond: (not (i_1123 >= x_1028))
  orig pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: m_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
xs: 0, qs: 1
nxs: 1, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

fail_1349: ENV: b:x_1:bool[x_1], k:(unit -> X),


abst_arg: b, x_1:bool[x_1];;
abst_arg: k, (unit ->
X);;
abst_arg: b, x_1:bool[x_1];;
abst_arg: k, (unit ->
X);;
fail_1349: (k ()) ===> (k ())
fail_1349:: (k ())
filter
  cond: true
  orig pbs: b := b
  pbs: b := b
fvsp: 1
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 1
ws2: 1
ws3: 0
ws4: 0
  unsat:false

ASSUME:
  cond: 
  pbs: b := b
  t1: true
filter
  cond: true
  orig pbs: b := b
  pbs: b := b
fvsp: 1
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 1
ws2: 1
ws3: 0
ws4: 0
  unsat:false

k_main_1216: ENV: i_1020:int, n_1019:int, k_main_1209:(unit -> X),
b_1322:x_1:bool[1 <= -i_1020 + n_1019 && x_1 || (not x_1); x_1],


abst_arg: i_1020, int;;
abst_arg: n_1019, int;;
abst_arg: k_main_1209, (unit ->
X);;
abst_arg: b_1322, x_1:bool[1 <= -i_1020 + n_1019 && x_1 || (not x_1); x_1];;
abst_arg: i_1020, int;;
abst_arg: n_1019, int;;
abst_arg: k_main_1209, (unit ->
X);;
abst_arg: b_1322, x_1:bool[1 <= -i_1020 + n_1019 && x_1 || (not x_1); x_1];;
k_main_1216: (l0
               (array_max_1011 n_1019 i_1020
                 (fun (i_652:int) (k_make_array_653:(int -> X)) -> (k_make_array_653 (n_1019 - i_652))) (-1)
                 (fun (m_634:int[i_1020 >= n_1019]) ->
                  (if (n_1019 <= m_634) (l0 (k_main_1209 ()))
                    (l1 (fail_1349 true (fun (x__677:unit) -> (k_main_1209 x__677)))))))) ===> (
l0
 (array_max_1011 n_1019 i_1020 (fun (i_652:int) (k_make_array_653:(int -> X)) -> (k_make_array_653 (n_1019 - i_652)))
   (-1)
   (fun (m_634:int[i_1020 >= n_1019]) ->
    (if (n_1019 <= m_634) (l0 (k_main_1209 ())) (l1 (fail_1349 true (fun (x__677:unit) -> (k_main_1209 x__677))))))))
k_main_1216:: (l0
                (array_max_1011 n_1019 i_1020
                  (fun (i_652:int) (k_make_array_653:(int -> X)) -> (k_make_array_653 (n_1019 - i_652))) (-1)
                  (fun (m_634:int[i_1020 >= n_1019]) ->
                   (if (n_1019 <= m_634) (l0 (k_main_1209 ()))
                     (l1 (fail_1349 true (fun (x__677:unit) -> (k_main_1209 x__677))))))))
abst_arg: i_652, int;;
abst_arg: k_make_array_653, (int ->
X);;
abst_arg: i_652, int;;
abst_arg: k_make_array_653, (int ->
X);;
filter
  cond: b_1322
  orig pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b2_1322 := b_1322
  pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b2_1322 := b_1322
fvsp: 3
fvsp: 1
xs: 0, qs: 2
nxs: 2, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: b_1322
  orig pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b2_1322 := b_1322
  pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b2_1322 := b_1322
fvsp: 3
fvsp: 1
xs: 0, qs: 2
nxs: 2, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

cond: b_1322
pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
     b2_1322 := b_1322
p:(1 <= (n_1019 - (1 * i_1020)))
fvsp: 3
fvsp: 1
xs: 1, qs: 1
nxs: 0, ys: 2
ws1: 3
ws2: 3
ws3: 0
ws4: 0
tt:b1_1322
ff:false

abst_arg: m_634, int[i_1020 >= n_1019];;
abst_arg: m_634, int[i_1020 >= n_1019];;
cond: b_1322
pbs: m_634 := (i_1020 >= n_1019);
     b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
     b2_1322 := b_1322
p:(n_1019 <= m_634)
fvsp: 2
fvsp: 3
fvsp: 1
xs: 0, qs: 3
nxs: 0, ys: 3
ws1: 6
ws2: 6
ws3: 3
ws4: 3
fvsp: 5
fvsp: 3
fvsp: 4
xs: 1, qs: 2
nxs: 1, ys: 2
ws1: 7
ws2: 7
ws3: 0
ws4: 0
tt:false
ff:false

filter
  cond: (n_1019 <= m_634); b_1322
  orig pbs: m_634 := (i_1020 >= n_1019);
            b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b2_1322 := b_1322
  pbs: m_634 := (i_1020 >= n_1019);
       b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b2_1322 := b_1322
fvsp: 2
fvsp: 3
fvsp: 1
xs: 0, qs: 3
nxs: 3, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

cond: (not (n_1019 <= m_634)); b_1322
pbs: m_634 := (i_1020 >= n_1019);
     b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
     b2_1322 := b_1322
p:true
tt:true
ff:false

abst_arg: x__677, unit;;
abst_arg: x__677, unit;;
filter
  cond: (not (n_1019 <= m_634)); b_1322
  orig pbs: m_634 := (i_1020 >= n_1019);
            b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b2_1322 := b_1322
  pbs: m_634 := (i_1020 >= n_1019);
       b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b2_1322 := b_1322
fvsp: 2
fvsp: 3
fvsp: 1
xs: 0, qs: 3
nxs: 3, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (not (n_1019 <= m_634)); b_1322
  orig pbs: m_634 := (i_1020 >= n_1019);
            b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b2_1322 := b_1322
  pbs: m_634 := (i_1020 >= n_1019);
       b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b2_1322 := b_1322
fvsp: 2
fvsp: 3
fvsp: 1
xs: 0, qs: 3
nxs: 3, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (not (n_1019 <= m_634)); b_1322
  orig pbs: m_634 := (i_1020 >= n_1019);
            b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b2_1322 := b_1322
  pbs: m_634 := (i_1020 >= n_1019);
       b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b2_1322 := b_1322
fvsp: 2
fvsp: 3
fvsp: 1
xs: 0, qs: 3
nxs: 3, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: b_1322
  orig pbs: m_634 := (i_1020 >= n_1019);
            b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b2_1322 := b_1322
  pbs: m_634 := (i_1020 >= n_1019);
       b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b2_1322 := b_1322
fvsp: 2
fvsp: 3
fvsp: 1
xs: 0, qs: 3
nxs: 2, ys: 1
ws1: 1
ws2: 1
ws3: 0
ws4: 0
  unsat:false

filter
  cond: b_1322
  orig pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b2_1322 := b_1322
  pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b2_1322 := b_1322
fvsp: 3
fvsp: 1
xs: 0, qs: 2
nxs: 2, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

ASSUME:
  cond: 
  pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b2_1322 := b_1322
  t1: b_1322
filter
  cond: b_1322
  orig pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b2_1322 := b_1322
  pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b2_1322 := b_1322
fvsp: 3
fvsp: 1
xs: 0, qs: 2
nxs: 2, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

k_main_1216: ENV: i_1020:int, n_1019:int, k_main_1209:(unit -> X),
b_1322:x_1:bool[1 <= -i_1020 + n_1019 && x_1 || (not x_1); x_1],


abst_arg: i_1020, int;;
abst_arg: n_1019, int;;
abst_arg: k_main_1209, (unit ->
X);;
abst_arg: b_1322, x_1:bool[1 <= -i_1020 + n_1019 && x_1 || (not x_1); x_1];;
abst_arg: i_1020, int;;
abst_arg: n_1019, int;;
abst_arg: k_main_1209, (unit ->
X);;
abst_arg: b_1322, x_1:bool[1 <= -i_1020 + n_1019 && x_1 || (not x_1); x_1];;
k_main_1216: (l1 (k_main_1209 ())) ===> (l1 (k_main_1209 ()))
k_main_1216:: (l1 (k_main_1209 ()))
filter
  cond: (not b_1322)
  orig pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b2_1322 := b_1322
  pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b2_1322 := b_1322
fvsp: 3
fvsp: 1
xs: 1, qs: 1
nxs: 2, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:(b2_1322 || b2_1322)

ASSUME:
  cond: 
  pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b2_1322 := b_1322
  t1: (not b_1322)
filter
  cond: (not b_1322)
  orig pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b2_1322 := b_1322
  pbs: b1_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b2_1322 := b_1322
fvsp: 3
fvsp: 1
xs: 1, qs: 1
nxs: 2, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:(b2_1322 || b2_1322)

ABST:
Main: main_1340
  main_1340 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1216 () true rand_bool)) (l1 (k_main_1216 () true false))))
        (l1 (k_main_1216 () true false)));;
  array_max_1011 x_1124 m_1125 k_array_max_1153 ->
      (if (m_1125 || m_1125) _|_ (l0 (if (m_1125 || m_1125) _|_ (k_array_max_1153 true))));;
  array_max_1011 x_1124 m_1125 k_array_max_1153 ->
      (l1
        (x_1124
          (if rand_bool (l0 (array_max_1011 x_1124 rand_bool (fun x__673 -> (k_array_max_1153 false))))
            (l1 (array_max_1011 x_1124 rand_bool (fun x__669 -> (k_array_max_1153 false)))))));;
  fail_1349 b k -> {fail} => k;;
  k_main_1216 k_main_1209 b1_1322 b2_1322 ->
      (l0
        (array_max_1011 (fun k_make_array_653 -> k_make_array_653) (if b1_1322 true rand_bool)
          (fun m_634 -> (if rand_bool (l0 k_main_1209) (l1 (fail_1349 true k_main_1209))))));;
  k_main_1216 k_main_1209 b1_1322 b2_1322 ->
      (if (b2_1322 || b2_1322) _|_ (l1 (if (b2_1322 || b2_1322) _|_ k_main_1209)));;
Types:
  main_1340 : unit
  array_max_1011 : ((unit -> unit) -> bool -> (bool -> unit) -> unit)
  array_max_1011 : ((unit -> unit) -> bool -> (bool -> unit) -> unit)
  fail_1349 : (bool -> unit -> unit)
  k_main_1216 : (unit -> bool -> bool -> unit)
  k_main_1216 : (unit -> bool -> bool -> unit)

LIFT:
Main: main_1340
  main_1340 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1216 () true rand_bool)) (l1 (k_main_1216 () true false))))
        (l1 (k_main_1216 () true false)));;
  array_max_1011 x_1124 m_1125 k_array_max_1153 ->
      (if (m_1125 || m_1125) _|_ (l0 (if (m_1125 || m_1125) _|_ (k_array_max_1153 true))));;
  array_max_1011 x_1124 m_1125 k_array_max_1153 ->
      (l1
        (x_1124
          (if rand_bool (l0 (array_max_1011 x_1124 rand_bool (f_680 k_array_max_1153)))
            (l1 (array_max_1011 x_1124 rand_bool (f_683 k_array_max_1153))))));;
  f_680 k_array_max_679 x__673 -> (k_array_max_679 false);;
  f_683 k_array_max_682 x__669 -> (k_array_max_682 false);;
  fail_1349 b k -> {fail} => k;;
  k_main_1216 k_main_1209 b1_1322 b2_1322 ->
      (l0 (array_max_1011 f_685 (if b1_1322 true rand_bool) (f_688 k_main_1209)));;
  f_685 k_make_array_653 -> k_make_array_653;;
  f_688 k_main_687 m_634 -> (if rand_bool (l0 k_main_687) (l1 (fail_1349 true k_main_687)));;
  k_main_1216 k_main_1209 b1_1322 b2_1322 ->
      (if (b2_1322 || b2_1322) _|_ (l1 (if (b2_1322 || b2_1322) _|_ k_main_1209)));;

TRANS_EAGER:
Main: main_1340
  main_1340 ->
      (let f_690 b_689 =
       (if b_689
         (l0
           (let f_692 b_691 =
            (if b_691 (l0 (let f_694 b_693 = (k_main_1216 () true b_693) in (if rand_bool (f_694 true) (f_694 false))))
              (l1 (k_main_1216 () true false)))
            in (if rand_bool (f_692 true) (f_692 false)))) (l1 (k_main_1216 () true false)))
       in (if rand_bool (f_690 true) (f_690 false)));;
  array_max_1011 x_1124 m_1125 k_array_max_1153 ->
      (let f_696 b_695 =
       (if b_695 _|_
         (l0
           (let f_698 b_697 = (if b_697 _|_ (k_array_max_1153 true)) in
            (let f_700 b_699 = (if b_699 (f_698 true) (f_698 m_1125)) in (f_700 m_1125)))))
       in (let f_702 b_701 = (if b_701 (f_696 true) (f_696 m_1125)) in (f_702 m_1125)));;
  array_max_1011 x_1124 m_1125 k_array_max_1153 ->
      (l1
        (x_1124
          (let f_704 b_703 =
           (if b_703
             (l0
               ((let f_706 b_705 = (array_max_1011 x_1124 b_705) in (if rand_bool (f_706 true) (f_706 false)))
                 (f_680 k_array_max_1153)))
             (l1
               ((let f_708 b_707 = (array_max_1011 x_1124 b_707) in (if rand_bool (f_708 true) (f_708 false)))
                 (f_683 k_array_max_1153))))
           in (if rand_bool (f_704 true) (f_704 false)))));;
  f_680 k_array_max_679 x__673 -> (k_array_max_679 false);;
  f_683 k_array_max_682 x__669 -> (k_array_max_682 false);;
  fail_1349 b k -> {fail} => k;;
  k_main_1216 k_main_1209 b1_1322 b2_1322 ->
      (l0
        ((let f_710 b_709 = (array_max_1011 f_685 b_709) in
          (let f_712 b_711 = (if b_711 (f_710 true) (if rand_bool (f_710 true) (f_710 false))) in (f_712 b1_1322)))
          (f_688 k_main_1209)));;
  f_685 k_make_array_653 -> k_make_array_653;;
  f_688 k_main_687 m_634 ->
      (let f_714 b_713 = (if b_713 (l0 k_main_687) (l1 (fail_1349 true k_main_687))) in
       (if rand_bool (f_714 true) (f_714 false)));;
  k_main_1216 k_main_1209 b1_1322 b2_1322 ->
      (let f_716 b_715 =
       (if b_715 _|_
         (l1
           (let f_718 b_717 = (if b_717 _|_ k_main_1209) in
            (let f_720 b_719 = (if b_719 (f_718 true) (f_718 b2_1322)) in (f_720 b2_1322)))))
       in (let f_722 b_721 = (if b_721 (f_716 true) (f_716 b2_1322)) in (f_722 b2_1322)));;

PUT_INTO_IF:
Main: main_1340
  main_1340 ->
      (let f_690 b_689 =
       (if b_689
         (l0
           (let f_692 b_691 =
            (if b_691 (l0 (let f_694 b_693 = (k_main_1216 () true b_693) in (if rand_bool (f_694 true) (f_694 false))))
              (l1 (k_main_1216 () true false)))
            in (if rand_bool (f_692 true) (f_692 false)))) (l1 (k_main_1216 () true false)))
       in (if rand_bool (f_690 true) (f_690 false)));;
  array_max_1011 x_1124 m_1125 k_array_max_1153 ->
      (let f_696 b_695 =
       (if b_695 _|_
         (l0
           (let f_698 b_697 = (if b_697 _|_ (k_array_max_1153 true)) in
            (let f_700 b_699 = (if b_699 (f_698 true) (f_698 m_1125)) in (f_700 m_1125)))))
       in (let f_702 b_701 = (if b_701 (f_696 true) (f_696 m_1125)) in (f_702 m_1125)));;
  array_max_1011 x_1124 m_1125 k_array_max_1153 ->
      (l1
        (x_1124
          (let f_704 b_703 =
           (if b_703
             (l0
               ((let f_706 b_705 = (array_max_1011 x_1124 b_705) in (if rand_bool (f_706 true) (f_706 false)))
                 (f_680 k_array_max_1153)))
             (l1
               ((let f_708 b_707 = (array_max_1011 x_1124 b_707) in (if rand_bool (f_708 true) (f_708 false)))
                 (f_683 k_array_max_1153))))
           in (if rand_bool (f_704 true) (f_704 false)))));;
  f_680 k_array_max_679 x__673 -> (k_array_max_679 false);;
  f_683 k_array_max_682 x__669 -> (k_array_max_682 false);;
  fail_1349 b k -> {fail} => k;;
  k_main_1216 k_main_1209 b1_1322 b2_1322 ->
      (l0
        ((let f_710 b_709 = (array_max_1011 f_685 b_709) in
          (let f_712 b_711 = (if b_711 (f_710 true) (if rand_bool (f_710 true) (f_710 false))) in (f_712 b1_1322)))
          (f_688 k_main_1209)));;
  f_685 k_make_array_653 -> k_make_array_653;;
  f_688 k_main_687 m_634 ->
      (let f_714 b_713 = (if b_713 (l0 k_main_687) (l1 (fail_1349 true k_main_687))) in
       (if rand_bool (f_714 true) (f_714 false)));;
  k_main_1216 k_main_1209 b1_1322 b2_1322 ->
      (let f_716 b_715 =
       (if b_715 _|_
         (l1
           (let f_718 b_717 = (if b_717 _|_ k_main_1209) in
            (let f_720 b_719 = (if b_719 (f_718 true) (f_718 b2_1322)) in (f_720 b2_1322)))))
       in (let f_722 b_721 = (if b_721 (f_716 true) (f_716 b2_1322)) in (f_722 b2_1322)));;

DONE!

(1-2) Checking HORS ... DONE!

Error trace::
  main_1340 ... --> 
  main_1091 ... --> 
  arg1_1087 ... --> 
  f_main_1344 ... --> 
  arg2_1089 ... --> 
  f_main_1345 ... --> 
  main_1018 [1/2] ... --> 
  br_main_1347 [1/2] ... --> 
  k_main_1216 [1/2] ... --> 
  m_1324 ... --> 
  array_max_1011 [2/2] ... --> 
  x_1126 ... --> 
  make_array_1008 ... --> 
  f_array_max_1341 ... --> 
  z_1127 [1/2] ... --> 
  f_array_max_1342 ... --> 
  array_max_1011 [1/2] ... --> 
  f_k_main_1343 [2/2] ... --> 
  fail_1349 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 1; 0

(1-3) Checking counterexample ... DONE!

(1-4) Discovering predicates ... 
DONE!

Prefix of spurious counterexample::
0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 1

Program with abstraction types (CEGAR-cycle 2)::
Main: main_1340
  main_1340 -> (main_1091 f_1346);;
  arg1_1087 k_main_arg1_1268 -> (rand_int k_main_arg1_1268);;
  arg2_1089 arg1_1298 k_main_arg2_1280 -> (rand_int k_main_arg2_1280);;
  array_max_1011 x_1028 i_1123 x_1124 m_1125 k_array_max_1153 when (i_1123 >= x_1028) -> (k_array_max_1153 m_1125);;
  array_max_1011 x_1028 i_1123 x_1124 m_1125 k_array_max_1153 when (not (i_1123 >= x_1028)) ->
      (x_1126 i_1123 m_1125 x_1028 x_1124 (f_array_max_1341 i_1123 m_1125 x_1028 k_array_max_1153 x_1124));;
  br_main_1347 b_1348 n_1019 i_1020 k_main_1209 when b_1348 -> (k_main_1216 i_1020 n_1019 k_main_1209 (i_1020 <= 0));;
  br_main_1347 b_1348 n_1019 i_1020 k_main_1209 when (not b_1348) -> (k_main_1216 i_1020 n_1019 k_main_1209 false);;
  f_1346 main_1299 -> end;;
  f_array_max_1341 i_1123 m_1125 x_1028 k_array_max_1153 x_1124 x_1192 ->
      (z_1127 i_1123 m_1125 x_1028 x_1192 (f_array_max_1342 i_1123 m_1125 x_1028 x_1192 k_array_max_1153 x_1124));;
  f_array_max_1342 i_1123 m_1125 x_1028 x_1192 k_array_max_1153 x_1124 z_1191 ->
      (array_max_1011 x_1028 (i_1123 + 1) x_1124 z_1191 k_array_max_1153);;
  f_k_main_1343 b_1322 i_1020 n_1019 k_main_1209 m_1328 when (m_1328 >= n_1019) -> (k_main_1209 ());;
  f_k_main_1343 b_1322 i_1020 n_1019 k_main_1209 m_1328 when (not (m_1328 >= n_1019)) -> (fail_1349 true k_main_1209);;
  f_main_1344 k_main_1263 arg1_1298 -> (arg2_1089 arg1_1298 (f_main_1345 arg1_1298 k_main_1263));;
  f_main_1345 arg1_1298 k_main_1263 arg2_1297 -> (main_1018 arg1_1298 arg2_1297 k_main_1263);;
  fail_1349 b k -> {fail} => (k ());;
  k_main_1216 i_1020 n_1019 k_main_1209 b_1322 when b_1322 ->
      (m_1324 b_1322 i_1020 n_1019 (f_k_main_1343 b_1322 i_1020 n_1019 k_main_1209));;
  k_main_1216 i_1020 n_1019 k_main_1209 b_1322 when (not b_1322) -> (k_main_1209 ());;
  m_1324 b_1322 i_1020 n_1019 k_main_m_1325 ->
      (array_max_1011 n_1019 i_1020 (make_array_1008 n_1019) (-1) k_main_m_1325);;
  main_1018 n_1019 i_1020 k_main_1209 when (n_1019 > 0) -> (br_main_1347 (i_1020 >= 0) n_1019 i_1020 k_main_1209);;
  main_1018 n_1019 i_1020 k_main_1209 when (not (n_1019 > 0)) -> (k_main_1216 i_1020 n_1019 k_main_1209 false);;
  main_1091 k_main_1263 -> (arg1_1087 (f_main_1344 k_main_1263));;
  make_array_1008 n_1009 i_1010 k_make_array_1142 -> (k_make_array_1142 (n_1009 - i_1010));;
  x_1126 i_1123 m_1125 x_1028 x_1124 k_array_max_x_1160 -> (x_1124 i_1123 k_array_max_x_1160);;
  z_1127 i_1123 m_1125 x_1028 x_1192 k_array_max_z_1169 when (x_1192 > m_1125) -> (k_array_max_z_1169 x_1192);;
  z_1127 i_1123 m_1125 x_1028 x_1192 k_array_max_z_1169 when (not (x_1192 > m_1125)) -> (k_array_max_z_1169 m_1125);;
Types:
  main_1340 : X
  array_max_1011 : (x_1:int ->
                    x_2:int ->
                    (x_4:int[x_4 <= 0] -> (x_6:int[x_6 >= x_1] -> X) -> X) ->
                    x_9:int[x_2 <= 0; 1 <= x_1 - x_2] -> (x_11:int[x_11 >= x_9; x_11 >= x_1; x_2 >= x_1] -> X) -> X)
  fail_1349 : (x_1:bool[x_1] -> (unit -> X) -> X)
  k_main_1216 : (x_1:int ->
                 x_2:int ->
                 (unit -> X) -> x_6:bool[x_1 <= 0 || (not x_6); 1 <= -x_1 + x_2 && x_6 || (not x_6); x_6] -> X)

(2-1) Abstracting ... EXPAND_NONREC:
Main: main_1340
  main_1340 ->
      (rand_int
        (fun arg1_1207 ->
         (rand_int
           (fun arg2_1210 ->
            (if (arg1_1207 > 0)
              (l0
                (if (arg2_1210 >= 0) (l0 (k_main_1216 arg2_1210 arg1_1207 (fun main_1182 -> end) (arg2_1210 <= 0)))
                  (l1 (k_main_1216 arg2_1210 arg1_1207 (fun main_1182 -> end) false))))
              (l1 (k_main_1216 arg2_1210 arg1_1207 (fun main_1182 -> end) false)))))));;
  array_max_1011 x_1028 i_1123 x_1124 m_1125 k_array_max_1153 when (i_1123 >= x_1028) ->
      (l0 (k_array_max_1153 m_1125));;
  array_max_1011 x_1028 i_1123 x_1124 m_1125 k_array_max_1153 when (not (i_1123 >= x_1028)) ->
      (l1
        (x_1124 i_1123
          (fun x_1188 ->
           (if (x_1188 > m_1125) (l0 (array_max_1011 x_1028 (i_1123 + 1) x_1124 x_1188 k_array_max_1153))
             (l1 (array_max_1011 x_1028 (i_1123 + 1) x_1124 m_1125 k_array_max_1153))))));;
  fail_1349 b k -> {fail} => (k ());;
  k_main_1216 i_1020 n_1019 k_main_1209 b_1322 when b_1322 ->
      (l0
        (array_max_1011 n_1019 i_1020 (fun i_1223 k_make_array_1224 -> (k_make_array_1224 (n_1019 - i_1223))) (-1)
          (fun m_1205 -> (if (n_1019 <= m_1205) (l0 (k_main_1209 ())) (l1 (fail_1349 true k_main_1209))))));;
  k_main_1216 i_1020 n_1019 k_main_1209 b_1322 when (not b_1322) -> (l1 (k_main_1209 ()));;

ETA_EXPAND:
Main: main_1340
  main_1340 ->
      (rand_int
        (fun (arg1_1207:int) ->
         (rand_int
           (fun (arg2_1210:int) ->
            (if (arg1_1207 > 0)
              (l0
                (if (arg2_1210 >= 0)
                  (l0 (k_main_1216 arg2_1210 arg1_1207 (fun (main_1182:unit) -> end) (arg2_1210 <= 0)))
                  (l1 (k_main_1216 arg2_1210 arg1_1207 (fun (main_1182:unit) -> end) false))))
              (l1 (k_main_1216 arg2_1210 arg1_1207 (fun (main_1182:unit) -> end) false)))))));;
  array_max_1011 x_1028 i_1123 x_1124 m_1125 k_array_max_1153 when (i_1123 >= x_1028) ->
      (l0 (k_array_max_1153 m_1125));;
  array_max_1011 x_1028 i_1123 x_1124 m_1125 k_array_max_1153 when (not (i_1123 >= x_1028)) ->
      (l1
        (x_1124 i_1123
          (fun (x_1188:x_1:int[x_1 >= x_1028]) ->
           (if (x_1188 > m_1125)
             (l0
               (array_max_1011 x_1028 (i_1123 + 1)
                 (fun (x__1245:x_1:int[x_1 <= 0]) (x__1246:(x_1:int[x_1 >= x_1028] -> X)) ->
                  (x_1124 x__1245 (fun (x__1247:x_1:int[x_1 >= x_1028]) -> (x__1246 x__1247)))) x_1188
                 (fun (x__1244:x_1:int[x_1 >= x_1188; x_1 >= x_1028; i_1123 + 1 >= x_1028]) ->
                  (k_array_max_1153 x__1244))))
             (l1
               (array_max_1011 x_1028 (i_1123 + 1)
                 (fun (x__1241:x_1:int[x_1 <= 0]) (x__1242:(x_1:int[x_1 >= x_1028] -> X)) ->
                  (x_1124 x__1241 (fun (x__1243:x_1:int[x_1 >= x_1028]) -> (x__1242 x__1243)))) m_1125
                 (fun (x__1240:x_1:int[x_1 >= m_1125; x_1 >= x_1028; i_1123 + 1 >= x_1028]) ->
                  (k_array_max_1153 x__1240))))))));;
  fail_1349 b k -> {fail} => (k ());;
  k_main_1216 i_1020 n_1019 k_main_1209 b_1322 when b_1322 ->
      (l0
        (array_max_1011 n_1019 i_1020
          (fun (i_1223:x_1:int[x_1 <= 0]) (k_make_array_1224:(x_1:int[x_1 >= n_1019] -> X)) ->
           (k_make_array_1224 (n_1019 - i_1223))) (-1)
          (fun (m_1205:x_1:int[x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019]) ->
           (if (n_1019 <= m_1205) (l0 (k_main_1209 ()))
             (l1 (fail_1349 true (fun (x__1248:unit) -> (k_main_1209 x__1248))))))));;
  k_main_1216 i_1020 n_1019 k_main_1209 b_1322 when (not b_1322) -> (l1 (k_main_1209 ()));;

main_1340: ENV: 

main_1340: (rand_int
             (fun (arg1_1207:int) ->
              (rand_int
                (fun (arg2_1210:int) ->
                 (if (arg1_1207 > 0)
                   (l0
                     (if (arg2_1210 >= 0)
                       (l0 (k_main_1216 arg2_1210 arg1_1207 (fun (main_1182:unit) -> end) (arg2_1210 <= 0)))
                       (l1 (k_main_1216 arg2_1210 arg1_1207 (fun (main_1182:unit) -> end) false))))
                   (l1 (k_main_1216 arg2_1210 arg1_1207 (fun (main_1182:unit) -> end) false))))))) ===> (
rand_int
 (fun (arg1_1207:int) ->
  (rand_int
    (fun (arg2_1210:int) ->
     (if (arg1_1207 > 0)
       (l0
         (if (arg2_1210 >= 0) (l0 (k_main_1216 arg2_1210 arg1_1207 (fun (main_1182:unit) -> end) (arg2_1210 <= 0)))
           (l1 (k_main_1216 arg2_1210 arg1_1207 (fun (main_1182:unit) -> end) false))))
       (l1 (k_main_1216 arg2_1210 arg1_1207 (fun (main_1182:unit) -> end) false)))))))
main_1340:: (rand_int
              (fun (arg1_1207:int) ->
               (rand_int
                 (fun (arg2_1210:int) ->
                  (if (arg1_1207 > 0)
                    (l0
                      (if (arg2_1210 >= 0)
                        (l0 (k_main_1216 arg2_1210 arg1_1207 (fun (main_1182:unit) -> end) (arg2_1210 <= 0)))
                        (l1 (k_main_1216 arg2_1210 arg1_1207 (fun (main_1182:unit) -> end) false))))
                    (l1 (k_main_1216 arg2_1210 arg1_1207 (fun (main_1182:unit) -> end) false)))))))
abst_arg: arg1_1207, int;;
abst_arg: arg1_1207, int;;
abst_arg: arg2_1210, int;;
abst_arg: arg2_1210, int;;
cond: true
pbs: 
p:(arg1_1207 > 0)
xs: 0, qs: 0
nxs: 0, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
tt:false
ff:false

cond: (arg1_1207 > 0); true
pbs: 
p:(arg2_1210 >= 0)
xs: 0, qs: 0
nxs: 0, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
tt:false
ff:false

abst_arg: main_1182, unit;;
abst_arg: main_1182, unit;;
filter
  cond: (arg2_1210 >= 0); (arg1_1207 > 0); true
  orig pbs: 
  unsat:false

cond: (arg2_1210 >= 0); (arg1_1207 > 0); true
pbs: 
p:((arg2_1210 <= 0) || (not (arg2_1210 <= 0)))
tt:true
ff:false

cond: (arg2_1210 >= 0); (arg1_1207 > 0); true
pbs: 
p:(((1 <= (((-1) * arg2_1210) + arg1_1207)) && (arg2_1210 <= 0)) || (not (arg2_1210 <= 0)))
tt:true
ff:false

cond: (arg2_1210 >= 0); (arg1_1207 > 0); true
pbs: 
p:(arg2_1210 <= 0)
xs: 0, qs: 0
nxs: 0, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
tt:false
ff:false

filter
  cond: (arg2_1210 >= 0); (arg1_1207 > 0); true
  orig pbs: 
  unsat:false

abst_arg: main_1182, unit;;
abst_arg: main_1182, unit;;
filter
  cond: (not (arg2_1210 >= 0)); (arg1_1207 > 0); true
  orig pbs: 
  unsat:false

cond: (not (arg2_1210 >= 0)); (arg1_1207 > 0); true
pbs: 
p:((arg2_1210 <= 0) || (not false))
tt:true
ff:false

cond: (not (arg2_1210 >= 0)); (arg1_1207 > 0); true
pbs: 
p:(((1 <= (((-1) * arg2_1210) + arg1_1207)) && false) || (not false))
tt:true
ff:false

cond: (not (arg2_1210 >= 0)); (arg1_1207 > 0); true
pbs: 
p:false
tt:false
ff:true

filter
  cond: (not (arg2_1210 >= 0)); (arg1_1207 > 0); true
  orig pbs: 
  unsat:false

abst_arg: main_1182, unit;;
abst_arg: main_1182, unit;;
filter
  cond: (not (arg1_1207 > 0)); true
  orig pbs: 
  unsat:false

cond: (not (arg1_1207 > 0)); true
pbs: 
p:((arg2_1210 <= 0) || (not false))
tt:true
ff:false

cond: (not (arg1_1207 > 0)); true
pbs: 
p:(((1 <= (((-1) * arg2_1210) + arg1_1207)) && false) || (not false))
tt:true
ff:false

cond: (not (arg1_1207 > 0)); true
pbs: 
p:false
tt:false
ff:true

filter
  cond: (not (arg1_1207 > 0)); true
  orig pbs: 
  unsat:false

filter
  cond: true
  orig pbs: 
  unsat:false

filter
  cond: true
  orig pbs: 
  unsat:false

ASSUME:
  cond: 
  pbs: 
  t1: true
filter
  cond: true
  orig pbs: 
  unsat:false

array_max_1011: ENV: x_1028:int, i_1123:int, x_1124:(x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X),
m_1125:int[i_1123 <= 0; 1 <= x_1028 - i_1123],
k_array_max_1153:(x_1:int[x_1 >= m_1125; x_1 >= x_1028; i_1123 >= x_1028] -> X),


abst_arg: x_1028, int;;
abst_arg: i_1123, int;;
abst_arg: x_1124, (x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X);;
abst_arg: m_1125, int[i_1123 <= 0; 1 <= x_1028 - i_1123];;
abst_arg: k_array_max_1153, (x_1:int[x_1 >= m_1125; x_1 >= x_1028; i_1123 >= x_1028] ->
X);;
abst_arg: x_1028, int;;
abst_arg: i_1123, int;;
abst_arg: x_1124, (x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X);;
abst_arg: m_1125, int[i_1123 <= 0; 1 <= x_1028 - i_1123];;
abst_arg: k_array_max_1153, (x_1:int[x_1 >= m_1125; x_1 >= x_1028; i_1123 >= x_1028] ->
X);;
array_max_1011: (l0 (k_array_max_1153 m_1125)) ===> (l0 (k_array_max_1153 m_1125))
array_max_1011:: (l0 (k_array_max_1153 m_1125))
cond: (i_1123 >= x_1028)
pbs: m1_1125 := (i_1123 <= 0)
p:(m_1125 >= m_1125)
tt:true
ff:false

cond: (i_1123 >= x_1028)
pbs: m1_1125 := (i_1123 <= 0)
p:(m_1125 >= x_1028)
fvsp: 1
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 1
ws2: 1
ws3: 0
ws4: 0
tt:false
ff:false

cond: (i_1123 >= x_1028)
pbs: m1_1125 := (i_1123 <= 0)
p:(i_1123 >= x_1028)
tt:true
ff:false

filter
  cond: (i_1123 >= x_1028)
  orig pbs: m1_1125 := (i_1123 <= 0);
            m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: m1_1125 := (i_1123 <= 0);
       m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 1
fvsp: 2
xs: 1, qs: 1
nxs: 2, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:(m2_1125 || m2_1125)

ASSUME:
  cond: 
  pbs: m1_1125 := (i_1123 <= 0);
       m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
  t1: (i_1123 >= x_1028)
filter
  cond: (i_1123 >= x_1028)
  orig pbs: m1_1125 := (i_1123 <= 0);
            m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: m1_1125 := (i_1123 <= 0);
       m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 1
fvsp: 2
xs: 1, qs: 1
nxs: 2, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:(m2_1125 || m2_1125)

array_max_1011: ENV: x_1028:int, i_1123:int, x_1124:(x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X),
m_1125:int[i_1123 <= 0; 1 <= x_1028 - i_1123],
k_array_max_1153:(x_1:int[x_1 >= m_1125; x_1 >= x_1028; i_1123 >= x_1028] -> X),


abst_arg: x_1028, int;;
abst_arg: i_1123, int;;
abst_arg: x_1124, (x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X);;
abst_arg: m_1125, int[i_1123 <= 0; 1 <= x_1028 - i_1123];;
abst_arg: k_array_max_1153, (x_1:int[x_1 >= m_1125; x_1 >= x_1028; i_1123 >= x_1028] ->
X);;
abst_arg: x_1028, int;;
abst_arg: i_1123, int;;
abst_arg: x_1124, (x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X);;
abst_arg: m_1125, int[i_1123 <= 0; 1 <= x_1028 - i_1123];;
abst_arg: k_array_max_1153, (x_1:int[x_1 >= m_1125; x_1 >= x_1028; i_1123 >= x_1028] ->
X);;
array_max_1011: (l1
                  (x_1124 i_1123
                    (fun (x_1188:x_1:int[x_1 >= x_1028]) ->
                     (if (x_1188 > m_1125)
                       (l0
                         (array_max_1011 x_1028 (i_1123 + 1)
                           (fun (x__1245:x_1:int[x_1 <= 0]) (x__1246:(x_1:int[x_1 >= x_1028] -> X)) ->
                            (x_1124 x__1245 (fun (x__1247:x_1:int[x_1 >= x_1028]) -> (x__1246 x__1247)))) x_1188
                           (fun (x__1244:x_1:int[x_1 >= x_1188; x_1 >= x_1028; i_1123 + 1 >= x_1028]) ->
                            (k_array_max_1153 x__1244))))
                       (l1
                         (array_max_1011 x_1028 (i_1123 + 1)
                           (fun (x__1241:x_1:int[x_1 <= 0]) (x__1242:(x_1:int[x_1 >= x_1028] -> X)) ->
                            (x_1124 x__1241 (fun (x__1243:x_1:int[x_1 >= x_1028]) -> (x__1242 x__1243)))) m_1125
                           (fun (x__1240:x_1:int[x_1 >= m_1125; x_1 >= x_1028; i_1123 + 1 >= x_1028]) ->
                            (k_array_max_1153 x__1240)))))))) ===> (l1
                                                                    (x_1124 i_1123
                                                                    (fun 
                                                                    (x_1188:x_1:int[x_1 >= x_1028]) ->
                                                                    (if (
                                                                    x_1188 > m_1125)
                                                                    (l0
                                                                    (array_max_1011 x_1028 (
                                                                    i_1123 + 1)
                                                                    (fun 
                                                                    (x__1245:x_1:int[
                                                                    x_1 <= 0]) (x__1246:(x_1:int[
                                                                    x_1 >= x_1028] -> X)) ->
                                                                    (x_1124 x__1245
                                                                    (fun 
                                                                    (x__1247:x_1:int[x_1 >= x_1028]) ->
                                                                    (x__1246 x__1247)))) x_1188
                                                                    (fun 
                                                                    (x__1244:x_1:int[
                                                                    x_1 >= x_1188; x_1 >= x_1028; i_1123 + 1 >= x_1028])
                                                                    -> (
                                                                    k_array_max_1153 x__1244))))
                                                                    (l1
                                                                    (array_max_1011 x_1028 (
                                                                    i_1123 + 1)
                                                                    (fun 
                                                                    (x__1241:x_1:int[
                                                                    x_1 <= 0]) (x__1242:(x_1:int[
                                                                    x_1 >= x_1028] -> X)) ->
                                                                    (x_1124 x__1241
                                                                    (fun 
                                                                    (x__1243:x_1:int[x_1 >= x_1028]) ->
                                                                    (x__1242 x__1243)))) m_1125
                                                                    (fun 
                                                                    (x__1240:x_1:int[
                                                                    x_1 >= m_1125; x_1 >= x_1028; i_1123 + 1 >= x_1028])
                                                                    -> (
                                                                    k_array_max_1153 x__1240))))))))
array_max_1011:: (l1
                   (x_1124 i_1123
                     (fun (x_1188:x_1:int[x_1 >= x_1028]) ->
                      (if (x_1188 > m_1125)
                        (l0
                          (array_max_1011 x_1028 (i_1123 + 1)
                            (fun (x__1245:x_1:int[x_1 <= 0]) (x__1246:(x_1:int[x_1 >= x_1028] -> X)) ->
                             (x_1124 x__1245 (fun (x__1247:x_1:int[x_1 >= x_1028]) -> (x__1246 x__1247)))) x_1188
                            (fun (x__1244:x_1:int[x_1 >= x_1188; x_1 >= x_1028; i_1123 + 1 >= x_1028]) ->
                             (k_array_max_1153 x__1244))))
                        (l1
                          (array_max_1011 x_1028 (i_1123 + 1)
                            (fun (x__1241:x_1:int[x_1 <= 0]) (x__1242:(x_1:int[x_1 >= x_1028] -> X)) ->
                             (x_1124 x__1241 (fun (x__1243:x_1:int[x_1 >= x_1028]) -> (x__1242 x__1243)))) m_1125
                            (fun (x__1240:x_1:int[x_1 >= m_1125; x_1 >= x_1028; i_1123 + 1 >= x_1028]) ->
                             (k_array_max_1153 x__1240))))))))
cond: (not (i_1123 >= x_1028))
pbs: m1_1125 := (i_1123 <= 0);
     m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
p:(i_1123 <= 0)
fvsp: 1
fvsp: 2
xs: 1, qs: 1
nxs: 0, ys: 2
ws1: 3
ws2: 3
ws3: 0
ws4: 0
tt:m1_1125
ff:false

abst_arg: x_1188, x_1:int[x_1 >= x_1028];;
abst_arg: x_1188, x_1:int[x_1 >= x_1028];;
cond: (not (i_1123 >= x_1028))
pbs: x_1188 := (x_1188 >= x_1028);
     m1_1125 := (i_1123 <= 0);
     m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
p:(x_1188 > m_1125)
fvsp: 2
fvsp: 1
fvsp: 2
xs: 0, qs: 3
nxs: 0, ys: 3
ws1: 6
ws2: 6
ws3: 3
ws4: 3
fvsp: 3
fvsp: 4
fvsp: 3
xs: 0, qs: 3
nxs: 0, ys: 3
ws1: 7
ws2: 7
ws3: 1
ws4: 1
fvsp: 5
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 7
ws2: 7
ws3: 0
ws4: 0
tt:false
ff:false

abst_arg: x__1245, x_1:int[x_1 <= 0];;
abst_arg: x__1246, (x_1:int[x_1 >= x_1028] ->
X);;
abst_arg: x__1245, x_1:int[x_1 <= 0];;
abst_arg: x__1246, (x_1:int[x_1 >= x_1028] ->
X);;
cond: (x_1188 > m_1125); (not (i_1123 >= x_1028))
pbs: x__1245 := (x__1245 <= 0);
     x_1188 := (x_1188 >= x_1028);
     m1_1125 := (i_1123 <= 0);
     m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
p:(x__1245 <= 0)
fvsp: 1
xs: 1, qs: 0
nxs: 0, ys: 1
ws1: 1
ws2: 1
ws3: 0
ws4: 0
tt:x__1245
ff:false

abst_arg: x__1247, x_1:int[x_1 >= x_1028];;
abst_arg: x__1247, x_1:int[x_1 >= x_1028];;
cond: (x_1188 > m_1125); (not (i_1123 >= x_1028))
pbs: x__1247 := (x__1247 >= x_1028);
     x__1245 := (x__1245 <= 0);
     x_1188 := (x_1188 >= x_1028);
     m1_1125 := (i_1123 <= 0);
     m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
p:(x__1247 >= x_1028)
fvsp: 2
fvsp: 2
fvsp: 1
fvsp: 2
xs: 1, qs: 3
nxs: 0, ys: 4
ws1: 10
ws2: 10
ws3: 3
ws4: 3
fvsp: 3
fvsp: 4
fvsp: 3
xs: 0, qs: 3
nxs: 0, ys: 3
ws1: 14
ws2: 14
ws3: 1
ws4: 1
fvsp: 5
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 15
ws2: 14
ws3: 0
ws4: 0
tt:x__1247
ff:false

filter
  cond: (x_1188 > m_1125); (not (i_1123 >= x_1028))
  orig pbs: x__1247 := (x__1247 >= x_1028);
            x__1245 := (x__1245 <= 0);
            x_1188 := (x_1188 >= x_1028);
            m1_1125 := (i_1123 <= 0);
            m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: x__1247 := (x__1247 >= x_1028);
       x_1188 := (x_1188 >= x_1028);
       m1_1125 := (i_1123 <= 0);
       m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
fvsp: 2
fvsp: 1
fvsp: 2
xs: 0, qs: 4
nxs: 4, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  pbs: x__1245 := (x__1245 <= 0)
fvsp: 1
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 1
ws2: 1
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (x_1188 > m_1125); (not (i_1123 >= x_1028))
  orig pbs: x__1247 := (x__1247 >= x_1028);
            x__1245 := (x__1245 <= 0);
            x_1188 := (x_1188 >= x_1028);
            m1_1125 := (i_1123 <= 0);
            m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: x__1247 := (x__1247 >= x_1028);
       x_1188 := (x_1188 >= x_1028);
       m1_1125 := (i_1123 <= 0);
       m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
fvsp: 2
fvsp: 1
fvsp: 2
xs: 0, qs: 4
nxs: 4, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  pbs: x__1245 := (x__1245 <= 0)
fvsp: 1
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 1
ws2: 1
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (x_1188 > m_1125); (not (i_1123 >= x_1028))
  orig pbs: x__1245 := (x__1245 <= 0);
            x_1188 := (x_1188 >= x_1028);
            m1_1125 := (i_1123 <= 0);
            m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: x_1188 := (x_1188 >= x_1028);
       m1_1125 := (i_1123 <= 0);
       m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
fvsp: 1
fvsp: 2
xs: 0, qs: 3
nxs: 3, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  pbs: x__1245 := (x__1245 <= 0)
fvsp: 1
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 1
ws2: 1
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (x_1188 > m_1125); (not (i_1123 >= x_1028))
  orig pbs: x__1245 := (x__1245 <= 0);
            x_1188 := (x_1188 >= x_1028);
            m1_1125 := (i_1123 <= 0);
            m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: x_1188 := (x_1188 >= x_1028);
       m1_1125 := (i_1123 <= 0);
       m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
fvsp: 1
fvsp: 2
xs: 0, qs: 3
nxs: 3, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  pbs: x__1245 := (x__1245 <= 0)
fvsp: 1
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 1
ws2: 1
ws3: 0
ws4: 0
  unsat:false

cond: (x_1188 > m_1125); (not (i_1123 >= x_1028))
pbs: x_1188 := (x_1188 >= x_1028);
     m1_1125 := (i_1123 <= 0);
     m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
p:((i_1123 + 1) <= 0)
fvsp: 2
fvsp: 1
fvsp: 2
xs: 0, qs: 3
nxs: 0, ys: 3
ws1: 6
ws2: 6
ws3: 3
ws4: 3
fvsp: 3
fvsp: 4
fvsp: 3
xs: 0, qs: 3
nxs: 0, ys: 3
ws1: 7
ws2: 7
ws3: 1
ws4: 1
fvsp: 5
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 7
ws2: 7
ws3: 0
ws4: 0
tt:false
ff:false

cond: (x_1188 > m_1125); (not (i_1123 >= x_1028))
pbs: x_1188 := (x_1188 >= x_1028);
     m1_1125 := (i_1123 <= 0);
     m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
p:(1 <= (x_1028 - (1 * (i_1123 + 1))))
fvsp: 2
fvsp: 1
fvsp: 2
xs: 0, qs: 3
nxs: 0, ys: 3
ws1: 6
ws2: 6
ws3: 3
ws4: 3
fvsp: 3
fvsp: 4
fvsp: 3
xs: 0, qs: 3
nxs: 0, ys: 3
ws1: 7
ws2: 7
ws3: 1
ws4: 1
fvsp: 5
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 7
ws2: 7
ws3: 0
ws4: 0
tt:false
ff:false

abst_arg: x__1244, x_1:int[x_1 >= x_1188; x_1 >= x_1028; i_1123 + 1 >= x_1028];;
abst_arg: x__1244, x_1:int[x_1 >= x_1188; x_1 >= x_1028; i_1123 + 1 >= x_1028];;
cond: (x_1188 > m_1125); (not (i_1123 >= x_1028))
pbs: x_3_1244 := ((i_1123 + 1) >= x_1028);
     x_2_1244 := (x__1244 >= x_1028);
     x_1_1244 := (x__1244 >= x_1188);
     x_1188 := (x_1188 >= x_1028);
     m1_1125 := (i_1123 <= 0);
     m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
p:(x__1244 >= m_1125)
fvsp: 2
fvsp: 2
fvsp: 2
fvsp: 2
fvsp: 1
fvsp: 2
xs: 1, qs: 5
nxs: 0, ys: 6
ws1: 21
ws2: 21
ws3: 10
ws4: 10
fvsp: 4
fvsp: 4
fvsp: 4
fvsp: 3
fvsp: 3
fvsp: 3
fvsp: 4
fvsp: 4
fvsp: 4
fvsp: 3
xs: 0, qs: 10
nxs: 0, ys: 10
ws1: 46
ws2: 41
ws3: 10
ws4: 10
fvsp: 6
fvsp: 5
fvsp: 5
fvsp: 5
fvsp: 6
fvsp: 6
fvsp: 6
fvsp: 5
fvsp: 5
fvsp: 5
xs: 0, qs: 10
nxs: 0, ys: 10
ws1: 57
ws2: 41
ws3: 0
ws4: 0
tt:x_1_1244
ff:false

cond: (x_1188 > m_1125); (not (i_1123 >= x_1028))
pbs: x_3_1244 := ((i_1123 + 1) >= x_1028);
     x_2_1244 := (x__1244 >= x_1028);
     x_1_1244 := (x__1244 >= x_1188);
     x_1188 := (x_1188 >= x_1028);
     m1_1125 := (i_1123 <= 0);
     m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
p:(x__1244 >= x_1028)
fvsp: 2
fvsp: 2
fvsp: 2
fvsp: 2
fvsp: 1
fvsp: 2
xs: 1, qs: 5
nxs: 0, ys: 6
ws1: 21
ws2: 21
ws3: 10
ws4: 10
fvsp: 4
fvsp: 4
fvsp: 4
fvsp: 3
fvsp: 3
fvsp: 3
fvsp: 4
fvsp: 4
fvsp: 4
fvsp: 3
xs: 1, qs: 9
nxs: 0, ys: 10
ws1: 46
ws2: 41
ws3: 7
ws4: 7
fvsp: 5
fvsp: 5
fvsp: 6
fvsp: 6
fvsp: 5
fvsp: 5
fvsp: 5
xs: 0, qs: 7
nxs: 0, ys: 7
ws1: 54
ws2: 41
ws3: 0
ws4: 0
tt:(x_2_1244 || (x_1_1244 && x_1188))
ff:false

cond: (x_1188 > m_1125); (not (i_1123 >= x_1028))
pbs: x_3_1244 := ((i_1123 + 1) >= x_1028);
     x_2_1244 := (x__1244 >= x_1028);
     x_1_1244 := (x__1244 >= x_1188);
     x_1188 := (x_1188 >= x_1028);
     m1_1125 := (i_1123 <= 0);
     m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
p:(i_1123 >= x_1028)
tt:false
ff:true

filter
  cond: (x_1188 > m_1125); (not (i_1123 >= x_1028))
  orig pbs: x_3_1244 := ((i_1123 + 1) >= x_1028);
            x_2_1244 := (x__1244 >= x_1028);
            x_1_1244 := (x__1244 >= x_1188);
            x_1188 := (x_1188 >= x_1028);
            m1_1125 := (i_1123 <= 0);
            m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: x_3_1244 := ((i_1123 + 1) >= x_1028);
       x_2_1244 := (x__1244 >= x_1028);
       x_1_1244 := (x__1244 >= x_1188);
       x_1188 := (x_1188 >= x_1028);
       m1_1125 := (i_1123 <= 0);
       m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
fvsp: 2
fvsp: 2
fvsp: 2
fvsp: 1
fvsp: 2
xs: 0, qs: 6
nxs: 6, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (x_1188 > m_1125); (not (i_1123 >= x_1028))
  orig pbs: x_3_1244 := ((i_1123 + 1) >= x_1028);
            x_2_1244 := (x__1244 >= x_1028);
            x_1_1244 := (x__1244 >= x_1188);
            x_1188 := (x_1188 >= x_1028);
            m1_1125 := (i_1123 <= 0);
            m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: x_3_1244 := ((i_1123 + 1) >= x_1028);
       x_2_1244 := (x__1244 >= x_1028);
       x_1_1244 := (x__1244 >= x_1188);
       x_1188 := (x_1188 >= x_1028);
       m1_1125 := (i_1123 <= 0);
       m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
fvsp: 2
fvsp: 2
fvsp: 2
fvsp: 1
fvsp: 2
xs: 0, qs: 6
nxs: 6, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (x_1188 > m_1125); (not (i_1123 >= x_1028))
  orig pbs: x_1188 := (x_1188 >= x_1028);
            m1_1125 := (i_1123 <= 0);
            m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: x_1188 := (x_1188 >= x_1028);
       m1_1125 := (i_1123 <= 0);
       m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
fvsp: 1
fvsp: 2
xs: 0, qs: 3
nxs: 3, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

abst_arg: x__1241, x_1:int[x_1 <= 0];;
abst_arg: x__1242, (x_1:int[x_1 >= x_1028] ->
X);;
abst_arg: x__1241, x_1:int[x_1 <= 0];;
abst_arg: x__1242, (x_1:int[x_1 >= x_1028] ->
X);;
cond: (not (x_1188 > m_1125)); (not (i_1123 >= x_1028))
pbs: x__1241 := (x__1241 <= 0);
     x_1188 := (x_1188 >= x_1028);
     m1_1125 := (i_1123 <= 0);
     m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
p:(x__1241 <= 0)
fvsp: 1
xs: 1, qs: 0
nxs: 0, ys: 1
ws1: 1
ws2: 1
ws3: 0
ws4: 0
tt:x__1241
ff:false

abst_arg: x__1243, x_1:int[x_1 >= x_1028];;
abst_arg: x__1243, x_1:int[x_1 >= x_1028];;
cond: (not (x_1188 > m_1125)); (not (i_1123 >= x_1028))
pbs: x__1243 := (x__1243 >= x_1028);
     x__1241 := (x__1241 <= 0);
     x_1188 := (x_1188 >= x_1028);
     m1_1125 := (i_1123 <= 0);
     m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
p:(x__1243 >= x_1028)
fvsp: 2
fvsp: 2
fvsp: 1
fvsp: 2
xs: 1, qs: 3
nxs: 0, ys: 4
ws1: 10
ws2: 10
ws3: 3
ws4: 3
fvsp: 3
fvsp: 4
fvsp: 3
xs: 0, qs: 3
nxs: 0, ys: 3
ws1: 14
ws2: 14
ws3: 1
ws4: 1
fvsp: 5
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 15
ws2: 14
ws3: 0
ws4: 0
tt:x__1243
ff:false

filter
  cond: (not (x_1188 > m_1125)); (not (i_1123 >= x_1028))
  orig pbs: x__1243 := (x__1243 >= x_1028);
            x__1241 := (x__1241 <= 0);
            x_1188 := (x_1188 >= x_1028);
            m1_1125 := (i_1123 <= 0);
            m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: x__1243 := (x__1243 >= x_1028);
       x_1188 := (x_1188 >= x_1028);
       m1_1125 := (i_1123 <= 0);
       m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
fvsp: 2
fvsp: 1
fvsp: 2
xs: 0, qs: 4
nxs: 4, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  pbs: x__1241 := (x__1241 <= 0)
fvsp: 1
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 1
ws2: 1
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (not (x_1188 > m_1125)); (not (i_1123 >= x_1028))
  orig pbs: x__1243 := (x__1243 >= x_1028);
            x__1241 := (x__1241 <= 0);
            x_1188 := (x_1188 >= x_1028);
            m1_1125 := (i_1123 <= 0);
            m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: x__1243 := (x__1243 >= x_1028);
       x_1188 := (x_1188 >= x_1028);
       m1_1125 := (i_1123 <= 0);
       m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
fvsp: 2
fvsp: 1
fvsp: 2
xs: 0, qs: 4
nxs: 4, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  pbs: x__1241 := (x__1241 <= 0)
fvsp: 1
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 1
ws2: 1
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (not (x_1188 > m_1125)); (not (i_1123 >= x_1028))
  orig pbs: x__1241 := (x__1241 <= 0);
            x_1188 := (x_1188 >= x_1028);
            m1_1125 := (i_1123 <= 0);
            m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: x_1188 := (x_1188 >= x_1028);
       m1_1125 := (i_1123 <= 0);
       m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
fvsp: 1
fvsp: 2
xs: 0, qs: 3
nxs: 3, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  pbs: x__1241 := (x__1241 <= 0)
fvsp: 1
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 1
ws2: 1
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (not (x_1188 > m_1125)); (not (i_1123 >= x_1028))
  orig pbs: x__1241 := (x__1241 <= 0);
            x_1188 := (x_1188 >= x_1028);
            m1_1125 := (i_1123 <= 0);
            m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: x_1188 := (x_1188 >= x_1028);
       m1_1125 := (i_1123 <= 0);
       m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
fvsp: 1
fvsp: 2
xs: 0, qs: 3
nxs: 3, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  pbs: x__1241 := (x__1241 <= 0)
fvsp: 1
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 1
ws2: 1
ws3: 0
ws4: 0
  unsat:false

cond: (not (x_1188 > m_1125)); (not (i_1123 >= x_1028))
pbs: x_1188 := (x_1188 >= x_1028);
     m1_1125 := (i_1123 <= 0);
     m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
p:((i_1123 + 1) <= 0)
fvsp: 2
fvsp: 1
fvsp: 2
xs: 0, qs: 3
nxs: 0, ys: 3
ws1: 6
ws2: 6
ws3: 3
ws4: 3
fvsp: 3
fvsp: 4
fvsp: 3
xs: 0, qs: 3
nxs: 0, ys: 3
ws1: 7
ws2: 7
ws3: 1
ws4: 1
fvsp: 5
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 7
ws2: 7
ws3: 0
ws4: 0
tt:false
ff:false

cond: (not (x_1188 > m_1125)); (not (i_1123 >= x_1028))
pbs: x_1188 := (x_1188 >= x_1028);
     m1_1125 := (i_1123 <= 0);
     m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
p:(1 <= (x_1028 - (1 * (i_1123 + 1))))
fvsp: 2
fvsp: 1
fvsp: 2
xs: 0, qs: 3
nxs: 0, ys: 3
ws1: 6
ws2: 6
ws3: 3
ws4: 3
fvsp: 3
fvsp: 4
fvsp: 3
xs: 0, qs: 3
nxs: 0, ys: 3
ws1: 7
ws2: 7
ws3: 1
ws4: 1
fvsp: 5
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 7
ws2: 7
ws3: 0
ws4: 0
tt:false
ff:false

abst_arg: x__1240, x_1:int[x_1 >= m_1125; x_1 >= x_1028; i_1123 + 1 >= x_1028];;
abst_arg: x__1240, x_1:int[x_1 >= m_1125; x_1 >= x_1028; i_1123 + 1 >= x_1028];;
cond: (not (x_1188 > m_1125)); (not (i_1123 >= x_1028))
pbs: x_3_1240 := ((i_1123 + 1) >= x_1028);
     x_2_1240 := (x__1240 >= x_1028);
     x_1_1240 := (x__1240 >= m_1125);
     x_1188 := (x_1188 >= x_1028);
     m1_1125 := (i_1123 <= 0);
     m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
p:(x__1240 >= m_1125)
fvsp: 2
fvsp: 2
fvsp: 2
fvsp: 2
fvsp: 1
fvsp: 2
xs: 1, qs: 5
nxs: 0, ys: 6
ws1: 21
ws2: 21
ws3: 10
ws4: 10
fvsp: 4
fvsp: 4
fvsp: 4
fvsp: 3
fvsp: 3
fvsp: 3
fvsp: 4
fvsp: 4
fvsp: 4
fvsp: 3
xs: 0, qs: 10
nxs: 0, ys: 10
ws1: 46
ws2: 41
ws3: 10
ws4: 10
fvsp: 6
fvsp: 5
fvsp: 5
fvsp: 5
fvsp: 6
fvsp: 6
fvsp: 6
fvsp: 5
fvsp: 5
fvsp: 5
xs: 0, qs: 10
nxs: 0, ys: 10
ws1: 57
ws2: 41
ws3: 0
ws4: 0
tt:x_1_1240
ff:false

cond: (not (x_1188 > m_1125)); (not (i_1123 >= x_1028))
pbs: x_3_1240 := ((i_1123 + 1) >= x_1028);
     x_2_1240 := (x__1240 >= x_1028);
     x_1_1240 := (x__1240 >= m_1125);
     x_1188 := (x_1188 >= x_1028);
     m1_1125 := (i_1123 <= 0);
     m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
p:(x__1240 >= x_1028)
fvsp: 2
fvsp: 2
fvsp: 2
fvsp: 2
fvsp: 1
fvsp: 2
xs: 1, qs: 5
nxs: 0, ys: 6
ws1: 21
ws2: 21
ws3: 10
ws4: 10
fvsp: 4
fvsp: 4
fvsp: 4
fvsp: 3
fvsp: 3
fvsp: 3
fvsp: 4
fvsp: 4
fvsp: 4
fvsp: 3
xs: 1, qs: 9
nxs: 0, ys: 10
ws1: 46
ws2: 41
ws3: 7
ws4: 7
fvsp: 5
fvsp: 5
fvsp: 6
fvsp: 6
fvsp: 5
fvsp: 5
fvsp: 5
xs: 0, qs: 7
nxs: 0, ys: 7
ws1: 54
ws2: 41
ws3: 0
ws4: 0
tt:(x_2_1240 || (x_1_1240 && x_1188))
ff:false

cond: (not (x_1188 > m_1125)); (not (i_1123 >= x_1028))
pbs: x_3_1240 := ((i_1123 + 1) >= x_1028);
     x_2_1240 := (x__1240 >= x_1028);
     x_1_1240 := (x__1240 >= m_1125);
     x_1188 := (x_1188 >= x_1028);
     m1_1125 := (i_1123 <= 0);
     m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
p:(i_1123 >= x_1028)
tt:false
ff:true

filter
  cond: (not (x_1188 > m_1125)); (not (i_1123 >= x_1028))
  orig pbs: x_3_1240 := ((i_1123 + 1) >= x_1028);
            x_2_1240 := (x__1240 >= x_1028);
            x_1_1240 := (x__1240 >= m_1125);
            x_1188 := (x_1188 >= x_1028);
            m1_1125 := (i_1123 <= 0);
            m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: x_3_1240 := ((i_1123 + 1) >= x_1028);
       x_2_1240 := (x__1240 >= x_1028);
       x_1_1240 := (x__1240 >= m_1125);
       x_1188 := (x_1188 >= x_1028);
       m1_1125 := (i_1123 <= 0);
       m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
fvsp: 2
fvsp: 2
fvsp: 2
fvsp: 1
fvsp: 2
xs: 0, qs: 6
nxs: 6, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (not (x_1188 > m_1125)); (not (i_1123 >= x_1028))
  orig pbs: x_3_1240 := ((i_1123 + 1) >= x_1028);
            x_2_1240 := (x__1240 >= x_1028);
            x_1_1240 := (x__1240 >= m_1125);
            x_1188 := (x_1188 >= x_1028);
            m1_1125 := (i_1123 <= 0);
            m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: x_3_1240 := ((i_1123 + 1) >= x_1028);
       x_2_1240 := (x__1240 >= x_1028);
       x_1_1240 := (x__1240 >= m_1125);
       x_1188 := (x_1188 >= x_1028);
       m1_1125 := (i_1123 <= 0);
       m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
fvsp: 2
fvsp: 2
fvsp: 2
fvsp: 1
fvsp: 2
xs: 0, qs: 6
nxs: 6, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (not (x_1188 > m_1125)); (not (i_1123 >= x_1028))
  orig pbs: x_1188 := (x_1188 >= x_1028);
            m1_1125 := (i_1123 <= 0);
            m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: x_1188 := (x_1188 >= x_1028);
       m1_1125 := (i_1123 <= 0);
       m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
fvsp: 1
fvsp: 2
xs: 0, qs: 3
nxs: 3, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (not (i_1123 >= x_1028))
  orig pbs: x_1188 := (x_1188 >= x_1028);
            m1_1125 := (i_1123 <= 0);
            m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: x_1188 := (x_1188 >= x_1028);
       m1_1125 := (i_1123 <= 0);
       m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 2
fvsp: 1
fvsp: 2
xs: 0, qs: 3
nxs: 3, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

filter
  cond: (not (i_1123 >= x_1028))
  orig pbs: m1_1125 := (i_1123 <= 0);
            m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: m1_1125 := (i_1123 <= 0);
       m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 1
fvsp: 2
xs: 0, qs: 2
nxs: 2, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

ASSUME:
  cond: 
  pbs: m1_1125 := (i_1123 <= 0);
       m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
  t1: (not (i_1123 >= x_1028))
filter
  cond: (not (i_1123 >= x_1028))
  orig pbs: m1_1125 := (i_1123 <= 0);
            m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
  pbs: m1_1125 := (i_1123 <= 0);
       m2_1125 := (1 <= (x_1028 - (1 * i_1123)))
fvsp: 1
fvsp: 2
xs: 0, qs: 2
nxs: 2, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

fail_1349: ENV: b:x_1:bool[x_1], k:(unit -> X),


abst_arg: b, x_1:bool[x_1];;
abst_arg: k, (unit ->
X);;
abst_arg: b, x_1:bool[x_1];;
abst_arg: k, (unit ->
X);;
fail_1349: (k ()) ===> (k ())
fail_1349:: (k ())
filter
  cond: true
  orig pbs: b := b
  pbs: b := b
fvsp: 1
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 1
ws2: 1
ws3: 0
ws4: 0
  unsat:false

ASSUME:
  cond: 
  pbs: b := b
  t1: true
filter
  cond: true
  orig pbs: b := b
  pbs: b := b
fvsp: 1
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 1
ws2: 1
ws3: 0
ws4: 0
  unsat:false

k_main_1216: ENV: i_1020:int, n_1019:int, k_main_1209:(unit -> X),
b_1322:x_1:bool[i_1020 <= 0 || (not x_1); 1 <= -i_1020 + n_1019 && x_1 || (not x_1); x_1],


abst_arg: i_1020, int;;
abst_arg: n_1019, int;;
abst_arg: k_main_1209, (unit ->
X);;
abst_arg: b_1322, x_1:bool[i_1020 <= 0 || (not x_1); 1 <= -i_1020 + n_1019 && x_1 || (not x_1); x_1];;
abst_arg: i_1020, int;;
abst_arg: n_1019, int;;
abst_arg: k_main_1209, (unit ->
X);;
abst_arg: b_1322, x_1:bool[i_1020 <= 0 || (not x_1); 1 <= -i_1020 + n_1019 && x_1 || (not x_1); x_1];;
k_main_1216: (l0
               (array_max_1011 n_1019 i_1020
                 (fun (i_1223:x_1:int[x_1 <= 0]) (k_make_array_1224:(x_1:int[x_1 >= n_1019] -> X)) ->
                  (k_make_array_1224 (n_1019 - i_1223))) (-1)
                 (fun (m_1205:x_1:int[x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019]) ->
                  (if (n_1019 <= m_1205) (l0 (k_main_1209 ()))
                    (l1 (fail_1349 true (fun (x__1248:unit) -> (k_main_1209 x__1248)))))))) ===> (
l0
 (array_max_1011 n_1019 i_1020
   (fun (i_1223:x_1:int[x_1 <= 0]) (k_make_array_1224:(x_1:int[x_1 >= n_1019] -> X)) ->
    (k_make_array_1224 (n_1019 - i_1223))) (-1)
   (fun (m_1205:x_1:int[x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019]) ->
    (if (n_1019 <= m_1205) (l0 (k_main_1209 ())) (l1 (fail_1349 true (fun (x__1248:unit) -> (k_main_1209 x__1248))))))))
k_main_1216:: (l0
                (array_max_1011 n_1019 i_1020
                  (fun (i_1223:x_1:int[x_1 <= 0]) (k_make_array_1224:(x_1:int[x_1 >= n_1019] -> X)) ->
                   (k_make_array_1224 (n_1019 - i_1223))) (-1)
                  (fun (m_1205:x_1:int[x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019]) ->
                   (if (n_1019 <= m_1205) (l0 (k_main_1209 ()))
                     (l1 (fail_1349 true (fun (x__1248:unit) -> (k_main_1209 x__1248))))))))
abst_arg: i_1223, x_1:int[x_1 <= 0];;
abst_arg: k_make_array_1224, (x_1:int[x_1 >= n_1019] ->
X);;
abst_arg: i_1223, x_1:int[x_1 <= 0];;
abst_arg: k_make_array_1224, (x_1:int[x_1 >= n_1019] ->
X);;
cond: b_1322
pbs: i_1223 := (i_1223 <= 0);
     b1_1322 := ((i_1020 <= 0) || (not b_1322));
     b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
     b3_1322 := b_1322
p:((n_1019 - i_1223) >= n_1019)
fvsp: 1
fvsp: 2
fvsp: 3
fvsp: 1
xs: 1, qs: 3
nxs: 0, ys: 4
ws1: 10
ws2: 10
ws3: 3
ws4: 3
fvsp: 5
fvsp: 3
fvsp: 4
xs: 0, qs: 3
nxs: 0, ys: 3
ws1: 14
ws2: 14
ws3: 1
ws4: 1
fvsp: 6
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 15
ws2: 14
ws3: 0
ws4: 0
tt:i_1223
ff:false

filter
  cond: b_1322
  orig pbs: i_1223 := (i_1223 <= 0);
            b1_1322 := ((i_1020 <= 0) || (not b_1322));
            b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b3_1322 := b_1322
  pbs: b1_1322 := ((i_1020 <= 0) || (not b_1322));
       b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b3_1322 := b_1322
fvsp: 2
fvsp: 3
fvsp: 1
xs: 0, qs: 3
nxs: 3, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  pbs: i_1223 := (i_1223 <= 0)
fvsp: 1
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 1
ws2: 1
ws3: 0
ws4: 0
  unsat:false

filter
  cond: b_1322
  orig pbs: i_1223 := (i_1223 <= 0);
            b1_1322 := ((i_1020 <= 0) || (not b_1322));
            b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b3_1322 := b_1322
  pbs: b1_1322 := ((i_1020 <= 0) || (not b_1322));
       b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b3_1322 := b_1322
fvsp: 2
fvsp: 3
fvsp: 1
xs: 0, qs: 3
nxs: 3, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  pbs: i_1223 := (i_1223 <= 0)
fvsp: 1
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 1
ws2: 1
ws3: 0
ws4: 0
  unsat:false

cond: b_1322
pbs: b1_1322 := ((i_1020 <= 0) || (not b_1322));
     b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
     b3_1322 := b_1322
p:(i_1020 <= 0)
fvsp: 2
fvsp: 3
fvsp: 1
xs: 1, qs: 2
nxs: 0, ys: 3
ws1: 6
ws2: 6
ws3: 1
ws4: 1
fvsp: 4
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 7
ws2: 7
ws3: 0
ws4: 0
tt:b1_1322
ff:false

cond: b_1322
pbs: b1_1322 := ((i_1020 <= 0) || (not b_1322));
     b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
     b3_1322 := b_1322
p:(1 <= (n_1019 - (1 * i_1020)))
fvsp: 2
fvsp: 3
fvsp: 1
xs: 1, qs: 2
nxs: 0, ys: 3
ws1: 6
ws2: 6
ws3: 1
ws4: 1
fvsp: 3
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 7
ws2: 7
ws3: 0
ws4: 0
tt:b2_1322
ff:false

abst_arg: m_1205, x_1:int[x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019];;
abst_arg: m_1205, x_1:int[x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019];;
cond: b_1322
pbs: m3_1205 := (i_1020 >= n_1019);
     m2_1205 := (m_1205 >= n_1019);
     m1_1205 := (m_1205 >= (-1));
     b1_1322 := ((i_1020 <= 0) || (not b_1322));
     b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
     b3_1322 := b_1322
p:(n_1019 <= m_1205)
fvsp: 2
fvsp: 2
fvsp: 1
fvsp: 2
fvsp: 3
fvsp: 1
xs: 1, qs: 5
nxs: 0, ys: 6
ws1: 21
ws2: 21
ws3: 10
ws4: 10
fvsp: 3
fvsp: 4
fvsp: 3
fvsp: 5
fvsp: 4
fvsp: 5
fvsp: 3
fvsp: 2
fvsp: 3
fvsp: 4
xs: 1, qs: 9
nxs: 1, ys: 9
ws1: 45
ws2: 40
ws3: 7
ws4: 7
fvsp: 5
fvsp: 6
fvsp: 4
fvsp: 5
fvsp: 4
fvsp: 5
fvsp: 6
xs: 0, qs: 7
nxs: 0, ys: 7
ws1: 53
ws2: 40
ws3: 0
ws4: 0
tt:m2_1205
ff:false

filter
  cond: (n_1019 <= m_1205); b_1322
  orig pbs: m3_1205 := (i_1020 >= n_1019);
            m2_1205 := (m_1205 >= n_1019);
            m1_1205 := (m_1205 >= (-1));
            b1_1322 := ((i_1020 <= 0) || (not b_1322));
            b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b3_1322 := b_1322
  pbs: m3_1205 := (i_1020 >= n_1019);
       m2_1205 := (m_1205 >= n_1019);
       m1_1205 := (m_1205 >= (-1));
       b1_1322 := ((i_1020 <= 0) || (not b_1322));
       b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b3_1322 := b_1322
fvsp: 2
fvsp: 2
fvsp: 1
fvsp: 2
fvsp: 3
fvsp: 1
xs: 0, qs: 6
nxs: 6, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

cond: (not (n_1019 <= m_1205)); b_1322
pbs: m3_1205 := (i_1020 >= n_1019);
     m1_1205 := (m_1205 >= (-1));
     b1_1322 := ((i_1020 <= 0) || (not b_1322));
     b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
     b3_1322 := b_1322
p:true
tt:true
ff:false

abst_arg: x__1248, unit;;
abst_arg: x__1248, unit;;
filter
  cond: (not (n_1019 <= m_1205)); b_1322
  orig pbs: m3_1205 := (i_1020 >= n_1019);
            m2_1205 := (m_1205 >= n_1019);
            m1_1205 := (m_1205 >= (-1));
            b1_1322 := ((i_1020 <= 0) || (not b_1322));
            b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b3_1322 := b_1322
  pbs: m3_1205 := (i_1020 >= n_1019);
       m2_1205 := (m_1205 >= n_1019);
       m1_1205 := (m_1205 >= (-1));
       b1_1322 := ((i_1020 <= 0) || (not b_1322));
       b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b3_1322 := b_1322
fvsp: 2
fvsp: 2
fvsp: 1
fvsp: 2
fvsp: 3
fvsp: 1
xs: 1, qs: 5
nxs: 6, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:(m2_1205 || m2_1205)

filter
  cond: (not (n_1019 <= m_1205)); b_1322
  orig pbs: m3_1205 := (i_1020 >= n_1019);
            m2_1205 := (m_1205 >= n_1019);
            m1_1205 := (m_1205 >= (-1));
            b1_1322 := ((i_1020 <= 0) || (not b_1322));
            b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b3_1322 := b_1322
  pbs: m3_1205 := (i_1020 >= n_1019);
       m2_1205 := (m_1205 >= n_1019);
       m1_1205 := (m_1205 >= (-1));
       b1_1322 := ((i_1020 <= 0) || (not b_1322));
       b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b3_1322 := b_1322
fvsp: 2
fvsp: 2
fvsp: 1
fvsp: 2
fvsp: 3
fvsp: 1
xs: 1, qs: 5
nxs: 6, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:(m2_1205 || m2_1205)

filter
  cond: (not (n_1019 <= m_1205)); b_1322
  orig pbs: m3_1205 := (i_1020 >= n_1019);
            m2_1205 := (m_1205 >= n_1019);
            m1_1205 := (m_1205 >= (-1));
            b1_1322 := ((i_1020 <= 0) || (not b_1322));
            b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b3_1322 := b_1322
  pbs: m3_1205 := (i_1020 >= n_1019);
       m2_1205 := (m_1205 >= n_1019);
       m1_1205 := (m_1205 >= (-1));
       b1_1322 := ((i_1020 <= 0) || (not b_1322));
       b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b3_1322 := b_1322
fvsp: 2
fvsp: 2
fvsp: 1
fvsp: 2
fvsp: 3
fvsp: 1
xs: 1, qs: 5
nxs: 6, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:(m2_1205 || m2_1205)

filter
  cond: b_1322
  orig pbs: m3_1205 := (i_1020 >= n_1019);
            m2_1205 := (m_1205 >= n_1019);
            m1_1205 := (m_1205 >= (-1));
            b1_1322 := ((i_1020 <= 0) || (not b_1322));
            b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b3_1322 := b_1322
  pbs: m3_1205 := (i_1020 >= n_1019);
       m2_1205 := (m_1205 >= n_1019);
       m1_1205 := (m_1205 >= (-1));
       b1_1322 := ((i_1020 <= 0) || (not b_1322));
       b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b3_1322 := b_1322
fvsp: 2
fvsp: 2
fvsp: 1
fvsp: 2
fvsp: 3
fvsp: 1
xs: 0, qs: 6
nxs: 3, ys: 3
ws1: 6
ws2: 6
ws3: 3
ws4: 3
fvsp: 4
fvsp: 3
fvsp: 3
xs: 0, qs: 3
nxs: 0, ys: 3
ws1: 7
ws2: 7
ws3: 1
ws4: 1
fvsp: 5
xs: 0, qs: 1
nxs: 0, ys: 1
ws1: 7
ws2: 7
ws3: 0
ws4: 0
  unsat:false

filter
  cond: b_1322
  orig pbs: b1_1322 := ((i_1020 <= 0) || (not b_1322));
            b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b3_1322 := b_1322
  pbs: b1_1322 := ((i_1020 <= 0) || (not b_1322));
       b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b3_1322 := b_1322
fvsp: 2
fvsp: 3
fvsp: 1
xs: 0, qs: 3
nxs: 3, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

ASSUME:
  cond: 
  pbs: b1_1322 := ((i_1020 <= 0) || (not b_1322));
       b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b3_1322 := b_1322
  t1: b_1322
filter
  cond: b_1322
  orig pbs: b1_1322 := ((i_1020 <= 0) || (not b_1322));
            b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b3_1322 := b_1322
  pbs: b1_1322 := ((i_1020 <= 0) || (not b_1322));
       b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b3_1322 := b_1322
fvsp: 2
fvsp: 3
fvsp: 1
xs: 0, qs: 3
nxs: 3, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:false

k_main_1216: ENV: i_1020:int, n_1019:int, k_main_1209:(unit -> X),
b_1322:x_1:bool[i_1020 <= 0 || (not x_1); 1 <= -i_1020 + n_1019 && x_1 || (not x_1); x_1],


abst_arg: i_1020, int;;
abst_arg: n_1019, int;;
abst_arg: k_main_1209, (unit ->
X);;
abst_arg: b_1322, x_1:bool[i_1020 <= 0 || (not x_1); 1 <= -i_1020 + n_1019 && x_1 || (not x_1); x_1];;
abst_arg: i_1020, int;;
abst_arg: n_1019, int;;
abst_arg: k_main_1209, (unit ->
X);;
abst_arg: b_1322, x_1:bool[i_1020 <= 0 || (not x_1); 1 <= -i_1020 + n_1019 && x_1 || (not x_1); x_1];;
k_main_1216: (l1 (k_main_1209 ())) ===> (l1 (k_main_1209 ()))
k_main_1216:: (l1 (k_main_1209 ()))
filter
  cond: (not b_1322)
  orig pbs: b1_1322 := ((i_1020 <= 0) || (not b_1322));
            b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b3_1322 := b_1322
  pbs: b1_1322 := ((i_1020 <= 0) || (not b_1322));
       b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b3_1322 := b_1322
fvsp: 2
fvsp: 3
fvsp: 1
xs: 1, qs: 2
nxs: 3, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:(b3_1322 || b3_1322)

ASSUME:
  cond: 
  pbs: b1_1322 := ((i_1020 <= 0) || (not b_1322));
       b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b3_1322 := b_1322
  t1: (not b_1322)
filter
  cond: (not b_1322)
  orig pbs: b1_1322 := ((i_1020 <= 0) || (not b_1322));
            b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
            b3_1322 := b_1322
  pbs: b1_1322 := ((i_1020 <= 0) || (not b_1322));
       b2_1322 := (((1 <= (((-1) * i_1020) + n_1019)) && b_1322) || (not b_1322));
       b3_1322 := b_1322
fvsp: 2
fvsp: 3
fvsp: 1
xs: 1, qs: 2
nxs: 3, ys: 0
ws1: 0
ws2: 0
ws3: 0
ws4: 0
  unsat:(b3_1322 || b3_1322)

ABST:
Main: main_1340
  main_1340 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1216 () true true rand_bool)) (l1 (k_main_1216 () true true false))))
        (l1 (k_main_1216 () true true false)));;
  array_max_1011 x_1124 m1_1125 m2_1125 k_array_max_1153 ->
      (if (m2_1125 || m2_1125) _|_ (l0 (if (m2_1125 || m2_1125) _|_ (k_array_max_1153 true rand_bool true))));;
  array_max_1011 x_1124 m1_1125 m2_1125 k_array_max_1153 ->
      (l1
        (x_1124 (if m1_1125 true rand_bool)
          (fun x_1188 ->
           (if rand_bool
             (l0
               (array_max_1011
                 (fun x__1245 x__1246 ->
                  (x_1124 (if x__1245 true rand_bool) (fun x__1247 -> (x__1246 (if x__1247 true rand_bool)))))
                 rand_bool rand_bool
                 (fun x_1_1244 x_2_1244 x_3_1244 ->
                  (k_array_max_1153 (if x_1_1244 true rand_bool) (if (x_2_1244 || (x_1_1244 && x_1188)) true rand_bool)
                    false))))
             (l1
               (array_max_1011
                 (fun x__1241 x__1242 ->
                  (x_1124 (if x__1241 true rand_bool) (fun x__1243 -> (x__1242 (if x__1243 true rand_bool)))))
                 rand_bool rand_bool
                 (fun x_1_1240 x_2_1240 x_3_1240 ->
                  (k_array_max_1153 (if x_1_1240 true rand_bool) (if (x_2_1240 || (x_1_1240 && x_1188)) true rand_bool)
                    false))))))));;
  fail_1349 b k -> {fail} => k;;
  k_main_1216 k_main_1209 b1_1322 b2_1322 b3_1322 ->
      (l0
        (array_max_1011 (fun i_1223 k_make_array_1224 -> (k_make_array_1224 (if i_1223 true rand_bool)))
          (if b1_1322 true rand_bool) (if b2_1322 true rand_bool)
          (fun m1_1205 m2_1205 m3_1205 ->
           (if (if m2_1205 true rand_bool) (l0 k_main_1209)
             (l1
               (if (m2_1205 || m2_1205) _|_
                 (fail_1349 true (if (m2_1205 || m2_1205) _|_ (if (m2_1205 || m2_1205) _|_ k_main_1209)))))))));;
  k_main_1216 k_main_1209 b1_1322 b2_1322 b3_1322 ->
      (if (b3_1322 || b3_1322) _|_ (l1 (if (b3_1322 || b3_1322) _|_ k_main_1209)));;
Types:
  main_1340 : unit
  array_max_1011 : ((bool -> (bool -> unit) -> unit) -> bool -> bool -> (bool -> bool -> bool -> unit) -> unit)
  array_max_1011 : ((bool -> (bool -> unit) -> unit) -> bool -> bool -> (bool -> bool -> bool -> unit) -> unit)
  fail_1349 : (bool -> unit -> unit)
  k_main_1216 : (unit -> bool -> bool -> bool -> unit)
  k_main_1216 : (unit -> bool -> bool -> bool -> unit)

LIFT:
Main: main_1340
  main_1340 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1216 () true true rand_bool)) (l1 (k_main_1216 () true true false))))
        (l1 (k_main_1216 () true true false)));;
  array_max_1011 x_1124 m1_1125 m2_1125 k_array_max_1153 ->
      (if (m2_1125 || m2_1125) _|_ (l0 (if (m2_1125 || m2_1125) _|_ (k_array_max_1153 true rand_bool true))));;
  array_max_1011 x_1124 m1_1125 m2_1125 k_array_max_1153 ->
      (l1 (x_1124 (if m1_1125 true rand_bool) (f_1252 x_1124 k_array_max_1153)));;
  f_1252 x_1250 k_array_max_1251 x_1188 ->
      (if rand_bool (l0 (array_max_1011 (f_1255 x_1250) rand_bool rand_bool (f_1262 k_array_max_1251 x_1188)))
        (l1 (array_max_1011 (f_1265 x_1250) rand_bool rand_bool (f_1272 k_array_max_1251 x_1188))));;
  f_1262 k_array_max_1260 x_1261 x_1_1244 x_2_1244 x_3_1244 ->
      (k_array_max_1260 (if x_1_1244 true rand_bool) (if (x_2_1244 || (x_1_1244 && x_1261)) true rand_bool) false);;
  f_1255 x_1254 x__1245 x__1246 -> (x_1254 (if x__1245 true rand_bool) (f_1258 x__1246));;
  f_1258 x__1257 x__1247 -> (x__1257 (if x__1247 true rand_bool));;
  f_1268 x__1267 x__1243 -> (x__1267 (if x__1243 true rand_bool));;
  f_1265 x_1264 x__1241 x__1242 -> (x_1264 (if x__1241 true rand_bool) (f_1268 x__1242));;
  f_1272 k_array_max_1270 x_1271 x_1_1240 x_2_1240 x_3_1240 ->
      (k_array_max_1270 (if x_1_1240 true rand_bool) (if (x_2_1240 || (x_1_1240 && x_1271)) true rand_bool) false);;
  fail_1349 b k -> {fail} => k;;
  k_main_1216 k_main_1209 b1_1322 b2_1322 b3_1322 ->
      (l0 (array_max_1011 f_1274 (if b1_1322 true rand_bool) (if b2_1322 true rand_bool) (f_1277 k_main_1209)));;
  f_1274 i_1223 k_make_array_1224 -> (k_make_array_1224 (if i_1223 true rand_bool));;
  f_1277 k_main_1276 m1_1205 m2_1205 m3_1205 ->
      (if (if m2_1205 true rand_bool) (l0 k_main_1276)
        (l1
          (if (m2_1205 || m2_1205) _|_
            (fail_1349 true (if (m2_1205 || m2_1205) _|_ (if (m2_1205 || m2_1205) _|_ k_main_1276))))));;
  k_main_1216 k_main_1209 b1_1322 b2_1322 b3_1322 ->
      (if (b3_1322 || b3_1322) _|_ (l1 (if (b3_1322 || b3_1322) _|_ k_main_1209)));;

TRANS_EAGER:
Main: main_1340
  main_1340 ->
      (let f_1279 b_1278 =
       (if b_1278
         (l0
           (let f_1281 b_1280 =
            (if b_1280
              (l0
                (let f_1283 b_1282 = (k_main_1216 () true true b_1282) in (if rand_bool (f_1283 true) (f_1283 false))))
              (l1 (k_main_1216 () true true false)))
            in (if rand_bool (f_1281 true) (f_1281 false)))) (l1 (k_main_1216 () true true false)))
       in (if rand_bool (f_1279 true) (f_1279 false)));;
  array_max_1011 x_1124 m1_1125 m2_1125 k_array_max_1153 ->
      (let f_1285 b_1284 =
       (if b_1284 _|_
         (l0
           (let f_1287 b_1286 =
            (if b_1286 _|_
              ((let f_1289 b_1288 = (k_array_max_1153 true b_1288) in (if rand_bool (f_1289 true) (f_1289 false))) true))
            in (let f_1291 b_1290 = (if b_1290 (f_1287 true) (f_1287 m2_1125)) in (f_1291 m2_1125)))))
       in (let f_1293 b_1292 = (if b_1292 (f_1285 true) (f_1285 m2_1125)) in (f_1293 m2_1125)));;
  array_max_1011 x_1124 m1_1125 m2_1125 k_array_max_1153 ->
      (l1
        ((let f_1297 b_1296 = (if b_1296 (x_1124 true) (if rand_bool (x_1124 true) (x_1124 false))) in (f_1297 m1_1125))
          (f_1252 x_1124 k_array_max_1153)));;
  f_1252 x_1250 k_array_max_1251 x_1188 ->
      (let f_1299 b_1298 =
       (if b_1298
         (l0
           ((let f_1301 b_1300 =
             ((let f_1303 b_1302 = (array_max_1011 (f_1255 x_1250) b_1302) in
               (if rand_bool (f_1303 true) (f_1303 false))) b_1300)
             in (if rand_bool (f_1301 true) (f_1301 false))) (f_1262 k_array_max_1251 x_1188)))
         (l1
           ((let f_1305 b_1304 =
             ((let f_1307 b_1306 = (array_max_1011 (f_1265 x_1250) b_1306) in
               (if rand_bool (f_1307 true) (f_1307 false))) b_1304)
             in (if rand_bool (f_1305 true) (f_1305 false))) (f_1272 k_array_max_1251 x_1188))))
       in (if rand_bool (f_1299 true) (f_1299 false)));;
  f_1262 k_array_max_1260 x_1261 x_1_1244 x_2_1244 x_3_1244 ->
      ((let f_1309 b_1308 =
        ((let f_1313 b_1312 =
          (if b_1312 (k_array_max_1260 true) (if rand_bool (k_array_max_1260 true) (k_array_max_1260 false))) in
          (f_1313 x_1_1244)) b_1308)
        in
        (let f_1315 b_1314 = (if b_1314 (f_1309 true) (if rand_bool (f_1309 true) (f_1309 false))) in
         (let f_1317 b_1316 =
          (if b_1316 (f_1315 true)
            (let f_1319 b_1318 = (if b_1318 (f_1315 x_1261) (f_1315 false)) in (f_1319 x_1_1244)))
          in (f_1317 x_2_1244)))) false);;
  f_1255 x_1254 x__1245 x__1246 ->
      ((let f_1323 b_1322 = (if b_1322 (x_1254 true) (if rand_bool (x_1254 true) (x_1254 false))) in (f_1323 x__1245))
        (f_1258 x__1246));;
  f_1258 x__1257 x__1247 ->
      (let f_1327 b_1326 = (if b_1326 (x__1257 true) (if rand_bool (x__1257 true) (x__1257 false))) in (f_1327 x__1247));;
  f_1268 x__1267 x__1243 ->
      (let f_1331 b_1330 = (if b_1330 (x__1267 true) (if rand_bool (x__1267 true) (x__1267 false))) in (f_1331 x__1243));;
  f_1265 x_1264 x__1241 x__1242 ->
      ((let f_1335 b_1334 = (if b_1334 (x_1264 true) (if rand_bool (x_1264 true) (x_1264 false))) in (f_1335 x__1241))
        (f_1268 x__1242));;
  f_1272 k_array_max_1270 x_1271 x_1_1240 x_2_1240 x_3_1240 ->
      ((let f_1337 b_1336 =
        ((let f_1341 b_1340 =
          (if b_1340 (k_array_max_1270 true) (if rand_bool (k_array_max_1270 true) (k_array_max_1270 false))) in
          (f_1341 x_1_1240)) b_1336)
        in
        (let f_1343 b_1342 = (if b_1342 (f_1337 true) (if rand_bool (f_1337 true) (f_1337 false))) in
         (let f_1345 b_1344 =
          (if b_1344 (f_1343 true)
            (let f_1347 b_1346 = (if b_1346 (f_1343 x_1271) (f_1343 false)) in (f_1347 x_1_1240)))
          in (f_1345 x_2_1240)))) false);;
  fail_1349 b k -> {fail} => k;;
  k_main_1216 k_main_1209 b1_1322 b2_1322 b3_1322 ->
      (l0
        ((let f_1349 b_1348 =
          ((let f_1351 b_1350 = (array_max_1011 f_1274 b_1350) in
            (let f_1353 b_1352 = (if b_1352 (f_1351 true) (if rand_bool (f_1351 true) (f_1351 false))) in
             (f_1353 b1_1322))) b_1348)
          in
          (let f_1355 b_1354 = (if b_1354 (f_1349 true) (if rand_bool (f_1349 true) (f_1349 false))) in
           (f_1355 b2_1322))) (f_1277 k_main_1209)));;
  f_1274 i_1223 k_make_array_1224 ->
      (let f_1359 b_1358 =
       (if b_1358 (k_make_array_1224 true) (if rand_bool (k_make_array_1224 true) (k_make_array_1224 false))) in
       (f_1359 i_1223));;
  f_1277 k_main_1276 m1_1205 m2_1205 m3_1205 ->
      (let f_1361 b_1360 =
       (if b_1360 (l0 k_main_1276)
         (l1
           (let f_1363 b_1362 =
            (if b_1362 _|_
              (fail_1349 true
                (let f_1365 b_1364 =
                 (if b_1364 _|_
                   (let f_1367 b_1366 = (if b_1366 _|_ k_main_1276) in
                    (let f_1369 b_1368 = (if b_1368 (f_1367 true) (f_1367 m2_1205)) in (f_1369 m2_1205))))
                 in (let f_1371 b_1370 = (if b_1370 (f_1365 true) (f_1365 m2_1205)) in (f_1371 m2_1205)))))
            in (let f_1373 b_1372 = (if b_1372 (f_1363 true) (f_1363 m2_1205)) in (f_1373 m2_1205)))))
       in
       (let f_1375 b_1374 = (if b_1374 (f_1361 true) (if rand_bool (f_1361 true) (f_1361 false))) in (f_1375 m2_1205)));;
  k_main_1216 k_main_1209 b1_1322 b2_1322 b3_1322 ->
      (let f_1377 b_1376 =
       (if b_1376 _|_
         (l1
           (let f_1379 b_1378 = (if b_1378 _|_ k_main_1209) in
            (let f_1381 b_1380 = (if b_1380 (f_1379 true) (f_1379 b3_1322)) in (f_1381 b3_1322)))))
       in (let f_1383 b_1382 = (if b_1382 (f_1377 true) (f_1377 b3_1322)) in (f_1383 b3_1322)));;

PUT_INTO_IF:
Main: main_1340
  main_1340 ->
      (let f_1279 b_1278 =
       (if b_1278
         (l0
           (let f_1281 b_1280 =
            (if b_1280
              (l0
                (let f_1283 b_1282 = (k_main_1216 () true true b_1282) in (if rand_bool (f_1283 true) (f_1283 false))))
              (l1 (k_main_1216 () true true false)))
            in (if rand_bool (f_1281 true) (f_1281 false)))) (l1 (k_main_1216 () true true false)))
       in (if rand_bool (f_1279 true) (f_1279 false)));;
  array_max_1011 x_1124 m1_1125 m2_1125 k_array_max_1153 ->
      (let f_1285 b_1284 =
       (if b_1284 _|_
         (l0
           (let f_1287 b_1286 =
            (if b_1286 _|_
              ((let f_1289 b_1288 = (k_array_max_1153 true b_1288) in (if rand_bool (f_1289 true) (f_1289 false))) true))
            in (let f_1291 b_1290 = (if b_1290 (f_1287 true) (f_1287 m2_1125)) in (f_1291 m2_1125)))))
       in (let f_1293 b_1292 = (if b_1292 (f_1285 true) (f_1285 m2_1125)) in (f_1293 m2_1125)));;
  array_max_1011 x_1124 m1_1125 m2_1125 k_array_max_1153 ->
      (l1
        ((let f_1297 b_1296 = (if b_1296 (x_1124 true) (if rand_bool (x_1124 true) (x_1124 false))) in (f_1297 m1_1125))
          (f_1252 x_1124 k_array_max_1153)));;
  f_1252 x_1250 k_array_max_1251 x_1188 ->
      (let f_1299 b_1298 =
       (if b_1298
         (l0
           ((let f_1301 b_1300 =
             ((let f_1303 b_1302 = (array_max_1011 (f_1255 x_1250) b_1302) in
               (if rand_bool (f_1303 true) (f_1303 false))) b_1300)
             in (if rand_bool (f_1301 true) (f_1301 false))) (f_1262 k_array_max_1251 x_1188)))
         (l1
           ((let f_1305 b_1304 =
             ((let f_1307 b_1306 = (array_max_1011 (f_1265 x_1250) b_1306) in
               (if rand_bool (f_1307 true) (f_1307 false))) b_1304)
             in (if rand_bool (f_1305 true) (f_1305 false))) (f_1272 k_array_max_1251 x_1188))))
       in (if rand_bool (f_1299 true) (f_1299 false)));;
  f_1262 k_array_max_1260 x_1261 x_1_1244 x_2_1244 x_3_1244 ->
      ((let f_1309 b_1308 =
        ((let f_1313 b_1312 =
          (if b_1312 (k_array_max_1260 true) (if rand_bool (k_array_max_1260 true) (k_array_max_1260 false))) in
          (f_1313 x_1_1244)) b_1308)
        in
        (let f_1315 b_1314 = (if b_1314 (f_1309 true) (if rand_bool (f_1309 true) (f_1309 false))) in
         (let f_1317 b_1316 =
          (if b_1316 (f_1315 true)
            (let f_1319 b_1318 = (if b_1318 (f_1315 x_1261) (f_1315 false)) in (f_1319 x_1_1244)))
          in (f_1317 x_2_1244)))) false);;
  f_1255 x_1254 x__1245 x__1246 ->
      ((let f_1323 b_1322 = (if b_1322 (x_1254 true) (if rand_bool (x_1254 true) (x_1254 false))) in (f_1323 x__1245))
        (f_1258 x__1246));;
  f_1258 x__1257 x__1247 ->
      (let f_1327 b_1326 = (if b_1326 (x__1257 true) (if rand_bool (x__1257 true) (x__1257 false))) in (f_1327 x__1247));;
  f_1268 x__1267 x__1243 ->
      (let f_1331 b_1330 = (if b_1330 (x__1267 true) (if rand_bool (x__1267 true) (x__1267 false))) in (f_1331 x__1243));;
  f_1265 x_1264 x__1241 x__1242 ->
      ((let f_1335 b_1334 = (if b_1334 (x_1264 true) (if rand_bool (x_1264 true) (x_1264 false))) in (f_1335 x__1241))
        (f_1268 x__1242));;
  f_1272 k_array_max_1270 x_1271 x_1_1240 x_2_1240 x_3_1240 ->
      ((let f_1337 b_1336 =
        ((let f_1341 b_1340 =
          (if b_1340 (k_array_max_1270 true) (if rand_bool (k_array_max_1270 true) (k_array_max_1270 false))) in
          (f_1341 x_1_1240)) b_1336)
        in
        (let f_1343 b_1342 = (if b_1342 (f_1337 true) (if rand_bool (f_1337 true) (f_1337 false))) in
         (let f_1345 b_1344 =
          (if b_1344 (f_1343 true)
            (let f_1347 b_1346 = (if b_1346 (f_1343 x_1271) (f_1343 false)) in (f_1347 x_1_1240)))
          in (f_1345 x_2_1240)))) false);;
  fail_1349 b k -> {fail} => k;;
  k_main_1216 k_main_1209 b1_1322 b2_1322 b3_1322 ->
      (l0
        ((let f_1349 b_1348 =
          ((let f_1351 b_1350 = (array_max_1011 f_1274 b_1350) in
            (let f_1353 b_1352 = (if b_1352 (f_1351 true) (if rand_bool (f_1351 true) (f_1351 false))) in
             (f_1353 b1_1322))) b_1348)
          in
          (let f_1355 b_1354 = (if b_1354 (f_1349 true) (if rand_bool (f_1349 true) (f_1349 false))) in
           (f_1355 b2_1322))) (f_1277 k_main_1209)));;
  f_1274 i_1223 k_make_array_1224 ->
      (let f_1359 b_1358 =
       (if b_1358 (k_make_array_1224 true) (if rand_bool (k_make_array_1224 true) (k_make_array_1224 false))) in
       (f_1359 i_1223));;
  f_1277 k_main_1276 m1_1205 m2_1205 m3_1205 ->
      (let f_1361 b_1360 =
       (if b_1360 (l0 k_main_1276)
         (l1
           (let f_1363 b_1362 =
            (if b_1362 _|_
              (fail_1349 true
                (let f_1365 b_1364 =
                 (if b_1364 _|_
                   (let f_1367 b_1366 = (if b_1366 _|_ k_main_1276) in
                    (let f_1369 b_1368 = (if b_1368 (f_1367 true) (f_1367 m2_1205)) in (f_1369 m2_1205))))
                 in (let f_1371 b_1370 = (if b_1370 (f_1365 true) (f_1365 m2_1205)) in (f_1371 m2_1205)))))
            in (let f_1373 b_1372 = (if b_1372 (f_1363 true) (f_1363 m2_1205)) in (f_1373 m2_1205)))))
       in
       (let f_1375 b_1374 = (if b_1374 (f_1361 true) (if rand_bool (f_1361 true) (f_1361 false))) in (f_1375 m2_1205)));;
  k_main_1216 k_main_1209 b1_1322 b2_1322 b3_1322 ->
      (let f_1377 b_1376 =
       (if b_1376 _|_
         (l1
           (let f_1379 b_1378 = (if b_1378 _|_ k_main_1209) in
            (let f_1381 b_1380 = (if b_1380 (f_1379 true) (f_1379 b3_1322)) in (f_1381 b3_1322)))))
       in (let f_1383 b_1382 = (if b_1382 (f_1377 true) (f_1377 b3_1322)) in (f_1383 b3_1322)));;

DONE!

(2-2) Checking HORS ... DONE!

Safe!

cycles: 2
total: 1.132 sec
  abst: 0.645 sec
  mc: 0.295 sec
  refine: 0.110 sec
    exparam: 0.056 sec
