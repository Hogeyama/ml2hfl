MoCHi: Model Checker for Higher-Order Programs
  Build: _713bffc (after 2014-07-02 21:25:09 +0900)
  FPAT version: b00026d
  TRecS version: 1.33
  OCaml version: 4.01.0
  Command: ./mochi.opt -ignore-conf a-max.cegar -debug-module CEGAR_abst_util,CEGAR_abst_CPS

Program with abstraction types (CEGAR-cycle 0)::
Main: main_1316
  main_1316 -> (main_1089 f_1322).
  arg1_1085 k_main_arg1_1244 -> (rand_int k_main_arg1_1244).
  arg2_1087 arg1_1274 k_main_arg2_1256 -> (rand_int k_main_arg2_1256).
  array_max_1011 x_1028 i_1013 x_1039 m_1015 k_array_max_1129 when (i_1013 >= x_1028) -> (k_array_max_1129 m_1015).
  array_max_1011 x_1028 i_1013 x_1039 m_1015 k_array_max_1129 when (not (i_1013 >= x_1028)) ->
      (x_1016 i_1013 m_1015 x_1028 x_1039 (f_array_max_1317 i_1013 m_1015 x_1028 k_array_max_1129 x_1039)).
  br_main_1323 b_1324 n_1019 i_1020 k_main_1185 when b_1324 -> (k_main_1192 i_1020 n_1019 k_main_1185 (i_1020 <= 0)).
  br_main_1323 b_1324 n_1019 i_1020 k_main_1185 when (not b_1324) -> (k_main_1192 i_1020 n_1019 k_main_1185 false).
  f_1322 main_1275 -> end.
  f_array_max_1317 i_1013 m_1015 x_1028 k_array_max_1129 x_1039 x_1168 ->
      (z_1017 i_1013 m_1015 x_1028 x_1168 (f_array_max_1318 i_1013 m_1015 x_1028 x_1168 k_array_max_1129 x_1039)).
  f_array_max_1318 i_1013 m_1015 x_1028 x_1168 k_array_max_1129 x_1039 z_1167 ->
      (array_max_1011 x_1028 (i_1013 + 1) x_1039 z_1167 k_array_max_1129).
  f_k_main_1319 b_1298 i_1020 n_1019 k_main_1185 m_1304 when (m_1304 >= n_1019) -> (k_main_1185 ()).
  f_k_main_1319 b_1298 i_1020 n_1019 k_main_1185 m_1304 when (not (m_1304 >= n_1019)) -> (fail_1325 true k_main_1185).
  f_main_1320 k_main_1239 arg1_1274 -> (arg2_1087 arg1_1274 (f_main_1321 arg1_1274 k_main_1239)).
  f_main_1321 arg1_1274 k_main_1239 arg2_1273 -> (main_1018 arg1_1274 arg2_1273 k_main_1239).
  fail_1325 b k -> {fail} => (k ()).
  k_main_1192 i_1020 n_1019 k_main_1185 b_1298 when b_1298 ->
      (m_1300 b_1298 i_1020 n_1019 (f_k_main_1319 b_1298 i_1020 n_1019 k_main_1185)).
  k_main_1192 i_1020 n_1019 k_main_1185 b_1298 when (not b_1298) -> (k_main_1185 ()).
  m_1300 b_1298 i_1020 n_1019 k_main_m_1301 ->
      (array_max_1011 n_1019 i_1020 (make_array_1008 n_1019) -1 k_main_m_1301).
  main_1018 n_1019 i_1020 k_main_1185 when (n_1019 > 0) -> (br_main_1323 (i_1020 >= 0) n_1019 i_1020 k_main_1185).
  main_1018 n_1019 i_1020 k_main_1185 when (not (n_1019 > 0)) -> (k_main_1192 i_1020 n_1019 k_main_1185 false).
  main_1089 k_main_1239 -> (arg1_1085 (f_main_1320 k_main_1239)).
  make_array_1008 n_1009 i_1010 k_make_array_1118 -> (k_make_array_1118 (n_1009 - i_1010)).
  x_1016 i_1013 m_1015 x_1028 x_1039 k_array_max_x_1136 -> (x_1039 i_1013 k_array_max_x_1136).
  z_1017 i_1013 m_1015 x_1028 x_1168 k_array_max_z_1145 when (x_1168 > m_1015) -> (k_array_max_z_1145 x_1168).
  z_1017 i_1013 m_1015 x_1028 x_1168 k_array_max_z_1145 when (not (x_1168 > m_1015)) -> (k_array_max_z_1145 m_1015).
Types:
  main_1316 : X
  array_max_1011 : (x_1:int -> x_2:int -> (int -> (int -> X) -> X) -> int -> (int[x_2 >= x_1] -> X) -> X)
  fail_1325 : (x_1:bool[x_1] -> (unit -> X) -> X)
  k_main_1192 : (x_1:int -> x_2:int -> (unit -> X) -> x_6:bool[(not x_6) || 1 <= 0 - x_1 + x_2 && x_6; x_6] -> X)

(0-1) Abstracting ... EXPAND_NONREC:
Main: main_1316
  main_1316 ->
      (rand_int
        (fun arg1_113 ->
         (rand_int
           (fun arg2_116 ->
            (if (arg1_113 > 0)
              (l0
                (if (arg2_116 >= 0) (l0 (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) (arg2_116 <= 0)))
                  (l1 (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) false))))
              (l1 (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) false))))))).
  array_max_1011 x_1028 i_1013 x_1039 m_1015 k_array_max_1129 when (i_1013 >= x_1028) ->
      (l0 (k_array_max_1129 m_1015)).
  array_max_1011 x_1028 i_1013 x_1039 m_1015 k_array_max_1129 when (not (i_1013 >= x_1028)) ->
      (l1
        (x_1039 i_1013
          (fun x_94 ->
           (if (x_94 > m_1015) (l0 (array_max_1011 x_1028 (i_1013 + 1) x_1039 x_94 k_array_max_1129))
             (l1 (array_max_1011 x_1028 (i_1013 + 1) x_1039 m_1015 k_array_max_1129)))))).
  fail_1325 b k -> {fail} => (k ()).
  k_main_1192 i_1020 n_1019 k_main_1185 b_1298 when b_1298 ->
      (l0
        (array_max_1011 n_1019 i_1020 (fun i_129 k_make_array_130 -> (k_make_array_130 (n_1019 - i_129))) -1
          (fun m_111 -> (if (n_1019 <= m_111) (l0 (k_main_1185 ())) (l1 (fail_1325 true k_main_1185)))))).
  k_main_1192 i_1020 n_1019 k_main_1185 b_1298 when (not b_1298) -> (l1 (k_main_1185 ())).

ETA: (rand_int
       (fun arg1_113 ->
        (rand_int
          (fun arg2_116 ->
           (if (arg1_113 > 0)
             (l0
               (if (arg2_116 >= 0) (l0 (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) (arg2_116 <= 0)))
                 (l1 (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) false))))
             (l1 (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) false))))))): X
ETA: (fun arg1_113 ->
      (rand_int
        (fun arg2_116 ->
         (if (arg1_113 > 0)
           (l0
             (if (arg2_116 >= 0) (l0 (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) (arg2_116 <= 0)))
               (l1 (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) false))))
           (l1 (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) false)))))): (int ->
unit)
ETA: (rand_int
       (fun arg2_116 ->
        (if (arg1_113 > 0)
          (l0
            (if (arg2_116 >= 0) (l0 (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) (arg2_116 <= 0)))
              (l1 (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) false))))
          (l1 (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) false))))): unit
ETA: (fun arg2_116 ->
      (if (arg1_113 > 0)
        (l0
          (if (arg2_116 >= 0) (l0 (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) (arg2_116 <= 0)))
            (l1 (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) false))))
        (l1 (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) false)))): (int ->
unit)
ETA: (if (arg1_113 > 0)
       (l0
         (if (arg2_116 >= 0) (l0 (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) (arg2_116 <= 0)))
           (l1 (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) false))))
       (l1 (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) false))): unit
ETA: (l1 (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) false)): unit
ETA: (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) false): unit
ETA: false: bool[(not arg2_116) || 1 <= 0 - arg2_116 + arg1_113 && arg2_116; arg2_116]
ETA: (fun main_88 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_113: int
ETA: arg2_116: int
ETA_AUX: (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false): unit
ETA: (l0
       (if (arg2_116 >= 0) (l0 (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) (arg2_116 <= 0)))
         (l1 (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) false)))): unit
ETA: (if (arg2_116 >= 0) (l0 (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) (arg2_116 <= 0)))
       (l1 (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) false))): unit
ETA: (l1 (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) false)): unit
ETA: (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) false): unit
ETA: false: bool[(not arg2_116) || 1 <= 0 - arg2_116 + arg1_113 && arg2_116; arg2_116]
ETA: (fun main_88 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_113: int
ETA: arg2_116: int
ETA_AUX: (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false): unit
ETA: (l0 (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) (arg2_116 <= 0))): unit
ETA: (k_main_1192 arg2_116 arg1_113 (fun main_88 -> end) (arg2_116 <= 0)): unit
ETA: (arg2_116 <= 0): bool[(not arg2_116) || 1 <= 0 - arg2_116 + arg1_113 && arg2_116; arg2_116]
ETA: (fun main_88 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_113: int
ETA: arg2_116: int
ETA_AUX: (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) (arg2_116 <= 0)): unit
ETA_AUX: (rand_int
           (fun (arg2_116:int) ->
            (if (arg1_113 > 0)
              (l0
                (if (arg2_116 >= 0) (l0 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) (arg2_116 <= 0)))
                  (l1 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false))))
              (l1 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false))))): unit
ETA_AUX: (rand_int
           (fun (arg1_113:int) ->
            (rand_int
              (fun (arg2_116:int) ->
               (if (arg1_113 > 0)
                 (l0
                   (if (arg2_116 >= 0) (l0 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) (arg2_116 <= 0)))
                     (l1 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false))))
                 (l1 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false))))))): X
ETA: (l0 (k_array_max_1129 m_1015)): X
ETA: (k_array_max_1129 m_1015): X
ETA: m_1015: int[i_1013 >= x_1028]
ETA_AUX: (k_array_max_1129 m_1015): X
ETA: (l1
       (x_1039 i_1013
         (fun x_94 ->
          (if (x_94 > m_1015) (l0 (array_max_1011 x_1028 (i_1013 + 1) x_1039 x_94 k_array_max_1129))
            (l1 (array_max_1011 x_1028 (i_1013 + 1) x_1039 m_1015 k_array_max_1129)))))): X
ETA: (x_1039 i_1013
       (fun x_94 ->
        (if (x_94 > m_1015) (l0 (array_max_1011 x_1028 (i_1013 + 1) x_1039 x_94 k_array_max_1129))
          (l1 (array_max_1011 x_1028 (i_1013 + 1) x_1039 m_1015 k_array_max_1129))))): X
ETA: (fun x_94 ->
      (if (x_94 > m_1015) (l0 (array_max_1011 x_1028 (i_1013 + 1) x_1039 x_94 k_array_max_1129))
        (l1 (array_max_1011 x_1028 (i_1013 + 1) x_1039 m_1015 k_array_max_1129)))): (int ->
X)
ETA: (if (x_94 > m_1015) (l0 (array_max_1011 x_1028 (i_1013 + 1) x_1039 x_94 k_array_max_1129))
       (l1 (array_max_1011 x_1028 (i_1013 + 1) x_1039 m_1015 k_array_max_1129))): X
ETA: (l1 (array_max_1011 x_1028 (i_1013 + 1) x_1039 m_1015 k_array_max_1129)): X
ETA: (array_max_1011 x_1028 (i_1013 + 1) x_1039 m_1015 k_array_max_1129): X
ETA: k_array_max_1129: (int[i_1013 + 1 >= x_1028] ->
X)
ETA_AUX: k_array_max_1129: (int[i_1013 + 1 >= x_1028] ->
X)
ETA_AUX: x__146: int[i_1013 >= x_1028]
ETA_AUX: (k_array_max_1129 x__146): X
ETA: m_1015: int
ETA: x_1039: (int -> (int -> X) -> X)
ETA_AUX: x_1039: (int -> (int -> X) -> X)
ETA_AUX: x__147: int
ETA_AUX: (x_1039 x__147): ((int -> X) ->
X)
ETA_AUX: x__148: (int ->
X)
ETA_AUX: x__149: int
ETA_AUX: (x__148 x__149): X
ETA_AUX: (x_1039 x__147 (fun (x__149:int) -> (x__148 x__149))): X
ETA: (i_1013 + 1): int
ETA: x_1028: int
ETA_AUX: (array_max_1011 x_1028 (i_1013 + 1)
           (fun (x__147:int) (x__148:(int -> X)) -> (x_1039 x__147 (fun (x__149:int) -> (x__148 x__149)))) m_1015
           (fun (x__146:int[i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__146))): X
ETA: (l0 (array_max_1011 x_1028 (i_1013 + 1) x_1039 x_94 k_array_max_1129)): X
ETA: (array_max_1011 x_1028 (i_1013 + 1) x_1039 x_94 k_array_max_1129): X
ETA: k_array_max_1129: (int[i_1013 + 1 >= x_1028] ->
X)
ETA_AUX: k_array_max_1129: (int[i_1013 + 1 >= x_1028] ->
X)
ETA_AUX: x__150: int[i_1013 >= x_1028]
ETA_AUX: (k_array_max_1129 x__150): X
ETA: x_94: int
ETA: x_1039: (int -> (int -> X) -> X)
ETA_AUX: x_1039: (int -> (int -> X) -> X)
ETA_AUX: x__151: int
ETA_AUX: (x_1039 x__151): ((int -> X) ->
X)
ETA_AUX: x__152: (int ->
X)
ETA_AUX: x__153: int
ETA_AUX: (x__152 x__153): X
ETA_AUX: (x_1039 x__151 (fun (x__153:int) -> (x__152 x__153))): X
ETA: (i_1013 + 1): int
ETA: x_1028: int
ETA_AUX: (array_max_1011 x_1028 (i_1013 + 1)
           (fun (x__151:int) (x__152:(int -> X)) -> (x_1039 x__151 (fun (x__153:int) -> (x__152 x__153)))) x_94
           (fun (x__150:int[i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__150))): X
ETA: i_1013: int
ETA_AUX: (x_1039 i_1013
           (fun (x_94:int) ->
            (if (x_94 > m_1015)
              (l0
                (array_max_1011 x_1028 (i_1013 + 1)
                  (fun (x__151:int) (x__152:(int -> X)) -> (x_1039 x__151 (fun (x__153:int) -> (x__152 x__153)))) x_94
                  (fun (x__150:int[i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__150))))
              (l1
                (array_max_1011 x_1028 (i_1013 + 1)
                  (fun (x__147:int) (x__148:(int -> X)) -> (x_1039 x__147 (fun (x__149:int) -> (x__148 x__149))))
                  m_1015 (fun (x__146:int[i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__146))))))): X
ETA: (k ()): X
ETA: (): unit
ETA_AUX: (k ()): X
ETA: (l0
       (array_max_1011 n_1019 i_1020 (fun i_129 k_make_array_130 -> (k_make_array_130 (n_1019 - i_129))) -1
         (fun m_111 -> (if (n_1019 <= m_111) (l0 (k_main_1185 ())) (l1 (fail_1325 true k_main_1185)))))): X
ETA: (array_max_1011 n_1019 i_1020 (fun i_129 k_make_array_130 -> (k_make_array_130 (n_1019 - i_129))) -1
       (fun m_111 -> (if (n_1019 <= m_111) (l0 (k_main_1185 ())) (l1 (fail_1325 true k_main_1185))))): X
ETA: (fun m_111 -> (if (n_1019 <= m_111) (l0 (k_main_1185 ())) (l1 (fail_1325 true k_main_1185)))): (int[
i_1020 >= n_1019] ->
X)
ETA: (if (n_1019 <= m_111) (l0 (k_main_1185 ())) (l1 (fail_1325 true k_main_1185))): X
ETA: (l1 (fail_1325 true k_main_1185)): X
ETA: (fail_1325 true k_main_1185): X
ETA: k_main_1185: (unit ->
X)
ETA_AUX: k_main_1185: (unit ->
X)
ETA_AUX: x__154: unit
ETA_AUX: (k_main_1185 x__154): X
ETA: true: x_1:bool[x_1]
ETA_AUX: (fail_1325 true (fun (x__154:unit) -> (k_main_1185 x__154))): X
ETA: (l0 (k_main_1185 ())): X
ETA: (k_main_1185 ()): X
ETA: (): unit
ETA_AUX: (k_main_1185 ()): X
ETA: -1: int
ETA: (fun i_129 k_make_array_130 -> (k_make_array_130 (n_1019 - i_129))): (
int -> (int -> X) -> X)
ETA: (fun k_make_array_130 -> (k_make_array_130 (n_1019 - i_129))): ((int -> X) ->
X)
ETA: (k_make_array_130 (n_1019 - i_129)): X
ETA: (n_1019 - i_129): int
ETA_AUX: (k_make_array_130 (n_1019 - i_129)): X
ETA: i_1020: int
ETA: n_1019: int
ETA_AUX: (array_max_1011 n_1019 i_1020
           (fun (i_129:int) (k_make_array_130:(int -> X)) -> (k_make_array_130 (n_1019 - i_129))) -1
           (fun (m_111:int[i_1020 >= n_1019]) ->
            (if (n_1019 <= m_111) (l0 (k_main_1185 ()))
              (l1 (fail_1325 true (fun (x__154:unit) -> (k_main_1185 x__154))))))): X
ETA: (l1 (k_main_1185 ())): X
ETA: (k_main_1185 ()): X
ETA: (): unit
ETA_AUX: (k_main_1185 ()): X
ETA_EXPAND:
Main: main_1316
  main_1316 ->
      (rand_int
        (fun (arg1_113:int) ->
         (rand_int
           (fun (arg2_116:int) ->
            (if (arg1_113 > 0)
              (l0
                (if (arg2_116 >= 0) (l0 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) (arg2_116 <= 0)))
                  (l1 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false))))
              (l1 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false))))))).
  array_max_1011 x_1028 i_1013 x_1039 m_1015 k_array_max_1129 when (i_1013 >= x_1028) ->
      (l0 (k_array_max_1129 m_1015)).
  array_max_1011 x_1028 i_1013 x_1039 m_1015 k_array_max_1129 when (not (i_1013 >= x_1028)) ->
      (l1
        (x_1039 i_1013
          (fun (x_94:int) ->
           (if (x_94 > m_1015)
             (l0
               (array_max_1011 x_1028 (i_1013 + 1)
                 (fun (x__151:int) (x__152:(int -> X)) -> (x_1039 x__151 (fun (x__153:int) -> (x__152 x__153)))) x_94
                 (fun (x__150:int[i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__150))))
             (l1
               (array_max_1011 x_1028 (i_1013 + 1)
                 (fun (x__147:int) (x__148:(int -> X)) -> (x_1039 x__147 (fun (x__149:int) -> (x__148 x__149)))) m_1015
                 (fun (x__146:int[i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__146)))))))).
  fail_1325 b k -> {fail} => (k ()).
  k_main_1192 i_1020 n_1019 k_main_1185 b_1298 when b_1298 ->
      (l0
        (array_max_1011 n_1019 i_1020
          (fun (i_129:int) (k_make_array_130:(int -> X)) -> (k_make_array_130 (n_1019 - i_129))) -1
          (fun (m_111:int[i_1020 >= n_1019]) ->
           (if (n_1019 <= m_111) (l0 (k_main_1185 ()))
             (l1 (fail_1325 true (fun (x__154:unit) -> (k_main_1185 x__154)))))))).
  k_main_1192 i_1020 n_1019 k_main_1185 b_1298 when (not b_1298) -> (l1 (k_main_1185 ())).

main_1316: ENV:

main_1316: (rand_int
             (fun (arg1_113:int) ->
              (rand_int
                (fun (arg2_116:int) ->
                 (if (arg1_113 > 0)
                   (l0
                     (if (arg2_116 >= 0)
                       (l0 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) (arg2_116 <= 0)))
                       (l1 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false))))
                   (l1 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false))))))) ===> (
rand_int
 (fun (arg1_113:int) ->
  (rand_int
    (fun (arg2_116:int) ->
     (if (arg1_113 > 0)
       (l0
         (if (arg2_116 >= 0) (l0 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) (arg2_116 <= 0)))
           (l1 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false))))
       (l1 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false)))))))
main_1316:: (rand_int
              (fun (arg1_113:int) ->
               (rand_int
                 (fun (arg2_116:int) ->
                  (if (arg1_113 > 0)
                    (l0
                      (if (arg2_116 >= 0)
                        (l0 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) (arg2_116 <= 0)))
                        (l1 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false))))
                    (l1 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false)))))))
abstract_term: (rand_int
                 (fun (arg1_113:int) ->
                  (rand_int
                    (fun (arg2_116:int) ->
                     (if (arg1_113 > 0)
                       (l0
                         (if (arg2_116 >= 0)
                           (l0 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) (arg2_116 <= 0)))
                           (l1 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false))))
                       (l1 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false))))))): X
abstract_term: (fun (arg1_113:int) ->
                (rand_int
                  (fun (arg2_116:int) ->
                   (if (arg1_113 > 0)
                     (l0
                       (if (arg2_116 >= 0)
                         (l0 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) (arg2_116 <= 0)))
                         (l1 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false))))
                     (l1 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false)))))): (int ->
X)
abst_arg: arg1_113, int
abst_arg: arg1_113, int
abstract_term: (rand_int
                 (fun (arg2_116:int) ->
                  (if (arg1_113 > 0)
                    (l0
                      (if (arg2_116 >= 0)
                        (l0 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) (arg2_116 <= 0)))
                        (l1 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false))))
                    (l1 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false))))): X
abstract_term: (fun (arg2_116:int) ->
                (if (arg1_113 > 0)
                  (l0
                    (if (arg2_116 >= 0)
                      (l0 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) (arg2_116 <= 0)))
                      (l1 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false))))
                  (l1 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false)))): (int ->
X)
abst_arg: arg2_116, int
abst_arg: arg2_116, int
abstract_term: (if (arg1_113 > 0)
                 (l0
                   (if (arg2_116 >= 0) (l0 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) (arg2_116 <= 0)))
                     (l1 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false))))
                 (l1 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false))): X
abstract_term: (arg1_113 > 0): x_1:bool[x_1]
cond: true
pbs:
p:(arg1_113 > 0)
tt:false
ff:false

abstract_term: (l0
                 (if (arg2_116 >= 0) (l0 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) (arg2_116 <= 0)))
                   (l1 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false)))): X
abstract_term: (if (arg2_116 >= 0) (l0 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) (arg2_116 <= 0)))
                 (l1 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false))): X
abstract_term: (arg2_116 >= 0): x_1:bool[x_1]
cond: (arg1_113 > 0); true
pbs:
p:(arg2_116 >= 0)
tt:false
ff:false

abstract_term: (l0 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) (arg2_116 <= 0))): X
abstract_term: (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) (arg2_116 <= 0)): X
abstract_term: (arg2_116 <= 0): bool[(not arg2_116) || 1 <= 0 - arg2_116 + arg1_113 && arg2_116; arg2_116]
cond: (arg2_116 >= 0); (arg1_113 > 0); true
pbs:
p:((not (arg2_116 <= 0)) || ((1 <= ((0 - arg2_116) + arg1_113)) && (arg2_116 <= 0)))
tt:true
ff:false

cond: (arg2_116 >= 0); (arg1_113 > 0); true
pbs:
p:(arg2_116 <= 0)
tt:false
ff:false

abstract_term: (fun (main_88:unit) -> end): (unit ->
X)
abst_arg: main_88, unit
abst_arg: main_88, unit
abstract_term: end: X
abstract_term: arg1_113: int
abstract_term: arg2_116: int
abstract_term: (l1 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false)): X
abstract_term: (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false): X
abstract_term: false: bool[(not arg2_116) || 1 <= 0 - arg2_116 + arg1_113 && arg2_116; arg2_116]
cond: (not (arg2_116 >= 0)); (arg1_113 > 0); true
pbs:
p:((not false) || ((1 <= ((0 - arg2_116) + arg1_113)) && false))
tt:true
ff:false

cond: (not (arg2_116 >= 0)); (arg1_113 > 0); true
pbs:
p:false
tt:false
ff:true

abstract_term: (fun (main_88:unit) -> end): (unit ->
X)
abst_arg: main_88, unit
abst_arg: main_88, unit
abstract_term: end: X
abstract_term: arg1_113: int
abstract_term: arg2_116: int
abstract_term: (l1 (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false)): X
abstract_term: (k_main_1192 arg2_116 arg1_113 (fun (main_88:unit) -> end) false): X
abstract_term: false: bool[(not arg2_116) || 1 <= 0 - arg2_116 + arg1_113 && arg2_116; arg2_116]
cond: (not (arg1_113 > 0)); true
pbs:
p:((not false) || ((1 <= ((0 - arg2_116) + arg1_113)) && false))
tt:true
ff:false

cond: (not (arg1_113 > 0)); true
pbs:
p:false
tt:false
ff:true

abstract_term: (fun (main_88:unit) -> end): (unit ->
X)
abst_arg: main_88, unit
abst_arg: main_88, unit
abstract_term: end: X
abstract_term: arg1_113: int
abstract_term: arg2_116: int
array_max_1011: ENV: x_1028:int, i_1013:int, x_1039:(int -> (int -> X) -> X), m_1015:int,
k_array_max_1129:(int[i_1013 >= x_1028] -> X),


abst_arg: x_1028, int
abst_arg: i_1013, int
abst_arg: x_1039, (int -> (int -> X) -> X)
abst_arg: m_1015, int
abst_arg: k_array_max_1129, (int[i_1013 >= x_1028] ->
X)
abst_arg: x_1028, int
abst_arg: i_1013, int
abst_arg: x_1039, (int -> (int -> X) -> X)
abst_arg: m_1015, int
abst_arg: k_array_max_1129, (int[i_1013 >= x_1028] ->
X)
array_max_1011: (l0 (k_array_max_1129 m_1015)) ===> (l0 (k_array_max_1129 m_1015))
array_max_1011:: (l0 (k_array_max_1129 m_1015))
abstract_term: (l0 (k_array_max_1129 m_1015)): X
abstract_term: (k_array_max_1129 m_1015): X
abstract_term: m_1015: int[i_1013 >= x_1028]
cond: (i_1013 >= x_1028)
pbs:
p:(i_1013 >= x_1028)
tt:true
ff:false

array_max_1011: ENV: x_1028:int, i_1013:int, x_1039:(int -> (int -> X) -> X), m_1015:int,
k_array_max_1129:(int[i_1013 >= x_1028] -> X),


abst_arg: x_1028, int
abst_arg: i_1013, int
abst_arg: x_1039, (int -> (int -> X) -> X)
abst_arg: m_1015, int
abst_arg: k_array_max_1129, (int[i_1013 >= x_1028] ->
X)
abst_arg: x_1028, int
abst_arg: i_1013, int
abst_arg: x_1039, (int -> (int -> X) -> X)
abst_arg: m_1015, int
abst_arg: k_array_max_1129, (int[i_1013 >= x_1028] ->
X)
array_max_1011: (l1
                  (x_1039 i_1013
                    (fun (x_94:int) ->
                     (if (x_94 > m_1015)
                       (l0
                         (array_max_1011 x_1028 (i_1013 + 1)
                           (fun (x__151:int) (x__152:(int -> X)) ->
                            (x_1039 x__151 (fun (x__153:int) -> (x__152 x__153)))) x_94
                           (fun (x__150:int[i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__150))))
                       (l1
                         (array_max_1011 x_1028 (i_1013 + 1)
                           (fun (x__147:int) (x__148:(int -> X)) ->
                            (x_1039 x__147 (fun (x__149:int) -> (x__148 x__149)))) m_1015
                           (fun (x__146:int[i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__146)))))))) ===> (
l1
 (x_1039 i_1013
   (fun (x_94:int) ->
    (if (x_94 > m_1015)
      (l0
        (array_max_1011 x_1028 (i_1013 + 1)
          (fun (x__151:int) (x__152:(int -> X)) -> (x_1039 x__151 (fun (x__153:int) -> (x__152 x__153)))) x_94
          (fun (x__150:int[i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__150))))
      (l1
        (array_max_1011 x_1028 (i_1013 + 1)
          (fun (x__147:int) (x__148:(int -> X)) -> (x_1039 x__147 (fun (x__149:int) -> (x__148 x__149)))) m_1015
          (fun (x__146:int[i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__146))))))))
array_max_1011:: (l1
                   (x_1039 i_1013
                     (fun (x_94:int) ->
                      (if (x_94 > m_1015)
                        (l0
                          (array_max_1011 x_1028 (i_1013 + 1)
                            (fun (x__151:int) (x__152:(int -> X)) ->
                             (x_1039 x__151 (fun (x__153:int) -> (x__152 x__153)))) x_94
                            (fun (x__150:int[i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__150))))
                        (l1
                          (array_max_1011 x_1028 (i_1013 + 1)
                            (fun (x__147:int) (x__148:(int -> X)) ->
                             (x_1039 x__147 (fun (x__149:int) -> (x__148 x__149)))) m_1015
                            (fun (x__146:int[i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__146))))))))
abstract_term: (l1
                 (x_1039 i_1013
                   (fun (x_94:int) ->
                    (if (x_94 > m_1015)
                      (l0
                        (array_max_1011 x_1028 (i_1013 + 1)
                          (fun (x__151:int) (x__152:(int -> X)) ->
                           (x_1039 x__151 (fun (x__153:int) -> (x__152 x__153)))) x_94
                          (fun (x__150:int[i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__150))))
                      (l1
                        (array_max_1011 x_1028 (i_1013 + 1)
                          (fun (x__147:int) (x__148:(int -> X)) ->
                           (x_1039 x__147 (fun (x__149:int) -> (x__148 x__149)))) m_1015
                          (fun (x__146:int[i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__146)))))))): X
abstract_term: (x_1039 i_1013
                 (fun (x_94:int) ->
                  (if (x_94 > m_1015)
                    (l0
                      (array_max_1011 x_1028 (i_1013 + 1)
                        (fun (x__151:int) (x__152:(int -> X)) -> (x_1039 x__151 (fun (x__153:int) -> (x__152 x__153))))
                        x_94 (fun (x__150:int[i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__150))))
                    (l1
                      (array_max_1011 x_1028 (i_1013 + 1)
                        (fun (x__147:int) (x__148:(int -> X)) -> (x_1039 x__147 (fun (x__149:int) -> (x__148 x__149))))
                        m_1015 (fun (x__146:int[i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__146))))))): X
abstract_term: (fun (x_94:int) ->
                (if (x_94 > m_1015)
                  (l0
                    (array_max_1011 x_1028 (i_1013 + 1)
                      (fun (x__151:int) (x__152:(int -> X)) -> (x_1039 x__151 (fun (x__153:int) -> (x__152 x__153))))
                      x_94 (fun (x__150:int[i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__150))))
                  (l1
                    (array_max_1011 x_1028 (i_1013 + 1)
                      (fun (x__147:int) (x__148:(int -> X)) -> (x_1039 x__147 (fun (x__149:int) -> (x__148 x__149))))
                      m_1015 (fun (x__146:int[i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__146)))))): (int ->
X)
abst_arg: x_94, int
abst_arg: x_94, int
abstract_term: (if (x_94 > m_1015)
                 (l0
                   (array_max_1011 x_1028 (i_1013 + 1)
                     (fun (x__151:int) (x__152:(int -> X)) -> (x_1039 x__151 (fun (x__153:int) -> (x__152 x__153))))
                     x_94 (fun (x__150:int[i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__150))))
                 (l1
                   (array_max_1011 x_1028 (i_1013 + 1)
                     (fun (x__147:int) (x__148:(int -> X)) -> (x_1039 x__147 (fun (x__149:int) -> (x__148 x__149))))
                     m_1015 (fun (x__146:int[i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__146))))): X
abstract_term: (x_94 > m_1015): x_1:bool[x_1]
cond: (not (i_1013 >= x_1028))
pbs:
p:(x_94 > m_1015)
tt:false
ff:false

abstract_term: (l0
                 (array_max_1011 x_1028 (i_1013 + 1)
                   (fun (x__151:int) (x__152:(int -> X)) -> (x_1039 x__151 (fun (x__153:int) -> (x__152 x__153)))) x_94
                   (fun (x__150:int[i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__150)))): X
abstract_term: (array_max_1011 x_1028 (i_1013 + 1)
                 (fun (x__151:int) (x__152:(int -> X)) -> (x_1039 x__151 (fun (x__153:int) -> (x__152 x__153)))) x_94
                 (fun (x__150:int[i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__150))): X
abstract_term: (fun (x__150:int[i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__150)): (int[
i_1013 + 1 >= x_1028] ->
X)
abst_arg: x__150, int[i_1013 + 1 >= x_1028]
abst_arg: x__150, int[i_1013 + 1 >= x_1028]
abstract_term: (k_array_max_1129 x__150): X
abstract_term: x__150: int[i_1013 >= x_1028]
cond: (x_94 > m_1015); (not (i_1013 >= x_1028))
pbs: x__150 := ((i_1013 + 1) >= x_1028)
p:(i_1013 >= x_1028)
tt:false
ff:true

abstract_term: x_94: int
abstract_term: (fun (x__151:int) (x__152:(int -> X)) -> (x_1039 x__151 (fun (x__153:int) -> (x__152 x__153)))): (
int -> (int -> X) -> X)
abst_arg: x__151, int
abst_arg: x__152, (int ->
X)
abst_arg: x__151, int
abst_arg: x__152, (int ->
X)
abstract_term: (x_1039 x__151 (fun (x__153:int) -> (x__152 x__153))): X
abstract_term: (fun (x__153:int) -> (x__152 x__153)): (int ->
X)
abst_arg: x__153, int
abst_arg: x__153, int
abstract_term: (x__152 x__153): X
abstract_term: x__153: int
abstract_term: x__151: int
abstract_term: (i_1013 + 1): int
abstract_term: x_1028: int
abstract_term: (l1
                 (array_max_1011 x_1028 (i_1013 + 1)
                   (fun (x__147:int) (x__148:(int -> X)) -> (x_1039 x__147 (fun (x__149:int) -> (x__148 x__149))))
                   m_1015 (fun (x__146:int[i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__146)))): X
abstract_term: (array_max_1011 x_1028 (i_1013 + 1)
                 (fun (x__147:int) (x__148:(int -> X)) -> (x_1039 x__147 (fun (x__149:int) -> (x__148 x__149)))) m_1015
                 (fun (x__146:int[i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__146))): X
abstract_term: (fun (x__146:int[i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__146)): (int[
i_1013 + 1 >= x_1028] ->
X)
abst_arg: x__146, int[i_1013 + 1 >= x_1028]
abst_arg: x__146, int[i_1013 + 1 >= x_1028]
abstract_term: (k_array_max_1129 x__146): X
abstract_term: x__146: int[i_1013 >= x_1028]
cond: (not (x_94 > m_1015)); (not (i_1013 >= x_1028))
pbs: x__146 := ((i_1013 + 1) >= x_1028)
p:(i_1013 >= x_1028)
tt:false
ff:true

abstract_term: m_1015: int
abstract_term: (fun (x__147:int) (x__148:(int -> X)) -> (x_1039 x__147 (fun (x__149:int) -> (x__148 x__149)))): (
int -> (int -> X) -> X)
abst_arg: x__147, int
abst_arg: x__148, (int ->
X)
abst_arg: x__147, int
abst_arg: x__148, (int ->
X)
abstract_term: (x_1039 x__147 (fun (x__149:int) -> (x__148 x__149))): X
abstract_term: (fun (x__149:int) -> (x__148 x__149)): (int ->
X)
abst_arg: x__149, int
abst_arg: x__149, int
abstract_term: (x__148 x__149): X
abstract_term: x__149: int
abstract_term: x__147: int
abstract_term: (i_1013 + 1): int
abstract_term: x_1028: int
abstract_term: i_1013: int
fail_1325: ENV: b:x_1:bool[x_1], k:(unit -> X),


abst_arg: b, x_1:bool[x_1]
abst_arg: k, (unit ->
X)
abst_arg: b, x_1:bool[x_1]
abst_arg: k, (unit ->
X)
fail_1325: (k ()) ===> (k ())
fail_1325:: (k ())
abstract_term: (k ()): X
abstract_term: (): unit
k_main_1192: ENV: i_1020:int, n_1019:int, k_main_1185:(unit -> X),
b_1298:bool[(not i_1020) || 1 <= 0 - i_1020 + n_1019 && i_1020; i_1020],


abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1185, (unit ->
X)
abst_arg: b_1298, bool[(not i_1020) || 1 <= 0 - i_1020 + n_1019 && i_1020; i_1020]
abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1185, (unit ->
X)
abst_arg: b_1298, bool[(not i_1020) || 1 <= 0 - i_1020 + n_1019 && i_1020; i_1020]
k_main_1192: (l0
               (array_max_1011 n_1019 i_1020
                 (fun (i_129:int) (k_make_array_130:(int -> X)) -> (k_make_array_130 (n_1019 - i_129))) -1
                 (fun (m_111:int[i_1020 >= n_1019]) ->
                  (if (n_1019 <= m_111) (l0 (k_main_1185 ()))
                    (l1 (fail_1325 true (fun (x__154:unit) -> (k_main_1185 x__154)))))))) ===> (
l0
 (array_max_1011 n_1019 i_1020 (fun (i_129:int) (k_make_array_130:(int -> X)) -> (k_make_array_130 (n_1019 - i_129)))
   -1
   (fun (m_111:int[i_1020 >= n_1019]) ->
    (if (n_1019 <= m_111) (l0 (k_main_1185 ())) (l1 (fail_1325 true (fun (x__154:unit) -> (k_main_1185 x__154))))))))
k_main_1192:: (l0
                (array_max_1011 n_1019 i_1020
                  (fun (i_129:int) (k_make_array_130:(int -> X)) -> (k_make_array_130 (n_1019 - i_129))) -1
                  (fun (m_111:int[i_1020 >= n_1019]) ->
                   (if (n_1019 <= m_111) (l0 (k_main_1185 ()))
                     (l1 (fail_1325 true (fun (x__154:unit) -> (k_main_1185 x__154))))))))
abstract_term: (l0
                 (array_max_1011 n_1019 i_1020
                   (fun (i_129:int) (k_make_array_130:(int -> X)) -> (k_make_array_130 (n_1019 - i_129))) -1
                   (fun (m_111:int[i_1020 >= n_1019]) ->
                    (if (n_1019 <= m_111) (l0 (k_main_1185 ()))
                      (l1 (fail_1325 true (fun (x__154:unit) -> (k_main_1185 x__154)))))))): X
abstract_term: (array_max_1011 n_1019 i_1020
                 (fun (i_129:int) (k_make_array_130:(int -> X)) -> (k_make_array_130 (n_1019 - i_129))) -1
                 (fun (m_111:int[i_1020 >= n_1019]) ->
                  (if (n_1019 <= m_111) (l0 (k_main_1185 ()))
                    (l1 (fail_1325 true (fun (x__154:unit) -> (k_main_1185 x__154))))))): X
abstract_term: (fun (m_111:int[i_1020 >= n_1019]) ->
                (if (n_1019 <= m_111) (l0 (k_main_1185 ()))
                  (l1 (fail_1325 true (fun (x__154:unit) -> (k_main_1185 x__154)))))): (int[
i_1020 >= n_1019] ->
X)
abst_arg: m_111, int[i_1020 >= n_1019]
abst_arg: m_111, int[i_1020 >= n_1019]
abstract_term: (if (n_1019 <= m_111) (l0 (k_main_1185 ()))
                 (l1 (fail_1325 true (fun (x__154:unit) -> (k_main_1185 x__154))))): X
abstract_term: (n_1019 <= m_111): x_1:bool[x_1]
cond: b_1298
pbs: m_111 := (i_1020 >= n_1019);
     b1_1298 := ((not b_1298) || ((1 <= ((0 - i_1020) + n_1019)) && b_1298));
     b2_1298 := b_1298
p:(n_1019 <= m_111)
tt:false
ff:false

abstract_term: (l0 (k_main_1185 ())): X
abstract_term: (k_main_1185 ()): X
abstract_term: (): unit
abstract_term: (l1 (fail_1325 true (fun (x__154:unit) -> (k_main_1185 x__154)))): X
abstract_term: (fail_1325 true (fun (x__154:unit) -> (k_main_1185 x__154))): X
abstract_term: (fun (x__154:unit) -> (k_main_1185 x__154)): (unit ->
X)
abst_arg: x__154, unit
abst_arg: x__154, unit
abstract_term: (k_main_1185 x__154): X
abstract_term: x__154: unit
abstract_term: true: x_1:bool[x_1]
cond: (not (n_1019 <= m_111)); b_1298
pbs: m_111 := (i_1020 >= n_1019);
     b1_1298 := ((not b_1298) || ((1 <= ((0 - i_1020) + n_1019)) && b_1298));
     b2_1298 := b_1298
p:true
tt:true
ff:false

abstract_term: -1: int
abstract_term: (fun (i_129:int) (k_make_array_130:(int -> X)) -> (k_make_array_130 (n_1019 - i_129))): (
int -> (int -> X) -> X)
abst_arg: i_129, int
abst_arg: k_make_array_130, (int ->
X)
abst_arg: i_129, int
abst_arg: k_make_array_130, (int ->
X)
abstract_term: (k_make_array_130 (n_1019 - i_129)): X
abstract_term: (n_1019 - i_129): int
abstract_term: i_1020: int
abstract_term: n_1019: int
k_main_1192: ENV: i_1020:int, n_1019:int, k_main_1185:(unit -> X),
b_1298:bool[(not i_1020) || 1 <= 0 - i_1020 + n_1019 && i_1020; i_1020],


abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1185, (unit ->
X)
abst_arg: b_1298, bool[(not i_1020) || 1 <= 0 - i_1020 + n_1019 && i_1020; i_1020]
abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1185, (unit ->
X)
abst_arg: b_1298, bool[(not i_1020) || 1 <= 0 - i_1020 + n_1019 && i_1020; i_1020]
k_main_1192: (l1 (k_main_1185 ())) ===> (l1 (k_main_1185 ()))
k_main_1192:: (l1 (k_main_1185 ()))
abstract_term: (l1 (k_main_1185 ())): X
abstract_term: (k_main_1185 ()): X2
abstract_term: (): unit
ABST:
Main: main
  main ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1192 () true rand_bool)) (l1 (k_main_1192 () true false))))
        (l1 (k_main_1192 () true false))).
  array_max_1011 x_1039 k_array_max_1129 -> (l0 (k_array_max_1129 true)).
  array_max_1011 x_1039 k_array_max_1129 ->
      (l1
        (x_1039
          (if rand_bool (l0 (array_max_1011 x_1039 (fun x__150 -> (k_array_max_1129 false))))
            (l1 (array_max_1011 x_1039 (fun x__146 -> (k_array_max_1129 false))))))).
  fail_1325 b k -> {fail} => k.
  k_main_1192 k_main_1185 b1_1298 b2_1298 ->
      (l0
        (array_max_1011 (fun k_make_array_130 -> k_make_array_130)
          (fun m_111 -> (if rand_bool (l0 k_main_1185) (l1 (fail_1325 true k_main_1185)))))).
  k_main_1192 k_main_1185 b1_1298 b2_1298 -> (if b2_1298 _|_ (l1 k_main_1185)).
Types:
  arg1_1085 : (unit -> unit)
  arg2_1087 : (unit -> unit)
  br_main_1323 : (unit -> unit)
  br_main_1323 : (unit -> unit)
  f_1322 : unit
  f_array_max_1317 : (unit -> (unit -> unit) -> unit)
  f_array_max_1318 : (unit -> (unit -> unit) -> unit)
  f_k_main_1319 : (unit -> unit)
  f_k_main_1319 : (unit -> unit)
  f_main_1320 : (unit -> unit)
  f_main_1321 : (unit -> unit)
  m_1300 : (unit -> unit)
  main_1018 : (unit -> unit)
  main_1018 : (unit -> unit)
  main_1089 : (unit -> unit)
  make_array_1008 : (unit -> unit)
  x_1016 : ((unit -> unit) -> unit -> unit)
  z_1017 : (unit -> unit)
  z_1017 : (unit -> unit)

LIFT:
Main: main_1316
  main_1316 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1192 () true rand_bool)) (l1 (k_main_1192 () true false))))
        (l1 (k_main_1192 () true false))).
  array_max_1011 x_1039 k_array_max_1129 -> (l0 (k_array_max_1129 true)).
  array_max_1011 x_1039 k_array_max_1129 ->
      (l1
        (x_1039
          (if rand_bool (l0 (array_max_1011 x_1039 (f_157 k_array_max_1129)))
            (l1 (array_max_1011 x_1039 (f_160 k_array_max_1129)))))).
  f_157 k_array_max_156 x__150 -> (k_array_max_156 false).
  f_160 k_array_max_159 x__146 -> (k_array_max_159 false).
  fail_1325 b k -> {fail} => k.
  k_main_1192 k_main_1185 b1_1298 b2_1298 -> (l0 (array_max_1011 f_162 (f_165 k_main_1185))).
  f_162 k_make_array_130 -> k_make_array_130.
  f_165 k_main_164 m_111 -> (if rand_bool (l0 k_main_164) (l1 (fail_1325 true k_main_164))).
  k_main_1192 k_main_1185 b1_1298 b2_1298 -> (if b2_1298 _|_ (l1 k_main_1185)).

TRANS_EAGER:
Main: main_1316
  main_1316 ->
      (let f_167 b_166 =
       (if b_166
         (l0
           (let f_169 b_168 =
            (if b_168 (l0 (let f_171 b_170 = (k_main_1192 () true b_170) in (if rand_bool (f_171 true) (f_171 false))))
              (l1 (k_main_1192 () true false)))
            in (if rand_bool (f_169 true) (f_169 false)))) (l1 (k_main_1192 () true false)))
       in (if rand_bool (f_167 true) (f_167 false))).
  array_max_1011 x_1039 k_array_max_1129 -> (l0 (k_array_max_1129 true)).
  array_max_1011 x_1039 k_array_max_1129 ->
      (l1
        (x_1039
          (let f_173 b_172 =
           (if b_172 (l0 (array_max_1011 x_1039 (f_157 k_array_max_1129)))
             (l1 (array_max_1011 x_1039 (f_160 k_array_max_1129))))
           in (if rand_bool (f_173 true) (f_173 false))))).
  f_157 k_array_max_156 x__150 -> (k_array_max_156 false).
  f_160 k_array_max_159 x__146 -> (k_array_max_159 false).
  fail_1325 b k -> {fail} => k.
  k_main_1192 k_main_1185 b1_1298 b2_1298 -> (l0 (array_max_1011 f_162 (f_165 k_main_1185))).
  f_162 k_make_array_130 -> k_make_array_130.
  f_165 k_main_164 m_111 ->
      (let f_175 b_174 = (if b_174 (l0 k_main_164) (l1 (fail_1325 true k_main_164))) in
       (if rand_bool (f_175 true) (f_175 false))).
  k_main_1192 k_main_1185 b1_1298 b2_1298 -> (let f_177 b_176 = (if b_176 _|_ (l1 k_main_1185)) in (f_177 b2_1298)).

PUT_INTO_IF:
Main: main_1316
  main_1316 ->
      (let f_167 b_166 =
       (if b_166
         (l0
           (let f_169 b_168 =
            (if b_168 (l0 (let f_171 b_170 = (k_main_1192 () true b_170) in (if rand_bool (f_171 true) (f_171 false))))
              (l1 (k_main_1192 () true false)))
            in (if rand_bool (f_169 true) (f_169 false)))) (l1 (k_main_1192 () true false)))
       in (if rand_bool (f_167 true) (f_167 false))).
  array_max_1011 x_1039 k_array_max_1129 -> (l0 (k_array_max_1129 true)).
  array_max_1011 x_1039 k_array_max_1129 ->
      (l1
        (x_1039
          (let f_173 b_172 =
           (if b_172 (l0 (array_max_1011 x_1039 (f_157 k_array_max_1129)))
             (l1 (array_max_1011 x_1039 (f_160 k_array_max_1129))))
           in (if rand_bool (f_173 true) (f_173 false))))).
  f_157 k_array_max_156 x__150 -> (k_array_max_156 false).
  f_160 k_array_max_159 x__146 -> (k_array_max_159 false).
  fail_1325 b k -> {fail} => k.
  k_main_1192 k_main_1185 b1_1298 b2_1298 -> (l0 (array_max_1011 f_162 (f_165 k_main_1185))).
  f_162 k_make_array_130 -> k_make_array_130.
  f_165 k_main_164 m_111 ->
      (let f_175 b_174 = (if b_174 (l0 k_main_164) (l1 (fail_1325 true k_main_164))) in
       (if rand_bool (f_175 true) (f_175 false))).
  k_main_1192 k_main_1185 b1_1298 b2_1298 -> (let f_177 b_176 = (if b_176 _|_ (l1 k_main_1185)) in (f_177 b2_1298)).

DONE!

(0-2) Checking HORS ... DONE!

Error trace::
  main_1316 ... -->
  main_1089 ... -->
  arg1_1085 ... -->
  f_main_1320 ... -->
  arg2_1087 ... -->
  f_main_1321 ... -->
  main_1018 [1/2] ... -->
  br_main_1323 [1/2] ... -->
  k_main_1192 [1/2] ... -->
  m_1300 ... -->
  array_max_1011 [2/2] ... -->
  x_1016 ... -->
  make_array_1008 ... -->
  f_array_max_1317 ... -->
  z_1017 [1/2] ... -->
  f_array_max_1318 ... -->
  array_max_1011 [1/2] ... -->
  f_k_main_1319 [2/2] ... -->
  fail_1325 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 1; 0

(0-3) Checking counterexample ... DONE!

(0-4) Discovering predicates ...
DONE!

Prefix of spurious counterexample::
0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 1

Program with abstraction types (CEGAR-cycle 1)::
Main: main_1316
  main_1316 -> (main_1089 f_1322).
  arg1_1085 k_main_arg1_1244 -> (rand_int k_main_arg1_1244).
  arg2_1087 arg1_1274 k_main_arg2_1256 -> (rand_int k_main_arg2_1256).
  array_max_1011 x_1028 i_1013 x_1039 m_1015 k_array_max_1129 when (i_1013 >= x_1028) -> (k_array_max_1129 m_1015).
  array_max_1011 x_1028 i_1013 x_1039 m_1015 k_array_max_1129 when (not (i_1013 >= x_1028)) ->
      (x_1016 i_1013 m_1015 x_1028 x_1039 (f_array_max_1317 i_1013 m_1015 x_1028 k_array_max_1129 x_1039)).
  br_main_1323 b_1324 n_1019 i_1020 k_main_1185 when b_1324 -> (k_main_1192 i_1020 n_1019 k_main_1185 (i_1020 <= 0)).
  br_main_1323 b_1324 n_1019 i_1020 k_main_1185 when (not b_1324) -> (k_main_1192 i_1020 n_1019 k_main_1185 false).
  f_1322 main_1275 -> end.
  f_array_max_1317 i_1013 m_1015 x_1028 k_array_max_1129 x_1039 x_1168 ->
      (z_1017 i_1013 m_1015 x_1028 x_1168 (f_array_max_1318 i_1013 m_1015 x_1028 x_1168 k_array_max_1129 x_1039)).
  f_array_max_1318 i_1013 m_1015 x_1028 x_1168 k_array_max_1129 x_1039 z_1167 ->
      (array_max_1011 x_1028 (i_1013 + 1) x_1039 z_1167 k_array_max_1129).
  f_k_main_1319 b_1298 i_1020 n_1019 k_main_1185 m_1304 when (m_1304 >= n_1019) -> (k_main_1185 ()).
  f_k_main_1319 b_1298 i_1020 n_1019 k_main_1185 m_1304 when (not (m_1304 >= n_1019)) -> (fail_1325 true k_main_1185).
  f_main_1320 k_main_1239 arg1_1274 -> (arg2_1087 arg1_1274 (f_main_1321 arg1_1274 k_main_1239)).
  f_main_1321 arg1_1274 k_main_1239 arg2_1273 -> (main_1018 arg1_1274 arg2_1273 k_main_1239).
  fail_1325 b k -> {fail} => (k ()).
  k_main_1192 i_1020 n_1019 k_main_1185 b_1298 when b_1298 ->
      (m_1300 b_1298 i_1020 n_1019 (f_k_main_1319 b_1298 i_1020 n_1019 k_main_1185)).
  k_main_1192 i_1020 n_1019 k_main_1185 b_1298 when (not b_1298) -> (k_main_1185 ()).
  m_1300 b_1298 i_1020 n_1019 k_main_m_1301 ->
      (array_max_1011 n_1019 i_1020 (make_array_1008 n_1019) -1 k_main_m_1301).
  main_1018 n_1019 i_1020 k_main_1185 when (n_1019 > 0) -> (br_main_1323 (i_1020 >= 0) n_1019 i_1020 k_main_1185).
  main_1018 n_1019 i_1020 k_main_1185 when (not (n_1019 > 0)) -> (k_main_1192 i_1020 n_1019 k_main_1185 false).
  main_1089 k_main_1239 -> (arg1_1085 (f_main_1320 k_main_1239)).
  make_array_1008 n_1009 i_1010 k_make_array_1118 -> (k_make_array_1118 (n_1009 - i_1010)).
  x_1016 i_1013 m_1015 x_1028 x_1039 k_array_max_x_1136 -> (x_1039 i_1013 k_array_max_x_1136).
  z_1017 i_1013 m_1015 x_1028 x_1168 k_array_max_z_1145 when (x_1168 > m_1015) -> (k_array_max_z_1145 x_1168).
  z_1017 i_1013 m_1015 x_1028 x_1168 k_array_max_z_1145 when (not (x_1168 > m_1015)) -> (k_array_max_z_1145 m_1015).
Types:
  main_1316 : X
  array_max_1011 : (x_1:int ->
                    x_2:int ->
                    (x_4:int[x_4 <= 0] -> (x_6:int[x_6 >= x_1] -> X) -> X) ->
                    x_9:int[x_2 <= 0] -> (x_11:int[x_11 >= x_9; x_11 >= x_1; x_2 >= x_1] -> X) -> X)
  fail_1325 : (x_1:bool[x_1] -> (unit -> X) -> X)
  k_main_1192 : (x_1:int ->
                 x_2:int ->
                 (unit -> X) -> x_6:bool[(not x_6) || x_1 <= 0; (not x_6) || 1 <= 0 - x_1 + x_2 && x_6; x_6] -> X)

(1-1) Abstracting ... EXPAND_NONREC:
Main: main_1316
  main_1316 ->
      (rand_int
        (fun arg1_621 ->
         (rand_int
           (fun arg2_624 ->
            (if (arg1_621 > 0)
              (l0
                (if (arg2_624 >= 0) (l0 (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) (arg2_624 <= 0)))
                  (l1 (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) false))))
              (l1 (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) false))))))).
  array_max_1011 x_1028 i_1013 x_1039 m_1015 k_array_max_1129 when (i_1013 >= x_1028) ->
      (l0 (k_array_max_1129 m_1015)).
  array_max_1011 x_1028 i_1013 x_1039 m_1015 k_array_max_1129 when (not (i_1013 >= x_1028)) ->
      (l1
        (x_1039 i_1013
          (fun x_602 ->
           (if (x_602 > m_1015) (l0 (array_max_1011 x_1028 (i_1013 + 1) x_1039 x_602 k_array_max_1129))
             (l1 (array_max_1011 x_1028 (i_1013 + 1) x_1039 m_1015 k_array_max_1129)))))).
  fail_1325 b k -> {fail} => (k ()).
  k_main_1192 i_1020 n_1019 k_main_1185 b_1298 when b_1298 ->
      (l0
        (array_max_1011 n_1019 i_1020 (fun i_637 k_make_array_638 -> (k_make_array_638 (n_1019 - i_637))) -1
          (fun m_619 -> (if (n_1019 <= m_619) (l0 (k_main_1185 ())) (l1 (fail_1325 true k_main_1185)))))).
  k_main_1192 i_1020 n_1019 k_main_1185 b_1298 when (not b_1298) -> (l1 (k_main_1185 ())).

ETA: (rand_int
       (fun arg1_621 ->
        (rand_int
          (fun arg2_624 ->
           (if (arg1_621 > 0)
             (l0
               (if (arg2_624 >= 0) (l0 (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) (arg2_624 <= 0)))
                 (l1 (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) false))))
             (l1 (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) false))))))): X
ETA: (fun arg1_621 ->
      (rand_int
        (fun arg2_624 ->
         (if (arg1_621 > 0)
           (l0
             (if (arg2_624 >= 0) (l0 (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) (arg2_624 <= 0)))
               (l1 (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) false))))
           (l1 (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) false)))))): (int ->
unit)
ETA: (rand_int
       (fun arg2_624 ->
        (if (arg1_621 > 0)
          (l0
            (if (arg2_624 >= 0) (l0 (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) (arg2_624 <= 0)))
              (l1 (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) false))))
          (l1 (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) false))))): unit
ETA: (fun arg2_624 ->
      (if (arg1_621 > 0)
        (l0
          (if (arg2_624 >= 0) (l0 (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) (arg2_624 <= 0)))
            (l1 (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) false))))
        (l1 (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) false)))): (int ->
unit)
ETA: (if (arg1_621 > 0)
       (l0
         (if (arg2_624 >= 0) (l0 (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) (arg2_624 <= 0)))
           (l1 (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) false))))
       (l1 (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) false))): unit
ETA: (l1 (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) false)): unit
ETA: (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) false): unit
ETA: false: x_1:bool[(not x_1) || arg2_624 <= 0; (not x_1) || 1 <= 0 - arg2_624 + arg1_621 && x_1; x_1]
ETA: (fun main_596 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_621: int
ETA: arg2_624: int
ETA_AUX: (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false): unit
ETA: (l0
       (if (arg2_624 >= 0) (l0 (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) (arg2_624 <= 0)))
         (l1 (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) false)))): unit
ETA: (if (arg2_624 >= 0) (l0 (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) (arg2_624 <= 0)))
       (l1 (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) false))): unit
ETA: (l1 (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) false)): unit
ETA: (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) false): unit
ETA: false: x_1:bool[(not x_1) || arg2_624 <= 0; (not x_1) || 1 <= 0 - arg2_624 + arg1_621 && x_1; x_1]
ETA: (fun main_596 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_621: int
ETA: arg2_624: int
ETA_AUX: (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false): unit
ETA: (l0 (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) (arg2_624 <= 0))): unit
ETA: (k_main_1192 arg2_624 arg1_621 (fun main_596 -> end) (arg2_624 <= 0)): unit
ETA: (arg2_624 <= 0): x_1:bool[(not x_1) || arg2_624 <= 0; (not x_1) || 1 <= 0 - arg2_624 + arg1_621 && x_1; x_1]
ETA: (fun main_596 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_621: int
ETA: arg2_624: int
ETA_AUX: (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) (arg2_624 <= 0)): unit
ETA_AUX: (rand_int
           (fun (arg2_624:int) ->
            (if (arg1_621 > 0)
              (l0
                (if (arg2_624 >= 0) (l0 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) (arg2_624 <= 0)))
                  (l1 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false))))
              (l1 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false))))): unit
ETA_AUX: (rand_int
           (fun (arg1_621:int) ->
            (rand_int
              (fun (arg2_624:int) ->
               (if (arg1_621 > 0)
                 (l0
                   (if (arg2_624 >= 0)
                     (l0 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) (arg2_624 <= 0)))
                     (l1 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false))))
                 (l1 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false))))))): X
ETA: (l0 (k_array_max_1129 m_1015)): X
ETA: (k_array_max_1129 m_1015): X
ETA: m_1015: x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 >= x_1028]
ETA_AUX: (k_array_max_1129 m_1015): X
ETA: (l1
       (x_1039 i_1013
         (fun x_602 ->
          (if (x_602 > m_1015) (l0 (array_max_1011 x_1028 (i_1013 + 1) x_1039 x_602 k_array_max_1129))
            (l1 (array_max_1011 x_1028 (i_1013 + 1) x_1039 m_1015 k_array_max_1129)))))): X
ETA: (x_1039 i_1013
       (fun x_602 ->
        (if (x_602 > m_1015) (l0 (array_max_1011 x_1028 (i_1013 + 1) x_1039 x_602 k_array_max_1129))
          (l1 (array_max_1011 x_1028 (i_1013 + 1) x_1039 m_1015 k_array_max_1129))))): X
ETA: (fun x_602 ->
      (if (x_602 > m_1015) (l0 (array_max_1011 x_1028 (i_1013 + 1) x_1039 x_602 k_array_max_1129))
        (l1 (array_max_1011 x_1028 (i_1013 + 1) x_1039 m_1015 k_array_max_1129)))): (x_1:int[
x_1 >= x_1028] ->
X)
ETA: (if (x_602 > m_1015) (l0 (array_max_1011 x_1028 (i_1013 + 1) x_1039 x_602 k_array_max_1129))
       (l1 (array_max_1011 x_1028 (i_1013 + 1) x_1039 m_1015 k_array_max_1129))): X
ETA: (l1 (array_max_1011 x_1028 (i_1013 + 1) x_1039 m_1015 k_array_max_1129)): X
ETA: (array_max_1011 x_1028 (i_1013 + 1) x_1039 m_1015 k_array_max_1129): X
ETA: k_array_max_1129: (x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028] ->
X)
ETA_AUX: k_array_max_1129: (x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028] ->
X)
ETA_AUX: x__654: x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 >= x_1028]
ETA_AUX: (k_array_max_1129 x__654): X
ETA: m_1015: int[i_1013 + 1 <= 0]
ETA: x_1039: (x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X)
ETA_AUX: x_1039: (x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X)
ETA_AUX: x__655: x_1:int[x_1 <= 0]
ETA_AUX: (x_1039 x__655): ((x_2:int[x_2 >= x_1028] -> X) ->
X)
ETA_AUX: x__656: (x_1:int[x_1 >= x_1028] ->
X)
ETA_AUX: x__657: x_1:int[x_1 >= x_1028]
ETA_AUX: (x__656 x__657): X
ETA_AUX: (x_1039 x__655 (fun (x__657:x_1:int[x_1 >= x_1028]) -> (x__656 x__657))): X
ETA: (i_1013 + 1): int
ETA: x_1028: int
ETA_AUX: (array_max_1011 x_1028 (i_1013 + 1)
           (fun (x__655:x_1:int[x_1 <= 0]) (x__656:(x_1:int[x_1 >= x_1028] -> X)) ->
            (x_1039 x__655 (fun (x__657:x_1:int[x_1 >= x_1028]) -> (x__656 x__657)))) m_1015
           (fun (x__654:x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__654))): X
ETA: (l0 (array_max_1011 x_1028 (i_1013 + 1) x_1039 x_602 k_array_max_1129)): X
ETA: (array_max_1011 x_1028 (i_1013 + 1) x_1039 x_602 k_array_max_1129): X
ETA: k_array_max_1129: (x_1:int[x_1 >= x_602; x_1 >= x_1028; i_1013 + 1 >= x_1028] ->
X)
ETA_AUX: k_array_max_1129: (x_1:int[x_1 >= x_602; x_1 >= x_1028; i_1013 + 1 >= x_1028] ->
X)
ETA_AUX: x__658: x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 >= x_1028]
ETA_AUX: (k_array_max_1129 x__658): X
ETA: x_602: int[i_1013 + 1 <= 0]
ETA: x_1039: (x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X)
ETA_AUX: x_1039: (x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X)
ETA_AUX: x__659: x_1:int[x_1 <= 0]
ETA_AUX: (x_1039 x__659): ((x_2:int[x_2 >= x_1028] -> X) ->
X)
ETA_AUX: x__660: (x_1:int[x_1 >= x_1028] ->
X)
ETA_AUX: x__661: x_1:int[x_1 >= x_1028]
ETA_AUX: (x__660 x__661): X
ETA_AUX: (x_1039 x__659 (fun (x__661:x_1:int[x_1 >= x_1028]) -> (x__660 x__661))): X
ETA: (i_1013 + 1): int
ETA: x_1028: int
ETA_AUX: (array_max_1011 x_1028 (i_1013 + 1)
           (fun (x__659:x_1:int[x_1 <= 0]) (x__660:(x_1:int[x_1 >= x_1028] -> X)) ->
            (x_1039 x__659 (fun (x__661:x_1:int[x_1 >= x_1028]) -> (x__660 x__661)))) x_602
           (fun (x__658:x_1:int[x_1 >= x_602; x_1 >= x_1028; i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__658))): X
ETA: i_1013: x_1:int[x_1 <= 0]
ETA_AUX: (x_1039 i_1013
           (fun (x_602:x_1:int[x_1 >= x_1028]) ->
            (if (x_602 > m_1015)
              (l0
                (array_max_1011 x_1028 (i_1013 + 1)
                  (fun (x__659:x_1:int[x_1 <= 0]) (x__660:(x_1:int[x_1 >= x_1028] -> X)) ->
                   (x_1039 x__659 (fun (x__661:x_1:int[x_1 >= x_1028]) -> (x__660 x__661)))) x_602
                  (fun (x__658:x_1:int[x_1 >= x_602; x_1 >= x_1028; i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__658))))
              (l1
                (array_max_1011 x_1028 (i_1013 + 1)
                  (fun (x__655:x_1:int[x_1 <= 0]) (x__656:(x_1:int[x_1 >= x_1028] -> X)) ->
                   (x_1039 x__655 (fun (x__657:x_1:int[x_1 >= x_1028]) -> (x__656 x__657)))) m_1015
                  (fun (x__654:x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                   (k_array_max_1129 x__654))))))): X
ETA: (k ()): X
ETA: (): unit
ETA_AUX: (k ()): X
ETA: (l0
       (array_max_1011 n_1019 i_1020 (fun i_637 k_make_array_638 -> (k_make_array_638 (n_1019 - i_637))) -1
         (fun m_619 -> (if (n_1019 <= m_619) (l0 (k_main_1185 ())) (l1 (fail_1325 true k_main_1185)))))): X
ETA: (array_max_1011 n_1019 i_1020 (fun i_637 k_make_array_638 -> (k_make_array_638 (n_1019 - i_637))) -1
       (fun m_619 -> (if (n_1019 <= m_619) (l0 (k_main_1185 ())) (l1 (fail_1325 true k_main_1185))))): X
ETA: (fun m_619 -> (if (n_1019 <= m_619) (l0 (k_main_1185 ())) (l1 (fail_1325 true k_main_1185)))): (x_1:int[
x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019] ->
X)
ETA: (if (n_1019 <= m_619) (l0 (k_main_1185 ())) (l1 (fail_1325 true k_main_1185))): X
ETA: (l1 (fail_1325 true k_main_1185)): X
ETA: (fail_1325 true k_main_1185): X
ETA: k_main_1185: (unit ->
X)
ETA_AUX: k_main_1185: (unit ->
X)
ETA_AUX: x__662: unit
ETA_AUX: (k_main_1185 x__662): X
ETA: true: x_1:bool[x_1]
ETA_AUX: (fail_1325 true (fun (x__662:unit) -> (k_main_1185 x__662))): X
ETA: (l0 (k_main_1185 ())): X
ETA: (k_main_1185 ()): X
ETA: (): unit
ETA_AUX: (k_main_1185 ()): X
ETA: -1: int[i_1020 <= 0]
ETA: (fun i_637 k_make_array_638 -> (k_make_array_638 (n_1019 - i_637))): (
x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= n_1019] -> X) -> X)
ETA: (fun k_make_array_638 -> (k_make_array_638 (n_1019 - i_637))): ((x_2:int[
x_2 >= n_1019] -> X) ->
X)
ETA: (k_make_array_638 (n_1019 - i_637)): X
ETA: (n_1019 - i_637): x_1:int[x_1 >= n_1019]
ETA_AUX: (k_make_array_638 (n_1019 - i_637)): X
ETA: i_1020: int
ETA: n_1019: int
ETA_AUX: (array_max_1011 n_1019 i_1020
           (fun (i_637:x_1:int[x_1 <= 0]) (k_make_array_638:(x_1:int[x_1 >= n_1019] -> X)) ->
            (k_make_array_638 (n_1019 - i_637))) -1
           (fun (m_619:x_1:int[x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019]) ->
            (if (n_1019 <= m_619) (l0 (k_main_1185 ()))
              (l1 (fail_1325 true (fun (x__662:unit) -> (k_main_1185 x__662))))))): X
ETA: (l1 (k_main_1185 ())): X
ETA: (k_main_1185 ()): X
ETA: (): unit
ETA_AUX: (k_main_1185 ()): X
ETA_EXPAND:
Main: main_1316
  main_1316 ->
      (rand_int
        (fun (arg1_621:int) ->
         (rand_int
           (fun (arg2_624:int) ->
            (if (arg1_621 > 0)
              (l0
                (if (arg2_624 >= 0) (l0 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) (arg2_624 <= 0)))
                  (l1 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false))))
              (l1 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false))))))).
  array_max_1011 x_1028 i_1013 x_1039 m_1015 k_array_max_1129 when (i_1013 >= x_1028) ->
      (l0 (k_array_max_1129 m_1015)).
  array_max_1011 x_1028 i_1013 x_1039 m_1015 k_array_max_1129 when (not (i_1013 >= x_1028)) ->
      (l1
        (x_1039 i_1013
          (fun (x_602:x_1:int[x_1 >= x_1028]) ->
           (if (x_602 > m_1015)
             (l0
               (array_max_1011 x_1028 (i_1013 + 1)
                 (fun (x__659:x_1:int[x_1 <= 0]) (x__660:(x_1:int[x_1 >= x_1028] -> X)) ->
                  (x_1039 x__659 (fun (x__661:x_1:int[x_1 >= x_1028]) -> (x__660 x__661)))) x_602
                 (fun (x__658:x_1:int[x_1 >= x_602; x_1 >= x_1028; i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__658))))
             (l1
               (array_max_1011 x_1028 (i_1013 + 1)
                 (fun (x__655:x_1:int[x_1 <= 0]) (x__656:(x_1:int[x_1 >= x_1028] -> X)) ->
                  (x_1039 x__655 (fun (x__657:x_1:int[x_1 >= x_1028]) -> (x__656 x__657)))) m_1015
                 (fun (x__654:x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__654)))))))).
  fail_1325 b k -> {fail} => (k ()).
  k_main_1192 i_1020 n_1019 k_main_1185 b_1298 when b_1298 ->
      (l0
        (array_max_1011 n_1019 i_1020
          (fun (i_637:x_1:int[x_1 <= 0]) (k_make_array_638:(x_1:int[x_1 >= n_1019] -> X)) ->
           (k_make_array_638 (n_1019 - i_637))) -1
          (fun (m_619:x_1:int[x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019]) ->
           (if (n_1019 <= m_619) (l0 (k_main_1185 ()))
             (l1 (fail_1325 true (fun (x__662:unit) -> (k_main_1185 x__662)))))))).
  k_main_1192 i_1020 n_1019 k_main_1185 b_1298 when (not b_1298) -> (l1 (k_main_1185 ())).

main_1316: ENV:

main_1316: (rand_int
             (fun (arg1_621:int) ->
              (rand_int
                (fun (arg2_624:int) ->
                 (if (arg1_621 > 0)
                   (l0
                     (if (arg2_624 >= 0)
                       (l0 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) (arg2_624 <= 0)))
                       (l1 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false))))
                   (l1 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false))))))) ===> (
rand_int
 (fun (arg1_621:int) ->
  (rand_int
    (fun (arg2_624:int) ->
     (if (arg1_621 > 0)
       (l0
         (if (arg2_624 >= 0) (l0 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) (arg2_624 <= 0)))
           (l1 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false))))
       (l1 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false)))))))
main_1316:: (rand_int
              (fun (arg1_621:int) ->
               (rand_int
                 (fun (arg2_624:int) ->
                  (if (arg1_621 > 0)
                    (l0
                      (if (arg2_624 >= 0)
                        (l0 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) (arg2_624 <= 0)))
                        (l1 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false))))
                    (l1 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false)))))))
abstract_term: (rand_int
                 (fun (arg1_621:int) ->
                  (rand_int
                    (fun (arg2_624:int) ->
                     (if (arg1_621 > 0)
                       (l0
                         (if (arg2_624 >= 0)
                           (l0 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) (arg2_624 <= 0)))
                           (l1 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false))))
                       (l1 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false))))))): X
abstract_term: (fun (arg1_621:int) ->
                (rand_int
                  (fun (arg2_624:int) ->
                   (if (arg1_621 > 0)
                     (l0
                       (if (arg2_624 >= 0)
                         (l0 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) (arg2_624 <= 0)))
                         (l1 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false))))
                     (l1 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false)))))): (int ->
X)
abst_arg: arg1_621, int
abst_arg: arg1_621, int
abstract_term: (rand_int
                 (fun (arg2_624:int) ->
                  (if (arg1_621 > 0)
                    (l0
                      (if (arg2_624 >= 0)
                        (l0 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) (arg2_624 <= 0)))
                        (l1 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false))))
                    (l1 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false))))): X
abstract_term: (fun (arg2_624:int) ->
                (if (arg1_621 > 0)
                  (l0
                    (if (arg2_624 >= 0)
                      (l0 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) (arg2_624 <= 0)))
                      (l1 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false))))
                  (l1 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false)))): (int ->
X)
abst_arg: arg2_624, int
abst_arg: arg2_624, int
abstract_term: (if (arg1_621 > 0)
                 (l0
                   (if (arg2_624 >= 0)
                     (l0 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) (arg2_624 <= 0)))
                     (l1 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false))))
                 (l1 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false))): X
abstract_term: (arg1_621 > 0): x_1:bool[x_1]
cond: true
pbs:
p:(arg1_621 > 0)
tt:false
ff:false

abstract_term: (l0
                 (if (arg2_624 >= 0) (l0 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) (arg2_624 <= 0)))
                   (l1 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false)))): X
abstract_term: (if (arg2_624 >= 0) (l0 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) (arg2_624 <= 0)))
                 (l1 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false))): X
abstract_term: (arg2_624 >= 0): x_1:bool[x_1]
cond: (arg1_621 > 0); true
pbs:
p:(arg2_624 >= 0)
tt:false
ff:false

abstract_term: (l0 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) (arg2_624 <= 0))): X
abstract_term: (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) (arg2_624 <= 0)): X
abstract_term: (arg2_624 <= 0): x_1:bool[(not x_1) || arg2_624 <= 0;
                                         (not x_1) || 1 <= 0 - arg2_624 + arg1_621 && x_1; x_1]
cond: (arg2_624 >= 0); (arg1_621 > 0); true
pbs:
p:((not (arg2_624 <= 0)) || (arg2_624 <= 0))
tt:true
ff:false

cond: (arg2_624 >= 0); (arg1_621 > 0); true
pbs:
p:((not (arg2_624 <= 0)) || ((1 <= ((0 - arg2_624) + arg1_621)) && (arg2_624 <= 0)))
tt:true
ff:false

cond: (arg2_624 >= 0); (arg1_621 > 0); true
pbs:
p:(arg2_624 <= 0)
tt:false
ff:false

abstract_term: (fun (main_596:unit) -> end): (unit ->
X)
abst_arg: main_596, unit
abst_arg: main_596, unit
abstract_term: end: X
abstract_term: arg1_621: int
abstract_term: arg2_624: int
abstract_term: (l1 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false)): X
abstract_term: (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false): X
abstract_term: false: x_1:bool[(not x_1) || arg2_624 <= 0; (not x_1) || 1 <= 0 - arg2_624 + arg1_621 && x_1; x_1]
cond: (not (arg2_624 >= 0)); (arg1_621 > 0); true
pbs:
p:((not false) || (arg2_624 <= 0))
tt:true
ff:false

cond: (not (arg2_624 >= 0)); (arg1_621 > 0); true
pbs:
p:((not false) || ((1 <= ((0 - arg2_624) + arg1_621)) && false))
tt:true
ff:false

cond: (not (arg2_624 >= 0)); (arg1_621 > 0); true
pbs:
p:false
tt:false
ff:true

abstract_term: (fun (main_596:unit) -> end): (unit ->
X)
abst_arg: main_596, unit
abst_arg: main_596, unit
abstract_term: end: X
abstract_term: arg1_621: int
abstract_term: arg2_624: int
abstract_term: (l1 (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false)): X
abstract_term: (k_main_1192 arg2_624 arg1_621 (fun (main_596:unit) -> end) false): X
abstract_term: false: x_1:bool[(not x_1) || arg2_624 <= 0; (not x_1) || 1 <= 0 - arg2_624 + arg1_621 && x_1; x_1]
cond: (not (arg1_621 > 0)); true
pbs:
p:((not false) || (arg2_624 <= 0))
tt:true
ff:false

cond: (not (arg1_621 > 0)); true
pbs:
p:((not false) || ((1 <= ((0 - arg2_624) + arg1_621)) && false))
tt:true
ff:false

cond: (not (arg1_621 > 0)); true
pbs:
p:false
tt:false
ff:true

abstract_term: (fun (main_596:unit) -> end): (unit ->
X)
abst_arg: main_596, unit
abst_arg: main_596, unit
abstract_term: end: X
abstract_term: arg1_621: int
abstract_term: arg2_624: int
array_max_1011: ENV: x_1028:int, i_1013:int, x_1039:(x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X),
m_1015:int[i_1013 <= 0], k_array_max_1129:(x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 >= x_1028] -> X),


abst_arg: x_1028, int
abst_arg: i_1013, int
abst_arg: x_1039, (x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X)
abst_arg: m_1015, int[i_1013 <= 0]
abst_arg: k_array_max_1129, (x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 >= x_1028] ->
X)
abst_arg: x_1028, int
abst_arg: i_1013, int
abst_arg: x_1039, (x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X)
abst_arg: m_1015, int[i_1013 <= 0]
abst_arg: k_array_max_1129, (x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 >= x_1028] ->
X)
array_max_1011: (l0 (k_array_max_1129 m_1015)) ===> (l0 (k_array_max_1129 m_1015))
array_max_1011:: (l0 (k_array_max_1129 m_1015))
abstract_term: (l0 (k_array_max_1129 m_1015)): X
abstract_term: (k_array_max_1129 m_1015): X
abstract_term: m_1015: x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 >= x_1028]
cond: (i_1013 >= x_1028)
pbs: m_1015 := (i_1013 <= 0)
p:(m_1015 >= m_1015)
tt:true
ff:false

cond: (i_1013 >= x_1028)
pbs: m_1015 := (i_1013 <= 0)
p:(m_1015 >= x_1028)
tt:false
ff:false

cond: (i_1013 >= x_1028)
pbs: m_1015 := (i_1013 <= 0)
p:(i_1013 >= x_1028)
tt:true
ff:false

array_max_1011: ENV: x_1028:int, i_1013:int, x_1039:(x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X),
m_1015:int[i_1013 <= 0], k_array_max_1129:(x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 >= x_1028] -> X),


abst_arg: x_1028, int
abst_arg: i_1013, int
abst_arg: x_1039, (x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X)
abst_arg: m_1015, int[i_1013 <= 0]
abst_arg: k_array_max_1129, (x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 >= x_1028] ->
X)
abst_arg: x_1028, int
abst_arg: i_1013, int
abst_arg: x_1039, (x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X)
abst_arg: m_1015, int[i_1013 <= 0]
abst_arg: k_array_max_1129, (x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 >= x_1028] ->
X)
array_max_1011: (l1
                  (x_1039 i_1013
                    (fun (x_602:x_1:int[x_1 >= x_1028]) ->
                     (if (x_602 > m_1015)
                       (l0
                         (array_max_1011 x_1028 (i_1013 + 1)
                           (fun (x__659:x_1:int[x_1 <= 0]) (x__660:(x_1:int[x_1 >= x_1028] -> X)) ->
                            (x_1039 x__659 (fun (x__661:x_1:int[x_1 >= x_1028]) -> (x__660 x__661)))) x_602
                           (fun (x__658:x_1:int[x_1 >= x_602; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                            (k_array_max_1129 x__658))))
                       (l1
                         (array_max_1011 x_1028 (i_1013 + 1)
                           (fun (x__655:x_1:int[x_1 <= 0]) (x__656:(x_1:int[x_1 >= x_1028] -> X)) ->
                            (x_1039 x__655 (fun (x__657:x_1:int[x_1 >= x_1028]) -> (x__656 x__657)))) m_1015
                           (fun (x__654:x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                            (k_array_max_1129 x__654)))))))) ===> (l1
                                                                    (
                                                                    x_1039 i_1013
                                                                    (fun
                                                                    (x_602:x_1:int[x_1 >= x_1028]) ->
                                                                    (if (
                                                                    x_602 > m_1015)
                                                                    (l0
                                                                    (array_max_1011 x_1028 (
                                                                    i_1013 + 1)
                                                                    (fun
                                                                    (x__659:x_1:int[
                                                                    x_1 <= 0]) (x__660:(x_1:int[
                                                                    x_1 >= x_1028] -> X)) ->
                                                                    (x_1039 x__659
                                                                    (fun
                                                                    (x__661:x_1:int[x_1 >= x_1028]) -> (
                                                                    x__660 x__661)))) x_602
                                                                    (fun
                                                                    (x__658:x_1:int[
                                                                    x_1 >= x_602; x_1 >= x_1028; i_1013 + 1 >= x_1028])
                                                                    -> (
                                                                    k_array_max_1129 x__658))))
                                                                    (l1
                                                                    (array_max_1011 x_1028 (
                                                                    i_1013 + 1)
                                                                    (fun
                                                                    (x__655:x_1:int[
                                                                    x_1 <= 0]) (x__656:(x_1:int[
                                                                    x_1 >= x_1028] -> X)) ->
                                                                    (x_1039 x__655
                                                                    (fun
                                                                    (x__657:x_1:int[x_1 >= x_1028]) -> (
                                                                    x__656 x__657)))) m_1015
                                                                    (fun
                                                                    (x__654:x_1:int[
                                                                    x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028])
                                                                    -> (
                                                                    k_array_max_1129 x__654))))))))
array_max_1011:: (l1
                   (x_1039 i_1013
                     (fun (x_602:x_1:int[x_1 >= x_1028]) ->
                      (if (x_602 > m_1015)
                        (l0
                          (array_max_1011 x_1028 (i_1013 + 1)
                            (fun (x__659:x_1:int[x_1 <= 0]) (x__660:(x_1:int[x_1 >= x_1028] -> X)) ->
                             (x_1039 x__659 (fun (x__661:x_1:int[x_1 >= x_1028]) -> (x__660 x__661)))) x_602
                            (fun (x__658:x_1:int[x_1 >= x_602; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                             (k_array_max_1129 x__658))))
                        (l1
                          (array_max_1011 x_1028 (i_1013 + 1)
                            (fun (x__655:x_1:int[x_1 <= 0]) (x__656:(x_1:int[x_1 >= x_1028] -> X)) ->
                             (x_1039 x__655 (fun (x__657:x_1:int[x_1 >= x_1028]) -> (x__656 x__657)))) m_1015
                            (fun (x__654:x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                             (k_array_max_1129 x__654))))))))
abstract_term: (l1
                 (x_1039 i_1013
                   (fun (x_602:x_1:int[x_1 >= x_1028]) ->
                    (if (x_602 > m_1015)
                      (l0
                        (array_max_1011 x_1028 (i_1013 + 1)
                          (fun (x__659:x_1:int[x_1 <= 0]) (x__660:(x_1:int[x_1 >= x_1028] -> X)) ->
                           (x_1039 x__659 (fun (x__661:x_1:int[x_1 >= x_1028]) -> (x__660 x__661)))) x_602
                          (fun (x__658:x_1:int[x_1 >= x_602; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                           (k_array_max_1129 x__658))))
                      (l1
                        (array_max_1011 x_1028 (i_1013 + 1)
                          (fun (x__655:x_1:int[x_1 <= 0]) (x__656:(x_1:int[x_1 >= x_1028] -> X)) ->
                           (x_1039 x__655 (fun (x__657:x_1:int[x_1 >= x_1028]) -> (x__656 x__657)))) m_1015
                          (fun (x__654:x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                           (k_array_max_1129 x__654)))))))): X
abstract_term: (x_1039 i_1013
                 (fun (x_602:x_1:int[x_1 >= x_1028]) ->
                  (if (x_602 > m_1015)
                    (l0
                      (array_max_1011 x_1028 (i_1013 + 1)
                        (fun (x__659:x_1:int[x_1 <= 0]) (x__660:(x_1:int[x_1 >= x_1028] -> X)) ->
                         (x_1039 x__659 (fun (x__661:x_1:int[x_1 >= x_1028]) -> (x__660 x__661)))) x_602
                        (fun (x__658:x_1:int[x_1 >= x_602; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                         (k_array_max_1129 x__658))))
                    (l1
                      (array_max_1011 x_1028 (i_1013 + 1)
                        (fun (x__655:x_1:int[x_1 <= 0]) (x__656:(x_1:int[x_1 >= x_1028] -> X)) ->
                         (x_1039 x__655 (fun (x__657:x_1:int[x_1 >= x_1028]) -> (x__656 x__657)))) m_1015
                        (fun (x__654:x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                         (k_array_max_1129 x__654))))))): X
abstract_term: (fun (x_602:x_1:int[x_1 >= x_1028]) ->
                (if (x_602 > m_1015)
                  (l0
                    (array_max_1011 x_1028 (i_1013 + 1)
                      (fun (x__659:x_1:int[x_1 <= 0]) (x__660:(x_1:int[x_1 >= x_1028] -> X)) ->
                       (x_1039 x__659 (fun (x__661:x_1:int[x_1 >= x_1028]) -> (x__660 x__661)))) x_602
                      (fun (x__658:x_1:int[x_1 >= x_602; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                       (k_array_max_1129 x__658))))
                  (l1
                    (array_max_1011 x_1028 (i_1013 + 1)
                      (fun (x__655:x_1:int[x_1 <= 0]) (x__656:(x_1:int[x_1 >= x_1028] -> X)) ->
                       (x_1039 x__655 (fun (x__657:x_1:int[x_1 >= x_1028]) -> (x__656 x__657)))) m_1015
                      (fun (x__654:x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                       (k_array_max_1129 x__654)))))): (x_1:int[x_1 >= x_1028] ->
X)
abst_arg: x_602, x_1:int[x_1 >= x_1028]
abst_arg: x_602, x_1:int[x_1 >= x_1028]
abstract_term: (if (x_602 > m_1015)
                 (l0
                   (array_max_1011 x_1028 (i_1013 + 1)
                     (fun (x__659:x_1:int[x_1 <= 0]) (x__660:(x_1:int[x_1 >= x_1028] -> X)) ->
                      (x_1039 x__659 (fun (x__661:x_1:int[x_1 >= x_1028]) -> (x__660 x__661)))) x_602
                     (fun (x__658:x_1:int[x_1 >= x_602; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                      (k_array_max_1129 x__658))))
                 (l1
                   (array_max_1011 x_1028 (i_1013 + 1)
                     (fun (x__655:x_1:int[x_1 <= 0]) (x__656:(x_1:int[x_1 >= x_1028] -> X)) ->
                      (x_1039 x__655 (fun (x__657:x_1:int[x_1 >= x_1028]) -> (x__656 x__657)))) m_1015
                     (fun (x__654:x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                      (k_array_max_1129 x__654))))): X
abstract_term: (x_602 > m_1015): x_1:bool[x_1]
cond: (not (i_1013 >= x_1028))
pbs: x_602 := (x_602 >= x_1028);
     m_1015 := (i_1013 <= 0)
p:(x_602 > m_1015)
tt:false
ff:false

abstract_term: (l0
                 (array_max_1011 x_1028 (i_1013 + 1)
                   (fun (x__659:x_1:int[x_1 <= 0]) (x__660:(x_1:int[x_1 >= x_1028] -> X)) ->
                    (x_1039 x__659 (fun (x__661:x_1:int[x_1 >= x_1028]) -> (x__660 x__661)))) x_602
                   (fun (x__658:x_1:int[x_1 >= x_602; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                    (k_array_max_1129 x__658)))): X
abstract_term: (array_max_1011 x_1028 (i_1013 + 1)
                 (fun (x__659:x_1:int[x_1 <= 0]) (x__660:(x_1:int[x_1 >= x_1028] -> X)) ->
                  (x_1039 x__659 (fun (x__661:x_1:int[x_1 >= x_1028]) -> (x__660 x__661)))) x_602
                 (fun (x__658:x_1:int[x_1 >= x_602; x_1 >= x_1028; i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__658))): X
abstract_term: (fun (x__658:x_1:int[x_1 >= x_602; x_1 >= x_1028; i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__658)): (x_1:int[
x_1 >= x_602; x_1 >= x_1028; i_1013 + 1 >= x_1028] ->
X)
abst_arg: x__658, x_1:int[x_1 >= x_602; x_1 >= x_1028; i_1013 + 1 >= x_1028]
abst_arg: x__658, x_1:int[x_1 >= x_602; x_1 >= x_1028; i_1013 + 1 >= x_1028]
abstract_term: (k_array_max_1129 x__658): X
abstract_term: x__658: x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 >= x_1028]
cond: (x_602 > m_1015); (not (i_1013 >= x_1028))
pbs: x_3_658 := ((i_1013 + 1) >= x_1028);
     x_2_658 := (x__658 >= x_1028);
     x_1_658 := (x__658 >= x_602);
     x_602 := (x_602 >= x_1028);
     m_1015 := (i_1013 <= 0)
p:(x__658 >= m_1015)
tt:x_1_658
ff:false

cond: (x_602 > m_1015); (not (i_1013 >= x_1028))
pbs: x_3_658 := ((i_1013 + 1) >= x_1028);
     x_2_658 := (x__658 >= x_1028);
     x_1_658 := (x__658 >= x_602);
     x_602 := (x_602 >= x_1028);
     m_1015 := (i_1013 <= 0)
p:(x__658 >= x_1028)
tt:(x_2_658 || (x_1_658 && x_602))
ff:false

cond: (x_602 > m_1015); (not (i_1013 >= x_1028))
pbs: x_3_658 := ((i_1013 + 1) >= x_1028);
     x_2_658 := (x__658 >= x_1028);
     x_1_658 := (x__658 >= x_602);
     x_602 := (x_602 >= x_1028);
     m_1015 := (i_1013 <= 0)
p:(i_1013 >= x_1028)
tt:false
ff:true

abstract_term: x_602: int[i_1013 + 1 <= 0]
cond: (x_602 > m_1015); (not (i_1013 >= x_1028))
pbs: x_602 := (x_602 >= x_1028);
     m_1015 := (i_1013 <= 0)
p:((i_1013 + 1) <= 0)
tt:false
ff:false

abstract_term: (fun (x__659:x_1:int[x_1 <= 0]) (x__660:(x_1:int[x_1 >= x_1028] -> X)) ->
                (x_1039 x__659 (fun (x__661:x_1:int[x_1 >= x_1028]) -> (x__660 x__661)))): (
x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X)
abst_arg: x__659, x_1:int[x_1 <= 0]
abst_arg: x__660, (x_1:int[x_1 >= x_1028] ->
X)
abst_arg: x__659, x_1:int[x_1 <= 0]
abst_arg: x__660, (x_1:int[x_1 >= x_1028] ->
X)
abstract_term: (x_1039 x__659 (fun (x__661:x_1:int[x_1 >= x_1028]) -> (x__660 x__661))): X
abstract_term: (fun (x__661:x_1:int[x_1 >= x_1028]) -> (x__660 x__661)): (x_1:int[
x_1 >= x_1028] ->
X)
abst_arg: x__661, x_1:int[x_1 >= x_1028]
abst_arg: x__661, x_1:int[x_1 >= x_1028]
abstract_term: (x__660 x__661): X
abstract_term: x__661: x_1:int[x_1 >= x_1028]
cond: (x_602 > m_1015); (not (i_1013 >= x_1028))
pbs: x__661 := (x__661 >= x_1028);
     x__659 := (x__659 <= 0);
     x_602 := (x_602 >= x_1028);
     m_1015 := (i_1013 <= 0)
p:(x__661 >= x_1028)
tt:x__661
ff:false

abstract_term: x__659: x_1:int[x_1 <= 0]
cond: (x_602 > m_1015); (not (i_1013 >= x_1028))
pbs: x__659 := (x__659 <= 0);
     x_602 := (x_602 >= x_1028);
     m_1015 := (i_1013 <= 0)
p:(x__659 <= 0)
tt:x__659
ff:false

abstract_term: (i_1013 + 1): int
abstract_term: x_1028: int
abstract_term: (l1
                 (array_max_1011 x_1028 (i_1013 + 1)
                   (fun (x__655:x_1:int[x_1 <= 0]) (x__656:(x_1:int[x_1 >= x_1028] -> X)) ->
                    (x_1039 x__655 (fun (x__657:x_1:int[x_1 >= x_1028]) -> (x__656 x__657)))) m_1015
                   (fun (x__654:x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                    (k_array_max_1129 x__654)))): X
abstract_term: (array_max_1011 x_1028 (i_1013 + 1)
                 (fun (x__655:x_1:int[x_1 <= 0]) (x__656:(x_1:int[x_1 >= x_1028] -> X)) ->
                  (x_1039 x__655 (fun (x__657:x_1:int[x_1 >= x_1028]) -> (x__656 x__657)))) m_1015
                 (fun (x__654:x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__654))): X
abstract_term: (fun (x__654:x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__654)): (x_1:int[
x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028] ->
X)
abst_arg: x__654, x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]
abst_arg: x__654, x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]
abstract_term: (k_array_max_1129 x__654): X
abstract_term: x__654: x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 >= x_1028]
cond: (not (x_602 > m_1015)); (not (i_1013 >= x_1028))
pbs: x_3_654 := ((i_1013 + 1) >= x_1028);
     x_2_654 := (x__654 >= x_1028);
     x_1_654 := (x__654 >= m_1015);
     x_602 := (x_602 >= x_1028);
     m_1015 := (i_1013 <= 0)
p:(x__654 >= m_1015)
tt:x_1_654
ff:false

cond: (not (x_602 > m_1015)); (not (i_1013 >= x_1028))
pbs: x_3_654 := ((i_1013 + 1) >= x_1028);
     x_2_654 := (x__654 >= x_1028);
     x_1_654 := (x__654 >= m_1015);
     x_602 := (x_602 >= x_1028);
     m_1015 := (i_1013 <= 0)
p:(x__654 >= x_1028)
tt:(x_2_654 || (x_1_654 && x_602))
ff:false

cond: (not (x_602 > m_1015)); (not (i_1013 >= x_1028))
pbs: x_3_654 := ((i_1013 + 1) >= x_1028);
     x_2_654 := (x__654 >= x_1028);
     x_1_654 := (x__654 >= m_1015);
     x_602 := (x_602 >= x_1028);
     m_1015 := (i_1013 <= 0)
p:(i_1013 >= x_1028)
tt:false
ff:true

abstract_term: m_1015: int[i_1013 + 1 <= 0]
cond: (not (x_602 > m_1015)); (not (i_1013 >= x_1028))
pbs: x_602 := (x_602 >= x_1028);
     m_1015 := (i_1013 <= 0)
p:((i_1013 + 1) <= 0)
tt:false
ff:false

abstract_term: (fun (x__655:x_1:int[x_1 <= 0]) (x__656:(x_1:int[x_1 >= x_1028] -> X)) ->
                (x_1039 x__655 (fun (x__657:x_1:int[x_1 >= x_1028]) -> (x__656 x__657)))): (
x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X)
abst_arg: x__655, x_1:int[x_1 <= 0]
abst_arg: x__656, (x_1:int[x_1 >= x_1028] ->
X)
abst_arg: x__655, x_1:int[x_1 <= 0]
abst_arg: x__656, (x_1:int[x_1 >= x_1028] ->
X)
abstract_term: (x_1039 x__655 (fun (x__657:x_1:int[x_1 >= x_1028]) -> (x__656 x__657))): X
abstract_term: (fun (x__657:x_1:int[x_1 >= x_1028]) -> (x__656 x__657)): (x_1:int[
x_1 >= x_1028] ->
X)
abst_arg: x__657, x_1:int[x_1 >= x_1028]
abst_arg: x__657, x_1:int[x_1 >= x_1028]
abstract_term: (x__656 x__657): X
abstract_term: x__657: x_1:int[x_1 >= x_1028]
cond: (not (x_602 > m_1015)); (not (i_1013 >= x_1028))
pbs: x__657 := (x__657 >= x_1028);
     x__655 := (x__655 <= 0);
     x_602 := (x_602 >= x_1028);
     m_1015 := (i_1013 <= 0)
p:(x__657 >= x_1028)
tt:x__657
ff:false

abstract_term: x__655: x_1:int[x_1 <= 0]
cond: (not (x_602 > m_1015)); (not (i_1013 >= x_1028))
pbs: x__655 := (x__655 <= 0);
     x_602 := (x_602 >= x_1028);
     m_1015 := (i_1013 <= 0)
p:(x__655 <= 0)
tt:x__655
ff:false

abstract_term: (i_1013 + 1): int
abstract_term: x_1028: int
abstract_term: i_1013: x_1:int[x_1 <= 0]
cond: (not (i_1013 >= x_1028))
pbs: m_1015 := (i_1013 <= 0)
p:(i_1013 <= 0)
tt:m_1015
ff:false

fail_1325: ENV: b:x_1:bool[x_1], k:(unit -> X),


abst_arg: b, x_1:bool[x_1]
abst_arg: k, (unit ->
X)
abst_arg: b, x_1:bool[x_1]
abst_arg: k, (unit ->
X)
fail_1325: (k ()) ===> (k ())
fail_1325:: (k ())
abstract_term: (k ()): X
abstract_term: (): unit
k_main_1192: ENV: i_1020:int, n_1019:int, k_main_1185:(unit -> X),
b_1298:x_1:bool[(not x_1) || i_1020 <= 0; (not x_1) || 1 <= 0 - i_1020 + n_1019 && x_1; x_1],


abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1185, (unit ->
X)
abst_arg: b_1298, x_1:bool[(not x_1) || i_1020 <= 0; (not x_1) || 1 <= 0 - i_1020 + n_1019 && x_1; x_1]
abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1185, (unit ->
X)
abst_arg: b_1298, x_1:bool[(not x_1) || i_1020 <= 0; (not x_1) || 1 <= 0 - i_1020 + n_1019 && x_1; x_1]
k_main_1192: (l0
               (array_max_1011 n_1019 i_1020
                 (fun (i_637:x_1:int[x_1 <= 0]) (k_make_array_638:(x_1:int[x_1 >= n_1019] -> X)) ->
                  (k_make_array_638 (n_1019 - i_637))) -1
                 (fun (m_619:x_1:int[x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019]) ->
                  (if (n_1019 <= m_619) (l0 (k_main_1185 ()))
                    (l1 (fail_1325 true (fun (x__662:unit) -> (k_main_1185 x__662)))))))) ===> (
l0
 (array_max_1011 n_1019 i_1020
   (fun (i_637:x_1:int[x_1 <= 0]) (k_make_array_638:(x_1:int[x_1 >= n_1019] -> X)) ->
    (k_make_array_638 (n_1019 - i_637))) -1
   (fun (m_619:x_1:int[x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019]) ->
    (if (n_1019 <= m_619) (l0 (k_main_1185 ())) (l1 (fail_1325 true (fun (x__662:unit) -> (k_main_1185 x__662))))))))
k_main_1192:: (l0
                (array_max_1011 n_1019 i_1020
                  (fun (i_637:x_1:int[x_1 <= 0]) (k_make_array_638:(x_1:int[x_1 >= n_1019] -> X)) ->
                   (k_make_array_638 (n_1019 - i_637))) -1
                  (fun (m_619:x_1:int[x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019]) ->
                   (if (n_1019 <= m_619) (l0 (k_main_1185 ()))
                     (l1 (fail_1325 true (fun (x__662:unit) -> (k_main_1185 x__662))))))))
abstract_term: (l0
                 (array_max_1011 n_1019 i_1020
                   (fun (i_637:x_1:int[x_1 <= 0]) (k_make_array_638:(x_1:int[x_1 >= n_1019] -> X)) ->
                    (k_make_array_638 (n_1019 - i_637))) -1
                   (fun (m_619:x_1:int[x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019]) ->
                    (if (n_1019 <= m_619) (l0 (k_main_1185 ()))
                      (l1 (fail_1325 true (fun (x__662:unit) -> (k_main_1185 x__662)))))))): X
abstract_term: (array_max_1011 n_1019 i_1020
                 (fun (i_637:x_1:int[x_1 <= 0]) (k_make_array_638:(x_1:int[x_1 >= n_1019] -> X)) ->
                  (k_make_array_638 (n_1019 - i_637))) -1
                 (fun (m_619:x_1:int[x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019]) ->
                  (if (n_1019 <= m_619) (l0 (k_main_1185 ()))
                    (l1 (fail_1325 true (fun (x__662:unit) -> (k_main_1185 x__662))))))): X
abstract_term: (fun (m_619:x_1:int[x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019]) ->
                (if (n_1019 <= m_619) (l0 (k_main_1185 ()))
                  (l1 (fail_1325 true (fun (x__662:unit) -> (k_main_1185 x__662)))))): (x_1:int[
x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019] ->
X)
abst_arg: m_619, x_1:int[x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019]
abst_arg: m_619, x_1:int[x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019]
abstract_term: (if (n_1019 <= m_619) (l0 (k_main_1185 ()))
                 (l1 (fail_1325 true (fun (x__662:unit) -> (k_main_1185 x__662))))): X
abstract_term: (n_1019 <= m_619): x_1:bool[x_1]
cond: b_1298
pbs: m3_619 := (i_1020 >= n_1019);
     m2_619 := (m_619 >= n_1019);
     m1_619 := (m_619 >= -1);
     b1_1298 := ((not b_1298) || (i_1020 <= 0));
     b2_1298 := ((not b_1298) || ((1 <= ((0 - i_1020) + n_1019)) && b_1298));
     b3_1298 := b_1298
p:(n_1019 <= m_619)
tt:m2_619
ff:false

abstract_term: (l0 (k_main_1185 ())): X
abstract_term: (k_main_1185 ()): X
abstract_term: (): unit
abstract_term: (l1 (fail_1325 true (fun (x__662:unit) -> (k_main_1185 x__662)))): X
abstract_term: (fail_1325 true (fun (x__662:unit) -> (k_main_1185 x__662))): X
abstract_term: (fun (x__662:unit) -> (k_main_1185 x__662)): (unit ->
X)
abst_arg: x__662, unit
abst_arg: x__662, unit
abstract_term: (k_main_1185 x__662): X
abstract_term: x__662: unit
abstract_term: true: x_1:bool[x_1]
cond: (not (n_1019 <= m_619)); b_1298
pbs: m3_619 := (i_1020 >= n_1019);
     m1_619 := (m_619 >= -1);
     b1_1298 := ((not b_1298) || (i_1020 <= 0));
     b2_1298 := ((not b_1298) || ((1 <= ((0 - i_1020) + n_1019)) && b_1298));
     b3_1298 := b_1298
p:true
tt:true
ff:false

abstract_term: -1: int[i_1020 <= 0]
cond: b_1298
pbs: b1_1298 := ((not b_1298) || (i_1020 <= 0));
     b2_1298 := ((not b_1298) || ((1 <= ((0 - i_1020) + n_1019)) && b_1298));
     b3_1298 := b_1298
p:(i_1020 <= 0)
tt:b1_1298
ff:false

abstract_term: (fun (i_637:x_1:int[x_1 <= 0]) (k_make_array_638:(x_1:int[x_1 >= n_1019] -> X)) ->
                (k_make_array_638 (n_1019 - i_637))): (x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= n_1019] -> X) -> X)
abst_arg: i_637, x_1:int[x_1 <= 0]
abst_arg: k_make_array_638, (x_1:int[x_1 >= n_1019] ->
X)
abst_arg: i_637, x_1:int[x_1 <= 0]
abst_arg: k_make_array_638, (x_1:int[x_1 >= n_1019] ->
X)
abstract_term: (k_make_array_638 (n_1019 - i_637)): X
abstract_term: (n_1019 - i_637): x_1:int[x_1 >= n_1019]
cond: b_1298
pbs: i_637 := (i_637 <= 0);
     b1_1298 := ((not b_1298) || (i_1020 <= 0));
     b2_1298 := ((not b_1298) || ((1 <= ((0 - i_1020) + n_1019)) && b_1298));
     b3_1298 := b_1298
p:((n_1019 - i_637) >= n_1019)
tt:i_637
ff:false

abstract_term: i_1020: int
abstract_term: n_1019: int
k_main_1192: ENV: i_1020:int, n_1019:int, k_main_1185:(unit -> X),
b_1298:x_1:bool[(not x_1) || i_1020 <= 0; (not x_1) || 1 <= 0 - i_1020 + n_1019 && x_1; x_1],


abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1185, (unit ->
X)
abst_arg: b_1298, x_1:bool[(not x_1) || i_1020 <= 0; (not x_1) || 1 <= 0 - i_1020 + n_1019 && x_1; x_1]
abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1185, (unit ->
X)
abst_arg: b_1298, x_1:bool[(not x_1) || i_1020 <= 0; (not x_1) || 1 <= 0 - i_1020 + n_1019 && x_1; x_1]
k_main_1192: (l1 (k_main_1185 ())) ===> (l1 (k_main_1185 ()))
k_main_1192:: (l1 (k_main_1185 ()))
abstract_term: (l1 (k_main_1185 ())): X
abstract_term: (k_main_1185 ()): X
abstract_term: (): unit
ABST:
Main: main_1316
  main_1316 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1192 () true true rand_bool)) (l1 (k_main_1192 () true true false))))
        (l1 (k_main_1192 () true true false))).
  array_max_1011 x_1039 m_1015 k_array_max_1129 -> (l0 (k_array_max_1129 true rand_bool true)).
  array_max_1011 x_1039 m_1015 k_array_max_1129 ->
      (l1
        (x_1039 (if m_1015 true rand_bool)
          (fun x_602 ->
           (if rand_bool
             (l0
               (array_max_1011
                 (fun x__659 x__660 ->
                  (x_1039 (if x__659 true rand_bool) (fun x__661 -> (x__660 (if x__661 true rand_bool))))) rand_bool
                 (fun x_1_658 x_2_658 x_3_658 ->
                  (k_array_max_1129 (if x_1_658 true rand_bool) (if (x_2_658 || (x_1_658 && x_602)) true rand_bool)
                    false))))
             (l1
               (array_max_1011
                 (fun x__655 x__656 ->
                  (x_1039 (if x__655 true rand_bool) (fun x__657 -> (x__656 (if x__657 true rand_bool))))) rand_bool
                 (fun x_1_654 x_2_654 x_3_654 ->
                  (k_array_max_1129 (if x_1_654 true rand_bool) (if (x_2_654 || (x_1_654 && x_602)) true rand_bool)
                    false)))))))).
  fail_1325 b k -> {fail} => k.
  k_main_1192 k_main_1185 b1_1298 b2_1298 b3_1298 ->
      (l0
        (array_max_1011 (fun i_637 k_make_array_638 -> (k_make_array_638 (if i_637 true rand_bool)))
          (if b1_1298 true rand_bool)
          (fun m1_619 m2_619 m3_619 ->
           (if (if m2_619 true rand_bool) (l0 k_main_1185) (l1 (fail_1325 true k_main_1185)))))).
  k_main_1192 k_main_1185 b1_1298 b2_1298 b3_1298 -> (if b3_1298 _|_ (l1 k_main_1185)).
Types:
  arg1_1085 : (unit -> unit)
  arg2_1087 : (unit -> unit)
  br_main_1323 : (unit -> unit)
  br_main_1323 : (unit -> unit)
  f_1322 : unit
  f_array_max_1317 : (unit -> (unit -> unit) -> unit)
  f_array_max_1318 : (unit -> (unit -> unit) -> unit)
  f_k_main_1319 : (unit -> unit)
  f_k_main_1319 : (unit -> unit)
  f_main_1320 : (unit -> unit)
  f_main_1321 : (unit -> unit)
  m_1300 : (unit -> unit)
  main_1018 : (unit -> unit)
  main_1018 : (unit -> unit)
  main_1089 : (unit -> unit)
  make_array_1008 : (unit -> unit)
  x_1016 : ((unit -> unit) -> unit -> unit)
  z_1017 : (unit -> unit)
  z_1017 : (unit -> unit)

LIFT:
Main: main_1316
  main_1316 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1192 () true true rand_bool)) (l1 (k_main_1192 () true true false))))
        (l1 (k_main_1192 () true true false))).
  array_max_1011 x_1039 m_1015 k_array_max_1129 -> (l0 (k_array_max_1129 true rand_bool true)).
  array_max_1011 x_1039 m_1015 k_array_max_1129 ->
      (l1 (x_1039 (if m_1015 true rand_bool) (f_666 x_1039 k_array_max_1129))).
  f_666 x_664 k_array_max_665 x_602 ->
      (if rand_bool (l0 (array_max_1011 (f_669 x_664) rand_bool (f_676 k_array_max_665 x_602)))
        (l1 (array_max_1011 (f_679 x_664) rand_bool (f_686 k_array_max_665 x_602)))).
  f_676 k_array_max_674 x_675 x_1_658 x_2_658 x_3_658 ->
      (k_array_max_674 (if x_1_658 true rand_bool) (if (x_2_658 || (x_1_658 && x_675)) true rand_bool) false).
  f_672 x__671 x__661 -> (x__671 (if x__661 true rand_bool)).
  f_669 x_668 x__659 x__660 -> (x_668 (if x__659 true rand_bool) (f_672 x__660)).
  f_679 x_678 x__655 x__656 -> (x_678 (if x__655 true rand_bool) (f_682 x__656)).
  f_682 x__681 x__657 -> (x__681 (if x__657 true rand_bool)).
  f_686 k_array_max_684 x_685 x_1_654 x_2_654 x_3_654 ->
      (k_array_max_684 (if x_1_654 true rand_bool) (if (x_2_654 || (x_1_654 && x_685)) true rand_bool) false).
  fail_1325 b k -> {fail} => k.
  k_main_1192 k_main_1185 b1_1298 b2_1298 b3_1298 ->
      (l0 (array_max_1011 f_688 (if b1_1298 true rand_bool) (f_691 k_main_1185))).
  f_688 i_637 k_make_array_638 -> (k_make_array_638 (if i_637 true rand_bool)).
  f_691 k_main_690 m1_619 m2_619 m3_619 ->
      (if (if m2_619 true rand_bool) (l0 k_main_690) (l1 (fail_1325 true k_main_690))).
  k_main_1192 k_main_1185 b1_1298 b2_1298 b3_1298 -> (if b3_1298 _|_ (l1 k_main_1185)).

TRANS_EAGER:
Main: main_1316
  main_1316 ->
      (let f_693 b_692 =
       (if b_692
         (l0
           (let f_695 b_694 =
            (if b_694
              (l0 (let f_697 b_696 = (k_main_1192 () true true b_696) in (if rand_bool (f_697 true) (f_697 false))))
              (l1 (k_main_1192 () true true false)))
            in (if rand_bool (f_695 true) (f_695 false)))) (l1 (k_main_1192 () true true false)))
       in (if rand_bool (f_693 true) (f_693 false))).
  array_max_1011 x_1039 m_1015 k_array_max_1129 ->
      (l0 ((let f_699 b_698 = (k_array_max_1129 true b_698) in (if rand_bool (f_699 true) (f_699 false))) true)).
  array_max_1011 x_1039 m_1015 k_array_max_1129 ->
      (l1
        ((let f_703 b_702 = (if b_702 (x_1039 true) (if rand_bool (x_1039 true) (x_1039 false))) in (f_703 m_1015))
          (f_666 x_1039 k_array_max_1129))).
  f_666 x_664 k_array_max_665 x_602 ->
      (let f_705 b_704 =
       (if b_704
         (l0
           ((let f_707 b_706 = (array_max_1011 (f_669 x_664) b_706) in (if rand_bool (f_707 true) (f_707 false)))
             (f_676 k_array_max_665 x_602)))
         (l1
           ((let f_709 b_708 = (array_max_1011 (f_679 x_664) b_708) in (if rand_bool (f_709 true) (f_709 false)))
             (f_686 k_array_max_665 x_602))))
       in (if rand_bool (f_705 true) (f_705 false))).
  f_676 k_array_max_674 x_675 x_1_658 x_2_658 x_3_658 ->
      ((let f_711 b_710 =
        ((let f_715 b_714 =
          (if b_714 (k_array_max_674 true) (if rand_bool (k_array_max_674 true) (k_array_max_674 false))) in
          (f_715 x_1_658)) b_710)
        in
        (let f_717 b_716 = (if b_716 (f_711 true) (if rand_bool (f_711 true) (f_711 false))) in
         (let f_719 b_718 =
          (if b_718 (f_717 true) (let f_721 b_720 = (if b_720 (f_717 x_675) (f_717 false)) in (f_721 x_1_658))) in
          (f_719 x_2_658)))) false).
  f_672 x__671 x__661 ->
      (let f_725 b_724 = (if b_724 (x__671 true) (if rand_bool (x__671 true) (x__671 false))) in (f_725 x__661)).
  f_669 x_668 x__659 x__660 ->
      ((let f_729 b_728 = (if b_728 (x_668 true) (if rand_bool (x_668 true) (x_668 false))) in (f_729 x__659))
        (f_672 x__660)).
  f_679 x_678 x__655 x__656 ->
      ((let f_733 b_732 = (if b_732 (x_678 true) (if rand_bool (x_678 true) (x_678 false))) in (f_733 x__655))
        (f_682 x__656)).
  f_682 x__681 x__657 ->
      (let f_737 b_736 = (if b_736 (x__681 true) (if rand_bool (x__681 true) (x__681 false))) in (f_737 x__657)).
  f_686 k_array_max_684 x_685 x_1_654 x_2_654 x_3_654 ->
      ((let f_739 b_738 =
        ((let f_743 b_742 =
          (if b_742 (k_array_max_684 true) (if rand_bool (k_array_max_684 true) (k_array_max_684 false))) in
          (f_743 x_1_654)) b_738)
        in
        (let f_745 b_744 = (if b_744 (f_739 true) (if rand_bool (f_739 true) (f_739 false))) in
         (let f_747 b_746 =
          (if b_746 (f_745 true) (let f_749 b_748 = (if b_748 (f_745 x_685) (f_745 false)) in (f_749 x_1_654))) in
          (f_747 x_2_654)))) false).
  fail_1325 b k -> {fail} => k.
  k_main_1192 k_main_1185 b1_1298 b2_1298 b3_1298 ->
      (l0
        ((let f_751 b_750 = (array_max_1011 f_688 b_750) in
          (let f_753 b_752 = (if b_752 (f_751 true) (if rand_bool (f_751 true) (f_751 false))) in (f_753 b1_1298)))
          (f_691 k_main_1185))).
  f_688 i_637 k_make_array_638 ->
      (let f_757 b_756 =
       (if b_756 (k_make_array_638 true) (if rand_bool (k_make_array_638 true) (k_make_array_638 false))) in
       (f_757 i_637)).
  f_691 k_main_690 m1_619 m2_619 m3_619 ->
      (let f_759 b_758 = (if b_758 (l0 k_main_690) (l1 (fail_1325 true k_main_690))) in
       (let f_761 b_760 = (if b_760 (f_759 true) (if rand_bool (f_759 true) (f_759 false))) in (f_761 m2_619))).
  k_main_1192 k_main_1185 b1_1298 b2_1298 b3_1298 ->
      (let f_763 b_762 = (if b_762 _|_ (l1 k_main_1185)) in (f_763 b3_1298)).

PUT_INTO_IF:
Main: main_1316
  main_1316 ->
      (let f_693 b_692 =
       (if b_692
         (l0
           (let f_695 b_694 =
            (if b_694
              (l0 (let f_697 b_696 = (k_main_1192 () true true b_696) in (if rand_bool (f_697 true) (f_697 false))))
              (l1 (k_main_1192 () true true false)))
            in (if rand_bool (f_695 true) (f_695 false)))) (l1 (k_main_1192 () true true false)))
       in (if rand_bool (f_693 true) (f_693 false))).
  array_max_1011 x_1039 m_1015 k_array_max_1129 ->
      (l0 ((let f_699 b_698 = (k_array_max_1129 true b_698) in (if rand_bool (f_699 true) (f_699 false))) true)).
  array_max_1011 x_1039 m_1015 k_array_max_1129 ->
      (l1
        ((let f_703 b_702 = (if b_702 (x_1039 true) (if rand_bool (x_1039 true) (x_1039 false))) in (f_703 m_1015))
          (f_666 x_1039 k_array_max_1129))).
  f_666 x_664 k_array_max_665 x_602 ->
      (let f_705 b_704 =
       (if b_704
         (l0
           ((let f_707 b_706 = (array_max_1011 (f_669 x_664) b_706) in (if rand_bool (f_707 true) (f_707 false)))
             (f_676 k_array_max_665 x_602)))
         (l1
           ((let f_709 b_708 = (array_max_1011 (f_679 x_664) b_708) in (if rand_bool (f_709 true) (f_709 false)))
             (f_686 k_array_max_665 x_602))))
       in (if rand_bool (f_705 true) (f_705 false))).
  f_676 k_array_max_674 x_675 x_1_658 x_2_658 x_3_658 ->
      ((let f_711 b_710 =
        ((let f_715 b_714 =
          (if b_714 (k_array_max_674 true) (if rand_bool (k_array_max_674 true) (k_array_max_674 false))) in
          (f_715 x_1_658)) b_710)
        in
        (let f_717 b_716 = (if b_716 (f_711 true) (if rand_bool (f_711 true) (f_711 false))) in
         (let f_719 b_718 =
          (if b_718 (f_717 true) (let f_721 b_720 = (if b_720 (f_717 x_675) (f_717 false)) in (f_721 x_1_658))) in
          (f_719 x_2_658)))) false).
  f_672 x__671 x__661 ->
      (let f_725 b_724 = (if b_724 (x__671 true) (if rand_bool (x__671 true) (x__671 false))) in (f_725 x__661)).
  f_669 x_668 x__659 x__660 ->
      ((let f_729 b_728 = (if b_728 (x_668 true) (if rand_bool (x_668 true) (x_668 false))) in (f_729 x__659))
        (f_672 x__660)).
  f_679 x_678 x__655 x__656 ->
      ((let f_733 b_732 = (if b_732 (x_678 true) (if rand_bool (x_678 true) (x_678 false))) in (f_733 x__655))
        (f_682 x__656)).
  f_682 x__681 x__657 ->
      (let f_737 b_736 = (if b_736 (x__681 true) (if rand_bool (x__681 true) (x__681 false))) in (f_737 x__657)).
  f_686 k_array_max_684 x_685 x_1_654 x_2_654 x_3_654 ->
      ((let f_739 b_738 =
        ((let f_743 b_742 =
          (if b_742 (k_array_max_684 true) (if rand_bool (k_array_max_684 true) (k_array_max_684 false))) in
          (f_743 x_1_654)) b_738)
        in
        (let f_745 b_744 = (if b_744 (f_739 true) (if rand_bool (f_739 true) (f_739 false))) in
         (let f_747 b_746 =
          (if b_746 (f_745 true) (let f_749 b_748 = (if b_748 (f_745 x_685) (f_745 false)) in (f_749 x_1_654))) in
          (f_747 x_2_654)))) false).
  fail_1325 b k -> {fail} => k.
  k_main_1192 k_main_1185 b1_1298 b2_1298 b3_1298 ->
      (l0
        ((let f_751 b_750 = (array_max_1011 f_688 b_750) in
          (let f_753 b_752 = (if b_752 (f_751 true) (if rand_bool (f_751 true) (f_751 false))) in (f_753 b1_1298)))
          (f_691 k_main_1185))).
  f_688 i_637 k_make_array_638 ->
      (let f_757 b_756 =
       (if b_756 (k_make_array_638 true) (if rand_bool (k_make_array_638 true) (k_make_array_638 false))) in
       (f_757 i_637)).
  f_691 k_main_690 m1_619 m2_619 m3_619 ->
      (let f_759 b_758 = (if b_758 (l0 k_main_690) (l1 (fail_1325 true k_main_690))) in
       (let f_761 b_760 = (if b_760 (f_759 true) (if rand_bool (f_759 true) (f_759 false))) in (f_761 m2_619))).
  k_main_1192 k_main_1185 b1_1298 b2_1298 b3_1298 ->
      (let f_763 b_762 = (if b_762 _|_ (l1 k_main_1185)) in (f_763 b3_1298)).

DONE!

(1-2) Checking HORS ... DONE!

Error trace::
  main_1316 ... -->
  main_1089 ... -->
  arg1_1085 ... -->
  f_main_1320 ... -->
  arg2_1087 ... -->
  f_main_1321 ... -->
  main_1018 [1/2] ... -->
  br_main_1323 [1/2] ... -->
  k_main_1192 [1/2] ... -->
  m_1300 ... -->
  array_max_1011 [1/2] ... -->
  f_k_main_1319 [2/2] ... -->
  fail_1325 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0

(1-3) Checking counterexample ... DONE!

(1-4) Discovering predicates ...
DONE!

Prefix of spurious counterexample::
0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0

Program with abstraction types (CEGAR-cycle 2)::
Main: main_1316
  main_1316 -> (main_1089 f_1322).
  arg1_1085 k_main_arg1_1244 -> (rand_int k_main_arg1_1244).
  arg2_1087 arg1_1274 k_main_arg2_1256 -> (rand_int k_main_arg2_1256).
  array_max_1011 x_1028 i_1013 x_1039 m_1015 k_array_max_1129 when (i_1013 >= x_1028) -> (k_array_max_1129 m_1015).
  array_max_1011 x_1028 i_1013 x_1039 m_1015 k_array_max_1129 when (not (i_1013 >= x_1028)) ->
      (x_1016 i_1013 m_1015 x_1028 x_1039 (f_array_max_1317 i_1013 m_1015 x_1028 k_array_max_1129 x_1039)).
  br_main_1323 b_1324 n_1019 i_1020 k_main_1185 when b_1324 -> (k_main_1192 i_1020 n_1019 k_main_1185 (i_1020 <= 0)).
  br_main_1323 b_1324 n_1019 i_1020 k_main_1185 when (not b_1324) -> (k_main_1192 i_1020 n_1019 k_main_1185 false).
  f_1322 main_1275 -> end.
  f_array_max_1317 i_1013 m_1015 x_1028 k_array_max_1129 x_1039 x_1168 ->
      (z_1017 i_1013 m_1015 x_1028 x_1168 (f_array_max_1318 i_1013 m_1015 x_1028 x_1168 k_array_max_1129 x_1039)).
  f_array_max_1318 i_1013 m_1015 x_1028 x_1168 k_array_max_1129 x_1039 z_1167 ->
      (array_max_1011 x_1028 (i_1013 + 1) x_1039 z_1167 k_array_max_1129).
  f_k_main_1319 b_1298 i_1020 n_1019 k_main_1185 m_1304 when (m_1304 >= n_1019) -> (k_main_1185 ()).
  f_k_main_1319 b_1298 i_1020 n_1019 k_main_1185 m_1304 when (not (m_1304 >= n_1019)) -> (fail_1325 true k_main_1185).
  f_main_1320 k_main_1239 arg1_1274 -> (arg2_1087 arg1_1274 (f_main_1321 arg1_1274 k_main_1239)).
  f_main_1321 arg1_1274 k_main_1239 arg2_1273 -> (main_1018 arg1_1274 arg2_1273 k_main_1239).
  fail_1325 b k -> {fail} => (k ()).
  k_main_1192 i_1020 n_1019 k_main_1185 b_1298 when b_1298 ->
      (m_1300 b_1298 i_1020 n_1019 (f_k_main_1319 b_1298 i_1020 n_1019 k_main_1185)).
  k_main_1192 i_1020 n_1019 k_main_1185 b_1298 when (not b_1298) -> (k_main_1185 ()).
  m_1300 b_1298 i_1020 n_1019 k_main_m_1301 ->
      (array_max_1011 n_1019 i_1020 (make_array_1008 n_1019) -1 k_main_m_1301).
  main_1018 n_1019 i_1020 k_main_1185 when (n_1019 > 0) -> (br_main_1323 (i_1020 >= 0) n_1019 i_1020 k_main_1185).
  main_1018 n_1019 i_1020 k_main_1185 when (not (n_1019 > 0)) -> (k_main_1192 i_1020 n_1019 k_main_1185 false).
  main_1089 k_main_1239 -> (arg1_1085 (f_main_1320 k_main_1239)).
  make_array_1008 n_1009 i_1010 k_make_array_1118 -> (k_make_array_1118 (n_1009 - i_1010)).
  x_1016 i_1013 m_1015 x_1028 x_1039 k_array_max_x_1136 -> (x_1039 i_1013 k_array_max_x_1136).
  z_1017 i_1013 m_1015 x_1028 x_1168 k_array_max_z_1145 when (x_1168 > m_1015) -> (k_array_max_z_1145 x_1168).
  z_1017 i_1013 m_1015 x_1028 x_1168 k_array_max_z_1145 when (not (x_1168 > m_1015)) -> (k_array_max_z_1145 m_1015).
Types:
  main_1316 : X
  array_max_1011 : (x_1:int ->
                    x_2:int ->
                    (x_4:int[x_4 <= 0] -> (x_6:int[x_6 >= x_1] -> X) -> X) ->
                    x_9:int[1 <= x_1 - x_2; x_2 <= 0] -> (x_11:int[x_11 >= x_9; x_11 >= x_1; x_2 >= x_1] -> X) -> X)
  fail_1325 : (x_1:bool[x_1] -> (unit -> X) -> X)
  k_main_1192 : (x_1:int ->
                 x_2:int ->
                 (unit -> X) -> x_6:bool[(not x_6) || x_1 <= 0; (not x_6) || 1 <= 0 - x_1 + x_2 && x_6; x_6] -> X)

(2-1) Abstracting ... EXPAND_NONREC:
Main: main_1316
  main_1316 ->
      (rand_int
        (fun arg1_1329 ->
         (rand_int
           (fun arg2_1332 ->
            (if (arg1_1329 > 0)
              (l0
                (if (arg2_1332 >= 0) (l0 (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) (arg2_1332 <= 0)))
                  (l1 (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) false))))
              (l1 (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) false))))))).
  array_max_1011 x_1028 i_1013 x_1039 m_1015 k_array_max_1129 when (i_1013 >= x_1028) ->
      (l0 (k_array_max_1129 m_1015)).
  array_max_1011 x_1028 i_1013 x_1039 m_1015 k_array_max_1129 when (not (i_1013 >= x_1028)) ->
      (l1
        (x_1039 i_1013
          (fun x_1310 ->
           (if (x_1310 > m_1015) (l0 (array_max_1011 x_1028 (i_1013 + 1) x_1039 x_1310 k_array_max_1129))
             (l1 (array_max_1011 x_1028 (i_1013 + 1) x_1039 m_1015 k_array_max_1129)))))).
  fail_1325 b k -> {fail} => (k ()).
  k_main_1192 i_1020 n_1019 k_main_1185 b_1298 when b_1298 ->
      (l0
        (array_max_1011 n_1019 i_1020 (fun i_1345 k_make_array_1346 -> (k_make_array_1346 (n_1019 - i_1345))) -1
          (fun m_1327 -> (if (n_1019 <= m_1327) (l0 (k_main_1185 ())) (l1 (fail_1325 true k_main_1185)))))).
  k_main_1192 i_1020 n_1019 k_main_1185 b_1298 when (not b_1298) -> (l1 (k_main_1185 ())).

ETA: (rand_int
       (fun arg1_1329 ->
        (rand_int
          (fun arg2_1332 ->
           (if (arg1_1329 > 0)
             (l0
               (if (arg2_1332 >= 0) (l0 (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) (arg2_1332 <= 0)))
                 (l1 (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) false))))
             (l1 (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) false))))))): X
ETA: (fun arg1_1329 ->
      (rand_int
        (fun arg2_1332 ->
         (if (arg1_1329 > 0)
           (l0
             (if (arg2_1332 >= 0) (l0 (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) (arg2_1332 <= 0)))
               (l1 (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) false))))
           (l1 (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) false)))))): (int ->
unit)
ETA: (rand_int
       (fun arg2_1332 ->
        (if (arg1_1329 > 0)
          (l0
            (if (arg2_1332 >= 0) (l0 (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) (arg2_1332 <= 0)))
              (l1 (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) false))))
          (l1 (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) false))))): unit
ETA: (fun arg2_1332 ->
      (if (arg1_1329 > 0)
        (l0
          (if (arg2_1332 >= 0) (l0 (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) (arg2_1332 <= 0)))
            (l1 (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) false))))
        (l1 (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) false)))): (int ->
unit)
ETA: (if (arg1_1329 > 0)
       (l0
         (if (arg2_1332 >= 0) (l0 (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) (arg2_1332 <= 0)))
           (l1 (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) false))))
       (l1 (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) false))): unit
ETA: (l1 (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) false)): unit
ETA: (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) false): unit
ETA: false: x_1:bool[(not x_1) || arg2_1332 <= 0; (not x_1) || 1 <= 0 - arg2_1332 + arg1_1329 && x_1; x_1]
ETA: (fun main_1304 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_1329: int
ETA: arg2_1332: int
ETA_AUX: (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false): unit
ETA: (l0
       (if (arg2_1332 >= 0) (l0 (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) (arg2_1332 <= 0)))
         (l1 (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) false)))): unit
ETA: (if (arg2_1332 >= 0) (l0 (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) (arg2_1332 <= 0)))
       (l1 (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) false))): unit
ETA: (l1 (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) false)): unit
ETA: (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) false): unit
ETA: false: x_1:bool[(not x_1) || arg2_1332 <= 0; (not x_1) || 1 <= 0 - arg2_1332 + arg1_1329 && x_1; x_1]
ETA: (fun main_1304 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_1329: int
ETA: arg2_1332: int
ETA_AUX: (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false): unit
ETA: (l0 (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) (arg2_1332 <= 0))): unit
ETA: (k_main_1192 arg2_1332 arg1_1329 (fun main_1304 -> end) (arg2_1332 <= 0)): unit
ETA: (arg2_1332 <= 0): x_1:bool[(not x_1) || arg2_1332 <= 0; (not x_1) || 1 <= 0 - arg2_1332 + arg1_1329 && x_1; x_1]
ETA: (fun main_1304 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_1329: int
ETA: arg2_1332: int
ETA_AUX: (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) (arg2_1332 <= 0)): unit
ETA_AUX: (rand_int
           (fun (arg2_1332:int) ->
            (if (arg1_1329 > 0)
              (l0
                (if (arg2_1332 >= 0)
                  (l0 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) (arg2_1332 <= 0)))
                  (l1 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false))))
              (l1 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false))))): unit
ETA_AUX: (rand_int
           (fun (arg1_1329:int) ->
            (rand_int
              (fun (arg2_1332:int) ->
               (if (arg1_1329 > 0)
                 (l0
                   (if (arg2_1332 >= 0)
                     (l0 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) (arg2_1332 <= 0)))
                     (l1 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false))))
                 (l1 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false))))))): X
ETA: (l0 (k_array_max_1129 m_1015)): X
ETA: (k_array_max_1129 m_1015): X
ETA: m_1015: x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 >= x_1028]
ETA_AUX: (k_array_max_1129 m_1015): X
ETA: (l1
       (x_1039 i_1013
         (fun x_1310 ->
          (if (x_1310 > m_1015) (l0 (array_max_1011 x_1028 (i_1013 + 1) x_1039 x_1310 k_array_max_1129))
            (l1 (array_max_1011 x_1028 (i_1013 + 1) x_1039 m_1015 k_array_max_1129)))))): X
ETA: (x_1039 i_1013
       (fun x_1310 ->
        (if (x_1310 > m_1015) (l0 (array_max_1011 x_1028 (i_1013 + 1) x_1039 x_1310 k_array_max_1129))
          (l1 (array_max_1011 x_1028 (i_1013 + 1) x_1039 m_1015 k_array_max_1129))))): X
ETA: (fun x_1310 ->
      (if (x_1310 > m_1015) (l0 (array_max_1011 x_1028 (i_1013 + 1) x_1039 x_1310 k_array_max_1129))
        (l1 (array_max_1011 x_1028 (i_1013 + 1) x_1039 m_1015 k_array_max_1129)))): (x_1:int[
x_1 >= x_1028] ->
X)
ETA: (if (x_1310 > m_1015) (l0 (array_max_1011 x_1028 (i_1013 + 1) x_1039 x_1310 k_array_max_1129))
       (l1 (array_max_1011 x_1028 (i_1013 + 1) x_1039 m_1015 k_array_max_1129))): X
ETA: (l1 (array_max_1011 x_1028 (i_1013 + 1) x_1039 m_1015 k_array_max_1129)): X
ETA: (array_max_1011 x_1028 (i_1013 + 1) x_1039 m_1015 k_array_max_1129): X
ETA: k_array_max_1129: (x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028] ->
X)
ETA_AUX: k_array_max_1129: (x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028] ->
X)
ETA_AUX: x__1362: x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 >= x_1028]
ETA_AUX: (k_array_max_1129 x__1362): X
ETA: m_1015: int[(1 <= (x_1028 - (1 * (i_1013 + 1)))); i_1013 + 1 <= 0]
ETA: x_1039: (x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X)
ETA_AUX: x_1039: (x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X)
ETA_AUX: x__1363: x_1:int[x_1 <= 0]
ETA_AUX: (x_1039 x__1363): ((x_2:int[x_2 >= x_1028] -> X) ->
X)
ETA_AUX: x__1364: (x_1:int[x_1 >= x_1028] ->
X)
ETA_AUX: x__1365: x_1:int[x_1 >= x_1028]
ETA_AUX: (x__1364 x__1365): X
ETA_AUX: (x_1039 x__1363 (fun (x__1365:x_1:int[x_1 >= x_1028]) -> (x__1364 x__1365))): X
ETA: (i_1013 + 1): int
ETA: x_1028: int
ETA_AUX: (array_max_1011 x_1028 (i_1013 + 1)
           (fun (x__1363:x_1:int[x_1 <= 0]) (x__1364:(x_1:int[x_1 >= x_1028] -> X)) ->
            (x_1039 x__1363 (fun (x__1365:x_1:int[x_1 >= x_1028]) -> (x__1364 x__1365)))) m_1015
           (fun (x__1362:x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__1362))): X
ETA: (l0 (array_max_1011 x_1028 (i_1013 + 1) x_1039 x_1310 k_array_max_1129)): X
ETA: (array_max_1011 x_1028 (i_1013 + 1) x_1039 x_1310 k_array_max_1129): X
ETA: k_array_max_1129: (x_1:int[x_1 >= x_1310; x_1 >= x_1028; i_1013 + 1 >= x_1028] ->
X)
ETA_AUX: k_array_max_1129: (x_1:int[x_1 >= x_1310; x_1 >= x_1028; i_1013 + 1 >= x_1028] ->
X)
ETA_AUX: x__1366: x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 >= x_1028]
ETA_AUX: (k_array_max_1129 x__1366): X
ETA: x_1310: int[(1 <= (x_1028 - (1 * (i_1013 + 1)))); i_1013 + 1 <= 0]
ETA: x_1039: (x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X)
ETA_AUX: x_1039: (x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X)
ETA_AUX: x__1367: x_1:int[x_1 <= 0]
ETA_AUX: (x_1039 x__1367): ((x_2:int[x_2 >= x_1028] -> X) ->
X)
ETA_AUX: x__1368: (x_1:int[x_1 >= x_1028] ->
X)
ETA_AUX: x__1369: x_1:int[x_1 >= x_1028]
ETA_AUX: (x__1368 x__1369): X
ETA_AUX: (x_1039 x__1367 (fun (x__1369:x_1:int[x_1 >= x_1028]) -> (x__1368 x__1369))): X
ETA: (i_1013 + 1): int
ETA: x_1028: int
ETA_AUX: (array_max_1011 x_1028 (i_1013 + 1)
           (fun (x__1367:x_1:int[x_1 <= 0]) (x__1368:(x_1:int[x_1 >= x_1028] -> X)) ->
            (x_1039 x__1367 (fun (x__1369:x_1:int[x_1 >= x_1028]) -> (x__1368 x__1369)))) x_1310
           (fun (x__1366:x_1:int[x_1 >= x_1310; x_1 >= x_1028; i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__1366))): X
ETA: i_1013: x_1:int[x_1 <= 0]
ETA_AUX: (x_1039 i_1013
           (fun (x_1310:x_1:int[x_1 >= x_1028]) ->
            (if (x_1310 > m_1015)
              (l0
                (array_max_1011 x_1028 (i_1013 + 1)
                  (fun (x__1367:x_1:int[x_1 <= 0]) (x__1368:(x_1:int[x_1 >= x_1028] -> X)) ->
                   (x_1039 x__1367 (fun (x__1369:x_1:int[x_1 >= x_1028]) -> (x__1368 x__1369)))) x_1310
                  (fun (x__1366:x_1:int[x_1 >= x_1310; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                   (k_array_max_1129 x__1366))))
              (l1
                (array_max_1011 x_1028 (i_1013 + 1)
                  (fun (x__1363:x_1:int[x_1 <= 0]) (x__1364:(x_1:int[x_1 >= x_1028] -> X)) ->
                   (x_1039 x__1363 (fun (x__1365:x_1:int[x_1 >= x_1028]) -> (x__1364 x__1365)))) m_1015
                  (fun (x__1362:x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                   (k_array_max_1129 x__1362))))))): X
ETA: (k ()): X
ETA: (): unit
ETA_AUX: (k ()): X
ETA: (l0
       (array_max_1011 n_1019 i_1020 (fun i_1345 k_make_array_1346 -> (k_make_array_1346 (n_1019 - i_1345))) -1
         (fun m_1327 -> (if (n_1019 <= m_1327) (l0 (k_main_1185 ())) (l1 (fail_1325 true k_main_1185)))))): X
ETA: (array_max_1011 n_1019 i_1020 (fun i_1345 k_make_array_1346 -> (k_make_array_1346 (n_1019 - i_1345))) -1
       (fun m_1327 -> (if (n_1019 <= m_1327) (l0 (k_main_1185 ())) (l1 (fail_1325 true k_main_1185))))): X
ETA: (fun m_1327 -> (if (n_1019 <= m_1327) (l0 (k_main_1185 ())) (l1 (fail_1325 true k_main_1185)))): (x_1:int[
x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019] ->
X)
ETA: (if (n_1019 <= m_1327) (l0 (k_main_1185 ())) (l1 (fail_1325 true k_main_1185))): X
ETA: (l1 (fail_1325 true k_main_1185)): X
ETA: (fail_1325 true k_main_1185): X
ETA: k_main_1185: (unit ->
X)
ETA_AUX: k_main_1185: (unit ->
X)
ETA_AUX: x__1370: unit
ETA_AUX: (k_main_1185 x__1370): X
ETA: true: x_1:bool[x_1]
ETA_AUX: (fail_1325 true (fun (x__1370:unit) -> (k_main_1185 x__1370))): X
ETA: (l0 (k_main_1185 ())): X
ETA: (k_main_1185 ()): X
ETA: (): unit
ETA_AUX: (k_main_1185 ()): X
ETA: -1: int[1 <= n_1019 - i_1020; i_1020 <= 0]
ETA: (fun i_1345 k_make_array_1346 -> (k_make_array_1346 (n_1019 - i_1345))): (
x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= n_1019] -> X) -> X)
ETA: (fun k_make_array_1346 -> (k_make_array_1346 (n_1019 - i_1345))): ((x_2:int[
x_2 >= n_1019] -> X) ->
X)
ETA: (k_make_array_1346 (n_1019 - i_1345)): X
ETA: (n_1019 - i_1345): x_1:int[x_1 >= n_1019]
ETA_AUX: (k_make_array_1346 (n_1019 - i_1345)): X
ETA: i_1020: int
ETA: n_1019: int
ETA_AUX: (array_max_1011 n_1019 i_1020
           (fun (i_1345:x_1:int[x_1 <= 0]) (k_make_array_1346:(x_1:int[x_1 >= n_1019] -> X)) ->
            (k_make_array_1346 (n_1019 - i_1345))) -1
           (fun (m_1327:x_1:int[x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019]) ->
            (if (n_1019 <= m_1327) (l0 (k_main_1185 ()))
              (l1 (fail_1325 true (fun (x__1370:unit) -> (k_main_1185 x__1370))))))): X
ETA: (l1 (k_main_1185 ())): X
ETA: (k_main_1185 ()): X
ETA: (): unit
ETA_AUX: (k_main_1185 ()): X
ETA_EXPAND:
Main: main_1316
  main_1316 ->
      (rand_int
        (fun (arg1_1329:int) ->
         (rand_int
           (fun (arg2_1332:int) ->
            (if (arg1_1329 > 0)
              (l0
                (if (arg2_1332 >= 0)
                  (l0 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) (arg2_1332 <= 0)))
                  (l1 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false))))
              (l1 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false))))))).
  array_max_1011 x_1028 i_1013 x_1039 m_1015 k_array_max_1129 when (i_1013 >= x_1028) ->
      (l0 (k_array_max_1129 m_1015)).
  array_max_1011 x_1028 i_1013 x_1039 m_1015 k_array_max_1129 when (not (i_1013 >= x_1028)) ->
      (l1
        (x_1039 i_1013
          (fun (x_1310:x_1:int[x_1 >= x_1028]) ->
           (if (x_1310 > m_1015)
             (l0
               (array_max_1011 x_1028 (i_1013 + 1)
                 (fun (x__1367:x_1:int[x_1 <= 0]) (x__1368:(x_1:int[x_1 >= x_1028] -> X)) ->
                  (x_1039 x__1367 (fun (x__1369:x_1:int[x_1 >= x_1028]) -> (x__1368 x__1369)))) x_1310
                 (fun (x__1366:x_1:int[x_1 >= x_1310; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                  (k_array_max_1129 x__1366))))
             (l1
               (array_max_1011 x_1028 (i_1013 + 1)
                 (fun (x__1363:x_1:int[x_1 <= 0]) (x__1364:(x_1:int[x_1 >= x_1028] -> X)) ->
                  (x_1039 x__1363 (fun (x__1365:x_1:int[x_1 >= x_1028]) -> (x__1364 x__1365)))) m_1015
                 (fun (x__1362:x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                  (k_array_max_1129 x__1362)))))))).
  fail_1325 b k -> {fail} => (k ()).
  k_main_1192 i_1020 n_1019 k_main_1185 b_1298 when b_1298 ->
      (l0
        (array_max_1011 n_1019 i_1020
          (fun (i_1345:x_1:int[x_1 <= 0]) (k_make_array_1346:(x_1:int[x_1 >= n_1019] -> X)) ->
           (k_make_array_1346 (n_1019 - i_1345))) -1
          (fun (m_1327:x_1:int[x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019]) ->
           (if (n_1019 <= m_1327) (l0 (k_main_1185 ()))
             (l1 (fail_1325 true (fun (x__1370:unit) -> (k_main_1185 x__1370)))))))).
  k_main_1192 i_1020 n_1019 k_main_1185 b_1298 when (not b_1298) -> (l1 (k_main_1185 ())).

main_1316: ENV:

main_1316: (rand_int
             (fun (arg1_1329:int) ->
              (rand_int
                (fun (arg2_1332:int) ->
                 (if (arg1_1329 > 0)
                   (l0
                     (if (arg2_1332 >= 0)
                       (l0 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) (arg2_1332 <= 0)))
                       (l1 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false))))
                   (l1 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false))))))) ===> (
rand_int
 (fun (arg1_1329:int) ->
  (rand_int
    (fun (arg2_1332:int) ->
     (if (arg1_1329 > 0)
       (l0
         (if (arg2_1332 >= 0) (l0 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) (arg2_1332 <= 0)))
           (l1 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false))))
       (l1 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false)))))))
main_1316:: (rand_int
              (fun (arg1_1329:int) ->
               (rand_int
                 (fun (arg2_1332:int) ->
                  (if (arg1_1329 > 0)
                    (l0
                      (if (arg2_1332 >= 0)
                        (l0 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) (arg2_1332 <= 0)))
                        (l1 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false))))
                    (l1 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false)))))))
abstract_term: (rand_int
                 (fun (arg1_1329:int) ->
                  (rand_int
                    (fun (arg2_1332:int) ->
                     (if (arg1_1329 > 0)
                       (l0
                         (if (arg2_1332 >= 0)
                           (l0 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) (arg2_1332 <= 0)))
                           (l1 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false))))
                       (l1 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false))))))): X
abstract_term: (fun (arg1_1329:int) ->
                (rand_int
                  (fun (arg2_1332:int) ->
                   (if (arg1_1329 > 0)
                     (l0
                       (if (arg2_1332 >= 0)
                         (l0 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) (arg2_1332 <= 0)))
                         (l1 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false))))
                     (l1 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false)))))): (int ->
X)
abst_arg: arg1_1329, int
abst_arg: arg1_1329, int
abstract_term: (rand_int
                 (fun (arg2_1332:int) ->
                  (if (arg1_1329 > 0)
                    (l0
                      (if (arg2_1332 >= 0)
                        (l0 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) (arg2_1332 <= 0)))
                        (l1 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false))))
                    (l1 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false))))): X
abstract_term: (fun (arg2_1332:int) ->
                (if (arg1_1329 > 0)
                  (l0
                    (if (arg2_1332 >= 0)
                      (l0 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) (arg2_1332 <= 0)))
                      (l1 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false))))
                  (l1 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false)))): (int ->
X)
abst_arg: arg2_1332, int
abst_arg: arg2_1332, int
abstract_term: (if (arg1_1329 > 0)
                 (l0
                   (if (arg2_1332 >= 0)
                     (l0 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) (arg2_1332 <= 0)))
                     (l1 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false))))
                 (l1 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false))): X
abstract_term: (arg1_1329 > 0): x_1:bool[x_1]
cond: true
pbs:
p:(arg1_1329 > 0)
tt:false
ff:false

abstract_term: (l0
                 (if (arg2_1332 >= 0)
                   (l0 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) (arg2_1332 <= 0)))
                   (l1 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false)))): X
abstract_term: (if (arg2_1332 >= 0)
                 (l0 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) (arg2_1332 <= 0)))
                 (l1 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false))): X
abstract_term: (arg2_1332 >= 0): x_1:bool[x_1]
cond: (arg1_1329 > 0); true
pbs:
p:(arg2_1332 >= 0)
tt:false
ff:false

abstract_term: (l0 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) (arg2_1332 <= 0))): X
abstract_term: (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) (arg2_1332 <= 0)): X
abstract_term: (arg2_1332 <= 0): x_1:bool[(not x_1) || arg2_1332 <= 0;
                                          (not x_1) || 1 <= 0 - arg2_1332 + arg1_1329 && x_1; x_1]
cond: (arg2_1332 >= 0); (arg1_1329 > 0); true
pbs:
p:((not (arg2_1332 <= 0)) || (arg2_1332 <= 0))
tt:true
ff:false

cond: (arg2_1332 >= 0); (arg1_1329 > 0); true
pbs:
p:((not (arg2_1332 <= 0)) || ((1 <= ((0 - arg2_1332) + arg1_1329)) && (arg2_1332 <= 0)))
tt:true
ff:false

cond: (arg2_1332 >= 0); (arg1_1329 > 0); true
pbs:
p:(arg2_1332 <= 0)
tt:false
ff:false

abstract_term: (fun (main_1304:unit) -> end): (unit ->
X)
abst_arg: main_1304, unit
abst_arg: main_1304, unit
abstract_term: end: X
abstract_term: arg1_1329: int
abstract_term: arg2_1332: int
abstract_term: (l1 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false)): X
abstract_term: (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false): X
abstract_term: false: x_1:bool[(not x_1) || arg2_1332 <= 0; (not x_1) || 1 <= 0 - arg2_1332 + arg1_1329 && x_1; x_1]
cond: (not (arg2_1332 >= 0)); (arg1_1329 > 0); true
pbs:
p:((not false) || (arg2_1332 <= 0))
tt:true
ff:false

cond: (not (arg2_1332 >= 0)); (arg1_1329 > 0); true
pbs:
p:((not false) || ((1 <= ((0 - arg2_1332) + arg1_1329)) && false))
tt:true
ff:false

cond: (not (arg2_1332 >= 0)); (arg1_1329 > 0); true
pbs:
p:false
tt:false
ff:true

abstract_term: (fun (main_1304:unit) -> end): (unit ->
X)
abst_arg: main_1304, unit
abst_arg: main_1304, unit
abstract_term: end: X
abstract_term: arg1_1329: int
abstract_term: arg2_1332: int
abstract_term: (l1 (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false)): X
abstract_term: (k_main_1192 arg2_1332 arg1_1329 (fun (main_1304:unit) -> end) false): X
abstract_term: false: x_1:bool[(not x_1) || arg2_1332 <= 0; (not x_1) || 1 <= 0 - arg2_1332 + arg1_1329 && x_1; x_1]
cond: (not (arg1_1329 > 0)); true
pbs:
p:((not false) || (arg2_1332 <= 0))
tt:true
ff:false

cond: (not (arg1_1329 > 0)); true
pbs:
p:((not false) || ((1 <= ((0 - arg2_1332) + arg1_1329)) && false))
tt:true
ff:false

cond: (not (arg1_1329 > 0)); true
pbs:
p:false
tt:false
ff:true

abstract_term: (fun (main_1304:unit) -> end): (unit ->
X)
abst_arg: main_1304, unit
abst_arg: main_1304, unit
abstract_term: end: X
abstract_term: arg1_1329: int
abstract_term: arg2_1332: int
array_max_1011: ENV: x_1028:int, i_1013:int, x_1039:(x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X),
m_1015:int[1 <= x_1028 - i_1013; i_1013 <= 0],
k_array_max_1129:(x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 >= x_1028] -> X),


abst_arg: x_1028, int
abst_arg: i_1013, int
abst_arg: x_1039, (x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X)
abst_arg: m_1015, int[1 <= x_1028 - i_1013; i_1013 <= 0]
abst_arg: k_array_max_1129, (x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 >= x_1028] ->
X)
abst_arg: x_1028, int
abst_arg: i_1013, int
abst_arg: x_1039, (x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X)
abst_arg: m_1015, int[1 <= x_1028 - i_1013; i_1013 <= 0]
abst_arg: k_array_max_1129, (x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 >= x_1028] ->
X)
array_max_1011: (l0 (k_array_max_1129 m_1015)) ===> (l0 (k_array_max_1129 m_1015))
array_max_1011:: (l0 (k_array_max_1129 m_1015))
abstract_term: (l0 (k_array_max_1129 m_1015)): X
abstract_term: (k_array_max_1129 m_1015): X
abstract_term: m_1015: x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 >= x_1028]
cond: (i_1013 >= x_1028)
pbs: m2_1015 := (i_1013 <= 0)
p:(m_1015 >= m_1015)
tt:true
ff:false

cond: (i_1013 >= x_1028)
pbs: m2_1015 := (i_1013 <= 0)
p:(m_1015 >= x_1028)
tt:false
ff:false

cond: (i_1013 >= x_1028)
pbs: m2_1015 := (i_1013 <= 0)
p:(i_1013 >= x_1028)
tt:true
ff:false

array_max_1011: ENV: x_1028:int, i_1013:int, x_1039:(x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X),
m_1015:int[1 <= x_1028 - i_1013; i_1013 <= 0],
k_array_max_1129:(x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 >= x_1028] -> X),


abst_arg: x_1028, int
abst_arg: i_1013, int
abst_arg: x_1039, (x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X)
abst_arg: m_1015, int[1 <= x_1028 - i_1013; i_1013 <= 0]
abst_arg: k_array_max_1129, (x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 >= x_1028] ->
X)
abst_arg: x_1028, int
abst_arg: i_1013, int
abst_arg: x_1039, (x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X)
abst_arg: m_1015, int[1 <= x_1028 - i_1013; i_1013 <= 0]
abst_arg: k_array_max_1129, (x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 >= x_1028] ->
X)
array_max_1011: (l1
                  (x_1039 i_1013
                    (fun (x_1310:x_1:int[x_1 >= x_1028]) ->
                     (if (x_1310 > m_1015)
                       (l0
                         (array_max_1011 x_1028 (i_1013 + 1)
                           (fun (x__1367:x_1:int[x_1 <= 0]) (x__1368:(x_1:int[x_1 >= x_1028] -> X)) ->
                            (x_1039 x__1367 (fun (x__1369:x_1:int[x_1 >= x_1028]) -> (x__1368 x__1369)))) x_1310
                           (fun (x__1366:x_1:int[x_1 >= x_1310; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                            (k_array_max_1129 x__1366))))
                       (l1
                         (array_max_1011 x_1028 (i_1013 + 1)
                           (fun (x__1363:x_1:int[x_1 <= 0]) (x__1364:(x_1:int[x_1 >= x_1028] -> X)) ->
                            (x_1039 x__1363 (fun (x__1365:x_1:int[x_1 >= x_1028]) -> (x__1364 x__1365)))) m_1015
                           (fun (x__1362:x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                            (k_array_max_1129 x__1362)))))))) ===> (l1
                                                                    (x_1039 i_1013
                                                                    (fun
                                                                    (x_1310:x_1:int[x_1 >= x_1028]) ->
                                                                    (if (
                                                                    x_1310 > m_1015)
                                                                    (l0
                                                                    (array_max_1011 x_1028 (
                                                                    i_1013 + 1)
                                                                    (fun
                                                                    (x__1367:x_1:int[
                                                                    x_1 <= 0]) (x__1368:(x_1:int[
                                                                    x_1 >= x_1028] -> X)) ->
                                                                    (x_1039 x__1367
                                                                    (fun
                                                                    (x__1369:x_1:int[x_1 >= x_1028]) ->
                                                                    (x__1368 x__1369)))) x_1310
                                                                    (fun
                                                                    (x__1366:x_1:int[
                                                                    x_1 >= x_1310; x_1 >= x_1028; i_1013 + 1 >= x_1028])
                                                                    -> (
                                                                    k_array_max_1129 x__1366))))
                                                                    (l1
                                                                    (array_max_1011 x_1028 (
                                                                    i_1013 + 1)
                                                                    (fun
                                                                    (x__1363:x_1:int[
                                                                    x_1 <= 0]) (x__1364:(x_1:int[
                                                                    x_1 >= x_1028] -> X)) ->
                                                                    (x_1039 x__1363
                                                                    (fun
                                                                    (x__1365:x_1:int[x_1 >= x_1028]) ->
                                                                    (x__1364 x__1365)))) m_1015
                                                                    (fun
                                                                    (x__1362:x_1:int[
                                                                    x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028])
                                                                    -> (
                                                                    k_array_max_1129 x__1362))))))))
array_max_1011:: (l1
                   (x_1039 i_1013
                     (fun (x_1310:x_1:int[x_1 >= x_1028]) ->
                      (if (x_1310 > m_1015)
                        (l0
                          (array_max_1011 x_1028 (i_1013 + 1)
                            (fun (x__1367:x_1:int[x_1 <= 0]) (x__1368:(x_1:int[x_1 >= x_1028] -> X)) ->
                             (x_1039 x__1367 (fun (x__1369:x_1:int[x_1 >= x_1028]) -> (x__1368 x__1369)))) x_1310
                            (fun (x__1366:x_1:int[x_1 >= x_1310; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                             (k_array_max_1129 x__1366))))
                        (l1
                          (array_max_1011 x_1028 (i_1013 + 1)
                            (fun (x__1363:x_1:int[x_1 <= 0]) (x__1364:(x_1:int[x_1 >= x_1028] -> X)) ->
                             (x_1039 x__1363 (fun (x__1365:x_1:int[x_1 >= x_1028]) -> (x__1364 x__1365)))) m_1015
                            (fun (x__1362:x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                             (k_array_max_1129 x__1362))))))))
abstract_term: (l1
                 (x_1039 i_1013
                   (fun (x_1310:x_1:int[x_1 >= x_1028]) ->
                    (if (x_1310 > m_1015)
                      (l0
                        (array_max_1011 x_1028 (i_1013 + 1)
                          (fun (x__1367:x_1:int[x_1 <= 0]) (x__1368:(x_1:int[x_1 >= x_1028] -> X)) ->
                           (x_1039 x__1367 (fun (x__1369:x_1:int[x_1 >= x_1028]) -> (x__1368 x__1369)))) x_1310
                          (fun (x__1366:x_1:int[x_1 >= x_1310; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                           (k_array_max_1129 x__1366))))
                      (l1
                        (array_max_1011 x_1028 (i_1013 + 1)
                          (fun (x__1363:x_1:int[x_1 <= 0]) (x__1364:(x_1:int[x_1 >= x_1028] -> X)) ->
                           (x_1039 x__1363 (fun (x__1365:x_1:int[x_1 >= x_1028]) -> (x__1364 x__1365)))) m_1015
                          (fun (x__1362:x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                           (k_array_max_1129 x__1362)))))))): X
abstract_term: (x_1039 i_1013
                 (fun (x_1310:x_1:int[x_1 >= x_1028]) ->
                  (if (x_1310 > m_1015)
                    (l0
                      (array_max_1011 x_1028 (i_1013 + 1)
                        (fun (x__1367:x_1:int[x_1 <= 0]) (x__1368:(x_1:int[x_1 >= x_1028] -> X)) ->
                         (x_1039 x__1367 (fun (x__1369:x_1:int[x_1 >= x_1028]) -> (x__1368 x__1369)))) x_1310
                        (fun (x__1366:x_1:int[x_1 >= x_1310; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                         (k_array_max_1129 x__1366))))
                    (l1
                      (array_max_1011 x_1028 (i_1013 + 1)
                        (fun (x__1363:x_1:int[x_1 <= 0]) (x__1364:(x_1:int[x_1 >= x_1028] -> X)) ->
                         (x_1039 x__1363 (fun (x__1365:x_1:int[x_1 >= x_1028]) -> (x__1364 x__1365)))) m_1015
                        (fun (x__1362:x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                         (k_array_max_1129 x__1362))))))): X
abstract_term: (fun (x_1310:x_1:int[x_1 >= x_1028]) ->
                (if (x_1310 > m_1015)
                  (l0
                    (array_max_1011 x_1028 (i_1013 + 1)
                      (fun (x__1367:x_1:int[x_1 <= 0]) (x__1368:(x_1:int[x_1 >= x_1028] -> X)) ->
                       (x_1039 x__1367 (fun (x__1369:x_1:int[x_1 >= x_1028]) -> (x__1368 x__1369)))) x_1310
                      (fun (x__1366:x_1:int[x_1 >= x_1310; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                       (k_array_max_1129 x__1366))))
                  (l1
                    (array_max_1011 x_1028 (i_1013 + 1)
                      (fun (x__1363:x_1:int[x_1 <= 0]) (x__1364:(x_1:int[x_1 >= x_1028] -> X)) ->
                       (x_1039 x__1363 (fun (x__1365:x_1:int[x_1 >= x_1028]) -> (x__1364 x__1365)))) m_1015
                      (fun (x__1362:x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                       (k_array_max_1129 x__1362)))))): (x_1:int[x_1 >= x_1028] ->
X)
abst_arg: x_1310, x_1:int[x_1 >= x_1028]
abst_arg: x_1310, x_1:int[x_1 >= x_1028]
abstract_term: (if (x_1310 > m_1015)
                 (l0
                   (array_max_1011 x_1028 (i_1013 + 1)
                     (fun (x__1367:x_1:int[x_1 <= 0]) (x__1368:(x_1:int[x_1 >= x_1028] -> X)) ->
                      (x_1039 x__1367 (fun (x__1369:x_1:int[x_1 >= x_1028]) -> (x__1368 x__1369)))) x_1310
                     (fun (x__1366:x_1:int[x_1 >= x_1310; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                      (k_array_max_1129 x__1366))))
                 (l1
                   (array_max_1011 x_1028 (i_1013 + 1)
                     (fun (x__1363:x_1:int[x_1 <= 0]) (x__1364:(x_1:int[x_1 >= x_1028] -> X)) ->
                      (x_1039 x__1363 (fun (x__1365:x_1:int[x_1 >= x_1028]) -> (x__1364 x__1365)))) m_1015
                     (fun (x__1362:x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                      (k_array_max_1129 x__1362))))): X
abstract_term: (x_1310 > m_1015): x_1:bool[x_1]
cond: (not (i_1013 >= x_1028))
pbs: x_1310 := (x_1310 >= x_1028);
     m1_1015 := (1 <= (x_1028 - (1 * i_1013)));
     m2_1015 := (i_1013 <= 0)
p:(x_1310 > m_1015)
tt:false
ff:false

abstract_term: (l0
                 (array_max_1011 x_1028 (i_1013 + 1)
                   (fun (x__1367:x_1:int[x_1 <= 0]) (x__1368:(x_1:int[x_1 >= x_1028] -> X)) ->
                    (x_1039 x__1367 (fun (x__1369:x_1:int[x_1 >= x_1028]) -> (x__1368 x__1369)))) x_1310
                   (fun (x__1366:x_1:int[x_1 >= x_1310; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                    (k_array_max_1129 x__1366)))): X
abstract_term: (array_max_1011 x_1028 (i_1013 + 1)
                 (fun (x__1367:x_1:int[x_1 <= 0]) (x__1368:(x_1:int[x_1 >= x_1028] -> X)) ->
                  (x_1039 x__1367 (fun (x__1369:x_1:int[x_1 >= x_1028]) -> (x__1368 x__1369)))) x_1310
                 (fun (x__1366:x_1:int[x_1 >= x_1310; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                  (k_array_max_1129 x__1366))): X
abstract_term: (fun (x__1366:x_1:int[x_1 >= x_1310; x_1 >= x_1028; i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__1366)): (x_1:int[
x_1 >= x_1310; x_1 >= x_1028; i_1013 + 1 >= x_1028] ->
X)
abst_arg: x__1366, x_1:int[x_1 >= x_1310; x_1 >= x_1028; i_1013 + 1 >= x_1028]
abst_arg: x__1366, x_1:int[x_1 >= x_1310; x_1 >= x_1028; i_1013 + 1 >= x_1028]
abstract_term: (k_array_max_1129 x__1366): X
abstract_term: x__1366: x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 >= x_1028]
cond: (x_1310 > m_1015); (not (i_1013 >= x_1028))
pbs: x_3_1366 := ((i_1013 + 1) >= x_1028);
     x_2_1366 := (x__1366 >= x_1028);
     x_1_1366 := (x__1366 >= x_1310);
     x_1310 := (x_1310 >= x_1028);
     m1_1015 := (1 <= (x_1028 - (1 * i_1013)));
     m2_1015 := (i_1013 <= 0)
p:(x__1366 >= m_1015)
tt:x_1_1366
ff:false

cond: (x_1310 > m_1015); (not (i_1013 >= x_1028))
pbs: x_3_1366 := ((i_1013 + 1) >= x_1028);
     x_2_1366 := (x__1366 >= x_1028);
     x_1_1366 := (x__1366 >= x_1310);
     x_1310 := (x_1310 >= x_1028);
     m1_1015 := (1 <= (x_1028 - (1 * i_1013)));
     m2_1015 := (i_1013 <= 0)
p:(x__1366 >= x_1028)
tt:(x_2_1366 || (x_1_1366 && x_1310))
ff:false

cond: (x_1310 > m_1015); (not (i_1013 >= x_1028))
pbs: x_3_1366 := ((i_1013 + 1) >= x_1028);
     x_2_1366 := (x__1366 >= x_1028);
     x_1_1366 := (x__1366 >= x_1310);
     x_1310 := (x_1310 >= x_1028);
     m1_1015 := (1 <= (x_1028 - (1 * i_1013)));
     m2_1015 := (i_1013 <= 0)
p:(i_1013 >= x_1028)
tt:false
ff:true

abstract_term: x_1310: int[(1 <= (x_1028 - (1 * (i_1013 + 1)))); i_1013 + 1 <= 0]
cond: (x_1310 > m_1015); (not (i_1013 >= x_1028))
pbs: x_1310 := (x_1310 >= x_1028);
     m1_1015 := (1 <= (x_1028 - (1 * i_1013)));
     m2_1015 := (i_1013 <= 0)
p:(1 <= (x_1028 - (1 * (i_1013 + 1))))
tt:false
ff:false

cond: (x_1310 > m_1015); (not (i_1013 >= x_1028))
pbs: x_1310 := (x_1310 >= x_1028);
     m1_1015 := (1 <= (x_1028 - (1 * i_1013)));
     m2_1015 := (i_1013 <= 0)
p:((i_1013 + 1) <= 0)
tt:false
ff:false

abstract_term: (fun (x__1367:x_1:int[x_1 <= 0]) (x__1368:(x_1:int[x_1 >= x_1028] -> X)) ->
                (x_1039 x__1367 (fun (x__1369:x_1:int[x_1 >= x_1028]) -> (x__1368 x__1369)))): (
x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X)
abst_arg: x__1367, x_1:int[x_1 <= 0]
abst_arg: x__1368, (x_1:int[x_1 >= x_1028] ->
X)
abst_arg: x__1367, x_1:int[x_1 <= 0]
abst_arg: x__1368, (x_1:int[x_1 >= x_1028] ->
X)
abstract_term: (x_1039 x__1367 (fun (x__1369:x_1:int[x_1 >= x_1028]) -> (x__1368 x__1369))): X
abstract_term: (fun (x__1369:x_1:int[x_1 >= x_1028]) -> (x__1368 x__1369)): (x_1:int[
x_1 >= x_1028] ->
X)
abst_arg: x__1369, x_1:int[x_1 >= x_1028]
abst_arg: x__1369, x_1:int[x_1 >= x_1028]
abstract_term: (x__1368 x__1369): X
abstract_term: x__1369: x_1:int[x_1 >= x_1028]
cond: (x_1310 > m_1015); (not (i_1013 >= x_1028))
pbs: x__1369 := (x__1369 >= x_1028);
     x__1367 := (x__1367 <= 0);
     x_1310 := (x_1310 >= x_1028);
     m1_1015 := (1 <= (x_1028 - (1 * i_1013)));
     m2_1015 := (i_1013 <= 0)
p:(x__1369 >= x_1028)
tt:x__1369
ff:false

abstract_term: x__1367: x_1:int[x_1 <= 0]
cond: (x_1310 > m_1015); (not (i_1013 >= x_1028))
pbs: x__1367 := (x__1367 <= 0);
     x_1310 := (x_1310 >= x_1028);
     m1_1015 := (1 <= (x_1028 - (1 * i_1013)));
     m2_1015 := (i_1013 <= 0)
p:(x__1367 <= 0)
tt:x__1367
ff:false

abstract_term: (i_1013 + 1): int
abstract_term: x_1028: int
abstract_term: (l1
                 (array_max_1011 x_1028 (i_1013 + 1)
                   (fun (x__1363:x_1:int[x_1 <= 0]) (x__1364:(x_1:int[x_1 >= x_1028] -> X)) ->
                    (x_1039 x__1363 (fun (x__1365:x_1:int[x_1 >= x_1028]) -> (x__1364 x__1365)))) m_1015
                   (fun (x__1362:x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                    (k_array_max_1129 x__1362)))): X
abstract_term: (array_max_1011 x_1028 (i_1013 + 1)
                 (fun (x__1363:x_1:int[x_1 <= 0]) (x__1364:(x_1:int[x_1 >= x_1028] -> X)) ->
                  (x_1039 x__1363 (fun (x__1365:x_1:int[x_1 >= x_1028]) -> (x__1364 x__1365)))) m_1015
                 (fun (x__1362:x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]) ->
                  (k_array_max_1129 x__1362))): X
abstract_term: (fun (x__1362:x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]) -> (k_array_max_1129 x__1362)): (x_1:int[
x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028] ->
X)
abst_arg: x__1362, x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]
abst_arg: x__1362, x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 + 1 >= x_1028]
abstract_term: (k_array_max_1129 x__1362): X
abstract_term: x__1362: x_1:int[x_1 >= m_1015; x_1 >= x_1028; i_1013 >= x_1028]
cond: (not (x_1310 > m_1015)); (not (i_1013 >= x_1028))
pbs: x_3_1362 := ((i_1013 + 1) >= x_1028);
     x_2_1362 := (x__1362 >= x_1028);
     x_1_1362 := (x__1362 >= m_1015);
     x_1310 := (x_1310 >= x_1028);
     m1_1015 := (1 <= (x_1028 - (1 * i_1013)));
     m2_1015 := (i_1013 <= 0)
p:(x__1362 >= m_1015)
tt:x_1_1362
ff:false

cond: (not (x_1310 > m_1015)); (not (i_1013 >= x_1028))
pbs: x_3_1362 := ((i_1013 + 1) >= x_1028);
     x_2_1362 := (x__1362 >= x_1028);
     x_1_1362 := (x__1362 >= m_1015);
     x_1310 := (x_1310 >= x_1028);
     m1_1015 := (1 <= (x_1028 - (1 * i_1013)));
     m2_1015 := (i_1013 <= 0)
p:(x__1362 >= x_1028)
tt:(x_2_1362 || (x_1_1362 && x_1310))
ff:false

cond: (not (x_1310 > m_1015)); (not (i_1013 >= x_1028))
pbs: x_3_1362 := ((i_1013 + 1) >= x_1028);
     x_2_1362 := (x__1362 >= x_1028);
     x_1_1362 := (x__1362 >= m_1015);
     x_1310 := (x_1310 >= x_1028);
     m1_1015 := (1 <= (x_1028 - (1 * i_1013)));
     m2_1015 := (i_1013 <= 0)
p:(i_1013 >= x_1028)
tt:false
ff:true

abstract_term: m_1015: int[(1 <= (x_1028 - (1 * (i_1013 + 1)))); i_1013 + 1 <= 0]
cond: (not (x_1310 > m_1015)); (not (i_1013 >= x_1028))
pbs: x_1310 := (x_1310 >= x_1028);
     m1_1015 := (1 <= (x_1028 - (1 * i_1013)));
     m2_1015 := (i_1013 <= 0)
p:(1 <= (x_1028 - (1 * (i_1013 + 1))))
tt:false
ff:false

cond: (not (x_1310 > m_1015)); (not (i_1013 >= x_1028))
pbs: x_1310 := (x_1310 >= x_1028);
     m1_1015 := (1 <= (x_1028 - (1 * i_1013)));
     m2_1015 := (i_1013 <= 0)
p:((i_1013 + 1) <= 0)
tt:false
ff:false

abstract_term: (fun (x__1363:x_1:int[x_1 <= 0]) (x__1364:(x_1:int[x_1 >= x_1028] -> X)) ->
                (x_1039 x__1363 (fun (x__1365:x_1:int[x_1 >= x_1028]) -> (x__1364 x__1365)))): (
x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= x_1028] -> X) -> X)
abst_arg: x__1363, x_1:int[x_1 <= 0]
abst_arg: x__1364, (x_1:int[x_1 >= x_1028] ->
X)
abst_arg: x__1363, x_1:int[x_1 <= 0]
abst_arg: x__1364, (x_1:int[x_1 >= x_1028] ->
X)
abstract_term: (x_1039 x__1363 (fun (x__1365:x_1:int[x_1 >= x_1028]) -> (x__1364 x__1365))): X
abstract_term: (fun (x__1365:x_1:int[x_1 >= x_1028]) -> (x__1364 x__1365)): (x_1:int[
x_1 >= x_1028] ->
X)
abst_arg: x__1365, x_1:int[x_1 >= x_1028]
abst_arg: x__1365, x_1:int[x_1 >= x_1028]
abstract_term: (x__1364 x__1365): X
abstract_term: x__1365: x_1:int[x_1 >= x_1028]
cond: (not (x_1310 > m_1015)); (not (i_1013 >= x_1028))
pbs: x__1365 := (x__1365 >= x_1028);
     x__1363 := (x__1363 <= 0);
     x_1310 := (x_1310 >= x_1028);
     m1_1015 := (1 <= (x_1028 - (1 * i_1013)));
     m2_1015 := (i_1013 <= 0)
p:(x__1365 >= x_1028)
tt:x__1365
ff:false

abstract_term: x__1363: x_1:int[x_1 <= 0]
cond: (not (x_1310 > m_1015)); (not (i_1013 >= x_1028))
pbs: x__1363 := (x__1363 <= 0);
     x_1310 := (x_1310 >= x_1028);
     m1_1015 := (1 <= (x_1028 - (1 * i_1013)));
     m2_1015 := (i_1013 <= 0)
p:(x__1363 <= 0)
tt:x__1363
ff:false

abstract_term: (i_1013 + 1): int
abstract_term: x_1028: int
abstract_term: i_1013: x_1:int[x_1 <= 0]
cond: (not (i_1013 >= x_1028))
pbs: m1_1015 := (1 <= (x_1028 - (1 * i_1013)));
     m2_1015 := (i_1013 <= 0)
p:(i_1013 <= 0)
tt:m2_1015
ff:false

fail_1325: ENV: b:x_1:bool[x_1], k:(unit -> X),


abst_arg: b, x_1:bool[x_1]
abst_arg: k, (unit ->
X)
abst_arg: b, x_1:bool[x_1]
abst_arg: k, (unit ->
X)
fail_1325: (k ()) ===> (k ())
fail_1325:: (k ())
abstract_term: (k ()): X
abstract_term: (): unit
k_main_1192: ENV: i_1020:int, n_1019:int, k_main_1185:(unit -> X),
b_1298:x_1:bool[(not x_1) || i_1020 <= 0; (not x_1) || 1 <= 0 - i_1020 + n_1019 && x_1; x_1],


abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1185, (unit ->
X)
abst_arg: b_1298, x_1:bool[(not x_1) || i_1020 <= 0; (not x_1) || 1 <= 0 - i_1020 + n_1019 && x_1; x_1]
abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1185, (unit ->
X)
abst_arg: b_1298, x_1:bool[(not x_1) || i_1020 <= 0; (not x_1) || 1 <= 0 - i_1020 + n_1019 && x_1; x_1]
k_main_1192: (l0
               (array_max_1011 n_1019 i_1020
                 (fun (i_1345:x_1:int[x_1 <= 0]) (k_make_array_1346:(x_1:int[x_1 >= n_1019] -> X)) ->
                  (k_make_array_1346 (n_1019 - i_1345))) -1
                 (fun (m_1327:x_1:int[x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019]) ->
                  (if (n_1019 <= m_1327) (l0 (k_main_1185 ()))
                    (l1 (fail_1325 true (fun (x__1370:unit) -> (k_main_1185 x__1370)))))))) ===> (
l0
 (array_max_1011 n_1019 i_1020
   (fun (i_1345:x_1:int[x_1 <= 0]) (k_make_array_1346:(x_1:int[x_1 >= n_1019] -> X)) ->
    (k_make_array_1346 (n_1019 - i_1345))) -1
   (fun (m_1327:x_1:int[x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019]) ->
    (if (n_1019 <= m_1327) (l0 (k_main_1185 ())) (l1 (fail_1325 true (fun (x__1370:unit) -> (k_main_1185 x__1370))))))))
k_main_1192:: (l0
                (array_max_1011 n_1019 i_1020
                  (fun (i_1345:x_1:int[x_1 <= 0]) (k_make_array_1346:(x_1:int[x_1 >= n_1019] -> X)) ->
                   (k_make_array_1346 (n_1019 - i_1345))) -1
                  (fun (m_1327:x_1:int[x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019]) ->
                   (if (n_1019 <= m_1327) (l0 (k_main_1185 ()))
                     (l1 (fail_1325 true (fun (x__1370:unit) -> (k_main_1185 x__1370))))))))
abstract_term: (l0
                 (array_max_1011 n_1019 i_1020
                   (fun (i_1345:x_1:int[x_1 <= 0]) (k_make_array_1346:(x_1:int[x_1 >= n_1019] -> X)) ->
                    (k_make_array_1346 (n_1019 - i_1345))) -1
                   (fun (m_1327:x_1:int[x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019]) ->
                    (if (n_1019 <= m_1327) (l0 (k_main_1185 ()))
                      (l1 (fail_1325 true (fun (x__1370:unit) -> (k_main_1185 x__1370)))))))): X
abstract_term: (array_max_1011 n_1019 i_1020
                 (fun (i_1345:x_1:int[x_1 <= 0]) (k_make_array_1346:(x_1:int[x_1 >= n_1019] -> X)) ->
                  (k_make_array_1346 (n_1019 - i_1345))) -1
                 (fun (m_1327:x_1:int[x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019]) ->
                  (if (n_1019 <= m_1327) (l0 (k_main_1185 ()))
                    (l1 (fail_1325 true (fun (x__1370:unit) -> (k_main_1185 x__1370))))))): X
abstract_term: (fun (m_1327:x_1:int[x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019]) ->
                (if (n_1019 <= m_1327) (l0 (k_main_1185 ()))
                  (l1 (fail_1325 true (fun (x__1370:unit) -> (k_main_1185 x__1370)))))): (x_1:int[
x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019] ->
X)
abst_arg: m_1327, x_1:int[x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019]
abst_arg: m_1327, x_1:int[x_1 >= -1; x_1 >= n_1019; i_1020 >= n_1019]
abstract_term: (if (n_1019 <= m_1327) (l0 (k_main_1185 ()))
                 (l1 (fail_1325 true (fun (x__1370:unit) -> (k_main_1185 x__1370))))): X
abstract_term: (n_1019 <= m_1327): x_1:bool[x_1]
cond: b_1298
pbs: m3_1327 := (i_1020 >= n_1019);
     m2_1327 := (m_1327 >= n_1019);
     m1_1327 := (m_1327 >= -1);
     b1_1298 := ((not b_1298) || (i_1020 <= 0));
     b2_1298 := ((not b_1298) || ((1 <= ((0 - i_1020) + n_1019)) && b_1298));
     b3_1298 := b_1298
p:(n_1019 <= m_1327)
tt:m2_1327
ff:false

abstract_term: (l0 (k_main_1185 ())): X
abstract_term: (k_main_1185 ()): X
abstract_term: (): unit
abstract_term: (l1 (fail_1325 true (fun (x__1370:unit) -> (k_main_1185 x__1370)))): X
abstract_term: (fail_1325 true (fun (x__1370:unit) -> (k_main_1185 x__1370))): X
abstract_term: (fun (x__1370:unit) -> (k_main_1185 x__1370)): (unit ->
X)
abst_arg: x__1370, unit
abst_arg: x__1370, unit
abstract_term: (k_main_1185 x__1370): X
abstract_term: x__1370: unit
abstract_term: true: x_1:bool[x_1]
cond: (not (n_1019 <= m_1327)); b_1298
pbs: m3_1327 := (i_1020 >= n_1019);
     m1_1327 := (m_1327 >= -1);
     b1_1298 := ((not b_1298) || (i_1020 <= 0));
     b2_1298 := ((not b_1298) || ((1 <= ((0 - i_1020) + n_1019)) && b_1298));
     b3_1298 := b_1298
p:true
tt:true
ff:false

abstract_term: -1: int[1 <= n_1019 - i_1020; i_1020 <= 0]
cond: b_1298
pbs: b1_1298 := ((not b_1298) || (i_1020 <= 0));
     b2_1298 := ((not b_1298) || ((1 <= ((0 - i_1020) + n_1019)) && b_1298));
     b3_1298 := b_1298
p:(1 <= (n_1019 - (1 * i_1020)))
tt:b2_1298
ff:false

cond: b_1298
pbs: b1_1298 := ((not b_1298) || (i_1020 <= 0));
     b2_1298 := ((not b_1298) || ((1 <= ((0 - i_1020) + n_1019)) && b_1298));
     b3_1298 := b_1298
p:(i_1020 <= 0)
tt:b1_1298
ff:false

abstract_term: (fun (i_1345:x_1:int[x_1 <= 0]) (k_make_array_1346:(x_1:int[x_1 >= n_1019] -> X)) ->
                (k_make_array_1346 (n_1019 - i_1345))): (x_1:int[x_1 <= 0] -> (x_3:int[x_3 >= n_1019] -> X) -> X)
abst_arg: i_1345, x_1:int[x_1 <= 0]
abst_arg: k_make_array_1346, (x_1:int[x_1 >= n_1019] ->
X)
abst_arg: i_1345, x_1:int[x_1 <= 0]
abst_arg: k_make_array_1346, (x_1:int[x_1 >= n_1019] ->
X)
abstract_term: (k_make_array_1346 (n_1019 - i_1345)): X
abstract_term: (n_1019 - i_1345): x_1:int[x_1 >= n_1019]
cond: b_1298
pbs: i_1345 := (i_1345 <= 0);
     b1_1298 := ((not b_1298) || (i_1020 <= 0));
     b2_1298 := ((not b_1298) || ((1 <= ((0 - i_1020) + n_1019)) && b_1298));
     b3_1298 := b_1298
p:((n_1019 - i_1345) >= n_1019)
tt:i_1345
ff:false

abstract_term: i_1020: int
abstract_term: n_1019: int
k_main_1192: ENV: i_1020:int, n_1019:int, k_main_1185:(unit -> X),
b_1298:x_1:bool[(not x_1) || i_1020 <= 0; (not x_1) || 1 <= 0 - i_1020 + n_1019 && x_1; x_1],


abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1185, (unit ->
X)
abst_arg: b_1298, x_1:bool[(not x_1) || i_1020 <= 0; (not x_1) || 1 <= 0 - i_1020 + n_1019 && x_1; x_1]
abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1185, (unit ->
X)
abst_arg: b_1298, x_1:bool[(not x_1) || i_1020 <= 0; (not x_1) || 1 <= 0 - i_1020 + n_1019 && x_1; x_1]
k_main_1192: (l1 (k_main_1185 ())) ===> (l1 (k_main_1185 ()))
k_main_1192:: (l1 (k_main_1185 ()))
abstract_term: (l1 (k_main_1185 ())): X
abstract_term: (k_main_1185 ()): X
abstract_term: (): unit
ABST:
Main: main_1316
  main_1316 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1192 () true true rand_bool)) (l1 (k_main_1192 () true true false))))
        (l1 (k_main_1192 () true true false))).
  array_max_1011 x_1039 m1_1015 m2_1015 k_array_max_1129 ->
      (if m1_1015 _|_ (l0 (k_array_max_1129 true rand_bool true))).
  array_max_1011 x_1039 m1_1015 m2_1015 k_array_max_1129 ->
      (l1
        (x_1039 (if m2_1015 true rand_bool)
          (fun x_1310 ->
           (if rand_bool
             (l0
               (array_max_1011
                 (fun x__1367 x__1368 ->
                  (x_1039 (if x__1367 true rand_bool) (fun x__1369 -> (x__1368 (if x__1369 true rand_bool)))))
                 rand_bool rand_bool
                 (fun x_1_1366 x_2_1366 x_3_1366 ->
                  (k_array_max_1129 (if x_1_1366 true rand_bool) (if (x_2_1366 || (x_1_1366 && x_1310)) true rand_bool)
                    false))))
             (l1
               (array_max_1011
                 (fun x__1363 x__1364 ->
                  (x_1039 (if x__1363 true rand_bool) (fun x__1365 -> (x__1364 (if x__1365 true rand_bool)))))
                 rand_bool rand_bool
                 (fun x_1_1362 x_2_1362 x_3_1362 ->
                  (k_array_max_1129 (if x_1_1362 true rand_bool) (if (x_2_1362 || (x_1_1362 && x_1310)) true rand_bool)
                    false)))))))).
  fail_1325 b k -> {fail} => k.
  k_main_1192 k_main_1185 b1_1298 b2_1298 b3_1298 ->
      (l0
        (array_max_1011 (fun i_1345 k_make_array_1346 -> (k_make_array_1346 (if i_1345 true rand_bool)))
          (if b2_1298 true rand_bool) (if b1_1298 true rand_bool)
          (fun m1_1327 m2_1327 m3_1327 ->
           (if (if m2_1327 true rand_bool) (l0 k_main_1185) (l1 (fail_1325 true k_main_1185)))))).
  k_main_1192 k_main_1185 b1_1298 b2_1298 b3_1298 -> (if b3_1298 _|_ (l1 k_main_1185)).
Types:
  arg1_1085 : (unit -> unit)
  arg2_1087 : (unit -> unit)
  br_main_1323 : (unit -> unit)
  br_main_1323 : (unit -> unit)
  f_1322 : unit
  f_array_max_1317 : (unit -> (unit -> unit) -> unit)
  f_array_max_1318 : (unit -> (unit -> unit) -> unit)
  f_k_main_1319 : (unit -> unit)
  f_k_main_1319 : (unit -> unit)
  f_main_1320 : (unit -> unit)
  f_main_1321 : (unit -> unit)
  m_1300 : (unit -> unit)
  main_1018 : (unit -> unit)
  main_1018 : (unit -> unit)
  main_1089 : (unit -> unit)
  make_array_1008 : (unit -> unit)
  x_1016 : ((unit -> unit) -> unit -> unit)
  z_1017 : (unit -> unit)
  z_1017 : (unit -> unit)

LIFT:
Main: main_1316
  main_1316 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1192 () true true rand_bool)) (l1 (k_main_1192 () true true false))))
        (l1 (k_main_1192 () true true false))).
  array_max_1011 x_1039 m1_1015 m2_1015 k_array_max_1129 ->
      (if m1_1015 _|_ (l0 (k_array_max_1129 true rand_bool true))).
  array_max_1011 x_1039 m1_1015 m2_1015 k_array_max_1129 ->
      (l1 (x_1039 (if m2_1015 true rand_bool) (f_1374 x_1039 k_array_max_1129))).
  f_1374 x_1372 k_array_max_1373 x_1310 ->
      (if rand_bool (l0 (array_max_1011 (f_1377 x_1372) rand_bool rand_bool (f_1384 k_array_max_1373 x_1310)))
        (l1 (array_max_1011 (f_1387 x_1372) rand_bool rand_bool (f_1394 k_array_max_1373 x_1310)))).
  f_1384 k_array_max_1382 x_1383 x_1_1366 x_2_1366 x_3_1366 ->
      (k_array_max_1382 (if x_1_1366 true rand_bool) (if (x_2_1366 || (x_1_1366 && x_1383)) true rand_bool) false).
  f_1377 x_1376 x__1367 x__1368 -> (x_1376 (if x__1367 true rand_bool) (f_1380 x__1368)).
  f_1380 x__1379 x__1369 -> (x__1379 (if x__1369 true rand_bool)).
  f_1390 x__1389 x__1365 -> (x__1389 (if x__1365 true rand_bool)).
  f_1387 x_1386 x__1363 x__1364 -> (x_1386 (if x__1363 true rand_bool) (f_1390 x__1364)).
  f_1394 k_array_max_1392 x_1393 x_1_1362 x_2_1362 x_3_1362 ->
      (k_array_max_1392 (if x_1_1362 true rand_bool) (if (x_2_1362 || (x_1_1362 && x_1393)) true rand_bool) false).
  fail_1325 b k -> {fail} => k.
  k_main_1192 k_main_1185 b1_1298 b2_1298 b3_1298 ->
      (l0 (array_max_1011 f_1396 (if b2_1298 true rand_bool) (if b1_1298 true rand_bool) (f_1399 k_main_1185))).
  f_1396 i_1345 k_make_array_1346 -> (k_make_array_1346 (if i_1345 true rand_bool)).
  f_1399 k_main_1398 m1_1327 m2_1327 m3_1327 ->
      (if (if m2_1327 true rand_bool) (l0 k_main_1398) (l1 (fail_1325 true k_main_1398))).
  k_main_1192 k_main_1185 b1_1298 b2_1298 b3_1298 -> (if b3_1298 _|_ (l1 k_main_1185)).

TRANS_EAGER:
Main: main_1316
  main_1316 ->
      (let f_1401 b_1400 =
       (if b_1400
         (l0
           (let f_1403 b_1402 =
            (if b_1402
              (l0
                (let f_1405 b_1404 = (k_main_1192 () true true b_1404) in (if rand_bool (f_1405 true) (f_1405 false))))
              (l1 (k_main_1192 () true true false)))
            in (if rand_bool (f_1403 true) (f_1403 false)))) (l1 (k_main_1192 () true true false)))
       in (if rand_bool (f_1401 true) (f_1401 false))).
  array_max_1011 x_1039 m1_1015 m2_1015 k_array_max_1129 ->
      (let f_1407 b_1406 =
       (if b_1406 _|_
         (l0 ((let f_1409 b_1408 = (k_array_max_1129 true b_1408) in (if rand_bool (f_1409 true) (f_1409 false))) true)))
       in (f_1407 m1_1015)).
  array_max_1011 x_1039 m1_1015 m2_1015 k_array_max_1129 ->
      (l1
        ((let f_1413 b_1412 = (if b_1412 (x_1039 true) (if rand_bool (x_1039 true) (x_1039 false))) in (f_1413 m2_1015))
          (f_1374 x_1039 k_array_max_1129))).
  f_1374 x_1372 k_array_max_1373 x_1310 ->
      (let f_1415 b_1414 =
       (if b_1414
         (l0
           ((let f_1417 b_1416 =
             ((let f_1419 b_1418 = (array_max_1011 (f_1377 x_1372) b_1418) in
               (if rand_bool (f_1419 true) (f_1419 false))) b_1416)
             in (if rand_bool (f_1417 true) (f_1417 false))) (f_1384 k_array_max_1373 x_1310)))
         (l1
           ((let f_1421 b_1420 =
             ((let f_1423 b_1422 = (array_max_1011 (f_1387 x_1372) b_1422) in
               (if rand_bool (f_1423 true) (f_1423 false))) b_1420)
             in (if rand_bool (f_1421 true) (f_1421 false))) (f_1394 k_array_max_1373 x_1310))))
       in (if rand_bool (f_1415 true) (f_1415 false))).
  f_1384 k_array_max_1382 x_1383 x_1_1366 x_2_1366 x_3_1366 ->
      ((let f_1425 b_1424 =
        ((let f_1429 b_1428 =
          (if b_1428 (k_array_max_1382 true) (if rand_bool (k_array_max_1382 true) (k_array_max_1382 false))) in
          (f_1429 x_1_1366)) b_1424)
        in
        (let f_1431 b_1430 = (if b_1430 (f_1425 true) (if rand_bool (f_1425 true) (f_1425 false))) in
         (let f_1433 b_1432 =
          (if b_1432 (f_1431 true)
            (let f_1435 b_1434 = (if b_1434 (f_1431 x_1383) (f_1431 false)) in (f_1435 x_1_1366)))
          in (f_1433 x_2_1366)))) false).
  f_1377 x_1376 x__1367 x__1368 ->
      ((let f_1439 b_1438 = (if b_1438 (x_1376 true) (if rand_bool (x_1376 true) (x_1376 false))) in (f_1439 x__1367))
        (f_1380 x__1368)).
  f_1380 x__1379 x__1369 ->
      (let f_1443 b_1442 = (if b_1442 (x__1379 true) (if rand_bool (x__1379 true) (x__1379 false))) in (f_1443 x__1369)).
  f_1390 x__1389 x__1365 ->
      (let f_1447 b_1446 = (if b_1446 (x__1389 true) (if rand_bool (x__1389 true) (x__1389 false))) in (f_1447 x__1365)).
  f_1387 x_1386 x__1363 x__1364 ->
      ((let f_1451 b_1450 = (if b_1450 (x_1386 true) (if rand_bool (x_1386 true) (x_1386 false))) in (f_1451 x__1363))
        (f_1390 x__1364)).
  f_1394 k_array_max_1392 x_1393 x_1_1362 x_2_1362 x_3_1362 ->
      ((let f_1453 b_1452 =
        ((let f_1457 b_1456 =
          (if b_1456 (k_array_max_1392 true) (if rand_bool (k_array_max_1392 true) (k_array_max_1392 false))) in
          (f_1457 x_1_1362)) b_1452)
        in
        (let f_1459 b_1458 = (if b_1458 (f_1453 true) (if rand_bool (f_1453 true) (f_1453 false))) in
         (let f_1461 b_1460 =
          (if b_1460 (f_1459 true)
            (let f_1463 b_1462 = (if b_1462 (f_1459 x_1393) (f_1459 false)) in (f_1463 x_1_1362)))
          in (f_1461 x_2_1362)))) false).
  fail_1325 b k -> {fail} => k.
  k_main_1192 k_main_1185 b1_1298 b2_1298 b3_1298 ->
      (l0
        ((let f_1465 b_1464 =
          ((let f_1467 b_1466 = (array_max_1011 f_1396 b_1466) in
            (let f_1469 b_1468 = (if b_1468 (f_1467 true) (if rand_bool (f_1467 true) (f_1467 false))) in
             (f_1469 b2_1298))) b_1464)
          in
          (let f_1471 b_1470 = (if b_1470 (f_1465 true) (if rand_bool (f_1465 true) (f_1465 false))) in
           (f_1471 b1_1298))) (f_1399 k_main_1185))).
  f_1396 i_1345 k_make_array_1346 ->
      (let f_1475 b_1474 =
       (if b_1474 (k_make_array_1346 true) (if rand_bool (k_make_array_1346 true) (k_make_array_1346 false))) in
       (f_1475 i_1345)).
  f_1399 k_main_1398 m1_1327 m2_1327 m3_1327 ->
      (let f_1477 b_1476 = (if b_1476 (l0 k_main_1398) (l1 (fail_1325 true k_main_1398))) in
       (let f_1479 b_1478 = (if b_1478 (f_1477 true) (if rand_bool (f_1477 true) (f_1477 false))) in (f_1479 m2_1327))).
  k_main_1192 k_main_1185 b1_1298 b2_1298 b3_1298 ->
      (let f_1481 b_1480 = (if b_1480 _|_ (l1 k_main_1185)) in (f_1481 b3_1298)).

PUT_INTO_IF:
Main: main_1316
  main_1316 ->
      (let f_1401 b_1400 =
       (if b_1400
         (l0
           (let f_1403 b_1402 =
            (if b_1402
              (l0
                (let f_1405 b_1404 = (k_main_1192 () true true b_1404) in (if rand_bool (f_1405 true) (f_1405 false))))
              (l1 (k_main_1192 () true true false)))
            in (if rand_bool (f_1403 true) (f_1403 false)))) (l1 (k_main_1192 () true true false)))
       in (if rand_bool (f_1401 true) (f_1401 false))).
  array_max_1011 x_1039 m1_1015 m2_1015 k_array_max_1129 ->
      (let f_1407 b_1406 =
       (if b_1406 _|_
         (l0 ((let f_1409 b_1408 = (k_array_max_1129 true b_1408) in (if rand_bool (f_1409 true) (f_1409 false))) true)))
       in (f_1407 m1_1015)).
  array_max_1011 x_1039 m1_1015 m2_1015 k_array_max_1129 ->
      (l1
        ((let f_1413 b_1412 = (if b_1412 (x_1039 true) (if rand_bool (x_1039 true) (x_1039 false))) in (f_1413 m2_1015))
          (f_1374 x_1039 k_array_max_1129))).
  f_1374 x_1372 k_array_max_1373 x_1310 ->
      (let f_1415 b_1414 =
       (if b_1414
         (l0
           ((let f_1417 b_1416 =
             ((let f_1419 b_1418 = (array_max_1011 (f_1377 x_1372) b_1418) in
               (if rand_bool (f_1419 true) (f_1419 false))) b_1416)
             in (if rand_bool (f_1417 true) (f_1417 false))) (f_1384 k_array_max_1373 x_1310)))
         (l1
           ((let f_1421 b_1420 =
             ((let f_1423 b_1422 = (array_max_1011 (f_1387 x_1372) b_1422) in
               (if rand_bool (f_1423 true) (f_1423 false))) b_1420)
             in (if rand_bool (f_1421 true) (f_1421 false))) (f_1394 k_array_max_1373 x_1310))))
       in (if rand_bool (f_1415 true) (f_1415 false))).
  f_1384 k_array_max_1382 x_1383 x_1_1366 x_2_1366 x_3_1366 ->
      ((let f_1425 b_1424 =
        ((let f_1429 b_1428 =
          (if b_1428 (k_array_max_1382 true) (if rand_bool (k_array_max_1382 true) (k_array_max_1382 false))) in
          (f_1429 x_1_1366)) b_1424)
        in
        (let f_1431 b_1430 = (if b_1430 (f_1425 true) (if rand_bool (f_1425 true) (f_1425 false))) in
         (let f_1433 b_1432 =
          (if b_1432 (f_1431 true)
            (let f_1435 b_1434 = (if b_1434 (f_1431 x_1383) (f_1431 false)) in (f_1435 x_1_1366)))
          in (f_1433 x_2_1366)))) false).
  f_1377 x_1376 x__1367 x__1368 ->
      ((let f_1439 b_1438 = (if b_1438 (x_1376 true) (if rand_bool (x_1376 true) (x_1376 false))) in (f_1439 x__1367))
        (f_1380 x__1368)).
  f_1380 x__1379 x__1369 ->
      (let f_1443 b_1442 = (if b_1442 (x__1379 true) (if rand_bool (x__1379 true) (x__1379 false))) in (f_1443 x__1369)).
  f_1390 x__1389 x__1365 ->
      (let f_1447 b_1446 = (if b_1446 (x__1389 true) (if rand_bool (x__1389 true) (x__1389 false))) in (f_1447 x__1365)).
  f_1387 x_1386 x__1363 x__1364 ->
      ((let f_1451 b_1450 = (if b_1450 (x_1386 true) (if rand_bool (x_1386 true) (x_1386 false))) in (f_1451 x__1363))
        (f_1390 x__1364)).
  f_1394 k_array_max_1392 x_1393 x_1_1362 x_2_1362 x_3_1362 ->
      ((let f_1453 b_1452 =
        ((let f_1457 b_1456 =
          (if b_1456 (k_array_max_1392 true) (if rand_bool (k_array_max_1392 true) (k_array_max_1392 false))) in
          (f_1457 x_1_1362)) b_1452)
        in
        (let f_1459 b_1458 = (if b_1458 (f_1453 true) (if rand_bool (f_1453 true) (f_1453 false))) in
         (let f_1461 b_1460 =
          (if b_1460 (f_1459 true)
            (let f_1463 b_1462 = (if b_1462 (f_1459 x_1393) (f_1459 false)) in (f_1463 x_1_1362)))
          in (f_1461 x_2_1362)))) false).
  fail_1325 b k -> {fail} => k.
  k_main_1192 k_main_1185 b1_1298 b2_1298 b3_1298 ->
      (l0
        ((let f_1465 b_1464 =
          ((let f_1467 b_1466 = (array_max_1011 f_1396 b_1466) in
            (let f_1469 b_1468 = (if b_1468 (f_1467 true) (if rand_bool (f_1467 true) (f_1467 false))) in
             (f_1469 b2_1298))) b_1464)
          in
          (let f_1471 b_1470 = (if b_1470 (f_1465 true) (if rand_bool (f_1465 true) (f_1465 false))) in
           (f_1471 b1_1298))) (f_1399 k_main_1185))).
  f_1396 i_1345 k_make_array_1346 ->
      (let f_1475 b_1474 =
       (if b_1474 (k_make_array_1346 true) (if rand_bool (k_make_array_1346 true) (k_make_array_1346 false))) in
       (f_1475 i_1345)).
  f_1399 k_main_1398 m1_1327 m2_1327 m3_1327 ->
      (let f_1477 b_1476 = (if b_1476 (l0 k_main_1398) (l1 (fail_1325 true k_main_1398))) in
       (let f_1479 b_1478 = (if b_1478 (f_1477 true) (if rand_bool (f_1477 true) (f_1477 false))) in (f_1479 m2_1327))).
  k_main_1192 k_main_1185 b1_1298 b2_1298 b3_1298 ->
      (let f_1481 b_1480 = (if b_1480 _|_ (l1 k_main_1185)) in (f_1481 b3_1298)).

DONE!

(2-2) Checking HORS ... DONE!

Safe!

cycles: 2
total: 0.762 sec
  abst: 0.408 sec
  mc: 0.159 sec
  refine: 0.107 sec
    exparam: 0.058 sec
