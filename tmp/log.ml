MoCHi: Model Checker for Higher-Order Programs
  Build: _fdd8e37 (after 2014-07-28 13:18:56 +0900)
  FPAT version: 053139e
  TRecS version: 1.33
  OCaml version: 4.01.0
  Command: ./mochi.opt -tupling tmp/test.ml -debug-module CEGAR_abst_util -abs-filter -disable-rc -color -tupling 
           -list-option -fpat -hccs 1 -bool-init-empty -debug-module CEGAR_abst_CPS -abs-filter

parsed:
 let rec append_1008 xs__ys_1009 =
   match xs__ys_1009 true with
   | (b_1010, _) ->
       if b_1010 then
         append_1008 (fun x_1036 -> (match x_1036 with
                                     | _ -> (true, 0)))
       else
         (match xs__ys_1009 false with
          | (_, r2_1011) -> if r2_1011 = 0 then
                              ()
                            else
                              {fail} ()
          | _ -> let u_1050 = {fail} () in
                 _|_)
   | _ -> let u_1051 = {fail} () in
          _|_
 in
 let mynot_1012 b_1013 = if b_1013 then
                           false
                         else
                           true in
 let main_1017_1014 x_1056 = match x_1056 with
                             | () -> append_1008 (fun b_1015 -> (mynot_1012 b_1015, 0)) in
 ()

set_target:
 let rec append_1008 (xs__ys_1009:(bool -> (bool * int))) =
   match xs__ys_1009 true with
   | (b_1010, _) ->
       if b_1010 then
         append_1008 (fun (x_1036:bool) -> (match x_1036 with
                                            | _ -> (true, 0)))
       else
         (match xs__ys_1009 false with
          | (_, r2_1011) -> if r2_1011 = 0 then
                              ()
                            else
                              {fail} ()
          | _ -> let u_1050 = {fail} () in
                 _|_)
   | _ -> let u_1051 = {fail} () in
          _|_
 in
 let mynot_1012 (b_1013:bool) = if b_1013 then
                                  false
                                else
                                  true in
 let main_1017_1014 (x_1056:unit) = match x_1056 with
                                    | () -> append_1008 (fun (b_1015:bool) -> (mynot_1012 b_1015, 0)) in
 let main_1072 = let arg1_1070 = () in
                 main_1017_1014 arg1_1070 in
 ()

abst_recdata:
 let rec append_1008 (xs__ys_1009:(bool -> (bool * int))) =
   match xs__ys_1009 true with
   | (b_1010, _) ->
       if b_1010 then
         append_1008 (fun (x_1075:bool) -> (true, 0))
       else
         (match xs__ys_1009 false with
          | (_, r2_1011) -> if r2_1011 = 0 then
                              ()
                            else
                              {fail} ()
          | _ -> let u_1074 = {fail} () in
                 _|_)
   | _ -> let u_1076 = {fail} () in
          _|_
 in
 let mynot_1012 (b_1013:bool) = if b_1013 then
                                  false
                                else
                                  true in
 let main_1017_1014 (x_1056:unit) = append_1008 (fun (b_1077:bool) -> (mynot_1012 b_1077, 0)) in
 let main_1072 = let arg1_1070 = () in
                 main_1017_1014 arg1_1070 in
 ()

encode_list:
 let rec append_1008 (xs__ys_1009:(bool -> (bool * int))) =
   let xs_1102 = xs__ys_1009 true in
   let b_1010 = fst xs_1102 in
   if b_1010 then
     append_1008 (fun (x_1100:bool) -> (true, 0))
   else
     let xs_1103 = xs__ys_1009 false in
     let r2_1011 = snd xs_1103 in
     if r2_1011 = 0 then
       ()
     else
       {fail} ()
 in
 let mynot_1012 (b_1013:bool) = if b_1013 then
                                  false
                                else
                                  true in
 let main_1017_1014 (x_1056:unit) = append_1008 (fun (b_1077:bool) -> (mynot_1012 b_1077, 0)) in
 let main_1072 = main_1017_1014 () in
 ()

inlined:
 let rec append_1008 (xs__ys_1009:(bool -> (bool * int))) =
   let xs_1102 = xs__ys_1009 true in
   if fst xs_1102 then
     append_1008 (fun (x_1100:bool) -> (true, 0))
   else
     let xs_1103 = xs__ys_1009 false in
     if snd xs_1103 = 0 then
       ()
     else
       {fail} ()
 in
 let mynot_1012 (b_1013:bool) = if b_1013 then
                                  false
                                else
                                  true in
 let main_1017_1014 (x_1056:unit) = append_1008 (fun (b_1077:bool) -> (mynot_1012 b_1077, 0)) in
 let main_1072 = main_1017_1014 () in
 ()

CPS:
 let rec append_1008 (xs__ys_1009:(bool -> ((bool * int) -> X) -> X)) (k_append_1119:(unit -> X)) =
   let xs_1102 (k_append_xs_1126:((bool * int) -> X)) = xs__ys_1009 true k_append_xs_1126 in
   xs_1102
     (fun (xs_1187:(bool * int)) ->
        (if fst xs_1187 then
           append_1008 (fun (x_1279:bool) -> fun (k_append_1280:((bool * int) -> X)) -> k_append_1280 (true, 0))
             k_append_1119
         else
           let xs_1103 (k_append_xs_1165:((bool * int) -> X)) = xs__ys_1009 false k_append_xs_1165 in
           xs_1103
             (fun (xs_1182:(bool * int)) ->
                (if snd xs_1182 = 0 then
                   k_append_1119 ()
                 else
                   {|fail|} () k_append_1119))))
 in
 let mynot_1012 (b_1013:bool) (k_mynot_1197:(bool -> X)) = if b_1013 then
                                                             k_mynot_1197 false
                                                           else
                                                             k_mynot_1197 true in
 let main_1017_1014 (x_1056:unit) (k_main_1017_1208:(unit -> X)) =
   append_1008
     (fun (b_1284:bool) ->
        fun (k_main_1017_1285:((bool * int) -> X)) ->
          mynot_1012 b_1284 (fun (x_1292:bool) -> k_main_1017_1285 (x_1292, 0))) k_main_1017_1208
 in
 let main_1072 (k_main_1255:(unit -> X)) = main_1017_1014 () k_main_1255 in
 main_1072 (fun (main_1261:unit) -> {end})

remove_pair:
 let rec append_1008 (xs__ys_1009:(bool -> (bool -> int -> X) -> X)) (k_append_1119:(unit -> X)) =
   let xs_1102 (k_append_xs_1126:(bool -> int -> X)) = xs__ys_1009 true k_append_xs_1126 in
   xs_1102
     (fun (xs0_1187:bool) ->
        fun (xs1_1187:int) ->
          (if xs0_1187 then
             append_1008 (fun (x_1279:bool) -> fun (k_append_1280:(bool -> int -> X)) -> k_append_1280 true 0)
               k_append_1119
           else
             let xs_1103 (k_append_xs_1165:(bool -> int -> X)) = xs__ys_1009 false k_append_xs_1165 in
             xs_1103
               (fun (xs0_1182:bool) ->
                  fun (xs1_1182:int) -> (if xs1_1182 = 0 then
                                           k_append_1119 ()
                                         else
                                           {|fail|} () k_append_1119))))
 in
 let mynot_1012 (b_1013:bool) (k_mynot_1197:(bool -> X)) = if b_1013 then
                                                             k_mynot_1197 false
                                                           else
                                                             k_mynot_1197 true in
 let main_1017_1014 (x_1056:unit) (k_main_1017_1208:(unit -> X)) =
   append_1008
     (fun (b_1284:bool) ->
        fun (k_main_1017_1285:(bool -> int -> X)) -> mynot_1012 b_1284 (fun (x_1292:bool) -> k_main_1017_1285 x_1292 0))
     k_main_1017_1208
 in
 let main_1072 (k_main_1255:(unit -> X)) = main_1017_1014 () k_main_1255 in
 main_1072 (fun (main_1261:unit) -> {end})

Program with abstraction types (CEGAR-cycle 0)::
Main: main_1294
  main_1294 -> (main_1072 f_1300).
  append_1008 xs__ys_1009 k_append_1119 -> (xs_1102 xs__ys_1009 (f_append_1295 k_append_1119 xs__ys_1009)).
  f_1300 main_1261 -> end.
  f_append_1295 k_append_1119 xs__ys_1009 xs0_1187 xs1_1187 when xs0_1187 ->
      (append_1008 (f_append_1296 xs0_1187 xs1_1187) k_append_1119).
  f_append_1295 k_append_1119 xs__ys_1009 xs0_1187 xs1_1187 when (not xs0_1187) ->
      (xs_1103 xs0_1187 xs1_1187 xs__ys_1009 (f_append_1297 xs0_1187 xs1_1187 k_append_1119)).
  f_append_1296 xs0_1187 xs1_1187 x_1279 k_append_1280 -> (k_append_1280 true 0).
  f_append_1297 xs0_1187 xs1_1187 k_append_1119 xs0_1182 xs1_1182 when (xs1_1182 = 0) -> (k_append_1119 ()).
  f_append_1297 xs0_1187 xs1_1187 k_append_1119 xs0_1182 xs1_1182 when (
      not (xs1_1182 = 0)) -> (fail_1301 true k_append_1119).
  f_main_1017_1298 x_1056 b_1284 k_main_1017_1285 ->
      (mynot_1012 b_1284 (f_main_1017_1299 b_1284 x_1056 k_main_1017_1285)).
  f_main_1017_1299 b_1284 x_1056 k_main_1017_1285 x_1292 -> (k_main_1017_1285 x_1292 0).
  fail_1301 b k -> {fail} => (k ()).
  main_1017_1014 x_1056 k_main_1017_1208 -> (append_1008 (f_main_1017_1298 x_1056) k_main_1017_1208).
  main_1072 k_main_1255 -> (main_1017_1014 () k_main_1255).
  mynot_1012 b_1013 k_mynot_1197 when b_1013 -> (k_mynot_1197 false).
  mynot_1012 b_1013 k_mynot_1197 when (not b_1013) -> (k_mynot_1197 true).
  xs_1102 xs__ys_1009 k_append_xs_1126 -> (xs__ys_1009 true k_append_xs_1126).
  xs_1103 xs0_1187 xs1_1187 xs__ys_1009 k_append_xs_1165 -> (xs__ys_1009 false k_append_xs_1165).
Types:
  main_1294 : X
  append_1008 : ((bool -> (bool -> int -> X) -> X) -> (unit -> X) -> X)
  fail_1301 : (bool -> (unit -> X) -> X)

(0-1) Abstracting ... EXPAND_NONREC:
Main: main_1294
  main_1294 ->
      (append_1008
        (fun b_1369 k_main_1017_1370 -> (if b_1369 (l0 (k_main_1017_1370 false 0)) (l1 (k_main_1017_1370 true 0))))
        (fun main_1345 -> end)).
  append_1008 xs__ys_1009 k_append_1119 ->
      (xs__ys_1009 true
        (fun xs0_1352 xs1_1353 ->
         (if xs0_1352 (l0 (append_1008 (fun x_1356 k_append_1357 -> (k_append_1357 true 0)) k_append_1119))
           (l1
             (xs__ys_1009 false
               (fun xs0_1366 xs1_1367 ->
                (if (xs1_1367 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119))))))))).
  fail_1301 b k -> {fail} => (k ()).

ETA: (append_1008
       (fun b_1369 k_main_1017_1370 -> (if b_1369 (l0 (k_main_1017_1370 false 0)) (l1 (k_main_1017_1370 true 0))))
       (fun main_1345 -> end)): X
ETA: (fun main_1345 -> end): (unit ->
X)
ETA: end: X
ETA: (fun b_1369 k_main_1017_1370 -> (if b_1369 (l0 (k_main_1017_1370 false 0)) (l1 (k_main_1017_1370 true 0)))): (
bool -> (bool -> int -> X) -> X)
ETA: (fun k_main_1017_1370 -> (if b_1369 (l0 (k_main_1017_1370 false 0)) (l1 (k_main_1017_1370 true 0)))): ((
bool -> int -> X) ->
X)
ETA: (if b_1369 (l0 (k_main_1017_1370 false 0)) (l1 (k_main_1017_1370 true 0))): X
ETA: (l1 (k_main_1017_1370 true 0)): X
ETA: (k_main_1017_1370 true 0): X
ETA: 0: int
ETA: true: bool
ETA_AUX: (k_main_1017_1370 true 0): X
ETA: (l0 (k_main_1017_1370 false 0)): X
ETA: (k_main_1017_1370 false 0): X
ETA: 0: int
ETA: false: bool
ETA_AUX: (k_main_1017_1370 false 0): X
ETA_AUX: (append_1008
           (fun (b_1369:bool) (k_main_1017_1370:(bool -> int -> X)) ->
            (if b_1369 (l0 (k_main_1017_1370 false 0)) (l1 (k_main_1017_1370 true 0)))) (
           fun (main_1345:unit) -> end)): X
ETA: (xs__ys_1009 true
       (fun xs0_1352 xs1_1353 ->
        (if xs0_1352 (l0 (append_1008 (fun x_1356 k_append_1357 -> (k_append_1357 true 0)) k_append_1119))
          (l1
            (xs__ys_1009 false
              (fun xs0_1366 xs1_1367 -> (if (xs1_1367 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119))))))))): X
ETA: (fun xs0_1352 xs1_1353 ->
      (if xs0_1352 (l0 (append_1008 (fun x_1356 k_append_1357 -> (k_append_1357 true 0)) k_append_1119))
        (l1
          (xs__ys_1009 false
            (fun xs0_1366 xs1_1367 -> (if (xs1_1367 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119)))))))): (
bool -> int -> X)
ETA: (fun xs1_1353 ->
      (if xs0_1352 (l0 (append_1008 (fun x_1356 k_append_1357 -> (k_append_1357 true 0)) k_append_1119))
        (l1
          (xs__ys_1009 false
            (fun xs0_1366 xs1_1367 -> (if (xs1_1367 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119)))))))): (int
->
X)
ETA: (if xs0_1352 (l0 (append_1008 (fun x_1356 k_append_1357 -> (k_append_1357 true 0)) k_append_1119))
       (l1
         (xs__ys_1009 false
           (fun xs0_1366 xs1_1367 -> (if (xs1_1367 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119))))))): X
ETA: (l1
       (xs__ys_1009 false
         (fun xs0_1366 xs1_1367 -> (if (xs1_1367 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119)))))): X
ETA: (xs__ys_1009 false
       (fun xs0_1366 xs1_1367 -> (if (xs1_1367 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119))))): X
ETA: (fun xs0_1366 xs1_1367 -> (if (xs1_1367 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119)))): (
bool -> int -> X)
ETA: (fun xs1_1367 -> (if (xs1_1367 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119)))): (int ->
X)
ETA: (if (xs1_1367 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119))): X
ETA: (l1 (fail_1301 true k_append_1119)): X
ETA: (fail_1301 true k_append_1119): X
ETA: k_append_1119: (unit ->
X)
ETA_AUX: k_append_1119: (unit ->
X)
ETA_AUX: x__1388: unit
ETA_AUX: (k_append_1119 x__1388): X
ETA: true: bool
ETA_AUX: (fail_1301 true (fun (x__1388:unit) -> (k_append_1119 x__1388))): X
ETA: (l0 (k_append_1119 ())): X
ETA: (k_append_1119 ()): X
ETA: (): unit
ETA_AUX: (k_append_1119 ()): X
ETA: false: bool
ETA_AUX: (xs__ys_1009 false
           (fun (xs0_1366:bool) (xs1_1367:int) ->
            (if (xs1_1367 = 0) (l0 (k_append_1119 ()))
              (l1 (fail_1301 true (fun (x__1388:unit) -> (k_append_1119 x__1388))))))): X
ETA: (l0 (append_1008 (fun x_1356 k_append_1357 -> (k_append_1357 true 0)) k_append_1119)): X
ETA: (append_1008 (fun x_1356 k_append_1357 -> (k_append_1357 true 0)) k_append_1119): X
ETA: k_append_1119: (unit ->
X)
ETA_AUX: k_append_1119: (unit ->
X)
ETA_AUX: x__1389: unit
ETA_AUX: (k_append_1119 x__1389): X
ETA: (fun x_1356 k_append_1357 -> (k_append_1357 true 0)): (bool -> (bool -> int -> X) -> X)
ETA: (fun k_append_1357 -> (k_append_1357 true 0)): ((bool -> int -> X) ->
X)
ETA: (k_append_1357 true 0): X
ETA: 0: int
ETA: true: bool
ETA_AUX: (k_append_1357 true 0): X
ETA_AUX: (append_1008 (fun (x_1356:bool) (k_append_1357:(bool -> int -> X)) -> (k_append_1357 true 0))
           (fun (x__1389:unit) -> (k_append_1119 x__1389))): X
ETA: true: bool
ETA_AUX: (xs__ys_1009 true
           (fun (xs0_1352:bool) (xs1_1353:int) ->
            (if xs0_1352
              (l0
                (append_1008 (fun (x_1356:bool) (k_append_1357:(bool -> int -> X)) -> (k_append_1357 true 0))
                  (fun (x__1389:unit) -> (k_append_1119 x__1389))))
              (l1
                (xs__ys_1009 false
                  (fun (xs0_1366:bool) (xs1_1367:int) ->
                   (if (xs1_1367 = 0) (l0 (k_append_1119 ()))
                     (l1 (fail_1301 true (fun (x__1388:unit) -> (k_append_1119 x__1388))))))))))): X
ETA: (k ()): X
ETA: (): unit
ETA_AUX: (k ()): X
ETA_EXPAND:
Main: main_1294
  main_1294 ->
      (append_1008
        (fun (b_1369:bool) (k_main_1017_1370:(bool -> int -> X)) ->
         (if b_1369 (l0 (k_main_1017_1370 false 0)) (l1 (k_main_1017_1370 true 0)))) (
        fun (main_1345:unit) -> end)).
  append_1008 xs__ys_1009 k_append_1119 ->
      (xs__ys_1009 true
        (fun (xs0_1352:bool) (xs1_1353:int) ->
         (if xs0_1352
           (l0
             (append_1008 (fun (x_1356:bool) (k_append_1357:(bool -> int -> X)) -> (k_append_1357 true 0))
               (fun (x__1389:unit) -> (k_append_1119 x__1389))))
           (l1
             (xs__ys_1009 false
               (fun (xs0_1366:bool) (xs1_1367:int) ->
                (if (xs1_1367 = 0) (l0 (k_append_1119 ()))
                  (l1 (fail_1301 true (fun (x__1388:unit) -> (k_append_1119 x__1388))))))))))).
  fail_1301 b k -> {fail} => (k ()).

main_1294: ENV: 

main_1294: (append_1008
             (fun (b_1369:bool) (k_main_1017_1370:(bool -> int -> X)) ->
              (if b_1369 (l0 (k_main_1017_1370 false 0)) (l1 (k_main_1017_1370 true 0)))) (
             fun (main_1345:unit) -> end)) ===> (append_1008
                                                  (fun (b_1369:bool) (k_main_1017_1370:(bool -> int -> X)) ->
                                                   (if b_1369 (l0 (k_main_1017_1370 false 0))
                                                     (l1 (k_main_1017_1370 true 0)))) (
                                                  fun (main_1345:unit) -> end))
main_1294:: (append_1008
              (fun (b_1369:bool) (k_main_1017_1370:(bool -> int -> X)) ->
               (if b_1369 (l0 (k_main_1017_1370 false 0)) (l1 (k_main_1017_1370 true 0))))
              (fun (main_1345:unit) -> end))
abstract_term: (append_1008
                 (fun (b_1369:bool) (k_main_1017_1370:(bool -> int -> X)) ->
                  (if b_1369 (l0 (k_main_1017_1370 false 0)) (l1 (k_main_1017_1370 true 0))))
                 (fun (main_1345:unit) -> end)): X
abstract_term: (fun (b_1369:bool) (k_main_1017_1370:(bool -> int -> X)) ->
                (if b_1369 (l0 (k_main_1017_1370 false 0)) (l1 (k_main_1017_1370 true 0)))): (
bool -> (bool -> int -> X) -> X)
abst_arg: b_1369, bool
abst_arg: k_main_1017_1370, (bool -> int -> X)
abst_arg: b_1369, bool
abst_arg: k_main_1017_1370, (bool -> int -> X)
abstract_term: (if b_1369 (l0 (k_main_1017_1370 false 0)) (l1 (k_main_1017_1370 true 0))): X
abstract_term: b_1369: x_1:bool[x_1]
cond: true
pbs: 
p:b_1369
ds:0, pbss:0
xs:0, qs:0
tt:false
ff:false

abstract_term: (l0 (k_main_1017_1370 false 0)): X
abstract_term: (k_main_1017_1370 false 0): X
abstract_term: false: bool
abstract_term: 0: int
filter
cond: b_1369; true
orig pbs: 
unsat:false

abstract_term: (l1 (k_main_1017_1370 true 0)): X
abstract_term: (k_main_1017_1370 true 0): X
abstract_term: true: bool
abstract_term: 0: int
filter
cond: (not b_1369); true
orig pbs: 
unsat:false

filter
cond: true
orig pbs: 
unsat:false

abstract_term: (fun (main_1345:unit) -> end): (unit ->
X)
abst_arg: main_1345, unit
abst_arg: main_1345, unit
abstract_term: end: X
filter
cond: true
orig pbs: 
unsat:false

filter
cond: true
orig pbs: 
unsat:false

append_1008: ENV: xs__ys_1009:(bool -> (bool -> int -> X) -> X), k_append_1119:(unit -> X),


abst_arg: xs__ys_1009, (bool -> (bool -> int -> X) -> X)
abst_arg: k_append_1119, (unit ->
X)
abst_arg: xs__ys_1009, (bool -> (bool -> int -> X) -> X)
abst_arg: k_append_1119, (unit ->
X)
append_1008: (xs__ys_1009 true
               (fun (xs0_1352:bool) (xs1_1353:int) ->
                (if xs0_1352
                  (l0
                    (append_1008 (fun (x_1356:bool) (k_append_1357:(bool -> int -> X)) -> (k_append_1357 true 0))
                      (fun (x__1389:unit) -> (k_append_1119 x__1389))))
                  (l1
                    (xs__ys_1009 false
                      (fun (xs0_1366:bool) (xs1_1367:int) ->
                       (if (xs1_1367 = 0) (l0 (k_append_1119 ()))
                         (l1 (fail_1301 true (fun (x__1388:unit) -> (k_append_1119 x__1388))))))))))) ===> (
xs__ys_1009 true
 (fun (xs0_1352:bool) (xs1_1353:int) ->
  (if xs0_1352
    (l0
      (append_1008 (fun (x_1356:bool) (k_append_1357:(bool -> int -> X)) -> (k_append_1357 true 0))
        (fun (x__1389:unit) -> (k_append_1119 x__1389))))
    (l1
      (xs__ys_1009 false
        (fun (xs0_1366:bool) (xs1_1367:int) ->
         (if (xs1_1367 = 0) (l0 (k_append_1119 ()))
           (l1 (fail_1301 true (fun (x__1388:unit) -> (k_append_1119 x__1388)))))))))))
append_1008:: (xs__ys_1009 true
                (fun (xs0_1352:bool) (xs1_1353:int) ->
                 (if xs0_1352
                   (l0
                     (append_1008 (fun (x_1356:bool) (k_append_1357:(bool -> int -> X)) -> (k_append_1357 true 0))
                       (fun (x__1389:unit) -> (k_append_1119 x__1389))))
                   (l1
                     (xs__ys_1009 false
                       (fun (xs0_1366:bool) (xs1_1367:int) ->
                        (if (xs1_1367 = 0) (l0 (k_append_1119 ()))
                          (l1 (fail_1301 true (fun (x__1388:unit) -> (k_append_1119 x__1388)))))))))))
abstract_term: (xs__ys_1009 true
                 (fun (xs0_1352:bool) (xs1_1353:int) ->
                  (if xs0_1352
                    (l0
                      (append_1008 (fun (x_1356:bool) (k_append_1357:(bool -> int -> X)) -> (k_append_1357 true 0))
                        (fun (x__1389:unit) -> (k_append_1119 x__1389))))
                    (l1
                      (xs__ys_1009 false
                        (fun (xs0_1366:bool) (xs1_1367:int) ->
                         (if (xs1_1367 = 0) (l0 (k_append_1119 ()))
                           (l1 (fail_1301 true (fun (x__1388:unit) -> (k_append_1119 x__1388))))))))))): X
abstract_term: true: bool
abstract_term: (fun (xs0_1352:bool) (xs1_1353:int) ->
                (if xs0_1352
                  (l0
                    (append_1008 (fun (x_1356:bool) (k_append_1357:(bool -> int -> X)) -> (k_append_1357 true 0))
                      (fun (x__1389:unit) -> (k_append_1119 x__1389))))
                  (l1
                    (xs__ys_1009 false
                      (fun (xs0_1366:bool) (xs1_1367:int) ->
                       (if (xs1_1367 = 0) (l0 (k_append_1119 ()))
                         (l1 (fail_1301 true (fun (x__1388:unit) -> (k_append_1119 x__1388)))))))))): (
bool -> int -> X)
abst_arg: xs0_1352, bool
abst_arg: xs1_1353, int
abst_arg: xs0_1352, bool
abst_arg: xs1_1353, int
abstract_term: (if xs0_1352
                 (l0
                   (append_1008 (fun (x_1356:bool) (k_append_1357:(bool -> int -> X)) -> (k_append_1357 true 0))
                     (fun (x__1389:unit) -> (k_append_1119 x__1389))))
                 (l1
                   (xs__ys_1009 false
                     (fun (xs0_1366:bool) (xs1_1367:int) ->
                      (if (xs1_1367 = 0) (l0 (k_append_1119 ()))
                        (l1 (fail_1301 true (fun (x__1388:unit) -> (k_append_1119 x__1388))))))))): X
abstract_term: xs0_1352: x_1:bool[x_1]
cond: true
pbs: 
p:xs0_1352
ds:0, pbss:0
xs:0, qs:0
tt:false
ff:false

abstract_term: (l0
                 (append_1008 (fun (x_1356:bool) (k_append_1357:(bool -> int -> X)) -> (k_append_1357 true 0))
                   (fun (x__1389:unit) -> (k_append_1119 x__1389)))): X
abstract_term: (append_1008 (fun (x_1356:bool) (k_append_1357:(bool -> int -> X)) -> (k_append_1357 true 0))
                 (fun (x__1389:unit) -> (k_append_1119 x__1389))): X
abstract_term: (fun (x_1356:bool) (k_append_1357:(bool -> int -> X)) -> (k_append_1357 true 0)): (
bool -> (bool -> int -> X) -> X)
abst_arg: x_1356, bool
abst_arg: k_append_1357, (bool -> int -> X)
abst_arg: x_1356, bool
abst_arg: k_append_1357, (bool -> int -> X)
abstract_term: (k_append_1357 true 0): X
abstract_term: true: bool
abstract_term: 0: int
filter
cond: xs0_1352; true
orig pbs: 
unsat:false

filter
cond: xs0_1352; true
orig pbs: 
unsat:false

abstract_term: (fun (x__1389:unit) -> (k_append_1119 x__1389)): (unit ->
X)
abst_arg: x__1389, unit
abst_arg: x__1389, unit
abstract_term: (k_append_1119 x__1389): X
abstract_term: x__1389: unit
filter
cond: xs0_1352; true
orig pbs: 
unsat:false

filter
cond: xs0_1352; true
orig pbs: 
unsat:false

filter
cond: xs0_1352; true
orig pbs: 
unsat:false

abstract_term: (l1
                 (xs__ys_1009 false
                   (fun (xs0_1366:bool) (xs1_1367:int) ->
                    (if (xs1_1367 = 0) (l0 (k_append_1119 ()))
                      (l1 (fail_1301 true (fun (x__1388:unit) -> (k_append_1119 x__1388)))))))): X
abstract_term: (xs__ys_1009 false
                 (fun (xs0_1366:bool) (xs1_1367:int) ->
                  (if (xs1_1367 = 0) (l0 (k_append_1119 ()))
                    (l1 (fail_1301 true (fun (x__1388:unit) -> (k_append_1119 x__1388))))))): X
abstract_term: false: bool
abstract_term: (fun (xs0_1366:bool) (xs1_1367:int) ->
                (if (xs1_1367 = 0) (l0 (k_append_1119 ()))
                  (l1 (fail_1301 true (fun (x__1388:unit) -> (k_append_1119 x__1388)))))): (
bool -> int -> X)
abst_arg: xs0_1366, bool
abst_arg: xs1_1367, int
abst_arg: xs0_1366, bool
abst_arg: xs1_1367, int
abstract_term: (if (xs1_1367 = 0) (l0 (k_append_1119 ()))
                 (l1 (fail_1301 true (fun (x__1388:unit) -> (k_append_1119 x__1388))))): X
abstract_term: (xs1_1367 = 0): x_1:bool[x_1]
cond: (not xs0_1352); true
pbs: 
p:(xs1_1367 = 0)
ds:0, pbss:0
xs:0, qs:0
tt:false
ff:false

abstract_term: (l0 (k_append_1119 ())): X
abstract_term: (k_append_1119 ()): X
abstract_term: (): unit
filter
cond: (xs1_1367 = 0); (not xs0_1352); true
orig pbs: 
unsat:false

abstract_term: (l1 (fail_1301 true (fun (x__1388:unit) -> (k_append_1119 x__1388)))): X
abstract_term: (fail_1301 true (fun (x__1388:unit) -> (k_append_1119 x__1388))): X
abstract_term: true: bool
abstract_term: (fun (x__1388:unit) -> (k_append_1119 x__1388)): (unit ->
X)
abst_arg: x__1388, unit
abst_arg: x__1388, unit
abstract_term: (k_append_1119 x__1388): X
abstract_term: x__1388: unit
filter
cond: (not (xs1_1367 = 0)); (not xs0_1352); true
orig pbs: 
unsat:false

filter
cond: (not (xs1_1367 = 0)); (not xs0_1352); true
orig pbs: 
unsat:false

filter
cond: (not (xs1_1367 = 0)); (not xs0_1352); true
orig pbs: 
unsat:false

filter
cond: (not xs0_1352); true
orig pbs: 
unsat:false

filter
cond: (not xs0_1352); true
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

fail_1301: ENV: b:bool, k:(unit -> X),


abst_arg: b, bool
abst_arg: k, (unit ->
X)
abst_arg: b, bool
abst_arg: k, (unit ->
X)
fail_1301: (k ()) ===> (k ())
fail_1301:: (k ())
abstract_term: (k ()): X
abstract_term: (): unit
filter
cond: true
orig pbs: 
unsat:false

ABST:
Main: main_1294
  main_1294 -> (append_1008 (fun k_main_1017_1370 -> (if rand_bool (l0 k_main_1017_1370) (l1 k_main_1017_1370))) ()).
  append_1008 xs__ys_1009 k_append_1119 ->
      (xs__ys_1009
        (if rand_bool (l0 (append_1008 (fun k_append_1357 -> k_append_1357) k_append_1119))
          (l1 (xs__ys_1009 (if rand_bool (l0 k_append_1119) (l1 (fail_1301 k_append_1119))))))).
  fail_1301 k -> {fail} => k.
Types:
  f_1300 : unit
  f_append_1295 : (unit -> (unit -> unit) -> unit)
  f_append_1296 : (unit -> unit)
  f_append_1297 : (unit -> unit)
  f_main_1017_1298 : (unit -> unit)
  f_main_1017_1299 : (unit -> unit)
  main_1017_1014 : (unit -> unit)
  main_1072 : (unit -> unit)
  mynot_1012 : (unit -> unit)
  xs_1102 : ((unit -> unit) -> unit -> unit)
  xs_1103 : ((unit -> unit) -> unit -> unit)

LIFT:
Main: main_1294
  main_1294 -> (append_1008 f_1391 ()).
  f_1391 k_main_1017_1370 -> (if rand_bool (l0 k_main_1017_1370) (l1 k_main_1017_1370)).
  append_1008 xs__ys_1009 k_append_1119 ->
      (xs__ys_1009
        (if rand_bool (l0 (append_1008 f_1393 k_append_1119))
          (l1 (xs__ys_1009 (if rand_bool (l0 k_append_1119) (l1 (fail_1301 k_append_1119))))))).
  f_1393 k_append_1357 -> k_append_1357.
  fail_1301 k -> {fail} => k.

TRANS_EAGER:
Main: main_1294
  main_1294 -> (append_1008 f_1391 ()).
  f_1391 k_main_1017_1370 ->
      (let f_1395 b_1394 = (if b_1394 (l0 k_main_1017_1370) (l1 k_main_1017_1370)) in
       (if rand_bool (f_1395 true) (f_1395 false))).
  append_1008 xs__ys_1009 k_append_1119 ->
      (xs__ys_1009
        (let f_1397 b_1396 =
         (if b_1396 (l0 (append_1008 f_1393 k_append_1119))
           (l1
             (xs__ys_1009
               (let f_1399 b_1398 = (if b_1398 (l0 k_append_1119) (l1 (fail_1301 k_append_1119))) in
                (if rand_bool (f_1399 true) (f_1399 false))))))
         in (if rand_bool (f_1397 true) (f_1397 false)))).
  f_1393 k_append_1357 -> k_append_1357.
  fail_1301 k -> {fail} => k.

PUT_INTO_IF:
Main: main_1294
  main_1294 -> (append_1008 f_1391 ()).
  f_1391 k_main_1017_1370 ->
      (let f_1395 b_1394 = (if b_1394 (l0 k_main_1017_1370) (l1 k_main_1017_1370)) in
       (if rand_bool (f_1395 true) (f_1395 false))).
  append_1008 xs__ys_1009 k_append_1119 ->
      (xs__ys_1009
        (let f_1397 b_1396 =
         (if b_1396 (l0 (append_1008 f_1393 k_append_1119))
           (l1
             (xs__ys_1009
               (let f_1399 b_1398 = (if b_1398 (l0 k_append_1119) (l1 (fail_1301 k_append_1119))) in
                (if rand_bool (f_1399 true) (f_1399 false))))))
         in (if rand_bool (f_1397 true) (f_1397 false)))).
  f_1393 k_append_1357 -> k_append_1357.
  fail_1301 k -> {fail} => k.

DONE!

(0-2) Checking HORS ... DONE!

Error trace::
  main_1294 ... --> 
  main_1072 ... --> 
  main_1017_1014 ... --> 
  append_1008 ... --> 
  xs_1102 ... --> 
  f_main_1017_1298 ... --> 
  mynot_1012 [1/2] ... --> 
  f_main_1017_1299 ... --> 
  f_append_1295 [2/2] ... --> 
  xs_1103 ... --> 
  f_main_1017_1298 ... --> 
  mynot_1012 [1/2] ... --> 
  f_main_1017_1299 ... --> 
  f_append_1297 [2/2] ... --> 
  fail_1301 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 1; 0

(0-3) Checking counterexample ... DONE!

(0-4) Discovering predicates ... 
DONE!

Prefix of spurious counterexample::
0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0

Program with abstraction types (CEGAR-cycle 1)::
Main: main_1294
  main_1294 -> (main_1072 f_1300).
  append_1008 xs__ys_1009 k_append_1119 -> (xs_1102 xs__ys_1009 (f_append_1295 k_append_1119 xs__ys_1009)).
  f_1300 main_1261 -> end.
  f_append_1295 k_append_1119 xs__ys_1009 xs0_1187 xs1_1187 when xs0_1187 ->
      (append_1008 (f_append_1296 xs0_1187 xs1_1187) k_append_1119).
  f_append_1295 k_append_1119 xs__ys_1009 xs0_1187 xs1_1187 when (not xs0_1187) ->
      (xs_1103 xs0_1187 xs1_1187 xs__ys_1009 (f_append_1297 xs0_1187 xs1_1187 k_append_1119)).
  f_append_1296 xs0_1187 xs1_1187 x_1279 k_append_1280 -> (k_append_1280 true 0).
  f_append_1297 xs0_1187 xs1_1187 k_append_1119 xs0_1182 xs1_1182 when (xs1_1182 = 0) -> (k_append_1119 ()).
  f_append_1297 xs0_1187 xs1_1187 k_append_1119 xs0_1182 xs1_1182 when (
      not (xs1_1182 = 0)) -> (fail_1301 true k_append_1119).
  f_main_1017_1298 x_1056 b_1284 k_main_1017_1285 ->
      (mynot_1012 b_1284 (f_main_1017_1299 b_1284 x_1056 k_main_1017_1285)).
  f_main_1017_1299 b_1284 x_1056 k_main_1017_1285 x_1292 -> (k_main_1017_1285 x_1292 0).
  fail_1301 b k -> {fail} => (k ()).
  main_1017_1014 x_1056 k_main_1017_1208 -> (append_1008 (f_main_1017_1298 x_1056) k_main_1017_1208).
  main_1072 k_main_1255 -> (main_1017_1014 () k_main_1255).
  mynot_1012 b_1013 k_mynot_1197 when b_1013 -> (k_mynot_1197 false).
  mynot_1012 b_1013 k_mynot_1197 when (not b_1013) -> (k_mynot_1197 true).
  xs_1102 xs__ys_1009 k_append_xs_1126 -> (xs__ys_1009 true k_append_xs_1126).
  xs_1103 xs0_1187 xs1_1187 xs__ys_1009 k_append_xs_1165 -> (xs__ys_1009 false k_append_xs_1165).
Types:
  main_1294 : X
  append_1008 : ((x_2:bool -> (bool -> int[x_2] -> X) -> X) -> (unit -> X) -> X)
  fail_1301 : (bool -> (unit -> X) -> X)

(1-1) Abstracting ... EXPAND_NONREC:
Main: main_1294
  main_1294 ->
      (append_1008
        (fun b_1669 k_main_1017_1670 -> (if b_1669 (l0 (k_main_1017_1670 false 0)) (l1 (k_main_1017_1670 true 0))))
        (fun main_1645 -> end)).
  append_1008 xs__ys_1009 k_append_1119 ->
      (xs__ys_1009 true
        (fun xs0_1652 xs1_1653 ->
         (if xs0_1652 (l0 (append_1008 (fun x_1656 k_append_1657 -> (k_append_1657 true 0)) k_append_1119))
           (l1
             (xs__ys_1009 false
               (fun xs0_1666 xs1_1667 ->
                (if (xs1_1667 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119))))))))).
  fail_1301 b k -> {fail} => (k ()).

ETA: (append_1008
       (fun b_1669 k_main_1017_1670 -> (if b_1669 (l0 (k_main_1017_1670 false 0)) (l1 (k_main_1017_1670 true 0))))
       (fun main_1645 -> end)): X
ETA: (fun main_1645 -> end): (unit ->
X)
ETA: end: X
ETA: (fun b_1669 k_main_1017_1670 -> (if b_1669 (l0 (k_main_1017_1670 false 0)) (l1 (k_main_1017_1670 true 0)))): (
x_1:bool -> (bool -> int[x_1] -> X) -> X)
ETA: (fun k_main_1017_1670 -> (if b_1669 (l0 (k_main_1017_1670 false 0)) (l1 (k_main_1017_1670 true 0)))): ((
bool -> int[b_1669] -> X) ->
X)
ETA: (if b_1669 (l0 (k_main_1017_1670 false 0)) (l1 (k_main_1017_1670 true 0))): X
ETA: (l1 (k_main_1017_1670 true 0)): X
ETA: (k_main_1017_1670 true 0): X
ETA: 0: int[b_1669]
ETA: true: bool
ETA_AUX: (k_main_1017_1670 true 0): X
ETA: (l0 (k_main_1017_1670 false 0)): X
ETA: (k_main_1017_1670 false 0): X
ETA: 0: int[b_1669]
ETA: false: bool
ETA_AUX: (k_main_1017_1670 false 0): X
ETA_AUX: (append_1008
           (fun (b_1669:bool) (k_main_1017_1670:(bool -> int[b_1669] -> X)) ->
            (if b_1669 (l0 (k_main_1017_1670 false 0)) (l1 (k_main_1017_1670 true 0)))) (
           fun (main_1645:unit) -> end)): X
ETA: (xs__ys_1009 true
       (fun xs0_1652 xs1_1653 ->
        (if xs0_1652 (l0 (append_1008 (fun x_1656 k_append_1657 -> (k_append_1657 true 0)) k_append_1119))
          (l1
            (xs__ys_1009 false
              (fun xs0_1666 xs1_1667 -> (if (xs1_1667 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119))))))))): X
ETA: (fun xs0_1652 xs1_1653 ->
      (if xs0_1652 (l0 (append_1008 (fun x_1656 k_append_1657 -> (k_append_1657 true 0)) k_append_1119))
        (l1
          (xs__ys_1009 false
            (fun xs0_1666 xs1_1667 -> (if (xs1_1667 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119)))))))): (
bool -> int[true] -> X)
ETA: (fun xs1_1653 ->
      (if xs0_1652 (l0 (append_1008 (fun x_1656 k_append_1657 -> (k_append_1657 true 0)) k_append_1119))
        (l1
          (xs__ys_1009 false
            (fun xs0_1666 xs1_1667 -> (if (xs1_1667 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119)))))))): (int[
true] ->
X)
ETA: (if xs0_1652 (l0 (append_1008 (fun x_1656 k_append_1657 -> (k_append_1657 true 0)) k_append_1119))
       (l1
         (xs__ys_1009 false
           (fun xs0_1666 xs1_1667 -> (if (xs1_1667 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119))))))): X
ETA: (l1
       (xs__ys_1009 false
         (fun xs0_1666 xs1_1667 -> (if (xs1_1667 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119)))))): X
ETA: (xs__ys_1009 false
       (fun xs0_1666 xs1_1667 -> (if (xs1_1667 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119))))): X
ETA: (fun xs0_1666 xs1_1667 -> (if (xs1_1667 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119)))): (
bool -> int[false] -> X)
ETA: (fun xs1_1667 -> (if (xs1_1667 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119)))): (int[
false] ->
X)
ETA: (if (xs1_1667 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119))): X
ETA: (l1 (fail_1301 true k_append_1119)): X
ETA: (fail_1301 true k_append_1119): X
ETA: k_append_1119: (unit ->
X)
ETA_AUX: k_append_1119: (unit ->
X)
ETA_AUX: x__1688: unit
ETA_AUX: (k_append_1119 x__1688): X
ETA: true: bool
ETA_AUX: (fail_1301 true (fun (x__1688:unit) -> (k_append_1119 x__1688))): X
ETA: (l0 (k_append_1119 ())): X
ETA: (k_append_1119 ()): X
ETA: (): unit
ETA_AUX: (k_append_1119 ()): X
ETA: false: bool
ETA_AUX: (xs__ys_1009 false
           (fun (xs0_1666:bool) (xs1_1667:int[false]) ->
            (if (xs1_1667 = 0) (l0 (k_append_1119 ()))
              (l1 (fail_1301 true (fun (x__1688:unit) -> (k_append_1119 x__1688))))))): X
ETA: (l0 (append_1008 (fun x_1656 k_append_1657 -> (k_append_1657 true 0)) k_append_1119)): X
ETA: (append_1008 (fun x_1656 k_append_1657 -> (k_append_1657 true 0)) k_append_1119): X
ETA: k_append_1119: (unit ->
X)
ETA_AUX: k_append_1119: (unit ->
X)
ETA_AUX: x__1689: unit
ETA_AUX: (k_append_1119 x__1689): X
ETA: (fun x_1656 k_append_1657 -> (k_append_1657 true 0)): (x_1:bool -> (bool -> int[x_1] -> X) -> X)
ETA: (fun k_append_1657 -> (k_append_1657 true 0)): ((bool -> int[x_1656] -> X) ->
X)
ETA: (k_append_1657 true 0): X
ETA: 0: int[x_1656]
ETA: true: bool
ETA_AUX: (k_append_1657 true 0): X
ETA_AUX: (append_1008 (fun (x_1656:bool) (k_append_1657:(bool -> int[x_1656] -> X)) -> (k_append_1657 true 0))
           (fun (x__1689:unit) -> (k_append_1119 x__1689))): X
ETA: true: bool
ETA_AUX: (xs__ys_1009 true
           (fun (xs0_1652:bool) (xs1_1653:int[true]) ->
            (if xs0_1652
              (l0
                (append_1008 (fun (x_1656:bool) (k_append_1657:(bool -> int[x_1656] -> X)) -> (k_append_1657 true 0))
                  (fun (x__1689:unit) -> (k_append_1119 x__1689))))
              (l1
                (xs__ys_1009 false
                  (fun (xs0_1666:bool) (xs1_1667:int[false]) ->
                   (if (xs1_1667 = 0) (l0 (k_append_1119 ()))
                     (l1 (fail_1301 true (fun (x__1688:unit) -> (k_append_1119 x__1688))))))))))): X
ETA: (k ()): X
ETA: (): unit
ETA_AUX: (k ()): X
ETA_EXPAND:
Main: main_1294
  main_1294 ->
      (append_1008
        (fun (b_1669:bool) (k_main_1017_1670:(bool -> int[b_1669] -> X)) ->
         (if b_1669 (l0 (k_main_1017_1670 false 0)) (l1 (k_main_1017_1670 true 0)))) (
        fun (main_1645:unit) -> end)).
  append_1008 xs__ys_1009 k_append_1119 ->
      (xs__ys_1009 true
        (fun (xs0_1652:bool) (xs1_1653:int[true]) ->
         (if xs0_1652
           (l0
             (append_1008 (fun (x_1656:bool) (k_append_1657:(bool -> int[x_1656] -> X)) -> (k_append_1657 true 0))
               (fun (x__1689:unit) -> (k_append_1119 x__1689))))
           (l1
             (xs__ys_1009 false
               (fun (xs0_1666:bool) (xs1_1667:int[false]) ->
                (if (xs1_1667 = 0) (l0 (k_append_1119 ()))
                  (l1 (fail_1301 true (fun (x__1688:unit) -> (k_append_1119 x__1688))))))))))).
  fail_1301 b k -> {fail} => (k ()).

main_1294: ENV: 

main_1294: (append_1008
             (fun (b_1669:bool) (k_main_1017_1670:(bool -> int[b_1669] -> X)) ->
              (if b_1669 (l0 (k_main_1017_1670 false 0)) (l1 (k_main_1017_1670 true 0)))) (
             fun (main_1645:unit) -> end)) ===> (append_1008
                                                  (fun (b_1669:bool) (k_main_1017_1670:(bool -> int[b_1669] -> X)) ->
                                                   (if b_1669 (l0 (k_main_1017_1670 false 0))
                                                     (l1 (k_main_1017_1670 true 0)))) (
                                                  fun (main_1645:unit) -> end))
main_1294:: (append_1008
              (fun (b_1669:bool) (k_main_1017_1670:(bool -> int[b_1669] -> X)) ->
               (if b_1669 (l0 (k_main_1017_1670 false 0)) (l1 (k_main_1017_1670 true 0))))
              (fun (main_1645:unit) -> end))
abstract_term: (append_1008
                 (fun (b_1669:bool) (k_main_1017_1670:(bool -> int[b_1669] -> X)) ->
                  (if b_1669 (l0 (k_main_1017_1670 false 0)) (l1 (k_main_1017_1670 true 0))))
                 (fun (main_1645:unit) -> end)): X
abstract_term: (fun (b_1669:bool) (k_main_1017_1670:(bool -> int[b_1669] -> X)) ->
                (if b_1669 (l0 (k_main_1017_1670 false 0)) (l1 (k_main_1017_1670 true 0)))): (
x_1:bool -> (bool -> int[x_1] -> X) -> X)
abst_arg: b_1669, bool
abst_arg: k_main_1017_1670, (bool -> int[b_1669] -> X)
abst_arg: b_1669, bool
abst_arg: k_main_1017_1670, (bool -> int[b_1669] -> X)
abstract_term: (if b_1669 (l0 (k_main_1017_1670 false 0)) (l1 (k_main_1017_1670 true 0))): X
abstract_term: b_1669: x_1:bool[x_1]
cond: true
pbs: 
p:b_1669
ds:0, pbss:0
xs:0, qs:0
tt:false
ff:false

abstract_term: (l0 (k_main_1017_1670 false 0)): X
abstract_term: (k_main_1017_1670 false 0): X
abstract_term: false: bool
abstract_term: 0: int[b_1669]
cond: b_1669; true
pbs: 
p:b_1669
tt:true
ff:false

filter
cond: b_1669; true
orig pbs: 
unsat:false

abstract_term: (l1 (k_main_1017_1670 true 0)): X
abstract_term: (k_main_1017_1670 true 0): X
abstract_term: true: bool
abstract_term: 0: int[b_1669]
cond: (not b_1669); true
pbs: 
p:b_1669
tt:false
ff:true

filter
cond: (not b_1669); true
orig pbs: 
unsat:false

filter
cond: true
orig pbs: 
unsat:false

abstract_term: (fun (main_1645:unit) -> end): (unit ->
X)
abst_arg: main_1645, unit
abst_arg: main_1645, unit
abstract_term: end: X
filter
cond: true
orig pbs: 
unsat:false

filter
cond: true
orig pbs: 
unsat:false

append_1008: ENV: xs__ys_1009:(x_1:bool -> (bool -> int[x_1] -> X) -> X), k_append_1119:(unit -> X),


abst_arg: xs__ys_1009, (x_1:bool -> (bool -> int[x_1] -> X) -> X)
abst_arg: k_append_1119, (unit ->
X)
abst_arg: xs__ys_1009, (x_1:bool -> (bool -> int[x_1] -> X) -> X)
abst_arg: k_append_1119, (unit ->
X)
append_1008: (xs__ys_1009 true
               (fun (xs0_1652:bool) (xs1_1653:int[true]) ->
                (if xs0_1652
                  (l0
                    (append_1008
                      (fun (x_1656:bool) (k_append_1657:(bool -> int[x_1656] -> X)) -> (k_append_1657 true 0))
                      (fun (x__1689:unit) -> (k_append_1119 x__1689))))
                  (l1
                    (xs__ys_1009 false
                      (fun (xs0_1666:bool) (xs1_1667:int[false]) ->
                       (if (xs1_1667 = 0) (l0 (k_append_1119 ()))
                         (l1 (fail_1301 true (fun (x__1688:unit) -> (k_append_1119 x__1688))))))))))) ===> (
xs__ys_1009 true
 (fun (xs0_1652:bool) (xs1_1653:int[true]) ->
  (if xs0_1652
    (l0
      (append_1008 (fun (x_1656:bool) (k_append_1657:(bool -> int[x_1656] -> X)) -> (k_append_1657 true 0))
        (fun (x__1689:unit) -> (k_append_1119 x__1689))))
    (l1
      (xs__ys_1009 false
        (fun (xs0_1666:bool) (xs1_1667:int[false]) ->
         (if (xs1_1667 = 0) (l0 (k_append_1119 ()))
           (l1 (fail_1301 true (fun (x__1688:unit) -> (k_append_1119 x__1688)))))))))))
append_1008:: (xs__ys_1009 true
                (fun (xs0_1652:bool) (xs1_1653:int[true]) ->
                 (if xs0_1652
                   (l0
                     (append_1008
                       (fun (x_1656:bool) (k_append_1657:(bool -> int[x_1656] -> X)) -> (k_append_1657 true 0))
                       (fun (x__1689:unit) -> (k_append_1119 x__1689))))
                   (l1
                     (xs__ys_1009 false
                       (fun (xs0_1666:bool) (xs1_1667:int[false]) ->
                        (if (xs1_1667 = 0) (l0 (k_append_1119 ()))
                          (l1 (fail_1301 true (fun (x__1688:unit) -> (k_append_1119 x__1688)))))))))))
abstract_term: (xs__ys_1009 true
                 (fun (xs0_1652:bool) (xs1_1653:int[true]) ->
                  (if xs0_1652
                    (l0
                      (append_1008
                        (fun (x_1656:bool) (k_append_1657:(bool -> int[x_1656] -> X)) -> (k_append_1657 true 0))
                        (fun (x__1689:unit) -> (k_append_1119 x__1689))))
                    (l1
                      (xs__ys_1009 false
                        (fun (xs0_1666:bool) (xs1_1667:int[false]) ->
                         (if (xs1_1667 = 0) (l0 (k_append_1119 ()))
                           (l1 (fail_1301 true (fun (x__1688:unit) -> (k_append_1119 x__1688))))))))))): X
abstract_term: true: bool
abstract_term: (fun (xs0_1652:bool) (xs1_1653:int[true]) ->
                (if xs0_1652
                  (l0
                    (append_1008
                      (fun (x_1656:bool) (k_append_1657:(bool -> int[x_1656] -> X)) -> (k_append_1657 true 0))
                      (fun (x__1689:unit) -> (k_append_1119 x__1689))))
                  (l1
                    (xs__ys_1009 false
                      (fun (xs0_1666:bool) (xs1_1667:int[false]) ->
                       (if (xs1_1667 = 0) (l0 (k_append_1119 ()))
                         (l1 (fail_1301 true (fun (x__1688:unit) -> (k_append_1119 x__1688)))))))))): (
bool -> int[true] -> X)
abst_arg: xs0_1652, bool
abst_arg: xs1_1653, int[true]
abst_arg: xs0_1652, bool
abst_arg: xs1_1653, int[true]
abstract_term: (if xs0_1652
                 (l0
                   (append_1008
                     (fun (x_1656:bool) (k_append_1657:(bool -> int[x_1656] -> X)) -> (k_append_1657 true 0))
                     (fun (x__1689:unit) -> (k_append_1119 x__1689))))
                 (l1
                   (xs__ys_1009 false
                     (fun (xs0_1666:bool) (xs1_1667:int[false]) ->
                      (if (xs1_1667 = 0) (l0 (k_append_1119 ()))
                        (l1 (fail_1301 true (fun (x__1688:unit) -> (k_append_1119 x__1688))))))))): X
abstract_term: xs0_1652: x_1:bool[x_1]
cond: true
pbs: 
p:xs0_1652
ds:0, pbss:0
xs:0, qs:0
tt:false
ff:false

abstract_term: (l0
                 (append_1008 (fun (x_1656:bool) (k_append_1657:(bool -> int[x_1656] -> X)) -> (k_append_1657 true 0))
                   (fun (x__1689:unit) -> (k_append_1119 x__1689)))): X
abstract_term: (append_1008 (fun (x_1656:bool) (k_append_1657:(bool -> int[x_1656] -> X)) -> (k_append_1657 true 0))
                 (fun (x__1689:unit) -> (k_append_1119 x__1689))): X
abstract_term: (fun (x_1656:bool) (k_append_1657:(bool -> int[x_1656] -> X)) -> (k_append_1657 true 0)): (
x_1:bool -> (bool -> int[x_1] -> X) -> X)
abst_arg: x_1656, bool
abst_arg: k_append_1657, (bool -> int[x_1656] -> X)
abst_arg: x_1656, bool
abst_arg: k_append_1657, (bool -> int[x_1656] -> X)
abstract_term: (k_append_1657 true 0): X
abstract_term: true: bool
abstract_term: 0: int[x_1656]
cond: xs0_1652; true
pbs: 
p:x_1656
ds:0, pbss:0
xs:0, qs:0
tt:false
ff:false

filter
cond: xs0_1652; true
orig pbs: 
unsat:false

filter
cond: xs0_1652; true
orig pbs: 
unsat:false

abstract_term: (fun (x__1689:unit) -> (k_append_1119 x__1689)): (unit ->
X)
abst_arg: x__1689, unit
abst_arg: x__1689, unit
abstract_term: (k_append_1119 x__1689): X
abstract_term: x__1689: unit
filter
cond: xs0_1652; true
orig pbs: 
unsat:false

filter
cond: xs0_1652; true
orig pbs: 
unsat:false

filter
cond: xs0_1652; true
orig pbs: 
unsat:false

abstract_term: (l1
                 (xs__ys_1009 false
                   (fun (xs0_1666:bool) (xs1_1667:int[false]) ->
                    (if (xs1_1667 = 0) (l0 (k_append_1119 ()))
                      (l1 (fail_1301 true (fun (x__1688:unit) -> (k_append_1119 x__1688)))))))): X
abstract_term: (xs__ys_1009 false
                 (fun (xs0_1666:bool) (xs1_1667:int[false]) ->
                  (if (xs1_1667 = 0) (l0 (k_append_1119 ()))
                    (l1 (fail_1301 true (fun (x__1688:unit) -> (k_append_1119 x__1688))))))): X
abstract_term: false: bool
abstract_term: (fun (xs0_1666:bool) (xs1_1667:int[false]) ->
                (if (xs1_1667 = 0) (l0 (k_append_1119 ()))
                  (l1 (fail_1301 true (fun (x__1688:unit) -> (k_append_1119 x__1688)))))): (
bool -> int[false] -> X)
abst_arg: xs0_1666, bool
abst_arg: xs1_1667, int[false]
abst_arg: xs0_1666, bool
abst_arg: xs1_1667, int[false]
abstract_term: (if (xs1_1667 = 0) (l0 (k_append_1119 ()))
                 (l1 (fail_1301 true (fun (x__1688:unit) -> (k_append_1119 x__1688))))): X
abstract_term: (xs1_1667 = 0): x_1:bool[x_1]
cond: (not xs0_1652); true
pbs: 
p:(xs1_1667 = 0)
ds:0, pbss:0
xs:0, qs:0
tt:false
ff:false

abstract_term: (l0 (k_append_1119 ())): X
abstract_term: (k_append_1119 ()): X
abstract_term: (): unit
filter
cond: (xs1_1667 = 0); (not xs0_1652); true
orig pbs: xs1_1667 := false
pbs: 

ds:0, pbss:0
xs:0, qs:0
unsat:xs1_1667

abstract_term: (l1 (fail_1301 true (fun (x__1688:unit) -> (k_append_1119 x__1688)))): X
abstract_term: (fail_1301 true (fun (x__1688:unit) -> (k_append_1119 x__1688))): X
abstract_term: true: bool
abstract_term: (fun (x__1688:unit) -> (k_append_1119 x__1688)): (unit ->
X)
abst_arg: x__1688, unit
abst_arg: x__1688, unit
abstract_term: (k_append_1119 x__1688): X
abstract_term: x__1688: unit
filter
cond: (not (xs1_1667 = 0)); (not xs0_1652); true
orig pbs: xs1_1667 := false
pbs: 

ds:0, pbss:0
xs:0, qs:0
unsat:xs1_1667

filter
cond: (not (xs1_1667 = 0)); (not xs0_1652); true
orig pbs: xs1_1667 := false
pbs: 

ds:0, pbss:0
xs:0, qs:0
unsat:xs1_1667

filter
cond: (not (xs1_1667 = 0)); (not xs0_1652); true
orig pbs: xs1_1667 := false
pbs: 

ds:0, pbss:0
xs:0, qs:0
unsat:xs1_1667

filter
cond: (not xs0_1652); true
orig pbs: xs1_1667 := false
pbs: 

ds:0, pbss:0
xs:0, qs:0
unsat:xs1_1667

filter
cond: (not xs0_1652); true
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

fail_1301: ENV: b:bool, k:(unit -> X),


abst_arg: b, bool
abst_arg: k, (unit ->
X)
abst_arg: b, bool
abst_arg: k, (unit ->
X)
fail_1301: (k ()) ===> (k ())
fail_1301:: (k ())
abstract_term: (k ()): X
abstract_term: (): unit
filter
cond: true
orig pbs: 
unsat:false

ABST:
Main: main_1294
  main_1294 ->
      (append_1008 (fun k_main_1017_1670 -> (if rand_bool (l0 (k_main_1017_1670 true)) (l1 (k_main_1017_1670 false))))
        ()).
  append_1008 xs__ys_1009 k_append_1119 ->
      (xs__ys_1009
        (fun xs1_1653 ->
         (if rand_bool (l0 (append_1008 (fun k_append_1657 -> (k_append_1657 rand_bool)) k_append_1119))
           (l1
             (xs__ys_1009
               (fun xs1_1667 ->
                (if xs1_1667 _|_
                  (if rand_bool (l0 (if xs1_1667 _|_ k_append_1119))
                    (l1 (if xs1_1667 _|_ (fail_1301 (if xs1_1667 _|_ (if xs1_1667 _|_ k_append_1119))))))))))))).
  fail_1301 k -> {fail} => k.
Types:
  f_1300 : unit
  f_append_1295 : (unit -> (unit -> unit) -> unit)
  f_append_1296 : (unit -> unit)
  f_append_1297 : (unit -> unit)
  f_main_1017_1298 : (unit -> unit)
  f_main_1017_1299 : (unit -> unit)
  main_1017_1014 : (unit -> unit)
  main_1072 : (unit -> unit)
  mynot_1012 : (unit -> unit)
  xs_1102 : ((unit -> unit) -> unit -> unit)
  xs_1103 : ((unit -> unit) -> unit -> unit)

LIFT:
Main: main_1294
  main_1294 -> (append_1008 f_1691 ()).
  f_1691 k_main_1017_1670 -> (if rand_bool (l0 (k_main_1017_1670 true)) (l1 (k_main_1017_1670 false))).
  append_1008 xs__ys_1009 k_append_1119 -> (xs__ys_1009 (f_1695 xs__ys_1009 k_append_1119)).
  f_1695 xs__ys_1693 k_append_1694 xs1_1653 ->
      (if rand_bool (l0 (append_1008 f_1697 k_append_1694)) (l1 (xs__ys_1693 (f_1700 k_append_1694)))).
  f_1697 k_append_1657 -> (k_append_1657 rand_bool).
  f_1700 k_append_1699 xs1_1667 ->
      (if xs1_1667 _|_
        (if rand_bool (l0 (if xs1_1667 _|_ k_append_1699))
          (l1 (if xs1_1667 _|_ (fail_1301 (if xs1_1667 _|_ (if xs1_1667 _|_ k_append_1699))))))).
  fail_1301 k -> {fail} => k.

TRANS_EAGER:
Main: main_1294
  main_1294 -> (append_1008 f_1691 ()).
  f_1691 k_main_1017_1670 ->
      (let f_1702 b_1701 = (if b_1701 (l0 (k_main_1017_1670 true)) (l1 (k_main_1017_1670 false))) in
       (if rand_bool (f_1702 true) (f_1702 false))).
  append_1008 xs__ys_1009 k_append_1119 -> (xs__ys_1009 (f_1695 xs__ys_1009 k_append_1119)).
  f_1695 xs__ys_1693 k_append_1694 xs1_1653 ->
      (let f_1704 b_1703 =
       (if b_1703 (l0 (append_1008 f_1697 k_append_1694)) (l1 (xs__ys_1693 (f_1700 k_append_1694)))) in
       (if rand_bool (f_1704 true) (f_1704 false))).
  f_1697 k_append_1657 -> (if rand_bool (k_append_1657 true) (k_append_1657 false)).
  f_1700 k_append_1699 xs1_1667 ->
      (let f_1708 b_1707 =
       (if b_1707 _|_
         (let f_1710 b_1709 =
          (if b_1709 (l0 (let f_1712 b_1711 = (if b_1711 _|_ k_append_1699) in (f_1712 xs1_1667)))
            (l1
              (let f_1714 b_1713 =
               (if b_1713 _|_
                 (fail_1301
                   (let f_1716 b_1715 =
                    (if b_1715 _|_ (let f_1718 b_1717 = (if b_1717 _|_ k_append_1699) in (f_1718 xs1_1667))) in
                    (f_1716 xs1_1667))))
               in (f_1714 xs1_1667))))
          in (if rand_bool (f_1710 true) (f_1710 false))))
       in (f_1708 xs1_1667)).
  fail_1301 k -> {fail} => k.

PUT_INTO_IF:
Main: main_1294
  main_1294 -> (append_1008 f_1691 ()).
  f_1691 k_main_1017_1670 ->
      (let f_1702 b_1701 = (if b_1701 (l0 (k_main_1017_1670 true)) (l1 (k_main_1017_1670 false))) in
       (if rand_bool (f_1702 true) (f_1702 false))).
  append_1008 xs__ys_1009 k_append_1119 -> (xs__ys_1009 (f_1695 xs__ys_1009 k_append_1119)).
  f_1695 xs__ys_1693 k_append_1694 xs1_1653 ->
      (let f_1704 b_1703 =
       (if b_1703 (l0 (append_1008 f_1697 k_append_1694)) (l1 (xs__ys_1693 (f_1700 k_append_1694)))) in
       (if rand_bool (f_1704 true) (f_1704 false))).
  f_1697 k_append_1657 -> (if rand_bool (k_append_1657 true) (k_append_1657 false)).
  f_1700 k_append_1699 xs1_1667 ->
      (let f_1708 b_1707 =
       (if b_1707 _|_
         (let f_1710 b_1709 =
          (if b_1709 (l0 (let f_1712 b_1711 = (if b_1711 _|_ k_append_1699) in (f_1712 xs1_1667)))
            (l1
              (let f_1714 b_1713 =
               (if b_1713 _|_
                 (fail_1301
                   (let f_1716 b_1715 =
                    (if b_1715 _|_ (let f_1718 b_1717 = (if b_1717 _|_ k_append_1699) in (f_1718 xs1_1667))) in
                    (f_1716 xs1_1667))))
               in (f_1714 xs1_1667))))
          in (if rand_bool (f_1710 true) (f_1710 false))))
       in (f_1708 xs1_1667)).
  fail_1301 k -> {fail} => k.

DONE!

(1-2) Checking HORS ... DONE!

Error trace::
  main_1294 ... --> 
  main_1072 ... --> 
  main_1017_1014 ... --> 
  append_1008 ... --> 
  xs_1102 ... --> 
  f_main_1017_1298 ... --> 
  mynot_1012 [1/2] ... --> 
  f_main_1017_1299 ... --> 
  f_append_1295 [2/2] ... --> 
  xs_1103 ... --> 
  f_main_1017_1298 ... --> 
  mynot_1012 [2/2] ... --> 
  f_main_1017_1299 ... --> 
  f_append_1297 [2/2] ... --> 
  fail_1301 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 0; 1; 0

(1-3) Checking counterexample ... DONE!

(1-4) Discovering predicates ... 
DONE!

Prefix of spurious counterexample::
0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 0; 1

Program with abstraction types (CEGAR-cycle 2)::
Main: main_1294
  main_1294 -> (main_1072 f_1300).
  append_1008 xs__ys_1009 k_append_1119 -> (xs_1102 xs__ys_1009 (f_append_1295 k_append_1119 xs__ys_1009)).
  f_1300 main_1261 -> end.
  f_append_1295 k_append_1119 xs__ys_1009 xs0_1187 xs1_1187 when xs0_1187 ->
      (append_1008 (f_append_1296 xs0_1187 xs1_1187) k_append_1119).
  f_append_1295 k_append_1119 xs__ys_1009 xs0_1187 xs1_1187 when (not xs0_1187) ->
      (xs_1103 xs0_1187 xs1_1187 xs__ys_1009 (f_append_1297 xs0_1187 xs1_1187 k_append_1119)).
  f_append_1296 xs0_1187 xs1_1187 x_1279 k_append_1280 -> (k_append_1280 true 0).
  f_append_1297 xs0_1187 xs1_1187 k_append_1119 xs0_1182 xs1_1182 when (xs1_1182 = 0) -> (k_append_1119 ()).
  f_append_1297 xs0_1187 xs1_1187 k_append_1119 xs0_1182 xs1_1182 when (
      not (xs1_1182 = 0)) -> (fail_1301 true k_append_1119).
  f_main_1017_1298 x_1056 b_1284 k_main_1017_1285 ->
      (mynot_1012 b_1284 (f_main_1017_1299 b_1284 x_1056 k_main_1017_1285)).
  f_main_1017_1299 b_1284 x_1056 k_main_1017_1285 x_1292 -> (k_main_1017_1285 x_1292 0).
  fail_1301 b k -> {fail} => (k ()).
  main_1017_1014 x_1056 k_main_1017_1208 -> (append_1008 (f_main_1017_1298 x_1056) k_main_1017_1208).
  main_1072 k_main_1255 -> (main_1017_1014 () k_main_1255).
  mynot_1012 b_1013 k_mynot_1197 when b_1013 -> (k_mynot_1197 false).
  mynot_1012 b_1013 k_mynot_1197 when (not b_1013) -> (k_mynot_1197 true).
  xs_1102 xs__ys_1009 k_append_xs_1126 -> (xs__ys_1009 true k_append_xs_1126).
  xs_1103 xs0_1187 xs1_1187 xs__ys_1009 k_append_xs_1165 -> (xs__ys_1009 false k_append_xs_1165).
Types:
  main_1294 : X
  append_1008 : ((x_2:bool -> (bool -> x_5:int[x_5 = 0; x_2] -> X) -> X) -> (unit -> X) -> X)
  fail_1301 : (bool -> (unit -> X) -> X)

(2-1) Abstracting ... EXPAND_NONREC:
Main: main_1294
  main_1294 ->
      (append_1008
        (fun b_2002 k_main_1017_2003 -> (if b_2002 (l0 (k_main_1017_2003 false 0)) (l1 (k_main_1017_2003 true 0))))
        (fun main_1978 -> end)).
  append_1008 xs__ys_1009 k_append_1119 ->
      (xs__ys_1009 true
        (fun xs0_1985 xs1_1986 ->
         (if xs0_1985 (l0 (append_1008 (fun x_1989 k_append_1990 -> (k_append_1990 true 0)) k_append_1119))
           (l1
             (xs__ys_1009 false
               (fun xs0_1999 xs1_2000 ->
                (if (xs1_2000 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119))))))))).
  fail_1301 b k -> {fail} => (k ()).

ETA: (append_1008
       (fun b_2002 k_main_1017_2003 -> (if b_2002 (l0 (k_main_1017_2003 false 0)) (l1 (k_main_1017_2003 true 0))))
       (fun main_1978 -> end)): X
ETA: (fun main_1978 -> end): (unit ->
X)
ETA: end: X
ETA: (fun b_2002 k_main_1017_2003 -> (if b_2002 (l0 (k_main_1017_2003 false 0)) (l1 (k_main_1017_2003 true 0)))): (
x_1:bool -> (bool -> x_4:int[x_4 = 0; x_1] -> X) -> X)
ETA: (fun k_main_1017_2003 -> (if b_2002 (l0 (k_main_1017_2003 false 0)) (l1 (k_main_1017_2003 true 0)))): ((
bool -> x_3:int[x_3 = 0; b_2002] -> X) ->
X)
ETA: (if b_2002 (l0 (k_main_1017_2003 false 0)) (l1 (k_main_1017_2003 true 0))): X
ETA: (l1 (k_main_1017_2003 true 0)): X
ETA: (k_main_1017_2003 true 0): X
ETA: 0: x_1:int[x_1 = 0; b_2002]
ETA: true: bool
ETA_AUX: (k_main_1017_2003 true 0): X
ETA: (l0 (k_main_1017_2003 false 0)): X
ETA: (k_main_1017_2003 false 0): X
ETA: 0: x_1:int[x_1 = 0; b_2002]
ETA: false: bool
ETA_AUX: (k_main_1017_2003 false 0): X
ETA_AUX: (append_1008
           (fun (b_2002:bool) (k_main_1017_2003:(bool -> x_2:int[x_2 = 0; b_2002] -> X)) ->
            (if b_2002 (l0 (k_main_1017_2003 false 0)) (l1 (k_main_1017_2003 true 0)))) (
           fun (main_1978:unit) -> end)): X
ETA: (xs__ys_1009 true
       (fun xs0_1985 xs1_1986 ->
        (if xs0_1985 (l0 (append_1008 (fun x_1989 k_append_1990 -> (k_append_1990 true 0)) k_append_1119))
          (l1
            (xs__ys_1009 false
              (fun xs0_1999 xs1_2000 -> (if (xs1_2000 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119))))))))): X
ETA: (fun xs0_1985 xs1_1986 ->
      (if xs0_1985 (l0 (append_1008 (fun x_1989 k_append_1990 -> (k_append_1990 true 0)) k_append_1119))
        (l1
          (xs__ys_1009 false
            (fun xs0_1999 xs1_2000 -> (if (xs1_2000 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119)))))))): (
bool -> x_2:int[x_2 = 0; true] -> X)
ETA: (fun xs1_1986 ->
      (if xs0_1985 (l0 (append_1008 (fun x_1989 k_append_1990 -> (k_append_1990 true 0)) k_append_1119))
        (l1
          (xs__ys_1009 false
            (fun xs0_1999 xs1_2000 -> (if (xs1_2000 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119)))))))): (x_1:int[
x_1 = 0; true] ->
X)
ETA: (if xs0_1985 (l0 (append_1008 (fun x_1989 k_append_1990 -> (k_append_1990 true 0)) k_append_1119))
       (l1
         (xs__ys_1009 false
           (fun xs0_1999 xs1_2000 -> (if (xs1_2000 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119))))))): X
ETA: (l1
       (xs__ys_1009 false
         (fun xs0_1999 xs1_2000 -> (if (xs1_2000 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119)))))): X
ETA: (xs__ys_1009 false
       (fun xs0_1999 xs1_2000 -> (if (xs1_2000 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119))))): X
ETA: (fun xs0_1999 xs1_2000 -> (if (xs1_2000 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119)))): (
bool -> x_2:int[x_2 = 0; false] -> X)
ETA: (fun xs1_2000 -> (if (xs1_2000 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119)))): (x_1:int[
x_1 = 0; false] ->
X)
ETA: (if (xs1_2000 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119))): X
ETA: (l1 (fail_1301 true k_append_1119)): X
ETA: (fail_1301 true k_append_1119): X
ETA: k_append_1119: (unit ->
X)
ETA_AUX: k_append_1119: (unit ->
X)
ETA_AUX: x__2021: unit
ETA_AUX: (k_append_1119 x__2021): X
ETA: true: bool
ETA_AUX: (fail_1301 true (fun (x__2021:unit) -> (k_append_1119 x__2021))): X
ETA: (l0 (k_append_1119 ())): X
ETA: (k_append_1119 ()): X
ETA: (): unit
ETA_AUX: (k_append_1119 ()): X
ETA: false: bool
ETA_AUX: (xs__ys_1009 false
           (fun (xs0_1999:bool) (xs1_2000:x_1:int[x_1 = 0; false]) ->
            (if (xs1_2000 = 0) (l0 (k_append_1119 ()))
              (l1 (fail_1301 true (fun (x__2021:unit) -> (k_append_1119 x__2021))))))): X
ETA: (l0 (append_1008 (fun x_1989 k_append_1990 -> (k_append_1990 true 0)) k_append_1119)): X
ETA: (append_1008 (fun x_1989 k_append_1990 -> (k_append_1990 true 0)) k_append_1119): X
ETA: k_append_1119: (unit ->
X)
ETA_AUX: k_append_1119: (unit ->
X)
ETA_AUX: x__2022: unit
ETA_AUX: (k_append_1119 x__2022): X
ETA: (fun x_1989 k_append_1990 -> (k_append_1990 true 0)): (x_1:bool -> (bool -> x_4:int[x_4 = 0; x_1] -> X) -> X)
ETA: (fun k_append_1990 -> (k_append_1990 true 0)): ((bool -> x_3:int[x_3 = 0; x_1989] -> X) ->
X)
ETA: (k_append_1990 true 0): X
ETA: 0: x_1:int[x_1 = 0; x_1989]
ETA: true: bool
ETA_AUX: (k_append_1990 true 0): X
ETA_AUX: (append_1008
           (fun (x_1989:bool) (k_append_1990:(bool -> x_2:int[x_2 = 0; x_1989] -> X)) -> (k_append_1990 true 0))
           (fun (x__2022:unit) -> (k_append_1119 x__2022))): X
ETA: true: bool
ETA_AUX: (xs__ys_1009 true
           (fun (xs0_1985:bool) (xs1_1986:x_1:int[x_1 = 0; true]) ->
            (if xs0_1985
              (l0
                (append_1008
                  (fun (x_1989:bool) (k_append_1990:(bool -> x_2:int[x_2 = 0; x_1989] -> X)) -> (k_append_1990 true 0))
                  (fun (x__2022:unit) -> (k_append_1119 x__2022))))
              (l1
                (xs__ys_1009 false
                  (fun (xs0_1999:bool) (xs1_2000:x_1:int[x_1 = 0; false]) ->
                   (if (xs1_2000 = 0) (l0 (k_append_1119 ()))
                     (l1 (fail_1301 true (fun (x__2021:unit) -> (k_append_1119 x__2021))))))))))): X
ETA: (k ()): X
ETA: (): unit
ETA_AUX: (k ()): X
ETA_EXPAND:
Main: main_1294
  main_1294 ->
      (append_1008
        (fun (b_2002:bool) (k_main_1017_2003:(bool -> x_2:int[x_2 = 0; b_2002] -> X)) ->
         (if b_2002 (l0 (k_main_1017_2003 false 0)) (l1 (k_main_1017_2003 true 0)))) (
        fun (main_1978:unit) -> end)).
  append_1008 xs__ys_1009 k_append_1119 ->
      (xs__ys_1009 true
        (fun (xs0_1985:bool) (xs1_1986:x_1:int[x_1 = 0; true]) ->
         (if xs0_1985
           (l0
             (append_1008
               (fun (x_1989:bool) (k_append_1990:(bool -> x_2:int[x_2 = 0; x_1989] -> X)) -> (k_append_1990 true 0))
               (fun (x__2022:unit) -> (k_append_1119 x__2022))))
           (l1
             (xs__ys_1009 false
               (fun (xs0_1999:bool) (xs1_2000:x_1:int[x_1 = 0; false]) ->
                (if (xs1_2000 = 0) (l0 (k_append_1119 ()))
                  (l1 (fail_1301 true (fun (x__2021:unit) -> (k_append_1119 x__2021))))))))))).
  fail_1301 b k -> {fail} => (k ()).

main_1294: ENV: 

main_1294: (append_1008
             (fun (b_2002:bool) (k_main_1017_2003:(bool -> x_2:int[x_2 = 0; b_2002] -> X)) ->
              (if b_2002 (l0 (k_main_1017_2003 false 0)) (l1 (k_main_1017_2003 true 0)))) (
             fun (main_1978:unit) -> end)) ===> (append_1008
                                                  (fun (b_2002:bool) 
                                                       (k_main_1017_2003:(
                                                       bool -> x_2:int[x_2 = 0; b_2002] -> X))
                                                   ->
                                                   (if b_2002 (l0 (k_main_1017_2003 false 0))
                                                     (l1 (k_main_1017_2003 true 0)))) (
                                                  fun (main_1978:unit) -> end))
main_1294:: (append_1008
              (fun (b_2002:bool) (k_main_1017_2003:(bool -> x_2:int[x_2 = 0; b_2002] -> X)) ->
               (if b_2002 (l0 (k_main_1017_2003 false 0)) (l1 (k_main_1017_2003 true 0))))
              (fun (main_1978:unit) -> end))
abstract_term: (append_1008
                 (fun (b_2002:bool) (k_main_1017_2003:(bool -> x_2:int[x_2 = 0; b_2002] -> X)) ->
                  (if b_2002 (l0 (k_main_1017_2003 false 0)) (l1 (k_main_1017_2003 true 0))))
                 (fun (main_1978:unit) -> end)): X
abstract_term: (fun (b_2002:bool) (k_main_1017_2003:(bool -> x_2:int[x_2 = 0; b_2002] -> X)) ->
                (if b_2002 (l0 (k_main_1017_2003 false 0)) (l1 (k_main_1017_2003 true 0)))): (
x_1:bool -> (bool -> x_4:int[x_4 = 0; x_1] -> X) -> X)
abst_arg: b_2002, bool
abst_arg: k_main_1017_2003, (bool -> x_2:int[x_2 = 0; b_2002] -> X)
abst_arg: b_2002, bool
abst_arg: k_main_1017_2003, (bool -> x_2:int[x_2 = 0; b_2002] -> X)
abstract_term: (if b_2002 (l0 (k_main_1017_2003 false 0)) (l1 (k_main_1017_2003 true 0))): X
abstract_term: b_2002: x_1:bool[x_1]
cond: true
pbs: 
p:b_2002
ds:0, pbss:0
xs:0, qs:0
tt:false
ff:false

abstract_term: (l0 (k_main_1017_2003 false 0)): X
abstract_term: (k_main_1017_2003 false 0): X
abstract_term: false: bool
abstract_term: 0: x_1:int[x_1 = 0; b_2002]
cond: b_2002; true
pbs: 
p:(0 = 0)
tt:true
ff:false

cond: b_2002; true
pbs: 
p:b_2002
tt:true
ff:false

filter
cond: b_2002; true
orig pbs: 
unsat:false

abstract_term: (l1 (k_main_1017_2003 true 0)): X
abstract_term: (k_main_1017_2003 true 0): X
abstract_term: true: bool
abstract_term: 0: x_1:int[x_1 = 0; b_2002]
cond: (not b_2002); true
pbs: 
p:(0 = 0)
tt:true
ff:false

cond: (not b_2002); true
pbs: 
p:b_2002
tt:false
ff:true

filter
cond: (not b_2002); true
orig pbs: 
unsat:false

filter
cond: true
orig pbs: 
unsat:false

abstract_term: (fun (main_1978:unit) -> end): (unit ->
X)
abst_arg: main_1978, unit
abst_arg: main_1978, unit
abstract_term: end: X
filter
cond: true
orig pbs: 
unsat:false

filter
cond: true
orig pbs: 
unsat:false

append_1008: ENV: xs__ys_1009:(x_1:bool -> (bool -> x_4:int[x_4 = 0; x_1] -> X) -> X), k_append_1119:(unit -> X),


abst_arg: xs__ys_1009, (x_1:bool -> (bool -> x_4:int[x_4 = 0; x_1] -> X) -> X)
abst_arg: k_append_1119, (unit ->
X)
abst_arg: xs__ys_1009, (x_1:bool -> (bool -> x_4:int[x_4 = 0; x_1] -> X) -> X)
abst_arg: k_append_1119, (unit ->
X)
append_1008: (xs__ys_1009 true
               (fun (xs0_1985:bool) (xs1_1986:x_1:int[x_1 = 0; true]) ->
                (if xs0_1985
                  (l0
                    (append_1008
                      (fun (x_1989:bool) (k_append_1990:(bool -> x_2:int[x_2 = 0; x_1989] -> X)) ->
                       (k_append_1990 true 0)) (fun (x__2022:unit) -> (k_append_1119 x__2022))))
                  (l1
                    (xs__ys_1009 false
                      (fun (xs0_1999:bool) (xs1_2000:x_1:int[x_1 = 0; false]) ->
                       (if (xs1_2000 = 0) (l0 (k_append_1119 ()))
                         (l1 (fail_1301 true (fun (x__2021:unit) -> (k_append_1119 x__2021))))))))))) ===> (
xs__ys_1009 true
 (fun (xs0_1985:bool) (xs1_1986:x_1:int[x_1 = 0; true]) ->
  (if xs0_1985
    (l0
      (append_1008
        (fun (x_1989:bool) (k_append_1990:(bool -> x_2:int[x_2 = 0; x_1989] -> X)) -> (k_append_1990 true 0))
        (fun (x__2022:unit) -> (k_append_1119 x__2022))))
    (l1
      (xs__ys_1009 false
        (fun (xs0_1999:bool) (xs1_2000:x_1:int[x_1 = 0; false]) ->
         (if (xs1_2000 = 0) (l0 (k_append_1119 ()))
           (l1 (fail_1301 true (fun (x__2021:unit) -> (k_append_1119 x__2021)))))))))))
append_1008:: (xs__ys_1009 true
                (fun (xs0_1985:bool) (xs1_1986:x_1:int[x_1 = 0; true]) ->
                 (if xs0_1985
                   (l0
                     (append_1008
                       (fun (x_1989:bool) (k_append_1990:(bool -> x_2:int[x_2 = 0; x_1989] -> X)) ->
                        (k_append_1990 true 0)) (fun (x__2022:unit) -> (k_append_1119 x__2022))))
                   (l1
                     (xs__ys_1009 false
                       (fun (xs0_1999:bool) (xs1_2000:x_1:int[x_1 = 0; false]) ->
                        (if (xs1_2000 = 0) (l0 (k_append_1119 ()))
                          (l1 (fail_1301 true (fun (x__2021:unit) -> (k_append_1119 x__2021)))))))))))
abstract_term: (xs__ys_1009 true
                 (fun (xs0_1985:bool) (xs1_1986:x_1:int[x_1 = 0; true]) ->
                  (if xs0_1985
                    (l0
                      (append_1008
                        (fun (x_1989:bool) (k_append_1990:(bool -> x_2:int[x_2 = 0; x_1989] -> X)) ->
                         (k_append_1990 true 0)) (fun (x__2022:unit) -> (k_append_1119 x__2022))))
                    (l1
                      (xs__ys_1009 false
                        (fun (xs0_1999:bool) (xs1_2000:x_1:int[x_1 = 0; false]) ->
                         (if (xs1_2000 = 0) (l0 (k_append_1119 ()))
                           (l1 (fail_1301 true (fun (x__2021:unit) -> (k_append_1119 x__2021))))))))))): X
abstract_term: true: bool
abstract_term: (fun (xs0_1985:bool) (xs1_1986:x_1:int[x_1 = 0; true]) ->
                (if xs0_1985
                  (l0
                    (append_1008
                      (fun (x_1989:bool) (k_append_1990:(bool -> x_2:int[x_2 = 0; x_1989] -> X)) ->
                       (k_append_1990 true 0)) (fun (x__2022:unit) -> (k_append_1119 x__2022))))
                  (l1
                    (xs__ys_1009 false
                      (fun (xs0_1999:bool) (xs1_2000:x_1:int[x_1 = 0; false]) ->
                       (if (xs1_2000 = 0) (l0 (k_append_1119 ()))
                         (l1 (fail_1301 true (fun (x__2021:unit) -> (k_append_1119 x__2021)))))))))): (
bool -> x_2:int[x_2 = 0; true] -> X)
abst_arg: xs0_1985, bool
abst_arg: xs1_1986, x_1:int[x_1 = 0; true]
abst_arg: xs0_1985, bool
abst_arg: xs1_1986, x_1:int[x_1 = 0; true]
abstract_term: (if xs0_1985
                 (l0
                   (append_1008
                     (fun (x_1989:bool) (k_append_1990:(bool -> x_2:int[x_2 = 0; x_1989] -> X)) ->
                      (k_append_1990 true 0)) (fun (x__2022:unit) -> (k_append_1119 x__2022))))
                 (l1
                   (xs__ys_1009 false
                     (fun (xs0_1999:bool) (xs1_2000:x_1:int[x_1 = 0; false]) ->
                      (if (xs1_2000 = 0) (l0 (k_append_1119 ()))
                        (l1 (fail_1301 true (fun (x__2021:unit) -> (k_append_1119 x__2021))))))))): X
abstract_term: xs0_1985: x_1:bool[x_1]
cond: true
pbs: xs11_1986 := (xs1_1986 = 0)
p:xs0_1985
ds:0, pbss:0
xs:0, qs:0
tt:false
ff:false

abstract_term: (l0
                 (append_1008
                   (fun (x_1989:bool) (k_append_1990:(bool -> x_2:int[x_2 = 0; x_1989] -> X)) -> (k_append_1990 true 0))
                   (fun (x__2022:unit) -> (k_append_1119 x__2022)))): X
abstract_term: (append_1008
                 (fun (x_1989:bool) (k_append_1990:(bool -> x_2:int[x_2 = 0; x_1989] -> X)) -> (k_append_1990 true 0))
                 (fun (x__2022:unit) -> (k_append_1119 x__2022))): X
abstract_term: (fun (x_1989:bool) (k_append_1990:(bool -> x_2:int[x_2 = 0; x_1989] -> X)) -> (k_append_1990 true 0)): (
x_1:bool -> (bool -> x_4:int[x_4 = 0; x_1] -> X) -> X)
abst_arg: x_1989, bool
abst_arg: k_append_1990, (bool -> x_2:int[x_2 = 0; x_1989] -> X)
abst_arg: x_1989, bool
abst_arg: k_append_1990, (bool -> x_2:int[x_2 = 0; x_1989] -> X)
abstract_term: (k_append_1990 true 0): X
abstract_term: true: bool
abstract_term: 0: x_1:int[x_1 = 0; x_1989]
cond: xs0_1985; true
pbs: xs11_1986 := (xs1_1986 = 0)
p:(0 = 0)
tt:true
ff:false

cond: xs0_1985; true
pbs: xs11_1986 := (xs1_1986 = 0)
p:x_1989
ds:0, pbss:0
xs:0, qs:0
tt:false
ff:false

filter
cond: xs0_1985; true
orig pbs: xs11_1986 := (xs1_1986 = 0)
pbs: xs11_1986 := (xs1_1986 = 0)

ds:1, pbss:1
xs:0, qs:1
unsat:false

filter
cond: xs0_1985; true
orig pbs: xs11_1986 := (xs1_1986 = 0)
pbs: xs11_1986 := (xs1_1986 = 0)

ds:1, pbss:1
xs:0, qs:1
unsat:false

abstract_term: (fun (x__2022:unit) -> (k_append_1119 x__2022)): (unit ->
X)
abst_arg: x__2022, unit
abst_arg: x__2022, unit
abstract_term: (k_append_1119 x__2022): X
abstract_term: x__2022: unit
filter
cond: xs0_1985; true
orig pbs: xs11_1986 := (xs1_1986 = 0)
pbs: xs11_1986 := (xs1_1986 = 0)

ds:1, pbss:1
xs:0, qs:1
unsat:false

filter
cond: xs0_1985; true
orig pbs: xs11_1986 := (xs1_1986 = 0)
pbs: xs11_1986 := (xs1_1986 = 0)

ds:1, pbss:1
xs:0, qs:1
unsat:false

filter
cond: xs0_1985; true
orig pbs: xs11_1986 := (xs1_1986 = 0)
pbs: xs11_1986 := (xs1_1986 = 0)

ds:1, pbss:1
xs:0, qs:1
unsat:false

abstract_term: (l1
                 (xs__ys_1009 false
                   (fun (xs0_1999:bool) (xs1_2000:x_1:int[x_1 = 0; false]) ->
                    (if (xs1_2000 = 0) (l0 (k_append_1119 ()))
                      (l1 (fail_1301 true (fun (x__2021:unit) -> (k_append_1119 x__2021)))))))): X
abstract_term: (xs__ys_1009 false
                 (fun (xs0_1999:bool) (xs1_2000:x_1:int[x_1 = 0; false]) ->
                  (if (xs1_2000 = 0) (l0 (k_append_1119 ()))
                    (l1 (fail_1301 true (fun (x__2021:unit) -> (k_append_1119 x__2021))))))): X
abstract_term: false: bool
abstract_term: (fun (xs0_1999:bool) (xs1_2000:x_1:int[x_1 = 0; false]) ->
                (if (xs1_2000 = 0) (l0 (k_append_1119 ()))
                  (l1 (fail_1301 true (fun (x__2021:unit) -> (k_append_1119 x__2021)))))): (
bool -> x_2:int[x_2 = 0; false] -> X)
abst_arg: xs0_1999, bool
abst_arg: xs1_2000, x_1:int[x_1 = 0; false]
abst_arg: xs0_1999, bool
abst_arg: xs1_2000, x_1:int[x_1 = 0; false]
abstract_term: (if (xs1_2000 = 0) (l0 (k_append_1119 ()))
                 (l1 (fail_1301 true (fun (x__2021:unit) -> (k_append_1119 x__2021))))): X
abstract_term: (xs1_2000 = 0): x_1:bool[x_1]
cond: (not xs0_1985); true
pbs: xs11_2000 := (xs1_2000 = 0);
     xs11_1986 := (xs1_1986 = 0)
p:(xs1_2000 = 0)
ds:1, pbss:1
xs:1, qs:0
tt:xs11_2000
ff:false

abstract_term: (l0 (k_append_1119 ())): X
abstract_term: (k_append_1119 ()): X
abstract_term: (): unit
filter
cond: (xs1_2000 = 0); (not xs0_1985); true
orig pbs: xs12_2000 := false;
          xs11_2000 := (xs1_2000 = 0);
          xs11_1986 := (xs1_1986 = 0)
pbs: xs11_1986 := (xs1_1986 = 0)

ds:1, pbss:1
xs:0, qs:1
pbs: xs11_2000 := (xs1_2000 = 0)

ds:1, pbss:1
xs:0, qs:1
pbs: 

ds:0, pbss:0
xs:0, qs:0
unsat:xs12_2000

abstract_term: (l1 (fail_1301 true (fun (x__2021:unit) -> (k_append_1119 x__2021)))): X
abstract_term: (fail_1301 true (fun (x__2021:unit) -> (k_append_1119 x__2021))): X
abstract_term: true: bool
abstract_term: (fun (x__2021:unit) -> (k_append_1119 x__2021)): (unit ->
X)
abst_arg: x__2021, unit
abst_arg: x__2021, unit
abstract_term: (k_append_1119 x__2021): X
abstract_term: x__2021: unit
filter
cond: (not (xs1_2000 = 0)); (not xs0_1985); true
orig pbs: xs12_2000 := false;
          xs11_2000 := (xs1_2000 = 0);
          xs11_1986 := (xs1_1986 = 0)
pbs: xs11_1986 := (xs1_1986 = 0)

ds:1, pbss:1
xs:0, qs:1
pbs: xs11_2000 := (xs1_2000 = 0)

ds:1, pbss:1
xs:1, qs:0
pbs: 

ds:0, pbss:0
xs:0, qs:0
unsat:((xs12_2000 || xs11_2000) || xs11_2000)

filter
cond: (not (xs1_2000 = 0)); (not xs0_1985); true
orig pbs: xs12_2000 := false;
          xs11_2000 := (xs1_2000 = 0);
          xs11_1986 := (xs1_1986 = 0)
pbs: xs11_1986 := (xs1_1986 = 0)

ds:1, pbss:1
xs:0, qs:1
pbs: xs11_2000 := (xs1_2000 = 0)

ds:1, pbss:1
xs:1, qs:0
pbs: 

ds:0, pbss:0
xs:0, qs:0
unsat:((xs12_2000 || xs11_2000) || xs11_2000)

filter
cond: (not (xs1_2000 = 0)); (not xs0_1985); true
orig pbs: xs12_2000 := false;
          xs11_2000 := (xs1_2000 = 0);
          xs11_1986 := (xs1_1986 = 0)
pbs: xs11_1986 := (xs1_1986 = 0)

ds:1, pbss:1
xs:0, qs:1
pbs: xs11_2000 := (xs1_2000 = 0)

ds:1, pbss:1
xs:1, qs:0
pbs: 

ds:0, pbss:0
xs:0, qs:0
unsat:((xs12_2000 || xs11_2000) || xs11_2000)

filter
cond: (not xs0_1985); true
orig pbs: xs12_2000 := false;
          xs11_2000 := (xs1_2000 = 0);
          xs11_1986 := (xs1_1986 = 0)
pbs: xs11_1986 := (xs1_1986 = 0)

ds:1, pbss:1
xs:0, qs:1
pbs: xs11_2000 := (xs1_2000 = 0)

ds:1, pbss:1
xs:0, qs:1
pbs: 

ds:0, pbss:0
xs:0, qs:0
unsat:xs12_2000

filter
cond: (not xs0_1985); true
orig pbs: xs11_1986 := (xs1_1986 = 0)
pbs: xs11_1986 := (xs1_1986 = 0)

ds:1, pbss:1
xs:0, qs:1
unsat:false

filter
cond: true
orig pbs: xs11_1986 := (xs1_1986 = 0)
pbs: xs11_1986 := (xs1_1986 = 0)

ds:1, pbss:1
xs:0, qs:1
unsat:false

filter
cond: true
orig pbs: 
unsat:false

fail_1301: ENV: b:bool, k:(unit -> X),


abst_arg: b, bool
abst_arg: k, (unit ->
X)
abst_arg: b, bool
abst_arg: k, (unit ->
X)
fail_1301: (k ()) ===> (k ())
fail_1301:: (k ())
abstract_term: (k ()): X
abstract_term: (): unit
filter
cond: true
orig pbs: 
unsat:false

ABST:
Main: main_1294
  main_1294 ->
      (append_1008
        (fun k_main_1017_2003 -> (if rand_bool (l0 (k_main_1017_2003 true true)) (l1 (k_main_1017_2003 true false))))
        ()).
  append_1008 xs__ys_1009 k_append_1119 ->
      (xs__ys_1009
        (fun xs11_1986 xs12_1986 ->
         (if rand_bool (l0 (append_1008 (fun k_append_1990 -> (k_append_1990 true rand_bool)) k_append_1119))
           (l1
             (xs__ys_1009
               (fun xs11_2000 xs12_2000 ->
                (if xs12_2000 _|_
                  (if (if xs11_2000 true rand_bool) (l0 (if xs12_2000 _|_ k_append_1119))
                    (l1
                      (if ((xs12_2000 || xs11_2000) || xs11_2000) _|_
                        (fail_1301
                          (if ((xs12_2000 || xs11_2000) || xs11_2000) _|_
                            (if ((xs12_2000 || xs11_2000) || xs11_2000) _|_ k_append_1119))))))))))))).
  fail_1301 k -> {fail} => k.
Types:
  f_1300 : unit
  f_append_1295 : (unit -> (unit -> unit) -> unit)
  f_append_1296 : (unit -> unit)
  f_append_1297 : (unit -> unit)
  f_main_1017_1298 : (unit -> unit)
  f_main_1017_1299 : (unit -> unit)
  main_1017_1014 : (unit -> unit)
  main_1072 : (unit -> unit)
  mynot_1012 : (unit -> unit)
  xs_1102 : ((unit -> unit) -> unit -> unit)
  xs_1103 : ((unit -> unit) -> unit -> unit)

LIFT:
Main: main_1294
  main_1294 -> (append_1008 f_2024 ()).
  f_2024 k_main_1017_2003 -> (if rand_bool (l0 (k_main_1017_2003 true true)) (l1 (k_main_1017_2003 true false))).
  append_1008 xs__ys_1009 k_append_1119 -> (xs__ys_1009 (f_2028 xs__ys_1009 k_append_1119)).
  f_2028 xs__ys_2026 k_append_2027 xs11_1986 xs12_1986 ->
      (if rand_bool (l0 (append_1008 f_2030 k_append_2027)) (l1 (xs__ys_2026 (f_2033 k_append_2027)))).
  f_2030 k_append_1990 -> (k_append_1990 true rand_bool).
  f_2033 k_append_2032 xs11_2000 xs12_2000 ->
      (if xs12_2000 _|_
        (if (if xs11_2000 true rand_bool) (l0 (if xs12_2000 _|_ k_append_2032))
          (l1
            (if ((xs12_2000 || xs11_2000) || xs11_2000) _|_
              (fail_1301
                (if ((xs12_2000 || xs11_2000) || xs11_2000) _|_
                  (if ((xs12_2000 || xs11_2000) || xs11_2000) _|_ k_append_2032))))))).
  fail_1301 k -> {fail} => k.

TRANS_EAGER:
Main: main_1294
  main_1294 -> (append_1008 f_2024 ()).
  f_2024 k_main_1017_2003 ->
      (let f_2035 b_2034 = (if b_2034 (l0 (k_main_1017_2003 true true)) (l1 (k_main_1017_2003 true false))) in
       (if rand_bool (f_2035 true) (f_2035 false))).
  append_1008 xs__ys_1009 k_append_1119 -> (xs__ys_1009 (f_2028 xs__ys_1009 k_append_1119)).
  f_2028 xs__ys_2026 k_append_2027 xs11_1986 xs12_1986 ->
      (let f_2037 b_2036 =
       (if b_2036 (l0 (append_1008 f_2030 k_append_2027)) (l1 (xs__ys_2026 (f_2033 k_append_2027)))) in
       (if rand_bool (f_2037 true) (f_2037 false))).
  f_2030 k_append_1990 ->
      (let f_2039 b_2038 = (k_append_1990 true b_2038) in (if rand_bool (f_2039 true) (f_2039 false))).
  f_2033 k_append_2032 xs11_2000 xs12_2000 ->
      (let f_2041 b_2040 =
       (if b_2040 _|_
         (let f_2043 b_2042 =
          (if b_2042 (l0 (let f_2045 b_2044 = (if b_2044 _|_ k_append_2032) in (f_2045 xs12_2000)))
            (l1
              (let f_2047 b_2046 =
               (if b_2046 _|_
                 (fail_1301
                   (let f_2049 b_2048 =
                    (if b_2048 _|_
                      (let f_2051 b_2050 = (if b_2050 _|_ k_append_2032) in
                       (let f_2053 b_2052 = (if b_2052 (f_2051 true) (f_2051 xs11_2000)) in
                        (let f_2055 b_2054 = (if b_2054 (f_2053 true) (f_2053 xs11_2000)) in (f_2055 xs12_2000)))))
                    in
                    (let f_2057 b_2056 = (if b_2056 (f_2049 true) (f_2049 xs11_2000)) in
                     (let f_2059 b_2058 = (if b_2058 (f_2057 true) (f_2057 xs11_2000)) in (f_2059 xs12_2000))))))
               in
               (let f_2061 b_2060 = (if b_2060 (f_2047 true) (f_2047 xs11_2000)) in
                (let f_2063 b_2062 = (if b_2062 (f_2061 true) (f_2061 xs11_2000)) in (f_2063 xs12_2000))))))
          in
          (let f_2065 b_2064 = (if b_2064 (f_2043 true) (if rand_bool (f_2043 true) (f_2043 false))) in
           (f_2065 xs11_2000))))
       in (f_2041 xs12_2000)).
  fail_1301 k -> {fail} => k.

PUT_INTO_IF:
Main: main_1294
  main_1294 -> (append_1008 f_2024 ()).
  f_2024 k_main_1017_2003 ->
      (let f_2035 b_2034 = (if b_2034 (l0 (k_main_1017_2003 true true)) (l1 (k_main_1017_2003 true false))) in
       (if rand_bool (f_2035 true) (f_2035 false))).
  append_1008 xs__ys_1009 k_append_1119 -> (xs__ys_1009 (f_2028 xs__ys_1009 k_append_1119)).
  f_2028 xs__ys_2026 k_append_2027 xs11_1986 xs12_1986 ->
      (let f_2037 b_2036 =
       (if b_2036 (l0 (append_1008 f_2030 k_append_2027)) (l1 (xs__ys_2026 (f_2033 k_append_2027)))) in
       (if rand_bool (f_2037 true) (f_2037 false))).
  f_2030 k_append_1990 ->
      (let f_2039 b_2038 = (k_append_1990 true b_2038) in (if rand_bool (f_2039 true) (f_2039 false))).
  f_2033 k_append_2032 xs11_2000 xs12_2000 ->
      (let f_2041 b_2040 =
       (if b_2040 _|_
         (let f_2043 b_2042 =
          (if b_2042 (l0 (let f_2045 b_2044 = (if b_2044 _|_ k_append_2032) in (f_2045 xs12_2000)))
            (l1
              (let f_2047 b_2046 =
               (if b_2046 _|_
                 (fail_1301
                   (let f_2049 b_2048 =
                    (if b_2048 _|_
                      (let f_2051 b_2050 = (if b_2050 _|_ k_append_2032) in
                       (let f_2053 b_2052 = (if b_2052 (f_2051 true) (f_2051 xs11_2000)) in
                        (let f_2055 b_2054 = (if b_2054 (f_2053 true) (f_2053 xs11_2000)) in (f_2055 xs12_2000)))))
                    in
                    (let f_2057 b_2056 = (if b_2056 (f_2049 true) (f_2049 xs11_2000)) in
                     (let f_2059 b_2058 = (if b_2058 (f_2057 true) (f_2057 xs11_2000)) in (f_2059 xs12_2000))))))
               in
               (let f_2061 b_2060 = (if b_2060 (f_2047 true) (f_2047 xs11_2000)) in
                (let f_2063 b_2062 = (if b_2062 (f_2061 true) (f_2061 xs11_2000)) in (f_2063 xs12_2000))))))
          in
          (let f_2065 b_2064 = (if b_2064 (f_2043 true) (if rand_bool (f_2043 true) (f_2043 false))) in
           (f_2065 xs11_2000))))
       in (f_2041 xs12_2000)).
  fail_1301 k -> {fail} => k.

DONE!

(2-2) Checking HORS ... DONE!

Some refinement types cannot be shown (unimplemented)

Safe!

cycles: 2
total: 0.219 sec
  abst: 0.029 sec
  mc: 0.016 sec
  refine: 0.084 sec
    exparam: 0.042 sec
