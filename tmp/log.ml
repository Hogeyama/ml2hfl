MoCHi: Model Checker for Higher-Order Programs
  Build: _3fd8589 (after 2014-08-05 12:01:14 +0900)
  FPAT version: d0f562a
  TRecS version: 1.33
  OCaml version: 4.01.0
  Command: ./mochi.opt test_relabs/sum.ml -disable-rc -color -list-option -fpat -hccs 1 -bool-init-empty -abs-filter 
           -tupling -debug-module Tupling

parsed:
 let rec sum_1008 n_1009 = if n_1009 <= 0 then
                             0
                           else
                             let s_1010 = sum_1008 (n_1009 - 1) in
                             n_1009 + s_1010 in
 let rec sum_acc_1011 n__a_1024 =
   match n__a_1024 with
   | (n_1012, a_1013) -> if n_1012 <= 0 then
                           a_1013
                         else
                           sum_acc_1011 (n_1012 - 1, n_1012 + a_1013)
 in
 let main_1014 n_1015 a_1016 =
   let s1_1017 = sum_1008 n_1015 in
   let s2_1018 = sum_acc_1011 (n_1015, a_1016) in
   if a_1016 + s1_1017 = s2_1018 then
     ()
   else
     {fail} ()
 in
 ()

set_target:
 let rec sum_1008 (n_1009:int) = if n_1009 <= 0 then
                                   0
                                 else
                                   let s_1010 = sum_1008 (n_1009 - 1) in
                                   n_1009 + s_1010 in
 let rec sum_acc_1011 (n__a_1024:(int * int)) =
   match n__a_1024 with
   | (n_1012, a_1013) -> if n_1012 <= 0 then
                           a_1013
                         else
                           sum_acc_1011 (n_1012 - 1, n_1012 + a_1013)
 in
 let main_1014 (n_1015:int) (a_1016:int) =
   let s1_1017 = sum_1008 n_1015 in
   let s2_1018 = sum_acc_1011 (n_1015, a_1016) in
   if a_1016 + s1_1017 = s2_1018 then
     ()
   else
     {fail} ()
 in
 let main_1062 = let arg1_1058 = rand_int () in
                 let arg2_1060 = rand_int () in
                 main_1014 arg1_1058 arg2_1060 in
 ()

make_fun_tuple:
 let rec sum_1008 (n_1009:int) = if n_1009 <= 0 then
                                   0
                                 else
                                   let s_1010 = sum_1008 (n_1009 - 1) in
                                   n_1009 + s_1010 in
 let rec sum_acc_1011 (n__a_1024:(int * int)) =
   match n__a_1024 with
   | (n_1012, a_1013) -> if n_1012 <= 0 then
                           a_1013
                         else
                           sum_acc_1011 (n_1012 - 1, n_1012 + a_1013)
 in
 let sum__sum_acc_1065 = (sum_acc_1011, sum_1008) in
 let sum_acc_1066 = fst sum__sum_acc_1065 in
 let sum_1067 = snd sum__sum_acc_1065 in
 let main_1014 (n_1015:int) (a_1016:int) =
   let s1_1017 = sum_1067 n_1015 in
   let s2_1018 = sum_acc_1066 (n_1015, a_1016) in
   if a_1016 + s1_1017 = s2_1018 then
     ()
   else
     {fail} ()
 in
 let main_1062 = let arg1_1058 = rand_int () in
                 let arg2_1060 = rand_int () in
                 main_1014 arg1_1058 arg2_1060 in
 ()

encode_list:
 let rec sum_1008 (n_1009:int) = if n_1009 <= 0 then
                                   0
                                 else
                                   let s_1010 = sum_1008 (n_1009 - 1) in
                                   n_1009 + s_1010 in
 let rec sum_acc_1011 (n__a_1024:(int * int)) =
   let n_1012 = fst n__a_1024 in
   let a_1013 = snd n__a_1024 in
   (label[IdTerm(n__a_1024, (n_1012, a_1013))]
    (if n_1012 <= 0 then
       a_1013
     else
       sum_acc_1011 (n_1012 - 1, n_1012 + a_1013)))
 in
 let sum__sum_acc_1065 = (sum_acc_1011, sum_1008) in
 let sum_acc_1066 = fst sum__sum_acc_1065 in
 let sum_1067 = snd sum__sum_acc_1065 in
 let main_1014 (n_1015:int) (a_1016:int) =
   let s1_1017 = sum_1067 n_1015 in
   let s2_1018 = sum_acc_1066 (n_1015, a_1016) in
   if a_1016 + s1_1017 = s2_1018 then
     ()
   else
     {fail} ()
 in
 let main_1062 = let arg1_1058 = rand_int () in
                 let arg2_1060 = rand_int () in
                 main_1014 arg1_1058 arg2_1060 in
 ()

ret_fun:
 let rec sum_1008 (n_1009:int) =
   let b_1098 = n_1009 <= 0 in
   if b_1098 then
     0
   else
     let n_1104 = n_1009 - 1 in
     let r_sum_1106 = sum_1008 n_1104 in
     n_1009 + r_sum_1106
 in
 let rec sum_acc_1011 (n__a_1024:(int * int)) =
   let n_1012 = fst n__a_1024 in
   let a_1013 = snd n__a_1024 in
   let b_1111 = n_1012 <= 0 in
   if b_1111 then
     a_1013
   else
     let n_1116 = n_1012 - 1 in
     let n_1119 = n_1012 + a_1013 in
     let n__n_1123 = (n_1116, n_1119) in
     let r_sum_acc_1125 = sum_acc_1011 n__n_1123 in
     r_sum_acc_1125
 in
 let main_1014 (n_1015:int) (a_1016:int) =
   let r_sum_1139 = sum_1008 n_1015 in
   let n__a_1145 = (n_1015, a_1016) in
   let r_sum_acc_1147 = sum_acc_1011 n__a_1145 in
   let n_1151 = a_1016 + r_sum_1139 in
   let b_1148 = n_1151 = r_sum_acc_1147 in
   if b_1148 then
     ()
   else
     let f_1153 = {fail} in
     let r_f_1156 = f_1153 () in
     r_f_1156
 in
 let r_f_1160 = rand_int () in
 let r_f_1164 = rand_int () in
 let r_main_1168 = main_1014 r_f_1160 in
 let r_main_1170 = r_main_1168 r_f_1164 in
 ()

ref_trans:
 let rec sum_1008 (n_1009:int) =
   if n_1009 <= 0 then
     0
   else
     let r_sum_1204 = sum_1008 (n_1009 - 1) in
     n_1009 + r_sum_1204
 in
 let rec sum_acc_1011 (n__a_1024:(int * int)) =
   if fst n__a_1024 <= 0 then
     snd n__a_1024
   else
     let n_1216 = fst (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) in
     let n_1217 = snd (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) in
     sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
 in
 let main_1014 (n_1015:int) (a_1016:int) =
   let r_sum_1219 = sum_1008 n_1015 in
   let n_1223 = fst (n_1015, a_1016) in
   let a_1224 = snd (n_1015, a_1016) in
   let r_sum_acc_1225 = sum_acc_1011 (n_1015, a_1016) in
   if a_1016 + r_sum_1219 = r_sum_acc_1225 then
     ()
   else
     {fail} ()
 in
 let r_f_1230 = rand_int () in
 let r_f_1232 = rand_int () in
 let r_main_1233 = main_1014 r_f_1230 in
 let r_r_main_1234 = r_main_1233 r_f_1232 in
 ()

inline_wrapped:
let rec sum_1008 n_1009 = if n_1009 <= 0 then
                            0
                          else
                            let r_sum_1204 = sum_1008 (n_1009 - 1) in
                            n_1009 + r_sum_1204 in
let rec sum_acc_1011 n__a_1024 =
  if fst n__a_1024 <= 0 then
    snd n__a_1024
  else
    let n_1216 = fst (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) in
    let n_1217 = snd (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) in
    sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
in
let main_1014 n_1015 a_1016 =
  let r_sum_1219 = sum_1008 n_1015 in
  let n_1223 = fst (n_1015, a_1016) in
  let a_1224 = snd (n_1015, a_1016) in
  let r_sum_acc_1225 = sum_acc_1011 (n_1015, a_1016) in
  if a_1016 + r_sum_1219 = r_sum_acc_1225 then
    ()
  else
    {fail} ()
in
let r_f_1230 = rand_int () in
let r_f_1232 = rand_int () in
let r_main_1233 = main_1014 r_f_1230 in
let r_r_main_1234 = r_main_1233 r_f_1232 in
()

flatten_let:
let rec sum_1008 n_1009 = if n_1009 <= 0 then
                            0
                          else
                            let r_sum_1204 = sum_1008 (n_1009 - 1) in
                            n_1009 + r_sum_1204 in
let rec sum_acc_1011 n__a_1024 =
  if fst n__a_1024 <= 0 then
    snd n__a_1024
  else
    let n_1216 = fst (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) in
    let n_1217 = snd (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) in
    sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
in
let main_1014 n_1015 a_1016 =
  let r_sum_1219 = sum_1008 n_1015 in
  let n_1223 = fst (n_1015, a_1016) in
  let a_1224 = snd (n_1015, a_1016) in
  let r_sum_acc_1225 = sum_acc_1011 (n_1015, a_1016) in
  if a_1016 + r_sum_1219 = r_sum_acc_1225 then
    ()
  else
    {fail} ()
in
let r_f_1230 = rand_int () in
let r_f_1232 = rand_int () in
let r_main_1233 = main_1014 r_f_1230 in
let r_r_main_1234 = r_main_1233 r_f_1232 in
()

NORMALIZE: a_1224
[r_sum_acc_1225]
NORMALIZE: n_1223
[r_sum_acc_1225]
normalize let:
let rec sum_1008 n_1009 = if n_1009 <= 0 then
                            0
                          else
                            let r_sum_1204 = sum_1008 (n_1009 - 1) in
                            n_1009 + r_sum_1204 in
let rec sum_acc_1011 n__a_1024 =
  if fst n__a_1024 <= 0 then
    snd n__a_1024
  else
    let n_1216 = fst (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) in
    let n_1217 = snd (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) in
    sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
in
let main_1014 n_1015 a_1016 =
  let r_sum_1219 = sum_1008 n_1015 in
  let r_sum_acc_1225 = sum_acc_1011 (n_1015, a_1016) in
  let n_1223 = fst (n_1015, a_1016) in
  let a_1224 = snd (n_1015, a_1016) in
  if a_1016 + r_sum_1219 = r_sum_acc_1225 then
    ()
  else
    {fail} ()
in
let r_f_1230 = rand_int () in
let r_f_1232 = rand_int () in
let r_main_1233 = main_1014 r_f_1230 in
let r_r_main_1234 = r_main_1233 r_f_1232 in
()

is_subsumed: rand_int (), rand_int (); is_subsumed: rand_int (), main_1014 r_f_1230; is_subsumed: 
rand_int (), r_main_1233 r_f_1232; is_subsumed: sum_1008 n_1015, sum_acc_1011 (n_1015, a_1016); is_subsumed: 
sum_acc_1011 (n_1015, a_1016), fst (n_1015, a_1016); is_subsumed: sum_1008 n_1015, 
fst (n_1015, a_1016); is_subsumed: fst (n_1015, a_1016), snd (n_1015, a_1016); is_subsumed: 
sum_acc_1011 (n_1015, a_1016), snd (n_1015, a_1016); is_subsumed: sum_1008 n_1015, 
snd (n_1015, a_1016); is_subsumed: fst (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024), 
snd (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024); 
elim_same_app:
let rec sum_1008 n_1009 = if n_1009 <= 0 then
                            0
                          else
                            let r_sum_1204 = sum_1008 (n_1009 - 1) in
                            n_1009 + r_sum_1204 in
let rec sum_acc_1011 n__a_1024 =
  if fst n__a_1024 <= 0 then
    snd n__a_1024
  else
    let n_1216 = fst (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) in
    let n_1217 = snd (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) in
    sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
in
let main_1014 n_1015 a_1016 =
  let r_sum_1219 = sum_1008 n_1015 in
  let r_sum_acc_1225 = sum_acc_1011 (n_1015, a_1016) in
  let n_1223 = fst (n_1015, a_1016) in
  let a_1224 = snd (n_1015, a_1016) in
  if a_1016 + r_sum_1219 = r_sum_acc_1225 then
    ()
  else
    {fail} ()
in
let r_f_1230 = rand_int () in
let r_f_1232 = rand_int () in
let r_main_1233 = main_1014 r_f_1230 in
let r_r_main_1234 = r_main_1233 r_f_1232 in
()

elim_unused_branch:
let rec sum_1008 n_1009 = if n_1009 <= 0 then
                            0
                          else
                            let r_sum_1204 = sum_1008 (n_1009 - 1) in
                            n_1009 + r_sum_1204 in
let rec sum_acc_1011 n__a_1024 =
  if fst n__a_1024 <= 0 then
    snd n__a_1024
  else
    let n_1216 = fst (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) in
    let n_1217 = snd (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) in
    sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
in
let main_1014 n_1015 a_1016 =
  let r_sum_1219 = sum_1008 n_1015 in
  let r_sum_acc_1225 = sum_acc_1011 (n_1015, a_1016) in
  let n_1223 = fst (n_1015, a_1016) in
  let a_1224 = snd (n_1015, a_1016) in
  if a_1016 + r_sum_1219 = r_sum_acc_1225 then
    ()
  else
    {fail} ()
in
let r_f_1230 = rand_int () in
let r_f_1232 = rand_int () in
let r_main_1233 = main_1014 r_f_1230 in
let r_r_main_1234 = r_main_1233 r_f_1232 in
()

elim_unused_let:
let rec sum_1008 n_1009 = if n_1009 <= 0 then
                            0
                          else
                            let r_sum_1204 = sum_1008 (n_1009 - 1) in
                            n_1009 + r_sum_1204 in
let rec sum_acc_1011 n__a_1024 =
  if fst n__a_1024 <= 0 then
    snd n__a_1024
  else
    sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
in
let main_1014 n_1015 a_1016 =
  let r_sum_1219 = sum_1008 n_1015 in
  let r_sum_acc_1225 = sum_acc_1011 (n_1015, a_1016) in
  if a_1016 + r_sum_1219 = r_sum_acc_1225 then
    ()
  else
    {fail} ()
in
let r_f_1230 = rand_int () in
let r_f_1232 = rand_int () in
let r_main_1233 = main_1014 r_f_1230 in
let r_r_main_1234 = r_main_1233 r_f_1232 in
()

tupled:
let rec sum_1008 n_1009 = if n_1009 <= 0 then
                            0
                          else
                            let r_sum_1204 = sum_1008 (n_1009 - 1) in
                            n_1009 + r_sum_1204 in
let rec sum_acc_1011 n__a_1024 =
  if fst n__a_1024 <= 0 then
    snd n__a_1024
  else
    sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
in
let main_1014 n_1015 a_1016 =
  let r_sum_1219 = sum_1008 n_1015 in
  let r_sum_acc_1225 = sum_acc_1011 (n_1015, a_1016) in
  if a_1016 + r_sum_1219 = r_sum_acc_1225 then
    ()
  else
    {fail} ()
in
let r_f_1230 = rand_int () in
let r_f_1232 = rand_int () in
let r_main_1233 = main_1014 r_f_1230 in
let r_r_main_1234 = r_main_1233 r_f_1232 in
()

normalize:
let rec sum_1008 n_1009 = if n_1009 <= 0 then
                            0
                          else
                            let r_sum_1248 = sum_1008 (n_1009 - 1) in
                            n_1009 + r_sum_1248 in
let rec sum_acc_1011 n__a_1024 =
  if fst n__a_1024 <= 0 then
    snd n__a_1024
  else
    sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
in
let main_1014 n_1015 a_1016 =
  let r_sum_1272 = sum_1008 n_1015 in
  let r_sum_acc_1276 = sum_acc_1011 (n_1015, a_1016) in
  if a_1016 + r_sum_1272 = r_sum_acc_1276 then
    ()
  else
    {fail} ()
in
let r_f_1283 = rand_int () in
let r_f_1285 = rand_int () in
let r_main_1286 = main_1014 r_f_1283 in
let r_r_main_1287 = r_main_1286 r_f_1285 in
let r_r_main_1234 = r_r_main_1287 in
()

r_f_1283: int
r_f_1285: int
n_1015: int
(n_1015, a_1016): (int * int)
n_1009 - 1: int
replace_app:
let rec sum_1008 n_1009 = if n_1009 <= 0 then
                            0
                          else
                            let r_sum_1248 = sum_1008 (n_1009 - 1) in
                            n_1009 + r_sum_1248 in
let rec sum_acc_1011 n__a_1024 =
  if fst n__a_1024 <= 0 then
    snd n__a_1024
  else
    sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
in
let main_1014 n_1015 a_1016 =
  let r_sum_1272 = sum_1008 n_1015 in
  let r_sum_acc_1276 = sum_acc_1011 (n_1015, a_1016) in
  if a_1016 + r_sum_1272 = r_sum_acc_1276 then
    ()
  else
    {fail} ()
in
let r_f_1283 = rand_int () in
let r_f_1285 = rand_int () in
let r_main_1286 = main_1014 r_f_1283 in
let r_r_main_1287 = r_main_1286 r_f_1285 in
let r_r_main_1234 = r_r_main_1287 in
()

is_subsumed: rand_int (), rand_int (); is_subsumed: rand_int (), main_1014 r_f_1283; is_subsumed: 
rand_int (), r_main_1286 r_f_1285; is_subsumed: main_1014 r_f_1283, r_r_main_1287; is_subsumed: 
rand_int (), r_r_main_1287; is_subsumed: rand_int (), r_r_main_1287; is_subsumed: 
sum_1008 n_1015, sum_acc_1011 (n_1015, a_1016); 
elim_unnecessary:
let rec sum_1008 n_1009 = if n_1009 <= 0 then
                            0
                          else
                            let r_sum_1248 = sum_1008 (n_1009 - 1) in
                            n_1009 + r_sum_1248 in
let rec sum_acc_1011 n__a_1024 =
  if fst n__a_1024 <= 0 then
    snd n__a_1024
  else
    sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
in
let main_1014 n_1015 a_1016 =
  let r_sum_1272 = sum_1008 n_1015 in
  let r_sum_acc_1276 = sum_acc_1011 (n_1015, a_1016) in
  if a_1016 + r_sum_1272 = r_sum_acc_1276 then
    ()
  else
    {fail} ()
in
let r_f_1283 = rand_int () in
let r_f_1285 = rand_int () in
let r_main_1286 = main_1014 r_f_1283 in
let r_r_main_1287 = r_main_1286 r_f_1285 in
let r_r_main_1234 = r_r_main_1287 in
()

inline_next_redex:
let rec sum_1008 n_1009 = if n_1009 <= 0 then
                            0
                          else
                            let r_sum_1248 = sum_1008 (n_1009 - 1) in
                            n_1009 + r_sum_1248 in
let rec sum_acc_1011 n__a_1024 =
  if fst n__a_1024 <= 0 then
    snd n__a_1024
  else
    sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
in
let main_1014 n_1015 a_1016 =
  let r_sum_1272 = sum_1008 n_1015 in
  let r_sum_acc_1276 = sum_acc_1011 (n_1015, a_1016) in
  if a_1016 + r_sum_1272 = r_sum_acc_1276 then
    ()
  else
    {fail} ()
in
let r_f_1283 = rand_int () in
let r_f_1285 = rand_int () in
let r_main_1286 = main_1014 r_f_1283 in
let r_r_main_1287 = r_main_1286 r_f_1285 in
let r_r_main_1234 = r_r_main_1287 in
()

reduce_bottomh:
let rec sum_1008 n_1009 = if n_1009 <= 0 then
                            0
                          else
                            let r_sum_1248 = sum_1008 (n_1009 - 1) in
                            n_1009 + r_sum_1248 in
let rec sum_acc_1011 n__a_1024 =
  if fst n__a_1024 <= 0 then
    snd n__a_1024
  else
    sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
in
let main_1014 n_1015 a_1016 =
  let r_sum_1272 = sum_1008 n_1015 in
  let r_sum_acc_1276 = sum_acc_1011 (n_1015, a_1016) in
  if a_1016 + r_sum_1272 = r_sum_acc_1276 then
    ()
  else
    {fail} ()
in
let r_f_1283 = rand_int () in
let r_f_1285 = rand_int () in
let r_main_1286 = main_1014 r_f_1283 in
let r_r_main_1287 = r_main_1286 r_f_1285 in
let r_r_main_1234 = r_r_main_1287 in
()

tupling:
 let rec sum_1008 (n_1009:int) =
   if n_1009 <= 0 then
     0
   else
     let r_sum_1248 = sum_1008 (n_1009 - 1) in
     n_1009 + r_sum_1248
 in
 let rec sum_acc_1011 (n__a_1024:(int * int)) =
   if fst n__a_1024 <= 0 then
     snd n__a_1024
   else
     sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
 in
 let main_1014 (n_1015:int) (a_1016:int) =
   let r_sum_1272 = sum_1008 n_1015 in
   let r_sum_acc_1276 = sum_acc_1011 (n_1015, a_1016) in
   if a_1016 + r_sum_1272 = r_sum_acc_1276 then
     ()
   else
     {fail} ()
 in
 let r_f_1283 = rand_int () in
 let r_f_1285 = rand_int () in
 let r_main_1286 = main_1014 r_f_1283 in
 let r_r_main_1287 = r_main_1286 r_f_1285 in
 let r_r_main_1234 = r_r_main_1287 in
 ()

CPS:
 let rec sum_1008 (n_1009:int) (k_sum_1297:(int -> X)) =
   if n_1009 <= 0 then
     k_sum_1297 0
   else
     let r_sum_1248 (k_sum_r_sum_1304:(int -> X)) = sum_1008 (n_1009 - 1) k_sum_r_sum_1304 in
     r_sum_1248 (fun (r_sum_1310:int) -> k_sum_1297 (n_1009 + r_sum_1310))
 in
 let rec sum_acc_1011 (n__a_1024:(int * int)) (k_sum_acc_1321:(int -> X)) =
   if fst n__a_1024 <= 0 then
     k_sum_acc_1321 (snd n__a_1024)
   else
     sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) k_sum_acc_1321
 in
 let main_1014 (n_1015:int) (a_1016:int) (k_main_1348:(unit -> X)) =
   let r_sum_1272 (k_main_r_sum_1355:(int -> X)) = sum_1008 n_1015 k_main_r_sum_1355 in
   r_sum_1272
     (fun (r_sum_1391:int) ->
        (let r_sum_acc_1276 (k_main_r_sum_acc_1373:(int -> X)) = sum_acc_1011 (n_1015, a_1016) k_main_r_sum_acc_1373 in
         r_sum_acc_1276
           (fun (r_sum_acc_1390:int) ->
              (if a_1016 + r_sum_1391 = r_sum_acc_1390 then
                 k_main_1348 ()
               else
                 {|fail|} () k_main_1348))))
 in
 let r_f_1283 (k_r_f_1402:(int -> X)) = rand_int_cps () k_r_f_1402 in
 r_f_1283
   (fun (r_f_1447:int) ->
      (let r_f_1285 (k_r_f_1414:(int -> X)) = rand_int_cps () k_r_f_1414 in
       r_f_1285
         (fun (r_f_1446:int) ->
            (let r_r_main_1287 (k_r_r_main_1435:(unit -> X)) = (main_1014 r_f_1447) r_f_1446 k_r_r_main_1435 in
             r_r_main_1287 (fun (r_r_main_1441:unit) -> {end})))))

remove_pair:
 let rec sum_1008 (n_1009:int) (k_sum_1297:(int -> X)) =
   if n_1009 <= 0 then
     k_sum_1297 0
   else
     let r_sum_1248 (k_sum_r_sum_1304:(int -> X)) = sum_1008 (n_1009 - 1) k_sum_r_sum_1304 in
     r_sum_1248 (fun (r_sum_1310:int) -> k_sum_1297 (n_1009 + r_sum_1310))
 in
 let rec sum_acc_1011 (n__a0_1024:int) (n__a1_1024:int) (k_sum_acc_1321:(int -> X)) =
   if n__a0_1024 <= 0 then
     k_sum_acc_1321 n__a1_1024
   else
     sum_acc_1011 (n__a0_1024 - 1) (n__a0_1024 + n__a1_1024) k_sum_acc_1321
 in
 let main_1014 (n_1015:int) (a_1016:int) (k_main_1348:(unit -> X)) =
   let r_sum_1272 (k_main_r_sum_1355:(int -> X)) = sum_1008 n_1015 k_main_r_sum_1355 in
   r_sum_1272
     (fun (r_sum_1391:int) ->
        (let r_sum_acc_1276 (k_main_r_sum_acc_1373:(int -> X)) = sum_acc_1011 n_1015 a_1016 k_main_r_sum_acc_1373 in
         r_sum_acc_1276
           (fun (r_sum_acc_1390:int) ->
              (if a_1016 + r_sum_1391 = r_sum_acc_1390 then
                 k_main_1348 ()
               else
                 {|fail|} () k_main_1348))))
 in
 let r_f_1283 (k_r_f_1402:(int -> X)) = rand_int_cps () k_r_f_1402 in
 r_f_1283
   (fun (r_f_1447:int) ->
      (let r_f_1285 (k_r_f_1414:(int -> X)) = rand_int_cps () k_r_f_1414 in
       r_f_1285
         (fun (r_f_1446:int) ->
            (let r_r_main_1287 (k_r_r_main_1435:(unit -> X)) = main_1014 r_f_1447 r_f_1446 k_r_r_main_1435 in
             r_r_main_1287 (fun (r_r_main_1441:unit) -> {end})))))

Program with abstraction types (CEGAR-cycle 0)::
Main: main_1472
  main_1472 -> (r_f_1283 f_1476);;
  f_1476 r_f_1447 -> (r_f_1285 r_f_1447 (f_1477 r_f_1447));;
  f_1477 r_f_1447 r_f_1446 -> (r_r_main_1287 r_f_1446 r_f_1447 (f_1478 r_f_1446 r_f_1447));;
  f_1478 r_f_1446 r_f_1447 r_r_main_1441 -> end;;
  f_main_1474 a_1016 n_1015 k_main_1348 r_sum_1391 ->
      (r_sum_acc_1276 a_1016 n_1015 r_sum_1391 (f_main_1475 a_1016 n_1015 r_sum_1391 k_main_1348));;
  f_main_1475 a_1016 n_1015 r_sum_1391 k_main_1348 r_sum_acc_1390 when (
      (a_1016 + r_sum_1391) = r_sum_acc_1390) -> (k_main_1348 ());;
  f_main_1475 a_1016 n_1015 r_sum_1391 k_main_1348 r_sum_acc_1390 when (
      not ((a_1016 + r_sum_1391) = r_sum_acc_1390)) -> (fail_1479 true k_main_1348);;
  f_sum_1473 n_1009 k_sum_1297 r_sum_1310 -> (k_sum_1297 (n_1009 + r_sum_1310));;
  fail_1479 b k -> {fail} => (k ());;
  main_1014 n_1015 a_1016 k_main_1348 -> (r_sum_1272 a_1016 n_1015 (f_main_1474 a_1016 n_1015 k_main_1348));;
  r_f_1283 k_r_f_1402 -> (rand_int k_r_f_1402);;
  r_f_1285 r_f_1447 k_r_f_1414 -> (rand_int k_r_f_1414);;
  r_r_main_1287 r_f_1446 r_f_1447 k_r_r_main_1435 -> (main_1014 r_f_1447 r_f_1446 k_r_r_main_1435);;
  r_sum_1248 n_1009 k_sum_r_sum_1304 -> (sum_1008 (n_1009 - 1) k_sum_r_sum_1304);;
  r_sum_1272 a_1016 n_1015 k_main_r_sum_1355 -> (sum_1008 n_1015 k_main_r_sum_1355);;
  r_sum_acc_1276 a_1016 n_1015 r_sum_1391 k_main_r_sum_acc_1373 -> (sum_acc_1011 n_1015 a_1016 k_main_r_sum_acc_1373);;
  sum_1008 n_1009 k_sum_1297 when (n_1009 <= 0) -> (k_sum_1297 0);;
  sum_1008 n_1009 k_sum_1297 when (not (n_1009 <= 0)) -> (r_sum_1248 n_1009 (f_sum_1473 n_1009 k_sum_1297));;
  sum_acc_1011 n__a0_1024 n__a1_1024 k_sum_acc_1321 when (n__a0_1024 <= 0) -> (k_sum_acc_1321 n__a1_1024);;
  sum_acc_1011 n__a0_1024 n__a1_1024 k_sum_acc_1321 when (not (n__a0_1024 <= 0)) ->
      (sum_acc_1011 (n__a0_1024 - 1) (n__a0_1024 + n__a1_1024) k_sum_acc_1321);;
Types:
  main_1472 : X
  fail_1479 : (bool -> (unit -> X) -> X)
  sum_1008 : (int -> (int -> X) -> X)
  sum_acc_1011 : (int -> int -> (int -> X) -> X)

(0-1) Abstracting ... DONE!

(0-2) Checking HORS ... DONE!

Error trace::
  main_1472 ... --> 
  r_f_1283 ... --> 
  f_1476 ... --> 
  r_f_1285 ... --> 
  f_1477 ... --> 
  r_r_main_1287 ... --> 
  main_1014 ... --> 
  r_sum_1272 ... --> 
  sum_1008 [1/2] ... --> 
  f_main_1474 ... --> 
  r_sum_acc_1276 ... --> 
  sum_acc_1011 [1/2] ... --> 
  f_main_1475 [2/2] ... --> 
  fail_1479 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0

(0-3) Checking counterexample ... DONE!

(0-4) Discovering predicates ... 
DONE!

Prefix of spurious counterexample::
0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1

Program with abstraction types (CEGAR-cycle 1)::
Main: main_1472
  main_1472 -> (r_f_1283 f_1476);;
  f_1476 r_f_1447 -> (r_f_1285 r_f_1447 (f_1477 r_f_1447));;
  f_1477 r_f_1447 r_f_1446 -> (r_r_main_1287 r_f_1446 r_f_1447 (f_1478 r_f_1446 r_f_1447));;
  f_1478 r_f_1446 r_f_1447 r_r_main_1441 -> end;;
  f_main_1474 a_1016 n_1015 k_main_1348 r_sum_1391 ->
      (r_sum_acc_1276 a_1016 n_1015 r_sum_1391 (f_main_1475 a_1016 n_1015 r_sum_1391 k_main_1348));;
  f_main_1475 a_1016 n_1015 r_sum_1391 k_main_1348 r_sum_acc_1390 when (
      (a_1016 + r_sum_1391) = r_sum_acc_1390) -> (k_main_1348 ());;
  f_main_1475 a_1016 n_1015 r_sum_1391 k_main_1348 r_sum_acc_1390 when (
      not ((a_1016 + r_sum_1391) = r_sum_acc_1390)) -> (fail_1479 true k_main_1348);;
  f_sum_1473 n_1009 k_sum_1297 r_sum_1310 -> (k_sum_1297 (n_1009 + r_sum_1310));;
  fail_1479 b k -> {fail} => (k ());;
  main_1014 n_1015 a_1016 k_main_1348 -> (r_sum_1272 a_1016 n_1015 (f_main_1474 a_1016 n_1015 k_main_1348));;
  r_f_1283 k_r_f_1402 -> (rand_int k_r_f_1402);;
  r_f_1285 r_f_1447 k_r_f_1414 -> (rand_int k_r_f_1414);;
  r_r_main_1287 r_f_1446 r_f_1447 k_r_r_main_1435 -> (main_1014 r_f_1447 r_f_1446 k_r_r_main_1435);;
  r_sum_1248 n_1009 k_sum_r_sum_1304 -> (sum_1008 (n_1009 - 1) k_sum_r_sum_1304);;
  r_sum_1272 a_1016 n_1015 k_main_r_sum_1355 -> (sum_1008 n_1015 k_main_r_sum_1355);;
  r_sum_acc_1276 a_1016 n_1015 r_sum_1391 k_main_r_sum_acc_1373 -> (sum_acc_1011 n_1015 a_1016 k_main_r_sum_acc_1373);;
  sum_1008 n_1009 k_sum_1297 when (n_1009 <= 0) -> (k_sum_1297 0);;
  sum_1008 n_1009 k_sum_1297 when (not (n_1009 <= 0)) -> (r_sum_1248 n_1009 (f_sum_1473 n_1009 k_sum_1297));;
  sum_acc_1011 n__a0_1024 n__a1_1024 k_sum_acc_1321 when (n__a0_1024 <= 0) -> (k_sum_acc_1321 n__a1_1024);;
  sum_acc_1011 n__a0_1024 n__a1_1024 k_sum_acc_1321 when (not (n__a0_1024 <= 0)) ->
      (sum_acc_1011 (n__a0_1024 - 1) (n__a0_1024 + n__a1_1024) k_sum_acc_1321);;
Types:
  main_1472 : X
  fail_1479 : (bool -> (unit -> X) -> X)
  sum_1008 : (int -> (x_3:int[x_3 = 0] -> X) -> X)
  sum_acc_1011 : (int -> x_2:int -> (x_4:int[x_4 = x_2] -> X) -> X)

(1-1) Abstracting ... DONE!

(1-2) Checking HORS ... DONE!

Error trace::
  main_1472 ... --> 
  r_f_1283 ... --> 
  f_1476 ... --> 
  r_f_1285 ... --> 
  f_1477 ... --> 
  r_r_main_1287 ... --> 
  main_1014 ... --> 
  r_sum_1272 ... --> 
  sum_1008 [2/2] ... --> 
  r_sum_1248 ... --> 
  sum_1008 [1/2] ... --> 
  f_sum_1473 ... --> 
  f_main_1474 ... --> 
  r_sum_acc_1276 ... --> 
  sum_acc_1011 [2/2] ... --> 
  sum_acc_1011 [1/2] ... --> 
  f_main_1475 [2/2] ... --> 
  fail_1479 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 1; 0; 1; 0

(1-3) Checking counterexample ... DONE!

(1-4) Discovering predicates ... 
DONE!

Prefix of spurious counterexample::
0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 1; 0; 1

Program with abstraction types (CEGAR-cycle 2)::
Main: main_1472
  main_1472 -> (r_f_1283 f_1476);;
  f_1476 r_f_1447 -> (r_f_1285 r_f_1447 (f_1477 r_f_1447));;
  f_1477 r_f_1447 r_f_1446 -> (r_r_main_1287 r_f_1446 r_f_1447 (f_1478 r_f_1446 r_f_1447));;
  f_1478 r_f_1446 r_f_1447 r_r_main_1441 -> end;;
  f_main_1474 a_1016 n_1015 k_main_1348 r_sum_1391 ->
      (r_sum_acc_1276 a_1016 n_1015 r_sum_1391 (f_main_1475 a_1016 n_1015 r_sum_1391 k_main_1348));;
  f_main_1475 a_1016 n_1015 r_sum_1391 k_main_1348 r_sum_acc_1390 when (
      (a_1016 + r_sum_1391) = r_sum_acc_1390) -> (k_main_1348 ());;
  f_main_1475 a_1016 n_1015 r_sum_1391 k_main_1348 r_sum_acc_1390 when (
      not ((a_1016 + r_sum_1391) = r_sum_acc_1390)) -> (fail_1479 true k_main_1348);;
  f_sum_1473 n_1009 k_sum_1297 r_sum_1310 -> (k_sum_1297 (n_1009 + r_sum_1310));;
  fail_1479 b k -> {fail} => (k ());;
  main_1014 n_1015 a_1016 k_main_1348 -> (r_sum_1272 a_1016 n_1015 (f_main_1474 a_1016 n_1015 k_main_1348));;
  r_f_1283 k_r_f_1402 -> (rand_int k_r_f_1402);;
  r_f_1285 r_f_1447 k_r_f_1414 -> (rand_int k_r_f_1414);;
  r_r_main_1287 r_f_1446 r_f_1447 k_r_r_main_1435 -> (main_1014 r_f_1447 r_f_1446 k_r_r_main_1435);;
  r_sum_1248 n_1009 k_sum_r_sum_1304 -> (sum_1008 (n_1009 - 1) k_sum_r_sum_1304);;
  r_sum_1272 a_1016 n_1015 k_main_r_sum_1355 -> (sum_1008 n_1015 k_main_r_sum_1355);;
  r_sum_acc_1276 a_1016 n_1015 r_sum_1391 k_main_r_sum_acc_1373 -> (sum_acc_1011 n_1015 a_1016 k_main_r_sum_acc_1373);;
  sum_1008 n_1009 k_sum_1297 when (n_1009 <= 0) -> (k_sum_1297 0);;
  sum_1008 n_1009 k_sum_1297 when (not (n_1009 <= 0)) -> (r_sum_1248 n_1009 (f_sum_1473 n_1009 k_sum_1297));;
  sum_acc_1011 n__a0_1024 n__a1_1024 k_sum_acc_1321 when (n__a0_1024 <= 0) -> (k_sum_acc_1321 n__a1_1024);;
  sum_acc_1011 n__a0_1024 n__a1_1024 k_sum_acc_1321 when (not (n__a0_1024 <= 0)) ->
      (sum_acc_1011 (n__a0_1024 - 1) (n__a0_1024 + n__a1_1024) k_sum_acc_1321);;
Types:
  main_1472 : X
  fail_1479 : (bool -> (unit -> X) -> X)
  sum_1008 : (x_1:int -> (x_3:int[x_1 <= 0 && x_3 = 0; 1 >= x_1 && x_3 = x_1; x_3 = 0] -> X) -> X)
  sum_acc_1011 : (x_1:int -> x_2:int[x_1 >= 0; 1 >= x_1] -> (x_4:int[x_4 = x_1 + x_2; x_4 = x_2] -> X) -> X)

(2-1) Abstracting ... DONE!

(2-2) Checking HORS ... DONE!

Error trace::
  main_1472 ... --> 
  r_f_1283 ... --> 
  f_1476 ... --> 
  r_f_1285 ... --> 
  f_1477 ... --> 
  r_r_main_1287 ... --> 
  main_1014 ... --> 
  r_sum_1272 ... --> 
  sum_1008 [1/2] ... --> 
  f_main_1474 ... --> 
  r_sum_acc_1276 ... --> 
  sum_acc_1011 [2/2] ... --> 
  sum_acc_1011 [1/2] ... --> 
  f_main_1475 [2/2] ... --> 
  fail_1479 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 1; 0

(2-3) Checking counterexample ... DONE!

(2-4) Discovering predicates ... 
DONE!

Prefix of spurious counterexample::
0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1

Program with abstraction types (CEGAR-cycle 3)::
Main: main_1472
  main_1472 -> (r_f_1283 f_1476);;
  f_1476 r_f_1447 -> (r_f_1285 r_f_1447 (f_1477 r_f_1447));;
  f_1477 r_f_1447 r_f_1446 -> (r_r_main_1287 r_f_1446 r_f_1447 (f_1478 r_f_1446 r_f_1447));;
  f_1478 r_f_1446 r_f_1447 r_r_main_1441 -> end;;
  f_main_1474 a_1016 n_1015 k_main_1348 r_sum_1391 ->
      (r_sum_acc_1276 a_1016 n_1015 r_sum_1391 (f_main_1475 a_1016 n_1015 r_sum_1391 k_main_1348));;
  f_main_1475 a_1016 n_1015 r_sum_1391 k_main_1348 r_sum_acc_1390 when (
      (a_1016 + r_sum_1391) = r_sum_acc_1390) -> (k_main_1348 ());;
  f_main_1475 a_1016 n_1015 r_sum_1391 k_main_1348 r_sum_acc_1390 when (
      not ((a_1016 + r_sum_1391) = r_sum_acc_1390)) -> (fail_1479 true k_main_1348);;
  f_sum_1473 n_1009 k_sum_1297 r_sum_1310 -> (k_sum_1297 (n_1009 + r_sum_1310));;
  fail_1479 b k -> {fail} => (k ());;
  main_1014 n_1015 a_1016 k_main_1348 -> (r_sum_1272 a_1016 n_1015 (f_main_1474 a_1016 n_1015 k_main_1348));;
  r_f_1283 k_r_f_1402 -> (rand_int k_r_f_1402);;
  r_f_1285 r_f_1447 k_r_f_1414 -> (rand_int k_r_f_1414);;
  r_r_main_1287 r_f_1446 r_f_1447 k_r_r_main_1435 -> (main_1014 r_f_1447 r_f_1446 k_r_r_main_1435);;
  r_sum_1248 n_1009 k_sum_r_sum_1304 -> (sum_1008 (n_1009 - 1) k_sum_r_sum_1304);;
  r_sum_1272 a_1016 n_1015 k_main_r_sum_1355 -> (sum_1008 n_1015 k_main_r_sum_1355);;
  r_sum_acc_1276 a_1016 n_1015 r_sum_1391 k_main_r_sum_acc_1373 -> (sum_acc_1011 n_1015 a_1016 k_main_r_sum_acc_1373);;
  sum_1008 n_1009 k_sum_1297 when (n_1009 <= 0) -> (k_sum_1297 0);;
  sum_1008 n_1009 k_sum_1297 when (not (n_1009 <= 0)) -> (r_sum_1248 n_1009 (f_sum_1473 n_1009 k_sum_1297));;
  sum_acc_1011 n__a0_1024 n__a1_1024 k_sum_acc_1321 when (n__a0_1024 <= 0) -> (k_sum_acc_1321 n__a1_1024);;
  sum_acc_1011 n__a0_1024 n__a1_1024 k_sum_acc_1321 when (not (n__a0_1024 <= 0)) ->
      (sum_acc_1011 (n__a0_1024 - 1) (n__a0_1024 + n__a1_1024) k_sum_acc_1321);;
Types:
  main_1472 : X
  fail_1479 : (bool -> (unit -> X) -> X)
  sum_1008 : (x_1:int -> (x_3:int[x_1 <= 0; x_1 <= 0 && x_3 = 0; 1 >= x_1 && x_3 = x_1; x_3 = 0] -> X) -> X)
  sum_acc_1011 : (x_1:int -> x_2:int[x_1 <= 0; x_1 >= 0; 1 >= x_1] -> (x_4:int[x_4 = x_1 + x_2; x_4 = x_2] -> X) -> X)

(3-1) Abstracting ... DONE!

(3-2) Checking HORS ... DONE!

Error trace::
  main_1472 ... --> 
  r_f_1283 ... --> 
  f_1476 ... --> 
  r_f_1285 ... --> 
  f_1477 ... --> 
  r_r_main_1287 ... --> 
  main_1014 ... --> 
  r_sum_1272 ... --> 
  sum_1008 [2/2] ... --> 
  r_sum_1248 ... --> 
  sum_1008 [1/2] ... --> 
  f_sum_1473 ... --> 
  f_main_1474 ... --> 
  r_sum_acc_1276 ... --> 
  sum_acc_1011 [1/2] ... --> 
  f_main_1475 [2/2] ... --> 
  fail_1479 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 1; 0

(3-3) Checking counterexample ... DONE!

(3-4) Discovering predicates ... 
DONE!

Prefix of spurious counterexample::
0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0

Program with abstraction types (CEGAR-cycle 4)::
Main: main_1472
  main_1472 -> (r_f_1283 f_1476);;
  f_1476 r_f_1447 -> (r_f_1285 r_f_1447 (f_1477 r_f_1447));;
  f_1477 r_f_1447 r_f_1446 -> (r_r_main_1287 r_f_1446 r_f_1447 (f_1478 r_f_1446 r_f_1447));;
  f_1478 r_f_1446 r_f_1447 r_r_main_1441 -> end;;
  f_main_1474 a_1016 n_1015 k_main_1348 r_sum_1391 ->
      (r_sum_acc_1276 a_1016 n_1015 r_sum_1391 (f_main_1475 a_1016 n_1015 r_sum_1391 k_main_1348));;
  f_main_1475 a_1016 n_1015 r_sum_1391 k_main_1348 r_sum_acc_1390 when (
      (a_1016 + r_sum_1391) = r_sum_acc_1390) -> (k_main_1348 ());;
  f_main_1475 a_1016 n_1015 r_sum_1391 k_main_1348 r_sum_acc_1390 when (
      not ((a_1016 + r_sum_1391) = r_sum_acc_1390)) -> (fail_1479 true k_main_1348);;
  f_sum_1473 n_1009 k_sum_1297 r_sum_1310 -> (k_sum_1297 (n_1009 + r_sum_1310));;
  fail_1479 b k -> {fail} => (k ());;
  main_1014 n_1015 a_1016 k_main_1348 -> (r_sum_1272 a_1016 n_1015 (f_main_1474 a_1016 n_1015 k_main_1348));;
  r_f_1283 k_r_f_1402 -> (rand_int k_r_f_1402);;
  r_f_1285 r_f_1447 k_r_f_1414 -> (rand_int k_r_f_1414);;
  r_r_main_1287 r_f_1446 r_f_1447 k_r_r_main_1435 -> (main_1014 r_f_1447 r_f_1446 k_r_r_main_1435);;
  r_sum_1248 n_1009 k_sum_r_sum_1304 -> (sum_1008 (n_1009 - 1) k_sum_r_sum_1304);;
  r_sum_1272 a_1016 n_1015 k_main_r_sum_1355 -> (sum_1008 n_1015 k_main_r_sum_1355);;
  r_sum_acc_1276 a_1016 n_1015 r_sum_1391 k_main_r_sum_acc_1373 -> (sum_acc_1011 n_1015 a_1016 k_main_r_sum_acc_1373);;
  sum_1008 n_1009 k_sum_1297 when (n_1009 <= 0) -> (k_sum_1297 0);;
  sum_1008 n_1009 k_sum_1297 when (not (n_1009 <= 0)) -> (r_sum_1248 n_1009 (f_sum_1473 n_1009 k_sum_1297));;
  sum_acc_1011 n__a0_1024 n__a1_1024 k_sum_acc_1321 when (n__a0_1024 <= 0) -> (k_sum_acc_1321 n__a1_1024);;
  sum_acc_1011 n__a0_1024 n__a1_1024 k_sum_acc_1321 when (not (n__a0_1024 <= 0)) ->
      (sum_acc_1011 (n__a0_1024 - 1) (n__a0_1024 + n__a1_1024) k_sum_acc_1321);;
Types:
  main_1472 : X
  fail_1479 : (bool -> (unit -> X) -> X)
  sum_1008 : (x_1:int ->
              (x_3:int[x_3 >= 0 && x_3 <= x_1; x_1 <= 0; x_1 <= 0 && x_3 = 0; 1 >= x_1 && x_3 = x_1; x_3 = 0] -> X) ->
              X)
  sum_acc_1011 : (x_1:int ->
                  x_2:int[x_1 <= 0; x_1 >= 0; 1 >= x_1] ->
                  (x_4:int[x_4 >= x_1 + x_2 && x_4 <= x_2; x_4 = x_1 + x_2; x_4 = x_2] -> X) -> X)

(4-1) Abstracting ... DONE!

(4-2) Checking HORS ... DONE!

Error trace::
  main_1472 ... --> 
  r_f_1283 ... --> 
  f_1476 ... --> 
  r_f_1285 ... --> 
  f_1477 ... --> 
  r_r_main_1287 ... --> 
  main_1014 ... --> 
  r_sum_1272 ... --> 
  sum_1008 [2/2] ... --> 
  r_sum_1248 ... --> 
  sum_1008 [2/2] ... --> 
  r_sum_1248 ... --> 
  sum_1008 [1/2] ... --> 
  f_sum_1473 ... --> 
  f_sum_1473 ... --> 
  f_main_1474 ... --> 
  r_sum_acc_1276 ... --> 
  sum_acc_1011 [1/2] ... --> 
  f_main_1475 [2/2] ... --> 
  fail_1479 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 1; 0; 0; 0; 0; 0; 0; 0; 1; 0

(4-3) Checking counterexample ... DONE!

(4-4) Discovering predicates ... 
DONE!

Prefix of spurious counterexample::
0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 1; 0; 0; 0; 0; 0; 0; 0

Program with abstraction types (CEGAR-cycle 5)::
Main: main_1472
  main_1472 -> (r_f_1283 f_1476);;
  f_1476 r_f_1447 -> (r_f_1285 r_f_1447 (f_1477 r_f_1447));;
  f_1477 r_f_1447 r_f_1446 -> (r_r_main_1287 r_f_1446 r_f_1447 (f_1478 r_f_1446 r_f_1447));;
  f_1478 r_f_1446 r_f_1447 r_r_main_1441 -> end;;
  f_main_1474 a_1016 n_1015 k_main_1348 r_sum_1391 ->
      (r_sum_acc_1276 a_1016 n_1015 r_sum_1391 (f_main_1475 a_1016 n_1015 r_sum_1391 k_main_1348));;
  f_main_1475 a_1016 n_1015 r_sum_1391 k_main_1348 r_sum_acc_1390 when (
      (a_1016 + r_sum_1391) = r_sum_acc_1390) -> (k_main_1348 ());;
  f_main_1475 a_1016 n_1015 r_sum_1391 k_main_1348 r_sum_acc_1390 when (
      not ((a_1016 + r_sum_1391) = r_sum_acc_1390)) -> (fail_1479 true k_main_1348);;
  f_sum_1473 n_1009 k_sum_1297 r_sum_1310 -> (k_sum_1297 (n_1009 + r_sum_1310));;
  fail_1479 b k -> {fail} => (k ());;
  main_1014 n_1015 a_1016 k_main_1348 -> (r_sum_1272 a_1016 n_1015 (f_main_1474 a_1016 n_1015 k_main_1348));;
  r_f_1283 k_r_f_1402 -> (rand_int k_r_f_1402);;
  r_f_1285 r_f_1447 k_r_f_1414 -> (rand_int k_r_f_1414);;
  r_r_main_1287 r_f_1446 r_f_1447 k_r_r_main_1435 -> (main_1014 r_f_1447 r_f_1446 k_r_r_main_1435);;
  r_sum_1248 n_1009 k_sum_r_sum_1304 -> (sum_1008 (n_1009 - 1) k_sum_r_sum_1304);;
  r_sum_1272 a_1016 n_1015 k_main_r_sum_1355 -> (sum_1008 n_1015 k_main_r_sum_1355);;
  r_sum_acc_1276 a_1016 n_1015 r_sum_1391 k_main_r_sum_acc_1373 -> (sum_acc_1011 n_1015 a_1016 k_main_r_sum_acc_1373);;
  sum_1008 n_1009 k_sum_1297 when (n_1009 <= 0) -> (k_sum_1297 0);;
  sum_1008 n_1009 k_sum_1297 when (not (n_1009 <= 0)) -> (r_sum_1248 n_1009 (f_sum_1473 n_1009 k_sum_1297));;
  sum_acc_1011 n__a0_1024 n__a1_1024 k_sum_acc_1321 when (n__a0_1024 <= 0) -> (k_sum_acc_1321 n__a1_1024);;
  sum_acc_1011 n__a0_1024 n__a1_1024 k_sum_acc_1321 when (not (n__a0_1024 <= 0)) ->
      (sum_acc_1011 (n__a0_1024 - 1) (n__a0_1024 + n__a1_1024) k_sum_acc_1321);;
Types:
  main_1472 : X
  fail_1479 : (bool -> (unit -> X) -> X)
  sum_1008 : (x_1:int ->
              (x_3:int[x_3 >= 0 && 2*x_3 <= -x_1; 2 <= x_1 + x_3 && 1 >= -x_1 + 2*x_3; 
                       x_3 >= 0 && 2*x_3 <= 3*x_1; x_3 >= 0 && x_3 <= x_1; 
                       x_1 <= 0; x_1 <= 0 && x_3 = 0; 1 >= x_1 && x_3 = x_1; 
                       x_3 = 0]
              -> X) -> X)
  sum_acc_1011 : (x_1:int ->
                  x_2:int[x_1 <= 0; x_1 >= 0; 1 >= x_1] ->
                  (x_4:int[2*x_4 >= 3*x_1 + 2*x_2 && x_4 <= x_2; x_4 >= x_1 + x_2 && x_4 <= x_2; 
                           x_4 = x_1 + x_2; x_4 = x_2]
                  -> X) -> X)

(5-1) Abstracting ... DONE!

(5-2) Checking HORS ... DONE!

Error trace::
  main_1472 ... --> 
  r_f_1283 ... --> 
  f_1476 ... --> 
  r_f_1285 ... --> 
  f_1477 ... --> 
  r_r_main_1287 ... --> 
  main_1014 ... --> 
  r_sum_1272 ... --> 
  sum_1008 [2/2] ... --> 
  r_sum_1248 ... --> 
  sum_1008 [2/2] ... --> 
  r_sum_1248 ... --> 
  sum_1008 [1/2] ... --> 
  f_sum_1473 ... --> 
  f_sum_1473 ... --> 
  f_main_1474 ... --> 
  r_sum_acc_1276 ... --> 
  sum_acc_1011 [2/2] ... --> 
  sum_acc_1011 [1/2] ... --> 
  f_main_1475 [2/2] ... --> 
  fail_1479 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 1; 0; 0; 0; 0; 0; 0; 1; 0; 1; 0

(5-3) Checking counterexample ... DONE!

(5-4) Discovering predicates ... 
DONE!

Prefix of spurious counterexample::
0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 1; 0; 0; 0; 0; 0; 0; 1; 0

Program with abstraction types (CEGAR-cycle 6)::
Main: main_1472
  main_1472 -> (r_f_1283 f_1476);;
  f_1476 r_f_1447 -> (r_f_1285 r_f_1447 (f_1477 r_f_1447));;
  f_1477 r_f_1447 r_f_1446 -> (r_r_main_1287 r_f_1446 r_f_1447 (f_1478 r_f_1446 r_f_1447));;
  f_1478 r_f_1446 r_f_1447 r_r_main_1441 -> end;;
  f_main_1474 a_1016 n_1015 k_main_1348 r_sum_1391 ->
      (r_sum_acc_1276 a_1016 n_1015 r_sum_1391 (f_main_1475 a_1016 n_1015 r_sum_1391 k_main_1348));;
  f_main_1475 a_1016 n_1015 r_sum_1391 k_main_1348 r_sum_acc_1390 when (
      (a_1016 + r_sum_1391) = r_sum_acc_1390) -> (k_main_1348 ());;
  f_main_1475 a_1016 n_1015 r_sum_1391 k_main_1348 r_sum_acc_1390 when (
      not ((a_1016 + r_sum_1391) = r_sum_acc_1390)) -> (fail_1479 true k_main_1348);;
  f_sum_1473 n_1009 k_sum_1297 r_sum_1310 -> (k_sum_1297 (n_1009 + r_sum_1310));;
  fail_1479 b k -> {fail} => (k ());;
  main_1014 n_1015 a_1016 k_main_1348 -> (r_sum_1272 a_1016 n_1015 (f_main_1474 a_1016 n_1015 k_main_1348));;
  r_f_1283 k_r_f_1402 -> (rand_int k_r_f_1402);;
  r_f_1285 r_f_1447 k_r_f_1414 -> (rand_int k_r_f_1414);;
  r_r_main_1287 r_f_1446 r_f_1447 k_r_r_main_1435 -> (main_1014 r_f_1447 r_f_1446 k_r_r_main_1435);;
  r_sum_1248 n_1009 k_sum_r_sum_1304 -> (sum_1008 (n_1009 - 1) k_sum_r_sum_1304);;
  r_sum_1272 a_1016 n_1015 k_main_r_sum_1355 -> (sum_1008 n_1015 k_main_r_sum_1355);;
  r_sum_acc_1276 a_1016 n_1015 r_sum_1391 k_main_r_sum_acc_1373 -> (sum_acc_1011 n_1015 a_1016 k_main_r_sum_acc_1373);;
  sum_1008 n_1009 k_sum_1297 when (n_1009 <= 0) -> (k_sum_1297 0);;
  sum_1008 n_1009 k_sum_1297 when (not (n_1009 <= 0)) -> (r_sum_1248 n_1009 (f_sum_1473 n_1009 k_sum_1297));;
  sum_acc_1011 n__a0_1024 n__a1_1024 k_sum_acc_1321 when (n__a0_1024 <= 0) -> (k_sum_acc_1321 n__a1_1024);;
  sum_acc_1011 n__a0_1024 n__a1_1024 k_sum_acc_1321 when (not (n__a0_1024 <= 0)) ->
      (sum_acc_1011 (n__a0_1024 - 1) (n__a0_1024 + n__a1_1024) k_sum_acc_1321);;
Types:
  main_1472 : X
  fail_1479 : (bool -> (unit -> X) -> X)
  sum_1008 : (x_1:int ->
              (x_3:int[1 <= x_3 && 1 >= -x_1 + 2*x_3; x_3 >= x_1 && 2*x_3 <= 3*x_1; 
                       x_3 >= 0 && 2*x_3 <= -x_1; 2 <= x_1 + x_3 && 1 >= -x_1 + 2*x_3; 
                       x_3 >= 0 && 2*x_3 <= 3*x_1; x_3 >= 0 && x_3 <= x_1; 
                       x_1 <= 0; x_1 <= 0 && x_3 = 0; 1 >= x_1 && x_3 = x_1; 
                       x_3 = 0]
              -> X) -> X)
  sum_acc_1011 : (x_1:int ->
                  x_2:int[x_1 <= 0; x_1 >= 0; 1 >= x_1] ->
                  (x_4:int[x_4 <= x_2 && 2*x_4 >= x_1 + 2*x_2; x_4 <= x_1 + x_2 && 1 >= 3*x_1 + 2*x_2 - 2*x_4; 
                           2*x_4 >= 3*x_1 + 2*x_2 && x_4 <= x_2; x_4 >= x_1 + x_2 && x_4 <= x_2; 
                           x_4 = x_1 + x_2; x_4 = x_2]
                  -> X) -> X)

(6-1) Abstracting ... 