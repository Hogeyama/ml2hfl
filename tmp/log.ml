MoCHi: Model Checker for Higher-Order Programs
  Build: _bb807d4 (after 2014-08-05 14:36:39 +0900)
  FPAT version: 3fa26b7
  TRecS version: 1.33
  OCaml version: 4.01.0
  Command: ./mochi.opt test_relabs/sum.ml -disable-rc -color -list-option -fpat -hccs 1 -bool-init-empty -abs-filter 
           -tupling -debug-module Tupling,Ref_trans,Ret_fun

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

ASSERT: a_1016 + sum_1008 n_1015 = sum_acc_1011 (n_1015, a_1016)
FUN: sum_acc_1011
FUN: sum_1008
FUN': sum_acc_1011
FUN': sum_1008
ALL_FUN_ARG: sum_acc_1011, sum_1008
FUN_ARG': sum_acc_1011, sum_1008
t':
let main_1014 n_1015 a_1016 =
  let s1_1017 = x_1069 in
  let s2_1018 = x_1068 in
  if a_1016 + s1_1017 = s2_1018 then
    ()
  else
    {fail} ()
in
let main_1062 = let arg1_1058 = rand_int () in
                let arg2_1060 = rand_int () in
                main_1014 arg1_1058 arg2_1060 in
()

t'':
let main_1014 n_1015 a_1016 =
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
let sum__sum_acc_1065 = (label[String add_fun_tuple] (label[String ] (sum_acc_1011, sum_1008))) in
let sum_acc_1066 = fst sum__sum_acc_1065 in
let sum_1067 = snd sum__sum_acc_1065 in
let main_1014 n_1015 a_1016 =
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
 let sum__sum_acc_1065 = (label[String add_fun_tuple] (label[String ] (sum_acc_1011, sum_1008))) in
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
 let sum__sum_acc_1065 = (label[String add_fun_tuple] (label[String ] (sum_acc_1011, sum_1008))) in
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

INPUT:
let rec sum_1008 n_1009 = if n_1009 <= 0 then
                            0
                          else
                            let s_1010 = sum_1008 (n_1009 - 1) in
                            n_1009 + s_1010 in
let rec sum_acc_1011 n__a_1024 =
  let n_1012 = fst n__a_1024 in
  let a_1013 = snd n__a_1024 in
  (label[IdTerm(n__a_1024, (n_1012, a_1013))]
   (if n_1012 <= 0 then
      a_1013
    else
      sum_acc_1011 (n_1012 - 1, n_1012 + a_1013)))
in
let sum__sum_acc_1065 = (label[String add_fun_tuple] (label[String ] (sum_acc_1011, sum_1008))) in
let sum_acc_1066 = fst sum__sum_acc_1065 in
let sum_1067 = snd sum__sum_acc_1065 in
let main_1014 n_1015 a_1016 =
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

normalize:
let rec sum_1008 n_1009 =
  let b_1098 = let n_1099 = n_1009 in
               let n_1100 = 0 in
               n_1099 <= n_1100 in
  if b_1098 then
    0
  else
    let s_1010 =
      let n_1104 = let n_1101 = n_1009 in
                   let n_1102 = 1 in
                   n_1101 - n_1102 in
      let sum_1103 = sum_1008 in
      let r_sum_1106 = sum_1103 n_1104 in
      r_sum_1106
    in
    let n_1107 = n_1009 in
    let s_1108 = s_1010 in
    n_1107 + s_1108
in
let rec sum_acc_1011 n__a_1024 =
  let n_1012 = let n__a_1109 = n__a_1024 in
               fst n__a_1109 in
  let a_1013 = let n__a_1110 = n__a_1024 in
               snd n__a_1110 in
  (label[IdTerm(n__a_1024, (let n_1126 = n_1012 in
                            let a_1127 = a_1013 in
                            (n_1126, a_1127)))]
   (let b_1111 = let n_1112 = n_1012 in
                 let n_1113 = 0 in
                 n_1112 <= n_1113 in
    if b_1111 then
      a_1013
    else
      let n__n_1123 =
        let n_1116 = let n_1114 = n_1012 in
                     let n_1115 = 1 in
                     n_1114 - n_1115 in
        let n_1119 = let n_1117 = n_1012 in
                     let a_1118 = a_1013 in
                     n_1117 + a_1118 in
        (n_1116, n_1119)
      in
      let sum_acc_1122 = sum_acc_1011 in
      let r_sum_acc_1125 = sum_acc_1122 n__n_1123 in
      r_sum_acc_1125))
in
let sum__sum_acc_1065 =
  (label[String add_fun_tuple]
   (label[String ] (let sum_acc_1130 = sum_acc_1011 in
                    let sum_1131 = sum_1008 in
                    (sum_acc_1130, sum_1131))))
in
let sum_acc_1066 = let sum__sum_acc_1134 = sum__sum_acc_1065 in
                   fst sum__sum_acc_1134 in
let sum_1067 = let sum__sum_acc_1135 = sum__sum_acc_1065 in
               snd sum__sum_acc_1135 in
let main_1014 n_1015 a_1016 =
  let s1_1017 = let n_1137 = n_1015 in
                let sum_1136 = sum_1067 in
                let r_sum_1139 = sum_1136 n_1137 in
                r_sum_1139 in
  let s2_1018 =
    let n__a_1145 = let n_1140 = n_1015 in
                    let a_1141 = a_1016 in
                    (n_1140, a_1141) in
    let sum_acc_1144 = sum_acc_1066 in
    let r_sum_acc_1147 = sum_acc_1144 n__a_1145 in
    r_sum_acc_1147
  in
  let b_1148 =
    let n_1151 = let a_1149 = a_1016 in
                 let s1_1150 = s1_1017 in
                 a_1149 + s1_1150 in
    let s2_1152 = s2_1018 in
    n_1151 = s2_1152
  in
  if b_1148 then
    ()
  else
    let u_1154 = () in
    let f_1153 = {fail} in
    let r_f_1156 = f_1153 u_1154 in
    r_f_1156
in
let main_1062 =
  let arg1_1058 = let u_1158 = () in
                  let f_1157 = rand_int in
                  let r_f_1160 = f_1157 u_1158 in
                  r_f_1160 in
  let arg2_1060 = let u_1162 = () in
                  let f_1161 = rand_int in
                  let r_f_1164 = f_1161 u_1162 in
                  r_f_1164 in
  let arg2_1167 = arg2_1060 in
  let arg1_1166 = arg1_1058 in
  let main_1165 = main_1014 in
  let r_main_1168 = main_1165 arg1_1166 in
  let r_main_1170 = r_main_1168 arg2_1167 in
  r_main_1170
in
()

inline_var_const:
let rec sum_1008 n_1009 =
  let b_1098 = n_1009 <= 0 in
  if b_1098 then
    0
  else
    let s_1010 = let n_1104 = n_1009 - 1 in
                 let r_sum_1106 = sum_1008 n_1104 in
                 r_sum_1106 in
    n_1009 + s_1010
in
let rec sum_acc_1011 n__a_1024 =
  let n_1012 = fst n__a_1024 in
  let a_1013 = snd n__a_1024 in
  (label[IdTerm(n__a_1024, (n_1012, a_1013))]
   (let b_1111 = n_1012 <= 0 in
    if b_1111 then
      a_1013
    else
      let n__n_1123 = let n_1116 = n_1012 - 1 in
                      let n_1119 = n_1012 + a_1013 in
                      (n_1116, n_1119) in
      let r_sum_acc_1125 = sum_acc_1011 n__n_1123 in
      r_sum_acc_1125))
in
let sum__sum_acc_1065 = (label[String add_fun_tuple] (label[String ] (sum_acc_1011, sum_1008))) in
let sum_acc_1066 = fst sum__sum_acc_1065 in
let sum_1067 = snd sum__sum_acc_1065 in
let main_1014 n_1015 a_1016 =
  let s1_1017 = let r_sum_1139 = sum_1067 n_1015 in
                r_sum_1139 in
  let s2_1018 = let n__a_1145 = (n_1015, a_1016) in
                let r_sum_acc_1147 = sum_acc_1066 n__a_1145 in
                r_sum_acc_1147 in
  let b_1148 = let n_1151 = a_1016 + s1_1017 in
               n_1151 = s2_1018 in
  if b_1148 then
    ()
  else
    let f_1153 = {fail} in
    let r_f_1156 = f_1153 () in
    r_f_1156
in
let main_1062 =
  let arg1_1058 = let r_f_1160 = rand_int () in
                  r_f_1160 in
  let arg2_1060 = let r_f_1164 = rand_int () in
                  r_f_1164 in
  let r_main_1168 = main_1014 arg1_1058 in
  let r_main_1170 = r_main_1168 arg2_1060 in
  r_main_1170
in
()

flatten_let:
let rec sum_1008 n_1009 =
  let b_1098 = n_1009 <= 0 in
  if b_1098 then
    0
  else
    let n_1104 = n_1009 - 1 in
    let r_sum_1106 = sum_1008 n_1104 in
    let s_1010 = r_sum_1106 in
    n_1009 + s_1010
in
let rec sum_acc_1011 n__a_1024 =
  let n_1012 = fst n__a_1024 in
  let a_1013 = snd n__a_1024 in
  (label[IdTerm(n__a_1024, (n_1012, a_1013))]
   (let b_1111 = n_1012 <= 0 in
    if b_1111 then
      a_1013
    else
      let n_1116 = n_1012 - 1 in
      let n_1119 = n_1012 + a_1013 in
      let n__n_1123 = (n_1116, n_1119) in
      let r_sum_acc_1125 = sum_acc_1011 n__n_1123 in
      r_sum_acc_1125))
in
let sum__sum_acc_1065 = (label[String add_fun_tuple] (label[String ] (sum_acc_1011, sum_1008))) in
let sum_acc_1066 = fst sum__sum_acc_1065 in
let sum_1067 = snd sum__sum_acc_1065 in
let main_1014 n_1015 a_1016 =
  let r_sum_1139 = sum_1067 n_1015 in
  let s1_1017 = r_sum_1139 in
  let n__a_1145 = (n_1015, a_1016) in
  let r_sum_acc_1147 = sum_acc_1066 n__a_1145 in
  let s2_1018 = r_sum_acc_1147 in
  let n_1151 = a_1016 + s1_1017 in
  let b_1148 = n_1151 = s2_1018 in
  if b_1148 then
    ()
  else
    let f_1153 = {fail} in
    let r_f_1156 = f_1153 () in
    r_f_1156
in
let r_f_1160 = rand_int () in
let arg1_1058 = r_f_1160 in
let r_f_1164 = rand_int () in
let arg2_1060 = r_f_1164 in
let r_main_1168 = main_1014 arg1_1058 in
let r_main_1170 = r_main_1168 arg2_1060 in
let main_1062 = r_main_1170 in
()

add_proj_info:
let rec sum_1008 n_1009 =
  let b_1098 = n_1009 <= 0 in
  if b_1098 then
    0
  else
    let n_1104 = n_1009 - 1 in
    let r_sum_1106 = sum_1008 n_1104 in
    let s_1010 = r_sum_1106 in
    n_1009 + s_1010
in
let rec sum_acc_1011 n__a_1024 =
  let n_1012 = fst n__a_1024 in
  let a_1013 = snd n__a_1024 in
  (label[IdTerm(n__a_1024, (n_1012, a_1013))]
   (let b_1111 = n_1012 <= 0 in
    if b_1111 then
      a_1013
    else
      let n_1116 = n_1012 - 1 in
      let n_1119 = n_1012 + a_1013 in
      let n__n_1123 = (n_1116, n_1119) in
      (label[String ret_fun]
       (label[IdTerm(n_1116, (fst n__n_1123))]
        (label[String ret_fun]
         (label[IdTerm(n_1119, (snd n__n_1123))] (let r_sum_acc_1125 = sum_acc_1011 n__n_1123 in
                                                  r_sum_acc_1125)))))))
in
let sum__sum_acc_1065 = (label[String add_fun_tuple] (label[String ] (sum_acc_1011, sum_1008))) in
let sum_acc_1066 = fst sum__sum_acc_1065 in
let sum_1067 = snd sum__sum_acc_1065 in
let main_1014 n_1015 a_1016 =
  let r_sum_1139 = sum_1067 n_1015 in
  let s1_1017 = r_sum_1139 in
  let n__a_1145 = (n_1015, a_1016) in
  (label[String ret_fun]
   (label[IdTerm(n_1015, (fst n__a_1145))]
    (label[String ret_fun]
     (label[IdTerm(a_1016, (snd n__a_1145))]
      (let r_sum_acc_1147 = sum_acc_1066 n__a_1145 in
       let s2_1018 = r_sum_acc_1147 in
       let n_1151 = a_1016 + s1_1017 in
       let b_1148 = n_1151 = s2_1018 in
       if b_1148 then
         ()
       else
         let f_1153 = {fail} in
         let r_f_1156 = f_1153 () in
         r_f_1156)))))
in
let r_f_1160 = rand_int () in
let arg1_1058 = r_f_1160 in
let r_f_1164 = rand_int () in
let arg2_1060 = r_f_1164 in
let r_main_1168 = main_1014 arg1_1058 in
let r_main_1170 = r_main_1168 arg2_1060 in
let main_1062 = r_main_1170 in
()

ret_fun:
let rec sum_1008 (n_1009:int) =
  let b_1098 = n_1009 <= 0 in
  if b_1098 then
    0
  else
    let n_1104 = n_1009 - 1 in
    let r_sum_1106 = sum_1008 n_1104 in
    let s_1010 = r_sum_1106 in
    n_1009 + s_1010
in
let rec sum_acc_1011 (n__a_1024:(int * int)) =
  let n_1012 = fst n__a_1024 in
  let a_1013 = snd n__a_1024 in
  (label[IdTerm(n__a_1024, (n_1012, a_1013))]
   (let b_1111 = n_1012 <= 0 in
    if b_1111 then
      a_1013
    else
      let n_1116 = n_1012 - 1 in
      let n_1119 = n_1012 + a_1013 in
      let n__n_1123 = (n_1116, n_1119) in
      (label[String ret_fun]
       (label[IdTerm(n_1116, (fst n__n_1123))]
        (label[String ret_fun]
         (label[IdTerm(n_1119, (snd n__n_1123))] (let r_sum_acc_1125 = sum_acc_1011 n__n_1123 in
                                                  r_sum_acc_1125)))))))
in
let sum__sum_acc_1065 = (label[String add_fun_tuple] (label[String ] (sum_acc_1011, sum_1008))) in
let sum_acc_1066 = fst sum__sum_acc_1065 in
let sum_1067 = snd sum__sum_acc_1065 in
let main_1014 (n_1015:int) (a_1016:int) =
  let r_sum_1139 = sum_1067 n_1015 in
  let s1_1017 = r_sum_1139 in
  let n__a_1145 = (n_1015, a_1016) in
  (label[String ret_fun]
   (label[IdTerm(n_1015, (fst n__a_1145))]
    (label[String ret_fun]
     (label[IdTerm(a_1016, (snd n__a_1145))]
      (let r_sum_acc_1147 = sum_acc_1066 n__a_1145 in
       let s2_1018 = r_sum_acc_1147 in
       let n_1151 = a_1016 + s1_1017 in
       let b_1148 = n_1151 = s2_1018 in
       if b_1148 then
         ()
       else
         let f_1153 = {fail} in
         let r_f_1156 = f_1153 () in
         r_f_1156)))))
in
let r_f_1160 = rand_int () in
let arg1_1058 = r_f_1160 in
let r_f_1164 = rand_int () in
let arg2_1060 = r_f_1164 in
let r_main_1168 = main_1014 arg1_1058 in
let r_main_1170 = r_main_1168 arg2_1060 in
let main_1062 = r_main_1170 in
()

remove_label:
let rec sum_1008 (n_1009:int) =
  let b_1098 = n_1009 <= 0 in
  if b_1098 then
    0
  else
    let n_1104 = n_1009 - 1 in
    let r_sum_1106 = sum_1008 n_1104 in
    let s_1010 = r_sum_1106 in
    n_1009 + s_1010
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
let sum__sum_acc_1065 = (label[String add_fun_tuple] (label[String ] (sum_acc_1011, sum_1008))) in
let sum_acc_1066 = fst sum__sum_acc_1065 in
let sum_1067 = snd sum__sum_acc_1065 in
let main_1014 (n_1015:int) (a_1016:int) =
  let r_sum_1139 = sum_1067 n_1015 in
  let s1_1017 = r_sum_1139 in
  let n__a_1145 = (n_1015, a_1016) in
  let r_sum_acc_1147 = sum_acc_1066 n__a_1145 in
  let s2_1018 = r_sum_acc_1147 in
  let n_1151 = snd n__a_1145 + s1_1017 in
  let b_1148 = n_1151 = s2_1018 in
  if b_1148 then
    ()
  else
    let f_1153 = {fail} in
    let r_f_1156 = f_1153 () in
    r_f_1156
in
let r_f_1160 = rand_int () in
let arg1_1058 = r_f_1160 in
let r_f_1164 = rand_int () in
let arg2_1060 = r_f_1164 in
let r_main_1168 = main_1014 arg1_1058 in
let r_main_1170 = r_main_1168 arg2_1060 in
let main_1062 = r_main_1170 in
()

flatten_tuple:
let rec sum_1008 (n_1009:int) =
  let b_1098 = n_1009 <= 0 in
  if b_1098 then
    0
  else
    let n_1104 = n_1009 - 1 in
    let r_sum_1106 = sum_1008 n_1104 in
    let s_1010 = r_sum_1106 in
    n_1009 + s_1010
in
let rec sum_acc_1011 (n__a_1024:(int * int)) =
  let n_1012 = let n__a0_1171 = n__a_1024 in
               fst n__a0_1171 in
  let a_1013 = let n__a1_1172 = n__a_1024 in
               snd n__a1_1172 in
  let b_1111 = n_1012 <= 0 in
  if b_1111 then
    a_1013
  else
    let n_1116 = n_1012 - 1 in
    let n_1119 = n_1012 + a_1013 in
    let n__n_1123 =
      let n_1173 = n_1116 in
      let n_1174 = n_1119 in
      let n_1176 = n_1174 in
      let n_1175 = n_1173 in
      (n_1175, n_1176)
    in
    let r_sum_acc_1125 = sum_acc_1011 n__n_1123 in
    r_sum_acc_1125
in
let sum__sum_acc_1065 =
  (label[String add_fun_tuple]
   (label[String ]
    (let sum_acc_1179 = sum_acc_1011 in
     let sum_1180 = sum_1008 in
     let sum_1182 = sum_1180 in
     let sum_acc_1181 = sum_acc_1179 in
     (sum_acc_1181, sum_1182))))
in
let sum_acc_1066 = let sum__sum_acc0_1185 = sum__sum_acc_1065 in
                   fst sum__sum_acc0_1185 in
let sum_1067 = let sum__sum_acc1_1186 = sum__sum_acc_1065 in
               snd sum__sum_acc1_1186 in
let main_1014 (n_1015:int) (a_1016:int) =
  let r_sum_1139 = sum_1067 n_1015 in
  let s1_1017 = r_sum_1139 in
  let n__a_1145 =
    let n_1187 = n_1015 in
    let a_1188 = a_1016 in
    let a_1190 = a_1188 in
    let n_1189 = n_1187 in
    (n_1189, a_1190)
  in
  let r_sum_acc_1147 = sum_acc_1066 n__a_1145 in
  let s2_1018 = r_sum_acc_1147 in
  let n_1151 = (let n__a1_1193 = n__a_1145 in
                snd n__a1_1193) + s1_1017 in
  let b_1148 = n_1151 = s2_1018 in
  if b_1148 then
    ()
  else
    let f_1153 = {fail} in
    let r_f_1156 = f_1153 () in
    r_f_1156
in
let r_f_1160 = rand_int () in
let arg1_1058 = r_f_1160 in
let r_f_1164 = rand_int () in
let arg2_1060 = r_f_1164 in
let r_main_1168 = main_1014 arg1_1058 in
let r_main_1170 = r_main_1168 arg2_1060 in
let main_1062 = r_main_1170 in
()

inline_var_const:
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
let sum__sum_acc_1065 = (label[String add_fun_tuple] (label[String ] (sum_acc_1011, sum_1008))) in
let sum_acc_1066 = fst sum__sum_acc_1065 in
let sum_1067 = snd sum__sum_acc_1065 in
let main_1014 (n_1015:int) (a_1016:int) =
  let r_sum_1139 = sum_1067 n_1015 in
  let n__a_1145 = (n_1015, a_1016) in
  let r_sum_acc_1147 = sum_acc_1066 n__a_1145 in
  let n_1151 = snd n__a_1145 + r_sum_1139 in
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

flatten_let:
let rec sum_1008 n_1009 =
  let b_1098 = n_1009 <= 0 in
  if b_1098 then
    0
  else
    let n_1104 = n_1009 - 1 in
    let r_sum_1106 = sum_1008 n_1104 in
    n_1009 + r_sum_1106
in
let rec sum_acc_1011 n__a_1024 =
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
let sum__sum_acc_1065 = (label[String add_fun_tuple] (label[String ] (sum_acc_1011, sum_1008))) in
let sum_acc_1066 = fst sum__sum_acc_1065 in
let sum_1067 = snd sum__sum_acc_1065 in
let main_1014 n_1015 a_1016 =
  let r_sum_1139 = sum_1067 n_1015 in
  let n__a_1145 = (n_1015, a_1016) in
  let r_sum_acc_1147 = sum_acc_1066 n__a_1145 in
  let n_1151 = snd n__a_1145 + r_sum_1139 in
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

beta_var_tuple:
let rec sum_1008 n_1009 =
  let b_1098 = n_1009 <= 0 in
  if b_1098 then
    0
  else
    let n_1104 = n_1009 - 1 in
    let r_sum_1106 = sum_1008 n_1104 in
    n_1009 + r_sum_1106
in
let rec sum_acc_1011 n__a_1024 =
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
let sum__sum_acc_1065 = (label[String add_fun_tuple] (label[String ] (sum_acc_1011, sum_1008))) in
let sum_acc_1066 = fst sum__sum_acc_1065 in
let sum_1067 = snd sum__sum_acc_1065 in
let main_1014 n_1015 a_1016 =
  let r_sum_1139 = sum_1067 n_1015 in
  let n__a_1145 = (n_1015, a_1016) in
  let r_sum_acc_1147 = sum_acc_1066 n__a_1145 in
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
 let sum__sum_acc_1065 = (label[String add_fun_tuple] (label[String ] (sum_acc_1011, sum_1008))) in
 let sum_acc_1066 = fst sum__sum_acc_1065 in
 let sum_1067 = snd sum__sum_acc_1065 in
 let main_1014 (n_1015:int) (a_1016:int) =
   let r_sum_1139 = sum_1067 n_1015 in
   let n__a_1145 = (n_1015, a_1016) in
   let r_sum_acc_1147 = sum_acc_1066 n__a_1145 in
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

INPUT: let rec sum_1008 n_1009 =
         let b_1098 = n_1009 <= 0 in
         if b_1098 then
           0
         else
           let n_1104 = n_1009 - 1 in
           let r_sum_1106 = sum_1008 n_1104 in
           n_1009 + r_sum_1106
       in
       let rec sum_acc_1011 n__a_1024 =
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
       let sum__sum_acc_1065 = (label[String add_fun_tuple] (label[String ] (sum_acc_1011, sum_1008))) in
       let sum_acc_1066 = fst sum__sum_acc_1065 in
       let sum_1067 = snd sum__sum_acc_1065 in
       let main_1014 n_1015 a_1016 =
         let r_sum_1139 = sum_1067 n_1015 in
         let n__a_1145 = (n_1015, a_1016) in
         let r_sum_acc_1147 = sum_acc_1066 n__a_1145 in
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
remove_label: let rec sum_1008 (n_1009:int) =
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
              let sum__sum_acc_1065 = (sum_acc_1011, sum_1008) in
              let sum_acc_1066 = fst sum__sum_acc_1065 in
              let sum_1067 = snd sum__sum_acc_1065 in
              let main_1014 (n_1015:int) (a_1016:int) =
                let r_sum_1139 = sum_1067 n_1015 in
                let n__a_1145 = (n_1015, a_1016) in
                let r_sum_acc_1147 = sum_acc_1066 n__a_1145 in
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
move_proj: let rec sum_1008 (n_1009:int) =
             let b_1098 = n_1009 <= 0 in
             if b_1098 then
               0
             else
               let n_1104 = n_1009 - 1 in
               let r_sum_1106 = sum_1008 n_1104 in
               n_1009 + r_sum_1106
           in
           let rec sum_acc_1011 (n__a_1024:(int * int)) =
             let x1_1196 = fst n__a_1024 in
             let x2_1197 = snd n__a_1024 in
             let n_1012 = x1_1196 in
             let a_1013 = x2_1197 in
             let b_1111 = n_1012 <= 0 in
             if b_1111 then
               a_1013
             else
               let n_1116 = n_1012 - 1 in
               let n_1119 = n_1012 + a_1013 in
               let n__n_1123 = (n_1116, n_1119) in
               let x1_1194 = fst n__n_1123 in
               let x2_1195 = snd n__n_1123 in
               let r_sum_acc_1125 = sum_acc_1011 n__n_1123 in
               r_sum_acc_1125
           in
           let sum__sum_acc_1065 = (sum_acc_1011, sum_1008) in
           let x1_1200 = fst sum__sum_acc_1065 in
           let x2_1201 = snd sum__sum_acc_1065 in
           let sum_acc_1066 = x1_1200 in
           let sum_1067 = x2_1201 in
           let main_1014 (n_1015:int) (a_1016:int) =
             let r_sum_1139 = sum_1067 n_1015 in
             let n__a_1145 = (n_1015, a_1016) in
             let x1_1198 = fst n__a_1145 in
             let x2_1199 = snd n__a_1145 in
             let r_sum_acc_1147 = sum_acc_1066 n__a_1145 in
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
inline_no_effect: let rec sum_1008 (n_1009:int) =
                    let b_1098 = n_1009 <= 0 in
                    if b_1098 then
                      0
                    else
                      let n_1104 = n_1009 - 1 in
                      let r_sum_1106 = sum_1008 n_1104 in
                      n_1009 + r_sum_1106
                  in
                  let rec sum_acc_1011 (n__a_1024:(int * int)) =
                    let x1_1196 = fst n__a_1024 in
                    let x2_1197 = snd n__a_1024 in
                    let n_1012 = x1_1196 in
                    let a_1013 = x2_1197 in
                    let b_1111 = n_1012 <= 0 in
                    if b_1111 then
                      a_1013
                    else
                      let n_1116 = n_1012 - 1 in
                      let n_1119 = n_1012 + a_1013 in
                      let n__n_1123 = (n_1116, n_1119) in
                      let x1_1194 = fst n__n_1123 in
                      let x2_1195 = snd n__n_1123 in
                      let r_sum_acc_1125 = sum_acc_1011 n__n_1123 in
                      r_sum_acc_1125
                  in
                  let sum__sum_acc_1065 = (sum_acc_1011, sum_1008) in
                  let x1_1200 = fst sum__sum_acc_1065 in
                  let x2_1201 = snd sum__sum_acc_1065 in
                  let sum_acc_1066 = x1_1200 in
                  let sum_1067 = x2_1201 in
                  let main_1014 (n_1015:int) (a_1016:int) =
                    let r_sum_1139 = sum_1067 n_1015 in
                    let n__a_1145 = (n_1015, a_1016) in
                    let x1_1198 = fst n__a_1145 in
                    let x2_1199 = snd n__a_1145 in
                    let r_sum_acc_1147 = sum_acc_1066 n__a_1145 in
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
normalize_let: let rec sum_1008 (n_1009:int) =
                 let b_1098 = let b_1203 = n_1009 <= 0 in
                              b_1203 in
                 if b_1098 then
                   0
                 else
                   let n_1104 = n_1009 - 1 in
                   let r_sum_1106 = let r_sum_1206 = sum_1008 n_1104 in
                                    r_sum_1206 in
                   n_1009 + r_sum_1106
               in
               let rec sum_acc_1011 (n__a_1024:(int * int)) =
                 let x1_1196 = let n_1208 = fst n__a_1024 in
                               n_1208 in
                 let x2_1197 = let a_1209 = snd n__a_1024 in
                               a_1209 in
                 let b_1111 = let b_1211 = x1_1196 <= 0 in
                              b_1211 in
                 if b_1111 then
                   x2_1197
                 else
                   let n_1116 = x1_1196 - 1 in
                   let n_1119 = x1_1196 + x2_1197 in
                   let n__n_1123 = let n__n_1217 = (n_1116, n_1119) in
                                   n__n_1217 in
                   let x1_1194 = let n_1218 = fst n__n_1123 in
                                 n_1218 in
                   let x2_1195 = let n_1219 = snd n__n_1123 in
                                 n_1219 in
                   let r_sum_acc_1125 = let r_sum_acc_1220 = sum_acc_1011 n__n_1123 in
                                        r_sum_acc_1220 in
                   r_sum_acc_1125
               in
               let sum__sum_acc_1065 = let sum_acc__sum_1223 = (sum_acc_1011, sum_1008) in
                                       sum_acc__sum_1223 in
               let x1_1200 = let sum_1224 = fst sum__sum_acc_1065 in
                             sum_1224 in
               let x2_1201 = let sum_acc_1225 = snd sum__sum_acc_1065 in
                             sum_acc_1225 in
               let sum_acc_1066 = x1_1200 in
               let sum_1067 = x2_1201 in
               let main_1014 (n_1015:int) (a_1016:int) =
                 let r_sum_1139 = let r_sum_1226 = sum_1067 n_1015 in
                                  r_sum_1226 in
                 let n__a_1145 = let n__a_1229 = (n_1015, a_1016) in
                                 n__a_1229 in
                 let x1_1198 = let n_1230 = fst n__a_1145 in
                               n_1230 in
                 let x2_1199 = let a_1231 = snd n__a_1145 in
                               a_1231 in
                 let r_sum_acc_1147 = let r_sum_acc_1232 = sum_acc_1066 n__a_1145 in
                                      r_sum_acc_1232 in
                 let n_1151 = a_1016 + r_sum_1139 in
                 let b_1148 = let b_1234 = n_1151 = r_sum_acc_1147 in
                              b_1234 in
                 if b_1148 then
                   ()
                 else
                   let f_1153 = {fail} in
                   let r_f_1156 = let r_f_1235 = f_1153 () in
                                  r_f_1235 in
                   r_f_1156
               in
               let r_f_1160 = let f_1236 = rand_int in
                              let r_f_1237 = f_1236 () in
                              r_f_1237 in
               let r_f_1164 = let f_1238 = rand_int in
                              let r_f_1239 = f_1238 () in
                              r_f_1239 in
               let r_main_1168 = let r_main_1240 = main_1014 r_f_1160 in
                                 r_main_1240 in
               let r_main_1170 = let r_r_main_1241 = r_main_1168 r_f_1164 in
                                 r_r_main_1241 in
               ()
flatten_let: let rec sum_1008 (n_1009:int) =
               let b_1203 = n_1009 <= 0 in
               if b_1203 then
                 0
               else
                 let n_1104 = n_1009 - 1 in
                 let r_sum_1206 = sum_1008 n_1104 in
                 n_1009 + r_sum_1206
             in
             let rec sum_acc_1011 (n__a_1024:(int * int)) =
               let n_1208 = fst n__a_1024 in
               let a_1209 = snd n__a_1024 in
               let b_1211 = n_1208 <= 0 in
               if b_1211 then
                 a_1209
               else
                 let n_1116 = n_1208 - 1 in
                 let n_1119 = n_1208 + a_1209 in
                 let n__n_1217 = (n_1116, n_1119) in
                 let n_1218 = fst n__n_1217 in
                 let n_1219 = snd n__n_1217 in
                 let r_sum_acc_1220 = sum_acc_1011 n__n_1217 in
                 r_sum_acc_1220
             in
             let sum_acc__sum_1223 = (sum_acc_1011, sum_1008) in
             let sum_1224 = fst sum_acc__sum_1223 in
             let sum_acc_1225 = snd sum_acc__sum_1223 in
             let main_1014 (n_1015:int) (a_1016:int) =
               let r_sum_1226 = sum_acc_1225 n_1015 in
               let n__a_1229 = (n_1015, a_1016) in
               let n_1230 = fst n__a_1229 in
               let a_1231 = snd n__a_1229 in
               let r_sum_acc_1232 = sum_1224 n__a_1229 in
               let n_1151 = a_1016 + r_sum_1226 in
               let b_1234 = n_1151 = r_sum_acc_1232 in
               if b_1234 then
                 ()
               else
                 let f_1153 = {fail} in
                 let r_f_1235 = f_1153 () in
                 r_f_1235
             in
             let r_f_1237 = rand_int () in
             let r_f_1239 = rand_int () in
             let r_main_1240 = main_1014 r_f_1237 in
             let r_r_main_1241 = r_main_1240 r_f_1239 in
             ()
sort_let_pair: let rec sum_1008 (n_1009:int) =
                 let b_1203 = n_1009 <= 0 in
                 if b_1203 then
                   0
                 else
                   let n_1104 = n_1009 - 1 in
                   let r_sum_1206 = sum_1008 n_1104 in
                   n_1009 + r_sum_1206
               in
               let rec sum_acc_1011 (n__a_1024:(int * int)) =
                 let n_1208 = fst n__a_1024 in
                 let a_1209 = snd n__a_1024 in
                 let b_1211 = n_1208 <= 0 in
                 if b_1211 then
                   a_1209
                 else
                   let n_1116 = n_1208 - 1 in
                   let n_1119 = n_1208 + a_1209 in
                   let n__n_1217 = (n_1116, n_1119) in
                   let n_1218 = fst n__n_1217 in
                   let n_1219 = snd n__n_1217 in
                   let r_sum_acc_1220 = sum_acc_1011 n__n_1217 in
                   r_sum_acc_1220
               in
               let sum_acc__sum_1223 = (sum_acc_1011, sum_1008) in
               let sum_1224 = fst sum_acc__sum_1223 in
               let sum_acc_1225 = snd sum_acc__sum_1223 in
               let main_1014 (n_1015:int) (a_1016:int) =
                 let r_sum_1226 = sum_acc_1225 n_1015 in
                 let n__a_1229 = (n_1015, a_1016) in
                 let n_1230 = fst n__a_1229 in
                 let a_1231 = snd n__a_1229 in
                 let r_sum_acc_1232 = sum_1224 n__a_1229 in
                 let n_1151 = a_1016 + r_sum_1226 in
                 let b_1234 = n_1151 = r_sum_acc_1232 in
                 if b_1234 then
                   ()
                 else
                   let f_1153 = {fail} in
                   let r_f_1235 = f_1153 () in
                   r_f_1235
               in
               let r_f_1237 = rand_int () in
               let r_f_1239 = rand_int () in
               let r_main_1240 = main_1014 r_f_1237 in
               let r_r_main_1241 = r_main_1240 r_f_1239 in
               ()
x: r_main_1240, y': x_1281
THIS IS ROOT
x: main_1014, y': x_1282
THIS IS ROOT
x: f_1153, y': x_1283
THIS IS ROOT
x: sum_1224, y': x_1284
THIS IS NOT ROOT
make_tree: (sum_acc__sum_1223:(((int * int) -> int) * (int -> int)))
make_tree: (sum_1224:((int * int) -> int))
make_tree: (sum_acc_1225:(int -> int))
y': x_1284
path: [0]
TREE: [[(n__a_1229:(int * int))];[(n_1015:int)]]
TREE': [[(x_1284:(int * int))];[(n_1015:int)]]
r': sum_acc__sum_1223:(((bool * (int * int)) * (bool * int)) -> ((bool * int) * (bool * int)))
|trees|': 1
  tree: [(true, x_1284);(true, n_1015)]
x: sum_acc_1225, y': x_1307
THIS IS NOT ROOT
make_tree: (sum_acc__sum_1223:(((int * int) -> int) * (int -> int)))
make_tree: (sum_1224:((int * int) -> int))
make_tree: (sum_acc_1225:(int -> int))
y': x_1307
path: [1]
TREE: [[];[(n_1015:int)]]
TREE': [[];[(x_1307:int)]]
r': sum_acc__sum_1223:(((bool * (int * int)) * (bool * int)) -> ((bool * int) * (bool * int)))
|trees|': 1
  tree: [(false, (0, 0));(true, x_1307)]
x: sum_acc_1011, y': x_1372
THIS IS ROOT
x: sum_1008, y': x_1375
THIS IS ROOT
ref_trans: let rec sum_1008 n_1009 =
             if n_1009 <= 0 then
               0
             else
               let r_sum_1206 = sum_1008 (n_1009 - 1) in
               n_1009 + r_sum_1206
           in
           let rec sum_acc_1011 n__a_1024 =
             if fst n__a_1024 <= 0 then
               snd n__a_1024
             else
               let n_1218 = fst (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) in
               let n_1219 = snd (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) in
               sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
           in
           let sum_acc__sum_1223 xx_1359 =
             ((if fst (fst xx_1359) = false then
                 (false, 0)
               else
                 (true, sum_acc_1011 (snd (fst xx_1359)))),
              (if fst (snd xx_1359) = false then
                 (false, 0)
               else
                 (true, sum_1008 (snd (snd xx_1359)))))
           in
           let sum_1224 x_1339 = snd (fst (sum_acc__sum_1223 ((true, x_1339), (false, 0)))) in
           let sum_acc_1225 x_1330 = snd (snd (sum_acc__sum_1223 ((false, (0, 0)), (true, x_1330)))) in
           let main_1014 n_1015 a_1016 =
             let r_sum_1226 =
               let r_sum_acc__sum_1329 = sum_acc__sum_1223 ((false, (0, 0)), (true, n_1015)) in
               snd (snd r_sum_acc__sum_1329)
             in
             let n_1230 = fst (n_1015, a_1016) in
             let a_1231 = snd (n_1015, a_1016) in
             let r_sum_acc_1232 =
               let r_sum_acc__sum_1304 = sum_acc__sum_1223 ((true, (n_1015, a_1016)), (true, n_1015)) in
               snd (fst r_sum_acc__sum_1304)
             in
             if a_1016 + r_sum_1226 = r_sum_acc_1232 then
               ()
             else
               {fail} ()
           in
           let r_f_1237 = rand_int () in
           let r_f_1239 = rand_int () in
           let r_main_1240 = main_1014 r_f_1237 in
           let r_r_main_1241 = r_main_1240 r_f_1239 in
           ()
ref_trans:
 let rec sum_1008 (n_1009:int) =
   if n_1009 <= 0 then
     0
   else
     let r_sum_1206 = sum_1008 (n_1009 - 1) in
     n_1009 + r_sum_1206
 in
 let rec sum_acc_1011 (n__a_1024:(int * int)) =
   if fst n__a_1024 <= 0 then
     snd n__a_1024
   else
     let n_1218 = fst (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) in
     let n_1219 = snd (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) in
     sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
 in
 let sum_acc__sum_1223 (xx_1359:((bool * (int * int)) * (bool * int))) =
   ((if fst (fst xx_1359) = false then
       (false, 0)
     else
       (true, sum_acc_1011 (snd (fst xx_1359)))),
    (if fst (snd xx_1359) = false then
       (false, 0)
     else
       (true, sum_1008 (snd (snd xx_1359)))))
 in
 let sum_1224 (x_1339:(int * int)) = snd (fst (sum_acc__sum_1223 ((true, x_1339), (false, 0)))) in
 let sum_acc_1225 (x_1330:int) = snd (snd (sum_acc__sum_1223 ((false, (0, 0)), (true, x_1330)))) in
 let main_1014 (n_1015:int) (a_1016:int) =
   let r_sum_1226 =
     let r_sum_acc__sum_1329 = sum_acc__sum_1223 ((false, (0, 0)), (true, n_1015)) in
     snd (snd r_sum_acc__sum_1329)
   in
   let n_1230 = fst (n_1015, a_1016) in
   let a_1231 = snd (n_1015, a_1016) in
   let r_sum_acc_1232 =
     let r_sum_acc__sum_1304 = sum_acc__sum_1223 ((true, (n_1015, a_1016)), (true, n_1015)) in
     snd (fst r_sum_acc__sum_1304)
   in
   if a_1016 + r_sum_1226 = r_sum_acc_1232 then
     ()
   else
     {fail} ()
 in
 let r_f_1237 = rand_int () in
 let r_f_1239 = rand_int () in
 let r_main_1240 = main_1014 r_f_1237 in
 let r_r_main_1241 = r_main_1240 r_f_1239 in
 ()

inline_wrapped:
let rec sum_1008 n_1009 = if n_1009 <= 0 then
                            0
                          else
                            let r_sum_1206 = sum_1008 (n_1009 - 1) in
                            n_1009 + r_sum_1206 in
let rec sum_acc_1011 n__a_1024 =
  if fst n__a_1024 <= 0 then
    snd n__a_1024
  else
    let n_1218 = fst (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) in
    let n_1219 = snd (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) in
    sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
in
let sum_acc__sum_1223 xx_1359 =
  if fst (fst xx_1359) = false then
    if fst (snd xx_1359) = false then
      ((false, 0), (false, 0))
    else
      ((false, 0), (true, sum_1008 (snd (snd xx_1359))))
  else
    if fst (snd xx_1359) = false then
      ((true, sum_acc_1011 (snd (fst xx_1359))), (false, 0))
    else
      ((true, sum_acc_1011 (snd (fst xx_1359))), (true, sum_1008 (snd (snd xx_1359))))
in
let sum_1224 x_1339 = snd (fst (sum_acc__sum_1223 ((true, x_1339), (false, 0)))) in
let sum_acc_1225 x_1330 = snd (snd (sum_acc__sum_1223 ((false, (0, 0)), (true, x_1330)))) in
let main_1014 n_1015 a_1016 =
  let r_sum_1226 =
    let r_sum_acc__sum_1329 = sum_acc__sum_1223 ((false, (0, 0)), (true, n_1015)) in
    snd (snd r_sum_acc__sum_1329)
  in
  let n_1230 = fst (n_1015, a_1016) in
  let a_1231 = snd (n_1015, a_1016) in
  let r_sum_acc_1232 =
    let r_sum_acc__sum_1304 = sum_acc__sum_1223 ((true, (n_1015, a_1016)), (true, n_1015)) in
    snd (fst r_sum_acc__sum_1304)
  in
  if a_1016 + r_sum_1226 = r_sum_acc_1232 then
    ()
  else
    {fail} ()
in
let r_f_1237 = rand_int () in
let r_f_1239 = rand_int () in
let r_main_1240 = main_1014 r_f_1237 in
let r_r_main_1241 = r_main_1240 r_f_1239 in
()

flatten_let:
let rec sum_1008 n_1009 = if n_1009 <= 0 then
                            0
                          else
                            let r_sum_1206 = sum_1008 (n_1009 - 1) in
                            n_1009 + r_sum_1206 in
let rec sum_acc_1011 n__a_1024 =
  if fst n__a_1024 <= 0 then
    snd n__a_1024
  else
    let n_1218 = fst (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) in
    let n_1219 = snd (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) in
    sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
in
let sum_acc__sum_1223 xx_1359 =
  if fst (fst xx_1359) = false then
    if fst (snd xx_1359) = false then
      ((false, 0), (false, 0))
    else
      ((false, 0), (true, sum_1008 (snd (snd xx_1359))))
  else
    if fst (snd xx_1359) = false then
      ((true, sum_acc_1011 (snd (fst xx_1359))), (false, 0))
    else
      ((true, sum_acc_1011 (snd (fst xx_1359))), (true, sum_1008 (snd (snd xx_1359))))
in
let sum_1224 x_1339 = snd (fst (sum_acc__sum_1223 ((true, x_1339), (false, 0)))) in
let sum_acc_1225 x_1330 = snd (snd (sum_acc__sum_1223 ((false, (0, 0)), (true, x_1330)))) in
let main_1014 n_1015 a_1016 =
  let r_sum_acc__sum_1329 = sum_acc__sum_1223 ((false, (0, 0)), (true, n_1015)) in
  let r_sum_1226 = snd (snd r_sum_acc__sum_1329) in
  let n_1230 = fst (n_1015, a_1016) in
  let a_1231 = snd (n_1015, a_1016) in
  let r_sum_acc__sum_1304 = sum_acc__sum_1223 ((true, (n_1015, a_1016)), (true, n_1015)) in
  let r_sum_acc_1232 = snd (fst r_sum_acc__sum_1304) in
  if a_1016 + r_sum_1226 = r_sum_acc_1232 then
    ()
  else
    {fail} ()
in
let r_f_1237 = rand_int () in
let r_f_1239 = rand_int () in
let r_main_1240 = main_1014 r_f_1237 in
let r_r_main_1241 = r_main_1240 r_f_1239 in
()

NORMALIZE: a_1231
[r_sum_acc__sum_1304]
NORMALIZE: n_1230
[r_sum_acc__sum_1304]
NORMALIZE: r_sum_1226
[r_sum_acc__sum_1304]
normalize let:
let rec sum_1008 n_1009 = if n_1009 <= 0 then
                            0
                          else
                            let r_sum_1206 = sum_1008 (n_1009 - 1) in
                            n_1009 + r_sum_1206 in
let rec sum_acc_1011 n__a_1024 =
  if fst n__a_1024 <= 0 then
    snd n__a_1024
  else
    let n_1218 = fst (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) in
    let n_1219 = snd (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) in
    sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
in
let sum_acc__sum_1223 xx_1359 =
  if fst (fst xx_1359) = false then
    if fst (snd xx_1359) = false then
      ((false, 0), (false, 0))
    else
      ((false, 0), (true, sum_1008 (snd (snd xx_1359))))
  else
    if fst (snd xx_1359) = false then
      ((true, sum_acc_1011 (snd (fst xx_1359))), (false, 0))
    else
      ((true, sum_acc_1011 (snd (fst xx_1359))), (true, sum_1008 (snd (snd xx_1359))))
in
let sum_1224 x_1339 = snd (fst (sum_acc__sum_1223 ((true, x_1339), (false, 0)))) in
let sum_acc_1225 x_1330 = snd (snd (sum_acc__sum_1223 ((false, (0, 0)), (true, x_1330)))) in
let main_1014 n_1015 a_1016 =
  let r_sum_acc__sum_1329 = sum_acc__sum_1223 ((false, (0, 0)), (true, n_1015)) in
  let r_sum_acc__sum_1304 = sum_acc__sum_1223 ((true, (n_1015, a_1016)), (true, n_1015)) in
  let r_sum_1226 = snd (snd r_sum_acc__sum_1329) in
  let n_1230 = fst (n_1015, a_1016) in
  let a_1231 = snd (n_1015, a_1016) in
  let r_sum_acc_1232 = snd (fst r_sum_acc__sum_1304) in
  if a_1016 + r_sum_1226 = r_sum_acc_1232 then
    ()
  else
    {fail} ()
in
let r_f_1237 = rand_int () in
let r_f_1239 = rand_int () in
let r_main_1240 = main_1014 r_f_1237 in
let r_r_main_1241 = r_main_1240 r_f_1239 in
()

is_subsumed: rand_int (), rand_int (); is_subsumed: rand_int (), main_1014 r_f_1237; is_subsumed: 
rand_int (), r_main_1240 r_f_1239; is_subsumed: sum_acc__sum_1223 ((false, (0, 0)), (true, n_1015)), 
sum_acc__sum_1223 ((true, (n_1015, a_1016)), (true, n_1015)); r_sum_acc__sum_1329 |-> r_sum_acc__sum_1304
is_subsumed: sum_acc__sum_1223 ((false, (0, 0)), (true, n_1015)), snd (snd r_sum_acc__sum_1304); is_subsumed: 
snd (snd r_sum_acc__sum_1304), fst (n_1015, a_1016); is_subsumed: sum_acc__sum_1223
                                                                    ((true, (n_1015, a_1016)), (true, n_1015)), 
fst (n_1015, a_1016); is_subsumed: sum_acc__sum_1223 ((false, (0, 0)), (true, n_1015)), 
fst (n_1015, a_1016); is_subsumed: fst (n_1015, a_1016), snd (n_1015, a_1016); is_subsumed: 
snd (snd r_sum_acc__sum_1304), snd (n_1015, a_1016); is_subsumed: sum_acc__sum_1223
                                                                    ((true, (n_1015, a_1016)), (true, n_1015)), 
snd (n_1015, a_1016); is_subsumed: sum_acc__sum_1223 ((false, (0, 0)), (true, n_1015)), 
snd (n_1015, a_1016); is_subsumed: snd (n_1015, a_1016), snd (fst r_sum_acc__sum_1304); is_subsumed: 
fst (n_1015, a_1016), snd (fst r_sum_acc__sum_1304); is_subsumed: snd (snd r_sum_acc__sum_1304), 
snd (fst r_sum_acc__sum_1304); is_subsumed: sum_acc__sum_1223 ((false, (0, 0)), (true, n_1015)), 
snd (fst r_sum_acc__sum_1304); is_subsumed: fst (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024), 
snd (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024); r_sum_acc__sum_1329
elim_same_app:
let rec sum_1008 n_1009 = if n_1009 <= 0 then
                            0
                          else
                            let r_sum_1206 = sum_1008 (n_1009 - 1) in
                            n_1009 + r_sum_1206 in
let rec sum_acc_1011 n__a_1024 =
  if fst n__a_1024 <= 0 then
    snd n__a_1024
  else
    let n_1218 = fst (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) in
    let n_1219 = snd (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) in
    sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
in
let sum_acc__sum_1223 xx_1359 =
  if fst (fst xx_1359) = false then
    if fst (snd xx_1359) = false then
      ((false, 0), (false, 0))
    else
      ((false, 0), (true, sum_1008 (snd (snd xx_1359))))
  else
    if fst (snd xx_1359) = false then
      ((true, sum_acc_1011 (snd (fst xx_1359))), (false, 0))
    else
      ((true, sum_acc_1011 (snd (fst xx_1359))), (true, sum_1008 (snd (snd xx_1359))))
in
let sum_1224 x_1339 = snd (fst (sum_acc__sum_1223 ((true, x_1339), (false, 0)))) in
let sum_acc_1225 x_1330 = snd (snd (sum_acc__sum_1223 ((false, (0, 0)), (true, x_1330)))) in
let main_1014 n_1015 a_1016 =
  let r_sum_acc__sum_1304 = sum_acc__sum_1223 ((true, (n_1015, a_1016)), (true, n_1015)) in
  let r_sum_1226 = snd (snd r_sum_acc__sum_1304) in
  let n_1230 = fst (n_1015, a_1016) in
  let a_1231 = snd (n_1015, a_1016) in
  let r_sum_acc_1232 = snd (fst r_sum_acc__sum_1304) in
  if a_1016 + r_sum_1226 = r_sum_acc_1232 then
    ()
  else
    {fail} ()
in
let r_f_1237 = rand_int () in
let r_f_1239 = rand_int () in
let r_main_1240 = main_1014 r_f_1237 in
let r_r_main_1241 = r_main_1240 r_f_1239 in
()

elim_unused_branch:
let rec sum_1008 n_1009 = if n_1009 <= 0 then
                            0
                          else
                            let r_sum_1206 = sum_1008 (n_1009 - 1) in
                            n_1009 + r_sum_1206 in
let rec sum_acc_1011 n__a_1024 =
  if fst n__a_1024 <= 0 then
    snd n__a_1024
  else
    let n_1218 = fst (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) in
    let n_1219 = snd (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) in
    sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
in
let sum_acc__sum_1223 xx_1359 =
  if fst (fst xx_1359) = false then
    if fst (snd xx_1359) = false then
      ((false, 0), (false, 0))
    else
      ((false, 0), (true, sum_1008 (snd (snd xx_1359))))
  else
    if fst (snd xx_1359) = false then
      ((true, sum_acc_1011 (snd (fst xx_1359))), (false, 0))
    else
      ((true, sum_acc_1011 (snd (fst xx_1359))), (true, sum_1008 (snd (snd xx_1359))))
in
let sum_1224 x_1339 = snd (fst (sum_acc__sum_1223 ((true, x_1339), (false, 0)))) in
let sum_acc_1225 x_1330 = snd (snd (sum_acc__sum_1223 ((false, (0, 0)), (true, x_1330)))) in
let main_1014 n_1015 a_1016 =
  let r_sum_acc__sum_1304 = sum_acc__sum_1223 ((true, (n_1015, a_1016)), (true, n_1015)) in
  let r_sum_1226 = snd (snd r_sum_acc__sum_1304) in
  let n_1230 = fst (n_1015, a_1016) in
  let a_1231 = snd (n_1015, a_1016) in
  let r_sum_acc_1232 = snd (fst r_sum_acc__sum_1304) in
  if a_1016 + r_sum_1226 = r_sum_acc_1232 then
    ()
  else
    {fail} ()
in
let r_f_1237 = rand_int () in
let r_f_1239 = rand_int () in
let r_main_1240 = main_1014 r_f_1237 in
let r_r_main_1241 = r_main_1240 r_f_1239 in
()

elim_unused_let:
let rec sum_1008 n_1009 = if n_1009 <= 0 then
                            0
                          else
                            let r_sum_1206 = sum_1008 (n_1009 - 1) in
                            n_1009 + r_sum_1206 in
let rec sum_acc_1011 n__a_1024 =
  if fst n__a_1024 <= 0 then
    snd n__a_1024
  else
    sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
in
let sum_acc__sum_1223 xx_1359 =
  if fst (fst xx_1359) = false then
    if fst (snd xx_1359) = false then
      ((false, 0), (false, 0))
    else
      ((false, 0), (true, sum_1008 (snd (snd xx_1359))))
  else
    if fst (snd xx_1359) = false then
      ((true, sum_acc_1011 (snd (fst xx_1359))), (false, 0))
    else
      ((true, sum_acc_1011 (snd (fst xx_1359))), (true, sum_1008 (snd (snd xx_1359))))
in
let sum_1224 x_1339 = snd (fst (sum_acc__sum_1223 ((true, x_1339), (false, 0)))) in
let sum_acc_1225 x_1330 = snd (snd (sum_acc__sum_1223 ((false, (0, 0)), (true, x_1330)))) in
let main_1014 n_1015 a_1016 =
  let r_sum_acc__sum_1304 = sum_acc__sum_1223 ((true, (n_1015, a_1016)), (true, n_1015)) in
  let r_sum_1226 = snd (snd r_sum_acc__sum_1304) in
  let r_sum_acc_1232 = snd (fst r_sum_acc__sum_1304) in
  if a_1016 + r_sum_1226 = r_sum_acc_1232 then
    ()
  else
    {fail} ()
in
let r_f_1237 = rand_int () in
let r_f_1239 = rand_int () in
let r_main_1240 = main_1014 r_f_1237 in
let r_r_main_1241 = r_main_1240 r_f_1239 in
()

TUPLE: (true, sum_acc_1011 (snd (fst xx_1359))), (true, sum_1008 (snd (snd xx_1359)))
sum_acc_1011
sum_1008
compose:
   sum_acc_1011, if fst x_1384 <= 0 then
                   snd x_1384
                 else
                   sum_acc_1011
                     (let x1_1386 = fst x_1384 - 1 in
                      let x2_1387 = fst x_1384 + snd x_1384 in
                      (x1_1386, x2_1387));
   sum_1008, if x_1385 <= 0 then
               0
             else
               let r_sum_1206 = sum_1008 (x_1385 - 1) in
               x_1385 + r_sum_1206;

compose:
   sum_acc_1011, sum_acc_1011
                   (let x1_1386 = fst x_1384 - 1 in
                    let x2_1387 = fst x_1384 + snd x_1384 in
                    (x1_1386, x2_1387));
   sum_1008, if x_1385 <= 0 then
               0
             else
               let r_sum_1206 = sum_1008 (x_1385 - 1) in
               x_1385 + r_sum_1206;

compose:
   sum_acc_1011, sum_acc_1011
                   (let x1_1386 = fst x_1384 - 1 in
                    let x2_1387 = fst x_1384 + snd x_1384 in
                    (x1_1386, x2_1387));
   sum_1008, let r_sum_1206 = sum_1008 (x_1385 - 1) in
             x_1385 + r_sum_1206;

PB: x:sum_acc_1011
PB: x:sum_1008
CHECK: x_1385 + r_sum_1206
compose_let
sum_acc_1011:sum_acc_1011 (let x1_1386 = fst x_1384 - 1 in
                           let x2_1387 = fst x_1384 + snd x_1384 in
                           (x1_1386, x2_1387))

sum_1008:let r_sum_1206 = sum_1008 (x_1385 - 1) in
         x_1385 + r_sum_1206

PB: x:sum_1008
CHECK: x_1385 + r_sum_1393
PB: x:sum_acc_1011
compose:
   sum_acc_1011, sum_acc_1011
                   (let x1_1386 = fst x_1384 - 1 in
                    let x2_1387 = fst x_1384 + snd x_1384 in
                    (x1_1386, x2_1387));
   sum_1008, 0;

PB: x:sum_acc_1011
PB: x:sum_1008
CHECK: 0
compose_let
sum_acc_1011:sum_acc_1011 (let x1_1386 = fst x_1384 - 1 in
                           let x2_1387 = fst x_1384 + snd x_1384 in
                           (x1_1386, x2_1387))

sum_1008:0

compose:
   sum_acc_1011, snd x_1384;
   sum_1008, if x_1385 <= 0 then
               0
             else
               let r_sum_1206 = sum_1008 (x_1385 - 1) in
               x_1385 + r_sum_1206;

compose:
   sum_acc_1011, snd x_1384;
   sum_1008, let r_sum_1206 = sum_1008 (x_1385 - 1) in
             x_1385 + r_sum_1206;

PB: x:sum_acc_1011
CHECK: snd x_1384
PB: x:sum_1008
CHECK: x_1385 + r_sum_1206
compose_let
sum_acc_1011:snd x_1384

sum_1008:let r_sum_1206 = sum_1008 (x_1385 - 1) in
         x_1385 + r_sum_1206

compose:
   sum_acc_1011, snd x_1384;
   sum_1008, 0;

PB: x:sum_acc_1011
CHECK: snd x_1384
PB: x:sum_1008
CHECK: 0
compose_let
sum_acc_1011:snd x_1384

sum_1008:0

ADD_fs: sum_acc_1011, sum_1008
ADD: (sum_acc__sum_1390:((int * int) -> int -> (int * int)))
TUPLE: (true, (n_1015, a_1016)), (true, n_1015)
tupled:
let rec sum_1008 n_1009 = if n_1009 <= 0 then
                            0
                          else
                            let r_sum_1206 = sum_1008 (n_1009 - 1) in
                            n_1009 + r_sum_1206 in
let rec sum_acc_1011 n__a_1024 =
  if fst n__a_1024 <= 0 then
    snd n__a_1024
  else
    sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
in
let rec sum_acc__sum_1390 x_1384 x_1385 =
  if fst x_1384 <= 0 then
    if x_1385 <= 0 then
      let r_1415 = snd x_1384 in
      let r_1416 = 0 in
      (r_1415, r_1416)
    else
      let r_1409 = snd x_1384 in
      let r_sum_1206 = sum_1008 (x_1385 - 1) in
      let r_1410 = x_1385 + r_sum_1206 in
      (r_1409, r_1410)
  else
    if x_1385 <= 0 then
      let r_1403 =
        sum_acc_1011 (let x1_1386 = fst x_1384 - 1 in
                      let x2_1387 = fst x_1384 + snd x_1384 in
                      (x1_1386, x2_1387))
      in
      let r_1404 = 0 in
      (r_1403, r_1404)
    else
      let p_1398 =
        sum_acc__sum_1390 (let x1_1395 = fst x_1384 - 1 in
                           let x2_1396 = fst x_1384 + snd x_1384 in
                           (x1_1395, x2_1396)) (x_1385 - 1)
      in
      let r_1397 = fst p_1398 in
      let r_sum_1393 = snd p_1398 in
      let r_1394 = x_1385 + r_sum_1393 in
      (r_1397, r_1394)
in
let sum_acc__sum_1223 xx_1359 =
  if fst (fst xx_1359) = false then
    if fst (snd xx_1359) = false then
      ((false, 0), (false, 0))
    else
      ((false, 0), (true, sum_1008 (snd (snd xx_1359))))
  else
    if fst (snd xx_1359) = false then
      ((true, sum_acc_1011 (snd (fst xx_1359))), (false, 0))
    else
      let r_1419 = sum_acc__sum_1390 (snd (fst xx_1359)) (snd (snd xx_1359)) in
      ((true, fst r_1419), (true, snd r_1419))
in
let sum_1224 x_1339 = snd (fst (sum_acc__sum_1223 ((true, x_1339), (false, 0)))) in
let sum_acc_1225 x_1330 = snd (snd (sum_acc__sum_1223 ((false, (0, 0)), (true, x_1330)))) in
let main_1014 n_1015 a_1016 =
  let r_sum_acc__sum_1304 = sum_acc__sum_1223 ((true, (n_1015, a_1016)), (true, n_1015)) in
  let r_sum_1226 = snd (snd r_sum_acc__sum_1304) in
  let r_sum_acc_1232 = snd (fst r_sum_acc__sum_1304) in
  if a_1016 + r_sum_1226 = r_sum_acc_1232 then
    ()
  else
    {fail} ()
in
let r_f_1237 = rand_int () in
let r_f_1239 = rand_int () in
let r_main_1240 = main_1014 r_f_1237 in
let r_r_main_1241 = r_main_1240 r_f_1239 in
()

normalize:
let rec sum_1008 n_1009 = if n_1009 <= 0 then
                            0
                          else
                            let r_sum_1428 = sum_1008 (n_1009 - 1) in
                            n_1009 + r_sum_1428 in
let rec sum_acc_1011 n__a_1024 =
  if fst n__a_1024 <= 0 then
    snd n__a_1024
  else
    sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
in
let rec sum_acc__sum_1390 x_1384 x_1385 =
  if fst x_1384 <= 0 then
    if x_1385 <= 0 then
      (snd x_1384, 0)
    else
      let r_sum_1494 = sum_1008 (x_1385 - 1) in
      (snd x_1384, x_1385 + r_sum_1494)
  else
    if x_1385 <= 0 then
      let r_sum_acc_1485 = sum_acc_1011 (fst x_1384 - 1, fst x_1384 + snd x_1384) in
      (r_sum_acc_1485, 0)
    else
      let r_sum_acc__sum_1466 = sum_acc__sum_1390 (fst x_1384 - 1, fst x_1384 + snd x_1384) (x_1385 - 1) in
      (fst r_sum_acc__sum_1466, x_1385 + snd r_sum_acc__sum_1466)
in
let sum_acc__sum_1223 xx_1359 =
  if fst (fst xx_1359) = false then
    if fst (snd xx_1359) = false then
      ((false, 0), (false, 0))
    else
      let r_sum_1570 = sum_1008 (snd (snd xx_1359)) in
      ((false, 0), (true, r_sum_1570))
  else
    if fst (snd xx_1359) = false then
      let r_sum_acc_1539 = sum_acc_1011 (snd (fst xx_1359)) in
      ((true, r_sum_acc_1539), (false, 0))
    else
      let r_sum_acc__sum_1515 = sum_acc__sum_1390 (snd (fst xx_1359)) (snd (snd xx_1359)) in
      ((true, fst r_sum_acc__sum_1515), (true, snd r_sum_acc__sum_1515))
in
let sum_1224 x_1339 =
  let r_sum_acc__sum_1620 = sum_acc__sum_1223 ((true, x_1339), (false, 0)) in
  snd (fst r_sum_acc__sum_1620)
in
let sum_acc_1225 x_1330 =
  let r_sum_acc__sum_1644 = sum_acc__sum_1223 ((false, (0, 0)), (true, x_1330)) in
  snd (snd r_sum_acc__sum_1644)
in
let main_1014 n_1015 a_1016 =
  let r_sum_acc__sum_1664 = sum_acc__sum_1223 ((true, (n_1015, a_1016)), (true, n_1015)) in
  if a_1016 + snd (snd r_sum_acc__sum_1664) = snd (fst r_sum_acc__sum_1664) then
    ()
  else
    {fail} ()
in
let r_f_1677 = rand_int () in
let r_f_1679 = rand_int () in
let r_main_1680 = main_1014 r_f_1677 in
let r_r_main_1681 = r_main_1680 r_f_1679 in
let r_r_main_1241 = r_r_main_1681 in
()

replace[2]: r_sum_acc__sum_1664
APPS: r_sum_acc__sum_1664 = sum_acc__sum_1223 ...0... (n_1015, a_1016) ...
APPS: r_sum_acc__sum_1664 = sum_acc__sum_1223 ...1... n_1015 ...
USED: r_sum_acc__sum_1664 = sum_acc__sum_1223 ...0... (n_1015, a_1016) ...
USED: r_sum_acc__sum_1664 = sum_acc__sum_1223 ...1... n_1015 ...
MUST: r_sum_acc__sum_1664 = sum_acc__sum_1223 ...1... n_1015 ...
MUST: r_sum_acc__sum_1664 = sum_acc__sum_1223 ...0... (n_1015, a_1016) ...
NEW: r_sum_acc__sum_1682 = sum_acc__sum_1223 ((true, (n_1015, a_1016)), (true, n_1015))
replace_app:
let rec sum_1008 n_1009 = if n_1009 <= 0 then
                            0
                          else
                            let r_sum_1428 = sum_1008 (n_1009 - 1) in
                            n_1009 + r_sum_1428 in
let rec sum_acc_1011 n__a_1024 =
  if fst n__a_1024 <= 0 then
    snd n__a_1024
  else
    sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
in
let rec sum_acc__sum_1390 x_1384 x_1385 =
  if fst x_1384 <= 0 then
    if x_1385 <= 0 then
      (snd x_1384, 0)
    else
      let r_sum_1494 = sum_1008 (x_1385 - 1) in
      (snd x_1384, x_1385 + r_sum_1494)
  else
    if x_1385 <= 0 then
      let r_sum_acc_1485 = sum_acc_1011 (fst x_1384 - 1, fst x_1384 + snd x_1384) in
      (r_sum_acc_1485, 0)
    else
      let r_sum_acc__sum_1466 = sum_acc__sum_1390 (fst x_1384 - 1, fst x_1384 + snd x_1384) (x_1385 - 1) in
      (fst r_sum_acc__sum_1466, x_1385 + snd r_sum_acc__sum_1466)
in
let sum_acc__sum_1223 xx_1359 =
  if fst (fst xx_1359) = false then
    if fst (snd xx_1359) = false then
      ((false, 0), (false, 0))
    else
      let r_sum_1570 = sum_1008 (snd (snd xx_1359)) in
      ((false, 0), (true, r_sum_1570))
  else
    if fst (snd xx_1359) = false then
      let r_sum_acc_1539 = sum_acc_1011 (snd (fst xx_1359)) in
      ((true, r_sum_acc_1539), (false, 0))
    else
      let r_sum_acc__sum_1515 = sum_acc__sum_1390 (snd (fst xx_1359)) (snd (snd xx_1359)) in
      ((true, fst r_sum_acc__sum_1515), (true, snd r_sum_acc__sum_1515))
in
let sum_1224 x_1339 =
  let r_sum_acc__sum_1620 = sum_acc__sum_1223 ((true, x_1339), (false, 0)) in
  snd (fst r_sum_acc__sum_1620)
in
let sum_acc_1225 x_1330 =
  let r_sum_acc__sum_1644 = sum_acc__sum_1223 ((false, (0, 0)), (true, x_1330)) in
  snd (snd r_sum_acc__sum_1644)
in
let main_1014 n_1015 a_1016 =
  let r_sum_acc__sum_1664 = sum_acc__sum_1223 ((true, (n_1015, a_1016)), (true, n_1015)) in
  let r_sum_acc__sum_1682 = sum_acc__sum_1223 ((true, (n_1015, a_1016)), (true, n_1015)) in
  if a_1016 + snd (snd r_sum_acc__sum_1682) = snd (fst r_sum_acc__sum_1682) then
    ()
  else
    {fail} ()
in
let r_f_1677 = rand_int () in
let r_f_1679 = rand_int () in
let r_main_1680 = main_1014 r_f_1677 in
let r_r_main_1681 = r_main_1680 r_f_1679 in
let r_r_main_1241 = r_r_main_1681 in
()

is_subsumed: rand_int (), rand_int (); is_subsumed: rand_int (), main_1014 r_f_1677; is_subsumed: 
rand_int (), r_main_1680 r_f_1679; is_subsumed: main_1014 r_f_1677, r_r_main_1681; is_subsumed: 
rand_int (), r_r_main_1681; is_subsumed: rand_int (), r_r_main_1681; is_subsumed: 
sum_acc__sum_1223 ((true, (n_1015, a_1016)), (true, n_1015)), sum_acc__sum_1223
                                                                ((true, (n_1015, a_1016)), (true, n_1015)); r_sum_acc__sum_1664 |-> r_sum_acc__sum_1682
r_sum_acc__sum_1664
elim_unnecessary:
let rec sum_1008 n_1009 = if n_1009 <= 0 then
                            0
                          else
                            let r_sum_1428 = sum_1008 (n_1009 - 1) in
                            n_1009 + r_sum_1428 in
let rec sum_acc_1011 n__a_1024 =
  if fst n__a_1024 <= 0 then
    snd n__a_1024
  else
    sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
in
let rec sum_acc__sum_1390 x_1384 x_1385 =
  if fst x_1384 <= 0 then
    if x_1385 <= 0 then
      (snd x_1384, 0)
    else
      let r_sum_1494 = sum_1008 (x_1385 - 1) in
      (snd x_1384, x_1385 + r_sum_1494)
  else
    if x_1385 <= 0 then
      let r_sum_acc_1485 = sum_acc_1011 (fst x_1384 - 1, fst x_1384 + snd x_1384) in
      (r_sum_acc_1485, 0)
    else
      let r_sum_acc__sum_1466 = sum_acc__sum_1390 (fst x_1384 - 1, fst x_1384 + snd x_1384) (x_1385 - 1) in
      (fst r_sum_acc__sum_1466, x_1385 + snd r_sum_acc__sum_1466)
in
let sum_acc__sum_1223 xx_1359 =
  if fst (fst xx_1359) = false then
    if fst (snd xx_1359) = false then
      ((false, 0), (false, 0))
    else
      let r_sum_1570 = sum_1008 (snd (snd xx_1359)) in
      ((false, 0), (true, r_sum_1570))
  else
    if fst (snd xx_1359) = false then
      let r_sum_acc_1539 = sum_acc_1011 (snd (fst xx_1359)) in
      ((true, r_sum_acc_1539), (false, 0))
    else
      let r_sum_acc__sum_1515 = sum_acc__sum_1390 (snd (fst xx_1359)) (snd (snd xx_1359)) in
      ((true, fst r_sum_acc__sum_1515), (true, snd r_sum_acc__sum_1515))
in
let sum_1224 x_1339 =
  let r_sum_acc__sum_1620 = sum_acc__sum_1223 ((true, x_1339), (false, 0)) in
  snd (fst r_sum_acc__sum_1620)
in
let sum_acc_1225 x_1330 =
  let r_sum_acc__sum_1644 = sum_acc__sum_1223 ((false, (0, 0)), (true, x_1330)) in
  snd (snd r_sum_acc__sum_1644)
in
let main_1014 n_1015 a_1016 =
  let r_sum_acc__sum_1682 = sum_acc__sum_1223 ((true, (n_1015, a_1016)), (true, n_1015)) in
  if a_1016 + snd (snd r_sum_acc__sum_1682) = snd (fst r_sum_acc__sum_1682) then
    ()
  else
    {fail} ()
in
let r_f_1677 = rand_int () in
let r_f_1679 = rand_int () in
let r_main_1680 = main_1014 r_f_1677 in
let r_r_main_1681 = r_main_1680 r_f_1679 in
let r_r_main_1241 = r_r_main_1681 in
()

inline_next_redex:
let rec sum_1008 n_1009 = if n_1009 <= 0 then
                            0
                          else
                            let r_sum_1428 = sum_1008 (n_1009 - 1) in
                            n_1009 + r_sum_1428 in
let rec sum_acc_1011 n__a_1024 =
  if fst n__a_1024 <= 0 then
    snd n__a_1024
  else
    sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
in
let rec sum_acc__sum_1390 x_1384 x_1385 =
  if fst x_1384 <= 0 then
    if x_1385 <= 0 then
      (snd x_1384, 0)
    else
      let r_sum_1494 = sum_1008 (x_1385 - 1) in
      (snd x_1384, x_1385 + r_sum_1494)
  else
    if x_1385 <= 0 then
      (sum_acc_1011 (fst x_1384 - 1, fst x_1384 + snd x_1384), 0)
    else
      let r_sum_acc__sum_1466 = sum_acc__sum_1390 (fst x_1384 - 1, fst x_1384 + snd x_1384) (x_1385 - 1) in
      (fst r_sum_acc__sum_1466, x_1385 + snd r_sum_acc__sum_1466)
in
let sum_acc__sum_1223 xx_1359 =
  if fst (fst xx_1359) = false then
    if fst (snd xx_1359) = false then
      ((false, 0), (false, 0))
    else
      ((false, 0), (true, sum_1008 (snd (snd xx_1359))))
  else
    if fst (snd xx_1359) = false then
      ((true, sum_acc_1011 (snd (fst xx_1359))), (false, 0))
    else
      let r_sum_acc__sum_1515 = sum_acc__sum_1390 (snd (fst xx_1359)) (snd (snd xx_1359)) in
      ((true, fst r_sum_acc__sum_1515), (true, snd r_sum_acc__sum_1515))
in
let sum_1224 x_1339 = snd (fst (sum_acc__sum_1223 ((true, x_1339), (false, 0)))) in
let sum_acc_1225 x_1330 = snd (snd (sum_acc__sum_1223 ((false, (0, 0)), (true, x_1330)))) in
let main_1014 n_1015 a_1016 =
  let r_sum_acc__sum_1682 = sum_acc__sum_1223 ((true, (n_1015, a_1016)), (true, n_1015)) in
  if a_1016 + snd (snd r_sum_acc__sum_1682) = snd (fst r_sum_acc__sum_1682) then
    ()
  else
    {fail} ()
in
let r_f_1677 = rand_int () in
let r_f_1679 = rand_int () in
let r_main_1680 = main_1014 r_f_1677 in
let r_r_main_1681 = r_main_1680 r_f_1679 in
let r_r_main_1241 = r_r_main_1681 in
()

reduce_bottomh:
let rec sum_1008 n_1009 = if n_1009 <= 0 then
                            0
                          else
                            let r_sum_1428 = sum_1008 (n_1009 - 1) in
                            n_1009 + r_sum_1428 in
let rec sum_acc_1011 n__a_1024 =
  if fst n__a_1024 <= 0 then
    snd n__a_1024
  else
    sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
in
let rec sum_acc__sum_1390 x_1384 x_1385 =
  if fst x_1384 <= 0 then
    if x_1385 <= 0 then
      (snd x_1384, 0)
    else
      let r_sum_1494 = sum_1008 (x_1385 - 1) in
      (snd x_1384, x_1385 + r_sum_1494)
  else
    if x_1385 <= 0 then
      (sum_acc_1011 (fst x_1384 - 1, fst x_1384 + snd x_1384), 0)
    else
      let r_sum_acc__sum_1466 = sum_acc__sum_1390 (fst x_1384 - 1, fst x_1384 + snd x_1384) (x_1385 - 1) in
      (fst r_sum_acc__sum_1466, x_1385 + snd r_sum_acc__sum_1466)
in
let sum_acc__sum_1223 xx_1359 =
  if fst (fst xx_1359) = false then
    if fst (snd xx_1359) = false then
      ((false, 0), (false, 0))
    else
      ((false, 0), (true, sum_1008 (snd (snd xx_1359))))
  else
    if fst (snd xx_1359) = false then
      ((true, sum_acc_1011 (snd (fst xx_1359))), (false, 0))
    else
      let r_sum_acc__sum_1515 = sum_acc__sum_1390 (snd (fst xx_1359)) (snd (snd xx_1359)) in
      ((true, fst r_sum_acc__sum_1515), (true, snd r_sum_acc__sum_1515))
in
let sum_1224 x_1339 = snd (fst (sum_acc__sum_1223 ((true, x_1339), (false, 0)))) in
let sum_acc_1225 x_1330 = snd (snd (sum_acc__sum_1223 ((false, (0, 0)), (true, x_1330)))) in
let main_1014 n_1015 a_1016 =
  let r_sum_acc__sum_1682 = sum_acc__sum_1223 ((true, (n_1015, a_1016)), (true, n_1015)) in
  if a_1016 + snd (snd r_sum_acc__sum_1682) = snd (fst r_sum_acc__sum_1682) then
    ()
  else
    {fail} ()
in
let r_f_1677 = rand_int () in
let r_f_1679 = rand_int () in
let r_main_1680 = main_1014 r_f_1677 in
let r_r_main_1681 = r_main_1680 r_f_1679 in
let r_r_main_1241 = r_r_main_1681 in
()

tupling:
 let rec sum_1008 (n_1009:int) =
   if n_1009 <= 0 then
     0
   else
     let r_sum_1428 = sum_1008 (n_1009 - 1) in
     n_1009 + r_sum_1428
 in
 let rec sum_acc_1011 (n__a_1024:(int * int)) =
   if fst n__a_1024 <= 0 then
     snd n__a_1024
   else
     sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024)
 in
 let rec sum_acc__sum_1390 (x_1384:(int * int)) (x_1385:int) =
   if fst x_1384 <= 0 then
     if x_1385 <= 0 then
       (snd x_1384, 0)
     else
       let r_sum_1494 = sum_1008 (x_1385 - 1) in
       (snd x_1384, x_1385 + r_sum_1494)
   else
     if x_1385 <= 0 then
       (sum_acc_1011 (fst x_1384 - 1, fst x_1384 + snd x_1384), 0)
     else
       let r_sum_acc__sum_1466 = sum_acc__sum_1390 (fst x_1384 - 1, fst x_1384 + snd x_1384) (x_1385 - 1) in
       (fst r_sum_acc__sum_1466, x_1385 + snd r_sum_acc__sum_1466)
 in
 let sum_acc__sum_1223 (xx_1359:((bool * (int * int)) * (bool * int))) =
   if fst (fst xx_1359) = false then
     if fst (snd xx_1359) = false then
       ((false, 0), (false, 0))
     else
       ((false, 0), (true, sum_1008 (snd (snd xx_1359))))
   else
     if fst (snd xx_1359) = false then
       ((true, sum_acc_1011 (snd (fst xx_1359))), (false, 0))
     else
       let r_sum_acc__sum_1515 = sum_acc__sum_1390 (snd (fst xx_1359)) (snd (snd xx_1359)) in
       ((true, fst r_sum_acc__sum_1515), (true, snd r_sum_acc__sum_1515))
 in
 let sum_1224 (x_1339:(int * int)) = snd (fst (sum_acc__sum_1223 ((true, x_1339), (false, 0)))) in
 let sum_acc_1225 (x_1330:int) = snd (snd (sum_acc__sum_1223 ((false, (0, 0)), (true, x_1330)))) in
 let main_1014 (n_1015:int) (a_1016:int) =
   let r_sum_acc__sum_1682 = sum_acc__sum_1223 ((true, (n_1015, a_1016)), (true, n_1015)) in
   if a_1016 + snd (snd r_sum_acc__sum_1682) = snd (fst r_sum_acc__sum_1682) then
     ()
   else
     {fail} ()
 in
 let r_f_1677 = rand_int () in
 let r_f_1679 = rand_int () in
 let r_main_1680 = main_1014 r_f_1677 in
 let r_r_main_1681 = r_main_1680 r_f_1679 in
 let r_r_main_1241 = r_r_main_1681 in
 ()

CPS:
 let rec sum_1008 (n_1009:int) (k_sum_1700:(int -> X)) =
   if n_1009 <= 0 then
     k_sum_1700 0
   else
     let r_sum_1428 (k_sum_r_sum_1707:(int -> X)) = sum_1008 (n_1009 - 1) k_sum_r_sum_1707 in
     r_sum_1428 (fun (r_sum_1713:int) -> k_sum_1700 (n_1009 + r_sum_1713))
 in
 let rec sum_acc_1011 (n__a_1024:(int * int)) (k_sum_acc_1724:(int -> X)) =
   if fst n__a_1024 <= 0 then
     k_sum_acc_1724 (snd n__a_1024)
   else
     sum_acc_1011 (fst n__a_1024 - 1, fst n__a_1024 + snd n__a_1024) k_sum_acc_1724
 in
 let rec sum_acc__sum_1390 (x_1384:(int * int)) (x_1385:int) (k_sum_acc__sum_1751:((int * int) -> X)) =
   if fst x_1384 <= 0 then
     if x_1385 <= 0 then
       k_sum_acc__sum_1751 (snd x_1384, 0)
     else
       let r_sum_1494 (k_sum_acc__sum_r_sum_1764:(int -> X)) = sum_1008 (x_1385 - 1) k_sum_acc__sum_r_sum_1764 in
       r_sum_1494 (fun (r_sum_1776:int) -> k_sum_acc__sum_1751 (snd x_1384, x_1385 + r_sum_1776))
   else
     if x_1385 <= 0 then
       sum_acc_1011 (fst x_1384 - 1, fst x_1384 + snd x_1384) (fun (x_2212:int) -> k_sum_acc__sum_1751 (x_2212, 0))
     else
       let r_sum_acc__sum_1466 (k_sum_acc__sum_r_sum_acc__sum_1827:((int * int) -> X)) =
         sum_acc__sum_1390 (fst x_1384 - 1, fst x_1384 + snd x_1384) (x_1385 - 1) k_sum_acc__sum_r_sum_acc__sum_1827
       in
       r_sum_acc__sum_1466
         (fun (r_sum_acc__sum_1839:(int * int)) ->
            k_sum_acc__sum_1751 (fst r_sum_acc__sum_1839, x_1385 + snd r_sum_acc__sum_1839))
 in
 let
   sum_acc__sum_1223 (xx_1359:((bool * (int * int)) * (bool * int))) 
                    (k_sum_acc__sum_1855:(((bool * int) * (bool * int)) -> X)) =
   if fst (fst xx_1359) = false then
     if fst (snd xx_1359) = false then
       k_sum_acc__sum_1855 ((false, 0), (false, 0))
     else
       sum_1008 (snd (snd xx_1359)) (fun (x_2230:int) -> k_sum_acc__sum_1855 ((false, 0), (true, x_2230)))
   else
     if fst (snd xx_1359) = false then
       sum_acc_1011 (snd (fst xx_1359)) (fun (x_2227:int) -> k_sum_acc__sum_1855 ((true, x_2227), (false, 0)))
     else
       let r_sum_acc__sum_1515 (k_sum_acc__sum_r_sum_acc__sum_1941:((int * int) -> X)) =
         sum_acc__sum_1390 (snd (fst xx_1359)) (snd (snd xx_1359)) k_sum_acc__sum_r_sum_acc__sum_1941
       in
       r_sum_acc__sum_1515
         (fun (r_sum_acc__sum_1965:(int * int)) ->
            k_sum_acc__sum_1855 ((true, fst r_sum_acc__sum_1965), (true, snd r_sum_acc__sum_1965)))
 in
 let main_1014 (n_1015:int) (a_1016:int) (k_main_2068:(unit -> X)) =
   let r_sum_acc__sum_1682 (k_main_r_sum_acc__sum_2099:(((bool * int) * (bool * int)) -> X)) =
     sum_acc__sum_1223 ((true, (n_1015, a_1016)), (true, n_1015)) k_main_r_sum_acc__sum_2099
   in
   r_sum_acc__sum_1682
     (fun (r_sum_acc__sum_2116:((bool * int) * (bool * int))) ->
        (if a_1016 + snd (snd r_sum_acc__sum_2116) = snd (fst r_sum_acc__sum_2116) then
           k_main_2068 ()
         else
           {|fail|} () k_main_2068))
 in
 let r_f_1677 (k_r_f_2127:(int -> X)) = rand_int_cps () k_r_f_2127 in
 r_f_1677
   (fun (r_f_2172:int) ->
      (let r_f_1679 (k_r_f_2139:(int -> X)) = rand_int_cps () k_r_f_2139 in
       r_f_1679
         (fun (r_f_2171:int) ->
            (let r_r_main_1681 (k_r_r_main_2160:(unit -> X)) = (main_1014 r_f_2172) r_f_2171 k_r_r_main_2160 in
             r_r_main_1681 (fun (r_r_main_2166:unit) -> {end})))))

remove_pair:
 let rec sum_1008 (n_1009:int) (k_sum_1700:(int -> X)) =
   if n_1009 <= 0 then
     k_sum_1700 0
   else
     let r_sum_1428 (k_sum_r_sum_1707:(int -> X)) = sum_1008 (n_1009 - 1) k_sum_r_sum_1707 in
     r_sum_1428 (fun (r_sum_1713:int) -> k_sum_1700 (n_1009 + r_sum_1713))
 in
 let rec sum_acc_1011 (n__a0_1024:int) (n__a1_1024:int) (k_sum_acc_1724:(int -> X)) =
   if n__a0_1024 <= 0 then
     k_sum_acc_1724 n__a1_1024
   else
     sum_acc_1011 (n__a0_1024 - 1) (n__a0_1024 + n__a1_1024) k_sum_acc_1724
 in
 let rec sum_acc__sum_1390 (x0_1384:int) (x1_1384:int) (x_1385:int) (k_sum_acc__sum_1751:(int -> int -> X)) =
   if x0_1384 <= 0 then
     if x_1385 <= 0 then
       k_sum_acc__sum_1751 x1_1384 0
     else
       let r_sum_1494 (k_sum_acc__sum_r_sum_1764:(int -> X)) = sum_1008 (x_1385 - 1) k_sum_acc__sum_r_sum_1764 in
       r_sum_1494 (fun (r_sum_1776:int) -> k_sum_acc__sum_1751 x1_1384 (x_1385 + r_sum_1776))
   else
     if x_1385 <= 0 then
       sum_acc_1011 (x0_1384 - 1) (x0_1384 + x1_1384) (fun (x_2212:int) -> k_sum_acc__sum_1751 x_2212 0)
     else
       let r_sum_acc__sum_1466 (k_sum_acc__sum_r_sum_acc__sum_1827:(int -> int -> X)) =
         sum_acc__sum_1390 (x0_1384 - 1) (x0_1384 + x1_1384) (x_1385 - 1) k_sum_acc__sum_r_sum_acc__sum_1827
       in
       r_sum_acc__sum_1466
         (fun (r_sum_acc__sum0_1839:int) ->
            fun (r_sum_acc__sum1_1839:int) -> k_sum_acc__sum_1751 r_sum_acc__sum0_1839 (x_1385 + r_sum_acc__sum1_1839))
 in
 let
   sum_acc__sum_1223 (xx00_1359:bool) (xx010_1359:int) (xx011_1359:int) (xx10_1359:bool) (xx11_1359:int) 
                    (k_sum_acc__sum_1855:(bool -> int -> bool -> int -> X)) =
   if xx00_1359 = false then
     if xx10_1359 = false then
       k_sum_acc__sum_1855 false 0 false 0
     else
       sum_1008 xx11_1359 (fun (x_2230:int) -> k_sum_acc__sum_1855 false 0 true x_2230)
   else
     if xx10_1359 = false then
       sum_acc_1011 xx010_1359 xx011_1359 (fun (x_2227:int) -> k_sum_acc__sum_1855 true x_2227 false 0)
     else
       let r_sum_acc__sum_1515 (k_sum_acc__sum_r_sum_acc__sum_1941:(int -> int -> X)) =
         sum_acc__sum_1390 xx010_1359 xx011_1359 xx11_1359 k_sum_acc__sum_r_sum_acc__sum_1941
       in
       r_sum_acc__sum_1515
         (fun (r_sum_acc__sum0_1965:int) ->
            fun (r_sum_acc__sum1_1965:int) -> k_sum_acc__sum_1855 true r_sum_acc__sum0_1965 true r_sum_acc__sum1_1965)
 in
 let main_1014 (n_1015:int) (a_1016:int) (k_main_2068:(unit -> X)) =
   let r_sum_acc__sum_1682 (k_main_r_sum_acc__sum_2099:(bool -> int -> bool -> int -> X)) =
     sum_acc__sum_1223 true n_1015 a_1016 true n_1015 k_main_r_sum_acc__sum_2099
   in
   r_sum_acc__sum_1682
     (fun (r_sum_acc__sum00_2116:bool) ->
        fun (r_sum_acc__sum01_2116:int) ->
          fun (r_sum_acc__sum10_2116:bool) ->
            fun (r_sum_acc__sum11_2116:int) ->
              (if a_1016 + r_sum_acc__sum11_2116 = r_sum_acc__sum01_2116 then
                 k_main_2068 ()
               else
                 {|fail|} () k_main_2068))
 in
 let r_f_1677 (k_r_f_2127:(int -> X)) = rand_int_cps () k_r_f_2127 in
 r_f_1677
   (fun (r_f_2172:int) ->
      (let r_f_1679 (k_r_f_2139:(int -> X)) = rand_int_cps () k_r_f_2139 in
       r_f_1679
         (fun (r_f_2171:int) ->
            (let r_r_main_1681 (k_r_r_main_2160:(unit -> X)) = main_1014 r_f_2172 r_f_2171 k_r_r_main_2160 in
             r_r_main_1681 (fun (r_r_main_2166:unit) -> {end})))))

Program with abstraction types (CEGAR-cycle 0)::
Main: main_2287
  main_2287 -> (r_f_1677 f_2296);;
  br_sum_acc__sum_2299 b_2300 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751 when b_2300 ->
      (k_sum_acc__sum_1751 x1_1384 0);;
  br_sum_acc__sum_2299 b_2300 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751 when (
      not b_2300) ->
      (r_sum_1494 x0_1384 x1_1384 x_1385 (f_sum_acc__sum_2289 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751));;
  br_sum_acc__sum_2301 b_2302 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751 when b_2302 ->
      (sum_acc_1011 (x0_1384 - 1) (x0_1384 + x1_1384) (f_sum_acc__sum_2290 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751));;
  br_sum_acc__sum_2301 b_2302 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751 when (
      not b_2302) ->
      (r_sum_acc__sum_1466 x0_1384 x1_1384 x_1385 (f_sum_acc__sum_2291 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751));;
  br_sum_acc__sum_2303 b_2304 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855 when b_2304 ->
      (k_sum_acc__sum_1855 false 0 false 0);;
  br_sum_acc__sum_2303 b_2304 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855 when (
      not b_2304) ->
      (sum_1008 xx11_1359 (f_sum_acc__sum_2292 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855));;
  br_sum_acc__sum_2305 b_2306 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855 when b_2306 ->
      (sum_acc_1011 xx010_1359 xx011_1359
        (f_sum_acc__sum_2293 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855));;
  br_sum_acc__sum_2305 b_2306 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855 when (
      not b_2306) ->
      (r_sum_acc__sum_1515 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359
        (f_sum_acc__sum_2294 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855));;
  f_2296 r_f_2172 -> (r_f_1679 r_f_2172 (f_2297 r_f_2172));;
  f_2297 r_f_2172 r_f_2171 -> (r_r_main_1681 r_f_2171 r_f_2172 (f_2298 r_f_2171 r_f_2172));;
  f_2298 r_f_2171 r_f_2172 r_r_main_2166 -> end;;
  f_main_2295 a_1016 n_1015 k_main_2068 r_sum_acc__sum00_2116 r_sum_acc__sum01_2116 r_sum_acc__sum10_2116 
  r_sum_acc__sum11_2116 when ((a_1016 + r_sum_acc__sum11_2116) = r_sum_acc__sum01_2116) -> (
      k_main_2068 ());;
  f_main_2295 a_1016 n_1015 k_main_2068 r_sum_acc__sum00_2116 r_sum_acc__sum01_2116 r_sum_acc__sum10_2116 
  r_sum_acc__sum11_2116 when (not ((a_1016 + r_sum_acc__sum11_2116) = r_sum_acc__sum01_2116)) ->
      (fail_2307 true k_main_2068);;
  f_sum_2288 n_1009 k_sum_1700 r_sum_1713 -> (k_sum_1700 (n_1009 + r_sum_1713));;
  f_sum_acc__sum_2289 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751 r_sum_1776 ->
      (k_sum_acc__sum_1751 x1_1384 (x_1385 + r_sum_1776));;
  f_sum_acc__sum_2290 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751 x_2212 -> (k_sum_acc__sum_1751 x_2212 0);;
  f_sum_acc__sum_2291 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751 r_sum_acc__sum0_1839 r_sum_acc__sum1_1839 ->
      (k_sum_acc__sum_1751 r_sum_acc__sum0_1839 (x_1385 + r_sum_acc__sum1_1839));;
  f_sum_acc__sum_2292 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855 x_2230 ->
      (k_sum_acc__sum_1855 false 0 true x_2230);;
  f_sum_acc__sum_2293 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855 x_2227 ->
      (k_sum_acc__sum_1855 true x_2227 false 0);;
  f_sum_acc__sum_2294 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855 r_sum_acc__sum0_1965 
  r_sum_acc__sum1_1965 -> (k_sum_acc__sum_1855 true r_sum_acc__sum0_1965 true r_sum_acc__sum1_1965);;
  fail_2307 b k -> {fail} => (k ());;
  main_1014 n_1015 a_1016 k_main_2068 -> (r_sum_acc__sum_1682 a_1016 n_1015 (f_main_2295 a_1016 n_1015 k_main_2068));;
  r_f_1677 k_r_f_2127 -> (rand_int k_r_f_2127);;
  r_f_1679 r_f_2172 k_r_f_2139 -> (rand_int k_r_f_2139);;
  r_r_main_1681 r_f_2171 r_f_2172 k_r_r_main_2160 -> (main_1014 r_f_2172 r_f_2171 k_r_r_main_2160);;
  r_sum_1428 n_1009 k_sum_r_sum_1707 -> (sum_1008 (n_1009 - 1) k_sum_r_sum_1707);;
  r_sum_1494 x0_1384 x1_1384 x_1385 k_sum_acc__sum_r_sum_1764 -> (sum_1008 (x_1385 - 1) k_sum_acc__sum_r_sum_1764);;
  r_sum_acc__sum_1466 x0_1384 x1_1384 x_1385 k_sum_acc__sum_r_sum_acc__sum_1827 ->
      (sum_acc__sum_1390 (x0_1384 - 1) (x0_1384 + x1_1384) (x_1385 - 1) k_sum_acc__sum_r_sum_acc__sum_1827);;
  r_sum_acc__sum_1515 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_r_sum_acc__sum_1941 ->
      (sum_acc__sum_1390 xx010_1359 xx011_1359 xx11_1359 k_sum_acc__sum_r_sum_acc__sum_1941);;
  r_sum_acc__sum_1682 a_1016 n_1015 k_main_r_sum_acc__sum_2099 ->
      (sum_acc__sum_1223 true n_1015 a_1016 true n_1015 k_main_r_sum_acc__sum_2099);;
  sum_1008 n_1009 k_sum_1700 when (n_1009 <= 0) -> (k_sum_1700 0);;
  sum_1008 n_1009 k_sum_1700 when (not (n_1009 <= 0)) -> (r_sum_1428 n_1009 (f_sum_2288 n_1009 k_sum_1700));;
  sum_acc_1011 n__a0_1024 n__a1_1024 k_sum_acc_1724 when (n__a0_1024 <= 0) -> (k_sum_acc_1724 n__a1_1024);;
  sum_acc_1011 n__a0_1024 n__a1_1024 k_sum_acc_1724 when (not (n__a0_1024 <= 0)) ->
      (sum_acc_1011 (n__a0_1024 - 1) (n__a0_1024 + n__a1_1024) k_sum_acc_1724);;
  sum_acc__sum_1223 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855 when (
      xx00_1359 <=> false) ->
      (br_sum_acc__sum_2303 (xx10_1359 <=> false) xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359
        k_sum_acc__sum_1855);;
  sum_acc__sum_1223 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855 when (
      not (xx00_1359 <=> false)) ->
      (br_sum_acc__sum_2305 (xx10_1359 <=> false) xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359
        k_sum_acc__sum_1855);;
  sum_acc__sum_1390 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751 when (
      x0_1384 <= 0) -> (br_sum_acc__sum_2299 (x_1385 <= 0) x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751);;
  sum_acc__sum_1390 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751 when (
      not (x0_1384 <= 0)) -> (br_sum_acc__sum_2301 (x_1385 <= 0) x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751);;
Types:
  main_2287 : X
  fail_2307 : (bool -> (unit -> X) -> X)
  sum_1008 : (int -> (int -> X) -> X)
  sum_acc_1011 : (int -> int -> (int -> X) -> X)
  sum_acc__sum_1390 : (int -> int -> int -> (int -> int -> X) -> X)

(0-1) Abstracting ... DONE!

(0-2) Checking HORS ... DONE!

Error trace::
  main_2287 ... --> 
  r_f_1677 ... --> 
  f_2296 ... --> 
  r_f_1679 ... --> 
  f_2297 ... --> 
  r_r_main_1681 ... --> 
  main_1014 ... --> 
  r_sum_acc__sum_1682 ... --> 
  sum_acc__sum_1223 [2/2] ... --> 
  br_sum_acc__sum_2305 [2/2] ... --> 
  r_sum_acc__sum_1515 ... --> 
  sum_acc__sum_1390 [1/2] ... --> 
  br_sum_acc__sum_2299 [1/2] ... --> 
  f_sum_acc__sum_2294 ... --> 
  f_main_2295 [2/2] ... --> 
  fail_2307 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 0; 0; 0; 0; 1; 0

(0-3) Checking counterexample ... DONE!

(0-4) Discovering predicates ... 
DONE!

Prefix of spurious counterexample::
0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 0; 0; 0; 0; 1

Program with abstraction types (CEGAR-cycle 1)::
Main: main_2287
  main_2287 -> (r_f_1677 f_2296);;
  br_sum_acc__sum_2299 b_2300 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751 when b_2300 ->
      (k_sum_acc__sum_1751 x1_1384 0);;
  br_sum_acc__sum_2299 b_2300 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751 when (
      not b_2300) ->
      (r_sum_1494 x0_1384 x1_1384 x_1385 (f_sum_acc__sum_2289 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751));;
  br_sum_acc__sum_2301 b_2302 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751 when b_2302 ->
      (sum_acc_1011 (x0_1384 - 1) (x0_1384 + x1_1384) (f_sum_acc__sum_2290 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751));;
  br_sum_acc__sum_2301 b_2302 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751 when (
      not b_2302) ->
      (r_sum_acc__sum_1466 x0_1384 x1_1384 x_1385 (f_sum_acc__sum_2291 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751));;
  br_sum_acc__sum_2303 b_2304 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855 when b_2304 ->
      (k_sum_acc__sum_1855 false 0 false 0);;
  br_sum_acc__sum_2303 b_2304 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855 when (
      not b_2304) ->
      (sum_1008 xx11_1359 (f_sum_acc__sum_2292 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855));;
  br_sum_acc__sum_2305 b_2306 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855 when b_2306 ->
      (sum_acc_1011 xx010_1359 xx011_1359
        (f_sum_acc__sum_2293 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855));;
  br_sum_acc__sum_2305 b_2306 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855 when (
      not b_2306) ->
      (r_sum_acc__sum_1515 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359
        (f_sum_acc__sum_2294 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855));;
  f_2296 r_f_2172 -> (r_f_1679 r_f_2172 (f_2297 r_f_2172));;
  f_2297 r_f_2172 r_f_2171 -> (r_r_main_1681 r_f_2171 r_f_2172 (f_2298 r_f_2171 r_f_2172));;
  f_2298 r_f_2171 r_f_2172 r_r_main_2166 -> end;;
  f_main_2295 a_1016 n_1015 k_main_2068 r_sum_acc__sum00_2116 r_sum_acc__sum01_2116 r_sum_acc__sum10_2116 
  r_sum_acc__sum11_2116 when ((a_1016 + r_sum_acc__sum11_2116) = r_sum_acc__sum01_2116) -> (
      k_main_2068 ());;
  f_main_2295 a_1016 n_1015 k_main_2068 r_sum_acc__sum00_2116 r_sum_acc__sum01_2116 r_sum_acc__sum10_2116 
  r_sum_acc__sum11_2116 when (not ((a_1016 + r_sum_acc__sum11_2116) = r_sum_acc__sum01_2116)) ->
      (fail_2307 true k_main_2068);;
  f_sum_2288 n_1009 k_sum_1700 r_sum_1713 -> (k_sum_1700 (n_1009 + r_sum_1713));;
  f_sum_acc__sum_2289 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751 r_sum_1776 ->
      (k_sum_acc__sum_1751 x1_1384 (x_1385 + r_sum_1776));;
  f_sum_acc__sum_2290 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751 x_2212 -> (k_sum_acc__sum_1751 x_2212 0);;
  f_sum_acc__sum_2291 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751 r_sum_acc__sum0_1839 r_sum_acc__sum1_1839 ->
      (k_sum_acc__sum_1751 r_sum_acc__sum0_1839 (x_1385 + r_sum_acc__sum1_1839));;
  f_sum_acc__sum_2292 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855 x_2230 ->
      (k_sum_acc__sum_1855 false 0 true x_2230);;
  f_sum_acc__sum_2293 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855 x_2227 ->
      (k_sum_acc__sum_1855 true x_2227 false 0);;
  f_sum_acc__sum_2294 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855 r_sum_acc__sum0_1965 
  r_sum_acc__sum1_1965 -> (k_sum_acc__sum_1855 true r_sum_acc__sum0_1965 true r_sum_acc__sum1_1965);;
  fail_2307 b k -> {fail} => (k ());;
  main_1014 n_1015 a_1016 k_main_2068 -> (r_sum_acc__sum_1682 a_1016 n_1015 (f_main_2295 a_1016 n_1015 k_main_2068));;
  r_f_1677 k_r_f_2127 -> (rand_int k_r_f_2127);;
  r_f_1679 r_f_2172 k_r_f_2139 -> (rand_int k_r_f_2139);;
  r_r_main_1681 r_f_2171 r_f_2172 k_r_r_main_2160 -> (main_1014 r_f_2172 r_f_2171 k_r_r_main_2160);;
  r_sum_1428 n_1009 k_sum_r_sum_1707 -> (sum_1008 (n_1009 - 1) k_sum_r_sum_1707);;
  r_sum_1494 x0_1384 x1_1384 x_1385 k_sum_acc__sum_r_sum_1764 -> (sum_1008 (x_1385 - 1) k_sum_acc__sum_r_sum_1764);;
  r_sum_acc__sum_1466 x0_1384 x1_1384 x_1385 k_sum_acc__sum_r_sum_acc__sum_1827 ->
      (sum_acc__sum_1390 (x0_1384 - 1) (x0_1384 + x1_1384) (x_1385 - 1) k_sum_acc__sum_r_sum_acc__sum_1827);;
  r_sum_acc__sum_1515 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_r_sum_acc__sum_1941 ->
      (sum_acc__sum_1390 xx010_1359 xx011_1359 xx11_1359 k_sum_acc__sum_r_sum_acc__sum_1941);;
  r_sum_acc__sum_1682 a_1016 n_1015 k_main_r_sum_acc__sum_2099 ->
      (sum_acc__sum_1223 true n_1015 a_1016 true n_1015 k_main_r_sum_acc__sum_2099);;
  sum_1008 n_1009 k_sum_1700 when (n_1009 <= 0) -> (k_sum_1700 0);;
  sum_1008 n_1009 k_sum_1700 when (not (n_1009 <= 0)) -> (r_sum_1428 n_1009 (f_sum_2288 n_1009 k_sum_1700));;
  sum_acc_1011 n__a0_1024 n__a1_1024 k_sum_acc_1724 when (n__a0_1024 <= 0) -> (k_sum_acc_1724 n__a1_1024);;
  sum_acc_1011 n__a0_1024 n__a1_1024 k_sum_acc_1724 when (not (n__a0_1024 <= 0)) ->
      (sum_acc_1011 (n__a0_1024 - 1) (n__a0_1024 + n__a1_1024) k_sum_acc_1724);;
  sum_acc__sum_1223 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855 when (
      xx00_1359 <=> false) ->
      (br_sum_acc__sum_2303 (xx10_1359 <=> false) xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359
        k_sum_acc__sum_1855);;
  sum_acc__sum_1223 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855 when (
      not (xx00_1359 <=> false)) ->
      (br_sum_acc__sum_2305 (xx10_1359 <=> false) xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359
        k_sum_acc__sum_1855);;
  sum_acc__sum_1390 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751 when (
      x0_1384 <= 0) -> (br_sum_acc__sum_2299 (x_1385 <= 0) x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751);;
  sum_acc__sum_1390 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751 when (
      not (x0_1384 <= 0)) -> (br_sum_acc__sum_2301 (x_1385 <= 0) x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751);;
Types:
  main_2287 : X
  fail_2307 : (bool -> (unit -> X) -> X)
  sum_1008 : (int -> (int -> X) -> X)
  sum_acc_1011 : (int -> int -> (int -> X) -> X)
  sum_acc__sum_1390 : (int -> x_2:int -> int -> (x_5:int -> x_6:int[x_6 = -x_2 + x_5] -> X) -> X)

(1-1) Abstracting ... DONE!

(1-2) Checking HORS ... DONE!

Error trace::
  main_2287 ... --> 
  r_f_1677 ... --> 
  f_2296 ... --> 
  r_f_1679 ... --> 
  f_2297 ... --> 
  r_r_main_1681 ... --> 
  main_1014 ... --> 
  r_sum_acc__sum_1682 ... --> 
  sum_acc__sum_1223 [2/2] ... --> 
  br_sum_acc__sum_2305 [2/2] ... --> 
  r_sum_acc__sum_1515 ... --> 
  sum_acc__sum_1390 [2/2] ... --> 
  br_sum_acc__sum_2301 [2/2] ... --> 
  r_sum_acc__sum_1466 ... --> 
  sum_acc__sum_1390 [1/2] ... --> 
  br_sum_acc__sum_2299 [1/2] ... --> 
  f_sum_acc__sum_2291 ... --> 
  f_sum_acc__sum_2294 ... --> 
  f_main_2295 [2/2] ... --> 
  fail_2307 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 0; 1; 1; 0; 0; 0; 0; 0; 1; 0

(1-3) Checking counterexample ... DONE!

(1-4) Discovering predicates ... 
DONE!

Prefix of spurious counterexample::
0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 0; 1; 1; 0; 0; 0; 0; 0; 1

Program with abstraction types (CEGAR-cycle 2)::
Main: main_2287
  main_2287 -> (r_f_1677 f_2296);;
  br_sum_acc__sum_2299 b_2300 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751 when b_2300 ->
      (k_sum_acc__sum_1751 x1_1384 0);;
  br_sum_acc__sum_2299 b_2300 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751 when (
      not b_2300) ->
      (r_sum_1494 x0_1384 x1_1384 x_1385 (f_sum_acc__sum_2289 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751));;
  br_sum_acc__sum_2301 b_2302 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751 when b_2302 ->
      (sum_acc_1011 (x0_1384 - 1) (x0_1384 + x1_1384) (f_sum_acc__sum_2290 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751));;
  br_sum_acc__sum_2301 b_2302 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751 when (
      not b_2302) ->
      (r_sum_acc__sum_1466 x0_1384 x1_1384 x_1385 (f_sum_acc__sum_2291 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751));;
  br_sum_acc__sum_2303 b_2304 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855 when b_2304 ->
      (k_sum_acc__sum_1855 false 0 false 0);;
  br_sum_acc__sum_2303 b_2304 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855 when (
      not b_2304) ->
      (sum_1008 xx11_1359 (f_sum_acc__sum_2292 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855));;
  br_sum_acc__sum_2305 b_2306 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855 when b_2306 ->
      (sum_acc_1011 xx010_1359 xx011_1359
        (f_sum_acc__sum_2293 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855));;
  br_sum_acc__sum_2305 b_2306 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855 when (
      not b_2306) ->
      (r_sum_acc__sum_1515 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359
        (f_sum_acc__sum_2294 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855));;
  f_2296 r_f_2172 -> (r_f_1679 r_f_2172 (f_2297 r_f_2172));;
  f_2297 r_f_2172 r_f_2171 -> (r_r_main_1681 r_f_2171 r_f_2172 (f_2298 r_f_2171 r_f_2172));;
  f_2298 r_f_2171 r_f_2172 r_r_main_2166 -> end;;
  f_main_2295 a_1016 n_1015 k_main_2068 r_sum_acc__sum00_2116 r_sum_acc__sum01_2116 r_sum_acc__sum10_2116 
  r_sum_acc__sum11_2116 when ((a_1016 + r_sum_acc__sum11_2116) = r_sum_acc__sum01_2116) -> (
      k_main_2068 ());;
  f_main_2295 a_1016 n_1015 k_main_2068 r_sum_acc__sum00_2116 r_sum_acc__sum01_2116 r_sum_acc__sum10_2116 
  r_sum_acc__sum11_2116 when (not ((a_1016 + r_sum_acc__sum11_2116) = r_sum_acc__sum01_2116)) ->
      (fail_2307 true k_main_2068);;
  f_sum_2288 n_1009 k_sum_1700 r_sum_1713 -> (k_sum_1700 (n_1009 + r_sum_1713));;
  f_sum_acc__sum_2289 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751 r_sum_1776 ->
      (k_sum_acc__sum_1751 x1_1384 (x_1385 + r_sum_1776));;
  f_sum_acc__sum_2290 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751 x_2212 -> (k_sum_acc__sum_1751 x_2212 0);;
  f_sum_acc__sum_2291 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751 r_sum_acc__sum0_1839 r_sum_acc__sum1_1839 ->
      (k_sum_acc__sum_1751 r_sum_acc__sum0_1839 (x_1385 + r_sum_acc__sum1_1839));;
  f_sum_acc__sum_2292 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855 x_2230 ->
      (k_sum_acc__sum_1855 false 0 true x_2230);;
  f_sum_acc__sum_2293 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855 x_2227 ->
      (k_sum_acc__sum_1855 true x_2227 false 0);;
  f_sum_acc__sum_2294 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855 r_sum_acc__sum0_1965 
  r_sum_acc__sum1_1965 -> (k_sum_acc__sum_1855 true r_sum_acc__sum0_1965 true r_sum_acc__sum1_1965);;
  fail_2307 b k -> {fail} => (k ());;
  main_1014 n_1015 a_1016 k_main_2068 -> (r_sum_acc__sum_1682 a_1016 n_1015 (f_main_2295 a_1016 n_1015 k_main_2068));;
  r_f_1677 k_r_f_2127 -> (rand_int k_r_f_2127);;
  r_f_1679 r_f_2172 k_r_f_2139 -> (rand_int k_r_f_2139);;
  r_r_main_1681 r_f_2171 r_f_2172 k_r_r_main_2160 -> (main_1014 r_f_2172 r_f_2171 k_r_r_main_2160);;
  r_sum_1428 n_1009 k_sum_r_sum_1707 -> (sum_1008 (n_1009 - 1) k_sum_r_sum_1707);;
  r_sum_1494 x0_1384 x1_1384 x_1385 k_sum_acc__sum_r_sum_1764 -> (sum_1008 (x_1385 - 1) k_sum_acc__sum_r_sum_1764);;
  r_sum_acc__sum_1466 x0_1384 x1_1384 x_1385 k_sum_acc__sum_r_sum_acc__sum_1827 ->
      (sum_acc__sum_1390 (x0_1384 - 1) (x0_1384 + x1_1384) (x_1385 - 1) k_sum_acc__sum_r_sum_acc__sum_1827);;
  r_sum_acc__sum_1515 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_r_sum_acc__sum_1941 ->
      (sum_acc__sum_1390 xx010_1359 xx011_1359 xx11_1359 k_sum_acc__sum_r_sum_acc__sum_1941);;
  r_sum_acc__sum_1682 a_1016 n_1015 k_main_r_sum_acc__sum_2099 ->
      (sum_acc__sum_1223 true n_1015 a_1016 true n_1015 k_main_r_sum_acc__sum_2099);;
  sum_1008 n_1009 k_sum_1700 when (n_1009 <= 0) -> (k_sum_1700 0);;
  sum_1008 n_1009 k_sum_1700 when (not (n_1009 <= 0)) -> (r_sum_1428 n_1009 (f_sum_2288 n_1009 k_sum_1700));;
  sum_acc_1011 n__a0_1024 n__a1_1024 k_sum_acc_1724 when (n__a0_1024 <= 0) -> (k_sum_acc_1724 n__a1_1024);;
  sum_acc_1011 n__a0_1024 n__a1_1024 k_sum_acc_1724 when (not (n__a0_1024 <= 0)) ->
      (sum_acc_1011 (n__a0_1024 - 1) (n__a0_1024 + n__a1_1024) k_sum_acc_1724);;
  sum_acc__sum_1223 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855 when (
      xx00_1359 <=> false) ->
      (br_sum_acc__sum_2303 (xx10_1359 <=> false) xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359
        k_sum_acc__sum_1855);;
  sum_acc__sum_1223 xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359 k_sum_acc__sum_1855 when (
      not (xx00_1359 <=> false)) ->
      (br_sum_acc__sum_2305 (xx10_1359 <=> false) xx00_1359 xx010_1359 xx011_1359 xx10_1359 xx11_1359
        k_sum_acc__sum_1855);;
  sum_acc__sum_1390 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751 when (
      x0_1384 <= 0) -> (br_sum_acc__sum_2299 (x_1385 <= 0) x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751);;
  sum_acc__sum_1390 x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751 when (
      not (x0_1384 <= 0)) -> (br_sum_acc__sum_2301 (x_1385 <= 0) x0_1384 x1_1384 x_1385 k_sum_acc__sum_1751);;
Types:
  main_2287 : X
  fail_2307 : (bool -> (unit -> X) -> X)
  sum_1008 : (int -> (int -> X) -> X)
  sum_acc_1011 : (int -> int -> (int -> X) -> X)
  sum_acc__sum_1390 : (x_1:int -> x_2:int -> x_3:int[x_3 = x_1] -> (x_5:int -> x_6:int[x_6 = -x_2 + x_5] -> X) -> X)

(2-1) Abstracting ... DONE!

(2-2) Checking HORS ... DONE!

Safe!

cycles: 2
total: 0.348 sec
  abst: 0.065 sec
  mc: 0.022 sec
  refine: 0.133 sec
    exparam: 0.070 sec
