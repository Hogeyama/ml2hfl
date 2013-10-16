MoCHi: Model Checker for Higher-Order Programs
  TRecS version: 1.35
  OCaml version: 4.00.1
  Command: ../mochi.opt fib.ml -ignore-conf

parsed::
 let rec fib_1030 n_1031 =
   if n_1031 = 0 then
     0
   else
     if n_1031 = 1 then
       1
     else
       fib_1030 (n_1031 - 1) + fib_1030 (n_1031 - 2) 
 in
 let rec fib'_1032 a_1033 b_1034 n_1035 =
   if n_1035 = 0 then
     a_1033
   else
     if n_1035 = 1 then
       b_1034
     else
       fib'_1032 b_1034 (a_1033 + b_1034) (n_1035 - 1) 
 in
 let rec fib''_1036 n_1037 =
   if n_1037 = 0 then
     (0, 0)
   else
     if n_1037 = 1 then
       (0, 1)
     else
       let r_1038 = fib''_1036 (n_1037 - 1) 
       in
         (snd r_1038, fst r_1038 + snd r_1038) 
 in
 let fib_fib_1039 a_1040 b_1041 n_1042 = (fib'_1032 a_1040 b_1041 n_1042, fib''_1036 n_1042) 
 in
 let main_1043 a_1044 b_1045 n_1046 =
   (match fib_fib_1039 a_1044 b_1045 n_1046 with
      | (r1_1047, r2_1048) ->
          if a_1044 = 0 && b_1045 = 1 then
            if r1_1047 = snd r2_1048 then
              ()
            else
              {fail} ()
          else
            ()
      | _ -> let u_1190 = {fail} 
             in
               _|_) 
 in
   ()

set_target::
 let rec fib_1030 n_1031 =
   if n_1031 = 0 then
     0
   else
     if n_1031 = 1 then
       1
     else
       fib_1030 (n_1031 - 1) + fib_1030 (n_1031 - 2) 
 in
 let rec fib'_1032 a_1033 b_1034 n_1035 =
   if n_1035 = 0 then
     a_1033
   else
     if n_1035 = 1 then
       b_1034
     else
       fib'_1032 b_1034 (a_1033 + b_1034) (n_1035 - 1) 
 in
 let rec fib''_1036 n_1037 =
   if n_1037 = 0 then
     (0, 0)
   else
     if n_1037 = 1 then
       (0, 1)
     else
       let r_1038 = fib''_1036 (n_1037 - 1) 
       in
         (snd r_1038, fst r_1038 + snd r_1038) 
 in
 let fib_fib_1039 a_1040 b_1041 n_1042 = (fib'_1032 a_1040 b_1041 n_1042, fib''_1036 n_1042) 
 in
 let main_1043 a_1044 b_1045 n_1046 =
   (match fib_fib_1039 a_1044 b_1045 n_1046 with
      | (r1_1047, r2_1048) ->
          if a_1044 = 0 && b_1045 = 1 then
            if r1_1047 = snd r2_1048 then
              ()
            else
              {fail} ()
          else
            ()
      | _ -> let u_1190 = {fail} 
             in
               _|_) 
 in
 let main_1194 =
   let arg1_1191 = rand_int () 
   in
   let arg2_1192 = rand_int () 
   in
   let arg3_1193 = rand_int () 
   in
     main_1043 arg1_1191 arg2_1192 arg3_1193 
 in
   ()

compose_same_arg
compose_same_arg
compose_let
fib'_1032:a_1040

fib''_1036:(0, 0)

compose_same_arg
compose_same_arg
compose_let
fib'_1032:b_1041

fib''_1036:(0, 1)

compose_same_arg
compose_let
fib'_1032:fib'_1032 b_1041 (a_1040 + b_1041) (n_1042 - 1)

fib''_1036:let r_1038 = fib''_1036 (n_1042 - 1) 
           in
             (snd r_1038, fst r_1038 + snd r_1038)

compose_let_same_arg
tupling::
 let rec fib_1030 =
   fun n_1031 ->
     (if n_1031 = 0 then
        0
      else
        if n_1031 = 1 then
          1
        else
          fib_1030 (n_1031 - 1) + fib_1030 (n_1031 - 2)) 
 in
 let rec fib'_1032 =
   fun a_1033 ->
     fun b_1034 ->
       fun n_1035 ->
         (if n_1035 = 0 then
            a_1033
          else
            if n_1035 = 1 then
              b_1034
            else
              fib'_1032 b_1034 (a_1033 + b_1034) (n_1035 - 1)) 
 in
 let rec fib''_1036 =
   fun n_1037 ->
     (if n_1037 = 0 then
        (0, 0)
      else
        if n_1037 = 1 then
          (0, 1)
        else
          let r_1038 = fib''_1036 (n_1037 - 1) 
          in
            (snd r_1038, fst r_1038 + snd r_1038)) 
 in
 let rec fib_fib_1039 =
   fun a_1040 ->
     fun b_1041 ->
       fun n_1042 ->
         (if n_1042 = 0 then
            let r_1200 = a_1040 
            in
            let r_1201 = r_1200 
            in
              (r_1201, (0, 0))
          else
            if n_1042 = 1 then
              let r_1205 = b_1041 
              in
              let r_1206 = r_1205 
              in
                (r_1206, (0, 1))
            else
              (match fib_fib_1039 b_1041 (a_1040 + b_1041) (n_1042 - 1) with
                 | (r_1210, r_1038) -> let r_1211 = (snd r_1038, fst r_1038 + snd r_1038) 
                                       in
                                         (r_1210, r_1211)
                 | _ -> let u_1216 = {fail} 
                        in
                          _|_)) 
 in
 let main_1043 =
   fun a_1044 ->
     fun b_1045 ->
       fun n_1046 ->
         (match fib_fib_1039 a_1044 b_1045 n_1046 with
            | (r1_1047, r2_1048) ->
                if a_1044 = 0 && b_1045 = 1 then
                  if r1_1047 = snd r2_1048 then
                    ()
                  else
                    {fail} ()
                else
                  ()
            | _ -> let u_1190 = {fail} 
                   in
                     _|_) 
 in
 let main_1194 =
   let arg1_1191 = rand_int () 
   in
   let arg2_1192 = rand_int () 
   in
   let arg3_1193 = rand_int () 
   in
     main_1043 arg1_1191 arg2_1192 arg3_1193 
 in
   ()

abst_list::
 let rec fib_1030 (n_1031:int) =
   if n_1031 = 0 then
     0
   else
     if n_1031 = 1 then
       1
     else
       fib_1030 (n_1031 - 1) + fib_1030 (n_1031 - 2) 
 in
 let rec fib'_1032 (a_1033:int) (b_1034:int) (n_1035:int) =
   if n_1035 = 0 then
     a_1033
   else
     if n_1035 = 1 then
       b_1034
     else
       fib'_1032 b_1034 (a_1033 + b_1034) (n_1035 - 1) 
 in
 let rec fib''_1036 (n_1037:int) =
   if n_1037 = 0 then
     (0, 0)
   else
     if n_1037 = 1 then
       (0, 1)
     else
       let r_1038 = fib''_1036 (n_1037 - 1) 
       in
         (snd r_1038, fst r_1038 + snd r_1038) 
 in
 let rec fib_fib_1039 (a_1040:int) (b_1041:int) (n_1042:int) =
   if n_1042 = 0 then
     let r_1200 = a_1040 
     in
     let r_1201 = r_1200 
     in
       (r_1201, (0, 0))
   else
     if n_1042 = 1 then
       let r_1205 = b_1041 
       in
       let r_1206 = r_1205 
       in
         (r_1206, (0, 1))
     else
       let xs_1307 = fib_fib_1039 b_1041 (a_1040 + b_1041) (n_1042 - 1) 
       in
       let r_1038 = snd xs_1307 
       in
       let r_1210 = fst xs_1307 
       in
       let r_1211 = (snd r_1038, fst r_1038 + snd r_1038) 
       in
         (r_1210, r_1211) 
 in
 let main_1043 (a_1044:int) (b_1045:int) (n_1046:int) =
   let xs_1318 = fib_fib_1039 a_1044 b_1045 n_1046 
   in
   let r2_1048 = snd xs_1318 
   in
   let r1_1047 = fst xs_1318 
   in
     if a_1044 = 0 && b_1045 = 1 then
       if r1_1047 = snd r2_1048 then
         ()
       else
         {fail} ()
     else
       () 
 in
 let main_1194 =
   let arg1_1191 = rand_int () 
   in
   let arg2_1192 = rand_int () 
   in
   let arg3_1193 = rand_int () 
   in
     main_1043 arg1_1191 arg2_1192 arg3_1193 
 in
   ()

inlined::
 let rec fib_1030 (n_1031:int) =
   if n_1031 = 0 then
     0
   else
     if n_1031 = 1 then
       1
     else
       fib_1030 (n_1031 - 1) + fib_1030 (n_1031 - 2) 
 in
 let rec fib'_1032 (a_1033:int) (b_1034:int) (n_1035:int) =
   if n_1035 = 0 then
     a_1033
   else
     if n_1035 = 1 then
       b_1034
     else
       fib'_1032 b_1034 (a_1033 + b_1034) (n_1035 - 1) 
 in
 let rec fib''_1036 (n_1037:int) =
   if n_1037 = 0 then
     (0, 0)
   else
     if n_1037 = 1 then
       (0, 1)
     else
       let r_1038 = fib''_1036 (n_1037 - 1) 
       in
         (snd r_1038, fst r_1038 + snd r_1038) 
 in
 let rec fib_fib_1039 (a_1040:int) (b_1041:int) (n_1042:int) =
   if n_1042 = 0 then
     let r_1200 = a_1040 
     in
     let r_1201 = r_1200 
     in
       (r_1201, (0, 0))
   else
     if n_1042 = 1 then
       let r_1205 = b_1041 
       in
       let r_1206 = r_1205 
       in
         (r_1206, (0, 1))
     else
       let xs_1307 = fib_fib_1039 b_1041 (a_1040 + b_1041) (n_1042 - 1) 
       in
       let r_1211 = (snd (snd xs_1307), fst (snd xs_1307) + snd (snd xs_1307)) 
       in
         (fst xs_1307, r_1211) 
 in
 let main_1043 (a_1044:int) (b_1045:int) (n_1046:int) =
   let xs_1318 = fib_fib_1039 a_1044 b_1045 n_1046 
   in
     if a_1044 = 0 && b_1045 = 1 then
       if fst xs_1318 = snd (snd xs_1318) then
         ()
       else
         {fail} ()
     else
       () 
 in
 let main_1194 =
   let arg1_1191 = rand_int () 
   in
   let arg2_1192 = rand_int () 
   in
   let arg3_1193 = rand_int () 
   in
     main_1043 arg1_1191 arg2_1192 arg3_1193 
 in
   ()

CPS::
 let rec fib_1030 (n_1031:int) (k_fib_1341:(int -> X)) =
   if n_1031 = 0 then
     k_fib_1341 0
   else
     if n_1031 = 1 then
       k_fib_1341 1
     else
       fib_1030 (n_1031 - 1) (fun x_1344 -> fib_1030 (n_1031 - 2) (fun x_1345 -> k_fib_1341 (x_1344 + x_1345))) 
 in
 let rec fib'_1032 (a_1033:int) (b_1034:int) (n_1035:int) (k_fib'_1393:(
   int -> X)) =
   if n_1035 = 0 then
     k_fib'_1393 a_1033
   else
     if n_1035 = 1 then
       k_fib'_1393 b_1034
     else
       fib'_1032 b_1034 (a_1033 + b_1034) (n_1035 - 1) k_fib'_1393 
 in
 let rec fib''_1036 (n_1037:int) (k_fib''_1427:((int * int) -> X)) =
   if n_1037 = 0 then
     k_fib''_1427 (0, 0)
   else
     if n_1037 = 1 then
       k_fib''_1427 (0, 1)
     else
       fib''_1036 (n_1037 - 1) (fun x_1440 -> k_fib''_1427 (snd x_1440, fst x_1440 + snd x_1440)) 
 in
 let rec fib_fib_1039 (a_1040:int) (b_1041:int) (n_1042:int) (k_fib_fib_1483:(
   (int * (int * int)) -> X)) =
   if n_1042 = 0 then
     k_fib_fib_1483 (a_1040, (0, 0))
   else
     if n_1042 = 1 then
       k_fib_fib_1483 (b_1041, (0, 1))
     else
       fib_fib_1039 b_1041 (a_1040 + b_1041) (n_1042 - 1)
         (fun x_1518 -> k_fib_fib_1483 (fst x_1518, (snd (snd x_1518), fst (snd x_1518) + snd (snd x_1518)))) 
 in
 let main_1043 (a_1044:int) (b_1045:int) (n_1046:int) (k_main_1571:(unit -> X)) =
   fib_fib_1039 a_1044 b_1045 n_1046
     (fun x_1574 ->
        (let k_main_1589 (b_1605:bool) =
           if b_1605 then
             if fst x_1574 = snd (snd x_1574) then
               k_main_1571 ()
             else
               {|fail|} () k_main_1571
           else
             k_main_1571 () 
         in
           if a_1044 = 0 then
             k_main_1589 (b_1045 = 1)
           else
             k_main_1589 false)) 
 in
   rand_int_cps ()
     (fun x_1611 ->
        rand_int_cps ()
          (fun x_1624 -> rand_int_cps () (fun x_1637 -> main_1043 x_1611 x_1624 x_1637 (fun x_1608 -> end))))

remove_pair::
 let rec fib_1030 (n_1031:int) (k_fib_1341:(int -> X)) =
   if n_1031 = 0 then
     k_fib_1341 0
   else
     if n_1031 = 1 then
       k_fib_1341 1
     else
       fib_1030 (n_1031 - 1) (fun x_1344 -> fib_1030 (n_1031 - 2) (fun x_1345 -> k_fib_1341 (x_1344 + x_1345))) 
 in
 let rec fib'_1032 (a_1033:int) (b_1034:int) (n_1035:int) (k_fib'_1393:(
   int -> X)) =
   if n_1035 = 0 then
     k_fib'_1393 a_1033
   else
     if n_1035 = 1 then
       k_fib'_1393 b_1034
     else
       fib'_1032 b_1034 (a_1033 + b_1034) (n_1035 - 1) k_fib'_1393 
 in
 let rec fib''_1036 (n_1037:int) (k_fib''_1427:(int -> (int -> X))) =
   if n_1037 = 0 then
     k_fib''_1427 0 0
   else
     if n_1037 = 1 then
       k_fib''_1427 0 1
     else
       fib''_1036 (n_1037 - 1) (fun x1_1440 -> fun x2_1440 -> k_fib''_1427 x2_1440 (x1_1440 + x2_1440)) 
 in
 let rec fib_fib_1039 (a_1040:int) (b_1041:int) (n_1042:int) (k_fib_fib_1483:(
   int -> (int -> (int -> X)))) =
   if n_1042 = 0 then
     k_fib_fib_1483 a_1040 0 0
   else
     if n_1042 = 1 then
       k_fib_fib_1483 b_1041 0 1
     else
       fib_fib_1039 b_1041 (a_1040 + b_1041) (n_1042 - 1)
         (fun x1_1518 -> fun x21_1518 -> fun x22_1518 -> k_fib_fib_1483 x1_1518 x22_1518 (x21_1518 + x22_1518)) 
 in
 let main_1043 (a_1044:int) (b_1045:int) (n_1046:int) (k_main_1571:(unit -> X)) =
   fib_fib_1039 a_1044 b_1045 n_1046
     (fun x1_1574 ->
        fun x21_1574 ->
          fun x22_1574 ->
            (let k_main_1589 (b_1605:bool) =
               if b_1605 then
                 if x1_1574 = x22_1574 then
                   k_main_1571 ()
                 else
                   {|fail|} () k_main_1571
               else
                 k_main_1571 () 
             in
               if a_1044 = 0 then
                 k_main_1589 (b_1045 = 1)
               else
                 k_main_1589 false)) 
 in
   rand_int_cps ()
     (fun x_1611 ->
        rand_int_cps ()
          (fun x_1624 -> rand_int_cps () (fun x_1637 -> main_1043 x_1611 x_1624 x_1637 (fun x_1608 -> end))))

Program with abstraction types (CEGAR-cycle 0)::
Main: main_1846
  main_1846 -> (rand_int f_1852)
  br_fib''_1860 b_1861 n_1037 k_fib''_1427 when b_1861 -> (k_fib''_1427 0 1)
  br_fib''_1860 b_1861 n_1037 k_fib''_1427 when (not b_1861) ->
      (fib''_1036 (n_1037 - 1) (f_fib''_1849 n_1037 k_fib''_1427))
  br_fib'_1858 b_1859 a_1033 b_1034 n_1035 k_fib'_1393 when b_1859 -> (k_fib'_1393 b_1034)
  br_fib'_1858 b_1859 a_1033 b_1034 n_1035 k_fib'_1393 when (not b_1859) ->
      (fib'_1032 b_1034 (a_1033 + b_1034) (n_1035 - 1) k_fib'_1393)
  br_fib_1856 b_1857 n_1031 k_fib_1341 when b_1857 -> (k_fib_1341 1)
  br_fib_1856 b_1857 n_1031 k_fib_1341 when (not b_1857) -> (fib_1030 (n_1031 - 1) (f_fib_1847 n_1031 k_fib_1341))
  br_fib_fib_1862 b_1863 a_1040 b_1041 n_1042 k_fib_fib_1483 when b_1863 -> (k_fib_fib_1483 b_1041 0 1)
  br_fib_fib_1862 b_1863 a_1040 b_1041 n_1042 k_fib_fib_1483 when (not b_1863) ->
      (fib_fib_1039 b_1041 (a_1040 + b_1041) (n_1042 - 1) (f_fib_fib_1850 a_1040 b_1041 n_1042 k_fib_fib_1483))
  br_k_main_1864 b_1865 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 when b_1865 ->
      (k_main_1571 ())
  br_k_main_1864 b_1865 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 when (
      not b_1865) -> (fail_1866 true k_main_1571)
  f_1852 x_1611 -> (rand_int (f_1853 x_1611))
  f_1853 x_1611 x_1624 -> (rand_int (f_1854 x_1611 x_1624))
  f_1854 x_1611 x_1624 x_1637 -> (main_1043 x_1611 x_1624 x_1637 (f_1855 x_1611 x_1624 x_1637))
  f_1855 x_1611 x_1624 x_1637 x_1608 -> end
  f_fib''_1849 n_1037 k_fib''_1427 x1_1440 x2_1440 -> (k_fib''_1427 x2_1440 (x1_1440 + x2_1440))
  f_fib_1847 n_1031 k_fib_1341 x_1344 -> (fib_1030 (n_1031 - 2) (f_fib_1848 n_1031 x_1344 k_fib_1341))
  f_fib_1848 n_1031 x_1344 k_fib_1341 x_1345 -> (k_fib_1341 (x_1344 + x_1345))
  f_fib_fib_1850 a_1040 b_1041 n_1042 k_fib_fib_1483 x1_1518 x21_1518 x22_1518 ->
      (k_fib_fib_1483 x1_1518 x22_1518 (x21_1518 + x22_1518))
  f_main_1851 a_1044 b_1045 n_1046 k_main_1571 x1_1574 x21_1574 x22_1574 when (
      a_1044 = 0) -> (k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 (b_1045 = 1))
  f_main_1851 a_1044 b_1045 n_1046 k_main_1571 x1_1574 x21_1574 x22_1574 when (
      not (a_1044 = 0)) -> (k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 false)
  fail_1866 b k -> {fail} => (k ())
  fib''_1036 n_1037 k_fib''_1427 when (n_1037 = 0) -> (k_fib''_1427 0 0)
  fib''_1036 n_1037 k_fib''_1427 when (not (n_1037 = 0)) -> (br_fib''_1860 (n_1037 = 1) n_1037 k_fib''_1427)
  fib'_1032 a_1033 b_1034 n_1035 k_fib'_1393 when (n_1035 = 0) -> (k_fib'_1393 a_1033)
  fib'_1032 a_1033 b_1034 n_1035 k_fib'_1393 when (not (n_1035 = 0)) ->
      (br_fib'_1858 (n_1035 = 1) a_1033 b_1034 n_1035 k_fib'_1393)
  fib_1030 n_1031 k_fib_1341 when (n_1031 = 0) -> (k_fib_1341 0)
  fib_1030 n_1031 k_fib_1341 when (not (n_1031 = 0)) -> (br_fib_1856 (n_1031 = 1) n_1031 k_fib_1341)
  fib_fib_1039 a_1040 b_1041 n_1042 k_fib_fib_1483 when (n_1042 = 0) -> (k_fib_fib_1483 a_1040 0 0)
  fib_fib_1039 a_1040 b_1041 n_1042 k_fib_fib_1483 when (not (n_1042 = 0)) ->
      (br_fib_fib_1862 (n_1042 = 1) a_1040 b_1041 n_1042 k_fib_fib_1483)
  k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 when b_1605 ->
      (br_k_main_1864 (x1_1574 = x22_1574) a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605)
  k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 when (not b_1605) -> (k_main_1571 ())
  main_1043 a_1044 b_1045 n_1046 k_main_1571 ->
      (fib_fib_1039 a_1044 b_1045 n_1046 (f_main_1851 a_1044 b_1045 n_1046 k_main_1571))
Types:
  main_1846 : X
  fail_1866 : (x_1:bool[x_1] -> (unit -> X) -> X)
  fib_1030 : (int -> (int -> X) -> X)
  fib_fib_1039 : (int -> int -> int -> (int -> int -> int -> X) -> X)
  
(0-1) Abstracting ... DONE!

(0-2) Checking HORS ... DONE!

Error trace::
  main_1846 ... --> 
  f_1852 ... --> 
  f_1853 ... --> 
  f_1854 ... --> 
  main_1043 ... --> 
  fib_fib_1039 [2/2] ... --> 
  br_fib_fib_1862 [1/2] ... --> 
  f_main_1851 [1/2] ... --> 
  k_main_1589 [1/2] ... --> 
  br_k_main_1864 [2/2] ... --> 
  fail_1866 ... --> fail -->
  ERROR!

Spurious counter-example::
  0; 0; 0; 0; 0; 1; 0; 0; 0; 1; 0

(0-3) Checking counter-example ... DONE!

(0-4) Discovering predicates ... 
begin AbsTypeInfer.refine(228)[1]
  program:
    main_1846  | true = ((Random.int 0) f_1852)
    br_fib''_1860 b_1861 n_1037 k_fib''_1427 | b_1861 = (k_fib''_1427 0
                                                                    1)
    br_fib''_1860 b_1861 n_1037 k_fib''_1427 | (not b_1861) = (fib''_1036
                                                                 (n_1037 - 1)
                                                                 (f_fib''_1849 n_1037
                                                                    k_fib''_1427))
    br_fib'_1858 b_1859 a_1033 b_1034 n_1035 k_fib'_1393 | b_1859 = (
    k_fib'_1393 b_1034)
    br_fib'_1858 b_1859 a_1033 b_1034 n_1035 k_fib'_1393 | (not b_1859) = (
    fib'_1032 b_1034
              (a_1033 + b_1034)
              (n_1035 - 1)
              k_fib'_1393)
    br_fib_1856 b_1857 n_1031 k_fib_1341 | b_1857 = (k_fib_1341 1)
    br_fib_1856 b_1857 n_1031 k_fib_1341 | (not b_1857) = (fib_1030 (
                                                                    n_1031 - 1)
                                                                    (
                                                                    f_fib_1847 n_1031
                                                                    k_fib_1341))
    br_fib_fib_1862 b_1863 a_1040 b_1041 n_1042 k_fib_fib_1483 | b_1863 = (
    k_fib_fib_1483 b_1041
                   0
                   1)
    br_fib_fib_1862 b_1863 a_1040 b_1041 n_1042 k_fib_fib_1483 | (not b_1863) = (
    fib_fib_1039 b_1041
                 (a_1040 + b_1041)
                 (n_1042 - 1)
                 (f_fib_fib_1850 a_1040
                                 b_1041
                                 n_1042
                                 k_fib_fib_1483))
    br_k_main_1864 b_1865 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 | b_1865 = (
    k_main_1571 ())
    br_k_main_1864 b_1865 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 | (
    not b_1865) = (fail_1866 true
                             k_main_1571)
    f_1852 x_1611 | true = ((Random.int 0) (f_1853 x_1611))
    f_1853 x_1611 x_1624 | true = ((Random.int 0) (f_1854 x_1611
                                                          x_1624))
    f_1854 x_1611 x_1624 x_1637 | true = (main_1043 x_1611
                                                    x_1624
                                                    x_1637
                                                    (f_1855 x_1611
                                                            x_1624
                                                            x_1637))
    f_1855 x_1611 x_1624 x_1637 x_1608 | true = end
    f_fib''_1849 n_1037 k_fib''_1427 x1_1440 x2_1440 | true = (k_fib''_1427 x2_1440
                                                                    (x1_1440 + x2_1440))
    f_fib_1847 n_1031 k_fib_1341 x_1344 | true = (fib_1030 (n_1031 - 2)
                                                           (f_fib_1848 n_1031
                                                                    x_1344
                                                                    k_fib_1341))
    f_fib_1848 n_1031 x_1344 k_fib_1341 x_1345 | true = (k_fib_1341 (
                                                                    x_1344 + x_1345))
    f_fib_fib_1850 a_1040 b_1041 n_1042 k_fib_fib_1483 x1_1518 x21_1518 x22_1518 | true = (
    k_fib_fib_1483 x1_1518
                   x22_1518
                   (x21_1518 + x22_1518))
    f_main_1851 a_1044 b_1045 n_1046 k_main_1571 x1_1574 x21_1574 x22_1574 | (
    a_1044 = 0) = (k_main_1589 a_1044
                               b_1045
                               n_1046
                               x1_1574
                               x21_1574
                               x22_1574
                               k_main_1571
                               (b_1045 = 1))
    f_main_1851 a_1044 b_1045 n_1046 k_main_1571 x1_1574 x21_1574 x22_1574 | (
    not (a_1044 = 0)) = (k_main_1589 a_1044
                                     b_1045
                                     n_1046
                                     x1_1574
                                     x21_1574
                                     x22_1574
                                     k_main_1571
                                     false)
    fail_1866 b k | true = (fail ())
    fib''_1036 n_1037 k_fib''_1427 | (n_1037 = 0) = (k_fib''_1427 0
                                                                  0)
    fib''_1036 n_1037 k_fib''_1427 | (not (n_1037 = 0)) = (br_fib''_1860 (
                                                                    n_1037 = 1)
                                                                    n_1037
                                                                    k_fib''_1427)
    fib'_1032 a_1033 b_1034 n_1035 k_fib'_1393 | (n_1035 = 0) = (k_fib'_1393 a_1033)
    fib'_1032 a_1033 b_1034 n_1035 k_fib'_1393 | (not (n_1035 = 0)) = (
    br_fib'_1858 (n_1035 = 1)
                 a_1033
                 b_1034
                 n_1035
                 k_fib'_1393)
    fib_1030 n_1031 k_fib_1341 | (n_1031 = 0) = (k_fib_1341 0)
    fib_1030 n_1031 k_fib_1341 | (not (n_1031 = 0)) = (br_fib_1856 (n_1031 = 1)
                                                                   n_1031
                                                                   k_fib_1341)
    fib_fib_1039 a_1040 b_1041 n_1042 k_fib_fib_1483 | (n_1042 = 0) = (
    k_fib_fib_1483 a_1040
                   0
                   0)
    fib_fib_1039 a_1040 b_1041 n_1042 k_fib_fib_1483 | (not (n_1042 = 0)) = (
    br_fib_fib_1862 (n_1042 = 1)
                    a_1040
                    b_1041
                    n_1042
                    k_fib_fib_1483)
    k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 | b_1605 = (
    br_k_main_1864 (x1_1574 = x22_1574)
                   a_1044
                   b_1045
                   n_1046
                   x1_1574
                   x21_1574
                   x22_1574
                   k_main_1571
                   b_1605)
    k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 | (
    not b_1605) = (k_main_1571 ())
    main_1043 a_1044 b_1045 n_1046 k_main_1571 | true = (fib_fib_1039
                                                           a_1044
                                                           b_1045
                                                           n_1046
                                                           (f_main_1851 a_1044
                                                                    b_1045
                                                                    n_1046
                                                                    k_main_1571))
    main_1846:X
    br_fib''_1860:bool -> int -> (int -> int -> X) -> X
    br_fib'_1858:bool -> int -> int -> int -> (int -> X) -> X
    br_fib_1856:bool -> int -> (int -> X) -> X
    br_fib_fib_1862:bool -> int -> int -> int -> (int -> int -> int -> X) -> X
    br_k_main_1864:bool -> int -> int -> int -> int -> int -> int -> (unit -> X) -> bool -> X
    f_1852:int -> X
    f_1853:int -> int -> X
    f_1854:int -> int -> int -> X
    f_1855:int -> int -> int -> unit -> X
    f_fib''_1849:int -> (int -> int -> X) -> int -> int -> X
    f_fib_1847:int -> (int -> X) -> int -> X
    f_fib_1848:int -> int -> (int -> X) -> int -> X
    f_fib_fib_1850:int -> int -> int -> (int -> int -> int -> X) -> int -> int -> int -> X
    f_main_1851:int -> int -> int -> (unit -> X) -> int -> int -> int -> X
    fail_1866:bool -> (unit -> X) -> X
    fib''_1036:int -> (int -> int -> X) -> X
    fib'_1032:int -> int -> int -> (int -> X) -> X
    fib_1030:int -> (int -> X) -> X
    fib_fib_1039:int -> int -> int -> (int -> int -> int -> X) -> X
    k_main_1589:int -> int -> int -> int -> int -> int -> (unit -> X) -> bool -> X
    main_1043:int -> int -> int -> (unit -> X) -> X
  inlined functions: br_fib''_1860,br_fib'_1858,br_fib_1856,br_fib_fib_1862,br_k_main_1864,f_1852,f_1853,f_1854,f_1855,f_fib''_1849,f_fib_1847,f_fib_1848,f_fib_fib_1850,f_main_1851,fib''_1036,fib'_1032,k_main_1589,main_1043
  counterexample: 0:0:0:0:0:1:0:0:0:1:0
  error traces:
    [true.nop(<f_1852@4:0> = var2)
     [true.nop((<f_1853@7:0> = <f_1852@4:0>) && (<f_1853@7:1> = var3))
      [true.nop((<f_1854@10:0> = <f_1853@7:0>) && ((<f_1854@10:1> = <f_1853@7:1>) && (<f_1854@10:2> = var4)))
       [true.
        ((<main_1043@12:0> = <f_1854@10:0>) &&
         ((<main_1043@12:1> = <f_1854@10:1>) && (<main_1043@12:2> = <f_1854@10:2>)))
        [true.
         ((<fib_fib_1039@14:0> = <main_1043@12:0>) &&
          ((<fib_fib_1039@14:1> = <main_1043@12:1>) && (<fib_fib_1039@14:2> = <main_1043@12:2>)))
         [(not (<fib_fib_1039@14:2> = 0)).
          ((<br_fib_fib_1862@17:0> = (<fib_fib_1039@14:2> = 1)) &&
           ((<br_fib_fib_1862@17:1> = <fib_fib_1039@14:0>) &&
            ((<br_fib_fib_1862@17:2> = <fib_fib_1039@14:1>) && (<br_fib_fib_1862@17:3> = <fib_fib_1039@14:2>))))
          [<br_fib_fib_1862@17:0>.
           ((<<br_fib_fib_1862@17:4>@20:0> = <br_fib_fib_1862@17:2>) &&
            ((<<br_fib_fib_1862@17:4>@20:1> = 0) && (<<br_fib_fib_1862@17:4>@20:2> = 1)))
           [true.
            ((<<fib_fib_1039@14:3>@22:0> = <<br_fib_fib_1862@17:4>@20:0>) &&
             ((<<fib_fib_1039@14:3>@22:1> = <<br_fib_fib_1862@17:4>@20:1>) &&
              (<<fib_fib_1039@14:3>@22:2> = <<br_fib_fib_1862@17:4>@20:2>)))
            [true.
             ((<f_main_1851@24:0> = <main_1043@12:0>) &&
              ((<f_main_1851@24:1> = <main_1043@12:1>) &&
               ((<f_main_1851@24:2> = <main_1043@12:2>) &&
                ((<f_main_1851@24:4> = <<fib_fib_1039@14:3>@22:0>) &&
                 ((<f_main_1851@24:5> = <<fib_fib_1039@14:3>@22:1>) &&
                  (<f_main_1851@24:6> = <<fib_fib_1039@14:3>@22:2>))))))
             [(<f_main_1851@24:0> = 0).
              ((<k_main_1589@27:0> = <f_main_1851@24:0>) &&
               ((<k_main_1589@27:1> = <f_main_1851@24:1>) &&
                ((<k_main_1589@27:2> = <f_main_1851@24:2>) &&
                 ((<k_main_1589@27:3> = <f_main_1851@24:4>) &&
                  ((<k_main_1589@27:4> = <f_main_1851@24:5>) &&
                   ((<k_main_1589@27:5> = <f_main_1851@24:6>) && (<k_main_1589@27:7> = (<f_main_1851@24:1> = 1))))))))
              [<k_main_1589@27:7>.
               ((<br_k_main_1864@30:0> = (<k_main_1589@27:3> = <k_main_1589@27:5>)) &&
                ((<br_k_main_1864@30:1> = <k_main_1589@27:0>) &&
                 ((<br_k_main_1864@30:2> = <k_main_1589@27:1>) &&
                  ((<br_k_main_1864@30:3> = <k_main_1589@27:2>) &&
                   ((<br_k_main_1864@30:4> = <k_main_1589@27:3>) &&
                    ((<br_k_main_1864@30:5> = <k_main_1589@27:4>) &&
                     ((<br_k_main_1864@30:6> = <k_main_1589@27:5>) && (<br_k_main_1864@30:8> = <k_main_1589@27:7>))))))))
               [(not <br_k_main_1864@30:0>).(<fail_1866@33:0> = true)[true.error
  begin RefTypeInfer.infer_etrs(229)[2]
    horn clauses:
      P[<fail_1866@33:0>](<fail_1866@33:0>:bool)|- bot
      
      P[<br_k_main_1864@30:8>](false:bool,<br_k_main_1864@30:1>:int,<br_k_main_1864@30:2>:int,<br_k_main_1864@30:3>:int,<br_k_main_1864@30:4>:int,<br_k_main_1864@30:5>:int,<br_k_main_1864@30:6>:int,<br_k_main_1864@30:8>:bool),
      <fail_1866@33:0> |- P[<fail_1866@33:0>](<fail_1866@33:0>:bool)
      
      P[<k_main_1589@27:7>](<br_k_main_1864@30:1>:int,<br_k_main_1864@30:2>:int,<br_k_main_1864@30:3>:int,<br_k_main_1864@30:4>:int,<br_k_main_1864@30:5>:int,<br_k_main_1864@30:6>:int,<br_k_main_1864@30:8>:bool),
      (<br_k_main_1864@30:8> && (<br_k_main_1864@30:0> = (<br_k_main_1864@30:4> = <br_k_main_1864@30:6>)))
      |- P[<br_k_main_1864@30:8>](<br_k_main_1864@30:0>:bool,<br_k_main_1864@30:1>:int,
                                  <br_k_main_1864@30:2>:int,<br_k_main_1864@30:3>:int,
                                  <br_k_main_1864@30:4>:int,<br_k_main_1864@30:5>:int,
                                  <br_k_main_1864@30:6>:int,<br_k_main_1864@30:8>:bool)
      
      P[<f_main_1851@24:6>](<k_main_1589@27:0>:int,<k_main_1589@27:1>:int,<k_main_1589@27:2>:int,<k_main_1589@27:3>:int,<k_main_1589@27:4>:int,<k_main_1589@27:5>:int),
      ((<k_main_1589@27:0> = 0) && (<k_main_1589@27:7> = (<k_main_1589@27:1> = 1)))
      |- P[<k_main_1589@27:7>](<k_main_1589@27:0>:int,<k_main_1589@27:1>:int,
                               <k_main_1589@27:2>:int,<k_main_1589@27:3>:int,
                               <k_main_1589@27:4>:int,<k_main_1589@27:5>:int,
                               <k_main_1589@27:7>:bool)
      
      P[<main_1043@12:2>](<f_main_1851@24:0>:int,<f_main_1851@24:1>:int,<f_main_1851@24:2>:int),
      P[<<fib_fib_1039@14:3>@22:2>](<f_main_1851@24:0>:int,<f_main_1851@24:1>:int,<f_main_1851@24:2>:int,<f_main_1851@24:4>:int,<f_main_1851@24:5>:int,<f_main_1851@24:6>:int)|- P[<f_main_1851@24:6>](
      <f_main_1851@24:0>:int,<f_main_1851@24:1>:int,<f_main_1851@24:2>:int,
      <f_main_1851@24:4>:int,<f_main_1851@24:5>:int,<f_main_1851@24:6>:int)
      
      P[<fib_fib_1039@14:2>](<fib_fib_1039@14:0>:int,<fib_fib_1039@14:1>:int,<fib_fib_1039@14:2>:int),
      P[<<br_fib_fib_1862@17:4>@20:2>]((<fib_fib_1039@14:2> = 1):bool,<fib_fib_1039@14:0>:int,<fib_fib_1039@14:1>:int,<fib_fib_1039@14:2>:int,<<fib_fib_1039@14:3>@22:0>:int,<<fib_fib_1039@14:3>@22:1>:int,<<fib_fib_1039@14:3>@22:2>:int),
      (<fib_fib_1039@14:2> <> 0)
      |- P[<<fib_fib_1039@14:3>@22:2>](<fib_fib_1039@14:0>:int,<fib_fib_1039@14:1>:int,
                                       <fib_fib_1039@14:2>:int,<<fib_fib_1039@14:3>@22:0>:int,
                                       <<fib_fib_1039@14:3>@22:1>:int,
                                       <<fib_fib_1039@14:3>@22:2>:int)
      
      P[<br_fib_fib_1862@17:3>](<br_fib_fib_1862@17:0>:bool,<br_fib_fib_1862@17:1>:int,<br_fib_fib_1862@17:2>:int,<br_fib_fib_1862@17:3>:int),
      ((<<br_fib_fib_1862@17:4>@20:1> = 0) &&
       ((<<br_fib_fib_1862@17:4>@20:2> = 1) &&
        ((<<br_fib_fib_1862@17:4>@20:0> = <br_fib_fib_1862@17:2>) && <br_fib_fib_1862@17:0>)))
      |- P[<<br_fib_fib_1862@17:4>@20:2>](<br_fib_fib_1862@17:0>:bool,
                                          <br_fib_fib_1862@17:1>:int,
                                          <br_fib_fib_1862@17:2>:int,
                                          <br_fib_fib_1862@17:3>:int,
                                          <<br_fib_fib_1862@17:4>@20:0>:int,
                                          <<br_fib_fib_1862@17:4>@20:1>:int,
                                          <<br_fib_fib_1862@17:4>@20:2>:int)
      
      P[<fib_fib_1039@14:2>](<br_fib_fib_1862@17:1>:int,<br_fib_fib_1862@17:2>:int,<br_fib_fib_1862@17:3>:int),
      ((<br_fib_fib_1862@17:3> <> 0) && (<br_fib_fib_1862@17:0> = (<br_fib_fib_1862@17:3> = 1)))
      |- P[<br_fib_fib_1862@17:3>](<br_fib_fib_1862@17:0>:bool,<br_fib_fib_1862@17:1>:int,
                                   <br_fib_fib_1862@17:2>:int,<br_fib_fib_1862@17:3>:int)
      
      P[<main_1043@12:2>](<fib_fib_1039@14:0>:int,<fib_fib_1039@14:1>:int,<fib_fib_1039@14:2>:int)|- P[<fib_fib_1039@14:2>](
      <fib_fib_1039@14:0>:int,<fib_fib_1039@14:1>:int,<fib_fib_1039@14:2>:int)
      
      P[<f_1854@10:2>](<main_1043@12:0>:int,<main_1043@12:1>:int,<main_1043@12:2>:int)|- P[<main_1043@12:2>](
      <main_1043@12:0>:int,<main_1043@12:1>:int,<main_1043@12:2>:int)
      
      P[<f_1853@7:1>](<f_1854@10:0>:int,<f_1854@10:1>:int)|- P[<f_1854@10:2>](
      <f_1854@10:0>:int,<f_1854@10:1>:int,<f_1854@10:2>:int)
      
      P[<f_1852@4:0>](<f_1853@7:0>:int)|- P[<f_1853@7:1>](<f_1853@7:0>:int,
                                                          <f_1853@7:1>:int)
      
      |- P[<f_1852@4:0>](<f_1852@4:0>:int)
    call trees:
      <main_1846@1>
        <f_1852@4>
          <f_1853@7>
            <f_1854@10>
              <main_1043@12>
                <fib_fib_1039@14>
                  <br_fib_fib_1862@17>
                  </<br_fib_fib_1862@17:4>@20>
                </<fib_fib_1039@14:3>@22>
                <f_main_1851@24>
                  <k_main_1589@27>
                    <br_k_main_1864@30>
                      <fail_1866@33>
    inlined horn clauses:
      P[<fail_1866@33:0>](<fail_1866@33:0>:bool)|- bot
      
      P[<<fib_fib_1039@14:3>@22:2>](0:int,1:int,<br_k_main_1864@30:3>:int,<br_k_main_1864@30:4>:int,<br_k_main_1864@30:5>:int,<br_k_main_1864@30:6>:int),
      ((<br_k_main_1864@30:4> <> <br_k_main_1864@30:6>) && <fail_1866@33:0>)
      |- P[<fail_1866@33:0>](<fail_1866@33:0>:bool)
      
      P[<fib_fib_1039@14:2>](<fib_fib_1039@14:0>:int,<fib_fib_1039@14:1>:int,<fib_fib_1039@14:2>:int),
      ((<fib_fib_1039@14:2> = 1) &&
       ((<<fib_fib_1039@14:3>@22:1> = 0) &&
        ((<<fib_fib_1039@14:3>@22:2> = 1) && (<<fib_fib_1039@14:3>@22:0> = <fib_fib_1039@14:1>))))
      |- P[<<fib_fib_1039@14:3>@22:2>](<fib_fib_1039@14:0>:int,<fib_fib_1039@14:1>:int,
                                       <fib_fib_1039@14:2>:int,<<fib_fib_1039@14:3>@22:0>:int,
                                       <<fib_fib_1039@14:3>@22:1>:int,
                                       <<fib_fib_1039@14:3>@22:2>:int)
      
      |- P[<fib_fib_1039@14:2>](<fib_fib_1039@14:0>:int,<fib_fib_1039@14:1>:int,
                                <fib_fib_1039@14:2>:int)
    begin ParamSubstInfer.infer(2827)[3]
      
    end ParamSubstInfer.infer(2827)[3] (0.008000 sec.)
    inferred extra parameters:
      
    begin RefTypeInfer.elim_coeffs(2829)[3]
      
    end RefTypeInfer.elim_coeffs(2829)[3] (0.000000 sec.)
    begin BwHcSolver.solve(3086)[3]
      lower bounds:
        P[<fib_fib_1039@14:2>](<fib_fib_1039@14:0>:int,<fib_fib_1039@14:1>:int,<fib_fib_1039@14:2>:int) = true
        P[<<fib_fib_1039@14:3>@22:2>](<fib_fib_1039@14:0>:int,<fib_fib_1039@14:1>:int,<fib_fib_1039@14:2>:int,<<fib_fib_1039@14:3>@22:0>:int,<<fib_fib_1039@14:3>@22:1>:int,<<fib_fib_1039@14:3>@22:2>:int) =
        ((<fib_fib_1039@14:2> = 1) &&
         ((<<fib_fib_1039@14:3>@22:1> = 0) &&
          ((<<fib_fib_1039@14:3>@22:2> = 1) && (<<fib_fib_1039@14:3>@22:0> = <fib_fib_1039@14:1>))))
        P[<fail_1866@33:0>](<fail_1866@33:0>:bool) = false
      begin BwHcSolver.solve_preds(3334)[4]
        input:
          P[<fail_1866@33:0>](<fail_1866@33:0>:bool) = false
          P[<fail_1866@33:0>](<fail_1866@33:0>:bool)|- bot
        finding a solution to P[<fail_1866@33:0>](var5:bool)
        begin InterpProver.interpolate(3344)[5]
          begin InterpProver.interpolate_fresh(3345)[6]
            begin InterpProver.interpolate_log(3346)[7]
              input1: false
              input2: true
              begin InterpProver.interpolate_check(3347)[8]
                begin InterpProver.interpolate_simplify(3348)[9]
                  begin InterpProver.interpolate_quick(3361)[10]
                    
                  end InterpProver.interpolate_quick(3361)[10] (0.000000 sec.)
                  begin minimizing # of disjunctions(3365)[10]
                    input: false
                    output: false
                  end minimizing # of disjunctions(3365)[10] (0.000000 sec.)
                  
                end InterpProver.interpolate_simplify(3348)[9] (0.000000 sec.)
                
              end InterpProver.interpolate_check(3347)[8] (0.000000 sec.)
              output: false
            end InterpProver.interpolate_log(3346)[7] (0.000000 sec.)
            
          end InterpProver.interpolate_fresh(3345)[6] (0.000000 sec.)
          
        end InterpProver.interpolate(3344)[5] (0.000000 sec.)
        solution:
          P[<fail_1866@33:0>](var5:bool) = false
        
      end BwHcSolver.solve_preds(3334)[4] (0.000000 sec.)
      begin BwHcSolver.solve_preds(3383)[4]
        input:
          P[<<fib_fib_1039@14:3>@22:2>](<fib_fib_1039@14:0>:int,<fib_fib_1039@14:1>:int,<fib_fib_1039@14:2>:int,<<fib_fib_1039@14:3>@22:0>:int,<<fib_fib_1039@14:3>@22:1>:int,<<fib_fib_1039@14:3>@22:2>:int) =
          ((<fib_fib_1039@14:2> = 1) &&
           ((<<fib_fib_1039@14:3>@22:1> = 0) &&
            ((<<fib_fib_1039@14:3>@22:2> = 1) && (<<fib_fib_1039@14:3>@22:0> = <fib_fib_1039@14:1>))))
          P[<<fib_fib_1039@14:3>@22:2>](0:int,1:int,<br_k_main_1864@30:3>:int,<br_k_main_1864@30:4>:int,<br_k_main_1864@30:5>:int,<br_k_main_1864@30:6>:int),
          ((<br_k_main_1864@30:4> <> <br_k_main_1864@30:6>) && <fail_1866@33:0>) |- bot
        finding a solution to P[<<fib_fib_1039@14:3>@22:2>](var6:int,var7:int,var8:int,var9:int,var10:int,var11:int)
        begin InterpProver.interpolate(3497)[5]
          begin InterpProver.interpolate_fresh(3498)[6]
            begin InterpProver.interpolate_log(3499)[7]
              input1: ((var9 = var7) && ((var11 = 1) && ((var10 = 0) && (var8 = 1))))
              input2: ((var9 <> var11) && ((var6 = 0) && (var7 = 1)))
              begin InterpProver.interpolate_check(3500)[8]
                begin InterpProver.interpolate_simplify(3501)[9]
                  begin InterpProver.interpolate_quick(3677)[10]
                    begin CsisatInterface.interpolate_csisat_wrap(3686)[11]
                      begin CsisatInterface.interpolate_csisat_post_process(3687)[12]
                        begin CsisatInterface.interpolate_csisat_log(3688)[13]
                          input1: (1 = v_sep_var11 & v_sep_var7 = v_sep_var9)
                          input2: (not v_sep_var11 = v_sep_var9 & 1 = v_sep_var7)
                          begin CsisatInterface.interpolate_csisat_raw(3689)[14]
                            
                          end CsisatInterface.interpolate_csisat_raw(3689)[14] (0.000000 sec.)
                          output: (1 = v_sep_var11 & v_sep_var7 = v_sep_var9)
                          
                        end CsisatInterface.interpolate_csisat_log(3688)[13] (0.000000 sec.)
                        after simplification: (1 = v_sep_var11 & v_sep_var7 = v_sep_var9)
                        after dnf conversion: ((1 = v_sep_var11 & v_sep_var7 = v_sep_var9))
                        
                      end CsisatInterface.interpolate_csisat_post_process(3687)[12] (0.000000 sec.)
                      
                    end CsisatInterface.interpolate_csisat_wrap(3686)[11] (0.000000 sec.)
                    
                  end InterpProver.interpolate_quick(3677)[10] (0.000000 sec.)
                  begin minimizing # of conjunctions(3704)[10]
                    input: ((var11 = 1) && (var7 = var9))
                    output: ((var7 = var9) && (var11 = 1))
                  end minimizing # of conjunctions(3704)[10] (0.000000 sec.)
                  
                end InterpProver.interpolate_simplify(3501)[9] (0.000000 sec.)
                
              end InterpProver.interpolate_check(3500)[8] (0.004000 sec.)
              output: ((var7 = var9) && (var11 = 1))
            end InterpProver.interpolate_log(3499)[7] (0.004000 sec.)
            
          end InterpProver.interpolate_fresh(3498)[6] (0.004000 sec.)
          
        end InterpProver.interpolate(3497)[5] (0.004000 sec.)
        solution:
          P[<<fib_fib_1039@14:3>@22:2>](var6:int,var7:int,var8:int,var9:int,var10:int,var11:int) =
          ((var7 = var9) && (var11 = 1))
        
      end BwHcSolver.solve_preds(3383)[4] (0.004000 sec.)
      begin HcSolver.check_validity(3879)[4]
        input:
          P[<fail_1866@33:0>](<fail_1866@33:0>:bool)|- bot
          
          P[<<fib_fib_1039@14:3>@22:2>](0:int,1:int,<br_k_main_1864@30:3>:int,<br_k_main_1864@30:4>:int,<br_k_main_1864@30:5>:int,<br_k_main_1864@30:6>:int),
          ((<br_k_main_1864@30:4> <> <br_k_main_1864@30:6>) && <fail_1866@33:0>)
          |- P[<fail_1866@33:0>](<fail_1866@33:0>:bool)
          
          P[<fib_fib_1039@14:2>](<fib_fib_1039@14:0>:int,<fib_fib_1039@14:1>:int,<fib_fib_1039@14:2>:int),
          ((<fib_fib_1039@14:2> = 1) &&
           ((<<fib_fib_1039@14:3>@22:1> = 0) &&
            ((<<fib_fib_1039@14:3>@22:2> = 1) && (<<fib_fib_1039@14:3>@22:0> = <fib_fib_1039@14:1>))))
          |- P[<<fib_fib_1039@14:3>@22:2>](<fib_fib_1039@14:0>:int,<fib_fib_1039@14:1>:int,
                                           <fib_fib_1039@14:2>:int,<<fib_fib_1039@14:3>@22:0>:int,
                                           <<fib_fib_1039@14:3>@22:1>:int,
                                           <<fib_fib_1039@14:3>@22:2>:int)
          
          |- P[<fib_fib_1039@14:2>](<fib_fib_1039@14:0>:int,<fib_fib_1039@14:1>:int,
                                    <fib_fib_1039@14:2>:int)
        checking substituted Horn clause:
           false |- bot
        checking substituted Horn clause:
           false |- bot
        checking substituted Horn clause:
           false |- bot
        checking substituted Horn clause:
           false |- bot
        
      end HcSolver.check_validity(3879)[4] (0.000000 sec.)
      solution:
        P[<fail_1866@33:0>](var5:bool) = false
        P[<<fib_fib_1039@14:3>@22:2>](var6:int,var7:int,var8:int,var9:int,var10:int,var11:int) =
        ((var11 = 1) && (var7 = var9))
        P[<fib_fib_1039@14:2>](var12:int,var13:int,var14:int) = true
    end BwHcSolver.solve(3086)[3] (0.004000 sec.)
    
  end RefTypeInfer.infer_etrs(229)[2] (0.020000 sec.)
  refinement types:
    main_1846: X
    f_1852: v1:int -> X
    f_1853: v1:int -> v2:int -> X
    f_1854: v1:int -> v2:int -> v3:int -> X
    main_1043: v1:int -> v2:int -> v3:int -> (v4:unit -> X) -> X
    fib_fib_1039: v1:int -> v2:int -> v3:int -> (v4:int -> v5:int -> v6:{v6:int | ((v6 = 1) && (v2 = v4))} -> X) -> X
    br_fib_fib_1862: v1:bool -> v2:int -> v3:int -> v4:int -> (v5:int -> v6:int -> v7:int -> X) -> X
    f_main_1851: v1:int -> v2:int -> v3:int -> (v4:unit -> X) -> v6:int -> v7:int -> v8:int -> X
    k_main_1589: v1:int -> v2:int -> v3:int -> v4:int -> v5:int -> v6:int -> (v7:unit -> X) -> v9:bool -> X
    br_k_main_1864: v1:bool ->
                    v2:int -> v3:int -> v4:int -> v5:int -> v6:int -> v7:int -> (v8:unit -> X) -> v10:bool -> X
    fail_1866: v1:{v1:bool | false} -> (v2:unit -> X) -> X
    br_fib''_1860: var30:bool -> var29:int -> (var28:int -> var27:int -> X) -> X
    br_fib'_1858: var37:bool -> var36:int -> var35:int -> var34:int -> (var33:int -> X) -> X
    br_fib_1856: var42:bool -> var41:int -> (var40:int -> X) -> X
    f_1855: var47:int -> var46:int -> var45:int -> var44:unit -> X
    f_fib''_1849: var54:int -> (var53:int -> var52:int -> X) -> var50:int -> var49:int -> X
    f_fib_1847: var59:int -> (var58:int -> X) -> var56:int -> X
    f_fib_1848: var65:int -> var64:int -> (var63:int -> X) -> var61:int -> X
    f_fib_fib_1850: var76:int ->
                    var75:int ->
                    var74:int -> (var73:int -> var72:int -> var71:int -> X) -> var69:int -> var68:int -> var67:int -> X
    fib''_1036: var81:int -> (var80:int -> var79:int -> X) -> X
    fib'_1032: var87:int -> var86:int -> var85:int -> (var84:int -> X) -> X
    fib_1030: var91:int -> (var90:int -> X) -> X
  abstraction types:
    main_1846: X
    f_1852: v1:int -> X
    f_1853: v1:int -> v2:int -> X
    f_1854: v1:int -> v2:int -> v3:int -> X
    main_1043: v1:int -> v2:int -> v3:int -> (v4:unit -> X) -> X
    fib_fib_1039: v1:int -> v2:int -> v3:int -> (v4:int -> v5:int -> v6:int[v6 -> (
                                                                    (v6 = 1) && (v2 = v4))] -> X) -> X
    br_fib_fib_1862: v1:bool -> v2:int -> v3:int -> v4:int -> (v5:int -> v6:int -> v7:int -> X) -> X
    f_main_1851: v1:int -> v2:int -> v3:int -> (v4:unit -> X) -> v6:int -> v7:int -> v8:int -> X
    k_main_1589: v1:int -> v2:int -> v3:int -> v4:int -> v5:int -> v6:int -> (v7:unit -> X) -> v9:bool -> X
    br_k_main_1864: v1:bool ->
                    v2:int -> v3:int -> v4:int -> v5:int -> v6:int -> v7:int -> (v8:unit -> X) -> v10:bool -> X
    fail_1866: v1:bool -> (v2:unit -> X) -> X
    br_fib''_1860: var30:bool -> var29:int -> (var28:int -> var27:int -> X) -> X
    br_fib'_1858: var37:bool -> var36:int -> var35:int -> var34:int -> (var33:int -> X) -> X
    br_fib_1856: var42:bool -> var41:int -> (var40:int -> X) -> X
    f_1855: var47:int -> var46:int -> var45:int -> var44:unit -> X
    f_fib''_1849: var54:int -> (var53:int -> var52:int -> X) -> var50:int -> var49:int -> X
    f_fib_1847: var59:int -> (var58:int -> X) -> var56:int -> X
    f_fib_1848: var65:int -> var64:int -> (var63:int -> X) -> var61:int -> X
    f_fib_fib_1850: var76:int ->
                    var75:int ->
                    var74:int -> (var73:int -> var72:int -> var71:int -> X) -> var69:int -> var68:int -> var67:int -> X
    fib''_1036: var81:int -> (var80:int -> var79:int -> X) -> X
    fib'_1032: var87:int -> var86:int -> var85:int -> (var84:int -> X) -> X
    fib_1030: var91:int -> (var90:int -> X) -> X
  
end AbsTypeInfer.refine(228)[1] (0.024000 sec.)
DONE!

Prefix of spurious counter-example::
0; 0; 0; 0; 0; 1; 0; 0; 0; 1

Program with abstraction types (CEGAR-cycle 1)::
Main: main_1846
  main_1846 -> (rand_int f_1852)
  br_fib''_1860 b_1861 n_1037 k_fib''_1427 when b_1861 -> (k_fib''_1427 0 1)
  br_fib''_1860 b_1861 n_1037 k_fib''_1427 when (not b_1861) ->
      (fib''_1036 (n_1037 - 1) (f_fib''_1849 n_1037 k_fib''_1427))
  br_fib'_1858 b_1859 a_1033 b_1034 n_1035 k_fib'_1393 when b_1859 -> (k_fib'_1393 b_1034)
  br_fib'_1858 b_1859 a_1033 b_1034 n_1035 k_fib'_1393 when (not b_1859) ->
      (fib'_1032 b_1034 (a_1033 + b_1034) (n_1035 - 1) k_fib'_1393)
  br_fib_1856 b_1857 n_1031 k_fib_1341 when b_1857 -> (k_fib_1341 1)
  br_fib_1856 b_1857 n_1031 k_fib_1341 when (not b_1857) -> (fib_1030 (n_1031 - 1) (f_fib_1847 n_1031 k_fib_1341))
  br_fib_fib_1862 b_1863 a_1040 b_1041 n_1042 k_fib_fib_1483 when b_1863 -> (k_fib_fib_1483 b_1041 0 1)
  br_fib_fib_1862 b_1863 a_1040 b_1041 n_1042 k_fib_fib_1483 when (not b_1863) ->
      (fib_fib_1039 b_1041 (a_1040 + b_1041) (n_1042 - 1) (f_fib_fib_1850 a_1040 b_1041 n_1042 k_fib_fib_1483))
  br_k_main_1864 b_1865 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 when b_1865 ->
      (k_main_1571 ())
  br_k_main_1864 b_1865 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 when (
      not b_1865) -> (fail_1866 true k_main_1571)
  f_1852 x_1611 -> (rand_int (f_1853 x_1611))
  f_1853 x_1611 x_1624 -> (rand_int (f_1854 x_1611 x_1624))
  f_1854 x_1611 x_1624 x_1637 -> (main_1043 x_1611 x_1624 x_1637 (f_1855 x_1611 x_1624 x_1637))
  f_1855 x_1611 x_1624 x_1637 x_1608 -> end
  f_fib''_1849 n_1037 k_fib''_1427 x1_1440 x2_1440 -> (k_fib''_1427 x2_1440 (x1_1440 + x2_1440))
  f_fib_1847 n_1031 k_fib_1341 x_1344 -> (fib_1030 (n_1031 - 2) (f_fib_1848 n_1031 x_1344 k_fib_1341))
  f_fib_1848 n_1031 x_1344 k_fib_1341 x_1345 -> (k_fib_1341 (x_1344 + x_1345))
  f_fib_fib_1850 a_1040 b_1041 n_1042 k_fib_fib_1483 x1_1518 x21_1518 x22_1518 ->
      (k_fib_fib_1483 x1_1518 x22_1518 (x21_1518 + x22_1518))
  f_main_1851 a_1044 b_1045 n_1046 k_main_1571 x1_1574 x21_1574 x22_1574 when (
      a_1044 = 0) -> (k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 (b_1045 = 1))
  f_main_1851 a_1044 b_1045 n_1046 k_main_1571 x1_1574 x21_1574 x22_1574 when (
      not (a_1044 = 0)) -> (k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 false)
  fail_1866 b k -> {fail} => (k ())
  fib''_1036 n_1037 k_fib''_1427 when (n_1037 = 0) -> (k_fib''_1427 0 0)
  fib''_1036 n_1037 k_fib''_1427 when (not (n_1037 = 0)) -> (br_fib''_1860 (n_1037 = 1) n_1037 k_fib''_1427)
  fib'_1032 a_1033 b_1034 n_1035 k_fib'_1393 when (n_1035 = 0) -> (k_fib'_1393 a_1033)
  fib'_1032 a_1033 b_1034 n_1035 k_fib'_1393 when (not (n_1035 = 0)) ->
      (br_fib'_1858 (n_1035 = 1) a_1033 b_1034 n_1035 k_fib'_1393)
  fib_1030 n_1031 k_fib_1341 when (n_1031 = 0) -> (k_fib_1341 0)
  fib_1030 n_1031 k_fib_1341 when (not (n_1031 = 0)) -> (br_fib_1856 (n_1031 = 1) n_1031 k_fib_1341)
  fib_fib_1039 a_1040 b_1041 n_1042 k_fib_fib_1483 when (n_1042 = 0) -> (k_fib_fib_1483 a_1040 0 0)
  fib_fib_1039 a_1040 b_1041 n_1042 k_fib_fib_1483 when (not (n_1042 = 0)) ->
      (br_fib_fib_1862 (n_1042 = 1) a_1040 b_1041 n_1042 k_fib_fib_1483)
  k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 when b_1605 ->
      (br_k_main_1864 (x1_1574 = x22_1574) a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605)
  k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 when (not b_1605) -> (k_main_1571 ())
  main_1043 a_1044 b_1045 n_1046 k_main_1571 ->
      (fib_fib_1039 a_1044 b_1045 n_1046 (f_main_1851 a_1044 b_1045 n_1046 k_main_1571))
Types:
  main_1846 : X
  fail_1866 : (x_1:bool[x_1] -> (unit -> X) -> X)
  fib_1030 : (int -> (int -> X) -> X)
  fib_fib_1039 : (int -> x_2:int -> int -> (x_5:int -> int -> x_7:int[x_7 = 1 && x_2 = x_5] -> X) -> X)
  
(1-1) Abstracting ... DONE!

(1-2) Checking HORS ... DONE!

Error trace::
  main_1846 ... --> 
  f_1852 ... --> 
  f_1853 ... --> 
  f_1854 ... --> 
  main_1043 ... --> 
  fib_fib_1039 [2/2] ... --> 
  br_fib_fib_1862 [2/2] ... --> 
  fib_fib_1039 [2/2] ... --> 
  br_fib_fib_1862 [1/2] ... --> 
  f_fib_fib_1850 ... --> 
  f_main_1851 [1/2] ... --> 
  k_main_1589 [1/2] ... --> 
  br_k_main_1864 [2/2] ... --> 
  fail_1866 ... --> fail -->
  ERROR!

Spurious counter-example::
  0; 0; 0; 0; 0; 1; 1; 1; 0; 0; 0; 0; 1; 0

(1-3) Checking counter-example ... DONE!

(1-4) Discovering predicates ... 
begin AbsTypeInfer.refine(4660)[1]
  program:
    main_1846  | true = ((Random.int 0) f_1852)
    br_fib''_1860 b_1861 n_1037 k_fib''_1427 | b_1861 = (k_fib''_1427 0
                                                                    1)
    br_fib''_1860 b_1861 n_1037 k_fib''_1427 | (not b_1861) = (fib''_1036
                                                                 (n_1037 - 1)
                                                                 (f_fib''_1849 n_1037
                                                                    k_fib''_1427))
    br_fib'_1858 b_1859 a_1033 b_1034 n_1035 k_fib'_1393 | b_1859 = (
    k_fib'_1393 b_1034)
    br_fib'_1858 b_1859 a_1033 b_1034 n_1035 k_fib'_1393 | (not b_1859) = (
    fib'_1032 b_1034
              (a_1033 + b_1034)
              (n_1035 - 1)
              k_fib'_1393)
    br_fib_1856 b_1857 n_1031 k_fib_1341 | b_1857 = (k_fib_1341 1)
    br_fib_1856 b_1857 n_1031 k_fib_1341 | (not b_1857) = (fib_1030 (
                                                                    n_1031 - 1)
                                                                    (
                                                                    f_fib_1847 n_1031
                                                                    k_fib_1341))
    br_fib_fib_1862 b_1863 a_1040 b_1041 n_1042 k_fib_fib_1483 | b_1863 = (
    k_fib_fib_1483 b_1041
                   0
                   1)
    br_fib_fib_1862 b_1863 a_1040 b_1041 n_1042 k_fib_fib_1483 | (not b_1863) = (
    fib_fib_1039 b_1041
                 (a_1040 + b_1041)
                 (n_1042 - 1)
                 (f_fib_fib_1850 a_1040
                                 b_1041
                                 n_1042
                                 k_fib_fib_1483))
    br_k_main_1864 b_1865 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 | b_1865 = (
    k_main_1571 ())
    br_k_main_1864 b_1865 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 | (
    not b_1865) = (fail_1866 true
                             k_main_1571)
    f_1852 x_1611 | true = ((Random.int 0) (f_1853 x_1611))
    f_1853 x_1611 x_1624 | true = ((Random.int 0) (f_1854 x_1611
                                                          x_1624))
    f_1854 x_1611 x_1624 x_1637 | true = (main_1043 x_1611
                                                    x_1624
                                                    x_1637
                                                    (f_1855 x_1611
                                                            x_1624
                                                            x_1637))
    f_1855 x_1611 x_1624 x_1637 x_1608 | true = end
    f_fib''_1849 n_1037 k_fib''_1427 x1_1440 x2_1440 | true = (k_fib''_1427 x2_1440
                                                                    (x1_1440 + x2_1440))
    f_fib_1847 n_1031 k_fib_1341 x_1344 | true = (fib_1030 (n_1031 - 2)
                                                           (f_fib_1848 n_1031
                                                                    x_1344
                                                                    k_fib_1341))
    f_fib_1848 n_1031 x_1344 k_fib_1341 x_1345 | true = (k_fib_1341 (
                                                                    x_1344 + x_1345))
    f_fib_fib_1850 a_1040 b_1041 n_1042 k_fib_fib_1483 x1_1518 x21_1518 x22_1518 | true = (
    k_fib_fib_1483 x1_1518
                   x22_1518
                   (x21_1518 + x22_1518))
    f_main_1851 a_1044 b_1045 n_1046 k_main_1571 x1_1574 x21_1574 x22_1574 | (
    a_1044 = 0) = (k_main_1589 a_1044
                               b_1045
                               n_1046
                               x1_1574
                               x21_1574
                               x22_1574
                               k_main_1571
                               (b_1045 = 1))
    f_main_1851 a_1044 b_1045 n_1046 k_main_1571 x1_1574 x21_1574 x22_1574 | (
    not (a_1044 = 0)) = (k_main_1589 a_1044
                                     b_1045
                                     n_1046
                                     x1_1574
                                     x21_1574
                                     x22_1574
                                     k_main_1571
                                     false)
    fail_1866 b k | true = (fail ())
    fib''_1036 n_1037 k_fib''_1427 | (n_1037 = 0) = (k_fib''_1427 0
                                                                  0)
    fib''_1036 n_1037 k_fib''_1427 | (not (n_1037 = 0)) = (br_fib''_1860 (
                                                                    n_1037 = 1)
                                                                    n_1037
                                                                    k_fib''_1427)
    fib'_1032 a_1033 b_1034 n_1035 k_fib'_1393 | (n_1035 = 0) = (k_fib'_1393 a_1033)
    fib'_1032 a_1033 b_1034 n_1035 k_fib'_1393 | (not (n_1035 = 0)) = (
    br_fib'_1858 (n_1035 = 1)
                 a_1033
                 b_1034
                 n_1035
                 k_fib'_1393)
    fib_1030 n_1031 k_fib_1341 | (n_1031 = 0) = (k_fib_1341 0)
    fib_1030 n_1031 k_fib_1341 | (not (n_1031 = 0)) = (br_fib_1856 (n_1031 = 1)
                                                                   n_1031
                                                                   k_fib_1341)
    fib_fib_1039 a_1040 b_1041 n_1042 k_fib_fib_1483 | (n_1042 = 0) = (
    k_fib_fib_1483 a_1040
                   0
                   0)
    fib_fib_1039 a_1040 b_1041 n_1042 k_fib_fib_1483 | (not (n_1042 = 0)) = (
    br_fib_fib_1862 (n_1042 = 1)
                    a_1040
                    b_1041
                    n_1042
                    k_fib_fib_1483)
    k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 | b_1605 = (
    br_k_main_1864 (x1_1574 = x22_1574)
                   a_1044
                   b_1045
                   n_1046
                   x1_1574
                   x21_1574
                   x22_1574
                   k_main_1571
                   b_1605)
    k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 | (
    not b_1605) = (k_main_1571 ())
    main_1043 a_1044 b_1045 n_1046 k_main_1571 | true = (fib_fib_1039
                                                           a_1044
                                                           b_1045
                                                           n_1046
                                                           (f_main_1851 a_1044
                                                                    b_1045
                                                                    n_1046
                                                                    k_main_1571))
    main_1846:X
    br_fib''_1860:bool -> int -> (int -> int -> X) -> X
    br_fib'_1858:bool -> int -> int -> int -> (int -> X) -> X
    br_fib_1856:bool -> int -> (int -> X) -> X
    br_fib_fib_1862:bool -> int -> int -> int -> (int -> int -> int -> X) -> X
    br_k_main_1864:bool -> int -> int -> int -> int -> int -> int -> (unit -> X) -> bool -> X
    f_1852:int -> X
    f_1853:int -> int -> X
    f_1854:int -> int -> int -> X
    f_1855:int -> int -> int -> unit -> X
    f_fib''_1849:int -> (int -> int -> X) -> int -> int -> X
    f_fib_1847:int -> (int -> X) -> int -> X
    f_fib_1848:int -> int -> (int -> X) -> int -> X
    f_fib_fib_1850:int -> int -> int -> (int -> int -> int -> X) -> int -> int -> int -> X
    f_main_1851:int -> int -> int -> (unit -> X) -> int -> int -> int -> X
    fail_1866:bool -> (unit -> X) -> X
    fib''_1036:int -> (int -> int -> X) -> X
    fib'_1032:int -> int -> int -> (int -> X) -> X
    fib_1030:int -> (int -> X) -> X
    fib_fib_1039:int -> int -> int -> (int -> int -> int -> X) -> X
    k_main_1589:int -> int -> int -> int -> int -> int -> (unit -> X) -> bool -> X
    main_1043:int -> int -> int -> (unit -> X) -> X
  inlined functions: br_fib''_1860,br_fib'_1858,br_fib_1856,br_fib_fib_1862,br_k_main_1864,f_1852,f_1853,f_1854,f_1855,f_fib''_1849,f_fib_1847,f_fib_1848,f_fib_fib_1850,f_main_1851,fib''_1036,fib'_1032,k_main_1589,main_1043
  counterexample: 0:0:0:0:0:1:1:1:0:0:0:0:1:0
  error traces:
    [true.nop(<f_1852@39:0> = var92)
     [true.nop((<f_1853@42:0> = <f_1852@39:0>) && (<f_1853@42:1> = var93))
      [true.nop((<f_1854@45:0> = <f_1853@42:0>) && ((<f_1854@45:1> = <f_1853@42:1>) && (<f_1854@45:2> = var94)))
       [true.
        ((<main_1043@47:0> = <f_1854@45:0>) &&
         ((<main_1043@47:1> = <f_1854@45:1>) && (<main_1043@47:2> = <f_1854@45:2>)))
        [true.
         ((<fib_fib_1039@49:0> = <main_1043@47:0>) &&
          ((<fib_fib_1039@49:1> = <main_1043@47:1>) && (<fib_fib_1039@49:2> = <main_1043@47:2>)))
         [(not (<fib_fib_1039@49:2> = 0)).
          ((<br_fib_fib_1862@52:0> = (<fib_fib_1039@49:2> = 1)) &&
           ((<br_fib_fib_1862@52:1> = <fib_fib_1039@49:0>) &&
            ((<br_fib_fib_1862@52:2> = <fib_fib_1039@49:1>) && (<br_fib_fib_1862@52:3> = <fib_fib_1039@49:2>))))
          [(not <br_fib_fib_1862@52:0>).
           ((<fib_fib_1039@55:0> = <br_fib_fib_1862@52:2>) &&
            ((<fib_fib_1039@55:1> = (<br_fib_fib_1862@52:1> + <br_fib_fib_1862@52:2>)) &&
             (<fib_fib_1039@55:2> = (<br_fib_fib_1862@52:3> - 1))))
           [(not (<fib_fib_1039@55:2> = 0)).
            ((<br_fib_fib_1862@58:0> = (<fib_fib_1039@55:2> = 1)) &&
             ((<br_fib_fib_1862@58:1> = <fib_fib_1039@55:0>) &&
              ((<br_fib_fib_1862@58:2> = <fib_fib_1039@55:1>) && (<br_fib_fib_1862@58:3> = <fib_fib_1039@55:2>))))
            [<br_fib_fib_1862@58:0>.
             ((<<br_fib_fib_1862@58:4>@61:0> = <br_fib_fib_1862@58:2>) &&
              ((<<br_fib_fib_1862@58:4>@61:1> = 0) && (<<br_fib_fib_1862@58:4>@61:2> = 1)))
             [true.
              ((<<fib_fib_1039@55:3>@63:0> = <<br_fib_fib_1862@58:4>@61:0>) &&
               ((<<fib_fib_1039@55:3>@63:1> = <<br_fib_fib_1862@58:4>@61:1>) &&
                (<<fib_fib_1039@55:3>@63:2> = <<br_fib_fib_1862@58:4>@61:2>)))
              [true.
               ((<f_fib_fib_1850@65:0> = <br_fib_fib_1862@52:1>) &&
                ((<f_fib_fib_1850@65:1> = <br_fib_fib_1862@52:2>) &&
                 ((<f_fib_fib_1850@65:2> = <br_fib_fib_1862@52:3>) &&
                  ((<f_fib_fib_1850@65:4> = <<fib_fib_1039@55:3>@63:0>) &&
                   ((<f_fib_fib_1850@65:5> = <<fib_fib_1039@55:3>@63:1>) &&
                    (<f_fib_fib_1850@65:6> = <<fib_fib_1039@55:3>@63:2>))))))
               [true.
                ((<<f_fib_fib_1850@65:3>@67:0> = <f_fib_fib_1850@65:4>) &&
                 ((<<f_fib_fib_1850@65:3>@67:1> = <f_fib_fib_1850@65:6>) &&
                  (<<f_fib_fib_1850@65:3>@67:2> = (<f_fib_fib_1850@65:5> + <f_fib_fib_1850@65:6>))))
                [true.
                 ((<<br_fib_fib_1862@52:4>@69:0> = <<f_fib_fib_1850@65:3>@67:0>) &&
                  ((<<br_fib_fib_1862@52:4>@69:1> = <<f_fib_fib_1850@65:3>@67:1>) &&
                   (<<br_fib_fib_1862@52:4>@69:2> = <<f_fib_fib_1850@65:3>@67:2>)))
                 [true.
                  ((<<fib_fib_1039@49:3>@71:0> = <<br_fib_fib_1862@52:4>@69:0>) &&
                   ((<<fib_fib_1039@49:3>@71:1> = <<br_fib_fib_1862@52:4>@69:1>) &&
                    (<<fib_fib_1039@49:3>@71:2> = <<br_fib_fib_1862@52:4>@69:2>)))
                  [true.
                   ((<f_main_1851@73:0> = <main_1043@47:0>) &&
                    ((<f_main_1851@73:1> = <main_1043@47:1>) &&
                     ((<f_main_1851@73:2> = <main_1043@47:2>) &&
                      ((<f_main_1851@73:4> = <<fib_fib_1039@49:3>@71:0>) &&
                       ((<f_main_1851@73:5> = <<fib_fib_1039@49:3>@71:1>) &&
                        (<f_main_1851@73:6> = <<fib_fib_1039@49:3>@71:2>))))))
                   [(<f_main_1851@73:0> = 0).
                    ((<k_main_1589@76:0> = <f_main_1851@73:0>) &&
                     ((<k_main_1589@76:1> = <f_main_1851@73:1>) &&
                      ((<k_main_1589@76:2> = <f_main_1851@73:2>) &&
                       ((<k_main_1589@76:3> = <f_main_1851@73:4>) &&
                        ((<k_main_1589@76:4> = <f_main_1851@73:5>) &&
                         ((<k_main_1589@76:5> = <f_main_1851@73:6>) && (<k_main_1589@76:7> = (<f_main_1851@73:1> = 1))))))))
                    [<k_main_1589@76:7>.
                     ((<br_k_main_1864@79:0> = (<k_main_1589@76:3> = <k_main_1589@76:5>)) &&
                      ((<br_k_main_1864@79:1> = <k_main_1589@76:0>) &&
                       ((<br_k_main_1864@79:2> = <k_main_1589@76:1>) &&
                        ((<br_k_main_1864@79:3> = <k_main_1589@76:2>) &&
                         ((<br_k_main_1864@79:4> = <k_main_1589@76:3>) &&
                          ((<br_k_main_1864@79:5> = <k_main_1589@76:4>) &&
                           ((<br_k_main_1864@79:6> = <k_main_1589@76:5>) &&
                            (<br_k_main_1864@79:8> = <k_main_1589@76:7>))))))))
                     [(not <br_k_main_1864@79:0>).(<fail_1866@82:0> = true)[true.error
  begin RefTypeInfer.infer_etrs(4661)[2]
    horn clauses:
      P[<fail_1866@82:0>](<fail_1866@82:0>:bool)|- bot
      
      P[<br_k_main_1864@79:8>](false:bool,<br_k_main_1864@79:1>:int,<br_k_main_1864@79:2>:int,<br_k_main_1864@79:3>:int,<br_k_main_1864@79:4>:int,<br_k_main_1864@79:5>:int,<br_k_main_1864@79:6>:int,<br_k_main_1864@79:8>:bool),
      <fail_1866@82:0> |- P[<fail_1866@82:0>](<fail_1866@82:0>:bool)
      
      P[<k_main_1589@76:7>](<br_k_main_1864@79:1>:int,<br_k_main_1864@79:2>:int,<br_k_main_1864@79:3>:int,<br_k_main_1864@79:4>:int,<br_k_main_1864@79:5>:int,<br_k_main_1864@79:6>:int,<br_k_main_1864@79:8>:bool),
      (<br_k_main_1864@79:8> && (<br_k_main_1864@79:0> = (<br_k_main_1864@79:4> = <br_k_main_1864@79:6>)))
      |- P[<br_k_main_1864@79:8>](<br_k_main_1864@79:0>:bool,<br_k_main_1864@79:1>:int,
                                  <br_k_main_1864@79:2>:int,<br_k_main_1864@79:3>:int,
                                  <br_k_main_1864@79:4>:int,<br_k_main_1864@79:5>:int,
                                  <br_k_main_1864@79:6>:int,<br_k_main_1864@79:8>:bool)
      
      P[<f_main_1851@73:6>](<k_main_1589@76:0>:int,<k_main_1589@76:1>:int,<k_main_1589@76:2>:int,<k_main_1589@76:3>:int,<k_main_1589@76:4>:int,<k_main_1589@76:5>:int),
      ((<k_main_1589@76:0> = 0) && (<k_main_1589@76:7> = (<k_main_1589@76:1> = 1)))
      |- P[<k_main_1589@76:7>](<k_main_1589@76:0>:int,<k_main_1589@76:1>:int,
                               <k_main_1589@76:2>:int,<k_main_1589@76:3>:int,
                               <k_main_1589@76:4>:int,<k_main_1589@76:5>:int,
                               <k_main_1589@76:7>:bool)
      
      P[<main_1043@47:2>](<f_main_1851@73:0>:int,<f_main_1851@73:1>:int,<f_main_1851@73:2>:int),
      P[<<fib_fib_1039@49:3>@71:2>](<f_main_1851@73:0>:int,<f_main_1851@73:1>:int,<f_main_1851@73:2>:int,<f_main_1851@73:4>:int,<f_main_1851@73:5>:int,<f_main_1851@73:6>:int)|- P[<f_main_1851@73:6>](
      <f_main_1851@73:0>:int,<f_main_1851@73:1>:int,<f_main_1851@73:2>:int,
      <f_main_1851@73:4>:int,<f_main_1851@73:5>:int,<f_main_1851@73:6>:int)
      
      P[<fib_fib_1039@49:2>](<fib_fib_1039@49:0>:int,<fib_fib_1039@49:1>:int,<fib_fib_1039@49:2>:int),
      P[<<br_fib_fib_1862@52:4>@69:2>]((<fib_fib_1039@49:2> = 1):bool,<fib_fib_1039@49:0>:int,<fib_fib_1039@49:1>:int,<fib_fib_1039@49:2>:int,<<fib_fib_1039@49:3>@71:0>:int,<<fib_fib_1039@49:3>@71:1>:int,<<fib_fib_1039@49:3>@71:2>:int),
      (<fib_fib_1039@49:2> <> 0)
      |- P[<<fib_fib_1039@49:3>@71:2>](<fib_fib_1039@49:0>:int,<fib_fib_1039@49:1>:int,
                                       <fib_fib_1039@49:2>:int,<<fib_fib_1039@49:3>@71:0>:int,
                                       <<fib_fib_1039@49:3>@71:1>:int,
                                       <<fib_fib_1039@49:3>@71:2>:int)
      
      P[<br_fib_fib_1862@52:3>](<br_fib_fib_1862@52:0>:bool,<br_fib_fib_1862@52:1>:int,<br_fib_fib_1862@52:2>:int,<br_fib_fib_1862@52:3>:int),
      P[<<f_fib_fib_1850@65:3>@67:2>](<br_fib_fib_1862@52:1>:int,<br_fib_fib_1862@52:2>:int,<br_fib_fib_1862@52:3>:int,<<br_fib_fib_1862@52:4>@69:0>:int,<<br_fib_fib_1862@52:4>@69:1>:int,<<br_fib_fib_1862@52:4>@69:2>:int),
      (not <br_fib_fib_1862@52:0>)
      |- P[<<br_fib_fib_1862@52:4>@69:2>](<br_fib_fib_1862@52:0>:bool,
                                          <br_fib_fib_1862@52:1>:int,
                                          <br_fib_fib_1862@52:2>:int,
                                          <br_fib_fib_1862@52:3>:int,
                                          <<br_fib_fib_1862@52:4>@69:0>:int,
                                          <<br_fib_fib_1862@52:4>@69:1>:int,
                                          <<br_fib_fib_1862@52:4>@69:2>:int)
      
      P[<f_fib_fib_1850@65:6>](<f_fib_fib_1850@65:0>:int,<f_fib_fib_1850@65:1>:int,<f_fib_fib_1850@65:2>:int,<<f_fib_fib_1850@65:3>@67:0>:int,(
                               <<f_fib_fib_1850@65:3>@67:2> + (-1 * <<f_fib_fib_1850@65:3>@67:1>)):int,<<f_fib_fib_1850@65:3>@67:1>:int)|- P[<<f_fib_fib_1850@65:3>@67:2>](
      <f_fib_fib_1850@65:0>:int,<f_fib_fib_1850@65:1>:int,<f_fib_fib_1850@65:2>:int,
      <<f_fib_fib_1850@65:3>@67:0>:int,<<f_fib_fib_1850@65:3>@67:1>:int,
      <<f_fib_fib_1850@65:3>@67:2>:int)
      
      P[<br_fib_fib_1862@52:3>](false:bool,<f_fib_fib_1850@65:0>:int,<f_fib_fib_1850@65:1>:int,<f_fib_fib_1850@65:2>:int),
      P[<<fib_fib_1039@55:3>@63:2>](<f_fib_fib_1850@65:1>:int,(<f_fib_fib_1850@65:0> + <f_fib_fib_1850@65:1>):int,(
                                    -1 + <f_fib_fib_1850@65:2>):int,<f_fib_fib_1850@65:4>:int,<f_fib_fib_1850@65:5>:int,<f_fib_fib_1850@65:6>:int)|- P[<f_fib_fib_1850@65:6>](
      <f_fib_fib_1850@65:0>:int,<f_fib_fib_1850@65:1>:int,<f_fib_fib_1850@65:2>:int,
      <f_fib_fib_1850@65:4>:int,<f_fib_fib_1850@65:5>:int,<f_fib_fib_1850@65:6>:int)
      
      P[<fib_fib_1039@55:2>](<fib_fib_1039@55:0>:int,<fib_fib_1039@55:1>:int,<fib_fib_1039@55:2>:int),
      P[<<br_fib_fib_1862@58:4>@61:2>]((<fib_fib_1039@55:2> = 1):bool,<fib_fib_1039@55:0>:int,<fib_fib_1039@55:1>:int,<fib_fib_1039@55:2>:int,<<fib_fib_1039@55:3>@63:0>:int,<<fib_fib_1039@55:3>@63:1>:int,<<fib_fib_1039@55:3>@63:2>:int),
      (<fib_fib_1039@55:2> <> 0)
      |- P[<<fib_fib_1039@55:3>@63:2>](<fib_fib_1039@55:0>:int,<fib_fib_1039@55:1>:int,
                                       <fib_fib_1039@55:2>:int,<<fib_fib_1039@55:3>@63:0>:int,
                                       <<fib_fib_1039@55:3>@63:1>:int,
                                       <<fib_fib_1039@55:3>@63:2>:int)
      
      P[<br_fib_fib_1862@58:3>](<br_fib_fib_1862@58:0>:bool,<br_fib_fib_1862@58:1>:int,<br_fib_fib_1862@58:2>:int,<br_fib_fib_1862@58:3>:int),
      ((<<br_fib_fib_1862@58:4>@61:1> = 0) &&
       ((<<br_fib_fib_1862@58:4>@61:2> = 1) &&
        ((<<br_fib_fib_1862@58:4>@61:0> = <br_fib_fib_1862@58:2>) && <br_fib_fib_1862@58:0>)))
      |- P[<<br_fib_fib_1862@58:4>@61:2>](<br_fib_fib_1862@58:0>:bool,
                                          <br_fib_fib_1862@58:1>:int,
                                          <br_fib_fib_1862@58:2>:int,
                                          <br_fib_fib_1862@58:3>:int,
                                          <<br_fib_fib_1862@58:4>@61:0>:int,
                                          <<br_fib_fib_1862@58:4>@61:1>:int,
                                          <<br_fib_fib_1862@58:4>@61:2>:int)
      
      P[<fib_fib_1039@55:2>](<br_fib_fib_1862@58:1>:int,<br_fib_fib_1862@58:2>:int,<br_fib_fib_1862@58:3>:int),
      ((<br_fib_fib_1862@58:3> <> 0) && (<br_fib_fib_1862@58:0> = (<br_fib_fib_1862@58:3> = 1)))
      |- P[<br_fib_fib_1862@58:3>](<br_fib_fib_1862@58:0>:bool,<br_fib_fib_1862@58:1>:int,
                                   <br_fib_fib_1862@58:2>:int,<br_fib_fib_1862@58:3>:int)
      
      P[<br_fib_fib_1862@52:3>](false:bool,(<fib_fib_1039@55:1> + (-1 * <fib_fib_1039@55:0>)):int,<fib_fib_1039@55:0>:int,(
                                1 + <fib_fib_1039@55:2>):int)|- P[<fib_fib_1039@55:2>](
      <fib_fib_1039@55:0>:int,<fib_fib_1039@55:1>:int,<fib_fib_1039@55:2>:int)
      
      P[<fib_fib_1039@49:2>](<br_fib_fib_1862@52:1>:int,<br_fib_fib_1862@52:2>:int,<br_fib_fib_1862@52:3>:int),
      ((<br_fib_fib_1862@52:3> <> 0) && (<br_fib_fib_1862@52:0> = (<br_fib_fib_1862@52:3> = 1)))
      |- P[<br_fib_fib_1862@52:3>](<br_fib_fib_1862@52:0>:bool,<br_fib_fib_1862@52:1>:int,
                                   <br_fib_fib_1862@52:2>:int,<br_fib_fib_1862@52:3>:int)
      
      P[<main_1043@47:2>](<fib_fib_1039@49:0>:int,<fib_fib_1039@49:1>:int,<fib_fib_1039@49:2>:int)|- P[<fib_fib_1039@49:2>](
      <fib_fib_1039@49:0>:int,<fib_fib_1039@49:1>:int,<fib_fib_1039@49:2>:int)
      
      P[<f_1854@45:2>](<main_1043@47:0>:int,<main_1043@47:1>:int,<main_1043@47:2>:int)|- P[<main_1043@47:2>](
      <main_1043@47:0>:int,<main_1043@47:1>:int,<main_1043@47:2>:int)
      
      P[<f_1853@42:1>](<f_1854@45:0>:int,<f_1854@45:1>:int)|- P[<f_1854@45:2>](
      <f_1854@45:0>:int,<f_1854@45:1>:int,<f_1854@45:2>:int)
      
      P[<f_1852@39:0>](<f_1853@42:0>:int)|- P[<f_1853@42:1>](<f_1853@42:0>:int,
                                                             <f_1853@42:1>:int)
      
      |- P[<f_1852@39:0>](<f_1852@39:0>:int)
    call trees:
      <main_1846@36>
        <f_1852@39>
          <f_1853@42>
            <f_1854@45>
              <main_1043@47>
                <fib_fib_1039@49>
                  <br_fib_fib_1862@52>
                    <fib_fib_1039@55>
                      <br_fib_fib_1862@58>
                      </<br_fib_fib_1862@58:4>@61>
                    </<fib_fib_1039@55:3>@63>
                    <f_fib_fib_1850@65>
                    </<f_fib_fib_1850@65:3>@67>
                  </<br_fib_fib_1862@52:4>@69>
                </<fib_fib_1039@49:3>@71>
                <f_main_1851@73>
                  <k_main_1589@76>
                    <br_k_main_1864@79>
                      <fail_1866@82>
    inlined horn clauses:
      P[<fail_1866@82:0>](<fail_1866@82:0>:bool)|- bot
      
      P[<<fib_fib_1039@49:3>@71:2>](0:int,1:int,<br_k_main_1864@79:3>:int,<br_k_main_1864@79:4>:int,<br_k_main_1864@79:5>:int,<br_k_main_1864@79:6>:int),
      ((<br_k_main_1864@79:4> <> <br_k_main_1864@79:6>) && <fail_1866@82:0>)
      |- P[<fail_1866@82:0>](<fail_1866@82:0>:bool)
      
      P[<fib_fib_1039@49:2>](<fib_fib_1039@49:0>:int,<fib_fib_1039@49:1>:int,<fib_fib_1039@49:2>:int),
      P[<<fib_fib_1039@55:3>@63:2>](<fib_fib_1039@49:1>:int,(<fib_fib_1039@49:0> + <fib_fib_1039@49:1>):int,(
                                    -1 + <fib_fib_1039@49:2>):int,<<fib_fib_1039@49:3>@71:0>:int,(
                                    <<fib_fib_1039@49:3>@71:2> + (-1 * <<fib_fib_1039@49:3>@71:1>)):int,<<fib_fib_1039@49:3>@71:1>:int),
      ((<fib_fib_1039@49:2> <> 0) && (<fib_fib_1039@49:2> <> 1))
      |- P[<<fib_fib_1039@49:3>@71:2>](<fib_fib_1039@49:0>:int,<fib_fib_1039@49:1>:int,
                                       <fib_fib_1039@49:2>:int,<<fib_fib_1039@49:3>@71:0>:int,
                                       <<fib_fib_1039@49:3>@71:1>:int,
                                       <<fib_fib_1039@49:3>@71:2>:int)
      
      P[<fib_fib_1039@55:2>](<fib_fib_1039@55:0>:int,<fib_fib_1039@55:1>:int,<fib_fib_1039@55:2>:int),
      ((<fib_fib_1039@55:2> = 1) &&
       ((<<fib_fib_1039@55:3>@63:1> = 0) &&
        ((<<fib_fib_1039@55:3>@63:2> = 1) && (<<fib_fib_1039@55:3>@63:0> = <fib_fib_1039@55:1>))))
      |- P[<<fib_fib_1039@55:3>@63:2>](<fib_fib_1039@55:0>:int,<fib_fib_1039@55:1>:int,
                                       <fib_fib_1039@55:2>:int,<<fib_fib_1039@55:3>@63:0>:int,
                                       <<fib_fib_1039@55:3>@63:1>:int,
                                       <<fib_fib_1039@55:3>@63:2>:int)
      
      P[<fib_fib_1039@49:2>]((<fib_fib_1039@55:1> + (-1 * <fib_fib_1039@55:0>)):int,<fib_fib_1039@55:0>:int,(
                             1 + <fib_fib_1039@55:2>):int),
      (((1 + <fib_fib_1039@55:2>) <> 0) && (<fib_fib_1039@55:2> <> 0))
      |- P[<fib_fib_1039@55:2>](<fib_fib_1039@55:0>:int,<fib_fib_1039@55:1>:int,
                                <fib_fib_1039@55:2>:int)
      
      |- P[<fib_fib_1039@49:2>](<fib_fib_1039@49:0>:int,<fib_fib_1039@49:1>:int,
                                <fib_fib_1039@49:2>:int)
    begin ParamSubstInfer.infer(8730)[3]
      
    end ParamSubstInfer.infer(8730)[3] (0.008000 sec.)
    inferred extra parameters:
      
    begin RefTypeInfer.elim_coeffs(8732)[3]
      
    end RefTypeInfer.elim_coeffs(8732)[3] (0.000000 sec.)
    begin BwHcSolver.solve(9131)[3]
      lower bounds:
        P[<fib_fib_1039@49:2>](<fib_fib_1039@49:0>:int,<fib_fib_1039@49:1>:int,<fib_fib_1039@49:2>:int) = true
        P[<fib_fib_1039@55:2>](<fib_fib_1039@55:0>:int,<fib_fib_1039@55:1>:int,<fib_fib_1039@55:2>:int) =
        (((1 + <fib_fib_1039@55:2>) <> 0) && (<fib_fib_1039@55:2> <> 0))
        P[<<fib_fib_1039@55:3>@63:2>](<fib_fib_1039@55:0>:int,<fib_fib_1039@55:1>:int,<fib_fib_1039@55:2>:int,<<fib_fib_1039@55:3>@63:0>:int,<<fib_fib_1039@55:3>@63:1>:int,<<fib_fib_1039@55:3>@63:2>:int) =
        ((<fib_fib_1039@55:2> = 1) &&
         ((<<fib_fib_1039@55:3>@63:1> = 0) &&
          ((<<fib_fib_1039@55:3>@63:2> = 1) && (<<fib_fib_1039@55:3>@63:0> = <fib_fib_1039@55:1>))))
        P[<<fib_fib_1039@49:3>@71:2>](<fib_fib_1039@49:0>:int,<fib_fib_1039@49:1>:int,<fib_fib_1039@49:2>:int,<<fib_fib_1039@49:3>@71:0>:int,<<fib_fib_1039@49:3>@71:1>:int,<<fib_fib_1039@49:3>@71:2>:int) =
        ((<fib_fib_1039@49:2> = 2) &&
         ((<<fib_fib_1039@49:3>@71:1> = 1) &&
          ((<<fib_fib_1039@49:3>@71:0> = (<fib_fib_1039@49:0> + <fib_fib_1039@49:1>)) &&
           (<<fib_fib_1039@49:3>@71:2> = <<fib_fib_1039@49:3>@71:1>))))
        P[<fail_1866@82:0>](<fail_1866@82:0>:bool) = false
      begin BwHcSolver.solve_preds(9781)[4]
        input:
          P[<fail_1866@82:0>](<fail_1866@82:0>:bool) = false
          P[<fail_1866@82:0>](<fail_1866@82:0>:bool)|- bot
        finding a solution to P[<fail_1866@82:0>](var95:bool)
        begin InterpProver.interpolate(9791)[5]
          begin InterpProver.interpolate_fresh(9792)[6]
            begin InterpProver.interpolate_log(9793)[7]
              input1: false
              input2: true
              begin InterpProver.interpolate_check(9794)[8]
                begin InterpProver.interpolate_simplify(9795)[9]
                  begin InterpProver.interpolate_quick(9808)[10]
                    
                  end InterpProver.interpolate_quick(9808)[10] (0.000000 sec.)
                  begin minimizing # of disjunctions(9812)[10]
                    input: false
                    output: false
                  end minimizing # of disjunctions(9812)[10] (0.000000 sec.)
                  
                end InterpProver.interpolate_simplify(9795)[9] (0.004000 sec.)
                
              end InterpProver.interpolate_check(9794)[8] (0.004000 sec.)
              output: false
            end InterpProver.interpolate_log(9793)[7] (0.004000 sec.)
            
          end InterpProver.interpolate_fresh(9792)[6] (0.004000 sec.)
          
        end InterpProver.interpolate(9791)[5] (0.004000 sec.)
        solution:
          P[<fail_1866@82:0>](var95:bool) = false
        
      end BwHcSolver.solve_preds(9781)[4] (0.004000 sec.)
      begin BwHcSolver.solve_preds(9830)[4]
        input:
          P[<<fib_fib_1039@49:3>@71:2>](<fib_fib_1039@49:0>:int,<fib_fib_1039@49:1>:int,<fib_fib_1039@49:2>:int,<<fib_fib_1039@49:3>@71:0>:int,<<fib_fib_1039@49:3>@71:1>:int,<<fib_fib_1039@49:3>@71:2>:int) =
          ((<fib_fib_1039@49:2> = 2) &&
           ((<<fib_fib_1039@49:3>@71:1> = 1) &&
            ((<<fib_fib_1039@49:3>@71:0> = (<fib_fib_1039@49:0> + <fib_fib_1039@49:1>)) &&
             (<<fib_fib_1039@49:3>@71:2> = <<fib_fib_1039@49:3>@71:1>))))
          P[<<fib_fib_1039@49:3>@71:2>](0:int,1:int,<br_k_main_1864@79:3>:int,<br_k_main_1864@79:4>:int,<br_k_main_1864@79:5>:int,<br_k_main_1864@79:6>:int),
          ((<br_k_main_1864@79:4> <> <br_k_main_1864@79:6>) && <fail_1866@82:0>) |- bot
        finding a solution to P[<<fib_fib_1039@49:3>@71:2>](var96:int,var97:int,var98:int,var99:int,var100:int,var101:int)
        begin InterpProver.interpolate(9963)[5]
          begin InterpProver.interpolate_fresh(9964)[6]
            begin InterpProver.interpolate_log(9965)[7]
              input1: ((var101 = var100) && ((var99 = (var96 + var97)) && ((var100 = 1) && (var98 = 2))))
              input2: ((var99 <> var101) && ((var96 = 0) && (var97 = 1)))
              begin InterpProver.interpolate_check(9966)[8]
                begin InterpProver.interpolate_simplify(9967)[9]
                  begin InterpProver.interpolate_quick(10128)[10]
                    begin CsisatInterface.interpolate_csisat_wrap(10137)[11]
                      begin CsisatInterface.interpolate_csisat_post_process(10138)[12]
                        begin CsisatInterface.interpolate_csisat_log(10139)[13]
                          input1: (1 = v_sep_var101 & v_sep_var99 = (v_sep_var96 + v_sep_var97))
                          input2: (not v_sep_var101 = v_sep_var99 & 0 = v_sep_var96 & 1 = v_sep_var97)
                          begin CsisatInterface.interpolate_csisat_raw(10140)[14]
                            
                          end CsisatInterface.interpolate_csisat_raw(10140)[14] (0.000000 sec.)
                          output: (1 = v_sep_var101 & v_sep_var99 = (v_sep_var96 + v_sep_var97))
                          
                        end CsisatInterface.interpolate_csisat_log(10139)[13] (0.000000 sec.)
                        after simplification: (1 = v_sep_var101 & v_sep_var99 = (v_sep_var96 + v_sep_var97))
                        after dnf conversion: ((1 = v_sep_var101 & v_sep_var99 = (v_sep_var96 + v_sep_var97)))
                        
                      end CsisatInterface.interpolate_csisat_post_process(10138)[12] (0.004000 sec.)
                      
                    end CsisatInterface.interpolate_csisat_wrap(10137)[11] (0.004000 sec.)
                    
                  end InterpProver.interpolate_quick(10128)[10] (0.004000 sec.)
                  begin minimizing # of conjunctions(10158)[10]
                    input: ((var101 = 1) && (var99 = (var96 + var97)))
                    output: ((var99 = (var96 + var97)) && (var101 = 1))
                  end minimizing # of conjunctions(10158)[10] (0.000000 sec.)
                  
                end InterpProver.interpolate_simplify(9967)[9] (0.004000 sec.)
                
              end InterpProver.interpolate_check(9966)[8] (0.004000 sec.)
              output: ((var99 = (var96 + var97)) && (var101 = 1))
            end InterpProver.interpolate_log(9965)[7] (0.004000 sec.)
            
          end InterpProver.interpolate_fresh(9964)[6] (0.004000 sec.)
          
        end InterpProver.interpolate(9963)[5] (0.004000 sec.)
        solution:
          P[<<fib_fib_1039@49:3>@71:2>](var96:int,var97:int,var98:int,var99:int,var100:int,var101:int) =
          ((var99 = (var96 + var97)) && (var101 = 1))
        
      end BwHcSolver.solve_preds(9830)[4] (0.004000 sec.)
      begin BwHcSolver.solve_preds(10296)[4]
        input:
          P[<fib_fib_1039@49:2>](<fib_fib_1039@49:0>:int,<fib_fib_1039@49:1>:int,<fib_fib_1039@49:2>:int) = true
          P[<<fib_fib_1039@55:3>@63:2>](<fib_fib_1039@55:0>:int,<fib_fib_1039@55:1>:int,<fib_fib_1039@55:2>:int,<<fib_fib_1039@55:3>@63:0>:int,<<fib_fib_1039@55:3>@63:1>:int,<<fib_fib_1039@55:3>@63:2>:int) =
          ((<fib_fib_1039@55:2> = 1) &&
           ((<<fib_fib_1039@55:3>@63:1> = 0) &&
            ((<<fib_fib_1039@55:3>@63:2> = 1) && (<<fib_fib_1039@55:3>@63:0> = <fib_fib_1039@55:1>))))
          P[<fib_fib_1039@49:2>](<fib_fib_1039@49:0>:int,<fib_fib_1039@49:1>:int,<fib_fib_1039@49:2>:int),
          P[<<fib_fib_1039@55:3>@63:2>](<fib_fib_1039@49:1>:int,(<fib_fib_1039@49:0> + <fib_fib_1039@49:1>):int,(
                                        -1 + <fib_fib_1039@49:2>):int,<<fib_fib_1039@49:3>@71:0>:int,(
                                        <<fib_fib_1039@49:3>@71:2> + (
                                        -1 * <<fib_fib_1039@49:3>@71:1>)):int,<<fib_fib_1039@49:3>@71:1>:int),
          (((<fib_fib_1039@49:2> <> 0) && (<fib_fib_1039@49:2> <> 1)) &&
           (not
              ((<<fib_fib_1039@49:3>@71:2> = 1) &&
               (<<fib_fib_1039@49:3>@71:0> = (<fib_fib_1039@49:0> + <fib_fib_1039@49:1>))))) |- bot
        finding a solution to P[<<fib_fib_1039@55:3>@63:2>](var102:int,var103:int,var104:int,var105:int,var106:int,var107:int)
        begin InterpProver.interpolate(10618)[5]
          begin InterpProver.interpolate_fresh(10619)[6]
            begin InterpProver.interpolate_log(10620)[7]
              input1: ((var105 = var103) && ((var107 = 1) && ((var106 = 0) && (var104 = 1))))
              input2: ((((var106 + var107) <> 1) || (var105 <> var103)) && ((var104 <> 0) && ((1 + var104) <> 0)))
              begin InterpProver.interpolate_check(10621)[8]
                begin InterpProver.interpolate_simplify(10622)[9]
                  begin InterpProver.interpolate_quick(10793)[10]
                    begin CsisatInterface.interpolate_csisat_wrap(10802)[11]
                      begin CsisatInterface.interpolate_csisat_post_process(10803)[12]
                        begin CsisatInterface.interpolate_csisat_log(10804)[13]
                          input1: (0 = v_sep_var106 & 1 = v_sep_var104 & 1 = v_sep_var107 & v_sep_var103 = v_sep_var105)
                          input2: ((not 1 = (v_sep_var106 + v_sep_var107) | not v_sep_var103 = v_sep_var105) & not 0 = v_sep_var104 & not 0 = (1 + v_sep_var104))
                          begin CsisatInterface.interpolate_csisat_raw(10805)[14]
                            
                          end CsisatInterface.interpolate_csisat_raw(10805)[14] (0.004000 sec.)
                          output: ((((v_sep_var106 + v_sep_var107) <= 1 & (-1*v_sep_var106 + -1*v_sep_var107) <= -1) | not v_sep_var103 = v_sep_var105) & v_sep_var103 = v_sep_var105)
                          
                        end CsisatInterface.interpolate_csisat_log(10804)[13] (0.004000 sec.)
                        after simplification: ((((v_sep_var106 + v_sep_var107) <= 1 & (-1*v_sep_var106 + -1*v_sep_var107) <= -1) | not v_sep_var103 = v_sep_var105) & v_sep_var103 = v_sep_var105)
                        after dnf conversion: (((v_sep_var106 + v_sep_var107) <= 1 & (-1*v_sep_var106 + -1*v_sep_var107) <= -1 & v_sep_var103 = v_sep_var105) | (not v_sep_var103 = v_sep_var105 & v_sep_var103 = v_sep_var105))
                        
                      end CsisatInterface.interpolate_csisat_post_process(10803)[12] (0.004000 sec.)
                      
                    end CsisatInterface.interpolate_csisat_wrap(10802)[11] (0.004000 sec.)
                    
                  end InterpProver.interpolate_quick(10793)[10] (0.008000 sec.)
                  begin minimizing # of conjunctions(10856)[10]
                    input: (((var106 + var107) = 1) && (var103 = var105))
                    output: ((var103 = var105) && ((var106 + var107) = 1))
                  end minimizing # of conjunctions(10856)[10] (0.000000 sec.)
                  
                end InterpProver.interpolate_simplify(10622)[9] (0.008000 sec.)
                
              end InterpProver.interpolate_check(10621)[8] (0.008000 sec.)
              output: ((var103 = var105) && ((var106 + var107) = 1))
            end InterpProver.interpolate_log(10620)[7] (0.008000 sec.)
            
          end InterpProver.interpolate_fresh(10619)[6] (0.008000 sec.)
          
        end InterpProver.interpolate(10618)[5] (0.008000 sec.)
        solution:
          P[<<fib_fib_1039@55:3>@63:2>](var102:int,var103:int,var104:int,var105:int,var106:int,var107:int) =
          ((var103 = var105) && ((var106 + var107) = 1))
        finding a solution to P[<fib_fib_1039@49:2>](var108:int,var109:int,var110:int)
        begin InterpProver.interpolate(11275)[5]
          begin InterpProver.interpolate_fresh(11276)[6]
            begin InterpProver.interpolate_log(11277)[7]
              input1: true
              input2: false
              begin InterpProver.interpolate_check(11278)[8]
                begin InterpProver.interpolate_simplify(11279)[9]
                  begin InterpProver.interpolate_quick(11292)[10]
                    
                  end InterpProver.interpolate_quick(11292)[10] (0.000000 sec.)
                  
                end InterpProver.interpolate_simplify(11279)[9] (0.000000 sec.)
                
              end InterpProver.interpolate_check(11278)[8] (0.000000 sec.)
              output: true
            end InterpProver.interpolate_log(11277)[7] (0.000000 sec.)
            
          end InterpProver.interpolate_fresh(11276)[6] (0.000000 sec.)
          
        end InterpProver.interpolate(11275)[5] (0.000000 sec.)
        solution:
          P[<fib_fib_1039@49:2>](var108:int,var109:int,var110:int) = true
        
      end BwHcSolver.solve_preds(10296)[4] (0.008000 sec.)
      begin HcSolver.check_validity(11440)[4]
        input:
          P[<fail_1866@82:0>](<fail_1866@82:0>:bool)|- bot
          
          P[<<fib_fib_1039@49:3>@71:2>](0:int,1:int,<br_k_main_1864@79:3>:int,<br_k_main_1864@79:4>:int,<br_k_main_1864@79:5>:int,<br_k_main_1864@79:6>:int),
          ((<br_k_main_1864@79:4> <> <br_k_main_1864@79:6>) && <fail_1866@82:0>)
          |- P[<fail_1866@82:0>](<fail_1866@82:0>:bool)
          
          P[<fib_fib_1039@49:2>](<fib_fib_1039@49:0>:int,<fib_fib_1039@49:1>:int,<fib_fib_1039@49:2>:int),
          P[<<fib_fib_1039@55:3>@63:2>](<fib_fib_1039@49:1>:int,(<fib_fib_1039@49:0> + <fib_fib_1039@49:1>):int,(
                                        -1 + <fib_fib_1039@49:2>):int,<<fib_fib_1039@49:3>@71:0>:int,(
                                        <<fib_fib_1039@49:3>@71:2> + (
                                        -1 * <<fib_fib_1039@49:3>@71:1>)):int,<<fib_fib_1039@49:3>@71:1>:int),
          ((<fib_fib_1039@49:2> <> 0) && (<fib_fib_1039@49:2> <> 1))
          |- P[<<fib_fib_1039@49:3>@71:2>](<fib_fib_1039@49:0>:int,<fib_fib_1039@49:1>:int,
                                           <fib_fib_1039@49:2>:int,<<fib_fib_1039@49:3>@71:0>:int,
                                           <<fib_fib_1039@49:3>@71:1>:int,
                                           <<fib_fib_1039@49:3>@71:2>:int)
          
          P[<fib_fib_1039@55:2>](<fib_fib_1039@55:0>:int,<fib_fib_1039@55:1>:int,<fib_fib_1039@55:2>:int),
          ((<fib_fib_1039@55:2> = 1) &&
           ((<<fib_fib_1039@55:3>@63:1> = 0) &&
            ((<<fib_fib_1039@55:3>@63:2> = 1) && (<<fib_fib_1039@55:3>@63:0> = <fib_fib_1039@55:1>))))
          |- P[<<fib_fib_1039@55:3>@63:2>](<fib_fib_1039@55:0>:int,<fib_fib_1039@55:1>:int,
                                           <fib_fib_1039@55:2>:int,<<fib_fib_1039@55:3>@63:0>:int,
                                           <<fib_fib_1039@55:3>@63:1>:int,
                                           <<fib_fib_1039@55:3>@63:2>:int)
          
          P[<fib_fib_1039@49:2>]((<fib_fib_1039@55:1> + (-1 * <fib_fib_1039@55:0>)):int,<fib_fib_1039@55:0>:int,(
                                 1 + <fib_fib_1039@55:2>):int),
          (((1 + <fib_fib_1039@55:2>) <> 0) && (<fib_fib_1039@55:2> <> 0))
          |- P[<fib_fib_1039@55:2>](<fib_fib_1039@55:0>:int,<fib_fib_1039@55:1>:int,
                                    <fib_fib_1039@55:2>:int)
          
          |- P[<fib_fib_1039@49:2>](<fib_fib_1039@49:0>:int,<fib_fib_1039@49:1>:int,
                                    <fib_fib_1039@49:2>:int)
        checking substituted Horn clause:
           false |- bot
        checking substituted Horn clause:
           false |- bot
        checking substituted Horn clause:
           false |- bot
        checking substituted Horn clause:
           false |- bot
        checking substituted Horn clause:
           false |- bot
        checking substituted Horn clause:
           false |- bot
        
      end HcSolver.check_validity(11440)[4] (0.004000 sec.)
      solution:
        P[<fail_1866@82:0>](var95:bool) = false
        P[<<fib_fib_1039@49:3>@71:2>](var96:int,var97:int,var98:int,var99:int,var100:int,var101:int) =
        ((var101 = 1) && (var99 = (var96 + var97)))
        P[<<fib_fib_1039@55:3>@63:2>](var102:int,var103:int,var104:int,var105:int,var106:int,var107:int) =
        ((var103 = var105) && ((var106 + var107) = 1))
        P[<fib_fib_1039@49:2>](var108:int,var109:int,var110:int) = true
        P[<fib_fib_1039@55:2>](var111:int,var112:int,var113:int) = true
    end BwHcSolver.solve(9131)[3] (0.024000 sec.)
    
  end RefTypeInfer.infer_etrs(4661)[2] (0.040000 sec.)
  refinement types:
    main_1846: X
    f_1852: v1:int -> X
    f_1853: v1:int -> v2:int -> X
    f_1854: v1:int -> v2:int -> v3:int -> X
    main_1043: v1:int -> v2:int -> v3:int -> (v4:unit -> X) -> X
    fib_fib_1039: v1:int ->
                  v2:int -> v3:int -> (v4:int -> v5:int -> v6:{v6:int | ((v6 = 1) && (v4 = (v1 + v2)))} -> X) -> X
    br_fib_fib_1862: v1:bool -> v2:int -> v3:int -> v4:int -> (v5:int -> v6:int -> v7:int -> X) -> X
    fib_fib_1039: v1:int ->
                  v2:int -> v3:int -> (v4:int -> v5:int -> v6:{v6:int | ((v2 = v4) && ((v5 + v6) = 1))} -> X) -> X
    br_fib_fib_1862: v1:bool -> v2:int -> v3:int -> v4:int -> (v5:int -> v6:int -> v7:int -> X) -> X
    f_fib_fib_1850: v1:int -> v2:int -> v3:int -> (v4:int -> v5:int -> v6:int -> X) -> v8:int -> v9:int -> v10:int -> X
    f_main_1851: v1:int -> v2:int -> v3:int -> (v4:unit -> X) -> v6:int -> v7:int -> v8:int -> X
    k_main_1589: v1:int -> v2:int -> v3:int -> v4:int -> v5:int -> v6:int -> (v7:unit -> X) -> v9:bool -> X
    br_k_main_1864: v1:bool ->
                    v2:int -> v3:int -> v4:int -> v5:int -> v6:int -> v7:int -> (v8:unit -> X) -> v10:bool -> X
    fail_1866: v1:{v1:bool | false} -> (v2:unit -> X) -> X
    br_fib''_1860: var132:bool -> var131:int -> (var130:int -> var129:int -> X) -> X
    br_fib'_1858: var139:bool -> var138:int -> var137:int -> var136:int -> (var135:int -> X) -> X
    br_fib_1856: var144:bool -> var143:int -> (var142:int -> X) -> X
    f_1855: var149:int -> var148:int -> var147:int -> var146:unit -> X
    f_fib''_1849: var156:int -> (var155:int -> var154:int -> X) -> var152:int -> var151:int -> X
    f_fib_1847: var161:int -> (var160:int -> X) -> var158:int -> X
    f_fib_1848: var167:int -> var166:int -> (var165:int -> X) -> var163:int -> X
    fib''_1036: var172:int -> (var171:int -> var170:int -> X) -> X
    fib'_1032: var178:int -> var177:int -> var176:int -> (var175:int -> X) -> X
    fib_1030: var182:int -> (var181:int -> X) -> X
  abstraction types:
    main_1846: X
    f_1852: v1:int -> X
    f_1853: v1:int -> v2:int -> X
    f_1854: v1:int -> v2:int -> v3:int -> X
    main_1043: v1:int -> v2:int -> v3:int -> (v4:unit -> X) -> X
    fib_fib_1039: var183:int ->
                  var184:int ->
                  var185:int ->
                  (var186:int ->
                   var187:int ->
                   var188:int[var188 -> ((var188 = 1) && (var186 = (var183 + var184))),
                              var188 -> ((var184 = var186) && ((var187 + var188) = 1))] -> X) -> X
    br_fib_fib_1862: var191:bool ->
                     var192:int -> var193:int -> var194:int -> (var195:int -> var196:int -> var197:int -> X) -> X
    f_fib_fib_1850: v1:int -> v2:int -> v3:int -> (v4:int -> v5:int -> v6:int -> X) -> v8:int -> v9:int -> v10:int -> X
    f_main_1851: v1:int -> v2:int -> v3:int -> (v4:unit -> X) -> v6:int -> v7:int -> v8:int -> X
    k_main_1589: v1:int -> v2:int -> v3:int -> v4:int -> v5:int -> v6:int -> (v7:unit -> X) -> v9:bool -> X
    br_k_main_1864: v1:bool ->
                    v2:int -> v3:int -> v4:int -> v5:int -> v6:int -> v7:int -> (v8:unit -> X) -> v10:bool -> X
    fail_1866: v1:bool -> (v2:unit -> X) -> X
    br_fib''_1860: var132:bool -> var131:int -> (var130:int -> var129:int -> X) -> X
    br_fib'_1858: var139:bool -> var138:int -> var137:int -> var136:int -> (var135:int -> X) -> X
    br_fib_1856: var144:bool -> var143:int -> (var142:int -> X) -> X
    f_1855: var149:int -> var148:int -> var147:int -> var146:unit -> X
    f_fib''_1849: var156:int -> (var155:int -> var154:int -> X) -> var152:int -> var151:int -> X
    f_fib_1847: var161:int -> (var160:int -> X) -> var158:int -> X
    f_fib_1848: var167:int -> var166:int -> (var165:int -> X) -> var163:int -> X
    fib''_1036: var172:int -> (var171:int -> var170:int -> X) -> X
    fib'_1032: var178:int -> var177:int -> var176:int -> (var175:int -> X) -> X
    fib_1030: var182:int -> (var181:int -> X) -> X
  
end AbsTypeInfer.refine(4660)[1] (0.040000 sec.)
DONE!

Prefix of spurious counter-example::
0; 0; 0; 0; 0; 1; 1; 1; 0; 0; 0; 0; 1

Program with abstraction types (CEGAR-cycle 2)::
Main: main_1846
  main_1846 -> (rand_int f_1852)
  br_fib''_1860 b_1861 n_1037 k_fib''_1427 when b_1861 -> (k_fib''_1427 0 1)
  br_fib''_1860 b_1861 n_1037 k_fib''_1427 when (not b_1861) ->
      (fib''_1036 (n_1037 - 1) (f_fib''_1849 n_1037 k_fib''_1427))
  br_fib'_1858 b_1859 a_1033 b_1034 n_1035 k_fib'_1393 when b_1859 -> (k_fib'_1393 b_1034)
  br_fib'_1858 b_1859 a_1033 b_1034 n_1035 k_fib'_1393 when (not b_1859) ->
      (fib'_1032 b_1034 (a_1033 + b_1034) (n_1035 - 1) k_fib'_1393)
  br_fib_1856 b_1857 n_1031 k_fib_1341 when b_1857 -> (k_fib_1341 1)
  br_fib_1856 b_1857 n_1031 k_fib_1341 when (not b_1857) -> (fib_1030 (n_1031 - 1) (f_fib_1847 n_1031 k_fib_1341))
  br_fib_fib_1862 b_1863 a_1040 b_1041 n_1042 k_fib_fib_1483 when b_1863 -> (k_fib_fib_1483 b_1041 0 1)
  br_fib_fib_1862 b_1863 a_1040 b_1041 n_1042 k_fib_fib_1483 when (not b_1863) ->
      (fib_fib_1039 b_1041 (a_1040 + b_1041) (n_1042 - 1) (f_fib_fib_1850 a_1040 b_1041 n_1042 k_fib_fib_1483))
  br_k_main_1864 b_1865 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 when b_1865 ->
      (k_main_1571 ())
  br_k_main_1864 b_1865 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 when (
      not b_1865) -> (fail_1866 true k_main_1571)
  f_1852 x_1611 -> (rand_int (f_1853 x_1611))
  f_1853 x_1611 x_1624 -> (rand_int (f_1854 x_1611 x_1624))
  f_1854 x_1611 x_1624 x_1637 -> (main_1043 x_1611 x_1624 x_1637 (f_1855 x_1611 x_1624 x_1637))
  f_1855 x_1611 x_1624 x_1637 x_1608 -> end
  f_fib''_1849 n_1037 k_fib''_1427 x1_1440 x2_1440 -> (k_fib''_1427 x2_1440 (x1_1440 + x2_1440))
  f_fib_1847 n_1031 k_fib_1341 x_1344 -> (fib_1030 (n_1031 - 2) (f_fib_1848 n_1031 x_1344 k_fib_1341))
  f_fib_1848 n_1031 x_1344 k_fib_1341 x_1345 -> (k_fib_1341 (x_1344 + x_1345))
  f_fib_fib_1850 a_1040 b_1041 n_1042 k_fib_fib_1483 x1_1518 x21_1518 x22_1518 ->
      (k_fib_fib_1483 x1_1518 x22_1518 (x21_1518 + x22_1518))
  f_main_1851 a_1044 b_1045 n_1046 k_main_1571 x1_1574 x21_1574 x22_1574 when (
      a_1044 = 0) -> (k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 (b_1045 = 1))
  f_main_1851 a_1044 b_1045 n_1046 k_main_1571 x1_1574 x21_1574 x22_1574 when (
      not (a_1044 = 0)) -> (k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 false)
  fail_1866 b k -> {fail} => (k ())
  fib''_1036 n_1037 k_fib''_1427 when (n_1037 = 0) -> (k_fib''_1427 0 0)
  fib''_1036 n_1037 k_fib''_1427 when (not (n_1037 = 0)) -> (br_fib''_1860 (n_1037 = 1) n_1037 k_fib''_1427)
  fib'_1032 a_1033 b_1034 n_1035 k_fib'_1393 when (n_1035 = 0) -> (k_fib'_1393 a_1033)
  fib'_1032 a_1033 b_1034 n_1035 k_fib'_1393 when (not (n_1035 = 0)) ->
      (br_fib'_1858 (n_1035 = 1) a_1033 b_1034 n_1035 k_fib'_1393)
  fib_1030 n_1031 k_fib_1341 when (n_1031 = 0) -> (k_fib_1341 0)
  fib_1030 n_1031 k_fib_1341 when (not (n_1031 = 0)) -> (br_fib_1856 (n_1031 = 1) n_1031 k_fib_1341)
  fib_fib_1039 a_1040 b_1041 n_1042 k_fib_fib_1483 when (n_1042 = 0) -> (k_fib_fib_1483 a_1040 0 0)
  fib_fib_1039 a_1040 b_1041 n_1042 k_fib_fib_1483 when (not (n_1042 = 0)) ->
      (br_fib_fib_1862 (n_1042 = 1) a_1040 b_1041 n_1042 k_fib_fib_1483)
  k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 when b_1605 ->
      (br_k_main_1864 (x1_1574 = x22_1574) a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605)
  k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 when (not b_1605) -> (k_main_1571 ())
  main_1043 a_1044 b_1045 n_1046 k_main_1571 ->
      (fib_fib_1039 a_1044 b_1045 n_1046 (f_main_1851 a_1044 b_1045 n_1046 k_main_1571))
Types:
  main_1846 : X
  fail_1866 : (x_1:bool[x_1] -> (unit -> X) -> X)
  fib_1030 : (int -> (int -> X) -> X)
  fib_fib_1039 : (x_1:int ->
                  x_2:int ->
                  int ->
                  (x_5:int ->
                   x_6:int ->
                   x_7:int[x_2 = x_5 && x_6 = -x_7 + 1; x_7 = 1 && x_1 = -x_2 + x_5; x_7 = 1 && x_2 = x_5] -> X) -> X)
  
(2-1) Abstracting ... DONE!

(2-2) Checking HORS ... DONE!

Error trace::
  main_1846 ... --> 
  f_1852 ... --> 
  f_1853 ... --> 
  f_1854 ... --> 
  main_1043 ... --> 
  fib_fib_1039 [2/2] ... --> 
  br_fib_fib_1862 [2/2] ... --> 
  fib_fib_1039 [1/2] ... --> 
  f_fib_fib_1850 ... --> 
  f_main_1851 [1/2] ... --> 
  k_main_1589 [1/2] ... --> 
  br_k_main_1864 [2/2] ... --> 
  fail_1866 ... --> fail -->
  ERROR!

Spurious counter-example::
  0; 0; 0; 0; 0; 1; 1; 0; 0; 0; 0; 1; 0

(2-3) Checking counter-example ... DONE!

(2-4) Discovering predicates ... 
begin AbsTypeInfer.refine(13956)[1]
  program:
    main_1846  | true = ((Random.int 0) f_1852)
    br_fib''_1860 b_1861 n_1037 k_fib''_1427 | b_1861 = (k_fib''_1427 0
                                                                    1)
    br_fib''_1860 b_1861 n_1037 k_fib''_1427 | (not b_1861) = (fib''_1036
                                                                 (n_1037 - 1)
                                                                 (f_fib''_1849 n_1037
                                                                    k_fib''_1427))
    br_fib'_1858 b_1859 a_1033 b_1034 n_1035 k_fib'_1393 | b_1859 = (
    k_fib'_1393 b_1034)
    br_fib'_1858 b_1859 a_1033 b_1034 n_1035 k_fib'_1393 | (not b_1859) = (
    fib'_1032 b_1034
              (a_1033 + b_1034)
              (n_1035 - 1)
              k_fib'_1393)
    br_fib_1856 b_1857 n_1031 k_fib_1341 | b_1857 = (k_fib_1341 1)
    br_fib_1856 b_1857 n_1031 k_fib_1341 | (not b_1857) = (fib_1030 (
                                                                    n_1031 - 1)
                                                                    (
                                                                    f_fib_1847 n_1031
                                                                    k_fib_1341))
    br_fib_fib_1862 b_1863 a_1040 b_1041 n_1042 k_fib_fib_1483 | b_1863 = (
    k_fib_fib_1483 b_1041
                   0
                   1)
    br_fib_fib_1862 b_1863 a_1040 b_1041 n_1042 k_fib_fib_1483 | (not b_1863) = (
    fib_fib_1039 b_1041
                 (a_1040 + b_1041)
                 (n_1042 - 1)
                 (f_fib_fib_1850 a_1040
                                 b_1041
                                 n_1042
                                 k_fib_fib_1483))
    br_k_main_1864 b_1865 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 | b_1865 = (
    k_main_1571 ())
    br_k_main_1864 b_1865 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 | (
    not b_1865) = (fail_1866 true
                             k_main_1571)
    f_1852 x_1611 | true = ((Random.int 0) (f_1853 x_1611))
    f_1853 x_1611 x_1624 | true = ((Random.int 0) (f_1854 x_1611
                                                          x_1624))
    f_1854 x_1611 x_1624 x_1637 | true = (main_1043 x_1611
                                                    x_1624
                                                    x_1637
                                                    (f_1855 x_1611
                                                            x_1624
                                                            x_1637))
    f_1855 x_1611 x_1624 x_1637 x_1608 | true = end
    f_fib''_1849 n_1037 k_fib''_1427 x1_1440 x2_1440 | true = (k_fib''_1427 x2_1440
                                                                    (x1_1440 + x2_1440))
    f_fib_1847 n_1031 k_fib_1341 x_1344 | true = (fib_1030 (n_1031 - 2)
                                                           (f_fib_1848 n_1031
                                                                    x_1344
                                                                    k_fib_1341))
    f_fib_1848 n_1031 x_1344 k_fib_1341 x_1345 | true = (k_fib_1341 (
                                                                    x_1344 + x_1345))
    f_fib_fib_1850 a_1040 b_1041 n_1042 k_fib_fib_1483 x1_1518 x21_1518 x22_1518 | true = (
    k_fib_fib_1483 x1_1518
                   x22_1518
                   (x21_1518 + x22_1518))
    f_main_1851 a_1044 b_1045 n_1046 k_main_1571 x1_1574 x21_1574 x22_1574 | (
    a_1044 = 0) = (k_main_1589 a_1044
                               b_1045
                               n_1046
                               x1_1574
                               x21_1574
                               x22_1574
                               k_main_1571
                               (b_1045 = 1))
    f_main_1851 a_1044 b_1045 n_1046 k_main_1571 x1_1574 x21_1574 x22_1574 | (
    not (a_1044 = 0)) = (k_main_1589 a_1044
                                     b_1045
                                     n_1046
                                     x1_1574
                                     x21_1574
                                     x22_1574
                                     k_main_1571
                                     false)
    fail_1866 b k | true = (fail ())
    fib''_1036 n_1037 k_fib''_1427 | (n_1037 = 0) = (k_fib''_1427 0
                                                                  0)
    fib''_1036 n_1037 k_fib''_1427 | (not (n_1037 = 0)) = (br_fib''_1860 (
                                                                    n_1037 = 1)
                                                                    n_1037
                                                                    k_fib''_1427)
    fib'_1032 a_1033 b_1034 n_1035 k_fib'_1393 | (n_1035 = 0) = (k_fib'_1393 a_1033)
    fib'_1032 a_1033 b_1034 n_1035 k_fib'_1393 | (not (n_1035 = 0)) = (
    br_fib'_1858 (n_1035 = 1)
                 a_1033
                 b_1034
                 n_1035
                 k_fib'_1393)
    fib_1030 n_1031 k_fib_1341 | (n_1031 = 0) = (k_fib_1341 0)
    fib_1030 n_1031 k_fib_1341 | (not (n_1031 = 0)) = (br_fib_1856 (n_1031 = 1)
                                                                   n_1031
                                                                   k_fib_1341)
    fib_fib_1039 a_1040 b_1041 n_1042 k_fib_fib_1483 | (n_1042 = 0) = (
    k_fib_fib_1483 a_1040
                   0
                   0)
    fib_fib_1039 a_1040 b_1041 n_1042 k_fib_fib_1483 | (not (n_1042 = 0)) = (
    br_fib_fib_1862 (n_1042 = 1)
                    a_1040
                    b_1041
                    n_1042
                    k_fib_fib_1483)
    k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 | b_1605 = (
    br_k_main_1864 (x1_1574 = x22_1574)
                   a_1044
                   b_1045
                   n_1046
                   x1_1574
                   x21_1574
                   x22_1574
                   k_main_1571
                   b_1605)
    k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 | (
    not b_1605) = (k_main_1571 ())
    main_1043 a_1044 b_1045 n_1046 k_main_1571 | true = (fib_fib_1039
                                                           a_1044
                                                           b_1045
                                                           n_1046
                                                           (f_main_1851 a_1044
                                                                    b_1045
                                                                    n_1046
                                                                    k_main_1571))
    main_1846:X
    br_fib''_1860:bool -> int -> (int -> int -> X) -> X
    br_fib'_1858:bool -> int -> int -> int -> (int -> X) -> X
    br_fib_1856:bool -> int -> (int -> X) -> X
    br_fib_fib_1862:bool -> int -> int -> int -> (int -> int -> int -> X) -> X
    br_k_main_1864:bool -> int -> int -> int -> int -> int -> int -> (unit -> X) -> bool -> X
    f_1852:int -> X
    f_1853:int -> int -> X
    f_1854:int -> int -> int -> X
    f_1855:int -> int -> int -> unit -> X
    f_fib''_1849:int -> (int -> int -> X) -> int -> int -> X
    f_fib_1847:int -> (int -> X) -> int -> X
    f_fib_1848:int -> int -> (int -> X) -> int -> X
    f_fib_fib_1850:int -> int -> int -> (int -> int -> int -> X) -> int -> int -> int -> X
    f_main_1851:int -> int -> int -> (unit -> X) -> int -> int -> int -> X
    fail_1866:bool -> (unit -> X) -> X
    fib''_1036:int -> (int -> int -> X) -> X
    fib'_1032:int -> int -> int -> (int -> X) -> X
    fib_1030:int -> (int -> X) -> X
    fib_fib_1039:int -> int -> int -> (int -> int -> int -> X) -> X
    k_main_1589:int -> int -> int -> int -> int -> int -> (unit -> X) -> bool -> X
    main_1043:int -> int -> int -> (unit -> X) -> X
  inlined functions: br_fib''_1860,br_fib'_1858,br_fib_1856,br_fib_fib_1862,br_k_main_1864,f_1852,f_1853,f_1854,f_1855,f_fib''_1849,f_fib_1847,f_fib_1848,f_fib_fib_1850,f_main_1851,fib''_1036,fib'_1032,k_main_1589,main_1043
  counterexample: 0:0:0:0:0:1:1:0:0:0:0:1:0
  error traces:
    [true.nop(<f_1852@88:0> = var200)
     [true.nop((<f_1853@91:0> = <f_1852@88:0>) && (<f_1853@91:1> = var201))
      [true.nop((<f_1854@94:0> = <f_1853@91:0>) && ((<f_1854@94:1> = <f_1853@91:1>) && (<f_1854@94:2> = var202)))
       [true.
        ((<main_1043@96:0> = <f_1854@94:0>) &&
         ((<main_1043@96:1> = <f_1854@94:1>) && (<main_1043@96:2> = <f_1854@94:2>)))
        [true.
         ((<fib_fib_1039@98:0> = <main_1043@96:0>) &&
          ((<fib_fib_1039@98:1> = <main_1043@96:1>) && (<fib_fib_1039@98:2> = <main_1043@96:2>)))
         [(not (<fib_fib_1039@98:2> = 0)).
          ((<br_fib_fib_1862@101:0> = (<fib_fib_1039@98:2> = 1)) &&
           ((<br_fib_fib_1862@101:1> = <fib_fib_1039@98:0>) &&
            ((<br_fib_fib_1862@101:2> = <fib_fib_1039@98:1>) && (<br_fib_fib_1862@101:3> = <fib_fib_1039@98:2>))))
          [(not <br_fib_fib_1862@101:0>).
           ((<fib_fib_1039@104:0> = <br_fib_fib_1862@101:2>) &&
            ((<fib_fib_1039@104:1> = (<br_fib_fib_1862@101:1> + <br_fib_fib_1862@101:2>)) &&
             (<fib_fib_1039@104:2> = (<br_fib_fib_1862@101:3> - 1))))
           [(<fib_fib_1039@104:2> = 0).
            ((<<fib_fib_1039@104:3>@107:0> = <fib_fib_1039@104:0>) &&
             ((<<fib_fib_1039@104:3>@107:1> = 0) && (<<fib_fib_1039@104:3>@107:2> = 0)))
            [true.
             ((<f_fib_fib_1850@109:0> = <br_fib_fib_1862@101:1>) &&
              ((<f_fib_fib_1850@109:1> = <br_fib_fib_1862@101:2>) &&
               ((<f_fib_fib_1850@109:2> = <br_fib_fib_1862@101:3>) &&
                ((<f_fib_fib_1850@109:4> = <<fib_fib_1039@104:3>@107:0>) &&
                 ((<f_fib_fib_1850@109:5> = <<fib_fib_1039@104:3>@107:1>) &&
                  (<f_fib_fib_1850@109:6> = <<fib_fib_1039@104:3>@107:2>))))))
             [true.
              ((<<f_fib_fib_1850@109:3>@111:0> = <f_fib_fib_1850@109:4>) &&
               ((<<f_fib_fib_1850@109:3>@111:1> = <f_fib_fib_1850@109:6>) &&
                (<<f_fib_fib_1850@109:3>@111:2> = (<f_fib_fib_1850@109:5> + <f_fib_fib_1850@109:6>))))
              [true.
               ((<<br_fib_fib_1862@101:4>@113:0> = <<f_fib_fib_1850@109:3>@111:0>) &&
                ((<<br_fib_fib_1862@101:4>@113:1> = <<f_fib_fib_1850@109:3>@111:1>) &&
                 (<<br_fib_fib_1862@101:4>@113:2> = <<f_fib_fib_1850@109:3>@111:2>)))
               [true.
                ((<<fib_fib_1039@98:3>@115:0> = <<br_fib_fib_1862@101:4>@113:0>) &&
                 ((<<fib_fib_1039@98:3>@115:1> = <<br_fib_fib_1862@101:4>@113:1>) &&
                  (<<fib_fib_1039@98:3>@115:2> = <<br_fib_fib_1862@101:4>@113:2>)))
                [true.
                 ((<f_main_1851@117:0> = <main_1043@96:0>) &&
                  ((<f_main_1851@117:1> = <main_1043@96:1>) &&
                   ((<f_main_1851@117:2> = <main_1043@96:2>) &&
                    ((<f_main_1851@117:4> = <<fib_fib_1039@98:3>@115:0>) &&
                     ((<f_main_1851@117:5> = <<fib_fib_1039@98:3>@115:1>) &&
                      (<f_main_1851@117:6> = <<fib_fib_1039@98:3>@115:2>))))))
                 [(<f_main_1851@117:0> = 0).
                  ((<k_main_1589@120:0> = <f_main_1851@117:0>) &&
                   ((<k_main_1589@120:1> = <f_main_1851@117:1>) &&
                    ((<k_main_1589@120:2> = <f_main_1851@117:2>) &&
                     ((<k_main_1589@120:3> = <f_main_1851@117:4>) &&
                      ((<k_main_1589@120:4> = <f_main_1851@117:5>) &&
                       ((<k_main_1589@120:5> = <f_main_1851@117:6>) &&
                        (<k_main_1589@120:7> = (<f_main_1851@117:1> = 1))))))))
                  [<k_main_1589@120:7>.
                   ((<br_k_main_1864@123:0> = (<k_main_1589@120:3> = <k_main_1589@120:5>)) &&
                    ((<br_k_main_1864@123:1> = <k_main_1589@120:0>) &&
                     ((<br_k_main_1864@123:2> = <k_main_1589@120:1>) &&
                      ((<br_k_main_1864@123:3> = <k_main_1589@120:2>) &&
                       ((<br_k_main_1864@123:4> = <k_main_1589@120:3>) &&
                        ((<br_k_main_1864@123:5> = <k_main_1589@120:4>) &&
                         ((<br_k_main_1864@123:6> = <k_main_1589@120:5>) &&
                          (<br_k_main_1864@123:8> = <k_main_1589@120:7>))))))))
                   [(not <br_k_main_1864@123:0>).(<fail_1866@126:0> = true)[true.error
  begin RefTypeInfer.infer_etrs(13957)[2]
    horn clauses:
      P[<fail_1866@126:0>](<fail_1866@126:0>:bool)|- bot
      
      P[<br_k_main_1864@123:8>](false:bool,<br_k_main_1864@123:1>:int,<br_k_main_1864@123:2>:int,<br_k_main_1864@123:3>:int,<br_k_main_1864@123:4>:int,<br_k_main_1864@123:5>:int,<br_k_main_1864@123:6>:int,<br_k_main_1864@123:8>:bool),
      <fail_1866@126:0> |- P[<fail_1866@126:0>](<fail_1866@126:0>:bool)
      
      P[<k_main_1589@120:7>](<br_k_main_1864@123:1>:int,<br_k_main_1864@123:2>:int,<br_k_main_1864@123:3>:int,<br_k_main_1864@123:4>:int,<br_k_main_1864@123:5>:int,<br_k_main_1864@123:6>:int,<br_k_main_1864@123:8>:bool),
      (<br_k_main_1864@123:8> && (<br_k_main_1864@123:0> = (<br_k_main_1864@123:4> = <br_k_main_1864@123:6>)))
      |- P[<br_k_main_1864@123:8>](<br_k_main_1864@123:0>:bool,<br_k_main_1864@123:1>:int,
                                   <br_k_main_1864@123:2>:int,<br_k_main_1864@123:3>:int,
                                   <br_k_main_1864@123:4>:int,<br_k_main_1864@123:5>:int,
                                   <br_k_main_1864@123:6>:int,<br_k_main_1864@123:8>:bool)
      
      P[<f_main_1851@117:6>](<k_main_1589@120:0>:int,<k_main_1589@120:1>:int,<k_main_1589@120:2>:int,<k_main_1589@120:3>:int,<k_main_1589@120:4>:int,<k_main_1589@120:5>:int),
      ((<k_main_1589@120:0> = 0) && (<k_main_1589@120:7> = (<k_main_1589@120:1> = 1)))
      |- P[<k_main_1589@120:7>](<k_main_1589@120:0>:int,<k_main_1589@120:1>:int,
                                <k_main_1589@120:2>:int,<k_main_1589@120:3>:int,
                                <k_main_1589@120:4>:int,<k_main_1589@120:5>:int,
                                <k_main_1589@120:7>:bool)
      
      P[<main_1043@96:2>](<f_main_1851@117:0>:int,<f_main_1851@117:1>:int,<f_main_1851@117:2>:int),
      P[<<fib_fib_1039@98:3>@115:2>](<f_main_1851@117:0>:int,<f_main_1851@117:1>:int,<f_main_1851@117:2>:int,<f_main_1851@117:4>:int,<f_main_1851@117:5>:int,<f_main_1851@117:6>:int)|- P[<f_main_1851@117:6>](
      <f_main_1851@117:0>:int,<f_main_1851@117:1>:int,<f_main_1851@117:2>:int,
      <f_main_1851@117:4>:int,<f_main_1851@117:5>:int,<f_main_1851@117:6>:int)
      
      P[<fib_fib_1039@98:2>](<fib_fib_1039@98:0>:int,<fib_fib_1039@98:1>:int,<fib_fib_1039@98:2>:int),
      P[<<br_fib_fib_1862@101:4>@113:2>]((<fib_fib_1039@98:2> = 1):bool,<fib_fib_1039@98:0>:int,<fib_fib_1039@98:1>:int,<fib_fib_1039@98:2>:int,<<fib_fib_1039@98:3>@115:0>:int,<<fib_fib_1039@98:3>@115:1>:int,<<fib_fib_1039@98:3>@115:2>:int),
      (<fib_fib_1039@98:2> <> 0)
      |- P[<<fib_fib_1039@98:3>@115:2>](<fib_fib_1039@98:0>:int,<fib_fib_1039@98:1>:int,
                                        <fib_fib_1039@98:2>:int,<<fib_fib_1039@98:3>@115:0>:int,
                                        <<fib_fib_1039@98:3>@115:1>:int,
                                        <<fib_fib_1039@98:3>@115:2>:int)
      
      P[<br_fib_fib_1862@101:3>](<br_fib_fib_1862@101:0>:bool,<br_fib_fib_1862@101:1>:int,<br_fib_fib_1862@101:2>:int,<br_fib_fib_1862@101:3>:int),
      P[<<f_fib_fib_1850@109:3>@111:2>](<br_fib_fib_1862@101:1>:int,<br_fib_fib_1862@101:2>:int,<br_fib_fib_1862@101:3>:int,<<br_fib_fib_1862@101:4>@113:0>:int,<<br_fib_fib_1862@101:4>@113:1>:int,<<br_fib_fib_1862@101:4>@113:2>:int),
      (not <br_fib_fib_1862@101:0>)
      |- P[<<br_fib_fib_1862@101:4>@113:2>](<br_fib_fib_1862@101:0>:bool,
                                            <br_fib_fib_1862@101:1>:int,
                                            <br_fib_fib_1862@101:2>:int,
                                            <br_fib_fib_1862@101:3>:int,
                                            <<br_fib_fib_1862@101:4>@113:0>:int,
                                            <<br_fib_fib_1862@101:4>@113:1>:int,
                                            <<br_fib_fib_1862@101:4>@113:2>:int)
      
      P[<f_fib_fib_1850@109:6>](<f_fib_fib_1850@109:0>:int,<f_fib_fib_1850@109:1>:int,<f_fib_fib_1850@109:2>:int,<<f_fib_fib_1850@109:3>@111:0>:int,(
                                <<f_fib_fib_1850@109:3>@111:2> + (-1 * <<f_fib_fib_1850@109:3>@111:1>)):int,<<f_fib_fib_1850@109:3>@111:1>:int)|- P[<<f_fib_fib_1850@109:3>@111:2>](
      <f_fib_fib_1850@109:0>:int,<f_fib_fib_1850@109:1>:int,<f_fib_fib_1850@109:2>:int,
      <<f_fib_fib_1850@109:3>@111:0>:int,<<f_fib_fib_1850@109:3>@111:1>:int,
      <<f_fib_fib_1850@109:3>@111:2>:int)
      
      P[<br_fib_fib_1862@101:3>](false:bool,<f_fib_fib_1850@109:0>:int,<f_fib_fib_1850@109:1>:int,<f_fib_fib_1850@109:2>:int),
      P[<<fib_fib_1039@104:3>@107:2>](<f_fib_fib_1850@109:1>:int,(<f_fib_fib_1850@109:0> + <f_fib_fib_1850@109:1>):int,(
                                      -1 + <f_fib_fib_1850@109:2>):int,<f_fib_fib_1850@109:4>:int,<f_fib_fib_1850@109:5>:int,<f_fib_fib_1850@109:6>:int)|- P[<f_fib_fib_1850@109:6>](
      <f_fib_fib_1850@109:0>:int,<f_fib_fib_1850@109:1>:int,<f_fib_fib_1850@109:2>:int,
      <f_fib_fib_1850@109:4>:int,<f_fib_fib_1850@109:5>:int,<f_fib_fib_1850@109:6>:int)
      
      P[<fib_fib_1039@104:2>](<fib_fib_1039@104:0>:int,<fib_fib_1039@104:1>:int,<fib_fib_1039@104:2>:int),
      ((<fib_fib_1039@104:2> = 0) &&
       ((<<fib_fib_1039@104:3>@107:1> = 0) &&
        ((<<fib_fib_1039@104:3>@107:2> = 0) && (<<fib_fib_1039@104:3>@107:0> = <fib_fib_1039@104:0>))))
      |- P[<<fib_fib_1039@104:3>@107:2>](<fib_fib_1039@104:0>:int,<fib_fib_1039@104:1>:int,
                                         <fib_fib_1039@104:2>:int,<<fib_fib_1039@104:3>@107:0>:int,
                                         <<fib_fib_1039@104:3>@107:1>:int,
                                         <<fib_fib_1039@104:3>@107:2>:int)
      
      P[<br_fib_fib_1862@101:3>](false:bool,(<fib_fib_1039@104:1> + (-1 * <fib_fib_1039@104:0>)):int,<fib_fib_1039@104:0>:int,(
                                 1 + <fib_fib_1039@104:2>):int)|- P[<fib_fib_1039@104:2>](
      <fib_fib_1039@104:0>:int,<fib_fib_1039@104:1>:int,<fib_fib_1039@104:2>:int)
      
      P[<fib_fib_1039@98:2>](<br_fib_fib_1862@101:1>:int,<br_fib_fib_1862@101:2>:int,<br_fib_fib_1862@101:3>:int),
      ((<br_fib_fib_1862@101:3> <> 0) && (<br_fib_fib_1862@101:0> = (<br_fib_fib_1862@101:3> = 1)))
      |- P[<br_fib_fib_1862@101:3>](<br_fib_fib_1862@101:0>:bool,<br_fib_fib_1862@101:1>:int,
                                    <br_fib_fib_1862@101:2>:int,<br_fib_fib_1862@101:3>:int)
      
      P[<main_1043@96:2>](<fib_fib_1039@98:0>:int,<fib_fib_1039@98:1>:int,<fib_fib_1039@98:2>:int)|- P[<fib_fib_1039@98:2>](
      <fib_fib_1039@98:0>:int,<fib_fib_1039@98:1>:int,<fib_fib_1039@98:2>:int)
      
      P[<f_1854@94:2>](<main_1043@96:0>:int,<main_1043@96:1>:int,<main_1043@96:2>:int)|- P[<main_1043@96:2>](
      <main_1043@96:0>:int,<main_1043@96:1>:int,<main_1043@96:2>:int)
      
      P[<f_1853@91:1>](<f_1854@94:0>:int,<f_1854@94:1>:int)|- P[<f_1854@94:2>](
      <f_1854@94:0>:int,<f_1854@94:1>:int,<f_1854@94:2>:int)
      
      P[<f_1852@88:0>](<f_1853@91:0>:int)|- P[<f_1853@91:1>](<f_1853@91:0>:int,
                                                             <f_1853@91:1>:int)
      
      |- P[<f_1852@88:0>](<f_1852@88:0>:int)
    call trees:
      <main_1846@85>
        <f_1852@88>
          <f_1853@91>
            <f_1854@94>
              <main_1043@96>
                <fib_fib_1039@98>
                  <br_fib_fib_1862@101>
                    <fib_fib_1039@104>
                    </<fib_fib_1039@104:3>@107>
                    <f_fib_fib_1850@109>
                    </<f_fib_fib_1850@109:3>@111>
                  </<br_fib_fib_1862@101:4>@113>
                </<fib_fib_1039@98:3>@115>
                <f_main_1851@117>
                  <k_main_1589@120>
                    <br_k_main_1864@123>
                      <fail_1866@126>
    inlined horn clauses:
      P[<fail_1866@126:0>](<fail_1866@126:0>:bool)|- bot
      
      P[<<fib_fib_1039@98:3>@115:2>](0:int,1:int,<br_k_main_1864@123:3>:int,<br_k_main_1864@123:4>:int,<br_k_main_1864@123:5>:int,<br_k_main_1864@123:6>:int),
      ((<br_k_main_1864@123:4> <> <br_k_main_1864@123:6>) && <fail_1866@126:0>)
      |- P[<fail_1866@126:0>](<fail_1866@126:0>:bool)
      
      P[<fib_fib_1039@98:2>](<fib_fib_1039@98:0>:int,<fib_fib_1039@98:1>:int,<fib_fib_1039@98:2>:int),
      P[<<fib_fib_1039@104:3>@107:2>](<fib_fib_1039@98:1>:int,(<fib_fib_1039@98:0> + <fib_fib_1039@98:1>):int,(
                                      -1 + <fib_fib_1039@98:2>):int,<<fib_fib_1039@98:3>@115:0>:int,(
                                      <<fib_fib_1039@98:3>@115:2> + (
                                      -1 * <<fib_fib_1039@98:3>@115:1>)):int,<<fib_fib_1039@98:3>@115:1>:int),
      ((<fib_fib_1039@98:2> <> 0) && (<fib_fib_1039@98:2> <> 1))
      |- P[<<fib_fib_1039@98:3>@115:2>](<fib_fib_1039@98:0>:int,<fib_fib_1039@98:1>:int,
                                        <fib_fib_1039@98:2>:int,<<fib_fib_1039@98:3>@115:0>:int,
                                        <<fib_fib_1039@98:3>@115:1>:int,
                                        <<fib_fib_1039@98:3>@115:2>:int)
      
      P[<fib_fib_1039@104:2>](<fib_fib_1039@104:0>:int,<fib_fib_1039@104:1>:int,<fib_fib_1039@104:2>:int),
      ((<fib_fib_1039@104:2> = 0) &&
       ((<<fib_fib_1039@104:3>@107:1> = 0) &&
        ((<<fib_fib_1039@104:3>@107:2> = 0) && (<<fib_fib_1039@104:3>@107:0> = <fib_fib_1039@104:0>))))
      |- P[<<fib_fib_1039@104:3>@107:2>](<fib_fib_1039@104:0>:int,<fib_fib_1039@104:1>:int,
                                         <fib_fib_1039@104:2>:int,<<fib_fib_1039@104:3>@107:0>:int,
                                         <<fib_fib_1039@104:3>@107:1>:int,
                                         <<fib_fib_1039@104:3>@107:2>:int)
      
      P[<fib_fib_1039@98:2>]((<fib_fib_1039@104:1> + (-1 * <fib_fib_1039@104:0>)):int,<fib_fib_1039@104:0>:int,(
                             1 + <fib_fib_1039@104:2>):int),
      (((1 + <fib_fib_1039@104:2>) <> 0) && (<fib_fib_1039@104:2> <> 0))
      |- P[<fib_fib_1039@104:2>](<fib_fib_1039@104:0>:int,<fib_fib_1039@104:1>:int,
                                 <fib_fib_1039@104:2>:int)
      
      |- P[<fib_fib_1039@98:2>](<fib_fib_1039@98:0>:int,<fib_fib_1039@98:1>:int,
                                <fib_fib_1039@98:2>:int)
    begin ParamSubstInfer.infer(17252)[3]
      
    end ParamSubstInfer.infer(17252)[3] (0.108000 sec.)
    inferred extra parameters:
      
    begin RefTypeInfer.elim_coeffs(17254)[3]
      
    end RefTypeInfer.elim_coeffs(17254)[3] (0.004000 sec.)
    begin BwHcSolver.solve(17627)[3]
      lower bounds:
        P[<fib_fib_1039@98:2>](<fib_fib_1039@98:0>:int,<fib_fib_1039@98:1>:int,<fib_fib_1039@98:2>:int) = true
        P[<fib_fib_1039@104:2>](<fib_fib_1039@104:0>:int,<fib_fib_1039@104:1>:int,<fib_fib_1039@104:2>:int) =
        (((1 + <fib_fib_1039@104:2>) <> 0) && (<fib_fib_1039@104:2> <> 0))
        P[<<fib_fib_1039@104:3>@107:2>](<fib_fib_1039@104:0>:int,<fib_fib_1039@104:1>:int,<fib_fib_1039@104:2>:int,<<fib_fib_1039@104:3>@107:0>:int,<<fib_fib_1039@104:3>@107:1>:int,<<fib_fib_1039@104:3>@107:2>:int) =
        false
        P[<<fib_fib_1039@98:3>@115:2>](<fib_fib_1039@98:0>:int,<fib_fib_1039@98:1>:int,<fib_fib_1039@98:2>:int,<<fib_fib_1039@98:3>@115:0>:int,<<fib_fib_1039@98:3>@115:1>:int,<<fib_fib_1039@98:3>@115:2>:int) =
        false
        P[<fail_1866@126:0>](<fail_1866@126:0>:bool) = false
      begin BwHcSolver.solve_preds(17872)[4]
        input:
          P[<fail_1866@126:0>](<fail_1866@126:0>:bool) = false
          P[<fail_1866@126:0>](<fail_1866@126:0>:bool)|- bot
        finding a solution to P[<fail_1866@126:0>](var203:bool)
        begin InterpProver.interpolate(17882)[5]
          begin InterpProver.interpolate_fresh(17883)[6]
            begin InterpProver.interpolate_log(17884)[7]
              input1: false
              input2: true
              begin InterpProver.interpolate_check(17885)[8]
                begin InterpProver.interpolate_simplify(17886)[9]
                  begin InterpProver.interpolate_quick(17899)[10]
                    
                  end InterpProver.interpolate_quick(17899)[10] (0.000000 sec.)
                  begin minimizing # of disjunctions(17903)[10]
                    input: false
                    output: false
                  end minimizing # of disjunctions(17903)[10] (0.000000 sec.)
                  
                end InterpProver.interpolate_simplify(17886)[9] (0.000000 sec.)
                
              end InterpProver.interpolate_check(17885)[8] (0.000000 sec.)
              output: false
            end InterpProver.interpolate_log(17884)[7] (0.000000 sec.)
            
          end InterpProver.interpolate_fresh(17883)[6] (0.000000 sec.)
          
        end InterpProver.interpolate(17882)[5] (0.000000 sec.)
        solution:
          P[<fail_1866@126:0>](var203:bool) = false
        
      end BwHcSolver.solve_preds(17872)[4] (0.000000 sec.)
      begin BwHcSolver.solve_preds(17921)[4]
        input:
          P[<<fib_fib_1039@98:3>@115:2>](<fib_fib_1039@98:0>:int,<fib_fib_1039@98:1>:int,<fib_fib_1039@98:2>:int,<<fib_fib_1039@98:3>@115:0>:int,<<fib_fib_1039@98:3>@115:1>:int,<<fib_fib_1039@98:3>@115:2>:int) =
          false
          P[<<fib_fib_1039@98:3>@115:2>](0:int,1:int,<br_k_main_1864@123:3>:int,<br_k_main_1864@123:4>:int,<br_k_main_1864@123:5>:int,<br_k_main_1864@123:6>:int),
          ((<br_k_main_1864@123:4> <> <br_k_main_1864@123:6>) && <fail_1866@126:0>) |- bot
        finding a solution to P[<<fib_fib_1039@98:3>@115:2>](var204:int,var205:int,var206:int,var207:int,var208:int,var209:int)
        begin InterpProver.interpolate(18000)[5]
          begin InterpProver.interpolate_fresh(18001)[6]
            begin InterpProver.interpolate_log(18002)[7]
              input1: false
              input2: ((var207 <> var209) && ((var204 = 0) && (var205 = 1)))
              begin InterpProver.interpolate_check(18003)[8]
                begin InterpProver.interpolate_simplify(18004)[9]
                  begin InterpProver.interpolate_quick(18063)[10]
                    
                  end InterpProver.interpolate_quick(18063)[10] (0.000000 sec.)
                  begin minimizing # of disjunctions(18067)[10]
                    input: false
                    output: false
                  end minimizing # of disjunctions(18067)[10] (0.000000 sec.)
                  
                end InterpProver.interpolate_simplify(18004)[9] (0.000000 sec.)
                
              end InterpProver.interpolate_check(18003)[8] (0.000000 sec.)
              output: false
            end InterpProver.interpolate_log(18002)[7] (0.000000 sec.)
            
          end InterpProver.interpolate_fresh(18001)[6] (0.000000 sec.)
          
        end InterpProver.interpolate(18000)[5] (0.000000 sec.)
        solution:
          P[<<fib_fib_1039@98:3>@115:2>](var204:int,var205:int,var206:int,var207:int,var208:int,var209:int) = false
        
      end BwHcSolver.solve_preds(17921)[4] (0.000000 sec.)
      begin BwHcSolver.solve_preds(18104)[4]
        input:
          P[<fib_fib_1039@98:2>](<fib_fib_1039@98:0>:int,<fib_fib_1039@98:1>:int,<fib_fib_1039@98:2>:int) = true
          P[<<fib_fib_1039@104:3>@107:2>](<fib_fib_1039@104:0>:int,<fib_fib_1039@104:1>:int,<fib_fib_1039@104:2>:int,<<fib_fib_1039@104:3>@107:0>:int,<<fib_fib_1039@104:3>@107:1>:int,<<fib_fib_1039@104:3>@107:2>:int) =
          false
          P[<fib_fib_1039@98:2>](<fib_fib_1039@98:0>:int,<fib_fib_1039@98:1>:int,<fib_fib_1039@98:2>:int),
          P[<<fib_fib_1039@104:3>@107:2>](<fib_fib_1039@98:1>:int,(<fib_fib_1039@98:0> + <fib_fib_1039@98:1>):int,(
                                          -1 + <fib_fib_1039@98:2>):int,<<fib_fib_1039@98:3>@115:0>:int,(
                                          <<fib_fib_1039@98:3>@115:2> + (
                                          -1 * <<fib_fib_1039@98:3>@115:1>)):int,<<fib_fib_1039@98:3>@115:1>:int),
          ((<fib_fib_1039@98:2> <> 0) && (<fib_fib_1039@98:2> <> 1)) |- bot
        finding a solution to P[<<fib_fib_1039@104:3>@107:2>](var210:int,var211:int,var212:int,var213:int,var214:int,var215:int)
        begin InterpProver.interpolate(18251)[5]
          begin InterpProver.interpolate_fresh(18252)[6]
            begin InterpProver.interpolate_log(18253)[7]
              input1: false
              input2: ((var212 <> 0) && ((1 + var212) <> 0))
              begin InterpProver.interpolate_check(18254)[8]
                begin InterpProver.interpolate_simplify(18255)[9]
                  begin InterpProver.interpolate_quick(18292)[10]
                    
                  end InterpProver.interpolate_quick(18292)[10] (0.000000 sec.)
                  begin minimizing # of disjunctions(18296)[10]
                    input: false
                    output: false
                  end minimizing # of disjunctions(18296)[10] (0.000000 sec.)
                  
                end InterpProver.interpolate_simplify(18255)[9] (0.000000 sec.)
                
              end InterpProver.interpolate_check(18254)[8] (0.000000 sec.)
              output: false
            end InterpProver.interpolate_log(18253)[7] (0.000000 sec.)
            
          end InterpProver.interpolate_fresh(18252)[6] (0.000000 sec.)
          
        end InterpProver.interpolate(18251)[5] (0.000000 sec.)
        solution:
          P[<<fib_fib_1039@104:3>@107:2>](var210:int,var211:int,var212:int,var213:int,var214:int,var215:int) = false
        finding a solution to P[<fib_fib_1039@98:2>](var216:int,var217:int,var218:int)
        begin InterpProver.interpolate(18319)[5]
          begin InterpProver.interpolate_fresh(18320)[6]
            begin InterpProver.interpolate_log(18321)[7]
              input1: true
              input2: false
              begin InterpProver.interpolate_check(18322)[8]
                begin InterpProver.interpolate_simplify(18323)[9]
                  begin InterpProver.interpolate_quick(18336)[10]
                    
                  end InterpProver.interpolate_quick(18336)[10] (0.000000 sec.)
                  
                end InterpProver.interpolate_simplify(18323)[9] (0.000000 sec.)
                
              end InterpProver.interpolate_check(18322)[8] (0.000000 sec.)
              output: true
            end InterpProver.interpolate_log(18321)[7] (0.000000 sec.)
            
          end InterpProver.interpolate_fresh(18320)[6] (0.000000 sec.)
          
        end InterpProver.interpolate(18319)[5] (0.000000 sec.)
        solution:
          P[<fib_fib_1039@98:2>](var216:int,var217:int,var218:int) = true
        
      end BwHcSolver.solve_preds(18104)[4] (0.000000 sec.)
      begin BwHcSolver.solve_preds(18373)[4]
        input:
          P[<fib_fib_1039@104:2>](<fib_fib_1039@104:0>:int,<fib_fib_1039@104:1>:int,<fib_fib_1039@104:2>:int) =
          (((1 + <fib_fib_1039@104:2>) <> 0) && (<fib_fib_1039@104:2> <> 0))
          P[<fib_fib_1039@104:2>](<fib_fib_1039@104:0>:int,<fib_fib_1039@104:1>:int,<fib_fib_1039@104:2>:int),
          ((<fib_fib_1039@104:2> = 0) &&
           ((<<fib_fib_1039@104:3>@107:1> = 0) &&
            ((<<fib_fib_1039@104:3>@107:2> = 0) && (<<fib_fib_1039@104:3>@107:0> = <fib_fib_1039@104:0>)))) |- bot
        finding a solution to P[<fib_fib_1039@104:2>](var219:int,var220:int,var221:int)
        begin InterpProver.interpolate(18410)[5]
          begin InterpProver.interpolate_fresh(18411)[6]
            begin InterpProver.interpolate_log(18412)[7]
              input1: ((var221 <> 0) && ((1 + var221) <> 0))
              input2: (var221 = 0)
              begin InterpProver.interpolate_check(18413)[8]
                begin InterpProver.interpolate_simplify(18414)[9]
                  begin InterpProver.interpolate_quick(18453)[10]
                    begin CsisatInterface.interpolate_csisat_wrap(18462)[11]
                      begin CsisatInterface.interpolate_csisat_post_process(18463)[12]
                        begin CsisatInterface.interpolate_csisat_log(18464)[13]
                          input1: (not 0 = v_sep_var221 & not 0 = (1 + v_sep_var221))
                          input2: 0 = v_sep_var221
                          begin CsisatInterface.interpolate_csisat_raw(18465)[14]
                            
                          end CsisatInterface.interpolate_csisat_raw(18465)[14] (0.000000 sec.)
                          output: not 0 = v_sep_var221
                          
                        end CsisatInterface.interpolate_csisat_log(18464)[13] (0.000000 sec.)
                        after simplification: not 0 = v_sep_var221
                        after dnf conversion: ((not 0 = v_sep_var221))
                        
                      end CsisatInterface.interpolate_csisat_post_process(18463)[12] (0.000000 sec.)
                      
                    end CsisatInterface.interpolate_csisat_wrap(18462)[11] (0.000000 sec.)
                    
                  end InterpProver.interpolate_quick(18453)[10] (0.000000 sec.)
                  
                end InterpProver.interpolate_simplify(18414)[9] (0.000000 sec.)
                
              end InterpProver.interpolate_check(18413)[8] (0.000000 sec.)
              output: (0 <> var221)
            end InterpProver.interpolate_log(18412)[7] (0.000000 sec.)
            
          end InterpProver.interpolate_fresh(18411)[6] (0.000000 sec.)
          
        end InterpProver.interpolate(18410)[5] (0.000000 sec.)
        solution:
          P[<fib_fib_1039@104:2>](var219:int,var220:int,var221:int) = (0 <> var221)
        
      end BwHcSolver.solve_preds(18373)[4] (0.000000 sec.)
      begin HcSolver.check_validity(18521)[4]
        input:
          P[<fail_1866@126:0>](<fail_1866@126:0>:bool)|- bot
          
          P[<<fib_fib_1039@98:3>@115:2>](0:int,1:int,<br_k_main_1864@123:3>:int,<br_k_main_1864@123:4>:int,<br_k_main_1864@123:5>:int,<br_k_main_1864@123:6>:int),
          ((<br_k_main_1864@123:4> <> <br_k_main_1864@123:6>) && <fail_1866@126:0>)
          |- P[<fail_1866@126:0>](<fail_1866@126:0>:bool)
          
          P[<fib_fib_1039@98:2>](<fib_fib_1039@98:0>:int,<fib_fib_1039@98:1>:int,<fib_fib_1039@98:2>:int),
          P[<<fib_fib_1039@104:3>@107:2>](<fib_fib_1039@98:1>:int,(<fib_fib_1039@98:0> + <fib_fib_1039@98:1>):int,(
                                          -1 + <fib_fib_1039@98:2>):int,<<fib_fib_1039@98:3>@115:0>:int,(
                                          <<fib_fib_1039@98:3>@115:2> + (
                                          -1 * <<fib_fib_1039@98:3>@115:1>)):int,<<fib_fib_1039@98:3>@115:1>:int),
          ((<fib_fib_1039@98:2> <> 0) && (<fib_fib_1039@98:2> <> 1))
          |- P[<<fib_fib_1039@98:3>@115:2>](<fib_fib_1039@98:0>:int,<fib_fib_1039@98:1>:int,
                                            <fib_fib_1039@98:2>:int,<<fib_fib_1039@98:3>@115:0>:int,
                                            <<fib_fib_1039@98:3>@115:1>:int,
                                            <<fib_fib_1039@98:3>@115:2>:int)
          
          P[<fib_fib_1039@104:2>](<fib_fib_1039@104:0>:int,<fib_fib_1039@104:1>:int,<fib_fib_1039@104:2>:int),
          ((<fib_fib_1039@104:2> = 0) &&
           ((<<fib_fib_1039@104:3>@107:1> = 0) &&
            ((<<fib_fib_1039@104:3>@107:2> = 0) && (<<fib_fib_1039@104:3>@107:0> = <fib_fib_1039@104:0>))))
          |- P[<<fib_fib_1039@104:3>@107:2>](<fib_fib_1039@104:0>:int,
                                             <fib_fib_1039@104:1>:int,
                                             <fib_fib_1039@104:2>:int,
                                             <<fib_fib_1039@104:3>@107:0>:int,
                                             <<fib_fib_1039@104:3>@107:1>:int,
                                             <<fib_fib_1039@104:3>@107:2>:int)
          
          P[<fib_fib_1039@98:2>]((<fib_fib_1039@104:1> + (-1 * <fib_fib_1039@104:0>)):int,<fib_fib_1039@104:0>:int,(
                                 1 + <fib_fib_1039@104:2>):int),
          (((1 + <fib_fib_1039@104:2>) <> 0) && (<fib_fib_1039@104:2> <> 0))
          |- P[<fib_fib_1039@104:2>](<fib_fib_1039@104:0>:int,<fib_fib_1039@104:1>:int,
                                     <fib_fib_1039@104:2>:int)
          
          |- P[<fib_fib_1039@98:2>](<fib_fib_1039@98:0>:int,<fib_fib_1039@98:1>:int,
                                    <fib_fib_1039@98:2>:int)
        checking substituted Horn clause:
           false |- bot
        checking substituted Horn clause:
           false |- bot
        checking substituted Horn clause:
           false |- bot
        checking substituted Horn clause:
           false |- bot
        checking substituted Horn clause:
           false |- bot
        checking substituted Horn clause:
           false |- bot
        
      end HcSolver.check_validity(18521)[4] (0.000000 sec.)
      solution:
        P[<fail_1866@126:0>](var203:bool) = false
        P[<<fib_fib_1039@98:3>@115:2>](var204:int,var205:int,var206:int,var207:int,var208:int,var209:int) = false
        P[<<fib_fib_1039@104:3>@107:2>](var210:int,var211:int,var212:int,var213:int,var214:int,var215:int) = false
        P[<fib_fib_1039@98:2>](var216:int,var217:int,var218:int) = true
        P[<fib_fib_1039@104:2>](var219:int,var220:int,var221:int) = (0 <> var221)
    end BwHcSolver.solve(17627)[3] (0.004000 sec.)
    
  end RefTypeInfer.infer_etrs(13957)[2] (0.124000 sec.)
  refinement types:
    main_1846: X
    f_1852: v1:int -> X
    f_1853: v1:int -> v2:int -> X
    f_1854: v1:int -> v2:int -> v3:int -> X
    main_1043: v1:int -> v2:int -> v3:int -> (v4:unit -> X) -> X
    fib_fib_1039: v1:int -> v2:int -> v3:int -> (v4:int -> v5:int -> v6:{v6:int | false} -> X) -> X
    br_fib_fib_1862: v1:bool -> v2:int -> v3:int -> v4:int -> (v5:int -> v6:int -> v7:int -> X) -> X
    fib_fib_1039: v1:int -> v2:int -> v3:{v3:int | (0 <> v3)} -> (v4:int -> v5:int -> v6:{v6:int | false} -> X) -> X
    f_fib_fib_1850: v1:int -> v2:int -> v3:int -> (v4:int -> v5:int -> v6:int -> X) -> v8:int -> v9:int -> v10:int -> X
    f_main_1851: v1:int -> v2:int -> v3:int -> (v4:unit -> X) -> v6:int -> v7:int -> v8:int -> X
    k_main_1589: v1:int -> v2:int -> v3:int -> v4:int -> v5:int -> v6:int -> (v7:unit -> X) -> v9:bool -> X
    br_k_main_1864: v1:bool ->
                    v2:int -> v3:int -> v4:int -> v5:int -> v6:int -> v7:int -> (v8:unit -> X) -> v10:bool -> X
    fail_1866: v1:{v1:bool | false} -> (v2:unit -> X) -> X
    br_fib''_1860: var240:bool -> var239:int -> (var238:int -> var237:int -> X) -> X
    br_fib'_1858: var247:bool -> var246:int -> var245:int -> var244:int -> (var243:int -> X) -> X
    br_fib_1856: var252:bool -> var251:int -> (var250:int -> X) -> X
    f_1855: var257:int -> var256:int -> var255:int -> var254:unit -> X
    f_fib''_1849: var264:int -> (var263:int -> var262:int -> X) -> var260:int -> var259:int -> X
    f_fib_1847: var269:int -> (var268:int -> X) -> var266:int -> X
    f_fib_1848: var275:int -> var274:int -> (var273:int -> X) -> var271:int -> X
    fib''_1036: var280:int -> (var279:int -> var278:int -> X) -> X
    fib'_1032: var286:int -> var285:int -> var284:int -> (var283:int -> X) -> X
    fib_1030: var290:int -> (var289:int -> X) -> X
  abstraction types:
    main_1846: X
    f_1852: v1:int -> X
    f_1853: v1:int -> v2:int -> X
    f_1854: v1:int -> v2:int -> v3:int -> X
    main_1043: v1:int -> v2:int -> v3:int -> (v4:unit -> X) -> X
    fib_fib_1039: var291:int ->
                  var292:int ->
                  var293:int[var293 -> (0 <> var293)] -> (var294:int -> var295:int -> var296:int -> X) -> X
    br_fib_fib_1862: v1:bool -> v2:int -> v3:int -> v4:int -> (v5:int -> v6:int -> v7:int -> X) -> X
    f_fib_fib_1850: v1:int -> v2:int -> v3:int -> (v4:int -> v5:int -> v6:int -> X) -> v8:int -> v9:int -> v10:int -> X
    f_main_1851: v1:int -> v2:int -> v3:int -> (v4:unit -> X) -> v6:int -> v7:int -> v8:int -> X
    k_main_1589: v1:int -> v2:int -> v3:int -> v4:int -> v5:int -> v6:int -> (v7:unit -> X) -> v9:bool -> X
    br_k_main_1864: v1:bool ->
                    v2:int -> v3:int -> v4:int -> v5:int -> v6:int -> v7:int -> (v8:unit -> X) -> v10:bool -> X
    fail_1866: v1:bool -> (v2:unit -> X) -> X
    br_fib''_1860: var240:bool -> var239:int -> (var238:int -> var237:int -> X) -> X
    br_fib'_1858: var247:bool -> var246:int -> var245:int -> var244:int -> (var243:int -> X) -> X
    br_fib_1856: var252:bool -> var251:int -> (var250:int -> X) -> X
    f_1855: var257:int -> var256:int -> var255:int -> var254:unit -> X
    f_fib''_1849: var264:int -> (var263:int -> var262:int -> X) -> var260:int -> var259:int -> X
    f_fib_1847: var269:int -> (var268:int -> X) -> var266:int -> X
    f_fib_1848: var275:int -> var274:int -> (var273:int -> X) -> var271:int -> X
    fib''_1036: var280:int -> (var279:int -> var278:int -> X) -> X
    fib'_1032: var286:int -> var285:int -> var284:int -> (var283:int -> X) -> X
    fib_1030: var290:int -> (var289:int -> X) -> X
  
end AbsTypeInfer.refine(13956)[1] (0.128000 sec.)
DONE!

Prefix of spurious counter-example::
0; 0; 0; 0; 0; 1; 1; 0

Program with abstraction types (CEGAR-cycle 3)::
Main: main_1846
  main_1846 -> (rand_int f_1852)
  br_fib''_1860 b_1861 n_1037 k_fib''_1427 when b_1861 -> (k_fib''_1427 0 1)
  br_fib''_1860 b_1861 n_1037 k_fib''_1427 when (not b_1861) ->
      (fib''_1036 (n_1037 - 1) (f_fib''_1849 n_1037 k_fib''_1427))
  br_fib'_1858 b_1859 a_1033 b_1034 n_1035 k_fib'_1393 when b_1859 -> (k_fib'_1393 b_1034)
  br_fib'_1858 b_1859 a_1033 b_1034 n_1035 k_fib'_1393 when (not b_1859) ->
      (fib'_1032 b_1034 (a_1033 + b_1034) (n_1035 - 1) k_fib'_1393)
  br_fib_1856 b_1857 n_1031 k_fib_1341 when b_1857 -> (k_fib_1341 1)
  br_fib_1856 b_1857 n_1031 k_fib_1341 when (not b_1857) -> (fib_1030 (n_1031 - 1) (f_fib_1847 n_1031 k_fib_1341))
  br_fib_fib_1862 b_1863 a_1040 b_1041 n_1042 k_fib_fib_1483 when b_1863 -> (k_fib_fib_1483 b_1041 0 1)
  br_fib_fib_1862 b_1863 a_1040 b_1041 n_1042 k_fib_fib_1483 when (not b_1863) ->
      (fib_fib_1039 b_1041 (a_1040 + b_1041) (n_1042 - 1) (f_fib_fib_1850 a_1040 b_1041 n_1042 k_fib_fib_1483))
  br_k_main_1864 b_1865 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 when b_1865 ->
      (k_main_1571 ())
  br_k_main_1864 b_1865 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 when (
      not b_1865) -> (fail_1866 true k_main_1571)
  f_1852 x_1611 -> (rand_int (f_1853 x_1611))
  f_1853 x_1611 x_1624 -> (rand_int (f_1854 x_1611 x_1624))
  f_1854 x_1611 x_1624 x_1637 -> (main_1043 x_1611 x_1624 x_1637 (f_1855 x_1611 x_1624 x_1637))
  f_1855 x_1611 x_1624 x_1637 x_1608 -> end
  f_fib''_1849 n_1037 k_fib''_1427 x1_1440 x2_1440 -> (k_fib''_1427 x2_1440 (x1_1440 + x2_1440))
  f_fib_1847 n_1031 k_fib_1341 x_1344 -> (fib_1030 (n_1031 - 2) (f_fib_1848 n_1031 x_1344 k_fib_1341))
  f_fib_1848 n_1031 x_1344 k_fib_1341 x_1345 -> (k_fib_1341 (x_1344 + x_1345))
  f_fib_fib_1850 a_1040 b_1041 n_1042 k_fib_fib_1483 x1_1518 x21_1518 x22_1518 ->
      (k_fib_fib_1483 x1_1518 x22_1518 (x21_1518 + x22_1518))
  f_main_1851 a_1044 b_1045 n_1046 k_main_1571 x1_1574 x21_1574 x22_1574 when (
      a_1044 = 0) -> (k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 (b_1045 = 1))
  f_main_1851 a_1044 b_1045 n_1046 k_main_1571 x1_1574 x21_1574 x22_1574 when (
      not (a_1044 = 0)) -> (k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 false)
  fail_1866 b k -> {fail} => (k ())
  fib''_1036 n_1037 k_fib''_1427 when (n_1037 = 0) -> (k_fib''_1427 0 0)
  fib''_1036 n_1037 k_fib''_1427 when (not (n_1037 = 0)) -> (br_fib''_1860 (n_1037 = 1) n_1037 k_fib''_1427)
  fib'_1032 a_1033 b_1034 n_1035 k_fib'_1393 when (n_1035 = 0) -> (k_fib'_1393 a_1033)
  fib'_1032 a_1033 b_1034 n_1035 k_fib'_1393 when (not (n_1035 = 0)) ->
      (br_fib'_1858 (n_1035 = 1) a_1033 b_1034 n_1035 k_fib'_1393)
  fib_1030 n_1031 k_fib_1341 when (n_1031 = 0) -> (k_fib_1341 0)
  fib_1030 n_1031 k_fib_1341 when (not (n_1031 = 0)) -> (br_fib_1856 (n_1031 = 1) n_1031 k_fib_1341)
  fib_fib_1039 a_1040 b_1041 n_1042 k_fib_fib_1483 when (n_1042 = 0) -> (k_fib_fib_1483 a_1040 0 0)
  fib_fib_1039 a_1040 b_1041 n_1042 k_fib_fib_1483 when (not (n_1042 = 0)) ->
      (br_fib_fib_1862 (n_1042 = 1) a_1040 b_1041 n_1042 k_fib_fib_1483)
  k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 when b_1605 ->
      (br_k_main_1864 (x1_1574 = x22_1574) a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605)
  k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 when (not b_1605) -> (k_main_1571 ())
  main_1043 a_1044 b_1045 n_1046 k_main_1571 ->
      (fib_fib_1039 a_1044 b_1045 n_1046 (f_main_1851 a_1044 b_1045 n_1046 k_main_1571))
Types:
  main_1846 : X
  fail_1866 : (x_1:bool[x_1] -> (unit -> X) -> X)
  fib_1030 : (int -> (int -> X) -> X)
  fib_fib_1039 : (x_1:int ->
                  x_2:int ->
                  x_3:int[(not (x_3 = 0))] ->
                  (x_5:int ->
                   x_6:int ->
                   x_7:int[x_2 = x_5 && x_6 = -x_7 + 1; x_7 = 1 && x_1 = -x_2 + x_5; x_7 = 1 && x_2 = x_5] -> X) -> X)
  
(3-1) Abstracting ... DONE!

(3-2) Checking HORS ... DONE!

Error trace::
  main_1846 ... --> 
  f_1852 ... --> 
  f_1853 ... --> 
  f_1854 ... --> 
  main_1043 ... --> 
  fib_fib_1039 [1/2] ... --> 
  f_main_1851 [1/2] ... --> 
  k_main_1589 [1/2] ... --> 
  br_k_main_1864 [2/2] ... --> 
  fail_1866 ... --> fail -->
  ERROR!

Spurious counter-example::
  0; 0; 0; 0; 0; 0; 0; 0; 1; 0

(3-3) Checking counter-example ... DONE!

(3-4) Discovering predicates ... 
begin AbsTypeInfer.refine(20412)[1]
  program:
    main_1846  | true = ((Random.int 0) f_1852)
    br_fib''_1860 b_1861 n_1037 k_fib''_1427 | b_1861 = (k_fib''_1427 0
                                                                    1)
    br_fib''_1860 b_1861 n_1037 k_fib''_1427 | (not b_1861) = (fib''_1036
                                                                 (n_1037 - 1)
                                                                 (f_fib''_1849 n_1037
                                                                    k_fib''_1427))
    br_fib'_1858 b_1859 a_1033 b_1034 n_1035 k_fib'_1393 | b_1859 = (
    k_fib'_1393 b_1034)
    br_fib'_1858 b_1859 a_1033 b_1034 n_1035 k_fib'_1393 | (not b_1859) = (
    fib'_1032 b_1034
              (a_1033 + b_1034)
              (n_1035 - 1)
              k_fib'_1393)
    br_fib_1856 b_1857 n_1031 k_fib_1341 | b_1857 = (k_fib_1341 1)
    br_fib_1856 b_1857 n_1031 k_fib_1341 | (not b_1857) = (fib_1030 (
                                                                    n_1031 - 1)
                                                                    (
                                                                    f_fib_1847 n_1031
                                                                    k_fib_1341))
    br_fib_fib_1862 b_1863 a_1040 b_1041 n_1042 k_fib_fib_1483 | b_1863 = (
    k_fib_fib_1483 b_1041
                   0
                   1)
    br_fib_fib_1862 b_1863 a_1040 b_1041 n_1042 k_fib_fib_1483 | (not b_1863) = (
    fib_fib_1039 b_1041
                 (a_1040 + b_1041)
                 (n_1042 - 1)
                 (f_fib_fib_1850 a_1040
                                 b_1041
                                 n_1042
                                 k_fib_fib_1483))
    br_k_main_1864 b_1865 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 | b_1865 = (
    k_main_1571 ())
    br_k_main_1864 b_1865 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 | (
    not b_1865) = (fail_1866 true
                             k_main_1571)
    f_1852 x_1611 | true = ((Random.int 0) (f_1853 x_1611))
    f_1853 x_1611 x_1624 | true = ((Random.int 0) (f_1854 x_1611
                                                          x_1624))
    f_1854 x_1611 x_1624 x_1637 | true = (main_1043 x_1611
                                                    x_1624
                                                    x_1637
                                                    (f_1855 x_1611
                                                            x_1624
                                                            x_1637))
    f_1855 x_1611 x_1624 x_1637 x_1608 | true = end
    f_fib''_1849 n_1037 k_fib''_1427 x1_1440 x2_1440 | true = (k_fib''_1427 x2_1440
                                                                    (x1_1440 + x2_1440))
    f_fib_1847 n_1031 k_fib_1341 x_1344 | true = (fib_1030 (n_1031 - 2)
                                                           (f_fib_1848 n_1031
                                                                    x_1344
                                                                    k_fib_1341))
    f_fib_1848 n_1031 x_1344 k_fib_1341 x_1345 | true = (k_fib_1341 (
                                                                    x_1344 + x_1345))
    f_fib_fib_1850 a_1040 b_1041 n_1042 k_fib_fib_1483 x1_1518 x21_1518 x22_1518 | true = (
    k_fib_fib_1483 x1_1518
                   x22_1518
                   (x21_1518 + x22_1518))
    f_main_1851 a_1044 b_1045 n_1046 k_main_1571 x1_1574 x21_1574 x22_1574 | (
    a_1044 = 0) = (k_main_1589 a_1044
                               b_1045
                               n_1046
                               x1_1574
                               x21_1574
                               x22_1574
                               k_main_1571
                               (b_1045 = 1))
    f_main_1851 a_1044 b_1045 n_1046 k_main_1571 x1_1574 x21_1574 x22_1574 | (
    not (a_1044 = 0)) = (k_main_1589 a_1044
                                     b_1045
                                     n_1046
                                     x1_1574
                                     x21_1574
                                     x22_1574
                                     k_main_1571
                                     false)
    fail_1866 b k | true = (fail ())
    fib''_1036 n_1037 k_fib''_1427 | (n_1037 = 0) = (k_fib''_1427 0
                                                                  0)
    fib''_1036 n_1037 k_fib''_1427 | (not (n_1037 = 0)) = (br_fib''_1860 (
                                                                    n_1037 = 1)
                                                                    n_1037
                                                                    k_fib''_1427)
    fib'_1032 a_1033 b_1034 n_1035 k_fib'_1393 | (n_1035 = 0) = (k_fib'_1393 a_1033)
    fib'_1032 a_1033 b_1034 n_1035 k_fib'_1393 | (not (n_1035 = 0)) = (
    br_fib'_1858 (n_1035 = 1)
                 a_1033
                 b_1034
                 n_1035
                 k_fib'_1393)
    fib_1030 n_1031 k_fib_1341 | (n_1031 = 0) = (k_fib_1341 0)
    fib_1030 n_1031 k_fib_1341 | (not (n_1031 = 0)) = (br_fib_1856 (n_1031 = 1)
                                                                   n_1031
                                                                   k_fib_1341)
    fib_fib_1039 a_1040 b_1041 n_1042 k_fib_fib_1483 | (n_1042 = 0) = (
    k_fib_fib_1483 a_1040
                   0
                   0)
    fib_fib_1039 a_1040 b_1041 n_1042 k_fib_fib_1483 | (not (n_1042 = 0)) = (
    br_fib_fib_1862 (n_1042 = 1)
                    a_1040
                    b_1041
                    n_1042
                    k_fib_fib_1483)
    k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 | b_1605 = (
    br_k_main_1864 (x1_1574 = x22_1574)
                   a_1044
                   b_1045
                   n_1046
                   x1_1574
                   x21_1574
                   x22_1574
                   k_main_1571
                   b_1605)
    k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 | (
    not b_1605) = (k_main_1571 ())
    main_1043 a_1044 b_1045 n_1046 k_main_1571 | true = (fib_fib_1039
                                                           a_1044
                                                           b_1045
                                                           n_1046
                                                           (f_main_1851 a_1044
                                                                    b_1045
                                                                    n_1046
                                                                    k_main_1571))
    main_1846:X
    br_fib''_1860:bool -> int -> (int -> int -> X) -> X
    br_fib'_1858:bool -> int -> int -> int -> (int -> X) -> X
    br_fib_1856:bool -> int -> (int -> X) -> X
    br_fib_fib_1862:bool -> int -> int -> int -> (int -> int -> int -> X) -> X
    br_k_main_1864:bool -> int -> int -> int -> int -> int -> int -> (unit -> X) -> bool -> X
    f_1852:int -> X
    f_1853:int -> int -> X
    f_1854:int -> int -> int -> X
    f_1855:int -> int -> int -> unit -> X
    f_fib''_1849:int -> (int -> int -> X) -> int -> int -> X
    f_fib_1847:int -> (int -> X) -> int -> X
    f_fib_1848:int -> int -> (int -> X) -> int -> X
    f_fib_fib_1850:int -> int -> int -> (int -> int -> int -> X) -> int -> int -> int -> X
    f_main_1851:int -> int -> int -> (unit -> X) -> int -> int -> int -> X
    fail_1866:bool -> (unit -> X) -> X
    fib''_1036:int -> (int -> int -> X) -> X
    fib'_1032:int -> int -> int -> (int -> X) -> X
    fib_1030:int -> (int -> X) -> X
    fib_fib_1039:int -> int -> int -> (int -> int -> int -> X) -> X
    k_main_1589:int -> int -> int -> int -> int -> int -> (unit -> X) -> bool -> X
    main_1043:int -> int -> int -> (unit -> X) -> X
  inlined functions: br_fib''_1860,br_fib'_1858,br_fib_1856,br_fib_fib_1862,br_k_main_1864,f_1852,f_1853,f_1854,f_1855,f_fib''_1849,f_fib_1847,f_fib_1848,f_fib_fib_1850,f_main_1851,fib''_1036,fib'_1032,k_main_1589,main_1043
  counterexample: 0:0:0:0:0:0:0:0:1:0
  error traces:
    [true.nop(<f_1852@132:0> = var299)
     [true.nop((<f_1853@135:0> = <f_1852@132:0>) && (<f_1853@135:1> = var300))
      [true.nop((<f_1854@138:0> = <f_1853@135:0>) && ((<f_1854@138:1> = <f_1853@135:1>) && (<f_1854@138:2> = var301)))
       [true.
        ((<main_1043@140:0> = <f_1854@138:0>) &&
         ((<main_1043@140:1> = <f_1854@138:1>) && (<main_1043@140:2> = <f_1854@138:2>)))
        [true.
         ((<fib_fib_1039@142:0> = <main_1043@140:0>) &&
          ((<fib_fib_1039@142:1> = <main_1043@140:1>) && (<fib_fib_1039@142:2> = <main_1043@140:2>)))
         [(<fib_fib_1039@142:2> = 0).
          ((<<fib_fib_1039@142:3>@145:0> = <fib_fib_1039@142:0>) &&
           ((<<fib_fib_1039@142:3>@145:1> = 0) && (<<fib_fib_1039@142:3>@145:2> = 0)))
          [true.
           ((<f_main_1851@147:0> = <main_1043@140:0>) &&
            ((<f_main_1851@147:1> = <main_1043@140:1>) &&
             ((<f_main_1851@147:2> = <main_1043@140:2>) &&
              ((<f_main_1851@147:4> = <<fib_fib_1039@142:3>@145:0>) &&
               ((<f_main_1851@147:5> = <<fib_fib_1039@142:3>@145:1>) &&
                (<f_main_1851@147:6> = <<fib_fib_1039@142:3>@145:2>))))))
           [(<f_main_1851@147:0> = 0).
            ((<k_main_1589@150:0> = <f_main_1851@147:0>) &&
             ((<k_main_1589@150:1> = <f_main_1851@147:1>) &&
              ((<k_main_1589@150:2> = <f_main_1851@147:2>) &&
               ((<k_main_1589@150:3> = <f_main_1851@147:4>) &&
                ((<k_main_1589@150:4> = <f_main_1851@147:5>) &&
                 ((<k_main_1589@150:5> = <f_main_1851@147:6>) && (<k_main_1589@150:7> = (<f_main_1851@147:1> = 1))))))))
            [<k_main_1589@150:7>.
             ((<br_k_main_1864@153:0> = (<k_main_1589@150:3> = <k_main_1589@150:5>)) &&
              ((<br_k_main_1864@153:1> = <k_main_1589@150:0>) &&
               ((<br_k_main_1864@153:2> = <k_main_1589@150:1>) &&
                ((<br_k_main_1864@153:3> = <k_main_1589@150:2>) &&
                 ((<br_k_main_1864@153:4> = <k_main_1589@150:3>) &&
                  ((<br_k_main_1864@153:5> = <k_main_1589@150:4>) &&
                   ((<br_k_main_1864@153:6> = <k_main_1589@150:5>) && (<br_k_main_1864@153:8> = <k_main_1589@150:7>))))))))
             [(not <br_k_main_1864@153:0>).(<fail_1866@156:0> = true)[true.error
  begin RefTypeInfer.infer_etrs(20413)[2]
    horn clauses:
      P[<fail_1866@156:0>](<fail_1866@156:0>:bool)|- bot
      
      P[<br_k_main_1864@153:8>](false:bool,<br_k_main_1864@153:1>:int,<br_k_main_1864@153:2>:int,<br_k_main_1864@153:3>:int,<br_k_main_1864@153:4>:int,<br_k_main_1864@153:5>:int,<br_k_main_1864@153:6>:int,<br_k_main_1864@153:8>:bool),
      <fail_1866@156:0> |- P[<fail_1866@156:0>](<fail_1866@156:0>:bool)
      
      P[<k_main_1589@150:7>](<br_k_main_1864@153:1>:int,<br_k_main_1864@153:2>:int,<br_k_main_1864@153:3>:int,<br_k_main_1864@153:4>:int,<br_k_main_1864@153:5>:int,<br_k_main_1864@153:6>:int,<br_k_main_1864@153:8>:bool),
      (<br_k_main_1864@153:8> && (<br_k_main_1864@153:0> = (<br_k_main_1864@153:4> = <br_k_main_1864@153:6>)))
      |- P[<br_k_main_1864@153:8>](<br_k_main_1864@153:0>:bool,<br_k_main_1864@153:1>:int,
                                   <br_k_main_1864@153:2>:int,<br_k_main_1864@153:3>:int,
                                   <br_k_main_1864@153:4>:int,<br_k_main_1864@153:5>:int,
                                   <br_k_main_1864@153:6>:int,<br_k_main_1864@153:8>:bool)
      
      P[<f_main_1851@147:6>](<k_main_1589@150:0>:int,<k_main_1589@150:1>:int,<k_main_1589@150:2>:int,<k_main_1589@150:3>:int,<k_main_1589@150:4>:int,<k_main_1589@150:5>:int),
      ((<k_main_1589@150:0> = 0) && (<k_main_1589@150:7> = (<k_main_1589@150:1> = 1)))
      |- P[<k_main_1589@150:7>](<k_main_1589@150:0>:int,<k_main_1589@150:1>:int,
                                <k_main_1589@150:2>:int,<k_main_1589@150:3>:int,
                                <k_main_1589@150:4>:int,<k_main_1589@150:5>:int,
                                <k_main_1589@150:7>:bool)
      
      P[<main_1043@140:2>](<f_main_1851@147:0>:int,<f_main_1851@147:1>:int,<f_main_1851@147:2>:int),
      P[<<fib_fib_1039@142:3>@145:2>](<f_main_1851@147:0>:int,<f_main_1851@147:1>:int,<f_main_1851@147:2>:int,<f_main_1851@147:4>:int,<f_main_1851@147:5>:int,<f_main_1851@147:6>:int)|- P[<f_main_1851@147:6>](
      <f_main_1851@147:0>:int,<f_main_1851@147:1>:int,<f_main_1851@147:2>:int,
      <f_main_1851@147:4>:int,<f_main_1851@147:5>:int,<f_main_1851@147:6>:int)
      
      P[<fib_fib_1039@142:2>](<fib_fib_1039@142:0>:int,<fib_fib_1039@142:1>:int,<fib_fib_1039@142:2>:int),
      ((<fib_fib_1039@142:2> = 0) &&
       ((<<fib_fib_1039@142:3>@145:1> = 0) &&
        ((<<fib_fib_1039@142:3>@145:2> = 0) && (<<fib_fib_1039@142:3>@145:0> = <fib_fib_1039@142:0>))))
      |- P[<<fib_fib_1039@142:3>@145:2>](<fib_fib_1039@142:0>:int,<fib_fib_1039@142:1>:int,
                                         <fib_fib_1039@142:2>:int,<<fib_fib_1039@142:3>@145:0>:int,
                                         <<fib_fib_1039@142:3>@145:1>:int,
                                         <<fib_fib_1039@142:3>@145:2>:int)
      
      P[<main_1043@140:2>](<fib_fib_1039@142:0>:int,<fib_fib_1039@142:1>:int,<fib_fib_1039@142:2>:int)|- P[<fib_fib_1039@142:2>](
      <fib_fib_1039@142:0>:int,<fib_fib_1039@142:1>:int,<fib_fib_1039@142:2>:int)
      
      P[<f_1854@138:2>](<main_1043@140:0>:int,<main_1043@140:1>:int,<main_1043@140:2>:int)|- P[<main_1043@140:2>](
      <main_1043@140:0>:int,<main_1043@140:1>:int,<main_1043@140:2>:int)
      
      P[<f_1853@135:1>](<f_1854@138:0>:int,<f_1854@138:1>:int)|- P[<f_1854@138:2>](
      <f_1854@138:0>:int,<f_1854@138:1>:int,<f_1854@138:2>:int)
      
      P[<f_1852@132:0>](<f_1853@135:0>:int)|- P[<f_1853@135:1>](<f_1853@135:0>:int,
                                                                <f_1853@135:1>:int)
      
      |- P[<f_1852@132:0>](<f_1852@132:0>:int)
    call trees:
      <main_1846@129>
        <f_1852@132>
          <f_1853@135>
            <f_1854@138>
              <main_1043@140>
                <fib_fib_1039@142>
                </<fib_fib_1039@142:3>@145>
                <f_main_1851@147>
                  <k_main_1589@150>
                    <br_k_main_1864@153>
                      <fail_1866@156>
    inlined horn clauses:
      P[<fail_1866@156:0>](<fail_1866@156:0>:bool)|- bot
      
      P[<<fib_fib_1039@142:3>@145:2>](0:int,1:int,<br_k_main_1864@153:3>:int,<br_k_main_1864@153:4>:int,<br_k_main_1864@153:5>:int,<br_k_main_1864@153:6>:int),
      ((<br_k_main_1864@153:4> <> <br_k_main_1864@153:6>) && <fail_1866@156:0>)
      |- P[<fail_1866@156:0>](<fail_1866@156:0>:bool)
      
      P[<fib_fib_1039@142:2>](<fib_fib_1039@142:0>:int,<fib_fib_1039@142:1>:int,<fib_fib_1039@142:2>:int),
      ((<fib_fib_1039@142:2> = 0) &&
       ((<<fib_fib_1039@142:3>@145:1> = 0) &&
        ((<<fib_fib_1039@142:3>@145:2> = 0) && (<<fib_fib_1039@142:3>@145:0> = <fib_fib_1039@142:0>))))
      |- P[<<fib_fib_1039@142:3>@145:2>](<fib_fib_1039@142:0>:int,<fib_fib_1039@142:1>:int,
                                         <fib_fib_1039@142:2>:int,<<fib_fib_1039@142:3>@145:0>:int,
                                         <<fib_fib_1039@142:3>@145:1>:int,
                                         <<fib_fib_1039@142:3>@145:2>:int)
      
      |- P[<fib_fib_1039@142:2>](<fib_fib_1039@142:0>:int,<fib_fib_1039@142:1>:int,
                                 <fib_fib_1039@142:2>:int)
    begin ParamSubstInfer.infer(22237)[3]
      
    end ParamSubstInfer.infer(22237)[3] (0.096000 sec.)
    inferred extra parameters:
      
    begin RefTypeInfer.elim_coeffs(22239)[3]
      
    end RefTypeInfer.elim_coeffs(22239)[3] (0.000000 sec.)
    begin BwHcSolver.solve(22470)[3]
      lower bounds:
        P[<fib_fib_1039@142:2>](<fib_fib_1039@142:0>:int,<fib_fib_1039@142:1>:int,<fib_fib_1039@142:2>:int) = true
        P[<<fib_fib_1039@142:3>@145:2>](<fib_fib_1039@142:0>:int,<fib_fib_1039@142:1>:int,<fib_fib_1039@142:2>:int,<<fib_fib_1039@142:3>@145:0>:int,<<fib_fib_1039@142:3>@145:1>:int,<<fib_fib_1039@142:3>@145:2>:int) =
        ((<fib_fib_1039@142:2> = 0) &&
         ((<<fib_fib_1039@142:3>@145:1> = 0) &&
          ((<<fib_fib_1039@142:3>@145:2> = 0) && (<<fib_fib_1039@142:3>@145:0> = <fib_fib_1039@142:0>))))
        P[<fail_1866@156:0>](<fail_1866@156:0>:bool) = false
      begin BwHcSolver.solve_preds(22676)[4]
        input:
          P[<fail_1866@156:0>](<fail_1866@156:0>:bool) = false
          P[<fail_1866@156:0>](<fail_1866@156:0>:bool)|- bot
        finding a solution to P[<fail_1866@156:0>](var302:bool)
        begin InterpProver.interpolate(22686)[5]
          begin InterpProver.interpolate_fresh(22687)[6]
            begin InterpProver.interpolate_log(22688)[7]
              input1: false
              input2: true
              begin InterpProver.interpolate_check(22689)[8]
                begin InterpProver.interpolate_simplify(22690)[9]
                  begin InterpProver.interpolate_quick(22703)[10]
                    
                  end InterpProver.interpolate_quick(22703)[10] (0.000000 sec.)
                  begin minimizing # of disjunctions(22707)[10]
                    input: false
                    output: false
                  end minimizing # of disjunctions(22707)[10] (0.000000 sec.)
                  
                end InterpProver.interpolate_simplify(22690)[9] (0.000000 sec.)
                
              end InterpProver.interpolate_check(22689)[8] (0.000000 sec.)
              output: false
            end InterpProver.interpolate_log(22688)[7] (0.000000 sec.)
            
          end InterpProver.interpolate_fresh(22687)[6] (0.000000 sec.)
          
        end InterpProver.interpolate(22686)[5] (0.000000 sec.)
        solution:
          P[<fail_1866@156:0>](var302:bool) = false
        
      end BwHcSolver.solve_preds(22676)[4] (0.000000 sec.)
      begin BwHcSolver.solve_preds(22725)[4]
        input:
          P[<<fib_fib_1039@142:3>@145:2>](<fib_fib_1039@142:0>:int,<fib_fib_1039@142:1>:int,<fib_fib_1039@142:2>:int,<<fib_fib_1039@142:3>@145:0>:int,<<fib_fib_1039@142:3>@145:1>:int,<<fib_fib_1039@142:3>@145:2>:int) =
          ((<fib_fib_1039@142:2> = 0) &&
           ((<<fib_fib_1039@142:3>@145:1> = 0) &&
            ((<<fib_fib_1039@142:3>@145:2> = 0) && (<<fib_fib_1039@142:3>@145:0> = <fib_fib_1039@142:0>))))
          P[<<fib_fib_1039@142:3>@145:2>](0:int,1:int,<br_k_main_1864@153:3>:int,<br_k_main_1864@153:4>:int,<br_k_main_1864@153:5>:int,<br_k_main_1864@153:6>:int),
          ((<br_k_main_1864@153:4> <> <br_k_main_1864@153:6>) && <fail_1866@156:0>) |- bot
        finding a solution to P[<<fib_fib_1039@142:3>@145:2>](var303:int,var304:int,var305:int,var306:int,var307:int,var308:int)
        begin InterpProver.interpolate(22833)[5]
          begin InterpProver.interpolate_fresh(22834)[6]
            begin InterpProver.interpolate_log(22835)[7]
              input1: ((var306 = var303) && ((var308 = 0) && ((var307 = 0) && (var305 = 0))))
              input2: ((var306 <> var308) && ((var303 = 0) && (var304 = 1)))
              begin InterpProver.interpolate_check(22836)[8]
                begin InterpProver.interpolate_simplify(22837)[9]
                  begin InterpProver.interpolate_quick(22992)[10]
                    begin CsisatInterface.interpolate_csisat_wrap(23001)[11]
                      begin CsisatInterface.interpolate_csisat_post_process(23002)[12]
                        begin CsisatInterface.interpolate_csisat_log(23003)[13]
                          input1: (0 = v_sep_var308 & v_sep_var303 = v_sep_var306)
                          input2: (not v_sep_var306 = v_sep_var308 & 0 = v_sep_var303)
                          begin CsisatInterface.interpolate_csisat_raw(23004)[14]
                            
                          end CsisatInterface.interpolate_csisat_raw(23004)[14] (0.000000 sec.)
                          output: (0 = v_sep_var308 & v_sep_var303 = v_sep_var306)
                          
                        end CsisatInterface.interpolate_csisat_log(23003)[13] (0.000000 sec.)
                        after simplification: (0 = v_sep_var308 & v_sep_var303 = v_sep_var306)
                        after dnf conversion: ((0 = v_sep_var308 & v_sep_var303 = v_sep_var306))
                        
                      end CsisatInterface.interpolate_csisat_post_process(23002)[12] (0.000000 sec.)
                      
                    end CsisatInterface.interpolate_csisat_wrap(23001)[11] (0.000000 sec.)
                    
                  end InterpProver.interpolate_quick(22992)[10] (0.000000 sec.)
                  begin minimizing # of conjunctions(23017)[10]
                    input: ((var308 = 0) && (var303 = var306))
                    output: ((var303 = var306) && (var308 = 0))
                  end minimizing # of conjunctions(23017)[10] (0.004000 sec.)
                  
                end InterpProver.interpolate_simplify(22837)[9] (0.004000 sec.)
                
              end InterpProver.interpolate_check(22836)[8] (0.004000 sec.)
              output: ((var303 = var306) && (var308 = 0))
            end InterpProver.interpolate_log(22835)[7] (0.004000 sec.)
            
          end InterpProver.interpolate_fresh(22834)[6] (0.004000 sec.)
          
        end InterpProver.interpolate(22833)[5] (0.004000 sec.)
        solution:
          P[<<fib_fib_1039@142:3>@145:2>](var303:int,var304:int,var305:int,var306:int,var307:int,var308:int) =
          ((var303 = var306) && (var308 = 0))
        
      end BwHcSolver.solve_preds(22725)[4] (0.004000 sec.)
      begin HcSolver.check_validity(23176)[4]
        input:
          P[<fail_1866@156:0>](<fail_1866@156:0>:bool)|- bot
          
          P[<<fib_fib_1039@142:3>@145:2>](0:int,1:int,<br_k_main_1864@153:3>:int,<br_k_main_1864@153:4>:int,<br_k_main_1864@153:5>:int,<br_k_main_1864@153:6>:int),
          ((<br_k_main_1864@153:4> <> <br_k_main_1864@153:6>) && <fail_1866@156:0>)
          |- P[<fail_1866@156:0>](<fail_1866@156:0>:bool)
          
          P[<fib_fib_1039@142:2>](<fib_fib_1039@142:0>:int,<fib_fib_1039@142:1>:int,<fib_fib_1039@142:2>:int),
          ((<fib_fib_1039@142:2> = 0) &&
           ((<<fib_fib_1039@142:3>@145:1> = 0) &&
            ((<<fib_fib_1039@142:3>@145:2> = 0) && (<<fib_fib_1039@142:3>@145:0> = <fib_fib_1039@142:0>))))
          |- P[<<fib_fib_1039@142:3>@145:2>](<fib_fib_1039@142:0>:int,
                                             <fib_fib_1039@142:1>:int,
                                             <fib_fib_1039@142:2>:int,
                                             <<fib_fib_1039@142:3>@145:0>:int,
                                             <<fib_fib_1039@142:3>@145:1>:int,
                                             <<fib_fib_1039@142:3>@145:2>:int)
          
          |- P[<fib_fib_1039@142:2>](<fib_fib_1039@142:0>:int,<fib_fib_1039@142:1>:int,
                                     <fib_fib_1039@142:2>:int)
        checking substituted Horn clause:
           false |- bot
        checking substituted Horn clause:
           false |- bot
        checking substituted Horn clause:
           false |- bot
        checking substituted Horn clause:
           false |- bot
        
      end HcSolver.check_validity(23176)[4] (0.004000 sec.)
      solution:
        P[<fail_1866@156:0>](var302:bool) = false
        P[<<fib_fib_1039@142:3>@145:2>](var303:int,var304:int,var305:int,var306:int,var307:int,var308:int) =
        ((var308 = 0) && (var303 = var306))
        P[<fib_fib_1039@142:2>](var309:int,var310:int,var311:int) = true
    end BwHcSolver.solve(22470)[3] (0.012000 sec.)
    
  end RefTypeInfer.infer_etrs(20413)[2] (0.120000 sec.)
  refinement types:
    main_1846: X
    f_1852: v1:int -> X
    f_1853: v1:int -> v2:int -> X
    f_1854: v1:int -> v2:int -> v3:int -> X
    main_1043: v1:int -> v2:int -> v3:int -> (v4:unit -> X) -> X
    fib_fib_1039: v1:int -> v2:int -> v3:int -> (v4:int -> v5:int -> v6:{v6:int | ((v6 = 0) && (v1 = v4))} -> X) -> X
    f_main_1851: v1:int -> v2:int -> v3:int -> (v4:unit -> X) -> v6:int -> v7:int -> v8:int -> X
    k_main_1589: v1:int -> v2:int -> v3:int -> v4:int -> v5:int -> v6:int -> (v7:unit -> X) -> v9:bool -> X
    br_k_main_1864: v1:bool ->
                    v2:int -> v3:int -> v4:int -> v5:int -> v6:int -> v7:int -> (v8:unit -> X) -> v10:bool -> X
    fail_1866: v1:{v1:bool | false} -> (v2:unit -> X) -> X
    br_fib''_1860: var327:bool -> var326:int -> (var325:int -> var324:int -> X) -> X
    br_fib'_1858: var334:bool -> var333:int -> var332:int -> var331:int -> (var330:int -> X) -> X
    br_fib_1856: var339:bool -> var338:int -> (var337:int -> X) -> X
    br_fib_fib_1862: var348:bool ->
                     var347:int -> var346:int -> var345:int -> (var344:int -> var343:int -> var342:int -> X) -> X
    f_1855: var353:int -> var352:int -> var351:int -> var350:unit -> X
    f_fib''_1849: var360:int -> (var359:int -> var358:int -> X) -> var356:int -> var355:int -> X
    f_fib_1847: var365:int -> (var364:int -> X) -> var362:int -> X
    f_fib_1848: var371:int -> var370:int -> (var369:int -> X) -> var367:int -> X
    f_fib_fib_1850: var382:int ->
                    var381:int ->
                    var380:int ->
                    (var379:int -> var378:int -> var377:int -> X) -> var375:int -> var374:int -> var373:int -> X
    fib''_1036: var387:int -> (var386:int -> var385:int -> X) -> X
    fib'_1032: var393:int -> var392:int -> var391:int -> (var390:int -> X) -> X
    fib_1030: var397:int -> (var396:int -> X) -> X
  abstraction types:
    main_1846: X
    f_1852: v1:int -> X
    f_1853: v1:int -> v2:int -> X
    f_1854: v1:int -> v2:int -> v3:int -> X
    main_1043: v1:int -> v2:int -> v3:int -> (v4:unit -> X) -> X
    fib_fib_1039: v1:int -> v2:int -> v3:int -> (v4:int -> v5:int -> v6:int[v6 -> (
                                                                    (v6 = 0) && (v1 = v4))] -> X) -> X
    f_main_1851: v1:int -> v2:int -> v3:int -> (v4:unit -> X) -> v6:int -> v7:int -> v8:int -> X
    k_main_1589: v1:int -> v2:int -> v3:int -> v4:int -> v5:int -> v6:int -> (v7:unit -> X) -> v9:bool -> X
    br_k_main_1864: v1:bool ->
                    v2:int -> v3:int -> v4:int -> v5:int -> v6:int -> v7:int -> (v8:unit -> X) -> v10:bool -> X
    fail_1866: v1:bool -> (v2:unit -> X) -> X
    br_fib''_1860: var327:bool -> var326:int -> (var325:int -> var324:int -> X) -> X
    br_fib'_1858: var334:bool -> var333:int -> var332:int -> var331:int -> (var330:int -> X) -> X
    br_fib_1856: var339:bool -> var338:int -> (var337:int -> X) -> X
    br_fib_fib_1862: var348:bool ->
                     var347:int -> var346:int -> var345:int -> (var344:int -> var343:int -> var342:int -> X) -> X
    f_1855: var353:int -> var352:int -> var351:int -> var350:unit -> X
    f_fib''_1849: var360:int -> (var359:int -> var358:int -> X) -> var356:int -> var355:int -> X
    f_fib_1847: var365:int -> (var364:int -> X) -> var362:int -> X
    f_fib_1848: var371:int -> var370:int -> (var369:int -> X) -> var367:int -> X
    f_fib_fib_1850: var382:int ->
                    var381:int ->
                    var380:int ->
                    (var379:int -> var378:int -> var377:int -> X) -> var375:int -> var374:int -> var373:int -> X
    fib''_1036: var387:int -> (var386:int -> var385:int -> X) -> X
    fib'_1032: var393:int -> var392:int -> var391:int -> (var390:int -> X) -> X
    fib_1030: var397:int -> (var396:int -> X) -> X
  
end AbsTypeInfer.refine(20412)[1] (0.128000 sec.)
DONE!

Prefix of spurious counter-example::
0; 0; 0; 0; 0; 0; 0; 0; 1

Program with abstraction types (CEGAR-cycle 4)::
Main: main_1846
  main_1846 -> (rand_int f_1852)
  br_fib''_1860 b_1861 n_1037 k_fib''_1427 when b_1861 -> (k_fib''_1427 0 1)
  br_fib''_1860 b_1861 n_1037 k_fib''_1427 when (not b_1861) ->
      (fib''_1036 (n_1037 - 1) (f_fib''_1849 n_1037 k_fib''_1427))
  br_fib'_1858 b_1859 a_1033 b_1034 n_1035 k_fib'_1393 when b_1859 -> (k_fib'_1393 b_1034)
  br_fib'_1858 b_1859 a_1033 b_1034 n_1035 k_fib'_1393 when (not b_1859) ->
      (fib'_1032 b_1034 (a_1033 + b_1034) (n_1035 - 1) k_fib'_1393)
  br_fib_1856 b_1857 n_1031 k_fib_1341 when b_1857 -> (k_fib_1341 1)
  br_fib_1856 b_1857 n_1031 k_fib_1341 when (not b_1857) -> (fib_1030 (n_1031 - 1) (f_fib_1847 n_1031 k_fib_1341))
  br_fib_fib_1862 b_1863 a_1040 b_1041 n_1042 k_fib_fib_1483 when b_1863 -> (k_fib_fib_1483 b_1041 0 1)
  br_fib_fib_1862 b_1863 a_1040 b_1041 n_1042 k_fib_fib_1483 when (not b_1863) ->
      (fib_fib_1039 b_1041 (a_1040 + b_1041) (n_1042 - 1) (f_fib_fib_1850 a_1040 b_1041 n_1042 k_fib_fib_1483))
  br_k_main_1864 b_1865 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 when b_1865 ->
      (k_main_1571 ())
  br_k_main_1864 b_1865 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 when (
      not b_1865) -> (fail_1866 true k_main_1571)
  f_1852 x_1611 -> (rand_int (f_1853 x_1611))
  f_1853 x_1611 x_1624 -> (rand_int (f_1854 x_1611 x_1624))
  f_1854 x_1611 x_1624 x_1637 -> (main_1043 x_1611 x_1624 x_1637 (f_1855 x_1611 x_1624 x_1637))
  f_1855 x_1611 x_1624 x_1637 x_1608 -> end
  f_fib''_1849 n_1037 k_fib''_1427 x1_1440 x2_1440 -> (k_fib''_1427 x2_1440 (x1_1440 + x2_1440))
  f_fib_1847 n_1031 k_fib_1341 x_1344 -> (fib_1030 (n_1031 - 2) (f_fib_1848 n_1031 x_1344 k_fib_1341))
  f_fib_1848 n_1031 x_1344 k_fib_1341 x_1345 -> (k_fib_1341 (x_1344 + x_1345))
  f_fib_fib_1850 a_1040 b_1041 n_1042 k_fib_fib_1483 x1_1518 x21_1518 x22_1518 ->
      (k_fib_fib_1483 x1_1518 x22_1518 (x21_1518 + x22_1518))
  f_main_1851 a_1044 b_1045 n_1046 k_main_1571 x1_1574 x21_1574 x22_1574 when (
      a_1044 = 0) -> (k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 (b_1045 = 1))
  f_main_1851 a_1044 b_1045 n_1046 k_main_1571 x1_1574 x21_1574 x22_1574 when (
      not (a_1044 = 0)) -> (k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 false)
  fail_1866 b k -> {fail} => (k ())
  fib''_1036 n_1037 k_fib''_1427 when (n_1037 = 0) -> (k_fib''_1427 0 0)
  fib''_1036 n_1037 k_fib''_1427 when (not (n_1037 = 0)) -> (br_fib''_1860 (n_1037 = 1) n_1037 k_fib''_1427)
  fib'_1032 a_1033 b_1034 n_1035 k_fib'_1393 when (n_1035 = 0) -> (k_fib'_1393 a_1033)
  fib'_1032 a_1033 b_1034 n_1035 k_fib'_1393 when (not (n_1035 = 0)) ->
      (br_fib'_1858 (n_1035 = 1) a_1033 b_1034 n_1035 k_fib'_1393)
  fib_1030 n_1031 k_fib_1341 when (n_1031 = 0) -> (k_fib_1341 0)
  fib_1030 n_1031 k_fib_1341 when (not (n_1031 = 0)) -> (br_fib_1856 (n_1031 = 1) n_1031 k_fib_1341)
  fib_fib_1039 a_1040 b_1041 n_1042 k_fib_fib_1483 when (n_1042 = 0) -> (k_fib_fib_1483 a_1040 0 0)
  fib_fib_1039 a_1040 b_1041 n_1042 k_fib_fib_1483 when (not (n_1042 = 0)) ->
      (br_fib_fib_1862 (n_1042 = 1) a_1040 b_1041 n_1042 k_fib_fib_1483)
  k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 when b_1605 ->
      (br_k_main_1864 (x1_1574 = x22_1574) a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605)
  k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 when (not b_1605) -> (k_main_1571 ())
  main_1043 a_1044 b_1045 n_1046 k_main_1571 ->
      (fib_fib_1039 a_1044 b_1045 n_1046 (f_main_1851 a_1044 b_1045 n_1046 k_main_1571))
Types:
  main_1846 : X
  fail_1866 : (x_1:bool[x_1] -> (unit -> X) -> X)
  fib_1030 : (int -> (int -> X) -> X)
  fib_fib_1039 : (x_1:int ->
                  x_2:int ->
                  x_3:int[(not (x_3 = 0))] ->
                  (x_5:int ->
                   x_6:int ->
                   x_7:int[x_7 = 0 && x_1 = x_5; x_2 = x_5 && x_6 = -x_7 + 1; 
                           x_7 = 1 && x_1 = -x_2 + x_5; x_7 = 1 && x_2 = x_5] -> X) -> X)
  
(4-1) Abstracting ... DONE!

(4-2) Checking HORS ... DONE!

Error trace::
  main_1846 ... --> 
  f_1852 ... --> 
  f_1853 ... --> 
  f_1854 ... --> 
  main_1043 ... --> 
  fib_fib_1039 [2/2] ... --> 
  br_fib_fib_1862 [2/2] ... --> 
  fib_fib_1039 [2/2] ... --> 
  br_fib_fib_1862 [2/2] ... --> 
  fib_fib_1039 [2/2] ... --> 
  br_fib_fib_1862 [1/2] ... --> 
  f_fib_fib_1850 ... --> 
  f_fib_fib_1850 ... --> 
  f_main_1851 [1/2] ... --> 
  k_main_1589 [1/2] ... --> 
  br_k_main_1864 [2/2] ... --> 
  fail_1866 ... --> fail -->
  ERROR!

Spurious counter-example::
  0; 0; 0; 0; 0; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 1; 0

(4-3) Checking counter-example ... DONE!

(4-4) Discovering predicates ... 
begin AbsTypeInfer.refine(26334)[1]
  program:
    main_1846  | true = ((Random.int 0) f_1852)
    br_fib''_1860 b_1861 n_1037 k_fib''_1427 | b_1861 = (k_fib''_1427 0
                                                                    1)
    br_fib''_1860 b_1861 n_1037 k_fib''_1427 | (not b_1861) = (fib''_1036
                                                                 (n_1037 - 1)
                                                                 (f_fib''_1849 n_1037
                                                                    k_fib''_1427))
    br_fib'_1858 b_1859 a_1033 b_1034 n_1035 k_fib'_1393 | b_1859 = (
    k_fib'_1393 b_1034)
    br_fib'_1858 b_1859 a_1033 b_1034 n_1035 k_fib'_1393 | (not b_1859) = (
    fib'_1032 b_1034
              (a_1033 + b_1034)
              (n_1035 - 1)
              k_fib'_1393)
    br_fib_1856 b_1857 n_1031 k_fib_1341 | b_1857 = (k_fib_1341 1)
    br_fib_1856 b_1857 n_1031 k_fib_1341 | (not b_1857) = (fib_1030 (
                                                                    n_1031 - 1)
                                                                    (
                                                                    f_fib_1847 n_1031
                                                                    k_fib_1341))
    br_fib_fib_1862 b_1863 a_1040 b_1041 n_1042 k_fib_fib_1483 | b_1863 = (
    k_fib_fib_1483 b_1041
                   0
                   1)
    br_fib_fib_1862 b_1863 a_1040 b_1041 n_1042 k_fib_fib_1483 | (not b_1863) = (
    fib_fib_1039 b_1041
                 (a_1040 + b_1041)
                 (n_1042 - 1)
                 (f_fib_fib_1850 a_1040
                                 b_1041
                                 n_1042
                                 k_fib_fib_1483))
    br_k_main_1864 b_1865 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 | b_1865 = (
    k_main_1571 ())
    br_k_main_1864 b_1865 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 | (
    not b_1865) = (fail_1866 true
                             k_main_1571)
    f_1852 x_1611 | true = ((Random.int 0) (f_1853 x_1611))
    f_1853 x_1611 x_1624 | true = ((Random.int 0) (f_1854 x_1611
                                                          x_1624))
    f_1854 x_1611 x_1624 x_1637 | true = (main_1043 x_1611
                                                    x_1624
                                                    x_1637
                                                    (f_1855 x_1611
                                                            x_1624
                                                            x_1637))
    f_1855 x_1611 x_1624 x_1637 x_1608 | true = end
    f_fib''_1849 n_1037 k_fib''_1427 x1_1440 x2_1440 | true = (k_fib''_1427 x2_1440
                                                                    (x1_1440 + x2_1440))
    f_fib_1847 n_1031 k_fib_1341 x_1344 | true = (fib_1030 (n_1031 - 2)
                                                           (f_fib_1848 n_1031
                                                                    x_1344
                                                                    k_fib_1341))
    f_fib_1848 n_1031 x_1344 k_fib_1341 x_1345 | true = (k_fib_1341 (
                                                                    x_1344 + x_1345))
    f_fib_fib_1850 a_1040 b_1041 n_1042 k_fib_fib_1483 x1_1518 x21_1518 x22_1518 | true = (
    k_fib_fib_1483 x1_1518
                   x22_1518
                   (x21_1518 + x22_1518))
    f_main_1851 a_1044 b_1045 n_1046 k_main_1571 x1_1574 x21_1574 x22_1574 | (
    a_1044 = 0) = (k_main_1589 a_1044
                               b_1045
                               n_1046
                               x1_1574
                               x21_1574
                               x22_1574
                               k_main_1571
                               (b_1045 = 1))
    f_main_1851 a_1044 b_1045 n_1046 k_main_1571 x1_1574 x21_1574 x22_1574 | (
    not (a_1044 = 0)) = (k_main_1589 a_1044
                                     b_1045
                                     n_1046
                                     x1_1574
                                     x21_1574
                                     x22_1574
                                     k_main_1571
                                     false)
    fail_1866 b k | true = (fail ())
    fib''_1036 n_1037 k_fib''_1427 | (n_1037 = 0) = (k_fib''_1427 0
                                                                  0)
    fib''_1036 n_1037 k_fib''_1427 | (not (n_1037 = 0)) = (br_fib''_1860 (
                                                                    n_1037 = 1)
                                                                    n_1037
                                                                    k_fib''_1427)
    fib'_1032 a_1033 b_1034 n_1035 k_fib'_1393 | (n_1035 = 0) = (k_fib'_1393 a_1033)
    fib'_1032 a_1033 b_1034 n_1035 k_fib'_1393 | (not (n_1035 = 0)) = (
    br_fib'_1858 (n_1035 = 1)
                 a_1033
                 b_1034
                 n_1035
                 k_fib'_1393)
    fib_1030 n_1031 k_fib_1341 | (n_1031 = 0) = (k_fib_1341 0)
    fib_1030 n_1031 k_fib_1341 | (not (n_1031 = 0)) = (br_fib_1856 (n_1031 = 1)
                                                                   n_1031
                                                                   k_fib_1341)
    fib_fib_1039 a_1040 b_1041 n_1042 k_fib_fib_1483 | (n_1042 = 0) = (
    k_fib_fib_1483 a_1040
                   0
                   0)
    fib_fib_1039 a_1040 b_1041 n_1042 k_fib_fib_1483 | (not (n_1042 = 0)) = (
    br_fib_fib_1862 (n_1042 = 1)
                    a_1040
                    b_1041
                    n_1042
                    k_fib_fib_1483)
    k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 | b_1605 = (
    br_k_main_1864 (x1_1574 = x22_1574)
                   a_1044
                   b_1045
                   n_1046
                   x1_1574
                   x21_1574
                   x22_1574
                   k_main_1571
                   b_1605)
    k_main_1589 a_1044 b_1045 n_1046 x1_1574 x21_1574 x22_1574 k_main_1571 b_1605 | (
    not b_1605) = (k_main_1571 ())
    main_1043 a_1044 b_1045 n_1046 k_main_1571 | true = (fib_fib_1039
                                                           a_1044
                                                           b_1045
                                                           n_1046
                                                           (f_main_1851 a_1044
                                                                    b_1045
                                                                    n_1046
                                                                    k_main_1571))
    main_1846:X
    br_fib''_1860:bool -> int -> (int -> int -> X) -> X
    br_fib'_1858:bool -> int -> int -> int -> (int -> X) -> X
    br_fib_1856:bool -> int -> (int -> X) -> X
    br_fib_fib_1862:bool -> int -> int -> int -> (int -> int -> int -> X) -> X
    br_k_main_1864:bool -> int -> int -> int -> int -> int -> int -> (unit -> X) -> bool -> X
    f_1852:int -> X
    f_1853:int -> int -> X
    f_1854:int -> int -> int -> X
    f_1855:int -> int -> int -> unit -> X
    f_fib''_1849:int -> (int -> int -> X) -> int -> int -> X
    f_fib_1847:int -> (int -> X) -> int -> X
    f_fib_1848:int -> int -> (int -> X) -> int -> X
    f_fib_fib_1850:int -> int -> int -> (int -> int -> int -> X) -> int -> int -> int -> X
    f_main_1851:int -> int -> int -> (unit -> X) -> int -> int -> int -> X
    fail_1866:bool -> (unit -> X) -> X
    fib''_1036:int -> (int -> int -> X) -> X
    fib'_1032:int -> int -> int -> (int -> X) -> X
    fib_1030:int -> (int -> X) -> X
    fib_fib_1039:int -> int -> int -> (int -> int -> int -> X) -> X
    k_main_1589:int -> int -> int -> int -> int -> int -> (unit -> X) -> bool -> X
    main_1043:int -> int -> int -> (unit -> X) -> X
  inlined functions: br_fib''_1860,br_fib'_1858,br_fib_1856,br_fib_fib_1862,br_k_main_1864,f_1852,f_1853,f_1854,f_1855,f_fib''_1849,f_fib_1847,f_fib_1848,f_fib_fib_1850,f_main_1851,fib''_1036,fib'_1032,k_main_1589,main_1043
  counterexample: 0:0:0:0:0:1:1:1:1:1:0:0:0:0:0:1:0
  error traces:
    [true.nop(<f_1852@162:0> = var398)
     [true.nop((<f_1853@165:0> = <f_1852@162:0>) && (<f_1853@165:1> = var399))
      [true.nop((<f_1854@168:0> = <f_1853@165:0>) && ((<f_1854@168:1> = <f_1853@165:1>) && (<f_1854@168:2> = var400)))
       [true.
        ((<main_1043@170:0> = <f_1854@168:0>) &&
         ((<main_1043@170:1> = <f_1854@168:1>) && (<main_1043@170:2> = <f_1854@168:2>)))
        [true.
         ((<fib_fib_1039@172:0> = <main_1043@170:0>) &&
          ((<fib_fib_1039@172:1> = <main_1043@170:1>) && (<fib_fib_1039@172:2> = <main_1043@170:2>)))
         [(not (<fib_fib_1039@172:2> = 0)).
          ((<br_fib_fib_1862@175:0> = (<fib_fib_1039@172:2> = 1)) &&
           ((<br_fib_fib_1862@175:1> = <fib_fib_1039@172:0>) &&
            ((<br_fib_fib_1862@175:2> = <fib_fib_1039@172:1>) && (<br_fib_fib_1862@175:3> = <fib_fib_1039@172:2>))))
          [(not <br_fib_fib_1862@175:0>).
           ((<fib_fib_1039@178:0> = <br_fib_fib_1862@175:2>) &&
            ((<fib_fib_1039@178:1> = (<br_fib_fib_1862@175:1> + <br_fib_fib_1862@175:2>)) &&
             (<fib_fib_1039@178:2> = (<br_fib_fib_1862@175:3> - 1))))
           [(not (<fib_fib_1039@178:2> = 0)).
            ((<br_fib_fib_1862@181:0> = (<fib_fib_1039@178:2> = 1)) &&
             ((<br_fib_fib_1862@181:1> = <fib_fib_1039@178:0>) &&
              ((<br_fib_fib_1862@181:2> = <fib_fib_1039@178:1>) && (<br_fib_fib_1862@181:3> = <fib_fib_1039@178:2>))))
            [(not <br_fib_fib_1862@181:0>).
             ((<fib_fib_1039@184:0> = <br_fib_fib_1862@181:2>) &&
              ((<fib_fib_1039@184:1> = (<br_fib_fib_1862@181:1> + <br_fib_fib_1862@181:2>)) &&
               (<fib_fib_1039@184:2> = (<br_fib_fib_1862@181:3> - 1))))
             [(not (<fib_fib_1039@184:2> = 0)).
              ((<br_fib_fib_1862@187:0> = (<fib_fib_1039@184:2> = 1)) &&
               ((<br_fib_fib_1862@187:1> = <fib_fib_1039@184:0>) &&
                ((<br_fib_fib_1862@187:2> = <fib_fib_1039@184:1>) && (<br_fib_fib_1862@187:3> = <fib_fib_1039@184:2>))))
              [<br_fib_fib_1862@187:0>.
               ((<<br_fib_fib_1862@187:4>@190:0> = <br_fib_fib_1862@187:2>) &&
                ((<<br_fib_fib_1862@187:4>@190:1> = 0) && (<<br_fib_fib_1862@187:4>@190:2> = 1)))
               [true.
                ((<<fib_fib_1039@184:3>@192:0> = <<br_fib_fib_1862@187:4>@190:0>) &&
                 ((<<fib_fib_1039@184:3>@192:1> = <<br_fib_fib_1862@187:4>@190:1>) &&
                  (<<fib_fib_1039@184:3>@192:2> = <<br_fib_fib_1862@187:4>@190:2>)))
                [true.
                 ((<f_fib_fib_1850@194:0> = <br_fib_fib_1862@181:1>) &&
                  ((<f_fib_fib_1850@194:1> = <br_fib_fib_1862@181:2>) &&
                   ((<f_fib_fib_1850@194:2> = <br_fib_fib_1862@181:3>) &&
                    ((<f_fib_fib_1850@194:4> = <<fib_fib_1039@184:3>@192:0>) &&
                     ((<f_fib_fib_1850@194:5> = <<fib_fib_1039@184:3>@192:1>) &&
                      (<f_fib_fib_1850@194:6> = <<fib_fib_1039@184:3>@192:2>))))))
                 [true.
                  ((<<f_fib_fib_1850@194:3>@196:0> = <f_fib_fib_1850@194:4>) &&
                   ((<<f_fib_fib_1850@194:3>@196:1> = <f_fib_fib_1850@194:6>) &&
                    (<<f_fib_fib_1850@194:3>@196:2> = (<f_fib_fib_1850@194:5> + <f_fib_fib_1850@194:6>))))
                  [true.
                   ((<<br_fib_fib_1862@181:4>@198:0> = <<f_fib_fib_1850@194:3>@196:0>) &&
                    ((<<br_fib_fib_1862@181:4>@198:1> = <<f_fib_fib_1850@194:3>@196:1>) &&
                     (<<br_fib_fib_1862@181:4>@198:2> = <<f_fib_fib_1850@194:3>@196:2>)))
                   [true.
                    ((<<fib_fib_1039@178:3>@200:0> = <<br_fib_fib_1862@181:4>@198:0>) &&
                     ((<<fib_fib_1039@178:3>@200:1> = <<br_fib_fib_1862@181:4>@198:1>) &&
                      (<<fib_fib_1039@178:3>@200:2> = <<br_fib_fib_1862@181:4>@198:2>)))
                    [true.
                     ((<f_fib_fib_1850@202:0> = <br_fib_fib_1862@175:1>) &&
                      ((<f_fib_fib_1850@202:1> = <br_fib_fib_1862@175:2>) &&
                       ((<f_fib_fib_1850@202:2> = <br_fib_fib_1862@175:3>) &&
                        ((<f_fib_fib_1850@202:4> = <<fib_fib_1039@178:3>@200:0>) &&
                         ((<f_fib_fib_1850@202:5> = <<fib_fib_1039@178:3>@200:1>) &&
                          (<f_fib_fib_1850@202:6> = <<fib_fib_1039@178:3>@200:2>))))))
                     [true.
                      ((<<f_fib_fib_1850@202:3>@204:0> = <f_fib_fib_1850@202:4>) &&
                       ((<<f_fib_fib_1850@202:3>@204:1> = <f_fib_fib_1850@202:6>) &&
                        (<<f_fib_fib_1850@202:3>@204:2> = (<f_fib_fib_1850@202:5> + <f_fib_fib_1850@202:6>))))
                      [true.
                       ((<<br_fib_fib_1862@175:4>@206:0> = <<f_fib_fib_1850@202:3>@204:0>) &&
                        ((<<br_fib_fib_1862@175:4>@206:1> = <<f_fib_fib_1850@202:3>@204:1>) &&
                         (<<br_fib_fib_1862@175:4>@206:2> = <<f_fib_fib_1850@202:3>@204:2>)))
                       [true.
                        ((<<fib_fib_1039@172:3>@208:0> = <<br_fib_fib_1862@175:4>@206:0>) &&
                         ((<<fib_fib_1039@172:3>@208:1> = <<br_fib_fib_1862@175:4>@206:1>) &&
                          (<<fib_fib_1039@172:3>@208:2> = <<br_fib_fib_1862@175:4>@206:2>)))
                        [true.
                         ((<f_main_1851@210:0> = <main_1043@170:0>) &&
                          ((<f_main_1851@210:1> = <main_1043@170:1>) &&
                           ((<f_main_1851@210:2> = <main_1043@170:2>) &&
                            ((<f_main_1851@210:4> = <<fib_fib_1039@172:3>@208:0>) &&
                             ((<f_main_1851@210:5> = <<fib_fib_1039@172:3>@208:1>) &&
                              (<f_main_1851@210:6> = <<fib_fib_1039@172:3>@208:2>))))))
                         [(<f_main_1851@210:0> = 0).
                          ((<k_main_1589@213:0> = <f_main_1851@210:0>) &&
                           ((<k_main_1589@213:1> = <f_main_1851@210:1>) &&
                            ((<k_main_1589@213:2> = <f_main_1851@210:2>) &&
                             ((<k_main_1589@213:3> = <f_main_1851@210:4>) &&
                              ((<k_main_1589@213:4> = <f_main_1851@210:5>) &&
                               ((<k_main_1589@213:5> = <f_main_1851@210:6>) &&
                                (<k_main_1589@213:7> = (<f_main_1851@210:1> = 1))))))))
                          [<k_main_1589@213:7>.
                           ((<br_k_main_1864@216:0> = (<k_main_1589@213:3> = <k_main_1589@213:5>)) &&
                            ((<br_k_main_1864@216:1> = <k_main_1589@213:0>) &&
                             ((<br_k_main_1864@216:2> = <k_main_1589@213:1>) &&
                              ((<br_k_main_1864@216:3> = <k_main_1589@213:2>) &&
                               ((<br_k_main_1864@216:4> = <k_main_1589@213:3>) &&
                                ((<br_k_main_1864@216:5> = <k_main_1589@213:4>) &&
                                 ((<br_k_main_1864@216:6> = <k_main_1589@213:5>) &&
                                  (<br_k_main_1864@216:8> = <k_main_1589@213:7>))))))))
                           [(not <br_k_main_1864@216:0>).(<fail_1866@219:0> = true)[true.error
  begin RefTypeInfer.infer_etrs(26335)[2]
    horn clauses:
      P[<fail_1866@219:0>](<fail_1866@219:0>:bool)|- bot
      
      P[<br_k_main_1864@216:8>](false:bool,<br_k_main_1864@216:1>:int,<br_k_main_1864@216:2>:int,<br_k_main_1864@216:3>:int,<br_k_main_1864@216:4>:int,<br_k_main_1864@216:5>:int,<br_k_main_1864@216:6>:int,<br_k_main_1864@216:8>:bool),
      <fail_1866@219:0> |- P[<fail_1866@219:0>](<fail_1866@219:0>:bool)
      
      P[<k_main_1589@213:7>](<br_k_main_1864@216:1>:int,<br_k_main_1864@216:2>:int,<br_k_main_1864@216:3>:int,<br_k_main_1864@216:4>:int,<br_k_main_1864@216:5>:int,<br_k_main_1864@216:6>:int,<br_k_main_1864@216:8>:bool),
      (<br_k_main_1864@216:8> && (<br_k_main_1864@216:0> = (<br_k_main_1864@216:4> = <br_k_main_1864@216:6>)))
      |- P[<br_k_main_1864@216:8>](<br_k_main_1864@216:0>:bool,<br_k_main_1864@216:1>:int,
                                   <br_k_main_1864@216:2>:int,<br_k_main_1864@216:3>:int,
                                   <br_k_main_1864@216:4>:int,<br_k_main_1864@216:5>:int,
                                   <br_k_main_1864@216:6>:int,<br_k_main_1864@216:8>:bool)
      
      P[<f_main_1851@210:6>](<k_main_1589@213:0>:int,<k_main_1589@213:1>:int,<k_main_1589@213:2>:int,<k_main_1589@213:3>:int,<k_main_1589@213:4>:int,<k_main_1589@213:5>:int),
      ((<k_main_1589@213:0> = 0) && (<k_main_1589@213:7> = (<k_main_1589@213:1> = 1)))
      |- P[<k_main_1589@213:7>](<k_main_1589@213:0>:int,<k_main_1589@213:1>:int,
                                <k_main_1589@213:2>:int,<k_main_1589@213:3>:int,
                                <k_main_1589@213:4>:int,<k_main_1589@213:5>:int,
                                <k_main_1589@213:7>:bool)
      
      P[<main_1043@170:2>](<f_main_1851@210:0>:int,<f_main_1851@210:1>:int,<f_main_1851@210:2>:int),
      P[<<fib_fib_1039@172:3>@208:2>](<f_main_1851@210:0>:int,<f_main_1851@210:1>:int,<f_main_1851@210:2>:int,<f_main_1851@210:4>:int,<f_main_1851@210:5>:int,<f_main_1851@210:6>:int)|- P[<f_main_1851@210:6>](
      <f_main_1851@210:0>:int,<f_main_1851@210:1>:int,<f_main_1851@210:2>:int,
      <f_main_1851@210:4>:int,<f_main_1851@210:5>:int,<f_main_1851@210:6>:int)
      
      P[<fib_fib_1039@172:2>](<fib_fib_1039@172:0>:int,<fib_fib_1039@172:1>:int,<fib_fib_1039@172:2>:int),
      P[<<br_fib_fib_1862@175:4>@206:2>]((<fib_fib_1039@172:2> = 1):bool,<fib_fib_1039@172:0>:int,<fib_fib_1039@172:1>:int,<fib_fib_1039@172:2>:int,<<fib_fib_1039@172:3>@208:0>:int,<<fib_fib_1039@172:3>@208:1>:int,<<fib_fib_1039@172:3>@208:2>:int),
      (<fib_fib_1039@172:2> <> 0)
      |- P[<<fib_fib_1039@172:3>@208:2>](<fib_fib_1039@172:0>:int,<fib_fib_1039@172:1>:int,
                                         <fib_fib_1039@172:2>:int,<<fib_fib_1039@172:3>@208:0>:int,
                                         <<fib_fib_1039@172:3>@208:1>:int,
                                         <<fib_fib_1039@172:3>@208:2>:int)
      
      P[<br_fib_fib_1862@175:3>](<br_fib_fib_1862@175:0>:bool,<br_fib_fib_1862@175:1>:int,<br_fib_fib_1862@175:2>:int,<br_fib_fib_1862@175:3>:int),
      P[<<f_fib_fib_1850@202:3>@204:2>](<br_fib_fib_1862@175:1>:int,<br_fib_fib_1862@175:2>:int,<br_fib_fib_1862@175:3>:int,<<br_fib_fib_1862@175:4>@206:0>:int,<<br_fib_fib_1862@175:4>@206:1>:int,<<br_fib_fib_1862@175:4>@206:2>:int),
      (not <br_fib_fib_1862@175:0>)
      |- P[<<br_fib_fib_1862@175:4>@206:2>](<br_fib_fib_1862@175:0>:bool,
                                            <br_fib_fib_1862@175:1>:int,
                                            <br_fib_fib_1862@175:2>:int,
                                            <br_fib_fib_1862@175:3>:int,
                                            <<br_fib_fib_1862@175:4>@206:0>:int,
                                            <<br_fib_fib_1862@175:4>@206:1>:int,
                                            <<br_fib_fib_1862@175:4>@206:2>:int)
      
      P[<f_fib_fib_1850@202:6>](<f_fib_fib_1850@202:0>:int,<f_fib_fib_1850@202:1>:int,<f_fib_fib_1850@202:2>:int,<<f_fib_fib_1850@202:3>@204:0>:int,(
                                <<f_fib_fib_1850@202:3>@204:2> + (-1 * <<f_fib_fib_1850@202:3>@204:1>)):int,<<f_fib_fib_1850@202:3>@204:1>:int)|- P[<<f_fib_fib_1850@202:3>@204:2>](
      <f_fib_fib_1850@202:0>:int,<f_fib_fib_1850@202:1>:int,<f_fib_fib_1850@202:2>:int,
      <<f_fib_fib_1850@202:3>@204:0>:int,<<f_fib_fib_1850@202:3>@204:1>:int,
      <<f_fib_fib_1850@202:3>@204:2>:int)
      
      P[<br_fib_fib_1862@175:3>](false:bool,<f_fib_fib_1850@202:0>:int,<f_fib_fib_1850@202:1>:int,<f_fib_fib_1850@202:2>:int),
      P[<<fib_fib_1039@178:3>@200:2>](<f_fib_fib_1850@202:1>:int,(<f_fib_fib_1850@202:0> + <f_fib_fib_1850@202:1>):int,(
                                      -1 + <f_fib_fib_1850@202:2>):int,<f_fib_fib_1850@202:4>:int,<f_fib_fib_1850@202:5>:int,<f_fib_fib_1850@202:6>:int)|- P[<f_fib_fib_1850@202:6>](
      <f_fib_fib_1850@202:0>:int,<f_fib_fib_1850@202:1>:int,<f_fib_fib_1850@202:2>:int,
      <f_fib_fib_1850@202:4>:int,<f_fib_fib_1850@202:5>:int,<f_fib_fib_1850@202:6>:int)
      
      P[<fib_fib_1039@178:2>](<fib_fib_1039@178:0>:int,<fib_fib_1039@178:1>:int,<fib_fib_1039@178:2>:int),
      P[<<br_fib_fib_1862@181:4>@198:2>]((<fib_fib_1039@178:2> = 1):bool,<fib_fib_1039@178:0>:int,<fib_fib_1039@178:1>:int,<fib_fib_1039@178:2>:int,<<fib_fib_1039@178:3>@200:0>:int,<<fib_fib_1039@178:3>@200:1>:int,<<fib_fib_1039@178:3>@200:2>:int),
      (<fib_fib_1039@178:2> <> 0)
      |- P[<<fib_fib_1039@178:3>@200:2>](<fib_fib_1039@178:0>:int,<fib_fib_1039@178:1>:int,
                                         <fib_fib_1039@178:2>:int,<<fib_fib_1039@178:3>@200:0>:int,
                                         <<fib_fib_1039@178:3>@200:1>:int,
                                         <<fib_fib_1039@178:3>@200:2>:int)
      
      P[<br_fib_fib_1862@181:3>](<br_fib_fib_1862@181:0>:bool,<br_fib_fib_1862@181:1>:int,<br_fib_fib_1862@181:2>:int,<br_fib_fib_1862@181:3>:int),
      P[<<f_fib_fib_1850@194:3>@196:2>](<br_fib_fib_1862@181:1>:int,<br_fib_fib_1862@181:2>:int,<br_fib_fib_1862@181:3>:int,<<br_fib_fib_1862@181:4>@198:0>:int,<<br_fib_fib_1862@181:4>@198:1>:int,<<br_fib_fib_1862@181:4>@198:2>:int),
      (not <br_fib_fib_1862@181:0>)
      |- P[<<br_fib_fib_1862@181:4>@198:2>](<br_fib_fib_1862@181:0>:bool,
                                            <br_fib_fib_1862@181:1>:int,
                                            <br_fib_fib_1862@181:2>:int,
                                            <br_fib_fib_1862@181:3>:int,
                                            <<br_fib_fib_1862@181:4>@198:0>:int,
                                            <<br_fib_fib_1862@181:4>@198:1>:int,
                                            <<br_fib_fib_1862@181:4>@198:2>:int)
      
      P[<f_fib_fib_1850@194:6>](<f_fib_fib_1850@194:0>:int,<f_fib_fib_1850@194:1>:int,<f_fib_fib_1850@194:2>:int,<<f_fib_fib_1850@194:3>@196:0>:int,(
                                <<f_fib_fib_1850@194:3>@196:2> + (-1 * <<f_fib_fib_1850@194:3>@196:1>)):int,<<f_fib_fib_1850@194:3>@196:1>:int)|- P[<<f_fib_fib_1850@194:3>@196:2>](
      <f_fib_fib_1850@194:0>:int,<f_fib_fib_1850@194:1>:int,<f_fib_fib_1850@194:2>:int,
      <<f_fib_fib_1850@194:3>@196:0>:int,<<f_fib_fib_1850@194:3>@196:1>:int,
      <<f_fib_fib_1850@194:3>@196:2>:int)
      
      P[<br_fib_fib_1862@181:3>](false:bool,<f_fib_fib_1850@194:0>:int,<f_fib_fib_1850@194:1>:int,<f_fib_fib_1850@194:2>:int),
      P[<<fib_fib_1039@184:3>@192:2>](<f_fib_fib_1850@194:1>:int,(<f_fib_fib_1850@194:0> + <f_fib_fib_1850@194:1>):int,(
                                      -1 + <f_fib_fib_1850@194:2>):int,<f_fib_fib_1850@194:4>:int,<f_fib_fib_1850@194:5>:int,<f_fib_fib_1850@194:6>:int)|- P[<f_fib_fib_1850@194:6>](
      <f_fib_fib_1850@194:0>:int,<f_fib_fib_1850@194:1>:int,<f_fib_fib_1850@194:2>:int,
      <f_fib_fib_1850@194:4>:int,<f_fib_fib_1850@194:5>:int,<f_fib_fib_1850@194:6>:int)
      
      P[<fib_fib_1039@184:2>](<fib_fib_1039@184:0>:int,<fib_fib_1039@184:1>:int,<fib_fib_1039@184:2>:int),
      P[<<br_fib_fib_1862@187:4>@190:2>]((<fib_fib_1039@184:2> = 1):bool,<fib_fib_1039@184:0>:int,<fib_fib_1039@184:1>:int,<fib_fib_1039@184:2>:int,<<fib_fib_1039@184:3>@192:0>:int,<<fib_fib_1039@184:3>@192:1>:int,<<fib_fib_1039@184:3>@192:2>:int),
      (<fib_fib_1039@184:2> <> 0)
      |- P[<<fib_fib_1039@184:3>@192:2>](<fib_fib_1039@184:0>:int,<fib_fib_1039@184:1>:int,
                                         <fib_fib_1039@184:2>:int,<<fib_fib_1039@184:3>@192:0>:int,
                                         <<fib_fib_1039@184:3>@192:1>:int,
                                         <<fib_fib_1039@184:3>@192:2>:int)
      
      P[<br_fib_fib_1862@187:3>](<br_fib_fib_1862@187:0>:bool,<br_fib_fib_1862@187:1>:int,<br_fib_fib_1862@187:2>:int,<br_fib_fib_1862@187:3>:int),
      ((<<br_fib_fib_1862@187:4>@190:1> = 0) &&
       ((<<br_fib_fib_1862@187:4>@190:2> = 1) &&
        ((<<br_fib_fib_1862@187:4>@190:0> = <br_fib_fib_1862@187:2>) && <br_fib_fib_1862@187:0>)))
      |- P[<<br_fib_f