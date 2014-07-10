MoCHi: Model Checker for Higher-Order Programs
  Build: _327acb0 (after 2014-06-25 18:06:52 +0900)
  TRecS version: 1.33
  OCaml version: 4.01.0
  Command: ./mochi.opt test_relabs/isort-h.ml -tupling -bool-init-empty -disable-rc -color -tupling -gchi -list-option

parsed:
 let is_none_1008 x_1012 = match x_1012 with
                           | (b_1009, _) -> not b_1009 in
 let none_1010 = (false, 0) in
 let some_1011 x_1012 = (true, x_1012) in
 let rec loop_1013 x_1028 = match x_1028 with
                            | () -> loop_1013 () in
 let rec insert_1014 x_1053 =
   match x_1053 with
   | (_ as x_1015) ->
       fun ysys_1016 ->
         (match fst (ysys_1016 (0, 0)) with
          | (b_1017, y_1018) ->
              if not b_1017 then
                let rs_1019 i_1020 = if i_1020 <= 0 then
                                       some_1011 x_1015
                                     else
                                       none_1010 in
                let rsrs_1021 x_1147 =
                  match x_1147 with
                  | (i_1022, j_1023) ->
                      if i_1022 = j_1023 then
                        let r_1024 = rs_1019 i_1022 in
                        (r_1024, r_1024)
                      else
                        (rs_1019 i_1022, rs_1019 j_1023)
                in
                rsrs_1021
              else
                let ys'ys'_1025 x_1206 = match x_1206 with
                                         | (i_1026, j_1027) -> ysys_1016 (i_1026 + 1, j_1027 + 1) in
                if x_1015 < y_1018 then
                  fun x_1234 ->
                    (match x_1234 with
                     | (i_1028, j_1029) ->
                         if i_1028 <= 0 then
                           if j_1029 <= 0 then
                             (some_1011 x_1015, some_1011 x_1015)
                           else
                             (match ysys_1016 (0, j_1029 - 1) with
                              | ((b1_1030, y'_1031), (b2_1032, y''_1033)) ->
                                  if not b2_1032 then
                                    loop_1013 ()
                                  else
                                    if j_1029 - 1 = 0 && y'_1031 <> y''_1033 then
                                      loop_1013 ()
                                    else
                                      if b_1017 <> b1_1030 || y_1018 <> y'_1031 then
                                        loop_1013 ()
                                      else
                                        (some_1011 x_1015, some_1011 y''_1033)
                              | _ -> let u_1350 = {fail} () in
                                     _|_)
                         else
                           if j_1029 <= 0 then
                             (fst (ysys_1016 (i_1028 - 1, 0)), some_1011 x_1015)
                           else
                             ysys_1016 (i_1028 - 1, j_1029 - 1))
                else
                  let ys''ys''_1034 = insert_1014 x_1015 ys'ys'_1025 in
                  fun x_1447 ->
                    (match x_1447 with
                     | (i_1035, j_1036) ->
                         if i_1035 <= 0 then
                           if j_1036 <= 0 then
                             (some_1011 y_1018, some_1011 y_1018)
                           else
                             (match ysys_1016 (0, j_1036 - 1) with
                              | ((b1_1037, y'_1038), (b2_1039, y''_1040)) ->
                                  if not b2_1039 then
                                    loop_1013 ()
                                  else
                                    if j_1036 - 1 = 0 && y'_1038 <> y''_1040 then
                                      loop_1013 ()
                                    else
                                      if b_1017 <> b1_1037 || y_1018 <> y'_1038 then
                                        loop_1013 ()
                                      else
                                        (some_1011 y_1018, some_1011 y''_1040)
                              | _ -> let u_1563 = {fail} () in
                                     _|_)
                         else
                           if j_1036 <= 0 then
                             (fst (ys''ys''_1034 (i_1035, 0)), some_1011 y_1018)
                           else
                             ys''ys''_1034 (i_1035 - 1, j_1036 - 1))
          | _ -> let u_1627 = {fail} () in
                 _|_)
 in
 let rec insertsort_1041 xsxs_1042 =
   if is_none_1008 (fst (xsxs_1042 (0, 0))) then
     let rsrs_1043 i_1044 = (none_1010, none_1010) in
     rsrs_1043
   else
     (match fst (xsxs_1042 (0, 0)) with
      | (_, x_1045) ->
          let xs'xs'_1046 x_1748 = match x_1748 with
                                   | (i_1047, j_1048) -> xsxs_1042 (i_1047 + 1, j_1048 + 1) in
          insert_1014 x_1045 (insertsort_1041 xs'xs'_1046)
      | _ -> let u_1799 = {fail} () in
             _|_)
 in
 let rec make_list_1049 n_1050 =
   if n_1050 = 0 then
     fun i_1051 -> none_1010
   else
     let n_1052 = rand_int () in
     let xs_1053 = make_list_1049 (n_1052 - 1) in
     fun i_1054 -> (if i_1054 = 0 then
                      some_1011 n_1052
                    else
                      xs_1053 (i_1054 - 1))
 in
 let rec check_1055 xsxs_1056 =
   match xsxs_1056 (0, 0) with
   | ((b1_1057, x1_1058), (b2_1059, x2_1060)) ->
       if not b1_1057 then
         true
       else
         if not b2_1059 then
           true
         else
           let xs'xs'_1061 x_1917 = match x_1917 with
                                    | (i_1062, j_1063) -> xsxs_1056 (i_1062 + 1, j_1063 + 1) in
           x1_1058 <= x2_1060 && check_1055 xs'xs'_1061
   | _ -> let u_1949 = {fail} () in
          _|_
 in
 let main_1064 n_1065 =
   let xs_1066 = make_list_1049 n_1065 in
   let xsxs_1067 x_1976 =
     match x_1976 with
     | (i_1068, j_1069) ->
         if i_1068 = j_1069 then
           let r_1070 = xs_1066 i_1068 in
           (r_1070, r_1070)
         else
           (xs_1066 i_1068, xs_1066 j_1069)
   in
   let ysys_1071 = insertsort_1041 xsxs_1067 in
   if check_1055 ysys_1071 then
     ()
   else
     {fail} ()
 in
 ()

spec (abstraction type environment for CPS transformed program):
 insert: (int ->
            (i1_2054:int ->
               j1_2055:int ->
                 (b1_2056:bool ->
                    r1_2057:int ->
                      b2_2058:bool ->
                        int[\r2_2059. ((((not b1_2056 || not b2_2058) || not (i1_2054 <= j1_2055)) || i1_2054 < 0) ||
                                       j1_2055 < 0)
                                      || r1_2057 <= r2_2059] -> X) -> X) ->
              ((i2_2070:int ->
                  j2_2071:int ->
                    (b3_2072:bool ->
                       r3_2073:int ->
                         b4_2074:bool ->
                           int[\r4_2075. ((((not b3_2072 || not b4_2074) || not (i2_2070 <= j2_2071)) || i2_2070 < 0)
                                          || j2_2071 < 0)
                                         || r3_2073 <= r4_2075] -> X) -> X) -> X) -> X)
 rs: (i_2092:int -> (b_2093:bool -> int[\__2094. i_2092 = 0 || not b_2093] -> X) -> X)
 insertsort: ((int -> int -> (bool -> int -> bool -> int -> X) -> X) ->
                ((i2_2117:int ->
                    j2_2118:int ->
                      (b3_2119:bool ->
                         r3_2120:int ->
                           b4_2121:bool ->
                             int[\r4_2122. ((((not b3_2119 || not b4_2121) || not (i2_2117 <= j2_2118)) || i2_2117 < 0)
                                            || j2_2118 < 0)
                                           || r3_2120 <= r4_2122] -> X) -> X) -> X) -> X)
 check: ((i_2138:int ->
            j_2139:int ->
              (b1_2140:bool ->
                 r1_2141:int ->
                   b2_2142:bool ->
                     int[\r2_2143. ((((not b1_2140 || not b2_2142) || not (i_2138 <= j_2139)) || i_2138 < 0) ||
                                    j_2139 < 0)
                                   || r1_2141 <= r2_2143] -> X) -> X) -> 
           (bool[\x_14_2154. x_14_2154] -> X) -> X)
 
spec (force inlined functions):
 none
 some
 
set_target:
 let is_none_1008 (x_1012:(bool * !!!)) = match x_1012 with
                                          | (b_1009, _) -> not b_1009 in
 let none_1010 = (false, 0) in
 let some_1011 (x_1012:!!!) = (true, x_1012) in
 let rec loop_1013 (x_1028:unit) = match x_1028 with
                                   | () -> loop_1013 () in
 let rec insert_1014 (x_1053:int) =
   match x_1053 with
   | (_ as x_1015) ->
       fun (ysys_1016:((int * int) -> ((bool * int) * (bool * int)))) ->
         (match fst (ysys_1016 (0, 0)) with
          | (b_1017, y_1018) ->
              if not b_1017 then
                let rs_1019 (i_1020:int) = if i_1020 <= 0 then
                                             some_1011 x_1015
                                           else
                                             none_1010 in
                let rsrs_1021 (x_1147:(int * int)) =
                  match x_1147 with
                  | (i_1022, j_1023) ->
                      if i_1022 = j_1023 then
                        let r_1024 = rs_1019 i_1022 in
                        (r_1024, r_1024)
                      else
                        (rs_1019 i_1022, rs_1019 j_1023)
                in
                rsrs_1021
              else
                let ys'ys'_1025 (x_1206:(int * int)) =
                  match x_1206 with
                  | (i_1026, j_1027) -> ysys_1016 (i_1026 + 1, j_1027 + 1)
                in
                if x_1015 < y_1018 then
                  fun (x_1234:(int * int)) ->
                    (match x_1234 with
                     | (i_1028, j_1029) ->
                         if i_1028 <= 0 then
                           if j_1029 <= 0 then
                             (some_1011 x_1015, some_1011 x_1015)
                           else
                             (match ysys_1016 (0, j_1029 - 1) with
                              | ((b1_1030, y'_1031), (b2_1032, y''_1033)) ->
                                  if not b2_1032 then
                                    loop_1013 ()
                                  else
                                    if j_1029 - 1 = 0 && y'_1031 <> y''_1033 then
                                      loop_1013 ()
                                    else
                                      if b_1017 <> b1_1030 || y_1018 <> y'_1031 then
                                        loop_1013 ()
                                      else
                                        (some_1011 x_1015, some_1011 y''_1033)
                              | _ -> let u_1350 = {fail} () in
                                     _|_)
                         else
                           if j_1029 <= 0 then
                             (fst (ysys_1016 (i_1028 - 1, 0)), some_1011 x_1015)
                           else
                             ysys_1016 (i_1028 - 1, j_1029 - 1))
                else
                  let ys''ys''_1034 = insert_1014 x_1015 ys'ys'_1025 in
                  fun (x_1447:(int * int)) ->
                    (match x_1447 with
                     | (i_1035, j_1036) ->
                         if i_1035 <= 0 then
                           if j_1036 <= 0 then
                             (some_1011 y_1018, some_1011 y_1018)
                           else
                             (match ysys_1016 (0, j_1036 - 1) with
                              | ((b1_1037, y'_1038), (b2_1039, y''_1040)) ->
                                  if not b2_1039 then
                                    loop_1013 ()
                                  else
                                    if j_1036 - 1 = 0 && y'_1038 <> y''_1040 then
                                      loop_1013 ()
                                    else
                                      if b_1017 <> b1_1037 || y_1018 <> y'_1038 then
                                        loop_1013 ()
                                      else
                                        (some_1011 y_1018, some_1011 y''_1040)
                              | _ -> let u_1563 = {fail} () in
                                     _|_)
                         else
                           if j_1036 <= 0 then
                             (fst (ys''ys''_1034 (i_1035, 0)), some_1011 y_1018)
                           else
                             ys''ys''_1034 (i_1035 - 1, j_1036 - 1))
          | _ -> let u_1627 = {fail} () in
                 _|_)
 in
 let rec insertsort_1041 (xsxs_1042:((int * int) -> ((bool * int) * !!!))) =
   if is_none_1008 (fst (xsxs_1042 (0, 0))) then
     let rsrs_1043 (i_1044:!!!) = (none_1010, none_1010) in
     rsrs_1043
   else
     (match fst (xsxs_1042 (0, 0)) with
      | (_, x_1045) ->
          let xs'xs'_1046 (x_1748:(int * int)) =
            match x_1748 with
            | (i_1047, j_1048) -> xsxs_1042 (i_1047 + 1, j_1048 + 1)
          in
          insert_1014 x_1045 (insertsort_1041 xs'xs'_1046)
      | _ -> let u_1799 = {fail} () in
             _|_)
 in
 let rec make_list_1049 (n_1050:int) =
   if n_1050 = 0 then
     fun (i_1051:int) -> none_1010
   else
     let n_1052 = rand_int () in
     let xs_1053 = make_list_1049 (n_1052 - 1) in
     fun (i_1054:int) -> (if i_1054 = 0 then
                            some_1011 n_1052
                          else
                            xs_1053 (i_1054 - 1))
 in
 let rec check_1055 (xsxs_1056:((int * int) -> ((bool * !!!) * (bool * !!!)))) =
   match xsxs_1056 (0, 0) with
   | ((b1_1057, x1_1058), (b2_1059, x2_1060)) ->
       if not b1_1057 then
         true
       else
         if not b2_1059 then
           true
         else
           let xs'xs'_1061 (x_1917:(int * int)) =
             match x_1917 with
             | (i_1062, j_1063) -> xsxs_1056 (i_1062 + 1, j_1063 + 1)
           in
           x1_1058 <= x2_1060 && check_1055 xs'xs'_1061
   | _ -> let u_1949 = {fail} () in
          _|_
 in
 let main_1064 (n_1065:int) =
   let xs_1066 = make_list_1049 n_1065 in
   let xsxs_1067 (x_1976:(int * int)) =
     match x_1976 with
     | (i_1068, j_1069) ->
         if i_1068 = j_1069 then
           let r_1070 = xs_1066 i_1068 in
           (r_1070, r_1070)
         else
           (xs_1066 i_1068, xs_1066 j_1069)
   in
   let ysys_1071 = insertsort_1041 xsxs_1067 in
   if check_1055 ysys_1071 then
     ()
   else
     {fail} ()
 in
 let main_2163 = let arg1_2161 = rand_int () in
                 main_1064 arg1_2161 in
 ()

copy_poly:
 let is_none_2185 (x_1012:(bool * int)) = match x_1012 with
                                          | (b_1009, _) -> not b_1009 in
 let none_1010 = (false, 0) in
 let some_2173 (x_1012:int) = (true, x_1012) in
 let rec loop_2167 (x_1028:unit) = match x_1028 with
                                   | () -> loop_2167 () in
 let rec insert_1014 (x_1053:int) =
   match x_1053 with
   | (_ as x_1015) ->
       fun (ysys_1016:((int * int) -> ((bool * int) * (bool * int)))) ->
         (match fst (ysys_1016 (0, 0)) with
          | (b_1017, y_1018) ->
              if not b_1017 then
                let rs_1019 (i_1020:int) = if i_1020 <= 0 then
                                             some_2173 x_1015
                                           else
                                             none_1010 in
                let rsrs_1021 (x_1147:(int * int)) =
                  match x_1147 with
                  | (i_1022, j_1023) ->
                      if i_1022 = j_1023 then
                        let r_1024 = rs_1019 i_1022 in
                        (r_1024, r_1024)
                      else
                        (rs_1019 i_1022, rs_1019 j_1023)
                in
                rsrs_1021
              else
                let ys'ys'_1025 (x_1206:(int * int)) =
                  match x_1206 with
                  | (i_1026, j_1027) -> ysys_1016 (i_1026 + 1, j_1027 + 1)
                in
                if x_1015 < y_1018 then
                  fun (x_1234:(int * int)) ->
                    (match x_1234 with
                     | (i_1028, j_1029) ->
                         if i_1028 <= 0 then
                           if j_1029 <= 0 then
                             (some_2173 x_1015, some_2173 x_1015)
                           else
                             (match ysys_1016 (0, j_1029 - 1) with
                              | ((b1_1030, y'_1031), (b2_1032, y''_1033)) ->
                                  if not b2_1032 then
                                    loop_2167 ()
                                  else
                                    if j_1029 - 1 = 0 && y'_1031 <> y''_1033 then
                                      loop_2167 ()
                                    else
                                      if b_1017 <> b1_1030 || y_1018 <> y'_1031 then
                                        loop_2167 ()
                                      else
                                        (some_2173 x_1015, some_2173 y''_1033)
                              | _ -> let u_1350 = {fail} () in
                                     _|_)
                         else
                           if j_1029 <= 0 then
                             (fst (ysys_1016 (i_1028 - 1, 0)), some_2173 x_1015)
                           else
                             ysys_1016 (i_1028 - 1, j_1029 - 1))
                else
                  let ys''ys''_1034 = insert_1014 x_1015 ys'ys'_1025 in
                  fun (x_1447:(int * int)) ->
                    (match x_1447 with
                     | (i_1035, j_1036) ->
                         if i_1035 <= 0 then
                           if j_1036 <= 0 then
                             (some_2173 y_1018, some_2173 y_1018)
                           else
                             (match ysys_1016 (0, j_1036 - 1) with
                              | ((b1_1037, y'_1038), (b2_1039, y''_1040)) ->
                                  if not b2_1039 then
                                    loop_2167 ()
                                  else
                                    if j_1036 - 1 = 0 && y'_1038 <> y''_1040 then
                                      loop_2167 ()
                                    else
                                      if b_1017 <> b1_1037 || y_1018 <> y'_1038 then
                                        loop_2167 ()
                                      else
                                        (some_2173 y_1018, some_2173 y''_1040)
                              | _ -> let u_1563 = {fail} () in
                                     _|_)
                         else
                           if j_1036 <= 0 then
                             (fst (ys''ys''_1034 (i_1035, 0)), some_2173 y_1018)
                           else
                             ys''ys''_1034 (i_1035 - 1, j_1036 - 1))
          | _ -> let u_1627 = {fail} () in
                 _|_)
 in
 let rec insertsort_2165 (xsxs_1042:((int * int) -> ((bool * int) * (bool * int)))) =
   if is_none_2185 (fst (xsxs_1042 (0, 0))) then
     let rsrs_2166 (i_1044:(int * int)) = (none_1010, none_1010) in
     rsrs_2166
   else
     (match fst (xsxs_1042 (0, 0)) with
      | (_, x_1045) ->
          let xs'xs'_1046 (x_1748:(int * int)) =
            match x_1748 with
            | (i_1047, j_1048) -> xsxs_1042 (i_1047 + 1, j_1048 + 1)
          in
          insert_1014 x_1045 (insertsort_2165 xs'xs'_1046)
      | _ -> let u_1799 = {fail} () in
             _|_)
 in
 let rec make_list_1049 (n_1050:int) =
   if n_1050 = 0 then
     fun (i_1051:int) -> none_1010
   else
     let n_1052 = rand_int () in
     let xs_1053 = make_list_1049 (n_1052 - 1) in
     fun (i_1054:int) -> (if i_1054 = 0 then
                            some_2173 n_1052
                          else
                            xs_1053 (i_1054 - 1))
 in
 let rec check_2164 (xsxs_1056:((int * int) -> ((bool * int) * (bool * int)))) =
   match xsxs_1056 (0, 0) with
   | ((b1_1057, x1_1058), (b2_1059, x2_1060)) ->
       if not b1_1057 then
         true
       else
         if not b2_1059 then
           true
         else
           let xs'xs'_1061 (x_1917:(int * int)) =
             match x_1917 with
             | (i_1062, j_1063) -> xsxs_1056 (i_1062 + 1, j_1063 + 1)
           in
           x1_1058 <= x2_1060 && check_2164 xs'xs'_1061
   | _ -> let u_1949 = {fail} () in
          _|_
 in
 let main_1064 (n_1065:int) =
   let xs_1066 = make_list_1049 n_1065 in
   let xsxs_1067 (x_1976:(int * int)) =
     match x_1976 with
     | (i_1068, j_1069) ->
         if i_1068 = j_1069 then
           let r_1070 = xs_1066 i_1068 in
           (r_1070, r_1070)
         else
           (xs_1066 i_1068, xs_1066 j_1069)
   in
   let ysys_1071 = insertsort_2165 xsxs_1067 in
   if check_2164 ysys_1071 then
     ()
   else
     {fail} ()
 in
 let main_2163 = let arg1_2161 = rand_int () in
                 main_1064 arg1_2161 in
 ()

spec (abstraction type environment for CPS transformed program):
 insert_1014: (int ->
                 (i1_2054:int ->
                    j1_2055:int ->
                      (b1_2056:bool ->
                         r1_2057:int ->
                           b2_2058:bool ->
                             int[\r2_2059. ((((not b1_2056 || not b2_2058) || not (i1_2054 <= j1_2055)) || i1_2054 < 0)
                                            || j1_2055 < 0)
                                           || r1_2057 <= r2_2059] -> X) -> X) ->
                   ((i2_2070:int ->
                       j2_2071:int ->
                         (b3_2072:bool ->
                            r3_2073:int ->
                              b4_2074:bool ->
                                int[\r4_2075. ((((not b3_2072 || not b4_2074) || not (i2_2070 <= j2_2071)) ||
                                                i2_2070 < 0)
                                               || j2_2071 < 0)
                                              || r3_2073 <= r4_2075] -> X) -> X) -> X) -> X)
 rs_1019: (i_2092:int -> (b_2093:bool -> int[\__2094. i_2092 = 0 || not b_2093] -> X) -> X)
 insertsort_2165: ((int -> int -> (bool -> int -> bool -> int -> X) -> X) ->
                     ((i2_2117:int ->
                         j2_2118:int ->
                           (b3_2119:bool ->
                              r3_2120:int ->
                                b4_2121:bool ->
                                  int[\r4_2122. ((((not b3_2119 || not b4_2121) || not (i2_2117 <= j2_2118)) ||
                                                  i2_2117 < 0)
                                                 || j2_2118 < 0)
                                                || r3_2120 <= r4_2122] -> X) -> X) -> X) -> X)
 check_2164: ((i_2138:int ->
                 j_2139:int ->
                   (b1_2140:bool ->
                      r1_2141:int ->
                        b2_2142:bool ->
                          int[\r2_2143. ((((not b1_2140 || not b2_2142) || not (i_2138 <= j_2139)) || i_2138 < 0) ||
                                         j_2139 < 0)
                                        || r1_2141 <= r2_2143] -> X) -> X) -> 
                (bool[\x_14_2154. x_14_2154] -> X) -> X)
 
spec (force inlined functions):
 none_1010
 some_2173
 
abst_recdata:
 let is_none_2185 (x_1012:(bool * int)) = match x_1012 with
                                          | (b_1009, _) -> not b_1009 in
 let none_1010 = (false, 0) in
 let some_2173 (x_1012:int) = (true, x_1012) in
 let rec loop_2167 (x_1028:unit) = loop_2167 () in
 let rec insert_1014 (x_1053:int) =
   match x_1053 with
   | (_ as x_1015) ->
       fun (ysys_1016:((int * int) -> ((bool * int) * (bool * int)))) ->
         (match fst (ysys_1016 (0, 0)) with
          | (b_1017, y_1018) ->
              if not b_1017 then
                let rs_1019 (i_1020:int) = if i_1020 <= 0 then
                                             some_2173 x_1015
                                           else
                                             none_1010 in
                let rsrs_1021 (x_1147:(int * int)) =
                  match x_1147 with
                  | (i_1022, j_1023) ->
                      if i_1022 = j_1023 then
                        let r_1024 = rs_1019 i_1022 in
                        (r_1024, r_1024)
                      else
                        (rs_1019 i_1022, rs_1019 j_1023)
                in
                rsrs_1021
              else
                let ys'ys'_1025 (x_1206:(int * int)) =
                  match x_1206 with
                  | (i_1026, j_1027) -> ysys_1016 (i_1026 + 1, j_1027 + 1)
                in
                if x_1015 < y_1018 then
                  fun (x_1234:(int * int)) ->
                    (match x_1234 with
                     | (i_1028, j_1029) ->
                         if i_1028 <= 0 then
                           if j_1029 <= 0 then
                             (some_2173 x_1015, some_2173 x_1015)
                           else
                             (match ysys_1016 (0, j_1029 - 1) with
                              | ((b1_1030, y'_1031), (b2_1032, y''_1033)) ->
                                  if not b2_1032 then
                                    loop_2167 ()
                                  else
                                    if j_1029 - 1 = 0 && y'_1031 <> y''_1033 then
                                      loop_2167 ()
                                    else
                                      if b_1017 <> b1_1030 || y_1018 <> y'_1031 then
                                        loop_2167 ()
                                      else
                                        (some_2173 x_1015, some_2173 y''_1033)
                              | _ -> let u_1350 = {fail} () in
                                     _|_)
                         else
                           if j_1029 <= 0 then
                             (fst (ysys_1016 (i_1028 - 1, 0)), some_2173 x_1015)
                           else
                             ysys_1016 (i_1028 - 1, j_1029 - 1))
                else
                  let ys''ys''_1034 = insert_1014 x_1015 ys'ys'_1025 in
                  fun (x_1447:(int * int)) ->
                    (match x_1447 with
                     | (i_1035, j_1036) ->
                         if i_1035 <= 0 then
                           if j_1036 <= 0 then
                             (some_2173 y_1018, some_2173 y_1018)
                           else
                             (match ysys_1016 (0, j_1036 - 1) with
                              | ((b1_1037, y'_1038), (b2_1039, y''_1040)) ->
                                  if not b2_1039 then
                                    loop_2167 ()
                                  else
                                    if j_1036 - 1 = 0 && y'_1038 <> y''_1040 then
                                      loop_2167 ()
                                    else
                                      if b_1017 <> b1_1037 || y_1018 <> y'_1038 then
                                        loop_2167 ()
                                      else
                                        (some_2173 y_1018, some_2173 y''_1040)
                              | _ -> let u_1563 = {fail} () in
                                     _|_)
                         else
                           if j_1036 <= 0 then
                             (fst (ys''ys''_1034 (i_1035, 0)), some_2173 y_1018)
                           else
                             ys''ys''_1034 (i_1035 - 1, j_1036 - 1))
          | _ -> let u_1627 = {fail} () in
                 _|_)
 in
 let rec insertsort_2165 (xsxs_1042:((int * int) -> ((bool * int) * (bool * int)))) =
   if is_none_2185 (fst (xsxs_1042 (0, 0))) then
     let rsrs_2166 (i_1044:(int * int)) = (none_1010, none_1010) in
     rsrs_2166
   else
     (match fst (xsxs_1042 (0, 0)) with
      | (_, x_1045) ->
          let xs'xs'_1046 (x_1748:(int * int)) =
            match x_1748 with
            | (i_1047, j_1048) -> xsxs_1042 (i_1047 + 1, j_1048 + 1)
          in
          insert_1014 x_1045 (insertsort_2165 xs'xs'_1046)
      | _ -> let u_1799 = {fail} () in
             _|_)
 in
 let rec make_list_1049 (n_1050:int) =
   if n_1050 = 0 then
     fun (i_1051:int) -> none_1010
   else
     let n_1052 = rand_int () in
     let xs_1053 = make_list_1049 (n_1052 - 1) in
     fun (i_1054:int) -> (if i_1054 = 0 then
                            some_2173 n_1052
                          else
                            xs_1053 (i_1054 - 1))
 in
 let rec check_2164 (xsxs_1056:((int * int) -> ((bool * int) * (bool * int)))) =
   match xsxs_1056 (0, 0) with
   | ((b1_1057, x1_1058), (b2_1059, x2_1060)) ->
       if not b1_1057 then
         true
       else
         if not b2_1059 then
           true
         else
           let xs'xs'_1061 (x_1917:(int * int)) =
             match x_1917 with
             | (i_1062, j_1063) -> xsxs_1056 (i_1062 + 1, j_1063 + 1)
           in
           x1_1058 <= x2_1060 && check_2164 xs'xs'_1061
   | _ -> let u_1949 = {fail} () in
          _|_
 in
 let main_1064 (n_1065:int) =
   let xs_1066 = make_list_1049 n_1065 in
   let xsxs_1067 (x_1976:(int * int)) =
     match x_1976 with
     | (i_1068, j_1069) ->
         if i_1068 = j_1069 then
           let r_1070 = xs_1066 i_1068 in
           (r_1070, r_1070)
         else
           (xs_1066 i_1068, xs_1066 j_1069)
   in
   let ysys_1071 = insertsort_2165 xsxs_1067 in
   if check_2164 ysys_1071 then
     ()
   else
     {fail} ()
 in
 let main_2163 = let arg1_2161 = rand_int () in
                 main_1064 arg1_2161 in
 ()

encode_list:
 let is_none_2185 (x_1012:(bool * int)) = let b_1009 = fst x_1012 in
                                          not b_1009 in
 let none_1010 = (false, 0) in
 let some_2173 (x_1012:int) = (true, x_1012) in
 let rec loop_2167 (x_1028:unit) = loop_2167 () in
 let rec insert_1014 (x_1053:int) =
   fun (ysys_1016:((int * int) -> ((bool * int) * (bool * int)))) ->
     (let xs_2203 = fst (ysys_1016 (0, 0)) in
      let y_1018 = snd xs_2203 in
      let b_1017 = fst xs_2203 in
      if not b_1017 then
        let rs_1019 (i_1020:int) = if i_1020 <= 0 then
                                     some_2173 x_1053
                                   else
                                     none_1010 in
        let rsrs_1021 (x_1147:(int * int)) =
          let j_1023 = snd x_1147 in
          let i_1022 = fst x_1147 in
          if i_1022 = j_1023 then
            let r_1024 = rs_1019 i_1022 in
            (r_1024, r_1024)
          else
            (rs_1019 i_1022, rs_1019 j_1023)
        in
        rsrs_1021
      else
        let ys'ys'_1025 (x_1206:(int * int)) =
          let j_1027 = snd x_1206 in
          let i_1026 = fst x_1206 in
          ysys_1016 (i_1026 + 1, j_1027 + 1)
        in
        if x_1053 < y_1018 then
          fun (x_1234:(int * int)) ->
            (let j_1029 = snd x_1234 in
             let i_1028 = fst x_1234 in
             if i_1028 <= 0 then
               if j_1029 <= 0 then
                 (some_2173 x_1053, some_2173 x_1053)
               else
                 let xs_2208 = ysys_1016 (0, j_1029 - 1) in
                 let y''_1033 = snd (snd xs_2208) in
                 let b2_1032 = fst (snd xs_2208) in
                 let b1_1030 = fst (fst xs_2208) in
                 let y'_1031 = snd (fst xs_2208) in
                 if not b2_1032 then
                   loop_2167 ()
                 else
                   if j_1029 - 1 = 0 && y'_1031 <> y''_1033 then
                     loop_2167 ()
                   else
                     if b_1017 <> b1_1030 || y_1018 <> y'_1031 then
                       loop_2167 ()
                     else
                       (some_2173 x_1053, some_2173 y''_1033)
             else
               if j_1029 <= 0 then
                 (fst (ysys_1016 (i_1028 - 1, 0)), some_2173 x_1053)
               else
                 ysys_1016 (i_1028 - 1, j_1029 - 1))
        else
          let ys''ys''_1034 = insert_1014 x_1053 ys'ys'_1025 in
          fun (x_1447:(int * int)) ->
            (let j_1036 = snd x_1447 in
             let i_1035 = fst x_1447 in
             if i_1035 <= 0 then
               if j_1036 <= 0 then
                 (some_2173 y_1018, some_2173 y_1018)
               else
                 let xs_2206 = ysys_1016 (0, j_1036 - 1) in
                 let y''_1040 = snd (snd xs_2206) in
                 let b2_1039 = fst (snd xs_2206) in
                 let b1_1037 = fst (fst xs_2206) in
                 let y'_1038 = snd (fst xs_2206) in
                 if not b2_1039 then
                   loop_2167 ()
                 else
                   if j_1036 - 1 = 0 && y'_1038 <> y''_1040 then
                     loop_2167 ()
                   else
                     if b_1017 <> b1_1037 || y_1018 <> y'_1038 then
                       loop_2167 ()
                     else
                       (some_2173 y_1018, some_2173 y''_1040)
             else
               if j_1036 <= 0 then
                 (fst (ys''ys''_1034 (i_1035, 0)), some_2173 y_1018)
               else
                 ys''ys''_1034 (i_1035 - 1, j_1036 - 1)))
 in
 let rec insertsort_2165 (xsxs_1042:((int * int) -> ((bool * int) * (bool * int)))) =
   if is_none_2185 (fst (xsxs_1042 (0, 0))) then
     let rsrs_2166 (i_1044:(int * int)) = (none_1010, none_1010) in
     rsrs_2166
   else
     let xs_2210 = fst (xsxs_1042 (0, 0)) in
     let x_1045 = snd xs_2210 in
     let xs'xs'_1046 (x_1748:(int * int)) =
       let j_1048 = snd x_1748 in
       let i_1047 = fst x_1748 in
       xsxs_1042 (i_1047 + 1, j_1048 + 1)
     in
     insert_1014 x_1045 (insertsort_2165 xs'xs'_1046)
 in
 let rec make_list_1049 (n_1050:int) =
   if n_1050 = 0 then
     fun (i_1051:int) -> none_1010
   else
     let n_1052 = rand_int () in
     let xs_1053 = make_list_1049 (n_1052 - 1) in
     fun (i_1054:int) -> (if i_1054 = 0 then
                            some_2173 n_1052
                          else
                            xs_1053 (i_1054 - 1))
 in
 let rec check_2164 (xsxs_1056:((int * int) -> ((bool * int) * (bool * int)))) =
   let xs_2212 = xsxs_1056 (0, 0) in
   let x2_1060 = snd (snd xs_2212) in
   let b2_1059 = fst (snd xs_2212) in
   let b1_1057 = fst (fst xs_2212) in
   let x1_1058 = snd (fst xs_2212) in
   if not b1_1057 then
     true
   else
     if not b2_1059 then
       true
     else
       let xs'xs'_1061 (x_1917:(int * int)) =
         let j_1063 = snd x_1917 in
         let i_1062 = fst x_1917 in
         xsxs_1056 (i_1062 + 1, j_1063 + 1)
       in
       x1_1058 <= x2_1060 && check_2164 xs'xs'_1061
 in
 let main_1064 (n_1065:int) =
   let xs_1066 = make_list_1049 n_1065 in
   let xsxs_1067 (x_1976:(int * int)) =
     let j_1069 = snd x_1976 in
     let i_1068 = fst x_1976 in
     if i_1068 = j_1069 then
       let r_1070 = xs_1066 i_1068 in
       (r_1070, r_1070)
     else
       (xs_1066 i_1068, xs_1066 j_1069)
   in
   let ysys_1071 = insertsort_2165 xsxs_1067 in
   if check_2164 ysys_1071 then
     ()
   else
     {fail} ()
 in
 let main_2163 = let arg1_2161 = rand_int () in
                 main_1064 arg1_2161 in
 ()

inlined:
 let is_none_2185 (x_1012:(bool * int)) = not fst x_1012 in
 let rec loop_2167 (x_1028:unit) = loop_2167 () in
 let rec insert_1014 (x_1053:int) =
   fun (ysys_1016:((int * int) -> ((bool * int) * (bool * int)))) ->
     (let xs_2203 = fst (ysys_1016 (0, 0)) in
      if not fst xs_2203 then
        let rs_1019 (i_1020:int) = if i_1020 <= 0 then
                                     (true, x_1053)
                                   else
                                     (false, 0) in
        let rsrs_1021 (x_1147:(int * int)) =
          if fst x_1147 = snd x_1147 then
            let r_1024 = rs_1019 (fst x_1147) in
            (r_1024, r_1024)
          else
            (rs_1019 (fst x_1147), rs_1019 (snd x_1147))
        in
        rsrs_1021
      else
        let ys'ys'_1025 (x_1206:(int * int)) = ysys_1016 (fst x_1206 + 1, snd x_1206 + 1) in
        if x_1053 < snd xs_2203 then
          fun (x_1234:(int * int)) ->
            (if fst x_1234 <= 0 then
               if snd x_1234 <= 0 then
                 ((true, x_1053), (true, x_1053))
               else
                 let xs_2208 = ysys_1016 (0, snd x_1234 - 1) in
                 let y''_1033 = snd (snd xs_2208) in
                 let b2_1032 = fst (snd xs_2208) in
                 let b1_1030 = fst (fst xs_2208) in
                 let y'_1031 = snd (fst xs_2208) in
                 if not b2_1032 then
                   loop_2167 ()
                 else
                   if snd x_1234 - 1 = 0 && y'_1031 <> y''_1033 then
                     loop_2167 ()
                   else
                     if fst xs_2203 <> b1_1030 || snd xs_2203 <> y'_1031 then
                       loop_2167 ()
                     else
                       ((true, x_1053), (true, y''_1033))
             else
               if snd x_1234 <= 0 then
                 (fst (ysys_1016 (fst x_1234 - 1, 0)), (true, x_1053))
               else
                 ysys_1016 (fst x_1234 - 1, snd x_1234 - 1))
        else
          let ys''ys''_1034 = insert_1014 x_1053 ys'ys'_1025 in
          fun (x_1447:(int * int)) ->
            (if fst x_1447 <= 0 then
               if snd x_1447 <= 0 then
                 ((let arg_2220 = snd xs_2203 in
                   (true, arg_2220)), (let arg_2219 = snd xs_2203 in
                                       (true, arg_2219)))
               else
                 let xs_2206 = ysys_1016 (0, snd x_1447 - 1) in
                 let y''_1040 = snd (snd xs_2206) in
                 let b2_1039 = fst (snd xs_2206) in
                 let b1_1037 = fst (fst xs_2206) in
                 let y'_1038 = snd (fst xs_2206) in
                 if not b2_1039 then
                   loop_2167 ()
                 else
                   if snd x_1447 - 1 = 0 && y'_1038 <> y''_1040 then
                     loop_2167 ()
                   else
                     if fst xs_2203 <> b1_1037 || snd xs_2203 <> y'_1038 then
                       loop_2167 ()
                     else
                       ((let arg_2218 = snd xs_2203 in
                         (true, arg_2218)), (true, y''_1040))
             else
               if snd x_1447 <= 0 then
                 (fst (ys''ys''_1034 (fst x_1447, 0)), (let arg_2217 = snd xs_2203 in
                                                        (true, arg_2217)))
               else
                 ys''ys''_1034 (fst x_1447 - 1, snd x_1447 - 1)))
 in
 let rec insertsort_2165 (xsxs_1042:((int * int) -> ((bool * int) * (bool * int)))) =
   if is_none_2185 (fst (xsxs_1042 (0, 0))) then
     let rsrs_2166 (i_1044:(int * int)) = ((false, 0), (false, 0)) in
     rsrs_2166
   else
     let xs_2210 = fst (xsxs_1042 (0, 0)) in
     let xs'xs'_1046 (x_1748:(int * int)) = xsxs_1042 (fst x_1748 + 1, snd x_1748 + 1) in
     insert_1014 (snd xs_2210) (insertsort_2165 xs'xs'_1046)
 in
 let rec make_list_1049 (n_1050:int) =
   if n_1050 = 0 then
     fun (i_1051:int) -> (false, 0)
   else
     let n_1052 = rand_int () in
     let xs_1053 = make_list_1049 (n_1052 - 1) in
     fun (i_1054:int) -> (if i_1054 = 0 then
                            (true, n_1052)
                          else
                            xs_1053 (i_1054 - 1))
 in
 let rec check_2164 (xsxs_1056:((int * int) -> ((bool * int) * (bool * int)))) =
   let xs_2212 = xsxs_1056 (0, 0) in
   let x2_1060 = snd (snd xs_2212) in
   let b2_1059 = fst (snd xs_2212) in
   let b1_1057 = fst (fst xs_2212) in
   let x1_1058 = snd (fst xs_2212) in
   if not b1_1057 then
     true
   else
     if not b2_1059 then
       true
     else
       let xs'xs'_1061 (x_1917:(int * int)) = xsxs_1056 (fst x_1917 + 1, snd x_1917 + 1) in
       x1_1058 <= x2_1060 && check_2164 xs'xs'_1061
 in
 let main_1064 (n_1065:int) =
   let xs_1066 = make_list_1049 n_1065 in
   let xsxs_1067 (x_1976:(int * int)) =
     if fst x_1976 = snd x_1976 then
       let r_1070 = xs_1066 (fst x_1976) in
       (r_1070, r_1070)
     else
       (xs_1066 (fst x_1976), xs_1066 (snd x_1976))
   in
   let ysys_1071 = insertsort_2165 xsxs_1067 in
   if check_2164 ysys_1071 then
     ()
   else
     {fail} ()
 in
 let main_2163 = let arg1_2161 = rand_int () in
                 main_1064 arg1_2161 in
 ()

CPS:
 let is_none_2185 (x_1012:(bool * int)) (k_is_none_2230:(bool -> X)) = k_is_none_2230 (not fst x_1012) in
 let rec loop_2167 (x_1028:unit) (k_loop_2237:(((bool * int) * (bool * int)) -> X)) = loop_2167 () k_loop_2237 in
 let rec
   insert_1014 (x_1053:int) (ysys_1016:((int * int) -> (((bool * int) * (bool * int)) -> X) -> X)) 
              (k_insert_2260:(((int * int) -> (((bool * int) * (bool * int)) -> X) -> X) -> X)) =
   let xs_2203 (k_insert_xs_2267:((bool * int) -> X)) =
     ysys_1016 (0, 0) (fun (p_3433:((bool * int) * (bool * int))) -> k_insert_xs_2267 (fst p_3433))
   in
   xs_2203
     (fun (xs_2791:(bool * int)) ->
        (if not fst xs_2791 then
           k_insert_2260
             (let rs_1019 (i_1020:int) (k_insert_rs_2288:((bool * int) -> X)) =
                if i_1020 <= 0 then
                  k_insert_rs_2288 (true, x_1053)
                else
                  k_insert_rs_2288 (false, 0)
              in
              let rsrs_1021 (x_1147:(int * int)) (k_insert_rsrs_2307:(((bool * int) * (bool * int)) -> X)) =
                if fst x_1147 = snd x_1147 then
                  let r_1024 (k_insert_rsrs_r_2314:((bool * int) -> X)) = rs_1019 (fst x_1147) k_insert_rsrs_r_2314 in
                  r_1024 (fun (r_2325:(bool * int)) -> k_insert_rsrs_2307 (r_2325, r_2325))
                else
                  rs_1019 (fst x_1147)
                    (fun (x_3568:(bool * int)) ->
                       rs_1019 (snd x_1147) (fun (x_3573:(bool * int)) -> k_insert_rsrs_2307 (x_3568, x_3573)))
              in
              rsrs_1021)
         else
           let ys'ys'_1025 (x_1206:(int * int)) (k_insert_ys'ys'_2364:(((bool * int) * (bool * int)) -> X)) =
             ysys_1016 (fst x_1206 + 1, snd x_1206 + 1) k_insert_ys'ys'_2364
           in
           if x_1053 < snd xs_2791 then
             k_insert_2260
               (fun (x_1234:(int * int)) ->
                  fun (k_insert_2384:(((bool * int) * (bool * int)) -> X)) ->
                    (if fst x_1234 <= 0 then
                       if snd x_1234 <= 0 then
                         k_insert_2384 ((true, x_1053), (true, x_1053))
                       else
                         let xs_2208 (k_insert_xs_2411:(((bool * int) * (bool * int)) -> X)) =
                           ysys_1016 (0, snd x_1234 - 1) k_insert_xs_2411
                         in
                         xs_2208
                           (fun (xs_2494:((bool * int) * (bool * int))) ->
                              (if not fst (snd xs_2494) then
                                 loop_2167 () k_insert_2384
                               else
                                 let k_insert_2437 (b_3521:bool) =
                                   if b_3521 then
                                     loop_2167 () k_insert_2384
                                   else
                                     let k_insert_3538 (b_3543:bool) =
                                       if b_3543 then
                                         loop_2167 () k_insert_2384
                                       else
                                         k_insert_2384 ((true, x_1053), (true, snd (snd xs_2494)))
                                     in
                                     if fst xs_2791 <> fst (fst xs_2494) then
                                       k_insert_3538 true
                                     else
                                       k_insert_3538 (snd xs_2791 <> snd (fst xs_2494))
                                 in
                                 if snd x_1234 - 1 = 0 then
                                   k_insert_2437 (snd (fst xs_2494) <> snd (snd xs_2494))
                                 else
                                   k_insert_2437 false))
                     else
                       if snd x_1234 <= 0 then
                         ysys_1016 (fst x_1234 - 1, 0)
                           (fun (p_3512:((bool * int) * (bool * int))) -> k_insert_2384 (fst p_3512, (true, x_1053)))
                       else
                         ysys_1016 (fst x_1234 - 1, snd x_1234 - 1) k_insert_2384))
           else
             let
               ys''ys''_1034 (k_insert_ys''ys''_2572:(((int * int) -> (((bool * int) * (bool * int)) -> X) -> X) -> X)) =
               insert_1014 x_1053 ys'ys'_1025 k_insert_ys''ys''_2572
             in
             ys''ys''_1034
               (fun (ys''ys''_2772:((int * int) -> (((bool * int) * (bool * int)) -> X) -> X)) ->
                  k_insert_2260
                    (fun (x_1447:(int * int)) ->
                       fun (k_insert_2591:(((bool * int) * (bool * int)) -> X)) ->
                         (if fst x_1447 <= 0 then
                            if snd x_1447 <= 0 then
                              k_insert_2591 ((true, snd xs_2791), (true, snd xs_2791))
                            else
                              let xs_2206 (k_insert_xs_2624:(((bool * int) * (bool * int)) -> X)) =
                                ysys_1016 (0, snd x_1447 - 1) k_insert_xs_2624
                              in
                              xs_2206
                                (fun (xs_2710:((bool * int) * (bool * int))) ->
                                   (if not fst (snd xs_2710) then
                                      loop_2167 () k_insert_2591
                                    else
                                      let k_insert_2650 (b_3452:bool) =
                                        if b_3452 then
                                          loop_2167 () k_insert_2591
                                        else
                                          let k_insert_3472 (b_3477:bool) =
                                            if b_3477 then
                                              loop_2167 () k_insert_2591
                                            else
                                              k_insert_2591 ((true, snd xs_2791), (true, snd (snd xs_2710)))
                                          in
                                          if fst xs_2791 <> fst (fst xs_2710) then
                                            k_insert_3472 true
                                          else
                                            k_insert_3472 (snd xs_2791 <> snd (fst xs_2710))
                                      in
                                      if snd x_1447 - 1 = 0 then
                                        k_insert_2650 (snd (fst xs_2710) <> snd (snd xs_2710))
                                      else
                                        k_insert_2650 false))
                          else
                            if snd x_1447 <= 0 then
                              ys''ys''_2772 (fst x_1447, 0)
                                (fun (p_3440:((bool * int) * (bool * int))) ->
                                   k_insert_2591 (fst p_3440, (true, snd xs_2791)))
                            else
                              ys''ys''_2772 (fst x_1447 - 1, snd x_1447 - 1) k_insert_2591)))))
 in
 let rec
   insertsort_2165 (xsxs_1042:((int * int) -> (((bool * int) * (bool * int)) -> X) -> X)) 
                  (k_insertsort_2811:(((int * int) -> (((bool * int) * (bool * int)) -> X) -> X) -> X)) =
   xsxs_1042 (0, 0)
     (fun (p_3576:((bool * int) * (bool * int))) ->
        is_none_2185 (fst p_3576)
          (fun (b_3579:bool) ->
             (if b_3579 then
                k_insertsort_2811
                  (let rsrs_3608 (i_3609:(int * int)) (k_insertsort_rsrs_3610:(((bool * int) * (bool * int)) -> X)) =
                     k_insertsort_rsrs_3610 ((false, 0), (false, 0))
                   in
                   rsrs_3608)
              else
                let xs_3581 (k_insertsort_xs_3582:((bool * int) -> X)) =
                  xsxs_1042 (0, 0) (fun (p_3622:((bool * int) * (bool * int))) -> k_insertsort_xs_3582 (fst p_3622))
                in
                xs_3581
                  (fun (xs_3590:(bool * int)) ->
                     (let
                        xs'xs'_3592 (x_3593:(int * int)) 
                                   (k_insertsort_xs'xs'_3594:(((bool * int) * (bool * int)) -> X)) =
                        xsxs_1042 (fst x_3593 + 1, snd x_3593 + 1) k_insertsort_xs'xs'_3594
                      in
                      insertsort_2165 xs'xs'_3592
                        (fun (x_3625:((int * int) -> (((bool * int) * (bool * int)) -> X) -> X)) ->
                           insert_1014 (snd xs_3590) x_3625 k_insertsort_2811))))))
 in
 let rec make_list_1049 (n_1050:int) (k_make_list_2998:((int -> ((bool * int) -> X) -> X) -> X)) =
   if n_1050 = 0 then
     k_make_list_2998 (fun (i_1051:int) -> fun (k_make_list_3000:((bool * int) -> X)) -> k_make_list_3000 (false, 0))
   else
     let n_1052 (k_make_list_n_3015:(int -> X)) = rand_int_cps () k_make_list_n_3015 in
     n_1052
       (fun (n_3073:int) ->
          (let xs_1053 (k_make_list_xs_3036:((int -> ((bool * int) -> X) -> X) -> X)) =
             make_list_1049 (n_3073 - 1) k_make_list_xs_3036
           in
           xs_1053
             (fun (xs_3072:(int -> ((bool * int) -> X) -> X)) ->
                k_make_list_2998
                  (fun (i_1054:int) ->
                     fun (k_make_list_3049:((bool * int) -> X)) ->
                       (if i_1054 = 0 then
                          k_make_list_3049 (true, n_3073)
                        else
                          xs_3072 (i_1054 - 1) k_make_list_3049)))))
 in
 let rec check_2164 (xsxs_1056:((int * int) -> (((bool * int) * (bool * int)) -> X) -> X)) (k_check_3093:(bool -> X)) =
   let xs_2212 (k_check_xs_3105:(((bool * int) * (bool * int)) -> X)) = xsxs_1056 (0, 0) k_check_xs_3105 in
   xs_2212
     (fun (xs_3183:((bool * int) * (bool * int))) ->
        (if not fst (fst xs_3183) then
           k_check_3093 true
         else
           if not fst (snd xs_3183) then
             k_check_3093 true
           else
             let xs'xs'_1061 (x_1917:(int * int)) (k_check_xs'xs'_3122:(((bool * int) * (bool * int)) -> X)) =
               xsxs_1056 (fst x_1917 + 1, snd x_1917 + 1) k_check_xs'xs'_3122
             in
             if snd (fst xs_3183) <= snd (snd xs_3183) then
               check_2164 xs'xs'_1061 k_check_3093
             else
               k_check_3093 false))
 in
 let main_1064 (n_1065:int) (k_main_3193:(unit -> X)) =
   let xs_1066 (k_main_xs_3206:((int -> ((bool * int) -> X) -> X) -> X)) = make_list_1049 n_1065 k_main_xs_3206 in
   xs_1066
     (fun (xs_3352:(int -> ((bool * int) -> X) -> X)) ->
        (let xsxs_1067 (x_1976:(int * int)) (k_main_xsxs_3221:(((bool * int) * (bool * int)) -> X)) =
           if fst x_1976 = snd x_1976 then
             let r_1070 (k_main_xsxs_r_3228:((bool * int) -> X)) = xs_3352 (fst x_1976) k_main_xsxs_r_3228 in
             r_1070 (fun (r_3239:(bool * int)) -> k_main_xsxs_3221 (r_3239, r_3239))
           else
             xs_3352 (fst x_1976)
               (fun (x_3637:(bool * int)) ->
                  xs_3352 (snd x_1976) (fun (x_3642:(bool * int)) -> k_main_xsxs_3221 (x_3637, x_3642)))
         in
         let ysys_1071 (k_main_ysys_3290:(((int * int) -> (((bool * int) * (bool * int)) -> X) -> X) -> X)) =
           insertsort_2165 xsxs_1067 k_main_ysys_3290
         in
         ysys_1071
           (fun (ysys_3344:((int * int) -> (((bool * int) * (bool * int)) -> X) -> X)) ->
              check_2164 ysys_3344
                (fun (b_3643:bool) -> (if b_3643 then
                                         k_main_3193 ()
                                       else
                                         {|fail|} () k_main_3193)))))
 in
 let main_2163 (k_main_3359:(unit -> X)) =
   let arg1_2161 (k_main_arg1_3364:(int -> X)) = rand_int_cps () k_main_arg1_3364 in
   arg1_2161 (fun (arg1_3380:int) -> main_1064 arg1_3380 k_main_3359)
 in
 main_2163 (fun (main_3381:unit) -> {end})

remove_pair:
 let is_none_2185 (x1_1012:bool) (x2_1012:int) (k_is_none_2230:(bool -> X)) = k_is_none_2230 (not x1_1012) in
 let rec loop_2167 (x_1028:unit) (k_loop_2237:(bool -> int -> bool -> int -> X)) = loop_2167 () k_loop_2237 in
 let rec
   insert_1014 (x_1053:int) (ysys_1016:(int -> int -> (bool -> int -> bool -> int -> X) -> X)) 
              (k_insert_2260:((int -> int -> (bool -> int -> bool -> int -> X) -> X) -> X)) =
   let xs_2203 (k_insert_xs_2267:(bool -> int -> X)) =
     ysys_1016 0 0
       (fun (p11_3433:bool) ->
          fun (p12_3433:int) -> fun (p21_3433:bool) -> fun (p22_3433:int) -> k_insert_xs_2267 p11_3433 p12_3433)
   in
   xs_2203
     (fun (xs1_2791:bool) ->
        fun (xs2_2791:int) ->
          (if not xs1_2791 then
             k_insert_2260
               (let rs_1019 (i_1020:int) (k_insert_rs_2288:(bool -> int -> X)) =
                  if i_1020 <= 0 then
                    k_insert_rs_2288 true x_1053
                  else
                    k_insert_rs_2288 false 0
                in
                let rsrs_1021 (x1_1147:int) (x2_1147:int) (k_insert_rsrs_2307:(bool -> int -> bool -> int -> X)) =
                  if x1_1147 = x2_1147 then
                    let r_1024 (k_insert_rsrs_r_2314:(bool -> int -> X)) = rs_1019 x1_1147 k_insert_rsrs_r_2314 in
                    r_1024
                      (fun (r1_2325:bool) -> fun (r2_2325:int) -> k_insert_rsrs_2307 r1_2325 r2_2325 r1_2325 r2_2325)
                  else
                    rs_1019 x1_1147
                      (fun (x1_3568:bool) ->
                         fun (x2_3568:int) ->
                           rs_1019 x2_1147
                             (fun (x1_3573:bool) ->
                                fun (x2_3573:int) -> k_insert_rsrs_2307 x1_3568 x2_3568 x1_3573 x2_3573))
                in
                rsrs_1021)
           else
             let ys'ys'_1025 (x1_1206:int) (x2_1206:int) (k_insert_ys'ys'_2364:(bool -> int -> bool -> int -> X)) =
               ysys_1016 (x1_1206 + 1) (x2_1206 + 1) k_insert_ys'ys'_2364
             in
             if x_1053 < xs2_2791 then
               k_insert_2260
                 (fun (x1_1234:int) ->
                    fun (x2_1234:int) ->
                      fun (k_insert_2384:(bool -> int -> bool -> int -> X)) ->
                        (if x1_1234 <= 0 then
                           if x2_1234 <= 0 then
                             k_insert_2384 true x_1053 true x_1053
                           else
                             let xs_2208 (k_insert_xs_2411:(bool -> int -> bool -> int -> X)) =
                               ysys_1016 0 (x2_1234 - 1) k_insert_xs_2411
                             in
                             xs_2208
                               (fun (xs11_2494:bool) ->
                                  fun (xs12_2494:int) ->
                                    fun (xs21_2494:bool) ->
                                      fun (xs22_2494:int) ->
                                        (if not xs21_2494 then
                                           loop_2167 () k_insert_2384
                                         else
                                           let k_insert_2437 (b_3521:bool) =
                                             if b_3521 then
                                               loop_2167 () k_insert_2384
                                             else
                                               let k_insert_3538 (b_3543:bool) =
                                                 if b_3543 then
                                                   loop_2167 () k_insert_2384
                                                 else
                                                   k_insert_2384 true x_1053 true xs22_2494
                                               in
                                               if xs1_2791 <> xs11_2494 then
                                                 k_insert_3538 true
                                               else
                                                 k_insert_3538 (xs2_2791 <> xs12_2494)
                                           in
                                           if x2_1234 - 1 = 0 then
                                             k_insert_2437 (xs12_2494 <> xs22_2494)
                                           else
                                             k_insert_2437 false))
                         else
                           if x2_1234 <= 0 then
                             ysys_1016 (x1_1234 - 1) 0
                               (fun (p11_3512:bool) ->
                                  fun (p12_3512:int) ->
                                    fun (p21_3512:bool) ->
                                      fun (p22_3512:int) -> k_insert_2384 p11_3512 p12_3512 true x_1053)
                           else
                             ysys_1016 (x1_1234 - 1) (x2_1234 - 1) k_insert_2384))
             else
               let
                 ys''ys''_1034 (k_insert_ys''ys''_2572:((int -> int -> (bool -> int -> bool -> int -> X) -> X) -> X)) =
                 insert_1014 x_1053 ys'ys'_1025 k_insert_ys''ys''_2572
               in
               ys''ys''_1034
                 (fun (ys''ys''_2772:(int -> int -> (bool -> int -> bool -> int -> X) -> X)) ->
                    k_insert_2260
                      (fun (x1_1447:int) ->
                         fun (x2_1447:int) ->
                           fun (k_insert_2591:(bool -> int -> bool -> int -> X)) ->
                             (if x1_1447 <= 0 then
                                if x2_1447 <= 0 then
                                  k_insert_2591 true xs2_2791 true xs2_2791
                                else
                                  let xs_2206 (k_insert_xs_2624:(bool -> int -> bool -> int -> X)) =
                                    ysys_1016 0 (x2_1447 - 1) k_insert_xs_2624
                                  in
                                  xs_2206
                                    (fun (xs11_2710:bool) ->
                                       fun (xs12_2710:int) ->
                                         fun (xs21_2710:bool) ->
                                           fun (xs22_2710:int) ->
                                             (if not xs21_2710 then
                                                loop_2167 () k_insert_2591
                                              else
                                                let k_insert_2650 (b_3452:bool) =
                                                  if b_3452 then
                                                    loop_2167 () k_insert_2591
                                                  else
                                                    let k_insert_3472
                                                       (b_3477:bool) =
                                                      if b_3477 then
                                                        loop_2167 () k_insert_2591
                                                      else
                                                        k_insert_2591 true xs2_2791 true xs22_2710
                                                    in
                                                    if xs1_2791 <> xs11_2710 then
                                                      k_insert_3472 true
                                                    else
                                                      k_insert_3472 (xs2_2791 <> xs12_2710)
                                                in
                                                if x2_1447 - 1 = 0 then
                                                  k_insert_2650 (xs12_2710 <> xs22_2710)
                                                else
                                                  k_insert_2650 false))
                              else
                                if x2_1447 <= 0 then
                                  ys''ys''_2772 x1_1447 0
                                    (fun (p11_3440:bool) ->
                                       fun (p12_3440:int) ->
                                         fun (p21_3440:bool) ->
                                           fun (p22_3440:int) -> k_insert_2591 p11_3440 p12_3440 true xs2_2791)
                                else
                                  ys''ys''_2772 (x1_1447 - 1) (x2_1447 - 1) k_insert_2591)))))
 in
 let rec
   insertsort_2165 (xsxs_1042:(int -> int -> (bool -> int -> bool -> int -> X) -> X)) 
                  (k_insertsort_2811:((int -> int -> (bool -> int -> bool -> int -> X) -> X) -> X)) =
   xsxs_1042 0 0
     (fun (p11_3576:bool) ->
        fun (p12_3576:int) ->
          fun (p21_3576:bool) ->
            fun (p22_3576:int) ->
              is_none_2185 p11_3576 p12_3576
                (fun (b_3579:bool) ->
                   (if b_3579 then
                      k_insertsort_2811
                        (let
                           rsrs_3608 (i1_3609:int) (i2_3609:int) 
                                    (k_insertsort_rsrs_3610:(bool -> int -> bool -> int -> X)) =
                           k_insertsort_rsrs_3610 false 0 false 0
                         in
                         rsrs_3608)
                    else
                      let xs_3581 (k_insertsort_xs_3582:(bool -> int -> X)) =
                        xsxs_1042 0 0
                          (fun (p11_3622:bool) ->
                             fun (p12_3622:int) ->
                               fun (p21_3622:bool) -> fun (p22_3622:int) -> k_insertsort_xs_3582 p11_3622 p12_3622)
                      in
                      xs_3581
                        (fun (xs1_3590:bool) ->
                           fun (xs2_3590:int) ->
                             (let
                                xs'xs'_3592 (x1_3593:int) (x2_3593:int) 
                                           (k_insertsort_xs'xs'_3594:(
                                           bool -> int -> bool -> int -> X)) =
                                xsxs_1042 (x1_3593 + 1) (x2_3593 + 1) k_insertsort_xs'xs'_3594
                              in
                              insertsort_2165 xs'xs'_3592
                                (fun (x_3625:(int -> int -> (bool -> int -> bool -> int -> X) -> X)) ->
                                   insert_1014 xs2_3590 x_3625 k_insertsort_2811))))))
 in
 let rec make_list_1049 (n_1050:int) (k_make_list_2998:((int -> (bool -> int -> X) -> X) -> X)) =
   if n_1050 = 0 then
     k_make_list_2998 (fun (i_1051:int) -> fun (k_make_list_3000:(bool -> int -> X)) -> k_make_list_3000 false 0)
   else
     let n_1052 (k_make_list_n_3015:(int -> X)) = rand_int_cps () k_make_list_n_3015 in
     n_1052
       (fun (n_3073:int) ->
          (let xs_1053 (k_make_list_xs_3036:((int -> (bool -> int -> X) -> X) -> X)) =
             make_list_1049 (n_3073 - 1) k_make_list_xs_3036
           in
           xs_1053
             (fun (xs_3072:(int -> (bool -> int -> X) -> X)) ->
                k_make_list_2998
                  (fun (i_1054:int) ->
                     fun (k_make_list_3049:(bool -> int -> X)) ->
                       (if i_1054 = 0 then
                          k_make_list_3049 true n_3073
                        else
                          xs_3072 (i_1054 - 1) k_make_list_3049)))))
 in
 let rec check_2164 (xsxs_1056:(int -> int -> (bool -> int -> bool -> int -> X) -> X)) (k_check_3093:(bool -> X)) =
   let xs_2212 (k_check_xs_3105:(bool -> int -> bool -> int -> X)) = xsxs_1056 0 0 k_check_xs_3105 in
   xs_2212
     (fun (xs11_3183:bool) ->
        fun (xs12_3183:int) ->
          fun (xs21_3183:bool) ->
            fun (xs22_3183:int) ->
              (if not xs11_3183 then
                 k_check_3093 true
               else
                 if not xs21_3183 then
                   k_check_3093 true
                 else
                   let
                     xs'xs'_1061 (x1_1917:int) (x2_1917:int) (k_check_xs'xs'_3122:(bool -> int -> bool -> int -> X)) =
                     xsxs_1056 (x1_1917 + 1) (x2_1917 + 1) k_check_xs'xs'_3122
                   in
                   if xs12_3183 <= xs22_3183 then
                     check_2164 xs'xs'_1061 k_check_3093
                   else
                     k_check_3093 false))
 in
 let main_1064 (n_1065:int) (k_main_3193:(unit -> X)) =
   let xs_1066 (k_main_xs_3206:((int -> (bool -> int -> X) -> X) -> X)) = make_list_1049 n_1065 k_main_xs_3206 in
   xs_1066
     (fun (xs_3352:(int -> (bool -> int -> X) -> X)) ->
        (let xsxs_1067 (x1_1976:int) (x2_1976:int) (k_main_xsxs_3221:(bool -> int -> bool -> int -> X)) =
           if x1_1976 = x2_1976 then
             let r_1070 (k_main_xsxs_r_3228:(bool -> int -> X)) = xs_3352 x1_1976 k_main_xsxs_r_3228 in
             r_1070 (fun (r1_3239:bool) -> fun (r2_3239:int) -> k_main_xsxs_3221 r1_3239 r2_3239 r1_3239 r2_3239)
           else
             xs_3352 x1_1976
               (fun (x1_3637:bool) ->
                  fun (x2_3637:int) ->
                    xs_3352 x2_1976
                      (fun (x1_3642:bool) -> fun (x2_3642:int) -> k_main_xsxs_3221 x1_3637 x2_3637 x1_3642 x2_3642))
         in
         let ysys_1071 (k_main_ysys_3290:((int -> int -> (bool -> int -> bool -> int -> X) -> X) -> X)) =
           insertsort_2165 xsxs_1067 k_main_ysys_3290
         in
         ysys_1071
           (fun (ysys_3344:(int -> int -> (bool -> int -> bool -> int -> X) -> X)) ->
              check_2164 ysys_3344
                (fun (b_3643:bool) -> (if b_3643 then
                                         k_main_3193 ()
                                       else
                                         {|fail|} () k_main_3193)))))
 in
 let main_2163 (k_main_3359:(unit -> X)) =
   let arg1_2161 (k_main_arg1_3364:(int -> X)) = rand_int_cps () k_main_arg1_3364 in
   arg1_2161 (fun (arg1_3380:int) -> main_1064 arg1_3380 k_main_3359)
 in
 main_2163 (fun (main_3381:unit) -> {end})

spec (abstraction type environment for CPS transformed program):
 insert_1014: (int ->
                 (i1_2054:int ->
                    j1_2055:int ->
                      (b1_2056:bool ->
                         r1_2057:int ->
                           b2_2058:bool ->
                             int[\r2_2059. ((((not b1_2056 || not b2_2058) || not (i1_2054 <= j1_2055)) || i1_2054 < 0)
                                            || j1_2055 < 0)
                                           || r1_2057 <= r2_2059] -> X) -> X) ->
                   ((i2_2070:int ->
                       j2_2071:int ->
                         (b3_2072:bool ->
                            r3_2073:int ->
                              b4_2074:bool ->
                                int[\r4_2075. ((((not b3_2072 || not b4_2074) || not (i2_2070 <= j2_2071)) ||
                                                i2_2070 < 0)
                                               || j2_2071 < 0)
                                              || r3_2073 <= r4_2075] -> X) -> X) -> X) -> X)
 rs_1019: (i_2092:int -> (b_2093:bool -> int[\__2094. i_2092 = 0 || not b_2093] -> X) -> X)
 insertsort_2165: ((int -> int -> (bool -> int -> bool -> int -> X) -> X) ->
                     ((i2_2117:int ->
                         j2_2118:int ->
                           (b3_2119:bool ->
                              r3_2120:int ->
                                b4_2121:bool ->
                                  int[\r4_2122. ((((not b3_2119 || not b4_2121) || not (i2_2117 <= j2_2118)) ||
                                                  i2_2117 < 0)
                                                 || j2_2118 < 0)
                                                || r3_2120 <= r4_2122] -> X) -> X) -> X) -> X)
 check_2164: ((i_2138:int ->
                 j_2139:int ->
                   (b1_2140:bool ->
                      r1_2141:int ->
                        b2_2142:bool ->
                          int[\r2_2143. ((((not b1_2140 || not b2_2142) || not (i_2138 <= j_2139)) || i_2138 < 0) ||
                                         j_2139 < 0)
                                        || r1_2141 <= r2_2143] -> X) -> X) -> 
                (bool[\x_14_2154. x_14_2154] -> X) -> X)
 
add_preds:
 let is_none_2185 (x1_1012:bool) (x2_1012:int) (k_is_none_2230:(bool -> X)) = k_is_none_2230 (not x1_1012) in
 let rec loop_2167 (x_1028:unit) (k_loop_2237:(bool -> int -> bool -> int -> X)) = loop_2167 () k_loop_2237 in
 let rec
   insert_1014 (x_1053:int) 
              (ysys_1016:(i1_2054:int ->
                            j1_2055:int ->
                              (b1_2056:bool ->
                                 r1_2057:int ->
                                   b2_2058:bool ->
                                     int[\r2_2059. ((((not b1_2056 || not b2_2058) || not (i1_2054 <= j1_2055)) ||
                                                     i1_2054 < 0)
                                                    || j1_2055 < 0)
                                                   || r1_2057 <= r2_2059] -> X) -> X))
              (k_insert_2260:((i2_2070:int ->
                                 j2_2071:int ->
                                   (b3_2072:bool ->
                                      r3_2073:int ->
                                        b4_2074:bool ->
                                          int[\r4_2075. ((((not b3_2072 || not b4_2074) || not (i2_2070 <= j2_2071)) ||
                                                          i2_2070 < 0)
                                                         || j2_2071 < 0)
                                                        || r3_2073 <= r4_2075] -> X) -> X) -> X)) =
   let xs_2203 (k_insert_xs_2267:(bool -> int -> X)) =
     ysys_1016 0 0
       (fun (p11_3433:bool) ->
          fun (p12_3433:int) -> fun (p21_3433:bool) -> fun (p22_3433:int) -> k_insert_xs_2267 p11_3433 p12_3433)
   in
   xs_2203
     (fun (xs1_2791:bool) ->
        fun (xs2_2791:int) ->
          (if not xs1_2791 then
             k_insert_2260
               (let
                  rs_1019 (i_1020:int) (k_insert_rs_2288:(b_2093:bool -> int[\__2094. i_1020 = 0 || not b_2093] -> X)) =
                  if i_1020 <= 0 then
                    k_insert_rs_2288 true x_1053
                  else
                    k_insert_rs_2288 false 0
                in
                let rsrs_1021 (x1_1147:int) (x2_1147:int) (k_insert_rsrs_2307:(bool -> int -> bool -> int -> X)) =
                  if x1_1147 = x2_1147 then
                    let r_1024 (k_insert_rsrs_r_2314:(bool -> int -> X)) = rs_1019 x1_1147 k_insert_rsrs_r_2314 in
                    r_1024
                      (fun (r1_2325:bool) -> fun (r2_2325:int) -> k_insert_rsrs_2307 r1_2325 r2_2325 r1_2325 r2_2325)
                  else
                    rs_1019 x1_1147
                      (fun (x1_3568:bool) ->
                         fun (x2_3568:int) ->
                           rs_1019 x2_1147
                             (fun (x1_3573:bool) ->
                                fun (x2_3573:int) -> k_insert_rsrs_2307 x1_3568 x2_3568 x1_3573 x2_3573))
                in
                rsrs_1021)
           else
             let ys'ys'_1025 (x1_1206:int) (x2_1206:int) (k_insert_ys'ys'_2364:(bool -> int -> bool -> int -> X)) =
               ysys_1016 (x1_1206 + 1) (x2_1206 + 1) k_insert_ys'ys'_2364
             in
             if x_1053 < xs2_2791 then
               k_insert_2260
                 (fun (x1_1234:int) ->
                    fun (x2_1234:int) ->
                      fun (k_insert_2384:(bool -> int -> bool -> int -> X)) ->
                        (if x1_1234 <= 0 then
                           if x2_1234 <= 0 then
                             k_insert_2384 true x_1053 true x_1053
                           else
                             let xs_2208 (k_insert_xs_2411:(bool -> int -> bool -> int -> X)) =
                               ysys_1016 0 (x2_1234 - 1) k_insert_xs_2411
                             in
                             xs_2208
                               (fun (xs11_2494:bool) ->
                                  fun (xs12_2494:int) ->
                                    fun (xs21_2494:bool) ->
                                      fun (xs22_2494:int) ->
                                        (if not xs21_2494 then
                                           loop_2167 () k_insert_2384
                                         else
                                           let k_insert_2437 (b_3521:bool) =
                                             if b_3521 then
                                               loop_2167 () k_insert_2384
                                             else
                                               let k_insert_3538 (b_3543:bool) =
                                                 if b_3543 then
                                                   loop_2167 () k_insert_2384
                                                 else
                                                   k_insert_2384 true x_1053 true xs22_2494
                                               in
                                               if xs1_2791 <> xs11_2494 then
                                                 k_insert_3538 true
                                               else
                                                 k_insert_3538 (xs2_2791 <> xs12_2494)
                                           in
                                           if x2_1234 - 1 = 0 then
                                             k_insert_2437 (xs12_2494 <> xs22_2494)
                                           else
                                             k_insert_2437 false))
                         else
                           if x2_1234 <= 0 then
                             ysys_1016 (x1_1234 - 1) 0
                               (fun (p11_3512:bool) ->
                                  fun (p12_3512:int) ->
                                    fun (p21_3512:bool) ->
                                      fun (p22_3512:int) -> k_insert_2384 p11_3512 p12_3512 true x_1053)
                           else
                             ysys_1016 (x1_1234 - 1) (x2_1234 - 1) k_insert_2384))
             else
               let
                 ys''ys''_1034 (k_insert_ys''ys''_2572:((int -> int -> (bool -> int -> bool -> int -> X) -> X) -> X)) =
                 insert_1014 x_1053 ys'ys'_1025 k_insert_ys''ys''_2572
               in
               ys''ys''_1034
                 (fun (ys''ys''_2772:(int -> int -> (bool -> int -> bool -> int -> X) -> X)) ->
                    k_insert_2260
                      (fun (x1_1447:int) ->
                         fun (x2_1447:int) ->
                           fun (k_insert_2591:(bool -> int -> bool -> int -> X)) ->
                             (if x1_1447 <= 0 then
                                if x2_1447 <= 0 then
                                  k_insert_2591 true xs2_2791 true xs2_2791
                                else
                                  let xs_2206 (k_insert_xs_2624:(bool -> int -> bool -> int -> X)) =
                                    ysys_1016 0 (x2_1447 - 1) k_insert_xs_2624
                                  in
                                  xs_2206
                                    (fun (xs11_2710:bool) ->
                                       fun (xs12_2710:int) ->
                                         fun (xs21_2710:bool) ->
                                           fun (xs22_2710:int) ->
                                             (if not xs21_2710 then
                                                loop_2167 () k_insert_2591
                                              else
                                                let k_insert_2650 (b_3452:bool) =
                                                  if b_3452 then
                                                    loop_2167 () k_insert_2591
                                                  else
                                                    let k_insert_3472
                                                       (b_3477:bool) =
                                                      if b_3477 then
                                                        loop_2167 () k_insert_2591
                                                      else
                                                        k_insert_2591 true xs2_2791 true xs22_2710
                                                    in
                                                    if xs1_2791 <> xs11_2710 then
                                                      k_insert_3472 true
                                                    else
                                                      k_insert_3472 (xs2_2791 <> xs12_2710)
                                                in
                                                if x2_1447 - 1 = 0 then
                                                  k_insert_2650 (xs12_2710 <> xs22_2710)
                                                else
                                                  k_insert_2650 false))
                              else
                                if x2_1447 <= 0 then
                                  ys''ys''_2772 x1_1447 0
                                    (fun (p11_3440:bool) ->
                                       fun (p12_3440:int) ->
                                         fun (p21_3440:bool) ->
                                           fun (p22_3440:int) -> k_insert_2591 p11_3440 p12_3440 true xs2_2791)
                                else
                                  ys''ys''_2772 (x1_1447 - 1) (x2_1447 - 1) k_insert_2591)))))
 in
 let rec
   insertsort_2165 (xsxs_1042:(int -> int -> (bool -> int -> bool -> int -> X) -> X)) 
                  (k_insertsort_2811:((i2_2117:int ->
                                         j2_2118:int ->
                                           (b3_2119:bool ->
                                              r3_2120:int ->
                                                b4_2121:bool ->
                                                  int[\r4_2122. ((((not b3_2119 || not b4_2121) ||
                                                                   not (i2_2117 <= j2_2118))
                                                                  || 
                                                                  i2_2117 < 0)
                                                                 || j2_2118 < 0)
                                                                || r3_2120 <= r4_2122] -> X) -> X) -> X)) =
   xsxs_1042 0 0
     (fun (p11_3576:bool) ->
        fun (p12_3576:int) ->
          fun (p21_3576:bool) ->
            fun (p22_3576:int) ->
              is_none_2185 p11_3576 p12_3576
                (fun (b_3579:bool) ->
                   (if b_3579 then
                      k_insertsort_2811
                        (let
                           rsrs_3608 (i1_3609:int) (i2_3609:int) 
                                    (k_insertsort_rsrs_3610:(bool -> int -> bool -> int -> X)) =
                           k_insertsort_rsrs_3610 false 0 false 0
                         in
                         rsrs_3608)
                    else
                      let xs_3581 (k_insertsort_xs_3582:(bool -> int -> X)) =
                        xsxs_1042 0 0
                          (fun (p11_3622:bool) ->
                             fun (p12_3622:int) ->
                               fun (p21_3622:bool) -> fun (p22_3622:int) -> k_insertsort_xs_3582 p11_3622 p12_3622)
                      in
                      xs_3581
                        (fun (xs1_3590:bool) ->
                           fun (xs2_3590:int) ->
                             (let
                                xs'xs'_3592 (x1_3593:int) (x2_3593:int) 
                                           (k_insertsort_xs'xs'_3594:(
                                           bool -> int -> bool -> int -> X)) =
                                xsxs_1042 (x1_3593 + 1) (x2_3593 + 1) k_insertsort_xs'xs'_3594
                              in
                              insertsort_2165 xs'xs'_3592
                                (fun (x_3625:(int -> int -> (bool -> int -> bool -> int -> X) -> X)) ->
                                   insert_1014 xs2_3590 x_3625 k_insertsort_2811))))))
 in
 let rec make_list_1049 (n_1050:int) (k_make_list_2998:((int -> (bool -> int -> X) -> X) -> X)) =
   if n_1050 = 0 then
     k_make_list_2998 (fun (i_1051:int) -> fun (k_make_list_3000:(bool -> int -> X)) -> k_make_list_3000 false 0)
   else
     let n_1052 (k_make_list_n_3015:(int -> X)) = rand_int_cps () k_make_list_n_3015 in
     n_1052
       (fun (n_3073:int) ->
          (let xs_1053 (k_make_list_xs_3036:((int -> (bool -> int -> X) -> X) -> X)) =
             make_list_1049 (n_3073 - 1) k_make_list_xs_3036
           in
           xs_1053
             (fun (xs_3072:(int -> (bool -> int -> X) -> X)) ->
                k_make_list_2998
                  (fun (i_1054:int) ->
                     fun (k_make_list_3049:(bool -> int -> X)) ->
                       (if i_1054 = 0 then
                          k_make_list_3049 true n_3073
                        else
                          xs_3072 (i_1054 - 1) k_make_list_3049)))))
 in
 let rec
   check_2164
             (xsxs_1056:(i_2138:int ->
                           j_2139:int ->
                             (b1_2140:bool ->
                                r1_2141:int ->
                                  b2_2142:bool ->
                                    int[\r2_2143. ((((not b1_2140 || not b2_2142) || not (i_2138 <= j_2139)) ||
                                                    i_2138 < 0)
                                                   || j_2139 < 0)
                                                  || r1_2141 <= r2_2143] -> X) -> X))
             (k_check_3093:(bool[\x_14_2154. x_14_2154] -> X)) =
   let xs_2212 (k_check_xs_3105:(bool -> int -> bool -> int -> X)) = xsxs_1056 0 0 k_check_xs_3105 in
   xs_2212
     (fun (xs11_3183:bool) ->
        fun (xs12_3183:int) ->
          fun (xs21_3183:bool) ->
            fun (xs22_3183:int) ->
              (if not xs11_3183 then
                 k_check_3093 true
               else
                 if not xs21_3183 then
                   k_check_3093 true
                 else
                   let
                     xs'xs'_1061 (x1_1917:int) (x2_1917:int) (k_check_xs'xs'_3122:(bool -> int -> bool -> int -> X)) =
                     xsxs_1056 (x1_1917 + 1) (x2_1917 + 1) k_check_xs'xs'_3122
                   in
                   if xs12_3183 <= xs22_3183 then
                     check_2164 xs'xs'_1061 k_check_3093
                   else
                     k_check_3093 false))
 in
 let main_1064 (n_1065:int) (k_main_3193:(unit -> X)) =
   let xs_1066 (k_main_xs_3206:((int -> (bool -> int -> X) -> X) -> X)) = make_list_1049 n_1065 k_main_xs_3206 in
   xs_1066
     (fun (xs_3352:(int -> (bool -> int -> X) -> X)) ->
        (let xsxs_1067 (x1_1976:int) (x2_1976:int) (k_main_xsxs_3221:(bool -> int -> bool -> int -> X)) =
           if x1_1976 = x2_1976 then
             let r_1070 (k_main_xsxs_r_3228:(bool -> int -> X)) = xs_3352 x1_1976 k_main_xsxs_r_3228 in
             r_1070 (fun (r1_3239:bool) -> fun (r2_3239:int) -> k_main_xsxs_3221 r1_3239 r2_3239 r1_3239 r2_3239)
           else
             xs_3352 x1_1976
               (fun (x1_3637:bool) ->
                  fun (x2_3637:int) ->
                    xs_3352 x2_1976
                      (fun (x1_3642:bool) -> fun (x2_3642:int) -> k_main_xsxs_3221 x1_3637 x2_3637 x1_3642 x2_3642))
         in
         let ysys_1071 (k_main_ysys_3290:((int -> int -> (bool -> int -> bool -> int -> X) -> X) -> X)) =
           insertsort_2165 xsxs_1067 k_main_ysys_3290
         in
         ysys_1071
           (fun (ysys_3344:(int -> int -> (bool -> int -> bool -> int -> X) -> X)) ->
              check_2164 ysys_3344
                (fun (b_3643:bool) -> (if b_3643 then
                                         k_main_3193 ()
                                       else
                                         {|fail|} () k_main_3193)))))
 in
 let main_2163 (k_main_3359:(unit -> X)) =
   let arg1_2161 (k_main_arg1_3364:(int -> X)) = rand_int_cps () k_main_arg1_3364 in
   arg1_2161 (fun (arg1_3380:int) -> main_1064 arg1_3380 k_main_3359)
 in
 main_2163 (fun (main_3381:unit) -> {end})

spec (abstraction type environment for CPS transformed program):
 insert_1014: (int ->
                 (i1_2054:int ->
                    j1_2055:int ->
                      (b1_2056:bool ->
                         r1_2057:int ->
                           b2_2058:bool ->
                             int[\r2_2059. ((((not b1_2056 || not b2_2058) || not (i1_2054 <= j1_2055)) || i1_2054 < 0)
                                            || j1_2055 < 0)
                                           || r1_2057 <= r2_2059] -> X) -> X) ->
                   ((i2_2070:int ->
                       j2_2071:int ->
                         (b3_2072:bool ->
                            r3_2073:int ->
                              b4_2074:bool ->
                                int[\r4_2075. ((((not b3_2072 || not b4_2074) || not (i2_2070 <= j2_2071)) ||
                                                i2_2070 < 0)
                                               || j2_2071 < 0)
                                              || r3_2073 <= r4_2075] -> X) -> X) -> X) -> X)
 rs_1019: (i_2092:int -> (b_2093:bool -> int[\__2094. i_2092 = 0 || not b_2093] -> X) -> X)
 insertsort_2165: ((int -> int -> (bool -> int -> bool -> int -> X) -> X) ->
                     ((i2_2117:int ->
                         j2_2118:int ->
                           (b3_2119:bool ->
                              r3_2120:int ->
                                b4_2121:bool ->
                                  int[\r4_2122. ((((not b3_2119 || not b4_2121) || not (i2_2117 <= j2_2118)) ||
                                                  i2_2117 < 0)
                                                 || j2_2118 < 0)
                                                || r3_2120 <= r4_2122] -> X) -> X) -> X) -> X)
 check_2164: ((i_2138:int ->
                 j_2139:int ->
                   (b1_2140:bool ->
                      r1_2141:int ->
                        b2_2142:bool ->
                          int[\r2_2143. ((((not b1_2140 || not b2_2142) || not (i_2138 <= j_2139)) || i_2138 < 0) ||
                                         j_2139 < 0)
                                        || r1_2141 <= r2_2143] -> X) -> X) -> 
                (bool[\x_14_2154. x_14_2154] -> X) -> X)
 
Program with abstraction types (CEGAR-cycle 0)::
Main: main_3652
  main_3652 -> (main_2163 f_3682)
  arg1_2161 k_main_arg1_3364 -> (rand_int k_main_arg1_3364)
  br_f_check_3701 b_3702 k_check_3093 xsxs_1056 xs11_3183 xs12_3183 xs21_3183 xs22_3183 when b_3702 ->
      (check_2164 (xs'xs'_1061 xs11_3183 xs12_3183 xs21_3183 xs22_3183 xsxs_1056) k_check_3093)
  br_f_check_3701 b_3702 k_check_3093 xsxs_1056 xs11_3183 xs12_3183 xs21_3183 xs22_3183 when (
      not b_3702) -> (k_check_3093 false)
  br_f_check_3703 b_3704 k_check_3093 xsxs_1056 xs11_3183 xs12_3183 xs21_3183 xs22_3183 when b_3704 ->
      (k_check_3093 true)
  br_f_check_3703 b_3704 k_check_3093 xsxs_1056 xs11_3183 xs12_3183 xs21_3183 xs22_3183 when (
      not b_3704) ->
      (br_f_check_3701 (xs12_3183 <= xs22_3183) k_check_3093 xsxs_1056 xs11_3183 xs12_3183 xs21_3183 xs22_3183)
  br_f_insert_3685 b_3686 x1_1234 x2_1234 x_1053 xs1_2791 xs2_2791 k_insert_2384 xs11_2494 xs12_2494 xs21_2494 
  xs22_2494 when b_3686 ->
      (k_insert_2437 x1_1234 x2_1234 x_1053 xs11_2494 xs12_2494 xs1_2791 xs21_2494 xs22_2494 xs2_2791 k_insert_2384
        (not (xs12_2494 = xs22_2494)))
  br_f_insert_3685 b_3686 x1_1234 x2_1234 x_1053 xs1_2791 xs2_2791 k_insert_2384 xs11_2494 xs12_2494 xs21_2494 
  xs22_2494 when (not b_3686) ->
      (k_insert_2437 x1_1234 x2_1234 x_1053 xs11_2494 xs12_2494 xs1_2791 xs21_2494 xs22_2494 xs2_2791 k_insert_2384
        false)
  br_f_insert_3687 b_3688 x_1053 xs1_2791 xs2_2791 ysys_1016 x1_1234 x2_1234 k_insert_2384 when b_3688 ->
      (k_insert_2384 true x_1053 true x_1053)
  br_f_insert_3687 b_3688 x_1053 xs1_2791 xs2_2791 ysys_1016 x1_1234 x2_1234 k_insert_2384 when (
      not b_3688) ->
      (xs_2208 x1_1234 x2_1234 x_1053 xs1_2791 xs2_2791 ysys_1016
        (f_insert_3659 x1_1234 x2_1234 x_1053 xs1_2791 xs2_2791 k_insert_2384))
  br_f_insert_3689 b_3690 x_1053 xs1_2791 xs2_2791 ysys_1016 x1_1234 x2_1234 k_insert_2384 when b_3690 ->
      (ysys_1016 (x1_1234 - 1) 0 (f_insert_3660 x1_1234 x2_1234 x_1053 xs1_2791 xs2_2791 k_insert_2384))
  br_f_insert_3689 b_3690 x_1053 xs1_2791 xs2_2791 ysys_1016 x1_1234 x2_1234 k_insert_2384 when (
      not b_3690) -> (ysys_1016 (x1_1234 - 1) (x2_1234 - 1) k_insert_2384)
  br_f_insert_3693 b_3694 x1_1447 x2_1447 x_1053 xs1_2791 xs2_2791 k_insert_2591 xs11_2710 xs12_2710 xs21_2710 
  xs22_2710 when b_3694 ->
      (k_insert_2650 x1_1447 x2_1447 x_1053 xs11_2710 xs12_2710 xs1_2791 xs21_2710 xs22_2710 xs2_2791 k_insert_2591
        (not (xs12_2710 = xs22_2710)))
  br_f_insert_3693 b_3694 x1_1447 x2_1447 x_1053 xs1_2791 xs2_2791 k_insert_2591 xs11_2710 xs12_2710 xs21_2710 
  xs22_2710 when (not b_3694) ->
      (k_insert_2650 x1_1447 x2_1447 x_1053 xs11_2710 xs12_2710 xs1_2791 xs21_2710 xs22_2710 xs2_2791 k_insert_2591
        false)
  br_f_insert_3695 b_3696 x_1053 xs1_2791 xs2_2791 ys''ys''_2772 ysys_1016 x1_1447 x2_1447 k_insert_2591 when b_3696 ->
      (k_insert_2591 true xs2_2791 true xs2_2791)
  br_f_insert_3695 b_3696 x_1053 xs1_2791 xs2_2791 ys''ys''_2772 ysys_1016 x1_1447 x2_1447 k_insert_2591 when (
      not b_3696) ->
      (xs_2206 x1_1447 x2_1447 x_1053 xs1_2791 xs2_2791 ysys_1016
        (f_insert_3663 x1_1447 x2_1447 x_1053 xs1_2791 xs2_2791 k_insert_2591))
  br_f_insert_3697 b_3698 x_1053 xs1_2791 xs2_2791 ys''ys''_2772 ysys_1016 x1_1447 x2_1447 k_insert_2591 when b_3698 ->
      (ys''ys''_2772 x1_1447 0 (f_insert_3664 x1_1447 x2_1447 x_1053 xs1_2791 xs2_2791 k_insert_2591))
  br_f_insert_3697 b_3698 x_1053 xs1_2791 xs2_2791 ys''ys''_2772 ysys_1016 x1_1447 x2_1447 k_insert_2591 when (
      not b_3698) -> (ys''ys''_2772 (x1_1447 - 1) (x2_1447 - 1) k_insert_2591)
  br_f_insert_3699 b_3700 x_1053 k_insert_2260 ysys_1016 xs1_2791 xs2_2791 when b_3700 ->
      (k_insert_2260 (f_insert_3658 x_1053 xs1_2791 xs2_2791 ysys_1016))
  br_f_insert_3699 b_3700 x_1053 k_insert_2260 ysys_1016 xs1_2791 xs2_2791 when (
      not b_3700) ->
      (ys''ys''_1034 x_1053 xs1_2791 xs2_2791 ysys_1016
        (f_insert_3661 x_1053 xs1_2791 xs2_2791 k_insert_2260 ysys_1016))
  br_k_insert_3683 b_3684 x1_1234 x2_1234 x_1053 xs11_2494 xs12_2494 xs1_2791 xs21_2494 xs22_2494 xs2_2791 
  k_insert_2384 b_3521 when b_3684 ->
      (k_insert_3538 b_3521 x1_1234 x2_1234 x_1053 xs11_2494 xs12_2494 xs1_2791 xs21_2494 xs22_2494 xs2_2791
        k_insert_2384 true)
  br_k_insert_3683 b_3684 x1_1234 x2_1234 x_1053 xs11_2494 xs12_2494 xs1_2791 xs21_2494 xs22_2494 xs2_2791 
  k_insert_2384 b_3521 when (not b_3684) ->
      (k_insert_3538 b_3521 x1_1234 x2_1234 x_1053 xs11_2494 xs12_2494 xs1_2791 xs21_2494 xs22_2494 xs2_2791
        k_insert_2384 (not (xs2_2791 = xs12_2494)))
  br_k_insert_3691 b_3692 x1_1447 x2_1447 x_1053 xs11_2710 xs12_2710 xs1_2791 xs21_2710 xs22_2710 xs2_2791 
  k_insert_2591 b_3452 when b_3692 ->
      (k_insert_3472 b_3452 x1_1447 x2_1447 x_1053 xs11_2710 xs12_2710 xs1_2791 xs21_2710 xs22_2710 xs2_2791
        k_insert_2591 true)
  br_k_insert_3691 b_3692 x1_1447 x2_1447 x_1053 xs11_2710 xs12_2710 xs1_2791 xs21_2710 xs22_2710 xs2_2791 
  k_insert_2591 b_3452 when (not b_3692) ->
      (k_insert_3472 b_3452 x1_1447 x2_1447 x_1053 xs11_2710 xs12_2710 xs1_2791 xs21_2710 xs22_2710 xs2_2791
        k_insert_2591 (not (xs2_2791 = xs12_2710)))
  check_2164 xsxs_1056 k_check_3093 -> (xs_2212 xsxs_1056 (f_check_3674 k_check_3093 xsxs_1056))
  f_3682 main_3381 -> end
  f_check_3674 k_check_3093 xsxs_1056 xs11_3183 xs12_3183 xs21_3183 xs22_3183 when (
      not xs11_3183) -> (k_check_3093 true)
  f_check_3674 k_check_3093 xsxs_1056 xs11_3183 xs12_3183 xs21_3183 xs22_3183 when (
      not (not xs11_3183)) ->
      (br_f_check_3703 (not xs21_3183) k_check_3093 xsxs_1056 xs11_3183 xs12_3183 xs21_3183 xs22_3183)
  f_insert_3654 x_1053 k_insert_2260 ysys_1016 xs1_2791 xs2_2791 when (
      not xs1_2791) -> (k_insert_2260 (rsrs_1021 x_1053 xs1_2791 xs2_2791))
  f_insert_3654 x_1053 k_insert_2260 ysys_1016 xs1_2791 xs2_2791 when (
      not (not xs1_2791)) -> (br_f_insert_3699 (x_1053 < xs2_2791) x_1053 k_insert_2260 ysys_1016 xs1_2791 xs2_2791)
  f_insert_3658 x_1053 xs1_2791 xs2_2791 ysys_1016 x1_1234 x2_1234 k_insert_2384 when (
      x1_1234 <= 0) ->
      (br_f_insert_3687 (x2_1234 <= 0) x_1053 xs1_2791 xs2_2791 ysys_1016 x1_1234 x2_1234 k_insert_2384)
  f_insert_3658 x_1053 xs1_2791 xs2_2791 ysys_1016 x1_1234 x2_1234 k_insert_2384 when (
      not (x1_1234 <= 0)) ->
      (br_f_insert_3689 (x2_1234 <= 0) x_1053 xs1_2791 xs2_2791 ysys_1016 x1_1234 x2_1234 k_insert_2384)
  f_insert_3659 x1_1234 x2_1234 x_1053 xs1_2791 xs2_2791 k_insert_2384 xs11_2494 xs12_2494 xs21_2494 xs22_2494 when (
      not xs21_2494) -> (loop_2167 () k_insert_2384)
  f_insert_3659 x1_1234 x2_1234 x_1053 xs1_2791 xs2_2791 k_insert_2384 xs11_2494 xs12_2494 xs21_2494 xs22_2494 when (
      not (not xs21_2494)) ->
      (br_f_insert_3685 ((x2_1234 - 1) = 0) x1_1234 x2_1234 x_1053 xs1_2791 xs2_2791 k_insert_2384 xs11_2494 xs12_2494
        xs21_2494 xs22_2494)
  f_insert_3660 x1_1234 x2_1234 x_1053 xs1_2791 xs2_2791 k_insert_2384 p11_3512 p12_3512 p21_3512 p22_3512 ->
      (k_insert_2384 p11_3512 p12_3512 true x_1053)
  f_insert_3661 x_1053 xs1_2791 xs2_2791 k_insert_2260 ysys_1016 ys''ys''_2772 ->
      (k_insert_2260 (f_insert_3662 x_1053 xs1_2791 xs2_2791 ys''ys''_2772 ysys_1016))
  f_insert_3662 x_1053 xs1_2791 xs2_2791 ys''ys''_2772 ysys_1016 x1_1447 x2_1447 k_insert_2591 when (
      x1_1447 <= 0) ->
      (br_f_insert_3695 (x2_1447 <= 0) x_1053 xs1_2791 xs2_2791 ys''ys''_2772 ysys_1016 x1_1447 x2_1447 k_insert_2591)
  f_insert_3662 x_1053 xs1_2791 xs2_2791 ys''ys''_2772 ysys_1016 x1_1447 x2_1447 k_insert_2591 when (
      not (x1_1447 <= 0)) ->
      (br_f_insert_3697 (x2_1447 <= 0) x_1053 xs1_2791 xs2_2791 ys''ys''_2772 ysys_1016 x1_1447 x2_1447 k_insert_2591)
  f_insert_3663 x1_1447 x2_1447 x_1053 xs1_2791 xs2_2791 k_insert_2591 xs11_2710 xs12_2710 xs21_2710 xs22_2710 when (
      not xs21_2710) -> (loop_2167 () k_insert_2591)
  f_insert_3663 x1_1447 x2_1447 x_1053 xs1_2791 xs2_2791 k_insert_2591 xs11_2710 xs12_2710 xs21_2710 xs22_2710 when (
      not (not xs21_2710)) ->
      (br_f_insert_3693 ((x2_1447 - 1) = 0) x1_1447 x2_1447 x_1053 xs1_2791 xs2_2791 k_insert_2591 xs11_2710 xs12_2710
        xs21_2710 xs22_2710)
  f_insert_3664 x1_1447 x2_1447 x_1053 xs1_2791 xs2_2791 k_insert_2591 p11_3440 p12_3440 p21_3440 p22_3440 ->
      (k_insert_2591 p11_3440 p12_3440 true xs2_2791)
  f_insertsort_3665 k_insertsort_2811 xsxs_1042 p11_3576 p12_3576 p21_3576 p22_3576 ->
      (is_none_2185 p11_3576 p12_3576
        (f_insertsort_3666 p11_3576 p12_3576 p21_3576 p22_3576 k_insertsort_2811 xsxs_1042))
  f_insertsort_3666 p11_3576 p12_3576 p21_3576 p22_3576 k_insertsort_2811 xsxs_1042 b_3579 when b_3579 ->
      (k_insertsort_2811 (rsrs_3608 b_3579 p11_3576 p12_3576 p21_3576 p22_3576))
  f_insertsort_3666 p11_3576 p12_3576 p21_3576 p22_3576 k_insertsort_2811 xsxs_1042 b_3579 when (
      not b_3579) ->
      (xs_3581 b_3579 p11_3576 p12_3576 p21_3576 p22_3576 xsxs_1042
        (f_insertsort_3668 b_3579 p11_3576 p12_3576 p21_3576 p22_3576 k_insertsort_2811 xsxs_1042))
  f_insertsort_3668 b_3579 p11_3576 p12_3576 p21_3576 p22_3576 k_insertsort_2811 xsxs_1042 xs1_3590 xs2_3590 ->
      (insertsort_2165 (xs'xs'_3592 b_3579 p11_3576 p12_3576 p21_3576 p22_3576 xs1_3590 xs2_3590 xsxs_1042)
        (f_insertsort_3669 b_3579 p11_3576 p12_3576 p21_3576 p22_3576 xs1_3590 xs2_3590 k_insertsort_2811))
  f_insertsort_3669 b_3579 p11_3576 p12_3576 p21_3576 p22_3576 xs1_3590 xs2_3590 k_insertsort_2811 x_3625 ->
      (insert_1014 xs2_3590 x_3625 k_insertsort_2811)
  f_main_3675 n_1065 k_main_3193 xs_3352 -> (ysys_1071 n_1065 xs_3352 (f_main_3679 n_1065 k_main_3193))
  f_main_3679 n_1065 k_main_3193 ysys_3344 -> (check_2164 ysys_3344 (f_main_3680 n_1065 k_main_3193))
  f_main_3680 n_1065 k_main_3193 b_3643 when b_3643 -> (k_main_3193 ())
  f_main_3680 n_1065 k_main_3193 b_3643 when (not b_3643) -> (fail_3705 true k_main_3193)
  f_main_3681 k_main_3359 arg1_3380 -> (main_1064 arg1_3380 k_main_3359)
  f_make_list_3670 n_1050 i_1051 k_make_list_3000 -> (k_make_list_3000 false 0)
  f_make_list_3671 n_1050 k_make_list_2998 n_3073 ->
      (xs_1053 n_1050 n_3073 (f_make_list_3672 n_1050 n_3073 k_make_list_2998))
  f_make_list_3672 n_1050 n_3073 k_make_list_2998 xs_3072 ->
      (k_make_list_2998 (f_make_list_3673 n_1050 n_3073 xs_3072))
  f_make_list_3673 n_1050 n_3073 xs_3072 i_1054 k_make_list_3049 when (i_1054 = 0) -> (k_make_list_3049 true n_3073)
  f_make_list_3673 n_1050 n_3073 xs_3072 i_1054 k_make_list_3049 when (
      not (i_1054 = 0)) -> (xs_3072 (i_1054 - 1) k_make_list_3049)
  f_rsrs_3655 x1_1147 x2_1147 x_1053 xs1_2791 xs2_2791 k_insert_rsrs_2307 r1_2325 r2_2325 ->
      (k_insert_rsrs_2307 r1_2325 r2_2325 r1_2325 r2_2325)
  f_rsrs_3656 x1_1147 x2_1147 x_1053 xs1_2791 xs2_2791 k_insert_rsrs_2307 x1_3568 x2_3568 ->
      (rs_1019 x_1053 xs1_2791 xs2_2791 x2_1147
        (f_rsrs_3657 x1_1147 x1_3568 x2_1147 x2_3568 x_1053 xs1_2791 xs2_2791 k_insert_rsrs_2307))
  f_rsrs_3657 x1_1147 x1_3568 x2_1147 x2_3568 x_1053 xs1_2791 xs2_2791 k_insert_rsrs_2307 x1_3573 x2_3573 ->
      (k_insert_rsrs_2307 x1_3568 x2_3568 x1_3573 x2_3573)
  f_xs_3653 x_1053 k_insert_xs_2267 p11_3433 p12_3433 p21_3433 p22_3433 -> (k_insert_xs_2267 p11_3433 p12_3433)
  f_xs_3667 b_3579 p11_3576 p12_3576 p21_3576 p22_3576 k_insertsort_xs_3582 p11_3622 p12_3622 p21_3622 p22_3622 ->
      (k_insertsort_xs_3582 p11_3622 p12_3622)
  f_xsxs_3676 n_1065 x1_1976 x2_1976 k_main_xsxs_3221 r1_3239 r2_3239 ->
      (k_main_xsxs_3221 r1_3239 r2_3239 r1_3239 r2_3239)
  f_xsxs_3677 n_1065 x1_1976 x2_1976 k_main_xsxs_3221 xs_3352 x1_3637 x2_3637 ->
      (xs_3352 x2_1976 (f_xsxs_3678 n_1065 x1_1976 x1_3637 x2_1976 x2_3637 k_main_xsxs_3221))
  f_xsxs_3678 n_1065 x1_1976 x1_3637 x2_1976 x2_3637 k_main_xsxs_3221 x1_3642 x2_3642 ->
      (k_main_xsxs_3221 x1_3637 x2_3637 x1_3642 x2_3642)
  fail_3705 b k -> {fail} => (k ())
  insert_1014 x_1053 ysys_1016 k_insert_2260 ->
      (xs_2203 x_1053 ysys_1016 (f_insert_3654 x_1053 k_insert_2260 ysys_1016))
  insertsort_2165 xsxs_1042 k_insertsort_2811 -> (xsxs_1042 0 0 (f_insertsort_3665 k_insertsort_2811 xsxs_1042))
  is_none_2185 x1_1012 x2_1012 k_is_none_2230 -> (k_is_none_2230 (not x1_1012))
  k_insert_2437 x1_1234 x2_1234 x_1053 xs11_2494 xs12_2494 xs1_2791 xs21_2494 xs22_2494 xs2_2791 k_insert_2384 b_3521 when b_3521 ->
      (loop_2167 () k_insert_2384)
  k_insert_2437 x1_1234 x2_1234 x_1053 xs11_2494 xs12_2494 xs1_2791 xs21_2494 xs22_2494 xs2_2791 k_insert_2384 b_3521 when (
      not b_3521) ->
      (br_k_insert_3683 (not (xs1_2791 <=> xs11_2494)) x1_1234 x2_1234 x_1053 xs11_2494 xs12_2494 xs1_2791 xs21_2494
        xs22_2494 xs2_2791 k_insert_2384 b_3521)
  k_insert_2650 x1_1447 x2_1447 x_1053 xs11_2710 xs12_2710 xs1_2791 xs21_2710 xs22_2710 xs2_2791 k_insert_2591 b_3452 when b_3452 ->
      (loop_2167 () k_insert_2591)
  k_insert_2650 x1_1447 x2_1447 x_1053 xs11_2710 xs12_2710 xs1_2791 xs21_2710 xs22_2710 xs2_2791 k_insert_2591 b_3452 when (
      not b_3452) ->
      (br_k_insert_3691 (not (xs1_2791 <=> xs11_2710)) x1_1447 x2_1447 x_1053 xs11_2710 xs12_2710 xs1_2791 xs21_2710
        xs22_2710 xs2_2791 k_insert_2591 b_3452)
  k_insert_3472 b_3452 x1_1447 x2_1447 x_1053 xs11_2710 xs12_2710 xs1_2791 xs21_2710 xs22_2710 xs2_2791 k_insert_2591 
  b_3477 when b_3477 -> (loop_2167 () k_insert_2591)
  k_insert_3472 b_3452 x1_1447 x2_1447 x_1053 xs11_2710 xs12_2710 xs1_2791 xs21_2710 xs22_2710 xs2_2791 k_insert_2591 
  b_3477 when (not b_3477) -> (k_insert_2591 true xs2_2791 true xs22_2710)
  k_insert_3538 b_3521 x1_1234 x2_1234 x_1053 xs11_2494 xs12_2494 xs1_2791 xs21_2494 xs22_2494 xs2_2791 k_insert_2384 
  b_3543 when b_3543 -> (loop_2167 () k_insert_2384)
  k_insert_3538 b_3521 x1_1234 x2_1234 x_1053 xs11_2494 xs12_2494 xs1_2791 xs21_2494 xs22_2494 xs2_2791 k_insert_2384 
  b_3543 when (not b_3543) -> (k_insert_2384 true x_1053 true xs22_2494)
  loop_2167 x_1028 k_loop_2237 -> (loop_2167 () k_loop_2237)
  main_1064 n_1065 k_main_3193 -> (xs_1066 n_1065 (f_main_3675 n_1065 k_main_3193))
  main_2163 k_main_3359 -> (arg1_2161 (f_main_3681 k_main_3359))
  make_list_1049 n_1050 k_make_list_2998 when (n_1050 = 0) -> (k_make_list_2998 (f_make_list_3670 n_1050))
  make_list_1049 n_1050 k_make_list_2998 when (not (n_1050 = 0)) ->
      (n_1052 n_1050 (f_make_list_3671 n_1050 k_make_list_2998))
  n_1052 n_1050 k_make_list_n_3015 -> (rand_int k_make_list_n_3015)
  r_1024 x1_1147 x2_1147 x_1053 xs1_2791 xs2_2791 k_insert_rsrs_r_2314 ->
      (rs_1019 x_1053 xs1_2791 xs2_2791 x1_1147 k_insert_rsrs_r_2314)
  r_1070 n_1065 x1_1976 x2_1976 xs_3352 k_main_xsxs_r_3228 -> (xs_3352 x1_1976 k_main_xsxs_r_3228)
  rs_1019 x_1053 xs1_2791 xs2_2791 i_1020 k_insert_rs_2288 when (i_1020 <= 0) -> (k_insert_rs_2288 true x_1053)
  rs_1019 x_1053 xs1_2791 xs2_2791 i_1020 k_insert_rs_2288 when (not (i_1020 <= 0)) -> (k_insert_rs_2288 false 0)
  rsrs_1021 x_1053 xs1_2791 xs2_2791 x1_1147 x2_1147 k_insert_rsrs_2307 when (
      x1_1147 = x2_1147) ->
      (r_1024 x1_1147 x2_1147 x_1053 xs1_2791 xs2_2791
        (f_rsrs_3655 x1_1147 x2_1147 x_1053 xs1_2791 xs2_2791 k_insert_rsrs_2307))
  rsrs_1021 x_1053 xs1_2791 xs2_2791 x1_1147 x2_1147 k_insert_rsrs_2307 when (
      not (x1_1147 = x2_1147)) ->
      (rs_1019 x_1053 xs1_2791 xs2_2791 x1_1147
        (f_rsrs_3656 x1_1147 x2_1147 x_1053 xs1_2791 xs2_2791 k_insert_rsrs_2307))
  rsrs_3608 b_3579 p11_3576 p12_3576 p21_3576 p22_3576 i1_3609 i2_3609 k_insertsort_rsrs_3610 ->
      (k_insertsort_rsrs_3610 false 0 false 0)
  xs'xs'_1061 xs11_3183 xs12_3183 xs21_3183 xs22_3183 xsxs_1056 x1_1917 x2_1917 k_check_xs'xs'_3122 ->
      (xsxs_1056 (x1_1917 + 1) (x2_1917 + 1) k_check_xs'xs'_3122)
  xs'xs'_3592 b_3579 p11_3576 p12_3576 p21_3576 p22_3576 xs1_3590 xs2_3590 xsxs_1042 x1_3593 x2_3593 
  k_insertsort_xs'xs'_3594 -> (xsxs_1042 (x1_3593 + 1) (x2_3593 + 1) k_insertsort_xs'xs'_3594)
  xs_1053 n_1050 n_3073 k_make_list_xs_3036 -> (make_list_1049 (n_3073 - 1) k_make_list_xs_3036)
  xs_1066 n_1065 k_main_xs_3206 -> (make_list_1049 n_1065 k_main_xs_3206)
  xs_2203 x_1053 ysys_1016 k_insert_xs_2267 -> (ysys_1016 0 0 (f_xs_3653 x_1053 k_insert_xs_2267))
  xs_2206 x1_1447 x2_1447 x_1053 xs1_2791 xs2_2791 ysys_1016 k_insert_xs_2624 ->
      (ysys_1016 0 (x2_1447 - 1) k_insert_xs_2624)
  xs_2208 x1_1234 x2_1234 x_1053 xs1_2791 xs2_2791 ysys_1016 k_insert_xs_2411 ->
      (ysys_1016 0 (x2_1234 - 1) k_insert_xs_2411)
  xs_2212 xsxs_1056 k_check_xs_3105 -> (xsxs_1056 0 0 k_check_xs_3105)
  xs_3581 b_3579 p11_3576 p12_3576 p21_3576 p22_3576 xsxs_1042 k_insertsort_xs_3582 ->
      (xsxs_1042 0 0 (f_xs_3667 b_3579 p11_3576 p12_3576 p21_3576 p22_3576 k_insertsort_xs_3582))
  xsxs_1067 n_1065 xs_3352 x1_1976 x2_1976 k_main_xsxs_3221 when (x1_1976 = x2_1976) ->
      (r_1070 n_1065 x1_1976 x2_1976 xs_3352 (f_xsxs_3676 n_1065 x1_1976 x2_1976 k_main_xsxs_3221))
  xsxs_1067 n_1065 xs_3352 x1_1976 x2_1976 k_main_xsxs_3221 when (not (x1_1976 = x2_1976)) ->
      (xs_3352 x1_1976 (f_xsxs_3677 n_1065 x1_1976 x2_1976 k_main_xsxs_3221 xs_3352))
  ys''ys''_1034 x_1053 xs1_2791 xs2_2791 ysys_1016 k_insert_ys''ys''_2572 ->
      (insert_1014 x_1053 (ys'ys'_1025 x_1053 xs1_2791 xs2_2791 ysys_1016) k_insert_ys''ys''_2572)
  ys'ys'_1025 x_1053 xs1_2791 xs2_2791 ysys_1016 x1_1206 x2_1206 k_insert_ys'ys'_2364 ->
      (ysys_1016 (x1_1206 + 1) (x2_1206 + 1) k_insert_ys'ys'_2364)
  ysys_1071 n_1065 xs_3352 k_main_ysys_3290 -> (insertsort_2165 (xsxs_1067 n_1065 xs_3352) k_main_ysys_3290)
Types:
  main_3652 : X
  check_2164 : ((x_2:int ->
                 x_3:int ->
                 (x_5:bool ->
                  x_6:int ->
                  x_7:bool -> x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_2 < 0 || x_3 < 0 || x_6 <= x_8]
                  -> X)
                 -> X)
                -> (x_12:bool[x_12] -> X) -> X)
  fail_3705 : (bool -> (unit -> X) -> X)
  insert_1014 : (int ->
                 (x_3:int ->
                  x_4:int ->
                  (x_6:bool ->
                   x_7:int ->
                   x_8:bool ->
                   x_9:int[(not x_6) || (not x_8) || (not (x_3 <= x_4)) || x_3 < 0 || x_4 < 0 || x_7 <= x_9] -> X)
                  -> X)
                 ->
                 ((x_14:int ->
                   x_15:int ->
                   (x_17:bool ->
                    x_18:int ->
                    x_19:bool ->
                    x_20:int[(not x_17) || (not x_19) || (not (x_14 <= x_15)) || x_14 < 0 || x_15 < 0 || x_18 <= x_20]
                    -> X)
                   -> X)
                 -> X) -> X)
  insertsort_2165 : ((int -> int -> (bool -> int -> bool -> int -> X) -> X) ->
                     ((x_13:int ->
                       x_14:int ->
                       (x_16:bool ->
                        x_17:int ->
                        x_18:bool ->
                        x_19:int[(not x_16) || (not x_18) || (not (x_13 <= x_14)) || 
                                 x_13 < 0 || x_14 < 0 || x_17 <= x_19]
                        -> X)
                       -> X)
                     -> X) -> X)
  loop_2167 : (unit -> (bool -> int -> bool -> int -> X) -> X)
  make_list_1049 : (int -> ((int -> (bool -> int -> X) -> X) -> X) -> X)

(0-1) Abstracting ... EXPAND_NONREC:
Main: main_3652
  main_3652 ->
      (rand_int
        (fun arg1_4748 ->
         (make_list_1049 arg1_4748
           (fun xs_4737 ->
            (insertsort_2165
              (fun x1_5030 x2_5031 k_main_xsxs_5032 ->
               (if (x2_5031 = x1_5030)
                 (l0 (xs_4737 x1_5030 (fun r1_4815 r2_4816 -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816))))
                 (l1
                   (xs_4737 x1_5030
                     (fun x1_4822 x2_4823 ->
                      (xs_4737 x2_5031 (fun x1_4830 x2_4831 -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831))))))))
              (fun ysys_4740 ->
               (check_2164 ysys_4740 (fun b_4746 -> (if b_4746 (l0 end) (l1 (fail_3705 true (fun main_4578 -> end))))))))))))
  check_2164 xsxs_1056 k_check_3093 ->
      (xsxs_1056 0 0
        (fun xs11_4587 xs12_4588 xs21_4589 xs22_4590 ->
         (if xs11_4587
           (l1
             (if xs21_4589
               (l1
                 (if (xs22_4590 >= xs12_4588)
                   (l0
                     (check_2164
                       (fun x1_4978 x2_4979 k_check_xs'xs'_4980 ->
                        (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1) k_check_xs'xs'_4980)) k_check_3093))
                   (l1 (k_check_3093 false)))) (l0 (k_check_3093 true)))) (
           l0 (k_check_3093 true)))))
  fail_3705 b k -> {fail} => (k ())
  insert_1014 x_1053 ysys_1016 k_insert_2260 ->
      (ysys_1016 0 0
        (fun p11_4797 p12_4798 p21_4799 p22_4800 ->
         (if p11_4797
           (l1
             (if (x_1053 < p12_4798)
               (l0
                 (k_insert_2260
                   (fun x1_4612 x2_4613 k_insert_4614 ->
                    (if (x1_4612 <= 0)
                      (l0
                        (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
                          (l1
                            (ysys_1016 0 (x2_4613 - 1)
                              (fun xs11_4631 xs12_4632 xs21_4633 xs22_4634 ->
                               (if xs21_4633
                                 (l1
                                   (if (1 = x2_4613)
                                     (l0
                                       (if (xs22_4634 = xs12_4632)
                                         (l1
                                           (if (p11_4797 <=> xs11_4631)
                                             (l1
                                               (if (xs12_4632 = p12_4798)
                                                 (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                                 (l0 (loop_2167 () k_insert_4614))))
                                             (l0 (l0 (loop_2167 () k_insert_4614))))) (
                                         l0 (loop_2167 () k_insert_4614))))
                                     (l1
                                       (l1
                                         (if (p11_4797 <=> xs11_4631)
                                           (l1
                                             (if (xs12_4632 = p12_4798) (
                                               l1 (k_insert_4614 true x_1053 true xs22_4634))
                                               (l0 (loop_2167 () k_insert_4614))))
                                           (l0 (l0 (loop_2167 () k_insert_4614))))))))
                                 (l0 (loop_2167 () k_insert_4614))))))))
                      (l1
                        (if (x2_4613 <= 0)
                          (l0
                            (ysys_1016 (x1_4612 - 1) 0
                              (fun p11_4641 p12_4642 p21_4643 p22_4644 -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
                          (l1 (ysys_1016 (x1_4612 - 1) (x2_4613 - 1) k_insert_4614))))))))
               (l1
                 (insert_1014 x_1053
                   (fun x1_5042 x2_5043 k_insert_ys'ys'_5044 ->
                    (ysys_1016 (x1_5042 + 1) (x2_5043 + 1) k_insert_ys'ys'_5044))
                   (fun ys''ys''_4650 ->
                    (k_insert_2260
                      (fun x1_4664 x2_4665 k_insert_4666 ->
                       (if (x1_4664 <= 0)
                         (l0
                           (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                             (l1
                               (ysys_1016 0 (x2_4665 - 1)
                                 (fun xs11_4683 xs12_4684 xs21_4685 xs22_4686 ->
                                  (if xs21_4685
                                    (l1
                                      (if (1 = x2_4665)
                                        (l0
                                          (if (xs22_4686 = xs12_4684)
                                            (l1
                                              (if (p11_4797 <=> xs11_4683)
                                                (l1
                                                  (if (xs12_4684 = p12_4798)
                                                    (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                    (l0 (loop_2167 () k_insert_4666))))
                                                (l0 (l0 (loop_2167 () k_insert_4666)))))
                                            (l0 (loop_2167 () k_insert_4666))))
                                        (l1
                                          (l1
                                            (if (p11_4797 <=> xs11_4683)
                                              (l1
                                                (if (xs12_4684 = p12_4798)
                                                  (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                  (l0 (loop_2167 () k_insert_4666))))
                                              (l0 (l0 (loop_2167 () k_insert_4666))))))))
                                    (l0 (loop_2167 () k_insert_4666))))))))
                         (l1
                           (if (x2_4665 <= 0)
                             (l0
                               (ys''ys''_4650 x1_4664 0
                                 (fun p11_4693 p12_4694 p21_4695 p22_4696 ->
                                  (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                             (l1 (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1) k_insert_4666))))))))))))
           (l0
             (k_insert_2260
               (fun x1_4962 x2_4963 k_insert_rsrs_4964 ->
                (if (x2_4963 = x1_4962)
                  (l0
                    (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                      (l1 (k_insert_rsrs_4964 false 0 false 0))))
                  (l1
                    (if (x1_4962 <= 0)
                      (l0
                        (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                          (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
                      (l1
                        (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                          (l1 (k_insert_rsrs_4964 false 0 false 0)))))))))))))
  insertsort_2165 xsxs_1042 k_insertsort_2811 ->
      (xsxs_1042 0 0
        (fun p11_4699 p12_4700 p21_4701 p22_4702 ->
         (if p11_4699
           (l1
             (xsxs_1042 0 0
               (fun p11_4807 p12_4808 p21_4809 p22_4810 ->
                (insertsort_2165
                  (fun x1_4989 x2_4990 k_insertsort_xs'xs'_4991 ->
                   (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1) k_insertsort_xs'xs'_4991))
                  (fun x_4734 -> (insert_1014 p12_4808 x_4734 k_insertsort_2811))))))
           (l0
             (k_insertsort_2811
               (fun i1_4970 i2_4971 k_insertsort_rsrs_4972 -> (k_insertsort_rsrs_4972 false 0 false 0)))))))
  loop_2167 x_1028 k_loop_2237 -> (loop_2167 () k_loop_2237)
  make_list_1049 n_1050 k_make_list_2998 when (n_1050 = 0) ->
      (l0 (k_make_list_2998 (fun i_4750 k_make_list_4751 -> (k_make_list_4751 false 0))))
  make_list_1049 n_1050 k_make_list_2998 when (not (n_1050 = 0)) ->
      (l1
        (rand_int
          (fun n_4754 ->
           (make_list_1049 (n_4754 - 1)
             (fun xs_4758 ->
              (k_make_list_2998
                (fun i_4767 k_make_list_4768 ->
                 (if (i_4767 = 0) (l0 (k_make_list_4768 true n_4754)) (l1 (xs_4758 (i_4767 - 1) k_make_list_4768))))))))))

ETA: (rand_int
       (fun arg1_4748 ->
        (make_list_1049 arg1_4748
          (fun xs_4737 ->
           (insertsort_2165
             (fun x1_5030 x2_5031 k_main_xsxs_5032 ->
              (if (x2_5031 = x1_5030)
                (l0 (xs_4737 x1_5030 (fun r1_4815 r2_4816 -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816))))
                (l1
                  (xs_4737 x1_5030
                    (fun x1_4822 x2_4823 ->
                     (xs_4737 x2_5031 (fun x1_4830 x2_4831 -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831))))))))
             (fun ysys_4740 ->
              (check_2164 ysys_4740 (fun b_4746 -> (if b_4746 (l0 end) (l1 (fail_3705 true (fun main_4578 -> end)))))))))))): X
ETA: (fun arg1_4748 ->
      (make_list_1049 arg1_4748
        (fun xs_4737 ->
         (insertsort_2165
           (fun x1_5030 x2_5031 k_main_xsxs_5032 ->
            (if (x2_5031 = x1_5030)
              (l0 (xs_4737 x1_5030 (fun r1_4815 r2_4816 -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816))))
              (l1
                (xs_4737 x1_5030
                  (fun x1_4822 x2_4823 ->
                   (xs_4737 x2_5031 (fun x1_4830 x2_4831 -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831))))))))
           (fun ysys_4740 ->
            (check_2164 ysys_4740 (fun b_4746 -> (if b_4746 (l0 end) (l1 (fail_3705 true (fun main_4578 -> end))))))))))): (int
->
unit)
ETA: (make_list_1049 arg1_4748
       (fun xs_4737 ->
        (insertsort_2165
          (fun x1_5030 x2_5031 k_main_xsxs_5032 ->
           (if (x2_5031 = x1_5030)
             (l0 (xs_4737 x1_5030 (fun r1_4815 r2_4816 -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816))))
             (l1
               (xs_4737 x1_5030
                 (fun x1_4822 x2_4823 ->
                  (xs_4737 x2_5031 (fun x1_4830 x2_4831 -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831))))))))
          (fun ysys_4740 ->
           (check_2164 ysys_4740 (fun b_4746 -> (if b_4746 (l0 end) (l1 (fail_3705 true (fun main_4578 -> end)))))))))): unit
ETA: (fun xs_4737 ->
      (insertsort_2165
        (fun x1_5030 x2_5031 k_main_xsxs_5032 ->
         (if (x2_5031 = x1_5030)
           (l0 (xs_4737 x1_5030 (fun r1_4815 r2_4816 -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816))))
           (l1
             (xs_4737 x1_5030
               (fun x1_4822 x2_4823 ->
                (xs_4737 x2_5031 (fun x1_4830 x2_4831 -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831))))))))
        (fun ysys_4740 ->
         (check_2164 ysys_4740 (fun b_4746 -> (if b_4746 (l0 end) (l1 (fail_3705 true (fun main_4578 -> end))))))))): ((
int -> (bool -> int -> X) -> X) ->
X)
ETA: (insertsort_2165
       (fun x1_5030 x2_5031 k_main_xsxs_5032 ->
        (if (x2_5031 = x1_5030)
          (l0 (xs_4737 x1_5030 (fun r1_4815 r2_4816 -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816))))
          (l1
            (xs_4737 x1_5030
              (fun x1_4822 x2_4823 ->
               (xs_4737 x2_5031 (fun x1_4830 x2_4831 -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831))))))))
       (fun ysys_4740 ->
        (check_2164 ysys_4740 (fun b_4746 -> (if b_4746 (l0 end) (l1 (fail_3705 true (fun main_4578 -> end)))))))): X
ETA: (fun ysys_4740 ->
      (check_2164 ysys_4740 (fun b_4746 -> (if b_4746 (l0 end) (l1 (fail_3705 true (fun main_4578 -> end))))))): ((
x_2:int ->
x_3:int ->
(x_5:bool ->
 x_6:int -> x_7:bool -> x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_2 < 0 || x_3 < 0 || x_6 <= x_8] -> X)
-> X) ->
X)
ETA: (check_2164 ysys_4740 (fun b_4746 -> (if b_4746 (l0 end) (l1 (fail_3705 true (fun main_4578 -> end)))))): X
ETA: (fun b_4746 -> (if b_4746 (l0 end) (l1 (fail_3705 true (fun main_4578 -> end))))): (x_1:bool[
x_1] ->
X)
ETA: (if b_4746 (l0 end) (l1 (fail_3705 true (fun main_4578 -> end)))): X
ETA: (l1 (fail_3705 true (fun main_4578 -> end))): X
ETA: (fail_3705 true (fun main_4578 -> end)): X
ETA: (fun main_4578 -> end): (unit ->
X)
ETA: end: X
ETA: true: bool
ETA_AUX: (fail_3705 true (fun (main_4578:unit) -> end)): X
ETA: (l0 end): X
ETA: end: X
ETA: ysys_4740: (x_1:int ->
                 x_2:int ->
                 (x_4:bool ->
                  x_5:int ->
                  x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7]
                  -> X)
                 -> X)
ETA_AUX: ysys_4740: (x_1:int ->
                     x_2:int ->
                     (x_4:bool ->
                      x_5:int ->
                      x_6:bool ->
                      x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
                     -> X)
ETA_AUX: x__5048: int
ETA_AUX: (ysys_4740 x__5048): (x_1:int ->
                               (x_3:bool ->
                                x_4:int ->
                                x_5:bool ->
                                x_6:int[(not x_3) || (not x_5) || (not (x__5048 <= x_1)) || 
                                        x__5048 < 0 || x_1 < 0 || x_4 <= x_6]
                                -> X)
                               -> X)
ETA_AUX: x__5049: int
ETA_AUX: (ysys_4740 x__5048 x__5049): ((x_2:bool ->
                                        x_3:int ->
                                        x_4:bool ->
                                        x_5:int[(not x_2) || (not x_4) || (
                                                not (x__5048 <= x__5049)) || 
                                                x__5048 < 0 || x__5049 < 0 || 
                                                x_3 <= x_5]
                                        -> X) ->
X)
ETA_AUX: x__5050: (x_1:bool ->
                   x_2:int ->
                   x_3:bool ->
                   x_4:int[(not x_1) || (not x_3) || (not (x__5048 <= x__5049)) || 
                           x__5048 < 0 || x__5049 < 0 || x_2 <= x_4]
                   -> X)
ETA_AUX: x__5051: bool
ETA_AUX: (x__5050 x__5051): (x_1:int ->
                             x_2:bool ->
                             x_3:int[(not x__5051) || (not x_2) || (not (x__5048 <= x__5049)) || 
                                     x__5048 < 0 || x__5049 < 0 || x_1 <= x_3]
                             -> X)
ETA_AUX: x__5052: int
ETA_AUX: (x__5050 x__5051 x__5052): (x_1:bool ->
                                     x_2:int[(not x__5051) || (not x_1) || (
                                             not (x__5048 <= x__5049)) || 
                                             x__5048 < 0 || x__5049 < 0 || 
                                             x__5052 <= x_2]
                                     -> X)
ETA_AUX: x__5053: bool
ETA_AUX: (x__5050 x__5051 x__5052 x__5053): (x_1:int[(not x__5051) || (
                                                     not x__5053) || (
                                                     not (x__5048 <= x__5049)) || 
                                                     x__5048 < 0 || x__5049 < 0 || 
                                                     x__5052 <= x_1] ->
X)
ETA_AUX: x__5054: x_1:int[(not x__5051) || (not x__5053) || (not (x__5048 <= x__5049)) || 
                          x__5048 < 0 || x__5049 < 0 || x__5052 <= x_1]
ETA_AUX: (x__5050 x__5051 x__5052 x__5053 x__5054): X
ETA_AUX: (ysys_4740 x__5048 x__5049
           (fun (x__5051:bool) (x__5052:int) (x__5053:bool) 
                (x__5054:x_1:int[(not x__5051) || (not x__5053) || (not (x__5048 <= x__5049)) || 
                                 x__5048 < 0 || x__5049 < 0 || x__5052 <= x_1])
            -> (x__5050 x__5051 x__5052 x__5053 x__5054))): X
ETA_AUX: (check_2164
           (fun (x__5048:int) (x__5049:int) 
                (x__5050:(x_1:bool ->
                          x_2:int ->
                          x_3:bool ->
                          x_4:int[(not x_1) || (not x_3) || (not (x__5048 <= x__5049)) || 
                                  x__5048 < 0 || x__5049 < 0 || x_2 <= x_4]
                          -> X))
            ->
            (ysys_4740 x__5048 x__5049
              (fun (x__5051:bool) (x__5052:int) (x__5053:bool) 
                   (x__5054:x_1:int[(not x__5051) || (not x__5053) || (
                                    not (x__5048 <= x__5049)) || x__5048 < 0 || 
                                    x__5049 < 0 || x__5052 <= x_1])
               -> (x__5050 x__5051 x__5052 x__5053 x__5054))))
           (fun (b_4746:x_1:bool[x_1]) -> (if b_4746 (l0 end) (l1 (fail_3705 true (fun (main_4578:unit) -> end)))))): X
ETA: (fun x1_5030 x2_5031 k_main_xsxs_5032 ->
      (if (x2_5031 = x1_5030)
        (l0 (xs_4737 x1_5030 (fun r1_4815 r2_4816 -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816))))
        (l1
          (xs_4737 x1_5030
            (fun x1_4822 x2_4823 ->
             (xs_4737 x2_5031 (fun x1_4830 x2_4831 -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831)))))))): (
int -> int -> (bool -> int -> bool -> int -> X) -> X)
ETA: (fun x2_5031 k_main_xsxs_5032 ->
      (if (x2_5031 = x1_5030)
        (l0 (xs_4737 x1_5030 (fun r1_4815 r2_4816 -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816))))
        (l1
          (xs_4737 x1_5030
            (fun x1_4822 x2_4823 ->
             (xs_4737 x2_5031 (fun x1_4830 x2_4831 -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831)))))))): (
int -> (bool -> int -> bool -> int -> X) -> X)
ETA: (fun k_main_xsxs_5032 ->
      (if (x2_5031 = x1_5030)
        (l0 (xs_4737 x1_5030 (fun r1_4815 r2_4816 -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816))))
        (l1
          (xs_4737 x1_5030
            (fun x1_4822 x2_4823 ->
             (xs_4737 x2_5031 (fun x1_4830 x2_4831 -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831)))))))): ((
bool -> int -> bool -> int -> X) ->
X)
ETA: (if (x2_5031 = x1_5030)
       (l0 (xs_4737 x1_5030 (fun r1_4815 r2_4816 -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816))))
       (l1
         (xs_4737 x1_5030
           (fun x1_4822 x2_4823 ->
            (xs_4737 x2_5031 (fun x1_4830 x2_4831 -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831))))))): X
ETA: (l1
       (xs_4737 x1_5030
         (fun x1_4822 x2_4823 ->
          (xs_4737 x2_5031 (fun x1_4830 x2_4831 -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831)))))): X
ETA: (xs_4737 x1_5030
       (fun x1_4822 x2_4823 ->
        (xs_4737 x2_5031 (fun x1_4830 x2_4831 -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831))))): X
ETA: (fun x1_4822 x2_4823 ->
      (xs_4737 x2_5031 (fun x1_4830 x2_4831 -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831)))): (
bool -> int -> X)
ETA: (fun x2_4823 -> (xs_4737 x2_5031 (fun x1_4830 x2_4831 -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831)))): (int
->
X)
ETA: (xs_4737 x2_5031 (fun x1_4830 x2_4831 -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831))): X
ETA: (fun x1_4830 x2_4831 -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831)): (
bool -> int -> X)
ETA: (fun x2_4831 -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831)): (int ->
X)
ETA: (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831): X
ETA: x2_4831: int
ETA: x1_4830: bool
ETA: x2_4823: int
ETA: x1_4822: bool
ETA_AUX: (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831): X
ETA: x2_5031: int
ETA_AUX: (xs_4737 x2_5031 (fun (x1_4830:bool) (x2_4831:int) -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831))): X
ETA: x1_5030: int
ETA_AUX: (xs_4737 x1_5030
           (fun (x1_4822:bool) (x2_4823:int) ->
            (xs_4737 x2_5031 (fun (x1_4830:bool) (x2_4831:int) -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831))))): X
ETA: (l0 (xs_4737 x1_5030 (fun r1_4815 r2_4816 -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816)))): X
ETA: (xs_4737 x1_5030 (fun r1_4815 r2_4816 -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816))): X
ETA: (fun r1_4815 r2_4816 -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816)): (
bool -> int -> X)
ETA: (fun r2_4816 -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816)): (int ->
X)
ETA: (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816): X
ETA: r2_4816: int
ETA: r1_4815: bool
ETA: r2_4816: int
ETA: r1_4815: bool
ETA_AUX: (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816): X
ETA: x1_5030: int
ETA_AUX: (xs_4737 x1_5030 (fun (r1_4815:bool) (r2_4816:int) -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816))): X
ETA_AUX: (insertsort_2165
           (fun (x1_5030:int) (x2_5031:int) (k_main_xsxs_5032:(bool -> int -> bool -> int -> X)) ->
            (if (x2_5031 = x1_5030)
              (l0
                (xs_4737 x1_5030
                  (fun (r1_4815:bool) (r2_4816:int) -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816))))
              (l1
                (xs_4737 x1_5030
                  (fun (x1_4822:bool) (x2_4823:int) ->
                   (xs_4737 x2_5031
                     (fun (x1_4830:bool) (x2_4831:int) -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831))))))))
           (fun (ysys_4740:(x_1:int ->
                            x_2:int ->
                            (x_4:bool ->
                             x_5:int ->
                             x_6:bool ->
                             x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7]
                             -> X)
                            -> X))
            ->
            (check_2164
              (fun (x__5048:int) (x__5049:int) 
                   (x__5050:(x_1:bool ->
                             x_2:int ->
                             x_3:bool ->
                             x_4:int[(not x_1) || (not x_3) || (not (x__5048 <= x__5049)) || 
                                     x__5048 < 0 || x__5049 < 0 || x_2 <= x_4]
                             -> X))
               ->
               (ysys_4740 x__5048 x__5049
                 (fun (x__5051:bool) (x__5052:int) (x__5053:bool) 
                      (x__5054:x_1:int[(not x__5051) || (not x__5053) || (
                                       not (x__5048 <= x__5049)) || x__5048 < 0 || 
                                       x__5049 < 0 || x__5052 <= x_1])
                  -> (x__5050 x__5051 x__5052 x__5053 x__5054))))
              (fun (b_4746:x_1:bool[x_1]) -> (if b_4746 (l0 end) (l1 (fail_3705 true (fun (main_4578:unit) -> end)))))))): X
ETA: arg1_4748: int
ETA_AUX: (make_list_1049 arg1_4748
           (fun (xs_4737:(int -> (bool -> int -> X) -> X)) ->
            (insertsort_2165
              (fun (x1_5030:int) (x2_5031:int) (k_main_xsxs_5032:(bool -> int -> bool -> int -> X)) ->
               (if (x2_5031 = x1_5030)
                 (l0
                   (xs_4737 x1_5030
                     (fun (r1_4815:bool) (r2_4816:int) -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816))))
                 (l1
                   (xs_4737 x1_5030
                     (fun (x1_4822:bool) (x2_4823:int) ->
                      (xs_4737 x2_5031
                        (fun (x1_4830:bool) (x2_4831:int) -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831))))))))
              (fun (ysys_4740:(x_1:int ->
                               x_2:int ->
                               (x_4:bool ->
                                x_5:int ->
                                x_6:bool ->
                                x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || 
                                        x_1 < 0 || x_2 < 0 || x_5 <= x_7]
                                -> X)
                               -> X))
               ->
               (check_2164
                 (fun (x__5048:int) (x__5049:int) 
                      (x__5050:(x_1:bool ->
                                x_2:int ->
                                x_3:bool ->
                                x_4:int[(not x_1) || (not x_3) || (not (x__5048 <= x__5049)) || 
                                        x__5048 < 0 || x__5049 < 0 || 
                                        x_2 <= x_4]
                                -> X))
                  ->
                  (ysys_4740 x__5048 x__5049
                    (fun (x__5051:bool) (x__5052:int) (x__5053:bool) 
                         (x__5054:x_1:int[(not x__5051) || (not x__5053) || (
                                          not (x__5048 <= x__5049)) || 
                                          x__5048 < 0 || x__5049 < 0 || 
                                          x__5052 <= x_1])
                     -> (x__5050 x__5051 x__5052 x__5053 x__5054))))
                 (fun (b_4746:x_1:bool[x_1]) ->
                  (if b_4746 (l0 end) (l1 (fail_3705 true (fun (main_4578:unit) -> end)))))))))): unit
ETA_AUX: (rand_int
           (fun (arg1_4748:int) ->
            (make_list_1049 arg1_4748
              (fun (xs_4737:(int -> (bool -> int -> X) -> X)) ->
               (insertsort_2165
                 (fun (x1_5030:int) (x2_5031:int) (k_main_xsxs_5032:(bool -> int -> bool -> int -> X)) ->
                  (if (x2_5031 = x1_5030)
                    (l0
                      (xs_4737 x1_5030
                        (fun (r1_4815:bool) (r2_4816:int) -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816))))
                    (l1
                      (xs_4737 x1_5030
                        (fun (x1_4822:bool) (x2_4823:int) ->
                         (xs_4737 x2_5031
                           (fun (x1_4830:bool) (x2_4831:int) -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831))))))))
                 (fun (ysys_4740:(x_1:int ->
                                  x_2:int ->
                                  (x_4:bool ->
                                   x_5:int ->
                                   x_6:bool ->
                                   x_7:int[(not x_4) || (not x_6) || (
                                           not (x_1 <= x_2)) || x_1 < 0 || 
                                           x_2 < 0 || x_5 <= x_7]
                                   -> X)
                                  -> X))
                  ->
                  (check_2164
                    (fun (x__5048:int) (x__5049:int) 
                         (x__5050:(x_1:bool ->
                                   x_2:int ->
                                   x_3:bool ->
                                   x_4:int[(not x_1) || (not x_3) || (
                                           not (x__5048 <= x__5049)) || 
                                           x__5048 < 0 || x__5049 < 0 || 
                                           x_2 <= x_4]
                                   -> X))
                     ->
                     (ysys_4740 x__5048 x__5049
                       (fun (x__5051:bool) (x__5052:int) (x__5053:bool) 
                            (x__5054:x_1:int[(not x__5051) || (not x__5053) || (
                                             not (x__5048 <= x__5049)) || 
                                             x__5048 < 0 || x__5049 < 0 || 
                                             x__5052 <= x_1])
                        -> (x__5050 x__5051 x__5052 x__5053 x__5054))))
                    (fun (b_4746:x_1:bool[x_1]) ->
                     (if b_4746 (l0 end) (l1 (fail_3705 true (fun (main_4578:unit) -> end)))))))))))): X
ETA: (xsxs_1056 0 0
       (fun xs11_4587 xs12_4588 xs21_4589 xs22_4590 ->
        (if xs11_4587
          (l1
            (if xs21_4589
              (l1
                (if (xs22_4590 >= xs12_4588)
                  (l0
                    (check_2164
                      (fun x1_4978 x2_4979 k_check_xs'xs'_4980 ->
                       (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1) k_check_xs'xs'_4980)) k_check_3093))
                  (l1 (k_check_3093 false)))) (l0 (k_check_3093 true)))) (
          l0 (k_check_3093 true))))): X
ETA: (fun xs11_4587 xs12_4588 xs21_4589 xs22_4590 ->
      (if xs11_4587
        (l1
          (if xs21_4589
            (l1
              (if (xs22_4590 >= xs12_4588)
                (l0
                  (check_2164
                    (fun x1_4978 x2_4979 k_check_xs'xs'_4980 ->
                     (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1) k_check_xs'xs'_4980)) k_check_3093))
                (l1 (k_check_3093 false)))) (l0 (k_check_3093 true)))) (
        l0 (k_check_3093 true)))): (x_1:bool ->
                                    x_2:int ->
                                    x_3:bool ->
                                    x_4:int[(not x_1) || (not x_3) || (not (0 <= 0)) || 0 < 0 || 0 < 0 || x_2 <= x_4]
                                    -> X)
ETA: (fun xs12_4588 xs21_4589 xs22_4590 ->
      (if xs11_4587
        (l1
          (if xs21_4589
            (l1
              (if (xs22_4590 >= xs12_4588)
                (l0
                  (check_2164
                    (fun x1_4978 x2_4979 k_check_xs'xs'_4980 ->
                     (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1) k_check_xs'xs'_4980)) k_check_3093))
                (l1 (k_check_3093 false)))) (l0 (k_check_3093 true)))) (
        l0 (k_check_3093 true)))): (x_1:int ->
                                    x_2:bool ->
                                    x_3:int[(not xs11_4587) || (not x_2) || (
                                            not (0 <= 0)) || 0 < 0 || 
                                            0 < 0 || x_1 <= x_3]
                                    -> X)
ETA: (fun xs21_4589 xs22_4590 ->
      (if xs11_4587
        (l1
          (if xs21_4589
            (l1
              (if (xs22_4590 >= xs12_4588)
                (l0
                  (check_2164
                    (fun x1_4978 x2_4979 k_check_xs'xs'_4980 ->
                     (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1) k_check_xs'xs'_4980)) k_check_3093))
                (l1 (k_check_3093 false)))) (l0 (k_check_3093 true)))) (
        l0 (k_check_3093 true)))): (x_1:bool ->
                                    x_2:int[(not xs11_4587) || (not x_1) || (
                                            not (0 <= 0)) || 0 < 0 || 
                                            0 < 0 || xs12_4588 <= x_2]
                                    -> X)
ETA: (fun xs22_4590 ->
      (if xs11_4587
        (l1
          (if xs21_4589
            (l1
              (if (xs22_4590 >= xs12_4588)
                (l0
                  (check_2164
                    (fun x1_4978 x2_4979 k_check_xs'xs'_4980 ->
                     (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1) k_check_xs'xs'_4980)) k_check_3093))
                (l1 (k_check_3093 false)))) (l0 (k_check_3093 true)))) (
        l0 (k_check_3093 true)))): (x_1:int[(not xs11_4587) || (not xs21_4589) || (
                                            not (0 <= 0)) || 0 < 0 || 
                                            0 < 0 || xs12_4588 <= x_1] ->
X)
ETA: (if xs11_4587
       (l1
         (if xs21_4589
           (l1
             (if (xs22_4590 >= xs12_4588)
               (l0
                 (check_2164
                   (fun x1_4978 x2_4979 k_check_xs'xs'_4980 ->
                    (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1) k_check_xs'xs'_4980)) k_check_3093))
               (l1 (k_check_3093 false)))) (l0 (k_check_3093 true)))) (
       l0 (k_check_3093 true))): X
ETA: (l0 (k_check_3093 true)): X
ETA: (k_check_3093 true): X
ETA: true: x_1:bool[x_1]
ETA_AUX: (k_check_3093 true): X
ETA: (l1
       (if xs21_4589
         (l1
           (if (xs22_4590 >= xs12_4588)
             (l0
               (check_2164
                 (fun x1_4978 x2_4979 k_check_xs'xs'_4980 ->
                  (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1) k_check_xs'xs'_4980)) k_check_3093)) (
             l1 (k_check_3093 false)))) (l0 (k_check_3093 true)))): X
ETA: (if xs21_4589
       (l1
         (if (xs22_4590 >= xs12_4588)
           (l0
             (check_2164
               (fun x1_4978 x2_4979 k_check_xs'xs'_4980 -> (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1) k_check_xs'xs'_4980))
               k_check_3093)) (l1 (k_check_3093 false)))) (l0 (k_check_3093 true))): X
ETA: (l0 (k_check_3093 true)): X
ETA: (k_check_3093 true): X
ETA: true: x_1:bool[x_1]
ETA_AUX: (k_check_3093 true): X
ETA: (l1
       (if (xs22_4590 >= xs12_4588)
         (l0
           (check_2164
             (fun x1_4978 x2_4979 k_check_xs'xs'_4980 -> (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1) k_check_xs'xs'_4980))
             k_check_3093)) (l1 (k_check_3093 false)))): X
ETA: (if (xs22_4590 >= xs12_4588)
       (l0
         (check_2164
           (fun x1_4978 x2_4979 k_check_xs'xs'_4980 -> (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1) k_check_xs'xs'_4980))
           k_check_3093)) (l1 (k_check_3093 false))): X
ETA: (l1 (k_check_3093 false)): X
ETA: (k_check_3093 false): X
ETA: false: x_1:bool[x_1]
ETA_AUX: (k_check_3093 false): X
ETA: (l0
       (check_2164
         (fun x1_4978 x2_4979 k_check_xs'xs'_4980 -> (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1) k_check_xs'xs'_4980))
         k_check_3093)): X
ETA: (check_2164
       (fun x1_4978 x2_4979 k_check_xs'xs'_4980 -> (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1) k_check_xs'xs'_4980))
       k_check_3093): X
ETA: k_check_3093: (x_1:bool[x_1] ->
X)
ETA_AUX: k_check_3093: (x_1:bool[x_1] ->
X)
ETA_AUX: x__5055: x_1:bool[x_1]
ETA_AUX: (k_check_3093 x__5055): X
ETA: (fun x1_4978 x2_4979 k_check_xs'xs'_4980 -> (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1) k_check_xs'xs'_4980)): (
x_1:int ->
x_2:int ->
(x_4:bool ->
 x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
-> X)
ETA: (fun x2_4979 k_check_xs'xs'_4980 -> (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1) k_check_xs'xs'_4980)): (
x_1:int ->
(x_3:bool ->
 x_4:int ->
 x_5:bool -> x_6:int[(not x_3) || (not x_5) || (not (x1_4978 <= x_1)) || x1_4978 < 0 || x_1 < 0 || x_4 <= x_6] -> X)
-> X)
ETA: (fun k_check_xs'xs'_4980 -> (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1) k_check_xs'xs'_4980)): ((
x_2:bool ->
x_3:int ->
x_4:bool -> x_5:int[(not x_2) || (not x_4) || (not (x1_4978 <= x2_4979)) || x1_4978 < 0 || x2_4979 < 0 || x_3 <= x_5]
-> X) ->
X)
ETA: (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1) k_check_xs'xs'_4980): X
ETA: k_check_xs'xs'_4980: (x_1:bool ->
                           x_2:int ->
                           x_3:bool ->
                           x_4:int[(not x_1) || (not x_3) || (not ((x1_4978 + 1) <= (x2_4979 + 1))) || 
                                   x1_4978 + 1 < 0 || x2_4979 + 1 < 0 || 
                                   x_2 <= x_4]
                           -> X)
ETA_AUX: k_check_xs'xs'_4980: (x_1:bool ->
                               x_2:int ->
                               x_3:bool ->
                               x_4:int[(not x_1) || (not x_3) || (not ((x1_4978 + 1) <= (x2_4979 + 1))) || 
                                       x1_4978 + 1 < 0 || x2_4979 + 1 < 0 || 
                                       x_2 <= x_4]
                               -> X)
ETA_AUX: x__5056: bool
ETA_AUX: (k_check_xs'xs'_4980 x__5056): (x_1:int ->
                                         x_2:bool ->
                                         x_3:int[(not x__5056) || (not x_2) || 
                                                 (not ((x1_4978 + 1) <= (x2_4979 + 1))) || 
                                                 x1_4978 + 1 < 0 || x2_4979 + 1 < 0 || 
                                                 x_1 <= x_3]
                                         -> X)
ETA_AUX: x__5057: int
ETA_AUX: (k_check_xs'xs'_4980 x__5056 x__5057): (x_1:bool ->
                                                 x_2:int[(not x__5056) || (
                                                         not x_1) || (
                                                         not ((x1_4978 + 1) <= (x2_4979 + 1))) || 
                                                         x1_4978 + 1 < 0 || 
                                                         x2_4979 + 1 < 0 || 
                                                         x__5057 <= x_2]
                                                 -> X)
ETA_AUX: x__5058: bool
ETA_AUX: (k_check_xs'xs'_4980 x__5056 x__5057 x__5058): (x_1:int[(not x__5056) || (
                                                                 not x__5058) || 
                                                                 (not ((x1_4978 + 1) <= (x2_4979 + 1))) || 
                                                                 x1_4978 + 1 < 0 || 
                                                                 x2_4979 + 1 < 0 || 
                                                                 x__5057 <= x_1] ->
X)
ETA_AUX: x__5059: x_1:int[(not x__5056) || (not x__5058) || (not (x1_4978 <= x2_4979)) || 
                          x1_4978 < 0 || x2_4979 < 0 || x__5057 <= x_1]
ETA_AUX: (k_check_xs'xs'_4980 x__5056 x__5057 x__5058 x__5059): X
ETA: (x2_4979 + 1): int
ETA: (x1_4978 + 1): int
ETA_AUX: (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1)
           (fun (x__5056:bool) (x__5057:int) (x__5058:bool) 
                (x__5059:x_1:int[(not x__5056) || (not x__5058) || (not ((x1_4978 + 1) <= (x2_4979 + 1))) || 
                                 x1_4978 + 1 < 0 || x2_4979 + 1 < 0 || 
                                 x__5057 <= x_1])
            -> (k_check_xs'xs'_4980 x__5056 x__5057 x__5058 x__5059))): X
ETA_AUX: (check_2164
           (fun (x1_4978:int) (x2_4979:int) 
                (k_check_xs'xs'_4980:(x_1:bool ->
                                      x_2:int ->
                                      x_3:bool ->
                                      x_4:int[(not x_1) || (not x_3) || (
                                              not (x1_4978 <= x2_4979)) || 
                                              x1_4978 < 0 || x2_4979 < 0 || 
                                              x_2 <= x_4]
                                      -> X))
            ->
            (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1)
              (fun (x__5056:bool) (x__5057:int) (x__5058:bool) 
                   (x__5059:x_1:int[(not x__5056) || (not x__5058) || (
                                    not ((x1_4978 + 1) <= (x2_4979 + 1))) || 
                                    x1_4978 + 1 < 0 || x2_4979 + 1 < 0 || 
                                    x__5057 <= x_1])
               -> (k_check_xs'xs'_4980 x__5056 x__5057 x__5058 x__5059))))
           (fun (x__5055:x_1:bool[x_1]) -> (k_check_3093 x__5055))): X
ETA: 0: int
ETA: 0: int
ETA_AUX: (xsxs_1056 0 0
           (fun (xs11_4587:bool) (xs12_4588:int) (xs21_4589:bool) 
                (xs22_4590:x_1:int[(not xs11_4587) || (not xs21_4589) || (
                                   not (0 <= 0)) || 0 < 0 || 0 < 0 || 
                                   xs12_4588 <= x_1])
            ->
            (if xs11_4587
              (l1
                (if xs21_4589
                  (l1
                    (if (xs22_4590 >= xs12_4588)
                      (l0
                        (check_2164
                          (fun (x1_4978:int) (x2_4979:int) 
                               (k_check_xs'xs'_4980:(x_1:bool ->
                                                     x_2:int ->
                                                     x_3:bool ->
                                                     x_4:int[(not x_1) || (
                                                             not x_3) || (
                                                             not (x1_4978 <= x2_4979)) || 
                                                             x1_4978 < 0 || 
                                                             x2_4979 < 0 || 
                                                             x_2 <= x_4]
                                                     -> X))
                           ->
                           (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1)
                             (fun (x__5056:bool) (x__5057:int) (x__5058:bool) 
                                  (x__5059:x_1:int[(not x__5056) || (
                                                   not x__5058) || (not ((x1_4978 + 1) <= (x2_4979 + 1))) || 
                                                   x1_4978 + 1 < 0 || 
                                                   x2_4979 + 1 < 0 || 
                                                   x__5057 <= x_1])
                              -> (k_check_xs'xs'_4980 x__5056 x__5057 x__5058 x__5059))))
                          (fun (x__5055:x_1:bool[x_1]) -> (k_check_3093 x__5055)))) (
                      l1 (k_check_3093 false)))) (l0 (k_check_3093 true)))) (
              l0 (k_check_3093 true))))): X
ETA: (k ()): X
ETA: (): unit
ETA_AUX: (k ()): X
ETA: (ysys_1016 0 0
       (fun p11_4797 p12_4798 p21_4799 p22_4800 ->
        (if p11_4797
          (l1
            (if (x_1053 < p12_4798)
              (l0
                (k_insert_2260
                  (fun x1_4612 x2_4613 k_insert_4614 ->
                   (if (x1_4612 <= 0)
                     (l0
                       (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
                         (l1
                           (ysys_1016 0 (x2_4613 - 1)
                             (fun xs11_4631 xs12_4632 xs21_4633 xs22_4634 ->
                              (if xs21_4633
                                (l1
                                  (if (1 = x2_4613)
                                    (l0
                                      (if (xs22_4634 = xs12_4632)
                                        (l1
                                          (if (p11_4797 <=> xs11_4631)
                                            (l1
                                              (if (xs12_4632 = p12_4798)
                                                (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                                (l0 (loop_2167 () k_insert_4614))))
                                            (l0 (l0 (loop_2167 () k_insert_4614))))) (
                                        l0 (loop_2167 () k_insert_4614))))
                                    (l1
                                      (l1
                                        (if (p11_4797 <=> xs11_4631)
                                          (l1
                                            (if (xs12_4632 = p12_4798) (
                                              l1 (k_insert_4614 true x_1053 true xs22_4634))
                                              (l0 (loop_2167 () k_insert_4614))))
                                          (l0 (l0 (loop_2167 () k_insert_4614)))))))) (
                                l0 (loop_2167 () k_insert_4614))))))))
                     (l1
                       (if (x2_4613 <= 0)
                         (l0
                           (ysys_1016 (x1_4612 - 1) 0
                             (fun p11_4641 p12_4642 p21_4643 p22_4644 -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
                         (l1 (ysys_1016 (x1_4612 - 1) (x2_4613 - 1) k_insert_4614))))))))
              (l1
                (insert_1014 x_1053
                  (fun x1_5042 x2_5043 k_insert_ys'ys'_5044 ->
                   (ysys_1016 (x1_5042 + 1) (x2_5043 + 1) k_insert_ys'ys'_5044))
                  (fun ys''ys''_4650 ->
                   (k_insert_2260
                     (fun x1_4664 x2_4665 k_insert_4666 ->
                      (if (x1_4664 <= 0)
                        (l0
                          (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                            (l1
                              (ysys_1016 0 (x2_4665 - 1)
                                (fun xs11_4683 xs12_4684 xs21_4685 xs22_4686 ->
                                 (if xs21_4685
                                   (l1
                                     (if (1 = x2_4665)
                                       (l0
                                         (if (xs22_4686 = xs12_4684)
                                           (l1
                                             (if (p11_4797 <=> xs11_4683)
                                               (l1
                                                 (if (xs12_4684 = p12_4798)
                                                   (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                   (l0 (loop_2167 () k_insert_4666))))
                                               (l0 (l0 (loop_2167 () k_insert_4666)))))
                                           (l0 (loop_2167 () k_insert_4666))))
                                       (l1
                                         (l1
                                           (if (p11_4797 <=> xs11_4683)
                                             (l1
                                               (if (xs12_4684 = p12_4798)
                                                 (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                 (l0 (loop_2167 () k_insert_4666))))
                                             (l0 (l0 (loop_2167 () k_insert_4666))))))))
                                   (l0 (loop_2167 () k_insert_4666))))))))
                        (l1
                          (if (x2_4665 <= 0)
                            (l0
                              (ys''ys''_4650 x1_4664 0
                                (fun p11_4693 p12_4694 p21_4695 p22_4696 ->
                                 (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                            (l1 (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1) k_insert_4666))))))))))))
          (l0
            (k_insert_2260
              (fun x1_4962 x2_4963 k_insert_rsrs_4964 ->
               (if (x2_4963 = x1_4962)
                 (l0
                   (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                     (l1 (k_insert_rsrs_4964 false 0 false 0))))
                 (l1
                   (if (x1_4962 <= 0)
                     (l0
                       (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                         (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
                     (l1
                       (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                         (l1 (k_insert_rsrs_4964 false 0 false 0))))))))))))): X
ETA: (fun p11_4797 p12_4798 p21_4799 p22_4800 ->
      (if p11_4797
        (l1
          (if (x_1053 < p12_4798)
            (l0
              (k_insert_2260
                (fun x1_4612 x2_4613 k_insert_4614 ->
                 (if (x1_4612 <= 0)
                   (l0
                     (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
                       (l1
                         (ysys_1016 0 (x2_4613 - 1)
                           (fun xs11_4631 xs12_4632 xs21_4633 xs22_4634 ->
                            (if xs21_4633
                              (l1
                                (if (1 = x2_4613)
                                  (l0
                                    (if (xs22_4634 = xs12_4632)
                                      (l1
                                        (if (p11_4797 <=> xs11_4631)
                                          (l1
                                            (if (xs12_4632 = p12_4798) (
                                              l1 (k_insert_4614 true x_1053 true xs22_4634))
                                              (l0 (loop_2167 () k_insert_4614))))
                                          (l0 (l0 (loop_2167 () k_insert_4614))))) (
                                      l0 (loop_2167 () k_insert_4614))))
                                  (l1
                                    (l1
                                      (if (p11_4797 <=> xs11_4631)
                                        (l1
                                          (if (xs12_4632 = p12_4798) (
                                            l1 (k_insert_4614 true x_1053 true xs22_4634))
                                            (l0 (loop_2167 () k_insert_4614)))) (
                                        l0 (l0 (loop_2167 () k_insert_4614)))))))) (
                              l0 (loop_2167 () k_insert_4614))))))))
                   (l1
                     (if (x2_4613 <= 0)
                       (l0
                         (ysys_1016 (x1_4612 - 1) 0
                           (fun p11_4641 p12_4642 p21_4643 p22_4644 -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
                       (l1 (ysys_1016 (x1_4612 - 1) (x2_4613 - 1) k_insert_4614))))))))
            (l1
              (insert_1014 x_1053
                (fun x1_5042 x2_5043 k_insert_ys'ys'_5044 ->
                 (ysys_1016 (x1_5042 + 1) (x2_5043 + 1) k_insert_ys'ys'_5044))
                (fun ys''ys''_4650 ->
                 (k_insert_2260
                   (fun x1_4664 x2_4665 k_insert_4666 ->
                    (if (x1_4664 <= 0)
                      (l0
                        (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                          (l1
                            (ysys_1016 0 (x2_4665 - 1)
                              (fun xs11_4683 xs12_4684 xs21_4685 xs22_4686 ->
                               (if xs21_4685
                                 (l1
                                   (if (1 = x2_4665)
                                     (l0
                                       (if (xs22_4686 = xs12_4684)
                                         (l1
                                           (if (p11_4797 <=> xs11_4683)
                                             (l1
                                               (if (xs12_4684 = p12_4798)
                                                 (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                 (l0 (loop_2167 () k_insert_4666))))
                                             (l0 (l0 (loop_2167 () k_insert_4666))))) (
                                         l0 (loop_2167 () k_insert_4666))))
                                     (l1
                                       (l1
                                         (if (p11_4797 <=> xs11_4683)
                                           (l1
                                             (if (xs12_4684 = p12_4798)
                                               (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                               (l0 (loop_2167 () k_insert_4666))))
                                           (l0 (l0 (loop_2167 () k_insert_4666))))))))
                                 (l0 (loop_2167 () k_insert_4666))))))))
                      (l1
                        (if (x2_4665 <= 0)
                          (l0
                            (ys''ys''_4650 x1_4664 0
                              (fun p11_4693 p12_4694 p21_4695 p22_4696 ->
                               (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                          (l1 (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1) k_insert_4666))))))))))))
        (l0
          (k_insert_2260
            (fun x1_4962 x2_4963 k_insert_rsrs_4964 ->
             (if (x2_4963 = x1_4962)
               (l0
                 (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                   (l1 (k_insert_rsrs_4964 false 0 false 0))))
               (l1
                 (if (x1_4962 <= 0)
                   (l0
                     (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                       (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
                   (l1
                     (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                       (l1 (k_insert_rsrs_4964 false 0 false 0)))))))))))): (
x_1:bool ->
x_2:int -> x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (0 <= 0)) || 0 < 0 || 0 < 0 || x_2 <= x_4] -> X)
ETA: (fun p12_4798 p21_4799 p22_4800 ->
      (if p11_4797
        (l1
          (if (x_1053 < p12_4798)
            (l0
              (k_insert_2260
                (fun x1_4612 x2_4613 k_insert_4614 ->
                 (if (x1_4612 <= 0)
                   (l0
                     (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
                       (l1
                         (ysys_1016 0 (x2_4613 - 1)
                           (fun xs11_4631 xs12_4632 xs21_4633 xs22_4634 ->
                            (if xs21_4633
                              (l1
                                (if (1 = x2_4613)
                                  (l0
                                    (if (xs22_4634 = xs12_4632)
                                      (l1
                                        (if (p11_4797 <=> xs11_4631)
                                          (l1
                                            (if (xs12_4632 = p12_4798) (
                                              l1 (k_insert_4614 true x_1053 true xs22_4634))
                                              (l0 (loop_2167 () k_insert_4614))))
                                          (l0 (l0 (loop_2167 () k_insert_4614))))) (
                                      l0 (loop_2167 () k_insert_4614))))
                                  (l1
                                    (l1
                                      (if (p11_4797 <=> xs11_4631)
                                        (l1
                                          (if (xs12_4632 = p12_4798) (
                                            l1 (k_insert_4614 true x_1053 true xs22_4634))
                                            (l0 (loop_2167 () k_insert_4614)))) (
                                        l0 (l0 (loop_2167 () k_insert_4614)))))))) (
                              l0 (loop_2167 () k_insert_4614))))))))
                   (l1
                     (if (x2_4613 <= 0)
                       (l0
                         (ysys_1016 (x1_4612 - 1) 0
                           (fun p11_4641 p12_4642 p21_4643 p22_4644 -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
                       (l1 (ysys_1016 (x1_4612 - 1) (x2_4613 - 1) k_insert_4614))))))))
            (l1
              (insert_1014 x_1053
                (fun x1_5042 x2_5043 k_insert_ys'ys'_5044 ->
                 (ysys_1016 (x1_5042 + 1) (x2_5043 + 1) k_insert_ys'ys'_5044))
                (fun ys''ys''_4650 ->
                 (k_insert_2260
                   (fun x1_4664 x2_4665 k_insert_4666 ->
                    (if (x1_4664 <= 0)
                      (l0
                        (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                          (l1
                            (ysys_1016 0 (x2_4665 - 1)
                              (fun xs11_4683 xs12_4684 xs21_4685 xs22_4686 ->
                               (if xs21_4685
                                 (l1
                                   (if (1 = x2_4665)
                                     (l0
                                       (if (xs22_4686 = xs12_4684)
                                         (l1
                                           (if (p11_4797 <=> xs11_4683)
                                             (l1
                                               (if (xs12_4684 = p12_4798)
                                                 (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                 (l0 (loop_2167 () k_insert_4666))))
                                             (l0 (l0 (loop_2167 () k_insert_4666))))) (
                                         l0 (loop_2167 () k_insert_4666))))
                                     (l1
                                       (l1
                                         (if (p11_4797 <=> xs11_4683)
                                           (l1
                                             (if (xs12_4684 = p12_4798)
                                               (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                               (l0 (loop_2167 () k_insert_4666))))
                                           (l0 (l0 (loop_2167 () k_insert_4666))))))))
                                 (l0 (loop_2167 () k_insert_4666))))))))
                      (l1
                        (if (x2_4665 <= 0)
                          (l0
                            (ys''ys''_4650 x1_4664 0
                              (fun p11_4693 p12_4694 p21_4695 p22_4696 ->
                               (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                          (l1 (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1) k_insert_4666))))))))))))
        (l0
          (k_insert_2260
            (fun x1_4962 x2_4963 k_insert_rsrs_4964 ->
             (if (x2_4963 = x1_4962)
               (l0
                 (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                   (l1 (k_insert_rsrs_4964 false 0 false 0))))
               (l1
                 (if (x1_4962 <= 0)
                   (l0
                     (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                       (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
                   (l1
                     (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                       (l1 (k_insert_rsrs_4964 false 0 false 0)))))))))))): (
x_1:int -> x_2:bool -> x_3:int[(not p11_4797) || (not x_2) || (not (0 <= 0)) || 0 < 0 || 0 < 0 || x_1 <= x_3] -> X)
ETA: (fun p21_4799 p22_4800 ->
      (if p11_4797
        (l1
          (if (x_1053 < p12_4798)
            (l0
              (k_insert_2260
                (fun x1_4612 x2_4613 k_insert_4614 ->
                 (if (x1_4612 <= 0)
                   (l0
                     (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
                       (l1
                         (ysys_1016 0 (x2_4613 - 1)
                           (fun xs11_4631 xs12_4632 xs21_4633 xs22_4634 ->
                            (if xs21_4633
                              (l1
                                (if (1 = x2_4613)
                                  (l0
                                    (if (xs22_4634 = xs12_4632)
                                      (l1
                                        (if (p11_4797 <=> xs11_4631)
                                          (l1
                                            (if (xs12_4632 = p12_4798) (
                                              l1 (k_insert_4614 true x_1053 true xs22_4634))
                                              (l0 (loop_2167 () k_insert_4614))))
                                          (l0 (l0 (loop_2167 () k_insert_4614))))) (
                                      l0 (loop_2167 () k_insert_4614))))
                                  (l1
                                    (l1
                                      (if (p11_4797 <=> xs11_4631)
                                        (l1
                                          (if (xs12_4632 = p12_4798) (
                                            l1 (k_insert_4614 true x_1053 true xs22_4634))
                                            (l0 (loop_2167 () k_insert_4614)))) (
                                        l0 (l0 (loop_2167 () k_insert_4614)))))))) (
                              l0 (loop_2167 () k_insert_4614))))))))
                   (l1
                     (if (x2_4613 <= 0)
                       (l0
                         (ysys_1016 (x1_4612 - 1) 0
                           (fun p11_4641 p12_4642 p21_4643 p22_4644 -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
                       (l1 (ysys_1016 (x1_4612 - 1) (x2_4613 - 1) k_insert_4614))))))))
            (l1
              (insert_1014 x_1053
                (fun x1_5042 x2_5043 k_insert_ys'ys'_5044 ->
                 (ysys_1016 (x1_5042 + 1) (x2_5043 + 1) k_insert_ys'ys'_5044))
                (fun ys''ys''_4650 ->
                 (k_insert_2260
                   (fun x1_4664 x2_4665 k_insert_4666 ->
                    (if (x1_4664 <= 0)
                      (l0
                        (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                          (l1
                            (ysys_1016 0 (x2_4665 - 1)
                              (fun xs11_4683 xs12_4684 xs21_4685 xs22_4686 ->
                               (if xs21_4685
                                 (l1
                                   (if (1 = x2_4665)
                                     (l0
                                       (if (xs22_4686 = xs12_4684)
                                         (l1
                                           (if (p11_4797 <=> xs11_4683)
                                             (l1
                                               (if (xs12_4684 = p12_4798)
                                                 (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                 (l0 (loop_2167 () k_insert_4666))))
                                             (l0 (l0 (loop_2167 () k_insert_4666))))) (
                                         l0 (loop_2167 () k_insert_4666))))
                                     (l1
                                       (l1
                                         (if (p11_4797 <=> xs11_4683)
                                           (l1
                                             (if (xs12_4684 = p12_4798)
                                               (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                               (l0 (loop_2167 () k_insert_4666))))
                                           (l0 (l0 (loop_2167 () k_insert_4666))))))))
                                 (l0 (loop_2167 () k_insert_4666))))))))
                      (l1
                        (if (x2_4665 <= 0)
                          (l0
                            (ys''ys''_4650 x1_4664 0
                              (fun p11_4693 p12_4694 p21_4695 p22_4696 ->
                               (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                          (l1 (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1) k_insert_4666))))))))))))
        (l0
          (k_insert_2260
            (fun x1_4962 x2_4963 k_insert_rsrs_4964 ->
             (if (x2_4963 = x1_4962)
               (l0
                 (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                   (l1 (k_insert_rsrs_4964 false 0 false 0))))
               (l1
                 (if (x1_4962 <= 0)
                   (l0
                     (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                       (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
                   (l1
                     (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                       (l1 (k_insert_rsrs_4964 false 0 false 0)))))))))))): (
x_1:bool -> x_2:int[(not p11_4797) || (not x_1) || (not (0 <= 0)) || 0 < 0 || 0 < 0 || p12_4798 <= x_2] -> X)
ETA: (fun p22_4800 ->
      (if p11_4797
        (l1
          (if (x_1053 < p12_4798)
            (l0
              (k_insert_2260
                (fun x1_4612 x2_4613 k_insert_4614 ->
                 (if (x1_4612 <= 0)
                   (l0
                     (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
                       (l1
                         (ysys_1016 0 (x2_4613 - 1)
                           (fun xs11_4631 xs12_4632 xs21_4633 xs22_4634 ->
                            (if xs21_4633
                              (l1
                                (if (1 = x2_4613)
                                  (l0
                                    (if (xs22_4634 = xs12_4632)
                                      (l1
                                        (if (p11_4797 <=> xs11_4631)
                                          (l1
                                            (if (xs12_4632 = p12_4798) (
                                              l1 (k_insert_4614 true x_1053 true xs22_4634))
                                              (l0 (loop_2167 () k_insert_4614))))
                                          (l0 (l0 (loop_2167 () k_insert_4614))))) (
                                      l0 (loop_2167 () k_insert_4614))))
                                  (l1
                                    (l1
                                      (if (p11_4797 <=> xs11_4631)
                                        (l1
                                          (if (xs12_4632 = p12_4798) (
                                            l1 (k_insert_4614 true x_1053 true xs22_4634))
                                            (l0 (loop_2167 () k_insert_4614)))) (
                                        l0 (l0 (loop_2167 () k_insert_4614)))))))) (
                              l0 (loop_2167 () k_insert_4614))))))))
                   (l1
                     (if (x2_4613 <= 0)
                       (l0
                         (ysys_1016 (x1_4612 - 1) 0
                           (fun p11_4641 p12_4642 p21_4643 p22_4644 -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
                       (l1 (ysys_1016 (x1_4612 - 1) (x2_4613 - 1) k_insert_4614))))))))
            (l1
              (insert_1014 x_1053
                (fun x1_5042 x2_5043 k_insert_ys'ys'_5044 ->
                 (ysys_1016 (x1_5042 + 1) (x2_5043 + 1) k_insert_ys'ys'_5044))
                (fun ys''ys''_4650 ->
                 (k_insert_2260
                   (fun x1_4664 x2_4665 k_insert_4666 ->
                    (if (x1_4664 <= 0)
                      (l0
                        (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                          (l1
                            (ysys_1016 0 (x2_4665 - 1)
                              (fun xs11_4683 xs12_4684 xs21_4685 xs22_4686 ->
                               (if xs21_4685
                                 (l1
                                   (if (1 = x2_4665)
                                     (l0
                                       (if (xs22_4686 = xs12_4684)
                                         (l1
                                           (if (p11_4797 <=> xs11_4683)
                                             (l1
                                               (if (xs12_4684 = p12_4798)
                                                 (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                 (l0 (loop_2167 () k_insert_4666))))
                                             (l0 (l0 (loop_2167 () k_insert_4666))))) (
                                         l0 (loop_2167 () k_insert_4666))))
                                     (l1
                                       (l1
                                         (if (p11_4797 <=> xs11_4683)
                                           (l1
                                             (if (xs12_4684 = p12_4798)
                                               (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                               (l0 (loop_2167 () k_insert_4666))))
                                           (l0 (l0 (loop_2167 () k_insert_4666))))))))
                                 (l0 (loop_2167 () k_insert_4666))))))))
                      (l1
                        (if (x2_4665 <= 0)
                          (l0
                            (ys''ys''_4650 x1_4664 0
                              (fun p11_4693 p12_4694 p21_4695 p22_4696 ->
                               (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                          (l1 (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1) k_insert_4666))))))))))))
        (l0
          (k_insert_2260
            (fun x1_4962 x2_4963 k_insert_rsrs_4964 ->
             (if (x2_4963 = x1_4962)
               (l0
                 (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                   (l1 (k_insert_rsrs_4964 false 0 false 0))))
               (l1
                 (if (x1_4962 <= 0)
                   (l0
                     (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                       (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
                   (l1
                     (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                       (l1 (k_insert_rsrs_4964 false 0 false 0)))))))))))): (x_1:int[
(not p11_4797) || (not p21_4799) || (not (0 <= 0)) || 0 < 0 || 0 < 0 || p12_4798 <= x_1] ->
X)
ETA: (if p11_4797
       (l1
         (if (x_1053 < p12_4798)
           (l0
             (k_insert_2260
               (fun x1_4612 x2_4613 k_insert_4614 ->
                (if (x1_4612 <= 0)
                  (l0
                    (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
                      (l1
                        (ysys_1016 0 (x2_4613 - 1)
                          (fun xs11_4631 xs12_4632 xs21_4633 xs22_4634 ->
                           (if xs21_4633
                             (l1
                               (if (1 = x2_4613)
                                 (l0
                                   (if (xs22_4634 = xs12_4632)
                                     (l1
                                       (if (p11_4797 <=> xs11_4631)
                                         (l1
                                           (if (xs12_4632 = p12_4798) (
                                             l1 (k_insert_4614 true x_1053 true xs22_4634))
                                             (l0 (loop_2167 () k_insert_4614)))) (
                                         l0 (l0 (loop_2167 () k_insert_4614))))) (
                                     l0 (loop_2167 () k_insert_4614))))
                                 (l1
                                   (l1
                                     (if (p11_4797 <=> xs11_4631)
                                       (l1
                                         (if (xs12_4632 = p12_4798) (
                                           l1 (k_insert_4614 true x_1053 true xs22_4634))
                                           (l0 (loop_2167 () k_insert_4614)))) (
                                       l0 (l0 (loop_2167 () k_insert_4614)))))))) (
                             l0 (loop_2167 () k_insert_4614))))))))
                  (l1
                    (if (x2_4613 <= 0)
                      (l0
                        (ysys_1016 (x1_4612 - 1) 0
                          (fun p11_4641 p12_4642 p21_4643 p22_4644 -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
                      (l1 (ysys_1016 (x1_4612 - 1) (x2_4613 - 1) k_insert_4614))))))))
           (l1
             (insert_1014 x_1053
               (fun x1_5042 x2_5043 k_insert_ys'ys'_5044 ->
                (ysys_1016 (x1_5042 + 1) (x2_5043 + 1) k_insert_ys'ys'_5044))
               (fun ys''ys''_4650 ->
                (k_insert_2260
                  (fun x1_4664 x2_4665 k_insert_4666 ->
                   (if (x1_4664 <= 0)
                     (l0
                       (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                         (l1
                           (ysys_1016 0 (x2_4665 - 1)
                             (fun xs11_4683 xs12_4684 xs21_4685 xs22_4686 ->
                              (if xs21_4685
                                (l1
                                  (if (1 = x2_4665)
                                    (l0
                                      (if (xs22_4686 = xs12_4684)
                                        (l1
                                          (if (p11_4797 <=> xs11_4683)
                                            (l1
                                              (if (xs12_4684 = p12_4798)
                                                (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                (l0 (loop_2167 () k_insert_4666))))
                                            (l0 (l0 (loop_2167 () k_insert_4666))))) (
                                        l0 (loop_2167 () k_insert_4666))))
                                    (l1
                                      (l1
                                        (if (p11_4797 <=> xs11_4683)
                                          (l1
                                            (if (xs12_4684 = p12_4798)
                                              (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                              (l0 (loop_2167 () k_insert_4666))))
                                          (l0 (l0 (loop_2167 () k_insert_4666)))))))) (
                                l0 (loop_2167 () k_insert_4666))))))))
                     (l1
                       (if (x2_4665 <= 0)
                         (l0
                           (ys''ys''_4650 x1_4664 0
                             (fun p11_4693 p12_4694 p21_4695 p22_4696 ->
                              (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                         (l1 (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1) k_insert_4666))))))))))))
       (l0
         (k_insert_2260
           (fun x1_4962 x2_4963 k_insert_rsrs_4964 ->
            (if (x2_4963 = x1_4962)
              (l0
                (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                  (l1 (k_insert_rsrs_4964 false 0 false 0))))
              (l1
                (if (x1_4962 <= 0)
                  (l0
                    (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                      (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
                  (l1
                    (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                      (l1 (k_insert_rsrs_4964 false 0 false 0))))))))))): X
ETA: (l0
       (k_insert_2260
         (fun x1_4962 x2_4963 k_insert_rsrs_4964 ->
          (if (x2_4963 = x1_4962)
            (l0
              (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                (l1 (k_insert_rsrs_4964 false 0 false 0))))
            (l1
              (if (x1_4962 <= 0)
                (l0
                  (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                    (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
                (l1
                  (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                    (l1 (k_insert_rsrs_4964 false 0 false 0)))))))))): X
ETA: (k_insert_2260
       (fun x1_4962 x2_4963 k_insert_rsrs_4964 ->
        (if (x2_4963 = x1_4962)
          (l0
            (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
              (l1 (k_insert_rsrs_4964 false 0 false 0))))
          (l1
            (if (x1_4962 <= 0)
              (l0
                (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                  (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
              (l1
                (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                  (l1 (k_insert_rsrs_4964 false 0 false 0))))))))): X
ETA: (fun x1_4962 x2_4963 k_insert_rsrs_4964 ->
      (if (x2_4963 = x1_4962)
        (l0
          (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
            (l1 (k_insert_rsrs_4964 false 0 false 0))))
        (l1
          (if (x1_4962 <= 0)
            (l0
              (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
            (l1
              (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                (l1 (k_insert_rsrs_4964 false 0 false 0)))))))): (x_1:int ->
                                                                  x_2:int ->
                                                                  (x_4:bool ->
                                                                   x_5:int ->
                                                                   x_6:bool ->
                                                                   x_7:int[
                                                                   (not x_4) || (
                                                                   not x_6) || (
                                                                   not (x_1 <= x_2)) || 
                                                                   x_1 < 0 || 
                                                                   x_2 < 0 || 
                                                                   x_5 <= x_7] -> X)
                                                                  -> X)
ETA: (fun x2_4963 k_insert_rsrs_4964 ->
      (if (x2_4963 = x1_4962)
        (l0
          (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
            (l1 (k_insert_rsrs_4964 false 0 false 0))))
        (l1
          (if (x1_4962 <= 0)
            (l0
              (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
            (l1
              (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                (l1 (k_insert_rsrs_4964 false 0 false 0)))))))): (x_1:int ->
                                                                  (x_3:bool ->
                                                                   x_4:int ->
                                                                   x_5:bool ->
                                                                   x_6:int[
                                                                   (not x_3) || (
                                                                   not x_5) || (
                                                                   not (x1_4962 <= x_1)) || 
                                                                   x1_4962 < 0 || 
                                                                   x_1 < 0 || 
                                                                   x_4 <= x_6] -> X)
                                                                  -> X)
ETA: (fun k_insert_rsrs_4964 ->
      (if (x2_4963 = x1_4962)
        (l0
          (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
            (l1 (k_insert_rsrs_4964 false 0 false 0))))
        (l1
          (if (x1_4962 <= 0)
            (l0
              (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
            (l1
              (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                (l1 (k_insert_rsrs_4964 false 0 false 0)))))))): ((x_2:bool ->
                                                                   x_3:int ->
                                                                   x_4:bool ->
                                                                   x_5:int[
                                                                   (not x_2) || (
                                                                   not x_4) || (
                                                                   not (x1_4962 <= x2_4963)) || 
                                                                   x1_4962 < 0 || 
                                                                   x2_4963 < 0 || 
                                                                   x_3 <= x_5] -> X) ->
X)
ETA: (if (x2_4963 = x1_4962)
       (l0
         (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053)) (l1 (k_insert_rsrs_4964 false 0 false 0))))
       (l1
         (if (x1_4962 <= 0)
           (l0
             (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
               (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
           (l1
             (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053)) (l1 (k_insert_rsrs_4964 false 0 false 0))))))): X
ETA: (l1
       (if (x1_4962 <= 0)
         (l0
           (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
             (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
         (l1
           (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053)) (l1 (k_insert_rsrs_4964 false 0 false 0)))))): X
ETA: (if (x1_4962 <= 0)
       (l0
         (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
           (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
       (l1 (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053)) (l1 (k_insert_rsrs_4964 false 0 false 0))))): X
ETA: (l1 (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053)) (l1 (k_insert_rsrs_4964 false 0 false 0)))): X
ETA: (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053)) (l1 (k_insert_rsrs_4964 false 0 false 0))): X
ETA: (l1 (k_insert_rsrs_4964 false 0 false 0)): X
ETA: (k_insert_rsrs_4964 false 0 false 0): X
ETA: 0: x_1:int[(not false) || (not false) || (not (x1_4962 <= x2_4963)) || x1_4962 < 0 || x2_4963 < 0 || 0 <= x_1]
ETA: false: bool
ETA: 0: int
ETA: false: bool
ETA_AUX: (k_insert_rsrs_4964 false 0 false 0): X
ETA: (l0 (k_insert_rsrs_4964 false 0 true x_1053)): X
ETA: (k_insert_rsrs_4964 false 0 true x_1053): X
ETA: x_1053: x_1:int[(not false) || (not true) || (not (x1_4962 <= x2_4963)) || x1_4962 < 0 || x2_4963 < 0 || 0 <= x_1]
ETA: true: bool
ETA: 0: int
ETA: false: bool
ETA_AUX: (k_insert_rsrs_4964 false 0 true x_1053): X
ETA: (l0
       (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
         (l1 (k_insert_rsrs_4964 true x_1053 false 0)))): X
ETA: (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053)) (l1 (k_insert_rsrs_4964 true x_1053 false 0))): X
ETA: (l1 (k_insert_rsrs_4964 true x_1053 false 0)): X
ETA: (k_insert_rsrs_4964 true x_1053 false 0): X
ETA: 0: x_1:int[(not true) || (not false) || (not (x1_4962 <= x2_4963)) || x1_4962 < 0 || x2_4963 < 0 || x_1053 <= x_1]
ETA: false: bool
ETA: x_1053: int
ETA: true: bool
ETA_AUX: (k_insert_rsrs_4964 true x_1053 false 0): X
ETA: (l0 (k_insert_rsrs_4964 true x_1053 true x_1053)): X
ETA: (k_insert_rsrs_4964 true x_1053 true x_1053): X
ETA: x_1053: x_1:int[(not true) || (not true) || (not (x1_4962 <= x2_4963)) || 
                     x1_4962 < 0 || x2_4963 < 0 || x_1053 <= x_1]
ETA: true: bool
ETA: x_1053: int
ETA: true: bool
ETA_AUX: (k_insert_rsrs_4964 true x_1053 true x_1053): X
ETA: (l0
       (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053)) (l1 (k_insert_rsrs_4964 false 0 false 0)))): X
ETA: (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053)) (l1 (k_insert_rsrs_4964 false 0 false 0))): X
ETA: (l1 (k_insert_rsrs_4964 false 0 false 0)): X
ETA: (k_insert_rsrs_4964 false 0 false 0): X
ETA: 0: x_1:int[(not false) || (not false) || (not (x1_4962 <= x2_4963)) || x1_4962 < 0 || x2_4963 < 0 || 0 <= x_1]
ETA: false: bool
ETA: 0: int
ETA: false: bool
ETA_AUX: (k_insert_rsrs_4964 false 0 false 0): X
ETA: (l0 (k_insert_rsrs_4964 true x_1053 true x_1053)): X
ETA: (k_insert_rsrs_4964 true x_1053 true x_1053): X
ETA: x_1053: x_1:int[(not true) || (not true) || (not (x1_4962 <= x2_4963)) || 
                     x1_4962 < 0 || x2_4963 < 0 || x_1053 <= x_1]
ETA: true: bool
ETA: x_1053: int
ETA: true: bool
ETA_AUX: (k_insert_rsrs_4964 true x_1053 true x_1053): X
ETA_AUX: (k_insert_2260
           (fun (x1_4962:int) (x2_4963:int) 
                (k_insert_rsrs_4964:(x_1:bool ->
                                     x_2:int ->
                                     x_3:bool ->
                                     x_4:int[(not x_1) || (not x_3) || (
                                             not (x1_4962 <= x2_4963)) || 
                                             x1_4962 < 0 || x2_4963 < 0 || 
                                             x_2 <= x_4]
                                     -> X))
            ->
            (if (x2_4963 = x1_4962)
              (l0
                (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                  (l1 (k_insert_rsrs_4964 false 0 false 0))))
              (l1
                (if (x1_4962 <= 0)
                  (l0
                    (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                      (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
                  (l1
                    (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                      (l1 (k_insert_rsrs_4964 false 0 false 0))))))))): X
ETA: (l1
       (if (x_1053 < p12_4798)
         (l0
           (k_insert_2260
             (fun x1_4612 x2_4613 k_insert_4614 ->
              (if (x1_4612 <= 0)
                (l0
                  (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
                    (l1
                      (ysys_1016 0 (x2_4613 - 1)
                        (fun xs11_4631 xs12_4632 xs21_4633 xs22_4634 ->
                         (if xs21_4633
                           (l1
                             (if (1 = x2_4613)
                               (l0
                                 (if (xs22_4634 = xs12_4632)
                                   (l1
                                     (if (p11_4797 <=> xs11_4631)
                                       (l1
                                         (if (xs12_4632 = p12_4798) (
                                           l1 (k_insert_4614 true x_1053 true xs22_4634))
                                           (l0 (loop_2167 () k_insert_4614)))) (
                                       l0 (l0 (loop_2167 () k_insert_4614))))) (
                                   l0 (loop_2167 () k_insert_4614))))
                               (l1
                                 (l1
                                   (if (p11_4797 <=> xs11_4631)
                                     (l1
                                       (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                         (l0 (loop_2167 () k_insert_4614)))) (
                                     l0 (l0 (loop_2167 () k_insert_4614)))))))) (
                           l0 (loop_2167 () k_insert_4614))))))))
                (l1
                  (if (x2_4613 <= 0)
                    (l0
                      (ysys_1016 (x1_4612 - 1) 0
                        (fun p11_4641 p12_4642 p21_4643 p22_4644 -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
                    (l1 (ysys_1016 (x1_4612 - 1) (x2_4613 - 1) k_insert_4614))))))))
         (l1
           (insert_1014 x_1053
             (fun x1_5042 x2_5043 k_insert_ys'ys'_5044 -> (ysys_1016 (x1_5042 + 1) (x2_5043 + 1) k_insert_ys'ys'_5044))
             (fun ys''ys''_4650 ->
              (k_insert_2260
                (fun x1_4664 x2_4665 k_insert_4666 ->
                 (if (x1_4664 <= 0)
                   (l0
                     (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                       (l1
                         (ysys_1016 0 (x2_4665 - 1)
                           (fun xs11_4683 xs12_4684 xs21_4685 xs22_4686 ->
                            (if xs21_4685
                              (l1
                                (if (1 = x2_4665)
                                  (l0
                                    (if (xs22_4686 = xs12_4684)
                                      (l1
                                        (if (p11_4797 <=> xs11_4683)
                                          (l1
                                            (if (xs12_4684 = p12_4798)
                                              (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                              (l0 (loop_2167 () k_insert_4666))))
                                          (l0 (l0 (loop_2167 () k_insert_4666))))) (
                                      l0 (loop_2167 () k_insert_4666))))
                                  (l1
                                    (l1
                                      (if (p11_4797 <=> xs11_4683)
                                        (l1
                                          (if (xs12_4684 = p12_4798) (
                                            l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                            (l0 (loop_2167 () k_insert_4666)))) (
                                        l0 (l0 (loop_2167 () k_insert_4666)))))))) (
                              l0 (loop_2167 () k_insert_4666))))))))
                   (l1
                     (if (x2_4665 <= 0)
                       (l0
                         (ys''ys''_4650 x1_4664 0
                           (fun p11_4693 p12_4694 p21_4695 p22_4696 -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                       (l1 (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1) k_insert_4666)))))))))))): X
ETA: (if (x_1053 < p12_4798)
       (l0
         (k_insert_2260
           (fun x1_4612 x2_4613 k_insert_4614 ->
            (if (x1_4612 <= 0)
              (l0
                (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
                  (l1
                    (ysys_1016 0 (x2_4613 - 1)
                      (fun xs11_4631 xs12_4632 xs21_4633 xs22_4634 ->
                       (if xs21_4633
                         (l1
                           (if (1 = x2_4613)
                             (l0
                               (if (xs22_4634 = xs12_4632)
                                 (l1
                                   (if (p11_4797 <=> xs11_4631)
                                     (l1
                                       (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                         (l0 (loop_2167 () k_insert_4614)))) (
                                     l0 (l0 (loop_2167 () k_insert_4614))))) (
                                 l0 (loop_2167 () k_insert_4614))))
                             (l1
                               (l1
                                 (if (p11_4797 <=> xs11_4631)
                                   (l1
                                     (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                       (l0 (loop_2167 () k_insert_4614)))) (
                                   l0 (l0 (loop_2167 () k_insert_4614)))))))) (
                         l0 (loop_2167 () k_insert_4614))))))))
              (l1
                (if (x2_4613 <= 0)
                  (l0
                    (ysys_1016 (x1_4612 - 1) 0
                      (fun p11_4641 p12_4642 p21_4643 p22_4644 -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
                  (l1 (ysys_1016 (x1_4612 - 1) (x2_4613 - 1) k_insert_4614))))))))
       (l1
         (insert_1014 x_1053
           (fun x1_5042 x2_5043 k_insert_ys'ys'_5044 -> (ysys_1016 (x1_5042 + 1) (x2_5043 + 1) k_insert_ys'ys'_5044))
           (fun ys''ys''_4650 ->
            (k_insert_2260
              (fun x1_4664 x2_4665 k_insert_4666 ->
               (if (x1_4664 <= 0)
                 (l0
                   (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                     (l1
                       (ysys_1016 0 (x2_4665 - 1)
                         (fun xs11_4683 xs12_4684 xs21_4685 xs22_4686 ->
                          (if xs21_4685
                            (l1
                              (if (1 = x2_4665)
                                (l0
                                  (if (xs22_4686 = xs12_4684)
                                    (l1
                                      (if (p11_4797 <=> xs11_4683)
                                        (l1
                                          (if (xs12_4684 = p12_4798) (
                                            l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                            (l0 (loop_2167 () k_insert_4666)))) (
                                        l0 (l0 (loop_2167 () k_insert_4666))))) (
                                    l0 (loop_2167 () k_insert_4666))))
                                (l1
                                  (l1
                                    (if (p11_4797 <=> xs11_4683)
                                      (l1
                                        (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                          (l0 (loop_2167 () k_insert_4666)))) (
                                      l0 (l0 (loop_2167 () k_insert_4666)))))))) (
                            l0 (loop_2167 () k_insert_4666))))))))
                 (l1
                   (if (x2_4665 <= 0)
                     (l0
                       (ys''ys''_4650 x1_4664 0
                         (fun p11_4693 p12_4694 p21_4695 p22_4696 -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                     (l1 (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1) k_insert_4666))))))))))): X
ETA: (l1
       (insert_1014 x_1053
         (fun x1_5042 x2_5043 k_insert_ys'ys'_5044 -> (ysys_1016 (x1_5042 + 1) (x2_5043 + 1) k_insert_ys'ys'_5044))
         (fun ys''ys''_4650 ->
          (k_insert_2260
            (fun x1_4664 x2_4665 k_insert_4666 ->
             (if (x1_4664 <= 0)
               (l0
                 (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                   (l1
                     (ysys_1016 0 (x2_4665 - 1)
                       (fun xs11_4683 xs12_4684 xs21_4685 xs22_4686 ->
                        (if xs21_4685
                          (l1
                            (if (1 = x2_4665)
                              (l0
                                (if (xs22_4686 = xs12_4684)
                                  (l1
                                    (if (p11_4797 <=> xs11_4683)
                                      (l1
                                        (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                          (l0 (loop_2167 () k_insert_4666)))) (
                                      l0 (l0 (loop_2167 () k_insert_4666))))) (
                                  l0 (loop_2167 () k_insert_4666))))
                              (l1
                                (l1
                                  (if (p11_4797 <=> xs11_4683)
                                    (l1
                                      (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                        (l0 (loop_2167 () k_insert_4666)))) (
                                    l0 (l0 (loop_2167 () k_insert_4666)))))))) (
                          l0 (loop_2167 () k_insert_4666))))))))
               (l1
                 (if (x2_4665 <= 0)
                   (l0
                     (ys''ys''_4650 x1_4664 0
                       (fun p11_4693 p12_4694 p21_4695 p22_4696 -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                   (l1 (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1) k_insert_4666)))))))))): X
ETA: (insert_1014 x_1053
       (fun x1_5042 x2_5043 k_insert_ys'ys'_5044 -> (ysys_1016 (x1_5042 + 1) (x2_5043 + 1) k_insert_ys'ys'_5044))
       (fun ys''ys''_4650 ->
        (k_insert_2260
          (fun x1_4664 x2_4665 k_insert_4666 ->
           (if (x1_4664 <= 0)
             (l0
               (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                 (l1
                   (ysys_1016 0 (x2_4665 - 1)
                     (fun xs11_4683 xs12_4684 xs21_4685 xs22_4686 ->
                      (if xs21_4685
                        (l1
                          (if (1 = x2_4665)
                            (l0
                              (if (xs22_4686 = xs12_4684)
                                (l1
                                  (if (p11_4797 <=> xs11_4683)
                                    (l1
                                      (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                        (l0 (loop_2167 () k_insert_4666)))) (
                                    l0 (l0 (loop_2167 () k_insert_4666))))) (
                                l0 (loop_2167 () k_insert_4666))))
                            (l1
                              (l1
                                (if (p11_4797 <=> xs11_4683)
                                  (l1
                                    (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                      (l0 (loop_2167 () k_insert_4666)))) (
                                  l0 (l0 (loop_2167 () k_insert_4666)))))))) (
                        l0 (loop_2167 () k_insert_4666))))))))
             (l1
               (if (x2_4665 <= 0)
                 (l0
                   (ys''ys''_4650 x1_4664 0
                     (fun p11_4693 p12_4694 p21_4695 p22_4696 -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                 (l1 (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1) k_insert_4666))))))))): X
ETA: (fun ys''ys''_4650 ->
      (k_insert_2260
        (fun x1_4664 x2_4665 k_insert_4666 ->
         (if (x1_4664 <= 0)
           (l0
             (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
               (l1
                 (ysys_1016 0 (x2_4665 - 1)
                   (fun xs11_4683 xs12_4684 xs21_4685 xs22_4686 ->
                    (if xs21_4685
                      (l1
                        (if (1 = x2_4665)
                          (l0
                            (if (xs22_4686 = xs12_4684)
                              (l1
                                (if (p11_4797 <=> xs11_4683)
                                  (l1
                                    (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                      (l0 (loop_2167 () k_insert_4666)))) (
                                  l0 (l0 (loop_2167 () k_insert_4666))))) (
                              l0 (loop_2167 () k_insert_4666))))
                          (l1
                            (l1
                              (if (p11_4797 <=> xs11_4683)
                                (l1
                                  (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                    (l0 (loop_2167 () k_insert_4666)))) (
                                l0 (l0 (loop_2167 () k_insert_4666)))))))) (
                      l0 (loop_2167 () k_insert_4666))))))))
           (l1
             (if (x2_4665 <= 0)
               (l0
                 (ys''ys''_4650 x1_4664 0
                   (fun p11_4693 p12_4694 p21_4695 p22_4696 -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
               (l1 (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1) k_insert_4666)))))))): ((
x_2:int ->
x_3:int ->
(x_5:bool ->
 x_6:int -> x_7:bool -> x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_2 < 0 || x_3 < 0 || x_6 <= x_8] -> X)
-> X) ->
X)
ETA: (k_insert_2260
       (fun x1_4664 x2_4665 k_insert_4666 ->
        (if (x1_4664 <= 0)
          (l0
            (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
              (l1
                (ysys_1016 0 (x2_4665 - 1)
                  (fun xs11_4683 xs12_4684 xs21_4685 xs22_4686 ->
                   (if xs21_4685
                     (l1
                       (if (1 = x2_4665)
                         (l0
                           (if (xs22_4686 = xs12_4684)
                             (l1
                               (if (p11_4797 <=> xs11_4683)
                                 (l1
                                   (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                     (l0 (loop_2167 () k_insert_4666)))) (
                                 l0 (l0 (loop_2167 () k_insert_4666))))) (
                             l0 (loop_2167 () k_insert_4666))))
                         (l1
                           (l1
                             (if (p11_4797 <=> xs11_4683)
                               (l1
                                 (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                   (l0 (loop_2167 () k_insert_4666)))) (
                               l0 (l0 (loop_2167 () k_insert_4666)))))))) (
                     l0 (loop_2167 () k_insert_4666))))))))
          (l1
            (if (x2_4665 <= 0)
              (l0
                (ys''ys''_4650 x1_4664 0
                  (fun p11_4693 p12_4694 p21_4695 p22_4696 -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
              (l1 (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1) k_insert_4666))))))): X
ETA: (fun x1_4664 x2_4665 k_insert_4666 ->
      (if (x1_4664 <= 0)
        (l0
          (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
            (l1
              (ysys_1016 0 (x2_4665 - 1)
                (fun xs11_4683 xs12_4684 xs21_4685 xs22_4686 ->
                 (if xs21_4685
                   (l1
                     (if (1 = x2_4665)
                       (l0
                         (if (xs22_4686 = xs12_4684)
                           (l1
                             (if (p11_4797 <=> xs11_4683)
                               (l1
                                 (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                   (l0 (loop_2167 () k_insert_4666)))) (
                               l0 (l0 (loop_2167 () k_insert_4666))))) (
                           l0 (loop_2167 () k_insert_4666))))
                       (l1
                         (l1
                           (if (p11_4797 <=> xs11_4683)
                             (l1
                               (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                 (l0 (loop_2167 () k_insert_4666)))) (
                             l0 (l0 (loop_2167 () k_insert_4666)))))))) (
                   l0 (loop_2167 () k_insert_4666))))))))
        (l1
          (if (x2_4665 <= 0)
            (l0
              (ys''ys''_4650 x1_4664 0
                (fun p11_4693 p12_4694 p21_4695 p22_4696 -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
            (l1 (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1) k_insert_4666)))))): (
x_1:int ->
x_2:int ->
(x_4:bool ->
 x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
-> X)
ETA: (fun x2_4665 k_insert_4666 ->
      (if (x1_4664 <= 0)
        (l0
          (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
            (l1
              (ysys_1016 0 (x2_4665 - 1)
                (fun xs11_4683 xs12_4684 xs21_4685 xs22_4686 ->
                 (if xs21_4685
                   (l1
                     (if (1 = x2_4665)
                       (l0
                         (if (xs22_4686 = xs12_4684)
                           (l1
                             (if (p11_4797 <=> xs11_4683)
                               (l1
                                 (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                   (l0 (loop_2167 () k_insert_4666)))) (
                               l0 (l0 (loop_2167 () k_insert_4666))))) (
                           l0 (loop_2167 () k_insert_4666))))
                       (l1
                         (l1
                           (if (p11_4797 <=> xs11_4683)
                             (l1
                               (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                 (l0 (loop_2167 () k_insert_4666)))) (
                             l0 (l0 (loop_2167 () k_insert_4666)))))))) (
                   l0 (loop_2167 () k_insert_4666))))))))
        (l1
          (if (x2_4665 <= 0)
            (l0
              (ys''ys''_4650 x1_4664 0
                (fun p11_4693 p12_4694 p21_4695 p22_4696 -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
            (l1 (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1) k_insert_4666)))))): (
x_1:int ->
(x_3:bool ->
 x_4:int ->
 x_5:bool -> x_6:int[(not x_3) || (not x_5) || (not (x1_4664 <= x_1)) || x1_4664 < 0 || x_1 < 0 || x_4 <= x_6] -> X)
-> X)
ETA: (fun k_insert_4666 ->
      (if (x1_4664 <= 0)
        (l0
          (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
            (l1
              (ysys_1016 0 (x2_4665 - 1)
                (fun xs11_4683 xs12_4684 xs21_4685 xs22_4686 ->
                 (if xs21_4685
                   (l1
                     (if (1 = x2_4665)
                       (l0
                         (if (xs22_4686 = xs12_4684)
                           (l1
                             (if (p11_4797 <=> xs11_4683)
                               (l1
                                 (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                   (l0 (loop_2167 () k_insert_4666)))) (
                               l0 (l0 (loop_2167 () k_insert_4666))))) (
                           l0 (loop_2167 () k_insert_4666))))
                       (l1
                         (l1
                           (if (p11_4797 <=> xs11_4683)
                             (l1
                               (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                 (l0 (loop_2167 () k_insert_4666)))) (
                             l0 (l0 (loop_2167 () k_insert_4666)))))))) (
                   l0 (loop_2167 () k_insert_4666))))))))
        (l1
          (if (x2_4665 <= 0)
            (l0
              (ys''ys''_4650 x1_4664 0
                (fun p11_4693 p12_4694 p21_4695 p22_4696 -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
            (l1 (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1) k_insert_4666)))))): ((
x_2:bool ->
x_3:int ->
x_4:bool -> x_5:int[(not x_2) || (not x_4) || (not (x1_4664 <= x2_4665)) || x1_4664 < 0 || x2_4665 < 0 || x_3 <= x_5]
-> X) ->
X)
ETA: (if (x1_4664 <= 0)
       (l0
         (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
           (l1
             (ysys_1016 0 (x2_4665 - 1)
               (fun xs11_4683 xs12_4684 xs21_4685 xs22_4686 ->
                (if xs21_4685
                  (l1
                    (if (1 = x2_4665)
                      (l0
                        (if (xs22_4686 = xs12_4684)
                          (l1
                            (if (p11_4797 <=> xs11_4683)
                              (l1
                                (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                  (l0 (loop_2167 () k_insert_4666)))) (
                              l0 (l0 (loop_2167 () k_insert_4666))))) (
                          l0 (loop_2167 () k_insert_4666))))
                      (l1
                        (l1
                          (if (p11_4797 <=> xs11_4683)
                            (l1
                              (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                (l0 (loop_2167 () k_insert_4666)))) (
                            l0 (l0 (loop_2167 () k_insert_4666)))))))) (
                  l0 (loop_2167 () k_insert_4666))))))))
       (l1
         (if (x2_4665 <= 0)
           (l0
             (ys''ys''_4650 x1_4664 0
               (fun p11_4693 p12_4694 p21_4695 p22_4696 -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
           (l1 (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1) k_insert_4666))))): X
ETA: (l1
       (if (x2_4665 <= 0)
         (l0
           (ys''ys''_4650 x1_4664 0
             (fun p11_4693 p12_4694 p21_4695 p22_4696 -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
         (l1 (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1) k_insert_4666)))): X
ETA: (if (x2_4665 <= 0)
       (l0
         (ys''ys''_4650 x1_4664 0
           (fun p11_4693 p12_4694 p21_4695 p22_4696 -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
       (l1 (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1) k_insert_4666))): X
ETA: (l1 (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1) k_insert_4666)): X
ETA: (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1) k_insert_4666): X
ETA: k_insert_4666: (x_1:bool ->
                     x_2:int ->
                     x_3:bool ->
                     x_4:int[(not x_1) || (not x_3) || (not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                             x1_4664 - 1 < 0 || x2_4665 - 1 < 0 || x_2 <= x_4]
                     -> X)
ETA_AUX: k_insert_4666: (x_1:bool ->
                         x_2:int ->
                         x_3:bool ->
                         x_4:int[(not x_1) || (not x_3) || (not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                 x1_4664 - 1 < 0 || x2_4665 - 1 < 0 || 
                                 x_2 <= x_4]
                         -> X)
ETA_AUX: x__5060: bool
ETA_AUX: (k_insert_4666 x__5060): (x_1:int ->
                                   x_2:bool ->
                                   x_3:int[(not x__5060) || (not x_2) || (
                                           not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                           x1_4664 - 1 < 0 || x2_4665 - 1 < 0 || 
                                           x_1 <= x_3]
                                   -> X)
ETA_AUX: x__5061: int
ETA_AUX: (k_insert_4666 x__5060 x__5061): (x_1:bool ->
                                           x_2:int[(not x__5060) || (
                                                   not x_1) || (not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                                   x1_4664 - 1 < 0 || 
                                                   x2_4665 - 1 < 0 || 
                                                   x__5061 <= x_2]
                                           -> X)
ETA_AUX: x__5062: bool
ETA_AUX: (k_insert_4666 x__5060 x__5061 x__5062): (x_1:int[(not x__5060) || (
                                                           not x__5062) || (
                                                           not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                                           x1_4664 - 1 < 0 || 
                                                           x2_4665 - 1 < 0 || 
                                                           x__5061 <= x_1] ->
X)
ETA_AUX: x__5063: x_1:int[(not x__5060) || (not x__5062) || (not (x1_4664 <= x2_4665)) || 
                          x1_4664 < 0 || x2_4665 < 0 || x__5061 <= x_1]
ETA_AUX: (k_insert_4666 x__5060 x__5061 x__5062 x__5063): X
ETA: (x2_4665 - 1): int
ETA: (x1_4664 - 1): int
ETA_AUX: (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1)
           (fun (x__5060:bool) (x__5061:int) (x__5062:bool) 
                (x__5063:x_1:int[(not x__5060) || (not x__5062) || (not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                 x1_4664 - 1 < 0 || x2_4665 - 1 < 0 || 
                                 x__5061 <= x_1])
            -> (k_insert_4666 x__5060 x__5061 x__5062 x__5063))): X
ETA: (l0
       (ys''ys''_4650 x1_4664 0
         (fun p11_4693 p12_4694 p21_4695 p22_4696 -> (k_insert_4666 p11_4693 p12_4694 true p12_4798)))): X
ETA: (ys''ys''_4650 x1_4664 0
       (fun p11_4693 p12_4694 p21_4695 p22_4696 -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))): X
ETA: (fun p11_4693 p12_4694 p21_4695 p22_4696 -> (k_insert_4666 p11_4693 p12_4694 true p12_4798)): (
x_1:bool ->
x_2:int ->
x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x1_4664 <= 0)) || x1_4664 < 0 || 0 < 0 || x_2 <= x_4] -> X)
ETA: (fun p12_4694 p21_4695 p22_4696 -> (k_insert_4666 p11_4693 p12_4694 true p12_4798)): (
x_1:int ->
x_2:bool -> x_3:int[(not p11_4693) || (not x_2) || (not (x1_4664 <= 0)) || x1_4664 < 0 || 0 < 0 || x_1 <= x_3] -> X)
ETA: (fun p21_4695 p22_4696 -> (k_insert_4666 p11_4693 p12_4694 true p12_4798)): (
x_1:bool -> x_2:int[(not p11_4693) || (not x_1) || (not (x1_4664 <= 0)) || x1_4664 < 0 || 0 < 0 || p12_4694 <= x_2] ->
X)
ETA: (fun p22_4696 -> (k_insert_4666 p11_4693 p12_4694 true p12_4798)): (x_1:int[
(not p11_4693) || (not p21_4695) || (not (x1_4664 <= 0)) || x1_4664 < 0 || 0 < 0 || p12_4694 <= x_1] ->
X)
ETA: (k_insert_4666 p11_4693 p12_4694 true p12_4798): X
ETA: p12_4798: x_1:int[(not p11_4693) || (not true) || (not (x1_4664 <= x2_4665)) || 
                       x1_4664 < 0 || x2_4665 < 0 || p12_4694 <= x_1]
ETA: true: bool
ETA: p12_4694: int
ETA: p11_4693: bool
ETA_AUX: (k_insert_4666 p11_4693 p12_4694 true p12_4798): X
ETA: 0: int
ETA: x1_4664: int
ETA_AUX: (ys''ys''_4650 x1_4664 0
           (fun (p11_4693:bool) (p12_4694:int) (p21_4695:bool) 
                (p22_4696:x_1:int[(not p11_4693) || (not p21_4695) || (
                                  not (x1_4664 <= 0)) || x1_4664 < 0 || 
                                  0 < 0 || p12_4694 <= x_1])
            -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))): X
ETA: (l0
       (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
         (l1
           (ysys_1016 0 (x2_4665 - 1)
             (fun xs11_4683 xs12_4684 xs21_4685 xs22_4686 ->
              (if xs21_4685
                (l1
                  (if (1 = x2_4665)
                    (l0
                      (if (xs22_4686 = xs12_4684)
                        (l1
                          (if (p11_4797 <=> xs11_4683)
                            (l1
                              (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                (l0 (loop_2167 () k_insert_4666)))) (
                            l0 (l0 (loop_2167 () k_insert_4666))))) (
                        l0 (loop_2167 () k_insert_4666))))
                    (l1
                      (l1
                        (if (p11_4797 <=> xs11_4683)
                          (l1
                            (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                              (l0 (loop_2167 () k_insert_4666)))) (l0 (l0 (loop_2167 () k_insert_4666))))))))
                (l0 (loop_2167 () k_insert_4666)))))))): X
ETA: (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
       (l1
         (ysys_1016 0 (x2_4665 - 1)
           (fun xs11_4683 xs12_4684 xs21_4685 xs22_4686 ->
            (if xs21_4685
              (l1
                (if (1 = x2_4665)
                  (l0
                    (if (xs22_4686 = xs12_4684)
                      (l1
                        (if (p11_4797 <=> xs11_4683)
                          (l1
                            (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                              (l0 (loop_2167 () k_insert_4666)))) (l0 (l0 (loop_2167 () k_insert_4666)))))
                      (l0 (loop_2167 () k_insert_4666))))
                  (l1
                    (l1
                      (if (p11_4797 <=> xs11_4683)
                        (l1
                          (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                            (l0 (loop_2167 () k_insert_4666)))) (l0 (l0 (loop_2167 () k_insert_4666))))))))
              (l0 (loop_2167 () k_insert_4666))))))): X
ETA: (l1
       (ysys_1016 0 (x2_4665 - 1)
         (fun xs11_4683 xs12_4684 xs21_4685 xs22_4686 ->
          (if xs21_4685
            (l1
              (if (1 = x2_4665)
                (l0
                  (if (xs22_4686 = xs12_4684)
                    (l1
                      (if (p11_4797 <=> xs11_4683)
                        (l1
                          (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                            (l0 (loop_2167 () k_insert_4666)))) (l0 (l0 (loop_2167 () k_insert_4666)))))
                    (l0 (loop_2167 () k_insert_4666))))
                (l1
                  (l1
                    (if (p11_4797 <=> xs11_4683)
                      (l1
                        (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                          (l0 (loop_2167 () k_insert_4666)))) (l0 (l0 (loop_2167 () k_insert_4666))))))))
            (l0 (loop_2167 () k_insert_4666)))))): X
ETA: (ysys_1016 0 (x2_4665 - 1)
       (fun xs11_4683 xs12_4684 xs21_4685 xs22_4686 ->
        (if xs21_4685
          (l1
            (if (1 = x2_4665)
              (l0
                (if (xs22_4686 = xs12_4684)
                  (l1
                    (if (p11_4797 <=> xs11_4683)
                      (l1
                        (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                          (l0 (loop_2167 () k_insert_4666)))) (l0 (l0 (loop_2167 () k_insert_4666)))))
                  (l0 (loop_2167 () k_insert_4666))))
              (l1
                (l1
                  (if (p11_4797 <=> xs11_4683)
                    (l1
                      (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                        (l0 (loop_2167 () k_insert_4666)))) (l0 (l0 (loop_2167 () k_insert_4666))))))))
          (l0 (loop_2167 () k_insert_4666))))): X
ETA: (fun xs11_4683 xs12_4684 xs21_4685 xs22_4686 ->
      (if xs21_4685
        (l1
          (if (1 = x2_4665)
            (l0
              (if (xs22_4686 = xs12_4684)
                (l1
                  (if (p11_4797 <=> xs11_4683)
                    (l1
                      (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                        (l0 (loop_2167 () k_insert_4666)))) (l0 (l0 (loop_2167 () k_insert_4666)))))
                (l0 (loop_2167 () k_insert_4666))))
            (l1
              (l1
                (if (p11_4797 <=> xs11_4683)
                  (l1
                    (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                      (l0 (loop_2167 () k_insert_4666)))) (l0 (l0 (loop_2167 () k_insert_4666))))))))
        (l0 (loop_2167 () k_insert_4666)))): (x_1:bool ->
                                              x_2:int ->
                                              x_3:bool ->
                                              x_4:int[(not x_1) || (not x_3) || (
                                                      not (0 <= (x2_4665 - 1))) || 
                                                      0 < 0 || x2_4665 - 1 < 0 || 
                                                      x_2 <= x_4]
                                              -> X)
ETA: (fun xs12_4684 xs21_4685 xs22_4686 ->
      (if xs21_4685
        (l1
          (if (1 = x2_4665)
            (l0
              (if (xs22_4686 = xs12_4684)
                (l1
                  (if (p11_4797 <=> xs11_4683)
                    (l1
                      (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                        (l0 (loop_2167 () k_insert_4666)))) (l0 (l0 (loop_2167 () k_insert_4666)))))
                (l0 (loop_2167 () k_insert_4666))))
            (l1
              (l1
                (if (p11_4797 <=> xs11_4683)
                  (l1
                    (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                      (l0 (loop_2167 () k_insert_4666)))) (l0 (l0 (loop_2167 () k_insert_4666))))))))
        (l0 (loop_2167 () k_insert_4666)))): (x_1:int ->
                                              x_2:bool ->
                                              x_3:int[(not xs11_4683) || (
                                                      not x_2) || (not (0 <= (x2_4665 - 1))) || 
                                                      0 < 0 || x2_4665 - 1 < 0 || 
                                                      x_1 <= x_3]
                                              -> X)
ETA: (fun xs21_4685 xs22_4686 ->
      (if xs21_4685
        (l1
          (if (1 = x2_4665)
            (l0
              (if (xs22_4686 = xs12_4684)
                (l1
                  (if (p11_4797 <=> xs11_4683)
                    (l1
                      (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                        (l0 (loop_2167 () k_insert_4666)))) (l0 (l0 (loop_2167 () k_insert_4666)))))
                (l0 (loop_2167 () k_insert_4666))))
            (l1
              (l1
                (if (p11_4797 <=> xs11_4683)
                  (l1
                    (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                      (l0 (loop_2167 () k_insert_4666)))) (l0 (l0 (loop_2167 () k_insert_4666))))))))
        (l0 (loop_2167 () k_insert_4666)))): (x_1:bool ->
                                              x_2:int[(not xs11_4683) || (
                                                      not x_1) || (not (0 <= (x2_4665 - 1))) || 
                                                      0 < 0 || x2_4665 - 1 < 0 || 
                                                      xs12_4684 <= x_2]
                                              -> X)
ETA: (fun xs22_4686 ->
      (if xs21_4685
        (l1
          (if (1 = x2_4665)
            (l0
              (if (xs22_4686 = xs12_4684)
                (l1
                  (if (p11_4797 <=> xs11_4683)
                    (l1
                      (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                        (l0 (loop_2167 () k_insert_4666)))) (l0 (l0 (loop_2167 () k_insert_4666)))))
                (l0 (loop_2167 () k_insert_4666))))
            (l1
              (l1
                (if (p11_4797 <=> xs11_4683)
                  (l1
                    (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                      (l0 (loop_2167 () k_insert_4666)))) (l0 (l0 (loop_2167 () k_insert_4666))))))))
        (l0 (loop_2167 () k_insert_4666)))): (x_1:int[(not xs11_4683) || (
                                                      not xs21_4685) || (
                                                      not (0 <= (x2_4665 - 1))) || 
                                                      0 < 0 || x2_4665 - 1 < 0 || 
                                                      xs12_4684 <= x_1] ->
X)
ETA: (if xs21_4685
       (l1
         (if (1 = x2_4665)
           (l0
             (if (xs22_4686 = xs12_4684)
               (l1
                 (if (p11_4797 <=> xs11_4683)
                   (l1
                     (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                       (l0 (loop_2167 () k_insert_4666)))) (l0 (l0 (loop_2167 () k_insert_4666)))))
               (l0 (loop_2167 () k_insert_4666))))
           (l1
             (l1
               (if (p11_4797 <=> xs11_4683)
                 (l1
                   (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                     (l0 (loop_2167 () k_insert_4666)))) (l0 (l0 (loop_2167 () k_insert_4666))))))))
       (l0 (loop_2167 () k_insert_4666))): X
ETA: (l0 (loop_2167 () k_insert_4666)): X
ETA: (loop_2167 () k_insert_4666): X
ETA: k_insert_4666: (bool -> int -> bool -> int -> X)
ETA_AUX: k_insert_4666: (bool -> int -> bool -> int -> X)
ETA_AUX: x__5064: bool
ETA_AUX: (k_insert_4666 x__5064): (int -> bool -> int -> X)
ETA_AUX: x__5065: int
ETA_AUX: (k_insert_4666 x__5064 x__5065): (bool -> int -> X)
ETA_AUX: x__5066: bool
ETA_AUX: (k_insert_4666 x__5064 x__5065 x__5066): (int ->
X)
ETA_AUX: x__5067: x_1:int[(not x__5064) || (not x__5066) || (not (x1_4664 <= x2_4665)) || 
                          x1_4664 < 0 || x2_4665 < 0 || x__5065 <= x_1]
ETA_AUX: (k_insert_4666 x__5064 x__5065 x__5066 x__5067): X
ETA: (): unit
ETA_AUX: (loop_2167 ()
           (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
            (k_insert_4666 x__5064 x__5065 x__5066 x__5067))): X
ETA: (l1
       (if (1 = x2_4665)
         (l0
           (if (xs22_4686 = xs12_4684)
             (l1
               (if (p11_4797 <=> xs11_4683)
                 (l1
                   (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                     (l0 (loop_2167 () k_insert_4666)))) (l0 (l0 (loop_2167 () k_insert_4666)))))
             (l0 (loop_2167 () k_insert_4666))))
         (l1
           (l1
             (if (p11_4797 <=> xs11_4683)
               (l1
                 (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                   (l0 (loop_2167 () k_insert_4666)))) (l0 (l0 (loop_2167 () k_insert_4666)))))))): X
ETA: (if (1 = x2_4665)
       (l0
         (if (xs22_4686 = xs12_4684)
           (l1
             (if (p11_4797 <=> xs11_4683)
               (l1
                 (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                   (l0 (loop_2167 () k_insert_4666)))) (l0 (l0 (loop_2167 () k_insert_4666)))))
           (l0 (loop_2167 () k_insert_4666))))
       (l1
         (l1
           (if (p11_4797 <=> xs11_4683)
             (l1
               (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                 (l0 (loop_2167 () k_insert_4666)))) (l0 (l0 (loop_2167 () k_insert_4666))))))): X
ETA: (l1
       (l1
         (if (p11_4797 <=> xs11_4683)
           (l1
             (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
               (l0 (loop_2167 () k_insert_4666)))) (l0 (l0 (loop_2167 () k_insert_4666)))))): X
ETA: (l1
       (if (p11_4797 <=> xs11_4683)
         (l1
           (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
             (l0 (loop_2167 () k_insert_4666)))) (l0 (l0 (loop_2167 () k_insert_4666))))): X
ETA: (if (p11_4797 <=> xs11_4683)
       (l1
         (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686)) (l0 (loop_2167 () k_insert_4666))))
       (l0 (l0 (loop_2167 () k_insert_4666)))): X
ETA: (l0 (l0 (loop_2167 () k_insert_4666))): X
ETA: (l0 (loop_2167 () k_insert_4666)): X
ETA: (loop_2167 () k_insert_4666): X
ETA: k_insert_4666: (bool -> int -> bool -> int -> X)
ETA_AUX: k_insert_4666: (bool -> int -> bool -> int -> X)
ETA_AUX: x__5068: bool
ETA_AUX: (k_insert_4666 x__5068): (int -> bool -> int -> X)
ETA_AUX: x__5069: int
ETA_AUX: (k_insert_4666 x__5068 x__5069): (bool -> int -> X)
ETA_AUX: x__5070: bool
ETA_AUX: (k_insert_4666 x__5068 x__5069 x__5070): (int ->
X)
ETA_AUX: x__5071: x_1:int[(not x__5068) || (not x__5070) || (not (x1_4664 <= x2_4665)) || 
                          x1_4664 < 0 || x2_4665 < 0 || x__5069 <= x_1]
ETA_AUX: (k_insert_4666 x__5068 x__5069 x__5070 x__5071): X
ETA: (): unit
ETA_AUX: (loop_2167 ()
           (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
            (k_insert_4666 x__5068 x__5069 x__5070 x__5071))): X
ETA: (l1
       (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686)) (l0 (loop_2167 () k_insert_4666)))): X
ETA: (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686)) (l0 (loop_2167 () k_insert_4666))): X
ETA: (l0 (loop_2167 () k_insert_4666)): X
ETA: (loop_2167 () k_insert_4666): X
ETA: k_insert_4666: (bool -> int -> bool -> int -> X)
ETA_AUX: k_insert_4666: (bool -> int -> bool -> int -> X)
ETA_AUX: x__5072: bool
ETA_AUX: (k_insert_4666 x__5072): (int -> bool -> int -> X)
ETA_AUX: x__5073: int
ETA_AUX: (k_insert_4666 x__5072 x__5073): (bool -> int -> X)
ETA_AUX: x__5074: bool
ETA_AUX: (k_insert_4666 x__5072 x__5073 x__5074): (int ->
X)
ETA_AUX: x__5075: x_1:int[(not x__5072) || (not x__5074) || (not (x1_4664 <= x2_4665)) || 
                          x1_4664 < 0 || x2_4665 < 0 || x__5073 <= x_1]
ETA_AUX: (k_insert_4666 x__5072 x__5073 x__5074 x__5075): X
ETA: (): unit
ETA_AUX: (loop_2167 ()
           (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
            (k_insert_4666 x__5072 x__5073 x__5074 x__5075))): X
ETA: (l1 (k_insert_4666 true p12_4798 true xs22_4686)): X
ETA: (k_insert_4666 true p12_4798 true xs22_4686): X
ETA: xs22_4686: x_1:int[(not true) || (not true) || (not (x1_4664 <= x2_4665)) || 
                        x1_4664 < 0 || x2_4665 < 0 || p12_4798 <= x_1]
ETA: true: bool
ETA: p12_4798: int
ETA: true: bool
ETA_AUX: (k_insert_4666 true p12_4798 true xs22_4686): X
ETA: (l0
       (if (xs22_4686 = xs12_4684)
         (l1
           (if (p11_4797 <=> xs11_4683)
             (l1
               (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                 (l0 (loop_2167 () k_insert_4666)))) (l0 (l0 (loop_2167 () k_insert_4666)))))
         (l0 (loop_2167 () k_insert_4666)))): X
ETA: (if (xs22_4686 = xs12_4684)
       (l1
         (if (p11_4797 <=> xs11_4683)
           (l1
             (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
               (l0 (loop_2167 () k_insert_4666)))) (l0 (l0 (loop_2167 () k_insert_4666)))))
       (l0 (loop_2167 () k_insert_4666))): X
ETA: (l0 (loop_2167 () k_insert_4666)): X
ETA: (loop_2167 () k_insert_4666): X
ETA: k_insert_4666: (bool -> int -> bool -> int -> X)
ETA_AUX: k_insert_4666: (bool -> int -> bool -> int -> X)
ETA_AUX: x__5076: bool
ETA_AUX: (k_insert_4666 x__5076): (int -> bool -> int -> X)
ETA_AUX: x__5077: int
ETA_AUX: (k_insert_4666 x__5076 x__5077): (bool -> int -> X)
ETA_AUX: x__5078: bool
ETA_AUX: (k_insert_4666 x__5076 x__5077 x__5078): (int ->
X)
ETA_AUX: x__5079: x_1:int[(not x__5076) || (not x__5078) || (not (x1_4664 <= x2_4665)) || 
                          x1_4664 < 0 || x2_4665 < 0 || x__5077 <= x_1]
ETA_AUX: (k_insert_4666 x__5076 x__5077 x__5078 x__5079): X
ETA: (): unit
ETA_AUX: (loop_2167 ()
           (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
            (k_insert_4666 x__5076 x__5077 x__5078 x__5079))): X
ETA: (l1
       (if (p11_4797 <=> xs11_4683)
         (l1
           (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
             (l0 (loop_2167 () k_insert_4666)))) (l0 (l0 (loop_2167 () k_insert_4666))))): X
ETA: (if (p11_4797 <=> xs11_4683)
       (l1
         (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686)) (l0 (loop_2167 () k_insert_4666))))
       (l0 (l0 (loop_2167 () k_insert_4666)))): X
ETA: (l0 (l0 (loop_2167 () k_insert_4666))): X
ETA: (l0 (loop_2167 () k_insert_4666)): X
ETA: (loop_2167 () k_insert_4666): X
ETA: k_insert_4666: (bool -> int -> bool -> int -> X)
ETA_AUX: k_insert_4666: (bool -> int -> bool -> int -> X)
ETA_AUX: x__5080: bool
ETA_AUX: (k_insert_4666 x__5080): (int -> bool -> int -> X)
ETA_AUX: x__5081: int
ETA_AUX: (k_insert_4666 x__5080 x__5081): (bool -> int -> X)
ETA_AUX: x__5082: bool
ETA_AUX: (k_insert_4666 x__5080 x__5081 x__5082): (int ->
X)
ETA_AUX: x__5083: x_1:int[(not x__5080) || (not x__5082) || (not (x1_4664 <= x2_4665)) || 
                          x1_4664 < 0 || x2_4665 < 0 || x__5081 <= x_1]
ETA_AUX: (k_insert_4666 x__5080 x__5081 x__5082 x__5083): X
ETA: (): unit
ETA_AUX: (loop_2167 ()
           (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
            (k_insert_4666 x__5080 x__5081 x__5082 x__5083))): X
ETA: (l1
       (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686)) (l0 (loop_2167 () k_insert_4666)))): X
ETA: (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686)) (l0 (loop_2167 () k_insert_4666))): X
ETA: (l0 (loop_2167 () k_insert_4666)): X
ETA: (loop_2167 () k_insert_4666): X
ETA: k_insert_4666: (bool -> int -> bool -> int -> X)
ETA_AUX: k_insert_4666: (bool -> int -> bool -> int -> X)
ETA_AUX: x__5084: bool
ETA_AUX: (k_insert_4666 x__5084): (int -> bool -> int -> X)
ETA_AUX: x__5085: int
ETA_AUX: (k_insert_4666 x__5084 x__5085): (bool -> int -> X)
ETA_AUX: x__5086: bool
ETA_AUX: (k_insert_4666 x__5084 x__5085 x__5086): (int ->
X)
ETA_AUX: x__5087: x_1:int[(not x__5084) || (not x__5086) || (not (x1_4664 <= x2_4665)) || 
                          x1_4664 < 0 || x2_4665 < 0 || x__5085 <= x_1]
ETA_AUX: (k_insert_4666 x__5084 x__5085 x__5086 x__5087): X
ETA: (): unit
ETA_AUX: (loop_2167 ()
           (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int) ->
            (k_insert_4666 x__5084 x__5085 x__5086 x__5087))): X
ETA: (l1 (k_insert_4666 true p12_4798 true xs22_4686)): X
ETA: (k_insert_4666 true p12_4798 true xs22_4686): X
ETA: xs22_4686: x_1:int[(not true) || (not true) || (not (x1_4664 <= x2_4665)) || 
                        x1_4664 < 0 || x2_4665 < 0 || p12_4798 <= x_1]
ETA: true: bool
ETA: p12_4798: int
ETA: true: bool
ETA_AUX: (k_insert_4666 true p12_4798 true xs22_4686): X
ETA: (x2_4665 - 1): int
ETA: 0: int
ETA_AUX: (ysys_1016 0 (x2_4665 - 1)
           (fun (xs11_4683:bool) (xs12_4684:int) (xs21_4685:bool) 
                (xs22_4686:x_1:int[(not xs11_4683) || (not xs21_4685) || (
                                   not (0 <= (x2_4665 - 1))) || 0 < 0 || 
                                   x2_4665 - 1 < 0 || xs12_4684 <= x_1])
            ->
            (if xs21_4685
              (l1
                (if (1 = x2_4665)
                  (l0
                    (if (xs22_4686 = xs12_4684)
                      (l1
                        (if (p11_4797 <=> xs11_4683)
                          (l1
                            (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                              (l0
                                (loop_2167 ()
                                  (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int) ->
                                   (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                          (l0
                            (l0
                              (loop_2167 ()
                                (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
                                 (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                      (l0
                        (loop_2167 ()
                          (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                           (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))))
                  (l1
                    (l1
                      (if (p11_4797 <=> xs11_4683)
                        (l1
                          (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                            (l0
                              (loop_2167 ()
                                (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
                                 (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                        (l0
                          (l0
                            (loop_2167 ()
                              (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
                               (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))))))))
              (l0
                (loop_2167 ()
                  (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                   (k_insert_4666 x__5064 x__5065 x__5066 x__5067))))))): X
ETA: (l0 (k_insert_4666 true p12_4798 true p12_4798)): X
ETA: (k_insert_4666 true p12_4798 true p12_4798): X
ETA: p12_4798: x_1:int[(not true) || (not true) || (not (x1_4664 <= x2_4665)) || 
                       x1_4664 < 0 || x2_4665 < 0 || p12_4798 <= x_1]
ETA: true: bool
ETA: p12_4798: int
ETA: true: bool
ETA_AUX: (k_insert_4666 true p12_4798 true p12_4798): X
ETA_AUX: (k_insert_2260
           (fun (x1_4664:int) (x2_4665:int) 
                (k_insert_4666:(x_1:bool ->
                                x_2:int ->
                                x_3:bool ->
                                x_4:int[(not x_1) || (not x_3) || (not (x1_4664 <= x2_4665)) || 
                                        x1_4664 < 0 || x2_4665 < 0 || 
                                        x_2 <= x_4]
                                -> X))
            ->
            (if (x1_4664 <= 0)
              (l0
                (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                  (l1
                    (ysys_1016 0 (x2_4665 - 1)
                      (fun (xs11_4683:bool) (xs12_4684:int) (xs21_4685:bool) 
                           (xs22_4686:x_1:int[(not xs11_4683) || (not xs21_4685) || (
                                              not (0 <= (x2_4665 - 1))) || 
                                              0 < 0 || x2_4665 - 1 < 0 || 
                                              xs12_4684 <= x_1])
                       ->
                       (if xs21_4685
                         (l1
                           (if (1 = x2_4665)
                             (l0
                               (if (xs22_4686 = xs12_4684)
                                 (l1
                                   (if (p11_4797 <=> xs11_4683)
                                     (l1
                                       (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                         (l0
                                           (loop_2167 ()
                                             (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int) ->
                                              (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                                     (l0
                                       (l0
                                         (loop_2167 ()
                                           (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
                                            (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                                 (l0
                                   (loop_2167 ()
                                     (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                                      (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))))
                             (l1
                               (l1
                                 (if (p11_4797 <=> xs11_4683)
                                   (l1
                                     (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                       (l0
                                         (loop_2167 ()
                                           (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
                                            (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                                   (l0
                                     (l0
                                       (loop_2167 ()
                                         (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
                                          (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))))))))
                         (l0
                           (loop_2167 ()
                             (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                              (k_insert_4666 x__5064 x__5065 x__5066 x__5067))))))))))
              (l1
                (if (x2_4665 <= 0)
                  (l0
                    (ys''ys''_4650 x1_4664 0
                      (fun (p11_4693:bool) (p12_4694:int) (p21_4695:bool) 
                           (p22_4696:x_1:int[(not p11_4693) || (not p21_4695) || (
                                             not (x1_4664 <= 0)) || x1_4664 < 0 || 
                                             0 < 0 || p12_4694 <= x_1])
                       -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                  (l1
                    (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1)
                      (fun (x__5060:bool) (x__5061:int) (x__5062:bool) 
                           (x__5063:x_1:int[(not x__5060) || (not x__5062) || 
                                            (not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                            x1_4664 - 1 < 0 || x2_4665 - 1 < 0 || 
                                            x__5061 <= x_1])
                       -> (k_insert_4666 x__5060 x__5061 x__5062 x__5063))))))))): X
ETA: (fun x1_5042 x2_5043 k_insert_ys'ys'_5044 -> (ysys_1016 (x1_5042 + 1) (x2_5043 + 1) k_insert_ys'ys'_5044)): (
x_1:int ->
x_2:int ->
(x_4:bool ->
 x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
-> X)
ETA: (fun x2_5043 k_insert_ys'ys'_5044 -> (ysys_1016 (x1_5042 + 1) (x2_5043 + 1) k_insert_ys'ys'_5044)): (
x_1:int ->
(x_3:bool ->
 x_4:int ->
 x_5:bool -> x_6:int[(not x_3) || (not x_5) || (not (x1_5042 <= x_1)) || x1_5042 < 0 || x_1 < 0 || x_4 <= x_6] -> X)
-> X)
ETA: (fun k_insert_ys'ys'_5044 -> (ysys_1016 (x1_5042 + 1) (x2_5043 + 1) k_insert_ys'ys'_5044)): ((
x_2:bool ->
x_3:int ->
x_4:bool -> x_5:int[(not x_2) || (not x_4) || (not (x1_5042 <= x2_5043)) || x1_5042 < 0 || x2_5043 < 0 || x_3 <= x_5]
-> X) ->
X)
ETA: (ysys_1016 (x1_5042 + 1) (x2_5043 + 1) k_insert_ys'ys'_5044): X
ETA: k_insert_ys'ys'_5044: (x_1:bool ->
                            x_2:int ->
                            x_3:bool ->
                            x_4:int[(not x_1) || (not x_3) || (not ((x1_5042 + 1) <= (x2_5043 + 1))) || 
                                    x1_5042 + 1 < 0 || x2_5043 + 1 < 0 || 
                                    x_2 <= x_4]
                            -> X)
ETA_AUX: k_insert_ys'ys'_5044: (x_1:bool ->
                                x_2:int ->
                                x_3:bool ->
                                x_4:int[(not x_1) || (not x_3) || (not ((x1_5042 + 1) <= (x2_5043 + 1))) || 
                                        x1_5042 + 1 < 0 || x2_5043 + 1 < 0 || 
                                        x_2 <= x_4]
                                -> X)
ETA_AUX: x__5088: bool
ETA_AUX: (k_insert_ys'ys'_5044 x__5088): (x_1:int ->
                                          x_2:bool ->
                                          x_3:int[(not x__5088) || (not x_2) || 
                                                  (not ((x1_5042 + 1) <= (x2_5043 + 1))) || 
                                                  x1_5042 + 1 < 0 || 
                                                  x2_5043 + 1 < 0 || 
                                                  x_1 <= x_3]
                                          -> X)
ETA_AUX: x__5089: int
ETA_AUX: (k_insert_ys'ys'_5044 x__5088 x__5089): (x_1:bool ->
                                                  x_2:int[(not x__5088) || (
                                                          not x_1) || (
                                                          not ((x1_5042 + 1) <= (x2_5043 + 1))) || 
                                                          x1_5042 + 1 < 0 || 
                                                          x2_5043 + 1 < 0 || 
                                                          x__5089 <= x_2]
                                                  -> X)
ETA_AUX: x__5090: bool
ETA_AUX: (k_insert_ys'ys'_5044 x__5088 x__5089 x__5090): (x_1:int[(not x__5088) || (
                                                                  not x__5090) || 
                                                                  (not ((x1_5042 + 1) <= (x2_5043 + 1))) || 
                                                                  x1_5042 + 1 < 0 || 
                                                                  x2_5043 + 1 < 0 || 
                                                                  x__5089 <= x_1] ->
X)
ETA_AUX: x__5091: x_1:int[(not x__5088) || (not x__5090) || (not (x1_5042 <= x2_5043)) || 
                          x1_5042 < 0 || x2_5043 < 0 || x__5089 <= x_1]
ETA_AUX: (k_insert_ys'ys'_5044 x__5088 x__5089 x__5090 x__5091): X
ETA: (x2_5043 + 1): int
ETA: (x1_5042 + 1): int
ETA_AUX: (ysys_1016 (x1_5042 + 1) (x2_5043 + 1)
           (fun (x__5088:bool) (x__5089:int) (x__5090:bool) 
                (x__5091:x_1:int[(not x__5088) || (not x__5090) || (not ((x1_5042 + 1) <= (x2_5043 + 1))) || 
                                 x1_5042 + 1 < 0 || x2_5043 + 1 < 0 || 
                                 x__5089 <= x_1])
            -> (k_insert_ys'ys'_5044 x__5088 x__5089 x__5090 x__5091))): X
ETA: x_1053: int
ETA_AUX: (insert_1014 x_1053
           (fun (x1_5042:int) (x2_5043:int) 
                (k_insert_ys'ys'_5044:(x_1:bool ->
                                       x_2:int ->
                                       x_3:bool ->
                                       x_4:int[(not x_1) || (not x_3) || (
                                               not (x1_5042 <= x2_5043)) || 
                                               x1_5042 < 0 || x2_5043 < 0 || 
                                               x_2 <= x_4]
                                       -> X))
            ->
            (ysys_1016 (x1_5042 + 1) (x2_5043 + 1)
              (fun (x__5088:bool) (x__5089:int) (x__5090:bool) 
                   (x__5091:x_1:int[(not x__5088) || (not x__5090) || (
                                    not ((x1_5042 + 1) <= (x2_5043 + 1))) || 
                                    x1_5042 + 1 < 0 || x2_5043 + 1 < 0 || 
                                    x__5089 <= x_1])
               -> (k_insert_ys'ys'_5044 x__5088 x__5089 x__5090 x__5091))))
           (fun (ys''ys''_4650:(x_1:int ->
                                x_2:int ->
                                (x_4:bool ->
                                 x_5:int ->
                                 x_6:bool ->
                                 x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || 
                                         x_1 < 0 || x_2 < 0 || x_5 <= x_7]
                                 -> X)
                                -> X))
            ->
            (k_insert_2260
              (fun (x1_4664:int) (x2_4665:int) 
                   (k_insert_4666:(x_1:bool ->
                                   x_2:int ->
                                   x_3:bool ->
                                   x_4:int[(not x_1) || (not x_3) || (
                                           not (x1_4664 <= x2_4665)) || 
                                           x1_4664 < 0 || x2_4665 < 0 || 
                                           x_2 <= x_4]
                                   -> X))
               ->
               (if (x1_4664 <= 0)
                 (l0
                   (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                     (l1
                       (ysys_1016 0 (x2_4665 - 1)
                         (fun (xs11_4683:bool) (xs12_4684:int) (xs21_4685:bool) 
                              (xs22_4686:x_1:int[(not xs11_4683) || (
                                                 not xs21_4685) || (not (0 <= (x2_4665 - 1))) || 
                                                 0 < 0 || x2_4665 - 1 < 0 || 
                                                 xs12_4684 <= x_1])
                          ->
                          (if xs21_4685
                            (l1
                              (if (1 = x2_4665)
                                (l0
                                  (if (xs22_4686 = xs12_4684)
                                    (l1
                                      (if (p11_4797 <=> xs11_4683)
                                        (l1
                                          (if (xs12_4684 = p12_4798) (
                                            l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                            (l0
                                              (loop_2167 ()
                                                (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int) ->
                                                 (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                                        (l0
                                          (l0
                                            (loop_2167 ()
                                              (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
                                               (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                                    (l0
                                      (loop_2167 ()
                                        (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                                         (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))))
                                (l1
                                  (l1
                                    (if (p11_4797 <=> xs11_4683)
                                      (l1
                                        (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                          (l0
                                            (loop_2167 ()
                                              (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
                                               (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                                      (l0
                                        (l0
                                          (loop_2167 ()
                                            (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
                                             (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))))))))
                            (l0
                              (loop_2167 ()
                                (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                                 (k_insert_4666 x__5064 x__5065 x__5066 x__5067))))))))))
                 (l1
                   (if (x2_4665 <= 0)
                     (l0
                       (ys''ys''_4650 x1_4664 0
                         (fun (p11_4693:bool) (p12_4694:int) (p21_4695:bool) 
                              (p22_4696:x_1:int[(not p11_4693) || (not p21_4695) || (
                                                not (x1_4664 <= 0)) || 
                                                x1_4664 < 0 || 0 < 0 || 
                                                p12_4694 <= x_1])
                          -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                     (l1
                       (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1)
                         (fun (x__5060:bool) (x__5061:int) (x__5062:bool) 
                              (x__5063:x_1:int[(not x__5060) || (not x__5062) || 
                                               (not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                               x1_4664 - 1 < 0 || x2_4665 - 1 < 0 || 
                                               x__5061 <= x_1])
                          -> (k_insert_4666 x__5060 x__5061 x__5062 x__5063))))))))))): X
ETA: (l0
       (k_insert_2260
         (fun x1_4612 x2_4613 k_insert_4614 ->
          (if (x1_4612 <= 0)
            (l0
              (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
                (l1
                  (ysys_1016 0 (x2_4613 - 1)
                    (fun xs11_4631 xs12_4632 xs21_4633 xs22_4634 ->
                     (if xs21_4633
                       (l1
                         (if (1 = x2_4613)
                           (l0
                             (if (xs22_4634 = xs12_4632)
                               (l1
                                 (if (p11_4797 <=> xs11_4631)
                                   (l1
                                     (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                       (l0 (loop_2167 () k_insert_4614)))) (
                                   l0 (l0 (loop_2167 () k_insert_4614))))) (
                               l0 (loop_2167 () k_insert_4614))))
                           (l1
                             (l1
                               (if (p11_4797 <=> xs11_4631)
                                 (l1
                                   (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                     (l0 (loop_2167 () k_insert_4614)))) (
                                 l0 (l0 (loop_2167 () k_insert_4614)))))))) (
                       l0 (loop_2167 () k_insert_4614))))))))
            (l1
              (if (x2_4613 <= 0)
                (l0
                  (ysys_1016 (x1_4612 - 1) 0
                    (fun p11_4641 p12_4642 p21_4643 p22_4644 -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
                (l1 (ysys_1016 (x1_4612 - 1) (x2_4613 - 1) k_insert_4614)))))))): X
ETA: (k_insert_2260
       (fun x1_4612 x2_4613 k_insert_4614 ->
        (if (x1_4612 <= 0)
          (l0
            (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
              (l1
                (ysys_1016 0 (x2_4613 - 1)
                  (fun xs11_4631 xs12_4632 xs21_4633 xs22_4634 ->
                   (if xs21_4633
                     (l1
                       (if (1 = x2_4613)
                         (l0
                           (if (xs22_4634 = xs12_4632)
                             (l1
                               (if (p11_4797 <=> xs11_4631)
                                 (l1
                                   (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                     (l0 (loop_2167 () k_insert_4614)))) (
                                 l0 (l0 (loop_2167 () k_insert_4614))))) (
                             l0 (loop_2167 () k_insert_4614))))
                         (l1
                           (l1
                             (if (p11_4797 <=> xs11_4631)
                               (l1
                                 (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                   (l0 (loop_2167 () k_insert_4614)))) (
                               l0 (l0 (loop_2167 () k_insert_4614)))))))) (
                     l0 (loop_2167 () k_insert_4614))))))))
          (l1
            (if (x2_4613 <= 0)
              (l0
                (ysys_1016 (x1_4612 - 1) 0
                  (fun p11_4641 p12_4642 p21_4643 p22_4644 -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
              (l1 (ysys_1016 (x1_4612 - 1) (x2_4613 - 1) k_insert_4614))))))): X
ETA: (fun x1_4612 x2_4613 k_insert_4614 ->
      (if (x1_4612 <= 0)
        (l0
          (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
            (l1
              (ysys_1016 0 (x2_4613 - 1)
                (fun xs11_4631 xs12_4632 xs21_4633 xs22_4634 ->
                 (if xs21_4633
                   (l1
                     (if (1 = x2_4613)
                       (l0
                         (if (xs22_4634 = xs12_4632)
                           (l1
                             (if (p11_4797 <=> xs11_4631)
                               (l1
                                 (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                   (l0 (loop_2167 () k_insert_4614)))) (
                               l0 (l0 (loop_2167 () k_insert_4614))))) (
                           l0 (loop_2167 () k_insert_4614))))
                       (l1
                         (l1
                           (if (p11_4797 <=> xs11_4631)
                             (l1
                               (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                 (l0 (loop_2167 () k_insert_4614)))) (
                             l0 (l0 (loop_2167 () k_insert_4614)))))))) (
                   l0 (loop_2167 () k_insert_4614))))))))
        (l1
          (if (x2_4613 <= 0)
            (l0
              (ysys_1016 (x1_4612 - 1) 0
                (fun p11_4641 p12_4642 p21_4643 p22_4644 -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
            (l1 (ysys_1016 (x1_4612 - 1) (x2_4613 - 1) k_insert_4614)))))): (
x_1:int ->
x_2:int ->
(x_4:bool ->
 x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
-> X)
ETA: (fun x2_4613 k_insert_4614 ->
      (if (x1_4612 <= 0)
        (l0
          (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
            (l1
              (ysys_1016 0 (x2_4613 - 1)
                (fun xs11_4631 xs12_4632 xs21_4633 xs22_4634 ->
                 (if xs21_4633
                   (l1
                     (if (1 = x2_4613)
                       (l0
                         (if (xs22_4634 = xs12_4632)
                           (l1
                             (if (p11_4797 <=> xs11_4631)
                               (l1
                                 (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                   (l0 (loop_2167 () k_insert_4614)))) (
                               l0 (l0 (loop_2167 () k_insert_4614))))) (
                           l0 (loop_2167 () k_insert_4614))))
                       (l1
                         (l1
                           (if (p11_4797 <=> xs11_4631)
                             (l1
                               (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                 (l0 (loop_2167 () k_insert_4614)))) (
                             l0 (l0 (loop_2167 () k_insert_4614)))))))) (
                   l0 (loop_2167 () k_insert_4614))))))))
        (l1
          (if (x2_4613 <= 0)
            (l0
              (ysys_1016 (x1_4612 - 1) 0
                (fun p11_4641 p12_4642 p21_4643 p22_4644 -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
            (l1 (ysys_1016 (x1_4612 - 1) (x2_4613 - 1) k_insert_4614)))))): (
x_1:int ->
(x_3:bool ->
 x_4:int ->
 x_5:bool -> x_6:int[(not x_3) || (not x_5) || (not (x1_4612 <= x_1)) || x1_4612 < 0 || x_1 < 0 || x_4 <= x_6] -> X)
-> X)
ETA: (fun k_insert_4614 ->
      (if (x1_4612 <= 0)
        (l0
          (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
            (l1
              (ysys_1016 0 (x2_4613 - 1)
                (fun xs11_4631 xs12_4632 xs21_4633 xs22_4634 ->
                 (if xs21_4633
                   (l1
                     (if (1 = x2_4613)
                       (l0
                         (if (xs22_4634 = xs12_4632)
                           (l1
                             (if (p11_4797 <=> xs11_4631)
                               (l1
                                 (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                   (l0 (loop_2167 () k_insert_4614)))) (
                               l0 (l0 (loop_2167 () k_insert_4614))))) (
                           l0 (loop_2167 () k_insert_4614))))
                       (l1
                         (l1
                           (if (p11_4797 <=> xs11_4631)
                             (l1
                               (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                 (l0 (loop_2167 () k_insert_4614)))) (
                             l0 (l0 (loop_2167 () k_insert_4614)))))))) (
                   l0 (loop_2167 () k_insert_4614))))))))
        (l1
          (if (x2_4613 <= 0)
            (l0
              (ysys_1016 (x1_4612 - 1) 0
                (fun p11_4641 p12_4642 p21_4643 p22_4644 -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
            (l1 (ysys_1016 (x1_4612 - 1) (x2_4613 - 1) k_insert_4614)))))): ((
x_2:bool ->
x_3:int ->
x_4:bool -> x_5:int[(not x_2) || (not x_4) || (not (x1_4612 <= x2_4613)) || x1_4612 < 0 || x2_4613 < 0 || x_3 <= x_5]
-> X) ->
X)
ETA: (if (x1_4612 <= 0)
       (l0
         (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
           (l1
             (ysys_1016 0 (x2_4613 - 1)
               (fun xs11_4631 xs12_4632 xs21_4633 xs22_4634 ->
                (if xs21_4633
                  (l1
                    (if (1 = x2_4613)
                      (l0
                        (if (xs22_4634 = xs12_4632)
                          (l1
                            (if (p11_4797 <=> xs11_4631)
                              (l1
                                (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                  (l0 (loop_2167 () k_insert_4614)))) (
                              l0 (l0 (loop_2167 () k_insert_4614))))) (
                          l0 (loop_2167 () k_insert_4614))))
                      (l1
                        (l1
                          (if (p11_4797 <=> xs11_4631)
                            (l1
                              (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                (l0 (loop_2167 () k_insert_4614)))) (
                            l0 (l0 (loop_2167 () k_insert_4614)))))))) (
                  l0 (loop_2167 () k_insert_4614))))))))
       (l1
         (if (x2_4613 <= 0)
           (l0
             (ysys_1016 (x1_4612 - 1) 0
               (fun p11_4641 p12_4642 p21_4643 p22_4644 -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
           (l1 (ysys_1016 (x1_4612 - 1) (x2_4613 - 1) k_insert_4614))))): X
ETA: (l1
       (if (x2_4613 <= 0)
         (l0
           (ysys_1016 (x1_4612 - 1) 0
             (fun p11_4641 p12_4642 p21_4643 p22_4644 -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
         (l1 (ysys_1016 (x1_4612 - 1) (x2_4613 - 1) k_insert_4614)))): X
ETA: (if (x2_4613 <= 0)
       (l0
         (ysys_1016 (x1_4612 - 1) 0
           (fun p11_4641 p12_4642 p21_4643 p22_4644 -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
       (l1 (ysys_1016 (x1_4612 - 1) (x2_4613 - 1) k_insert_4614))): X
ETA: (l1 (ysys_1016 (x1_4612 - 1) (x2_4613 - 1) k_insert_4614)): X
ETA: (ysys_1016 (x1_4612 - 1) (x2_4613 - 1) k_insert_4614): X
ETA: k_insert_4614: (x_1:bool ->
                     x_2:int ->
                     x_3:bool ->
                     x_4:int[(not x_1) || (not x_3) || (not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                             x1_4612 - 1 < 0 || x2_4613 - 1 < 0 || x_2 <= x_4]
                     -> X)
ETA_AUX: k_insert_4614: (x_1:bool ->
                         x_2:int ->
                         x_3:bool ->
                         x_4:int[(not x_1) || (not x_3) || (not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                                 x1_4612 - 1 < 0 || x2_4613 - 1 < 0 || 
                                 x_2 <= x_4]
                         -> X)
ETA_AUX: x__5092: bool
ETA_AUX: (k_insert_4614 x__5092): (x_1:int ->
                                   x_2:bool ->
                                   x_3:int[(not x__5092) || (not x_2) || (
                                           not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                                           x1_4612 - 1 < 0 || x2_4613 - 1 < 0 || 
                                           x_1 <= x_3]
                                   -> X)
ETA_AUX: x__5093: int
ETA_AUX: (k_insert_4614 x__5092 x__5093): (x_1:bool ->
                                           x_2:int[(not x__5092) || (
                                                   not x_1) || (not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                                                   x1_4612 - 1 < 0 || 
                                                   x2_4613 - 1 < 0 || 
                                                   x__5093 <= x_2]
                                           -> X)
ETA_AUX: x__5094: bool
ETA_AUX: (k_insert_4614 x__5092 x__5093 x__5094): (x_1:int[(not x__5092) || (
                                                           not x__5094) || (
                                                           not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                                                           x1_4612 - 1 < 0 || 
                                                           x2_4613 - 1 < 0 || 
                                                           x__5093 <= x_1] ->
X)
ETA_AUX: x__5095: x_1:int[(not x__5092) || (not x__5094) || (not (x1_4612 <= x2_4613)) || 
                          x1_4612 < 0 || x2_4613 < 0 || x__5093 <= x_1]
ETA_AUX: (k_insert_4614 x__5092 x__5093 x__5094 x__5095): X
ETA: (x2_4613 - 1): int
ETA: (x1_4612 - 1): int
ETA_AUX: (ysys_1016 (x1_4612 - 1) (x2_4613 - 1)
           (fun (x__5092:bool) (x__5093:int) (x__5094:bool) 
                (x__5095:x_1:int[(not x__5092) || (not x__5094) || (not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                                 x1_4612 - 1 < 0 || x2_4613 - 1 < 0 || 
                                 x__5093 <= x_1])
            -> (k_insert_4614 x__5092 x__5093 x__5094 x__5095))): X
ETA: (l0
       (ysys_1016 (x1_4612 - 1) 0
         (fun p11_4641 p12_4642 p21_4643 p22_4644 -> (k_insert_4614 p11_4641 p12_4642 true x_1053)))): X
ETA: (ysys_1016 (x1_4612 - 1) 0
       (fun p11_4641 p12_4642 p21_4643 p22_4644 -> (k_insert_4614 p11_4641 p12_4642 true x_1053))): X
ETA: (fun p11_4641 p12_4642 p21_4643 p22_4644 -> (k_insert_4614 p11_4641 p12_4642 true x_1053)): (
x_1:bool ->
x_2:int ->
x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not ((x1_4612 - 1) <= 0)) || x1_4612 - 1 < 0 || 0 < 0 || x_2 <= x_4] ->
X)
ETA: (fun p12_4642 p21_4643 p22_4644 -> (k_insert_4614 p11_4641 p12_4642 true x_1053)): (
x_1:int ->
x_2:bool ->
x_3:int[(not p11_4641) || (not x_2) || (not ((x1_4612 - 1) <= 0)) || x1_4612 - 1 < 0 || 0 < 0 || x_1 <= x_3] -> X)
ETA: (fun p21_4643 p22_4644 -> (k_insert_4614 p11_4641 p12_4642 true x_1053)): (
x_1:bool ->
x_2:int[(not p11_4641) || (not x_1) || (not ((x1_4612 - 1) <= 0)) || x1_4612 - 1 < 0 || 0 < 0 || p12_4642 <= x_2] -> X)
ETA: (fun p22_4644 -> (k_insert_4614 p11_4641 p12_4642 true x_1053)): (x_1:int[
(not p11_4641) || (not p21_4643) || (not ((x1_4612 - 1) <= 0)) || x1_4612 - 1 < 0 || 0 < 0 || p12_4642 <= x_1] ->
X)
ETA: (k_insert_4614 p11_4641 p12_4642 true x_1053): X
ETA: x_1053: x_1:int[(not p11_4641) || (not true) || (not (x1_4612 <= x2_4613)) || 
                     x1_4612 < 0 || x2_4613 < 0 || p12_4642 <= x_1]
ETA: true: bool
ETA: p12_4642: int
ETA: p11_4641: bool
ETA_AUX: (k_insert_4614 p11_4641 p12_4642 true x_1053): X
ETA: 0: int
ETA: (x1_4612 - 1): int
ETA_AUX: (ysys_1016 (x1_4612 - 1) 0
           (fun (p11_4641:bool) (p12_4642:int) (p21_4643:bool) 
                (p22_4644:x_1:int[(not p11_4641) || (not p21_4643) || (
                                  not ((x1_4612 - 1) <= 0)) || x1_4612 - 1 < 0 || 
                                  0 < 0 || p12_4642 <= x_1])
            -> (k_insert_4614 p11_4641 p12_4642 true x_1053))): X
ETA: (l0
       (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
         (l1
           (ysys_1016 0 (x2_4613 - 1)
             (fun xs11_4631 xs12_4632 xs21_4633 xs22_4634 ->
              (if xs21_4633
                (l1
                  (if (1 = x2_4613)
                    (l0
                      (if (xs22_4634 = xs12_4632)
                        (l1
                          (if (p11_4797 <=> xs11_4631)
                            (l1
                              (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                (l0 (loop_2167 () k_insert_4614)))) (
                            l0 (l0 (loop_2167 () k_insert_4614))))) (
                        l0 (loop_2167 () k_insert_4614))))
                    (l1
                      (l1
                        (if (p11_4797 <=> xs11_4631)
                          (l1
                            (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                              (l0 (loop_2167 () k_insert_4614)))) (l0 (l0 (loop_2167 () k_insert_4614))))))))
                (l0 (loop_2167 () k_insert_4614)))))))): X
ETA: (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
       (l1
         (ysys_1016 0 (x2_4613 - 1)
           (fun xs11_4631 xs12_4632 xs21_4633 xs22_4634 ->
            (if xs21_4633
              (l1
                (if (1 = x2_4613)
                  (l0
                    (if (xs22_4634 = xs12_4632)
                      (l1
                        (if (p11_4797 <=> xs11_4631)
                          (l1
                            (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                              (l0 (loop_2167 () k_insert_4614)))) (l0 (l0 (loop_2167 () k_insert_4614)))))
                      (l0 (loop_2167 () k_insert_4614))))
                  (l1
                    (l1
                      (if (p11_4797 <=> xs11_4631)
                        (l1
                          (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                            (l0 (loop_2167 () k_insert_4614)))) (l0 (l0 (loop_2167 () k_insert_4614))))))))
              (l0 (loop_2167 () k_insert_4614))))))): X
ETA: (l1
       (ysys_1016 0 (x2_4613 - 1)
         (fun xs11_4631 xs12_4632 xs21_4633 xs22_4634 ->
          (if xs21_4633
            (l1
              (if (1 = x2_4613)
                (l0
                  (if (xs22_4634 = xs12_4632)
                    (l1
                      (if (p11_4797 <=> xs11_4631)
                        (l1
                          (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                            (l0 (loop_2167 () k_insert_4614)))) (l0 (l0 (loop_2167 () k_insert_4614)))))
                    (l0 (loop_2167 () k_insert_4614))))
                (l1
                  (l1
                    (if (p11_4797 <=> xs11_4631)
                      (l1
                        (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                          (l0 (loop_2167 () k_insert_4614)))) (l0 (l0 (loop_2167 () k_insert_4614))))))))
            (l0 (loop_2167 () k_insert_4614)))))): X
ETA: (ysys_1016 0 (x2_4613 - 1)
       (fun xs11_4631 xs12_4632 xs21_4633 xs22_4634 ->
        (if xs21_4633
          (l1
            (if (1 = x2_4613)
              (l0
                (if (xs22_4634 = xs12_4632)
                  (l1
                    (if (p11_4797 <=> xs11_4631)
                      (l1
                        (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                          (l0 (loop_2167 () k_insert_4614)))) (l0 (l0 (loop_2167 () k_insert_4614)))))
                  (l0 (loop_2167 () k_insert_4614))))
              (l1
                (l1
                  (if (p11_4797 <=> xs11_4631)
                    (l1
                      (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                        (l0 (loop_2167 () k_insert_4614)))) (l0 (l0 (loop_2167 () k_insert_4614))))))))
          (l0 (loop_2167 () k_insert_4614))))): X
ETA: (fun xs11_4631 xs12_4632 xs21_4633 xs22_4634 ->
      (if xs21_4633
        (l1
          (if (1 = x2_4613)
            (l0
              (if (xs22_4634 = xs12_4632)
                (l1
                  (if (p11_4797 <=> xs11_4631)
                    (l1
                      (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                        (l0 (loop_2167 () k_insert_4614)))) (l0 (l0 (loop_2167 () k_insert_4614)))))
                (l0 (loop_2167 () k_insert_4614))))
            (l1
              (l1
                (if (p11_4797 <=> xs11_4631)
                  (l1
                    (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                      (l0 (loop_2167 () k_insert_4614)))) (l0 (l0 (loop_2167 () k_insert_4614))))))))
        (l0 (loop_2167 () k_insert_4614)))): (x_1:bool ->
                                              x_2:int ->
                                              x_3:bool ->
                                              x_4:int[(not x_1) || (not x_3) || (
                                                      not (0 <= (x2_4613 - 1))) || 
                                                      0 < 0 || x2_4613 - 1 < 0 || 
                                                      x_2 <= x_4]
                                              -> X)
ETA: (fun xs12_4632 xs21_4633 xs22_4634 ->
      (if xs21_4633
        (l1
          (if (1 = x2_4613)
            (l0
              (if (xs22_4634 = xs12_4632)
                (l1
                  (if (p11_4797 <=> xs11_4631)
                    (l1
                      (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                        (l0 (loop_2167 () k_insert_4614)))) (l0 (l0 (loop_2167 () k_insert_4614)))))
                (l0 (loop_2167 () k_insert_4614))))
            (l1
              (l1
                (if (p11_4797 <=> xs11_4631)
                  (l1
                    (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                      (l0 (loop_2167 () k_insert_4614)))) (l0 (l0 (loop_2167 () k_insert_4614))))))))
        (l0 (loop_2167 () k_insert_4614)))): (x_1:int ->
                                              x_2:bool ->
                                              x_3:int[(not xs11_4631) || (
                                                      not x_2) || (not (0 <= (x2_4613 - 1))) || 
                                                      0 < 0 || x2_4613 - 1 < 0 || 
                                                      x_1 <= x_3]
                                              -> X)
ETA: (fun xs21_4633 xs22_4634 ->
      (if xs21_4633
        (l1
          (if (1 = x2_4613)
            (l0
              (if (xs22_4634 = xs12_4632)
                (l1
                  (if (p11_4797 <=> xs11_4631)
                    (l1
                      (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                        (l0 (loop_2167 () k_insert_4614)))) (l0 (l0 (loop_2167 () k_insert_4614)))))
                (l0 (loop_2167 () k_insert_4614))))
            (l1
              (l1
                (if (p11_4797 <=> xs11_4631)
                  (l1
                    (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                      (l0 (loop_2167 () k_insert_4614)))) (l0 (l0 (loop_2167 () k_insert_4614))))))))
        (l0 (loop_2167 () k_insert_4614)))): (x_1:bool ->
                                              x_2:int[(not xs11_4631) || (
                                                      not x_1) || (not (0 <= (x2_4613 - 1))) || 
                                                      0 < 0 || x2_4613 - 1 < 0 || 
                                                      xs12_4632 <= x_2]
                                              -> X)
ETA: (fun xs22_4634 ->
      (if xs21_4633
        (l1
          (if (1 = x2_4613)
            (l0
              (if (xs22_4634 = xs12_4632)
                (l1
                  (if (p11_4797 <=> xs11_4631)
                    (l1
                      (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                        (l0 (loop_2167 () k_insert_4614)))) (l0 (l0 (loop_2167 () k_insert_4614)))))
                (l0 (loop_2167 () k_insert_4614))))
            (l1
              (l1
                (if (p11_4797 <=> xs11_4631)
                  (l1
                    (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                      (l0 (loop_2167 () k_insert_4614)))) (l0 (l0 (loop_2167 () k_insert_4614))))))))
        (l0 (loop_2167 () k_insert_4614)))): (x_1:int[(not xs11_4631) || (
                                                      not xs21_4633) || (
                                                      not (0 <= (x2_4613 - 1))) || 
                                                      0 < 0 || x2_4613 - 1 < 0 || 
                                                      xs12_4632 <= x_1] ->
X)
ETA: (if xs21_4633
       (l1
         (if (1 = x2_4613)
           (l0
             (if (xs22_4634 = xs12_4632)
               (l1
                 (if (p11_4797 <=> xs11_4631)
                   (l1
                     (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                       (l0 (loop_2167 () k_insert_4614)))) (l0 (l0 (loop_2167 () k_insert_4614)))))
               (l0 (loop_2167 () k_insert_4614))))
           (l1
             (l1
               (if (p11_4797 <=> xs11_4631)
                 (l1
                   (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                     (l0 (loop_2167 () k_insert_4614)))) (l0 (l0 (loop_2167 () k_insert_4614))))))))
       (l0 (loop_2167 () k_insert_4614))): X
ETA: (l0 (loop_2167 () k_insert_4614)): X
ETA: (loop_2167 () k_insert_4614): X
ETA: k_insert_4614: (bool -> int -> bool -> int -> X)
ETA_AUX: k_insert_4614: (bool -> int -> bool -> int -> X)
ETA_AUX: x__5096: bool
ETA_AUX: (k_insert_4614 x__5096): (int -> bool -> int -> X)
ETA_AUX: x__5097: int
ETA_AUX: (k_insert_4614 x__5096 x__5097): (bool -> int -> X)
ETA_AUX: x__5098: bool
ETA_AUX: (k_insert_4614 x__5096 x__5097 x__5098): (int ->
X)
ETA_AUX: x__5099: x_1:int[(not x__5096) || (not x__5098) || (not (x1_4612 <= x2_4613)) || 
                          x1_4612 < 0 || x2_4613 < 0 || x__5097 <= x_1]
ETA_AUX: (k_insert_4614 x__5096 x__5097 x__5098 x__5099): X
ETA: (): unit
ETA_AUX: (loop_2167 ()
           (fun (x__5096:bool) (x__5097:int) (x__5098:bool) (x__5099:int) ->
            (k_insert_4614 x__5096 x__5097 x__5098 x__5099))): X
ETA: (l1
       (if (1 = x2_4613)
         (l0
           (if (xs22_4634 = xs12_4632)
             (l1
               (if (p11_4797 <=> xs11_4631)
                 (l1
                   (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                     (l0 (loop_2167 () k_insert_4614)))) (l0 (l0 (loop_2167 () k_insert_4614)))))
             (l0 (loop_2167 () k_insert_4614))))
         (l1
           (l1
             (if (p11_4797 <=> xs11_4631)
               (l1
                 (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                   (l0 (loop_2167 () k_insert_4614)))) (l0 (l0 (loop_2167 () k_insert_4614)))))))): X
ETA: (if (1 = x2_4613)
       (l0
         (if (xs22_4634 = xs12_4632)
           (l1
             (if (p11_4797 <=> xs11_4631)
               (l1
                 (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                   (l0 (loop_2167 () k_insert_4614)))) (l0 (l0 (loop_2167 () k_insert_4614)))))
           (l0 (loop_2167 () k_insert_4614))))
       (l1
         (l1
           (if (p11_4797 <=> xs11_4631)
             (l1
               (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                 (l0 (loop_2167 () k_insert_4614)))) (l0 (l0 (loop_2167 () k_insert_4614))))))): X
ETA: (l1
       (l1
         (if (p11_4797 <=> xs11_4631)
           (l1
             (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
               (l0 (loop_2167 () k_insert_4614)))) (l0 (l0 (loop_2167 () k_insert_4614)))))): X
ETA: (l1
       (if (p11_4797 <=> xs11_4631)
         (l1
           (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634)) (l0 (loop_2167 () k_insert_4614))))
         (l0 (l0 (loop_2167 () k_insert_4614))))): X
ETA: (if (p11_4797 <=> xs11_4631)
       (l1
         (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634)) (l0 (loop_2167 () k_insert_4614))))
       (l0 (l0 (loop_2167 () k_insert_4614)))): X
ETA: (l0 (l0 (loop_2167 () k_insert_4614))): X
ETA: (l0 (loop_2167 () k_insert_4614)): X
ETA: (loop_2167 () k_insert_4614): X
ETA: k_insert_4614: (bool -> int -> bool -> int -> X)
ETA_AUX: k_insert_4614: (bool -> int -> bool -> int -> X)
ETA_AUX: x__5100: bool
ETA_AUX: (k_insert_4614 x__5100): (int -> bool -> int -> X)
ETA_AUX: x__5101: int
ETA_AUX: (k_insert_4614 x__5100 x__5101): (bool -> int -> X)
ETA_AUX: x__5102: bool
ETA_AUX: (k_insert_4614 x__5100 x__5101 x__5102): (int ->
X)
ETA_AUX: x__5103: x_1:int[(not x__5100) || (not x__5102) || (not (x1_4612 <= x2_4613)) || 
                          x1_4612 < 0 || x2_4613 < 0 || x__5101 <= x_1]
ETA_AUX: (k_insert_4614 x__5100 x__5101 x__5102 x__5103): X
ETA: (): unit
ETA_AUX: (loop_2167 ()
           (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int) ->
            (k_insert_4614 x__5100 x__5101 x__5102 x__5103))): X
ETA: (l1 (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634)) (l0 (loop_2167 () k_insert_4614)))): X
ETA: (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634)) (l0 (loop_2167 () k_insert_4614))): X
ETA: (l0 (loop_2167 () k_insert_4614)): X
ETA: (loop_2167 () k_insert_4614): X
ETA: k_insert_4614: (bool -> int -> bool -> int -> X)
ETA_AUX: k_insert_4614: (bool -> int -> bool -> int -> X)
ETA_AUX: x__5104: bool
ETA_AUX: (k_insert_4614 x__5104): (int -> bool -> int -> X)
ETA_AUX: x__5105: int
ETA_AUX: (k_insert_4614 x__5104 x__5105): (bool -> int -> X)
ETA_AUX: x__5106: bool
ETA_AUX: (k_insert_4614 x__5104 x__5105 x__5106): (int ->
X)
ETA_AUX: x__5107: x_1:int[(not x__5104) || (not x__5106) || (not (x1_4612 <= x2_4613)) || 
                          x1_4612 < 0 || x2_4613 < 0 || x__5105 <= x_1]
ETA_AUX: (k_insert_4614 x__5104 x__5105 x__5106 x__5107): X
ETA: (): unit
ETA_AUX: (loop_2167 ()
           (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int) ->
            (k_insert_4614 x__5104 x__5105 x__5106 x__5107))): X
ETA: (l1 (k_insert_4614 true x_1053 true xs22_4634)): X
ETA: (k_insert_4614 true x_1053 true xs22_4634): X
ETA: xs22_4634: x_1:int[(not true) || (not true) || (not (x1_4612 <= x2_4613)) || 
                        x1_4612 < 0 || x2_4613 < 0 || x_1053 <= x_1]
ETA: true: bool
ETA: x_1053: int
ETA: true: bool
ETA_AUX: (k_insert_4614 true x_1053 true xs22_4634): X
ETA: (l0
       (if (xs22_4634 = xs12_4632)
         (l1
           (if (p11_4797 <=> xs11_4631)
             (l1
               (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                 (l0 (loop_2167 () k_insert_4614)))) (l0 (l0 (loop_2167 () k_insert_4614)))))
         (l0 (loop_2167 () k_insert_4614)))): X
ETA: (if (xs22_4634 = xs12_4632)
       (l1
         (if (p11_4797 <=> xs11_4631)
           (l1
             (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
               (l0 (loop_2167 () k_insert_4614)))) (l0 (l0 (loop_2167 () k_insert_4614)))))
       (l0 (loop_2167 () k_insert_4614))): X
ETA: (l0 (loop_2167 () k_insert_4614)): X
ETA: (loop_2167 () k_insert_4614): X
ETA: k_insert_4614: (bool -> int -> bool -> int -> X)
ETA_AUX: k_insert_4614: (bool -> int -> bool -> int -> X)
ETA_AUX: x__5108: bool
ETA_AUX: (k_insert_4614 x__5108): (int -> bool -> int -> X)
ETA_AUX: x__5109: int
ETA_AUX: (k_insert_4614 x__5108 x__5109): (bool -> int -> X)
ETA_AUX: x__5110: bool
ETA_AUX: (k_insert_4614 x__5108 x__5109 x__5110): (int ->
X)
ETA_AUX: x__5111: x_1:int[(not x__5108) || (not x__5110) || (not (x1_4612 <= x2_4613)) || 
                          x1_4612 < 0 || x2_4613 < 0 || x__5109 <= x_1]
ETA_AUX: (k_insert_4614 x__5108 x__5109 x__5110 x__5111): X
ETA: (): unit
ETA_AUX: (loop_2167 ()
           (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
            (k_insert_4614 x__5108 x__5109 x__5110 x__5111))): X
ETA: (l1
       (if (p11_4797 <=> xs11_4631)
         (l1
           (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634)) (l0 (loop_2167 () k_insert_4614))))
         (l0 (l0 (loop_2167 () k_insert_4614))))): X
ETA: (if (p11_4797 <=> xs11_4631)
       (l1
         (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634)) (l0 (loop_2167 () k_insert_4614))))
       (l0 (l0 (loop_2167 () k_insert_4614)))): X
ETA: (l0 (l0 (loop_2167 () k_insert_4614))): X
ETA: (l0 (loop_2167 () k_insert_4614)): X
ETA: (loop_2167 () k_insert_4614): X
ETA: k_insert_4614: (bool -> int -> bool -> int -> X)
ETA_AUX: k_insert_4614: (bool -> int -> bool -> int -> X)
ETA_AUX: x__5112: bool
ETA_AUX: (k_insert_4614 x__5112): (int -> bool -> int -> X)
ETA_AUX: x__5113: int
ETA_AUX: (k_insert_4614 x__5112 x__5113): (bool -> int -> X)
ETA_AUX: x__5114: bool
ETA_AUX: (k_insert_4614 x__5112 x__5113 x__5114): (int ->
X)
ETA_AUX: x__5115: x_1:int[(not x__5112) || (not x__5114) || (not (x1_4612 <= x2_4613)) || 
                          x1_4612 < 0 || x2_4613 < 0 || x__5113 <= x_1]
ETA_AUX: (k_insert_4614 x__5112 x__5113 x__5114 x__5115): X
ETA: (): unit
ETA_AUX: (loop_2167 ()
           (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int) ->
            (k_insert_4614 x__5112 x__5113 x__5114 x__5115))): X
ETA: (l1 (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634)) (l0 (loop_2167 () k_insert_4614)))): X
ETA: (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634)) (l0 (loop_2167 () k_insert_4614))): X
ETA: (l0 (loop_2167 () k_insert_4614)): X
ETA: (loop_2167 () k_insert_4614): X
ETA: k_insert_4614: (bool -> int -> bool -> int -> X)
ETA_AUX: k_insert_4614: (bool -> int -> bool -> int -> X)
ETA_AUX: x__5116: bool
ETA_AUX: (k_insert_4614 x__5116): (int -> bool -> int -> X)
ETA_AUX: x__5117: int
ETA_AUX: (k_insert_4614 x__5116 x__5117): (bool -> int -> X)
ETA_AUX: x__5118: bool
ETA_AUX: (k_insert_4614 x__5116 x__5117 x__5118): (int ->
X)
ETA_AUX: x__5119: x_1:int[(not x__5116) || (not x__5118) || (not (x1_4612 <= x2_4613)) || 
                          x1_4612 < 0 || x2_4613 < 0 || x__5117 <= x_1]
ETA_AUX: (k_insert_4614 x__5116 x__5117 x__5118 x__5119): X
ETA: (): unit
ETA_AUX: (loop_2167 ()
           (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int) ->
            (k_insert_4614 x__5116 x__5117 x__5118 x__5119))): X
ETA: (l1 (k_insert_4614 true x_1053 true xs22_4634)): X
ETA: (k_insert_4614 true x_1053 true xs22_4634): X
ETA: xs22_4634: x_1:int[(not true) || (not true) || (not (x1_4612 <= x2_4613)) || 
                        x1_4612 < 0 || x2_4613 < 0 || x_1053 <= x_1]
ETA: true: bool
ETA: x_1053: int
ETA: true: bool
ETA_AUX: (k_insert_4614 true x_1053 true xs22_4634): X
ETA: (x2_4613 - 1): int
ETA: 0: int
ETA_AUX: (ysys_1016 0 (x2_4613 - 1)
           (fun (xs11_4631:bool) (xs12_4632:int) (xs21_4633:bool) 
                (xs22_4634:x_1:int[(not xs11_4631) || (not xs21_4633) || (
                                   not (0 <= (x2_4613 - 1))) || 0 < 0 || 
                                   x2_4613 - 1 < 0 || xs12_4632 <= x_1])
            ->
            (if xs21_4633
              (l1
                (if (1 = x2_4613)
                  (l0
                    (if (xs22_4634 = xs12_4632)
                      (l1
                        (if (p11_4797 <=> xs11_4631)
                          (l1
                            (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                              (l0
                                (loop_2167 ()
                                  (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int) ->
                                   (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                          (l0
                            (l0
                              (loop_2167 ()
                                (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int) ->
                                 (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))))))
                      (l0
                        (loop_2167 ()
                          (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                           (k_insert_4614 x__5108 x__5109 x__5110 x__5111))))))
                  (l1
                    (l1
                      (if (p11_4797 <=> xs11_4631)
                        (l1
                          (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                            (l0
                              (loop_2167 ()
                                (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int) ->
                                 (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))))
                        (l0
                          (l0
                            (loop_2167 ()
                              (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int) ->
                               (k_insert_4614 x__5100 x__5101 x__5102 x__5103))))))))))
              (l0
                (loop_2167 ()
                  (fun (x__5096:bool) (x__5097:int) (x__5098:bool) (x__5099:int) ->
                   (k_insert_4614 x__5096 x__5097 x__5098 x__5099))))))): X
ETA: (l0 (k_insert_4614 true x_1053 true x_1053)): X
ETA: (k_insert_4614 true x_1053 true x_1053): X
ETA: x_1053: x_1:int[(not true) || (not true) || (not (x1_4612 <= x2_4613)) || 
                     x1_4612 < 0 || x2_4613 < 0 || x_1053 <= x_1]
ETA: true: bool
ETA: x_1053: int
ETA: true: bool
ETA_AUX: (k_insert_4614 true x_1053 true x_1053): X
ETA_AUX: (k_insert_2260
           (fun (x1_4612:int) (x2_4613:int) 
                (k_insert_4614:(x_1:bool ->
                                x_2:int ->
                                x_3:bool ->
                                x_4:int[(not x_1) || (not x_3) || (not (x1_4612 <= x2_4613)) || 
                                        x1_4612 < 0 || x2_4613 < 0 || 
                                        x_2 <= x_4]
                                -> X))
            ->
            (if (x1_4612 <= 0)
              (l0
                (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
                  (l1
                    (ysys_1016 0 (x2_4613 - 1)
                      (fun (xs11_4631:bool) (xs12_4632:int) (xs21_4633:bool) 
                           (xs22_4634:x_1:int[(not xs11_4631) || (not xs21_4633) || (
                                              not (0 <= (x2_4613 - 1))) || 
                                              0 < 0 || x2_4613 - 1 < 0 || 
                                              xs12_4632 <= x_1])
                       ->
                       (if xs21_4633
                         (l1
                           (if (1 = x2_4613)
                             (l0
                               (if (xs22_4634 = xs12_4632)
                                 (l1
                                   (if (p11_4797 <=> xs11_4631)
                                     (l1
                                       (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                         (l0
                                           (loop_2167 ()
                                             (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int) ->
                                              (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                                     (l0
                                       (l0
                                         (loop_2167 ()
                                           (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int) ->
                                            (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))))))
                                 (l0
                                   (loop_2167 ()
                                     (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                                      (k_insert_4614 x__5108 x__5109 x__5110 x__5111))))))
                             (l1
                               (l1
                                 (if (p11_4797 <=> xs11_4631)
                                   (l1
                                     (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                       (l0
                                         (loop_2167 ()
                                           (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int) ->
                                            (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))))
                                   (l0
                                     (l0
                                       (loop_2167 ()
                                         (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int) ->
                                          (k_insert_4614 x__5100 x__5101 x__5102 x__5103))))))))))
                         (l0
                           (loop_2167 ()
                             (fun (x__5096:bool) (x__5097:int) (x__5098:bool) (x__5099:int) ->
                              (k_insert_4614 x__5096 x__5097 x__5098 x__5099))))))))))
              (l1
                (if (x2_4613 <= 0)
                  (l0
                    (ysys_1016 (x1_4612 - 1) 0
                      (fun (p11_4641:bool) (p12_4642:int) (p21_4643:bool) 
                           (p22_4644:x_1:int[(not p11_4641) || (not p21_4643) || (
                                             not ((x1_4612 - 1) <= 0)) || 
                                             x1_4612 - 1 < 0 || 0 < 0 || 
                                             p12_4642 <= x_1])
                       -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
                  (l1
                    (ysys_1016 (x1_4612 - 1) (x2_4613 - 1)
                      (fun (x__5092:bool) (x__5093:int) (x__5094:bool) 
                           (x__5095:x_1:int[(not x__5092) || (not x__5094) || 
                                            (not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                                            x1_4612 - 1 < 0 || x2_4613 - 1 < 0 || 
                                            x__5093 <= x_1])
                       -> (k_insert_4614 x__5092 x__5093 x__5094 x__5095))))))))): X
ETA: 0: int
ETA: 0: int
ETA_AUX: (ysys_1016 0 0
           (fun (p11_4797:bool) (p12_4798:int) (p21_4799:bool) 
                (p22_4800:x_1:int[(not p11_4797) || (not p21_4799) || (
                                  not (0 <= 0)) || 0 < 0 || 0 < 0 || 
                                  p12_4798 <= x_1])
            ->
            (if p11_4797
              (l1
                (if (x_1053 < p12_4798)
                  (l0
                    (k_insert_2260
                      (fun (x1_4612:int) (x2_4613:int) 
                           (k_insert_4614:(x_1:bool ->
                                           x_2:int ->
                                           x_3:bool ->
                                           x_4:int[(not x_1) || (not x_3) || (
                                                   not (x1_4612 <= x2_4613)) || 
                                                   x1_4612 < 0 || x2_4613 < 0 || 
                                                   x_2 <= x_4]
                                           -> X))
                       ->
                       (if (x1_4612 <= 0)
                         (l0
                           (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
                             (l1
                               (ysys_1016 0 (x2_4613 - 1)
                                 (fun (xs11_4631:bool) (xs12_4632:int) (xs21_4633:bool) 
                                      (xs22_4634:x_1:int[(not xs11_4631) || (
                                                         not xs21_4633) || (
                                                         not (0 <= (x2_4613 - 1))) || 
                                                         0 < 0 || x2_4613 - 1 < 0 || 
                                                         xs12_4632 <= x_1])
                                  ->
                                  (if xs21_4633
                                    (l1
                                      (if (1 = x2_4613)
                                        (l0
                                          (if (xs22_4634 = xs12_4632)
                                            (l1
                                              (if (p11_4797 <=> xs11_4631)
                                                (l1
                                                  (if (xs12_4632 = p12_4798)
                                                    (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                                    (l0
                                                      (loop_2167 ()
                                                        (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int)
                                                         -> (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                                                (l0
                                                  (l0
                                                    (loop_2167 ()
                                                      (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int) ->
                                                       (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))))))
                                            (l0
                                              (loop_2167 ()
                                                (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                                                 (k_insert_4614 x__5108 x__5109 x__5110 x__5111))))))
                                        (l1
                                          (l1
                                            (if (p11_4797 <=> xs11_4631)
                                              (l1
                                                (if (xs12_4632 = p12_4798)
                                                  (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                                  (l0
                                                    (loop_2167 ()
                                                      (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int) ->
                                                       (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))))
                                              (l0
                                                (l0
                                                  (loop_2167 ()
                                                    (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int) ->
                                                     (k_insert_4614 x__5100 x__5101 x__5102 x__5103))))))))))
                                    (l0
                                      (loop_2167 ()
                                        (fun (x__5096:bool) (x__5097:int) (x__5098:bool) (x__5099:int) ->
                                         (k_insert_4614 x__5096 x__5097 x__5098 x__5099))))))))))
                         (l1
                           (if (x2_4613 <= 0)
                             (l0
                               (ysys_1016 (x1_4612 - 1) 0
                                 (fun (p11_4641:bool) (p12_4642:int) (p21_4643:bool) 
                                      (p22_4644:x_1:int[(not p11_4641) || (
                                                        not p21_4643) || (
                                                        not ((x1_4612 - 1) <= 0)) || 
                                                        x1_4612 - 1 < 0 || 
                                                        0 < 0 || p12_4642 <= x_1])
                                  -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
                             (l1
                               (ysys_1016 (x1_4612 - 1) (x2_4613 - 1)
                                 (fun (x__5092:bool) (x__5093:int) (x__5094:bool) 
                                      (x__5095:x_1:int[(not x__5092) || (
                                                       not x__5094) || (
                                                       not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                                                       x1_4612 - 1 < 0 || 
                                                       x2_4613 - 1 < 0 || 
                                                       x__5093 <= x_1])
                                  -> (k_insert_4614 x__5092 x__5093 x__5094 x__5095))))))))))
                  (l1
                    (insert_1014 x_1053
                      (fun (x1_5042:int) (x2_5043:int) 
                           (k_insert_ys'ys'_5044:(x_1:bool ->
                                                  x_2:int ->
                                                  x_3:bool ->
                                                  x_4:int[(not x_1) || (
                                                          not x_3) || (
                                                          not (x1_5042 <= x2_5043)) || 
                                                          x1_5042 < 0 || 
                                                          x2_5043 < 0 || 
                                                          x_2 <= x_4]
                                                  -> X))
                       ->
                       (ysys_1016 (x1_5042 + 1) (x2_5043 + 1)
                         (fun (x__5088:bool) (x__5089:int) (x__5090:bool) 
                              (x__5091:x_1:int[(not x__5088) || (not x__5090) || 
                                               (not ((x1_5042 + 1) <= (x2_5043 + 1))) || 
                                               x1_5042 + 1 < 0 || x2_5043 + 1 < 0 || 
                                               x__5089 <= x_1])
                          -> (k_insert_ys'ys'_5044 x__5088 x__5089 x__5090 x__5091))))
                      (fun (ys''ys''_4650:(x_1:int ->
                                           x_2:int ->
                                           (x_4:bool ->
                                            x_5:int ->
                                            x_6:bool ->
                                            x_7:int[(not x_4) || (not x_6) || (
                                                    not (x_1 <= x_2)) || 
                                                    x_1 < 0 || x_2 < 0 || 
                                                    x_5 <= x_7]
                                            -> X)
                                           -> X))
                       ->
                       (k_insert_2260
                         (fun (x1_4664:int) (x2_4665:int) 
                              (k_insert_4666:(x_1:bool ->
                                              x_2:int ->
                                              x_3:bool ->
                                              x_4:int[(not x_1) || (not x_3) || (
                                                      not (x1_4664 <= x2_4665)) || 
                                                      x1_4664 < 0 || 
                                                      x2_4665 < 0 || 
                                                      x_2 <= x_4]
                                              -> X))
                          ->
                          (if (x1_4664 <= 0)
                            (l0
                              (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                                (l1
                                  (ysys_1016 0 (x2_4665 - 1)
                                    (fun (xs11_4683:bool) (xs12_4684:int) (xs21_4685:bool) 
                                         (xs22_4686:x_1:int[(not xs11_4683) || (
                                                            not xs21_4685) || (
                                                            not (0 <= (x2_4665 - 1))) || 
                                                            0 < 0 || 
                                                            x2_4665 - 1 < 0 || 
                                                            xs12_4684 <= x_1])
                                     ->
                                     (if xs21_4685
                                       (l1
                                         (if (1 = x2_4665)
                                           (l0
                                             (if (xs22_4686 = xs12_4684)
                                               (l1
                                                 (if (p11_4797 <=> xs11_4683)
                                                   (l1
                                                     (if (xs12_4684 = p12_4798)
                                                       (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                       (l0
                                                         (loop_2167 ()
                                                           (fun (x__5084:bool) (x__5085:int) (x__5086:bool) 
                                                                (x__5087:int)
                                                            -> (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                                                   (l0
                                                     (l0
                                                       (loop_2167 ()
                                                         (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int)
                                                          -> (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                                               (l0
                                                 (loop_2167 ()
                                                   (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                                                    (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))))
                                           (l1
                                             (l1
                                               (if (p11_4797 <=> xs11_4683)
                                                 (l1
                                                   (if (xs12_4684 = p12_4798)
                                                     (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                     (l0
                                                       (loop_2167 ()
                                                         (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int)
                                                          -> (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                                                 (l0
                                                   (l0
                                                     (loop_2167 ()
                                                       (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int)
                                                        -> (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))))))))
                                       (l0
                                         (loop_2167 ()
                                           (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                                            (k_insert_4666 x__5064 x__5065 x__5066 x__5067))))))))))
                            (l1
                              (if (x2_4665 <= 0)
                                (l0
                                  (ys''ys''_4650 x1_4664 0
                                    (fun (p11_4693:bool) (p12_4694:int) (p21_4695:bool) 
                                         (p22_4696:x_1:int[(not p11_4693) || (
                                                           not p21_4695) || (
                                                           not (x1_4664 <= 0)) || 
                                                           x1_4664 < 0 || 
                                                           0 < 0 || p12_4694 <= x_1])
                                     -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                                (l1
                                  (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1)
                                    (fun (x__5060:bool) (x__5061:int) (x__5062:bool) 
                                         (x__5063:x_1:int[(not x__5060) || (
                                                          not x__5062) || (
                                                          not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                                          x1_4664 - 1 < 0 || 
                                                          x2_4665 - 1 < 0 || 
                                                          x__5061 <= x_1])
                                     -> (k_insert_4666 x__5060 x__5061 x__5062 x__5063))))))))))))))
              (l0
                (k_insert_2260
                  (fun (x1_4962:int) (x2_4963:int) 
                       (k_insert_rsrs_4964:(x_1:bool ->
                                            x_2:int ->
                                            x_3:bool ->
                                            x_4:int[(not x_1) || (not x_3) || (
                                                    not (x1_4962 <= x2_4963)) || 
                                                    x1_4962 < 0 || x2_4963 < 0 || 
                                                    x_2 <= x_4]
                                            -> X))
                   ->
                   (if (x2_4963 = x1_4962)
                     (l0
                       (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                         (l1 (k_insert_rsrs_4964 false 0 false 0))))
                     (l1
                       (if (x1_4962 <= 0)
                         (l0
                           (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                             (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
                         (l1
                           (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                             (l1 (k_insert_rsrs_4964 false 0 false 0))))))))))))): X
ETA: (xsxs_1042 0 0
       (fun p11_4699 p12_4700 p21_4701 p22_4702 ->
        (if p11_4699
          (l1
            (xsxs_1042 0 0
              (fun p11_4807 p12_4808 p21_4809 p22_4810 ->
               (insertsort_2165
                 (fun x1_4989 x2_4990 k_insertsort_xs'xs'_4991 ->
                  (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1) k_insertsort_xs'xs'_4991))
                 (fun x_4734 -> (insert_1014 p12_4808 x_4734 k_insertsort_2811))))))
          (l0
            (k_insertsort_2811 (fun i1_4970 i2_4971 k_insertsort_rsrs_4972 -> (k_insertsort_rsrs_4972 false 0 false 0))))))): X
ETA: (fun p11_4699 p12_4700 p21_4701 p22_4702 ->
      (if p11_4699
        (l1
          (xsxs_1042 0 0
            (fun p11_4807 p12_4808 p21_4809 p22_4810 ->
             (insertsort_2165
               (fun x1_4989 x2_4990 k_insertsort_xs'xs'_4991 ->
                (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1) k_insertsort_xs'xs'_4991))
               (fun x_4734 -> (insert_1014 p12_4808 x_4734 k_insertsort_2811))))))
        (l0
          (k_insertsort_2811 (fun i1_4970 i2_4971 k_insertsort_rsrs_4972 -> (k_insertsort_rsrs_4972 false 0 false 0)))))): (
bool -> int -> bool -> int -> X)
ETA: (fun p12_4700 p21_4701 p22_4702 ->
      (if p11_4699
        (l1
          (xsxs_1042 0 0
            (fun p11_4807 p12_4808 p21_4809 p22_4810 ->
             (insertsort_2165
               (fun x1_4989 x2_4990 k_insertsort_xs'xs'_4991 ->
                (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1) k_insertsort_xs'xs'_4991))
               (fun x_4734 -> (insert_1014 p12_4808 x_4734 k_insertsort_2811))))))
        (l0
          (k_insertsort_2811 (fun i1_4970 i2_4971 k_insertsort_rsrs_4972 -> (k_insertsort_rsrs_4972 false 0 false 0)))))): (
int -> bool -> int -> X)
ETA: (fun p21_4701 p22_4702 ->
      (if p11_4699
        (l1
          (xsxs_1042 0 0
            (fun p11_4807 p12_4808 p21_4809 p22_4810 ->
             (insertsort_2165
               (fun x1_4989 x2_4990 k_insertsort_xs'xs'_4991 ->
                (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1) k_insertsort_xs'xs'_4991))
               (fun x_4734 -> (insert_1014 p12_4808 x_4734 k_insertsort_2811))))))
        (l0
          (k_insertsort_2811 (fun i1_4970 i2_4971 k_insertsort_rsrs_4972 -> (k_insertsort_rsrs_4972 false 0 false 0)))))): (
bool -> int -> X)
ETA: (fun p22_4702 ->
      (if p11_4699
        (l1
          (xsxs_1042 0 0
            (fun p11_4807 p12_4808 p21_4809 p22_4810 ->
             (insertsort_2165
               (fun x1_4989 x2_4990 k_insertsort_xs'xs'_4991 ->
                (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1) k_insertsort_xs'xs'_4991))
               (fun x_4734 -> (insert_1014 p12_4808 x_4734 k_insertsort_2811))))))
        (l0
          (k_insertsort_2811 (fun i1_4970 i2_4971 k_insertsort_rsrs_4972 -> (k_insertsort_rsrs_4972 false 0 false 0)))))): (int
->
X)
ETA: (if p11_4699
       (l1
         (xsxs_1042 0 0
           (fun p11_4807 p12_4808 p21_4809 p22_4810 ->
            (insertsort_2165
              (fun x1_4989 x2_4990 k_insertsort_xs'xs'_4991 ->
               (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1) k_insertsort_xs'xs'_4991))
              (fun x_4734 -> (insert_1014 p12_4808 x_4734 k_insertsort_2811))))))
       (l0 (k_insertsort_2811 (fun i1_4970 i2_4971 k_insertsort_rsrs_4972 -> (k_insertsort_rsrs_4972 false 0 false 0))))): X
ETA: (l0 (k_insertsort_2811 (fun i1_4970 i2_4971 k_insertsort_rsrs_4972 -> (k_insertsort_rsrs_4972 false 0 false 0)))): X
ETA: (k_insertsort_2811 (fun i1_4970 i2_4971 k_insertsort_rsrs_4972 -> (k_insertsort_rsrs_4972 false 0 false 0))): X
ETA: (fun i1_4970 i2_4971 k_insertsort_rsrs_4972 -> (k_insertsort_rsrs_4972 false 0 false 0)): (
x_1:int ->
x_2:int ->
(x_4:bool ->
 x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
-> X)
ETA: (fun i2_4971 k_insertsort_rsrs_4972 -> (k_insertsort_rsrs_4972 false 0 false 0)): (
x_1:int ->
(x_3:bool ->
 x_4:int ->
 x_5:bool -> x_6:int[(not x_3) || (not x_5) || (not (i1_4970 <= x_1)) || i1_4970 < 0 || x_1 < 0 || x_4 <= x_6] -> X)
-> X)
ETA: (fun k_insertsort_rsrs_4972 -> (k_insertsort_rsrs_4972 false 0 false 0)): ((
x_2:bool ->
x_3:int ->
x_4:bool -> x_5:int[(not x_2) || (not x_4) || (not (i1_4970 <= i2_4971)) || i1_4970 < 0 || i2_4971 < 0 || x_3 <= x_5]
-> X) ->
X)
ETA: (k_insertsort_rsrs_4972 false 0 false 0): X
ETA: 0: x_1:int[(not false) || (not false) || (not (i1_4970 <= i2_4971)) || i1_4970 < 0 || i2_4971 < 0 || 0 <= x_1]
ETA: false: bool
ETA: 0: int
ETA: false: bool
ETA_AUX: (k_insertsort_rsrs_4972 false 0 false 0): X
ETA_AUX: (k_insertsort_2811
           (fun (i1_4970:int) (i2_4971:int) 
                (k_insertsort_rsrs_4972:(x_1:bool ->
                                         x_2:int ->
                                         x_3:bool ->
                                         x_4:int[(not x_1) || (not x_3) || (
                                                 not (i1_4970 <= i2_4971)) || 
                                                 i1_4970 < 0 || i2_4971 < 0 || 
                                                 x_2 <= x_4]
                                         -> X))
            -> (k_insertsort_rsrs_4972 false 0 false 0))): X
ETA: (l1
       (xsxs_1042 0 0
         (fun p11_4807 p12_4808 p21_4809 p22_4810 ->
          (insertsort_2165
            (fun x1_4989 x2_4990 k_insertsort_xs'xs'_4991 ->
             (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1) k_insertsort_xs'xs'_4991))
            (fun x_4734 -> (insert_1014 p12_4808 x_4734 k_insertsort_2811)))))): X
ETA: (xsxs_1042 0 0
       (fun p11_4807 p12_4808 p21_4809 p22_4810 ->
        (insertsort_2165
          (fun x1_4989 x2_4990 k_insertsort_xs'xs'_4991 ->
           (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1) k_insertsort_xs'xs'_4991))
          (fun x_4734 -> (insert_1014 p12_4808 x_4734 k_insertsort_2811))))): X
ETA: (fun p11_4807 p12_4808 p21_4809 p22_4810 ->
      (insertsort_2165
        (fun x1_4989 x2_4990 k_insertsort_xs'xs'_4991 ->
         (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1) k_insertsort_xs'xs'_4991))
        (fun x_4734 -> (insert_1014 p12_4808 x_4734 k_insertsort_2811)))): (
bool -> int -> bool -> int -> X)
ETA: (fun p12_4808 p21_4809 p22_4810 ->
      (insertsort_2165
        (fun x1_4989 x2_4990 k_insertsort_xs'xs'_4991 ->
         (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1) k_insertsort_xs'xs'_4991))
        (fun x_4734 -> (insert_1014 p12_4808 x_4734 k_insertsort_2811)))): (
int -> bool -> int -> X)
ETA: (fun p21_4809 p22_4810 ->
      (insertsort_2165
        (fun x1_4989 x2_4990 k_insertsort_xs'xs'_4991 ->
         (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1) k_insertsort_xs'xs'_4991))
        (fun x_4734 -> (insert_1014 p12_4808 x_4734 k_insertsort_2811)))): (
bool -> int -> X)
ETA: (fun p22_4810 ->
      (insertsort_2165
        (fun x1_4989 x2_4990 k_insertsort_xs'xs'_4991 ->
         (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1) k_insertsort_xs'xs'_4991))
        (fun x_4734 -> (insert_1014 p12_4808 x_4734 k_insertsort_2811)))): (int ->
X)
ETA: (insertsort_2165
       (fun x1_4989 x2_4990 k_insertsort_xs'xs'_4991 ->
        (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1) k_insertsort_xs'xs'_4991))
       (fun x_4734 -> (insert_1014 p12_4808 x_4734 k_insertsort_2811))): X
ETA: (fun x_4734 -> (insert_1014 p12_4808 x_4734 k_insertsort_2811)): ((
x_2:int ->
x_3:int ->
(x_5:bool ->
 x_6:int -> x_7:bool -> x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_2 < 0 || x_3 < 0 || x_6 <= x_8] -> X)
-> X) ->
X)
ETA: (insert_1014 p12_4808 x_4734 k_insertsort_2811): X
ETA: k_insertsort_2811: ((x_2:int ->
                          x_3:int ->
                          (x_5:bool ->
                           x_6:int ->
                           x_7:bool ->
                           x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_2 < 0 || x_3 < 0 || x_6 <= x_8] ->
                           X)
                          -> X) ->
X)
ETA_AUX: k_insertsort_2811: ((x_2:int ->
                              x_3:int ->
                              (x_5:bool ->
                               x_6:int ->
                               x_7:bool ->
                               x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_2 < 0 || x_3 < 0 || x_6 <= x_8]
                               -> X)
                              -> X) ->
X)
ETA_AUX: x__5120: (x_1:int ->
                   x_2:int ->
                   (x_4:bool ->
                    x_5:int ->
                    x_6:bool ->
                    x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
                   -> X)
ETA_AUX: x__5121: int
ETA_AUX: (x__5120 x__5121): (x_1:int ->
                             (x_3:bool ->
                              x_4:int ->
                              x_5:bool ->
                              x_6:int[(not x_3) || (not x_5) || (not (x__5121 <= x_1)) || 
                                      x__5121 < 0 || x_1 < 0 || x_4 <= x_6]
                              -> X)
                             -> X)
ETA_AUX: x__5122: int
ETA_AUX: (x__5120 x__5121 x__5122): ((x_2:bool ->
                                      x_3:int ->
                                      x_4:bool ->
                                      x_5:int[(not x_2) || (not x_4) || (
                                              not (x__5121 <= x__5122)) || 
                                              x__5121 < 0 || x__5122 < 0 || 
                                              x_3 <= x_5]
                                      -> X) ->
X)
ETA_AUX: x__5123: (x_1:bool ->
                   x_2:int ->
                   x_3:bool ->
                   x_4:int[(not x_1) || (not x_3) || (not (x__5121 <= x__5122)) || 
                           x__5121 < 0 || x__5122 < 0 || x_2 <= x_4]
                   -> X)
ETA_AUX: x__5124: bool
ETA_AUX: (x__5123 x__5124): (x_1:int ->
                             x_2:bool ->
                             x_3:int[(not x__5124) || (not x_2) || (not (x__5121 <= x__5122)) || 
                                     x__5121 < 0 || x__5122 < 0 || x_1 <= x_3]
                             -> X)
ETA_AUX: x__5125: int
ETA_AUX: (x__5123 x__5124 x__5125): (x_1:bool ->
                                     x_2:int[(not x__5124) || (not x_1) || (
                                             not (x__5121 <= x__5122)) || 
                                             x__5121 < 0 || x__5122 < 0 || 
                                             x__5125 <= x_2]
                                     -> X)
ETA_AUX: x__5126: bool
ETA_AUX: (x__5123 x__5124 x__5125 x__5126): (x_1:int[(not x__5124) || (
                                                     not x__5126) || (
                                                     not (x__5121 <= x__5122)) || 
                                                     x__5121 < 0 || x__5122 < 0 || 
                                                     x__5125 <= x_1] ->
X)
ETA_AUX: x__5127: x_1:int[(not x__5124) || (not x__5126) || (not (x__5121 <= x__5122)) || 
                          x__5121 < 0 || x__5122 < 0 || x__5125 <= x_1]
ETA_AUX: (x__5123 x__5124 x__5125 x__5126 x__5127): X
ETA_AUX: (x__5120 x__5121 x__5122
           (fun (x__5124:bool) (x__5125:int) (x__5126:bool) 
                (x__5127:x_1:int[(not x__5124) || (not x__5126) || (not (x__5121 <= x__5122)) || 
                                 x__5121 < 0 || x__5122 < 0 || x__5125 <= x_1])
            -> (x__5123 x__5124 x__5125 x__5126 x__5127))): X
ETA_AUX: (k_insertsort_2811
           (fun (x__5121:int) (x__5122:int) 
                (x__5123:(x_1:bool ->
                          x_2:int ->
                          x_3:bool ->
                          x_4:int[(not x_1) || (not x_3) || (not (x__5121 <= x__5122)) || 
                                  x__5121 < 0 || x__5122 < 0 || x_2 <= x_4]
                          -> X))
            ->
            (x__5120 x__5121 x__5122
              (fun (x__5124:bool) (x__5125:int) (x__5126:bool) 
                   (x__5127:x_1:int[(not x__5124) || (not x__5126) || (
                                    not (x__5121 <= x__5122)) || x__5121 < 0 || 
                                    x__5122 < 0 || x__5125 <= x_1])
               -> (x__5123 x__5124 x__5125 x__5126 x__5127))))): X
ETA: x_4734: (x_1:int ->
              x_2:int ->
              (x_4:bool ->
               x_5:int ->
               x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] ->
               X)
              -> X)
ETA_AUX: x_4734: (x_1:int ->
                  x_2:int ->
                  (x_4:bool ->
                   x_5:int ->
                   x_6:bool ->
                   x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
                  -> X)
ETA_AUX: x__5128: int
ETA_AUX: (x_4734 x__5128): (x_1:int ->
                            (x_3:bool ->
                             x_4:int ->
                             x_5:bool ->
                             x_6:int[(not x_3) || (not x_5) || (not (x__5128 <= x_1)) || 
                                     x__5128 < 0 || x_1 < 0 || x_4 <= x_6]
                             -> X)
                            -> X)
ETA_AUX: x__5129: int
ETA_AUX: (x_4734 x__5128 x__5129): ((x_2:bool ->
                                     x_3:int ->
                                     x_4:bool ->
                                     x_5:int[(not x_2) || (not x_4) || (
                                             not (x__5128 <= x__5129)) || 
                                             x__5128 < 0 || x__5129 < 0 || 
                                             x_3 <= x_5]
                                     -> X) ->
X)
ETA_AUX: x__5130: (x_1:bool ->
                   x_2:int ->
                   x_3:bool ->
                   x_4:int[(not x_1) || (not x_3) || (not (x__5128 <= x__5129)) || 
                           x__5128 < 0 || x__5129 < 0 || x_2 <= x_4]
                   -> X)
ETA_AUX: x__5131: bool
ETA_AUX: (x__5130 x__5131): (x_1:int ->
                             x_2:bool ->
                             x_3:int[(not x__5131) || (not x_2) || (not (x__5128 <= x__5129)) || 
                                     x__5128 < 0 || x__5129 < 0 || x_1 <= x_3]
                             -> X)
ETA_AUX: x__5132: int
ETA_AUX: (x__5130 x__5131 x__5132): (x_1:bool ->
                                     x_2:int[(not x__5131) || (not x_1) || (
                                             not (x__5128 <= x__5129)) || 
                                             x__5128 < 0 || x__5129 < 0 || 
                                             x__5132 <= x_2]
                                     -> X)
ETA_AUX: x__5133: bool
ETA_AUX: (x__5130 x__5131 x__5132 x__5133): (x_1:int[(not x__5131) || (
                                                     not x__5133) || (
                                                     not (x__5128 <= x__5129)) || 
                                                     x__5128 < 0 || x__5129 < 0 || 
                                                     x__5132 <= x_1] ->
X)
ETA_AUX: x__5134: x_1:int[(not x__5131) || (not x__5133) || (not (x__5128 <= x__5129)) || 
                          x__5128 < 0 || x__5129 < 0 || x__5132 <= x_1]
ETA_AUX: (x__5130 x__5131 x__5132 x__5133 x__5134): X
ETA_AUX: (x_4734 x__5128 x__5129
           (fun (x__5131:bool) (x__5132:int) (x__5133:bool) 
                (x__5134:x_1:int[(not x__5131) || (not x__5133) || (not (x__5128 <= x__5129)) || 
                                 x__5128 < 0 || x__5129 < 0 || x__5132 <= x_1])
            -> (x__5130 x__5131 x__5132 x__5133 x__5134))): X
ETA: p12_4808: int
ETA_AUX: (insert_1014 p12_4808
           (fun (x__5128:int) (x__5129:int) 
                (x__5130:(x_1:bool ->
                          x_2:int ->
                          x_3:bool ->
                          x_4:int[(not x_1) || (not x_3) || (not (x__5128 <= x__5129)) || 
                                  x__5128 < 0 || x__5129 < 0 || x_2 <= x_4]
                          -> X))
            ->
            (x_4734 x__5128 x__5129
              (fun (x__5131:bool) (x__5132:int) (x__5133:bool) 
                   (x__5134:x_1:int[(not x__5131) || (not x__5133) || (
                                    not (x__5128 <= x__5129)) || x__5128 < 0 || 
                                    x__5129 < 0 || x__5132 <= x_1])
               -> (x__5130 x__5131 x__5132 x__5133 x__5134))))
           (fun (x__5120:(x_1:int ->
                          x_2:int ->
                          (x_4:bool ->
                           x_5:int ->
                           x_6:bool ->
                           x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] ->
                           X)
                          -> X))
            ->
            (k_insertsort_2811
              (fun (x__5121:int) (x__5122:int) 
                   (x__5123:(x_1:bool ->
                             x_2:int ->
                             x_3:bool ->
                             x_4:int[(not x_1) || (not x_3) || (not (x__5121 <= x__5122)) || 
                                     x__5121 < 0 || x__5122 < 0 || x_2 <= x_4]
                             -> X))
               ->
               (x__5120 x__5121 x__5122
                 (fun (x__5124:bool) (x__5125:int) (x__5126:bool) 
                      (x__5127:x_1:int[(not x__5124) || (not x__5126) || (
                                       not (x__5121 <= x__5122)) || x__5121 < 0 || 
                                       x__5122 < 0 || x__5125 <= x_1])
                  -> (x__5123 x__5124 x__5125 x__5126 x__5127))))))): X
ETA: (fun x1_4989 x2_4990 k_insertsort_xs'xs'_4991 -> (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1) k_insertsort_xs'xs'_4991)): (
int -> int -> (bool -> int -> bool -> int -> X) -> X)
ETA: (fun x2_4990 k_insertsort_xs'xs'_4991 -> (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1) k_insertsort_xs'xs'_4991)): (
int -> (bool -> int -> bool -> int -> X) -> X)
ETA: (fun k_insertsort_xs'xs'_4991 -> (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1) k_insertsort_xs'xs'_4991)): ((
bool -> int -> bool -> int -> X) ->
X)
ETA: (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1) k_insertsort_xs'xs'_4991): X
ETA: k_insertsort_xs'xs'_4991: (bool -> int -> bool -> int -> X)
ETA_AUX: k_insertsort_xs'xs'_4991: (bool -> int -> bool -> int -> X)
ETA_AUX: x__5135: bool
ETA_AUX: (k_insertsort_xs'xs'_4991 x__5135): (int -> bool -> int -> X)
ETA_AUX: x__5136: int
ETA_AUX: (k_insertsort_xs'xs'_4991 x__5135 x__5136): (bool -> int -> X)
ETA_AUX: x__5137: bool
ETA_AUX: (k_insertsort_xs'xs'_4991 x__5135 x__5136 x__5137): (int ->
X)
ETA_AUX: x__5138: int
ETA_AUX: (k_insertsort_xs'xs'_4991 x__5135 x__5136 x__5137 x__5138): X
ETA: (x2_4990 + 1): int
ETA: (x1_4989 + 1): int
ETA_AUX: (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1)
           (fun (x__5135:bool) (x__5136:int) (x__5137:bool) (x__5138:int) ->
            (k_insertsort_xs'xs'_4991 x__5135 x__5136 x__5137 x__5138))): X
ETA_AUX: (insertsort_2165
           (fun (x1_4989:int) (x2_4990:int) (k_insertsort_xs'xs'_4991:(bool -> int -> bool -> int -> X)) ->
            (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1)
              (fun (x__5135:bool) (x__5136:int) (x__5137:bool) (x__5138:int) ->
               (k_insertsort_xs'xs'_4991 x__5135 x__5136 x__5137 x__5138))))
           (fun (x_4734:(x_1:int ->
                         x_2:int ->
                         (x_4:bool ->
                          x_5:int ->
                          x_6:bool ->
                          x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] ->
                          X)
                         -> X))
            ->
            (insert_1014 p12_4808
              (fun (x__5128:int) (x__5129:int) 
                   (x__5130:(x_1:bool ->
                             x_2:int ->
                             x_3:bool ->
                             x_4:int[(not x_1) || (not x_3) || (not (x__5128 <= x__5129)) || 
                                     x__5128 < 0 || x__5129 < 0 || x_2 <= x_4]
                             -> X))
               ->
               (x_4734 x__5128 x__5129
                 (fun (x__5131:bool) (x__5132:int) (x__5133:bool) 
                      (x__5134:x_1:int[(not x__5131) || (not x__5133) || (
                                       not (x__5128 <= x__5129)) || x__5128 < 0 || 
                                       x__5129 < 0 || x__5132 <= x_1])
                  -> (x__5130 x__5131 x__5132 x__5133 x__5134))))
              (fun (x__5120:(x_1:int ->
                             x_2:int ->
                             (x_4:bool ->
                              x_5:int ->
                              x_6:bool ->
                              x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7]
                              -> X)
                             -> X))
               ->
               (k_insertsort_2811
                 (fun (x__5121:int) (x__5122:int) 
                      (x__5123:(x_1:bool ->
                                x_2:int ->
                                x_3:bool ->
                                x_4:int[(not x_1) || (not x_3) || (not (x__5121 <= x__5122)) || 
                                        x__5121 < 0 || x__5122 < 0 || 
                                        x_2 <= x_4]
                                -> X))
                  ->
                  (x__5120 x__5121 x__5122
                    (fun (x__5124:bool) (x__5125:int) (x__5126:bool) 
                         (x__5127:x_1:int[(not x__5124) || (not x__5126) || (
                                          not (x__5121 <= x__5122)) || 
                                          x__5121 < 0 || x__5122 < 0 || 
                                          x__5125 <= x_1])
                     -> (x__5123 x__5124 x__5125 x__5126 x__5127))))))))): X
ETA: 0: int
ETA: 0: int
ETA_AUX: (xsxs_1042 0 0
           (fun (p11_4807:bool) (p12_4808:int) (p21_4809:bool) (p22_4810:int) ->
            (insertsort_2165
              (fun (x1_4989:int) (x2_4990:int) (k_insertsort_xs'xs'_4991:(bool -> int -> bool -> int -> X)) ->
               (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1)
                 (fun (x__5135:bool) (x__5136:int) (x__5137:bool) (x__5138:int) ->
                  (k_insertsort_xs'xs'_4991 x__5135 x__5136 x__5137 x__5138))))
              (fun (x_4734:(x_1:int ->
                            x_2:int ->
                            (x_4:bool ->
                             x_5:int ->
                             x_6:bool ->
                             x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7]
                             -> X)
                            -> X))
               ->
               (insert_1014 p12_4808
                 (fun (x__5128:int) (x__5129:int) 
                      (x__5130:(x_1:bool ->
                                x_2:int ->
                                x_3:bool ->
                                x_4:int[(not x_1) || (not x_3) || (not (x__5128 <= x__5129)) || 
                                        x__5128 < 0 || x__5129 < 0 || 
                                        x_2 <= x_4]
                                -> X))
                  ->
                  (x_4734 x__5128 x__5129
                    (fun (x__5131:bool) (x__5132:int) (x__5133:bool) 
                         (x__5134:x_1:int[(not x__5131) || (not x__5133) || (
                                          not (x__5128 <= x__5129)) || 
                                          x__5128 < 0 || x__5129 < 0 || 
                                          x__5132 <= x_1])
                     -> (x__5130 x__5131 x__5132 x__5133 x__5134))))
                 (fun (x__5120:(x_1:int ->
                                x_2:int ->
                                (x_4:bool ->
                                 x_5:int ->
                                 x_6:bool ->
                                 x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || 
                                         x_1 < 0 || x_2 < 0 || x_5 <= x_7]
                                 -> X)
                                -> X))
                  ->
                  (k_insertsort_2811
                    (fun (x__5121:int) (x__5122:int) 
                         (x__5123:(x_1:bool ->
                                   x_2:int ->
                                   x_3:bool ->
                                   x_4:int[(not x_1) || (not x_3) || (
                                           not (x__5121 <= x__5122)) || 
                                           x__5121 < 0 || x__5122 < 0 || 
                                           x_2 <= x_4]
                                   -> X))
                     ->
                     (x__5120 x__5121 x__5122
                       (fun (x__5124:bool) (x__5125:int) (x__5126:bool) 
                            (x__5127:x_1:int[(not x__5124) || (not x__5126) || (
                                             not (x__5121 <= x__5122)) || 
                                             x__5121 < 0 || x__5122 < 0 || 
                                             x__5125 <= x_1])
                        -> (x__5123 x__5124 x__5125 x__5126 x__5127))))))))))): X
ETA: 0: int
ETA: 0: int
ETA_AUX: (xsxs_1042 0 0
           (fun (p11_4699:bool) (p12_4700:int) (p21_4701:bool) (p22_4702:int) ->
            (if p11_4699
              (l1
                (xsxs_1042 0 0
                  (fun (p11_4807:bool) (p12_4808:int) (p21_4809:bool) (p22_4810:int) ->
                   (insertsort_2165
                     (fun (x1_4989:int) (x2_4990:int) (k_insertsort_xs'xs'_4991:(bool -> int -> bool -> int -> X)) ->
                      (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1)
                        (fun (x__5135:bool) (x__5136:int) (x__5137:bool) (x__5138:int) ->
                         (k_insertsort_xs'xs'_4991 x__5135 x__5136 x__5137 x__5138))))
                     (fun (x_4734:(x_1:int ->
                                   x_2:int ->
                                   (x_4:bool ->
                                    x_5:int ->
                                    x_6:bool ->
                                    x_7:int[(not x_4) || (not x_6) || (
                                            not (x_1 <= x_2)) || x_1 < 0 || 
                                            x_2 < 0 || x_5 <= x_7]
                                    -> X)
                                   -> X))
                      ->
                      (insert_1014 p12_4808
                        (fun (x__5128:int) (x__5129:int) 
                             (x__5130:(x_1:bool ->
                                       x_2:int ->
                                       x_3:bool ->
                                       x_4:int[(not x_1) || (not x_3) || (
                                               not (x__5128 <= x__5129)) || 
                                               x__5128 < 0 || x__5129 < 0 || 
                                               x_2 <= x_4]
                                       -> X))
                         ->
                         (x_4734 x__5128 x__5129
                           (fun (x__5131:bool) (x__5132:int) (x__5133:bool) 
                                (x__5134:x_1:int[(not x__5131) || (not x__5133) || (
                                                 not (x__5128 <= x__5129)) || 
                                                 x__5128 < 0 || x__5129 < 0 || 
                                                 x__5132 <= x_1])
                            -> (x__5130 x__5131 x__5132 x__5133 x__5134))))
                        (fun (x__5120:(x_1:int ->
                                       x_2:int ->
                                       (x_4:bool ->
                                        x_5:int ->
                                        x_6:bool ->
                                        x_7:int[(not x_4) || (not x_6) || (
                                                not (x_1 <= x_2)) || 
                                                x_1 < 0 || x_2 < 0 || 
                                                x_5 <= x_7]
                                        -> X)
                                       -> X))
                         ->
                         (k_insertsort_2811
                           (fun (x__5121:int) (x__5122:int) 
                                (x__5123:(x_1:bool ->
                                          x_2:int ->
                                          x_3:bool ->
                                          x_4:int[(not x_1) || (not x_3) || (
                                                  not (x__5121 <= x__5122)) || 
                                                  x__5121 < 0 || x__5122 < 0 || 
                                                  x_2 <= x_4]
                                          -> X))
                            ->
                            (x__5120 x__5121 x__5122
                              (fun (x__5124:bool) (x__5125:int) (x__5126:bool) 
                                   (x__5127:x_1:int[(not x__5124) || (
                                                    not x__5126) || (
                                                    not (x__5121 <= x__5122)) || 
                                                    x__5121 < 0 || x__5122 < 0 || 
                                                    x__5125 <= x_1])
                               -> (x__5123 x__5124 x__5125 x__5126 x__5127))))))))))))
              (l0
                (k_insertsort_2811
                  (fun (i1_4970:int) (i2_4971:int) 
                       (k_insertsort_rsrs_4972:(x_1:bool ->
                                                x_2:int ->
                                                x_3:bool ->
                                                x_4:int[(not x_1) || (
                                                        not x_3) || (
                                                        not (i1_4970 <= i2_4971)) || 
                                                        i1_4970 < 0 || 
                                                        i2_4971 < 0 || 
                                                        x_2 <= x_4]
                                                -> X))
                   -> (k_insertsort_rsrs_4972 false 0 false 0))))))): X
ETA: (loop_2167 () k_loop_2237): X
ETA: k_loop_2237: (bool -> int -> bool -> int -> X)
ETA_AUX: k_loop_2237: (bool -> int -> bool -> int -> X)
ETA_AUX: x__5139: bool
ETA_AUX: (k_loop_2237 x__5139): (int -> bool -> int -> X)
ETA_AUX: x__5140: int
ETA_AUX: (k_loop_2237 x__5139 x__5140): (bool -> int -> X)
ETA_AUX: x__5141: bool
ETA_AUX: (k_loop_2237 x__5139 x__5140 x__5141): (int ->
X)
ETA_AUX: x__5142: int
ETA_AUX: (k_loop_2237 x__5139 x__5140 x__5141 x__5142): X
ETA: (): unit
ETA_AUX: (loop_2167 ()
           (fun (x__5139:bool) (x__5140:int) (x__5141:bool) (x__5142:int) ->
            (k_loop_2237 x__5139 x__5140 x__5141 x__5142))): X
ETA: (l0 (k_make_list_2998 (fun i_4750 k_make_list_4751 -> (k_make_list_4751 false 0)))): X
ETA: (k_make_list_2998 (fun i_4750 k_make_list_4751 -> (k_make_list_4751 false 0))): X
ETA: (fun i_4750 k_make_list_4751 -> (k_make_list_4751 false 0)): (int -> (bool -> int -> X) -> X)
ETA: (fun k_make_list_4751 -> (k_make_list_4751 false 0)): ((bool -> int -> X) ->
X)
ETA: (k_make_list_4751 false 0): X
ETA: 0: int
ETA: false: bool
ETA_AUX: (k_make_list_4751 false 0): X
ETA_AUX: (k_make_list_2998 (fun (i_4750:int) (k_make_list_4751:(bool -> int -> X)) -> (k_make_list_4751 false 0))): X
ETA: (l1
       (rand_int
         (fun n_4754 ->
          (make_list_1049 (n_4754 - 1)
            (fun xs_4758 ->
             (k_make_list_2998
               (fun i_4767 k_make_list_4768 ->
                (if (i_4767 = 0) (l0 (k_make_list_4768 true n_4754)) (l1 (xs_4758 (i_4767 - 1) k_make_list_4768)))))))))): X
ETA: (rand_int
       (fun n_4754 ->
        (make_list_1049 (n_4754 - 1)
          (fun xs_4758 ->
           (k_make_list_2998
             (fun i_4767 k_make_list_4768 ->
              (if (i_4767 = 0) (l0 (k_make_list_4768 true n_4754)) (l1 (xs_4758 (i_4767 - 1) k_make_list_4768))))))))): X
ETA: (fun n_4754 ->
      (make_list_1049 (n_4754 - 1)
        (fun xs_4758 ->
         (k_make_list_2998
           (fun i_4767 k_make_list_4768 ->
            (if (i_4767 = 0) (l0 (k_make_list_4768 true n_4754)) (l1 (xs_4758 (i_4767 - 1) k_make_list_4768)))))))): (int
->
unit)
ETA: (make_list_1049 (n_4754 - 1)
       (fun xs_4758 ->
        (k_make_list_2998
          (fun i_4767 k_make_list_4768 ->
           (if (i_4767 = 0) (l0 (k_make_list_4768 true n_4754)) (l1 (xs_4758 (i_4767 - 1) k_make_list_4768))))))): unit
ETA: (fun xs_4758 ->
      (k_make_list_2998
        (fun i_4767 k_make_list_4768 ->
         (if (i_4767 = 0) (l0 (k_make_list_4768 true n_4754)) (l1 (xs_4758 (i_4767 - 1) k_make_list_4768)))))): ((
int -> (bool -> int -> X) -> X) ->
X)
ETA: (k_make_list_2998
       (fun i_4767 k_make_list_4768 ->
        (if (i_4767 = 0) (l0 (k_make_list_4768 true n_4754)) (l1 (xs_4758 (i_4767 - 1) k_make_list_4768))))): X
ETA: (fun i_4767 k_make_list_4768 ->
      (if (i_4767 = 0) (l0 (k_make_list_4768 true n_4754)) (l1 (xs_4758 (i_4767 - 1) k_make_list_4768)))): (
int -> (bool -> int -> X) -> X)
ETA: (fun k_make_list_4768 ->
      (if (i_4767 = 0) (l0 (k_make_list_4768 true n_4754)) (l1 (xs_4758 (i_4767 - 1) k_make_list_4768)))): ((
bool -> int -> X) ->
X)
ETA: (if (i_4767 = 0) (l0 (k_make_list_4768 true n_4754)) (l1 (xs_4758 (i_4767 - 1) k_make_list_4768))): X
ETA: (l1 (xs_4758 (i_4767 - 1) k_make_list_4768)): X
ETA: (xs_4758 (i_4767 - 1) k_make_list_4768): X
ETA: k_make_list_4768: (bool -> int -> X)
ETA_AUX: k_make_list_4768: (bool -> int -> X)
ETA_AUX: x__5143: bool
ETA_AUX: (k_make_list_4768 x__5143): (int ->
X)
ETA_AUX: x__5144: int
ETA_AUX: (k_make_list_4768 x__5143 x__5144): X
ETA: (i_4767 - 1): int
ETA_AUX: (xs_4758 (i_4767 - 1) (fun (x__5143:bool) (x__5144:int) -> (k_make_list_4768 x__5143 x__5144))): X
ETA: (l0 (k_make_list_4768 true n_4754)): X
ETA: (k_make_list_4768 true n_4754): X
ETA: n_4754: int
ETA: true: bool
ETA_AUX: (k_make_list_4768 true n_4754): X
ETA_AUX: (k_make_list_2998
           (fun (i_4767:int) (k_make_list_4768:(bool -> int -> X)) ->
            (if (i_4767 = 0) (l0 (k_make_list_4768 true n_4754))
              (l1 (xs_4758 (i_4767 - 1) (fun (x__5143:bool) (x__5144:int) -> (k_make_list_4768 x__5143 x__5144))))))): X
ETA: (n_4754 - 1): int
ETA_AUX: (make_list_1049 (n_4754 - 1)
           (fun (xs_4758:(int -> (bool -> int -> X) -> X)) ->
            (k_make_list_2998
              (fun (i_4767:int) (k_make_list_4768:(bool -> int -> X)) ->
               (if (i_4767 = 0) (l0 (k_make_list_4768 true n_4754))
                 (l1 (xs_4758 (i_4767 - 1) (fun (x__5143:bool) (x__5144:int) -> (k_make_list_4768 x__5143 x__5144))))))))): unit
ETA_AUX: (rand_int
           (fun (n_4754:int) ->
            (make_list_1049 (n_4754 - 1)
              (fun (xs_4758:(int -> (bool -> int -> X) -> X)) ->
               (k_make_list_2998
                 (fun (i_4767:int) (k_make_list_4768:(bool -> int -> X)) ->
                  (if (i_4767 = 0) (l0 (k_make_list_4768 true n_4754))
                    (l1 (xs_4758 (i_4767 - 1) (fun (x__5143:bool) (x__5144:int) -> (k_make_list_4768 x__5143 x__5144))))))))))): X
ETA_EXPAND:
Main: main_3652
  main_3652 ->
      (rand_int
        (fun (arg1_4748:int) ->
         (make_list_1049 arg1_4748
           (fun (xs_4737:(int -> (bool -> int -> X) -> X)) ->
            (insertsort_2165
              (fun (x1_5030:int) (x2_5031:int) (k_main_xsxs_5032:(bool -> int -> bool -> int -> X)) ->
               (if (x2_5031 = x1_5030)
                 (l0
                   (xs_4737 x1_5030
                     (fun (r1_4815:bool) (r2_4816:int) -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816))))
                 (l1
                   (xs_4737 x1_5030
                     (fun (x1_4822:bool) (x2_4823:int) ->
                      (xs_4737 x2_5031
                        (fun (x1_4830:bool) (x2_4831:int) -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831))))))))
              (fun (ysys_4740:(x_1:int ->
                               x_2:int ->
                               (x_4:bool ->
                                x_5:int ->
                                x_6:bool ->
                                x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || 
                                        x_1 < 0 || x_2 < 0 || x_5 <= x_7]
                                -> X)
                               -> X))
               ->
               (check_2164
                 (fun (x__5048:int) (x__5049:int) 
                      (x__5050:(x_1:bool ->
                                x_2:int ->
                                x_3:bool ->
                                x_4:int[(not x_1) || (not x_3) || (not (x__5048 <= x__5049)) || 
                                        x__5048 < 0 || x__5049 < 0 || 
                                        x_2 <= x_4]
                                -> X))
                  ->
                  (ysys_4740 x__5048 x__5049
                    (fun (x__5051:bool) (x__5052:int) (x__5053:bool) 
                         (x__5054:x_1:int[(not x__5051) || (not x__5053) || (
                                          not (x__5048 <= x__5049)) || 
                                          x__5048 < 0 || x__5049 < 0 || 
                                          x__5052 <= x_1])
                     -> (x__5050 x__5051 x__5052 x__5053 x__5054))))
                 (fun (b_4746:x_1:bool[x_1]) ->
                  (if b_4746 (l0 end) (l1 (fail_3705 true (fun (main_4578:unit) -> end))))))))))))
  check_2164 xsxs_1056 k_check_3093 ->
      (xsxs_1056 0 0
        (fun (xs11_4587:bool) (xs12_4588:int) (xs21_4589:bool) 
             (xs22_4590:x_1:int[(not xs11_4587) || (not xs21_4589) || (
                                not (0 <= 0)) || 0 < 0 || 0 < 0 || xs12_4588 <= x_1])
         ->
         (if xs11_4587
           (l1
             (if xs21_4589
               (l1
                 (if (xs22_4590 >= xs12_4588)
                   (l0
                     (check_2164
                       (fun (x1_4978:int) (x2_4979:int) 
                            (k_check_xs'xs'_4980:(x_1:bool ->
                                                  x_2:int ->
                                                  x_3:bool ->
                                                  x_4:int[(not x_1) || (
                                                          not x_3) || (
                                                          not (x1_4978 <= x2_4979)) || 
                                                          x1_4978 < 0 || 
                                                          x2_4979 < 0 || 
                                                          x_2 <= x_4]
                                                  -> X))
                        ->
                        (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1)
                          (fun (x__5056:bool) (x__5057:int) (x__5058:bool) 
                               (x__5059:x_1:int[(not x__5056) || (not x__5058) || 
                                                (not ((x1_4978 + 1) <= (x2_4979 + 1))) || 
                                                x1_4978 + 1 < 0 || x2_4979 + 1 < 0 || 
                                                x__5057 <= x_1])
                           -> (k_check_xs'xs'_4980 x__5056 x__5057 x__5058 x__5059))))
                       (fun (x__5055:x_1:bool[x_1]) -> (k_check_3093 x__5055)))) (
                   l1 (k_check_3093 false)))) (l0 (k_check_3093 true)))) (
           l0 (k_check_3093 true)))))
  fail_3705 b k -> {fail} => (k ())
  insert_1014 x_1053 ysys_1016 k_insert_2260 ->
      (ysys_1016 0 0
        (fun (p11_4797:bool) (p12_4798:int) (p21_4799:bool) 
             (p22_4800:x_1:int[(not p11_4797) || (not p21_4799) || (not (0 <= 0)) || 0 < 0 || 0 < 0 || p12_4798 <= x_1])
         ->
         (if p11_4797
           (l1
             (if (x_1053 < p12_4798)
               (l0
                 (k_insert_2260
                   (fun (x1_4612:int) (x2_4613:int) 
                        (k_insert_4614:(x_1:bool ->
                                        x_2:int ->
                                        x_3:bool ->
                                        x_4:int[(not x_1) || (not x_3) || (
                                                not (x1_4612 <= x2_4613)) || 
                                                x1_4612 < 0 || x2_4613 < 0 || 
                                                x_2 <= x_4]
                                        -> X))
                    ->
                    (if (x1_4612 <= 0)
                      (l0
                        (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
                          (l1
                            (ysys_1016 0 (x2_4613 - 1)
                              (fun (xs11_4631:bool) (xs12_4632:int) (xs21_4633:bool) 
                                   (xs22_4634:x_1:int[(not xs11_4631) || (
                                                      not xs21_4633) || (
                                                      not (0 <= (x2_4613 - 1))) || 
                                                      0 < 0 || x2_4613 - 1 < 0 || 
                                                      xs12_4632 <= x_1])
                               ->
                               (if xs21_4633
                                 (l1
                                   (if (1 = x2_4613)
                                     (l0
                                       (if (xs22_4634 = xs12_4632)
                                         (l1
                                           (if (p11_4797 <=> xs11_4631)
                                             (l1
                                               (if (xs12_4632 = p12_4798)
                                                 (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                                 (l0
                                                   (loop_2167 ()
                                                     (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int) ->
                                                      (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                                             (l0
                                               (l0
                                                 (loop_2167 ()
                                                   (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int) ->
                                                    (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))))))
                                         (l0
                                           (loop_2167 ()
                                             (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                                              (k_insert_4614 x__5108 x__5109 x__5110 x__5111))))))
                                     (l1
                                       (l1
                                         (if (p11_4797 <=> xs11_4631)
                                           (l1
                                             (if (xs12_4632 = p12_4798) (
                                               l1 (k_insert_4614 true x_1053 true xs22_4634))
                                               (l0
                                                 (loop_2167 ()
                                                   (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int) ->
                                                    (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))))
                                           (l0
                                             (l0
                                               (loop_2167 ()
                                                 (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int) ->
                                                  (k_insert_4614 x__5100 x__5101 x__5102 x__5103))))))))))
                                 (l0
                                   (loop_2167 ()
                                     (fun (x__5096:bool) (x__5097:int) (x__5098:bool) (x__5099:int) ->
                                      (k_insert_4614 x__5096 x__5097 x__5098 x__5099))))))))))
                      (l1
                        (if (x2_4613 <= 0)
                          (l0
                            (ysys_1016 (x1_4612 - 1) 0
                              (fun (p11_4641:bool) (p12_4642:int) (p21_4643:bool) 
                                   (p22_4644:x_1:int[(not p11_4641) || (
                                                     not p21_4643) || (
                                                     not ((x1_4612 - 1) <= 0)) || 
                                                     x1_4612 - 1 < 0 || 
                                                     0 < 0 || p12_4642 <= x_1])
                               -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
                          (l1
                            (ysys_1016 (x1_4612 - 1) (x2_4613 - 1)
                              (fun (x__5092:bool) (x__5093:int) (x__5094:bool) 
                                   (x__5095:x_1:int[(not x__5092) || (
                                                    not x__5094) || (
                                                    not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                                                    x1_4612 - 1 < 0 || 
                                                    x2_4613 - 1 < 0 || 
                                                    x__5093 <= x_1])
                               -> (k_insert_4614 x__5092 x__5093 x__5094 x__5095))))))))))
               (l1
                 (insert_1014 x_1053
                   (fun (x1_5042:int) (x2_5043:int) 
                        (k_insert_ys'ys'_5044:(x_1:bool ->
                                               x_2:int ->
                                               x_3:bool ->
                                               x_4:int[(not x_1) || (
                                                       not x_3) || (not (x1_5042 <= x2_5043)) || 
                                                       x1_5042 < 0 || 
                                                       x2_5043 < 0 || 
                                                       x_2 <= x_4]
                                               -> X))
                    ->
                    (ysys_1016 (x1_5042 + 1) (x2_5043 + 1)
                      (fun (x__5088:bool) (x__5089:int) (x__5090:bool) 
                           (x__5091:x_1:int[(not x__5088) || (not x__5090) || 
                                            (not ((x1_5042 + 1) <= (x2_5043 + 1))) || 
                                            x1_5042 + 1 < 0 || x2_5043 + 1 < 0 || 
                                            x__5089 <= x_1])
                       -> (k_insert_ys'ys'_5044 x__5088 x__5089 x__5090 x__5091))))
                   (fun (ys''ys''_4650:(x_1:int ->
                                        x_2:int ->
                                        (x_4:bool ->
                                         x_5:int ->
                                         x_6:bool ->
                                         x_7:int[(not x_4) || (not x_6) || (
                                                 not (x_1 <= x_2)) || 
                                                 x_1 < 0 || x_2 < 0 || 
                                                 x_5 <= x_7]
                                         -> X)
                                        -> X))
                    ->
                    (k_insert_2260
                      (fun (x1_4664:int) (x2_4665:int) 
                           (k_insert_4666:(x_1:bool ->
                                           x_2:int ->
                                           x_3:bool ->
                                           x_4:int[(not x_1) || (not x_3) || (
                                                   not (x1_4664 <= x2_4665)) || 
                                                   x1_4664 < 0 || x2_4665 < 0 || 
                                                   x_2 <= x_4]
                                           -> X))
                       ->
                       (if (x1_4664 <= 0)
                         (l0
                           (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                             (l1
                               (ysys_1016 0 (x2_4665 - 1)
                                 (fun (xs11_4683:bool) (xs12_4684:int) (xs21_4685:bool) 
                                      (xs22_4686:x_1:int[(not xs11_4683) || (
                                                         not xs21_4685) || (
                                                         not (0 <= (x2_4665 - 1))) || 
                                                         0 < 0 || x2_4665 - 1 < 0 || 
                                                         xs12_4684 <= x_1])
                                  ->
                                  (if xs21_4685
                                    (l1
                                      (if (1 = x2_4665)
                                        (l0
                                          (if (xs22_4686 = xs12_4684)
                                            (l1
                                              (if (p11_4797 <=> xs11_4683)
                                                (l1
                                                  (if (xs12_4684 = p12_4798)
                                                    (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                    (l0
                                                      (loop_2167 ()
                                                        (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int)
                                                         -> (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                                                (l0
                                                  (l0
                                                    (loop_2167 ()
                                                      (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
                                                       (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                                            (l0
                                              (loop_2167 ()
                                                (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                                                 (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))))
                                        (l1
                                          (l1
                                            (if (p11_4797 <=> xs11_4683)
                                              (l1
                                                (if (xs12_4684 = p12_4798)
                                                  (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                  (l0
                                                    (loop_2167 ()
                                                      (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
                                                       (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                                              (l0
                                                (l0
                                                  (loop_2167 ()
                                                    (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
                                                     (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))))))))
                                    (l0
                                      (loop_2167 ()
                                        (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                                         (k_insert_4666 x__5064 x__5065 x__5066 x__5067))))))))))
                         (l1
                           (if (x2_4665 <= 0)
                             (l0
                               (ys''ys''_4650 x1_4664 0
                                 (fun (p11_4693:bool) (p12_4694:int) (p21_4695:bool) 
                                      (p22_4696:x_1:int[(not p11_4693) || (
                                                        not p21_4695) || (
                                                        not (x1_4664 <= 0)) || 
                                                        x1_4664 < 0 || 
                                                        0 < 0 || p12_4694 <= x_1])
                                  -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                             (l1
                               (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1)
                                 (fun (x__5060:bool) (x__5061:int) (x__5062:bool) 
                                      (x__5063:x_1:int[(not x__5060) || (
                                                       not x__5062) || (
                                                       not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                                       x1_4664 - 1 < 0 || 
                                                       x2_4665 - 1 < 0 || 
                                                       x__5061 <= x_1])
                                  -> (k_insert_4666 x__5060 x__5061 x__5062 x__5063))))))))))))))
           (l0
             (k_insert_2260
               (fun (x1_4962:int) (x2_4963:int) 
                    (k_insert_rsrs_4964:(x_1:bool ->
                                         x_2:int ->
                                         x_3:bool ->
                                         x_4:int[(not x_1) || (not x_3) || (
                                                 not (x1_4962 <= x2_4963)) || 
                                                 x1_4962 < 0 || x2_4963 < 0 || 
                                                 x_2 <= x_4]
                                         -> X))
                ->
                (if (x2_4963 = x1_4962)
                  (l0
                    (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                      (l1 (k_insert_rsrs_4964 false 0 false 0))))
                  (l1
                    (if (x1_4962 <= 0)
                      (l0
                        (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                          (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
                      (l1
                        (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                          (l1 (k_insert_rsrs_4964 false 0 false 0)))))))))))))
  insertsort_2165 xsxs_1042 k_insertsort_2811 ->
      (xsxs_1042 0 0
        (fun (p11_4699:bool) (p12_4700:int) (p21_4701:bool) (p22_4702:int) ->
         (if p11_4699
           (l1
             (xsxs_1042 0 0
               (fun (p11_4807:bool) (p12_4808:int) (p21_4809:bool) (p22_4810:int) ->
                (insertsort_2165
                  (fun (x1_4989:int) (x2_4990:int) (k_insertsort_xs'xs'_4991:(bool -> int -> bool -> int -> X)) ->
                   (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1)
                     (fun (x__5135:bool) (x__5136:int) (x__5137:bool) (x__5138:int) ->
                      (k_insertsort_xs'xs'_4991 x__5135 x__5136 x__5137 x__5138))))
                  (fun (x_4734:(x_1:int ->
                                x_2:int ->
                                (x_4:bool ->
                                 x_5:int ->
                                 x_6:bool ->
                                 x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || 
                                         x_1 < 0 || x_2 < 0 || x_5 <= x_7]
                                 -> X)
                                -> X))
                   ->
                   (insert_1014 p12_4808
                     (fun (x__5128:int) (x__5129:int) 
                          (x__5130:(x_1:bool ->
                                    x_2:int ->
                                    x_3:bool ->
                                    x_4:int[(not x_1) || (not x_3) || (
                                            not (x__5128 <= x__5129)) || 
                                            x__5128 < 0 || x__5129 < 0 || 
                                            x_2 <= x_4]
                                    -> X))
                      ->
                      (x_4734 x__5128 x__5129
                        (fun (x__5131:bool) (x__5132:int) (x__5133:bool) 
                             (x__5134:x_1:int[(not x__5131) || (not x__5133) || (
                                              not (x__5128 <= x__5129)) || 
                                              x__5128 < 0 || x__5129 < 0 || 
                                              x__5132 <= x_1])
                         -> (x__5130 x__5131 x__5132 x__5133 x__5134))))
                     (fun (x__5120:(x_1:int ->
                                    x_2:int ->
                                    (x_4:bool ->
                                     x_5:int ->
                                     x_6:bool ->
                                     x_7:int[(not x_4) || (not x_6) || (
                                             not (x_1 <= x_2)) || x_1 < 0 || 
                                             x_2 < 0 || x_5 <= x_7]
                                     -> X)
                                    -> X))
                      ->
                      (k_insertsort_2811
                        (fun (x__5121:int) (x__5122:int) 
                             (x__5123:(x_1:bool ->
                                       x_2:int ->
                                       x_3:bool ->
                                       x_4:int[(not x_1) || (not x_3) || (
                                               not (x__5121 <= x__5122)) || 
                                               x__5121 < 0 || x__5122 < 0 || 
                                               x_2 <= x_4]
                                       -> X))
                         ->
                         (x__5120 x__5121 x__5122
                           (fun (x__5124:bool) (x__5125:int) (x__5126:bool) 
                                (x__5127:x_1:int[(not x__5124) || (not x__5126) || (
                                                 not (x__5121 <= x__5122)) || 
                                                 x__5121 < 0 || x__5122 < 0 || 
                                                 x__5125 <= x_1])
                            -> (x__5123 x__5124 x__5125 x__5126 x__5127))))))))))))
           (l0
             (k_insertsort_2811
               (fun (i1_4970:int) (i2_4971:int) 
                    (k_insertsort_rsrs_4972:(x_1:bool ->
                                             x_2:int ->
                                             x_3:bool ->
                                             x_4:int[(not x_1) || (not x_3) || (
                                                     not (i1_4970 <= i2_4971)) || 
                                                     i1_4970 < 0 || i2_4971 < 0 || 
                                                     x_2 <= x_4]
                                             -> X))
                -> (k_insertsort_rsrs_4972 false 0 false 0)))))))
  loop_2167 x_1028 k_loop_2237 ->
      (loop_2167 ()
        (fun (x__5139:bool) (x__5140:int) (x__5141:bool) (x__5142:int) -> (k_loop_2237 x__5139 x__5140 x__5141 x__5142)))
  make_list_1049 n_1050 k_make_list_2998 when (n_1050 = 0) ->
      (l0 (k_make_list_2998 (fun (i_4750:int) (k_make_list_4751:(bool -> int -> X)) -> (k_make_list_4751 false 0))))
  make_list_1049 n_1050 k_make_list_2998 when (not (n_1050 = 0)) ->
      (l1
        (rand_int
          (fun (n_4754:int) ->
           (make_list_1049 (n_4754 - 1)
             (fun (xs_4758:(int -> (bool -> int -> X) -> X)) ->
              (k_make_list_2998
                (fun (i_4767:int) (k_make_list_4768:(bool -> int -> X)) ->
                 (if (i_4767 = 0) (l0 (k_make_list_4768 true n_4754))
                   (l1 (xs_4758 (i_4767 - 1) (fun (x__5143:bool) (x__5144:int) -> (k_make_list_4768 x__5143 x__5144))))))))))))

main_3652: ENV: 

main_3652: (rand_int
             (fun (arg1_4748:int) ->
              (make_list_1049 arg1_4748
                (fun (xs_4737:(int -> (bool -> int -> X) -> X)) ->
                 (insertsort_2165
                   (fun (x1_5030:int) (x2_5031:int) (k_main_xsxs_5032:(bool -> int -> bool -> int -> X)) ->
                    (if (x2_5031 = x1_5030)
                      (l0
                        (xs_4737 x1_5030
                          (fun (r1_4815:bool) (r2_4816:int) -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816))))
                      (l1
                        (xs_4737 x1_5030
                          (fun (x1_4822:bool) (x2_4823:int) ->
                           (xs_4737 x2_5031
                             (fun (x1_4830:bool) (x2_4831:int) -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831))))))))
                   (fun (ysys_4740:(x_1:int ->
                                    x_2:int ->
                                    (x_4:bool ->
                                     x_5:int ->
                                     x_6:bool ->
                                     x_7:int[(not x_4) || (not x_6) || (
                                             not (x_1 <= x_2)) || x_1 < 0 || 
                                             x_2 < 0 || x_5 <= x_7]
                                     -> X)
                                    -> X))
                    ->
                    (check_2164
                      (fun (x__5048:int) (x__5049:int) 
                           (x__5050:(x_1:bool ->
                                     x_2:int ->
                                     x_3:bool ->
                                     x_4:int[(not x_1) || (not x_3) || (
                                             not (x__5048 <= x__5049)) || 
                                             x__5048 < 0 || x__5049 < 0 || 
                                             x_2 <= x_4]
                                     -> X))
                       ->
                       (ysys_4740 x__5048 x__5049
                         (fun (x__5051:bool) (x__5052:int) (x__5053:bool) 
                              (x__5054:x_1:int[(not x__5051) || (not x__5053) || (
                                               not (x__5048 <= x__5049)) || 
                                               x__5048 < 0 || x__5049 < 0 || 
                                               x__5052 <= x_1])
                          -> (x__5050 x__5051 x__5052 x__5053 x__5054))))
                      (fun (b_4746:x_1:bool[x_1]) ->
                       (if b_4746 (l0 end) (l1 (fail_3705 true (fun (main_4578:unit) -> end)))))))))))) ===> (
rand_int
 (fun (arg1_4748:int) ->
  (make_list_1049 arg1_4748
    (fun (xs_4737:(int -> (bool -> int -> X) -> X)) ->
     (insertsort_2165
       (fun (x1_5030:int) (x2_5031:int) (k_main_xsxs_5032:(bool -> int -> bool -> int -> X)) ->
        (if (x2_5031 = x1_5030)
          (l0
            (xs_4737 x1_5030 (fun (r1_4815:bool) (r2_4816:int) -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816))))
          (l1
            (xs_4737 x1_5030
              (fun (x1_4822:bool) (x2_4823:int) ->
               (xs_4737 x2_5031
                 (fun (x1_4830:bool) (x2_4831:int) -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831))))))))
       (fun (ysys_4740:(x_1:int ->
                        x_2:int ->
                        (x_4:bool ->
                         x_5:int ->
                         x_6:bool ->
                         x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
                        -> X))
        ->
        (check_2164
          (fun (x__5048:int) (x__5049:int) 
               (x__5050:(x_1:bool ->
                         x_2:int ->
                         x_3:bool ->
                         x_4:int[(not x_1) || (not x_3) || (not (x__5048 <= x__5049)) || 
                                 x__5048 < 0 || x__5049 < 0 || x_2 <= x_4]
                         -> X))
           ->
           (ysys_4740 x__5048 x__5049
             (fun (x__5051:bool) (x__5052:int) (x__5053:bool) 
                  (x__5054:x_1:int[(not x__5051) || (not x__5053) || (
                                   not (x__5048 <= x__5049)) || x__5048 < 0 || 
                                   x__5049 < 0 || x__5052 <= x_1])
              -> (x__5050 x__5051 x__5052 x__5053 x__5054))))
          (fun (b_4746:x_1:bool[x_1]) -> (if b_4746 (l0 end) (l1 (fail_3705 true (fun (main_4578:unit) -> end))))))))))))
main_3652:: (rand_int
              (fun (arg1_4748:int) ->
               (make_list_1049 arg1_4748
                 (fun (xs_4737:(int -> (bool -> int -> X) -> X)) ->
                  (insertsort_2165
                    (fun (x1_5030:int) (x2_5031:int) (k_main_xsxs_5032:(bool -> int -> bool -> int -> X)) ->
                     (if (x2_5031 = x1_5030)
                       (l0
                         (xs_4737 x1_5030
                           (fun (r1_4815:bool) (r2_4816:int) -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816))))
                       (l1
                         (xs_4737 x1_5030
                           (fun (x1_4822:bool) (x2_4823:int) ->
                            (xs_4737 x2_5031
                              (fun (x1_4830:bool) (x2_4831:int) -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831))))))))
                    (fun (ysys_4740:(x_1:int ->
                                     x_2:int ->
                                     (x_4:bool ->
                                      x_5:int ->
                                      x_6:bool ->
                                      x_7:int[(not x_4) || (not x_6) || (
                                              not (x_1 <= x_2)) || x_1 < 0 || 
                                              x_2 < 0 || x_5 <= x_7]
                                      -> X)
                                     -> X))
                     ->
                     (check_2164
                       (fun (x__5048:int) (x__5049:int) 
                            (x__5050:(x_1:bool ->
                                      x_2:int ->
                                      x_3:bool ->
                                      x_4:int[(not x_1) || (not x_3) || (
                                              not (x__5048 <= x__5049)) || 
                                              x__5048 < 0 || x__5049 < 0 || 
                                              x_2 <= x_4]
                                      -> X))
                        ->
                        (ysys_4740 x__5048 x__5049
                          (fun (x__5051:bool) (x__5052:int) (x__5053:bool) 
                               (x__5054:x_1:int[(not x__5051) || (not x__5053) || (
                                                not (x__5048 <= x__5049)) || 
                                                x__5048 < 0 || x__5049 < 0 || 
                                                x__5052 <= x_1])
                           -> (x__5050 x__5051 x__5052 x__5053 x__5054))))
                       (fun (b_4746:x_1:bool[x_1]) ->
                        (if b_4746 (l0 end) (l1 (fail_3705 true (fun (main_4578:unit) -> end))))))))))))
abstract_term: (rand_int
                 (fun (arg1_4748:int) ->
                  (make_list_1049 arg1_4748
                    (fun (xs_4737:(int -> (bool -> int -> X) -> X)) ->
                     (insertsort_2165
                       (fun (x1_5030:int) (x2_5031:int) (k_main_xsxs_5032:(bool -> int -> bool -> int -> X)) ->
                        (if (x2_5031 = x1_5030)
                          (l0
                            (xs_4737 x1_5030
                              (fun (r1_4815:bool) (r2_4816:int) -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816))))
                          (l1
                            (xs_4737 x1_5030
                              (fun (x1_4822:bool) (x2_4823:int) ->
                               (xs_4737 x2_5031
                                 (fun (x1_4830:bool) (x2_4831:int) ->
                                  (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831))))))))
                       (fun (ysys_4740:(x_1:int ->
                                        x_2:int ->
                                        (x_4:bool ->
                                         x_5:int ->
                                         x_6:bool ->
                                         x_7:int[(not x_4) || (not x_6) || (
                                                 not (x_1 <= x_2)) || 
                                                 x_1 < 0 || x_2 < 0 || 
                                                 x_5 <= x_7]
                                         -> X)
                                        -> X))
                        ->
                        (check_2164
                          (fun (x__5048:int) (x__5049:int) 
                               (x__5050:(x_1:bool ->
                                         x_2:int ->
                                         x_3:bool ->
                                         x_4:int[(not x_1) || (not x_3) || (
                                                 not (x__5048 <= x__5049)) || 
                                                 x__5048 < 0 || x__5049 < 0 || 
                                                 x_2 <= x_4]
                                         -> X))
                           ->
                           (ysys_4740 x__5048 x__5049
                             (fun (x__5051:bool) (x__5052:int) (x__5053:bool) 
                                  (x__5054:x_1:int[(not x__5051) || (
                                                   not x__5053) || (not (x__5048 <= x__5049)) || 
                                                   x__5048 < 0 || x__5049 < 0 || 
                                                   x__5052 <= x_1])
                              -> (x__5050 x__5051 x__5052 x__5053 x__5054))))
                          (fun (b_4746:x_1:bool[x_1]) ->
                           (if b_4746 (l0 end) (l1 (fail_3705 true (fun (main_4578:unit) -> end)))))))))))): X
abstract_term: (fun (arg1_4748:int) ->
                (make_list_1049 arg1_4748
                  (fun (xs_4737:(int -> (bool -> int -> X) -> X)) ->
                   (insertsort_2165
                     (fun (x1_5030:int) (x2_5031:int) (k_main_xsxs_5032:(bool -> int -> bool -> int -> X)) ->
                      (if (x2_5031 = x1_5030)
                        (l0
                          (xs_4737 x1_5030
                            (fun (r1_4815:bool) (r2_4816:int) -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816))))
                        (l1
                          (xs_4737 x1_5030
                            (fun (x1_4822:bool) (x2_4823:int) ->
                             (xs_4737 x2_5031
                               (fun (x1_4830:bool) (x2_4831:int) -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831))))))))
                     (fun (ysys_4740:(x_1:int ->
                                      x_2:int ->
                                      (x_4:bool ->
                                       x_5:int ->
                                       x_6:bool ->
                                       x_7:int[(not x_4) || (not x_6) || (
                                               not (x_1 <= x_2)) || x_1 < 0 || 
                                               x_2 < 0 || x_5 <= x_7]
                                       -> X)
                                      -> X))
                      ->
                      (check_2164
                        (fun (x__5048:int) (x__5049:int) 
                             (x__5050:(x_1:bool ->
                                       x_2:int ->
                                       x_3:bool ->
                                       x_4:int[(not x_1) || (not x_3) || (
                                               not (x__5048 <= x__5049)) || 
                                               x__5048 < 0 || x__5049 < 0 || 
                                               x_2 <= x_4]
                                       -> X))
                         ->
                         (ysys_4740 x__5048 x__5049
                           (fun (x__5051:bool) (x__5052:int) (x__5053:bool) 
                                (x__5054:x_1:int[(not x__5051) || (not x__5053) || (
                                                 not (x__5048 <= x__5049)) || 
                                                 x__5048 < 0 || x__5049 < 0 || 
                                                 x__5052 <= x_1])
                            -> (x__5050 x__5051 x__5052 x__5053 x__5054))))
                        (fun (b_4746:x_1:bool[x_1]) ->
                         (if b_4746 (l0 end) (l1 (fail_3705 true (fun (main_4578:unit) -> end))))))))))): (int ->
X)
abst_arg: arg1_4748, int
abst_arg: arg1_4748, int
abstract_term: (make_list_1049 arg1_4748
                 (fun (xs_4737:(int -> (bool -> int -> X) -> X)) ->
                  (insertsort_2165
                    (fun (x1_5030:int) (x2_5031:int) (k_main_xsxs_5032:(bool -> int -> bool -> int -> X)) ->
                     (if (x2_5031 = x1_5030)
                       (l0
                         (xs_4737 x1_5030
                           (fun (r1_4815:bool) (r2_4816:int) -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816))))
                       (l1
                         (xs_4737 x1_5030
                           (fun (x1_4822:bool) (x2_4823:int) ->
                            (xs_4737 x2_5031
                              (fun (x1_4830:bool) (x2_4831:int) -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831))))))))
                    (fun (ysys_4740:(x_1:int ->
                                     x_2:int ->
                                     (x_4:bool ->
                                      x_5:int ->
                                      x_6:bool ->
                                      x_7:int[(not x_4) || (not x_6) || (
                                              not (x_1 <= x_2)) || x_1 < 0 || 
                                              x_2 < 0 || x_5 <= x_7]
                                      -> X)
                                     -> X))
                     ->
                     (check_2164
                       (fun (x__5048:int) (x__5049:int) 
                            (x__5050:(x_1:bool ->
                                      x_2:int ->
                                      x_3:bool ->
                                      x_4:int[(not x_1) || (not x_3) || (
                                              not (x__5048 <= x__5049)) || 
                                              x__5048 < 0 || x__5049 < 0 || 
                                              x_2 <= x_4]
                                      -> X))
                        ->
                        (ysys_4740 x__5048 x__5049
                          (fun (x__5051:bool) (x__5052:int) (x__5053:bool) 
                               (x__5054:x_1:int[(not x__5051) || (not x__5053) || (
                                                not (x__5048 <= x__5049)) || 
                                                x__5048 < 0 || x__5049 < 0 || 
                                                x__5052 <= x_1])
                           -> (x__5050 x__5051 x__5052 x__5053 x__5054))))
                       (fun (b_4746:x_1:bool[x_1]) ->
                        (if b_4746 (l0 end) (l1 (fail_3705 true (fun (main_4578:unit) -> end)))))))))): X
abstract_term: (fun (xs_4737:(int -> (bool -> int -> X) -> X)) ->
                (insertsort_2165
                  (fun (x1_5030:int) (x2_5031:int) (k_main_xsxs_5032:(bool -> int -> bool -> int -> X)) ->
                   (if (x2_5031 = x1_5030)
                     (l0
                       (xs_4737 x1_5030
                         (fun (r1_4815:bool) (r2_4816:int) -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816))))
                     (l1
                       (xs_4737 x1_5030
                         (fun (x1_4822:bool) (x2_4823:int) ->
                          (xs_4737 x2_5031
                            (fun (x1_4830:bool) (x2_4831:int) -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831))))))))
                  (fun (ysys_4740:(x_1:int ->
                                   x_2:int ->
                                   (x_4:bool ->
                                    x_5:int ->
                                    x_6:bool ->
                                    x_7:int[(not x_4) || (not x_6) || (
                                            not (x_1 <= x_2)) || x_1 < 0 || 
                                            x_2 < 0 || x_5 <= x_7]
                                    -> X)
                                   -> X))
                   ->
                   (check_2164
                     (fun (x__5048:int) (x__5049:int) 
                          (x__5050:(x_1:bool ->
                                    x_2:int ->
                                    x_3:bool ->
                                    x_4:int[(not x_1) || (not x_3) || (
                                            not (x__5048 <= x__5049)) || 
                                            x__5048 < 0 || x__5049 < 0 || 
                                            x_2 <= x_4]
                                    -> X))
                      ->
                      (ysys_4740 x__5048 x__5049
                        (fun (x__5051:bool) (x__5052:int) (x__5053:bool) 
                             (x__5054:x_1:int[(not x__5051) || (not x__5053) || (
                                              not (x__5048 <= x__5049)) || 
                                              x__5048 < 0 || x__5049 < 0 || 
                                              x__5052 <= x_1])
                         -> (x__5050 x__5051 x__5052 x__5053 x__5054))))
                     (fun (b_4746:x_1:bool[x_1]) ->
                      (if b_4746 (l0 end) (l1 (fail_3705 true (fun (main_4578:unit) -> end))))))))): ((
int -> (bool -> int -> X) -> X) ->
X)
abst_arg: xs_4737, (int -> (bool -> int -> X) -> X)
abst_arg: xs_4737, (int -> (bool -> int -> X) -> X)
abstract_term: (insertsort_2165
                 (fun (x1_5030:int) (x2_5031:int) (k_main_xsxs_5032:(bool -> int -> bool -> int -> X)) ->
                  (if (x2_5031 = x1_5030)
                    (l0
                      (xs_4737 x1_5030
                        (fun (r1_4815:bool) (r2_4816:int) -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816))))
                    (l1
                      (xs_4737 x1_5030
                        (fun (x1_4822:bool) (x2_4823:int) ->
                         (xs_4737 x2_5031
                           (fun (x1_4830:bool) (x2_4831:int) -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831))))))))
                 (fun (ysys_4740:(x_1:int ->
                                  x_2:int ->
                                  (x_4:bool ->
                                   x_5:int ->
                                   x_6:bool ->
                                   x_7:int[(not x_4) || (not x_6) || (
                                           not (x_1 <= x_2)) || x_1 < 0 || 
                                           x_2 < 0 || x_5 <= x_7]
                                   -> X)
                                  -> X))
                  ->
                  (check_2164
                    (fun (x__5048:int) (x__5049:int) 
                         (x__5050:(x_1:bool ->
                                   x_2:int ->
                                   x_3:bool ->
                                   x_4:int[(not x_1) || (not x_3) || (
                                           not (x__5048 <= x__5049)) || 
                                           x__5048 < 0 || x__5049 < 0 || 
                                           x_2 <= x_4]
                                   -> X))
                     ->
                     (ysys_4740 x__5048 x__5049
                       (fun (x__5051:bool) (x__5052:int) (x__5053:bool) 
                            (x__5054:x_1:int[(not x__5051) || (not x__5053) || (
                                             not (x__5048 <= x__5049)) || 
                                             x__5048 < 0 || x__5049 < 0 || 
                                             x__5052 <= x_1])
                        -> (x__5050 x__5051 x__5052 x__5053 x__5054))))
                    (fun (b_4746:x_1:bool[x_1]) ->
                     (if b_4746 (l0 end) (l1 (fail_3705 true (fun (main_4578:unit) -> end)))))))): X
abstract_term: (fun (ysys_4740:(x_1:int ->
                                x_2:int ->
                                (x_4:bool ->
                                 x_5:int ->
                                 x_6:bool ->
                                 x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || 
                                         x_1 < 0 || x_2 < 0 || x_5 <= x_7]
                                 -> X)
                                -> X))
                ->
                (check_2164
                  (fun (x__5048:int) (x__5049:int) 
                       (x__5050:(x_1:bool ->
                                 x_2:int ->
                                 x_3:bool ->
                                 x_4:int[(not x_1) || (not x_3) || (not (x__5048 <= x__5049)) || 
                                         x__5048 < 0 || x__5049 < 0 || 
                                         x_2 <= x_4]
                                 -> X))
                   ->
                   (ysys_4740 x__5048 x__5049
                     (fun (x__5051:bool) (x__5052:int) (x__5053:bool) 
                          (x__5054:x_1:int[(not x__5051) || (not x__5053) || (
                                           not (x__5048 <= x__5049)) || 
                                           x__5048 < 0 || x__5049 < 0 || 
                                           x__5052 <= x_1])
                      -> (x__5050 x__5051 x__5052 x__5053 x__5054))))
                  (fun (b_4746:x_1:bool[x_1]) ->
                   (if b_4746 (l0 end) (l1 (fail_3705 true (fun (main_4578:unit) -> end))))))): ((
x_2:int ->
x_3:int ->
(x_5:bool ->
 x_6:int -> x_7:bool -> x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_2 < 0 || x_3 < 0 || x_6 <= x_8] -> X)
-> X) ->
X)
abst_arg: ysys_4740, (x_1:int ->
                      x_2:int ->
                      (x_4:bool ->
                       x_5:int ->
                       x_6:bool ->
                       x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
                      -> X)
abst_arg: ysys_4740, (x_1:int ->
                      x_2:int ->
                      (x_4:bool ->
                       x_5:int ->
                       x_6:bool ->
                       x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
                      -> X)
abstract_term: (check_2164
                 (fun (x__5048:int) (x__5049:int) 
                      (x__5050:(x_1:bool ->
                                x_2:int ->
                                x_3:bool ->
                                x_4:int[(not x_1) || (not x_3) || (not (x__5048 <= x__5049)) || 
                                        x__5048 < 0 || x__5049 < 0 || 
                                        x_2 <= x_4]
                                -> X))
                  ->
                  (ysys_4740 x__5048 x__5049
                    (fun (x__5051:bool) (x__5052:int) (x__5053:bool) 
                         (x__5054:x_1:int[(not x__5051) || (not x__5053) || (
                                          not (x__5048 <= x__5049)) || 
                                          x__5048 < 0 || x__5049 < 0 || 
                                          x__5052 <= x_1])
                     -> (x__5050 x__5051 x__5052 x__5053 x__5054))))
                 (fun (b_4746:x_1:bool[x_1]) ->
                  (if b_4746 (l0 end) (l1 (fail_3705 true (fun (main_4578:unit) -> end)))))): X
abstract_term: (fun (b_4746:x_1:bool[x_1]) -> (if b_4746 (l0 end) (l1 (fail_3705 true (fun (main_4578:unit) -> end))))): (x_1:bool[
x_1] ->
X)
abst_arg: b_4746, x_1:bool[x_1]
abst_arg: b_4746, x_1:bool[x_1]
abstract_term: (if b_4746 (l0 end) (l1 (fail_3705 true (fun (main_4578:unit) -> end)))): X
abstract_term: b_4746: x_1:bool[x_1]
cond: true
pbs: b_4746 := b_4746
p:b_4746
tt:b_4746
ff:false

abstract_term: (l0 end): X
abstract_term: end: X
abstract_term: (l1 (fail_3705 true (fun (main_4578:unit) -> end))): X
abstract_term: (fail_3705 true (fun (main_4578:unit) -> end)): X
abstract_term: (fun (main_4578:unit) -> end): (unit ->
X)
abst_arg: main_4578, unit
abst_arg: main_4578, unit
abstract_term: end: X
abstract_term: true: bool
abstract_term: (fun (x__5048:int) (x__5049:int) 
                    (x__5050:(x_1:bool ->
                              x_2:int ->
                              x_3:bool ->
                              x_4:int[(not x_1) || (not x_3) || (not (x__5048 <= x__5049)) || 
                                      x__5048 < 0 || x__5049 < 0 || x_2 <= x_4]
                              -> X))
                ->
                (ysys_4740 x__5048 x__5049
                  (fun (x__5051:bool) (x__5052:int) (x__5053:bool) 
                       (x__5054:x_1:int[(not x__5051) || (not x__5053) || (
                                        not (x__5048 <= x__5049)) || 
                                        x__5048 < 0 || x__5049 < 0 || 
                                        x__5052 <= x_1])
                   -> (x__5050 x__5051 x__5052 x__5053 x__5054)))): (
x_1:int ->
x_2:int ->
(x_4:bool ->
 x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
-> X)
abst_arg: x__5048, int
abst_arg: x__5049, int
abst_arg: x__5050, (x_1:bool ->
                    x_2:int ->
                    x_3:bool ->
                    x_4:int[(not x_1) || (not x_3) || (not (x__5048 <= x__5049)) || 
                            x__5048 < 0 || x__5049 < 0 || x_2 <= x_4]
                    -> X)
abst_arg: x__5048, int
abst_arg: x__5049, int
abst_arg: x__5050, (x_1:bool ->
                    x_2:int ->
                    x_3:bool ->
                    x_4:int[(not x_1) || (not x_3) || (not (x__5048 <= x__5049)) || 
                            x__5048 < 0 || x__5049 < 0 || x_2 <= x_4]
                    -> X)
abstract_term: (ysys_4740 x__5048 x__5049
                 (fun (x__5051:bool) (x__5052:int) (x__5053:bool) 
                      (x__5054:x_1:int[(not x__5051) || (not x__5053) || (
                                       not (x__5048 <= x__5049)) || x__5048 < 0 || 
                                       x__5049 < 0 || x__5052 <= x_1])
                  -> (x__5050 x__5051 x__5052 x__5053 x__5054))): X
abstract_term: (fun (x__5051:bool) (x__5052:int) (x__5053:bool) 
                    (x__5054:x_1:int[(not x__5051) || (not x__5053) || (
                                     not (x__5048 <= x__5049)) || x__5048 < 0 || 
                                     x__5049 < 0 || x__5052 <= x_1])
                -> (x__5050 x__5051 x__5052 x__5053 x__5054)): (x_1:bool ->
                                                                x_2:int ->
                                                                x_3:bool ->
                                                                x_4:int[
                                                                (not x_1) || (
                                                                not x_3) || (
                                                                not (x__5048 <= x__5049)) || 
                                                                x__5048 < 0 || 
                                                                x__5049 < 0 || 
                                                                x_2 <= x_4] -> X)
abst_arg: x__5051, bool
abst_arg: x__5052, int
abst_arg: x__5053, bool
abst_arg: x__5054, x_1:int[(not x__5051) || (not x__5053) || (not (x__5048 <= x__5049)) || 
                           x__5048 < 0 || x__5049 < 0 || x__5052 <= x_1]
abst_arg: x__5051, bool
abst_arg: x__5052, int
abst_arg: x__5053, bool
abst_arg: x__5054, x_1:int[(not x__5051) || (not x__5053) || (not (x__5048 <= x__5049)) || 
                           x__5048 < 0 || x__5049 < 0 || x__5052 <= x_1]
abstract_term: (x__5050 x__5051 x__5052 x__5053 x__5054): X
abstract_term: x__5054: x_1:int[(not x__5051) || (not x__5053) || (not (x__5048 <= x__5049)) || 
                                x__5048 < 0 || x__5049 < 0 || x__5052 <= x_1]
cond: true
pbs: x__5054 := ((((((not x__5051) || (not x__5053)) || (not (x__5048 <= x__5049))) || (x__5048 < 0)) || (x__5049 < 0))
                 || (x__5052 <= x__5054))
p:((((((not x__5051) || (not x__5053)) || (not (x__5048 <= x__5049))) || (x__5048 < 0)) || (x__5049 < 0)) ||
   (x__5052 <= x__5054))
tt:x__5054
ff:false

abstract_term: x__5053: bool
abstract_term: x__5052: int
abstract_term: x__5051: bool
abstract_term: x__5049: int
abstract_term: x__5048: int
abstract_term: (fun (x1_5030:int) (x2_5031:int) (k_main_xsxs_5032:(bool -> int -> bool -> int -> X)) ->
                (if (x2_5031 = x1_5030)
                  (l0
                    (xs_4737 x1_5030
                      (fun (r1_4815:bool) (r2_4816:int) -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816))))
                  (l1
                    (xs_4737 x1_5030
                      (fun (x1_4822:bool) (x2_4823:int) ->
                       (xs_4737 x2_5031
                         (fun (x1_4830:bool) (x2_4831:int) -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831)))))))): (
int -> int -> (bool -> int -> bool -> int -> X) -> X)
abst_arg: x1_5030, int
abst_arg: x2_5031, int
abst_arg: k_main_xsxs_5032, (bool -> int -> bool -> int -> X)
abst_arg: x1_5030, int
abst_arg: x2_5031, int
abst_arg: k_main_xsxs_5032, (bool -> int -> bool -> int -> X)
abstract_term: (if (x2_5031 = x1_5030)
                 (l0
                   (xs_4737 x1_5030
                     (fun (r1_4815:bool) (r2_4816:int) -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816))))
                 (l1
                   (xs_4737 x1_5030
                     (fun (x1_4822:bool) (x2_4823:int) ->
                      (xs_4737 x2_5031
                        (fun (x1_4830:bool) (x2_4831:int) -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831))))))): X
abstract_term: (x2_5031 = x1_5030): x_1:bool[x_1]
cond: true
pbs: 
p:(x2_5031 = x1_5030)
tt:false
ff:false

abstract_term: (l0
                 (xs_4737 x1_5030
                   (fun (r1_4815:bool) (r2_4816:int) -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816)))): X
abstract_term: (xs_4737 x1_5030
                 (fun (r1_4815:bool) (r2_4816:int) -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816))): X
abstract_term: (fun (r1_4815:bool) (r2_4816:int) -> (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816)): (
bool -> int -> X)
abst_arg: r1_4815, bool
abst_arg: r2_4816, int
abst_arg: r1_4815, bool
abst_arg: r2_4816, int
abstract_term: (k_main_xsxs_5032 r1_4815 r2_4816 r1_4815 r2_4816): X
abstract_term: r2_4816: int
abstract_term: r1_4815: bool
abstract_term: r2_4816: int
abstract_term: r1_4815: bool
abstract_term: x1_5030: int
abstract_term: (l1
                 (xs_4737 x1_5030
                   (fun (x1_4822:bool) (x2_4823:int) ->
                    (xs_4737 x2_5031
                      (fun (x1_4830:bool) (x2_4831:int) -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831)))))): X
abstract_term: (xs_4737 x1_5030
                 (fun (x1_4822:bool) (x2_4823:int) ->
                  (xs_4737 x2_5031
                    (fun (x1_4830:bool) (x2_4831:int) -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831))))): X
abstract_term: (fun (x1_4822:bool) (x2_4823:int) ->
                (xs_4737 x2_5031
                  (fun (x1_4830:bool) (x2_4831:int) -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831)))): (
bool -> int -> X)
abst_arg: x1_4822, bool
abst_arg: x2_4823, int
abst_arg: x1_4822, bool
abst_arg: x2_4823, int
abstract_term: (xs_4737 x2_5031
                 (fun (x1_4830:bool) (x2_4831:int) -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831))): X
abstract_term: (fun (x1_4830:bool) (x2_4831:int) -> (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831)): (
bool -> int -> X)
abst_arg: x1_4830, bool
abst_arg: x2_4831, int
abst_arg: x1_4830, bool
abst_arg: x2_4831, int
abstract_term: (k_main_xsxs_5032 x1_4822 x2_4823 x1_4830 x2_4831): X
abstract_term: x2_4831: int
abstract_term: x1_4830: bool
abstract_term: x2_4823: int
abstract_term: x1_4822: bool
abstract_term: x2_5031: int
abstract_term: x1_5030: int
abstract_term: arg1_4748: int
check_2164: ENV: xsxs_1056:(x_1:int ->
                            x_2:int ->
                            (x_4:bool ->
                             x_5:int ->
                             x_6:bool ->
                             x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7]
                             -> X)
                            -> X), k_check_3093:(x_1:bool[x_1] -> X),


abst_arg: xsxs_1056, (x_1:int ->
                      x_2:int ->
                      (x_4:bool ->
                       x_5:int ->
                       x_6:bool ->
                       x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
                      -> X)
abst_arg: k_check_3093, (x_1:bool[x_1] ->
X)
abst_arg: xsxs_1056, (x_1:int ->
                      x_2:int ->
                      (x_4:bool ->
                       x_5:int ->
                       x_6:bool ->
                       x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
                      -> X)
abst_arg: k_check_3093, (x_1:bool[x_1] ->
X)
check_2164: (xsxs_1056 0 0
              (fun (xs11_4587:bool) (xs12_4588:int) (xs21_4589:bool) 
                   (xs22_4590:x_1:int[(not xs11_4587) || (not xs21_4589) || (
                                      not (0 <= 0)) || 0 < 0 || 0 < 0 || 
                                      xs12_4588 <= x_1])
               ->
               (if xs11_4587
                 (l1
                   (if xs21_4589
                     (l1
                       (if (xs22_4590 >= xs12_4588)
                         (l0
                           (check_2164
                             (fun (x1_4978:int) (x2_4979:int) 
                                  (k_check_xs'xs'_4980:(x_1:bool ->
                                                        x_2:int ->
                                                        x_3:bool ->
                                                        x_4:int[(not x_1) || (
                                                                not x_3) || (
                                                                not (x1_4978 <= x2_4979)) || 
                                                                x1_4978 < 0 || 
                                                                x2_4979 < 0 || 
                                                                x_2 <= x_4]
                                                        -> X))
                              ->
                              (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1)
                                (fun (x__5056:bool) (x__5057:int) (x__5058:bool) 
                                     (x__5059:x_1:int[(not x__5056) || (
                                                      not x__5058) || (
                                                      not ((x1_4978 + 1) <= (x2_4979 + 1))) || 
                                                      x1_4978 + 1 < 0 || 
                                                      x2_4979 + 1 < 0 || 
                                                      x__5057 <= x_1])
                                 -> (k_check_xs'xs'_4980 x__5056 x__5057 x__5058 x__5059))))
                             (fun (x__5055:x_1:bool[x_1]) -> (k_check_3093 x__5055)))) (
                         l1 (k_check_3093 false)))) (l0 (k_check_3093 true)))) (
                 l0 (k_check_3093 true))))) ===> (xsxs_1056 0 0
                                                   (fun (xs11_4587:bool) (xs12_4588:int) (xs21_4589:bool) 
                                                        (xs22_4590:x_1:int[
                                                        (not xs11_4587) || (
                                                        not xs21_4589) || (
                                                        not (0 <= 0)) || 
                                                        0 < 0 || 0 < 0 || 
                                                        xs12_4588 <= x_1])
                                                    ->
                                                    (if xs11_4587
                                                      (l1
                                                        (if xs21_4589
                                                          (l1
                                                            (if (xs22_4590 >= xs12_4588)
                                                              (l0
                                                                (check_2164
                                                                  (fun 
                                                                   (x1_4978:int) (x2_4979:int) 
                                                                   (k_check_xs'xs'_4980:(
                                                                   x_1:bool ->
                                                                   x_2:int ->
                                                                   x_3:bool ->
                                                                   x_4:int[
                                                                   (not x_1) || (
                                                                   not x_3) || (
                                                                   not (x1_4978 <= x2_4979)) || 
                                                                   x1_4978 < 0 || 
                                                                   x2_4979 < 0 || 
                                                                   x_2 <= x_4] -> X)) ->
                                                                   (xsxs_1056 (
                                                                    x1_4978 + 1) (
                                                                    x2_4979 + 1)
                                                                    (fun 
                                                                    (x__5056:bool) (x__5057:int) (x__5058:bool) 
                                                                    (x__5059:x_1:int[
                                                                    (
                                                                    not x__5056) || (
                                                                    not x__5058) || 
                                                                    (
                                                                    not ((x1_4978 + 1) <= (x2_4979 + 1))) || 
                                                                    x1_4978 + 1 < 0 || 
                                                                    x2_4979 + 1 < 0 || 
                                                                    x__5057 <= x_1]) ->
                                                                    (k_check_xs'xs'_4980 x__5056 x__5057 x__5058
                                                                    x__5059))))
                                                                  (fun 
                                                                   (x__5055:x_1:bool[x_1]) -> (
                                                                   k_check_3093 x__5055)))) (
                                                              l1 (k_check_3093 false)))) (
                                                          l0 (k_check_3093 true)))) (
                                                      l0 (k_check_3093 true)))))
check_2164:: (xsxs_1056 0 0
               (fun (xs11_4587:bool) (xs12_4588:int) (xs21_4589:bool) 
                    (xs22_4590:x_1:int[(not xs11_4587) || (not xs21_4589) || (
                                       not (0 <= 0)) || 0 < 0 || 0 < 0 || 
                                       xs12_4588 <= x_1])
                ->
                (if xs11_4587
                  (l1
                    (if xs21_4589
                      (l1
                        (if (xs22_4590 >= xs12_4588)
                          (l0
                            (check_2164
                              (fun (x1_4978:int) (x2_4979:int) 
                                   (k_check_xs'xs'_4980:(x_1:bool ->
                                                         x_2:int ->
                                                         x_3:bool ->
                                                         x_4:int[(not x_1) || (
                                                                 not x_3) || (
                                                                 not (x1_4978 <= x2_4979)) || 
                                                                 x1_4978 < 0 || 
                                                                 x2_4979 < 0 || 
                                                                 x_2 <= x_4]
                                                         -> X))
                               ->
                               (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1)
                                 (fun (x__5056:bool) (x__5057:int) (x__5058:bool) 
                                      (x__5059:x_1:int[(not x__5056) || (
                                                       not x__5058) || (
                                                       not ((x1_4978 + 1) <= (x2_4979 + 1))) || 
                                                       x1_4978 + 1 < 0 || 
                                                       x2_4979 + 1 < 0 || 
                                                       x__5057 <= x_1])
                                  -> (k_check_xs'xs'_4980 x__5056 x__5057 x__5058 x__5059))))
                              (fun (x__5055:x_1:bool[x_1]) -> (k_check_3093 x__5055)))) (
                          l1 (k_check_3093 false)))) (l0 (k_check_3093 true)))) (
                  l0 (k_check_3093 true)))))
abstract_term: (xsxs_1056 0 0
                 (fun (xs11_4587:bool) (xs12_4588:int) (xs21_4589:bool) 
                      (xs22_4590:x_1:int[(not xs11_4587) || (not xs21_4589) || (
                                         not (0 <= 0)) || 0 < 0 || 0 < 0 || 
                                         xs12_4588 <= x_1])
                  ->
                  (if xs11_4587
                    (l1
                      (if xs21_4589
                        (l1
                          (if (xs22_4590 >= xs12_4588)
                            (l0
                              (check_2164
                                (fun (x1_4978:int) (x2_4979:int) 
                                     (k_check_xs'xs'_4980:(x_1:bool ->
                                                           x_2:int ->
                                                           x_3:bool ->
                                                           x_4:int[(not x_1) || (
                                                                   not x_3) || (
                                                                   not (x1_4978 <= x2_4979)) || 
                                                                   x1_4978 < 0 || 
                                                                   x2_4979 < 0 || 
                                                                   x_2 <= x_4]
                                                           -> X))
                                 ->
                                 (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1)
                                   (fun (x__5056:bool) (x__5057:int) (x__5058:bool) 
                                        (x__5059:x_1:int[(not x__5056) || (
                                                         not x__5058) || (
                                                         not ((x1_4978 + 1) <= (x2_4979 + 1))) || 
                                                         x1_4978 + 1 < 0 || 
                                                         x2_4979 + 1 < 0 || 
                                                         x__5057 <= x_1])
                                    -> (k_check_xs'xs'_4980 x__5056 x__5057 x__5058 x__5059))))
                                (fun (x__5055:x_1:bool[x_1]) -> (k_check_3093 x__5055)))) (
                            l1 (k_check_3093 false)))) (l0 (k_check_3093 true)))) (
                    l0 (k_check_3093 true))))): X
abstract_term: (fun (xs11_4587:bool) (xs12_4588:int) (xs21_4589:bool) 
                    (xs22_4590:x_1:int[(not xs11_4587) || (not xs21_4589) || (
                                       not (0 <= 0)) || 0 < 0 || 0 < 0 || 
                                       xs12_4588 <= x_1])
                ->
                (if xs11_4587
                  (l1
                    (if xs21_4589
                      (l1
                        (if (xs22_4590 >= xs12_4588)
                          (l0
                            (check_2164
                              (fun (x1_4978:int) (x2_4979:int) 
                                   (k_check_xs'xs'_4980:(x_1:bool ->
                                                         x_2:int ->
                                                         x_3:bool ->
                                                         x_4:int[(not x_1) || (
                                                                 not x_3) || (
                                                                 not (x1_4978 <= x2_4979)) || 
                                                                 x1_4978 < 0 || 
                                                                 x2_4979 < 0 || 
                                                                 x_2 <= x_4]
                                                         -> X))
                               ->
                               (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1)
                                 (fun (x__5056:bool) (x__5057:int) (x__5058:bool) 
                                      (x__5059:x_1:int[(not x__5056) || (
                                                       not x__5058) || (
                                                       not ((x1_4978 + 1) <= (x2_4979 + 1))) || 
                                                       x1_4978 + 1 < 0 || 
                                                       x2_4979 + 1 < 0 || 
                                                       x__5057 <= x_1])
                                  -> (k_check_xs'xs'_4980 x__5056 x__5057 x__5058 x__5059))))
                              (fun (x__5055:x_1:bool[x_1]) -> (k_check_3093 x__5055)))) (
                          l1 (k_check_3093 false)))) (l0 (k_check_3093 true)))) (
                  l0 (k_check_3093 true)))): (x_1:bool ->
                                              x_2:int ->
                                              x_3:bool ->
                                              x_4:int[(not x_1) || (not x_3) || (
                                                      not (0 <= 0)) || 
                                                      0 < 0 || 0 < 0 || 
                                                      x_2 <= x_4]
                                              -> X)
abst_arg: xs11_4587, bool
abst_arg: xs12_4588, int
abst_arg: xs21_4589, bool
abst_arg: xs22_4590, x_1:int[(not xs11_4587) || (not xs21_4589) || (not (0 <= 0)) || 0 < 0 || 0 < 0 || xs12_4588 <= x_1]
abst_arg: xs11_4587, bool
abst_arg: xs12_4588, int
abst_arg: xs21_4589, bool
abst_arg: xs22_4590, x_1:int[(not xs11_4587) || (not xs21_4589) || (not (0 <= 0)) || 0 < 0 || 0 < 0 || xs12_4588 <= x_1]
abstract_term: (if xs11_4587
                 (l1
                   (if xs21_4589
                     (l1
                       (if (xs22_4590 >= xs12_4588)
                         (l0
                           (check_2164
                             (fun (x1_4978:int) (x2_4979:int) 
                                  (k_check_xs'xs'_4980:(x_1:bool ->
                                                        x_2:int ->
                                                        x_3:bool ->
                                                        x_4:int[(not x_1) || (
                                                                not x_3) || (
                                                                not (x1_4978 <= x2_4979)) || 
                                                                x1_4978 < 0 || 
                                                                x2_4979 < 0 || 
                                                                x_2 <= x_4]
                                                        -> X))
                              ->
                              (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1)
                                (fun (x__5056:bool) (x__5057:int) (x__5058:bool) 
                                     (x__5059:x_1:int[(not x__5056) || (
                                                      not x__5058) || (
                                                      not ((x1_4978 + 1) <= (x2_4979 + 1))) || 
                                                      x1_4978 + 1 < 0 || 
                                                      x2_4979 + 1 < 0 || 
                                                      x__5057 <= x_1])
                                 -> (k_check_xs'xs'_4980 x__5056 x__5057 x__5058 x__5059))))
                             (fun (x__5055:x_1:bool[x_1]) -> (k_check_3093 x__5055)))) (
                         l1 (k_check_3093 false)))) (l0 (k_check_3093 true)))) (
                 l0 (k_check_3093 true))): X
abstract_term: xs11_4587: x_1:bool[x_1]
cond: true
pbs: xs22_4590 := ((((((not xs11_4587) || (not xs21_4589)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                   (xs12_4588 <= xs22_4590))
p:xs11_4587
tt:false
ff:false

abstract_term: (l1
                 (if xs21_4589
                   (l1
                     (if (xs22_4590 >= xs12_4588)
                       (l0
                         (check_2164
                           (fun (x1_4978:int) (x2_4979:int) 
                                (k_check_xs'xs'_4980:(x_1:bool ->
                                                      x_2:int ->
                                                      x_3:bool ->
                                                      x_4:int[(not x_1) || (
                                                              not x_3) || (
                                                              not (x1_4978 <= x2_4979)) || 
                                                              x1_4978 < 0 || 
                                                              x2_4979 < 0 || 
                                                              x_2 <= x_4]
                                                      -> X))
                            ->
                            (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1)
                              (fun (x__5056:bool) (x__5057:int) (x__5058:bool) 
                                   (x__5059:x_1:int[(not x__5056) || (
                                                    not x__5058) || (
                                                    not ((x1_4978 + 1) <= (x2_4979 + 1))) || 
                                                    x1_4978 + 1 < 0 || 
                                                    x2_4979 + 1 < 0 || 
                                                    x__5057 <= x_1])
                               -> (k_check_xs'xs'_4980 x__5056 x__5057 x__5058 x__5059))))
                           (fun (x__5055:x_1:bool[x_1]) -> (k_check_3093 x__5055)))) (
                       l1 (k_check_3093 false)))) (l0 (k_check_3093 true)))): X
abstract_term: (if xs21_4589
                 (l1
                   (if (xs22_4590 >= xs12_4588)
                     (l0
                       (check_2164
                         (fun (x1_4978:int) (x2_4979:int) 
                              (k_check_xs'xs'_4980:(x_1:bool ->
                                                    x_2:int ->
                                                    x_3:bool ->
                                                    x_4:int[(not x_1) || (
                                                            not x_3) || (
                                                            not (x1_4978 <= x2_4979)) || 
                                                            x1_4978 < 0 || 
                                                            x2_4979 < 0 || 
                                                            x_2 <= x_4]
                                                    -> X))
                          ->
                          (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1)
                            (fun (x__5056:bool) (x__5057:int) (x__5058:bool) 
                                 (x__5059:x_1:int[(not x__5056) || (not x__5058) || 
                                                  (not ((x1_4978 + 1) <= (x2_4979 + 1))) || 
                                                  x1_4978 + 1 < 0 || 
                                                  x2_4979 + 1 < 0 || 
                                                  x__5057 <= x_1])
                             -> (k_check_xs'xs'_4980 x__5056 x__5057 x__5058 x__5059))))
                         (fun (x__5055:x_1:bool[x_1]) -> (k_check_3093 x__5055)))) (
                     l1 (k_check_3093 false)))) (l0 (k_check_3093 true))): X
abstract_term: xs21_4589: x_1:bool[x_1]
cond: xs11_4587; true
pbs: xs22_4590 := ((((((not xs11_4587) || (not xs21_4589)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                   (xs12_4588 <= xs22_4590))
p:xs21_4589
tt:false
ff:false

abstract_term: (l1
                 (if (xs22_4590 >= xs12_4588)
                   (l0
                     (check_2164
                       (fun (x1_4978:int) (x2_4979:int) 
                            (k_check_xs'xs'_4980:(x_1:bool ->
                                                  x_2:int ->
                                                  x_3:bool ->
                                                  x_4:int[(not x_1) || (
                                                          not x_3) || (
                                                          not (x1_4978 <= x2_4979)) || 
                                                          x1_4978 < 0 || 
                                                          x2_4979 < 0 || 
                                                          x_2 <= x_4]
                                                  -> X))
                        ->
                        (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1)
                          (fun (x__5056:bool) (x__5057:int) (x__5058:bool) 
                               (x__5059:x_1:int[(not x__5056) || (not x__5058) || 
                                                (not ((x1_4978 + 1) <= (x2_4979 + 1))) || 
                                                x1_4978 + 1 < 0 || x2_4979 + 1 < 0 || 
                                                x__5057 <= x_1])
                           -> (k_check_xs'xs'_4980 x__5056 x__5057 x__5058 x__5059))))
                       (fun (x__5055:x_1:bool[x_1]) -> (k_check_3093 x__5055)))) (
                   l1 (k_check_3093 false)))): X
abstract_term: (if (xs22_4590 >= xs12_4588)
                 (l0
                   (check_2164
                     (fun (x1_4978:int) (x2_4979:int) 
                          (k_check_xs'xs'_4980:(x_1:bool ->
                                                x_2:int ->
                                                x_3:bool ->
                                                x_4:int[(not x_1) || (
                                                        not x_3) || (
                                                        not (x1_4978 <= x2_4979)) || 
                                                        x1_4978 < 0 || 
                                                        x2_4979 < 0 || 
                                                        x_2 <= x_4]
                                                -> X))
                      ->
                      (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1)
                        (fun (x__5056:bool) (x__5057:int) (x__5058:bool) 
                             (x__5059:x_1:int[(not x__5056) || (not x__5058) || 
                                              (not ((x1_4978 + 1) <= (x2_4979 + 1))) || 
                                              x1_4978 + 1 < 0 || x2_4979 + 1 < 0 || 
                                              x__5057 <= x_1])
                         -> (k_check_xs'xs'_4980 x__5056 x__5057 x__5058 x__5059))))
                     (fun (x__5055:x_1:bool[x_1]) -> (k_check_3093 x__5055)))) (
                 l1 (k_check_3093 false))): X
abstract_term: (xs22_4590 >= xs12_4588): x_1:bool[x_1]
cond: xs21_4589; xs11_4587; true
pbs: xs22_4590 := ((((((not xs11_4587) || (not xs21_4589)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                   (xs12_4588 <= xs22_4590))
p:(xs22_4590 >= xs12_4588)
tt:xs22_4590
ff:false

abstract_term: (l0
                 (check_2164
                   (fun (x1_4978:int) (x2_4979:int) 
                        (k_check_xs'xs'_4980:(x_1:bool ->
                                              x_2:int ->
                                              x_3:bool ->
                                              x_4:int[(not x_1) || (not x_3) || (
                                                      not (x1_4978 <= x2_4979)) || 
                                                      x1_4978 < 0 || 
                                                      x2_4979 < 0 || 
                                                      x_2 <= x_4]
                                              -> X))
                    ->
                    (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1)
                      (fun (x__5056:bool) (x__5057:int) (x__5058:bool) 
                           (x__5059:x_1:int[(not x__5056) || (not x__5058) || 
                                            (not ((x1_4978 + 1) <= (x2_4979 + 1))) || 
                                            x1_4978 + 1 < 0 || x2_4979 + 1 < 0 || 
                                            x__5057 <= x_1])
                       -> (k_check_xs'xs'_4980 x__5056 x__5057 x__5058 x__5059))))
                   (fun (x__5055:x_1:bool[x_1]) -> (k_check_3093 x__5055)))): X
abstract_term: (check_2164
                 (fun (x1_4978:int) (x2_4979:int) 
                      (k_check_xs'xs'_4980:(x_1:bool ->
                                            x_2:int ->
                                            x_3:bool ->
                                            x_4:int[(not x_1) || (not x_3) || (
                                                    not (x1_4978 <= x2_4979)) || 
                                                    x1_4978 < 0 || x2_4979 < 0 || 
                                                    x_2 <= x_4]
                                            -> X))
                  ->
                  (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1)
                    (fun (x__5056:bool) (x__5057:int) (x__5058:bool) 
                         (x__5059:x_1:int[(not x__5056) || (not x__5058) || (
                                          not ((x1_4978 + 1) <= (x2_4979 + 1))) || 
                                          x1_4978 + 1 < 0 || x2_4979 + 1 < 0 || 
                                          x__5057 <= x_1])
                     -> (k_check_xs'xs'_4980 x__5056 x__5057 x__5058 x__5059))))
                 (fun (x__5055:x_1:bool[x_1]) -> (k_check_3093 x__5055))): X
abstract_term: (fun (x__5055:x_1:bool[x_1]) -> (k_check_3093 x__5055)): (x_1:bool[
x_1] ->
X)
abst_arg: x__5055, x_1:bool[x_1]
abst_arg: x__5055, x_1:bool[x_1]
abstract_term: (k_check_3093 x__5055): X
abstract_term: x__5055: x_1:bool[x_1]
cond: (xs22_4590 >= xs12_4588); xs21_4589; xs11_4587; true
pbs: x__5055 := x__5055;
     xs22_4590 := ((((((not xs11_4587) || (not xs21_4589)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                   (xs12_4588 <= xs22_4590))
p:x__5055
tt:x__5055
ff:false

abstract_term: (fun (x1_4978:int) (x2_4979:int) 
                    (k_check_xs'xs'_4980:(x_1:bool ->
                                          x_2:int ->
                                          x_3:bool ->
                                          x_4:int[(not x_1) || (not x_3) || (
                                                  not (x1_4978 <= x2_4979)) || 
                                                  x1_4978 < 0 || x2_4979 < 0 || 
                                                  x_2 <= x_4]
                                          -> X))
                ->
                (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1)
                  (fun (x__5056:bool) (x__5057:int) (x__5058:bool) 
                       (x__5059:x_1:int[(not x__5056) || (not x__5058) || (
                                        not ((x1_4978 + 1) <= (x2_4979 + 1))) || 
                                        x1_4978 + 1 < 0 || x2_4979 + 1 < 0 || 
                                        x__5057 <= x_1])
                   -> (k_check_xs'xs'_4980 x__5056 x__5057 x__5058 x__5059)))): (
x_1:int ->
x_2:int ->
(x_4:bool ->
 x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
-> X)
abst_arg: x1_4978, int
abst_arg: x2_4979, int
abst_arg: k_check_xs'xs'_4980, (x_1:bool ->
                                x_2:int ->
                                x_3:bool ->
                                x_4:int[(not x_1) || (not x_3) || (not (x1_4978 <= x2_4979)) || 
                                        x1_4978 < 0 || x2_4979 < 0 || 
                                        x_2 <= x_4]
                                -> X)
abst_arg: x1_4978, int
abst_arg: x2_4979, int
abst_arg: k_check_xs'xs'_4980, (x_1:bool ->
                                x_2:int ->
                                x_3:bool ->
                                x_4:int[(not x_1) || (not x_3) || (not (x1_4978 <= x2_4979)) || 
                                        x1_4978 < 0 || x2_4979 < 0 || 
                                        x_2 <= x_4]
                                -> X)
abstract_term: (xsxs_1056 (x1_4978 + 1) (x2_4979 + 1)
                 (fun (x__5056:bool) (x__5057:int) (x__5058:bool) 
                      (x__5059:x_1:int[(not x__5056) || (not x__5058) || (
                                       not ((x1_4978 + 1) <= (x2_4979 + 1))) || 
                                       x1_4978 + 1 < 0 || x2_4979 + 1 < 0 || 
                                       x__5057 <= x_1])
                  -> (k_check_xs'xs'_4980 x__5056 x__5057 x__5058 x__5059))): X
abstract_term: (fun (x__5056:bool) (x__5057:int) (x__5058:bool) 
                    (x__5059:x_1:int[(not x__5056) || (not x__5058) || (
                                     not ((x1_4978 + 1) <= (x2_4979 + 1))) || 
                                     x1_4978 + 1 < 0 || x2_4979 + 1 < 0 || 
                                     x__5057 <= x_1])
                -> (k_check_xs'xs'_4980 x__5056 x__5057 x__5058 x__5059)): (
x_1:bool ->
x_2:int ->
x_3:bool ->
x_4:int[(not x_1) || (not x_3) || (not ((x1_4978 + 1) <= (x2_4979 + 1))) || 
        x1_4978 + 1 < 0 || x2_4979 + 1 < 0 || x_2 <= x_4]
-> X)
abst_arg: x__5056, bool
abst_arg: x__5057, int
abst_arg: x__5058, bool
abst_arg: x__5059, x_1:int[(not x__5056) || (not x__5058) || (not ((x1_4978 + 1) <= (x2_4979 + 1))) || 
                           x1_4978 + 1 < 0 || x2_4979 + 1 < 0 || x__5057 <= x_1]
abst_arg: x__5056, bool
abst_arg: x__5057, int
abst_arg: x__5058, bool
abst_arg: x__5059, x_1:int[(not x__5056) || (not x__5058) || (not ((x1_4978 + 1) <= (x2_4979 + 1))) || 
                           x1_4978 + 1 < 0 || x2_4979 + 1 < 0 || x__5057 <= x_1]
abstract_term: (k_check_xs'xs'_4980 x__5056 x__5057 x__5058 x__5059): X
abstract_term: x__5059: x_1:int[(not x__5056) || (not x__5058) || (not (x1_4978 <= x2_4979)) || 
                                x1_4978 < 0 || x2_4979 < 0 || x__5057 <= x_1]
cond: (xs22_4590 >= xs12_4588); xs21_4589; xs11_4587; true
pbs: x__5059 := ((((((not x__5056) || (not x__5058)) || (not ((x1_4978 + 1) <= (x2_4979 + 1)))) || ((x1_4978 + 1) < 0))
                  || ((x2_4979 + 1) < 0))
                 || (x__5057 <= x__5059));
     xs22_4590 := ((((((not xs11_4587) || (not xs21_4589)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                   (xs12_4588 <= xs22_4590))
p:((((((not x__5056) || (not x__5058)) || (not (x1_4978 <= x2_4979))) || (x1_4978 < 0)) || (x2_4979 < 0)) ||
   (x__5057 <= x__5059))
tt:x__5059
ff:false

abstract_term: x__5058: bool
abstract_term: x__5057: int
abstract_term: x__5056: bool
abstract_term: (x2_4979 + 1): int
abstract_term: (x1_4978 + 1): int
abstract_term: (l1 (k_check_3093 false)): X
abstract_term: (k_check_3093 false): X
abstract_term: false: x_1:bool[x_1]
cond: (not (xs22_4590 >= xs12_4588)); xs21_4589; xs11_4587; true
pbs: xs22_4590 := ((((((not xs11_4587) || (not xs21_4589)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                   (xs12_4588 <= xs22_4590))
p:false
tt:false
ff:true

abstract_term: (l0 (k_check_3093 true)): X
abstract_term: (k_check_3093 true): X
abstract_term: true: x_1:bool[x_1]
cond: (not xs21_4589); xs11_4587; true
pbs: xs22_4590 := ((((((not xs11_4587) || (not xs21_4589)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                   (xs12_4588 <= xs22_4590))
p:true
tt:true
ff:false

abstract_term: (l0 (k_check_3093 true)): X
abstract_term: (k_check_3093 true): X
abstract_term: true: x_1:bool[x_1]
cond: (not xs11_4587); true
pbs: xs22_4590 := ((((((not xs11_4587) || (not xs21_4589)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                   (xs12_4588 <= xs22_4590))
p:true
tt:true
ff:false

abstract_term: 0: int
abstract_term: 0: int
fail_3705: ENV: b:bool, k:(unit -> X),


abst_arg: b, bool
abst_arg: k, (unit ->
X)
abst_arg: b, bool
abst_arg: k, (unit ->
X)
fail_3705: (k ()) ===> (k ())
fail_3705:: (k ())
abstract_term: (k ()): X
abstract_term: (): unit
insert_1014: ENV: x_1053:int,
ysys_1016:(x_1:int ->
           x_2:int ->
           (x_4:bool ->
            x_5:int ->
            x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
           -> X),
k_insert_2260:((x_2:int ->
                x_3:int ->
                (x_5:bool ->
                 x_6:int ->
                 x_7:bool -> x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_2 < 0 || x_3 < 0 || x_6 <= x_8]
                 -> X)
                -> X) -> X),


abst_arg: x_1053, int
abst_arg: ysys_1016, (x_1:int ->
                      x_2:int ->
                      (x_4:bool ->
                       x_5:int ->
                       x_6:bool ->
                       x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
                      -> X)
abst_arg: k_insert_2260, ((x_2:int ->
                           x_3:int ->
                           (x_5:bool ->
                            x_6:int ->
                            x_7:bool ->
                            x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_2 < 0 || x_3 < 0 || x_6 <= x_8]
                            -> X)
                           -> X) ->
X)
abst_arg: x_1053, int
abst_arg: ysys_1016, (x_1:int ->
                      x_2:int ->
                      (x_4:bool ->
                       x_5:int ->
                       x_6:bool ->
                       x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
                      -> X)
abst_arg: k_insert_2260, ((x_2:int ->
                           x_3:int ->
                           (x_5:bool ->
                            x_6:int ->
                            x_7:bool ->
                            x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_2 < 0 || x_3 < 0 || x_6 <= x_8]
                            -> X)
                           -> X) ->
X)
insert_1014: (ysys_1016 0 0
               (fun (p11_4797:bool) (p12_4798:int) (p21_4799:bool) 
                    (p22_4800:x_1:int[(not p11_4797) || (not p21_4799) || (
                                      not (0 <= 0)) || 0 < 0 || 0 < 0 || 
                                      p12_4798 <= x_1])
                ->
                (if p11_4797
                  (l1
                    (if (x_1053 < p12_4798)
                      (l0
                        (k_insert_2260
                          (fun (x1_4612:int) (x2_4613:int) 
                               (k_insert_4614:(x_1:bool ->
                                               x_2:int ->
                                               x_3:bool ->
                                               x_4:int[(not x_1) || (
                                                       not x_3) || (not (x1_4612 <= x2_4613)) || 
                                                       x1_4612 < 0 || 
                                                       x2_4613 < 0 || 
                                                       x_2 <= x_4]
                                               -> X))
                           ->
                           (if (x1_4612 <= 0)
                             (l0
                               (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
                                 (l1
                                   (ysys_1016 0 (x2_4613 - 1)
                                     (fun (xs11_4631:bool) (xs12_4632:int) (xs21_4633:bool) 
                                          (xs22_4634:x_1:int[(not xs11_4631) || (
                                                             not xs21_4633) || (
                                                             not (0 <= (x2_4613 - 1))) || 
                                                             0 < 0 || 
                                                             x2_4613 - 1 < 0 || 
                                                             xs12_4632 <= x_1])
                                      ->
                                      (if xs21_4633
                                        (l1
                                          (if (1 = x2_4613)
                                            (l0
                                              (if (xs22_4634 = xs12_4632)
                                                (l1
                                                  (if (p11_4797 <=> xs11_4631)
                                                    (l1
                                                      (if (xs12_4632 = p12_4798)
                                                        (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                                        (l0
                                                          (loop_2167 ()
                                                            (fun (x__5116:bool) (x__5117:int) (x__5118:bool) 
                                                                 (x__5119:int)
                                                             -> (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                                                    (l0
                                                      (l0
                                                        (loop_2167 ()
                                                          (fun (x__5112:bool) (x__5113:int) (x__5114:bool) 
                                                               (x__5115:int)
                                                           -> (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))))))
                                                (l0
                                                  (loop_2167 ()
                                                    (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                                                     (k_insert_4614 x__5108 x__5109 x__5110 x__5111))))))
                                            (l1
                                              (l1
                                                (if (p11_4797 <=> xs11_4631)
                                                  (l1
                                                    (if (xs12_4632 = p12_4798)
                                                      (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                                      (l0
                                                        (loop_2167 ()
                                                          (fun (x__5104:bool) (x__5105:int) (x__5106:bool) 
                                                               (x__5107:int)
                                                           -> (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))))
                                                  (l0
                                                    (l0
                                                      (loop_2167 ()
                                                        (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int)
                                                         -> (k_insert_4614 x__5100 x__5101 x__5102 x__5103))))))))))
                                        (l0
                                          (loop_2167 ()
                                            (fun (x__5096:bool) (x__5097:int) (x__5098:bool) (x__5099:int) ->
                                             (k_insert_4614 x__5096 x__5097 x__5098 x__5099))))))))))
                             (l1
                               (if (x2_4613 <= 0)
                                 (l0
                                   (ysys_1016 (x1_4612 - 1) 0
                                     (fun (p11_4641:bool) (p12_4642:int) (p21_4643:bool) 
                                          (p22_4644:x_1:int[(not p11_4641) || (
                                                            not p21_4643) || (
                                                            not ((x1_4612 - 1) <= 0)) || 
                                                            x1_4612 - 1 < 0 || 
                                                            0 < 0 || 
                                                            p12_4642 <= x_1])
                                      -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
                                 (l1
                                   (ysys_1016 (x1_4612 - 1) (x2_4613 - 1)
                                     (fun (x__5092:bool) (x__5093:int) (x__5094:bool) 
                                          (x__5095:x_1:int[(not x__5092) || (
                                                           not x__5094) || (
                                                           not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                                                           x1_4612 - 1 < 0 || 
                                                           x2_4613 - 1 < 0 || 
                                                           x__5093 <= x_1])
                                      -> (k_insert_4614 x__5092 x__5093 x__5094 x__5095))))))))))
                      (l1
                        (insert_1014 x_1053
                          (fun (x1_5042:int) (x2_5043:int) 
                               (k_insert_ys'ys'_5044:(x_1:bool ->
                                                      x_2:int ->
                                                      x_3:bool ->
                                                      x_4:int[(not x_1) || (
                                                              not x_3) || (
                                                              not (x1_5042 <= x2_5043)) || 
                                                              x1_5042 < 0 || 
                                                              x2_5043 < 0 || 
                                                              x_2 <= x_4]
                                                      -> X))
                           ->
                           (ysys_1016 (x1_5042 + 1) (x2_5043 + 1)
                             (fun (x__5088:bool) (x__5089:int) (x__5090:bool) 
                                  (x__5091:x_1:int[(not x__5088) || (
                                                   not x__5090) || (not ((x1_5042 + 1) <= (x2_5043 + 1))) || 
                                                   x1_5042 + 1 < 0 || 
                                                   x2_5043 + 1 < 0 || 
                                                   x__5089 <= x_1])
                              -> (k_insert_ys'ys'_5044 x__5088 x__5089 x__5090 x__5091))))
                          (fun (ys''ys''_4650:(x_1:int ->
                                               x_2:int ->
                                               (x_4:bool ->
                                                x_5:int ->
                                                x_6:bool ->
                                                x_7:int[(not x_4) || (
                                                        not x_6) || (
                                                        not (x_1 <= x_2)) || 
                                                        x_1 < 0 || x_2 < 0 || 
                                                        x_5 <= x_7]
                                                -> X)
                                               -> X))
                           ->
                           (k_insert_2260
                             (fun (x1_4664:int) (x2_4665:int) 
                                  (k_insert_4666:(x_1:bool ->
                                                  x_2:int ->
                                                  x_3:bool ->
                                                  x_4:int[(not x_1) || (
                                                          not x_3) || (
                                                          not (x1_4664 <= x2_4665)) || 
                                                          x1_4664 < 0 || 
                                                          x2_4665 < 0 || 
                                                          x_2 <= x_4]
                                                  -> X))
                              ->
                              (if (x1_4664 <= 0)
                                (l0
                                  (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                                    (l1
                                      (ysys_1016 0 (x2_4665 - 1)
                                        (fun (xs11_4683:bool) (xs12_4684:int) (xs21_4685:bool) 
                                             (xs22_4686:x_1:int[(not xs11_4683) || (
                                                                not xs21_4685) || (
                                                                not (0 <= (x2_4665 - 1))) || 
                                                                0 < 0 || 
                                                                x2_4665 - 1 < 0 || 
                                                                xs12_4684 <= x_1])
                                         ->
                                         (if xs21_4685
                                           (l1
                                             (if (1 = x2_4665)
                                               (l0
                                                 (if (xs22_4686 = xs12_4684)
                                                   (l1
                                                     (if (p11_4797 <=> xs11_4683)
                                                       (l1
                                                         (if (xs12_4684 = p12_4798)
                                                           (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                           (l0
                                                             (loop_2167 ()
                                                               (fun (x__5084:bool) (x__5085:int) (x__5086:bool) 
                                                                    (x__5087:int)
                                                                -> (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                                                       (l0
                                                         (l0
                                                           (loop_2167 ()
                                                             (fun (x__5080:bool) (x__5081:int) (x__5082:bool) 
                                                                  (x__5083:int)
                                                              -> (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                                                   (l0
                                                     (loop_2167 ()
                                                       (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int)
                                                        -> (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))))
                                               (l1
                                                 (l1
                                                   (if (p11_4797 <=> xs11_4683)
                                                     (l1
                                                       (if (xs12_4684 = p12_4798)
                                                         (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                         (l0
                                                           (loop_2167 ()
                                                             (fun (x__5072:bool) (x__5073:int) (x__5074:bool) 
                                                                  (x__5075:int)
                                                              -> (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                                                     (l0
                                                       (l0
                                                         (loop_2167 ()
                                                           (fun (x__5068:bool) (x__5069:int) (x__5070:bool) 
                                                                (x__5071:int)
                                                            -> (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))))))))
                                           (l0
                                             (loop_2167 ()
                                               (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                                                (k_insert_4666 x__5064 x__5065 x__5066 x__5067))))))))))
                                (l1
                                  (if (x2_4665 <= 0)
                                    (l0
                                      (ys''ys''_4650 x1_4664 0
                                        (fun (p11_4693:bool) (p12_4694:int) (p21_4695:bool) 
                                             (p22_4696:x_1:int[(not p11_4693) || (
                                                               not p21_4695) || (
                                                               not (x1_4664 <= 0)) || 
                                                               x1_4664 < 0 || 
                                                               0 < 0 || 
                                                               p12_4694 <= x_1])
                                         -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                                    (l1
                                      (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1)
                                        (fun (x__5060:bool) (x__5061:int) (x__5062:bool) 
                                             (x__5063:x_1:int[(not x__5060) || (
                                                              not x__5062) || 
                                                              (not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                                              x1_4664 - 1 < 0 || 
                                                              x2_4665 - 1 < 0 || 
                                                              x__5061 <= x_1])
                                         -> (k_insert_4666 x__5060 x__5061 x__5062 x__5063))))))))))))))
                  (l0
                    (k_insert_2260
                      (fun (x1_4962:int) (x2_4963:int) 
                           (k_insert_rsrs_4964:(x_1:bool ->
                                                x_2:int ->
                                                x_3:bool ->
                                                x_4:int[(not x_1) || (
                                                        not x_3) || (
                                                        not (x1_4962 <= x2_4963)) || 
                                                        x1_4962 < 0 || 
                                                        x2_4963 < 0 || 
                                                        x_2 <= x_4]
                                                -> X))
                       ->
                       (if (x2_4963 = x1_4962)
                         (l0
                           (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                             (l1 (k_insert_rsrs_4964 false 0 false 0))))
                         (l1
                           (if (x1_4962 <= 0)
                             (l0
                               (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                                 (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
                             (l1
                               (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                                 (l1 (k_insert_rsrs_4964 false 0 false 0))))))))))))) ===> (
ysys_1016 0 0
 (fun (p11_4797:bool) (p12_4798:int) (p21_4799:bool) 
      (p22_4800:x_1:int[(not p11_4797) || (not p21_4799) || (not (0 <= 0)) || 0 < 0 || 0 < 0 || p12_4798 <= x_1])
  ->
  (if p11_4797
    (l1
      (if (x_1053 < p12_4798)
        (l0
          (k_insert_2260
            (fun (x1_4612:int) (x2_4613:int) 
                 (k_insert_4614:(x_1:bool ->
                                 x_2:int ->
                                 x_3:bool ->
                                 x_4:int[(not x_1) || (not x_3) || (not (x1_4612 <= x2_4613)) || 
                                         x1_4612 < 0 || x2_4613 < 0 || 
                                         x_2 <= x_4]
                                 -> X))
             ->
             (if (x1_4612 <= 0)
               (l0
                 (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
                   (l1
                     (ysys_1016 0 (x2_4613 - 1)
                       (fun (xs11_4631:bool) (xs12_4632:int) (xs21_4633:bool) 
                            (xs22_4634:x_1:int[(not xs11_4631) || (not xs21_4633) || (
                                               not (0 <= (x2_4613 - 1))) || 
                                               0 < 0 || x2_4613 - 1 < 0 || 
                                               xs12_4632 <= x_1])
                        ->
                        (if xs21_4633
                          (l1
                            (if (1 = x2_4613)
                              (l0
                                (if (xs22_4634 = xs12_4632)
                                  (l1
                                    (if (p11_4797 <=> xs11_4631)
                                      (l1
                                        (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                          (l0
                                            (loop_2167 ()
                                              (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int) ->
                                               (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                                      (l0
                                        (l0
                                          (loop_2167 ()
                                            (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int) ->
                                             (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))))))
                                  (l0
                                    (loop_2167 ()
                                      (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                                       (k_insert_4614 x__5108 x__5109 x__5110 x__5111))))))
                              (l1
                                (l1
                                  (if (p11_4797 <=> xs11_4631)
                                    (l1
                                      (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                        (l0
                                          (loop_2167 ()
                                            (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int) ->
                                             (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))))
                                    (l0
                                      (l0
                                        (loop_2167 ()
                                          (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int) ->
                                           (k_insert_4614 x__5100 x__5101 x__5102 x__5103))))))))))
                          (l0
                            (loop_2167 ()
                              (fun (x__5096:bool) (x__5097:int) (x__5098:bool) (x__5099:int) ->
                               (k_insert_4614 x__5096 x__5097 x__5098 x__5099))))))))))
               (l1
                 (if (x2_4613 <= 0)
                   (l0
                     (ysys_1016 (x1_4612 - 1) 0
                       (fun (p11_4641:bool) (p12_4642:int) (p21_4643:bool) 
                            (p22_4644:x_1:int[(not p11_4641) || (not p21_4643) || (
                                              not ((x1_4612 - 1) <= 0)) || 
                                              x1_4612 - 1 < 0 || 0 < 0 || 
                                              p12_4642 <= x_1])
                        -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
                   (l1
                     (ysys_1016 (x1_4612 - 1) (x2_4613 - 1)
                       (fun (x__5092:bool) (x__5093:int) (x__5094:bool) 
                            (x__5095:x_1:int[(not x__5092) || (not x__5094) || 
                                             (not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                                             x1_4612 - 1 < 0 || x2_4613 - 1 < 0 || 
                                             x__5093 <= x_1])
                        -> (k_insert_4614 x__5092 x__5093 x__5094 x__5095))))))))))
        (l1
          (insert_1014 x_1053
            (fun (x1_5042:int) (x2_5043:int) 
                 (k_insert_ys'ys'_5044:(x_1:bool ->
                                        x_2:int ->
                                        x_3:bool ->
                                        x_4:int[(not x_1) || (not x_3) || (
                                                not (x1_5042 <= x2_5043)) || 
                                                x1_5042 < 0 || x2_5043 < 0 || 
                                                x_2 <= x_4]
                                        -> X))
             ->
             (ysys_1016 (x1_5042 + 1) (x2_5043 + 1)
               (fun (x__5088:bool) (x__5089:int) (x__5090:bool) 
                    (x__5091:x_1:int[(not x__5088) || (not x__5090) || (
                                     not ((x1_5042 + 1) <= (x2_5043 + 1))) || 
                                     x1_5042 + 1 < 0 || x2_5043 + 1 < 0 || 
                                     x__5089 <= x_1])
                -> (k_insert_ys'ys'_5044 x__5088 x__5089 x__5090 x__5091))))
            (fun (ys''ys''_4650:(x_1:int ->
                                 x_2:int ->
                                 (x_4:bool ->
                                  x_5:int ->
                                  x_6:bool ->
                                  x_7:int[(not x_4) || (not x_6) || (
                                          not (x_1 <= x_2)) || x_1 < 0 || 
                                          x_2 < 0 || x_5 <= x_7]
                                  -> X)
                                 -> X))
             ->
             (k_insert_2260
               (fun (x1_4664:int) (x2_4665:int) 
                    (k_insert_4666:(x_1:bool ->
                                    x_2:int ->
                                    x_3:bool ->
                                    x_4:int[(not x_1) || (not x_3) || (
                                            not (x1_4664 <= x2_4665)) || 
                                            x1_4664 < 0 || x2_4665 < 0 || 
                                            x_2 <= x_4]
                                    -> X))
                ->
                (if (x1_4664 <= 0)
                  (l0
                    (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                      (l1
                        (ysys_1016 0 (x2_4665 - 1)
                          (fun (xs11_4683:bool) (xs12_4684:int) (xs21_4685:bool) 
                               (xs22_4686:x_1:int[(not xs11_4683) || (
                                                  not xs21_4685) || (
                                                  not (0 <= (x2_4665 - 1))) || 
                                                  0 < 0 || x2_4665 - 1 < 0 || 
                                                  xs12_4684 <= x_1])
                           ->
                           (if xs21_4685
                             (l1
                               (if (1 = x2_4665)
                                 (l0
                                   (if (xs22_4686 = xs12_4684)
                                     (l1
                                       (if (p11_4797 <=> xs11_4683)
                                         (l1
                                           (if (xs12_4684 = p12_4798) (
                                             l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                             (l0
                                               (loop_2167 ()
                                                 (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int) ->
                                                  (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                                         (l0
                                           (l0
                                             (loop_2167 ()
                                               (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
                                                (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                                     (l0
                                       (loop_2167 ()
                                         (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                                          (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))))
                                 (l1
                                   (l1
                                     (if (p11_4797 <=> xs11_4683)
                                       (l1
                                         (if (xs12_4684 = p12_4798) (
                                           l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                           (l0
                                             (loop_2167 ()
                                               (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
                                                (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                                       (l0
                                         (l0
                                           (loop_2167 ()
                                             (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
                                              (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))))))))
                             (l0
                               (loop_2167 ()
                                 (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                                  (k_insert_4666 x__5064 x__5065 x__5066 x__5067))))))))))
                  (l1
                    (if (x2_4665 <= 0)
                      (l0
                        (ys''ys''_4650 x1_4664 0
                          (fun (p11_4693:bool) (p12_4694:int) (p21_4695:bool) 
                               (p22_4696:x_1:int[(not p11_4693) || (not p21_4695) || (
                                                 not (x1_4664 <= 0)) || 
                                                 x1_4664 < 0 || 0 < 0 || 
                                                 p12_4694 <= x_1])
                           -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                      (l1
                        (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1)
                          (fun (x__5060:bool) (x__5061:int) (x__5062:bool) 
                               (x__5063:x_1:int[(not x__5060) || (not x__5062) || 
                                                (not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                                x1_4664 - 1 < 0 || x2_4665 - 1 < 0 || 
                                                x__5061 <= x_1])
                           -> (k_insert_4666 x__5060 x__5061 x__5062 x__5063))))))))))))))
    (l0
      (k_insert_2260
        (fun (x1_4962:int) (x2_4963:int) 
             (k_insert_rsrs_4964:(x_1:bool ->
                                  x_2:int ->
                                  x_3:bool ->
                                  x_4:int[(not x_1) || (not x_3) || (
                                          not (x1_4962 <= x2_4963)) || 
                                          x1_4962 < 0 || x2_4963 < 0 || 
                                          x_2 <= x_4]
                                  -> X))
         ->
         (if (x2_4963 = x1_4962)
           (l0
             (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
               (l1 (k_insert_rsrs_4964 false 0 false 0))))
           (l1
             (if (x1_4962 <= 0)
               (l0
                 (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                   (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
               (l1
                 (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                   (l1 (k_insert_rsrs_4964 false 0 false 0)))))))))))))
insert_1014:: (ysys_1016 0 0
                (fun (p11_4797:bool) (p12_4798:int) (p21_4799:bool) 
                     (p22_4800:x_1:int[(not p11_4797) || (not p21_4799) || (
                                       not (0 <= 0)) || 0 < 0 || 0 < 0 || 
                                       p12_4798 <= x_1])
                 ->
                 (if p11_4797
                   (l1
                     (if (x_1053 < p12_4798)
                       (l0
                         (k_insert_2260
                           (fun (x1_4612:int) (x2_4613:int) 
                                (k_insert_4614:(x_1:bool ->
                                                x_2:int ->
                                                x_3:bool ->
                                                x_4:int[(not x_1) || (
                                                        not x_3) || (
                                                        not (x1_4612 <= x2_4613)) || 
                                                        x1_4612 < 0 || 
                                                        x2_4613 < 0 || 
                                                        x_2 <= x_4]
                                                -> X))
                            ->
                            (if (x1_4612 <= 0)
                              (l0
                                (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
                                  (l1
                                    (ysys_1016 0 (x2_4613 - 1)
                                      (fun (xs11_4631:bool) (xs12_4632:int) (xs21_4633:bool) 
                                           (xs22_4634:x_1:int[(not xs11_4631) || (
                                                              not xs21_4633) || (
                                                              not (0 <= (x2_4613 - 1))) || 
                                                              0 < 0 || 
                                                              x2_4613 - 1 < 0 || 
                                                              xs12_4632 <= x_1])
                                       ->
                                       (if xs21_4633
                                         (l1
                                           (if (1 = x2_4613)
                                             (l0
                                               (if (xs22_4634 = xs12_4632)
                                                 (l1
                                                   (if (p11_4797 <=> xs11_4631)
                                                     (l1
                                                       (if (xs12_4632 = p12_4798)
                                                         (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                                         (l0
                                                           (loop_2167 ()
                                                             (fun (x__5116:bool) (x__5117:int) (x__5118:bool) 
                                                                  (x__5119:int)
                                                              -> (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                                                     (l0
                                                       (l0
                                                         (loop_2167 ()
                                                           (fun (x__5112:bool) (x__5113:int) (x__5114:bool) 
                                                                (x__5115:int)
                                                            -> (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))))))
                                                 (l0
                                                   (loop_2167 ()
                                                     (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                                                      (k_insert_4614 x__5108 x__5109 x__5110 x__5111))))))
                                             (l1
                                               (l1
                                                 (if (p11_4797 <=> xs11_4631)
                                                   (l1
                                                     (if (xs12_4632 = p12_4798)
                                                       (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                                       (l0
                                                         (loop_2167 ()
                                                           (fun (x__5104:bool) (x__5105:int) (x__5106:bool) 
                                                                (x__5107:int)
                                                            -> (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))))
                                                   (l0
                                                     (l0
                                                       (loop_2167 ()
                                                         (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int)
                                                          -> (k_insert_4614 x__5100 x__5101 x__5102 x__5103))))))))))
                                         (l0
                                           (loop_2167 ()
                                             (fun (x__5096:bool) (x__5097:int) (x__5098:bool) (x__5099:int) ->
                                              (k_insert_4614 x__5096 x__5097 x__5098 x__5099))))))))))
                              (l1
                                (if (x2_4613 <= 0)
                                  (l0
                                    (ysys_1016 (x1_4612 - 1) 0
                                      (fun (p11_4641:bool) (p12_4642:int) (p21_4643:bool) 
                                           (p22_4644:x_1:int[(not p11_4641) || (
                                                             not p21_4643) || (
                                                             not ((x1_4612 - 1) <= 0)) || 
                                                             x1_4612 - 1 < 0 || 
                                                             0 < 0 || 
                                                             p12_4642 <= x_1])
                                       -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
                                  (l1
                                    (ysys_1016 (x1_4612 - 1) (x2_4613 - 1)
                                      (fun (x__5092:bool) (x__5093:int) (x__5094:bool) 
                                           (x__5095:x_1:int[(not x__5092) || (
                                                            not x__5094) || (
                                                            not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                                                            x1_4612 - 1 < 0 || 
                                                            x2_4613 - 1 < 0 || 
                                                            x__5093 <= x_1])
                                       -> (k_insert_4614 x__5092 x__5093 x__5094 x__5095))))))))))
                       (l1
                         (insert_1014 x_1053
                           (fun (x1_5042:int) (x2_5043:int) 
                                (k_insert_ys'ys'_5044:(x_1:bool ->
                                                       x_2:int ->
                                                       x_3:bool ->
                                                       x_4:int[(not x_1) || (
                                                               not x_3) || (
                                                               not (x1_5042 <= x2_5043)) || 
                                                               x1_5042 < 0 || 
                                                               x2_5043 < 0 || 
                                                               x_2 <= x_4]
                                                       -> X))
                            ->
                            (ysys_1016 (x1_5042 + 1) (x2_5043 + 1)
                              (fun (x__5088:bool) (x__5089:int) (x__5090:bool) 
                                   (x__5091:x_1:int[(not x__5088) || (
                                                    not x__5090) || (
                                                    not ((x1_5042 + 1) <= (x2_5043 + 1))) || 
                                                    x1_5042 + 1 < 0 || 
                                                    x2_5043 + 1 < 0 || 
                                                    x__5089 <= x_1])
                               -> (k_insert_ys'ys'_5044 x__5088 x__5089 x__5090 x__5091))))
                           (fun (ys''ys''_4650:(x_1:int ->
                                                x_2:int ->
                                                (x_4:bool ->
                                                 x_5:int ->
                                                 x_6:bool ->
                                                 x_7:int[(not x_4) || (
                                                         not x_6) || (
                                                         not (x_1 <= x_2)) || 
                                                         x_1 < 0 || x_2 < 0 || 
                                                         x_5 <= x_7]
                                                 -> X)
                                                -> X))
                            ->
                            (k_insert_2260
                              (fun (x1_4664:int) (x2_4665:int) 
                                   (k_insert_4666:(x_1:bool ->
                                                   x_2:int ->
                                                   x_3:bool ->
                                                   x_4:int[(not x_1) || (
                                                           not x_3) || (
                                                           not (x1_4664 <= x2_4665)) || 
                                                           x1_4664 < 0 || 
                                                           x2_4665 < 0 || 
                                                           x_2 <= x_4]
                                                   -> X))
                               ->
                               (if (x1_4664 <= 0)
                                 (l0
                                   (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                                     (l1
                                       (ysys_1016 0 (x2_4665 - 1)
                                         (fun (xs11_4683:bool) (xs12_4684:int) (xs21_4685:bool) 
                                              (xs22_4686:x_1:int[(not xs11_4683) || (
                                                                 not xs21_4685) || (
                                                                 not (0 <= (x2_4665 - 1))) || 
                                                                 0 < 0 || 
                                                                 x2_4665 - 1 < 0 || 
                                                                 xs12_4684 <= x_1])
                                          ->
                                          (if xs21_4685
                                            (l1
                                              (if (1 = x2_4665)
                                                (l0
                                                  (if (xs22_4686 = xs12_4684)
                                                    (l1
                                                      (if (p11_4797 <=> xs11_4683)
                                                        (l1
                                                          (if (xs12_4684 = p12_4798)
                                                            (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                            (l0
                                                              (loop_2167 ()
                                                                (fun 
                                                                 (x__5084:bool) (x__5085:int) (x__5086:bool) 
                                                                 (x__5087:int) ->
                                                                 (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                                                        (l0
                                                          (l0
                                                            (loop_2167 ()
                                                              (fun (x__5080:bool) (x__5081:int) (x__5082:bool) 
                                                                   (x__5083:int)
                                                               -> (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                                                    (l0
                                                      (loop_2167 ()
                                                        (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int)
                                                         -> (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))))
                                                (l1
                                                  (l1
                                                    (if (p11_4797 <=> xs11_4683)
                                                      (l1
                                                        (if (xs12_4684 = p12_4798)
                                                          (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                          (l0
                                                            (loop_2167 ()
                                                              (fun (x__5072:bool) (x__5073:int) (x__5074:bool) 
                                                                   (x__5075:int)
                                                               -> (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                                                      (l0
                                                        (l0
                                                          (loop_2167 ()
                                                            (fun (x__5068:bool) (x__5069:int) (x__5070:bool) 
                                                                 (x__5071:int)
                                                             -> (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))))))))
                                            (l0
                                              (loop_2167 ()
                                                (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                                                 (k_insert_4666 x__5064 x__5065 x__5066 x__5067))))))))))
                                 (l1
                                   (if (x2_4665 <= 0)
                                     (l0
                                       (ys''ys''_4650 x1_4664 0
                                         (fun (p11_4693:bool) (p12_4694:int) (p21_4695:bool) 
                                              (p22_4696:x_1:int[(not p11_4693) || (
                                                                not p21_4695) || (
                                                                not (x1_4664 <= 0)) || 
                                                                x1_4664 < 0 || 
                                                                0 < 0 || 
                                                                p12_4694 <= x_1])
                                          -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                                     (l1
                                       (ys''ys''_4650 (x1_4664 - 1) (
                                         x2_4665 - 1)
                                         (fun (x__5060:bool) (x__5061:int) (x__5062:bool) 
                                              (x__5063:x_1:int[(not x__5060) || (
                                                               not x__5062) || 
                                                               (not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                                               x1_4664 - 1 < 0 || 
                                                               x2_4665 - 1 < 0 || 
                                                               x__5061 <= x_1])
                                          -> (k_insert_4666 x__5060 x__5061 x__5062 x__5063))))))))))))))
                   (l0
                     (k_insert_2260
                       (fun (x1_4962:int) (x2_4963:int) 
                            (k_insert_rsrs_4964:(x_1:bool ->
                                                 x_2:int ->
                                                 x_3:bool ->
                                                 x_4:int[(not x_1) || (
                                                         not x_3) || (
                                                         not (x1_4962 <= x2_4963)) || 
                                                         x1_4962 < 0 || 
                                                         x2_4963 < 0 || 
                                                         x_2 <= x_4]
                                                 -> X))
                        ->
                        (if (x2_4963 = x1_4962)
                          (l0
                            (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                              (l1 (k_insert_rsrs_4964 false 0 false 0))))
                          (l1
                            (if (x1_4962 <= 0)
                              (l0
                                (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                                  (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
                              (l1
                                (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                                  (l1 (k_insert_rsrs_4964 false 0 false 0)))))))))))))
abstract_term: (ysys_1016 0 0
                 (fun (p11_4797:bool) (p12_4798:int) (p21_4799:bool) 
                      (p22_4800:x_1:int[(not p11_4797) || (not p21_4799) || (
                                        not (0 <= 0)) || 0 < 0 || 0 < 0 || 
                                        p12_4798 <= x_1])
                  ->
                  (if p11_4797
                    (l1
                      (if (x_1053 < p12_4798)
                        (l0
                          (k_insert_2260
                            (fun (x1_4612:int) (x2_4613:int) 
                                 (k_insert_4614:(x_1:bool ->
                                                 x_2:int ->
                                                 x_3:bool ->
                                                 x_4:int[(not x_1) || (
                                                         not x_3) || (
                                                         not (x1_4612 <= x2_4613)) || 
                                                         x1_4612 < 0 || 
                                                         x2_4613 < 0 || 
                                                         x_2 <= x_4]
                                                 -> X))
                             ->
                             (if (x1_4612 <= 0)
                               (l0
                                 (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
                                   (l1
                                     (ysys_1016 0 (x2_4613 - 1)
                                       (fun (xs11_4631:bool) (xs12_4632:int) (xs21_4633:bool) 
                                            (xs22_4634:x_1:int[(not xs11_4631) || (
                                                               not xs21_4633) || (
                                                               not (0 <= (x2_4613 - 1))) || 
                                                               0 < 0 || 
                                                               x2_4613 - 1 < 0 || 
                                                               xs12_4632 <= x_1])
                                        ->
                                        (if xs21_4633
                                          (l1
                                            (if (1 = x2_4613)
                                              (l0
                                                (if (xs22_4634 = xs12_4632)
                                                  (l1
                                                    (if (p11_4797 <=> xs11_4631)
                                                      (l1
                                                        (if (xs12_4632 = p12_4798)
                                                          (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                                          (l0
                                                            (loop_2167 ()
                                                              (fun (x__5116:bool) (x__5117:int) (x__5118:bool) 
                                                                   (x__5119:int)
                                                               -> (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                                                      (l0
                                                        (l0
                                                          (loop_2167 ()
                                                            (fun (x__5112:bool) (x__5113:int) (x__5114:bool) 
                                                                 (x__5115:int)
                                                             -> (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))))))
                                                  (l0
                                                    (loop_2167 ()
                                                      (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                                                       (k_insert_4614 x__5108 x__5109 x__5110 x__5111))))))
                                              (l1
                                                (l1
                                                  (if (p11_4797 <=> xs11_4631)
                                                    (l1
                                                      (if (xs12_4632 = p12_4798)
                                                        (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                                        (l0
                                                          (loop_2167 ()
                                                            (fun (x__5104:bool) (x__5105:int) (x__5106:bool) 
                                                                 (x__5107:int)
                                                             -> (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))))
                                                    (l0
                                                      (l0
                                                        (loop_2167 ()
                                                          (fun (x__5100:bool) (x__5101:int) (x__5102:bool) 
                                                               (x__5103:int)
                                                           -> (k_insert_4614 x__5100 x__5101 x__5102 x__5103))))))))))
                                          (l0
                                            (loop_2167 ()
                                              (fun (x__5096:bool) (x__5097:int) (x__5098:bool) (x__5099:int) ->
                                               (k_insert_4614 x__5096 x__5097 x__5098 x__5099))))))))))
                               (l1
                                 (if (x2_4613 <= 0)
                                   (l0
                                     (ysys_1016 (x1_4612 - 1) 0
                                       (fun (p11_4641:bool) (p12_4642:int) (p21_4643:bool) 
                                            (p22_4644:x_1:int[(not p11_4641) || (
                                                              not p21_4643) || (
                                                              not ((x1_4612 - 1) <= 0)) || 
                                                              x1_4612 - 1 < 0 || 
                                                              0 < 0 || 
                                                              p12_4642 <= x_1])
                                        -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
                                   (l1
                                     (ysys_1016 (x1_4612 - 1) (x2_4613 - 1)
                                       (fun (x__5092:bool) (x__5093:int) (x__5094:bool) 
                                            (x__5095:x_1:int[(not x__5092) || (
                                                             not x__5094) || (
                                                             not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                                                             x1_4612 - 1 < 0 || 
                                                             x2_4613 - 1 < 0 || 
                                                             x__5093 <= x_1])
                                        -> (k_insert_4614 x__5092 x__5093 x__5094 x__5095))))))))))
                        (l1
                          (insert_1014 x_1053
                            (fun (x1_5042:int) (x2_5043:int) 
                                 (k_insert_ys'ys'_5044:(x_1:bool ->
                                                        x_2:int ->
                                                        x_3:bool ->
                                                        x_4:int[(not x_1) || (
                                                                not x_3) || (
                                                                not (x1_5042 <= x2_5043)) || 
                                                                x1_5042 < 0 || 
                                                                x2_5043 < 0 || 
                                                                x_2 <= x_4]
                                                        -> X))
                             ->
                             (ysys_1016 (x1_5042 + 1) (x2_5043 + 1)
                               (fun (x__5088:bool) (x__5089:int) (x__5090:bool) 
                                    (x__5091:x_1:int[(not x__5088) || (
                                                     not x__5090) || (
                                                     not ((x1_5042 + 1) <= (x2_5043 + 1))) || 
                                                     x1_5042 + 1 < 0 || 
                                                     x2_5043 + 1 < 0 || 
                                                     x__5089 <= x_1])
                                -> (k_insert_ys'ys'_5044 x__5088 x__5089 x__5090 x__5091))))
                            (fun (ys''ys''_4650:(x_1:int ->
                                                 x_2:int ->
                                                 (x_4:bool ->
                                                  x_5:int ->
                                                  x_6:bool ->
                                                  x_7:int[(not x_4) || (
                                                          not x_6) || (
                                                          not (x_1 <= x_2)) || 
                                                          x_1 < 0 || 
                                                          x_2 < 0 || 
                                                          x_5 <= x_7]
                                                  -> X)
                                                 -> X))
                             ->
                             (k_insert_2260
                               (fun (x1_4664:int) (x2_4665:int) 
                                    (k_insert_4666:(x_1:bool ->
                                                    x_2:int ->
                                                    x_3:bool ->
                                                    x_4:int[(not x_1) || (
                                                            not x_3) || (
                                                            not (x1_4664 <= x2_4665)) || 
                                                            x1_4664 < 0 || 
                                                            x2_4665 < 0 || 
                                                            x_2 <= x_4]
                                                    -> X))
                                ->
                                (if (x1_4664 <= 0)
                                  (l0
                                    (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                                      (l1
                                        (ysys_1016 0 (x2_4665 - 1)
                                          (fun (xs11_4683:bool) (xs12_4684:int) (xs21_4685:bool) 
                                               (xs22_4686:x_1:int[(not xs11_4683) || (
                                                                  not xs21_4685) || (
                                                                  not (0 <= (x2_4665 - 1))) || 
                                                                  0 < 0 || 
                                                                  x2_4665 - 1 < 0 || 
                                                                  xs12_4684 <= x_1])
                                           ->
                                           (if xs21_4685
                                             (l1
                                               (if (1 = x2_4665)
                                                 (l0
                                                   (if (xs22_4686 = xs12_4684)
                                                     (l1
                                                       (if (p11_4797 <=> xs11_4683)
                                                         (l1
                                                           (if (xs12_4684 = p12_4798)
                                                             (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                             (l0
                                                               (loop_2167 ()
                                                                 (fun 
                                                                  (x__5084:bool) (x__5085:int) (x__5086:bool) 
                                                                  (x__5087:int) ->
                                                                  (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                                                         (l0
                                                           (l0
                                                             (loop_2167 ()
                                                               (fun (x__5080:bool) (x__5081:int) (x__5082:bool) 
                                                                    (x__5083:int)
                                                                -> (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                                                     (l0
                                                       (loop_2167 ()
                                                         (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int)
                                                          -> (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))))
                                                 (l1
                                                   (l1
                                                     (if (p11_4797 <=> xs11_4683)
                                                       (l1
                                                         (if (xs12_4684 = p12_4798)
                                                           (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                           (l0
                                                             (loop_2167 ()
                                                               (fun (x__5072:bool) (x__5073:int) (x__5074:bool) 
                                                                    (x__5075:int)
                                                                -> (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                                                       (l0
                                                         (l0
                                                           (loop_2167 ()
                                                             (fun (x__5068:bool) (x__5069:int) (x__5070:bool) 
                                                                  (x__5071:int)
                                                              -> (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))))))))
                                             (l0
                                               (loop_2167 ()
                                                 (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                                                  (k_insert_4666 x__5064 x__5065 x__5066 x__5067))))))))))
                                  (l1
                                    (if (x2_4665 <= 0)
                                      (l0
                                        (ys''ys''_4650 x1_4664 0
                                          (fun (p11_4693:bool) (p12_4694:int) (p21_4695:bool) 
                                               (p22_4696:x_1:int[(not p11_4693) || (
                                                                 not p21_4695) || (
                                                                 not (x1_4664 <= 0)) || 
                                                                 x1_4664 < 0 || 
                                                                 0 < 0 || 
                                                                 p12_4694 <= x_1])
                                           -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                                      (l1
                                        (ys''ys''_4650 (x1_4664 - 1) (
                                          x2_4665 - 1)
                                          (fun (x__5060:bool) (x__5061:int) (x__5062:bool) 
                                               (x__5063:x_1:int[(not x__5060) || (
                                                                not x__5062) || 
                                                                (not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                                                x1_4664 - 1 < 0 || 
                                                                x2_4665 - 1 < 0 || 
                                                                x__5061 <= x_1])
                                           -> (k_insert_4666 x__5060 x__5061 x__5062 x__5063))))))))))))))
                    (l0
                      (k_insert_2260
                        (fun (x1_4962:int) (x2_4963:int) 
                             (k_insert_rsrs_4964:(x_1:bool ->
                                                  x_2:int ->
                                                  x_3:bool ->
                                                  x_4:int[(not x_1) || (
                                                          not x_3) || (
                                                          not (x1_4962 <= x2_4963)) || 
                                                          x1_4962 < 0 || 
                                                          x2_4963 < 0 || 
                                                          x_2 <= x_4]
                                                  -> X))
                         ->
                         (if (x2_4963 = x1_4962)
                           (l0
                             (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                               (l1 (k_insert_rsrs_4964 false 0 false 0))))
                           (l1
                             (if (x1_4962 <= 0)
                               (l0
                                 (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                                   (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
                               (l1
                                 (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                                   (l1 (k_insert_rsrs_4964 false 0 false 0))))))))))))): X
abstract_term: (fun (p11_4797:bool) (p12_4798:int) (p21_4799:bool) 
                    (p22_4800:x_1:int[(not p11_4797) || (not p21_4799) || (
                                      not (0 <= 0)) || 0 < 0 || 0 < 0 || 
                                      p12_4798 <= x_1])
                ->
                (if p11_4797
                  (l1
                    (if (x_1053 < p12_4798)
                      (l0
                        (k_insert_2260
                          (fun (x1_4612:int) (x2_4613:int) 
                               (k_insert_4614:(x_1:bool ->
                                               x_2:int ->
                                               x_3:bool ->
                                               x_4:int[(not x_1) || (
                                                       not x_3) || (not (x1_4612 <= x2_4613)) || 
                                                       x1_4612 < 0 || 
                                                       x2_4613 < 0 || 
                                                       x_2 <= x_4]
                                               -> X))
                           ->
                           (if (x1_4612 <= 0)
                             (l0
                               (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
                                 (l1
                                   (ysys_1016 0 (x2_4613 - 1)
                                     (fun (xs11_4631:bool) (xs12_4632:int) (xs21_4633:bool) 
                                          (xs22_4634:x_1:int[(not xs11_4631) || (
                                                             not xs21_4633) || (
                                                             not (0 <= (x2_4613 - 1))) || 
                                                             0 < 0 || 
                                                             x2_4613 - 1 < 0 || 
                                                             xs12_4632 <= x_1])
                                      ->
                                      (if xs21_4633
                                        (l1
                                          (if (1 = x2_4613)
                                            (l0
                                              (if (xs22_4634 = xs12_4632)
                                                (l1
                                                  (if (p11_4797 <=> xs11_4631)
                                                    (l1
                                                      (if (xs12_4632 = p12_4798)
                                                        (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                                        (l0
                                                          (loop_2167 ()
                                                            (fun (x__5116:bool) (x__5117:int) (x__5118:bool) 
                                                                 (x__5119:int)
                                                             -> (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                                                    (l0
                                                      (l0
                                                        (loop_2167 ()
                                                          (fun (x__5112:bool) (x__5113:int) (x__5114:bool) 
                                                               (x__5115:int)
                                                           -> (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))))))
                                                (l0
                                                  (loop_2167 ()
                                                    (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                                                     (k_insert_4614 x__5108 x__5109 x__5110 x__5111))))))
                                            (l1
                                              (l1
                                                (if (p11_4797 <=> xs11_4631)
                                                  (l1
                                                    (if (xs12_4632 = p12_4798)
                                                      (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                                      (l0
                                                        (loop_2167 ()
                                                          (fun (x__5104:bool) (x__5105:int) (x__5106:bool) 
                                                               (x__5107:int)
                                                           -> (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))))
                                                  (l0
                                                    (l0
                                                      (loop_2167 ()
                                                        (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int)
                                                         -> (k_insert_4614 x__5100 x__5101 x__5102 x__5103))))))))))
                                        (l0
                                          (loop_2167 ()
                                            (fun (x__5096:bool) (x__5097:int) (x__5098:bool) (x__5099:int) ->
                                             (k_insert_4614 x__5096 x__5097 x__5098 x__5099))))))))))
                             (l1
                               (if (x2_4613 <= 0)
                                 (l0
                                   (ysys_1016 (x1_4612 - 1) 0
                                     (fun (p11_4641:bool) (p12_4642:int) (p21_4643:bool) 
                                          (p22_4644:x_1:int[(not p11_4641) || (
                                                            not p21_4643) || (
                                                            not ((x1_4612 - 1) <= 0)) || 
                                                            x1_4612 - 1 < 0 || 
                                                            0 < 0 || 
                                                            p12_4642 <= x_1])
                                      -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
                                 (l1
                                   (ysys_1016 (x1_4612 - 1) (x2_4613 - 1)
                                     (fun (x__5092:bool) (x__5093:int) (x__5094:bool) 
                                          (x__5095:x_1:int[(not x__5092) || (
                                                           not x__5094) || (
                                                           not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                                                           x1_4612 - 1 < 0 || 
                                                           x2_4613 - 1 < 0 || 
                                                           x__5093 <= x_1])
                                      -> (k_insert_4614 x__5092 x__5093 x__5094 x__5095))))))))))
                      (l1
                        (insert_1014 x_1053
                          (fun (x1_5042:int) (x2_5043:int) 
                               (k_insert_ys'ys'_5044:(x_1:bool ->
                                                      x_2:int ->
                                                      x_3:bool ->
                                                      x_4:int[(not x_1) || (
                                                              not x_3) || (
                                                              not (x1_5042 <= x2_5043)) || 
                                                              x1_5042 < 0 || 
                                                              x2_5043 < 0 || 
                                                              x_2 <= x_4]
                                                      -> X))
                           ->
                           (ysys_1016 (x1_5042 + 1) (x2_5043 + 1)
                             (fun (x__5088:bool) (x__5089:int) (x__5090:bool) 
                                  (x__5091:x_1:int[(not x__5088) || (
                                                   not x__5090) || (not ((x1_5042 + 1) <= (x2_5043 + 1))) || 
                                                   x1_5042 + 1 < 0 || 
                                                   x2_5043 + 1 < 0 || 
                                                   x__5089 <= x_1])
                              -> (k_insert_ys'ys'_5044 x__5088 x__5089 x__5090 x__5091))))
                          (fun (ys''ys''_4650:(x_1:int ->
                                               x_2:int ->
                                               (x_4:bool ->
                                                x_5:int ->
                                                x_6:bool ->
                                                x_7:int[(not x_4) || (
                                                        not x_6) || (
                                                        not (x_1 <= x_2)) || 
                                                        x_1 < 0 || x_2 < 0 || 
                                                        x_5 <= x_7]
                                                -> X)
                                               -> X))
                           ->
                           (k_insert_2260
                             (fun (x1_4664:int) (x2_4665:int) 
                                  (k_insert_4666:(x_1:bool ->
                                                  x_2:int ->
                                                  x_3:bool ->
                                                  x_4:int[(not x_1) || (
                                                          not x_3) || (
                                                          not (x1_4664 <= x2_4665)) || 
                                                          x1_4664 < 0 || 
                                                          x2_4665 < 0 || 
                                                          x_2 <= x_4]
                                                  -> X))
                              ->
                              (if (x1_4664 <= 0)
                                (l0
                                  (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                                    (l1
                                      (ysys_1016 0 (x2_4665 - 1)
                                        (fun (xs11_4683:bool) (xs12_4684:int) (xs21_4685:bool) 
                                             (xs22_4686:x_1:int[(not xs11_4683) || (
                                                                not xs21_4685) || (
                                                                not (0 <= (x2_4665 - 1))) || 
                                                                0 < 0 || 
                                                                x2_4665 - 1 < 0 || 
                                                                xs12_4684 <= x_1])
                                         ->
                                         (if xs21_4685
                                           (l1
                                             (if (1 = x2_4665)
                                               (l0
                                                 (if (xs22_4686 = xs12_4684)
                                                   (l1
                                                     (if (p11_4797 <=> xs11_4683)
                                                       (l1
                                                         (if (xs12_4684 = p12_4798)
                                                           (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                           (l0
                                                             (loop_2167 ()
                                                               (fun (x__5084:bool) (x__5085:int) (x__5086:bool) 
                                                                    (x__5087:int)
                                                                -> (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                                                       (l0
                                                         (l0
                                                           (loop_2167 ()
                                                             (fun (x__5080:bool) (x__5081:int) (x__5082:bool) 
                                                                  (x__5083:int)
                                                              -> (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                                                   (l0
                                                     (loop_2167 ()
                                                       (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int)
                                                        -> (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))))
                                               (l1
                                                 (l1
                                                   (if (p11_4797 <=> xs11_4683)
                                                     (l1
                                                       (if (xs12_4684 = p12_4798)
                                                         (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                         (l0
                                                           (loop_2167 ()
                                                             (fun (x__5072:bool) (x__5073:int) (x__5074:bool) 
                                                                  (x__5075:int)
                                                              -> (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                                                     (l0
                                                       (l0
                                                         (loop_2167 ()
                                                           (fun (x__5068:bool) (x__5069:int) (x__5070:bool) 
                                                                (x__5071:int)
                                                            -> (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))))))))
                                           (l0
                                             (loop_2167 ()
                                               (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                                                (k_insert_4666 x__5064 x__5065 x__5066 x__5067))))))))))
                                (l1
                                  (if (x2_4665 <= 0)
                                    (l0
                                      (ys''ys''_4650 x1_4664 0
                                        (fun (p11_4693:bool) (p12_4694:int) (p21_4695:bool) 
                                             (p22_4696:x_1:int[(not p11_4693) || (
                                                               not p21_4695) || (
                                                               not (x1_4664 <= 0)) || 
                                                               x1_4664 < 0 || 
                                                               0 < 0 || 
                                                               p12_4694 <= x_1])
                                         -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                                    (l1
                                      (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1)
                                        (fun (x__5060:bool) (x__5061:int) (x__5062:bool) 
                                             (x__5063:x_1:int[(not x__5060) || (
                                                              not x__5062) || 
                                                              (not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                                              x1_4664 - 1 < 0 || 
                                                              x2_4665 - 1 < 0 || 
                                                              x__5061 <= x_1])
                                         -> (k_insert_4666 x__5060 x__5061 x__5062 x__5063))))))))))))))
                  (l0
                    (k_insert_2260
                      (fun (x1_4962:int) (x2_4963:int) 
                           (k_insert_rsrs_4964:(x_1:bool ->
                                                x_2:int ->
                                                x_3:bool ->
                                                x_4:int[(not x_1) || (
                                                        not x_3) || (
                                                        not (x1_4962 <= x2_4963)) || 
                                                        x1_4962 < 0 || 
                                                        x2_4963 < 0 || 
                                                        x_2 <= x_4]
                                                -> X))
                       ->
                       (if (x2_4963 = x1_4962)
                         (l0
                           (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                             (l1 (k_insert_rsrs_4964 false 0 false 0))))
                         (l1
                           (if (x1_4962 <= 0)
                             (l0
                               (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                                 (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
                             (l1
                               (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                                 (l1 (k_insert_rsrs_4964 false 0 false 0)))))))))))): (
x_1:bool ->
x_2:int -> x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (0 <= 0)) || 0 < 0 || 0 < 0 || x_2 <= x_4] -> X)
abst_arg: p11_4797, bool
abst_arg: p12_4798, int
abst_arg: p21_4799, bool
abst_arg: p22_4800, x_1:int[(not p11_4797) || (not p21_4799) || (not (0 <= 0)) || 0 < 0 || 0 < 0 || p12_4798 <= x_1]
abst_arg: p11_4797, bool
abst_arg: p12_4798, int
abst_arg: p21_4799, bool
abst_arg: p22_4800, x_1:int[(not p11_4797) || (not p21_4799) || (not (0 <= 0)) || 0 < 0 || 0 < 0 || p12_4798 <= x_1]
abstract_term: (if p11_4797
                 (l1
                   (if (x_1053 < p12_4798)
                     (l0
                       (k_insert_2260
                         (fun (x1_4612:int) (x2_4613:int) 
                              (k_insert_4614:(x_1:bool ->
                                              x_2:int ->
                                              x_3:bool ->
                                              x_4:int[(not x_1) || (not x_3) || (
                                                      not (x1_4612 <= x2_4613)) || 
                                                      x1_4612 < 0 || 
                                                      x2_4613 < 0 || 
                                                      x_2 <= x_4]
                                              -> X))
                          ->
                          (if (x1_4612 <= 0)
                            (l0
                              (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
                                (l1
                                  (ysys_1016 0 (x2_4613 - 1)
                                    (fun (xs11_4631:bool) (xs12_4632:int) (xs21_4633:bool) 
                                         (xs22_4634:x_1:int[(not xs11_4631) || (
                                                            not xs21_4633) || (
                                                            not (0 <= (x2_4613 - 1))) || 
                                                            0 < 0 || 
                                                            x2_4613 - 1 < 0 || 
                                                            xs12_4632 <= x_1])
                                     ->
                                     (if xs21_4633
                                       (l1
                                         (if (1 = x2_4613)
                                           (l0
                                             (if (xs22_4634 = xs12_4632)
                                               (l1
                                                 (if (p11_4797 <=> xs11_4631)
                                                   (l1
                                                     (if (xs12_4632 = p12_4798)
                                                       (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                                       (l0
                                                         (loop_2167 ()
                                                           (fun (x__5116:bool) (x__5117:int) (x__5118:bool) 
                                                                (x__5119:int)
                                                            -> (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                                                   (l0
                                                     (l0
                                                       (loop_2167 ()
                                                         (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int)
                                                          -> (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))))))
                                               (l0
                                                 (loop_2167 ()
                                                   (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                                                    (k_insert_4614 x__5108 x__5109 x__5110 x__5111))))))
                                           (l1
                                             (l1
                                               (if (p11_4797 <=> xs11_4631)
                                                 (l1
                                                   (if (xs12_4632 = p12_4798)
                                                     (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                                     (l0
                                                       (loop_2167 ()
                                                         (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int)
                                                          -> (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))))
                                                 (l0
                                                   (l0
                                                     (loop_2167 ()
                                                       (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int)
                                                        -> (k_insert_4614 x__5100 x__5101 x__5102 x__5103))))))))))
                                       (l0
                                         (loop_2167 ()
                                           (fun (x__5096:bool) (x__5097:int) (x__5098:bool) (x__5099:int) ->
                                            (k_insert_4614 x__5096 x__5097 x__5098 x__5099))))))))))
                            (l1
                              (if (x2_4613 <= 0)
                                (l0
                                  (ysys_1016 (x1_4612 - 1) 0
                                    (fun (p11_4641:bool) (p12_4642:int) (p21_4643:bool) 
                                         (p22_4644:x_1:int[(not p11_4641) || (
                                                           not p21_4643) || (
                                                           not ((x1_4612 - 1) <= 0)) || 
                                                           x1_4612 - 1 < 0 || 
                                                           0 < 0 || p12_4642 <= x_1])
                                     -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
                                (l1
                                  (ysys_1016 (x1_4612 - 1) (x2_4613 - 1)
                                    (fun (x__5092:bool) (x__5093:int) (x__5094:bool) 
                                         (x__5095:x_1:int[(not x__5092) || (
                                                          not x__5094) || (
                                                          not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                                                          x1_4612 - 1 < 0 || 
                                                          x2_4613 - 1 < 0 || 
                                                          x__5093 <= x_1])
                                     -> (k_insert_4614 x__5092 x__5093 x__5094 x__5095))))))))))
                     (l1
                       (insert_1014 x_1053
                         (fun (x1_5042:int) (x2_5043:int) 
                              (k_insert_ys'ys'_5044:(x_1:bool ->
                                                     x_2:int ->
                                                     x_3:bool ->
                                                     x_4:int[(not x_1) || (
                                                             not x_3) || (
                                                             not (x1_5042 <= x2_5043)) || 
                                                             x1_5042 < 0 || 
                                                             x2_5043 < 0 || 
                                                             x_2 <= x_4]
                                                     -> X))
                          ->
                          (ysys_1016 (x1_5042 + 1) (x2_5043 + 1)
                            (fun (x__5088:bool) (x__5089:int) (x__5090:bool) 
                                 (x__5091:x_1:int[(not x__5088) || (not x__5090) || 
                                                  (not ((x1_5042 + 1) <= (x2_5043 + 1))) || 
                                                  x1_5042 + 1 < 0 || 
                                                  x2_5043 + 1 < 0 || 
                                                  x__5089 <= x_1])
                             -> (k_insert_ys'ys'_5044 x__5088 x__5089 x__5090 x__5091))))
                         (fun (ys''ys''_4650:(x_1:int ->
                                              x_2:int ->
                                              (x_4:bool ->
                                               x_5:int ->
                                               x_6:bool ->
                                               x_7:int[(not x_4) || (
                                                       not x_6) || (not (x_1 <= x_2)) || 
                                                       x_1 < 0 || x_2 < 0 || 
                                                       x_5 <= x_7]
                                               -> X)
                                              -> X))
                          ->
                          (k_insert_2260
                            (fun (x1_4664:int) (x2_4665:int) 
                                 (k_insert_4666:(x_1:bool ->
                                                 x_2:int ->
                                                 x_3:bool ->
                                                 x_4:int[(not x_1) || (
                                                         not x_3) || (
                                                         not (x1_4664 <= x2_4665)) || 
                                                         x1_4664 < 0 || 
                                                         x2_4665 < 0 || 
                                                         x_2 <= x_4]
                                                 -> X))
                             ->
                             (if (x1_4664 <= 0)
                               (l0
                                 (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                                   (l1
                                     (ysys_1016 0 (x2_4665 - 1)
                                       (fun (xs11_4683:bool) (xs12_4684:int) (xs21_4685:bool) 
                                            (xs22_4686:x_1:int[(not xs11_4683) || (
                                                               not xs21_4685) || (
                                                               not (0 <= (x2_4665 - 1))) || 
                                                               0 < 0 || 
                                                               x2_4665 - 1 < 0 || 
                                                               xs12_4684 <= x_1])
                                        ->
                                        (if xs21_4685
                                          (l1
                                            (if (1 = x2_4665)
                                              (l0
                                                (if (xs22_4686 = xs12_4684)
                                                  (l1
                                                    (if (p11_4797 <=> xs11_4683)
                                                      (l1
                                                        (if (xs12_4684 = p12_4798)
                                                          (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                          (l0
                                                            (loop_2167 ()
                                                              (fun (x__5084:bool) (x__5085:int) (x__5086:bool) 
                                                                   (x__5087:int)
                                                               -> (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                                                      (l0
                                                        (l0
                                                          (loop_2167 ()
                                                            (fun (x__5080:bool) (x__5081:int) (x__5082:bool) 
                                                                 (x__5083:int)
                                                             -> (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                                                  (l0
                                                    (loop_2167 ()
                                                      (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                                                       (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))))
                                              (l1
                                                (l1
                                                  (if (p11_4797 <=> xs11_4683)
                                                    (l1
                                                      (if (xs12_4684 = p12_4798)
                                                        (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                        (l0
                                                          (loop_2167 ()
                                                            (fun (x__5072:bool) (x__5073:int) (x__5074:bool) 
                                                                 (x__5075:int)
                                                             -> (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                                                    (l0
                                                      (l0
                                                        (loop_2167 ()
                                                          (fun (x__5068:bool) (x__5069:int) (x__5070:bool) 
                                                               (x__5071:int)
                                                           -> (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))))))))
                                          (l0
                                            (loop_2167 ()
                                              (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                                               (k_insert_4666 x__5064 x__5065 x__5066 x__5067))))))))))
                               (l1
                                 (if (x2_4665 <= 0)
                                   (l0
                                     (ys''ys''_4650 x1_4664 0
                                       (fun (p11_4693:bool) (p12_4694:int) (p21_4695:bool) 
                                            (p22_4696:x_1:int[(not p11_4693) || (
                                                              not p21_4695) || (
                                                              not (x1_4664 <= 0)) || 
                                                              x1_4664 < 0 || 
                                                              0 < 0 || 
                                                              p12_4694 <= x_1])
                                        -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                                   (l1
                                     (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1)
                                       (fun (x__5060:bool) (x__5061:int) (x__5062:bool) 
                                            (x__5063:x_1:int[(not x__5060) || (
                                                             not x__5062) || (
                                                             not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                                             x1_4664 - 1 < 0 || 
                                                             x2_4665 - 1 < 0 || 
                                                             x__5061 <= x_1])
                                        -> (k_insert_4666 x__5060 x__5061 x__5062 x__5063))))))))))))))
                 (l0
                   (k_insert_2260
                     (fun (x1_4962:int) (x2_4963:int) 
                          (k_insert_rsrs_4964:(x_1:bool ->
                                               x_2:int ->
                                               x_3:bool ->
                                               x_4:int[(not x_1) || (
                                                       not x_3) || (not (x1_4962 <= x2_4963)) || 
                                                       x1_4962 < 0 || 
                                                       x2_4963 < 0 || 
                                                       x_2 <= x_4]
                                               -> X))
                      ->
                      (if (x2_4963 = x1_4962)
                        (l0
                          (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                            (l1 (k_insert_rsrs_4964 false 0 false 0))))
                        (l1
                          (if (x1_4962 <= 0)
                            (l0
                              (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                                (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
                            (l1
                              (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                                (l1 (k_insert_rsrs_4964 false 0 false 0))))))))))): X
abstract_term: p11_4797: x_1:bool[x_1]
cond: true
pbs: p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:p11_4797
tt:false
ff:false

abstract_term: (l1
                 (if (x_1053 < p12_4798)
                   (l0
                     (k_insert_2260
                       (fun (x1_4612:int) (x2_4613:int) 
                            (k_insert_4614:(x_1:bool ->
                                            x_2:int ->
                                            x_3:bool ->
                                            x_4:int[(not x_1) || (not x_3) || (
                                                    not (x1_4612 <= x2_4613)) || 
                                                    x1_4612 < 0 || x2_4613 < 0 || 
                                                    x_2 <= x_4]
                                            -> X))
                        ->
                        (if (x1_4612 <= 0)
                          (l0
                            (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
                              (l1
                                (ysys_1016 0 (x2_4613 - 1)
                                  (fun (xs11_4631:bool) (xs12_4632:int) (xs21_4633:bool) 
                                       (xs22_4634:x_1:int[(not xs11_4631) || (
                                                          not xs21_4633) || (
                                                          not (0 <= (x2_4613 - 1))) || 
                                                          0 < 0 || x2_4613 - 1 < 0 || 
                                                          xs12_4632 <= x_1])
                                   ->
                                   (if xs21_4633
                                     (l1
                                       (if (1 = x2_4613)
                                         (l0
                                           (if (xs22_4634 = xs12_4632)
                                             (l1
                                               (if (p11_4797 <=> xs11_4631)
                                                 (l1
                                                   (if (xs12_4632 = p12_4798)
                                                     (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                                     (l0
                                                       (loop_2167 ()
                                                         (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int)
                                                          -> (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                                                 (l0
                                                   (l0
                                                     (loop_2167 ()
                                                       (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int)
                                                        -> (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))))))
                                             (l0
                                               (loop_2167 ()
                                                 (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                                                  (k_insert_4614 x__5108 x__5109 x__5110 x__5111))))))
                                         (l1
                                           (l1
                                             (if (p11_4797 <=> xs11_4631)
                                               (l1
                                                 (if (xs12_4632 = p12_4798)
                                                   (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                                   (l0
                                                     (loop_2167 ()
                                                       (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int)
                                                        -> (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))))
                                               (l0
                                                 (l0
                                                   (loop_2167 ()
                                                     (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int) ->
                                                      (k_insert_4614 x__5100 x__5101 x__5102 x__5103))))))))))
                                     (l0
                                       (loop_2167 ()
                                         (fun (x__5096:bool) (x__5097:int) (x__5098:bool) (x__5099:int) ->
                                          (k_insert_4614 x__5096 x__5097 x__5098 x__5099))))))))))
                          (l1
                            (if (x2_4613 <= 0)
                              (l0
                                (ysys_1016 (x1_4612 - 1) 0
                                  (fun (p11_4641:bool) (p12_4642:int) (p21_4643:bool) 
                                       (p22_4644:x_1:int[(not p11_4641) || (
                                                         not p21_4643) || (
                                                         not ((x1_4612 - 1) <= 0)) || 
                                                         x1_4612 - 1 < 0 || 
                                                         0 < 0 || p12_4642 <= x_1])
                                   -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
                              (l1
                                (ysys_1016 (x1_4612 - 1) (x2_4613 - 1)
                                  (fun (x__5092:bool) (x__5093:int) (x__5094:bool) 
                                       (x__5095:x_1:int[(not x__5092) || (
                                                        not x__5094) || (
                                                        not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                                                        x1_4612 - 1 < 0 || 
                                                        x2_4613 - 1 < 0 || 
                                                        x__5093 <= x_1])
                                   -> (k_insert_4614 x__5092 x__5093 x__5094 x__5095))))))))))
                   (l1
                     (insert_1014 x_1053
                       (fun (x1_5042:int) (x2_5043:int) 
                            (k_insert_ys'ys'_5044:(x_1:bool ->
                                                   x_2:int ->
                                                   x_3:bool ->
                                                   x_4:int[(not x_1) || (
                                                           not x_3) || (
                                                           not (x1_5042 <= x2_5043)) || 
                                                           x1_5042 < 0 || 
                                                           x2_5043 < 0 || 
                                                           x_2 <= x_4]
                                                   -> X))
                        ->
                        (ysys_1016 (x1_5042 + 1) (x2_5043 + 1)
                          (fun (x__5088:bool) (x__5089:int) (x__5090:bool) 
                               (x__5091:x_1:int[(not x__5088) || (not x__5090) || 
                                                (not ((x1_5042 + 1) <= (x2_5043 + 1))) || 
                                                x1_5042 + 1 < 0 || x2_5043 + 1 < 0 || 
                                                x__5089 <= x_1])
                           -> (k_insert_ys'ys'_5044 x__5088 x__5089 x__5090 x__5091))))
                       (fun (ys''ys''_4650:(x_1:int ->
                                            x_2:int ->
                                            (x_4:bool ->
                                             x_5:int ->
                                             x_6:bool ->
                                             x_7:int[(not x_4) || (not x_6) || (
                                                     not (x_1 <= x_2)) || 
                                                     x_1 < 0 || x_2 < 0 || 
                                                     x_5 <= x_7]
                                             -> X)
                                            -> X))
                        ->
                        (k_insert_2260
                          (fun (x1_4664:int) (x2_4665:int) 
                               (k_insert_4666:(x_1:bool ->
                                               x_2:int ->
                                               x_3:bool ->
                                               x_4:int[(not x_1) || (
                                                       not x_3) || (not (x1_4664 <= x2_4665)) || 
                                                       x1_4664 < 0 || 
                                                       x2_4665 < 0 || 
                                                       x_2 <= x_4]
                                               -> X))
                           ->
                           (if (x1_4664 <= 0)
                             (l0
                               (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                                 (l1
                                   (ysys_1016 0 (x2_4665 - 1)
                                     (fun (xs11_4683:bool) (xs12_4684:int) (xs21_4685:bool) 
                                          (xs22_4686:x_1:int[(not xs11_4683) || (
                                                             not xs21_4685) || (
                                                             not (0 <= (x2_4665 - 1))) || 
                                                             0 < 0 || 
                                                             x2_4665 - 1 < 0 || 
                                                             xs12_4684 <= x_1])
                                      ->
                                      (if xs21_4685
                                        (l1
                                          (if (1 = x2_4665)
                                            (l0
                                              (if (xs22_4686 = xs12_4684)
                                                (l1
                                                  (if (p11_4797 <=> xs11_4683)
                                                    (l1
                                                      (if (xs12_4684 = p12_4798)
                                                        (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                        (l0
                                                          (loop_2167 ()
                                                            (fun (x__5084:bool) (x__5085:int) (x__5086:bool) 
                                                                 (x__5087:int)
                                                             -> (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                                                    (l0
                                                      (l0
                                                        (loop_2167 ()
                                                          (fun (x__5080:bool) (x__5081:int) (x__5082:bool) 
                                                               (x__5083:int)
                                                           -> (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                                                (l0
                                                  (loop_2167 ()
                                                    (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                                                     (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))))
                                            (l1
                                              (l1
                                                (if (p11_4797 <=> xs11_4683)
                                                  (l1
                                                    (if (xs12_4684 = p12_4798)
                                                      (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                      (l0
                                                        (loop_2167 ()
                                                          (fun (x__5072:bool) (x__5073:int) (x__5074:bool) 
                                                               (x__5075:int)
                                                           -> (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                                                  (l0
                                                    (l0
                                                      (loop_2167 ()
                                                        (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int)
                                                         -> (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))))))))
                                        (l0
                                          (loop_2167 ()
                                            (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                                             (k_insert_4666 x__5064 x__5065 x__5066 x__5067))))))))))
                             (l1
                               (if (x2_4665 <= 0)
                                 (l0
                                   (ys''ys''_4650 x1_4664 0
                                     (fun (p11_4693:bool) (p12_4694:int) (p21_4695:bool) 
                                          (p22_4696:x_1:int[(not p11_4693) || (
                                                            not p21_4695) || (
                                                            not (x1_4664 <= 0)) || 
                                                            x1_4664 < 0 || 
                                                            0 < 0 || 
                                                            p12_4694 <= x_1])
                                      -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                                 (l1
                                   (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1)
                                     (fun (x__5060:bool) (x__5061:int) (x__5062:bool) 
                                          (x__5063:x_1:int[(not x__5060) || (
                                                           not x__5062) || (
                                                           not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                                           x1_4664 - 1 < 0 || 
                                                           x2_4665 - 1 < 0 || 
                                                           x__5061 <= x_1])
                                      -> (k_insert_4666 x__5060 x__5061 x__5062 x__5063)))))))))))))): X
abstract_term: (if (x_1053 < p12_4798)
                 (l0
                   (k_insert_2260
                     (fun (x1_4612:int) (x2_4613:int) 
                          (k_insert_4614:(x_1:bool ->
                                          x_2:int ->
                                          x_3:bool ->
                                          x_4:int[(not x_1) || (not x_3) || (
                                                  not (x1_4612 <= x2_4613)) || 
                                                  x1_4612 < 0 || x2_4613 < 0 || 
                                                  x_2 <= x_4]
                                          -> X))
                      ->
                      (if (x1_4612 <= 0)
                        (l0
                          (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
                            (l1
                              (ysys_1016 0 (x2_4613 - 1)
                                (fun (xs11_4631:bool) (xs12_4632:int) (xs21_4633:bool) 
                                     (xs22_4634:x_1:int[(not xs11_4631) || (
                                                        not xs21_4633) || (
                                                        not (0 <= (x2_4613 - 1))) || 
                                                        0 < 0 || x2_4613 - 1 < 0 || 
                                                        xs12_4632 <= x_1])
                                 ->
                                 (if xs21_4633
                                   (l1
                                     (if (1 = x2_4613)
                                       (l0
                                         (if (xs22_4634 = xs12_4632)
                                           (l1
                                             (if (p11_4797 <=> xs11_4631)
                                               (l1
                                                 (if (xs12_4632 = p12_4798)
                                                   (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                                   (l0
                                                     (loop_2167 ()
                                                       (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int)
                                                        -> (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                                               (l0
                                                 (l0
                                                   (loop_2167 ()
                                                     (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int) ->
                                                      (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))))))
                                           (l0
                                             (loop_2167 ()
                                               (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                                                (k_insert_4614 x__5108 x__5109 x__5110 x__5111))))))
                                       (l1
                                         (l1
                                           (if (p11_4797 <=> xs11_4631)
                                             (l1
                                               (if (xs12_4632 = p12_4798)
                                                 (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                                 (l0
                                                   (loop_2167 ()
                                                     (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int) ->
                                                      (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))))
                                             (l0
                                               (l0
                                                 (loop_2167 ()
                                                   (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int) ->
                                                    (k_insert_4614 x__5100 x__5101 x__5102 x__5103))))))))))
                                   (l0
                                     (loop_2167 ()
                                       (fun (x__5096:bool) (x__5097:int) (x__5098:bool) (x__5099:int) ->
                                        (k_insert_4614 x__5096 x__5097 x__5098 x__5099))))))))))
                        (l1
                          (if (x2_4613 <= 0)
                            (l0
                              (ysys_1016 (x1_4612 - 1) 0
                                (fun (p11_4641:bool) (p12_4642:int) (p21_4643:bool) 
                                     (p22_4644:x_1:int[(not p11_4641) || (
                                                       not p21_4643) || (
                                                       not ((x1_4612 - 1) <= 0)) || 
                                                       x1_4612 - 1 < 0 || 
                                                       0 < 0 || p12_4642 <= x_1])
                                 -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
                            (l1
                              (ysys_1016 (x1_4612 - 1) (x2_4613 - 1)
                                (fun (x__5092:bool) (x__5093:int) (x__5094:bool) 
                                     (x__5095:x_1:int[(not x__5092) || (
                                                      not x__5094) || (
                                                      not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                                                      x1_4612 - 1 < 0 || 
                                                      x2_4613 - 1 < 0 || 
                                                      x__5093 <= x_1])
                                 -> (k_insert_4614 x__5092 x__5093 x__5094 x__5095))))))))))
                 (l1
                   (insert_1014 x_1053
                     (fun (x1_5042:int) (x2_5043:int) 
                          (k_insert_ys'ys'_5044:(x_1:bool ->
                                                 x_2:int ->
                                                 x_3:bool ->
                                                 x_4:int[(not x_1) || (
                                                         not x_3) || (
                                                         not (x1_5042 <= x2_5043)) || 
                                                         x1_5042 < 0 || 
                                                         x2_5043 < 0 || 
                                                         x_2 <= x_4]
                                                 -> X))
                      ->
                      (ysys_1016 (x1_5042 + 1) (x2_5043 + 1)
                        (fun (x__5088:bool) (x__5089:int) (x__5090:bool) 
                             (x__5091:x_1:int[(not x__5088) || (not x__5090) || 
                                              (not ((x1_5042 + 1) <= (x2_5043 + 1))) || 
                                              x1_5042 + 1 < 0 || x2_5043 + 1 < 0 || 
                                              x__5089 <= x_1])
                         -> (k_insert_ys'ys'_5044 x__5088 x__5089 x__5090 x__5091))))
                     (fun (ys''ys''_4650:(x_1:int ->
                                          x_2:int ->
                                          (x_4:bool ->
                                           x_5:int ->
                                           x_6:bool ->
                                           x_7:int[(not x_4) || (not x_6) || (
                                                   not (x_1 <= x_2)) || 
                                                   x_1 < 0 || x_2 < 0 || 
                                                   x_5 <= x_7]
                                           -> X)
                                          -> X))
                      ->
                      (k_insert_2260
                        (fun (x1_4664:int) (x2_4665:int) 
                             (k_insert_4666:(x_1:bool ->
                                             x_2:int ->
                                             x_3:bool ->
                                             x_4:int[(not x_1) || (not x_3) || (
                                                     not (x1_4664 <= x2_4665)) || 
                                                     x1_4664 < 0 || x2_4665 < 0 || 
                                                     x_2 <= x_4]
                                             -> X))
                         ->
                         (if (x1_4664 <= 0)
                           (l0
                             (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                               (l1
                                 (ysys_1016 0 (x2_4665 - 1)
                                   (fun (xs11_4683:bool) (xs12_4684:int) (xs21_4685:bool) 
                                        (xs22_4686:x_1:int[(not xs11_4683) || (
                                                           not xs21_4685) || (
                                                           not (0 <= (x2_4665 - 1))) || 
                                                           0 < 0 || x2_4665 - 1 < 0 || 
                                                           xs12_4684 <= x_1])
                                    ->
                                    (if xs21_4685
                                      (l1
                                        (if (1 = x2_4665)
                                          (l0
                                            (if (xs22_4686 = xs12_4684)
                                              (l1
                                                (if (p11_4797 <=> xs11_4683)
                                                  (l1
                                                    (if (xs12_4684 = p12_4798)
                                                      (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                      (l0
                                                        (loop_2167 ()
                                                          (fun (x__5084:bool) (x__5085:int) (x__5086:bool) 
                                                               (x__5087:int)
                                                           -> (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                                                  (l0
                                                    (l0
                                                      (loop_2167 ()
                                                        (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int)
                                                         -> (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                                              (l0
                                                (loop_2167 ()
                                                  (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                                                   (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))))
                                          (l1
                                            (l1
                                              (if (p11_4797 <=> xs11_4683)
                                                (l1
                                                  (if (xs12_4684 = p12_4798)
                                                    (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                    (l0
                                                      (loop_2167 ()
                                                        (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int)
                                                         -> (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                                                (l0
                                                  (l0
                                                    (loop_2167 ()
                                                      (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
                                                       (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))))))))
                                      (l0
                                        (loop_2167 ()
                                          (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                                           (k_insert_4666 x__5064 x__5065 x__5066 x__5067))))))))))
                           (l1
                             (if (x2_4665 <= 0)
                               (l0
                                 (ys''ys''_4650 x1_4664 0
                                   (fun (p11_4693:bool) (p12_4694:int) (p21_4695:bool) 
                                        (p22_4696:x_1:int[(not p11_4693) || (
                                                          not p21_4695) || (
                                                          not (x1_4664 <= 0)) || 
                                                          x1_4664 < 0 || 
                                                          0 < 0 || p12_4694 <= x_1])
                                    -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                               (l1
                                 (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1)
                                   (fun (x__5060:bool) (x__5061:int) (x__5062:bool) 
                                        (x__5063:x_1:int[(not x__5060) || (
                                                         not x__5062) || (
                                                         not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                                         x1_4664 - 1 < 0 || 
                                                         x2_4665 - 1 < 0 || 
                                                         x__5061 <= x_1])
                                    -> (k_insert_4666 x__5060 x__5061 x__5062 x__5063))))))))))))): X
abstract_term: (x_1053 < p12_4798): x_1:bool[x_1]
cond: p11_4797; true
pbs: p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:(x_1053 < p12_4798)
tt:false
ff:false

abstract_term: (l0
                 (k_insert_2260
                   (fun (x1_4612:int) (x2_4613:int) 
                        (k_insert_4614:(x_1:bool ->
                                        x_2:int ->
                                        x_3:bool ->
                                        x_4:int[(not x_1) || (not x_3) || (
                                                not (x1_4612 <= x2_4613)) || 
                                                x1_4612 < 0 || x2_4613 < 0 || 
                                                x_2 <= x_4]
                                        -> X))
                    ->
                    (if (x1_4612 <= 0)
                      (l0
                        (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
                          (l1
                            (ysys_1016 0 (x2_4613 - 1)
                              (fun (xs11_4631:bool) (xs12_4632:int) (xs21_4633:bool) 
                                   (xs22_4634:x_1:int[(not xs11_4631) || (
                                                      not xs21_4633) || (
                                                      not (0 <= (x2_4613 - 1))) || 
                                                      0 < 0 || x2_4613 - 1 < 0 || 
                                                      xs12_4632 <= x_1])
                               ->
                               (if xs21_4633
                                 (l1
                                   (if (1 = x2_4613)
                                     (l0
                                       (if (xs22_4634 = xs12_4632)
                                         (l1
                                           (if (p11_4797 <=> xs11_4631)
                                             (l1
                                               (if (xs12_4632 = p12_4798)
                                                 (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                                 (l0
                                                   (loop_2167 ()
                                                     (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int) ->
                                                      (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                                             (l0
                                               (l0
                                                 (loop_2167 ()
                                                   (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int) ->
                                                    (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))))))
                                         (l0
                                           (loop_2167 ()
                                             (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                                              (k_insert_4614 x__5108 x__5109 x__5110 x__5111))))))
                                     (l1
                                       (l1
                                         (if (p11_4797 <=> xs11_4631)
                                           (l1
                                             (if (xs12_4632 = p12_4798) (
                                               l1 (k_insert_4614 true x_1053 true xs22_4634))
                                               (l0
                                                 (loop_2167 ()
                                                   (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int) ->
                                                    (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))))
                                           (l0
                                             (l0
                                               (loop_2167 ()
                                                 (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int) ->
                                                  (k_insert_4614 x__5100 x__5101 x__5102 x__5103))))))))))
                                 (l0
                                   (loop_2167 ()
                                     (fun (x__5096:bool) (x__5097:int) (x__5098:bool) (x__5099:int) ->
                                      (k_insert_4614 x__5096 x__5097 x__5098 x__5099))))))))))
                      (l1
                        (if (x2_4613 <= 0)
                          (l0
                            (ysys_1016 (x1_4612 - 1) 0
                              (fun (p11_4641:bool) (p12_4642:int) (p21_4643:bool) 
                                   (p22_4644:x_1:int[(not p11_4641) || (
                                                     not p21_4643) || (
                                                     not ((x1_4612 - 1) <= 0)) || 
                                                     x1_4612 - 1 < 0 || 
                                                     0 < 0 || p12_4642 <= x_1])
                               -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
                          (l1
                            (ysys_1016 (x1_4612 - 1) (x2_4613 - 1)
                              (fun (x__5092:bool) (x__5093:int) (x__5094:bool) 
                                   (x__5095:x_1:int[(not x__5092) || (
                                                    not x__5094) || (
                                                    not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                                                    x1_4612 - 1 < 0 || 
                                                    x2_4613 - 1 < 0 || 
                                                    x__5093 <= x_1])
                               -> (k_insert_4614 x__5092 x__5093 x__5094 x__5095)))))))))): X
abstract_term: (k_insert_2260
                 (fun (x1_4612:int) (x2_4613:int) 
                      (k_insert_4614:(x_1:bool ->
                                      x_2:int ->
                                      x_3:bool ->
                                      x_4:int[(not x_1) || (not x_3) || (
                                              not (x1_4612 <= x2_4613)) || 
                                              x1_4612 < 0 || x2_4613 < 0 || 
                                              x_2 <= x_4]
                                      -> X))
                  ->
                  (if (x1_4612 <= 0)
                    (l0
                      (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
                        (l1
                          (ysys_1016 0 (x2_4613 - 1)
                            (fun (xs11_4631:bool) (xs12_4632:int) (xs21_4633:bool) 
                                 (xs22_4634:x_1:int[(not xs11_4631) || (
                                                    not xs21_4633) || (
                                                    not (0 <= (x2_4613 - 1))) || 
                                                    0 < 0 || x2_4613 - 1 < 0 || 
                                                    xs12_4632 <= x_1])
                             ->
                             (if xs21_4633
                               (l1
                                 (if (1 = x2_4613)
                                   (l0
                                     (if (xs22_4634 = xs12_4632)
                                       (l1
                                         (if (p11_4797 <=> xs11_4631)
                                           (l1
                                             (if (xs12_4632 = p12_4798) (
                                               l1 (k_insert_4614 true x_1053 true xs22_4634))
                                               (l0
                                                 (loop_2167 ()
                                                   (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int) ->
                                                    (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                                           (l0
                                             (l0
                                               (loop_2167 ()
                                                 (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int) ->
                                                  (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))))))
                                       (l0
                                         (loop_2167 ()
                                           (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                                            (k_insert_4614 x__5108 x__5109 x__5110 x__5111))))))
                                   (l1
                                     (l1
                                       (if (p11_4797 <=> xs11_4631)
                                         (l1
                                           (if (xs12_4632 = p12_4798) (
                                             l1 (k_insert_4614 true x_1053 true xs22_4634))
                                             (l0
                                               (loop_2167 ()
                                                 (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int) ->
                                                  (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))))
                                         (l0
                                           (l0
                                             (loop_2167 ()
                                               (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int) ->
                                                (k_insert_4614 x__5100 x__5101 x__5102 x__5103))))))))))
                               (l0
                                 (loop_2167 ()
                                   (fun (x__5096:bool) (x__5097:int) (x__5098:bool) (x__5099:int) ->
                                    (k_insert_4614 x__5096 x__5097 x__5098 x__5099))))))))))
                    (l1
                      (if (x2_4613 <= 0)
                        (l0
                          (ysys_1016 (x1_4612 - 1) 0
                            (fun (p11_4641:bool) (p12_4642:int) (p21_4643:bool) 
                                 (p22_4644:x_1:int[(not p11_4641) || (
                                                   not p21_4643) || (
                                                   not ((x1_4612 - 1) <= 0)) || 
                                                   x1_4612 - 1 < 0 || 
                                                   0 < 0 || p12_4642 <= x_1])
                             -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
                        (l1
                          (ysys_1016 (x1_4612 - 1) (x2_4613 - 1)
                            (fun (x__5092:bool) (x__5093:int) (x__5094:bool) 
                                 (x__5095:x_1:int[(not x__5092) || (not x__5094) || 
                                                  (not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                                                  x1_4612 - 1 < 0 || 
                                                  x2_4613 - 1 < 0 || 
                                                  x__5093 <= x_1])
                             -> (k_insert_4614 x__5092 x__5093 x__5094 x__5095))))))))): X
abstract_term: (fun (x1_4612:int) (x2_4613:int) 
                    (k_insert_4614:(x_1:bool ->
                                    x_2:int ->
                                    x_3:bool ->
                                    x_4:int[(not x_1) || (not x_3) || (
                                            not (x1_4612 <= x2_4613)) || 
                                            x1_4612 < 0 || x2_4613 < 0 || 
                                            x_2 <= x_4]
                                    -> X))
                ->
                (if (x1_4612 <= 0)
                  (l0
                    (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
                      (l1
                        (ysys_1016 0 (x2_4613 - 1)
                          (fun (xs11_4631:bool) (xs12_4632:int) (xs21_4633:bool) 
                               (xs22_4634:x_1:int[(not xs11_4631) || (
                                                  not xs21_4633) || (
                                                  not (0 <= (x2_4613 - 1))) || 
                                                  0 < 0 || x2_4613 - 1 < 0 || 
                                                  xs12_4632 <= x_1])
                           ->
                           (if xs21_4633
                             (l1
                               (if (1 = x2_4613)
                                 (l0
                                   (if (xs22_4634 = xs12_4632)
                                     (l1
                                       (if (p11_4797 <=> xs11_4631)
                                         (l1
                                           (if (xs12_4632 = p12_4798) (
                                             l1 (k_insert_4614 true x_1053 true xs22_4634))
                                             (l0
                                               (loop_2167 ()
                                                 (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int) ->
                                                  (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                                         (l0
                                           (l0
                                             (loop_2167 ()
                                               (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int) ->
                                                (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))))))
                                     (l0
                                       (loop_2167 ()
                                         (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                                          (k_insert_4614 x__5108 x__5109 x__5110 x__5111))))))
                                 (l1
                                   (l1
                                     (if (p11_4797 <=> xs11_4631)
                                       (l1
                                         (if (xs12_4632 = p12_4798) (
                                           l1 (k_insert_4614 true x_1053 true xs22_4634))
                                           (l0
                                             (loop_2167 ()
                                               (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int) ->
                                                (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))))
                                       (l0
                                         (l0
                                           (loop_2167 ()
                                             (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int) ->
                                              (k_insert_4614 x__5100 x__5101 x__5102 x__5103))))))))))
                             (l0
                               (loop_2167 ()
                                 (fun (x__5096:bool) (x__5097:int) (x__5098:bool) (x__5099:int) ->
                                  (k_insert_4614 x__5096 x__5097 x__5098 x__5099))))))))))
                  (l1
                    (if (x2_4613 <= 0)
                      (l0
                        (ysys_1016 (x1_4612 - 1) 0
                          (fun (p11_4641:bool) (p12_4642:int) (p21_4643:bool) 
                               (p22_4644:x_1:int[(not p11_4641) || (not p21_4643) || (
                                                 not ((x1_4612 - 1) <= 0)) || 
                                                 x1_4612 - 1 < 0 || 0 < 0 || 
                                                 p12_4642 <= x_1])
                           -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
                      (l1
                        (ysys_1016 (x1_4612 - 1) (x2_4613 - 1)
                          (fun (x__5092:bool) (x__5093:int) (x__5094:bool) 
                               (x__5095:x_1:int[(not x__5092) || (not x__5094) || 
                                                (not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                                                x1_4612 - 1 < 0 || x2_4613 - 1 < 0 || 
                                                x__5093 <= x_1])
                           -> (k_insert_4614 x__5092 x__5093 x__5094 x__5095)))))))): (
x_1:int ->
x_2:int ->
(x_4:bool ->
 x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
-> X)
abst_arg: x1_4612, int
abst_arg: x2_4613, int
abst_arg: k_insert_4614, (x_1:bool ->
                          x_2:int ->
                          x_3:bool ->
                          x_4:int[(not x_1) || (not x_3) || (not (x1_4612 <= x2_4613)) || 
                                  x1_4612 < 0 || x2_4613 < 0 || x_2 <= x_4]
                          -> X)
abst_arg: x1_4612, int
abst_arg: x2_4613, int
abst_arg: k_insert_4614, (x_1:bool ->
                          x_2:int ->
                          x_3:bool ->
                          x_4:int[(not x_1) || (not x_3) || (not (x1_4612 <= x2_4613)) || 
                                  x1_4612 < 0 || x2_4613 < 0 || x_2 <= x_4]
                          -> X)
abstract_term: (if (x1_4612 <= 0)
                 (l0
                   (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
                     (l1
                       (ysys_1016 0 (x2_4613 - 1)
                         (fun (xs11_4631:bool) (xs12_4632:int) (xs21_4633:bool) 
                              (xs22_4634:x_1:int[(not xs11_4631) || (
                                                 not xs21_4633) || (not (0 <= (x2_4613 - 1))) || 
                                                 0 < 0 || x2_4613 - 1 < 0 || 
                                                 xs12_4632 <= x_1])
                          ->
                          (if xs21_4633
                            (l1
                              (if (1 = x2_4613)
                                (l0
                                  (if (xs22_4634 = xs12_4632)
                                    (l1
                                      (if (p11_4797 <=> xs11_4631)
                                        (l1
                                          (if (xs12_4632 = p12_4798) (
                                            l1 (k_insert_4614 true x_1053 true xs22_4634))
                                            (l0
                                              (loop_2167 ()
                                                (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int) ->
                                                 (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                                        (l0
                                          (l0
                                            (loop_2167 ()
                                              (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int) ->
                                               (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))))))
                                    (l0
                                      (loop_2167 ()
                                        (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                                         (k_insert_4614 x__5108 x__5109 x__5110 x__5111))))))
                                (l1
                                  (l1
                                    (if (p11_4797 <=> xs11_4631)
                                      (l1
                                        (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                          (l0
                                            (loop_2167 ()
                                              (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int) ->
                                               (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))))
                                      (l0
                                        (l0
                                          (loop_2167 ()
                                            (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int) ->
                                             (k_insert_4614 x__5100 x__5101 x__5102 x__5103))))))))))
                            (l0
                              (loop_2167 ()
                                (fun (x__5096:bool) (x__5097:int) (x__5098:bool) (x__5099:int) ->
                                 (k_insert_4614 x__5096 x__5097 x__5098 x__5099))))))))))
                 (l1
                   (if (x2_4613 <= 0)
                     (l0
                       (ysys_1016 (x1_4612 - 1) 0
                         (fun (p11_4641:bool) (p12_4642:int) (p21_4643:bool) 
                              (p22_4644:x_1:int[(not p11_4641) || (not p21_4643) || (
                                                not ((x1_4612 - 1) <= 0)) || 
                                                x1_4612 - 1 < 0 || 0 < 0 || 
                                                p12_4642 <= x_1])
                          -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
                     (l1
                       (ysys_1016 (x1_4612 - 1) (x2_4613 - 1)
                         (fun (x__5092:bool) (x__5093:int) (x__5094:bool) 
                              (x__5095:x_1:int[(not x__5092) || (not x__5094) || 
                                               (not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                                               x1_4612 - 1 < 0 || x2_4613 - 1 < 0 || 
                                               x__5093 <= x_1])
                          -> (k_insert_4614 x__5092 x__5093 x__5094 x__5095))))))): X
abstract_term: (x1_4612 <= 0): x_1:bool[x_1]
cond: (x_1053 < p12_4798); p11_4797; true
pbs: p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:(x1_4612 <= 0)
tt:false
ff:false

abstract_term: (l0
                 (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
                   (l1
                     (ysys_1016 0 (x2_4613 - 1)
                       (fun (xs11_4631:bool) (xs12_4632:int) (xs21_4633:bool) 
                            (xs22_4634:x_1:int[(not xs11_4631) || (not xs21_4633) || (
                                               not (0 <= (x2_4613 - 1))) || 
                                               0 < 0 || x2_4613 - 1 < 0 || 
                                               xs12_4632 <= x_1])
                        ->
                        (if xs21_4633
                          (l1
                            (if (1 = x2_4613)
                              (l0
                                (if (xs22_4634 = xs12_4632)
                                  (l1
                                    (if (p11_4797 <=> xs11_4631)
                                      (l1
                                        (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                          (l0
                                            (loop_2167 ()
                                              (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int) ->
                                               (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                                      (l0
                                        (l0
                                          (loop_2167 ()
                                            (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int) ->
                                             (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))))))
                                  (l0
                                    (loop_2167 ()
                                      (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                                       (k_insert_4614 x__5108 x__5109 x__5110 x__5111))))))
                              (l1
                                (l1
                                  (if (p11_4797 <=> xs11_4631)
                                    (l1
                                      (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                        (l0
                                          (loop_2167 ()
                                            (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int) ->
                                             (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))))
                                    (l0
                                      (l0
                                        (loop_2167 ()
                                          (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int) ->
                                           (k_insert_4614 x__5100 x__5101 x__5102 x__5103))))))))))
                          (l0
                            (loop_2167 ()
                              (fun (x__5096:bool) (x__5097:int) (x__5098:bool) (x__5099:int) ->
                               (k_insert_4614 x__5096 x__5097 x__5098 x__5099)))))))))): X
abstract_term: (if (x2_4613 <= 0) (l0 (k_insert_4614 true x_1053 true x_1053))
                 (l1
                   (ysys_1016 0 (x2_4613 - 1)
                     (fun (xs11_4631:bool) (xs12_4632:int) (xs21_4633:bool) 
                          (xs22_4634:x_1:int[(not xs11_4631) || (not xs21_4633) || (
                                             not (0 <= (x2_4613 - 1))) || 
                                             0 < 0 || x2_4613 - 1 < 0 || 
                                             xs12_4632 <= x_1])
                      ->
                      (if xs21_4633
                        (l1
                          (if (1 = x2_4613)
                            (l0
                              (if (xs22_4634 = xs12_4632)
                                (l1
                                  (if (p11_4797 <=> xs11_4631)
                                    (l1
                                      (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                        (l0
                                          (loop_2167 ()
                                            (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int) ->
                                             (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                                    (l0
                                      (l0
                                        (loop_2167 ()
                                          (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int) ->
                                           (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))))))
                                (l0
                                  (loop_2167 ()
                                    (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                                     (k_insert_4614 x__5108 x__5109 x__5110 x__5111))))))
                            (l1
                              (l1
                                (if (p11_4797 <=> xs11_4631)
                                  (l1
                                    (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                      (l0
                                        (loop_2167 ()
                                          (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int) ->
                                           (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))))
                                  (l0
                                    (l0
                                      (loop_2167 ()
                                        (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int) ->
                                         (k_insert_4614 x__5100 x__5101 x__5102 x__5103))))))))))
                        (l0
                          (loop_2167 ()
                            (fun (x__5096:bool) (x__5097:int) (x__5098:bool) (x__5099:int) ->
                             (k_insert_4614 x__5096 x__5097 x__5098 x__5099))))))))): X
abstract_term: (x2_4613 <= 0): x_1:bool[x_1]
cond: (x1_4612 <= 0); (x_1053 < p12_4798); p11_4797; true
pbs: p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:(x2_4613 <= 0)
tt:false
ff:false

abstract_term: (l0 (k_insert_4614 true x_1053 true x_1053)): X
abstract_term: (k_insert_4614 true x_1053 true x_1053): X
abstract_term: x_1053: x_1:int[(not true) || (not true) || (not (x1_4612 <= x2_4613)) || 
                               x1_4612 < 0 || x2_4613 < 0 || x_1053 <= x_1]
cond: (x2_4613 <= 0); (x1_4612 <= 0); (x_1053 < p12_4798); p11_4797; true
pbs: p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not true) || (not true)) || (not (x1_4612 <= x2_4613))) || (x1_4612 < 0)) || (x2_4613 < 0)) ||
   (x_1053 <= x_1053))
tt:true
ff:false

abstract_term: true: bool
abstract_term: x_1053: int
abstract_term: true: bool
abstract_term: (l1
                 (ysys_1016 0 (x2_4613 - 1)
                   (fun (xs11_4631:bool) (xs12_4632:int) (xs21_4633:bool) 
                        (xs22_4634:x_1:int[(not xs11_4631) || (not xs21_4633) || (
                                           not (0 <= (x2_4613 - 1))) || 
                                           0 < 0 || x2_4613 - 1 < 0 || 
                                           xs12_4632 <= x_1])
                    ->
                    (if xs21_4633
                      (l1
                        (if (1 = x2_4613)
                          (l0
                            (if (xs22_4634 = xs12_4632)
                              (l1
                                (if (p11_4797 <=> xs11_4631)
                                  (l1
                                    (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                      (l0
                                        (loop_2167 ()
                                          (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int) ->
                                           (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                                  (l0
                                    (l0
                                      (loop_2167 ()
                                        (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int) ->
                                         (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))))))
                              (l0
                                (loop_2167 ()
                                  (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                                   (k_insert_4614 x__5108 x__5109 x__5110 x__5111))))))
                          (l1
                            (l1
                              (if (p11_4797 <=> xs11_4631)
                                (l1
                                  (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                    (l0
                                      (loop_2167 ()
                                        (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int) ->
                                         (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))))
                                (l0
                                  (l0
                                    (loop_2167 ()
                                      (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int) ->
                                       (k_insert_4614 x__5100 x__5101 x__5102 x__5103))))))))))
                      (l0
                        (loop_2167 ()
                          (fun (x__5096:bool) (x__5097:int) (x__5098:bool) (x__5099:int) ->
                           (k_insert_4614 x__5096 x__5097 x__5098 x__5099)))))))): X
abstract_term: (ysys_1016 0 (x2_4613 - 1)
                 (fun (xs11_4631:bool) (xs12_4632:int) (xs21_4633:bool) 
                      (xs22_4634:x_1:int[(not xs11_4631) || (not xs21_4633) || (
                                         not (0 <= (x2_4613 - 1))) || 
                                         0 < 0 || x2_4613 - 1 < 0 || 
                                         xs12_4632 <= x_1])
                  ->
                  (if xs21_4633
                    (l1
                      (if (1 = x2_4613)
                        (l0
                          (if (xs22_4634 = xs12_4632)
                            (l1
                              (if (p11_4797 <=> xs11_4631)
                                (l1
                                  (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                    (l0
                                      (loop_2167 ()
                                        (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int) ->
                                         (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                                (l0
                                  (l0
                                    (loop_2167 ()
                                      (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int) ->
                                       (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))))))
                            (l0
                              (loop_2167 ()
                                (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                                 (k_insert_4614 x__5108 x__5109 x__5110 x__5111))))))
                        (l1
                          (l1
                            (if (p11_4797 <=> xs11_4631)
                              (l1
                                (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                  (l0
                                    (loop_2167 ()
                                      (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int) ->
                                       (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))))
                              (l0
                                (l0
                                  (loop_2167 ()
                                    (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int) ->
                                     (k_insert_4614 x__5100 x__5101 x__5102 x__5103))))))))))
                    (l0
                      (loop_2167 ()
                        (fun (x__5096:bool) (x__5097:int) (x__5098:bool) (x__5099:int) ->
                         (k_insert_4614 x__5096 x__5097 x__5098 x__5099))))))): X
abstract_term: (fun (xs11_4631:bool) (xs12_4632:int) (xs21_4633:bool) 
                    (xs22_4634:x_1:int[(not xs11_4631) || (not xs21_4633) || (
                                       not (0 <= (x2_4613 - 1))) || 0 < 0 || 
                                       x2_4613 - 1 < 0 || xs12_4632 <= x_1])
                ->
                (if xs21_4633
                  (l1
                    (if (1 = x2_4613)
                      (l0
                        (if (xs22_4634 = xs12_4632)
                          (l1
                            (if (p11_4797 <=> xs11_4631)
                              (l1
                                (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                  (l0
                                    (loop_2167 ()
                                      (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int) ->
                                       (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                              (l0
                                (l0
                                  (loop_2167 ()
                                    (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int) ->
                                     (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))))))
                          (l0
                            (loop_2167 ()
                              (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                               (k_insert_4614 x__5108 x__5109 x__5110 x__5111))))))
                      (l1
                        (l1
                          (if (p11_4797 <=> xs11_4631)
                            (l1
                              (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                (l0
                                  (loop_2167 ()
                                    (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int) ->
                                     (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))))
                            (l0
                              (l0
                                (loop_2167 ()
                                  (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int) ->
                                   (k_insert_4614 x__5100 x__5101 x__5102 x__5103))))))))))
                  (l0
                    (loop_2167 ()
                      (fun (x__5096:bool) (x__5097:int) (x__5098:bool) (x__5099:int) ->
                       (k_insert_4614 x__5096 x__5097 x__5098 x__5099)))))): (
x_1:bool ->
x_2:int ->
x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (0 <= (x2_4613 - 1))) || 0 < 0 || x2_4613 - 1 < 0 || x_2 <= x_4] ->
X)
abst_arg: xs11_4631, bool
abst_arg: xs12_4632, int
abst_arg: xs21_4633, bool
abst_arg: xs22_4634, x_1:int[(not xs11_4631) || (not xs21_4633) || (not (0 <= (x2_4613 - 1))) || 
                             0 < 0 || x2_4613 - 1 < 0 || xs12_4632 <= x_1]
abst_arg: xs11_4631, bool
abst_arg: xs12_4632, int
abst_arg: xs21_4633, bool
abst_arg: xs22_4634, x_1:int[(not xs11_4631) || (not xs21_4633) || (not (0 <= (x2_4613 - 1))) || 
                             0 < 0 || x2_4613 - 1 < 0 || xs12_4632 <= x_1]
abstract_term: (if xs21_4633
                 (l1
                   (if (1 = x2_4613)
                     (l0
                       (if (xs22_4634 = xs12_4632)
                         (l1
                           (if (p11_4797 <=> xs11_4631)
                             (l1
                               (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                                 (l0
                                   (loop_2167 ()
                                     (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int) ->
                                      (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                             (l0
                               (l0
                                 (loop_2167 ()
                                   (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int) ->
                                    (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))))))
                         (l0
                           (loop_2167 ()
                             (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                              (k_insert_4614 x__5108 x__5109 x__5110 x__5111))))))
                     (l1
                       (l1
                         (if (p11_4797 <=> xs11_4631)
                           (l1
                             (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                               (l0
                                 (loop_2167 ()
                                   (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int) ->
                                    (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))))
                           (l0
                             (l0
                               (loop_2167 ()
                                 (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int) ->
                                  (k_insert_4614 x__5100 x__5101 x__5102 x__5103))))))))))
                 (l0
                   (loop_2167 ()
                     (fun (x__5096:bool) (x__5097:int) (x__5098:bool) (x__5099:int) ->
                      (k_insert_4614 x__5096 x__5097 x__5098 x__5099))))): X
abstract_term: xs21_4633: x_1:bool[x_1]
cond: (not (x2_4613 <= 0)); (x1_4612 <= 0); (x_1053 < p12_4798); p11_4797; true
pbs: xs22_4634 := ((((((not xs11_4631) || (not xs21_4633)) || (not (0 <= (x2_4613 - 1)))) || (0 < 0)) ||
                    ((x2_4613 - 1) < 0))
                   || (xs12_4632 <= xs22_4634));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:xs21_4633
tt:false
ff:false

abstract_term: (l1
                 (if (1 = x2_4613)
                   (l0
                     (if (xs22_4634 = xs12_4632)
                       (l1
                         (if (p11_4797 <=> xs11_4631)
                           (l1
                             (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                               (l0
                                 (loop_2167 ()
                                   (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int) ->
                                    (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                           (l0
                             (l0
                               (loop_2167 ()
                                 (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int) ->
                                  (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))))))
                       (l0
                         (loop_2167 ()
                           (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                            (k_insert_4614 x__5108 x__5109 x__5110 x__5111))))))
                   (l1
                     (l1
                       (if (p11_4797 <=> xs11_4631)
                         (l1
                           (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                             (l0
                               (loop_2167 ()
                                 (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int) ->
                                  (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))))
                         (l0
                           (l0
                             (loop_2167 ()
                               (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int) ->
                                (k_insert_4614 x__5100 x__5101 x__5102 x__5103)))))))))): X
abstract_term: (if (1 = x2_4613)
                 (l0
                   (if (xs22_4634 = xs12_4632)
                     (l1
                       (if (p11_4797 <=> xs11_4631)
                         (l1
                           (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                             (l0
                               (loop_2167 ()
                                 (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int) ->
                                  (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                         (l0
                           (l0
                             (loop_2167 ()
                               (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int) ->
                                (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))))))
                     (l0
                       (loop_2167 ()
                         (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                          (k_insert_4614 x__5108 x__5109 x__5110 x__5111))))))
                 (l1
                   (l1
                     (if (p11_4797 <=> xs11_4631)
                       (l1
                         (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                           (l0
                             (loop_2167 ()
                               (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int) ->
                                (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))))
                       (l0
                         (l0
                           (loop_2167 ()
                             (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int) ->
                              (k_insert_4614 x__5100 x__5101 x__5102 x__5103))))))))): X
abstract_term: (1 = x2_4613): x_1:bool[x_1]
cond: xs21_4633; (not (x2_4613 <= 0)); (x1_4612 <= 0); (x_1053 < p12_4798); p11_4797; true
pbs: xs22_4634 := ((((((not xs11_4631) || (not xs21_4633)) || (not (0 <= (x2_4613 - 1)))) || (0 < 0)) ||
                    ((x2_4613 - 1) < 0))
                   || (xs12_4632 <= xs22_4634));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:(1 = x2_4613)
tt:false
ff:false

abstract_term: (l0
                 (if (xs22_4634 = xs12_4632)
                   (l1
                     (if (p11_4797 <=> xs11_4631)
                       (l1
                         (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                           (l0
                             (loop_2167 ()
                               (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int) ->
                                (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                       (l0
                         (l0
                           (loop_2167 ()
                             (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int) ->
                              (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))))))
                   (l0
                     (loop_2167 ()
                       (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                        (k_insert_4614 x__5108 x__5109 x__5110 x__5111)))))): X
abstract_term: (if (xs22_4634 = xs12_4632)
                 (l1
                   (if (p11_4797 <=> xs11_4631)
                     (l1
                       (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                         (l0
                           (loop_2167 ()
                             (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int) ->
                              (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                     (l0
                       (l0
                         (loop_2167 ()
                           (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int) ->
                            (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))))))
                 (l0
                   (loop_2167 ()
                     (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                      (k_insert_4614 x__5108 x__5109 x__5110 x__5111))))): X
abstract_term: (xs22_4634 = xs12_4632): x_1:bool[x_1]
cond: (1 = x2_4613); xs21_4633; (not (x2_4613 <= 0)); (x1_4612 <= 0); (x_1053 < p12_4798); p11_4797; true
pbs: xs22_4634 := ((((((not xs11_4631) || (not xs21_4633)) || (not (0 <= (x2_4613 - 1)))) || (0 < 0)) ||
                    ((x2_4613 - 1) < 0))
                   || (xs12_4632 <= xs22_4634));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:(xs22_4634 = xs12_4632)
tt:false
ff:false

abstract_term: (l1
                 (if (p11_4797 <=> xs11_4631)
                   (l1
                     (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                       (l0
                         (loop_2167 ()
                           (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int) ->
                            (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                   (l0
                     (l0
                       (loop_2167 ()
                         (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int) ->
                          (k_insert_4614 x__5112 x__5113 x__5114 x__5115))))))): X
abstract_term: (if (p11_4797 <=> xs11_4631)
                 (l1
                   (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                     (l0
                       (loop_2167 ()
                         (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int) ->
                          (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))))
                 (l0
                   (l0
                     (loop_2167 ()
                       (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int) ->
                        (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))))): X
abstract_term: (p11_4797 <=> xs11_4631): x_1:bool[x_1]
cond: (xs22_4634 = xs12_4632); (1 = x2_4613); xs21_4633; (not (x2_4613 <= 0)); (
      x1_4612 <= 0); (x_1053 < p12_4798); p11_4797; true
pbs: xs22_4634 := ((((((not xs11_4631) || (not xs21_4633)) || (not (0 <= (x2_4613 - 1)))) || (0 < 0)) ||
                    ((x2_4613 - 1) < 0))
                   || (xs12_4632 <= xs22_4634));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:(p11_4797 <=> xs11_4631)
tt:false
ff:false

abstract_term: (l1
                 (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                   (l0
                     (loop_2167 ()
                       (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int) ->
                        (k_insert_4614 x__5116 x__5117 x__5118 x__5119)))))): X
abstract_term: (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                 (l0
                   (loop_2167 ()
                     (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int) ->
                      (k_insert_4614 x__5116 x__5117 x__5118 x__5119))))): X
abstract_term: (xs12_4632 = p12_4798): x_1:bool[x_1]
cond: (p11_4797 <=> xs11_4631); (xs22_4634 = xs12_4632); (1 = x2_4613); xs21_4633; (
      not (x2_4613 <= 0)); (x1_4612 <= 0); (x_1053 < p12_4798); p11_4797; true
pbs: xs22_4634 := ((((((not xs11_4631) || (not xs21_4633)) || (not (0 <= (x2_4613 - 1)))) || (0 < 0)) ||
                    ((x2_4613 - 1) < 0))
                   || (xs12_4632 <= xs22_4634));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:(xs12_4632 = p12_4798)
tt:false
ff:false

abstract_term: (l1 (k_insert_4614 true x_1053 true xs22_4634)): X
abstract_term: (k_insert_4614 true x_1053 true xs22_4634): X
abstract_term: xs22_4634: x_1:int[(not true) || (not true) || (not (x1_4612 <= x2_4613)) || 
                                  x1_4612 < 0 || x2_4613 < 0 || x_1053 <= x_1]
cond: (xs12_4632 = p12_4798); (p11_4797 <=> xs11_4631); (xs22_4634 = xs12_4632); (
      1 = x2_4613); xs21_4633; (not (x2_4613 <= 0)); (x1_4612 <= 0); (
      x_1053 < p12_4798); p11_4797; true
pbs: xs22_4634 := ((((((not xs11_4631) || (not xs21_4633)) || (not (0 <= (x2_4613 - 1)))) || (0 < 0)) ||
                    ((x2_4613 - 1) < 0))
                   || (xs12_4632 <= xs22_4634));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not true) || (not true)) || (not (x1_4612 <= x2_4613))) || (x1_4612 < 0)) || (x2_4613 < 0)) ||
   (x_1053 <= xs22_4634))
tt:true
ff:false

abstract_term: true: bool
abstract_term: x_1053: int
abstract_term: true: bool
abstract_term: (l0
                 (loop_2167 ()
                   (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int) ->
                    (k_insert_4614 x__5116 x__5117 x__5118 x__5119)))): X
abstract_term: (loop_2167 ()
                 (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int) ->
                  (k_insert_4614 x__5116 x__5117 x__5118 x__5119))): X
abstract_term: (fun (x__5116:bool) (x__5117:int) (x__5118:bool) (x__5119:int) ->
                (k_insert_4614 x__5116 x__5117 x__5118 x__5119)): (bool -> int -> bool -> int -> X)
abst_arg: x__5116, bool
abst_arg: x__5117, int
abst_arg: x__5118, bool
abst_arg: x__5119, int
abst_arg: x__5116, bool
abst_arg: x__5117, int
abst_arg: x__5118, bool
abst_arg: x__5119, int
abstract_term: (k_insert_4614 x__5116 x__5117 x__5118 x__5119): X
abstract_term: x__5119: x_1:int[(not x__5116) || (not x__5118) || (not (x1_4612 <= x2_4613)) || 
                                x1_4612 < 0 || x2_4613 < 0 || x__5117 <= x_1]
cond: (not (xs12_4632 = p12_4798)); (p11_4797 <=> xs11_4631); (xs22_4634 = xs12_4632); (
      1 = x2_4613); xs21_4633; (not (x2_4613 <= 0)); (x1_4612 <= 0); (
      x_1053 < p12_4798); p11_4797; true
pbs: xs22_4634 := ((((((not xs11_4631) || (not xs21_4633)) || (not (0 <= (x2_4613 - 1)))) || (0 < 0)) ||
                    ((x2_4613 - 1) < 0))
                   || (xs12_4632 <= xs22_4634));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not x__5116) || (not x__5118)) || (not (x1_4612 <= x2_4613))) || (x1_4612 < 0)) || (x2_4613 < 0)) ||
   (x__5117 <= x__5119))
tt:false
ff:false

abstract_term: x__5118: bool
abstract_term: x__5117: int
abstract_term: x__5116: bool
abstract_term: (): unit
abstract_term: (l0
                 (l0
                   (loop_2167 ()
                     (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int) ->
                      (k_insert_4614 x__5112 x__5113 x__5114 x__5115))))): X
abstract_term: (l0
                 (loop_2167 ()
                   (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int) ->
                    (k_insert_4614 x__5112 x__5113 x__5114 x__5115)))): X
abstract_term: (loop_2167 ()
                 (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int) ->
                  (k_insert_4614 x__5112 x__5113 x__5114 x__5115))): X
abstract_term: (fun (x__5112:bool) (x__5113:int) (x__5114:bool) (x__5115:int) ->
                (k_insert_4614 x__5112 x__5113 x__5114 x__5115)): (bool -> int -> bool -> int -> X)
abst_arg: x__5112, bool
abst_arg: x__5113, int
abst_arg: x__5114, bool
abst_arg: x__5115, int
abst_arg: x__5112, bool
abst_arg: x__5113, int
abst_arg: x__5114, bool
abst_arg: x__5115, int
abstract_term: (k_insert_4614 x__5112 x__5113 x__5114 x__5115): X
abstract_term: x__5115: x_1:int[(not x__5112) || (not x__5114) || (not (x1_4612 <= x2_4613)) || 
                                x1_4612 < 0 || x2_4613 < 0 || x__5113 <= x_1]
cond: (not (p11_4797 <=> xs11_4631)); (xs22_4634 = xs12_4632); (1 = x2_4613); xs21_4633; (
      not (x2_4613 <= 0)); (x1_4612 <= 0); (x_1053 < p12_4798); p11_4797; true
pbs: xs22_4634 := ((((((not xs11_4631) || (not xs21_4633)) || (not (0 <= (x2_4613 - 1)))) || (0 < 0)) ||
                    ((x2_4613 - 1) < 0))
                   || (xs12_4632 <= xs22_4634));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not x__5112) || (not x__5114)) || (not (x1_4612 <= x2_4613))) || (x1_4612 < 0)) || (x2_4613 < 0)) ||
   (x__5113 <= x__5115))
tt:false
ff:false

abstract_term: x__5114: bool
abstract_term: x__5113: int
abstract_term: x__5112: bool
abstract_term: (): unit
abstract_term: (l0
                 (loop_2167 ()
                   (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                    (k_insert_4614 x__5108 x__5109 x__5110 x__5111)))): X
abstract_term: (loop_2167 ()
                 (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                  (k_insert_4614 x__5108 x__5109 x__5110 x__5111))): X
abstract_term: (fun (x__5108:bool) (x__5109:int) (x__5110:bool) (x__5111:int) ->
                (k_insert_4614 x__5108 x__5109 x__5110 x__5111)): (bool -> int -> bool -> int -> X)
abst_arg: x__5108, bool
abst_arg: x__5109, int
abst_arg: x__5110, bool
abst_arg: x__5111, int
abst_arg: x__5108, bool
abst_arg: x__5109, int
abst_arg: x__5110, bool
abst_arg: x__5111, int
abstract_term: (k_insert_4614 x__5108 x__5109 x__5110 x__5111): X
abstract_term: x__5111: x_1:int[(not x__5108) || (not x__5110) || (not (x1_4612 <= x2_4613)) || 
                                x1_4612 < 0 || x2_4613 < 0 || x__5109 <= x_1]
cond: (not (xs22_4634 = xs12_4632)); (1 = x2_4613); xs21_4633; (not (x2_4613 <= 0)); (
      x1_4612 <= 0); (x_1053 < p12_4798); p11_4797; true
pbs: xs22_4634 := ((((((not xs11_4631) || (not xs21_4633)) || (not (0 <= (x2_4613 - 1)))) || (0 < 0)) ||
                    ((x2_4613 - 1) < 0))
                   || (xs12_4632 <= xs22_4634));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not x__5108) || (not x__5110)) || (not (x1_4612 <= x2_4613))) || (x1_4612 < 0)) || (x2_4613 < 0)) ||
   (x__5109 <= x__5111))
tt:false
ff:false

abstract_term: x__5110: bool
abstract_term: x__5109: int
abstract_term: x__5108: bool
abstract_term: (): unit
abstract_term: (l1
                 (l1
                   (if (p11_4797 <=> xs11_4631)
                     (l1
                       (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                         (l0
                           (loop_2167 ()
                             (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int) ->
                              (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))))
                     (l0
                       (l0
                         (loop_2167 ()
                           (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int) ->
                            (k_insert_4614 x__5100 x__5101 x__5102 x__5103)))))))): X
abstract_term: (l1
                 (if (p11_4797 <=> xs11_4631)
                   (l1
                     (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                       (l0
                         (loop_2167 ()
                           (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int) ->
                            (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))))
                   (l0
                     (l0
                       (loop_2167 ()
                         (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int) ->
                          (k_insert_4614 x__5100 x__5101 x__5102 x__5103))))))): X
abstract_term: (if (p11_4797 <=> xs11_4631)
                 (l1
                   (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                     (l0
                       (loop_2167 ()
                         (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int) ->
                          (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))))
                 (l0
                   (l0
                     (loop_2167 ()
                       (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int) ->
                        (k_insert_4614 x__5100 x__5101 x__5102 x__5103)))))): X
abstract_term: (p11_4797 <=> xs11_4631): x_1:bool[x_1]
cond: (not (1 = x2_4613)); xs21_4633; (not (x2_4613 <= 0)); (x1_4612 <= 0); (x_1053 < p12_4798); p11_4797; true
pbs: xs22_4634 := ((((((not xs11_4631) || (not xs21_4633)) || (not (0 <= (x2_4613 - 1)))) || (0 < 0)) ||
                    ((x2_4613 - 1) < 0))
                   || (xs12_4632 <= xs22_4634));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:(p11_4797 <=> xs11_4631)
tt:false
ff:false

abstract_term: (l1
                 (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                   (l0
                     (loop_2167 ()
                       (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int) ->
                        (k_insert_4614 x__5104 x__5105 x__5106 x__5107)))))): X
abstract_term: (if (xs12_4632 = p12_4798) (l1 (k_insert_4614 true x_1053 true xs22_4634))
                 (l0
                   (loop_2167 ()
                     (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int) ->
                      (k_insert_4614 x__5104 x__5105 x__5106 x__5107))))): X
abstract_term: (xs12_4632 = p12_4798): x_1:bool[x_1]
cond: (p11_4797 <=> xs11_4631); (not (1 = x2_4613)); xs21_4633; (not (x2_4613 <= 0)); (
      x1_4612 <= 0); (x_1053 < p12_4798); p11_4797; true
pbs: xs22_4634 := ((((((not xs11_4631) || (not xs21_4633)) || (not (0 <= (x2_4613 - 1)))) || (0 < 0)) ||
                    ((x2_4613 - 1) < 0))
                   || (xs12_4632 <= xs22_4634));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:(xs12_4632 = p12_4798)
tt:false
ff:false

abstract_term: (l1 (k_insert_4614 true x_1053 true xs22_4634)): X
abstract_term: (k_insert_4614 true x_1053 true xs22_4634): X
abstract_term: xs22_4634: x_1:int[(not true) || (not true) || (not (x1_4612 <= x2_4613)) || 
                                  x1_4612 < 0 || x2_4613 < 0 || x_1053 <= x_1]
cond: (xs12_4632 = p12_4798); (p11_4797 <=> xs11_4631); (not (1 = x2_4613)); xs21_4633; (
      not (x2_4613 <= 0)); (x1_4612 <= 0); (x_1053 < p12_4798); p11_4797; true
pbs: xs22_4634 := ((((((not xs11_4631) || (not xs21_4633)) || (not (0 <= (x2_4613 - 1)))) || (0 < 0)) ||
                    ((x2_4613 - 1) < 0))
                   || (xs12_4632 <= xs22_4634));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not true) || (not true)) || (not (x1_4612 <= x2_4613))) || (x1_4612 < 0)) || (x2_4613 < 0)) ||
   (x_1053 <= xs22_4634))
tt:xs22_4634
ff:false

abstract_term: true: bool
abstract_term: x_1053: int
abstract_term: true: bool
abstract_term: (l0
                 (loop_2167 ()
                   (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int) ->
                    (k_insert_4614 x__5104 x__5105 x__5106 x__5107)))): X
abstract_term: (loop_2167 ()
                 (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int) ->
                  (k_insert_4614 x__5104 x__5105 x__5106 x__5107))): X
abstract_term: (fun (x__5104:bool) (x__5105:int) (x__5106:bool) (x__5107:int) ->
                (k_insert_4614 x__5104 x__5105 x__5106 x__5107)): (bool -> int -> bool -> int -> X)
abst_arg: x__5104, bool
abst_arg: x__5105, int
abst_arg: x__5106, bool
abst_arg: x__5107, int
abst_arg: x__5104, bool
abst_arg: x__5105, int
abst_arg: x__5106, bool
abst_arg: x__5107, int
abstract_term: (k_insert_4614 x__5104 x__5105 x__5106 x__5107): X
abstract_term: x__5107: x_1:int[(not x__5104) || (not x__5106) || (not (x1_4612 <= x2_4613)) || 
                                x1_4612 < 0 || x2_4613 < 0 || x__5105 <= x_1]
cond: (not (xs12_4632 = p12_4798)); (p11_4797 <=> xs11_4631); (not (1 = x2_4613)); xs21_4633; (
      not (x2_4613 <= 0)); (x1_4612 <= 0); (x_1053 < p12_4798); p11_4797; true
pbs: xs22_4634 := ((((((not xs11_4631) || (not xs21_4633)) || (not (0 <= (x2_4613 - 1)))) || (0 < 0)) ||
                    ((x2_4613 - 1) < 0))
                   || (xs12_4632 <= xs22_4634));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not x__5104) || (not x__5106)) || (not (x1_4612 <= x2_4613))) || (x1_4612 < 0)) || (x2_4613 < 0)) ||
   (x__5105 <= x__5107))
tt:false
ff:false

abstract_term: x__5106: bool
abstract_term: x__5105: int
abstract_term: x__5104: bool
abstract_term: (): unit
abstract_term: (l0
                 (l0
                   (loop_2167 ()
                     (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int) ->
                      (k_insert_4614 x__5100 x__5101 x__5102 x__5103))))): X
abstract_term: (l0
                 (loop_2167 ()
                   (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int) ->
                    (k_insert_4614 x__5100 x__5101 x__5102 x__5103)))): X
abstract_term: (loop_2167 ()
                 (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int) ->
                  (k_insert_4614 x__5100 x__5101 x__5102 x__5103))): X
abstract_term: (fun (x__5100:bool) (x__5101:int) (x__5102:bool) (x__5103:int) ->
                (k_insert_4614 x__5100 x__5101 x__5102 x__5103)): (bool -> int -> bool -> int -> X)
abst_arg: x__5100, bool
abst_arg: x__5101, int
abst_arg: x__5102, bool
abst_arg: x__5103, int
abst_arg: x__5100, bool
abst_arg: x__5101, int
abst_arg: x__5102, bool
abst_arg: x__5103, int
abstract_term: (k_insert_4614 x__5100 x__5101 x__5102 x__5103): X
abstract_term: x__5103: x_1:int[(not x__5100) || (not x__5102) || (not (x1_4612 <= x2_4613)) || 
                                x1_4612 < 0 || x2_4613 < 0 || x__5101 <= x_1]
cond: (not (p11_4797 <=> xs11_4631)); (not (1 = x2_4613)); xs21_4633; (
      not (x2_4613 <= 0)); (x1_4612 <= 0); (x_1053 < p12_4798); p11_4797; true
pbs: xs22_4634 := ((((((not xs11_4631) || (not xs21_4633)) || (not (0 <= (x2_4613 - 1)))) || (0 < 0)) ||
                    ((x2_4613 - 1) < 0))
                   || (xs12_4632 <= xs22_4634));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not x__5100) || (not x__5102)) || (not (x1_4612 <= x2_4613))) || (x1_4612 < 0)) || (x2_4613 < 0)) ||
   (x__5101 <= x__5103))
tt:false
ff:false

abstract_term: x__5102: bool
abstract_term: x__5101: int
abstract_term: x__5100: bool
abstract_term: (): unit
abstract_term: (l0
                 (loop_2167 ()
                   (fun (x__5096:bool) (x__5097:int) (x__5098:bool) (x__5099:int) ->
                    (k_insert_4614 x__5096 x__5097 x__5098 x__5099)))): X
abstract_term: (loop_2167 ()
                 (fun (x__5096:bool) (x__5097:int) (x__5098:bool) (x__5099:int) ->
                  (k_insert_4614 x__5096 x__5097 x__5098 x__5099))): X
abstract_term: (fun (x__5096:bool) (x__5097:int) (x__5098:bool) (x__5099:int) ->
                (k_insert_4614 x__5096 x__5097 x__5098 x__5099)): (bool -> int -> bool -> int -> X)
abst_arg: x__5096, bool
abst_arg: x__5097, int
abst_arg: x__5098, bool
abst_arg: x__5099, int
abst_arg: x__5096, bool
abst_arg: x__5097, int
abst_arg: x__5098, bool
abst_arg: x__5099, int
abstract_term: (k_insert_4614 x__5096 x__5097 x__5098 x__5099): X
abstract_term: x__5099: x_1:int[(not x__5096) || (not x__5098) || (not (x1_4612 <= x2_4613)) || 
                                x1_4612 < 0 || x2_4613 < 0 || x__5097 <= x_1]
cond: (not xs21_4633); (not (x2_4613 <= 0)); (x1_4612 <= 0); (x_1053 < p12_4798); p11_4797; true
pbs: xs22_4634 := ((((((not xs11_4631) || (not xs21_4633)) || (not (0 <= (x2_4613 - 1)))) || (0 < 0)) ||
                    ((x2_4613 - 1) < 0))
                   || (xs12_4632 <= xs22_4634));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not x__5096) || (not x__5098)) || (not (x1_4612 <= x2_4613))) || (x1_4612 < 0)) || (x2_4613 < 0)) ||
   (x__5097 <= x__5099))
tt:false
ff:false

abstract_term: x__5098: bool
abstract_term: x__5097: int
abstract_term: x__5096: bool
abstract_term: (): unit
abstract_term: (x2_4613 - 1): int
abstract_term: 0: int
abstract_term: (l1
                 (if (x2_4613 <= 0)
                   (l0
                     (ysys_1016 (x1_4612 - 1) 0
                       (fun (p11_4641:bool) (p12_4642:int) (p21_4643:bool) 
                            (p22_4644:x_1:int[(not p11_4641) || (not p21_4643) || (
                                              not ((x1_4612 - 1) <= 0)) || 
                                              x1_4612 - 1 < 0 || 0 < 0 || 
                                              p12_4642 <= x_1])
                        -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
                   (l1
                     (ysys_1016 (x1_4612 - 1) (x2_4613 - 1)
                       (fun (x__5092:bool) (x__5093:int) (x__5094:bool) 
                            (x__5095:x_1:int[(not x__5092) || (not x__5094) || 
                                             (not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                                             x1_4612 - 1 < 0 || x2_4613 - 1 < 0 || 
                                             x__5093 <= x_1])
                        -> (k_insert_4614 x__5092 x__5093 x__5094 x__5095)))))): X
abstract_term: (if (x2_4613 <= 0)
                 (l0
                   (ysys_1016 (x1_4612 - 1) 0
                     (fun (p11_4641:bool) (p12_4642:int) (p21_4643:bool) 
                          (p22_4644:x_1:int[(not p11_4641) || (not p21_4643) || (
                                            not ((x1_4612 - 1) <= 0)) || 
                                            x1_4612 - 1 < 0 || 0 < 0 || 
                                            p12_4642 <= x_1])
                      -> (k_insert_4614 p11_4641 p12_4642 true x_1053))))
                 (l1
                   (ysys_1016 (x1_4612 - 1) (x2_4613 - 1)
                     (fun (x__5092:bool) (x__5093:int) (x__5094:bool) 
                          (x__5095:x_1:int[(not x__5092) || (not x__5094) || (
                                           not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                                           x1_4612 - 1 < 0 || x2_4613 - 1 < 0 || 
                                           x__5093 <= x_1])
                      -> (k_insert_4614 x__5092 x__5093 x__5094 x__5095))))): X
abstract_term: (x2_4613 <= 0): x_1:bool[x_1]
cond: (not (x1_4612 <= 0)); (x_1053 < p12_4798); p11_4797; true
pbs: p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:(x2_4613 <= 0)
tt:false
ff:false

abstract_term: (l0
                 (ysys_1016 (x1_4612 - 1) 0
                   (fun (p11_4641:bool) (p12_4642:int) (p21_4643:bool) 
                        (p22_4644:x_1:int[(not p11_4641) || (not p21_4643) || (
                                          not ((x1_4612 - 1) <= 0)) || 
                                          x1_4612 - 1 < 0 || 0 < 0 || 
                                          p12_4642 <= x_1])
                    -> (k_insert_4614 p11_4641 p12_4642 true x_1053)))): X
abstract_term: (ysys_1016 (x1_4612 - 1) 0
                 (fun (p11_4641:bool) (p12_4642:int) (p21_4643:bool) 
                      (p22_4644:x_1:int[(not p11_4641) || (not p21_4643) || (
                                        not ((x1_4612 - 1) <= 0)) || 
                                        x1_4612 - 1 < 0 || 0 < 0 || p12_4642 <= x_1])
                  -> (k_insert_4614 p11_4641 p12_4642 true x_1053))): X
abstract_term: (fun (p11_4641:bool) (p12_4642:int) (p21_4643:bool) 
                    (p22_4644:x_1:int[(not p11_4641) || (not p21_4643) || (
                                      not ((x1_4612 - 1) <= 0)) || x1_4612 - 1 < 0 || 
                                      0 < 0 || p12_4642 <= x_1])
                -> (k_insert_4614 p11_4641 p12_4642 true x_1053)): (x_1:bool ->
                                                                    x_2:int ->
                                                                    x_3:bool ->
                                                                    x_4:int[
                                                                    (
                                                                    not x_1) || (
                                                                    not x_3) || (
                                                                    not ((x1_4612 - 1) <= 0)) || 
                                                                    x1_4612 - 1 < 0 || 
                                                                    0 < 0 || 
                                                                    x_2 <= x_4] -> X)
abst_arg: p11_4641, bool
abst_arg: p12_4642, int
abst_arg: p21_4643, bool
abst_arg: p22_4644, x_1:int[(not p11_4641) || (not p21_4643) || (not ((x1_4612 - 1) <= 0)) || 
                            x1_4612 - 1 < 0 || 0 < 0 || p12_4642 <= x_1]
abst_arg: p11_4641, bool
abst_arg: p12_4642, int
abst_arg: p21_4643, bool
abst_arg: p22_4644, x_1:int[(not p11_4641) || (not p21_4643) || (not ((x1_4612 - 1) <= 0)) || 
                            x1_4612 - 1 < 0 || 0 < 0 || p12_4642 <= x_1]
abstract_term: (k_insert_4614 p11_4641 p12_4642 true x_1053): X
abstract_term: x_1053: x_1:int[(not p11_4641) || (not true) || (not (x1_4612 <= x2_4613)) || 
                               x1_4612 < 0 || x2_4613 < 0 || p12_4642 <= x_1]
cond: (x2_4613 <= 0); (not (x1_4612 <= 0)); (x_1053 < p12_4798); p11_4797; true
pbs: p22_4644 := ((((((not p11_4641) || (not p21_4643)) || (not ((x1_4612 - 1) <= 0))) || ((x1_4612 - 1) < 0)) ||
                   (0 < 0))
                  || (p12_4642 <= p22_4644));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not p11_4641) || (not true)) || (not (x1_4612 <= x2_4613))) || (x1_4612 < 0)) || (x2_4613 < 0)) ||
   (p12_4642 <= x_1053))
tt:true
ff:false

abstract_term: true: bool
abstract_term: p12_4642: int
abstract_term: p11_4641: bool
abstract_term: 0: int
abstract_term: (x1_4612 - 1): int
abstract_term: (l1
                 (ysys_1016 (x1_4612 - 1) (x2_4613 - 1)
                   (fun (x__5092:bool) (x__5093:int) (x__5094:bool) 
                        (x__5095:x_1:int[(not x__5092) || (not x__5094) || (
                                         not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                                         x1_4612 - 1 < 0 || x2_4613 - 1 < 0 || 
                                         x__5093 <= x_1])
                    -> (k_insert_4614 x__5092 x__5093 x__5094 x__5095)))): X
abstract_term: (ysys_1016 (x1_4612 - 1) (x2_4613 - 1)
                 (fun (x__5092:bool) (x__5093:int) (x__5094:bool) 
                      (x__5095:x_1:int[(not x__5092) || (not x__5094) || (
                                       not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                                       x1_4612 - 1 < 0 || x2_4613 - 1 < 0 || 
                                       x__5093 <= x_1])
                  -> (k_insert_4614 x__5092 x__5093 x__5094 x__5095))): X
abstract_term: (fun (x__5092:bool) (x__5093:int) (x__5094:bool) 
                    (x__5095:x_1:int[(not x__5092) || (not x__5094) || (
                                     not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                                     x1_4612 - 1 < 0 || x2_4613 - 1 < 0 || 
                                     x__5093 <= x_1])
                -> (k_insert_4614 x__5092 x__5093 x__5094 x__5095)): (
x_1:bool ->
x_2:int ->
x_3:bool ->
x_4:int[(not x_1) || (not x_3) || (not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
        x1_4612 - 1 < 0 || x2_4613 - 1 < 0 || x_2 <= x_4]
-> X)
abst_arg: x__5092, bool
abst_arg: x__5093, int
abst_arg: x__5094, bool
abst_arg: x__5095, x_1:int[(not x__5092) || (not x__5094) || (not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                           x1_4612 - 1 < 0 || x2_4613 - 1 < 0 || x__5093 <= x_1]
abst_arg: x__5092, bool
abst_arg: x__5093, int
abst_arg: x__5094, bool
abst_arg: x__5095, x_1:int[(not x__5092) || (not x__5094) || (not ((x1_4612 - 1) <= (x2_4613 - 1))) || 
                           x1_4612 - 1 < 0 || x2_4613 - 1 < 0 || x__5093 <= x_1]
abstract_term: (k_insert_4614 x__5092 x__5093 x__5094 x__5095): X
abstract_term: x__5095: x_1:int[(not x__5092) || (not x__5094) || (not (x1_4612 <= x2_4613)) || 
                                x1_4612 < 0 || x2_4613 < 0 || x__5093 <= x_1]
cond: (not (x2_4613 <= 0)); (not (x1_4612 <= 0)); (x_1053 < p12_4798); p11_4797; true
pbs: x__5095 := ((((((not x__5092) || (not x__5094)) || (not ((x1_4612 - 1) <= (x2_4613 - 1)))) || ((x1_4612 - 1) < 0))
                  || ((x2_4613 - 1) < 0))
                 || (x__5093 <= x__5095));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not x__5092) || (not x__5094)) || (not (x1_4612 <= x2_4613))) || (x1_4612 < 0)) || (x2_4613 < 0)) ||
   (x__5093 <= x__5095))
tt:x__5095
ff:false

abstract_term: x__5094: bool
abstract_term: x__5093: int
abstract_term: x__5092: bool
abstract_term: (x2_4613 - 1): int
abstract_term: (x1_4612 - 1): int
abstract_term: (l1
                 (insert_1014 x_1053
                   (fun (x1_5042:int) (x2_5043:int) 
                        (k_insert_ys'ys'_5044:(x_1:bool ->
                                               x_2:int ->
                                               x_3:bool ->
                                               x_4:int[(not x_1) || (
                                                       not x_3) || (not (x1_5042 <= x2_5043)) || 
                                                       x1_5042 < 0 || 
                                                       x2_5043 < 0 || 
                                                       x_2 <= x_4]
                                               -> X))
                    ->
                    (ysys_1016 (x1_5042 + 1) (x2_5043 + 1)
                      (fun (x__5088:bool) (x__5089:int) (x__5090:bool) 
                           (x__5091:x_1:int[(not x__5088) || (not x__5090) || 
                                            (not ((x1_5042 + 1) <= (x2_5043 + 1))) || 
                                            x1_5042 + 1 < 0 || x2_5043 + 1 < 0 || 
                                            x__5089 <= x_1])
                       -> (k_insert_ys'ys'_5044 x__5088 x__5089 x__5090 x__5091))))
                   (fun (ys''ys''_4650:(x_1:int ->
                                        x_2:int ->
                                        (x_4:bool ->
                                         x_5:int ->
                                         x_6:bool ->
                                         x_7:int[(not x_4) || (not x_6) || (
                                                 not (x_1 <= x_2)) || 
                                                 x_1 < 0 || x_2 < 0 || 
                                                 x_5 <= x_7]
                                         -> X)
                                        -> X))
                    ->
                    (k_insert_2260
                      (fun (x1_4664:int) (x2_4665:int) 
                           (k_insert_4666:(x_1:bool ->
                                           x_2:int ->
                                           x_3:bool ->
                                           x_4:int[(not x_1) || (not x_3) || (
                                                   not (x1_4664 <= x2_4665)) || 
                                                   x1_4664 < 0 || x2_4665 < 0 || 
                                                   x_2 <= x_4]
                                           -> X))
                       ->
                       (if (x1_4664 <= 0)
                         (l0
                           (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                             (l1
                               (ysys_1016 0 (x2_4665 - 1)
                                 (fun (xs11_4683:bool) (xs12_4684:int) (xs21_4685:bool) 
                                      (xs22_4686:x_1:int[(not xs11_4683) || (
                                                         not xs21_4685) || (
                                                         not (0 <= (x2_4665 - 1))) || 
                                                         0 < 0 || x2_4665 - 1 < 0 || 
                                                         xs12_4684 <= x_1])
                                  ->
                                  (if xs21_4685
                                    (l1
                                      (if (1 = x2_4665)
                                        (l0
                                          (if (xs22_4686 = xs12_4684)
                                            (l1
                                              (if (p11_4797 <=> xs11_4683)
                                                (l1
                                                  (if (xs12_4684 = p12_4798)
                                                    (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                    (l0
                                                      (loop_2167 ()
                                                        (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int)
                                                         -> (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                                                (l0
                                                  (l0
                                                    (loop_2167 ()
                                                      (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
                                                       (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                                            (l0
                                              (loop_2167 ()
                                                (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                                                 (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))))
                                        (l1
                                          (l1
                                            (if (p11_4797 <=> xs11_4683)
                                              (l1
                                                (if (xs12_4684 = p12_4798)
                                                  (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                  (l0
                                                    (loop_2167 ()
                                                      (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
                                                       (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                                              (l0
                                                (l0
                                                  (loop_2167 ()
                                                    (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
                                                     (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))))))))
                                    (l0
                                      (loop_2167 ()
                                        (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                                         (k_insert_4666 x__5064 x__5065 x__5066 x__5067))))))))))
                         (l1
                           (if (x2_4665 <= 0)
                             (l0
                               (ys''ys''_4650 x1_4664 0
                                 (fun (p11_4693:bool) (p12_4694:int) (p21_4695:bool) 
                                      (p22_4696:x_1:int[(not p11_4693) || (
                                                        not p21_4695) || (
                                                        not (x1_4664 <= 0)) || 
                                                        x1_4664 < 0 || 
                                                        0 < 0 || p12_4694 <= x_1])
                                  -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                             (l1
                               (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1)
                                 (fun (x__5060:bool) (x__5061:int) (x__5062:bool) 
                                      (x__5063:x_1:int[(not x__5060) || (
                                                       not x__5062) || (
                                                       not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                                       x1_4664 - 1 < 0 || 
                                                       x2_4665 - 1 < 0 || 
                                                       x__5061 <= x_1])
                                  -> (k_insert_4666 x__5060 x__5061 x__5062 x__5063)))))))))))): X
abstract_term: (insert_1014 x_1053
                 (fun (x1_5042:int) (x2_5043:int) 
                      (k_insert_ys'ys'_5044:(x_1:bool ->
                                             x_2:int ->
                                             x_3:bool ->
                                             x_4:int[(not x_1) || (not x_3) || (
                                                     not (x1_5042 <= x2_5043)) || 
                                                     x1_5042 < 0 || x2_5043 < 0 || 
                                                     x_2 <= x_4]
                                             -> X))
                  ->
                  (ysys_1016 (x1_5042 + 1) (x2_5043 + 1)
                    (fun (x__5088:bool) (x__5089:int) (x__5090:bool) 
                         (x__5091:x_1:int[(not x__5088) || (not x__5090) || (
                                          not ((x1_5042 + 1) <= (x2_5043 + 1))) || 
                                          x1_5042 + 1 < 0 || x2_5043 + 1 < 0 || 
                                          x__5089 <= x_1])
                     -> (k_insert_ys'ys'_5044 x__5088 x__5089 x__5090 x__5091))))
                 (fun (ys''ys''_4650:(x_1:int ->
                                      x_2:int ->
                                      (x_4:bool ->
                                       x_5:int ->
                                       x_6:bool ->
                                       x_7:int[(not x_4) || (not x_6) || (
                                               not (x_1 <= x_2)) || x_1 < 0 || 
                                               x_2 < 0 || x_5 <= x_7]
                                       -> X)
                                      -> X))
                  ->
                  (k_insert_2260
                    (fun (x1_4664:int) (x2_4665:int) 
                         (k_insert_4666:(x_1:bool ->
                                         x_2:int ->
                                         x_3:bool ->
                                         x_4:int[(not x_1) || (not x_3) || (
                                                 not (x1_4664 <= x2_4665)) || 
                                                 x1_4664 < 0 || x2_4665 < 0 || 
                                                 x_2 <= x_4]
                                         -> X))
                     ->
                     (if (x1_4664 <= 0)
                       (l0
                         (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                           (l1
                             (ysys_1016 0 (x2_4665 - 1)
                               (fun (xs11_4683:bool) (xs12_4684:int) (xs21_4685:bool) 
                                    (xs22_4686:x_1:int[(not xs11_4683) || (
                                                       not xs21_4685) || (
                                                       not (0 <= (x2_4665 - 1))) || 
                                                       0 < 0 || x2_4665 - 1 < 0 || 
                                                       xs12_4684 <= x_1])
                                ->
                                (if xs21_4685
                                  (l1
                                    (if (1 = x2_4665)
                                      (l0
                                        (if (xs22_4686 = xs12_4684)
                                          (l1
                                            (if (p11_4797 <=> xs11_4683)
                                              (l1
                                                (if (xs12_4684 = p12_4798)
                                                  (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                  (l0
                                                    (loop_2167 ()
                                                      (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int) ->
                                                       (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                                              (l0
                                                (l0
                                                  (loop_2167 ()
                                                    (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
                                                     (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                                          (l0
                                            (loop_2167 ()
                                              (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                                               (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))))
                                      (l1
                                        (l1
                                          (if (p11_4797 <=> xs11_4683)
                                            (l1
                                              (if (xs12_4684 = p12_4798)
                                                (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                (l0
                                                  (loop_2167 ()
                                                    (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
                                                     (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                                            (l0
                                              (l0
                                                (loop_2167 ()
                                                  (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
                                                   (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))))))))
                                  (l0
                                    (loop_2167 ()
                                      (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                                       (k_insert_4666 x__5064 x__5065 x__5066 x__5067))))))))))
                       (l1
                         (if (x2_4665 <= 0)
                           (l0
                             (ys''ys''_4650 x1_4664 0
                               (fun (p11_4693:bool) (p12_4694:int) (p21_4695:bool) 
                                    (p22_4696:x_1:int[(not p11_4693) || (
                                                      not p21_4695) || (
                                                      not (x1_4664 <= 0)) || 
                                                      x1_4664 < 0 || 
                                                      0 < 0 || p12_4694 <= x_1])
                                -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                           (l1
                             (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1)
                               (fun (x__5060:bool) (x__5061:int) (x__5062:bool) 
                                    (x__5063:x_1:int[(not x__5060) || (
                                                     not x__5062) || (
                                                     not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                                     x1_4664 - 1 < 0 || 
                                                     x2_4665 - 1 < 0 || 
                                                     x__5061 <= x_1])
                                -> (k_insert_4666 x__5060 x__5061 x__5062 x__5063))))))))))): X
abstract_term: (fun (ys''ys''_4650:(x_1:int ->
                                    x_2:int ->
                                    (x_4:bool ->
                                     x_5:int ->
                                     x_6:bool ->
                                     x_7:int[(not x_4) || (not x_6) || (
                                             not (x_1 <= x_2)) || x_1 < 0 || 
                                             x_2 < 0 || x_5 <= x_7]
                                     -> X)
                                    -> X))
                ->
                (k_insert_2260
                  (fun (x1_4664:int) (x2_4665:int) 
                       (k_insert_4666:(x_1:bool ->
                                       x_2:int ->
                                       x_3:bool ->
                                       x_4:int[(not x_1) || (not x_3) || (
                                               not (x1_4664 <= x2_4665)) || 
                                               x1_4664 < 0 || x2_4665 < 0 || 
                                               x_2 <= x_4]
                                       -> X))
                   ->
                   (if (x1_4664 <= 0)
                     (l0
                       (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                         (l1
                           (ysys_1016 0 (x2_4665 - 1)
                             (fun (xs11_4683:bool) (xs12_4684:int) (xs21_4685:bool) 
                                  (xs22_4686:x_1:int[(not xs11_4683) || (
                                                     not xs21_4685) || (
                                                     not (0 <= (x2_4665 - 1))) || 
                                                     0 < 0 || x2_4665 - 1 < 0 || 
                                                     xs12_4684 <= x_1])
                              ->
                              (if xs21_4685
                                (l1
                                  (if (1 = x2_4665)
                                    (l0
                                      (if (xs22_4686 = xs12_4684)
                                        (l1
                                          (if (p11_4797 <=> xs11_4683)
                                            (l1
                                              (if (xs12_4684 = p12_4798)
                                                (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                                (l0
                                                  (loop_2167 ()
                                                    (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int) ->
                                                     (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                                            (l0
                                              (l0
                                                (loop_2167 ()
                                                  (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
                                                   (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                                        (l0
                                          (loop_2167 ()
                                            (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                                             (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))))
                                    (l1
                                      (l1
                                        (if (p11_4797 <=> xs11_4683)
                                          (l1
                                            (if (xs12_4684 = p12_4798)
                                              (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                              (l0
                                                (loop_2167 ()
                                                  (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
                                                   (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                                          (l0
                                            (l0
                                              (loop_2167 ()
                                                (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
                                                 (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))))))))
                                (l0
                                  (loop_2167 ()
                                    (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                                     (k_insert_4666 x__5064 x__5065 x__5066 x__5067))))))))))
                     (l1
                       (if (x2_4665 <= 0)
                         (l0
                           (ys''ys''_4650 x1_4664 0
                             (fun (p11_4693:bool) (p12_4694:int) (p21_4695:bool) 
                                  (p22_4696:x_1:int[(not p11_4693) || (
                                                    not p21_4695) || (
                                                    not (x1_4664 <= 0)) || 
                                                    x1_4664 < 0 || 0 < 0 || 
                                                    p12_4694 <= x_1])
                              -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                         (l1
                           (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1)
                             (fun (x__5060:bool) (x__5061:int) (x__5062:bool) 
                                  (x__5063:x_1:int[(not x__5060) || (
                                                   not x__5062) || (not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                                   x1_4664 - 1 < 0 || 
                                                   x2_4665 - 1 < 0 || 
                                                   x__5061 <= x_1])
                              -> (k_insert_4666 x__5060 x__5061 x__5062 x__5063)))))))))): ((
x_2:int ->
x_3:int ->
(x_5:bool ->
 x_6:int -> x_7:bool -> x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_2 < 0 || x_3 < 0 || x_6 <= x_8] -> X)
-> X) ->
X)
abst_arg: ys''ys''_4650, (x_1:int ->
                          x_2:int ->
                          (x_4:bool ->
                           x_5:int ->
                           x_6:bool ->
                           x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] ->
                           X)
                          -> X)
abst_arg: ys''ys''_4650, (x_1:int ->
                          x_2:int ->
                          (x_4:bool ->
                           x_5:int ->
                           x_6:bool ->
                           x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] ->
                           X)
                          -> X)
abstract_term: (k_insert_2260
                 (fun (x1_4664:int) (x2_4665:int) 
                      (k_insert_4666:(x_1:bool ->
                                      x_2:int ->
                                      x_3:bool ->
                                      x_4:int[(not x_1) || (not x_3) || (
                                              not (x1_4664 <= x2_4665)) || 
                                              x1_4664 < 0 || x2_4665 < 0 || 
                                              x_2 <= x_4]
                                      -> X))
                  ->
                  (if (x1_4664 <= 0)
                    (l0
                      (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                        (l1
                          (ysys_1016 0 (x2_4665 - 1)
                            (fun (xs11_4683:bool) (xs12_4684:int) (xs21_4685:bool) 
                                 (xs22_4686:x_1:int[(not xs11_4683) || (
                                                    not xs21_4685) || (
                                                    not (0 <= (x2_4665 - 1))) || 
                                                    0 < 0 || x2_4665 - 1 < 0 || 
                                                    xs12_4684 <= x_1])
                             ->
                             (if xs21_4685
                               (l1
                                 (if (1 = x2_4665)
                                   (l0
                                     (if (xs22_4686 = xs12_4684)
                                       (l1
                                         (if (p11_4797 <=> xs11_4683)
                                           (l1
                                             (if (xs12_4684 = p12_4798)
                                               (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                               (l0
                                                 (loop_2167 ()
                                                   (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int) ->
                                                    (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                                           (l0
                                             (l0
                                               (loop_2167 ()
                                                 (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
                                                  (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                                       (l0
                                         (loop_2167 ()
                                           (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                                            (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))))
                                   (l1
                                     (l1
                                       (if (p11_4797 <=> xs11_4683)
                                         (l1
                                           (if (xs12_4684 = p12_4798) (
                                             l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                             (l0
                                               (loop_2167 ()
                                                 (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
                                                  (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                                         (l0
                                           (l0
                                             (loop_2167 ()
                                               (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
                                                (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))))))))
                               (l0
                                 (loop_2167 ()
                                   (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                                    (k_insert_4666 x__5064 x__5065 x__5066 x__5067))))))))))
                    (l1
                      (if (x2_4665 <= 0)
                        (l0
                          (ys''ys''_4650 x1_4664 0
                            (fun (p11_4693:bool) (p12_4694:int) (p21_4695:bool) 
                                 (p22_4696:x_1:int[(not p11_4693) || (
                                                   not p21_4695) || (
                                                   not (x1_4664 <= 0)) || 
                                                   x1_4664 < 0 || 0 < 0 || 
                                                   p12_4694 <= x_1])
                             -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                        (l1
                          (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1)
                            (fun (x__5060:bool) (x__5061:int) (x__5062:bool) 
                                 (x__5063:x_1:int[(not x__5060) || (not x__5062) || 
                                                  (not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                                  x1_4664 - 1 < 0 || 
                                                  x2_4665 - 1 < 0 || 
                                                  x__5061 <= x_1])
                             -> (k_insert_4666 x__5060 x__5061 x__5062 x__5063))))))))): X
abstract_term: (fun (x1_4664:int) (x2_4665:int) 
                    (k_insert_4666:(x_1:bool ->
                                    x_2:int ->
                                    x_3:bool ->
                                    x_4:int[(not x_1) || (not x_3) || (
                                            not (x1_4664 <= x2_4665)) || 
                                            x1_4664 < 0 || x2_4665 < 0 || 
                                            x_2 <= x_4]
                                    -> X))
                ->
                (if (x1_4664 <= 0)
                  (l0
                    (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                      (l1
                        (ysys_1016 0 (x2_4665 - 1)
                          (fun (xs11_4683:bool) (xs12_4684:int) (xs21_4685:bool) 
                               (xs22_4686:x_1:int[(not xs11_4683) || (
                                                  not xs21_4685) || (
                                                  not (0 <= (x2_4665 - 1))) || 
                                                  0 < 0 || x2_4665 - 1 < 0 || 
                                                  xs12_4684 <= x_1])
                           ->
                           (if xs21_4685
                             (l1
                               (if (1 = x2_4665)
                                 (l0
                                   (if (xs22_4686 = xs12_4684)
                                     (l1
                                       (if (p11_4797 <=> xs11_4683)
                                         (l1
                                           (if (xs12_4684 = p12_4798) (
                                             l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                             (l0
                                               (loop_2167 ()
                                                 (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int) ->
                                                  (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                                         (l0
                                           (l0
                                             (loop_2167 ()
                                               (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
                                                (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                                     (l0
                                       (loop_2167 ()
                                         (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                                          (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))))
                                 (l1
                                   (l1
                                     (if (p11_4797 <=> xs11_4683)
                                       (l1
                                         (if (xs12_4684 = p12_4798) (
                                           l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                           (l0
                                             (loop_2167 ()
                                               (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
                                                (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                                       (l0
                                         (l0
                                           (loop_2167 ()
                                             (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
                                              (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))))))))
                             (l0
                               (loop_2167 ()
                                 (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                                  (k_insert_4666 x__5064 x__5065 x__5066 x__5067))))))))))
                  (l1
                    (if (x2_4665 <= 0)
                      (l0
                        (ys''ys''_4650 x1_4664 0
                          (fun (p11_4693:bool) (p12_4694:int) (p21_4695:bool) 
                               (p22_4696:x_1:int[(not p11_4693) || (not p21_4695) || (
                                                 not (x1_4664 <= 0)) || 
                                                 x1_4664 < 0 || 0 < 0 || 
                                                 p12_4694 <= x_1])
                           -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                      (l1
                        (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1)
                          (fun (x__5060:bool) (x__5061:int) (x__5062:bool) 
                               (x__5063:x_1:int[(not x__5060) || (not x__5062) || 
                                                (not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                                x1_4664 - 1 < 0 || x2_4665 - 1 < 0 || 
                                                x__5061 <= x_1])
                           -> (k_insert_4666 x__5060 x__5061 x__5062 x__5063)))))))): (
x_1:int ->
x_2:int ->
(x_4:bool ->
 x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
-> X)
abst_arg: x1_4664, int
abst_arg: x2_4665, int
abst_arg: k_insert_4666, (x_1:bool ->
                          x_2:int ->
                          x_3:bool ->
                          x_4:int[(not x_1) || (not x_3) || (not (x1_4664 <= x2_4665)) || 
                                  x1_4664 < 0 || x2_4665 < 0 || x_2 <= x_4]
                          -> X)
abst_arg: x1_4664, int
abst_arg: x2_4665, int
abst_arg: k_insert_4666, (x_1:bool ->
                          x_2:int ->
                          x_3:bool ->
                          x_4:int[(not x_1) || (not x_3) || (not (x1_4664 <= x2_4665)) || 
                                  x1_4664 < 0 || x2_4665 < 0 || x_2 <= x_4]
                          -> X)
abstract_term: (if (x1_4664 <= 0)
                 (l0
                   (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                     (l1
                       (ysys_1016 0 (x2_4665 - 1)
                         (fun (xs11_4683:bool) (xs12_4684:int) (xs21_4685:bool) 
                              (xs22_4686:x_1:int[(not xs11_4683) || (
                                                 not xs21_4685) || (not (0 <= (x2_4665 - 1))) || 
                                                 0 < 0 || x2_4665 - 1 < 0 || 
                                                 xs12_4684 <= x_1])
                          ->
                          (if xs21_4685
                            (l1
                              (if (1 = x2_4665)
                                (l0
                                  (if (xs22_4686 = xs12_4684)
                                    (l1
                                      (if (p11_4797 <=> xs11_4683)
                                        (l1
                                          (if (xs12_4684 = p12_4798) (
                                            l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                            (l0
                                              (loop_2167 ()
                                                (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int) ->
                                                 (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                                        (l0
                                          (l0
                                            (loop_2167 ()
                                              (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
                                               (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                                    (l0
                                      (loop_2167 ()
                                        (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                                         (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))))
                                (l1
                                  (l1
                                    (if (p11_4797 <=> xs11_4683)
                                      (l1
                                        (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                          (l0
                                            (loop_2167 ()
                                              (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
                                               (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                                      (l0
                                        (l0
                                          (loop_2167 ()
                                            (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
                                             (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))))))))
                            (l0
                              (loop_2167 ()
                                (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                                 (k_insert_4666 x__5064 x__5065 x__5066 x__5067))))))))))
                 (l1
                   (if (x2_4665 <= 0)
                     (l0
                       (ys''ys''_4650 x1_4664 0
                         (fun (p11_4693:bool) (p12_4694:int) (p21_4695:bool) 
                              (p22_4696:x_1:int[(not p11_4693) || (not p21_4695) || (
                                                not (x1_4664 <= 0)) || 
                                                x1_4664 < 0 || 0 < 0 || 
                                                p12_4694 <= x_1])
                          -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                     (l1
                       (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1)
                         (fun (x__5060:bool) (x__5061:int) (x__5062:bool) 
                              (x__5063:x_1:int[(not x__5060) || (not x__5062) || 
                                               (not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                               x1_4664 - 1 < 0 || x2_4665 - 1 < 0 || 
                                               x__5061 <= x_1])
                          -> (k_insert_4666 x__5060 x__5061 x__5062 x__5063))))))): X
abstract_term: (x1_4664 <= 0): x_1:bool[x_1]
cond: (not (x_1053 < p12_4798)); p11_4797; true
pbs: p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:(x1_4664 <= 0)
tt:false
ff:false

abstract_term: (l0
                 (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                   (l1
                     (ysys_1016 0 (x2_4665 - 1)
                       (fun (xs11_4683:bool) (xs12_4684:int) (xs21_4685:bool) 
                            (xs22_4686:x_1:int[(not xs11_4683) || (not xs21_4685) || (
                                               not (0 <= (x2_4665 - 1))) || 
                                               0 < 0 || x2_4665 - 1 < 0 || 
                                               xs12_4684 <= x_1])
                        ->
                        (if xs21_4685
                          (l1
                            (if (1 = x2_4665)
                              (l0
                                (if (xs22_4686 = xs12_4684)
                                  (l1
                                    (if (p11_4797 <=> xs11_4683)
                                      (l1
                                        (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                          (l0
                                            (loop_2167 ()
                                              (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int) ->
                                               (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                                      (l0
                                        (l0
                                          (loop_2167 ()
                                            (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
                                             (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                                  (l0
                                    (loop_2167 ()
                                      (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                                       (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))))
                              (l1
                                (l1
                                  (if (p11_4797 <=> xs11_4683)
                                    (l1
                                      (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                        (l0
                                          (loop_2167 ()
                                            (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
                                             (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                                    (l0
                                      (l0
                                        (loop_2167 ()
                                          (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
                                           (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))))))))
                          (l0
                            (loop_2167 ()
                              (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                               (k_insert_4666 x__5064 x__5065 x__5066 x__5067)))))))))): X
abstract_term: (if (x2_4665 <= 0) (l0 (k_insert_4666 true p12_4798 true p12_4798))
                 (l1
                   (ysys_1016 0 (x2_4665 - 1)
                     (fun (xs11_4683:bool) (xs12_4684:int) (xs21_4685:bool) 
                          (xs22_4686:x_1:int[(not xs11_4683) || (not xs21_4685) || (
                                             not (0 <= (x2_4665 - 1))) || 
                                             0 < 0 || x2_4665 - 1 < 0 || 
                                             xs12_4684 <= x_1])
                      ->
                      (if xs21_4685
                        (l1
                          (if (1 = x2_4665)
                            (l0
                              (if (xs22_4686 = xs12_4684)
                                (l1
                                  (if (p11_4797 <=> xs11_4683)
                                    (l1
                                      (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                        (l0
                                          (loop_2167 ()
                                            (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int) ->
                                             (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                                    (l0
                                      (l0
                                        (loop_2167 ()
                                          (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
                                           (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                                (l0
                                  (loop_2167 ()
                                    (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                                     (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))))
                            (l1
                              (l1
                                (if (p11_4797 <=> xs11_4683)
                                  (l1
                                    (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                      (l0
                                        (loop_2167 ()
                                          (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
                                           (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                                  (l0
                                    (l0
                                      (loop_2167 ()
                                        (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
                                         (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))))))))
                        (l0
                          (loop_2167 ()
                            (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                             (k_insert_4666 x__5064 x__5065 x__5066 x__5067))))))))): X
abstract_term: (x2_4665 <= 0): x_1:bool[x_1]
cond: (x1_4664 <= 0); (not (x_1053 < p12_4798)); p11_4797; true
pbs: p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:(x2_4665 <= 0)
tt:false
ff:false

abstract_term: (l0 (k_insert_4666 true p12_4798 true p12_4798)): X
abstract_term: (k_insert_4666 true p12_4798 true p12_4798): X
abstract_term: p12_4798: x_1:int[(not true) || (not true) || (not (x1_4664 <= x2_4665)) || 
                                 x1_4664 < 0 || x2_4665 < 0 || p12_4798 <= x_1]
cond: (x2_4665 <= 0); (x1_4664 <= 0); (not (x_1053 < p12_4798)); p11_4797; true
pbs: p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not true) || (not true)) || (not (x1_4664 <= x2_4665))) || (x1_4664 < 0)) || (x2_4665 < 0)) ||
   (p12_4798 <= p12_4798))
tt:true
ff:false

abstract_term: true: bool
abstract_term: p12_4798: int
abstract_term: true: bool
abstract_term: (l1
                 (ysys_1016 0 (x2_4665 - 1)
                   (fun (xs11_4683:bool) (xs12_4684:int) (xs21_4685:bool) 
                        (xs22_4686:x_1:int[(not xs11_4683) || (not xs21_4685) || (
                                           not (0 <= (x2_4665 - 1))) || 
                                           0 < 0 || x2_4665 - 1 < 0 || 
                                           xs12_4684 <= x_1])
                    ->
                    (if xs21_4685
                      (l1
                        (if (1 = x2_4665)
                          (l0
                            (if (xs22_4686 = xs12_4684)
                              (l1
                                (if (p11_4797 <=> xs11_4683)
                                  (l1
                                    (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                      (l0
                                        (loop_2167 ()
                                          (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int) ->
                                           (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                                  (l0
                                    (l0
                                      (loop_2167 ()
                                        (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
                                         (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                              (l0
                                (loop_2167 ()
                                  (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                                   (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))))
                          (l1
                            (l1
                              (if (p11_4797 <=> xs11_4683)
                                (l1
                                  (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                    (l0
                                      (loop_2167 ()
                                        (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
                                         (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                                (l0
                                  (l0
                                    (loop_2167 ()
                                      (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
                                       (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))))))))
                      (l0
                        (loop_2167 ()
                          (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                           (k_insert_4666 x__5064 x__5065 x__5066 x__5067)))))))): X
abstract_term: (ysys_1016 0 (x2_4665 - 1)
                 (fun (xs11_4683:bool) (xs12_4684:int) (xs21_4685:bool) 
                      (xs22_4686:x_1:int[(not xs11_4683) || (not xs21_4685) || (
                                         not (0 <= (x2_4665 - 1))) || 
                                         0 < 0 || x2_4665 - 1 < 0 || 
                                         xs12_4684 <= x_1])
                  ->
                  (if xs21_4685
                    (l1
                      (if (1 = x2_4665)
                        (l0
                          (if (xs22_4686 = xs12_4684)
                            (l1
                              (if (p11_4797 <=> xs11_4683)
                                (l1
                                  (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                    (l0
                                      (loop_2167 ()
                                        (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int) ->
                                         (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                                (l0
                                  (l0
                                    (loop_2167 ()
                                      (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
                                       (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                            (l0
                              (loop_2167 ()
                                (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                                 (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))))
                        (l1
                          (l1
                            (if (p11_4797 <=> xs11_4683)
                              (l1
                                (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                  (l0
                                    (loop_2167 ()
                                      (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
                                       (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                              (l0
                                (l0
                                  (loop_2167 ()
                                    (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
                                     (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))))))))
                    (l0
                      (loop_2167 ()
                        (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                         (k_insert_4666 x__5064 x__5065 x__5066 x__5067))))))): X
abstract_term: (fun (xs11_4683:bool) (xs12_4684:int) (xs21_4685:bool) 
                    (xs22_4686:x_1:int[(not xs11_4683) || (not xs21_4685) || (
                                       not (0 <= (x2_4665 - 1))) || 0 < 0 || 
                                       x2_4665 - 1 < 0 || xs12_4684 <= x_1])
                ->
                (if xs21_4685
                  (l1
                    (if (1 = x2_4665)
                      (l0
                        (if (xs22_4686 = xs12_4684)
                          (l1
                            (if (p11_4797 <=> xs11_4683)
                              (l1
                                (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                  (l0
                                    (loop_2167 ()
                                      (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int) ->
                                       (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                              (l0
                                (l0
                                  (loop_2167 ()
                                    (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
                                     (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                          (l0
                            (loop_2167 ()
                              (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                               (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))))
                      (l1
                        (l1
                          (if (p11_4797 <=> xs11_4683)
                            (l1
                              (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                (l0
                                  (loop_2167 ()
                                    (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
                                     (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                            (l0
                              (l0
                                (loop_2167 ()
                                  (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
                                   (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))))))))
                  (l0
                    (loop_2167 ()
                      (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                       (k_insert_4666 x__5064 x__5065 x__5066 x__5067)))))): (
x_1:bool ->
x_2:int ->
x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (0 <= (x2_4665 - 1))) || 0 < 0 || x2_4665 - 1 < 0 || x_2 <= x_4] ->
X)
abst_arg: xs11_4683, bool
abst_arg: xs12_4684, int
abst_arg: xs21_4685, bool
abst_arg: xs22_4686, x_1:int[(not xs11_4683) || (not xs21_4685) || (not (0 <= (x2_4665 - 1))) || 
                             0 < 0 || x2_4665 - 1 < 0 || xs12_4684 <= x_1]
abst_arg: xs11_4683, bool
abst_arg: xs12_4684, int
abst_arg: xs21_4685, bool
abst_arg: xs22_4686, x_1:int[(not xs11_4683) || (not xs21_4685) || (not (0 <= (x2_4665 - 1))) || 
                             0 < 0 || x2_4665 - 1 < 0 || xs12_4684 <= x_1]
abstract_term: (if xs21_4685
                 (l1
                   (if (1 = x2_4665)
                     (l0
                       (if (xs22_4686 = xs12_4684)
                         (l1
                           (if (p11_4797 <=> xs11_4683)
                             (l1
                               (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                                 (l0
                                   (loop_2167 ()
                                     (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int) ->
                                      (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                             (l0
                               (l0
                                 (loop_2167 ()
                                   (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
                                    (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                         (l0
                           (loop_2167 ()
                             (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                              (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))))
                     (l1
                       (l1
                         (if (p11_4797 <=> xs11_4683)
                           (l1
                             (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                               (l0
                                 (loop_2167 ()
                                   (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
                                    (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                           (l0
                             (l0
                               (loop_2167 ()
                                 (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
                                  (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))))))))
                 (l0
                   (loop_2167 ()
                     (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                      (k_insert_4666 x__5064 x__5065 x__5066 x__5067))))): X
abstract_term: xs21_4685: x_1:bool[x_1]
cond: (not (x2_4665 <= 0)); (x1_4664 <= 0); (not (x_1053 < p12_4798)); p11_4797; true
pbs: xs22_4686 := ((((((not xs11_4683) || (not xs21_4685)) || (not (0 <= (x2_4665 - 1)))) || (0 < 0)) ||
                    ((x2_4665 - 1) < 0))
                   || (xs12_4684 <= xs22_4686));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:xs21_4685
tt:false
ff:false

abstract_term: (l1
                 (if (1 = x2_4665)
                   (l0
                     (if (xs22_4686 = xs12_4684)
                       (l1
                         (if (p11_4797 <=> xs11_4683)
                           (l1
                             (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                               (l0
                                 (loop_2167 ()
                                   (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int) ->
                                    (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                           (l0
                             (l0
                               (loop_2167 ()
                                 (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
                                  (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                       (l0
                         (loop_2167 ()
                           (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                            (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))))
                   (l1
                     (l1
                       (if (p11_4797 <=> xs11_4683)
                         (l1
                           (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                             (l0
                               (loop_2167 ()
                                 (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
                                  (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                         (l0
                           (l0
                             (loop_2167 ()
                               (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
                                (k_insert_4666 x__5068 x__5069 x__5070 x__5071)))))))))): X
abstract_term: (if (1 = x2_4665)
                 (l0
                   (if (xs22_4686 = xs12_4684)
                     (l1
                       (if (p11_4797 <=> xs11_4683)
                         (l1
                           (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                             (l0
                               (loop_2167 ()
                                 (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int) ->
                                  (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                         (l0
                           (l0
                             (loop_2167 ()
                               (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
                                (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                     (l0
                       (loop_2167 ()
                         (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                          (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))))
                 (l1
                   (l1
                     (if (p11_4797 <=> xs11_4683)
                       (l1
                         (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                           (l0
                             (loop_2167 ()
                               (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
                                (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                       (l0
                         (l0
                           (loop_2167 ()
                             (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
                              (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))))))): X
abstract_term: (1 = x2_4665): x_1:bool[x_1]
cond: xs21_4685; (not (x2_4665 <= 0)); (x1_4664 <= 0); (not (x_1053 < p12_4798)); p11_4797; true
pbs: xs22_4686 := ((((((not xs11_4683) || (not xs21_4685)) || (not (0 <= (x2_4665 - 1)))) || (0 < 0)) ||
                    ((x2_4665 - 1) < 0))
                   || (xs12_4684 <= xs22_4686));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:(1 = x2_4665)
tt:false
ff:false

abstract_term: (l0
                 (if (xs22_4686 = xs12_4684)
                   (l1
                     (if (p11_4797 <=> xs11_4683)
                       (l1
                         (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                           (l0
                             (loop_2167 ()
                               (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int) ->
                                (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                       (l0
                         (l0
                           (loop_2167 ()
                             (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
                              (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                   (l0
                     (loop_2167 ()
                       (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                        (k_insert_4666 x__5076 x__5077 x__5078 x__5079)))))): X
abstract_term: (if (xs22_4686 = xs12_4684)
                 (l1
                   (if (p11_4797 <=> xs11_4683)
                     (l1
                       (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                         (l0
                           (loop_2167 ()
                             (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int) ->
                              (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                     (l0
                       (l0
                         (loop_2167 ()
                           (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
                            (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))))
                 (l0
                   (loop_2167 ()
                     (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                      (k_insert_4666 x__5076 x__5077 x__5078 x__5079))))): X
abstract_term: (xs22_4686 = xs12_4684): x_1:bool[x_1]
cond: (1 = x2_4665); xs21_4685; (not (x2_4665 <= 0)); (x1_4664 <= 0); (not (x_1053 < p12_4798)); p11_4797; true
pbs: xs22_4686 := ((((((not xs11_4683) || (not xs21_4685)) || (not (0 <= (x2_4665 - 1)))) || (0 < 0)) ||
                    ((x2_4665 - 1) < 0))
                   || (xs12_4684 <= xs22_4686));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:(xs22_4686 = xs12_4684)
tt:false
ff:false

abstract_term: (l1
                 (if (p11_4797 <=> xs11_4683)
                   (l1
                     (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                       (l0
                         (loop_2167 ()
                           (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int) ->
                            (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                   (l0
                     (l0
                       (loop_2167 ()
                         (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
                          (k_insert_4666 x__5080 x__5081 x__5082 x__5083))))))): X
abstract_term: (if (p11_4797 <=> xs11_4683)
                 (l1
                   (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                     (l0
                       (loop_2167 ()
                         (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int) ->
                          (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))))
                 (l0
                   (l0
                     (loop_2167 ()
                       (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
                        (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))))): X
abstract_term: (p11_4797 <=> xs11_4683): x_1:bool[x_1]
cond: (xs22_4686 = xs12_4684); (1 = x2_4665); xs21_4685; (not (x2_4665 <= 0)); (
      x1_4664 <= 0); (not (x_1053 < p12_4798)); p11_4797; true
pbs: xs22_4686 := ((((((not xs11_4683) || (not xs21_4685)) || (not (0 <= (x2_4665 - 1)))) || (0 < 0)) ||
                    ((x2_4665 - 1) < 0))
                   || (xs12_4684 <= xs22_4686));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:(p11_4797 <=> xs11_4683)
tt:false
ff:false

abstract_term: (l1
                 (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                   (l0
                     (loop_2167 ()
                       (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int) ->
                        (k_insert_4666 x__5084 x__5085 x__5086 x__5087)))))): X
abstract_term: (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                 (l0
                   (loop_2167 ()
                     (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int) ->
                      (k_insert_4666 x__5084 x__5085 x__5086 x__5087))))): X
abstract_term: (xs12_4684 = p12_4798): x_1:bool[x_1]
cond: (p11_4797 <=> xs11_4683); (xs22_4686 = xs12_4684); (1 = x2_4665); xs21_4685; (
      not (x2_4665 <= 0)); (x1_4664 <= 0); (not (x_1053 < p12_4798)); p11_4797; true
pbs: xs22_4686 := ((((((not xs11_4683) || (not xs21_4685)) || (not (0 <= (x2_4665 - 1)))) || (0 < 0)) ||
                    ((x2_4665 - 1) < 0))
                   || (xs12_4684 <= xs22_4686));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:(xs12_4684 = p12_4798)
tt:false
ff:false

abstract_term: (l1 (k_insert_4666 true p12_4798 true xs22_4686)): X
abstract_term: (k_insert_4666 true p12_4798 true xs22_4686): X
abstract_term: xs22_4686: x_1:int[(not true) || (not true) || (not (x1_4664 <= x2_4665)) || 
                                  x1_4664 < 0 || x2_4665 < 0 || p12_4798 <= x_1]
cond: (xs12_4684 = p12_4798); (p11_4797 <=> xs11_4683); (xs22_4686 = xs12_4684); (
      1 = x2_4665); xs21_4685; (not (x2_4665 <= 0)); (x1_4664 <= 0); (
      not (x_1053 < p12_4798)); p11_4797; true
pbs: xs22_4686 := ((((((not xs11_4683) || (not xs21_4685)) || (not (0 <= (x2_4665 - 1)))) || (0 < 0)) ||
                    ((x2_4665 - 1) < 0))
                   || (xs12_4684 <= xs22_4686));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not true) || (not true)) || (not (x1_4664 <= x2_4665))) || (x1_4664 < 0)) || (x2_4665 < 0)) ||
   (p12_4798 <= xs22_4686))
tt:true
ff:false

abstract_term: true: bool
abstract_term: p12_4798: int
abstract_term: true: bool
abstract_term: (l0
                 (loop_2167 ()
                   (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int) ->
                    (k_insert_4666 x__5084 x__5085 x__5086 x__5087)))): X
abstract_term: (loop_2167 ()
                 (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int) ->
                  (k_insert_4666 x__5084 x__5085 x__5086 x__5087))): X
abstract_term: (fun (x__5084:bool) (x__5085:int) (x__5086:bool) (x__5087:int) ->
                (k_insert_4666 x__5084 x__5085 x__5086 x__5087)): (bool -> int -> bool -> int -> X)
abst_arg: x__5084, bool
abst_arg: x__5085, int
abst_arg: x__5086, bool
abst_arg: x__5087, int
abst_arg: x__5084, bool
abst_arg: x__5085, int
abst_arg: x__5086, bool
abst_arg: x__5087, int
abstract_term: (k_insert_4666 x__5084 x__5085 x__5086 x__5087): X
abstract_term: x__5087: x_1:int[(not x__5084) || (not x__5086) || (not (x1_4664 <= x2_4665)) || 
                                x1_4664 < 0 || x2_4665 < 0 || x__5085 <= x_1]
cond: (not (xs12_4684 = p12_4798)); (p11_4797 <=> xs11_4683); (xs22_4686 = xs12_4684); (
      1 = x2_4665); xs21_4685; (not (x2_4665 <= 0)); (x1_4664 <= 0); (
      not (x_1053 < p12_4798)); p11_4797; true
pbs: xs22_4686 := ((((((not xs11_4683) || (not xs21_4685)) || (not (0 <= (x2_4665 - 1)))) || (0 < 0)) ||
                    ((x2_4665 - 1) < 0))
                   || (xs12_4684 <= xs22_4686));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not x__5084) || (not x__5086)) || (not (x1_4664 <= x2_4665))) || (x1_4664 < 0)) || (x2_4665 < 0)) ||
   (x__5085 <= x__5087))
tt:false
ff:false

abstract_term: x__5086: bool
abstract_term: x__5085: int
abstract_term: x__5084: bool
abstract_term: (): unit
abstract_term: (l0
                 (l0
                   (loop_2167 ()
                     (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
                      (k_insert_4666 x__5080 x__5081 x__5082 x__5083))))): X
abstract_term: (l0
                 (loop_2167 ()
                   (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
                    (k_insert_4666 x__5080 x__5081 x__5082 x__5083)))): X
abstract_term: (loop_2167 ()
                 (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
                  (k_insert_4666 x__5080 x__5081 x__5082 x__5083))): X
abstract_term: (fun (x__5080:bool) (x__5081:int) (x__5082:bool) (x__5083:int) ->
                (k_insert_4666 x__5080 x__5081 x__5082 x__5083)): (bool -> int -> bool -> int -> X)
abst_arg: x__5080, bool
abst_arg: x__5081, int
abst_arg: x__5082, bool
abst_arg: x__5083, int
abst_arg: x__5080, bool
abst_arg: x__5081, int
abst_arg: x__5082, bool
abst_arg: x__5083, int
abstract_term: (k_insert_4666 x__5080 x__5081 x__5082 x__5083): X
abstract_term: x__5083: x_1:int[(not x__5080) || (not x__5082) || (not (x1_4664 <= x2_4665)) || 
                                x1_4664 < 0 || x2_4665 < 0 || x__5081 <= x_1]
cond: (not (p11_4797 <=> xs11_4683)); (xs22_4686 = xs12_4684); (1 = x2_4665); xs21_4685; (
      not (x2_4665 <= 0)); (x1_4664 <= 0); (not (x_1053 < p12_4798)); p11_4797; true
pbs: xs22_4686 := ((((((not xs11_4683) || (not xs21_4685)) || (not (0 <= (x2_4665 - 1)))) || (0 < 0)) ||
                    ((x2_4665 - 1) < 0))
                   || (xs12_4684 <= xs22_4686));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not x__5080) || (not x__5082)) || (not (x1_4664 <= x2_4665))) || (x1_4664 < 0)) || (x2_4665 < 0)) ||
   (x__5081 <= x__5083))
tt:false
ff:false

abstract_term: x__5082: bool
abstract_term: x__5081: int
abstract_term: x__5080: bool
abstract_term: (): unit
abstract_term: (l0
                 (loop_2167 ()
                   (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                    (k_insert_4666 x__5076 x__5077 x__5078 x__5079)))): X
abstract_term: (loop_2167 ()
                 (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                  (k_insert_4666 x__5076 x__5077 x__5078 x__5079))): X
abstract_term: (fun (x__5076:bool) (x__5077:int) (x__5078:bool) (x__5079:int) ->
                (k_insert_4666 x__5076 x__5077 x__5078 x__5079)): (bool -> int -> bool -> int -> X)
abst_arg: x__5076, bool
abst_arg: x__5077, int
abst_arg: x__5078, bool
abst_arg: x__5079, int
abst_arg: x__5076, bool
abst_arg: x__5077, int
abst_arg: x__5078, bool
abst_arg: x__5079, int
abstract_term: (k_insert_4666 x__5076 x__5077 x__5078 x__5079): X
abstract_term: x__5079: x_1:int[(not x__5076) || (not x__5078) || (not (x1_4664 <= x2_4665)) || 
                                x1_4664 < 0 || x2_4665 < 0 || x__5077 <= x_1]
cond: (not (xs22_4686 = xs12_4684)); (1 = x2_4665); xs21_4685; (not (x2_4665 <= 0)); (
      x1_4664 <= 0); (not (x_1053 < p12_4798)); p11_4797; true
pbs: xs22_4686 := ((((((not xs11_4683) || (not xs21_4685)) || (not (0 <= (x2_4665 - 1)))) || (0 < 0)) ||
                    ((x2_4665 - 1) < 0))
                   || (xs12_4684 <= xs22_4686));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not x__5076) || (not x__5078)) || (not (x1_4664 <= x2_4665))) || (x1_4664 < 0)) || (x2_4665 < 0)) ||
   (x__5077 <= x__5079))
tt:false
ff:false

abstract_term: x__5078: bool
abstract_term: x__5077: int
abstract_term: x__5076: bool
abstract_term: (): unit
abstract_term: (l1
                 (l1
                   (if (p11_4797 <=> xs11_4683)
                     (l1
                       (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                         (l0
                           (loop_2167 ()
                             (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
                              (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                     (l0
                       (l0
                         (loop_2167 ()
                           (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
                            (k_insert_4666 x__5068 x__5069 x__5070 x__5071)))))))): X
abstract_term: (l1
                 (if (p11_4797 <=> xs11_4683)
                   (l1
                     (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                       (l0
                         (loop_2167 ()
                           (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
                            (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                   (l0
                     (l0
                       (loop_2167 ()
                         (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
                          (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))))): X
abstract_term: (if (p11_4797 <=> xs11_4683)
                 (l1
                   (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                     (l0
                       (loop_2167 ()
                         (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
                          (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))))
                 (l0
                   (l0
                     (loop_2167 ()
                       (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
                        (k_insert_4666 x__5068 x__5069 x__5070 x__5071)))))): X
abstract_term: (p11_4797 <=> xs11_4683): x_1:bool[x_1]
cond: (not (1 = x2_4665)); xs21_4685; (not (x2_4665 <= 0)); (x1_4664 <= 0); (not (x_1053 < p12_4798)); p11_4797; true
pbs: xs22_4686 := ((((((not xs11_4683) || (not xs21_4685)) || (not (0 <= (x2_4665 - 1)))) || (0 < 0)) ||
                    ((x2_4665 - 1) < 0))
                   || (xs12_4684 <= xs22_4686));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:(p11_4797 <=> xs11_4683)
tt:false
ff:false

abstract_term: (l1
                 (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                   (l0
                     (loop_2167 ()
                       (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
                        (k_insert_4666 x__5072 x__5073 x__5074 x__5075)))))): X
abstract_term: (if (xs12_4684 = p12_4798) (l1 (k_insert_4666 true p12_4798 true xs22_4686))
                 (l0
                   (loop_2167 ()
                     (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
                      (k_insert_4666 x__5072 x__5073 x__5074 x__5075))))): X
abstract_term: (xs12_4684 = p12_4798): x_1:bool[x_1]
cond: (p11_4797 <=> xs11_4683); (not (1 = x2_4665)); xs21_4685; (not (x2_4665 <= 0)); (
      x1_4664 <= 0); (not (x_1053 < p12_4798)); p11_4797; true
pbs: xs22_4686 := ((((((not xs11_4683) || (not xs21_4685)) || (not (0 <= (x2_4665 - 1)))) || (0 < 0)) ||
                    ((x2_4665 - 1) < 0))
                   || (xs12_4684 <= xs22_4686));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:(xs12_4684 = p12_4798)
tt:false
ff:false

abstract_term: (l1 (k_insert_4666 true p12_4798 true xs22_4686)): X
abstract_term: (k_insert_4666 true p12_4798 true xs22_4686): X
abstract_term: xs22_4686: x_1:int[(not true) || (not true) || (not (x1_4664 <= x2_4665)) || 
                                  x1_4664 < 0 || x2_4665 < 0 || p12_4798 <= x_1]
cond: (xs12_4684 = p12_4798); (p11_4797 <=> xs11_4683); (not (1 = x2_4665)); xs21_4685; (
      not (x2_4665 <= 0)); (x1_4664 <= 0); (not (x_1053 < p12_4798)); p11_4797; true
pbs: xs22_4686 := ((((((not xs11_4683) || (not xs21_4685)) || (not (0 <= (x2_4665 - 1)))) || (0 < 0)) ||
                    ((x2_4665 - 1) < 0))
                   || (xs12_4684 <= xs22_4686));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not true) || (not true)) || (not (x1_4664 <= x2_4665))) || (x1_4664 < 0)) || (x2_4665 < 0)) ||
   (p12_4798 <= xs22_4686))
tt:xs22_4686
ff:false

abstract_term: true: bool
abstract_term: p12_4798: int
abstract_term: true: bool
abstract_term: (l0
                 (loop_2167 ()
                   (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
                    (k_insert_4666 x__5072 x__5073 x__5074 x__5075)))): X
abstract_term: (loop_2167 ()
                 (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
                  (k_insert_4666 x__5072 x__5073 x__5074 x__5075))): X
abstract_term: (fun (x__5072:bool) (x__5073:int) (x__5074:bool) (x__5075:int) ->
                (k_insert_4666 x__5072 x__5073 x__5074 x__5075)): (bool -> int -> bool -> int -> X)
abst_arg: x__5072, bool
abst_arg: x__5073, int
abst_arg: x__5074, bool
abst_arg: x__5075, int
abst_arg: x__5072, bool
abst_arg: x__5073, int
abst_arg: x__5074, bool
abst_arg: x__5075, int
abstract_term: (k_insert_4666 x__5072 x__5073 x__5074 x__5075): X
abstract_term: x__5075: x_1:int[(not x__5072) || (not x__5074) || (not (x1_4664 <= x2_4665)) || 
                                x1_4664 < 0 || x2_4665 < 0 || x__5073 <= x_1]
cond: (not (xs12_4684 = p12_4798)); (p11_4797 <=> xs11_4683); (not (1 = x2_4665)); xs21_4685; (
      not (x2_4665 <= 0)); (x1_4664 <= 0); (not (x_1053 < p12_4798)); p11_4797; true
pbs: xs22_4686 := ((((((not xs11_4683) || (not xs21_4685)) || (not (0 <= (x2_4665 - 1)))) || (0 < 0)) ||
                    ((x2_4665 - 1) < 0))
                   || (xs12_4684 <= xs22_4686));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not x__5072) || (not x__5074)) || (not (x1_4664 <= x2_4665))) || (x1_4664 < 0)) || (x2_4665 < 0)) ||
   (x__5073 <= x__5075))
tt:false
ff:false

abstract_term: x__5074: bool
abstract_term: x__5073: int
abstract_term: x__5072: bool
abstract_term: (): unit
abstract_term: (l0
                 (l0
                   (loop_2167 ()
                     (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
                      (k_insert_4666 x__5068 x__5069 x__5070 x__5071))))): X
abstract_term: (l0
                 (loop_2167 ()
                   (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
                    (k_insert_4666 x__5068 x__5069 x__5070 x__5071)))): X
abstract_term: (loop_2167 ()
                 (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
                  (k_insert_4666 x__5068 x__5069 x__5070 x__5071))): X
abstract_term: (fun (x__5068:bool) (x__5069:int) (x__5070:bool) (x__5071:int) ->
                (k_insert_4666 x__5068 x__5069 x__5070 x__5071)): (bool -> int -> bool -> int -> X)
abst_arg: x__5068, bool
abst_arg: x__5069, int
abst_arg: x__5070, bool
abst_arg: x__5071, int
abst_arg: x__5068, bool
abst_arg: x__5069, int
abst_arg: x__5070, bool
abst_arg: x__5071, int
abstract_term: (k_insert_4666 x__5068 x__5069 x__5070 x__5071): X
abstract_term: x__5071: x_1:int[(not x__5068) || (not x__5070) || (not (x1_4664 <= x2_4665)) || 
                                x1_4664 < 0 || x2_4665 < 0 || x__5069 <= x_1]
cond: (not (p11_4797 <=> xs11_4683)); (not (1 = x2_4665)); xs21_4685; (
      not (x2_4665 <= 0)); (x1_4664 <= 0); (not (x_1053 < p12_4798)); p11_4797; true
pbs: xs22_4686 := ((((((not xs11_4683) || (not xs21_4685)) || (not (0 <= (x2_4665 - 1)))) || (0 < 0)) ||
                    ((x2_4665 - 1) < 0))
                   || (xs12_4684 <= xs22_4686));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not x__5068) || (not x__5070)) || (not (x1_4664 <= x2_4665))) || (x1_4664 < 0)) || (x2_4665 < 0)) ||
   (x__5069 <= x__5071))
tt:false
ff:false

abstract_term: x__5070: bool
abstract_term: x__5069: int
abstract_term: x__5068: bool
abstract_term: (): unit
abstract_term: (l0
                 (loop_2167 ()
                   (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                    (k_insert_4666 x__5064 x__5065 x__5066 x__5067)))): X
abstract_term: (loop_2167 ()
                 (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                  (k_insert_4666 x__5064 x__5065 x__5066 x__5067))): X
abstract_term: (fun (x__5064:bool) (x__5065:int) (x__5066:bool) (x__5067:int) ->
                (k_insert_4666 x__5064 x__5065 x__5066 x__5067)): (bool -> int -> bool -> int -> X)
abst_arg: x__5064, bool
abst_arg: x__5065, int
abst_arg: x__5066, bool
abst_arg: x__5067, int
abst_arg: x__5064, bool
abst_arg: x__5065, int
abst_arg: x__5066, bool
abst_arg: x__5067, int
abstract_term: (k_insert_4666 x__5064 x__5065 x__5066 x__5067): X
abstract_term: x__5067: x_1:int[(not x__5064) || (not x__5066) || (not (x1_4664 <= x2_4665)) || 
                                x1_4664 < 0 || x2_4665 < 0 || x__5065 <= x_1]
cond: (not xs21_4685); (not (x2_4665 <= 0)); (x1_4664 <= 0); (not (x_1053 < p12_4798)); p11_4797; true
pbs: xs22_4686 := ((((((not xs11_4683) || (not xs21_4685)) || (not (0 <= (x2_4665 - 1)))) || (0 < 0)) ||
                    ((x2_4665 - 1) < 0))
                   || (xs12_4684 <= xs22_4686));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not x__5064) || (not x__5066)) || (not (x1_4664 <= x2_4665))) || (x1_4664 < 0)) || (x2_4665 < 0)) ||
   (x__5065 <= x__5067))
tt:false
ff:false

abstract_term: x__5066: bool
abstract_term: x__5065: int
abstract_term: x__5064: bool
abstract_term: (): unit
abstract_term: (x2_4665 - 1): int
abstract_term: 0: int
abstract_term: (l1
                 (if (x2_4665 <= 0)
                   (l0
                     (ys''ys''_4650 x1_4664 0
                       (fun (p11_4693:bool) (p12_4694:int) (p21_4695:bool) 
                            (p22_4696:x_1:int[(not p11_4693) || (not p21_4695) || (
                                              not (x1_4664 <= 0)) || 
                                              x1_4664 < 0 || 0 < 0 || 
                                              p12_4694 <= x_1])
                        -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                   (l1
                     (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1)
                       (fun (x__5060:bool) (x__5061:int) (x__5062:bool) 
                            (x__5063:x_1:int[(not x__5060) || (not x__5062) || 
                                             (not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                             x1_4664 - 1 < 0 || x2_4665 - 1 < 0 || 
                                             x__5061 <= x_1])
                        -> (k_insert_4666 x__5060 x__5061 x__5062 x__5063)))))): X
abstract_term: (if (x2_4665 <= 0)
                 (l0
                   (ys''ys''_4650 x1_4664 0
                     (fun (p11_4693:bool) (p12_4694:int) (p21_4695:bool) 
                          (p22_4696:x_1:int[(not p11_4693) || (not p21_4695) || (
                                            not (x1_4664 <= 0)) || x1_4664 < 0 || 
                                            0 < 0 || p12_4694 <= x_1])
                      -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))))
                 (l1
                   (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1)
                     (fun (x__5060:bool) (x__5061:int) (x__5062:bool) 
                          (x__5063:x_1:int[(not x__5060) || (not x__5062) || (
                                           not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                           x1_4664 - 1 < 0 || x2_4665 - 1 < 0 || 
                                           x__5061 <= x_1])
                      -> (k_insert_4666 x__5060 x__5061 x__5062 x__5063))))): X
abstract_term: (x2_4665 <= 0): x_1:bool[x_1]
cond: (not (x1_4664 <= 0)); (not (x_1053 < p12_4798)); p11_4797; true
pbs: p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:(x2_4665 <= 0)
tt:false
ff:false

abstract_term: (l0
                 (ys''ys''_4650 x1_4664 0
                   (fun (p11_4693:bool) (p12_4694:int) (p21_4695:bool) 
                        (p22_4696:x_1:int[(not p11_4693) || (not p21_4695) || (
                                          not (x1_4664 <= 0)) || x1_4664 < 0 || 
                                          0 < 0 || p12_4694 <= x_1])
                    -> (k_insert_4666 p11_4693 p12_4694 true p12_4798)))): X
abstract_term: (ys''ys''_4650 x1_4664 0
                 (fun (p11_4693:bool) (p12_4694:int) (p21_4695:bool) 
                      (p22_4696:x_1:int[(not p11_4693) || (not p21_4695) || (
                                        not (x1_4664 <= 0)) || x1_4664 < 0 || 
                                        0 < 0 || p12_4694 <= x_1])
                  -> (k_insert_4666 p11_4693 p12_4694 true p12_4798))): X
abstract_term: (fun (p11_4693:bool) (p12_4694:int) (p21_4695:bool) 
                    (p22_4696:x_1:int[(not p11_4693) || (not p21_4695) || (
                                      not (x1_4664 <= 0)) || x1_4664 < 0 || 
                                      0 < 0 || p12_4694 <= x_1])
                -> (k_insert_4666 p11_4693 p12_4694 true p12_4798)): (
x_1:bool ->
x_2:int ->
x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x1_4664 <= 0)) || x1_4664 < 0 || 0 < 0 || x_2 <= x_4] -> X)
abst_arg: p11_4693, bool
abst_arg: p12_4694, int
abst_arg: p21_4695, bool
abst_arg: p22_4696, x_1:int[(not p11_4693) || (not p21_4695) || (not (x1_4664 <= 0)) || 
                            x1_4664 < 0 || 0 < 0 || p12_4694 <= x_1]
abst_arg: p11_4693, bool
abst_arg: p12_4694, int
abst_arg: p21_4695, bool
abst_arg: p22_4696, x_1:int[(not p11_4693) || (not p21_4695) || (not (x1_4664 <= 0)) || 
                            x1_4664 < 0 || 0 < 0 || p12_4694 <= x_1]
abstract_term: (k_insert_4666 p11_4693 p12_4694 true p12_4798): X
abstract_term: p12_4798: x_1:int[(not p11_4693) || (not true) || (not (x1_4664 <= x2_4665)) || 
                                 x1_4664 < 0 || x2_4665 < 0 || p12_4694 <= x_1]
cond: (x2_4665 <= 0); (not (x1_4664 <= 0)); (not (x_1053 < p12_4798)); p11_4797; true
pbs: p22_4696 := ((((((not p11_4693) || (not p21_4695)) || (not (x1_4664 <= 0))) || (x1_4664 < 0)) || (0 < 0)) ||
                  (p12_4694 <= p22_4696));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not p11_4693) || (not true)) || (not (x1_4664 <= x2_4665))) || (x1_4664 < 0)) || (x2_4665 < 0)) ||
   (p12_4694 <= p12_4798))
tt:true
ff:false

abstract_term: true: bool
abstract_term: p12_4694: int
abstract_term: p11_4693: bool
abstract_term: 0: int
abstract_term: x1_4664: int
abstract_term: (l1
                 (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1)
                   (fun (x__5060:bool) (x__5061:int) (x__5062:bool) 
                        (x__5063:x_1:int[(not x__5060) || (not x__5062) || (
                                         not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                         x1_4664 - 1 < 0 || x2_4665 - 1 < 0 || 
                                         x__5061 <= x_1])
                    -> (k_insert_4666 x__5060 x__5061 x__5062 x__5063)))): X
abstract_term: (ys''ys''_4650 (x1_4664 - 1) (x2_4665 - 1)
                 (fun (x__5060:bool) (x__5061:int) (x__5062:bool) 
                      (x__5063:x_1:int[(not x__5060) || (not x__5062) || (
                                       not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                       x1_4664 - 1 < 0 || x2_4665 - 1 < 0 || 
                                       x__5061 <= x_1])
                  -> (k_insert_4666 x__5060 x__5061 x__5062 x__5063))): X
abstract_term: (fun (x__5060:bool) (x__5061:int) (x__5062:bool) 
                    (x__5063:x_1:int[(not x__5060) || (not x__5062) || (
                                     not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                                     x1_4664 - 1 < 0 || x2_4665 - 1 < 0 || 
                                     x__5061 <= x_1])
                -> (k_insert_4666 x__5060 x__5061 x__5062 x__5063)): (
x_1:bool ->
x_2:int ->
x_3:bool ->
x_4:int[(not x_1) || (not x_3) || (not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
        x1_4664 - 1 < 0 || x2_4665 - 1 < 0 || x_2 <= x_4]
-> X)
abst_arg: x__5060, bool
abst_arg: x__5061, int
abst_arg: x__5062, bool
abst_arg: x__5063, x_1:int[(not x__5060) || (not x__5062) || (not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                           x1_4664 - 1 < 0 || x2_4665 - 1 < 0 || x__5061 <= x_1]
abst_arg: x__5060, bool
abst_arg: x__5061, int
abst_arg: x__5062, bool
abst_arg: x__5063, x_1:int[(not x__5060) || (not x__5062) || (not ((x1_4664 - 1) <= (x2_4665 - 1))) || 
                           x1_4664 - 1 < 0 || x2_4665 - 1 < 0 || x__5061 <= x_1]
abstract_term: (k_insert_4666 x__5060 x__5061 x__5062 x__5063): X
abstract_term: x__5063: x_1:int[(not x__5060) || (not x__5062) || (not (x1_4664 <= x2_4665)) || 
                                x1_4664 < 0 || x2_4665 < 0 || x__5061 <= x_1]
cond: (not (x2_4665 <= 0)); (not (x1_4664 <= 0)); (not (x_1053 < p12_4798)); p11_4797; true
pbs: x__5063 := ((((((not x__5060) || (not x__5062)) || (not ((x1_4664 - 1) <= (x2_4665 - 1)))) || ((x1_4664 - 1) < 0))
                  || ((x2_4665 - 1) < 0))
                 || (x__5061 <= x__5063));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not x__5060) || (not x__5062)) || (not (x1_4664 <= x2_4665))) || (x1_4664 < 0)) || (x2_4665 < 0)) ||
   (x__5061 <= x__5063))
tt:x__5063
ff:false

abstract_term: x__5062: bool
abstract_term: x__5061: int
abstract_term: x__5060: bool
abstract_term: (x2_4665 - 1): int
abstract_term: (x1_4664 - 1): int
abstract_term: (fun (x1_5042:int) (x2_5043:int) 
                    (k_insert_ys'ys'_5044:(x_1:bool ->
                                           x_2:int ->
                                           x_3:bool ->
                                           x_4:int[(not x_1) || (not x_3) || (
                                                   not (x1_5042 <= x2_5043)) || 
                                                   x1_5042 < 0 || x2_5043 < 0 || 
                                                   x_2 <= x_4]
                                           -> X))
                ->
                (ysys_1016 (x1_5042 + 1) (x2_5043 + 1)
                  (fun (x__5088:bool) (x__5089:int) (x__5090:bool) 
                       (x__5091:x_1:int[(not x__5088) || (not x__5090) || (
                                        not ((x1_5042 + 1) <= (x2_5043 + 1))) || 
                                        x1_5042 + 1 < 0 || x2_5043 + 1 < 0 || 
                                        x__5089 <= x_1])
                   -> (k_insert_ys'ys'_5044 x__5088 x__5089 x__5090 x__5091)))): (
x_1:int ->
x_2:int ->
(x_4:bool ->
 x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
-> X)
abst_arg: x1_5042, int
abst_arg: x2_5043, int
abst_arg: k_insert_ys'ys'_5044, (x_1:bool ->
                                 x_2:int ->
                                 x_3:bool ->
                                 x_4:int[(not x_1) || (not x_3) || (not (x1_5042 <= x2_5043)) || 
                                         x1_5042 < 0 || x2_5043 < 0 || 
                                         x_2 <= x_4]
                                 -> X)
abst_arg: x1_5042, int
abst_arg: x2_5043, int
abst_arg: k_insert_ys'ys'_5044, (x_1:bool ->
                                 x_2:int ->
                                 x_3:bool ->
                                 x_4:int[(not x_1) || (not x_3) || (not (x1_5042 <= x2_5043)) || 
                                         x1_5042 < 0 || x2_5043 < 0 || 
                                         x_2 <= x_4]
                                 -> X)
abstract_term: (ysys_1016 (x1_5042 + 1) (x2_5043 + 1)
                 (fun (x__5088:bool) (x__5089:int) (x__5090:bool) 
                      (x__5091:x_1:int[(not x__5088) || (not x__5090) || (
                                       not ((x1_5042 + 1) <= (x2_5043 + 1))) || 
                                       x1_5042 + 1 < 0 || x2_5043 + 1 < 0 || 
                                       x__5089 <= x_1])
                  -> (k_insert_ys'ys'_5044 x__5088 x__5089 x__5090 x__5091))): X
abstract_term: (fun (x__5088:bool) (x__5089:int) (x__5090:bool) 
                    (x__5091:x_1:int[(not x__5088) || (not x__5090) || (
                                     not ((x1_5042 + 1) <= (x2_5043 + 1))) || 
                                     x1_5042 + 1 < 0 || x2_5043 + 1 < 0 || 
                                     x__5089 <= x_1])
                -> (k_insert_ys'ys'_5044 x__5088 x__5089 x__5090 x__5091)): (
x_1:bool ->
x_2:int ->
x_3:bool ->
x_4:int[(not x_1) || (not x_3) || (not ((x1_5042 + 1) <= (x2_5043 + 1))) || 
        x1_5042 + 1 < 0 || x2_5043 + 1 < 0 || x_2 <= x_4]
-> X)
abst_arg: x__5088, bool
abst_arg: x__5089, int
abst_arg: x__5090, bool
abst_arg: x__5091, x_1:int[(not x__5088) || (not x__5090) || (not ((x1_5042 + 1) <= (x2_5043 + 1))) || 
                           x1_5042 + 1 < 0 || x2_5043 + 1 < 0 || x__5089 <= x_1]
abst_arg: x__5088, bool
abst_arg: x__5089, int
abst_arg: x__5090, bool
abst_arg: x__5091, x_1:int[(not x__5088) || (not x__5090) || (not ((x1_5042 + 1) <= (x2_5043 + 1))) || 
                           x1_5042 + 1 < 0 || x2_5043 + 1 < 0 || x__5089 <= x_1]
abstract_term: (k_insert_ys'ys'_5044 x__5088 x__5089 x__5090 x__5091): X
abstract_term: x__5091: x_1:int[(not x__5088) || (not x__5090) || (not (x1_5042 <= x2_5043)) || 
                                x1_5042 < 0 || x2_5043 < 0 || x__5089 <= x_1]
cond: (not (x_1053 < p12_4798)); p11_4797; true
pbs: x__5091 := ((((((not x__5088) || (not x__5090)) || (not ((x1_5042 + 1) <= (x2_5043 + 1)))) || ((x1_5042 + 1) < 0))
                  || ((x2_5043 + 1) < 0))
                 || (x__5089 <= x__5091));
     p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not x__5088) || (not x__5090)) || (not (x1_5042 <= x2_5043))) || (x1_5042 < 0)) || (x2_5043 < 0)) ||
   (x__5089 <= x__5091))
tt:x__5091
ff:false

abstract_term: x__5090: bool
abstract_term: x__5089: int
abstract_term: x__5088: bool
abstract_term: (x2_5043 + 1): int
abstract_term: (x1_5042 + 1): int
abstract_term: x_1053: int
abstract_term: (l0
                 (k_insert_2260
                   (fun (x1_4962:int) (x2_4963:int) 
                        (k_insert_rsrs_4964:(x_1:bool ->
                                             x_2:int ->
                                             x_3:bool ->
                                             x_4:int[(not x_1) || (not x_3) || (
                                                     not (x1_4962 <= x2_4963)) || 
                                                     x1_4962 < 0 || x2_4963 < 0 || 
                                                     x_2 <= x_4]
                                             -> X))
                    ->
                    (if (x2_4963 = x1_4962)
                      (l0
                        (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                          (l1 (k_insert_rsrs_4964 false 0 false 0))))
                      (l1
                        (if (x1_4962 <= 0)
                          (l0
                            (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                              (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
                          (l1
                            (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                              (l1 (k_insert_rsrs_4964 false 0 false 0)))))))))): X
abstract_term: (k_insert_2260
                 (fun (x1_4962:int) (x2_4963:int) 
                      (k_insert_rsrs_4964:(x_1:bool ->
                                           x_2:int ->
                                           x_3:bool ->
                                           x_4:int[(not x_1) || (not x_3) || (
                                                   not (x1_4962 <= x2_4963)) || 
                                                   x1_4962 < 0 || x2_4963 < 0 || 
                                                   x_2 <= x_4]
                                           -> X))
                  ->
                  (if (x2_4963 = x1_4962)
                    (l0
                      (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                        (l1 (k_insert_rsrs_4964 false 0 false 0))))
                    (l1
                      (if (x1_4962 <= 0)
                        (l0
                          (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                            (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
                        (l1
                          (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                            (l1 (k_insert_rsrs_4964 false 0 false 0))))))))): X
abstract_term: (fun (x1_4962:int) (x2_4963:int) 
                    (k_insert_rsrs_4964:(x_1:bool ->
                                         x_2:int ->
                                         x_3:bool ->
                                         x_4:int[(not x_1) || (not x_3) || (
                                                 not (x1_4962 <= x2_4963)) || 
                                                 x1_4962 < 0 || x2_4963 < 0 || 
                                                 x_2 <= x_4]
                                         -> X))
                ->
                (if (x2_4963 = x1_4962)
                  (l0
                    (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                      (l1 (k_insert_rsrs_4964 false 0 false 0))))
                  (l1
                    (if (x1_4962 <= 0)
                      (l0
                        (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                          (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
                      (l1
                        (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                          (l1 (k_insert_rsrs_4964 false 0 false 0)))))))): (
x_1:int ->
x_2:int ->
(x_4:bool ->
 x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
-> X)
abst_arg: x1_4962, int
abst_arg: x2_4963, int
abst_arg: k_insert_rsrs_4964, (x_1:bool ->
                               x_2:int ->
                               x_3:bool ->
                               x_4:int[(not x_1) || (not x_3) || (not (x1_4962 <= x2_4963)) || 
                                       x1_4962 < 0 || x2_4963 < 0 || 
                                       x_2 <= x_4]
                               -> X)
abst_arg: x1_4962, int
abst_arg: x2_4963, int
abst_arg: k_insert_rsrs_4964, (x_1:bool ->
                               x_2:int ->
                               x_3:bool ->
                               x_4:int[(not x_1) || (not x_3) || (not (x1_4962 <= x2_4963)) || 
                                       x1_4962 < 0 || x2_4963 < 0 || 
                                       x_2 <= x_4]
                               -> X)
abstract_term: (if (x2_4963 = x1_4962)
                 (l0
                   (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                     (l1 (k_insert_rsrs_4964 false 0 false 0))))
                 (l1
                   (if (x1_4962 <= 0)
                     (l0
                       (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                         (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
                     (l1
                       (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                         (l1 (k_insert_rsrs_4964 false 0 false 0))))))): X
abstract_term: (x2_4963 = x1_4962): x_1:bool[x_1]
cond: (not p11_4797); true
pbs: p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:(x2_4963 = x1_4962)
tt:false
ff:false

abstract_term: (l0
                 (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                   (l1 (k_insert_rsrs_4964 false 0 false 0)))): X
abstract_term: (if (x1_4962 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                 (l1 (k_insert_rsrs_4964 false 0 false 0))): X
abstract_term: (x1_4962 <= 0): x_1:bool[x_1]
cond: (x2_4963 = x1_4962); (not p11_4797); true
pbs: p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:(x1_4962 <= 0)
tt:false
ff:false

abstract_term: (l0 (k_insert_rsrs_4964 true x_1053 true x_1053)): X
abstract_term: (k_insert_rsrs_4964 true x_1053 true x_1053): X
abstract_term: x_1053: x_1:int[(not true) || (not true) || (not (x1_4962 <= x2_4963)) || 
                               x1_4962 < 0 || x2_4963 < 0 || x_1053 <= x_1]
cond: (x1_4962 <= 0); (x2_4963 = x1_4962); (not p11_4797); true
pbs: p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not true) || (not true)) || (not (x1_4962 <= x2_4963))) || (x1_4962 < 0)) || (x2_4963 < 0)) ||
   (x_1053 <= x_1053))
tt:true
ff:false

abstract_term: true: bool
abstract_term: x_1053: int
abstract_term: true: bool
abstract_term: (l1 (k_insert_rsrs_4964 false 0 false 0)): X
abstract_term: (k_insert_rsrs_4964 false 0 false 0): X
abstract_term: 0: x_1:int[(not false) || (not false) || (not (x1_4962 <= x2_4963)) || 
                          x1_4962 < 0 || x2_4963 < 0 || 0 <= x_1]
cond: (not (x1_4962 <= 0)); (x2_4963 = x1_4962); (not p11_4797); true
pbs: p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not false) || (not false)) || (not (x1_4962 <= x2_4963))) || (x1_4962 < 0)) || (x2_4963 < 0)) || (0 <= 0))
tt:true
ff:false

abstract_term: false: bool
abstract_term: 0: int
abstract_term: false: bool
abstract_term: (l1
                 (if (x1_4962 <= 0)
                   (l0
                     (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                       (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
                   (l1
                     (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                       (l1 (k_insert_rsrs_4964 false 0 false 0)))))): X
abstract_term: (if (x1_4962 <= 0)
                 (l0
                   (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                     (l1 (k_insert_rsrs_4964 true x_1053 false 0))))
                 (l1
                   (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                     (l1 (k_insert_rsrs_4964 false 0 false 0))))): X
abstract_term: (x1_4962 <= 0): x_1:bool[x_1]
cond: (not (x2_4963 = x1_4962)); (not p11_4797); true
pbs: p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:(x1_4962 <= 0)
tt:false
ff:false

abstract_term: (l0
                 (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                   (l1 (k_insert_rsrs_4964 true x_1053 false 0)))): X
abstract_term: (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 true x_1053 true x_1053))
                 (l1 (k_insert_rsrs_4964 true x_1053 false 0))): X
abstract_term: (x2_4963 <= 0): x_1:bool[x_1]
cond: (x1_4962 <= 0); (not (x2_4963 = x1_4962)); (not p11_4797); true
pbs: p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:(x2_4963 <= 0)
tt:false
ff:false

abstract_term: (l0 (k_insert_rsrs_4964 true x_1053 true x_1053)): X
abstract_term: (k_insert_rsrs_4964 true x_1053 true x_1053): X
abstract_term: x_1053: x_1:int[(not true) || (not true) || (not (x1_4962 <= x2_4963)) || 
                               x1_4962 < 0 || x2_4963 < 0 || x_1053 <= x_1]
cond: (x2_4963 <= 0); (x1_4962 <= 0); (not (x2_4963 = x1_4962)); (not p11_4797); true
pbs: p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not true) || (not true)) || (not (x1_4962 <= x2_4963))) || (x1_4962 < 0)) || (x2_4963 < 0)) ||
   (x_1053 <= x_1053))
tt:true
ff:false

abstract_term: true: bool
abstract_term: x_1053: int
abstract_term: true: bool
abstract_term: (l1 (k_insert_rsrs_4964 true x_1053 false 0)): X
abstract_term: (k_insert_rsrs_4964 true x_1053 false 0): X
abstract_term: 0: x_1:int[(not true) || (not false) || (not (x1_4962 <= x2_4963)) || 
                          x1_4962 < 0 || x2_4963 < 0 || x_1053 <= x_1]
cond: (not (x2_4963 <= 0)); (x1_4962 <= 0); (not (x2_4963 = x1_4962)); (not p11_4797); true
pbs: p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not true) || (not false)) || (not (x1_4962 <= x2_4963))) || (x1_4962 < 0)) || (x2_4963 < 0)) || (x_1053 <= 0))
tt:true
ff:false

abstract_term: false: bool
abstract_term: x_1053: int
abstract_term: true: bool
abstract_term: (l1
                 (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                   (l1 (k_insert_rsrs_4964 false 0 false 0)))): X
abstract_term: (if (x2_4963 <= 0) (l0 (k_insert_rsrs_4964 false 0 true x_1053))
                 (l1 (k_insert_rsrs_4964 false 0 false 0))): X
abstract_term: (x2_4963 <= 0): x_1:bool[x_1]
cond: (not (x1_4962 <= 0)); (not (x2_4963 = x1_4962)); (not p11_4797); true
pbs: p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:(x2_4963 <= 0)
tt:false
ff:false

abstract_term: (l0 (k_insert_rsrs_4964 false 0 true x_1053)): X
abstract_term: (k_insert_rsrs_4964 false 0 true x_1053): X
abstract_term: x_1053: x_1:int[(not false) || (not true) || (not (x1_4962 <= x2_4963)) || 
                               x1_4962 < 0 || x2_4963 < 0 || 0 <= x_1]
cond: (x2_4963 <= 0); (not (x1_4962 <= 0)); (not (x2_4963 = x1_4962)); (not p11_4797); true
pbs: p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not false) || (not true)) || (not (x1_4962 <= x2_4963))) || (x1_4962 < 0)) || (x2_4963 < 0)) || (0 <= x_1053))
tt:true
ff:false

abstract_term: true: bool
abstract_term: 0: int
abstract_term: false: bool
abstract_term: (l1 (k_insert_rsrs_4964 false 0 false 0)): X
abstract_term: (k_insert_rsrs_4964 false 0 false 0): X
abstract_term: 0: x_1:int[(not false) || (not false) || (not (x1_4962 <= x2_4963)) || 
                          x1_4962 < 0 || x2_4963 < 0 || 0 <= x_1]
cond: (not (x2_4963 <= 0)); (not (x1_4962 <= 0)); (not (x2_4963 = x1_4962)); (not p11_4797); true
pbs: p22_4800 := ((((((not p11_4797) || (not p21_4799)) || (not (0 <= 0))) || (0 < 0)) || (0 < 0)) ||
                  (p12_4798 <= p22_4800))
p:((((((not false) || (not false)) || (not (x1_4962 <= x2_4963))) || (x1_4962 < 0)) || (x2_4963 < 0)) || (0 <= 0))
tt:true
ff:false

abstract_term: false: bool
abstract_term: 0: int
abstract_term: false: bool
abstract_term: 0: int
abstract_term: 0: int
insertsort_2165: ENV: xsxs_1042:(int -> int -> (bool -> int -> bool -> int -> X) -> X),
k_insertsort_2811:((x_2:int ->
                    x_3:int ->
                    (x_5:bool ->
                     x_6:int ->
                     x_7:bool ->
                     x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_2 < 0 || x_3 < 0 || x_6 <= x_8] -> X)
                    -> X) -> X),


abst_arg: xsxs_1042, (int -> int -> (bool -> int -> bool -> int -> X) -> X)
abst_arg: k_insertsort_2811, ((x_2:int ->
                               x_3:int ->
                               (x_5:bool ->
                                x_6:int ->
                                x_7:bool ->
                                x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || 
                                        x_2 < 0 || x_3 < 0 || x_6 <= x_8]
                                -> X)
                               -> X) ->
X)
abst_arg: xsxs_1042, (int -> int -> (bool -> int -> bool -> int -> X) -> X)
abst_arg: k_insertsort_2811, ((x_2:int ->
                               x_3:int ->
                               (x_5:bool ->
                                x_6:int ->
                                x_7:bool ->
                                x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || 
                                        x_2 < 0 || x_3 < 0 || x_6 <= x_8]
                                -> X)
                               -> X) ->
X)
insertsort_2165: (xsxs_1042 0 0
                   (fun (p11_4699:bool) (p12_4700:int) (p21_4701:bool) (p22_4702:int) ->
                    (if p11_4699
                      (l1
                        (xsxs_1042 0 0
                          (fun (p11_4807:bool) (p12_4808:int) (p21_4809:bool) (p22_4810:int) ->
                           (insertsort_2165
                             (fun (x1_4989:int) (x2_4990:int) 
                                  (k_insertsort_xs'xs'_4991:(bool -> int -> bool -> int -> X))
                              ->
                              (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1)
                                (fun (x__5135:bool) (x__5136:int) (x__5137:bool) (x__5138:int) ->
                                 (k_insertsort_xs'xs'_4991 x__5135 x__5136 x__5137 x__5138))))
                             (fun (x_4734:(x_1:int ->
                                           x_2:int ->
                                           (x_4:bool ->
                                            x_5:int ->
                                            x_6:bool ->
                                            x_7:int[(not x_4) || (not x_6) || (
                                                    not (x_1 <= x_2)) || 
                                                    x_1 < 0 || x_2 < 0 || 
                                                    x_5 <= x_7]
                                            -> X)
                                           -> X))
                              ->
                              (insert_1014 p12_4808
                                (fun (x__5128:int) (x__5129:int) 
                                     (x__5130:(x_1:bool ->
                                               x_2:int ->
                                               x_3:bool ->
                                               x_4:int[(not x_1) || (
                                                       not x_3) || (not (x__5128 <= x__5129)) || 
                                                       x__5128 < 0 || 
                                                       x__5129 < 0 || 
                                                       x_2 <= x_4]
                                               -> X))
                                 ->
                                 (x_4734 x__5128 x__5129
                                   (fun (x__5131:bool) (x__5132:int) (x__5133:bool) 
                                        (x__5134:x_1:int[(not x__5131) || (
                                                         not x__5133) || (
                                                         not (x__5128 <= x__5129)) || 
                                                         x__5128 < 0 || 
                                                         x__5129 < 0 || 
                                                         x__5132 <= x_1])
                                    -> (x__5130 x__5131 x__5132 x__5133 x__5134))))
                                (fun (x__5120:(x_1:int ->
                                               x_2:int ->
                                               (x_4:bool ->
                                                x_5:int ->
                                                x_6:bool ->
                                                x_7:int[(not x_4) || (
                                                        not x_6) || (
                                                        not (x_1 <= x_2)) || 
                                                        x_1 < 0 || x_2 < 0 || 
                                                        x_5 <= x_7]
                                                -> X)
                                               -> X))
                                 ->
                                 (k_insertsort_2811
                                   (fun (x__5121:int) (x__5122:int) 
                                        (x__5123:(x_1:bool ->
                                                  x_2:int ->
                                                  x_3:bool ->
                                                  x_4:int[(not x_1) || (
                                                          not x_3) || (
                                                          not (x__5121 <= x__5122)) || 
                                                          x__5121 < 0 || 
                                                          x__5122 < 0 || 
                                                          x_2 <= x_4]
                                                  -> X))
                                    ->
                                    (x__5120 x__5121 x__5122
                                      (fun (x__5124:bool) (x__5125:int) (x__5126:bool) 
                                           (x__5127:x_1:int[(not x__5124) || (
                                                            not x__5126) || (
                                                            not (x__5121 <= x__5122)) || 
                                                            x__5121 < 0 || 
                                                            x__5122 < 0 || 
                                                            x__5125 <= x_1])
                                       -> (x__5123 x__5124 x__5125 x__5126 x__5127))))))))))))
                      (l0
                        (k_insertsort_2811
                          (fun (i1_4970:int) (i2_4971:int) 
                               (k_insertsort_rsrs_4972:(x_1:bool ->
                                                        x_2:int ->
                                                        x_3:bool ->
                                                        x_4:int[(not x_1) || (
                                                                not x_3) || (
                                                                not (i1_4970 <= i2_4971)) || 
                                                                i1_4970 < 0 || 
                                                                i2_4971 < 0 || 
                                                                x_2 <= x_4]
                                                        -> X))
                           -> (k_insertsort_rsrs_4972 false 0 false 0))))))) ===> (
xsxs_1042 0 0
 (fun (p11_4699:bool) (p12_4700:int) (p21_4701:bool) (p22_4702:int) ->
  (if p11_4699
    (l1
      (xsxs_1042 0 0
        (fun (p11_4807:bool) (p12_4808:int) (p21_4809:bool) (p22_4810:int) ->
         (insertsort_2165
           (fun (x1_4989:int) (x2_4990:int) (k_insertsort_xs'xs'_4991:(bool -> int -> bool -> int -> X)) ->
            (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1)
              (fun (x__5135:bool) (x__5136:int) (x__5137:bool) (x__5138:int) ->
               (k_insertsort_xs'xs'_4991 x__5135 x__5136 x__5137 x__5138))))
           (fun (x_4734:(x_1:int ->
                         x_2:int ->
                         (x_4:bool ->
                          x_5:int ->
                          x_6:bool ->
                          x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] ->
                          X)
                         -> X))
            ->
            (insert_1014 p12_4808
              (fun (x__5128:int) (x__5129:int) 
                   (x__5130:(x_1:bool ->
                             x_2:int ->
                             x_3:bool ->
                             x_4:int[(not x_1) || (not x_3) || (not (x__5128 <= x__5129)) || 
                                     x__5128 < 0 || x__5129 < 0 || x_2 <= x_4]
                             -> X))
               ->
               (x_4734 x__5128 x__5129
                 (fun (x__5131:bool) (x__5132:int) (x__5133:bool) 
                      (x__5134:x_1:int[(not x__5131) || (not x__5133) || (
                                       not (x__5128 <= x__5129)) || x__5128 < 0 || 
                                       x__5129 < 0 || x__5132 <= x_1])
                  -> (x__5130 x__5131 x__5132 x__5133 x__5134))))
              (fun (x__5120:(x_1:int ->
                             x_2:int ->
                             (x_4:bool ->
                              x_5:int ->
                              x_6:bool ->
                              x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7]
                              -> X)
                             -> X))
               ->
               (k_insertsort_2811
                 (fun (x__5121:int) (x__5122:int) 
                      (x__5123:(x_1:bool ->
                                x_2:int ->
                                x_3:bool ->
                                x_4:int[(not x_1) || (not x_3) || (not (x__5121 <= x__5122)) || 
                                        x__5121 < 0 || x__5122 < 0 || 
                                        x_2 <= x_4]
                                -> X))
                  ->
                  (x__5120 x__5121 x__5122
                    (fun (x__5124:bool) (x__5125:int) (x__5126:bool) 
                         (x__5127:x_1:int[(not x__5124) || (not x__5126) || (
                                          not (x__5121 <= x__5122)) || 
                                          x__5121 < 0 || x__5122 < 0 || 
                                          x__5125 <= x_1])
                     -> (x__5123 x__5124 x__5125 x__5126 x__5127))))))))))))
    (l0
      (k_insertsort_2811
        (fun (i1_4970:int) (i2_4971:int) 
             (k_insertsort_rsrs_4972:(x_1:bool ->
                                      x_2:int ->
                                      x_3:bool ->
                                      x_4:int[(not x_1) || (not x_3) || (
                                              not (i1_4970 <= i2_4971)) || 
                                              i1_4970 < 0 || i2_4971 < 0 || 
                                              x_2 <= x_4]
                                      -> X))
         -> (k_insertsort_rsrs_4972 false 0 false 0)))))))
insertsort_2165:: (xsxs_1042 0 0
                    (fun (p11_4699:bool) (p12_4700:int) (p21_4701:bool) (p22_4702:int) ->
                     (if p11_4699
                       (l1
                         (xsxs_1042 0 0
                           (fun (p11_4807:bool) (p12_4808:int) (p21_4809:bool) (p22_4810:int) ->
                            (insertsort_2165
                              (fun (x1_4989:int) (x2_4990:int) 
                                   (k_insertsort_xs'xs'_4991:(bool -> int -> bool -> int -> X))
                               ->
                               (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1)
                                 (fun (x__5135:bool) (x__5136:int) (x__5137:bool) (x__5138:int) ->
                                  (k_insertsort_xs'xs'_4991 x__5135 x__5136 x__5137 x__5138))))
                              (fun (x_4734:(x_1:int ->
                                            x_2:int ->
                                            (x_4:bool ->
                                             x_5:int ->
                                             x_6:bool ->
                                             x_7:int[(not x_4) || (not x_6) || (
                                                     not (x_1 <= x_2)) || 
                                                     x_1 < 0 || x_2 < 0 || 
                                                     x_5 <= x_7]
                                             -> X)
                                            -> X))
                               ->
                               (insert_1014 p12_4808
                                 (fun (x__5128:int) (x__5129:int) 
                                      (x__5130:(x_1:bool ->
                                                x_2:int ->
                                                x_3:bool ->
                                                x_4:int[(not x_1) || (
                                                        not x_3) || (
                                                        not (x__5128 <= x__5129)) || 
                                                        x__5128 < 0 || 
                                                        x__5129 < 0 || 
                                                        x_2 <= x_4]
                                                -> X))
                                  ->
                                  (x_4734 x__5128 x__5129
                                    (fun (x__5131:bool) (x__5132:int) (x__5133:bool) 
                                         (x__5134:x_1:int[(not x__5131) || (
                                                          not x__5133) || (
                                                          not (x__5128 <= x__5129)) || 
                                                          x__5128 < 0 || 
                                                          x__5129 < 0 || 
                                                          x__5132 <= x_1])
                                     -> (x__5130 x__5131 x__5132 x__5133 x__5134))))
                                 (fun (x__5120:(x_1:int ->
                                                x_2:int ->
                                                (x_4:bool ->
                                                 x_5:int ->
                                                 x_6:bool ->
                                                 x_7:int[(not x_4) || (
                                                         not x_6) || (
                                                         not (x_1 <= x_2)) || 
                                                         x_1 < 0 || x_2 < 0 || 
                                                         x_5 <= x_7]
                                                 -> X)
                                                -> X))
                                  ->
                                  (k_insertsort_2811
                                    (fun (x__5121:int) (x__5122:int) 
                                         (x__5123:(x_1:bool ->
                                                   x_2:int ->
                                                   x_3:bool ->
                                                   x_4:int[(not x_1) || (
                                                           not x_3) || (
                                                           not (x__5121 <= x__5122)) || 
                                                           x__5121 < 0 || 
                                                           x__5122 < 0 || 
                                                           x_2 <= x_4]
                                                   -> X))
                                     ->
                                     (x__5120 x__5121 x__5122
                                       (fun (x__5124:bool) (x__5125:int) (x__5126:bool) 
                                            (x__5127:x_1:int[(not x__5124) || (
                                                             not x__5126) || (
                                                             not (x__5121 <= x__5122)) || 
                                                             x__5121 < 0 || 
                                                             x__5122 < 0 || 
                                                             x__5125 <= x_1])
                                        -> (x__5123 x__5124 x__5125 x__5126 x__5127))))))))))))
                       (l0
                         (k_insertsort_2811
                           (fun (i1_4970:int) (i2_4971:int) 
                                (k_insertsort_rsrs_4972:(x_1:bool ->
                                                         x_2:int ->
                                                         x_3:bool ->
                                                         x_4:int[(not x_1) || (
                                                                 not x_3) || (
                                                                 not (i1_4970 <= i2_4971)) || 
                                                                 i1_4970 < 0 || 
                                                                 i2_4971 < 0 || 
                                                                 x_2 <= x_4]
                                                         -> X))
                            -> (k_insertsort_rsrs_4972 false 0 false 0)))))))
abstract_term: (xsxs_1042 0 0
                 (fun (p11_4699:bool) (p12_4700:int) (p21_4701:bool) (p22_4702:int) ->
                  (if p11_4699
                    (l1
                      (xsxs_1042 0 0
                        (fun (p11_4807:bool) (p12_4808:int) (p21_4809:bool) (p22_4810:int) ->
                         (insertsort_2165
                           (fun (x1_4989:int) (x2_4990:int) 
                                (k_insertsort_xs'xs'_4991:(bool -> int -> bool -> int -> X))
                            ->
                            (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1)
                              (fun (x__5135:bool) (x__5136:int) (x__5137:bool) (x__5138:int) ->
                               (k_insertsort_xs'xs'_4991 x__5135 x__5136 x__5137 x__5138))))
                           (fun (x_4734:(x_1:int ->
                                         x_2:int ->
                                         (x_4:bool ->
                                          x_5:int ->
                                          x_6:bool ->
                                          x_7:int[(not x_4) || (not x_6) || (
                                                  not (x_1 <= x_2)) || 
                                                  x_1 < 0 || x_2 < 0 || 
                                                  x_5 <= x_7]
                                          -> X)
                                         -> X))
                            ->
                            (insert_1014 p12_4808
                              (fun (x__5128:int) (x__5129:int) 
                                   (x__5130:(x_1:bool ->
                                             x_2:int ->
                                             x_3:bool ->
                                             x_4:int[(not x_1) || (not x_3) || (
                                                     not (x__5128 <= x__5129)) || 
                                                     x__5128 < 0 || x__5129 < 0 || 
                                                     x_2 <= x_4]
                                             -> X))
                               ->
                               (x_4734 x__5128 x__5129
                                 (fun (x__5131:bool) (x__5132:int) (x__5133:bool) 
                                      (x__5134:x_1:int[(not x__5131) || (
                                                       not x__5133) || (
                                                       not (x__5128 <= x__5129)) || 
                                                       x__5128 < 0 || 
                                                       x__5129 < 0 || 
                                                       x__5132 <= x_1])
                                  -> (x__5130 x__5131 x__5132 x__5133 x__5134))))
                              (fun (x__5120:(x_1:int ->
                                             x_2:int ->
                                             (x_4:bool ->
                                              x_5:int ->
                                              x_6:bool ->
                                              x_7:int[(not x_4) || (not x_6) || (
                                                      not (x_1 <= x_2)) || 
                                                      x_1 < 0 || x_2 < 0 || 
                                                      x_5 <= x_7]
                                              -> X)
                                             -> X))
                               ->
                               (k_insertsort_2811
                                 (fun (x__5121:int) (x__5122:int) 
                                      (x__5123:(x_1:bool ->
                                                x_2:int ->
                                                x_3:bool ->
                                                x_4:int[(not x_1) || (
                                                        not x_3) || (
                                                        not (x__5121 <= x__5122)) || 
                                                        x__5121 < 0 || 
                                                        x__5122 < 0 || 
                                                        x_2 <= x_4]
                                                -> X))
                                  ->
                                  (x__5120 x__5121 x__5122
                                    (fun (x__5124:bool) (x__5125:int) (x__5126:bool) 
                                         (x__5127:x_1:int[(not x__5124) || (
                                                          not x__5126) || (
                                                          not (x__5121 <= x__5122)) || 
                                                          x__5121 < 0 || 
                                                          x__5122 < 0 || 
                                                          x__5125 <= x_1])
                                     -> (x__5123 x__5124 x__5125 x__5126 x__5127))))))))))))
                    (l0
                      (k_insertsort_2811
                        (fun (i1_4970:int) (i2_4971:int) 
                             (k_insertsort_rsrs_4972:(x_1:bool ->
                                                      x_2:int ->
                                                      x_3:bool ->
                                                      x_4:int[(not x_1) || (
                                                              not x_3) || (
                                                              not (i1_4970 <= i2_4971)) || 
                                                              i1_4970 < 0 || 
                                                              i2_4971 < 0 || 
                                                              x_2 <= x_4]
                                                      -> X))
                         -> (k_insertsort_rsrs_4972 false 0 false 0))))))): X
abstract_term: (fun (p11_4699:bool) (p12_4700:int) (p21_4701:bool) (p22_4702:int) ->
                (if p11_4699
                  (l1
                    (xsxs_1042 0 0
                      (fun (p11_4807:bool) (p12_4808:int) (p21_4809:bool) (p22_4810:int) ->
                       (insertsort_2165
                         (fun (x1_4989:int) (x2_4990:int) (k_insertsort_xs'xs'_4991:(bool -> int -> bool -> int -> X))
                          ->
                          (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1)
                            (fun (x__5135:bool) (x__5136:int) (x__5137:bool) (x__5138:int) ->
                             (k_insertsort_xs'xs'_4991 x__5135 x__5136 x__5137 x__5138))))
                         (fun (x_4734:(x_1:int ->
                                       x_2:int ->
                                       (x_4:bool ->
                                        x_5:int ->
                                        x_6:bool ->
                                        x_7:int[(not x_4) || (not x_6) || (
                                                not (x_1 <= x_2)) || 
                                                x_1 < 0 || x_2 < 0 || 
                                                x_5 <= x_7]
                                        -> X)
                                       -> X))
                          ->
                          (insert_1014 p12_4808
                            (fun (x__5128:int) (x__5129:int) 
                                 (x__5130:(x_1:bool ->
                                           x_2:int ->
                                           x_3:bool ->
                                           x_4:int[(not x_1) || (not x_3) || (
                                                   not (x__5128 <= x__5129)) || 
                                                   x__5128 < 0 || x__5129 < 0 || 
                                                   x_2 <= x_4]
                                           -> X))
                             ->
                             (x_4734 x__5128 x__5129
                               (fun (x__5131:bool) (x__5132:int) (x__5133:bool) 
                                    (x__5134:x_1:int[(not x__5131) || (
                                                     not x__5133) || (
                                                     not (x__5128 <= x__5129)) || 
                                                     x__5128 < 0 || x__5129 < 0 || 
                                                     x__5132 <= x_1])
                                -> (x__5130 x__5131 x__5132 x__5133 x__5134))))
                            (fun (x__5120:(x_1:int ->
                                           x_2:int ->
                                           (x_4:bool ->
                                            x_5:int ->
                                            x_6:bool ->
                                            x_7:int[(not x_4) || (not x_6) || (
                                                    not (x_1 <= x_2)) || 
                                                    x_1 < 0 || x_2 < 0 || 
                                                    x_5 <= x_7]
                                            -> X)
                                           -> X))
                             ->
                             (k_insertsort_2811
                               (fun (x__5121:int) (x__5122:int) 
                                    (x__5123:(x_1:bool ->
                                              x_2:int ->
                                              x_3:bool ->
                                              x_4:int[(not x_1) || (not x_3) || (
                                                      not (x__5121 <= x__5122)) || 
                                                      x__5121 < 0 || 
                                                      x__5122 < 0 || 
                                                      x_2 <= x_4]
                                              -> X))
                                ->
                                (x__5120 x__5121 x__5122
                                  (fun (x__5124:bool) (x__5125:int) (x__5126:bool) 
                                       (x__5127:x_1:int[(not x__5124) || (
                                                        not x__5126) || (
                                                        not (x__5121 <= x__5122)) || 
                                                        x__5121 < 0 || 
                                                        x__5122 < 0 || 
                                                        x__5125 <= x_1])
                                   -> (x__5123 x__5124 x__5125 x__5126 x__5127))))))))))))
                  (l0
                    (k_insertsort_2811
                      (fun (i1_4970:int) (i2_4971:int) 
                           (k_insertsort_rsrs_4972:(x_1:bool ->
                                                    x_2:int ->
                                                    x_3:bool ->
                                                    x_4:int[(not x_1) || (
                                                            not x_3) || (
                                                            not (i1_4970 <= i2_4971)) || 
                                                            i1_4970 < 0 || 
                                                            i2_4971 < 0 || 
                                                            x_2 <= x_4]
                                                    -> X))
                       -> (k_insertsort_rsrs_4972 false 0 false 0)))))): (
bool -> int -> bool -> int -> X)
abst_arg: p11_4699, bool
abst_arg: p12_4700, int
abst_arg: p21_4701, bool
abst_arg: p22_4702, int
abst_arg: p11_4699, bool
abst_arg: p12_4700, int
abst_arg: p21_4701, bool
abst_arg: p22_4702, int
abstract_term: (if p11_4699
                 (l1
                   (xsxs_1042 0 0
                     (fun (p11_4807:bool) (p12_4808:int) (p21_4809:bool) (p22_4810:int) ->
                      (insertsort_2165
                        (fun (x1_4989:int) (x2_4990:int) (k_insertsort_xs'xs'_4991:(bool -> int -> bool -> int -> X))
                         ->
                         (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1)
                           (fun (x__5135:bool) (x__5136:int) (x__5137:bool) (x__5138:int) ->
                            (k_insertsort_xs'xs'_4991 x__5135 x__5136 x__5137 x__5138))))
                        (fun (x_4734:(x_1:int ->
                                      x_2:int ->
                                      (x_4:bool ->
                                       x_5:int ->
                                       x_6:bool ->
                                       x_7:int[(not x_4) || (not x_6) || (
                                               not (x_1 <= x_2)) || x_1 < 0 || 
                                               x_2 < 0 || x_5 <= x_7]
                                       -> X)
                                      -> X))
                         ->
                         (insert_1014 p12_4808
                           (fun (x__5128:int) (x__5129:int) 
                                (x__5130:(x_1:bool ->
                                          x_2:int ->
                                          x_3:bool ->
                                          x_4:int[(not x_1) || (not x_3) || (
                                                  not (x__5128 <= x__5129)) || 
                                                  x__5128 < 0 || x__5129 < 0 || 
                                                  x_2 <= x_4]
                                          -> X))
                            ->
                            (x_4734 x__5128 x__5129
                              (fun (x__5131:bool) (x__5132:int) (x__5133:bool) 
                                   (x__5134:x_1:int[(not x__5131) || (
                                                    not x__5133) || (
                                                    not (x__5128 <= x__5129)) || 
                                                    x__5128 < 0 || x__5129 < 0 || 
                                                    x__5132 <= x_1])
                               -> (x__5130 x__5131 x__5132 x__5133 x__5134))))
                           (fun (x__5120:(x_1:int ->
                                          x_2:int ->
                                          (x_4:bool ->
                                           x_5:int ->
                                           x_6:bool ->
                                           x_7:int[(not x_4) || (not x_6) || (
                                                   not (x_1 <= x_2)) || 
                                                   x_1 < 0 || x_2 < 0 || 
                                                   x_5 <= x_7]
                                           -> X)
                                          -> X))
                            ->
                            (k_insertsort_2811
                              (fun (x__5121:int) (x__5122:int) 
                                   (x__5123:(x_1:bool ->
                                             x_2:int ->
                                             x_3:bool ->
                                             x_4:int[(not x_1) || (not x_3) || (
                                                     not (x__5121 <= x__5122)) || 
                                                     x__5121 < 0 || x__5122 < 0 || 
                                                     x_2 <= x_4]
                                             -> X))
                               ->
                               (x__5120 x__5121 x__5122
                                 (fun (x__5124:bool) (x__5125:int) (x__5126:bool) 
                                      (x__5127:x_1:int[(not x__5124) || (
                                                       not x__5126) || (
                                                       not (x__5121 <= x__5122)) || 
                                                       x__5121 < 0 || 
                                                       x__5122 < 0 || 
                                                       x__5125 <= x_1])
                                  -> (x__5123 x__5124 x__5125 x__5126 x__5127))))))))))))
                 (l0
                   (k_insertsort_2811
                     (fun (i1_4970:int) (i2_4971:int) 
                          (k_insertsort_rsrs_4972:(x_1:bool ->
                                                   x_2:int ->
                                                   x_3:bool ->
                                                   x_4:int[(not x_1) || (
                                                           not x_3) || (
                                                           not (i1_4970 <= i2_4971)) || 
                                                           i1_4970 < 0 || 
                                                           i2_4971 < 0 || 
                                                           x_2 <= x_4]
                                                   -> X))
                      -> (k_insertsort_rsrs_4972 false 0 false 0))))): X
abstract_term: p11_4699: x_1:bool[x_1]
cond: true
pbs: 
p:p11_4699
tt:false
ff:false

abstract_term: (l1
                 (xsxs_1042 0 0
                   (fun (p11_4807:bool) (p12_4808:int) (p21_4809:bool) (p22_4810:int) ->
                    (insertsort_2165
                      (fun (x1_4989:int) (x2_4990:int) (k_insertsort_xs'xs'_4991:(bool -> int -> bool -> int -> X)) ->
                       (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1)
                         (fun (x__5135:bool) (x__5136:int) (x__5137:bool) (x__5138:int) ->
                          (k_insertsort_xs'xs'_4991 x__5135 x__5136 x__5137 x__5138))))
                      (fun (x_4734:(x_1:int ->
                                    x_2:int ->
                                    (x_4:bool ->
                                     x_5:int ->
                                     x_6:bool ->
                                     x_7:int[(not x_4) || (not x_6) || (
                                             not (x_1 <= x_2)) || x_1 < 0 || 
                                             x_2 < 0 || x_5 <= x_7]
                                     -> X)
                                    -> X))
                       ->
                       (insert_1014 p12_4808
                         (fun (x__5128:int) (x__5129:int) 
                              (x__5130:(x_1:bool ->
                                        x_2:int ->
                                        x_3:bool ->
                                        x_4:int[(not x_1) || (not x_3) || (
                                                not (x__5128 <= x__5129)) || 
                                                x__5128 < 0 || x__5129 < 0 || 
                                                x_2 <= x_4]
                                        -> X))
                          ->
                          (x_4734 x__5128 x__5129
                            (fun (x__5131:bool) (x__5132:int) (x__5133:bool) 
                                 (x__5134:x_1:int[(not x__5131) || (not x__5133) || (
                                                  not (x__5128 <= x__5129)) || 
                                                  x__5128 < 0 || x__5129 < 0 || 
                                                  x__5132 <= x_1])
                             -> (x__5130 x__5131 x__5132 x__5133 x__5134))))
                         (fun (x__5120:(x_1:int ->
                                        x_2:int ->
                                        (x_4:bool ->
                                         x_5:int ->
                                         x_6:bool ->
                                         x_7:int[(not x_4) || (not x_6) || (
                                                 not (x_1 <= x_2)) || 
                                                 x_1 < 0 || x_2 < 0 || 
                                                 x_5 <= x_7]
                                         -> X)
                                        -> X))
                          ->
                          (k_insertsort_2811
                            (fun (x__5121:int) (x__5122:int) 
                                 (x__5123:(x_1:bool ->
                                           x_2:int ->
                                           x_3:bool ->
                                           x_4:int[(not x_1) || (not x_3) || (
                                                   not (x__5121 <= x__5122)) || 
                                                   x__5121 < 0 || x__5122 < 0 || 
                                                   x_2 <= x_4]
                                           -> X))
                             ->
                             (x__5120 x__5121 x__5122
                               (fun (x__5124:bool) (x__5125:int) (x__5126:bool) 
                                    (x__5127:x_1:int[(not x__5124) || (
                                                     not x__5126) || (
                                                     not (x__5121 <= x__5122)) || 
                                                     x__5121 < 0 || x__5122 < 0 || 
                                                     x__5125 <= x_1])
                                -> (x__5123 x__5124 x__5125 x__5126 x__5127)))))))))))): X
abstract_term: (xsxs_1042 0 0
                 (fun (p11_4807:bool) (p12_4808:int) (p21_4809:bool) (p22_4810:int) ->
                  (insertsort_2165
                    (fun (x1_4989:int) (x2_4990:int) (k_insertsort_xs'xs'_4991:(bool -> int -> bool -> int -> X)) ->
                     (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1)
                       (fun (x__5135:bool) (x__5136:int) (x__5137:bool) (x__5138:int) ->
                        (k_insertsort_xs'xs'_4991 x__5135 x__5136 x__5137 x__5138))))
                    (fun (x_4734:(x_1:int ->
                                  x_2:int ->
                                  (x_4:bool ->
                                   x_5:int ->
                                   x_6:bool ->
                                   x_7:int[(not x_4) || (not x_6) || (
                                           not (x_1 <= x_2)) || x_1 < 0 || 
                                           x_2 < 0 || x_5 <= x_7]
                                   -> X)
                                  -> X))
                     ->
                     (insert_1014 p12_4808
                       (fun (x__5128:int) (x__5129:int) 
                            (x__5130:(x_1:bool ->
                                      x_2:int ->
                                      x_3:bool ->
                                      x_4:int[(not x_1) || (not x_3) || (
                                              not (x__5128 <= x__5129)) || 
                                              x__5128 < 0 || x__5129 < 0 || 
                                              x_2 <= x_4]
                                      -> X))
                        ->
                        (x_4734 x__5128 x__5129
                          (fun (x__5131:bool) (x__5132:int) (x__5133:bool) 
                               (x__5134:x_1:int[(not x__5131) || (not x__5133) || (
                                                not (x__5128 <= x__5129)) || 
                                                x__5128 < 0 || x__5129 < 0 || 
                                                x__5132 <= x_1])
                           -> (x__5130 x__5131 x__5132 x__5133 x__5134))))
                       (fun (x__5120:(x_1:int ->
                                      x_2:int ->
                                      (x_4:bool ->
                                       x_5:int ->
                                       x_6:bool ->
                                       x_7:int[(not x_4) || (not x_6) || (
                                               not (x_1 <= x_2)) || x_1 < 0 || 
                                               x_2 < 0 || x_5 <= x_7]
                                       -> X)
                                      -> X))
                        ->
                        (k_insertsort_2811
                          (fun (x__5121:int) (x__5122:int) 
                               (x__5123:(x_1:bool ->
                                         x_2:int ->
                                         x_3:bool ->
                                         x_4:int[(not x_1) || (not x_3) || (
                                                 not (x__5121 <= x__5122)) || 
                                                 x__5121 < 0 || x__5122 < 0 || 
                                                 x_2 <= x_4]
                                         -> X))
                           ->
                           (x__5120 x__5121 x__5122
                             (fun (x__5124:bool) (x__5125:int) (x__5126:bool) 
                                  (x__5127:x_1:int[(not x__5124) || (
                                                   not x__5126) || (not (x__5121 <= x__5122)) || 
                                                   x__5121 < 0 || x__5122 < 0 || 
                                                   x__5125 <= x_1])
                              -> (x__5123 x__5124 x__5125 x__5126 x__5127))))))))))): X
abstract_term: (fun (p11_4807:bool) (p12_4808:int) (p21_4809:bool) (p22_4810:int) ->
                (insertsort_2165
                  (fun (x1_4989:int) (x2_4990:int) (k_insertsort_xs'xs'_4991:(bool -> int -> bool -> int -> X)) ->
                   (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1)
                     (fun (x__5135:bool) (x__5136:int) (x__5137:bool) (x__5138:int) ->
                      (k_insertsort_xs'xs'_4991 x__5135 x__5136 x__5137 x__5138))))
                  (fun (x_4734:(x_1:int ->
                                x_2:int ->
                                (x_4:bool ->
                                 x_5:int ->
                                 x_6:bool ->
                                 x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || 
                                         x_1 < 0 || x_2 < 0 || x_5 <= x_7]
                                 -> X)
                                -> X))
                   ->
                   (insert_1014 p12_4808
                     (fun (x__5128:int) (x__5129:int) 
                          (x__5130:(x_1:bool ->
                                    x_2:int ->
                                    x_3:bool ->
                                    x_4:int[(not x_1) || (not x_3) || (
                                            not (x__5128 <= x__5129)) || 
                                            x__5128 < 0 || x__5129 < 0 || 
                                            x_2 <= x_4]
                                    -> X))
                      ->
                      (x_4734 x__5128 x__5129
                        (fun (x__5131:bool) (x__5132:int) (x__5133:bool) 
                             (x__5134:x_1:int[(not x__5131) || (not x__5133) || (
                                              not (x__5128 <= x__5129)) || 
                                              x__5128 < 0 || x__5129 < 0 || 
                                              x__5132 <= x_1])
                         -> (x__5130 x__5131 x__5132 x__5133 x__5134))))
                     (fun (x__5120:(x_1:int ->
                                    x_2:int ->
                                    (x_4:bool ->
                                     x_5:int ->
                                     x_6:bool ->
                                     x_7:int[(not x_4) || (not x_6) || (
                                             not (x_1 <= x_2)) || x_1 < 0 || 
                                             x_2 < 0 || x_5 <= x_7]
                                     -> X)
                                    -> X))
                      ->
                      (k_insertsort_2811
                        (fun (x__5121:int) (x__5122:int) 
                             (x__5123:(x_1:bool ->
                                       x_2:int ->
                                       x_3:bool ->
                                       x_4:int[(not x_1) || (not x_3) || (
                                               not (x__5121 <= x__5122)) || 
                                               x__5121 < 0 || x__5122 < 0 || 
                                               x_2 <= x_4]
                                       -> X))
                         ->
                         (x__5120 x__5121 x__5122
                           (fun (x__5124:bool) (x__5125:int) (x__5126:bool) 
                                (x__5127:x_1:int[(not x__5124) || (not x__5126) || (
                                                 not (x__5121 <= x__5122)) || 
                                                 x__5121 < 0 || x__5122 < 0 || 
                                                 x__5125 <= x_1])
                            -> (x__5123 x__5124 x__5125 x__5126 x__5127)))))))))): (
bool -> int -> bool -> int -> X)
abst_arg: p11_4807, bool
abst_arg: p12_4808, int
abst_arg: p21_4809, bool
abst_arg: p22_4810, int
abst_arg: p11_4807, bool
abst_arg: p12_4808, int
abst_arg: p21_4809, bool
abst_arg: p22_4810, int
abstract_term: (insertsort_2165
                 (fun (x1_4989:int) (x2_4990:int) (k_insertsort_xs'xs'_4991:(bool -> int -> bool -> int -> X)) ->
                  (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1)
                    (fun (x__5135:bool) (x__5136:int) (x__5137:bool) (x__5138:int) ->
                     (k_insertsort_xs'xs'_4991 x__5135 x__5136 x__5137 x__5138))))
                 (fun (x_4734:(x_1:int ->
                               x_2:int ->
                               (x_4:bool ->
                                x_5:int ->
                                x_6:bool ->
                                x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || 
                                        x_1 < 0 || x_2 < 0 || x_5 <= x_7]
                                -> X)
                               -> X))
                  ->
                  (insert_1014 p12_4808
                    (fun (x__5128:int) (x__5129:int) 
                         (x__5130:(x_1:bool ->
                                   x_2:int ->
                                   x_3:bool ->
                                   x_4:int[(not x_1) || (not x_3) || (
                                           not (x__5128 <= x__5129)) || 
                                           x__5128 < 0 || x__5129 < 0 || 
                                           x_2 <= x_4]
                                   -> X))
                     ->
                     (x_4734 x__5128 x__5129
                       (fun (x__5131:bool) (x__5132:int) (x__5133:bool) 
                            (x__5134:x_1:int[(not x__5131) || (not x__5133) || (
                                             not (x__5128 <= x__5129)) || 
                                             x__5128 < 0 || x__5129 < 0 || 
                                             x__5132 <= x_1])
                        -> (x__5130 x__5131 x__5132 x__5133 x__5134))))
                    (fun (x__5120:(x_1:int ->
                                   x_2:int ->
                                   (x_4:bool ->
                                    x_5:int ->
                                    x_6:bool ->
                                    x_7:int[(not x_4) || (not x_6) || (
                                            not (x_1 <= x_2)) || x_1 < 0 || 
                                            x_2 < 0 || x_5 <= x_7]
                                    -> X)
                                   -> X))
                     ->
                     (k_insertsort_2811
                       (fun (x__5121:int) (x__5122:int) 
                            (x__5123:(x_1:bool ->
                                      x_2:int ->
                                      x_3:bool ->
                                      x_4:int[(not x_1) || (not x_3) || (
                                              not (x__5121 <= x__5122)) || 
                                              x__5121 < 0 || x__5122 < 0 || 
                                              x_2 <= x_4]
                                      -> X))
                        ->
                        (x__5120 x__5121 x__5122
                          (fun (x__5124:bool) (x__5125:int) (x__5126:bool) 
                               (x__5127:x_1:int[(not x__5124) || (not x__5126) || (
                                                not (x__5121 <= x__5122)) || 
                                                x__5121 < 0 || x__5122 < 0 || 
                                                x__5125 <= x_1])
                           -> (x__5123 x__5124 x__5125 x__5126 x__5127))))))))): X
abstract_term: (fun (x_4734:(x_1:int ->
                             x_2:int ->
                             (x_4:bool ->
                              x_5:int ->
                              x_6:bool ->
                              x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7]
                              -> X)
                             -> X))
                ->
                (insert_1014 p12_4808
                  (fun (x__5128:int) (x__5129:int) 
                       (x__5130:(x_1:bool ->
                                 x_2:int ->
                                 x_3:bool ->
                                 x_4:int[(not x_1) || (not x_3) || (not (x__5128 <= x__5129)) || 
                                         x__5128 < 0 || x__5129 < 0 || 
                                         x_2 <= x_4]
                                 -> X))
                   ->
                   (x_4734 x__5128 x__5129
                     (fun (x__5131:bool) (x__5132:int) (x__5133:bool) 
                          (x__5134:x_1:int[(not x__5131) || (not x__5133) || (
                                           not (x__5128 <= x__5129)) || 
                                           x__5128 < 0 || x__5129 < 0 || 
                                           x__5132 <= x_1])
                      -> (x__5130 x__5131 x__5132 x__5133 x__5134))))
                  (fun (x__5120:(x_1:int ->
                                 x_2:int ->
                                 (x_4:bool ->
                                  x_5:int ->
                                  x_6:bool ->
                                  x_7:int[(not x_4) || (not x_6) || (
                                          not (x_1 <= x_2)) || x_1 < 0 || 
                                          x_2 < 0 || x_5 <= x_7]
                                  -> X)
                                 -> X))
                   ->
                   (k_insertsort_2811
                     (fun (x__5121:int) (x__5122:int) 
                          (x__5123:(x_1:bool ->
                                    x_2:int ->
                                    x_3:bool ->
                                    x_4:int[(not x_1) || (not x_3) || (
                                            not (x__5121 <= x__5122)) || 
                                            x__5121 < 0 || x__5122 < 0 || 
                                            x_2 <= x_4]
                                    -> X))
                      ->
                      (x__5120 x__5121 x__5122
                        (fun (x__5124:bool) (x__5125:int) (x__5126:bool) 
                             (x__5127:x_1:int[(not x__5124) || (not x__5126) || (
                                              not (x__5121 <= x__5122)) || 
                                              x__5121 < 0 || x__5122 < 0 || 
                                              x__5125 <= x_1])
                         -> (x__5123 x__5124 x__5125 x__5126 x__5127)))))))): ((
x_2:int ->
x_3:int ->
(x_5:bool ->
 x_6:int -> x_7:bool -> x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_2 < 0 || x_3 < 0 || x_6 <= x_8] -> X)
-> X) ->
X)
abst_arg: x_4734, (x_1:int ->
                   x_2:int ->
                   (x_4:bool ->
                    x_5:int ->
                    x_6:bool ->
                    x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
                   -> X)
abst_arg: x_4734, (x_1:int ->
                   x_2:int ->
                   (x_4:bool ->
                    x_5:int ->
                    x_6:bool ->
                    x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
                   -> X)
abstract_term: (insert_1014 p12_4808
                 (fun (x__5128:int) (x__5129:int) 
                      (x__5130:(x_1:bool ->
                                x_2:int ->
                                x_3:bool ->
                                x_4:int[(not x_1) || (not x_3) || (not (x__5128 <= x__5129)) || 
                                        x__5128 < 0 || x__5129 < 0 || 
                                        x_2 <= x_4]
                                -> X))
                  ->
                  (x_4734 x__5128 x__5129
                    (fun (x__5131:bool) (x__5132:int) (x__5133:bool) 
                         (x__5134:x_1:int[(not x__5131) || (not x__5133) || (
                                          not (x__5128 <= x__5129)) || 
                                          x__5128 < 0 || x__5129 < 0 || 
                                          x__5132 <= x_1])
                     -> (x__5130 x__5131 x__5132 x__5133 x__5134))))
                 (fun (x__5120:(x_1:int ->
                                x_2:int ->
                                (x_4:bool ->
                                 x_5:int ->
                                 x_6:bool ->
                                 x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || 
                                         x_1 < 0 || x_2 < 0 || x_5 <= x_7]
                                 -> X)
                                -> X))
                  ->
                  (k_insertsort_2811
                    (fun (x__5121:int) (x__5122:int) 
                         (x__5123:(x_1:bool ->
                                   x_2:int ->
                                   x_3:bool ->
                                   x_4:int[(not x_1) || (not x_3) || (
                                           not (x__5121 <= x__5122)) || 
                                           x__5121 < 0 || x__5122 < 0 || 
                                           x_2 <= x_4]
                                   -> X))
                     ->
                     (x__5120 x__5121 x__5122
                       (fun (x__5124:bool) (x__5125:int) (x__5126:bool) 
                            (x__5127:x_1:int[(not x__5124) || (not x__5126) || (
                                             not (x__5121 <= x__5122)) || 
                                             x__5121 < 0 || x__5122 < 0 || 
                                             x__5125 <= x_1])
                        -> (x__5123 x__5124 x__5125 x__5126 x__5127))))))): X
abstract_term: (fun (x__5120:(x_1:int ->
                              x_2:int ->
                              (x_4:bool ->
                               x_5:int ->
                               x_6:bool ->
                               x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7]
                               -> X)
                              -> X))
                ->
                (k_insertsort_2811
                  (fun (x__5121:int) (x__5122:int) 
                       (x__5123:(x_1:bool ->
                                 x_2:int ->
                                 x_3:bool ->
                                 x_4:int[(not x_1) || (not x_3) || (not (x__5121 <= x__5122)) || 
                                         x__5121 < 0 || x__5122 < 0 || 
                                         x_2 <= x_4]
                                 -> X))
                   ->
                   (x__5120 x__5121 x__5122
                     (fun (x__5124:bool) (x__5125:int) (x__5126:bool) 
                          (x__5127:x_1:int[(not x__5124) || (not x__5126) || (
                                           not (x__5121 <= x__5122)) || 
                                           x__5121 < 0 || x__5122 < 0 || 
                                           x__5125 <= x_1])
                      -> (x__5123 x__5124 x__5125 x__5126 x__5127)))))): ((
x_2:int ->
x_3:int ->
(x_5:bool ->
 x_6:int -> x_7:bool -> x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_2 < 0 || x_3 < 0 || x_6 <= x_8] -> X)
-> X) ->
X)
abst_arg: x__5120, (x_1:int ->
                    x_2:int ->
                    (x_4:bool ->
                     x_5:int ->
                     x_6:bool ->
                     x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
                    -> X)
abst_arg: x__5120, (x_1:int ->
                    x_2:int ->
                    (x_4:bool ->
                     x_5:int ->
                     x_6:bool ->
                     x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
                    -> X)
abstract_term: (k_insertsort_2811
                 (fun (x__5121:int) (x__5122:int) 
                      (x__5123:(x_1:bool ->
                                x_2:int ->
                                x_3:bool ->
                                x_4:int[(not x_1) || (not x_3) || (not (x__5121 <= x__5122)) || 
                                        x__5121 < 0 || x__5122 < 0 || 
                                        x_2 <= x_4]
                                -> X))
                  ->
                  (x__5120 x__5121 x__5122
                    (fun (x__5124:bool) (x__5125:int) (x__5126:bool) 
                         (x__5127:x_1:int[(not x__5124) || (not x__5126) || (
                                          not (x__5121 <= x__5122)) || 
                                          x__5121 < 0 || x__5122 < 0 || 
                                          x__5125 <= x_1])
                     -> (x__5123 x__5124 x__5125 x__5126 x__5127))))): X
abstract_term: (fun (x__5121:int) (x__5122:int) 
                    (x__5123:(x_1:bool ->
                              x_2:int ->
                              x_3:bool ->
                              x_4:int[(not x_1) || (not x_3) || (not (x__5121 <= x__5122)) || 
                                      x__5121 < 0 || x__5122 < 0 || x_2 <= x_4]
                              -> X))
                ->
                (x__5120 x__5121 x__5122
                  (fun (x__5124:bool) (x__5125:int) (x__5126:bool) 
                       (x__5127:x_1:int[(not x__5124) || (not x__5126) || (
                                        not (x__5121 <= x__5122)) || 
                                        x__5121 < 0 || x__5122 < 0 || 
                                        x__5125 <= x_1])
                   -> (x__5123 x__5124 x__5125 x__5126 x__5127)))): (
x_1:int ->
x_2:int ->
(x_4:bool ->
 x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
-> X)
abst_arg: x__5121, int
abst_arg: x__5122, int
abst_arg: x__5123, (x_1:bool ->
                    x_2:int ->
                    x_3:bool ->
                    x_4:int[(not x_1) || (not x_3) || (not (x__5121 <= x__5122)) || 
                            x__5121 < 0 || x__5122 < 0 || x_2 <= x_4]
                    -> X)
abst_arg: x__5121, int
abst_arg: x__5122, int
abst_arg: x__5123, (x_1:bool ->
                    x_2:int ->
                    x_3:bool ->
                    x_4:int[(not x_1) || (not x_3) || (not (x__5121 <= x__5122)) || 
                            x__5121 < 0 || x__5122 < 0 || x_2 <= x_4]
                    -> X)
abstract_term: (x__5120 x__5121 x__5122
                 (fun (x__5124:bool) (x__5125:int) (x__5126:bool) 
                      (x__5127:x_1:int[(not x__5124) || (not x__5126) || (
                                       not (x__5121 <= x__5122)) || x__5121 < 0 || 
                                       x__5122 < 0 || x__5125 <= x_1])
                  -> (x__5123 x__5124 x__5125 x__5126 x__5127))): X
abstract_term: (fun (x__5124:bool) (x__5125:int) (x__5126:bool) 
                    (x__5127:x_1:int[(not x__5124) || (not x__5126) || (
                                     not (x__5121 <= x__5122)) || x__5121 < 0 || 
                                     x__5122 < 0 || x__5125 <= x_1])
                -> (x__5123 x__5124 x__5125 x__5126 x__5127)): (x_1:bool ->
                                                                x_2:int ->
                                                                x_3:bool ->
                                                                x_4:int[
                                                                (not x_1) || (
                                                                not x_3) || (
                                                                not (x__5121 <= x__5122)) || 
                                                                x__5121 < 0 || 
                                                                x__5122 < 0 || 
                                                                x_2 <= x_4] -> X)
abst_arg: x__5124, bool
abst_arg: x__5125, int
abst_arg: x__5126, bool
abst_arg: x__5127, x_1:int[(not x__5124) || (not x__5126) || (not (x__5121 <= x__5122)) || 
                           x__5121 < 0 || x__5122 < 0 || x__5125 <= x_1]
abst_arg: x__5124, bool
abst_arg: x__5125, int
abst_arg: x__5126, bool
abst_arg: x__5127, x_1:int[(not x__5124) || (not x__5126) || (not (x__5121 <= x__5122)) || 
                           x__5121 < 0 || x__5122 < 0 || x__5125 <= x_1]
abstract_term: (x__5123 x__5124 x__5125 x__5126 x__5127): X
abstract_term: x__5127: x_1:int[(not x__5124) || (not x__5126) || (not (x__5121 <= x__5122)) || 
                                x__5121 < 0 || x__5122 < 0 || x__5125 <= x_1]
cond: p11_4699; true
pbs: x__5127 := ((((((not x__5124) || (not x__5126)) || (not (x__5121 <= x__5122))) || (x__5121 < 0)) || (x__5122 < 0))
                 || (x__5125 <= x__5127))
p:((((((not x__5124) || (not x__5126)) || (not (x__5121 <= x__5122))) || (x__5121 < 0)) || (x__5122 < 0)) ||
   (x__5125 <= x__5127))
tt:x__5127
ff:false

abstract_term: x__5126: bool
abstract_term: x__5125: int
abstract_term: x__5124: bool
abstract_term: x__5122: int
abstract_term: x__5121: int
abstract_term: (fun (x__5128:int) (x__5129:int) 
                    (x__5130:(x_1:bool ->
                              x_2:int ->
                              x_3:bool ->
                              x_4:int[(not x_1) || (not x_3) || (not (x__5128 <= x__5129)) || 
                                      x__5128 < 0 || x__5129 < 0 || x_2 <= x_4]
                              -> X))
                ->
                (x_4734 x__5128 x__5129
                  (fun (x__5131:bool) (x__5132:int) (x__5133:bool) 
                       (x__5134:x_1:int[(not x__5131) || (not x__5133) || (
                                        not (x__5128 <= x__5129)) || 
                                        x__5128 < 0 || x__5129 < 0 || 
                                        x__5132 <= x_1])
                   -> (x__5130 x__5131 x__5132 x__5133 x__5134)))): (
x_1:int ->
x_2:int ->
(x_4:bool ->
 x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_1 < 0 || x_2 < 0 || x_5 <= x_7] -> X)
-> X)
abst_arg: x__5128, int
abst_arg: x__5129, int
abst_arg: x__5130, (x_1:bool ->
                    x_2:int ->
                    x_3:bool ->
                    x_4:int[(not x_1) || (not x_3) || (not (x__5128 <= x__5129)) || 
                            x__5128 < 0 || x__5129 < 0 || x_2 <= x_4]
                    -> X)
abst_arg: x__5128, int
abst_arg: x__5129, int
abst_arg: x__5130, (x_1:bool ->
                    x_2:int ->
                    x_3:bool ->
                    x_4:int[(not x_1) || (not x_3) || (not (x__5128 <= x__5129)) || 
                            x__5128 < 0 || x__5129 < 0 || x_2 <= x_4]
                    -> X)
abstract_term: (x_4734 x__5128 x__5129
                 (fun (x__5131:bool) (x__5132:int) (x__5133:bool) 
                      (x__5134:x_1:int[(not x__5131) || (not x__5133) || (
                                       not (x__5128 <= x__5129)) || x__5128 < 0 || 
                                       x__5129 < 0 || x__5132 <= x_1])
                  -> (x__5130 x__5131 x__5132 x__5133 x__5134))): X
abstract_term: (fun (x__5131:bool) (x__5132:int) (x__5133:bool) 
                    (x__5134:x_1:int[(not x__5131) || (not x__5133) || (
                                     not (x__5128 <= x__5129)) || x__5128 < 0 || 
                                     x__5129 < 0 || x__5132 <= x_1])
                -> (x__5130 x__5131 x__5132 x__5133 x__5134)): (x_1:bool ->
                                                                x_2:int ->
                                                                x_3:bool ->
                                                                x_4:int[
                                                                (not x_1) || (
                                                                not x_3) || (
                                                                not (x__5128 <= x__5129)) || 
                                                                x__5128 < 0 || 
                                                                x__5129 < 0 || 
                                                                x_2 <= x_4] -> X)
abst_arg: x__5131, bool
abst_arg: x__5132, int
abst_arg: x__5133, bool
abst_arg: x__5134, x_1:int[(not x__5131) || (not x__5133) || (not (x__5128 <= x__5129)) || 
                           x__5128 < 0 || x__5129 < 0 || x__5132 <= x_1]
abst_arg: x__5131, bool
abst_arg: x__5132, int
abst_arg: x__5133, bool
abst_arg: x__5134, x_1:int[(not x__5131) || (not x__5133) || (not (x__5128 <= x__5129)) || 
                           x__5128 < 0 || x__5129 < 0 || x__5132 <= x_1]
abstract_term: (x__5130 x__5131 x__5132 x__5133 x__5134): X
abstract_term: x__5134: x_1:int[(not x__5131) || (not x__5133) || (not (x__5128 <= x__5129)) || 
                                x__5128 < 0 || x__5129 < 0 || x__5132 <= x_1]
cond: p11_4699; true
pbs: x__5134 := ((((((not x__5131) || (not x__5133)) || (not (x__5128 <= x__5129))) || (x__5128 < 0)) || (x__5129 < 0))
                 || (x__5132 <= x__5134))
p:((((((not x__5131) || (not x__5133)) || (not (x__5128 <= x__5129))) || (x__5128 < 0)) || (x__5129 < 0)) ||
   (x__5132 <= x__5134))
tt:x__5134
ff:false

abstract_term: x__5133: bool
abstract_term: x__5132: int
abstract_term: x__5131: bool
abstract_term: x__5129: int
abstract_term: x__5128: int
abstract_term: p12_4808: int
abstract_term: (fun (x1_4989:int) (x2_4990:int) (k_insertsort_xs'xs'_4991:(bool -> int -> bool -> int -> X)) ->
                (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1)
                  (fun (x__5135:bool) (x__5136:int) (x__5137:bool) (x__5138:int) ->
                   (k_insertsort_xs'xs'_4991 x__5135 x__5136 x__5137 x__5138)))): (
int -> int -> (bool -> int -> bool -> int -> X) -> X)
abst_arg: x1_4989, int
abst_arg: x2_4990, int
abst_arg: k_insertsort_xs'xs'_4991, (bool -> int -> bool -> int -> X)
abst_arg: x1_4989, int
abst_arg: x2_4990, int
abst_arg: k_insertsort_xs'xs'_4991, (bool -> int -> bool -> int -> X)
abstract_term: (xsxs_1042 (x1_4989 + 1) (x2_4990 + 1)
                 (fun (x__5135:bool) (x__5136:int) (x__5137:bool) (x__5138:int) ->
                  (k_insertsort_xs'xs'_4991 x__5135 x__5136 x__5137 x__5138))): X
abstract_term: (fun (x__5135:bool) (x__5136:int) (x__5137:bool) (x__5138:int) ->
                (k_insertsort_xs'xs'_4991 x__5135 x__5136 x__5137 x__5138)): (
bool -> int -> bool -> int -> X)
abst_arg: x__5135, bool
abst_arg: x__5136, int
abst_arg: x__5137, bool
abst_arg: x__5138, int
abst_arg: x__5135, bool
abst_arg: x__5136, int
abst_arg: x__5137, bool
abst_arg: x__5138, int
abstract_term: (k_insertsort_xs'xs'_4991 x__5135 x__5136 x__5137 x__5138): X
abstract_term: x__5138: int
abstract_term: x__5137: bool
abstract_term: x__5136: int
abstract_term: x__5135: bool
abstract_term: (x2_4990 + 1): int
abstract_term: (x1_4989 + 1): int
abstract_term: 0: int
abstract_term: 0: int
abstract_term: (l0
                 (k_insertsort_2811
                   (fun (i1_4970:int) (i2_4971:int) 
                        (k_insertsort_rsrs_4972:(x_1:bool ->
                                                 x_2:int ->
                                                 x_3:bool ->
                                                 x_4:int[(not x_1) || (
                                                         not x_3) || (
                                                         not (i1_4970 <= i2_4971)) || 
                                                         i1_4970 < 0 || 
                                                         i2_4971 < 0 || 
                                                         x_2 <= x_4]
                                                 -> X))
                    -> (k_insertsort_rsrs_4972 false 0 false 0)))): X
abstract_term: (k_insertsort_2811
                 (fun (i1_4970:int) (i2_4971:int) 
                      (k_insertsort_rsrs_4972:(x_1:bool ->
                                               x_2:int ->
                                               x_3:bool ->
                                               x_4:int[(not x_1) || (
                                                       not x_3) || (not (i1_4970 <= i2_4971)) || 
                                                       i1_4970 < 0 || 
                                                       i2_4971 < 0 || 
                                                       x_2 <= x_4]
                                               -> X))
                  -> (k_insertsort_rsrs_4972 false 0 false 0))): X
abstract_term: (fun (i1_4970:int) (i2_4971:int) 
                    (k_insertsort_rsrs_4972:(x_1:bool ->
                                             x_2:int ->
                                             x_3:bool ->
                                             x_4:int[(not x_1) || (not x_3) || (
                                                     not (i1_4970 <= i2_4971)) || 
                                                     i1_4970 < 0 || i2_4971 < 0 || 
                                                     x_2 <= x_4]
                                             -> X))
                -> (k_insertsort_rsrs_4972 false 0 false 0)): (x_1:int ->
                                                               x_2:int ->
                                                               (x_4:bool ->
                                                                x_5:int ->
                                                                x_6:bool ->
                                                                x_7:int[
                                                                (not x_4) || (
                                                                not x_6) || (
                                                                not (x_1 <= x_2)) || 
                                                                x_1 < 0 || 
                                                                x_2 < 0 || 
                                                                x_5 <= x_7] -> X)
                                                               -> X)
abst_arg: i1_4970, int
abst_arg: i2_4971, int
abst_arg: k_insertsort_rsrs_4972, (x_1:bool ->
                                   x_2:int ->
                                   x_3:bool ->
                                   x_4:int[(not x_1) || (not x_3) || (
                                           not (i1_4970 <= i2_4971)) || 
                                           i1_4970 < 0 || i2_4971 < 0 || 
                                           x_2 <= x_4]
                                   -> X)
abst_arg: i1_4970, int
abst_arg: i2_4971, int
abst_arg: k_insertsort_rsrs_4972, (x_1:bool ->
                                   x_2:int ->
                                   x_3:bool ->
                                   x_4:int[(not x_1) || (not x_3) || (
                                           not (i1_4970 <= i2_4971)) || 
                                           i1_4970 < 0 || i2_4971 < 0 || 
                                           x_2 <= x_4]
                                   -> X)
abstract_term: (k_insertsort_rsrs_4972 false 0 false 0): X
abstract_term: 0: x_1:int[(not false) || (not false) || (not (i1_4970 <= i2_4971)) || 
                          i1_4970 < 0 || i2_4971 < 0 || 0 <= x_1]
cond: (not p11_4699); true
pbs: 
p:((((((not false) || (not false)) || (not (i1_4970 <= i2_4971))) || (i1_4970 < 0)) || (i2_4971 < 0)) || (0 <= 0))
tt:true
ff:false

abstract_term: false: bool
abstract_term: 0: int
abstract_term: false: bool
abstract_term: 0: int
abstract_term: 0: int
loop_2167: ENV: x_1028:unit, k_loop_2237:(bool -> int -> bool -> int -> X),


abst_arg: x_1028, unit
abst_arg: k_loop_2237, (bool -> int -> bool -> int -> X)
abst_arg: x_1028, unit
abst_arg: k_loop_2237, (bool -> int -> bool -> int -> X)
loop_2167: (loop_2167 ()
             (fun (x__5139:bool) (x__5140:int) (x__5141:bool) (x__5142:int) ->
              (k_loop_2237 x__5139 x__5140 x__5141 x__5142))) ===> (loop_2167 ()
                                                                    (fun 
                                                                    (x__5139:bool) (x__5140:int) (x__5141:bool) 
                                                                    (x__5142:int) ->
                                                                    (k_loop_2237 x__5139 x__5140 x__5141 x__5142)))
loop_2167:: (loop_2167 ()
              (fun (x__5139:bool) (x__5140:int) (x__5141:bool) (x__5142:int) ->
               (k_loop_2237 x__5139 x__5140 x__5141 x__5142)))
abstract_term: (loop_2167 ()
                 (fun (x__5139:bool) (x__5140:int) (x__5141:bool) (x__5142:int) ->
                  (k_loop_2237 x__5139 x__5140 x__5141 x__5142))): X
abstract_term: (fun (x__5139:bool) (x__5140:int) (x__5141:bool) (x__5142:int) ->
                (k_loop_2237 x__5139 x__5140 x__5141 x__5142)): (bool -> int -> bool -> int -> X)
abst_arg: x__5139, bool
abst_arg: x__5140, int
abst_arg: x__5141, bool
abst_arg: x__5142, int
abst_arg: x__5139, bool
abst_arg: x__5140, int
abst_arg: x__5141, bool
abst_arg: x__5142, int
abstract_term: (k_loop_2237 x__5139 x__5140 x__5141 x__5142): X
abstract_term: x__5142: int
abstract_term: x__5141: bool
abstract_term: x__5140: int
abstract_term: x__5139: bool
abstract_term: (): unit
make_list_1049: ENV: n_1050:int, k_make_list_2998:((int -> (bool -> int -> X) -> X) -> X),


abst_arg: n_1050, int
abst_arg: k_make_list_2998, ((int -> (bool -> int -> X) -> X) ->
X)
abst_arg: n_1050, int
abst_arg: k_make_list_2998, ((int -> (bool -> int -> X) -> X) ->
X)
make_list_1049: (l0
                  (k_make_list_2998
                    (fun (i_4750:int) (k_make_list_4751:(bool -> int -> X)) -> (k_make_list_4751 false 0)))) ===> (
l0 (k_make_list_2998 (fun (i_4750:int) (k_make_list_4751:(bool -> int -> X)) -> (k_make_list_4751 false 0))))
make_list_1049:: (l0
                   (k_make_list_2998
                     (fun (i_4750:int) (k_make_list_4751:(bool -> int -> X)) -> (k_make_list_4751 false 0))))
abstract_term: (l0
                 (k_make_list_2998
                   (fun (i_4750:int) (k_make_list_4751:(bool -> int -> X)) -> (k_make_list_4751 false 0)))): X
abstract_term: (k_make_list_2998 (fun (i_4750:int) (k_make_list_4751:(bool -> int -> X)) -> (k_make_list_4751 false 0))): X
abstract_term: (fun (i_4750:int) (k_make_list_4751:(bool -> int -> X)) -> (k_make_list_4751 false 0)): (
int -> (bool -> int -> X) -> X)
abst_arg: i_4750, int
abst_arg: k_make_list_4751, (bool -> int -> X)
abst_arg: i_4750, int
abst_arg: k_make_list_4751, (bool -> int -> X)
abstract_term: (k_make_list_4751 false 0): X
abstract_term: 0: int
abstract_term: false: bool
make_list_1049: ENV: n_1050:int, k_make_list_2998:((int -> (bool -> int -> X) -> X) -> X),


abst_arg: n_1050, int
abst_arg: k_make_list_2998, ((int -> (bool -> int -> X) -> X) ->
X)
abst_arg: n_1050, int
abst_arg: k_make_list_2998, ((int -> (bool -> int -> X) -> X) ->
X)
make_list_1049: (l1
                  (rand_int
                    (fun (n_4754:int) ->
                     (make_list_1049 (n_4754 - 1)
                       (fun (xs_4758:(int -> (bool -> int -> X) -> X)) ->
                        (k_make_list_2998
                          (fun (i_4767:int) (k_make_list_4768:(bool -> int -> X)) ->
                           (if (i_4767 = 0) (l0 (k_make_list_4768 true n_4754))
                             (l1
                               (xs_4758 (i_4767 - 1)
                                 (fun (x__5143:bool) (x__5144:int) -> (k_make_list_4768 x__5143 x__5144)))))))))))) ===> (
l1
 (rand_int
   (fun (n_4754:int) ->
    (make_list_1049 (n_4754 - 1)
      (fun (xs_4758:(int -> (bool -> int -> X) -> X)) ->
       (k_make_list_2998
         (fun (i_4767:int) (k_make_list_4768:(bool -> int -> X)) ->
          (if (i_4767 = 0) (l0 (k_make_list_4768 true n_4754))
            (l1 (xs_4758 (i_4767 - 1) (fun (x__5143:bool) (x__5144:int) -> (k_make_list_4768 x__5143 x__5144))))))))))))
make_list_1049:: (l1
                   (rand_int
                     (fun (n_4754:int) ->
                      (make_list_1049 (n_4754 - 1)
                        (fun (xs_4758:(int -> (bool -> int -> X) -> X)) ->
                         (k_make_list_2998
                           (fun (i_4767:int) (k_make_list_4768:(bool -> int -> X)) ->
                            (if (i_4767 = 0) (l0 (k_make_list_4768 true n_4754))
                              (l1
                                (xs_4758 (i_4767 - 1)
                                  (fun (x__5143:bool) (x__5144:int) -> (k_make_list_4768 x__5143 x__5144))))))))))))
abstract_term: (l1
                 (rand_int
                   (fun (n_4754:int) ->
                    (make_list_1049 (n_4754 - 1)
                      (fun (xs_4758:(int -> (bool -> int -> X) -> X)) ->
                       (k_make_list_2998
                         (fun (i_4767:int) (k_make_list_4768:(bool -> int -> X)) ->
                          (if (i_4767 = 0) (l0 (k_make_list_4768 true n_4754))
                            (l1
                              (xs_4758 (i_4767 - 1)
                                (fun (x__5143:bool) (x__5144:int) -> (k_make_list_4768 x__5143 x__5144)))))))))))): X
abstract_term: (rand_int
                 (fun (n_4754:int) ->
                  (make_list_1049 (n_4754 - 1)
                    (fun (xs_4758:(int -> (bool -> int -> X) -> X)) ->
                     (k_make_list_2998
                       (fun (i_4767:int) (k_make_list_4768:(bool -> int -> X)) ->
                        (if (i_4767 = 0) (l0 (k_make_list_4768 true n_4754))
                          (l1
                            (xs_4758 (i_4767 - 1)
                              (fun (x__5143:bool) (x__5144:int) -> (k_make_list_4768 x__5143 x__5144))))))))))): X
abstract_term: (fun (n_4754:int) ->
                (make_list_1049 (n_4754 - 1)
                  (fun (xs_4758:(int -> (bool -> int -> X) -> X)) ->
                   (k_make_list_2998
                     (fun (i_4767:int) (k_make_list_4768:(bool -> int -> X)) ->
                      (if (i_4767 = 0) (l0 (k_make_list_4768 true n_4754))
                        (l1
                          (xs_4758 (i_4767 - 1)
                            (fun (x__5143:bool) (x__5144:int) -> (k_make_list_4768 x__5143 x__5144)))))))))): (int ->
X)
abst_arg: n_4754, int
abst_arg: n_4754, int
abstract_term: (make_list_1049 (n_4754 - 1)
                 (fun (xs_4758:(int -> (bool -> int -> X) -> X)) ->
                  (k_make_list_2998
                    (fun (i_4767:int) (k_make_list_4768:(bool -> int -> X)) ->
                     (if (i_4767 = 0) (l0 (k_make_list_4768 true n_4754))
                       (l1
                         (xs_4758 (i_4767 - 1) (fun (x__5143:bool) (x__5144:int) -> (k_make_list_4768 x__5143 x__5144))))))))): X
abstract_term: (fun (xs_4758:(int -> (bool -> int -> X) -> X)) ->
                (k_make_list_2998
                  (fun (i_4767:int) (k_make_list_4768:(bool -> int -> X)) ->
                   (if (i_4767 = 0) (l0 (k_make_list_4768 true n_4754))
                     (l1
                       (xs_4758 (i_4767 - 1) (fun (x__5143:bool) (x__5144:int) -> (k_make_list_4768 x__5143 x__5144)))))))): ((
int -> (bool -> int -> X) -> X) ->
X)
abst_arg: xs_4758, (int -> (bool -> int -> X) -> X)
abst_arg: xs_4758, (int -> (bool -> int -> X) -> X)
abstract_term: (k_make_list_2998
                 (fun (i_4767:int) (k_make_list_4768:(bool -> int -> X)) ->
                  (if (i_4767 = 0) (l0 (k_make_list_4768 true n_4754))
                    (l1 (xs_4758 (i_4767 - 1) (fun (x__5143:bool) (x__5144:int) -> (k_make_list_4768 x__5143 x__5144))))))): X
abstract_term: (fun (i_4767:int) (k_make_list_4768:(bool -> int -> X)) ->
                (if (i_4767 = 0) (l0 (k_make_list_4768 true n_4754))
                  (l1 (xs_4758 (i_4767 - 1) (fun (x__5143:bool) (x__5144:int) -> (k_make_list_4768 x__5143 x__5144)))))): (
int -> (bool -> int -> X) -> X)
abst_arg: i_4767, int
abst_arg: k_make_list_4768, (bool -> int -> X)
abst_arg: i_4767, int
abst_arg: k_make_list_4768, (bool -> int -> X)
abstract_term: (if (i_4767 = 0) (l0 (k_make_list_4768 true n_4754))
                 (l1 (xs_4758 (i_4767 - 1) (fun (x__5143:bool) (x__5144:int) -> (k_make_list_4768 x__5143 x__5144))))): X
abstract_term: (i_4767 = 0): x_1:bool[x_1]
cond: (not (n_1050 = 0))
pbs: 
p:(i_4767 = 0)
tt:false
ff:false

abstract_term: (l0 (k_make_list_4768 true n_4754)): X
abstract_term: (k_make_list_4768 true n_4754): X
abstract_term: n_4754: int
abstract_term: true: bool
abstract_term: (l1 (xs_4758 (i_4767 - 1) (fun (x__5143:bool) (x__5144:int) -> (k_make_list_4768 x__5143 x__5144)))): X
abstract_term: (xs_4758 (i_4767 - 1) (fun (x__5143:bool) (x__5144:int) -> (k_make_list_4768 x__5143 x__5144))): X
abstract_term: (fun (x__5143:bool) (x__5144:int) -> (k_make_list_4768 x__5143 x__5144)): (
bool -> int -> X)
abst_arg: x__5143, bool
abst_arg: x__5144, int
abst_arg: x__5143, bool
abst_arg: x__5144, int
abstract_term: (k_make_list_4768 x__5143 x__5144): X
abstract_term: x__5144: int
abstract_term: x__5143: bool
abstract_term: (i_4767 - 1): int
abstract_term: (n_4754 - 1): int
ABST:
Main: main_3652
  main_3652 ->
      (make_list_1049
        (fun xs_4737 ->
         (insertsort_2165
           (fun k_main_xsxs_5032 ->
            (if rand_bool (l0 (xs_4737 k_main_xsxs_5032)) (l1 (xs_4737 (xs_4737 k_main_xsxs_5032)))))
           (fun ysys_4740 ->
            (check_2164 (fun x__5050 -> (ysys_4740 (fun x__5054 -> (x__5050 (if x__5054 true rand_bool)))))
              (fun b_4746 -> (if (if b_4746 true rand_bool) (l0 ()) (l1 (if b_4746 _|_ (fail_3705 ()))))))))))
  check_2164 xsxs_1056 k_check_3093 ->
      (xsxs_1056
        (fun xs22_4590 ->
         (if rand_bool
           (l1
             (if rand_bool
               (l1
                 (if (if xs22_4590 true rand_bool)
                   (l0
                     (check_2164
                       (fun k_check_xs'xs'_4980 ->
                        (xsxs_1056 (fun x__5059 -> (k_check_xs'xs'_4980 (if x__5059 true rand_bool)))))
                       (fun x__5055 -> (k_check_3093 (if x__5055 true rand_bool)))))
                   (l1 (if xs22_4590 _|_ (k_check_3093 false))))) (l0 (k_check_3093 true)))) (
           l0 (k_check_3093 true)))))
  fail_3705 k -> {fail} => k
  insert_1014 ysys_1016 k_insert_2260 ->
      (ysys_1016
        (fun p22_4800 ->
         (if rand_bool
           (l1
             (if rand_bool
               (l0
                 (k_insert_2260
                   (fun k_insert_4614 ->
                    (if rand_bool
                      (l0
                        (if rand_bool (l0 (k_insert_4614 true))
                          (l1
                            (ysys_1016
                              (fun xs22_4634 ->
                               (if rand_bool
                                 (l1
                                   (if rand_bool
                                     (l0
                                       (if rand_bool
                                         (l1
                                           (if rand_bool
                                             (l1
                                               (if rand_bool (l1 (k_insert_4614 true))
                                                 (l0 (loop_2167 (k_insert_4614 rand_bool)))))
                                             (l0 (l0 (loop_2167 (k_insert_4614 rand_bool))))))
                                         (l0 (loop_2167 (k_insert_4614 rand_bool)))))
                                     (l1
                                       (l1
                                         (if rand_bool
                                           (l1
                                             (if rand_bool (l1 (k_insert_4614 (if xs22_4634 true rand_bool)))
                                               (l0 (loop_2167 (k_insert_4614 rand_bool)))))
                                           (l0 (l0 (loop_2167 (k_insert_4614 rand_bool)))))))))
                                 (l0 (loop_2167 (k_insert_4614 rand_bool)))))))))
                      (l1
                        (if rand_bool (l0 (ysys_1016 (fun p22_4644 -> (k_insert_4614 true))))
                          (l1 (ysys_1016 (fun x__5095 -> (k_insert_4614 (if x__5095 true rand_bool)))))))))))
               (l1
                 (insert_1014
                   (fun k_insert_ys'ys'_5044 ->
                    (ysys_1016 (fun x__5091 -> (k_insert_ys'ys'_5044 (if x__5091 true rand_bool)))))
                   (fun ys''ys''_4650 ->
                    (k_insert_2260
                      (fun k_insert_4666 ->
                       (if rand_bool
                         (l0
                           (if rand_bool (l0 (k_insert_4666 true))
                             (l1
                               (ysys_1016
                                 (fun xs22_4686 ->
                                  (if rand_bool
                                    (l1
                                      (if rand_bool
                                        (l0
                                          (if rand_bool
                                            (l1
                                              (if rand_bool
                                                (l1
                                                  (if rand_bool (l1 (k_insert_4666 true))
                                                    (l0 (loop_2167 (k_insert_4666 rand_bool)))))
                                                (l0 (l0 (loop_2167 (k_insert_4666 rand_bool))))))
                                            (l0 (loop_2167 (k_insert_4666 rand_bool)))))
                                        (l1
                                          (l1
                                            (if rand_bool
                                              (l1
                                                (if rand_bool (l1 (k_insert_4666 (if xs22_4686 true rand_bool)))
                                                  (l0 (loop_2167 (k_insert_4666 rand_bool)))))
                                              (l0 (l0 (loop_2167 (k_insert_4666 rand_bool)))))))))
                                    (l0 (loop_2167 (k_insert_4666 rand_bool)))))))))
                         (l1
                           (if rand_bool (l0 (ys''ys''_4650 (fun p22_4696 -> (k_insert_4666 true))))
                             (l1 (ys''ys''_4650 (fun x__5063 -> (k_insert_4666 (if x__5063 true rand_bool)))))))))))))))
           (l0
             (k_insert_2260
               (fun k_insert_rsrs_4964 ->
                (if rand_bool (l0 (if rand_bool (l0 (k_insert_rsrs_4964 true)) (l1 (k_insert_rsrs_4964 true))))
                  (l1
                    (if rand_bool (l0 (if rand_bool (l0 (k_insert_rsrs_4964 true)) (l1 (k_insert_rsrs_4964 true))))
                      (l1 (if rand_bool (l0 (k_insert_rsrs_4964 true)) (l1 (k_insert_rsrs_4964 true)))))))))))))
  insertsort_2165 xsxs_1042 k_insertsort_2811 ->
      (xsxs_1042
        (if rand_bool
          (l1
            (xsxs_1042
              (insertsort_2165 xsxs_1042
                (fun x_4734 ->
                 (insert_1014 (fun x__5130 -> (x_4734 (fun x__5134 -> (x__5130 (if x__5134 true rand_bool)))))
                   (fun x__5120 ->
                    (k_insertsort_2811
                      (fun x__5123 -> (x__5120 (fun x__5127 -> (x__5123 (if x__5127 true rand_bool))))))))))))
          (l0 (k_insertsort_2811 (fun k_insertsort_rsrs_4972 -> (k_insertsort_rsrs_4972 true))))))
  loop_2167 k_loop_2237 -> (loop_2167 k_loop_2237)
  make_list_1049 k_make_list_2998 -> (l0 (k_make_list_2998 (fun k_make_list_4751 -> k_make_list_4751)))
  make_list_1049 k_make_list_2998 ->
      (l1
        (make_list_1049
          (fun xs_4758 ->
           (k_make_list_2998
             (fun k_make_list_4768 -> (if rand_bool (l0 k_make_list_4768) (l1 (xs_4758 k_make_list_4768))))))))
Types:
  arg1_2161 : (unit -> unit)
  br_f_check_3701 : ((bool -> unit) -> ((bool -> unit) -> unit) -> unit)
  br_f_check_3703 : ((bool -> unit) -> ((bool -> unit) -> unit) -> unit)
  br_f_insert_3685 : (unit -> unit)
  br_f_insert_3687 : (((bool -> unit) -> unit) -> unit -> unit)
  br_f_insert_3689 : (((bool -> unit) -> unit) -> unit -> unit)
  br_f_insert_3693 : (unit -> unit)
  br_f_insert_3695 : ((unit -> unit) -> ((bool -> unit) -> unit) -> unit -> unit)
  br_f_insert_3697 : ((unit -> unit) -> ((bool -> unit) -> unit) -> unit -> unit)
  br_f_insert_3699 : ((((bool -> unit) -> unit) -> unit) -> ((bool -> unit) -> unit) -> unit)
  br_k_insert_3683 : (unit -> unit)
  br_k_insert_3691 : (unit -> unit)
  f_3682 : unit
  f_check_3674 : ((bool -> unit) -> ((bool -> unit) -> unit) -> unit)
  f_insert_3654 : ((((bool -> unit) -> unit) -> unit) -> ((bool -> unit) -> unit) -> unit)
  f_insert_3658 : (((bool -> unit) -> unit) -> unit -> unit)
  f_insert_3659 : (unit -> unit)
  f_insert_3660 : (unit -> unit)
  f_insert_3661 : ((((bool -> unit) -> unit) -> unit) -> ((bool -> unit) -> unit) -> (unit -> unit) -> unit)
  f_insert_3662 : ((unit -> unit) -> ((bool -> unit) -> unit) -> unit -> unit)
  f_insert_3663 : (unit -> unit)
  f_insert_3664 : (unit -> unit)
  f_insertsort_3665 : ((((bool -> unit) -> unit) -> unit) -> (unit -> unit) -> unit)
  f_insertsort_3666 : ((((bool -> unit) -> unit) -> unit) -> (unit -> unit) -> unit)
  f_insertsort_3668 : ((((bool -> unit) -> unit) -> unit) -> (unit -> unit) -> unit)
  f_insertsort_3669 : ((((bool -> unit) -> unit) -> unit) -> (unit -> unit) -> unit)
  f_main_3675 : (unit -> (unit -> unit) -> unit)
  f_main_3679 : (unit -> (unit -> unit) -> unit)
  f_main_3680 : (unit -> unit)
  f_main_3681 : (unit -> unit)
  f_make_list_3670 : (unit -> unit)
  f_make_list_3671 : (((unit -> unit) -> unit) -> unit)
  f_make_list_3672 : (((unit -> unit) -> unit) -> (unit -> unit) -> unit)
  f_make_list_3673 : ((unit -> unit) -> unit -> unit)
  f_rsrs_3655 : (unit -> unit)
  f_rsrs_3656 : (unit -> unit)
  f_rsrs_3657 : (unit -> unit)
  f_xs_3653 : (unit -> unit)
  f_xs_3667 : (unit -> unit)
  f_xsxs_3676 : (unit -> unit)
  f_xsxs_3677 : (unit -> (unit -> unit) -> unit)
  f_xsxs_3678 : (unit -> unit)
  is_none_2185 : (unit -> unit)
  k_insert_2437 : (unit -> unit)
  k_insert_2650 : (unit -> unit)
  k_insert_3472 : (unit -> unit)
  k_insert_3538 : (unit -> unit)
  main_1064 : (unit -> unit)
  main_2163 : (unit -> unit)
  n_1052 : (unit -> unit)
  r_1024 : (unit -> unit)
  r_1070 : ((unit -> unit) -> unit -> unit)
  rs_1019 : ((bool -> unit) -> unit)
  rsrs_1021 : (unit -> unit)
  rsrs_3608 : (unit -> unit)
  xs'xs'_1061 : (((bool -> unit) -> unit) -> unit -> unit)
  xs'xs'_3592 : ((unit -> unit) -> unit -> unit)
  xs_1053 : (((unit -> unit) -> unit) -> unit)
  xs_1066 : (((unit -> unit) -> unit) -> unit)
  xs_2203 : (((bool -> unit) -> unit) -> unit -> unit)
  xs_2206 : (((bool -> unit) -> unit) -> unit -> unit)
  xs_2208 : (((bool -> unit) -> unit) -> unit -> unit)
  xs_2212 : (((bool -> unit) -> unit) -> unit -> unit)
  xs_3581 : ((unit -> unit) -> unit -> unit)
  xsxs_1067 : ((unit -> unit) -> unit -> unit)
  ys''ys''_1034 : (((bool -> unit) -> unit) -> ((unit -> unit) -> unit) -> unit)
  ys'ys'_1025 : (((bool -> unit) -> unit) -> unit -> unit)
  ysys_1071 : ((unit -> unit) -> ((unit -> unit) -> unit) -> unit)

LIFT:
Main: main_3652
  main_3652 -> (make_list_1049 f_5146)
  f_5146 xs_4737 -> (insertsort_2165 (f_5149 xs_4737) f_5151)
  f_5149 xs_5148 k_main_xsxs_5032 ->
      (if rand_bool (l0 (xs_5148 k_main_xsxs_5032)) (l1 (xs_5148 (xs_5148 k_main_xsxs_5032))))
  f_5151 ysys_4740 -> (check_2164 (f_5154 ysys_4740) f_5159)
  f_5157 x__5156 x__5054 -> (x__5156 (if x__5054 true rand_bool))
  f_5154 ysys_5153 x__5050 -> (ysys_5153 (f_5157 x__5050))
  f_5159 b_4746 -> (if (if b_4746 true rand_bool) (l0 ()) (l1 (if b_4746 _|_ (fail_3705 ()))))
  check_2164 xsxs_1056 k_check_3093 -> (xsxs_1056 (f_5163 xsxs_1056 k_check_3093))
  f_5163 xsxs_5161 k_check_5162 xs22_4590 ->
      (if rand_bool
        (l1
          (if rand_bool
            (l1
              (if (if xs22_4590 true rand_bool) (l0 (check_2164 (f_5166 xsxs_5161) (f_5172 k_check_5162)))
                (l1 (if xs22_4590 _|_ (k_check_5162 false))))) (l0 (k_check_5162 true)))) (
        l0 (k_check_5162 true)))
  f_5172 k_check_5171 x__5055 -> (k_check_5171 (if x__5055 true rand_bool))
  f_5166 xsxs_5165 k_check_xs'xs'_4980 -> (xsxs_5165 (f_5169 k_check_xs'xs'_4980))
  f_5169 k_check_xs'xs'_5168 x__5059 -> (k_check_xs'xs'_5168 (if x__5059 true rand_bool))
  fail_3705 k -> {fail} => k
  insert_1014 ysys_1016 k_insert_2260 -> (ysys_1016 (f_5176 ysys_1016 k_insert_2260))
  f_5176 ysys_5174 k_insert_5175 p22_4800 ->
      (if rand_bool
        (l1
          (if rand_bool (l0 (k_insert_5175 (f_5179 ysys_5174)))
            (l1 (insert_1014 (f_5191 ysys_5174) (f_5198 ysys_5174 k_insert_5175))))) (
        l0 (k_insert_5175 f_5213)))
  f_5211 k_insert_5210 x__5063 -> (k_insert_5210 (if x__5063 true rand_bool))
  f_5208 k_insert_5207 p22_4696 -> (k_insert_5207 true)
  f_5205 k_insert_5204 xs22_4686 ->
      (if rand_bool
        (l1
          (if rand_bool
            (l0
              (if rand_bool
                (l1
                  (if rand_bool
                    (l1 (if rand_bool (l1 (k_insert_5204 true)) (l0 (loop_2167 (k_insert_5204 rand_bool)))))
                    (l0 (l0 (loop_2167 (k_insert_5204 rand_bool)))))) (
                l0 (loop_2167 (k_insert_5204 rand_bool)))))
            (l1
              (l1
                (if rand_bool
                  (l1
                    (if rand_bool (l1 (k_insert_5204 (if xs22_4686 true rand_bool)))
                      (l0 (loop_2167 (k_insert_5204 rand_bool))))) (l0 (l0 (loop_2167 (k_insert_5204 rand_bool)))))))))
        (l0 (loop_2167 (k_insert_5204 rand_bool))))
  f_5202 ysys_5200 ys''ys''_5201 k_insert_4666 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_insert_4666 true)) (l1 (ysys_5200 (f_5205 k_insert_4666)))))
        (l1 (if rand_bool (l0 (ys''ys''_5201 (f_5208 k_insert_4666))) (l1 (ys''ys''_5201 (f_5211 k_insert_4666))))))
  f_5198 ysys_5196 k_insert_5197 ys''ys''_4650 -> (k_insert_5197 (f_5202 ysys_5196 ys''ys''_4650))
  f_5191 ysys_5190 k_insert_ys'ys'_5044 -> (ysys_5190 (f_5194 k_insert_ys'ys'_5044))
  f_5194 k_insert_ys'ys'_5193 x__5091 -> (k_insert_ys'ys'_5193 (if x__5091 true rand_bool))
  f_5179 ysys_5178 k_insert_4614 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_insert_4614 true)) (l1 (ysys_5178 (f_5182 k_insert_4614)))))
        (l1 (if rand_bool (l0 (ysys_5178 (f_5185 k_insert_4614))) (l1 (ysys_5178 (f_5188 k_insert_4614))))))
  f_5182 k_insert_5181 xs22_4634 ->
      (if rand_bool
        (l1
          (if rand_bool
            (l0
              (if rand_bool
                (l1
                  (if rand_bool
                    (l1 (if rand_bool (l1 (k_insert_5181 true)) (l0 (loop_2167 (k_insert_5181 rand_bool)))))
                    (l0 (l0 (loop_2167 (k_insert_5181 rand_bool)))))) (
                l0 (loop_2167 (k_insert_5181 rand_bool)))))
            (l1
              (l1
                (if rand_bool
                  (l1
                    (if rand_bool (l1 (k_insert_5181 (if xs22_4634 true rand_bool)))
                      (l0 (loop_2167 (k_insert_5181 rand_bool))))) (l0 (l0 (loop_2167 (k_insert_5181 rand_bool)))))))))
        (l0 (loop_2167 (k_insert_5181 rand_bool))))
  f_5185 k_insert_5184 p22_4644 -> (k_insert_5184 true)
  f_5188 k_insert_5187 x__5095 -> (k_insert_5187 (if x__5095 true rand_bool))
  f_5213 k_insert_rsrs_4964 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_insert_rsrs_4964 true)) (l1 (k_insert_rsrs_4964 true))))
        (l1
          (if rand_bool (l0 (if rand_bool (l0 (k_insert_rsrs_4964 true)) (l1 (k_insert_rsrs_4964 true))))
            (l1 (if rand_bool (l0 (k_insert_rsrs_4964 true)) (l1 (k_insert_rsrs_4964 true)))))))
  insertsort_2165 xsxs_1042 k_insertsort_2811 ->
      (xsxs_1042
        (if rand_bool (l1 (xsxs_1042 (insertsort_2165 xsxs_1042 (f_5216 k_insertsort_2811))))
          (l0 (k_insertsort_2811 f_5233))))
  f_5231 x__5230 x__5127 -> (x__5230 (if x__5127 true rand_bool))
  f_5228 x__5227 x__5123 -> (x__5227 (f_5231 x__5123))
  f_5225 k_insertsort_5224 x__5120 -> (k_insertsort_5224 (f_5228 x__5120))
  f_5219 x_5218 x__5130 -> (x_5218 (f_5222 x__5130))
  f_5222 x__5221 x__5134 -> (x__5221 (if x__5134 true rand_bool))
  f_5216 k_insertsort_5215 x_4734 -> (insert_1014 (f_5219 x_4734) (f_5225 k_insertsort_5215))
  f_5233 k_insertsort_rsrs_4972 -> (k_insertsort_rsrs_4972 true)
  loop_2167 k_loop_2237 -> (loop_2167 k_loop_2237)
  make_list_1049 k_make_list_2998 -> (l0 (k_make_list_2998 f_5235))
  f_5235 k_make_list_4751 -> k_make_list_4751
  make_list_1049 k_make_list_2998 -> (l1 (make_list_1049 (f_5238 k_make_list_2998)))
  f_5238 k_make_list_5237 xs_4758 -> (k_make_list_5237 (f_5241 xs_4758))
  f_5241 xs_5240 k_make_list_4768 -> (if rand_bool (l0 k_make_list_4768) (l1 (xs_5240 k_make_list_4768)))

TRANS_EAGER:
Main: main_3652
  main_3652 -> (make_list_1049 f_5146)
  f_5146 xs_4737 -> (insertsort_2165 (f_5149 xs_4737) f_5151)
  f_5149 xs_5148 k_main_xsxs_5032 ->
      (let f_5243 b_5242 = (if b_5242 (l0 (xs_5148 k_main_xsxs_5032)) (l1 (xs_5148 (xs_5148 k_main_xsxs_5032)))) in
       (if rand_bool (f_5243 true) (f_5243 false)))
  f_5151 ysys_4740 -> (check_2164 (f_5154 ysys_4740) f_5159)
  f_5157 x__5156 x__5054 ->
      (let f_5247 b_5246 = (if b_5246 (x__5156 true) (if rand_bool (x__5156 true) (x__5156 false))) in (f_5247 x__5054))
  f_5154 ysys_5153 x__5050 -> (ysys_5153 (f_5157 x__5050))
  f_5159 b_4746 ->
      (let f_5249 b_5248 =
       (if b_5248 (l0 ()) (l1 (let f_5251 b_5250 = (if b_5250 _|_ (fail_3705 ())) in (f_5251 b_4746)))) in
       (let f_5253 b_5252 = (if b_5252 (f_5249 true) (if rand_bool (f_5249 true) (f_5249 false))) in (f_5253 b_4746)))
  check_2164 xsxs_1056 k_check_3093 -> (xsxs_1056 (f_5163 xsxs_1056 k_check_3093))
  f_5163 xsxs_5161 k_check_5162 xs22_4590 ->
      (let f_5255 b_5254 =
       (if b_5254
         (l1
           (let f_5257 b_5256 =
            (if b_5256
              (l1
                (let f_5259 b_5258 =
                 (if b_5258 (l0 (check_2164 (f_5166 xsxs_5161) (f_5172 k_check_5162)))
                   (l1 (let f_5261 b_5260 = (if b_5260 _|_ (k_check_5162 false)) in (f_5261 xs22_4590))))
                 in
                 (let f_5263 b_5262 = (if b_5262 (f_5259 true) (if rand_bool (f_5259 true) (f_5259 false))) in
                  (f_5263 xs22_4590)))) (l0 (k_check_5162 true)))
            in (if rand_bool (f_5257 true) (f_5257 false)))) (l0 (k_check_5162 true)))
       in (if rand_bool (f_5255 true) (f_5255 false)))
  f_5172 k_check_5171 x__5055 ->
      (let f_5267 b_5266 = (if b_5266 (k_check_5171 true) (if rand_bool (k_check_5171 true) (k_check_5171 false))) in
       (f_5267 x__5055))
  f_5166 xsxs_5165 k_check_xs'xs'_4980 -> (xsxs_5165 (f_5169 k_check_xs'xs'_4980))
  f_5169 k_check_xs'xs'_5168 x__5059 ->
      (let f_5271 b_5270 =
       (if b_5270 (k_check_xs'xs'_5168 true) (if rand_bool (k_check_xs'xs'_5168 true) (k_check_xs'xs'_5168 false))) in
       (f_5271 x__5059))
  fail_3705 k -> {fail} => k
  insert_1014 ysys_1016 k_insert_2260 -> (ysys_1016 (f_5176 ysys_1016 k_insert_2260))
  f_5176 ysys_5174 k_insert_5175 p22_4800 ->
      (let f_5273 b_5272 =
       (if b_5272
         (l1
           (let f_5275 b_5274 =
            (if b_5274 (l0 (k_insert_5175 (f_5179 ysys_5174)))
              (l1 (insert_1014 (f_5191 ysys_5174) (f_5198 ysys_5174 k_insert_5175))))
            in (if rand_bool (f_5275 true) (f_5275 false)))) (l0 (k_insert_5175 f_5213)))
       in (if rand_bool (f_5273 true) (f_5273 false)))
  f_5211 k_insert_5210 x__5063 ->
      (let f_5279 b_5278 = (if b_5278 (k_insert_5210 true) (if rand_bool (k_insert_5210 true) (k_insert_5210 false)))
       in (f_5279 x__5063))
  f_5208 k_insert_5207 p22_4696 -> (k_insert_5207 true)
  f_5205 k_insert_5204 xs22_4686 ->
      (let f_5281 b_5280 =
       (if b_5280
         (l1
           (let f_5283 b_5282 =
            (if b_5282
              (l0
                (let f_5285 b_5284 =
                 (if b_5284
                   (l1
                     (let f_5287 b_5286 =
                      (if b_5286
                        (l1
                          (let f_5289 b_5288 =
                           (if b_5288 (l1 (k_insert_5204 true))
                             (l0 (loop_2167 (if rand_bool (k_insert_5204 true) (k_insert_5204 false)))))
                           in (if rand_bool (f_5289 true) (f_5289 false))))
                        (l0 (l0 (loop_2167 (if rand_bool (k_insert_5204 true) (k_insert_5204 false))))))
                      in (if rand_bool (f_5287 true) (f_5287 false))))
                   (l0 (loop_2167 (if rand_bool (k_insert_5204 true) (k_insert_5204 false)))))
                 in (if rand_bool (f_5285 true) (f_5285 false))))
              (l1
                (l1
                  (let f_5297 b_5296 =
                   (if b_5296
                     (l1
                       (let f_5299 b_5298 =
                        (if b_5298
                          (l1
                            (let f_5303 b_5302 =
                             (if b_5302 (k_insert_5204 true) (if rand_bool (k_insert_5204 true) (k_insert_5204 false)))
                             in (f_5303 xs22_4686)))
                          (l0 (loop_2167 (if rand_bool (k_insert_5204 true) (k_insert_5204 false)))))
                        in (if rand_bool (f_5299 true) (f_5299 false))))
                     (l0 (l0 (loop_2167 (if rand_bool (k_insert_5204 true) (k_insert_5204 false))))))
                   in (if rand_bool (f_5297 true) (f_5297 false))))))
            in (if rand_bool (f_5283 true) (f_5283 false))))
         (l0 (loop_2167 (if rand_bool (k_insert_5204 true) (k_insert_5204 false)))))
       in (if rand_bool (f_5281 true) (f_5281 false)))
  f_5202 ysys_5200 ys''ys''_5201 k_insert_4666 ->
      (let f_5311 b_5310 =
       (if b_5310
         (l0
           (let f_5313 b_5312 = (if b_5312 (l0 (k_insert_4666 true)) (l1 (ysys_5200 (f_5205 k_insert_4666)))) in
            (if rand_bool (f_5313 true) (f_5313 false))))
         (l1
           (let f_5315 b_5314 =
            (if b_5314 (l0 (ys''ys''_5201 (f_5208 k_insert_4666))) (l1 (ys''ys''_5201 (f_5211 k_insert_4666)))) in
            (if rand_bool (f_5315 true) (f_5315 false)))))
       in (if rand_bool (f_5311 true) (f_5311 false)))
  f_5198 ysys_5196 k_insert_5197 ys''ys''_4650 -> (k_insert_5197 (f_5202 ysys_5196 ys''ys''_4650))
  f_5191 ysys_5190 k_insert_ys'ys'_5044 -> (ysys_5190 (f_5194 k_insert_ys'ys'_5044))
  f_5194 k_insert_ys'ys'_5193 x__5091 ->
      (let f_5319 b_5318 =
       (if b_5318 (k_insert_ys'ys'_5193 true) (if rand_bool (k_insert_ys'ys'_5193 true) (k_insert_ys'ys'_5193 false)))
       in (f_5319 x__5091))
  f_5179 ysys_5178 k_insert_4614 ->
      (let f_5321 b_5320 =
       (if b_5320
         (l0
           (let f_5323 b_5322 = (if b_5322 (l0 (k_insert_4614 true)) (l1 (ysys_5178 (f_5182 k_insert_4614)))) in
            (if rand_bool (f_5323 true) (f_5323 false))))
         (l1
           (let f_5325 b_5324 =
            (if b_5324 (l0 (ysys_5178 (f_5185 k_insert_4614))) (l1 (ysys_5178 (f_5188 k_insert_4614)))) in
            (if rand_bool (f_5325 true) (f_5325 false)))))
       in (if rand_bool (f_5321 true) (f_5321 false)))
  f_5182 k_insert_5181 xs22_4634 ->
      (let f_5327 b_5326 =
       (if b_5326
         (l1
           (let f_5329 b_5328 =
            (if b_5328
              (l0
                (let f_5331 b_5330 =
                 (if b_5330
                   (l1
                     (let f_5333 b_5332 =
                      (if b_5332
                        (l1
                          (let f_5335 b_5334 =
                           (if b_5334 (l1 (k_insert_5181 true))
                             (l0 (loop_2167 (if rand_bool (k_insert_5181 true) (k_insert_5181 false)))))
                           in (if rand_bool (f_5335 true) (f_5335 false))))
                        (l0 (l0 (loop_2167 (if rand_bool (k_insert_5181 true) (k_insert_5181 false))))))
                      in (if rand_bool (f_5333 true) (f_5333 false))))
                   (l0 (loop_2167 (if rand_bool (k_insert_5181 true) (k_insert_5181 false)))))
                 in (if rand_bool (f_5331 true) (f_5331 false))))
              (l1
                (l1
                  (let f_5343 b_5342 =
                   (if b_5342
                     (l1
                       (let f_5345 b_5344 =
                        (if b_5344
                          (l1
                            (let f_5349 b_5348 =
                             (if b_5348 (k_insert_5181 true) (if rand_bool (k_insert_5181 true) (k_insert_5181 false)))
                             in (f_5349 xs22_4634)))
                          (l0 (loop_2167 (if rand_bool (k_insert_5181 true) (k_insert_5181 false)))))
                        in (if rand_bool (f_5345 true) (f_5345 false))))
                     (l0 (l0 (loop_2167 (if rand_bool (k_insert_5181 true) (k_insert_5181 false))))))
                   in (if rand_bool (f_5343 true) (f_5343 false))))))
            in (if rand_bool (f_5329 true) (f_5329 false))))
         (l0 (loop_2167 (if rand_bool (k_insert_5181 true) (k_insert_5181 false)))))
       in (if rand_bool (f_5327 true) (f_5327 false)))
  f_5185 k_insert_5184 p22_4644 -> (k_insert_5184 true)
  f_5188 k_insert_5187 x__5095 ->
      (let f_5359 b_5358 = (if b_5358 (k_insert_5187 true) (if rand_bool (k_insert_5187 true) (k_insert_5187 false)))
       in (f_5359 x__5095))
  f_5213 k_insert_rsrs_4964 ->
      (let f_5361 b_5360 =
       (if b_5360
         (l0
           (let f_5363 b_5362 = (if b_5362 (l0 (k_insert_rsrs_4964 true)) (l1 (k_insert_rsrs_4964 true))) in
            (if rand_bool (f_5363 true) (f_5363 false))))
         (l1
           (let f_5365 b_5364 =
            (if b_5364
              (l0
                (let f_5367 b_5366 = (if b_5366 (l0 (k_insert_rsrs_4964 true)) (l1 (k_insert_rsrs_4964 true))) in
                 (if rand_bool (f_5367 true) (f_5367 false))))
              (l1
                (let f_5369 b_5368 = (if b_5368 (l0 (k_insert_rsrs_4964 true)) (l1 (k_insert_rsrs_4964 true))) in
                 (if rand_bool (f_5369 true) (f_5369 false)))))
            in (if rand_bool (f_5365 true) (f_5365 false)))))
       in (if rand_bool (f_5361 true) (f_5361 false)))
  insertsort_2165 xsxs_1042 k_insertsort_2811 ->
      (xsxs_1042
        (let f_5371 b_5370 =
         (if b_5370 (l1 (xsxs_1042 (insertsort_2165 xsxs_1042 (f_5216 k_insertsort_2811))))
           (l0 (k_insertsort_2811 f_5233)))
         in (if rand_bool (f_5371 true) (f_5371 false))))
  f_5231 x__5230 x__5127 ->
      (let f_5375 b_5374 = (if b_5374 (x__5230 true) (if rand_bool (x__5230 true) (x__5230 false))) in (f_5375 x__5127))
  f_5228 x__5227 x__5123 -> (x__5227 (f_5231 x__5123))
  f_5225 k_insertsort_5224 x__5120 -> (k_insertsort_5224 (f_5228 x__5120))
  f_5219 x_5218 x__5130 -> (x_5218 (f_5222 x__5130))
  f_5222 x__5221 x__5134 ->
      (let f_5379 b_5378 = (if b_5378 (x__5221 true) (if rand_bool (x__5221 true) (x__5221 false))) in (f_5379 x__5134))
  f_5216 k_insertsort_5215 x_4734 -> (insert_1014 (f_5219 x_4734) (f_5225 k_insertsort_5215))
  f_5233 k_insertsort_rsrs_4972 -> (k_insertsort_rsrs_4972 true)
  loop_2167 k_loop_2237 -> (loop_2167 k_loop_2237)
  make_list_1049 k_make_list_2998 -> (l0 (k_make_list_2998 f_5235))
  f_5235 k_make_list_4751 -> k_make_list_4751
  make_list_1049 k_make_list_2998 -> (l1 (make_list_1049 (f_5238 k_make_list_2998)))
  f_5238 k_make_list_5237 xs_4758 -> (k_make_list_5237 (f_5241 xs_4758))
  f_5241 xs_5240 k_make_list_4768 ->
      (let f_5381 b_5380 = (if b_5380 (l0 k_make_list_4768) (l1 (xs_5240 k_make_list_4768))) in
       (if rand_bool (f_5381 true) (f_5381 false)))

PUT_INTO_IF:
Main: main_3652
  main_3652 -> (make_list_1049 f_5146)
  f_5146 xs_4737 -> (insertsort_2165 (f_5149 xs_4737) f_5151)
  f_5149 xs_5148 k_main_xsxs_5032 ->
      (let f_5243 b_5242 = (if b_5242 (l0 (xs_5148 k_main_xsxs_5032)) (l1 (xs_5148 (xs_5148 k_main_xsxs_5032)))) in
       (if rand_bool (f_5243 true) (f_5243 false)))
  f_5151 ysys_4740 -> (check_2164 (f_5154 ysys_4740) f_5159)
  f_5157 x__5156 x__5054 ->
      (let f_5247 b_5246 = (if b_5246 (x__5156 true) (if rand_bool (x__5156 true) (x__5156 false))) in (f_5247 x__5054))
  f_5154 ysys_5153 x__5050 -> (ysys_5153 (f_5157 x__5050))
  f_5159 b_4746 ->
      (let f_5249 b_5248 =
       (if b_5248 (l0 ()) (l1 (let f_5251 b_5250 = (if b_5250 _|_ (fail_3705 ())) in (f_5251 b_4746)))) in
       (let f_5253 b_5252 = (if b_5252 (f_5249 true) (if rand_bool (f_5249 true) (f_5249 false))) in (f_5253 b_4746)))
  check_2164 xsxs_1056 k_check_3093 -> (xsxs_1056 (f_5163 xsxs_1056 k_check_3093))
  f_5163 xsxs_5161 k_check_5162 xs22_4590 ->
      (let f_5255 b_5254 =
       (if b_5254
         (l1
           (let f_5257 b_5256 =
            (if b_5256
              (l1
                (let f_5259 b_5258 =
                 (if b_5258 (l0 (check_2164 (f_5166 xsxs_5161) (f_5172 k_check_5162)))
                   (l1 (let f_5261 b_5260 = (if b_5260 _|_ (k_check_5162 false)) in (f_5261 xs22_4590))))
                 in
                 (let f_5263 b_5262 = (if b_5262 (f_5259 true) (if rand_bool (f_5259 true) (f_5259 false))) in
                  (f_5263 xs22_4590)))) (l0 (k_check_5162 true)))
            in (if rand_bool (f_5257 true) (f_5257 false)))) (l0 (k_check_5162 true)))
       in (if rand_bool (f_5255 true) (f_5255 false)))
  f_5172 k_check_5171 x__5055 ->
      (let f_5267 b_5266 = (if b_5266 (k_check_5171 true) (if rand_bool (k_check_5171 true) (k_check_5171 false))) in
       (f_5267 x__5055))
  f_5166 xsxs_5165 k_check_xs'xs'_4980 -> (xsxs_5165 (f_5169 k_check_xs'xs'_4980))
  f_5169 k_check_xs'xs'_5168 x__5059 ->
      (let f_5271 b_5270 =
       (if b_5270 (k_check_xs'xs'_5168 true) (if rand_bool (k_check_xs'xs'_5168 true) (k_check_xs'xs'_5168 false))) in
       (f_5271 x__5059))
  fail_3705 k -> {fail} => k
  insert_1014 ysys_1016 k_insert_2260 -> (ysys_1016 (f_5176 ysys_1016 k_insert_2260))
  f_5176 ysys_5174 k_insert_5175 p22_4800 ->
      (let f_5273 b_5272 =
       (if b_5272
         (l1
           (let f_5275 b_5274 =
            (if b_5274 (l0 (k_insert_5175 (f_5179 ysys_5174)))
              (l1 (insert_1014 (f_5191 ysys_5174) (f_5198 ysys_5174 k_insert_5175))))
            in (if rand_bool (f_5275 true) (f_5275 false)))) (l0 (k_insert_5175 f_5213)))
       in (if rand_bool (f_5273 true) (f_5273 false)))
  f_5211 k_insert_5210 x__5063 ->
      (let f_5279 b_5278 = (if b_5278 (k_insert_5210 true) (if rand_bool (k_insert_5210 true) (k_insert_5210 false)))
       in (f_5279 x__5063))
  f_5208 k_insert_5207 p22_4696 -> (k_insert_5207 true)
  f_5205 k_insert_5204 xs22_4686 ->
      (let f_5281 b_5280 =
       (if b_5280
         (l1
           (let f_5283 b_5282 =
            (if b_5282
              (l0
                (let f_5285 b_5284 =
                 (if b_5284
                   (l1
                     (let f_5287 b_5286 =
                      (if b_5286
                        (l1
                          (let f_5289 b_5288 =
                           (if b_5288 (l1 (k_insert_5204 true))
                             (l0 (loop_2167 (if rand_bool (k_insert_5204 true) (k_insert_5204 false)))))
                           in (if rand_bool (f_5289 true) (f_5289 false))))
                        (l0 (l0 (loop_2167 (if rand_bool (k_insert_5204 true) (k_insert_5204 false))))))
                      in (if rand_bool (f_5287 true) (f_5287 false))))
                   (l0 (loop_2167 (if rand_bool (k_insert_5204 true) (k_insert_5204 false)))))
                 in (if rand_bool (f_5285 true) (f_5285 false))))
              (l1
                (l1
                  (let f_5297 b_5296 =
                   (if b_5296
                     (l1
                       (let f_5299 b_5298 =
                        (if b_5298
                          (l1
                            (let f_5303 b_5302 =
                             (if b_5302 (k_insert_5204 true) (if rand_bool (k_insert_5204 true) (k_insert_5204 false)))
                             in (f_5303 xs22_4686)))
                          (l0 (loop_2167 (if rand_bool (k_insert_5204 true) (k_insert_5204 false)))))
                        in (if rand_bool (f_5299 true) (f_5299 false))))
                     (l0 (l0 (loop_2167 (if rand_bool (k_insert_5204 true) (k_insert_5204 false))))))
                   in (if rand_bool (f_5297 true) (f_5297 false))))))
            in (if rand_bool (f_5283 true) (f_5283 false))))
         (l0 (loop_2167 (if rand_bool (k_insert_5204 true) (k_insert_5204 false)))))
       in (if rand_bool (f_5281 true) (f_5281 false)))
  f_5202 ysys_5200 ys''ys''_5201 k_insert_4666 ->
      (let f_5311 b_5310 =
       (if b_5310
         (l0
           (let f_5313 b_5312 = (if b_5312 (l0 (k_insert_4666 true)) (l1 (ysys_5200 (f_5205 k_insert_4666)))) in
            (if rand_bool (f_5313 true) (f_5313 false))))
         (l1
           (let f_5315 b_5314 =
            (if b_5314 (l0 (ys''ys''_5201 (f_5208 k_insert_4666))) (l1 (ys''ys''_5201 (f_5211 k_insert_4666)))) in
            (if rand_bool (f_5315 true) (f_5315 false)))))
       in (if rand_bool (f_5311 true) (f_5311 false)))
  f_5198 ysys_5196 k_insert_5197 ys''ys''_4650 -> (k_insert_5197 (f_5202 ysys_5196 ys''ys''_4650))
  f_5191 ysys_5190 k_insert_ys'ys'_5044 -> (ysys_5190 (f_5194 k_insert_ys'ys'_5044))
  f_5194 k_insert_ys'ys'_5193 x__5091 ->
      (let f_5319 b_5318 =
       (if b_5318 (k_insert_ys'ys'_5193 true) (if rand_bool (k_insert_ys'ys'_5193 true) (k_insert_ys'ys'_5193 false)))
       in (f_5319 x__5091))
  f_5179 ysys_5178 k_insert_4614 ->
      (let f_5321 b_5320 =
       (if b_5320
         (l0
           (let f_5323 b_5322 = (if b_5322 (l0 (k_insert_4614 true)) (l1 (ysys_5178 (f_5182 k_insert_4614)))) in
            (if rand_bool (f_5323 true) (f_5323 false))))
         (l1
           (let f_5325 b_5324 =
            (if b_5324 (l0 (ysys_5178 (f_5185 k_insert_4614))) (l1 (ysys_5178 (f_5188 k_insert_4614)))) in
            (if rand_bool (f_5325 true) (f_5325 false)))))
       in (if rand_bool (f_5321 true) (f_5321 false)))
  f_5182 k_insert_5181 xs22_4634 ->
      (let f_5327 b_5326 =
       (if b_5326
         (l1
           (let f_5329 b_5328 =
            (if b_5328
              (l0
                (let f_5331 b_5330 =
                 (if b_5330
                   (l1
                     (let f_5333 b_5332 =
                      (if b_5332
                        (l1
                          (let f_5335 b_5334 =
                           (if b_5334 (l1 (k_insert_5181 true))
                             (l0 (loop_2167 (if rand_bool (k_insert_5181 true) (k_insert_5181 false)))))
                           in (if rand_bool (f_5335 true) (f_5335 false))))
                        (l0 (l0 (loop_2167 (if rand_bool (k_insert_5181 true) (k_insert_5181 false))))))
                      in (if rand_bool (f_5333 true) (f_5333 false))))
                   (l0 (loop_2167 (if rand_bool (k_insert_5181 true) (k_insert_5181 false)))))
                 in (if rand_bool (f_5331 true) (f_5331 false))))
              (l1
                (l1
                  (let f_5343 b_5342 =
                   (if b_5342
                     (l1
                       (let f_5345 b_5344 =
                        (if b_5344
                          (l1
                            (let f_5349 b_5348 =
                             (if b_5348 (k_insert_5181 true) (if rand_bool (k_insert_5181 true) (k_insert_5181 false)))
                             in (f_5349 xs22_4634)))
                          (l0 (loop_2167 (if rand_bool (k_insert_5181 true) (k_insert_5181 false)))))
                        in (if rand_bool (f_5345 true) (f_5345 false))))
                     (l0 (l0 (loop_2167 (if rand_bool (k_insert_5181 true) (k_insert_5181 false))))))
                   in (if rand_bool (f_5343 true) (f_5343 false))))))
            in (if rand_bool (f_5329 true) (f_5329 false))))
         (l0 (loop_2167 (if rand_bool (k_insert_5181 true) (k_insert_5181 false)))))
       in (if rand_bool (f_5327 true) (f_5327 false)))
  f_5185 k_insert_5184 p22_4644 -> (k_insert_5184 true)
  f_5188 k_insert_5187 x__5095 ->
      (let f_5359 b_5358 = (if b_5358 (k_insert_5187 true) (if rand_bool (k_insert_5187 true) (k_insert_5187 false)))
       in (f_5359 x__5095))
  f_5213 k_insert_rsrs_4964 ->
      (let f_5361 b_5360 =
       (if b_5360
         (l0
           (let f_5363 b_5362 = (if b_5362 (l0 (k_insert_rsrs_4964 true)) (l1 (k_insert_rsrs_4964 true))) in
            (if rand_bool (f_5363 true) (f_5363 false))))
         (l1
           (let f_5365 b_5364 =
            (if b_5364
              (l0
                (let f_5367 b_5366 = (if b_5366 (l0 (k_insert_rsrs_4964 true)) (l1 (k_insert_rsrs_4964 true))) in
                 (if rand_bool (f_5367 true) (f_5367 false))))
              (l1
                (let f_5369 b_5368 = (if b_5368 (l0 (k_insert_rsrs_4964 true)) (l1 (k_insert_rsrs_4964 true))) in
                 (if rand_bool (f_5369 true) (f_5369 false)))))
            in (if rand_bool (f_5365 true) (f_5365 false)))))
       in (if rand_bool (f_5361 true) (f_5361 false)))
  insertsort_2165 xsxs_1042 k_insertsort_2811 ->
      (xsxs_1042
        (let f_5371 b_5370 =
         (if b_5370 (l1 (xsxs_1042 (insertsort_2165 xsxs_1042 (f_5216 k_insertsort_2811))))
           (l0 (k_insertsort_2811 f_5233)))
         in (if rand_bool (f_5371 true) (f_5371 false))))
  f_5231 x__5230 x__5127 ->
      (let f_5375 b_5374 = (if b_5374 (x__5230 true) (if rand_bool (x__5230 true) (x__5230 false))) in (f_5375 x__5127))
  f_5228 x__5227 x__5123 -> (x__5227 (f_5231 x__5123))
  f_5225 k_insertsort_5224 x__5120 -> (k_insertsort_5224 (f_5228 x__5120))
  f_5219 x_5218 x__5130 -> (x_5218 (f_5222 x__5130))
  f_5222 x__5221 x__5134 ->
      (let f_5379 b_5378 = (if b_5378 (x__5221 true) (if rand_bool (x__5221 true) (x__5221 false))) in (f_5379 x__5134))
  f_5216 k_insertsort_5215 x_4734 -> (insert_1014 (f_5219 x_4734) (f_5225 k_insertsort_5215))
  f_5233 k_insertsort_rsrs_4972 -> (k_insertsort_rsrs_4972 true)
  loop_2167 k_loop_2237 -> (loop_2167 k_loop_2237)
  make_list_1049 k_make_list_2998 -> (l0 (k_make_list_2998 f_5235))
  f_5235 k_make_list_4751 -> k_make_list_4751
  make_list_1049 k_make_list_2998 -> (l1 (make_list_1049 (f_5238 k_make_list_2998)))
  f_5238 k_make_list_5237 xs_4758 -> (k_make_list_5237 (f_5241 xs_4758))
  f_5241 xs_5240 k_make_list_4768 ->
      (let f_5381 b_5380 = (if b_5380 (l0 k_make_list_4768) (l1 (xs_5240 k_make_list_4768))) in
       (if rand_bool (f_5381 true) (f_5381 false)))

DONE!

(0-2) Checking HORS ... DONE!

Some refinement types cannot be shown (unimplemented)

Some refinement types cannot be shown (unimplemented)

Some refinement types cannot be shown (unimplemented)

Some refinement types cannot be shown (unimplemented)

Some refinement types cannot be shown (unimplemented)

Safe!

cycles: 0
total: 0.628 sec
  abst: 0.377 sec
  mc: 0.052 sec
  refine: 0.000 sec
    exparam: 0.000 sec
