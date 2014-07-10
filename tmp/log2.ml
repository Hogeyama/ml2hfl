MoCHi: Model Checker for Higher-Order Programs
  Build: _1296d1f (after 2014-06-20 19:09:35 +0900)
  TRecS version: 1.33
  OCaml version: 4.01.0
  Command: ./mochi.opt test_relabs/isort-h.ml -tupling -bool-init-empty -disable-rc -color -tupling -gchi -list-option

parsed:
 let is_none_1008 x_1012 = match x_1012 with
                           | (b_1009, _) -> not b_1009 in
 let none_1010 = (false, 0) in
 let some_1011 x_1012 = (true, x_1012) in
 let rec insert_1013 x_1050 =
   match x_1050 with
   | (_ as x_1014) ->
       fun ysys_1015 ->
         (match fst (ysys_1015 (0, 0)) with
          | (b_1016, y_1017) ->
              if not b_1016 then
                let rs_1018 i_1019 = if i_1019 = 0 then
                                       some_1011 x_1014
                                     else
                                       none_1010 in
                let rsrs_1020 x_1144 =
                  match x_1144 with
                  | (i_1021, j_1022) ->
                      if i_1021 = j_1022 then
                        let r_1023 = rs_1018 i_1021 in
                        (r_1023, r_1023)
                      else
                        (rs_1018 i_1021, rs_1018 j_1022)
                in
                rsrs_1020
              else
                let ys'ys'_1024 x_1203 = match x_1203 with
                                         | (i_1025, j_1026) -> ysys_1015 (i_1025 + 1, j_1026 + 1) in
                if x_1014 < y_1017 then
                  fun x_1231 ->
                    (match x_1231 with
                     | (i_1027, j_1028) ->
                         if i_1027 <= 0 then
                           if j_1028 <= 0 then
                             (some_1011 x_1014, some_1011 x_1014)
                           else
                             (match ysys_1015 (0, j_1028 - 1) with
                              | ((b1_1029, y'_1030), (_, y''_1031)) ->
                                  if b_1016 <> b1_1029 || y_1017 <> y'_1030 then
                                    let rec loop_1032 x_1297 = match
                                                               x_1297 with
                                                               | () -> loop_1032 () in
                                    loop_1032 ()
                                  else
                                    (some_1011 x_1014, some_1011 y''_1031)
                              | _ -> let u_1319 = {fail} () in
                                     _|_)
                         else
                           if j_1028 <= 0 then
                             (fst (ysys_1015 (i_1027 - 1, 0)), some_1011 x_1014)
                           else
                             ysys_1015 (i_1027 - 1, j_1028 - 1))
                else
                  let ys''ys''_1033 = insert_1013 x_1014 ys'ys'_1024 in
                  fun x_1416 ->
                    (match x_1416 with
                     | (i_1034, j_1035) ->
                         if i_1034 = 0 then
                           if j_1035 = 0 then
                             (some_1011 y_1017, some_1011 y_1017)
                           else
                             (some_1011 y_1017, snd (ysys_1015 (0, j_1035 - 1)))
                         else
                           if j_1035 = 0 then
                             (fst (ys''ys''_1033 (i_1034, 0)), some_1011 y_1017)
                           else
                             ys''ys''_1033 (i_1034 - 1, j_1035 - 1))
          | _ -> let u_1525 = {fail} () in
                 _|_)
 in
 let rec insertsort_1036 xsxs_1037 =
   if is_none_1008 (fst (xsxs_1037 (0, 0))) then
     let rsrs_1038 i_1039 = (none_1010, none_1010) in
     rsrs_1038
   else
     (match fst (xsxs_1037 (0, 0)) with
      | (_, x_1040) ->
          let xs'xs'_1041 x_1646 = match x_1646 with
                                   | (i_1042, j_1043) -> xsxs_1037 (i_1042 + 1, j_1043 + 1) in
          insert_1013 x_1040 (insertsort_1036 xs'xs'_1041)
      | _ -> let u_1697 = {fail} () in
             _|_)
 in
 let rec make_list_1044 n_1045 =
   if n_1045 = 0 then
     fun i_1046 -> none_1010
   else
     let n_1047 = rand_int () in
     let xs_1048 = make_list_1044 (n_1047 - 1) in
     fun i_1049 -> (if i_1049 = 0 then
                      some_1011 n_1047
                    else
                      xs_1048 (i_1049 - 1))
 in
 let rec check_1050 xsxs_1051 =
   match xsxs_1051 (0, 0) with
   | ((b1_1052, x1_1053), (b2_1054, x2_1055)) ->
       if not b1_1052 then
         true
       else
         if not b2_1054 then
           true
         else
           let xs'xs'_1056 x_1815 = match x_1815 with
                                    | (i_1057, j_1058) -> xsxs_1051 (i_1057 + 1, j_1058 + 1) in
           x1_1053 <= x2_1055 && check_1050 xs'xs'_1056
   | _ -> let u_1847 = {fail} () in
          _|_
 in
 let main_1059 n_1060 =
   let xs_1061 = make_list_1044 n_1060 in
   let xsxs_1062 x_1874 =
     match x_1874 with
     | (i_1063, j_1064) ->
         if i_1063 = j_1064 then
           let r_1065 = xs_1061 i_1063 in
           (r_1065, r_1065)
         else
           (xs_1061 i_1063, xs_1061 j_1064)
   in
   let ysys_1066 = insertsort_1036 xsxs_1062 in
   if check_1050 ysys_1066 then
     ()
   else
     {fail} ()
 in
 ()

spec (abstraction type environment for CPS transformed program):
 insert: (int ->
            (i1_1952:int ->
               j1_1953:int ->
                 (b1_1954:bool ->
                    r1_1955:int ->
                      b2_1956:bool ->
                        int[\r2_1957. ((not b1_1954 || not b2_1956) || not (i1_1952 <= j1_1953)) || r1_1955 <= r2_1957]
                          -> X) -> X) ->
              ((i2_1968:int ->
                  j2_1969:int ->
                    (b3_1970:bool ->
                       r3_1971:int ->
                         b4_1972:bool ->
                           int[\r4_1973. ((not b3_1970 || not b4_1972) || not (i2_1968 <= j2_1969)) ||
                                         r3_1971 <= r4_1973] -> X) -> X) -> X) -> X)
 rs: (i_1990:int -> (b_1991:bool -> int[\__1992. i_1990 = 0 || not b_1991] -> X) -> X)
 insertsort: ((int -> int -> (bool -> int -> bool -> int -> X) -> X) ->
                ((i2_2015:int ->
                    j2_2016:int ->
                      (b3_2017:bool ->
                         r3_2018:int ->
                           b4_2019:bool ->
                             int[\r4_2020. ((not b3_2017 || not b4_2019) || not (i2_2015 <= j2_2016)) ||
                                           r3_2018 <= r4_2020] -> X) -> X) -> X) -> X)
 check: ((x_4_2036:int ->
            x_5_2037:int ->
              (b1_2038:bool ->
                 r1_2039:int ->
                   b2_2040:bool ->
                     int[\r2_2041. ((not b1_2038 || not b2_2040) || not (x_4_2036 <= x_5_2037)) || r1_2039 <= r2_2041]
                       -> X) -> X) -> (bool[\x_14_2052. x_14_2052] -> X) -> X)

spec (force inlined functions):
 none
 some

set_target:
 let is_none_1008 (x_1012:(bool * !!!)) = match x_1012 with
                                          | (b_1009, _) -> not b_1009 in
 let none_1010 = (false, 0) in
 let some_1011 (x_1012:!!!) = (true, x_1012) in
 let rec insert_1013 (x_1050:int) =
   match x_1050 with
   | (_ as x_1014) ->
       fun (ysys_1015:((int * int) -> ((bool * int) * (bool * int)))) ->
         (match fst (ysys_1015 (0, 0)) with
          | (b_1016, y_1017) ->
              if not b_1016 then
                let rs_1018 (i_1019:int) = if i_1019 = 0 then
                                             some_1011 x_1014
                                           else
                                             none_1010 in
                let rsrs_1020 (x_1144:(int * int)) =
                  match x_1144 with
                  | (i_1021, j_1022) ->
                      if i_1021 = j_1022 then
                        let r_1023 = rs_1018 i_1021 in
                        (r_1023, r_1023)
                      else
                        (rs_1018 i_1021, rs_1018 j_1022)
                in
                rsrs_1020
              else
                let ys'ys'_1024 (x_1203:(int * int)) =
                  match x_1203 with
                  | (i_1025, j_1026) -> ysys_1015 (i_1025 + 1, j_1026 + 1)
                in
                if x_1014 < y_1017 then
                  fun (x_1231:(int * int)) ->
                    (match x_1231 with
                     | (i_1027, j_1028) ->
                         if i_1027 <= 0 then
                           if j_1028 <= 0 then
                             (some_1011 x_1014, some_1011 x_1014)
                           else
                             (match ysys_1015 (0, j_1028 - 1) with
                              | ((b1_1029, y'_1030), (_, y''_1031)) ->
                                  if b_1016 <> b1_1029 || y_1017 <> y'_1030 then
                                    let rec loop_1032 (x_1297:unit) = match
                                                                    x_1297 with
                                                                    | () -> loop_1032 () in
                                    loop_1032 ()
                                  else
                                    (some_1011 x_1014, some_1011 y''_1031)
                              | _ -> let u_1319 = {fail} () in
                                     _|_)
                         else
                           if j_1028 <= 0 then
                             (fst (ysys_1015 (i_1027 - 1, 0)), some_1011 x_1014)
                           else
                             ysys_1015 (i_1027 - 1, j_1028 - 1))
                else
                  let ys''ys''_1033 = insert_1013 x_1014 ys'ys'_1024 in
                  fun (x_1416:(int * int)) ->
                    (match x_1416 with
                     | (i_1034, j_1035) ->
                         if i_1034 = 0 then
                           if j_1035 = 0 then
                             (some_1011 y_1017, some_1011 y_1017)
                           else
                             (some_1011 y_1017, snd (ysys_1015 (0, j_1035 - 1)))
                         else
                           if j_1035 = 0 then
                             (fst (ys''ys''_1033 (i_1034, 0)), some_1011 y_1017)
                           else
                             ys''ys''_1033 (i_1034 - 1, j_1035 - 1))
          | _ -> let u_1525 = {fail} () in
                 _|_)
 in
 let rec insertsort_1036 (xsxs_1037:((int * int) -> ((bool * int) * !!!))) =
   if is_none_1008 (fst (xsxs_1037 (0, 0))) then
     let rsrs_1038 (i_1039:!!!) = (none_1010, none_1010) in
     rsrs_1038
   else
     (match fst (xsxs_1037 (0, 0)) with
      | (_, x_1040) ->
          let xs'xs'_1041 (x_1646:(int * int)) =
            match x_1646 with
            | (i_1042, j_1043) -> xsxs_1037 (i_1042 + 1, j_1043 + 1)
          in
          insert_1013 x_1040 (insertsort_1036 xs'xs'_1041)
      | _ -> let u_1697 = {fail} () in
             _|_)
 in
 let rec make_list_1044 (n_1045:int) =
   if n_1045 = 0 then
     fun (i_1046:int) -> none_1010
   else
     let n_1047 = rand_int () in
     let xs_1048 = make_list_1044 (n_1047 - 1) in
     fun (i_1049:int) -> (if i_1049 = 0 then
                            some_1011 n_1047
                          else
                            xs_1048 (i_1049 - 1))
 in
 let rec check_1050 (xsxs_1051:((int * int) -> ((bool * !!!) * (bool * !!!)))) =
   match xsxs_1051 (0, 0) with
   | ((b1_1052, x1_1053), (b2_1054, x2_1055)) ->
       if not b1_1052 then
         true
       else
         if not b2_1054 then
           true
         else
           let xs'xs'_1056 (x_1815:(int * int)) =
             match x_1815 with
             | (i_1057, j_1058) -> xsxs_1051 (i_1057 + 1, j_1058 + 1)
           in
           x1_1053 <= x2_1055 && check_1050 xs'xs'_1056
   | _ -> let u_1847 = {fail} () in
          _|_
 in
 let main_1059 (n_1060:int) =
   let xs_1061 = make_list_1044 n_1060 in
   let xsxs_1062 (x_1874:(int * int)) =
     match x_1874 with
     | (i_1063, j_1064) ->
         if i_1063 = j_1064 then
           let r_1065 = xs_1061 i_1063 in
           (r_1065, r_1065)
         else
           (xs_1061 i_1063, xs_1061 j_1064)
   in
   let ysys_1066 = insertsort_1036 xsxs_1062 in
   if check_1050 ysys_1066 then
     ()
   else
     {fail} ()
 in
 let main_2061 = let arg1_2059 = rand_int () in
                 main_1059 arg1_2059 in
 ()

copy_poly:
 let is_none_2077 (x_1012:(bool * int)) = match x_1012 with
                                          | (b_1009, _) -> not b_1009 in
 let none_1010 = (false, 0) in
 let some_2066 (x_1012:int) = (true, x_1012) in
 let rec insert_1013 (x_1050:int) =
   match x_1050 with
   | (_ as x_1014) ->
       fun (ysys_1015:((int * int) -> ((bool * int) * (bool * int)))) ->
         (match fst (ysys_1015 (0, 0)) with
          | (b_1016, y_1017) ->
              if not b_1016 then
                let rs_1018 (i_1019:int) = if i_1019 = 0 then
                                             some_2066 x_1014
                                           else
                                             none_1010 in
                let rsrs_1020 (x_1144:(int * int)) =
                  match x_1144 with
                  | (i_1021, j_1022) ->
                      if i_1021 = j_1022 then
                        let r_1023 = rs_1018 i_1021 in
                        (r_1023, r_1023)
                      else
                        (rs_1018 i_1021, rs_1018 j_1022)
                in
                rsrs_1020
              else
                let ys'ys'_1024 (x_1203:(int * int)) =
                  match x_1203 with
                  | (i_1025, j_1026) -> ysys_1015 (i_1025 + 1, j_1026 + 1)
                in
                if x_1014 < y_1017 then
                  fun (x_1231:(int * int)) ->
                    (match x_1231 with
                     | (i_1027, j_1028) ->
                         if i_1027 <= 0 then
                           if j_1028 <= 0 then
                             (some_2066 x_1014, some_2066 x_1014)
                           else
                             (match ysys_1015 (0, j_1028 - 1) with
                              | ((b1_1029, y'_1030), (_, y''_1031)) ->
                                  if b_1016 <> b1_1029 || y_1017 <> y'_1030 then
                                    let rec loop_2062 (x_1297:unit) = match
                                                                    x_1297 with
                                                                    | () -> loop_2062 () in
                                    loop_2062 ()
                                  else
                                    (some_2066 x_1014, some_2066 y''_1031)
                              | _ -> let u_1319 = {fail} () in
                                     _|_)
                         else
                           if j_1028 <= 0 then
                             (fst (ysys_1015 (i_1027 - 1, 0)), some_2066 x_1014)
                           else
                             ysys_1015 (i_1027 - 1, j_1028 - 1))
                else
                  let ys''ys''_1033 = insert_1013 x_1014 ys'ys'_1024 in
                  fun (x_1416:(int * int)) ->
                    (match x_1416 with
                     | (i_1034, j_1035) ->
                         if i_1034 = 0 then
                           if j_1035 = 0 then
                             (some_2066 y_1017, some_2066 y_1017)
                           else
                             (some_2066 y_1017, snd (ysys_1015 (0, j_1035 - 1)))
                         else
                           if j_1035 = 0 then
                             (fst (ys''ys''_1033 (i_1034, 0)), some_2066 y_1017)
                           else
                             ys''ys''_1033 (i_1034 - 1, j_1035 - 1))
          | _ -> let u_1525 = {fail} () in
                 _|_)
 in
 let rec insertsort_2064 (xsxs_1037:((int * int) -> ((bool * int) * (bool * int)))) =
   if is_none_2077 (fst (xsxs_1037 (0, 0))) then
     let rsrs_2065 (i_1039:(int * int)) = (none_1010, none_1010) in
     rsrs_2065
   else
     (match fst (xsxs_1037 (0, 0)) with
      | (_, x_1040) ->
          let xs'xs'_1041 (x_1646:(int * int)) =
            match x_1646 with
            | (i_1042, j_1043) -> xsxs_1037 (i_1042 + 1, j_1043 + 1)
          in
          insert_1013 x_1040 (insertsort_2064 xs'xs'_1041)
      | _ -> let u_1697 = {fail} () in
             _|_)
 in
 let rec make_list_1044 (n_1045:int) =
   if n_1045 = 0 then
     fun (i_1046:int) -> none_1010
   else
     let n_1047 = rand_int () in
     let xs_1048 = make_list_1044 (n_1047 - 1) in
     fun (i_1049:int) -> (if i_1049 = 0 then
                            some_2066 n_1047
                          else
                            xs_1048 (i_1049 - 1))
 in
 let rec check_2063 (xsxs_1051:((int * int) -> ((bool * int) * (bool * int)))) =
   match xsxs_1051 (0, 0) with
   | ((b1_1052, x1_1053), (b2_1054, x2_1055)) ->
       if not b1_1052 then
         true
       else
         if not b2_1054 then
           true
         else
           let xs'xs'_1056 (x_1815:(int * int)) =
             match x_1815 with
             | (i_1057, j_1058) -> xsxs_1051 (i_1057 + 1, j_1058 + 1)
           in
           x1_1053 <= x2_1055 && check_2063 xs'xs'_1056
   | _ -> let u_1847 = {fail} () in
          _|_
 in
 let main_1059 (n_1060:int) =
   let xs_1061 = make_list_1044 n_1060 in
   let xsxs_1062 (x_1874:(int * int)) =
     match x_1874 with
     | (i_1063, j_1064) ->
         if i_1063 = j_1064 then
           let r_1065 = xs_1061 i_1063 in
           (r_1065, r_1065)
         else
           (xs_1061 i_1063, xs_1061 j_1064)
   in
   let ysys_1066 = insertsort_2064 xsxs_1062 in
   if check_2063 ysys_1066 then
     ()
   else
     {fail} ()
 in
 let main_2061 = let arg1_2059 = rand_int () in
                 main_1059 arg1_2059 in
 ()

spec (abstraction type environment for CPS transformed program):
 insert_1013: (int ->
                 (i1_1952:int ->
                    j1_1953:int ->
                      (b1_1954:bool ->
                         r1_1955:int ->
                           b2_1956:bool ->
                             int[\r2_1957. ((not b1_1954 || not b2_1956) || not (i1_1952 <= j1_1953)) ||
                                           r1_1955 <= r2_1957] -> X) -> X) ->
                   ((i2_1968:int ->
                       j2_1969:int ->
                         (b3_1970:bool ->
                            r3_1971:int ->
                              b4_1972:bool ->
                                int[\r4_1973. ((not b3_1970 || not b4_1972) || not (i2_1968 <= j2_1969)) ||
                                              r3_1971 <= r4_1973] -> X) -> X) -> X) -> X)
 rs_1018: (i_1990:int -> (b_1991:bool -> int[\__1992. i_1990 = 0 || not b_1991] -> X) -> X)
 insertsort_2064: ((int -> int -> (bool -> int -> bool -> int -> X) -> X) ->
                     ((i2_2015:int ->
                         j2_2016:int ->
                           (b3_2017:bool ->
                              r3_2018:int ->
                                b4_2019:bool ->
                                  int[\r4_2020. ((not b3_2017 || not b4_2019) || not (i2_2015 <= j2_2016)) ||
                                                r3_2018 <= r4_2020] -> X) -> X) -> X) -> X)
 check_2063: ((x_4_2036:int ->
                 x_5_2037:int ->
                   (b1_2038:bool ->
                      r1_2039:int ->
                        b2_2040:bool ->
                          int[\r2_2041. ((not b1_2038 || not b2_2040) || not (x_4_2036 <= x_5_2037)) ||
                                        r1_2039 <= r2_2041] -> X) -> X) ->
                (bool[\x_14_2052. x_14_2052] -> X) -> X)

spec (force inlined functions):
 none_1010
 some_2066

abst_recdata:
 let is_none_2077 (x_1012:(bool * int)) = match x_1012 with
                                          | (b_1009, _) -> not b_1009 in
 let none_1010 = (false, 0) in
 let some_2066 (x_1012:int) = (true, x_1012) in
 let rec insert_1013 (x_1050:int) =
   match x_1050 with
   | (_ as x_1014) ->
       fun (ysys_1015:((int * int) -> ((bool * int) * (bool * int)))) ->
         (match fst (ysys_1015 (0, 0)) with
          | (b_1016, y_1017) ->
              if not b_1016 then
                let rs_1018 (i_1019:int) = if i_1019 = 0 then
                                             some_2066 x_1014
                                           else
                                             none_1010 in
                let rsrs_1020 (x_1144:(int * int)) =
                  match x_1144 with
                  | (i_1021, j_1022) ->
                      if i_1021 = j_1022 then
                        let r_1023 = rs_1018 i_1021 in
                        (r_1023, r_1023)
                      else
                        (rs_1018 i_1021, rs_1018 j_1022)
                in
                rsrs_1020
              else
                let ys'ys'_1024 (x_1203:(int * int)) =
                  match x_1203 with
                  | (i_1025, j_1026) -> ysys_1015 (i_1025 + 1, j_1026 + 1)
                in
                if x_1014 < y_1017 then
                  fun (x_1231:(int * int)) ->
                    (match x_1231 with
                     | (i_1027, j_1028) ->
                         if i_1027 <= 0 then
                           if j_1028 <= 0 then
                             (some_2066 x_1014, some_2066 x_1014)
                           else
                             (match ysys_1015 (0, j_1028 - 1) with
                              | ((b1_1029, y'_1030), (_, y''_1031)) ->
                                  if b_1016 <> b1_1029 || y_1017 <> y'_1030 then
                                    let rec loop_2062 (x_1297:unit) = loop_2062 () in
                                    loop_2062 ()
                                  else
                                    (some_2066 x_1014, some_2066 y''_1031)
                              | _ -> let u_1319 = {fail} () in
                                     _|_)
                         else
                           if j_1028 <= 0 then
                             (fst (ysys_1015 (i_1027 - 1, 0)), some_2066 x_1014)
                           else
                             ysys_1015 (i_1027 - 1, j_1028 - 1))
                else
                  let ys''ys''_1033 = insert_1013 x_1014 ys'ys'_1024 in
                  fun (x_1416:(int * int)) ->
                    (match x_1416 with
                     | (i_1034, j_1035) ->
                         if i_1034 = 0 then
                           if j_1035 = 0 then
                             (some_2066 y_1017, some_2066 y_1017)
                           else
                             (some_2066 y_1017, snd (ysys_1015 (0, j_1035 - 1)))
                         else
                           if j_1035 = 0 then
                             (fst (ys''ys''_1033 (i_1034, 0)), some_2066 y_1017)
                           else
                             ys''ys''_1033 (i_1034 - 1, j_1035 - 1))
          | _ -> let u_1525 = {fail} () in
                 _|_)
 in
 let rec insertsort_2064 (xsxs_1037:((int * int) -> ((bool * int) * (bool * int)))) =
   if is_none_2077 (fst (xsxs_1037 (0, 0))) then
     let rsrs_2065 (i_1039:(int * int)) = (none_1010, none_1010) in
     rsrs_2065
   else
     (match fst (xsxs_1037 (0, 0)) with
      | (_, x_1040) ->
          let xs'xs'_1041 (x_1646:(int * int)) =
            match x_1646 with
            | (i_1042, j_1043) -> xsxs_1037 (i_1042 + 1, j_1043 + 1)
          in
          insert_1013 x_1040 (insertsort_2064 xs'xs'_1041)
      | _ -> let u_1697 = {fail} () in
             _|_)
 in
 let rec make_list_1044 (n_1045:int) =
   if n_1045 = 0 then
     fun (i_1046:int) -> none_1010
   else
     let n_1047 = rand_int () in
     let xs_1048 = make_list_1044 (n_1047 - 1) in
     fun (i_1049:int) -> (if i_1049 = 0 then
                            some_2066 n_1047
                          else
                            xs_1048 (i_1049 - 1))
 in
 let rec check_2063 (xsxs_1051:((int * int) -> ((bool * int) * (bool * int)))) =
   match xsxs_1051 (0, 0) with
   | ((b1_1052, x1_1053), (b2_1054, x2_1055)) ->
       if not b1_1052 then
         true
       else
         if not b2_1054 then
           true
         else
           let xs'xs'_1056 (x_1815:(int * int)) =
             match x_1815 with
             | (i_1057, j_1058) -> xsxs_1051 (i_1057 + 1, j_1058 + 1)
           in
           x1_1053 <= x2_1055 && check_2063 xs'xs'_1056
   | _ -> let u_1847 = {fail} () in
          _|_
 in
 let main_1059 (n_1060:int) =
   let xs_1061 = make_list_1044 n_1060 in
   let xsxs_1062 (x_1874:(int * int)) =
     match x_1874 with
     | (i_1063, j_1064) ->
         if i_1063 = j_1064 then
           let r_1065 = xs_1061 i_1063 in
           (r_1065, r_1065)
         else
           (xs_1061 i_1063, xs_1061 j_1064)
   in
   let ysys_1066 = insertsort_2064 xsxs_1062 in
   if check_2063 ysys_1066 then
     ()
   else
     {fail} ()
 in
 let main_2061 = let arg1_2059 = rand_int () in
                 main_1059 arg1_2059 in
 ()

encode_list:
 let is_none_2077 (x_1012:(bool * int)) = let b_1009 = fst x_1012 in
                                          not b_1009 in
 let none_1010 = (false, 0) in
 let some_2066 (x_1012:int) = (true, x_1012) in
 let rec insert_1013 (x_1050:int) =
   fun (ysys_1015:((int * int) -> ((bool * int) * (bool * int)))) ->
     (let xs_2095 = fst (ysys_1015 (0, 0)) in
      let y_1017 = snd xs_2095 in
      let b_1016 = fst xs_2095 in
      if not b_1016 then
        let rs_1018 (i_1019:int) = if i_1019 = 0 then
                                     some_2066 x_1050
                                   else
                                     none_1010 in
        let rsrs_1020 (x_1144:(int * int)) =
          let j_1022 = snd x_1144 in
          let i_1021 = fst x_1144 in
          if i_1021 = j_1022 then
            let r_1023 = rs_1018 i_1021 in
            (r_1023, r_1023)
          else
            (rs_1018 i_1021, rs_1018 j_1022)
        in
        rsrs_1020
      else
        let ys'ys'_1024 (x_1203:(int * int)) =
          let j_1026 = snd x_1203 in
          let i_1025 = fst x_1203 in
          ysys_1015 (i_1025 + 1, j_1026 + 1)
        in
        if x_1050 < y_1017 then
          fun (x_1231:(int * int)) ->
            (let j_1028 = snd x_1231 in
             let i_1027 = fst x_1231 in
             if i_1027 <= 0 then
               if j_1028 <= 0 then
                 (some_2066 x_1050, some_2066 x_1050)
               else
                 let xs_2099 = ysys_1015 (0, j_1028 - 1) in
                 let y''_1031 = snd (snd xs_2099) in
                 let b1_1029 = fst (fst xs_2099) in
                 let y'_1030 = snd (fst xs_2099) in
                 if b_1016 <> b1_1029 || y_1017 <> y'_1030 then
                   let rec loop_2062 (x_1297:unit) = loop_2062 () in
                   loop_2062 ()
                 else
                   (some_2066 x_1050, some_2066 y''_1031)
             else
               if j_1028 <= 0 then
                 (fst (ysys_1015 (i_1027 - 1, 0)), some_2066 x_1050)
               else
                 ysys_1015 (i_1027 - 1, j_1028 - 1))
        else
          let ys''ys''_1033 = insert_1013 x_1050 ys'ys'_1024 in
          fun (x_1416:(int * int)) ->
            (let j_1035 = snd x_1416 in
             let i_1034 = fst x_1416 in
             if i_1034 = 0 then
               if j_1035 = 0 then
                 (some_2066 y_1017, some_2066 y_1017)
               else
                 (some_2066 y_1017, snd (ysys_1015 (0, j_1035 - 1)))
             else
               if j_1035 = 0 then
                 (fst (ys''ys''_1033 (i_1034, 0)), some_2066 y_1017)
               else
                 ys''ys''_1033 (i_1034 - 1, j_1035 - 1)))
 in
 let rec insertsort_2064 (xsxs_1037:((int * int) -> ((bool * int) * (bool * int)))) =
   if is_none_2077 (fst (xsxs_1037 (0, 0))) then
     let rsrs_2065 (i_1039:(int * int)) = (none_1010, none_1010) in
     rsrs_2065
   else
     let xs_2101 = fst (xsxs_1037 (0, 0)) in
     let x_1040 = snd xs_2101 in
     let xs'xs'_1041 (x_1646:(int * int)) =
       let j_1043 = snd x_1646 in
       let i_1042 = fst x_1646 in
       xsxs_1037 (i_1042 + 1, j_1043 + 1)
     in
     insert_1013 x_1040 (insertsort_2064 xs'xs'_1041)
 in
 let rec make_list_1044 (n_1045:int) =
   if n_1045 = 0 then
     fun (i_1046:int) -> none_1010
   else
     let n_1047 = rand_int () in
     let xs_1048 = make_list_1044 (n_1047 - 1) in
     fun (i_1049:int) -> (if i_1049 = 0 then
                            some_2066 n_1047
                          else
                            xs_1048 (i_1049 - 1))
 in
 let rec check_2063 (xsxs_1051:((int * int) -> ((bool * int) * (bool * int)))) =
   let xs_2103 = xsxs_1051 (0, 0) in
   let x2_1055 = snd (snd xs_2103) in
   let b2_1054 = fst (snd xs_2103) in
   let b1_1052 = fst (fst xs_2103) in
   let x1_1053 = snd (fst xs_2103) in
   if not b1_1052 then
     true
   else
     if not b2_1054 then
       true
     else
       let xs'xs'_1056 (x_1815:(int * int)) =
         let j_1058 = snd x_1815 in
         let i_1057 = fst x_1815 in
         xsxs_1051 (i_1057 + 1, j_1058 + 1)
       in
       x1_1053 <= x2_1055 && check_2063 xs'xs'_1056
 in
 let main_1059 (n_1060:int) =
   let xs_1061 = make_list_1044 n_1060 in
   let xsxs_1062 (x_1874:(int * int)) =
     let j_1064 = snd x_1874 in
     let i_1063 = fst x_1874 in
     if i_1063 = j_1064 then
       let r_1065 = xs_1061 i_1063 in
       (r_1065, r_1065)
     else
       (xs_1061 i_1063, xs_1061 j_1064)
   in
   let ysys_1066 = insertsort_2064 xsxs_1062 in
   if check_2063 ysys_1066 then
     ()
   else
     {fail} ()
 in
 let main_2061 = let arg1_2059 = rand_int () in
                 main_1059 arg1_2059 in
 ()

inlined:
 let is_none_2077 (x_1012:(bool * int)) = not fst x_1012 in
 let rec insert_1013 (x_1050:int) =
   fun (ysys_1015:((int * int) -> ((bool * int) * (bool * int)))) ->
     (let xs_2095 = fst (ysys_1015 (0, 0)) in
      if not fst xs_2095 then
        let rs_1018 (i_1019:int) = if i_1019 = 0 then
                                     (true, x_1050)
                                   else
                                     (false, 0) in
        let rsrs_1020 (x_1144:(int * int)) =
          if fst x_1144 = snd x_1144 then
            let r_1023 = rs_1018 (fst x_1144) in
            (r_1023, r_1023)
          else
            (rs_1018 (fst x_1144), rs_1018 (snd x_1144))
        in
        rsrs_1020
      else
        let ys'ys'_1024 (x_1203:(int * int)) = ysys_1015 (fst x_1203 + 1, snd x_1203 + 1) in
        if x_1050 < snd xs_2095 then
          fun (x_1231:(int * int)) ->
            (if fst x_1231 <= 0 then
               if snd x_1231 <= 0 then
                 ((true, x_1050), (true, x_1050))
               else
                 let xs_2099 = ysys_1015 (0, snd x_1231 - 1) in
                 let y''_1031 = snd (snd xs_2099) in
                 let b1_1029 = fst (fst xs_2099) in
                 let y'_1030 = snd (fst xs_2099) in
                 if fst xs_2095 <> b1_1029 || snd xs_2095 <> y'_1030 then
                   let rec loop_2062 (x_1297:unit) = loop_2062 () in
                   loop_2062 ()
                 else
                   ((true, x_1050), (true, y''_1031))
             else
               if snd x_1231 <= 0 then
                 (fst (ysys_1015 (fst x_1231 - 1, 0)), (true, x_1050))
               else
                 ysys_1015 (fst x_1231 - 1, snd x_1231 - 1))
        else
          let ys''ys''_1033 = insert_1013 x_1050 ys'ys'_1024 in
          fun (x_1416:(int * int)) ->
            (if fst x_1416 = 0 then
               if snd x_1416 = 0 then
                 ((let arg_2111 = snd xs_2095 in
                   (true, arg_2111)), (let arg_2110 = snd xs_2095 in
                                       (true, arg_2110)))
               else
                 ((let arg_2109 = snd xs_2095 in
                   (true, arg_2109)), snd (ysys_1015 (0, snd x_1416 - 1)))
             else
               if snd x_1416 = 0 then
                 (fst (ys''ys''_1033 (fst x_1416, 0)), (let arg_2108 = snd xs_2095 in
                                                        (true, arg_2108)))
               else
                 ys''ys''_1033 (fst x_1416 - 1, snd x_1416 - 1)))
 in
 let rec insertsort_2064 (xsxs_1037:((int * int) -> ((bool * int) * (bool * int)))) =
   if is_none_2077 (fst (xsxs_1037 (0, 0))) then
     let rsrs_2065 (i_1039:(int * int)) = ((false, 0), (false, 0)) in
     rsrs_2065
   else
     let xs_2101 = fst (xsxs_1037 (0, 0)) in
     let xs'xs'_1041 (x_1646:(int * int)) = xsxs_1037 (fst x_1646 + 1, snd x_1646 + 1) in
     insert_1013 (snd xs_2101) (insertsort_2064 xs'xs'_1041)
 in
 let rec make_list_1044 (n_1045:int) =
   if n_1045 = 0 then
     fun (i_1046:int) -> (false, 0)
   else
     let n_1047 = rand_int () in
     let xs_1048 = make_list_1044 (n_1047 - 1) in
     fun (i_1049:int) -> (if i_1049 = 0 then
                            (true, n_1047)
                          else
                            xs_1048 (i_1049 - 1))
 in
 let rec check_2063 (xsxs_1051:((int * int) -> ((bool * int) * (bool * int)))) =
   let xs_2103 = xsxs_1051 (0, 0) in
   let x2_1055 = snd (snd xs_2103) in
   let b2_1054 = fst (snd xs_2103) in
   let b1_1052 = fst (fst xs_2103) in
   let x1_1053 = snd (fst xs_2103) in
   if not b1_1052 then
     true
   else
     if not b2_1054 then
       true
     else
       let xs'xs'_1056 (x_1815:(int * int)) = xsxs_1051 (fst x_1815 + 1, snd x_1815 + 1) in
       x1_1053 <= x2_1055 && check_2063 xs'xs'_1056
 in
 let main_1059 (n_1060:int) =
   let xs_1061 = make_list_1044 n_1060 in
   let xsxs_1062 (x_1874:(int * int)) =
     if fst x_1874 = snd x_1874 then
       let r_1065 = xs_1061 (fst x_1874) in
       (r_1065, r_1065)
     else
       (xs_1061 (fst x_1874), xs_1061 (snd x_1874))
   in
   let ysys_1066 = insertsort_2064 xsxs_1062 in
   if check_2063 ysys_1066 then
     ()
   else
     {fail} ()
 in
 let main_2061 = let arg1_2059 = rand_int () in
                 main_1059 arg1_2059 in
 ()

CPS:
 let is_none_2077 (x_1012:(bool * int)) (k_is_none_2121:(bool -> X)) = k_is_none_2121 (not fst x_1012) in
 let rec
   insert_1013 (x_1050:int) (ysys_1015:((int * int) -> (((bool * int) * (bool * int)) -> X) -> X))
              (k_insert_2134:(((int * int) -> (((bool * int) * (bool * int)) -> X) -> X) -> X)) =
   let xs_2095 (k_insert_xs_2141:((bool * int) -> X)) =
     ysys_1015 (0, 0) (fun (p_3222:((bool * int) * (bool * int))) -> k_insert_xs_2141 (fst p_3222))
   in
   xs_2095
     (fun (xs_2584:(bool * int)) ->
        (if not fst xs_2584 then
           k_insert_2134
             (let rs_1018 (i_1019:int) (k_insert_rs_2162:((bool * int) -> X)) =
                if i_1019 = 0 then
                  k_insert_rs_2162 (true, x_1050)
                else
                  k_insert_rs_2162 (false, 0)
              in
              let rsrs_1020 (x_1144:(int * int)) (k_insert_rsrs_2181:(((bool * int) * (bool * int)) -> X)) =
                if fst x_1144 = snd x_1144 then
                  let r_1023 (k_insert_rsrs_r_2188:((bool * int) -> X)) = rs_1018 (fst x_1144) k_insert_rsrs_r_2188 in
                  r_1023 (fun (r_2199:(bool * int)) -> k_insert_rsrs_2181 (r_2199, r_2199))
                else
                  rs_1018 (fst x_1144)
                    (fun (x_3308:(bool * int)) ->
                       rs_1018 (snd x_1144) (fun (x_3313:(bool * int)) -> k_insert_rsrs_2181 (x_3308, x_3313)))
              in
              rsrs_1020)
         else
           let ys'ys'_1024 (x_1203:(int * int)) (k_insert_ys'ys'_2238:(((bool * int) * (bool * int)) -> X)) =
             ysys_1015 (fst x_1203 + 1, snd x_1203 + 1) k_insert_ys'ys'_2238
           in
           if x_1050 < snd xs_2584 then
             k_insert_2134
               (fun (x_1231:(int * int)) ->
                  fun (k_insert_2258:(((bool * int) * (bool * int)) -> X)) ->
                    (if fst x_1231 <= 0 then
                       if snd x_1231 <= 0 then
                         k_insert_2258 ((true, x_1050), (true, x_1050))
                       else
                         let xs_2099 (k_insert_xs_2285:(((bool * int) * (bool * int)) -> X)) =
                           ysys_1015 (0, snd x_1231 - 1) k_insert_xs_2285
                         in
                         xs_2099
                           (fun (xs_2354:((bool * int) * (bool * int))) ->
                              (let k_insert_2299 (b_3275:bool) =
                                 if b_3275 then
                                   let rec
                                     loop_3286 (x_3287:unit) (k_insert_loop_3288:(((bool * int) * (bool * int)) -> X)) =
                                     loop_3286 () k_insert_loop_3288
                                   in
                                   loop_3286 () k_insert_2258
                                 else
                                   k_insert_2258 ((true, x_1050), (true, snd (snd xs_2354)))
                               in
                               if fst xs_2584 <> fst (fst xs_2354) then
                                 k_insert_2299 true
                               else
                                 k_insert_2299 (snd xs_2584 <> snd (fst xs_2354))))
                     else
                       if snd x_1231 <= 0 then
                         ysys_1015 (fst x_1231 - 1, 0)
                           (fun (p_3266:((bool * int) * (bool * int))) -> k_insert_2258 (fst p_3266, (true, x_1050)))
                       else
                         ysys_1015 (fst x_1231 - 1, snd x_1231 - 1) k_insert_2258))
           else
             let
               ys''ys''_1033 (k_insert_ys''ys''_2432:(((int * int) -> (((bool * int) * (bool * int)) -> X) -> X) -> X)) =
               insert_1013 x_1050 ys'ys'_1024 k_insert_ys''ys''_2432
             in
             ys''ys''_1033
               (fun (ys''ys''_2565:((int * int) -> (((bool * int) * (bool * int)) -> X) -> X)) ->
                  k_insert_2134
                    (fun (x_1416:(int * int)) ->
                       fun (k_insert_2451:(((bool * int) * (bool * int)) -> X)) ->
                         (if fst x_1416 = 0 then
                            if snd x_1416 = 0 then
                              k_insert_2451 ((true, snd xs_2584), (true, snd xs_2584))
                            else
                              ysys_1015 (0, snd x_1416 - 1)
                                (fun (p_3251:((bool * int) * (bool * int))) ->
                                   k_insert_2451 ((true, snd xs_2584), snd p_3251))
                          else
                            if snd x_1416 = 0 then
                              ys''ys''_2565 (fst x_1416, 0)
                                (fun (p_3229:((bool * int) * (bool * int))) ->
                                   k_insert_2451 (fst p_3229, (true, snd xs_2584)))
                            else
                              ys''ys''_2565 (fst x_1416 - 1, snd x_1416 - 1) k_insert_2451)))))
 in
 let rec
   insertsort_2064 (xsxs_1037:((int * int) -> (((bool * int) * (bool * int)) -> X) -> X))
                  (k_insertsort_2604:(((int * int) -> (((bool * int) * (bool * int)) -> X) -> X) -> X)) =
   xsxs_1037 (0, 0)
     (fun (p_3316:((bool * int) * (bool * int))) ->
        is_none_2077 (fst p_3316)
          (fun (b_3319:bool) ->
             (if b_3319 then
                k_insertsort_2604
                  (let rsrs_3348 (i_3349:(int * int)) (k_insertsort_rsrs_3350:(((bool * int) * (bool * int)) -> X)) =
                     k_insertsort_rsrs_3350 ((false, 0), (false, 0))
                   in
                   rsrs_3348)
              else
                let xs_3321 (k_insertsort_xs_3322:((bool * int) -> X)) =
                  xsxs_1037 (0, 0) (fun (p_3362:((bool * int) * (bool * int))) -> k_insertsort_xs_3322 (fst p_3362))
                in
                xs_3321
                  (fun (xs_3330:(bool * int)) ->
                     (let
                        xs'xs'_3332 (x_3333:(int * int))
                                   (k_insertsort_xs'xs'_3334:(((bool * int) * (bool * int)) -> X)) =
                        xsxs_1037 (fst x_3333 + 1, snd x_3333 + 1) k_insertsort_xs'xs'_3334
                      in
                      insertsort_2064 xs'xs'_3332
                        (fun (x_3365:((int * int) -> (((bool * int) * (bool * int)) -> X) -> X)) ->
                           insert_1013 (snd xs_3330) x_3365 k_insertsort_2604))))))
 in
 let rec make_list_1044 (n_1045:int) (k_make_list_2791:((int -> ((bool * int) -> X) -> X) -> X)) =
   if n_1045 = 0 then
     k_make_list_2791 (fun (i_1046:int) -> fun (k_make_list_2793:((bool * int) -> X)) -> k_make_list_2793 (false, 0))
   else
     let n_1047 (k_make_list_n_2808:(int -> X)) = rand_int_cps () k_make_list_n_2808 in
     n_1047
       (fun (n_2866:int) ->
          (let xs_1048 (k_make_list_xs_2829:((int -> ((bool * int) -> X) -> X) -> X)) =
             make_list_1044 (n_2866 - 1) k_make_list_xs_2829
           in
           xs_1048
             (fun (xs_2865:(int -> ((bool * int) -> X) -> X)) ->
                k_make_list_2791
                  (fun (i_1049:int) ->
                     fun (k_make_list_2842:((bool * int) -> X)) ->
                       (if i_1049 = 0 then
                          k_make_list_2842 (true, n_2866)
                        else
                          xs_2865 (i_1049 - 1) k_make_list_2842)))))
 in
 let rec check_2063 (xsxs_1051:((int * int) -> (((bool * int) * (bool * int)) -> X) -> X)) (k_check_2886:(bool -> X)) =
   let xs_2103 (k_check_xs_2898:(((bool * int) * (bool * int)) -> X)) = xsxs_1051 (0, 0) k_check_xs_2898 in
   xs_2103
     (fun (xs_2976:((bool * int) * (bool * int))) ->
        (if not fst (fst xs_2976) then
           k_check_2886 true
         else
           if not fst (snd xs_2976) then
             k_check_2886 true
           else
             let xs'xs'_1056 (x_1815:(int * int)) (k_check_xs'xs'_2915:(((bool * int) * (bool * int)) -> X)) =
               xsxs_1051 (fst x_1815 + 1, snd x_1815 + 1) k_check_xs'xs'_2915
             in
             if snd (fst xs_2976) <= snd (snd xs_2976) then
               check_2063 xs'xs'_1056 k_check_2886
             else
               k_check_2886 false))
 in
 let main_1059 (n_1060:int) (k_main_2986:(unit -> X)) =
   let xs_1061 (k_main_xs_2999:((int -> ((bool * int) -> X) -> X) -> X)) = make_list_1044 n_1060 k_main_xs_2999 in
   xs_1061
     (fun (xs_3145:(int -> ((bool * int) -> X) -> X)) ->
        (let xsxs_1062 (x_1874:(int * int)) (k_main_xsxs_3014:(((bool * int) * (bool * int)) -> X)) =
           if fst x_1874 = snd x_1874 then
             let r_1065 (k_main_xsxs_r_3021:((bool * int) -> X)) = xs_3145 (fst x_1874) k_main_xsxs_r_3021 in
             r_1065 (fun (r_3032:(bool * int)) -> k_main_xsxs_3014 (r_3032, r_3032))
           else
             xs_3145 (fst x_1874)
               (fun (x_3377:(bool * int)) ->
                  xs_3145 (snd x_1874) (fun (x_3382:(bool * int)) -> k_main_xsxs_3014 (x_3377, x_3382)))
         in
         let ysys_1066 (k_main_ysys_3083:(((int * int) -> (((bool * int) * (bool * int)) -> X) -> X) -> X)) =
           insertsort_2064 xsxs_1062 k_main_ysys_3083
         in
         ysys_1066
           (fun (ysys_3137:((int * int) -> (((bool * int) * (bool * int)) -> X) -> X)) ->
              check_2063 ysys_3137
                (fun (b_3383:bool) -> (if b_3383 then
                                         k_main_2986 ()
                                       else
                                         {|fail|} () k_main_2986)))))
 in
 let main_2061 (k_main_3152:(unit -> X)) =
   let arg1_2059 (k_main_arg1_3157:(int -> X)) = rand_int_cps () k_main_arg1_3157 in
   arg1_2059 (fun (arg1_3173:int) -> main_1059 arg1_3173 k_main_3152)
 in
 main_2061 (fun (main_3174:unit) -> {end})

remove_pair:
 let is_none_2077 (x1_1012:bool) (x2_1012:int) (k_is_none_2121:(bool -> X)) = k_is_none_2121 (not x1_1012) in
 let rec
   insert_1013 (x_1050:int) (ysys_1015:(int -> int -> (bool -> int -> bool -> int -> X) -> X))
              (k_insert_2134:((int -> int -> (bool -> int -> bool -> int -> X) -> X) -> X)) =
   let xs_2095 (k_insert_xs_2141:(bool -> int -> X)) =
     ysys_1015 0 0
       (fun (p11_3222:bool) ->
          fun (p12_3222:int) -> fun (p21_3222:bool) -> fun (p22_3222:int) -> k_insert_xs_2141 p11_3222 p12_3222)
   in
   xs_2095
     (fun (xs1_2584:bool) ->
        fun (xs2_2584:int) ->
          (if not xs1_2584 then
             k_insert_2134
               (let rs_1018 (i_1019:int) (k_insert_rs_2162:(bool -> int -> X)) =
                  if i_1019 = 0 then
                    k_insert_rs_2162 true x_1050
                  else
                    k_insert_rs_2162 false 0
                in
                let rsrs_1020 (x1_1144:int) (x2_1144:int) (k_insert_rsrs_2181:(bool -> int -> bool -> int -> X)) =
                  if x1_1144 = x2_1144 then
                    let r_1023 (k_insert_rsrs_r_2188:(bool -> int -> X)) = rs_1018 x1_1144 k_insert_rsrs_r_2188 in
                    r_1023
                      (fun (r1_2199:bool) -> fun (r2_2199:int) -> k_insert_rsrs_2181 r1_2199 r2_2199 r1_2199 r2_2199)
                  else
                    rs_1018 x1_1144
                      (fun (x1_3308:bool) ->
                         fun (x2_3308:int) ->
                           rs_1018 x2_1144
                             (fun (x1_3313:bool) ->
                                fun (x2_3313:int) -> k_insert_rsrs_2181 x1_3308 x2_3308 x1_3313 x2_3313))
                in
                rsrs_1020)
           else
             let ys'ys'_1024 (x1_1203:int) (x2_1203:int) (k_insert_ys'ys'_2238:(bool -> int -> bool -> int -> X)) =
               ysys_1015 (x1_1203 + 1) (x2_1203 + 1) k_insert_ys'ys'_2238
             in
             if x_1050 < xs2_2584 then
               k_insert_2134
                 (fun (x1_1231:int) ->
                    fun (x2_1231:int) ->
                      fun (k_insert_2258:(bool -> int -> bool -> int -> X)) ->
                        (if x1_1231 <= 0 then
                           if x2_1231 <= 0 then
                             k_insert_2258 true x_1050 true x_1050
                           else
                             let xs_2099 (k_insert_xs_2285:(bool -> int -> bool -> int -> X)) =
                               ysys_1015 0 (x2_1231 - 1) k_insert_xs_2285
                             in
                             xs_2099
                               (fun (xs11_2354:bool) ->
                                  fun (xs12_2354:int) ->
                                    fun (xs21_2354:bool) ->
                                      fun (xs22_2354:int) ->
                                        (let k_insert_2299 (b_3275:bool) =
                                           if b_3275 then
                                             let rec
                                               loop_3286 (x_3287:unit)
                                                        (k_insert_loop_3288:(
                                                        bool -> int -> bool -> int -> X)) =
                                               loop_3286 () k_insert_loop_3288
                                             in
                                             loop_3286 () k_insert_2258
                                           else
                                             k_insert_2258 true x_1050 true xs22_2354
                                         in
                                         if xs1_2584 <> xs11_2354 then
                                           k_insert_2299 true
                                         else
                                           k_insert_2299 (xs2_2584 <> xs12_2354)))
                         else
                           if x2_1231 <= 0 then
                             ysys_1015 (x1_1231 - 1) 0
                               (fun (p11_3266:bool) ->
                                  fun (p12_3266:int) ->
                                    fun (p21_3266:bool) ->
                                      fun (p22_3266:int) -> k_insert_2258 p11_3266 p12_3266 true x_1050)
                           else
                             ysys_1015 (x1_1231 - 1) (x2_1231 - 1) k_insert_2258))
             else
               let
                 ys''ys''_1033 (k_insert_ys''ys''_2432:((int -> int -> (bool -> int -> bool -> int -> X) -> X) -> X)) =
                 insert_1013 x_1050 ys'ys'_1024 k_insert_ys''ys''_2432
               in
               ys''ys''_1033
                 (fun (ys''ys''_2565:(int -> int -> (bool -> int -> bool -> int -> X) -> X)) ->
                    k_insert_2134
                      (fun (x1_1416:int) ->
                         fun (x2_1416:int) ->
                           fun (k_insert_2451:(bool -> int -> bool -> int -> X)) ->
                             (if x1_1416 = 0 then
                                if x2_1416 = 0 then
                                  k_insert_2451 true xs2_2584 true xs2_2584
                                else
                                  ysys_1015 0 (x2_1416 - 1)
                                    (fun (p11_3251:bool) ->
                                       fun (p12_3251:int) ->
                                         fun (p21_3251:bool) ->
                                           fun (p22_3251:int) -> k_insert_2451 true xs2_2584 p21_3251 p22_3251)
                              else
                                if x2_1416 = 0 then
                                  ys''ys''_2565 x1_1416 0
                                    (fun (p11_3229:bool) ->
                                       fun (p12_3229:int) ->
                                         fun (p21_3229:bool) ->
                                           fun (p22_3229:int) -> k_insert_2451 p11_3229 p12_3229 true xs2_2584)
                                else
                                  ys''ys''_2565 (x1_1416 - 1) (x2_1416 - 1) k_insert_2451)))))
 in
 let rec
   insertsort_2064 (xsxs_1037:(int -> int -> (bool -> int -> bool -> int -> X) -> X))
                  (k_insertsort_2604:((int -> int -> (bool -> int -> bool -> int -> X) -> X) -> X)) =
   xsxs_1037 0 0
     (fun (p11_3316:bool) ->
        fun (p12_3316:int) ->
          fun (p21_3316:bool) ->
            fun (p22_3316:int) ->
              is_none_2077 p11_3316 p12_3316
                (fun (b_3319:bool) ->
                   (if b_3319 then
                      k_insertsort_2604
                        (let
                           rsrs_3348 (i1_3349:int) (i2_3349:int)
                                    (k_insertsort_rsrs_3350:(bool -> int -> bool -> int -> X)) =
                           k_insertsort_rsrs_3350 false 0 false 0
                         in
                         rsrs_3348)
                    else
                      let xs_3321 (k_insertsort_xs_3322:(bool -> int -> X)) =
                        xsxs_1037 0 0
                          (fun (p11_3362:bool) ->
                             fun (p12_3362:int) ->
                               fun (p21_3362:bool) -> fun (p22_3362:int) -> k_insertsort_xs_3322 p11_3362 p12_3362)
                      in
                      xs_3321
                        (fun (xs1_3330:bool) ->
                           fun (xs2_3330:int) ->
                             (let
                                xs'xs'_3332 (x1_3333:int) (x2_3333:int)
                                           (k_insertsort_xs'xs'_3334:(
                                           bool -> int -> bool -> int -> X)) =
                                xsxs_1037 (x1_3333 + 1) (x2_3333 + 1) k_insertsort_xs'xs'_3334
                              in
                              insertsort_2064 xs'xs'_3332
                                (fun (x_3365:(int -> int -> (bool -> int -> bool -> int -> X) -> X)) ->
                                   insert_1013 xs2_3330 x_3365 k_insertsort_2604))))))
 in
 let rec make_list_1044 (n_1045:int) (k_make_list_2791:((int -> (bool -> int -> X) -> X) -> X)) =
   if n_1045 = 0 then
     k_make_list_2791 (fun (i_1046:int) -> fun (k_make_list_2793:(bool -> int -> X)) -> k_make_list_2793 false 0)
   else
     let n_1047 (k_make_list_n_2808:(int -> X)) = rand_int_cps () k_make_list_n_2808 in
     n_1047
       (fun (n_2866:int) ->
          (let xs_1048 (k_make_list_xs_2829:((int -> (bool -> int -> X) -> X) -> X)) =
             make_list_1044 (n_2866 - 1) k_make_list_xs_2829
           in
           xs_1048
             (fun (xs_2865:(int -> (bool -> int -> X) -> X)) ->
                k_make_list_2791
                  (fun (i_1049:int) ->
                     fun (k_make_list_2842:(bool -> int -> X)) ->
                       (if i_1049 = 0 then
                          k_make_list_2842 true n_2866
                        else
                          xs_2865 (i_1049 - 1) k_make_list_2842)))))
 in
 let rec check_2063 (xsxs_1051:(int -> int -> (bool -> int -> bool -> int -> X) -> X)) (k_check_2886:(bool -> X)) =
   let xs_2103 (k_check_xs_2898:(bool -> int -> bool -> int -> X)) = xsxs_1051 0 0 k_check_xs_2898 in
   xs_2103
     (fun (xs11_2976:bool) ->
        fun (xs12_2976:int) ->
          fun (xs21_2976:bool) ->
            fun (xs22_2976:int) ->
              (if not xs11_2976 then
                 k_check_2886 true
               else
                 if not xs21_2976 then
                   k_check_2886 true
                 else
                   let
                     xs'xs'_1056 (x1_1815:int) (x2_1815:int) (k_check_xs'xs'_2915:(bool -> int -> bool -> int -> X)) =
                     xsxs_1051 (x1_1815 + 1) (x2_1815 + 1) k_check_xs'xs'_2915
                   in
                   if xs12_2976 <= xs22_2976 then
                     check_2063 xs'xs'_1056 k_check_2886
                   else
                     k_check_2886 false))
 in
 let main_1059 (n_1060:int) (k_main_2986:(unit -> X)) =
   let xs_1061 (k_main_xs_2999:((int -> (bool -> int -> X) -> X) -> X)) = make_list_1044 n_1060 k_main_xs_2999 in
   xs_1061
     (fun (xs_3145:(int -> (bool -> int -> X) -> X)) ->
        (let xsxs_1062 (x1_1874:int) (x2_1874:int) (k_main_xsxs_3014:(bool -> int -> bool -> int -> X)) =
           if x1_1874 = x2_1874 then
             let r_1065 (k_main_xsxs_r_3021:(bool -> int -> X)) = xs_3145 x1_1874 k_main_xsxs_r_3021 in
             r_1065 (fun (r1_3032:bool) -> fun (r2_3032:int) -> k_main_xsxs_3014 r1_3032 r2_3032 r1_3032 r2_3032)
           else
             xs_3145 x1_1874
               (fun (x1_3377:bool) ->
                  fun (x2_3377:int) ->
                    xs_3145 x2_1874
                      (fun (x1_3382:bool) -> fun (x2_3382:int) -> k_main_xsxs_3014 x1_3377 x2_3377 x1_3382 x2_3382))
         in
         let ysys_1066 (k_main_ysys_3083:((int -> int -> (bool -> int -> bool -> int -> X) -> X) -> X)) =
           insertsort_2064 xsxs_1062 k_main_ysys_3083
         in
         ysys_1066
           (fun (ysys_3137:(int -> int -> (bool -> int -> bool -> int -> X) -> X)) ->
              check_2063 ysys_3137
                (fun (b_3383:bool) -> (if b_3383 then
                                         k_main_2986 ()
                                       else
                                         {|fail|} () k_main_2986)))))
 in
 let main_2061 (k_main_3152:(unit -> X)) =
   let arg1_2059 (k_main_arg1_3157:(int -> X)) = rand_int_cps () k_main_arg1_3157 in
   arg1_2059 (fun (arg1_3173:int) -> main_1059 arg1_3173 k_main_3152)
 in
 main_2061 (fun (main_3174:unit) -> {end})

spec (abstraction type environment for CPS transformed program):
 insert_1013: (int ->
                 (i1_1952:int ->
                    j1_1953:int ->
                      (b1_1954:bool ->
                         r1_1955:int ->
                           b2_1956:bool ->
                             int[\r2_1957. ((not b1_1954 || not b2_1956) || not (i1_1952 <= j1_1953)) ||
                                           r1_1955 <= r2_1957] -> X) -> X) ->
                   ((i2_1968:int ->
                       j2_1969:int ->
                         (b3_1970:bool ->
                            r3_1971:int ->
                              b4_1972:bool ->
                                int[\r4_1973. ((not b3_1970 || not b4_1972) || not (i2_1968 <= j2_1969)) ||
                                              r3_1971 <= r4_1973] -> X) -> X) -> X) -> X)
 rs_1018: (i_1990:int -> (b_1991:bool -> int[\__1992. i_1990 = 0 || not b_1991] -> X) -> X)
 insertsort_2064: ((int -> int -> (bool -> int -> bool -> int -> X) -> X) ->
                     ((i2_2015:int ->
                         j2_2016:int ->
                           (b3_2017:bool ->
                              r3_2018:int ->
                                b4_2019:bool ->
                                  int[\r4_2020. ((not b3_2017 || not b4_2019) || not (i2_2015 <= j2_2016)) ||
                                                r3_2018 <= r4_2020] -> X) -> X) -> X) -> X)
 check_2063: ((x_4_2036:int ->
                 x_5_2037:int ->
                   (b1_2038:bool ->
                      r1_2039:int ->
                        b2_2040:bool ->
                          int[\r2_2041. ((not b1_2038 || not b2_2040) || not (x_4_2036 <= x_5_2037)) ||
                                        r1_2039 <= r2_2041] -> X) -> X) ->
                (bool[\x_14_2052. x_14_2052] -> X) -> X)

add_preds:
 let is_none_2077 (x1_1012:bool) (x2_1012:int) (k_is_none_2121:(bool -> X)) = k_is_none_2121 (not x1_1012) in
 let rec
   insert_1013 (x_1050:int)
              (ysys_1015:(i1_1952:int ->
                            j1_1953:int ->
                              (b1_1954:bool ->
                                 r1_1955:int ->
                                   b2_1956:bool ->
                                     int[\r2_1957. ((not b1_1954 || not b2_1956) || not (i1_1952 <= j1_1953)) ||
                                                   r1_1955 <= r2_1957] -> X) -> X))
              (k_insert_2134:((i2_1968:int ->
                                 j2_1969:int ->
                                   (b3_1970:bool ->
                                      r3_1971:int ->
                                        b4_1972:bool ->
                                          int[\r4_1973. ((not b3_1970 || not b4_1972) || not (i2_1968 <= j2_1969)) ||
                                                        r3_1971 <= r4_1973] -> X) -> X) -> X)) =
   let xs_2095 (k_insert_xs_2141:(bool -> int -> X)) =
     ysys_1015 0 0
       (fun (p11_3222:bool) ->
          fun (p12_3222:int) -> fun (p21_3222:bool) -> fun (p22_3222:int) -> k_insert_xs_2141 p11_3222 p12_3222)
   in
   xs_2095
     (fun (xs1_2584:bool) ->
        fun (xs2_2584:int) ->
          (if not xs1_2584 then
             k_insert_2134
               (let
                  rs_1018 (i_1019:int) (k_insert_rs_2162:(b_1991:bool -> int[\__1992. i_1019 = 0 || not b_1991] -> X)) =
                  if i_1019 = 0 then
                    k_insert_rs_2162 true x_1050
                  else
                    k_insert_rs_2162 false 0
                in
                let rsrs_1020 (x1_1144:int) (x2_1144:int) (k_insert_rsrs_2181:(bool -> int -> bool -> int -> X)) =
                  if x1_1144 = x2_1144 then
                    let r_1023 (k_insert_rsrs_r_2188:(bool -> int -> X)) = rs_1018 x1_1144 k_insert_rsrs_r_2188 in
                    r_1023
                      (fun (r1_2199:bool) -> fun (r2_2199:int) -> k_insert_rsrs_2181 r1_2199 r2_2199 r1_2199 r2_2199)
                  else
                    rs_1018 x1_1144
                      (fun (x1_3308:bool) ->
                         fun (x2_3308:int) ->
                           rs_1018 x2_1144
                             (fun (x1_3313:bool) ->
                                fun (x2_3313:int) -> k_insert_rsrs_2181 x1_3308 x2_3308 x1_3313 x2_3313))
                in
                rsrs_1020)
           else
             let ys'ys'_1024 (x1_1203:int) (x2_1203:int) (k_insert_ys'ys'_2238:(bool -> int -> bool -> int -> X)) =
               ysys_1015 (x1_1203 + 1) (x2_1203 + 1) k_insert_ys'ys'_2238
             in
             if x_1050 < xs2_2584 then
               k_insert_2134
                 (fun (x1_1231:int) ->
                    fun (x2_1231:int) ->
                      fun (k_insert_2258:(bool -> int -> bool -> int -> X)) ->
                        (if x1_1231 <= 0 then
                           if x2_1231 <= 0 then
                             k_insert_2258 true x_1050 true x_1050
                           else
                             let xs_2099 (k_insert_xs_2285:(bool -> int -> bool -> int -> X)) =
                               ysys_1015 0 (x2_1231 - 1) k_insert_xs_2285
                             in
                             xs_2099
                               (fun (xs11_2354:bool) ->
                                  fun (xs12_2354:int) ->
                                    fun (xs21_2354:bool) ->
                                      fun (xs22_2354:int) ->
                                        (let k_insert_2299 (b_3275:bool) =
                                           if b_3275 then
                                             let rec
                                               loop_3286 (x_3287:unit)
                                                        (k_insert_loop_3288:(
                                                        bool -> int -> bool -> int -> X)) =
                                               loop_3286 () k_insert_loop_3288
                                             in
                                             loop_3286 () k_insert_2258
                                           else
                                             k_insert_2258 true x_1050 true xs22_2354
                                         in
                                         if xs1_2584 <> xs11_2354 then
                                           k_insert_2299 true
                                         else
                                           k_insert_2299 (xs2_2584 <> xs12_2354)))
                         else
                           if x2_1231 <= 0 then
                             ysys_1015 (x1_1231 - 1) 0
                               (fun (p11_3266:bool) ->
                                  fun (p12_3266:int) ->
                                    fun (p21_3266:bool) ->
                                      fun (p22_3266:int) -> k_insert_2258 p11_3266 p12_3266 true x_1050)
                           else
                             ysys_1015 (x1_1231 - 1) (x2_1231 - 1) k_insert_2258))
             else
               let
                 ys''ys''_1033 (k_insert_ys''ys''_2432:((int -> int -> (bool -> int -> bool -> int -> X) -> X) -> X)) =
                 insert_1013 x_1050 ys'ys'_1024 k_insert_ys''ys''_2432
               in
               ys''ys''_1033
                 (fun (ys''ys''_2565:(int -> int -> (bool -> int -> bool -> int -> X) -> X)) ->
                    k_insert_2134
                      (fun (x1_1416:int) ->
                         fun (x2_1416:int) ->
                           fun (k_insert_2451:(bool -> int -> bool -> int -> X)) ->
                             (if x1_1416 = 0 then
                                if x2_1416 = 0 then
                                  k_insert_2451 true xs2_2584 true xs2_2584
                                else
                                  ysys_1015 0 (x2_1416 - 1)
                                    (fun (p11_3251:bool) ->
                                       fun (p12_3251:int) ->
                                         fun (p21_3251:bool) ->
                                           fun (p22_3251:int) -> k_insert_2451 true xs2_2584 p21_3251 p22_3251)
                              else
                                if x2_1416 = 0 then
                                  ys''ys''_2565 x1_1416 0
                                    (fun (p11_3229:bool) ->
                                       fun (p12_3229:int) ->
                                         fun (p21_3229:bool) ->
                                           fun (p22_3229:int) -> k_insert_2451 p11_3229 p12_3229 true xs2_2584)
                                else
                                  ys''ys''_2565 (x1_1416 - 1) (x2_1416 - 1) k_insert_2451)))))
 in
 let rec
   insertsort_2064 (xsxs_1037:(int -> int -> (bool -> int -> bool -> int -> X) -> X))
                  (k_insertsort_2604:((i2_2015:int ->
                                         j2_2016:int ->
                                           (b3_2017:bool ->
                                              r3_2018:int ->
                                                b4_2019:bool ->
                                                  int[\r4_2020. ((not b3_2017 || not b4_2019) ||
                                                                 not (i2_2015 <= j2_2016))
                                                                || r3_2018 <= r4_2020] -> X) -> X) -> X)) =
   xsxs_1037 0 0
     (fun (p11_3316:bool) ->
        fun (p12_3316:int) ->
          fun (p21_3316:bool) ->
            fun (p22_3316:int) ->
              is_none_2077 p11_3316 p12_3316
                (fun (b_3319:bool) ->
                   (if b_3319 then
                      k_insertsort_2604
                        (let
                           rsrs_3348 (i1_3349:int) (i2_3349:int)
                                    (k_insertsort_rsrs_3350:(bool -> int -> bool -> int -> X)) =
                           k_insertsort_rsrs_3350 false 0 false 0
                         in
                         rsrs_3348)
                    else
                      let xs_3321 (k_insertsort_xs_3322:(bool -> int -> X)) =
                        xsxs_1037 0 0
                          (fun (p11_3362:bool) ->
                             fun (p12_3362:int) ->
                               fun (p21_3362:bool) -> fun (p22_3362:int) -> k_insertsort_xs_3322 p11_3362 p12_3362)
                      in
                      xs_3321
                        (fun (xs1_3330:bool) ->
                           fun (xs2_3330:int) ->
                             (let
                                xs'xs'_3332 (x1_3333:int) (x2_3333:int)
                                           (k_insertsort_xs'xs'_3334:(
                                           bool -> int -> bool -> int -> X)) =
                                xsxs_1037 (x1_3333 + 1) (x2_3333 + 1) k_insertsort_xs'xs'_3334
                              in
                              insertsort_2064 xs'xs'_3332
                                (fun (x_3365:(int -> int -> (bool -> int -> bool -> int -> X) -> X)) ->
                                   insert_1013 xs2_3330 x_3365 k_insertsort_2604))))))
 in
 let rec make_list_1044 (n_1045:int) (k_make_list_2791:((int -> (bool -> int -> X) -> X) -> X)) =
   if n_1045 = 0 then
     k_make_list_2791 (fun (i_1046:int) -> fun (k_make_list_2793:(bool -> int -> X)) -> k_make_list_2793 false 0)
   else
     let n_1047 (k_make_list_n_2808:(int -> X)) = rand_int_cps () k_make_list_n_2808 in
     n_1047
       (fun (n_2866:int) ->
          (let xs_1048 (k_make_list_xs_2829:((int -> (bool -> int -> X) -> X) -> X)) =
             make_list_1044 (n_2866 - 1) k_make_list_xs_2829
           in
           xs_1048
             (fun (xs_2865:(int -> (bool -> int -> X) -> X)) ->
                k_make_list_2791
                  (fun (i_1049:int) ->
                     fun (k_make_list_2842:(bool -> int -> X)) ->
                       (if i_1049 = 0 then
                          k_make_list_2842 true n_2866
                        else
                          xs_2865 (i_1049 - 1) k_make_list_2842)))))
 in
 let rec
   check_2063
             (xsxs_1051:(x_4_2036:int ->
                           x_5_2037:int ->
                             (b1_2038:bool ->
                                r1_2039:int ->
                                  b2_2040:bool ->
                                    int[\r2_2041. ((not b1_2038 || not b2_2040) || not (x_4_2036 <= x_5_2037)) ||
                                                  r1_2039 <= r2_2041] -> X) -> X))
             (k_check_2886:(bool[\x_14_2052. x_14_2052] -> X)) =
   let xs_2103 (k_check_xs_2898:(bool -> int -> bool -> int -> X)) = xsxs_1051 0 0 k_check_xs_2898 in
   xs_2103
     (fun (xs11_2976:bool) ->
        fun (xs12_2976:int) ->
          fun (xs21_2976:bool) ->
            fun (xs22_2976:int) ->
              (if not xs11_2976 then
                 k_check_2886 true
               else
                 if not xs21_2976 then
                   k_check_2886 true
                 else
                   let
                     xs'xs'_1056 (x1_1815:int) (x2_1815:int) (k_check_xs'xs'_2915:(bool -> int -> bool -> int -> X)) =
                     xsxs_1051 (x1_1815 + 1) (x2_1815 + 1) k_check_xs'xs'_2915
                   in
                   if xs12_2976 <= xs22_2976 then
                     check_2063 xs'xs'_1056 k_check_2886
                   else
                     k_check_2886 false))
 in
 let main_1059 (n_1060:int) (k_main_2986:(unit -> X)) =
   let xs_1061 (k_main_xs_2999:((int -> (bool -> int -> X) -> X) -> X)) = make_list_1044 n_1060 k_main_xs_2999 in
   xs_1061
     (fun (xs_3145:(int -> (bool -> int -> X) -> X)) ->
        (let xsxs_1062 (x1_1874:int) (x2_1874:int) (k_main_xsxs_3014:(bool -> int -> bool -> int -> X)) =
           if x1_1874 = x2_1874 then
             let r_1065 (k_main_xsxs_r_3021:(bool -> int -> X)) = xs_3145 x1_1874 k_main_xsxs_r_3021 in
             r_1065 (fun (r1_3032:bool) -> fun (r2_3032:int) -> k_main_xsxs_3014 r1_3032 r2_3032 r1_3032 r2_3032)
           else
             xs_3145 x1_1874
               (fun (x1_3377:bool) ->
                  fun (x2_3377:int) ->
                    xs_3145 x2_1874
                      (fun (x1_3382:bool) -> fun (x2_3382:int) -> k_main_xsxs_3014 x1_3377 x2_3377 x1_3382 x2_3382))
         in
         let ysys_1066 (k_main_ysys_3083:((int -> int -> (bool -> int -> bool -> int -> X) -> X) -> X)) =
           insertsort_2064 xsxs_1062 k_main_ysys_3083
         in
         ysys_1066
           (fun (ysys_3137:(int -> int -> (bool -> int -> bool -> int -> X) -> X)) ->
              check_2063 ysys_3137
                (fun (b_3383:bool) -> (if b_3383 then
                                         k_main_2986 ()
                                       else
                                         {|fail|} () k_main_2986)))))
 in
 let main_2061 (k_main_3152:(unit -> X)) =
   let arg1_2059 (k_main_arg1_3157:(int -> X)) = rand_int_cps () k_main_arg1_3157 in
   arg1_2059 (fun (arg1_3173:int) -> main_1059 arg1_3173 k_main_3152)
 in
 main_2061 (fun (main_3174:unit) -> {end})

spec (abstraction type environment for CPS transformed program):
 insert_1013: (int ->
                 (i1_1952:int ->
                    j1_1953:int ->
                      (b1_1954:bool ->
                         r1_1955:int ->
                           b2_1956:bool ->
                             int[\r2_1957. ((not b1_1954 || not b2_1956) || not (i1_1952 <= j1_1953)) ||
                                           r1_1955 <= r2_1957] -> X) -> X) ->
                   ((i2_1968:int ->
                       j2_1969:int ->
                         (b3_1970:bool ->
                            r3_1971:int ->
                              b4_1972:bool ->
                                int[\r4_1973. ((not b3_1970 || not b4_1972) || not (i2_1968 <= j2_1969)) ||
                                              r3_1971 <= r4_1973] -> X) -> X) -> X) -> X)
 rs_1018: (i_1990:int -> (b_1991:bool -> int[\__1992. i_1990 = 0 || not b_1991] -> X) -> X)
 insertsort_2064: ((int -> int -> (bool -> int -> bool -> int -> X) -> X) ->
                     ((i2_2015:int ->
                         j2_2016:int ->
                           (b3_2017:bool ->
                              r3_2018:int ->
                                b4_2019:bool ->
                                  int[\r4_2020. ((not b3_2017 || not b4_2019) || not (i2_2015 <= j2_2016)) ||
                                                r3_2018 <= r4_2020] -> X) -> X) -> X) -> X)
 check_2063: ((x_4_2036:int ->
                 x_5_2037:int ->
                   (b1_2038:bool ->
                      r1_2039:int ->
                        b2_2040:bool ->
                          int[\r2_2041. ((not b1_2038 || not b2_2040) || not (x_4_2036 <= x_5_2037)) ||
                                        r1_2039 <= r2_2041] -> X) -> X) ->
                (bool[\x_14_2052. x_14_2052] -> X) -> X)

Program with abstraction types (CEGAR-cycle 0)::
Main: main_3392
  main_3392 -> (main_2061 f_3422)
  arg1_2059 k_main_arg1_3157 -> (rand_int k_main_arg1_3157)
  br_f_check_3433 b_3434 k_check_2886 xsxs_1051 xs11_2976 xs12_2976 xs21_2976 xs22_2976 when b_3434 ->
      (check_2063 (xs'xs'_1056 xs11_2976 xs12_2976 xs21_2976 xs22_2976 xsxs_1051) k_check_2886)
  br_f_check_3433 b_3434 k_check_2886 xsxs_1051 xs11_2976 xs12_2976 xs21_2976 xs22_2976 when (
      not b_3434) -> (k_check_2886 false)
  br_f_check_3435 b_3436 k_check_2886 xsxs_1051 xs11_2976 xs12_2976 xs21_2976 xs22_2976 when b_3436 ->
      (k_check_2886 true)
  br_f_check_3435 b_3436 k_check_2886 xsxs_1051 xs11_2976 xs12_2976 xs21_2976 xs22_2976 when (
      not b_3436) ->
      (br_f_check_3433 (xs12_2976 <= xs22_2976) k_check_2886 xsxs_1051 xs11_2976 xs12_2976 xs21_2976 xs22_2976)
  br_f_insert_3423 b_3424 x_1050 xs1_2584 xs2_2584 ysys_1015 x1_1231 x2_1231 k_insert_2258 when b_3424 ->
      (k_insert_2258 true x_1050 true x_1050)
  br_f_insert_3423 b_3424 x_1050 xs1_2584 xs2_2584 ysys_1015 x1_1231 x2_1231 k_insert_2258 when (
      not b_3424) ->
      (xs_2099 x1_1231 x2_1231 x_1050 xs1_2584 xs2_2584 ysys_1015
        (f_insert_3399 x1_1231 x2_1231 x_1050 xs1_2584 xs2_2584 k_insert_2258))
  br_f_insert_3425 b_3426 x_1050 xs1_2584 xs2_2584 ysys_1015 x1_1231 x2_1231 k_insert_2258 when b_3426 ->
      (ysys_1015 (x1_1231 - 1) 0 (f_insert_3400 x1_1231 x2_1231 x_1050 xs1_2584 xs2_2584 k_insert_2258))
  br_f_insert_3425 b_3426 x_1050 xs1_2584 xs2_2584 ysys_1015 x1_1231 x2_1231 k_insert_2258 when (
      not b_3426) -> (ysys_1015 (x1_1231 - 1) (x2_1231 - 1) k_insert_2258)
  br_f_insert_3427 b_3428 x_1050 xs1_2584 xs2_2584 ys''ys''_2565 ysys_1015 x1_1416 x2_1416 k_insert_2451 when b_3428 ->
      (k_insert_2451 true xs2_2584 true xs2_2584)
  br_f_insert_3427 b_3428 x_1050 xs1_2584 xs2_2584 ys''ys''_2565 ysys_1015 x1_1416 x2_1416 k_insert_2451 when (
      not b_3428) -> (ysys_1015 0 (x2_1416 - 1) (f_insert_3403 x1_1416 x2_1416 x_1050 xs1_2584 xs2_2584 k_insert_2451))
  br_f_insert_3429 b_3430 x_1050 xs1_2584 xs2_2584 ys''ys''_2565 ysys_1015 x1_1416 x2_1416 k_insert_2451 when b_3430 ->
      (ys''ys''_2565 x1_1416 0 (f_insert_3404 x1_1416 x2_1416 x_1050 xs1_2584 xs2_2584 k_insert_2451))
  br_f_insert_3429 b_3430 x_1050 xs1_2584 xs2_2584 ys''ys''_2565 ysys_1015 x1_1416 x2_1416 k_insert_2451 when (
      not b_3430) -> (ys''ys''_2565 (x1_1416 - 1) (x2_1416 - 1) k_insert_2451)
  br_f_insert_3431 b_3432 x_1050 k_insert_2134 ysys_1015 xs1_2584 xs2_2584 when b_3432 ->
      (k_insert_2134 (f_insert_3398 x_1050 xs1_2584 xs2_2584 ysys_1015))
  br_f_insert_3431 b_3432 x_1050 k_insert_2134 ysys_1015 xs1_2584 xs2_2584 when (
      not b_3432) ->
      (ys''ys''_1033 x_1050 xs1_2584 xs2_2584 ysys_1015
        (f_insert_3401 x_1050 xs1_2584 xs2_2584 k_insert_2134 ysys_1015))
  check_2063 xsxs_1051 k_check_2886 -> (xs_2103 xsxs_1051 (f_check_3414 k_check_2886 xsxs_1051))
  f_3422 main_3174 -> end
  f_check_3414 k_check_2886 xsxs_1051 xs11_2976 xs12_2976 xs21_2976 xs22_2976 when (
      not xs11_2976) -> (k_check_2886 true)
  f_check_3414 k_check_2886 xsxs_1051 xs11_2976 xs12_2976 xs21_2976 xs22_2976 when (
      not (not xs11_2976)) ->
      (br_f_check_3435 (not xs21_2976) k_check_2886 xsxs_1051 xs11_2976 xs12_2976 xs21_2976 xs22_2976)
  f_insert_3394 x_1050 k_insert_2134 ysys_1015 xs1_2584 xs2_2584 when (
      not xs1_2584) -> (k_insert_2134 (rsrs_1020 x_1050 xs1_2584 xs2_2584))
  f_insert_3394 x_1050 k_insert_2134 ysys_1015 xs1_2584 xs2_2584 when (
      not (not xs1_2584)) -> (br_f_insert_3431 (x_1050 < xs2_2584) x_1050 k_insert_2134 ysys_1015 xs1_2584 xs2_2584)
  f_insert_3398 x_1050 xs1_2584 xs2_2584 ysys_1015 x1_1231 x2_1231 k_insert_2258 when (
      x1_1231 <= 0) ->
      (br_f_insert_3423 (x2_1231 <= 0) x_1050 xs1_2584 xs2_2584 ysys_1015 x1_1231 x2_1231 k_insert_2258)
  f_insert_3398 x_1050 xs1_2584 xs2_2584 ysys_1015 x1_1231 x2_1231 k_insert_2258 when (
      not (x1_1231 <= 0)) ->
      (br_f_insert_3425 (x2_1231 <= 0) x_1050 xs1_2584 xs2_2584 ysys_1015 x1_1231 x2_1231 k_insert_2258)
  f_insert_3399 x1_1231 x2_1231 x_1050 xs1_2584 xs2_2584 k_insert_2258 xs11_2354 xs12_2354 xs21_2354 xs22_2354 when (
      not (xs1_2584 <=> xs11_2354)) ->
      (k_insert_2299 x1_1231 x2_1231 x_1050 xs11_2354 xs12_2354 xs1_2584 xs21_2354 xs22_2354 xs2_2584 k_insert_2258
        true)
  f_insert_3399 x1_1231 x2_1231 x_1050 xs1_2584 xs2_2584 k_insert_2258 xs11_2354 xs12_2354 xs21_2354 xs22_2354 when (
      not (not (xs1_2584 <=> xs11_2354))) ->
      (k_insert_2299 x1_1231 x2_1231 x_1050 xs11_2354 xs12_2354 xs1_2584 xs21_2354 xs22_2354 xs2_2584 k_insert_2258
        (not (xs2_2584 = xs12_2354)))
  f_insert_3400 x1_1231 x2_1231 x_1050 xs1_2584 xs2_2584 k_insert_2258 p11_3266 p12_3266 p21_3266 p22_3266 ->
      (k_insert_2258 p11_3266 p12_3266 true x_1050)
  f_insert_3401 x_1050 xs1_2584 xs2_2584 k_insert_2134 ysys_1015 ys''ys''_2565 ->
      (k_insert_2134 (f_insert_3402 x_1050 xs1_2584 xs2_2584 ys''ys''_2565 ysys_1015))
  f_insert_3402 x_1050 xs1_2584 xs2_2584 ys''ys''_2565 ysys_1015 x1_1416 x2_1416 k_insert_2451 when (
      x1_1416 = 0) ->
      (br_f_insert_3427 (x2_1416 = 0) x_1050 xs1_2584 xs2_2584 ys''ys''_2565 ysys_1015 x1_1416 x2_1416 k_insert_2451)
  f_insert_3402 x_1050 xs1_2584 xs2_2584 ys''ys''_2565 ysys_1015 x1_1416 x2_1416 k_insert_2451 when (
      not (x1_1416 = 0)) ->
      (br_f_insert_3429 (x2_1416 = 0) x_1050 xs1_2584 xs2_2584 ys''ys''_2565 ysys_1015 x1_1416 x2_1416 k_insert_2451)
  f_insert_3403 x1_1416 x2_1416 x_1050 xs1_2584 xs2_2584 k_insert_2451 p11_3251 p12_3251 p21_3251 p22_3251 ->
      (k_insert_2451 true xs2_2584 p21_3251 p22_3251)
  f_insert_3404 x1_1416 x2_1416 x_1050 xs1_2584 xs2_2584 k_insert_2451 p11_3229 p12_3229 p21_3229 p22_3229 ->
      (k_insert_2451 p11_3229 p12_3229 true xs2_2584)
  f_insertsort_3405 k_insertsort_2604 xsxs_1037 p11_3316 p12_3316 p21_3316 p22_3316 ->
      (is_none_2077 p11_3316 p12_3316
        (f_insertsort_3406 p11_3316 p12_3316 p21_3316 p22_3316 k_insertsort_2604 xsxs_1037))
  f_insertsort_3406 p11_3316 p12_3316 p21_3316 p22_3316 k_insertsort_2604 xsxs_1037 b_3319 when b_3319 ->
      (k_insertsort_2604 (rsrs_3348 b_3319 p11_3316 p12_3316 p21_3316 p22_3316))
  f_insertsort_3406 p11_3316 p12_3316 p21_3316 p22_3316 k_insertsort_2604 xsxs_1037 b_3319 when (
      not b_3319) ->
      (xs_3321 b_3319 p11_3316 p12_3316 p21_3316 p22_3316 xsxs_1037
        (f_insertsort_3408 b_3319 p11_3316 p12_3316 p21_3316 p22_3316 k_insertsort_2604 xsxs_1037))
  f_insertsort_3408 b_3319 p11_3316 p12_3316 p21_3316 p22_3316 k_insertsort_2604 xsxs_1037 xs1_3330 xs2_3330 ->
      (insertsort_2064 (xs'xs'_3332 b_3319 p11_3316 p12_3316 p21_3316 p22_3316 xs1_3330 xs2_3330 xsxs_1037)
        (f_insertsort_3409 b_3319 p11_3316 p12_3316 p21_3316 p22_3316 xs1_3330 xs2_3330 k_insertsort_2604))
  f_insertsort_3409 b_3319 p11_3316 p12_3316 p21_3316 p22_3316 xs1_3330 xs2_3330 k_insertsort_2604 x_3365 ->
      (insert_1013 xs2_3330 x_3365 k_insertsort_2604)
  f_main_3415 n_1060 k_main_2986 xs_3145 -> (ysys_1066 n_1060 xs_3145 (f_main_3419 n_1060 k_main_2986))
  f_main_3419 n_1060 k_main_2986 ysys_3137 -> (check_2063 ysys_3137 (f_main_3420 n_1060 k_main_2986))
  f_main_3420 n_1060 k_main_2986 b_3383 when b_3383 -> (k_main_2986 ())
  f_main_3420 n_1060 k_main_2986 b_3383 when (not b_3383) -> (fail_3437 true k_main_2986)
  f_main_3421 k_main_3152 arg1_3173 -> (main_1059 arg1_3173 k_main_3152)
  f_make_list_3410 n_1045 i_1046 k_make_list_2793 -> (k_make_list_2793 false 0)
  f_make_list_3411 n_1045 k_make_list_2791 n_2866 ->
      (xs_1048 n_1045 n_2866 (f_make_list_3412 n_1045 n_2866 k_make_list_2791))
  f_make_list_3412 n_1045 n_2866 k_make_list_2791 xs_2865 ->
      (k_make_list_2791 (f_make_list_3413 n_1045 n_2866 xs_2865))
  f_make_list_3413 n_1045 n_2866 xs_2865 i_1049 k_make_list_2842 when (i_1049 = 0) -> (k_make_list_2842 true n_2866)
  f_make_list_3413 n_1045 n_2866 xs_2865 i_1049 k_make_list_2842 when (
      not (i_1049 = 0)) -> (xs_2865 (i_1049 - 1) k_make_list_2842)
  f_rsrs_3395 x1_1144 x2_1144 x_1050 xs1_2584 xs2_2584 k_insert_rsrs_2181 r1_2199 r2_2199 ->
      (k_insert_rsrs_2181 r1_2199 r2_2199 r1_2199 r2_2199)
  f_rsrs_3396 x1_1144 x2_1144 x_1050 xs1_2584 xs2_2584 k_insert_rsrs_2181 x1_3308 x2_3308 ->
      (rs_1018 x_1050 xs1_2584 xs2_2584 x2_1144
        (f_rsrs_3397 x1_1144 x1_3308 x2_1144 x2_3308 x_1050 xs1_2584 xs2_2584 k_insert_rsrs_2181))
  f_rsrs_3397 x1_1144 x1_3308 x2_1144 x2_3308 x_1050 xs1_2584 xs2_2584 k_insert_rsrs_2181 x1_3313 x2_3313 ->
      (k_insert_rsrs_2181 x1_3308 x2_3308 x1_3313 x2_3313)
  f_xs_3393 x_1050 k_insert_xs_2141 p11_3222 p12_3222 p21_3222 p22_3222 -> (k_insert_xs_2141 p11_3222 p12_3222)
  f_xs_3407 b_3319 p11_3316 p12_3316 p21_3316 p22_3316 k_insertsort_xs_3322 p11_3362 p12_3362 p21_3362 p22_3362 ->
      (k_insertsort_xs_3322 p11_3362 p12_3362)
  f_xsxs_3416 n_1060 x1_1874 x2_1874 k_main_xsxs_3014 r1_3032 r2_3032 ->
      (k_main_xsxs_3014 r1_3032 r2_3032 r1_3032 r2_3032)
  f_xsxs_3417 n_1060 x1_1874 x2_1874 k_main_xsxs_3014 xs_3145 x1_3377 x2_3377 ->
      (xs_3145 x2_1874 (f_xsxs_3418 n_1060 x1_1874 x1_3377 x2_1874 x2_3377 k_main_xsxs_3014))
  f_xsxs_3418 n_1060 x1_1874 x1_3377 x2_1874 x2_3377 k_main_xsxs_3014 x1_3382 x2_3382 ->
      (k_main_xsxs_3014 x1_3377 x2_3377 x1_3382 x2_3382)
  fail_3437 b k -> {fail} => (k ())
  insert_1013 x_1050 ysys_1015 k_insert_2134 ->
      (xs_2095 x_1050 ysys_1015 (f_insert_3394 x_1050 k_insert_2134 ysys_1015))
  insertsort_2064 xsxs_1037 k_insertsort_2604 -> (xsxs_1037 0 0 (f_insertsort_3405 k_insertsort_2604 xsxs_1037))
  is_none_2077 x1_1012 x2_1012 k_is_none_2121 -> (k_is_none_2121 (not x1_1012))
  k_insert_2299 x1_1231 x2_1231 x_1050 xs11_2354 xs12_2354 xs1_2584 xs21_2354 xs22_2354 xs2_2584 k_insert_2258 b_3275 when b_3275 ->
      (loop_3286 b_3275 x1_1231 x2_1231 x_1050 xs11_2354 xs12_2354 xs1_2584 xs21_2354 xs22_2354 xs2_2584 ()
        k_insert_2258)
  k_insert_2299 x1_1231 x2_1231 x_1050 xs11_2354 xs12_2354 xs1_2584 xs21_2354 xs22_2354 xs2_2584 k_insert_2258 b_3275 when (
      not b_3275) -> (k_insert_2258 true x_1050 true xs22_2354)
  loop_3286 b_3275 x1_1231 x2_1231 x_1050 xs11_2354 xs12_2354 xs1_2584 xs21_2354 xs22_2354 xs2_2584 x_3287
  k_insert_loop_3288 ->
      (loop_3286 b_3275 x1_1231 x2_1231 x_1050 xs11_2354 xs12_2354 xs1_2584 xs21_2354 xs22_2354 xs2_2584 ()
        k_insert_loop_3288)
  main_1059 n_1060 k_main_2986 -> (xs_1061 n_1060 (f_main_3415 n_1060 k_main_2986))
  main_2061 k_main_3152 -> (arg1_2059 (f_main_3421 k_main_3152))
  make_list_1044 n_1045 k_make_list_2791 when (n_1045 = 0) -> (k_make_list_2791 (f_make_list_3410 n_1045))
  make_list_1044 n_1045 k_make_list_2791 when (not (n_1045 = 0)) ->
      (n_1047 n_1045 (f_make_list_3411 n_1045 k_make_list_2791))
  n_1047 n_1045 k_make_list_n_2808 -> (rand_int k_make_list_n_2808)
  r_1023 x1_1144 x2_1144 x_1050 xs1_2584 xs2_2584 k_insert_rsrs_r_2188 ->
      (rs_1018 x_1050 xs1_2584 xs2_2584 x1_1144 k_insert_rsrs_r_2188)
  r_1065 n_1060 x1_1874 x2_1874 xs_3145 k_main_xsxs_r_3021 -> (xs_3145 x1_1874 k_main_xsxs_r_3021)
  rs_1018 x_1050 xs1_2584 xs2_2584 i_1019 k_insert_rs_2162 when (i_1019 = 0) -> (k_insert_rs_2162 true x_1050)
  rs_1018 x_1050 xs1_2584 xs2_2584 i_1019 k_insert_rs_2162 when (not (i_1019 = 0)) -> (k_insert_rs_2162 false 0)
  rsrs_1020 x_1050 xs1_2584 xs2_2584 x1_1144 x2_1144 k_insert_rsrs_2181 when (
      x1_1144 = x2_1144) ->
      (r_1023 x1_1144 x2_1144 x_1050 xs1_2584 xs2_2584
        (f_rsrs_3395 x1_1144 x2_1144 x_1050 xs1_2584 xs2_2584 k_insert_rsrs_2181))
  rsrs_1020 x_1050 xs1_2584 xs2_2584 x1_1144 x2_1144 k_insert_rsrs_2181 when (
      not (x1_1144 = x2_1144)) ->
      (rs_1018 x_1050 xs1_2584 xs2_2584 x1_1144
        (f_rsrs_3396 x1_1144 x2_1144 x_1050 xs1_2584 xs2_2584 k_insert_rsrs_2181))
  rsrs_3348 b_3319 p11_3316 p12_3316 p21_3316 p22_3316 i1_3349 i2_3349 k_insertsort_rsrs_3350 ->
      (k_insertsort_rsrs_3350 false 0 false 0)
  xs'xs'_1056 xs11_2976 xs12_2976 xs21_2976 xs22_2976 xsxs_1051 x1_1815 x2_1815 k_check_xs'xs'_2915 ->
      (xsxs_1051 (x1_1815 + 1) (x2_1815 + 1) k_check_xs'xs'_2915)
  xs'xs'_3332 b_3319 p11_3316 p12_3316 p21_3316 p22_3316 xs1_3330 xs2_3330 xsxs_1037 x1_3333 x2_3333
  k_insertsort_xs'xs'_3334 -> (xsxs_1037 (x1_3333 + 1) (x2_3333 + 1) k_insertsort_xs'xs'_3334)
  xs_1048 n_1045 n_2866 k_make_list_xs_2829 -> (make_list_1044 (n_2866 - 1) k_make_list_xs_2829)
  xs_1061 n_1060 k_main_xs_2999 -> (make_list_1044 n_1060 k_main_xs_2999)
  xs_2095 x_1050 ysys_1015 k_insert_xs_2141 -> (ysys_1015 0 0 (f_xs_3393 x_1050 k_insert_xs_2141))
  xs_2099 x1_1231 x2_1231 x_1050 xs1_2584 xs2_2584 ysys_1015 k_insert_xs_2285 ->
      (ysys_1015 0 (x2_1231 - 1) k_insert_xs_2285)
  xs_2103 xsxs_1051 k_check_xs_2898 -> (xsxs_1051 0 0 k_check_xs_2898)
  xs_3321 b_3319 p11_3316 p12_3316 p21_3316 p22_3316 xsxs_1037 k_insertsort_xs_3322 ->
      (xsxs_1037 0 0 (f_xs_3407 b_3319 p11_3316 p12_3316 p21_3316 p22_3316 k_insertsort_xs_3322))
  xsxs_1062 n_1060 xs_3145 x1_1874 x2_1874 k_main_xsxs_3014 when (x1_1874 = x2_1874) ->
      (r_1065 n_1060 x1_1874 x2_1874 xs_3145 (f_xsxs_3416 n_1060 x1_1874 x2_1874 k_main_xsxs_3014))
  xsxs_1062 n_1060 xs_3145 x1_1874 x2_1874 k_main_xsxs_3014 when (not (x1_1874 = x2_1874)) ->
      (xs_3145 x1_1874 (f_xsxs_3417 n_1060 x1_1874 x2_1874 k_main_xsxs_3014 xs_3145))
  ys''ys''_1033 x_1050 xs1_2584 xs2_2584 ysys_1015 k_insert_ys''ys''_2432 ->
      (insert_1013 x_1050 (ys'ys'_1024 x_1050 xs1_2584 xs2_2584 ysys_1015) k_insert_ys''ys''_2432)
  ys'ys'_1024 x_1050 xs1_2584 xs2_2584 ysys_1015 x1_1203 x2_1203 k_insert_ys'ys'_2238 ->
      (ysys_1015 (x1_1203 + 1) (x2_1203 + 1) k_insert_ys'ys'_2238)
  ysys_1066 n_1060 xs_3145 k_main_ysys_3083 -> (insertsort_2064 (xsxs_1062 n_1060 xs_3145) k_main_ysys_3083)
Types:
  main_3392 : X
  check_2063 : ((x_2:int ->
                 x_3:int ->
                 (x_5:bool ->
                  x_6:int -> x_7:bool -> x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_6 <= x_8] -> X)
                 -> X)
                -> (x_12:bool[x_12] -> X) -> X)
  fail_3437 : (bool -> (unit -> X) -> X)
  insert_1013 : (int ->
                 (x_3:int ->
                  x_4:int ->
                  (x_6:bool ->
                   x_7:int -> x_8:bool -> x_9:int[(not x_6) || (not x_8) || (not (x_3 <= x_4)) || x_7 <= x_9] -> X)
                  -> X)
                 ->
                 ((x_14:int ->
                   x_15:int ->
                   (x_17:bool ->
                    x_18:int ->
                    x_19:bool -> x_20:int[(not x_17) || (not x_19) || (not (x_14 <= x_15)) || x_18 <= x_20] -> X)
                   -> X)
                 -> X) -> X)
  insertsort_2064 : ((int -> int -> (bool -> int -> bool -> int -> X) -> X) ->
                     ((x_13:int ->
                       x_14:int ->
                       (x_16:bool ->
                        x_17:int ->
                        x_18:bool -> x_19:int[(not x_16) || (not x_18) || (not (x_13 <= x_14)) || x_17 <= x_19] -> X)
                       -> X)
                     -> X) -> X)
  loop_3286 : (bool ->
               int ->
               int ->
               int -> bool -> int -> bool -> bool -> int -> int -> unit -> (bool -> int -> bool -> int -> X) -> X)
  make_list_1044 : (int -> ((int -> (bool -> int -> X) -> X) -> X) -> X)

(0-1) Abstracting ... EXPAND_NONREC:
Main: main_3392
  main_3392 ->
      (rand_int
        (fun arg1_4199 ->
         (make_list_1044 arg1_4199
           (fun xs_4188 ->
            (insertsort_2064
              (fun x1_4404 x2_4405 k_main_xsxs_4406 ->
               (if (x2_4405 = x1_4404)
                 (l0 (xs_4188 x1_4404 (fun r1_4266 r2_4267 -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267))))
                 (l1
                   (xs_4188 x1_4404
                     (fun x1_4273 x2_4274 ->
                      (xs_4188 x2_4405 (fun x1_4281 x2_4282 -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282))))))))
              (fun ysys_4191 ->
               (check_2063 ysys_4191 (fun b_4197 -> (if b_4197 (l0 end) (l1 (fail_3437 true (fun main_4039 -> end))))))))))))
  check_2063 xsxs_1051 k_check_2886 ->
      (xsxs_1051 0 0
        (fun xs11_4048 xs12_4049 xs21_4050 xs22_4051 ->
         (if xs11_4048
           (l1
             (if xs21_4050
               (l1
                 (if (xs22_4051 >= xs12_4049)
                   (l0
                     (check_2063
                       (fun x1_4359 x2_4360 k_check_xs'xs'_4361 ->
                        (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1) k_check_xs'xs'_4361)) k_check_2886))
                   (l1 (k_check_2886 false)))) (l0 (k_check_2886 true)))) (
           l0 (k_check_2886 true)))))
  fail_3437 b k -> {fail} => (k ())
  insert_1013 x_1050 ysys_1015 k_insert_2134 ->
      (ysys_1015 0 0
        (fun p11_4248 p12_4249 p21_4250 p22_4251 ->
         (if p11_4248
           (l1
             (if (x_1050 < p12_4249)
               (l0
                 (k_insert_2134
                   (fun x1_4073 x2_4074 k_insert_4075 ->
                    (if (x1_4073 <= 0)
                      (l0
                        (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
                          (l1
                            (ysys_1015 0 (x2_4074 - 1)
                              (fun xs11_4092 xs12_4093 xs21_4094 xs22_4095 ->
                               (if (p11_4248 <=> xs11_4092)
                                 (l1
                                   (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                                     (l0
                                       (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092
                                         xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 () k_insert_4075))))
                                 (l0
                                   (l0
                                     (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094
                                       xs22_4095 p12_4249 () k_insert_4075)))))))))
                      (l1
                        (if (x2_4074 <= 0)
                          (l0
                            (ysys_1015 (x1_4073 - 1) 0
                              (fun p11_4102 p12_4103 p21_4104 p22_4105 -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
                          (l1 (ysys_1015 (x1_4073 - 1) (x2_4074 - 1) k_insert_4075))))))))
               (l1
                 (insert_1013 x_1050
                   (fun x1_4416 x2_4417 k_insert_ys'ys'_4418 ->
                    (ysys_1015 (x1_4416 + 1) (x2_4417 + 1) k_insert_ys'ys'_4418))
                   (fun ys''ys''_4111 ->
                    (k_insert_2134
                      (fun x1_4125 x2_4126 k_insert_4127 ->
                       (if (x1_4125 = 0)
                         (l0
                           (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                             (l1
                               (ysys_1015 0 (x2_4126 - 1)
                                 (fun p11_4134 p12_4135 p21_4136 p22_4137 ->
                                  (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
                         (l1
                           (if (x2_4126 = 0)
                             (l0
                               (ys''ys''_4111 x1_4125 0
                                 (fun p11_4144 p12_4145 p21_4146 p22_4147 ->
                                  (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                             (l1 (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1) k_insert_4127))))))))))))
           (l0
             (k_insert_2134
               (fun x1_4343 x2_4344 k_insert_rsrs_4345 ->
                (if (x2_4344 = x1_4343)
                  (l0
                    (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                      (l1 (k_insert_rsrs_4345 false 0 false 0))))
                  (l1
                    (if (x1_4343 = 0)
                      (l0
                        (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                          (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
                      (l1
                        (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050))
                          (l1 (k_insert_rsrs_4345 false 0 false 0)))))))))))))
  insertsort_2064 xsxs_1037 k_insertsort_2604 ->
      (xsxs_1037 0 0
        (fun p11_4150 p12_4151 p21_4152 p22_4153 ->
         (if p11_4150
           (l1
             (xsxs_1037 0 0
               (fun p11_4258 p12_4259 p21_4260 p22_4261 ->
                (insertsort_2064
                  (fun x1_4370 x2_4371 k_insertsort_xs'xs'_4372 ->
                   (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1) k_insertsort_xs'xs'_4372))
                  (fun x_4185 -> (insert_1013 p12_4259 x_4185 k_insertsort_2604))))))
           (l0
             (k_insertsort_2604
               (fun i1_4351 i2_4352 k_insertsort_rsrs_4353 -> (k_insertsort_rsrs_4353 false 0 false 0)))))))
  loop_3286 b_3275 x1_1231 x2_1231 x_1050 xs11_2354 xs12_2354 xs1_2584 xs21_2354 xs22_2354 xs2_2584 x_3287
  k_insert_loop_3288 ->
      (loop_3286 b_3275 x1_1231 x2_1231 x_1050 xs11_2354 xs12_2354 xs1_2584 xs21_2354 xs22_2354 xs2_2584 ()
        k_insert_loop_3288)
  make_list_1044 n_1045 k_make_list_2791 when (n_1045 = 0) ->
      (l0 (k_make_list_2791 (fun i_4201 k_make_list_4202 -> (k_make_list_4202 false 0))))
  make_list_1044 n_1045 k_make_list_2791 when (not (n_1045 = 0)) ->
      (l1
        (rand_int
          (fun n_4205 ->
           (make_list_1044 (n_4205 - 1)
             (fun xs_4209 ->
              (k_make_list_2791
                (fun i_4218 k_make_list_4219 ->
                 (if (i_4218 = 0) (l0 (k_make_list_4219 true n_4205)) (l1 (xs_4209 (i_4218 - 1) k_make_list_4219))))))))))

ETA: (rand_int
       (fun arg1_4199 ->
        (make_list_1044 arg1_4199
          (fun xs_4188 ->
           (insertsort_2064
             (fun x1_4404 x2_4405 k_main_xsxs_4406 ->
              (if (x2_4405 = x1_4404)
                (l0 (xs_4188 x1_4404 (fun r1_4266 r2_4267 -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267))))
                (l1
                  (xs_4188 x1_4404
                    (fun x1_4273 x2_4274 ->
                     (xs_4188 x2_4405 (fun x1_4281 x2_4282 -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282))))))))
             (fun ysys_4191 ->
              (check_2063 ysys_4191 (fun b_4197 -> (if b_4197 (l0 end) (l1 (fail_3437 true (fun main_4039 -> end)))))))))))): X
ETA: (fun arg1_4199 ->
      (make_list_1044 arg1_4199
        (fun xs_4188 ->
         (insertsort_2064
           (fun x1_4404 x2_4405 k_main_xsxs_4406 ->
            (if (x2_4405 = x1_4404)
              (l0 (xs_4188 x1_4404 (fun r1_4266 r2_4267 -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267))))
              (l1
                (xs_4188 x1_4404
                  (fun x1_4273 x2_4274 ->
                   (xs_4188 x2_4405 (fun x1_4281 x2_4282 -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282))))))))
           (fun ysys_4191 ->
            (check_2063 ysys_4191 (fun b_4197 -> (if b_4197 (l0 end) (l1 (fail_3437 true (fun main_4039 -> end))))))))))): (int
->
unit)
ETA: (make_list_1044 arg1_4199
       (fun xs_4188 ->
        (insertsort_2064
          (fun x1_4404 x2_4405 k_main_xsxs_4406 ->
           (if (x2_4405 = x1_4404)
             (l0 (xs_4188 x1_4404 (fun r1_4266 r2_4267 -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267))))
             (l1
               (xs_4188 x1_4404
                 (fun x1_4273 x2_4274 ->
                  (xs_4188 x2_4405 (fun x1_4281 x2_4282 -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282))))))))
          (fun ysys_4191 ->
           (check_2063 ysys_4191 (fun b_4197 -> (if b_4197 (l0 end) (l1 (fail_3437 true (fun main_4039 -> end)))))))))): unit
ETA: (fun xs_4188 ->
      (insertsort_2064
        (fun x1_4404 x2_4405 k_main_xsxs_4406 ->
         (if (x2_4405 = x1_4404)
           (l0 (xs_4188 x1_4404 (fun r1_4266 r2_4267 -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267))))
           (l1
             (xs_4188 x1_4404
               (fun x1_4273 x2_4274 ->
                (xs_4188 x2_4405 (fun x1_4281 x2_4282 -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282))))))))
        (fun ysys_4191 ->
         (check_2063 ysys_4191 (fun b_4197 -> (if b_4197 (l0 end) (l1 (fail_3437 true (fun main_4039 -> end))))))))): ((
int -> (bool -> int -> X) -> X) ->
X)
ETA: (insertsort_2064
       (fun x1_4404 x2_4405 k_main_xsxs_4406 ->
        (if (x2_4405 = x1_4404)
          (l0 (xs_4188 x1_4404 (fun r1_4266 r2_4267 -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267))))
          (l1
            (xs_4188 x1_4404
              (fun x1_4273 x2_4274 ->
               (xs_4188 x2_4405 (fun x1_4281 x2_4282 -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282))))))))
       (fun ysys_4191 ->
        (check_2063 ysys_4191 (fun b_4197 -> (if b_4197 (l0 end) (l1 (fail_3437 true (fun main_4039 -> end)))))))): X
ETA: (fun ysys_4191 ->
      (check_2063 ysys_4191 (fun b_4197 -> (if b_4197 (l0 end) (l1 (fail_3437 true (fun main_4039 -> end))))))): ((
x_2:int ->
x_3:int ->
(x_5:bool -> x_6:int -> x_7:bool -> x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_6 <= x_8] -> X) -> X) ->
X)
ETA: (check_2063 ysys_4191 (fun b_4197 -> (if b_4197 (l0 end) (l1 (fail_3437 true (fun main_4039 -> end)))))): X
ETA: (fun b_4197 -> (if b_4197 (l0 end) (l1 (fail_3437 true (fun main_4039 -> end))))): (x_1:bool[
x_1] ->
X)
ETA: (if b_4197 (l0 end) (l1 (fail_3437 true (fun main_4039 -> end)))): X
ETA: (l1 (fail_3437 true (fun main_4039 -> end))): X
ETA: (fail_3437 true (fun main_4039 -> end)): X
ETA: (fun main_4039 -> end): (unit ->
X)
ETA: end: X
ETA: true: bool
ETA_AUX: (fail_3437 true (fun (main_4039:unit) -> end)): X
ETA: (l0 end): X
ETA: end: X
ETA: ysys_4191: (x_1:int ->
                 x_2:int ->
                 (x_4:bool ->
                  x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                 -> X)
ETA_AUX: ysys_4191: (x_1:int ->
                     x_2:int ->
                     (x_4:bool ->
                      x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                     -> X)
ETA_AUX: x__4422: int
ETA_AUX: (ysys_4191 x__4422): (x_1:int ->
                               (x_3:bool ->
                                x_4:int ->
                                x_5:bool -> x_6:int[(not x_3) || (not x_5) || (not (x__4422 <= x_1)) || x_4 <= x_6] ->
                                X)
                               -> X)
ETA_AUX: x__4423: int
ETA_AUX: (ysys_4191 x__4422 x__4423): ((x_2:bool ->
                                        x_3:int ->
                                        x_4:bool ->
                                        x_5:int[(not x_2) || (not x_4) || (not (x__4422 <= x__4423)) || x_3 <= x_5] ->
                                        X) ->
X)
ETA_AUX: x__4424: (x_1:bool ->
                   x_2:int ->
                   x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4422 <= x__4423)) || x_2 <= x_4] -> X)
ETA_AUX: x__4425: bool
ETA_AUX: (x__4424 x__4425): (x_1:int ->
                             x_2:bool ->
                             x_3:int[(not x__4425) || (not x_2) || (not (x__4422 <= x__4423)) || x_1 <= x_3] -> X)
ETA_AUX: x__4426: int
ETA_AUX: (x__4424 x__4425 x__4426): (x_1:bool ->
                                     x_2:int[(not x__4425) || (not x_1) || (not (x__4422 <= x__4423)) || x__4426 <= x_2]
                                     -> X)
ETA_AUX: x__4427: bool
ETA_AUX: (x__4424 x__4425 x__4426 x__4427): (x_1:int[(not x__4425) || (
                                                     not x__4427) || (
                                                     not (x__4422 <= x__4423)) ||
                                                     x__4426 <= x_1] ->
X)
ETA_AUX: x__4428: x_1:int[(not x__4425) || (not x__4427) || (not (x__4422 <= x__4423)) || x__4426 <= x_1]
ETA_AUX: (x__4424 x__4425 x__4426 x__4427 x__4428): X
ETA_AUX: (ysys_4191 x__4422 x__4423
           (fun (x__4425:bool) (x__4426:int) (x__4427:bool)
                (x__4428:x_1:int[(not x__4425) || (not x__4427) || (not (x__4422 <= x__4423)) || x__4426 <= x_1])
            -> (x__4424 x__4425 x__4426 x__4427 x__4428))): X
ETA_AUX: (check_2063
           (fun (x__4422:int) (x__4423:int)
                (x__4424:(x_1:bool ->
                          x_2:int ->
                          x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4422 <= x__4423)) || x_2 <= x_4] -> X))
            ->
            (ysys_4191 x__4422 x__4423
              (fun (x__4425:bool) (x__4426:int) (x__4427:bool)
                   (x__4428:x_1:int[(not x__4425) || (not x__4427) || (not (x__4422 <= x__4423)) || x__4426 <= x_1])
               -> (x__4424 x__4425 x__4426 x__4427 x__4428))))
           (fun (b_4197:x_1:bool[x_1]) -> (if b_4197 (l0 end) (l1 (fail_3437 true (fun (main_4039:unit) -> end)))))): X
ETA: (fun x1_4404 x2_4405 k_main_xsxs_4406 ->
      (if (x2_4405 = x1_4404)
        (l0 (xs_4188 x1_4404 (fun r1_4266 r2_4267 -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267))))
        (l1
          (xs_4188 x1_4404
            (fun x1_4273 x2_4274 ->
             (xs_4188 x2_4405 (fun x1_4281 x2_4282 -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282)))))))): (
int -> int -> (bool -> int -> bool -> int -> X) -> X)
ETA: (fun x2_4405 k_main_xsxs_4406 ->
      (if (x2_4405 = x1_4404)
        (l0 (xs_4188 x1_4404 (fun r1_4266 r2_4267 -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267))))
        (l1
          (xs_4188 x1_4404
            (fun x1_4273 x2_4274 ->
             (xs_4188 x2_4405 (fun x1_4281 x2_4282 -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282)))))))): (
int -> (bool -> int -> bool -> int -> X) -> X)
ETA: (fun k_main_xsxs_4406 ->
      (if (x2_4405 = x1_4404)
        (l0 (xs_4188 x1_4404 (fun r1_4266 r2_4267 -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267))))
        (l1
          (xs_4188 x1_4404
            (fun x1_4273 x2_4274 ->
             (xs_4188 x2_4405 (fun x1_4281 x2_4282 -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282)))))))): ((
bool -> int -> bool -> int -> X) ->
X)
ETA: (if (x2_4405 = x1_4404)
       (l0 (xs_4188 x1_4404 (fun r1_4266 r2_4267 -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267))))
       (l1
         (xs_4188 x1_4404
           (fun x1_4273 x2_4274 ->
            (xs_4188 x2_4405 (fun x1_4281 x2_4282 -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282))))))): X
ETA: (l1
       (xs_4188 x1_4404
         (fun x1_4273 x2_4274 ->
          (xs_4188 x2_4405 (fun x1_4281 x2_4282 -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282)))))): X
ETA: (xs_4188 x1_4404
       (fun x1_4273 x2_4274 ->
        (xs_4188 x2_4405 (fun x1_4281 x2_4282 -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282))))): X
ETA: (fun x1_4273 x2_4274 ->
      (xs_4188 x2_4405 (fun x1_4281 x2_4282 -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282)))): (
bool -> int -> X)
ETA: (fun x2_4274 -> (xs_4188 x2_4405 (fun x1_4281 x2_4282 -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282)))): (int
->
X)
ETA: (xs_4188 x2_4405 (fun x1_4281 x2_4282 -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282))): X
ETA: (fun x1_4281 x2_4282 -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282)): (
bool -> int -> X)
ETA: (fun x2_4282 -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282)): (int ->
X)
ETA: (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282): X
ETA: x2_4282: int
ETA: x1_4281: bool
ETA: x2_4274: int
ETA: x1_4273: bool
ETA_AUX: (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282): X
ETA: x2_4405: int
ETA_AUX: (xs_4188 x2_4405 (fun (x1_4281:bool) (x2_4282:int) -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282))): X
ETA: x1_4404: int
ETA_AUX: (xs_4188 x1_4404
           (fun (x1_4273:bool) (x2_4274:int) ->
            (xs_4188 x2_4405 (fun (x1_4281:bool) (x2_4282:int) -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282))))): X
ETA: (l0 (xs_4188 x1_4404 (fun r1_4266 r2_4267 -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267)))): X
ETA: (xs_4188 x1_4404 (fun r1_4266 r2_4267 -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267))): X
ETA: (fun r1_4266 r2_4267 -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267)): (
bool -> int -> X)
ETA: (fun r2_4267 -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267)): (int ->
X)
ETA: (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267): X
ETA: r2_4267: int
ETA: r1_4266: bool
ETA: r2_4267: int
ETA: r1_4266: bool
ETA_AUX: (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267): X
ETA: x1_4404: int
ETA_AUX: (xs_4188 x1_4404 (fun (r1_4266:bool) (r2_4267:int) -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267))): X
ETA_AUX: (insertsort_2064
           (fun (x1_4404:int) (x2_4405:int) (k_main_xsxs_4406:(bool -> int -> bool -> int -> X)) ->
            (if (x2_4405 = x1_4404)
              (l0
                (xs_4188 x1_4404
                  (fun (r1_4266:bool) (r2_4267:int) -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267))))
              (l1
                (xs_4188 x1_4404
                  (fun (x1_4273:bool) (x2_4274:int) ->
                   (xs_4188 x2_4405
                     (fun (x1_4281:bool) (x2_4282:int) -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282))))))))
           (fun (ysys_4191:(x_1:int ->
                            x_2:int ->
                            (x_4:bool ->
                             x_5:int ->
                             x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                            -> X))
            ->
            (check_2063
              (fun (x__4422:int) (x__4423:int)
                   (x__4424:(x_1:bool ->
                             x_2:int ->
                             x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4422 <= x__4423)) || x_2 <= x_4] ->
                             X))
               ->
               (ysys_4191 x__4422 x__4423
                 (fun (x__4425:bool) (x__4426:int) (x__4427:bool)
                      (x__4428:x_1:int[(not x__4425) || (not x__4427) || (not (x__4422 <= x__4423)) || x__4426 <= x_1])
                  -> (x__4424 x__4425 x__4426 x__4427 x__4428))))
              (fun (b_4197:x_1:bool[x_1]) -> (if b_4197 (l0 end) (l1 (fail_3437 true (fun (main_4039:unit) -> end)))))))): X
ETA: arg1_4199: int
ETA_AUX: (make_list_1044 arg1_4199
           (fun (xs_4188:(int -> (bool -> int -> X) -> X)) ->
            (insertsort_2064
              (fun (x1_4404:int) (x2_4405:int) (k_main_xsxs_4406:(bool -> int -> bool -> int -> X)) ->
               (if (x2_4405 = x1_4404)
                 (l0
                   (xs_4188 x1_4404
                     (fun (r1_4266:bool) (r2_4267:int) -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267))))
                 (l1
                   (xs_4188 x1_4404
                     (fun (x1_4273:bool) (x2_4274:int) ->
                      (xs_4188 x2_4405
                        (fun (x1_4281:bool) (x2_4282:int) -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282))))))))
              (fun (ysys_4191:(x_1:int ->
                               x_2:int ->
                               (x_4:bool ->
                                x_5:int ->
                                x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                               -> X))
               ->
               (check_2063
                 (fun (x__4422:int) (x__4423:int)
                      (x__4424:(x_1:bool ->
                                x_2:int ->
                                x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4422 <= x__4423)) || x_2 <= x_4]
                                -> X))
                  ->
                  (ysys_4191 x__4422 x__4423
                    (fun (x__4425:bool) (x__4426:int) (x__4427:bool)
                         (x__4428:x_1:int[(not x__4425) || (not x__4427) || (
                                          not (x__4422 <= x__4423)) ||
                                          x__4426 <= x_1])
                     -> (x__4424 x__4425 x__4426 x__4427 x__4428))))
                 (fun (b_4197:x_1:bool[x_1]) ->
                  (if b_4197 (l0 end) (l1 (fail_3437 true (fun (main_4039:unit) -> end)))))))))): unit
ETA_AUX: (rand_int
           (fun (arg1_4199:int) ->
            (make_list_1044 arg1_4199
              (fun (xs_4188:(int -> (bool -> int -> X) -> X)) ->
               (insertsort_2064
                 (fun (x1_4404:int) (x2_4405:int) (k_main_xsxs_4406:(bool -> int -> bool -> int -> X)) ->
                  (if (x2_4405 = x1_4404)
                    (l0
                      (xs_4188 x1_4404
                        (fun (r1_4266:bool) (r2_4267:int) -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267))))
                    (l1
                      (xs_4188 x1_4404
                        (fun (x1_4273:bool) (x2_4274:int) ->
                         (xs_4188 x2_4405
                           (fun (x1_4281:bool) (x2_4282:int) -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282))))))))
                 (fun (ysys_4191:(x_1:int ->
                                  x_2:int ->
                                  (x_4:bool ->
                                   x_5:int ->
                                   x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                                  -> X))
                  ->
                  (check_2063
                    (fun (x__4422:int) (x__4423:int)
                         (x__4424:(x_1:bool ->
                                   x_2:int ->
                                   x_3:bool ->
                                   x_4:int[(not x_1) || (not x_3) || (not (x__4422 <= x__4423)) || x_2 <= x_4] -> X))
                     ->
                     (ysys_4191 x__4422 x__4423
                       (fun (x__4425:bool) (x__4426:int) (x__4427:bool)
                            (x__4428:x_1:int[(not x__4425) || (not x__4427) || (
                                             not (x__4422 <= x__4423)) ||
                                             x__4426 <= x_1])
                        -> (x__4424 x__4425 x__4426 x__4427 x__4428))))
                    (fun (b_4197:x_1:bool[x_1]) ->
                     (if b_4197 (l0 end) (l1 (fail_3437 true (fun (main_4039:unit) -> end)))))))))))): X
ETA: (xsxs_1051 0 0
       (fun xs11_4048 xs12_4049 xs21_4050 xs22_4051 ->
        (if xs11_4048
          (l1
            (if xs21_4050
              (l1
                (if (xs22_4051 >= xs12_4049)
                  (l0
                    (check_2063
                      (fun x1_4359 x2_4360 k_check_xs'xs'_4361 ->
                       (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1) k_check_xs'xs'_4361)) k_check_2886))
                  (l1 (k_check_2886 false)))) (l0 (k_check_2886 true)))) (
          l0 (k_check_2886 true))))): X
ETA: (fun xs11_4048 xs12_4049 xs21_4050 xs22_4051 ->
      (if xs11_4048
        (l1
          (if xs21_4050
            (l1
              (if (xs22_4051 >= xs12_4049)
                (l0
                  (check_2063
                    (fun x1_4359 x2_4360 k_check_xs'xs'_4361 ->
                     (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1) k_check_xs'xs'_4361)) k_check_2886))
                (l1 (k_check_2886 false)))) (l0 (k_check_2886 true)))) (
        l0 (k_check_2886 true)))): (x_1:bool ->
                                    x_2:int ->
                                    x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (0 <= 0)) || x_2 <= x_4] -> X)
ETA: (fun xs12_4049 xs21_4050 xs22_4051 ->
      (if xs11_4048
        (l1
          (if xs21_4050
            (l1
              (if (xs22_4051 >= xs12_4049)
                (l0
                  (check_2063
                    (fun x1_4359 x2_4360 k_check_xs'xs'_4361 ->
                     (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1) k_check_xs'xs'_4361)) k_check_2886))
                (l1 (k_check_2886 false)))) (l0 (k_check_2886 true)))) (
        l0 (k_check_2886 true)))): (x_1:int ->
                                    x_2:bool -> x_3:int[(not xs11_4048) || (not x_2) || (not (0 <= 0)) || x_1 <= x_3]
                                    -> X)
ETA: (fun xs21_4050 xs22_4051 ->
      (if xs11_4048
        (l1
          (if xs21_4050
            (l1
              (if (xs22_4051 >= xs12_4049)
                (l0
                  (check_2063
                    (fun x1_4359 x2_4360 k_check_xs'xs'_4361 ->
                     (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1) k_check_xs'xs'_4361)) k_check_2886))
                (l1 (k_check_2886 false)))) (l0 (k_check_2886 true)))) (
        l0 (k_check_2886 true)))): (x_1:bool ->
                                    x_2:int[(not xs11_4048) || (not x_1) || (not (0 <= 0)) || xs12_4049 <= x_2] -> X)
ETA: (fun xs22_4051 ->
      (if xs11_4048
        (l1
          (if xs21_4050
            (l1
              (if (xs22_4051 >= xs12_4049)
                (l0
                  (check_2063
                    (fun x1_4359 x2_4360 k_check_xs'xs'_4361 ->
                     (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1) k_check_xs'xs'_4361)) k_check_2886))
                (l1 (k_check_2886 false)))) (l0 (k_check_2886 true)))) (
        l0 (k_check_2886 true)))): (x_1:int[(not xs11_4048) || (not xs21_4050) || (not (0 <= 0)) || xs12_4049 <= x_1]
->
X)
ETA: (if xs11_4048
       (l1
         (if xs21_4050
           (l1
             (if (xs22_4051 >= xs12_4049)
               (l0
                 (check_2063
                   (fun x1_4359 x2_4360 k_check_xs'xs'_4361 ->
                    (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1) k_check_xs'xs'_4361)) k_check_2886))
               (l1 (k_check_2886 false)))) (l0 (k_check_2886 true)))) (
       l0 (k_check_2886 true))): X
ETA: (l0 (k_check_2886 true)): X
ETA: (k_check_2886 true): X
ETA: true: x_1:bool[x_1]
ETA_AUX: (k_check_2886 true): X
ETA: (l1
       (if xs21_4050
         (l1
           (if (xs22_4051 >= xs12_4049)
             (l0
               (check_2063
                 (fun x1_4359 x2_4360 k_check_xs'xs'_4361 ->
                  (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1) k_check_xs'xs'_4361)) k_check_2886)) (
             l1 (k_check_2886 false)))) (l0 (k_check_2886 true)))): X
ETA: (if xs21_4050
       (l1
         (if (xs22_4051 >= xs12_4049)
           (l0
             (check_2063
               (fun x1_4359 x2_4360 k_check_xs'xs'_4361 -> (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1) k_check_xs'xs'_4361))
               k_check_2886)) (l1 (k_check_2886 false)))) (l0 (k_check_2886 true))): X
ETA: (l0 (k_check_2886 true)): X
ETA: (k_check_2886 true): X
ETA: true: x_1:bool[x_1]
ETA_AUX: (k_check_2886 true): X
ETA: (l1
       (if (xs22_4051 >= xs12_4049)
         (l0
           (check_2063
             (fun x1_4359 x2_4360 k_check_xs'xs'_4361 -> (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1) k_check_xs'xs'_4361))
             k_check_2886)) (l1 (k_check_2886 false)))): X
ETA: (if (xs22_4051 >= xs12_4049)
       (l0
         (check_2063
           (fun x1_4359 x2_4360 k_check_xs'xs'_4361 -> (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1) k_check_xs'xs'_4361))
           k_check_2886)) (l1 (k_check_2886 false))): X
ETA: (l1 (k_check_2886 false)): X
ETA: (k_check_2886 false): X
ETA: false: x_1:bool[x_1]
ETA_AUX: (k_check_2886 false): X
ETA: (l0
       (check_2063
         (fun x1_4359 x2_4360 k_check_xs'xs'_4361 -> (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1) k_check_xs'xs'_4361))
         k_check_2886)): X
ETA: (check_2063
       (fun x1_4359 x2_4360 k_check_xs'xs'_4361 -> (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1) k_check_xs'xs'_4361))
       k_check_2886): X
ETA: k_check_2886: (x_1:bool[x_1] ->
X)
ETA_AUX: k_check_2886: (x_1:bool[x_1] ->
X)
ETA_AUX: x__4429: x_1:bool[x_1]
ETA_AUX: (k_check_2886 x__4429): X
ETA: (fun x1_4359 x2_4360 k_check_xs'xs'_4361 -> (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1) k_check_xs'xs'_4361)): (
x_1:int ->
x_2:int ->
(x_4:bool -> x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X) -> X)
ETA: (fun x2_4360 k_check_xs'xs'_4361 -> (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1) k_check_xs'xs'_4361)): (
x_1:int ->
(x_3:bool -> x_4:int -> x_5:bool -> x_6:int[(not x_3) || (not x_5) || (not (x1_4359 <= x_1)) || x_4 <= x_6] -> X) -> X)
ETA: (fun k_check_xs'xs'_4361 -> (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1) k_check_xs'xs'_4361)): ((
x_2:bool -> x_3:int -> x_4:bool -> x_5:int[(not x_2) || (not x_4) || (not (x1_4359 <= x2_4360)) || x_3 <= x_5] -> X) ->
X)
ETA: (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1) k_check_xs'xs'_4361): X
ETA: k_check_xs'xs'_4361: (x_1:bool ->
                           x_2:int ->
                           x_3:bool ->
                           x_4:int[(not x_1) || (not x_3) || (not ((x1_4359 + 1) <= (x2_4360 + 1))) || x_2 <= x_4] -> X)
ETA_AUX: k_check_xs'xs'_4361: (x_1:bool ->
                               x_2:int ->
                               x_3:bool ->
                               x_4:int[(not x_1) || (not x_3) || (not ((x1_4359 + 1) <= (x2_4360 + 1))) || x_2 <= x_4]
                               -> X)
ETA_AUX: x__4430: bool
ETA_AUX: (k_check_xs'xs'_4361 x__4430): (x_1:int ->
                                         x_2:bool ->
                                         x_3:int[(not x__4430) || (not x_2) ||
                                                 (not ((x1_4359 + 1) <= (x2_4360 + 1))) ||
                                                 x_1 <= x_3]
                                         -> X)
ETA_AUX: x__4431: int
ETA_AUX: (k_check_xs'xs'_4361 x__4430 x__4431): (x_1:bool ->
                                                 x_2:int[(not x__4430) || (
                                                         not x_1) || (
                                                         not ((x1_4359 + 1) <= (x2_4360 + 1))) ||
                                                         x__4431 <= x_2]
                                                 -> X)
ETA_AUX: x__4432: bool
ETA_AUX: (k_check_xs'xs'_4361 x__4430 x__4431 x__4432): (x_1:int[(not x__4430) || (
                                                                 not x__4432) ||
                                                                 (not ((x1_4359 + 1) <= (x2_4360 + 1))) ||
                                                                 x__4431 <= x_1] ->
X)
ETA_AUX: x__4433: x_1:int[(not x__4430) || (not x__4432) || (not (x1_4359 <= x2_4360)) || x__4431 <= x_1]
ETA_AUX: (k_check_xs'xs'_4361 x__4430 x__4431 x__4432 x__4433): X
ETA: (x2_4360 + 1): int
ETA: (x1_4359 + 1): int
ETA_AUX: (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1)
           (fun (x__4430:bool) (x__4431:int) (x__4432:bool)
                (x__4433:x_1:int[(not x__4430) || (not x__4432) || (not ((x1_4359 + 1) <= (x2_4360 + 1))) ||
                                 x__4431 <= x_1])
            -> (k_check_xs'xs'_4361 x__4430 x__4431 x__4432 x__4433))): X
ETA_AUX: (check_2063
           (fun (x1_4359:int) (x2_4360:int)
                (k_check_xs'xs'_4361:(x_1:bool ->
                                      x_2:int ->
                                      x_3:bool ->
                                      x_4:int[(not x_1) || (not x_3) || (not (x1_4359 <= x2_4360)) || x_2 <= x_4] -> X))
            ->
            (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1)
              (fun (x__4430:bool) (x__4431:int) (x__4432:bool)
                   (x__4433:x_1:int[(not x__4430) || (not x__4432) || (
                                    not ((x1_4359 + 1) <= (x2_4360 + 1))) ||
                                    x__4431 <= x_1])
               -> (k_check_xs'xs'_4361 x__4430 x__4431 x__4432 x__4433))))
           (fun (x__4429:x_1:bool[x_1]) -> (k_check_2886 x__4429))): X
ETA: 0: int
ETA: 0: int
ETA_AUX: (xsxs_1051 0 0
           (fun (xs11_4048:bool) (xs12_4049:int) (xs21_4050:bool)
                (xs22_4051:x_1:int[(not xs11_4048) || (not xs21_4050) || (not (0 <= 0)) || xs12_4049 <= x_1])
            ->
            (if xs11_4048
              (l1
                (if xs21_4050
                  (l1
                    (if (xs22_4051 >= xs12_4049)
                      (l0
                        (check_2063
                          (fun (x1_4359:int) (x2_4360:int)
                               (k_check_xs'xs'_4361:(x_1:bool ->
                                                     x_2:int ->
                                                     x_3:bool ->
                                                     x_4:int[(not x_1) || (
                                                             not x_3) || (
                                                             not (x1_4359 <= x2_4360)) ||
                                                             x_2 <= x_4]
                                                     -> X))
                           ->
                           (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1)
                             (fun (x__4430:bool) (x__4431:int) (x__4432:bool)
                                  (x__4433:x_1:int[(not x__4430) || (
                                                   not x__4432) || (not ((x1_4359 + 1) <= (x2_4360 + 1))) ||
                                                   x__4431 <= x_1])
                              -> (k_check_xs'xs'_4361 x__4430 x__4431 x__4432 x__4433))))
                          (fun (x__4429:x_1:bool[x_1]) -> (k_check_2886 x__4429)))) (
                      l1 (k_check_2886 false)))) (l0 (k_check_2886 true)))) (
              l0 (k_check_2886 true))))): X
ETA: (k ()): X
ETA: (): unit
ETA_AUX: (k ()): X
ETA: (ysys_1015 0 0
       (fun p11_4248 p12_4249 p21_4250 p22_4251 ->
        (if p11_4248
          (l1
            (if (x_1050 < p12_4249)
              (l0
                (k_insert_2134
                  (fun x1_4073 x2_4074 k_insert_4075 ->
                   (if (x1_4073 <= 0)
                     (l0
                       (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
                         (l1
                           (ysys_1015 0 (x2_4074 - 1)
                             (fun xs11_4092 xs12_4093 xs21_4094 xs22_4095 ->
                              (if (p11_4248 <=> xs11_4092)
                                (l1
                                  (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                                    (l0
                                      (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092
                                        xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 () k_insert_4075))))
                                (l0
                                  (l0
                                    (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094
                                      xs22_4095 p12_4249 () k_insert_4075)))))))))
                     (l1
                       (if (x2_4074 <= 0)
                         (l0
                           (ysys_1015 (x1_4073 - 1) 0
                             (fun p11_4102 p12_4103 p21_4104 p22_4105 -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
                         (l1 (ysys_1015 (x1_4073 - 1) (x2_4074 - 1) k_insert_4075))))))))
              (l1
                (insert_1013 x_1050
                  (fun x1_4416 x2_4417 k_insert_ys'ys'_4418 ->
                   (ysys_1015 (x1_4416 + 1) (x2_4417 + 1) k_insert_ys'ys'_4418))
                  (fun ys''ys''_4111 ->
                   (k_insert_2134
                     (fun x1_4125 x2_4126 k_insert_4127 ->
                      (if (x1_4125 = 0)
                        (l0
                          (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                            (l1
                              (ysys_1015 0 (x2_4126 - 1)
                                (fun p11_4134 p12_4135 p21_4136 p22_4137 ->
                                 (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
                        (l1
                          (if (x2_4126 = 0)
                            (l0
                              (ys''ys''_4111 x1_4125 0
                                (fun p11_4144 p12_4145 p21_4146 p22_4147 ->
                                 (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                            (l1 (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1) k_insert_4127))))))))))))
          (l0
            (k_insert_2134
              (fun x1_4343 x2_4344 k_insert_rsrs_4345 ->
               (if (x2_4344 = x1_4343)
                 (l0
                   (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                     (l1 (k_insert_rsrs_4345 false 0 false 0))))
                 (l1
                   (if (x1_4343 = 0)
                     (l0
                       (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                         (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
                     (l1
                       (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050))
                         (l1 (k_insert_rsrs_4345 false 0 false 0))))))))))))): X
ETA: (fun p11_4248 p12_4249 p21_4250 p22_4251 ->
      (if p11_4248
        (l1
          (if (x_1050 < p12_4249)
            (l0
              (k_insert_2134
                (fun x1_4073 x2_4074 k_insert_4075 ->
                 (if (x1_4073 <= 0)
                   (l0
                     (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
                       (l1
                         (ysys_1015 0 (x2_4074 - 1)
                           (fun xs11_4092 xs12_4093 xs21_4094 xs22_4095 ->
                            (if (p11_4248 <=> xs11_4092)
                              (l1
                                (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                                  (l0
                                    (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093
                                      p11_4248 xs21_4094 xs22_4095 p12_4249 () k_insert_4075))))
                              (l0
                                (l0
                                  (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094
                                    xs22_4095 p12_4249 () k_insert_4075)))))))))
                   (l1
                     (if (x2_4074 <= 0)
                       (l0
                         (ysys_1015 (x1_4073 - 1) 0
                           (fun p11_4102 p12_4103 p21_4104 p22_4105 -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
                       (l1 (ysys_1015 (x1_4073 - 1) (x2_4074 - 1) k_insert_4075))))))))
            (l1
              (insert_1013 x_1050
                (fun x1_4416 x2_4417 k_insert_ys'ys'_4418 ->
                 (ysys_1015 (x1_4416 + 1) (x2_4417 + 1) k_insert_ys'ys'_4418))
                (fun ys''ys''_4111 ->
                 (k_insert_2134
                   (fun x1_4125 x2_4126 k_insert_4127 ->
                    (if (x1_4125 = 0)
                      (l0
                        (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                          (l1
                            (ysys_1015 0 (x2_4126 - 1)
                              (fun p11_4134 p12_4135 p21_4136 p22_4137 ->
                               (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
                      (l1
                        (if (x2_4126 = 0)
                          (l0
                            (ys''ys''_4111 x1_4125 0
                              (fun p11_4144 p12_4145 p21_4146 p22_4147 ->
                               (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                          (l1 (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1) k_insert_4127))))))))))))
        (l0
          (k_insert_2134
            (fun x1_4343 x2_4344 k_insert_rsrs_4345 ->
             (if (x2_4344 = x1_4343)
               (l0
                 (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                   (l1 (k_insert_rsrs_4345 false 0 false 0))))
               (l1
                 (if (x1_4343 = 0)
                   (l0
                     (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                       (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
                   (l1
                     (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050))
                       (l1 (k_insert_rsrs_4345 false 0 false 0)))))))))))): (
x_1:bool -> x_2:int -> x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (0 <= 0)) || x_2 <= x_4] -> X)
ETA: (fun p12_4249 p21_4250 p22_4251 ->
      (if p11_4248
        (l1
          (if (x_1050 < p12_4249)
            (l0
              (k_insert_2134
                (fun x1_4073 x2_4074 k_insert_4075 ->
                 (if (x1_4073 <= 0)
                   (l0
                     (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
                       (l1
                         (ysys_1015 0 (x2_4074 - 1)
                           (fun xs11_4092 xs12_4093 xs21_4094 xs22_4095 ->
                            (if (p11_4248 <=> xs11_4092)
                              (l1
                                (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                                  (l0
                                    (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093
                                      p11_4248 xs21_4094 xs22_4095 p12_4249 () k_insert_4075))))
                              (l0
                                (l0
                                  (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094
                                    xs22_4095 p12_4249 () k_insert_4075)))))))))
                   (l1
                     (if (x2_4074 <= 0)
                       (l0
                         (ysys_1015 (x1_4073 - 1) 0
                           (fun p11_4102 p12_4103 p21_4104 p22_4105 -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
                       (l1 (ysys_1015 (x1_4073 - 1) (x2_4074 - 1) k_insert_4075))))))))
            (l1
              (insert_1013 x_1050
                (fun x1_4416 x2_4417 k_insert_ys'ys'_4418 ->
                 (ysys_1015 (x1_4416 + 1) (x2_4417 + 1) k_insert_ys'ys'_4418))
                (fun ys''ys''_4111 ->
                 (k_insert_2134
                   (fun x1_4125 x2_4126 k_insert_4127 ->
                    (if (x1_4125 = 0)
                      (l0
                        (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                          (l1
                            (ysys_1015 0 (x2_4126 - 1)
                              (fun p11_4134 p12_4135 p21_4136 p22_4137 ->
                               (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
                      (l1
                        (if (x2_4126 = 0)
                          (l0
                            (ys''ys''_4111 x1_4125 0
                              (fun p11_4144 p12_4145 p21_4146 p22_4147 ->
                               (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                          (l1 (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1) k_insert_4127))))))))))))
        (l0
          (k_insert_2134
            (fun x1_4343 x2_4344 k_insert_rsrs_4345 ->
             (if (x2_4344 = x1_4343)
               (l0
                 (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                   (l1 (k_insert_rsrs_4345 false 0 false 0))))
               (l1
                 (if (x1_4343 = 0)
                   (l0
                     (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                       (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
                   (l1
                     (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050))
                       (l1 (k_insert_rsrs_4345 false 0 false 0)))))))))))): (
x_1:int -> x_2:bool -> x_3:int[(not p11_4248) || (not x_2) || (not (0 <= 0)) || x_1 <= x_3] -> X)
ETA: (fun p21_4250 p22_4251 ->
      (if p11_4248
        (l1
          (if (x_1050 < p12_4249)
            (l0
              (k_insert_2134
                (fun x1_4073 x2_4074 k_insert_4075 ->
                 (if (x1_4073 <= 0)
                   (l0
                     (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
                       (l1
                         (ysys_1015 0 (x2_4074 - 1)
                           (fun xs11_4092 xs12_4093 xs21_4094 xs22_4095 ->
                            (if (p11_4248 <=> xs11_4092)
                              (l1
                                (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                                  (l0
                                    (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093
                                      p11_4248 xs21_4094 xs22_4095 p12_4249 () k_insert_4075))))
                              (l0
                                (l0
                                  (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094
                                    xs22_4095 p12_4249 () k_insert_4075)))))))))
                   (l1
                     (if (x2_4074 <= 0)
                       (l0
                         (ysys_1015 (x1_4073 - 1) 0
                           (fun p11_4102 p12_4103 p21_4104 p22_4105 -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
                       (l1 (ysys_1015 (x1_4073 - 1) (x2_4074 - 1) k_insert_4075))))))))
            (l1
              (insert_1013 x_1050
                (fun x1_4416 x2_4417 k_insert_ys'ys'_4418 ->
                 (ysys_1015 (x1_4416 + 1) (x2_4417 + 1) k_insert_ys'ys'_4418))
                (fun ys''ys''_4111 ->
                 (k_insert_2134
                   (fun x1_4125 x2_4126 k_insert_4127 ->
                    (if (x1_4125 = 0)
                      (l0
                        (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                          (l1
                            (ysys_1015 0 (x2_4126 - 1)
                              (fun p11_4134 p12_4135 p21_4136 p22_4137 ->
                               (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
                      (l1
                        (if (x2_4126 = 0)
                          (l0
                            (ys''ys''_4111 x1_4125 0
                              (fun p11_4144 p12_4145 p21_4146 p22_4147 ->
                               (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                          (l1 (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1) k_insert_4127))))))))))))
        (l0
          (k_insert_2134
            (fun x1_4343 x2_4344 k_insert_rsrs_4345 ->
             (if (x2_4344 = x1_4343)
               (l0
                 (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                   (l1 (k_insert_rsrs_4345 false 0 false 0))))
               (l1
                 (if (x1_4343 = 0)
                   (l0
                     (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                       (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
                   (l1
                     (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050))
                       (l1 (k_insert_rsrs_4345 false 0 false 0)))))))))))): (
x_1:bool -> x_2:int[(not p11_4248) || (not x_1) || (not (0 <= 0)) || p12_4249 <= x_2] -> X)
ETA: (fun p22_4251 ->
      (if p11_4248
        (l1
          (if (x_1050 < p12_4249)
            (l0
              (k_insert_2134
                (fun x1_4073 x2_4074 k_insert_4075 ->
                 (if (x1_4073 <= 0)
                   (l0
                     (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
                       (l1
                         (ysys_1015 0 (x2_4074 - 1)
                           (fun xs11_4092 xs12_4093 xs21_4094 xs22_4095 ->
                            (if (p11_4248 <=> xs11_4092)
                              (l1
                                (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                                  (l0
                                    (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093
                                      p11_4248 xs21_4094 xs22_4095 p12_4249 () k_insert_4075))))
                              (l0
                                (l0
                                  (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094
                                    xs22_4095 p12_4249 () k_insert_4075)))))))))
                   (l1
                     (if (x2_4074 <= 0)
                       (l0
                         (ysys_1015 (x1_4073 - 1) 0
                           (fun p11_4102 p12_4103 p21_4104 p22_4105 -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
                       (l1 (ysys_1015 (x1_4073 - 1) (x2_4074 - 1) k_insert_4075))))))))
            (l1
              (insert_1013 x_1050
                (fun x1_4416 x2_4417 k_insert_ys'ys'_4418 ->
                 (ysys_1015 (x1_4416 + 1) (x2_4417 + 1) k_insert_ys'ys'_4418))
                (fun ys''ys''_4111 ->
                 (k_insert_2134
                   (fun x1_4125 x2_4126 k_insert_4127 ->
                    (if (x1_4125 = 0)
                      (l0
                        (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                          (l1
                            (ysys_1015 0 (x2_4126 - 1)
                              (fun p11_4134 p12_4135 p21_4136 p22_4137 ->
                               (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
                      (l1
                        (if (x2_4126 = 0)
                          (l0
                            (ys''ys''_4111 x1_4125 0
                              (fun p11_4144 p12_4145 p21_4146 p22_4147 ->
                               (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                          (l1 (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1) k_insert_4127))))))))))))
        (l0
          (k_insert_2134
            (fun x1_4343 x2_4344 k_insert_rsrs_4345 ->
             (if (x2_4344 = x1_4343)
               (l0
                 (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                   (l1 (k_insert_rsrs_4345 false 0 false 0))))
               (l1
                 (if (x1_4343 = 0)
                   (l0
                     (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                       (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
                   (l1
                     (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050))
                       (l1 (k_insert_rsrs_4345 false 0 false 0)))))))))))): (x_1:int[
(not p11_4248) || (not p21_4250) || (not (0 <= 0)) || p12_4249 <= x_1] ->
X)
ETA: (if p11_4248
       (l1
         (if (x_1050 < p12_4249)
           (l0
             (k_insert_2134
               (fun x1_4073 x2_4074 k_insert_4075 ->
                (if (x1_4073 <= 0)
                  (l0
                    (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
                      (l1
                        (ysys_1015 0 (x2_4074 - 1)
                          (fun xs11_4092 xs12_4093 xs21_4094 xs22_4095 ->
                           (if (p11_4248 <=> xs11_4092)
                             (l1
                               (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                                 (l0
                                   (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093
                                     p11_4248 xs21_4094 xs22_4095 p12_4249 () k_insert_4075))))
                             (l0
                               (l0
                                 (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094
                                   xs22_4095 p12_4249 () k_insert_4075)))))))))
                  (l1
                    (if (x2_4074 <= 0)
                      (l0
                        (ysys_1015 (x1_4073 - 1) 0
                          (fun p11_4102 p12_4103 p21_4104 p22_4105 -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
                      (l1 (ysys_1015 (x1_4073 - 1) (x2_4074 - 1) k_insert_4075))))))))
           (l1
             (insert_1013 x_1050
               (fun x1_4416 x2_4417 k_insert_ys'ys'_4418 ->
                (ysys_1015 (x1_4416 + 1) (x2_4417 + 1) k_insert_ys'ys'_4418))
               (fun ys''ys''_4111 ->
                (k_insert_2134
                  (fun x1_4125 x2_4126 k_insert_4127 ->
                   (if (x1_4125 = 0)
                     (l0
                       (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                         (l1
                           (ysys_1015 0 (x2_4126 - 1)
                             (fun p11_4134 p12_4135 p21_4136 p22_4137 ->
                              (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
                     (l1
                       (if (x2_4126 = 0)
                         (l0
                           (ys''ys''_4111 x1_4125 0
                             (fun p11_4144 p12_4145 p21_4146 p22_4147 ->
                              (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                         (l1 (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1) k_insert_4127))))))))))))
       (l0
         (k_insert_2134
           (fun x1_4343 x2_4344 k_insert_rsrs_4345 ->
            (if (x2_4344 = x1_4343)
              (l0
                (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                  (l1 (k_insert_rsrs_4345 false 0 false 0))))
              (l1
                (if (x1_4343 = 0)
                  (l0
                    (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                      (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
                  (l1
                    (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050))
                      (l1 (k_insert_rsrs_4345 false 0 false 0))))))))))): X
ETA: (l0
       (k_insert_2134
         (fun x1_4343 x2_4344 k_insert_rsrs_4345 ->
          (if (x2_4344 = x1_4343)
            (l0
              (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                (l1 (k_insert_rsrs_4345 false 0 false 0))))
            (l1
              (if (x1_4343 = 0)
                (l0
                  (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                    (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
                (l1
                  (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050))
                    (l1 (k_insert_rsrs_4345 false 0 false 0)))))))))): X
ETA: (k_insert_2134
       (fun x1_4343 x2_4344 k_insert_rsrs_4345 ->
        (if (x2_4344 = x1_4343)
          (l0
            (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
              (l1 (k_insert_rsrs_4345 false 0 false 0))))
          (l1
            (if (x1_4343 = 0)
              (l0
                (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                  (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
              (l1
                (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050))
                  (l1 (k_insert_rsrs_4345 false 0 false 0))))))))): X
ETA: (fun x1_4343 x2_4344 k_insert_rsrs_4345 ->
      (if (x2_4344 = x1_4343)
        (l0
          (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050)) (l1 (k_insert_rsrs_4345 false 0 false 0))))
        (l1
          (if (x1_4343 = 0)
            (l0
              (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
            (l1
              (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050)) (l1 (k_insert_rsrs_4345 false 0 false 0)))))))): (
x_1:int ->
x_2:int ->
(x_4:bool -> x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X) -> X)
ETA: (fun x2_4344 k_insert_rsrs_4345 ->
      (if (x2_4344 = x1_4343)
        (l0
          (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050)) (l1 (k_insert_rsrs_4345 false 0 false 0))))
        (l1
          (if (x1_4343 = 0)
            (l0
              (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
            (l1
              (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050)) (l1 (k_insert_rsrs_4345 false 0 false 0)))))))): (
x_1:int ->
(x_3:bool -> x_4:int -> x_5:bool -> x_6:int[(not x_3) || (not x_5) || (not (x1_4343 <= x_1)) || x_4 <= x_6] -> X) -> X)
ETA: (fun k_insert_rsrs_4345 ->
      (if (x2_4344 = x1_4343)
        (l0
          (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050)) (l1 (k_insert_rsrs_4345 false 0 false 0))))
        (l1
          (if (x1_4343 = 0)
            (l0
              (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
            (l1
              (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050)) (l1 (k_insert_rsrs_4345 false 0 false 0)))))))): ((
x_2:bool -> x_3:int -> x_4:bool -> x_5:int[(not x_2) || (not x_4) || (not (x1_4343 <= x2_4344)) || x_3 <= x_5] -> X) ->
X)
ETA: (if (x2_4344 = x1_4343)
       (l0
         (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050)) (l1 (k_insert_rsrs_4345 false 0 false 0))))
       (l1
         (if (x1_4343 = 0)
           (l0
             (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
               (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
           (l1
             (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050)) (l1 (k_insert_rsrs_4345 false 0 false 0))))))): X
ETA: (l1
       (if (x1_4343 = 0)
         (l0
           (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
             (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
         (l1 (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050)) (l1 (k_insert_rsrs_4345 false 0 false 0)))))): X
ETA: (if (x1_4343 = 0)
       (l0
         (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
           (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
       (l1 (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050)) (l1 (k_insert_rsrs_4345 false 0 false 0))))): X
ETA: (l1 (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050)) (l1 (k_insert_rsrs_4345 false 0 false 0)))): X
ETA: (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050)) (l1 (k_insert_rsrs_4345 false 0 false 0))): X
ETA: (l1 (k_insert_rsrs_4345 false 0 false 0)): X
ETA: (k_insert_rsrs_4345 false 0 false 0): X
ETA: 0: x_1:int[(not false) || (not false) || (not (x1_4343 <= x2_4344)) || 0 <= x_1]
ETA: false: bool
ETA: 0: int
ETA: false: bool
ETA_AUX: (k_insert_rsrs_4345 false 0 false 0): X
ETA: (l0 (k_insert_rsrs_4345 false 0 true x_1050)): X
ETA: (k_insert_rsrs_4345 false 0 true x_1050): X
ETA: x_1050: x_1:int[(not false) || (not true) || (not (x1_4343 <= x2_4344)) || 0 <= x_1]
ETA: true: bool
ETA: 0: int
ETA: false: bool
ETA_AUX: (k_insert_rsrs_4345 false 0 true x_1050): X
ETA: (l0
       (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
         (l1 (k_insert_rsrs_4345 true x_1050 false 0)))): X
ETA: (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050)) (l1 (k_insert_rsrs_4345 true x_1050 false 0))): X
ETA: (l1 (k_insert_rsrs_4345 true x_1050 false 0)): X
ETA: (k_insert_rsrs_4345 true x_1050 false 0): X
ETA: 0: x_1:int[(not true) || (not false) || (not (x1_4343 <= x2_4344)) || x_1050 <= x_1]
ETA: false: bool
ETA: x_1050: int
ETA: true: bool
ETA_AUX: (k_insert_rsrs_4345 true x_1050 false 0): X
ETA: (l0 (k_insert_rsrs_4345 true x_1050 true x_1050)): X
ETA: (k_insert_rsrs_4345 true x_1050 true x_1050): X
ETA: x_1050: x_1:int[(not true) || (not true) || (not (x1_4343 <= x2_4344)) || x_1050 <= x_1]
ETA: true: bool
ETA: x_1050: int
ETA: true: bool
ETA_AUX: (k_insert_rsrs_4345 true x_1050 true x_1050): X
ETA: (l0 (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050)) (l1 (k_insert_rsrs_4345 false 0 false 0)))): X
ETA: (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050)) (l1 (k_insert_rsrs_4345 false 0 false 0))): X
ETA: (l1 (k_insert_rsrs_4345 false 0 false 0)): X
ETA: (k_insert_rsrs_4345 false 0 false 0): X
ETA: 0: x_1:int[(not false) || (not false) || (not (x1_4343 <= x2_4344)) || 0 <= x_1]
ETA: false: bool
ETA: 0: int
ETA: false: bool
ETA_AUX: (k_insert_rsrs_4345 false 0 false 0): X
ETA: (l0 (k_insert_rsrs_4345 true x_1050 true x_1050)): X
ETA: (k_insert_rsrs_4345 true x_1050 true x_1050): X
ETA: x_1050: x_1:int[(not true) || (not true) || (not (x1_4343 <= x2_4344)) || x_1050 <= x_1]
ETA: true: bool
ETA: x_1050: int
ETA: true: bool
ETA_AUX: (k_insert_rsrs_4345 true x_1050 true x_1050): X
ETA_AUX: (k_insert_2134
           (fun (x1_4343:int) (x2_4344:int)
                (k_insert_rsrs_4345:(x_1:bool ->
                                     x_2:int ->
                                     x_3:bool ->
                                     x_4:int[(not x_1) || (not x_3) || (not (x1_4343 <= x2_4344)) || x_2 <= x_4] -> X))
            ->
            (if (x2_4344 = x1_4343)
              (l0
                (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                  (l1 (k_insert_rsrs_4345 false 0 false 0))))
              (l1
                (if (x1_4343 = 0)
                  (l0
                    (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                      (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
                  (l1
                    (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050))
                      (l1 (k_insert_rsrs_4345 false 0 false 0))))))))): X
ETA: (l1
       (if (x_1050 < p12_4249)
         (l0
           (k_insert_2134
             (fun x1_4073 x2_4074 k_insert_4075 ->
              (if (x1_4073 <= 0)
                (l0
                  (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
                    (l1
                      (ysys_1015 0 (x2_4074 - 1)
                        (fun xs11_4092 xs12_4093 xs21_4094 xs22_4095 ->
                         (if (p11_4248 <=> xs11_4092)
                           (l1
                             (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                               (l0
                                 (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093
                                   p11_4248 xs21_4094 xs22_4095 p12_4249 () k_insert_4075))))
                           (l0
                             (l0
                               (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095
                                 p12_4249 () k_insert_4075)))))))))
                (l1
                  (if (x2_4074 <= 0)
                    (l0
                      (ysys_1015 (x1_4073 - 1) 0
                        (fun p11_4102 p12_4103 p21_4104 p22_4105 -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
                    (l1 (ysys_1015 (x1_4073 - 1) (x2_4074 - 1) k_insert_4075))))))))
         (l1
           (insert_1013 x_1050
             (fun x1_4416 x2_4417 k_insert_ys'ys'_4418 -> (ysys_1015 (x1_4416 + 1) (x2_4417 + 1) k_insert_ys'ys'_4418))
             (fun ys''ys''_4111 ->
              (k_insert_2134
                (fun x1_4125 x2_4126 k_insert_4127 ->
                 (if (x1_4125 = 0)
                   (l0
                     (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                       (l1
                         (ysys_1015 0 (x2_4126 - 1)
                           (fun p11_4134 p12_4135 p21_4136 p22_4137 -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
                   (l1
                     (if (x2_4126 = 0)
                       (l0
                         (ys''ys''_4111 x1_4125 0
                           (fun p11_4144 p12_4145 p21_4146 p22_4147 -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                       (l1 (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1) k_insert_4127)))))))))))): X
ETA: (if (x_1050 < p12_4249)
       (l0
         (k_insert_2134
           (fun x1_4073 x2_4074 k_insert_4075 ->
            (if (x1_4073 <= 0)
              (l0
                (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
                  (l1
                    (ysys_1015 0 (x2_4074 - 1)
                      (fun xs11_4092 xs12_4093 xs21_4094 xs22_4095 ->
                       (if (p11_4248 <=> xs11_4092)
                         (l1
                           (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                             (l0
                               (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093
                                 p11_4248 xs21_4094 xs22_4095 p12_4249 () k_insert_4075))))
                         (l0
                           (l0
                             (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095
                               p12_4249 () k_insert_4075)))))))))
              (l1
                (if (x2_4074 <= 0)
                  (l0
                    (ysys_1015 (x1_4073 - 1) 0
                      (fun p11_4102 p12_4103 p21_4104 p22_4105 -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
                  (l1 (ysys_1015 (x1_4073 - 1) (x2_4074 - 1) k_insert_4075))))))))
       (l1
         (insert_1013 x_1050
           (fun x1_4416 x2_4417 k_insert_ys'ys'_4418 -> (ysys_1015 (x1_4416 + 1) (x2_4417 + 1) k_insert_ys'ys'_4418))
           (fun ys''ys''_4111 ->
            (k_insert_2134
              (fun x1_4125 x2_4126 k_insert_4127 ->
               (if (x1_4125 = 0)
                 (l0
                   (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                     (l1
                       (ysys_1015 0 (x2_4126 - 1)
                         (fun p11_4134 p12_4135 p21_4136 p22_4137 -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
                 (l1
                   (if (x2_4126 = 0)
                     (l0
                       (ys''ys''_4111 x1_4125 0
                         (fun p11_4144 p12_4145 p21_4146 p22_4147 -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                     (l1 (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1) k_insert_4127))))))))))): X
ETA: (l1
       (insert_1013 x_1050
         (fun x1_4416 x2_4417 k_insert_ys'ys'_4418 -> (ysys_1015 (x1_4416 + 1) (x2_4417 + 1) k_insert_ys'ys'_4418))
         (fun ys''ys''_4111 ->
          (k_insert_2134
            (fun x1_4125 x2_4126 k_insert_4127 ->
             (if (x1_4125 = 0)
               (l0
                 (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                   (l1
                     (ysys_1015 0 (x2_4126 - 1)
                       (fun p11_4134 p12_4135 p21_4136 p22_4137 -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
               (l1
                 (if (x2_4126 = 0)
                   (l0
                     (ys''ys''_4111 x1_4125 0
                       (fun p11_4144 p12_4145 p21_4146 p22_4147 -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                   (l1 (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1) k_insert_4127)))))))))): X
ETA: (insert_1013 x_1050
       (fun x1_4416 x2_4417 k_insert_ys'ys'_4418 -> (ysys_1015 (x1_4416 + 1) (x2_4417 + 1) k_insert_ys'ys'_4418))
       (fun ys''ys''_4111 ->
        (k_insert_2134
          (fun x1_4125 x2_4126 k_insert_4127 ->
           (if (x1_4125 = 0)
             (l0
               (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                 (l1
                   (ysys_1015 0 (x2_4126 - 1)
                     (fun p11_4134 p12_4135 p21_4136 p22_4137 -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
             (l1
               (if (x2_4126 = 0)
                 (l0
                   (ys''ys''_4111 x1_4125 0
                     (fun p11_4144 p12_4145 p21_4146 p22_4147 -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                 (l1 (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1) k_insert_4127))))))))): X
ETA: (fun ys''ys''_4111 ->
      (k_insert_2134
        (fun x1_4125 x2_4126 k_insert_4127 ->
         (if (x1_4125 = 0)
           (l0
             (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
               (l1
                 (ysys_1015 0 (x2_4126 - 1)
                   (fun p11_4134 p12_4135 p21_4136 p22_4137 -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
           (l1
             (if (x2_4126 = 0)
               (l0
                 (ys''ys''_4111 x1_4125 0
                   (fun p11_4144 p12_4145 p21_4146 p22_4147 -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
               (l1 (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1) k_insert_4127)))))))): ((
x_2:int ->
x_3:int ->
(x_5:bool -> x_6:int -> x_7:bool -> x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_6 <= x_8] -> X) -> X) ->
X)
ETA: (k_insert_2134
       (fun x1_4125 x2_4126 k_insert_4127 ->
        (if (x1_4125 = 0)
          (l0
            (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
              (l1
                (ysys_1015 0 (x2_4126 - 1)
                  (fun p11_4134 p12_4135 p21_4136 p22_4137 -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
          (l1
            (if (x2_4126 = 0)
              (l0
                (ys''ys''_4111 x1_4125 0
                  (fun p11_4144 p12_4145 p21_4146 p22_4147 -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
              (l1 (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1) k_insert_4127))))))): X
ETA: (fun x1_4125 x2_4126 k_insert_4127 ->
      (if (x1_4125 = 0)
        (l0
          (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
            (l1
              (ysys_1015 0 (x2_4126 - 1)
                (fun p11_4134 p12_4135 p21_4136 p22_4137 -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
        (l1
          (if (x2_4126 = 0)
            (l0
              (ys''ys''_4111 x1_4125 0
                (fun p11_4144 p12_4145 p21_4146 p22_4147 -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
            (l1 (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1) k_insert_4127)))))): (
x_1:int ->
x_2:int ->
(x_4:bool -> x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X) -> X)
ETA: (fun x2_4126 k_insert_4127 ->
      (if (x1_4125 = 0)
        (l0
          (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
            (l1
              (ysys_1015 0 (x2_4126 - 1)
                (fun p11_4134 p12_4135 p21_4136 p22_4137 -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
        (l1
          (if (x2_4126 = 0)
            (l0
              (ys''ys''_4111 x1_4125 0
                (fun p11_4144 p12_4145 p21_4146 p22_4147 -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
            (l1 (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1) k_insert_4127)))))): (
x_1:int ->
(x_3:bool -> x_4:int -> x_5:bool -> x_6:int[(not x_3) || (not x_5) || (not (x1_4125 <= x_1)) || x_4 <= x_6] -> X) -> X)
ETA: (fun k_insert_4127 ->
      (if (x1_4125 = 0)
        (l0
          (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
            (l1
              (ysys_1015 0 (x2_4126 - 1)
                (fun p11_4134 p12_4135 p21_4136 p22_4137 -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
        (l1
          (if (x2_4126 = 0)
            (l0
              (ys''ys''_4111 x1_4125 0
                (fun p11_4144 p12_4145 p21_4146 p22_4147 -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
            (l1 (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1) k_insert_4127)))))): ((
x_2:bool -> x_3:int -> x_4:bool -> x_5:int[(not x_2) || (not x_4) || (not (x1_4125 <= x2_4126)) || x_3 <= x_5] -> X) ->
X)
ETA: (if (x1_4125 = 0)
       (l0
         (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
           (l1
             (ysys_1015 0 (x2_4126 - 1)
               (fun p11_4134 p12_4135 p21_4136 p22_4137 -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
       (l1
         (if (x2_4126 = 0)
           (l0
             (ys''ys''_4111 x1_4125 0
               (fun p11_4144 p12_4145 p21_4146 p22_4147 -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
           (l1 (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1) k_insert_4127))))): X
ETA: (l1
       (if (x2_4126 = 0)
         (l0
           (ys''ys''_4111 x1_4125 0
             (fun p11_4144 p12_4145 p21_4146 p22_4147 -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
         (l1 (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1) k_insert_4127)))): X
ETA: (if (x2_4126 = 0)
       (l0
         (ys''ys''_4111 x1_4125 0
           (fun p11_4144 p12_4145 p21_4146 p22_4147 -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
       (l1 (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1) k_insert_4127))): X
ETA: (l1 (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1) k_insert_4127)): X
ETA: (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1) k_insert_4127): X
ETA: k_insert_4127: (x_1:bool ->
                     x_2:int ->
                     x_3:bool ->
                     x_4:int[(not x_1) || (not x_3) || (not ((x1_4125 - 1) <= (x2_4126 - 1))) || x_2 <= x_4] -> X)
ETA_AUX: k_insert_4127: (x_1:bool ->
                         x_2:int ->
                         x_3:bool ->
                         x_4:int[(not x_1) || (not x_3) || (not ((x1_4125 - 1) <= (x2_4126 - 1))) || x_2 <= x_4] -> X)
ETA_AUX: x__4434: bool
ETA_AUX: (k_insert_4127 x__4434): (x_1:int ->
                                   x_2:bool ->
                                   x_3:int[(not x__4434) || (not x_2) || (
                                           not ((x1_4125 - 1) <= (x2_4126 - 1))) ||
                                           x_1 <= x_3]
                                   -> X)
ETA_AUX: x__4435: int
ETA_AUX: (k_insert_4127 x__4434 x__4435): (x_1:bool ->
                                           x_2:int[(not x__4434) || (
                                                   not x_1) || (not ((x1_4125 - 1) <= (x2_4126 - 1))) ||
                                                   x__4435 <= x_2]
                                           -> X)
ETA_AUX: x__4436: bool
ETA_AUX: (k_insert_4127 x__4434 x__4435 x__4436): (x_1:int[(not x__4434) || (
                                                           not x__4436) || (
                                                           not ((x1_4125 - 1) <= (x2_4126 - 1))) ||
                                                           x__4435 <= x_1] ->
X)
ETA_AUX: x__4437: x_1:int[(not x__4434) || (not x__4436) || (not (x1_4125 <= x2_4126)) || x__4435 <= x_1]
ETA_AUX: (k_insert_4127 x__4434 x__4435 x__4436 x__4437): X
ETA: (x2_4126 - 1): int
ETA: (x1_4125 - 1): int
ETA_AUX: (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1)
           (fun (x__4434:bool) (x__4435:int) (x__4436:bool)
                (x__4437:x_1:int[(not x__4434) || (not x__4436) || (not ((x1_4125 - 1) <= (x2_4126 - 1))) ||
                                 x__4435 <= x_1])
            -> (k_insert_4127 x__4434 x__4435 x__4436 x__4437))): X
ETA: (l0
       (ys''ys''_4111 x1_4125 0
         (fun p11_4144 p12_4145 p21_4146 p22_4147 -> (k_insert_4127 p11_4144 p12_4145 true p12_4249)))): X
ETA: (ys''ys''_4111 x1_4125 0
       (fun p11_4144 p12_4145 p21_4146 p22_4147 -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))): X
ETA: (fun p11_4144 p12_4145 p21_4146 p22_4147 -> (k_insert_4127 p11_4144 p12_4145 true p12_4249)): (
x_1:bool -> x_2:int -> x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x1_4125 <= 0)) || x_2 <= x_4] -> X)
ETA: (fun p12_4145 p21_4146 p22_4147 -> (k_insert_4127 p11_4144 p12_4145 true p12_4249)): (
x_1:int -> x_2:bool -> x_3:int[(not p11_4144) || (not x_2) || (not (x1_4125 <= 0)) || x_1 <= x_3] -> X)
ETA: (fun p21_4146 p22_4147 -> (k_insert_4127 p11_4144 p12_4145 true p12_4249)): (
x_1:bool -> x_2:int[(not p11_4144) || (not x_1) || (not (x1_4125 <= 0)) || p12_4145 <= x_2] -> X)
ETA: (fun p22_4147 -> (k_insert_4127 p11_4144 p12_4145 true p12_4249)): (x_1:int[
(not p11_4144) || (not p21_4146) || (not (x1_4125 <= 0)) || p12_4145 <= x_1] ->
X)
ETA: (k_insert_4127 p11_4144 p12_4145 true p12_4249): X
ETA: p12_4249: x_1:int[(not p11_4144) || (not true) || (not (x1_4125 <= x2_4126)) || p12_4145 <= x_1]
ETA: true: bool
ETA: p12_4145: int
ETA: p11_4144: bool
ETA_AUX: (k_insert_4127 p11_4144 p12_4145 true p12_4249): X
ETA: 0: int
ETA: x1_4125: int
ETA_AUX: (ys''ys''_4111 x1_4125 0
           (fun (p11_4144:bool) (p12_4145:int) (p21_4146:bool)
                (p22_4147:x_1:int[(not p11_4144) || (not p21_4146) || (not (x1_4125 <= 0)) || p12_4145 <= x_1])
            -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))): X
ETA: (l0
       (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
         (l1
           (ysys_1015 0 (x2_4126 - 1)
             (fun p11_4134 p12_4135 p21_4136 p22_4137 -> (k_insert_4127 true p12_4249 p21_4136 p22_4137)))))): X
ETA: (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
       (l1
         (ysys_1015 0 (x2_4126 - 1)
           (fun p11_4134 p12_4135 p21_4136 p22_4137 -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))): X
ETA: (l1
       (ysys_1015 0 (x2_4126 - 1)
         (fun p11_4134 p12_4135 p21_4136 p22_4137 -> (k_insert_4127 true p12_4249 p21_4136 p22_4137)))): X
ETA: (ysys_1015 0 (x2_4126 - 1)
       (fun p11_4134 p12_4135 p21_4136 p22_4137 -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))): X
ETA: (fun p11_4134 p12_4135 p21_4136 p22_4137 -> (k_insert_4127 true p12_4249 p21_4136 p22_4137)): (
x_1:bool -> x_2:int -> x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (0 <= (x2_4126 - 1))) || x_2 <= x_4] -> X)
ETA: (fun p12_4135 p21_4136 p22_4137 -> (k_insert_4127 true p12_4249 p21_4136 p22_4137)): (
x_1:int -> x_2:bool -> x_3:int[(not p11_4134) || (not x_2) || (not (0 <= (x2_4126 - 1))) || x_1 <= x_3] -> X)
ETA: (fun p21_4136 p22_4137 -> (k_insert_4127 true p12_4249 p21_4136 p22_4137)): (
x_1:bool -> x_2:int[(not p11_4134) || (not x_1) || (not (0 <= (x2_4126 - 1))) || p12_4135 <= x_2] -> X)
ETA: (fun p22_4137 -> (k_insert_4127 true p12_4249 p21_4136 p22_4137)): (x_1:int[
(not p11_4134) || (not p21_4136) || (not (0 <= (x2_4126 - 1))) || p12_4135 <= x_1] ->
X)
ETA: (k_insert_4127 true p12_4249 p21_4136 p22_4137): X
ETA: p22_4137: x_1:int[(not true) || (not p21_4136) || (not (x1_4125 <= x2_4126)) || p12_4249 <= x_1]
ETA: p21_4136: bool
ETA: p12_4249: int
ETA: true: bool
ETA_AUX: (k_insert_4127 true p12_4249 p21_4136 p22_4137): X
ETA: (x2_4126 - 1): int
ETA: 0: int
ETA_AUX: (ysys_1015 0 (x2_4126 - 1)
           (fun (p11_4134:bool) (p12_4135:int) (p21_4136:bool)
                (p22_4137:x_1:int[(not p11_4134) || (not p21_4136) || (not (0 <= (x2_4126 - 1))) || p12_4135 <= x_1])
            -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))): X
ETA: (l0 (k_insert_4127 true p12_4249 true p12_4249)): X
ETA: (k_insert_4127 true p12_4249 true p12_4249): X
ETA: p12_4249: x_1:int[(not true) || (not true) || (not (x1_4125 <= x2_4126)) || p12_4249 <= x_1]
ETA: true: bool
ETA: p12_4249: int
ETA: true: bool
ETA_AUX: (k_insert_4127 true p12_4249 true p12_4249): X
ETA_AUX: (k_insert_2134
           (fun (x1_4125:int) (x2_4126:int)
                (k_insert_4127:(x_1:bool ->
                                x_2:int ->
                                x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x1_4125 <= x2_4126)) || x_2 <= x_4]
                                -> X))
            ->
            (if (x1_4125 = 0)
              (l0
                (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                  (l1
                    (ysys_1015 0 (x2_4126 - 1)
                      (fun (p11_4134:bool) (p12_4135:int) (p21_4136:bool)
                           (p22_4137:x_1:int[(not p11_4134) || (not p21_4136) || (
                                             not (0 <= (x2_4126 - 1))) ||
                                             p12_4135 <= x_1])
                       -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
              (l1
                (if (x2_4126 = 0)
                  (l0
                    (ys''ys''_4111 x1_4125 0
                      (fun (p11_4144:bool) (p12_4145:int) (p21_4146:bool)
                           (p22_4147:x_1:int[(not p11_4144) || (not p21_4146) || (
                                             not (x1_4125 <= 0)) || p12_4145 <= x_1])
                       -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                  (l1
                    (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1)
                      (fun (x__4434:bool) (x__4435:int) (x__4436:bool)
                           (x__4437:x_1:int[(not x__4434) || (not x__4436) ||
                                            (not ((x1_4125 - 1) <= (x2_4126 - 1))) ||
                                            x__4435 <= x_1])
                       -> (k_insert_4127 x__4434 x__4435 x__4436 x__4437))))))))): X
ETA: (fun x1_4416 x2_4417 k_insert_ys'ys'_4418 -> (ysys_1015 (x1_4416 + 1) (x2_4417 + 1) k_insert_ys'ys'_4418)): (
x_1:int ->
x_2:int ->
(x_4:bool -> x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X) -> X)
ETA: (fun x2_4417 k_insert_ys'ys'_4418 -> (ysys_1015 (x1_4416 + 1) (x2_4417 + 1) k_insert_ys'ys'_4418)): (
x_1:int ->
(x_3:bool -> x_4:int -> x_5:bool -> x_6:int[(not x_3) || (not x_5) || (not (x1_4416 <= x_1)) || x_4 <= x_6] -> X) -> X)
ETA: (fun k_insert_ys'ys'_4418 -> (ysys_1015 (x1_4416 + 1) (x2_4417 + 1) k_insert_ys'ys'_4418)): ((
x_2:bool -> x_3:int -> x_4:bool -> x_5:int[(not x_2) || (not x_4) || (not (x1_4416 <= x2_4417)) || x_3 <= x_5] -> X) ->
X)
ETA: (ysys_1015 (x1_4416 + 1) (x2_4417 + 1) k_insert_ys'ys'_4418): X
ETA: k_insert_ys'ys'_4418: (x_1:bool ->
                            x_2:int ->
                            x_3:bool ->
                            x_4:int[(not x_1) || (not x_3) || (not ((x1_4416 + 1) <= (x2_4417 + 1))) || x_2 <= x_4] ->
                            X)
ETA_AUX: k_insert_ys'ys'_4418: (x_1:bool ->
                                x_2:int ->
                                x_3:bool ->
                                x_4:int[(not x_1) || (not x_3) || (not ((x1_4416 + 1) <= (x2_4417 + 1))) || x_2 <= x_4]
                                -> X)
ETA_AUX: x__4438: bool
ETA_AUX: (k_insert_ys'ys'_4418 x__4438): (x_1:int ->
                                          x_2:bool ->
                                          x_3:int[(not x__4438) || (not x_2) ||
                                                  (not ((x1_4416 + 1) <= (x2_4417 + 1))) ||
                                                  x_1 <= x_3]
                                          -> X)
ETA_AUX: x__4439: int
ETA_AUX: (k_insert_ys'ys'_4418 x__4438 x__4439): (x_1:bool ->
                                                  x_2:int[(not x__4438) || (
                                                          not x_1) || (
                                                          not ((x1_4416 + 1) <= (x2_4417 + 1))) ||
                                                          x__4439 <= x_2]
                                                  -> X)
ETA_AUX: x__4440: bool
ETA_AUX: (k_insert_ys'ys'_4418 x__4438 x__4439 x__4440): (x_1:int[(not x__4438) || (
                                                                  not x__4440) ||
                                                                  (not ((x1_4416 + 1) <= (x2_4417 + 1))) ||
                                                                  x__4439 <= x_1] ->
X)
ETA_AUX: x__4441: x_1:int[(not x__4438) || (not x__4440) || (not (x1_4416 <= x2_4417)) || x__4439 <= x_1]
ETA_AUX: (k_insert_ys'ys'_4418 x__4438 x__4439 x__4440 x__4441): X
ETA: (x2_4417 + 1): int
ETA: (x1_4416 + 1): int
ETA_AUX: (ysys_1015 (x1_4416 + 1) (x2_4417 + 1)
           (fun (x__4438:bool) (x__4439:int) (x__4440:bool)
                (x__4441:x_1:int[(not x__4438) || (not x__4440) || (not ((x1_4416 + 1) <= (x2_4417 + 1))) ||
                                 x__4439 <= x_1])
            -> (k_insert_ys'ys'_4418 x__4438 x__4439 x__4440 x__4441))): X
ETA: x_1050: int
ETA_AUX: (insert_1013 x_1050
           (fun (x1_4416:int) (x2_4417:int)
                (k_insert_ys'ys'_4418:(x_1:bool ->
                                       x_2:int ->
                                       x_3:bool ->
                                       x_4:int[(not x_1) || (not x_3) || (not (x1_4416 <= x2_4417)) || x_2 <= x_4] -> X))
            ->
            (ysys_1015 (x1_4416 + 1) (x2_4417 + 1)
              (fun (x__4438:bool) (x__4439:int) (x__4440:bool)
                   (x__4441:x_1:int[(not x__4438) || (not x__4440) || (
                                    not ((x1_4416 + 1) <= (x2_4417 + 1))) ||
                                    x__4439 <= x_1])
               -> (k_insert_ys'ys'_4418 x__4438 x__4439 x__4440 x__4441))))
           (fun (ys''ys''_4111:(x_1:int ->
                                x_2:int ->
                                (x_4:bool ->
                                 x_5:int ->
                                 x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                                -> X))
            ->
            (k_insert_2134
              (fun (x1_4125:int) (x2_4126:int)
                   (k_insert_4127:(x_1:bool ->
                                   x_2:int ->
                                   x_3:bool ->
                                   x_4:int[(not x_1) || (not x_3) || (not (x1_4125 <= x2_4126)) || x_2 <= x_4] -> X))
               ->
               (if (x1_4125 = 0)
                 (l0
                   (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                     (l1
                       (ysys_1015 0 (x2_4126 - 1)
                         (fun (p11_4134:bool) (p12_4135:int) (p21_4136:bool)
                              (p22_4137:x_1:int[(not p11_4134) || (not p21_4136) || (
                                                not (0 <= (x2_4126 - 1))) ||
                                                p12_4135 <= x_1])
                          -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
                 (l1
                   (if (x2_4126 = 0)
                     (l0
                       (ys''ys''_4111 x1_4125 0
                         (fun (p11_4144:bool) (p12_4145:int) (p21_4146:bool)
                              (p22_4147:x_1:int[(not p11_4144) || (not p21_4146) || (
                                                not (x1_4125 <= 0)) ||
                                                p12_4145 <= x_1])
                          -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                     (l1
                       (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1)
                         (fun (x__4434:bool) (x__4435:int) (x__4436:bool)
                              (x__4437:x_1:int[(not x__4434) || (not x__4436) ||
                                               (not ((x1_4125 - 1) <= (x2_4126 - 1))) ||
                                               x__4435 <= x_1])
                          -> (k_insert_4127 x__4434 x__4435 x__4436 x__4437))))))))))): X
ETA: (l0
       (k_insert_2134
         (fun x1_4073 x2_4074 k_insert_4075 ->
          (if (x1_4073 <= 0)
            (l0
              (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
                (l1
                  (ysys_1015 0 (x2_4074 - 1)
                    (fun xs11_4092 xs12_4093 xs21_4094 xs22_4095 ->
                     (if (p11_4248 <=> xs11_4092)
                       (l1
                         (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                           (l0
                             (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093
                               p11_4248 xs21_4094 xs22_4095 p12_4249 () k_insert_4075))))
                       (l0
                         (l0
                           (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095
                             p12_4249 () k_insert_4075)))))))))
            (l1
              (if (x2_4074 <= 0)
                (l0
                  (ysys_1015 (x1_4073 - 1) 0
                    (fun p11_4102 p12_4103 p21_4104 p22_4105 -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
                (l1 (ysys_1015 (x1_4073 - 1) (x2_4074 - 1) k_insert_4075)))))))): X
ETA: (k_insert_2134
       (fun x1_4073 x2_4074 k_insert_4075 ->
        (if (x1_4073 <= 0)
          (l0
            (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
              (l1
                (ysys_1015 0 (x2_4074 - 1)
                  (fun xs11_4092 xs12_4093 xs21_4094 xs22_4095 ->
                   (if (p11_4248 <=> xs11_4092)
                     (l1
                       (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                         (l0
                           (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248
                             xs21_4094 xs22_4095 p12_4249 () k_insert_4075))))
                     (l0
                       (l0
                         (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095
                           p12_4249 () k_insert_4075)))))))))
          (l1
            (if (x2_4074 <= 0)
              (l0
                (ysys_1015 (x1_4073 - 1) 0
                  (fun p11_4102 p12_4103 p21_4104 p22_4105 -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
              (l1 (ysys_1015 (x1_4073 - 1) (x2_4074 - 1) k_insert_4075))))))): X
ETA: (fun x1_4073 x2_4074 k_insert_4075 ->
      (if (x1_4073 <= 0)
        (l0
          (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
            (l1
              (ysys_1015 0 (x2_4074 - 1)
                (fun xs11_4092 xs12_4093 xs21_4094 xs22_4095 ->
                 (if (p11_4248 <=> xs11_4092)
                   (l1
                     (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                       (l0
                         (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248
                           xs21_4094 xs22_4095 p12_4249 () k_insert_4075))))
                   (l0
                     (l0
                       (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249
                         () k_insert_4075)))))))))
        (l1
          (if (x2_4074 <= 0)
            (l0
              (ysys_1015 (x1_4073 - 1) 0
                (fun p11_4102 p12_4103 p21_4104 p22_4105 -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
            (l1 (ysys_1015 (x1_4073 - 1) (x2_4074 - 1) k_insert_4075)))))): (
x_1:int ->
x_2:int ->
(x_4:bool -> x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X) -> X)
ETA: (fun x2_4074 k_insert_4075 ->
      (if (x1_4073 <= 0)
        (l0
          (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
            (l1
              (ysys_1015 0 (x2_4074 - 1)
                (fun xs11_4092 xs12_4093 xs21_4094 xs22_4095 ->
                 (if (p11_4248 <=> xs11_4092)
                   (l1
                     (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                       (l0
                         (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248
                           xs21_4094 xs22_4095 p12_4249 () k_insert_4075))))
                   (l0
                     (l0
                       (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249
                         () k_insert_4075)))))))))
        (l1
          (if (x2_4074 <= 0)
            (l0
              (ysys_1015 (x1_4073 - 1) 0
                (fun p11_4102 p12_4103 p21_4104 p22_4105 -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
            (l1 (ysys_1015 (x1_4073 - 1) (x2_4074 - 1) k_insert_4075)))))): (
x_1:int ->
(x_3:bool -> x_4:int -> x_5:bool -> x_6:int[(not x_3) || (not x_5) || (not (x1_4073 <= x_1)) || x_4 <= x_6] -> X) -> X)
ETA: (fun k_insert_4075 ->
      (if (x1_4073 <= 0)
        (l0
          (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
            (l1
              (ysys_1015 0 (x2_4074 - 1)
                (fun xs11_4092 xs12_4093 xs21_4094 xs22_4095 ->
                 (if (p11_4248 <=> xs11_4092)
                   (l1
                     (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                       (l0
                         (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248
                           xs21_4094 xs22_4095 p12_4249 () k_insert_4075))))
                   (l0
                     (l0
                       (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249
                         () k_insert_4075)))))))))
        (l1
          (if (x2_4074 <= 0)
            (l0
              (ysys_1015 (x1_4073 - 1) 0
                (fun p11_4102 p12_4103 p21_4104 p22_4105 -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
            (l1 (ysys_1015 (x1_4073 - 1) (x2_4074 - 1) k_insert_4075)))))): ((
x_2:bool -> x_3:int -> x_4:bool -> x_5:int[(not x_2) || (not x_4) || (not (x1_4073 <= x2_4074)) || x_3 <= x_5] -> X) ->
X)
ETA: (if (x1_4073 <= 0)
       (l0
         (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
           (l1
             (ysys_1015 0 (x2_4074 - 1)
               (fun xs11_4092 xs12_4093 xs21_4094 xs22_4095 ->
                (if (p11_4248 <=> xs11_4092)
                  (l1
                    (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                      (l0
                        (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248
                          xs21_4094 xs22_4095 p12_4249 () k_insert_4075))))
                  (l0
                    (l0
                      (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249
                        () k_insert_4075)))))))))
       (l1
         (if (x2_4074 <= 0)
           (l0
             (ysys_1015 (x1_4073 - 1) 0
               (fun p11_4102 p12_4103 p21_4104 p22_4105 -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
           (l1 (ysys_1015 (x1_4073 - 1) (x2_4074 - 1) k_insert_4075))))): X
ETA: (l1
       (if (x2_4074 <= 0)
         (l0
           (ysys_1015 (x1_4073 - 1) 0
             (fun p11_4102 p12_4103 p21_4104 p22_4105 -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
         (l1 (ysys_1015 (x1_4073 - 1) (x2_4074 - 1) k_insert_4075)))): X
ETA: (if (x2_4074 <= 0)
       (l0
         (ysys_1015 (x1_4073 - 1) 0
           (fun p11_4102 p12_4103 p21_4104 p22_4105 -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
       (l1 (ysys_1015 (x1_4073 - 1) (x2_4074 - 1) k_insert_4075))): X
ETA: (l1 (ysys_1015 (x1_4073 - 1) (x2_4074 - 1) k_insert_4075)): X
ETA: (ysys_1015 (x1_4073 - 1) (x2_4074 - 1) k_insert_4075): X
ETA: k_insert_4075: (x_1:bool ->
                     x_2:int ->
                     x_3:bool ->
                     x_4:int[(not x_1) || (not x_3) || (not ((x1_4073 - 1) <= (x2_4074 - 1))) || x_2 <= x_4] -> X)
ETA_AUX: k_insert_4075: (x_1:bool ->
                         x_2:int ->
                         x_3:bool ->
                         x_4:int[(not x_1) || (not x_3) || (not ((x1_4073 - 1) <= (x2_4074 - 1))) || x_2 <= x_4] -> X)
ETA_AUX: x__4442: bool
ETA_AUX: (k_insert_4075 x__4442): (x_1:int ->
                                   x_2:bool ->
                                   x_3:int[(not x__4442) || (not x_2) || (
                                           not ((x1_4073 - 1) <= (x2_4074 - 1))) ||
                                           x_1 <= x_3]
                                   -> X)
ETA_AUX: x__4443: int
ETA_AUX: (k_insert_4075 x__4442 x__4443): (x_1:bool ->
                                           x_2:int[(not x__4442) || (
                                                   not x_1) || (not ((x1_4073 - 1) <= (x2_4074 - 1))) ||
                                                   x__4443 <= x_2]
                                           -> X)
ETA_AUX: x__4444: bool
ETA_AUX: (k_insert_4075 x__4442 x__4443 x__4444): (x_1:int[(not x__4442) || (
                                                           not x__4444) || (
                                                           not ((x1_4073 - 1) <= (x2_4074 - 1))) ||
                                                           x__4443 <= x_1] ->
X)
ETA_AUX: x__4445: x_1:int[(not x__4442) || (not x__4444) || (not (x1_4073 <= x2_4074)) || x__4443 <= x_1]
ETA_AUX: (k_insert_4075 x__4442 x__4443 x__4444 x__4445): X
ETA: (x2_4074 - 1): int
ETA: (x1_4073 - 1): int
ETA_AUX: (ysys_1015 (x1_4073 - 1) (x2_4074 - 1)
           (fun (x__4442:bool) (x__4443:int) (x__4444:bool)
                (x__4445:x_1:int[(not x__4442) || (not x__4444) || (not ((x1_4073 - 1) <= (x2_4074 - 1))) ||
                                 x__4443 <= x_1])
            -> (k_insert_4075 x__4442 x__4443 x__4444 x__4445))): X
ETA: (l0
       (ysys_1015 (x1_4073 - 1) 0
         (fun p11_4102 p12_4103 p21_4104 p22_4105 -> (k_insert_4075 p11_4102 p12_4103 true x_1050)))): X
ETA: (ysys_1015 (x1_4073 - 1) 0
       (fun p11_4102 p12_4103 p21_4104 p22_4105 -> (k_insert_4075 p11_4102 p12_4103 true x_1050))): X
ETA: (fun p11_4102 p12_4103 p21_4104 p22_4105 -> (k_insert_4075 p11_4102 p12_4103 true x_1050)): (
x_1:bool -> x_2:int -> x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not ((x1_4073 - 1) <= 0)) || x_2 <= x_4] -> X)
ETA: (fun p12_4103 p21_4104 p22_4105 -> (k_insert_4075 p11_4102 p12_4103 true x_1050)): (
x_1:int -> x_2:bool -> x_3:int[(not p11_4102) || (not x_2) || (not ((x1_4073 - 1) <= 0)) || x_1 <= x_3] -> X)
ETA: (fun p21_4104 p22_4105 -> (k_insert_4075 p11_4102 p12_4103 true x_1050)): (
x_1:bool -> x_2:int[(not p11_4102) || (not x_1) || (not ((x1_4073 - 1) <= 0)) || p12_4103 <= x_2] -> X)
ETA: (fun p22_4105 -> (k_insert_4075 p11_4102 p12_4103 true x_1050)): (x_1:int[
(not p11_4102) || (not p21_4104) || (not ((x1_4073 - 1) <= 0)) || p12_4103 <= x_1] ->
X)
ETA: (k_insert_4075 p11_4102 p12_4103 true x_1050): X
ETA: x_1050: x_1:int[(not p11_4102) || (not true) || (not (x1_4073 <= x2_4074)) || p12_4103 <= x_1]
ETA: true: bool
ETA: p12_4103: int
ETA: p11_4102: bool
ETA_AUX: (k_insert_4075 p11_4102 p12_4103 true x_1050): X
ETA: 0: int
ETA: (x1_4073 - 1): int
ETA_AUX: (ysys_1015 (x1_4073 - 1) 0
           (fun (p11_4102:bool) (p12_4103:int) (p21_4104:bool)
                (p22_4105:x_1:int[(not p11_4102) || (not p21_4104) || (not ((x1_4073 - 1) <= 0)) || p12_4103 <= x_1])
            -> (k_insert_4075 p11_4102 p12_4103 true x_1050))): X
ETA: (l0
       (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
         (l1
           (ysys_1015 0 (x2_4074 - 1)
             (fun xs11_4092 xs12_4093 xs21_4094 xs22_4095 ->
              (if (p11_4248 <=> xs11_4092)
                (l1
                  (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                    (l0
                      (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248
                        xs21_4094 xs22_4095 p12_4249 () k_insert_4075))))
                (l0
                  (l0
                    (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 ()
                      k_insert_4075))))))))): X
ETA: (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
       (l1
         (ysys_1015 0 (x2_4074 - 1)
           (fun xs11_4092 xs12_4093 xs21_4094 xs22_4095 ->
            (if (p11_4248 <=> xs11_4092)
              (l1
                (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                  (l0
                    (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248
                      xs21_4094 xs22_4095 p12_4249 () k_insert_4075))))
              (l0
                (l0
                  (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 ()
                    k_insert_4075)))))))): X
ETA: (l1
       (ysys_1015 0 (x2_4074 - 1)
         (fun xs11_4092 xs12_4093 xs21_4094 xs22_4095 ->
          (if (p11_4248 <=> xs11_4092)
            (l1
              (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                (l0
                  (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094
                    xs22_4095 p12_4249 () k_insert_4075))))
            (l0
              (l0
                (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 ()
                  k_insert_4075))))))): X
ETA: (ysys_1015 0 (x2_4074 - 1)
       (fun xs11_4092 xs12_4093 xs21_4094 xs22_4095 ->
        (if (p11_4248 <=> xs11_4092)
          (l1
            (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
              (l0
                (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094
                  xs22_4095 p12_4249 () k_insert_4075))))
          (l0
            (l0
              (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 ()
                k_insert_4075)))))): X
ETA: (fun xs11_4092 xs12_4093 xs21_4094 xs22_4095 ->
      (if (p11_4248 <=> xs11_4092)
        (l1
          (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
            (l0
              (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094
                xs22_4095 p12_4249 () k_insert_4075))))
        (l0
          (l0
            (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 ()
              k_insert_4075))))): (x_1:bool ->
                                   x_2:int ->
                                   x_3:bool ->
                                   x_4:int[(not x_1) || (not x_3) || (not (0 <= (x2_4074 - 1))) || x_2 <= x_4] -> X)
ETA: (fun xs12_4093 xs21_4094 xs22_4095 ->
      (if (p11_4248 <=> xs11_4092)
        (l1
          (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
            (l0
              (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094
                xs22_4095 p12_4249 () k_insert_4075))))
        (l0
          (l0
            (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 ()
              k_insert_4075))))): (x_1:int ->
                                   x_2:bool ->
                                   x_3:int[(not xs11_4092) || (not x_2) || (not (0 <= (x2_4074 - 1))) || x_1 <= x_3] ->
                                   X)
ETA: (fun xs21_4094 xs22_4095 ->
      (if (p11_4248 <=> xs11_4092)
        (l1
          (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
            (l0
              (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094
                xs22_4095 p12_4249 () k_insert_4075))))
        (l0
          (l0
            (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 ()
              k_insert_4075))))): (x_1:bool ->
                                   x_2:int[(not xs11_4092) || (not x_1) || (
                                           not (0 <= (x2_4074 - 1))) ||
                                           xs12_4093 <= x_2]
                                   -> X)
ETA: (fun xs22_4095 ->
      (if (p11_4248 <=> xs11_4092)
        (l1
          (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
            (l0
              (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094
                xs22_4095 p12_4249 () k_insert_4075))))
        (l0
          (l0
            (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 ()
              k_insert_4075))))): (x_1:int[(not xs11_4092) || (not xs21_4094) || (
                                           not (0 <= (x2_4074 - 1))) ||
                                           xs12_4093 <= x_1] ->
X)
ETA: (if (p11_4248 <=> xs11_4092)
       (l1
         (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
           (l0
             (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094
               xs22_4095 p12_4249 () k_insert_4075))))
       (l0
         (l0
           (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 ()
             k_insert_4075)))): X
ETA: (l0
       (l0
         (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 ()
           k_insert_4075))): X
ETA: (l0
       (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 ()
         k_insert_4075)): X
ETA: (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 () k_insert_4075): X
ETA: k_insert_4075: (bool -> int -> bool -> int -> X)
ETA_AUX: k_insert_4075: (bool -> int -> bool -> int -> X)
ETA_AUX: x__4446: bool
ETA_AUX: (k_insert_4075 x__4446): (int -> bool -> int -> X)
ETA_AUX: x__4447: int
ETA_AUX: (k_insert_4075 x__4446 x__4447): (bool -> int -> X)
ETA_AUX: x__4448: bool
ETA_AUX: (k_insert_4075 x__4446 x__4447 x__4448): (int ->
X)
ETA_AUX: x__4449: x_1:int[(not x__4446) || (not x__4448) || (not (x1_4073 <= x2_4074)) || x__4447 <= x_1]
ETA_AUX: (k_insert_4075 x__4446 x__4447 x__4448 x__4449): X
ETA: (): unit
ETA: p12_4249: int
ETA: xs22_4095: int
ETA: xs21_4094: bool
ETA: p11_4248: bool
ETA: xs12_4093: int
ETA: xs11_4092: bool
ETA: x_1050: int
ETA: x2_4074: int
ETA: x1_4073: int
ETA: true: bool
ETA_AUX: (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 ()
           (fun (x__4446:bool) (x__4447:int) (x__4448:bool) (x__4449:int) ->
            (k_insert_4075 x__4446 x__4447 x__4448 x__4449))): X
ETA: (l1
       (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
         (l0
           (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094
             xs22_4095 p12_4249 () k_insert_4075)))): X
ETA: (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
       (l0
         (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094
           xs22_4095 p12_4249 () k_insert_4075))): X
ETA: (l0
       (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095
         p12_4249 () k_insert_4075)): X
ETA: (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095
       p12_4249 () k_insert_4075): X
ETA: k_insert_4075: (bool -> int -> bool -> int -> X)
ETA_AUX: k_insert_4075: (bool -> int -> bool -> int -> X)
ETA_AUX: x__4450: bool
ETA_AUX: (k_insert_4075 x__4450): (int -> bool -> int -> X)
ETA_AUX: x__4451: int
ETA_AUX: (k_insert_4075 x__4450 x__4451): (bool -> int -> X)
ETA_AUX: x__4452: bool
ETA_AUX: (k_insert_4075 x__4450 x__4451 x__4452): (int ->
X)
ETA_AUX: x__4453: x_1:int[(not x__4450) || (not x__4452) || (not (x1_4073 <= x2_4074)) || x__4451 <= x_1]
ETA_AUX: (k_insert_4075 x__4450 x__4451 x__4452 x__4453): X
ETA: (): unit
ETA: p12_4249: int
ETA: xs22_4095: int
ETA: xs21_4094: bool
ETA: p11_4248: bool
ETA: xs12_4093: int
ETA: xs11_4092: bool
ETA: x_1050: int
ETA: x2_4074: int
ETA: x1_4073: int
ETA: (not (p12_4249 = xs12_4093)): bool
ETA_AUX: (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094
           xs22_4095 p12_4249 ()
           (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
            (k_insert_4075 x__4450 x__4451 x__4452 x__4453))): X
ETA: (l1 (k_insert_4075 true x_1050 true xs22_4095)): X
ETA: (k_insert_4075 true x_1050 true xs22_4095): X
ETA: xs22_4095: x_1:int[(not true) || (not true) || (not (x1_4073 <= x2_4074)) || x_1050 <= x_1]
ETA: true: bool
ETA: x_1050: int
ETA: true: bool
ETA_AUX: (k_insert_4075 true x_1050 true xs22_4095): X
ETA: (x2_4074 - 1): int
ETA: 0: int
ETA_AUX: (ysys_1015 0 (x2_4074 - 1)
           (fun (xs11_4092:bool) (xs12_4093:int) (xs21_4094:bool)
                (xs22_4095:x_1:int[(not xs11_4092) || (not xs21_4094) || (not (0 <= (x2_4074 - 1))) || xs12_4093 <= x_1])
            ->
            (if (p11_4248 <=> xs11_4092)
              (l1
                (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                  (l0
                    (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248
                      xs21_4094 xs22_4095 p12_4249 ()
                      (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
                       (k_insert_4075 x__4450 x__4451 x__4452 x__4453))))))
              (l0
                (l0
                  (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 ()
                    (fun (x__4446:bool) (x__4447:int) (x__4448:bool) (x__4449:int) ->
                     (k_insert_4075 x__4446 x__4447 x__4448 x__4449)))))))): X
ETA: (l0 (k_insert_4075 true x_1050 true x_1050)): X
ETA: (k_insert_4075 true x_1050 true x_1050): X
ETA: x_1050: x_1:int[(not true) || (not true) || (not (x1_4073 <= x2_4074)) || x_1050 <= x_1]
ETA: true: bool
ETA: x_1050: int
ETA: true: bool
ETA_AUX: (k_insert_4075 true x_1050 true x_1050): X
ETA_AUX: (k_insert_2134
           (fun (x1_4073:int) (x2_4074:int)
                (k_insert_4075:(x_1:bool ->
                                x_2:int ->
                                x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x1_4073 <= x2_4074)) || x_2 <= x_4]
                                -> X))
            ->
            (if (x1_4073 <= 0)
              (l0
                (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
                  (l1
                    (ysys_1015 0 (x2_4074 - 1)
                      (fun (xs11_4092:bool) (xs12_4093:int) (xs21_4094:bool)
                           (xs22_4095:x_1:int[(not xs11_4092) || (not xs21_4094) || (
                                              not (0 <= (x2_4074 - 1))) ||
                                              xs12_4093 <= x_1])
                       ->
                       (if (p11_4248 <=> xs11_4092)
                         (l1
                           (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                             (l0
                               (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093
                                 p11_4248 xs21_4094 xs22_4095 p12_4249 ()
                                 (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
                                  (k_insert_4075 x__4450 x__4451 x__4452 x__4453))))))
                         (l0
                           (l0
                             (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095
                               p12_4249 ()
                               (fun (x__4446:bool) (x__4447:int) (x__4448:bool) (x__4449:int) ->
                                (k_insert_4075 x__4446 x__4447 x__4448 x__4449)))))))))))
              (l1
                (if (x2_4074 <= 0)
                  (l0
                    (ysys_1015 (x1_4073 - 1) 0
                      (fun (p11_4102:bool) (p12_4103:int) (p21_4104:bool)
                           (p22_4105:x_1:int[(not p11_4102) || (not p21_4104) || (
                                             not ((x1_4073 - 1) <= 0)) ||
                                             p12_4103 <= x_1])
                       -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
                  (l1
                    (ysys_1015 (x1_4073 - 1) (x2_4074 - 1)
                      (fun (x__4442:bool) (x__4443:int) (x__4444:bool)
                           (x__4445:x_1:int[(not x__4442) || (not x__4444) ||
                                            (not ((x1_4073 - 1) <= (x2_4074 - 1))) ||
                                            x__4443 <= x_1])
                       -> (k_insert_4075 x__4442 x__4443 x__4444 x__4445))))))))): X
ETA: 0: int
ETA: 0: int
ETA_AUX: (ysys_1015 0 0
           (fun (p11_4248:bool) (p12_4249:int) (p21_4250:bool)
                (p22_4251:x_1:int[(not p11_4248) || (not p21_4250) || (not (0 <= 0)) || p12_4249 <= x_1])
            ->
            (if p11_4248
              (l1
                (if (x_1050 < p12_4249)
                  (l0
                    (k_insert_2134
                      (fun (x1_4073:int) (x2_4074:int)
                           (k_insert_4075:(x_1:bool ->
                                           x_2:int ->
                                           x_3:bool ->
                                           x_4:int[(not x_1) || (not x_3) || (not (x1_4073 <= x2_4074)) || x_2 <= x_4]
                                           -> X))
                       ->
                       (if (x1_4073 <= 0)
                         (l0
                           (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
                             (l1
                               (ysys_1015 0 (x2_4074 - 1)
                                 (fun (xs11_4092:bool) (xs12_4093:int) (xs21_4094:bool)
                                      (xs22_4095:x_1:int[(not xs11_4092) || (
                                                         not xs21_4094) || (
                                                         not (0 <= (x2_4074 - 1))) ||
                                                         xs12_4093 <= x_1])
                                  ->
                                  (if (p11_4248 <=> xs11_4092)
                                    (l1
                                      (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                                        (l0
                                          (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092
                                            xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 ()
                                            (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
                                             (k_insert_4075 x__4450 x__4451 x__4452 x__4453))))))
                                    (l0
                                      (l0
                                        (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094
                                          xs22_4095 p12_4249 ()
                                          (fun (x__4446:bool) (x__4447:int) (x__4448:bool) (x__4449:int) ->
                                           (k_insert_4075 x__4446 x__4447 x__4448 x__4449)))))))))))
                         (l1
                           (if (x2_4074 <= 0)
                             (l0
                               (ysys_1015 (x1_4073 - 1) 0
                                 (fun (p11_4102:bool) (p12_4103:int) (p21_4104:bool)
                                      (p22_4105:x_1:int[(not p11_4102) || (
                                                        not p21_4104) || (
                                                        not ((x1_4073 - 1) <= 0)) ||
                                                        p12_4103 <= x_1])
                                  -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
                             (l1
                               (ysys_1015 (x1_4073 - 1) (x2_4074 - 1)
                                 (fun (x__4442:bool) (x__4443:int) (x__4444:bool)
                                      (x__4445:x_1:int[(not x__4442) || (
                                                       not x__4444) || (
                                                       not ((x1_4073 - 1) <= (x2_4074 - 1))) ||
                                                       x__4443 <= x_1])
                                  -> (k_insert_4075 x__4442 x__4443 x__4444 x__4445))))))))))
                  (l1
                    (insert_1013 x_1050
                      (fun (x1_4416:int) (x2_4417:int)
                           (k_insert_ys'ys'_4418:(x_1:bool ->
                                                  x_2:int ->
                                                  x_3:bool ->
                                                  x_4:int[(not x_1) || (
                                                          not x_3) || (
                                                          not (x1_4416 <= x2_4417)) ||
                                                          x_2 <= x_4]
                                                  -> X))
                       ->
                       (ysys_1015 (x1_4416 + 1) (x2_4417 + 1)
                         (fun (x__4438:bool) (x__4439:int) (x__4440:bool)
                              (x__4441:x_1:int[(not x__4438) || (not x__4440) ||
                                               (not ((x1_4416 + 1) <= (x2_4417 + 1))) ||
                                               x__4439 <= x_1])
                          -> (k_insert_ys'ys'_4418 x__4438 x__4439 x__4440 x__4441))))
                      (fun (ys''ys''_4111:(x_1:int ->
                                           x_2:int ->
                                           (x_4:bool ->
                                            x_5:int ->
                                            x_6:bool ->
                                            x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                                           -> X))
                       ->
                       (k_insert_2134
                         (fun (x1_4125:int) (x2_4126:int)
                              (k_insert_4127:(x_1:bool ->
                                              x_2:int ->
                                              x_3:bool ->
                                              x_4:int[(not x_1) || (not x_3) || (
                                                      not (x1_4125 <= x2_4126)) ||
                                                      x_2 <= x_4]
                                              -> X))
                          ->
                          (if (x1_4125 = 0)
                            (l0
                              (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                                (l1
                                  (ysys_1015 0 (x2_4126 - 1)
                                    (fun (p11_4134:bool) (p12_4135:int) (p21_4136:bool)
                                         (p22_4137:x_1:int[(not p11_4134) || (
                                                           not p21_4136) || (
                                                           not (0 <= (x2_4126 - 1))) ||
                                                           p12_4135 <= x_1])
                                     -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
                            (l1
                              (if (x2_4126 = 0)
                                (l0
                                  (ys''ys''_4111 x1_4125 0
                                    (fun (p11_4144:bool) (p12_4145:int) (p21_4146:bool)
                                         (p22_4147:x_1:int[(not p11_4144) || (
                                                           not p21_4146) || (
                                                           not (x1_4125 <= 0)) ||
                                                           p12_4145 <= x_1])
                                     -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                                (l1
                                  (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1)
                                    (fun (x__4434:bool) (x__4435:int) (x__4436:bool)
                                         (x__4437:x_1:int[(not x__4434) || (
                                                          not x__4436) || (
                                                          not ((x1_4125 - 1) <= (x2_4126 - 1))) ||
                                                          x__4435 <= x_1])
                                     -> (k_insert_4127 x__4434 x__4435 x__4436 x__4437))))))))))))))
              (l0
                (k_insert_2134
                  (fun (x1_4343:int) (x2_4344:int)
                       (k_insert_rsrs_4345:(x_1:bool ->
                                            x_2:int ->
                                            x_3:bool ->
                                            x_4:int[(not x_1) || (not x_3) || (not (x1_4343 <= x2_4344)) || x_2 <= x_4]
                                            -> X))
                   ->
                   (if (x2_4344 = x1_4343)
                     (l0
                       (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                         (l1 (k_insert_rsrs_4345 false 0 false 0))))
                     (l1
                       (if (x1_4343 = 0)
                         (l0
                           (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                             (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
                         (l1
                           (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050))
                             (l1 (k_insert_rsrs_4345 false 0 false 0))))))))))))): X
ETA: (xsxs_1037 0 0
       (fun p11_4150 p12_4151 p21_4152 p22_4153 ->
        (if p11_4150
          (l1
            (xsxs_1037 0 0
              (fun p11_4258 p12_4259 p21_4260 p22_4261 ->
               (insertsort_2064
                 (fun x1_4370 x2_4371 k_insertsort_xs'xs'_4372 ->
                  (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1) k_insertsort_xs'xs'_4372))
                 (fun x_4185 -> (insert_1013 p12_4259 x_4185 k_insertsort_2604))))))
          (l0
            (k_insertsort_2604 (fun i1_4351 i2_4352 k_insertsort_rsrs_4353 -> (k_insertsort_rsrs_4353 false 0 false 0))))))): X
ETA: (fun p11_4150 p12_4151 p21_4152 p22_4153 ->
      (if p11_4150
        (l1
          (xsxs_1037 0 0
            (fun p11_4258 p12_4259 p21_4260 p22_4261 ->
             (insertsort_2064
               (fun x1_4370 x2_4371 k_insertsort_xs'xs'_4372 ->
                (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1) k_insertsort_xs'xs'_4372))
               (fun x_4185 -> (insert_1013 p12_4259 x_4185 k_insertsort_2604))))))
        (l0
          (k_insertsort_2604 (fun i1_4351 i2_4352 k_insertsort_rsrs_4353 -> (k_insertsort_rsrs_4353 false 0 false 0)))))): (
bool -> int -> bool -> int -> X)
ETA: (fun p12_4151 p21_4152 p22_4153 ->
      (if p11_4150
        (l1
          (xsxs_1037 0 0
            (fun p11_4258 p12_4259 p21_4260 p22_4261 ->
             (insertsort_2064
               (fun x1_4370 x2_4371 k_insertsort_xs'xs'_4372 ->
                (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1) k_insertsort_xs'xs'_4372))
               (fun x_4185 -> (insert_1013 p12_4259 x_4185 k_insertsort_2604))))))
        (l0
          (k_insertsort_2604 (fun i1_4351 i2_4352 k_insertsort_rsrs_4353 -> (k_insertsort_rsrs_4353 false 0 false 0)))))): (
int -> bool -> int -> X)
ETA: (fun p21_4152 p22_4153 ->
      (if p11_4150
        (l1
          (xsxs_1037 0 0
            (fun p11_4258 p12_4259 p21_4260 p22_4261 ->
             (insertsort_2064
               (fun x1_4370 x2_4371 k_insertsort_xs'xs'_4372 ->
                (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1) k_insertsort_xs'xs'_4372))
               (fun x_4185 -> (insert_1013 p12_4259 x_4185 k_insertsort_2604))))))
        (l0
          (k_insertsort_2604 (fun i1_4351 i2_4352 k_insertsort_rsrs_4353 -> (k_insertsort_rsrs_4353 false 0 false 0)))))): (
bool -> int -> X)
ETA: (fun p22_4153 ->
      (if p11_4150
        (l1
          (xsxs_1037 0 0
            (fun p11_4258 p12_4259 p21_4260 p22_4261 ->
             (insertsort_2064
               (fun x1_4370 x2_4371 k_insertsort_xs'xs'_4372 ->
                (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1) k_insertsort_xs'xs'_4372))
               (fun x_4185 -> (insert_1013 p12_4259 x_4185 k_insertsort_2604))))))
        (l0
          (k_insertsort_2604 (fun i1_4351 i2_4352 k_insertsort_rsrs_4353 -> (k_insertsort_rsrs_4353 false 0 false 0)))))): (int
->
X)
ETA: (if p11_4150
       (l1
         (xsxs_1037 0 0
           (fun p11_4258 p12_4259 p21_4260 p22_4261 ->
            (insertsort_2064
              (fun x1_4370 x2_4371 k_insertsort_xs'xs'_4372 ->
               (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1) k_insertsort_xs'xs'_4372))
              (fun x_4185 -> (insert_1013 p12_4259 x_4185 k_insertsort_2604))))))
       (l0 (k_insertsort_2604 (fun i1_4351 i2_4352 k_insertsort_rsrs_4353 -> (k_insertsort_rsrs_4353 false 0 false 0))))): X
ETA: (l0 (k_insertsort_2604 (fun i1_4351 i2_4352 k_insertsort_rsrs_4353 -> (k_insertsort_rsrs_4353 false 0 false 0)))): X
ETA: (k_insertsort_2604 (fun i1_4351 i2_4352 k_insertsort_rsrs_4353 -> (k_insertsort_rsrs_4353 false 0 false 0))): X
ETA: (fun i1_4351 i2_4352 k_insertsort_rsrs_4353 -> (k_insertsort_rsrs_4353 false 0 false 0)): (
x_1:int ->
x_2:int ->
(x_4:bool -> x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X) -> X)
ETA: (fun i2_4352 k_insertsort_rsrs_4353 -> (k_insertsort_rsrs_4353 false 0 false 0)): (
x_1:int ->
(x_3:bool -> x_4:int -> x_5:bool -> x_6:int[(not x_3) || (not x_5) || (not (i1_4351 <= x_1)) || x_4 <= x_6] -> X) -> X)
ETA: (fun k_insertsort_rsrs_4353 -> (k_insertsort_rsrs_4353 false 0 false 0)): ((
x_2:bool -> x_3:int -> x_4:bool -> x_5:int[(not x_2) || (not x_4) || (not (i1_4351 <= i2_4352)) || x_3 <= x_5] -> X) ->
X)
ETA: (k_insertsort_rsrs_4353 false 0 false 0): X
ETA: 0: x_1:int[(not false) || (not false) || (not (i1_4351 <= i2_4352)) || 0 <= x_1]
ETA: false: bool
ETA: 0: int
ETA: false: bool
ETA_AUX: (k_insertsort_rsrs_4353 false 0 false 0): X
ETA_AUX: (k_insertsort_2604
           (fun (i1_4351:int) (i2_4352:int)
                (k_insertsort_rsrs_4353:(x_1:bool ->
                                         x_2:int ->
                                         x_3:bool ->
                                         x_4:int[(not x_1) || (not x_3) || (not (i1_4351 <= i2_4352)) || x_2 <= x_4] ->
                                         X))
            -> (k_insertsort_rsrs_4353 false 0 false 0))): X
ETA: (l1
       (xsxs_1037 0 0
         (fun p11_4258 p12_4259 p21_4260 p22_4261 ->
          (insertsort_2064
            (fun x1_4370 x2_4371 k_insertsort_xs'xs'_4372 ->
             (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1) k_insertsort_xs'xs'_4372))
            (fun x_4185 -> (insert_1013 p12_4259 x_4185 k_insertsort_2604)))))): X
ETA: (xsxs_1037 0 0
       (fun p11_4258 p12_4259 p21_4260 p22_4261 ->
        (insertsort_2064
          (fun x1_4370 x2_4371 k_insertsort_xs'xs'_4372 ->
           (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1) k_insertsort_xs'xs'_4372))
          (fun x_4185 -> (insert_1013 p12_4259 x_4185 k_insertsort_2604))))): X
ETA: (fun p11_4258 p12_4259 p21_4260 p22_4261 ->
      (insertsort_2064
        (fun x1_4370 x2_4371 k_insertsort_xs'xs'_4372 ->
         (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1) k_insertsort_xs'xs'_4372))
        (fun x_4185 -> (insert_1013 p12_4259 x_4185 k_insertsort_2604)))): (
bool -> int -> bool -> int -> X)
ETA: (fun p12_4259 p21_4260 p22_4261 ->
      (insertsort_2064
        (fun x1_4370 x2_4371 k_insertsort_xs'xs'_4372 ->
         (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1) k_insertsort_xs'xs'_4372))
        (fun x_4185 -> (insert_1013 p12_4259 x_4185 k_insertsort_2604)))): (
int -> bool -> int -> X)
ETA: (fun p21_4260 p22_4261 ->
      (insertsort_2064
        (fun x1_4370 x2_4371 k_insertsort_xs'xs'_4372 ->
         (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1) k_insertsort_xs'xs'_4372))
        (fun x_4185 -> (insert_1013 p12_4259 x_4185 k_insertsort_2604)))): (
bool -> int -> X)
ETA: (fun p22_4261 ->
      (insertsort_2064
        (fun x1_4370 x2_4371 k_insertsort_xs'xs'_4372 ->
         (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1) k_insertsort_xs'xs'_4372))
        (fun x_4185 -> (insert_1013 p12_4259 x_4185 k_insertsort_2604)))): (int ->
X)
ETA: (insertsort_2064
       (fun x1_4370 x2_4371 k_insertsort_xs'xs'_4372 ->
        (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1) k_insertsort_xs'xs'_4372))
       (fun x_4185 -> (insert_1013 p12_4259 x_4185 k_insertsort_2604))): X
ETA: (fun x_4185 -> (insert_1013 p12_4259 x_4185 k_insertsort_2604)): ((
x_2:int ->
x_3:int ->
(x_5:bool -> x_6:int -> x_7:bool -> x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_6 <= x_8] -> X) -> X) ->
X)
ETA: (insert_1013 p12_4259 x_4185 k_insertsort_2604): X
ETA: k_insertsort_2604: ((x_2:int ->
                          x_3:int ->
                          (x_5:bool ->
                           x_6:int ->
                           x_7:bool -> x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_6 <= x_8] -> X)
                          -> X) ->
X)
ETA_AUX: k_insertsort_2604: ((x_2:int ->
                              x_3:int ->
                              (x_5:bool ->
                               x_6:int ->
                               x_7:bool -> x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_6 <= x_8] -> X)
                              -> X) ->
X)
ETA_AUX: x__4454: (x_1:int ->
                   x_2:int ->
                   (x_4:bool ->
                    x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                   -> X)
ETA_AUX: x__4455: int
ETA_AUX: (x__4454 x__4455): (x_1:int ->
                             (x_3:bool ->
                              x_4:int ->
                              x_5:bool -> x_6:int[(not x_3) || (not x_5) || (not (x__4455 <= x_1)) || x_4 <= x_6] -> X)
                             -> X)
ETA_AUX: x__4456: int
ETA_AUX: (x__4454 x__4455 x__4456): ((x_2:bool ->
                                      x_3:int ->
                                      x_4:bool ->
                                      x_5:int[(not x_2) || (not x_4) || (not (x__4455 <= x__4456)) || x_3 <= x_5] -> X)
->
X)
ETA_AUX: x__4457: (x_1:bool ->
                   x_2:int ->
                   x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4455 <= x__4456)) || x_2 <= x_4] -> X)
ETA_AUX: x__4458: bool
ETA_AUX: (x__4457 x__4458): (x_1:int ->
                             x_2:bool ->
                             x_3:int[(not x__4458) || (not x_2) || (not (x__4455 <= x__4456)) || x_1 <= x_3] -> X)
ETA_AUX: x__4459: int
ETA_AUX: (x__4457 x__4458 x__4459): (x_1:bool ->
                                     x_2:int[(not x__4458) || (not x_1) || (not (x__4455 <= x__4456)) || x__4459 <= x_2]
                                     -> X)
ETA_AUX: x__4460: bool
ETA_AUX: (x__4457 x__4458 x__4459 x__4460): (x_1:int[(not x__4458) || (
                                                     not x__4460) || (
                                                     not (x__4455 <= x__4456)) ||
                                                     x__4459 <= x_1] ->
X)
ETA_AUX: x__4461: x_1:int[(not x__4458) || (not x__4460) || (not (x__4455 <= x__4456)) || x__4459 <= x_1]
ETA_AUX: (x__4457 x__4458 x__4459 x__4460 x__4461): X
ETA_AUX: (x__4454 x__4455 x__4456
           (fun (x__4458:bool) (x__4459:int) (x__4460:bool)
                (x__4461:x_1:int[(not x__4458) || (not x__4460) || (not (x__4455 <= x__4456)) || x__4459 <= x_1])
            -> (x__4457 x__4458 x__4459 x__4460 x__4461))): X
ETA_AUX: (k_insertsort_2604
           (fun (x__4455:int) (x__4456:int)
                (x__4457:(x_1:bool ->
                          x_2:int ->
                          x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4455 <= x__4456)) || x_2 <= x_4] -> X))
            ->
            (x__4454 x__4455 x__4456
              (fun (x__4458:bool) (x__4459:int) (x__4460:bool)
                   (x__4461:x_1:int[(not x__4458) || (not x__4460) || (not (x__4455 <= x__4456)) || x__4459 <= x_1])
               -> (x__4457 x__4458 x__4459 x__4460 x__4461))))): X
ETA: x_4185: (x_1:int ->
              x_2:int ->
              (x_4:bool ->
               x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
              -> X)
ETA_AUX: x_4185: (x_1:int ->
                  x_2:int ->
                  (x_4:bool ->
                   x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                  -> X)
ETA_AUX: x__4462: int
ETA_AUX: (x_4185 x__4462): (x_1:int ->
                            (x_3:bool ->
                             x_4:int ->
                             x_5:bool -> x_6:int[(not x_3) || (not x_5) || (not (x__4462 <= x_1)) || x_4 <= x_6] -> X)
                            -> X)
ETA_AUX: x__4463: int
ETA_AUX: (x_4185 x__4462 x__4463): ((x_2:bool ->
                                     x_3:int ->
                                     x_4:bool ->
                                     x_5:int[(not x_2) || (not x_4) || (not (x__4462 <= x__4463)) || x_3 <= x_5] -> X)
->
X)
ETA_AUX: x__4464: (x_1:bool ->
                   x_2:int ->
                   x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4462 <= x__4463)) || x_2 <= x_4] -> X)
ETA_AUX: x__4465: bool
ETA_AUX: (x__4464 x__4465): (x_1:int ->
                             x_2:bool ->
                             x_3:int[(not x__4465) || (not x_2) || (not (x__4462 <= x__4463)) || x_1 <= x_3] -> X)
ETA_AUX: x__4466: int
ETA_AUX: (x__4464 x__4465 x__4466): (x_1:bool ->
                                     x_2:int[(not x__4465) || (not x_1) || (not (x__4462 <= x__4463)) || x__4466 <= x_2]
                                     -> X)
ETA_AUX: x__4467: bool
ETA_AUX: (x__4464 x__4465 x__4466 x__4467): (x_1:int[(not x__4465) || (
                                                     not x__4467) || (
                                                     not (x__4462 <= x__4463)) ||
                                                     x__4466 <= x_1] ->
X)
ETA_AUX: x__4468: x_1:int[(not x__4465) || (not x__4467) || (not (x__4462 <= x__4463)) || x__4466 <= x_1]
ETA_AUX: (x__4464 x__4465 x__4466 x__4467 x__4468): X
ETA_AUX: (x_4185 x__4462 x__4463
           (fun (x__4465:bool) (x__4466:int) (x__4467:bool)
                (x__4468:x_1:int[(not x__4465) || (not x__4467) || (not (x__4462 <= x__4463)) || x__4466 <= x_1])
            -> (x__4464 x__4465 x__4466 x__4467 x__4468))): X
ETA: p12_4259: int
ETA_AUX: (insert_1013 p12_4259
           (fun (x__4462:int) (x__4463:int)
                (x__4464:(x_1:bool ->
                          x_2:int ->
                          x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4462 <= x__4463)) || x_2 <= x_4] -> X))
            ->
            (x_4185 x__4462 x__4463
              (fun (x__4465:bool) (x__4466:int) (x__4467:bool)
                   (x__4468:x_1:int[(not x__4465) || (not x__4467) || (not (x__4462 <= x__4463)) || x__4466 <= x_1])
               -> (x__4464 x__4465 x__4466 x__4467 x__4468))))
           (fun (x__4454:(x_1:int ->
                          x_2:int ->
                          (x_4:bool ->
                           x_5:int ->
                           x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                          -> X))
            ->
            (k_insertsort_2604
              (fun (x__4455:int) (x__4456:int)
                   (x__4457:(x_1:bool ->
                             x_2:int ->
                             x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4455 <= x__4456)) || x_2 <= x_4] ->
                             X))
               ->
               (x__4454 x__4455 x__4456
                 (fun (x__4458:bool) (x__4459:int) (x__4460:bool)
                      (x__4461:x_1:int[(not x__4458) || (not x__4460) || (not (x__4455 <= x__4456)) || x__4459 <= x_1])
                  -> (x__4457 x__4458 x__4459 x__4460 x__4461))))))): X
ETA: (fun x1_4370 x2_4371 k_insertsort_xs'xs'_4372 -> (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1) k_insertsort_xs'xs'_4372)): (
int -> int -> (bool -> int -> bool -> int -> X) -> X)
ETA: (fun x2_4371 k_insertsort_xs'xs'_4372 -> (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1) k_insertsort_xs'xs'_4372)): (
int -> (bool -> int -> bool -> int -> X) -> X)
ETA: (fun k_insertsort_xs'xs'_4372 -> (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1) k_insertsort_xs'xs'_4372)): ((
bool -> int -> bool -> int -> X) ->
X)
ETA: (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1) k_insertsort_xs'xs'_4372): X
ETA: k_insertsort_xs'xs'_4372: (bool -> int -> bool -> int -> X)
ETA_AUX: k_insertsort_xs'xs'_4372: (bool -> int -> bool -> int -> X)
ETA_AUX: x__4469: bool
ETA_AUX: (k_insertsort_xs'xs'_4372 x__4469): (int -> bool -> int -> X)
ETA_AUX: x__4470: int
ETA_AUX: (k_insertsort_xs'xs'_4372 x__4469 x__4470): (bool -> int -> X)
ETA_AUX: x__4471: bool
ETA_AUX: (k_insertsort_xs'xs'_4372 x__4469 x__4470 x__4471): (int ->
X)
ETA_AUX: x__4472: int
ETA_AUX: (k_insertsort_xs'xs'_4372 x__4469 x__4470 x__4471 x__4472): X
ETA: (x2_4371 + 1): int
ETA: (x1_4370 + 1): int
ETA_AUX: (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1)
           (fun (x__4469:bool) (x__4470:int) (x__4471:bool) (x__4472:int) ->
            (k_insertsort_xs'xs'_4372 x__4469 x__4470 x__4471 x__4472))): X
ETA_AUX: (insertsort_2064
           (fun (x1_4370:int) (x2_4371:int) (k_insertsort_xs'xs'_4372:(bool -> int -> bool -> int -> X)) ->
            (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1)
              (fun (x__4469:bool) (x__4470:int) (x__4471:bool) (x__4472:int) ->
               (k_insertsort_xs'xs'_4372 x__4469 x__4470 x__4471 x__4472))))
           (fun (x_4185:(x_1:int ->
                         x_2:int ->
                         (x_4:bool ->
                          x_5:int ->
                          x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                         -> X))
            ->
            (insert_1013 p12_4259
              (fun (x__4462:int) (x__4463:int)
                   (x__4464:(x_1:bool ->
                             x_2:int ->
                             x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4462 <= x__4463)) || x_2 <= x_4] ->
                             X))
               ->
               (x_4185 x__4462 x__4463
                 (fun (x__4465:bool) (x__4466:int) (x__4467:bool)
                      (x__4468:x_1:int[(not x__4465) || (not x__4467) || (not (x__4462 <= x__4463)) || x__4466 <= x_1])
                  -> (x__4464 x__4465 x__4466 x__4467 x__4468))))
              (fun (x__4454:(x_1:int ->
                             x_2:int ->
                             (x_4:bool ->
                              x_5:int ->
                              x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                             -> X))
               ->
               (k_insertsort_2604
                 (fun (x__4455:int) (x__4456:int)
                      (x__4457:(x_1:bool ->
                                x_2:int ->
                                x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4455 <= x__4456)) || x_2 <= x_4]
                                -> X))
                  ->
                  (x__4454 x__4455 x__4456
                    (fun (x__4458:bool) (x__4459:int) (x__4460:bool)
                         (x__4461:x_1:int[(not x__4458) || (not x__4460) || (
                                          not (x__4455 <= x__4456)) ||
                                          x__4459 <= x_1])
                     -> (x__4457 x__4458 x__4459 x__4460 x__4461))))))))): X
ETA: 0: int
ETA: 0: int
ETA_AUX: (xsxs_1037 0 0
           (fun (p11_4258:bool) (p12_4259:int) (p21_4260:bool) (p22_4261:int) ->
            (insertsort_2064
              (fun (x1_4370:int) (x2_4371:int) (k_insertsort_xs'xs'_4372:(bool -> int -> bool -> int -> X)) ->
               (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1)
                 (fun (x__4469:bool) (x__4470:int) (x__4471:bool) (x__4472:int) ->
                  (k_insertsort_xs'xs'_4372 x__4469 x__4470 x__4471 x__4472))))
              (fun (x_4185:(x_1:int ->
                            x_2:int ->
                            (x_4:bool ->
                             x_5:int ->
                             x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                            -> X))
               ->
               (insert_1013 p12_4259
                 (fun (x__4462:int) (x__4463:int)
                      (x__4464:(x_1:bool ->
                                x_2:int ->
                                x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4462 <= x__4463)) || x_2 <= x_4]
                                -> X))
                  ->
                  (x_4185 x__4462 x__4463
                    (fun (x__4465:bool) (x__4466:int) (x__4467:bool)
                         (x__4468:x_1:int[(not x__4465) || (not x__4467) || (
                                          not (x__4462 <= x__4463)) ||
                                          x__4466 <= x_1])
                     -> (x__4464 x__4465 x__4466 x__4467 x__4468))))
                 (fun (x__4454:(x_1:int ->
                                x_2:int ->
                                (x_4:bool ->
                                 x_5:int ->
                                 x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                                -> X))
                  ->
                  (k_insertsort_2604
                    (fun (x__4455:int) (x__4456:int)
                         (x__4457:(x_1:bool ->
                                   x_2:int ->
                                   x_3:bool ->
                                   x_4:int[(not x_1) || (not x_3) || (not (x__4455 <= x__4456)) || x_2 <= x_4] -> X))
                     ->
                     (x__4454 x__4455 x__4456
                       (fun (x__4458:bool) (x__4459:int) (x__4460:bool)
                            (x__4461:x_1:int[(not x__4458) || (not x__4460) || (
                                             not (x__4455 <= x__4456)) ||
                                             x__4459 <= x_1])
                        -> (x__4457 x__4458 x__4459 x__4460 x__4461))))))))))): X
ETA: 0: int
ETA: 0: int
ETA_AUX: (xsxs_1037 0 0
           (fun (p11_4150:bool) (p12_4151:int) (p21_4152:bool) (p22_4153:int) ->
            (if p11_4150
              (l1
                (xsxs_1037 0 0
                  (fun (p11_4258:bool) (p12_4259:int) (p21_4260:bool) (p22_4261:int) ->
                   (insertsort_2064
                     (fun (x1_4370:int) (x2_4371:int) (k_insertsort_xs'xs'_4372:(bool -> int -> bool -> int -> X)) ->
                      (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1)
                        (fun (x__4469:bool) (x__4470:int) (x__4471:bool) (x__4472:int) ->
                         (k_insertsort_xs'xs'_4372 x__4469 x__4470 x__4471 x__4472))))
                     (fun (x_4185:(x_1:int ->
                                   x_2:int ->
                                   (x_4:bool ->
                                    x_5:int ->
                                    x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] ->
                                    X)
                                   -> X))
                      ->
                      (insert_1013 p12_4259
                        (fun (x__4462:int) (x__4463:int)
                             (x__4464:(x_1:bool ->
                                       x_2:int ->
                                       x_3:bool ->
                                       x_4:int[(not x_1) || (not x_3) || (not (x__4462 <= x__4463)) || x_2 <= x_4] -> X))
                         ->
                         (x_4185 x__4462 x__4463
                           (fun (x__4465:bool) (x__4466:int) (x__4467:bool)
                                (x__4468:x_1:int[(not x__4465) || (not x__4467) || (
                                                 not (x__4462 <= x__4463)) ||
                                                 x__4466 <= x_1])
                            -> (x__4464 x__4465 x__4466 x__4467 x__4468))))
                        (fun (x__4454:(x_1:int ->
                                       x_2:int ->
                                       (x_4:bool ->
                                        x_5:int ->
                                        x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7]
                                        -> X)
                                       -> X))
                         ->
                         (k_insertsort_2604
                           (fun (x__4455:int) (x__4456:int)
                                (x__4457:(x_1:bool ->
                                          x_2:int ->
                                          x_3:bool ->
                                          x_4:int[(not x_1) || (not x_3) || (not (x__4455 <= x__4456)) || x_2 <= x_4]
                                          -> X))
                            ->
                            (x__4454 x__4455 x__4456
                              (fun (x__4458:bool) (x__4459:int) (x__4460:bool)
                                   (x__4461:x_1:int[(not x__4458) || (
                                                    not x__4460) || (
                                                    not (x__4455 <= x__4456)) ||
                                                    x__4459 <= x_1])
                               -> (x__4457 x__4458 x__4459 x__4460 x__4461))))))))))))
              (l0
                (k_insertsort_2604
                  (fun (i1_4351:int) (i2_4352:int)
                       (k_insertsort_rsrs_4353:(x_1:bool ->
                                                x_2:int ->
                                                x_3:bool ->
                                                x_4:int[(not x_1) || (
                                                        not x_3) || (
                                                        not (i1_4351 <= i2_4352)) ||
                                                        x_2 <= x_4]
                                                -> X))
                   -> (k_insertsort_rsrs_4353 false 0 false 0))))))): X
ETA: (loop_3286 b_3275 x1_1231 x2_1231 x_1050 xs11_2354 xs12_2354 xs1_2584 xs21_2354 xs22_2354 xs2_2584 ()
       k_insert_loop_3288): X
ETA: k_insert_loop_3288: (bool -> int -> bool -> int -> X)
ETA_AUX: k_insert_loop_3288: (bool -> int -> bool -> int -> X)
ETA_AUX: x__4473: bool
ETA_AUX: (k_insert_loop_3288 x__4473): (int -> bool -> int -> X)
ETA_AUX: x__4474: int
ETA_AUX: (k_insert_loop_3288 x__4473 x__4474): (bool -> int -> X)
ETA_AUX: x__4475: bool
ETA_AUX: (k_insert_loop_3288 x__4473 x__4474 x__4475): (int ->
X)
ETA_AUX: x__4476: int
ETA_AUX: (k_insert_loop_3288 x__4473 x__4474 x__4475 x__4476): X
ETA: (): unit
ETA: xs2_2584: int
ETA: xs22_2354: int
ETA: xs21_2354: bool
ETA: xs1_2584: bool
ETA: xs12_2354: int
ETA: xs11_2354: bool
ETA: x_1050: int
ETA: x2_1231: int
ETA: x1_1231: int
ETA: b_3275: bool
ETA_AUX: (loop_3286 b_3275 x1_1231 x2_1231 x_1050 xs11_2354 xs12_2354 xs1_2584 xs21_2354 xs22_2354 xs2_2584 ()
           (fun (x__4473:bool) (x__4474:int) (x__4475:bool) (x__4476:int) ->
            (k_insert_loop_3288 x__4473 x__4474 x__4475 x__4476))): X
ETA: (l0 (k_make_list_2791 (fun i_4201 k_make_list_4202 -> (k_make_list_4202 false 0)))): X
ETA: (k_make_list_2791 (fun i_4201 k_make_list_4202 -> (k_make_list_4202 false 0))): X
ETA: (fun i_4201 k_make_list_4202 -> (k_make_list_4202 false 0)): (int -> (bool -> int -> X) -> X)
ETA: (fun k_make_list_4202 -> (k_make_list_4202 false 0)): ((bool -> int -> X) ->
X)
ETA: (k_make_list_4202 false 0): X
ETA: 0: int
ETA: false: bool
ETA_AUX: (k_make_list_4202 false 0): X
ETA_AUX: (k_make_list_2791 (fun (i_4201:int) (k_make_list_4202:(bool -> int -> X)) -> (k_make_list_4202 false 0))): X
ETA: (l1
       (rand_int
         (fun n_4205 ->
          (make_list_1044 (n_4205 - 1)
            (fun xs_4209 ->
             (k_make_list_2791
               (fun i_4218 k_make_list_4219 ->
                (if (i_4218 = 0) (l0 (k_make_list_4219 true n_4205)) (l1 (xs_4209 (i_4218 - 1) k_make_list_4219)))))))))): X
ETA: (rand_int
       (fun n_4205 ->
        (make_list_1044 (n_4205 - 1)
          (fun xs_4209 ->
           (k_make_list_2791
             (fun i_4218 k_make_list_4219 ->
              (if (i_4218 = 0) (l0 (k_make_list_4219 true n_4205)) (l1 (xs_4209 (i_4218 - 1) k_make_list_4219))))))))): X
ETA: (fun n_4205 ->
      (make_list_1044 (n_4205 - 1)
        (fun xs_4209 ->
         (k_make_list_2791
           (fun i_4218 k_make_list_4219 ->
            (if (i_4218 = 0) (l0 (k_make_list_4219 true n_4205)) (l1 (xs_4209 (i_4218 - 1) k_make_list_4219)))))))): (int
->
unit)
ETA: (make_list_1044 (n_4205 - 1)
       (fun xs_4209 ->
        (k_make_list_2791
          (fun i_4218 k_make_list_4219 ->
           (if (i_4218 = 0) (l0 (k_make_list_4219 true n_4205)) (l1 (xs_4209 (i_4218 - 1) k_make_list_4219))))))): unit
ETA: (fun xs_4209 ->
      (k_make_list_2791
        (fun i_4218 k_make_list_4219 ->
         (if (i_4218 = 0) (l0 (k_make_list_4219 true n_4205)) (l1 (xs_4209 (i_4218 - 1) k_make_list_4219)))))): ((
int -> (bool -> int -> X) -> X) ->
X)
ETA: (k_make_list_2791
       (fun i_4218 k_make_list_4219 ->
        (if (i_4218 = 0) (l0 (k_make_list_4219 true n_4205)) (l1 (xs_4209 (i_4218 - 1) k_make_list_4219))))): X
ETA: (fun i_4218 k_make_list_4219 ->
      (if (i_4218 = 0) (l0 (k_make_list_4219 true n_4205)) (l1 (xs_4209 (i_4218 - 1) k_make_list_4219)))): (
int -> (bool -> int -> X) -> X)
ETA: (fun k_make_list_4219 ->
      (if (i_4218 = 0) (l0 (k_make_list_4219 true n_4205)) (l1 (xs_4209 (i_4218 - 1) k_make_list_4219)))): ((
bool -> int -> X) ->
X)
ETA: (if (i_4218 = 0) (l0 (k_make_list_4219 true n_4205)) (l1 (xs_4209 (i_4218 - 1) k_make_list_4219))): X
ETA: (l1 (xs_4209 (i_4218 - 1) k_make_list_4219)): X
ETA: (xs_4209 (i_4218 - 1) k_make_list_4219): X
ETA: k_make_list_4219: (bool -> int -> X)
ETA_AUX: k_make_list_4219: (bool -> int -> X)
ETA_AUX: x__4477: bool
ETA_AUX: (k_make_list_4219 x__4477): (int ->
X)
ETA_AUX: x__4478: int
ETA_AUX: (k_make_list_4219 x__4477 x__4478): X
ETA: (i_4218 - 1): int
ETA_AUX: (xs_4209 (i_4218 - 1) (fun (x__4477:bool) (x__4478:int) -> (k_make_list_4219 x__4477 x__4478))): X
ETA: (l0 (k_make_list_4219 true n_4205)): X
ETA: (k_make_list_4219 true n_4205): X
ETA: n_4205: int
ETA: true: bool
ETA_AUX: (k_make_list_4219 true n_4205): X
ETA_AUX: (k_make_list_2791
           (fun (i_4218:int) (k_make_list_4219:(bool -> int -> X)) ->
            (if (i_4218 = 0) (l0 (k_make_list_4219 true n_4205))
              (l1 (xs_4209 (i_4218 - 1) (fun (x__4477:bool) (x__4478:int) -> (k_make_list_4219 x__4477 x__4478))))))): X
ETA: (n_4205 - 1): int
ETA_AUX: (make_list_1044 (n_4205 - 1)
           (fun (xs_4209:(int -> (bool -> int -> X) -> X)) ->
            (k_make_list_2791
              (fun (i_4218:int) (k_make_list_4219:(bool -> int -> X)) ->
               (if (i_4218 = 0) (l0 (k_make_list_4219 true n_4205))
                 (l1 (xs_4209 (i_4218 - 1) (fun (x__4477:bool) (x__4478:int) -> (k_make_list_4219 x__4477 x__4478))))))))): unit
ETA_AUX: (rand_int
           (fun (n_4205:int) ->
            (make_list_1044 (n_4205 - 1)
              (fun (xs_4209:(int -> (bool -> int -> X) -> X)) ->
               (k_make_list_2791
                 (fun (i_4218:int) (k_make_list_4219:(bool -> int -> X)) ->
                  (if (i_4218 = 0) (l0 (k_make_list_4219 true n_4205))
                    (l1 (xs_4209 (i_4218 - 1) (fun (x__4477:bool) (x__4478:int) -> (k_make_list_4219 x__4477 x__4478))))))))))): X
ETA_EXPAND:
Main: main_3392
  main_3392 ->
      (rand_int
        (fun (arg1_4199:int) ->
         (make_list_1044 arg1_4199
           (fun (xs_4188:(int -> (bool -> int -> X) -> X)) ->
            (insertsort_2064
              (fun (x1_4404:int) (x2_4405:int) (k_main_xsxs_4406:(bool -> int -> bool -> int -> X)) ->
               (if (x2_4405 = x1_4404)
                 (l0
                   (xs_4188 x1_4404
                     (fun (r1_4266:bool) (r2_4267:int) -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267))))
                 (l1
                   (xs_4188 x1_4404
                     (fun (x1_4273:bool) (x2_4274:int) ->
                      (xs_4188 x2_4405
                        (fun (x1_4281:bool) (x2_4282:int) -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282))))))))
              (fun (ysys_4191:(x_1:int ->
                               x_2:int ->
                               (x_4:bool ->
                                x_5:int ->
                                x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                               -> X))
               ->
               (check_2063
                 (fun (x__4422:int) (x__4423:int)
                      (x__4424:(x_1:bool ->
                                x_2:int ->
                                x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4422 <= x__4423)) || x_2 <= x_4]
                                -> X))
                  ->
                  (ysys_4191 x__4422 x__4423
                    (fun (x__4425:bool) (x__4426:int) (x__4427:bool)
                         (x__4428:x_1:int[(not x__4425) || (not x__4427) || (
                                          not (x__4422 <= x__4423)) ||
                                          x__4426 <= x_1])
                     -> (x__4424 x__4425 x__4426 x__4427 x__4428))))
                 (fun (b_4197:x_1:bool[x_1]) ->
                  (if b_4197 (l0 end) (l1 (fail_3437 true (fun (main_4039:unit) -> end))))))))))))
  check_2063 xsxs_1051 k_check_2886 ->
      (xsxs_1051 0 0
        (fun (xs11_4048:bool) (xs12_4049:int) (xs21_4050:bool)
             (xs22_4051:x_1:int[(not xs11_4048) || (not xs21_4050) || (not (0 <= 0)) || xs12_4049 <= x_1])
         ->
         (if xs11_4048
           (l1
             (if xs21_4050
               (l1
                 (if (xs22_4051 >= xs12_4049)
                   (l0
                     (check_2063
                       (fun (x1_4359:int) (x2_4360:int)
                            (k_check_xs'xs'_4361:(x_1:bool ->
                                                  x_2:int ->
                                                  x_3:bool ->
                                                  x_4:int[(not x_1) || (
                                                          not x_3) || (
                                                          not (x1_4359 <= x2_4360)) ||
                                                          x_2 <= x_4]
                                                  -> X))
                        ->
                        (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1)
                          (fun (x__4430:bool) (x__4431:int) (x__4432:bool)
                               (x__4433:x_1:int[(not x__4430) || (not x__4432) ||
                                                (not ((x1_4359 + 1) <= (x2_4360 + 1))) ||
                                                x__4431 <= x_1])
                           -> (k_check_xs'xs'_4361 x__4430 x__4431 x__4432 x__4433))))
                       (fun (x__4429:x_1:bool[x_1]) -> (k_check_2886 x__4429)))) (
                   l1 (k_check_2886 false)))) (l0 (k_check_2886 true)))) (
           l0 (k_check_2886 true)))))
  fail_3437 b k -> {fail} => (k ())
  insert_1013 x_1050 ysys_1015 k_insert_2134 ->
      (ysys_1015 0 0
        (fun (p11_4248:bool) (p12_4249:int) (p21_4250:bool)
             (p22_4251:x_1:int[(not p11_4248) || (not p21_4250) || (not (0 <= 0)) || p12_4249 <= x_1])
         ->
         (if p11_4248
           (l1
             (if (x_1050 < p12_4249)
               (l0
                 (k_insert_2134
                   (fun (x1_4073:int) (x2_4074:int)
                        (k_insert_4075:(x_1:bool ->
                                        x_2:int ->
                                        x_3:bool ->
                                        x_4:int[(not x_1) || (not x_3) || (not (x1_4073 <= x2_4074)) || x_2 <= x_4] ->
                                        X))
                    ->
                    (if (x1_4073 <= 0)
                      (l0
                        (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
                          (l1
                            (ysys_1015 0 (x2_4074 - 1)
                              (fun (xs11_4092:bool) (xs12_4093:int) (xs21_4094:bool)
                                   (xs22_4095:x_1:int[(not xs11_4092) || (
                                                      not xs21_4094) || (
                                                      not (0 <= (x2_4074 - 1))) ||
                                                      xs12_4093 <= x_1])
                               ->
                               (if (p11_4248 <=> xs11_4092)
                                 (l1
                                   (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                                     (l0
                                       (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092
                                         xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 ()
                                         (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
                                          (k_insert_4075 x__4450 x__4451 x__4452 x__4453))))))
                                 (l0
                                   (l0
                                     (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094
                                       xs22_4095 p12_4249 ()
                                       (fun (x__4446:bool) (x__4447:int) (x__4448:bool) (x__4449:int) ->
                                        (k_insert_4075 x__4446 x__4447 x__4448 x__4449)))))))))))
                      (l1
                        (if (x2_4074 <= 0)
                          (l0
                            (ysys_1015 (x1_4073 - 1) 0
                              (fun (p11_4102:bool) (p12_4103:int) (p21_4104:bool)
                                   (p22_4105:x_1:int[(not p11_4102) || (
                                                     not p21_4104) || (
                                                     not ((x1_4073 - 1) <= 0)) ||
                                                     p12_4103 <= x_1])
                               -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
                          (l1
                            (ysys_1015 (x1_4073 - 1) (x2_4074 - 1)
                              (fun (x__4442:bool) (x__4443:int) (x__4444:bool)
                                   (x__4445:x_1:int[(not x__4442) || (
                                                    not x__4444) || (
                                                    not ((x1_4073 - 1) <= (x2_4074 - 1))) ||
                                                    x__4443 <= x_1])
                               -> (k_insert_4075 x__4442 x__4443 x__4444 x__4445))))))))))
               (l1
                 (insert_1013 x_1050
                   (fun (x1_4416:int) (x2_4417:int)
                        (k_insert_ys'ys'_4418:(x_1:bool ->
                                               x_2:int ->
                                               x_3:bool ->
                                               x_4:int[(not x_1) || (
                                                       not x_3) || (not (x1_4416 <= x2_4417)) ||
                                                       x_2 <= x_4]
                                               -> X))
                    ->
                    (ysys_1015 (x1_4416 + 1) (x2_4417 + 1)
                      (fun (x__4438:bool) (x__4439:int) (x__4440:bool)
                           (x__4441:x_1:int[(not x__4438) || (not x__4440) ||
                                            (not ((x1_4416 + 1) <= (x2_4417 + 1))) ||
                                            x__4439 <= x_1])
                       -> (k_insert_ys'ys'_4418 x__4438 x__4439 x__4440 x__4441))))
                   (fun (ys''ys''_4111:(x_1:int ->
                                        x_2:int ->
                                        (x_4:bool ->
                                         x_5:int ->
                                         x_6:bool ->
                                         x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                                        -> X))
                    ->
                    (k_insert_2134
                      (fun (x1_4125:int) (x2_4126:int)
                           (k_insert_4127:(x_1:bool ->
                                           x_2:int ->
                                           x_3:bool ->
                                           x_4:int[(not x_1) || (not x_3) || (not (x1_4125 <= x2_4126)) || x_2 <= x_4]
                                           -> X))
                       ->
                       (if (x1_4125 = 0)
                         (l0
                           (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                             (l1
                               (ysys_1015 0 (x2_4126 - 1)
                                 (fun (p11_4134:bool) (p12_4135:int) (p21_4136:bool)
                                      (p22_4137:x_1:int[(not p11_4134) || (
                                                        not p21_4136) || (
                                                        not (0 <= (x2_4126 - 1))) ||
                                                        p12_4135 <= x_1])
                                  -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
                         (l1
                           (if (x2_4126 = 0)
                             (l0
                               (ys''ys''_4111 x1_4125 0
                                 (fun (p11_4144:bool) (p12_4145:int) (p21_4146:bool)
                                      (p22_4147:x_1:int[(not p11_4144) || (
                                                        not p21_4146) || (
                                                        not (x1_4125 <= 0)) ||
                                                        p12_4145 <= x_1])
                                  -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                             (l1
                               (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1)
                                 (fun (x__4434:bool) (x__4435:int) (x__4436:bool)
                                      (x__4437:x_1:int[(not x__4434) || (
                                                       not x__4436) || (
                                                       not ((x1_4125 - 1) <= (x2_4126 - 1))) ||
                                                       x__4435 <= x_1])
                                  -> (k_insert_4127 x__4434 x__4435 x__4436 x__4437))))))))))))))
           (l0
             (k_insert_2134
               (fun (x1_4343:int) (x2_4344:int)
                    (k_insert_rsrs_4345:(x_1:bool ->
                                         x_2:int ->
                                         x_3:bool ->
                                         x_4:int[(not x_1) || (not x_3) || (not (x1_4343 <= x2_4344)) || x_2 <= x_4] ->
                                         X))
                ->
                (if (x2_4344 = x1_4343)
                  (l0
                    (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                      (l1 (k_insert_rsrs_4345 false 0 false 0))))
                  (l1
                    (if (x1_4343 = 0)
                      (l0
                        (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                          (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
                      (l1
                        (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050))
                          (l1 (k_insert_rsrs_4345 false 0 false 0)))))))))))))
  insertsort_2064 xsxs_1037 k_insertsort_2604 ->
      (xsxs_1037 0 0
        (fun (p11_4150:bool) (p12_4151:int) (p21_4152:bool) (p22_4153:int) ->
         (if p11_4150
           (l1
             (xsxs_1037 0 0
               (fun (p11_4258:bool) (p12_4259:int) (p21_4260:bool) (p22_4261:int) ->
                (insertsort_2064
                  (fun (x1_4370:int) (x2_4371:int) (k_insertsort_xs'xs'_4372:(bool -> int -> bool -> int -> X)) ->
                   (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1)
                     (fun (x__4469:bool) (x__4470:int) (x__4471:bool) (x__4472:int) ->
                      (k_insertsort_xs'xs'_4372 x__4469 x__4470 x__4471 x__4472))))
                  (fun (x_4185:(x_1:int ->
                                x_2:int ->
                                (x_4:bool ->
                                 x_5:int ->
                                 x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                                -> X))
                   ->
                   (insert_1013 p12_4259
                     (fun (x__4462:int) (x__4463:int)
                          (x__4464:(x_1:bool ->
                                    x_2:int ->
                                    x_3:bool ->
                                    x_4:int[(not x_1) || (not x_3) || (not (x__4462 <= x__4463)) || x_2 <= x_4] -> X))
                      ->
                      (x_4185 x__4462 x__4463
                        (fun (x__4465:bool) (x__4466:int) (x__4467:bool)
                             (x__4468:x_1:int[(not x__4465) || (not x__4467) || (
                                              not (x__4462 <= x__4463)) ||
                                              x__4466 <= x_1])
                         -> (x__4464 x__4465 x__4466 x__4467 x__4468))))
                     (fun (x__4454:(x_1:int ->
                                    x_2:int ->
                                    (x_4:bool ->
                                     x_5:int ->
                                     x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] ->
                                     X)
                                    -> X))
                      ->
                      (k_insertsort_2604
                        (fun (x__4455:int) (x__4456:int)
                             (x__4457:(x_1:bool ->
                                       x_2:int ->
                                       x_3:bool ->
                                       x_4:int[(not x_1) || (not x_3) || (not (x__4455 <= x__4456)) || x_2 <= x_4] -> X))
                         ->
                         (x__4454 x__4455 x__4456
                           (fun (x__4458:bool) (x__4459:int) (x__4460:bool)
                                (x__4461:x_1:int[(not x__4458) || (not x__4460) || (
                                                 not (x__4455 <= x__4456)) ||
                                                 x__4459 <= x_1])
                            -> (x__4457 x__4458 x__4459 x__4460 x__4461))))))))))))
           (l0
             (k_insertsort_2604
               (fun (i1_4351:int) (i2_4352:int)
                    (k_insertsort_rsrs_4353:(x_1:bool ->
                                             x_2:int ->
                                             x_3:bool ->
                                             x_4:int[(not x_1) || (not x_3) || (not (i1_4351 <= i2_4352)) || x_2 <= x_4]
                                             -> X))
                -> (k_insertsort_rsrs_4353 false 0 false 0)))))))
  loop_3286 b_3275 x1_1231 x2_1231 x_1050 xs11_2354 xs12_2354 xs1_2584 xs21_2354 xs22_2354 xs2_2584 x_3287
  k_insert_loop_3288 ->
      (loop_3286 b_3275 x1_1231 x2_1231 x_1050 xs11_2354 xs12_2354 xs1_2584 xs21_2354 xs22_2354 xs2_2584 ()
        (fun (x__4473:bool) (x__4474:int) (x__4475:bool) (x__4476:int) ->
         (k_insert_loop_3288 x__4473 x__4474 x__4475 x__4476)))
  make_list_1044 n_1045 k_make_list_2791 when (n_1045 = 0) ->
      (l0 (k_make_list_2791 (fun (i_4201:int) (k_make_list_4202:(bool -> int -> X)) -> (k_make_list_4202 false 0))))
  make_list_1044 n_1045 k_make_list_2791 when (not (n_1045 = 0)) ->
      (l1
        (rand_int
          (fun (n_4205:int) ->
           (make_list_1044 (n_4205 - 1)
             (fun (xs_4209:(int -> (bool -> int -> X) -> X)) ->
              (k_make_list_2791
                (fun (i_4218:int) (k_make_list_4219:(bool -> int -> X)) ->
                 (if (i_4218 = 0) (l0 (k_make_list_4219 true n_4205))
                   (l1 (xs_4209 (i_4218 - 1) (fun (x__4477:bool) (x__4478:int) -> (k_make_list_4219 x__4477 x__4478))))))))))))

main_3392: ENV:

main_3392: (rand_int
             (fun (arg1_4199:int) ->
              (make_list_1044 arg1_4199
                (fun (xs_4188:(int -> (bool -> int -> X) -> X)) ->
                 (insertsort_2064
                   (fun (x1_4404:int) (x2_4405:int) (k_main_xsxs_4406:(bool -> int -> bool -> int -> X)) ->
                    (if (x2_4405 = x1_4404)
                      (l0
                        (xs_4188 x1_4404
                          (fun (r1_4266:bool) (r2_4267:int) -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267))))
                      (l1
                        (xs_4188 x1_4404
                          (fun (x1_4273:bool) (x2_4274:int) ->
                           (xs_4188 x2_4405
                             (fun (x1_4281:bool) (x2_4282:int) -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282))))))))
                   (fun (ysys_4191:(x_1:int ->
                                    x_2:int ->
                                    (x_4:bool ->
                                     x_5:int ->
                                     x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] ->
                                     X)
                                    -> X))
                    ->
                    (check_2063
                      (fun (x__4422:int) (x__4423:int)
                           (x__4424:(x_1:bool ->
                                     x_2:int ->
                                     x_3:bool ->
                                     x_4:int[(not x_1) || (not x_3) || (not (x__4422 <= x__4423)) || x_2 <= x_4] -> X))
                       ->
                       (ysys_4191 x__4422 x__4423
                         (fun (x__4425:bool) (x__4426:int) (x__4427:bool)
                              (x__4428:x_1:int[(not x__4425) || (not x__4427) || (
                                               not (x__4422 <= x__4423)) ||
                                               x__4426 <= x_1])
                          -> (x__4424 x__4425 x__4426 x__4427 x__4428))))
                      (fun (b_4197:x_1:bool[x_1]) ->
                       (if b_4197 (l0 end) (l1 (fail_3437 true (fun (main_4039:unit) -> end)))))))))))) ===> (
rand_int
 (fun (arg1_4199:int) ->
  (make_list_1044 arg1_4199
    (fun (xs_4188:(int -> (bool -> int -> X) -> X)) ->
     (insertsort_2064
       (fun (x1_4404:int) (x2_4405:int) (k_main_xsxs_4406:(bool -> int -> bool -> int -> X)) ->
        (if (x2_4405 = x1_4404)
          (l0
            (xs_4188 x1_4404 (fun (r1_4266:bool) (r2_4267:int) -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267))))
          (l1
            (xs_4188 x1_4404
              (fun (x1_4273:bool) (x2_4274:int) ->
               (xs_4188 x2_4405
                 (fun (x1_4281:bool) (x2_4282:int) -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282))))))))
       (fun (ysys_4191:(x_1:int ->
                        x_2:int ->
                        (x_4:bool ->
                         x_5:int ->
                         x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                        -> X))
        ->
        (check_2063
          (fun (x__4422:int) (x__4423:int)
               (x__4424:(x_1:bool ->
                         x_2:int ->
                         x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4422 <= x__4423)) || x_2 <= x_4] -> X))
           ->
           (ysys_4191 x__4422 x__4423
             (fun (x__4425:bool) (x__4426:int) (x__4427:bool)
                  (x__4428:x_1:int[(not x__4425) || (not x__4427) || (not (x__4422 <= x__4423)) || x__4426 <= x_1])
              -> (x__4424 x__4425 x__4426 x__4427 x__4428))))
          (fun (b_4197:x_1:bool[x_1]) -> (if b_4197 (l0 end) (l1 (fail_3437 true (fun (main_4039:unit) -> end))))))))))))
main_3392:: (rand_int
              (fun (arg1_4199:int) ->
               (make_list_1044 arg1_4199
                 (fun (xs_4188:(int -> (bool -> int -> X) -> X)) ->
                  (insertsort_2064
                    (fun (x1_4404:int) (x2_4405:int) (k_main_xsxs_4406:(bool -> int -> bool -> int -> X)) ->
                     (if (x2_4405 = x1_4404)
                       (l0
                         (xs_4188 x1_4404
                           (fun (r1_4266:bool) (r2_4267:int) -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267))))
                       (l1
                         (xs_4188 x1_4404
                           (fun (x1_4273:bool) (x2_4274:int) ->
                            (xs_4188 x2_4405
                              (fun (x1_4281:bool) (x2_4282:int) -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282))))))))
                    (fun (ysys_4191:(x_1:int ->
                                     x_2:int ->
                                     (x_4:bool ->
                                      x_5:int ->
                                      x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7]
                                      -> X)
                                     -> X))
                     ->
                     (check_2063
                       (fun (x__4422:int) (x__4423:int)
                            (x__4424:(x_1:bool ->
                                      x_2:int ->
                                      x_3:bool ->
                                      x_4:int[(not x_1) || (not x_3) || (not (x__4422 <= x__4423)) || x_2 <= x_4] -> X))
                        ->
                        (ysys_4191 x__4422 x__4423
                          (fun (x__4425:bool) (x__4426:int) (x__4427:bool)
                               (x__4428:x_1:int[(not x__4425) || (not x__4427) || (
                                                not (x__4422 <= x__4423)) ||
                                                x__4426 <= x_1])
                           -> (x__4424 x__4425 x__4426 x__4427 x__4428))))
                       (fun (b_4197:x_1:bool[x_1]) ->
                        (if b_4197 (l0 end) (l1 (fail_3437 true (fun (main_4039:unit) -> end))))))))))))
abstract_term: (rand_int
                 (fun (arg1_4199:int) ->
                  (make_list_1044 arg1_4199
                    (fun (xs_4188:(int -> (bool -> int -> X) -> X)) ->
                     (insertsort_2064
                       (fun (x1_4404:int) (x2_4405:int) (k_main_xsxs_4406:(bool -> int -> bool -> int -> X)) ->
                        (if (x2_4405 = x1_4404)
                          (l0
                            (xs_4188 x1_4404
                              (fun (r1_4266:bool) (r2_4267:int) -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267))))
                          (l1
                            (xs_4188 x1_4404
                              (fun (x1_4273:bool) (x2_4274:int) ->
                               (xs_4188 x2_4405
                                 (fun (x1_4281:bool) (x2_4282:int) ->
                                  (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282))))))))
                       (fun (ysys_4191:(x_1:int ->
                                        x_2:int ->
                                        (x_4:bool ->
                                         x_5:int ->
                                         x_6:bool ->
                                         x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                                        -> X))
                        ->
                        (check_2063
                          (fun (x__4422:int) (x__4423:int)
                               (x__4424:(x_1:bool ->
                                         x_2:int ->
                                         x_3:bool ->
                                         x_4:int[(not x_1) || (not x_3) || (not (x__4422 <= x__4423)) || x_2 <= x_4] ->
                                         X))
                           ->
                           (ysys_4191 x__4422 x__4423
                             (fun (x__4425:bool) (x__4426:int) (x__4427:bool)
                                  (x__4428:x_1:int[(not x__4425) || (
                                                   not x__4427) || (not (x__4422 <= x__4423)) ||
                                                   x__4426 <= x_1])
                              -> (x__4424 x__4425 x__4426 x__4427 x__4428))))
                          (fun (b_4197:x_1:bool[x_1]) ->
                           (if b_4197 (l0 end) (l1 (fail_3437 true (fun (main_4039:unit) -> end)))))))))))): X
abstract_term: (fun (arg1_4199:int) ->
                (make_list_1044 arg1_4199
                  (fun (xs_4188:(int -> (bool -> int -> X) -> X)) ->
                   (insertsort_2064
                     (fun (x1_4404:int) (x2_4405:int) (k_main_xsxs_4406:(bool -> int -> bool -> int -> X)) ->
                      (if (x2_4405 = x1_4404)
                        (l0
                          (xs_4188 x1_4404
                            (fun (r1_4266:bool) (r2_4267:int) -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267))))
                        (l1
                          (xs_4188 x1_4404
                            (fun (x1_4273:bool) (x2_4274:int) ->
                             (xs_4188 x2_4405
                               (fun (x1_4281:bool) (x2_4282:int) -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282))))))))
                     (fun (ysys_4191:(x_1:int ->
                                      x_2:int ->
                                      (x_4:bool ->
                                       x_5:int ->
                                       x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7]
                                       -> X)
                                      -> X))
                      ->
                      (check_2063
                        (fun (x__4422:int) (x__4423:int)
                             (x__4424:(x_1:bool ->
                                       x_2:int ->
                                       x_3:bool ->
                                       x_4:int[(not x_1) || (not x_3) || (not (x__4422 <= x__4423)) || x_2 <= x_4] -> X))
                         ->
                         (ysys_4191 x__4422 x__4423
                           (fun (x__4425:bool) (x__4426:int) (x__4427:bool)
                                (x__4428:x_1:int[(not x__4425) || (not x__4427) || (
                                                 not (x__4422 <= x__4423)) ||
                                                 x__4426 <= x_1])
                            -> (x__4424 x__4425 x__4426 x__4427 x__4428))))
                        (fun (b_4197:x_1:bool[x_1]) ->
                         (if b_4197 (l0 end) (l1 (fail_3437 true (fun (main_4039:unit) -> end))))))))))): (int ->
X)
abst_arg: arg1_4199, int
abst_arg: arg1_4199, int
abstract_term: (make_list_1044 arg1_4199
                 (fun (xs_4188:(int -> (bool -> int -> X) -> X)) ->
                  (insertsort_2064
                    (fun (x1_4404:int) (x2_4405:int) (k_main_xsxs_4406:(bool -> int -> bool -> int -> X)) ->
                     (if (x2_4405 = x1_4404)
                       (l0
                         (xs_4188 x1_4404
                           (fun (r1_4266:bool) (r2_4267:int) -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267))))
                       (l1
                         (xs_4188 x1_4404
                           (fun (x1_4273:bool) (x2_4274:int) ->
                            (xs_4188 x2_4405
                              (fun (x1_4281:bool) (x2_4282:int) -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282))))))))
                    (fun (ysys_4191:(x_1:int ->
                                     x_2:int ->
                                     (x_4:bool ->
                                      x_5:int ->
                                      x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7]
                                      -> X)
                                     -> X))
                     ->
                     (check_2063
                       (fun (x__4422:int) (x__4423:int)
                            (x__4424:(x_1:bool ->
                                      x_2:int ->
                                      x_3:bool ->
                                      x_4:int[(not x_1) || (not x_3) || (not (x__4422 <= x__4423)) || x_2 <= x_4] -> X))
                        ->
                        (ysys_4191 x__4422 x__4423
                          (fun (x__4425:bool) (x__4426:int) (x__4427:bool)
                               (x__4428:x_1:int[(not x__4425) || (not x__4427) || (
                                                not (x__4422 <= x__4423)) ||
                                                x__4426 <= x_1])
                           -> (x__4424 x__4425 x__4426 x__4427 x__4428))))
                       (fun (b_4197:x_1:bool[x_1]) ->
                        (if b_4197 (l0 end) (l1 (fail_3437 true (fun (main_4039:unit) -> end)))))))))): X
abstract_term: (fun (xs_4188:(int -> (bool -> int -> X) -> X)) ->
                (insertsort_2064
                  (fun (x1_4404:int) (x2_4405:int) (k_main_xsxs_4406:(bool -> int -> bool -> int -> X)) ->
                   (if (x2_4405 = x1_4404)
                     (l0
                       (xs_4188 x1_4404
                         (fun (r1_4266:bool) (r2_4267:int) -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267))))
                     (l1
                       (xs_4188 x1_4404
                         (fun (x1_4273:bool) (x2_4274:int) ->
                          (xs_4188 x2_4405
                            (fun (x1_4281:bool) (x2_4282:int) -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282))))))))
                  (fun (ysys_4191:(x_1:int ->
                                   x_2:int ->
                                   (x_4:bool ->
                                    x_5:int ->
                                    x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] ->
                                    X)
                                   -> X))
                   ->
                   (check_2063
                     (fun (x__4422:int) (x__4423:int)
                          (x__4424:(x_1:bool ->
                                    x_2:int ->
                                    x_3:bool ->
                                    x_4:int[(not x_1) || (not x_3) || (not (x__4422 <= x__4423)) || x_2 <= x_4] -> X))
                      ->
                      (ysys_4191 x__4422 x__4423
                        (fun (x__4425:bool) (x__4426:int) (x__4427:bool)
                             (x__4428:x_1:int[(not x__4425) || (not x__4427) || (
                                              not (x__4422 <= x__4423)) ||
                                              x__4426 <= x_1])
                         -> (x__4424 x__4425 x__4426 x__4427 x__4428))))
                     (fun (b_4197:x_1:bool[x_1]) ->
                      (if b_4197 (l0 end) (l1 (fail_3437 true (fun (main_4039:unit) -> end))))))))): ((
int -> (bool -> int -> X) -> X) ->
X)
abst_arg: xs_4188, (int -> (bool -> int -> X) -> X)
abst_arg: xs_4188, (int -> (bool -> int -> X) -> X)
abstract_term: (insertsort_2064
                 (fun (x1_4404:int) (x2_4405:int) (k_main_xsxs_4406:(bool -> int -> bool -> int -> X)) ->
                  (if (x2_4405 = x1_4404)
                    (l0
                      (xs_4188 x1_4404
                        (fun (r1_4266:bool) (r2_4267:int) -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267))))
                    (l1
                      (xs_4188 x1_4404
                        (fun (x1_4273:bool) (x2_4274:int) ->
                         (xs_4188 x2_4405
                           (fun (x1_4281:bool) (x2_4282:int) -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282))))))))
                 (fun (ysys_4191:(x_1:int ->
                                  x_2:int ->
                                  (x_4:bool ->
                                   x_5:int ->
                                   x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                                  -> X))
                  ->
                  (check_2063
                    (fun (x__4422:int) (x__4423:int)
                         (x__4424:(x_1:bool ->
                                   x_2:int ->
                                   x_3:bool ->
                                   x_4:int[(not x_1) || (not x_3) || (not (x__4422 <= x__4423)) || x_2 <= x_4] -> X))
                     ->
                     (ysys_4191 x__4422 x__4423
                       (fun (x__4425:bool) (x__4426:int) (x__4427:bool)
                            (x__4428:x_1:int[(not x__4425) || (not x__4427) || (
                                             not (x__4422 <= x__4423)) ||
                                             x__4426 <= x_1])
                        -> (x__4424 x__4425 x__4426 x__4427 x__4428))))
                    (fun (b_4197:x_1:bool[x_1]) ->
                     (if b_4197 (l0 end) (l1 (fail_3437 true (fun (main_4039:unit) -> end)))))))): X
abstract_term: (fun (ysys_4191:(x_1:int ->
                                x_2:int ->
                                (x_4:bool ->
                                 x_5:int ->
                                 x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                                -> X))
                ->
                (check_2063
                  (fun (x__4422:int) (x__4423:int)
                       (x__4424:(x_1:bool ->
                                 x_2:int ->
                                 x_3:bool ->
                                 x_4:int[(not x_1) || (not x_3) || (not (x__4422 <= x__4423)) || x_2 <= x_4] -> X))
                   ->
                   (ysys_4191 x__4422 x__4423
                     (fun (x__4425:bool) (x__4426:int) (x__4427:bool)
                          (x__4428:x_1:int[(not x__4425) || (not x__4427) || (
                                           not (x__4422 <= x__4423)) ||
                                           x__4426 <= x_1])
                      -> (x__4424 x__4425 x__4426 x__4427 x__4428))))
                  (fun (b_4197:x_1:bool[x_1]) ->
                   (if b_4197 (l0 end) (l1 (fail_3437 true (fun (main_4039:unit) -> end))))))): ((
x_2:int ->
x_3:int ->
(x_5:bool -> x_6:int -> x_7:bool -> x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_6 <= x_8] -> X) -> X) ->
X)
abst_arg: ysys_4191, (x_1:int ->
                      x_2:int ->
                      (x_4:bool ->
                       x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                      -> X)
abst_arg: ysys_4191, (x_1:int ->
                      x_2:int ->
                      (x_4:bool ->
                       x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                      -> X)
abstract_term: (check_2063
                 (fun (x__4422:int) (x__4423:int)
                      (x__4424:(x_1:bool ->
                                x_2:int ->
                                x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4422 <= x__4423)) || x_2 <= x_4]
                                -> X))
                  ->
                  (ysys_4191 x__4422 x__4423
                    (fun (x__4425:bool) (x__4426:int) (x__4427:bool)
                         (x__4428:x_1:int[(not x__4425) || (not x__4427) || (
                                          not (x__4422 <= x__4423)) ||
                                          x__4426 <= x_1])
                     -> (x__4424 x__4425 x__4426 x__4427 x__4428))))
                 (fun (b_4197:x_1:bool[x_1]) ->
                  (if b_4197 (l0 end) (l1 (fail_3437 true (fun (main_4039:unit) -> end)))))): X
abstract_term: (fun (b_4197:x_1:bool[x_1]) -> (if b_4197 (l0 end) (l1 (fail_3437 true (fun (main_4039:unit) -> end))))): (x_1:bool[
x_1] ->
X)
abst_arg: b_4197, x_1:bool[x_1]
abst_arg: b_4197, x_1:bool[x_1]
abstract_term: (if b_4197 (l0 end) (l1 (fail_3437 true (fun (main_4039:unit) -> end)))): X
abstract_term: b_4197: x_1:bool[x_1]
cond: true
pbs: b_4197 := b_4197
p:b_4197
tt:b_4197
ff:false

abstract_term: (l0 end): X
abstract_term: end: X
abstract_term: (l1 (fail_3437 true (fun (main_4039:unit) -> end))): X
abstract_term: (fail_3437 true (fun (main_4039:unit) -> end)): X
abstract_term: (fun (main_4039:unit) -> end): (unit ->
X)
abst_arg: main_4039, unit
abst_arg: main_4039, unit
abstract_term: end: X
abstract_term: true: bool
abstract_term: (fun (x__4422:int) (x__4423:int)
                    (x__4424:(x_1:bool ->
                              x_2:int ->
                              x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4422 <= x__4423)) || x_2 <= x_4]
                              -> X))
                ->
                (ysys_4191 x__4422 x__4423
                  (fun (x__4425:bool) (x__4426:int) (x__4427:bool)
                       (x__4428:x_1:int[(not x__4425) || (not x__4427) || (not (x__4422 <= x__4423)) || x__4426 <= x_1])
                   -> (x__4424 x__4425 x__4426 x__4427 x__4428)))): (
x_1:int ->
x_2:int ->
(x_4:bool -> x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X) -> X)
abst_arg: x__4422, int
abst_arg: x__4423, int
abst_arg: x__4424, (x_1:bool ->
                    x_2:int ->
                    x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4422 <= x__4423)) || x_2 <= x_4] -> X)
abst_arg: x__4422, int
abst_arg: x__4423, int
abst_arg: x__4424, (x_1:bool ->
                    x_2:int ->
                    x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4422 <= x__4423)) || x_2 <= x_4] -> X)
abstract_term: (ysys_4191 x__4422 x__4423
                 (fun (x__4425:bool) (x__4426:int) (x__4427:bool)
                      (x__4428:x_1:int[(not x__4425) || (not x__4427) || (not (x__4422 <= x__4423)) || x__4426 <= x_1])
                  -> (x__4424 x__4425 x__4426 x__4427 x__4428))): X
abstract_term: (fun (x__4425:bool) (x__4426:int) (x__4427:bool)
                    (x__4428:x_1:int[(not x__4425) || (not x__4427) || (not (x__4422 <= x__4423)) || x__4426 <= x_1])
                -> (x__4424 x__4425 x__4426 x__4427 x__4428)): (x_1:bool ->
                                                                x_2:int ->
                                                                x_3:bool ->
                                                                x_4:int[
                                                                (not x_1) || (
                                                                not x_3) || (
                                                                not (x__4422 <= x__4423)) ||
                                                                x_2 <= x_4] -> X)
abst_arg: x__4425, bool
abst_arg: x__4426, int
abst_arg: x__4427, bool
abst_arg: x__4428, x_1:int[(not x__4425) || (not x__4427) || (not (x__4422 <= x__4423)) || x__4426 <= x_1]
abst_arg: x__4425, bool
abst_arg: x__4426, int
abst_arg: x__4427, bool
abst_arg: x__4428, x_1:int[(not x__4425) || (not x__4427) || (not (x__4422 <= x__4423)) || x__4426 <= x_1]
abstract_term: (x__4424 x__4425 x__4426 x__4427 x__4428): X
abstract_term: x__4428: x_1:int[(not x__4425) || (not x__4427) || (not (x__4422 <= x__4423)) || x__4426 <= x_1]
cond: true
pbs: x__4428 := ((((not x__4425) || (not x__4427)) || (not (x__4422 <= x__4423))) || (x__4426 <= x__4428))
p:((((not x__4425) || (not x__4427)) || (not (x__4422 <= x__4423))) || (x__4426 <= x__4428))
tt:x__4428
ff:false

abstract_term: x__4427: bool
abstract_term: x__4426: int
abstract_term: x__4425: bool
abstract_term: x__4423: int
abstract_term: x__4422: int
abstract_term: (fun (x1_4404:int) (x2_4405:int) (k_main_xsxs_4406:(bool -> int -> bool -> int -> X)) ->
                (if (x2_4405 = x1_4404)
                  (l0
                    (xs_4188 x1_4404
                      (fun (r1_4266:bool) (r2_4267:int) -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267))))
                  (l1
                    (xs_4188 x1_4404
                      (fun (x1_4273:bool) (x2_4274:int) ->
                       (xs_4188 x2_4405
                         (fun (x1_4281:bool) (x2_4282:int) -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282)))))))): (
int -> int -> (bool -> int -> bool -> int -> X) -> X)
abst_arg: x1_4404, int
abst_arg: x2_4405, int
abst_arg: k_main_xsxs_4406, (bool -> int -> bool -> int -> X)
abst_arg: x1_4404, int
abst_arg: x2_4405, int
abst_arg: k_main_xsxs_4406, (bool -> int -> bool -> int -> X)
abstract_term: (if (x2_4405 = x1_4404)
                 (l0
                   (xs_4188 x1_4404
                     (fun (r1_4266:bool) (r2_4267:int) -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267))))
                 (l1
                   (xs_4188 x1_4404
                     (fun (x1_4273:bool) (x2_4274:int) ->
                      (xs_4188 x2_4405
                        (fun (x1_4281:bool) (x2_4282:int) -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282))))))): X
abstract_term: (x2_4405 = x1_4404): x_1:bool[x_1]
cond: true
pbs:
p:(x2_4405 = x1_4404)
tt:false
ff:false

abstract_term: (l0
                 (xs_4188 x1_4404
                   (fun (r1_4266:bool) (r2_4267:int) -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267)))): X
abstract_term: (xs_4188 x1_4404
                 (fun (r1_4266:bool) (r2_4267:int) -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267))): X
abstract_term: (fun (r1_4266:bool) (r2_4267:int) -> (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267)): (
bool -> int -> X)
abst_arg: r1_4266, bool
abst_arg: r2_4267, int
abst_arg: r1_4266, bool
abst_arg: r2_4267, int
abstract_term: (k_main_xsxs_4406 r1_4266 r2_4267 r1_4266 r2_4267): X
abstract_term: r2_4267: int
abstract_term: r1_4266: bool
abstract_term: r2_4267: int
abstract_term: r1_4266: bool
abstract_term: x1_4404: int
abstract_term: (l1
                 (xs_4188 x1_4404
                   (fun (x1_4273:bool) (x2_4274:int) ->
                    (xs_4188 x2_4405
                      (fun (x1_4281:bool) (x2_4282:int) -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282)))))): X
abstract_term: (xs_4188 x1_4404
                 (fun (x1_4273:bool) (x2_4274:int) ->
                  (xs_4188 x2_4405
                    (fun (x1_4281:bool) (x2_4282:int) -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282))))): X
abstract_term: (fun (x1_4273:bool) (x2_4274:int) ->
                (xs_4188 x2_4405
                  (fun (x1_4281:bool) (x2_4282:int) -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282)))): (
bool -> int -> X)
abst_arg: x1_4273, bool
abst_arg: x2_4274, int
abst_arg: x1_4273, bool
abst_arg: x2_4274, int
abstract_term: (xs_4188 x2_4405
                 (fun (x1_4281:bool) (x2_4282:int) -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282))): X
abstract_term: (fun (x1_4281:bool) (x2_4282:int) -> (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282)): (
bool -> int -> X)
abst_arg: x1_4281, bool
abst_arg: x2_4282, int
abst_arg: x1_4281, bool
abst_arg: x2_4282, int
abstract_term: (k_main_xsxs_4406 x1_4273 x2_4274 x1_4281 x2_4282): X
abstract_term: x2_4282: int
abstract_term: x1_4281: bool
abstract_term: x2_4274: int
abstract_term: x1_4273: bool
abstract_term: x2_4405: int
abstract_term: x1_4404: int
abstract_term: arg1_4199: int
check_2063: ENV: xsxs_1051:(x_1:int ->
                            x_2:int ->
                            (x_4:bool ->
                             x_5:int ->
                             x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                            -> X), k_check_2886:(x_1:bool[x_1] -> X),


abst_arg: xsxs_1051, (x_1:int ->
                      x_2:int ->
                      (x_4:bool ->
                       x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                      -> X)
abst_arg: k_check_2886, (x_1:bool[x_1] ->
X)
abst_arg: xsxs_1051, (x_1:int ->
                      x_2:int ->
                      (x_4:bool ->
                       x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                      -> X)
abst_arg: k_check_2886, (x_1:bool[x_1] ->
X)
check_2063: (xsxs_1051 0 0
              (fun (xs11_4048:bool) (xs12_4049:int) (xs21_4050:bool)
                   (xs22_4051:x_1:int[(not xs11_4048) || (not xs21_4050) || (not (0 <= 0)) || xs12_4049 <= x_1])
               ->
               (if xs11_4048
                 (l1
                   (if xs21_4050
                     (l1
                       (if (xs22_4051 >= xs12_4049)
                         (l0
                           (check_2063
                             (fun (x1_4359:int) (x2_4360:int)
                                  (k_check_xs'xs'_4361:(x_1:bool ->
                                                        x_2:int ->
                                                        x_3:bool ->
                                                        x_4:int[(not x_1) || (
                                                                not x_3) || (
                                                                not (x1_4359 <= x2_4360)) ||
                                                                x_2 <= x_4]
                                                        -> X))
                              ->
                              (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1)
                                (fun (x__4430:bool) (x__4431:int) (x__4432:bool)
                                     (x__4433:x_1:int[(not x__4430) || (
                                                      not x__4432) || (
                                                      not ((x1_4359 + 1) <= (x2_4360 + 1))) ||
                                                      x__4431 <= x_1])
                                 -> (k_check_xs'xs'_4361 x__4430 x__4431 x__4432 x__4433))))
                             (fun (x__4429:x_1:bool[x_1]) -> (k_check_2886 x__4429)))) (
                         l1 (k_check_2886 false)))) (l0 (k_check_2886 true)))) (
                 l0 (k_check_2886 true))))) ===> (xsxs_1051 0 0
                                                   (fun (xs11_4048:bool) (xs12_4049:int) (xs21_4050:bool)
                                                        (xs22_4051:x_1:int[
                                                        (not xs11_4048) || (
                                                        not xs21_4050) || (
                                                        not (0 <= 0)) ||
                                                        xs12_4049 <= x_1])
                                                    ->
                                                    (if xs11_4048
                                                      (l1
                                                        (if xs21_4050
                                                          (l1
                                                            (if (xs22_4051 >= xs12_4049)
                                                              (l0
                                                                (check_2063
                                                                  (fun
                                                                   (x1_4359:int) (x2_4360:int)
                                                                   (k_check_xs'xs'_4361:(
                                                                   x_1:bool ->
                                                                   x_2:int ->
                                                                   x_3:bool ->
                                                                   x_4:int[
                                                                   (not x_1) || (
                                                                   not x_3) || (
                                                                   not (x1_4359 <= x2_4360)) ||
                                                                   x_2 <= x_4] -> X)) ->
                                                                   (xsxs_1051 (
                                                                    x1_4359 + 1) (
                                                                    x2_4360 + 1)
                                                                    (fun
                                                                    (x__4430:bool) (x__4431:int) (x__4432:bool)
                                                                    (x__4433:x_1:int[
                                                                    (
                                                                    not x__4430) || (
                                                                    not x__4432) ||
                                                                    (
                                                                    not ((x1_4359 + 1) <= (x2_4360 + 1))) ||
                                                                    x__4431 <= x_1]) ->
                                                                    (k_check_xs'xs'_4361 x__4430 x__4431 x__4432
                                                                    x__4433))))
                                                                  (fun
                                                                   (x__4429:x_1:bool[x_1]) -> (
                                                                   k_check_2886 x__4429)))) (
                                                              l1 (k_check_2886 false)))) (
                                                          l0 (k_check_2886 true)))) (
                                                      l0 (k_check_2886 true)))))
check_2063:: (xsxs_1051 0 0
               (fun (xs11_4048:bool) (xs12_4049:int) (xs21_4050:bool)
                    (xs22_4051:x_1:int[(not xs11_4048) || (not xs21_4050) || (not (0 <= 0)) || xs12_4049 <= x_1])
                ->
                (if xs11_4048
                  (l1
                    (if xs21_4050
                      (l1
                        (if (xs22_4051 >= xs12_4049)
                          (l0
                            (check_2063
                              (fun (x1_4359:int) (x2_4360:int)
                                   (k_check_xs'xs'_4361:(x_1:bool ->
                                                         x_2:int ->
                                                         x_3:bool ->
                                                         x_4:int[(not x_1) || (
                                                                 not x_3) || (
                                                                 not (x1_4359 <= x2_4360)) ||
                                                                 x_2 <= x_4]
                                                         -> X))
                               ->
                               (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1)
                                 (fun (x__4430:bool) (x__4431:int) (x__4432:bool)
                                      (x__4433:x_1:int[(not x__4430) || (
                                                       not x__4432) || (
                                                       not ((x1_4359 + 1) <= (x2_4360 + 1))) ||
                                                       x__4431 <= x_1])
                                  -> (k_check_xs'xs'_4361 x__4430 x__4431 x__4432 x__4433))))
                              (fun (x__4429:x_1:bool[x_1]) -> (k_check_2886 x__4429)))) (
                          l1 (k_check_2886 false)))) (l0 (k_check_2886 true)))) (
                  l0 (k_check_2886 true)))))
abstract_term: (xsxs_1051 0 0
                 (fun (xs11_4048:bool) (xs12_4049:int) (xs21_4050:bool)
                      (xs22_4051:x_1:int[(not xs11_4048) || (not xs21_4050) || (not (0 <= 0)) || xs12_4049 <= x_1])
                  ->
                  (if xs11_4048
                    (l1
                      (if xs21_4050
                        (l1
                          (if (xs22_4051 >= xs12_4049)
                            (l0
                              (check_2063
                                (fun (x1_4359:int) (x2_4360:int)
                                     (k_check_xs'xs'_4361:(x_1:bool ->
                                                           x_2:int ->
                                                           x_3:bool ->
                                                           x_4:int[(not x_1) || (
                                                                   not x_3) || (
                                                                   not (x1_4359 <= x2_4360)) ||
                                                                   x_2 <= x_4]
                                                           -> X))
                                 ->
                                 (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1)
                                   (fun (x__4430:bool) (x__4431:int) (x__4432:bool)
                                        (x__4433:x_1:int[(not x__4430) || (
                                                         not x__4432) || (
                                                         not ((x1_4359 + 1) <= (x2_4360 + 1))) ||
                                                         x__4431 <= x_1])
                                    -> (k_check_xs'xs'_4361 x__4430 x__4431 x__4432 x__4433))))
                                (fun (x__4429:x_1:bool[x_1]) -> (k_check_2886 x__4429)))) (
                            l1 (k_check_2886 false)))) (l0 (k_check_2886 true)))) (
                    l0 (k_check_2886 true))))): X
abstract_term: (fun (xs11_4048:bool) (xs12_4049:int) (xs21_4050:bool)
                    (xs22_4051:x_1:int[(not xs11_4048) || (not xs21_4050) || (not (0 <= 0)) || xs12_4049 <= x_1])
                ->
                (if xs11_4048
                  (l1
                    (if xs21_4050
                      (l1
                        (if (xs22_4051 >= xs12_4049)
                          (l0
                            (check_2063
                              (fun (x1_4359:int) (x2_4360:int)
                                   (k_check_xs'xs'_4361:(x_1:bool ->
                                                         x_2:int ->
                                                         x_3:bool ->
                                                         x_4:int[(not x_1) || (
                                                                 not x_3) || (
                                                                 not (x1_4359 <= x2_4360)) ||
                                                                 x_2 <= x_4]
                                                         -> X))
                               ->
                               (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1)
                                 (fun (x__4430:bool) (x__4431:int) (x__4432:bool)
                                      (x__4433:x_1:int[(not x__4430) || (
                                                       not x__4432) || (
                                                       not ((x1_4359 + 1) <= (x2_4360 + 1))) ||
                                                       x__4431 <= x_1])
                                  -> (k_check_xs'xs'_4361 x__4430 x__4431 x__4432 x__4433))))
                              (fun (x__4429:x_1:bool[x_1]) -> (k_check_2886 x__4429)))) (
                          l1 (k_check_2886 false)))) (l0 (k_check_2886 true)))) (
                  l0 (k_check_2886 true)))): (x_1:bool ->
                                              x_2:int ->
                                              x_3:bool ->
                                              x_4:int[(not x_1) || (not x_3) || (not (0 <= 0)) || x_2 <= x_4] -> X)
abst_arg: xs11_4048, bool
abst_arg: xs12_4049, int
abst_arg: xs21_4050, bool
abst_arg: xs22_4051, x_1:int[(not xs11_4048) || (not xs21_4050) || (not (0 <= 0)) || xs12_4049 <= x_1]
abst_arg: xs11_4048, bool
abst_arg: xs12_4049, int
abst_arg: xs21_4050, bool
abst_arg: xs22_4051, x_1:int[(not xs11_4048) || (not xs21_4050) || (not (0 <= 0)) || xs12_4049 <= x_1]
abstract_term: (if xs11_4048
                 (l1
                   (if xs21_4050
                     (l1
                       (if (xs22_4051 >= xs12_4049)
                         (l0
                           (check_2063
                             (fun (x1_4359:int) (x2_4360:int)
                                  (k_check_xs'xs'_4361:(x_1:bool ->
                                                        x_2:int ->
                                                        x_3:bool ->
                                                        x_4:int[(not x_1) || (
                                                                not x_3) || (
                                                                not (x1_4359 <= x2_4360)) ||
                                                                x_2 <= x_4]
                                                        -> X))
                              ->
                              (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1)
                                (fun (x__4430:bool) (x__4431:int) (x__4432:bool)
                                     (x__4433:x_1:int[(not x__4430) || (
                                                      not x__4432) || (
                                                      not ((x1_4359 + 1) <= (x2_4360 + 1))) ||
                                                      x__4431 <= x_1])
                                 -> (k_check_xs'xs'_4361 x__4430 x__4431 x__4432 x__4433))))
                             (fun (x__4429:x_1:bool[x_1]) -> (k_check_2886 x__4429)))) (
                         l1 (k_check_2886 false)))) (l0 (k_check_2886 true)))) (
                 l0 (k_check_2886 true))): X
abstract_term: xs11_4048: x_1:bool[x_1]
cond: true
pbs: xs22_4051 := ((((not xs11_4048) || (not xs21_4050)) || (not (0 <= 0))) || (xs12_4049 <= xs22_4051))
p:xs11_4048
tt:false
ff:false

abstract_term: (l1
                 (if xs21_4050
                   (l1
                     (if (xs22_4051 >= xs12_4049)
                       (l0
                         (check_2063
                           (fun (x1_4359:int) (x2_4360:int)
                                (k_check_xs'xs'_4361:(x_1:bool ->
                                                      x_2:int ->
                                                      x_3:bool ->
                                                      x_4:int[(not x_1) || (
                                                              not x_3) || (
                                                              not (x1_4359 <= x2_4360)) ||
                                                              x_2 <= x_4]
                                                      -> X))
                            ->
                            (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1)
                              (fun (x__4430:bool) (x__4431:int) (x__4432:bool)
                                   (x__4433:x_1:int[(not x__4430) || (
                                                    not x__4432) || (
                                                    not ((x1_4359 + 1) <= (x2_4360 + 1))) ||
                                                    x__4431 <= x_1])
                               -> (k_check_xs'xs'_4361 x__4430 x__4431 x__4432 x__4433))))
                           (fun (x__4429:x_1:bool[x_1]) -> (k_check_2886 x__4429)))) (
                       l1 (k_check_2886 false)))) (l0 (k_check_2886 true)))): X
abstract_term: (if xs21_4050
                 (l1
                   (if (xs22_4051 >= xs12_4049)
                     (l0
                       (check_2063
                         (fun (x1_4359:int) (x2_4360:int)
                              (k_check_xs'xs'_4361:(x_1:bool ->
                                                    x_2:int ->
                                                    x_3:bool ->
                                                    x_4:int[(not x_1) || (
                                                            not x_3) || (
                                                            not (x1_4359 <= x2_4360)) ||
                                                            x_2 <= x_4]
                                                    -> X))
                          ->
                          (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1)
                            (fun (x__4430:bool) (x__4431:int) (x__4432:bool)
                                 (x__4433:x_1:int[(not x__4430) || (not x__4432) ||
                                                  (not ((x1_4359 + 1) <= (x2_4360 + 1))) ||
                                                  x__4431 <= x_1])
                             -> (k_check_xs'xs'_4361 x__4430 x__4431 x__4432 x__4433))))
                         (fun (x__4429:x_1:bool[x_1]) -> (k_check_2886 x__4429)))) (
                     l1 (k_check_2886 false)))) (l0 (k_check_2886 true))): X
abstract_term: xs21_4050: x_1:bool[x_1]
cond: xs11_4048; true
pbs: xs22_4051 := ((((not xs11_4048) || (not xs21_4050)) || (not (0 <= 0))) || (xs12_4049 <= xs22_4051))
p:xs21_4050
tt:false
ff:false

abstract_term: (l1
                 (if (xs22_4051 >= xs12_4049)
                   (l0
                     (check_2063
                       (fun (x1_4359:int) (x2_4360:int)
                            (k_check_xs'xs'_4361:(x_1:bool ->
                                                  x_2:int ->
                                                  x_3:bool ->
                                                  x_4:int[(not x_1) || (
                                                          not x_3) || (
                                                          not (x1_4359 <= x2_4360)) ||
                                                          x_2 <= x_4]
                                                  -> X))
                        ->
                        (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1)
                          (fun (x__4430:bool) (x__4431:int) (x__4432:bool)
                               (x__4433:x_1:int[(not x__4430) || (not x__4432) ||
                                                (not ((x1_4359 + 1) <= (x2_4360 + 1))) ||
                                                x__4431 <= x_1])
                           -> (k_check_xs'xs'_4361 x__4430 x__4431 x__4432 x__4433))))
                       (fun (x__4429:x_1:bool[x_1]) -> (k_check_2886 x__4429)))) (
                   l1 (k_check_2886 false)))): X
abstract_term: (if (xs22_4051 >= xs12_4049)
                 (l0
                   (check_2063
                     (fun (x1_4359:int) (x2_4360:int)
                          (k_check_xs'xs'_4361:(x_1:bool ->
                                                x_2:int ->
                                                x_3:bool ->
                                                x_4:int[(not x_1) || (
                                                        not x_3) || (
                                                        not (x1_4359 <= x2_4360)) ||
                                                        x_2 <= x_4]
                                                -> X))
                      ->
                      (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1)
                        (fun (x__4430:bool) (x__4431:int) (x__4432:bool)
                             (x__4433:x_1:int[(not x__4430) || (not x__4432) ||
                                              (not ((x1_4359 + 1) <= (x2_4360 + 1))) ||
                                              x__4431 <= x_1])
                         -> (k_check_xs'xs'_4361 x__4430 x__4431 x__4432 x__4433))))
                     (fun (x__4429:x_1:bool[x_1]) -> (k_check_2886 x__4429)))) (
                 l1 (k_check_2886 false))): X
abstract_term: (xs22_4051 >= xs12_4049): x_1:bool[x_1]
cond: xs21_4050; xs11_4048; true
pbs: xs22_4051 := ((((not xs11_4048) || (not xs21_4050)) || (not (0 <= 0))) || (xs12_4049 <= xs22_4051))
p:(xs22_4051 >= xs12_4049)
tt:xs22_4051
ff:false

abstract_term: (l0
                 (check_2063
                   (fun (x1_4359:int) (x2_4360:int)
                        (k_check_xs'xs'_4361:(x_1:bool ->
                                              x_2:int ->
                                              x_3:bool ->
                                              x_4:int[(not x_1) || (not x_3) || (
                                                      not (x1_4359 <= x2_4360)) ||
                                                      x_2 <= x_4]
                                              -> X))
                    ->
                    (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1)
                      (fun (x__4430:bool) (x__4431:int) (x__4432:bool)
                           (x__4433:x_1:int[(not x__4430) || (not x__4432) ||
                                            (not ((x1_4359 + 1) <= (x2_4360 + 1))) ||
                                            x__4431 <= x_1])
                       -> (k_check_xs'xs'_4361 x__4430 x__4431 x__4432 x__4433))))
                   (fun (x__4429:x_1:bool[x_1]) -> (k_check_2886 x__4429)))): X
abstract_term: (check_2063
                 (fun (x1_4359:int) (x2_4360:int)
                      (k_check_xs'xs'_4361:(x_1:bool ->
                                            x_2:int ->
                                            x_3:bool ->
                                            x_4:int[(not x_1) || (not x_3) || (not (x1_4359 <= x2_4360)) || x_2 <= x_4]
                                            -> X))
                  ->
                  (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1)
                    (fun (x__4430:bool) (x__4431:int) (x__4432:bool)
                         (x__4433:x_1:int[(not x__4430) || (not x__4432) || (
                                          not ((x1_4359 + 1) <= (x2_4360 + 1))) ||
                                          x__4431 <= x_1])
                     -> (k_check_xs'xs'_4361 x__4430 x__4431 x__4432 x__4433))))
                 (fun (x__4429:x_1:bool[x_1]) -> (k_check_2886 x__4429))): X
abstract_term: (fun (x__4429:x_1:bool[x_1]) -> (k_check_2886 x__4429)): (x_1:bool[
x_1] ->
X)
abst_arg: x__4429, x_1:bool[x_1]
abst_arg: x__4429, x_1:bool[x_1]
abstract_term: (k_check_2886 x__4429): X
abstract_term: x__4429: x_1:bool[x_1]
cond: (xs22_4051 >= xs12_4049); xs21_4050; xs11_4048; true
pbs: x__4429 := x__4429;
     xs22_4051 := ((((not xs11_4048) || (not xs21_4050)) || (not (0 <= 0))) || (xs12_4049 <= xs22_4051))
p:x__4429
tt:x__4429
ff:false

abstract_term: (fun (x1_4359:int) (x2_4360:int)
                    (k_check_xs'xs'_4361:(x_1:bool ->
                                          x_2:int ->
                                          x_3:bool ->
                                          x_4:int[(not x_1) || (not x_3) || (not (x1_4359 <= x2_4360)) || x_2 <= x_4]
                                          -> X))
                ->
                (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1)
                  (fun (x__4430:bool) (x__4431:int) (x__4432:bool)
                       (x__4433:x_1:int[(not x__4430) || (not x__4432) || (
                                        not ((x1_4359 + 1) <= (x2_4360 + 1))) ||
                                        x__4431 <= x_1])
                   -> (k_check_xs'xs'_4361 x__4430 x__4431 x__4432 x__4433)))): (
x_1:int ->
x_2:int ->
(x_4:bool -> x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X) -> X)
abst_arg: x1_4359, int
abst_arg: x2_4360, int
abst_arg: k_check_xs'xs'_4361, (x_1:bool ->
                                x_2:int ->
                                x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x1_4359 <= x2_4360)) || x_2 <= x_4]
                                -> X)
abst_arg: x1_4359, int
abst_arg: x2_4360, int
abst_arg: k_check_xs'xs'_4361, (x_1:bool ->
                                x_2:int ->
                                x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x1_4359 <= x2_4360)) || x_2 <= x_4]
                                -> X)
abstract_term: (xsxs_1051 (x1_4359 + 1) (x2_4360 + 1)
                 (fun (x__4430:bool) (x__4431:int) (x__4432:bool)
                      (x__4433:x_1:int[(not x__4430) || (not x__4432) || (
                                       not ((x1_4359 + 1) <= (x2_4360 + 1))) ||
                                       x__4431 <= x_1])
                  -> (k_check_xs'xs'_4361 x__4430 x__4431 x__4432 x__4433))): X
abstract_term: (fun (x__4430:bool) (x__4431:int) (x__4432:bool)
                    (x__4433:x_1:int[(not x__4430) || (not x__4432) || (
                                     not ((x1_4359 + 1) <= (x2_4360 + 1))) ||
                                     x__4431 <= x_1])
                -> (k_check_xs'xs'_4361 x__4430 x__4431 x__4432 x__4433)): (
x_1:bool ->
x_2:int -> x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not ((x1_4359 + 1) <= (x2_4360 + 1))) || x_2 <= x_4] -> X)
abst_arg: x__4430, bool
abst_arg: x__4431, int
abst_arg: x__4432, bool
abst_arg: x__4433, x_1:int[(not x__4430) || (not x__4432) || (not ((x1_4359 + 1) <= (x2_4360 + 1))) || x__4431 <= x_1]
abst_arg: x__4430, bool
abst_arg: x__4431, int
abst_arg: x__4432, bool
abst_arg: x__4433, x_1:int[(not x__4430) || (not x__4432) || (not ((x1_4359 + 1) <= (x2_4360 + 1))) || x__4431 <= x_1]
abstract_term: (k_check_xs'xs'_4361 x__4430 x__4431 x__4432 x__4433): X
abstract_term: x__4433: x_1:int[(not x__4430) || (not x__4432) || (not (x1_4359 <= x2_4360)) || x__4431 <= x_1]
cond: (xs22_4051 >= xs12_4049); xs21_4050; xs11_4048; true
pbs: x__4433 := ((((not x__4430) || (not x__4432)) || (not ((x1_4359 + 1) <= (x2_4360 + 1)))) || (x__4431 <= x__4433));
     xs22_4051 := ((((not xs11_4048) || (not xs21_4050)) || (not (0 <= 0))) || (xs12_4049 <= xs22_4051))
p:((((not x__4430) || (not x__4432)) || (not (x1_4359 <= x2_4360))) || (x__4431 <= x__4433))
tt:x__4433
ff:false

abstract_term: x__4432: bool
abstract_term: x__4431: int
abstract_term: x__4430: bool
abstract_term: (x2_4360 + 1): int
abstract_term: (x1_4359 + 1): int
abstract_term: (l1 (k_check_2886 false)): X
abstract_term: (k_check_2886 false): X
abstract_term: false: x_1:bool[x_1]
cond: (not (xs22_4051 >= xs12_4049)); xs21_4050; xs11_4048; true
pbs: xs22_4051 := ((((not xs11_4048) || (not xs21_4050)) || (not (0 <= 0))) || (xs12_4049 <= xs22_4051))
p:false
tt:false
ff:true

abstract_term: (l0 (k_check_2886 true)): X
abstract_term: (k_check_2886 true): X
abstract_term: true: x_1:bool[x_1]
cond: (not xs21_4050); xs11_4048; true
pbs: xs22_4051 := ((((not xs11_4048) || (not xs21_4050)) || (not (0 <= 0))) || (xs12_4049 <= xs22_4051))
p:true
tt:true
ff:false

abstract_term: (l0 (k_check_2886 true)): X
abstract_term: (k_check_2886 true): X
abstract_term: true: x_1:bool[x_1]
cond: (not xs11_4048); true
pbs: xs22_4051 := ((((not xs11_4048) || (not xs21_4050)) || (not (0 <= 0))) || (xs12_4049 <= xs22_4051))
p:true
tt:true
ff:false

abstract_term: 0: int
abstract_term: 0: int
fail_3437: ENV: b:bool, k:(unit -> X),


abst_arg: b, bool
abst_arg: k, (unit ->
X)
abst_arg: b, bool
abst_arg: k, (unit ->
X)
fail_3437: (k ()) ===> (k ())
fail_3437:: (k ())
abstract_term: (k ()): X
abstract_term: (): unit
insert_1013: ENV: x_1050:int,
ysys_1015:(x_1:int ->
           x_2:int ->
           (x_4:bool -> x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
           -> X),
k_insert_2134:((x_2:int ->
                x_3:int ->
                (x_5:bool ->
                 x_6:int -> x_7:bool -> x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_6 <= x_8] -> X)
                -> X) -> X),


abst_arg: x_1050, int
abst_arg: ysys_1015, (x_1:int ->
                      x_2:int ->
                      (x_4:bool ->
                       x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                      -> X)
abst_arg: k_insert_2134, ((x_2:int ->
                           x_3:int ->
                           (x_5:bool ->
                            x_6:int ->
                            x_7:bool -> x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_6 <= x_8] -> X)
                           -> X) ->
X)
abst_arg: x_1050, int
abst_arg: ysys_1015, (x_1:int ->
                      x_2:int ->
                      (x_4:bool ->
                       x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                      -> X)
abst_arg: k_insert_2134, ((x_2:int ->
                           x_3:int ->
                           (x_5:bool ->
                            x_6:int ->
                            x_7:bool -> x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_6 <= x_8] -> X)
                           -> X) ->
X)
insert_1013: (ysys_1015 0 0
               (fun (p11_4248:bool) (p12_4249:int) (p21_4250:bool)
                    (p22_4251:x_1:int[(not p11_4248) || (not p21_4250) || (not (0 <= 0)) || p12_4249 <= x_1])
                ->
                (if p11_4248
                  (l1
                    (if (x_1050 < p12_4249)
                      (l0
                        (k_insert_2134
                          (fun (x1_4073:int) (x2_4074:int)
                               (k_insert_4075:(x_1:bool ->
                                               x_2:int ->
                                               x_3:bool ->
                                               x_4:int[(not x_1) || (
                                                       not x_3) || (not (x1_4073 <= x2_4074)) ||
                                                       x_2 <= x_4]
                                               -> X))
                           ->
                           (if (x1_4073 <= 0)
                             (l0
                               (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
                                 (l1
                                   (ysys_1015 0 (x2_4074 - 1)
                                     (fun (xs11_4092:bool) (xs12_4093:int) (xs21_4094:bool)
                                          (xs22_4095:x_1:int[(not xs11_4092) || (
                                                             not xs21_4094) || (
                                                             not (0 <= (x2_4074 - 1))) ||
                                                             xs12_4093 <= x_1])
                                      ->
                                      (if (p11_4248 <=> xs11_4092)
                                        (l1
                                          (if (xs12_4093 = p12_4249) (
                                            l1 (k_insert_4075 true x_1050 true xs22_4095))
                                            (l0
                                              (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092
                                                xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 ()
                                                (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
                                                 (k_insert_4075 x__4450 x__4451 x__4452 x__4453))))))
                                        (l0
                                          (l0
                                            (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248
                                              xs21_4094 xs22_4095 p12_4249 ()
                                              (fun (x__4446:bool) (x__4447:int) (x__4448:bool) (x__4449:int) ->
                                               (k_insert_4075 x__4446 x__4447 x__4448 x__4449)))))))))))
                             (l1
                               (if (x2_4074 <= 0)
                                 (l0
                                   (ysys_1015 (x1_4073 - 1) 0
                                     (fun (p11_4102:bool) (p12_4103:int) (p21_4104:bool)
                                          (p22_4105:x_1:int[(not p11_4102) || (
                                                            not p21_4104) || (
                                                            not ((x1_4073 - 1) <= 0)) ||
                                                            p12_4103 <= x_1])
                                      -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
                                 (l1
                                   (ysys_1015 (x1_4073 - 1) (x2_4074 - 1)
                                     (fun (x__4442:bool) (x__4443:int) (x__4444:bool)
                                          (x__4445:x_1:int[(not x__4442) || (
                                                           not x__4444) || (
                                                           not ((x1_4073 - 1) <= (x2_4074 - 1))) ||
                                                           x__4443 <= x_1])
                                      -> (k_insert_4075 x__4442 x__4443 x__4444 x__4445))))))))))
                      (l1
                        (insert_1013 x_1050
                          (fun (x1_4416:int) (x2_4417:int)
                               (k_insert_ys'ys'_4418:(x_1:bool ->
                                                      x_2:int ->
                                                      x_3:bool ->
                                                      x_4:int[(not x_1) || (
                                                              not x_3) || (
                                                              not (x1_4416 <= x2_4417)) ||
                                                              x_2 <= x_4]
                                                      -> X))
                           ->
                           (ysys_1015 (x1_4416 + 1) (x2_4417 + 1)
                             (fun (x__4438:bool) (x__4439:int) (x__4440:bool)
                                  (x__4441:x_1:int[(not x__4438) || (
                                                   not x__4440) || (not ((x1_4416 + 1) <= (x2_4417 + 1))) ||
                                                   x__4439 <= x_1])
                              -> (k_insert_ys'ys'_4418 x__4438 x__4439 x__4440 x__4441))))
                          (fun (ys''ys''_4111:(x_1:int ->
                                               x_2:int ->
                                               (x_4:bool ->
                                                x_5:int ->
                                                x_6:bool ->
                                                x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] ->
                                                X)
                                               -> X))
                           ->
                           (k_insert_2134
                             (fun (x1_4125:int) (x2_4126:int)
                                  (k_insert_4127:(x_1:bool ->
                                                  x_2:int ->
                                                  x_3:bool ->
                                                  x_4:int[(not x_1) || (
                                                          not x_3) || (
                                                          not (x1_4125 <= x2_4126)) ||
                                                          x_2 <= x_4]
                                                  -> X))
                              ->
                              (if (x1_4125 = 0)
                                (l0
                                  (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                                    (l1
                                      (ysys_1015 0 (x2_4126 - 1)
                                        (fun (p11_4134:bool) (p12_4135:int) (p21_4136:bool)
                                             (p22_4137:x_1:int[(not p11_4134) || (
                                                               not p21_4136) || (
                                                               not (0 <= (x2_4126 - 1))) ||
                                                               p12_4135 <= x_1])
                                         -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
                                (l1
                                  (if (x2_4126 = 0)
                                    (l0
                                      (ys''ys''_4111 x1_4125 0
                                        (fun (p11_4144:bool) (p12_4145:int) (p21_4146:bool)
                                             (p22_4147:x_1:int[(not p11_4144) || (
                                                               not p21_4146) || (
                                                               not (x1_4125 <= 0)) ||
                                                               p12_4145 <= x_1])
                                         -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                                    (l1
                                      (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1)
                                        (fun (x__4434:bool) (x__4435:int) (x__4436:bool)
                                             (x__4437:x_1:int[(not x__4434) || (
                                                              not x__4436) ||
                                                              (not ((x1_4125 - 1) <= (x2_4126 - 1))) ||
                                                              x__4435 <= x_1])
                                         -> (k_insert_4127 x__4434 x__4435 x__4436 x__4437))))))))))))))
                  (l0
                    (k_insert_2134
                      (fun (x1_4343:int) (x2_4344:int)
                           (k_insert_rsrs_4345:(x_1:bool ->
                                                x_2:int ->
                                                x_3:bool ->
                                                x_4:int[(not x_1) || (
                                                        not x_3) || (
                                                        not (x1_4343 <= x2_4344)) ||
                                                        x_2 <= x_4]
                                                -> X))
                       ->
                       (if (x2_4344 = x1_4343)
                         (l0
                           (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                             (l1 (k_insert_rsrs_4345 false 0 false 0))))
                         (l1
                           (if (x1_4343 = 0)
                             (l0
                               (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                                 (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
                             (l1
                               (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050))
                                 (l1 (k_insert_rsrs_4345 false 0 false 0))))))))))))) ===> (
ysys_1015 0 0
 (fun (p11_4248:bool) (p12_4249:int) (p21_4250:bool)
      (p22_4251:x_1:int[(not p11_4248) || (not p21_4250) || (not (0 <= 0)) || p12_4249 <= x_1])
  ->
  (if p11_4248
    (l1
      (if (x_1050 < p12_4249)
        (l0
          (k_insert_2134
            (fun (x1_4073:int) (x2_4074:int)
                 (k_insert_4075:(x_1:bool ->
                                 x_2:int ->
                                 x_3:bool ->
                                 x_4:int[(not x_1) || (not x_3) || (not (x1_4073 <= x2_4074)) || x_2 <= x_4] -> X))
             ->
             (if (x1_4073 <= 0)
               (l0
                 (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
                   (l1
                     (ysys_1015 0 (x2_4074 - 1)
                       (fun (xs11_4092:bool) (xs12_4093:int) (xs21_4094:bool)
                            (xs22_4095:x_1:int[(not xs11_4092) || (not xs21_4094) || (
                                               not (0 <= (x2_4074 - 1))) ||
                                               xs12_4093 <= x_1])
                        ->
                        (if (p11_4248 <=> xs11_4092)
                          (l1
                            (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                              (l0
                                (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093
                                  p11_4248 xs21_4094 xs22_4095 p12_4249 ()
                                  (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
                                   (k_insert_4075 x__4450 x__4451 x__4452 x__4453))))))
                          (l0
                            (l0
                              (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095
                                p12_4249 ()
                                (fun (x__4446:bool) (x__4447:int) (x__4448:bool) (x__4449:int) ->
                                 (k_insert_4075 x__4446 x__4447 x__4448 x__4449)))))))))))
               (l1
                 (if (x2_4074 <= 0)
                   (l0
                     (ysys_1015 (x1_4073 - 1) 0
                       (fun (p11_4102:bool) (p12_4103:int) (p21_4104:bool)
                            (p22_4105:x_1:int[(not p11_4102) || (not p21_4104) || (
                                              not ((x1_4073 - 1) <= 0)) ||
                                              p12_4103 <= x_1])
                        -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
                   (l1
                     (ysys_1015 (x1_4073 - 1) (x2_4074 - 1)
                       (fun (x__4442:bool) (x__4443:int) (x__4444:bool)
                            (x__4445:x_1:int[(not x__4442) || (not x__4444) ||
                                             (not ((x1_4073 - 1) <= (x2_4074 - 1))) ||
                                             x__4443 <= x_1])
                        -> (k_insert_4075 x__4442 x__4443 x__4444 x__4445))))))))))
        (l1
          (insert_1013 x_1050
            (fun (x1_4416:int) (x2_4417:int)
                 (k_insert_ys'ys'_4418:(x_1:bool ->
                                        x_2:int ->
                                        x_3:bool ->
                                        x_4:int[(not x_1) || (not x_3) || (not (x1_4416 <= x2_4417)) || x_2 <= x_4] ->
                                        X))
             ->
             (ysys_1015 (x1_4416 + 1) (x2_4417 + 1)
               (fun (x__4438:bool) (x__4439:int) (x__4440:bool)
                    (x__4441:x_1:int[(not x__4438) || (not x__4440) || (
                                     not ((x1_4416 + 1) <= (x2_4417 + 1))) ||
                                     x__4439 <= x_1])
                -> (k_insert_ys'ys'_4418 x__4438 x__4439 x__4440 x__4441))))
            (fun (ys''ys''_4111:(x_1:int ->
                                 x_2:int ->
                                 (x_4:bool ->
                                  x_5:int ->
                                  x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                                 -> X))
             ->
             (k_insert_2134
               (fun (x1_4125:int) (x2_4126:int)
                    (k_insert_4127:(x_1:bool ->
                                    x_2:int ->
                                    x_3:bool ->
                                    x_4:int[(not x_1) || (not x_3) || (not (x1_4125 <= x2_4126)) || x_2 <= x_4] -> X))
                ->
                (if (x1_4125 = 0)
                  (l0
                    (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                      (l1
                        (ysys_1015 0 (x2_4126 - 1)
                          (fun (p11_4134:bool) (p12_4135:int) (p21_4136:bool)
                               (p22_4137:x_1:int[(not p11_4134) || (not p21_4136) || (
                                                 not (0 <= (x2_4126 - 1))) ||
                                                 p12_4135 <= x_1])
                           -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
                  (l1
                    (if (x2_4126 = 0)
                      (l0
                        (ys''ys''_4111 x1_4125 0
                          (fun (p11_4144:bool) (p12_4145:int) (p21_4146:bool)
                               (p22_4147:x_1:int[(not p11_4144) || (not p21_4146) || (
                                                 not (x1_4125 <= 0)) ||
                                                 p12_4145 <= x_1])
                           -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                      (l1
                        (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1)
                          (fun (x__4434:bool) (x__4435:int) (x__4436:bool)
                               (x__4437:x_1:int[(not x__4434) || (not x__4436) ||
                                                (not ((x1_4125 - 1) <= (x2_4126 - 1))) ||
                                                x__4435 <= x_1])
                           -> (k_insert_4127 x__4434 x__4435 x__4436 x__4437))))))))))))))
    (l0
      (k_insert_2134
        (fun (x1_4343:int) (x2_4344:int)
             (k_insert_rsrs_4345:(x_1:bool ->
                                  x_2:int ->
                                  x_3:bool ->
                                  x_4:int[(not x_1) || (not x_3) || (not (x1_4343 <= x2_4344)) || x_2 <= x_4] -> X))
         ->
         (if (x2_4344 = x1_4343)
           (l0
             (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
               (l1 (k_insert_rsrs_4345 false 0 false 0))))
           (l1
             (if (x1_4343 = 0)
               (l0
                 (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                   (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
               (l1
                 (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050))
                   (l1 (k_insert_rsrs_4345 false 0 false 0)))))))))))))
insert_1013:: (ysys_1015 0 0
                (fun (p11_4248:bool) (p12_4249:int) (p21_4250:bool)
                     (p22_4251:x_1:int[(not p11_4248) || (not p21_4250) || (not (0 <= 0)) || p12_4249 <= x_1])
                 ->
                 (if p11_4248
                   (l1
                     (if (x_1050 < p12_4249)
                       (l0
                         (k_insert_2134
                           (fun (x1_4073:int) (x2_4074:int)
                                (k_insert_4075:(x_1:bool ->
                                                x_2:int ->
                                                x_3:bool ->
                                                x_4:int[(not x_1) || (
                                                        not x_3) || (
                                                        not (x1_4073 <= x2_4074)) ||
                                                        x_2 <= x_4]
                                                -> X))
                            ->
                            (if (x1_4073 <= 0)
                              (l0
                                (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
                                  (l1
                                    (ysys_1015 0 (x2_4074 - 1)
                                      (fun (xs11_4092:bool) (xs12_4093:int) (xs21_4094:bool)
                                           (xs22_4095:x_1:int[(not xs11_4092) || (
                                                              not xs21_4094) || (
                                                              not (0 <= (x2_4074 - 1))) ||
                                                              xs12_4093 <= x_1])
                                       ->
                                       (if (p11_4248 <=> xs11_4092)
                                         (l1
                                           (if (xs12_4093 = p12_4249) (
                                             l1 (k_insert_4075 true x_1050 true xs22_4095))
                                             (l0
                                               (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092
                                                 xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 ()
                                                 (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
                                                  (k_insert_4075 x__4450 x__4451 x__4452 x__4453))))))
                                         (l0
                                           (l0
                                             (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248
                                               xs21_4094 xs22_4095 p12_4249 ()
                                               (fun (x__4446:bool) (x__4447:int) (x__4448:bool) (x__4449:int) ->
                                                (k_insert_4075 x__4446 x__4447 x__4448 x__4449)))))))))))
                              (l1
                                (if (x2_4074 <= 0)
                                  (l0
                                    (ysys_1015 (x1_4073 - 1) 0
                                      (fun (p11_4102:bool) (p12_4103:int) (p21_4104:bool)
                                           (p22_4105:x_1:int[(not p11_4102) || (
                                                             not p21_4104) || (
                                                             not ((x1_4073 - 1) <= 0)) ||
                                                             p12_4103 <= x_1])
                                       -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
                                  (l1
                                    (ysys_1015 (x1_4073 - 1) (x2_4074 - 1)
                                      (fun (x__4442:bool) (x__4443:int) (x__4444:bool)
                                           (x__4445:x_1:int[(not x__4442) || (
                                                            not x__4444) || (
                                                            not ((x1_4073 - 1) <= (x2_4074 - 1))) ||
                                                            x__4443 <= x_1])
                                       -> (k_insert_4075 x__4442 x__4443 x__4444 x__4445))))))))))
                       (l1
                         (insert_1013 x_1050
                           (fun (x1_4416:int) (x2_4417:int)
                                (k_insert_ys'ys'_4418:(x_1:bool ->
                                                       x_2:int ->
                                                       x_3:bool ->
                                                       x_4:int[(not x_1) || (
                                                               not x_3) || (
                                                               not (x1_4416 <= x2_4417)) ||
                                                               x_2 <= x_4]
                                                       -> X))
                            ->
                            (ysys_1015 (x1_4416 + 1) (x2_4417 + 1)
                              (fun (x__4438:bool) (x__4439:int) (x__4440:bool)
                                   (x__4441:x_1:int[(not x__4438) || (
                                                    not x__4440) || (
                                                    not ((x1_4416 + 1) <= (x2_4417 + 1))) ||
                                                    x__4439 <= x_1])
                               -> (k_insert_ys'ys'_4418 x__4438 x__4439 x__4440 x__4441))))
                           (fun (ys''ys''_4111:(x_1:int ->
                                                x_2:int ->
                                                (x_4:bool ->
                                                 x_5:int ->
                                                 x_6:bool ->
                                                 x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] ->
                                                 X)
                                                -> X))
                            ->
                            (k_insert_2134
                              (fun (x1_4125:int) (x2_4126:int)
                                   (k_insert_4127:(x_1:bool ->
                                                   x_2:int ->
                                                   x_3:bool ->
                                                   x_4:int[(not x_1) || (
                                                           not x_3) || (
                                                           not (x1_4125 <= x2_4126)) ||
                                                           x_2 <= x_4]
                                                   -> X))
                               ->
                               (if (x1_4125 = 0)
                                 (l0
                                   (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                                     (l1
                                       (ysys_1015 0 (x2_4126 - 1)
                                         (fun (p11_4134:bool) (p12_4135:int) (p21_4136:bool)
                                              (p22_4137:x_1:int[(not p11_4134) || (
                                                                not p21_4136) || (
                                                                not (0 <= (x2_4126 - 1))) ||
                                                                p12_4135 <= x_1])
                                          -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
                                 (l1
                                   (if (x2_4126 = 0)
                                     (l0
                                       (ys''ys''_4111 x1_4125 0
                                         (fun (p11_4144:bool) (p12_4145:int) (p21_4146:bool)
                                              (p22_4147:x_1:int[(not p11_4144) || (
                                                                not p21_4146) || (
                                                                not (x1_4125 <= 0)) ||
                                                                p12_4145 <= x_1])
                                          -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                                     (l1
                                       (ys''ys''_4111 (x1_4125 - 1) (
                                         x2_4126 - 1)
                                         (fun (x__4434:bool) (x__4435:int) (x__4436:bool)
                                              (x__4437:x_1:int[(not x__4434) || (
                                                               not x__4436) ||
                                                               (not ((x1_4125 - 1) <= (x2_4126 - 1))) ||
                                                               x__4435 <= x_1])
                                          -> (k_insert_4127 x__4434 x__4435 x__4436 x__4437))))))))))))))
                   (l0
                     (k_insert_2134
                       (fun (x1_4343:int) (x2_4344:int)
                            (k_insert_rsrs_4345:(x_1:bool ->
                                                 x_2:int ->
                                                 x_3:bool ->
                                                 x_4:int[(not x_1) || (
                                                         not x_3) || (
                                                         not (x1_4343 <= x2_4344)) ||
                                                         x_2 <= x_4]
                                                 -> X))
                        ->
                        (if (x2_4344 = x1_4343)
                          (l0
                            (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                              (l1 (k_insert_rsrs_4345 false 0 false 0))))
                          (l1
                            (if (x1_4343 = 0)
                              (l0
                                (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                                  (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
                              (l1
                                (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050))
                                  (l1 (k_insert_rsrs_4345 false 0 false 0)))))))))))))
abstract_term: (ysys_1015 0 0
                 (fun (p11_4248:bool) (p12_4249:int) (p21_4250:bool)
                      (p22_4251:x_1:int[(not p11_4248) || (not p21_4250) || (not (0 <= 0)) || p12_4249 <= x_1])
                  ->
                  (if p11_4248
                    (l1
                      (if (x_1050 < p12_4249)
                        (l0
                          (k_insert_2134
                            (fun (x1_4073:int) (x2_4074:int)
                                 (k_insert_4075:(x_1:bool ->
                                                 x_2:int ->
                                                 x_3:bool ->
                                                 x_4:int[(not x_1) || (
                                                         not x_3) || (
                                                         not (x1_4073 <= x2_4074)) ||
                                                         x_2 <= x_4]
                                                 -> X))
                             ->
                             (if (x1_4073 <= 0)
                               (l0
                                 (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
                                   (l1
                                     (ysys_1015 0 (x2_4074 - 1)
                                       (fun (xs11_4092:bool) (xs12_4093:int) (xs21_4094:bool)
                                            (xs22_4095:x_1:int[(not xs11_4092) || (
                                                               not xs21_4094) || (
                                                               not (0 <= (x2_4074 - 1))) ||
                                                               xs12_4093 <= x_1])
                                        ->
                                        (if (p11_4248 <=> xs11_4092)
                                          (l1
                                            (if (xs12_4093 = p12_4249) (
                                              l1 (k_insert_4075 true x_1050 true xs22_4095))
                                              (l0
                                                (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050
                                                  xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 ()
                                                  (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
                                                   (k_insert_4075 x__4450 x__4451 x__4452 x__4453))))))
                                          (l0
                                            (l0
                                              (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248
                                                xs21_4094 xs22_4095 p12_4249 ()
                                                (fun (x__4446:bool) (x__4447:int) (x__4448:bool) (x__4449:int) ->
                                                 (k_insert_4075 x__4446 x__4447 x__4448 x__4449)))))))))))
                               (l1
                                 (if (x2_4074 <= 0)
                                   (l0
                                     (ysys_1015 (x1_4073 - 1) 0
                                       (fun (p11_4102:bool) (p12_4103:int) (p21_4104:bool)
                                            (p22_4105:x_1:int[(not p11_4102) || (
                                                              not p21_4104) || (
                                                              not ((x1_4073 - 1) <= 0)) ||
                                                              p12_4103 <= x_1])
                                        -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
                                   (l1
                                     (ysys_1015 (x1_4073 - 1) (x2_4074 - 1)
                                       (fun (x__4442:bool) (x__4443:int) (x__4444:bool)
                                            (x__4445:x_1:int[(not x__4442) || (
                                                             not x__4444) || (
                                                             not ((x1_4073 - 1) <= (x2_4074 - 1))) ||
                                                             x__4443 <= x_1])
                                        -> (k_insert_4075 x__4442 x__4443 x__4444 x__4445))))))))))
                        (l1
                          (insert_1013 x_1050
                            (fun (x1_4416:int) (x2_4417:int)
                                 (k_insert_ys'ys'_4418:(x_1:bool ->
                                                        x_2:int ->
                                                        x_3:bool ->
                                                        x_4:int[(not x_1) || (
                                                                not x_3) || (
                                                                not (x1_4416 <= x2_4417)) ||
                                                                x_2 <= x_4]
                                                        -> X))
                             ->
                             (ysys_1015 (x1_4416 + 1) (x2_4417 + 1)
                               (fun (x__4438:bool) (x__4439:int) (x__4440:bool)
                                    (x__4441:x_1:int[(not x__4438) || (
                                                     not x__4440) || (
                                                     not ((x1_4416 + 1) <= (x2_4417 + 1))) ||
                                                     x__4439 <= x_1])
                                -> (k_insert_ys'ys'_4418 x__4438 x__4439 x__4440 x__4441))))
                            (fun (ys''ys''_4111:(x_1:int ->
                                                 x_2:int ->
                                                 (x_4:bool ->
                                                  x_5:int ->
                                                  x_6:bool ->
                                                  x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7]
                                                  -> X)
                                                 -> X))
                             ->
                             (k_insert_2134
                               (fun (x1_4125:int) (x2_4126:int)
                                    (k_insert_4127:(x_1:bool ->
                                                    x_2:int ->
                                                    x_3:bool ->
                                                    x_4:int[(not x_1) || (
                                                            not x_3) || (
                                                            not (x1_4125 <= x2_4126)) ||
                                                            x_2 <= x_4]
                                                    -> X))
                                ->
                                (if (x1_4125 = 0)
                                  (l0
                                    (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                                      (l1
                                        (ysys_1015 0 (x2_4126 - 1)
                                          (fun (p11_4134:bool) (p12_4135:int) (p21_4136:bool)
                                               (p22_4137:x_1:int[(not p11_4134) || (
                                                                 not p21_4136) || (
                                                                 not (0 <= (x2_4126 - 1))) ||
                                                                 p12_4135 <= x_1])
                                           -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
                                  (l1
                                    (if (x2_4126 = 0)
                                      (l0
                                        (ys''ys''_4111 x1_4125 0
                                          (fun (p11_4144:bool) (p12_4145:int) (p21_4146:bool)
                                               (p22_4147:x_1:int[(not p11_4144) || (
                                                                 not p21_4146) || (
                                                                 not (x1_4125 <= 0)) ||
                                                                 p12_4145 <= x_1])
                                           -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                                      (l1
                                        (ys''ys''_4111 (x1_4125 - 1) (
                                          x2_4126 - 1)
                                          (fun (x__4434:bool) (x__4435:int) (x__4436:bool)
                                               (x__4437:x_1:int[(not x__4434) || (
                                                                not x__4436) ||
                                                                (not ((x1_4125 - 1) <= (x2_4126 - 1))) ||
                                                                x__4435 <= x_1])
                                           -> (k_insert_4127 x__4434 x__4435 x__4436 x__4437))))))))))))))
                    (l0
                      (k_insert_2134
                        (fun (x1_4343:int) (x2_4344:int)
                             (k_insert_rsrs_4345:(x_1:bool ->
                                                  x_2:int ->
                                                  x_3:bool ->
                                                  x_4:int[(not x_1) || (
                                                          not x_3) || (
                                                          not (x1_4343 <= x2_4344)) ||
                                                          x_2 <= x_4]
                                                  -> X))
                         ->
                         (if (x2_4344 = x1_4343)
                           (l0
                             (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                               (l1 (k_insert_rsrs_4345 false 0 false 0))))
                           (l1
                             (if (x1_4343 = 0)
                               (l0
                                 (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                                   (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
                               (l1
                                 (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050))
                                   (l1 (k_insert_rsrs_4345 false 0 false 0))))))))))))): X
abstract_term: (fun (p11_4248:bool) (p12_4249:int) (p21_4250:bool)
                    (p22_4251:x_1:int[(not p11_4248) || (not p21_4250) || (not (0 <= 0)) || p12_4249 <= x_1])
                ->
                (if p11_4248
                  (l1
                    (if (x_1050 < p12_4249)
                      (l0
                        (k_insert_2134
                          (fun (x1_4073:int) (x2_4074:int)
                               (k_insert_4075:(x_1:bool ->
                                               x_2:int ->
                                               x_3:bool ->
                                               x_4:int[(not x_1) || (
                                                       not x_3) || (not (x1_4073 <= x2_4074)) ||
                                                       x_2 <= x_4]
                                               -> X))
                           ->
                           (if (x1_4073 <= 0)
                             (l0
                               (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
                                 (l1
                                   (ysys_1015 0 (x2_4074 - 1)
                                     (fun (xs11_4092:bool) (xs12_4093:int) (xs21_4094:bool)
                                          (xs22_4095:x_1:int[(not xs11_4092) || (
                                                             not xs21_4094) || (
                                                             not (0 <= (x2_4074 - 1))) ||
                                                             xs12_4093 <= x_1])
                                      ->
                                      (if (p11_4248 <=> xs11_4092)
                                        (l1
                                          (if (xs12_4093 = p12_4249) (
                                            l1 (k_insert_4075 true x_1050 true xs22_4095))
                                            (l0
                                              (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092
                                                xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 ()
                                                (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
                                                 (k_insert_4075 x__4450 x__4451 x__4452 x__4453))))))
                                        (l0
                                          (l0
                                            (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248
                                              xs21_4094 xs22_4095 p12_4249 ()
                                              (fun (x__4446:bool) (x__4447:int) (x__4448:bool) (x__4449:int) ->
                                               (k_insert_4075 x__4446 x__4447 x__4448 x__4449)))))))))))
                             (l1
                               (if (x2_4074 <= 0)
                                 (l0
                                   (ysys_1015 (x1_4073 - 1) 0
                                     (fun (p11_4102:bool) (p12_4103:int) (p21_4104:bool)
                                          (p22_4105:x_1:int[(not p11_4102) || (
                                                            not p21_4104) || (
                                                            not ((x1_4073 - 1) <= 0)) ||
                                                            p12_4103 <= x_1])
                                      -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
                                 (l1
                                   (ysys_1015 (x1_4073 - 1) (x2_4074 - 1)
                                     (fun (x__4442:bool) (x__4443:int) (x__4444:bool)
                                          (x__4445:x_1:int[(not x__4442) || (
                                                           not x__4444) || (
                                                           not ((x1_4073 - 1) <= (x2_4074 - 1))) ||
                                                           x__4443 <= x_1])
                                      -> (k_insert_4075 x__4442 x__4443 x__4444 x__4445))))))))))
                      (l1
                        (insert_1013 x_1050
                          (fun (x1_4416:int) (x2_4417:int)
                               (k_insert_ys'ys'_4418:(x_1:bool ->
                                                      x_2:int ->
                                                      x_3:bool ->
                                                      x_4:int[(not x_1) || (
                                                              not x_3) || (
                                                              not (x1_4416 <= x2_4417)) ||
                                                              x_2 <= x_4]
                                                      -> X))
                           ->
                           (ysys_1015 (x1_4416 + 1) (x2_4417 + 1)
                             (fun (x__4438:bool) (x__4439:int) (x__4440:bool)
                                  (x__4441:x_1:int[(not x__4438) || (
                                                   not x__4440) || (not ((x1_4416 + 1) <= (x2_4417 + 1))) ||
                                                   x__4439 <= x_1])
                              -> (k_insert_ys'ys'_4418 x__4438 x__4439 x__4440 x__4441))))
                          (fun (ys''ys''_4111:(x_1:int ->
                                               x_2:int ->
                                               (x_4:bool ->
                                                x_5:int ->
                                                x_6:bool ->
                                                x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] ->
                                                X)
                                               -> X))
                           ->
                           (k_insert_2134
                             (fun (x1_4125:int) (x2_4126:int)
                                  (k_insert_4127:(x_1:bool ->
                                                  x_2:int ->
                                                  x_3:bool ->
                                                  x_4:int[(not x_1) || (
                                                          not x_3) || (
                                                          not (x1_4125 <= x2_4126)) ||
                                                          x_2 <= x_4]
                                                  -> X))
                              ->
                              (if (x1_4125 = 0)
                                (l0
                                  (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                                    (l1
                                      (ysys_1015 0 (x2_4126 - 1)
                                        (fun (p11_4134:bool) (p12_4135:int) (p21_4136:bool)
                                             (p22_4137:x_1:int[(not p11_4134) || (
                                                               not p21_4136) || (
                                                               not (0 <= (x2_4126 - 1))) ||
                                                               p12_4135 <= x_1])
                                         -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
                                (l1
                                  (if (x2_4126 = 0)
                                    (l0
                                      (ys''ys''_4111 x1_4125 0
                                        (fun (p11_4144:bool) (p12_4145:int) (p21_4146:bool)
                                             (p22_4147:x_1:int[(not p11_4144) || (
                                                               not p21_4146) || (
                                                               not (x1_4125 <= 0)) ||
                                                               p12_4145 <= x_1])
                                         -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                                    (l1
                                      (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1)
                                        (fun (x__4434:bool) (x__4435:int) (x__4436:bool)
                                             (x__4437:x_1:int[(not x__4434) || (
                                                              not x__4436) ||
                                                              (not ((x1_4125 - 1) <= (x2_4126 - 1))) ||
                                                              x__4435 <= x_1])
                                         -> (k_insert_4127 x__4434 x__4435 x__4436 x__4437))))))))))))))
                  (l0
                    (k_insert_2134
                      (fun (x1_4343:int) (x2_4344:int)
                           (k_insert_rsrs_4345:(x_1:bool ->
                                                x_2:int ->
                                                x_3:bool ->
                                                x_4:int[(not x_1) || (
                                                        not x_3) || (
                                                        not (x1_4343 <= x2_4344)) ||
                                                        x_2 <= x_4]
                                                -> X))
                       ->
                       (if (x2_4344 = x1_4343)
                         (l0
                           (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                             (l1 (k_insert_rsrs_4345 false 0 false 0))))
                         (l1
                           (if (x1_4343 = 0)
                             (l0
                               (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                                 (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
                             (l1
                               (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050))
                                 (l1 (k_insert_rsrs_4345 false 0 false 0)))))))))))): (
x_1:bool -> x_2:int -> x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (0 <= 0)) || x_2 <= x_4] -> X)
abst_arg: p11_4248, bool
abst_arg: p12_4249, int
abst_arg: p21_4250, bool
abst_arg: p22_4251, x_1:int[(not p11_4248) || (not p21_4250) || (not (0 <= 0)) || p12_4249 <= x_1]
abst_arg: p11_4248, bool
abst_arg: p12_4249, int
abst_arg: p21_4250, bool
abst_arg: p22_4251, x_1:int[(not p11_4248) || (not p21_4250) || (not (0 <= 0)) || p12_4249 <= x_1]
abstract_term: (if p11_4248
                 (l1
                   (if (x_1050 < p12_4249)
                     (l0
                       (k_insert_2134
                         (fun (x1_4073:int) (x2_4074:int)
                              (k_insert_4075:(x_1:bool ->
                                              x_2:int ->
                                              x_3:bool ->
                                              x_4:int[(not x_1) || (not x_3) || (
                                                      not (x1_4073 <= x2_4074)) ||
                                                      x_2 <= x_4]
                                              -> X))
                          ->
                          (if (x1_4073 <= 0)
                            (l0
                              (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
                                (l1
                                  (ysys_1015 0 (x2_4074 - 1)
                                    (fun (xs11_4092:bool) (xs12_4093:int) (xs21_4094:bool)
                                         (xs22_4095:x_1:int[(not xs11_4092) || (
                                                            not xs21_4094) || (
                                                            not (0 <= (x2_4074 - 1))) ||
                                                            xs12_4093 <= x_1])
                                     ->
                                     (if (p11_4248 <=> xs11_4092)
                                       (l1
                                         (if (xs12_4093 = p12_4249) (
                                           l1 (k_insert_4075 true x_1050 true xs22_4095))
                                           (l0
                                             (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092
                                               xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 ()
                                               (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
                                                (k_insert_4075 x__4450 x__4451 x__4452 x__4453))))))
                                       (l0
                                         (l0
                                           (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248
                                             xs21_4094 xs22_4095 p12_4249 ()
                                             (fun (x__4446:bool) (x__4447:int) (x__4448:bool) (x__4449:int) ->
                                              (k_insert_4075 x__4446 x__4447 x__4448 x__4449)))))))))))
                            (l1
                              (if (x2_4074 <= 0)
                                (l0
                                  (ysys_1015 (x1_4073 - 1) 0
                                    (fun (p11_4102:bool) (p12_4103:int) (p21_4104:bool)
                                         (p22_4105:x_1:int[(not p11_4102) || (
                                                           not p21_4104) || (
                                                           not ((x1_4073 - 1) <= 0)) ||
                                                           p12_4103 <= x_1])
                                     -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
                                (l1
                                  (ysys_1015 (x1_4073 - 1) (x2_4074 - 1)
                                    (fun (x__4442:bool) (x__4443:int) (x__4444:bool)
                                         (x__4445:x_1:int[(not x__4442) || (
                                                          not x__4444) || (
                                                          not ((x1_4073 - 1) <= (x2_4074 - 1))) ||
                                                          x__4443 <= x_1])
                                     -> (k_insert_4075 x__4442 x__4443 x__4444 x__4445))))))))))
                     (l1
                       (insert_1013 x_1050
                         (fun (x1_4416:int) (x2_4417:int)
                              (k_insert_ys'ys'_4418:(x_1:bool ->
                                                     x_2:int ->
                                                     x_3:bool ->
                                                     x_4:int[(not x_1) || (
                                                             not x_3) || (
                                                             not (x1_4416 <= x2_4417)) ||
                                                             x_2 <= x_4]
                                                     -> X))
                          ->
                          (ysys_1015 (x1_4416 + 1) (x2_4417 + 1)
                            (fun (x__4438:bool) (x__4439:int) (x__4440:bool)
                                 (x__4441:x_1:int[(not x__4438) || (not x__4440) ||
                                                  (not ((x1_4416 + 1) <= (x2_4417 + 1))) ||
                                                  x__4439 <= x_1])
                             -> (k_insert_ys'ys'_4418 x__4438 x__4439 x__4440 x__4441))))
                         (fun (ys''ys''_4111:(x_1:int ->
                                              x_2:int ->
                                              (x_4:bool ->
                                               x_5:int ->
                                               x_6:bool ->
                                               x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                                              -> X))
                          ->
                          (k_insert_2134
                            (fun (x1_4125:int) (x2_4126:int)
                                 (k_insert_4127:(x_1:bool ->
                                                 x_2:int ->
                                                 x_3:bool ->
                                                 x_4:int[(not x_1) || (
                                                         not x_3) || (
                                                         not (x1_4125 <= x2_4126)) ||
                                                         x_2 <= x_4]
                                                 -> X))
                             ->
                             (if (x1_4125 = 0)
                               (l0
                                 (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                                   (l1
                                     (ysys_1015 0 (x2_4126 - 1)
                                       (fun (p11_4134:bool) (p12_4135:int) (p21_4136:bool)
                                            (p22_4137:x_1:int[(not p11_4134) || (
                                                              not p21_4136) || (
                                                              not (0 <= (x2_4126 - 1))) ||
                                                              p12_4135 <= x_1])
                                        -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
                               (l1
                                 (if (x2_4126 = 0)
                                   (l0
                                     (ys''ys''_4111 x1_4125 0
                                       (fun (p11_4144:bool) (p12_4145:int) (p21_4146:bool)
                                            (p22_4147:x_1:int[(not p11_4144) || (
                                                              not p21_4146) || (
                                                              not (x1_4125 <= 0)) ||
                                                              p12_4145 <= x_1])
                                        -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                                   (l1
                                     (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1)
                                       (fun (x__4434:bool) (x__4435:int) (x__4436:bool)
                                            (x__4437:x_1:int[(not x__4434) || (
                                                             not x__4436) || (
                                                             not ((x1_4125 - 1) <= (x2_4126 - 1))) ||
                                                             x__4435 <= x_1])
                                        -> (k_insert_4127 x__4434 x__4435 x__4436 x__4437))))))))))))))
                 (l0
                   (k_insert_2134
                     (fun (x1_4343:int) (x2_4344:int)
                          (k_insert_rsrs_4345:(x_1:bool ->
                                               x_2:int ->
                                               x_3:bool ->
                                               x_4:int[(not x_1) || (
                                                       not x_3) || (not (x1_4343 <= x2_4344)) ||
                                                       x_2 <= x_4]
                                               -> X))
                      ->
                      (if (x2_4344 = x1_4343)
                        (l0
                          (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                            (l1 (k_insert_rsrs_4345 false 0 false 0))))
                        (l1
                          (if (x1_4343 = 0)
                            (l0
                              (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                                (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
                            (l1
                              (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050))
                                (l1 (k_insert_rsrs_4345 false 0 false 0))))))))))): X
abstract_term: p11_4248: x_1:bool[x_1]
cond: true
pbs: p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:p11_4248
tt:false
ff:false

abstract_term: (l1
                 (if (x_1050 < p12_4249)
                   (l0
                     (k_insert_2134
                       (fun (x1_4073:int) (x2_4074:int)
                            (k_insert_4075:(x_1:bool ->
                                            x_2:int ->
                                            x_3:bool ->
                                            x_4:int[(not x_1) || (not x_3) || (not (x1_4073 <= x2_4074)) || x_2 <= x_4]
                                            -> X))
                        ->
                        (if (x1_4073 <= 0)
                          (l0
                            (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
                              (l1
                                (ysys_1015 0 (x2_4074 - 1)
                                  (fun (xs11_4092:bool) (xs12_4093:int) (xs21_4094:bool)
                                       (xs22_4095:x_1:int[(not xs11_4092) || (
                                                          not xs21_4094) || (
                                                          not (0 <= (x2_4074 - 1))) ||
                                                          xs12_4093 <= x_1])
                                   ->
                                   (if (p11_4248 <=> xs11_4092)
                                     (l1
                                       (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                                         (l0
                                           (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092
                                             xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 ()
                                             (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
                                              (k_insert_4075 x__4450 x__4451 x__4452 x__4453))))))
                                     (l0
                                       (l0
                                         (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094
                                           xs22_4095 p12_4249 ()
                                           (fun (x__4446:bool) (x__4447:int) (x__4448:bool) (x__4449:int) ->
                                            (k_insert_4075 x__4446 x__4447 x__4448 x__4449)))))))))))
                          (l1
                            (if (x2_4074 <= 0)
                              (l0
                                (ysys_1015 (x1_4073 - 1) 0
                                  (fun (p11_4102:bool) (p12_4103:int) (p21_4104:bool)
                                       (p22_4105:x_1:int[(not p11_4102) || (
                                                         not p21_4104) || (
                                                         not ((x1_4073 - 1) <= 0)) ||
                                                         p12_4103 <= x_1])
                                   -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
                              (l1
                                (ysys_1015 (x1_4073 - 1) (x2_4074 - 1)
                                  (fun (x__4442:bool) (x__4443:int) (x__4444:bool)
                                       (x__4445:x_1:int[(not x__4442) || (
                                                        not x__4444) || (
                                                        not ((x1_4073 - 1) <= (x2_4074 - 1))) ||
                                                        x__4443 <= x_1])
                                   -> (k_insert_4075 x__4442 x__4443 x__4444 x__4445))))))))))
                   (l1
                     (insert_1013 x_1050
                       (fun (x1_4416:int) (x2_4417:int)
                            (k_insert_ys'ys'_4418:(x_1:bool ->
                                                   x_2:int ->
                                                   x_3:bool ->
                                                   x_4:int[(not x_1) || (
                                                           not x_3) || (
                                                           not (x1_4416 <= x2_4417)) ||
                                                           x_2 <= x_4]
                                                   -> X))
                        ->
                        (ysys_1015 (x1_4416 + 1) (x2_4417 + 1)
                          (fun (x__4438:bool) (x__4439:int) (x__4440:bool)
                               (x__4441:x_1:int[(not x__4438) || (not x__4440) ||
                                                (not ((x1_4416 + 1) <= (x2_4417 + 1))) ||
                                                x__4439 <= x_1])
                           -> (k_insert_ys'ys'_4418 x__4438 x__4439 x__4440 x__4441))))
                       (fun (ys''ys''_4111:(x_1:int ->
                                            x_2:int ->
                                            (x_4:bool ->
                                             x_5:int ->
                                             x_6:bool ->
                                             x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                                            -> X))
                        ->
                        (k_insert_2134
                          (fun (x1_4125:int) (x2_4126:int)
                               (k_insert_4127:(x_1:bool ->
                                               x_2:int ->
                                               x_3:bool ->
                                               x_4:int[(not x_1) || (
                                                       not x_3) || (not (x1_4125 <= x2_4126)) ||
                                                       x_2 <= x_4]
                                               -> X))
                           ->
                           (if (x1_4125 = 0)
                             (l0
                               (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                                 (l1
                                   (ysys_1015 0 (x2_4126 - 1)
                                     (fun (p11_4134:bool) (p12_4135:int) (p21_4136:bool)
                                          (p22_4137:x_1:int[(not p11_4134) || (
                                                            not p21_4136) || (
                                                            not (0 <= (x2_4126 - 1))) ||
                                                            p12_4135 <= x_1])
                                      -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
                             (l1
                               (if (x2_4126 = 0)
                                 (l0
                                   (ys''ys''_4111 x1_4125 0
                                     (fun (p11_4144:bool) (p12_4145:int) (p21_4146:bool)
                                          (p22_4147:x_1:int[(not p11_4144) || (
                                                            not p21_4146) || (
                                                            not (x1_4125 <= 0)) ||
                                                            p12_4145 <= x_1])
                                      -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                                 (l1
                                   (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1)
                                     (fun (x__4434:bool) (x__4435:int) (x__4436:bool)
                                          (x__4437:x_1:int[(not x__4434) || (
                                                           not x__4436) || (
                                                           not ((x1_4125 - 1) <= (x2_4126 - 1))) ||
                                                           x__4435 <= x_1])
                                      -> (k_insert_4127 x__4434 x__4435 x__4436 x__4437)))))))))))))): X
abstract_term: (if (x_1050 < p12_4249)
                 (l0
                   (k_insert_2134
                     (fun (x1_4073:int) (x2_4074:int)
                          (k_insert_4075:(x_1:bool ->
                                          x_2:int ->
                                          x_3:bool ->
                                          x_4:int[(not x_1) || (not x_3) || (not (x1_4073 <= x2_4074)) || x_2 <= x_4]
                                          -> X))
                      ->
                      (if (x1_4073 <= 0)
                        (l0
                          (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
                            (l1
                              (ysys_1015 0 (x2_4074 - 1)
                                (fun (xs11_4092:bool) (xs12_4093:int) (xs21_4094:bool)
                                     (xs22_4095:x_1:int[(not xs11_4092) || (
                                                        not xs21_4094) || (
                                                        not (0 <= (x2_4074 - 1))) ||
                                                        xs12_4093 <= x_1])
                                 ->
                                 (if (p11_4248 <=> xs11_4092)
                                   (l1
                                     (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                                       (l0
                                         (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092
                                           xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 ()
                                           (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
                                            (k_insert_4075 x__4450 x__4451 x__4452 x__4453))))))
                                   (l0
                                     (l0
                                       (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094
                                         xs22_4095 p12_4249 ()
                                         (fun (x__4446:bool) (x__4447:int) (x__4448:bool) (x__4449:int) ->
                                          (k_insert_4075 x__4446 x__4447 x__4448 x__4449)))))))))))
                        (l1
                          (if (x2_4074 <= 0)
                            (l0
                              (ysys_1015 (x1_4073 - 1) 0
                                (fun (p11_4102:bool) (p12_4103:int) (p21_4104:bool)
                                     (p22_4105:x_1:int[(not p11_4102) || (
                                                       not p21_4104) || (
                                                       not ((x1_4073 - 1) <= 0)) ||
                                                       p12_4103 <= x_1])
                                 -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
                            (l1
                              (ysys_1015 (x1_4073 - 1) (x2_4074 - 1)
                                (fun (x__4442:bool) (x__4443:int) (x__4444:bool)
                                     (x__4445:x_1:int[(not x__4442) || (
                                                      not x__4444) || (
                                                      not ((x1_4073 - 1) <= (x2_4074 - 1))) ||
                                                      x__4443 <= x_1])
                                 -> (k_insert_4075 x__4442 x__4443 x__4444 x__4445))))))))))
                 (l1
                   (insert_1013 x_1050
                     (fun (x1_4416:int) (x2_4417:int)
                          (k_insert_ys'ys'_4418:(x_1:bool ->
                                                 x_2:int ->
                                                 x_3:bool ->
                                                 x_4:int[(not x_1) || (
                                                         not x_3) || (
                                                         not (x1_4416 <= x2_4417)) ||
                                                         x_2 <= x_4]
                                                 -> X))
                      ->
                      (ysys_1015 (x1_4416 + 1) (x2_4417 + 1)
                        (fun (x__4438:bool) (x__4439:int) (x__4440:bool)
                             (x__4441:x_1:int[(not x__4438) || (not x__4440) ||
                                              (not ((x1_4416 + 1) <= (x2_4417 + 1))) ||
                                              x__4439 <= x_1])
                         -> (k_insert_ys'ys'_4418 x__4438 x__4439 x__4440 x__4441))))
                     (fun (ys''ys''_4111:(x_1:int ->
                                          x_2:int ->
                                          (x_4:bool ->
                                           x_5:int ->
                                           x_6:bool ->
                                           x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                                          -> X))
                      ->
                      (k_insert_2134
                        (fun (x1_4125:int) (x2_4126:int)
                             (k_insert_4127:(x_1:bool ->
                                             x_2:int ->
                                             x_3:bool ->
                                             x_4:int[(not x_1) || (not x_3) || (not (x1_4125 <= x2_4126)) || x_2 <= x_4]
                                             -> X))
                         ->
                         (if (x1_4125 = 0)
                           (l0
                             (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                               (l1
                                 (ysys_1015 0 (x2_4126 - 1)
                                   (fun (p11_4134:bool) (p12_4135:int) (p21_4136:bool)
                                        (p22_4137:x_1:int[(not p11_4134) || (
                                                          not p21_4136) || (
                                                          not (0 <= (x2_4126 - 1))) ||
                                                          p12_4135 <= x_1])
                                    -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
                           (l1
                             (if (x2_4126 = 0)
                               (l0
                                 (ys''ys''_4111 x1_4125 0
                                   (fun (p11_4144:bool) (p12_4145:int) (p21_4146:bool)
                                        (p22_4147:x_1:int[(not p11_4144) || (
                                                          not p21_4146) || (
                                                          not (x1_4125 <= 0)) ||
                                                          p12_4145 <= x_1])
                                    -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                               (l1
                                 (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1)
                                   (fun (x__4434:bool) (x__4435:int) (x__4436:bool)
                                        (x__4437:x_1:int[(not x__4434) || (
                                                         not x__4436) || (
                                                         not ((x1_4125 - 1) <= (x2_4126 - 1))) ||
                                                         x__4435 <= x_1])
                                    -> (k_insert_4127 x__4434 x__4435 x__4436 x__4437))))))))))))): X
abstract_term: (x_1050 < p12_4249): x_1:bool[x_1]
cond: p11_4248; true
pbs: p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:(x_1050 < p12_4249)
tt:false
ff:false

abstract_term: (l0
                 (k_insert_2134
                   (fun (x1_4073:int) (x2_4074:int)
                        (k_insert_4075:(x_1:bool ->
                                        x_2:int ->
                                        x_3:bool ->
                                        x_4:int[(not x_1) || (not x_3) || (not (x1_4073 <= x2_4074)) || x_2 <= x_4] ->
                                        X))
                    ->
                    (if (x1_4073 <= 0)
                      (l0
                        (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
                          (l1
                            (ysys_1015 0 (x2_4074 - 1)
                              (fun (xs11_4092:bool) (xs12_4093:int) (xs21_4094:bool)
                                   (xs22_4095:x_1:int[(not xs11_4092) || (
                                                      not xs21_4094) || (
                                                      not (0 <= (x2_4074 - 1))) ||
                                                      xs12_4093 <= x_1])
                               ->
                               (if (p11_4248 <=> xs11_4092)
                                 (l1
                                   (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                                     (l0
                                       (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092
                                         xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 ()
                                         (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
                                          (k_insert_4075 x__4450 x__4451 x__4452 x__4453))))))
                                 (l0
                                   (l0
                                     (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094
                                       xs22_4095 p12_4249 ()
                                       (fun (x__4446:bool) (x__4447:int) (x__4448:bool) (x__4449:int) ->
                                        (k_insert_4075 x__4446 x__4447 x__4448 x__4449)))))))))))
                      (l1
                        (if (x2_4074 <= 0)
                          (l0
                            (ysys_1015 (x1_4073 - 1) 0
                              (fun (p11_4102:bool) (p12_4103:int) (p21_4104:bool)
                                   (p22_4105:x_1:int[(not p11_4102) || (
                                                     not p21_4104) || (
                                                     not ((x1_4073 - 1) <= 0)) ||
                                                     p12_4103 <= x_1])
                               -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
                          (l1
                            (ysys_1015 (x1_4073 - 1) (x2_4074 - 1)
                              (fun (x__4442:bool) (x__4443:int) (x__4444:bool)
                                   (x__4445:x_1:int[(not x__4442) || (
                                                    not x__4444) || (
                                                    not ((x1_4073 - 1) <= (x2_4074 - 1))) ||
                                                    x__4443 <= x_1])
                               -> (k_insert_4075 x__4442 x__4443 x__4444 x__4445)))))))))): X
abstract_term: (k_insert_2134
                 (fun (x1_4073:int) (x2_4074:int)
                      (k_insert_4075:(x_1:bool ->
                                      x_2:int ->
                                      x_3:bool ->
                                      x_4:int[(not x_1) || (not x_3) || (not (x1_4073 <= x2_4074)) || x_2 <= x_4] -> X))
                  ->
                  (if (x1_4073 <= 0)
                    (l0
                      (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
                        (l1
                          (ysys_1015 0 (x2_4074 - 1)
                            (fun (xs11_4092:bool) (xs12_4093:int) (xs21_4094:bool)
                                 (xs22_4095:x_1:int[(not xs11_4092) || (
                                                    not xs21_4094) || (
                                                    not (0 <= (x2_4074 - 1))) ||
                                                    xs12_4093 <= x_1])
                             ->
                             (if (p11_4248 <=> xs11_4092)
                               (l1
                                 (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                                   (l0
                                     (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093
                                       p11_4248 xs21_4094 xs22_4095 p12_4249 ()
                                       (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
                                        (k_insert_4075 x__4450 x__4451 x__4452 x__4453))))))
                               (l0
                                 (l0
                                   (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094
                                     xs22_4095 p12_4249 ()
                                     (fun (x__4446:bool) (x__4447:int) (x__4448:bool) (x__4449:int) ->
                                      (k_insert_4075 x__4446 x__4447 x__4448 x__4449)))))))))))
                    (l1
                      (if (x2_4074 <= 0)
                        (l0
                          (ysys_1015 (x1_4073 - 1) 0
                            (fun (p11_4102:bool) (p12_4103:int) (p21_4104:bool)
                                 (p22_4105:x_1:int[(not p11_4102) || (
                                                   not p21_4104) || (
                                                   not ((x1_4073 - 1) <= 0)) ||
                                                   p12_4103 <= x_1])
                             -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
                        (l1
                          (ysys_1015 (x1_4073 - 1) (x2_4074 - 1)
                            (fun (x__4442:bool) (x__4443:int) (x__4444:bool)
                                 (x__4445:x_1:int[(not x__4442) || (not x__4444) ||
                                                  (not ((x1_4073 - 1) <= (x2_4074 - 1))) ||
                                                  x__4443 <= x_1])
                             -> (k_insert_4075 x__4442 x__4443 x__4444 x__4445))))))))): X
abstract_term: (fun (x1_4073:int) (x2_4074:int)
                    (k_insert_4075:(x_1:bool ->
                                    x_2:int ->
                                    x_3:bool ->
                                    x_4:int[(not x_1) || (not x_3) || (not (x1_4073 <= x2_4074)) || x_2 <= x_4] -> X))
                ->
                (if (x1_4073 <= 0)
                  (l0
                    (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
                      (l1
                        (ysys_1015 0 (x2_4074 - 1)
                          (fun (xs11_4092:bool) (xs12_4093:int) (xs21_4094:bool)
                               (xs22_4095:x_1:int[(not xs11_4092) || (
                                                  not xs21_4094) || (
                                                  not (0 <= (x2_4074 - 1))) ||
                                                  xs12_4093 <= x_1])
                           ->
                           (if (p11_4248 <=> xs11_4092)
                             (l1
                               (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                                 (l0
                                   (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093
                                     p11_4248 xs21_4094 xs22_4095 p12_4249 ()
                                     (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
                                      (k_insert_4075 x__4450 x__4451 x__4452 x__4453))))))
                             (l0
                               (l0
                                 (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094
                                   xs22_4095 p12_4249 ()
                                   (fun (x__4446:bool) (x__4447:int) (x__4448:bool) (x__4449:int) ->
                                    (k_insert_4075 x__4446 x__4447 x__4448 x__4449)))))))))))
                  (l1
                    (if (x2_4074 <= 0)
                      (l0
                        (ysys_1015 (x1_4073 - 1) 0
                          (fun (p11_4102:bool) (p12_4103:int) (p21_4104:bool)
                               (p22_4105:x_1:int[(not p11_4102) || (not p21_4104) || (
                                                 not ((x1_4073 - 1) <= 0)) ||
                                                 p12_4103 <= x_1])
                           -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
                      (l1
                        (ysys_1015 (x1_4073 - 1) (x2_4074 - 1)
                          (fun (x__4442:bool) (x__4443:int) (x__4444:bool)
                               (x__4445:x_1:int[(not x__4442) || (not x__4444) ||
                                                (not ((x1_4073 - 1) <= (x2_4074 - 1))) ||
                                                x__4443 <= x_1])
                           -> (k_insert_4075 x__4442 x__4443 x__4444 x__4445)))))))): (
x_1:int ->
x_2:int ->
(x_4:bool -> x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X) -> X)
abst_arg: x1_4073, int
abst_arg: x2_4074, int
abst_arg: k_insert_4075, (x_1:bool ->
                          x_2:int ->
                          x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x1_4073 <= x2_4074)) || x_2 <= x_4] -> X)
abst_arg: x1_4073, int
abst_arg: x2_4074, int
abst_arg: k_insert_4075, (x_1:bool ->
                          x_2:int ->
                          x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x1_4073 <= x2_4074)) || x_2 <= x_4] -> X)
abstract_term: (if (x1_4073 <= 0)
                 (l0
                   (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
                     (l1
                       (ysys_1015 0 (x2_4074 - 1)
                         (fun (xs11_4092:bool) (xs12_4093:int) (xs21_4094:bool)
                              (xs22_4095:x_1:int[(not xs11_4092) || (
                                                 not xs21_4094) || (not (0 <= (x2_4074 - 1))) ||
                                                 xs12_4093 <= x_1])
                          ->
                          (if (p11_4248 <=> xs11_4092)
                            (l1
                              (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                                (l0
                                  (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093
                                    p11_4248 xs21_4094 xs22_4095 p12_4249 ()
                                    (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
                                     (k_insert_4075 x__4450 x__4451 x__4452 x__4453))))))
                            (l0
                              (l0
                                (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095
                                  p12_4249 ()
                                  (fun (x__4446:bool) (x__4447:int) (x__4448:bool) (x__4449:int) ->
                                   (k_insert_4075 x__4446 x__4447 x__4448 x__4449)))))))))))
                 (l1
                   (if (x2_4074 <= 0)
                     (l0
                       (ysys_1015 (x1_4073 - 1) 0
                         (fun (p11_4102:bool) (p12_4103:int) (p21_4104:bool)
                              (p22_4105:x_1:int[(not p11_4102) || (not p21_4104) || (
                                                not ((x1_4073 - 1) <= 0)) ||
                                                p12_4103 <= x_1])
                          -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
                     (l1
                       (ysys_1015 (x1_4073 - 1) (x2_4074 - 1)
                         (fun (x__4442:bool) (x__4443:int) (x__4444:bool)
                              (x__4445:x_1:int[(not x__4442) || (not x__4444) ||
                                               (not ((x1_4073 - 1) <= (x2_4074 - 1))) ||
                                               x__4443 <= x_1])
                          -> (k_insert_4075 x__4442 x__4443 x__4444 x__4445))))))): X
abstract_term: (x1_4073 <= 0): x_1:bool[x_1]
cond: (x_1050 < p12_4249); p11_4248; true
pbs: p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:(x1_4073 <= 0)
tt:false
ff:false

abstract_term: (l0
                 (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
                   (l1
                     (ysys_1015 0 (x2_4074 - 1)
                       (fun (xs11_4092:bool) (xs12_4093:int) (xs21_4094:bool)
                            (xs22_4095:x_1:int[(not xs11_4092) || (not xs21_4094) || (
                                               not (0 <= (x2_4074 - 1))) ||
                                               xs12_4093 <= x_1])
                        ->
                        (if (p11_4248 <=> xs11_4092)
                          (l1
                            (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                              (l0
                                (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093
                                  p11_4248 xs21_4094 xs22_4095 p12_4249 ()
                                  (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
                                   (k_insert_4075 x__4450 x__4451 x__4452 x__4453))))))
                          (l0
                            (l0
                              (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095
                                p12_4249 ()
                                (fun (x__4446:bool) (x__4447:int) (x__4448:bool) (x__4449:int) ->
                                 (k_insert_4075 x__4446 x__4447 x__4448 x__4449))))))))))): X
abstract_term: (if (x2_4074 <= 0) (l0 (k_insert_4075 true x_1050 true x_1050))
                 (l1
                   (ysys_1015 0 (x2_4074 - 1)
                     (fun (xs11_4092:bool) (xs12_4093:int) (xs21_4094:bool)
                          (xs22_4095:x_1:int[(not xs11_4092) || (not xs21_4094) || (
                                             not (0 <= (x2_4074 - 1))) ||
                                             xs12_4093 <= x_1])
                      ->
                      (if (p11_4248 <=> xs11_4092)
                        (l1
                          (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                            (l0
                              (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093
                                p11_4248 xs21_4094 xs22_4095 p12_4249 ()
                                (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
                                 (k_insert_4075 x__4450 x__4451 x__4452 x__4453))))))
                        (l0
                          (l0
                            (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095
                              p12_4249 ()
                              (fun (x__4446:bool) (x__4447:int) (x__4448:bool) (x__4449:int) ->
                               (k_insert_4075 x__4446 x__4447 x__4448 x__4449)))))))))): X
abstract_term: (x2_4074 <= 0): x_1:bool[x_1]
cond: (x1_4073 <= 0); (x_1050 < p12_4249); p11_4248; true
pbs: p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:(x2_4074 <= 0)
tt:false
ff:false

abstract_term: (l0 (k_insert_4075 true x_1050 true x_1050)): X
abstract_term: (k_insert_4075 true x_1050 true x_1050): X
abstract_term: x_1050: x_1:int[(not true) || (not true) || (not (x1_4073 <= x2_4074)) || x_1050 <= x_1]
cond: (x2_4074 <= 0); (x1_4073 <= 0); (x_1050 < p12_4249); p11_4248; true
pbs: p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:((((not true) || (not true)) || (not (x1_4073 <= x2_4074))) || (x_1050 <= x_1050))
tt:true
ff:false

abstract_term: true: bool
abstract_term: x_1050: int
abstract_term: true: bool
abstract_term: (l1
                 (ysys_1015 0 (x2_4074 - 1)
                   (fun (xs11_4092:bool) (xs12_4093:int) (xs21_4094:bool)
                        (xs22_4095:x_1:int[(not xs11_4092) || (not xs21_4094) || (
                                           not (0 <= (x2_4074 - 1))) ||
                                           xs12_4093 <= x_1])
                    ->
                    (if (p11_4248 <=> xs11_4092)
                      (l1
                        (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                          (l0
                            (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248
                              xs21_4094 xs22_4095 p12_4249 ()
                              (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
                               (k_insert_4075 x__4450 x__4451 x__4452 x__4453))))))
                      (l0
                        (l0
                          (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095
                            p12_4249 ()
                            (fun (x__4446:bool) (x__4447:int) (x__4448:bool) (x__4449:int) ->
                             (k_insert_4075 x__4446 x__4447 x__4448 x__4449))))))))): X
abstract_term: (ysys_1015 0 (x2_4074 - 1)
                 (fun (xs11_4092:bool) (xs12_4093:int) (xs21_4094:bool)
                      (xs22_4095:x_1:int[(not xs11_4092) || (not xs21_4094) || (
                                         not (0 <= (x2_4074 - 1))) ||
                                         xs12_4093 <= x_1])
                  ->
                  (if (p11_4248 <=> xs11_4092)
                    (l1
                      (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                        (l0
                          (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248
                            xs21_4094 xs22_4095 p12_4249 ()
                            (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
                             (k_insert_4075 x__4450 x__4451 x__4452 x__4453))))))
                    (l0
                      (l0
                        (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095
                          p12_4249 ()
                          (fun (x__4446:bool) (x__4447:int) (x__4448:bool) (x__4449:int) ->
                           (k_insert_4075 x__4446 x__4447 x__4448 x__4449)))))))): X
abstract_term: (fun (xs11_4092:bool) (xs12_4093:int) (xs21_4094:bool)
                    (xs22_4095:x_1:int[(not xs11_4092) || (not xs21_4094) || (
                                       not (0 <= (x2_4074 - 1))) || xs12_4093 <= x_1])
                ->
                (if (p11_4248 <=> xs11_4092)
                  (l1
                    (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                      (l0
                        (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248
                          xs21_4094 xs22_4095 p12_4249 ()
                          (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
                           (k_insert_4075 x__4450 x__4451 x__4452 x__4453))))))
                  (l0
                    (l0
                      (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249
                        ()
                        (fun (x__4446:bool) (x__4447:int) (x__4448:bool) (x__4449:int) ->
                         (k_insert_4075 x__4446 x__4447 x__4448 x__4449))))))): (
x_1:bool -> x_2:int -> x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (0 <= (x2_4074 - 1))) || x_2 <= x_4] -> X)
abst_arg: xs11_4092, bool
abst_arg: xs12_4093, int
abst_arg: xs21_4094, bool
abst_arg: xs22_4095, x_1:int[(not xs11_4092) || (not xs21_4094) || (not (0 <= (x2_4074 - 1))) || xs12_4093 <= x_1]
abst_arg: xs11_4092, bool
abst_arg: xs12_4093, int
abst_arg: xs21_4094, bool
abst_arg: xs22_4095, x_1:int[(not xs11_4092) || (not xs21_4094) || (not (0 <= (x2_4074 - 1))) || xs12_4093 <= x_1]
abstract_term: (if (p11_4248 <=> xs11_4092)
                 (l1
                   (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                     (l0
                       (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248
                         xs21_4094 xs22_4095 p12_4249 ()
                         (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
                          (k_insert_4075 x__4450 x__4451 x__4452 x__4453))))))
                 (l0
                   (l0
                     (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249
                       ()
                       (fun (x__4446:bool) (x__4447:int) (x__4448:bool) (x__4449:int) ->
                        (k_insert_4075 x__4446 x__4447 x__4448 x__4449)))))): X
abstract_term: (p11_4248 <=> xs11_4092): x_1:bool[x_1]
cond: (not (x2_4074 <= 0)); (x1_4073 <= 0); (x_1050 < p12_4249); p11_4248; true
pbs: xs22_4095 := ((((not xs11_4092) || (not xs21_4094)) || (not (0 <= (x2_4074 - 1)))) || (xs12_4093 <= xs22_4095));
     p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:(p11_4248 <=> xs11_4092)
tt:false
ff:false

abstract_term: (l1
                 (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                   (l0
                     (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248
                       xs21_4094 xs22_4095 p12_4249 ()
                       (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
                        (k_insert_4075 x__4450 x__4451 x__4452 x__4453)))))): X
abstract_term: (if (xs12_4093 = p12_4249) (l1 (k_insert_4075 true x_1050 true xs22_4095))
                 (l0
                   (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248
                     xs21_4094 xs22_4095 p12_4249 ()
                     (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
                      (k_insert_4075 x__4450 x__4451 x__4452 x__4453))))): X
abstract_term: (xs12_4093 = p12_4249): x_1:bool[x_1]
cond: (p11_4248 <=> xs11_4092); (not (x2_4074 <= 0)); (x1_4073 <= 0); (x_1050 < p12_4249); p11_4248; true
pbs: xs22_4095 := ((((not xs11_4092) || (not xs21_4094)) || (not (0 <= (x2_4074 - 1)))) || (xs12_4093 <= xs22_4095));
     p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:(xs12_4093 = p12_4249)
tt:false
ff:false

abstract_term: (l1 (k_insert_4075 true x_1050 true xs22_4095)): X
abstract_term: (k_insert_4075 true x_1050 true xs22_4095): X
abstract_term: xs22_4095: x_1:int[(not true) || (not true) || (not (x1_4073 <= x2_4074)) || x_1050 <= x_1]
cond: (xs12_4093 = p12_4249); (p11_4248 <=> xs11_4092); (not (x2_4074 <= 0)); (
      x1_4073 <= 0); (x_1050 < p12_4249); p11_4248; true
pbs: xs22_4095 := ((((not xs11_4092) || (not xs21_4094)) || (not (0 <= (x2_4074 - 1)))) || (xs12_4093 <= xs22_4095));
     p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:((((not true) || (not true)) || (not (x1_4073 <= x2_4074))) || (x_1050 <= xs22_4095))
tt:false
ff:false

abstract_term: true: bool
abstract_term: x_1050: int
abstract_term: true: bool
abstract_term: (l0
                 (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094
                   xs22_4095 p12_4249 ()
                   (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
                    (k_insert_4075 x__4450 x__4451 x__4452 x__4453)))): X
abstract_term: (loop_3286 (not (p12_4249 = xs12_4093)) x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094
                 xs22_4095 p12_4249 ()
                 (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
                  (k_insert_4075 x__4450 x__4451 x__4452 x__4453))): X
abstract_term: (fun (x__4450:bool) (x__4451:int) (x__4452:bool) (x__4453:int) ->
                (k_insert_4075 x__4450 x__4451 x__4452 x__4453)): (bool -> int -> bool -> int -> X)
abst_arg: x__4450, bool
abst_arg: x__4451, int
abst_arg: x__4452, bool
abst_arg: x__4453, int
abst_arg: x__4450, bool
abst_arg: x__4451, int
abst_arg: x__4452, bool
abst_arg: x__4453, int
abstract_term: (k_insert_4075 x__4450 x__4451 x__4452 x__4453): X
abstract_term: x__4453: x_1:int[(not x__4450) || (not x__4452) || (not (x1_4073 <= x2_4074)) || x__4451 <= x_1]
cond: (not (xs12_4093 = p12_4249)); (p11_4248 <=> xs11_4092); (not (x2_4074 <= 0)); (
      x1_4073 <= 0); (x_1050 < p12_4249); p11_4248; true
pbs: xs22_4095 := ((((not xs11_4092) || (not xs21_4094)) || (not (0 <= (x2_4074 - 1)))) || (xs12_4093 <= xs22_4095));
     p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:((((not x__4450) || (not x__4452)) || (not (x1_4073 <= x2_4074))) || (x__4451 <= x__4453))
tt:false
ff:false

abstract_term: x__4452: bool
abstract_term: x__4451: int
abstract_term: x__4450: bool
abstract_term: (): unit
abstract_term: p12_4249: int
abstract_term: xs22_4095: int
abstract_term: xs21_4094: bool
abstract_term: p11_4248: bool
abstract_term: xs12_4093: int
abstract_term: xs11_4092: bool
abstract_term: x_1050: int
abstract_term: x2_4074: int
abstract_term: x1_4073: int
abstract_term: (not (p12_4249 = xs12_4093)): bool
abstract_term: (l0
                 (l0
                   (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 ()
                     (fun (x__4446:bool) (x__4447:int) (x__4448:bool) (x__4449:int) ->
                      (k_insert_4075 x__4446 x__4447 x__4448 x__4449))))): X
abstract_term: (l0
                 (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 ()
                   (fun (x__4446:bool) (x__4447:int) (x__4448:bool) (x__4449:int) ->
                    (k_insert_4075 x__4446 x__4447 x__4448 x__4449)))): X
abstract_term: (loop_3286 true x1_4073 x2_4074 x_1050 xs11_4092 xs12_4093 p11_4248 xs21_4094 xs22_4095 p12_4249 ()
                 (fun (x__4446:bool) (x__4447:int) (x__4448:bool) (x__4449:int) ->
                  (k_insert_4075 x__4446 x__4447 x__4448 x__4449))): X
abstract_term: (fun (x__4446:bool) (x__4447:int) (x__4448:bool) (x__4449:int) ->
                (k_insert_4075 x__4446 x__4447 x__4448 x__4449)): (bool -> int -> bool -> int -> X)
abst_arg: x__4446, bool
abst_arg: x__4447, int
abst_arg: x__4448, bool
abst_arg: x__4449, int
abst_arg: x__4446, bool
abst_arg: x__4447, int
abst_arg: x__4448, bool
abst_arg: x__4449, int
abstract_term: (k_insert_4075 x__4446 x__4447 x__4448 x__4449): X
abstract_term: x__4449: x_1:int[(not x__4446) || (not x__4448) || (not (x1_4073 <= x2_4074)) || x__4447 <= x_1]
cond: (not (p11_4248 <=> xs11_4092)); (not (x2_4074 <= 0)); (x1_4073 <= 0); (x_1050 < p12_4249); p11_4248; true
pbs: xs22_4095 := ((((not xs11_4092) || (not xs21_4094)) || (not (0 <= (x2_4074 - 1)))) || (xs12_4093 <= xs22_4095));
     p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:((((not x__4446) || (not x__4448)) || (not (x1_4073 <= x2_4074))) || (x__4447 <= x__4449))
tt:false
ff:false

abstract_term: x__4448: bool
abstract_term: x__4447: int
abstract_term: x__4446: bool
abstract_term: (): unit
abstract_term: p12_4249: int
abstract_term: xs22_4095: int
abstract_term: xs21_4094: bool
abstract_term: p11_4248: bool
abstract_term: xs12_4093: int
abstract_term: xs11_4092: bool
abstract_term: x_1050: int
abstract_term: x2_4074: int
abstract_term: x1_4073: int
abstract_term: true: bool
abstract_term: (x2_4074 - 1): int
abstract_term: 0: int
abstract_term: (l1
                 (if (x2_4074 <= 0)
                   (l0
                     (ysys_1015 (x1_4073 - 1) 0
                       (fun (p11_4102:bool) (p12_4103:int) (p21_4104:bool)
                            (p22_4105:x_1:int[(not p11_4102) || (not p21_4104) || (
                                              not ((x1_4073 - 1) <= 0)) ||
                                              p12_4103 <= x_1])
                        -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
                   (l1
                     (ysys_1015 (x1_4073 - 1) (x2_4074 - 1)
                       (fun (x__4442:bool) (x__4443:int) (x__4444:bool)
                            (x__4445:x_1:int[(not x__4442) || (not x__4444) ||
                                             (not ((x1_4073 - 1) <= (x2_4074 - 1))) ||
                                             x__4443 <= x_1])
                        -> (k_insert_4075 x__4442 x__4443 x__4444 x__4445)))))): X
abstract_term: (if (x2_4074 <= 0)
                 (l0
                   (ysys_1015 (x1_4073 - 1) 0
                     (fun (p11_4102:bool) (p12_4103:int) (p21_4104:bool)
                          (p22_4105:x_1:int[(not p11_4102) || (not p21_4104) || (
                                            not ((x1_4073 - 1) <= 0)) ||
                                            p12_4103 <= x_1])
                      -> (k_insert_4075 p11_4102 p12_4103 true x_1050))))
                 (l1
                   (ysys_1015 (x1_4073 - 1) (x2_4074 - 1)
                     (fun (x__4442:bool) (x__4443:int) (x__4444:bool)
                          (x__4445:x_1:int[(not x__4442) || (not x__4444) || (
                                           not ((x1_4073 - 1) <= (x2_4074 - 1))) ||
                                           x__4443 <= x_1])
                      -> (k_insert_4075 x__4442 x__4443 x__4444 x__4445))))): X
abstract_term: (x2_4074 <= 0): x_1:bool[x_1]
cond: (not (x1_4073 <= 0)); (x_1050 < p12_4249); p11_4248; true
pbs: p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:(x2_4074 <= 0)
tt:false
ff:false

abstract_term: (l0
                 (ysys_1015 (x1_4073 - 1) 0
                   (fun (p11_4102:bool) (p12_4103:int) (p21_4104:bool)
                        (p22_4105:x_1:int[(not p11_4102) || (not p21_4104) || (
                                          not ((x1_4073 - 1) <= 0)) ||
                                          p12_4103 <= x_1])
                    -> (k_insert_4075 p11_4102 p12_4103 true x_1050)))): X
abstract_term: (ysys_1015 (x1_4073 - 1) 0
                 (fun (p11_4102:bool) (p12_4103:int) (p21_4104:bool)
                      (p22_4105:x_1:int[(not p11_4102) || (not p21_4104) || (
                                        not ((x1_4073 - 1) <= 0)) ||
                                        p12_4103 <= x_1])
                  -> (k_insert_4075 p11_4102 p12_4103 true x_1050))): X
abstract_term: (fun (p11_4102:bool) (p12_4103:int) (p21_4104:bool)
                    (p22_4105:x_1:int[(not p11_4102) || (not p21_4104) || (not ((x1_4073 - 1) <= 0)) || p12_4103 <= x_1])
                -> (k_insert_4075 p11_4102 p12_4103 true x_1050)): (x_1:bool ->
                                                                    x_2:int ->
                                                                    x_3:bool ->
                                                                    x_4:int[
                                                                    (
                                                                    not x_1) || (
                                                                    not x_3) || (
                                                                    not ((x1_4073 - 1) <= 0)) ||
                                                                    x_2 <= x_4] -> X)
abst_arg: p11_4102, bool
abst_arg: p12_4103, int
abst_arg: p21_4104, bool
abst_arg: p22_4105, x_1:int[(not p11_4102) || (not p21_4104) || (not ((x1_4073 - 1) <= 0)) || p12_4103 <= x_1]
abst_arg: p11_4102, bool
abst_arg: p12_4103, int
abst_arg: p21_4104, bool
abst_arg: p22_4105, x_1:int[(not p11_4102) || (not p21_4104) || (not ((x1_4073 - 1) <= 0)) || p12_4103 <= x_1]
abstract_term: (k_insert_4075 p11_4102 p12_4103 true x_1050): X
abstract_term: x_1050: x_1:int[(not p11_4102) || (not true) || (not (x1_4073 <= x2_4074)) || p12_4103 <= x_1]
cond: (x2_4074 <= 0); (not (x1_4073 <= 0)); (x_1050 < p12_4249); p11_4248; true
pbs: p22_4105 := ((((not p11_4102) || (not p21_4104)) || (not ((x1_4073 - 1) <= 0))) || (p12_4103 <= p22_4105));
     p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:((((not p11_4102) || (not true)) || (not (x1_4073 <= x2_4074))) || (p12_4103 <= x_1050))
tt:true
ff:false

abstract_term: true: bool
abstract_term: p12_4103: int
abstract_term: p11_4102: bool
abstract_term: 0: int
abstract_term: (x1_4073 - 1): int
abstract_term: (l1
                 (ysys_1015 (x1_4073 - 1) (x2_4074 - 1)
                   (fun (x__4442:bool) (x__4443:int) (x__4444:bool)
                        (x__4445:x_1:int[(not x__4442) || (not x__4444) || (
                                         not ((x1_4073 - 1) <= (x2_4074 - 1))) ||
                                         x__4443 <= x_1])
                    -> (k_insert_4075 x__4442 x__4443 x__4444 x__4445)))): X
abstract_term: (ysys_1015 (x1_4073 - 1) (x2_4074 - 1)
                 (fun (x__4442:bool) (x__4443:int) (x__4444:bool)
                      (x__4445:x_1:int[(not x__4442) || (not x__4444) || (
                                       not ((x1_4073 - 1) <= (x2_4074 - 1))) ||
                                       x__4443 <= x_1])
                  -> (k_insert_4075 x__4442 x__4443 x__4444 x__4445))): X
abstract_term: (fun (x__4442:bool) (x__4443:int) (x__4444:bool)
                    (x__4445:x_1:int[(not x__4442) || (not x__4444) || (
                                     not ((x1_4073 - 1) <= (x2_4074 - 1))) ||
                                     x__4443 <= x_1])
                -> (k_insert_4075 x__4442 x__4443 x__4444 x__4445)): (
x_1:bool ->
x_2:int -> x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not ((x1_4073 - 1) <= (x2_4074 - 1))) || x_2 <= x_4] -> X)
abst_arg: x__4442, bool
abst_arg: x__4443, int
abst_arg: x__4444, bool
abst_arg: x__4445, x_1:int[(not x__4442) || (not x__4444) || (not ((x1_4073 - 1) <= (x2_4074 - 1))) || x__4443 <= x_1]
abst_arg: x__4442, bool
abst_arg: x__4443, int
abst_arg: x__4444, bool
abst_arg: x__4445, x_1:int[(not x__4442) || (not x__4444) || (not ((x1_4073 - 1) <= (x2_4074 - 1))) || x__4443 <= x_1]
abstract_term: (k_insert_4075 x__4442 x__4443 x__4444 x__4445): X
abstract_term: x__4445: x_1:int[(not x__4442) || (not x__4444) || (not (x1_4073 <= x2_4074)) || x__4443 <= x_1]
cond: (not (x2_4074 <= 0)); (not (x1_4073 <= 0)); (x_1050 < p12_4249); p11_4248; true
pbs: x__4445 := ((((not x__4442) || (not x__4444)) || (not ((x1_4073 - 1) <= (x2_4074 - 1)))) || (x__4443 <= x__4445));
     p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:((((not x__4442) || (not x__4444)) || (not (x1_4073 <= x2_4074))) || (x__4443 <= x__4445))
tt:x__4445
ff:false

abstract_term: x__4444: bool
abstract_term: x__4443: int
abstract_term: x__4442: bool
abstract_term: (x2_4074 - 1): int
abstract_term: (x1_4073 - 1): int
abstract_term: (l1
                 (insert_1013 x_1050
                   (fun (x1_4416:int) (x2_4417:int)
                        (k_insert_ys'ys'_4418:(x_1:bool ->
                                               x_2:int ->
                                               x_3:bool ->
                                               x_4:int[(not x_1) || (
                                                       not x_3) || (not (x1_4416 <= x2_4417)) ||
                                                       x_2 <= x_4]
                                               -> X))
                    ->
                    (ysys_1015 (x1_4416 + 1) (x2_4417 + 1)
                      (fun (x__4438:bool) (x__4439:int) (x__4440:bool)
                           (x__4441:x_1:int[(not x__4438) || (not x__4440) ||
                                            (not ((x1_4416 + 1) <= (x2_4417 + 1))) ||
                                            x__4439 <= x_1])
                       -> (k_insert_ys'ys'_4418 x__4438 x__4439 x__4440 x__4441))))
                   (fun (ys''ys''_4111:(x_1:int ->
                                        x_2:int ->
                                        (x_4:bool ->
                                         x_5:int ->
                                         x_6:bool ->
                                         x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                                        -> X))
                    ->
                    (k_insert_2134
                      (fun (x1_4125:int) (x2_4126:int)
                           (k_insert_4127:(x_1:bool ->
                                           x_2:int ->
                                           x_3:bool ->
                                           x_4:int[(not x_1) || (not x_3) || (not (x1_4125 <= x2_4126)) || x_2 <= x_4]
                                           -> X))
                       ->
                       (if (x1_4125 = 0)
                         (l0
                           (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                             (l1
                               (ysys_1015 0 (x2_4126 - 1)
                                 (fun (p11_4134:bool) (p12_4135:int) (p21_4136:bool)
                                      (p22_4137:x_1:int[(not p11_4134) || (
                                                        not p21_4136) || (
                                                        not (0 <= (x2_4126 - 1))) ||
                                                        p12_4135 <= x_1])
                                  -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
                         (l1
                           (if (x2_4126 = 0)
                             (l0
                               (ys''ys''_4111 x1_4125 0
                                 (fun (p11_4144:bool) (p12_4145:int) (p21_4146:bool)
                                      (p22_4147:x_1:int[(not p11_4144) || (
                                                        not p21_4146) || (
                                                        not (x1_4125 <= 0)) ||
                                                        p12_4145 <= x_1])
                                  -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                             (l1
                               (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1)
                                 (fun (x__4434:bool) (x__4435:int) (x__4436:bool)
                                      (x__4437:x_1:int[(not x__4434) || (
                                                       not x__4436) || (
                                                       not ((x1_4125 - 1) <= (x2_4126 - 1))) ||
                                                       x__4435 <= x_1])
                                  -> (k_insert_4127 x__4434 x__4435 x__4436 x__4437)))))))))))): X
abstract_term: (insert_1013 x_1050
                 (fun (x1_4416:int) (x2_4417:int)
                      (k_insert_ys'ys'_4418:(x_1:bool ->
                                             x_2:int ->
                                             x_3:bool ->
                                             x_4:int[(not x_1) || (not x_3) || (not (x1_4416 <= x2_4417)) || x_2 <= x_4]
                                             -> X))
                  ->
                  (ysys_1015 (x1_4416 + 1) (x2_4417 + 1)
                    (fun (x__4438:bool) (x__4439:int) (x__4440:bool)
                         (x__4441:x_1:int[(not x__4438) || (not x__4440) || (
                                          not ((x1_4416 + 1) <= (x2_4417 + 1))) ||
                                          x__4439 <= x_1])
                     -> (k_insert_ys'ys'_4418 x__4438 x__4439 x__4440 x__4441))))
                 (fun (ys''ys''_4111:(x_1:int ->
                                      x_2:int ->
                                      (x_4:bool ->
                                       x_5:int ->
                                       x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7]
                                       -> X)
                                      -> X))
                  ->
                  (k_insert_2134
                    (fun (x1_4125:int) (x2_4126:int)
                         (k_insert_4127:(x_1:bool ->
                                         x_2:int ->
                                         x_3:bool ->
                                         x_4:int[(not x_1) || (not x_3) || (not (x1_4125 <= x2_4126)) || x_2 <= x_4] ->
                                         X))
                     ->
                     (if (x1_4125 = 0)
                       (l0
                         (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                           (l1
                             (ysys_1015 0 (x2_4126 - 1)
                               (fun (p11_4134:bool) (p12_4135:int) (p21_4136:bool)
                                    (p22_4137:x_1:int[(not p11_4134) || (
                                                      not p21_4136) || (
                                                      not (0 <= (x2_4126 - 1))) ||
                                                      p12_4135 <= x_1])
                                -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
                       (l1
                         (if (x2_4126 = 0)
                           (l0
                             (ys''ys''_4111 x1_4125 0
                               (fun (p11_4144:bool) (p12_4145:int) (p21_4146:bool)
                                    (p22_4147:x_1:int[(not p11_4144) || (
                                                      not p21_4146) || (
                                                      not (x1_4125 <= 0)) ||
                                                      p12_4145 <= x_1])
                                -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                           (l1
                             (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1)
                               (fun (x__4434:bool) (x__4435:int) (x__4436:bool)
                                    (x__4437:x_1:int[(not x__4434) || (
                                                     not x__4436) || (
                                                     not ((x1_4125 - 1) <= (x2_4126 - 1))) ||
                                                     x__4435 <= x_1])
                                -> (k_insert_4127 x__4434 x__4435 x__4436 x__4437))))))))))): X
abstract_term: (fun (ys''ys''_4111:(x_1:int ->
                                    x_2:int ->
                                    (x_4:bool ->
                                     x_5:int ->
                                     x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] ->
                                     X)
                                    -> X))
                ->
                (k_insert_2134
                  (fun (x1_4125:int) (x2_4126:int)
                       (k_insert_4127:(x_1:bool ->
                                       x_2:int ->
                                       x_3:bool ->
                                       x_4:int[(not x_1) || (not x_3) || (not (x1_4125 <= x2_4126)) || x_2 <= x_4] -> X))
                   ->
                   (if (x1_4125 = 0)
                     (l0
                       (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                         (l1
                           (ysys_1015 0 (x2_4126 - 1)
                             (fun (p11_4134:bool) (p12_4135:int) (p21_4136:bool)
                                  (p22_4137:x_1:int[(not p11_4134) || (
                                                    not p21_4136) || (
                                                    not (0 <= (x2_4126 - 1))) ||
                                                    p12_4135 <= x_1])
                              -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
                     (l1
                       (if (x2_4126 = 0)
                         (l0
                           (ys''ys''_4111 x1_4125 0
                             (fun (p11_4144:bool) (p12_4145:int) (p21_4146:bool)
                                  (p22_4147:x_1:int[(not p11_4144) || (
                                                    not p21_4146) || (
                                                    not (x1_4125 <= 0)) ||
                                                    p12_4145 <= x_1])
                              -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                         (l1
                           (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1)
                             (fun (x__4434:bool) (x__4435:int) (x__4436:bool)
                                  (x__4437:x_1:int[(not x__4434) || (
                                                   not x__4436) || (not ((x1_4125 - 1) <= (x2_4126 - 1))) ||
                                                   x__4435 <= x_1])
                              -> (k_insert_4127 x__4434 x__4435 x__4436 x__4437)))))))))): ((
x_2:int ->
x_3:int ->
(x_5:bool -> x_6:int -> x_7:bool -> x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_6 <= x_8] -> X) -> X) ->
X)
abst_arg: ys''ys''_4111, (x_1:int ->
                          x_2:int ->
                          (x_4:bool ->
                           x_5:int ->
                           x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                          -> X)
abst_arg: ys''ys''_4111, (x_1:int ->
                          x_2:int ->
                          (x_4:bool ->
                           x_5:int ->
                           x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                          -> X)
abstract_term: (k_insert_2134
                 (fun (x1_4125:int) (x2_4126:int)
                      (k_insert_4127:(x_1:bool ->
                                      x_2:int ->
                                      x_3:bool ->
                                      x_4:int[(not x_1) || (not x_3) || (not (x1_4125 <= x2_4126)) || x_2 <= x_4] -> X))
                  ->
                  (if (x1_4125 = 0)
                    (l0
                      (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                        (l1
                          (ysys_1015 0 (x2_4126 - 1)
                            (fun (p11_4134:bool) (p12_4135:int) (p21_4136:bool)
                                 (p22_4137:x_1:int[(not p11_4134) || (
                                                   not p21_4136) || (
                                                   not (0 <= (x2_4126 - 1))) ||
                                                   p12_4135 <= x_1])
                             -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
                    (l1
                      (if (x2_4126 = 0)
                        (l0
                          (ys''ys''_4111 x1_4125 0
                            (fun (p11_4144:bool) (p12_4145:int) (p21_4146:bool)
                                 (p22_4147:x_1:int[(not p11_4144) || (
                                                   not p21_4146) || (
                                                   not (x1_4125 <= 0)) ||
                                                   p12_4145 <= x_1])
                             -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                        (l1
                          (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1)
                            (fun (x__4434:bool) (x__4435:int) (x__4436:bool)
                                 (x__4437:x_1:int[(not x__4434) || (not x__4436) ||
                                                  (not ((x1_4125 - 1) <= (x2_4126 - 1))) ||
                                                  x__4435 <= x_1])
                             -> (k_insert_4127 x__4434 x__4435 x__4436 x__4437))))))))): X
abstract_term: (fun (x1_4125:int) (x2_4126:int)
                    (k_insert_4127:(x_1:bool ->
                                    x_2:int ->
                                    x_3:bool ->
                                    x_4:int[(not x_1) || (not x_3) || (not (x1_4125 <= x2_4126)) || x_2 <= x_4] -> X))
                ->
                (if (x1_4125 = 0)
                  (l0
                    (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                      (l1
                        (ysys_1015 0 (x2_4126 - 1)
                          (fun (p11_4134:bool) (p12_4135:int) (p21_4136:bool)
                               (p22_4137:x_1:int[(not p11_4134) || (not p21_4136) || (
                                                 not (0 <= (x2_4126 - 1))) ||
                                                 p12_4135 <= x_1])
                           -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
                  (l1
                    (if (x2_4126 = 0)
                      (l0
                        (ys''ys''_4111 x1_4125 0
                          (fun (p11_4144:bool) (p12_4145:int) (p21_4146:bool)
                               (p22_4147:x_1:int[(not p11_4144) || (not p21_4146) || (
                                                 not (x1_4125 <= 0)) ||
                                                 p12_4145 <= x_1])
                           -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                      (l1
                        (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1)
                          (fun (x__4434:bool) (x__4435:int) (x__4436:bool)
                               (x__4437:x_1:int[(not x__4434) || (not x__4436) ||
                                                (not ((x1_4125 - 1) <= (x2_4126 - 1))) ||
                                                x__4435 <= x_1])
                           -> (k_insert_4127 x__4434 x__4435 x__4436 x__4437)))))))): (
x_1:int ->
x_2:int ->
(x_4:bool -> x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X) -> X)
abst_arg: x1_4125, int
abst_arg: x2_4126, int
abst_arg: k_insert_4127, (x_1:bool ->
                          x_2:int ->
                          x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x1_4125 <= x2_4126)) || x_2 <= x_4] -> X)
abst_arg: x1_4125, int
abst_arg: x2_4126, int
abst_arg: k_insert_4127, (x_1:bool ->
                          x_2:int ->
                          x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x1_4125 <= x2_4126)) || x_2 <= x_4] -> X)
abstract_term: (if (x1_4125 = 0)
                 (l0
                   (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                     (l1
                       (ysys_1015 0 (x2_4126 - 1)
                         (fun (p11_4134:bool) (p12_4135:int) (p21_4136:bool)
                              (p22_4137:x_1:int[(not p11_4134) || (not p21_4136) || (
                                                not (0 <= (x2_4126 - 1))) ||
                                                p12_4135 <= x_1])
                          -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))))
                 (l1
                   (if (x2_4126 = 0)
                     (l0
                       (ys''ys''_4111 x1_4125 0
                         (fun (p11_4144:bool) (p12_4145:int) (p21_4146:bool)
                              (p22_4147:x_1:int[(not p11_4144) || (not p21_4146) || (
                                                not (x1_4125 <= 0)) ||
                                                p12_4145 <= x_1])
                          -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                     (l1
                       (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1)
                         (fun (x__4434:bool) (x__4435:int) (x__4436:bool)
                              (x__4437:x_1:int[(not x__4434) || (not x__4436) ||
                                               (not ((x1_4125 - 1) <= (x2_4126 - 1))) ||
                                               x__4435 <= x_1])
                          -> (k_insert_4127 x__4434 x__4435 x__4436 x__4437))))))): X
abstract_term: (x1_4125 = 0): x_1:bool[x_1]
cond: (not (x_1050 < p12_4249)); p11_4248; true
pbs: p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:(x1_4125 = 0)
tt:false
ff:false

abstract_term: (l0
                 (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                   (l1
                     (ysys_1015 0 (x2_4126 - 1)
                       (fun (p11_4134:bool) (p12_4135:int) (p21_4136:bool)
                            (p22_4137:x_1:int[(not p11_4134) || (not p21_4136) || (
                                              not (0 <= (x2_4126 - 1))) ||
                                              p12_4135 <= x_1])
                        -> (k_insert_4127 true p12_4249 p21_4136 p22_4137)))))): X
abstract_term: (if (x2_4126 = 0) (l0 (k_insert_4127 true p12_4249 true p12_4249))
                 (l1
                   (ysys_1015 0 (x2_4126 - 1)
                     (fun (p11_4134:bool) (p12_4135:int) (p21_4136:bool)
                          (p22_4137:x_1:int[(not p11_4134) || (not p21_4136) || (
                                            not (0 <= (x2_4126 - 1))) ||
                                            p12_4135 <= x_1])
                      -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))))): X
abstract_term: (x2_4126 = 0): x_1:bool[x_1]
cond: (x1_4125 = 0); (not (x_1050 < p12_4249)); p11_4248; true
pbs: p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:(x2_4126 = 0)
tt:false
ff:false

abstract_term: (l0 (k_insert_4127 true p12_4249 true p12_4249)): X
abstract_term: (k_insert_4127 true p12_4249 true p12_4249): X
abstract_term: p12_4249: x_1:int[(not true) || (not true) || (not (x1_4125 <= x2_4126)) || p12_4249 <= x_1]
cond: (x2_4126 = 0); (x1_4125 = 0); (not (x_1050 < p12_4249)); p11_4248; true
pbs: p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:((((not true) || (not true)) || (not (x1_4125 <= x2_4126))) || (p12_4249 <= p12_4249))
tt:true
ff:false

abstract_term: true: bool
abstract_term: p12_4249: int
abstract_term: true: bool
abstract_term: (l1
                 (ysys_1015 0 (x2_4126 - 1)
                   (fun (p11_4134:bool) (p12_4135:int) (p21_4136:bool)
                        (p22_4137:x_1:int[(not p11_4134) || (not p21_4136) || (
                                          not (0 <= (x2_4126 - 1))) ||
                                          p12_4135 <= x_1])
                    -> (k_insert_4127 true p12_4249 p21_4136 p22_4137)))): X
abstract_term: (ysys_1015 0 (x2_4126 - 1)
                 (fun (p11_4134:bool) (p12_4135:int) (p21_4136:bool)
                      (p22_4137:x_1:int[(not p11_4134) || (not p21_4136) || (
                                        not (0 <= (x2_4126 - 1))) ||
                                        p12_4135 <= x_1])
                  -> (k_insert_4127 true p12_4249 p21_4136 p22_4137))): X
abstract_term: (fun (p11_4134:bool) (p12_4135:int) (p21_4136:bool)
                    (p22_4137:x_1:int[(not p11_4134) || (not p21_4136) || (not (0 <= (x2_4126 - 1))) || p12_4135 <= x_1])
                -> (k_insert_4127 true p12_4249 p21_4136 p22_4137)): (
x_1:bool -> x_2:int -> x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (0 <= (x2_4126 - 1))) || x_2 <= x_4] -> X)
abst_arg: p11_4134, bool
abst_arg: p12_4135, int
abst_arg: p21_4136, bool
abst_arg: p22_4137, x_1:int[(not p11_4134) || (not p21_4136) || (not (0 <= (x2_4126 - 1))) || p12_4135 <= x_1]
abst_arg: p11_4134, bool
abst_arg: p12_4135, int
abst_arg: p21_4136, bool
abst_arg: p22_4137, x_1:int[(not p11_4134) || (not p21_4136) || (not (0 <= (x2_4126 - 1))) || p12_4135 <= x_1]
abstract_term: (k_insert_4127 true p12_4249 p21_4136 p22_4137): X
abstract_term: p22_4137: x_1:int[(not true) || (not p21_4136) || (not (x1_4125 <= x2_4126)) || p12_4249 <= x_1]
cond: (not (x2_4126 = 0)); (x1_4125 = 0); (not (x_1050 < p12_4249)); p11_4248; true
pbs: p22_4137 := ((((not p11_4134) || (not p21_4136)) || (not (0 <= (x2_4126 - 1)))) || (p12_4135 <= p22_4137));
     p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:((((not true) || (not p21_4136)) || (not (x1_4125 <= x2_4126))) || (p12_4249 <= p22_4137))
tt:false
ff:false

abstract_term: p21_4136: bool
abstract_term: p12_4249: int
abstract_term: true: bool
abstract_term: (x2_4126 - 1): int
abstract_term: 0: int
abstract_term: (l1
                 (if (x2_4126 = 0)
                   (l0
                     (ys''ys''_4111 x1_4125 0
                       (fun (p11_4144:bool) (p12_4145:int) (p21_4146:bool)
                            (p22_4147:x_1:int[(not p11_4144) || (not p21_4146) || (
                                              not (x1_4125 <= 0)) ||
                                              p12_4145 <= x_1])
                        -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                   (l1
                     (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1)
                       (fun (x__4434:bool) (x__4435:int) (x__4436:bool)
                            (x__4437:x_1:int[(not x__4434) || (not x__4436) ||
                                             (not ((x1_4125 - 1) <= (x2_4126 - 1))) ||
                                             x__4435 <= x_1])
                        -> (k_insert_4127 x__4434 x__4435 x__4436 x__4437)))))): X
abstract_term: (if (x2_4126 = 0)
                 (l0
                   (ys''ys''_4111 x1_4125 0
                     (fun (p11_4144:bool) (p12_4145:int) (p21_4146:bool)
                          (p22_4147:x_1:int[(not p11_4144) || (not p21_4146) || (not (x1_4125 <= 0)) || p12_4145 <= x_1])
                      -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))))
                 (l1
                   (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1)
                     (fun (x__4434:bool) (x__4435:int) (x__4436:bool)
                          (x__4437:x_1:int[(not x__4434) || (not x__4436) || (
                                           not ((x1_4125 - 1) <= (x2_4126 - 1))) ||
                                           x__4435 <= x_1])
                      -> (k_insert_4127 x__4434 x__4435 x__4436 x__4437))))): X
abstract_term: (x2_4126 = 0): x_1:bool[x_1]
cond: (not (x1_4125 = 0)); (not (x_1050 < p12_4249)); p11_4248; true
pbs: p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:(x2_4126 = 0)
tt:false
ff:false

abstract_term: (l0
                 (ys''ys''_4111 x1_4125 0
                   (fun (p11_4144:bool) (p12_4145:int) (p21_4146:bool)
                        (p22_4147:x_1:int[(not p11_4144) || (not p21_4146) || (not (x1_4125 <= 0)) || p12_4145 <= x_1])
                    -> (k_insert_4127 p11_4144 p12_4145 true p12_4249)))): X
abstract_term: (ys''ys''_4111 x1_4125 0
                 (fun (p11_4144:bool) (p12_4145:int) (p21_4146:bool)
                      (p22_4147:x_1:int[(not p11_4144) || (not p21_4146) || (not (x1_4125 <= 0)) || p12_4145 <= x_1])
                  -> (k_insert_4127 p11_4144 p12_4145 true p12_4249))): X
abstract_term: (fun (p11_4144:bool) (p12_4145:int) (p21_4146:bool)
                    (p22_4147:x_1:int[(not p11_4144) || (not p21_4146) || (not (x1_4125 <= 0)) || p12_4145 <= x_1])
                -> (k_insert_4127 p11_4144 p12_4145 true p12_4249)): (
x_1:bool -> x_2:int -> x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x1_4125 <= 0)) || x_2 <= x_4] -> X)
abst_arg: p11_4144, bool
abst_arg: p12_4145, int
abst_arg: p21_4146, bool
abst_arg: p22_4147, x_1:int[(not p11_4144) || (not p21_4146) || (not (x1_4125 <= 0)) || p12_4145 <= x_1]
abst_arg: p11_4144, bool
abst_arg: p12_4145, int
abst_arg: p21_4146, bool
abst_arg: p22_4147, x_1:int[(not p11_4144) || (not p21_4146) || (not (x1_4125 <= 0)) || p12_4145 <= x_1]
abstract_term: (k_insert_4127 p11_4144 p12_4145 true p12_4249): X
abstract_term: p12_4249: x_1:int[(not p11_4144) || (not true) || (not (x1_4125 <= x2_4126)) || p12_4145 <= x_1]
cond: (x2_4126 = 0); (not (x1_4125 = 0)); (not (x_1050 < p12_4249)); p11_4248; true
pbs: p22_4147 := ((((not p11_4144) || (not p21_4146)) || (not (x1_4125 <= 0))) || (p12_4145 <= p22_4147));
     p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:((((not p11_4144) || (not true)) || (not (x1_4125 <= x2_4126))) || (p12_4145 <= p12_4249))
tt:false
ff:false

abstract_term: true: bool
abstract_term: p12_4145: int
abstract_term: p11_4144: bool
abstract_term: 0: int
abstract_term: x1_4125: int
abstract_term: (l1
                 (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1)
                   (fun (x__4434:bool) (x__4435:int) (x__4436:bool)
                        (x__4437:x_1:int[(not x__4434) || (not x__4436) || (
                                         not ((x1_4125 - 1) <= (x2_4126 - 1))) ||
                                         x__4435 <= x_1])
                    -> (k_insert_4127 x__4434 x__4435 x__4436 x__4437)))): X
abstract_term: (ys''ys''_4111 (x1_4125 - 1) (x2_4126 - 1)
                 (fun (x__4434:bool) (x__4435:int) (x__4436:bool)
                      (x__4437:x_1:int[(not x__4434) || (not x__4436) || (
                                       not ((x1_4125 - 1) <= (x2_4126 - 1))) ||
                                       x__4435 <= x_1])
                  -> (k_insert_4127 x__4434 x__4435 x__4436 x__4437))): X
abstract_term: (fun (x__4434:bool) (x__4435:int) (x__4436:bool)
                    (x__4437:x_1:int[(not x__4434) || (not x__4436) || (
                                     not ((x1_4125 - 1) <= (x2_4126 - 1))) ||
                                     x__4435 <= x_1])
                -> (k_insert_4127 x__4434 x__4435 x__4436 x__4437)): (
x_1:bool ->
x_2:int -> x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not ((x1_4125 - 1) <= (x2_4126 - 1))) || x_2 <= x_4] -> X)
abst_arg: x__4434, bool
abst_arg: x__4435, int
abst_arg: x__4436, bool
abst_arg: x__4437, x_1:int[(not x__4434) || (not x__4436) || (not ((x1_4125 - 1) <= (x2_4126 - 1))) || x__4435 <= x_1]
abst_arg: x__4434, bool
abst_arg: x__4435, int
abst_arg: x__4436, bool
abst_arg: x__4437, x_1:int[(not x__4434) || (not x__4436) || (not ((x1_4125 - 1) <= (x2_4126 - 1))) || x__4435 <= x_1]
abstract_term: (k_insert_4127 x__4434 x__4435 x__4436 x__4437): X
abstract_term: x__4437: x_1:int[(not x__4434) || (not x__4436) || (not (x1_4125 <= x2_4126)) || x__4435 <= x_1]
cond: (not (x2_4126 = 0)); (not (x1_4125 = 0)); (not (x_1050 < p12_4249)); p11_4248; true
pbs: x__4437 := ((((not x__4434) || (not x__4436)) || (not ((x1_4125 - 1) <= (x2_4126 - 1)))) || (x__4435 <= x__4437));
     p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:((((not x__4434) || (not x__4436)) || (not (x1_4125 <= x2_4126))) || (x__4435 <= x__4437))
tt:x__4437
ff:false

abstract_term: x__4436: bool
abstract_term: x__4435: int
abstract_term: x__4434: bool
abstract_term: (x2_4126 - 1): int
abstract_term: (x1_4125 - 1): int
abstract_term: (fun (x1_4416:int) (x2_4417:int)
                    (k_insert_ys'ys'_4418:(x_1:bool ->
                                           x_2:int ->
                                           x_3:bool ->
                                           x_4:int[(not x_1) || (not x_3) || (not (x1_4416 <= x2_4417)) || x_2 <= x_4]
                                           -> X))
                ->
                (ysys_1015 (x1_4416 + 1) (x2_4417 + 1)
                  (fun (x__4438:bool) (x__4439:int) (x__4440:bool)
                       (x__4441:x_1:int[(not x__4438) || (not x__4440) || (
                                        not ((x1_4416 + 1) <= (x2_4417 + 1))) ||
                                        x__4439 <= x_1])
                   -> (k_insert_ys'ys'_4418 x__4438 x__4439 x__4440 x__4441)))): (
x_1:int ->
x_2:int ->
(x_4:bool -> x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X) -> X)
abst_arg: x1_4416, int
abst_arg: x2_4417, int
abst_arg: k_insert_ys'ys'_4418, (x_1:bool ->
                                 x_2:int ->
                                 x_3:bool ->
                                 x_4:int[(not x_1) || (not x_3) || (not (x1_4416 <= x2_4417)) || x_2 <= x_4] -> X)
abst_arg: x1_4416, int
abst_arg: x2_4417, int
abst_arg: k_insert_ys'ys'_4418, (x_1:bool ->
                                 x_2:int ->
                                 x_3:bool ->
                                 x_4:int[(not x_1) || (not x_3) || (not (x1_4416 <= x2_4417)) || x_2 <= x_4] -> X)
abstract_term: (ysys_1015 (x1_4416 + 1) (x2_4417 + 1)
                 (fun (x__4438:bool) (x__4439:int) (x__4440:bool)
                      (x__4441:x_1:int[(not x__4438) || (not x__4440) || (
                                       not ((x1_4416 + 1) <= (x2_4417 + 1))) ||
                                       x__4439 <= x_1])
                  -> (k_insert_ys'ys'_4418 x__4438 x__4439 x__4440 x__4441))): X
abstract_term: (fun (x__4438:bool) (x__4439:int) (x__4440:bool)
                    (x__4441:x_1:int[(not x__4438) || (not x__4440) || (
                                     not ((x1_4416 + 1) <= (x2_4417 + 1))) ||
                                     x__4439 <= x_1])
                -> (k_insert_ys'ys'_4418 x__4438 x__4439 x__4440 x__4441)): (
x_1:bool ->
x_2:int -> x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not ((x1_4416 + 1) <= (x2_4417 + 1))) || x_2 <= x_4] -> X)
abst_arg: x__4438, bool
abst_arg: x__4439, int
abst_arg: x__4440, bool
abst_arg: x__4441, x_1:int[(not x__4438) || (not x__4440) || (not ((x1_4416 + 1) <= (x2_4417 + 1))) || x__4439 <= x_1]
abst_arg: x__4438, bool
abst_arg: x__4439, int
abst_arg: x__4440, bool
abst_arg: x__4441, x_1:int[(not x__4438) || (not x__4440) || (not ((x1_4416 + 1) <= (x2_4417 + 1))) || x__4439 <= x_1]
abstract_term: (k_insert_ys'ys'_4418 x__4438 x__4439 x__4440 x__4441): X
abstract_term: x__4441: x_1:int[(not x__4438) || (not x__4440) || (not (x1_4416 <= x2_4417)) || x__4439 <= x_1]
cond: (not (x_1050 < p12_4249)); p11_4248; true
pbs: x__4441 := ((((not x__4438) || (not x__4440)) || (not ((x1_4416 + 1) <= (x2_4417 + 1)))) || (x__4439 <= x__4441));
     p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:((((not x__4438) || (not x__4440)) || (not (x1_4416 <= x2_4417))) || (x__4439 <= x__4441))
tt:x__4441
ff:false

abstract_term: x__4440: bool
abstract_term: x__4439: int
abstract_term: x__4438: bool
abstract_term: (x2_4417 + 1): int
abstract_term: (x1_4416 + 1): int
abstract_term: x_1050: int
abstract_term: (l0
                 (k_insert_2134
                   (fun (x1_4343:int) (x2_4344:int)
                        (k_insert_rsrs_4345:(x_1:bool ->
                                             x_2:int ->
                                             x_3:bool ->
                                             x_4:int[(not x_1) || (not x_3) || (not (x1_4343 <= x2_4344)) || x_2 <= x_4]
                                             -> X))
                    ->
                    (if (x2_4344 = x1_4343)
                      (l0
                        (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                          (l1 (k_insert_rsrs_4345 false 0 false 0))))
                      (l1
                        (if (x1_4343 = 0)
                          (l0
                            (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                              (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
                          (l1
                            (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050))
                              (l1 (k_insert_rsrs_4345 false 0 false 0)))))))))): X
abstract_term: (k_insert_2134
                 (fun (x1_4343:int) (x2_4344:int)
                      (k_insert_rsrs_4345:(x_1:bool ->
                                           x_2:int ->
                                           x_3:bool ->
                                           x_4:int[(not x_1) || (not x_3) || (not (x1_4343 <= x2_4344)) || x_2 <= x_4]
                                           -> X))
                  ->
                  (if (x2_4344 = x1_4343)
                    (l0
                      (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                        (l1 (k_insert_rsrs_4345 false 0 false 0))))
                    (l1
                      (if (x1_4343 = 0)
                        (l0
                          (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                            (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
                        (l1
                          (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050))
                            (l1 (k_insert_rsrs_4345 false 0 false 0))))))))): X
abstract_term: (fun (x1_4343:int) (x2_4344:int)
                    (k_insert_rsrs_4345:(x_1:bool ->
                                         x_2:int ->
                                         x_3:bool ->
                                         x_4:int[(not x_1) || (not x_3) || (not (x1_4343 <= x2_4344)) || x_2 <= x_4] ->
                                         X))
                ->
                (if (x2_4344 = x1_4343)
                  (l0
                    (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                      (l1 (k_insert_rsrs_4345 false 0 false 0))))
                  (l1
                    (if (x1_4343 = 0)
                      (l0
                        (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                          (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
                      (l1
                        (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050))
                          (l1 (k_insert_rsrs_4345 false 0 false 0)))))))): (
x_1:int ->
x_2:int ->
(x_4:bool -> x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X) -> X)
abst_arg: x1_4343, int
abst_arg: x2_4344, int
abst_arg: k_insert_rsrs_4345, (x_1:bool ->
                               x_2:int ->
                               x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x1_4343 <= x2_4344)) || x_2 <= x_4]
                               -> X)
abst_arg: x1_4343, int
abst_arg: x2_4344, int
abst_arg: k_insert_rsrs_4345, (x_1:bool ->
                               x_2:int ->
                               x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x1_4343 <= x2_4344)) || x_2 <= x_4]
                               -> X)
abstract_term: (if (x2_4344 = x1_4343)
                 (l0
                   (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                     (l1 (k_insert_rsrs_4345 false 0 false 0))))
                 (l1
                   (if (x1_4343 = 0)
                     (l0
                       (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                         (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
                     (l1
                       (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050))
                         (l1 (k_insert_rsrs_4345 false 0 false 0))))))): X
abstract_term: (x2_4344 = x1_4343): x_1:bool[x_1]
cond: (not p11_4248); true
pbs: p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:(x2_4344 = x1_4343)
tt:false
ff:false

abstract_term: (l0
                 (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                   (l1 (k_insert_rsrs_4345 false 0 false 0)))): X
abstract_term: (if (x1_4343 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                 (l1 (k_insert_rsrs_4345 false 0 false 0))): X
abstract_term: (x1_4343 = 0): x_1:bool[x_1]
cond: (x2_4344 = x1_4343); (not p11_4248); true
pbs: p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:(x1_4343 = 0)
tt:false
ff:false

abstract_term: (l0 (k_insert_rsrs_4345 true x_1050 true x_1050)): X
abstract_term: (k_insert_rsrs_4345 true x_1050 true x_1050): X
abstract_term: x_1050: x_1:int[(not true) || (not true) || (not (x1_4343 <= x2_4344)) || x_1050 <= x_1]
cond: (x1_4343 = 0); (x2_4344 = x1_4343); (not p11_4248); true
pbs: p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:((((not true) || (not true)) || (not (x1_4343 <= x2_4344))) || (x_1050 <= x_1050))
tt:true
ff:false

abstract_term: true: bool
abstract_term: x_1050: int
abstract_term: true: bool
abstract_term: (l1 (k_insert_rsrs_4345 false 0 false 0)): X
abstract_term: (k_insert_rsrs_4345 false 0 false 0): X
abstract_term: 0: x_1:int[(not false) || (not false) || (not (x1_4343 <= x2_4344)) || 0 <= x_1]
cond: (not (x1_4343 = 0)); (x2_4344 = x1_4343); (not p11_4248); true
pbs: p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:((((not false) || (not false)) || (not (x1_4343 <= x2_4344))) || (0 <= 0))
tt:true
ff:false

abstract_term: false: bool
abstract_term: 0: int
abstract_term: false: bool
abstract_term: (l1
                 (if (x1_4343 = 0)
                   (l0
                     (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                       (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
                   (l1
                     (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050))
                       (l1 (k_insert_rsrs_4345 false 0 false 0)))))): X
abstract_term: (if (x1_4343 = 0)
                 (l0
                   (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                     (l1 (k_insert_rsrs_4345 true x_1050 false 0))))
                 (l1
                   (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050))
                     (l1 (k_insert_rsrs_4345 false 0 false 0))))): X
abstract_term: (x1_4343 = 0): x_1:bool[x_1]
cond: (not (x2_4344 = x1_4343)); (not p11_4248); true
pbs: p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:(x1_4343 = 0)
tt:false
ff:false

abstract_term: (l0
                 (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                   (l1 (k_insert_rsrs_4345 true x_1050 false 0)))): X
abstract_term: (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 true x_1050 true x_1050))
                 (l1 (k_insert_rsrs_4345 true x_1050 false 0))): X
abstract_term: (x2_4344 = 0): x_1:bool[x_1]
cond: (x1_4343 = 0); (not (x2_4344 = x1_4343)); (not p11_4248); true
pbs: p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:(x2_4344 = 0)
tt:false
ff:true

abstract_term: (l0 (k_insert_rsrs_4345 true x_1050 true x_1050)): X
abstract_term: (k_insert_rsrs_4345 true x_1050 true x_1050): X
abstract_term: x_1050: x_1:int[(not true) || (not true) || (not (x1_4343 <= x2_4344)) || x_1050 <= x_1]
cond: (x2_4344 = 0); (x1_4343 = 0); (not (x2_4344 = x1_4343)); (not p11_4248); true
pbs: p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:((((not true) || (not true)) || (not (x1_4343 <= x2_4344))) || (x_1050 <= x_1050))
tt:true
ff:false

abstract_term: true: bool
abstract_term: x_1050: int
abstract_term: true: bool
abstract_term: (l1 (k_insert_rsrs_4345 true x_1050 false 0)): X
abstract_term: (k_insert_rsrs_4345 true x_1050 false 0): X
abstract_term: 0: x_1:int[(not true) || (not false) || (not (x1_4343 <= x2_4344)) || x_1050 <= x_1]
cond: (not (x2_4344 = 0)); (x1_4343 = 0); (not (x2_4344 = x1_4343)); (not p11_4248); true
pbs: p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:((((not true) || (not false)) || (not (x1_4343 <= x2_4344))) || (x_1050 <= 0))
tt:true
ff:false

abstract_term: false: bool
abstract_term: x_1050: int
abstract_term: true: bool
abstract_term: (l1
                 (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050))
                   (l1 (k_insert_rsrs_4345 false 0 false 0)))): X
abstract_term: (if (x2_4344 = 0) (l0 (k_insert_rsrs_4345 false 0 true x_1050))
                 (l1 (k_insert_rsrs_4345 false 0 false 0))): X
abstract_term: (x2_4344 = 0): x_1:bool[x_1]
cond: (not (x1_4343 = 0)); (not (x2_4344 = x1_4343)); (not p11_4248); true
pbs: p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:(x2_4344 = 0)
tt:false
ff:false

abstract_term: (l0 (k_insert_rsrs_4345 false 0 true x_1050)): X
abstract_term: (k_insert_rsrs_4345 false 0 true x_1050): X
abstract_term: x_1050: x_1:int[(not false) || (not true) || (not (x1_4343 <= x2_4344)) || 0 <= x_1]
cond: (x2_4344 = 0); (not (x1_4343 = 0)); (not (x2_4344 = x1_4343)); (not p11_4248); true
pbs: p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:((((not false) || (not true)) || (not (x1_4343 <= x2_4344))) || (0 <= x_1050))
tt:true
ff:false

abstract_term: true: bool
abstract_term: 0: int
abstract_term: false: bool
abstract_term: (l1 (k_insert_rsrs_4345 false 0 false 0)): X
abstract_term: (k_insert_rsrs_4345 false 0 false 0): X
abstract_term: 0: x_1:int[(not false) || (not false) || (not (x1_4343 <= x2_4344)) || 0 <= x_1]
cond: (not (x2_4344 = 0)); (not (x1_4343 = 0)); (not (x2_4344 = x1_4343)); (not p11_4248); true
pbs: p22_4251 := ((((not p11_4248) || (not p21_4250)) || (not (0 <= 0))) || (p12_4249 <= p22_4251))
p:((((not false) || (not false)) || (not (x1_4343 <= x2_4344))) || (0 <= 0))
tt:true
ff:false

abstract_term: false: bool
abstract_term: 0: int
abstract_term: false: bool
abstract_term: 0: int
abstract_term: 0: int
insertsort_2064: ENV: xsxs_1037:(int -> int -> (bool -> int -> bool -> int -> X) -> X),
k_insertsort_2604:((x_2:int ->
                    x_3:int ->
                    (x_5:bool ->
                     x_6:int -> x_7:bool -> x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_6 <= x_8] -> X)
                    -> X) -> X),


abst_arg: xsxs_1037, (int -> int -> (bool -> int -> bool -> int -> X) -> X)
abst_arg: k_insertsort_2604, ((x_2:int ->
                               x_3:int ->
                               (x_5:bool ->
                                x_6:int ->
                                x_7:bool -> x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_6 <= x_8] -> X)
                               -> X) ->
X)
abst_arg: xsxs_1037, (int -> int -> (bool -> int -> bool -> int -> X) -> X)
abst_arg: k_insertsort_2604, ((x_2:int ->
                               x_3:int ->
                               (x_5:bool ->
                                x_6:int ->
                                x_7:bool -> x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_6 <= x_8] -> X)
                               -> X) ->
X)
insertsort_2064: (xsxs_1037 0 0
                   (fun (p11_4150:bool) (p12_4151:int) (p21_4152:bool) (p22_4153:int) ->
                    (if p11_4150
                      (l1
                        (xsxs_1037 0 0
                          (fun (p11_4258:bool) (p12_4259:int) (p21_4260:bool) (p22_4261:int) ->
                           (insertsort_2064
                             (fun (x1_4370:int) (x2_4371:int)
                                  (k_insertsort_xs'xs'_4372:(bool -> int -> bool -> int -> X))
                              ->
                              (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1)
                                (fun (x__4469:bool) (x__4470:int) (x__4471:bool) (x__4472:int) ->
                                 (k_insertsort_xs'xs'_4372 x__4469 x__4470 x__4471 x__4472))))
                             (fun (x_4185:(x_1:int ->
                                           x_2:int ->
                                           (x_4:bool ->
                                            x_5:int ->
                                            x_6:bool ->
                                            x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                                           -> X))
                              ->
                              (insert_1013 p12_4259
                                (fun (x__4462:int) (x__4463:int)
                                     (x__4464:(x_1:bool ->
                                               x_2:int ->
                                               x_3:bool ->
                                               x_4:int[(not x_1) || (
                                                       not x_3) || (not (x__4462 <= x__4463)) ||
                                                       x_2 <= x_4]
                                               -> X))
                                 ->
                                 (x_4185 x__4462 x__4463
                                   (fun (x__4465:bool) (x__4466:int) (x__4467:bool)
                                        (x__4468:x_1:int[(not x__4465) || (
                                                         not x__4467) || (
                                                         not (x__4462 <= x__4463)) ||
                                                         x__4466 <= x_1])
                                    -> (x__4464 x__4465 x__4466 x__4467 x__4468))))
                                (fun (x__4454:(x_1:int ->
                                               x_2:int ->
                                               (x_4:bool ->
                                                x_5:int ->
                                                x_6:bool ->
                                                x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] ->
                                                X)
                                               -> X))
                                 ->
                                 (k_insertsort_2604
                                   (fun (x__4455:int) (x__4456:int)
                                        (x__4457:(x_1:bool ->
                                                  x_2:int ->
                                                  x_3:bool ->
                                                  x_4:int[(not x_1) || (
                                                          not x_3) || (
                                                          not (x__4455 <= x__4456)) ||
                                                          x_2 <= x_4]
                                                  -> X))
                                    ->
                                    (x__4454 x__4455 x__4456
                                      (fun (x__4458:bool) (x__4459:int) (x__4460:bool)
                                           (x__4461:x_1:int[(not x__4458) || (
                                                            not x__4460) || (
                                                            not (x__4455 <= x__4456)) ||
                                                            x__4459 <= x_1])
                                       -> (x__4457 x__4458 x__4459 x__4460 x__4461))))))))))))
                      (l0
                        (k_insertsort_2604
                          (fun (i1_4351:int) (i2_4352:int)
                               (k_insertsort_rsrs_4353:(x_1:bool ->
                                                        x_2:int ->
                                                        x_3:bool ->
                                                        x_4:int[(not x_1) || (
                                                                not x_3) || (
                                                                not (i1_4351 <= i2_4352)) ||
                                                                x_2 <= x_4]
                                                        -> X))
                           -> (k_insertsort_rsrs_4353 false 0 false 0))))))) ===> (
xsxs_1037 0 0
 (fun (p11_4150:bool) (p12_4151:int) (p21_4152:bool) (p22_4153:int) ->
  (if p11_4150
    (l1
      (xsxs_1037 0 0
        (fun (p11_4258:bool) (p12_4259:int) (p21_4260:bool) (p22_4261:int) ->
         (insertsort_2064
           (fun (x1_4370:int) (x2_4371:int) (k_insertsort_xs'xs'_4372:(bool -> int -> bool -> int -> X)) ->
            (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1)
              (fun (x__4469:bool) (x__4470:int) (x__4471:bool) (x__4472:int) ->
               (k_insertsort_xs'xs'_4372 x__4469 x__4470 x__4471 x__4472))))
           (fun (x_4185:(x_1:int ->
                         x_2:int ->
                         (x_4:bool ->
                          x_5:int ->
                          x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                         -> X))
            ->
            (insert_1013 p12_4259
              (fun (x__4462:int) (x__4463:int)
                   (x__4464:(x_1:bool ->
                             x_2:int ->
                             x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4462 <= x__4463)) || x_2 <= x_4] ->
                             X))
               ->
               (x_4185 x__4462 x__4463
                 (fun (x__4465:bool) (x__4466:int) (x__4467:bool)
                      (x__4468:x_1:int[(not x__4465) || (not x__4467) || (not (x__4462 <= x__4463)) || x__4466 <= x_1])
                  -> (x__4464 x__4465 x__4466 x__4467 x__4468))))
              (fun (x__4454:(x_1:int ->
                             x_2:int ->
                             (x_4:bool ->
                              x_5:int ->
                              x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                             -> X))
               ->
               (k_insertsort_2604
                 (fun (x__4455:int) (x__4456:int)
                      (x__4457:(x_1:bool ->
                                x_2:int ->
                                x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4455 <= x__4456)) || x_2 <= x_4]
                                -> X))
                  ->
                  (x__4454 x__4455 x__4456
                    (fun (x__4458:bool) (x__4459:int) (x__4460:bool)
                         (x__4461:x_1:int[(not x__4458) || (not x__4460) || (
                                          not (x__4455 <= x__4456)) ||
                                          x__4459 <= x_1])
                     -> (x__4457 x__4458 x__4459 x__4460 x__4461))))))))))))
    (l0
      (k_insertsort_2604
        (fun (i1_4351:int) (i2_4352:int)
             (k_insertsort_rsrs_4353:(x_1:bool ->
                                      x_2:int ->
                                      x_3:bool ->
                                      x_4:int[(not x_1) || (not x_3) || (not (i1_4351 <= i2_4352)) || x_2 <= x_4] -> X))
         -> (k_insertsort_rsrs_4353 false 0 false 0)))))))
insertsort_2064:: (xsxs_1037 0 0
                    (fun (p11_4150:bool) (p12_4151:int) (p21_4152:bool) (p22_4153:int) ->
                     (if p11_4150
                       (l1
                         (xsxs_1037 0 0
                           (fun (p11_4258:bool) (p12_4259:int) (p21_4260:bool) (p22_4261:int) ->
                            (insertsort_2064
                              (fun (x1_4370:int) (x2_4371:int)
                                   (k_insertsort_xs'xs'_4372:(bool -> int -> bool -> int -> X))
                               ->
                               (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1)
                                 (fun (x__4469:bool) (x__4470:int) (x__4471:bool) (x__4472:int) ->
                                  (k_insertsort_xs'xs'_4372 x__4469 x__4470 x__4471 x__4472))))
                              (fun (x_4185:(x_1:int ->
                                            x_2:int ->
                                            (x_4:bool ->
                                             x_5:int ->
                                             x_6:bool ->
                                             x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                                            -> X))
                               ->
                               (insert_1013 p12_4259
                                 (fun (x__4462:int) (x__4463:int)
                                      (x__4464:(x_1:bool ->
                                                x_2:int ->
                                                x_3:bool ->
                                                x_4:int[(not x_1) || (
                                                        not x_3) || (
                                                        not (x__4462 <= x__4463)) ||
                                                        x_2 <= x_4]
                                                -> X))
                                  ->
                                  (x_4185 x__4462 x__4463
                                    (fun (x__4465:bool) (x__4466:int) (x__4467:bool)
                                         (x__4468:x_1:int[(not x__4465) || (
                                                          not x__4467) || (
                                                          not (x__4462 <= x__4463)) ||
                                                          x__4466 <= x_1])
                                     -> (x__4464 x__4465 x__4466 x__4467 x__4468))))
                                 (fun (x__4454:(x_1:int ->
                                                x_2:int ->
                                                (x_4:bool ->
                                                 x_5:int ->
                                                 x_6:bool ->
                                                 x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] ->
                                                 X)
                                                -> X))
                                  ->
                                  (k_insertsort_2604
                                    (fun (x__4455:int) (x__4456:int)
                                         (x__4457:(x_1:bool ->
                                                   x_2:int ->
                                                   x_3:bool ->
                                                   x_4:int[(not x_1) || (
                                                           not x_3) || (
                                                           not (x__4455 <= x__4456)) ||
                                                           x_2 <= x_4]
                                                   -> X))
                                     ->
                                     (x__4454 x__4455 x__4456
                                       (fun (x__4458:bool) (x__4459:int) (x__4460:bool)
                                            (x__4461:x_1:int[(not x__4458) || (
                                                             not x__4460) || (
                                                             not (x__4455 <= x__4456)) ||
                                                             x__4459 <= x_1])
                                        -> (x__4457 x__4458 x__4459 x__4460 x__4461))))))))))))
                       (l0
                         (k_insertsort_2604
                           (fun (i1_4351:int) (i2_4352:int)
                                (k_insertsort_rsrs_4353:(x_1:bool ->
                                                         x_2:int ->
                                                         x_3:bool ->
                                                         x_4:int[(not x_1) || (
                                                                 not x_3) || (
                                                                 not (i1_4351 <= i2_4352)) ||
                                                                 x_2 <= x_4]
                                                         -> X))
                            -> (k_insertsort_rsrs_4353 false 0 false 0)))))))
abstract_term: (xsxs_1037 0 0
                 (fun (p11_4150:bool) (p12_4151:int) (p21_4152:bool) (p22_4153:int) ->
                  (if p11_4150
                    (l1
                      (xsxs_1037 0 0
                        (fun (p11_4258:bool) (p12_4259:int) (p21_4260:bool) (p22_4261:int) ->
                         (insertsort_2064
                           (fun (x1_4370:int) (x2_4371:int)
                                (k_insertsort_xs'xs'_4372:(bool -> int -> bool -> int -> X))
                            ->
                            (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1)
                              (fun (x__4469:bool) (x__4470:int) (x__4471:bool) (x__4472:int) ->
                               (k_insertsort_xs'xs'_4372 x__4469 x__4470 x__4471 x__4472))))
                           (fun (x_4185:(x_1:int ->
                                         x_2:int ->
                                         (x_4:bool ->
                                          x_5:int ->
                                          x_6:bool ->
                                          x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                                         -> X))
                            ->
                            (insert_1013 p12_4259
                              (fun (x__4462:int) (x__4463:int)
                                   (x__4464:(x_1:bool ->
                                             x_2:int ->
                                             x_3:bool ->
                                             x_4:int[(not x_1) || (not x_3) || (not (x__4462 <= x__4463)) || x_2 <= x_4]
                                             -> X))
                               ->
                               (x_4185 x__4462 x__4463
                                 (fun (x__4465:bool) (x__4466:int) (x__4467:bool)
                                      (x__4468:x_1:int[(not x__4465) || (
                                                       not x__4467) || (
                                                       not (x__4462 <= x__4463)) ||
                                                       x__4466 <= x_1])
                                  -> (x__4464 x__4465 x__4466 x__4467 x__4468))))
                              (fun (x__4454:(x_1:int ->
                                             x_2:int ->
                                             (x_4:bool ->
                                              x_5:int ->
                                              x_6:bool ->
                                              x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                                             -> X))
                               ->
                               (k_insertsort_2604
                                 (fun (x__4455:int) (x__4456:int)
                                      (x__4457:(x_1:bool ->
                                                x_2:int ->
                                                x_3:bool ->
                                                x_4:int[(not x_1) || (
                                                        not x_3) || (
                                                        not (x__4455 <= x__4456)) ||
                                                        x_2 <= x_4]
                                                -> X))
                                  ->
                                  (x__4454 x__4455 x__4456
                                    (fun (x__4458:bool) (x__4459:int) (x__4460:bool)
                                         (x__4461:x_1:int[(not x__4458) || (
                                                          not x__4460) || (
                                                          not (x__4455 <= x__4456)) ||
                                                          x__4459 <= x_1])
                                     -> (x__4457 x__4458 x__4459 x__4460 x__4461))))))))))))
                    (l0
                      (k_insertsort_2604
                        (fun (i1_4351:int) (i2_4352:int)
                             (k_insertsort_rsrs_4353:(x_1:bool ->
                                                      x_2:int ->
                                                      x_3:bool ->
                                                      x_4:int[(not x_1) || (
                                                              not x_3) || (
                                                              not (i1_4351 <= i2_4352)) ||
                                                              x_2 <= x_4]
                                                      -> X))
                         -> (k_insertsort_rsrs_4353 false 0 false 0))))))): X
abstract_term: (fun (p11_4150:bool) (p12_4151:int) (p21_4152:bool) (p22_4153:int) ->
                (if p11_4150
                  (l1
                    (xsxs_1037 0 0
                      (fun (p11_4258:bool) (p12_4259:int) (p21_4260:bool) (p22_4261:int) ->
                       (insertsort_2064
                         (fun (x1_4370:int) (x2_4371:int) (k_insertsort_xs'xs'_4372:(bool -> int -> bool -> int -> X))
                          ->
                          (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1)
                            (fun (x__4469:bool) (x__4470:int) (x__4471:bool) (x__4472:int) ->
                             (k_insertsort_xs'xs'_4372 x__4469 x__4470 x__4471 x__4472))))
                         (fun (x_4185:(x_1:int ->
                                       x_2:int ->
                                       (x_4:bool ->
                                        x_5:int ->
                                        x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7]
                                        -> X)
                                       -> X))
                          ->
                          (insert_1013 p12_4259
                            (fun (x__4462:int) (x__4463:int)
                                 (x__4464:(x_1:bool ->
                                           x_2:int ->
                                           x_3:bool ->
                                           x_4:int[(not x_1) || (not x_3) || (not (x__4462 <= x__4463)) || x_2 <= x_4]
                                           -> X))
                             ->
                             (x_4185 x__4462 x__4463
                               (fun (x__4465:bool) (x__4466:int) (x__4467:bool)
                                    (x__4468:x_1:int[(not x__4465) || (
                                                     not x__4467) || (
                                                     not (x__4462 <= x__4463)) ||
                                                     x__4466 <= x_1])
                                -> (x__4464 x__4465 x__4466 x__4467 x__4468))))
                            (fun (x__4454:(x_1:int ->
                                           x_2:int ->
                                           (x_4:bool ->
                                            x_5:int ->
                                            x_6:bool ->
                                            x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                                           -> X))
                             ->
                             (k_insertsort_2604
                               (fun (x__4455:int) (x__4456:int)
                                    (x__4457:(x_1:bool ->
                                              x_2:int ->
                                              x_3:bool ->
                                              x_4:int[(not x_1) || (not x_3) || (
                                                      not (x__4455 <= x__4456)) ||
                                                      x_2 <= x_4]
                                              -> X))
                                ->
                                (x__4454 x__4455 x__4456
                                  (fun (x__4458:bool) (x__4459:int) (x__4460:bool)
                                       (x__4461:x_1:int[(not x__4458) || (
                                                        not x__4460) || (
                                                        not (x__4455 <= x__4456)) ||
                                                        x__4459 <= x_1])
                                   -> (x__4457 x__4458 x__4459 x__4460 x__4461))))))))))))
                  (l0
                    (k_insertsort_2604
                      (fun (i1_4351:int) (i2_4352:int)
                           (k_insertsort_rsrs_4353:(x_1:bool ->
                                                    x_2:int ->
                                                    x_3:bool ->
                                                    x_4:int[(not x_1) || (
                                                            not x_3) || (
                                                            not (i1_4351 <= i2_4352)) ||
                                                            x_2 <= x_4]
                                                    -> X))
                       -> (k_insertsort_rsrs_4353 false 0 false 0)))))): (
bool -> int -> bool -> int -> X)
abst_arg: p11_4150, bool
abst_arg: p12_4151, int
abst_arg: p21_4152, bool
abst_arg: p22_4153, int
abst_arg: p11_4150, bool
abst_arg: p12_4151, int
abst_arg: p21_4152, bool
abst_arg: p22_4153, int
abstract_term: (if p11_4150
                 (l1
                   (xsxs_1037 0 0
                     (fun (p11_4258:bool) (p12_4259:int) (p21_4260:bool) (p22_4261:int) ->
                      (insertsort_2064
                        (fun (x1_4370:int) (x2_4371:int) (k_insertsort_xs'xs'_4372:(bool -> int -> bool -> int -> X))
                         ->
                         (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1)
                           (fun (x__4469:bool) (x__4470:int) (x__4471:bool) (x__4472:int) ->
                            (k_insertsort_xs'xs'_4372 x__4469 x__4470 x__4471 x__4472))))
                        (fun (x_4185:(x_1:int ->
                                      x_2:int ->
                                      (x_4:bool ->
                                       x_5:int ->
                                       x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7]
                                       -> X)
                                      -> X))
                         ->
                         (insert_1013 p12_4259
                           (fun (x__4462:int) (x__4463:int)
                                (x__4464:(x_1:bool ->
                                          x_2:int ->
                                          x_3:bool ->
                                          x_4:int[(not x_1) || (not x_3) || (not (x__4462 <= x__4463)) || x_2 <= x_4]
                                          -> X))
                            ->
                            (x_4185 x__4462 x__4463
                              (fun (x__4465:bool) (x__4466:int) (x__4467:bool)
                                   (x__4468:x_1:int[(not x__4465) || (
                                                    not x__4467) || (
                                                    not (x__4462 <= x__4463)) ||
                                                    x__4466 <= x_1])
                               -> (x__4464 x__4465 x__4466 x__4467 x__4468))))
                           (fun (x__4454:(x_1:int ->
                                          x_2:int ->
                                          (x_4:bool ->
                                           x_5:int ->
                                           x_6:bool ->
                                           x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                                          -> X))
                            ->
                            (k_insertsort_2604
                              (fun (x__4455:int) (x__4456:int)
                                   (x__4457:(x_1:bool ->
                                             x_2:int ->
                                             x_3:bool ->
                                             x_4:int[(not x_1) || (not x_3) || (not (x__4455 <= x__4456)) || x_2 <= x_4]
                                             -> X))
                               ->
                               (x__4454 x__4455 x__4456
                                 (fun (x__4458:bool) (x__4459:int) (x__4460:bool)
                                      (x__4461:x_1:int[(not x__4458) || (
                                                       not x__4460) || (
                                                       not (x__4455 <= x__4456)) ||
                                                       x__4459 <= x_1])
                                  -> (x__4457 x__4458 x__4459 x__4460 x__4461))))))))))))
                 (l0
                   (k_insertsort_2604
                     (fun (i1_4351:int) (i2_4352:int)
                          (k_insertsort_rsrs_4353:(x_1:bool ->
                                                   x_2:int ->
                                                   x_3:bool ->
                                                   x_4:int[(not x_1) || (
                                                           not x_3) || (
                                                           not (i1_4351 <= i2_4352)) ||
                                                           x_2 <= x_4]
                                                   -> X))
                      -> (k_insertsort_rsrs_4353 false 0 false 0))))): X
abstract_term: p11_4150: x_1:bool[x_1]
cond: true
pbs:
p:p11_4150
tt:false
ff:false

abstract_term: (l1
                 (xsxs_1037 0 0
                   (fun (p11_4258:bool) (p12_4259:int) (p21_4260:bool) (p22_4261:int) ->
                    (insertsort_2064
                      (fun (x1_4370:int) (x2_4371:int) (k_insertsort_xs'xs'_4372:(bool -> int -> bool -> int -> X)) ->
                       (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1)
                         (fun (x__4469:bool) (x__4470:int) (x__4471:bool) (x__4472:int) ->
                          (k_insertsort_xs'xs'_4372 x__4469 x__4470 x__4471 x__4472))))
                      (fun (x_4185:(x_1:int ->
                                    x_2:int ->
                                    (x_4:bool ->
                                     x_5:int ->
                                     x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] ->
                                     X)
                                    -> X))
                       ->
                       (insert_1013 p12_4259
                         (fun (x__4462:int) (x__4463:int)
                              (x__4464:(x_1:bool ->
                                        x_2:int ->
                                        x_3:bool ->
                                        x_4:int[(not x_1) || (not x_3) || (not (x__4462 <= x__4463)) || x_2 <= x_4] ->
                                        X))
                          ->
                          (x_4185 x__4462 x__4463
                            (fun (x__4465:bool) (x__4466:int) (x__4467:bool)
                                 (x__4468:x_1:int[(not x__4465) || (not x__4467) || (
                                                  not (x__4462 <= x__4463)) ||
                                                  x__4466 <= x_1])
                             -> (x__4464 x__4465 x__4466 x__4467 x__4468))))
                         (fun (x__4454:(x_1:int ->
                                        x_2:int ->
                                        (x_4:bool ->
                                         x_5:int ->
                                         x_6:bool ->
                                         x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                                        -> X))
                          ->
                          (k_insertsort_2604
                            (fun (x__4455:int) (x__4456:int)
                                 (x__4457:(x_1:bool ->
                                           x_2:int ->
                                           x_3:bool ->
                                           x_4:int[(not x_1) || (not x_3) || (not (x__4455 <= x__4456)) || x_2 <= x_4]
                                           -> X))
                             ->
                             (x__4454 x__4455 x__4456
                               (fun (x__4458:bool) (x__4459:int) (x__4460:bool)
                                    (x__4461:x_1:int[(not x__4458) || (
                                                     not x__4460) || (
                                                     not (x__4455 <= x__4456)) ||
                                                     x__4459 <= x_1])
                                -> (x__4457 x__4458 x__4459 x__4460 x__4461)))))))))))): X
abstract_term: (xsxs_1037 0 0
                 (fun (p11_4258:bool) (p12_4259:int) (p21_4260:bool) (p22_4261:int) ->
                  (insertsort_2064
                    (fun (x1_4370:int) (x2_4371:int) (k_insertsort_xs'xs'_4372:(bool -> int -> bool -> int -> X)) ->
                     (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1)
                       (fun (x__4469:bool) (x__4470:int) (x__4471:bool) (x__4472:int) ->
                        (k_insertsort_xs'xs'_4372 x__4469 x__4470 x__4471 x__4472))))
                    (fun (x_4185:(x_1:int ->
                                  x_2:int ->
                                  (x_4:bool ->
                                   x_5:int ->
                                   x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                                  -> X))
                     ->
                     (insert_1013 p12_4259
                       (fun (x__4462:int) (x__4463:int)
                            (x__4464:(x_1:bool ->
                                      x_2:int ->
                                      x_3:bool ->
                                      x_4:int[(not x_1) || (not x_3) || (not (x__4462 <= x__4463)) || x_2 <= x_4] -> X))
                        ->
                        (x_4185 x__4462 x__4463
                          (fun (x__4465:bool) (x__4466:int) (x__4467:bool)
                               (x__4468:x_1:int[(not x__4465) || (not x__4467) || (
                                                not (x__4462 <= x__4463)) ||
                                                x__4466 <= x_1])
                           -> (x__4464 x__4465 x__4466 x__4467 x__4468))))
                       (fun (x__4454:(x_1:int ->
                                      x_2:int ->
                                      (x_4:bool ->
                                       x_5:int ->
                                       x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7]
                                       -> X)
                                      -> X))
                        ->
                        (k_insertsort_2604
                          (fun (x__4455:int) (x__4456:int)
                               (x__4457:(x_1:bool ->
                                         x_2:int ->
                                         x_3:bool ->
                                         x_4:int[(not x_1) || (not x_3) || (not (x__4455 <= x__4456)) || x_2 <= x_4] ->
                                         X))
                           ->
                           (x__4454 x__4455 x__4456
                             (fun (x__4458:bool) (x__4459:int) (x__4460:bool)
                                  (x__4461:x_1:int[(not x__4458) || (
                                                   not x__4460) || (not (x__4455 <= x__4456)) ||
                                                   x__4459 <= x_1])
                              -> (x__4457 x__4458 x__4459 x__4460 x__4461))))))))))): X
abstract_term: (fun (p11_4258:bool) (p12_4259:int) (p21_4260:bool) (p22_4261:int) ->
                (insertsort_2064
                  (fun (x1_4370:int) (x2_4371:int) (k_insertsort_xs'xs'_4372:(bool -> int -> bool -> int -> X)) ->
                   (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1)
                     (fun (x__4469:bool) (x__4470:int) (x__4471:bool) (x__4472:int) ->
                      (k_insertsort_xs'xs'_4372 x__4469 x__4470 x__4471 x__4472))))
                  (fun (x_4185:(x_1:int ->
                                x_2:int ->
                                (x_4:bool ->
                                 x_5:int ->
                                 x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                                -> X))
                   ->
                   (insert_1013 p12_4259
                     (fun (x__4462:int) (x__4463:int)
                          (x__4464:(x_1:bool ->
                                    x_2:int ->
                                    x_3:bool ->
                                    x_4:int[(not x_1) || (not x_3) || (not (x__4462 <= x__4463)) || x_2 <= x_4] -> X))
                      ->
                      (x_4185 x__4462 x__4463
                        (fun (x__4465:bool) (x__4466:int) (x__4467:bool)
                             (x__4468:x_1:int[(not x__4465) || (not x__4467) || (
                                              not (x__4462 <= x__4463)) ||
                                              x__4466 <= x_1])
                         -> (x__4464 x__4465 x__4466 x__4467 x__4468))))
                     (fun (x__4454:(x_1:int ->
                                    x_2:int ->
                                    (x_4:bool ->
                                     x_5:int ->
                                     x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] ->
                                     X)
                                    -> X))
                      ->
                      (k_insertsort_2604
                        (fun (x__4455:int) (x__4456:int)
                             (x__4457:(x_1:bool ->
                                       x_2:int ->
                                       x_3:bool ->
                                       x_4:int[(not x_1) || (not x_3) || (not (x__4455 <= x__4456)) || x_2 <= x_4] -> X))
                         ->
                         (x__4454 x__4455 x__4456
                           (fun (x__4458:bool) (x__4459:int) (x__4460:bool)
                                (x__4461:x_1:int[(not x__4458) || (not x__4460) || (
                                                 not (x__4455 <= x__4456)) ||
                                                 x__4459 <= x_1])
                            -> (x__4457 x__4458 x__4459 x__4460 x__4461)))))))))): (
bool -> int -> bool -> int -> X)
abst_arg: p11_4258, bool
abst_arg: p12_4259, int
abst_arg: p21_4260, bool
abst_arg: p22_4261, int
abst_arg: p11_4258, bool
abst_arg: p12_4259, int
abst_arg: p21_4260, bool
abst_arg: p22_4261, int
abstract_term: (insertsort_2064
                 (fun (x1_4370:int) (x2_4371:int) (k_insertsort_xs'xs'_4372:(bool -> int -> bool -> int -> X)) ->
                  (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1)
                    (fun (x__4469:bool) (x__4470:int) (x__4471:bool) (x__4472:int) ->
                     (k_insertsort_xs'xs'_4372 x__4469 x__4470 x__4471 x__4472))))
                 (fun (x_4185:(x_1:int ->
                               x_2:int ->
                               (x_4:bool ->
                                x_5:int ->
                                x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                               -> X))
                  ->
                  (insert_1013 p12_4259
                    (fun (x__4462:int) (x__4463:int)
                         (x__4464:(x_1:bool ->
                                   x_2:int ->
                                   x_3:bool ->
                                   x_4:int[(not x_1) || (not x_3) || (not (x__4462 <= x__4463)) || x_2 <= x_4] -> X))
                     ->
                     (x_4185 x__4462 x__4463
                       (fun (x__4465:bool) (x__4466:int) (x__4467:bool)
                            (x__4468:x_1:int[(not x__4465) || (not x__4467) || (
                                             not (x__4462 <= x__4463)) ||
                                             x__4466 <= x_1])
                        -> (x__4464 x__4465 x__4466 x__4467 x__4468))))
                    (fun (x__4454:(x_1:int ->
                                   x_2:int ->
                                   (x_4:bool ->
                                    x_5:int ->
                                    x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] ->
                                    X)
                                   -> X))
                     ->
                     (k_insertsort_2604
                       (fun (x__4455:int) (x__4456:int)
                            (x__4457:(x_1:bool ->
                                      x_2:int ->
                                      x_3:bool ->
                                      x_4:int[(not x_1) || (not x_3) || (not (x__4455 <= x__4456)) || x_2 <= x_4] -> X))
                        ->
                        (x__4454 x__4455 x__4456
                          (fun (x__4458:bool) (x__4459:int) (x__4460:bool)
                               (x__4461:x_1:int[(not x__4458) || (not x__4460) || (
                                                not (x__4455 <= x__4456)) ||
                                                x__4459 <= x_1])
                           -> (x__4457 x__4458 x__4459 x__4460 x__4461))))))))): X
abstract_term: (fun (x_4185:(x_1:int ->
                             x_2:int ->
                             (x_4:bool ->
                              x_5:int ->
                              x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                             -> X))
                ->
                (insert_1013 p12_4259
                  (fun (x__4462:int) (x__4463:int)
                       (x__4464:(x_1:bool ->
                                 x_2:int ->
                                 x_3:bool ->
                                 x_4:int[(not x_1) || (not x_3) || (not (x__4462 <= x__4463)) || x_2 <= x_4] -> X))
                   ->
                   (x_4185 x__4462 x__4463
                     (fun (x__4465:bool) (x__4466:int) (x__4467:bool)
                          (x__4468:x_1:int[(not x__4465) || (not x__4467) || (
                                           not (x__4462 <= x__4463)) ||
                                           x__4466 <= x_1])
                      -> (x__4464 x__4465 x__4466 x__4467 x__4468))))
                  (fun (x__4454:(x_1:int ->
                                 x_2:int ->
                                 (x_4:bool ->
                                  x_5:int ->
                                  x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                                 -> X))
                   ->
                   (k_insertsort_2604
                     (fun (x__4455:int) (x__4456:int)
                          (x__4457:(x_1:bool ->
                                    x_2:int ->
                                    x_3:bool ->
                                    x_4:int[(not x_1) || (not x_3) || (not (x__4455 <= x__4456)) || x_2 <= x_4] -> X))
                      ->
                      (x__4454 x__4455 x__4456
                        (fun (x__4458:bool) (x__4459:int) (x__4460:bool)
                             (x__4461:x_1:int[(not x__4458) || (not x__4460) || (
                                              not (x__4455 <= x__4456)) ||
                                              x__4459 <= x_1])
                         -> (x__4457 x__4458 x__4459 x__4460 x__4461)))))))): ((
x_2:int ->
x_3:int ->
(x_5:bool -> x_6:int -> x_7:bool -> x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_6 <= x_8] -> X) -> X) ->
X)
abst_arg: x_4185, (x_1:int ->
                   x_2:int ->
                   (x_4:bool ->
                    x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                   -> X)
abst_arg: x_4185, (x_1:int ->
                   x_2:int ->
                   (x_4:bool ->
                    x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                   -> X)
abstract_term: (insert_1013 p12_4259
                 (fun (x__4462:int) (x__4463:int)
                      (x__4464:(x_1:bool ->
                                x_2:int ->
                                x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4462 <= x__4463)) || x_2 <= x_4]
                                -> X))
                  ->
                  (x_4185 x__4462 x__4463
                    (fun (x__4465:bool) (x__4466:int) (x__4467:bool)
                         (x__4468:x_1:int[(not x__4465) || (not x__4467) || (
                                          not (x__4462 <= x__4463)) ||
                                          x__4466 <= x_1])
                     -> (x__4464 x__4465 x__4466 x__4467 x__4468))))
                 (fun (x__4454:(x_1:int ->
                                x_2:int ->
                                (x_4:bool ->
                                 x_5:int ->
                                 x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                                -> X))
                  ->
                  (k_insertsort_2604
                    (fun (x__4455:int) (x__4456:int)
                         (x__4457:(x_1:bool ->
                                   x_2:int ->
                                   x_3:bool ->
                                   x_4:int[(not x_1) || (not x_3) || (not (x__4455 <= x__4456)) || x_2 <= x_4] -> X))
                     ->
                     (x__4454 x__4455 x__4456
                       (fun (x__4458:bool) (x__4459:int) (x__4460:bool)
                            (x__4461:x_1:int[(not x__4458) || (not x__4460) || (
                                             not (x__4455 <= x__4456)) ||
                                             x__4459 <= x_1])
                        -> (x__4457 x__4458 x__4459 x__4460 x__4461))))))): X
abstract_term: (fun (x__4454:(x_1:int ->
                              x_2:int ->
                              (x_4:bool ->
                               x_5:int ->
                               x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                              -> X))
                ->
                (k_insertsort_2604
                  (fun (x__4455:int) (x__4456:int)
                       (x__4457:(x_1:bool ->
                                 x_2:int ->
                                 x_3:bool ->
                                 x_4:int[(not x_1) || (not x_3) || (not (x__4455 <= x__4456)) || x_2 <= x_4] -> X))
                   ->
                   (x__4454 x__4455 x__4456
                     (fun (x__4458:bool) (x__4459:int) (x__4460:bool)
                          (x__4461:x_1:int[(not x__4458) || (not x__4460) || (
                                           not (x__4455 <= x__4456)) ||
                                           x__4459 <= x_1])
                      -> (x__4457 x__4458 x__4459 x__4460 x__4461)))))): ((
x_2:int ->
x_3:int ->
(x_5:bool -> x_6:int -> x_7:bool -> x_8:int[(not x_5) || (not x_7) || (not (x_2 <= x_3)) || x_6 <= x_8] -> X) -> X) ->
X)
abst_arg: x__4454, (x_1:int ->
                    x_2:int ->
                    (x_4:bool ->
                     x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                    -> X)
abst_arg: x__4454, (x_1:int ->
                    x_2:int ->
                    (x_4:bool ->
                     x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X)
                    -> X)
abstract_term: (k_insertsort_2604
                 (fun (x__4455:int) (x__4456:int)
                      (x__4457:(x_1:bool ->
                                x_2:int ->
                                x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4455 <= x__4456)) || x_2 <= x_4]
                                -> X))
                  ->
                  (x__4454 x__4455 x__4456
                    (fun (x__4458:bool) (x__4459:int) (x__4460:bool)
                         (x__4461:x_1:int[(not x__4458) || (not x__4460) || (
                                          not (x__4455 <= x__4456)) ||
                                          x__4459 <= x_1])
                     -> (x__4457 x__4458 x__4459 x__4460 x__4461))))): X
abstract_term: (fun (x__4455:int) (x__4456:int)
                    (x__4457:(x_1:bool ->
                              x_2:int ->
                              x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4455 <= x__4456)) || x_2 <= x_4]
                              -> X))
                ->
                (x__4454 x__4455 x__4456
                  (fun (x__4458:bool) (x__4459:int) (x__4460:bool)
                       (x__4461:x_1:int[(not x__4458) || (not x__4460) || (not (x__4455 <= x__4456)) || x__4459 <= x_1])
                   -> (x__4457 x__4458 x__4459 x__4460 x__4461)))): (
x_1:int ->
x_2:int ->
(x_4:bool -> x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X) -> X)
abst_arg: x__4455, int
abst_arg: x__4456, int
abst_arg: x__4457, (x_1:bool ->
                    x_2:int ->
                    x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4455 <= x__4456)) || x_2 <= x_4] -> X)
abst_arg: x__4455, int
abst_arg: x__4456, int
abst_arg: x__4457, (x_1:bool ->
                    x_2:int ->
                    x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4455 <= x__4456)) || x_2 <= x_4] -> X)
abstract_term: (x__4454 x__4455 x__4456
                 (fun (x__4458:bool) (x__4459:int) (x__4460:bool)
                      (x__4461:x_1:int[(not x__4458) || (not x__4460) || (not (x__4455 <= x__4456)) || x__4459 <= x_1])
                  -> (x__4457 x__4458 x__4459 x__4460 x__4461))): X
abstract_term: (fun (x__4458:bool) (x__4459:int) (x__4460:bool)
                    (x__4461:x_1:int[(not x__4458) || (not x__4460) || (not (x__4455 <= x__4456)) || x__4459 <= x_1])
                -> (x__4457 x__4458 x__4459 x__4460 x__4461)): (x_1:bool ->
                                                                x_2:int ->
                                                                x_3:bool ->
                                                                x_4:int[
                                                                (not x_1) || (
                                                                not x_3) || (
                                                                not (x__4455 <= x__4456)) ||
                                                                x_2 <= x_4] -> X)
abst_arg: x__4458, bool
abst_arg: x__4459, int
abst_arg: x__4460, bool
abst_arg: x__4461, x_1:int[(not x__4458) || (not x__4460) || (not (x__4455 <= x__4456)) || x__4459 <= x_1]
abst_arg: x__4458, bool
abst_arg: x__4459, int
abst_arg: x__4460, bool
abst_arg: x__4461, x_1:int[(not x__4458) || (not x__4460) || (not (x__4455 <= x__4456)) || x__4459 <= x_1]
abstract_term: (x__4457 x__4458 x__4459 x__4460 x__4461): X
abstract_term: x__4461: x_1:int[(not x__4458) || (not x__4460) || (not (x__4455 <= x__4456)) || x__4459 <= x_1]
cond: p11_4150; true
pbs: x__4461 := ((((not x__4458) || (not x__4460)) || (not (x__4455 <= x__4456))) || (x__4459 <= x__4461))
p:((((not x__4458) || (not x__4460)) || (not (x__4455 <= x__4456))) || (x__4459 <= x__4461))
tt:x__4461
ff:false

abstract_term: x__4460: bool
abstract_term: x__4459: int
abstract_term: x__4458: bool
abstract_term: x__4456: int
abstract_term: x__4455: int
abstract_term: (fun (x__4462:int) (x__4463:int)
                    (x__4464:(x_1:bool ->
                              x_2:int ->
                              x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4462 <= x__4463)) || x_2 <= x_4]
                              -> X))
                ->
                (x_4185 x__4462 x__4463
                  (fun (x__4465:bool) (x__4466:int) (x__4467:bool)
                       (x__4468:x_1:int[(not x__4465) || (not x__4467) || (not (x__4462 <= x__4463)) || x__4466 <= x_1])
                   -> (x__4464 x__4465 x__4466 x__4467 x__4468)))): (
x_1:int ->
x_2:int ->
(x_4:bool -> x_5:int -> x_6:bool -> x_7:int[(not x_4) || (not x_6) || (not (x_1 <= x_2)) || x_5 <= x_7] -> X) -> X)
abst_arg: x__4462, int
abst_arg: x__4463, int
abst_arg: x__4464, (x_1:bool ->
                    x_2:int ->
                    x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4462 <= x__4463)) || x_2 <= x_4] -> X)
abst_arg: x__4462, int
abst_arg: x__4463, int
abst_arg: x__4464, (x_1:bool ->
                    x_2:int ->
                    x_3:bool -> x_4:int[(not x_1) || (not x_3) || (not (x__4462 <= x__4463)) || x_2 <= x_4] -> X)
abstract_term: (x_4185 x__4462 x__4463
                 (fun (x__4465:bool) (x__4466:int) (x__4467:bool)
                      (x__4468:x_1:int[(not x__4465) || (not x__4467) || (not (x__4462 <= x__4463)) || x__4466 <= x_1])
                  -> (x__4464 x__4465 x__4466 x__4467 x__4468))): X
abstract_term: (fun (x__4465:bool) (x__4466:int) (x__4467:bool)
                    (x__4468:x_1:int[(not x__4465) || (not x__4467) || (not (x__4462 <= x__4463)) || x__4466 <= x_1])
                -> (x__4464 x__4465 x__4466 x__4467 x__4468)): (x_1:bool ->
                                                                x_2:int ->
                                                                x_3:bool ->
                                                                x_4:int[
                                                                (not x_1) || (
                                                                not x_3) || (
                                                                not (x__4462 <= x__4463)) ||
                                                                x_2 <= x_4] -> X)
abst_arg: x__4465, bool
abst_arg: x__4466, int
abst_arg: x__4467, bool
abst_arg: x__4468, x_1:int[(not x__4465) || (not x__4467) || (not (x__4462 <= x__4463)) || x__4466 <= x_1]
abst_arg: x__4465, bool
abst_arg: x__4466, int
abst_arg: x__4467, bool
abst_arg: x__4468, x_1:int[(not x__4465) || (not x__4467) || (not (x__4462 <= x__4463)) || x__4466 <= x_1]
abstract_term: (x__4464 x__4465 x__4466 x__4467 x__4468): X
abstract_term: x__4468: x_1:int[(not x__4465) || (not x__4467) || (not (x__4462 <= x__4463)) || x__4466 <= x_1]
cond: p11_4150; true
pbs: x__4468 := ((((not x__4465) || (not x__4467)) || (not (x__4462 <= x__4463))) || (x__4466 <= x__4468))
p:((((not x__4465) || (not x__4467)) || (not (x__4462 <= x__4463))) || (x__4466 <= x__4468))
tt:x__4468
ff:false

abstract_term: x__4467: bool
abstract_term: x__4466: int
abstract_term: x__4465: bool
abstract_term: x__4463: int
abstract_term: x__4462: int
abstract_term: p12_4259: int
abstract_term: (fun (x1_4370:int) (x2_4371:int) (k_insertsort_xs'xs'_4372:(bool -> int -> bool -> int -> X)) ->
                (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1)
                  (fun (x__4469:bool) (x__4470:int) (x__4471:bool) (x__4472:int) ->
                   (k_insertsort_xs'xs'_4372 x__4469 x__4470 x__4471 x__4472)))): (
int -> int -> (bool -> int -> bool -> int -> X) -> X)
abst_arg: x1_4370, int
abst_arg: x2_4371, int
abst_arg: k_insertsort_xs'xs'_4372, (bool -> int -> bool -> int -> X)
abst_arg: x1_4370, int
abst_arg: x2_4371, int
abst_arg: k_insertsort_xs'xs'_4372, (bool -> int -> bool -> int -> X)
abstract_term: (xsxs_1037 (x1_4370 + 1) (x2_4371 + 1)
                 (fun (x__4469:bool) (x__4470:int) (x__4471:bool) (x__4472:int) ->
                  (k_insertsort_xs'xs'_4372 x__4469 x__4470 x__4471 x__4472))): X
abstract_term: (fun (x__4469:bool) (x__4470:int) (x__4471:bool) (x__4472:int) ->
                (k_insertsort_xs'xs'_4372 x__4469 x__4470 x__4471 x__4472)): (
bool -> int -> bool -> int -> X)
abst_arg: x__4469, bool
abst_arg: x__4470, int
abst_arg: x__4471, bool
abst_arg: x__4472, int
abst_arg: x__4469, bool
abst_arg: x__4470, int
abst_arg: x__4471, bool
abst_arg: x__4472, int
abstract_term: (k_insertsort_xs'xs'_4372 x__4469 x__4470 x__4471 x__4472): X
abstract_term: x__4472: int
abstract_term: x__4471: bool
abstract_term: x__4470: int
abstract_term: x__4469: bool
abstract_term: (x2_4371 + 1): int
abstract_term: (x1_4370 + 1): int
abstract_term: 0: int
abstract_term: 0: int
abstract_term: (l0
                 (k_insertsort_2604
                   (fun (i1_4351:int) (i2_4352:int)
                        (k_insertsort_rsrs_4353:(x_1:bool ->
                                                 x_2:int ->
                                                 x_3:bool ->
                                                 x_4:int[(not x_1) || (
                                                         not x_3) || (
                                                         not (i1_4351 <= i2_4352)) ||
                                                         x_2 <= x_4]
                                                 -> X))
                    -> (k_insertsort_rsrs_4353 false 0 false 0)))): X
abstract_term: (k_insertsort_2604
                 (fun (i1_4351:int) (i2_4352:int)
                      (k_insertsort_rsrs_4353:(x_1:bool ->
                                               x_2:int ->
                                               x_3:bool ->
                                               x_4:int[(not x_1) || (
                                                       not x_3) || (not (i1_4351 <= i2_4352)) ||
                                                       x_2 <= x_4]
                                               -> X))
                  -> (k_insertsort_rsrs_4353 false 0 false 0))): X
abstract_term: (fun (i1_4351:int) (i2_4352:int)
                    (k_insertsort_rsrs_4353:(x_1:bool ->
                                             x_2:int ->
                                             x_3:bool ->
                                             x_4:int[(not x_1) || (not x_3) || (not (i1_4351 <= i2_4352)) || x_2 <= x_4]
                                             -> X))
                -> (k_insertsort_rsrs_4353 false 0 false 0)): (x_1:int ->
                                                               x_2:int ->
                                                               (x_4:bool ->
                                                                x_5:int ->
                                                                x_6:bool ->
                                                                x_7:int[
                                                                (not x_4) || (
                                                                not x_6) || (
                                                                not (x_1 <= x_2)) ||
                                                                x_5 <= x_7] -> X)
                                                               -> X)
abst_arg: i1_4351, int
abst_arg: i2_4352, int
abst_arg: k_insertsort_rsrs_4353, (x_1:bool ->
                                   x_2:int ->
                                   x_3:bool ->
                                   x_4:int[(not x_1) || (not x_3) || (not (i1_4351 <= i2_4352)) || x_2 <= x_4] -> X)
abst_arg: i1_4351, int
abst_arg: i2_4352, int
abst_arg: k_insertsort_rsrs_4353, (x_1:bool ->
                                   x_2:int ->
                                   x_3:bool ->
                                   x_4:int[(not x_1) || (not x_3) || (not (i1_4351 <= i2_4352)) || x_2 <= x_4] -> X)
abstract_term: (k_insertsort_rsrs_4353 false 0 false 0): X
abstract_term: 0: x_1:int[(not false) || (not false) || (not (i1_4351 <= i2_4352)) || 0 <= x_1]
cond: (not p11_4150); true
pbs:
p:((((not false) || (not false)) || (not (i1_4351 <= i2_4352))) || (0 <= 0))
tt:true
ff:false

abstract_term: false: bool
abstract_term: 0: int
abstract_term: false: bool
abstract_term: 0: int
abstract_term: 0: int
loop_3286: ENV: b_3275:bool, x1_1231:int, x2_1231:int, x_1050:int, xs11_2354:bool, xs12_2354:int, xs1_2584:bool,
xs21_2354:bool, xs22_2354:int, xs2_2584:int, x_3287:unit, k_insert_loop_3288:(
bool -> int -> bool -> int -> X),


abst_arg: b_3275, bool
abst_arg: x1_1231, int
abst_arg: x2_1231, int
abst_arg: x_1050, int
abst_arg: xs11_2354, bool
abst_arg: xs12_2354, int
abst_arg: xs1_2584, bool
abst_arg: xs21_2354, bool
abst_arg: xs22_2354, int
abst_arg: xs2_2584, int
abst_arg: x_3287, unit
abst_arg: k_insert_loop_3288, (bool -> int -> bool -> int -> X)
abst_arg: b_3275, bool
abst_arg: x1_1231, int
abst_arg: x2_1231, int
abst_arg: x_1050, int
abst_arg: xs11_2354, bool
abst_arg: xs12_2354, int
abst_arg: xs1_2584, bool
abst_arg: xs21_2354, bool
abst_arg: xs22_2354, int
abst_arg: xs2_2584, int
abst_arg: x_3287, unit
abst_arg: k_insert_loop_3288, (bool -> int -> bool -> int -> X)
loop_3286: (loop_3286 b_3275 x1_1231 x2_1231 x_1050 xs11_2354 xs12_2354 xs1_2584 xs21_2354 xs22_2354 xs2_2584 ()
             (fun (x__4473:bool) (x__4474:int) (x__4475:bool) (x__4476:int) ->
              (k_insert_loop_3288 x__4473 x__4474 x__4475 x__4476))) ===> (
loop_3286 b_3275 x1_1231 x2_1231 x_1050 xs11_2354 xs12_2354 xs1_2584 xs21_2354 xs22_2354 xs2_2584 ()
 (fun (x__4473:bool) (x__4474:int) (x__4475:bool) (x__4476:int) -> (k_insert_loop_3288 x__4473 x__4474 x__4475 x__4476)))
loop_3286:: (loop_3286 b_3275 x1_1231 x2_1231 x_1050 xs11_2354 xs12_2354 xs1_2584 xs21_2354 xs22_2354 xs2_2584 ()
              (fun (x__4473:bool) (x__4474:int) (x__4475:bool) (x__4476:int) ->
               (k_insert_loop_3288 x__4473 x__4474 x__4475 x__4476)))
abstract_term: (loop_3286 b_3275 x1_1231 x2_1231 x_1050 xs11_2354 xs12_2354 xs1_2584 xs21_2354 xs22_2354 xs2_2584 ()
                 (fun (x__4473:bool) (x__4474:int) (x__4475:bool) (x__4476:int) ->
                  (k_insert_loop_3288 x__4473 x__4474 x__4475 x__4476))): X
abstract_term: (fun (x__4473:bool) (x__4474:int) (x__4475:bool) (x__4476:int) ->
                (k_insert_loop_3288 x__4473 x__4474 x__4475 x__4476)): (
bool -> int -> bool -> int -> X)
abst_arg: x__4473, bool
abst_arg: x__4474, int
abst_arg: x__4475, bool
abst_arg: x__4476, int
abst_arg: x__4473, bool
abst_arg: x__4474, int
abst_arg: x__4475, bool
abst_arg: x__4476, int
abstract_term: (k_insert_loop_3288 x__4473 x__4474 x__4475 x__4476): X
abstract_term: x__4476: int
abstract_term: x__4475: bool
abstract_term: x__4474: int
abstract_term: x__4473: bool
abstract_term: (): unit
abstract_term: xs2_2584: int
abstract_term: xs22_2354: int
abstract_term: xs21_2354: bool
abstract_term: xs1_2584: bool
abstract_term: xs12_2354: int
abstract_term: xs11_2354: bool
abstract_term: x_1050: int
abstract_term: x2_1231: int
abstract_term: x1_1231: int
abstract_term: b_3275: bool
make_list_1044: ENV: n_1045:int, k_make_list_2791:((int -> (bool -> int -> X) -> X) -> X),


abst_arg: n_1045, int
abst_arg: k_make_list_2791, ((int -> (bool -> int -> X) -> X) ->
X)
abst_arg: n_1045, int
abst_arg: k_make_list_2791, ((int -> (bool -> int -> X) -> X) ->
X)
make_list_1044: (l0
                  (k_make_list_2791
                    (fun (i_4201:int) (k_make_list_4202:(bool -> int -> X)) -> (k_make_list_4202 false 0)))) ===> (
l0 (k_make_list_2791 (fun (i_4201:int) (k_make_list_4202:(bool -> int -> X)) -> (k_make_list_4202 false 0))))
make_list_1044:: (l0
                   (k_make_list_2791
                     (fun (i_4201:int) (k_make_list_4202:(bool -> int -> X)) -> (k_make_list_4202 false 0))))
abstract_term: (l0
                 (k_make_list_2791
                   (fun (i_4201:int) (k_make_list_4202:(bool -> int -> X)) -> (k_make_list_4202 false 0)))): X
abstract_term: (k_make_list_2791 (fun (i_4201:int) (k_make_list_4202:(bool -> int -> X)) -> (k_make_list_4202 false 0))): X
abstract_term: (fun (i_4201:int) (k_make_list_4202:(bool -> int -> X)) -> (k_make_list_4202 false 0)): (
int -> (bool -> int -> X) -> X)
abst_arg: i_4201, int
abst_arg: k_make_list_4202, (bool -> int -> X)
abst_arg: i_4201, int
abst_arg: k_make_list_4202, (bool -> int -> X)
abstract_term: (k_make_list_4202 false 0): X
abstract_term: 0: int
abstract_term: false: bool
make_list_1044: ENV: n_1045:int, k_make_list_2791:((int -> (bool -> int -> X) -> X) -> X),


abst_arg: n_1045, int
abst_arg: k_make_list_2791, ((int -> (bool -> int -> X) -> X) ->
X)
abst_arg: n_1045, int
abst_arg: k_make_list_2791, ((int -> (bool -> int -> X) -> X) ->
X)
make_list_1044: (l1
                  (rand_int
                    (fun (n_4205:int) ->
                     (make_list_1044 (n_4205 - 1)
                       (fun (xs_4209:(int -> (bool -> int -> X) -> X)) ->
                        (k_make_list_2791
                          (fun (i_4218:int) (k_make_list_4219:(bool -> int -> X)) ->
                           (if (i_4218 = 0) (l0 (k_make_list_4219 true n_4205))
                             (l1
                               (xs_4209 (i_4218 - 1)
                                 (fun (x__4477:bool) (x__4478:int) -> (k_make_list_4219 x__4477 x__4478)))))))))))) ===> (
l1
 (rand_int
   (fun (n_4205:int) ->
    (make_list_1044 (n_4205 - 1)
      (fun (xs_4209:(int -> (bool -> int -> X) -> X)) ->
       (k_make_list_2791
         (fun (i_4218:int) (k_make_list_4219:(bool -> int -> X)) ->
          (if (i_4218 = 0) (l0 (k_make_list_4219 true n_4205))
            (l1 (xs_4209 (i_4218 - 1) (fun (x__4477:bool) (x__4478:int) -> (k_make_list_4219 x__4477 x__4478))))))))))))
make_list_1044:: (l1
                   (rand_int
                     (fun (n_4205:int) ->
                      (make_list_1044 (n_4205 - 1)
                        (fun (xs_4209:(int -> (bool -> int -> X) -> X)) ->
                         (k_make_list_2791
                           (fun (i_4218:int) (k_make_list_4219:(bool -> int -> X)) ->
                            (if (i_4218 = 0) (l0 (k_make_list_4219 true n_4205))
                              (l1
                                (xs_4209 (i_4218 - 1)
                                  (fun (x__4477:bool) (x__4478:int) -> (k_make_list_4219 x__4477 x__4478))))))))))))
abstract_term: (l1
                 (rand_int
                   (fun (n_4205:int) ->
                    (make_list_1044 (n_4205 - 1)
                      (fun (xs_4209:(int -> (bool -> int -> X) -> X)) ->
                       (k_make_list_2791
                         (fun (i_4218:int) (k_make_list_4219:(bool -> int -> X)) ->
                          (if (i_4218 = 0) (l0 (k_make_list_4219 true n_4205))
                            (l1
                              (xs_4209 (i_4218 - 1)
                                (fun (x__4477:bool) (x__4478:int) -> (k_make_list_4219 x__4477 x__4478)))))))))))): X
abstract_term: (rand_int
                 (fun (n_4205:int) ->
                  (make_list_1044 (n_4205 - 1)
                    (fun (xs_4209:(int -> (bool -> int -> X) -> X)) ->
                     (k_make_list_2791
                       (fun (i_4218:int) (k_make_list_4219:(bool -> int -> X)) ->
                        (if (i_4218 = 0) (l0 (k_make_list_4219 true n_4205))
                          (l1
                            (xs_4209 (i_4218 - 1)
                              (fun (x__4477:bool) (x__4478:int) -> (k_make_list_4219 x__4477 x__4478))))))))))): X
abstract_term: (fun (n_4205:int) ->
                (make_list_1044 (n_4205 - 1)
                  (fun (xs_4209:(int -> (bool -> int -> X) -> X)) ->
                   (k_make_list_2791
                     (fun (i_4218:int) (k_make_list_4219:(bool -> int -> X)) ->
                      (if (i_4218 = 0) (l0 (k_make_list_4219 true n_4205))
                        (l1
                          (xs_4209 (i_4218 - 1)
                            (fun (x__4477:bool) (x__4478:int) -> (k_make_list_4219 x__4477 x__4478)))))))))): (int ->
X)
abst_arg: n_4205, int
abst_arg: n_4205, int
abstract_term: (make_list_1044 (n_4205 - 1)
                 (fun (xs_4209:(int -> (bool -> int -> X) -> X)) ->
                  (k_make_list_2791
                    (fun (i_4218:int) (k_make_list_4219:(bool -> int -> X)) ->
                     (if (i_4218 = 0) (l0 (k_make_list_4219 true n_4205))
                       (l1
                         (xs_4209 (i_4218 - 1) (fun (x__4477:bool) (x__4478:int) -> (k_make_list_4219 x__4477 x__4478))))))))): X
abstract_term: (fun (xs_4209:(int -> (bool -> int -> X) -> X)) ->
                (k_make_list_2791
                  (fun (i_4218:int) (k_make_list_4219:(bool -> int -> X)) ->
                   (if (i_4218 = 0) (l0 (k_make_list_4219 true n_4205))
                     (l1
                       (xs_4209 (i_4218 - 1) (fun (x__4477:bool) (x__4478:int) -> (k_make_list_4219 x__4477 x__4478)))))))): ((
int -> (bool -> int -> X) -> X) ->
X)
abst_arg: xs_4209, (int -> (bool -> int -> X) -> X)
abst_arg: xs_4209, (int -> (bool -> int -> X) -> X)
abstract_term: (k_make_list_2791
                 (fun (i_4218:int) (k_make_list_4219:(bool -> int -> X)) ->
                  (if (i_4218 = 0) (l0 (k_make_list_4219 true n_4205))
                    (l1 (xs_4209 (i_4218 - 1) (fun (x__4477:bool) (x__4478:int) -> (k_make_list_4219 x__4477 x__4478))))))): X
abstract_term: (fun (i_4218:int) (k_make_list_4219:(bool -> int -> X)) ->
                (if (i_4218 = 0) (l0 (k_make_list_4219 true n_4205))
                  (l1 (xs_4209 (i_4218 - 1) (fun (x__4477:bool) (x__4478:int) -> (k_make_list_4219 x__4477 x__4478)))))): (
int -> (bool -> int -> X) -> X)
abst_arg: i_4218, int
abst_arg: k_make_list_4219, (bool -> int -> X)
abst_arg: i_4218, int
abst_arg: k_make_list_4219, (bool -> int -> X)
abstract_term: (if (i_4218 = 0) (l0 (k_make_list_4219 true n_4205))
                 (l1 (xs_4209 (i_4218 - 1) (fun (x__4477:bool) (x__4478:int) -> (k_make_list_4219 x__4477 x__4478))))): X
abstract_term: (i_4218 = 0): x_1:bool[x_1]
cond: (not (n_1045 = 0))
pbs:
p:(i_4218 = 0)
tt:false
ff:false

abstract_term: (l0 (k_make_list_4219 true n_4205)): X
abstract_term: (k_make_list_4219 true n_4205): X
abstract_term: n_4205: int
abstract_term: true: bool
abstract_term: (l1 (xs_4209 (i_4218 - 1) (fun (x__4477:bool) (x__4478:int) -> (k_make_list_4219 x__4477 x__4478)))): X
abstract_term: (xs_4209 (i_4218 - 1) (fun (x__4477:bool) (x__4478:int) -> (k_make_list_4219 x__4477 x__4478))): X
abstract_term: (fun (x__4477:bool) (x__4478:int) -> (k_make_list_4219 x__4477 x__4478)): (
bool -> int -> X)
abst_arg: x__4477, bool
abst_arg: x__4478, int
abst_arg: x__4477, bool
abst_arg: x__4478, int
abstract_term: (k_make_list_4219 x__4477 x__4478): X
abstract_term: x__4478: int
abstract_term: x__4477: bool
abstract_term: (i_4218 - 1): int
abstract_term: (n_4205 - 1): int
ABST:
Main: main_3392
  main_3392 ->
      (make_list_1044
        (fun xs_4188 ->
         (insertsort_2064
           (fun k_main_xsxs_4406 ->
            (if rand_bool (l0 (xs_4188 k_main_xsxs_4406)) (l1 (xs_4188 (xs_4188 k_main_xsxs_4406)))))
           (fun ysys_4191 ->
            (check_2063 (fun x__4424 -> (ysys_4191 (fun x__4428 -> (x__4424 (if x__4428 true rand_bool)))))
              (fun b_4197 -> (if (if b_4197 true rand_bool) (l0 ()) (l1 (if b_4197 _|_ (fail_3437 ()))))))))))
  check_2063 xsxs_1051 k_check_2886 ->
      (xsxs_1051
        (fun xs22_4051 ->
         (if rand_bool
           (l1
             (if rand_bool
               (l1
                 (if (if xs22_4051 true rand_bool)
                   (l0
                     (check_2063
                       (fun k_check_xs'xs'_4361 ->
                        (xsxs_1051 (fun x__4433 -> (k_check_xs'xs'_4361 (if x__4433 true rand_bool)))))
                       (fun x__4429 -> (k_check_2886 (if x__4429 true rand_bool)))))
                   (l1 (if xs22_4051 _|_ (k_check_2886 false))))) (l0 (k_check_2886 true)))) (
           l0 (k_check_2886 true)))))
  fail_3437 k -> {fail} => k
  insert_1013 ysys_1015 k_insert_2134 ->
      (ysys_1015
        (fun p22_4251 ->
         (if rand_bool
           (l1
             (if rand_bool
               (l0
                 (k_insert_2134
                   (fun k_insert_4075 ->
                    (if rand_bool
                      (l0
                        (if rand_bool (l0 (k_insert_4075 true))
                          (l1
                            (ysys_1015
                              (fun xs22_4095 ->
                               (if rand_bool
                                 (l1
                                   (if rand_bool (l1 (k_insert_4075 rand_bool))
                                     (l0 (loop_3286 (k_insert_4075 rand_bool)))))
                                 (l0 (l0 (loop_3286 (k_insert_4075 rand_bool))))))))))
                      (l1
                        (if rand_bool (l0 (ysys_1015 (fun p22_4105 -> (k_insert_4075 true))))
                          (l1 (ysys_1015 (fun x__4445 -> (k_insert_4075 (if x__4445 true rand_bool)))))))))))
               (l1
                 (insert_1013
                   (fun k_insert_ys'ys'_4418 ->
                    (ysys_1015 (fun x__4441 -> (k_insert_ys'ys'_4418 (if x__4441 true rand_bool)))))
                   (fun ys''ys''_4111 ->
                    (k_insert_2134
                      (fun k_insert_4127 ->
                       (if rand_bool
                         (l0
                           (if rand_bool (l0 (k_insert_4127 true))
                             (l1 (ysys_1015 (fun p22_4137 -> (k_insert_4127 rand_bool))))))
                         (l1
                           (if rand_bool (l0 (ys''ys''_4111 (fun p22_4147 -> (k_insert_4127 rand_bool))))
                             (l1 (ys''ys''_4111 (fun x__4437 -> (k_insert_4127 (if x__4437 true rand_bool)))))))))))))))
           (l0
             (k_insert_2134
               (fun k_insert_rsrs_4345 ->
                (if rand_bool (l0 (if rand_bool (l0 (k_insert_rsrs_4345 true)) (l1 (k_insert_rsrs_4345 true))))
                  (l1
                    (if rand_bool (l0 (l1 (k_insert_rsrs_4345 true)))
                      (l1 (if rand_bool (l0 (k_insert_rsrs_4345 true)) (l1 (k_insert_rsrs_4345 true)))))))))))))
  insertsort_2064 xsxs_1037 k_insertsort_2604 ->
      (xsxs_1037
        (if rand_bool
          (l1
            (xsxs_1037
              (insertsort_2064 xsxs_1037
                (fun x_4185 ->
                 (insert_1013 (fun x__4464 -> (x_4185 (fun x__4468 -> (x__4464 (if x__4468 true rand_bool)))))
                   (fun x__4454 ->
                    (k_insertsort_2604
                      (fun x__4457 -> (x__4454 (fun x__4461 -> (x__4457 (if x__4461 true rand_bool))))))))))))
          (l0 (k_insertsort_2604 (fun k_insertsort_rsrs_4353 -> (k_insertsort_rsrs_4353 true))))))
  loop_3286 k_insert_loop_3288 -> (loop_3286 k_insert_loop_3288)
  make_list_1044 k_make_list_2791 -> (l0 (k_make_list_2791 (fun k_make_list_4202 -> k_make_list_4202)))
  make_list_1044 k_make_list_2791 ->
      (l1
        (make_list_1044
          (fun xs_4209 ->
           (k_make_list_2791
             (fun k_make_list_4219 -> (if rand_bool (l0 k_make_list_4219) (l1 (xs_4209 k_make_list_4219))))))))
Types:
  arg1_2059 : (unit -> unit)
  br_f_check_3433 : ((bool -> unit) -> ((bool -> unit) -> unit) -> unit)
  br_f_check_3435 : ((bool -> unit) -> ((bool -> unit) -> unit) -> unit)
  br_f_insert_3423 : (((bool -> unit) -> unit) -> unit -> unit)
  br_f_insert_3425 : (((bool -> unit) -> unit) -> unit -> unit)
  br_f_insert_3427 : ((unit -> unit) -> ((bool -> unit) -> unit) -> unit -> unit)
  br_f_insert_3429 : ((unit -> unit) -> ((bool -> unit) -> unit) -> unit -> unit)
  br_f_insert_3431 : ((((bool -> unit) -> unit) -> unit) -> ((bool -> unit) -> unit) -> unit)
  f_3422 : unit
  f_check_3414 : ((bool -> unit) -> ((bool -> unit) -> unit) -> unit)
  f_insert_3394 : ((((bool -> unit) -> unit) -> unit) -> ((bool -> unit) -> unit) -> unit)
  f_insert_3398 : (((bool -> unit) -> unit) -> unit -> unit)
  f_insert_3399 : (unit -> unit)
  f_insert_3400 : (unit -> unit)
  f_insert_3401 : ((((bool -> unit) -> unit) -> unit) -> ((bool -> unit) -> unit) -> (unit -> unit) -> unit)
  f_insert_3402 : ((unit -> unit) -> ((bool -> unit) -> unit) -> unit -> unit)
  f_insert_3403 : (unit -> unit)
  f_insert_3404 : (unit -> unit)
  f_insertsort_3405 : ((((bool -> unit) -> unit) -> unit) -> (unit -> unit) -> unit)
  f_insertsort_3406 : ((((bool -> unit) -> unit) -> unit) -> (unit -> unit) -> unit)
  f_insertsort_3408 : ((((bool -> unit) -> unit) -> unit) -> (unit -> unit) -> unit)
  f_insertsort_3409 : ((((bool -> unit) -> unit) -> unit) -> (unit -> unit) -> unit)
  f_main_3415 : (unit -> (unit -> unit) -> unit)
  f_main_3419 : (unit -> (unit -> unit) -> unit)
  f_main_3420 : (unit -> unit)
  f_main_3421 : (unit -> unit)
  f_make_list_3410 : (unit -> unit)
  f_make_list_3411 : (((unit -> unit) -> unit) -> unit)
  f_make_list_3412 : (((unit -> unit) -> unit) -> (unit -> unit) -> unit)
  f_make_list_3413 : ((unit -> unit) -> unit -> unit)
  f_rsrs_3395 : (unit -> unit)
  f_rsrs_3396 : (unit -> unit)
  f_rsrs_3397 : (unit -> unit)
  f_xs_3393 : (unit -> unit)
  f_xs_3407 : (unit -> unit)
  f_xsxs_3416 : (unit -> unit)
  f_xsxs_3417 : (unit -> (unit -> unit) -> unit)
  f_xsxs_3418 : (unit -> unit)
  is_none_2077 : (unit -> unit)
  k_insert_2299 : (unit -> unit)
  main_1059 : (unit -> unit)
  main_2061 : (unit -> unit)
  n_1047 : (unit -> unit)
  r_1023 : (unit -> unit)
  r_1065 : ((unit -> unit) -> unit -> unit)
  rs_1018 : ((bool -> unit) -> unit)
  rsrs_1020 : (unit -> unit)
  rsrs_3348 : (unit -> unit)
  xs'xs'_1056 : (((bool -> unit) -> unit) -> unit -> unit)
  xs'xs'_3332 : ((unit -> unit) -> unit -> unit)
  xs_1048 : (((unit -> unit) -> unit) -> unit)
  xs_1061 : (((unit -> unit) -> unit) -> unit)
  xs_2095 : (((bool -> unit) -> unit) -> unit -> unit)
  xs_2099 : (((bool -> unit) -> unit) -> unit -> unit)
  xs_2103 : (((bool -> unit) -> unit) -> unit -> unit)
  xs_3321 : ((unit -> unit) -> unit -> unit)
  xsxs_1062 : ((unit -> unit) -> unit -> unit)
  ys''ys''_1033 : (((bool -> unit) -> unit) -> ((unit -> unit) -> unit) -> unit)
  ys'ys'_1024 : (((bool -> unit) -> unit) -> unit -> unit)
  ysys_1066 : ((unit -> unit) -> ((unit -> unit) -> unit) -> unit)

LIFT:
Main: main_3392
  main_3392 -> (make_list_1044 f_4480)
  f_4480 xs_4188 -> (insertsort_2064 (f_4483 xs_4188) f_4485)
  f_4483 xs_4482 k_main_xsxs_4406 ->
      (if rand_bool (l0 (xs_4482 k_main_xsxs_4406)) (l1 (xs_4482 (xs_4482 k_main_xsxs_4406))))
  f_4485 ysys_4191 -> (check_2063 (f_4488 ysys_4191) f_4493)
  f_4491 x__4490 x__4428 -> (x__4490 (if x__4428 true rand_bool))
  f_4488 ysys_4487 x__4424 -> (ysys_4487 (f_4491 x__4424))
  f_4493 b_4197 -> (if (if b_4197 true rand_bool) (l0 ()) (l1 (if b_4197 _|_ (fail_3437 ()))))
  check_2063 xsxs_1051 k_check_2886 -> (xsxs_1051 (f_4497 xsxs_1051 k_check_2886))
  f_4497 xsxs_4495 k_check_4496 xs22_4051 ->
      (if rand_bool
        (l1
          (if rand_bool
            (l1
              (if (if xs22_4051 true rand_bool) (l0 (check_2063 (f_4500 xsxs_4495) (f_4506 k_check_4496)))
                (l1 (if xs22_4051 _|_ (k_check_4496 false))))) (l0 (k_check_4496 true)))) (
        l0 (k_check_4496 true)))
  f_4506 k_check_4505 x__4429 -> (k_check_4505 (if x__4429 true rand_bool))
  f_4500 xsxs_4499 k_check_xs'xs'_4361 -> (xsxs_4499 (f_4503 k_check_xs'xs'_4361))
  f_4503 k_check_xs'xs'_4502 x__4433 -> (k_check_xs'xs'_4502 (if x__4433 true rand_bool))
  fail_3437 k -> {fail} => k
  insert_1013 ysys_1015 k_insert_2134 -> (ysys_1015 (f_4510 ysys_1015 k_insert_2134))
  f_4510 ysys_4508 k_insert_4509 p22_4251 ->
      (if rand_bool
        (l1
          (if rand_bool (l0 (k_insert_4509 (f_4513 ysys_4508)))
            (l1 (insert_1013 (f_4525 ysys_4508) (f_4532 ysys_4508 k_insert_4509))))) (
        l0 (k_insert_4509 f_4547)))
  f_4545 k_insert_4544 x__4437 -> (k_insert_4544 (if x__4437 true rand_bool))
  f_4542 k_insert_4541 p22_4147 -> (k_insert_4541 rand_bool)
  f_4539 k_insert_4538 p22_4137 -> (k_insert_4538 rand_bool)
  f_4536 ysys_4534 ys''ys''_4535 k_insert_4127 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_insert_4127 true)) (l1 (ysys_4534 (f_4539 k_insert_4127)))))
        (l1 (if rand_bool (l0 (ys''ys''_4535 (f_4542 k_insert_4127))) (l1 (ys''ys''_4535 (f_4545 k_insert_4127))))))
  f_4532 ysys_4530 k_insert_4531 ys''ys''_4111 -> (k_insert_4531 (f_4536 ysys_4530 ys''ys''_4111))
  f_4525 ysys_4524 k_insert_ys'ys'_4418 -> (ysys_4524 (f_4528 k_insert_ys'ys'_4418))
  f_4528 k_insert_ys'ys'_4527 x__4441 -> (k_insert_ys'ys'_4527 (if x__4441 true rand_bool))
  f_4513 ysys_4512 k_insert_4075 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_insert_4075 true)) (l1 (ysys_4512 (f_4516 k_insert_4075)))))
        (l1 (if rand_bool (l0 (ysys_4512 (f_4519 k_insert_4075))) (l1 (ysys_4512 (f_4522 k_insert_4075))))))
  f_4516 k_insert_4515 xs22_4095 ->
      (if rand_bool (l1 (if rand_bool (l1 (k_insert_4515 rand_bool)) (l0 (loop_3286 (k_insert_4515 rand_bool)))))
        (l0 (l0 (loop_3286 (k_insert_4515 rand_bool)))))
  f_4519 k_insert_4518 p22_4105 -> (k_insert_4518 true)
  f_4522 k_insert_4521 x__4445 -> (k_insert_4521 (if x__4445 true rand_bool))
  f_4547 k_insert_rsrs_4345 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_insert_rsrs_4345 true)) (l1 (k_insert_rsrs_4345 true))))
        (l1
          (if rand_bool (l0 (l1 (k_insert_rsrs_4345 true)))
            (l1 (if rand_bool (l0 (k_insert_rsrs_4345 true)) (l1 (k_insert_rsrs_4345 true)))))))
  insertsort_2064 xsxs_1037 k_insertsort_2604 ->
      (xsxs_1037
        (if rand_bool (l1 (xsxs_1037 (insertsort_2064 xsxs_1037 (f_4550 k_insertsort_2604))))
          (l0 (k_insertsort_2604 f_4567))))
  f_4565 x__4564 x__4461 -> (x__4564 (if x__4461 true rand_bool))
  f_4562 x__4561 x__4457 -> (x__4561 (f_4565 x__4457))
  f_4559 k_insertsort_4558 x__4454 -> (k_insertsort_4558 (f_4562 x__4454))
  f_4553 x_4552 x__4464 -> (x_4552 (f_4556 x__4464))
  f_4556 x__4555 x__4468 -> (x__4555 (if x__4468 true rand_bool))
  f_4550 k_insertsort_4549 x_4185 -> (insert_1013 (f_4553 x_4185) (f_4559 k_insertsort_4549))
  f_4567 k_insertsort_rsrs_4353 -> (k_insertsort_rsrs_4353 true)
  loop_3286 k_insert_loop_3288 -> (loop_3286 k_insert_loop_3288)
  make_list_1044 k_make_list_2791 -> (l0 (k_make_list_2791 f_4569))
  f_4569 k_make_list_4202 -> k_make_list_4202
  make_list_1044 k_make_list_2791 -> (l1 (make_list_1044 (f_4572 k_make_list_2791)))
  f_4572 k_make_list_4571 xs_4209 -> (k_make_list_4571 (f_4575 xs_4209))
  f_4575 xs_4574 k_make_list_4219 -> (if rand_bool (l0 k_make_list_4219) (l1 (xs_4574 k_make_list_4219)))

TRANS_EAGER:
Main: main_3392
  main_3392 -> (make_list_1044 f_4480)
  f_4480 xs_4188 -> (insertsort_2064 (f_4483 xs_4188) f_4485)
  f_4483 xs_4482 k_main_xsxs_4406 ->
      (let f_4577 b_4576 = (if b_4576 (l0 (xs_4482 k_main_xsxs_4406)) (l1 (xs_4482 (xs_4482 k_main_xsxs_4406)))) in
       (if rand_bool (f_4577 true) (f_4577 false)))
  f_4485 ysys_4191 -> (check_2063 (f_4488 ysys_4191) f_4493)
  f_4491 x__4490 x__4428 ->
      (let f_4581 b_4580 = (if b_4580 (x__4490 true) (if rand_bool (x__4490 true) (x__4490 false))) in (f_4581 x__4428))
  f_4488 ysys_4487 x__4424 -> (ysys_4487 (f_4491 x__4424))
  f_4493 b_4197 ->
      (let f_4583 b_4582 =
       (if b_4582 (l0 ()) (l1 (let f_4585 b_4584 = (if b_4584 _|_ (fail_3437 ())) in (f_4585 b_4197)))) in
       (let f_4587 b_4586 = (if b_4586 (f_4583 true) (if rand_bool (f_4583 true) (f_4583 false))) in (f_4587 b_4197)))
  check_2063 xsxs_1051 k_check_2886 -> (xsxs_1051 (f_4497 xsxs_1051 k_check_2886))
  f_4497 xsxs_4495 k_check_4496 xs22_4051 ->
      (let f_4589 b_4588 =
       (if b_4588
         (l1
           (let f_4591 b_4590 =
            (if b_4590
              (l1
                (let f_4593 b_4592 =
                 (if b_4592 (l0 (check_2063 (f_4500 xsxs_4495) (f_4506 k_check_4496)))
                   (l1 (let f_4595 b_4594 = (if b_4594 _|_ (k_check_4496 false)) in (f_4595 xs22_4051))))
                 in
                 (let f_4597 b_4596 = (if b_4596 (f_4593 true) (if rand_bool (f_4593 true) (f_4593 false))) in
                  (f_4597 xs22_4051)))) (l0 (k_check_4496 true)))
            in (if rand_bool (f_4591 true) (f_4591 false)))) (l0 (k_check_4496 true)))
       in (if rand_bool (f_4589 true) (f_4589 false)))
  f_4506 k_check_4505 x__4429 ->
      (let f_4601 b_4600 = (if b_4600 (k_check_4505 true) (if rand_bool (k_check_4505 true) (k_check_4505 false))) in
       (f_4601 x__4429))
  f_4500 xsxs_4499 k_check_xs'xs'_4361 -> (xsxs_4499 (f_4503 k_check_xs'xs'_4361))
  f_4503 k_check_xs'xs'_4502 x__4433 ->
      (let f_4605 b_4604 =
       (if b_4604 (k_check_xs'xs'_4502 true) (if rand_bool (k_check_xs'xs'_4502 true) (k_check_xs'xs'_4502 false))) in
       (f_4605 x__4433))
  fail_3437 k -> {fail} => k
  insert_1013 ysys_1015 k_insert_2134 -> (ysys_1015 (f_4510 ysys_1015 k_insert_2134))
  f_4510 ysys_4508 k_insert_4509 p22_4251 ->
      (let f_4607 b_4606 =
       (if b_4606
         (l1
           (let f_4609 b_4608 =
            (if b_4608 (l0 (k_insert_4509 (f_4513 ysys_4508)))
              (l1 (insert_1013 (f_4525 ysys_4508) (f_4532 ysys_4508 k_insert_4509))))
            in (if rand_bool (f_4609 true) (f_4609 false)))) (l0 (k_insert_4509 f_4547)))
       in (if rand_bool (f_4607 true) (f_4607 false)))
  f_4545 k_insert_4544 x__4437 ->
      (let f_4613 b_4612 = (if b_4612 (k_insert_4544 true) (if rand_bool (k_insert_4544 true) (k_insert_4544 false)))
       in (f_4613 x__4437))
  f_4542 k_insert_4541 p22_4147 -> (if rand_bool (k_insert_4541 true) (k_insert_4541 false))
  f_4539 k_insert_4538 p22_4137 -> (if rand_bool (k_insert_4538 true) (k_insert_4538 false))
  f_4536 ysys_4534 ys''ys''_4535 k_insert_4127 ->
      (let f_4619 b_4618 =
       (if b_4618
         (l0
           (let f_4621 b_4620 = (if b_4620 (l0 (k_insert_4127 true)) (l1 (ysys_4534 (f_4539 k_insert_4127)))) in
            (if rand_bool (f_4621 true) (f_4621 false))))
         (l1
           (let f_4623 b_4622 =
            (if b_4622 (l0 (ys''ys''_4535 (f_4542 k_insert_4127))) (l1 (ys''ys''_4535 (f_4545 k_insert_4127)))) in
            (if rand_bool (f_4623 true) (f_4623 false)))))
       in (if rand_bool (f_4619 true) (f_4619 false)))
  f_4532 ysys_4530 k_insert_4531 ys''ys''_4111 -> (k_insert_4531 (f_4536 ysys_4530 ys''ys''_4111))
  f_4525 ysys_4524 k_insert_ys'ys'_4418 -> (ysys_4524 (f_4528 k_insert_ys'ys'_4418))
  f_4528 k_insert_ys'ys'_4527 x__4441 ->
      (let f_4627 b_4626 =
       (if b_4626 (k_insert_ys'ys'_4527 true) (if rand_bool (k_insert_ys'ys'_4527 true) (k_insert_ys'ys'_4527 false)))
       in (f_4627 x__4441))
  f_4513 ysys_4512 k_insert_4075 ->
      (let f_4629 b_4628 =
       (if b_4628
         (l0
           (let f_4631 b_4630 = (if b_4630 (l0 (k_insert_4075 true)) (l1 (ysys_4512 (f_4516 k_insert_4075)))) in
            (if rand_bool (f_4631 true) (f_4631 false))))
         (l1
           (let f_4633 b_4632 =
            (if b_4632 (l0 (ysys_4512 (f_4519 k_insert_4075))) (l1 (ysys_4512 (f_4522 k_insert_4075)))) in
            (if rand_bool (f_4633 true) (f_4633 false)))))
       in (if rand_bool (f_4629 true) (f_4629 false)))
  f_4516 k_insert_4515 xs22_4095 ->
      (let f_4635 b_4634 =
       (if b_4634
         (l1
           (let f_4637 b_4636 =
            (if b_4636 (l1 (if rand_bool (k_insert_4515 true) (k_insert_4515 false)))
              (l0 (loop_3286 (if rand_bool (k_insert_4515 true) (k_insert_4515 false)))))
            in (if rand_bool (f_4637 true) (f_4637 false))))
         (l0 (l0 (loop_3286 (if rand_bool (k_insert_4515 true) (k_insert_4515 false))))))
       in (if rand_bool (f_4635 true) (f_4635 false)))
  f_4519 k_insert_4518 p22_4105 -> (k_insert_4518 true)
  f_4522 k_insert_4521 x__4445 ->
      (let f_4647 b_4646 = (if b_4646 (k_insert_4521 true) (if rand_bool (k_insert_4521 true) (k_insert_4521 false)))
       in (f_4647 x__4445))
  f_4547 k_insert_rsrs_4345 ->
      (let f_4649 b_4648 =
       (if b_4648
         (l0
           (let f_4651 b_4650 = (if b_4650 (l0 (k_insert_rsrs_4345 true)) (l1 (k_insert_rsrs_4345 true))) in
            (if rand_bool (f_4651 true) (f_4651 false))))
         (l1
           (let f_4653 b_4652 =
            (if b_4652 (l0 (l1 (k_insert_rsrs_4345 true)))
              (l1
                (let f_4655 b_4654 = (if b_4654 (l0 (k_insert_rsrs_4345 true)) (l1 (k_insert_rsrs_4345 true))) in
                 (if rand_bool (f_4655 true) (f_4655 false)))))
            in (if rand_bool (f_4653 true) (f_4653 false)))))
       in (if rand_bool (f_4649 true) (f_4649 false)))
  insertsort_2064 xsxs_1037 k_insertsort_2604 ->
      (xsxs_1037
        (let f_4657 b_4656 =
         (if b_4656 (l1 (xsxs_1037 (insertsort_2064 xsxs_1037 (f_4550 k_insertsort_2604))))
           (l0 (k_insertsort_2604 f_4567)))
         in (if rand_bool (f_4657 true) (f_4657 false))))
  f_4565 x__4564 x__4461 ->
      (let f_4661 b_4660 = (if b_4660 (x__4564 true) (if rand_bool (x__4564 true) (x__4564 false))) in (f_4661 x__4461))
  f_4562 x__4561 x__4457 -> (x__4561 (f_4565 x__4457))
  f_4559 k_insertsort_4558 x__4454 -> (k_insertsort_4558 (f_4562 x__4454))
  f_4553 x_4552 x__4464 -> (x_4552 (f_4556 x__4464))
  f_4556 x__4555 x__4468 ->
      (let f_4665 b_4664 = (if b_4664 (x__4555 true) (if rand_bool (x__4555 true) (x__4555 false))) in (f_4665 x__4468))
  f_4550 k_insertsort_4549 x_4185 -> (insert_1013 (f_4553 x_4185) (f_4559 k_insertsort_4549))
  f_4567 k_insertsort_rsrs_4353 -> (k_insertsort_rsrs_4353 true)
  loop_3286 k_insert_loop_3288 -> (loop_3286 k_insert_loop_3288)
  make_list_1044 k_make_list_2791 -> (l0 (k_make_list_2791 f_4569))
  f_4569 k_make_list_4202 -> k_make_list_4202
  make_list_1044 k_make_list_2791 -> (l1 (make_list_1044 (f_4572 k_make_list_2791)))
  f_4572 k_make_list_4571 xs_4209 -> (k_make_list_4571 (f_4575 xs_4209))
  f_4575 xs_4574 k_make_list_4219 ->
      (let f_4667 b_4666 = (if b_4666 (l0 k_make_list_4219) (l1 (xs_4574 k_make_list_4219))) in
       (if rand_bool (f_4667 true) (f_4667 false)))

PUT_INTO_IF:
Main: main_3392
  main_3392 -> (make_list_1044 f_4480)
  f_4480 xs_4188 -> (insertsort_2064 (f_4483 xs_4188) f_4485)
  f_4483 xs_4482 k_main_xsxs_4406 ->
      (let f_4577 b_4576 = (if b_4576 (l0 (xs_4482 k_main_xsxs_4406)) (l1 (xs_4482 (xs_4482 k_main_xsxs_4406)))) in
       (if rand_bool (f_4577 true) (f_4577 false)))
  f_4485 ysys_4191 -> (check_2063 (f_4488 ysys_4191) f_4493)
  f_4491 x__4490 x__4428 ->
      (let f_4581 b_4580 = (if b_4580 (x__4490 true) (if rand_bool (x__4490 true) (x__4490 false))) in (f_4581 x__4428))
  f_4488 ysys_4487 x__4424 -> (ysys_4487 (f_4491 x__4424))
  f_4493 b_4197 ->
      (let f_4583 b_4582 =
       (if b_4582 (l0 ()) (l1 (let f_4585 b_4584 = (if b_4584 _|_ (fail_3437 ())) in (f_4585 b_4197)))) in
       (let f_4587 b_4586 = (if b_4586 (f_4583 true) (if rand_bool (f_4583 true) (f_4583 false))) in (f_4587 b_4197)))
  check_2063 xsxs_1051 k_check_2886 -> (xsxs_1051 (f_4497 xsxs_1051 k_check_2886))
  f_4497 xsxs_4495 k_check_4496 xs22_4051 ->
      (let f_4589 b_4588 =
       (if b_4588
         (l1
           (let f_4591 b_4590 =
            (if b_4590
              (l1
                (let f_4593 b_4592 =
                 (if b_4592 (l0 (check_2063 (f_4500 xsxs_4495) (f_4506 k_check_4496)))
                   (l1 (let f_4595 b_4594 = (if b_4594 _|_ (k_check_4496 false)) in (f_4595 xs22_4051))))
                 in
                 (let f_4597 b_4596 = (if b_4596 (f_4593 true) (if rand_bool (f_4593 true) (f_4593 false))) in
                  (f_4597 xs22_4051)))) (l0 (k_check_4496 true)))
            in (if rand_bool (f_4591 true) (f_4591 false)))) (l0 (k_check_4496 true)))
       in (if rand_bool (f_4589 true) (f_4589 false)))
  f_4506 k_check_4505 x__4429 ->
      (let f_4601 b_4600 = (if b_4600 (k_check_4505 true) (if rand_bool (k_check_4505 true) (k_check_4505 false))) in
       (f_4601 x__4429))
  f_4500 xsxs_4499 k_check_xs'xs'_4361 -> (xsxs_4499 (f_4503 k_check_xs'xs'_4361))
  f_4503 k_check_xs'xs'_4502 x__4433 ->
      (let f_4605 b_4604 =
       (if b_4604 (k_check_xs'xs'_4502 true) (if rand_bool (k_check_xs'xs'_4502 true) (k_check_xs'xs'_4502 false))) in
       (f_4605 x__4433))
  fail_3437 k -> {fail} => k
  insert_1013 ysys_1015 k_insert_2134 -> (ysys_1015 (f_4510 ysys_1015 k_insert_2134))
  f_4510 ysys_4508 k_insert_4509 p22_4251 ->
      (let f_4607 b_4606 =
       (if b_4606
         (l1
           (let f_4609 b_4608 =
            (if b_4608 (l0 (k_insert_4509 (f_4513 ysys_4508)))
              (l1 (insert_1013 (f_4525 ysys_4508) (f_4532 ysys_4508 k_insert_4509))))
            in (if rand_bool (f_4609 true) (f_4609 false)))) (l0 (k_insert_4509 f_4547)))
       in (if rand_bool (f_4607 true) (f_4607 false)))
  f_4545 k_insert_4544 x__4437 ->
      (let f_4613 b_4612 = (if b_4612 (k_insert_4544 true) (if rand_bool (k_insert_4544 true) (k_insert_4544 false)))
       in (f_4613 x__4437))
  f_4542 k_insert_4541 p22_4147 -> (if rand_bool (k_insert_4541 true) (k_insert_4541 false))
  f_4539 k_insert_4538 p22_4137 -> (if rand_bool (k_insert_4538 true) (k_insert_4538 false))
  f_4536 ysys_4534 ys''ys''_4535 k_insert_4127 ->
      (let f_4619 b_4618 =
       (if b_4618
         (l0
           (let f_4621 b_4620 = (if b_4620 (l0 (k_insert_4127 true)) (l1 (ysys_4534 (f_4539 k_insert_4127)))) in
            (if rand_bool (f_4621 true) (f_4621 false))))
         (l1
           (let f_4623 b_4622 =
            (if b_4622 (l0 (ys''ys''_4535 (f_4542 k_insert_4127))) (l1 (ys''ys''_4535 (f_4545 k_insert_4127)))) in
            (if rand_bool (f_4623 true) (f_4623 false)))))
       in (if rand_bool (f_4619 true) (f_4619 false)))
  f_4532 ysys_4530 k_insert_4531 ys''ys''_4111 -> (k_insert_4531 (f_4536 ysys_4530 ys''ys''_4111))
  f_4525 ysys_4524 k_insert_ys'ys'_4418 -> (ysys_4524 (f_4528 k_insert_ys'ys'_4418))
  f_4528 k_insert_ys'ys'_4527 x__4441 ->
      (let f_4627 b_4626 =
       (if b_4626 (k_insert_ys'ys'_4527 true) (if rand_bool (k_insert_ys'ys'_4527 true) (k_insert_ys'ys'_4527 false)))
       in (f_4627 x__4441))
  f_4513 ysys_4512 k_insert_4075 ->
      (let f_4629 b_4628 =
       (if b_4628
         (l0
           (let f_4631 b_4630 = (if b_4630 (l0 (k_insert_4075 true)) (l1 (ysys_4512 (f_4516 k_insert_4075)))) in
            (if rand_bool (f_4631 true) (f_4631 false))))
         (l1
           (let f_4633 b_4632 =
            (if b_4632 (l0 (ysys_4512 (f_4519 k_insert_4075))) (l1 (ysys_4512 (f_4522 k_insert_4075)))) in
            (if rand_bool (f_4633 true) (f_4633 false)))))
       in (if rand_bool (f_4629 true) (f_4629 false)))
  f_4516 k_insert_4515 xs22_4095 ->
      (let f_4635 b_4634 =
       (if b_4634
         (l1
           (let f_4637 b_4636 =
            (if b_4636 (l1 (if rand_bool (k_insert_4515 true) (k_insert_4515 false)))
              (l0 (loop_3286 (if rand_bool (k_insert_4515 true) (k_insert_4515 false)))))
            in (if rand_bool (f_4637 true) (f_4637 false))))
         (l0 (l0 (loop_3286 (if rand_bool (k_insert_4515 true) (k_insert_4515 false))))))
       in (if rand_bool (f_4635 true) (f_4635 false)))
  f_4519 k_insert_4518 p22_4105 -> (k_insert_4518 true)
  f_4522 k_insert_4521 x__4445 ->
      (let f_4647 b_4646 = (if b_4646 (k_insert_4521 true) (if rand_bool (k_insert_4521 true) (k_insert_4521 false)))
       in (f_4647 x__4445))
  f_4547 k_insert_rsrs_4345 ->
      (let f_4649 b_4648 =
       (if b_4648
         (l0
           (let f_4651 b_4650 = (if b_4650 (l0 (k_insert_rsrs_4345 true)) (l1 (k_insert_rsrs_4345 true))) in
            (if rand_bool (f_4651 true) (f_4651 false))))
         (l1
           (let f_4653 b_4652 =
            (if b_4652 (l0 (l1 (k_insert_rsrs_4345 true)))
              (l1
                (let f_4655 b_4654 = (if b_4654 (l0 (k_insert_rsrs_4345 true)) (l1 (k_insert_rsrs_4345 true))) in
                 (if rand_bool (f_4655 true) (f_4655 false)))))
            in (if rand_bool (f_4653 true) (f_4653 false)))))
       in (if rand_bool (f_4649 true) (f_4649 false)))
  insertsort_2064 xsxs_1037 k_insertsort_2604 ->
      (xsxs_1037
        (let f_4657 b_4656 =
         (if b_4656 (l1 (xsxs_1037 (insertsort_2064 xsxs_1037 (f_4550 k_insertsort_2604))))
           (l0 (k_insertsort_2604 f_4567)))
         in (if rand_bool (f_4657 true) (f_4657 false))))
  f_4565 x__4564 x__4461 ->
      (let f_4661 b_4660 = (if b_4660 (x__4564 true) (if rand_bool (x__4564 true) (x__4564 false))) in (f_4661 x__4461))
  f_4562 x__4561 x__4457 -> (x__4561 (f_4565 x__4457))
  f_4559 k_insertsort_4558 x__4454 -> (k_insertsort_4558 (f_4562 x__4454))
  f_4553 x_4552 x__4464 -> (x_4552 (f_4556 x__4464))
  f_4556 x__4555 x__4468 ->
      (let f_4665 b_4664 = (if b_4664 (x__4555 true) (if rand_bool (x__4555 true) (x__4555 false))) in (f_4665 x__4468))
  f_4550 k_insertsort_4549 x_4185 -> (insert_1013 (f_4553 x_4185) (f_4559 k_insertsort_4549))
  f_4567 k_insertsort_rsrs_4353 -> (k_insertsort_rsrs_4353 true)
  loop_3286 k_insert_loop_3288 -> (loop_3286 k_insert_loop_3288)
  make_list_1044 k_make_list_2791 -> (l0 (k_make_list_2791 f_4569))
  f_4569 k_make_list_4202 -> k_make_list_4202
  make_list_1044 k_make_list_2791 -> (l1 (make_list_1044 (f_4572 k_make_list_2791)))
  f_4572 k_make_list_4571 xs_4209 -> (k_make_list_4571 (f_4575 xs_4209))
  f_4575 xs_4574 k_make_list_4219 ->
      (let f_4667 b_4666 = (if b_4666 (l0 k_make_list_4219) (l1 (xs_4574 k_make_list_4219))) in
       (if rand_bool (f_4667 true) (f_4667 false)))

DONE!

(0-2) Checking HORS ... DONE!

Error trace::
  main_3392 ... -->
  main_2061 ... -->
  arg1_2059 ... -->
  f_main_3421 ... -->
  main_1059 ... -->
  xs_1061 ... -->
  make_list_1044 [2/2] ... -->
  n_1047 ... -->
  f_make_list_3411 ... -->
  xs_1048 ... -->
  make_list_1044 [1/2] ... -->
  f_make_list_3412 ... -->
  f_main_3415 ... -->
  ysys_1066 ... -->
  insertsort_2064 ... -->
  xsxs_1062 [1/2] ... -->
  r_1065 ... -->
  f_make_list_3413 [1/2] ... -->
  f_xsxs_3416 ... -->
  f_insertsort_3405 ... -->
  is_none_2077 ... -->
  f_insertsort_3406 [2/2] ... -->
  xs_3321 ... -->
  xsxs_1062 [1/2] ... -->
  r_1065 ... -->
  f_make_list_3413 [1/2] ... -->
  f_xsxs_3416 ... -->
  f_xs_3407 ... -->
  f_insertsort_3408 ... -->
  insertsort_2064 ... -->
  xs'xs'_3332 ... -->
  xsxs_1062 [1/2] ... -->
  r_1065 ... -->
  f_make_list_3413 [1/2] ... -->
  f_xsxs_3416 ... -->
  f_insertsort_3405 ... -->
  is_none_2077 ... -->
  f_insertsort_3406 [1/2] ... -->
  f_insertsort_3409 ... -->
  insert_1013 ... -->
  xs_2095 ... -->
  rsrs_3348 ... -->
  f_xs_3393 ... -->
  f_insert_3394 [2/2] ... -->
  br_f_insert_3431 [1/2] ... -->
  f_main_3419 ... -->
  check_2063 ... -->
  xs_2103 ... -->
  f_insert_3398 [1/2] ... -->
  br_f_insert_3423 [2/2] ... -->
  xs_2099 ... -->
  rsrs_3348 ... -->
  f_insert_3399 [2/2] ... -->
  k_insert_2299 [2/2] ... -->
  f_check_3414 [2/2] ... -->
  br_f_check_3435 [2/2] ... -->
  br_f_check_3433 [2/2] ... -->
  f_main_3420 [2/2] ... -->
  fail_3437 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 1; 0; 0; 1; 1; 1; 1; 1; 1; 0

(0-3) Checking counterexample ... DONE!

(0-4) Discovering predicates ...
