MoCHi: Model Checker for Higher-Order Programs
  Build: c81d999 (2014-06-27 16:26:06 +0900)
  TRecS version: 1.33
  OCaml version: 4.01.0
  Command: ./mochi.opt test/search.ml -debug 2

parsed:
 let rec exists_1011 test_1012 f_1013 n_1014 m_1015 =
   if n_1014 < m_1015 then
     if test_1012 (f_1013 n_1014) then
       Ref n_1014
     else
       exists_1011 test_1012 f_1013 (n_1014 + 1) m_1015
   else
     None
 in
 let mult3_1016 n_1017 = 3 * n_1017 in
 let main_1018 n_1019 m_1020 =
   let test_1021 x_1022 = x_1022 = m_1020 in
   (match exists_1011 test_1021 mult3_1016 0 n_1019 with
    | None -> ()
    | (Some x_1023) -> if 0 <= x_1023 && x_1023 < n_1019 then
                         ()
                       else
                         {fail} ())
 in
 ()

set_target:
 let rec exists_1011 (test_1012:(!!! -> bool)) (f_1013:(int -> !!!)) (n_1014:int) (m_1015:int) =
   if n_1014 < m_1015 then
     if test_1012 (f_1013 n_1014) then
       Ref n_1014
     else
       exists_1011 test_1012 f_1013 (n_1014 + 1) m_1015
   else
     None
 in
 let mult3_1016 (n_1017:int) = 3 * n_1017 in
 let main_1018 (n_1019:int) (m_1020:int) =
   let test_1021 (x_1022:int) = x_1022 = m_1020 in
   (match exists_1011 test_1021 mult3_1016 0 n_1019 with
    | None -> ()
    | (Some x_1023) -> if 0 <= x_1023 && x_1023 < n_1019 then
                         ()
                       else
                         {fail} ())
 in
 let main_1092 = let arg1_1088 = rand_int () in
                 let arg2_1090 = rand_int () in
                 main_1018 arg1_1088 arg2_1090 in
 ()

check': (Some ((n_1014:int):int):option), option
Fatal error: exception Assert_failure("type_check.ml", 144, 95)
