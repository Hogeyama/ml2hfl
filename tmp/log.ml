MoCHi: Model Checker for Higher-Order Programs
  Build: _089eb4b (after 2014-08-06 14:23:29 +0900)
  FPAT version: 3fa26b7
  TRecS version: 1.33
  OCaml version: 4.01.0
  Command: ./mochi.opt test_relabs/append_nil2_nth.ml -use-spec -disable-rc -color -list-option -fpat -hccs 1 
           -bool-init-empty -abs-filter -tupling -debug-module Tupling,Ref_trans,Ret_fun

parsed:
 let rec make_list_1008 n_1009 = if n_1009 < 0 then
                                   []
                                 else
                                   rand_int ()::make_list_1008 (n_1009 - 1) in
 let rec append_1010 xs__ys_1023 =
   match xs__ys_1023 with
   | (xs_1011, ys_1012) ->
       (match xs_1011 with
        | [] -> ys_1012
        | x_1013::xs'_1014 ->
            let rs_1015 = append_1010 (xs'_1014, ys_1012) in
            let rs'_1016 = x_1013::rs_1015 in
            rs'_1016)
 in
 let main_1017 i_1018 n_1019 =
   let xs_1020 = make_list_1008 n_1019 in
   let ys_1021 = append_1010 (xs_1020, []) in
   if List.nth ys_1021 i_1018 = List.nth xs_1020 i_1018 then
     ()
   else
     {fail} ()
 in
 ()

spec (abstraction type environment for CEGAR-loop):
 append: ((bool ->
             int ->
               b1_1055:bool ->
                 int ->
                   (bool ->
                      bool ->
                        int ->
                          b2_1061:bool[\b2_1060. not b1_1055 || b2_1060] ->
                            b3_1062:bool -> int[\x_1063. not b2_1061 || not b3_1062 && x_1063 = 0] -> X) -> X) ->
            ((b1_1078:bool ->
                i_1079:int ->
                  b2_1080:bool ->
                    j_1081:int ->
                      b3_1082:bool ->
                        int ->
                          (b41_1085:bool[\b41_1084. not b1_1078 || b41_1084] ->
                             bool ->
                               x_1087:int ->
                                 b51_1089:bool[\b51_1088. not b2_1080 || b51_1088] ->
                                   bool ->
                                     int[\y_1091. not ((b41_1085 && b51_1089) && i_1079 = j_1081) || x_1087 = y_1091]
                                       ->
                                       b61_1094:bool[\b61_1093. not b3_1082 || b61_1093] ->
                                         b62_1095:bool -> int[\z_1096. not b61_1094 || not b62_1095 && z_1096 = 0] -> X)
                            -> X) -> X) -> X)
 ys: ((bool ->
         int ->
           b2_1123:bool ->
             int ->
               (bool ->
                  bool ->
                    int ->
                      b41_1129:bool[\b41_1128. not b2_1123 || b41_1128] ->
                        b42_1130:bool -> int[\x_1131. not b41_1129 || not b42_1130 && x_1131 = 0] -> X) -> X) ->
        int -> (b3_1147:bool -> int[\x_1148. not b3_1147 && x_1148 = 0] -> X) -> X)
 
set_target:
 let rec make_list_1008 (n_1009:int) = if n_1009 < 0 then
                                         []
                                       else
                                         rand_int ()::make_list_1008 (n_1009 - 1) in
 let rec append_1010 (xs__ys_1023:(!!! list * !!! list)) =
   match xs__ys_1023 with
   | (xs_1011, ys_1012) ->
       (match xs_1011 with
        | [] -> ys_1012
        | x_1013::xs'_1014 ->
            let rs_1015 = append_1010 (xs'_1014, ys_1012) in
            let rs'_1016 = x_1013::rs_1015 in
            rs'_1016)
 in
 let main_1017 (i_1018:int) (n_1019:int) =
   let xs_1020 = make_list_1008 n_1019 in
   let ys_1021 = append_1010 (xs_1020, []) in
   if List.nth ys_1021 i_1018 = List.nth xs_1020 i_1018 then
     ()
   else
     {fail} ()
 in
 let main_1161 = let arg1_1157 = rand_int () in
                 let arg2_1159 = rand_int () in
                 main_1017 arg1_1157 arg2_1159 in
 ()

ASSERT: List.nth (append_1010 (make_list_1008 n_1019, [])) i_1018 = List.nth (make_list_1008 n_1019) i_1018
FUN: List.nth
FUN: List.nth
FUN': List.nth
FUN': List.nth
ALL_FUN_ARG: List.nth, List.nth
FUN_ARG: List.nth, make_list_1008
FUN_ARG: List.nth, append_1010
FUN_ARG': List.nth, List.nth


let rec make_list_1008 n_1009 = if n_1009 < 0 then
                                  []
                                else
                                  rand_int ()::make_list_1008 (n_1009 - 1) in
let rec append_1010 xs__ys_1023 =
  match xs__ys_1023 with
  | (xs_1011, ys_1012) ->
      (match xs_1011 with
       | [] -> ys_1012
       | x_1013::xs'_1014 ->
           let rs_1015 = append_1010 (xs'_1014, ys_1012) in
           let rs'_1016 = x_1013::rs_1015 in
           rs'_1016)
in
let main_1017 i_1018 n_1019 =
  let xs_1020 = make_list_1008 n_1019 in
  let ys_1021 = append_1010 (xs_1020, []) in
  if List.nth ys_1021 i_1018 = List.nth xs_1020 i_1018 then
    ()
  else
    {fail} ()
in
let main_1161 = let arg1_1157 = rand_int () in
                let arg2_1159 = rand_int () in
                main_1017 arg1_1157 arg2_1159 in
()
make_ext_funs:
 let List.nth_1162 (x_1163:int list) (x_1164:int) = rand_int () in
 let rec make_list_1008 (n_1009:int) = if n_1009 < 0 then
                                         []
                                       else
                                         rand_int ()::make_list_1008 (n_1009 - 1) in
 let rec append_1010 (xs__ys_1023:(!!! list * !!! list)) =
   match xs__ys_1023 with
   | (xs_1011, ys_1012) ->
       (match xs_1011 with
        | [] -> ys_1012
        | x_1013::xs'_1014 ->
            let rs_1015 = append_1010 (xs'_1014, ys_1012) in
            let rs'_1016 = x_1013::rs_1015 in
            rs'_1016)
 in
 let main_1017 (i_1018:int) (n_1019:int) =
   let xs_1020 = make_list_1008 n_1019 in
   let ys_1021 = append_1010 (xs_1020, []) in
   if List.nth_1162 ys_1021 i_1018 = List.nth_1162 xs_1020 i_1018 then
     ()
   else
     {fail} ()
 in
 let main_1161 = let arg1_1157 = rand_int () in
                 let arg2_1159 = rand_int () in
                 main_1017 arg1_1157 arg2_1159 in
 ()

copy_poly:
 let List.nth_1162 (x_1163:int list) (x_1164:int) = rand_int () in
 let rec make_list_1008 (n_1009:int) = if n_1009 < 0 then
                                         []
                                       else
                                         rand_int ()::make_list_1008 (n_1009 - 1) in
 let rec append_1165 (xs__ys_1023:(int list * int list)) =
   match xs__ys_1023 with
   | (xs_1011, ys_1012) ->
       (match xs_1011 with
        | [] -> ys_1012
        | x_1013::xs'_1014 ->
            let rs_1015 = append_1165 (xs'_1014, ys_1012) in
            let rs'_1016 = x_1013::rs_1015 in
            rs'_1016)
 in
 let main_1017 (i_1018:int) (n_1019:int) =
   let xs_1020 = make_list_1008 n_1019 in
   let ys_1021 = append_1165 (xs_1020, []) in
   if List.nth_1162 ys_1021 i_1018 = List.nth_1162 xs_1020 i_1018 then
     ()
   else
     {fail} ()
 in
 let main_1161 = let arg1_1157 = rand_int () in
                 let arg2_1159 = rand_int () in
                 main_1017 arg1_1157 arg2_1159 in
 ()

spec (abstraction type environment for CEGAR-loop):
 append_1165: ((bool ->
                  int ->
                    b1_1055:bool ->
                      int ->
                        (bool ->
                           bool ->
                             int ->
                               b2_1061:bool[\b2_1060. not b1_1055 || b2_1060] ->
                                 b3_1062:bool -> int[\x_1063. not b2_1061 || not b3_1062 && x_1063 = 0] -> X) -> X) ->
                 ((b1_1078:bool ->
                     i_1079:int ->
                       b2_1080:bool ->
                         j_1081:int ->
                           b3_1082:bool ->
                             int ->
                               (b41_1085:bool[\b41_1084. not b1_1078 || b41_1084] ->
                                  bool ->
                                    x_1087:int ->
                                      b51_1089:bool[\b51_1088. not b2_1080 || b51_1088] ->
                                        bool ->
                                          int[\y_1091. not ((b41_1085 && b51_1089) && i_1079 = j_1081) ||
                                                       x_1087 = y_1091] ->
                                            b61_1094:bool[\b61_1093. not b3_1082 || b61_1093] ->
                                              b62_1095:bool ->
                                                int[\z_1096. not b61_1094 || not b62_1095 && z_1096 = 0] -> X) -> X) ->
                    X) -> X)
 ys_1021: ((bool ->
              int ->
                b2_1123:bool ->
                  int ->
                    (bool ->
                       bool ->
                         int ->
                           b41_1129:bool[\b41_1128. not b2_1123 || b41_1128] ->
                             b42_1130:bool -> int[\x_1131. not b41_1129 || not b42_1130 && x_1131 = 0] -> X) -> X) ->
             int -> (b3_1147:bool -> int[\x_1148. not b3_1147 && x_1148 = 0] -> X) -> X)
 
abst_recdata:
 let List.nth_1162 (x_1163:int list) (x_1164:int) = rand_int () in
 let rec make_list_1008 (n_1009:int) = if n_1009 < 0 then
                                         []
                                       else
                                         rand_int ()::make_list_1008 (n_1009 - 1) in
 let rec append_1165 (xs__ys_1023:(int list * int list)) =
   match xs__ys_1023 with
   | (xs_1011, ys_1012) ->
       (match xs_1011 with
        | [] -> ys_1012
        | x_1013::xs'_1014 ->
            let rs_1168 = append_1165 (xs'_1014, ys_1012) in
            let rs'_1169 = x_1013::rs_1168 in
            rs'_1169)
 in
 let main_1017 (i_1018:int) (n_1019:int) =
   let xs_1020 = make_list_1008 n_1019 in
   let ys_1021 = append_1165 (xs_1020, []) in
   if List.nth_1162 ys_1021 i_1018 = List.nth_1162 xs_1020 i_1018 then
     ()
   else
     {fail} ()
 in
 let main_1161 = let arg1_1157 = rand_int () in
                 let arg2_1159 = rand_int () in
                 main_1017 arg1_1157 arg2_1159 in
 ()

encode_list:
 let List.nth_1162 (x_1163:(int -> (bool * int))) (x_1164:int) = rand_int () in
 let rec make_list_1008 (n_1009:int) =
   if n_1009 < 0 then
     fun (x_1236:int) -> (false, 0)
   else
     let x_1227 = rand_int () in
     let xs_1228 = make_list_1008 (n_1009 - 1) in
     fun (i_1226:int) -> (if i_1226 = 0 then
                            (true, x_1227)
                          else
                            xs_1228 (i_1226 - 1))
 in
 let rec append_1165 (xs__ys_1023:((int -> (bool * int)) * (int -> (bool * int)))) =
   let xs_1011 = fst xs__ys_1023 in
   let ys_1012 = snd xs__ys_1023 in
   (label[IdTerm(xs__ys_1023, (xs_1011, ys_1012))]
    (if fst (xs_1011 0) = false then
       (label[IdTerm(xs_1011, (fun x_1427 -> (false, 0)))] ys_1012)
     else
       if fst (xs_1011 0) <> false then
         let xs'_1014 (x_1269:int) = xs_1011 (x_1269 + 1) in
         let x_1013 = snd (xs_1011 0) in
         (label[IdTerm(xs_1011, (fun i_1398 -> (if i_1398 = 0 then
                                                  (true, x_1013)
                                                else
                                                  xs'_1014 (i_1398 - 1))))]
          (let rs_1194 = append_1165 (xs'_1014, ys_1012) in
           let rs'_1195 (i_1369:int) = if i_1369 = 0 then
                                         (true, x_1013)
                                       else
                                         rs_1194 (i_1369 - 1) in
           rs'_1195))
       else
         _|_))
 in
 let main_1017 (i_1018:int) (n_1019:int) =
   let xs_1020 = make_list_1008 n_1019 in
   let ys_1021 = append_1165 (xs_1020, (fun (x_1560:int) -> (false, 0))) in
   if (let x_1610 = ys_1021 i_1018 in
       snd x_1610) = (let x_1600 = xs_1020 i_1018 in
                      snd x_1600) then
     ()
   else
     {fail} ()
 in
 let main_1161 = let arg1_1157 = rand_int () in
                 let arg2_1159 = rand_int () in
                 main_1017 arg1_1157 arg2_1159 in
 ()

INPUT:
let List.nth_1162 x_1163 x_1164 = rand_int () in
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1236 -> (false, 0)
  else
    let x_1227 = rand_int () in
    let xs_1228 = make_list_1008 (n_1009 - 1) in
    fun i_1226 -> (if i_1226 = 0 then
                     (true, x_1227)
                   else
                     xs_1228 (i_1226 - 1))
in
let rec append_1165 xs__ys_1023 =
  let xs_1011 = fst xs__ys_1023 in
  let ys_1012 = snd xs__ys_1023 in
  (label[IdTerm(xs__ys_1023, (xs_1011, ys_1012))]
   (if fst (xs_1011 0) = false then
      (label[IdTerm(xs_1011, (fun x_1427 -> (false, 0)))] ys_1012)
    else
      if fst (xs_1011 0) <> false then
        let xs'_1014 x_1269 = xs_1011 (x_1269 + 1) in
        let x_1013 = snd (xs_1011 0) in
        (label[IdTerm(xs_1011, (fun i_1398 -> (if i_1398 = 0 then
                                                 (true, x_1013)
                                               else
                                                 xs'_1014 (i_1398 - 1))))]
         (let rs_1194 = append_1165 (xs'_1014, ys_1012) in
          let rs'_1195 i_1369 = if i_1369 = 0 then
                                  (true, x_1013)
                                else
                                  rs_1194 (i_1369 - 1) in
          rs'_1195))
      else
        _|_))
in
let main_1017 i_1018 n_1019 =
  let xs_1020 = make_list_1008 n_1019 in
  let ys_1021 = append_1165 (xs_1020, (fun x_1560 -> (false, 0))) in
  if (let x_1610 = ys_1021 i_1018 in
      snd x_1610) = (let x_1600 = xs_1020 i_1018 in
                     snd x_1600) then
    ()
  else
    {fail} ()
in
let main_1161 = let arg1_1157 = rand_int () in
                let arg2_1159 = rand_int () in
                main_1017 arg1_1157 arg2_1159 in
()

normalize:
let List.nth_1162 x_1163 x_1164 = let u_1615 = () in
                                  let f_1614 = rand_int in
                                  let r_f_1617 = f_1614 u_1615 in
                                  r_f_1617 in
let rec make_list_1008 n_1009 =
  let b_1618 = let n_1619 = n_1009 in
               let n_1620 = 0 in
               n_1619 < n_1620 in
  if b_1618 then
    fun x_1236 -> (let b_1621 = false in
                   let n_1622 = 0 in
                   (b_1621, n_1622))
  else
    let x_1227 = let u_1626 = () in
                 let f_1625 = rand_int in
                 let r_f_1628 = f_1625 u_1626 in
                 r_f_1628 in
    let xs_1228 =
      let n_1632 = let n_1629 = n_1009 in
                   let n_1630 = 1 in
                   n_1629 - n_1630 in
      let make_list_1631 = make_list_1008 in
      let r_make_list_1634 = make_list_1631 n_1632 in
      r_make_list_1634
    in
    fun i_1226 ->
      (let b_1635 = let i_1636 = i_1226 in
                    let n_1637 = 0 in
                    i_1636 = n_1637 in
       if b_1635 then
         let b_1638 = true in
         let x_1639 = x_1227 in
         (b_1638, x_1639)
       else
         let n_1645 = let i_1642 = i_1226 in
                      let n_1643 = 1 in
                      i_1642 - n_1643 in
         let xs_1644 = xs_1228 in
         let r_xs_1647 = xs_1644 n_1645 in
         r_xs_1647)
in
let rec append_1165 xs__ys_1023 =
  let xs_1011 = let xs__ys_1649 = xs__ys_1023 in
                fst xs__ys_1649 in
  let ys_1012 = let xs__ys_1650 = xs__ys_1023 in
                snd xs__ys_1650 in
  (label[IdTerm(xs__ys_1023, (let xs_1719 = xs_1011 in
                              let ys_1720 = ys_1012 in
                              (xs_1719, ys_1720)))]
   (let b_1651 =
      let r_xs_0_1657 =
        let r_xs_1656 = let n_1653 = 0 in
                        let xs_1652 = xs_1011 in
                        let r_xs_1655 = xs_1652 n_1653 in
                        r_xs_1655 in
        fst r_xs_1656
      in
      let b_1658 = false in
      r_xs_0_1657 = b_1658
    in
    if b_1651 then
      (label[IdTerm(xs_1011, (fun x_1427 -> (let b_1659 = false in
                                             let n_1660 = 0 in
                                             (b_1659, n_1660))))] ys_1012)
    else
      let b_1663 =
        let b_1671 =
          let r_xs_0_1669 =
            let r_xs_1668 = let n_1665 = 0 in
                            let xs_1664 = xs_1011 in
                            let r_xs_1667 = xs_1664 n_1665 in
                            r_xs_1667 in
            fst r_xs_1668
          in
          let b_1670 = false in
          r_xs_0_1669 = b_1670
        in
        not b_1671
      in
      if b_1663 then
        let xs'_1014 x_1269 =
          let n_1675 = let x_1672 = x_1269 in
                       let n_1673 = 1 in
                       x_1672 + n_1673 in
          let xs_1674 = xs_1011 in
          let r_xs_1677 = xs_1674 n_1675 in
          r_xs_1677
        in
        let x_1013 =
          let r_xs_1682 = let n_1679 = 0 in
                          let xs_1678 = xs_1011 in
                          let r_xs_1681 = xs_1678 n_1679 in
                          r_xs_1681 in
          snd r_xs_1682
        in
        (label[IdTerm(xs_1011,
               (fun i_1398 ->
                  (let b_1704 = let i_1705 = i_1398 in
                                let n_1706 = 0 in
                                i_1705 = n_1706 in
                   if b_1704 then
                     let b_1707 = true in
                     let x_1708 = x_1013 in
                     (b_1707, x_1708)
                   else
                     let n_1714 = let i_1711 = i_1398 in
                                  let n_1712 = 1 in
                                  i_1711 - n_1712 in
                     let xs'_1713 = xs'_1014 in
                     let r_xs'_1716 = xs'_1713 n_1714 in
                     r_xs'_1716)))]
         (let rs_1194 =
            let xs'__ys_1688 = let xs'_1683 = xs'_1014 in
                               let ys_1684 = ys_1012 in
                               (xs'_1683, ys_1684) in
            let append_1687 = append_1165 in
            let r_append_1690 = append_1687 xs'__ys_1688 in
            r_append_1690
          in
          let rs'_1195 i_1369 =
            let b_1691 = let i_1692 = i_1369 in
                         let n_1693 = 0 in
                         i_1692 = n_1693 in
            if b_1691 then
              let b_1694 = true in
              let x_1695 = x_1013 in
              (b_1694, x_1695)
            else
              let n_1701 = let i_1698 = i_1369 in
                           let n_1699 = 1 in
                           i_1698 - n_1699 in
              let rs_1700 = rs_1194 in
              let r_rs_1703 = rs_1700 n_1701 in
              r_rs_1703
          in
          rs'_1195))
      else
        _|_))
in
let main_1017 i_1018 n_1019 =
  let xs_1020 =
    let n_1724 = n_1019 in
    let make_list_1723 = make_list_1008 in
    let r_make_list_1726 = make_list_1723 n_1724 in
    r_make_list_1726
  in
  let ys_1021 =
    let xs__f_1736 =
      let xs_1727 = xs_1020 in
      let f_1732 = fun x_1560 -> (let b_1728 = false in
                                  let n_1729 = 0 in
                                  (b_1728, n_1729)) in
      (xs_1727, f_1732)
    in
    let append_1735 = append_1165 in
    let r_append_1738 = append_1735 xs__f_1736 in
    r_append_1738
  in
  let b_1739 =
    let x_1_1750 =
      let x_1610 = let i_1741 = i_1018 in
                   let ys_1740 = ys_1021 in
                   let r_ys_1743 = ys_1740 i_1741 in
                   r_ys_1743 in
      let x_1744 = x_1610 in
      snd x_1744
    in
    let x_1_1751 =
      let x_1600 = let i_1746 = i_1018 in
                   let xs_1745 = xs_1020 in
                   let r_xs_1748 = xs_1745 i_1746 in
                   r_xs_1748 in
      let x_1749 = x_1600 in
      snd x_1749
    in
    x_1_1750 = x_1_1751
  in
  if b_1739 then
    ()
  else
    let u_1753 = () in
    let f_1752 = {fail} in
    let r_f_1755 = f_1752 u_1753 in
    r_f_1755
in
let main_1161 =
  let arg1_1157 = let u_1757 = () in
                  let f_1756 = rand_int in
                  let r_f_1759 = f_1756 u_1757 in
                  r_f_1759 in
  let arg2_1159 = let u_1761 = () in
                  let f_1760 = rand_int in
                  let r_f_1763 = f_1760 u_1761 in
                  r_f_1763 in
  let arg2_1766 = arg2_1159 in
  let arg1_1765 = arg1_1157 in
  let main_1764 = main_1017 in
  let r_main_1767 = main_1764 arg1_1765 in
  let r_main_1769 = r_main_1767 arg2_1766 in
  r_main_1769
in
()

inline_var_const:
let List.nth_1162 x_1163 x_1164 = let r_f_1617 = rand_int () in
                                  r_f_1617 in
let rec make_list_1008 n_1009 =
  let b_1618 = n_1009 < 0 in
  if b_1618 then
    fun x_1236 -> (false, 0)
  else
    let x_1227 = let r_f_1628 = rand_int () in
                 r_f_1628 in
    let xs_1228 = let n_1632 = n_1009 - 1 in
                  let r_make_list_1634 = make_list_1008 n_1632 in
                  r_make_list_1634 in
    fun i_1226 ->
      (let b_1635 = i_1226 = 0 in
       if b_1635 then
         (true, x_1227)
       else
         let n_1645 = i_1226 - 1 in
         let r_xs_1647 = xs_1228 n_1645 in
         r_xs_1647)
in
let rec append_1165 xs__ys_1023 =
  let xs_1011 = fst xs__ys_1023 in
  let ys_1012 = snd xs__ys_1023 in
  (label[IdTerm(xs__ys_1023, (xs_1011, ys_1012))]
   (let b_1651 =
      let r_xs_0_1657 = let r_xs_1656 = let r_xs_1655 = xs_1011 0 in
                                        r_xs_1655 in
                        fst r_xs_1656 in
      r_xs_0_1657 = false
    in
    if b_1651 then
      (label[IdTerm(xs_1011, (fun x_1427 -> (false, 0)))] ys_1012)
    else
      let b_1663 =
        let b_1671 =
          let r_xs_0_1669 = let r_xs_1668 = let r_xs_1667 = xs_1011 0 in
                                            r_xs_1667 in
                            fst r_xs_1668 in
          r_xs_0_1669 = false
        in
        not b_1671
      in
      if b_1663 then
        let xs'_1014 x_1269 = let n_1675 = x_1269 + 1 in
                              let r_xs_1677 = xs_1011 n_1675 in
                              r_xs_1677 in
        let x_1013 = let r_xs_1682 = let r_xs_1681 = xs_1011 0 in
                                     r_xs_1681 in
                     snd r_xs_1682 in
        (label[IdTerm(xs_1011,
               (fun i_1398 ->
                  (let b_1704 = i_1398 = 0 in
                   if b_1704 then
                     (true, x_1013)
                   else
                     let n_1714 = i_1398 - 1 in
                     let r_xs'_1716 = xs'_1014 n_1714 in
                     r_xs'_1716)))]
         (let rs_1194 =
            let xs'__ys_1688 = (xs'_1014, ys_1012) in
            let r_append_1690 = append_1165 xs'__ys_1688 in
            r_append_1690
          in
          let rs'_1195 i_1369 =
            let b_1691 = i_1369 = 0 in
            if b_1691 then
              (true, x_1013)
            else
              let n_1701 = i_1369 - 1 in
              let r_rs_1703 = rs_1194 n_1701 in
              r_rs_1703
          in
          rs'_1195))
      else
        _|_))
in
let main_1017 i_1018 n_1019 =
  let xs_1020 = let r_make_list_1726 = make_list_1008 n_1019 in
                r_make_list_1726 in
  let ys_1021 =
    let xs__f_1736 = let f_1732 x_1560 = (false, 0) in
                     (xs_1020, f_1732) in
    let r_append_1738 = append_1165 xs__f_1736 in
    r_append_1738
  in
  let b_1739 =
    let x_1_1750 = let x_1610 = let r_ys_1743 = ys_1021 i_1018 in
                                r_ys_1743 in
                   snd x_1610 in
    let x_1_1751 = let x_1600 = let r_xs_1748 = xs_1020 i_1018 in
                                r_xs_1748 in
                   snd x_1600 in
    x_1_1750 = x_1_1751
  in
  if b_1739 then
    ()
  else
    let f_1752 = {fail} in
    let r_f_1755 = f_1752 () in
    r_f_1755
in
let main_1161 =
  let arg1_1157 = let r_f_1759 = rand_int () in
                  r_f_1759 in
  let arg2_1159 = let r_f_1763 = rand_int () in
                  r_f_1763 in
  let r_main_1767 = main_1017 arg1_1157 in
  let r_main_1769 = r_main_1767 arg2_1159 in
  r_main_1769
in
()

flatten_let:
let List.nth_1162 x_1163 x_1164 = let r_f_1617 = rand_int () in
                                  r_f_1617 in
let rec make_list_1008 n_1009 =
  let b_1618 = n_1009 < 0 in
  if b_1618 then
    fun x_1236 -> (false, 0)
  else
    let r_f_1628 = rand_int () in
    let x_1227 = r_f_1628 in
    let n_1632 = n_1009 - 1 in
    let r_make_list_1634 = make_list_1008 n_1632 in
    let xs_1228 = r_make_list_1634 in
    fun i_1226 ->
      (let b_1635 = i_1226 = 0 in
       if b_1635 then
         (true, x_1227)
       else
         let n_1645 = i_1226 - 1 in
         let r_xs_1647 = xs_1228 n_1645 in
         r_xs_1647)
in
let rec append_1165 xs__ys_1023 =
  let xs_1011 = fst xs__ys_1023 in
  let ys_1012 = snd xs__ys_1023 in
  (label[IdTerm(xs__ys_1023, (xs_1011, ys_1012))]
   (let r_xs_1655 = xs_1011 0 in
    let r_xs_1656 = r_xs_1655 in
    let r_xs_0_1657 = fst r_xs_1656 in
    let b_1651 = r_xs_0_1657 = false in
    if b_1651 then
      (label[IdTerm(xs_1011, (fun x_1427 -> (false, 0)))] ys_1012)
    else
      let r_xs_1667 = xs_1011 0 in
      let r_xs_1668 = r_xs_1667 in
      let r_xs_0_1669 = fst r_xs_1668 in
      let b_1671 = r_xs_0_1669 = false in
      let b_1663 = not b_1671 in
      if b_1663 then
        let xs'_1014 x_1269 = let n_1675 = x_1269 + 1 in
                              let r_xs_1677 = xs_1011 n_1675 in
                              r_xs_1677 in
        let r_xs_1681 = xs_1011 0 in
        let r_xs_1682 = r_xs_1681 in
        let x_1013 = snd r_xs_1682 in
        (label[IdTerm(xs_1011,
               (fun i_1398 ->
                  (let b_1704 = i_1398 = 0 in
                   if b_1704 then
                     (true, x_1013)
                   else
                     let n_1714 = i_1398 - 1 in
                     let r_xs'_1716 = xs'_1014 n_1714 in
                     r_xs'_1716)))]
         (let xs'__ys_1688 = (xs'_1014, ys_1012) in
          let r_append_1690 = append_1165 xs'__ys_1688 in
          let rs_1194 = r_append_1690 in
          let rs'_1195 i_1369 =
            let b_1691 = i_1369 = 0 in
            if b_1691 then
              (true, x_1013)
            else
              let n_1701 = i_1369 - 1 in
              let r_rs_1703 = rs_1194 n_1701 in
              r_rs_1703
          in
          rs'_1195))
      else
        _|_))
in
let main_1017 i_1018 n_1019 =
  let r_make_list_1726 = make_list_1008 n_1019 in
  let xs_1020 = r_make_list_1726 in
  let f_1732 x_1560 = (false, 0) in
  let xs__f_1736 = (xs_1020, f_1732) in
  let r_append_1738 = append_1165 xs__f_1736 in
  let ys_1021 = r_append_1738 in
  let r_ys_1743 = ys_1021 i_1018 in
  let x_1610 = r_ys_1743 in
  let x_1_1750 = snd x_1610 in
  let r_xs_1748 = xs_1020 i_1018 in
  let x_1600 = r_xs_1748 in
  let x_1_1751 = snd x_1600 in
  let b_1739 = x_1_1750 = x_1_1751 in
  if b_1739 then
    ()
  else
    let f_1752 = {fail} in
    let r_f_1755 = f_1752 () in
    r_f_1755
in
let r_f_1759 = rand_int () in
let arg1_1157 = r_f_1759 in
let r_f_1763 = rand_int () in
let arg2_1159 = r_f_1763 in
let r_main_1767 = main_1017 arg1_1157 in
let r_main_1769 = r_main_1767 arg2_1159 in
let main_1161 = r_main_1769 in
()

add_proj_info:
let List.nth_1162 x_1163 x_1164 = let r_f_1617 = rand_int () in
                                  r_f_1617 in
let rec make_list_1008 n_1009 =
  let b_1618 = n_1009 < 0 in
  if b_1618 then
    fun x_1236 -> (false, 0)
  else
    let r_f_1628 = rand_int () in
    let x_1227 = r_f_1628 in
    let n_1632 = n_1009 - 1 in
    let r_make_list_1634 = make_list_1008 n_1632 in
    let xs_1228 = r_make_list_1634 in
    fun i_1226 ->
      (let b_1635 = i_1226 = 0 in
       if b_1635 then
         (true, x_1227)
       else
         let n_1645 = i_1226 - 1 in
         let r_xs_1647 = xs_1228 n_1645 in
         r_xs_1647)
in
let rec append_1165 xs__ys_1023 =
  let xs_1011 = fst xs__ys_1023 in
  let ys_1012 = snd xs__ys_1023 in
  (label[IdTerm(xs__ys_1023, (xs_1011, ys_1012))]
   (let r_xs_1655 = xs_1011 0 in
    let r_xs_1656 = r_xs_1655 in
    let r_xs_0_1657 = fst r_xs_1656 in
    let b_1651 = r_xs_0_1657 = false in
    if b_1651 then
      (label[IdTerm(xs_1011, (fun x_1427 -> (false, 0)))] ys_1012)
    else
      let r_xs_1667 = xs_1011 0 in
      let r_xs_1668 = r_xs_1667 in
      let r_xs_0_1669 = fst r_xs_1668 in
      let b_1671 = r_xs_0_1669 = false in
      let b_1663 = not b_1671 in
      if b_1663 then
        let xs'_1014 x_1269 = let n_1675 = x_1269 + 1 in
                              let r_xs_1677 = xs_1011 n_1675 in
                              r_xs_1677 in
        let r_xs_1681 = xs_1011 0 in
        let r_xs_1682 = r_xs_1681 in
        let x_1013 = snd r_xs_1682 in
        (label[IdTerm(xs_1011,
               (fun i_1398 ->
                  (let b_1704 = i_1398 = 0 in
                   if b_1704 then
                     (true, x_1013)
                   else
                     let n_1714 = i_1398 - 1 in
                     let r_xs'_1716 = xs'_1014 n_1714 in
                     r_xs'_1716)))]
         (let xs'__ys_1688 = (xs'_1014, ys_1012) in
          (label[String "ret_fun"]
           (label[IdTerm(xs'_1014, (fst xs'__ys_1688))]
            (label[String "ret_fun"]
             (label[IdTerm(ys_1012, (snd xs'__ys_1688))]
              (let r_append_1690 = append_1165 xs'__ys_1688 in
               let rs_1194 = r_append_1690 in
               let rs'_1195 i_1369 =
                 let b_1691 = i_1369 = 0 in
                 if b_1691 then
                   (true, x_1013)
                 else
                   let n_1701 = i_1369 - 1 in
                   let r_rs_1703 = rs_1194 n_1701 in
                   r_rs_1703
               in
               rs'_1195)))))))
      else
        _|_))
in
let main_1017 i_1018 n_1019 =
  let r_make_list_1726 = make_list_1008 n_1019 in
  let xs_1020 = r_make_list_1726 in
  let f_1732 x_1560 = (false, 0) in
  let xs__f_1736 = (xs_1020, f_1732) in
  (label[String "ret_fun"]
   (label[IdTerm(xs_1020, (fst xs__f_1736))]
    (label[String "ret_fun"]
     (label[IdTerm(f_1732, (snd xs__f_1736))]
      (let r_append_1738 = append_1165 xs__f_1736 in
       let ys_1021 = r_append_1738 in
       let r_ys_1743 = ys_1021 i_1018 in
       let x_1610 = r_ys_1743 in
       let x_1_1750 = snd x_1610 in
       let r_xs_1748 = xs_1020 i_1018 in
       let x_1600 = r_xs_1748 in
       let x_1_1751 = snd x_1600 in
       let b_1739 = x_1_1750 = x_1_1751 in
       if b_1739 then
         ()
       else
         let f_1752 = {fail} in
         let r_f_1755 = f_1752 () in
         r_f_1755)))))
in
let r_f_1759 = rand_int () in
let arg1_1157 = r_f_1759 in
let r_f_1763 = rand_int () in
let arg2_1159 = r_f_1763 in
let r_main_1767 = main_1017 arg1_1157 in
let r_main_1769 = r_main_1767 arg2_1159 in
let main_1161 = r_main_1769 in
()

ret_fun:
let List.nth_1162 (x_1163:(int -> (bool * int))) =
  ((fun (x_1164:int) -> (let r_f_1617 = rand_int () in
                         r_f_1617)), x_1163)
in
let rec make_list_1008 (n_1009:int) =
  let b_1618 = n_1009 < 0 in
  if b_1618 then
    fun (x_1236:int) -> (false, 0)
  else
    let r_f_1628 = rand_int () in
    let x_1227 = r_f_1628 in
    let n_1632 = n_1009 - 1 in
    let r_make_list_1634 = make_list_1008 n_1632 in
    let xs_1228 = r_make_list_1634 in
    fun (i_1226:int) ->
      (let b_1635 = i_1226 = 0 in
       if b_1635 then
         (true, x_1227)
       else
         let n_1645 = i_1226 - 1 in
         let r_xs_1647 = xs_1228 n_1645 in
         r_xs_1647)
in
let rec append_1165 (xs__ys_1023:((int -> (bool * int)) * (int -> (bool * int)))) =
  let xs_1011 = fst xs__ys_1023 in
  let ys_1012 = snd xs__ys_1023 in
  (label[IdTerm(xs__ys_1023, (xs_1011, ys_1012))]
   (let r_xs_1655 = xs_1011 0 in
    let r_xs_1656 = r_xs_1655 in
    let r_xs_0_1657 = fst r_xs_1656 in
    let b_1651 = r_xs_0_1657 = false in
    if b_1651 then
      (label[IdTerm(xs_1011, (fun x_1427 -> (false, 0)))] (ys_1012, xs__ys_1023))
    else
      let r_xs_1667 = xs_1011 0 in
      let r_xs_1668 = r_xs_1667 in
      let r_xs_0_1669 = fst r_xs_1668 in
      let b_1671 = r_xs_0_1669 = false in
      let b_1663 = not b_1671 in
      if b_1663 then
        let xs'_1014 (x_1269:int) = let n_1675 = x_1269 + 1 in
                                    let r_xs_1677 = xs_1011 n_1675 in
                                    r_xs_1677 in
        let r_xs_1681 = xs_1011 0 in
        let r_xs_1682 = r_xs_1681 in
        let x_1013 = snd r_xs_1682 in
        (label[IdTerm(xs_1011,
               (fun i_1398 ->
                  (let b_1704 = i_1398 = 0 in
                   if b_1704 then
                     (true, x_1013)
                   else
                     let n_1714 = i_1398 - 1 in
                     let r_xs'_1716 = xs'_1014 n_1714 in
                     r_xs'_1716)))]
         (let xs'__ys_1688 = (xs'_1014, ys_1012) in
          (label[String "ret_fun"]
           (label[IdTerm(xs'_1014, (fst xs'__ys_1688))]
            (label[String "ret_fun"]
             (label[IdTerm(ys_1012, (snd xs'__ys_1688))]
              (let r_append_xs'__ys_1775 = append_1165 xs'__ys_1688 in
               let r_append_1690 = fst r_append_xs'__ys_1775 in
               let xs'__ys_1776 = snd r_append_xs'__ys_1775 in
               (label[String "ret_fun"]
                (label[IdTerm(xs'__ys_1688, xs'__ys_1776)]
                 (let rs_1194 = r_append_1690 in
                  let rs'_1195 (i_1369:int) =
                    let b_1691 = i_1369 = 0 in
                    if b_1691 then
                      (true, x_1013)
                    else
                      let n_1701 = i_1369 - 1 in
                      let r_rs_1703 = rs_1194 n_1701 in
                      r_rs_1703
                  in
                  (rs'_1195, xs__ys_1023)))))))))))
      else
        (_|_, xs__ys_1023)))
in
let main_1017 (i_1018:int) (n_1019:int) =
  let r_make_list_1726 = make_list_1008 n_1019 in
  let xs_1020 = r_make_list_1726 in
  let f_1732 (x_1560:int) = (false, 0) in
  let xs__f_1736 = (xs_1020, f_1732) in
  (label[String "ret_fun"]
   (label[IdTerm(xs_1020, (fst xs__f_1736))]
    (label[String "ret_fun"]
     (label[IdTerm(f_1732, (snd xs__f_1736))]
      (let r_append_xs__f_1790 = append_1165 xs__f_1736 in
       let r_append_1738 = fst r_append_xs__f_1790 in
       let xs__f_1791 = snd r_append_xs__f_1790 in
       (label[String "ret_fun"]
        (label[IdTerm(xs__f_1736, xs__f_1791)]
         (let ys_1021 = r_append_1738 in
          let r_ys_1743 = ys_1021 i_1018 in
          let x_1610 = r_ys_1743 in
          let x_1_1750 = snd x_1610 in
          let r_xs_1748 = xs_1020 i_1018 in
          let x_1600 = r_xs_1748 in
          let x_1_1751 = snd x_1600 in
          let b_1739 = x_1_1750 = x_1_1751 in
          if b_1739 then
            ()
          else
            let f_1752 = {fail} in
            let r_f_1755 = f_1752 () in
            r_f_1755))))))))
in
let r_f_1759 = rand_int () in
let arg1_1157 = r_f_1759 in
let r_f_1763 = rand_int () in
let arg2_1159 = r_f_1763 in
let r_main_1767 = main_1017 arg1_1157 in
let r_main_1769 = r_main_1767 arg2_1159 in
let main_1161 = r_main_1769 in
()

subst_label:
let List.nth_1162 (x_1163:(int -> (bool * int))) =
  ((fun (x_1164:int) -> (let r_f_1617 = rand_int () in
                         r_f_1617)), x_1163)
in
let rec make_list_1008 (n_1009:int) =
  let b_1618 = n_1009 < 0 in
  if b_1618 then
    fun (x_1236:int) -> (false, 0)
  else
    let r_f_1628 = rand_int () in
    let x_1227 = r_f_1628 in
    let n_1632 = n_1009 - 1 in
    let r_make_list_1634 = make_list_1008 n_1632 in
    let xs_1228 = r_make_list_1634 in
    fun (i_1226:int) ->
      (let b_1635 = i_1226 = 0 in
       if b_1635 then
         (true, x_1227)
       else
         let n_1645 = i_1226 - 1 in
         let r_xs_1647 = xs_1228 n_1645 in
         r_xs_1647)
in
let rec append_1165 (xs__ys_1023:((int -> (bool * int)) * (int -> (bool * int)))) =
  let xs_1011 = fst xs__ys_1023 in
  let ys_1012 = snd xs__ys_1023 in
  let r_xs_1655 = xs_1011 0 in
  let r_xs_1656 = r_xs_1655 in
  let r_xs_0_1657 = fst r_xs_1656 in
  let b_1651 = r_xs_0_1657 = false in
  if b_1651 then
    (ys_1012, (xs_1011, ys_1012))
  else
    let r_xs_1667 = xs_1011 0 in
    let r_xs_1668 = r_xs_1667 in
    let r_xs_0_1669 = fst r_xs_1668 in
    let b_1671 = r_xs_0_1669 = false in
    let b_1663 = not b_1671 in
    if b_1663 then
      let xs'_1014 (x_1269:int) = let n_1675 = x_1269 + 1 in
                                  let r_xs_1677 = xs_1011 n_1675 in
                                  r_xs_1677 in
      let r_xs_1681 = xs_1011 0 in
      let r_xs_1682 = r_xs_1681 in
      let x_1013 = snd r_xs_1682 in
      let xs'__ys_1688 = (xs'_1014, ys_1012) in
      (label[String "ret_fun"]
       (label[String "ret_fun"]
        (let r_append_xs'__ys_1775 = append_1165 xs'__ys_1688 in
         let r_append_1690 = fst r_append_xs'__ys_1775 in
         let xs'__ys_1776 = snd r_append_xs'__ys_1775 in
         (label[String "ret_fun"]
          (let rs_1194 = r_append_1690 in
           let rs'_1195 (i_1369:int) =
             let b_1691 = i_1369 = 0 in
             if b_1691 then
               (true, x_1013)
             else
               let n_1701 = i_1369 - 1 in
               let r_rs_1703 = rs_1194 n_1701 in
               r_rs_1703
           in
           (rs'_1195, (xs_1011, ys_1012)))))))
    else
      (_|_, (xs_1011, ys_1012))
in
let main_1017 (i_1018:int) (n_1019:int) =
  let r_make_list_1726 = make_list_1008 n_1019 in
  let xs_1020 = r_make_list_1726 in
  let f_1732 (x_1560:int) = (false, 0) in
  let xs__f_1736 = (xs_1020, f_1732) in
  (label[String "ret_fun"]
   (label[String "ret_fun"]
    (let r_append_xs__f_1790 = append_1165 xs__f_1736 in
     let r_append_1738 = fst r_append_xs__f_1790 in
     let xs__f_1791 = snd r_append_xs__f_1790 in
     (label[String "ret_fun"]
      (let ys_1021 = r_append_1738 in
       let r_ys_1743 = ys_1021 i_1018 in
       let x_1610 = r_ys_1743 in
       let x_1_1750 = snd x_1610 in
       let r_xs_1748 = (fst xs__f_1736) i_1018 in
       let x_1600 = r_xs_1748 in
       let x_1_1751 = snd x_1600 in
       let b_1739 = x_1_1750 = x_1_1751 in
       if b_1739 then
         ()
       else
         let f_1752 = {fail} in
         let r_f_1755 = f_1752 () in
         r_f_1755)))))
in
let r_f_1759 = rand_int () in
let arg1_1157 = r_f_1759 in
let r_f_1763 = rand_int () in
let arg2_1159 = r_f_1763 in
let r_main_1767 = main_1017 arg1_1157 in
let r_main_1769 = r_main_1767 arg2_1159 in
let main_1161 = r_main_1769 in
()

remove_label:
let List.nth_1162 (x_1163:(int -> (bool * int))) =
  ((fun (x_1164:int) -> (let r_f_1617 = rand_int () in
                         r_f_1617)), x_1163)
in
let rec make_list_1008 (n_1009:int) =
  let b_1618 = n_1009 < 0 in
  if b_1618 then
    fun (x_1236:int) -> (false, 0)
  else
    let r_f_1628 = rand_int () in
    let x_1227 = r_f_1628 in
    let n_1632 = n_1009 - 1 in
    let r_make_list_1634 = make_list_1008 n_1632 in
    let xs_1228 = r_make_list_1634 in
    fun (i_1226:int) ->
      (let b_1635 = i_1226 = 0 in
       if b_1635 then
         (true, x_1227)
       else
         let n_1645 = i_1226 - 1 in
         let r_xs_1647 = xs_1228 n_1645 in
         r_xs_1647)
in
let rec append_1165 (xs__ys_1023:((int -> (bool * int)) * (int -> (bool * int)))) =
  let xs_1011 = fst xs__ys_1023 in
  let ys_1012 = snd xs__ys_1023 in
  let r_xs_1655 = xs_1011 0 in
  let r_xs_1656 = r_xs_1655 in
  let r_xs_0_1657 = fst r_xs_1656 in
  let b_1651 = r_xs_0_1657 = false in
  if b_1651 then
    (ys_1012, (xs_1011, ys_1012))
  else
    let r_xs_1667 = xs_1011 0 in
    let r_xs_1668 = r_xs_1667 in
    let r_xs_0_1669 = fst r_xs_1668 in
    let b_1671 = r_xs_0_1669 = false in
    let b_1663 = not b_1671 in
    if b_1663 then
      let xs'_1014 (x_1269:int) = let n_1675 = x_1269 + 1 in
                                  let r_xs_1677 = xs_1011 n_1675 in
                                  r_xs_1677 in
      let r_xs_1681 = xs_1011 0 in
      let r_xs_1682 = r_xs_1681 in
      let x_1013 = snd r_xs_1682 in
      let xs'__ys_1688 = (xs'_1014, ys_1012) in
      let r_append_xs'__ys_1775 = append_1165 xs'__ys_1688 in
      let r_append_1690 = fst r_append_xs'__ys_1775 in
      let xs'__ys_1776 = snd r_append_xs'__ys_1775 in
      let rs_1194 = r_append_1690 in
      let rs'_1195 (i_1369:int) =
        let b_1691 = i_1369 = 0 in
        if b_1691 then
          (true, x_1013)
        else
          let n_1701 = i_1369 - 1 in
          let r_rs_1703 = rs_1194 n_1701 in
          r_rs_1703
      in
      (rs'_1195, (xs_1011, ys_1012))
    else
      (_|_, (xs_1011, ys_1012))
in
let main_1017 (i_1018:int) (n_1019:int) =
  let r_make_list_1726 = make_list_1008 n_1019 in
  let xs_1020 = r_make_list_1726 in
  let f_1732 (x_1560:int) = (false, 0) in
  let xs__f_1736 = (xs_1020, f_1732) in
  let r_append_xs__f_1790 = append_1165 xs__f_1736 in
  let r_append_1738 = fst r_append_xs__f_1790 in
  let xs__f_1791 = snd r_append_xs__f_1790 in
  let ys_1021 = r_append_1738 in
  let r_ys_1743 = ys_1021 i_1018 in
  let x_1610 = r_ys_1743 in
  let x_1_1750 = snd x_1610 in
  let r_xs_1748 = (fst xs__f_1736) i_1018 in
  let x_1600 = r_xs_1748 in
  let x_1_1751 = snd x_1600 in
  let b_1739 = x_1_1750 = x_1_1751 in
  if b_1739 then
    ()
  else
    let f_1752 = {fail} in
    let r_f_1755 = f_1752 () in
    r_f_1755
in
let r_f_1759 = rand_int () in
let arg1_1157 = r_f_1759 in
let r_f_1763 = rand_int () in
let arg2_1159 = r_f_1763 in
let r_main_1767 = main_1017 arg1_1157 in
let r_main_1769 = r_main_1767 arg2_1159 in
let main_1161 = r_main_1769 in
()

flatten_tuple:
let List.nth_1162 (x_1163:(int -> (bool * int))) =
  let f_1792 = fun (x_1164:int) -> (let r_f_1617 = rand_int () in
                                    r_f_1617) in
  let x_1793 = x_1163 in
  let x_1795 = x_1793 in
  let f_1794 = f_1792 in
  (f_1794, x_1795)
in
let rec make_list_1008 (n_1009:int) =
  let b_1618 = n_1009 < 0 in
  if b_1618 then
    fun (x_1236:int) ->
      (let b_1804 = false in
       let n_1805 = 0 in
       let n_1807 = n_1805 in
       let b_1806 = b_1804 in
       (b_1806, n_1807))
  else
    let r_f_1628 = rand_int () in
    let x_1227 = r_f_1628 in
    let n_1632 = n_1009 - 1 in
    let r_make_list_1634 = make_list_1008 n_1632 in
    let xs_1228 = r_make_list_1634 in
    fun (i_1226:int) ->
      (let b_1635 = i_1226 = 0 in
       if b_1635 then
         let b_1798 = true in
         let x_1799 = x_1227 in
         let x_1801 = x_1799 in
         let b_1800 = b_1798 in
         (b_1800, x_1801)
       else
         let n_1645 = i_1226 - 1 in
         let r_xs_1647 = xs_1228 n_1645 in
         r_xs_1647)
in
let rec append_1165 (xs__ys_1023:((int -> (bool * int)) * (int -> (bool * int)))) =
  let xs_1011 = let xs__ys0_1810 = xs__ys_1023 in
                fst xs__ys0_1810 in
  let ys_1012 = let xs__ys1_1811 = xs__ys_1023 in
                snd xs__ys1_1811 in
  let r_xs_1655 = xs_1011 0 in
  let r_xs_1656 = r_xs_1655 in
  let r_xs_0_1657 = let r_xs0_1812 = r_xs_1656 in
                    fst r_xs0_1812 in
  let b_1651 = r_xs_0_1657 = false in
  if b_1651 then
    let ys_1865 = ys_1012 in
    let xs__ys_1866 =
      let xs_1859 = xs_1011 in
      let ys_1860 = ys_1012 in
      let ys_1862 = ys_1860 in
      let xs_1861 = xs_1859 in
      (xs_1861, ys_1862)
    in
    let xs_1868 = fst xs__ys_1866 in
    let ys_1869 = snd xs__ys_1866 in
    let ys_1867 = ys_1865 in
    (ys_1867, xs_1868, ys_1869)
  else
    let r_xs_1667 = xs_1011 0 in
    let r_xs_1668 = r_xs_1667 in
    let r_xs_0_1669 = let r_xs0_1813 = r_xs_1668 in
                      fst r_xs0_1813 in
    let b_1671 = r_xs_0_1669 = false in
    let b_1663 = not b_1671 in
    if b_1663 then
      let xs'_1014 (x_1269:int) = let n_1675 = x_1269 + 1 in
                                  let r_xs_1677 = xs_1011 n_1675 in
                                  r_xs_1677 in
      let r_xs_1681 = xs_1011 0 in
      let r_xs_1682 = r_xs_1681 in
      let x_1013 = let r_xs1_1828 = r_xs_1682 in
                   snd r_xs1_1828 in
      let xs'__ys_1688 =
        let xs'_1829 = xs'_1014 in
        let ys_1830 = ys_1012 in
        let ys_1832 = ys_1830 in
        let xs'_1831 = xs'_1829 in
        (xs'_1831, ys_1832)
      in
      let r_append_xs'__ys_1775 = append_1165 xs'__ys_1688 in
      let r_append_1690 = let r_append_xs'__ys0_1835 = r_append_xs'__ys_1775 in
                          #0 r_append_xs'__ys0_1835 in
      let xs'__ys_1776 =
        let r_append_xs'__ys1_1836 = r_append_xs'__ys_1775 in
        (#1 r_append_xs'__ys1_1836, #2 r_append_xs'__ys1_1836)
      in
      let rs_1194 = r_append_1690 in
      let rs'_1195 (i_1369:int) =
        let b_1691 = i_1369 = 0 in
        if b_1691 then
          let b_1839 = true in
          let x_1840 = x_1013 in
          let x_1842 = x_1840 in
          let b_1841 = b_1839 in
          (b_1841, x_1842)
        else
          let n_1701 = i_1369 - 1 in
          let r_rs_1703 = rs_1194 n_1701 in
          r_rs_1703
      in
      let rs'_1851 = rs'_1195 in
      let xs__ys_1852 =
        let xs_1845 = xs_1011 in
        let ys_1846 = ys_1012 in
        let ys_1848 = ys_1846 in
        let xs_1847 = xs_1845 in
        (xs_1847, ys_1848)
      in
      let xs_1854 = fst xs__ys_1852 in
      let ys_1855 = snd xs__ys_1852 in
      let rs'_1853 = rs'_1851 in
      (rs'_1853, xs_1854, ys_1855)
    else
      let bot_1820 = _|_ in
      let xs__ys_1821 =
        let xs_1814 = xs_1011 in
        let ys_1815 = ys_1012 in
        let ys_1817 = ys_1815 in
        let xs_1816 = xs_1814 in
        (xs_1816, ys_1817)
      in
      let xs_1823 = fst xs__ys_1821 in
      let ys_1824 = snd xs__ys_1821 in
      let bot_1822 = bot_1820 in
      (bot_1822, xs_1823, ys_1824)
in
let main_1017 (i_1018:int) (n_1019:int) =
  let r_make_list_1726 = make_list_1008 n_1019 in
  let xs_1020 = r_make_list_1726 in
  let f_1732 (x_1560:int) =
    let b_1873 = false in
    let n_1874 = 0 in
    let n_1876 = n_1874 in
    let b_1875 = b_1873 in
    (b_1875, n_1876)
  in
  let xs__f_1736 =
    let xs_1879 = xs_1020 in
    let f_1880 = f_1732 in
    let f_1882 = f_1880 in
    let xs_1881 = xs_1879 in
    (xs_1881, f_1882)
  in
  let r_append_xs__f_1790 = append_1165 xs__f_1736 in
  let r_append_1738 = let r_append_xs__f0_1885 = r_append_xs__f_1790 in
                      #0 r_append_xs__f0_1885 in
  let xs__f_1791 = let r_append_xs__f1_1886 = r_append_xs__f_1790 in
                   (#1 r_append_xs__f1_1886, #2 r_append_xs__f1_1886) in
  let ys_1021 = r_append_1738 in
  let r_ys_1743 = ys_1021 i_1018 in
  let x_1610 = r_ys_1743 in
  let x_1_1750 = let x1_1889 = x_1610 in
                 snd x1_1889 in
  let r_xs_1748 = (let xs__f0_1890 = xs__f_1736 in
                   fst xs__f0_1890) i_1018 in
  let x_1600 = r_xs_1748 in
  let x_1_1751 = let x1_1891 = x_1600 in
                 snd x1_1891 in
  let b_1739 = x_1_1750 = x_1_1751 in
  if b_1739 then
    ()
  else
    let f_1752 = {fail} in
    let r_f_1755 = f_1752 () in
    r_f_1755
in
let r_f_1759 = rand_int () in
let arg1_1157 = r_f_1759 in
let r_f_1763 = rand_int () in
let arg2_1159 = r_f_1763 in
let r_main_1767 = main_1017 arg1_1157 in
let r_main_1769 = r_main_1767 arg2_1159 in
let main_1161 = r_main_1769 in
()

inline_var_const:
let List.nth_1162 (x_1163:(int -> (bool * int))) =
  let f_1792 = fun (x_1164:int) -> (let r_f_1617 = rand_int () in
                                    r_f_1617) in
  (f_1792, x_1163)
in
let rec make_list_1008 (n_1009:int) =
  let b_1618 = n_1009 < 0 in
  if b_1618 then
    fun (x_1236:int) -> (false, 0)
  else
    let r_f_1628 = rand_int () in
    let n_1632 = n_1009 - 1 in
    let r_make_list_1634 = make_list_1008 n_1632 in
    fun (i_1226:int) ->
      (let b_1635 = i_1226 = 0 in
       if b_1635 then
         (true, r_f_1628)
       else
         let n_1645 = i_1226 - 1 in
         let r_xs_1647 = r_make_list_1634 n_1645 in
         r_xs_1647)
in
let rec append_1165 (xs__ys_1023:((int -> (bool * int)) * (int -> (bool * int)))) =
  let xs_1011 = fst xs__ys_1023 in
  let ys_1012 = snd xs__ys_1023 in
  let r_xs_1655 = xs_1011 0 in
  let r_xs_0_1657 = fst r_xs_1655 in
  let b_1651 = r_xs_0_1657 = false in
  if b_1651 then
    let xs__ys_1866 = (xs_1011, ys_1012) in
    let xs_1868 = fst xs__ys_1866 in
    let ys_1869 = snd xs__ys_1866 in
    (ys_1012, xs_1868, ys_1869)
  else
    let r_xs_1667 = xs_1011 0 in
    let r_xs_0_1669 = fst r_xs_1667 in
    let b_1671 = r_xs_0_1669 = false in
    let b_1663 = not b_1671 in
    if b_1663 then
      let xs'_1014 (x_1269:int) = let n_1675 = x_1269 + 1 in
                                  let r_xs_1677 = xs_1011 n_1675 in
                                  r_xs_1677 in
      let r_xs_1681 = xs_1011 0 in
      let x_1013 = snd r_xs_1681 in
      let xs'__ys_1688 = (xs'_1014, ys_1012) in
      let r_append_xs'__ys_1775 = append_1165 xs'__ys_1688 in
      let r_append_1690 = #0 r_append_xs'__ys_1775 in
      let xs'__ys_1776 = (#1 r_append_xs'__ys_1775, #2 r_append_xs'__ys_1775) in
      let rs'_1195 (i_1369:int) =
        let b_1691 = i_1369 = 0 in
        if b_1691 then
          (true, x_1013)
        else
          let n_1701 = i_1369 - 1 in
          let r_rs_1703 = r_append_1690 n_1701 in
          r_rs_1703
      in
      let xs__ys_1852 = (xs_1011, ys_1012) in
      let xs_1854 = fst xs__ys_1852 in
      let ys_1855 = snd xs__ys_1852 in
      (rs'_1195, xs_1854, ys_1855)
    else
      let bot_1820 = _|_ in
      let xs__ys_1821 = (xs_1011, ys_1012) in
      let xs_1823 = fst xs__ys_1821 in
      let ys_1824 = snd xs__ys_1821 in
      (bot_1820, xs_1823, ys_1824)
in
let main_1017 (i_1018:int) (n_1019:int) =
  let r_make_list_1726 = make_list_1008 n_1019 in
  let f_1732 (x_1560:int) = (false, 0) in
  let xs__f_1736 = (r_make_list_1726, f_1732) in
  let r_append_xs__f_1790 = append_1165 xs__f_1736 in
  let r_append_1738 = #0 r_append_xs__f_1790 in
  let xs__f_1791 = (#1 r_append_xs__f_1790, #2 r_append_xs__f_1790) in
  let r_ys_1743 = r_append_1738 i_1018 in
  let x_1_1750 = snd r_ys_1743 in
  let r_xs_1748 = (fst xs__f_1736) i_1018 in
  let x_1_1751 = snd r_xs_1748 in
  let b_1739 = x_1_1750 = x_1_1751 in
  if b_1739 then
    ()
  else
    let f_1752 = {fail} in
    let r_f_1755 = f_1752 () in
    r_f_1755
in
let r_f_1759 = rand_int () in
let r_f_1763 = rand_int () in
let r_main_1767 = main_1017 r_f_1759 in
let r_main_1769 = r_main_1767 r_f_1763 in
()

flatten_let:
let List.nth_1162 x_1163 = let f_1792 x_1164 = let r_f_1617 = rand_int () in
                                               r_f_1617 in
                           (f_1792, x_1163) in
let rec make_list_1008 n_1009 =
  let b_1618 = n_1009 < 0 in
  if b_1618 then
    fun x_1236 -> (false, 0)
  else
    let r_f_1628 = rand_int () in
    let n_1632 = n_1009 - 1 in
    let r_make_list_1634 = make_list_1008 n_1632 in
    fun i_1226 ->
      (let b_1635 = i_1226 = 0 in
       if b_1635 then
         (true, r_f_1628)
       else
         let n_1645 = i_1226 - 1 in
         let r_xs_1647 = r_make_list_1634 n_1645 in
         r_xs_1647)
in
let rec append_1165 xs__ys_1023 =
  let xs_1011 = fst xs__ys_1023 in
  let ys_1012 = snd xs__ys_1023 in
  let r_xs_1655 = xs_1011 0 in
  let r_xs_0_1657 = fst r_xs_1655 in
  let b_1651 = r_xs_0_1657 = false in
  if b_1651 then
    let xs_1868 = fst xs__ys_1023 in
    let ys_1869 = snd xs__ys_1023 in
    (ys_1012, xs_1868, ys_1869)
  else
    let r_xs_1667 = xs_1011 0 in
    let r_xs_0_1669 = fst r_xs_1667 in
    let b_1671 = r_xs_0_1669 = false in
    let b_1663 = not b_1671 in
    if b_1663 then
      let xs'_1014 x_1269 = let n_1675 = x_1269 + 1 in
                            let r_xs_1677 = xs_1011 n_1675 in
                            r_xs_1677 in
      let r_xs_1681 = xs_1011 0 in
      let x_1013 = snd r_xs_1681 in
      let xs'__ys_1688 = (xs'_1014, ys_1012) in
      let r_append_xs'__ys_1775 = append_1165 xs'__ys_1688 in
      let r_append_1690 = #0 r_append_xs'__ys_1775 in
      let xs'__ys_1776 = (#1 r_append_xs'__ys_1775, #2 r_append_xs'__ys_1775) in
      let rs'_1195 i_1369 =
        let b_1691 = i_1369 = 0 in
        if b_1691 then
          (true, x_1013)
        else
          let n_1701 = i_1369 - 1 in
          let r_rs_1703 = r_append_1690 n_1701 in
          r_rs_1703
      in
      let xs_1854 = fst xs__ys_1023 in
      let ys_1855 = snd xs__ys_1023 in
      (rs'_1195, xs_1854, ys_1855)
    else
      let bot_1820 = _|_ in
      let xs_1823 = fst xs__ys_1023 in
      let ys_1824 = snd xs__ys_1023 in
      (bot_1820, xs_1823, ys_1824)
in
let main_1017 i_1018 n_1019 =
  let r_make_list_1726 = make_list_1008 n_1019 in
  let f_1732 x_1560 = (false, 0) in
  let xs__f_1736 = (r_make_list_1726, f_1732) in
  let r_append_xs__f_1790 = append_1165 xs__f_1736 in
  let r_append_1738 = #0 r_append_xs__f_1790 in
  let xs__f_1791 = (#1 r_append_xs__f_1790, #2 r_append_xs__f_1790) in
  let r_ys_1743 = r_append_1738 i_1018 in
  let x_1_1750 = snd r_ys_1743 in
  let r_xs_1748 = (fst xs__f_1736) i_1018 in
  let x_1_1751 = snd r_xs_1748 in
  let b_1739 = x_1_1750 = x_1_1751 in
  if b_1739 then
    ()
  else
    let f_1752 = {fail} in
    let r_f_1755 = f_1752 () in
    r_f_1755
in
let r_f_1759 = rand_int () in
let r_f_1763 = rand_int () in
let r_main_1767 = main_1017 r_f_1759 in
let r_main_1769 = r_main_1767 r_f_1763 in
()

beta_var_tuple:
let rec make_list_1008 n_1009 =
  let b_1618 = n_1009 < 0 in
  if b_1618 then
    fun x_1236 -> (false, 0)
  else
    let r_f_1628 = rand_int () in
    let n_1632 = n_1009 - 1 in
    let r_make_list_1634 = make_list_1008 n_1632 in
    fun i_1226 ->
      (let b_1635 = i_1226 = 0 in
       if b_1635 then
         (true, r_f_1628)
       else
         let n_1645 = i_1226 - 1 in
         let r_xs_1647 = r_make_list_1634 n_1645 in
         r_xs_1647)
in
let rec append_1165 xs__ys_1023 =
  let xs_1011 = fst xs__ys_1023 in
  let ys_1012 = snd xs__ys_1023 in
  let r_xs_1655 = xs_1011 0 in
  let r_xs_0_1657 = fst r_xs_1655 in
  let b_1651 = r_xs_0_1657 = false in
  if b_1651 then
    let xs_1868 = fst xs__ys_1023 in
    let ys_1869 = snd xs__ys_1023 in
    (ys_1012, xs_1868, ys_1869)
  else
    let r_xs_1667 = xs_1011 0 in
    let r_xs_0_1669 = fst r_xs_1667 in
    let b_1671 = r_xs_0_1669 = false in
    let b_1663 = not b_1671 in
    if b_1663 then
      let xs'_1014 x_1269 = let n_1675 = x_1269 + 1 in
                            let r_xs_1677 = xs_1011 n_1675 in
                            r_xs_1677 in
      let r_xs_1681 = xs_1011 0 in
      let x_1013 = snd r_xs_1681 in
      let xs'__ys_1688 = (xs'_1014, ys_1012) in
      let r_append_xs'__ys_1775 = append_1165 xs'__ys_1688 in
      let r_append_1690 = #0 r_append_xs'__ys_1775 in
      let rs'_1195 i_1369 =
        let b_1691 = i_1369 = 0 in
        if b_1691 then
          (true, x_1013)
        else
          let n_1701 = i_1369 - 1 in
          let r_rs_1703 = r_append_1690 n_1701 in
          r_rs_1703
      in
      let xs_1854 = fst xs__ys_1023 in
      let ys_1855 = snd xs__ys_1023 in
      (rs'_1195, xs_1854, ys_1855)
    else
      let bot_1820 = _|_ in
      let xs_1823 = fst xs__ys_1023 in
      let ys_1824 = snd xs__ys_1023 in
      (bot_1820, xs_1823, ys_1824)
in
let main_1017 i_1018 n_1019 =
  let r_make_list_1726 = make_list_1008 n_1019 in
  let f_1732 x_1560 = (false, 0) in
  let xs__f_1736 = (r_make_list_1726, f_1732) in
  let r_append_xs__f_1790 = append_1165 xs__f_1736 in
  let r_append_1738 = #0 r_append_xs__f_1790 in
  let r_ys_1743 = r_append_1738 i_1018 in
  let x_1_1750 = snd r_ys_1743 in
  let r_xs_1748 = r_make_list_1726 i_1018 in
  let x_1_1751 = snd r_xs_1748 in
  let b_1739 = x_1_1750 = x_1_1751 in
  if b_1739 then
    ()
  else
    let f_1752 = {fail} in
    let r_f_1755 = f_1752 () in
    r_f_1755
in
let r_f_1759 = rand_int () in
let r_f_1763 = rand_int () in
let r_main_1767 = main_1017 r_f_1759 in
let r_main_1769 = r_main_1767 r_f_1763 in
()

ret_fun:
 let rec make_list_1008 (n_1009:int) =
   let b_1618 = n_1009 < 0 in
   if b_1618 then
     fun (x_1236:int) -> (false, 0)
   else
     let r_f_1628 = rand_int () in
     let n_1632 = n_1009 - 1 in
     let r_make_list_1634 = make_list_1008 n_1632 in
     fun (i_1226:int) ->
       (let b_1635 = i_1226 = 0 in
        if b_1635 then
          (true, r_f_1628)
        else
          let n_1645 = i_1226 - 1 in
          let r_xs_1647 = r_make_list_1634 n_1645 in
          r_xs_1647)
 in
 let rec append_1165 (xs__ys_1023:((int -> (bool * int)) * (int -> (bool * int)))) =
   let xs_1011 = fst xs__ys_1023 in
   let ys_1012 = snd xs__ys_1023 in
   let r_xs_1655 = xs_1011 0 in
   let r_xs_0_1657 = fst r_xs_1655 in
   let b_1651 = r_xs_0_1657 = false in
   if b_1651 then
     let xs_1868 = fst xs__ys_1023 in
     let ys_1869 = snd xs__ys_1023 in
     (ys_1012, xs_1868, ys_1869)
   else
     let r_xs_1667 = xs_1011 0 in
     let r_xs_0_1669 = fst r_xs_1667 in
     let b_1671 = r_xs_0_1669 = false in
     let b_1663 = not b_1671 in
     if b_1663 then
       let xs'_1014 (x_1269:int) = let n_1675 = x_1269 + 1 in
                                   let r_xs_1677 = xs_1011 n_1675 in
                                   r_xs_1677 in
       let r_xs_1681 = xs_1011 0 in
       let x_1013 = snd r_xs_1681 in
       let xs'__ys_1688 = (xs'_1014, ys_1012) in
       let r_append_xs'__ys_1775 = append_1165 xs'__ys_1688 in
       let r_append_1690 = #0 r_append_xs'__ys_1775 in
       let rs'_1195 (i_1369:int) =
         let b_1691 = i_1369 = 0 in
         if b_1691 then
           (true, x_1013)
         else
           let n_1701 = i_1369 - 1 in
           let r_rs_1703 = r_append_1690 n_1701 in
           r_rs_1703
       in
       let xs_1854 = fst xs__ys_1023 in
       let ys_1855 = snd xs__ys_1023 in
       (rs'_1195, xs_1854, ys_1855)
     else
       let bot_1820 = _|_ in
       let xs_1823 = fst xs__ys_1023 in
       let ys_1824 = snd xs__ys_1023 in
       (bot_1820, xs_1823, ys_1824)
 in
 let main_1017 (i_1018:int) (n_1019:int) =
   let r_make_list_1726 = make_list_1008 n_1019 in
   let f_1732 (x_1560:int) = (false, 0) in
   let xs__f_1736 = (r_make_list_1726, f_1732) in
   let r_append_xs__f_1790 = append_1165 xs__f_1736 in
   let r_append_1738 = #0 r_append_xs__f_1790 in
   let r_ys_1743 = r_append_1738 i_1018 in
   let x_1_1750 = snd r_ys_1743 in
   let r_xs_1748 = r_make_list_1726 i_1018 in
   let x_1_1751 = snd r_xs_1748 in
   let b_1739 = x_1_1750 = x_1_1751 in
   if b_1739 then
     ()
   else
     let f_1752 = {fail} in
     let r_f_1755 = f_1752 () in
     r_f_1755
 in
 let r_f_1759 = rand_int () in
 let r_f_1763 = rand_int () in
 let r_main_1767 = main_1017 r_f_1759 in
 let r_main_1769 = r_main_1767 r_f_1763 in
 ()

INPUT: let rec make_list_1008 n_1009 =
         let b_1618 = n_1009 < 0 in
         if b_1618 then
           fun x_1236 -> (false, 0)
         else
           let r_f_1628 = rand_int () in
           let n_1632 = n_1009 - 1 in
           let r_make_list_1634 = make_list_1008 n_1632 in
           fun i_1226 ->
             (let b_1635 = i_1226 = 0 in
              if b_1635 then
                (true, r_f_1628)
              else
                let n_1645 = i_1226 - 1 in
                let r_xs_1647 = r_make_list_1634 n_1645 in
                r_xs_1647)
       in
       let rec append_1165 xs__ys_1023 =
         let xs_1011 = fst xs__ys_1023 in
         let ys_1012 = snd xs__ys_1023 in
         let r_xs_1655 = xs_1011 0 in
         let r_xs_0_1657 = fst r_xs_1655 in
         let b_1651 = r_xs_0_1657 = false in
         if b_1651 then
           let xs_1868 = fst xs__ys_1023 in
           let ys_1869 = snd xs__ys_1023 in
           (ys_1012, xs_1868, ys_1869)
         else
           let r_xs_1667 = xs_1011 0 in
           let r_xs_0_1669 = fst r_xs_1667 in
           let b_1671 = r_xs_0_1669 = false in
           let b_1663 = not b_1671 in
           if b_1663 then
             let xs'_1014 x_1269 = let n_1675 = x_1269 + 1 in
                                   let r_xs_1677 = xs_1011 n_1675 in
                                   r_xs_1677 in
             let r_xs_1681 = xs_1011 0 in
             let x_1013 = snd r_xs_1681 in
             let xs'__ys_1688 = (xs'_1014, ys_1012) in
             let r_append_xs'__ys_1775 = append_1165 xs'__ys_1688 in
             let r_append_1690 = #0 r_append_xs'__ys_1775 in
             let rs'_1195 i_1369 =
               let b_1691 = i_1369 = 0 in
               if b_1691 then
                 (true, x_1013)
               else
                 let n_1701 = i_1369 - 1 in
                 let r_rs_1703 = r_append_1690 n_1701 in
                 r_rs_1703
             in
             let xs_1854 = fst xs__ys_1023 in
             let ys_1855 = snd xs__ys_1023 in
             (rs'_1195, xs_1854, ys_1855)
           else
             let bot_1820 = _|_ in
             let xs_1823 = fst xs__ys_1023 in
             let ys_1824 = snd xs__ys_1023 in
             (bot_1820, xs_1823, ys_1824)
       in
       let main_1017 i_1018 n_1019 =
         let r_make_list_1726 = make_list_1008 n_1019 in
         let f_1732 x_1560 = (false, 0) in
         let xs__f_1736 = (r_make_list_1726, f_1732) in
         let r_append_xs__f_1790 = append_1165 xs__f_1736 in
         let r_append_1738 = #0 r_append_xs__f_1790 in
         let r_ys_1743 = r_append_1738 i_1018 in
         let x_1_1750 = snd r_ys_1743 in
         let r_xs_1748 = r_make_list_1726 i_1018 in
         let x_1_1751 = snd r_xs_1748 in
         let b_1739 = x_1_1750 = x_1_1751 in
         if b_1739 then
           ()
         else
           let f_1752 = {fail} in
           let r_f_1755 = f_1752 () in
           r_f_1755
       in
       let r_f_1759 = rand_int () in
       let r_f_1763 = rand_int () in
       let r_main_1767 = main_1017 r_f_1759 in
       let r_main_1769 = r_main_1767 r_f_1763 in
       ()
remove_label: let rec make_list_1008 (n_1009:int) =
                let b_1618 = n_1009 < 0 in
                if b_1618 then
                  fun (x_1236:int) -> (false, 0)
                else
                  let r_f_1628 = rand_int () in
                  let n_1632 = n_1009 - 1 in
                  let r_make_list_1634 = make_list_1008 n_1632 in
                  fun (i_1226:int) ->
                    (let b_1635 = i_1226 = 0 in
                     if b_1635 then
                       (true, r_f_1628)
                     else
                       let n_1645 = i_1226 - 1 in
                       let r_xs_1647 = r_make_list_1634 n_1645 in
                       r_xs_1647)
              in
              let rec append_1165 (xs__ys_1023:((int -> (bool * int)) * (int -> (bool * int)))) =
                let xs_1011 = fst xs__ys_1023 in
                let ys_1012 = snd xs__ys_1023 in
                let r_xs_1655 = xs_1011 0 in
                let r_xs_0_1657 = fst r_xs_1655 in
                let b_1651 = r_xs_0_1657 = false in
                if b_1651 then
                  let xs_1868 = fst xs__ys_1023 in
                  let ys_1869 = snd xs__ys_1023 in
                  (ys_1012, xs_1868, ys_1869)
                else
                  let r_xs_1667 = xs_1011 0 in
                  let r_xs_0_1669 = fst r_xs_1667 in
                  let b_1671 = r_xs_0_1669 = false in
                  let b_1663 = not b_1671 in
                  if b_1663 then
                    let xs'_1014 (x_1269:int) = let n_1675 = x_1269 + 1 in
                                                let r_xs_1677 = xs_1011 n_1675 in
                                                r_xs_1677 in
                    let r_xs_1681 = xs_1011 0 in
                    let x_1013 = snd r_xs_1681 in
                    let xs'__ys_1688 = (xs'_1014, ys_1012) in
                    let r_append_xs'__ys_1775 = append_1165 xs'__ys_1688 in
                    let r_append_1690 = #0 r_append_xs'__ys_1775 in
                    let rs'_1195 (i_1369:int) =
                      let b_1691 = i_1369 = 0 in
                      if b_1691 then
                        (true, x_1013)
                      else
                        let n_1701 = i_1369 - 1 in
                        let r_rs_1703 = r_append_1690 n_1701 in
                        r_rs_1703
                    in
                    let xs_1854 = fst xs__ys_1023 in
                    let ys_1855 = snd xs__ys_1023 in
                    (rs'_1195, xs_1854, ys_1855)
                  else
                    let bot_1820 = _|_ in
                    let xs_1823 = fst xs__ys_1023 in
                    let ys_1824 = snd xs__ys_1023 in
                    (bot_1820, xs_1823, ys_1824)
              in
              let main_1017 (i_1018:int) (n_1019:int) =
                let r_make_list_1726 = make_list_1008 n_1019 in
                let f_1732 (x_1560:int) = (false, 0) in
                let xs__f_1736 = (r_make_list_1726, f_1732) in
                let r_append_xs__f_1790 = append_1165 xs__f_1736 in
                let r_append_1738 = #0 r_append_xs__f_1790 in
                let r_ys_1743 = r_append_1738 i_1018 in
                let x_1_1750 = snd r_ys_1743 in
                let r_xs_1748 = r_make_list_1726 i_1018 in
                let x_1_1751 = snd r_xs_1748 in
                let b_1739 = x_1_1750 = x_1_1751 in
                if b_1739 then
                  ()
                else
                  let f_1752 = {fail} in
                  let r_f_1755 = f_1752 () in
                  r_f_1755
              in
              let r_f_1759 = rand_int () in
              let r_f_1763 = rand_int () in
              let r_main_1767 = main_1017 r_f_1759 in
              let r_main_1769 = r_main_1767 r_f_1763 in
              ()
move_proj: let rec make_list_1008 (n_1009:int) =
             let b_1618 = n_1009 < 0 in
             if b_1618 then
               fun (x_1236:int) -> (false, 0)
             else
               let r_f_1628 = rand_int () in
               let n_1632 = n_1009 - 1 in
               let r_make_list_1634 = make_list_1008 n_1632 in
               fun (i_1226:int) ->
                 (let b_1635 = i_1226 = 0 in
                  if b_1635 then
                    (true, r_f_1628)
                  else
                    let n_1645 = i_1226 - 1 in
                    let r_xs_1647 = r_make_list_1634 n_1645 in
                    r_xs_1647)
           in
           let rec append_1165 (xs__ys_1023:((int -> (bool * int)) * (int -> (bool * int)))) =
             let x1_1907 = fst xs__ys_1023 in
             let x2_1908 = snd xs__ys_1023 in
             let xs_1011 = x1_1907 in
             let ys_1012 = x2_1908 in
             let r_xs_1655 = xs_1011 0 in
             let x1_1905 = fst r_xs_1655 in
             let x2_1906 = snd r_xs_1655 in
             let b_1651 = x1_1905 = false in
             if b_1651 then
               let xs_1868 = x1_1907 in
               let ys_1869 = x2_1908 in
               (ys_1012, xs_1868, ys_1869)
             else
               let r_xs_1667 = xs_1011 0 in
               let x1_1903 = fst r_xs_1667 in
               let x2_1904 = snd r_xs_1667 in
               let b_1671 = x1_1903 = false in
               let b_1663 = not b_1671 in
               if b_1663 then
                 let xs'_1014 (x_1269:int) =
                   let n_1675 = x_1269 + 1 in
                   let r_xs_1677 = xs_1011 n_1675 in
                   let x1_1892 = fst r_xs_1677 in
                   let x2_1893 = snd r_xs_1677 in
                   r_xs_1677
                 in
                 let r_xs_1681 = xs_1011 0 in
                 let x1_1901 = fst r_xs_1681 in
                 let x2_1902 = snd r_xs_1681 in
                 let xs'__ys_1688 = (xs'_1014, ys_1012) in
                 let x1_1899 = fst xs'__ys_1688 in
                 let x2_1900 = snd xs'__ys_1688 in
                 let r_append_xs'__ys_1775 = append_1165 xs'__ys_1688 in
                 let r1_1896 = #0 r_append_xs'__ys_1775 in
                 let x2_1897 = #1 r_append_xs'__ys_1775 in
                 let x3_1898 = #2 r_append_xs'__ys_1775 in
                 let rs'_1195 (i_1369:int) =
                   let b_1691 = i_1369 = 0 in
                   if b_1691 then
                     (true, x2_1902)
                   else
                     let n_1701 = i_1369 - 1 in
                     let r_rs_1703 = r1_1896 n_1701 in
                     let x1_1894 = fst r_rs_1703 in
                     let x2_1895 = snd r_rs_1703 in
                     r_rs_1703
                 in
                 let xs_1854 = x1_1907 in
                 let ys_1855 = x2_1908 in
                 (rs'_1195, xs_1854, ys_1855)
               else
                 let bot_1820 = _|_ in
                 let xs_1823 = x1_1907 in
                 let ys_1824 = x2_1908 in
                 (bot_1820, xs_1823, ys_1824)
           in
           let main_1017 (i_1018:int) (n_1019:int) =
             let r_make_list_1726 = make_list_1008 n_1019 in
             let f_1732 (x_1560:int) = (false, 0) in
             let xs__f_1736 = (r_make_list_1726, f_1732) in
             let x1_1916 = fst xs__f_1736 in
             let x2_1917 = snd xs__f_1736 in
             let r_append_xs__f_1790 = append_1165 xs__f_1736 in
             let r1_1913 = #0 r_append_xs__f_1790 in
             let x2_1914 = #1 r_append_xs__f_1790 in
             let x3_1915 = #2 r_append_xs__f_1790 in
             let r_ys_1743 = r1_1913 i_1018 in
             let x1_1911 = fst r_ys_1743 in
             let x2_1912 = snd r_ys_1743 in
             let r_xs_1748 = r_make_list_1726 i_1018 in
             let x1_1909 = fst r_xs_1748 in
             let x2_1910 = snd r_xs_1748 in
             let b_1739 = x2_1912 = x2_1910 in
             if b_1739 then
               ()
             else
               let f_1752 = {fail} in
               let r_f_1755 = f_1752 () in
               r_f_1755
           in
           let r_f_1759 = rand_int () in
           let r_f_1763 = rand_int () in
           let r_main_1767 = main_1017 r_f_1759 in
           let r_main_1769 = r_main_1767 r_f_1763 in
           ()
inline_no_effect: let rec make_list_1008 (n_1009:int) =
                    let b_1618 = n_1009 < 0 in
                    if b_1618 then
                      fun (x_1236:int) -> (false, 0)
                    else
                      let r_f_1628 = rand_int () in
                      let n_1632 = n_1009 - 1 in
                      let r_make_list_1634 = make_list_1008 n_1632 in
                      fun (i_1226:int) ->
                        (let b_1635 = i_1226 = 0 in
                         if b_1635 then
                           (true, r_f_1628)
                         else
                           let n_1645 = i_1226 - 1 in
                           let r_xs_1647 = r_make_list_1634 n_1645 in
                           r_xs_1647)
                  in
                  let rec append_1165 (xs__ys_1023:((int -> (bool * int)) * (int -> (bool * int)))) =
                    let x1_1907 = fst xs__ys_1023 in
                    let x2_1908 = snd xs__ys_1023 in
                    let xs_1011 = x1_1907 in
                    let ys_1012 = x2_1908 in
                    let r_xs_1655 = xs_1011 0 in
                    let x1_1905 = fst r_xs_1655 in
                    let x2_1906 = snd r_xs_1655 in
                    let b_1651 = x1_1905 = false in
                    if b_1651 then
                      let xs_1868 = x1_1907 in
                      let ys_1869 = x2_1908 in
                      (ys_1012, xs_1868, ys_1869)
                    else
                      let r_xs_1667 = xs_1011 0 in
                      let x1_1903 = fst r_xs_1667 in
                      let x2_1904 = snd r_xs_1667 in
                      let b_1671 = x1_1903 = false in
                      let b_1663 = not b_1671 in
                      if b_1663 then
                        let xs'_1014 (x_1269:int) =
                          let n_1675 = x_1269 + 1 in
                          let r_xs_1677 = xs_1011 n_1675 in
                          let x1_1892 = fst r_xs_1677 in
                          let x2_1893 = snd r_xs_1677 in
                          r_xs_1677
                        in
                        let r_xs_1681 = xs_1011 0 in
                        let x1_1901 = fst r_xs_1681 in
                        let x2_1902 = snd r_xs_1681 in
                        let xs'__ys_1688 = (xs'_1014, ys_1012) in
                        let x1_1899 = fst xs'__ys_1688 in
                        let x2_1900 = snd xs'__ys_1688 in
                        let r_append_xs'__ys_1775 = append_1165 xs'__ys_1688 in
                        let r1_1896 = #0 r_append_xs'__ys_1775 in
                        let x2_1897 = #1 r_append_xs'__ys_1775 in
                        let x3_1898 = #2 r_append_xs'__ys_1775 in
                        let rs'_1195 (i_1369:int) =
                          let b_1691 = i_1369 = 0 in
                          if b_1691 then
                            (true, x2_1902)
                          else
                            let n_1701 = i_1369 - 1 in
                            let r_rs_1703 = r1_1896 n_1701 in
                            let x1_1894 = fst r_rs_1703 in
                            let x2_1895 = snd r_rs_1703 in
                            r_rs_1703
                        in
                        let xs_1854 = x1_1907 in
                        let ys_1855 = x2_1908 in
                        (rs'_1195, xs_1854, ys_1855)
                      else
                        let bot_1820 = _|_ in
                        let xs_1823 = x1_1907 in
                        let ys_1824 = x2_1908 in
                        (bot_1820, xs_1823, ys_1824)
                  in
                  let main_1017 (i_1018:int) (n_1019:int) =
                    let r_make_list_1726 = make_list_1008 n_1019 in
                    let f_1732 (x_1560:int) = (false, 0) in
                    let xs__f_1736 = (r_make_list_1726, f_1732) in
                    let x1_1916 = fst xs__f_1736 in
                    let x2_1917 = snd xs__f_1736 in
                    let r_append_xs__f_1790 = append_1165 xs__f_1736 in
                    let r1_1913 = #0 r_append_xs__f_1790 in
                    let x2_1914 = #1 r_append_xs__f_1790 in
                    let x3_1915 = #2 r_append_xs__f_1790 in
                    let r_ys_1743 = r1_1913 i_1018 in
                    let x1_1911 = fst r_ys_1743 in
                    let x2_1912 = snd r_ys_1743 in
                    let r_xs_1748 = r_make_list_1726 i_1018 in
                    let x1_1909 = fst r_xs_1748 in
                    let x2_1910 = snd r_xs_1748 in
                    let b_1739 = x2_1912 = x2_1910 in
                    if b_1739 then
                      ()
                    else
                      let f_1752 = {fail} in
                      let r_f_1755 = f_1752 () in
                      r_f_1755
                  in
                  let r_f_1759 = rand_int () in
                  let r_f_1763 = rand_int () in
                  let r_main_1767 = main_1017 r_f_1759 in
                  let r_main_1769 = r_main_1767 r_f_1763 in
                  ()
normalize_let: let rec make_list_1008 (n_1009:int) =
                 let b_1618 = let b_1919 = n_1009 < 0 in
                              b_1919 in
                 if b_1618 then
                   fun (x_1236:int) -> (let b_1934 = false in
                                        let b__n_1938 = (b_1934, 0) in
                                        b__n_1938)
                 else
                   let r_f_1628 = let f_1920 = rand_int in
                                  let r_f_1921 = f_1920 () in
                                  r_f_1921 in
                   let n_1632 = n_1009 - 1 in
                   let r_make_list_1634 = let r_make_list_1924 = make_list_1008 n_1632 in
                                          r_make_list_1924 in
                   fun (i_1226:int) ->
                     (let b_1635 = let b_1926 = i_1226 = 0 in
                                   b_1926 in
                      if b_1635 then
                        let b_1930 = true in
                        let b__r_f_1933 = (b_1930, r_f_1628) in
                        b__r_f_1933
                      else
                        let n_1645 = i_1226 - 1 in
                        let r_xs_1647 = let r_r_make_list_1929 = r_make_list_1634 n_1645 in
                                        r_r_make_list_1929 in
                        r_xs_1647)
               in
               let rec append_1165 (xs__ys_1023:((int -> (bool * int)) * (int -> (bool * int)))) =
                 let x1_1907 = let xs_1939 = fst xs__ys_1023 in
                               xs_1939 in
                 let x2_1908 = let ys_1940 = snd xs__ys_1023 in
                               ys_1940 in
                 let xs_1011 = x1_1907 in
                 let ys_1012 = x2_1908 in
                 let r_xs_1655 = let r_xs_1941 = xs_1011 0 in
                                 r_xs_1941 in
                 let x1_1905 = let r_xs_0_1942 = fst r_xs_1655 in
                               r_xs_0_1942 in
                 let x2_1906 = let r_xs_1_1943 = snd r_xs_1655 in
                               r_xs_1_1943 in
                 let b_1651 = let b_1944 = false in
                              let b_1945 = x1_1905 = b_1944 in
                              b_1945 in
                 if b_1651 then
                   let xs_1868 = x1_1907 in
                   let ys_1869 = x2_1908 in
                   let ys__xs__ys_1990 = (ys_1012, xs_1868, ys_1869) in
                   ys__xs__ys_1990
                 else
                   let r_xs_1667 = let r_xs_1946 = xs_1011 0 in
                                   r_xs_1946 in
                   let x1_1903 = let r_xs_0_1947 = fst r_xs_1667 in
                                 r_xs_0_1947 in
                   let x2_1904 = let r_xs_1_1948 = snd r_xs_1667 in
                                 r_xs_1_1948 in
                   let b_1671 = let b_1949 = false in
                                let b_1950 = x1_1903 = b_1949 in
                                b_1950 in
                   let b_1663 = not b_1671 in
                   if b_1663 then
                     let xs'_1014 (x_1269:int) =
                       let n_1675 = x_1269 + 1 in
                       let r_xs_1677 = let r_xs_1957 = xs_1011 n_1675 in
                                       r_xs_1957 in
                       let x1_1892 = let r_xs_0_1958 = fst r_xs_1677 in
                                     r_xs_0_1958 in
                       let x2_1893 = let r_xs_1_1959 = snd r_xs_1677 in
                                     r_xs_1_1959 in
                       r_xs_1677
                     in
                     let r_xs_1681 = let r_xs_1960 = xs_1011 0 in
                                     r_xs_1960 in
                     let x1_1901 = let r_xs_0_1961 = fst r_xs_1681 in
                                   r_xs_0_1961 in
                     let x2_1902 = let r_xs_1_1962 = snd r_xs_1681 in
                                   r_xs_1_1962 in
                     let xs'__ys_1688 = let xs'__ys_1965 = (xs'_1014, ys_1012) in
                                        xs'__ys_1965 in
                     let x1_1899 = let xs'_1966 = fst xs'__ys_1688 in
                                   xs'_1966 in
                     let x2_1900 = let ys_1967 = snd xs'__ys_1688 in
                                   ys_1967 in
                     let r_append_xs'__ys_1775 = let r_append_1968 = append_1165 xs'__ys_1688 in
                                                 r_append_1968 in
                     let r1_1896 = let r_append_xs'__ys_0_1969 = #0 r_append_xs'__ys_1775 in
                                   r_append_xs'__ys_0_1969 in
                     let x2_1897 = let r_append_xs'__ys_1_1970 = #1 r_append_xs'__ys_1775 in
                                   r_append_xs'__ys_1_1970 in
                     let x3_1898 = let r_append_xs'__ys_2_1971 = #2 r_append_xs'__ys_1775 in
                                   r_append_xs'__ys_2_1971 in
                     let rs'_1195 (i_1369:int) =
                       let b_1691 = let b_1973 = i_1369 = 0 in
                                    b_1973 in
                       if b_1691 then
                         let b_1979 = true in
                         let b__x2_1982 = (b_1979, x2_1902) in
                         b__x2_1982
                       else
                         let n_1701 = i_1369 - 1 in
                         let r_rs_1703 = let r_r1_1976 = r1_1896 n_1701 in
                                         r_r1_1976 in
                         let x1_1894 = let r_rs_0_1977 = fst r_rs_1703 in
                                       r_rs_0_1977 in
                         let x2_1895 = let r_rs_1_1978 = snd r_rs_1703 in
                                       r_rs_1_1978 in
                         r_rs_1703
                     in
                     let xs_1854 = x1_1907 in
                     let ys_1855 = x2_1908 in
                     let rs'__xs__ys_1986 = (rs'_1195, xs_1854, ys_1855) in
                     rs'__xs__ys_1986
                   else
                     let bot_1820 = _|_ in
                     let xs_1823 = x1_1907 in
                     let ys_1824 = x2_1908 in
                     let bot__xs__ys_1954 = (bot_1820, xs_1823, ys_1824) in
                     bot__xs__ys_1954
               in
               let main_1017 (i_1018:int) (n_1019:int) =
                 let r_make_list_1726 = let r_make_list_1991 = make_list_1008 n_1019 in
                                        r_make_list_1991 in
                 let f_1732 (x_1560:int) = let b_1992 = false in
                                           let b__n_1996 = (b_1992, 0) in
                                           b__n_1996 in
                 let xs__f_1736 = let r_make_list__f_1999 = (r_make_list_1726, f_1732) in
                                  r_make_list__f_1999 in
                 let x1_1916 = let xs_2000 = fst xs__f_1736 in
                               xs_2000 in
                 let x2_1917 = let f_2001 = snd xs__f_1736 in
                               f_2001 in
                 let r_append_xs__f_1790 = let r_append_2002 = append_1165 xs__f_1736 in
                                           r_append_2002 in
                 let r1_1913 = let r_append_xs__f_0_2003 = #0 r_append_xs__f_1790 in
                               r_append_xs__f_0_2003 in
                 let x2_1914 = let r_append_xs__f_1_2004 = #1 r_append_xs__f_1790 in
                               r_append_xs__f_1_2004 in
                 let x3_1915 = let r_append_xs__f_2_2005 = #2 r_append_xs__f_1790 in
                               r_append_xs__f_2_2005 in
                 let r_ys_1743 = let r_r1_2006 = r1_1913 i_1018 in
                                 r_r1_2006 in
                 let x1_1911 = let r_ys_0_2007 = fst r_ys_1743 in
                               r_ys_0_2007 in
                 let x2_1912 = let r_ys_1_2008 = snd r_ys_1743 in
                               r_ys_1_2008 in
                 let r_xs_1748 = let r_r_make_list_2009 = r_make_list_1726 i_1018 in
                                 r_r_make_list_2009 in
                 let x1_1909 = let r_xs_0_2010 = fst r_xs_1748 in
                               r_xs_0_2010 in
                 let x2_1910 = let r_xs_1_2011 = snd r_xs_1748 in
                               r_xs_1_2011 in
                 let b_1739 = let b_2012 = x2_1912 = x2_1910 in
                              b_2012 in
                 if b_1739 then
                   ()
                 else
                   let f_1752 = {fail} in
                   let r_f_1755 = let r_f_2013 = f_1752 () in
                                  r_f_2013 in
                   r_f_1755
               in
               let r_f_1759 = let f_2014 = rand_int in
                              let r_f_2015 = f_2014 () in
                              r_f_2015 in
               let r_f_1763 = let f_2016 = rand_int in
                              let r_f_2017 = f_2016 () in
                              r_f_2017 in
               let r_main_1767 = let r_main_2018 = main_1017 r_f_1759 in
                                 r_main_2018 in
               let r_main_1769 = let r_r_main_2019 = r_main_1767 r_f_1763 in
                                 r_r_main_2019 in
               ()
flatten_let: let rec make_list_1008 (n_1009:int) =
               let b_1919 = n_1009 < 0 in
               if b_1919 then
                 fun (x_1236:int) -> (let b__n_1938 = (false, 0) in
                                      b__n_1938)
               else
                 let r_f_1921 = rand_int () in
                 let n_1632 = n_1009 - 1 in
                 let r_make_list_1924 = make_list_1008 n_1632 in
                 fun (i_1226:int) ->
                   (let b_1926 = i_1226 = 0 in
                    if b_1926 then
                      let b__r_f_1933 = (true, r_f_1921) in
                      b__r_f_1933
                    else
                      let n_1645 = i_1226 - 1 in
                      let r_r_make_list_1929 = r_make_list_1924 n_1645 in
                      r_r_make_list_1929)
             in
             let rec append_1165 (xs__ys_1023:((int -> (bool * int)) * (int -> (bool * int)))) =
               let xs_1939 = fst xs__ys_1023 in
               let ys_1940 = snd xs__ys_1023 in
               let r_xs_1941 = xs_1939 0 in
               let r_xs_0_1942 = fst r_xs_1941 in
               let r_xs_1_1943 = snd r_xs_1941 in
               let b_1945 = r_xs_0_1942 = false in
               if b_1945 then
                 let ys__xs__ys_1990 = (ys_1940, xs_1939, ys_1940) in
                 ys__xs__ys_1990
               else
                 let r_xs_1946 = xs_1939 0 in
                 let r_xs_0_1947 = fst r_xs_1946 in
                 let r_xs_1_1948 = snd r_xs_1946 in
                 let b_1950 = r_xs_0_1947 = false in
                 let b_1663 = not b_1950 in
                 if b_1663 then
                   let xs'_1014 (x_1269:int) =
                     let n_1675 = x_1269 + 1 in
                     let r_xs_1957 = xs_1939 n_1675 in
                     let r_xs_0_1958 = fst r_xs_1957 in
                     let r_xs_1_1959 = snd r_xs_1957 in
                     r_xs_1957
                   in
                   let r_xs_1960 = xs_1939 0 in
                   let r_xs_0_1961 = fst r_xs_1960 in
                   let r_xs_1_1962 = snd r_xs_1960 in
                   let xs'__ys_1965 = (xs'_1014, ys_1940) in
                   let xs'_1966 = fst xs'__ys_1965 in
                   let ys_1967 = snd xs'__ys_1965 in
                   let r_append_1968 = append_1165 xs'__ys_1965 in
                   let r_append_xs'__ys_0_1969 = #0 r_append_1968 in
                   let r_append_xs'__ys_1_1970 = #1 r_append_1968 in
                   let r_append_xs'__ys_2_1971 = #2 r_append_1968 in
                   let rs'_1195 (i_1369:int) =
                     let b_1973 = i_1369 = 0 in
                     if b_1973 then
                       let b__x2_1982 = (true, r_xs_1_1962) in
                       b__x2_1982
                     else
                       let n_1701 = i_1369 - 1 in
                       let r_r1_1976 = r_append_xs'__ys_0_1969 n_1701 in
                       let r_rs_0_1977 = fst r_r1_1976 in
                       let r_rs_1_1978 = snd r_r1_1976 in
                       r_r1_1976
                   in
                   let rs'__xs__ys_1986 = (rs'_1195, xs_1939, ys_1940) in
                   rs'__xs__ys_1986
                 else
                   let bot_1820 = _|_ in
                   let bot__xs__ys_1954 = (bot_1820, xs_1939, ys_1940) in
                   bot__xs__ys_1954
             in
             let main_1017 (i_1018:int) (n_1019:int) =
               let r_make_list_1991 = make_list_1008 n_1019 in
               let f_1732 (x_1560:int) = let b__n_1996 = (false, 0) in
                                         b__n_1996 in
               let r_make_list__f_1999 = (r_make_list_1991, f_1732) in
               let xs_2000 = fst r_make_list__f_1999 in
               let f_2001 = snd r_make_list__f_1999 in
               let r_append_2002 = append_1165 r_make_list__f_1999 in
               let r_append_xs__f_0_2003 = #0 r_append_2002 in
               let r_append_xs__f_1_2004 = #1 r_append_2002 in
               let r_append_xs__f_2_2005 = #2 r_append_2002 in
               let r_r1_2006 = r_append_xs__f_0_2003 i_1018 in
               let r_ys_0_2007 = fst r_r1_2006 in
               let r_ys_1_2008 = snd r_r1_2006 in
               let r_r_make_list_2009 = r_make_list_1991 i_1018 in
               let r_xs_0_2010 = fst r_r_make_list_2009 in
               let r_xs_1_2011 = snd r_r_make_list_2009 in
               let b_2012 = r_ys_1_2008 = r_xs_1_2011 in
               if b_2012 then
                 ()
               else
                 let f_1752 = {fail} in
                 let r_f_2013 = f_1752 () in
                 r_f_2013
             in
             let r_f_2015 = rand_int () in
             let r_f_2017 = rand_int () in
             let r_main_2018 = main_1017 r_f_2015 in
             let r_r_main_2019 = r_main_2018 r_f_2017 in
             ()
sort_let_pair: let rec make_list_1008 (n_1009:int) =
                 let b_1919 = n_1009 < 0 in
                 if b_1919 then
                   fun (x_1236:int) -> (let b__n_1938 = (false, 0) in
                                        b__n_1938)
                 else
                   let r_f_1921 = rand_int () in
                   let n_1632 = n_1009 - 1 in
                   let r_make_list_1924 = make_list_1008 n_1632 in
                   fun (i_1226:int) ->
                     (let b_1926 = i_1226 = 0 in
                      if b_1926 then
                        let b__r_f_1933 = (true, r_f_1921) in
                        b__r_f_1933
                      else
                        let n_1645 = i_1226 - 1 in
                        let r_r_make_list_1929 = r_make_list_1924 n_1645 in
                        r_r_make_list_1929)
               in
               let rec append_1165 (xs__ys_1023:((int -> (bool * int)) * (int -> (bool * int)))) =
                 let xs_1939 = fst xs__ys_1023 in
                 let ys_1940 = snd xs__ys_1023 in
                 let r_xs_1941 = xs_1939 0 in
                 let r_xs_0_1942 = fst r_xs_1941 in
                 let r_xs_1_1943 = snd r_xs_1941 in
                 let b_1945 = r_xs_0_1942 = false in
                 if b_1945 then
                   let ys__xs__ys_1990 = (ys_1940, xs_1939, ys_1940) in
                   ys__xs__ys_1990
                 else
                   let r_xs_1946 = xs_1939 0 in
                   let r_xs_0_1947 = fst r_xs_1946 in
                   let r_xs_1_1948 = snd r_xs_1946 in
                   let b_1950 = r_xs_0_1947 = false in
                   let b_1663 = not b_1950 in
                   if b_1663 then
                     let xs'_1014 (x_1269:int) =
                       let n_1675 = x_1269 + 1 in
                       let r_xs_1957 = xs_1939 n_1675 in
                       let r_xs_0_1958 = fst r_xs_1957 in
                       let r_xs_1_1959 = snd r_xs_1957 in
                       r_xs_1957
                     in
                     let r_xs_1960 = xs_1939 0 in
                     let r_xs_0_1961 = fst r_xs_1960 in
                     let r_xs_1_1962 = snd r_xs_1960 in
                     let xs'__ys_1965 = (xs'_1014, ys_1940) in
                     let xs'_1966 = fst xs'__ys_1965 in
                     let ys_1967 = snd xs'__ys_1965 in
                     let r_append_1968 = append_1165 xs'__ys_1965 in
                     let r_append_xs'__ys_0_1969 = #0 r_append_1968 in
                     let r_append_xs'__ys_1_1970 = #1 r_append_1968 in
                     let r_append_xs'__ys_2_1971 = #2 r_append_1968 in
                     let rs'_1195 (i_1369:int) =
                       let b_1973 = i_1369 = 0 in
                       if b_1973 then
                         let b__x2_1982 = (true, r_xs_1_1962) in
                         b__x2_1982
                       else
                         let n_1701 = i_1369 - 1 in
                         let r_r1_1976 = r_append_xs'__ys_0_1969 n_1701 in
                         let r_rs_0_1977 = fst r_r1_1976 in
                         let r_rs_1_1978 = snd r_r1_1976 in
                         r_r1_1976
                     in
                     let rs'__xs__ys_1986 = (rs'_1195, xs_1939, ys_1940) in
                     rs'__xs__ys_1986
                   else
                     let bot_1820 = _|_ in
                     let bot__xs__ys_1954 = (bot_1820, xs_1939, ys_1940) in
                     bot__xs__ys_1954
               in
               let main_1017 (i_1018:int) (n_1019:int) =
                 let r_make_list_1991 = make_list_1008 n_1019 in
                 let f_1732 (x_1560:int) = let b__n_1996 = (false, 0) in
                                           b__n_1996 in
                 let r_make_list__f_1999 = (r_make_list_1991, f_1732) in
                 let xs_2000 = fst r_make_list__f_1999 in
                 let f_2001 = snd r_make_list__f_1999 in
                 let r_append_2002 = append_1165 r_make_list__f_1999 in
                 let r_append_xs__f_0_2003 = #0 r_append_2002 in
                 let r_append_xs__f_1_2004 = #1 r_append_2002 in
                 let r_append_xs__f_2_2005 = #2 r_append_2002 in
                 let r_r1_2006 = r_append_xs__f_0_2003 i_1018 in
                 let r_ys_0_2007 = fst r_r1_2006 in
                 let r_ys_1_2008 = snd r_r1_2006 in
                 let r_r_make_list_2009 = r_make_list_1991 i_1018 in
                 let r_xs_0_2010 = fst r_r_make_list_2009 in
                 let r_xs_1_2011 = snd r_r_make_list_2009 in
                 let b_2012 = r_ys_1_2008 = r_xs_1_2011 in
                 if b_2012 then
                   ()
                 else
                   let f_1752 = {fail} in
                   let r_f_2013 = f_1752 () in
                   r_f_2013
               in
               let r_f_2015 = rand_int () in
               let r_f_2017 = rand_int () in
               let r_main_2018 = main_1017 r_f_2015 in
               let r_r_main_2019 = r_main_2018 r_f_2017 in
               ()
x: r_main_2018, y': x_2020
THIS IS ROOT
x: main_1017, y': x_2021
THIS IS ROOT
x: f_1752, y': x_2195
THIS IS ROOT
x: r_make_list_1991, y': i_2196
THIS IS ROOT
x: r_append_xs__f_0_2003, y': i_2197
THIS IS NOT ROOT
make_tree: (r_append_2002:((int -> (bool * int)) * (int -> (bool * int)) * (int -> (bool * int))))
make_tree: (r_append_xs__f_0_2003:(int -> (bool * int)))
make_tree: (r_append_xs__f_1_2004:(int -> (bool * int)))
make_tree: (r_append_xs__f_2_2005:(int -> (bool * int)))
y': i_2197
path: [0]
TREE: [[(i_1018:int)];[];[]]
TREE': [[(i_2197:int)];[];[]]
r': r_append_2002:(((bool * int) * (bool * int) * (bool * int)) ->
                     ((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))))
|trees|': 1
  tree: [(true, i_2197);(false, 0);(false, 0)]
x: append_1165, y': x_2257
THIS IS ROOT
x: make_list_1008, y': x_2302
THIS IS ROOT
x: r_append_xs'__ys_0_1969, y': i_2856
THIS IS NOT ROOT
make_tree: (r_append_1968:((int -> (bool * int)) * (int -> (bool * int)) * (int -> (bool * int))))
make_tree: (r_append_xs'__ys_0_1969:(int -> (bool * int)))
make_tree: (r_append_xs'__ys_1_1970:(int -> (bool * int)))
make_tree: (r_append_xs'__ys_2_1971:(int -> (bool * int)))
y': i_2856
path: [0]
TREE: [[(n_1701:int)];[];[]]
TREE': [[(i_2856:int)];[];[]]
r': r_append_1968:(((bool * int) * (bool * int) * (bool * int)) ->
                     ((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))))
|trees|': 1
  tree: [(true, i_2856);(false, 0);(false, 0)]
x: append_1165, y': x_2916
THIS IS ROOT
x: xs_1939, y': i_2961
THIS IS NOT ROOT
make_tree: (xs__ys_1023:((int -> (bool * int)) * (int -> (bool * int))))
make_tree: (xs_1939:(int -> (bool * int)))
make_tree: (ys_1940:(int -> (bool * int)))
y': i_2961
path: [0]
TREE: [[(0:int)];[]]
TREE': [[(i_2961:int)];[]]
r': xs__ys_1023:(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int))))
|trees|': 1
  tree: [(true, i_2961);(false, 0)]
x: xs_1939, y': i_2982
THIS IS NOT ROOT
make_tree: (xs__ys_1023:((int -> (bool * int)) * (int -> (bool * int))))
make_tree: (xs_1939:(int -> (bool * int)))
make_tree: (ys_1940:(int -> (bool * int)))
y': i_2982
path: [0]
TREE: [[(n_1675:int); (0:int)];[]]
TREE': [[(i_2982:int)];[]]
r': xs__ys_1023:(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int))))
|trees|': 1
  tree: [(true, i_2982);(false, 0)]
x: xs_1939, y': i_3003
THIS IS NOT ROOT
make_tree: (xs__ys_1023:((int -> (bool * int)) * (int -> (bool * int))))
make_tree: (xs_1939:(int -> (bool * int)))
make_tree: (ys_1940:(int -> (bool * int)))
y': i_3003
path: [0]
TREE: [[(0:int)];[]]
TREE': [[(i_3003:int)];[]]
r': xs__ys_1023:(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int))))
|trees|': 1
  tree: [(true, i_3003);(false, 0)]
x: xs_1939, y': i_3125
THIS IS NOT ROOT
make_tree: (xs__ys_1023:((int -> (bool * int)) * (int -> (bool * int))))
make_tree: (xs_1939:(int -> (bool * int)))
make_tree: (ys_1940:(int -> (bool * int)))
y': i_3125
path: [0]
TREE: [[(0:int)];[]]
TREE': [[(i_3125:int)];[]]
r': xs__ys_1023:(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int))))
|trees|': 1
  tree: [(true, i_3125);(false, 0)]
x: r_make_list_1924, y': i_3205
THIS IS ROOT
x: make_list_1008, y': x_3206
THIS IS ROOT
ref_trans: let rec make_list_1008 n_1009 =
             if n_1009 < 0 then
               fun x_1236 -> (false, 0)
             else
               let r_f_1921 = rand_int () in
               let r_make_list_1924 = make_list_1008 (n_1009 - 1) in
               fun i_1226 -> (if i_1226 = 0 then
                                (true, r_f_1921)
                              else
                                r_make_list_1924 (i_1226 - 1))
           in
           let rec append_1165 xs__ys_1023 =
             let xs_1939 i_3153 = snd (fst (xs__ys_1023 ((true, i_3153), (false, 0)))) in
             let ys_1940 i_3146 = snd (snd (xs__ys_1023 ((false, 0), (true, i_3146)))) in
             let r_xs_1941 = let r_xs__ys_3145 = xs__ys_1023 ((true, 0), (false, 0)) in
                             snd (fst r_xs__ys_3145) in
             let r_xs_1_1943 = snd r_xs_1941 in
             if fst r_xs_1941 = false then
               let ys__xs__ys_1990 iii_3100 =
                 ((if fst (#0 iii_3100) = false then
                     (false, (true, 0))
                   else
                     (true, ys_1940 (snd (#0 iii_3100)))),
                  (if fst (#1 iii_3100) = false then
                     (false, (true, 0))
                   else
                     (true, xs_1939 (snd (#1 iii_3100)))),
                  (if fst (#2 iii_3100) = false then
                     (false, (true, 0))
                   else
                     (true, ys_1940 (snd (#2 iii_3100)))))
               in
               ys__xs__ys_1990
             else
               let r_xs_1946 = let r_xs__ys_3023 = xs__ys_1023 ((true, 0), (false, 0)) in
                               snd (fst r_xs__ys_3023) in
               let r_xs_1_1948 = snd r_xs_1946 in
               if fst r_xs_1946 <> false then
                 let xs'_1014 x_1269 =
                   let r_xs_1957 =
                     let r_xs__ys_3002 = xs__ys_1023 ((true, x_1269 + 1), (false, 0)) in
                     snd (fst r_xs__ys_3002)
                   in
                   let r_xs_0_1958 = fst r_xs_1957 in
                   let r_xs_1_1959 = snd r_xs_1957 in
                   r_xs_1957
                 in
                 let r_xs_1960 = let r_xs__ys_2981 = xs__ys_1023 ((true, 0), (false, 0)) in
                                 snd (fst r_xs__ys_2981) in
                 let r_xs_0_1961 = fst r_xs_1960 in
                 let xs'__ys_1965 ii_2944 =
                   ((if fst (fst ii_2944) = false then
                       (false, (true, 0))
                     else
                       (true, xs'_1014 (snd (fst ii_2944)))),
                    (if fst (snd ii_2944) = false then
                       (false, (true, 0))
                     else
                       (true, ys_1940 (snd (snd ii_2944)))))
                 in
                 let xs'_1966 i_2924 = snd (fst (xs'__ys_1965 ((true, i_2924), (false, 0)))) in
                 let ys_1967 i_2917 = snd (snd (xs'__ys_1965 ((false, 0), (true, i_2917)))) in
                 let r_append_1968 = append_1165 xs'__ys_1965 in
                 let r_append_xs'__ys_0_1969 i_2906 = snd (#0 (r_append_1968 ((true, i_2906), (false, 0), (false, 0)))) in
                 let r_append_xs'__ys_1_1970 i_2896 = snd (#1 (r_append_1968 ((false, 0), (true, i_2896), (false, 0)))) in
                 let r_append_xs'__ys_2_1971 i_2886 = snd (#2 (r_append_1968 ((false, 0), (false, 0), (true, i_2886)))) in
                 let rs'_1195 i_1369 =
                   if i_1369 = 0 then
                     (true, snd r_xs_1960)
                   else
                     let r_r1_1976 =
                       let r_r_append_2885 = r_append_1968 ((true, i_1369 - 1), (false, 0), (false, 0)) in
                       snd (#0 r_r_append_2885)
                     in
                     let r_rs_0_1977 = fst r_r1_1976 in
                     let r_rs_1_1978 = snd r_r1_1976 in
                     r_r1_1976
                 in
                 let rs'__xs__ys_1986 iii_2831 =
                   ((if fst (#0 iii_2831) = false then
                       (false, (true, 0))
                     else
                       (true, rs'_1195 (snd (#0 iii_2831)))),
                    (if fst (#1 iii_2831) = false then
                       (false, (true, 0))
                     else
                       (true, xs_1939 (snd (#1 iii_2831)))),
                    (if fst (#2 iii_2831) = false then
                       (false, (true, 0))
                     else
                       (true, ys_1940 (snd (#2 iii_2831)))))
                 in
                 rs'__xs__ys_1986
               else
                 let bot_1820 = _|_ in
                 let bot__xs__ys_1954 iii_2519 =
                   ((if fst (#0 iii_2519) = false then
                       (false, (true, 0))
                     else
                       (true, bot_1820 (snd (#0 iii_2519)))),
                    (if fst (#1 iii_2519) = false then
                       (false, (true, 0))
                     else
                       (true, xs_1939 (snd (#1 iii_2519)))),
                    (if fst (#2 iii_2519) = false then
                       (false, (true, 0))
                     else
                       (true, ys_1940 (snd (#2 iii_2519)))))
                 in
                 bot__xs__ys_1954
           in
           let main_1017 i_1018 n_1019 =
             let r_make_list_1991 = make_list_1008 n_1019 in
             let f_1732 x_1560 = (false, 0) in
             let r_make_list__f_1999 ix_2285 =
               ((if fst (fst ix_2285) = false then
                   (false, (true, 0))
                 else
                   (true, r_make_list_1991 (snd (fst ix_2285)))),
                (if fst (snd ix_2285) = false then
                   (false, (true, 0))
                 else
                   (true, f_1732 (snd (snd ix_2285)))))
             in
             let xs_2000 i_2265 = snd (fst (r_make_list__f_1999 ((true, i_2265), (false, 0)))) in
             let f_2001 x_2258 = snd (snd (r_make_list__f_1999 ((false, 0), (true, x_2258)))) in
             let r_append_2002 = append_1165 r_make_list__f_1999 in
             let r_append_xs__f_0_2003 i_2247 = snd (#0 (r_append_2002 ((true, i_2247), (false, 0), (false, 0)))) in
             let r_append_xs__f_1_2004 i_2237 = snd (#1 (r_append_2002 ((false, 0), (true, i_2237), (false, 0)))) in
             let r_append_xs__f_2_2005 i_2227 = snd (#2 (r_append_2002 ((false, 0), (false, 0), (true, i_2227)))) in
             let r_r1_2006 =
               let r_r_append_2226 = r_append_2002 ((true, i_1018), (false, 0), (false, 0)) in
               snd (#0 r_r_append_2226)
             in
             let r_ys_0_2007 = fst r_r1_2006 in
             let r_r_make_list_2009 = r_make_list_1991 i_1018 in
             let r_xs_0_2010 = fst r_r_make_list_2009 in
             if snd r_r1_2006 = snd r_r_make_list_2009 then
               ()
             else
               {fail} ()
           in
           let r_f_2015 = rand_int () in
           let r_f_2017 = rand_int () in
           let r_main_2018 = main_1017 r_f_2015 in
           let r_r_main_2019 = r_main_2018 r_f_2017 in
           ()
ref_trans:
 let rec make_list_1008 (n_1009:int) =
   if n_1009 < 0 then
     fun (x_1236:int) -> (false, 0)
   else
     let r_f_1921 = rand_int () in
     let r_make_list_1924 = make_list_1008 (n_1009 - 1) in
     fun (i_1226:int) -> (if i_1226 = 0 then
                            (true, r_f_1921)
                          else
                            r_make_list_1924 (i_1226 - 1))
 in
 let rec append_1165 (xs__ys_1023:(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int))))) =
   let xs_1939 (i_3153:int) = snd (fst (xs__ys_1023 ((true, i_3153), (false, 0)))) in
   let ys_1940 (i_3146:int) = snd (snd (xs__ys_1023 ((false, 0), (true, i_3146)))) in
   let r_xs_1941 = let r_xs__ys_3145 = xs__ys_1023 ((true, 0), (false, 0)) in
                   snd (fst r_xs__ys_3145) in
   let r_xs_1_1943 = snd r_xs_1941 in
   if fst r_xs_1941 = false then
     let ys__xs__ys_1990 (iii_3100:((bool * int) * (bool * int) * (bool * int))) =
       ((if fst (#0 iii_3100) = false then
           (false, (true, 0))
         else
           (true, ys_1940 (snd (#0 iii_3100)))),
        (if fst (#1 iii_3100) = false then
           (false, (true, 0))
         else
           (true, xs_1939 (snd (#1 iii_3100)))),
        (if fst (#2 iii_3100) = false then
           (false, (true, 0))
         else
           (true, ys_1940 (snd (#2 iii_3100)))))
     in
     ys__xs__ys_1990
   else
     let r_xs_1946 = let r_xs__ys_3023 = xs__ys_1023 ((true, 0), (false, 0)) in
                     snd (fst r_xs__ys_3023) in
     let r_xs_1_1948 = snd r_xs_1946 in
     if fst r_xs_1946 <> false then
       let xs'_1014 (x_1269:int) =
         let r_xs_1957 = let r_xs__ys_3002 = xs__ys_1023 ((true, x_1269 + 1), (false, 0)) in
                         snd (fst r_xs__ys_3002) in
         let r_xs_0_1958 = fst r_xs_1957 in
         let r_xs_1_1959 = snd r_xs_1957 in
         r_xs_1957
       in
       let r_xs_1960 = let r_xs__ys_2981 = xs__ys_1023 ((true, 0), (false, 0)) in
                       snd (fst r_xs__ys_2981) in
       let r_xs_0_1961 = fst r_xs_1960 in
       let xs'__ys_1965 (ii_2944:((bool * int) * (bool * int))) =
         ((if fst (fst ii_2944) = false then
             (false, (true, 0))
           else
             (true, xs'_1014 (snd (fst ii_2944)))),
          (if fst (snd ii_2944) = false then
             (false, (true, 0))
           else
             (true, ys_1940 (snd (snd ii_2944)))))
       in
       let xs'_1966 (i_2924:int) = snd (fst (xs'__ys_1965 ((true, i_2924), (false, 0)))) in
       let ys_1967 (i_2917:int) = snd (snd (xs'__ys_1965 ((false, 0), (true, i_2917)))) in
       let r_append_1968 = append_1165 xs'__ys_1965 in
       let r_append_xs'__ys_0_1969 (i_2906:int) = snd (#0 (r_append_1968 ((true, i_2906), (false, 0), (false, 0)))) in
       let r_append_xs'__ys_1_1970 (i_2896:int) = snd (#1 (r_append_1968 ((false, 0), (true, i_2896), (false, 0)))) in
       let r_append_xs'__ys_2_1971 (i_2886:int) = snd (#2 (r_append_1968 ((false, 0), (false, 0), (true, i_2886)))) in
       let rs'_1195 (i_1369:int) =
         if i_1369 = 0 then
           (true, snd r_xs_1960)
         else
           let r_r1_1976 =
             let r_r_append_2885 = r_append_1968 ((true, i_1369 - 1), (false, 0), (false, 0)) in
             snd (#0 r_r_append_2885)
           in
           let r_rs_0_1977 = fst r_r1_1976 in
           let r_rs_1_1978 = snd r_r1_1976 in
           r_r1_1976
       in
       let rs'__xs__ys_1986 (iii_2831:((bool * int) * (bool * int) * (bool * int))) =
         ((if fst (#0 iii_2831) = false then
             (false, (true, 0))
           else
             (true, rs'_1195 (snd (#0 iii_2831)))),
          (if fst (#1 iii_2831) = false then
             (false, (true, 0))
           else
             (true, xs_1939 (snd (#1 iii_2831)))),
          (if fst (#2 iii_2831) = false then
             (false, (true, 0))
           else
             (true, ys_1940 (snd (#2 iii_2831)))))
       in
       rs'__xs__ys_1986
     else
       let bot_1820 = _|_ in
       let bot__xs__ys_1954 (iii_2519:((bool * int) * (bool * int) * (bool * int))) =
         ((if fst (#0 iii_2519) = false then
             (false, (true, 0))
           else
             (true, bot_1820 (snd (#0 iii_2519)))),
          (if fst (#1 iii_2519) = false then
             (false, (true, 0))
           else
             (true, xs_1939 (snd (#1 iii_2519)))),
          (if fst (#2 iii_2519) = false then
             (false, (true, 0))
           else
             (true, ys_1940 (snd (#2 iii_2519)))))
       in
       bot__xs__ys_1954
 in
 let main_1017 (i_1018:int) (n_1019:int) =
   let r_make_list_1991 = make_list_1008 n_1019 in
   let f_1732 (x_1560:int) = (false, 0) in
   let r_make_list__f_1999 (ix_2285:((bool * int) * (bool * int))) =
     ((if fst (fst ix_2285) = false then
         (false, (true, 0))
       else
         (true, r_make_list_1991 (snd (fst ix_2285)))),
      (if fst (snd ix_2285) = false then
         (false, (true, 0))
       else
         (true, f_1732 (snd (snd ix_2285)))))
   in
   let xs_2000 (i_2265:int) = snd (fst (r_make_list__f_1999 ((true, i_2265), (false, 0)))) in
   let f_2001 (x_2258:int) = snd (snd (r_make_list__f_1999 ((false, 0), (true, x_2258)))) in
   let r_append_2002 = append_1165 r_make_list__f_1999 in
   let r_append_xs__f_0_2003 (i_2247:int) = snd (#0 (r_append_2002 ((true, i_2247), (false, 0), (false, 0)))) in
   let r_append_xs__f_1_2004 (i_2237:int) = snd (#1 (r_append_2002 ((false, 0), (true, i_2237), (false, 0)))) in
   let r_append_xs__f_2_2005 (i_2227:int) = snd (#2 (r_append_2002 ((false, 0), (false, 0), (true, i_2227)))) in
   let r_r1_2006 =
     let r_r_append_2226 = r_append_2002 ((true, i_1018), (false, 0), (false, 0)) in
     snd (#0 r_r_append_2226)
   in
   let r_ys_0_2007 = fst r_r1_2006 in
   let r_r_make_list_2009 = r_make_list_1991 i_1018 in
   let r_xs_0_2010 = fst r_r_make_list_2009 in
   if snd r_r1_2006 = snd r_r_make_list_2009 then
     ()
   else
     {fail} ()
 in
 let r_f_2015 = rand_int () in
 let r_f_2017 = rand_int () in
 let r_main_2018 = main_1017 r_f_2015 in
 let r_r_main_2019 = r_main_2018 r_f_2017 in
 ()

inline_wrapped:
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1236 -> (false, 0)
  else
    let r_f_1921 = rand_int () in
    let r_make_list_1924 = make_list_1008 (n_1009 - 1) in
    fun i_1226 -> (if i_1226 = 0 then
                     (true, r_f_1921)
                   else
                     r_make_list_1924 (i_1226 - 1))
in
let rec append_1165 xs__ys_1023 =
  let xs_1939 i_3153 = snd (fst (xs__ys_1023 ((true, i_3153), (false, 0)))) in
  let ys_1940 i_3146 = snd (snd (xs__ys_1023 ((false, 0), (true, i_3146)))) in
  let r_xs_1941 = let r_xs__ys_3145 = xs__ys_1023 ((true, 0), (false, 0)) in
                  snd (fst r_xs__ys_3145) in
  let r_xs_1_1943 = snd r_xs_1941 in
  if fst r_xs_1941 = false then
    let ys__xs__ys_1990 iii_3100 =
      if fst (#0 iii_3100) = false then
        if fst (#1 iii_3100) = false then
          if fst (#2 iii_3100) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_3100))))
        else
          if fst (#2 iii_3100) = false then
            ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_3100))), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_3100))), (true, ys_1940 (snd (#2 iii_3100))))
      else
        if fst (#1 iii_3100) = false then
          if fst (#2 iii_3100) = false then
            ((true, ys_1940 (snd (#0 iii_3100))), (false, (true, 0)), (false, (true, 0)))
          else
            ((true, ys_1940 (snd (#0 iii_3100))), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_3100))))
        else
          if fst (#2 iii_3100) = false then
            ((true, ys_1940 (snd (#0 iii_3100))), (true, xs_1939 (snd (#1 iii_3100))), (false, (true, 0)))
          else
            ((true, ys_1940 (snd (#0 iii_3100))), (true, xs_1939 (snd (#1 iii_3100))), 
             (true, ys_1940 (snd (#2 iii_3100))))
    in
    ys__xs__ys_1990
  else
    let r_xs_1946 = let r_xs__ys_3023 = xs__ys_1023 ((true, 0), (false, 0)) in
                    snd (fst r_xs__ys_3023) in
    let r_xs_1_1948 = snd r_xs_1946 in
    if fst r_xs_1946 <> false then
      let xs'_1014 x_1269 =
        let r_xs_1957 = let r_xs__ys_3002 = xs__ys_1023 ((true, x_1269 + 1), (false, 0)) in
                        snd (fst r_xs__ys_3002) in
        let r_xs_0_1958 = fst r_xs_1957 in
        let r_xs_1_1959 = snd r_xs_1957 in
        r_xs_1957
      in
      let r_xs_1960 = let r_xs__ys_2981 = xs__ys_1023 ((true, 0), (false, 0)) in
                      snd (fst r_xs__ys_2981) in
      let r_xs_0_1961 = fst r_xs_1960 in
      let xs'__ys_1965 ii_2944 =
        if fst (fst ii_2944) = false then
          if fst (snd ii_2944) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, ys_1940 (snd (snd ii_2944))))
        else
          if fst (snd ii_2944) = false then
            ((true, xs'_1014 (snd (fst ii_2944))), (false, (true, 0)))
          else
            ((true, xs'_1014 (snd (fst ii_2944))), (true, ys_1940 (snd (snd ii_2944))))
      in
      let xs'_1966 i_2924 = snd (fst (xs'__ys_1965 ((true, i_2924), (false, 0)))) in
      let ys_1967 i_2917 = snd (snd (xs'__ys_1965 ((false, 0), (true, i_2917)))) in
      let r_append_1968 = append_1165 xs'__ys_1965 in
      let r_append_xs'__ys_0_1969 i_2906 = snd (#0 (r_append_1968 ((true, i_2906), (false, 0), (false, 0)))) in
      let r_append_xs'__ys_1_1970 i_2896 = snd (#1 (r_append_1968 ((false, 0), (true, i_2896), (false, 0)))) in
      let r_append_xs'__ys_2_1971 i_2886 = snd (#2 (r_append_1968 ((false, 0), (false, 0), (true, i_2886)))) in
      let rs'_1195 i_1369 =
        if i_1369 = 0 then
          (true, snd r_xs_1960)
        else
          let r_r1_1976 =
            let r_r_append_2885 = r_append_1968 ((true, i_1369 - 1), (false, 0), (false, 0)) in
            snd (#0 r_r_append_2885)
          in
          let r_rs_0_1977 = fst r_r1_1976 in
          let r_rs_1_1978 = snd r_r1_1976 in
          r_r1_1976
      in
      let rs'__xs__ys_1986 iii_2831 =
        if fst (#0 iii_2831) = false then
          if fst (#1 iii_2831) = false then
            if fst (#2 iii_2831) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2831))))
          else
            if fst (#2 iii_2831) = false then
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2831))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2831))), (true, ys_1940 (snd (#2 iii_2831))))
        else
          if fst (#1 iii_2831) = false then
            if fst (#2 iii_2831) = false then
              ((true, rs'_1195 (snd (#0 iii_2831))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, rs'_1195 (snd (#0 iii_2831))), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2831))))
          else
            if fst (#2 iii_2831) = false then
              ((true, rs'_1195 (snd (#0 iii_2831))), (true, xs_1939 (snd (#1 iii_2831))), (false, (true, 0)))
            else
              ((true, rs'_1195 (snd (#0 iii_2831))), (true, xs_1939 (snd (#1 iii_2831))), 
               (true, ys_1940 (snd (#2 iii_2831))))
      in
      rs'__xs__ys_1986
    else
      let bot_1820 = _|_ in
      let bot__xs__ys_1954 iii_2519 =
        if fst (#0 iii_2519) = false then
          if fst (#1 iii_2519) = false then
            if fst (#2 iii_2519) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2519))))
          else
            if fst (#2 iii_2519) = false then
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2519))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2519))), (true, ys_1940 (snd (#2 iii_2519))))
        else
          if fst (#1 iii_2519) = false then
            if fst (#2 iii_2519) = false then
              ((true, bot_1820 (snd (#0 iii_2519))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, bot_1820 (snd (#0 iii_2519))), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2519))))
          else
            if fst (#2 iii_2519) = false then
              ((true, bot_1820 (snd (#0 iii_2519))), (true, xs_1939 (snd (#1 iii_2519))), (false, (true, 0)))
            else
              ((true, bot_1820 (snd (#0 iii_2519))), (true, xs_1939 (snd (#1 iii_2519))), 
               (true, ys_1940 (snd (#2 iii_2519))))
      in
      bot__xs__ys_1954
in
let main_1017 i_1018 n_1019 =
  let r_make_list_1991 = make_list_1008 n_1019 in
  let f_1732 x_1560 = (false, 0) in
  let r_make_list__f_1999 ix_2285 =
    if fst (fst ix_2285) = false then
      if fst (snd ix_2285) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, f_1732 (snd (snd ix_2285))))
    else
      if fst (snd ix_2285) = false then
        ((true, r_make_list_1991 (snd (fst ix_2285))), (false, (true, 0)))
      else
        ((true, r_make_list_1991 (snd (fst ix_2285))), (true, f_1732 (snd (snd ix_2285))))
  in
  let xs_2000 i_2265 = snd (fst (r_make_list__f_1999 ((true, i_2265), (false, 0)))) in
  let f_2001 x_2258 = snd (snd (r_make_list__f_1999 ((false, 0), (true, x_2258)))) in
  let r_append_2002 = append_1165 r_make_list__f_1999 in
  let r_append_xs__f_0_2003 i_2247 = snd (#0 (r_append_2002 ((true, i_2247), (false, 0), (false, 0)))) in
  let r_append_xs__f_1_2004 i_2237 = snd (#1 (r_append_2002 ((false, 0), (true, i_2237), (false, 0)))) in
  let r_append_xs__f_2_2005 i_2227 = snd (#2 (r_append_2002 ((false, 0), (false, 0), (true, i_2227)))) in
  let r_r1_2006 =
    let r_r_append_2226 = r_append_2002 ((true, i_1018), (false, 0), (false, 0)) in
    snd (#0 r_r_append_2226)
  in
  let r_ys_0_2007 = fst r_r1_2006 in
  let r_r_make_list_2009 = r_make_list_1991 i_1018 in
  let r_xs_0_2010 = fst r_r_make_list_2009 in
  if snd r_r1_2006 = snd r_r_make_list_2009 then
    ()
  else
    {fail} ()
in
let r_f_2015 = rand_int () in
let r_f_2017 = rand_int () in
let r_main_2018 = main_1017 r_f_2015 in
let r_r_main_2019 = r_main_2018 r_f_2017 in
()

flatten_let:
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1236 -> (false, 0)
  else
    let r_f_1921 = rand_int () in
    let r_make_list_1924 = make_list_1008 (n_1009 - 1) in
    fun i_1226 -> (if i_1226 = 0 then
                     (true, r_f_1921)
                   else
                     r_make_list_1924 (i_1226 - 1))
in
let rec append_1165 xs__ys_1023 =
  let xs_1939 i_3153 = snd (fst (xs__ys_1023 ((true, i_3153), (false, 0)))) in
  let ys_1940 i_3146 = snd (snd (xs__ys_1023 ((false, 0), (true, i_3146)))) in
  let r_xs__ys_3145 = xs__ys_1023 ((true, 0), (false, 0)) in
  let r_xs_1941 = snd (fst r_xs__ys_3145) in
  let r_xs_1_1943 = snd r_xs_1941 in
  if fst r_xs_1941 = false then
    let ys__xs__ys_1990 iii_3100 =
      if fst (#0 iii_3100) = false then
        if fst (#1 iii_3100) = false then
          if fst (#2 iii_3100) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_3100))))
        else
          if fst (#2 iii_3100) = false then
            ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_3100))), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_3100))), (true, ys_1940 (snd (#2 iii_3100))))
      else
        if fst (#1 iii_3100) = false then
          if fst (#2 iii_3100) = false then
            ((true, ys_1940 (snd (#0 iii_3100))), (false, (true, 0)), (false, (true, 0)))
          else
            ((true, ys_1940 (snd (#0 iii_3100))), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_3100))))
        else
          if fst (#2 iii_3100) = false then
            ((true, ys_1940 (snd (#0 iii_3100))), (true, xs_1939 (snd (#1 iii_3100))), (false, (true, 0)))
          else
            ((true, ys_1940 (snd (#0 iii_3100))), (true, xs_1939 (snd (#1 iii_3100))), 
             (true, ys_1940 (snd (#2 iii_3100))))
    in
    ys__xs__ys_1990
  else
    let r_xs__ys_3023 = xs__ys_1023 ((true, 0), (false, 0)) in
    let r_xs_1946 = snd (fst r_xs__ys_3023) in
    let r_xs_1_1948 = snd r_xs_1946 in
    if fst r_xs_1946 <> false then
      let xs'_1014 x_1269 =
        let r_xs__ys_3002 = xs__ys_1023 ((true, x_1269 + 1), (false, 0)) in
        let r_xs_1957 = snd (fst r_xs__ys_3002) in
        let r_xs_0_1958 = fst r_xs_1957 in
        let r_xs_1_1959 = snd r_xs_1957 in
        r_xs_1957
      in
      let r_xs__ys_2981 = xs__ys_1023 ((true, 0), (false, 0)) in
      let r_xs_1960 = snd (fst r_xs__ys_2981) in
      let r_xs_0_1961 = fst r_xs_1960 in
      let xs'__ys_1965 ii_2944 =
        if fst (fst ii_2944) = false then
          if fst (snd ii_2944) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, ys_1940 (snd (snd ii_2944))))
        else
          if fst (snd ii_2944) = false then
            ((true, xs'_1014 (snd (fst ii_2944))), (false, (true, 0)))
          else
            ((true, xs'_1014 (snd (fst ii_2944))), (true, ys_1940 (snd (snd ii_2944))))
      in
      let xs'_1966 i_2924 = snd (fst (xs'__ys_1965 ((true, i_2924), (false, 0)))) in
      let ys_1967 i_2917 = snd (snd (xs'__ys_1965 ((false, 0), (true, i_2917)))) in
      let r_append_1968 = append_1165 xs'__ys_1965 in
      let r_append_xs'__ys_0_1969 i_2906 = snd (#0 (r_append_1968 ((true, i_2906), (false, 0), (false, 0)))) in
      let r_append_xs'__ys_1_1970 i_2896 = snd (#1 (r_append_1968 ((false, 0), (true, i_2896), (false, 0)))) in
      let r_append_xs'__ys_2_1971 i_2886 = snd (#2 (r_append_1968 ((false, 0), (false, 0), (true, i_2886)))) in
      let rs'_1195 i_1369 =
        if i_1369 = 0 then
          (true, snd r_xs_1960)
        else
          let r_r_append_2885 = r_append_1968 ((true, i_1369 - 1), (false, 0), (false, 0)) in
          let r_r1_1976 = snd (#0 r_r_append_2885) in
          let r_rs_0_1977 = fst r_r1_1976 in
          let r_rs_1_1978 = snd r_r1_1976 in
          r_r1_1976
      in
      let rs'__xs__ys_1986 iii_2831 =
        if fst (#0 iii_2831) = false then
          if fst (#1 iii_2831) = false then
            if fst (#2 iii_2831) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2831))))
          else
            if fst (#2 iii_2831) = false then
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2831))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2831))), (true, ys_1940 (snd (#2 iii_2831))))
        else
          if fst (#1 iii_2831) = false then
            if fst (#2 iii_2831) = false then
              ((true, rs'_1195 (snd (#0 iii_2831))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, rs'_1195 (snd (#0 iii_2831))), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2831))))
          else
            if fst (#2 iii_2831) = false then
              ((true, rs'_1195 (snd (#0 iii_2831))), (true, xs_1939 (snd (#1 iii_2831))), (false, (true, 0)))
            else
              ((true, rs'_1195 (snd (#0 iii_2831))), (true, xs_1939 (snd (#1 iii_2831))), 
               (true, ys_1940 (snd (#2 iii_2831))))
      in
      rs'__xs__ys_1986
    else
      let bot_1820 = _|_ in
      let bot__xs__ys_1954 iii_2519 =
        if fst (#0 iii_2519) = false then
          if fst (#1 iii_2519) = false then
            if fst (#2 iii_2519) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2519))))
          else
            if fst (#2 iii_2519) = false then
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2519))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2519))), (true, ys_1940 (snd (#2 iii_2519))))
        else
          if fst (#1 iii_2519) = false then
            if fst (#2 iii_2519) = false then
              ((true, bot_1820 (snd (#0 iii_2519))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, bot_1820 (snd (#0 iii_2519))), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2519))))
          else
            if fst (#2 iii_2519) = false then
              ((true, bot_1820 (snd (#0 iii_2519))), (true, xs_1939 (snd (#1 iii_2519))), (false, (true, 0)))
            else
              ((true, bot_1820 (snd (#0 iii_2519))), (true, xs_1939 (snd (#1 iii_2519))), 
               (true, ys_1940 (snd (#2 iii_2519))))
      in
      bot__xs__ys_1954
in
let main_1017 i_1018 n_1019 =
  let r_make_list_1991 = make_list_1008 n_1019 in
  let f_1732 x_1560 = (false, 0) in
  let r_make_list__f_1999 ix_2285 =
    if fst (fst ix_2285) = false then
      if fst (snd ix_2285) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, f_1732 (snd (snd ix_2285))))
    else
      if fst (snd ix_2285) = false then
        ((true, r_make_list_1991 (snd (fst ix_2285))), (false, (true, 0)))
      else
        ((true, r_make_list_1991 (snd (fst ix_2285))), (true, f_1732 (snd (snd ix_2285))))
  in
  let xs_2000 i_2265 = snd (fst (r_make_list__f_1999 ((true, i_2265), (false, 0)))) in
  let f_2001 x_2258 = snd (snd (r_make_list__f_1999 ((false, 0), (true, x_2258)))) in
  let r_append_2002 = append_1165 r_make_list__f_1999 in
  let r_append_xs__f_0_2003 i_2247 = snd (#0 (r_append_2002 ((true, i_2247), (false, 0), (false, 0)))) in
  let r_append_xs__f_1_2004 i_2237 = snd (#1 (r_append_2002 ((false, 0), (true, i_2237), (false, 0)))) in
  let r_append_xs__f_2_2005 i_2227 = snd (#2 (r_append_2002 ((false, 0), (false, 0), (true, i_2227)))) in
  let r_r_append_2226 = r_append_2002 ((true, i_1018), (false, 0), (false, 0)) in
  let r_r1_2006 = snd (#0 r_r_append_2226) in
  let r_ys_0_2007 = fst r_r1_2006 in
  let r_r_make_list_2009 = r_make_list_1991 i_1018 in
  let r_xs_0_2010 = fst r_r_make_list_2009 in
  if snd r_r1_2006 = snd r_r_make_list_2009 then
    ()
  else
    {fail} ()
in
let r_f_2015 = rand_int () in
let r_f_2017 = rand_int () in
let r_main_2018 = main_1017 r_f_2015 in
let r_r_main_2019 = r_main_2018 r_f_2017 in
()

NORMALIZE: r_ys_0_2007
[r_r_make_list_2009]
NORMALIZE: r_r1_2006
[r_r_make_list_2009]
normalize let:
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1236 -> (false, 0)
  else
    let r_f_1921 = rand_int () in
    let r_make_list_1924 = make_list_1008 (n_1009 - 1) in
    fun i_1226 -> (if i_1226 = 0 then
                     (true, r_f_1921)
                   else
                     r_make_list_1924 (i_1226 - 1))
in
let rec append_1165 xs__ys_1023 =
  let xs_1939 i_3153 = snd (fst (xs__ys_1023 ((true, i_3153), (false, 0)))) in
  let ys_1940 i_3146 = snd (snd (xs__ys_1023 ((false, 0), (true, i_3146)))) in
  let r_xs__ys_3145 = xs__ys_1023 ((true, 0), (false, 0)) in
  let r_xs_1941 = snd (fst r_xs__ys_3145) in
  let r_xs_1_1943 = snd r_xs_1941 in
  if fst r_xs_1941 = false then
    let ys__xs__ys_1990 iii_3100 =
      if fst (#0 iii_3100) = false then
        if fst (#1 iii_3100) = false then
          if fst (#2 iii_3100) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_3100))))
        else
          if fst (#2 iii_3100) = false then
            ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_3100))), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_3100))), (true, ys_1940 (snd (#2 iii_3100))))
      else
        if fst (#1 iii_3100) = false then
          if fst (#2 iii_3100) = false then
            ((true, ys_1940 (snd (#0 iii_3100))), (false, (true, 0)), (false, (true, 0)))
          else
            ((true, ys_1940 (snd (#0 iii_3100))), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_3100))))
        else
          if fst (#2 iii_3100) = false then
            ((true, ys_1940 (snd (#0 iii_3100))), (true, xs_1939 (snd (#1 iii_3100))), (false, (true, 0)))
          else
            ((true, ys_1940 (snd (#0 iii_3100))), (true, xs_1939 (snd (#1 iii_3100))), 
             (true, ys_1940 (snd (#2 iii_3100))))
    in
    ys__xs__ys_1990
  else
    let r_xs__ys_3023 = xs__ys_1023 ((true, 0), (false, 0)) in
    let r_xs_1946 = snd (fst r_xs__ys_3023) in
    let r_xs_1_1948 = snd r_xs_1946 in
    if fst r_xs_1946 <> false then
      let xs'_1014 x_1269 =
        let r_xs__ys_3002 = xs__ys_1023 ((true, x_1269 + 1), (false, 0)) in
        let r_xs_1957 = snd (fst r_xs__ys_3002) in
        let r_xs_0_1958 = fst r_xs_1957 in
        let r_xs_1_1959 = snd r_xs_1957 in
        r_xs_1957
      in
      let r_xs__ys_2981 = xs__ys_1023 ((true, 0), (false, 0)) in
      let r_xs_1960 = snd (fst r_xs__ys_2981) in
      let r_xs_0_1961 = fst r_xs_1960 in
      let xs'__ys_1965 ii_2944 =
        if fst (fst ii_2944) = false then
          if fst (snd ii_2944) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, ys_1940 (snd (snd ii_2944))))
        else
          if fst (snd ii_2944) = false then
            ((true, xs'_1014 (snd (fst ii_2944))), (false, (true, 0)))
          else
            ((true, xs'_1014 (snd (fst ii_2944))), (true, ys_1940 (snd (snd ii_2944))))
      in
      let xs'_1966 i_2924 = snd (fst (xs'__ys_1965 ((true, i_2924), (false, 0)))) in
      let ys_1967 i_2917 = snd (snd (xs'__ys_1965 ((false, 0), (true, i_2917)))) in
      let r_append_1968 = append_1165 xs'__ys_1965 in
      let r_append_xs'__ys_0_1969 i_2906 = snd (#0 (r_append_1968 ((true, i_2906), (false, 0), (false, 0)))) in
      let r_append_xs'__ys_1_1970 i_2896 = snd (#1 (r_append_1968 ((false, 0), (true, i_2896), (false, 0)))) in
      let r_append_xs'__ys_2_1971 i_2886 = snd (#2 (r_append_1968 ((false, 0), (false, 0), (true, i_2886)))) in
      let rs'_1195 i_1369 =
        if i_1369 = 0 then
          (true, snd r_xs_1960)
        else
          let r_r_append_2885 = r_append_1968 ((true, i_1369 - 1), (false, 0), (false, 0)) in
          let r_r1_1976 = snd (#0 r_r_append_2885) in
          let r_rs_0_1977 = fst r_r1_1976 in
          let r_rs_1_1978 = snd r_r1_1976 in
          r_r1_1976
      in
      let rs'__xs__ys_1986 iii_2831 =
        if fst (#0 iii_2831) = false then
          if fst (#1 iii_2831) = false then
            if fst (#2 iii_2831) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2831))))
          else
            if fst (#2 iii_2831) = false then
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2831))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2831))), (true, ys_1940 (snd (#2 iii_2831))))
        else
          if fst (#1 iii_2831) = false then
            if fst (#2 iii_2831) = false then
              ((true, rs'_1195 (snd (#0 iii_2831))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, rs'_1195 (snd (#0 iii_2831))), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2831))))
          else
            if fst (#2 iii_2831) = false then
              ((true, rs'_1195 (snd (#0 iii_2831))), (true, xs_1939 (snd (#1 iii_2831))), (false, (true, 0)))
            else
              ((true, rs'_1195 (snd (#0 iii_2831))), (true, xs_1939 (snd (#1 iii_2831))), 
               (true, ys_1940 (snd (#2 iii_2831))))
      in
      rs'__xs__ys_1986
    else
      let bot_1820 = _|_ in
      let bot__xs__ys_1954 iii_2519 =
        if fst (#0 iii_2519) = false then
          if fst (#1 iii_2519) = false then
            if fst (#2 iii_2519) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2519))))
          else
            if fst (#2 iii_2519) = false then
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2519))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2519))), (true, ys_1940 (snd (#2 iii_2519))))
        else
          if fst (#1 iii_2519) = false then
            if fst (#2 iii_2519) = false then
              ((true, bot_1820 (snd (#0 iii_2519))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, bot_1820 (snd (#0 iii_2519))), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2519))))
          else
            if fst (#2 iii_2519) = false then
              ((true, bot_1820 (snd (#0 iii_2519))), (true, xs_1939 (snd (#1 iii_2519))), (false, (true, 0)))
            else
              ((true, bot_1820 (snd (#0 iii_2519))), (true, xs_1939 (snd (#1 iii_2519))), 
               (true, ys_1940 (snd (#2 iii_2519))))
      in
      bot__xs__ys_1954
in
let main_1017 i_1018 n_1019 =
  let r_make_list_1991 = make_list_1008 n_1019 in
  let f_1732 x_1560 = (false, 0) in
  let r_make_list__f_1999 ix_2285 =
    if fst (fst ix_2285) = false then
      if fst (snd ix_2285) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, f_1732 (snd (snd ix_2285))))
    else
      if fst (snd ix_2285) = false then
        ((true, r_make_list_1991 (snd (fst ix_2285))), (false, (true, 0)))
      else
        ((true, r_make_list_1991 (snd (fst ix_2285))), (true, f_1732 (snd (snd ix_2285))))
  in
  let xs_2000 i_2265 = snd (fst (r_make_list__f_1999 ((true, i_2265), (false, 0)))) in
  let f_2001 x_2258 = snd (snd (r_make_list__f_1999 ((false, 0), (true, x_2258)))) in
  let r_append_2002 = append_1165 r_make_list__f_1999 in
  let r_append_xs__f_0_2003 i_2247 = snd (#0 (r_append_2002 ((true, i_2247), (false, 0), (false, 0)))) in
  let r_append_xs__f_1_2004 i_2237 = snd (#1 (r_append_2002 ((false, 0), (true, i_2237), (false, 0)))) in
  let r_append_xs__f_2_2005 i_2227 = snd (#2 (r_append_2002 ((false, 0), (false, 0), (true, i_2227)))) in
  let r_r_append_2226 = r_append_2002 ((true, i_1018), (false, 0), (false, 0)) in
  let r_r_make_list_2009 = r_make_list_1991 i_1018 in
  let r_r1_2006 = snd (#0 r_r_append_2226) in
  let r_ys_0_2007 = fst r_r1_2006 in
  let r_xs_0_2010 = fst r_r_make_list_2009 in
  if snd r_r1_2006 = snd r_r_make_list_2009 then
    ()
  else
    {fail} ()
in
let r_f_2015 = rand_int () in
let r_f_2017 = rand_int () in
let r_main_2018 = main_1017 r_f_2015 in
let r_r_main_2019 = r_main_2018 r_f_2017 in
()

is_subsumed: rand_int (), rand_int ()
is_subsumed: rand_int (), main_1017 r_f_2015
is_subsumed: rand_int (), r_main_2018 r_f_2017
is_subsumed: make_list_1008 n_1019, append_1165 r_make_list__f_1999
is_subsumed: make_list_1008 n_1019, r_append_2002 ((true, i_1018), (false, 0), (false, 0))
is_subsumed: r_append_2002 ((true, i_1018), (false, 0), (false, 0)), 
r_make_list_1991 i_1018
is_subsumed: append_1165 r_make_list__f_1999, r_make_list_1991 i_1018
is_subsumed: r_make_list_1991 i_1018, snd (#0 r_r_append_2226)
is_subsumed: append_1165 r_make_list__f_1999, snd (#0 r_r_append_2226)
is_subsumed: make_list_1008 n_1019, snd (#0 r_r_append_2226)
is_subsumed: r_make_list_1991 i_1018, fst r_r1_2006
is_subsumed: r_append_2002 ((true, i_1018), (false, 0), (false, 0)), 
fst r_r1_2006
is_subsumed: append_1165 r_make_list__f_1999, fst r_r1_2006
is_subsumed: make_list_1008 n_1019, fst r_r1_2006
is_subsumed: fst r_r1_2006, fst r_r_make_list_2009
is_subsumed: snd (#0 r_r_append_2226), fst r_r_make_list_2009
is_subsumed: r_append_2002 ((true, i_1018), (false, 0), (false, 0)), 
fst r_r_make_list_2009
is_subsumed: append_1165 r_make_list__f_1999, fst r_r_make_list_2009
is_subsumed: make_list_1008 n_1019, fst r_r_make_list_2009
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), snd r_xs_1941
is_subsumed: snd r_xs_1941, xs__ys_1023 ((true, 0), (false, 0))
is_subsumed: snd (fst r_xs__ys_3145), xs__ys_1023 ((true, 0), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, 0), (false, 0))
r_xs__ys_3145 |-> r_xs__ys_3023
is_subsumed: snd r_xs_1941, snd (fst r_xs__ys_3023)
is_subsumed: snd (fst r_xs__ys_3145), snd (fst r_xs__ys_3023)
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), snd (fst r_xs__ys_3023)
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), snd r_xs_1946
is_subsumed: snd r_xs_1941, snd r_xs_1946
is_subsumed: snd (fst r_xs__ys_3145), snd r_xs_1946
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), snd r_xs_1946
is_subsumed: snd r_xs_1946, _|_
is_subsumed: snd (fst r_xs__ys_3023), _|_
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), _|_
is_subsumed: snd r_xs_1941, _|_
is_subsumed: snd (fst r_xs__ys_3145), _|_
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), _|_
is_subsumed: snd r_xs_1946, xs__ys_1023 ((true, 0), (false, 0))
is_subsumed: snd (fst r_xs__ys_3023), xs__ys_1023 ((true, 0), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, 0), (false, 0))
is_subsumed: snd r_xs_1941, xs__ys_1023 ((true, 0), (false, 0))
is_subsumed: snd (fst r_xs__ys_3145), xs__ys_1023 ((true, 0), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, 0), (false, 0))
r_xs__ys_3023 |-> r_xs__ys_2981
r_xs__ys_3145 |-> r_xs__ys_2981
is_subsumed: snd r_xs_1946, snd (fst r_xs__ys_2981)
is_subsumed: snd (fst r_xs__ys_3023), snd (fst r_xs__ys_2981)
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), snd (fst r_xs__ys_2981)
is_subsumed: snd r_xs_1941, snd (fst r_xs__ys_2981)
is_subsumed: snd (fst r_xs__ys_3145), snd (fst r_xs__ys_2981)
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), snd (fst r_xs__ys_2981)
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), fst r_xs_1960
is_subsumed: snd r_xs_1946, fst r_xs_1960
is_subsumed: snd (fst r_xs__ys_3023), fst r_xs_1960
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), fst r_xs_1960
is_subsumed: snd r_xs_1941, fst r_xs_1960
is_subsumed: snd (fst r_xs__ys_3145), fst r_xs_1960
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), fst r_xs_1960
is_subsumed: fst r_xs_1960, append_1165 xs'__ys_1965
is_subsumed: snd (fst r_xs__ys_2981), append_1165 xs'__ys_1965
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), append_1165 xs'__ys_1965
is_subsumed: snd r_xs_1946, append_1165 xs'__ys_1965
is_subsumed: snd (fst r_xs__ys_3023), append_1165 xs'__ys_1965
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), append_1165 xs'__ys_1965
is_subsumed: snd r_xs_1941, append_1165 xs'__ys_1965
is_subsumed: snd (fst r_xs__ys_3145), append_1165 xs'__ys_1965
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), append_1165 xs'__ys_1965
is_subsumed: fst r_xs_1960, r_append_1968 ((true, i_1369 - 1), (false, 0), (false, 0))
is_subsumed: snd (fst r_xs__ys_2981), r_append_1968 ((true, i_1369 - 1), (false, 0), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_1968 ((true, i_1369 - 1), (false, 0), (false, 0))
is_subsumed: snd r_xs_1946, r_append_1968 ((true, i_1369 - 1), (false, 0), (false, 0))
is_subsumed: snd (fst r_xs__ys_3023), r_append_1968 ((true, i_1369 - 1), (false, 0), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_1968 ((true, i_1369 - 1), (false, 0), (false, 0))
is_subsumed: snd r_xs_1941, r_append_1968 ((true, i_1369 - 1), (false, 0), (false, 0))
is_subsumed: snd (fst r_xs__ys_3145), r_append_1968 ((true, i_1369 - 1), (false, 0), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_1968 ((true, i_1369 - 1), (false, 0), (false, 0))
is_subsumed: append_1165 xs'__ys_1965, snd (#0 r_r_append_2885)
is_subsumed: fst r_xs_1960, snd (#0 r_r_append_2885)
is_subsumed: snd (fst r_xs__ys_2981), snd (#0 r_r_append_2885)
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), snd (#0 r_r_append_2885)
is_subsumed: snd r_xs_1946, snd (#0 r_r_append_2885)
is_subsumed: snd (fst r_xs__ys_3023), snd (#0 r_r_append_2885)
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), snd (#0 r_r_append_2885)
is_subsumed: snd r_xs_1941, snd (#0 r_r_append_2885)
is_subsumed: snd (fst r_xs__ys_3145), snd (#0 r_r_append_2885)
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), snd (#0 r_r_append_2885)
is_subsumed: r_append_1968 ((true, i_1369 - 1), (false, 0), (false, 0)), 
fst r_r1_1976
is_subsumed: append_1165 xs'__ys_1965, fst r_r1_1976
is_subsumed: fst r_xs_1960, fst r_r1_1976
is_subsumed: snd (fst r_xs__ys_2981), fst r_r1_1976
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), fst r_r1_1976
is_subsumed: snd r_xs_1946, fst r_r1_1976
is_subsumed: snd (fst r_xs__ys_3023), fst r_r1_1976
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), fst r_r1_1976
is_subsumed: snd r_xs_1941, fst r_r1_1976
is_subsumed: snd (fst r_xs__ys_3145), fst r_r1_1976
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), fst r_r1_1976
is_subsumed: fst r_r1_1976, snd r_r1_1976
is_subsumed: r_append_1968 ((true, i_1369 - 1), (false, 0), (false, 0)), 
snd r_r1_1976
is_subsumed: append_1165 xs'__ys_1965, snd r_r1_1976
is_subsumed: fst r_xs_1960, snd r_r1_1976
is_subsumed: snd (fst r_xs__ys_2981), snd r_r1_1976
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), snd r_r1_1976
is_subsumed: snd r_xs_1946, snd r_r1_1976
is_subsumed: snd (fst r_xs__ys_3023), snd r_r1_1976
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), snd r_r1_1976
is_subsumed: snd r_xs_1941, snd r_r1_1976
is_subsumed: snd (fst r_xs__ys_3145), snd r_r1_1976
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), snd r_r1_1976
is_subsumed: snd r_xs_1946, xs__ys_1023 ((true, x_1269 + 1), (false, 0))
is_subsumed: snd (fst r_xs__ys_3023), xs__ys_1023 ((true, x_1269 + 1), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, x_1269 + 1), (false, 0))
is_subsumed: snd r_xs_1941, xs__ys_1023 ((true, x_1269 + 1), (false, 0))
is_subsumed: snd (fst r_xs__ys_3145), xs__ys_1023 ((true, x_1269 + 1), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, x_1269 + 1), (false, 0))
is_subsumed: snd r_xs_1946, snd (fst r_xs__ys_3002)
is_subsumed: snd (fst r_xs__ys_3023), snd (fst r_xs__ys_3002)
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), snd (fst r_xs__ys_3002)
is_subsumed: snd r_xs_1941, snd (fst r_xs__ys_3002)
is_subsumed: snd (fst r_xs__ys_3145), snd (fst r_xs__ys_3002)
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), snd (fst r_xs__ys_3002)
is_subsumed: xs__ys_1023 ((true, x_1269 + 1), (false, 0)), fst r_xs_1957
is_subsumed: snd r_xs_1946, fst r_xs_1957
is_subsumed: snd (fst r_xs__ys_3023), fst r_xs_1957
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), fst r_xs_1957
is_subsumed: snd r_xs_1941, fst r_xs_1957
is_subsumed: snd (fst r_xs__ys_3145), fst r_xs_1957
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), fst r_xs_1957
is_subsumed: fst r_xs_1957, snd r_xs_1957
is_subsumed: xs__ys_1023 ((true, x_1269 + 1), (false, 0)), snd r_xs_1957
is_subsumed: snd r_xs_1946, snd r_xs_1957
is_subsumed: snd (fst r_xs__ys_3023), snd r_xs_1957
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), snd r_xs_1957
is_subsumed: snd r_xs_1941, snd r_xs_1957
is_subsumed: snd (fst r_xs__ys_3145), snd r_xs_1957
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), snd r_xs_1957
is_subsumed: rand_int (), make_list_1008 (n_1009 - 1)
r_xs__ys_3023; r_xs__ys_3145; r_xs__ys_3145
r_xs__ys_3023 |-> r_xs__ys_3145
r_xs__ys_2981 |-> r_xs__ys_3145
elim_same_app:
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1236 -> (false, 0)
  else
    let r_f_1921 = rand_int () in
    let r_make_list_1924 = make_list_1008 (n_1009 - 1) in
    fun i_1226 -> (if i_1226 = 0 then
                     (true, r_f_1921)
                   else
                     r_make_list_1924 (i_1226 - 1))
in
let rec append_1165 xs__ys_1023 =
  let xs_1939 i_3153 = snd (fst (xs__ys_1023 ((true, i_3153), (false, 0)))) in
  let ys_1940 i_3146 = snd (snd (xs__ys_1023 ((false, 0), (true, i_3146)))) in
  let r_xs__ys_3145 = xs__ys_1023 ((true, 0), (false, 0)) in
  let r_xs_1941 = snd (fst r_xs__ys_3145) in
  let r_xs_1_1943 = snd r_xs_1941 in
  if fst r_xs_1941 = false then
    let ys__xs__ys_1990 iii_3100 =
      if fst (#0 iii_3100) = false then
        if fst (#1 iii_3100) = false then
          if fst (#2 iii_3100) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_3100))))
        else
          if fst (#2 iii_3100) = false then
            ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_3100))), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_3100))), (true, ys_1940 (snd (#2 iii_3100))))
      else
        if fst (#1 iii_3100) = false then
          if fst (#2 iii_3100) = false then
            ((true, ys_1940 (snd (#0 iii_3100))), (false, (true, 0)), (false, (true, 0)))
          else
            ((true, ys_1940 (snd (#0 iii_3100))), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_3100))))
        else
          if fst (#2 iii_3100) = false then
            ((true, ys_1940 (snd (#0 iii_3100))), (true, xs_1939 (snd (#1 iii_3100))), (false, (true, 0)))
          else
            ((true, ys_1940 (snd (#0 iii_3100))), (true, xs_1939 (snd (#1 iii_3100))), 
             (true, ys_1940 (snd (#2 iii_3100))))
    in
    ys__xs__ys_1990
  else
    let r_xs_1946 = snd (fst r_xs__ys_3145) in
    let r_xs_1_1948 = snd r_xs_1946 in
    if fst r_xs_1946 <> false then
      let xs'_1014 x_1269 =
        let r_xs__ys_3002 = xs__ys_1023 ((true, x_1269 + 1), (false, 0)) in
        let r_xs_1957 = snd (fst r_xs__ys_3002) in
        let r_xs_0_1958 = fst r_xs_1957 in
        let r_xs_1_1959 = snd r_xs_1957 in
        r_xs_1957
      in
      let r_xs_1960 = snd (fst r_xs__ys_3145) in
      let r_xs_0_1961 = fst r_xs_1960 in
      let xs'__ys_1965 ii_2944 =
        if fst (fst ii_2944) = false then
          if fst (snd ii_2944) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, ys_1940 (snd (snd ii_2944))))
        else
          if fst (snd ii_2944) = false then
            ((true, xs'_1014 (snd (fst ii_2944))), (false, (true, 0)))
          else
            ((true, xs'_1014 (snd (fst ii_2944))), (true, ys_1940 (snd (snd ii_2944))))
      in
      let xs'_1966 i_2924 = snd (fst (xs'__ys_1965 ((true, i_2924), (false, 0)))) in
      let ys_1967 i_2917 = snd (snd (xs'__ys_1965 ((false, 0), (true, i_2917)))) in
      let r_append_1968 = append_1165 xs'__ys_1965 in
      let r_append_xs'__ys_0_1969 i_2906 = snd (#0 (r_append_1968 ((true, i_2906), (false, 0), (false, 0)))) in
      let r_append_xs'__ys_1_1970 i_2896 = snd (#1 (r_append_1968 ((false, 0), (true, i_2896), (false, 0)))) in
      let r_append_xs'__ys_2_1971 i_2886 = snd (#2 (r_append_1968 ((false, 0), (false, 0), (true, i_2886)))) in
      let rs'_1195 i_1369 =
        if i_1369 = 0 then
          (true, snd r_xs_1960)
        else
          let r_r_append_2885 = r_append_1968 ((true, i_1369 - 1), (false, 0), (false, 0)) in
          let r_r1_1976 = snd (#0 r_r_append_2885) in
          let r_rs_0_1977 = fst r_r1_1976 in
          let r_rs_1_1978 = snd r_r1_1976 in
          r_r1_1976
      in
      let rs'__xs__ys_1986 iii_2831 =
        if fst (#0 iii_2831) = false then
          if fst (#1 iii_2831) = false then
            if fst (#2 iii_2831) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2831))))
          else
            if fst (#2 iii_2831) = false then
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2831))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2831))), (true, ys_1940 (snd (#2 iii_2831))))
        else
          if fst (#1 iii_2831) = false then
            if fst (#2 iii_2831) = false then
              ((true, rs'_1195 (snd (#0 iii_2831))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, rs'_1195 (snd (#0 iii_2831))), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2831))))
          else
            if fst (#2 iii_2831) = false then
              ((true, rs'_1195 (snd (#0 iii_2831))), (true, xs_1939 (snd (#1 iii_2831))), (false, (true, 0)))
            else
              ((true, rs'_1195 (snd (#0 iii_2831))), (true, xs_1939 (snd (#1 iii_2831))), 
               (true, ys_1940 (snd (#2 iii_2831))))
      in
      rs'__xs__ys_1986
    else
      let bot_1820 = _|_ in
      let bot__xs__ys_1954 iii_2519 =
        if fst (#0 iii_2519) = false then
          if fst (#1 iii_2519) = false then
            if fst (#2 iii_2519) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2519))))
          else
            if fst (#2 iii_2519) = false then
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2519))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2519))), (true, ys_1940 (snd (#2 iii_2519))))
        else
          if fst (#1 iii_2519) = false then
            if fst (#2 iii_2519) = false then
              ((true, bot_1820 (snd (#0 iii_2519))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, bot_1820 (snd (#0 iii_2519))), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2519))))
          else
            if fst (#2 iii_2519) = false then
              ((true, bot_1820 (snd (#0 iii_2519))), (true, xs_1939 (snd (#1 iii_2519))), (false, (true, 0)))
            else
              ((true, bot_1820 (snd (#0 iii_2519))), (true, xs_1939 (snd (#1 iii_2519))), 
               (true, ys_1940 (snd (#2 iii_2519))))
      in
      bot__xs__ys_1954
in
let main_1017 i_1018 n_1019 =
  let r_make_list_1991 = make_list_1008 n_1019 in
  let f_1732 x_1560 = (false, 0) in
  let r_make_list__f_1999 ix_2285 =
    if fst (fst ix_2285) = false then
      if fst (snd ix_2285) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, f_1732 (snd (snd ix_2285))))
    else
      if fst (snd ix_2285) = false then
        ((true, r_make_list_1991 (snd (fst ix_2285))), (false, (true, 0)))
      else
        ((true, r_make_list_1991 (snd (fst ix_2285))), (true, f_1732 (snd (snd ix_2285))))
  in
  let xs_2000 i_2265 = snd (fst (r_make_list__f_1999 ((true, i_2265), (false, 0)))) in
  let f_2001 x_2258 = snd (snd (r_make_list__f_1999 ((false, 0), (true, x_2258)))) in
  let r_append_2002 = append_1165 r_make_list__f_1999 in
  let r_append_xs__f_0_2003 i_2247 = snd (#0 (r_append_2002 ((true, i_2247), (false, 0), (false, 0)))) in
  let r_append_xs__f_1_2004 i_2237 = snd (#1 (r_append_2002 ((false, 0), (true, i_2237), (false, 0)))) in
  let r_append_xs__f_2_2005 i_2227 = snd (#2 (r_append_2002 ((false, 0), (false, 0), (true, i_2227)))) in
  let r_r_append_2226 = r_append_2002 ((true, i_1018), (false, 0), (false, 0)) in
  let r_r_make_list_2009 = r_make_list_1991 i_1018 in
  let r_r1_2006 = snd (#0 r_r_append_2226) in
  let r_ys_0_2007 = fst r_r1_2006 in
  let r_xs_0_2010 = fst r_r_make_list_2009 in
  if snd r_r1_2006 = snd r_r_make_list_2009 then
    ()
  else
    {fail} ()
in
let r_f_2015 = rand_int () in
let r_f_2017 = rand_int () in
let r_main_2018 = main_1017 r_f_2015 in
let r_r_main_2019 = r_main_2018 r_f_2017 in
()

elim_unused_branch:
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1236 -> (false, 0)
  else
    let r_f_1921 = rand_int () in
    let r_make_list_1924 = make_list_1008 (n_1009 - 1) in
    fun i_1226 -> (if i_1226 = 0 then
                     (true, r_f_1921)
                   else
                     r_make_list_1924 (i_1226 - 1))
in
let rec append_1165 xs__ys_1023 =
  let xs_1939 i_3153 = snd (fst (xs__ys_1023 ((true, i_3153), (false, 0)))) in
  let ys_1940 i_3146 = snd (snd (xs__ys_1023 ((false, 0), (true, i_3146)))) in
  let r_xs__ys_3145 = xs__ys_1023 ((true, 0), (false, 0)) in
  let r_xs_1941 = snd (fst r_xs__ys_3145) in
  let r_xs_1_1943 = snd r_xs_1941 in
  if fst r_xs_1941 = false then
    let ys__xs__ys_1990 iii_3100 =
      if fst (#0 iii_3100) = false then
        if fst (#1 iii_3100) = false then
          if fst (#2 iii_3100) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_3100))))
        else
          if fst (#2 iii_3100) = false then
            ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_3100))), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_3100))), (true, ys_1940 (snd (#2 iii_3100))))
      else
        if fst (#1 iii_3100) = false then
          if fst (#2 iii_3100) = false then
            ((true, ys_1940 (snd (#0 iii_3100))), (false, (true, 0)), (false, (true, 0)))
          else
            ((true, ys_1940 (snd (#0 iii_3100))), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_3100))))
        else
          if fst (#2 iii_3100) = false then
            ((true, ys_1940 (snd (#0 iii_3100))), (true, xs_1939 (snd (#1 iii_3100))), (false, (true, 0)))
          else
            ((true, ys_1940 (snd (#0 iii_3100))), (true, xs_1939 (snd (#1 iii_3100))), 
             (true, ys_1940 (snd (#2 iii_3100))))
    in
    ys__xs__ys_1990
  else
    let r_xs_1946 = snd (fst r_xs__ys_3145) in
    let r_xs_1_1948 = snd r_xs_1946 in
    if fst r_xs_1946 <> false then
      let xs'_1014 x_1269 =
        let r_xs__ys_3002 = xs__ys_1023 ((true, x_1269 + 1), (false, 0)) in
        let r_xs_1957 = snd (fst r_xs__ys_3002) in
        let r_xs_0_1958 = fst r_xs_1957 in
        let r_xs_1_1959 = snd r_xs_1957 in
        r_xs_1957
      in
      let r_xs_1960 = snd (fst r_xs__ys_3145) in
      let r_xs_0_1961 = fst r_xs_1960 in
      let xs'__ys_1965 ii_2944 =
        if fst (fst ii_2944) = false then
          if fst (snd ii_2944) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, ys_1940 (snd (snd ii_2944))))
        else
          if fst (snd ii_2944) = false then
            ((true, xs'_1014 (snd (fst ii_2944))), (false, (true, 0)))
          else
            ((true, xs'_1014 (snd (fst ii_2944))), (true, ys_1940 (snd (snd ii_2944))))
      in
      let xs'_1966 i_2924 = snd (fst (xs'__ys_1965 ((true, i_2924), (false, 0)))) in
      let ys_1967 i_2917 = snd (snd (xs'__ys_1965 ((false, 0), (true, i_2917)))) in
      let r_append_1968 = append_1165 xs'__ys_1965 in
      let r_append_xs'__ys_0_1969 i_2906 = snd (#0 (r_append_1968 ((true, i_2906), (false, 0), (false, 0)))) in
      let r_append_xs'__ys_1_1970 i_2896 = snd (#1 (r_append_1968 ((false, 0), (true, i_2896), (false, 0)))) in
      let r_append_xs'__ys_2_1971 i_2886 = snd (#2 (r_append_1968 ((false, 0), (false, 0), (true, i_2886)))) in
      let rs'_1195 i_1369 =
        if i_1369 = 0 then
          (true, snd r_xs_1960)
        else
          let r_r_append_2885 = r_append_1968 ((true, i_1369 - 1), (false, 0), (false, 0)) in
          let r_r1_1976 = snd (#0 r_r_append_2885) in
          let r_rs_0_1977 = fst r_r1_1976 in
          let r_rs_1_1978 = snd r_r1_1976 in
          r_r1_1976
      in
      let rs'__xs__ys_1986 iii_2831 =
        if fst (#0 iii_2831) = false then
          if fst (#1 iii_2831) = false then
            if fst (#2 iii_2831) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2831))))
          else
            if fst (#2 iii_2831) = false then
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2831))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2831))), (true, ys_1940 (snd (#2 iii_2831))))
        else
          if fst (#1 iii_2831) = false then
            if fst (#2 iii_2831) = false then
              ((true, rs'_1195 (snd (#0 iii_2831))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, rs'_1195 (snd (#0 iii_2831))), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2831))))
          else
            if fst (#2 iii_2831) = false then
              ((true, rs'_1195 (snd (#0 iii_2831))), (true, xs_1939 (snd (#1 iii_2831))), (false, (true, 0)))
            else
              ((true, rs'_1195 (snd (#0 iii_2831))), (true, xs_1939 (snd (#1 iii_2831))), 
               (true, ys_1940 (snd (#2 iii_2831))))
      in
      rs'__xs__ys_1986
    else
      let bot_1820 = _|_ in
      let bot__xs__ys_1954 iii_2519 =
        if fst (#0 iii_2519) = false then
          if fst (#1 iii_2519) = false then
            if fst (#2 iii_2519) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2519))))
          else
            if fst (#2 iii_2519) = false then
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2519))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2519))), (true, ys_1940 (snd (#2 iii_2519))))
        else
          if fst (#1 iii_2519) = false then
            if fst (#2 iii_2519) = false then
              ((true, bot_1820 (snd (#0 iii_2519))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, bot_1820 (snd (#0 iii_2519))), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2519))))
          else
            if fst (#2 iii_2519) = false then
              ((true, bot_1820 (snd (#0 iii_2519))), (true, xs_1939 (snd (#1 iii_2519))), (false, (true, 0)))
            else
              ((true, bot_1820 (snd (#0 iii_2519))), (true, xs_1939 (snd (#1 iii_2519))), 
               (true, ys_1940 (snd (#2 iii_2519))))
      in
      bot__xs__ys_1954
in
let main_1017 i_1018 n_1019 =
  let r_make_list_1991 = make_list_1008 n_1019 in
  let f_1732 x_1560 = (false, 0) in
  let r_make_list__f_1999 ix_2285 =
    if fst (fst ix_2285) = false then
      if fst (snd ix_2285) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, f_1732 (snd (snd ix_2285))))
    else
      if fst (snd ix_2285) = false then
        ((true, r_make_list_1991 (snd (fst ix_2285))), (false, (true, 0)))
      else
        ((true, r_make_list_1991 (snd (fst ix_2285))), (true, f_1732 (snd (snd ix_2285))))
  in
  let xs_2000 i_2265 = snd (fst (r_make_list__f_1999 ((true, i_2265), (false, 0)))) in
  let f_2001 x_2258 = snd (snd (r_make_list__f_1999 ((false, 0), (true, x_2258)))) in
  let r_append_2002 = append_1165 r_make_list__f_1999 in
  let r_append_xs__f_0_2003 i_2247 = snd (#0 (r_append_2002 ((true, i_2247), (false, 0), (false, 0)))) in
  let r_append_xs__f_1_2004 i_2237 = snd (#1 (r_append_2002 ((false, 0), (true, i_2237), (false, 0)))) in
  let r_append_xs__f_2_2005 i_2227 = snd (#2 (r_append_2002 ((false, 0), (false, 0), (true, i_2227)))) in
  let r_r_append_2226 = r_append_2002 ((true, i_1018), (false, 0), (false, 0)) in
  let r_r_make_list_2009 = r_make_list_1991 i_1018 in
  let r_r1_2006 = snd (#0 r_r_append_2226) in
  let r_ys_0_2007 = fst r_r1_2006 in
  let r_xs_0_2010 = fst r_r_make_list_2009 in
  if snd r_r1_2006 = snd r_r_make_list_2009 then
    ()
  else
    {fail} ()
in
let r_f_2015 = rand_int () in
let r_f_2017 = rand_int () in
let r_main_2018 = main_1017 r_f_2015 in
let r_r_main_2019 = r_main_2018 r_f_2017 in
()

elim_unused_let:
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1236 -> (false, 0)
  else
    let r_f_1921 = rand_int () in
    let r_make_list_1924 = make_list_1008 (n_1009 - 1) in
    fun i_1226 -> (if i_1226 = 0 then
                     (true, r_f_1921)
                   else
                     r_make_list_1924 (i_1226 - 1))
in
let rec append_1165 xs__ys_1023 =
  let xs_1939 i_3153 = snd (fst (xs__ys_1023 ((true, i_3153), (false, 0)))) in
  let ys_1940 i_3146 = snd (snd (xs__ys_1023 ((false, 0), (true, i_3146)))) in
  let r_xs__ys_3145 = xs__ys_1023 ((true, 0), (false, 0)) in
  let r_xs_1941 = snd (fst r_xs__ys_3145) in
  if fst r_xs_1941 = false then
    let ys__xs__ys_1990 iii_3100 =
      if fst (#0 iii_3100) = false then
        if fst (#1 iii_3100) = false then
          if fst (#2 iii_3100) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_3100))))
        else
          if fst (#2 iii_3100) = false then
            ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_3100))), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_3100))), (true, ys_1940 (snd (#2 iii_3100))))
      else
        if fst (#1 iii_3100) = false then
          if fst (#2 iii_3100) = false then
            ((true, ys_1940 (snd (#0 iii_3100))), (false, (true, 0)), (false, (true, 0)))
          else
            ((true, ys_1940 (snd (#0 iii_3100))), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_3100))))
        else
          if fst (#2 iii_3100) = false then
            ((true, ys_1940 (snd (#0 iii_3100))), (true, xs_1939 (snd (#1 iii_3100))), (false, (true, 0)))
          else
            ((true, ys_1940 (snd (#0 iii_3100))), (true, xs_1939 (snd (#1 iii_3100))), 
             (true, ys_1940 (snd (#2 iii_3100))))
    in
    ys__xs__ys_1990
  else
    let r_xs_1946 = snd (fst r_xs__ys_3145) in
    if fst r_xs_1946 <> false then
      let xs'_1014 x_1269 =
        let r_xs__ys_3002 = xs__ys_1023 ((true, x_1269 + 1), (false, 0)) in
        let r_xs_1957 = snd (fst r_xs__ys_3002) in
        r_xs_1957
      in
      let r_xs_1960 = snd (fst r_xs__ys_3145) in
      let xs'__ys_1965 ii_2944 =
        if fst (fst ii_2944) = false then
          if fst (snd ii_2944) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, ys_1940 (snd (snd ii_2944))))
        else
          if fst (snd ii_2944) = false then
            ((true, xs'_1014 (snd (fst ii_2944))), (false, (true, 0)))
          else
            ((true, xs'_1014 (snd (fst ii_2944))), (true, ys_1940 (snd (snd ii_2944))))
      in
      let xs'_1966 i_2924 = snd (fst (xs'__ys_1965 ((true, i_2924), (false, 0)))) in
      let ys_1967 i_2917 = snd (snd (xs'__ys_1965 ((false, 0), (true, i_2917)))) in
      let r_append_1968 = append_1165 xs'__ys_1965 in
      let r_append_xs'__ys_0_1969 i_2906 = snd (#0 (r_append_1968 ((true, i_2906), (false, 0), (false, 0)))) in
      let r_append_xs'__ys_1_1970 i_2896 = snd (#1 (r_append_1968 ((false, 0), (true, i_2896), (false, 0)))) in
      let r_append_xs'__ys_2_1971 i_2886 = snd (#2 (r_append_1968 ((false, 0), (false, 0), (true, i_2886)))) in
      let rs'_1195 i_1369 =
        if i_1369 = 0 then
          (true, snd r_xs_1960)
        else
          let r_r_append_2885 = r_append_1968 ((true, i_1369 - 1), (false, 0), (false, 0)) in
          let r_r1_1976 = snd (#0 r_r_append_2885) in
          r_r1_1976
      in
      let rs'__xs__ys_1986 iii_2831 =
        if fst (#0 iii_2831) = false then
          if fst (#1 iii_2831) = false then
            if fst (#2 iii_2831) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2831))))
          else
            if fst (#2 iii_2831) = false then
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2831))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2831))), (true, ys_1940 (snd (#2 iii_2831))))
        else
          if fst (#1 iii_2831) = false then
            if fst (#2 iii_2831) = false then
              ((true, rs'_1195 (snd (#0 iii_2831))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, rs'_1195 (snd (#0 iii_2831))), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2831))))
          else
            if fst (#2 iii_2831) = false then
              ((true, rs'_1195 (snd (#0 iii_2831))), (true, xs_1939 (snd (#1 iii_2831))), (false, (true, 0)))
            else
              ((true, rs'_1195 (snd (#0 iii_2831))), (true, xs_1939 (snd (#1 iii_2831))), 
               (true, ys_1940 (snd (#2 iii_2831))))
      in
      rs'__xs__ys_1986
    else
      let bot_1820 = _|_ in
      let bot__xs__ys_1954 iii_2519 =
        if fst (#0 iii_2519) = false then
          if fst (#1 iii_2519) = false then
            if fst (#2 iii_2519) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2519))))
          else
            if fst (#2 iii_2519) = false then
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2519))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2519))), (true, ys_1940 (snd (#2 iii_2519))))
        else
          if fst (#1 iii_2519) = false then
            if fst (#2 iii_2519) = false then
              ((true, bot_1820 (snd (#0 iii_2519))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, bot_1820 (snd (#0 iii_2519))), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2519))))
          else
            if fst (#2 iii_2519) = false then
              ((true, bot_1820 (snd (#0 iii_2519))), (true, xs_1939 (snd (#1 iii_2519))), (false, (true, 0)))
            else
              ((true, bot_1820 (snd (#0 iii_2519))), (true, xs_1939 (snd (#1 iii_2519))), 
               (true, ys_1940 (snd (#2 iii_2519))))
      in
      bot__xs__ys_1954
in
let main_1017 i_1018 n_1019 =
  let r_make_list_1991 = make_list_1008 n_1019 in
  let f_1732 x_1560 = (false, 0) in
  let r_make_list__f_1999 ix_2285 =
    if fst (fst ix_2285) = false then
      if fst (snd ix_2285) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, f_1732 (snd (snd ix_2285))))
    else
      if fst (snd ix_2285) = false then
        ((true, r_make_list_1991 (snd (fst ix_2285))), (false, (true, 0)))
      else
        ((true, r_make_list_1991 (snd (fst ix_2285))), (true, f_1732 (snd (snd ix_2285))))
  in
  let xs_2000 i_2265 = snd (fst (r_make_list__f_1999 ((true, i_2265), (false, 0)))) in
  let f_2001 x_2258 = snd (snd (r_make_list__f_1999 ((false, 0), (true, x_2258)))) in
  let r_append_2002 = append_1165 r_make_list__f_1999 in
  let r_append_xs__f_0_2003 i_2247 = snd (#0 (r_append_2002 ((true, i_2247), (false, 0), (false, 0)))) in
  let r_append_xs__f_1_2004 i_2237 = snd (#1 (r_append_2002 ((false, 0), (true, i_2237), (false, 0)))) in
  let r_append_xs__f_2_2005 i_2227 = snd (#2 (r_append_2002 ((false, 0), (false, 0), (true, i_2227)))) in
  let r_r_append_2226 = r_append_2002 ((true, i_1018), (false, 0), (false, 0)) in
  let r_r_make_list_2009 = r_make_list_1991 i_1018 in
  let r_r1_2006 = snd (#0 r_r_append_2226) in
  if snd r_r1_2006 = snd r_r_make_list_2009 then
    ()
  else
    {fail} ()
in
let r_f_2015 = rand_int () in
let r_f_2017 = rand_int () in
let r_main_2018 = main_1017 r_f_2015 in
let r_r_main_2019 = r_main_2018 r_f_2017 in
()

TUPLE: (true, bot_1820 (snd (#0 iii_2519))), (true, xs_1939 (snd (#1 iii_2519))), (true, ys_1940 (snd (#2 iii_2519)))
bot_1820
TUPLE: (true, bot_1820 (snd (#0 iii_2519))), (true, xs_1939 (snd (#1 iii_2519))), (false, (true, 0))
bot_1820
TUPLE: (true, bot_1820 (snd (#0 iii_2519))), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2519)))
bot_1820
TUPLE: (false, (true, 0)), (true, xs_1939 (snd (#1 iii_2519))), (true, ys_1940 (snd (#2 iii_2519)))
xs_1939
ys_1940
compose:
   xs_1939, snd
            (fst
             (xs__ys_1023
               (let x1_3302 = let x1_3294 = true in
                              let x2_3295 = x_3292 in
                              (x1_3294, x2_3295) in
                let x2_3303 = let x1_3298 = false in
                              let x2_3299 = 0 in
                              (x1_3298, x2_3299) in
                (x1_3302, x2_3303))));
   ys_1940, snd
            (snd
             (xs__ys_1023
               (let x1_3314 = let x1_3306 = false in
                              let x2_3307 = 0 in
                              (x1_3306, x2_3307) in
                let x2_3315 = let x1_3310 = true in
                              let x2_3311 = x_3293 in
                              (x1_3310, x2_3311) in
                (x1_3314, x2_3315))));

PB: x:xs_1939
CHECK: snd
       (fst
        (xs__ys_1023
          (let x1_3302 = let x1_3294 = true in
                         let x2_3295 = x_3292 in
                         (x1_3294, x2_3295) in
           let x2_3303 = let x1_3298 = false in
                         let x2_3299 = 0 in
                         (x1_3298, x2_3299) in
           (x1_3302, x2_3303))))
PB: x:ys_1940
CHECK: snd
       (snd
        (xs__ys_1023
          (let x1_3314 = let x1_3306 = false in
                         let x2_3307 = 0 in
                         (x1_3306, x2_3307) in
           let x2_3315 = let x1_3310 = true in
                         let x2_3311 = x_3293 in
                         (x1_3310, x2_3311) in
           (x1_3314, x2_3315))))
compose_let
xs_1939:snd
        (fst
         (xs__ys_1023
           (let x1_3302 = let x1_3294 = true in
                          let x2_3295 = x_3292 in
                          (x1_3294, x2_3295) in
            let x2_3303 = let x1_3298 = false in
                          let x2_3299 = 0 in
                          (x1_3298, x2_3299) in
            (x1_3302, x2_3303))))

ys_1940:snd
        (snd
         (xs__ys_1023
           (let x1_3314 = let x1_3306 = false in
                          let x2_3307 = 0 in
                          (x1_3306, x2_3307) in
            let x2_3315 = let x1_3310 = true in
                          let x2_3311 = x_3293 in
                          (x1_3310, x2_3311) in
            (x1_3314, x2_3315))))

ADD_fs: xs_1939, ys_1940
ADD: (xs__ys_3318:(int -> int -> ((bool * int) * (bool * int))))
TUPLE: (true, xs'_1014 (snd (fst ii_2944))), (true, ys_1940 (snd (snd ii_2944)))
xs'_1014
ys_1940
compose:
   xs'_1014, let r_xs__ys_3002 =
               xs__ys_1023
                 (let x1_3347 = let x1_3339 = true in
                                let x2_3340 = x_3337 + 1 in
                                (x1_3339, x2_3340) in
                  let x2_3348 = let x1_3343 = false in
                                let x2_3344 = 0 in
                                (x1_3343, x2_3344) in
                  (x1_3347, x2_3348))
             in
             let r_xs_1957 = snd (fst r_xs__ys_3002) in
             r_xs_1957;
   ys_1940, snd
            (snd
             (xs__ys_1023
               (let x1_3359 = let x1_3351 = false in
                              let x2_3352 = 0 in
                              (x1_3351, x2_3352) in
                let x2_3360 = let x1_3355 = true in
                              let x2_3356 = x_3338 in
                              (x1_3355, x2_3356) in
                (x1_3359, x2_3360))));

PB: x:xs'_1014
CHECK: r_xs_1957
CHECK: snd (fst r_xs__ys_3002)
CHECK: xs__ys_1023
         (let x1_3347 = let x1_3339 = true in
                        let x2_3340 = x_3337 + 1 in
                        (x1_3339, x2_3340) in
          let x2_3348 = let x1_3343 = false in
                        let x2_3344 = 0 in
                        (x1_3343, x2_3344) in
          (x1_3347, x2_3348))
PB: x:ys_1940
CHECK: snd
       (snd
        (xs__ys_1023
          (let x1_3359 = let x1_3351 = false in
                         let x2_3352 = 0 in
                         (x1_3351, x2_3352) in
           let x2_3360 = let x1_3355 = true in
                         let x2_3356 = x_3338 in
                         (x1_3355, x2_3356) in
           (x1_3359, x2_3360))))
compose_let
xs'_1014:let r_xs__ys_3002 =
           xs__ys_1023
             (let x1_3347 = let x1_3339 = true in
                            let x2_3340 = x_3337 + 1 in
                            (x1_3339, x2_3340) in
              let x2_3348 = let x1_3343 = false in
                            let x2_3344 = 0 in
                            (x1_3343, x2_3344) in
              (x1_3347, x2_3348))
         in
         let r_xs_1957 = snd (fst r_xs__ys_3002) in
         r_xs_1957

ys_1940:snd
        (snd
         (xs__ys_1023
           (let x1_3359 = let x1_3351 = false in
                          let x2_3352 = 0 in
                          (x1_3351, x2_3352) in
            let x2_3360 = let x1_3355 = true in
                          let x2_3356 = x_3338 in
                          (x1_3355, x2_3356) in
            (x1_3359, x2_3360))))

ADD_fs: xs'_1014, ys_1940
ADD: (xs'__ys_3363:(int -> int -> ((bool * int) * (bool * int))))
TUPLE: (true, rs'_1195 (snd (#0 iii_2831))), (true, xs_1939 (snd (#1 iii_2831))), (true, ys_1940 (snd (#2 iii_2831)))
rs'_1195
xs_1939
ys_1940
compose:
   rs'_1195, if x_3377 = 0 then
               let x1_3398 = true in
               let x2_3399 = snd r_xs_1960 in
               (x1_3398, x2_3399)
             else
               let r_r_append_2885 =
                 r_append_1968
                   (let x1_3392 = let x1_3380 = true in
                                  let x2_3381 = x_3377 - 1 in
                                  (x1_3380, x2_3381) in
                    let x2_3393 = let x1_3384 = false in
                                  let x2_3385 = 0 in
                                  (x1_3384, x2_3385) in
                    let x3_3394 = let x1_3388 = false in
                                  let x2_3389 = 0 in
                                  (x1_3388, x2_3389) in
                    (x1_3392, x2_3393, x3_3394))
               in
               let r_r1_1976 = snd (#0 r_r_append_2885) in
               r_r1_1976;
   xs_1939, snd
            (fst
             (xs__ys_1023
               (let x1_3410 = let x1_3402 = true in
                              let x2_3403 = x_3378 in
                              (x1_3402, x2_3403) in
                let x2_3411 = let x1_3406 = false in
                              let x2_3407 = 0 in
                              (x1_3406, x2_3407) in
                (x1_3410, x2_3411))));
   ys_1940, snd
            (snd
             (xs__ys_1023
               (let x1_3422 = let x1_3414 = false in
                              let x2_3415 = 0 in
                              (x1_3414, x2_3415) in
                let x2_3423 = let x1_3418 = true in
                              let x2_3419 = x_3379 in
                              (x1_3418, x2_3419) in
                (x1_3422, x2_3423))));

compose:
   rs'_1195, let r_r_append_2885 =
               r_append_1968
                 (let x1_3392 = let x1_3380 = true in
                                let x2_3381 = x_3377 - 1 in
                                (x1_3380, x2_3381) in
                  let x2_3393 = let x1_3384 = false in
                                let x2_3385 = 0 in
                                (x1_3384, x2_3385) in
                  let x3_3394 = let x1_3388 = false in
                                let x2_3389 = 0 in
                                (x1_3388, x2_3389) in
                  (x1_3392, x2_3393, x3_3394))
             in
             let r_r1_1976 = snd (#0 r_r_append_2885) in
             r_r1_1976;
   xs_1939, snd
            (fst
             (xs__ys_1023
               (let x1_3410 = let x1_3402 = true in
                              let x2_3403 = x_3378 in
                              (x1_3402, x2_3403) in
                let x2_3411 = let x1_3406 = false in
                              let x2_3407 = 0 in
                              (x1_3406, x2_3407) in
                (x1_3410, x2_3411))));
   ys_1940, snd
            (snd
             (xs__ys_1023
               (let x1_3422 = let x1_3414 = false in
                              let x2_3415 = 0 in
                              (x1_3414, x2_3415) in
                let x2_3423 = let x1_3418 = true in
                              let x2_3419 = x_3379 in
                              (x1_3418, x2_3419) in
                (x1_3422, x2_3423))));

PB: x:rs'_1195
CHECK: r_r1_1976
CHECK: snd (#0 r_r_append_2885)
CHECK: r_append_1968
         (let x1_3392 = let x1_3380 = true in
                        let x2_3381 = x_3377 - 1 in
                        (x1_3380, x2_3381) in
          let x2_3393 = let x1_3384 = false in
                        let x2_3385 = 0 in
                        (x1_3384, x2_3385) in
          let x3_3394 = let x1_3388 = false in
                        let x2_3389 = 0 in
                        (x1_3388, x2_3389) in
          (x1_3392, x2_3393, x3_3394))
PB: x:xs_1939
CHECK: snd
       (fst
        (xs__ys_1023
          (let x1_3410 = let x1_3402 = true in
                         let x2_3403 = x_3378 in
                         (x1_3402, x2_3403) in
           let x2_3411 = let x1_3406 = false in
                         let x2_3407 = 0 in
                         (x1_3406, x2_3407) in
           (x1_3410, x2_3411))))
PB: x:ys_1940
CHECK: snd
       (snd
        (xs__ys_1023
          (let x1_3422 = let x1_3414 = false in
                         let x2_3415 = 0 in
                         (x1_3414, x2_3415) in
           let x2_3423 = let x1_3418 = true in
                         let x2_3419 = x_3379 in
                         (x1_3418, x2_3419) in
           (x1_3422, x2_3423))))
compose_let
rs'_1195:let r_r_append_2885 =
           r_append_1968
             (let x1_3392 = let x1_3380 = true in
                            let x2_3381 = x_3377 - 1 in
                            (x1_3380, x2_3381) in
              let x2_3393 = let x1_3384 = false in
                            let x2_3385 = 0 in
                            (x1_3384, x2_3385) in
              let x3_3394 = let x1_3388 = false in
                            let x2_3389 = 0 in
                            (x1_3388, x2_3389) in
              (x1_3392, x2_3393, x3_3394))
         in
         let r_r1_1976 = snd (#0 r_r_append_2885) in
         r_r1_1976

xs_1939:snd
        (fst
         (xs__ys_1023
           (let x1_3410 = let x1_3402 = true in
                          let x2_3403 = x_3378 in
                          (x1_3402, x2_3403) in
            let x2_3411 = let x1_3406 = false in
                          let x2_3407 = 0 in
                          (x1_3406, x2_3407) in
            (x1_3410, x2_3411))))

ys_1940:snd
        (snd
         (xs__ys_1023
           (let x1_3422 = let x1_3414 = false in
                          let x2_3415 = 0 in
                          (x1_3414, x2_3415) in
            let x2_3423 = let x1_3418 = true in
                          let x2_3419 = x_3379 in
                          (x1_3418, x2_3419) in
            (x1_3422, x2_3423))))

compose:
   rs'_1195, let x1_3398 = true in
             let x2_3399 = snd r_xs_1960 in
             (x1_3398, x2_3399);
   xs_1939, snd
            (fst
             (xs__ys_1023
               (let x1_3410 = let x1_3402 = true in
                              let x2_3403 = x_3378 in
                              (x1_3402, x2_3403) in
                let x2_3411 = let x1_3406 = false in
                              let x2_3407 = 0 in
                              (x1_3406, x2_3407) in
                (x1_3410, x2_3411))));
   ys_1940, snd
            (snd
             (xs__ys_1023
               (let x1_3422 = let x1_3414 = false in
                              let x2_3415 = 0 in
                              (x1_3414, x2_3415) in
                let x2_3423 = let x1_3418 = true in
                              let x2_3419 = x_3379 in
                              (x1_3418, x2_3419) in
                (x1_3422, x2_3423))));

PB: x:rs'_1195
CHECK: (x1_3398, x2_3399)
CHECK: snd r_xs_1960
CHECK: true
PB: x:xs_1939
CHECK: snd
       (fst
        (xs__ys_1023
          (let x1_3410 = let x1_3402 = true in
                         let x2_3403 = x_3378 in
                         (x1_3402, x2_3403) in
           let x2_3411 = let x1_3406 = false in
                         let x2_3407 = 0 in
                         (x1_3406, x2_3407) in
           (x1_3410, x2_3411))))
PB: x:ys_1940
CHECK: snd
       (snd
        (xs__ys_1023
          (let x1_3422 = let x1_3414 = false in
                         let x2_3415 = 0 in
                         (x1_3414, x2_3415) in
           let x2_3423 = let x1_3418 = true in
                         let x2_3419 = x_3379 in
                         (x1_3418, x2_3419) in
           (x1_3422, x2_3423))))
compose_let
rs'_1195:let x1_3398 = true in
         let x2_3399 = snd r_xs_1960 in
         (x1_3398, x2_3399)

xs_1939:snd
        (fst
         (xs__ys_1023
           (let x1_3410 = let x1_3402 = true in
                          let x2_3403 = x_3378 in
                          (x1_3402, x2_3403) in
            let x2_3411 = let x1_3406 = false in
                          let x2_3407 = 0 in
                          (x1_3406, x2_3407) in
            (x1_3410, x2_3411))))

ys_1940:snd
        (snd
         (xs__ys_1023
           (let x1_3422 = let x1_3414 = false in
                          let x2_3415 = 0 in
                          (x1_3414, x2_3415) in
            let x2_3423 = let x1_3418 = true in
                          let x2_3419 = x_3379 in
                          (x1_3418, x2_3419) in
            (x1_3422, x2_3423))))

ADD_fs: rs'_1195, xs_1939, ys_1940
ADD: (rs'__xs__ys_3426:(int -> int -> int -> ((bool * int) * (bool * int) * (bool * int))))
TUPLE: (true, rs'_1195 (snd (#0 iii_2831))), (true, xs_1939 (snd (#1 iii_2831))), (false, (true, 0))
rs'_1195
xs_1939
compose:
   rs'_1195, if x_3455 = 0 then
               let x1_3475 = true in
               let x2_3476 = snd r_xs_1960 in
               (x1_3475, x2_3476)
             else
               let r_r_append_2885 =
                 r_append_1968
                   (let x1_3469 = let x1_3457 = true in
                                  let x2_3458 = x_3455 - 1 in
                                  (x1_3457, x2_3458) in
                    let x2_3470 = let x1_3461 = false in
                                  let x2_3462 = 0 in
                                  (x1_3461, x2_3462) in
                    let x3_3471 = let x1_3465 = false in
                                  let x2_3466 = 0 in
                                  (x1_3465, x2_3466) in
                    (x1_3469, x2_3470, x3_3471))
               in
               let r_r1_1976 = snd (#0 r_r_append_2885) in
               r_r1_1976;
   xs_1939, snd
            (fst
             (xs__ys_1023
               (let x1_3487 = let x1_3479 = true in
                              let x2_3480 = x_3456 in
                              (x1_3479, x2_3480) in
                let x2_3488 = let x1_3483 = false in
                              let x2_3484 = 0 in
                              (x1_3483, x2_3484) in
                (x1_3487, x2_3488))));

compose:
   rs'_1195, let r_r_append_2885 =
               r_append_1968
                 (let x1_3469 = let x1_3457 = true in
                                let x2_3458 = x_3455 - 1 in
                                (x1_3457, x2_3458) in
                  let x2_3470 = let x1_3461 = false in
                                let x2_3462 = 0 in
                                (x1_3461, x2_3462) in
                  let x3_3471 = let x1_3465 = false in
                                let x2_3466 = 0 in
                                (x1_3465, x2_3466) in
                  (x1_3469, x2_3470, x3_3471))
             in
             let r_r1_1976 = snd (#0 r_r_append_2885) in
             r_r1_1976;
   xs_1939, snd
            (fst
             (xs__ys_1023
               (let x1_3487 = let x1_3479 = true in
                              let x2_3480 = x_3456 in
                              (x1_3479, x2_3480) in
                let x2_3488 = let x1_3483 = false in
                              let x2_3484 = 0 in
                              (x1_3483, x2_3484) in
                (x1_3487, x2_3488))));

PB: x:rs'_1195
CHECK: r_r1_1976
CHECK: snd (#0 r_r_append_2885)
CHECK: r_append_1968
         (let x1_3469 = let x1_3457 = true in
                        let x2_3458 = x_3455 - 1 in
                        (x1_3457, x2_3458) in
          let x2_3470 = let x1_3461 = false in
                        let x2_3462 = 0 in
                        (x1_3461, x2_3462) in
          let x3_3471 = let x1_3465 = false in
                        let x2_3466 = 0 in
                        (x1_3465, x2_3466) in
          (x1_3469, x2_3470, x3_3471))
PB: x:xs_1939
CHECK: snd
       (fst
        (xs__ys_1023
          (let x1_3487 = let x1_3479 = true in
                         let x2_3480 = x_3456 in
                         (x1_3479, x2_3480) in
           let x2_3488 = let x1_3483 = false in
                         let x2_3484 = 0 in
                         (x1_3483, x2_3484) in
           (x1_3487, x2_3488))))
compose_let
rs'_1195:let r_r_append_2885 =
           r_append_1968
             (let x1_3469 = let x1_3457 = true in
                            let x2_3458 = x_3455 - 1 in
                            (x1_3457, x2_3458) in
              let x2_3470 = let x1_3461 = false in
                            let x2_3462 = 0 in
                            (x1_3461, x2_3462) in
              let x3_3471 = let x1_3465 = false in
                            let x2_3466 = 0 in
                            (x1_3465, x2_3466) in
              (x1_3469, x2_3470, x3_3471))
         in
         let r_r1_1976 = snd (#0 r_r_append_2885) in
         r_r1_1976

xs_1939:snd
        (fst
         (xs__ys_1023
           (let x1_3487 = let x1_3479 = true in
                          let x2_3480 = x_3456 in
                          (x1_3479, x2_3480) in
            let x2_3488 = let x1_3483 = false in
                          let x2_3484 = 0 in
                          (x1_3483, x2_3484) in
            (x1_3487, x2_3488))))

compose:
   rs'_1195, let x1_3475 = true in
             let x2_3476 = snd r_xs_1960 in
             (x1_3475, x2_3476);
   xs_1939, snd
            (fst
             (xs__ys_1023
               (let x1_3487 = let x1_3479 = true in
                              let x2_3480 = x_3456 in
                              (x1_3479, x2_3480) in
                let x2_3488 = let x1_3483 = false in
                              let x2_3484 = 0 in
                              (x1_3483, x2_3484) in
                (x1_3487, x2_3488))));

PB: x:rs'_1195
CHECK: (x1_3475, x2_3476)
CHECK: snd r_xs_1960
CHECK: true
PB: x:xs_1939
CHECK: snd
       (fst
        (xs__ys_1023
          (let x1_3487 = let x1_3479 = true in
                         let x2_3480 = x_3456 in
                         (x1_3479, x2_3480) in
           let x2_3488 = let x1_3483 = false in
                         let x2_3484 = 0 in
                         (x1_3483, x2_3484) in
           (x1_3487, x2_3488))))
compose_let
rs'_1195:let x1_3475 = true in
         let x2_3476 = snd r_xs_1960 in
         (x1_3475, x2_3476)

xs_1939:snd
        (fst
         (xs__ys_1023
           (let x1_3487 = let x1_3479 = true in
                          let x2_3480 = x_3456 in
                          (x1_3479, x2_3480) in
            let x2_3488 = let x1_3483 = false in
                          let x2_3484 = 0 in
                          (x1_3483, x2_3484) in
            (x1_3487, x2_3488))))

ADD_fs: rs'_1195, xs_1939
ADD: (rs'__xs_3491:(int -> int -> ((bool * int) * (bool * int))))
TUPLE: (true, rs'_1195 (snd (#0 iii_2831))), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2831)))
rs'_1195
ys_1940
compose:
   rs'_1195, if x_3516 = 0 then
               let x1_3536 = true in
               let x2_3537 = snd r_xs_1960 in
               (x1_3536, x2_3537)
             else
               let r_r_append_2885 =
                 r_append_1968
                   (let x1_3530 = let x1_3518 = true in
                                  let x2_3519 = x_3516 - 1 in
                                  (x1_3518, x2_3519) in
                    let x2_3531 = let x1_3522 = false in
                                  let x2_3523 = 0 in
                                  (x1_3522, x2_3523) in
                    let x3_3532 = let x1_3526 = false in
                                  let x2_3527 = 0 in
                                  (x1_3526, x2_3527) in
                    (x1_3530, x2_3531, x3_3532))
               in
               let r_r1_1976 = snd (#0 r_r_append_2885) in
               r_r1_1976;
   ys_1940, snd
            (snd
             (xs__ys_1023
               (let x1_3548 = let x1_3540 = false in
                              let x2_3541 = 0 in
                              (x1_3540, x2_3541) in
                let x2_3549 = let x1_3544 = true in
                              let x2_3545 = x_3517 in
                              (x1_3544, x2_3545) in
                (x1_3548, x2_3549))));

compose:
   rs'_1195, let r_r_append_2885 =
               r_append_1968
                 (let x1_3530 = let x1_3518 = true in
                                let x2_3519 = x_3516 - 1 in
                                (x1_3518, x2_3519) in
                  let x2_3531 = let x1_3522 = false in
                                let x2_3523 = 0 in
                                (x1_3522, x2_3523) in
                  let x3_3532 = let x1_3526 = false in
                                let x2_3527 = 0 in
                                (x1_3526, x2_3527) in
                  (x1_3530, x2_3531, x3_3532))
             in
             let r_r1_1976 = snd (#0 r_r_append_2885) in
             r_r1_1976;
   ys_1940, snd
            (snd
             (xs__ys_1023
               (let x1_3548 = let x1_3540 = false in
                              let x2_3541 = 0 in
                              (x1_3540, x2_3541) in
                let x2_3549 = let x1_3544 = true in
                              let x2_3545 = x_3517 in
                              (x1_3544, x2_3545) in
                (x1_3548, x2_3549))));

PB: x:rs'_1195
CHECK: r_r1_1976
CHECK: snd (#0 r_r_append_2885)
CHECK: r_append_1968
         (let x1_3530 = let x1_3518 = true in
                        let x2_3519 = x_3516 - 1 in
                        (x1_3518, x2_3519) in
          let x2_3531 = let x1_3522 = false in
                        let x2_3523 = 0 in
                        (x1_3522, x2_3523) in
          let x3_3532 = let x1_3526 = false in
                        let x2_3527 = 0 in
                        (x1_3526, x2_3527) in
          (x1_3530, x2_3531, x3_3532))
PB: x:ys_1940
CHECK: snd
       (snd
        (xs__ys_1023
          (let x1_3548 = let x1_3540 = false in
                         let x2_3541 = 0 in
                         (x1_3540, x2_3541) in
           let x2_3549 = let x1_3544 = true in
                         let x2_3545 = x_3517 in
                         (x1_3544, x2_3545) in
           (x1_3548, x2_3549))))
compose_let
rs'_1195:let r_r_append_2885 =
           r_append_1968
             (let x1_3530 = let x1_3518 = true in
                            let x2_3519 = x_3516 - 1 in
                            (x1_3518, x2_3519) in
              let x2_3531 = let x1_3522 = false in
                            let x2_3523 = 0 in
                            (x1_3522, x2_3523) in
              let x3_3532 = let x1_3526 = false in
                            let x2_3527 = 0 in
                            (x1_3526, x2_3527) in
              (x1_3530, x2_3531, x3_3532))
         in
         let r_r1_1976 = snd (#0 r_r_append_2885) in
         r_r1_1976

ys_1940:snd
        (snd
         (xs__ys_1023
           (let x1_3548 = let x1_3540 = false in
                          let x2_3541 = 0 in
                          (x1_3540, x2_3541) in
            let x2_3549 = let x1_3544 = true in
                          let x2_3545 = x_3517 in
                          (x1_3544, x2_3545) in
            (x1_3548, x2_3549))))

compose:
   rs'_1195, let x1_3536 = true in
             let x2_3537 = snd r_xs_1960 in
             (x1_3536, x2_3537);
   ys_1940, snd
            (snd
             (xs__ys_1023
               (let x1_3548 = let x1_3540 = false in
                              let x2_3541 = 0 in
                              (x1_3540, x2_3541) in
                let x2_3549 = let x1_3544 = true in
                              let x2_3545 = x_3517 in
                              (x1_3544, x2_3545) in
                (x1_3548, x2_3549))));

PB: x:rs'_1195
CHECK: (x1_3536, x2_3537)
CHECK: snd r_xs_1960
CHECK: true
PB: x:ys_1940
CHECK: snd
       (snd
        (xs__ys_1023
          (let x1_3548 = let x1_3540 = false in
                         let x2_3541 = 0 in
                         (x1_3540, x2_3541) in
           let x2_3549 = let x1_3544 = true in
                         let x2_3545 = x_3517 in
                         (x1_3544, x2_3545) in
           (x1_3548, x2_3549))))
compose_let
rs'_1195:let x1_3536 = true in
         let x2_3537 = snd r_xs_1960 in
         (x1_3536, x2_3537)

ys_1940:snd
        (snd
         (xs__ys_1023
           (let x1_3548 = let x1_3540 = false in
                          let x2_3541 = 0 in
                          (x1_3540, x2_3541) in
            let x2_3549 = let x1_3544 = true in
                          let x2_3545 = x_3517 in
                          (x1_3544, x2_3545) in
            (x1_3548, x2_3549))))

ADD_fs: rs'_1195, ys_1940
ADD: (rs'__ys_3552:(int -> int -> ((bool * int) * (bool * int))))
TUPLE: (false, (true, 0)), (true, xs_1939 (snd (#1 iii_2831))), (true, ys_1940 (snd (#2 iii_2831)))
xs_1939
ys_1940
ADD_fs: xs_1939, ys_1940
ADD: (xs__ys_3318:(int -> int -> ((bool * int) * (bool * int))))
TUPLE: (true, ys_1940 (snd (#0 iii_3100))), (true, xs_1939 (snd (#1 iii_3100))), (true, ys_1940 (snd (#2 iii_3100)))
ys_1940
xs_1939
ys_1940
compose:
   ys_1940, snd
            (snd
             (xs__ys_1023
               (let x1_3626 = let x1_3618 = false in
                              let x2_3619 = 0 in
                              (x1_3618, x2_3619) in
                let x2_3627 = let x1_3622 = true in
                              let x2_3623 = x_3615 in
                              (x1_3622, x2_3623) in
                (x1_3626, x2_3627))));
   xs_1939, snd
            (fst
             (xs__ys_1023
               (let x1_3638 = let x1_3630 = true in
                              let x2_3631 = x_3616 in
                              (x1_3630, x2_3631) in
                let x2_3639 = let x1_3634 = false in
                              let x2_3635 = 0 in
                              (x1_3634, x2_3635) in
                (x1_3638, x2_3639))));
   ys_1940, snd
            (snd
             (xs__ys_1023
               (let x1_3650 = let x1_3642 = false in
                              let x2_3643 = 0 in
                              (x1_3642, x2_3643) in
                let x2_3651 = let x1_3646 = true in
                              let x2_3647 = x_3617 in
                              (x1_3646, x2_3647) in
                (x1_3650, x2_3651))));

PB: x:ys_1940
CHECK: snd
       (snd
        (xs__ys_1023
          (let x1_3626 = let x1_3618 = false in
                         let x2_3619 = 0 in
                         (x1_3618, x2_3619) in
           let x2_3627 = let x1_3622 = true in
                         let x2_3623 = x_3615 in
                         (x1_3622, x2_3623) in
           (x1_3626, x2_3627))))
PB: x:xs_1939
CHECK: snd
       (fst
        (xs__ys_1023
          (let x1_3638 = let x1_3630 = true in
                         let x2_3631 = x_3616 in
                         (x1_3630, x2_3631) in
           let x2_3639 = let x1_3634 = false in
                         let x2_3635 = 0 in
                         (x1_3634, x2_3635) in
           (x1_3638, x2_3639))))
PB: x:ys_1940
CHECK: snd
       (snd
        (xs__ys_1023
          (let x1_3650 = let x1_3642 = false in
                         let x2_3643 = 0 in
                         (x1_3642, x2_3643) in
           let x2_3651 = let x1_3646 = true in
                         let x2_3647 = x_3617 in
                         (x1_3646, x2_3647) in
           (x1_3650, x2_3651))))
compose_let
ys_1940:snd
        (snd
         (xs__ys_1023
           (let x1_3626 = let x1_3618 = false in
                          let x2_3619 = 0 in
                          (x1_3618, x2_3619) in
            let x2_3627 = let x1_3622 = true in
                          let x2_3623 = x_3615 in
                          (x1_3622, x2_3623) in
            (x1_3626, x2_3627))))

xs_1939:snd
        (fst
         (xs__ys_1023
           (let x1_3638 = let x1_3630 = true in
                          let x2_3631 = x_3616 in
                          (x1_3630, x2_3631) in
            let x2_3639 = let x1_3634 = false in
                          let x2_3635 = 0 in
                          (x1_3634, x2_3635) in
            (x1_3638, x2_3639))))

ys_1940:snd
        (snd
         (xs__ys_1023
           (let x1_3650 = let x1_3642 = false in
                          let x2_3643 = 0 in
                          (x1_3642, x2_3643) in
            let x2_3651 = let x1_3646 = true in
                          let x2_3647 = x_3617 in
                          (x1_3646, x2_3647) in
            (x1_3650, x2_3651))))

ADD_fs: ys_1940, xs_1939, ys_1940
ADD: (ys__xs__ys_3654:(int -> int -> int -> ((bool * int) * (bool * int) * (bool * int))))
TUPLE: (true, ys_1940 (snd (#0 iii_3100))), (true, xs_1939 (snd (#1 iii_3100))), (false, (true, 0))
ys_1940
xs_1939
compose:
   ys_1940, snd
            (snd
             (xs__ys_1023
               (let x1_3684 = let x1_3676 = false in
                              let x2_3677 = 0 in
                              (x1_3676, x2_3677) in
                let x2_3685 = let x1_3680 = true in
                              let x2_3681 = x_3674 in
                              (x1_3680, x2_3681) in
                (x1_3684, x2_3685))));
   xs_1939, snd
            (fst
             (xs__ys_1023
               (let x1_3696 = let x1_3688 = true in
                              let x2_3689 = x_3675 in
                              (x1_3688, x2_3689) in
                let x2_3697 = let x1_3692 = false in
                              let x2_3693 = 0 in
                              (x1_3692, x2_3693) in
                (x1_3696, x2_3697))));

PB: x:ys_1940
CHECK: snd
       (snd
        (xs__ys_1023
          (let x1_3684 = let x1_3676 = false in
                         let x2_3677 = 0 in
                         (x1_3676, x2_3677) in
           let x2_3685 = let x1_3680 = true in
                         let x2_3681 = x_3674 in
                         (x1_3680, x2_3681) in
           (x1_3684, x2_3685))))
PB: x:xs_1939
CHECK: snd
       (fst
        (xs__ys_1023
          (let x1_3696 = let x1_3688 = true in
                         let x2_3689 = x_3675 in
                         (x1_3688, x2_3689) in
           let x2_3697 = let x1_3692 = false in
                         let x2_3693 = 0 in
                         (x1_3692, x2_3693) in
           (x1_3696, x2_3697))))
compose_let
ys_1940:snd
        (snd
         (xs__ys_1023
           (let x1_3684 = let x1_3676 = false in
                          let x2_3677 = 0 in
                          (x1_3676, x2_3677) in
            let x2_3685 = let x1_3680 = true in
                          let x2_3681 = x_3674 in
                          (x1_3680, x2_3681) in
            (x1_3684, x2_3685))))

xs_1939:snd
        (fst
         (xs__ys_1023
           (let x1_3696 = let x1_3688 = true in
                          let x2_3689 = x_3675 in
                          (x1_3688, x2_3689) in
            let x2_3697 = let x1_3692 = false in
                          let x2_3693 = 0 in
                          (x1_3692, x2_3693) in
            (x1_3696, x2_3697))))

ADD_fs: ys_1940, xs_1939
ADD: (ys__xs_3700:(int -> int -> ((bool * int) * (bool * int))))
TUPLE: (true, ys_1940 (snd (#0 iii_3100))), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_3100)))
ys_1940
ys_1940
compose:
   ys_1940, snd
            (snd
             (xs__ys_1023
               (let x1_3729 = let x1_3721 = false in
                              let x2_3722 = 0 in
                              (x1_3721, x2_3722) in
                let x2_3730 = let x1_3725 = true in
                              let x2_3726 = x_3719 in
                              (x1_3725, x2_3726) in
                (x1_3729, x2_3730))));
   ys_1940, snd
            (snd
             (xs__ys_1023
               (let x1_3741 = let x1_3733 = false in
                              let x2_3734 = 0 in
                              (x1_3733, x2_3734) in
                let x2_3742 = let x1_3737 = true in
                              let x2_3738 = x_3720 in
                              (x1_3737, x2_3738) in
                (x1_3741, x2_3742))));

PB: x:ys_1940
CHECK: snd
       (snd
        (xs__ys_1023
          (let x1_3729 = let x1_3721 = false in
                         let x2_3722 = 0 in
                         (x1_3721, x2_3722) in
           let x2_3730 = let x1_3725 = true in
                         let x2_3726 = x_3719 in
                         (x1_3725, x2_3726) in
           (x1_3729, x2_3730))))
PB: x:ys_1940
CHECK: snd
       (snd
        (xs__ys_1023
          (let x1_3741 = let x1_3733 = false in
                         let x2_3734 = 0 in
                         (x1_3733, x2_3734) in
           let x2_3742 = let x1_3737 = true in
                         let x2_3738 = x_3720 in
                         (x1_3737, x2_3738) in
           (x1_3741, x2_3742))))
compose_let
ys_1940:snd
        (snd
         (xs__ys_1023
           (let x1_3729 = let x1_3721 = false in
                          let x2_3722 = 0 in
                          (x1_3721, x2_3722) in
            let x2_3730 = let x1_3725 = true in
                          let x2_3726 = x_3719 in
                          (x1_3725, x2_3726) in
            (x1_3729, x2_3730))))

ys_1940:snd
        (snd
         (xs__ys_1023
           (let x1_3741 = let x1_3733 = false in
                          let x2_3734 = 0 in
                          (x1_3733, x2_3734) in
            let x2_3742 = let x1_3737 = true in
                          let x2_3738 = x_3720 in
                          (x1_3737, x2_3738) in
            (x1_3741, x2_3742))))

ADD_fs: ys_1940, ys_1940
ADD: (ys__ys_3745:(int -> int -> ((bool * int) * (bool * int))))
TUPLE: (false, (true, 0)), (true, xs_1939 (snd (#1 iii_3100))), (true, ys_1940 (snd (#2 iii_3100)))
xs_1939
ys_1940
ADD_fs: xs_1939, ys_1940
ADD: (xs__ys_3318:(int -> int -> ((bool * int) * (bool * int))))
TUPLE: (true, r_make_list_1991 (snd (fst ix_2285))), (true, f_1732 (snd (snd ix_2285)))
r_make_list_1991
tupled:
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1236 -> (false, 0)
  else
    let r_f_1921 = rand_int () in
    let r_make_list_1924 = make_list_1008 (n_1009 - 1) in
    fun i_1226 -> (if i_1226 = 0 then
                     (true, r_f_1921)
                   else
                     r_make_list_1924 (i_1226 - 1))
in
let rec append_1165 xs__ys_1023 =
  let xs_1939 i_3153 = snd (fst (xs__ys_1023 ((true, i_3153), (false, 0)))) in
  let ys_1940 i_3146 = snd (snd (xs__ys_1023 ((false, 0), (true, i_3146)))) in
  let rec ys__ys_3745 x_3719 x_3720 =
    let r_3748 =
      snd
      (snd
       (xs__ys_1023
         (let x1_3729 = let x1_3721 = false in
                        let x2_3722 = 0 in
                        (x1_3721, x2_3722) in
          let x2_3730 = let x1_3725 = true in
                        let x2_3726 = x_3719 in
                        (x1_3725, x2_3726) in
          (x1_3729, x2_3730))))
    in
    let r_3749 =
      snd
      (snd
       (xs__ys_1023
         (let x1_3741 = let x1_3733 = false in
                        let x2_3734 = 0 in
                        (x1_3733, x2_3734) in
          let x2_3742 = let x1_3737 = true in
                        let x2_3738 = x_3720 in
                        (x1_3737, x2_3738) in
          (x1_3741, x2_3742))))
    in
    (r_3748, r_3749)
  in
  let rec xs__ys_3318 x_3292 x_3293 =
    let r_3321 =
      snd
      (fst
       (xs__ys_1023
         (let x1_3302 = let x1_3294 = true in
                        let x2_3295 = x_3292 in
                        (x1_3294, x2_3295) in
          let x2_3303 = let x1_3298 = false in
                        let x2_3299 = 0 in
                        (x1_3298, x2_3299) in
          (x1_3302, x2_3303))))
    in
    let r_3322 =
      snd
      (snd
       (xs__ys_1023
         (let x1_3314 = let x1_3306 = false in
                        let x2_3307 = 0 in
                        (x1_3306, x2_3307) in
          let x2_3315 = let x1_3310 = true in
                        let x2_3311 = x_3293 in
                        (x1_3310, x2_3311) in
          (x1_3314, x2_3315))))
    in
    (r_3321, r_3322)
  in
  let rec ys__xs__ys_3654 x_3615 x_3616 x_3617 =
    let r_3658 =
      snd
      (snd
       (xs__ys_1023
         (let x1_3626 = let x1_3618 = false in
                        let x2_3619 = 0 in
                        (x1_3618, x2_3619) in
          let x2_3627 = let x1_3622 = true in
                        let x2_3623 = x_3615 in
                        (x1_3622, x2_3623) in
          (x1_3626, x2_3627))))
    in
    let r_3659 =
      snd
      (fst
       (xs__ys_1023
         (let x1_3638 = let x1_3630 = true in
                        let x2_3631 = x_3616 in
                        (x1_3630, x2_3631) in
          let x2_3639 = let x1_3634 = false in
                        let x2_3635 = 0 in
                        (x1_3634, x2_3635) in
          (x1_3638, x2_3639))))
    in
    let r_3660 =
      snd
      (snd
       (xs__ys_1023
         (let x1_3650 = let x1_3642 = false in
                        let x2_3643 = 0 in
                        (x1_3642, x2_3643) in
          let x2_3651 = let x1_3646 = true in
                        let x2_3647 = x_3617 in
                        (x1_3646, x2_3647) in
          (x1_3650, x2_3651))))
    in
    (r_3658, r_3659, r_3660)
  in
  let rec ys__xs_3700 x_3674 x_3675 =
    let r_3703 =
      snd
      (snd
       (xs__ys_1023
         (let x1_3684 = let x1_3676 = false in
                        let x2_3677 = 0 in
                        (x1_3676, x2_3677) in
          let x2_3685 = let x1_3680 = true in
                        let x2_3681 = x_3674 in
                        (x1_3680, x2_3681) in
          (x1_3684, x2_3685))))
    in
    let r_3704 =
      snd
      (fst
       (xs__ys_1023
         (let x1_3696 = let x1_3688 = true in
                        let x2_3689 = x_3675 in
                        (x1_3688, x2_3689) in
          let x2_3697 = let x1_3692 = false in
                        let x2_3693 = 0 in
                        (x1_3692, x2_3693) in
          (x1_3696, x2_3697))))
    in
    (r_3703, r_3704)
  in
  let r_xs__ys_3145 = xs__ys_1023 ((true, 0), (false, 0)) in
  let r_xs_1941 = snd (fst r_xs__ys_3145) in
  if fst r_xs_1941 = false then
    let ys__xs__ys_1990 iii_3100 =
      if fst (#0 iii_3100) = false then
        if fst (#1 iii_3100) = false then
          if fst (#2 iii_3100) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_3100))))
        else
          if fst (#2 iii_3100) = false then
            ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_3100))), (false, (true, 0)))
          else
            let r_3790 = xs__ys_3318 (snd (#1 iii_3100)) (snd (#2 iii_3100)) in
            ((false, (true, 0)), (true, fst r_3790), (true, snd r_3790))
      else
        if fst (#1 iii_3100) = false then
          if fst (#2 iii_3100) = false then
            ((true, ys_1940 (snd (#0 iii_3100))), (false, (true, 0)), (false, (true, 0)))
          else
            let r_3752 = ys__ys_3745 (snd (#0 iii_3100)) (snd (#2 iii_3100)) in
            ((true, fst r_3752), (false, (true, 0)), (true, snd r_3752))
        else
          if fst (#2 iii_3100) = false then
            let r_3707 = ys__xs_3700 (snd (#0 iii_3100)) (snd (#1 iii_3100)) in
            ((true, fst r_3707), (true, snd r_3707), (false, (true, 0)))
          else
            let r_3664 = ys__xs__ys_3654 (snd (#0 iii_3100)) (snd (#1 iii_3100)) (snd (#2 iii_3100)) in
            ((true, #0 r_3664), (true, #1 r_3664), (true, #2 r_3664))
    in
    ys__xs__ys_1990
  else
    let r_xs_1946 = snd (fst r_xs__ys_3145) in
    if fst r_xs_1946 <> false then
      let xs'_1014 x_1269 =
        let r_xs__ys_3002 = xs__ys_1023 ((true, x_1269 + 1), (false, 0)) in
        let r_xs_1957 = snd (fst r_xs__ys_3002) in
        r_xs_1957
      in
      let rec xs'__ys_3363 x_3337 x_3338 =
        let r_xs__ys_3002 =
          xs__ys_1023
            (let x1_3347 = let x1_3339 = true in
                           let x2_3340 = x_3337 + 1 in
                           (x1_3339, x2_3340) in
             let x2_3348 = let x1_3343 = false in
                           let x2_3344 = 0 in
                           (x1_3343, x2_3344) in
             (x1_3347, x2_3348))
        in
        let r_xs_1957 = snd (fst r_xs__ys_3002) in
        let r_3366 = r_xs_1957 in
        let r_3367 =
          snd
          (snd
           (xs__ys_1023
             (let x1_3359 = let x1_3351 = false in
                            let x2_3352 = 0 in
                            (x1_3351, x2_3352) in
              let x2_3360 = let x1_3355 = true in
                            let x2_3356 = x_3338 in
                            (x1_3355, x2_3356) in
              (x1_3359, x2_3360))))
        in
        (r_3366, r_3367)
      in
      let r_xs_1960 = snd (fst r_xs__ys_3145) in
      let xs'__ys_1965 ii_2944 =
        if fst (fst ii_2944) = false then
          if fst (snd ii_2944) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, ys_1940 (snd (snd ii_2944))))
        else
          if fst (snd ii_2944) = false then
            ((true, xs'_1014 (snd (fst ii_2944))), (false, (true, 0)))
          else
            let r_3370 = xs'__ys_3363 (snd (fst ii_2944)) (snd (snd ii_2944)) in
            ((true, fst r_3370), (true, snd r_3370))
      in
      let xs'_1966 i_2924 = snd (fst (xs'__ys_1965 ((true, i_2924), (false, 0)))) in
      let ys_1967 i_2917 = snd (snd (xs'__ys_1965 ((false, 0), (true, i_2917)))) in
      let r_append_1968 = append_1165 xs'__ys_1965 in
      let r_append_xs'__ys_0_1969 i_2906 = snd (#0 (r_append_1968 ((true, i_2906), (false, 0), (false, 0)))) in
      let r_append_xs'__ys_1_1970 i_2896 = snd (#1 (r_append_1968 ((false, 0), (true, i_2896), (false, 0)))) in
      let r_append_xs'__ys_2_1971 i_2886 = snd (#2 (r_append_1968 ((false, 0), (false, 0), (true, i_2886)))) in
      let rs'_1195 i_1369 =
        if i_1369 = 0 then
          (true, snd r_xs_1960)
        else
          let r_r_append_2885 = r_append_1968 ((true, i_1369 - 1), (false, 0), (false, 0)) in
          let r_r1_1976 = snd (#0 r_r_append_2885) in
          r_r1_1976
      in
      let rec rs'__xs_3491 x_3455 x_3456 =
        if x_3455 = 0 then
          let x1_3475 = true in
          let x2_3476 = snd r_xs_1960 in
          let r_3500 = (x1_3475, x2_3476) in
          let r_3501 =
            snd
            (fst
             (xs__ys_1023
               (let x1_3487 = let x1_3479 = true in
                              let x2_3480 = x_3456 in
                              (x1_3479, x2_3480) in
                let x2_3488 = let x1_3483 = false in
                              let x2_3484 = 0 in
                              (x1_3483, x2_3484) in
                (x1_3487, x2_3488))))
          in
          (r_3500, r_3501)
        else
          let r_r_append_2885 =
            r_append_1968
              (let x1_3469 = let x1_3457 = true in
                             let x2_3458 = x_3455 - 1 in
                             (x1_3457, x2_3458) in
               let x2_3470 = let x1_3461 = false in
                             let x2_3462 = 0 in
                             (x1_3461, x2_3462) in
               let x3_3471 = let x1_3465 = false in
                             let x2_3466 = 0 in
                             (x1_3465, x2_3466) in
               (x1_3469, x2_3470, x3_3471))
          in
          let r_r1_1976 = snd (#0 r_r_append_2885) in
          let r_3494 = r_r1_1976 in
          let r_3495 =
            snd
            (fst
             (xs__ys_1023
               (let x1_3487 = let x1_3479 = true in
                              let x2_3480 = x_3456 in
                              (x1_3479, x2_3480) in
                let x2_3488 = let x1_3483 = false in
                              let x2_3484 = 0 in
                              (x1_3483, x2_3484) in
                (x1_3487, x2_3488))))
          in
          (r_3494, r_3495)
      in
      let rec rs'__ys_3552 x_3516 x_3517 =
        if x_3516 = 0 then
          let x1_3536 = true in
          let x2_3537 = snd r_xs_1960 in
          let r_3561 = (x1_3536, x2_3537) in
          let r_3562 =
            snd
            (snd
             (xs__ys_1023
               (let x1_3548 = let x1_3540 = false in
                              let x2_3541 = 0 in
                              (x1_3540, x2_3541) in
                let x2_3549 = let x1_3544 = true in
                              let x2_3545 = x_3517 in
                              (x1_3544, x2_3545) in
                (x1_3548, x2_3549))))
          in
          (r_3561, r_3562)
        else
          let r_r_append_2885 =
            r_append_1968
              (let x1_3530 = let x1_3518 = true in
                             let x2_3519 = x_3516 - 1 in
                             (x1_3518, x2_3519) in
               let x2_3531 = let x1_3522 = false in
                             let x2_3523 = 0 in
                             (x1_3522, x2_3523) in
               let x3_3532 = let x1_3526 = false in
                             let x2_3527 = 0 in
                             (x1_3526, x2_3527) in
               (x1_3530, x2_3531, x3_3532))
          in
          let r_r1_1976 = snd (#0 r_r_append_2885) in
          let r_3555 = r_r1_1976 in
          let r_3556 =
            snd
            (snd
             (xs__ys_1023
               (let x1_3548 = let x1_3540 = false in
                              let x2_3541 = 0 in
                              (x1_3540, x2_3541) in
                let x2_3549 = let x1_3544 = true in
                              let x2_3545 = x_3517 in
                              (x1_3544, x2_3545) in
                (x1_3548, x2_3549))))
          in
          (r_3555, r_3556)
      in
      let rec rs'__xs__ys_3426 x_3377 x_3378 x_3379 =
        if x_3377 = 0 then
          let x1_3398 = true in
          let x2_3399 = snd r_xs_1960 in
          let r_3439 = (x1_3398, x2_3399) in
          let r_3440 =
            snd
            (fst
             (xs__ys_1023
               (let x1_3410 = let x1_3402 = true in
                              let x2_3403 = x_3378 in
                              (x1_3402, x2_3403) in
                let x2_3411 = let x1_3406 = false in
                              let x2_3407 = 0 in
                              (x1_3406, x2_3407) in
                (x1_3410, x2_3411))))
          in
          let r_3441 =
            snd
            (snd
             (xs__ys_1023
               (let x1_3422 = let x1_3414 = false in
                              let x2_3415 = 0 in
                              (x1_3414, x2_3415) in
                let x2_3423 = let x1_3418 = true in
                              let x2_3419 = x_3379 in
                              (x1_3418, x2_3419) in
                (x1_3422, x2_3423))))
          in
          (r_3439, r_3440, r_3441)
        else
          let r_r_append_2885 =
            r_append_1968
              (let x1_3392 = let x1_3380 = true in
                             let x2_3381 = x_3377 - 1 in
                             (x1_3380, x2_3381) in
               let x2_3393 = let x1_3384 = false in
                             let x2_3385 = 0 in
                             (x1_3384, x2_3385) in
               let x3_3394 = let x1_3388 = false in
                             let x2_3389 = 0 in
                             (x1_3388, x2_3389) in
               (x1_3392, x2_3393, x3_3394))
          in
          let r_r1_1976 = snd (#0 r_r_append_2885) in
          let r_3430 = r_r1_1976 in
          let r_3431 =
            snd
            (fst
             (xs__ys_1023
               (let x1_3410 = let x1_3402 = true in
                              let x2_3403 = x_3378 in
                              (x1_3402, x2_3403) in
                let x2_3411 = let x1_3406 = false in
                              let x2_3407 = 0 in
                              (x1_3406, x2_3407) in
                (x1_3410, x2_3411))))
          in
          let r_3432 =
            snd
            (snd
             (xs__ys_1023
               (let x1_3422 = let x1_3414 = false in
                              let x2_3415 = 0 in
                              (x1_3414, x2_3415) in
                let x2_3423 = let x1_3418 = true in
                              let x2_3419 = x_3379 in
                              (x1_3418, x2_3419) in
                (x1_3422, x2_3423))))
          in
          (r_3430, r_3431, r_3432)
      in
      let rs'__xs__ys_1986 iii_2831 =
        if fst (#0 iii_2831) = false then
          if fst (#1 iii_2831) = false then
            if fst (#2 iii_2831) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2831))))
          else
            if fst (#2 iii_2831) = false then
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2831))), (false, (true, 0)))
            else
              let r_3603 = xs__ys_3318 (snd (#1 iii_2831)) (snd (#2 iii_2831)) in
              ((false, (true, 0)), (true, fst r_3603), (true, snd r_3603))
        else
          if fst (#1 iii_2831) = false then
            if fst (#2 iii_2831) = false then
              ((true, rs'_1195 (snd (#0 iii_2831))), (false, (true, 0)), (false, (true, 0)))
            else
              let r_3565 = rs'__ys_3552 (snd (#0 iii_2831)) (snd (#2 iii_2831)) in
              ((true, fst r_3565), (false, (true, 0)), (true, snd r_3565))
          else
            if fst (#2 iii_2831) = false then
              let r_3504 = rs'__xs_3491 (snd (#0 iii_2831)) (snd (#1 iii_2831)) in
              ((true, fst r_3504), (true, snd r_3504), (false, (true, 0)))
            else
              let r_3445 = rs'__xs__ys_3426 (snd (#0 iii_2831)) (snd (#1 iii_2831)) (snd (#2 iii_2831)) in
              ((true, #0 r_3445), (true, #1 r_3445), (true, #2 r_3445))
      in
      rs'__xs__ys_1986
    else
      let bot_1820 = _|_ in
      let bot__xs__ys_1954 iii_2519 =
        if fst (#0 iii_2519) = false then
          if fst (#1 iii_2519) = false then
            if fst (#2 iii_2519) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2519))))
          else
            if fst (#2 iii_2519) = false then
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2519))), (false, (true, 0)))
            else
              let r_3325 = xs__ys_3318 (snd (#1 iii_2519)) (snd (#2 iii_2519)) in
              ((false, (true, 0)), (true, fst r_3325), (true, snd r_3325))
        else
          if fst (#1 iii_2519) = false then
            if fst (#2 iii_2519) = false then
              ((true, bot_1820 (snd (#0 iii_2519))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, bot_1820 (snd (#0 iii_2519))), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2519))))
          else
            if fst (#2 iii_2519) = false then
              ((true, bot_1820 (snd (#0 iii_2519))), (true, xs_1939 (snd (#1 iii_2519))), (false, (true, 0)))
            else
              ((true, bot_1820 (snd (#0 iii_2519))), (true, xs_1939 (snd (#1 iii_2519))), 
               (true, ys_1940 (snd (#2 iii_2519))))
      in
      bot__xs__ys_1954
in
let main_1017 i_1018 n_1019 =
  let r_make_list_1991 = make_list_1008 n_1019 in
  let f_1732 x_1560 = (false, 0) in
  let r_make_list__f_1999 ix_2285 =
    if fst (fst ix_2285) = false then
      if fst (snd ix_2285) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, f_1732 (snd (snd ix_2285))))
    else
      if fst (snd ix_2285) = false then
        ((true, r_make_list_1991 (snd (fst ix_2285))), (false, (true, 0)))
      else
        ((true, r_make_list_1991 (snd (fst ix_2285))), (true, f_1732 (snd (snd ix_2285))))
  in
  let xs_2000 i_2265 = snd (fst (r_make_list__f_1999 ((true, i_2265), (false, 0)))) in
  let f_2001 x_2258 = snd (snd (r_make_list__f_1999 ((false, 0), (true, x_2258)))) in
  let r_append_2002 = append_1165 r_make_list__f_1999 in
  let r_append_xs__f_0_2003 i_2247 = snd (#0 (r_append_2002 ((true, i_2247), (false, 0), (false, 0)))) in
  let r_append_xs__f_1_2004 i_2237 = snd (#1 (r_append_2002 ((false, 0), (true, i_2237), (false, 0)))) in
  let r_append_xs__f_2_2005 i_2227 = snd (#2 (r_append_2002 ((false, 0), (false, 0), (true, i_2227)))) in
  let r_r_append_2226 = r_append_2002 ((true, i_1018), (false, 0), (false, 0)) in
  let r_r_make_list_2009 = r_make_list_1991 i_1018 in
  let r_r1_2006 = snd (#0 r_r_append_2226) in
  if snd r_r1_2006 = snd r_r_make_list_2009 then
    ()
  else
    {fail} ()
in
let r_f_2015 = rand_int () in
let r_f_2017 = rand_int () in
let r_main_2018 = main_1017 r_f_2015 in
let r_r_main_2019 = r_main_2018 r_f_2017 in
()

normalize:
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1236 -> (false, 0)
  else
    let r_f_3805 = rand_int () in
    let r_make_list_3808 = make_list_1008 (n_1009 - 1) in
    fun i_1226 -> (if i_1226 = 0 then
                     (true, r_f_3805)
                   else
                     r_make_list_3808 (i_1226 - 1))
in
let rec append_1165 xs__ys_1023 =
  let xs_1939 i_3153 = let r_xs__ys_3841 = xs__ys_1023 ((true, i_3153), (false, 0)) in
                       snd (fst r_xs__ys_3841) in
  let ys_1940 i_3146 = let r_xs__ys_3860 = xs__ys_1023 ((false, 0), (true, i_3146)) in
                       snd (snd r_xs__ys_3860) in
  let rec ys__ys_3745 x_3719 x_3720 =
    let r_xs__ys_3874 = xs__ys_1023 ((false, 0), (true, x_3719)) in
    let r_xs__ys_3888 = xs__ys_1023 ((false, 0), (true, x_3720)) in
    (snd (snd r_xs__ys_3874), snd (snd r_xs__ys_3888))
  in
  let rec xs__ys_3318 x_3292 x_3293 =
    let r_xs__ys_3905 = xs__ys_1023 ((true, x_3292), (false, 0)) in
    let r_xs__ys_3919 = xs__ys_1023 ((false, 0), (true, x_3293)) in
    (snd (fst r_xs__ys_3905), snd (snd r_xs__ys_3919))
  in
  let rec ys__xs__ys_3654 x_3615 x_3616 x_3617 =
    let r_xs__ys_3936 = xs__ys_1023 ((false, 0), (true, x_3615)) in
    let r_xs__ys_3950 = xs__ys_1023 ((true, x_3616), (false, 0)) in
    let r_xs__ys_3964 = xs__ys_1023 ((false, 0), (true, x_3617)) in
    (snd (snd r_xs__ys_3936), snd (fst r_xs__ys_3950), snd (snd r_xs__ys_3964))
  in
  let rec ys__xs_3700 x_3674 x_3675 =
    let r_xs__ys_3982 = xs__ys_1023 ((false, 0), (true, x_3674)) in
    let r_xs__ys_3996 = xs__ys_1023 ((true, x_3675), (false, 0)) in
    (snd (snd r_xs__ys_3982), snd (fst r_xs__ys_3996))
  in
  let r_xs__ys_4017 = xs__ys_1023 ((true, 0), (false, 0)) in
  if fst (snd (fst r_xs__ys_4017)) = false then
    let ys__xs__ys_1990 iii_3100 =
      if fst (#0 iii_3100) = false then
        if fst (#1 iii_3100) = false then
          if fst (#2 iii_3100) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            let r_ys_5483 = ys_1940 (snd (#2 iii_3100)) in
            ((false, (true, 0)), (false, (true, 0)), (true, r_ys_5483))
        else
          if fst (#2 iii_3100) = false then
            let r_xs_5430 = xs_1939 (snd (#1 iii_3100)) in
            ((false, (true, 0)), (true, r_xs_5430), (false, (true, 0)))
          else
            let r_xs__ys_5383 = xs__ys_3318 (snd (#1 iii_3100)) (snd (#2 iii_3100)) in
            ((false, (true, 0)), (true, fst r_xs__ys_5383), (true, snd r_xs__ys_5383))
      else
        if fst (#1 iii_3100) = false then
          if fst (#2 iii_3100) = false then
            let r_ys_5335 = ys_1940 (snd (#0 iii_3100)) in
            ((true, r_ys_5335), (false, (true, 0)), (false, (true, 0)))
          else
            let r_ys__ys_5299 = ys__ys_3745 (snd (#0 iii_3100)) (snd (#2 iii_3100)) in
            ((true, fst r_ys__ys_5299), (false, (true, 0)), (true, snd r_ys__ys_5299))
        else
          if fst (#2 iii_3100) = false then
            let r_ys__xs_5257 = ys__xs_3700 (snd (#0 iii_3100)) (snd (#1 iii_3100)) in
            ((true, fst r_ys__xs_5257), (true, snd r_ys__xs_5257), (false, (true, 0)))
          else
            let r_ys__xs__ys_5225 = ys__xs__ys_3654 (snd (#0 iii_3100)) (snd (#1 iii_3100)) (snd (#2 iii_3100)) in
            ((true, #0 r_ys__xs__ys_5225), (true, #1 r_ys__xs__ys_5225), (true, #2 r_ys__xs__ys_5225))
    in
    ys__xs__ys_1990
  else
    if fst (snd (fst r_xs__ys_4017)) <> false then
      let xs'_1014 x_1269 = let r_xs__ys_4368 = xs__ys_1023 ((true, x_1269 + 1), (false, 0)) in
                            snd (fst r_xs__ys_4368) in
      let rec xs'__ys_3363 x_3337 x_3338 =
        let r_xs__ys_4383 = xs__ys_1023 ((true, x_3337 + 1), (false, 0)) in
        let r_xs__ys_4398 = xs__ys_1023 ((false, 0), (true, x_3338)) in
        (snd (fst r_xs__ys_4383), snd (snd r_xs__ys_4398))
      in
      let xs'__ys_1965 ii_2944 =
        if fst (fst ii_2944) = false then
          if fst (snd ii_2944) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let r_ys_4478 = ys_1940 (snd (snd ii_2944)) in
            ((false, (true, 0)), (true, r_ys_4478))
        else
          if fst (snd ii_2944) = false then
            let r_xs'_4437 = xs'_1014 (snd (fst ii_2944)) in
            ((true, r_xs'_4437), (false, (true, 0)))
          else
            let r_xs'__ys_4413 = xs'__ys_3363 (snd (fst ii_2944)) (snd (snd ii_2944)) in
            ((true, fst r_xs'__ys_4413), (true, snd r_xs'__ys_4413))
      in
      let xs'_1966 i_2924 = let r_xs'__ys_4538 = xs'__ys_1965 ((true, i_2924), (false, 0)) in
                            snd (fst r_xs'__ys_4538) in
      let ys_1967 i_2917 = let r_xs'__ys_4557 = xs'__ys_1965 ((false, 0), (true, i_2917)) in
                           snd (snd r_xs'__ys_4557) in
      let r_append_4560 = append_1165 xs'__ys_1965 in
      let r_append_xs'__ys_0_1969 i_2906 =
        let r_r_append_4584 = r_append_4560 ((true, i_2906), (false, 0), (false, 0)) in
        snd (#0 r_r_append_4584)
      in
      let r_append_xs'__ys_1_1970 i_2896 =
        let r_r_append_4610 = r_append_4560 ((false, 0), (true, i_2896), (false, 0)) in
        snd (#1 r_r_append_4610)
      in
      let r_append_xs'__ys_2_1971 i_2886 =
        let r_r_append_4636 = r_append_4560 ((false, 0), (false, 0), (true, i_2886)) in
        snd (#2 r_r_append_4636)
      in
      let rs'_1195 i_1369 =
        if i_1369 = 0 then
          (true, snd (snd (fst r_xs__ys_4017)))
        else
          let r_r_append_4663 = r_append_4560 ((true, i_1369 - 1), (false, 0), (false, 0)) in
          snd (#0 r_r_append_4663)
      in
      let rec rs'__xs_3491 x_3455 x_3456 =
        if x_3455 = 0 then
          let r_xs__ys_4726 = xs__ys_1023 ((true, x_3456), (false, 0)) in
          ((true, snd (snd (fst r_xs__ys_4017))), snd (fst r_xs__ys_4726))
        else
          let r_r_append_4690 = r_append_4560 ((true, x_3455 - 1), (false, 0), (false, 0)) in
          let r_xs__ys_4705 = xs__ys_1023 ((true, x_3456), (false, 0)) in
          (snd (#0 r_r_append_4690), snd (fst r_xs__ys_4705))
      in
      let rec rs'__ys_3552 x_3516 x_3517 =
        if x_3516 = 0 then
          let r_xs__ys_4785 = xs__ys_1023 ((false, 0), (true, x_3517)) in
          ((true, snd (snd (fst r_xs__ys_4017))), snd (snd r_xs__ys_4785))
        else
          let r_r_append_4749 = r_append_4560 ((true, x_3516 - 1), (false, 0), (false, 0)) in
          let r_xs__ys_4764 = xs__ys_1023 ((false, 0), (true, x_3517)) in
          (snd (#0 r_r_append_4749), snd (snd r_xs__ys_4764))
      in
      let rec rs'__xs__ys_3426 x_3377 x_3378 x_3379 =
        if x_3377 = 0 then
          let r_xs__ys_4859 = xs__ys_1023 ((true, x_3378), (false, 0)) in
          let r_xs__ys_4873 = xs__ys_1023 ((false, 0), (true, x_3379)) in
          ((true, snd (snd (fst r_xs__ys_4017))), snd (fst r_xs__ys_4859), snd (snd r_xs__ys_4873))
        else
          let r_r_append_4808 = r_append_4560 ((true, x_3377 - 1), (false, 0), (false, 0)) in
          let r_xs__ys_4823 = xs__ys_1023 ((true, x_3378), (false, 0)) in
          let r_xs__ys_4837 = xs__ys_1023 ((false, 0), (true, x_3379)) in
          (snd (#0 r_r_append_4808), snd (fst r_xs__ys_4823), snd (snd r_xs__ys_4837))
      in
      let rs'__xs__ys_1986 iii_2831 =
        if fst (#0 iii_2831) = false then
          if fst (#1 iii_2831) = false then
            if fst (#2 iii_2831) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              let r_ys_5149 = ys_1940 (snd (#2 iii_2831)) in
              ((false, (true, 0)), (false, (true, 0)), (true, r_ys_5149))
          else
            if fst (#2 iii_2831) = false then
              let r_xs_5096 = xs_1939 (snd (#1 iii_2831)) in
              ((false, (true, 0)), (true, r_xs_5096), (false, (true, 0)))
            else
              let r_xs__ys_5049 = xs__ys_3318 (snd (#1 iii_2831)) (snd (#2 iii_2831)) in
              ((false, (true, 0)), (true, fst r_xs__ys_5049), (true, snd r_xs__ys_5049))
        else
          if fst (#1 iii_2831) = false then
            if fst (#2 iii_2831) = false then
              let r_rs'_5001 = rs'_1195 (snd (#0 iii_2831)) in
              ((true, r_rs'_5001), (false, (true, 0)), (false, (true, 0)))
            else
              let r_rs'__ys_4965 = rs'__ys_3552 (snd (#0 iii_2831)) (snd (#2 iii_2831)) in
              ((true, fst r_rs'__ys_4965), (false, (true, 0)), (true, snd r_rs'__ys_4965))
          else
            if fst (#2 iii_2831) = false then
              let r_rs'__xs_4923 = rs'__xs_3491 (snd (#0 iii_2831)) (snd (#1 iii_2831)) in
              ((true, fst r_rs'__xs_4923), (true, snd r_rs'__xs_4923), (false, (true, 0)))
            else
              let r_rs'__xs__ys_4891 = rs'__xs__ys_3426 (snd (#0 iii_2831)) (snd (#1 iii_2831)) (snd (#2 iii_2831)) in
              ((true, #0 r_rs'__xs__ys_4891), (true, #1 r_rs'__xs__ys_4891), (true, #2 r_rs'__xs__ys_4891))
      in
      rs'__xs__ys_1986
    else
      let bot_1820 = _|_ in
      let bot__xs__ys_1954 iii_2519 =
        if fst (#0 iii_2519) = false then
          if fst (#1 iii_2519) = false then
            if fst (#2 iii_2519) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              let r_ys_4288 = ys_1940 (snd (#2 iii_2519)) in
              ((false, (true, 0)), (false, (true, 0)), (true, r_ys_4288))
          else
            if fst (#2 iii_2519) = false then
              let r_xs_4235 = xs_1939 (snd (#1 iii_2519)) in
              ((false, (true, 0)), (true, r_xs_4235), (false, (true, 0)))
            else
              let r_xs__ys_4188 = xs__ys_3318 (snd (#1 iii_2519)) (snd (#2 iii_2519)) in
              ((false, (true, 0)), (true, fst r_xs__ys_4188), (true, snd r_xs__ys_4188))
        else
          if fst (#1 iii_2519) = false then
            if fst (#2 iii_2519) = false then
              let r_bot_4140 = bot_1820 (snd (#0 iii_2519)) in
              ((true, r_bot_4140), (false, (true, 0)), (false, (true, 0)))
            else
              let r_bot_4105 = bot_1820 (snd (#0 iii_2519)) in
              let r_ys_4126 = ys_1940 (snd (#2 iii_2519)) in
              ((true, r_bot_4105), (false, (true, 0)), (true, r_ys_4126))
          else
            if fst (#2 iii_2519) = false then
              let r_bot_4064 = bot_1820 (snd (#0 iii_2519)) in
              let r_xs_4074 = xs_1939 (snd (#1 iii_2519)) in
              ((true, r_bot_4064), (true, r_xs_4074), (false, (true, 0)))
            else
              let r_bot_4030 = bot_1820 (snd (#0 iii_2519)) in
              let r_xs_4040 = xs_1939 (snd (#1 iii_2519)) in
              let r_ys_4050 = ys_1940 (snd (#2 iii_2519)) in
              ((true, r_bot_4030), (true, r_xs_4040), (true, r_ys_4050))
      in
      bot__xs__ys_1954
in
let main_1017 i_1018 n_1019 =
  let r_make_list_5550 = make_list_1008 n_1019 in
  let f_1732 x_1560 = (false, 0) in
  let r_make_list__f_1999 ix_2285 =
    if fst (fst ix_2285) = false then
      if fst (snd ix_2285) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        let r_f_5626 = f_1732 (snd (snd ix_2285)) in
        ((false, (true, 0)), (true, r_f_5626))
    else
      if fst (snd ix_2285) = false then
        let r_r_make_list_5585 = r_make_list_5550 (snd (fst ix_2285)) in
        ((true, r_r_make_list_5585), (false, (true, 0)))
      else
        let r_r_make_list_5562 = r_make_list_5550 (snd (fst ix_2285)) in
        let r_f_5572 = f_1732 (snd (snd ix_2285)) in
        ((true, r_r_make_list_5562), (true, r_f_5572))
  in
  let xs_2000 i_2265 =
    let r_r_make_list__f_5686 = r_make_list__f_1999 ((true, i_2265), (false, 0)) in
    snd (fst r_r_make_list__f_5686)
  in
  let f_2001 x_2258 =
    let r_r_make_list__f_5705 = r_make_list__f_1999 ((false, 0), (true, x_2258)) in
    snd (snd r_r_make_list__f_5705)
  in
  let r_append_5708 = append_1165 r_make_list__f_1999 in
  let r_append_xs__f_0_2003 i_2247 =
    let r_r_append_5732 = r_append_5708 ((true, i_2247), (false, 0), (false, 0)) in
    snd (#0 r_r_append_5732)
  in
  let r_append_xs__f_1_2004 i_2237 =
    let r_r_append_5758 = r_append_5708 ((false, 0), (true, i_2237), (false, 0)) in
    snd (#1 r_r_append_5758)
  in
  let r_append_xs__f_2_2005 i_2227 =
    let r_r_append_5784 = r_append_5708 ((false, 0), (false, 0), (true, i_2227)) in
    snd (#2 r_r_append_5784)
  in
  let r_r_append_5808 = r_append_5708 ((true, i_1018), (false, 0), (false, 0)) in
  let r_r_make_list_5809 = r_make_list_5550 i_1018 in
  if snd (snd (#0 r_r_append_5808)) = snd r_r_make_list_5809 then
    ()
  else
    {fail} ()
in
let r_f_5821 = rand_int () in
let r_f_5823 = rand_int () in
let r_main_5824 = main_1017 r_f_5821 in
let r_r_main_5825 = r_main_5824 r_f_5823 in
let r_r_main_2019 = r_r_main_5825 in
()

replace[1]: r_r_append_5808
APPS: r_r_append_5808 = r_append_5708 ...0... i_1018 ...
USED: r_r_append_5808 = r_append_5708 ...0... i_1018 ...
MUST: r_r_append_5808 = r_append_5708 ...0... i_1018 ...
NEW: r_r_append_5826 = r_append_5708 ((true, i_1018), (false, 0), (false, 0))
replace[1]: r_r_append_5784
APPS: r_r_append_5784 = r_append_5708 ...2... i_2227 ...
USED: r_r_append_5784 = r_append_5708 ...2... i_2227 ...
MUST: r_r_append_5784 = r_append_5708 ...2... i_2227 ...
NEW: r_r_append_5837 = r_append_5708 ((false, 0), (false, 0), (true, i_2227))
replace[1]: r_r_append_5758
APPS: r_r_append_5758 = r_append_5708 ...1... i_2237 ...
USED: r_r_append_5758 = r_append_5708 ...1... i_2237 ...
MUST: r_r_append_5758 = r_append_5708 ...1... i_2237 ...
NEW: r_r_append_5848 = r_append_5708 ((false, 0), (true, i_2237), (false, 0))
replace[1]: r_r_append_5732
APPS: r_r_append_5732 = r_append_5708 ...0... i_2247 ...
USED: r_r_append_5732 = r_append_5708 ...0... i_2247 ...
MUST: r_r_append_5732 = r_append_5708 ...0... i_2247 ...
NEW: r_r_append_5859 = r_append_5708 ((true, i_2247), (false, 0), (false, 0))
replace[1]: r_r_make_list__f_5705
APPS: r_r_make_list__f_5705 = r_make_list__f_1999 ...1... x_2258 ...
USED: r_r_make_list__f_5705 = r_make_list__f_1999 ...1... x_2258 ...
MUST: r_r_make_list__f_5705 = r_make_list__f_1999 ...1... x_2258 ...
NEW: r_r_make_list__f_5870 = r_make_list__f_1999 ((false, 0), (true, x_2258))
replace[1]: r_r_make_list__f_5686
APPS: r_r_make_list__f_5686 = r_make_list__f_1999 ...0... i_2265 ...
USED: r_r_make_list__f_5686 = r_make_list__f_1999 ...0... i_2265 ...
MUST: r_r_make_list__f_5686 = r_make_list__f_1999 ...0... i_2265 ...
NEW: r_r_make_list__f_5878 = r_make_list__f_1999 ((true, i_2265), (false, 0))
replace[2]: r_xs__ys_4823
APPS: r_xs__ys_4837 = xs__ys_1023 ...1... x_3379 ...
APPS: r_xs__ys_4823 = xs__ys_1023 ...0... x_3378 ...
APPS: r_xs__ys_4017 = xs__ys_1023 ...0... 0 ...
USED: r_xs__ys_4837 = xs__ys_1023 ...1... x_3379 ...
USED: r_xs__ys_4823 = xs__ys_1023 ...0... x_3378 ...
MUST: r_xs__ys_4823 = xs__ys_1023 ...0... x_3378 ...
MUST: r_xs__ys_4837 = xs__ys_1023 ...1... x_3379 ...
NEW: r_xs__ys_5886 = xs__ys_1023 ((true, x_3378), (true, x_3379))
replace[1]: r_r_append_4808
APPS: r_r_append_4808 = r_append_4560 ...0... x_3377 - 1 ...
USED: r_r_append_4808 = r_append_4560 ...0... x_3377 - 1 ...
MUST: r_r_append_4808 = r_append_4560 ...0... x_3377 - 1 ...
NEW: r_r_append_5895 = r_append_4560 ((true, x_3377 - 1), (false, 0), (false, 0))
replace[2]: r_xs__ys_4859
APPS: r_xs__ys_4873 = xs__ys_1023 ...1... x_3379 ...
APPS: r_xs__ys_4859 = xs__ys_1023 ...0... x_3378 ...
APPS: r_xs__ys_4017 = xs__ys_1023 ...0... 0 ...
USED: r_xs__ys_4873 = xs__ys_1023 ...1... x_3379 ...
USED: r_xs__ys_4859 = xs__ys_1023 ...0... x_3378 ...
USED: r_xs__ys_4017 = xs__ys_1023 ...0... 0 ...
MUST: r_xs__ys_4859 = xs__ys_1023 ...0... x_3378 ...
MUST: r_xs__ys_4873 = xs__ys_1023 ...1... x_3379 ...
replace[1]: r_xs__ys_4873
APPS: r_xs__ys_4873 = xs__ys_1023 ...1... x_3379 ...
APPS: r_xs__ys_4017 = xs__ys_1023 ...0... 0 ...
USED: r_xs__ys_4873 = xs__ys_1023 ...1... x_3379 ...
USED: r_xs__ys_4017 = xs__ys_1023 ...0... 0 ...
MUST: r_xs__ys_4873 = xs__ys_1023 ...1... x_3379 ...
NEW: r_xs__ys_5907 = xs__ys_1023 ((true, 0), (true, x_3379))
replace[1]: r_xs__ys_4764
APPS: r_xs__ys_4764 = xs__ys_1023 ...1... x_3517 ...
APPS: r_xs__ys_4017 = xs__ys_1023 ...0... 0 ...
USED: r_xs__ys_4764 = xs__ys_1023 ...1... x_3517 ...
MUST: r_xs__ys_4764 = xs__ys_1023 ...1... x_3517 ...
NEW: r_xs__ys_5916 = xs__ys_1023 ((false, 0), (true, x_3517))
replace[1]: r_r_append_4749
APPS: r_r_append_4749 = r_append_4560 ...0... x_3516 - 1 ...
USED: r_r_append_4749 = r_append_4560 ...0... x_3516 - 1 ...
MUST: r_r_append_4749 = r_append_4560 ...0... x_3516 - 1 ...
NEW: r_r_append_5924 = r_append_4560 ((true, x_3516 - 1), (false, 0), (false, 0))
replace[1]: r_xs__ys_4785
APPS: r_xs__ys_4785 = xs__ys_1023 ...1... x_3517 ...
APPS: r_xs__ys_4017 = xs__ys_1023 ...0... 0 ...
USED: r_xs__ys_4785 = xs__ys_1023 ...1... x_3517 ...
USED: r_xs__ys_4017 = xs__ys_1023 ...0... 0 ...
MUST: r_xs__ys_4785 = xs__ys_1023 ...1... x_3517 ...
NEW: r_xs__ys_5935 = xs__ys_1023 ((true, 0), (true, x_3517))
replace[1]: r_xs__ys_4705
APPS: r_xs__ys_4705 = xs__ys_1023 ...0... x_3456 ...
APPS: r_xs__ys_4017 = xs__ys_1023 ...0... 0 ...
USED: r_xs__ys_4705 = xs__ys_1023 ...0... x_3456 ...
MUST: r_xs__ys_4705 = xs__ys_1023 ...0... x_3456 ...
NEW: r_xs__ys_5944 = xs__ys_1023 ((true, x_3456), (false, 0))
replace[1]: r_r_append_4690
APPS: r_r_append_4690 = r_append_4560 ...0... x_3455 - 1 ...
USED: r_r_append_4690 = r_append_4560 ...0... x_3455 - 1 ...
MUST: r_r_append_4690 = r_append_4560 ...0... x_3455 - 1 ...
NEW: r_r_append_5952 = r_append_4560 ((true, x_3455 - 1), (false, 0), (false, 0))
replace[1]: r_xs__ys_4726
APPS: r_xs__ys_4726 = xs__ys_1023 ...0... x_3456 ...
APPS: r_xs__ys_4017 = xs__ys_1023 ...0... 0 ...
USED: r_xs__ys_4726 = xs__ys_1023 ...0... x_3456 ...
USED: r_xs__ys_4017 = xs__ys_1023 ...0... 0 ...
MUST: r_xs__ys_4726 = xs__ys_1023 ...0... x_3456 ...
replace[1]: r_r_append_4663
APPS: r_r_append_4663 = r_append_4560 ...0... i_1369 - 1 ...
USED: r_r_append_4663 = r_append_4560 ...0... i_1369 - 1 ...
MUST: r_r_append_4663 = r_append_4560 ...0... i_1369 - 1 ...
NEW: r_r_append_5964 = r_append_4560 ((true, i_1369 - 1), (false, 0), (false, 0))
replace[1]: r_r_append_4636
APPS: r_r_append_4636 = r_append_4560 ...2... i_2886 ...
USED: r_r_append_4636 = r_append_4560 ...2... i_2886 ...
MUST: r_r_append_4636 = r_append_4560 ...2... i_2886 ...
NEW: r_r_append_5975 = r_append_4560 ((false, 0), (false, 0), (true, i_2886))
replace[1]: r_r_append_4610
APPS: r_r_append_4610 = r_append_4560 ...1... i_2896 ...
USED: r_r_append_4610 = r_append_4560 ...1... i_2896 ...
MUST: r_r_append_4610 = r_append_4560 ...1... i_2896 ...
NEW: r_r_append_5986 = r_append_4560 ((false, 0), (true, i_2896), (false, 0))
replace[1]: r_r_append_4584
APPS: r_r_append_4584 = r_append_4560 ...0... i_2906 ...
USED: r_r_append_4584 = r_append_4560 ...0... i_2906 ...
MUST: r_r_append_4584 = r_append_4560 ...0... i_2906 ...
NEW: r_r_append_5997 = r_append_4560 ((true, i_2906), (false, 0), (false, 0))
replace[1]: r_xs'__ys_4557
APPS: r_xs'__ys_4557 = xs'__ys_1965 ...1... i_2917 ...
USED: r_xs'__ys_4557 = xs'__ys_1965 ...1... i_2917 ...
MUST: r_xs'__ys_4557 = xs'__ys_1965 ...1... i_2917 ...
NEW: r_xs'__ys_6008 = xs'__ys_1965 ((false, 0), (true, i_2917))
replace[1]: r_xs'__ys_4538
APPS: r_xs'__ys_4538 = xs'__ys_1965 ...0... i_2924 ...
USED: r_xs'__ys_4538 = xs'__ys_1965 ...0... i_2924 ...
MUST: r_xs'__ys_4538 = xs'__ys_1965 ...0... i_2924 ...
NEW: r_xs'__ys_6016 = xs'__ys_1965 ((true, i_2924), (false, 0))
replace[2]: r_xs__ys_4383
APPS: r_xs__ys_4398 = xs__ys_1023 ...1... x_3338 ...
APPS: r_xs__ys_4383 = xs__ys_1023 ...0... x_3337 + 1 ...
APPS: r_xs__ys_4017 = xs__ys_1023 ...0... 0 ...
USED: r_xs__ys_4398 = xs__ys_1023 ...1... x_3338 ...
USED: r_xs__ys_4383 = xs__ys_1023 ...0... x_3337 + 1 ...
MUST: r_xs__ys_4383 = xs__ys_1023 ...0... x_3337 + 1 ...
MUST: r_xs__ys_4398 = xs__ys_1023 ...1... x_3338 ...
NEW: r_xs__ys_6024 = xs__ys_1023 ((true, x_3337 + 1), (true, x_3338))
replace[1]: r_xs__ys_4368
APPS: r_xs__ys_4368 = xs__ys_1023 ...0... x_1269 + 1 ...
APPS: r_xs__ys_4017 = xs__ys_1023 ...0... 0 ...
USED: r_xs__ys_4368 = xs__ys_1023 ...0... x_1269 + 1 ...
MUST: r_xs__ys_4368 = xs__ys_1023 ...0... x_1269 + 1 ...
NEW: r_xs__ys_6033 = xs__ys_1023 ((true, x_1269 + 1), (false, 0))
replace[1]: r_xs__ys_4017
APPS: r_xs__ys_4017 = xs__ys_1023 ...0... 0 ...
USED: r_xs__ys_4017 = xs__ys_1023 ...0... 0 ...
MUST: r_xs__ys_4017 = xs__ys_1023 ...0... 0 ...
NEW: r_xs__ys_6041 = xs__ys_1023 ((true, 0), (false, 0))
replace[2]: r_xs__ys_3982
APPS: r_xs__ys_3996 = xs__ys_1023 ...0... x_3675 ...
APPS: r_xs__ys_3982 = xs__ys_1023 ...1... x_3674 ...
USED: r_xs__ys_3996 = xs__ys_1023 ...0... x_3675 ...
USED: r_xs__ys_3982 = xs__ys_1023 ...1... x_3674 ...
MUST: r_xs__ys_3982 = xs__ys_1023 ...1... x_3674 ...
MUST: r_xs__ys_3996 = xs__ys_1023 ...0... x_3675 ...
NEW: r_xs__ys_6049 = xs__ys_1023 ((true, x_3675), (true, x_3674))
replace[3]: r_xs__ys_3936
APPS: r_xs__ys_3964 = xs__ys_1023 ...1... x_3617 ...
APPS: r_xs__ys_3950 = xs__ys_1023 ...0... x_3616 ...
APPS: r_xs__ys_3936 = xs__ys_1023 ...1... x_3615 ...
USED: r_xs__ys_3964 = xs__ys_1023 ...1... x_3617 ...
USED: r_xs__ys_3950 = xs__ys_1023 ...0... x_3616 ...
USED: r_xs__ys_3936 = xs__ys_1023 ...1... x_3615 ...
MUST: r_xs__ys_3936 = xs__ys_1023 ...1... x_3615 ...
MUST: r_xs__ys_3950 = xs__ys_1023 ...0... x_3616 ...
MUST: r_xs__ys_3964 = xs__ys_1023 ...1... x_3617 ...
replace[2]: r_xs__ys_3950
APPS: r_xs__ys_3964 = xs__ys_1023 ...1... x_3617 ...
APPS: r_xs__ys_3950 = xs__ys_1023 ...0... x_3616 ...
USED: r_xs__ys_3964 = xs__ys_1023 ...1... x_3617 ...
USED: r_xs__ys_3950 = xs__ys_1023 ...0... x_3616 ...
MUST: r_xs__ys_3950 = xs__ys_1023 ...0... x_3616 ...
MUST: r_xs__ys_3964 = xs__ys_1023 ...1... x_3617 ...
NEW: r_xs__ys_6059 = xs__ys_1023 ((true, x_3616), (true, x_3617))
replace[2]: r_xs__ys_3905
APPS: r_xs__ys_3919 = xs__ys_1023 ...1... x_3293 ...
APPS: r_xs__ys_3905 = xs__ys_1023 ...0... x_3292 ...
USED: r_xs__ys_3919 = xs__ys_1023 ...1... x_3293 ...
USED: r_xs__ys_3905 = xs__ys_1023 ...0... x_3292 ...
MUST: r_xs__ys_3905 = xs__ys_1023 ...0... x_3292 ...
MUST: r_xs__ys_3919 = xs__ys_1023 ...1... x_3293 ...
NEW: r_xs__ys_6068 = xs__ys_1023 ((true, x_3292), (true, x_3293))
replace[2]: r_xs__ys_3874
APPS: r_xs__ys_3888 = xs__ys_1023 ...1... x_3720 ...
APPS: r_xs__ys_3874 = xs__ys_1023 ...1... x_3719 ...
USED: r_xs__ys_3888 = xs__ys_1023 ...1... x_3720 ...
USED: r_xs__ys_3874 = xs__ys_1023 ...1... x_3719 ...
MUST: r_xs__ys_3874 = xs__ys_1023 ...1... x_3719 ...
MUST: r_xs__ys_3888 = xs__ys_1023 ...1... x_3720 ...
replace[1]: r_xs__ys_3888
APPS: r_xs__ys_3888 = xs__ys_1023 ...1... x_3720 ...
USED: r_xs__ys_3888 = xs__ys_1023 ...1... x_3720 ...
MUST: r_xs__ys_3888 = xs__ys_1023 ...1... x_3720 ...
NEW: r_xs__ys_6078 = xs__ys_1023 ((false, 0), (true, x_3720))
replace[1]: r_xs__ys_3860
APPS: r_xs__ys_3860 = xs__ys_1023 ...1... i_3146 ...
USED: r_xs__ys_3860 = xs__ys_1023 ...1... i_3146 ...
MUST: r_xs__ys_3860 = xs__ys_1023 ...1... i_3146 ...
NEW: r_xs__ys_6086 = xs__ys_1023 ((false, 0), (true, i_3146))
replace[1]: r_xs__ys_3841
APPS: r_xs__ys_3841 = xs__ys_1023 ...0... i_3153 ...
USED: r_xs__ys_3841 = xs__ys_1023 ...0... i_3153 ...
MUST: r_xs__ys_3841 = xs__ys_1023 ...0... i_3153 ...
NEW: r_xs__ys_6094 = xs__ys_1023 ((true, i_3153), (false, 0))
replace_app:
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1236 -> (false, 0)
  else
    let r_f_3805 = rand_int () in
    let r_make_list_3808 = make_list_1008 (n_1009 - 1) in
    fun i_1226 -> (if i_1226 = 0 then
                     (true, r_f_3805)
                   else
                     r_make_list_3808 (i_1226 - 1))
in
let rec append_1165 xs__ys_1023 =
  let xs_1939 i_3153 =
    let r_xs__ys_3841 = xs__ys_1023 ((true, i_3153), (false, 0)) in
    let r_xs__ys_6094 = xs__ys_1023 ((true, i_3153), (false, 0)) in
    snd (fst r_xs__ys_6094)
  in
  let ys_1940 i_3146 =
    let r_xs__ys_3860 = xs__ys_1023 ((false, 0), (true, i_3146)) in
    let r_xs__ys_6086 = xs__ys_1023 ((false, 0), (true, i_3146)) in
    snd (snd r_xs__ys_6086)
  in
  let rec ys__ys_3745 x_3719 x_3720 =
    let r_xs__ys_3874 = xs__ys_1023 ((false, 0), (true, x_3719)) in
    let r_xs__ys_3888 = xs__ys_1023 ((false, 0), (true, x_3720)) in
    let r_xs__ys_6078 = xs__ys_1023 ((false, 0), (true, x_3720)) in
    (snd (snd r_xs__ys_3874), snd (snd r_xs__ys_6078))
  in
  let rec xs__ys_3318 x_3292 x_3293 =
    let r_xs__ys_3905 = xs__ys_1023 ((true, x_3292), (false, 0)) in
    let r_xs__ys_3919 = xs__ys_1023 ((false, 0), (true, x_3293)) in
    let r_xs__ys_6068 = xs__ys_1023 ((true, x_3292), (true, x_3293)) in
    (snd (fst r_xs__ys_6068), snd (snd r_xs__ys_6068))
  in
  let rec ys__xs__ys_3654 x_3615 x_3616 x_3617 =
    let r_xs__ys_3936 = xs__ys_1023 ((false, 0), (true, x_3615)) in
    let r_xs__ys_3950 = xs__ys_1023 ((true, x_3616), (false, 0)) in
    let r_xs__ys_3964 = xs__ys_1023 ((false, 0), (true, x_3617)) in
    let r_xs__ys_6059 = xs__ys_1023 ((true, x_3616), (true, x_3617)) in
    (snd (snd r_xs__ys_3936), snd (fst r_xs__ys_6059), snd (snd r_xs__ys_6059))
  in
  let rec ys__xs_3700 x_3674 x_3675 =
    let r_xs__ys_3982 = xs__ys_1023 ((false, 0), (true, x_3674)) in
    let r_xs__ys_3996 = xs__ys_1023 ((true, x_3675), (false, 0)) in
    let r_xs__ys_6049 = xs__ys_1023 ((true, x_3675), (true, x_3674)) in
    (snd (snd r_xs__ys_6049), snd (fst r_xs__ys_6049))
  in
  let r_xs__ys_4017 = xs__ys_1023 ((true, 0), (false, 0)) in
  let r_xs__ys_6041 = xs__ys_1023 ((true, 0), (false, 0)) in
  if fst (snd (fst r_xs__ys_6041)) = false then
    let ys__xs__ys_1990 iii_3100 =
      if fst (#0 iii_3100) = false then
        if fst (#1 iii_3100) = false then
          if fst (#2 iii_3100) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            let r_ys_5483 = ys_1940 (snd (#2 iii_3100)) in
            ((false, (true, 0)), (false, (true, 0)), (true, r_ys_5483))
        else
          if fst (#2 iii_3100) = false then
            let r_xs_5430 = xs_1939 (snd (#1 iii_3100)) in
            ((false, (true, 0)), (true, r_xs_5430), (false, (true, 0)))
          else
            let r_xs__ys_5383 = xs__ys_3318 (snd (#1 iii_3100)) (snd (#2 iii_3100)) in
            ((false, (true, 0)), (true, fst r_xs__ys_5383), (true, snd r_xs__ys_5383))
      else
        if fst (#1 iii_3100) = false then
          if fst (#2 iii_3100) = false then
            let r_ys_5335 = ys_1940 (snd (#0 iii_3100)) in
            ((true, r_ys_5335), (false, (true, 0)), (false, (true, 0)))
          else
            let r_ys__ys_5299 = ys__ys_3745 (snd (#0 iii_3100)) (snd (#2 iii_3100)) in
            ((true, fst r_ys__ys_5299), (false, (true, 0)), (true, snd r_ys__ys_5299))
        else
          if fst (#2 iii_3100) = false then
            let r_ys__xs_5257 = ys__xs_3700 (snd (#0 iii_3100)) (snd (#1 iii_3100)) in
            ((true, fst r_ys__xs_5257), (true, snd r_ys__xs_5257), (false, (true, 0)))
          else
            let r_ys__xs__ys_5225 = ys__xs__ys_3654 (snd (#0 iii_3100)) (snd (#1 iii_3100)) (snd (#2 iii_3100)) in
            ((true, #0 r_ys__xs__ys_5225), (true, #1 r_ys__xs__ys_5225), (true, #2 r_ys__xs__ys_5225))
    in
    ys__xs__ys_1990
  else
    if fst (snd (fst r_xs__ys_6041)) <> false then
      let xs'_1014 x_1269 =
        let r_xs__ys_4368 = xs__ys_1023 ((true, x_1269 + 1), (false, 0)) in
        let r_xs__ys_6033 = xs__ys_1023 ((true, x_1269 + 1), (false, 0)) in
        snd (fst r_xs__ys_6033)
      in
      let rec xs'__ys_3363 x_3337 x_3338 =
        let r_xs__ys_4383 = xs__ys_1023 ((true, x_3337 + 1), (false, 0)) in
        let r_xs__ys_4398 = xs__ys_1023 ((false, 0), (true, x_3338)) in
        let r_xs__ys_6024 = xs__ys_1023 ((true, x_3337 + 1), (true, x_3338)) in
        (snd (fst r_xs__ys_6024), snd (snd r_xs__ys_6024))
      in
      let xs'__ys_1965 ii_2944 =
        if fst (fst ii_2944) = false then
          if fst (snd ii_2944) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let r_ys_4478 = ys_1940 (snd (snd ii_2944)) in
            ((false, (true, 0)), (true, r_ys_4478))
        else
          if fst (snd ii_2944) = false then
            let r_xs'_4437 = xs'_1014 (snd (fst ii_2944)) in
            ((true, r_xs'_4437), (false, (true, 0)))
          else
            let r_xs'__ys_4413 = xs'__ys_3363 (snd (fst ii_2944)) (snd (snd ii_2944)) in
            ((true, fst r_xs'__ys_4413), (true, snd r_xs'__ys_4413))
      in
      let xs'_1966 i_2924 =
        let r_xs'__ys_4538 = xs'__ys_1965 ((true, i_2924), (false, 0)) in
        let r_xs'__ys_6016 = xs'__ys_1965 ((true, i_2924), (false, 0)) in
        snd (fst r_xs'__ys_6016)
      in
      let ys_1967 i_2917 =
        let r_xs'__ys_4557 = xs'__ys_1965 ((false, 0), (true, i_2917)) in
        let r_xs'__ys_6008 = xs'__ys_1965 ((false, 0), (true, i_2917)) in
        snd (snd r_xs'__ys_6008)
      in
      let r_append_4560 = append_1165 xs'__ys_1965 in
      let r_append_xs'__ys_0_1969 i_2906 =
        let r_r_append_4584 = r_append_4560 ((true, i_2906), (false, 0), (false, 0)) in
        let r_r_append_5997 = r_append_4560 ((true, i_2906), (false, 0), (false, 0)) in
        snd (#0 r_r_append_5997)
      in
      let r_append_xs'__ys_1_1970 i_2896 =
        let r_r_append_4610 = r_append_4560 ((false, 0), (true, i_2896), (false, 0)) in
        let r_r_append_5986 = r_append_4560 ((false, 0), (true, i_2896), (false, 0)) in
        snd (#1 r_r_append_5986)
      in
      let r_append_xs'__ys_2_1971 i_2886 =
        let r_r_append_4636 = r_append_4560 ((false, 0), (false, 0), (true, i_2886)) in
        let r_r_append_5975 = r_append_4560 ((false, 0), (false, 0), (true, i_2886)) in
        snd (#2 r_r_append_5975)
      in
      let rs'_1195 i_1369 =
        if i_1369 = 0 then
          (true, snd (snd (fst r_xs__ys_6041)))
        else
          let r_r_append_4663 = r_append_4560 ((true, i_1369 - 1), (false, 0), (false, 0)) in
          let r_r_append_5964 = r_append_4560 ((true, i_1369 - 1), (false, 0), (false, 0)) in
          snd (#0 r_r_append_5964)
      in
      let rec rs'__xs_3491 x_3455 x_3456 =
        if x_3455 = 0 then
          let r_xs__ys_4726 = xs__ys_1023 ((true, x_3456), (false, 0)) in
          ((true, snd (snd (fst r_xs__ys_6041))), snd (fst r_xs__ys_4726))
        else
          let r_r_append_4690 = r_append_4560 ((true, x_3455 - 1), (false, 0), (false, 0)) in
          let r_r_append_5952 = r_append_4560 ((true, x_3455 - 1), (false, 0), (false, 0)) in
          let r_xs__ys_4705 = xs__ys_1023 ((true, x_3456), (false, 0)) in
          let r_xs__ys_5944 = xs__ys_1023 ((true, x_3456), (false, 0)) in
          (snd (#0 r_r_append_5952), snd (fst r_xs__ys_5944))
      in
      let rec rs'__ys_3552 x_3516 x_3517 =
        if x_3516 = 0 then
          let r_xs__ys_4785 = xs__ys_1023 ((false, 0), (true, x_3517)) in
          let r_xs__ys_5935 = xs__ys_1023 ((true, 0), (true, x_3517)) in
          ((true, snd (snd (fst r_xs__ys_5935))), snd (snd r_xs__ys_5935))
        else
          let r_r_append_4749 = r_append_4560 ((true, x_3516 - 1), (false, 0), (false, 0)) in
          let r_r_append_5924 = r_append_4560 ((true, x_3516 - 1), (false, 0), (false, 0)) in
          let r_xs__ys_4764 = xs__ys_1023 ((false, 0), (true, x_3517)) in
          let r_xs__ys_5916 = xs__ys_1023 ((false, 0), (true, x_3517)) in
          (snd (#0 r_r_append_5924), snd (snd r_xs__ys_5916))
      in
      let rec rs'__xs__ys_3426 x_3377 x_3378 x_3379 =
        if x_3377 = 0 then
          let r_xs__ys_4859 = xs__ys_1023 ((true, x_3378), (false, 0)) in
          let r_xs__ys_4873 = xs__ys_1023 ((false, 0), (true, x_3379)) in
          let r_xs__ys_5907 = xs__ys_1023 ((true, 0), (true, x_3379)) in
          ((true, snd (snd (fst r_xs__ys_5907))), snd (fst r_xs__ys_4859), snd (snd r_xs__ys_5907))
        else
          let r_r_append_4808 = r_append_4560 ((true, x_3377 - 1), (false, 0), (false, 0)) in
          let r_r_append_5895 = r_append_4560 ((true, x_3377 - 1), (false, 0), (false, 0)) in
          let r_xs__ys_4823 = xs__ys_1023 ((true, x_3378), (false, 0)) in
          let r_xs__ys_4837 = xs__ys_1023 ((false, 0), (true, x_3379)) in
          let r_xs__ys_5886 = xs__ys_1023 ((true, x_3378), (true, x_3379)) in
          (snd (#0 r_r_append_5895), snd (fst r_xs__ys_5886), snd (snd r_xs__ys_5886))
      in
      let rs'__xs__ys_1986 iii_2831 =
        if fst (#0 iii_2831) = false then
          if fst (#1 iii_2831) = false then
            if fst (#2 iii_2831) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              let r_ys_5149 = ys_1940 (snd (#2 iii_2831)) in
              ((false, (true, 0)), (false, (true, 0)), (true, r_ys_5149))
          else
            if fst (#2 iii_2831) = false then
              let r_xs_5096 = xs_1939 (snd (#1 iii_2831)) in
              ((false, (true, 0)), (true, r_xs_5096), (false, (true, 0)))
            else
              let r_xs__ys_5049 = xs__ys_3318 (snd (#1 iii_2831)) (snd (#2 iii_2831)) in
              ((false, (true, 0)), (true, fst r_xs__ys_5049), (true, snd r_xs__ys_5049))
        else
          if fst (#1 iii_2831) = false then
            if fst (#2 iii_2831) = false then
              let r_rs'_5001 = rs'_1195 (snd (#0 iii_2831)) in
              ((true, r_rs'_5001), (false, (true, 0)), (false, (true, 0)))
            else
              let r_rs'__ys_4965 = rs'__ys_3552 (snd (#0 iii_2831)) (snd (#2 iii_2831)) in
              ((true, fst r_rs'__ys_4965), (false, (true, 0)), (true, snd r_rs'__ys_4965))
          else
            if fst (#2 iii_2831) = false then
              let r_rs'__xs_4923 = rs'__xs_3491 (snd (#0 iii_2831)) (snd (#1 iii_2831)) in
              ((true, fst r_rs'__xs_4923), (true, snd r_rs'__xs_4923), (false, (true, 0)))
            else
              let r_rs'__xs__ys_4891 = rs'__xs__ys_3426 (snd (#0 iii_2831)) (snd (#1 iii_2831)) (snd (#2 iii_2831)) in
              ((true, #0 r_rs'__xs__ys_4891), (true, #1 r_rs'__xs__ys_4891), (true, #2 r_rs'__xs__ys_4891))
      in
      rs'__xs__ys_1986
    else
      let bot_1820 = _|_ in
      let bot__xs__ys_1954 iii_2519 =
        if fst (#0 iii_2519) = false then
          if fst (#1 iii_2519) = false then
            if fst (#2 iii_2519) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              let r_ys_4288 = ys_1940 (snd (#2 iii_2519)) in
              ((false, (true, 0)), (false, (true, 0)), (true, r_ys_4288))
          else
            if fst (#2 iii_2519) = false then
              let r_xs_4235 = xs_1939 (snd (#1 iii_2519)) in
              ((false, (true, 0)), (true, r_xs_4235), (false, (true, 0)))
            else
              let r_xs__ys_4188 = xs__ys_3318 (snd (#1 iii_2519)) (snd (#2 iii_2519)) in
              ((false, (true, 0)), (true, fst r_xs__ys_4188), (true, snd r_xs__ys_4188))
        else
          if fst (#1 iii_2519) = false then
            if fst (#2 iii_2519) = false then
              let r_bot_4140 = bot_1820 (snd (#0 iii_2519)) in
              ((true, r_bot_4140), (false, (true, 0)), (false, (true, 0)))
            else
              let r_bot_4105 = bot_1820 (snd (#0 iii_2519)) in
              let r_ys_4126 = ys_1940 (snd (#2 iii_2519)) in
              ((true, r_bot_4105), (false, (true, 0)), (true, r_ys_4126))
          else
            if fst (#2 iii_2519) = false then
              let r_bot_4064 = bot_1820 (snd (#0 iii_2519)) in
              let r_xs_4074 = xs_1939 (snd (#1 iii_2519)) in
              ((true, r_bot_4064), (true, r_xs_4074), (false, (true, 0)))
            else
              let r_bot_4030 = bot_1820 (snd (#0 iii_2519)) in
              let r_xs_4040 = xs_1939 (snd (#1 iii_2519)) in
              let r_ys_4050 = ys_1940 (snd (#2 iii_2519)) in
              ((true, r_bot_4030), (true, r_xs_4040), (true, r_ys_4050))
      in
      bot__xs__ys_1954
in
let main_1017 i_1018 n_1019 =
  let r_make_list_5550 = make_list_1008 n_1019 in
  let f_1732 x_1560 = (false, 0) in
  let r_make_list__f_1999 ix_2285 =
    if fst (fst ix_2285) = false then
      if fst (snd ix_2285) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        let r_f_5626 = f_1732 (snd (snd ix_2285)) in
        ((false, (true, 0)), (true, r_f_5626))
    else
      if fst (snd ix_2285) = false then
        let r_r_make_list_5585 = r_make_list_5550 (snd (fst ix_2285)) in
        ((true, r_r_make_list_5585), (false, (true, 0)))
      else
        let r_r_make_list_5562 = r_make_list_5550 (snd (fst ix_2285)) in
        let r_f_5572 = f_1732 (snd (snd ix_2285)) in
        ((true, r_r_make_list_5562), (true, r_f_5572))
  in
  let xs_2000 i_2265 =
    let r_r_make_list__f_5686 = r_make_list__f_1999 ((true, i_2265), (false, 0)) in
    let r_r_make_list__f_5878 = r_make_list__f_1999 ((true, i_2265), (false, 0)) in
    snd (fst r_r_make_list__f_5878)
  in
  let f_2001 x_2258 =
    let r_r_make_list__f_5705 = r_make_list__f_1999 ((false, 0), (true, x_2258)) in
    let r_r_make_list__f_5870 = r_make_list__f_1999 ((false, 0), (true, x_2258)) in
    snd (snd r_r_make_list__f_5870)
  in
  let r_append_5708 = append_1165 r_make_list__f_1999 in
  let r_append_xs__f_0_2003 i_2247 =
    let r_r_append_5732 = r_append_5708 ((true, i_2247), (false, 0), (false, 0)) in
    let r_r_append_5859 = r_append_5708 ((true, i_2247), (false, 0), (false, 0)) in
    snd (#0 r_r_append_5859)
  in
  let r_append_xs__f_1_2004 i_2237 =
    let r_r_append_5758 = r_append_5708 ((false, 0), (true, i_2237), (false, 0)) in
    let r_r_append_5848 = r_append_5708 ((false, 0), (true, i_2237), (false, 0)) in
    snd (#1 r_r_append_5848)
  in
  let r_append_xs__f_2_2005 i_2227 =
    let r_r_append_5784 = r_append_5708 ((false, 0), (false, 0), (true, i_2227)) in
    let r_r_append_5837 = r_append_5708 ((false, 0), (false, 0), (true, i_2227)) in
    snd (#2 r_r_append_5837)
  in
  let r_r_append_5808 = r_append_5708 ((true, i_1018), (false, 0), (false, 0)) in
  let r_r_append_5826 = r_append_5708 ((true, i_1018), (false, 0), (false, 0)) in
  let r_r_make_list_5809 = r_make_list_5550 i_1018 in
  if snd (snd (#0 r_r_append_5826)) = snd r_r_make_list_5809 then
    ()
  else
    {fail} ()
in
let r_f_5821 = rand_int () in
let r_f_5823 = rand_int () in
let r_main_5824 = main_1017 r_f_5821 in
let r_r_main_5825 = r_main_5824 r_f_5823 in
let r_r_main_2019 = r_r_main_5825 in
()

is_subsumed: rand_int (), rand_int ()
is_subsumed: rand_int (), main_1017 r_f_5821
is_subsumed: rand_int (), r_main_5824 r_f_5823
is_subsumed: main_1017 r_f_5821, r_r_main_5825
is_subsumed: rand_int (), r_r_main_5825
is_subsumed: rand_int (), r_r_main_5825
is_subsumed: make_list_1008 n_1019, append_1165 r_make_list__f_1999
is_subsumed: make_list_1008 n_1019, r_append_5708 ((true, i_1018), (false, 0), (false, 0))
is_subsumed: r_append_5708 ((true, i_1018), (false, 0), (false, 0)), 
r_append_5708 ((true, i_1018), (false, 0), (false, 0))
is_subsumed: make_list_1008 n_1019, r_append_5708 ((true, i_1018), (false, 0), (false, 0))
r_r_append_5808 |-> r_r_append_5826
is_subsumed: r_append_5708 ((true, i_1018), (false, 0), (false, 0)), 
r_make_list_5550 i_1018
is_subsumed: r_append_5708 ((true, i_1018), (false, 0), (false, 0)), 
r_make_list_5550 i_1018
is_subsumed: append_1165 r_make_list__f_1999, r_make_list_5550 i_1018
is_subsumed: make_list_1008 n_1019, r_append_5708 ((false, 0), (false, 0), (true, i_2227))
is_subsumed: r_append_5708 ((false, 0), (false, 0), (true, i_2227)), 
r_append_5708 ((false, 0), (false, 0), (true, i_2227))
is_subsumed: make_list_1008 n_1019, r_append_5708 ((false, 0), (false, 0), (true, i_2227))
r_r_append_5784 |-> r_r_append_5837
is_subsumed: make_list_1008 n_1019, r_append_5708 ((false, 0), (true, i_2237), (false, 0))
is_subsumed: r_append_5708 ((false, 0), (true, i_2237), (false, 0)), 
r_append_5708 ((false, 0), (true, i_2237), (false, 0))
is_subsumed: make_list_1008 n_1019, r_append_5708 ((false, 0), (true, i_2237), (false, 0))
r_r_append_5758 |-> r_r_append_5848
is_subsumed: make_list_1008 n_1019, r_append_5708 ((true, i_2247), (false, 0), (false, 0))
is_subsumed: r_append_5708 ((true, i_2247), (false, 0), (false, 0)), 
r_append_5708 ((true, i_2247), (false, 0), (false, 0))
is_subsumed: make_list_1008 n_1019, r_append_5708 ((true, i_2247), (false, 0), (false, 0))
r_r_append_5732 |-> r_r_append_5859
is_subsumed: make_list_1008 n_1019, r_make_list__f_1999 ((false, 0), (true, x_2258))
is_subsumed: r_make_list__f_1999 ((false, 0), (true, x_2258)), r_make_list__f_1999 ((false, 0), (true, x_2258))
is_subsumed: make_list_1008 n_1019, r_make_list__f_1999 ((false, 0), (true, x_2258))
r_r_make_list__f_5705 |-> r_r_make_list__f_5870
is_subsumed: make_list_1008 n_1019, r_make_list__f_1999 ((true, i_2265), (false, 0))
is_subsumed: r_make_list__f_1999 ((true, i_2265), (false, 0)), r_make_list__f_1999 ((true, i_2265), (false, 0))
is_subsumed: make_list_1008 n_1019, r_make_list__f_1999 ((true, i_2265), (false, 0))
r_r_make_list__f_5686 |-> r_r_make_list__f_5878
is_subsumed: r_make_list_5550 (snd (fst ix_2285)), f_1732 (snd (snd ix_2285))
is_subsumed: make_list_1008 n_1019, f_1732 (snd (snd ix_2285))
is_subsumed: make_list_1008 n_1019, f_1732 (snd (snd ix_2285))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, 0), (false, 0))
r_xs__ys_4017 |-> r_xs__ys_6041
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), _|_
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), _|_
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), bot_1820 (snd (#0 iii_2519))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), bot_1820 (snd (#0 iii_2519))
is_subsumed: bot_1820 (snd (#0 iii_2519)), xs_1939 (snd (#1 iii_2519))
is_subsumed: _|_, xs_1939 (snd (#1 iii_2519))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs_1939 (snd (#1 iii_2519))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs_1939 (snd (#1 iii_2519))
is_subsumed: xs_1939 (snd (#1 iii_2519)), ys_1940 (snd (#2 iii_2519))
is_subsumed: bot_1820 (snd (#0 iii_2519)), ys_1940 (snd (#2 iii_2519))
is_subsumed: _|_, ys_1940 (snd (#2 iii_2519))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), ys_1940 (snd (#2 iii_2519))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), ys_1940 (snd (#2 iii_2519))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), bot_1820 (snd (#0 iii_2519))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), bot_1820 (snd (#0 iii_2519))
is_subsumed: bot_1820 (snd (#0 iii_2519)), xs_1939 (snd (#1 iii_2519))
is_subsumed: _|_, xs_1939 (snd (#1 iii_2519))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs_1939 (snd (#1 iii_2519))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs_1939 (snd (#1 iii_2519))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), bot_1820 (snd (#0 iii_2519))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), bot_1820 (snd (#0 iii_2519))
is_subsumed: bot_1820 (snd (#0 iii_2519)), ys_1940 (snd (#2 iii_2519))
is_subsumed: _|_, ys_1940 (snd (#2 iii_2519))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), ys_1940 (snd (#2 iii_2519))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), ys_1940 (snd (#2 iii_2519))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), bot_1820 (snd (#0 iii_2519))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), bot_1820 (snd (#0 iii_2519))
is_subsumed: _|_, xs__ys_3318 (snd (#1 iii_2519)) (snd (#2 iii_2519))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_3318 (snd (#1 iii_2519)) (snd (#2 iii_2519))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_3318 (snd (#1 iii_2519)) (snd (#2 iii_2519))
is_subsumed: _|_, xs_1939 (snd (#1 iii_2519))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs_1939 (snd (#1 iii_2519))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs_1939 (snd (#1 iii_2519))
is_subsumed: _|_, ys_1940 (snd (#2 iii_2519))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), ys_1940 (snd (#2 iii_2519))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), ys_1940 (snd (#2 iii_2519))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), append_1165 xs'__ys_1965
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), append_1165 xs'__ys_1965
is_subsumed: append_1165 xs'__ys_1965, rs'__xs__ys_3426 (snd (#0 iii_2831)) (snd (#1 iii_2831)) (snd (#2 iii_2831))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), rs'__xs__ys_3426 (snd (#0 iii_2831)) (
                                                    snd (#1 iii_2831)) (
                                                    snd (#2 iii_2831))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), rs'__xs__ys_3426 (snd (#0 iii_2831)) (
                                                    snd (#1 iii_2831)) (
                                                    snd (#2 iii_2831))
is_subsumed: append_1165 xs'__ys_1965, rs'__xs_3491 (snd (#0 iii_2831)) (snd (#1 iii_2831))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), rs'__xs_3491 (snd (#0 iii_2831)) (snd (#1 iii_2831))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), rs'__xs_3491 (snd (#0 iii_2831)) (snd (#1 iii_2831))
is_subsumed: append_1165 xs'__ys_1965, rs'__ys_3552 (snd (#0 iii_2831)) (snd (#2 iii_2831))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), rs'__ys_3552 (snd (#0 iii_2831)) (snd (#2 iii_2831))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), rs'__ys_3552 (snd (#0 iii_2831)) (snd (#2 iii_2831))
is_subsumed: append_1165 xs'__ys_1965, rs'_1195 (snd (#0 iii_2831))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), rs'_1195 (snd (#0 iii_2831))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), rs'_1195 (snd (#0 iii_2831))
is_subsumed: append_1165 xs'__ys_1965, xs__ys_3318 (snd (#1 iii_2831)) (snd (#2 iii_2831))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_3318 (snd (#1 iii_2831)) (snd (#2 iii_2831))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_3318 (snd (#1 iii_2831)) (snd (#2 iii_2831))
is_subsumed: append_1165 xs'__ys_1965, xs_1939 (snd (#1 iii_2831))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs_1939 (snd (#1 iii_2831))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs_1939 (snd (#1 iii_2831))
is_subsumed: append_1165 xs'__ys_1965, ys_1940 (snd (#2 iii_2831))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), ys_1940 (snd (#2 iii_2831))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), ys_1940 (snd (#2 iii_2831))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((true, x_3377 - 1), (false, 0), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((true, x_3377 - 1), (false, 0), (false, 0))
is_subsumed: r_append_4560 ((true, x_3377 - 1), (false, 0), (false, 0)), 
r_append_4560 ((true, x_3377 - 1), (false, 0), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((true, x_3377 - 1), (false, 0), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((true, x_3377 - 1), (false, 0), (false, 0))
r_r_append_4808 |-> r_r_append_5895
is_subsumed: r_append_4560 ((true, x_3377 - 1), (false, 0), (false, 0)), 
xs__ys_1023 ((true, x_3378), (false, 0))
is_subsumed: r_append_4560 ((true, x_3377 - 1), (false, 0), (false, 0)), 
xs__ys_1023 ((true, x_3378), (false, 0))
is_subsumed: append_1165 xs'__ys_1965, xs__ys_1023 ((true, x_3378), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, x_3378), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, x_3378), (false, 0))
is_subsumed: xs__ys_1023 ((true, x_3378), (false, 0)), xs__ys_1023 ((false, 0), (true, x_3379))
is_subsumed: r_append_4560 ((true, x_3377 - 1), (false, 0), (false, 0)), 
xs__ys_1023 ((false, 0), (true, x_3379))
is_subsumed: r_append_4560 ((true, x_3377 - 1), (false, 0), (false, 0)), 
xs__ys_1023 ((false, 0), (true, x_3379))
is_subsumed: append_1165 xs'__ys_1965, xs__ys_1023 ((false, 0), (true, x_3379))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((false, 0), (true, x_3379))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((false, 0), (true, x_3379))
is_subsumed: xs__ys_1023 ((false, 0), (true, x_3379)), xs__ys_1023 ((true, x_3378), (true, x_3379))
is_subsumed: xs__ys_1023 ((true, x_3378), (false, 0)), xs__ys_1023 ((true, x_3378), (true, x_3379))
is_subsumed: r_append_4560 ((true, x_3377 - 1), (false, 0), (false, 0)), 
xs__ys_1023 ((true, x_3378), (true, x_3379))
is_subsumed: r_append_4560 ((true, x_3377 - 1), (false, 0), (false, 0)), 
xs__ys_1023 ((true, x_3378), (true, x_3379))
is_subsumed: append_1165 xs'__ys_1965, xs__ys_1023 ((true, x_3378), (true, x_3379))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, x_3378), (true, x_3379))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, x_3378), (true, x_3379))
r_xs__ys_4837 |-> r_xs__ys_5886
r_xs__ys_4823 |-> r_xs__ys_5886
is_subsumed: append_1165 xs'__ys_1965, xs__ys_1023 ((true, x_3378), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, x_3378), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, x_3378), (false, 0))
is_subsumed: xs__ys_1023 ((true, x_3378), (false, 0)), xs__ys_1023 ((false, 0), (true, x_3379))
is_subsumed: append_1165 xs'__ys_1965, xs__ys_1023 ((false, 0), (true, x_3379))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((false, 0), (true, x_3379))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((false, 0), (true, x_3379))
is_subsumed: xs__ys_1023 ((false, 0), (true, x_3379)), xs__ys_1023 ((true, 0), (true, x_3379))
is_subsumed: xs__ys_1023 ((true, x_3378), (false, 0)), xs__ys_1023 ((true, 0), (true, x_3379))
is_subsumed: append_1165 xs'__ys_1965, xs__ys_1023 ((true, 0), (true, x_3379))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, 0), (true, x_3379))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, 0), (true, x_3379))
r_xs__ys_4873 |-> r_xs__ys_5907
r_xs__ys_6041 |-> r_xs__ys_5907
r_xs__ys_4017 |-> r_xs__ys_5907
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((true, x_3516 - 1), (false, 0), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((true, x_3516 - 1), (false, 0), (false, 0))
is_subsumed: r_append_4560 ((true, x_3516 - 1), (false, 0), (false, 0)), 
r_append_4560 ((true, x_3516 - 1), (false, 0), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((true, x_3516 - 1), (false, 0), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((true, x_3516 - 1), (false, 0), (false, 0))
r_r_append_4749 |-> r_r_append_5924
is_subsumed: r_append_4560 ((true, x_3516 - 1), (false, 0), (false, 0)), 
xs__ys_1023 ((false, 0), (true, x_3517))
is_subsumed: r_append_4560 ((true, x_3516 - 1), (false, 0), (false, 0)), 
xs__ys_1023 ((false, 0), (true, x_3517))
is_subsumed: append_1165 xs'__ys_1965, xs__ys_1023 ((false, 0), (true, x_3517))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((false, 0), (true, x_3517))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((false, 0), (true, x_3517))
is_subsumed: xs__ys_1023 ((false, 0), (true, x_3517)), xs__ys_1023 ((false, 0), (true, x_3517))
is_subsumed: r_append_4560 ((true, x_3516 - 1), (false, 0), (false, 0)), 
xs__ys_1023 ((false, 0), (true, x_3517))
is_subsumed: r_append_4560 ((true, x_3516 - 1), (false, 0), (false, 0)), 
xs__ys_1023 ((false, 0), (true, x_3517))
is_subsumed: append_1165 xs'__ys_1965, xs__ys_1023 ((false, 0), (true, x_3517))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((false, 0), (true, x_3517))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((false, 0), (true, x_3517))
r_xs__ys_4764 |-> r_xs__ys_5916
is_subsumed: append_1165 xs'__ys_1965, xs__ys_1023 ((false, 0), (true, x_3517))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((false, 0), (true, x_3517))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((false, 0), (true, x_3517))
is_subsumed: xs__ys_1023 ((false, 0), (true, x_3517)), xs__ys_1023 ((true, 0), (true, x_3517))
is_subsumed: append_1165 xs'__ys_1965, xs__ys_1023 ((true, 0), (true, x_3517))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, 0), (true, x_3517))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, 0), (true, x_3517))
r_xs__ys_4785 |-> r_xs__ys_5935
r_xs__ys_6041 |-> r_xs__ys_5935
r_xs__ys_4017 |-> r_xs__ys_5935
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((true, x_3455 - 1), (false, 0), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((true, x_3455 - 1), (false, 0), (false, 0))
is_subsumed: r_append_4560 ((true, x_3455 - 1), (false, 0), (false, 0)), 
r_append_4560 ((true, x_3455 - 1), (false, 0), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((true, x_3455 - 1), (false, 0), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((true, x_3455 - 1), (false, 0), (false, 0))
r_r_append_4690 |-> r_r_append_5952
is_subsumed: r_append_4560 ((true, x_3455 - 1), (false, 0), (false, 0)), 
xs__ys_1023 ((true, x_3456), (false, 0))
is_subsumed: r_append_4560 ((true, x_3455 - 1), (false, 0), (false, 0)), 
xs__ys_1023 ((true, x_3456), (false, 0))
is_subsumed: append_1165 xs'__ys_1965, xs__ys_1023 ((true, x_3456), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, x_3456), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, x_3456), (false, 0))
is_subsumed: xs__ys_1023 ((true, x_3456), (false, 0)), xs__ys_1023 ((true, x_3456), (false, 0))
is_subsumed: r_append_4560 ((true, x_3455 - 1), (false, 0), (false, 0)), 
xs__ys_1023 ((true, x_3456), (false, 0))
is_subsumed: r_append_4560 ((true, x_3455 - 1), (false, 0), (false, 0)), 
xs__ys_1023 ((true, x_3456), (false, 0))
is_subsumed: append_1165 xs'__ys_1965, xs__ys_1023 ((true, x_3456), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, x_3456), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, x_3456), (false, 0))
r_xs__ys_4705 |-> r_xs__ys_5944
is_subsumed: append_1165 xs'__ys_1965, xs__ys_1023 ((true, x_3456), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, x_3456), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, x_3456), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((true, i_1369 - 1), (false, 0), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((true, i_1369 - 1), (false, 0), (false, 0))
is_subsumed: r_append_4560 ((true, i_1369 - 1), (false, 0), (false, 0)), 
r_append_4560 ((true, i_1369 - 1), (false, 0), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((true, i_1369 - 1), (false, 0), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((true, i_1369 - 1), (false, 0), (false, 0))
r_r_append_4663 |-> r_r_append_5964
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((false, 0), (false, 0), (true, i_2886))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((false, 0), (false, 0), (true, i_2886))
is_subsumed: r_append_4560 ((false, 0), (false, 0), (true, i_2886)), 
r_append_4560 ((false, 0), (false, 0), (true, i_2886))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((false, 0), (false, 0), (true, i_2886))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((false, 0), (false, 0), (true, i_2886))
r_r_append_4636 |-> r_r_append_5975
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((false, 0), (true, i_2896), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((false, 0), (true, i_2896), (false, 0))
is_subsumed: r_append_4560 ((false, 0), (true, i_2896), (false, 0)), 
r_append_4560 ((false, 0), (true, i_2896), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((false, 0), (true, i_2896), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((false, 0), (true, i_2896), (false, 0))
r_r_append_4610 |-> r_r_append_5986
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((true, i_2906), (false, 0), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((true, i_2906), (false, 0), (false, 0))
is_subsumed: r_append_4560 ((true, i_2906), (false, 0), (false, 0)), 
r_append_4560 ((true, i_2906), (false, 0), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((true, i_2906), (false, 0), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), r_append_4560 ((true, i_2906), (false, 0), (false, 0))
r_r_append_4584 |-> r_r_append_5997
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs'__ys_1965 ((false, 0), (true, i_2917))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs'__ys_1965 ((false, 0), (true, i_2917))
is_subsumed: xs'__ys_1965 ((false, 0), (true, i_2917)), xs'__ys_1965 ((false, 0), (true, i_2917))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs'__ys_1965 ((false, 0), (true, i_2917))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs'__ys_1965 ((false, 0), (true, i_2917))
r_xs'__ys_4557 |-> r_xs'__ys_6008
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs'__ys_1965 ((true, i_2924), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs'__ys_1965 ((true, i_2924), (false, 0))
is_subsumed: xs'__ys_1965 ((true, i_2924), (false, 0)), xs'__ys_1965 ((true, i_2924), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs'__ys_1965 ((true, i_2924), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs'__ys_1965 ((true, i_2924), (false, 0))
r_xs'__ys_4538 |-> r_xs'__ys_6016
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs'__ys_3363 (snd (fst ii_2944)) (snd (snd ii_2944))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs'__ys_3363 (snd (fst ii_2944)) (snd (snd ii_2944))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs'_1014 (snd (fst ii_2944))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs'_1014 (snd (fst ii_2944))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), ys_1940 (snd (snd ii_2944))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), ys_1940 (snd (snd ii_2944))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, x_3337 + 1), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, x_3337 + 1), (false, 0))
is_subsumed: xs__ys_1023 ((true, x_3337 + 1), (false, 0)), xs__ys_1023 ((false, 0), (true, x_3338))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((false, 0), (true, x_3338))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((false, 0), (true, x_3338))
is_subsumed: xs__ys_1023 ((false, 0), (true, x_3338)), xs__ys_1023 ((true, x_3337 + 1), (true, x_3338))
is_subsumed: xs__ys_1023 ((true, x_3337 + 1), (false, 0)), xs__ys_1023 ((true, x_3337 + 1), (true, x_3338))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, x_3337 + 1), (true, x_3338))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, x_3337 + 1), (true, x_3338))
r_xs__ys_4398 |-> r_xs__ys_6024
r_xs__ys_4383 |-> r_xs__ys_6024
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, x_1269 + 1), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, x_1269 + 1), (false, 0))
is_subsumed: xs__ys_1023 ((true, x_1269 + 1), (false, 0)), xs__ys_1023 ((true, x_1269 + 1), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, x_1269 + 1), (false, 0))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_1023 ((true, x_1269 + 1), (false, 0))
r_xs__ys_4368 |-> r_xs__ys_6033
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), ys__xs__ys_3654 (snd (#0 iii_3100)) (
                                                    snd (#1 iii_3100)) (
                                                    snd (#2 iii_3100))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), ys__xs__ys_3654 (snd (#0 iii_3100)) (
                                                    snd (#1 iii_3100)) (
                                                    snd (#2 iii_3100))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), ys__xs_3700 (snd (#0 iii_3100)) (snd (#1 iii_3100))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), ys__xs_3700 (snd (#0 iii_3100)) (snd (#1 iii_3100))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), ys__ys_3745 (snd (#0 iii_3100)) (snd (#2 iii_3100))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), ys__ys_3745 (snd (#0 iii_3100)) (snd (#2 iii_3100))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), ys_1940 (snd (#0 iii_3100))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), ys_1940 (snd (#0 iii_3100))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_3318 (snd (#1 iii_3100)) (snd (#2 iii_3100))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs__ys_3318 (snd (#1 iii_3100)) (snd (#2 iii_3100))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs_1939 (snd (#1 iii_3100))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), xs_1939 (snd (#1 iii_3100))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), ys_1940 (snd (#2 iii_3100))
is_subsumed: xs__ys_1023 ((true, 0), (false, 0)), ys_1940 (snd (#2 iii_3100))
is_subsumed: xs__ys_1023 ((false, 0), (true, x_3674)), xs__ys_1023 ((true, x_3675), (false, 0))
is_subsumed: xs__ys_1023 ((true, x_3675), (false, 0)), xs__ys_1023 ((true, x_3675), (true, x_3674))
is_subsumed: xs__ys_1023 ((false, 0), (true, x_3674)), xs__ys_1023 ((true, x_3675), (true, x_3674))
r_xs__ys_3996 |-> r_xs__ys_6049
r_xs__ys_3982 |-> r_xs__ys_6049
is_subsumed: xs__ys_1023 ((false, 0), (true, x_3615)), xs__ys_1023 ((true, x_3616), (false, 0))
is_subsumed: xs__ys_1023 ((true, x_3616), (false, 0)), xs__ys_1023 ((false, 0), (true, x_3617))
is_subsumed: xs__ys_1023 ((false, 0), (true, x_3615)), xs__ys_1023 ((false, 0), (true, x_3617))
is_subsumed: xs__ys_1023 ((false, 0), (true, x_3617)), xs__ys_1023 ((true, x_3616), (true, x_3617))
is_subsumed: xs__ys_1023 ((true, x_3616), (false, 0)), xs__ys_1023 ((true, x_3616), (true, x_3617))
is_subsumed: xs__ys_1023 ((false, 0), (true, x_3615)), xs__ys_1023 ((true, x_3616), (true, x_3617))
r_xs__ys_3964 |-> r_xs__ys_6059
r_xs__ys_3950 |-> r_xs__ys_6059
is_subsumed: xs__ys_1023 ((true, x_3292), (false, 0)), xs__ys_1023 ((false, 0), (true, x_3293))
is_subsumed: xs__ys_1023 ((false, 0), (true, x_3293)), xs__ys_1023 ((true, x_3292), (true, x_3293))
is_subsumed: xs__ys_1023 ((true, x_3292), (false, 0)), xs__ys_1023 ((true, x_3292), (true, x_3293))
r_xs__ys_3919 |-> r_xs__ys_6068
r_xs__ys_3905 |-> r_xs__ys_6068
is_subsumed: xs__ys_1023 ((false, 0), (true, x_3719)), xs__ys_1023 ((false, 0), (true, x_3720))
is_subsumed: xs__ys_1023 ((false, 0), (true, x_3720)), xs__ys_1023 ((false, 0), (true, x_3720))
is_subsumed: xs__ys_1023 ((false, 0), (true, x_3719)), xs__ys_1023 ((false, 0), (true, x_3720))
r_xs__ys_3888 |-> r_xs__ys_6078
is_subsumed: xs__ys_1023 ((false, 0), (true, i_3146)), xs__ys_1023 ((false, 0), (true, i_3146))
r_xs__ys_3860 |-> r_xs__ys_6086
is_subsumed: xs__ys_1023 ((true, i_3153), (false, 0)), xs__ys_1023 ((true, i_3153), (false, 0))
r_xs__ys_3841 |-> r_xs__ys_6094
is_subsumed: rand_int (), make_list_1008 (n_1009 - 1)
r_xs__ys_3841; r_xs__ys_3860; r_xs__ys_3888; r_xs__ys_3919; r_xs__ys_3905; r_xs__ys_3950; r_xs__ys_3964; 
r_xs__ys_3996; r_xs__ys_3982; r_xs__ys_4017; r_xs__ys_4873; r_xs__ys_6041; r_xs__ys_4017; r_r_append_4808; 
r_xs__ys_4823; r_xs__ys_4837; r_xs__ys_4764; r_r_append_4749; r_xs__ys_4017; r_xs__ys_6041; r_xs__ys_4785; 
r_xs__ys_4705; r_r_append_4690; r_r_append_4663; r_r_append_4636; r_r_append_4610; r_r_append_4584; r_xs'__ys_4557; 
r_xs'__ys_4538; r_xs__ys_4383; r_xs__ys_4398; r_xs__ys_4368; r_r_append_5808; r_r_append_5784; r_r_append_5758; 
r_r_append_5732; r_r_make_list__f_5705; r_r_make_list__f_5686
elim_unnecessary:
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1236 -> (false, 0)
  else
    let r_f_3805 = rand_int () in
    let r_make_list_3808 = make_list_1008 (n_1009 - 1) in
    fun i_1226 -> (if i_1226 = 0 then
                     (true, r_f_3805)
                   else
                     r_make_list_3808 (i_1226 - 1))
in
let rec append_1165 xs__ys_1023 =
  let xs_1939 i_3153 = let r_xs__ys_6094 = xs__ys_1023 ((true, i_3153), (false, 0)) in
                       snd (fst r_xs__ys_6094) in
  let ys_1940 i_3146 = let r_xs__ys_6086 = xs__ys_1023 ((false, 0), (true, i_3146)) in
                       snd (snd r_xs__ys_6086) in
  let rec ys__ys_3745 x_3719 x_3720 =
    let r_xs__ys_3874 = xs__ys_1023 ((false, 0), (true, x_3719)) in
    let r_xs__ys_6078 = xs__ys_1023 ((false, 0), (true, x_3720)) in
    (snd (snd r_xs__ys_3874), snd (snd r_xs__ys_6078))
  in
  let rec xs__ys_3318 x_3292 x_3293 =
    let r_xs__ys_6068 = xs__ys_1023 ((true, x_3292), (true, x_3293)) in
    (snd (fst r_xs__ys_6068), snd (snd r_xs__ys_6068))
  in
  let rec ys__xs__ys_3654 x_3615 x_3616 x_3617 =
    let r_xs__ys_3936 = xs__ys_1023 ((false, 0), (true, x_3615)) in
    let r_xs__ys_6059 = xs__ys_1023 ((true, x_3616), (true, x_3617)) in
    (snd (snd r_xs__ys_3936), snd (fst r_xs__ys_6059), snd (snd r_xs__ys_6059))
  in
  let rec ys__xs_3700 x_3674 x_3675 =
    let r_xs__ys_6049 = xs__ys_1023 ((true, x_3675), (true, x_3674)) in
    (snd (snd r_xs__ys_6049), snd (fst r_xs__ys_6049))
  in
  let r_xs__ys_6041 = xs__ys_1023 ((true, 0), (false, 0)) in
  if fst (snd (fst r_xs__ys_6041)) = false then
    let ys__xs__ys_1990 iii_3100 =
      if fst (#0 iii_3100) = false then
        if fst (#1 iii_3100) = false then
          if fst (#2 iii_3100) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            let r_ys_5483 = ys_1940 (snd (#2 iii_3100)) in
            ((false, (true, 0)), (false, (true, 0)), (true, r_ys_5483))
        else
          if fst (#2 iii_3100) = false then
            let r_xs_5430 = xs_1939 (snd (#1 iii_3100)) in
            ((false, (true, 0)), (true, r_xs_5430), (false, (true, 0)))
          else
            let r_xs__ys_5383 = xs__ys_3318 (snd (#1 iii_3100)) (snd (#2 iii_3100)) in
            ((false, (true, 0)), (true, fst r_xs__ys_5383), (true, snd r_xs__ys_5383))
      else
        if fst (#1 iii_3100) = false then
          if fst (#2 iii_3100) = false then
            let r_ys_5335 = ys_1940 (snd (#0 iii_3100)) in
            ((true, r_ys_5335), (false, (true, 0)), (false, (true, 0)))
          else
            let r_ys__ys_5299 = ys__ys_3745 (snd (#0 iii_3100)) (snd (#2 iii_3100)) in
            ((true, fst r_ys__ys_5299), (false, (true, 0)), (true, snd r_ys__ys_5299))
        else
          if fst (#2 iii_3100) = false then
            let r_ys__xs_5257 = ys__xs_3700 (snd (#0 iii_3100)) (snd (#1 iii_3100)) in
            ((true, fst r_ys__xs_5257), (true, snd r_ys__xs_5257), (false, (true, 0)))
          else
            let r_ys__xs__ys_5225 = ys__xs__ys_3654 (snd (#0 iii_3100)) (snd (#1 iii_3100)) (snd (#2 iii_3100)) in
            ((true, #0 r_ys__xs__ys_5225), (true, #1 r_ys__xs__ys_5225), (true, #2 r_ys__xs__ys_5225))
    in
    ys__xs__ys_1990
  else
    if fst (snd (fst r_xs__ys_6041)) <> false then
      let xs'_1014 x_1269 = let r_xs__ys_6033 = xs__ys_1023 ((true, x_1269 + 1), (false, 0)) in
                            snd (fst r_xs__ys_6033) in
      let rec xs'__ys_3363 x_3337 x_3338 =
        let r_xs__ys_6024 = xs__ys_1023 ((true, x_3337 + 1), (true, x_3338)) in
        (snd (fst r_xs__ys_6024), snd (snd r_xs__ys_6024))
      in
      let xs'__ys_1965 ii_2944 =
        if fst (fst ii_2944) = false then
          if fst (snd ii_2944) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let r_ys_4478 = ys_1940 (snd (snd ii_2944)) in
            ((false, (true, 0)), (true, r_ys_4478))
        else
          if fst (snd ii_2944) = false then
            let r_xs'_4437 = xs'_1014 (snd (fst ii_2944)) in
            ((true, r_xs'_4437), (false, (true, 0)))
          else
            let r_xs'__ys_4413 = xs'__ys_3363 (snd (fst ii_2944)) (snd (snd ii_2944)) in
            ((true, fst r_xs'__ys_4413), (true, snd r_xs'__ys_4413))
      in
      let xs'_1966 i_2924 = let r_xs'__ys_6016 = xs'__ys_1965 ((true, i_2924), (false, 0)) in
                            snd (fst r_xs'__ys_6016) in
      let ys_1967 i_2917 = let r_xs'__ys_6008 = xs'__ys_1965 ((false, 0), (true, i_2917)) in
                           snd (snd r_xs'__ys_6008) in
      let r_append_4560 = append_1165 xs'__ys_1965 in
      let r_append_xs'__ys_0_1969 i_2906 =
        let r_r_append_5997 = r_append_4560 ((true, i_2906), (false, 0), (false, 0)) in
        snd (#0 r_r_append_5997)
      in
      let r_append_xs'__ys_1_1970 i_2896 =
        let r_r_append_5986 = r_append_4560 ((false, 0), (true, i_2896), (false, 0)) in
        snd (#1 r_r_append_5986)
      in
      let r_append_xs'__ys_2_1971 i_2886 =
        let r_r_append_5975 = r_append_4560 ((false, 0), (false, 0), (true, i_2886)) in
        snd (#2 r_r_append_5975)
      in
      let rs'_1195 i_1369 =
        if i_1369 = 0 then
          (true, snd (snd (fst r_xs__ys_6041)))
        else
          let r_r_append_5964 = r_append_4560 ((true, i_1369 - 1), (false, 0), (false, 0)) in
          snd (#0 r_r_append_5964)
      in
      let rec rs'__xs_3491 x_3455 x_3456 =
        if x_3455 = 0 then
          let r_xs__ys_4726 = xs__ys_1023 ((true, x_3456), (false, 0)) in
          ((true, snd (snd (fst r_xs__ys_6041))), snd (fst r_xs__ys_4726))
        else
          let r_r_append_5952 = r_append_4560 ((true, x_3455 - 1), (false, 0), (false, 0)) in
          let r_xs__ys_5944 = xs__ys_1023 ((true, x_3456), (false, 0)) in
          (snd (#0 r_r_append_5952), snd (fst r_xs__ys_5944))
      in
      let rec rs'__ys_3552 x_3516 x_3517 =
        if x_3516 = 0 then
          let r_xs__ys_5935 = xs__ys_1023 ((true, 0), (true, x_3517)) in
          ((true, snd (snd (fst r_xs__ys_5935))), snd (snd r_xs__ys_5935))
        else
          let r_r_append_5924 = r_append_4560 ((true, x_3516 - 1), (false, 0), (false, 0)) in
          let r_xs__ys_5916 = xs__ys_1023 ((false, 0), (true, x_3517)) in
          (snd (#0 r_r_append_5924), snd (snd r_xs__ys_5916))
      in
      let rec rs'__xs__ys_3426 x_3377 x_3378 x_3379 =
        if x_3377 = 0 then
          let r_xs__ys_4859 = xs__ys_1023 ((true, x_3378), (false, 0)) in
          let r_xs__ys_5907 = xs__ys_1023 ((true, 0), (true, x_3379)) in
          ((true, snd (snd (fst r_xs__ys_5907))), snd (fst r_xs__ys_4859), snd (snd r_xs__ys_5907))
        else
          let r_r_append_5895 = r_append_4560 ((true, x_3377 - 1), (false, 0), (false, 0)) in
          let r_xs__ys_5886 = xs__ys_1023 ((true, x_3378), (true, x_3379)) in
          (snd (#0 r_r_append_5895), snd (fst r_xs__ys_5886), snd (snd r_xs__ys_5886))
      in
      let rs'__xs__ys_1986 iii_2831 =
        if fst (#0 iii_2831) = false then
          if fst (#1 iii_2831) = false then
            if fst (#2 iii_2831) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              let r_ys_5149 = ys_1940 (snd (#2 iii_2831)) in
              ((false, (true, 0)), (false, (true, 0)), (true, r_ys_5149))
          else
            if fst (#2 iii_2831) = false then
              let r_xs_5096 = xs_1939 (snd (#1 iii_2831)) in
              ((false, (true, 0)), (true, r_xs_5096), (false, (true, 0)))
            else
              let r_xs__ys_5049 = xs__ys_3318 (snd (#1 iii_2831)) (snd (#2 iii_2831)) in
              ((false, (true, 0)), (true, fst r_xs__ys_5049), (true, snd r_xs__ys_5049))
        else
          if fst (#1 iii_2831) = false then
            if fst (#2 iii_2831) = false then
              let r_rs'_5001 = rs'_1195 (snd (#0 iii_2831)) in
              ((true, r_rs'_5001), (false, (true, 0)), (false, (true, 0)))
            else
              let r_rs'__ys_4965 = rs'__ys_3552 (snd (#0 iii_2831)) (snd (#2 iii_2831)) in
              ((true, fst r_rs'__ys_4965), (false, (true, 0)), (true, snd r_rs'__ys_4965))
          else
            if fst (#2 iii_2831) = false then
              let r_rs'__xs_4923 = rs'__xs_3491 (snd (#0 iii_2831)) (snd (#1 iii_2831)) in
              ((true, fst r_rs'__xs_4923), (true, snd r_rs'__xs_4923), (false, (true, 0)))
            else
              let r_rs'__xs__ys_4891 = rs'__xs__ys_3426 (snd (#0 iii_2831)) (snd (#1 iii_2831)) (snd (#2 iii_2831)) in
              ((true, #0 r_rs'__xs__ys_4891), (true, #1 r_rs'__xs__ys_4891), (true, #2 r_rs'__xs__ys_4891))
      in
      rs'__xs__ys_1986
    else
      let bot_1820 = _|_ in
      let bot__xs__ys_1954 iii_2519 =
        if fst (#0 iii_2519) = false then
          if fst (#1 iii_2519) = false then
            if fst (#2 iii_2519) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              let r_ys_4288 = ys_1940 (snd (#2 iii_2519)) in
              ((false, (true, 0)), (false, (true, 0)), (true, r_ys_4288))
          else
            if fst (#2 iii_2519) = false then
              let r_xs_4235 = xs_1939 (snd (#1 iii_2519)) in
              ((false, (true, 0)), (true, r_xs_4235), (false, (true, 0)))
            else
              let r_xs__ys_4188 = xs__ys_3318 (snd (#1 iii_2519)) (snd (#2 iii_2519)) in
              ((false, (true, 0)), (true, fst r_xs__ys_4188), (true, snd r_xs__ys_4188))
        else
          if fst (#1 iii_2519) = false then
            if fst (#2 iii_2519) = false then
              let r_bot_4140 = bot_1820 (snd (#0 iii_2519)) in
              ((true, r_bot_4140), (false, (true, 0)), (false, (true, 0)))
            else
              let r_bot_4105 = bot_1820 (snd (#0 iii_2519)) in
              let r_ys_4126 = ys_1940 (snd (#2 iii_2519)) in
              ((true, r_bot_4105), (false, (true, 0)), (true, r_ys_4126))
          else
            if fst (#2 iii_2519) = false then
              let r_bot_4064 = bot_1820 (snd (#0 iii_2519)) in
              let r_xs_4074 = xs_1939 (snd (#1 iii_2519)) in
              ((true, r_bot_4064), (true, r_xs_4074), (false, (true, 0)))
            else
              let r_bot_4030 = bot_1820 (snd (#0 iii_2519)) in
              let r_xs_4040 = xs_1939 (snd (#1 iii_2519)) in
              let r_ys_4050 = ys_1940 (snd (#2 iii_2519)) in
              ((true, r_bot_4030), (true, r_xs_4040), (true, r_ys_4050))
      in
      bot__xs__ys_1954
in
let main_1017 i_1018 n_1019 =
  let r_make_list_5550 = make_list_1008 n_1019 in
  let f_1732 x_1560 = (false, 0) in
  let r_make_list__f_1999 ix_2285 =
    if fst (fst ix_2285) = false then
      if fst (snd ix_2285) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        let r_f_5626 = f_1732 (snd (snd ix_2285)) in
        ((false, (true, 0)), (true, r_f_5626))
    else
      if fst (snd ix_2285) = false then
        let r_r_make_list_5585 = r_make_list_5550 (snd (fst ix_2285)) in
        ((true, r_r_make_list_5585), (false, (true, 0)))
      else
        let r_r_make_list_5562 = r_make_list_5550 (snd (fst ix_2285)) in
        let r_f_5572 = f_1732 (snd (snd ix_2285)) in
        ((true, r_r_make_list_5562), (true, r_f_5572))
  in
  let xs_2000 i_2265 =
    let r_r_make_list__f_5878 = r_make_list__f_1999 ((true, i_2265), (false, 0)) in
    snd (fst r_r_make_list__f_5878)
  in
  let f_2001 x_2258 =
    let r_r_make_list__f_5870 = r_make_list__f_1999 ((false, 0), (true, x_2258)) in
    snd (snd r_r_make_list__f_5870)
  in
  let r_append_5708 = append_1165 r_make_list__f_1999 in
  let r_append_xs__f_0_2003 i_2247 =
    let r_r_append_5859 = r_append_5708 ((true, i_2247), (false, 0), (false, 0)) in
    snd (#0 r_r_append_5859)
  in
  let r_append_xs__f_1_2004 i_2237 =
    let r_r_append_5848 = r_append_5708 ((false, 0), (true, i_2237), (false, 0)) in
    snd (#1 r_r_append_5848)
  in
  let r_append_xs__f_2_2005 i_2227 =
    let r_r_append_5837 = r_append_5708 ((false, 0), (false, 0), (true, i_2227)) in
    snd (#2 r_r_append_5837)
  in
  let r_r_append_5826 = r_append_5708 ((true, i_1018), (false, 0), (false, 0)) in
  let r_r_make_list_5809 = r_make_list_5550 i_1018 in
  if snd (snd (#0 r_r_append_5826)) = snd r_r_make_list_5809 then
    ()
  else
    {fail} ()
in
let r_f_5821 = rand_int () in
let r_f_5823 = rand_int () in
let r_main_5824 = main_1017 r_f_5821 in
let r_r_main_5825 = r_main_5824 r_f_5823 in
let r_r_main_2019 = r_r_main_5825 in
()

inline_next_redex:
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1236 -> (false, 0)
  else
    let r_f_3805 = rand_int () in
    let r_make_list_3808 = make_list_1008 (n_1009 - 1) in
    fun i_1226 -> (if i_1226 = 0 then
                     (true, r_f_3805)
                   else
                     r_make_list_3808 (i_1226 - 1))
in
let rec append_1165 xs__ys_1023 =
  let xs_1939 i_3153 = snd (fst (xs__ys_1023 ((true, i_3153), (false, 0)))) in
  let ys_1940 i_3146 = snd (snd (xs__ys_1023 ((false, 0), (true, i_3146)))) in
  let rec ys__ys_3745 x_3719 x_3720 =
    let r_xs__ys_3874 = xs__ys_1023 ((false, 0), (true, x_3719)) in
    (snd (snd r_xs__ys_3874), snd (snd (xs__ys_1023 ((false, 0), (true, x_3720)))))
  in
  let rec xs__ys_3318 x_3292 x_3293 =
    let r_xs__ys_6068 = xs__ys_1023 ((true, x_3292), (true, x_3293)) in
    (snd (fst r_xs__ys_6068), snd (snd r_xs__ys_6068))
  in
  let rec ys__xs__ys_3654 x_3615 x_3616 x_3617 =
    let r_xs__ys_3936 = xs__ys_1023 ((false, 0), (true, x_3615)) in
    let r_xs__ys_6059 = xs__ys_1023 ((true, x_3616), (true, x_3617)) in
    (snd (snd r_xs__ys_3936), snd (fst r_xs__ys_6059), snd (snd r_xs__ys_6059))
  in
  let rec ys__xs_3700 x_3674 x_3675 =
    let r_xs__ys_6049 = xs__ys_1023 ((true, x_3675), (true, x_3674)) in
    (snd (snd r_xs__ys_6049), snd (fst r_xs__ys_6049))
  in
  let r_xs__ys_6041 = xs__ys_1023 ((true, 0), (false, 0)) in
  if fst (snd (fst r_xs__ys_6041)) = false then
    let ys__xs__ys_1990 iii_3100 =
      if fst (#0 iii_3100) = false then
        if fst (#1 iii_3100) = false then
          if fst (#2 iii_3100) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_3100))))
        else
          if fst (#2 iii_3100) = false then
            ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_3100))), (false, (true, 0)))
          else
            let r_xs__ys_5383 = xs__ys_3318 (snd (#1 iii_3100)) (snd (#2 iii_3100)) in
            ((false, (true, 0)), (true, fst r_xs__ys_5383), (true, snd r_xs__ys_5383))
      else
        if fst (#1 iii_3100) = false then
          if fst (#2 iii_3100) = false then
            ((true, ys_1940 (snd (#0 iii_3100))), (false, (true, 0)), (false, (true, 0)))
          else
            let r_ys__ys_5299 = ys__ys_3745 (snd (#0 iii_3100)) (snd (#2 iii_3100)) in
            ((true, fst r_ys__ys_5299), (false, (true, 0)), (true, snd r_ys__ys_5299))
        else
          if fst (#2 iii_3100) = false then
            let r_ys__xs_5257 = ys__xs_3700 (snd (#0 iii_3100)) (snd (#1 iii_3100)) in
            ((true, fst r_ys__xs_5257), (true, snd r_ys__xs_5257), (false, (true, 0)))
          else
            let r_ys__xs__ys_5225 = ys__xs__ys_3654 (snd (#0 iii_3100)) (snd (#1 iii_3100)) (snd (#2 iii_3100)) in
            ((true, #0 r_ys__xs__ys_5225), (true, #1 r_ys__xs__ys_5225), (true, #2 r_ys__xs__ys_5225))
    in
    ys__xs__ys_1990
  else
    if fst (snd (fst r_xs__ys_6041)) <> false then
      let xs'_1014 x_1269 = snd (fst (xs__ys_1023 ((true, x_1269 + 1), (false, 0)))) in
      let rec xs'__ys_3363 x_3337 x_3338 =
        let r_xs__ys_6024 = xs__ys_1023 ((true, x_3337 + 1), (true, x_3338)) in
        (snd (fst r_xs__ys_6024), snd (snd r_xs__ys_6024))
      in
      let xs'__ys_1965 ii_2944 =
        if fst (fst ii_2944) = false then
          if fst (snd ii_2944) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, ys_1940 (snd (snd ii_2944))))
        else
          if fst (snd ii_2944) = false then
            ((true, xs'_1014 (snd (fst ii_2944))), (false, (true, 0)))
          else
            let r_xs'__ys_4413 = xs'__ys_3363 (snd (fst ii_2944)) (snd (snd ii_2944)) in
            ((true, fst r_xs'__ys_4413), (true, snd r_xs'__ys_4413))
      in
      let xs'_1966 i_2924 = snd (fst (xs'__ys_1965 ((true, i_2924), (false, 0)))) in
      let ys_1967 i_2917 = snd (snd (xs'__ys_1965 ((false, 0), (true, i_2917)))) in
      let r_append_4560 = append_1165 xs'__ys_1965 in
      let r_append_xs'__ys_0_1969 i_2906 = snd (#0 (r_append_4560 ((true, i_2906), (false, 0), (false, 0)))) in
      let r_append_xs'__ys_1_1970 i_2896 = snd (#1 (r_append_4560 ((false, 0), (true, i_2896), (false, 0)))) in
      let r_append_xs'__ys_2_1971 i_2886 = snd (#2 (r_append_4560 ((false, 0), (false, 0), (true, i_2886)))) in
      let rs'_1195 i_1369 =
        if i_1369 = 0 then
          (true, snd (snd (fst r_xs__ys_6041)))
        else
          snd (#0 (r_append_4560 ((true, i_1369 - 1), (false, 0), (false, 0))))
      in
      let rec rs'__xs_3491 x_3455 x_3456 =
        if x_3455 = 0 then
          ((true, snd (snd (fst r_xs__ys_6041))), snd (fst (xs__ys_1023 ((true, x_3456), (false, 0)))))
        else
          let r_r_append_5952 = r_append_4560 ((true, x_3455 - 1), (false, 0), (false, 0)) in
          (snd (#0 r_r_append_5952), snd (fst (xs__ys_1023 ((true, x_3456), (false, 0)))))
      in
      let rec rs'__ys_3552 x_3516 x_3517 =
        if x_3516 = 0 then
          let r_xs__ys_5935 = xs__ys_1023 ((true, 0), (true, x_3517)) in
          ((true, snd (snd (fst r_xs__ys_5935))), snd (snd r_xs__ys_5935))
        else
          let r_r_append_5924 = r_append_4560 ((true, x_3516 - 1), (false, 0), (false, 0)) in
          (snd (#0 r_r_append_5924), snd (snd (xs__ys_1023 ((false, 0), (true, x_3517)))))
      in
      let rec rs'__xs__ys_3426 x_3377 x_3378 x_3379 =
        if x_3377 = 0 then
          let r_xs__ys_4859 = xs__ys_1023 ((true, x_3378), (false, 0)) in
          let r_xs__ys_5907 = xs__ys_1023 ((true, 0), (true, x_3379)) in
          ((true, snd (snd (fst r_xs__ys_5907))), snd (fst r_xs__ys_4859), snd (snd r_xs__ys_5907))
        else
          let r_r_append_5895 = r_append_4560 ((true, x_3377 - 1), (false, 0), (false, 0)) in
          let r_xs__ys_5886 = xs__ys_1023 ((true, x_3378), (true, x_3379)) in
          (snd (#0 r_r_append_5895), snd (fst r_xs__ys_5886), snd (snd r_xs__ys_5886))
      in
      let rs'__xs__ys_1986 iii_2831 =
        if fst (#0 iii_2831) = false then
          if fst (#1 iii_2831) = false then
            if fst (#2 iii_2831) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2831))))
          else
            if fst (#2 iii_2831) = false then
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2831))), (false, (true, 0)))
            else
              let r_xs__ys_5049 = xs__ys_3318 (snd (#1 iii_2831)) (snd (#2 iii_2831)) in
              ((false, (true, 0)), (true, fst r_xs__ys_5049), (true, snd r_xs__ys_5049))
        else
          if fst (#1 iii_2831) = false then
            if fst (#2 iii_2831) = false then
              ((true, rs'_1195 (snd (#0 iii_2831))), (false, (true, 0)), (false, (true, 0)))
            else
              let r_rs'__ys_4965 = rs'__ys_3552 (snd (#0 iii_2831)) (snd (#2 iii_2831)) in
              ((true, fst r_rs'__ys_4965), (false, (true, 0)), (true, snd r_rs'__ys_4965))
          else
            if fst (#2 iii_2831) = false then
              let r_rs'__xs_4923 = rs'__xs_3491 (snd (#0 iii_2831)) (snd (#1 iii_2831)) in
              ((true, fst r_rs'__xs_4923), (true, snd r_rs'__xs_4923), (false, (true, 0)))
            else
              let r_rs'__xs__ys_4891 = rs'__xs__ys_3426 (snd (#0 iii_2831)) (snd (#1 iii_2831)) (snd (#2 iii_2831)) in
              ((true, #0 r_rs'__xs__ys_4891), (true, #1 r_rs'__xs__ys_4891), (true, #2 r_rs'__xs__ys_4891))
      in
      rs'__xs__ys_1986
    else
      let bot_1820 = _|_ in
      let bot__xs__ys_1954 iii_2519 =
        if fst (#0 iii_2519) = false then
          if fst (#1 iii_2519) = false then
            if fst (#2 iii_2519) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2519))))
          else
            if fst (#2 iii_2519) = false then
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2519))), (false, (true, 0)))
            else
              let r_xs__ys_4188 = xs__ys_3318 (snd (#1 iii_2519)) (snd (#2 iii_2519)) in
              ((false, (true, 0)), (true, fst r_xs__ys_4188), (true, snd r_xs__ys_4188))
        else
          if fst (#1 iii_2519) = false then
            if fst (#2 iii_2519) = false then
              ((true, bot_1820 (snd (#0 iii_2519))), (false, (true, 0)), (false, (true, 0)))
            else
              let r_bot_4105 = bot_1820 (snd (#0 iii_2519)) in
              ((true, r_bot_4105), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2519))))
          else
            if fst (#2 iii_2519) = false then
              let r_bot_4064 = bot_1820 (snd (#0 iii_2519)) in
              ((true, r_bot_4064), (true, xs_1939 (snd (#1 iii_2519))), (false, (true, 0)))
            else
              let r_bot_4030 = bot_1820 (snd (#0 iii_2519)) in
              let r_xs_4040 = xs_1939 (snd (#1 iii_2519)) in
              ((true, r_bot_4030), (true, r_xs_4040), (true, ys_1940 (snd (#2 iii_2519))))
      in
      bot__xs__ys_1954
in
let main_1017 i_1018 n_1019 =
  let r_make_list_5550 = make_list_1008 n_1019 in
  let f_1732 x_1560 = (false, 0) in
  let r_make_list__f_1999 ix_2285 =
    if fst (fst ix_2285) = false then
      if fst (snd ix_2285) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, f_1732 (snd (snd ix_2285))))
    else
      if fst (snd ix_2285) = false then
        ((true, r_make_list_5550 (snd (fst ix_2285))), (false, (true, 0)))
      else
        let r_r_make_list_5562 = r_make_list_5550 (snd (fst ix_2285)) in
        ((true, r_r_make_list_5562), (true, f_1732 (snd (snd ix_2285))))
  in
  let xs_2000 i_2265 = snd (fst (r_make_list__f_1999 ((true, i_2265), (false, 0)))) in
  let f_2001 x_2258 = snd (snd (r_make_list__f_1999 ((false, 0), (true, x_2258)))) in
  let r_append_5708 = append_1165 r_make_list__f_1999 in
  let r_append_xs__f_0_2003 i_2247 = snd (#0 (r_append_5708 ((true, i_2247), (false, 0), (false, 0)))) in
  let r_append_xs__f_1_2004 i_2237 = snd (#1 (r_append_5708 ((false, 0), (true, i_2237), (false, 0)))) in
  let r_append_xs__f_2_2005 i_2227 = snd (#2 (r_append_5708 ((false, 0), (false, 0), (true, i_2227)))) in
  let r_r_append_5826 = r_append_5708 ((true, i_1018), (false, 0), (false, 0)) in
  let r_r_make_list_5809 = r_make_list_5550 i_1018 in
  if snd (snd (#0 r_r_append_5826)) = snd r_r_make_list_5809 then
    ()
  else
    {fail} ()
in
let r_f_5821 = rand_int () in
let r_f_5823 = rand_int () in
let r_main_5824 = main_1017 r_f_5821 in
let r_r_main_5825 = r_main_5824 r_f_5823 in
let r_r_main_2019 = r_r_main_5825 in
()

reduce_bottom:
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1236 -> (false, 0)
  else
    let r_f_3805 = rand_int () in
    let r_make_list_3808 = make_list_1008 (n_1009 - 1) in
    fun i_1226 -> (if i_1226 = 0 then
                     (true, r_f_3805)
                   else
                     r_make_list_3808 (i_1226 - 1))
in
let rec append_1165 xs__ys_1023 =
  let xs_1939 i_3153 = snd (fst (xs__ys_1023 ((true, i_3153), (false, 0)))) in
  let ys_1940 i_3146 = snd (snd (xs__ys_1023 ((false, 0), (true, i_3146)))) in
  let rec ys__ys_3745 x_3719 x_3720 =
    let r_xs__ys_3874 = xs__ys_1023 ((false, 0), (true, x_3719)) in
    (snd (snd r_xs__ys_3874), snd (snd (xs__ys_1023 ((false, 0), (true, x_3720)))))
  in
  let rec xs__ys_3318 x_3292 x_3293 =
    let r_xs__ys_6068 = xs__ys_1023 ((true, x_3292), (true, x_3293)) in
    (snd (fst r_xs__ys_6068), snd (snd r_xs__ys_6068))
  in
  let rec ys__xs__ys_3654 x_3615 x_3616 x_3617 =
    let r_xs__ys_3936 = xs__ys_1023 ((false, 0), (true, x_3615)) in
    let r_xs__ys_6059 = xs__ys_1023 ((true, x_3616), (true, x_3617)) in
    (snd (snd r_xs__ys_3936), snd (fst r_xs__ys_6059), snd (snd r_xs__ys_6059))
  in
  let rec ys__xs_3700 x_3674 x_3675 =
    let r_xs__ys_6049 = xs__ys_1023 ((true, x_3675), (true, x_3674)) in
    (snd (snd r_xs__ys_6049), snd (fst r_xs__ys_6049))
  in
  let r_xs__ys_6041 = xs__ys_1023 ((true, 0), (false, 0)) in
  if fst (snd (fst r_xs__ys_6041)) = false then
    let ys__xs__ys_1990 iii_3100 =
      if fst (#0 iii_3100) = false then
        if fst (#1 iii_3100) = false then
          if fst (#2 iii_3100) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_3100))))
        else
          if fst (#2 iii_3100) = false then
            ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_3100))), (false, (true, 0)))
          else
            let r_xs__ys_5383 = xs__ys_3318 (snd (#1 iii_3100)) (snd (#2 iii_3100)) in
            ((false, (true, 0)), (true, fst r_xs__ys_5383), (true, snd r_xs__ys_5383))
      else
        if fst (#1 iii_3100) = false then
          if fst (#2 iii_3100) = false then
            ((true, ys_1940 (snd (#0 iii_3100))), (false, (true, 0)), (false, (true, 0)))
          else
            let r_ys__ys_5299 = ys__ys_3745 (snd (#0 iii_3100)) (snd (#2 iii_3100)) in
            ((true, fst r_ys__ys_5299), (false, (true, 0)), (true, snd r_ys__ys_5299))
        else
          if fst (#2 iii_3100) = false then
            let r_ys__xs_5257 = ys__xs_3700 (snd (#0 iii_3100)) (snd (#1 iii_3100)) in
            ((true, fst r_ys__xs_5257), (true, snd r_ys__xs_5257), (false, (true, 0)))
          else
            let r_ys__xs__ys_5225 = ys__xs__ys_3654 (snd (#0 iii_3100)) (snd (#1 iii_3100)) (snd (#2 iii_3100)) in
            ((true, #0 r_ys__xs__ys_5225), (true, #1 r_ys__xs__ys_5225), (true, #2 r_ys__xs__ys_5225))
    in
    ys__xs__ys_1990
  else
    if fst (snd (fst r_xs__ys_6041)) <> false then
      let xs'_1014 x_1269 = snd (fst (xs__ys_1023 ((true, x_1269 + 1), (false, 0)))) in
      let rec xs'__ys_3363 x_3337 x_3338 =
        let r_xs__ys_6024 = xs__ys_1023 ((true, x_3337 + 1), (true, x_3338)) in
        (snd (fst r_xs__ys_6024), snd (snd r_xs__ys_6024))
      in
      let xs'__ys_1965 ii_2944 =
        if fst (fst ii_2944) = false then
          if fst (snd ii_2944) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, ys_1940 (snd (snd ii_2944))))
        else
          if fst (snd ii_2944) = false then
            ((true, xs'_1014 (snd (fst ii_2944))), (false, (true, 0)))
          else
            let r_xs'__ys_4413 = xs'__ys_3363 (snd (fst ii_2944)) (snd (snd ii_2944)) in
            ((true, fst r_xs'__ys_4413), (true, snd r_xs'__ys_4413))
      in
      let xs'_1966 i_2924 = snd (fst (xs'__ys_1965 ((true, i_2924), (false, 0)))) in
      let ys_1967 i_2917 = snd (snd (xs'__ys_1965 ((false, 0), (true, i_2917)))) in
      let r_append_4560 = append_1165 xs'__ys_1965 in
      let r_append_xs'__ys_0_1969 i_2906 = snd (#0 (r_append_4560 ((true, i_2906), (false, 0), (false, 0)))) in
      let r_append_xs'__ys_1_1970 i_2896 = snd (#1 (r_append_4560 ((false, 0), (true, i_2896), (false, 0)))) in
      let r_append_xs'__ys_2_1971 i_2886 = snd (#2 (r_append_4560 ((false, 0), (false, 0), (true, i_2886)))) in
      let rs'_1195 i_1369 =
        if i_1369 = 0 then
          (true, snd (snd (fst r_xs__ys_6041)))
        else
          snd (#0 (r_append_4560 ((true, i_1369 - 1), (false, 0), (false, 0))))
      in
      let rec rs'__xs_3491 x_3455 x_3456 =
        if x_3455 = 0 then
          ((true, snd (snd (fst r_xs__ys_6041))), snd (fst (xs__ys_1023 ((true, x_3456), (false, 0)))))
        else
          let r_r_append_5952 = r_append_4560 ((true, x_3455 - 1), (false, 0), (false, 0)) in
          (snd (#0 r_r_append_5952), snd (fst (xs__ys_1023 ((true, x_3456), (false, 0)))))
      in
      let rec rs'__ys_3552 x_3516 x_3517 =
        if x_3516 = 0 then
          let r_xs__ys_5935 = xs__ys_1023 ((true, 0), (true, x_3517)) in
          ((true, snd (snd (fst r_xs__ys_5935))), snd (snd r_xs__ys_5935))
        else
          let r_r_append_5924 = r_append_4560 ((true, x_3516 - 1), (false, 0), (false, 0)) in
          (snd (#0 r_r_append_5924), snd (snd (xs__ys_1023 ((false, 0), (true, x_3517)))))
      in
      let rec rs'__xs__ys_3426 x_3377 x_3378 x_3379 =
        if x_3377 = 0 then
          let r_xs__ys_4859 = xs__ys_1023 ((true, x_3378), (false, 0)) in
          let r_xs__ys_5907 = xs__ys_1023 ((true, 0), (true, x_3379)) in
          ((true, snd (snd (fst r_xs__ys_5907))), snd (fst r_xs__ys_4859), snd (snd r_xs__ys_5907))
        else
          let r_r_append_5895 = r_append_4560 ((true, x_3377 - 1), (false, 0), (false, 0)) in
          let r_xs__ys_5886 = xs__ys_1023 ((true, x_3378), (true, x_3379)) in
          (snd (#0 r_r_append_5895), snd (fst r_xs__ys_5886), snd (snd r_xs__ys_5886))
      in
      let rs'__xs__ys_1986 iii_2831 =
        if fst (#0 iii_2831) = false then
          if fst (#1 iii_2831) = false then
            if fst (#2 iii_2831) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2831))))
          else
            if fst (#2 iii_2831) = false then
              ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2831))), (false, (true, 0)))
            else
              let r_xs__ys_5049 = xs__ys_3318 (snd (#1 iii_2831)) (snd (#2 iii_2831)) in
              ((false, (true, 0)), (true, fst r_xs__ys_5049), (true, snd r_xs__ys_5049))
        else
          if fst (#1 iii_2831) = false then
            if fst (#2 iii_2831) = false then
              ((true, rs'_1195 (snd (#0 iii_2831))), (false, (true, 0)), (false, (true, 0)))
            else
              let r_rs'__ys_4965 = rs'__ys_3552 (snd (#0 iii_2831)) (snd (#2 iii_2831)) in
              ((true, fst r_rs'__ys_4965), (false, (true, 0)), (true, snd r_rs'__ys_4965))
          else
            if fst (#2 iii_2831) = false then
              let r_rs'__xs_4923 = rs'__xs_3491 (snd (#0 iii_2831)) (snd (#1 iii_2831)) in
              ((true, fst r_rs'__xs_4923), (true, snd r_rs'__xs_4923), (false, (true, 0)))
            else
              let r_rs'__xs__ys_4891 = rs'__xs__ys_3426 (snd (#0 iii_2831)) (snd (#1 iii_2831)) (snd (#2 iii_2831)) in
              ((true, #0 r_rs'__xs__ys_4891), (true, #1 r_rs'__xs__ys_4891), (true, #2 r_rs'__xs__ys_4891))
      in
      rs'__xs__ys_1986
    else
      _|_
in
let main_1017 i_1018 n_1019 =
  let r_make_list_5550 = make_list_1008 n_1019 in
  let f_1732 x_1560 = (false, 0) in
  let r_make_list__f_1999 ix_2285 =
    if fst (fst ix_2285) = false then
      if fst (snd ix_2285) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, f_1732 (snd (snd ix_2285))))
    else
      if fst (snd ix_2285) = false then
        ((true, r_make_list_5550 (snd (fst ix_2285))), (false, (true, 0)))
      else
        let r_r_make_list_5562 = r_make_list_5550 (snd (fst ix_2285)) in
        ((true, r_r_make_list_5562), (true, f_1732 (snd (snd ix_2285))))
  in
  let xs_2000 i_2265 = snd (fst (r_make_list__f_1999 ((true, i_2265), (false, 0)))) in
  let f_2001 x_2258 = snd (snd (r_make_list__f_1999 ((false, 0), (true, x_2258)))) in
  let r_append_5708 = append_1165 r_make_list__f_1999 in
  let r_append_xs__f_0_2003 i_2247 = snd (#0 (r_append_5708 ((true, i_2247), (false, 0), (false, 0)))) in
  let r_append_xs__f_1_2004 i_2237 = snd (#1 (r_append_5708 ((false, 0), (true, i_2237), (false, 0)))) in
  let r_append_xs__f_2_2005 i_2227 = snd (#2 (r_append_5708 ((false, 0), (false, 0), (true, i_2227)))) in
  let r_r_append_5826 = r_append_5708 ((true, i_1018), (false, 0), (false, 0)) in
  let r_r_make_list_5809 = r_make_list_5550 i_1018 in
  if snd (snd (#0 r_r_append_5826)) = snd r_r_make_list_5809 then
    ()
  else
    {fail} ()
in
let r_f_5821 = rand_int () in
let r_f_5823 = rand_int () in
let r_main_5824 = main_1017 r_f_5821 in
let r_r_main_5825 = r_main_5824 r_f_5823 in
let r_r_main_2019 = r_r_main_5825 in
()

tupling:
 let rec make_list_1008 (n_1009:int) =
   if n_1009 < 0 then
     fun (x_1236:int) -> (false, 0)
   else
     let r_f_3805 = rand_int () in
     let r_make_list_3808 = make_list_1008 (n_1009 - 1) in
     fun (i_1226:int) -> (if i_1226 = 0 then
                            (true, r_f_3805)
                          else
                            r_make_list_3808 (i_1226 - 1))
 in
 let rec append_1165 (xs__ys_1023:(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int))))) =
   let xs_1939 (i_3153:int) = snd (fst (xs__ys_1023 ((true, i_3153), (false, 0)))) in
   let ys_1940 (i_3146:int) = snd (snd (xs__ys_1023 ((false, 0), (true, i_3146)))) in
   let rec ys__ys_3745 (x_3719:int) (x_3720:int) =
     let r_xs__ys_3874 = xs__ys_1023 ((false, 0), (true, x_3719)) in
     (snd (snd r_xs__ys_3874), snd (snd (xs__ys_1023 ((false, 0), (true, x_3720)))))
   in
   let rec xs__ys_3318 (x_3292:int) (x_3293:int) =
     let r_xs__ys_6068 = xs__ys_1023 ((true, x_3292), (true, x_3293)) in
     (snd (fst r_xs__ys_6068), snd (snd r_xs__ys_6068))
   in
   let rec ys__xs__ys_3654 (x_3615:int) (x_3616:int) (x_3617:int) =
     let r_xs__ys_3936 = xs__ys_1023 ((false, 0), (true, x_3615)) in
     let r_xs__ys_6059 = xs__ys_1023 ((true, x_3616), (true, x_3617)) in
     (snd (snd r_xs__ys_3936), snd (fst r_xs__ys_6059), snd (snd r_xs__ys_6059))
   in
   let rec ys__xs_3700 (x_3674:int) (x_3675:int) =
     let r_xs__ys_6049 = xs__ys_1023 ((true, x_3675), (true, x_3674)) in
     (snd (snd r_xs__ys_6049), snd (fst r_xs__ys_6049))
   in
   let r_xs__ys_6041 = xs__ys_1023 ((true, 0), (false, 0)) in
   if fst (snd (fst r_xs__ys_6041)) = false then
     let ys__xs__ys_1990 (iii_3100:((bool * int) * (bool * int) * (bool * int))) =
       if fst (#0 iii_3100) = false then
         if fst (#1 iii_3100) = false then
           if fst (#2 iii_3100) = false then
             ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
           else
             ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_3100))))
         else
           if fst (#2 iii_3100) = false then
             ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_3100))), (false, (true, 0)))
           else
             let r_xs__ys_5383 = xs__ys_3318 (snd (#1 iii_3100)) (snd (#2 iii_3100)) in
             ((false, (true, 0)), (true, fst r_xs__ys_5383), (true, snd r_xs__ys_5383))
       else
         if fst (#1 iii_3100) = false then
           if fst (#2 iii_3100) = false then
             ((true, ys_1940 (snd (#0 iii_3100))), (false, (true, 0)), (false, (true, 0)))
           else
             let r_ys__ys_5299 = ys__ys_3745 (snd (#0 iii_3100)) (snd (#2 iii_3100)) in
             ((true, fst r_ys__ys_5299), (false, (true, 0)), (true, snd r_ys__ys_5299))
         else
           if fst (#2 iii_3100) = false then
             let r_ys__xs_5257 = ys__xs_3700 (snd (#0 iii_3100)) (snd (#1 iii_3100)) in
             ((true, fst r_ys__xs_5257), (true, snd r_ys__xs_5257), (false, (true, 0)))
           else
             let r_ys__xs__ys_5225 = ys__xs__ys_3654 (snd (#0 iii_3100)) (snd (#1 iii_3100)) (snd (#2 iii_3100)) in
             ((true, #0 r_ys__xs__ys_5225), (true, #1 r_ys__xs__ys_5225), (true, #2 r_ys__xs__ys_5225))
     in
     ys__xs__ys_1990
   else
     if fst (snd (fst r_xs__ys_6041)) <> false then
       let xs'_1014 (x_1269:int) = snd (fst (xs__ys_1023 ((true, x_1269 + 1), (false, 0)))) in
       let rec xs'__ys_3363 (x_3337:int) (x_3338:int) =
         let r_xs__ys_6024 = xs__ys_1023 ((true, x_3337 + 1), (true, x_3338)) in
         (snd (fst r_xs__ys_6024), snd (snd r_xs__ys_6024))
       in
       let xs'__ys_1965 (ii_2944:((bool * int) * (bool * int))) =
         if fst (fst ii_2944) = false then
           if fst (snd ii_2944) = false then
             ((false, (true, 0)), (false, (true, 0)))
           else
             ((false, (true, 0)), (true, ys_1940 (snd (snd ii_2944))))
         else
           if fst (snd ii_2944) = false then
             ((true, xs'_1014 (snd (fst ii_2944))), (false, (true, 0)))
           else
             let r_xs'__ys_4413 = xs'__ys_3363 (snd (fst ii_2944)) (snd (snd ii_2944)) in
             ((true, fst r_xs'__ys_4413), (true, snd r_xs'__ys_4413))
       in
       let xs'_1966 (i_2924:int) = snd (fst (xs'__ys_1965 ((true, i_2924), (false, 0)))) in
       let ys_1967 (i_2917:int) = snd (snd (xs'__ys_1965 ((false, 0), (true, i_2917)))) in
       let r_append_4560 = append_1165 xs'__ys_1965 in
       let r_append_xs'__ys_0_1969 (i_2906:int) = snd (#0 (r_append_4560 ((true, i_2906), (false, 0), (false, 0)))) in
       let r_append_xs'__ys_1_1970 (i_2896:int) = snd (#1 (r_append_4560 ((false, 0), (true, i_2896), (false, 0)))) in
       let r_append_xs'__ys_2_1971 (i_2886:int) = snd (#2 (r_append_4560 ((false, 0), (false, 0), (true, i_2886)))) in
       let rs'_1195 (i_1369:int) =
         if i_1369 = 0 then
           (true, snd (snd (fst r_xs__ys_6041)))
         else
           snd (#0 (r_append_4560 ((true, i_1369 - 1), (false, 0), (false, 0))))
       in
       let rec rs'__xs_3491 (x_3455:int) (x_3456:int) =
         if x_3455 = 0 then
           ((true, snd (snd (fst r_xs__ys_6041))), snd (fst (xs__ys_1023 ((true, x_3456), (false, 0)))))
         else
           let r_r_append_5952 = r_append_4560 ((true, x_3455 - 1), (false, 0), (false, 0)) in
           (snd (#0 r_r_append_5952), snd (fst (xs__ys_1023 ((true, x_3456), (false, 0)))))
       in
       let rec rs'__ys_3552 (x_3516:int) (x_3517:int) =
         if x_3516 = 0 then
           let r_xs__ys_5935 = xs__ys_1023 ((true, 0), (true, x_3517)) in
           ((true, snd (snd (fst r_xs__ys_5935))), snd (snd r_xs__ys_5935))
         else
           let r_r_append_5924 = r_append_4560 ((true, x_3516 - 1), (false, 0), (false, 0)) in
           (snd (#0 r_r_append_5924), snd (snd (xs__ys_1023 ((false, 0), (true, x_3517)))))
       in
       let rec rs'__xs__ys_3426 (x_3377:int) (x_3378:int) (x_3379:int) =
         if x_3377 = 0 then
           let r_xs__ys_4859 = xs__ys_1023 ((true, x_3378), (false, 0)) in
           let r_xs__ys_5907 = xs__ys_1023 ((true, 0), (true, x_3379)) in
           ((true, snd (snd (fst r_xs__ys_5907))), snd (fst r_xs__ys_4859), snd (snd r_xs__ys_5907))
         else
           let r_r_append_5895 = r_append_4560 ((true, x_3377 - 1), (false, 0), (false, 0)) in
           let r_xs__ys_5886 = xs__ys_1023 ((true, x_3378), (true, x_3379)) in
           (snd (#0 r_r_append_5895), snd (fst r_xs__ys_5886), snd (snd r_xs__ys_5886))
       in
       let rs'__xs__ys_1986 (iii_2831:((bool * int) * (bool * int) * (bool * int))) =
         if fst (#0 iii_2831) = false then
           if fst (#1 iii_2831) = false then
             if fst (#2 iii_2831) = false then
               ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
             else
               ((false, (true, 0)), (false, (true, 0)), (true, ys_1940 (snd (#2 iii_2831))))
           else
             if fst (#2 iii_2831) = false then
               ((false, (true, 0)), (true, xs_1939 (snd (#1 iii_2831))), (false, (true, 0)))
             else
               let r_xs__ys_5049 = xs__ys_3318 (snd (#1 iii_2831)) (snd (#2 iii_2831)) in
               ((false, (true, 0)), (true, fst r_xs__ys_5049), (true, snd r_xs__ys_5049))
         else
           if fst (#1 iii_2831) = false then
             if fst (#2 iii_2831) = false then
               ((true, rs'_1195 (snd (#0 iii_2831))), (false, (true, 0)), (false, (true, 0)))
             else
               let r_rs'__ys_4965 = rs'__ys_3552 (snd (#0 iii_2831)) (snd (#2 iii_2831)) in
               ((true, fst r_rs'__ys_4965), (false, (true, 0)), (true, snd r_rs'__ys_4965))
           else
             if fst (#2 iii_2831) = false then
               let r_rs'__xs_4923 = rs'__xs_3491 (snd (#0 iii_2831)) (snd (#1 iii_2831)) in
               ((true, fst r_rs'__xs_4923), (true, snd r_rs'__xs_4923), (false, (true, 0)))
             else
               let r_rs'__xs__ys_4891 = rs'__xs__ys_3426 (snd (#0 iii_2831)) (snd (#1 iii_2831)) (snd (#2 iii_2831)) in
               ((true, #0 r_rs'__xs__ys_4891), (true, #1 r_rs'__xs__ys_4891), (true, #2 r_rs'__xs__ys_4891))
       in
       rs'__xs__ys_1986
     else
       _|_
 in
 let main_1017 (i_1018:int) (n_1019:int) =
   let r_make_list_5550 = make_list_1008 n_1019 in
   let f_1732 (x_1560:int) = (false, 0) in
   let r_make_list__f_1999 (ix_2285:((bool * int) * (bool * int))) =
     if fst (fst ix_2285) = false then
       if fst (snd ix_2285) = false then
         ((false, (true, 0)), (false, (true, 0)))
       else
         ((false, (true, 0)), (true, f_1732 (snd (snd ix_2285))))
     else
       if fst (snd ix_2285) = false then
         ((true, r_make_list_5550 (snd (fst ix_2285))), (false, (true, 0)))
       else
         let r_r_make_list_5562 = r_make_list_5550 (snd (fst ix_2285)) in
         ((true, r_r_make_list_5562), (true, f_1732 (snd (snd ix_2285))))
   in
   let xs_2000 (i_2265:int) = snd (fst (r_make_list__f_1999 ((true, i_2265), (false, 0)))) in
   let f_2001 (x_2258:int) = snd (snd (r_make_list__f_1999 ((false, 0), (true, x_2258)))) in
   let r_append_5708 = append_1165 r_make_list__f_1999 in
   let r_append_xs__f_0_2003 (i_2247:int) = snd (#0 (r_append_5708 ((true, i_2247), (false, 0), (false, 0)))) in
   let r_append_xs__f_1_2004 (i_2237:int) = snd (#1 (r_append_5708 ((false, 0), (true, i_2237), (false, 0)))) in
   let r_append_xs__f_2_2005 (i_2227:int) = snd (#2 (r_append_5708 ((false, 0), (false, 0), (true, i_2227)))) in
   let r_r_append_5826 = r_append_5708 ((true, i_1018), (false, 0), (false, 0)) in
   let r_r_make_list_5809 = r_make_list_5550 i_1018 in
   if snd (snd (#0 r_r_append_5826)) = snd r_r_make_list_5809 then
     ()
   else
     {fail} ()
 in
 let r_f_5821 = rand_int () in
 let r_f_5823 = rand_int () in
 let r_main_5824 = main_1017 r_f_5821 in
 let r_r_main_5825 = r_main_5824 r_f_5823 in
 let r_r_main_2019 = r_r_main_5825 in
 ()

CPS:
 let rec make_list_1008 (n_1009:int) (k_make_list_6114:((int -> ((bool * int) -> X) -> X) -> X)) =
   if n_1009 < 0 then
     k_make_list_6114 (fun (x_1236:int) -> fun (k_make_list_6116:((bool * int) -> X)) -> k_make_list_6116 (false, 0))
   else
     let r_f_3805 (k_make_list_r_f_6132:(int -> X)) = rand_int_cps () k_make_list_r_f_6132 in
     r_f_3805
       (fun (r_f_6191:int) ->
          (let r_make_list_3808 (k_make_list_r_make_list_6153:((int -> ((bool * int) -> X) -> X) -> X)) =
             make_list_1008 (n_1009 - 1) k_make_list_r_make_list_6153
           in
           r_make_list_3808
             (fun (r_make_list_6190:(int -> ((bool * int) -> X) -> X)) ->
                k_make_list_6114
                  (fun (i_1226:int) ->
                     fun (k_make_list_6166:((bool * int) -> X)) ->
                       (if i_1226 = 0 then
                          k_make_list_6166 (true, r_f_6191)
                        else
                          r_make_list_6190 (i_1226 - 1) k_make_list_6166)))))
 in
 let rec
   append_1165
              (xs__ys_1023:(((bool * int) * (bool * int)) ->
                              (((bool * (bool * int)) * (bool * (bool * int))) -> X) -> X))
              (k_append_6214:((((bool * int) * (bool * int) * (bool * int)) ->
                                 (((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X) -> X)
                                -> X)) =
   let xs_1939 (i_3153:int) (k_append_xs_6221:((bool * int) -> X)) =
     xs__ys_1023 ((true, i_3153), (false, 0))
       (fun (p_9137:((bool * (bool * int)) * (bool * (bool * int)))) -> k_append_xs_6221 (snd (fst p_9137)))
   in
   let ys_1940 (i_3146:int) (k_append_ys_6265:((bool * int) -> X)) =
     xs__ys_1023 ((false, 0), (true, i_3146))
       (fun (p_9147:((bool * (bool * int)) * (bool * (bool * int)))) -> k_append_ys_6265 (snd (snd p_9147)))
   in
   let rec ys__ys_3745 (x_3719:int) (x_3720:int) (k_append_ys__ys_6309:(((bool * int) * (bool * int)) -> X)) =
     let r_xs__ys_3874 (k_append_ys__ys_r_xs__ys_6334:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
       xs__ys_1023 ((false, 0), (true, x_3719)) k_append_ys__ys_r_xs__ys_6334
     in
     r_xs__ys_3874
       (fun (r_xs__ys_6380:((bool * (bool * int)) * (bool * (bool * int)))) ->
          xs__ys_1023 ((false, 0), (true, x_3720))
            (fun (p_9165:((bool * (bool * int)) * (bool * (bool * int)))) ->
               k_append_ys__ys_6309 (snd (snd r_xs__ys_6380), snd (snd p_9165))))
   in
   let rec xs__ys_3318 (x_3292:int) (x_3293:int) (k_append_xs__ys_6391:(((bool * int) * (bool * int)) -> X)) =
     let r_xs__ys_6068 (k_append_xs__ys_r_xs__ys_6416:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
       xs__ys_1023 ((true, x_3292), (true, x_3293)) k_append_xs__ys_r_xs__ys_6416
     in
     r_xs__ys_6068
       (fun (r_xs__ys_6428:((bool * (bool * int)) * (bool * (bool * int)))) ->
          k_append_xs__ys_6391 (snd (fst r_xs__ys_6428), snd (snd r_xs__ys_6428)))
   in
   let rec
     ys__xs__ys_3654 (x_3615:int) (x_3616:int) (x_3617:int) 
                    (k_append_ys__xs__ys_6439:(((bool * int) * (bool * int) * (bool * int)) -> X)) =
     let r_xs__ys_3936 (k_append_ys__xs__ys_r_xs__ys_6464:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
       xs__ys_1023 ((false, 0), (true, x_3615)) k_append_ys__xs__ys_r_xs__ys_6464
     in
     r_xs__ys_3936
       (fun (r_xs__ys_6509:((bool * (bool * int)) * (bool * (bool * int)))) ->
          (let
             r_xs__ys_6059 (k_append_ys__xs__ys_r_xs__ys_6494:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
             xs__ys_1023 ((true, x_3616), (true, x_3617)) k_append_ys__xs__ys_r_xs__ys_6494
           in
           r_xs__ys_6059
             (fun (r_xs__ys_6508:((bool * (bool * int)) * (bool * (bool * int)))) ->
                k_append_ys__xs__ys_6439 (snd (snd r_xs__ys_6509), snd (fst r_xs__ys_6508), snd (snd r_xs__ys_6508)))))
   in
   let rec ys__xs_3700 (x_3674:int) (x_3675:int) (k_append_ys__xs_6521:(((bool * int) * (bool * int)) -> X)) =
     let r_xs__ys_6049 (k_append_ys__xs_r_xs__ys_6546:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
       xs__ys_1023 ((true, x_3675), (true, x_3674)) k_append_ys__xs_r_xs__ys_6546
     in
     r_xs__ys_6049
       (fun (r_xs__ys_6558:((bool * (bool * int)) * (bool * (bool * int)))) ->
          k_append_ys__xs_6521 (snd (snd r_xs__ys_6558), snd (fst r_xs__ys_6558)))
   in
   let r_xs__ys_6041 (k_append_r_xs__ys_6590:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
     xs__ys_1023 ((true, 0), (false, 0)) k_append_r_xs__ys_6590
   in
   r_xs__ys_6041
     (fun (r_xs__ys_8447:((bool * (bool * int)) * (bool * (bool * int)))) ->
        (if fst (snd (fst r_xs__ys_8447)) = false then
           k_append_6214
             (let
                ys__xs__ys_1990 (iii_3100:((bool * int) * (bool * int) * (bool * int))) 
                               (k_append_ys__xs__ys_6600:(((bool * (bool * int)) * (
                                                           bool * (bool * int)) * (
                                                           bool * (bool * int))) -> X)) =
                if fst (#0 iii_3100) = false then
                  if fst (#1 iii_3100) = false then
                    if fst (#2 iii_3100) = false then
                      k_append_ys__xs__ys_6600 ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
                    else
                      ys_1940 (snd (#2 iii_3100))
                        (fun (x_9823:(bool * int)) ->
                           k_append_ys__xs__ys_6600 ((false, (true, 0)), (false, (true, 0)), (true, x_9823)))
                  else
                    if fst (#2 iii_3100) = false then
                      xs_1939 (snd (#1 iii_3100))
                        (fun (x_9810:(bool * int)) ->
                           k_append_ys__xs__ys_6600 ((false, (true, 0)), (true, x_9810), (false, (true, 0))))
                    else
                      let r_xs__ys_5383 (k_append_ys__xs__ys_r_xs__ys_6752:(((bool * int) * (bool * int)) -> X)) =
                        xs__ys_3318 (snd (#1 iii_3100)) (snd (#2 iii_3100)) k_append_ys__xs__ys_r_xs__ys_6752
                      in
                      r_xs__ys_5383
                        (fun (r_xs__ys_6790:((bool * int) * (bool * int))) ->
                           k_append_ys__xs__ys_6600
                             ((false, (true, 0)), (true, fst r_xs__ys_6790), (true, snd r_xs__ys_6790)))
                else
                  if fst (#1 iii_3100) = false then
                    if fst (#2 iii_3100) = false then
                      ys_1940 (snd (#0 iii_3100))
                        (fun (x_9767:(bool * int)) ->
                           k_append_ys__xs__ys_6600 ((true, x_9767), (false, (true, 0)), (false, (true, 0))))
                    else
                      let r_ys__ys_5299 (k_append_ys__xs__ys_r_ys__ys_6854:(((bool * int) * (bool * int)) -> X)) =
                        ys__ys_3745 (snd (#0 iii_3100)) (snd (#2 iii_3100)) k_append_ys__xs__ys_r_ys__ys_6854
                      in
                      r_ys__ys_5299
                        (fun (r_ys__ys_6892:((bool * int) * (bool * int))) ->
                           k_append_ys__xs__ys_6600
                             ((true, fst r_ys__ys_6892), (false, (true, 0)), (true, snd r_ys__ys_6892)))
                  else
                    if fst (#2 iii_3100) = false then
                      let r_ys__xs_5257 (k_append_ys__xs__ys_r_ys__xs_6904:(((bool * int) * (bool * int)) -> X)) =
                        ys__xs_3700 (snd (#0 iii_3100)) (snd (#1 iii_3100)) k_append_ys__xs__ys_r_ys__xs_6904
                      in
                      r_ys__xs_5257
                        (fun (r_ys__xs_6942:((bool * int) * (bool * int))) ->
                           k_append_ys__xs__ys_6600
                             ((true, fst r_ys__xs_6942), (true, snd r_ys__xs_6942), (false, (true, 0))))
                    else
                      let
                        r_ys__xs__ys_5225
                                         (k_append_ys__xs__ys_r_ys__xs__ys_6951:(
                                         ((bool * int) * (bool * int) * (bool * int)) -> X)) =
                        ys__xs__ys_3654 (snd (#0 iii_3100)) (snd (#1 iii_3100)) (
                          snd (#2 iii_3100)) k_append_ys__xs__ys_r_ys__xs__ys_6951
                      in
                      r_ys__xs__ys_5225
                        (fun (r_ys__xs__ys_6983:((bool * int) * (bool * int) * (bool * int))) ->
                           k_append_ys__xs__ys_6600
                             ((true, #0 r_ys__xs__ys_6983), (true, #1 r_ys__xs__ys_6983), (true, #2 r_ys__xs__ys_6983)))
              in
              ys__xs__ys_1990)
         else
           if fst (snd (fst r_xs__ys_8447)) <> false then
             let xs'_1014 (x_1269:int) (k_append_xs'_7005:((bool * int) -> X)) =
               xs__ys_1023 ((true, x_1269 + 1), (false, 0))
                 (fun (p_9217:((bool * (bool * int)) * (bool * (bool * int)))) -> k_append_xs'_7005 (snd (fst p_9217)))
             in
             let rec
               xs'__ys_3363 (x_3337:int) (x_3338:int) (k_append_xs'__ys_7049:(((bool * int) * (bool * int)) -> X)) =
               let
                 r_xs__ys_6024 (k_append_xs'__ys_r_xs__ys_7074:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                 xs__ys_1023 ((true, x_3337 + 1), (true, x_3338)) k_append_xs'__ys_r_xs__ys_7074
               in
               r_xs__ys_6024
                 (fun (r_xs__ys_7086:((bool * (bool * int)) * (bool * (bool * int)))) ->
                    k_append_xs'__ys_7049 (snd (fst r_xs__ys_7086), snd (snd r_xs__ys_7086)))
             in
             let
               xs'__ys_1965 (ii_2944:((bool * int) * (bool * int))) 
                           (k_append_xs'__ys_7097:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
               if fst (fst ii_2944) = false then
                 if fst (snd ii_2944) = false then
                   k_append_xs'__ys_7097 ((false, (true, 0)), (false, (true, 0)))
                 else
                   ys_1940 (snd (snd ii_2944))
                     (fun (x_9246:(bool * int)) -> k_append_xs'__ys_7097 ((false, (true, 0)), (true, x_9246)))
               else
                 if fst (snd ii_2944) = false then
                   xs'_1014 (snd (fst ii_2944))
                     (fun (x_9243:(bool * int)) -> k_append_xs'__ys_7097 ((true, x_9243), (false, (true, 0))))
                 else
                   let r_xs'__ys_4413 (k_append_xs'__ys_r_xs'__ys_7207:(((bool * int) * (bool * int)) -> X)) =
                     xs'__ys_3363 (snd (fst ii_2944)) (snd (snd ii_2944)) k_append_xs'__ys_r_xs'__ys_7207
                   in
                   r_xs'__ys_4413
                     (fun (r_xs'__ys_7231:((bool * int) * (bool * int))) ->
                        k_append_xs'__ys_7097 ((true, fst r_xs'__ys_7231), (true, snd r_xs'__ys_7231)))
             in
             let
               r_append_4560
                            (k_append_r_append_7352:((((bool * int) * (bool * int) * (bool * int)) ->
                                                        (((bool * (bool * int)) * (
                                                          bool * (bool * int)) * (
                                                          bool * (bool * int))) -> X) -> X) -> X)) =
               append_1165 xs'__ys_1965 k_append_r_append_7352
             in
             r_append_4560
               (fun (r_append_8406:(((bool * int) * (bool * int) * (bool * int)) ->
                                      (((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X) ->
                                        X)) ->
                  k_append_6214
                    (let rs'_1195 (i_1369:int) (k_append_rs'_7515:((bool * int) -> X)) =
                       if i_1369 = 0 then
                         k_append_rs'_7515 (true, snd (snd (fst r_xs__ys_8447)))
                       else
                         r_append_8406 ((true, i_1369 - 1), (false, 0), (false, 0))
                           (fun (p_9366:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                              k_append_rs'_7515 (snd (#0 p_9366)))
                     in
                     let rec
                       rs'__xs_3491 (x_3455:int) (x_3456:int) 
                                   (k_append_rs'__xs_7573:(((bool * int) * (bool * int)) -> X)) =
                       if x_3455 = 0 then
                         xs__ys_1023 ((true, x_3456), (false, 0))
                           (fun (p_9405:((bool * (bool * int)) * (bool * (bool * int)))) ->
                              k_append_rs'__xs_7573 ((true, snd (snd (fst r_xs__ys_8447))), snd (fst p_9405)))
                       else
                         let
                           r_r_append_5952
                                          (k_append_rs'__xs_r_r_append_7652:(
                                          ((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                           r_append_8406 ((true, x_3455 - 1), (false, 0), (false, 0)) k_append_rs'__xs_r_r_append_7652
                         in
                         r_r_append_5952
                           (fun (r_r_append_7698:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                              xs__ys_1023 ((true, x_3456), (false, 0))
                                (fun (p_9393:((bool * (bool * int)) * (bool * (bool * int)))) ->
                                   k_append_rs'__xs_7573 (snd (#0 r_r_append_7698), snd (fst p_9393))))
                     in
                     let rec
                       rs'__ys_3552 (x_3516:int) (x_3517:int) 
                                   (k_append_rs'__ys_7708:(((bool * int) * (bool * int)) -> X)) =
                       if x_3516 = 0 then
                         let
                           r_xs__ys_5935
                                        (k_append_rs'__ys_r_xs__ys_7733:(
                                        ((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                           xs__ys_1023 ((true, 0), (true, x_3517)) k_append_rs'__ys_r_xs__ys_7733
                         in
                         r_xs__ys_5935
                           (fun (r_xs__ys_7751:((bool * (bool * int)) * (bool * (bool * int)))) ->
                              k_append_rs'__ys_7708 ((true, snd (snd (fst r_xs__ys_7751))), snd (snd r_xs__ys_7751)))
                       else
                         let
                           r_r_append_5924
                                          (k_append_rs'__ys_r_r_append_7784:(
                                          ((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                           r_append_8406 ((true, x_3516 - 1), (false, 0), (false, 0)) k_append_rs'__ys_r_r_append_7784
                         in
                         r_r_append_5924
                           (fun (r_r_append_7830:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                              xs__ys_1023 ((false, 0), (true, x_3517))
                                (fun (p_9438:((bool * (bool * int)) * (bool * (bool * int)))) ->
                                   k_append_rs'__ys_7708 (snd (#0 r_r_append_7830), snd (snd p_9438))))
                     in
                     let rec
                       rs'__xs__ys_3426 (x_3377:int) (x_3378:int) (x_3379:int) 
                                       (k_append_rs'__xs__ys_7841:(((bool * int) * (bool * int) * (bool * int)) -> X)) =
                       if x_3377 = 0 then
                         let
                           r_xs__ys_4859
                                        (k_append_rs'__xs__ys_r_xs__ys_7866:(
                                        ((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                           xs__ys_1023 ((true, x_3378), (false, 0)) k_append_rs'__xs__ys_r_xs__ys_7866
                         in
                         r_xs__ys_4859
                           (fun (r_xs__ys_7917:((bool * (bool * int)) * (bool * (bool * int)))) ->
                              (let
                                 r_xs__ys_5907
                                              (k_append_rs'__xs__ys_r_xs__ys_7896:(
                                              ((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                                 xs__ys_1023 ((true, 0), (true, x_3379)) k_append_rs'__xs__ys_r_xs__ys_7896
                               in
                               r_xs__ys_5907
                                 (fun (r_xs__ys_7916:((bool * (bool * int)) * (bool * (bool * int)))) ->
                                    k_append_rs'__xs__ys_7841
                                      ((true, snd (snd (fst r_xs__ys_7916))), 
                                       snd (fst r_xs__ys_7917), snd (snd r_xs__ys_7916)))))
                       else
                         let
                           r_r_append_5895
                                          (k_append_rs'__xs__ys_r_r_append_7950:(
                                          ((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                           r_append_8406 ((true, x_3377 - 1), (false, 0), (false, 0))
                             k_append_rs'__xs__ys_r_r_append_7950
                         in
                         r_r_append_5895
                           (fun (r_r_append_7995:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                              (let
                                 r_xs__ys_5886
                                              (k_append_rs'__xs__ys_r_xs__ys_7980:(
                                              ((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                                 xs__ys_1023 ((true, x_3378), (true, x_3379)) k_append_rs'__xs__ys_r_xs__ys_7980
                               in
                               r_xs__ys_5886
                                 (fun (r_xs__ys_7994:((bool * (bool * int)) * (bool * (bool * int)))) ->
                                    k_append_rs'__xs__ys_7841
                                      (snd (#0 r_r_append_7995), snd (fst r_xs__ys_7994), snd (snd r_xs__ys_7994)))))
                     in
                     let
                       rs'__xs__ys_1986 (iii_2831:((bool * int) * (bool * int) * (bool * int))) 
                                       (k_append_rs'__xs__ys_8004:(((
                                                                    bool * (bool * int)) * (
                                                                    bool * (bool * int)) * (
                                                                    bool * (bool * int))) -> X)) =
                       if fst (#0 iii_2831) = false then
                         if fst (#1 iii_2831) = false then
                           if fst (#2 iii_2831) = false then
                             k_append_rs'__xs__ys_8004 ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
                           else
                             ys_1940 (snd (#2 iii_2831))
                               (fun (x_9628:(bool * int)) ->
                                  k_append_rs'__xs__ys_8004 ((false, (true, 0)), (false, (true, 0)), (true, x_9628)))
                         else
                           if fst (#2 iii_2831) = false then
                             xs_1939 (snd (#1 iii_2831))
                               (fun (x_9615:(bool * int)) ->
                                  k_append_rs'__xs__ys_8004 ((false, (true, 0)), (true, x_9615), (false, (true, 0))))
                           else
                             let
                               r_xs__ys_5049 (k_append_rs'__xs__ys_r_xs__ys_8156:(((bool * int) * (bool * int)) -> X)) =
                               xs__ys_3318 (snd (#1 iii_2831)) (snd (#2 iii_2831)) k_append_rs'__xs__ys_r_xs__ys_8156
                             in
                             r_xs__ys_5049
                               (fun (r_xs__ys_8194:((bool * int) * (bool * int))) ->
                                  k_append_rs'__xs__ys_8004
                                    ((false, (true, 0)), (true, fst r_xs__ys_8194), (true, snd r_xs__ys_8194)))
                       else
                         if fst (#1 iii_2831) = false then
                           if fst (#2 iii_2831) = false then
                             rs'_1195 (snd (#0 iii_2831))
                               (fun (x_9572:(bool * int)) ->
                                  k_append_rs'__xs__ys_8004 ((true, x_9572), (false, (true, 0)), (false, (true, 0))))
                           else
                             let
                               r_rs'__ys_4965
                                             (k_append_rs'__xs__ys_r_rs'__ys_8258:(
                                             ((bool * int) * (bool * int)) -> X)) =
                               rs'__ys_3552 (snd (#0 iii_2831)) (snd (#2 iii_2831)) k_append_rs'__xs__ys_r_rs'__ys_8258
                             in
                             r_rs'__ys_4965
                               (fun (r_rs'__ys_8296:((bool * int) * (bool * int))) ->
                                  k_append_rs'__xs__ys_8004
                                    ((true, fst r_rs'__ys_8296), (false, (true, 0)), (true, snd r_rs'__ys_8296)))
                         else
                           if fst (#2 iii_2831) = false then
                             let
                               r_rs'__xs_4923
                                             (k_append_rs'__xs__ys_r_rs'__xs_8308:(
                                             ((bool * int) * (bool * int)) -> X)) =
                               rs'__xs_3491 (snd (#0 iii_2831)) (snd (#1 iii_2831)) k_append_rs'__xs__ys_r_rs'__xs_8308
                             in
                             r_rs'__xs_4923
                               (fun (r_rs'__xs_8346:((bool * int) * (bool * int))) ->
                                  k_append_rs'__xs__ys_8004
                                    ((true, fst r_rs'__xs_8346), (true, snd r_rs'__xs_8346), (false, (true, 0))))
                           else
                             let
                               r_rs'__xs__ys_4891
                                                 (k_append_rs'__xs__ys_r_rs'__xs__ys_8355:(
                                                 ((bool * int) * (bool * int) * (bool * int)) -> X)) =
                               rs'__xs__ys_3426 (snd (#0 iii_2831)) (
                                 snd (#1 iii_2831)) (snd (#2 iii_2831)) k_append_rs'__xs__ys_r_rs'__xs__ys_8355
                             in
                             r_rs'__xs__ys_4891
                               (fun (r_rs'__xs__ys_8387:((bool * int) * (bool * int) * (bool * int))) ->
                                  k_append_rs'__xs__ys_8004
                                    ((true, #0 r_rs'__xs__ys_8387), (true, #1 r_rs'__xs__ys_8387), 
                                     (true, #2 r_rs'__xs__ys_8387)))
                     in
                     rs'__xs__ys_1986))
           else
             _|_))
 in
 let main_1017 (i_1018:int) (n_1019:int) (k_main_8489:(unit -> X)) =
   let r_make_list_5550 (k_main_r_make_list_8502:((int -> ((bool * int) -> X) -> X) -> X)) =
     make_list_1008 n_1019 k_main_r_make_list_8502
   in
   r_make_list_5550
     (fun (r_make_list_9048:(int -> ((bool * int) -> X) -> X)) ->
        (let f_1732 (x_1560:int) (k_main_f_8517:((bool * int) -> X)) = k_main_f_8517 (false, 0) in
         let
           r_make_list__f_1999 (ix_2285:((bool * int) * (bool * int))) 
                              (k_main_r_make_list__f_8530:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
           if fst (fst ix_2285) = false then
             if fst (snd ix_2285) = false then
               k_main_r_make_list__f_8530 ((false, (true, 0)), (false, (true, 0)))
             else
               f_1732 (snd (snd ix_2285))
                 (fun (x_9904:(bool * int)) -> k_main_r_make_list__f_8530 ((false, (true, 0)), (true, x_9904)))
           else
             if fst (snd ix_2285) = false then
               r_make_list_9048 (snd (fst ix_2285))
                 (fun (x_9901:(bool * int)) -> k_main_r_make_list__f_8530 ((true, x_9901), (false, (true, 0))))
             else
               let r_r_make_list_5562 (k_main_r_make_list__f_r_r_make_list_8639:((bool * int) -> X)) =
                 r_make_list_9048 (snd (fst ix_2285)) k_main_r_make_list__f_r_r_make_list_8639
               in
               r_r_make_list_5562
                 (fun (r_r_make_list_8673:(bool * int)) ->
                    f_1732 (snd (snd ix_2285))
                      (fun (x_9883:(bool * int)) ->
                         k_main_r_make_list__f_8530 ((true, r_r_make_list_8673), (true, x_9883))))
         in
         let
           r_append_5708
                        (k_main_r_append_8785:((((bool * int) * (bool * int) * (bool * int)) ->
                                                  (((bool * (bool * int)) * (
                                                    bool * (bool * int)) * (
                                                    bool * (bool * int))) -> X) -> X) -> X)) =
           append_1165 r_make_list__f_1999 k_main_r_append_8785
         in
         r_append_5708
           (fun (r_append_9028:(((bool * int) * (bool * int) * (bool * int)) ->
                                  (((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X) -> X)) ->
              (let
                 r_r_append_5826
                                (k_main_r_r_append_8982:(((bool * (bool * int)) * (
                                                          bool * (bool * int)) * (
                                                          bool * (bool * int))) -> X)) =
                 r_append_9028 ((true, i_1018), (false, 0), (false, 0)) k_main_r_r_append_8982
               in
               r_r_append_5826
                 (fun (r_r_append_9012:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                    (let r_r_make_list_5809 (k_main_r_r_make_list_8994:((bool * int) -> X)) =
                       r_make_list_9048 i_1018 k_main_r_r_make_list_8994
                     in
                     r_r_make_list_5809
                       (fun (r_r_make_list_9011:(bool * int)) ->
                          (if snd (snd (#0 r_r_append_9012)) = snd r_r_make_list_9011 then
                             k_main_8489 ()
                           else
                             {|fail|} () k_main_8489))))))))
 in
 let r_f_5821 (k_r_f_9059:(int -> X)) = rand_int_cps () k_r_f_9059 in
 r_f_5821
   (fun (r_f_9104:int) ->
      (let r_f_5823 (k_r_f_9071:(int -> X)) = rand_int_cps () k_r_f_9071 in
       r_f_5823
         (fun (r_f_9103:int) ->
            (let r_r_main_5825 (k_r_r_main_9092:(unit -> X)) = (main_1017 r_f_9104) r_f_9103 k_r_r_main_9092 in
             r_r_main_5825 (fun (r_r_main_9098:unit) -> {end})))))

remove_pair:
 let rec make_list_1008 (n_1009:int) (k_make_list_6114:((int -> (bool -> int -> X) -> X) -> X)) =
   if n_1009 < 0 then
     k_make_list_6114 (fun (x_1236:int) -> fun (k_make_list_6116:(bool -> int -> X)) -> k_make_list_6116 false 0)
   else
     let r_f_3805 (k_make_list_r_f_6132:(int -> X)) = rand_int_cps () k_make_list_r_f_6132 in
     r_f_3805
       (fun (r_f_6191:int) ->
          (let r_make_list_3808 (k_make_list_r_make_list_6153:((int -> (bool -> int -> X) -> X) -> X)) =
             make_list_1008 (n_1009 - 1) k_make_list_r_make_list_6153
           in
           r_make_list_3808
             (fun (r_make_list_6190:(int -> (bool -> int -> X) -> X)) ->
                k_make_list_6114
                  (fun (i_1226:int) ->
                     fun (k_make_list_6166:(bool -> int -> X)) ->
                       (if i_1226 = 0 then
                          k_make_list_6166 true r_f_6191
                        else
                          r_make_list_6190 (i_1226 - 1) k_make_list_6166)))))
 in
 let rec
   append_1165 (xs__ys_1023:(bool -> int -> bool -> int -> (bool -> bool -> int -> bool -> bool -> int -> X) -> X)) 
              (k_append_6214:((bool ->
                                 int ->
                                   bool ->
                                     int ->
                                       bool ->
                                         int ->
                                           (bool -> bool -> int -> bool -> bool -> int -> bool -> bool -> int -> X) ->
                                             X) -> X)) =
   let xs_1939 (i_3153:int) (k_append_xs_6221:(bool -> int -> X)) =
     xs__ys_1023 true i_3153 false 0
       (fun (p00_9137:bool) ->
          fun (p010_9137:bool) ->
            fun (p011_9137:int) ->
              fun (p10_9137:bool) ->
                fun (p110_9137:bool) -> fun (p111_9137:int) -> k_append_xs_6221 p010_9137 p011_9137)
   in
   let ys_1940 (i_3146:int) (k_append_ys_6265:(bool -> int -> X)) =
     xs__ys_1023 false 0 true i_3146
       (fun (p00_9147:bool) ->
          fun (p010_9147:bool) ->
            fun (p011_9147:int) ->
              fun (p10_9147:bool) ->
                fun (p110_9147:bool) -> fun (p111_9147:int) -> k_append_ys_6265 p110_9147 p111_9147)
   in
   let rec ys__ys_3745 (x_3719:int) (x_3720:int) (k_append_ys__ys_6309:(bool -> int -> bool -> int -> X)) =
     let r_xs__ys_3874 (k_append_ys__ys_r_xs__ys_6334:(bool -> bool -> int -> bool -> bool -> int -> X)) =
       xs__ys_1023 false 0 true x_3719 k_append_ys__ys_r_xs__ys_6334
     in
     r_xs__ys_3874
       (fun (r_xs__ys00_6380:bool) ->
          fun (r_xs__ys010_6380:bool) ->
            fun (r_xs__ys011_6380:int) ->
              fun (r_xs__ys10_6380:bool) ->
                fun (r_xs__ys110_6380:bool) ->
                  fun (r_xs__ys111_6380:int) ->
                    xs__ys_1023 false 0 true x_3720
                      (fun (p00_9165:bool) ->
                         fun (p010_9165:bool) ->
                           fun (p011_9165:int) ->
                             fun (p10_9165:bool) ->
                               fun (p110_9165:bool) ->
                                 fun (p111_9165:int) ->
                                   k_append_ys__ys_6309 r_xs__ys110_6380 r_xs__ys111_6380 p110_9165 p111_9165))
   in
   let rec xs__ys_3318 (x_3292:int) (x_3293:int) (k_append_xs__ys_6391:(bool -> int -> bool -> int -> X)) =
     let r_xs__ys_6068 (k_append_xs__ys_r_xs__ys_6416:(bool -> bool -> int -> bool -> bool -> int -> X)) =
       xs__ys_1023 true x_3292 true x_3293 k_append_xs__ys_r_xs__ys_6416
     in
     r_xs__ys_6068
       (fun (r_xs__ys00_6428:bool) ->
          fun (r_xs__ys010_6428:bool) ->
            fun (r_xs__ys011_6428:int) ->
              fun (r_xs__ys10_6428:bool) ->
                fun (r_xs__ys110_6428:bool) ->
                  fun (r_xs__ys111_6428:int) ->
                    k_append_xs__ys_6391 r_xs__ys010_6428 r_xs__ys011_6428 r_xs__ys110_6428 r_xs__ys111_6428)
   in
   let rec
     ys__xs__ys_3654 (x_3615:int) (x_3616:int) (x_3617:int) 
                    (k_append_ys__xs__ys_6439:(bool -> int -> bool -> int -> bool -> int -> X)) =
     let r_xs__ys_3936 (k_append_ys__xs__ys_r_xs__ys_6464:(bool -> bool -> int -> bool -> bool -> int -> X)) =
       xs__ys_1023 false 0 true x_3615 k_append_ys__xs__ys_r_xs__ys_6464
     in
     r_xs__ys_3936
       (fun (r_xs__ys00_6509:bool) ->
          fun (r_xs__ys010_6509:bool) ->
            fun (r_xs__ys011_6509:int) ->
              fun (r_xs__ys10_6509:bool) ->
                fun (r_xs__ys110_6509:bool) ->
                  fun (r_xs__ys111_6509:int) ->
                    (let
                       r_xs__ys_6059
                                    (k_append_ys__xs__ys_r_xs__ys_6494:(
                                    bool -> bool -> int -> bool -> bool -> int -> X)) =
                       xs__ys_1023 true x_3616 true x_3617 k_append_ys__xs__ys_r_xs__ys_6494
                     in
                     r_xs__ys_6059
                       (fun (r_xs__ys00_6508:bool) ->
                          fun (r_xs__ys010_6508:bool) ->
                            fun (r_xs__ys011_6508:int) ->
                              fun (r_xs__ys10_6508:bool) ->
                                fun (r_xs__ys110_6508:bool) ->
                                  fun (r_xs__ys111_6508:int) ->
                                    k_append_ys__xs__ys_6439 r_xs__ys110_6509 r_xs__ys111_6509 r_xs__ys010_6508
                                      r_xs__ys011_6508 r_xs__ys110_6508 r_xs__ys111_6508)))
   in
   let rec ys__xs_3700 (x_3674:int) (x_3675:int) (k_append_ys__xs_6521:(bool -> int -> bool -> int -> X)) =
     let r_xs__ys_6049 (k_append_ys__xs_r_xs__ys_6546:(bool -> bool -> int -> bool -> bool -> int -> X)) =
       xs__ys_1023 true x_3675 true x_3674 k_append_ys__xs_r_xs__ys_6546
     in
     r_xs__ys_6049
       (fun (r_xs__ys00_6558:bool) ->
          fun (r_xs__ys010_6558:bool) ->
            fun (r_xs__ys011_6558:int) ->
              fun (r_xs__ys10_6558:bool) ->
                fun (r_xs__ys110_6558:bool) ->
                  fun (r_xs__ys111_6558:int) ->
                    k_append_ys__xs_6521 r_xs__ys110_6558 r_xs__ys111_6558 r_xs__ys010_6558 r_xs__ys011_6558)
   in
   let r_xs__ys_6041 (k_append_r_xs__ys_6590:(bool -> bool -> int -> bool -> bool -> int -> X)) =
     xs__ys_1023 true 0 false 0 k_append_r_xs__ys_6590
   in
   r_xs__ys_6041
     (fun (r_xs__ys00_8447:bool) ->
        fun (r_xs__ys010_8447:bool) ->
          fun (r_xs__ys011_8447:int) ->
            fun (r_xs__ys10_8447:bool) ->
              fun (r_xs__ys110_8447:bool) ->
                fun (r_xs__ys111_8447:int) ->
                  (if r_xs__ys010_8447 = false then
                     k_append_6214
                       (let
                          ys__xs__ys_1990 (iii00_3100:bool) (iii01_3100:int) (iii10_3100:bool) (iii11_3100:int) 
                                         (iii20_3100:bool) (iii21_3100:int) 
                                         (k_append_ys__xs__ys_6600:(bool ->
                                                                    bool ->
                                                                    int ->
                                                                    bool -> bool -> int -> bool -> bool -> int -> X)) =
                          if iii00_3100 = false then
                            if iii10_3100 = false then
                              if iii20_3100 = false then
                                k_append_ys__xs__ys_6600 false true 0 false true 0 false true 0
                              else
                                ys_1940 iii21_3100
                                  (fun (x0_9823:bool) ->
                                     fun (x1_9823:int) ->
                                       k_append_ys__xs__ys_6600 false true 0 false true 0 true x0_9823 x1_9823)
                            else
                              if iii20_3100 = false then
                                xs_1939 iii11_3100
                                  (fun (x0_9810:bool) ->
                                     fun (x1_9810:int) ->
                                       k_append_ys__xs__ys_6600 false true 0 true x0_9810 x1_9810 false true 0)
                              else
                                let
                                  r_xs__ys_5383 (k_append_ys__xs__ys_r_xs__ys_6752:(bool -> int -> bool -> int -> X)) =
                                  xs__ys_3318 iii11_3100 iii21_3100 k_append_ys__xs__ys_r_xs__ys_6752
                                in
                                r_xs__ys_5383
                                  (fun (r_xs__ys00_6790:bool) ->
                                     fun (r_xs__ys01_6790:int) ->
                                       fun (r_xs__ys10_6790:bool) ->
                                         fun (r_xs__ys11_6790:int) ->
                                           k_append_ys__xs__ys_6600 false true 0 true r_xs__ys00_6790 r_xs__ys01_6790
                                             true r_xs__ys10_6790 r_xs__ys11_6790)
                          else
                            if iii10_3100 = false then
                              if iii20_3100 = false then
                                ys_1940 iii01_3100
                                  (fun (x0_9767:bool) ->
                                     fun (x1_9767:int) ->
                                       k_append_ys__xs__ys_6600 true x0_9767 x1_9767 false true 0 false true 0)
                              else
                                let
                                  r_ys__ys_5299 (k_append_ys__xs__ys_r_ys__ys_6854:(bool -> int -> bool -> int -> X)) =
                                  ys__ys_3745 iii01_3100 iii21_3100 k_append_ys__xs__ys_r_ys__ys_6854
                                in
                                r_ys__ys_5299
                                  (fun (r_ys__ys00_6892:bool) ->
                                     fun (r_ys__ys01_6892:int) ->
                                       fun (r_ys__ys10_6892:bool) ->
                                         fun (r_ys__ys11_6892:int) ->
                                           k_append_ys__xs__ys_6600 true r_ys__ys00_6892 r_ys__ys01_6892 false true 0
                                             true r_ys__ys10_6892 r_ys__ys11_6892)
                            else
                              if iii20_3100 = false then
                                let
                                  r_ys__xs_5257 (k_append_ys__xs__ys_r_ys__xs_6904:(bool -> int -> bool -> int -> X)) =
                                  ys__xs_3700 iii01_3100 iii11_3100 k_append_ys__xs__ys_r_ys__xs_6904
                                in
                                r_ys__xs_5257
                                  (fun (r_ys__xs00_6942:bool) ->
                                     fun (r_ys__xs01_6942:int) ->
                                       fun (r_ys__xs10_6942:bool) ->
                                         fun (r_ys__xs11_6942:int) ->
                                           k_append_ys__xs__ys_6600 true r_ys__xs00_6942 r_ys__xs01_6942 true
                                             r_ys__xs10_6942 r_ys__xs11_6942 false true 0)
                              else
                                let
                                  r_ys__xs__ys_5225
                                                   (k_append_ys__xs__ys_r_ys__xs__ys_6951:(
                                                   bool -> int -> bool -> int -> bool -> int -> X)) =
                                  ys__xs__ys_3654 iii01_3100 iii11_3100 iii21_3100
                                    k_append_ys__xs__ys_r_ys__xs__ys_6951
                                in
                                r_ys__xs__ys_5225
                                  (fun (r_ys__xs__ys00_6983:bool) ->
                                     fun (r_ys__xs__ys01_6983:int) ->
                                       fun (r_ys__xs__ys10_6983:bool) ->
                                         fun (r_ys__xs__ys11_6983:int) ->
                                           fun (r_ys__xs__ys20_6983:bool) ->
                                             fun (r_ys__xs__ys21_6983:int) ->
                                               k_append_ys__xs__ys_6600 true r_ys__xs__ys00_6983 r_ys__xs__ys01_6983
                                                 true r_ys__xs__ys10_6983 r_ys__xs__ys11_6983 true r_ys__xs__ys20_6983
                                                 r_ys__xs__ys21_6983)
                        in
                        ys__xs__ys_1990)
                   else
                     if r_xs__ys010_8447 <> false then
                       let xs'_1014 (x_1269:int) (k_append_xs'_7005:(bool -> int -> X)) =
                         xs__ys_1023 true (x_1269 + 1) false 0
                           (fun (p00_9217:bool) ->
                              fun (p010_9217:bool) ->
                                fun (p011_9217:int) ->
                                  fun (p10_9217:bool) ->
                                    fun (p110_9217:bool) ->
                                      fun (p111_9217:int) -> k_append_xs'_7005 p010_9217 p011_9217)
                       in
                       let rec
                         xs'__ys_3363 (x_3337:int) (x_3338:int) 
                                     (k_append_xs'__ys_7049:(bool -> int -> bool -> int -> X)) =
                         let
                           r_xs__ys_6024
                                        (k_append_xs'__ys_r_xs__ys_7074:(
                                        bool -> bool -> int -> bool -> bool -> int -> X)) =
                           xs__ys_1023 true (x_3337 + 1) true x_3338 k_append_xs'__ys_r_xs__ys_7074
                         in
                         r_xs__ys_6024
                           (fun (r_xs__ys00_7086:bool) ->
                              fun (r_xs__ys010_7086:bool) ->
                                fun (r_xs__ys011_7086:int) ->
                                  fun (r_xs__ys10_7086:bool) ->
                                    fun (r_xs__ys110_7086:bool) ->
                                      fun (r_xs__ys111_7086:int) ->
                                        k_append_xs'__ys_7049 r_xs__ys010_7086 r_xs__ys011_7086 r_xs__ys110_7086
                                          r_xs__ys111_7086)
                       in
                       let
                         xs'__ys_1965 (ii00_2944:bool) (ii01_2944:int) (ii10_2944:bool) (ii11_2944:int) 
                                     (k_append_xs'__ys_7097:(bool -> bool -> int -> bool -> bool -> int -> X)) =
                         if ii00_2944 = false then
                           if ii10_2944 = false then
                             k_append_xs'__ys_7097 false true 0 false true 0
                           else
                             ys_1940 ii11_2944
                               (fun (x0_9246:bool) ->
                                  fun (x1_9246:int) -> k_append_xs'__ys_7097 false true 0 true x0_9246 x1_9246)
                         else
                           if ii10_2944 = false then
                             xs'_1014 ii01_2944
                               (fun (x0_9243:bool) ->
                                  fun (x1_9243:int) -> k_append_xs'__ys_7097 true x0_9243 x1_9243 false true 0)
                           else
                             let r_xs'__ys_4413 (k_append_xs'__ys_r_xs'__ys_7207:(bool -> int -> bool -> int -> X)) =
                               xs'__ys_3363 ii01_2944 ii11_2944 k_append_xs'__ys_r_xs'__ys_7207
                             in
                             r_xs'__ys_4413
                               (fun (r_xs'__ys00_7231:bool) ->
                                  fun (r_xs'__ys01_7231:int) ->
                                    fun (r_xs'__ys10_7231:bool) ->
                                      fun (r_xs'__ys11_7231:int) ->
                                        k_append_xs'__ys_7097 true r_xs'__ys00_7231 r_xs'__ys01_7231 true
                                          r_xs'__ys10_7231 r_xs'__ys11_7231)
                       in
                       let
                         r_append_4560
                                      (k_append_r_append_7352:((bool ->
                                                                  int ->
                                                                    bool ->
                                                                    int ->
                                                                    bool ->
                                                                    int ->
                                                                    (bool ->
                                                                    bool ->
                                                                    int ->
                                                                    bool -> bool -> int -> bool -> bool -> int -> X) ->
                                                                    X) -> X)) =
                         append_1165 xs'__ys_1965 k_append_r_append_7352
                       in
                       r_append_4560
                         (fun (r_append_8406:(bool ->
                                                int ->
                                                  bool ->
                                                    int ->
                                                      bool ->
                                                        int ->
                                                          (bool ->
                                                             bool ->
                                                               int -> bool -> bool -> int -> bool -> bool -> int -> X)
                                                            -> X)) ->
                            k_append_6214
                              (let rs'_1195 (i_1369:int) (k_append_rs'_7515:(bool -> int -> X)) =
                                 if i_1369 = 0 then
                                   k_append_rs'_7515 true r_xs__ys011_8447
                                 else
                                   r_append_8406 true (i_1369 - 1) false 0 false 0
                                     (fun (p00_9366:bool) ->
                                        fun (p010_9366:bool) ->
                                          fun (p011_9366:int) ->
                                            fun (p10_9366:bool) ->
                                              fun (p110_9366:bool) ->
                                                fun (p111_9366:int) ->
                                                  fun (p20_9366:bool) ->
                                                    fun (p210_9366:bool) ->
                                                      fun (p211_9366:int) -> k_append_rs'_7515 p010_9366 p011_9366)
                               in
                               let rec
                                 rs'__xs_3491 (x_3455:int) (x_3456:int) 
                                             (k_append_rs'__xs_7573:(
                                             bool -> int -> bool -> int -> X)) =
                                 if x_3455 = 0 then
                                   xs__ys_1023 true x_3456 false 0
                                     (fun (p00_9405:bool) ->
                                        fun (p010_9405:bool) ->
                                          fun (p011_9405:int) ->
                                            fun (p10_9405:bool) ->
                                              fun (p110_9405:bool) ->
                                                fun (p111_9405:int) ->
                                                  k_append_rs'__xs_7573 true r_xs__ys011_8447 p010_9405 p011_9405)
                                 else
                                   let
                                     r_r_append_5952
                                                    (k_append_rs'__xs_r_r_append_7652:(
                                                    bool ->
                                                      bool -> int -> bool -> bool -> int -> bool -> bool -> int -> X)) =
                                     r_append_8406 true (x_3455 - 1) false 0 false 0 k_append_rs'__xs_r_r_append_7652
                                   in
                                   r_r_append_5952
                                     (fun (r_r_append00_7698:bool) ->
                                        fun (r_r_append010_7698:bool) ->
                                          fun (r_r_append011_7698:int) ->
                                            fun (r_r_append10_7698:bool) ->
                                              fun (r_r_append110_7698:bool) ->
                                                fun (r_r_append111_7698:int) ->
                                                  fun (r_r_append20_7698:bool) ->
                                                    fun (r_r_append210_7698:bool) ->
                                                      fun (r_r_append211_7698:int) ->
                                                        xs__ys_1023 true x_3456 false 0
                                                          (fun (p00_9393:bool) ->
                                                             fun (p010_9393:bool) ->
                                                               fun (p011_9393:int) ->
                                                                 fun (p10_9393:bool) ->
                                                                   fun (p110_9393:bool) ->
                                                                    fun (p111_9393:int) ->
                                                                    k_append_rs'__xs_7573 r_r_append010_7698
                                                                    r_r_append011_7698 p010_9393 p011_9393))
                               in
                               let rec
                                 rs'__ys_3552 (x_3516:int) (x_3517:int) 
                                             (k_append_rs'__ys_7708:(
                                             bool -> int -> bool -> int -> X)) =
                                 if x_3516 = 0 then
                                   let
                                     r_xs__ys_5935
                                                  (k_append_rs'__ys_r_xs__ys_7733:(
                                                  bool -> bool -> int -> bool -> bool -> int -> X)) =
                                     xs__ys_1023 true 0 true x_3517 k_append_rs'__ys_r_xs__ys_7733
                                   in
                                   r_xs__ys_5935
                                     (fun (r_xs__ys00_7751:bool) ->
                                        fun (r_xs__ys010_7751:bool) ->
                                          fun (r_xs__ys011_7751:int) ->
                                            fun (r_xs__ys10_7751:bool) ->
                                              fun (r_xs__ys110_7751:bool) ->
                                                fun (r_xs__ys111_7751:int) ->
                                                  k_append_rs'__ys_7708 true r_xs__ys011_7751 r_xs__ys110_7751
                                                    r_xs__ys111_7751)
                                 else
                                   let
                                     r_r_append_5924
                                                    (k_append_rs'__ys_r_r_append_7784:(
                                                    bool ->
                                                      bool -> int -> bool -> bool -> int -> bool -> bool -> int -> X)) =
                                     r_append_8406 true (x_3516 - 1) false 0 false 0 k_append_rs'__ys_r_r_append_7784
                                   in
                                   r_r_append_5924
                                     (fun (r_r_append00_7830:bool) ->
                                        fun (r_r_append010_7830:bool) ->
                                          fun (r_r_append011_7830:int) ->
                                            fun (r_r_append10_7830:bool) ->
                                              fun (r_r_append110_7830:bool) ->
                                                fun (r_r_append111_7830:int) ->
                                                  fun (r_r_append20_7830:bool) ->
                                                    fun (r_r_append210_7830:bool) ->
                                                      fun (r_r_append211_7830:int) ->
                                                        xs__ys_1023 false 0 true x_3517
                                                          (fun (p00_9438:bool) ->
                                                             fun (p010_9438:bool) ->
                                                               fun (p011_9438:int) ->
                                                                 fun (p10_9438:bool) ->
                                                                   fun (p110_9438:bool) ->
                                                                    fun (p111_9438:int) ->
                                                                    k_append_rs'__ys_7708 r_r_append010_7830
                                                                    r_r_append011_7830 p110_9438 p111_9438))
                               in
                               let rec
                                 rs'__xs__ys_3426 (x_3377:int) (x_3378:int) (x_3379:int) 
                                                 (k_append_rs'__xs__ys_7841:(
                                                 bool -> int -> bool -> int -> bool -> int -> X)) =
                                 if x_3377 = 0 then
                                   let
                                     r_xs__ys_4859
                                                  (k_append_rs'__xs__ys_r_xs__ys_7866:(
                                                  bool -> bool -> int -> bool -> bool -> int -> X)) =
                                     xs__ys_1023 true x_3378 false 0 k_append_rs'__xs__ys_r_xs__ys_7866
                                   in
                                   r_xs__ys_4859
                                     (fun (r_xs__ys00_7917:bool) ->
                                        fun (r_xs__ys010_7917:bool) ->
                                          fun (r_xs__ys011_7917:int) ->
                                            fun (r_xs__ys10_7917:bool) ->
                                              fun (r_xs__ys110_7917:bool) ->
                                                fun (r_xs__ys111_7917:int) ->
                                                  (let
                                                     r_xs__ys_5907
                                                                  (k_append_rs'__xs__ys_r_xs__ys_7896:(
                                                                  bool -> 
                                                                    bool -> int -> bool -> bool -> int -> X)) =
                                                     xs__ys_1023 true 0 true x_3379 k_append_rs'__xs__ys_r_xs__ys_7896
                                                   in
                                                   r_xs__ys_5907
                                                     (fun (r_xs__ys00_7916:bool) ->
                                                        fun (r_xs__ys010_7916:bool) ->
                                                          fun (r_xs__ys011_7916:int) ->
                                                            fun (r_xs__ys10_7916:bool) ->
                                                              fun (r_xs__ys110_7916:bool) ->
                                                                fun (r_xs__ys111_7916:int) ->
                                                                  k_append_rs'__xs__ys_7841 true r_xs__ys011_7916
                                                                    r_xs__ys010_7917 r_xs__ys011_7917 r_xs__ys110_7916
                                                                    r_xs__ys111_7916)))
                                 else
                                   let
                                     r_r_append_5895
                                                    (k_append_rs'__xs__ys_r_r_append_7950:(
                                                    bool ->
                                                      bool -> int -> bool -> bool -> int -> bool -> bool -> int -> X)) =
                                     r_append_8406 true (x_3377 - 1) false 0 false 0
                                       k_append_rs'__xs__ys_r_r_append_7950
                                   in
                                   r_r_append_5895
                                     (fun (r_r_append00_7995:bool) ->
                                        fun (r_r_append010_7995:bool) ->
                                          fun (r_r_append011_7995:int) ->
                                            fun (r_r_append10_7995:bool) ->
                                              fun (r_r_append110_7995:bool) ->
                                                fun (r_r_append111_7995:int) ->
                                                  fun (r_r_append20_7995:bool) ->
                                                    fun (r_r_append210_7995:bool) ->
                                                      fun (r_r_append211_7995:int) ->
                                                        (let
                                                           r_xs__ys_5886
                                                            (k_append_rs'__xs__ys_r_xs__ys_7980:(
                                                           bool -> bool -> int -> bool -> bool -> int -> X)) =
                                                           xs__ys_1023 true x_3378 true x_3379
                                                             k_append_rs'__xs__ys_r_xs__ys_7980
                                                         in
                                                         r_xs__ys_5886
                                                           (fun (r_xs__ys00_7994:bool) ->
                                                              fun (r_xs__ys010_7994:bool) ->
                                                                fun (r_xs__ys011_7994:int) ->
                                                                  fun (r_xs__ys10_7994:bool) ->
                                                                    fun (r_xs__ys110_7994:bool) ->
                                                                    fun (r_xs__ys111_7994:int) ->
                                                                    k_append_rs'__xs__ys_7841 r_r_append010_7995
                                                                    r_r_append011_7995 r_xs__ys010_7994
                                                                    r_xs__ys011_7994 r_xs__ys110_7994 r_xs__ys111_7994)))
                               in
                               let
                                 rs'__xs__ys_1986 (iii00_2831:bool) (iii01_2831:int) (iii10_2831:bool) (iii11_2831:int) 
                                                 (iii20_2831:bool) (iii21_2831:int) 
                                                 (k_append_rs'__xs__ys_8004:(
                                                 bool -> bool -> int -> bool -> bool -> int -> bool -> bool -> int -> X)) =
                                 if iii00_2831 = false then
                                   if iii10_2831 = false then
                                     if iii20_2831 = false then
                                       k_append_rs'__xs__ys_8004 false true 0 false true 0 false true 0
                                     else
                                       ys_1940 iii21_2831
                                         (fun (x0_9628:bool) ->
                                            fun (x1_9628:int) ->
                                              k_append_rs'__xs__ys_8004 false true 0 false true 0 true x0_9628 x1_9628)
                                   else
                                     if iii20_2831 = false then
                                       xs_1939 iii11_2831
                                         (fun (x0_9615:bool) ->
                                            fun (x1_9615:int) ->
                                              k_append_rs'__xs__ys_8004 false true 0 true x0_9615 x1_9615 false true 0)
                                     else
                                       let
                                         r_xs__ys_5049
                                                      (k_append_rs'__xs__ys_r_xs__ys_8156:(
                                                      bool -> int -> bool -> int -> X)) =
                                         xs__ys_3318 iii11_2831 iii21_2831 k_append_rs'__xs__ys_r_xs__ys_8156
                                       in
                                       r_xs__ys_5049
                                         (fun (r_xs__ys00_8194:bool) ->
                                            fun (r_xs__ys01_8194:int) ->
                                              fun (r_xs__ys10_8194:bool) ->
                                                fun (r_xs__ys11_8194:int) ->
                                                  k_append_rs'__xs__ys_8004 false true 0 true r_xs__ys00_8194
                                                    r_xs__ys01_8194 true r_xs__ys10_8194 r_xs__ys11_8194)
                                 else
                                   if iii10_2831 = false then
                                     if iii20_2831 = false then
                                       rs'_1195 iii01_2831
                                         (fun (x0_9572:bool) ->
                                            fun (x1_9572:int) ->
                                              k_append_rs'__xs__ys_8004 true x0_9572 x1_9572 false true 0 false true 0)
                                     else
                                       let
                                         r_rs'__ys_4965
                                                       (k_append_rs'__xs__ys_r_rs'__ys_8258:(
                                                       bool -> int -> bool -> int -> X)) =
                                         rs'__ys_3552 iii01_2831 iii21_2831 k_append_rs'__xs__ys_r_rs'__ys_8258
                                       in
                                       r_rs'__ys_4965
                                         (fun (r_rs'__ys00_8296:bool) ->
                                            fun (r_rs'__ys01_8296:int) ->
                                              fun (r_rs'__ys10_8296:bool) ->
                                                fun (r_rs'__ys11_8296:int) ->
                                                  k_append_rs'__xs__ys_8004 true r_rs'__ys00_8296 r_rs'__ys01_8296
                                                    false true 0 true r_rs'__ys10_8296 r_rs'__ys11_8296)
                                   else
                                     if iii20_2831 = false then
                                       let
                                         r_rs'__xs_4923
                                                       (k_append_rs'__xs__ys_r_rs'__xs_8308:(
                                                       bool -> int -> bool -> int -> X)) =
                                         rs'__xs_3491 iii01_2831 iii11_2831 k_append_rs'__xs__ys_r_rs'__xs_8308
                                       in
                                       r_rs'__xs_4923
                                         (fun (r_rs'__xs00_8346:bool) ->
                                            fun (r_rs'__xs01_8346:int) ->
                                              fun (r_rs'__xs10_8346:bool) ->
                                                fun (r_rs'__xs11_8346:int) ->
                                                  k_append_rs'__xs__ys_8004 true r_rs'__xs00_8346 r_rs'__xs01_8346 true
                                                    r_rs'__xs10_8346 r_rs'__xs11_8346 false true 0)
                                     else
                                       let
                                         r_rs'__xs__ys_4891
                                                           (k_append_rs'__xs__ys_r_rs'__xs__ys_8355:(
                                                           bool -> int -> bool -> int -> bool -> int -> X)) =
                                         rs'__xs__ys_3426 iii01_2831 iii11_2831 iii21_2831
                                           k_append_rs'__xs__ys_r_rs'__xs__ys_8355
                                       in
                                       r_rs'__xs__ys_4891
                                         (fun (r_rs'__xs__ys00_8387:bool) ->
                                            fun (r_rs'__xs__ys01_8387:int) ->
                                              fun (r_rs'__xs__ys10_8387:bool) ->
                                                fun (r_rs'__xs__ys11_8387:int) ->
                                                  fun (r_rs'__xs__ys20_8387:bool) ->
                                                    fun (r_rs'__xs__ys21_8387:int) ->
                                                      k_append_rs'__xs__ys_8004 true r_rs'__xs__ys00_8387
                                                        r_rs'__xs__ys01_8387 true r_rs'__xs__ys10_8387
                                                        r_rs'__xs__ys11_8387 true r_rs'__xs__ys20_8387
                                                        r_rs'__xs__ys21_8387)
                               in
                               rs'__xs__ys_1986))
                     else
                       _|_))
 in
 let main_1017 (i_1018:int) (n_1019:int) (k_main_8489:(unit -> X)) =
   let r_make_list_5550 (k_main_r_make_list_8502:((int -> (bool -> int -> X) -> X) -> X)) =
     make_list_1008 n_1019 k_main_r_make_list_8502
   in
   r_make_list_5550
     (fun (r_make_list_9048:(int -> (bool -> int -> X) -> X)) ->
        (let f_1732 (x_1560:int) (k_main_f_8517:(bool -> int -> X)) = k_main_f_8517 false 0 in
         let
           r_make_list__f_1999 (ix00_2285:bool) (ix01_2285:int) (ix10_2285:bool) (ix11_2285:int) 
                              (k_main_r_make_list__f_8530:(bool -> bool -> int -> bool -> bool -> int -> X)) =
           if ix00_2285 = false then
             if ix10_2285 = false then
               k_main_r_make_list__f_8530 false true 0 false true 0
             else
               f_1732 ix11_2285
                 (fun (x0_9904:bool) ->
                    fun (x1_9904:int) -> k_main_r_make_list__f_8530 false true 0 true x0_9904 x1_9904)
           else
             if ix10_2285 = false then
               r_make_list_9048 ix01_2285
                 (fun (x0_9901:bool) ->
                    fun (x1_9901:int) -> k_main_r_make_list__f_8530 true x0_9901 x1_9901 false true 0)
             else
               let r_r_make_list_5562 (k_main_r_make_list__f_r_r_make_list_8639:(bool -> int -> X)) =
                 r_make_list_9048 ix01_2285 k_main_r_make_list__f_r_r_make_list_8639
               in
               r_r_make_list_5562
                 (fun (r_r_make_list0_8673:bool) ->
                    fun (r_r_make_list1_8673:int) ->
                      f_1732 ix11_2285
                        (fun (x0_9883:bool) ->
                           fun (x1_9883:int) ->
                             k_main_r_make_list__f_8530 true r_r_make_list0_8673 r_r_make_list1_8673 true x0_9883
                               x1_9883))
         in
         let
           r_append_5708
                        (k_main_r_append_8785:((bool ->
                                                  int ->
                                                    bool ->
                                                      int ->
                                                        bool ->
                                                          int ->
                                                            (bool ->
                                                               bool ->
                                                                 int -> bool -> bool -> int -> bool -> bool -> int -> X)
                                                              -> X) -> X)) =
           append_1165 r_make_list__f_1999 k_main_r_append_8785
         in
         r_append_5708
           (fun (r_append_9028:(bool ->
                                  int ->
                                    bool ->
                                      int ->
                                        bool ->
                                          int ->
                                            (bool -> bool -> int -> bool -> bool -> int -> bool -> bool -> int -> X) ->
                                              X)) ->
              (let
                 r_r_append_5826
                                (k_main_r_r_append_8982:(bool ->
                                                           bool ->
                                                             int -> bool -> bool -> int -> bool -> bool -> int -> X)) =
                 r_append_9028 true i_1018 false 0 false 0 k_main_r_r_append_8982
               in
               r_r_append_5826
                 (fun (r_r_append00_9012:bool) ->
                    fun (r_r_append010_9012:bool) ->
                      fun (r_r_append011_9012:int) ->
                        fun (r_r_append10_9012:bool) ->
                          fun (r_r_append110_9012:bool) ->
                            fun (r_r_append111_9012:int) ->
                              fun (r_r_append20_9012:bool) ->
                                fun (r_r_append210_9012:bool) ->
                                  fun (r_r_append211_9012:int) ->
                                    (let r_r_make_list_5809 (k_main_r_r_make_list_8994:(bool -> int -> X)) =
                                       r_make_list_9048 i_1018 k_main_r_r_make_list_8994
                                     in
                                     r_r_make_list_5809
                                       (fun (r_r_make_list0_9011:bool) ->
                                          fun (r_r_make_list1_9011:int) ->
                                            (if r_r_append011_9012 = r_r_make_list1_9011 then
                                               k_main_8489 ()
                                             else
                                               {|fail|} () k_main_8489))))))))
 in
 let r_f_5821 (k_r_f_9059:(int -> X)) = rand_int_cps () k_r_f_9059 in
 r_f_5821
   (fun (r_f_9104:int) ->
      (let r_f_5823 (k_r_f_9071:(int -> X)) = rand_int_cps () k_r_f_9071 in
       r_f_5823
         (fun (r_f_9103:int) ->
            (let r_r_main_5825 (k_r_r_main_9092:(unit -> X)) = main_1017 r_f_9104 r_f_9103 k_r_r_main_9092 in
             r_r_main_5825 (fun (r_r_main_9098:unit) -> {end})))))

spec (abstraction type environment for CEGAR-loop):
 append_1165: ((bool ->
                  int ->
                    b1_1055:bool ->
                      int ->
                        (bool ->
                           bool ->
                             int ->
                               b2_1061:bool[\b2_1060. not b1_1055 || b2_1060] ->
                                 b3_1062:bool -> int[\x_1063. not b2_1061 || not b3_1062 && x_1063 = 0] -> X) -> X) ->
                 ((b1_1078:bool ->
                     i_1079:int ->
                       b2_1080:bool ->
                         j_1081:int ->
                           b3_1082:bool ->
                             int ->
                               (b41_1085:bool[\b41_1084. not b1_1078 || b41_1084] ->
                                  bool ->
                                    x_1087:int ->
                                      b51_1089:bool[\b51_1088. not b2_1080 || b51_1088] ->
                                        bool ->
                                          int[\y_1091. not ((b41_1085 && b51_1089) && i_1079 = j_1081) ||
                                                       x_1087 = y_1091] ->
                                            b61_1094:bool[\b61_1093. not b3_1082 || b61_1093] ->
                                              b62_1095:bool ->
                                                int[\z_1096. not b61_1094 || not b62_1095 && z_1096 = 0] -> X) -> X) ->
                    X) -> X)
 ys_1940: ((bool ->
              int ->
                b2_1123:bool ->
                  int ->
                    (bool ->
                       bool ->
                         int ->
                           b41_1129:bool[\b41_1128. not b2_1123 || b41_1128] ->
                             b42_1130:bool -> int[\x_1131. not b41_1129 || not b42_1130 && x_1131 = 0] -> X) -> X) ->
             int -> (b3_1147:bool -> int[\x_1148. not b3_1147 && x_1148 = 0] -> X) -> X)
 
spec (abstraction type environment for CEGAR-loop):
 append_1165: ((bool ->
                  int ->
                    b1_1055:bool ->
                      int ->
                        (bool ->
                           bool ->
                             int ->
                               b2_1061:bool[\b2_1060. not b1_1055 || b2_1060] ->
                                 b3_1062:bool -> int[\x_1063. not b2_1061 || not b3_1062 && x_1063 = 0] -> X) -> X) ->
                 ((b1_1078:bool ->
                     i_1079:int ->
                       b2_1080:bool ->
                         j_1081:int ->
                           b3_1082:bool ->
                             int ->
                               (b41_1085:bool[\b41_1084. not b1_1078 || b41_1084] ->
                                  bool ->
                                    x_1087:int ->
                                      b51_1089:bool[\b51_1088. not b2_1080 || b51_1088] ->
                                        bool ->
                                          int[\y_1091. not ((b41_1085 && b51_1089) && i_1079 = j_1081) ||
                                                       x_1087 = y_1091] ->
                                            b61_1094:bool[\b61_1093. not b3_1082 || b61_1093] ->
                                              b62_1095:bool ->
                                                int[\z_1096. not b61_1094 || not b62_1095 && z_1096 = 0] -> X) -> X) ->
                    X) -> X)
 ys_1940: ((bool ->
              int ->
                b2_1123:bool ->
                  int ->
                    (bool ->
                       bool ->
                         int ->
                           b41_1129:bool[\b41_1128. not b2_1123 || b41_1128] ->
                             b42_1130:bool -> int[\x_1131. not b41_1129 || not b42_1130 && x_1131 = 0] -> X) -> X) ->
             int -> (b3_1147:bool -> int[\x_1148. not b3_1147 && x_1148 = 0] -> X) -> X)
 
Program with abstraction types (CEGAR-cycle 0)::
Main: main_10031
  main_10031 -> (r_f_5821 f_10084);;
  append_1165 xs__ys_1023 k_append_6214 -> (r_xs__ys_6041 xs__ys_1023 (f_append_10044 k_append_6214 xs__ys_1023));;
  br_f_append_10115 b_10116 k_append_6214 xs__ys_1023 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 
  r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 when b_10116 ->
      (r_append_4560 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447
        r_xs__ys111_8447 xs__ys_1023
        (f_append_10057 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447
          r_xs__ys111_8447 k_append_6214 xs__ys_1023));;
  br_f_append_10115 b_10116 k_append_6214 xs__ys_1023 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 
  r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 when (not b_10116) -> _|_;;
  br_r_make_list__f_10117 b_10118 i_1018 n_1019 r_make_list_9048 ix00_2285 ix01_2285 ix10_2285 ix11_2285 
  k_main_r_make_list__f_8530 when b_10118 -> (k_main_r_make_list__f_8530 false true 0 false true 0);;
  br_r_make_list__f_10117 b_10118 i_1018 n_1019 r_make_list_9048 ix00_2285 ix01_2285 ix10_2285 ix11_2285 
  k_main_r_make_list__f_8530 when (not b_10118) ->
      (f_1732 i_1018 n_1019 ix11_2285
        (f_r_make_list__f_10077 i_1018 ix00_2285 ix01_2285 ix10_2285 ix11_2285 n_1019 k_main_r_make_list__f_8530));;
  br_r_make_list__f_10119 b_10120 i_1018 n_1019 r_make_list_9048 ix00_2285 ix01_2285 ix10_2285 ix11_2285 
  k_main_r_make_list__f_8530 when b_10120 ->
      (r_make_list_9048 ix01_2285
        (f_r_make_list__f_10078 i_1018 ix00_2285 ix01_2285 ix10_2285 ix11_2285 n_1019 k_main_r_make_list__f_8530));;
  br_r_make_list__f_10119 b_10120 i_1018 n_1019 r_make_list_9048 ix00_2285 ix01_2285 ix10_2285 ix11_2285 
  k_main_r_make_list__f_8530 when (not b_10120) ->
      (r_r_make_list_5562 i_1018 ix00_2285 ix01_2285 ix10_2285 ix11_2285 n_1019 r_make_list_9048
        (f_r_make_list__f_10079 i_1018 ix00_2285 ix01_2285 ix10_2285 ix11_2285 n_1019 k_main_r_make_list__f_8530));;
  br_rs'__xs__ys_10103 b_10104 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 r_append_8406 xs__ys_1023 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 
  k_append_rs'__xs__ys_8004 when b_10104 -> (k_append_rs'__xs__ys_8004 false true 0 false true 0 false true 0);;
  br_rs'__xs__ys_10103 b_10104 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 r_append_8406 xs__ys_1023 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 
  k_append_rs'__xs__ys_8004 when (not b_10104) ->
      (ys_1940 xs__ys_1023 iii21_2831
        (f_rs'__xs__ys_10069 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 r_xs__ys00_8447
          r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_rs'__xs__ys_8004));;
  br_rs'__xs__ys_10105 b_10106 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 r_append_8406 xs__ys_1023 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 
  k_append_rs'__xs__ys_8004 when b_10106 ->
      (xs_1939 xs__ys_1023 iii11_2831
        (f_rs'__xs__ys_10070 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 r_xs__ys00_8447
          r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_rs'__xs__ys_8004));;
  br_rs'__xs__ys_10105 b_10106 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 r_append_8406 xs__ys_1023 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 
  k_append_rs'__xs__ys_8004 when (not b_10106) ->
      (r_xs__ys_5049 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 r_xs__ys00_8447 r_xs__ys010_8447
        r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 xs__ys_1023
        (f_rs'__xs__ys_10071 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 r_xs__ys00_8447
          r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_rs'__xs__ys_8004));;
  br_rs'__xs__ys_10107 b_10108 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 r_append_8406 xs__ys_1023 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 
  k_append_rs'__xs__ys_8004 when b_10108 ->
      (br_rs'__xs__ys_10103 (iii20_2831 <=> false) r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447
        r_xs__ys110_8447 r_xs__ys111_8447 r_append_8406 xs__ys_1023 iii00_2831 iii01_2831 iii10_2831 iii11_2831
        iii20_2831 iii21_2831 k_append_rs'__xs__ys_8004);;
  br_rs'__xs__ys_10107 b_10108 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 r_append_8406 xs__ys_1023 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 
  k_append_rs'__xs__ys_8004 when (not b_10108) ->
      (br_rs'__xs__ys_10105 (iii20_2831 <=> false) r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447
        r_xs__ys110_8447 r_xs__ys111_8447 r_append_8406 xs__ys_1023 iii00_2831 iii01_2831 iii10_2831 iii11_2831
        iii20_2831 iii21_2831 k_append_rs'__xs__ys_8004);;
  br_rs'__xs__ys_10109 b_10110 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 r_append_8406 xs__ys_1023 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 
  k_append_rs'__xs__ys_8004 when b_10110 ->
      (rs'_1195 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447
        r_append_8406 iii01_2831
        (f_rs'__xs__ys_10072 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 r_xs__ys00_8447
          r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_rs'__xs__ys_8004));;
  br_rs'__xs__ys_10109 b_10110 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 r_append_8406 xs__ys_1023 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 
  k_append_rs'__xs__ys_8004 when (not b_10110) ->
      (r_rs'__ys_4965 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 r_xs__ys00_8447
        r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 r_append_8406 xs__ys_1023
        (f_rs'__xs__ys_10073 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 r_xs__ys00_8447
          r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_rs'__xs__ys_8004));;
  br_rs'__xs__ys_10111 b_10112 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 r_append_8406 xs__ys_1023 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 
  k_append_rs'__xs__ys_8004 when b_10112 ->
      (r_rs'__xs_4923 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 r_xs__ys00_8447
        r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 r_append_8406 xs__ys_1023
        (f_rs'__xs__ys_10074 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 r_xs__ys00_8447
          r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_rs'__xs__ys_8004));;
  br_rs'__xs__ys_10111 b_10112 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 r_append_8406 xs__ys_1023 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 
  k_append_rs'__xs__ys_8004 when (not b_10112) ->
      (r_rs'__xs__ys_4891 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 r_xs__ys00_8447
        r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 r_append_8406 xs__ys_1023
        (f_rs'__xs__ys_10075 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 r_xs__ys00_8447
          r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_rs'__xs__ys_8004));;
  br_rs'__xs__ys_10113 b_10114 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 r_append_8406 xs__ys_1023 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 
  k_append_rs'__xs__ys_8004 when b_10114 ->
      (br_rs'__xs__ys_10109 (iii20_2831 <=> false) r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447
        r_xs__ys110_8447 r_xs__ys111_8447 r_append_8406 xs__ys_1023 iii00_2831 iii01_2831 iii10_2831 iii11_2831
        iii20_2831 iii21_2831 k_append_rs'__xs__ys_8004);;
  br_rs'__xs__ys_10113 b_10114 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 r_append_8406 xs__ys_1023 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 
  k_append_rs'__xs__ys_8004 when (not b_10114) ->
      (br_rs'__xs__ys_10111 (iii20_2831 <=> false) r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447
        r_xs__ys110_8447 r_xs__ys111_8447 r_append_8406 xs__ys_1023 iii00_2831 iii01_2831 iii10_2831 iii11_2831
        iii20_2831 iii21_2831 k_append_rs'__xs__ys_8004);;
  br_xs'__ys_10099 b_10100 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 xs__ys_1023 ii00_2944 ii01_2944 ii10_2944 ii11_2944 k_append_xs'__ys_7097 when b_10100 ->
      (k_append_xs'__ys_7097 false true 0 false true 0);;
  br_xs'__ys_10099 b_10100 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 xs__ys_1023 ii00_2944 ii01_2944 ii10_2944 ii11_2944 k_append_xs'__ys_7097 when (
      not b_10100) ->
      (ys_1940 xs__ys_1023 ii11_2944
        (f_xs'__ys_10054 ii00_2944 ii01_2944 ii10_2944 ii11_2944 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447
          r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_xs'__ys_7097));;
  br_xs'__ys_10101 b_10102 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 xs__ys_1023 ii00_2944 ii01_2944 ii10_2944 ii11_2944 k_append_xs'__ys_7097 when b_10102 ->
      (xs'_1014 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447
        xs__ys_1023 ii01_2944
        (f_xs'__ys_10055 ii00_2944 ii01_2944 ii10_2944 ii11_2944 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447
          r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_xs'__ys_7097));;
  br_xs'__ys_10101 b_10102 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 xs__ys_1023 ii00_2944 ii01_2944 ii10_2944 ii11_2944 k_append_xs'__ys_7097 when (
      not b_10102) ->
      (r_xs'__ys_4413 ii00_2944 ii01_2944 ii10_2944 ii11_2944 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447
        r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 xs__ys_1023
        (f_xs'__ys_10056 ii00_2944 ii01_2944 ii10_2944 ii11_2944 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447
          r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_xs'__ys_7097));;
  br_ys__xs__ys_10087 b_10088 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 xs__ys_1023 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 
  k_append_ys__xs__ys_6600 when b_10088 -> (k_append_ys__xs__ys_6600 false true 0 false true 0 false true 0);;
  br_ys__xs__ys_10087 b_10088 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 xs__ys_1023 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 
  k_append_ys__xs__ys_6600 when (not b_10088) ->
      (ys_1940 xs__ys_1023 iii21_3100
        (f_ys__xs__ys_10045 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 r_xs__ys00_8447
          r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_ys__xs__ys_6600));;
  br_ys__xs__ys_10089 b_10090 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 xs__ys_1023 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 
  k_append_ys__xs__ys_6600 when b_10090 ->
      (xs_1939 xs__ys_1023 iii11_3100
        (f_ys__xs__ys_10046 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 r_xs__ys00_8447
          r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_ys__xs__ys_6600));;
  br_ys__xs__ys_10089 b_10090 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 xs__ys_1023 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 
  k_append_ys__xs__ys_6600 when (not b_10090) ->
      (r_xs__ys_5383 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 r_xs__ys00_8447 r_xs__ys010_8447
        r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 xs__ys_1023
        (f_ys__xs__ys_10047 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 r_xs__ys00_8447
          r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_ys__xs__ys_6600));;
  br_ys__xs__ys_10091 b_10092 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 xs__ys_1023 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 
  k_append_ys__xs__ys_6600 when b_10092 ->
      (br_ys__xs__ys_10087 (iii20_3100 <=> false) r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447
        r_xs__ys110_8447 r_xs__ys111_8447 xs__ys_1023 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100
        k_append_ys__xs__ys_6600);;
  br_ys__xs__ys_10091 b_10092 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 xs__ys_1023 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 
  k_append_ys__xs__ys_6600 when (not b_10092) ->
      (br_ys__xs__ys_10089 (iii20_3100 <=> false) r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447
        r_xs__ys110_8447 r_xs__ys111_8447 xs__ys_1023 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100
        k_append_ys__xs__ys_6600);;
  br_ys__xs__ys_10093 b_10094 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 xs__ys_1023 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 
  k_append_ys__xs__ys_6600 when b_10094 ->
      (ys_1940 xs__ys_1023 iii01_3100
        (f_ys__xs__ys_10048 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 r_xs__ys00_8447
          r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_ys__xs__ys_6600));;
  br_ys__xs__ys_10093 b_10094 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 xs__ys_1023 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 
  k_append_ys__xs__ys_6600 when (not b_10094) ->
      (r_ys__ys_5299 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 r_xs__ys00_8447 r_xs__ys010_8447
        r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 xs__ys_1023
        (f_ys__xs__ys_10049 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 r_xs__ys00_8447
          r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_ys__xs__ys_6600));;
  br_ys__xs__ys_10095 b_10096 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 xs__ys_1023 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 
  k_append_ys__xs__ys_6600 when b_10096 ->
      (r_ys__xs_5257 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 r_xs__ys00_8447 r_xs__ys010_8447
        r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 xs__ys_1023
        (f_ys__xs__ys_10050 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 r_xs__ys00_8447
          r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_ys__xs__ys_6600));;
  br_ys__xs__ys_10095 b_10096 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 xs__ys_1023 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 
  k_append_ys__xs__ys_6600 when (not b_10096) ->
      (r_ys__xs__ys_5225 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 r_xs__ys00_8447
        r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 xs__ys_1023
        (f_ys__xs__ys_10051 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 r_xs__ys00_8447
          r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_ys__xs__ys_6600));;
  br_ys__xs__ys_10097 b_10098 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 xs__ys_1023 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 
  k_append_ys__xs__ys_6600 when b_10098 ->
      (br_ys__xs__ys_10093 (iii20_3100 <=> false) r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447
        r_xs__ys110_8447 r_xs__ys111_8447 xs__ys_1023 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100
        k_append_ys__xs__ys_6600);;
  br_ys__xs__ys_10097 b_10098 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 xs__ys_1023 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 
  k_append_ys__xs__ys_6600 when (not b_10098) ->
      (br_ys__xs__ys_10095 (iii20_3100 <=> false) r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447
        r_xs__ys110_8447 r_xs__ys111_8447 xs__ys_1023 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100
        k_append_ys__xs__ys_6600);;
  f_10084 r_f_9104 -> (r_f_5823 r_f_9104 (f_10085 r_f_9104));;
  f_10085 r_f_9104 r_f_9103 -> (r_r_main_5825 r_f_9103 r_f_9104 (f_10086 r_f_9103 r_f_9104));;
  f_10086 r_f_9103 r_f_9104 r_r_main_9098 -> end;;
  f_1732 i_1018 n_1019 x_1560 k_main_f_8517 -> (k_main_f_8517 false 0);;
  f_append_10044 k_append_6214 xs__ys_1023 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 
  r_xs__ys110_8447 r_xs__ys111_8447 when (r_xs__ys010_8447 <=> false) ->
      (k_append_6214
        (ys__xs__ys_1990 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447
          r_xs__ys111_8447 xs__ys_1023));;
  f_append_10044 k_append_6214 xs__ys_1023 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 
  r_xs__ys110_8447 r_xs__ys111_8447 when (not (r_xs__ys010_8447 <=> false)) ->
      (br_f_append_10115 (not (r_xs__ys010_8447 <=> false)) k_append_6214 xs__ys_1023 r_xs__ys00_8447 r_xs__ys010_8447
        r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447);;
  f_append_10057 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  k_append_6214 xs__ys_1023 r_append_8406 ->
      (k_append_6214
        (rs'__xs__ys_1986 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447
          r_xs__ys111_8447 r_append_8406 xs__ys_1023));;
  f_main_10076 i_1018 n_1019 k_main_8489 r_make_list_9048 ->
      (r_append_5708 i_1018 n_1019 r_make_list_9048 (f_main_10081 i_1018 n_1019 k_main_8489 r_make_list_9048));;
  f_main_10081 i_1018 n_1019 k_main_8489 r_make_list_9048 r_append_9028 ->
      (r_r_append_5826 i_1018 n_1019 r_append_9028 (f_main_10082 i_1018 n_1019 k_main_8489 r_make_list_9048));;
  f_main_10082 i_1018 n_1019 k_main_8489 r_make_list_9048 r_r_append00_9012 r_r_append010_9012 r_r_append011_9012 
  r_r_append10_9012 r_r_append110_9012 r_r_append111_9012 r_r_append20_9012 r_r_append210_9012 r_r_append211_9012 ->
      (r_r_make_list_5809 i_1018 n_1019 r_r_append00_9012 r_r_append010_9012 r_r_append011_9012 r_r_append10_9012
        r_r_append110_9012 r_r_append111_9012 r_r_append20_9012 r_r_append210_9012 r_r_append211_9012 r_make_list_9048
        (f_main_10083 i_1018 n_1019 r_r_append00_9012 r_r_append010_9012 r_r_append011_9012 r_r_append10_9012
          r_r_append110_9012 r_r_append111_9012 r_r_append20_9012 r_r_append210_9012 r_r_append211_9012 k_main_8489));;
  f_main_10083 i_1018 n_1019 r_r_append00_9012 r_r_append010_9012 r_r_append011_9012 r_r_append10_9012 
  r_r_append110_9012 r_r_append111_9012 r_r_append20_9012 r_r_append210_9012 r_r_append211_9012 k_main_8489 
  r_r_make_list0_9011 r_r_make_list1_9011 when (r_r_append011_9012 = r_r_make_list1_9011) -> (
      k_main_8489 ());;
  f_main_10083 i_1018 n_1019 r_r_append00_9012 r_r_append010_9012 r_r_append011_9012 r_r_append10_9012 
  r_r_append110_9012 r_r_append111_9012 r_r_append20_9012 r_r_append210_9012 r_r_append211_9012 k_main_8489 
  r_r_make_list0_9011 r_r_make_list1_9011 when (not (r_r_append011_9012 = r_r_make_list1_9011)) ->
      (fail_10215 true k_main_8489);;
  f_make_list_10032 n_1009 x_1236 k_make_list_6116 -> (k_make_list_6116 false 0);;
  f_make_list_10033 n_1009 k_make_list_6114 r_f_6191 ->
      (r_make_list_3808 n_1009 r_f_6191 (f_make_list_10034 n_1009 r_f_6191 k_make_list_6114));;
  f_make_list_10034 n_1009 r_f_6191 k_make_list_6114 r_make_list_6190 ->
      (k_make_list_6114 (f_make_list_10035 n_1009 r_f_6191 r_make_list_6190));;
  f_make_list_10035 n_1009 r_f_6191 r_make_list_6190 i_1226 k_make_list_6166 when (
      i_1226 = 0) -> (k_make_list_6166 true r_f_6191);;
  f_make_list_10035 n_1009 r_f_6191 r_make_list_6190 i_1226 k_make_list_6166 when (
      not (i_1226 = 0)) -> (r_make_list_6190 (i_1226 - 1) k_make_list_6166);;
  f_r_make_list__f_10077 i_1018 ix00_2285 ix01_2285 ix10_2285 ix11_2285 n_1019 k_main_r_make_list__f_8530 x0_9904 
  x1_9904 -> (k_main_r_make_list__f_8530 false true 0 true x0_9904 x1_9904);;
  f_r_make_list__f_10078 i_1018 ix00_2285 ix01_2285 ix10_2285 ix11_2285 n_1019 k_main_r_make_list__f_8530 x0_9901 
  x1_9901 -> (k_main_r_make_list__f_8530 true x0_9901 x1_9901 false true 0);;
  f_r_make_list__f_10079 i_1018 ix00_2285 ix01_2285 ix10_2285 ix11_2285 n_1019 k_main_r_make_list__f_8530 
  r_r_make_list0_8673 r_r_make_list1_8673 ->
      (f_1732 i_1018 n_1019 ix11_2285
        (f_r_make_list__f_10080 i_1018 ix00_2285 ix01_2285 ix10_2285 ix11_2285 n_1019 r_r_make_list0_8673
          r_r_make_list1_8673 k_main_r_make_list__f_8530));;
  f_r_make_list__f_10080 i_1018 ix00_2285 ix01_2285 ix10_2285 ix11_2285 n_1019 r_r_make_list0_8673 r_r_make_list1_8673 
  k_main_r_make_list__f_8530 x0_9883 x1_9883 ->
      (k_main_r_make_list__f_8530 true r_r_make_list0_8673 r_r_make_list1_8673 true x0_9883 x1_9883);;
  f_rs'_10058 i_1369 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 k_append_rs'_7515 p00_9366 p010_9366 p011_9366 p10_9366 p110_9366 p111_9366 p20_9366 p210_9366 
  p211_9366 -> (k_append_rs'_7515 p010_9366 p011_9366);;
  f_rs'__xs_10059 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  x_3455 x_3456 k_append_rs'__xs_7573 p00_9405 p010_9405 p011_9405 p10_9405 p110_9405 p111_9405 ->
      (k_append_rs'__xs_7573 true r_xs__ys011_8447 p010_9405 p011_9405);;
  f_rs'__xs_10060 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  x_3455 x_3456 k_append_rs'__xs_7573 xs__ys_1023 r_r_append00_7698 r_r_append010_7698 r_r_append011_7698 
  r_r_append10_7698 r_r_append110_7698 r_r_append111_7698 r_r_append20_7698 r_r_append210_7698 r_r_append211_7698 ->
      (xs__ys_1023 true x_3456 false 0
        (f_rs'__xs_10061 r_r_append00_7698 r_r_append010_7698 r_r_append011_7698 r_r_append10_7698 r_r_append110_7698
          r_r_append111_7698 r_r_append20_7698 r_r_append210_7698 r_r_append211_7698 r_xs__ys00_8447 r_xs__ys010_8447
          r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 x_3455 x_3456 k_append_rs'__xs_7573));;
  f_rs'__xs_10061 r_r_append00_7698 r_r_append010_7698 r_r_append011_7698 r_r_append10_7698 r_r_append110_7698 
  r_r_append111_7698 r_r_append20_7698 r_r_append210_7698 r_r_append211_7698 r_xs__ys00_8447 r_xs__ys010_8447 
  r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 x_3455 x_3456 k_append_rs'__xs_7573 p00_9393 
  p010_9393 p011_9393 p10_9393 p110_9393 p111_9393 ->
      (k_append_rs'__xs_7573 r_r_append010_7698 r_r_append011_7698 p010_9393 p011_9393);;
  f_rs'__xs__ys_10065 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 x_3377 x_3378 x_3379 k_append_rs'__xs__ys_7841 xs__ys_1023 r_xs__ys00_7917 r_xs__ys010_7917 
  r_xs__ys011_7917 r_xs__ys10_7917 r_xs__ys110_7917 r_xs__ys111_7917 ->
      (r_xs__ys_5907 r_xs__ys00_7917 r_xs__ys00_8447 r_xs__ys010_7917 r_xs__ys010_8447 r_xs__ys011_7917
        r_xs__ys011_8447 r_xs__ys10_7917 r_xs__ys10_8447 r_xs__ys110_7917 r_xs__ys110_8447 r_xs__ys111_7917
        r_xs__ys111_8447 x_3377 x_3378 x_3379 xs__ys_1023
        (f_rs'__xs__ys_10066 r_xs__ys00_7917 r_xs__ys00_8447 r_xs__ys010_7917 r_xs__ys010_8447 r_xs__ys011_7917
          r_xs__ys011_8447 r_xs__ys10_7917 r_xs__ys10_8447 r_xs__ys110_7917 r_xs__ys110_8447 r_xs__ys111_7917
          r_xs__ys111_8447 x_3377 x_3378 x_3379 k_append_rs'__xs__ys_7841));;
  f_rs'__xs__ys_10066 r_xs__ys00_7917 r_xs__ys00_8447 r_xs__ys010_7917 r_xs__ys010_8447 r_xs__ys011_7917 
  r_xs__ys011_8447 r_xs__ys10_7917 r_xs__ys10_8447 r_xs__ys110_7917 r_xs__ys110_8447 r_xs__ys111_7917 r_xs__ys111_8447 
  x_3377 x_3378 x_3379 k_append_rs'__xs__ys_7841 r_xs__ys00_7916 r_xs__ys010_7916 r_xs__ys011_7916 r_xs__ys10_7916 
  r_xs__ys110_7916 r_xs__ys111_7916 ->
      (k_append_rs'__xs__ys_7841 true r_xs__ys011_7916 r_xs__ys010_7917 r_xs__ys011_7917 r_xs__ys110_7916
        r_xs__ys111_7916);;
  f_rs'__xs__ys_10067 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 
  r_xs__ys111_8447 x_3377 x_3378 x_3379 k_append_rs'__xs__ys_7841 xs__ys_1023 r_r_append00_7995 r_r_append010_7995 
  r_r_append011_7995 r_r_append10_7995 r_r_append110_7995 r_r_append111_7995 r_r_append20_7995 r_r_append210_7995 
  r_r_append211_7995 ->
      (r_xs__ys_5886 r_r_append00_7995 r_r_append010_7995 r_r_append011_7995 r_r_append10_7995 r_r_append110_7995
        r_r_append111_7995 r_r_append20_7995 r_r_append210_7995 r_r_append211_7995 r_xs__ys00_8447 r_xs__ys010_8447
        r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 x_3377 x_3378 x_3379 xs__ys_1023
        (f_rs'__xs__ys_10068 r_r_append00_7995 r_r_append010_7995 r_r_append011_7995 r_r_append10_7995
          r_r_append110_7995 r_r_append111_7995 r_r_append20_7995 r_r_append210_7995 r_r_append211_7995 r_xs__ys00_8447
          r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 x_3377 x_3378 x_3379
          k_append_rs'__xs__ys_7841));;
  f_rs'__xs__ys_10068 r_r_append00_7995 r_r_append010_7995 r_r_append011_7995 r_r_append10_7995 r_r_append110_7995 
  r_r_append111_7995 r_r_append20_7995 r_r_append210_7995 r_r_append211_7995 r_xs__ys00_8447 r_xs__ys010_8447 
  r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 x_3377 x_3378 x_3379 k_append_rs'__xs__ys_7841 
  r_xs__ys00_7994 r_xs__ys010_7994 r_xs__ys011_7994 r_xs__ys10_7994 r_xs__ys110_7994 r_xs__ys111_7994 ->
      (k_append_rs'__xs__ys_7841 r_r_append010_7995 r_r_append011_7995 r_xs__ys010_7994 r_xs__ys011_7994
        r_xs__ys110_7994 r_xs__ys111_7994);;
  f_rs'__xs__ys_10069 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 r_xs__ys00_8447 
  r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_rs'__xs__ys_8004 
  x0_9628 x1_9628 -> (k_append_rs'__xs__ys_8004 false true 0 false true 0 true x0_9628 x1_9628);;
  f_rs'__xs__ys_10070 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 r_xs__ys00_8447 
  r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_rs'__xs__ys_8004 
  x0_9615 x1_9615 -> (k_append_rs'__xs__ys_8004 false true 0 true x0_9615 x1_9615 false true 0);;
  f_rs'__xs__ys_10071 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 r_xs__ys00_8447 
  r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_rs'__xs__ys_8004 
  r_xs__ys00_8194 r_xs__ys01_8194 r_xs__ys10_8194 r_xs__ys11_8194 ->
      (k_append_rs'__xs__ys_8004 false true 0 true r_xs__ys00_8194 r_xs__ys01_8194 true r_xs__ys10_8194 r_xs__ys11_8194);;
  f_rs'__xs__ys_10072 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 r_xs__ys00_8447 
  r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_rs'__xs__ys_8004 
  x0_9572 x1_9572 -> (k_append_rs'__xs__ys_8004 true x0_9572 x1_9572 false true 0 false true 0);;
  f_rs'__xs__ys_10073 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 r_xs__ys00_8447 
  r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_rs'__xs__ys_8004 
  r_rs'__ys00_8296 r_rs'__ys01_8296 r_rs'__ys10_8296 r_rs'__ys11_8296 ->
      (k_append_rs'__xs__ys_8004 true r_rs'__ys00_8296 r_rs'__ys01_8296 false true 0 true r_rs'__ys10_8296
        r_rs'__ys11_8296);;
  f_rs'__xs__ys_10074 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 r_xs__ys00_8447 
  r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_rs'__xs__ys_8004 
  r_rs'__xs00_8346 r_rs'__xs01_8346 r_rs'__xs10_8346 r_rs'__xs11_8346 ->
      (k_append_rs'__xs__ys_8004 true r_rs'__xs00_8346 r_rs'__xs01_8346 true r_rs'__xs10_8346 r_rs'__xs11_8346 false
        true 0);;
  f_rs'__xs__ys_10075 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 r_xs__ys00_8447 
  r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_rs'__xs__ys_8004 
  r_rs'__xs__ys00_8387 r_rs'__xs__ys01_8387 r_rs'__xs__ys10_8387 r_rs'__xs__ys11_8387 r_rs'__xs__ys20_8387 
  r_rs'__xs__ys21_8387 ->
      (k_append_rs'__xs__ys_8004 true r_rs'__xs__ys00_8387 r_rs'__xs__ys01_8387 true r_rs'__xs__ys10_8387
        r_rs'__xs__ys11_8387 true r_rs'__xs__ys20_8387 r_rs'__xs__ys21_8387);;
  f_rs'__ys_10062 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  x_3516 x_3517 k_append_rs'__ys_7708 r_xs__ys00_7751 r_xs__ys010_7751 r_xs__ys011_7751 r_xs__ys10_7751 
  r_xs__ys110_7751 r_xs__ys111_7751 ->
      (k_append_rs'__ys_7708 true r_xs__ys011_7751 r_xs__ys110_7751 r_xs__ys111_7751);;
  f_rs'__ys_10063 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  x_3516 x_3517 k_append_rs'__ys_7708 xs__ys_1023 r_r_append00_7830 r_r_append010_7830 r_r_append011_7830 
  r_r_append10_7830 r_r_append110_7830 r_r_append111_7830 r_r_append20_7830 r_r_append210_7830 r_r_append211_7830 ->
      (xs__ys_1023 false 0 true x_3517
        (f_rs'__ys_10064 r_r_append00_7830 r_r_append010_7830 r_r_append011_7830 r_r_append10_7830 r_r_append110_7830
          r_r_append111_7830 r_r_append20_7830 r_r_append210_7830 r_r_append211_7830 r_xs__ys00_8447 r_xs__ys010_8447
          r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 x_3516 x_3517 k_append_rs'__ys_7708));;
  f_rs'__ys_10064 r_r_append00_7830 r_r_append010_7830 r_r_append011_7830 r_r_append10_7830 r_r_append110_7830 
  r_r_append111_7830 r_r_append20_7830 r_r_append210_7830 r_r_append211_7830 r_xs__ys00_8447 r_xs__ys010_8447 
  r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 x_3516 x_3517 k_append_rs'__ys_7708 p00_9438 
  p010_9438 p011_9438 p10_9438 p110_9438 p111_9438 ->
      (k_append_rs'__ys_7708 r_r_append010_7830 r_r_append011_7830 p110_9438 p111_9438);;
  f_xs'_10052 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  x_1269 k_append_xs'_7005 p00_9217 p010_9217 p011_9217 p10_9217 p110_9217 p111_9217 ->
      (k_append_xs'_7005 p010_9217 p011_9217);;
  f_xs'__ys_10053 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  x_3337 x_3338 k_append_xs'__ys_7049 r_xs__ys00_7086 r_xs__ys010_7086 r_xs__ys011_7086 r_xs__ys10_7086 
  r_xs__ys110_7086 r_xs__ys111_7086 ->
      (k_append_xs'__ys_7049 r_xs__ys010_7086 r_xs__ys011_7086 r_xs__ys110_7086 r_xs__ys111_7086);;
  f_xs'__ys_10054 ii00_2944 ii01_2944 ii10_2944 ii11_2944 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 
  r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_xs'__ys_7097 x0_9246 x1_9246 ->
      (k_append_xs'__ys_7097 false true 0 true x0_9246 x1_9246);;
  f_xs'__ys_10055 ii00_2944 ii01_2944 ii10_2944 ii11_2944 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 
  r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_xs'__ys_7097 x0_9243 x1_9243 ->
      (k_append_xs'__ys_7097 true x0_9243 x1_9243 false true 0);;
  f_xs'__ys_10056 ii00_2944 ii01_2944 ii10_2944 ii11_2944 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 
  r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_xs'__ys_7097 r_xs'__ys00_7231 r_xs'__ys01_7231 
  r_xs'__ys10_7231 r_xs'__ys11_7231 ->
      (k_append_xs'__ys_7097 true r_xs'__ys00_7231 r_xs'__ys01_7231 true r_xs'__ys10_7231 r_xs'__ys11_7231);;
  f_xs_10036 i_3153 k_append_xs_6221 p00_9137 p010_9137 p011_9137 p10_9137 p110_9137 p111_9137 ->
      (k_append_xs_6221 p010_9137 p011_9137);;
  f_xs__ys_10040 x_3292 x_3293 k_append_xs__ys_6391 r_xs__ys00_6428 r_xs__ys010_6428 r_xs__ys011_6428 r_xs__ys10_6428 
  r_xs__ys110_6428 r_xs__ys111_6428 ->
      (k_append_xs__ys_6391 r_xs__ys010_6428 r_xs__ys011_6428 r_xs__ys110_6428 r_xs__ys111_6428);;
  f_ys_10037 i_3146 k_append_ys_6265 p00_9147 p010_9147 p011_9147 p10_9147 p110_9147 p111_9147 ->
      (k_append_ys_6265 p110_9147 p111_9147);;
  f_ys__xs_10043 x_3674 x_3675 k_append_ys__xs_6521 r_xs__ys00_6558 r_xs__ys010_6558 r_xs__ys011_6558 r_xs__ys10_6558 
  r_xs__ys110_6558 r_xs__ys111_6558 ->
      (k_append_ys__xs_6521 r_xs__ys110_6558 r_xs__ys111_6558 r_xs__ys010_6558 r_xs__ys011_6558);;
  f_ys__xs__ys_10041 x_3615 x_3616 x_3617 k_append_ys__xs__ys_6439 xs__ys_1023 r_xs__ys00_6509 r_xs__ys010_6509 
  r_xs__ys011_6509 r_xs__ys10_6509 r_xs__ys110_6509 r_xs__ys111_6509 ->
      (r_xs__ys_6059 r_xs__ys00_6509 r_xs__ys010_6509 r_xs__ys011_6509 r_xs__ys10_6509 r_xs__ys110_6509
        r_xs__ys111_6509 x_3615 x_3616 x_3617 xs__ys_1023
        (f_ys__xs__ys_10042 r_xs__ys00_6509 r_xs__ys010_6509 r_xs__ys011_6509 r_xs__ys10_6509 r_xs__ys110_6509
          r_xs__ys111_6509 x_3615 x_3616 x_3617 k_append_ys__xs__ys_6439));;
  f_ys__xs__ys_10042 r_xs__ys00_6509 r_xs__ys010_6509 r_xs__ys011_6509 r_xs__ys10_6509 r_xs__ys110_6509 
  r_xs__ys111_6509 x_3615 x_3616 x_3617 k_append_ys__xs__ys_6439 r_xs__ys00_6508 r_xs__ys010_6508 r_xs__ys011_6508 
  r_xs__ys10_6508 r_xs__ys110_6508 r_xs__ys111_6508 ->
      (k_append_ys__xs__ys_6439 r_xs__ys110_6509 r_xs__ys111_6509 r_xs__ys010_6508 r_xs__ys011_6508 r_xs__ys110_6508
        r_xs__ys111_6508);;
  f_ys__xs__ys_10045 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 r_xs__ys00_8447 
  r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_ys__xs__ys_6600 x0_9823 
  x1_9823 -> (k_append_ys__xs__ys_6600 false true 0 false true 0 true x0_9823 x1_9823);;
  f_ys__xs__ys_10046 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 r_xs__ys00_8447 
  r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_ys__xs__ys_6600 x0_9810 
  x1_9810 -> (k_append_ys__xs__ys_6600 false true 0 true x0_9810 x1_9810 false true 0);;
  f_ys__xs__ys_10047 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 r_xs__ys00_8447 
  r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_ys__xs__ys_6600 
  r_xs__ys00_6790 r_xs__ys01_6790 r_xs__ys10_6790 r_xs__ys11_6790 ->
      (k_append_ys__xs__ys_6600 false true 0 true r_xs__ys00_6790 r_xs__ys01_6790 true r_xs__ys10_6790 r_xs__ys11_6790);;
  f_ys__xs__ys_10048 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 r_xs__ys00_8447 
  r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_ys__xs__ys_6600 x0_9767 
  x1_9767 -> (k_append_ys__xs__ys_6600 true x0_9767 x1_9767 false true 0 false true 0);;
  f_ys__xs__ys_10049 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 r_xs__ys00_8447 
  r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_ys__xs__ys_6600 
  r_ys__ys00_6892 r_ys__ys01_6892 r_ys__ys10_6892 r_ys__ys11_6892 ->
      (k_append_ys__xs__ys_6600 true r_ys__ys00_6892 r_ys__ys01_6892 false true 0 true r_ys__ys10_6892 r_ys__ys11_6892);;
  f_ys__xs__ys_10050 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 r_xs__ys00_8447 
  r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_ys__xs__ys_6600 
  r_ys__xs00_6942 r_ys__xs01_6942 r_ys__xs10_6942 r_ys__xs11_6942 ->
      (k_append_ys__xs__ys_6600 true r_ys__xs00_6942 r_ys__xs01_6942 true r_ys__xs10_6942 r_ys__xs11_6942 false true 0);;
  f_ys__xs__ys_10051 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 r_xs__ys00_8447 
  r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 k_append_ys__xs__ys_6600 
  r_ys__xs__ys00_6983 r_ys__xs__ys01_6983 r_ys__xs__ys10_6983 r_ys__xs__ys11_6983 r_ys__xs__ys20_6983 
  r_ys__xs__ys21_6983 ->
      (k_append_ys__xs__ys_6600 true r_ys__xs__ys00_6983 r_ys__xs__ys01_6983 true r_ys__xs__ys10_6983
        r_ys__xs__ys11_6983 true r_ys__xs__ys20_6983 r_ys__xs__ys21_6983);;
  f_ys__ys_10038 x_3719 x_3720 k_append_ys__ys_6309 xs__ys_1023 r_xs__ys00_6380 r_xs__ys010_6380 r_xs__ys011_6380 
  r_xs__ys10_6380 r_xs__ys110_6380 r_xs__ys111_6380 ->
      (xs__ys_1023 false 0 true x_3720
        (f_ys__ys_10039 r_xs__ys00_6380 r_xs__ys010_6380 r_xs__ys011_6380 r_xs__ys10_6380 r_xs__ys110_6380
          r_xs__ys111_6380 x_3719 x_3720 k_append_ys__ys_6309));;
  f_ys__ys_10039 r_xs__ys00_6380 r_xs__ys010_6380 r_xs__ys011_6380 r_xs__ys10_6380 r_xs__ys110_6380 r_xs__ys111_6380 
  x_3719 x_3720 k_append_ys__ys_6309 p00_9165 p010_9165 p011_9165 p10_9165 p110_9165 p111_9165 ->
      (k_append_ys__ys_6309 r_xs__ys110_6380 r_xs__ys111_6380 p110_9165 p111_9165);;
  fail_10215 b k -> {fail} => (k ());;
  main_1017 i_1018 n_1019 k_main_8489 -> (r_make_list_5550 i_1018 n_1019 (f_main_10076 i_1018 n_1019 k_main_8489));;
  make_list_1008 n_1009 k_make_list_6114 when (n_1009 < 0) -> (k_make_list_6114 (f_make_list_10032 n_1009));;
  make_list_1008 n_1009 k_make_list_6114 when (not (n_1009 < 0)) ->
      (r_f_3805 n_1009 (f_make_list_10033 n_1009 k_make_list_6114));;
  r_append_4560 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  xs__ys_1023 k_append_r_append_7352 ->
      (append_1165
        (xs'__ys_1965 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447
          r_xs__ys111_8447 xs__ys_1023) k_append_r_append_7352);;
  r_append_5708 i_1018 n_1019 r_make_list_9048 k_main_r_append_8785 ->
      (append_1165 (r_make_list__f_1999 i_1018 n_1019 r_make_list_9048) k_main_r_append_8785);;
  r_f_3805 n_1009 k_make_list_r_f_6132 -> (rand_int k_make_list_r_f_6132);;
  r_f_5821 k_r_f_9059 -> (rand_int k_r_f_9059);;
  r_f_5823 r_f_9104 k_r_f_9071 -> (rand_int k_r_f_9071);;
  r_make_list_3808 n_1009 r_f_6191 k_make_list_r_make_list_6153 ->
      (make_list_1008 (n_1009 - 1) k_make_list_r_make_list_6153);;
  r_make_list_5550 i_1018 n_1019 k_main_r_make_list_8502 -> (make_list_1008 n_1019 k_main_r_make_list_8502);;
  r_make_list__f_1999 i_1018 n_1019 r_make_list_9048 ix00_2285 ix01_2285 ix10_2285 ix11_2285 k_main_r_make_list__f_8530 when (
      ix00_2285 <=> false) ->
      (br_r_make_list__f_10117 (ix10_2285 <=> false) i_1018 n_1019 r_make_list_9048 ix00_2285 ix01_2285 ix10_2285
        ix11_2285 k_main_r_make_list__f_8530);;
  r_make_list__f_1999 i_1018 n_1019 r_make_list_9048 ix00_2285 ix01_2285 ix10_2285 ix11_2285 k_main_r_make_list__f_8530 when (
      not (ix00_2285 <=> false)) ->
      (br_r_make_list__f_10119 (ix10_2285 <=> false) i_1018 n_1019 r_make_list_9048 ix00_2285 ix01_2285 ix10_2285
        ix11_2285 k_main_r_make_list__f_8530);;
  r_r_append_5826 i_1018 n_1019 r_append_9028 k_main_r_r_append_8982 ->
      (r_append_9028 true i_1018 false 0 false 0 k_main_r_r_append_8982);;
  r_r_append_5895 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  x_3377 x_3378 x_3379 r_append_8406 k_append_rs'__xs__ys_r_r_append_7950 ->
      (r_append_8406 true (x_3377 - 1) false 0 false 0 k_append_rs'__xs__ys_r_r_append_7950);;
  r_r_append_5924 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  x_3516 x_3517 r_append_8406 k_append_rs'__ys_r_r_append_7784 ->
      (r_append_8406 true (x_3516 - 1) false 0 false 0 k_append_rs'__ys_r_r_append_7784);;
  r_r_append_5952 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  x_3455 x_3456 r_append_8406 k_append_rs'__xs_r_r_append_7652 ->
      (r_append_8406 true (x_3455 - 1) false 0 false 0 k_append_rs'__xs_r_r_append_7652);;
  r_r_main_5825 r_f_9103 r_f_9104 k_r_r_main_9092 -> (main_1017 r_f_9104 r_f_9103 k_r_r_main_9092);;
  r_r_make_list_5562 i_1018 ix00_2285 ix01_2285 ix10_2285 ix11_2285 n_1019 r_make_list_9048 
  k_main_r_make_list__f_r_r_make_list_8639 -> (r_make_list_9048 ix01_2285 k_main_r_make_list__f_r_r_make_list_8639);;
  r_r_make_list_5809 i_1018 n_1019 r_r_append00_9012 r_r_append010_9012 r_r_append011_9012 r_r_append10_9012 
  r_r_append110_9012 r_r_append111_9012 r_r_append20_9012 r_r_append210_9012 r_r_append211_9012 r_make_list_9048 
  k_main_r_r_make_list_8994 -> (r_make_list_9048 i_1018 k_main_r_r_make_list_8994);;
  r_rs'__xs_4923 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 r_xs__ys00_8447 r_xs__ys010_8447 
  r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 r_append_8406 xs__ys_1023 
  k_append_rs'__xs__ys_r_rs'__xs_8308 ->
      (rs'__xs_3491 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447
        r_append_8406 xs__ys_1023 iii01_2831 iii11_2831 k_append_rs'__xs__ys_r_rs'__xs_8308);;
  r_rs'__xs__ys_4891 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 r_xs__ys00_8447 
  r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 r_append_8406 xs__ys_1023 
  k_append_rs'__xs__ys_r_rs'__xs__ys_8355 ->
      (rs'__xs__ys_3426 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447
        r_xs__ys111_8447 r_append_8406 xs__ys_1023 iii01_2831 iii11_2831 iii21_2831
        k_append_rs'__xs__ys_r_rs'__xs__ys_8355);;
  r_rs'__ys_4965 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 r_xs__ys00_8447 r_xs__ys010_8447 
  r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 r_append_8406 xs__ys_1023 
  k_append_rs'__xs__ys_r_rs'__ys_8258 ->
      (rs'__ys_3552 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447
        r_append_8406 xs__ys_1023 iii01_2831 iii21_2831 k_append_rs'__xs__ys_r_rs'__ys_8258);;
  r_xs'__ys_4413 ii00_2944 ii01_2944 ii10_2944 ii11_2944 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 
  r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 xs__ys_1023 k_append_xs'__ys_r_xs'__ys_7207 ->
      (xs'__ys_3363 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447
        xs__ys_1023 ii01_2944 ii11_2944 k_append_xs'__ys_r_xs'__ys_7207);;
  r_xs__ys_3874 x_3719 x_3720 xs__ys_1023 k_append_ys__ys_r_xs__ys_6334 ->
      (xs__ys_1023 false 0 true x_3719 k_append_ys__ys_r_xs__ys_6334);;
  r_xs__ys_3936 x_3615 x_3616 x_3617 xs__ys_1023 k_append_ys__xs__ys_r_xs__ys_6464 ->
      (xs__ys_1023 false 0 true x_3615 k_append_ys__xs__ys_r_xs__ys_6464);;
  r_xs__ys_4859 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  x_3377 x_3378 x_3379 xs__ys_1023 k_append_rs'__xs__ys_r_xs__ys_7866 ->
      (xs__ys_1023 true x_3378 false 0 k_append_rs'__xs__ys_r_xs__ys_7866);;
  r_xs__ys_5049 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 r_xs__ys00_8447 r_xs__ys010_8447 
  r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 xs__ys_1023 k_append_rs'__xs__ys_r_xs__ys_8156 ->
      (xs__ys_3318 xs__ys_1023 iii11_2831 iii21_2831 k_append_rs'__xs__ys_r_xs__ys_8156);;
  r_xs__ys_5383 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 r_xs__ys00_8447 r_xs__ys010_8447 
  r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 xs__ys_1023 k_append_ys__xs__ys_r_xs__ys_6752 ->
      (xs__ys_3318 xs__ys_1023 iii11_3100 iii21_3100 k_append_ys__xs__ys_r_xs__ys_6752);;
  r_xs__ys_5886 r_r_append00_7995 r_r_append010_7995 r_r_append011_7995 r_r_append10_7995 r_r_append110_7995 
  r_r_append111_7995 r_r_append20_7995 r_r_append210_7995 r_r_append211_7995 r_xs__ys00_8447 r_xs__ys010_8447 
  r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 x_3377 x_3378 x_3379 xs__ys_1023 
  k_append_rs'__xs__ys_r_xs__ys_7980 -> (xs__ys_1023 true x_3378 true x_3379 k_append_rs'__xs__ys_r_xs__ys_7980);;
  r_xs__ys_5907 r_xs__ys00_7917 r_xs__ys00_8447 r_xs__ys010_7917 r_xs__ys010_8447 r_xs__ys011_7917 r_xs__ys011_8447 
  r_xs__ys10_7917 r_xs__ys10_8447 r_xs__ys110_7917 r_xs__ys110_8447 r_xs__ys111_7917 r_xs__ys111_8447 x_3377 x_3378 
  x_3379 xs__ys_1023 k_append_rs'__xs__ys_r_xs__ys_7896 ->
      (xs__ys_1023 true 0 true x_3379 k_append_rs'__xs__ys_r_xs__ys_7896);;
  r_xs__ys_5935 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  x_3516 x_3517 xs__ys_1023 k_append_rs'__ys_r_xs__ys_7733 ->
      (xs__ys_1023 true 0 true x_3517 k_append_rs'__ys_r_xs__ys_7733);;
  r_xs__ys_6024 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  x_3337 x_3338 xs__ys_1023 k_append_xs'__ys_r_xs__ys_7074 ->
      (xs__ys_1023 true (x_3337 + 1) true x_3338 k_append_xs'__ys_r_xs__ys_7074);;
  r_xs__ys_6041 xs__ys_1023 k_append_r_xs__ys_6590 -> (xs__ys_1023 true 0 false 0 k_append_r_xs__ys_6590);;
  r_xs__ys_6049 x_3674 x_3675 xs__ys_1023 k_append_ys__xs_r_xs__ys_6546 ->
      (xs__ys_1023 true x_3675 true x_3674 k_append_ys__xs_r_xs__ys_6546);;
  r_xs__ys_6059 r_xs__ys00_6509 r_xs__ys010_6509 r_xs__ys011_6509 r_xs__ys10_6509 r_xs__ys110_6509 r_xs__ys111_6509 
  x_3615 x_3616 x_3617 xs__ys_1023 k_append_ys__xs__ys_r_xs__ys_6494 ->
      (xs__ys_1023 true x_3616 true x_3617 k_append_ys__xs__ys_r_xs__ys_6494);;
  r_xs__ys_6068 x_3292 x_3293 xs__ys_1023 k_append_xs__ys_r_xs__ys_6416 ->
      (xs__ys_1023 true x_3292 true x_3293 k_append_xs__ys_r_xs__ys_6416);;
  r_ys__xs_5257 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 r_xs__ys00_8447 r_xs__ys010_8447 
  r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 xs__ys_1023 k_append_ys__xs__ys_r_ys__xs_6904 ->
      (ys__xs_3700 xs__ys_1023 iii01_3100 iii11_3100 k_append_ys__xs__ys_r_ys__xs_6904);;
  r_ys__xs__ys_5225 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 r_xs__ys00_8447 r_xs__ys010_8447 
  r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 xs__ys_1023 k_append_ys__xs__ys_r_ys__xs__ys_6951 ->
      (ys__xs__ys_3654 xs__ys_1023 iii01_3100 iii11_3100 iii21_3100 k_append_ys__xs__ys_r_ys__xs__ys_6951);;
  r_ys__ys_5299 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 r_xs__ys00_8447 r_xs__ys010_8447 
  r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 xs__ys_1023 k_append_ys__xs__ys_r_ys__ys_6854 ->
      (ys__ys_3745 xs__ys_1023 iii01_3100 iii21_3100 k_append_ys__xs__ys_r_ys__ys_6854);;
  rs'_1195 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  r_append_8406 i_1369 k_append_rs'_7515 when (i_1369 = 0) -> (k_append_rs'_7515 true r_xs__ys011_8447);;
  rs'_1195 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  r_append_8406 i_1369 k_append_rs'_7515 when (not (i_1369 = 0)) ->
      (r_append_8406 true (i_1369 - 1) false 0 false 0
        (f_rs'_10058 i_1369 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447
          r_xs__ys111_8447 k_append_rs'_7515));;
  rs'__xs_3491 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  r_append_8406 xs__ys_1023 x_3455 x_3456 k_append_rs'__xs_7573 when (
      x_3455 = 0) ->
      (xs__ys_1023 true x_3456 false 0
        (f_rs'__xs_10059 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447
          r_xs__ys111_8447 x_3455 x_3456 k_append_rs'__xs_7573));;
  rs'__xs_3491 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  r_append_8406 xs__ys_1023 x_3455 x_3456 k_append_rs'__xs_7573 when (
      not (x_3455 = 0)) ->
      (r_r_append_5952 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447
        r_xs__ys111_8447 x_3455 x_3456 r_append_8406
        (f_rs'__xs_10060 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447
          r_xs__ys111_8447 x_3455 x_3456 k_append_rs'__xs_7573 xs__ys_1023));;
  rs'__xs__ys_1986 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  r_append_8406 xs__ys_1023 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 k_append_rs'__xs__ys_8004 when (
      iii00_2831 <=> false) ->
      (br_rs'__xs__ys_10107 (iii10_2831 <=> false) r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447
        r_xs__ys110_8447 r_xs__ys111_8447 r_append_8406 xs__ys_1023 iii00_2831 iii01_2831 iii10_2831 iii11_2831
        iii20_2831 iii21_2831 k_append_rs'__xs__ys_8004);;
  rs'__xs__ys_1986 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  r_append_8406 xs__ys_1023 iii00_2831 iii01_2831 iii10_2831 iii11_2831 iii20_2831 iii21_2831 k_append_rs'__xs__ys_8004 when (
      not (iii00_2831 <=> false)) ->
      (br_rs'__xs__ys_10113 (iii10_2831 <=> false) r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447
        r_xs__ys110_8447 r_xs__ys111_8447 r_append_8406 xs__ys_1023 iii00_2831 iii01_2831 iii10_2831 iii11_2831
        iii20_2831 iii21_2831 k_append_rs'__xs__ys_8004);;
  rs'__xs__ys_3426 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  r_append_8406 xs__ys_1023 x_3377 x_3378 x_3379 k_append_rs'__xs__ys_7841 when (
      x_3377 = 0) ->
      (r_xs__ys_4859 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447
        r_xs__ys111_8447 x_3377 x_3378 x_3379 xs__ys_1023
        (f_rs'__xs__ys_10065 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447
          r_xs__ys111_8447 x_3377 x_3378 x_3379 k_append_rs'__xs__ys_7841 xs__ys_1023));;
  rs'__xs__ys_3426 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  r_append_8406 xs__ys_1023 x_3377 x_3378 x_3379 k_append_rs'__xs__ys_7841 when (
      not (x_3377 = 0)) ->
      (r_r_append_5895 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447
        r_xs__ys111_8447 x_3377 x_3378 x_3379 r_append_8406
        (f_rs'__xs__ys_10067 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447
          r_xs__ys111_8447 x_3377 x_3378 x_3379 k_append_rs'__xs__ys_7841 xs__ys_1023));;
  rs'__ys_3552 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  r_append_8406 xs__ys_1023 x_3516 x_3517 k_append_rs'__ys_7708 when (
      x_3516 = 0) ->
      (r_xs__ys_5935 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447
        r_xs__ys111_8447 x_3516 x_3517 xs__ys_1023
        (f_rs'__ys_10062 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447
          r_xs__ys111_8447 x_3516 x_3517 k_append_rs'__ys_7708));;
  rs'__ys_3552 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  r_append_8406 xs__ys_1023 x_3516 x_3517 k_append_rs'__ys_7708 when (
      not (x_3516 = 0)) ->
      (r_r_append_5924 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447
        r_xs__ys111_8447 x_3516 x_3517 r_append_8406
        (f_rs'__ys_10063 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447
          r_xs__ys111_8447 x_3516 x_3517 k_append_rs'__ys_7708 xs__ys_1023));;
  xs'_1014 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  xs__ys_1023 x_1269 k_append_xs'_7005 ->
      (xs__ys_1023 true (x_1269 + 1) false 0
        (f_xs'_10052 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447
          r_xs__ys111_8447 x_1269 k_append_xs'_7005));;
  xs'__ys_1965 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  xs__ys_1023 ii00_2944 ii01_2944 ii10_2944 ii11_2944 k_append_xs'__ys_7097 when (
      ii00_2944 <=> false) ->
      (br_xs'__ys_10099 (ii10_2944 <=> false) r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447
        r_xs__ys110_8447 r_xs__ys111_8447 xs__ys_1023 ii00_2944 ii01_2944 ii10_2944 ii11_2944 k_append_xs'__ys_7097);;
  xs'__ys_1965 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  xs__ys_1023 ii00_2944 ii01_2944 ii10_2944 ii11_2944 k_append_xs'__ys_7097 when (
      not (ii00_2944 <=> false)) ->
      (br_xs'__ys_10101 (ii10_2944 <=> false) r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447
        r_xs__ys110_8447 r_xs__ys111_8447 xs__ys_1023 ii00_2944 ii01_2944 ii10_2944 ii11_2944 k_append_xs'__ys_7097);;
  xs'__ys_3363 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  xs__ys_1023 x_3337 x_3338 k_append_xs'__ys_7049 ->
      (r_xs__ys_6024 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447
        r_xs__ys111_8447 x_3337 x_3338 xs__ys_1023
        (f_xs'__ys_10053 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447
          r_xs__ys111_8447 x_3337 x_3338 k_append_xs'__ys_7049));;
  xs_1939 xs__ys_1023 i_3153 k_append_xs_6221 ->
      (xs__ys_1023 true i_3153 false 0 (f_xs_10036 i_3153 k_append_xs_6221));;
  xs__ys_3318 xs__ys_1023 x_3292 x_3293 k_append_xs__ys_6391 ->
      (r_xs__ys_6068 x_3292 x_3293 xs__ys_1023 (f_xs__ys_10040 x_3292 x_3293 k_append_xs__ys_6391));;
  ys_1940 xs__ys_1023 i_3146 k_append_ys_6265 ->
      (xs__ys_1023 false 0 true i_3146 (f_ys_10037 i_3146 k_append_ys_6265));;
  ys__xs_3700 xs__ys_1023 x_3674 x_3675 k_append_ys__xs_6521 ->
      (r_xs__ys_6049 x_3674 x_3675 xs__ys_1023 (f_ys__xs_10043 x_3674 x_3675 k_append_ys__xs_6521));;
  ys__xs__ys_1990 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  xs__ys_1023 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 k_append_ys__xs__ys_6600 when (
      iii00_3100 <=> false) ->
      (br_ys__xs__ys_10091 (iii10_3100 <=> false) r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447
        r_xs__ys110_8447 r_xs__ys111_8447 xs__ys_1023 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100
        k_append_ys__xs__ys_6600);;
  ys__xs__ys_1990 r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447 r_xs__ys110_8447 r_xs__ys111_8447 
  xs__ys_1023 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100 k_append_ys__xs__ys_6600 when (
      not (iii00_3100 <=> false)) ->
      (br_ys__xs__ys_10097 (iii10_3100 <=> false) r_xs__ys00_8447 r_xs__ys010_8447 r_xs__ys011_8447 r_xs__ys10_8447
        r_xs__ys110_8447 r_xs__ys111_8447 xs__ys_1023 iii00_3100 iii01_3100 iii10_3100 iii11_3100 iii20_3100 iii21_3100
        k_append_ys__xs__ys_6600);;
  ys__xs__ys_3654 xs__ys_1023 x_3615 x_3616 x_3617 k_append_ys__xs__ys_6439 ->
      (r_xs__ys_3936 x_3615 x_3616 x_3617 xs__ys_1023
        (f_ys__xs__ys_10041 x_3615 x_3616 x_3617 k_append_ys__xs__ys_6439 xs__ys_1023));;
  ys__ys_3745 xs__ys_1023 x_3719 x_3720 k_append_ys__ys_6309 ->
      (r_xs__ys_3874 x_3719 x_3720 xs__ys_1023 (f_ys__ys_10038 x_3719 x_3720 k_append_ys__ys_6309 xs__ys_1023));;
Types:
  main_10031 : X
  append_1165 : ((bool ->
                  int ->
                  x_4:bool ->
                  int ->
                  (bool ->
                   bool ->
                   int ->
                   x_10:bool[x_10 || (not x_4)] -> x_11:bool -> x_12:int[(not x_11) && x_12 = 0 || (not x_10)] -> X)
                  -> X)
                 ->
                 ((x_17:bool ->
                   x_18:int ->
                   x_19:bool ->
                   x_20:int ->
                   x_21:bool ->
                   int ->
                   (x_24:bool[x_24 || (not x_17)] ->
                    bool ->
                    x_26:int ->
                    x_27:bool[x_27 || (not x_19)] ->
                    bool ->
                    x_29:int[x_29 = x_26 || (not ((x_27 && x_24) && (x_20 = x_18)))] ->
                    x_30:bool[x_30 || (not x_21)] -> x_31:bool -> x_32:int[(not x_31) && x_32 = 0 || (not x_30)] -> X)
                   -> X)
                 -> X) -> X)
  fail_10215 : (bool -> (unit -> X) -> X)
  make_list_1008 : (int -> ((int -> (bool -> int -> X) -> X) -> X) -> X)
  xs_1939 : ((bool -> int -> bool -> int -> (bool -> bool -> int -> bool -> bool -> int -> X) -> X) ->
             int -> (bool -> int -> X) -> X)
  xs__ys_3318 : ((bool -> int -> bool -> int -> (bool -> bool -> int -> bool -> bool -> int -> X) -> X) ->
                 int -> int -> (bool -> int -> bool -> int -> X) -> X)
  ys_1940 : ((bool ->
              int ->
              x_4:bool ->
              int ->
              (bool ->
               bool ->
               int -> x_10:bool[x_10 || (not x_4)] -> x_11:bool -> x_12:int[(not x_11) && x_12 = 0 || (not x_10)] -> X)
              -> X)
             -> int -> (x_17:bool -> x_18:int[(not x_17) && x_18 = 0] -> X) -> X)

(0-1) Abstracting ... DONE!

(0-2) Checking HORS ... DONE!

Error trace::
  main_10031 ... --> 
  r_f_5821 ... --> 
  f_10084 ... --> 
  r_f_5823 ... --> 
  f_10085 ... --> 
  r_r_main_5825 ... --> 
  main_1017 ... --> 
  r_make_list_5550 ... --> 
  make_list_1008 [2/2] ... --> 
  r_f_3805 ... --> 
  f_make_list_10033 ... --> 
  r_make_list_3808 ... --> 
  make_list_1008 [1/2] ... --> 
  f_make_list_10034 ... --> 
  f_main_10076 ... --> 
  r_append_5708 ... --> 
  append_1165 ... --> 
  r_xs__ys_6041 ... --> 
  r_make_list__f_1999 [2/2] ... --> 
  br_r_make_list__f_10119 [2/2] ... --> 
  r_r_make_list_5562 ... --> 
  f_make_list_10035 [1/2] ... --> 
  f_r_make_list__f_10079 ... --> 
  f_1732 ... --> 
  f_r_make_list__f_10080 ... --> 
  f_append_10044 [1/2] ... --> 
  f_main_10081 ... --> 
  r_r_append_5826 ... --> 
  ys__xs__ys_1990 [2/2] ... --> 
  br_ys__xs__ys_10097 [2/2] ... --> 
  br_ys__xs__ys_10095 [2/2] ... --> 
  r_ys__xs__ys_5225 ... --> 
  ys__xs__ys_3654 ... --> 
  r_xs__ys_3936 ... --> 
  r_make_list__f_1999 [2/2] ... --> 
  br_r_make_list__f_10119 [2/2] ... --> 
  r_r_make_list_5562 ... --> 
  f_make_list_10035 [1/2] ... --> 
  f_r_make_list__f_10079 ... --> 
  f_1732 ... --> 
  f_r_make_list__f_10080 ... --> 
  f_ys__xs__ys_10041 ... --> 
  r_xs__ys_6059 ... --> 
  r_make_list__f_1999 [2/2] ... --> 
  br_r_make_list__f_10119 [2/2] ... --> 
  r_r_make_list_5562 ... --> 
  f_make_list_10035 [1/2] ... --> 
  f_r_make_list__f_10079 ... --> 
  f_1732 ... --> 
  f_r_make_list__f_10080 ... --> 
  f_ys__xs__ys_10042 ... --> 
  f_ys__xs__ys_10051 ... --> 
  f_main_10082 ... --> 
  r_r_make_list_5809 ... --> 
  f_make_list_10035 [1/2] ... --> 
  f_main_10083 [2/2] ... --> 
  fail_10215 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 0; 0; 1; 1; 0; 0; 0; 
  0; 0; 0; 0; 1; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0

(0-3) Checking counterexample ... DONE!

(0-4) Discovering predicates ... 
