let fst3 (x,y,z) = x
let snd3 (x,y,z) = y
let trd (x,y,z) = z

let rand_int () = Random.int 0

let rec make_list_1008 (n_1009:int) =
  if n_1009 < 0 then
    fun (x_1123:int) -> (false, 0)
  else
    let cons_1117 (x_1113:int) (xs_1114:(int -> (bool * int))) =
      let x_1592 (i_1112:int) =
        if i_1112 = 0 then
          (true, x_1113)
        else
          let x_4081 = xs_1114 (i_1112 - 1) in
          let xs_1525 (n_1526:int) = if n_1526 = i_1112 - 1 then
                                       x_4081
                                     else
                                       xs_1114 n_1526 in
          x_4081
      in
      let x_1732 (ii_3523:((bool * int) * (bool * int))) =
        if fst (fst ii_3523) = false then
          let x_4157 =
            if fst (snd ii_3523) = false then
              (false, (true, 0))
            else
              let x_4163 = xs_1114 (snd (snd ii_3523)) in
              (true, x_4163)
          in
          ((false, (true, 0)), x_4157)
        else
          if fst (snd ii_3523) = false then
            let x_4122 = x_1592 (snd (fst ii_3523)) in
            ((true, x_4122), (false, (true, 0)))
          else
            let x_4099 = x_1592 (snd (fst ii_3523)) in
            let x_4109 = xs_1114 (snd (snd ii_3523)) in
            ((true, x_4099), (true, x_4109))
      in
      x_1732
    in
    let x_4194 = make_list_1008 (n_1009 - 1) in
    let x_4196 = rand_int () in
    let x_4197 = cons_1117 x_4196 in
    let x_4198 = x_4197 x_4194 in
    let x_1739 (i_3447:int) = let x_4215 = x_4198 ((true, i_3447), (false, 0)) in
                              snd (fst x_4215) in
    let x_1740 (i_3440:int) = let x_4234 = x_4198 ((false, 0), (true, i_3440)) in
                              snd (snd x_4234) in
    x_1739

let rec append_1059 (x_1023:(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int))))) =
  let x_1746 (i_3310:int) = let x_4260 = x_1023 ((true, i_3310), (false, 0)) in
                            snd (fst x_4260) in
  let x_1747 (i_3303:int) = let x_4279 = x_1023 ((false, 0), (true, i_3303)) in
                            snd (snd x_4279) in
  let rec x_x_x_4007 (x_3968:int) (x_3969:int) (x_3970:int) =
    let x_4293 = x_1023 ((false, 0), (true, x_3968)) in
    let x_4307 = x_1023 ((true, x_3969), (false, 0)) in
    let x_4321 = x_1023 ((false, 0), (true, x_3970)) in
    (snd (snd x_4293), snd (fst x_4307), snd (snd x_4321))
  in
  let x_4343 = x_1023 ((true, 0), (false, 0)) in
  if fst (snd (fst x_4343)) = false then
    let x_1811 (iii_3257:((bool * int) * (bool * int) * (bool * int))) =
      if fst (fst3 iii_3257) = false then
        let x_5559 =
          if fst (snd3 iii_3257) = false then
            (false, (true, 0))
          else
            let x_5565 = x_1746 (snd (snd3 iii_3257)) in
            (true, x_5565)
        in
        let x_5585 =
          if fst (trd iii_3257) = false then
            (false, (true, 0))
          else
            let x_5591 = x_1747 (snd (trd iii_3257)) in
            (true, x_5591)
        in
        ((false, (true, 0)), x_5559, x_5585)
      else
        if fst (snd3 iii_3257) = false then
          let x_5497 = x_1747 (snd (fst3 iii_3257)) in
          let x_5512 =
            if fst (trd iii_3257) = false then
              (false, (true, 0))
            else
              let x_5518 = x_1747 (snd (trd iii_3257)) in
              (true, x_5518)
          in
          ((true, x_5497), (false, (true, 0)), x_5512)
        else
          if fst (trd iii_3257) = false then
            let x_5456 = x_1747 (snd (fst3 iii_3257)) in
            let x_5466 = x_1746 (snd (snd3 iii_3257)) in
            ((true, x_5456), (true, x_5466), (false, (true, 0)))
          else
            let x_5424 = x_x_x_4007 (snd (fst3 iii_3257)) (snd (snd3 iii_3257)) (snd (trd iii_3257)) in
            ((true, fst3 x_5424), (true, snd3 x_5424), (true, trd x_5424))
    in
    x_1811
  else
    if fst (snd (fst x_4343)) <> false then
      let xs'_1014 (x_1150:int) = let x_4572 = x_1023 ((true, x_1150 + 1), (false, 0)) in
                                  snd (fst x_4572) in
      let rec xs'_x_3843 (x_3817:int) (x_3818:int) =
        let x_4587 = x_1023 ((true, x_3817 + 1), (false, 0)) in
        let x_4602 = x_1023 ((false, 0), (true, x_3818)) in
        (snd (fst x_4587), snd (snd x_4602))
      in
      let cons_1225 (x_1221:int) (xs_1222:(int -> (bool * int))) =
        let x_1625 (i_1220:int) =
          if i_1220 = 0 then
            (true, x_1221)
          else
            let x_4613 = xs_1222 (i_1220 - 1) in
            let xs_1543 (n_1544:int) = if n_1544 = i_1220 - 1 then
                                         x_4613
                                       else
                                         xs_1222 n_1544 in
            x_4613
        in
        let x_1785 (ii_3086:((bool * int) * (bool * int))) =
          if fst (fst ii_3086) = false then
            let x_4689 =
              if fst (snd ii_3086) = false then
                (false, (true, 0))
              else
                let x_4695 = xs_1222 (snd (snd ii_3086)) in
                (true, x_4695)
            in
            ((false, (true, 0)), x_4689)
          else
            if fst (snd ii_3086) = false then
              let x_4654 = x_1625 (snd (fst ii_3086)) in
              ((true, x_4654), (false, (true, 0)))
            else
              let x_4631 = x_1625 (snd (fst ii_3086)) in
              let x_4641 = xs_1222 (snd (snd ii_3086)) in
              ((true, x_4631), (true, x_4641))
        in
        x_1785
      in
      let x_1788 (ii_3004:((bool * int) * (bool * int))) =
        if fst (fst ii_3004) = false then
          let x_4789 =
            if fst (snd ii_3004) = false then
              (false, (true, 0))
            else
              let x_4795 = x_1747 (snd (snd ii_3004)) in
              (true, x_4795)
          in
          ((false, (true, 0)), x_4789)
        else
          if fst (snd ii_3004) = false then
            let x_4754 = xs'_1014 (snd (fst ii_3004)) in
            ((true, x_4754), (false, (true, 0)))
          else
            let x_4730 = xs'_x_3843 (snd (fst ii_3004)) (snd (snd ii_3004)) in
            ((true, fst x_4730), (true, snd x_4730))
      in
      let x_1789 (i_2984:int) = let x_4840 = x_1788 ((true, i_2984), (false, 0)) in
                                snd (fst x_4840) in
      let x_1790 (i_2977:int) = let x_4859 = x_1788 ((false, 0), (true, i_2977)) in
                                snd (snd x_4859) in
      let x_4862 = append_1059 x_1788 in
      let x_1792 (i_2966:int) = let x_4886 = x_4862 ((true, i_2966), (false, 0), (false, 0)) in
                                snd (fst3 x_4886) in
      let x_1793 (i_2956:int) = let x_4912 = x_4862 ((false, 0), (true, i_2956), (false, 0)) in
                                snd (snd3 x_4912) in
      let x_1794 (i_2946:int) = let x_4938 = x_4862 ((false, 0), (false, 0), (true, i_2946)) in
                                snd (trd x_4938) in
      let rec x_x_3895 (x_3857:int) (x_3858:int) =
        let x_4956 = x_4862 ((false, 0), (true, x_3857), (false, 0)) in
        let x_4974 = x_4862 ((false, 0), (false, 0), (true, x_3858)) in
        let x_6098 = x_4862 ((false, 0), (true, x_3857), (true, x_3858)) in
        (snd (snd3 x_6098), snd (trd x_6098))
      in
      let x_1797 (ii_2929:((bool * int) * (bool * int))) =
        if fst (fst ii_2929) = false then
          let x_5045 =
            if fst (snd ii_2929) = false then
              (false, (true, 0))
            else
              let x_5051 = x_1794 (snd (snd ii_2929)) in
              (true, x_5051)
          in
          ((false, (true, 0)), x_5045)
        else
          if fst (snd ii_2929) = false then
            let x_5010 = x_1793 (snd (fst ii_2929)) in
            ((true, x_5010), (false, (true, 0)))
          else
            let x_4986 = x_x_3895 (snd (fst ii_2929)) (snd (snd ii_2929)) in
            ((true, fst x_4986), (true, snd x_4986))
      in
      let x_1798 (i_2909:int) = let x_5096 = x_1797 ((true, i_2909), (false, 0)) in
                                snd (fst x_5096) in
      let x_1799 (i_2902:int) = let x_5115 = x_1797 ((false, 0), (true, i_2902)) in
                                snd (snd x_5115) in
      let x_5119 = cons_1225 (snd (snd (fst x_4343))) in
      let x_5120 = x_5119 x_1792 in
      let x_1802 (i_2893:int) = let x_5137 = x_5120 ((true, i_2893), (false, 0)) in
                                snd (fst x_5137) in
      let rec x_x_x_3948 (x_3909:int) (x_3910:int) (x_3911:int) =
        let x_5151 = x_5120 ((true, x_3909), (false, 0)) in
        let x_5165 = x_1023 ((true, x_3910), (false, 0)) in
        let x_5179 = x_1023 ((false, 0), (true, x_3911)) in
        (snd (fst x_5151), snd (fst x_5165), snd (snd x_5179))
      in
      let x_1803 (i_2886:int) = let x_5202 = x_5120 ((false, 0), (true, i_2886)) in
                                snd (snd x_5202) in
      let x_1807 (iii_2861:((bool * int) * (bool * int) * (bool * int))) =
        if fst (fst3 iii_2861) = false then
          let x_5349 =
            if fst (snd3 iii_2861) = false then
              (false, (true, 0))
            else
              let x_5355 = x_1746 (snd (snd3 iii_2861)) in
              (true, x_5355)
          in
          let x_5375 =
            if fst (trd iii_2861) = false then
              (false, (true, 0))
            else
              let x_5381 = x_1747 (snd (trd iii_2861)) in
              (true, x_5381)
          in
          ((false, (true, 0)), x_5349, x_5375)
        else
          if fst (snd3 iii_2861) = false then
            let x_5287 = x_1802 (snd (fst3 iii_2861)) in
            let x_5302 =
              if fst (trd iii_2861) = false then
                (false, (true, 0))
              else
                let x_5308 = x_1747 (snd (trd iii_2861)) in
                (true, x_5308)
            in
            ((true, x_5287), (false, (true, 0)), x_5302)
          else
            if fst (trd iii_2861) = false then
              let x_5246 = x_1802 (snd (fst3 iii_2861)) in
              let x_5256 = x_1746 (snd (snd3 iii_2861)) in
              ((true, x_5246), (true, x_5256), (false, (true, 0)))
            else
              let x_5214 = x_x_x_3948 (snd (fst3 iii_2861)) (snd (snd3 iii_2861)) (snd (trd iii_2861)) in
              ((true, fst3 x_5214), (true, snd3 x_5214), (true, trd x_5214))
      in
      x_1807
    else
      let rec loop () = loop () in loop ()

let main_1015 (i_1016:int) (n_1017:int) =
  let x_5625 = make_list_1008 n_1017 in
  let f_1479 (x_1329:int) = (false, 0) in
  let x_1820 (ix_2198:((bool * int) * (bool * int))) =
    if fst (fst ix_2198) = false then
      let x_5695 =
        if fst (snd ix_2198) = false then
          (false, (true, 0))
        else
          let x_5701 = f_1479 (snd (snd ix_2198)) in
          (true, x_5701)
      in
      ((false, (true, 0)), x_5695)
    else
      if fst (snd ix_2198) = false then
        let x_5660 = x_5625 (snd (fst ix_2198)) in
        ((true, x_5660), (false, (true, 0)))
      else
        let x_5637 = x_5625 (snd (fst ix_2198)) in
        let x_5647 = f_1479 (snd (snd ix_2198)) in
        ((true, x_5637), (true, x_5647))
  in
  let x_1821 (i_2178:int) = let x_5746 = x_1820 ((true, i_2178), (false, 0)) in
                            snd (fst x_5746) in
  let x_1822 (x_2171:int) = let x_5765 = x_1820 ((false, 0), (true, x_2171)) in
                            snd (snd x_5765) in
  let x_5768 = append_1059 x_1820 in
  let x_1824 (i_2160:int) = let x_5792 = x_5768 ((true, i_2160), (false, 0), (false, 0)) in
                            snd (fst3 x_5792) in
  let x_1825 (i_2150:int) = let x_5818 = x_5768 ((false, 0), (true, i_2150), (false, 0)) in
                            snd (snd3 x_5818) in
  let x_1826 (i_2140:int) = let x_5844 = x_5768 ((false, 0), (false, 0), (true, i_2140)) in
                            snd (trd x_5844) in
  let rec x_x_4065 (x_4027:int) (x_4028:int) =
    let x_5862 = x_5768 ((false, 0), (true, x_4027), (false, 0)) in
    let x_5880 = x_5768 ((false, 0), (false, 0), (true, x_4028)) in
    let x_6069 = x_5768 ((false, 0), (true, x_4027), (true, x_4028)) in
    (snd (snd3 x_6069), snd (trd x_6069))
  in
  let x_1829 (ii_2123:((bool * int) * (bool * int))) =
    if fst (fst ii_2123) = false then
      let x_5951 =
        if fst (snd ii_2123) = false then
          (false, (true, 0))
        else
          let x_5957 = x_1826 (snd (snd ii_2123)) in
          (true, x_5957)
      in
      ((false, (true, 0)), x_5951)
    else
      if fst (snd ii_2123) = false then
        let x_5916 = x_1825 (snd (fst ii_2123)) in
        ((true, x_5916), (false, (true, 0)))
      else
        let x_5892 = x_x_4065 (snd (fst ii_2123)) (snd (snd ii_2123)) in
        ((true, fst x_5892), (true, snd x_5892))
  in
  let x_1830 (i_2103:int) = let x_6002 = x_1829 ((true, i_2103), (false, 0)) in
                            snd (fst x_6002) in
  let x_1831 (i_2096:int) = let x_6021 = x_1829 ((false, 0), (true, i_2096)) in
                            snd (snd x_6021) in
  let x_6045 = x_5768 ((true, i_1016), (false, 0), (false, 0)) in
  let x_6046 = x_5625 i_1016 in
  if fst (snd (fst3 x_6045)) <> false then
    let n_1504 = snd (snd (fst3 x_6045)) in
    if fst x_6046 <> false then
      let n_1505 = snd x_6046 in
      assert (n_1504 = n_1505)
