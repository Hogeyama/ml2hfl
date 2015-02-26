let rec state_0 (lt_15:int) (st_17:int) (y_16:int)= 
  if (lt_15>=y_16) then
    let lt_15 = read_int () in 
    let rt_11 = st_17 in 
    (state_2 (): unit)
  else
      let lt_15 = read_int () in 
        let nd_12 = read_int () in 
          let rv_18 = nd_12 in 
            let nd_12 = read_int () in 
            if ((0)=rv_18) then
              (state_0 (lt_15:int) (st_17:int) (y_16:int): unit)
            else
              let lt_19 = read_int () in 
              (state_0 (lt_15:int) (st_17:int) (y_16:int): unit)


and state_1 (lt_15:int) (st_17:int) (x_13:int) (y_16:int) = 
    let p_14 = x_13 in 
    (state_0 (lt_15:int) (st_17:int) (y_16:int): unit)
and state_2 (u:unit) = 
  ()
let main () =
  let lt_15 = read_int () in
  let st_17 = read_int () in
  let x_13 = read_int () in
  let y_16 = read_int () in
  state_1 (lt_15:int) (st_17:int) (x_13:int) (y_16:int)
