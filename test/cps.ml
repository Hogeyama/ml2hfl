let f x =
  let g y = x + y in
    g
in
  if f 0 0 = 0 then () else fail ()

(*
S_63  -> F_44 X_56 0.
Fail_60 k_61 u_62 -> fail.
G_46 x_58 x_49 y_47 -> x_49 (x_58 + y_47).
F_44 x_48 x_45 -> x_48 (G_46 x_45).
X_54 x_55 -> _case 2 (x_55 = 0) (X_50 end) (Fail_60 (X_52 X_50) end).
X_50 x_51 -> x_51.
X_52 x_59 x_53 -> x_59 x_53.
X_56 x_57 -> x_57 X_54 0.
*)
