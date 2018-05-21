open Util
open Combinator

let solver = Osdp.Sdp.Csdp

open Osdp.Lmi.Float

let test1 () =
  let () = Format.printf "LMI@." in
  let p = var "p" 2 in
  let a = << [1.5, -0.7; 1, 0] >>  (* or <:lmi< ... >> *) in
  let e1 = << p - a ' * p * a >> in
  let e2 = << p - eye(2) >> in
  let () = Format.printf "e1 = %a@." pp e1 in
  let () = Format.printf "e2 = %a@." pp e2 in
  let ret, _, vars = solve ~solver Purefeas [e1; e2] in
  let () = Format.printf "%a@." Osdp.SdpRet.pp ret in
  let () = Format.printf "%a@." Mat.pp (value_mat p vars) in
  Format.printf "@."

open Osdp.Sos.Float

let test2 () =
  let () = Format.printf "SOS@." in
  let deg = 4 in
  let p, _ = var_poly "p" 2 ~homogen:true deg in
  let a0 = <:sos< 1.5 x0 - 0.7 x1 >> in
  let a1 = <:sos< x0 >> in
  let e1 = <:sos< p - p(a0, x0) >> in
  (* or let l = [a0; a1] in let e1 = <:sos< p - p($l$) >> *)
  let e2 = <:sos< p - (x0^4 + x1^4) >> in
  let () = Format.printf "e1 = %a@." pp e1 in
  let () = Format.printf "e2 = %a@." pp e2 in
  let ret, _, vars, _ = solve ~solver Purefeas [e1; e2] in
  let () = Format.printf "%a@." Osdp.SdpRet.pp ret in
  Format.printf "%a@." Poly.pp (value_poly p vars)
