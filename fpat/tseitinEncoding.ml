open Util
open Combinator

let l = ref []

let encode =
  let fmake () = Idnt.new_var () |> flip Formula.mk_var [] in
  Formula.fold
    (object
      method fatom atm = Formula.of_atom atm
      method ftrue () = Formula.mk_true
      method ffalse () = Formula.mk_false
      method fnot r =
        let x = fmake () in
        l := Formula.mk_iff x (Formula.bnot r) :: !l;
        x
      method fand r1 r2 =
        let x = fmake () in
        l := Formula.mk_iff x (Formula.mk_and r1 r2) :: !l;
        x
      method for_ r1 r2 =
        let x = fmake () in
        l := Formula.mk_iff x (Formula.mk_or r1 r2) :: !l;
        x
      method fimply r1 r2 =
        let x = fmake () in
        l := Formula.mk_iff x (Formula.imply r1 r2) :: !l;
        x
      method fiff r1 r2 =
        let x = fmake () in
        l := Formula.mk_iff x (Formula.mk_iff r1 r2) :: !l;
        x
      method fforall _ _ =
        raise (Global.NotImplemented "TseitinEncoding.encode")
      method fexists _ _ =
        raise (Global.NotImplemented "TseitinEncoding.encode")
      method fbox idx r1 = assert false
      method fdiamond idx r1 = assert false
      method fmu x r1 = assert false
      method fnu x r1 = assert false
    end)
let encode formula =
  l := [];
  let x = encode formula in
  x :: !l
let encode = Logger.log_block1 "TseitinEncoding.encode" encode
