open Util
open Combinator

(** Modal mu-calculus formulas *)

include Formula

let is_alt_free phi = assert false
let is_guarded phi = assert false

(** translate a formula to an equivalent guarded formula *)
let guard phi = assert false

let add p c ord = (c, p) :: ord
(** the return value is not guaranteed to be a partial order
    @require no imply/iff, in negation normal form *)
let rec closure phi p ord =
  Formula.para
    (object
      method fatom atm = fun p ord ->
        ord |> add p (Formula.of_atom atm)
      method ftrue () = fun p ord ->
        ord(* |> add p Formula.mk_true*)
      method ffalse () = fun p ord ->
        ord(* |> add p Formula.mk_false*)
      method fnot phi1 r1 = fun p ord ->
        let phi = Formula.mk_atom Const.Not [phi1 |> Formula.term_of] in
        ord |> add p phi(* |> r1 phi*)
      method fand phi1 r1 phi2 r2 = fun p ord ->
        let phi = Formula.mk_atom Const.And
            [phi1 |> Formula.term_of; phi2 |> Formula.term_of]
        in
        ord |> add p phi |> r1 phi |> r2 phi
      method for_ phi1 r1 phi2 r2 = fun p ord ->
        let phi = Formula.mk_atom Const.Or
            [phi1 |> Formula.term_of; phi2 |> Formula.term_of]
        in
        ord |> add p phi |> r1 phi |> r2 phi
      method fimply phi1 r1 phi2 r2 = fun p ord ->
        assert false
      (*let phi = Formula.imply phi1 phi2 in
        ord |> add p phi |> r1 phi |> r2 phi*)
      method fiff phi1 r1 phi2 r2 = fun p ord ->
        assert false
      (*let phi = Formula.mk_iff phi1 phi2 in
        ord |> add p phi |> r1 phi |> r2 phi*)
      method fforall xty phi1 r1 = fun p ord ->
        assert false
      (*let phi = Formula.forall [xty] phi1 in
        ord |> add p phi |> r1 phi*)
      method fexists xty phi1 r1 = fun p ord ->
        assert false
      (*let phi = Formula.exists [xty] phi1 in
        ord |> add p phi |> r1 phi*)
      method fbox idx phi1 r1 = fun p ord ->
        let phi = Formula.box idx phi1 in (* @todo *)
        ord |> add p phi |> r1 phi
      method fdiamond idx phi1 r1 = fun p ord ->
        let phi = Formula.diamond idx phi1 in (* @todo *)
        ord |> add p phi |> r1 phi
      method fmu x phi1 _ = fun p ord ->
        let phi = Formula.mu x phi1 in (* @todo *)
        let unfolded = Formula.subst [x, phi |> Formula.term_of] phi1 in
        ord |> add p phi
        |> if_ (List.mem_assoc unfolded) id (closure unfolded phi)
      method fnu x phi1 _ = fun p ord ->
        let phi = Formula.nu x phi1 in (* @todo *)
        let unfolded = Formula.subst [x, phi |> Formula.term_of] phi1 in
        ord |> add p phi
        |> if_ (List.mem_assoc unfolded) id (closure unfolded phi)
    end)
    phi p ord
let closure phi = closure phi phi []
