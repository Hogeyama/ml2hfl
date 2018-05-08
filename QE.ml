open Util
open FpatInterface
open CEGAR_syntax
open CEGAR_util

module F = Fpat

module Debug = Debug.Make(struct let check = Flag.Debug.make_check __MODULE__ end)

let rec conv_formula' t =
  match t with
  | App(App(Var ("exists"|"forall" as q), args), t1) ->
      let args' =
        match decomp_app args with
        | Var "args", ts -> List.map (function Var x -> F.Idnt.make x, F.Type.mk_int | _ -> invalid_arg "FpatInterface.conv_term") ts
        | _ -> invalid_arg "FpatInterface.conv_term"
      in
      let f = match q with "exists" -> F.Formula.exists | "forall" -> F.Formula.forall | _ -> assert false in
      f args' @@ conv_formula' t1
  | Const(c) ->
      F.Formula.of_term @@ F.Term.mk_const (conv_const c)
  | Var(x) ->
      F.Formula.of_term @@ F.Term.mk_var @@ conv_var x
  | App(t1, t2) -> F.Formula.of_term @@ F.Term.mk_app (F.Formula.term_of @@ conv_formula' t1) [F.Formula.term_of @@ conv_formula' t2]
  | Fun _ -> assert false
  | Let _ -> assert false

let eliminate t =
  let of_formula phi =
    let open Fpat in
    let open Term in
    let open Z3 in
    let phi =
      phi
      |> CunFormula.elim_unit
      |> Formula.map_atom CunAtom.elim_beq_bneq
    in
    let tenv, phi =
      SimTypInfer.infer_formula [] phi
    in
    let ctenv =
      phi
      |> CunFormula.get_adts
      |> Z3Interface.mk_constructors tenv
    in
    F.Z3Interface.of_formula phi ctenv tenv []
  in
  let fv = get_fv t in
  let map = List.map (Pair.add_right @@ String.replace_chars (function '!' -> "_bang_" | c -> String.of_char c)) fv in
  Debug.printf "  map: %a@." Print.(list (pair string string)) map;
  t
  |@> Debug.printf "  BEFORE: %a@." CEGAR_print.term
  |> subst_map @@ List.map (fun (x,y) -> x, Var y) map
  |> conv_formula'
  |@> Debug.printf "  conv_foromula': %a@." F.Formula.pr
  |> of_formula
  |@> Debug.printf "  of_formula: %s@." -| Z3.Expr.to_string
  |> F.Z3Interface.qelim
  |@> Debug.printf "  qelim: %s@." -| Z3.Expr.to_string
  |> F.Z3Interface.formula_of
  |@> Debug.printf "  formula_of: %a@." F.Formula.pr
  |> inv_formula
  |> subst_map @@ List.map (fun (x,y) -> y, Var x) map
  |@> Debug.printf "  AFTER: %a@." CEGAR_print.term
