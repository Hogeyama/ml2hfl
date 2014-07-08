open Syntax
open Type
open Term_util

let origWithExparam = ref (make_int 0)
let exCoefficients = ref []

let to_string_CoeffInfos f =
  let g {desc = Var v} = Id.name v in
  let h = function
    | {desc = Const (Int n)} -> string_of_int n
    | _ -> raise (Invalid_argument "")
  in
  let isZero = function
    | {desc=Const (Int n)} -> n = 0
    | {desc=Var v} -> CEGAR_syntax.isEX_COEFFS v.Id.name
    | t -> (Format.printf "%a@." print_term t;raise (Invalid_argument ""))
  in
  let areAllZero = List.for_all isZero (List.map f !exCoefficients) in
  try
    if areAllZero then "" else
      List.fold_left2
	(fun s c v -> s ^ "\t" ^ g c ^ " = " ^ h v ^ "\n")
	""
	(List.rev !exCoefficients)
	(List.rev_map f !exCoefficients)
  with _ -> ""

let withExparam = ref (make_int 0)

let rec transType = function
  | TFun ({Id.name=t1Name; Id.typ=t1} as t1Id, t2) when is_fun_typ t1 ->
    let t1 = transType t1 in
    TFun (Id.new_var ~name:(t1Name^"_EXPARAM") TInt, TFun ({t1Id with Id.typ = t1}, transType t2))
  | TFun (t1, t2) -> TFun (t1, transType t2)
  | t -> t

let counter = ref 0
let nthCoefficient = ref []

let freshCoefficient () =
  let _ = counter := !counter + 1 in
  let freshName = "c" ^ string_of_int (!counter - 1) ^ "_COEFFICIENT" in
  let freshId = Id.new_var ~name:freshName TInt in
  let _ = nthCoefficient := !nthCoefficient @ [freshId] in
  let freshCoeff = make_var freshId in
  (exCoefficients := freshCoeff :: !exCoefficients; freshCoeff)

let rec makeTemplate = function
  | [] -> freshCoefficient ()
  | x :: xs ->
    let term = make_mul (freshCoefficient ()) (make_var x) in
    make_add term (makeTemplate xs)

let rec insertExparam scope expr =
  match expr.desc with
    | Const _
    | RandInt _
    | RandValue _ -> expr
    | Var v ->
      let typ = transType v.Id.typ in
      {desc = Var {v with Id.typ = typ}; typ = typ}
    | Fun (x, e) -> assert false (* ? *)
    | App (f, args) ->
      let insertToArgs = function
	| t when is_base_typ t.typ -> [insertExparam scope t]
	| t -> [makeTemplate scope; insertExparam scope t]
      in
      { expr with
	desc = App (insertExparam scope f, BRA_util.concat_map insertToArgs args)}
    | If (predicate, thenClause, elseClause) ->
      { expr with
	desc = If ((insertExparam scope predicate),
		   (insertExparam scope thenClause),
		   (insertExparam scope elseClause))}
    | Branch (_, _) -> assert false (* ? *)
    | Let (flag, bindings, e) ->
      let rec extend sc = function
	| [] -> sc
	| (x, [], body) :: bs when (Id.typ x) = TInt -> extend (x :: sc) bs
	| _ :: bs -> extend sc bs
      in
      let scope =
	if flag = Nonrecursive then scope else extend scope bindings
      in
      let insertExparamBinding (x, args, body) =
	let insertExparamArgs (sc, ags) = function
	  | t when Id.typ t = TInt -> (t::sc, ags@[t])
	  | t when is_base_typ (Id.typ t) -> (sc, ags@[t])
	  | t when is_fun_typ (Id.typ t) ->
	    let t_exparamId = Id.new_var ~name:((Id.name t) ^ "_EXPARAM") TInt in
	    (t_exparamId::sc, ags@[t_exparamId; {t with Id.typ = transType t.Id.typ}])
	  | _ -> assert false
	in
	let (scope, args) =
	  List.fold_left
	    insertExparamArgs
	    (scope, [])
	    args
	in
	({x with Id.typ = transType x.Id.typ}, args, insertExparam scope body)
      in
      { expr with
	desc = Let (flag, List.map insertExparamBinding bindings, insertExparam (extend scope bindings) e)}
    | BinOp (op, expr1, expr2) ->
      { expr with
	desc = BinOp (op, insertExparam scope expr1, insertExparam scope expr2)}
    | Not e ->
      { expr with
	desc = Not (insertExparam scope e)}
    | _ -> assert false (* unimplemented *)

let rec removeDummySubstitutions = function
  | { desc = Let (Recursive, [id, [], {desc = Const (Int 0)}], e) } -> removeDummySubstitutions e
  | e -> e

let substituteZero e =
  let toZero = function
    | { desc = Var id } when CEGAR_syntax.isEX_COEFFS (Id.name id) -> make_int 0
    | e -> e
  in
  BRA_transform.everywhere_expr toZero e

let initPreprocessForExparam e =
  let e = removeDummySubstitutions e in
  let _ = withExparam := e in
  substituteZero e

let addTemplate prog =
  let _ = counter := 0 in
  let prog = insertExparam [] prog in
  let _ = counter := !counter - 1 in
  let rec dummySubst = function
    | (-1) -> prog
    | n -> make_letrec [(List.nth !nthCoefficient n), [], make_int 0] (dummySubst (n-1))
(*    | n -> make_letrec [(List.nth !nthCoefficient n), [], make_var (List.nth !nthCoefficient n)] (tmp (n-1))*)
  in
  begin
    origWithExparam := prog;
    dummySubst !counter
  end
