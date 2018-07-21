open Util
open Combinator

(** Type constants *)

(** {6 Constructors} *)

type t =
  (* base types *)
  | Unit
  | Bool
  | Int
  | Real
  | Rational
  | String
  | Ext of string (* external types *)
  | Bot
  | Top
  | Unknown
  (* composed types *)
  | Arrow
  | Tuple of int (*>=2*)
  | Array
  | List
  | Adt of Idnt.t * Idnt.t list
  | Vector of int
  (* intersection and union types *)
  | Inter of int
  | Union of int
  (* refinement types *)
  | Ref (*of t * Idnt.t * Formulat.t*)
  (* abstraction types *)
  | Abs (*of t * Idnt.t * Formulat.t*)
  | RegExp
  | Set

(** {6 Inspectors} *)

let arity_of = function
  | Unit | Bool | Int | Real | Rational | String -> 0
  | Ext(_) -> 0(*@todo*)
  | Bot | Top -> 0
  | Unknown -> 0
		
  | Arrow -> 2
  | Tuple(n) -> n
  | Array -> 1
  | List -> 1
  | Adt(_, _) -> 0
  | Vector(_) -> 1

  | Inter(n) -> n
  | Union(n) -> n
  | Ref -> 2
  | Abs -> 2

let rec string_of = function
  | Unit -> "unit"
  | Bool -> "bool"
  | Int -> "int"
  | Real -> "real"
  | Rational -> "rational"
  | String -> "string"
  | Ext(a) -> a
  | Bot -> "bot"
  | Top -> "top"
  | Unknown -> "unknown"

  | Arrow -> "arrow"
  | Tuple(n) -> "tuple " ^ string_of_int n
  | Array -> "array"
  | List -> "list"
  | Adt(d, cs) ->
     Idnt.string_of d
     (*^ " = "
     ^ String.concat "|" (List.map Idnt.string_of cs)*)
  | Vector(n) -> "vector " ^ string_of_int n

  | Inter(n) -> "inter " ^ string_of_int n
  | Union(n) -> "union " ^ string_of_int n
  | Set -> "set"
  | Ref -> "refine"
  | Abs -> "abst"

let sexp_of = function
  | Unit -> "Unit"
  | Bool -> "Bool"
  | Int -> "Int"
  | Real -> "Real"
  | _ -> assert false

let is_base = function
  | Unit | Bool | Int | Real | Rational | String
  | Array
  | Ext(_)(*@todo?*)
  | Bot | Top
  | Unknown -> true
  | _ -> false

let is_adt = function
  | Adt(_, _) -> true
  | _ -> false

let is_ext = function
  | Ext _ -> true
  | _ -> false

let is_unknown = function
  | Unknown -> true
  | _ -> false
	
let equiv_mod_unknown tyc1 tyc2 =
  tyc1 = tyc2
  || is_unknown tyc1
  || is_unknown tyc2

(** {6 Printers} *)

let pr uprs ppf c =
  match c, uprs with
  | Tuple(_), [] -> (*@todo*)
     Format.fprintf ppf "()"
  | Tuple(_), _ ->
     Format.fprintf
       ppf
       "@[(@[<hov>%a@])@]"
       (Printer.concat_uprs uprs " *@ ") ()
  | Arrow, [upr1; upr2] ->
     Format.fprintf
       ppf
       "@[<hov>%a ->@ %a@]"
       upr1 ()
       upr2 ()
  | _, _ ->
     Printer.concat_uprs_app
       ((Printer.upr_of String.pr (string_of c)) :: uprs)
       "@ "
       ppf
       ()

let pr_tex uprs ppf c =
  match c, uprs with
  | Tuple(_), [] -> (*@todo*)
     Format.fprintf ppf "\\left(\\right)"
  | Tuple(_), _ ->
     Format.fprintf
       ppf
       "@[\\left(@[<hov>%a@]\\right)@]"
       (Printer.concat_uprs uprs " *@ ") ()
  | Arrow, [upr1; upr2] ->
     Format.fprintf
       ppf
       "@[<hov>%a \\rightarrow@ %a@]"
       upr1 ()
       upr2 ()
  | _, _ ->
     Printer.concat_uprs_app
       ((Printer.upr_of String.pr (string_of c)) :: uprs)
       "@ "
       ppf
       ()
