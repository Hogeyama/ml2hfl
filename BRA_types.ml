(***** Types *****)

module InnerState = Map.Make(
  struct
    type t = Syntax.id
    let compare = Id.compare
  end)

type variables_info = { update_flag : Syntax.typed_term
		      ; set_flag : Syntax.typed_term
		      ; prev_set_flag : Syntax.typed_term
                      ; prev_statevars : Syntax.typed_term list
                      ; statevars : Syntax.typed_term list
                      ; argvars : Syntax.typed_term list
	              }

type state = { initial_state : Syntax.typed_term list
             ; statetable : variables_info InnerState.t }

type function_info = { id : Syntax.id
		     ; args : Syntax.id list
		     }

type coefficient_info = { coeffs : int list
			; constant : int
			}

type predicate_info = { variables : Syntax.id list
		      ; prev_variables : Syntax.id list
		      ; error_paths : Fpat.Term.t list
		      ; coefficients : coefficient_info list
		      }

let updated_predicate_info pr new_coeffs new_error_paths =
  { pr with coefficients = new_coeffs; error_paths = new_error_paths }

let pr_ranking_function fm { variables = vs; coefficients = coefficients} =
  let show_ranking_function {coeffs = cs; constant = const} =
    let fold_by acc v c =
      acc ^ (if c = 0 then ""
	else if c = 1 then v.Id.name ^ "+"
	else string_of_int c ^ v.Id.name ^ "+")
    in
    let s = List.fold_left2 fold_by "" vs cs in
    if const = 0 then String.sub s 0 (String.length s - 1)
    else s ^ string_of_int const
  in
  match coefficients with
    | [] -> Format.fprintf fm "0"
    | c::cs -> Format.fprintf fm "%s" (List.fold_left (fun acc c' -> acc ^ ", " ^ show_ranking_function c') (show_ranking_function c) cs)

type holed_program = { program : Syntax.typed_term
		     ; verified : function_info
		     ; state : state
		     }
