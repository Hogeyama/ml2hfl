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

type predicate_info = { variables : Syntax.id list
		      ; prev_variables : Syntax.id list
		      ; coefficients : int list list
		      }

let updated_predicate_info ({coefficients = old_coeffs} as pr) new_coeffs =
  { pr with coefficients = new_coeffs :: old_coeffs }

type holed_program = { program : Syntax.typed_term
		     ; verified : function_info
		     ; state : state
		     }
