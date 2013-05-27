open BRA_util
open Type
open Syntax
open BRA_types
open BRA_state

(***** Constants *****)

let hole_term = make_var (Id.new_var "__HOLE__" TBool)

(***** Functions *****)

(* apply a transformation throughout an AST in bottom-up manner *)
let rec everywhere_expr f {desc = desc; typ = typ} =
  let ev = everywhere_expr f in
  let expr =
    begin
      match desc with
	| App (func, args) -> App (ev func, List.map ev args)
	| If (cond_expr, then_expr, else_expr) -> If (ev cond_expr, ev then_expr, ev else_expr)
	| Let (flag, bindings, e) ->
	  let fmap (ident, args, body) = (ident, args, ev body) in
	  Let (flag, List.map fmap bindings, ev e)
	| BinOp (op, e1, e2) -> BinOp (op, ev e1, ev e2)
	| Not e -> Not (ev e)
	| e -> e
    end
  in f { desc = expr
       ; typ = typ }

(* regularization of program form *)
let rec regularization = function
  | {desc = Let (Nonrecursive, [top_id, _, body], {desc = Unit; typ = TUnit})}
      -> body
  | t -> t
(*
and (set_main : Syntax.typed_term -> Syntax.typed_term) = function
  | {desc = Let (rec_flag, args, body); typ = typ} ->
    {desc = Let (rec_flag, args, set_main body); typ = typ}
  | {typ = typ} as t ->
    let main_id = Id.new_var "main" typ in
    let main_var = make_var main_id in
    {t with desc = Let (Nonrecursive, [main_id, [Id.new_var "_" TUnit], t],
			{desc = App (main_var, [{desc = Unit; typ = TUnit}]); typ = typ})}
*)

(* conversion to parse-able string *)
let parens s = "(" ^ s ^ ")"
let rec show_typed_term t = show_term t.desc
and show_term = function
  | Unit -> "()"
  | True -> "true"
  | False -> "false"
  | Int n -> string_of_int n
  | App ({desc=RandInt _}, _) -> "Random.int 0"
  | Var v -> v.Id.name
  | Fun (f, body) -> "fun " ^ f.Id.name ^ " -> " ^ show_typed_term body
  | App ({desc=Event("fail", _)}, _) -> "assert false"
  | App (f, args) -> show_typed_term f ^ List.fold_left (fun acc a -> acc ^ " " ^ parens (show_typed_term a)) "" args
  | If (t1, t2, t3) -> "if " ^ show_typed_term t1 ^ " then " ^ show_typed_term t2 ^ " else " ^ show_typed_term t3
  | Let (_, [], _) -> assert false
  | Let (rec_flag, b::bs, t) ->
    let show_bind (x, args, body) =
      x.Id.name
      ^ (List.fold_left (fun acc a -> acc ^ " " ^ a.Id.name) "" args)
      ^ "="
      ^ show_typed_term body in
    (if rec_flag = Nonrecursive then "let " else "let rec ")
    ^ show_bind b
    ^ List.fold_left (fun acc x -> acc ^ " and " ^ show_bind x) "" bs
    ^ " in "
    ^ show_typed_term t
  | BinOp (binop, t1, t2) -> parens (show_typed_term t1) ^ show_binop binop ^ parens (show_typed_term t2)
  | Not t -> "not " ^ parens (show_typed_term t)
  | t -> raise (Invalid_argument "show_term")
and show_binop = function
  | Eq -> "="
  | Lt -> "<"
  | Gt -> ">"
  | Leq -> "<="
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"

let retyping t =
  let lb = t |> show_typed_term
             |> Lexing.from_string
  in
  let () = lb.Lexing.lex_curr_p <-
    {Lexing.pos_fname = Filename.basename !Flag.filename;
     Lexing.pos_lnum = 1;
     Lexing.pos_cnum = 0;
     Lexing.pos_bol = 0};
  in
  let orig = Parse.use_file lb in
  let parsed = Parser_wrapper.from_use_file orig in
  let _ =
    if true && !Flag.debug_level > 0
    then Format.printf "transformed::@. @[%a@.@." Syntax.pp_print_term parsed
  in
  (orig, parsed)

let extract_functions (target_program : typed_term) =
  let ext acc (id, args, body) = if args = [] then acc else {id=id; args=args}::acc in
  let rec iter t =
    match t.desc with
      | Let (_, bindings, body) -> List.fold_left ext [] bindings @ iter body
      | t -> []
  in
  let extracted = iter (regularization target_program) in
  extracted

let rec transform_function_definitions f term =
  let sub ((_, args, _) as binding) = if args <> [] then f binding else binding in
  match term with 
    | {desc = Let (rec_flag, bindings, cont)} as t -> { t with desc = Let (rec_flag, List.map sub bindings, transform_function_definitions f cont) }
    | t -> t

let rec transform_main_expr f = function
  | {desc = Let (rec_flag, bindings, body)} as t -> { t with desc = Let (rec_flag, bindings, transform_main_expr f body) }
  | t -> everywhere_expr f t

let extract_id = function
  | {desc = (Var v)} -> v
  | _ -> assert false

let implement_recieving ({program = program; state = state} as holed) =
  let passed = passed_statevars holed in
  let placeholders f = List.map (fun v -> Id.new_var "_" (Id.typ (extract_id v))) (passed f) in (* (expl) placeholders 4 = " _ _ _ _ " *)
  let rec set_state f = function
    | [] -> []
    | [arg] -> (List.map extract_id (passed f))@[arg]
    | arg::args -> (placeholders f)@[arg]@(set_state f) args
  in
  { holed with program = transform_function_definitions (fun (id, args, body) -> (id, set_state id args, body)) program }

let implement_transform_initial_application ({program = program; state = state} as holed) =
  let sub = function
    | {desc = App (func, args)} as t -> {t with desc = App (func, concat_map (fun arg -> state.BRA_types.initial_state@[arg]) args)}
    | t -> t
  in
  { holed with program = transform_main_expr sub program }

let implement_propagation ({program = program; state = state} as holed) =
  let propagated = propagated_statevars holed in
  let sub = function
    | {desc = App (func, args)} as t -> {t with desc = App (func, concat_map (fun arg -> propagated@[arg]) args)}
    | t -> t
  in
  { holed with program = transform_function_definitions (fun (id, args, body) -> (id, args, everywhere_expr sub body)) program }

let transform_program_by_call holed =
  holed |> implement_recieving
        |> implement_transform_initial_application
        |> implement_propagation

(* restore type *)
let restore_type state = function
  | {desc = Var v; typ = t} as e ->
    let rec restore_type' acc i = function
      | TFun ({Id.typ = t1}, t2) as t ->
	let fresh_id = Id.new_var ("d_"^v.Id.name^(string_of_int i)) t1 in
	{ desc = Fun (fresh_id
			,(restore_type'
			    { desc = App (acc, (state.initial_state@[make_var fresh_id]))
			    ; typ = t}
			    (i+1)
			    t2))
	; typ = t}
      | t -> acc
    in restore_type' e 0 t
  | _ -> raise (Invalid_argument "restore_type")

let to_holed_programs (target_program : typed_term) (defined_functions : function_info list) =
  let state_template = build_state defined_functions in
  let hole_insert target state typed =
    let sub (id, args, body) =
      let body' =
	if id = target.id then
	  let prev_set_flag = get_prev_set_flag state target in
	  let set_flag = get_set_flag state target in
	  let update_flag = get_update_flag state target in
	  let prev_statevars = get_prev_statevars state target in
	  let statevars = get_statevars state target in
	  let argvars = get_argvars state target in
	  let add_update_statement cont prev_statevar statevar argvar =
	    make_let [extract_id statevar, [], make_if update_flag (restore_type state argvar) prev_statevar] cont
	  in
	  make_let
	    [Id.new_var "_" TUnit, [], make_if prev_set_flag (make_if hole_term unit_term (make_app fail_term [unit_term])) unit_term]
	    (make_let
	       [(extract_id update_flag, [], randbool_unit_term)]
	       (make_let
		  [(extract_id set_flag, [], make_or update_flag prev_set_flag)]
		  (fold_left3 add_update_statement 
		     body prev_statevars statevars argvars)))
	else body
      in (id, args, body')
    in
    { typed with desc = match typed.desc with
      | Let (rec_flag, bindings, body) -> Let (rec_flag, List.map sub bindings, body)
      | t -> t
    }
  in
  let hole_inserted_programs = 
    List.map (fun f -> 
      let f_state = state_template f in
      { program = everywhere_expr (hole_insert f f_state) target_program
      ; verified = f
      ; state = f_state}) defined_functions
  in
  let state_inserted_programs = 
    List.map transform_program_by_call hole_inserted_programs
  in state_inserted_programs

let construct_LLRF {variables = variables_; prev_variables = prev_variables_; coefficients = coefficients_} =
  let variables = (List.map make_var variables_) @ [make_int 1] in
  let prev_variables = (List.map make_var prev_variables_) @ [make_int 1] in
  let coefficients = List.map (List.map make_int) coefficients_ in
  let rec rank vs cs = try List.fold_left2
			     (fun rk t1 t2 -> make_add rk (make_mul t1 t2))
			     (make_mul (List.hd vs) (List.hd cs))
			     (List.tl vs)
			     (List.tl cs)
    with Invalid_argument _ -> raise (Invalid_argument "construct_LLRF")
  in
  let rec iter addition = function
    | [c] ->
      addition (make_and (make_gt (rank prev_variables c) (rank variables c))
		  (make_geq (rank variables c) (make_int 0)))
    | c::cs ->
      make_or
	(addition (make_and (make_gt (rank prev_variables c) (rank variables c))
		     (make_geq (rank variables c) (make_int 0))))
	(iter (fun t -> make_and (make_eq (rank prev_variables c) (rank variables c)) t) cs)
    | [] -> false_term
  in
  iter (fun t -> t) coefficients

(* plug holed program with predicate *)
let pluging (holed_program : holed_program) (predicate : typed_term) =
  let hole2pred = function
    | {desc = Var {Id.name = "__HOLE__"}} -> predicate
    | t -> t
  in everywhere_expr hole2pred holed_program.program
