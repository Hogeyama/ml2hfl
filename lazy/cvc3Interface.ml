open ExtList
open ExtString

let cvc3in = ref stdin
let cvc3out = ref stdout

let open_cvc3 () =
  let cin, cout = Unix.open_process (Flag.cvc3 ^ " +int") in
  cvc3in := cin;
  cvc3out := cout

let close_cvc3 () =
  match Unix.close_process (!cvc3in, !cvc3out) with
    Unix.WEXITED(_) | Unix.WSIGNALED(_) | Unix.WSTOPPED(_) -> ()

let string_of_type ty =
  match ty with
    SimType.Unit -> "INT"
  | SimType.Bool -> "BOOLEAN"
  | SimType.Int -> "INT"
  | SimType.Fun(_, _) -> assert false

let string_of_env env =
  String.concat ", "
    (List.map (fun (x, ty) -> Var.string_of x ^ ":" ^ string_of_type ty) env)

let rec string_of_term t =
  match Term.fun_args t with
    Term.Var(_, x), [] ->
      Var.string_of x
  | Term.Const(_, Const.Int(n)), [] ->
      string_of_int n
  | Term.Const(_, Const.Add), [t1; t2] ->
      "(" ^ string_of_term t1 ^ " + " ^ string_of_term t2 ^ ")"
  | Term.Const(_, Const.Sub), [t1; t2] ->
      "(" ^ string_of_term t1 ^ " - " ^ string_of_term t2 ^ ")"
  | Term.Const(_, Const.Mul), [Term.Const(_, Const.Int(m)); t]
  | Term.Const(_, Const.Mul), [t; Term.Const(_, Const.Int(m))] ->
      "(" ^ string_of_int m ^ " * " ^ string_of_term t ^ ")"
  | Term.Const(_, Const.Minus), [t] ->
      "(- " ^ string_of_term t ^ ")"
  | Term.Const(_, Const.Leq), [t1; t2] ->
      "(" ^ string_of_term t1 ^ " <= " ^ string_of_term t2 ^ ")"
  | Term.Const(_, Const.Geq), [t1; t2] ->
      "(" ^ string_of_term t1 ^ " >= " ^ string_of_term t2 ^ ")"
  | Term.Const(_, Const.Lt), [t1; t2] ->
      "(" ^ string_of_term t1 ^ " < " ^ string_of_term t2 ^ ")"
  | Term.Const(_, Const.Gt), [t1; t2] ->
      "(" ^ string_of_term t1 ^ " > " ^ string_of_term t2 ^ ")"
  | Term.Const(_, Const.Eq), [t1; t2] ->
      "(" ^ string_of_term t1 ^ " = " ^ string_of_term t2 ^ ")"
    (*
      "(" ^ string_of_term t1 ^ " <=> " ^ string_of_term t2 ^ ")"
       *)
  | Term.Const(_, Const.Neq), [t1; t2] ->
      string_of_term (Term.bnot (Term.eq t1 t2))
  | Term.Const(_, Const.Unit), [] ->
      "0"(*"UNIT"*)
  | Term.Const(_, Const.True), [] ->
      "TRUE"
  | Term.Const(_, Const.False), [] ->
      "FALSE"
  | Term.Const(_, Const.And), [t1; t2] ->
      "(" ^ string_of_term t1 ^ " AND " ^ string_of_term t2 ^ ")"
  | Term.Const(_, Const.Or), [t1; t2] ->
      "(" ^ string_of_term t1 ^ " OR " ^ string_of_term t2 ^ ")"
  | Term.Const(_, Const.Imply), [t1; t2] ->
      "(" ^ string_of_term t1 ^ " => " ^ string_of_term t2 ^ ")"
    (*???string_of_term (Term.bor [Term.bnot t1; t2])*)
  | Term.Const(_, Const.Iff), [t1; t2] ->
      "(" ^ string_of_term t1 ^ " <=> " ^ string_of_term t2 ^ ")"
  | Term.Const(_, Const.Not), [t] -> 
      "(NOT " ^ string_of_term t ^ ")"
  | Term.Forall(_, xs, t), [] ->
      let env = List.map (fun x -> x, SimType.Int) xs in
      "(FORALL (" ^ string_of_env env ^ "): " ^ string_of_term t ^ ")"
  | _, _ ->
      let _ = Format.printf "%a@." Term.pr t in
      assert false

let is_valid t =
  let cin = !cvc3in in
  let cout = !cvc3out in
  let fm = Format.formatter_of_out_channel cout in

  let env = List.map (fun x -> x, SimType.Int) (Term.fvs t) in
  let inp =
    "PUSH;" ^
    string_of_env env ^ ";" ^
    String.concat " "
      (List.map (fun t -> "ASSERT " ^ (string_of_term t) ^ "; ") []) ^
    "QUERY " ^ string_of_term t ^ ";" ^
    "POP;\n"
  in
  let _ = Format.printf "input to cvc3: %s@ " inp in
  let _ = Format.fprintf fm "%s@?" inp in
  let res = input_line cin in
  if Str.string_match (Str.regexp ".*Valid") res 0 then
    true
  else if Str.string_match (Str.regexp ".*Invalid") res 0 then
    false
  else
    let _ = Format.printf "unknown error of CVC3: %s@ " res in
    assert false

(*
let checksat env p =
  let cin = !cvc3in in
  let cout = !cvc3out in
  let fm = Format.formatter_of_out_channel cout in

  let types = List.fold_left (fun str (x,_) -> str ^ x ^ ":" ^ string_of_typ env x ^ "; ") "" env in
  let query = "CHECKSAT " ^ string_of_term env p ^ ";" in

  let q = "PUSH;"^types^query^"\nPOP;" in
  let _ = if Flag.debug && Flag.print_cvc3 then Format.fprintf Format.std_formatter "checksat: %s@." q in

  let () = Format.fprintf fm "%s@?" q in
  let s = input_line cin in
    if Str.string_match (Str.regexp ".*Satisfiable") s 0 then
      true
    else if Str.string_match (Str.regexp ".*Unsatisfiable") s 0 then
      false
    else begin
      Format.printf "CVC3 reported an error@."; assert false
    end
*)