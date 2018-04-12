open Util

module S = CEGAR_syntax


let parse_atom s len i =
  if len <= i then
    i, None
  else if s.[i] = '|' then
    try
      let j = String.index_from s (i+1) '|' in
      j+1, Some (String.sub s i (j-i+1))
    with Not_found -> invalid_arg "Smtlib2_interface.parse_atom"
  else
    let rec aux acc_rev s len i =
      let return () = Some (String.of_list @@ List.rev acc_rev) in
      if len <= i then
        i, !!return
      else
        match s.[i] with
        | c when Char.is_whitespace c -> i, !!return
        | ')' when acc_rev = [] -> i, None
        | ')' -> i, !!return
        | c -> aux (c::acc_rev) s len (i+1)
    in
    aux [] s len i

let term_of_atom a =
  let open S.Term in
  match a with
  | "true" -> true_
  | "false" -> false_
  | _ ->
      try
        int (int_of_string a)
      with _ -> var a
let binop_of_atom a =
  let open S.Term in
  match a with
  | "=" -> S.make_eq_int
  | "<" -> (<)
  | ">" -> (>)
  | "<=" -> (<=)
  | ">=" -> (>=)
  | "and" -> (&&)
  | "or" -> (||)
  | "+" -> (+)
  | "-" -> (-)
  | _ -> unsupported ("binop_of_atom "^a)
let rec term_of_sexp s =
  let open Sexp in
  match s with
  | A a -> term_of_atom a
  | S [A "not"; s'] -> S.make_not @@ term_of_sexp s'
  | S [A "exists"; s1; s2] ->
      Format.eprintf "WARNING: exists is replaced with false@.";
      S.Term.false_
  | S (A "and" :: ss) -> List.fold_right (S.make_and -| term_of_sexp) ss S.Term.true_
  | S [A "-"; s] -> S.Term.(int 0 - term_of_sexp s)
  | S [A a; s1; s2] when a.[0] <> '|' -> binop_of_atom a (term_of_sexp s1) (term_of_sexp s2)
  | S (A a :: ss) when a.[0] = '|' -> S.make_app (term_of_atom a) @@ List.map term_of_sexp ss
  | _ ->
      Format.eprintf "%a@." print s;
      unsupported "Smtlib2_interface.term_of_sexp"

let type_of_atom a =
  match a with
  | "Int" -> CEGAR_type.typ_int
  | "Bool" -> !!CEGAR_type.typ_bool
  | _ -> unsupported "type_of_atom"

let parse_model s =
  let unsupported () = unsupported "Smtlib2_interface.parse_model" in
  let open Sexp in
  match Sexp.parse ~parse_atom s with
  | [A "sat"; S (A "model" :: sol)] ->
      let aux s =
        match s with
        | S (A "define-fun" :: A id :: S args :: A ty :: body :: []) ->
            let args =
              let aux s =
                match s with
                | S (A x::A ty::[]) -> x, type_of_atom ty
                | _ -> !!unsupported
              in
              List.map aux args
            in
            let body = term_of_sexp body in
            id, (args, body)
        | _ -> !!unsupported
      in
      List.map aux sol
  | _ -> !!unsupported
