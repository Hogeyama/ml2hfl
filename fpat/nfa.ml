open Ttype
(* string_of_vpattern : vpattern -> string *)
let string_of_vpattern = function
  | Any     -> "Any"
  | Var s   -> Printf.sprintf "Var (%s)" s
  | Const c -> Printf.sprintf "Const (%s)" c
(* string_of_epattern : epattern -> string *)
let string_of_epattern = function
  | Call (name, vpat) ->
     Printf.sprintf "Call(\"%s\", %s)" name @@ string_of_vpattern vpat 
  | Ret  (name, vpat) ->
     Printf.sprintf "Ret(\"%s\", %s)" name @@ string_of_vpattern vpat
(* string_of_tcontract : tcontract -> string *)
let string_of_tcontract tcont =
  let rec inner = function
    | Atom epat -> 
       Printf.sprintf "Atom (%s)" @@ string_of_epattern epat
    | Neg epat -> 
       Printf.sprintf "Neg (%s)" @@ string_of_epattern epat
    | Concat (t1, t2) ->
       let str_t1 = inner t1 in
       let str_t2 = inner t2 in
       Printf.sprintf "Concat (%s, %s)" str_t1 str_t2
    | Kleene t1 ->
       Printf.sprintf "Kleene (%s)" @@ inner t1
    | Not t1 ->
       Printf.sprintf "Not (%s)" @@ inner t1
    | Or (t1, t2) ->
       let str_t1 = inner t1 in
       let str_t2 = inner t2 in
       Printf.sprintf "Concat (%s, %s)" str_t1 str_t2 
    | Wilds -> "Wilds"
  in inner tcont

(* nfa_def : *)
let nfa_def () = 
  let inchan = open_in "nfa_def.ml" in
  let structure = 
    Parse.implementation @@ Lexing.from_channel inchan
  in close_in inchan;structure

(* make_make_nfa : tcontract -> structure_item list *)
let make_make_nfa tcont =
  try
    let tcont_str = string_of_tcontract tcont in
    let meta_make_nfa = 
      ("let nfa =\n let r = " 
       ^ tcont_str ^ 
         "\n in make_nfa @@ make_delta @@ tcon2nfa r") 
    in
    Parse.implementation @@ Lexing.from_string meta_make_nfa
  with e ->
    print_string "On Nfa.make_make_nfa'\n";
    raise e
(* make : tcontract -> structure *)
let make tcont =
  try
    let structure_invok = make_make_nfa tcont in
    let structure_body = nfa_def () in
    let rev_body = List.rev structure_body in
    List.append structure_invok rev_body
  with 
    e -> print_string "\n On Nfa.make \n";raise e
