(* Contracts AST *)
type fname = string
type vpattern = Any | Var of string | Const of string
type epattern =
  | Call of fname * vpattern
  | Ret of fname * vpattern
type tcontract =
  | Atom of epattern 
  | Neg of epattern
  | Concat of tcontract * tcontract
  | Kleene of tcontract
  | Not of tcontract
  | Or of tcontract * tcontract
  | Wilds
type flat = FAny | FInt | FBool
type scontract =
  | SFlat of flat (* stab : int *)
  | SFun1 of fname * scontract * scontract
  | SFun2 of scontract * scontract
type scontracts = scontract list
(* S' where R *)
type hotcontract = scontracts * tcontract

(* Automata *)
(* Automata alphabets *)
type alpha = ACall of fname | ARet of fname | Eps
(* Automata states *)
type state = int * bool

(* utility *)
let ( *< ) f g x = f (g x)
let ( *> ) f g x = g (f x)
let unique cmp list =
  let rec inner acc = function
    | [] -> acc
    | l::[] -> l::acc
    | l1::l2::ls ->
       if (cmp l1 l2) = 0 
       then inner acc (l1::ls)
       else inner (l1::acc) (l2::ls)
  in inner [] @@ List.sort cmp list 

(* print functions *)
let print_state (i, b) =
  Printf.printf "(%d, %b)" i b
let print_states states =
  print_string "[";
  ignore @@
    List.map (fun state -> print_state state) states;
  print_string "]\n"

(* make_delta : (alpha -> state -> state list) list 
-> alpha -> state -> state list *)
let make_delta (fs, states) =
  let is_accept state =
    let (a, b) = List.find (fun (a, b) -> a = state) states in b
  in
  let with_acceptable states =
    List.map (fun s -> (s, is_accept s)) states
  in
  fun a s -> List.concat @@ 
               List.map (fun f -> let s' = f a s in with_acceptable s') fs 

(* make_nfa : (alpha -> state -> state list) -> alpha -> bool *)
let make_nfa delta =
  let states = ref [] in
  let eps_close ss =
    let aux ss =
      List.fold_left (fun ss (s, _) -> (delta Eps s) @ ss) ss ss
    in
    let rec inner ss =
      let ss' = 
        unique (fun (i1,_) (i2,_) -> i1 - i2 ) @@ aux ss
      in
      if ss = ss' then ss else inner ss'
    in inner ss
  in
  states := eps_close [(0,false)];(* initialize states. *)
  fun alpha ->
  let states' = 
    eps_close @@ 
      unique (fun (i1,_) (i2,_) -> i1 - i2 ) @@
        List.concat @@ List.rev_map (fun (s, _) -> delta alpha s) !states
  in
  print_states states';
  states := states';
  not @@ List.fold_left (fun a (_,b) -> a || b) false states'
  
(* tcontract -> ((alpha -> state -> state option list) * state) list *)
let tcon2nfa tcont = 
  let binds = ref [(0,false);(1,true)] in
  let turn_bits states =
    let f = fun (n, b) ->
      if List.exists ((=) n) states 
      then (n, not b)
      else (n, b)
    in 
    binds := List.map f (!binds)
  in
  let eq a e =
    match a with
    | ACall n1 ->
       (function | Call (n2, _) -> n1 = n2 | _ -> false ) e
    | ARet n1  ->
       (function | Ret (n2, _) -> n1 = n2 | _ -> false ) e
    | _ -> false
  in
  let free_state flag () =
    let ss' = List.sort (fun (a,_) (b,_) -> b - a) (!binds) 
    in
    match ss' with
    | [] -> failwith "init error."
    | (i, b)::ss -> 
       binds := ((i + 1, flag)::ss');(i + 1)
  in
  (* make_eps : state -> state -> fun *)
  let make_eps s_from s_to =
    fun a s ->
    if s = s_from then
      match a with
      | Eps -> [s_to] | _ -> []
    else []
  in
  (* inner : bool -> delta list -> int -> int -> tcontract -> 
             (int list) *)
  let rec inner flag fs s0 s99 tcont =
    match tcont with
    | Atom e ->
       let f = fun a s -> 
         if (eq a e) && s = s0 then [s99] else []
       in (f::fs)
    | Neg e -> (* NFAをはみ出してるかも? *)
       let f = fun a s ->
         if not (eq a e) && s = s0 then [s99] else []
       in (f::fs)
    | Concat (t1, t2) -> 
       let s_mid1 = free_state flag () in
       let s_mid2 = free_state flag () in
       let f1 = inner flag fs s0 s_mid1 t1 in
       let f2 = inner flag f1 s_mid2 s99 t2 in
       let f_mid = make_eps s_mid1 s_mid2
       in (f_mid::f2)
    | Kleene t ->
       let s_mid1 = free_state flag () in
       let s_mid2 = free_state flag () in
       let f_body = inner flag fs s_mid1 s_mid2 t in
       let f1 = make_eps s0 s_mid1 in
       let f2 = make_eps s_mid2 s99 in
       let f_back = make_eps s_mid2 s_mid1 in
       let f_pass = make_eps s0 s99 in
       (f1::f2::f_back::f_pass::f_body)
    | Not t ->
       inner flag fs s0 s99 t
       (* turn_bits [s0;s99];
       inner (not flag) fs s0 s99 t*)
    | Or (t1, t2) -> failwith "or-stab"
    | Wilds -> 
       let f = fun a s ->
         if s = s0 then [s0;s99] else []
       in (f::fs) 
  in
  let result = inner false [] 0 1 tcont in
  (result, !binds)

(* module test *)
(*
let cont1 = Wilds
let nfa1 = make_nfa @@ make_delta @@ tcon2nfa cont1

let cont2 = Concat (Wilds, Atom (Call("hoge", Any)))
let nfa2 = make_nfa @@ make_delta @@ tcon2nfa cont2

let cont3 = Kleene (Atom (Call("hoge", Any)))
let nfa3 = make_nfa @@ make_delta @@ tcon2nfa cont3

let cont4 = Kleene (Neg (Call("hoge", Any)))
let nfa4 = make_nfa @@ make_delta @@ tcon2nfa cont4

let cont5_5 = Concat (
                  Atom (Call ("hoge", Any)),
                  Concat (
                      Kleene (Neg (Ret ("hoge", Any))),
                      Atom (Call ("hoge", Any))
                    )
                )
let nfa5_5 = make_nfa @@ make_delta @@ tcon2nfa cont5_5

let cont5 = 
  Not (Concat (
           Wilds, 
           Concat (
               Atom (Call ("hoge", Any)),
               Concat (
                   Kleene (Neg (Ret ("hoge", Any))),
                   Atom (Call ("hoge", Any))
                 )
             )
         )
      )
let nfa5 = make_nfa @@ make_delta @@ tcon2nfa cont5
*)
