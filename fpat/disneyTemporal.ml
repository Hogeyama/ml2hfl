open Ttype

(* string2items : string -> Parsetree.structure_item *)
let string2items str =
  let open Parsetree in
  let ast = 
    Parse.implementation @@ 
      Lexing.from_string str 
  in List.map (fun ast -> ast.pstr_desc) ast

(* string2exp : string -> Parsetree.expression *)
let string2exp str =
  let ast =
    Parse.expression @@
      Lexing.from_string str
  in ast

let dummy_loc =
  let open Parsetree in
  let open Location in
  let open Lexing in 
  {loc_start = dummy_pos;loc_end = dummy_pos;loc_ghost = true;}

let make_id l name =
  let open Parsetree in
  let open Location in
  let open Longident in
  (Pexp_ident {txt=(Lident name);loc=l})

let dummy_id = 
  let dummy_loc =
    let open Location in
    let open Lexing in
    {loc_start = dummy_pos;loc_end = dummy_pos;loc_ghost = true;}
  in make_id dummy_loc

let dummy_pat ppat_desc' =
  let open Parsetree in
  let dummy_loc =
    let open Location in
    let open Lexing in
    {loc_start = dummy_pos;loc_end = dummy_pos;loc_ghost = true;}
  in 
  {ppat_desc = ppat_desc';
   ppat_loc = dummy_loc;
   ppat_attributes = [];}
  
(* make_exp : expression_desc -> expression *)
let make_exp exp =
  let open Parsetree in
  let open Location in
  let open Lexing in
  let dummy_loc = 
    {loc_start = dummy_pos;loc_end = dummy_pos;loc_ghost = true;}
  in
  {
    pexp_desc = exp;
    pexp_loc  = dummy_loc;
    pexp_attributes = [];
  }

(* compiler *)
(* compile : Constant \cup {id} -> Bool -> scontract -> monitor *)
let compile_dummy nfa f cont = (fun x -> true, fun () -> None)
(* compile :  expression -> scontruct -> expression *)
let rec compile f cont =
  let open Parsetree in
  let open Location in
  let open Asttypes in
  match cont with
  | SFlat e ->
     let app = 
       make_exp (Pexp_apply (f, [(Nolabel, make_exp @@ dummy_id "x")])) in
     let pat = dummy_pat (Ppat_var {txt = "x"; loc = dummy_loc;}) in
     let cond = 
       make_exp 
         (Pexp_fun (Nolabel, None, pat, app)) (* (fun x -> f x) *)
     in `CMonitor cond
  | SFun1 (name, dom, range) ->
     let pat = dummy_pat (Ppat_var {txt = "x"; loc = dummy_loc;}) in
     let app  = string2exp ("nfa (ACall \""^name^"\")") in
     let app2 = string2exp ("nfa (ARet \""^name^"\")") in
     let mcall =
       compile (make_exp (Pexp_fun (Nolabel, None, pat, app))) dom in
     let mret  =
       compile (make_exp (Pexp_fun (Nolabel, None, pat, app2))) range in
     let app3 = 
       make_exp (Pexp_apply (f, [(Nolabel, make_exp @@ dummy_id "x")])) in
     let cond = make_exp (Pexp_fun (Nolabel, None, pat, app3)) in
     `FMonitor (cond, (mcall, mret))
  | SFun2 (dom, range) ->
     let anytrue = string2exp "fun x -> true" in
     let mcall = compile anytrue dom in
     let mret  = compile anytrue range in
     let pat = dummy_pat (Ppat_var {txt = "x"; loc = dummy_loc;}) in
     let app = 
       make_exp (Pexp_apply (f, [(Nolabel, make_exp @@ dummy_id "x")])) in
     let cond = make_exp (Pexp_fun (Nolabel, None, pat, app)) in
     `FMonitor (cond, (mcall, mret))

(* is_constant : program -> bool *)
let is_constant = function
  | `PConstant c -> true
  | `PFunction f -> false
  | _ -> failwith "bad variant"

(* make_toplet_item : string -> expression -> structure_item *)
let make_toplet_item name exp =
  let open Parsetree in
  let open Asttypes in
  let open Location in
  let open Lexing in
  let dummy_loc = 
    {loc_start = dummy_pos;loc_end = dummy_pos;loc_ghost = true;}
  in
  let pattern =
    {
      ppat_desc = Ppat_var {txt=name;loc=dummy_loc};
      ppat_loc  = dummy_loc;
      ppat_attributes = [];
    }
  in
  let value_binding =
    [{
        pvb_pat = pattern;
        pvb_expr = exp;
        pvb_loc = dummy_loc;
        pvb_attributes = [];
    }] 
  in
  {
    pstr_desc=Pstr_value (Nonrecursive, value_binding);
    pstr_loc=dummy_loc;
  }

let parse_contract contracts =
  let contract = List.hd contracts in (* 今はheadだけ *)
  let open Parsetree in
  let open Asttypes in
  try
    let Pstr_attribute (_, payload) = contract.pstr_desc in
    let PStr str = payload in 
    let Pstr_eval (exp, _) = (List.hd str).pstr_desc in
    let Pexp_constant const = exp.pexp_desc in
    let Pconst_string (cont_txt, _) = const in
    let lexbuf = Lexing.from_string cont_txt in
    TcontParser.hotcontract TcontLexer.token lexbuf
  with 
    e -> failwith ("Temporal Contract Faile:" ^ (Printexc.to_string e))

(*(SFlat FAny, Wilds)*) (* dummy *)
(* app_guard : scont -> monitor list -> structure -> structure *)
let app_guard sconts monitors structure =
  let open Parsetree in
  let open Asttypes in
  (* guard_exp : monitor *)
  let rec guard_exp monitor = 
    let false_exp = string2exp "false" in
    let neq = make_exp @@ dummy_id "(<>)" in
    let isntfalse e = Pexp_apply (neq, [(Nolabel, e);(Nolabel, false_exp)]) in
    let patx = dummy_pat (Ppat_var {txt = "x"; loc = dummy_loc;}) in
    match monitor with
    | `CMonitor f ->
       let exp = string2exp "x" in
       let fx = make_exp @@ Pexp_apply (f, [(Nolabel, exp)]) in
       let condition = 
         make_exp @@ isntfalse fx in
       let assertion = make_exp (Pexp_assert (condition)) in
       let body = make_exp (Pexp_sequence (assertion, exp)) in
       Pexp_fun (Nolabel, None, patx, body)
    | `FMonitor (f, (m1, m2)) ->
       let exp = string2exp "x" in
       let fx = make_exp @@ Pexp_apply (f, [(Nolabel, string2exp "()")]) in
       let condition = make_exp @@ isntfalse fx in
       let assertion = make_exp (Pexp_assert (condition)) in
       let pat = dummy_pat (Ppat_var {txt = "y"; loc = dummy_loc;}) in
       let guarded_y =
         make_exp 
           (Pexp_apply (make_exp @@ guard_exp m1, [(Nolabel,string2exp "y")])) 
       in
       let xy = make_exp (Pexp_apply (exp, [(Nolabel, guarded_y)])) in
       let lam_body =
         make_exp (Pexp_apply (make_exp @@ guard_exp m2, [(Nolabel, xy)])) 
       in
       let result = make_exp (Pexp_fun (Nolabel, None, pat, lam_body)) in
       let xbody = make_exp (Pexp_sequence (assertion, result)) in
       Pexp_fun (Nolabel, None, patx, xbody)
  in
  let guard_str_item rest = 
    let open Location in
    let open Lexing in
    let guard_let monitors bind =
      let rename new_name bind =
         let ppat_desc' = Ppat_var new_name in
         let pvb_pat' = {bind.pvb_pat with ppat_desc = ppat_desc'} in
         {bind with pvb_pat = pvb_pat'}
      in
      let inner bind (monitor, namef) =
        match bind.pvb_pat.ppat_desc with
        | Ppat_var name ->
           if name.txt = namef 
           then
             let name_str = namef ^ "_origin" in
             let name' = {name with txt = name_str} in
             let def_origin = rename name' bind in
             let def_guard = make_exp @@ guard_exp monitor in
             let fun_id = make_exp @@ dummy_id name_str in
             let def_new =
               make_exp (Pexp_let (Nonrecursive,
                                   [def_origin],
                                   make_exp @@
                                     Pexp_apply(def_guard, [(Nolabel, fun_id)])
                                  )
                        ) 
             in {bind with pvb_expr = def_new}
           else bind
        | _ -> bind
      in List.fold_left inner bind monitors
    in
    fun structure_item ->
    match structure_item.pstr_desc with
    | Pstr_value (rflag, binds) ->
       let top_let = 
         { structure_item with
           pstr_desc =
             Pstr_value (Nonrecursive, List.map (guard_let monitors) binds)
         }
       in
       top_let :: rest
    | _ -> structure_item :: rest
  in
  List.fold_left 
    (fun a b -> (guard_str_item a b)) [] @@ List.rev structure

(* mdepth : monitor -> int *)(* for debug *)
let rec mdepth = function
  | `CMonitor _ -> 1
  | `FMonitor (_, (a, b)) -> 1 + max (mdepth a) (mdepth b)

(* print_monitor : monitor -> unit *)
let print_monitor monitor =
  let rec inner =function
    | `CMonitor c ->
       let c_str = Pprintast.string_of_expression c in
       Printf.printf "CM(%s)" c_str
    | `FMonitor (f, (m1, m2)) ->
       let f_str = Pprintast.string_of_expression f in
       Printf.printf "FM(%s,(" f_str;
       inner m1;print_string ",";inner m2;
       print_string "))"
  in inner monitor

(* print_monitors : (monitor, string) list -> unit *)
let print_monitors monitors =
  print_string "MONITORS:\n";
  ignore @@
    List.map (fun (mon, name) ->
               print_string (name ^ ":");
               print_monitor mon;
               print_string "\n"
             ) monitors

(* monitoring : hotcontract -> structure -> structure *)
let monitoring contracts structure =
  let fun_name = function 
    |SFun1 (name,_,_) -> name | _ -> failwith "toplevel needs name"
  in
  let any = string2exp "fun x -> true" in
  let (ss, r) = parse_contract contracts in
  let rev_nfas = Nfa.make r in
  let monitors = List.map (fun s -> (compile any s, fun_name s)) ss in
  print_monitors monitors;print_string "\n\n";
  let monitored = app_guard ss monitors structure in
  let whole = List.rev_append rev_nfas monitored
  in whole
