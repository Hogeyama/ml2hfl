

open Format
open Util
open Syntax
open Type







(*
let rec flatten_app_if = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | Var x -> Var x
  | Fun(x, t) -> Fun(x, t)
  | App(If(t1,t2,t3), ts) ->
      let t1' = flatten_app_if t1 in
      let t2' = flatten_app_if (app2app t2 ts) in
      let t3' = flatten_app_if (app2app t3 ts) in
        If(t1', t2', t3')
  | App(Branch(t1,t2), ts) ->
      let t1' = flatten_app_if (app2app t1 ts) in
      let t2' = flatten_app_if (app2app t2 ts) in
        Branch(t1', t2')
  | App(t, ts) ->
      let t' = flatten_app_if t in
      let ts' = List.map flatten_app_if ts in
        App(t', ts')
  | If(t1, t2, t3) ->
      let t1' = flatten_app_if t1 in
      let t2' = flatten_app_if t2 in
      let t3' = flatten_app_if t3 in
        If(t1', t2', t3')
  | Branch(t1, t2) ->
      let t1' = flatten_app_if t1 in
      let t2' = flatten_app_if t2 in
        Branch(t1', t2')
  | Let(flag, f, xs, t1, t2) ->
      let t1' = flatten_app_if t1 in
      let t2' = flatten_app_if t2 in
        Let(flag, f, xs, t1', t2')
(*
  | Let(flag, bindings, t) ->
      let bindings' = List.map (fun (f,xs,t) -> f,xs,flatten_app_if t) bindings in
      let t' = flatten_app_if t in
        Let(flag, bindings', t')
*)
  | BinOp(op, t1, t2) ->
      let t1' = flatten_app_if t1 in
      let t2' = flatten_app_if t2 in
        BinOp(op, t1', t2')
  | Not t ->
      let t' = flatten_app_if t in
        Not t'
  | Fail -> Fail
  | Label(b, t) ->
      let t' = flatten_app_if t in
        Label(b, t')
  | LabelInt(n, t) -> LabelInt(n, flatten_app_if t)
  | Event s -> Event s





let cps2hors t =
  let label_n = max_label_num t in
  let s = new_var' "S" TUnit in
    (*let _,(t0,defs) = Lift.lift t in
      let defs' = (s,[],t0)::defs in
      let defs'' = List.map (fun (x,xs,t) -> (x,xs,part_eval t)) defs' in*)
  let defs, t0 = lift t in
  let () =
    let t' =  List.fold_right (fun (f, (xs, t')) t -> Syntax.Let(Nonrecursive,f,xs,t',t)) defs t0 in
    Syntax.print_term Syntax.ML false t'
  in
  let defs,t0 = Typing.typing_defs defs t0 in
  let defs = List.map (fun (x, (xs, t)) ->
                         let typs = get_args x.typ in
                         let len = List.length xs in
                         let n = (List.length typs) - len in
                         let ds = tabulate n (fun i -> new_var' "d" (List.nth typs (len+i)).typ) in
                           x, (xs @ ds, if ds = [] then t else App(t, List.map (fun id -> Var(id)) ds))) defs in
  let defs' = (s,([],t0))::defs in
  let nonterms = List.map (fun (f, _) -> f) defs' in
  let sub = List.map (fun x -> x, Var {x with name=String.capitalize x.name}) nonterms in
  let defs'' = List.map (fun (x, (xs, t)) -> (match List.assoc x sub with Var x -> x, xs, subst_term sub (part_eval t) | _ -> assert false)) defs' in

  let defs'' = List.map (fun (f,xs,t) -> f,xs,flatten_app_if t) defs'' in

  (*
    Syntax.print_term Syntax.ML false t;
    let t' =  List.fold_right (fun (f, xs, t') t -> Syntax.Letrec(f,xs,t',t)) defs'' (Var s) in
    Syntax.print_term Syntax.ML false t';
    Typing.typing t';
  *)
  let spec_br q =
    let rec aux i spec =
      if i < 0
      then spec
      else aux (i-1) ((q, "br" ^ string_of_int i, [q])::spec)
    in
      aux label_n []
  in

  let spec =
    match !Flag.mode with
        Flag.Reachability ->
          (0, "br", [0;0])::
          (0, "then", [0])::
          (0, "else", [0])::
          (0, "unit", [])::
            spec_br 0
      | Flag.FileAccess ->
          (0, "br", [0; 0])::
          (1, "br", [1; 1])::
          (2, "br", [2; 2])::
          (3, "br", [3; 3])::
          (0, "newr", [1])::
          (1, "read", [1])::
          (1, "close", [0])::
          (0, "neww", [2])::
          (2, "write", [2])::
          (2, "close", [0])::
          (2, "newr", [3])::
          (1, "neww", [3])::
          (3, "read", [3])::
          (3, "write", [3])::
          (3, "close", [3])::
          (0, "unit", [])::
          (3, "unit", [])::
          (0, "then", [0])::
          (0, "else", [0])::
          (1, "then", [1])::
          (1, "else", [1])::
          (2, "then", [2])::
          (2, "else", [2])::
          (3, "then", [3])::
          (3, "else", [3])::
            (spec_br 0 @ spec_br 1 @ spec_br 2 @ spec_br 3)
  in
    defs'', spec
*)




let node_of_string s =
  match s with
      "br" -> BrNode
    | "fail" -> FailNode
    | "then" -> LabNode true
    | "else" -> LabNode false
    | s when String.sub s 0 2 = "br" ->
        let len = String.length s in
          PatNode (int_of_string (String.sub s 2 (len-2)))
    | s -> EventNode s

let get_pair s =
  let n1 = String.index s ',' in
  let n2 = String.index s ')' in
  let node = node_of_string (String.sub s 1 (n1-1)) in
  let n = int_of_string (String.sub s (n1+1) (n2-n1-1)) in
  let s' = String.sub s (n2+1) (String.length s-n2-1) in
    node, n, s'

let rec parse_trace s =
  if s.[0] = '.'
  then []
  else
    if s.[0] = ' '
    then
      parse_trace (String.sub s 1 (String.length s - 1))
    else
      let node,n,s' = get_pair s in
        (node,n) :: parse_trace s'


let print_hors_to_file hors =
  let cout = open_out "log.hors" in
  let fm = formatter_of_out_channel cout in
    print_hors fm hors;
    pp_print_flush fm ();
    flush cout;
    close_out cout








let print_hors_to_file hors =
  let cout = open_out "log.hors" in
  let fm = formatter_of_out_channel cout in
    print_hors fm hors;
    pp_print_flush fm ();
    flush cout;
    close_out cout





let model_check_aux (funs,spec) =
  let cin,cout = Unix.open_process Flag.trecs in
  let fm = formatter_of_out_channel cout in
  let () = print_hors fm (funs, spec) in
  let () = pp_print_flush fm () in
  let () = close_out cout in
  let s1 = ignore (input_line cin); ignore (input_line cin); input_line cin in
  let s2 = ignore (input_line cin); input_line cin in
  let _ = close_in cin in
  let () = pp_print_flush std_formatter () in
  let () =
    match Unix.close_process (cin, cout) with
        Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> ()
  in
    if s1 = "The property is not satisfied."
    then
      let () =
        if Flag.print_trecs_output
        then printf "TRecS output: %s@.@." s2
      in
      let ce = parse_trace s2 in
      let ce' = List.flatten (List.map (function (BrNode,_) -> [] | t -> [fst t]) ce) in
        Some ce'
    else
      begin
        assert (String.sub s1 (String.length s1 - 3) 3 = " : ");
        None
      end







let spec_br n q =
  let rec aux i spec =
    if i < 0
    then spec
    else aux (i-1) ((q, "br" ^ string_of_int i, [q])::spec)
  in
    aux n []



let spec t =
  let n = max_label_num t in
  match !Flag.mode with
      Flag.Reachability ->
        (0, "br", [0;0])::
          (0, "then", [0])::
          (0, "else", [0])::
          (0, "unit", [])::
          spec_br n 0
    | Flag.FileAccess ->
        (0, "br", [0; 0])::
          (1, "br", [1; 1])::
          (2, "br", [2; 2])::
          (3, "br", [3; 3])::
          (0, "newr", [1])::
          (1, "read", [1])::
          (1, "close", [0])::
          (0, "neww", [2])::
          (2, "write", [2])::
          (2, "close", [0])::
          (2, "newr", [3])::
          (1, "neww", [3])::
          (3, "read", [3])::
          (3, "write", [3])::
          (3, "close", [3])::
          (0, "unit", [])::
          (3, "unit", [])::
          (0, "then", [0])::
          (0, "else", [0])::
          (1, "then", [1])::
          (1, "else", [1])::
          (2, "then", [2])::
          (2, "else", [2])::
          (3, "then", [3])::
          (3, "else", [3])::
          (spec_br n 0 @ spec_br n 1 @ spec_br n 2 @ spec_br n 3)




let capitalize defs =
  let fs = List.map fst defs in
  let map = List.map (fun f -> f, Id.make (Id.id f) (String.capitalize (Id.name f)) (Id.typ f)) fs in
  let map_term = List.map (fun (f,f') -> f, {desc=Var f';typ=Id.typ f'}) map in
  let aux (f,(xs,t)) = Id.assoc f map, (xs, subst_term map_term t) in
    List.map aux defs





let eta_expand (f,(xs,t)) =
  let rec aux typs xs =
    match typs,xs with
        [],[] -> []
      | typ::typs,[] -> Id.new_var "x" typ :: aux typs []
      | _::typs,_::xs -> aux typs xs
      | _ -> assert false
  in
  let ds = aux (get_argtyps (Id.typ f)) xs in
    f, (xs@ds, app2app t (List.map make_var ds))


let model_check (defs,t0) =
(*
  let print_fundef (f, (xs, t)) =
    Format.printf "DEBUG: %a %a-> %a.\n" print_id f print_ids xs pp_print_term t
  in
  let () = List.iter print_fundef defs in
  let () = Format.printf "DEBUG: %a@." pp_print_term t0 in
*)
  let defs = List.map eta_expand defs in
  let () = List.iter (fun (f,(xs,t)) -> Format.printf "%a %a=> %a.\n" print_id_typ f print_ids xs (print_term' ML 0 false) t; Type_check.check t) defs in
  let s = Id.new_var "S" TUnit in
  let funs = (s,([],t0))::defs in
  let funs = capitalize funs in
  let t = List.fold_left (fun t2 (f,(xs,t1)) -> {desc=Let(Flag.Nonrecursive, f, xs, t1, t2);typ=t2.typ}) t0 defs in (* for max_label_num *)
  let spec = spec t in
  let () = print_hors_to_file (funs,spec) in
    try
      model_check_aux (funs,spec)
    with
        Assert_failure(s,_,_) when s <> "" -> assert false
      | End_of_file -> (printf "\nTRecS failed@."; assert false)











