open Util
open Combinator

type t =
  | Leaf
  | Label of Idnt.t * Idnt.t * Idnt.t
  | Alter of t * t
  | Emp

let rec pr ppf u =
  match u with
  | Leaf ->
    Format.fprintf ppf "()"
  | Label(id1, id2, id3) ->
    Format.fprintf ppf "%a(%a, %a)" Idnt.pr id1 Idnt.pr id2 Idnt.pr id3
  | Alter(u1, u2) ->
    Format.fprintf ppf "(%a | %a)" pr u1 pr u2
  | Emp ->
    ()

let rec rename rn u =
  match u with
  | Leaf ->
    Leaf
  | Label(id1, id2, id3) ->
    Label(id1,
          (try List.assoc id2 rn with Not_found -> id2),
          (try List.assoc id3 rn with Not_found -> id3))
  | Alter(u1, u2) ->
    Alter(rename rn u1, rename rn u2)
  | Emp ->
    Emp

let rec next_ids u =
  match u with
  | Leaf ->
    []
  | Label(id1, id2, id3) ->
    [id2; id3]
  | Alter(u1, u2) ->
    (*List.unique*) ((next_ids u1) @ (next_ids u2))
  | Emp ->
    []

let assoc id rs =
  try List.assoc id rs with Not_found -> failwith ("state " ^ id ^ " not found")
(* without changing order*)
let remove_unreachable ids rs =
  let reachable_ids =
    Set_.reachable
      (List.unique ids)
      (fun id -> List.map Idnt.string_of (next_ids (assoc id rs)))
  in
  List.filter (fun (id, _) -> List.mem id reachable_ids) rs

let rec reachable id rs =
  List.map
    (fun id -> id, assoc id rs)
    (Set_.reachable [id]
       (fun id -> List.map Idnt.string_of (next_ids (assoc id rs))))

let rec minimize rs =
  let rec f rs nece =
    match rs with
    | [] -> nece
    | (id, u)::rs ->
      begin
        try begin
          let (id', _) =
            List.find
              (fun (id', u') ->
                 (rename [id, id'] u) = (rename [id, id'] u'))
              nece
          in
(*
          Format.printf "=  %a@," pr u;
*)
          let rn = [id, id'] in
          f (List.map (fun (id, u) -> id, rename rn u) rs)
            (List.map (fun (id, u) -> id, rename rn u) nece)
        end with Not_found -> begin
(*
          Format.printf "<> %a@," pr u;
*)
            f rs ((id, u)::nece)
          end
      end
  in
  let rs' = List.rev (f rs []) in
  if List.length rs' < List.length rs then
    minimize rs'
  else
    rs'

let canonize u =
  let rec aux u =
    match u with
    | Leaf | Label(_, _, _) -> [u]
    | Alter(u1, u2) ->
      let x = (aux u1) @ (aux u2) in
      let x' = List.unique x in
      (*let _ = assert (x = x') in*)
      x'
    | Emp -> []
  in
  let xs = aux u in
  let xs = List.sort xs in
  match xs with
  | [] -> failwith "canonize"
  | [x] -> x
  | x::xs -> List.fold_left (fun x y -> Alter(x, y)) x xs

let rec of_table_aux u =
  match u with
  | Leaf ->
    [Idnt.make "leaf", []]
  | Label(id1, id2, id3) ->
    [id1, [id2; id3]]
  | Alter(u1, u2) ->
    (of_table_aux u1) @ (of_table_aux u2)
  | Emp ->
    failwith "TreeAutomaton.of_table_aux"
let of_table rs =
  List.map (fun (id, u) -> id, of_table_aux u) rs

let make_table xs = List.classify xs

let pr_table ppf trs =
  List.iter
    (fun (id1, xs) ->
       List.iter
         (fun (id2, ids) ->
            Format.fprintf
              ppf
              "%a %a -> %a.@\n"
              Idnt.pr id1 Idnt.pr id2 (List.pr Idnt.pr " ") ids)
         xs)
    trs

let next_ids_table xs = List.concat_map (fun (_, ids) -> ids) xs

(* without changing order*)
let remove_unreachable_table ids trs =
  let reachable_ids =
    Set_.reachable
      (List.unique ids)
      (fun id -> next_ids_table (assoc id trs))
  in
  List.filter (fun (id, _) -> List.mem id reachable_ids) trs

let rec reachable_table id trs =
  List.map
    (fun id -> id, assoc id trs)
    (Set_.reachable [id] (fun id -> next_ids_table (assoc id trs)))

let number_of_states trs =
  let states = List.map (fun (q,_) -> q) trs in
  let states = List.unique states in
  List.length states

(*
let parse_file filename =
  let inchan = open_in filename in
  let lb = Lexing.from_channel inchan in
  let _ = lb.Lexing.lex_curr_p <-
    { Lexing.pos_fname = Filename.basename filename; Lexing.pos_lnum = 1; Lexing.pos_cnum = 0; Lexing.pos_bol = 0 } in
  let tl = Parser.xmltypedefs Lexer.token lb in
  let _ = close_in inchan in
  tl

let parse_string str =
  let lb = Lexing.from_string str in
  let _ = lb.Lexing.lex_curr_p <-
    { Lexing.pos_fname = ""; Lexing.pos_lnum = 1; Lexing.pos_cnum = 0; Lexing.pos_bol = 0 } in
  Parser.xmltypedefs Lexer.token lb

let _ =
  let filename =
    if Array.length Sys.argv = 2 then
      Sys.argv.(1)
    else
      failwith "invalid arguments"
  in
  let env = parse_file filename in
  Format.printf "@[<v>";
  let env = RegTreeExp.elim_kleene_env env in
  let env = RegTreeExp.elim_option_env env in
(*
  List.iter (fun (id, t) -> Format.printf "type %s = %a@," id RegTreeExp.pr t) env;
  Format.printf "@,";
*)
  let rs = RegTreeExp.to_ta_env env env [] [] in
  let rs = List.map (fun (id, u) -> id, TreeAutomaton.canonize u) rs in
  let (id, _) = List.hd rs in
  let rs = TreeAutomaton.remove_unreachable [id] rs in
(*
  List.iter (fun (id, u) -> Format.printf "%s -> %a@," id TreeAutomaton.pr u) rs;
*)
  let rs = TreeAutomaton.minimize rs in
  let rn = List.mapi (fun i (id, _) -> id, "q" ^ (string_of_int i)) rs in
  let rs = List.map (fun (id, u) -> List.assoc id rn, TreeAutomaton.rename rn u) rs in
(*
  List.iter (fun (id, u) -> Format.printf "%s -> %a@," id TreeAutomaton.pr u) rs;
*)
  let trs = TreeAutomaton.of_table rs in
  let trs_out = trs in
  Format.printf "%%BEGINA@,";
  Format.printf "%a" TreeAutomaton.pr_table trs_out;
  Format.printf "%%ENDA@,";
  Format.printf "@]"
 *)
