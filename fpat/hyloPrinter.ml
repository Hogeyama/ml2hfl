open Util
open Combinator
open Syntax
open Str
open Printf
open Reduction
open Is_out

let out_chan = stdout
(* ファイル出力 *)
(* let out_chan = open_out "output.txt" *)

(* 三つ組リストを三つのリストにする *)
let rec unzip3  = function
  | [] -> ([], [], [])
  | (x, y, z) :: rest ->
    let (xs, ys, zs) = unzip3 rest in
    (x :: xs, y :: ys, z :: zs)

(* 'a リストの :: を指定した文字列に置き換えて対応した関数で出力 *)
let print_list s print = function
  | [] -> ()
  | x :: xs ->
      print x;
      List.iter (fun y -> fprintf out_chan "%s" s; print y) xs

(* variable を出力 *)
let print_variable = function
  | Var s -> fprintf out_chan "%s" s
  | Intlit i -> fprintf out_chan "%d" i
  | Plus -> fprintf out_chan "plus"
  | Minus -> fprintf out_chan "minus"
  | Mul -> fprintf out_chan "mul"
  | Div -> fprintf out_chan "div"
  | Max -> fprintf out_chan "max"
  | Min -> fprintf out_chan "min"
  | Less -> fprintf out_chan "<"
  | Greater -> fprintf out_chan ">"
  | Error -> fprintf out_chan "error"

(* c を出力 *)
let print_c = function
  | Nil -> fprintf out_chan "Nil"
  | Cons -> fprintf out_chan "Cons"
  | True -> fprintf out_chan "true"
  | False -> fprintf out_chan "false"

(* p を出力 *)
let rec print_p = function
  | Pc (c, p) ->
      print_c c;
      fprintf out_chan " ";
      print_p p;
  | Pt ps ->
      fprintf out_chan "(";
      print_list ", " print_p ps;
      fprintf out_chan ")"
  | Pv v -> print_variable v


(* termを文字列に変換 *)
let rec print_term = function
  | Tv v -> print_variable v
  | Tt ts ->
    begin
      match ts with
      | [t] -> print_term t
      | _ -> fprintf out_chan "(";
             print_list ", " print_term ts;
             fprintf out_chan ")"
    end
  | Ta (v, t) ->
      print_variable v;
      fprintf out_chan " ";
      print_term t
  | Tc (c, t) ->
      print_c c;
      fprintf out_chan " ";
      print_term t


(* vsを文字列に変換 *)
let rec print_vs = function
  | V v -> print_variable v
  | Vs vs ->
      fprintf out_chan "(";
      print_list ", " print_variable vs;
      fprintf out_chan ")"


(* fnct を出力 *)
let rec print_fnct = function
  | Identity -> fprintf out_chan "I"
  | Constant a -> fprintf out_chan "!%s" a
  | Product (f::fs) -> print_list " x " print_fnct (f::fs)
  | Sum (f::fs) -> print_list " + " print_fnct (f::fs)
  | _ -> failwith "Functor error!"

(* expr を出力 *)
let rec print_expr = function
  | App (Const c, Tuple []) -> print_c c
  | App (f, g) ->
    begin
      match f with
      | Tag (_, _) | Const _ | EVar _ 
      | Tuple _ | Banana (_, _) | Id
      | Envelope _ | Wildcard -> print_expr f; fprintf out_chan " "
      | _ -> fprintf out_chan "("; print_expr f; fprintf out_chan ") "
    end;
    begin
      match g with
      | Tag (_, _) | Const _ | EVar _ 
      | Tuple _ | Banana (_, _) | Id
      | Envelope _ | Wildcard -> print_expr g
      | _ -> fprintf out_chan "("; print_expr g; fprintf out_chan ")"
    end
  | Circle (f, g) ->
    begin
      match f with
      | Tag (_, _) | Const _ | EVar _ 
      | Tuple _ | Banana (_, _) | Id
      | Envelope _ | Wildcard -> print_expr f
      | _ -> fprintf out_chan "("; print_expr f; fprintf out_chan ")"
    end;
    fprintf out_chan " . ";
    begin
      match g with
      | Tag (_, _) | Const _ | EVar _ 
      | Tuple _ | Banana (_, _) | Id
      | Envelope _ | Wildcard -> print_expr g
      | _ -> fprintf out_chan "("; print_expr g; fprintf out_chan ")"
    end
  | Plus (e :: es) ->
      print_list " + " print_expr (e::es);
  | InvTri (e :: es) ->
      print_list " # " print_expr (e::es);
  | Tag (t, e) ->
      fprintf out_chan "(%d, " t;
      print_expr e;
      fprintf out_chan ")"
  | Lambda (Tuple [], t) ->
      print_expr t
  | Lambda (InvTri es, body) ->
      fprintf out_chan "\\ ( ";
      print_expr (InvTri es);
      fprintf out_chan "). ";
      print_expr body
  | Lambda (arg, body) ->
      fprintf out_chan "\\ ";
      print_expr arg;
      fprintf out_chan ". ";
      print_expr body
  | Id -> fprintf out_chan "id"
  | Case (t, ptns) ->
    let print_ptn (p, e) =
      print_expr p;
      fprintf out_chan " -> ";
      print_expr e
    in
      fprintf out_chan "case ";
      print_expr t;
      fprintf out_chan " of \n\t";
      print_list ";\n\t" print_ptn ptns
  | Const c -> print_c c
  | EVar v -> print_variable v
  | Tuple [e] -> print_expr e
  | Tuple es ->
      fprintf out_chan "(";
      print_list ", " print_expr es;
      fprintf out_chan ")"
  | Banana (b, f) ->
      fprintf out_chan "(| ";
      print_expr b;
      fprintf out_chan " |)"
  | Envelope h ->
      print_hylo h ~flag:false
  | Wildcard -> fprintf out_chan "_"
  | _ -> failwith "Expr error!"

and print_hylo ?(flag=true) (Hylo (phi, eta, psi, g, f)) =
  if flag then
    begin
      if is_out psi f then
      (* Banana style *)
	print_expr (Banana (reduct (Circle (phi, eta)), f))
      else
	begin
	  fprintf out_chan " [|\n";
          print_expr phi; fprintf out_chan ", \n";
          print_expr eta; fprintf out_chan ", \n";
          print_expr psi; fprintf out_chan "\n|] \n";
          fprintf out_chan "G = "; print_fnct g; fprintf out_chan "\n";
          fprintf out_chan "F = "; print_fnct f; fprintf out_chan "\n"
	end
    end
  else
    begin
      fprintf out_chan " [|";
      print_expr phi; fprintf out_chan ", ";
      print_expr eta; fprintf out_chan ", ";
      print_expr psi; fprintf out_chan "|] "
    end
