open Util

type result_tree =
  | Exists of (result_tree * result_tree)
  | Forall of (int * result_tree)
  | Label of string * result_tree
  | End | Fail

let rec string_of_result_tree = function
  | Exists(r1, r2) -> String.join " " ["(br_exists"; (string_of_result_tree r1); (string_of_result_tree r2); ")"]
  | Forall(0, r) -> String.join " " ["(br_forall _"; (string_of_result_tree r); ")"]
  | Forall(1, r) -> String.join " " ["(br_forall"; (string_of_result_tree r); "_ )"]
  | Label(l, r) -> "(" ^ l ^ " " ^ string_of_result_tree r ^ ")"
  | End -> "unit"
  | Fail -> "fail"
