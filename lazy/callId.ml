open ExtList
open ExtString
open Var

(** Call-ids *)

let pr ppf (x, uid) = Format.fprintf ppf "<%a@@%d>" Var.pr x uid

(** @return the call id of top level function call *)
let rec tlfc_of x =
  match x with
    V(_) -> raise Not_found
  | C(_) -> assert false
  | T(x', uid, _) -> try tlfc_of x' with Not_found -> x', uid

(** @return the call id of parent function call *)
let rec fc_ref_of x =
  match x with
    V(_) -> assert false
  | C(_) -> assert false
  | T(x', uid, _) -> x', uid

let rec ancestor_of (x, uid) (x', uid') =
  (x = x' && uid = uid') ||
  (match x' with
    V(_) -> false
  | C(_) -> assert false
  | T(x', uid', _) -> ancestor_of (x, uid) (x', uid'))

