open Util
open Combinator

type t = HCCS.t -> PredSubst.t

exception NoSolution
exception UnsolvableCore of string list
exception Unknown

(** {6 Dynamically linked solvers} *)
    
let ext_init_rsrefine = ref (fun _ -> () : PredVar.t list -> unit)
let init_rsrefine pvs =
  Logger.log_block1 "HCCSSolver.init_rsrefine" !ext_init_rsrefine pvs
let ext_solve_rsrefine_rs1 =
  ref (fun _ -> failwith "./configure --enable-rsrefine" : t)
let solve_rsrefine_rs1 hcs =
  Logger.log_block1 "HCCSSolver.solve_rsrefine_rs1" !ext_solve_rsrefine_rs1 hcs
let ext_solve_rsrefine_rs4 =
  ref (fun _ -> failwith "./configure --enable-rsrefine" : t)
let solve_rsrefine_rs4 hcs =
  Logger.log_block1 "HCCSSolver.solve_rsrefine_rs4" !ext_solve_rsrefine_rs4 hcs
let ext_solve_rsrefine_rs8 =
  ref (fun _ -> failwith "./configure --enable-rsrefine" : t)
let solve_rsrefine_rs8 hcs =
  Logger.log_block1 "HCCSSolver.solve_rsrefine_rs8" !ext_solve_rsrefine_rs8 hcs
let ext_solve_rsrefine_rse =
  ref (fun _ -> failwith "./configure --enable-rsrefine" : t)
let solve_rsrefine_rse hcs =
  Logger.log_block1 "HCCSSolver.solve_rsrefine_rse" !ext_solve_rsrefine_rse hcs
let ext_solve_rsrefine_itm =
  ref (fun _ -> failwith "./configure --enable-rsrefine" : t)
let solve_rsrefine_itm hcs =
  Logger.log_block1 "HCCSSolver.solve_rsrefine_itm" !ext_solve_rsrefine_itm hcs
let ext_solve_rsrefine_exl =
  ref (fun _ -> failwith "./configure --enable-rsrefine" : t)
let solve_rsrefine_exl hcs =
  Logger.log_block1 "HCCSSolver.solve_rsrefine_exl" !ext_solve_rsrefine_exl hcs

let ext_solve_duality =
  ref (fun _ -> assert false : t)
let solve_duality hcs =
  Logger.log_block1 "HCCSSolver.solve_duality" !ext_solve_duality hcs
let ext_solve_pdr =
  ref (fun _ -> assert false : t)
let solve_pdr hcs =
  Logger.log_block1 "HCCSSolver.solve_pdr" !ext_solve_pdr hcs

let ref_solver = ref (fun _ -> assert false : t)
let link_dyn solver = ref_solver := solver
let get_dyn () = !ref_solver

let ext_solve_unit = ref (fun _ _ -> assert false : t -> t)
let ext_solve_bool = ref (fun _ _ -> assert false : t -> t)
let ext_solve_tuple = ref (fun _ _ -> assert false : t -> t)
let check_solvability_first = ref false
    
let solve_dyn hcs =
  if !check_solvability_first && not (FwHCCSSolver.is_solvable hcs) then
    begin
      Logger.printf "not solvable:@,  %a@," HCCS.pr hcs;
      raise NoSolution
    end;
  hcs
  |> (!ref_solver
      |> UndefHCCSSolver.solve (* this should be first *)
      |> UnusedHCCSSolver.solve (* this should be first *)
      |> CheckHCCSSolver.solve
      |> !ext_solve_unit |> !ext_solve_bool |> !ext_solve_tuple)
let solve_dyn =
  Logger.log_block1 "HCCSSolver.solve_dyn"
    ~after:(Logger.printf "solution:@,  %a" PredSubst.pr)
    solve_dyn

let ext_of_string = ref (fun _ -> assert false)
let of_string_dyn str = !ext_of_string str
