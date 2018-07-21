open Util
open Combinator

(** Function definitions *)

(** @require a.name = b.name => length a.args = length b.args *)
type t = {
  name: string;
  args: Pattern.t list; (* @require length length > 0 *)
  guard: Formula.t; (* a boolean expression that does not
                       involve side-effects and function calls *)
  body: Term.t
}

let make name args guard body =
  { name = name; args = args; guard = guard; body = body }

let name_of f = f.name
let args_of f = f.args
let guard_of f = f.guard
let body_of f = f.body

let use_anf = ref true

let pr ppf fdef =
  if Formula.is_true fdef.guard then
    Format.fprintf
      ppf
      "@[<hov2>%a =@ %a@]"
      (List.pr_app Pattern.pr "@ ")
      (Pattern.V (Idnt.make fdef.name) :: fdef.args)
      MLExp.pr fdef.body
  else
    Format.fprintf
      ppf
      "@[<hov2>@[<hov2>%a@ | %a@] =@ %a@]"
      (List.pr_app Pattern.pr "@ ")
      (Pattern.V (Idnt.make fdef.name) :: fdef.args)
      Formula.pr fdef.guard
      MLExp.pr fdef.body

let coeffs fdef = Formula.coeffs fdef.guard @ Term.coeffs fdef.body
let arity_of fdef = fdef.args |> List.length

let map_body f fd = { fd with body = f fd.body }
let alpha_body = map_body Term.alpha_conv

let rec pat_match pat t =
  match pat with
  | Pattern.V(x) -> [x, t]
  | Pattern.T(ps) ->
    List.mapi
      (fun i p ->
         pat_match p
           (TupTerm.mk_proj (List.map (fun _ -> Type.mk_unknown) ps) i t))
      ps
    |> List.concat
  | _ -> assert false

let get_sub fdef args =
  if List.length fdef.args <> List.length args then begin
    Format.printf
      "fdef.args: %a@,args: %a@,"
      (List.pr Pattern.pr ",") fdef.args
      (List.pr Term.pr ",") args;
    assert false
  end;
  List.concat_map2 pat_match fdef.args args

let apply fdef args =
  let sub = get_sub fdef args in
  assert (fdef.guard = Formula.mk_true);
  Term.subst sub fdef.body
let apply = Logger.log_block2 "Fdef.apply" apply


(** peel letand and return bodies *)
let flatten_letand exp =
  match Term.fun_args exp with
  | (Term.Const(Const.ML_LetAnd), es) -> es
  | _ -> failwith "Fdef.flatten_letand"

(** convert MLExp.t to Fdef.t list
    @require toplevel function definition *)
let of_mlexp = function
  | Term.App(e1, e2) ->
    (* Format.printf "@[<hov2>e1: %a, e2: %a\n@]@." Term.pr e1 Term.pr e2; *)
    (* let t2 = Term.fun_args e |> snd in *)
    (* Format.printf "@[<hov2>args: %a\n@]" (List.pr Term.pr ",") t2; *)
    begin
      match Term.fun_args (Term.App(e1, e2)) with
      | Term.Const(Const.ML_Let(_)),
        e' :: [Term.Binder
                 (Binder.Lambda(_), Pattern.V(fid), Term.Const(Const.Undef))] ->
        let args, body = Term.args_body_of e' in
        let body = if !use_anf then MLExp.anf body else body in
        [make (Idnt.base fid) args Formula.mk_true body] (* @todo use guard *)
      | Term.Const(Const.ML_LetRec _), e1' :: e2' :: [] ->
        let pats, e2' = Term.args_body_of e2' in
        let fids = List.map (function Pattern.V(fid) -> fid) pats in
        begin
          match e2' with
          | Term.Const(Const.Undef) -> (* case of toplevel function def *)
            let aux (fid, e) =
              let args, body = Term.args_body_of e in
              let body = if !use_anf then MLExp.anf body else body in
              make (Idnt.base fid) args Formula.mk_true body
              (* @todo use guard *)
            in
            e1'
            |> Term.args_body_of
            |> snd
            |> flatten_letand
            |> List.zip fids
            |> List.map aux
          | _ ->
            raise (Global.NotImplemented
                     "Fdef.of_mlexp: toplevel function definition is required.")
        end
      | Term.Const(Const.ML_Let(_)), _ ->
        raise (Global.NotImplemented
                 "Fdef.of_mlexp: toplevel function definition is required.")
      | _ ->
        raise (Global.NotImplemented
                 "Fdef.of_mlexp: an ANF expression is required.")
    end
  | _ ->
    raise (Global.NotImplemented
             "Fdef.of_mlexp: an ANF expression is required. (Outer-most term is not App)).")

let of_mlexp_with_adt tcenv = ADTFormula.typing_term tcenv >> of_mlexp

let rec_check f =
  let name = Idnt.make f.name in
  let body = f.body in
  Term.fvs body |> List.mem name
let rec_check =
  Logger.log_block1 "Fdef.rec_check"
    ~before:(name_of >> Logger.printf "input (name): %a@," String.pr)
    ~after:(Logger.printf "output: %a" Bool.pr)
    rec_check

let has_read_bool fdef = MLExp.has_read_bool fdef.body



let make_fdefs progs tenv =
  progs
  |> List.map @@ Pair.map_fst @@ (of_mlexp_with_adt tenv >> List.map alpha_body)
  |> List.split
  |> Pair.map List.concat List.concat
let make_fdefs =
  Logger.log_block2 "Fdef.make_fdefs"
    ~after:(Logger.printf "output: %a" (Pair.pr (List.pr pr "@,@,") TypEnv.pr))
    make_fdefs

