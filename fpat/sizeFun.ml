open Util
open Combinator

type elem =
  (* size_name, [(Constructor0, (args, term)); ...] *)
  Idnt.t * (Idnt.t * (Idnt.t list * Term.t)) list
type t = elem list

let use_default_size = ref false

let sizes_to_infer = ref [] (** for printing results *)
let subst tsub =
  List.map
    (fun (size_name, body) ->
       size_name,
       List.map
         (fun (id, (xs, t)) ->
            let tsub = (* subst 0 to unused coeffs *)
              tsub @ (Term.coeffs t |> List.map (flip Pair.make IntTerm.zero))
            in
            id, (xs, Term.subst tsub t))
         body)

(** {6 Printers} *)

let pr_elem_body ppf (con_id, (ids, term)) =
  if ids = []
  then Format.fprintf ppf "%a : @[%a@]" Idnt.pr con_id Term.pr term
  else
    Format.fprintf ppf "%a : @[%a. %a@]"
      Idnt.pr con_id Idnt.pr_list ids Term.pr term
let pr_elem ppf (ty_id, tl) =
  Format.fprintf ppf "%a = @[%a@]"
    Idnt.pr ty_id (List.pr pr_elem_body "@,| ") tl
let pr ppf sizes = Format.fprintf ppf "%a" (List.pr pr_elem "@,") sizes

(** find size functions whose return type is ty *)
let of_ty (sizes:t) (tenv:TypEnv.t) ty =
  let con_ty = List.filter (fun (_, typ) -> ty = Type.ret_of typ) tenv in
  List.filter (snd >> List.for_all (fst >> flip List.mem_assoc con_ty)) sizes

(** return vars and terms for con *)
let of_con sizes con =
  List.map
    (fun (id, defs) ->
       if List.mem_assoc con defs then List.assoc con defs
       else begin
         Logger.printf2
           "size function %a does not contain the rule for %a@,"
           Idnt.pr id Idnt.pr con;
         assert false
       end)
    sizes
let of_con sizes con = Logger.log_block2 "SizeFun.of_con" of_con sizes con

let is_size x =
  Str.string_match
    (Str.regexp "size_[a-z][a-zA-Z0-9]*") (Idnt.string_of x) 0

(** make a default size function for ty *)
let make_size tenv ty : elem =
  Logger.printf "tenv: %a@,@," TypEnv.pr tenv;
  let con_ty = (* make constructors tenv *)
    match Type.fun_args ty with
    | Type.Const(TypConst.Adt(d, cs)), [] ->
      List.map (fun c -> c, TypEnv.lookup tenv c) cs
    | _ ->
      Logger.printf ~kind:Logger.Error "%a is not ADT@," Type.pr ty;
      assert false
  in
  (Idnt.make @@ "Size_" ^ (Type.string_of ty)),
  List.map
    (fun (con, ty) ->
       let args = Type.args_of ty |> List.map (fun ty -> Idnt.new_var (), ty) in
       Logger.printf2 "ty: %a, args: %a@," Type.pr ty TypEnv.pr args;
       let terms =
         List.fold_left
           (fun term (x,typ) ->
              if Type.is_adt typ then IntTerm.add term (Term.mk_var x)
              else term)
           IntTerm.one
           args
       in
       con, (List.map fst args, terms))
    con_ty
let make_size =
  Logger.log_block2 "SizeFun.make_size"
    ~after:(Logger.printf "output: %a" pr_elem)
    make_size

(** make a size function template for ty *)
let make_template tenv ty1 : elem =
  Logger.printf "tenv: %a@,@," TypEnv.pr tenv;
  let con_ty = (* make constructors tenv *)
    match Type.fun_args ty1 with
    | Type.Const(TypConst.Adt(d, cs)), [] ->
      List.map (fun c -> c, TypEnv.lookup tenv c) cs
    | _ ->
      Logger.printf ~kind:Logger.Error "%a is not ADT@," Type.pr ty1;
      assert false
  in
  (Idnt.make @@ "Size_" ^ (Type.string_of ty1)),
  List.map
    (fun (con, ty) ->
       let new_var_args =
         Type.args_of ty |> List.map (fun ty -> Idnt.new_var (), ty)
       in
       let args = List.map (fun (id,ty) -> id, Type.mk_int) new_var_args in
       Logger.printf2 "ty: %a, args: %a@," Type.pr ty TypEnv.pr args;
       let args_without_fun =
         List.filter (fun (id,ty) -> not (Type.is_fun ty)) new_var_args
         |> List.map (fun (id,ty) -> id, Type.mk_int) in
       let terms = Template.mk_linexp args_without_fun in
       con, (List.map fst args, terms))
    con_ty
let make_template tenv ty =
  make_template tenv ty
  |> (fun cs -> sizes_to_infer := cs :: !sizes_to_infer; cs)
let make_template =
  Logger.log_block2 "SizeFun.make_template"
    ~after:(Logger.printf "output: %a" pr_elem)
    make_template

(** make templates or default size functions for ADTs *)
let make_sizes tenv : t =
  tenv
  |> TypEnv.codom
  |> List.filter (Type.ret_of >> Type.is_adt)
  |> List.map Type.ret_of
  |> List.unique
  |> List.map (if !use_default_size then make_size tenv else make_template tenv)
