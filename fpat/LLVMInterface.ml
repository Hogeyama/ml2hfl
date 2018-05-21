open Util
open Combinator

open Llvm
open Llvm_bitreader
open Opcode
open ValueKind

let context = global_context ()
let int32_t = i32_type context
let float_t = float_type context

let is_prim_typ ty =
  match classify_type ty with
    TypeKind.Void -> assert false
  | TypeKind.Half -> assert false
  | TypeKind.Float -> true
  | TypeKind.Double -> true
  | TypeKind.X86fp80 -> assert false
  | TypeKind.Fp128 -> assert false
  | TypeKind.Ppc_fp128 -> assert false
  | TypeKind.Label -> assert false
  | TypeKind.Integer -> true
  | TypeKind.Function -> assert false
  | TypeKind.Struct -> false
  | TypeKind.Array -> false
  | TypeKind.Pointer -> assert false
  | TypeKind.Vector -> false
  | TypeKind.Metadata -> assert false

let is_int_typ ty =
  match classify_type ty with
    TypeKind.Void -> assert false
  | TypeKind.Half -> assert false
  | TypeKind.Float -> false
  | TypeKind.Double -> false
  | TypeKind.X86fp80 -> assert false
  | TypeKind.Fp128 -> assert false
  | TypeKind.Ppc_fp128 -> assert false
  | TypeKind.Label -> assert false
  | TypeKind.Integer -> true
  | TypeKind.Function -> assert false
  | TypeKind.Struct -> false
  | TypeKind.Array -> false
  | TypeKind.Pointer -> assert false
  | TypeKind.Vector -> false
  | TypeKind.Metadata -> assert false

let is_float_typ ty =
  match classify_type ty with
    TypeKind.Void -> assert false
  | TypeKind.Half -> assert false
  | TypeKind.Float -> true
  | TypeKind.Double -> false
  | TypeKind.X86fp80 -> assert false
  | TypeKind.Fp128 -> assert false
  | TypeKind.Ppc_fp128 -> assert false
  | TypeKind.Label -> assert false
  | TypeKind.Integer -> false
  | TypeKind.Function -> assert false
  | TypeKind.Struct -> false
  | TypeKind.Array -> false
  | TypeKind.Pointer -> assert false
  | TypeKind.Vector -> false
  | TypeKind.Metadata -> assert false

let is_store instr =
  instr_opcode instr = Store

let is_ret instr =
  instr_opcode instr = Ret


let var_of lv =
  match classify_value lv with
    GlobalVariable
  | Instruction _ ->
    begin
      let x = Idnt.make (value_name lv) in
      Logger.printf "var: %a@," Idnt.pr x;
      x
    end
  | _ ->
      let _ = dump_value lv in
      assert false
let var_of =
  Logger.log_block1
        "LLVMInterface.var_of"
    var_of

(*
let rec typ_of ty =
  match classify_type ty with
    TypeKind.Void -> assert false
  | TypeKind.Half -> assert false
  | TypeKind.Float -> Type.mk_real
  | TypeKind.Double -> assert false
  | TypeKind.X86fp80 -> assert false
  | TypeKind.Fp128 -> assert false
  | TypeKind.Ppc_fp128 -> assert false
  | TypeKind.Label -> assert false
  | TypeKind.Integer -> Type.mk_int
  | TypeKind.Function -> assert false
  | TypeKind.Struct -> assert false
  | TypeKind.Array ->
      let size = array_length ty in
      let ety = typ_of (element_type ty) in
      Type.mk_array ety (*size*)
  | TypeKind.Pointer -> assert false
  | TypeKind.Vector ->
      let size = vector_size ty in
      let ety = typ_of (element_type ty) in
      Type.mk_vector ety size
  | TypeKind.Metadata -> assert false
 *)

(*
(** @todo how to extract the float value of a LLVM value? *)
let float_of_const lv =
  let md = create_module context "float_of_const" in
  let builder = builder context in
  let main_type = function_type float_t [| |] in
  let main = declare_function "main" main_type md in
  let bb = append_block context "entry" main in
  position_at_end bb builder;
  ignore (build_ret lv builder);
  (*Llvm_analysis.assert_valid_function main;*)
  let engine = Llvm_executionengine.ExecutionEngine.create md in
  let res =
    Llvm_executionengine.ExecutionEngine.run_function
      main [| |] engine
  in
  Some(Llvm_executionengine.GenericValue.as_float float_t res)
*)

(*
val make_typed_var : Idnt.t -> Type.t -> ('b, 'c) t
let make_typed_var x ty = Var([Attr.Type(ty)], x)
*)
let term_of_aux lv ty =
  if is_prim_typ ty then begin
    Term.mk_var (var_of lv) (*(typ_of ty)*)
  end else
    let size = vector_size ty in
    let t = Term.mk_var (var_of lv) (*(typ_of ty)*) in
    VecTerm.make_fvec
      (List.map (VecTerm.elem size t) (List.from_to 0 (size - 1)))

let rec term_of lv =
  try
    match classify_value lv with
      GlobalVariable ->
        term_of_aux lv (element_type (type_of lv))
    | Instruction _ ->
        term_of_aux lv (type_of lv)
    | ConstantAggregateZero ->
        let ty = type_of lv in
        if is_int_typ ty then
          IntTerm.zero
        else if is_float_typ ty then
          RealTerm.zero
        else
          let ety = element_type ty in
          VecTerm.make_fvec
            (List.unfold
              (fun i ->
                if i < vector_size ty then
                  if is_int_typ ety then
                    Some(IntTerm.zero, i + 1)
                  else if is_float_typ ety then
                    Some(RealTerm.zero, i + 1)
                  else
                    assert false
                else
                  None)
              0)
    | Function ->
        Term.mk_var (Idnt.make (value_name lv))
    | ConstantFP ->
        (match ExtLLVM.float_of_const lv with
          Some(f) -> RealTerm.make f
        | None -> assert false)
    | ConstantInt ->
        (match int64_of_const lv with
          Some(i) -> IntTerm.make (Int64.to_int i)
        | None -> assert false)
    | UndefValue ->
        MLExp.mk_undef
    | _ ->
        let () = dump_value lv in
        assert false
  with Failure("Unknown Value class") ->
    let ty = type_of lv in
    if is_prim_typ ty then
      term_of (const_extractvalue lv [|0|])
    else
      VecTerm.make_fvec
        (List.unfold
          (fun i ->
            if i < vector_size ty then
              let t = 
                term_of (const_extractelement lv (const_int int32_t i))
              in
              Some (t, i + 1)
            else
              None)
          0)

let rename_instr instr =
  Logger.log (fun () ->
    dump_value instr);
  let target = value_name instr in
  (if not (is_store instr || is_ret instr) && target = "" then
    set_value_name (Idnt.string_of (Idnt.new_var ())) instr);
  ()
let rename_instr =
  Logger.log_block1
        "LLVMInterface.rename_instr"
    rename_instr

let rename_block bb = iter_instrs rename_instr bb
let rename_function f = iter_blocks rename_block f
let rename_module md = iter_functions rename_function md

let string_of_opcode opcode =
  match opcode with
  | Invalid -> assert false
  | Ret -> "ret"
  | Br -> assert false
  | Switch -> assert false
  | IndirectBr -> assert false
  | Invoke -> assert false
  | Invalid2 -> assert false
  | Unreachable -> assert false
  | Add -> assert false
  | FAdd -> "fadd"
  | Sub -> assert false
  | FSub -> "fsub"
  | Mul -> assert false
  | FMul -> "fmul"
  | UDiv -> assert false
  | SDiv -> assert false
  | FDiv -> assert false
  | URem -> assert false
  | SRem -> assert false
  | FRem -> assert false
  | Shl -> assert false
  | LShr -> assert false
  | AShr -> assert false
  | And -> assert false
  | Or -> assert false
  | Xor -> assert false
  | Alloca -> assert false
  | Load -> "load"
  | Store -> "store"
  | GetElementPtr -> assert false
  | Trunc -> assert false
  | ZExt -> assert false
  | SExt -> assert false
  | FPToUI -> assert false
  | FPToSI -> assert false
  | UIToFP -> assert false
  | SIToFP -> assert false
  | FPTrunc -> assert false
  | FPExt -> assert false
  | PtrToInt -> assert false
  | IntToPtr -> assert false
  | BitCast -> assert false
  | ICmp -> assert false
  | FCmp -> assert false
  | PHI -> assert false
  | Call -> "call"
  | Select -> assert false
  | UserOp1 -> assert false
  | UserOp2 -> assert false
  | VAArg -> assert false
  | ExtractElement -> "extractelement"
  | InsertElement -> "insertelement"
  | ShuffleVector -> "shufflevector"
  | ExtractValue -> "extractvalue"
  | InsertValue -> "insertvalue"
  | Fence -> assert false
  | AtomicCmpXchg -> assert false
  | AtomicRMW -> assert false
  | Resume -> assert false
  | LandingPad -> assert false

let is_rsq f = f = "llvm.fischersp.rsq"

(** @todo make this independent of maestro *)
let call f ts tys =
  match ts with
    [t] when f = "llvm.fischersp.add4" ->
      VecTerm.sum t
  | [t1; t2] when f = "llvm.fischersp.max" ->
      let ty = List.hd tys in
      if is_prim_typ ty then
        RealTerm.max t1 t2
      else
        VecTerm.max (vector_size ty) t1 t2
  | [t1; t2] when f = "llvm.fischersp.min" ->
      let ty = List.hd tys in
      if is_prim_typ ty then
        RealTerm.min t1 t2
      else
        VecTerm.min (vector_size ty) t1 t2
  | [t] when is_rsq f ->
      RealTerm.rsq t
  | [t] when f = "llvm.fischersp.rcp" ->
      RealTerm.rcp t
  | [t] when f = "llvm.log2.f32" ->
      RealTerm.log2 t
  | [t] when f = "llvm.exp2.f32" ->
      RealTerm.exp2 t
  | [_; _; _; _] when f = "llvm.fischersp.ext" ->
      Term.mk_app (Term.var_of_string f) ts
  | [_] when f = "llvm.fischersp.tex.read" ->
      Term.mk_app (Term.var_of_string f) ts
  | _ ->
    begin
      Format.printf "%s not supported@," f;
      assert false (*Call(f, ts)*)
    end

let rs_simplify =
  [(* rcp (rcp x) -> x *)
   Term.mk_app
     (Term.mk_const Const.FRcp)
       [Term.mk_app
          (Term.mk_const Const.FRcp)
          [Term.var_of_string "x"]],
   (Term.var_of_string "x");
   (* <x, y, z>.0 -> x *)
   Term.mk_app
     (Term.mk_const (Const.VElem(Type.mk_real, 3, 0)))
     [Term.mk_app
        (Term.mk_const (Const.Vector(Type.mk_real, 3)))
        [Term.var_of_string "x";
         Term.var_of_string "y";
         Term.var_of_string "z"]],
   Term.var_of_string "x";
   (* <x, y, z>.1 -> y *)
   Term.mk_app
     (Term.mk_const (Const.VElem(Type.mk_real, 3, 1)))
     [Term.mk_app
        (Term.mk_const (Const.Vector(Type.mk_real, 3)))
        [Term.var_of_string "x";
         Term.var_of_string "y";
         Term.var_of_string "z"]],
   Term.var_of_string "y";
   (* <x, y, z>.2 -> z *)
   Term.mk_app
     (Term.mk_const (Const.VElem(Type.mk_real, 3, 2)))
     [Term.mk_app
        (Term.mk_const (Const.Vector(Type.mk_real, 3)))
        [Term.var_of_string "x";
         Term.var_of_string "y";
         Term.var_of_string "z"]],
   Term.var_of_string "z";
   (* <x, y, z, w>.0 -> x *)
   Term.mk_app
     (Term.mk_const (Const.VElem(Type.mk_real, 4, 0)))
     [Term.mk_app
        (Term.mk_const (Const.Vector(Type.mk_real, 4)))
        [Term.var_of_string "x";
         Term.var_of_string "y";
         Term.var_of_string "z";
         Term.var_of_string "w"]],
   Term.var_of_string "x";
   (* <x, y, z, w>.1 -> y *)
   Term.mk_app
     (Term.mk_const (Const.VElem(Type.mk_real, 4, 1)))
     [Term.mk_app
        (Term.mk_const (Const.Vector(Type.mk_real, 4)))
        [Term.var_of_string "x";
         Term.var_of_string "y";
         Term.var_of_string "z";
         Term.var_of_string "w"]],
   Term.var_of_string "y";
   (* <x, y, z, w>.2 -> z *)
   Term.mk_app
     (Term.mk_const (Const.VElem(Type.mk_real, 4, 2)))
     [Term.mk_app
        (Term.mk_const (Const.Vector(Type.mk_real, 4)))
        [Term.var_of_string "x";
         Term.var_of_string "y";
         Term.var_of_string "z";
         Term.var_of_string "w"]],
   Term.var_of_string "z";
   (* <x, y, z, w>.3 -> w *)
   Term.mk_app
     (Term.mk_const (Const.VElem(Type.mk_real, 4, 3)))
     [Term.mk_app
        (Term.mk_const (Const.Vector(Type.mk_real, 4)))
        [Term.var_of_string "x";
         Term.var_of_string "y";
         Term.var_of_string "z";
         Term.var_of_string "w"]],
   Term.var_of_string "w"]

let extract t =
  let x_0 size =
    Term.mk_app
      (Term.mk_const (Const.VElem(Type.mk_real, size, 0)))
      [Term.var_of_string "x"]
  in
  let x_1 size =
    Term.mk_app
      (Term.mk_const (Const.VElem(Type.mk_real, size, 1)))
      [Term.var_of_string "x"]
  in
  let x_2 size =
    Term.mk_app
      (Term.mk_const (Const.VElem(Type.mk_real, size, 2)))
      [Term.var_of_string "x"]
  in
  let y_0 size =
    Term.mk_app
      (Term.mk_const (Const.VElem(Type.mk_real, size, 0)))
      [Term.var_of_string "y"]
  in
  let y_1 size =
    Term.mk_app
      (Term.mk_const (Const.VElem(Type.mk_real, size, 1)))
      [Term.var_of_string "y"]
  in
  let y_2 size =
    Term.mk_app
      (Term.mk_const (Const.VElem(Type.mk_real, size, 2)))
      [Term.var_of_string "y"]
  in
  let rs =
    (* <x.0 + y.0, x.1 + y.1, x.2 + y.2> -> <x.0, x.1, x.2> + <y.0, y.1, y.2> *)
    List.map
      (fun (size1, size2) ->
        Term.mk_app
          (Term.mk_const (Const.Vector(Type.mk_real, 3)))
          [Term.mk_app (Term.mk_const (Const.Add(Type.mk_real))) [x_0 size1; y_0 size2];
           Term.mk_app (Term.mk_const (Const.Add(Type.mk_real))) [x_1 size1; y_1 size2];
           Term.mk_app (Term.mk_const (Const.Add(Type.mk_real))) [x_2 size1; y_2 size2]],
        Term.mk_app
          (Term.mk_const (Const.Add(Type.mk_vector Type.mk_real 3)))
          [Term.mk_app
             (Term.mk_const (Const.Vector(Type.mk_real, 3)))
             [x_0 size1; x_1 size1; x_2 size1];
           Term.mk_app
             (Term.mk_const (Const.Vector(Type.mk_real, 3)))
             [y_0 size2; y_1 size2; y_2 size2]])
      [(3, 3); (3, 4); (4, 3); (4, 4)] @
    (* x.0 * y.0 + x.1 * y.1 + x.2 * y.2 -> dot <x.0, x.1, x.2> <y.0, y.1, y.2> *)
    List.map
      (fun (size1, size2) ->
        Term.mk_app
          (Term.mk_const (Const.Add(Type.mk_real)))
          [Term.mk_app
             (Term.mk_const (Const.Add(Type.mk_real)))
             [Term.mk_app
                (Term.mk_const (Const.Mul(Type.mk_real)))
                [x_0 size1; y_0 size2];
              Term.mk_app
                (Term.mk_const (Const.Mul(Type.mk_real)))
                [x_1 size1; y_1 size2]];
           Term.mk_app
             (Term.mk_const (Const.Mul(Type.mk_real)))
             [x_2 size1; y_2 size2]],
        Term.mk_app
          (Term.mk_const (Const.VDot(Type.mk_real, 3)))
          [Term.mk_app
             (Term.mk_const (Const.Vector(Type.mk_real, 3)))
             [x_0 size1; x_1 size1; x_2 size1];
           Term.mk_app
             (Term.mk_const (Const.Vector(Type.mk_real, 3)))
             [y_0 size2; y_1 size2; y_2 size2]])
      [(3, 3); (3, 4); (4, 3); (4, 4)] @
    [(* rsq (dot v v) -> rnorm v *)
     Term.mk_app
       (Term.mk_const Const.FRsq)
         [Term.mk_app
            (Term.mk_const (Const.VDot(Type.mk_real, 3)))
            [Term.var_of_string "v";
             Term.var_of_string "v"]],
     Term.mk_app
       (Term.mk_const (Const.VRnorm(Type.mk_real, 3)))
       [Term.var_of_string "v"];
     (* <rnorm <x, y, z> * x, rnorm <x, y, z> * y, rnorm <x, y, z> * z> -> normalize <x, y, z> *)
     Term.mk_app
       (Term.mk_const (Const.Vector(Type.mk_real, 3)))
       [Term.mk_app
         (Term.mk_const (Const.Mul(Type.mk_real)))
         [Term.mk_app
            (Term.mk_const (Const.VRnorm(Type.mk_real, 3)))
            [Term.mk_app
               (Term.mk_const (Const.Vector(Type.mk_real, 3)))
               [Term.var_of_string "x";
                Term.var_of_string "y";
                Term.var_of_string "z"]];
          Term.var_of_string "x"];
       Term.mk_app
         (Term.mk_const (Const.Mul(Type.mk_real)))
         [Term.mk_app
            (Term.mk_const (Const.VRnorm(Type.mk_real, 3)))
            [Term.mk_app
               (Term.mk_const (Const.Vector(Type.mk_real, 3)))
               [Term.var_of_string "x";
                Term.var_of_string "y";
                Term.var_of_string "z"]];
          Term.var_of_string "y"];
       Term.mk_app
         (Term.mk_const (Const.Mul(Type.mk_real)))
         [Term.mk_app
            (Term.mk_const (Const.VRnorm(Type.mk_real, 3)))
            [Term.mk_app
               (Term.mk_const (Const.Vector(Type.mk_real, 3)))
               [Term.var_of_string "x";
                Term.var_of_string "y";
                Term.var_of_string "z"]];
          Term.var_of_string "z"]],
     Term.mk_app
       (Term.mk_const (Const.VNormalize(Type.mk_real, 3)))
       [Term.mk_app
          (Term.mk_const (Const.Vector(Type.mk_real, 3)))
          [Term.var_of_string "x";
           Term.var_of_string "y";
           Term.var_of_string "z"]];
     (* <x * rnorm <x, y, z>, y * rnorm <x, y, z>, z * rnorm <x, y, z>> -> normalize <x, y, z> *)
     Term.mk_app
       (Term.mk_const (Const.Vector(Type.mk_real, 3)))
       [Term.mk_app
         (Term.mk_const (Const.Mul(Type.mk_real)))
         [Term.var_of_string "x";
          Term.mk_app
            (Term.mk_const (Const.VRnorm(Type.mk_real, 3)))
            [Term.mk_app
               (Term.mk_const (Const.Vector(Type.mk_real, 3)))
               [Term.var_of_string "x";
                Term.var_of_string "y";
                Term.var_of_string "z"]]];
       Term.mk_app
         (Term.mk_const (Const.Mul(Type.mk_real)))
         [Term.var_of_string "y";
          Term.mk_app
            (Term.mk_const (Const.VRnorm(Type.mk_real, 3)))
            [Term.mk_app
               (Term.mk_const (Const.Vector(Type.mk_real, 3)))
               [Term.var_of_string "x";
                Term.var_of_string "y";
                Term.var_of_string "z"]]];
       Term.mk_app
         (Term.mk_const (Const.Mul(Type.mk_real)))
         [Term.var_of_string "z";
          Term.mk_app
            (Term.mk_const (Const.VRnorm(Type.mk_real, 3)))
            [Term.mk_app
               (Term.mk_const (Const.Vector(Type.mk_real, 3)))
               [Term.var_of_string "x";
                Term.var_of_string "y";
                Term.var_of_string "z"]]]],
     Term.mk_app
       (Term.mk_const (Const.VNormalize(Type.mk_real, 3)))
       [Term.mk_app
          (Term.mk_const (Const.Vector(Type.mk_real, 3)))
          [Term.var_of_string "x";
           Term.var_of_string "y";
           Term.var_of_string "z"]]
    ]
  in
  RewriteSystem.normalize (rs_simplify @ rs) t

(** @todo refactor this *)
let symbolic_eval_instr env instr =
  let target = Idnt.make (value_name instr) in
  let opcode = instr_opcode instr in
  let operands =
    List.unfold
      (fun i ->
        if i < num_operands instr then
          (*let _ = dump_value (operand instr i) in*)
          Some(operand instr i, i + 1)
        else
          None)
      0
  in

  Logger.log (fun () ->
    Format.printf "analyzing instruction:@,";
    dump_value instr;
    Format.printf "target: %a@," Idnt.pr target;
    Format.printf "opcode: %s@," (string_of_opcode opcode);
    Format.printf "operands: ...@,");

  let xtys =
    List.filter
      (fun (_, t) ->
        match Term.fun_args t with
          Term.Const(Const.VNormalize(_, _)), [_] ->
            false
        | Term.Const(Const.Add(Type.Const(TypConst.Vector(_)))), [_; _] ->
            false
        (*| Term.Const(Const.Vector(_, _)), ts
          when List.exists (fun t -> match Term.fun_args t with Term.Const(Const.FAdd), _ -> true | _, _ -> false) ts ->
            false*)
        | _ -> true)
      env
  in

  let env' =
    match opcode, operands with
      Load, [lv] ->
        let t = Term.subst xtys (term_of lv) in
        [target, extract t]
    | Store, [lv1; lv2] ->
        let t = Term.subst xtys (term_of lv1) in
        [var_of lv2, extract t]
    | ShuffleVector, [lv1; lv2; lv3] ->
        let t1 = Term.subst xtys (term_of lv1) in
        let t2 = Term.subst xtys (term_of lv2) in
        let t3 = Term.subst xtys (term_of lv3) in
        let t =
          VecTerm.vshuffle
            (vector_size (type_of lv1)) t1 t2 t3
        in
        [target, extract t]
    | InsertElement, [lv1; lv2; lv3] ->
        let t1 = Term.subst xtys (term_of lv1) in
        let t2 = Term.subst xtys (term_of lv2) in
        let t3 = Term.subst xtys (term_of lv3) in
        let t =
          VecTerm.vinsert_element
            (vector_size (type_of lv1)) t1 t2 t3
        in
        [target, extract t]
    | ExtractElement, [lv1; lv2] ->
        let t1 = Term.subst xtys (term_of lv1) in
        let t2 = Term.subst xtys (term_of lv2) in
        let t =
          VecTerm.vextract_element
            (vector_size (type_of lv1)) t1 t2
        in
        [target, extract t]
    | InsertValue, [lv1; lv2] ->
        (*Logger.log (fun () -> Format.printf "****InsertValue:@,"; dump_value instr);*)
        let idxs = ExtLLVM.get_indices instr in
        (*Logger.log (fun () -> Format.printf "****@,");*)
        let t1 = Term.subst xtys (term_of lv1) in
        let t2 = Term.subst xtys (term_of lv2) in
        (*let ts = List.map (term_of >> Term.subst xtys) lvs in*)
        let t =
          Array.fold_left
            (fun t1 idx ->
              (*Logger.log (fun () -> Format.printf "index: %d@," idx);*)
              VecTerm.vinsert_element
                (array_length (type_of lv1)) t1 t2 (IntTerm.make idx))
            t1
            idxs
        in
        [target, extract t]
    | ExtractValue, [lv1] ->
        (*Logger.log (fun () -> Format.printf "****ExtractValue:@,"; dump_value instr);*)
        let idxs = ExtLLVM.get_indices instr in
        let t1 = Term.subst xtys (term_of lv1) in
        (*let ts = List.map (term_of >> Term.subst xtys) lvs in*)
        let t =
          List.elem_of_singleton
            (List.map
              (fun idx ->
                (*Logger.log (fun () -> Format.printf "index: %d@," idx);*)
                VecTerm.vextract_element
                  (array_length (type_of lv1)) t1 (IntTerm.make idx))
              (Array.to_list idxs))
        in
        [target, extract t]
    | FAdd, [lv1; lv2] ->
        let t1 = Term.subst xtys (term_of lv1) in
        let t2 = Term.subst xtys (term_of lv2) in
        let t =
          let ty = type_of lv1 in
          if is_prim_typ ty then
            RealTerm.add t1 t2
          else
            VecTerm.add (vector_size ty) t1 t2
        in
        [target, extract t]
    | FSub, [lv1; lv2] ->
        let t1 = Term.subst xtys (term_of lv1) in
        let t2 = Term.subst xtys (term_of lv2) in
        let t =
          let ty = type_of lv1 in
          if is_prim_typ ty then
            RealTerm.sub t1 t2
          else
            VecTerm.sub (vector_size ty) t1 t2
        in
        [target, extract t]
    | FMul, [lv1; lv2] ->
        let t1 = Term.subst xtys (term_of lv1) in
        let t2 = Term.subst xtys (term_of lv2) in
        let t =
          let ty = type_of lv1 in
          if is_prim_typ ty then
            RealTerm.mul t1 t2
          else
            VecTerm.mul (vector_size ty) t1 t2
        in
        [target, extract t]
    | Call, lvs ->
        let Term.Var(f) = term_of (List.last lvs) in
        let ts =
          List.map
            (term_of >> Term.subst xtys)
            (List.initial lvs)
        in
        let tys =
          List.map
            type_of
            (List.initial lvs)
        in
        let t = call (Idnt.string_of f) ts tys in
        [target, extract t]
    | Ret, [] ->
        []
    | _, lvs ->
      begin
        dump_value instr;
        Logger.debug_assert_false
          ~on_failure:
          (fun () -> Format.printf "env:@,  %a@," TermSubst.pr env)
          ()
      end
  in
  Logger.printf
    "obtained assignment:@,  %a@,"
    TermSubst.pr env';
  env' @ env
let symbolic_eval_instr =
  Logger.log_block2
    "LLVMInterface.symbolic_eval_instr"
    symbolic_eval_instr

let symbolic_eval_block bb =
  fold_left_instrs
    symbolic_eval_instr
    []
    bb

let symbolic_eval_function f =
  Logger.printf
    "symbolic evaluating: %a@,"
    String.pr (value_name f);
  fold_left_blocks
    (fun res bb ->
      res @ symbolic_eval_block bb)
    []
    f
let symbolic_eval_function =
  Logger.log_block1
    "LLVMInterface.symbolic_eval_function"
    symbolic_eval_function

let symbolic_eval_module md =
  let env =
    fold_left_functions
      (fun res f ->
        res @ symbolic_eval_function f)
      []
      md
  in
  let (x, t) :: env' = env in
  let rgb =
    Idnt.make (Idnt.string_of x ^ "_rgb"),
    (VecTerm.make_fvec
       [VecTerm.elem 4 t 0;
        VecTerm.elem 4 t 1;
        VecTerm.elem 4 t 2])
  in
  let alpha =
    Idnt.make (Idnt.string_of x ^ "_alpha"), VecTerm.elem 4 t 3
  in
  rgb, alpha, env'

let parse_file filename =
  let mb = MemoryBuffer.of_file filename in
  try
    begin
      let parsed = parse_bitcode context mb in
      Llvm_analysis.assert_valid_module parsed;
      parsed
    end
  with _ ->
    begin
      MemoryBuffer.dispose mb;
      Logger.debug_assert_false
        ~on_failure:
        (fun () -> Format.printf "parsing failed@,")
        ()
    end
let parse_file =
  Logger.log_block1
    "LLVMInterface.parse_file"
    parse_file

let preprocess md =
  let pm = PassManager.create () in
  Llvm_ipo.add_function_inlining pm;
  Llvm_scalar_opts.add_memory_to_register_promotion pm;
  (*Llvm_scalar_opts.add_constant_propagation pm;*)
  PassManager.run_module md pm;
  rename_module md

let print md = dump_module md

let for_each_global f md =
  iter_globals
    (fun lv ->
      let x = value_name lv in
      f x)
    md
