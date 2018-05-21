open Util
open Combinator

open Pretty
open Cil
module E = Errormsg
module H = Hashtbl

(** @require Heapify.default_heapify (with Heapify.heapifyNonArrays)
             and Simplify.doGlobal are applied
    @ensure all the malloc, __builtin_alloca, free, read, and write operations on the heap
            are encoded as operations on a global array
    @todo the translated program may contain a pointer dereference of the form *p,
          which is ill-typed (but intension is *(global_heap + p))
          this should have been translated to a call to a function for dereference
          but such a translation is not easy because Call instruction is neither lval nor exp
    @todo global_heap must have the type char* so that the semantics on pointer arithmetic is preserved
    @todo allow a global variable to have its address taken
          heapify does not move address taken global variables to the heap? *)

let disable_heap = ref false

let global_heap = "global_heap"
let global_heap_var = makeGlobalVar global_heap voidPtrType
let global_pointer = "global_pointer"
let global_pointer_var = makeGlobalVar global_pointer uintType

let update_heap = "update_heap"
let update_heap_exp =
  Lval(Var((emptyFunction update_heap).svar), NoOffset)

class heap2ArrayVisitor =
object (self) inherit nopCilVisitor
  method vlval lval = (* for heap read *)
    match lval with
    | (Mem(Lval(Var(v), off)), NoOffset) ->
       let exp = Lval(Var(v), off)
       in
       ChangeTo(Mem(mkCast exp voidPtrType(* @todo do not use void ptr *)(* @todo this type must not be translated to unsigned int *)), NoOffset)
    | (Var v, off) ->
       DoChildren
    | _ -> assert false
  method vexpr exp = (* for pointer arithmetics *)
    match exp with
    | BinOp(PlusPI, b1, b2, typ)
    | BinOp(IndexPI, b1, b2, typ) ->
       ChangeTo
         (BinOp
            (PlusA,
             b1,
             BinOp(Mult, b2, integer (bitsSizeOf typ / 8), intType),
             intType))
    | BinOp(MinusPI, b1, b2, typ) ->
       ChangeTo
         (BinOp
            (MinusA,
             b1,
             BinOp(Mult, b2, integer (bitsSizeOf typ / 8), intType),
             intType))
    | BinOp(MinusPP, b1, b2, typ) ->
       assert false
    | _ ->
       DoChildren
  method vinst inst =
    match inst with
    | Set((Mem(Lval(Var(v), off)), NoOffset), e, loc) ->
       (* for heap write *)
       let insts =
         [Call
            (Some(Var(global_heap_var), NoOffset),
             update_heap_exp,
             [Lval(Var(global_heap_var), NoOffset);
              Lval(Var(v), off);
              e],
             loc)]
       in
       ChangeTo(insts)
    | Call(opt, Lval(Var(v), NoOffset), args, _) ->
       if v.vname = "malloc" || v.vname = "__builtin_alloca" then
         begin
           match opt, args with
           | Some(Var(v'), _), [e(*Const(CInt64(size, _, _))*)] ->
              (*let n = Int64.to_int (Int64.div size (Int64.of_int (bitsSizeOf (v'.vtype) / 8))) in*)
              let insts = 
                [Set
                   ((Var(v'), NoOffset),
                    Lval(Var(global_pointer_var), NoOffset),
                    locUnknown); (* assign current free address stored in global_pointer to v' *)
                 Set
                   ((Var(global_pointer_var), NoOffset),
                    BinOp
                      (PlusA,
                       Lval(Var(global_pointer_var), NoOffset),
                       e(*integer n*),
                       uintType),
                    locUnknown)] (* increment current free address stored in global_pointer by e *)
              in
              ChangeTo(insts)
           | _ ->
              assert false
         end
       else if v.vname = "free" then
         ChangeTo([])
       else
         DoChildren
    | _ ->
       DoChildren
end

(* translate the type of pointers except global_heap to unsigned int
   @todo can we move this to heap2ArrayVisitor? *)
class heap2ArrayTypeVisitor =
object (self) inherit nopCilVisitor
  method vtype typ =
    match typ with
    | TPtr(TVoid(_), _) ->
       (* @todo variables with void pointer type other than global_heap should also be translated *)
       DoChildren
    | TPtr(_, _) ->
       ChangeTo(uintType)
    | _ ->
       DoChildren
end

let heap2array f =
  if not !disable_heap then
    begin
      visitCilFileSameGlobals (new heap2ArrayVisitor) f;
      visitCilFileSameGlobals (new heap2ArrayTypeVisitor) f;
      f.globals <-
        GVar(global_heap_var, { init = None }, locUnknown) ::
          GVar(global_pointer_var, { init = Some(makeZeroInit uintType) }, locUnknown) ::
            f.globals
    end


(*
let do_h2a_feature (f:file) = assert false

let feature =
  {
    fd_name = "Heap2Array";
    fd_enabled = ref true;
    fd_description = "Heap to Array transformation";
    fd_extraopt = [];
    fd_doit = do_h2a_feature;
    fd_post_check = false
  }
 *)
