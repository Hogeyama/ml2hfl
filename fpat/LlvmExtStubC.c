/* This program is written by Naoki Takashima */

#include <llvm-c/Core.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include "LlvmExtStubCpp.h"

#include <stdio.h>

/* llvalue -> float option */
CAMLprim value llvm_float_of_const(LLVMValueRef val)
{
    CAMLparam0();
    CAMLlocal1(opt);
    
    if (LLVMIsAConstantFP(val)) {
        double dbl = LlvmConstantFpGetDouble(val);
        opt = caml_alloc(1, 0);  /* Some(tag=0)のブロック */
        Store_field(opt, 0, caml_copy_double(dbl));
        CAMLreturn(opt);         /* Some v */
    }
    
    CAMLreturn(Val_int(0));      /* None */
}

/* llvalue -> int array */
CAMLprim value llvm_get_indices(LLVMValueRef v)
{
    CAMLparam0();
    CAMLlocal1(ary);
    
    size_t len, i;
    unsigned *p = LlvmExtGetIndices(v, &len);
    
    ary = caml_alloc(len, 0);  /* 長さlenの配列 */
    for (i = 0; i < len; i++) Store_field(ary, i, Val_int(p[i]));
    LlvmExtFreeArray(p);
    
    CAMLreturn(ary);
}
