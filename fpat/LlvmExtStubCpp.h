// This program is written by Naoki Takashima

#ifndef LLVM_EXT_STUB_CPP_H_INCLUDED
#define LLVM_EXT_STUB_CPP_H_INCLUDED

#include <llvm-c/Core.h>

#ifdef __cplusplus
extern "C" {
#endif

void LlvmExtFreeArray(unsigned *ary);
unsigned *LlvmExtGetIndices(LLVMValueRef vref, size_t *len);
double LlvmConstantFpGetDouble(LLVMValueRef val);

#ifdef __cplusplus
} // end of extern "C"
#endif

#endif // LLVM_EXT_STUB_CPP_H_INCLUDED

