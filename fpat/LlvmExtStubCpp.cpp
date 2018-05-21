//============================================================
// This program is written by Naoki Takashima
// LlvmExtStubCpp.cpp
// Llvm_extに必要なC++の関数を，C言語からアクセス可能にする．
//============================================================

#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>
#include <llvm-c/Core.h>
#include <algorithm>
#include <limits>
#include "LlvmExtStubCpp.h"

#include <iostream>

using namespace llvm;

extern "C" {

//--------------------------------------------------
// LlvmExtArrayRefToCArray
// LLVMのArrayRefをC言語の配列に変換する．
// 内部で配列を動的に割り当てており，戻り値はポインタ．
// 同時に，引数lenに配列の長さが代入される．
// 割り当てられた配列は，LlvmExtFreeArrayで解放する．
//--------------------------------------------------
unsigned *LlvmExtArrayRefToCArray(ArrayRef<unsigned> aref, size_t *len)
{
    *len = aref.size();
    unsigned *ary = new unsigned[*len];
    std::copy(aref.begin(), aref.end(), ary);
    return ary;
}


//--------------------------------------------------
// LlvmExtFreeArray
// LlvmExtArrayRefToCArrayが割り当てた配列を解放する．
//--------------------------------------------------
void LlvmExtFreeArray(unsigned *ary)
{
    delete [] ary;
}


//--------------------------------------------------
// LlvmExtGetIndices
// vrefが以下のいずれかのクラスの場合，
// そのgetIndices()メンバ関数を呼び出す．
// - ConstantExpr(ExtractValueConstantExpr,
//       InsertValueConstantExprの時)
// - ExtractValueInst
// - InsertValueInst
// - GetElementPtrInst
// それ以外の場合はNULLポインタを返す．
// 戻り値はLlvmExtArrayRefToCArrayが返すC言語の配列．
//--------------------------------------------------
unsigned *LlvmExtGetIndices(LLVMValueRef vref, size_t *len)
{
    Value *v = unwrap<Value>(vref);

    if (ConstantExpr *e = dyn_cast<ConstantExpr>(v))
        if (e->hasIndices())
            return LlvmExtArrayRefToCArray(e->getIndices(), len);

    if (ExtractValueInst *e = dyn_cast<ExtractValueInst>(v))
        return LlvmExtArrayRefToCArray(e->getIndices(), len);

    if (InsertValueInst *e = dyn_cast<InsertValueInst>(v))
        return LlvmExtArrayRefToCArray(e->getIndices(), len);

    // Indices を持たないクラスの場合．
    *len = 0;
    return 0; // nullポインタ
}


//--------------------------------------------------
// LlvmConstantFpGetDouble
// ConstantFPがdoubleかfloatを表している時に，
// それをdoubleに変換する．
// それ以外の浮動小数点数(X86_FP80など)はnanを返す．
//--------------------------------------------------
double LlvmConstantFpGetDouble(LLVMValueRef val)
{
    const APFloat &apf = unwrap<ConstantFP>(val)->getValueAPF();
    const fltSemantics *sem = &apf.getSemantics();

    if (sem == &APFloat::IEEEdouble)
        return apf.convertToDouble();

    if (sem == &APFloat::IEEEsingle)
        return static_cast<double>(apf.convertToFloat());

    // 変換できない場合nanを返す．
    return std::numeric_limits<double>::signaling_NaN();
}

} // end of extern "C"

