// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2019-2022 Second State INC

#include "executor/executor.h"

namespace WasmEdge {
namespace Executor {

namespace {
ValVariant packVal(const ValType &Type, const ValVariant &Val) {
  if (Type.isPackType()) {
    switch (Type.getCode()) {
    case TypeCode::I8:
      return ValVariant(Val.get<uint32_t>() & 0xFFU);
    case TypeCode::I16:
      return ValVariant(Val.get<uint32_t>() & 0xFFFFU);
    default:
      assumingUnreachable();
    }
  }
  return Val;
}

ValVariant unpackVal(const ValType &Type, const ValVariant &Val,
                     bool IsSigned = false) {
  if (Type.isPackType()) {
    uint32_t Num = Val.get<uint32_t>();
    switch (Type.getCode()) {
    case TypeCode::I8:
      if (IsSigned) {
        return static_cast<uint32_t>(static_cast<int8_t>(Num));
      } else {
        return static_cast<uint32_t>(static_cast<uint8_t>(Num));
      }
    case TypeCode::I16:
      if (IsSigned) {
        return static_cast<uint32_t>(static_cast<int16_t>(Num));
      } else {
        return static_cast<uint32_t>(static_cast<uint16_t>(Num));
      }
    default:
      assumingUnreachable();
    }
  }
  return Val;
}

std::vector<ValVariant> packVals(const ValType &Type,
                                 std::vector<ValVariant> &&Vals) {
  for (uint32_t I = 0; I < Vals.size(); I++) {
    Vals[I] = packVal(Type, Vals[I]);
  }
  return std::move(Vals);
}
} // namespace

Expect<void> Executor::runRefNullOp(Runtime::StackManager &StackMgr,
                                    const ValType &Type) const noexcept {
  StackMgr.push(RefVariant(Type));
  return {};
}

Expect<void> Executor::runRefIsNullOp(ValVariant &Val) const noexcept {
  Val.emplace<uint32_t>(Val.get<RefVariant>().isNull() ? 1U : 0U);
  return {};
}

Expect<void> Executor::runRefFuncOp(Runtime::StackManager &StackMgr,
                                    uint32_t Idx) const noexcept {
  const auto *FuncInst = getFuncInstByIdx(StackMgr, Idx);
  StackMgr.push(RefVariant(FuncInst));
  return {};
}

Expect<void> Executor::runRefEqOp(ValVariant &Val1,
                                  const ValVariant &Val2) const noexcept {
  Val1.emplace<uint32_t>(Val1.get<RefVariant>().asPtr<void>() ==
                                 Val2.get<RefVariant>().asPtr<void>()
                             ? 1U
                             : 0U);
  return {};
}

Expect<void>
Executor::runRefAsNonNullOp(RefVariant &Ref,
                            const AST::Instruction &Instr) const noexcept {
  if (Ref.isNull()) {
    spdlog::error(ErrCode::Value::CastNullToNonNull);
    spdlog::error(
        ErrInfo::InfoInstruction(Instr.getOpCode(), Instr.getOffset()));
    return Unexpect(ErrCode::Value::CastNullToNonNull);
  }
  Ref = RefVariant(Ref.getType().toNonNullableRef(), Ref);
  return {};
}

Expect<void> Executor::runStructNewOp(Runtime::StackManager &StackMgr,
                                      const AST::CompositeType &CompType,
                                      bool IsDefault) const noexcept {
  if (IsDefault) {
    StackMgr.push(RefVariant(Runtime::HeapManager::newStruct(CompType)));
  } else {
    uint32_t N = CompType.getFieldTypes().size();
    auto Vals = StackMgr.pop(N);
    for (uint32_t I = 0; I < N; I++) {
      Vals[I] = packVal(CompType.getFieldTypes()[I].getStorageType(), Vals[I]);
    }
    StackMgr.push(
        RefVariant(Runtime::HeapManager::newStruct(CompType, std::move(Vals))));
  }
  return {};
}

Expect<void> Executor::runStructGetOp(ValVariant &Val, const uint32_t Idx,
                                      const AST::CompositeType &CompType,
                                      const AST::Instruction &Instr,
                                      bool IsSigned) const noexcept {
  const auto *Inst =
      Val.get<RefVariant>().asPtr<Runtime::Instance::StructInstance>();
  if (Inst == nullptr) {
    // TODO: GC - error log
    spdlog::error(ErrCode::Value::AccessNullStruct);
    spdlog::error(
        ErrInfo::InfoInstruction(Instr.getOpCode(), Instr.getOffset()));
    return Unexpect(ErrCode::Value::AccessNullStruct);
  }
  const auto &SType = CompType.getFieldTypes()[Idx].getStorageType();
  Val = unpackVal(SType, Inst->getData(Idx), IsSigned);
  return {};
}

Expect<void>
Executor::runStructSetOp(const ValVariant &Val, const RefVariant &InstRef,
                         const AST::CompositeType &CompType, uint32_t Idx,
                         const AST::Instruction &Instr) const noexcept {
  auto *Inst = InstRef.asPtr<Runtime::Instance::StructInstance>();
  if (Inst == nullptr) {
    // TODO: GC - error log
    spdlog::error(ErrCode::Value::AccessNullStruct);
    spdlog::error(
        ErrInfo::InfoInstruction(Instr.getOpCode(), Instr.getOffset()));
    return Unexpect(ErrCode::Value::AccessNullStruct);
  }
  const auto &SType = CompType.getFieldTypes()[Idx].getStorageType();
  Inst->getData(Idx) = packVal(SType, Val);
  return {};
}

Expect<void> Executor::runArrayNewOp(Runtime::StackManager &StackMgr,
                                     const AST::CompositeType &CompType,
                                     uint32_t InitCnt,
                                     uint32_t ValCnt) const noexcept {
  assuming(InitCnt == 0 || InitCnt == 1 || InitCnt == ValCnt);
  const auto &VType = CompType.getFieldTypes()[0].getStorageType();
  if (InitCnt == 0) {
    StackMgr.push(RefVariant(Runtime::HeapManager::newArray(CompType, ValCnt)));
  } else if (InitCnt == 1) {
    StackMgr.getTop() = RefVariant(Runtime::HeapManager::newArray(
        CompType, ValCnt, packVal(VType, StackMgr.getTop())));
  } else {
    StackMgr.push(RefVariant(Runtime::HeapManager::newArray(
        CompType, packVals(VType, StackMgr.pop(ValCnt)))));
  }
  return {};
}

Expect<void>
Executor::runArrayNewDataOp(uint32_t N, ValVariant &SVal,
                            const AST::CompositeType &CompType,
                            const Runtime::Instance::DataInstance &DataInst,
                            const AST::Instruction &Instr) const noexcept {
  const uint32_t S = SVal.get<uint32_t>();
  const uint32_t BSize =
      CompType.getFieldTypes()[0].getStorageType().getBitWidth() / 8;
  if (static_cast<uint64_t>(S) + static_cast<uint64_t>(N) * BSize >
      DataInst.getData().size()) {
    // TODO: GC - error log
    spdlog::error(ErrCode::Value::MemoryOutOfBounds);
    spdlog::error(
        ErrInfo::InfoInstruction(Instr.getOpCode(), Instr.getOffset()));
    return Unexpect(ErrCode::Value::MemoryOutOfBounds);
  }
  auto *Inst = Runtime::HeapManager::newArray(CompType, N);
  for (uint32_t Idx = 0; Idx < N; Idx++) {
    // The value has been packed.
    Inst->getData(Idx) = DataInst.loadValue(S + Idx * BSize, BSize);
  }
  SVal.emplace<RefVariant>(Inst);
  return {};
}

Expect<void>
Executor::runArrayNewElemOp(uint32_t N, ValVariant &SVal,
                            const AST::CompositeType &CompType,
                            const Runtime::Instance::ElementInstance &ElemInst,
                            const AST::Instruction &Instr) const noexcept {
  const uint32_t S = SVal.get<uint32_t>();
  auto ElemSrc = ElemInst.getRefs();
  if (static_cast<uint64_t>(S) + static_cast<uint64_t>(N) > ElemSrc.size()) {
    // TODO: GC - error log
    spdlog::error(ErrCode::Value::TableOutOfBounds);
    spdlog::error(
        ErrInfo::InfoInstruction(Instr.getOpCode(), Instr.getOffset()));
    return Unexpect(ErrCode::Value::TableOutOfBounds);
  }
  const auto &SType = CompType.getFieldTypes()[0].getStorageType();
  std::vector<ValVariant> Refs(ElemSrc.begin() + S, ElemSrc.begin() + S + N);
  auto *Inst = Runtime::HeapManager::newArray(CompType,
                                              packVals(SType, std::move(Refs)));
  SVal.emplace<RefVariant>(Inst);
  return {};
}

Expect<void>
Executor::runArraySetOp(const ValVariant &Val, const uint32_t Idx,
                        const RefVariant &InstRef,
                        const AST::CompositeType &CompType,
                        const AST::Instruction &Instr) const noexcept {
  auto *Inst = InstRef.asPtr<Runtime::Instance::ArrayInstance>();
  if (Inst == nullptr) {
    // TODO: GC - error log
    spdlog::error(ErrCode::Value::AccessNullArray);
    spdlog::error(
        ErrInfo::InfoInstruction(Instr.getOpCode(), Instr.getOffset()));
    return Unexpect(ErrCode::Value::AccessNullArray);
  }
  if (Idx >= Inst->getLength()) {
    // TODO: GC - error log
    spdlog::error(ErrCode::Value::ArrayOutOfBounds);
    spdlog::error(
        ErrInfo::InfoInstruction(Instr.getOpCode(), Instr.getOffset()));
    return Unexpect(ErrCode::Value::ArrayOutOfBounds);
  }
  const auto &SType = CompType.getFieldTypes()[0].getStorageType();
  Inst->getData(Idx) = packVal(SType, Val);
  return {};
}

Expect<void> Executor::runArrayGetOp(ValVariant &Val, const uint32_t Idx,
                                     const AST::CompositeType &CompType,
                                     const AST::Instruction &Instr,
                                     bool IsSigned) const noexcept {
  const auto *Inst =
      Val.get<RefVariant>().asPtr<Runtime::Instance::ArrayInstance>();
  if (Inst == nullptr) {
    // TODO: GC - error log
    spdlog::error(ErrCode::Value::AccessNullArray);
    spdlog::error(
        ErrInfo::InfoInstruction(Instr.getOpCode(), Instr.getOffset()));
    return Unexpect(ErrCode::Value::AccessNullArray);
  }
  if (Idx >= Inst->getLength()) {
    // TODO: GC - error log
    spdlog::error(ErrCode::Value::ArrayOutOfBounds);
    spdlog::error(
        ErrInfo::InfoInstruction(Instr.getOpCode(), Instr.getOffset()));
    return Unexpect(ErrCode::Value::ArrayOutOfBounds);
  }
  const auto &SType = CompType.getFieldTypes()[0].getStorageType();
  Val = unpackVal(SType, Inst->getData(Idx), IsSigned);
  return {};
}

Expect<void>
Executor::runArrayLenOp(ValVariant &Val,
                        const AST::Instruction &Instr) const noexcept {
  const auto *Inst =
      Val.get<RefVariant>().asPtr<Runtime::Instance::ArrayInstance>();
  if (Inst == nullptr) {
    spdlog::error(ErrCode::Value::AccessNullArray);
    spdlog::error(
        ErrInfo::InfoInstruction(Instr.getOpCode(), Instr.getOffset()));
    return Unexpect(ErrCode::Value::AccessNullArray);
  }
  Val.emplace<uint32_t>(Inst->getLength());
  return {};
}

Expect<void>
Executor::runArrayFillOp(uint32_t N, const ValVariant &Val, uint32_t D,
                         const RefVariant &InstRef,
                         const AST::CompositeType &CompType,
                         const AST::Instruction &Instr) const noexcept {
  auto *Inst = InstRef.asPtr<Runtime::Instance::ArrayInstance>();
  if (Inst == nullptr) {
    // TODO: GC - error log
    spdlog::error(ErrCode::Value::AccessNullArray);
    spdlog::error(
        ErrInfo::InfoInstruction(Instr.getOpCode(), Instr.getOffset()));
    return Unexpect(ErrCode::Value::AccessNullArray);
  }
  if (static_cast<uint64_t>(D) + static_cast<uint64_t>(N) > Inst->getLength()) {
    // TODO: GC - error log
    spdlog::error(ErrCode::Value::ArrayOutOfBounds);
    spdlog::error(
        ErrInfo::InfoInstruction(Instr.getOpCode(), Instr.getOffset()));
    return Unexpect(ErrCode::Value::ArrayOutOfBounds);
  }
  const auto &SType = CompType.getFieldTypes()[0].getStorageType();
  for (uint32_t Idx = D; Idx < N + D; Idx++) {
    Inst->getData(Idx) = packVal(SType, Val);
  }
  return {};
}

Expect<void>
Executor::runArrayCopyOp(uint32_t N, uint32_t S, const RefVariant &InstSrcRef,
                         uint32_t D, const RefVariant &InstDstRef,
                         const AST::CompositeType &SrcCompType,
                         const AST::CompositeType &DstCompType,
                         const AST::Instruction &Instr) const noexcept {
  auto *InstSrc = InstSrcRef.asPtr<Runtime::Instance::ArrayInstance>();
  auto *InstDst = InstDstRef.asPtr<Runtime::Instance::ArrayInstance>();
  if (InstSrc == nullptr || InstDst == nullptr) {
    // TODO: GC - error log
    spdlog::error(ErrCode::Value::AccessNullArray);
    spdlog::error(
        ErrInfo::InfoInstruction(Instr.getOpCode(), Instr.getOffset()));
    return Unexpect(ErrCode::Value::AccessNullArray);
  }
  if (static_cast<uint64_t>(S) + static_cast<uint64_t>(N) >
      InstSrc->getLength()) {
    // TODO: GC - error log
    spdlog::error(ErrCode::Value::ArrayOutOfBounds);
    spdlog::error(
        ErrInfo::InfoInstruction(Instr.getOpCode(), Instr.getOffset()));
    return Unexpect(ErrCode::Value::ArrayOutOfBounds);
  }
  if (static_cast<uint64_t>(D) + static_cast<uint64_t>(N) >
      InstDst->getLength()) {
    // TODO: GC - error log
    spdlog::error(ErrCode::Value::ArrayOutOfBounds);
    spdlog::error(
        ErrInfo::InfoInstruction(Instr.getOpCode(), Instr.getOffset()));
    return Unexpect(ErrCode::Value::ArrayOutOfBounds);
  }
  const auto &SrcSType = SrcCompType.getFieldTypes()[0].getStorageType();
  const auto &DstSType = DstCompType.getFieldTypes()[0].getStorageType();
  for (uint32_t Off = 0; Off < N; Off++) {
    if (D <= S) {
      InstDst->getData(D + Off) =
          packVal(DstSType, unpackVal(SrcSType, InstSrc->getData(S + Off)));
    } else {
      InstDst->getData(D + N - Off - 1) = packVal(
          DstSType, unpackVal(SrcSType, InstSrc->getData(S + N - Off - 1)));
    }
  }
  return {};
}

Expect<void>
Executor::runArrayInitDataOp(uint32_t N, uint32_t S, uint32_t D,
                             const RefVariant &InstRef,
                             const AST::CompositeType &CompType,
                             const Runtime::Instance::DataInstance &DataInst,
                             const AST::Instruction &Instr) const noexcept {
  const uint32_t BSize =
      CompType.getFieldTypes()[0].getStorageType().getBitWidth() / 8;
  auto *Inst = InstRef.asPtr<Runtime::Instance::ArrayInstance>();
  if (Inst == nullptr) {
    // TODO: GC - error log
    spdlog::error(ErrCode::Value::AccessNullArray);
    spdlog::error(
        ErrInfo::InfoInstruction(Instr.getOpCode(), Instr.getOffset()));
    return Unexpect(ErrCode::Value::AccessNullArray);
  }
  if (static_cast<uint64_t>(D) + static_cast<uint64_t>(N) > Inst->getLength()) {
    // TODO: GC - error log
    spdlog::error(ErrCode::Value::ArrayOutOfBounds);
    spdlog::error(
        ErrInfo::InfoInstruction(Instr.getOpCode(), Instr.getOffset()));
    return Unexpect(ErrCode::Value::ArrayOutOfBounds);
  }
  if (static_cast<uint64_t>(S) + static_cast<uint64_t>(N) * BSize >
      DataInst.getData().size()) {
    // TODO: GC - error log
    spdlog::error(ErrCode::Value::MemoryOutOfBounds);
    spdlog::error(
        ErrInfo::InfoInstruction(Instr.getOpCode(), Instr.getOffset()));
    return Unexpect(ErrCode::Value::MemoryOutOfBounds);
  }
  for (uint32_t Off = 0; Off < N; Off++) {
    // The value has been packed.
    Inst->getData(D + Off) = DataInst.loadValue(S + Off * BSize, BSize);
  }
  return {};
}

Expect<void>
Executor::runArrayInitElemOp(uint32_t N, uint32_t S, uint32_t D,
                             const RefVariant &InstRef,
                             const AST::CompositeType &CompType,
                             const Runtime::Instance::ElementInstance &ElemInst,
                             const AST::Instruction &Instr) const noexcept {
  auto ElemSrc = ElemInst.getRefs();
  auto *Inst = InstRef.asPtr<Runtime::Instance::ArrayInstance>();
  if (Inst == nullptr) {
    // TODO: GC - error log
    spdlog::error(ErrCode::Value::AccessNullArray);
    spdlog::error(
        ErrInfo::InfoInstruction(Instr.getOpCode(), Instr.getOffset()));
    return Unexpect(ErrCode::Value::AccessNullArray);
  }
  if (static_cast<uint64_t>(D) + static_cast<uint64_t>(N) > Inst->getLength()) {
    // TODO: GC - error log
    spdlog::error(ErrCode::Value::ArrayOutOfBounds);
    spdlog::error(
        ErrInfo::InfoInstruction(Instr.getOpCode(), Instr.getOffset()));
    return Unexpect(ErrCode::Value::ArrayOutOfBounds);
  }
  if (static_cast<uint64_t>(S) + static_cast<uint64_t>(N) > ElemSrc.size()) {
    // TODO: GC - error log
    spdlog::error(ErrCode::Value::TableOutOfBounds);
    spdlog::error(
        ErrInfo::InfoInstruction(Instr.getOpCode(), Instr.getOffset()));
    return Unexpect(ErrCode::Value::TableOutOfBounds);
  }
  const auto &SType = CompType.getFieldTypes()[0].getStorageType();
  for (uint32_t Off = 0; Off < N; Off++) {
    // The value has been packed.
    Inst->getData(D + Off) = packVal(SType, ElemSrc[S + Off]);
  }
  return {};
}

Expect<void> Executor::runRefTestOp(Span<const AST::SubType *const> TList,
                                    ValVariant &Val,
                                    const AST::Instruction &Instr,
                                    bool IsCast) const noexcept {
  const auto &VT = Val.get<RefVariant>().getType();
  // TODO: GC - is this same-module-matching?
  if (AST::TypeMatcher::matchType(TList, Instr.getValType(), VT)) {
    if (!IsCast) {
      Val.emplace<uint32_t>(1U);
    }
  } else {
    if (IsCast) {
      // TODO: GC - error log
      spdlog::error(ErrCode::Value::CastFailed);
      spdlog::error(
          ErrInfo::InfoInstruction(Instr.getOpCode(), Instr.getOffset()));
      return Unexpect(ErrCode::Value::CastFailed);
    } else {
      Val.emplace<uint32_t>(0U);
    }
  }
  return {};
}

Expect<void> Executor::runRefConvOp(RefVariant &Ref,
                                    TypeCode TCode) const noexcept {
  if (Ref.isNull()) {
    Ref = RefVariant(ValType(TypeCode::RefNull, TCode));
  } else {
    Ref = RefVariant(ValType(TypeCode::Ref, TCode), Ref);
  }
  return {};
}

Expect<void> Executor::runRefI31Op(ValVariant &Val) const noexcept {
  uint32_t RefNum = (Val.get<uint32_t>() & 0x7FFFFFFFU) | 0x80000000U;
  Val = RefVariant(ValType(TypeCode::Ref, TypeCode::I31Ref),
                   reinterpret_cast<void *>(RefNum));
  return {};
}

Expect<void> Executor::runI31GetOp(ValVariant &Val,
                                   const AST::Instruction &Instr,
                                   bool IsSigned) const noexcept {
  uint32_t RefNum = static_cast<uint32_t>(
      reinterpret_cast<uintptr_t>(Val.get<RefVariant>().asPtr<void>()));
  if ((RefNum & 0x80000000U) == 0) {
    // TODO: GC - error log
    spdlog::error(ErrCode::Value::AccessNullI31);
    spdlog::error(
        ErrInfo::InfoInstruction(Instr.getOpCode(), Instr.getOffset()));
    return Unexpect(ErrCode::Value::AccessNullI31);
  }
  RefNum &= 0x7FFFFFFFU;
  if (IsSigned) {
    RefNum |= ((RefNum & 0x40000000U) << 1);
  }
  Val.emplace<uint32_t>(RefNum);
  return {};
}

} // namespace Executor
} // namespace WasmEdge
