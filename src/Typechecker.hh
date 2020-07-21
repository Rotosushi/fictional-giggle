#pragma once
#include <string>
using std::string;
#include <memory>
using std::unique_ptr;
#include <utility>
using std::move;
using std::optional;
using std::pair;
using std::get;

#include "llvm-10/llvm/IR/LLVMContext.h"

#include "Ast.hh"
#include "SymbolTable.hh"
#include "OperatorTable.hh"


class Typechecker {
  LLVMContext* ctx;
public:

  Typechecker(LLVMContext* c) : ctx(c) {}

  bool equivalent(const Type * const t1, const Type * const t2);
  bool is_polymorphic(const Type* const t);
  optional<unique_ptr<Ast>> HasInstance(const ProcSet* const, const Type* const t, SymbolTable* env);

  unique_ptr<Type> getype(const Empty* const e, SymbolTable* env);
  unique_ptr<Type> getype(const Variable* const v, SymbolTable* env);
  unique_ptr<Type> getype(const Call* const c, SymbolTable* env);
  unique_ptr<Type> getype(const Bind* const b, SymbolTable* env);
  unique_ptr<Type> getype(const Binop* const b, SymbolTable* env);
  unique_ptr<Type> getype(const Unop* const u, SymbolTable* env);
  unique_ptr<Type> getype(const Cond* const c, SymbolTable* env);
  unique_ptr<Type> getype(const Entity* const e, SymbolTable* env);
  unique_ptr<Type> getype(const ProcSet* const p, SymbolTable* env);
  unique_ptr<Type> getype(const MonoType* const m, SymbolTable* env);
  unqiue_ptr<Type> getype(const ProcType* const p, SymbolTable* env);

};
