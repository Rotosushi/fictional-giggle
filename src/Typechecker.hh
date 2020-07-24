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

/*
  A typing judgement has two possible
  results we care about.
  we care if the judgement fails or succeeds,
  and given failure or success we care about
  either the failure (to report it to the user)
  or the success, where we care about the type result.

  to support both of these control paths, we need to
  combine these four peices of information into
  the result type of the procedure which carries out
  the typeing judgements.
*/

struct Judgement {
  bool succeeded;
  union {
    unique_ptr<TypeNode> type;
    TypeError       error;
  };

  Judgement() : succeeded(false), error(Location(), "Default Constructed Judgement") {}
  Judgement(unique_ptr<TypeNode> t) : succeeded(true), type(t) {}
  Judgement(const TypeError& err) : succeeded(false), error(err) {}

  operator bool()
  {
    return succeeded;
  }
  // this is a spicy language feature c++
  operator unique_ptr<TypeNode>()
  {
    return type.release();
  }

  operator TypeError()
  {
    return error;
  }
};

class Typechecker {
  LLVMContext* ctx;
public:

  Typechecker(LLVMContext* c) : ctx(c) {}

  Judgement equivalent(const EntityNode& t1, const EntityNode& t2);
  Judgement HasInstance(EntityNode& proc, const TypeNode* const type, SymbolTable* env);

  Judgement getype(const EmptyNode* const e, SymbolTable* env);
  Judgement getype(const VariableNode* const v, SymbolTable* env);
  Judgement getype(const CallNode* const c, SymbolTable* env);
  Judgement getype(const BindNode* const b, SymbolTable* env);
  Judgement getype(const BinopNode* const b, SymbolTable* env);
  Judgement getype(const UnopNode* const u, SymbolTable* env);
  Judgement getype(const CondNode* const c, SymbolTable* env);
  Judgement getype(const EntityNode* const e, SymbolTable* env);

};
