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
#include <stack>
using std::stack;

#include "llvm-10/llvm/IR/LLVMContext.h"

#include "Ast.hh"
#include "Error.hh"
#include "SymbolTable.hh"
#include "BinopTable.hh"
#include "UnopTable.hh"

class BinopTable;

/*
  A typing judgement has two possible
  results we care about.
  we care if the judgement fails or succeeds,
  and given failure or success we care about
  either the failure, to report it to the user;
  or the success, where we care about the type result.

  to support both of these control paths, we need to
  combine these four peices of information
  (success or failure, error message or result type.)
  into the result type of the procedure
  which carries out the typeing judgements.

  an additional complication was the function
  HasInstance, which needs to return a full on
  Ast. in which types are not directly representable,
  unless wrapped in an entity structure. but
  given we just let HasInstance be unique, we
  can store a TypeNode* directly, and then we get
  to save on time and space within the typechecker,
  as each judgement isn't the size of a procedure,
  (my assumed largest union member, as it has so many
  standard library data structure members.)
*/

/*
  i just noticed that a tagged union
  could be implemented with the same
  memory layout, if we treat the first
  value as an enumeration of the possible
  held union types. the boolean is
  essentially acting as a two value
  enum, with true being mapped to type,
  and false being mapped to error.
  so, the enum maps which elem is
  currently active within the record.
*/
struct Judgement {
  bool succeeded;
  union U {
    unique_ptr<TypeNode> jdgmt;
    TypeError       error;

    U() : error() {}
    U(unique_ptr<TypeNode> t) : jdgmt(move(t)) {}
    U(const TypeNode* const t) : jdgmt(t->clone()) {}
    U(const AtomicType* const t) : jdgmt(t->clone()) {}
    U(const ProcType* const t) : jdgmt(t->clone()) {}
    U(PrimitiveType t) : jdgmt(make_unique<AtomicType>(AtomicType(t), Location())) {}
    U(const TypeError& err) : error(err) {}
    ~U() {}
  } u;

  Judgement() : succeeded(false), u(TypeError(Location(), "Default Constructed Judgement")) {}
  Judgement(unique_ptr<TypeNode> t) : succeeded(true), u(move(t)) {}
  Judgement(PrimitiveType t) : succeeded(true), u(t) {}
  Judgement(const TypeNode* const t) : succeeded(true), u(t) {}
  Judgement(const AtomicType* const t) : succeeded(true), u(t) {}
  Judgement(const ProcType* const t) : succeeded(true), u(t) {}
  Judgement(const TypeError& err) : succeeded(false), u(err) {}
  Judgement(const Location& loc, const string& str) : succeeded(false), u(TypeError(loc, str.data())) {}
  Judgement(const Judgement& rhs)
    : succeeded(rhs.succeeded)
  {
    if (succeeded)
    {
      u.jdgmt = rhs.u.jdgmt->clone();
    }
    else
    {
      u.error = rhs.u.error;
    }
  }


  operator bool()
  {
    return succeeded;
  }
};

Judgement equivalent(const TypeNode* t1, const TypeNode* t2);

class Typechecker {
  stack<SymbolTable*> scopes;
  BinopTable* binops;
  UnopTable* unops;

public:

  Typechecker(SymbolTable* e, BinopTable* b, BinopTable* u)
    : binops(b), unops(u) {
      scopes.push(e);
    }

  optional<ProcedureLiteral> GetInstanceOf(Procedure* proc_def, const TypeNode* const target_type);

  bool ValidBinopArgTypes(TypeNode* binop_elim_type, TypeNode* lhs_type, TypeNode* rhs_type);
  bool ValidUnopArgType(TypeNode* unop_elim_type, TypeNode* rhs_type);

  Judgement getype(const Ast* const a);
  Judgement getype(const EmptyNode* const e);
  Judgement getype(const VariableNode* const v);
  Judgement getype(const CallNode* const c);
  Judgement getype(const BindNode* const b);
  Judgement getype(const BinopNode* const b);
  Judgement getype(const UnopNode* const u);
  Judgement getype(const CondNode* const c);
  Judgement getype(const WhileNode* const w);
  Judgement getype(const EntityNode* const e);
  Judgement getype(const Procedure* const p);
  Judgement getype(const TypeNode* const t);
  Judgement getype(const AtomicType* const t);
  Judgement getype(const ProcType* const t);

};
