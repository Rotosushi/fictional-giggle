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
#include "Error.hh"
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

/*
  i just noticed that a tagged union
  could be implemented with the same
  memory layout, if we treat the first
  value as an enumeration of the possible
  held union types. the boolean is
  essentially acting as a two value
  enum, with true being mapped to type,
  and false being mapped to error.

*/
struct Judgement {
  bool succeeded;
  union U {
    unique_ptr<Ast> jdgmt;
    TypeError       error;

    U() : error() {}
    U(unique_ptr<Ast> t) : jdgmt(move(t)) {}
    U(PrimitiveType t) : jdgmt(make_unique<EntityNode>(AtomicType(t), Location())) {}
    U(const TypeError& err) : error(err) {}
    ~U() {}
  } u;

  Judgement() : succeeded(false), u(TypeError(Location(), "Default Constructed Judgement")) {}
  Judgement(unique_ptr<Ast> t) : succeeded(true), u(move(t)) {}
  Judgement(PrimitiveType t) : succeeded(true), u(t) {}
  Judgement(const AtomicType* const t) : succeeded(true), u(make_unique<EntityNode>(*t, Location())) {}
  Judgement(const ProcType* const t) : succeeded(true), u(make_unique<EntityNode>(*t, Location())) {}
  Judgement(const TypeError& err) : succeeded(false), u(err) {}
  Judgement(const Location& loc, const string& str) : succeeded(false), u(TypeError(loc, str)) {}

  operator bool()
  {
    return succeeded;
  }
};

class Typechecker {
public:

  static Judgement equivalent(const TypeNode* t1, const TypeNode* t2);

  static Judgement getype(const Ast* const a, SymbolTable* env);
  static Judgement getype(const EmptyNode* const e, SymbolTable* env);
  static Judgement getype(const VariableNode* const v, SymbolTable* env);
  static Judgement getype(const CallNode* const c, SymbolTable* env);
  static Judgement getype(const BindNode* const b, SymbolTable* env);
  static Judgement getype(const BinopNode* const b, SymbolTable* env);
  static Judgement getype(const UnopNode* const u, SymbolTable* env);
  static Judgement getype(const CondNode* const c, SymbolTable* env);
  static Judgement getype(const EntityNode* const e, SymbolTable* env);
  static Judgement getype(const Procedure* const p, SymbolTable* env);
  static Judgement getype(const TypeNode* const t, SymbolTable* env);
  static Judgement getype(const AtomicType* const t, SymbolTable* env);
  static Judgement getype(const ProcType* const t, SymbolTable* env);

};
