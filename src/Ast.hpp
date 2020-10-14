#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Location.hpp"
#include "Type.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"

// these must be forward declared to compile the program.
class SymbolTable;
class OperatorTable;


class Ast
{
public:
  Location location;
  Ast() = delete;
  Ast(const Location& loc) : location(loc) {}
  Ast(const Ast& other) : location(other.location) {}
  virtual ~Ast() = default;

  virtual shared_ptr<Ast> clone();
  virtual string to_string();
  virtual TypeJudgement getype(SymbolTable* env, OperatorTable* ops);
  /*
    so, the term we are observing to potentially replace
    is only accessable via a pointer. since this pointer
    is itself always stored within the parent node to the
    node being observed, if we want to be sure we are replacing
    the right node, AND define this procedure from the
    perspective of each node defining substitution in a
    mutually recursive fashion, we must have a pointer to
    the pointer to the term, so that we can potentially assign
    a new term to the parent pointer. (this is the exact pointer
    we will have a pointer too, and each mutual recursion will
    set up the correct parent pointer to be potentially replaced.)

    also, i am observing that this procedure is also defined
    within a switch statement over Ast objects. meaning we need
    to define substitution as a virtual method of Ast's themselves.

    additionally, in only one case do we ever perform the replacement
    itself, and that is if we are observing a variable which
    has the same id as the argument we are replacing for.
  */
  virtual EvalJudgement evaluate(SymbolTable* env, OperatorTable* ops);
protected:
  virtual shared_ptr<Ast> clone_internal() = 0;
  virtual string to_string_internal() = 0;
  virtual TypeJudgement getype_internal(SymbolTable* env, OperatorTable* ops) = 0;
  virtual EvalJudgement evaluate_internal(SymbolTable* env, OperatorTable* ops) = 0;
};
