
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Location.hpp"
#include "Type.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "SymbolTable.hpp"
#include "OperatorTable.hpp"

shared_ptr<Ast> Ast::clone()
{
  return clone_internal();
}

string Ast::to_string()
{
  return to_string_internal();
}

TypeJudgement Ast::getype(SymbolTable* env, OperatorTable* ops)
{
  return getype_internal(env, ops);
}

EvalJudgement Ast::evaluate(SymbolTable* env, OperatorTable* ops)
{
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
  return evaluate_internal(env, ops);
}
