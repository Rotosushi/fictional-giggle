
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
  
  return evaluate_internal(env, ops);
}
