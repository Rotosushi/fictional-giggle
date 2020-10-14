#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Ast.hpp"
#include "SymbolTable.hpp"
#include "OperatorTable.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Unop.hpp"

shared_ptr<Ast> Unop::clone_internal()
{
  return make_shared(Unop(op, rhs->clone(), location));
}

string Unop::to_string_internal()
{
  string result;
  result  = op;
  result += " ";
  result += rhs->to_string();
  return result;
}

TypeJudgement Unop:getype_internal(SymbolTable* env, OperatorTable* ops)
{

}

EvalJudgement Unop::evaluate_internal(SymbolTable* env, OperatorTable* ops)
{

}
