
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Location.hpp"
#include "Type.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "SymbolTable.hpp"

string Ast::to_string()
{
  return to_string_internal();
}

TypeJudgement Ast::getype(SymbolTable* env, BinopSet* binops)
{
  return getype_internal(env, binops);
}

EvalJudgement Ast::evaluate(SymbolTable* env, BinopSet* binops)
{
  return evaluate_internal(env, binops);
}
