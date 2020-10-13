#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Ast.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Unop.hpp"

string Unop::to_string_internal()
{
  string result;
  result  = op;
  result += " ";
  result += rhs->to_string();
  return result;
}

TypeJudgement Unop:getype_internal(SymbolTable* env, BinopSet*)
