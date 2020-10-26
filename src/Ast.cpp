
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Location.hpp"
#include "Type.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Environment.hpp"
#include "Ast.hpp"

shared_ptr<Ast> Ast::clone()
{
  return clone_internal();
}

string Ast::to_string()
{
  return to_string_internal();
}

TypeJudgement Ast::getype(Environment env)
{
  return getype_internal(env);
}

EvalJudgement Ast::evaluate(Environment env)
{
  return evaluate_internal(env);
}
