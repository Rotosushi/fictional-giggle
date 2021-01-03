#include <string>
using std::string;
#include <memory>
using std::shared_ptr;
using std::unique_ptr;

#include "Ast.hpp"
#include "Environment.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Object.hpp"
#include "TypeLiteral.hpp"


unique_ptr<Object> TypeLiteral::clone()
{
  return unique_ptr<Object>(new TypeLiteral(*this));
}

string TypeLiteral::to_string()
{
  return value->to_string();
}

TypeJudgement TypeLiteral::getype(Environment& env)
{
  return TypeJudgement(value);
}

void TypeLiteral::substitute(string& var, shared_ptr<Ast>* term, shared_ptr<Ast>& value, Environment& env)
{
  return;
}

bool TypeLiteral::appears_free(string& var)
{
  return false;
}

void TypeLiteral::rename_binding(string& old_name, string& new_name)
{
  return;
}
