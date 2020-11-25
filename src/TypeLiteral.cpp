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

TypeJudgement TypeLiteral::getype(Environment env)
{
  return value;
}

void TypeLiteral::substitute(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env)
{
  return;
}

void TypeLiteral::rename_binding(string old_name, string new_name)
{
  return;
}

bool TypeLiteral::appears_free(string name)
{
  return false;
}
