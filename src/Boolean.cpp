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
#include "Boolean.hpp"


unique_ptr<Object> Boolean::clone()
{
  return unique_ptr<Object>(new Boolean(*this));
}

string Boolean::to_string()
{
  if (value)
    return "true";
  else
    return "false";
}

TypeJudgement Boolean::getype(Environment env)
{
  return TypeJudgement(shared_ptr<Type>(new MonoType(AtomicType::Bool, Location())));
}

void Boolean::substitute(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env)
{
  return;
}

void Boolean::rename_binding(string old_name, string new_name)
{
  return;
}

bool Boolean::appears_free(string name)
{
  return false;
}
