#include <string>
using std::string;
#include <vector>
using std::vector;
#include <utility>
using std::pair;
using std::get;
#include <memory>
using std::shared_ptr;
using std::unique_ptr;

#include "Ast.hpp"
#include "Environment.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Object.hpp"
#include "Integer.hpp"

unique_ptr<Object> Integer::clone()
{
  return unique_ptr<Object>(new Integer(*this));
}

string Integer::to_string()
{
  return std::to_string(value);
}

TypeJudgement Integer::getype(Environment& env)
{
  return TypeJudgement(shared_ptr<Type>(new MonoType(AtomicType::Int, Location())));
}

void Integer::substitute(string& var, shared_ptr<Ast>* term, shared_ptr<Ast>& value, Environment& env)
{
  return;
}

void Integer::rename_binding(string& old_name, string& new_name)
{
  return;
}

bool Integer::appears_free(string& var)
{
  return false;
}
