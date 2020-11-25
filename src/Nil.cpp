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
#include "Nil.hpp"


unique_ptr<Object> Nil::clone()
{
  return unique_ptr<Object>(new Nil(*this));
}

string Nil::to_string()
{
  return "nil";
}

TypeJudgement Nil::getype(Environment env)
{
  return TypeJudgement(shared_ptr<Type>(new MonoType(AtomicType::Nil, Location())));
}

void Nil::substitute(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env)
{
  return;
}

void Nil::rename_binding(string old_name, string new_name)
{
}

bool Nil::appears_free(string name)
{
  return false;
}
