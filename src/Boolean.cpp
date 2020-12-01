#include <string>
using std::string;
#include <vector>
using std::vector>
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

void Boolean::substitute(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env)
{
  return;
}

void Boolean::rename_binding_in_body_internal(vector<pair<string, string>>& renaming_pairs)
{
  return;
}

bool Boolean::appears_free(vector<string>& names, vector<string>& appeared_free)
{
  return false;
}
