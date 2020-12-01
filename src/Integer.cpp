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
#include "Integer.hpp"

unique_ptr<Object> Integer::clone()
{
  return unique_ptr<Object>(new Integer(*this));
}

string Integer::to_string()
{
  return std::to_string(value);
}

TypeJudgement Integer::getype(Environment env)
{
  return TypeJudgement(shared_ptr<Type>(new MonoType(AtomicType::Int, Location())));
}

void Integer::substitute(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env)
{
  return;
}

void Integer::rename_binding_in_body_internal(vector<pair<string, string>>& renaming_pairs)
{
  return;
}

bool Integer::appears_free(string name)
{
  return false;
}
