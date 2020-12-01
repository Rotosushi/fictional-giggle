
#include <string>
using std::string;
#include <list>
using std::list;
#include <vector>
using std::vector>
#include <utility>
using std::pair;
using std::get;
#include <memory>
using std::shared_ptr;
using std::unique_ptr;
using std::make_shared;
using std::make_unique;

#include "Ast.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Environment.hpp"
#include "Entity.hpp"

shared_ptr<Ast> Entity::clone_internal()
{
  return shared_ptr<Ast>(new Entity(*this));
}

string Entity::to_string_internal()
{
  return literal->to_string();
}

TypeJudgement Entity::getype_internal(Environment env)
{
  return literal->getype(env);
}

EvalJudgement Entity::evaluate_internal(Environment env)
{
  return EvalJudgement(shared_ptr<Ast>(new Entity(*this)));
}

void Entity::substitute_internal(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env)
{
  return literal->substitute(subs, term, env);
}

bool Entity::appears_free_internal(string var)
{
  return literal->appears_free(var);
}

void Entity::rename_binding_in_body_internal(vector<pair<string, string>>& renaming_pairs)
{
  literal->rename_binding(old_name, new_name);
}
