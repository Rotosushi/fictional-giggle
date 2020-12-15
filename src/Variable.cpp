
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <utility>
using std::pair;
using std::get;
#include <memory>
using std::shared_ptr;
using std::make_shared;

#include "Ast.hpp"
#include "Environment.hpp"
#include "Variable.hpp"

shared_ptr<Ast> Variable::clone_internal()
{
  return shared_ptr<Ast>(new Variable(id, location));
}

string Variable::to_string_internal()
{
  return id;
}

TypeJudgement Variable::getype_internal(Environment env)
{
  /*
      id is-a-member-of env
  ---------------------------
  env |- (id : type = value) : type
  */
  optional<shared_ptr<Ast>> bound_term = (*env.scope)[id];

  if (bound_term)
  {
    TypeJudgement bound_type = (*bound_term)->getype(env);
    return bound_type;
  }
  else
  {
    return TypeJudgement(TypeError(location, "variable [" + id + "] not bound in environment."));
  }
}

EvalJudgement Variable::evaluate_internal(Environment env)
{
/*
ENV |- id : type = value
-------------------------
    id -> value
*/

  optional<shared_ptr<Ast>> term = (*env.scope)[id];

  if (term)
  {
    return EvalJudgement(*term);
  }
  else
  {
    string errdsc = "variable ["
                  + id
                  + "] not bound in environment.";
    return EvalJudgement(EvalError(location, errdsc));
  }
}

void Variable::substitute_internal(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env)
{
  for (auto&& sub : subs)
  {
    if (get<string>(sub) == id)
    {
      (*term) = get<shared_ptr<Ast>>(sub)->clone();
      break;
    }
  }
}

bool Variable::appears_free_internal(vector<string>& names, vector<string>& appeared_free)
{
  /*
  if (id == var)
    return true;
  else
    return false;
  */
  for (auto&& name : names)
    if (id == name)
    {
      insert_if_unique(name, appeared_free);
      return true;
    }

  return false;
}

void Variable::rename_binding_in_body_internal(vector<pair<string, string>>& renaming_pairs)
{
  for (auto&& pair : renaming_pairs)
    if (get<0>(pair) == id)
    {
      id = get<1>(pair);
      break;
    }
}
