
#include <string>
using std::string;
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

void Variable::substitute_internal(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env)
{
  if (id == var)
  {
    (*term) = value->clone();
  }
}

bool Variable::appears_free_internal(string var)
{
  if (id == var)
    return true;
  else
    return false;
}

void Variable::rename_binding_internal(string old_name, string new_name)
{
  if (id == old_name)
    id = new_name;
}
