
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
    return TypeJudgement(TypeError(location, "variable [" + id + "] not defined in environment."));
  }
}

EvalJudgement Variable::evaluate_internal(Environment env)
{

}
