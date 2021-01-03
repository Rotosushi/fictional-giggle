
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
#include "TypeJudgement.hpp"
#include "Entity.hpp"
#include "Bind.hpp"

shared_ptr<Ast> Bind::clone_internal()
{
  return shared_ptr<Ast>(new Bind(id, rhs->clone(), location));
}

string Bind::to_string_internal()
{
  string result;
  result  = id;
  result += " := ";
  result += rhs->to_string();
  return result;
}

TypeJudgement Bind::getype_internal(Environment& env)
{
  /*
ENV |- id is-not-currently-bound-in-local-scope, term2 : type2
      ------------------------------------------
            ENV |- id := term2 : type2
  */
  optional<shared_ptr<Ast>> sym = env.scope->lookupInLocalScopeOnly(id);

  if (sym)
  {
    string errdsc = "id ["
                  + id
                  + "] is already bound to ["
                  + (*sym)->to_string()
                  + "]\n";
    return TypeJudgement(TypeError(location, errdsc));
  }
  else
  {
    // recall that judgements track their own failure
    // state, so it need not be explicitly mentioned
    // that this call can fail here. as such
    // it is a prime candidate to be a one-liner,
    // however, that isn't debugger friendly.

    TypeJudgement rhsjdgmt = rhs->getype(env);

    if (!rhsjdgmt)
      return rhsjdgmt;

    env.scope->bind(id, shared_ptr<Ast>(new Entity(rhsjdgmt.u.jdgmt, rhs->location)));
    env.cleanup_list->push_back(id);

    return rhsjdgmt;
  }
}

EvalJudgement Bind::evaluate_internal(Environment& env)
{
  auto is_entity = [](shared_ptr<Ast> term)
  {
    Entity* entity = dynamic_cast<Entity*>(term.get());
    return entity != nullptr;
  };
  optional<shared_ptr<Ast>> sym = env.scope->lookupInLocalScopeOnly(id);

  if (sym)
  {
    string errdsc = "id ["
                  + id
                  + "] is already bound to ["
                  + (*sym)->to_string()
                  + "]\n";
    return EvalJudgement(EvalError(location, errdsc));
  }
  else
  {
    EvalJudgement result = rhs->evaluate(env);

    if (!result)
      return result;

    env.scope->bind(id, result.u.jdgmt);
    return result;
  }
}


void Bind::substitute_internal(string& var, shared_ptr<Ast>* term, shared_ptr<Ast>& value, Environment& env)
{
  rhs->substitute(var, &rhs, value, env);
}

bool Bind::appears_free_internal(string& var)
{
  return rhs->appears_free(var);
}

void Bind::rename_binding_internal(string& old_name, string& new_name)
{
  rhs->rename_binding(old_name, new_name);
}
