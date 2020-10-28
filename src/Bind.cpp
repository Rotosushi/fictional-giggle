
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;
using std::make_shared;

#include "Ast.hpp"
#include "Environment.hpp"
#include "TypeJudgement.hpp"
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

TypeJudgement Bind::getype_internal(Environment env)
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
    return rhsjdgmt;
  }
}

EvalJudgement Bind::evaluate_internal(Environment env)
{
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
    return result;
  }
}

void Bind::substitute(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env)
{
  rhs->substitute(var, &rhs, value, env);
}

bool Bind::appears_free(string var)
{
  return rhs->appears_free(var);
}

void Bind::rename_binding(string old_name, string new_name)
{
  rhs->rename_binding(old_name, new_name);
}
