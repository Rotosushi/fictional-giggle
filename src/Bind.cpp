
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

}
