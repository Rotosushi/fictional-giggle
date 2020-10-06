#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Ast.hpp"
#include "SymbolTable.hpp"
#include "TypeJudgement.hpp"
#include "Bind.hpp"

string Bind::to_string_internal()
{
  string result;
  result  = id;
  result += " := ";
  result += rhs->to_string();
  return result;
}

TypeJudgement Bind::getype_internal(SymbolTable* env)
{
  /*
ENV |- id is-not-currently-bound-in-local-scope, term2 : type2
      ------------------------------------------
            ENV |- id := term2 : type2
  */
  optional<shared_ptr<Ast>> sym = env->lookupInLocalScopeOnly(id);

  if (sym)
  {
    string errdsc = "id ["
                  + id
                  + "] is already bound to ["
                  + sym->to_string()
                  + "]\n";
    return TypeJudgement(location, errdsc);
  }
  else
  {
    // recal that judgements track their own failure
    // state, so it need not be explicitly mentioned
    // that this call can fail here.
    TypeJudgement rhstype = rhs->getype(env);
    return rhstype;
  }
}

EvalJudgement Bind::evaluate_internal(SymbolTable* env)
{

}
