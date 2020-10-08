
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Ast.hpp"
#include "SymbolTable.hpp"
#include "Variable.hpp"

string Variable::to_string_internal()
{
  return id;
}

TypeJudgement Variable::getype_internal(SymbolTable* env)
{
  /*
      id is-a-member-of env
  ---------------------------
  env |- (id : type = value) : type
  */
  optional<shared_ptr<Ast>> bound_term = (*env)[id];

  if (bound_term)
  {
    TypeJudgement bound_type = (*bound_term)->getype(env);
    return bound_type;
  }
  else
  {
    return Judgement(location, "variable [" + id + "] not defined in environment.");
  }
}

EvalJudgement Variable::evaluate_internal(SymbolTable* env)
{

}
