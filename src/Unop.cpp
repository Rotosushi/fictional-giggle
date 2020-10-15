#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Ast.hpp"
#include "Environment.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "UnopEliminators.hpp"
#include "Unop.hpp"

shared_ptr<Ast> Unop::clone_internal()
{
  return make_shared(Unop(op, rhs->clone(), location));
}

string Unop::to_string_internal()
{
  string result;
  result  = op;
  result += " ";
  result += rhs->to_string();
  return result;
}

TypeJudgement Unop:getype_internal(Environment env)
{
  auto UnopEliminators = env.unops->FindUnops(op);

  if (UnopELiminators)
  {
    auto rhsjdgmt = rhs->getype(env);

    if (rhsjdgmt)
      return rhsjdgmt;

    shared_ptr<Type> rhstype = rhsjdgmt.u.jdgmt;

    auto eliminator = UnopEliminators->HasEliminator(rhstype);

    if (eliminator)
    {
      return TypeJudgement(eliminator->result_type());
    }
    else
    {
      // error, no instance found for actual argument type
      string errdsc = "No instance of unop ["
                    + op
                    + "] for actual argument type ["
                    + rhs->to_string()
                    + "]\n";
      return TypeJudgement(location, errdsc);
    }
  }
  else
  {
    // error, no unop found for symbol (op)
    string errdsc = "No unop found for symbol ["
                  + op
                  + "]\n";
    return TypeJudgement(location, errdsc);
  }
}

EvalJudgement Unop::evaluate_internal(Environment env)
{

}
