#include <string>
using std::string;
#include <vector>
using std::vector;
#include <utility>
using std::pair;
using std::get;
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
  return shared_ptr<Ast>(new Unop(op, rhs->clone(), location));
}

string Unop::to_string_internal()
{
  string result;
  result  = op;
  result += rhs->to_string();
  return result;
}

TypeJudgement Unop::getype_internal(Environment env)
{
  auto UnopEliminators = env.unops->FindUnop(op);

  if (UnopEliminators)
  {
    auto rhsjdgmt = rhs->getype(env);

    if (!rhsjdgmt)
      return rhsjdgmt;

    if (rhsjdgmt.u.jdgmt->is_polymorphic())
      return TypeJudgement(shared_ptr<Type>(new MonoType(AtomicType::Poly, Location())));

    shared_ptr<Type> rhstype = rhsjdgmt.u.jdgmt;

    auto eliminator = (*UnopEliminators)->HasEliminator(rhstype);

    if (eliminator)
    {
      return TypeJudgement(eliminator->GetResultType());
    }
    else
    {
      // error, no instance found for actual argument type
      string errdsc = "No instance of unop ["
                    + op
                    + "] for actual argument type ["
                    + rhs->to_string()
                    + "]\n";
      return TypeJudgement(TypeError(location, errdsc));
    }
  }
  else
  {
    // error, no unop found for symbol (op)
    string errdsc = "No unop found for symbol ["
                  + op
                  + "]\n";
    return TypeJudgement(TypeError(location, errdsc));
  }
}

EvalJudgement Unop::evaluate_internal(Environment env)
{
  auto UnopEliminators = env.unops->FindUnop(op);

  if (UnopEliminators)
  {
    EvalJudgement rhsEvalJdgmt = rhs->evaluate(env);

    if (!rhsEvalJdgmt)
      return rhsEvalJdgmt;

    auto rhsTypeJdgmt = rhsEvalJdgmt.u.jdgmt->getype(env);

    if (!rhsTypeJdgmt)
      return EvalJudgement(EvalError(rhsTypeJdgmt.u.error));

    shared_ptr<Type> rhstype = rhsTypeJdgmt.u.jdgmt;

    auto eliminator = (*UnopEliminators)->HasEliminator(rhstype);

    if (eliminator)
    {
      shared_ptr<Ast> result = (*eliminator)(rhsEvalJdgmt.u.jdgmt);
      return EvalJudgement(result);
    }
    else
    {
      // error, no instance found for actual argument type
      string errdsc = "No instance of unop ["
                    + op
                    + "] for actual argument type ["
                    + rhs->to_string()
                    + "]\n";
      return EvalJudgement(EvalError(location, errdsc));
    }
  }
  else
  {
    // error, no unop found for symbol (op)
    string errdsc = "No unop found for symbol ["
                  + op
                  + "]\n";
    return EvalJudgement(EvalError(location, errdsc));
  }
}

void Unop::substitute_internal(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env)
{
  rhs->substitute(subs, &rhs, env);
}

bool Unop::appears_free_internal(vector<string>& names, vector<string>& appeared_free)
{
  return rhs->appears_free(names, appeared_free);
}

void Unop::rename_binding_in_body_internal(vector<pair<string, string>>& renaming_pairs)
{
  rhs->rename_binding_in_body(renaming_pairs);
}
