
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;
using std::make_shared;
#include <tuple>
using std::tuple;
using std::get;
using std::make_tuple;

#include "Ast.hpp"
#include "Environment.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "BinopEliminators.hpp"
#include "Binop.hpp"

shared_ptr<Ast> Binop::clone_internal()
{
  return shared_ptr<Ast>(new Binop(op, lhs->clone(), rhs->clone(), location));
}

string Binop::to_string_internal()
{
  string result;
  result += lhs->to_string();
  result += " ";
  result += op;
  result += " ";
  result += rhs->to_string();
  return result;
}

TypeJudgement Binop::getype_internal(Environment env)
{
  /*
    we find the Binop Eliminator via the BinopSet
    then ask the Eliminator for something with the
    types of the left and right hand side.
    we are relying on this Eliminator Lookup procedure
    to then uniquely select an elimination
    procedure. if this happens then this term is
    well formed. otherwise this term is untypable.
  */
  auto BinopEliminators = env.binops->FindBinop(op);

  if (BinopEliminators)
  {
    auto lhsjdgmt = lhs->getype(env);

    if (!lhsjdgmt)
      return lhsjdgmt;

    auto rhsjdgmt = rhs->getype(env);

    if (!rhsjdgmt)
      return rhsjdgmt;

    shared_ptr<Type> lhstype = lhsjdgmt.u.jdgmt;
    shared_ptr<Type> rhstype = rhsjdgmt.u.jdgmt;

    auto eliminator = (*BinopEliminators)->HasEliminator(lhstype, rhstype);

    if (eliminator)
    {
      return TypeJudgement(eliminator->GetResultType());
    }
    else
    {
      // error, no instance of binop valid for actual argument types.
      string errdsc = "No instance of binop ["
                    + op
                    + "] for actual lhstype:["
                    + lhstype->to_string()
                    + "] and actual rhstype:["
                    + rhstype->to_string()
                    + "]\n";
      return TypeJudgement(TypeError(location, errdsc));
    }
  }
  else
  {
    // error, no binop by that symbol
    string errdsc = "No binop found for symbol ["
                  + op
                  + "]\n";
    return TypeJudgement(TypeError(location, errdsc));
  }
}

EvalJudgement Binop::evaluate_internal(Environment env)
{
  auto BinopEliminators = env.binops->FindBinop(op);

  if (BinopEliminators)
  {
    auto lhsTypeJdgmt = lhs->getype(env);

    if (!lhsTypeJdgmt)
      return lhsTypeJdgmt;

    auto rhsTypeJdgmt = rhs->getype(env);

    if (!rhsTypeJdgmt)
      return rhsTypeJdgmt;

    shared_ptr<Type> lhstype = lhsTypeJdgmt.u.jdgmt;
    shared_ptr<Type> rhstype = rhsTypeJdgmt.u.jdgmt;

    auto eliminator = (*BinopEliminators)->HasEliminator(lhstype, rhstype);

    if (eliminator)
    {
      auto lhsEvalJdgmt = lhs->evaluate(env);

      if (!lhsEvalJdgmt)
        return lhsEvalJdgmt;

      auto rhsEvalJdgmt = rhs->evaluate(env);

      if (!rhsEvalJdgmt)
        return rhsEvalJdgmt;

      shared_ptr<Ast> lhsvalue = lhsEvalJdgmt.u.jdgmt;
      shared_ptr<Ast> rhsvalue = rhsEvalJdgmt.u.jdgmt;
      return EvalJudgement((*eliminator)(lhsvalue, rhsvalue));
    }
    else
    {
      // error, no instance of binop valid for actual argument types.
      string errdsc = "No instance of binop ["
                    + op
                    + "] for actual lhstype:["
                    + lhstype->to_string()
                    + "] and actual rhstype:["
                    + rhstype->to_string()
                    + "]\n";
      return EvalJudgement(EvalError(location, errdsc));
    }
  }
  else
  {
    // error, no binop by that symbol
    string errdsc = "No binop found for symbol ["
                  + op
                  + "]\n";
    return EvalJudgement(EvalError(location, errdsc));
  }
}

void Binop::substitute(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env)
{
  lhs->substitute(var, &lhs, value, env);
  rhs->substitute(var, &rhs, value, env);
}

bool Binop::appears_free(string var)
{
  return lhs->appears_free(var) || rhs->appears_free(var);
}

void Binop::rename_binding(string old_name, string new_name)
{
  lhs->rename_binding(old_name, new_name);
  rhs->rename_binding(old_name, new_name);
}
