#pragma once
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
  return make_shared(Binop(op, lhs->clone(), rhs->clone(), location));
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

    shared_ptr<Type> lhstype = lhsjdgmt.u.judgement;
    shared_ptr<Type> rhstype = rhsjdgmt.u.judgement;

    auto eliminator = BinopEliminators->HasEliminator(lhstype, rhstype);

    if (eliminator)
    {
      return TypeJudgement(eliminator->result_type());
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
      return TypeJudgement(location, errdsc);
    }
  }
  else
  {
    // error, no binop by that symbol
    string errdsc = "No binop found for symbol ["
                  + op
                  + "]\n";
    return TypeJudgement(location, errdsc);
  }
}

EvalJudgement Binop::evaluate_internal(Environment env)
{

}
