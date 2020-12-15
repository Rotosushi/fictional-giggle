
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <utility>
using std::pair;
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
#include "Entity.hpp"
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

    // we need to check for the 'early-exit-strategy'
    // i.e. polymorphism, after we have typed both
    // sides (with at least polymorphic type)
    // otherwise the user could sneak badly formed
    // code past the typechecker in the rhs of the
    // equation.
    if (lhsjdgmt.u.jdgmt->is_polymorphic())
      return TypeJudgement(shared_ptr<Type>(new MonoType(AtomicType::Poly, Location())));


    if (rhsjdgmt.u.jdgmt->is_polymorphic())
      return TypeJudgement(shared_ptr<Type>(new MonoType(AtomicType::Poly, Location())));

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
  auto is_entity = [](shared_ptr<Ast> term)
  {
    Entity* entity = dynamic_cast<Entity*>(term.get());
    return entity != nullptr;
  };

  auto BinopEliminators = env.binops->FindBinop(op);

  if (BinopEliminators)
  {
    auto lhsEvalJdgmt = lhs->evaluate(env);

    if (!lhsEvalJdgmt)
      return lhsEvalJdgmt;

    auto rhsEvalJdgmt = rhs->evaluate(env);

    if (!rhsEvalJdgmt)
      return rhsEvalJdgmt;

    auto lhsTypeJdgmt = lhsEvalJdgmt.u.jdgmt->getype(env);

    if (!lhsTypeJdgmt)
      return EvalJudgement(EvalError(lhsTypeJdgmt.u.error));

    auto rhsTypeJdgmt = rhsEvalJdgmt.u.jdgmt->getype(env);

    if (!rhsTypeJdgmt)
      return EvalJudgement(EvalError(rhsTypeJdgmt.u.error));


    auto eliminator = (*BinopEliminators)->HasEliminator(lhsTypeJdgmt.u.jdgmt, rhsTypeJdgmt.u.jdgmt);


    if (eliminator)
    {
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
                    + lhsTypeJdgmt.u.jdgmt->to_string()
                    + "] and actual rhstype:["
                    + rhsTypeJdgmt.u.jdgmt->to_string()
                    + "]\n";
      return EvalJudgement(EvalError(location, errdsc));
    }
    /*
    just for fun, we are going to assume that evaluation gets to
    happen before getting the types, (because it had to typecheck to get here right?)
    so that we can hopefully evaluate
    possible polymorphic leaves into monomorphic ones. (that's the idea at least)
    auto lhsTypeJdgmt = lhs->getype(env);

    if (!lhsTypeJdgmt)
      return EvalJudgement(EvalError(lhsTypeJdgmt.u.error.location(), lhsTypeJdgmt.u.error.what()));

    auto rhsTypeJdgmt = rhs->getype(env);

    if (!rhsTypeJdgmt)
      return EvalJudgement(EvalError(rhsTypeJdgmt.u.error.location(), rhsTypeJdgmt.u.error.what()));

    shared_ptr<Type> lhstype = lhsTypeJdgmt.u.jdgmt;
    shared_ptr<Type> rhstype = rhsTypeJdgmt.u.jdgmt;

    auto eliminator = (*BinopEliminators)->HasEliminator(lhstype, rhstype);

    if (eliminator)
    {
      EvalJudgement lhsEvalJdgmt = lhs->evaluate(env);

      if (!lhsEvalJdgmt)
        return lhsEvalJdgmt;

      EvalJudgement rhsEvalJdgmt = rhs->evaluate(env);

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
    */
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

void Binop::substitute_internal(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env)
{
  lhs->substitute(subs, &lhs, env);
  rhs->substitute(subs, &rhs, env);
}

bool Binop::appears_free_internal(vector<string>& names, vector<string>& appeared_free)
{
  bool bl = lhs->appears_free(names, appeared_free);
  bool br = rhs->appears_free(names, appeared_free);
  return bl || br;
}

void Binop::rename_binding_in_body_internal(vector<pair<string, string>>& renaming_pairs)
{
  lhs->rename_binding_in_body(renaming_pairs);
  rhs->rename_binding_in_body(renaming_pairs);
}
