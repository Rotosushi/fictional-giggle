
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;
using std::unique_ptr;

#include "Ast.hpp"
#include "Environment.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Entity.hpp"
#include "Object.hpp"
#include "Lambda.hpp"
#include "PolyLambda.hpp"


EvalJudgement PolyLambda::HasInstance(shared_ptr<Type> target_type, Environment env)
{
  auto build_target_lambda = [this](shared_ptr<Type> target_type)
  {
    return shared_ptr<Ast>(new Entity(unique_ptr<Lambda>(new Lambda(def.arg_id, target_type, def.scope, def.body)), Location()));
  };
  /*
  the only way to introduce a polymorphic
  type is to omit a procedure argument's type
  annotation.
  */

  if (this->def.arg_type->is_polymorphic())
  {
    /*
      look for a possible overload to evaluate,
      if one exists return it for evaluation.
      if none exists try and create one,
      if the created version types, return
      a copy of it for evaluation,
      otherwise report that no
      instance can be constructed for the
      target type
    */
    for (shared_ptr<Type> instance_type : instances)
    {
      TypeJudgement eqjdgmt = TypesEquivalent(instance_type, target_type);
      if (eqjdgmt)
      {
        return EvalJudgement(build_target_lambda(instance_type));
      }
    }

    shared_ptr<Ast> newInst = build_target_lambda(target_type);

    TypeJudgement newInstJdgmt = newInst->getype(env);

    if (newInstJdgmt)
    {
      instances.push_back(target_type->clone());
      return EvalJudgement(newInst);
    }
    else
    {
      return EvalJudgement(EvalError(newInstJdgmt.u.error));
    }
  }
  else
  {
    /*
    this polylambda is not polymorphic, meaning it is
    simply overloaded, which doesn't happen according
    to the grammar, but it is the other side of the
    if conditional, so it's included early.
      look for a possible match in the definition,
      and if that fails and no alternatives exist,
      then report no instance for the target type
      exist. if there is a match, return a copy
      of the lambda to be evaluated.

    it's also implemented early, because it is easy to
    imagine it's implementation given the above implementation.
    */
    TypeJudgement defeqjdgmt = TypesEquivalent(def.arg_type, target_type);
    if (defeqjdgmt)
    {
      return EvalJudgement(build_target_lambda(target_type));
    }

    for (shared_ptr<Type> instance_type : instances)
    {
      TypeJudgement eqjdgmt = TypesEquivalent(instance_type, target_type);
      if (eqjdgmt)
      {
        return EvalJudgement(build_target_lambda(target_type));
      }
    }

    string errdsc = "No instance of Overloaded Lambda found for target type ["
                  + target_type->to_string()
                  + "]";
    return EvalJudgement(EvalError(Location(), errdsc));
  }
}

unique_ptr<Object> PolyLambda::clone()
{
  return unique_ptr<Object>(new PolyLambda(*this));
}

string PolyLambda::to_string()
{
  return def.to_string();
}

TypeJudgement PolyLambda::getype(Environment env)
{
  return def.getype(env);
}

// recall that when we call substitute on a polylambda object,
// that always corresponds to an evaluation context, and just
// like with Lambdas we avoid substituting for any bound ids
// within the body. (these are what the procedure is polymorphic
// over (and will be handed within an Application term),
// whereas this substitution corresponds to bindings between
// this polylambda and it's outer context.)
void PolyLambda::substitute(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env)
{
  auto name_exists_in_args = [](string& name, vector<pair<string, shared_ptr<Type>>>& args)
  {
    for (auto&& arg: args)
      if (get<string>(arg) == name)
        return true;
    return false;
  };

  vector<pair<string, shared_ptr<Ast>>> new_subs;
  for (auto&& sub : subs)
    if (!name_exists_in_args(get<string>(sub), def.args))
      new_subs.push_back(sub);

  // just to save ourselves from doing a bunch
  // or pointless work.
  if (new_subs.size() > 0)
    def.body->substitute(new_subs, &(def.body), env);
}

// this is from a context of trying to avoid improper
// bindings between terms, so in this case we are actually
// specifically trying to modify the bound variables.
void PolyLambda::rename_binding(string old_name, string new_name)
{
  if (def.arg_id == old_name)
  {
    def.arg_id = new_name;
  }
  def.body->rename_binding(old_name, new_name);
}

bool PolyLambda::appears_free(string name)
{
  if (def.arg_id == name)
  {
    // the name appears bound in this term,
    // not free.
    return false;
  }
  else
  {
    return def.body->appears_free(name);
  }
}
