
#include <string>
using std::string;
#include <vector>
using std::vector>
#include <utility>
using std::pair;
using std::get;
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


EvalJudgement PolyLambda::HasInstance(vector<shared_ptr<Type>> target_types, Environment env)
{
  if (target_types.size() != def.args.size())
  {
    string errdsc = "mismatch of the number of actual arguments ["
                  + std::to_string(target_types.size())
                  + "] to formal arguments ["
                  + std::to_string(gef.args.size())
                  + "]";
    EvalError err(Location(), errdsc);
  }
  /*

  */
  auto build_target_lambda = [this](shared_ptr<Type> target_types)
  {
    vector<pair<string, shared_ptr<Type>>> arglist;
    int len = target_types.size();
    for (int i = 0; i < len; ++i)
    {
      pair<string, shared_ptr<Type>>& arg = def.args[i];
      arglist.push_back(make_pair(get<string>(arg), target_types[i]));
    }
    return shared_ptr<Ast>(new Entity(unique_ptr<Object>(new Lambda(argslist, def.scope, def.body)), Location()));
  };

  auto argument_types_match = [this](vector<shared_ptr<Type>>& formal_types, vector<shared_ptr<Type>>& actual_types)
  {
    bool match = false;

    int len = formal_types.size();
    for (int i = 0; i < len; i++)
    {
      TypeJudgement eqjdgmt = TypesEquivalent(formal_types[i], actual_types[i]);

      if (eqjdgmt && i == len)
        match = true;
      else if (!eqjdgmt)
      {
        match = false;
        break;
      }
      // there is a little hole here for the
      // "i need to check the rest of the args"
      // case within this loop.
      // 'hole' is when (eqjdgmt == true && i != len)
    }

    return match;
  }
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

  for (vector<shared_ptr<Type>>& instance_types : instances)
  {
    if (argument_types_match(instance_types, target_types));
    {
      return EvalJudgement(build_target_lambda(instance_types));
    }
  }

  shared_ptr<Ast> newInst = build_target_lambda(target_type);

  TypeJudgement newInstJdgmt = newInst->getype(env);

  if (newInstJdgmt)
  {
    instances.push_back(target_types);
    return EvalJudgement(newInst);
  }
  else
  {
    return EvalJudgement(EvalError(newInstJdgmt.u.error));
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
void PolyLambda::rename_binding_in_body_internal(vector<pair<string, string>>& renaming_pairs)
{
  if (def.arg_id == old_name)
  {
    def.arg_id = new_name;
  }
  def.body->rename_binding_in_body(old_name, new_name);
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
