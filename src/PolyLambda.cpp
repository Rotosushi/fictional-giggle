
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <utility>
using std::pair;
using std::get;
#include <memory>
using std::shared_ptr;
using std::unique_ptr;

#include "Gensym.hpp"
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
  /*

  */
  auto build_target_lambda = [this, &target_type]()
  {
    return shared_ptr<Ast>(new Entity(unique_ptr<Object>(new Lambda(arg_id, target_type, scope, body)), Location()));
  };


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

  for (shared_ptr<Type>& instance_type : instances)
  {
    if (TypesEquivalent(instance_type, target_type))
    {
      return EvalJudgement(build_target_lambda());
    }
  }

  shared_ptr<Ast> newInst = build_target_lambda();

  TypeJudgement newInstJdgmt = newInst->getype(env);

  if (newInstJdgmt)
  {
    instances.push_back(target_type);
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
  string result;
  result  = "\\ ";
  result += arg_id;
  result += ": ";
  result += arg_type->to_string();
  result += " => ";
  result += body->to_string();
  return result;
}

TypeJudgement PolyLambda::getype(Environment& env)
{
  /*
    (the entire argument list is the first portion
      of the type, and the return type is the last part.)

        ENV |- id : type1, term : type2
        --------------------------------
    ENV |- \ id : type1 => term : type1 -> type2
  */
  this->scope->bind(arg_id, shared_ptr<Ast>(new Entity(arg_type, Location())));

  Environment new_env(env.parser, this->scope, env.precedences, env.binops, env.unops, this->cleanup_list);
  TypeJudgement type2 = body->getype(new_env);

  this->scope->unbind(arg_id);


  if (type2)
  {
    return TypeJudgement(shared_ptr<Type>(new ProcType(arg_type, type2.u.jdgmt, Location())));
  }
  else
  {
    return type2;
  }
}

// recall that when we call substitute on a polylambda object,
// that always corresponds to an evaluation context, and just
// like with Lambdas we avoid substituting for any bound ids
// within the body. (these are what the procedure is polymorphic
// over (and will be handed within an Application term),
// whereas this substitution corresponds to bindings between
// this polylambda and it's outer context.)
void PolyLambda::substitute(string& var, shared_ptr<Ast>* term, shared_ptr<Ast>& value, Environment& env)
{
  if (arg_id == var)
    return;
  else if ((*term)->appears_free(arg_id))
  {
    string new_name = gensym();
    string old_name = arg_id;

    arg_id = new_name;
    body->rename_binding(old_name, new_name);
  }

  body->substitute(arg_id, &body, value, env);
}

// this is from a context of trying to avoid improper
// bindings between terms, so in this case we are actually
// specifically trying to modify the bound variables.
void PolyLambda::rename_binding(string& old_name, string& new_name)
{
  /*
    if within the body of the lambda whose bindings we are renaming
    is itself a lambda which has a biding with the same name as what we are
    looking to replace, we do not rename, because that lambda
    is introducing that binding for it's own body, and that makes
    it a separate binding than the one we are looking for.
   */
   if (arg_id == old_name)
    return;
  else
    body->rename_binding(old_name, new_name);

}

bool PolyLambda::appears_free(string& var)
{
  /*
  funnily enough
  appears_free is essentially rename_binding_in_body

  every name which appears bound in this procedure by definition
  cannot appear_free any lower than this point (in the sense
  of the tree representing this execution).
  */

  if (var == arg_id)
    return false;
  else
    return body->appears_free(var);
}
