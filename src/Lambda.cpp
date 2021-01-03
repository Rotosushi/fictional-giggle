
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
#include <algorithm>
using std::remove_if;

#include "Ast.hpp"
#include "Environment.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Object.hpp"
#include "Entity.hpp"
#include "Gensym.hpp"
#include "Lambda.hpp"


unique_ptr<Object> Lambda::clone()
{
  return unique_ptr<Object>(new Lambda(*this));
}

string Lambda::to_string()
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

TypeJudgement Lambda::getype(Environment& env)
{
  /*
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

void Lambda::substitute(string& var, shared_ptr<Ast>* term, shared_ptr<Ast>& value, Environment& env)
{
  /*
    construct a new ids and values list to pass further into
    this recursion, such that any name which appears bound in
    this lambda is removed from the new ids and values sets.
    this is because we do not want to replace terms which appear
    bound in this lower scope. (we also do not want to destroy
    information in the substitution chain as we have no idea
    where the algorithm is going from here, from here.)
    however, we know that whatever happens, in the term lower than
    this point, the number of names we are substituting in is
    smaller or equal to the length. (so we always go down or
    stay the same, which points to the recursion stopping eventually.
    as all positive integer values reduce to zero. wheeee.)

  */

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

void Lambda::rename_binding(string& old_name, string& new_name)
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

bool Lambda::appears_free(string& var)
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
