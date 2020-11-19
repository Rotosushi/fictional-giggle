#include <string>
using std::string;
#include <memory>
using std::shared_ptr;
using std::make_shared;

#include "Ast.hpp"
#include "Environment.hpp"
#include "Reference.hpp"

shared_ptr<Ast> Reference::clone_internal()
{
  return shared_ptr<Ast>(new Reference(ref->clone(), Location()));
}

string Reference::to_string_internal()
{
  return ref->to_string();
}

TypeJudgement Reference::getype_internal(Environment env)
{
  /*
    the type of a reference is 'ref T'
    where T is the type of the Entity
    being reffered to.
  */
  TypeJudgement refjdgmt = ref->getype(env);
  if (refjdgmt)
  {
    shared_ptr<Type> reftype = shared_ptr<Type>(new RefType(refjdgmt.u.jdgmt, location));
    return TypeJudgement(reftype);
  }
  else
  {
    return refjdgmt;
  }
}

EvalJudgement Reference::evaluate_internal(Environment env)
{
  return EvalJudgement(shared_ptr<Ast>(new Reference(*this)));
}

void Reference::substitute_internal(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env)
{
  ref->substitute(var, &ref, value, env);
}

bool Reference::appears_free_internal(string var)
{
  return ref->appears_free(var);
}

void Reference::rename_binding_internal(string old_name, string new_name)
{
  ref->rename_binding(old_name, new_name);
}
