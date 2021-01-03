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
using std::make_shared;

#include "Ast.hpp"
#include "Environment.hpp"
#include "Reference.hpp"

unique_ptr<Object> Reference::clone()
{
  return unique_ptr<Object>(new Reference(ref));
}

string Reference::to_string()
{
  return ref->to_string();
}

TypeJudgement Reference::getype(Environment& env)
{
  /*
    the type of a reference is 'ref T'
    where T is the type of the Entity
    being reffered to.
  */
  TypeJudgement refjdgmt = ref->getype(env);
  if (refjdgmt)
  {
    shared_ptr<Type> reftype = shared_ptr<Type>(new RefType(refjdgmt.u.jdgmt, Location()));
    return TypeJudgement(reftype);
  }
  else
  {
    return refjdgmt;
  }
}

void Reference::substitute(string& var, shared_ptr<Ast>* term, shared_ptr<Ast>& value, Environment& env)
{
  return;
}

bool Reference::appears_free(string& var)
{
  return false;
}

void Reference::rename_binding(string& old_name, string& new_name)
{
  return;
}
