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

TypeJudgement Reference::getype(Environment env)
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

void Reference::substitute(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env)
{
  return;
}

bool Reference::appears_free(vector<string>& names, vector<string>& appeared_free)
{
  return false;
}

void Reference::rename_binding_in_body(vector<pair<string, string>>& renaming_pairs)
{
  return;
}
