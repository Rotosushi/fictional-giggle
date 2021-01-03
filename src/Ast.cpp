
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <utility>
using std::pair;
using std::get;
#include <memory>
using std::shared_ptr;

#include "Location.hpp"
#include "Type.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Environment.hpp"
#include "Entity.hpp"
#include "Empty.hpp"
#include "Reference.hpp"
#include "Ast.hpp"

/*
  observations from the c source:
  so, the term we are observing to potentially replace
  is only accessable via a pointer. (precisely because the
  data structure is a tree) since this pointer
  is itself always stored within the parent node to the
  node being observed (and dynamically dispatched over),
  if we want to be sure we are replacing
  the right node, AND define this procedure from the
  perspective of each node defining substitution in a
  mutually recursive fashion, we must have a pointer to
  the pointer to the term (the very term we are observing,
  and dynamically dispatching over), so that we can potentially assign
  a new term to the parent pointer. (this is the exact pointer
  we will have a pointer too, and each mutual recursion will
  set up the correct parent pointer to be potentially replaced.)
  this assignment is the precise action which enacts the actual
  replacement (since we are doing tree replacement
  along with inserting/extracting clones from the
  environment, closures come from the fact that
  when we return the procedure body which was just specialized
  against a single argument, the part of the tree which
  represents the lambda being returned has been modified
  such that the tree is pointing to a valid instance of
  the term which was substituted in. this in effect means
  that we do not need to support closures directly
  in the code that I write.) (because when the execution
  reaches a point where it needs the data that was substituted
  into the tree, it's always there, precisely because we have
  substituted in a copy.) now, when it comes to what it means
  to partially apply a procedure, and thus create a closure
  in the assembly translation of the code, this
  is a different question. (an early attempt at an
  idea which may be a solution is to create a closure
  structure which is a function pointer, and, i think,
  a tuple which contains, by-value, the terms representing
  what was passed in as arguments. (my other secret goal
  is to implement the monomorphic procedure call exactly
  like one would call a c procedure. which is why i say
  by-value here.))

  also, i am observing that this procedure (the substitution method)
  is also defined
  within a switch statement over Ast objects. meaning we need
  to define substitution as a virtual method of Ast's themselves.
  this makes it weird to consider the idea of substitution
  in the first place, as we are always potetially causing the
  deletion of whatever object we just used the vtable of.

  additionally, in only one case do we ever perform the replacement
  itself, and that is if we are observing a variable which
  has the same id as the argument we are replacing for.
  thus making every other substitution body, (and for that
  matter, the bodies of the procedures resposible for renaming
  variables when the need arises), essentially bookeeping only.
  and all of them except variable is just to support the mutual resursion
  through the tree.
*/

shared_ptr<Ast> Ast::clone()
{
  return this->clone_internal();
}

string Ast::to_string()
{
  return this->to_string_internal();
}

TypeJudgement Ast::getype(Environment& env)
{
  if (cached_type == nullptr)
  {
    TypeJudgement thsjdgmt = this->getype_internal(env);
    if (thsjdgmt)
    {
      cached_type = thsjdgmt.u.jdgmt;

      return TypeJudgement(cached_type);
    }
    else
    {
      return thsjdgmt;
    }
  }
  else
  {
    return TypeJudgement(cached_type);
  }
}

EvalJudgement Ast::evaluate(Environment& env)
{
  auto is_entity = [](shared_ptr<Ast> term)
  {
    Entity* entity = dynamic_cast<Entity*>(term.get());
    return entity != nullptr;
  };

  EvalJudgement evljdgmt = this->evaluate_internal(env);

  while (evljdgmt && !is_entity(evljdgmt.u.jdgmt))
  {
    evljdgmt = evljdgmt.u.jdgmt->evaluate_internal(env);
  }

  return evljdgmt;
}

void Ast::substitute(string& var, shared_ptr<Ast>* term, shared_ptr<Ast>& value, Environment& env)
{
  return this->substitute_internal(var, term, value, env);
}

bool Ast::appears_free(string& var)
{
  return this->appears_free_internal(var);
}

void Ast::rename_binding(string& old_name, string& new_name)
{
  return this->rename_binding_internal(old_name, new_name);
}


void Ast::insert_if_unique(string& name, vector<string>& names)
{
  bool appears = false;
  for (auto&& id : names)
    if (id == name)
    {
      appears = true;
      break;
    }

  if (!appears)
    names.push_back(name);
}
