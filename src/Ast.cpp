
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Location.hpp"
#include "Type.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Environment.hpp"
#include "Ast.hpp"

shared_ptr<Ast> Ast::clone()
{
  return clone_internal();
}

string Ast::to_string()
{
  return to_string_internal();
}

TypeJudgement Ast::getype(Environment env)
{
  return getype_internal(env);
}

EvalJudgement Ast::evaluate(Environment env)
{
  auto is_entity = [](shared_ptr<Ast> term)
  {
    Entity* ent;
    return (ent = dynamic_cast<Entity*>(term.get())) == nullptr;
  }

  /*
    in the previous version, this is
    a loop over a peice of dispatch.

    given that we only have the ability
    to dynamically dispatch based on the
    runtime type of the 'this' ptr in c++,
    if we are given a partially evaluated
    result, we need to continue evaluation
    by dispatching over the partial result.
    this means that we replace the loop by
    a recursive call.

    what is a partial result? anything not
    in beta-normal form. (or: anything that
    isn't an entity in the language.
    (
    so, calling it that now leads to some
    confusion, because the things that
    make up the language itself I have called
    entities in the past. whereas now i
    specifically mean the class 'Entity'.
    ))
  */

  EvalJudgement result = evaluate_internal(env);

  if (result.succeeded() && !is_entity(result.u.jdgmt))
  {
    result = result.u.jdgmt->evaluate(env);
  }

  return result;
}

/* this is a very simplistic version of
  renaming. it (mostly) ensures the semantics,
  and should be kept separate
  from anything but evaluating a given Ast.
  we don't want to rename the bindings the user
  types in, because that would confuse the
  writer of the program.
*/
string Ast::generate_name(int len)
{
  const char symset[]  = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
  const int  symsetlen = 52;
  string result;
  for (int i = 0; i < len; ++i)
  {
    result += symset[rand() % symsetlen];
  }
  return result;
}
