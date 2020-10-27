
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Ast.hpp"
#include "Environment.hpp"
#include "TypeJudgement.hpp"
#include "Iteration.hpp"

shared_ptr<Ast> Iteration::clone_internal()
{
  return shared_ptr<Ast>(new Iteration(cond->clone(), body->clone(), location));
}

string Iteration::to_string_internal()
{
  string result;
  result  = "while ";
  result += cond->to_string();
  result += " do ";
  result += body->to_string();
  return result;
}

TypeJudgement Iteration::getype_internal(Environment env)
{
  /*
  ENV |- 'while' t1 : T1 'do' t2 : T2,
  if T1 has-type Bool, and t2 : T2
  -------------------------------------------------
  ENV |- ('while' t1 : T1 'do' t2 : T2 ) : T2

  should we assign a meaning to?
  x := while (y < z) y <- f(y)
  as in, the result of the while term
  is the final body value. i don't
  see an initial argument against this?
  it allows for one to validly use a
  loop in certain contexts in which it would
  be ambiguous previously.
  */
  TypeJudgement condjdgmt = cond->getype(env);

  if (!condjdgmt)
    return condjdgmt;

  shared_ptr<Type> condtype = condjdgmt.u.jdgmt;
  shared_ptr<Type> booltype = shared_ptr<Type>(new MonoType(AtomicType::Bool, Location()));

  if (TypesEquivalent(condtype, booltype))
  {
    TypeJudgement bodyjdgmt = body->getype(env);
    return bodyjdgmt;
  }
  else
  {
    string errdsc = "conditional expression has type ["
                  + condtype->to_string()
                  + "] instead of the expected type [Bool]\n";
    return TypeJudgement(TypeError(location, errdsc));
  }
}

EvalJudgement Iteration::evaluate_internal(Environment env)
{

}
