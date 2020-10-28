
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

  auto is_bool_true = [](shared_ptr<Ast> term)
  {
    // so like, in the abstract these two unguarded
    // casts are suspicious. however, the type
    // system saves us here.
    // cast the Ast* to an Entity*
    Entity* ent = dynamic_cast<Entity*>(term.get());
    // cast the Object* to a Boolean*
    Boolean* bol = dynamic_cast<Boolean*>(ent->literal.get());

    return bol->value;
  }

  EvalJudgement condjdgmt = cond->evaluate(env);

  if (condjdgmt && is_bool_true(condjdgmt.u.jdgmt))
  {
    do {
      EvalJudgement bodyjdgmt = body->evaluate(env);
    } while ((condjdgmt = cond->evaluate(env)) && is_bool_true(condjdgmt.u.jdgmt));
  }

  return EvalJudgement(shared_ptr<Ast>(new Entity((void*)nullptr, location)));
}

void Iteration::substitute(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env)
{
  cond->substitute(var, &cond, value, env);
  body->substitute(var, &body, value, env);
}

bool Iteration::appears_free(string var)
{
  return cond->appears_free(var) || body->appears_free(var);
}

void Iteration::rename_binding(string old_name, string new_name)
{
  cond->rename_binding(old_name, new_name);
  body->rename_binding(old_name, new_name);
}
