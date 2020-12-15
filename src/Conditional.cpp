
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <utility>
using std::pair;
using std::get;
#include <memory>
using std::shared_ptr;
using std::make_shared;

#include "Ast.hpp"
#include "Environment.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Entity.hpp"
#include "Conditional.hpp"

shared_ptr<Ast> Conditional::clone_internal()
{
  return shared_ptr<Ast>(new Conditional(cond->clone(), fst->clone(), snd->clone(), location));
}

string Conditional::to_string_internal()
{
  string result;
  result  = "if ";
  result += cond->to_string();
  result += " then ";
  result += fst->to_string();
  result += " else ";
  result += snd->to_string();
  return result;
}

TypeJudgement Conditional::getype_internal(Environment env)
{
  /*
    ENV |- 'if' t1 : T1 'then' t2 : T2 'else' t3 : T3,
    if T1 has-type Bool, and T2 is-equal-to T3,
    -------------------------------------------------
    ENV |- ('if' t1 : T1 'then' t2 : T2 'else' t3 : T3) : T2

    if the term representing the condition has type Bool,
    and the terms representing the alternative expressions
    have the same type as one another, then we can conclude
    that the type of the entire expression is the type of
    the alternative expressions (which are the same type,
    so we arbitrarily select the first)
  */
  TypeJudgement condjdgmt = cond->getype(env);

  if (condjdgmt)
  {
    shared_ptr<Type> booltype = shared_ptr<Type>(new MonoType(AtomicType::Bool, Location()));
    shared_ptr<Type> polytype = shared_ptr<Type>(new MonoType(AtomicType::Poly, Location()));
    shared_ptr<Type> condtype = condjdgmt.u.jdgmt;

    /*
      in the case of a polymorphic test expression,
      we want to allow the programmer to insert any
      shape in, and then once they bind all instances
      during evaluation of the term, we can simply
      type the monomorphic term with the standard judgements.
    */
    if (TypesEquivalent(condtype, booltype) || TypesEquivalent(condtype, polytype))
    {
      TypeJudgement fstjdgmt = fst->getype(env);

      if (!fstjdgmt)
        return fstjdgmt;

      TypeJudgement sndjdgmt = snd->getype(env);

      if (!sndjdgmt)
        return sndjdgmt;

      shared_ptr<Type> fsttype = fstjdgmt.u.jdgmt;
      shared_ptr<Type> sndtype = sndjdgmt.u.jdgmt;

      if (TypesEquivalent(fsttype, sndtype))
      {
        /*
          since these types are equivalent
          returning either is a valid option.
        */
        return fstjdgmt;
      }
      else if (TypesEquivalent(fsttype, polytype) || TypesEquivalent(sndtype, polytype))
      {
        return TypeJudgement(polytype);
      }
      else
      {
        string errdsc = "conditional alternatives have different types; fst:["
                      + fsttype->to_string()
                      + "] snd:["
                      + sndtype->to_string()
                      + "]\n";
        return TypeJudgement(TypeError(location, errdsc));
      }
    }
    else
    {
      string errdsc = "Test expression has type ["
                    + condtype->to_string()
                    + "] not the expected type [Bool]\n";
      return TypeJudgement(TypeError(location, errdsc));
    }
  }
  else
  {
    return condjdgmt;
  }
}

EvalJudgement Conditional::evaluate_internal(Environment env)
{
  auto is_entity = [](shared_ptr<Ast> term)
  {
    Entity* entity = dynamic_cast<Entity*>(term.get());
    return entity != nullptr;
  };

  auto to_boolean_value = [](shared_ptr<Ast> term)
  {
    // so like, in the abstract these two unguarded
    // casts are suspicious. however, the type
    // system saves us here.
    // cast the Ast* to an Entity*
    Entity* ent = dynamic_cast<Entity*>(term.get());
    // cast the Object* to a Boolean*
    Boolean* bol = dynamic_cast<Boolean*>(ent->literal.get());

    return bol->value;
  };

  EvalJudgement condjdgmt = cond->evaluate(env);

  if (condjdgmt && to_boolean_value(condjdgmt.u.jdgmt))
  {
    EvalJudgement fstjdgmt = fst->evaluate(env);

    return fstjdgmt;
  }
  else if (condjdgmt)
  {
    EvalJudgement sndjdgmt = snd->evaluate(env);

    return sndjdgmt;
  }
  else
  {
    return condjdgmt;
  }
}

void Conditional::substitute_internal(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env)
{
  cond->substitute(subs, &cond, env);
  fst->substitute(subs, &fst, env);
  snd->substitute(subs, &snd, env);
}

bool Conditional::appears_free_internal(vector<string>& names, vector<string>& appeared_free)
{
  /*
  return cond->appears_free(var)
      || fst->appears_free(var)
      || snd->appears_free(var);
  i think this is a case where we
  want to traverse each branch, specifically
  because this algorithm is building up the
  appeared_free list via side effect, and we
  absolutely do not want to miss a binding.
  */
  bool b1 = cond->appears_free(names, appeared_free);
  bool b2 = fst->appears_free(names, appeared_free);
  bool b3 = snd->appears_free(names, appeared_free);

  return b1 || b2 || b3;
}

void Conditional::rename_binding_in_body_internal(vector<pair<string, string>>& renaming_pairs)
{
  cond->rename_binding_in_body(renaming_pairs);
  fst->rename_binding_in_body(renaming_pairs);
  snd->rename_binding_in_body(renaming_pairs);
}
