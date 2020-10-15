
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;
using std::make_shared;

#include "Ast.hpp"
#include "SymbolTable.hpp"
#include "OperatorTable.hpp"
#include "TypeJudgement.hpp"
#include "Conditional.hpp"

shared_ptr<Ast> Conditional::clone_interal()
{
  return make_shared(Conditional(cond->clone(), fst->clone(), snd->clone(), location));
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
    shared_ptr<Type> booltype = make_shared(MonoType(AtomicType::Bool));
    Type* ct = condjdgmt.u.jdgmt.get();

    if (TypesEquivalent(ct, booltype.get()))
    {
      TypeJudgement fstjdgmt = fst->getype(env, ops);

      if (!fstjdgmt)
        return fstjdgmt;

      TypeJudgement sndjdgmt = snd->getype(env, ops);

      if (!sndjdgmt)
        return sndjdgmt;

      Type* fsttype = fstjdgmt.u.jdgmt.get();
      Type* sndtype = sndjdgmt.u.jdgmt.get();

      if (TypesEquivalent(fsttype, sndtype))
      {
        /*
          since these types are equivalent
          returning either is a valid option.
        */
        return fstjdgmt;
      }
      else
      {
        string errdsc = "conditional alternatives "
                      + "have different types fst:["
                      + fsttype->to_string()
                      + "] snd:["
                      + sndtype->to_string()
                      + "]\n";
        return TypeJudgement(location, errdsc);
      }
    }
    else
    {
      string errdsc = "Test expression"
                    + " has type ["
                    + ct->to_string()
                    + "] not the expected type [Bool]\n";
      return TypeJudgement(location, errdsc);
    }
  }
  else
  {
    return condjdgmt;
  }
}

EvalJudgement Conditional::evaluate_internal(SymbolTable* env, OperatorTable* ops)
{

}
