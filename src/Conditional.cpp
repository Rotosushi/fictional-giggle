
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Ast.hpp"
#include "SymbolTable.hpp"
#include "TypeJudgement.hpp"
#include "Conditional.hpp"

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

TypeJudgement Conditional::getype_internal(SymbolTable* env, BinopSet* binops)
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
  TypeJudgement condtype = cond->getype(env, binops);

  if (condtype)
  {
    shared_ptr<Type> booltype = make_shared(MonoType(AtomicType::Bool));
    Type* ct = condtype.u.judgement.get();

    if (TypesEquivalent(ct, booltype.get()))
    {
      TypeJudgement fstjdgmt = fst->getype(env, binops);

      if (!fstjdgmt)
        return fstjdgmt;

      TypeJudgement sndjdgmt = snd->getype(env, binops);

      if (!sndjdgmt)
        return sndjdgmt;

      Type* fsttype = fstjdgmt.u.judgment.get();
      Type* sndtype = sndjdgmt.u.judgment.get();

      if (TypesEquivalent(fsttype, sndtype))
      {
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
                    + "] not Bool\n";
      return TypeJudgement(location, errdsc);
    }
  }
  else
  {
    return condtype;
  }
}

EvalJudgement Conditional::evaluate_internal(SymbolTable* env, BinopSet* binops)
{

}
