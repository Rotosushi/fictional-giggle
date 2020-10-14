
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Ast.hpp"
#include "SymbolTable.hpp"
#include "OperatorTable.hpp"
#include "TypeJudgement.hpp"
#include "Iteration.hpp"

shared_ptr<Ast> Iteration::clone_internal()
{
  return make_shared(Iteration(cond->clone(), body->clone(), location));
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

TypeJudgement Iteration::getype_internal(SymbolTable* env, OperatorTable* ops)
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
  */
  TypeJudgement condjdgmt = cond->getype(env, ops);

  if (!condjdgmt)
    return condjdgmt;

  Type* condtype = condjdgmt.u.judgement.get();
  shared_ptr<Type> booltype = make_shared<MonoType>(AtomicType::Bool);

  if (TypesEquivalent(condtype, booltype))
  {
    TypeJudgement bodyjdgmt = body->getype(env, ops);
    return bodyjdgmt;
  }
  else
  {
    string errdsc = "conditional expression doesn't have type Bool"
                  + " instead has type ["
                  + condtype->to_string()
                  + "]\n";
    return TypeJudgement(location, errdsc);
  }
}

EvalJudgement Iteration::evaluate_internal(SymbolTable* env, BinopSet* binops)
{

}
