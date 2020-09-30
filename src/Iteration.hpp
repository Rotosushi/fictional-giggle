#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Entity.hpp"
#include "SymbolTable.hpp"
#include "TypeJudgement.hpp"

class Iteration : public Entity
{
public:
  shared_ptr<Entity> cond;
  shared_ptr<Entity> body;

  Iteration(shared_ptr<Entity> c, shared_ptr<Entity> b, const Location& l)
    : Entity(l), cond(c), body(b) {}

  Iteration(const Iteration& other)
    : Entity(other.location), cond(other.cond), body(other.body) {}

protected:
  virtual string to_string_internal() override
  {
    string result;
    result  = "while ";
    result += cond->to_string();
    result += " do ";
    result += body->to_string();
    return result;
  }

  virtual TypeJudgement getype_internal() override
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
    TypeJudgement condjdgmt = cond->getype(env);

    if (!condjdgmt)
      return condjdgmt;

    Type* condtype = condjdgmt.u.judgement.get();
    shared_ptr<Type> booltype = make_shared<MonoType>(AtomicType::Bool);

    if (TypesEquivalent(condtype, booltype))
    {
      TypeJudgement bodyjdgmt = body->getype(env);
      return bodyjdgmt;
    }
    else
    {
      string errdsc = "conditional expression doesn't have type Bool"
                    + "instead has type ["
                    + condtype->to_string()
                    + "]\n";
      return TypeJudgement(location, errdsc);
    }
  }

};
