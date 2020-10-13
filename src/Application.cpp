
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;
using std::make_shared;

#include "SymbolTable.hpp"
#include "TypeJudgement.hpp"
#include "Ast.hpp"
#include "Application.hpp"

shared_ptr<Ast> Application::clone_interal()
{
  return make_shared(Application(lhs->clone(), rhs->clone(), location));
}

string Application::to_string_internal()
{
  string result;
  result += "(";
  result += lhs->to_string();
  result += " ";
  result += rhs->to_string();
  result += ")";
  return result;
}

TypeJudgement Application::getype_internal(SymbolTable* env, OperatorTable* ops)
{
  /*
  ENV |- lhs : type1 -> type2, rhs : type1
  --------------------------------------------
          ENV |- lhs rhs : type2

  the lhs must be a procedure, and the procedures
  lhs type must be equal to the type of the rhs
  of the application.
  */
  TypeJudgement typeA = lhs->getype(env, ops);

  if (typeA)
  {
    ProcType* pt = dynamic_cast<ProcType*>(typeA.u.judgement.get());
    if (pt)
    {
      if (typeA.u.judgement.get()->is_polymorphic())
      {
        return TypeJudgement(make_shared(MonoType(AtomicType::Poly)));
      }
      else
      {
        TypeJudgement typeB = rhs->getype(env, ops);

        if (typeB)
        {
          Type* type1 = pt->lhs.get();

          if (type1 == nullptr)
            throw "bad lhs type\n";

          Type* type2 = pt->rhs.get();

          if (type2 == nullptr)
            throw "bad rhs type\n";

          Type* type3 = typeB.u.jdgmt.get();

          if (type3 == nullptr)
            throw "bad argument type\n";

          if (TypesEquivalent(type1, type3))
          {
            /*
              the type of the result of the application
              is the rhs of the procedure type.
            */
            return TypeJudgement(type2);
          }
          else
          {
            string errdsc = "argument type ["
                          + type3->to_string()
                          + "] not equivalent to formal type ["
                          + type1->to_string()
                          + "]\n";
            return TypeJudgement(location, errdsc);
          }
        }
        else
        {
          return typeB;
        }
      }
    }
    else
    {
      Type*     tA = typeA.u.judgement.get();
      MonoType* mt = dynamic_cast<MonoType*>(tA);

      if (mt == nullptr)
      {
        throw "bad lhs application ptr\n";
      }
      else if (tA->is_polymorphic())
      {
        return TypeJudgement(make_shared(MonoType(AtomicType::Poly)));
      }
      else
      {
        string errdsc = "cannot apply non-procedure type ["
                      + tA->to_string()
                      + "]\n";
        return TypeJudgement(location, errdsc);
      }
    }
  }
  else
  {
    return typeA;
  }
}


EvalJudgement Application::evaluate_internal(SybolTable* env, OperatorTable* ops)
{

}
































/* ------------------------------------------------------------------ */
