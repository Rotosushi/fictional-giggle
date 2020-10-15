
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;
using std::make_shared;

#include "SymbolTable.hpp"
#include "TypeJudgement.hpp"
#include "Ast.hpp"
#include "Application.hpp"

shared_ptr<Ast> Application::clone_internal()
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

TypeJudgement Application::getype_internal(Environment env)
{
  /*
  ENV |- lhs : type1 -> type2, rhs : type1
  --------------------------------------------
          ENV |- lhs rhs : type2

  the lhs must be a procedure, and the procedures
  lhs type must be equal to the type of the rhs
  of the application.
  */
  TypeJudgement typeA = lhs->getype(env);

  if (typeA)
  {
    ProcType* pt = dynamic_cast<ProcType*>(typeA.u.jdgmt.get());
    if (pt)
    {
      if (typeA.u.jdgmt.get()->is_polymorphic())
      {
        return TypeJudgement(make_shared(MonoType(AtomicType::Poly)));
      }
      else
      {
        TypeJudgement typeB = rhs->getype(env);

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
      Type*     tA = typeA.u.jdgmt.get();
      MonoType* mt = dynamic_cast<MonoType*>(tA);

      if (mt == nullptr)
      {
        throw "bad lhs application ptr\n";
      }
      else if (tA->is_polymorphic())
      {
        /*
          recall that the only way in which a poly
          type can be created is when the type annotation
          of a lambda argument is left off. this means
          that to be here, we by definition are observing
          the type of a variable appearing as the leftmost
          term in an application term; which is also
          by definition appearing as a subterm of said
          PolyLambda object, given that we
          are currently observing a term which is
          supposed to be substituted with another
          term of polymorphic type at some later time
          than right now during typechecking,
          we have absolutely no way of inferring any
          information on the possible return type
          of this polymorphic term could be,
          except of course, that the term itself
          is going to be polymorphic, which means
          that we type the entire expression
          as polymorphic. this can be thought
          of as infecting the rest of the expression
          outwards, as many of the other judgements around
          polymorphism are the same, when we encounter
          a point of non-determinism, we can only call it
          what it is, only when it comes to trying
          to retrieve a deterministic point are we
          forced to 'dispatch' to the point of
          determinism. (or we fail, for some
          reason or another (which is, of course, reported.))
        */
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


EvalJudgement Application::evaluate_internal(Environment env)
{

}
































/* ------------------------------------------------------------------ */
