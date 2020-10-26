
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;
using std::make_shared;

#include "Ast.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Environment.hpp"
#include "Application.hpp"

shared_ptr<Ast> Application::clone_internal()
{
  return shared_ptr<Ast>(new Application(lhs->clone(), rhs->clone(), location));
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

  given, multiple argument procedures, and
  therefore, multiple dispatch, what does
  this rule look like? well in english,
  we still want to ensure that the given
  polymorphic procedure has a deterministic
  and singular definition whose formal
  argument list matches the type and
  number of actual arguments present within
  the call expression. (do we want to allow partial
  application?)
  we must say that each type matches exactly.
  (which will change if/when we consider subtyping)
  and that (for this initial version) that the number
  must match exactly. we also say that any type
  is comparable to the poly type. (we check
  polytypes by typechecking the body after replacement
  by a monotype. this means that the defining
  occurance of a polymorphic procedure, is not
  truly typechecked. at least the portions which
  interoperate with the poly type. however,
  once a monotype has been provided, in this
  case it is present within the actual argument
  list, we typecheck the monotyped instance of
  the polymorphic procedure whose formal arguments are
  now typed as the actual arguments, if we can type
  the new monomorphic instance, we can validly call the
  monomorphic instance. (and we can inferr
  that this application site is a call to that
  specific monomorphic instance.))
  only in the case of an application site whose
  actual argument types are polymorphic at
  runtime would we need to apply the dispatch
  procedure, whose sole job is to dispatch to
  the right existing monomorphic procedure based on
  the runtime types of it's arguments.

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
          that to be here, we, by definition, are observing
          the type of a variable appearing as the leftmost
          term in an application term; which is also
          by definition appearing as a subterm of a
          PolyLambda object. given that we
          are currently observing a term which is
          supposed to be substituted with another
          term of monomorphic type at some later time
          than right now during typechecking,
          we have absolutely no way of inferring any
          information on the possible return type
          of this polymorphic term could be,
          except of course, that the term itself
          is going to be polymorphic, which means
          that we type the entire expression
          as polymorphic. this can be thought
          of as infecting the rest of the expression
          outwards, as all of the other judgements around
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
