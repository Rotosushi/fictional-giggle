
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <utility>
using std::pair;
using std::make_pair;
using std::get;
#include <memory>
using std::shared_ptr;
using std::make_shared;

#include "PinkException.hpp"
#include "Ast.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Environment.hpp"
#include "Entity.hpp"
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

TypeJudgement Application::getype_internal(Environment& env)
{
  /*
   ENV |- lhs : type1 -> type2, rhs : type1
   --------------------------------------------
           ENV |- lhs rhs : type2
  */
   TypeJudgement typeA = lhs->getype(env);


   if (typeA) // could we type the lhs?
   {
     shared_ptr<Type> tA =  typeA.u.jdgmt;
     ProcType* pt = dynamic_cast<ProcType*>(tA.get());
     if (pt) // does the lhs have procedure type?
     {
       if (tA->is_polymorphic()) // is the procedure polymorphic?
       {
         /*
           then the result of the application is also polymorphic.
         */
         return TypeJudgement(shared_ptr<Type>(new MonoType(AtomicType::Poly, Location())));
       }
       else
       {
         TypeJudgement typeB = rhs->getype(env);

         if (typeB)
         {
             shared_ptr<Type> type1 = pt->argument_type;

             if (type1 == nullptr)
               throw PinkException("bad lhs type\n", __FILE__, __LINE__);

             shared_ptr<Type> type2 = pt->return_type;

             if (type2 == nullptr)
               throw PinkException("bad rhs type\n", __FILE__, __LINE__);

             shared_ptr<Type> type3 = typeB.u.jdgmt;

             if (type3 == nullptr)
               throw PinkException("bad argument type\n", __FILE__, __LINE__);

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
               return TypeJudgement(TypeError(location, errdsc));
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
         // not MonoType, not ProcType, it's undefined!
         throw PinkException("bad lhs application ptr\n", __FILE__, __LINE__);
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
           information on what the possible return type
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
         return TypeJudgement(shared_ptr<Type>(new MonoType(AtomicType::Poly, lhs->location)));
       }
       else
       {
         string errdsc = "cannot apply non-procedure type ["
                       + tA->to_string()
                       + "]\n";
         return TypeJudgement(TypeError(location, errdsc));
       }
     }
   }
   else
   {
     return typeA;
   }
}


EvalJudgement Application::evaluate_internal(Environment& env)
{
  auto is_lambda = [](shared_ptr<Ast> term)
    {
      Entity* ent = dynamic_cast<Entity*>(term.get());

      if (!ent)
        return false;

      Lambda* lam = dynamic_cast<Lambda*>(ent->literal.get());

      if (!lam)
        return false;
      else
        return true;
    };

    auto is_entity = [](shared_ptr<Ast> term)
    {
      Entity* entity = dynamic_cast<Entity*>(term.get());
      return entity != nullptr;
    };
    /*
      evaluate the lhs down to a lambda,
      evaluate the argument down to a
        value.
      the type of the argument must match
      the type of the value. (and if the
      lambda is monomorphic, the typechecker
      will have already confirmed that.)
      if the lambda is a PolyLambda, then
      we must use HasInstance to extract
      a monomorph before we can then
      evaluate the monomorphic instance
      just like we evaluate a lambda,
      or report the errror.
      term1 -> term1'
  ----------------------
  term1 term2 -> term1' term2
      term2 -> term2'
  ----------------------
  value1 term2 -> value1 term2'
  (\ id : type = term) (value2) -> [id -> value2]term
    */
    EvalJudgement lhsEvalJdgmt = lhs->evaluate(env);

    if (!lhsEvalJdgmt)
      return lhsEvalJdgmt;

    if (lhsEvalJdgmt)
    {
      Entity* ent = dynamic_cast<Entity*>(lhsEvalJdgmt.u.jdgmt.get());
      if (ent)
      {
        EvalJudgement rhsEvalJdgmt = rhs->evaluate(env);

        if (!rhsEvalJdgmt)
          return rhsEvalJdgmt;

        PolyLambda* polyLam = dynamic_cast<PolyLambda*>(ent->literal.get());

        Lambda* evallam = nullptr;

        if (polyLam)
        {

          TypeJudgement rhsTypeJdgmt = rhsEvalJdgmt.u.jdgmt->getype(env);
          // extract an evaluatable instance before we
          // can substitute. we need the type of the rhs
          // in order to extract an instance.
          if (rhsTypeJdgmt)
          {
            shared_ptr<Type> rhstype = rhsTypeJdgmt.u.jdgmt;


            EvalJudgement polyLamInstJdgmt = polyLam->HasInstance(rhstype, env);

            if (polyLamInstJdgmt)
            {
              // normally, unguarded casts are some stinky
              // code. however, HasInstance only deals in
              // Lambda Entity return values. so we have
              // some assurances here.
              shared_ptr<Ast> polyInstPtr = polyLamInstJdgmt.u.jdgmt;
              Entity* polyInst = dynamic_cast<Entity*>(polyInstPtr.get());
              evallam = dynamic_cast<Lambda*>(polyInst->literal.get());
              // the lambda set up for evaluation has come
              // from the set of procedures held within the PolyLambda set.
            }
            else
            {
              // we could not extract an instance from the PolyLambda,
              // the reason will be stored within the EvalJudgement
              // HasInstance returned.
              return polyLamInstJdgmt;
            }
          }
          else
          {
            // somehow we failed to type the rhs??
            return EvalJudgement(EvalError(rhsTypeJdgmt.u.error.location(), rhsTypeJdgmt.u.error.what()));
          }
        }
        else
        {
          // not a polymorph, is it a monomorph?
          // if this fails, we know this is a Literal Object
          // and it is not something we can apply.
          evallam = dynamic_cast<Lambda*>(ent->literal.get());
        }

        // so at this point in the algorithm we either
        // have an instance of the lambda we want to
        // execute pointed to by evallam. (evil-lamb?)
        // or we have a nullptr.
        if (evallam)
        {
          // we have our lambda object, and our value to
          // substitute in. so boom, let's substitute.
          shared_ptr<Ast> temp = evallam->body->clone();

          temp->substitute(evallam->arg_id, &temp, rhsEvalJdgmt.u.jdgmt, env);

          return EvalJudgement(temp);
        }
        else
        {
          // not a polymorph or a monomorph,
          // still an entity.
          string errdsc = "Cannot apply the Object ["
                        + lhsEvalJdgmt.u.jdgmt->to_string()
                        + "], expecting a [Lambda] or [PolyLambda]";
          return EvalJudgement(EvalError(location, errdsc));
        }
      }
      else
      {
        // not an entity term on the lhs
        string errdsc = "Cannot apply term ["
                      + lhsEvalJdgmt.u.jdgmt->to_string()
                      + "], expecting a [Lambda] or [PolyLambda]";
        return EvalJudgement(EvalError(location, errdsc));
      }
    }
    else
    {
      return lhsEvalJdgmt;
    }
}


void Application::substitute_internal(string& var, shared_ptr<Ast>* term, shared_ptr<Ast>& value, Environment& env)
{
  //[id -> value]lhs rhs := [id -> value]lhs [id -> value]rhs
  lhs->substitute(var, &lhs, value, env);
  rhs->substitute(var, &rhs, value, env);
}

bool Application::appears_free_internal(string& var)
{
  bool bl = lhs->appears_free(var);
  bool ba = rhs->appears_free(var);
  return bl || ba;
}

void Application::rename_binding_internal(string& old_name, string& new_name)
{
  lhs->rename_binding(old_name, new_name);
  rhs->rename_binding(old_name, new_name);
}































/* ------------------------------------------------------------------ */
