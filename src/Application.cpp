
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
  vector<shared_ptr<Ast>> arg_list;
  for (shared_ptr<Ast> arg : actual_args)
  {
    arg_list.push_back(arg->clone());
  }
  return shared_ptr<Ast>(new Application(lhs->clone(), arg_list, location));
}

string Application::to_string_internal()
{
  string result;
  result += "(";
  result += lhs->to_string();
  result += " ";
  int len = actual_args.size();
  for (int i = 0; i < len; i++)
  {
    auto& arg = actual_args[i];
    result += arg->to_string();
    if (i < (len - 1))
      result += " ";
  }
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
        return TypeJudgement(pt->return_type);
      }
      else
      {
        vector<shared_ptr<Type>> actual_types;

        if (actual_args.size() == pt->arg_types.size())
        {
          // can we obtain a type for each actual argument provided
          // by the application term?
          for (shared_ptr<Ast> arg : actual_args)
          {
            TypeJudgement argtypejdgmt = arg->getype(env);

            if (argtypejdgmt)
              actual_types.push_back(argtypejdgmt.u.jdgmt);
            else
              return argtypejdgmt;
          }

          int len = actual_types.size();
          for (int i = 0; i < len; ++i)
          {
            TypeJudgement argjdgmt = TypesEquivalent(actual_types[i], pt->arg_types[i]);

            if (!argjdgmt.succeeded())
              return argjdgmt;
          }

          // what allows us to provide symmetrical procedures
          // is when we allow the construction of tuples within
          // the language.
          return TypeJudgement(pt->return_type);
        }
        else
        {
          string errdsc = "formal argument types do not match actual argument types";
          TypeError te(location, errdsc);
          return TypeJudgement(te);
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
          and, since we still have to 'type' the term as
          polymorphic we are still allowed a little bit of
          error checking of polymorphic terms, it's really not
          much though, mostly in the parser honestly.
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


EvalJudgement Application::evaluate_internal(Environment env)
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

  Entity* ent = dynamic_cast<Entity*>(lhsEvalJdgmt.u.jdgmt.get());
  if (ent)
  {

      /*
        can we type the procedure?
        can we type each of the arguments?
        does the type of each actual argument match
         the type of each formal argument?
        if yes to all of the above,
          substitute each argument name for
          each actual value.
      */
      vector<shared_ptr<Type>> actual_types;
      vector<shared_ptr<Ast>> actual_values;
      Lambda* evallam = nullptr;

      for (auto& arg : actual_args)
      {
        // we evaluate the arguments before we attempt to
        // use their type to extract some value.
        // this means we apply polymorphic subterms and
        // then we can observe either the failed application,
        // or the monomorphic value that is the result of
        // the application. this is in contrast to the
        // typing judgement which uses the type of the
        // term unevaluated (albeit because it has no choice)
        // which means it never types polymorphic
        // terms! (that's okay though, because we always type
        // the attempts at applying polymorphic terms before
        // we "go wrong")
        EvalJudgement valuejdgmt = arg->evaluate(env);

        if (valuejdgmt)
        {
          actual_values.push_back(valuejdgmt.u.jdgmt);

          TypeJudgement value_type = valuejdgmt.u.jdgmt->getype(env);
          if (value_type)
            actual_types.push_back(value_type.u.jdgmt);
          else
            return EvalJudgement(EvalError(value_type.u.error));
        }
        else
          return valuejdgmt;
      }

      PolyLambda* polylam = dynamic_cast<PolyLambda*>(ent->literal.get());
      if (polylam != nullptr)
      {
        EvalJudgement instjdgmt = polylam->HasInstance(actual_types, env);

        if (instjdgmt)
        {
          Entity* mono = dynamic_cast<Entity*>(instjdgmt.u.jdgmt.get());
          evallam = dynamic_cast<Lambda*>(mono->literal.get());
        }
        else
        {
          return instjdgmt;
        }
      }
      else
      {
        // not a polymorph, is it a monomorph?
        // if this fails, we know this is an object
        // we can not apply.
        evallam = dynamic_cast<Lambda*>(ent->literal.get());
      }

    // so at this point in the algorithm we either
    // have an instance of the lambda we want to
    // execute pointed to by evallam. (evil-lamb?)
    // or we have a nullptr, meaning we have an
    // entity on the lhs that is not a lambda.
    // not is it an entity from which we could
    // retrieve a lambda.
    if (evallam)
    {
      vector<pair<string, shared_ptr<Ast>>> substitutions;

      int len = evallam->args.size();
      for (int i = 0; i < len; i++)
      {
        string& arg_id = get<string>(evallam->args[i]);
        shared_ptr<Ast>& arg_value = actual_values[i];
        substitutions.push_back(make_pair(arg_id, arg_value));
      }

      // we have our lambda object, and our values to
      // substitute in. so boom, let's substitute.
      shared_ptr<Ast> temp = evallam->body->clone();

      temp->substitute(substitutions, &temp, env);

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


void Application::substitute_internal(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env)
{
  //[id -> value]lhs rhs := [id -> value]lhs [id -> value]rhs
  lhs->substitute(subs, &lhs, env);

  for (shared_ptr<Ast>& arg : actual_args)
  {
    arg->substitute(subs, &arg, env);
  }
}

bool Application::appears_free_internal(vector<string>& names, vector<string>& appeared_free)
{
  bool bl = lhs->appears_free(names, appeared_free);
  bool ba = true;
  for (shared_ptr<Ast>& arg : actual_args)
  {
    ba = arg->appears_free(names, appeared_free);
  }
  return bl || ba;
}

void Application::rename_binding_in_body_internal(vector<pair<string, string>>& renaming_pairs)
{
  lhs->rename_binding_in_body(renaming_pairs);

  for (shared_ptr<Ast>& arg : actual_args)
  {
    arg->rename_binding_in_body(renaming_pairs);
  }
}































/* ------------------------------------------------------------------ */
