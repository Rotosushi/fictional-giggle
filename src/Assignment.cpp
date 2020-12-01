#include <string>
using std::string;
#include <vector>
using std::vector>
#include <utility>
using std::pair;
using std::get;
#include <memory>
using std::shared_ptr;
using std::make_shared;

#include "Ast.hpp"
#include "Type.hpp"
#include "Entity.hpp"
#include "Reference.hpp"
#include "Environment.hpp"
#include "Assignment.hpp"

shared_ptr<Ast> Assignment::clone_internal()
{
  return shared_ptr<Ast>(new Assignment(dst->clone(), src->clone(), Location()));
}

string Assignment::to_string_internal()
{
  return dst->to_string() + " <- " + src->to_string();
}

TypeJudgement Assignment::getype_internal(Environment env)
{
  auto is_ref_type = [](shared_ptr<Type> type)
  {
    RefType* reftype = dynamic_cast<RefType*>(type.get());
    return reftype != nullptr;
  };
  /*
    ENV |- lhs : ref T, rhs : T
    ------------------------------
    ENV |- lhs '<-' rhs : T

    the destination has to have refrence type,
    the source has to have the same type as the
    refferent type of the lhs, then we can say
    this is a valid assignment term.
    also, given the strict nature of the language,
    and the desire to build things, we allow references
    as first class values. this means we need to evaluate
    both sides before the assignment can occur.
    (typeing is already always greedy)
  */
  TypeJudgement dstjdgmt = dst->getype(env);

  if (!dstjdgmt)
    return dstjdgmt;

  RefType* reftype = dynamic_cast<RefType*>(dstjdgmt.u.jdgmt.get());

  if (reftype != nullptr)
  {
    shared_ptr<Type> dsttype = reftype->ref_type;

    TypeJudgement srcjdgmt = src->getype(env);

    if (!srcjdgmt)
      return srcjdgmt;

    shared_ptr<Type> srctype = srcjdgmt.u.jdgmt;

    if (TypesEquivalent(dsttype, srctype))
    {
      /*
        since we can assign the ref the value of the
        source term, we are free to make the result of
        evaluation the value of the source term itself.
        (this allows for compound assignment expressions,
         as opposed to returning nil here, which while just as
         valid an typeing/execution strategy, nixes compound assignment.)
      */
      return TypeJudgement(srctype);
    }
    else
    {
      string errdsc = "Cannot assign value of type ["
                    + srctype->to_string()
                    + "] to destination of type ["
                    + dstjdgmt.u.jdgmt->to_string()
                    + "]";
      TypeError error(location, errdsc);
      return TypeJudgement(error);
    }
  }
  else
  {
    string errdsc = "Cannot assign to ["
                  + dstjdgmt.u.jdgmt->to_string()
                  + "], destination must have reference type.";
    TypeError error(location, errdsc);
    return TypeJudgement(error);
  }
}

EvalJudgement Assignment::evaluate_internal(Environment env)
{
  /*
    ENV |- lhs : ref T, rhs : T
    ------------------------------
    ENV |- lhs '<-' rhs : T

    the destination has to have refrence type,
    the source has to have the same type as the
    refferent type of the lhs, then we can say
    this is a valid assignment term.
    also, given the strict nature of the language,
    and the desire to build things, we allow references
    as first class values. this means we need to evaluate
    both sides before the assignment can occur.
  */
  EvalJudgement dstjdgmt = dst->evaluate(env);

  if (!dstjdgmt)
    return dstjdgmt;

  Entity* ent = dynamic_cast<Entity*>(dstjdgmt.u.jdgmt.get());

  Reference* reference = dynamic_cast<Reference*>(ent->literal.get());

  if (reference != nullptr)
  {
    EvalJudgement srcjdgmt = src->evaluate(env);

    if (!srcjdgmt)
      return srcjdgmt;

    shared_ptr<Ast> value = srcjdgmt.u.jdgmt;

    reference->ref = value;
    return value;
  }
  else
  {
    string errdsc = "Cannot assign to ["
                  + dstjdgmt.u.jdgmt->to_string()
                  + "], destination must be a reference";
    EvalError error(location, errdsc);
    return EvalJudgement(error);
  }
}

void Assignment::substitute_internal(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env)
{
  // okay, so like, are there any shenanigans
  // regarding name conflicts in this term?
  // pretty sure assignment cannot introduce a
  // binding so no.
  // but what we are substituting for may appear
  // in either side, unlike binding, which never needs
  // to substitute for the binding it introduces.
  dst->substitute(subs, &dst, env);
  src->substitute(subs, &src, env);
}

bool Assignment::appears_free_internal(vector<string>& names, vector<string>& appeared_free)
{
  bool bd = dst->appears_free(names, appeared_free);
  bool bs = src->appears_free(names, appeared_free);
  return bd || bs;
}

void Assignment::rename_binding_in_body_internal(vector<pair<string, string>>& renaming_pairs)
{
  dst->rename_binding(renaming_pairs);
  src->rename_binding(renaming_pairs);
}
