#include <string>
using std::string;
#include <vector>
using std::vector;
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

TypeJudgement Assignment::getype_internal(Environment& env)
{
  auto is_ref_type = [](shared_ptr<Type> type)
  {
    RefType* reftype = dynamic_cast<RefType*>(type.get());
    return reftype != nullptr;
  };
  /*
    ENV |- lhs : T, rhs : T
    ------------------------------
    ENV |- lhs '<-' rhs : T

    okay, slight change of plan, references are a great
    concept, and do theoretically allow one to build
    anything, however, I want to interact directly
    with the hardware, such that we can turn on LED's
    and such, this requires me to be able to point at
    memory locations which correlate to pins on the
    CPU. I want to write code which interacts directly with
    the specialized IC's on the board, drivers and whatnot.
    now, this requires creative semantic definitions of
    our language features such that the operation of
    the machine and the abstract machine of the language
    are in alignment in many aspects.
    where what is abstracted away is
    the specific way which this CPU
    adds two numbers, and what is brought to the
    conceptual surface is that this hardware element
    has this interface, and you can write values of
    this size to it. and then from there we can write
    libraries to further abstract the usage of these
    hardware components. essentially, i want to bring good
    software design pattern support into the
    abstraction layer of C. so i can use some restricted
    higher level concepts in Embedded development.
    the stuff that we can find conveinent mappings for at
    least. and i want to start from a slightly different
    kernel from something like c++ or objective-c.
    short reason OOP offers one half of the
    solution to the expression problem, functional
    programming offers the other. i think open
    procedures along with essentially c style
    records and unions and single inheritance,
    gives the language enough
    scaffolding to really build procedures, instead
    of having to write all the scaffolding and then
    the behavior when you code in c. or as in c++/OOP
    languages and function languages show, they scale
    in one way or the other, but not uniformly to
    scale comprehensibly over time. (because the
    design needs to be able to be flexible yet sound
    this is super tricky, and i don't expect this current solution to
    completely solve any problems, however, these
    problems still need solving, and what am i doing but
    exploring the problem space.)
  */
  TypeJudgement dstjdgmt = dst->getype(env);

  if (!dstjdgmt)
    return dstjdgmt;

  shared_ptr<Type> dsttype = dstjdgmt.u.jdgmt;

  if (dsttype->is_polymorphic())
    return TypeJudgement(shared_ptr<Type>(new MonoType(AtomicType::Poly, Location())));

  TypeJudgement srcjdgmt = src->getype(env);

  if (!srcjdgmt)
    return srcjdgmt;

  shared_ptr<Type> srctype = srcjdgmt.u.jdgmt;

  if (srctype->is_polymorphic())
    return TypeJudgement(shared_ptr<Type>(new MonoType(AtomicType::Poly, Location())));

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

EvalJudgement Assignment::evaluate_internal(Environment& env)
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

  // evaluate the destination
  EvalJudgement dstevaljdgmt = dst->evaluate(env);

  if (!dstevaljdgmt)
    return dstevaljdgmt;

  TypeJudgement dsttypejdgmt = dstevaljdgmt.u.jdgmt->getype(env);

  if (!dsttypejdgmt)
    return EvalJudgement(EvalError(dsttypejdgmt.u.error));

  shared_ptr<Type> dsttype = dsttypejdgmt.u.jdgmt;


  // evaluate the source
  EvalJudgement srcevaljdgmt = src->evaluate(env);

  if (!srcevaljdgmt)
    return srcevaljdgmt;

  TypeJudgement srctypejdgmt = srcevaljdgmt.u.jdgmt->getype(env);

  if (!srctypejdgmt)
    return EvalJudgement(EvalError(srctypejdgmt.u.error));

  shared_ptr<Type> srctype = srctypejdgmt.u.jdgmt;

  if (TypesEquivalent(dsttype, srctype))
  {
    /*
      since we can assign the value of the
      source term, we are free to make the result of
      evaluation the value of the source term itself.
      (this allows for compound assignment expressions,
       as opposed to returning nil here, which while just as
       valid an typeing/execution strategy, nixes compound assignment.)
    */
    Entity* source = dynamic_cast<Entity*>(srcevaljdgmt.u.jdgmt.get());
    Entity* destination = dynamic_cast<Entity*>(dstevaljdgmt.u.jdgmt.get());

    destination->literal = source->literal->clone();
    return EvalJudgement(srcevaljdgmt.u.jdgmt);
  }
  else
  {
    string errdsc = "Cannot assign value of type ["
                  + srctype->to_string()
                  + "] to destination of type ["
                  + dsttype->to_string()
                  + "]";
    TypeError error(location, errdsc);
    return EvalJudgement(EvalError(error));
  }
}

void Assignment::substitute_internal(string& id, shared_ptr<Ast>* term, shared_ptr<Ast>& value, Environment& env)
{
  // okay, so like, are there any shenanigans
  // regarding name conflicts in this term?
  // pretty sure assignment cannot introduce a
  // binding so no.
  // but what we are substituting for may appear
  // in either side, unlike binding, which never needs
  // to substitute for the binding it introduces.
  dst->substitute(id, &dst, value, env);
  src->substitute(id, &src, value, env);
}

bool Assignment::appears_free_internal(string& id)
{
  bool bd = dst->appears_free(id);
  bool bs = src->appears_free(id);
  return bd || bs;
}

void Assignment::rename_binding_internal(string& old_name, string& new_name)
{
  dst->rename_binding(old_name, new_name);
  src->rename_binding(old_name, new_name);
}
