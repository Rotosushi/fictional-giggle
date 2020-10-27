
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <memory>
using std::shared_ptr;
using std::unique_ptr;
using std::make_shared;
using std::make_unique;

#include "Ast.hpp"
#include "Environment.hpp"
#include "Entity.hpp"

unique_ptr<Object> Object::clone()
{
  return clone_internal();
}

string Object::to_string()
{
  return to_string_internal();
}

TypeJudgement Object::getype(Environment env)
{
  return getype_internal(env);
}

/* ------------------------------------------------------------------------- */

unique_ptr<Object> TypeLiteral::clone_internal()
{
  return unique_ptr<Object>(new TypeLiteral(*this));
}

string TypeLiteral::to_string_internal()
{
  return value->to_string();
}

TypeJudgement TypeLiteral::getype_internal(Environment env)
{
  return value->clone();
}

/* ------------------------------------------------------------------------- */

unique_ptr<Object> Nil::clone_internal()
{
  return unique_ptr<Object>(new Nil(*this));
}

string Nil::to_string_internal()
{
  return "nil";
}

TypeJudgement Nil::getype_internal(Environment env)
{
  return TypeJudgement(shared_ptr<Type>(new MonoType(AtomicType::Nil, Location())));
}

/* ------------------------------------------------------------------------- */

unique_ptr<Object> Integer::clone_internal()
{
  return unique_ptr<Object>(new Integer(*this));
}

string Integer::to_string_internal()
{
  return std::to_string(value);
}

TypeJudgement Integer::getype_internal(Environment env)
{
  return TypeJudgement(shared_ptr<Type>(new MonoType(AtomicType::Int, Location())));
}

/* ------------------------------------------------------------------------- */

unique_ptr<Object> Boolean::clone_internal()
{
  return unique_ptr<Object>(new Boolean(*this));
}

string Boolean::to_string_internal()
{
  if (value)
    return "true";
  else
    return "false";
}

TypeJudgement Boolean::getype_internal(Environment env)
{
  return TypeJudgement(shared_ptr<Type>(new MonoType(AtomicType::Bool, Location())));
}

/* ------------------------------------------------------------------------- */

unique_ptr<Object> Lambda::clone_internal()
{
  return unique_ptr<Object>(new Lambda(*this));
}

string Lambda::to_string_internal()
{
  string result;
  result  = "\\";
  result += arg_id;
  result += ": ";
  result += arg_type->to_string();
  result += " => ";
  result += body->to_string();
  return result;
}

TypeJudgement Lambda::getype_internal(Environment env)
{
  /*
        ENV |- id : type1, term : type2
        --------------------------------
    ENV |- \ id : type1 => term : type1 -> type2
  */

  this->scope->bind(this->arg_id, shared_ptr<Ast>(new Entity(this->arg_type, Location())));
  TypeJudgement type2 = body->getype(Environment(this->scope, env.precedences, env.binops, env.unops));
  this->scope->unbind(this->arg_id);

  if (type2)
  {
    return TypeJudgement(shared_ptr<Type>(new ProcType(this->arg_type, type2.u.jdgmt, Location())));
  }
  else
  {
    return type2;
  }
}

/* ------------------------------------------------------------------------- */

optional<Lambda> PolyLambda::HasInstance(shared_ptr<Type> target_type, Environment env)
{
  /*
  the only way to introduce a polymorphic
  type is to omit a procedure argument's type
  annotation.
  */
  if (this->def.arg_type->is_polymorphic())
  {
    /*
      look for a possible overload to evaluate,
      if one exists return a copy for evaluation.
      if none exists try and create one,
      if the created version types, return
      a copy of it for evaluation,
      otherwise report that no
      instance can be constructed for the
      target type
    */
  }
  else
  {
    /*
      look for a possible match in the definition,
      and if that fails and no alternatives exist,
      then report no instance for the target type
      exist. if there is a match, return a copy
      of the lambda to be evaluated.
    */
  }
}

unique_ptr<Object> PolyLambda::clone_internal()
{
  return unique_ptr<Object>(new PolyLambda(*this));
}

string PolyLambda::to_string_internal()
{
  return def.to_string();
}

TypeJudgement PolyLambda::getype_internal(Environment env)
{
  return def.getype(env);
}

/* ------------------------------------------------------------------------- */

shared_ptr<Ast> Entity::clone_internal()
{
  return shared_ptr<Ast>(new Entity(*this));
}

string Entity::to_string_internal()
{
  return literal->to_string();
}

TypeJudgement Entity::getype_internal(Environment env)
{
  return literal->getype(env);
}

EvalJudgement Entity::evaluate_internal(Environment env)
{
  /*
    so, honestly, we might seriously consider
    = deleting this procedure, just to ensure that
    we never even attempt to evaluate objects.
    but, with the way that i want to write the
    main evaluate method, i doubt it will matter.
  */
  return EvalJudgement(shared_ptr<Ast>(new Entity(*this)));
}
