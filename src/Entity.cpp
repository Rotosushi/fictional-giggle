
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
#include "SymbolTable.hpp"
#include "OperatorTable.hpp"
#include "Entity.hpp"

shared_ptr<Object> Object::clone()
{
  return clone_internal();
}

string Object::to_string()
{
  return to_string_internal();
}

TypeJudgement Object::getype(SymbolTable* env, OperatorTable* ops)
{
  return getype_internal(env, ops);
}

/* ------------------------------------------------------------------------- */

shared_ptr<Object> Nil::clone_internal()
{
  return make_shared(Nil(*this));
}

string Nil::to_string_internal()
{
  return "nil";
}

TypeJudgement Nil::getype_internal(SymbolTable* env, OperatorTable* env)
{
  return TypeJudgement(make_shared(MonoType(AtomicType::Nil)));
}

/* ------------------------------------------------------------------------- */

shared_ptr<Object> Integer::clone_interal()
{
  return make_shared(Integer(value));
}

string Integer::to_string_internal()
{
  return string(value);
}

TypeJudgement Integer::getype_internal(SymbolTable* env, BinopSet* binops)
{
  return TypeJudgement(MonoType(AtomicType::Int));
}

/* ------------------------------------------------------------------------- */

shared_ptr<Object> Boolean::clone_interal()
{
  return make_shared(Boolean(value));
}

string Boolean::to_string_internal()
{
  return string(value);
}

TypeJudgement Boolean::getype_internal(SymbolTable* env, BinopSet* binops)
{
  return TypeJudgement(MonoType(AtomicType::Bool));
}

/* ------------------------------------------------------------------------- */

shared_ptr<Object> Lambda::clone_interal()
{
  return make_shared(Lambda(arg_id, arg_type->clone(), scope, body->clone(), location));
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

  this->scope.bind(this->arg_id, this->arg_type);
  TypeJudgement type2 = body->getype(Environment(this->scope, env->precedences, env->binops, env->unops));
  this->scope.unbind(this->arg_id);

  if (type2)
  {
    return TypeJudgement(ProcType(this->arg_type, type2.u.judgement));
  }
  else
  {
    return type2;
  }
}

/* ------------------------------------------------------------------------- */

optional<Lambda> PolyLambda::HasInstance(shared_ptr<Type> target_type, SymbolTable* env, OperatorTable* ops)
{
  /*
  the only way to introduce a polymorphic
  type is to omit a procedure argument's type
  annotation.
  */
  if (this->arg_type->is_polymorphic())
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

shared_ptr<Object> PolyLambda::clone_internal()
{
  return make_shared(PolyLambda(*this));
}

/* ------------------------------------------------------------------------- */

shared_ptr<Ast> Entity::clone_interal()
{
  return make_shared(Entity(literal->clone()));
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
    deleting this procedure, just to ensure that
    we never even attempt to evaluate objects.
    but, with the way that i want to write the
    main evaluate method, i doubt it will matter.
  */
  return EvalJudgement(*this);
}
