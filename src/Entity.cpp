
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

/* ------------------------------------------------------------------------- */

unique_ptr<Object> TypeLiteral::clone()
{
  return unique_ptr<Object>(new TypeLiteral(*this));
}

string TypeLiteral::to_string()
{
  return value->to_string();
}

TypeJudgement TypeLiteral::getype(Environment env)
{
  return value->clone();
}

void TypeLiteral::substitute(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env)
{
  return;
}

void TypeLiteral::rename_binding(string old_name, string new_name)
{
  return;
}

bool TypeLiteral::appears_free(string name)
{
  return false;
}

/* ------------------------------------------------------------------------- */

unique_ptr<Object> Nil::clone()
{
  return unique_ptr<Object>(new Nil(*this));
}

string Nil::to_string()
{
  return "nil";
}

TypeJudgement Nil::getype(Environment env)
{
  return TypeJudgement(shared_ptr<Type>(new MonoType(AtomicType::Nil, Location())));
}

void Nil::substitute(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env)
{
  return;
}

void Nil::rename_binding(string old_name, string new_name)
{
}

bool Nil::appears_free(string name)
{
  return false;
}

/* ------------------------------------------------------------------------- */

unique_ptr<Object> Integer::clone()
{
  return unique_ptr<Object>(new Integer(*this));
}

string Integer::to_string()
{
  return std::to_string(value);
}

TypeJudgement Integer::getype(Environment env)
{
  return TypeJudgement(shared_ptr<Type>(new MonoType(AtomicType::Int, Location())));
}

void Integer::substitute(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env)
{
  return;
}

void Integer::rename_binding(string old_name, string new_name)
{
  return;
}

bool Integer::appears_free(string name)
{
  return false;
}

/* ------------------------------------------------------------------------- */

unique_ptr<Object> Boolean::clone()
{
  return unique_ptr<Object>(new Boolean(*this));
}

string Boolean::to_string()
{
  if (value)
    return "true";
  else
    return "false";
}

TypeJudgement Boolean::getype(Environment env)
{
  return TypeJudgement(shared_ptr<Type>(new MonoType(AtomicType::Bool, Location())));
}

void Boolean::substitute(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env)
{
  return;
}

void Boolean::rename_binding(string old_name, string new_name)
{
  return;
}

bool Boolean::appears_free(string name)
{
  return false;
}
/* ------------------------------------------------------------------------- */

unique_ptr<Object> Lambda::clone()
{
  return unique_ptr<Object>(new Lambda(*this));
}

string Lambda::to_string()
{
  string result;
  result  = "\\ ";
  result += arg_id;
  result += ": ";
  result += arg_type->to_string();
  result += " => ";
  result += body->to_string();
  return result;
}

TypeJudgement Lambda::getype(Environment env)
{
  /*
        ENV |- id : type1, term : type2
        --------------------------------
    ENV |- \ id : type1 => term : type1 -> type2
  */

  this->scope->bind(this->arg_id, shared_ptr<Ast>(new Entity(this->arg_type, Location())));
  TypeJudgement type2 = body->getype(Environment(this->scope, env.precedences, env.binops, env.unops, this->cleanup_list));
  this->scope->unbind(this->arg_id);

  if (type2)
  {
    // destructors get called here
    for (string& id : (*this->cleanup_list))
    {
      this->scope->unbind(id);
    }
    return TypeJudgement(shared_ptr<Type>(new ProcType(this->arg_type, type2.u.jdgmt, Location())));
  }
  else
  {
    return type2;
  }
}

void Lambda::substitute(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env)
{
  if (arg_id == var)
  {
    return;
  }
  else
  {
    return body->substitute(var, &body, value, env);
  }
}

void Lambda::rename_binding(string old_name, string new_name)
{
  if (arg_id == old_name)
  {
    arg_id = new_name;
  }
  body->rename_binding(old_name, new_name);
}

bool Lambda::appears_free(string name)
{
  if (arg_id == name)
  {
    // arg_id matches the free name, meaning
    // that the name appears free in the body
    // of this lambda.
    return false;
  }
  else
  {
    return body->appears_free(name);
  }
}

/* ------------------------------------------------------------------------- */

EvalJudgement PolyLambda::HasInstance(shared_ptr<Type> target_type, Environment env)
{
  auto build_target_lambda = [this](shared_ptr<Type> target_type)
  {
    return shared_ptr<Ast>(new Entity(unique_ptr<Lambda>(new Lambda(def.arg_id, target_type, def.scope, def.body)), Location()));
  };
  /*
  the only way to introduce a polymorphic
  type is to omit a procedure argument's type
  annotation.
  */

  if (this->def.arg_type->is_polymorphic())
  {
    /*
      look for a possible overload to evaluate,
      if one exists return it for evaluation.
      if none exists try and create one,
      if the created version types, return
      a copy of it for evaluation,
      otherwise report that no
      instance can be constructed for the
      target type
    */
    for (shared_ptr<Type> instance_type : instances)
    {
      TypeJudgement eqjdgmt = TypesEquivalent(instance_type, target_type);
      if (eqjdgmt)
      {
        return EvalJudgement(build_target_lambda(instance_type));
      }
    }

    shared_ptr<Ast> newInst = build_target_lambda(target_type);

    TypeJudgement newInstJdgmt = newInst->getype(env);

    if (newInstJdgmt)
    {
      instances.push_back(target_type->clone());
      return EvalJudgement(newInst);
    }
    else
    {
      return EvalJudgement(EvalError(newInstJdgmt.u.error));
    }
  }
  else
  {
    /*
    this polylambda is not polymorphic, meaning it is
    simply overloaded, which doesn't happen according
    to the grammar, but it is the other side of the
    if conditional, so it's included early.
      look for a possible match in the definition,
      and if that fails and no alternatives exist,
      then report no instance for the target type
      exist. if there is a match, return a copy
      of the lambda to be evaluated.

    it's also implemented early, because it is easy to
    imagine it's implementation given the above implementation.
    */
    TypeJudgement defeqjdgmt = TypesEquivalent(def.arg_type, target_type);
    if (defeqjdgmt)
    {
      return EvalJudgement(build_target_lambda(target_type));
    }

    for (shared_ptr<Type> instance_type : instances)
    {
      TypeJudgement eqjdgmt = TypesEquivalent(instance_type, target_type);
      if (eqjdgmt)
      {
        return EvalJudgement(build_target_lambda(target_type));
      }
    }

    string errdsc = "No instance of Overloaded Lambda found for target type ["
                  + target_type->to_string()
                  + "]";
    return EvalJudgement(EvalError(Location(), errdsc));
  }
}

unique_ptr<Object> PolyLambda::clone()
{
  return unique_ptr<Object>(new PolyLambda(*this));
}

string PolyLambda::to_string()
{
  return def.to_string();
}

TypeJudgement PolyLambda::getype(Environment env)
{
  return def.getype(env);
}

void PolyLambda::substitute(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env)
{
  if (def.arg_id == var)
  {
    // the name appears bound in term,
    // not free, so we cannot substitute.
    return;
  }
  else
  {
    // we can search the body for places to
    // substitute.
    return def.body->substitute(var, &def.body, value, env);
  }
}

void PolyLambda::rename_binding(string old_name, string new_name)
{
  if (def.arg_id == old_name)
  {
    def.arg_id = new_name;
  }
  def.body->rename_binding(old_name, new_name);
}

bool PolyLambda::appears_free(string name)
{
  if (def.arg_id == name)
  {
    // the name appears bound in this term,
    // not free.
    return false;
  }
  else
  {
    return def.body->appears_free(name);
  }
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
  return EvalJudgement(shared_ptr<Ast>(new Entity(*this)));
}

void Entity::substitute_internal(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env)
{
  return literal->substitute(var, term, value, env);
}

bool Entity::appears_free_internal(string var)
{
  return literal->appears_free(var);
}

void Entity::rename_binding_internal(string old_name, string new_name)
{
  literal->rename_binding(old_name, new_name);
}
