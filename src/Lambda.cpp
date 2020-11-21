
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;
using std::unique_ptr;

#include "Ast.hpp"
#include "Environment.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Object.hpp"
#include "Entity.hpp"
#include "Lambda.hpp"


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
    if ((*this->cleanup_list).size() > 0)
    {
      // destructors get called here
      for (string& id : (*this->cleanup_list))
      {
        this->scope->unbind(id);
      }

      (*this->cleanup_list).clear();
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
