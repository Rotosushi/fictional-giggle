
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <utility>
using std::pair;
using std::get;
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
  for (pair<string, shared_ptr<Type>>&& arg : this->args)
  {
    this->scope->bind(get<string>(arg), shared_ptr<Ast>(new Entity(get<shared_ptr<Type>>(arg), Location())));
  }

  TypeJudgement type2 = body->getype(Environment(this->scope, env.precedences, env.binops, env.unops, this->cleanup_list));

  for (pair<string, shared_ptr<Type>>&& arg : this->args)
  {
    this->scope->unbind(get<string>(arg));
  }

  if (type2)
  {
    // without this check,
    // the for-each loop mechanism seg faults
    // because the standard case of a container
    // having no elements isn't handled properly???
    // or is my memory corrupt somehow????
    // either way, this fixes the problem so...
    if ((*this->cleanup_list).size() > 0)
    {
      // destructors would get called here
      // if we had any
      for (string& id : (*this->cleanup_list))
      {
        this->scope->unbind(id);
      }

      (*this->cleanup_list).clear();
    }

    return TypeJudgement(shared_ptr<Type>(new ProcType(this->args, type2.u.jdgmt, Location())));
  }
  else
  {
    return type2;
  }
}

void Lambda::substitute(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env)
{
  /*
    construct a new ids and values list to pass further into
    this recursion, such that any name which appears bound in
    this lambda is removed from the new ids and values sets.
    this is because we do not want to replace terms which appear
    bound in this lower scope. (we also do not want to destroy
    information in the substitution chain as we have no idea
    where the algorithm is going from here, from here.)

  */
  /*
  if (arg_id == var)
  {
    return;
  }
  else
  {
    return body->substitute(ids, &body, values, env);
  }
  */
  auto name_exists_in_args = [](string& name, vector<pair<string, shared_ptr<Type>>>& args)
  {
    for (auto&& arg: args)
      if (get<string>(arg) == name)
        return true;
    return false;
  };

  vector<pair<string, shared_ptr<Ast>>> new_subs;
  for (auto&& sub : subs)
    if (!name_exists_in_args(get<string>(sub), args))
      new_subs.push_back(sub);

  // just to save ourselves from doing a bunch
  // or pointless work.
  if (new_subs.size() > 0)
    body->substitute(new_subs, &body, env);
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
