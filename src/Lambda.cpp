
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
  int len = args.size();
  for (int i = 0; i < len; ++i)
  {
    auto& arg = args[i];
    result += get<string>(arg);
    result += ": ";
    result += get<shared_ptr<Type>>(arg)->to_string();
    // every arg except the last is in a comma separated list
    if (i < (len - 1))
      result += ", ";
  }
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
    // or is my memory corrupt somehow???? <<-- this is more likely
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
    however, we know that whatever happens, in the term lower than
    this point, the number of names we are substituting in is
    smaller or equal to the length. (so we always go down or
    stay the same, which points to the recursion stopping eventually.
    as all positive integer values reduce to zero. wheeee.)

  */
  /*
  notice how this single if statement does
  the equivalent of the below algorithm,
  when the argument list has a size of one.
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
  // or pointless work. though, i'm sure we are still
  // redoing a lot of computation still based on
  // the purely recursive nature of this algorithm.
  if (new_subs.size() > 0)
  {
    /*
      okay, so we have avoided substituting for variables
      which appear bound lower in the term.
      we now need to avoid introducing a binding in the
      term being substituted in, with a binding occuring
      in the arguments of this lambda. if that is the
      case, then we need to rename the bindings within the
      body of the lambda before we can substitute.
    */
    vector<pair<string, string>> renamings;
    vector<string> bound_names;
    vector<string> appeared_free;
    for (auto&& arg : args)
      bound_names.push_back(get<string>(arg));

    // if we see any bound name conflicts they are
    // recorded within appeared_free.
    if ((*term->appears_free(bound_names, appeared_free)))
    {
      // so we need to construct a list of pairs
      // such that each bound name is associated with an
      // appropriate replacement name, such that
      // each replacement name itself does not
      // appear bound within the procedure.
      vector<string> possible_renamings;
      vector<string> renamings_free;
      /*
        okay, this seems wrong in the cases where we need
        to loop. but the first iteration, if each name is
        selected unique the first time, we shouldn't have
        any issues.
        the problem stems from the fact that we never remove
        names from the appeared_free if we genereate a name
        which was unique from the bindings, but then did
        appear_free within the term. this binding would
        expand the size of the appeared_free list, by inserting
        the newly conflicting name, but this name would still
        occur within the list upon the next iteration,
        which would cause the algorithm to generate a superfluous
        name on the next iteration. i think this could cause further issues.
        but this bug would occur based upon RNG as well,
        which makes it triply nasty if it happens.
      */
      do
      {
        possible_renamings.clear();
        for (int i = 0; i < appeared_free.size(); i++)
        {
          possible_renamings.push_back(generate_name(5));
        }

      } while ((*term)->appears_free(possible_renamings, appeared_free))

      // every element which appeared_free is renamed to its possible new name.
      for (int i = 0; i < appeared_free.size(); i++)
      {
        renamings.push_back(make_pair(appeared_free[i], possible_renamings[i]));
      }

      // rename the formal arguments which conflict.
      for (auto&& arg : args)
        for (auto&& pair : renamings)
          if (get<0>(pair) == get<string>(arg))
          {
            get<string>(arg) = get<1>(pair);
          }
      // rename the formal arguments appearances within the body
      body->rename_binding_in_body_internal(renamings);
    }

    body->substitute(new_subs, &body, env);
  }

}

void Lambda::rename_binding_in_body_internal(vector<pair<string, string>>& renaming_pairs)
{
  /*
    if within the body of the lambda whose binding we are renaming
    is itself a lambda whose has a biding with the same name as what we are
    looking to replace, we do not rename, because that lambda
    is introducing that binding for it's own body, and that makes
    it a separate binding than the one we are looking for.
    if the binding is not the one that we are looking for, then
    we need to look for instances of the binding within the
    body of the lambda.
   */

}

bool Lambda::appears_free(vector<string>& names, vector<string>& appeared_free)
{
  /*
  if (arg_id == name)
  {
    // arg_id matches the free name, meaning
    // that the name appears bound in the body
    // of this lambda.
    return false;
  }
  else
  {
    return body->appears_free(name);
  }
  */
}
