
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

#include "Gensym.hpp"
#include "Ast.hpp"
#include "Environment.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Entity.hpp"
#include "Object.hpp"
#include "Lambda.hpp"
#include "PolyLambda.hpp"


EvalJudgement PolyLambda::HasInstance(vector<shared_ptr<Type>> target_types, Environment env)
{

  if (target_types.size() != args.size())
  {
    string errdsc = "mismatch of the number of actual arguments ["
                  + std::to_string(target_types.size())
                  + "] to formal arguments ["
                  + std::to_string(args.size())
                  + "]";
    EvalError err(Location(), errdsc);
    return EvalJudgement(err);
  }
  /*

  */
  auto build_target_lambda = [this](vector<shared_ptr<Type>> target_types)
  {
    vector<pair<string, shared_ptr<Type>>> arglist;
    int len = target_types.size();
    for (int i = 0; i < len; ++i)
    {
      pair<string, shared_ptr<Type>>& arg = args[i];
      arglist.push_back(make_pair(get<string>(arg), target_types[i]));
    }
    return shared_ptr<Ast>(new Entity(unique_ptr<Object>(new Lambda(arglist, scope, body)), Location()));
  };

  // this could be return an optional error term,
  // which would allow us to report the mismatch
  // to the user.
  auto argument_types_match = [this](vector<shared_ptr<Type>>& formal_types, vector<shared_ptr<Type>>& actual_types)
  {
    bool match = false;

    int len = formal_types.size();
    for (int i = 0; i < len; i++)
    {
      TypeJudgement eqjdgmt = TypesEquivalent(formal_types[i], actual_types[i]);

      if (eqjdgmt && i == len)
        match = true;
      else if (!eqjdgmt)
      {
        match = false;
        break;
      }
      // there is a little hole here for the
      // "i need to check the rest of the args"
      // case within this loop.
      // 'hole' is when (eqjdgmt == true && i != len)
    }

    return match;
  };

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

  for (vector<shared_ptr<Type>>& instance_types : instances)
  {
    if (argument_types_match(instance_types, target_types))
    {
      return EvalJudgement(build_target_lambda(instance_types));
    }
  }

  shared_ptr<Ast> newInst = build_target_lambda(target_types);

  TypeJudgement newInstJdgmt = newInst->getype(env);

  if (newInstJdgmt)
  {
    instances.push_back(target_types);
    return EvalJudgement(newInst);
  }
  else
  {
    return EvalJudgement(EvalError(newInstJdgmt.u.error));
  }
}

unique_ptr<Object> PolyLambda::clone()
{
  return unique_ptr<Object>(new PolyLambda(*this));
}

string PolyLambda::to_string()
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

TypeJudgement PolyLambda::getype(Environment env)
{
  /*
    (the entire argument list is the first portion
      of the type, and the return type is the last part.)

        ENV |- id : type1, term : type2
        --------------------------------
    ENV |- \ id : type1 => term : type1 -> type2
  */
  for (pair<string, shared_ptr<Type>>& arg : this->args)
  {
    this->scope->bind(get<string>(arg), shared_ptr<Ast>(new Entity(get<shared_ptr<Type>>(arg), Location())));
  }

  TypeJudgement type2 = body->getype(Environment(this->scope, env.precedences, env.binops, env.unops, this->cleanup_list));

  for (pair<string, shared_ptr<Type>>& arg : this->args)
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

// recall that when we call substitute on a polylambda object,
// that always corresponds to an evaluation context, and just
// like with Lambdas we avoid substituting for any bound ids
// within the body. (these are what the procedure is polymorphic
// over (and will be handed within an Application term),
// whereas this substitution corresponds to bindings between
// this polylambda and it's outer context.)
void PolyLambda::substitute(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env)
{
  auto name_exists_in_args = [](string& name, vector<pair<string, shared_ptr<Type>>>& args)
  {
    for (auto& arg: args)
      if (get<string>(arg) == name)
        return true;
    return false;
  };

  vector<pair<string, shared_ptr<Ast>>> new_subs;
  for (auto& sub : subs)
    if (!name_exists_in_args(get<string>(sub), args))
      new_subs.push_back(sub);

  /// just to save ourselves from doing a bunch
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
    for (auto& arg : args)
      bound_names.push_back(get<string>(arg));

    // if we see any bound name conflicts they are
    // recorded within appeared_free.
    if (((*term)->appears_free(bound_names, appeared_free)))
    {
      // so we need to construct a list of pairs
      // such that each bad name is associated with an
      // appropriate replacement name, such that
      // each replacement name itself does not
      // appear free within the term.
      for (string& free_name : appeared_free)
        renamings.push_back(make_pair(free_name, gensym()));

      // rename the formal arguments which conflict.
      for (auto& arg : args)
        for (auto& pair : renamings)
          if (get<0>(pair) == get<string>(arg))
          {
            get<string>(arg) = get<1>(pair);
          }
      // rename the formal arguments appearances within the body
      body->rename_binding_in_body(renamings);
    }

    body->substitute(new_subs, &body, env);
  }
}

// this is from a context of trying to avoid improper
// bindings between terms, so in this case we are actually
// specifically trying to modify the bound variables.
void PolyLambda::rename_binding_in_body(vector<pair<string, string>>& renaming_pairs)
{
  /*
    if within the body of the lambda whose bindings we are renaming
    is itself a lambda which has a biding with the same name as what we are
    looking to replace, we do not rename, because that lambda
    is introducing that binding for it's own body, and that makes
    it a separate binding than the one we are looking for, despite
    the name match.
    in fact, given that this lambda is introducing the binding,
    all instances of the name within the lambdas body are separate
    from the binding we are looking for. now, we do not want to
    modify the list of pairs we were passed (because it is a reference
    to save space, and that means we are sharing the list with the rest of
    the algorithm peices to do our work.) now, we have a clever
    solution to this problem, we allocate a new list, and pass it in
    while typing the rest of the body, this does double duty of
    keeping the renaming consistent with the rules of substitution,
    and threading the right data to the right parts of the algorithm
    without having to directly communicate with any of the other
    peices of the algorithm. (yay side effects!)
    This practice is especially usefull in the downwards sense
    of the tree of execution, in the reverse sense, really the
    usefullness is present in the constructor pattern, building
    things and then returning what you built. otherwise we need
    to consider who has ownership of memory and all that.
   */
   auto name_exists_in_args = [this](string& name)
   {
     for (auto& arg: args)
       if (get<string>(arg) == name)
         return true;
     return false;
   };

   vector<pair<string, string>> new_pairs;

   for (pair<string, string>& old_pair : renaming_pairs)
   {
     if (!name_exists_in_args(get<0>(old_pair)))
     {
       new_pairs.emplace_back(old_pair);
     }
   }

   if (new_pairs.size() > 0)
    body->rename_binding_in_body(new_pairs);
}

bool PolyLambda::appears_free(vector<string>& names, vector<string>& appeared_free)
{
  /*
  funnily enough
  appears_free is essentially rename_binding_in_body_internal

  every name which appears bound in this procedure by definition
  cannot appear_free any lower than this point (in the sense
  of the tree representing this execution).
  */

  // the closure allows for early exit of the
  // inner loop without my explicit use of a goto.
  // the alternative pattern is one of the few
  // justifiable uses of a goto in c/c++ imo.
  auto name_exists_in_args = [this](string& name)
  {
    for (pair<string, shared_ptr<Type>>& arg: args)
      if (get<string>(arg) == name)
        return true;
    return false;
  };

  vector<string> new_names;

  for (string& name : names)
    if (!name_exists_in_args(name))
    {
      new_names.emplace_back(name);
    }

  if (new_names.size() > 0)
    return body->appears_free(new_names, appeared_free);
  else
    return false;
}
