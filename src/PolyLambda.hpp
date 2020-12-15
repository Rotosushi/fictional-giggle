
#pragma once
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <utility>
using std::pair;
#include <memory>
using std::shared_ptr;
using std::unique_ptr;

#include "Ast.hpp"
#include "Environment.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Object.hpp"
#include "Lambda.hpp"

/*
 so, polylambda uses a lambda as a member so
 we don't have to repeat the typing and evaluation
 judgements for lambdas within here.
 given that they are equivalent for the two
 constructs, given our definitions of the other
 language constructs handling of polymorphic
 names.
*/

class PolyLambda : public Object
{
public:
  vector<pair<string, shared_ptr<Type>>> args;
  shared_ptr<SymbolTable> scope;
  shared_ptr<Ast> body;
  shared_ptr<vector<string>> cleanup_list;
  list<vector<shared_ptr<Type>>> instances;

  PolyLambda(vector<pair<string, shared_ptr<Type>>>& args,
         shared_ptr<SymbolTable> enclosing_scope, const shared_ptr<Ast>& bd)
    : args(args),
      scope(enclosing_scope),
      body(bd),
      cleanup_list(shared_ptr<vector<string>>(new vector<string>())),
      instances()
  {

  }

  PolyLambda(PolyLambda& other)
    : args(other.args),
      scope(other.scope),
      body(other.body),
      cleanup_list(shared_ptr<vector<string>>(new vector<string>(*(other.cleanup_list)))),
      instances(other.instances)
  {

  }

  EvalJudgement HasInstance(vector<shared_ptr<Type>> target_arg_types, Environment env);

  virtual void substitute(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env) override;
  virtual void rename_binding_in_body(vector<pair<string, string>>& renaming_pairs) override;
  virtual bool appears_free(vector<string>& names, vector<string>& appeared_free) override;
  virtual unique_ptr<Object> clone() override;
  virtual string to_string() override;
  virtual TypeJudgement getype(Environment env) override;
};
