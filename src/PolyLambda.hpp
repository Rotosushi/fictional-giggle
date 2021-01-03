
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
  string arg_id;
  shared_ptr<Type> arg_type;
  shared_ptr<SymbolTable> scope;
  shared_ptr<Ast> body;
  shared_ptr<vector<string>> cleanup_list;
  list<shared_ptr<Type>> instances;

  PolyLambda(string a_i, shared_ptr<Type> a_t,
         shared_ptr<SymbolTable> enclosing_scope, const shared_ptr<Ast>& bd)
    : arg_id(a_i),
      arg_type(a_t),
      scope(enclosing_scope),
      body(bd),
      cleanup_list(shared_ptr<vector<string>>(new vector<string>())),
      instances()
  {

  }

  PolyLambda(PolyLambda& other)
    : arg_id(other.arg_id),
      arg_type(other.arg_type),
      scope(other.scope),
      body(other.body),
      cleanup_list(shared_ptr<vector<string>>(new vector<string>(*(other.cleanup_list)))),
      instances(other.instances)
  {

  }

  EvalJudgement HasInstance(shared_ptr<Type> target_type, Environment env);

  virtual unique_ptr<Object> clone() override;
  virtual string to_string() override;
  virtual TypeJudgement getype(Environment& env) override;
  virtual void substitute(string& var, shared_ptr<Ast>* term, shared_ptr<Ast>& value, Environment& env) override;
  virtual bool appears_free(string& var) override;
  virtual void rename_binding(string& old_name, string& new_name) override;
};
