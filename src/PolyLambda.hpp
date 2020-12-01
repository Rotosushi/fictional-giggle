
#pragma once
#include <string>
using std::string;
#include <vector>
using std::vector>
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
  Lambda def;
  list<vector<shared_ptr<Type>>> instances;

  PolyLambda(Lambda& def)
    : def(def), instances() {}

  PolyLambda(PolyLambda& other)
    : def(other.def), instances(other.instances) {}

  EvalJudgement HasInstance(vector<shared_ptr<Type>> target_arg_types, Environment env);

  virtual void substitute(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env) override;
  virtual void rename_binding_in_body_internal(vector<pair<string, string>>& renaming_pairs) override;
  virtual bool appears_free(string name) override;
  virtual unique_ptr<Object> clone() override;
  virtual string to_string() override;
  virtual TypeJudgement getype(Environment env) override;
};
