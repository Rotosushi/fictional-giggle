
#pragma once
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
#include "Lambda.hpp"

class PolyLambda : public Object
{
public:
  Lambda def;
  list<vector<shared_ptr<Type>>> instances;
  /*
  and now, soon:

  Constraints principle_type;

  */

  PolyLambda(Lambda& def)
    : def(def), instances() {}

  PolyLambda(PolyLambda& other)
    : def(other.def), instances(other.instances) {}

  EvalJudgement HasInstance(vector<shared_ptr<Type>> target_arg_types, Environment env);

  virtual void substitute(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env) override;
  virtual void rename_binding(string old_name, string new_name) override;
  virtual bool appears_free(string name) override;
  virtual unique_ptr<Object> clone() override;
  virtual string to_string() override;
  virtual TypeJudgement getype(Environment env) override;
};
