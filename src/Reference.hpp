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
using std::make_shared;

#include "Ast.hpp"
#include "Environment.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Object.hpp"

class Reference : public Object
{
public:
  shared_ptr<Ast> ref;

  Reference(shared_ptr<Ast> r)
    : ref(r) {}

  Reference(const Reference& other)
    : ref(other.ref) {}

  virtual unique_ptr<Object> clone() override;
  virtual string to_string() override;
  virtual TypeJudgement getype(Environment env) override;
  virtual void substitute(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env) override;
  virtual bool appears_free(vector<string>& names, vector<string>& appeared_free) override;
  virtual void rename_binding_in_body(vector<pair<string, string>>& renaming_pairs) override;
};
