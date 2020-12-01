#pragma once
#include <string>
using std::string;
#include <vector>
using std::vector>
#include <utility>
using std::pair;
#include <memory>
using std::unique_ptr;
using std::shared_ptr;

#include "Ast.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Environment.hpp"
#include "Object.hpp"

class Empty : public Object
{
  int null;
public:
  Empty();

  Empty(const Empty& other);

  virtual ~Empty() {};

  virtual unique_ptr<Object> clone() override;
  virtual string to_string() override;
  virtual TypeJudgement getype(Environment env) override;
  virtual void substitute(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env) override;
  virtual bool appears_free(string var) override;
  virtual void rename_binding_in_body_internal(vector<pair<string, string>>& renaming_pairs) override;
};
