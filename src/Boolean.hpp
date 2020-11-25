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

class Boolean : public Object
{
public:
  bool value;

  Boolean(bool v) : value(v) {}
  Boolean(const Boolean& other) : value(other.value) {}

  virtual void substitute(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env) override;
  virtual void rename_binding(string old_name, string new_name) override;
  virtual bool appears_free(string name) override;
  virtual unique_ptr<Object> clone() override;
  virtual string to_string() override;
  virtual TypeJudgement getype(Environment env) override;
};
