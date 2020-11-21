#pragma once
#include <string>
using std::string;
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
  virtual void substitute(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env) override;
  virtual bool appears_free(string var) override;
  virtual void rename_binding(string old_name, string new_name) override;
};
