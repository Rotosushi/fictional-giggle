#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;
using std::make_shared;

#include "Ast.hpp"
#include "Environment.hpp"

class Reference : public Ast
{
public:
  shared_ptr<Ast> ref;

  Reference(shared_ptr<Ast> r, const Location& loc)
    : Ast(loc), ref(r) {}

  Reference(const Reference& other)
    : Ast(other.location), ref(other.ref) {}

protected:
  virtual shared_ptr<Ast> clone_internal() override;
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(Environment env) override;
  virtual EvalJudgement evaluate_internal(Environment env) override;
  virtual void substitute_internal(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env) override;
  virtual bool appears_free_internal(string var) override;
  virtual void rename_binding_internal(string old_name, string new_name) override;
};
