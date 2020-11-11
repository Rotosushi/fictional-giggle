#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Ast.hpp"
#include "Environment.hpp"
#include "TypeJudgement.hpp"

class Iteration : public Ast
{
public:
  shared_ptr<Ast> cond;
  shared_ptr<Ast> body;

  Iteration(shared_ptr<Ast> c, shared_ptr<Ast> b, const Location& l)
    : Ast(l), cond(c), body(b) {}

  Iteration(const Iteration& other)
    : Ast(other.location), cond(other.cond), body(other.body) {}

protected:
  virtual void substitute_internal(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env) override;
  virtual bool appears_free_internal(string var) override;
  virtual void rename_binding_internal(string old_name, string new_name) override;
  virtual shared_ptr<Ast> clone_internal() override;
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(Environment env) override;
  virtual EvalJudgement evaluate_internal(Environment env) override;

};
