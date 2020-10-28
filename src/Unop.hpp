#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Ast.hpp"
#include "Environment.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"


class Unop : public Ast
{
public:
  string op;
  shared_ptr<Ast> rhs;

  Unop(const string& op, shared_ptr<Ast> rhs, const Location& loc)
    : Ast(loc), op(op), rhs(rhs) {}

  Unop(const Unop& other)
    : Ast(other.location), op(other.op), rhs(other.rhs) {}

protected:
  virtual shared_ptr<Ast> clone_internal() override;
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(Environment env) override;
  virtual EvalJudgement evaluate_internal(Environment env) override;
  virtual void substitute(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env) override;
  virtual bool appears_free(string var) override;
  virtual void rename_binding(string old_name, string new_name) override;
};
