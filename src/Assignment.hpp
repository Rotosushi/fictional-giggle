#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;
using std::make_shared;

#include "Ast.hpp"
#include "Environment.hpp"


class Assignment : public Ast
{
public:
  shared_ptr<Ast> dst;
  shared_ptr<Ast> src;

  Assignment(shared_ptr<Ast> d, shared_ptr<Ast> s, const Location& l)
    : Ast(l), dst(d), src(s) {}

  Assignment(const Assignment& other)
    : Ast(other.location), dst(other.dst), src(other.src) {}

protected:
  virtual shared_ptr<Ast> clone_internal() override;
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(Environment env) override;
  virtual EvalJudgement evaluate_internal(Environment env) override;
  virtual void substitute_internal(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env) override;
  virtual bool appears_free_internal(string var) override;
  virtual void rename_binding_internal(string old_name, string new_name) override;
};
