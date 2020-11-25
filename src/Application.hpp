#pragma once
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <memory>
using std::shared_ptr;
using std::make_shared;

#include "Ast.hpp"
#include "Environment.hpp"

class Application : public Ast
{
public:
  shared_ptr<Ast> lhs;
  vector<shared_ptr<Ast>> actual_args;

  Application(shared_ptr<Ast> l, vector<shared_ptr<Ast>> args, const Location& loc)
    : Ast(loc), lhs(l), actual_args(r) {}

  Application(const Application& other)
    : Ast(other.location), lhs(other.lhs)
  {
    for (shared_ptr<Ast> arg : other.actual_args)
    {
      this->actual_args.push_pack(arg->clone());
    }
  }

protected:
  virtual shared_ptr<Ast> clone_internal() override;
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(Environment env) override;
  virtual EvalJudgement evaluate_internal(Environment env) override;
  virtual void substitute_internal(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env) override;
  virtual bool appears_free_internal(string var) override;
  virtual void rename_binding_internal(string old_name, string new_name) override;
};
