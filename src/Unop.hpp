#pragma once
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <utility>
using std::pair;
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
  virtual void substitute_internal(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env) override;
  virtual bool appears_free_internal(vector<string>& names, vector<string>& appeared_free) override;
  virtual void rename_binding_in_body_internal(vector<pair<string, string>>& renaming_pairs) override;
  virtual shared_ptr<Ast> clone_internal() override;
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(Environment env) override;
  virtual EvalJudgement evaluate_internal(Environment env) override;



};
