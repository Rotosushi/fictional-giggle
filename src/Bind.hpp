#pragma once
#include <string>
using std::string;
#include <vector>
using std::vector>
#include <utility>
using std::pair;
#include <memory>
using std::shared_ptr;

#include "Ast.hpp"
#include "Environment.hpp"
#include "TypeJudgement.hpp"

class Bind : public Ast
{
public:
  string id;
  shared_ptr<Ast> rhs;

  Bind(const string& str, shared_ptr<Ast> r, const Location& l)
    : Ast(l), id(str), rhs(r) {}

  Bind(const Bind& other)
    : Ast(other.location), id(other.id), rhs(other.rhs) {}

protected:
  virtual void substitute_internal(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env) override;
  virtual bool appears_free_internal(vector<string>& names, vector<string>& appeared_free) override;
  virtual void rename_binding_in_body_internal(vector<pair<string, string>>& renaming_pairs) override;
  virtual shared_ptr<Ast> clone_internal() override;
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(Environment env) override;
  virtual EvalJudgement evaluate_internal(Environment env) override;

};
