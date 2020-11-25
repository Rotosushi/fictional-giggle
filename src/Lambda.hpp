#pragma once
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <utility>
using std::pair;
#include <memory>
using std::shared_ptr;
using std::unique_ptr;

#include "Ast.hpp"
#include "Environment.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Object.hpp"

class Lambda : public Object
{
public:
  vector<pair<string, shared_ptr<Type>>> args;
  shared_ptr<SymbolTable> scope;
  shared_ptr<Ast> body;
  shared_ptr<list<string>> cleanup_list;

  Lambda(vector<pair<string, shared_ptr<Type>>>& args,
         shared_ptr<SymbolTable> enclosing_scope, const shared_ptr<Ast>& bd)
    : args(args),
      scope(enclosing_scope),
      body(bd),
      cleanup_list(shared_ptr<list<string>>(new list<string>()))
  { }

  Lambda(const Lambda& other)
    : args(other.args),
      scope(other.scope),
      body(other.body->clone()),
      cleanup_list(shared_ptr<list<string>>(new list<string>(*(other.cleanup_list))))
  { }

  virtual void substitute(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env) override;
  virtual void rename_binding(string old_name, string new_name) override;
  virtual bool appears_free(string name) override;
  virtual unique_ptr<Object> clone() override;
  virtual string to_string() override;
  virtual TypeJudgement getype(Environment env) override;
};
