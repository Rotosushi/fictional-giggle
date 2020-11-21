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

class Lambda : public Object
{
public:
  //vector<pair<string,Type>> args;
  string arg_id;
  shared_ptr<Type> arg_type;
  shared_ptr<SymbolTable> scope;
  shared_ptr<Ast> body;
  shared_ptr<list<string>> cleanup_list;

  Lambda(const string& a_id, const shared_ptr<Type>& a_type,
         shared_ptr<SymbolTable> enclosing_scope, const shared_ptr<Ast>& bd)
    : arg_id(a_id), arg_type(a_type), scope(enclosing_scope), body(bd), cleanup_list(shared_ptr<list<string>>(new list<string>())) {}

  Lambda(const Lambda& other)
    : arg_id(other.arg_id), arg_type(other.arg_type->clone()), scope(other.scope), body(other.body->clone()), cleanup_list(shared_ptr<list<string>>(new list<string>()))
  {
    for (const string& id : (*other.cleanup_list))
    {
      cleanup_list->push_back(id);
    }
  }

  virtual void substitute(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env) override;
  virtual void rename_binding(string old_name, string new_name) override;
  virtual bool appears_free(string name) override;
  virtual unique_ptr<Object> clone() override;
  virtual string to_string() override;
  virtual TypeJudgement getype(Environment env) override;
};
