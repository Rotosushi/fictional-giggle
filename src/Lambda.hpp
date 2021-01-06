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

/*
 since monomorphic lambdas can never be polymorphic,
 we can store them as monomorphs. when we construct
 a polymorphic lambda, we expect that to be a set of
 lambdas already. and so when we consider a formal
 procedure definition, we can consider it as an overload set
 of lambdas. the one major difference between specialization
 and overloads is that we can rationalize changing the number of
 arguments between overloads, whereas we cannot rationalize
 changing the number of arguments of a template (so dynamically.)
 we could imagine a different template over a different number
 of names, perhaps we could allow these two templates to share
 a name? this seems ill advised however, as this further confuses
 the concept of a closure around a template.
*/

class Lambda : public Object
{
public:
  string arg_id;
  shared_ptr<Type> arg_type;
  shared_ptr<SymbolTable> scope;
  shared_ptr<Ast> body;
  shared_ptr<vector<string>> cleanup_list;

  Lambda(string a_i, shared_ptr<Type> a_t,
         shared_ptr<SymbolTable> enclosing_scope, const shared_ptr<Ast>& bd)
    : arg_id(a_i),
      arg_type(a_t),
      scope(enclosing_scope),
      body(bd),
      cleanup_list(shared_ptr<vector<string>>(new vector<string>()))
  {

  }

  Lambda(const Lambda& other)
    : arg_id(other.arg_id),
      arg_type(other.arg_type),
      scope(other.scope),
      body(other.body),
      cleanup_list(shared_ptr<vector<string>>(new vector<string>(*(other.cleanup_list))))
  {

  }

  void rename_bindings(vector<string>& to_rename);

  virtual unique_ptr<Object> clone() override;
  virtual string to_string() override;
  virtual TypeJudgement getype(Environment& env) override;
  virtual void substitute(string& var, shared_ptr<Ast>* term, shared_ptr<Ast>& value, Environment& env) override;
  virtual void rename_binding(string& old_name, string& new_name) override;
  virtual bool appears_free(string& var) override;
};
