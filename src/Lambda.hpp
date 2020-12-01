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

  void rename_bindings(vector<string>& to_rename);

  virtual void substitute(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env) override;
  virtual void rename_binding_in_body_internal(vector<pair<string, string>>& renaming_pairs) override;
  virtual bool appears_free(vector<string>& names, vector<string>& appeared_free) override;
  virtual unique_ptr<Object> clone() override;
  virtual string to_string() override;
  virtual TypeJudgement getype(Environment env) override;
};
