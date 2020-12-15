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
/*
  types, but as first class values.
  these are an implementation secret,
  only used for typechecking monomorphic
  procedures.
  someday, maybe I can come up with an argument
  for the validity of terms like
  mytype := <type> "type-op" <type>

  and maybe even
  sndtype := mytype "type-op" <type>

  further:
  typeproc := \x:Type => x "type-op" <type>

  which we probably want to restrict to compile time
  execution, because I think allowing the program
  to modify itself on the fly is not a static language
  feature. (this could very well be a place where the language
  could branch here.)
*/
class TypeLiteral : public Object
{
public:
  shared_ptr<Type> value;

  TypeLiteral(shared_ptr<Type> t) : value(t) {}
  TypeLiteral(const TypeLiteral& other) : value(other.value) {}

  virtual void substitute(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env) override;
  virtual void rename_binding_in_body(vector<pair<string, string>>& renaming_pairs) override;
  virtual bool appears_free(vector<string>& names, vector<string>& appeared_free) override;
  virtual unique_ptr<Object> clone() override;
  virtual string to_string() override;
  virtual TypeJudgement getype(Environment env) override;
protected:

};
