#pragma once
#include <string>
using std::string;
#include <list>
using std::list;
#include <vector>
using std::vector;
#include <memory>
using std::shared_ptr;
using std::unique_ptr;

#include "Ast.hpp"
#include "Environment.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Object.hpp"
#include "TypeLiteral.hpp"
#include "Nil.hpp"
#include "Integer.hpp"
#include "Boolean.hpp"
#include "Lambda.hpp"
#include "PolyLambda.hpp"





/*
  we know that each entity has a location,
  so we can simply factor that into the
  Ast itself.
*/
class Entity : public Ast
{
public:
  unique_ptr<Object> literal;

  Entity(void* v, const Location& loc)
    : Ast(loc), literal((new Nil())) {}

  Entity(int i, const Location& loc)
    : Ast(loc), literal((new Integer(i))) {}

  Entity(bool b, const Location& loc)
    : Ast(loc), literal((new Boolean(b))) {}

  Entity(shared_ptr<Type> l, const Location& loc)
    : Ast(loc), literal(unique_ptr<Object>(new TypeLiteral(l))) {}

  Entity(unique_ptr<Lambda> l, const Location& loc)
    : Ast(loc), literal(move(l)) {}

  Entity(unique_ptr<PolyLambda> l, const Location& loc)
    : Ast(loc), literal(move(l)) {}

  Entity(unique_ptr<Object> literal, const Location& loc)
    : Ast(loc), literal(move(literal)) {}

  Entity(const Entity& other)
    : Ast(other.location), literal(other.literal->clone()) {}

  Entity& operator=(const Entity& rhs)
  {
    literal = rhs.literal->clone();
    return *this;
  }

protected:
  virtual void substitute_internal(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env) override;
  virtual bool appears_free_internal(string var) override;
  virtual void rename_binding_internal(string old_name, string new_name) override;
  virtual shared_ptr<Ast> clone_internal() override;
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(Environment env) override;
  virtual EvalJudgement evaluate_internal(Environment env) override;
};
