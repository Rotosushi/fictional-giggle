#pragma once
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <memory>
using std::shared_ptr;
using std::unique_ptr;
using std::make_unique;

#include "Ast.hpp"
#include "Environment.hpp"

class Object
{
public:
  virtual shared_ptr<Object> clone();
  virtual string to_string();
  virtual TypeJudgement getype(Environment env);

protected:
  virtual shared_ptr<Object> clone_internal() = 0;
  virtual string to_string_internal() = 0;
  virtual TypeJudgement getype_internal(Environment env) = 0;
};

class Nil : public Object
{
public:
  Nil() {}
  Nil(const Nil& other) {}

protected:
  virtual shared_ptr<Object> clone_internal() override;
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(Environment env) override;
};

class Integer : public Object
{
public:
  int value;

  Integer(int v) : value(v) {}

protected:
  virtual shared_ptr<Object> clone_internal() override;
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(Environment env) override;
};

class Boolean : public Object
{
public:
  bool value;

  Boolean(bool v) : value(v) {}

protected:
  virtual shared_ptr<Object> clone_internal() override;
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(Environment env) override;
};

class Lambda : public Object
{
public:
  //vector<pair<string,Type>> args;
  string arg_id;
  shared_ptr<Type> arg_type;
  shared_ptr<SymbolTable> scope;
  shared_ptr<Ast> body;

  Lambda(const string& a_id, const shared_ptr<Type>& a_type, shared_ptr<SymbolTable> enclosing_scope, const shared_ptr<Ast>& bd)
    : arg_id(a_id), arg_type(a_type), scope(enclosing_scope), body(bd) {}

  Lambda(const Lambda& other)
    : arg_id(other.arg_id), arg_type(other.arg_type), scope(other.scope), body(other.body) {}


protected:
  virtual shared_ptr<Object> clone_internal() override;
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(Environment env) override;
};

class PolyLambda : public Object
{
public:
  Lambda def;
  list<Lambda> instances;

  PolyLambda(Lambda& def)
    : def(def), instances() {}

  PolyLambda(const PolyLambda& other)
    : def(other.def), instances(other.instances) {}

  optional<Lambda> HasInstance(shared_ptr<Type> target_type, SymbolTable* env, OperatorTable* ops);

protected:
  virtual shared_ptr<Object> clone_internal() override;
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(Environment env) override;
};

/*
  I think if you do a replacement of
  Literal for Object, this language would
  start to sound object oriented.
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

  Entity(const Lambda& l, const Location& loc)
    : Ast(loc), literal((new Lambda(l))) {}

  Entity(const PolyLambda& l, const Location& loc)
    : Ast(loc), literal((new PolyLambda(l))) {}

protected:
  virtual shared_ptr<Ast> clone_internal() override;
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(Environment env) override;
  virtual EvalJudgement evaluate_internal(Environment env) override;
};
