#pragma once
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <memory>
using std::shared_ptr;

#include "Ast.hpp"
#include "SymbolTable.hpp"

class Literal
{
public:
  string to_string() { return to_string_internal(); }
  TypeJudgement getype(SymbolTable* env) { return getype_internal(env); }

protected:
  virtual string to_string_internal() = delete;
  virtual TypeJudgement getype_internal(SymbolTable* env) = delete;
};

class Integer : public Literal
{
public:
  int value;

protected:
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(SymbolTable* env) override;
};

class Boolean : public Literal
{
public:
  bool value;

protected:
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(SymbolTable* env) override;
};

class Lambda : public Literal
{
public:
  //vector<pair<string,Type>> args;
  string arg_id;
  shared_ptr<Type> arg_type;
  SymbolTable scope;
  shared_ptr<Ast> body;
  list<Lambda> alternatives;

  Lambda(const string& a_id, const shared_ptr<Type>& a_type, const SymbolTable* enclosing_scope, const shared_ptr<Ast>& bd)
    : arg_id(a_id), arg_type(a_type), scope(enclosing_scope), body(bd) {}

  Lambda(const Lambda& other)
    : arg_id(other.arg_id), arg_type(other.arg_type), scope(other.scope), body(other.body), alternatives(other.alternatives) {}

  optional<Lambda> HasInstance(shared_ptr<Type> target_type);

protected:
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(SymbolTable* env) override;
};

/*
  I think if you do a replacement of
  Entity for Object, this language would
  start to sound object oriented.
*/
class Entity : public Ast
{
public:
  unique_ptr<Literal> literal;

  Entity(int i, const Location& l)
    : Ast(l), literal(make_unique(Integer(i))) {}

  Entity(bool b, const Location& l)
    : Ast(l), literal(make_unique(Boolean(b))) {}

  Entity(const Lambda& l, const Location& l)
    : Ast(l), literal(make_unique(Lambda(l))) {}

protected:
  virtual string to_string_internal() override
  {
    return literal->to_string();
  }

  virtual TypeJudgement getype_internal(SymbolTable* env) override
  {
    return literal->getype(env);
  }

  virtual EvalJudgement evaluate_internal(SymbolTable* env) override
  {
    return EvalJudgement(*this);
  }
};
