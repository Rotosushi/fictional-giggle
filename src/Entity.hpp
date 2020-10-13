#pragma once
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <memory>
using std::shared_ptr;

#include "Ast.hpp"
#include "SymbolTable.hpp"
#include "OperatorTable.hpp"

class Object
{
public:
  virtual shared_ptr<Literal> clone() { return clone_internal(); }
  virtual string to_string() { return to_string_internal(); }
  virtual TypeJudgement getype(SymbolTable* env, OperatorTable* ops) { return getype_internal(env, ops); }

protected:
  virtual shared_ptr<Literal> clone_internal() = delete;
  virtual string to_string_internal() = delete;
  virtual TypeJudgement getype_internal(SymbolTable* env, OperatorTable* ops) = delete;
};

class Integer : public Object
{
public:
  int value;

protected:
  virtual shared_ptr<Literal> clone_internal() override;
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(SymbolTable* env, OperatorTable* ops) override;
};

class Boolean : public Object
{
public:
  bool value;

protected:
  virtual shared_ptr<Literal> clone_internal() override;
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(SymbolTable* env, OperatorTable* ops) override;
};

class Lambda : public Object
{
public:
  //vector<pair<string,Type>> args;
  string arg_id;
  shared_ptr<Type> arg_type;
  SymbolTable scope;
  shared_ptr<Ast> body;

  Lambda(const string& a_id, const shared_ptr<Type>& a_type, const SymbolTable* enclosing_scope, const shared_ptr<Ast>& bd)
    : arg_id(a_id), arg_type(a_type), scope(enclosing_scope), body(bd) {}

  Lambda(const Lambda& other)
    : arg_id(other.arg_id), arg_type(other.arg_type), scope(other.scope), body(other.body) {}


protected:
  virtual shared_ptr<Literal> clone_internal() override;
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(SymbolTable* env, OperatorTable* ops) override;
};

class PolyLambda : public Object
{
public:
  Lambda def;
  list<Lambda> instances;

  PolyLambda(Lambda& def)
    : def(def) instances() {}

  PolyLambda(const PolyLambda& other)
    : def(other.def), instances(other.instances) {}

  optional<Lambda> HasInstance(shared_ptr<Type> target_type, SymbolTable* env, OperatorTable* ops);

protected:
  virtual shared_ptr<Literal> clone_internal() override;
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(SymbolTable* env, OperatorTable* ops) override;
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

  Entity(int i, const Location& l)
    : Ast(l), literal(make_unique(Integer(i))) {}

  Entity(bool b, const Location& l)
    : Ast(l), literal(make_unique(Boolean(b))) {}

  Entity(const Lambda& l, const Location& l)
    : Ast(l), literal(make_unique(Lambda(l))) {}

  Entity(const PolyLambda& l, const Location& l)
    : Ast(l), literal(make_unique(PolyLambda(l))) {}

protected:
  virtual shared_ptr<Ast> clone_internal() override;
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(SymbolTable* env, OperatorTable* ops) override;
  virtual EvalJudgement evaluate_internal(SymbolTable* env, OperatorTable* ops) override;
};
