#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Location.hpp"
#include "Type.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "SymbolTable.hpp"
#include "OperatorTable.hpp"

/*
  an Ast could be defined as anything
  in the programming language which can be
  said to participate in Typeing and evaluation.
*/


class Ast
{
public:
  Location location;
  Ast() = delete;
  Ast(const Location& loc) : location(loc) {}
  Ast(const Ast& other) : location(other.location) {}
  virtual ~Ast() = default;

  virtual shared_ptr<Ast> clone();
  virtual string to_string();
  virtual TypeJudgement getype(SymbolTable* env, OperatorTable* ops);
  virtual EvalJudgement evaluate(SymbolTable* env, OperatorTable* ops);
protected:
  virtual shared_ptr<Ast> clone_interal() = delete;
  virtual string to_string_internal() = delete;
  virtual TypeJudgement getype_internal(SymbolTable* env, OperatorTable* ops) = delete;
  virtual EvalJudgement evaluate_internal(SymbolTable* env, OperatorTable* ops) = delete;
};
