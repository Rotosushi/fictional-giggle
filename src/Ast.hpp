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
  Ast(const Location& loc) : loc(loc) {}
  virtual ~Ast() = default;

  virtual string to_string();
  virtual TypeJudgement getype(SymbolTable* env);
  virtual EvalJudgement evaluate(SymbolTable* env);
protected:
  virtual string to_string_internal() = delete;
  virtual TypeJudgement getype_internal(SymbolTable* env) = delete;
  virtual EvalJudgement evaluate_internal(SymbolTable* env) = delete;
};
