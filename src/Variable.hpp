#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Ast.hpp"
#include "SymbolTable.hpp"

class Variable : public Ast
{
public:
  string id;

  Variable(const string& str, const Location& loc)
    : Ast(loc), id(str) {}
  Variable(const Variable& other)
    : Ast(other.loc), id(other.id) {}

protected:
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(SymbolTable* env, BinopSet* binops) override;
  virtual EvalJudgement evaluate_internal(SymbolTable* env, BinopSet* binops) override;
};
