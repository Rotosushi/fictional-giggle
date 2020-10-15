#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Ast.hpp"
#include "SymbolTable.hpp"
#include "OperatorTable.hpp"

class Variable : public Ast
{
public:
  string id;

  Variable(const string& str, const Location& loc)
    : Ast(loc), id(str) {}
  Variable(const Variable& other)
    : Ast(other.location), id(other.id) {}

protected:
  virtual shared_ptr<Ast> clone_internal() override;
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(Environment env) override;
  virtual EvalJudgement evaluate_internal(Environment env) override;
};
