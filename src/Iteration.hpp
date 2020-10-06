#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Ast.hpp"
#include "SymbolTable.hpp"
#include "TypeJudgement.hpp"

class Iteration : public Ast
{
public:
  shared_ptr<Ast> cond;
  shared_ptr<Ast> body;

  Iteration(shared_ptr<Ast> c, shared_ptr<Ast> b, const Location& l)
    : Ast(l), cond(c), body(b) {}

  Iteration(const Iteration& other)
    : Ast(other.location), cond(other.cond), body(other.body) {}

protected:
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(SymbolTable* env) override;
  virtual EvalJudgement evaluate_internal(SymbolTable* env) override;

};
