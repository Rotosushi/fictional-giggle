#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;
using std::make_shared;

#include "Ast.hpp"

class Application : public Ast
{
public:
  shared_ptr<Ast> lhs;
  shared_ptr<Ast> rhs;

  Application(shared_ptr<Ast> l, shared_ptr<Ast> r, const Location& loc)
    : Ast(loc), lhs(l), rhs(r) {}

  Application(const Application& other)
    : Ast(other.loc), lhs(other.lhs), rhs(other.rhs) {}

protected:
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(SymbolTable* env) override;
  virtual EvalJudgement evaluate_internal(SymbolTable* env) override;

};
