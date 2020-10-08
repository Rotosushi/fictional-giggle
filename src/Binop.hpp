#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"


class Binop : public Ast
{
public:
  string op;
  shared_ptr<Ast> lhs;
  shared_ptr<Ast> rhs;

  Binop(const string& op, shared_ptr<Ast> lhs, shared_ptr<Ast> rhs, const Location& loc)
    : Ast(loc), op(op), lhs(lhs), rhs(rhs) {}

  Binop(const Binop& other)
    : Ast(other.loc), op(other.op), lhs(other.lhs), rhs(other.rhs) {}

protected:
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(SymbolTable* env, BinopSet* binops) override;
  virtual EvalJudgement evaluate_internal(SymbolTable* env, BinopSet* binops) override;
};
