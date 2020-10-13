#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Ast.hpp"
#include "SymbolTable.hpp"
#include "OperatorTable.hpp"
#include "TypeJudgement.hpp"

class Conditional : public Ast
{
public:
  shared_ptr<Ast> cond;
  shared_ptr<Ast> fst;
  shared_ptr<Ast> snd;

  Conditional(shared_ptr<Ast> c, shared_ptr<Ast> f, shared_ptr<Ast> s, const Location& l)
    : Ast(l), cond(c), fst(f), snd(s) {}

  Conditional(const Conditional& other)
    : Ast(other.location), cond(other.cond), fst(other.fst), snd(other.snd) {}

protected:
  virtual shared_ptr<Ast> clone_internal() override;
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(SymbolTable* env, OperatorTable* ops) override;
  virtual EvalJudgement evaluate_internal(SymbolTable* env, OperatorTable* ops) override;
};
