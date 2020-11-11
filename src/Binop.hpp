#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Ast.hpp"
#include "Environment.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Entity.hpp"
#include "UnopEliminators.hpp"

class BinopSet;


class Binop : public Ast
{
public:
  string op;
  shared_ptr<Ast> lhs;
  shared_ptr<Ast> rhs;

  Binop(const string& op, shared_ptr<Ast> lhs, shared_ptr<Ast> rhs, const Location& loc)
    : Ast(loc), op(op), lhs(lhs), rhs(rhs) {}

  Binop(const Binop& other)
    : Ast(other.location), op(other.op), lhs(other.lhs), rhs(other.rhs) {}

protected:

  virtual void substitute_internal(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env) override;
  virtual bool appears_free_internal(string var) override;
  virtual void rename_binding_internal(string old_name, string new_name) override;
  virtual shared_ptr<Ast> clone_internal() override;
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(Environment env) override;
  virtual EvalJudgement evaluate_internal(Environment env) override;



};
