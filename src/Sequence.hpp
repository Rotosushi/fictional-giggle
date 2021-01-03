#pragma once
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <utility>
using std::pair;
#include <memory>
using std::shared_ptr;

#include "Ast.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Environment.hpp"

class Sequence : public Ast
{
public:
  shared_ptr<Ast> lhs;
  shared_ptr<Ast> rhs;

  Sequence(shared_ptr<Ast> l, shared_ptr<Ast> r, const Location& loc)
    : Ast(loc), lhs(l), rhs(r) {}

  Sequence(const Sequence& other)
    : Ast(other.location), lhs(other.lhs), rhs(other.rhs) {}

protected:
    virtual shared_ptr<Ast> clone_internal() override;
    virtual string to_string_internal() override;
    virtual TypeJudgement getype_internal(Environment& env) override;
    virtual EvalJudgement evaluate_internal(Environment& env) override;
    virtual void substitute_internal(string& var, shared_ptr<Ast>* term, shared_ptr<Ast>& value, Environment& env) override;
    virtual bool appears_free_internal(string& var) override;
    virtual void rename_binding_internal(string& old_name, string& new_name) override;
};
