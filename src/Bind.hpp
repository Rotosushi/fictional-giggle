#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Ast.hpp"
#include "Environment.hpp"
#include "TypeJudgement.hpp"

class Bind : public Ast
{
public:
  string id;
  shared_ptr<Ast> rhs;

  Bind(const string& str, shared_ptr<Ast> r, const Location& l)
    : Ast(l), id(str), rhs(r) {}

  Bind(const Bind& other)
    : Ast(other.location), id(other.id), rhs(other.rhs) {}

protected:
    virtual shared_ptr<Ast> clone_internal() override;
    virtual string to_string_internal() override;
    virtual TypeJudgement getype_internal(Environment env) override;
    virtual EvalJudgement evaluate_internal(Environment env) override;
};
