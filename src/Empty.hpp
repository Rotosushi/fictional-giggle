#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Ast.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Environment.hpp"

class Empty : public Ast
{
public:
  Empty(const Location& loc)
    : Ast(loc) {}

  Empty(const Empty& other)
    : Ast(other.location) {}

protected:
  virtual shared_ptr<Ast> clone_internal() override;
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(Environment env) override;
  virtual EvalJudgement evaluate_internal(Environment env) override;
};
