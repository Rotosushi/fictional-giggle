#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

class Ast;
#include "EvalError.hpp"

class EvalJudgement
{
public:
  bool success;
  union U
  {
    shared_ptr<Ast> jdgmt;
    EvalError       error;

    ~U() {}
    U() : error(Location(), "Default Judgement") {}
    U(shared_ptr<Ast> jdgmt)
      : jdgmt(jdgmt) {}
    U(EvalError err)
      : error(err) {}
  } u;

  EvalJudgement() : success(false), u() {}
  EvalJudgement(shared_ptr<Ast> jdgmt)
    : success(true), u(jdgmt) {}
  EvalJudgement(EvalError err)
    : success(false), u(err) {}

  bool succeeded();
  operator bool();
};
