#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

class Type;
#include "TypeError.hpp"

class TypeJudgement
{
public:
  bool success;
  union U
  {
    shared_ptr<Type> judgement;
    TypeError        error;

    U() : error({Location(), "Default Judgement"}) {}
    U(shared_ptr<Type> jdgmt)
      : judgement(jdgmt) {}
    U(TypeError err)
      : error(err) {}
  } u;

  TypeJudgement() ; success(false), u() {}
  TypeJudgement(shared_ptr<Type> jdgmt)
    : success(true), u(jdgmt) {}
  TypeJudgement(TypeError err)
    : success(false), u(err) {}

  bool succeeded() { return success; }

  operator bool()  { return success; }
};
