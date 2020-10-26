#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

class Type;
#include "TypeError.hpp"

/*
  a type judgement encapsulates the success or failure
  aspect of typeability. if the judgement is constructed
  around a type, then it is a successful judgement
  if it constructed around a TypeError, then it's a
  failed judgement. in both cases we need to return
  information to the caller alongside the success
  or failure. this allows each typeing procedure to share
  the exact same return type.
*/

class TypeJudgement
{
public:
  bool success;
  union U
  {
    shared_ptr<Type> jdgmt;
    TypeError        error;

    ~U() {}
    U() : error({Location(), "Default Judgement"}) {}
    U(shared_ptr<Type> jdgmt)
      : jdgmt(jdgmt) {}
    U(TypeError err)
      : error(err) {}
  } u;

  TypeJudgement() : success(false), u() {}
  TypeJudgement(shared_ptr<Type> jdgmt)
    : success(true), u(jdgmt) {}
  TypeJudgement(TypeError err)
    : success(false), u(err) {}

  TypeJudgement(const TypeJudgement&& other)
    : success(other.success)
    {
      if (success)
      {
        u.jdgmt = other.u.jdgmt;
      }
      else
      {
        u.error = other.u.error;
      }
    }

  bool succeeded();
  operator bool();
};
