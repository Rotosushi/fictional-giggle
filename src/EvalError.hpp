#pragma once
#include <string>
using std::string;

#include "Location.hpp"
#include "PinkError.hpp"

class EvalError : public PinkError
{
public:
  EvalError(const Location& l, const string& s)
    : PinkError(l, s) {}

  EvalError(PinkError& other)
    : PinkError(other.location(), other.what()) {}

  virtual string what() override;
};
