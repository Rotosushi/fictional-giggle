#pragma once
#include <string>
using std::string;

#include "Location.hpp"
#include "Error.hpp"

class EvalError : public Error
{
public:
  EvalError(const Location& l, const string& s)
    : Error({l, s}) {}

  virtual string what() override;
};
