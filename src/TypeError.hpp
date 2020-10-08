#pragma once
#include <string>
using std::string;

#include "Location.hpp"
#include "Error.hpp"

class TypeError : public Error
{
public:
  TypeError(const Location& loc, const string& str)
    : Error({loc, str}) {}

  virtual string what() override
  {
    return "Type Error: " + dsc;
  }
};
