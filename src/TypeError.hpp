#pragma once
#include <string>
using std::string;

#include "Location.hpp"
#include "PinkError.hpp"

class TypeError : public PinkError
{
public:
  TypeError(const Location& loc, const string& str)
    : PinkError(loc, str) {}

  virtual string what() override;
};
