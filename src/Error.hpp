#pragma once
#include <string>
using std::string;

#include "Location.hpp"

class Error
{
public:
  Location loc;
  string   dsc;

  Error(const Location& l, const string& str)
    : loc(l), dsc(str) {}

  virtual ~Error() {}

  Location location();
  virtual string   what();
};
