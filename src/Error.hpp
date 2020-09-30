#pragma once
#include <string>
using std::string;

#include "Location.hpp"

class Error
{
  Location loc;
  string   dsc;

public:
  Error(const Location& l, const string& str)
    : loc(l), dsc(str) {}

  virtual Location location() { return loc; }
  virtual char*    what()     { return dsc; }
};
