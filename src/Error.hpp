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

  virtual Location location() { return loc; }
  virtual string   what()     { return dsc; }
};
