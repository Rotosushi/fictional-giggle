#pragma once
#include <string>
using std::string;

#include "Location.hpp"

class PinkError
{
public:
  Location loc;
  string   dsc;

  PinkError(const Location& l, const string& str)
    : loc(l), dsc(str) {}

  PinkError(PinkError& other)
    : loc(other.loc), dsc(other.dsc) {}

  virtual ~PinkError() {}

  Location& location();
  virtual string what();
};

string buildErrStr(Location loc, string errdesc, string errtext);
string buildErrStr(PinkError err, string errtxt);
