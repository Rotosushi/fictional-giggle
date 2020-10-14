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

  virtual ~PinkError() {}

  Location location();
  virtual string what();
};

string buildErrStr(const Location& loc, const string& errdesc, const string& errtext);
string buildErrStr(const PinkError& err, const string& errtxt);
