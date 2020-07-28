#pragma once
#include <string>
using std::string;

#include "Location.hh"

class ParserError
{
  Location errloc;
  string errstr;
public:
  ParserError(const Location& loc, const char* str)
    : errloc(loc), errstr(str) {}

  const Location& location() const
  {
    return errloc;
  }

  const char* what() const
  {
    return errstr.data();
  }
};

class TypeError
{
  Location errloc;
  string errstr;
public:
  TypeError()
    : errloc(), errstr("default constructed TypeError\n") {}
  TypeError(const Location& loc, const char* str)
    : errloc(loc), errstr(str) {}

  const Location& location() const
  {
    return errloc;
  }

  const char* what() const
  {
    return errstr.data();
  }
};

string buildErrStr(const Location& loc, const string& errdesc, const string& errtext);
string buildErrStr(const ParserError& prserr, const string& errtext);
