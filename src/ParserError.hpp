#pragma once
#include <string>
using std::string;

#include "PinkError.hpp"

class ParserError : public PinkError
{
public:
  ParserError(const string& err, const Location& loc)
    : PinkError(loc, err) {}

  ParserError(const ParserError& other)
    : ParserError(other.dsc, other.loc) {}

  ParserError(PinkError& other)
    : PinkError(other.location(), other.what()) {}

  virtual string what() override;
};
