#pragma once
#include <string>
using std::string;

#include "PinkError.hpp"

class ParserError : public PinkError
{
public:
  ParserError(const string& err, const Location& loc)
    : PinkError(loc, err) {}

  virtual string what() override;
};
