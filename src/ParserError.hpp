#pragma once
#include <string>
using std::string;

#include "Error.hpp"

class ParserError : public Error
{
public:
  ParserError(const string& err, const Location& loc)
    : Error({err, loc}) {}

  virtual string what() override;
}
