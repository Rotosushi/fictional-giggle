#include <string>
using std::string;

#include "ParserError.hpp"

string ParserError::what()
{
  return "Parser Error: " + dsc;
}
