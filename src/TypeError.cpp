#include <string>
using std::string;

#include "TypeError.hpp"

string TypeError::what()
{
  return "Type Error: " + dsc;
}
