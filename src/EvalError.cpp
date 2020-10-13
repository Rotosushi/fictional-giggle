#include <string>
using std::string;

#include "EvalError.hpp"

string EvalError::what()
{
  return "Evaluation Error: " + dsc;
}
