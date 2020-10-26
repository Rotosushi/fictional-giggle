
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "TypeJudgement.hpp"


bool TypeJudgement::succeeded()
{
  return success;
}

TypeJudgement::operator bool()
{
  return success;
}
