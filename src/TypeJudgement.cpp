
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Type.hpp"
#include "TypeError.hpp"
#include "TypeJudgement.hpp"


bool TypeJudgement::succeeded()
{
  return success;
}

TypeJudgement::operator bool()
{
  return success;
}
