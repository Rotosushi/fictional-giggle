
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

TypeJudgement& TypeJudgement::operator=(const TypeJudgement& other)
{
  success = other.success;
  if (success)
    u.jdgmt = other.u.jdgmt;
  else
    u.error = other.u.error;

  return *this;
}
