#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "ParserJudgement.hpp"

bool ParserJudgement::succeeded()
{
  return success;
}

ParserJudgement::operator bool()
{
  return success;
}
