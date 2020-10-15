

#include "EvalJudgement.hpp"

bool EvalJudgement::succeeded()
{
  return success;
}

ParserJudgement::operator bool()
{
  return success;
}
