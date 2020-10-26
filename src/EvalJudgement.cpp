

#include "EvalJudgement.hpp"

bool EvalJudgement::succeeded()
{
  return success;
}

EvalJudgement::operator bool()
{
  return success;
}
