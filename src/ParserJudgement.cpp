#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Ast.hpp"
#include "ParserError.hpp"

bool ParserJudgement::succeeded()
{
  return success;
}

ParserJudgement::operator bool()
{
  return success;
}
