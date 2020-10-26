
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;
using std::make_shared;

#include "Ast.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Environment.hpp"
#include "Empty.hpp"

shared_ptr<Ast> Empty::clone_internal()
{
  return shared_ptr<Ast>(new Empty(*this));
}

string Empty::to_string_internal()
{
  return "";
}

TypeJudgement Empty::getype_internal(Environment env)
{
  return TypeJudgement(MonoType(AtomicType::None));
}

EvalJudgement Empty::evaluate_internal(Environment env)
{
  return EvalJudgement(make_shared(Empty(*this)));
}
