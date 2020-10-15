
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;
using std::make_shared;

#include "Ast.hpp"
#include "Type.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "SymbolTable.hpp"
#include "OperatorTable.hpp"

shared_ptr<Ast> Empty::clone_internal()
{
  return make_shared(Empty(*this));
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
