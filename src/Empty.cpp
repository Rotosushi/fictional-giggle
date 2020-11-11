
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

Empty::Empty(const Location& loc)
  : Ast(loc), null(0)
{}

Empty::Empty(const Empty& other)
  : Ast(other.location), null(0)
{}

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
  return TypeJudgement(shared_ptr<Type>(new MonoType(AtomicType::None, location)));
}

EvalJudgement Empty::evaluate_internal(Environment env)
{
  return EvalJudgement(shared_ptr<Ast>(new Empty(*this)));
}


void Empty::substitute_internal(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env)
{
  return;
}

bool Empty::appears_free_internal(string var)
{
  return false;
}

void Empty::rename_binding_internal(string old_name, string new_name)
{
  return;
}
