
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;
using std::unique_ptr;
using std::make_shared;

#include "Ast.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Environment.hpp"
#include "Object.hpp"
#include "Empty.hpp"

Empty::Empty()
  : null(0)
{}

Empty::Empty(const Empty& other)
  : null(0)
{}

unique_ptr<Object> Empty::clone()
{
  return unique_ptr<Object>(new Empty(*this));
}

string Empty::to_string()
{
  return "";
}

TypeJudgement Empty::getype(Environment env)
{
  return TypeJudgement(shared_ptr<Type>(new MonoType(AtomicType::None, Location())));
}

void Empty::substitute(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env)
{
  return;
}

bool Empty::appears_free(string var)
{
  return false;
}

void Empty::rename_binding(string old_name, string new_name)
{
  return;
}
