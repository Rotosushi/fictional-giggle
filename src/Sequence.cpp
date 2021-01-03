#include <string>
using std::string;
#include <vector>
using std::vector;
#include <utility>
using std::pair;
using std::get;
#include <memory>
using std::shared_ptr;

#include "Ast.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Environment.hpp"
#include "Sequence.hpp"

shared_ptr<Ast> Sequence::clone_internal()
{
  return shared_ptr<Ast>(new Sequence(lhs->clone(), rhs->clone(), Location()));
}

string Sequence::to_string_internal()
{
  return lhs->to_string() + "; " + rhs->to_string();
}

TypeJudgement Sequence::getype_internal(Environment& env)
{
  TypeJudgement lhstypejdgmt = lhs->getype(env);

  if (lhstypejdgmt)
  {
    TypeJudgement rhstypejdgmt = rhs->getype(env);
    return rhstypejdgmt;
  }
  else
  {
    return lhstypejdgmt;
  }
}

EvalJudgement Sequence::evaluate_internal(Environment& env)
{
  EvalJudgement lhsevaljdgmt = lhs->evaluate(env);

  if (lhsevaljdgmt)
  {
    EvalJudgement rhsevaljdgmt = rhs->evaluate(env);
    return rhsevaljdgmt;
  }
  else
  {
    return lhsevaljdgmt;
  }
}

void Sequence::substitute_internal(string& var, shared_ptr<Ast>* term, shared_ptr<Ast>& value, Environment& env)
{
  lhs->substitute(var, &lhs, value, env);
  rhs->substitute(var, &rhs, value, env);
}

bool Sequence::appears_free_internal(string& var)
{
  bool bl = lhs->appears_free(var);
  bool br = rhs->appears_free(var);
  return bl || br;
}

void Sequence::rename_binding_internal(string& old_name, string& new_name)
{
  lhs->rename_binding(old_name, new_name);
  rhs->rename_binding(old_name, new_name);
}
