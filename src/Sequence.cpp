#include <string>
using std::string;
#include <vector>
using std::vector>
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

TypeJudgement Sequence::getype_internal(Environment env)
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

EvalJudgement Sequence::evaluate_internal(Environment env)
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

void Sequence::substitute_internal(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env)
{
  lhs->substitute(subs, &lhs, env);
  rhs->substitute(subs, &rhs, env);
}

bool Sequence::appears_free_internal(string var)
{
  return lhs->appears_free(var) || rhs->appears_free(var);
}

void Sequence::rename_binding_in_body_internal(vector<pair<string, string>>& renaming_pairs)
{
  lhs->rename_binding(old_name, new_name);
  rhs->rename_binding(old_name, new_name);
}
