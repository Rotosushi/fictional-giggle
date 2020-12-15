
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <utility>
using std::pair;
using std::get;
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

void Empty::substitute(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env)
{
  return;
}

bool Empty::appears_free(vector<string>& names, vector<string>& appeared_free)
{
  return false;
}

void Empty::rename_binding_in_body(vector<pair<string, string>>& renaming_pairs)
{
  return;
}
